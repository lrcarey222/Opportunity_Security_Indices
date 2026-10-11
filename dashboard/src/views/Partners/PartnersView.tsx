// View 5: Partners (spec_react.md 8.6; paper section 9). How a reporting economy rates every partner
// as a trusted supplier (Friendshore, FS), an export market (Partnership Opportunity, PO) and a
// development partner (DP), summarised by the Partnership Strength Index (PSI); the dyad drill-down;
// the partner-level PSI.

import { useCallback, useEffect, useMemo, useState } from 'react';
import { useParams } from 'react-router';
import * as Plot from '@observablehq/plot';
import { useTitle } from '../../App.tsx';
import { ChartCard } from '../../charts/ChartCard.tsx';
import { Legend, RampLegend } from '../../charts/legends.tsx';
import { PlotFigure, Tip } from '../../charts/PlotFigure.tsx';
import { divColor, labelMargin, seqColor, symDomain } from '../../charts/theme.ts';
import { WorldMap, type MapCell } from '../../charts/WorldMap.tsx';
import { CountrySelect } from '../../components/CountrySelect.tsx';
import { TableView, type Col } from '../../components/TableView.tsx';
import { EmptyState, FilterRow, Loading, Notice, PageHead, RankChip } from '../../components/ui.tsx';
import type { Table } from '../../engine/types.ts';
import { tableToCsv } from '../../lib/csv.ts';
import { cellLabel, DASH, fmtNum, fmtSigned, tierLabel } from '../../lib/format.ts';
import { countryName } from '../../state/appData.ts';
import { useApp, useEngine, usePartner, useScenario } from '../../state/EngineProvider.tsx';
import { useGoto } from '../../state/nav.ts';
import { toSpec } from '../../state/url.ts';
import { COMPARE_COLOR, INK, PILLAR_COLOR, SIGNAL, TIER_COLOR, tierKey } from '../../theme/palette.ts';
import { numOr, rowsOf, showWord, strOr, useCountryOptions, type Row } from '../common.tsx';

const BLOC_X: Record<string, number> = { 'China-leaning': 0, Nonaligned: 0.5, 'U.S.-leaning': 1 };

interface DyadRow {
  partner_iso: string;
  Country: string;
  tier: 'core' | 'partner' | 'other';
  bloc: string | null;
  psi: number | null;
  psi_base: number | null;
  delta: number | null;
  friendshore: number | null;
  friendshore_base: number | null;
  opportunity: number | null;
  development: number | null;
  rank: number | null;
  rank_base: number | null;
  rank_delta: number | null;
  rankable: boolean;
  reason: string | null;
}

interface CellPoint {
  tech: string;
  stage: string;
  index: 'Friendshore' | 'Opportunity';
  value: number | null;
  best: boolean;
  excluded: boolean;
}

function hash01(s: string): number {
  let h = 2166136261;
  for (let i = 0; i < s.length; i++) h = Math.imul(h ^ s.charCodeAt(i), 16777619);
  return ((h >>> 0) % 1000) / 1000;
}

export default function PartnersView() {
  useTitle('Partners');
  const app = useApp();
  const { client } = useEngine();
  const params = useParams();
  const goto = useGoto();
  const { scenario, key, show, isBaseline, view, setView } = useScenario();
  const partner = usePartner(true);
  const reporter = params.iso3 && app.Uindex.has(params.iso3) ? params.iso3 : app.Uindex.has(app.defaultCountry) ? app.defaultCountry : app.U[0];
  const uSet = useMemo(() => new Set(app.U), [app.U]);
  const options = useCountryOptions((c) => uSet.has(c.iso3c));
  const [dyads, setDyads] = useState<{ key: string; table: Table | null; error: string | null } | null>(null);
  const [dec, setDec] = useState<{ key: string; table: Table | null; error: string | null } | null>(null);

  // the reporter's dyads, once the scenario's partner layer is computed (so the worker's cache serves it)
  const dyKey = `${key}|${reporter}`;
  useEffect(() => {
    if (!partner.fresh) return;
    let live = true;
    client.dyads(toSpec(scenario), reporter).then(
      (t) => live && setDyads({ key: dyKey, table: t, error: null }),
      (e: Error) => live && setDyads({ key: dyKey, table: null, error: e.message }),
    );
    return () => {
      live = false;
    };
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [dyKey, partner.fresh, client]);

  const rows = useMemo((): DyadRow[] => {
    if (!dyads?.table) return [];
    return rowsOf(dyads.table).map((r) => {
      const iso = String(r.partner_iso);
      const c = app.byIso.get(iso);
      return {
        partner_iso: iso,
        Country: c?.country ?? countryName(app, iso),
        tier: tierKey(c?.tier),
        bloc: c?.bloc ?? null,
        psi: numOr(r.psi),
        psi_base: numOr(r.psi_base),
        delta: numOr(r.delta),
        friendshore: numOr(r.friendshore),
        friendshore_base: numOr(r.friendshore_base),
        opportunity: numOr(r.opportunity),
        development: numOr(r.development),
        rank: numOr(r.rank),
        rank_base: numOr(r.rank_base),
        rank_delta: numOr(r.rank_delta),
        rankable: r.rankable === true,
        reason: strOr(r.rank_exclusion),
      };
    });
  }, [dyads, app]);

  const ranked = useMemo(() => rows.filter((r) => r.rank !== null).sort((a, b) => (a.rank as number) - (b.rank as number)), [rows]);
  const sel = view.partner && rows.some((r) => r.partner_iso === view.partner) ? view.partner : (ranked[0]?.partner_iso ?? null);
  const selRow = rows.find((r) => r.partner_iso === sel);

  const decKey = `${key}|${reporter}|${sel}`;
  useEffect(() => {
    if (!sel || !partner.fresh) return;
    let live = true;
    client.decompose(toSpec(scenario), reporter, sel).then(
      (t) => live && setDec({ key: decKey, table: t, error: null }),
      (e: Error) => live && setDec({ key: decKey, table: null, error: e.message }),
    );
    return () => {
      live = false;
    };
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [decKey, partner.fresh, client]);

  const choose = useCallback((iso: string) => setView({ partner: iso }, { replace: true }), [setView]);

  // (b) top 20
  const top20 = useMemo(() => {
    const val = (r: DyadRow) => (show === 'baseline' ? r.psi_base : show === 'change' ? r.delta : r.psi);
    const base = show === 'baseline' ? rows.filter((r) => r.rank_base !== null).sort((a, b) => (a.rank_base as number) - (b.rank_base as number)) : ranked;
    return base.slice(0, 20).map((r) => ({ ...r, shown: val(r) }));
  }, [rows, ranked, show]);
  const buildTop = useCallback(
    (width: number): Plot.PlotOptions | null => {
      if (!top20.length) return null;
      const change = show === 'change';
      return {
        width,
        height: 40 + top20.length * 22,
        marginLeft: labelMargin(top20.map((r) => r.Country), { min: 90, max: 180 }),
        marginRight: 60,
        x: { label: change ? 'Change in PSI against the paper' : 'PSI (dyad)', grid: true, nice: true, domain: change ? symDomain(top20.map((r) => r.shown), 0.005).map((v) => v * 1.5) : [0, 1] },
        y: { domain: top20.map((r) => r.Country), label: null, tickSize: 0 },
        marks: [
          Plot.barX(top20, { y: 'Country', x: 'shown', fill: (r: DyadRow) => TIER_COLOR[r.tier], insetTop: 4, insetBottom: 4 }),
          !change && !isBaseline ? Plot.tickX(top20, { y: 'Country', x: 'psi_base', stroke: COMPARE_COLOR.baseline, strokeWidth: 2 }) : null,
          // labels sit beyond the bar's end: right of positive bars, left of negative ones
          Plot.text(top20.filter((r) => !change || (r.shown ?? 0) >= 0), { y: 'Country', x: 'shown', text: (r: { shown: number | null }) => (change ? fmtSigned(r.shown, 3) : fmtNum(r.shown, 3)), dx: 6, textAnchor: 'start', fill: '#d1d5db' }),
          change ? Plot.text(top20.filter((r) => (r.shown ?? 0) < 0), { y: 'Country', x: 'shown', text: (r: { shown: number | null }) => fmtSigned(r.shown, 3), dx: -6, textAnchor: 'end', fill: '#d1d5db' }) : null,
          Plot.ruleX([0], { stroke: 'rgb(255 255 255 / 0.22)' }),
          Plot.barX(top20, Plot.pointerY({ y: 'Country', x: 'shown', fill: 'none', stroke: SIGNAL, strokeWidth: 2, insetTop: 3, insetBottom: 3 })),
        ],
      };
    },
    [top20, show, isBaseline],
  );

  // (c) bloc vs Friendshore
  const blocPts = useMemo(
    () =>
      rows
        .filter((r) => r.bloc && BLOC_X[r.bloc] !== undefined && (show === 'baseline' ? r.friendshore_base : r.friendshore) !== null)
        .map((r) => ({ ...r, x: BLOC_X[r.bloc as string] + (hash01(r.partner_iso) - 0.5) * 0.3, y: show === 'baseline' ? r.friendshore_base : r.friendshore })),
    [rows, show],
  );
  const buildBloc = useCallback(
    (width: number): Plot.PlotOptions | null => {
      if (!blocPts.length) return null;
      return {
        width,
        height: 360,
        marginLeft: 48,
        x: { domain: [-0.25, 1.25], ticks: [0, 0.5, 1], tickFormat: (v: number) => (v === 0 ? 'China-leaning' : v === 0.5 ? 'Nonaligned' : 'U.S.-leaning'), label: "Partner's UN-voting bloc" },
        y: { domain: [0, 1], label: '↑ Friendshore best-five', grid: true },
        marks: [
          Plot.dot(blocPts, { x: 'x', y: 'y', r: 4, fill: (r: DyadRow) => TIER_COLOR[r.tier], stroke: INK[800], strokeWidth: 1.5 }),
          Plot.dot(blocPts.filter((p) => p.partner_iso === sel), { x: 'x', y: 'y', r: 6, fill: 'none', stroke: SIGNAL, strokeWidth: 2.5 }),
          Plot.dot(blocPts, Plot.pointer({ x: 'x', y: 'y', r: 8, fill: 'none', stroke: SIGNAL, strokeWidth: 2, maxRadius: 24 })),
        ],
      };
    },
    [blocPts, sel],
  );

  // (d) dyad cells
  const cellPts = useMemo((): CellPoint[] => {
    if (!dec?.table) return [];
    const out: CellPoint[] = [];
    for (const r of rowsOf(dec.table)) {
      const base = { tech: String(r.tech), stage: String(r.supply_chain), excluded: r.excluded === true };
      out.push({ ...base, index: 'Friendshore', value: numOr(r.friendshore_index), best: r.in_top5_fs === true });
      out.push({ ...base, index: 'Opportunity', value: numOr(r.opportunity_index), best: r.in_top5_po === true });
    }
    return out;
  }, [dec]);
  const buildCells = useCallback(
    (width: number): Plot.PlotOptions | null => {
      const pts = cellPts.filter((p) => p.value !== null);
      if (!pts.length) return null;
      return {
        width,
        height: 300,
        marginLeft: 44,
        marginTop: 28,
        marginBottom: 96,
        fx: { domain: app.stages, label: null, axis: 'top', padding: 0.06 },
        x: { domain: app.techs, label: null, tickRotate: -50, tickSize: 0 },
        y: { domain: [0, 1], label: '↑ Index', grid: true, ticks: 4 },
        color: { domain: ['Friendshore', 'Opportunity'], range: [PILLAR_COLOR.ES, PILLAR_COLOR.EO] },
        symbol: { domain: ['best five', 'other'], range: ['star', 'circle'] },
        marks: [
          ...(['Friendshore', 'Opportunity'] as const).map((ix) =>
            Plot.dot(pts.filter((p) => p.index === ix), {
              fx: 'stage', x: 'tech', y: 'value', fill: 'index', symbol: (p: CellPoint) => (p.best ? 'best five' : 'other'),
              r: (p: CellPoint) => (p.best ? 7 : 4), stroke: INK[800], strokeWidth: 1, dx: ix === 'Friendshore' ? -3 : 3,
            }),
          ),
          Plot.dot(pts, Plot.pointer({ fx: 'stage', x: 'tech', y: 'value', r: 9, fill: 'none', stroke: SIGNAL, strokeWidth: 2, maxRadius: 20 })),
        ],
      };
    },
    [cellPts, app],
  );

  // (e) partner-level PSI
  const psiRows = useMemo(() => {
    if (!partner.data) return [];
    return rowsOf(partner.data.compare.psi_partner).map((r) => ({
      iso: String(r.partner_iso),
      Country: String(r.Country ?? r.partner_iso),
      psi: numOr(r.psi),
      psi_base: numOr(r.psi_base),
      delta: numOr(r.delta),
      rank: numOr(r.rank),
      rank_delta: numOr(r.rank_delta),
      friendshore: numOr(r.friendshore),
      opportunity: numOr(r.opportunity),
      development: numOr(r.development),
      rankable: r.rankable === true,
      reason: strOr(r.rank_exclusion),
      shown: show === 'baseline' ? numOr(r.psi_base) : show === 'change' ? numOr(r.delta) : numOr(r.psi),
    }));
  }, [partner.data, show]);
  const psiDom = useMemo(() => {
    const v = psiRows.filter((r) => r.rankable && r.shown !== null).map((r) => r.shown as number);
    if (show === 'change') {
      const L = Math.max(1e-9, ...v.map(Math.abs));
      return { kind: 'div' as const, lo: -L, hi: L };
    }
    return { kind: 'seq' as const, lo: v.length ? Math.min(...v) : 0, hi: v.length ? Math.max(...v) : 1 };
  }, [psiRows, show]);
  const psiCells = useMemo(() => {
    const color = psiDom.kind === 'div' ? divColor(psiDom.hi) : seqColor(psiDom.lo, psiDom.hi);
    const m = new Map<string, MapCell>();
    for (const r of psiRows) {
      const c = app.byIso.get(r.iso);
      if (!c?.iso_n3) continue;
      m.set(c.iso_n3, {
        iso3c: r.iso,
        fill: r.rankable && r.shown !== null ? color(r.shown) : 'hatch',
        tip: (
          <Tip
            value={show === 'change' ? fmtSigned(r.shown, 3) : fmtNum(r.shown, 3)}
            title={r.Country}
            lines={[`rank ${r.rank ?? DASH}${!isBaseline && r.rank_delta ? ` (${r.rank_delta > 0 ? 'up' : 'down'} ${Math.abs(r.rank_delta)})` : ''}`, !r.rankable ? r.reason ?? 'not rankable' : null]}
          />
        ),
      });
    }
    return m;
  }, [psiRows, psiDom, app, show, isBaseline]);

  const dyTip = (r: DyadRow & { shown?: number | null }) => (
    <Tip
      value={`PSI ${fmtNum(r.psi, 3)}`}
      title={`${r.Country} (${tierLabel(r.tier)})`}
      lines={[
        `FS ${fmtNum(r.friendshore, 3)} · PO ${fmtNum(r.opportunity, 3)} · DP ${fmtNum(r.development, 3)}`,
        `rank ${r.rank ?? DASH}${!isBaseline ? `; paper ${fmtNum(r.psi_base, 3)}, rank ${r.rank_base ?? DASH}` : ''}`,
        r.bloc ? `bloc: ${r.bloc}` : null,
        !r.rankable ? r.reason ?? 'not rankable' : null,
      ]}
    />
  );

  const rankCols: Col<DyadRow>[] = [
    { id: 'rank', header: 'Rank', value: (r) => r.rank, num: true },
    { id: 'Country', header: 'Partner', value: (r) => r.Country, minWidth: 150 },
    { id: 'tier', header: 'Tier', value: (r) => tierLabel(r.tier) },
    { id: 'psi', header: 'PSI', value: (r) => r.psi, cell: (r) => fmtNum(r.psi, 3), num: true },
    { id: 'friendshore', header: 'FS best-five', value: (r) => r.friendshore, cell: (r) => fmtNum(r.friendshore, 3), num: true },
    { id: 'opportunity', header: 'PO best-five', value: (r) => r.opportunity, cell: (r) => fmtNum(r.opportunity, 3), num: true },
    { id: 'development', header: 'DP', value: (r) => r.development, cell: (r) => fmtNum(r.development, 3), num: true },
    { id: 'bloc', header: 'Bloc', value: (r) => r.bloc, minWidth: 110 },
    { id: 'rank_delta', header: 'Rank change', value: (r) => r.rank_delta, cell: (r) => <RankChip change={r.rank_delta} />, num: true },
    { id: 'rankable', header: 'Rankable', value: (r) => (r.rankable ? 'yes' : `no${r.reason ? `: ${r.reason}` : ''}`), minWidth: 300 },
  ];

  const decCols: Col<Row>[] = [
    { id: 'cell', header: 'Cell', value: (r) => cellLabel(String(r.tech), String(r.supply_chain)), minWidth: 170 },
    { id: 'fs', header: 'Friendshore', value: (r) => numOr(r.friendshore_index), cell: (r) => fmtNum(r.friendshore_index, 3), num: true },
    { id: 'po', header: 'Opportunity', value: (r) => numOr(r.opportunity_index), cell: (r) => fmtNum(r.opportunity_index, 3), num: true },
    { id: 'best', header: 'Best five', value: (r) => [r.in_top5_fs === true ? 'FS' : '', r.in_top5_po === true ? 'PO' : ''].filter(Boolean).join(', ') || null },
    { id: 'imp', header: 'FS imports', value: (r) => numOr(r.fs_imp_trade_index), cell: (r) => fmtNum(r.fs_imp_trade_index, 3), num: true },
    { id: 'need', header: 'FS need', value: (r) => numOr(r.fs_es_need), cell: (r) => fmtNum(r.fs_es_need, 3), num: true },
    { id: 'eo_p', header: 'FS partner EO', value: (r) => numOr(r.fs_eo_partner), cell: (r) => fmtNum(r.fs_eo_partner, 3), num: true },
    { id: 'out', header: 'FS outbound', value: (r) => numOr(r.fs_outbound_index), cell: (r) => fmtNum(r.fs_outbound_index, 3), num: true },
    { id: 'pen', header: 'FS penalty (r + p)', value: (r) => (numOr(r.fs_penalty_reporter) ?? 0) + (numOr(r.fs_penalty_partner) ?? 0), cell: (r) => fmtNum((numOr(r.fs_penalty_reporter) ?? 0) + (numOr(r.fs_penalty_partner) ?? 0), 3), num: true },
    { id: 'fsraw', header: 'FS raw / adj', value: (r) => numOr(r.fsi_raw), cell: (r) => `${fmtNum(r.fsi_raw, 3)} / ${fmtNum(r.fsi_adj, 3)}` },
    { id: 'trade', header: 'PO exports', value: (r) => numOr(r.po_trade_index), cell: (r) => fmtNum(r.po_trade_index, 3), num: true },
    { id: 'poeo', header: 'PO opportunity', value: (r) => numOr(r.po_econ_opp_index), cell: (r) => fmtNum(r.po_econ_opp_index, 3), num: true },
    { id: 'poneed', header: 'PO market need', value: (r) => numOr(r.po_partner_es_need), cell: (r) => fmtNum(r.po_partner_es_need, 3), num: true },
    { id: 'popen', header: 'PO penalty', value: (r) => numOr(r.po_penalty), cell: (r) => fmtNum(r.po_penalty, 3), num: true },
    { id: 'poraw', header: 'PO raw / adj', value: (r) => numOr(r.opportunity_raw), cell: (r) => `${fmtNum(r.opportunity_raw, 3)} / ${fmtNum(r.opportunity_adj, 3)}` },
    { id: 'excluded', header: 'Excluded', value: (r) => (r.excluded === true ? 'excluded (fossil)' : '') },
  ];

  const loadingPartner = !partner.data;
  const dyUpdating = !partner.fresh || (dyads !== null && dyads.key !== dyKey);
  const rName = countryName(app, reporter);
  return (
    <div>
      <PageHead
        eyebrow="Bilateral indices"
        title="Partners"
        lede="How a reporting economy rates every partner: as a trusted supplier (Friendshore, FS), as an export market (Partnership Opportunity, PO) and as a development partner (DP). Each is a best-five summary over technologies and stages; the Partnership Strength Index (PSI) combines them."
      />
      <FilterRow result={rows.length ? `${ranked.length} ranked partners of ${rows.length}` : undefined}>
        <CountrySelect label="Reporter" value={reporter} options={options} onChange={(v) => goto(`/partners/${v}`)} />
      </FilterRow>
      {loadingPartner ? (
        <Loading label="Loading partnership data" />
      ) : (
        <div className="flex flex-col gap-6">
          {dyads?.error && <Notice kind="error">{dyads.error}</Notice>}
          <div className="grid gap-6 xl:grid-cols-2">
            <ChartCard
              title={`${rName}: top 20 partners by PSI`}
              sub={`${showWord(show)} Dyadic PSI; gray tick = the paper's value. Colour = the partner's coalition tier. Click a bar for the dyad's detail.`}
              updating={dyUpdating}
              summary={`Top ${top20.length} partners of ${rName} by PSI.`}
              legend={
                <Legend
                  items={[
                    { label: 'Coalition core', color: TIER_COLOR.core },
                    { label: 'Coalition partner', color: TIER_COLOR.partner },
                    { label: 'Other economy', color: TIER_COLOR.other },
                    ...(!isBaseline && show !== 'change' ? [{ label: "Paper's PSI (tick)", kind: 'tick' as const, color: COMPARE_COLOR.baseline }] : []),
                  ]}
                />
              }
              table={() => <TableView rows={top20} cols={rankCols} caption="Top 20 partners" pageSize={20} />}
            >
              {!dyads ? <Loading label="Loading dyads" /> : top20.length ? (
                <PlotFigure<DyadRow> build={buildTop} height={40 + top20.length * 22} ariaLabel="Top partners by PSI" tooltip={dyTip} onSelect={(r) => choose(r.partner_iso)} />
              ) : (
                <EmptyState>No partnership scores for this reporter.</EmptyState>
              )}
            </ChartCard>
            <ChartCard
              title="Voting bloc and Friendshore"
              sub="Each partner's UN-voting bloc (jittered within its band) against its Friendshore best-five with the reporter. Colour = coalition tier."
              updating={dyUpdating}
              summary={`${blocPts.length} partners by voting bloc and Friendshore.`}
              legend={
                <Legend
                  items={[
                    { label: 'Coalition core', kind: 'dot', color: TIER_COLOR.core },
                    { label: 'Coalition partner', kind: 'dot', color: TIER_COLOR.partner },
                    { label: 'Other economy', kind: 'dot', color: TIER_COLOR.other },
                    { label: 'Selected partner', kind: 'outline', color: SIGNAL },
                  ]}
                />
              }
              table={() => (
                <TableView
                  rows={blocPts}
                  caption="Voting bloc and Friendshore"
                  search
                  cols={[
                    { id: 'Country', header: 'Partner', value: (r) => r.Country },
                    { id: 'bloc', header: 'Bloc', value: (r) => r.bloc },
                    { id: 'fs', header: 'FS best-five', value: (r) => r.y, cell: (r) => fmtNum(r.y, 3), num: true },
                  ]}
                />
              )}
            >
              {blocPts.length ? (
                <PlotFigure<DyadRow> build={buildBloc} height={360} ariaLabel="Voting bloc against Friendshore" tooltip={dyTip} onSelect={(r) => choose(r.partner_iso)} />
              ) : (
                <EmptyState>No voting-bloc data in this build.</EmptyState>
              )}
            </ChartCard>
          </div>

          <ChartCard
            title={`${rName}: partner ranking`}
            sub="All partners of the reporter. Select a row for the dyad's detail. Rank change compares with the paper; the reason a partner is not rankable is in the last column."
            updating={dyUpdating}
            csv={dyads?.table ? () => ({ name: `psi-dyads-${reporter}`, text: tableToCsv(dyads.table as Table) }) : undefined}
          >
            {!dyads ? (
              <Loading label="Loading dyads" />
            ) : (
              <TableView rows={rows} cols={rankCols} caption="Partner ranking" search pageSize={15} initialSort={{ id: 'rank' }} rowKey={(r) => r.partner_iso} onRowClick={(r) => choose(r.partner_iso)} selectedKey={sel} />
            )}
          </ChartCard>

          <ChartCard
            title={selRow ? `${rName} and ${selRow.Country}: the dyad` : 'The dyad'}
            sub="Friendshore (the reporter imports from the partner) and Opportunity (the reporter exports to the partner) per technology and stage; the cells forming each best-five are drawn as stars. The table lists the components and climate penalties."
            updating={!!dec && dec.key !== decKey}
            legend={
              <Legend
                items={[
                  { label: 'Friendshore', kind: 'dot', color: PILLAR_COLOR.ES },
                  { label: 'Opportunity', kind: 'dot', color: PILLAR_COLOR.EO },
                  { label: 'Star = in the best five', kind: 'diamond', color: '#d1d5db' },
                ]}
              />
            }
            table={() => <TableView rows={rowsOf(dec?.table)} cols={decCols} caption="Dyad components" pageSize={33} />}
            csv={dec?.table ? () => ({ name: `dyad-${reporter}-${sel}`, text: tableToCsv(dec.table as Table) }) : undefined}
          >
            {!sel ? (
              <EmptyState>Select a partner.</EmptyState>
            ) : !dec ? (
              <Loading label="Decomposing the dyad" />
            ) : dec.error ? (
              <Notice kind="error">{dec.error}</Notice>
            ) : (
              // 33 technology labels need ~640 px: on a phone the chart scrolls inside the card
              <div className="overflow-x-auto">
                <PlotFigure<CellPoint>
                  build={buildCells}
                  height={300}
                  minWidth={640}
                  ariaLabel="Friendshore and Opportunity per cell"
                  tooltip={(p) => (
                    <Tip value={fmtNum(p.value, 3)} title={`${p.index}: ${cellLabel(p.tech, p.stage)}`} lines={[p.best ? 'in the best five' : null, p.excluded ? 'excluded (fossil)' : null]} />
                  )}
                />
              </div>
            )}
          </ChartCard>

          <ChartCard
            title={`Partnership Strength Index, ${partner.data?.psi.view === 'world' ? 'world' : 'coalition'} view`}
            sub={`${showWord(show)} The partner-level PSI averages each partner's ratings over the viewpoint's reporters (${partner.data?.psi.reporters.length ?? 0} reporters; see Advanced). Hatched = not rankable.`}
            updating={!partner.fresh}
            legend={
              <div className="flex flex-wrap items-end gap-x-8 gap-y-3">
                <RampLegend kind={psiDom.kind} min={psiDom.lo} max={psiDom.hi} label={show === 'change' ? 'Change in PSI (increase = blue)' : 'PSI'} format={(v) => fmtNum(v, 2)} />
                <Legend items={[{ label: 'Not rankable', kind: 'hatch' }]} />
              </div>
            }
            table={() => (
              <TableView
                rows={psiRows}
                caption="Partner-level PSI"
                search
                initialSort={{ id: 'rank' }}
                cols={[
                  { id: 'rank', header: 'Rank', value: (r) => r.rank, num: true },
                  { id: 'Country', header: 'Partner', value: (r) => r.Country, minWidth: 150 },
                  { id: 'psi', header: 'PSI', value: (r) => r.psi, cell: (r) => fmtNum(r.psi, 3), num: true },
                  { id: 'psi_base', header: 'Paper', value: (r) => r.psi_base, cell: (r) => fmtNum(r.psi_base, 3), num: true },
                  { id: 'fs', header: 'FS', value: (r) => r.friendshore, cell: (r) => fmtNum(r.friendshore, 3), num: true },
                  { id: 'po', header: 'PO', value: (r) => r.opportunity, cell: (r) => fmtNum(r.opportunity, 3), num: true },
                  { id: 'dp', header: 'DP', value: (r) => r.development, cell: (r) => fmtNum(r.development, 3), num: true },
                  { id: 'rd', header: 'Rank change', value: (r) => r.rank_delta, cell: (r) => <RankChip change={r.rank_delta} />, num: true },
                  { id: 'rankable', header: 'Rankable', value: (r) => (r.rankable ? 'yes' : `no${r.reason ? `: ${r.reason}` : ''}`) },
                ]}
              />
            )}
            csv={partner.data ? () => ({ name: 'psi-partner', text: tableToCsv(partner.data!.compare.psi_partner) }) : undefined}
          >
            <WorldMap cells={psiCells} ariaLabel="Map of the partner-level PSI" onSelect={(iso) => app.Uindex.has(iso) && goto(`/partners/${iso}`)} selected={reporter} />
          </ChartCard>
        </div>
      )}
    </div>
  );
}

