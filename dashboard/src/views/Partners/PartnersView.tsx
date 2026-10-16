// View 5: Partners (spec_react.md 8.6; paper section 9). How a reporting economy rates every partner
// as a friendshoring opportunity (Friendshore), an export market (Partnership Opportunity) and a development
// partner (Development Potential), summarised by the Partnership Strength Index; the pair drill-down
// (with the CM-FDI investment tie and the Chinese-parent ownership component, spec_country.md C4.1);
// the partner-level Partnership Strength on the three-colour scale (C1.5). No bare acronyms (C3.1).

import { useCallback, useEffect, useMemo, useState } from 'react';
import { Link, useParams } from 'react-router';
import * as Plot from '@observablehq/plot';
import { useTitle } from '../../App.tsx';
import { ChartCard } from '../../charts/ChartCard.tsx';
import { Legend, TriLegend } from '../../charts/legends.tsx';
import { PlotFigure, Tip } from '../../charts/PlotFigure.tsx';
import { triScale } from '../../charts/scale.ts';
import { labelMargin, symDomain } from '../../charts/theme.ts';
import { WorldMap, type MapCell } from '../../charts/WorldMap.tsx';
import { CountrySelect } from '../../components/CountrySelect.tsx';
import { TableView, type Col } from '../../components/TableView.tsx';
import { Term } from '../../components/Term.tsx';
import { EmptyState, FilterRow, Loading, Notice, PageHead, RankChip, Select } from '../../components/ui.tsx';
import type { Table } from '../../engine/types.ts';
import { rowsToCsv, tableToCsv } from '../../lib/csv.ts';
import { cellLabel, cellOptions, DASH, fmtNum, fmtPct, fmtSigned, stageSlug, tierLabel } from '../../lib/format.ts';
import { devDataLink } from '../../country/derive/links.ts';
import { countryName } from '../../state/appData.ts';
import { useApp, useEngine, usePartner, useScenario } from '../../state/EngineProvider.tsx';
import { useGoto } from '../../state/nav.ts';
import { toSpec } from '../../state/url.ts';
import { COMPARE_COLOR, INK, PILLAR_COLOR, SIGNAL, TIER_COLOR, tierKey } from '../../theme/palette.ts';
import { CountryInsightsLink, numOr, rowsOf, showWord, strOr, useCountryOptions, type Row } from '../common.tsx';
import { InvestmentCards } from './InvestmentCard.tsx';
import { SectorBars, sectorCols, sideValue, type SectorRow } from './SectorPanels.tsx';
import { dpOfCell, loadTies, type Ties } from './ties.ts';

/** The source of the investment tie in a sector (Friendshore's outbound component). */
const tieSource = (v: unknown): string | null => (v === 'cm_fdi' ? 'Factory projects (CM-FDI)' : v === 'imf_dip' ? 'IMF direct investment' : null);
const INDEX_NAME: Record<'Friendshore' | 'Opportunity', string> = { Friendshore: 'Friendshore', Opportunity: 'Partnership Opportunity' };

/**
 * The route from Development Potential to the raw data behind it in Sector Insights: in a sector, the
 * selected partner's aid and investment partners there; else the World Bank indicators.
 */
function DevDataNote({ iso3, name, sector }: { iso3: string | null; name: string | null; sector: { tech: string; stage: string } | null }) {
  return (
    <p className="text-sm">
      <Link className="link" to={devDataLink(iso3, sector)}>
        The development data behind this &rarr;
      </Link>
      <span className="text-paper-mute">
        {sector && iso3
          ? ` ${name ?? iso3}'s aid received and given, and its direct-investment positions, in Sector Insights`
          : ` World Bank development indicators, OECD aid flows and IMF direct-investment positions${name ? ` (${name} highlighted)` : ''}, in Sector Insights`}
      </span>
    </p>
  );
}

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

  // the sector choice (?tech=&stage=, shared with the other views): one technology x stage, or all sectors
  const sector = useMemo(() => {
    if (!view.tech || !view.stage) return null;
    const c = app.cells.findIndex((x) => x.tech === view.tech && x.supply_chain === view.stage);
    return c < 0 ? null : { tech: view.tech, stage: view.stage, c, label: cellLabel(view.tech, view.stage) };
  }, [view.tech, view.stage, app.cells]);
  const sectorOptions = useMemo(
    () => [
      { value: '', label: 'All sectors (Partnership Strength)' },
      ...cellOptions(app.cells, (x) => x.supply_chain, (x) => stageSlug(x.tech, x.supply_chain)),
    ],
    [app.cells],
  );
  const pickSector = (slug: string) => {
    const x = app.cells.find((c) => stageSlug(c.tech, c.supply_chain) === slug);
    setView(x ? { tech: x.tech, stage: x.supply_chain } : { tech: null, stage: null });
  };
  const [cellTab, setCellTab] = useState<{ key: string; table: Table | null; error: string | null } | null>(null);
  const cpKey = `${key}|${reporter}|${sector?.c ?? ''}`;
  useEffect(() => {
    if (!sector || !partner.fresh) return;
    let live = true;
    client.cellPartners(toSpec(scenario), reporter, sector.c).then(
      (t) => live && setCellTab({ key: cpKey, table: t, error: null }),
      (e: Error) => live && setCellTab({ key: cpKey, table: null, error: e.message }),
    );
    return () => {
      live = false;
    };
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [cpKey, partner.fresh, client]);
  // the auxiliary `ties` chunk (direct investment, factory projects, Development Potential by sector)
  const [ties, setTies] = useState<{ ties: Ties | null; error: string | null } | null>(null);
  useEffect(() => {
    let live = true;
    loadTies(app.manifest).then(
      (t) => live && setTies({ ties: t, error: t ? null : 'This data build has no direct-investment data.' }),
      (e: Error) => live && setTies({ ties: null, error: `Could not load the direct-investment data: ${e.message}` }),
    );
    return () => {
      live = false;
    };
  }, [app.manifest]);
  const secRows = useMemo((): SectorRow[] | null => {
    if (!sector || !cellTab?.table || cellTab.key !== cpKey) return null;
    const dp = ties?.ties ? dpOfCell(ties.ties, sector.c + 1) : new Map<string, number | null>();
    const psiBy = new Map(rows.map((r) => [r.partner_iso, r]));
    return rowsOf(cellTab.table).map((r) => {
      const iso = String(r.partner_iso);
      const c = app.byIso.get(iso);
      const d = psiBy.get(iso);
      return {
        partner_iso: iso,
        Country: c?.country ?? countryName(app, iso),
        tier: tierKey(c?.tier),
        bloc: c?.bloc ?? null,
        fs: numOr(r.fs), fs_base: numOr(r.fs_base), fs_rank: numOr(r.fs_rank), fs_rank_base: numOr(r.fs_rank_base),
        po: numOr(r.po), po_base: numOr(r.po_base), po_rank: numOr(r.po_rank), po_rank_base: numOr(r.po_rank_base),
        dp: dp.get(iso) ?? null,
        psi: d?.psi ?? null,
        psi_rank: d?.rank ?? null,
      };
    });
  }, [sector, cellTab, cpKey, ties, rows, app]);

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
        x: { label: change ? 'Change in Partnership Strength against the paper →' : 'Partnership Strength (0 to 1) →', grid: true, nice: true, domain: change ? symDomain(top20.map((r) => r.shown), 0.005).map((v) => v * 1.5) : [0, 1] },
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
  // with a sector chosen: Friendshore in that sector (scenario, or the paper in Baseline mode)
  const secBy = useMemo(() => new Map((secRows ?? []).map((r) => [r.partner_iso, r])), [secRows]);
  const blocY = useCallback(
    (r: DyadRow): number | null => {
      if (!sector) return show === 'baseline' ? r.friendshore_base : r.friendshore;
      const s = secBy.get(r.partner_iso);
      return s ? sideValue(s, 'fs', show === 'change' ? 'scenario' : show) : null;
    },
    [sector, secBy, show],
  );
  const blocPts = useMemo(
    () =>
      rows
        .filter((r) => r.bloc && BLOC_X[r.bloc] !== undefined && blocY(r) !== null)
        .map((r) => ({ ...r, x: BLOC_X[r.bloc as string] + (hash01(r.partner_iso) - 0.5) * 0.3, y: blocY(r) })),
    [rows, blocY],
  );
  const fsLabel = sector ? `Friendshore in ${sector.label}` : 'Friendshore (best five sectors)';
  const buildBloc = useCallback(
    (width: number): Plot.PlotOptions | null => {
      if (!blocPts.length) return null;
      return {
        width,
        height: 360,
        marginLeft: 48,
        x: { domain: [-0.25, 1.25], ticks: [0, 0.5, 1], tickFormat: (v: number) => (v === 0 ? 'China-leaning' : v === 0.5 ? 'Nonaligned' : 'U.S.-leaning'), label: "Partner's UN-voting bloc" },
        y: { domain: [0, 1], label: `↑ ${fsLabel}`, grid: true },
        marks: [
          Plot.dot(blocPts, { x: 'x', y: 'y', r: 4, fill: (r: DyadRow) => TIER_COLOR[r.tier], stroke: INK[800], strokeWidth: 1.5 }),
          Plot.dot(blocPts.filter((p) => p.partner_iso === sel), { x: 'x', y: 'y', r: 6, fill: 'none', stroke: SIGNAL, strokeWidth: 2.5 }),
          Plot.dot(blocPts, Plot.pointer({ x: 'x', y: 'y', r: 8, fill: 'none', stroke: SIGNAL, strokeWidth: 2, maxRadius: 24 })),
        ],
      };
    },
    [blocPts, sel, fsLabel],
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
        y: { domain: [0, 1], label: '↑ Score (0 to 1)', grid: true, ticks: 4 },
        color: { domain: ['Friendshore', 'Opportunity'], range: [PILLAR_COLOR.ES, PILLAR_COLOR.EO] },
        symbol: { domain: ['best five', 'other'], range: ['star', 'circle'] },
        marks: [
          ...(['Friendshore', 'Opportunity'] as const).map((ix) =>
            Plot.dot(pts.filter((p) => p.index === ix), {
              fx: 'stage', x: 'tech', y: 'value', fill: 'index', symbol: (p: CellPoint) => (p.best ? 'best five' : 'other'),
              r: (p: CellPoint) => (p.best ? 7 : 4), stroke: INK[800], strokeWidth: 1, dx: ix === 'Friendshore' ? -3 : 3,
            }),
          ),
          // the sector chosen above
          sector ? Plot.dot(pts.filter((p) => p.tech === sector.tech && p.stage === sector.stage), { fx: 'stage', x: 'tech', y: 'value', r: 11, fill: 'none', stroke: SIGNAL, strokeWidth: 1.5, strokeDasharray: '2 2' }) : null,
          Plot.dot(pts, Plot.pointer({ fx: 'stage', x: 'tech', y: 'value', r: 9, fill: 'none', stroke: SIGNAL, strokeWidth: 2, maxRadius: 20 })),
        ],
      };
    },
    [cellPts, app, sector],
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
  // the partner-level map's scale: the ranked partners (white = their median; Change: zero)
  const psiScale = useMemo(
    () => triScale(psiRows.filter((r) => r.rankable && r.shown !== null).map((r) => r.shown as number), { center: show === 'change' ? 'zero' : 'median' }),
    [psiRows, show],
  );
  const psiSide = (v: number | null) => {
    const sd = psiScale.side(v);
    if (sd === null) return null;
    if (show === 'change') return sd === 'above' ? 'rose against the paper' : sd === 'below' ? 'fell against the paper' : 'no change';
    return sd === 'median' ? `at the median of the ${psiScale.n} ranked partners (${fmtNum(psiScale.mid, 2)})` : `${sd} the median of the ${psiScale.n} ranked partners (${fmtNum(psiScale.mid, 2)})`;
  };
  const psiCells = useMemo(() => {
    const m = new Map<string, MapCell>();
    for (const r of psiRows) {
      const c = app.byIso.get(r.iso);
      if (!c?.iso_n3) continue;
      const ok = r.rankable && r.shown !== null;
      m.set(c.iso_n3, {
        iso3c: r.iso,
        fill: ok ? (psiScale.color(r.shown) ?? 'hatch') : 'hatch',
        tip: (
          <Tip
            value={show === 'change' ? fmtSigned(r.shown, 3) : fmtNum(r.shown, 3)}
            title={r.Country}
            lines={[
              ok ? psiSide(r.shown) : null,
              `rank ${r.rank ?? DASH}${!isBaseline && r.rank_delta ? ` (${r.rank_delta > 0 ? 'up' : 'down'} ${Math.abs(r.rank_delta)})` : ''}`,
              !r.rankable ? `Not ranked: ${r.reason ?? 'no reason recorded'}` : null,
            ]}
          />
        ),
      });
    }
    return m;
    // psiSide reads psiScale and show
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [psiRows, psiScale, app, show, isBaseline]);

  const dyTip = (r: DyadRow & { shown?: number | null }) => (
    <Tip
      value={`Partnership Strength ${fmtNum(r.psi, 3)}`}
      title={`${r.Country} (${tierLabel(r.tier)})`}
      lines={[
        `Friendshore ${fmtNum(r.friendshore, 3)} · Partnership Opportunity ${fmtNum(r.opportunity, 3)} · Development Potential ${fmtNum(r.development, 3)}`,
        `rank ${r.rank ?? DASH}${!isBaseline ? `; paper ${fmtNum(r.psi_base, 3)}, rank ${r.rank_base ?? DASH}` : ''}`,
        r.bloc ? `UN-voting bloc: ${r.bloc}` : null,
        !r.rankable ? `Not ranked: ${r.reason ?? 'no reason recorded'}` : null,
      ]}
    />
  );

  const rankCols: Col<DyadRow>[] = [
    { id: 'rank', header: 'Rank', value: (r) => r.rank, num: true },
    { id: 'Country', header: 'Partner', value: (r) => r.Country, minWidth: 150 },
    { id: 'tier', header: 'Tier', value: (r) => tierLabel(r.tier) },
    { id: 'psi', header: 'Partnership Strength', value: (r) => r.psi, cell: (r) => fmtNum(r.psi, 3), num: true },
    { id: 'friendshore', header: 'Friendshore (best five)', value: (r) => r.friendshore, cell: (r) => fmtNum(r.friendshore, 3), num: true },
    { id: 'opportunity', header: 'Partnership Opportunity (best five)', value: (r) => r.opportunity, cell: (r) => fmtNum(r.opportunity, 3), num: true },
    { id: 'development', header: 'Development Potential', value: (r) => r.development, cell: (r) => fmtNum(r.development, 3), num: true },
    { id: 'bloc', header: 'UN-voting bloc', value: (r) => r.bloc, minWidth: 110 },
    { id: 'rank_delta', header: 'Rank change', value: (r) => r.rank_delta, cell: (r) => <RankChip change={r.rank_delta} />, num: true },
    { id: 'rankable', header: 'Ranked', value: (r) => (r.rankable ? 'yes' : `no${r.reason ? `: ${r.reason}` : ''}`), minWidth: 300 },
  ];

  // the pair's components per sector: Friendshore (friendshoring) parts first, then Partnership Opportunity (market)
  const decCols: Col<Row>[] = [
    { id: 'cell', header: 'Sector', value: (r) => cellLabel(String(r.tech), String(r.supply_chain)), minWidth: 170 },
    { id: 'fs', header: 'Friendshore', value: (r) => numOr(r.friendshore_index), cell: (r) => fmtNum(r.friendshore_index, 3), num: true },
    { id: 'po', header: 'Partnership Opportunity', value: (r) => numOr(r.opportunity_index), cell: (r) => fmtNum(r.opportunity_index, 3), num: true },
    { id: 'best', header: 'In the best five of', value: (r) => [r.in_top5_fs === true ? 'Friendshore' : '', r.in_top5_po === true ? 'Partnership Opportunity' : ''].filter(Boolean).join(', ') || null, minWidth: 160 },
    { id: 'imp', header: 'Friendshore: imports from the partner', title: "How much the economy already buys from the partner in this sector (ranked)", value: (r) => numOr(r.fs_imp_trade_index), cell: (r) => fmtNum(r.fs_imp_trade_index, 3), num: true },
    { id: 'need', header: 'Friendshore: own security need', title: '1 minus the economy’s Energy Security in this sector', value: (r) => numOr(r.fs_es_need), cell: (r) => fmtNum(r.fs_es_need, 3), num: true },
    { id: 'eo_p', header: "Friendshore: partner's Economic Opportunity", value: (r) => numOr(r.fs_eo_partner), cell: (r) => fmtNum(r.fs_eo_partner, 3), num: true },
    { id: 'out', header: 'Friendshore: investment tie', title: 'Direct investment of the economy in the partner (ranked): IMF positions, or factory projects in four clean-tech manufacturing sectors', value: (r) => numOr(r.fs_outbound_index), cell: (r) => fmtNum(r.fs_outbound_index, 3), num: true },
    { id: 'out_src', header: 'Investment tie source', value: (r) => tieSource(r.fs_outbound_source), minWidth: 150 },
    { id: 'geo', header: 'Friendshore: UN-vote alignment', value: (r) => numOr(r.fs_geo_alignment), cell: (r) => fmtNum(r.fs_geo_alignment, 3), num: true },
    {
      id: 'cpe', header: "Friendshore: Chinese-parent share of the partner's foreign-owned factories",
      title: 'Share of the partner’s foreign-invested factories in this technology whose parent is headquartered in China, Hong Kong or Macao; Friendshore scores 1 minus the share (blank: fewer than two projects or not one of the four sectors)',
      value: (r) => numOr(r.fs_chinese_parent_exposure), cell: (r) => (numOr(r.fs_chinese_parent_exposure) === null ? DASH : fmtPct(r.fs_chinese_parent_exposure, 0)), num: true, minWidth: 150,
    },
    { id: 'pen', header: 'Friendshore: climate penalty (both sides)', value: (r) => (numOr(r.fs_penalty_reporter) ?? 0) + (numOr(r.fs_penalty_partner) ?? 0), cell: (r) => fmtNum((numOr(r.fs_penalty_reporter) ?? 0) + (numOr(r.fs_penalty_partner) ?? 0), 3), num: true },
    { id: 'fsraw', header: 'Friendshore before / after the penalty', value: (r) => numOr(r.fsi_raw), cell: (r) => `${fmtNum(r.fsi_raw, 3)} / ${fmtNum(r.fsi_adj, 3)}` },
    { id: 'trade', header: 'Partnership Opportunity: exports to the partner', value: (r) => numOr(r.po_trade_index), cell: (r) => fmtNum(r.po_trade_index, 3), num: true },
    { id: 'poeo', header: 'Partnership Opportunity: own Economic Opportunity', value: (r) => numOr(r.po_econ_opp_index), cell: (r) => fmtNum(r.po_econ_opp_index, 3), num: true },
    { id: 'poneed', header: "Partnership Opportunity: partner's security need", value: (r) => numOr(r.po_partner_es_need), cell: (r) => fmtNum(r.po_partner_es_need, 3), num: true },
    { id: 'popen', header: 'Partnership Opportunity: climate penalty', value: (r) => numOr(r.po_penalty), cell: (r) => fmtNum(r.po_penalty, 3), num: true },
    { id: 'poraw', header: 'Partnership Opportunity before / after the penalty', value: (r) => numOr(r.opportunity_raw), cell: (r) => `${fmtNum(r.opportunity_raw, 3)} / ${fmtNum(r.opportunity_adj, 3)}` },
    { id: 'excluded', header: 'Excluded', value: (r) => (r.excluded === true ? 'excluded (coal, oil or gas)' : '') },
  ];

  const loadingPartner = !partner.data;
  const dyUpdating = !partner.fresh || (dyads !== null && dyads.key !== dyKey);
  const rName = countryName(app, reporter);
  return (
    <div>
      <PageHead
        eyebrow="Partners"
        title="Partners"
        lede={
          <>
            How strong a partner each economy is for the one you pick, across energy supply chains. The <Term id="psi" /> combines{' '}
            <Term id="fs" /> (a friendshoring opportunity: a partner where it could invest and source to meet its own security need in a sector, because the partner has an economic opportunity there, the two already trade and invest with each other and they are aligned in UN votes),{' '}
            <Term id="po" /> (a market for its exports) and, for middle- and lower-income partners, <Term id="dp" />. Each pair is summarised by its{' '}
            <Term id="best_five">five strongest sectors</Term>.
          </>
        }
      >
        <CountryInsightsLink iso3={reporter} country={rName} />
      </PageHead>
      <FilterRow result={rows.length ? `${ranked.length} ranked partners of ${rows.length}` : undefined}>
        <CountrySelect label="Economy (whose partners)" value={reporter} options={options} onChange={(v) => goto(`/partners/${v}`)} />
        <Select label="Sector" value={sector ? stageSlug(sector.tech, sector.stage) : ''} options={sectorOptions} onChange={pickSector} className="min-w-[16rem]" />
      </FilterRow>
      {sector && (
        <p className="-mt-3 mb-5 max-w-3xl text-sm text-paper-dim" data-testid="sector-note">
          Showing <strong className="text-paper">{sector.label}</strong> only: {rName}&apos;s partners ranked on <Term id="fs" /> and <Term id="po" /> in this one
          sector (the dials apply), not on the five-sector summaries. {rName}&apos;s own security need is the same for every partner in one sector: it sets how much the
          sector matters to {rName}, so the Friendshore ranking turns on the partner&apos;s Economic Opportunity, the trade and investment ties, UN-vote alignment and
          ownership. Development Potential is shown for this sector at the paper baseline; the Partnership
          Strength rank is across all sectors. Choose &ldquo;All sectors&rdquo; for the Partnership Strength ranking.{' '}
          <Link className="link" to={devDataLink(sel, sector)} data-testid="dev-data-link">
            The development data behind this &rarr;
          </Link>
        </p>
      )}
      {loadingPartner ? (
        <Loading label="Loading partnership data" />
      ) : (
        <div className="flex flex-col gap-6">
          {dyads?.error && <Notice kind="error">{dyads.error}</Notice>}
          {sector && cellTab?.error && <Notice kind="error">{cellTab.error}</Notice>}
          {sector && (
            <div className="grid gap-6 xl:grid-cols-2">
              {(['fs', 'po'] as const).map((sd) => (
                <SectorBars key={sd} rows={secRows} side={sd} sector={sector.label} rName={rName} show={show} isBaseline={isBaseline} updating={!partner.fresh || (!!cellTab && cellTab.key !== cpKey)} onSelect={choose} />
              ))}
            </div>
          )}
          <div className={sector ? 'grid gap-6' : 'grid gap-6 xl:grid-cols-2'}>
            {!sector && (
            <ChartCard
              title={`${rName}: its 20 strongest partners`}
              sub={`${showWord(show)} Each bar is one partner's Partnership Strength from ${rName}'s point of view (0 to 1; longer is a stronger partner)${!isBaseline && show !== 'change' ? "; the gray tick is the paper's value" : ''}. Colour shows the partner's place in the coalition. Click a bar to see the pair sector by sector.`}
              updating={dyUpdating}
              summary={`The ${top20.length} strongest partners of ${rName} by Partnership Strength.`}
              legend={
                <Legend
                  items={[
                    { label: 'Coalition core', color: TIER_COLOR.core },
                    { label: 'Coalition partner', color: TIER_COLOR.partner },
                    { label: 'Other economy', color: TIER_COLOR.other },
                    ...(!isBaseline && show !== 'change' ? [{ label: "Paper's value (tick)", kind: 'tick' as const, color: COMPARE_COLOR.baseline }] : []),
                  ]}
                />
              }
              table={() => <TableView rows={top20} cols={rankCols} caption="The 20 strongest partners" pageSize={20} />}
            >
              {!dyads ? <Loading label="Loading dyads" /> : top20.length ? (
                <PlotFigure<DyadRow> build={buildTop} height={40 + top20.length * 22} ariaLabel="Strongest partners by Partnership Strength" tooltip={dyTip} onSelect={(r) => choose(r.partner_iso)} />
              ) : (
                <EmptyState>No partnership scores for this economy.</EmptyState>
              )}
            </ChartCard>
            )}
            <ChartCard
              title={sector ? `UN-vote alignment and Friendshore in ${sector.label}` : 'UN-vote alignment and Friendshore'}
              sub={`${sector && show === 'change' ? 'Scenario values.' : ''} Each dot is one partner: across, its bloc in UN General Assembly votes (dots are spread within each band so they do not overlap); up, how strong a friendshoring opportunity it is for ${rName} (${sector ? `Friendshore in ${sector.label}` : 'Friendshore, average of its five strongest sectors'}). Colour shows the partner's place in the coalition.`}
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
                  caption="UN-vote alignment and Friendshore"
                  search
                  cols={[
                    { id: 'Country', header: 'Partner', value: (r) => r.Country },
                    { id: 'bloc', header: 'UN-voting bloc', value: (r) => r.bloc },
                    { id: 'fs', header: fsLabel, value: (r) => r.y, cell: (r) => fmtNum(r.y, 3), num: true },
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

          {sector ? (
            <ChartCard
              title={`${rName}: every partner in ${sector.label}`}
              sub={`${rName}'s partners in ${sector.label}: Friendshore (as a friendshoring opportunity: where ${rName}'s security need meets the partner's economic opportunity, on existing trade and investment ties) and Partnership Opportunity (as an export market) in this sector under the scenario, with their ranks among sovereign economies, Development Potential in this sector (paper baseline) and the Partnership Strength rank across all sectors. Select a row to see the pair sector by sector.`}
              updating={!partner.fresh || (!!cellTab && cellTab.key !== cpKey)}
              notes={<DevDataNote iso3={sel} name={selRow?.Country ?? null} sector={sector} />}
              csv={secRows ? () => ({ name: `partners-${reporter}-${stageSlug(sector.tech, sector.stage)}`, text: rowsToCsv(secRows.map((r) => ({ ...r, sector: sector.label })) as unknown as Record<string, unknown>[]) }) : undefined}
            >
              {!secRows ? (
                <Loading label={`Scoring ${sector.label}`} />
              ) : (
                <TableView rows={secRows} cols={sectorCols(sector.label)} caption={`Partners in ${sector.label}`} search pageSize={15} initialSort={{ id: 'fs_rank' }} rowKey={(r) => r.partner_iso} onRowClick={(r) => choose(r.partner_iso)} selectedKey={sel} />
              )}
            </ChartCard>
          ) : (
          <ChartCard
            title={`${rName}: every partner, ranked`}
            sub={`All of ${rName}'s partners by Partnership Strength. Select a row to see the pair sector by sector. Rank change compares with the paper; the last column says why a partner is not ranked.`}
            updating={dyUpdating}
            notes={<DevDataNote iso3={sel} name={selRow?.Country ?? null} sector={null} />}
            csv={dyads?.table ? () => ({ name: `psi-dyads-${reporter}`, text: tableToCsv(dyads.table as Table) }) : undefined}
          >
            {!dyads ? (
              <Loading label="Loading dyads" />
            ) : (
              <TableView rows={rows} cols={rankCols} caption="Partner ranking" search pageSize={15} initialSort={{ id: 'rank' }} rowKey={(r) => r.partner_iso} onRowClick={(r) => choose(r.partner_iso)} selectedKey={sel} />
            )}
          </ChartCard>
          )}

          <ChartCard
            title={selRow ? `${rName} and ${selRow.Country}, sector by sector` : 'The pair, sector by sector'}
            sub={`For each sector: Friendshore (how strong a friendshoring opportunity ${selRow?.Country ?? 'the partner'} is for ${rName}: where ${rName}'s security need meets the partner's economic opportunity, on their trade and investment ties) and Partnership Opportunity (how good a market it is for ${rName}'s exports), both 0 to 1. Stars mark the five sectors that make up each summary. The table lists every part of both scores, including the investment tie (factory projects in Solar, Wind, Batteries and Electric Vehicles manufacturing, IMF direct investment elsewhere) and the Chinese-parent share of the partner's foreign-owned factories.`}
            updating={!!dec && dec.key !== decKey}
            legend={
              <Legend
                items={[
                  { label: 'Friendshore', kind: 'dot', color: PILLAR_COLOR.ES },
                  { label: 'Partnership Opportunity', kind: 'dot', color: PILLAR_COLOR.EO },
                  { label: 'Star = one of the five strongest sectors', kind: 'diamond', color: '#d1d5db' },
                  ...(sector ? [{ label: `Dashed ring = ${sector.label}`, kind: 'outline' as const, color: SIGNAL }] : []),
                ]}
              />
            }
            table={() => <TableView rows={rowsOf(dec?.table)} cols={decCols} caption="The pair's components by sector" pageSize={33} />}
            csv={dec?.table ? () => ({ name: `dyad-${reporter}-${sel}`, text: tableToCsv(dec.table as Table) }) : undefined}
          >
            {!sel ? (
              <EmptyState>Select a partner.</EmptyState>
            ) : !dec ? (
              <Loading label="Breaking the pair down by sector" />
            ) : dec.error ? (
              <Notice kind="error">{dec.error}</Notice>
            ) : (
              // 33 technology labels need ~640 px: on a phone the chart scrolls inside the card
              <div className="overflow-x-auto">
                <PlotFigure<CellPoint>
                  build={buildCells}
                  height={300}
                  minWidth={640}
                  ariaLabel="Friendshore and Partnership Opportunity by sector"
                  tooltip={(p) => (
                    <Tip value={fmtNum(p.value, 3)} title={`${INDEX_NAME[p.index]}: ${cellLabel(p.tech, p.stage)}`} lines={[p.best ? 'one of the five strongest sectors' : null, p.excluded ? 'excluded (coal, oil or gas)' : null]} />
                  )}
                />
              </div>
            )}
          </ChartCard>

          <InvestmentCards
            iso={reporter}
            name={rName}
            ties={ties?.ties ?? null}
            error={ties?.error ?? null}
            sector={sector ? { tech: sector.tech, stage: sector.stage } : null}
            nameOf={(iso) => countryName(app, iso)}
            selected={sel}
            onSelect={(iso) => rows.some((r) => r.partner_iso === iso) && choose(iso)}
          />

          <ChartCard
            title={`Partnership Strength as a partner, ${partner.data?.psi.view === 'world' ? 'all reporting economies' : 'core coalition'} view`}
            sub={`${showWord(show)} Each economy is coloured by how strongly the ${partner.data?.psi.reporters.length ?? 0} ${partner.data?.psi.view === 'world' ? 'reporting economies' : 'core coalition members'}, on average, rate it as a partner (the viewpoint is set under Advanced): ${show === 'change' ? 'orange fell against the paper, white no change, blue rose' : 'orange weaker than most ranked partners, white the middle, blue stronger'}. Hatched: not ranked (hover for why). Click an economy to see its own partners.`}
            updating={!partner.fresh}
            legend={
              <TriLegend
                scale={psiScale}
                label={show === 'change' ? 'Change in Partnership Strength against the paper' : 'Partnership Strength as a partner'}
                format={(v) => (show === 'change' ? fmtSigned(v, 2) : fmtNum(v, 2))}
                noun="ranked partners"
                {...(show === 'change' ? { lowWord: 'fell', highWord: 'rose' } : { lowWord: 'weaker', highWord: 'stronger' })}
                missing="Hatched: not ranked"
              />
            }
            table={() => (
              <TableView
                rows={psiRows}
                caption="Partnership Strength as a partner"
                search
                initialSort={{ id: 'rank' }}
                cols={[
                  { id: 'rank', header: 'Rank', value: (r) => r.rank, num: true },
                  { id: 'Country', header: 'Partner', value: (r) => r.Country, minWidth: 150 },
                  { id: 'psi', header: 'Partnership Strength', value: (r) => r.psi, cell: (r) => fmtNum(r.psi, 3), num: true },
                  { id: 'psi_base', header: 'Paper', value: (r) => r.psi_base, cell: (r) => fmtNum(r.psi_base, 3), num: true },
                  { id: 'side', header: 'Colour', value: (r) => (r.rankable && r.shown !== null ? psiSide(r.shown) : 'hatched'), minWidth: 220 },
                  { id: 'fs', header: 'Friendshore', value: (r) => r.friendshore, cell: (r) => fmtNum(r.friendshore, 3), num: true },
                  { id: 'po', header: 'Partnership Opportunity', value: (r) => r.opportunity, cell: (r) => fmtNum(r.opportunity, 3), num: true },
                  { id: 'dp', header: 'Development Potential', value: (r) => r.development, cell: (r) => fmtNum(r.development, 3), num: true },
                  { id: 'rd', header: 'Rank change', value: (r) => r.rank_delta, cell: (r) => <RankChip change={r.rank_delta} />, num: true },
                  { id: 'rankable', header: 'Ranked', value: (r) => (r.rankable ? 'yes' : `no${r.reason ? `: ${r.reason}` : ''}`) },
                ]}
              />
            )}
            csv={partner.data ? () => ({ name: 'psi-partner', text: tableToCsv(partner.data!.compare.psi_partner) }) : undefined}
          >
            <WorldMap cells={psiCells} ariaLabel="Map of Partnership Strength as a partner" onSelect={(iso) => app.Uindex.has(iso) && goto(`/partners/${iso}`)} selected={reporter} ringStyle="double" />
          </ChartCard>
        </div>
      )}
    </div>
  );
}

