// Explore, country-level measures (spec_sector.md 4.6 sections 1-6): the KPI strip with the full-width
// bloc bar; map + ranking (two columns at >= 1024 px; out-of-scope economies dimmed on the map); the
// distribution (beeswarm with world and coalition medians and a "0" line; <= 15 observations: the
// holders' bloc bar and a table); across sectors (share split per item under stage headings, the
// current item outlined; for cell measures the 11 x 3 grid of each cell's top 5 on one share scale);
// over time (group area with Value | Share, the highlight's lines with index = 100, a dumbbell for
// two-point measures; a click sets the year); the table with economies without data on request.

import { useMemo, useState } from 'react';
import { WorldMap } from '../../../charts/WorldMap.tsx';
import { RampLegend } from '../../../charts/legends.tsx';
import { absMax, divColor } from '../../../charts/theme.ts';
import { TableView } from '../../../components/TableView.tsx';
import { Checkbox, Segmented, StatTile } from '../../../components/ui.tsx';
import { rowsToCsv } from '../../../lib/csv.ts';
import { useMediaQuery } from '../../../shell/useMediaQuery.ts';
import { GROUP_COLOR } from '../../../theme/palette.ts';
import { STAGE_LABELS, STAGE_SLUGS, TECH_LABELS, TECH_SLUGS, type StageSlug } from '../../contract.ts';
import type { FactRow } from '../../data/facts.ts';
import type { GeoInfo } from '../../data/model.ts';
import { reasonText } from '../../derive/cells.ts';
import { basisWords, concentration, isWorldBasis, type Concentration } from '../../derive/concentration.ts';
import { sectorCsv } from '../../derive/csv.ts';
import { acrossRows, exploreRows, geoMark, inScope, isDiverging, kpis, poles, shareLegal, type ExploreRow } from '../../derive/explore.ts';
import { blocModeWords, blocNote, GROUP_SHORT, partOf, TRADE_BLOC_VARS } from '../../derive/groups.ts';
import { groupArea, normalizeArea, topLines, type Line } from '../../derive/series.ts';
import { stateOf, stateShort, stateWords } from '../../derive/states.ts';
import { slice } from '../../derive/summary.ts';
import { concentrationSummary } from '../../derive/text.ts';
import { lc, axisFormatter, fmtEffN, fmtHhi, fmtRank, fmtShare, fmtValue, listText, logAxisFormatter, ordinal } from '../../format.ts';
import { ChunkGate, concFromSummary, fmtFact, highlightOffset, highlightLabel, mapCells, SPLIT_COLUMNS, shareSplitRow, splitParts, SplitTable, statesPresent } from '../common.tsx';
import { useTrade } from '../data.ts';
import {
  Beeswarm, BlocBar, CellGrid, Dumbbell, GroupArea, GroupLegend, LabelledLines, LeagueBars, SectorChartCard, ShareSplitChart, StateLegend,
  type BeeswarmPoint, type CellGridTech, type DumbbellRow, type LeagueRow, type ShareSplitRow,
} from '../kit.tsx';
import { indexFor, timeIndex, type ExploreCtx } from './ctx.ts';

const SHARE_UNIT = { short: 'share', long: 'share of world', unit_kind: 'share' };

/** Value formatters of a measure in the explorer's Show-as mode. */
function useFormat(c: ExploreCtx, conc: Concentration | null) {
  const { model, v, item } = c;
  const share = c.as === 'share' && shareLegal(v) && !!conc && conc.W !== null;
  const unit = share ? SHARE_UNIT : model.unitOf(v, item?.idx ?? null);
  const shown = (x: number) => (share && conc?.W ? x / conc.W : x);
  const fmt = (x: number, ctx: 'tile' | 'table' | 'axis' = 'tile') => (share ? fmtShare(x) : fmtFact(model, v, item, x, ctx));
  return { share, unit, shown, fmt };
}

function KpiStrip({ c, rows, world }: { c: ExploreCtx; rows: FactRow[]; world: { value: number } | null }) {
  const { model, v, item, year, highlight, eu } = c;
  const k = useMemo(() => kpis(model, v, rows, world, highlight, year ?? undefined), [model, v, rows, world, highlight, year]);
  const conc = k.conc;
  const tiles: { key: string; label: string; value: string; sub: string }[] = [];
  const name = (g: number) => model.geos[g]?.name ?? '';
  const n = conc.rows.filter((r) => model.geos[r.geo]?.kind === 'country' || model.geos[r.geo]?.kind === 'eu').length;
  if (v.agg !== 'none' && conc.W !== null) {
    tiles.push({ key: 'w', label: isWorldBasis(conc.basis) ? 'World total' : `Total of the economies listed by ${v.source.short}`, value: fmtFact(model, v, item, conc.W), sub: `${conc.basis === 'source_world' ? `as reported by ${v.source.short}` : conc.basis === 'sum_all' ? 'sum of every economy' : conc.basis === 'one' ? 'shares sum to 1' : 'not a world total'} · ${year ?? ''}` });
    tiles.push({ key: 'coal', label: 'Coalition', value: fmtShare(conc.split.core + conc.split.partner), sub: `core ${fmtShare(conc.split.core)} · partners ${fmtShare(conc.split.partner)} · ${k.coalitionWith} of ${k.coalitionTotal} members with data` });
    tiles.push({ key: 'chn', label: 'China', value: k.china.share !== null ? fmtShare(k.china.share) : 'no value', sub: k.china.rank !== null ? `${ordinal(k.china.rank)} of ${n}` : `not in ${v.source.short}` });
    tiles.push({ key: 'top3', label: 'Top 3', value: fmtShare(conc.top3_share), sub: listText(k.top3.map((t) => name(t.geo))) });
    tiles.push({ key: 'hhi', label: 'Concentration', value: `HHI ${fmtHhi(conc.hhi, conc.hhi_lower_bound)}`, sub: `≈ ${fmtEffN(conc.eff_n)} effective economies` });
  } else {
    tiles.push({ key: 'med', label: 'Median', value: Number.isFinite(k.median) ? fmtFact(model, v, item, k.median) : '–', sub: `of ${rows.filter((r) => model.geos[r.geo]?.kind === 'country' && Number.isFinite(r.value)).length} economies` });
    tiles.push({ key: 'cmed', label: 'Coalition median', value: Number.isFinite(k.coalitionMedian) ? fmtFact(model, v, item, k.coalitionMedian) : '–', sub: `${k.coalitionWith} of ${k.coalitionTotal} members with data` });
    tiles.push({ key: 'chn', label: 'China', value: k.china.value !== null ? fmtFact(model, v, item, k.china.value) : 'no value', sub: k.china.rank !== null ? `${ordinal(k.china.rank)} of ${n}` : `not in ${v.source.short}` });
    tiles.push({ key: 'top3', label: 'Highest', value: k.top3[0] ? name(k.top3[0].geo) : '–', sub: listText(k.top3.slice(1).map((t) => name(t.geo))) });
  }
  if (highlight && k.highlight) {
    const h = k.highlight;
    tiles.push({
      key: 'hl',
      label: 'Highlight',
      value: h.value !== null ? `${highlight.name} ${fmtFact(model, v, item, h.value)}` : highlight.name,
      sub: h.value !== null ? [h.share !== null ? `${fmtShare(h.share)} ${basisWords(conc.basis, v.source.short)}` : null, h.rank !== null ? `${ordinal(h.rank)} of ${n}` : null].filter(Boolean).join(' · ') : stateWords(h.state, v, model, highlight),
    });
  }
  const off = highlight ? highlightOffset(model, conc, highlight.idx) : null;
  return (
    <section aria-label="Key figures" className="space-y-3">
      <div className="grid grid-cols-2 gap-3 md:grid-cols-3 xl:grid-cols-6" role="list" aria-label="Key figures">
        {tiles.map((t) => (
          <div key={t.key} role="listitem" title={`${v.label} · ${model.unitOf(v, item?.idx ?? null).long} · ${year ?? ''} · ${v.source.short}`}>
            <StatTile label={t.label} value={<span className="font-medium">{t.value}</span>} sub={t.sub} />
          </div>
        ))}
      </div>
      {conc.W !== null && v.agg === 'sum' && (
        <BlocBar parts={splitParts(conc)} ariaLabel={concentrationSummary(conc, model, v, highlight)} label={`Shares ${basisWords(conc.basis, v.source.short)}${eu === 'bloc' ? ' · EU-27 counted as one' : ''}`} highlight={highlight && off !== null ? { offset: off, label: highlightLabel(conc, highlight) } : undefined} />
      )}
    </section>
  );
}

function MapRanking({ c, rows, mapRows, world, conc, mode, inherited }: { c: ExploreCtx; rows: FactRow[]; mapRows: FactRow[]; world: { value: number } | null; conc: Concentration; mode: string; inherited: number }) {
  const { model, v, item, year, highlight, scope, eu } = c;
  const wide = useMediaQuery('(min-width: 1024px)');
  const { share, unit, shown, fmt } = useFormat(c, conc);
  const diverging = isDiverging(v);
  const scoped = useMemo(() => rows.filter((r) => { const g = model.geos[r.geo]; return !!g && (g.kind === 'country' || g.kind === 'eu') && inScope(g, scope); }), [rows, model, scope]);
  const sconc = useMemo(() => concentration(scoped, model.geos, world, v.world_basis), [scoped, model, world, v.world_basis]);
  const heavy = useMemo(() => {
    const xs = sconc.rows.map((r) => r.value).filter((x) => x > 0).sort((a, z) => a - z);
    return xs.length > 2 && xs[xs.length >> 1] > 0 && xs[xs.length - 1] / xs[xs.length >> 1] > 100;
  }, [sconc]);
  const log = !diverging && (c.scale === 'log' || (c.scale === null && (v.scale === 'log' || heavy))) && sconc.rows.every((r) => r.value > 0);
  const map = useMemo(() => {
    const div = diverging ? divColor(Math.max(absMax(rows.map((r) => r.value)), 1e-9)) : undefined;
    return mapCells(model, v, item, mapRows, { log, highlight, year: year ?? undefined, diverging: div, dimOut: (g) => !inScope(g, scope) });
  }, [diverging, rows, mapRows, model, v, item, log, highlight, year, scope]);
  const top = wide ? 15 : 10;
  const league: LeagueRow[] = [];
  sconc.rows.forEach((r, i) => {
    const g = model.geos[r.geo];
    const isH = highlight?.idx === r.geo;
    if (i >= top && !isH) return;
    const f = rows.find((x) => x.geo === r.geo);
    const st = f ? stateOf(f, g, v, null, year ?? undefined) : null;
    league.push({ id: g.code, label: g.name, group: geoMark(g), value: shown(r.value), display: `${fmt(shown(r.value))}${!share && r.share !== null ? ` · ${fmtShare(r.share)}` : ''}`, pinned: i >= top && isH, state: st?.old ? 'old' : ((st?.state ?? 'obs') as LeagueRow['state']), badge: g.re_export_hub ? 're-export hub' : f?.status === 'mirror' ? 'mirror' : st?.old && st.obsYear ? String(st.obsYear) : undefined });
  });
  if (highlight && !sconc.rows.some((r) => r.geo === highlight.idx)) league.push({ id: highlight.code, label: highlight.name, group: geoMark(highlight), value: NaN, display: inScope(highlight, scope) ? 'no value' : 'outside the selected economies', pinned: true, state: 'not_covered' });
  const pick = (code: string) => {
    const g = model.geoByCode.get(code);
    c.setHighlight(g && g.idx !== highlight?.idx ? g : null);
  };
  const tickFormat = useMemo(() => (share ? (x: number) => `${Math.round(x * 100)}%` : log ? logAxisFormatter(unit, v.display_scale) : axisFormatter(sconc.rows.map((r) => r.value), unit, v.display_scale)), [share, log, sconc, unit, v.display_scale]);
  const blocWords = eu === 'bloc' ? blocModeWords(mode as never, v.source.short) : null;
  return (
    <SectorChartCard
      title={`${v.short}${item && item.axis !== 'none' ? `: ${item.axis === 'cell' ? model.cellLabel(item) : item.label}` : ''}, ${year ?? ''}`}
      vars={[{ var: v.id, item: item?.slug ?? null, year }]}
      table={() => <ExploreTableBody c={c} rows={rows} world={world} absent={false} />}
      csv={() => ({ name: `${v.id}-${item?.slug ?? 'all'}-${year ?? ''}-${c.scope}`, text: rowsCsv(c, exploreRows(model, v, rows, world, year, scope, false)) })}
      legend={
        <div className="space-y-2">
          {diverging ? (
            <p className="text-xs text-paper-mute">
              Map colours diverge around zero: {poles(v).negLabel.replace(' ←', '')} | {poles(v).posLabel.replace('→ ', '')}.
            </p>
          ) : (
            <RampLegend kind="seq" min={map.lo} max={map.hi} label={`${model.unitOf(v, item?.idx ?? null).short || model.unitOf(v, item?.idx ?? null).long}${year ? `, ${year}` : ''}${log ? ' (log scale)' : ''}`} format={(x) => fmtValue(log ? Math.pow(10, x) : x, model.unitOf(v, item?.idx ?? null), { scale: v.display_scale })} />
          )}
          <GroupLegend groups={['china', 'core', 'partner', 'rest']} highlight={highlight?.name ?? null} euBloc={eu === 'bloc'} />
          <StateLegend states={statesPresent(model, v, rows, year ?? undefined)} source={v.source.short} />
        </div>
      }
      notes={
        <>
          {scope !== 'all' && <p>The ranking covers the selected economies; the map shows all, the others dimmed.</p>}
          {inherited > 0 && <p>{inherited} territories without their own UN votes take their sovereign's position (basis "inherited"): shown on the map, not ranked or counted in the figures.</p>}
          {eu === 'bloc' && <p>{blocNote(model)}</p>}
          {blocWords && <p>{blocWords}</p>}
        </>
      }
      summary={concentrationSummary(sconc, model, v, highlight)}
    >
      <div className="grid gap-6 lg:grid-cols-5">
        <div className="lg:col-span-3">
          <WorldMap cells={map.cells} ariaLabel={`Map of ${v.label}${year ? `, ${year}` : ''}`} selected={highlight?.code ?? null} onSelect={pick} />
        </div>
        <div className="lg:col-span-2">
          {league.length ? (
            <LeagueBars rows={league} axisLabel={`${share ? 'Share of world' : unit.long || unit.short}${year ? `, ${year}` : ''}`} selected={highlight?.code ?? null} onSelect={(r) => pick(r.id)} top={top} diverging={diverging ? poles(v) : undefined} ariaLabel={`${v.label}, ranked`} tickFormat={tickFormat} log={log && !share} />
          ) : (
            <p className="text-sm text-paper-mute">No values for the selected economies.</p>
          )}
        </div>
      </div>
    </SectorChartCard>
  );
}

function Distribution({ c, rows, conc }: { c: ExploreCtx; rows: FactRow[]; conc: Concentration }) {
  const { model, v, item, year, highlight, scope } = c;
  const { share, shown, fmt } = useFormat(c, conc);
  const scoped = rows.filter((r) => model.geos[r.geo]?.kind === 'country' && inScope(model.geos[r.geo], scope) && Number.isFinite(r.value));
  // a 0 is "reported zero" only for additive measures; a bounded 0 (UN-vote position) is an observation
  const zeros = v.agg === 'sum' ? scoped.filter((r) => r.value === 0).length : 0;
  const vals = scoped.map((r) => r.value).sort((a, z) => a - z);
  const k = useMemo(() => kpis(model, v, scoped, null, null, year ?? undefined), [model, v, scoped, year]);
  const heavy = vals.length > 2 && vals[vals.length >> 1] > 0 && vals[vals.length - 1] / vals[vals.length >> 1] > 100;
  const log = !isDiverging(v) && (c.scale === 'log' || (c.scale === null && (v.scale === 'log' || heavy)));
  if (scoped.length <= 15) {
    if (!scoped.length) return null;
    return (
      <SectorChartCard title="Distribution" vars={[{ var: v.id, item: item?.slug ?? null, year }]} csv={() => ({ name: `${v.id}-${item?.slug ?? 'all'}-${year ?? ''}-holders`, text: sectorCsv(model, scoped.map((r) => ({ geo: model.geos[r.geo], v, item, year: r.obs_year ?? year, value: r.value, status: r.status }))) })} summary={`${scoped.length} economies with a value: too few for a distribution, so the holders are listed.`}>
        {conc.W !== null && v.agg === 'sum' && <BlocBar parts={splitParts(conc)} ariaLabel="Shares by group" />}
        <div className="mt-3">
          <TableView rows={[...scoped].sort((a, z) => z.value - a.value)} caption="Holders" cols={[{ id: 'e', header: 'Economy', value: (r) => model.geos[r.geo].name }, { id: 'g', header: 'Group', value: (r) => GROUP_SHORT[partOf(model.geos[r.geo])] }, { id: 'v', header: 'Value', value: (r) => r.value, cell: (r) => fmt(shown(r.value), 'table'), num: true }]} />
        </div>
      </SectorChartCard>
    );
  }
  const pts: BeeswarmPoint[] = scoped
    .filter((r) => !log || r.value > 0)
    .map((r) => {
      const g = model.geos[r.geo];
      const st = stateOf(r, g, v, null, year ?? undefined);
      return { id: g.code, label: g.name, group: geoMark(g), value: shown(r.value), display: fmt(shown(r.value)), state: st.old ? 'old' : (st.state as BeeswarmPoint['state']), labelled: g.code === 'CHN' || g.idx === highlight?.idx };
    });
  const medians = [
    ...(Number.isFinite(k.median) ? [{ label: 'World median', value: shown(k.median) }] : []),
    ...(Number.isFinite(k.coalitionMedian) ? [{ label: 'Coalition median', value: shown(k.coalitionMedian) }] : []),
  ];
  return (
    <SectorChartCard
      title="Distribution"
      vars={[{ var: v.id, item: item?.slug ?? null, year }]}
      table={() => <TableView rows={[...pts].sort((a, z) => z.value - a.value)} caption={`Distribution of ${lc(v.label)}`} search rowKey={(p) => p.id} cols={[{ id: 'e', header: 'Economy', value: (p) => p.label }, { id: 'g', header: 'Group', value: (p) => GROUP_SHORT[p.group] }, { id: 'v', header: share ? 'Share of world' : `Value (${model.unitOf(v, item?.idx ?? null).short})`, value: (p) => p.value, cell: (p) => p.display, num: true }, { id: 's', header: 'State', value: (p) => (p.state === 'old' ? 'old' : p.state ?? 'observed') }]} />}
      csv={() => ({ name: `${v.id}-${item?.slug ?? 'all'}-${year ?? ''}-distribution`, text: sectorCsv(model, scoped.map((r) => ({ geo: model.geos[r.geo], v, item, year: r.obs_year ?? year, value: r.value, status: r.status }))) })}
      legend={<GroupLegend groups={['china', 'core', 'partner', 'rest']} highlight={highlight?.name ?? null} />}
      notes={<p>{zeros ? `${zeros} economies report zero (not drawn${log ? ' on the log scale' : ''}). ` : ''}Faded dots are old observations.</p>}
      summary={`${pts.length} economies; world median ${Number.isFinite(k.median) ? fmt(shown(k.median)) : '–'}, coalition median ${Number.isFinite(k.coalitionMedian) ? fmt(shown(k.coalitionMedian)) : '–'}.`}
    >
      <Beeswarm points={pts} medians={medians} log={log && !share} zeroStrip={v.agg === 'sum'} xLabel={`${share ? 'Share of world' : model.unitOf(v, item?.idx ?? null).long}${year ? `, ${year}` : ''}${log ? ' (log scale)' : ''}`} ariaLabel={`Distribution of ${lc(v.label)}`} selected={highlight?.code ?? null} onSelect={(p) => { const g = model.geoByCode.get(p.id); c.setHighlight(g && g.idx !== highlight?.idx ? g : null); }} />
    </SectorChartCard>
  );
}

const SECTION_WORDS: Record<string, string> = { upstream: 'Upstream', midstream: 'Midstream', downstream: 'Downstream', minerals: 'Minerals', fuels: 'Fuels', commoditys: 'Commodities', activitys: 'Activities' };

export function AcrossSectors({ c }: { c: ExploreCtx }) {
  const { model, v, item, year, eu } = c;
  const rows = useMemo(() => acrossRows(model, v, eu, year), [model, v, eu, year]);
  const split: ShareSplitRow[] = rows.map((r) =>
    shareSplitRow(model, v, r.item, concFromSummary(r.s), { id: r.item.slug, label: r.item.axis === 'cell' ? model.cellLabel(r.item) : r.item.label, sub: `${v.source.short} ${r.s.year}`, year: r.s.year, section: r.section, outlined: r.item.idx === item?.idx }),
  );
  if (split.length < 2) return null;
  const sections = [...new Set(rows.map((r) => r.section))].map((s) => ({ key: s, label: SECTION_WORDS[s] ?? s, stage: (STAGE_SLUGS as readonly string[]).includes(s) ? (s as StageSlug) : null }));
  return (
    <div className="space-y-6">
      <SectorChartCard
        title={`${v.short} across sectors`}
        vars={[{ var: v.id, year }]}
        table={() => <SplitTable rows={split} caption={`${v.short} across sectors`} />}
        subExtra={` · Each row is one ${v.item_axis === 'cell' ? 'sector' : v.item_axis}, latest year of the source; the current one is outlined. Shares are within each row and never added across rows.`}
        csv={() => ({ name: `${v.id}-across-sectors-${eu}`, text: rowsToCsv(rows.map((r) => ({ item: r.item.slug, var_id: v.id, year: r.s.year, eu_mode: eu, world_total: r.s.W * v.display_scale, china: r.s.china, core: r.s.core, partner: r.s.partner, rest: r.s.rest, unattributed: r.s.unattributed, top1: model.geos[r.s.top1_geo ?? -1]?.code ?? null, top1_share: r.s.top1_share, top3_share: r.s.top3_share, hhi: r.s.hhi, hhi_lower_bound: r.s.hhi_lower_bound, eff_n: r.s.eff_n, source: v.source.short }))) })}
        legend={<GroupLegend />}
        summary={`${split.length} ${v.item_axis === 'cell' ? 'sectors' : `${v.item_axis}s`} with group shares of ${lc(v.short)}.`}
      >
        <ShareSplitChart rows={split} columns={SPLIT_COLUMNS} sections={sections} ariaLabel={`${v.short} by ${v.item_axis === 'cell' ? 'sector' : v.item_axis}`} onRowClick={(r) => { const it = rows.find((x) => x.item.slug === r.id)?.item; if (it) c.setItem(it); }} />
      </SectorChartCard>
      {v.item_axis === 'cell' && c.facts && <CellTops c={c} />}
    </div>
  );
}

/** The 11 x 3 grid: each cell's top 5 holders on one share-of-world scale; cells without data say why. */
function CellTops({ c }: { c: ExploreCtx }) {
  const { model, v, eu, item } = c;
  const facts = c.facts!;
  const tops = useMemo(() => {
    const m = new Map<string, { geo: GeoInfo; share: number }[]>();
    for (const cell of model.cells) {
      const y = model.summaryLatest(cell.idx, v.idx, eu)?.year ?? null;
      if (y === null || !facts.has(v.idx, cell.idx, y)) continue;
      const sl = slice(model, facts, v, cell.idx, y, eu);
      const cc = concentration(sl.rows, model.geos, sl.world, sl.basis);
      if (cc.W === null) continue;
      m.set(cell.slug, cc.rows.filter((r) => r.share !== null && r.value > 0).slice(0, 5).map((r) => ({ geo: model.geos[r.geo], share: r.share as number })));
    }
    return m;
  }, [model, v, eu, facts]);
  if (!tops.size) return null;
  const max = Math.max(...[...tops.values()].flatMap((xs) => xs.map((x) => x.share)), 1e-9);
  const techs: CellGridTech[] = TECH_SLUGS.filter((t) => model.cells.some((x) => x.techSlug === t)).map((t) => ({ slug: t, label: TECH_LABELS[t], headline: t !== 'semiconductors' }));
  return (
    <SectorChartCard
      title={`Top five in each sector: ${lc(v.short)}`}
      vars={[{ var: v.id }]}
      table={() => <TableView rows={[...tops.entries()].flatMap(([cell, xs]) => xs.map((x, i) => ({ cell, rank: i + 1, x })))} caption="Top five in each sector" search pageSize={25} cols={[{ id: 'c', header: 'Sector', value: (r) => { const it = model.item('cell', r.cell); return it ? model.cellLabel(it) : r.cell; } }, { id: 'r', header: 'Rank', value: (r) => r.rank, num: true }, { id: 'e', header: 'Economy', value: (r) => r.x.geo.name }, { id: 's', header: 'Share of world', value: (r) => r.x.share, cell: (r) => fmtShare(r.x.share), num: true }]} />}
      subExtra={` · One share-of-world scale across the grid (longest bar ${fmtShare(max)}).`}
      csv={() => ({ name: `${v.id}-top5-by-cell-${eu}`, text: rowsToCsv([...tops.entries()].flatMap(([cell, xs]) => xs.map((x, i) => ({ item: cell, rank: i + 1, iso3c: x.geo.code, economy: x.geo.name, group: x.geo.group, share_of_world: x.share, var_id: v.id, source: v.source.short })))) })}
      summary={`The five largest holders of ${lc(v.short)} in each of ${tops.size} sectors.`}
    >
      <CellGrid
        techs={techs}
        stages={STAGE_SLUGS.map((s) => ({ slug: s, label: STAGE_LABELS[s] }))}
        ariaLabel={`Top five holders of ${lc(v.short)} by sector`}
        selected={item?.axis === 'cell' ? item.slug : null}
        renderTile={(t, stage) => {
          const cell = model.cell(t.slug as never, stage);
          if (!cell) return <div className="min-h-[96px]" aria-hidden="true" />;
          const xs = tops.get(cell.slug);
          if (!xs) {
            const fam = model.coverage.get(`${cell.idx}|${v.family}`);
            return <div className="min-h-[96px] border hairline p-2 text-xs text-paper-mute">{fam && fam.status !== 'country' ? reasonText(fam.reason) : 'No data for this sector.'}</div>;
          }
          return (
            <button type="button" className={`block min-h-[96px] w-full border p-2 text-left text-[11px] ${item?.idx === cell.idx ? 'border-signal' : 'hairline'}`} aria-label={`${model.cellLabel(cell)}: ${xs.map((x) => `${x.geo.name} ${fmtShare(x.share)}`).join(', ')}`} onClick={() => c.setItem(cell)}>
              <ul className="space-y-0.5" aria-hidden="true">
                {xs.map((x) => (
                  <li key={x.geo.code} className="grid grid-cols-[2.5rem_1fr] items-center gap-1">
                    <span className="truncate text-paper-dim">{x.geo.code}</span>
                    <span className="flex items-center gap-1">
                      <span className="inline-block h-2" style={{ width: `${(70 * x.share) / max}%`, background: GROUP_COLOR[geoMark(x.geo)] }} />
                      <span className="tabular text-paper-mute">{Math.round(x.share * 100)}%</span>
                    </span>
                  </li>
                ))}
              </ul>
            </button>
          );
        }}
      />
    </SectorChartCard>
  );
}

function OverTime({ c }: { c: ExploreCtx }) {
  const { model, v, item, highlight, years } = c;
  const idx = timeIndex(c);
  const [mode, setMode] = useState<'value' | 'share'>('value');
  const [index100, setIndex100] = useState(false);
  if (!idx || !item) return null;
  const yrs = idx.yearsOf(v.idx, item.idx);
  const unit = model.unitOf(v, item.idx);
  const fmt = (x: number) => fmtValue(x, unit, { scale: v.display_scale });
  const pickX = (x: number) => {
    if (years.includes(x)) c.setYear(x);
  };
  if (yrs.length === 2) {
    const [y0, y1] = yrs;
    const projLater = v.projection_from !== null && y1 >= v.projection_from;
    const r1 = idx.rows(v.idx, item.idx, y1).filter((r) => model.geos[r.geo]?.kind === 'country' && Number.isFinite(r.value));
    const top = [...r1].sort((a, z) => z.value - a.value).slice(0, 20);
    if (highlight && !top.some((r) => r.geo === highlight.idx)) {
      const h = r1.find((r) => r.geo === highlight.idx);
      if (h) top.push(h);
    }
    const rows: DumbbellRow[] = top.map((r) => {
      const g = model.geos[r.geo];
      const a0 = idx.value(v.idx, item.idx, y0, r.geo);
      const v0 = a0 && Number.isFinite(a0.value) ? a0.value : NaN;
      return projLater
        ? { id: g.code, label: g.name, a: r.value, b: v0, group: geoMark(g), aLabel: `${fmt(r.value)} (${y1}, projection)`, bLabel: `${Number.isFinite(v0) ? fmt(v0) : '–'} (${y0})` }
        : { id: g.code, label: g.name, a: v0, b: r.value, group: geoMark(g), aLabel: `${Number.isFinite(v0) ? fmt(v0) : '–'} (${y0})`, bLabel: `${fmt(r.value)} (${y1})` };
    });
    if (!rows.length) return null;
    return (
      <SectorChartCard
        title={`${v.short}, ${y0} and ${y1}${projLater ? ' (projection)' : ''}`}
        vars={[{ var: v.id, item: item.slug, year: y0 }, { var: v.id, item: item.slug, year: y1 }]}
        table={() => <TableView rows={rows} caption={`${v.short}, ${y0} and ${y1}`} cols={[{ id: 'e', header: 'Economy', value: (r) => r.label }, { id: 'a', header: projLater ? `${y1} (projection)` : String(y0), value: (r) => r.a, cell: (r) => (Number.isFinite(r.a) ? fmt(r.a) : '–'), num: true }, { id: 'b', header: projLater ? String(y0) : String(y1), value: (r) => r.b, cell: (r) => (Number.isFinite(r.b) ? fmt(r.b) : '–'), num: true }]} />}
        csv={() => ({ name: `${v.id}-${item.slug}-${y0}-${y1}`, text: sectorCsv(model, top.flatMap((r) => { const g = model.geos[r.geo]; const a0 = idx.value(v.idx, item.idx, y0, r.geo); return [{ geo: g, v, item, year: y0, value: a0?.value ?? null, status: a0?.status ?? null }, { geo: g, v, item, year: y1, value: r.value, status: r.status }]; })) })}
        notes={v.id.startsWith('trade.') ? <p>Nominal US dollars; 2020 was a COVID trough.</p> : undefined}
        summary={`The ${rows.length} largest economies by ${lc(v.short)} in ${y1}, against ${y0}.`}
      >
        <Dumbbell rows={rows} xLabel={unit.long || unit.short} aName={projLater ? `${y1} (projection)` : String(y0)} bName={projLater ? String(y0) : String(y1)} ariaLabel={`${v.short}, ${y0} and ${y1}`} selected={highlight?.code ?? null} tickFormat={axisFormatter(rows.flatMap((r) => [r.a, r.b]), unit, v.display_scale)} />
      </SectorChartCard>
    );
  }
  if (yrs.length < 3) return null;
  const win: [number, number] = [yrs[0], yrs[yrs.length - 1]];
  const additive = v.agg === 'sum' && v.world_basis !== 'none';
  const area = additive ? groupArea(idx, model, v.idx, item.idx, yrs) : [];
  const areaShown = mode === 'share' ? normalizeArea(area) : area;
  const lines: Line[] = topLines(idx, model, v.idx, item.idx, { n: highlight ? 3 : 5, years: yrs, extra: highlight ? [highlight.idx] : [], rest: !additive }).filter((l) => l.geo !== null || !index100);
  return (
    <div className="space-y-6">
      {additive && area.length > 0 && (
        <SectorChartCard
          title={`${v.short} by group, ${win[0]}-${win[1]}`}
          vars={[{ var: v.id, item: item.slug, window: win }]}
          tools={<Segmented label="Show" hideLabel value={mode} options={[{ value: 'value', label: 'Value' }, { value: 'share', label: 'Share' }]} onChange={setMode} />}
          table={() => <TableView rows={yrs} caption="By group and year" cols={[{ id: 'y', header: 'Year', value: (y) => y }, ...(['china', 'core', 'partner', 'rest', 'unattributed'] as const).map((k) => ({ id: k, header: GROUP_SHORT[k], value: (y: number) => areaShown.find((p) => p.x === y && p.group === k)?.value ?? null, cell: (y: number) => { const x = areaShown.find((p) => p.x === y && p.group === k)?.value; return x === undefined ? '–' : mode === 'share' ? fmtShare(x) : fmt(x); }, num: true }))]} />}
          csv={() => ({ name: `${v.id}-${item.slug}-by-group`, text: rowsToCsv(yrs.map((y) => ({ year: y, var_id: v.id, item: item.slug, mode, ...Object.fromEntries(areaShown.filter((p) => p.x === y).map((p) => [p.group, p.value])), source: v.source.short }))) })}
          legend={<GroupLegend estimate={area.some((p) => p.est)} />}
          notes={<p>Click a year to show it above.</p>}
          summary={`${v.short} by group from ${win[0]} to ${win[1]}.`}
        >
          <GroupArea series={areaShown} normalize={mode === 'share'} yLabel={mode === 'share' ? 'Share of the world total' : unit.long || unit.short} ariaLabel={`${v.short} by group over time`} yTickFormat={mode === 'share' ? (x) => `${Math.round(x * 100)}%` : axisFormatter(area.map((p) => p.value), unit, v.display_scale)} valueFormat={(x) => (mode === 'share' ? fmtShare(x) : fmt(x))} onPickX={pickX} />
        </SectorChartCard>
      )}
      {lines.length > 0 && (
        <SectorChartCard
          title={`${highlight ? `${highlight.name} and the largest economies` : 'The largest economies'}, ${win[0]}-${win[1]}`}
          vars={[{ var: v.id, item: item.slug, window: win }]}
          tools={<Checkbox label="Index, first year = 100" checked={index100} onChange={setIndex100} />}
          table={() => <TableView rows={yrs} caption="By economy and year" cols={[{ id: 'y', header: 'Year', value: (y) => y }, ...lines.map((l) => ({ id: l.id, header: l.label, value: (y: number) => l.points.find((p) => p.x === y)?.y ?? null, cell: (y: number) => { const x = l.points.find((p) => p.x === y)?.y; return x === undefined ? '–' : fmt(x); }, num: true }))]} />}
          csv={() => ({ name: `${v.id}-${item.slug}-lines`, text: sectorCsv(model, lines.flatMap((l) => l.points.map((p) => ({ geo: l.geo !== null ? model.geos[l.geo] : null, economy: l.geo === null ? l.label : undefined, v, item, year: p.x, value: p.y, status: p.est ? 'est' : 'obs' })))) })}
          legend={<GroupLegend groups={['china', 'core', 'partner', 'rest']} highlight={highlight?.name ?? null} estimate={lines.some((l) => l.points.some((p) => p.est))} />}
          summary={lines[0] ? `${lines[0].label} is the largest in ${win[1]}.` : ''}
        >
          <LabelledLines lines={lines.map((l) => ({ id: l.id, label: l.label, group: l.group, points: l.points }))} highlight={highlight?.code ?? null} index={index100} yLabel={unit.long || unit.short} ariaLabel={`${v.short} of the largest economies over time`} valueFormat={fmt} yTickFormat={axisFormatter(lines.flatMap((l) => l.points.map((p) => p.y)), unit, v.display_scale)} onPickX={pickX} />
        </SectorChartCard>
      )}
    </div>
  );
}

function rowsCsv(c: ExploreCtx, rows: ExploreRow[]): string {
  return sectorCsv(c.model, rows.map((r) => ({ geo: r.g, v: c.v, item: c.item, year: r.year, value: r.value, status: r.status || (r.state.state === 'not_covered' ? 'not_covered' : r.state.state === 'none' ? 'none' : ''), share: r.share, rank: r.rank })));
}

function ExploreTableBody({ c, rows, world, absent }: { c: ExploreCtx; rows: FactRow[]; world: { value: number } | null; absent: boolean }) {
  const { model, v, item, year, scope, highlight } = c;
  const data = useMemo(() => exploreRows(model, v, rows, world, year, scope, absent), [model, v, rows, world, year, scope, absent]);
  const unit = model.unitOf(v, item?.idx ?? null);
  return (
    <TableView
      rows={data}
      caption={`${v.label}${year ? `, ${year}` : ''}`}
      search
      pageSize={25}
      rowKey={(r) => r.g.code}
      selectedKey={highlight?.code ?? null}
      onRowClick={(r) => c.setHighlight(r.g.idx === highlight?.idx ? null : r.g)}
      cols={[
        { id: 'e', header: 'Economy', value: (r) => r.g.name, minWidth: 150 },
        { id: 'g', header: 'Group', value: (r) => GROUP_SHORT[partOf(r.g)] },
        { id: 'eu', header: 'EU', value: (r) => (r.g.kind === 'eu' ? 'EU-27' : r.g.eu_member ? 'member' : '') },
        { id: 'v', header: `Value (${unit.short || unit.long})`, value: (r) => r.value, cell: (r) => (r.value !== null ? fmtValue(r.value, unit, { scale: v.display_scale, ctx: 'table' }) : stateShort(r.state, v)), num: true },
        ...(v.world_basis !== 'none' ? [{ id: 's', header: 'Share', value: (r: ExploreRow) => r.share, cell: (r: ExploreRow) => fmtShare(r.share), num: true }] : []),
        { id: 'r', header: 'Rank', value: (r) => r.rank, cell: (r) => fmtRank(r.rank), num: true },
        { id: 'y', header: 'Year', value: (r) => r.year, num: true },
        { id: 'st', header: 'State', value: (r) => stateShort(r.state, v) },
        { id: 'f', header: 'Source flag', value: (r) => (r.g.re_export_hub ? 're-export hub' : r.status === 'mirror' ? "partners' declarations" : r.status === 'est' ? 'includes an estimate' : null) },
      ]}
    />
  );
}

function ExploreTable({ c, rows, world }: { c: ExploreCtx; rows: FactRow[]; world: { value: number } | null }) {
  const [absent, setAbsent] = useState(false);
  const { model, v, item, year, scope } = c;
  return (
    <SectorChartCard
      title="Table"
      vars={[{ var: v.id, item: item?.slug ?? null, year }]}
      tools={<Checkbox label="Show economies without data" checked={absent} onChange={setAbsent} />}
      csv={() => ({ name: `${v.id}-${item?.slug ?? 'all'}-${year ?? ''}-table`, text: rowsCsv(c, exploreRows(model, v, rows, world, year, scope, absent)) })}
      summary={`${v.label} for every economy${scope !== 'all' ? ' in the selected group' : ''}.`}
    >
      <ExploreTableBody c={c} rows={rows} world={world} absent={absent} />
    </SectorChartCard>
  );
}

export function CountryLayout({ c }: { c: ExploreCtx }) {
  const { model, v, item, year, eu } = c;
  const idx = indexFor(c, year);
  const needTrade = eu === 'bloc' && TRADE_BLOC_VARS.has(v.id) && !!item?.techSlug;
  const { trade, h: th } = useTrade(item?.techSlug ?? null, needTrade);
  const sl = useMemo(() => (idx && item && year !== null ? slice(model, idx, v, item.idx, year, eu, trade) : null), [idx, item, year, model, v, eu, trade]);
  // territories that inherit their sovereign's UN-vote position are copies, never ranked as country data
  const ranked = useMemo(() => (sl ? sl.rows.filter((r) => model.geos[r.geo]?.alignment_basis !== 'inherited' || v.id !== 'development.alignment') : []), [sl, model, v.id]);
  const inherited = sl ? sl.rows.length - ranked.length : 0;
  const conc = useMemo(() => (sl ? concentration(ranked, model.geos, sl.world, sl.basis) : null), [sl, ranked, model]);
  if (!item) return <p className="text-sm text-paper-mute">No item with data for this measure.</p>;
  if (!sl || !conc) return <p className="text-sm text-paper-mute">No values for {year ?? 'this year'}.</p>;
  return (
    <div className="space-y-8">
      {needTrade && !trade && (
        <ChunkGate h={th} name={`sector-trade-${item.techSlug}`}>
          {() => null}
        </ChunkGate>
      )}
      <KpiStrip c={c} rows={ranked} world={sl.world} />
      <MapRanking c={c} rows={ranked} mapRows={sl.rows} world={sl.world} conc={conc} mode={sl.mode} inherited={inherited} />
      <Distribution c={c} rows={ranked} conc={conc} />
      <AcrossSectors c={c} />
      <OverTime c={c} />
      <ExploreTable c={c} rows={sl.rows} world={sl.world} />
    </div>
  );
}
