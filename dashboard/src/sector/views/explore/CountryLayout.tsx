// Explore, country-level measures (spec_sector.md 4.6 sections 1-6): the KPI strip with the full-width
// bloc bar; map + ranking (two columns at >= 1024 px; out-of-scope economies dimmed on the map); the
// distribution (beeswarm with world and coalition medians and a "0" line; <= 15 observations: the
// holders' bloc bar and a table); across sectors (share split per item under stage headings, the
// current item outlined; for cell measures the 11 x 3 grid of each cell's top 5 on one share scale);
// over time (group area with Value | Share, the highlight's lines with index = 100, a dumbbell for
// two-point measures; a click sets the year); the table with economies without data on request.

import { useEffect, useMemo, useState, type ReactNode } from 'react';
import { WorldMap } from '../../../charts/WorldMap.tsx';
import { TableView } from '../../../components/TableView.tsx';
import { Checkbox, Segmented, Select, StatTile } from '../../../components/ui.tsx';
import { Term } from '../../../components/Term.tsx';
import { InfoTip } from '../../../components/InfoTip.tsx';
import { rowsToCsv } from '../../../lib/csv.ts';
import { useMediaQuery } from '../../../shell/useMediaQuery.ts';
import { GROUP_COLOR } from '../../../theme/palette.ts';
import { STAGE_LABELS, STAGE_SLUGS, TECH_LABELS, TECH_SLUGS, type StageSlug } from '../../contract.ts';
import type { FactRow, FactsIndex } from '../../data/facts.ts';
import type { GeoInfo, SectorModel, VarInfo } from '../../data/model.ts';
import { basisWords, concentration, isWorldBasis, type Concentration } from '../../derive/concentration.ts';
import { sectorCsv } from '../../derive/csv.ts';
import { acrossRows, exploreRows, geoMark, inScope, isDiverging, kpis, poles, shareLegal, type ExploreRow } from '../../derive/explore.ts';
import { blocModeWords, blocNote, GROUP_SHORT, partOf, TRADE_BLOC_VARS } from '../../derive/groups.ts';
import { groupArea, normalizeArea, topLines, type Line } from '../../derive/series.ts';
import { stateOf, stateShort, stateWords } from '../../derive/states.ts';
import { ASSUMED_BADGE, ASSUMED_MARK, assumedCount, assumedNote } from '../../derive/notes.ts';
import { slice } from '../../derive/summary.ts';
import { subComponents, subSlice, validSub, type SubComponent } from '../../derive/subs.ts';
import { concentrationSummary } from '../../derive/text.ts';
import { lc, axisFormatter, fmtEffN, fmtHhi, fmtRank, fmtShare, fmtValue, listText, logAxisFormatter, ordinal } from '../../format.ts';
import { AREA_READING, ChunkGate, concFromSummary, dumbbellReading, fmtFact, highlightOffset, highlightLabel, HowToRead, LINES_READING, MAP_READING, MAP_READING_SIGNED, mapCells, SliceMapLegend, SPLIT_COLUMNS, SPLIT_READING, shareSplitRow, splitParts, SplitTable, statesPresent } from '../common.tsx';
import { useTrade } from '../data.ts';
import {
  Beeswarm, BlocBar, CellGrid, Dumbbell, GroupArea, GroupLegend, LabelledLines, LeagueBars, SectorChartCard, ShareSplitChart, StateLegend,
  type BeeswarmPoint, type CellGridTech, type DumbbellRow, type LeagueRow, type ShareSplitRow,
} from '../kit.tsx';
import { indexFor, timeIndex, type ExploreCtx } from './ctx.ts';
import { useSectorUrl } from '../state.tsx';

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
  const tiles: { key: string; label: string; value: string; sub: string; chip?: ReactNode }[] = [];
  const name = (g: number) => model.geos[g]?.name ?? '';
  const n = conc.rows.filter((r) => model.geos[r.geo]?.kind === 'country' || model.geos[r.geo]?.kind === 'eu').length;
  if (v.agg !== 'none' && conc.W !== null) {
    tiles.push({ key: 'w', label: isWorldBasis(conc.basis) ? 'World total' : `Total of the economies listed by ${v.source.short}`, value: fmtFact(model, v, item, conc.W), sub: `${conc.basis === 'source_world' ? `as reported by ${v.source.short}` : conc.basis === 'sum_all' ? 'sum of every economy' : conc.basis === 'one' ? 'shares sum to 1' : 'not a world total'} · ${year ?? ''}` });
    tiles.push({ key: 'coal', label: 'Coalition', value: fmtShare(conc.split.core + conc.split.partner), sub: `core ${fmtShare(conc.split.core)} · partners ${fmtShare(conc.split.partner)} · ${k.coalitionWith} of ${k.coalitionTotal} members with data` });
    tiles.push({ key: 'chn', label: 'China', value: k.china.share !== null ? fmtShare(k.china.share) : 'no value', sub: k.china.rank !== null ? `${ordinal(k.china.rank)} of ${n}` : `not in ${v.source.short}` });
    tiles.push({ key: 'top3', label: 'Top 3', value: fmtShare(conc.top3_share), sub: listText(k.top3.map((t) => name(t.geo))) });
    tiles.push({ key: 'hhi', label: 'Concentration (HHI)', value: fmtHhi(conc.hhi, conc.hhi_lower_bound), sub: `like ${fmtEffN(conc.eff_n)} equal-sized economies`, chip: <InfoTip id="hhi" term="concentration index (HHI)" /> });
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
            <StatTile label={t.label} value={<span className="font-medium">{t.value}</span>} sub={t.sub} chip={t.chip} />
          </div>
        ))}
      </div>
      {conc.W !== null && v.agg === 'sum' && (
        <BlocBar parts={splitParts(conc)} ariaLabel={concentrationSummary(conc, model, v, highlight)} label={`Shares ${basisWords(conc.basis, v.source.short)}${eu === 'bloc' ? ' · EU-27 counted as one' : ''}`} highlight={highlight && off !== null ? { offset: off, label: highlightLabel(conc, highlight) } : undefined} />
      )}
    </section>
  );
}

function MapRanking({ c, rows, mapRows, world, conc, mode, inherited, sub }: { c: ExploreCtx; rows: FactRow[]; mapRows: FactRow[]; world: { value: number } | null; conc: Concentration; mode: string; inherited: number; sub?: string | null }) {
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
  const map = useMemo(() => mapCells(model, v, item, mapRows, { highlight, year: year ?? undefined, diverging, dimOut: (g) => !inScope(g, scope) }), [diverging, mapRows, model, v, item, highlight, year, scope]);
  const top = wide ? 15 : 10;
  const league: LeagueRow[] = [];
  sconc.rows.forEach((r, i) => {
    const g = model.geos[r.geo];
    const isH = highlight?.idx === r.geo;
    if (i >= top && !isH) return;
    const f = rows.find((x) => x.geo === r.geo);
    const st = f ? stateOf(f, g, v, null, year ?? undefined) : null;
    league.push({ id: g.code, label: g.name, group: geoMark(g), value: shown(r.value), display: `${fmt(shown(r.value))}${st?.assumed ? ` ${ASSUMED_MARK}` : ''}${!share && r.share !== null ? ` · ${fmtShare(r.share)}` : ''}`, pinned: i >= top && isH, state: st?.old ? 'old' : ((st?.state ?? 'obs') as LeagueRow['state']), badge: st?.assumed ? ASSUMED_BADGE : g.re_export_hub ? 're-export hub' : f?.status === 'mirror' ? 'mirror' : st?.old && st.obsYear ? String(st.obsYear) : undefined });
  });
  if (highlight && !sconc.rows.some((r) => r.geo === highlight.idx)) league.push({ id: highlight.code, label: highlight.name, group: geoMark(highlight), value: NaN, display: inScope(highlight, scope) ? 'no value' : 'outside the selected economies', pinned: true, state: 'not_covered' });
  const pick = (code: string) => {
    const g = model.geoByCode.get(code);
    c.setHighlight(g && g.idx !== highlight?.idx ? g : null);
  };
  const tickFormat = useMemo(() => (share ? (x: number) => `${Math.round(x * 100)}%` : log ? logAxisFormatter(unit, v.display_scale) : axisFormatter(sconc.rows.map((r) => r.value), unit, v.display_scale)), [share, log, sconc, unit, v.display_scale]);
  const blocWords = eu === 'bloc' ? blocModeWords(mode as never, v.source.short) : null;
  const nAssumed = assumedCount(v.id, scoped);
  return (
    <SectorChartCard
      title={`${v.short}${item && item.axis !== 'none' ? `: ${item.axis === 'cell' ? model.cellLabel(item) : item.label}` : ''}${sub ? ` (${sub})` : ''}, ${year ?? ''}`}
      vars={[{ var: v.id, item: item?.slug ?? null, year }]}
      subExtra={<HowToRead>{diverging ? MAP_READING_SIGNED : MAP_READING} The bars rank the largest; colours there show the group (China, coalition core, partners, rest).</HowToRead>}
      table={() => <ExploreTableBody c={c} rows={rows} world={world} absent={false} />}
      csv={() => ({ name: `${v.id}-${item?.slug ?? 'all'}-${year ?? ''}-${c.scope}`, text: rowsCsv(c, exploreRows(model, v, rows, world, year, scope, false)) })}
      legend={
        <div className="space-y-2">
          <SliceMapLegend model={model} v={v} item={item} year={year} scale={map.scale} poles={diverging ? poles(v) : undefined} hatched={map.hatched > 0} zero={[...map.cells.values()].some((x) => x.fill === 'zero')} />
          <GroupLegend groups={['china', 'core', 'partner', 'rest']} highlight={highlight?.name ?? null} euBloc={eu === 'bloc'} />
          <StateLegend states={statesPresent(model, v, rows, year ?? undefined)} source={v.source.short} />
        </div>
      }
      notes={
        <>
          {scope !== 'all' && <p>The ranking covers the selected economies; the map shows all, the others dimmed.</p>}
          {nAssumed > 0 && <p>{assumedNote(v.id, nAssumed)}</p>}
          {inherited > 0 && <p>{inherited} territories without their own UN votes take their sovereign's position (basis "inherited"): shown on the map, not ranked or counted in the figures.</p>}
          {eu === 'bloc' && <p>{blocNote(model)}</p>}
          {blocWords && <p>{blocWords}</p>}
        </>
      }
      summary={concentrationSummary(sconc, model, v, highlight)}
    >
      <div className="grid gap-6 lg:grid-cols-5">
        <div className="lg:col-span-3">
          <WorldMap cells={map.cells} ringStyle="double" ariaLabel={`Map of ${v.label}${year ? `, ${year}` : ''}`} selected={highlight?.code ?? null} onSelect={pick} />
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
      <SectorChartCard title="Distribution" vars={[{ var: v.id, item: item?.slug ?? null, year }]} subExtra={<HowToRead>Too few economies report this measure for a dot chart, so the holders are listed instead, largest first{conc.W !== null && v.agg === 'sum' ? '; the bar above the table splits the world total by group: China, the coalition core, coalition partners, the rest of the world and the part not assigned to any economy' : ''}.</HowToRead>} csv={() => ({ name: `${v.id}-${item?.slug ?? 'all'}-${year ?? ''}-holders`, text: sectorCsv(model, scoped.map((r) => ({ geo: model.geos[r.geo], v, item, year: r.obs_year ?? year, value: r.value, status: r.status }))) })} summary={`${scoped.length} economies with a value: too few for a distribution, so the holders are listed.`}>
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
      subExtra={<HowToRead>Each dot is one economy, placed by its value; dots with similar values stack up so none hides another. The ticks mark the world and coalition medians: half of the economies sit on each side.</HowToRead>}
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
        subExtra={<HowToRead>{SPLIT_READING} Each row is one {v.item_axis === 'cell' ? 'sector' : v.item_axis} in its source’s latest year; the one you picked is outlined. Shares are within each row and never added across rows. Click a row to switch to it.</HowToRead>}
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

/** One holder in a "Top five" tile: a share, or (share null) a value the source lists without a figure. */
export interface CellTop {
  geo: GeoInfo;
  share: number | null;
  /** the words for a holder without a figure ("<1% or not reported") */
  words: string | null;
}

/** "<1% or not reported": a source's 0 that means "under 1% or not reported" (status nq). */
export const NQ_SHARE_WORDS = '<1% or not reported';

/**
 * The top five holders of each cell (share of world, largest first), then the holders the source lists
 * without a figure (status nq: IEA's "under 1% or not reported"), up to five rows. Reported zeros of an
 * additive measure are no holders. Pure.
 */
export function cellTops(model: SectorModel, facts: FactsIndex, v: VarInfo, eu: 'members' | 'bloc'): Map<string, CellTop[]> {
  const m = new Map<string, CellTop[]>();
  for (const cell of model.cells) {
    const y = model.summaryLatest(cell.idx, v.idx, eu)?.year ?? null;
    if (y === null || !facts.has(v.idx, cell.idx, y)) continue;
    const sl = slice(model, facts, v, cell.idx, y, eu);
    const cc = concentration(sl.rows, model.geos, sl.world, sl.basis);
    if (cc.W === null) continue;
    const status = new Map(sl.rows.map((r) => [r.geo, r.status]));
    const pos: CellTop[] = cc.rows.filter((r) => r.share !== null && r.value > 0).map((r) => ({ geo: model.geos[r.geo], share: r.share as number, words: null }));
    const nq: CellTop[] = sl.rows
      .filter((r) => status.get(r.geo) === 'nq' && !(r.value > 0) && model.geos[r.geo] && (model.geos[r.geo].kind !== 'residual' && model.geos[r.geo].kind !== 'world'))
      .map((r) => ({ geo: model.geos[r.geo], share: null, words: NQ_SHARE_WORDS }));
    const xs = [...pos.slice(0, 5), ...nq].slice(0, 5);
    if (xs.length) m.set(cell.slug, xs);
  }
  return m;
}

/** One cell's rows: the display name (truncated, full name on hover) in an 8rem column, a bar and the share. */
function TopRows({ xs, max }: { xs: CellTop[]; max: number }) {
  return (
    <ul className="space-y-1" aria-hidden="true">
      {xs.map((x) => (
        <li key={x.geo.code} className="grid grid-cols-[minmax(0,8rem)_minmax(0,1fr)] items-center gap-2">
          <span className="truncate text-paper-dim" title={x.geo.name}>
            {x.geo.name}
          </span>
          {x.share !== null ? (
            <span className="flex min-w-0 items-center gap-1.5">
              <span className="inline-block h-2 shrink-0" style={{ width: `${Math.max(2, (60 * x.share) / max)}%`, background: GROUP_COLOR[geoMark(x.geo)] }} />
              <span className="tabular text-paper-mute">{fmtShare(x.share)}</span>
            </span>
          ) : (
            <span className="text-paper-mute">{x.words}</span>
          )}
        </li>
      ))}
    </ul>
  );
}

/** Below this many cells with data, the grid gives way to a compact list of those cells. */
const CELL_TOPS_GRID_MIN = 12;

/** Each cell's top 5 holders on one share-of-world scale: the 11 x 3 grid, or a compact list when few cells have data. */
function CellTops({ c }: { c: ExploreCtx }) {
  const { model, v, eu, item } = c;
  const facts = c.facts!;
  const tops = useMemo(() => cellTops(model, facts, v, eu), [model, v, eu, facts]);
  if (!tops.size) return null;
  const max = Math.max(...[...tops.values()].flatMap((xs) => xs.map((x) => x.share ?? 0)), 1e-9);
  const techs: CellGridTech[] = TECH_SLUGS.filter((t) => model.cells.some((x) => x.techSlug === t)).map((t) => ({ slug: t, label: TECH_LABELS[t], headline: t !== 'semiconductors' }));
  const withData = model.cells.filter((x) => tops.has(x.slug));
  const compact = withData.length < CELL_TOPS_GRID_MIN;
  const aria = (cell: (typeof withData)[number], xs: CellTop[]) => `${model.cellLabel(cell)}: ${xs.map((x) => `${x.geo.name} ${x.share !== null ? fmtShare(x.share) : x.words}`).join(', ')}`;
  const tile = (cell: (typeof withData)[number], xs: CellTop[], heading: boolean) => (
    <button
      type="button"
      className={`block w-full border p-2 text-left text-xs ${heading ? '' : 'min-h-[96px]'} ${item?.idx === cell.idx ? 'border-signal' : 'hairline'}`}
      aria-label={aria(cell, xs)}
      aria-pressed={item?.idx === cell.idx}
      onClick={() => c.setItem(cell)}
    >
      {heading && (
        <span className="mb-1.5 block text-sm font-medium text-paper" aria-hidden="true">
          {model.cellLabel(cell)}
        </span>
      )}
      <TopRows xs={xs} max={max} />
    </button>
  );
  const nqAny = [...tops.values()].some((xs) => xs.some((x) => x.share === null));
  return (
    <SectorChartCard
      title={`Top five in each sector: ${lc(v.short)}`}
      vars={[{ var: v.id }]}
      table={() => <TableView rows={[...tops.entries()].flatMap(([cell, xs]) => xs.map((x, i) => ({ cell, rank: i + 1, x })))} caption="Top five in each sector" search pageSize={25} cols={[{ id: 'c', header: 'Sector', value: (r) => { const it = model.item('cell', r.cell); return it ? model.cellLabel(it) : r.cell; } }, { id: 'r', header: 'Rank', value: (r) => (r.x.share !== null ? r.rank : null), num: true }, { id: 'e', header: 'Economy', value: (r) => r.x.geo.name }, { id: 's', header: 'Share of world', value: (r) => r.x.share, cell: (r) => (r.x.share !== null ? fmtShare(r.x.share) : (r.x.words ?? '–')), num: true }]} />}
      subExtra={<HowToRead>Each block is one sector and its five largest holders, each bar one economy’s share of the world total in its group colour. Every block uses the same scale (the longest bar is {fmtShare(max)}), so bars can be compared across sectors. Click a block to switch to that sector.</HowToRead>}
      csv={() => ({ name: `${v.id}-top5-by-cell-${eu}`, text: rowsToCsv([...tops.entries()].flatMap(([cell, xs]) => xs.map((x, i) => ({ item: cell, rank: x.share !== null ? i + 1 : null, iso3c: x.geo.code, economy: x.geo.name, group: x.geo.group, share_of_world: x.share, note: x.words, var_id: v.id, source: v.source.short })))) })}
      notes={nqAny ? <p>"{NQ_SHARE_WORDS}": the source lists the economy with a 0, which it uses for shares under 1% and for shares it does not report.</p> : undefined}
      summary={`The five largest holders of ${lc(v.short)} in each of ${tops.size} sector${tops.size === 1 ? '' : 's'}.`}
    >
      {compact ? (
        <div className="space-y-4">
          <p className="text-sm text-paper-dim">
            {v.source.short} reports {lc(v.short)} for {withData.length} of the {model.cells.length} sectors: {listText(withData.map((x) => model.cellLabel(x)))}. The other sectors have no data for this measure.
          </p>
          {STAGE_SLUGS.filter((s) => withData.some((x) => x.stageSlug === s)).map((s) => (
            <div key={s}>
              <h4 className="eyebrow mb-2">{STAGE_LABELS[s]}</h4>
              <ul className="grid gap-3 sm:grid-cols-2 xl:grid-cols-3" aria-label={`${STAGE_LABELS[s]} sectors`}>
                {withData.filter((x) => x.stageSlug === s).map((cell) => (
                  <li key={cell.slug} className="min-w-0">
                    {tile(cell, tops.get(cell.slug)!, true)}
                  </li>
                ))}
              </ul>
            </div>
          ))}
        </div>
      ) : (
        <CellGrid
          techs={techs}
          stages={STAGE_SLUGS.map((s) => ({ slug: s, label: STAGE_LABELS[s] }))}
          ariaLabel={`Top five holders of ${lc(v.short)} by sector`}
          selected={item?.axis === 'cell' ? item.slug : null}
          renderTile={(t, stage) => {
            const cell = model.cell(t.slug as never, stage);
            if (!cell) return <div className="min-h-[96px]" aria-hidden="true" />;
            const xs = tops.get(cell.slug);
            if (!xs) return <div className="min-h-[96px] border hairline p-2 text-xs text-paper-mute">No data</div>;
            return tile(cell, xs, false);
          }}
        />
      )}
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
        subExtra={<HowToRead>{projLater ? `Each row is one economy; its two dots are the value in ${y0} and the source's projection for ${y1}. A line running to the right means growth is expected.` : dumbbellReading(String(y0), String(y1))}</HowToRead>}
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
          subExtra={<HowToRead>{AREA_READING}</HowToRead>}
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
          subExtra={<HowToRead>{LINES_READING}</HowToRead>}
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
  return sectorCsv(c.model, rows.map((r) => ({ geo: r.g, v: c.v, item: c.item, year: r.year, value: r.value, status: r.status || (r.state.state === 'not_covered' ? 'not_covered' : r.state.state === 'none' ? 'none' : ''), share: r.share, rank: r.rank, note: r.note })));
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
        { id: 'f', header: 'Source note', value: (r) => [r.g.re_export_hub ? 're-export hub' : r.status === 'mirror' ? "partners' declarations" : r.status === 'est' ? 'includes an estimate' : '', r.note].filter(Boolean).join('; ') || null },
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
      subExtra={<HowToRead>One row per economy: its value, its share of the world total where there is one, its rank, the year of the observation and any source note. Click a row to highlight the economy everywhere on the page.</HowToRead>}
      tools={<Checkbox label="Show economies without data" checked={absent} onChange={setAbsent} />}
      csv={() => ({ name: `${v.id}-${item?.slug ?? 'all'}-${year ?? ''}-table`, text: rowsCsv(c, exploreRows(model, v, rows, world, year, scope, absent)) })}
      summary={`${v.label} for every economy${scope !== 'all' ? ' in the selected group' : ''}.`}
    >
      <ExploreTableBody c={c} rows={rows} world={world} absent={absent} />
    </SectorChartCard>
  );
}

/**
 * "View by: Whole basket | Sub-component" for trade exports and imports of a cell (C5.4). The trade
 * chunk loads only once the reader asks for sub-components; then the first (largest) one is picked.
 */
function SubRow({ subs, sub, loaded, onWant, onPick }: { subs: readonly SubComponent[]; sub: string | null; loaded: boolean; onWant: () => void; onPick: (s: string | null) => void }) {
  return (
    <div className="flex flex-wrap items-end gap-3" role="group" aria-label="View by sub-component">
      <Segmented<'whole' | 'sub'> label="View by" value={sub ? 'sub' : 'whole'} options={[{ value: 'whole', label: 'Whole basket' }, { value: 'sub', label: 'Sub-component' }]} onChange={(x) => (x === 'whole' ? onPick(null) : subs.length > 1 ? onPick(sub ?? subs[0].name) : onWant())} />
      {sub && subs.length > 1 && <Select label="Sub-component" value={sub} options={subs.map((x) => ({ value: x.name, label: `${x.name}${Number.isFinite(x.share) ? ` (${fmtShare(x.share)} of the basket)` : ''}` }))} onChange={(x) => onPick(x)} className="min-w-[14rem]" />}
      {loaded && subs.length === 1 && <p className="pb-1 text-sm text-paper-mute">One group of codes: the whole basket.</p>}
      {sub && (
        <p className="basis-full text-xs text-paper-mute">
          One <Term id="sub_component">sub-component</Term> of the basket: the key figures, map, ranking, distribution and table follow it; the across-sectors and over-time cards show whole baskets.
        </p>
      )}
    </div>
  );
}

export function CountryLayout({ c }: { c: ExploreCtx }) {
  const { model, v, item, year, eu } = c;
  const { state, set } = useSectorUrl();
  const idx = indexFor(c, year);
  const subCapable = (v.id === 'trade.exports' || v.id === 'trade.imports') && !!item && item.axis === 'cell' && !!item.has_trade && (year === 2020 || year === 2024);
  const [wantSub, setWantSub] = useState(false);
  const needTrade = (eu === 'bloc' && TRADE_BLOC_VARS.has(v.id) && !!item?.techSlug) || (subCapable && (wantSub || !!state.sub));
  const { trade, h: th } = useTrade(item?.techSlug ?? null, needTrade);
  const subs = useMemo(() => (subCapable && trade && item ? subComponents(trade, model, item.idx) : []), [subCapable, trade, item, model]);
  const sub = subCapable && subs.length > 1 ? validSub(subs, state.sub) : null;
  // asked for sub-components before the chunk loaded: pick the largest once it is there
  useEffect(() => {
    if (wantSub && !state.sub && subs.length > 1) set({ sub: subs[0].name });
    if (wantSub && subs.length) setWantSub(false);
  }, [wantSub, subs, state.sub, set]);
  const sl = useMemo(() => {
    if (!idx || !item || year === null) return null;
    if (sub && trade) return { ...subSlice(trade, model, item.idx, sub, year, v.id === 'trade.imports' ? 'imports' : 'exports', eu), basis: 'sum_all' as const };
    return slice(model, idx, v, item.idx, year, eu, trade);
  }, [idx, item, year, model, v, eu, trade, sub]);
  // territories that inherit their sovereign's UN-vote position are copies, never ranked as country data
  const ranked = useMemo(() => (sl ? sl.rows.filter((r) => model.geos[r.geo]?.alignment_basis !== 'inherited' || v.id !== 'development.alignment') : []), [sl, model, v.id]);
  const inherited = sl ? sl.rows.length - ranked.length : 0;
  const conc = useMemo(() => (sl ? concentration(ranked, model.geos, sl.world, sl.basis) : null), [sl, ranked, model]);
  if (!item) return <p className="text-sm text-paper-mute">No item with data for this measure.</p>;
  if (!sl || !conc) return <p className="text-sm text-paper-mute">No values for {year ?? 'this year'}.</p>;
  return (
    <div className="space-y-8">
      {subCapable && <SubRow subs={subs} sub={sub} loaded={!!trade} onWant={() => setWantSub(true)} onPick={(x) => set({ sub: x })} />}
      {needTrade && !trade && (
        <ChunkGate h={th} name={`sector-trade-${item.techSlug}`}>
          {() => null}
        </ChunkGate>
      )}
      <KpiStrip c={c} rows={ranked} world={sl.world} />
      <MapRanking c={c} rows={ranked} mapRows={sl.rows} world={sl.world} conc={conc} mode={sl.mode} inherited={inherited} sub={sub} />
      <Distribution c={c} rows={ranked} conc={conc} />
      <AcrossSectors c={c} />
      <OverTime c={c} />
      <ExploreTable c={c} rows={sl.rows} world={sl.world} />
    </div>
  );
}
