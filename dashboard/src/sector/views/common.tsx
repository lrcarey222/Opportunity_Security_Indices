// Helpers shared by the Sector views: page container, chunk gates (loading with size, error with
// Retry, 3.0 "4.0 Errors"), highlight resolution, league / share-split / map row builders, section
// headers, CSV builders and the question card. Descriptive words only (1).

import { useMemo, type ReactNode } from 'react';
import { Link } from 'react-router';
import { Loading, Notice } from '../../components/ui.tsx';
import { TableView } from '../../components/TableView.tsx';
import { InfoTip } from '../../components/InfoTip.tsx';
import { rowsToCsv } from '../../lib/csv.ts';
import type { MapCell } from '../../charts/WorldMap.tsx';
import { triScale, type TriScale } from '../../charts/scale.ts';
import { TriLegend } from '../../charts/legends.tsx';
import { questionFact, type Question } from '../../content/questions.ts';
import type { SectorHeadline, SplitPart } from '../contract.ts';
import type { ChunkHandle } from '../data/SectorDataProvider.tsx';
import type { FactRow } from '../data/facts.ts';
import type { GeoInfo, ItemInfo, SectorModel, VarInfo } from '../data/model.ts';
import { useSectorStore } from '../data/SectorDataProvider.tsx';
import type { Concentration } from '../derive/concentration.ts';
import { basisWords } from '../derive/concentration.ts';
import { GROUP_ORDER, partOf } from '../derive/groups.ts';
import { stateOf, stateShort, stateWords, type StateOf } from '../derive/states.ts';
import { ASSUMED_BADGE, ASSUMED_MARK, ASSUMED_ZERO_VARS, noteLine, noteWords } from '../derive/notes.ts';
import { csvRow, sectorCsv, type CsvInput } from '../derive/csv.ts';
import { slice, summaryFromFacts } from '../derive/summary.ts';
import { fmtShare, fmtSize, fmtValue, fmtRank, type FormatContext } from '../format.ts';
import { chunkWords } from './data.ts';
import type { LeagueRow, MarkGroup, ShareSplitRow, ValueState } from './kit.tsx';
import { useSectorUrl } from './state.tsx';

export const PAGE = 'mx-auto w-full max-w-7xl px-4 py-8 sm:px-6 lg:px-8';

/** Measures with a glossary entry (spec_country C3.6: the first mention of a measure uses its term). */
export const MEASURE_GLOSSARY: Readonly<Record<string, string>> = {
  'trade.rca': 'rca',
  'trade.feasibility': 'feasibility',
  'consumption.balance': 'net_balance',
  'consumption.self_sufficiency': 'self_sufficiency',
  'technology.trl': 'trl',
  'investment.fdi_projects': 'cmfdi',
  'investment.fdi_value': 'cmfdi',
  'investment.fdi_parent_share': 'chinese_parent',
};

/** The "i" of the chosen measure, beside a Measure select (outside its label), when the glossary has it. */
export function MeasureTip({ v }: { v: VarInfo | null | undefined }) {
  const id = v && Object.hasOwn(MEASURE_GLOSSARY, v.id) ? MEASURE_GLOSSARY[v.id] : null;
  if (!v || !id) return null;
  return (
    <span className="pb-1.5">
      <InfoTip id={id} />
    </span>
  );
}

export function Page({ children }: { children: ReactNode }) {
  return <div className={PAGE}>{children}</div>;
}

/** Loading (with the chunk size) / error (with Retry) / children. */
export function ChunkGate<T>({ h, name, children, minHeight = 120 }: { h: ChunkHandle<T>; name: string; children: (data: T) => ReactNode; minHeight?: number }) {
  const store = useSectorStore();
  if (h.data) return <>{children(h.data)}</>;
  if (h.status === 'error') {
    if (h.error?.code === 'NOT_IN_BUNDLE') return <Notice kind="info">This data is not part of the current sector data build.</Notice>;
    return (
      <Notice kind="error" title={`The ${chunkWords(name)} could not be loaded`} action={<button type="button" className="btn btn-ghost btn-sm" onClick={h.retry}>Retry</button>}>
        {h.error?.message ?? 'Unknown error.'}
      </Notice>
    );
  }
  const bytes = h.bytes ?? store.chunkBytes(name as never);
  return (
    <div style={{ minHeight }}>
      <Loading label={`Loading ${chunkWords(name)}${bytes ? `, ${fmtSize(bytes)}` : ''}`} />
    </div>
  );
}

/**
 * The plain-language reading line of a card (spec_country.md C3.5-C3.6): what each mark is, how to read
 * position and colour, what high and low mean. Rendered after the caption's unit / year / source.
 */
export function HowToRead({ children }: { children: ReactNode }) {
  return <span className="mt-1 block text-paper-dim">{children}</span>;
}

/** The reading line of a map on the three-colour scale. */
export const MAP_READING = 'Each country is coloured by its value: orange lower than most of the economies shown, white the middle one (the median), blue higher. Hatched: no value (hover for why). Click a country to highlight it everywhere on the page.';
/** The same for a signed measure (net balance, net trade, changes). */
export const MAP_READING_SIGNED = 'Each country is coloured by its value: orange below zero, white zero, blue above zero; the deeper the colour, the further from zero. Hatched: no value (hover for why).';
/** The reading line of a ranking. */
export const SPLIT_READING = 'Each bar is one measure, split by who holds it: China, the coalition core, coalition partners, the rest of the world and the part the source does not assign to any economy. The longer a segment, the larger that group’s share; the columns beside it name the largest holder and give the top-three share and the concentration index.';

/** Two-year dot pairs (Dumbbell): one row per economy (or group / sector, named by `row`). */
export function dumbbellReading(a: string, b: string, row = 'economy'): string {
  return `Each row is one ${row}; its two dots are the value in ${a} and in ${b}. A line running to the right means the value rose; the longer the line, the bigger the change.`;
}

export const AREA_READING = 'Each band is one group: China, the coalition core, coalition partners, the rest of the world and, where the source has one, the part not assigned to any economy. Stacked, they make the world total, so a band that widens is gaining; "Share" shows the same bands as parts of 100%.';

export const LINES_READING = 'Each line is one economy, year by year; the higher the line, the larger the amount. With "Index, first year = 100" every line starts at 100, so the steepest line grew fastest.';

export const BARS_READING = 'Each bar is one economy, largest first; its colour shows the group: China, the coalition core, coalition partners or the rest of the world.';

/** A profile / page section with an h2 and its anchor. */
export function Section({ id, title, sub, children, aside }: { id: string; title: string; sub?: ReactNode; children: ReactNode; aside?: ReactNode }) {
  return (
    <section id={id} aria-labelledby={`${id}-h`} className="scroll-mt-[calc(var(--sector-bar-h,4.75rem)+4rem)] xl:scroll-mt-[calc(var(--sector-bar-h,4.75rem)+1.5rem)] border-t hairline pt-8">
      <div className="mb-4 flex flex-wrap items-end justify-between gap-3">
        <div>
          <h2 id={`${id}-h`} className="rule-neis-sm text-xl font-medium text-paper">
            {title}
          </h2>
          {sub && <p className="mt-1 max-w-3xl text-sm text-paper-mute">{sub}</p>}
        </div>
        {aside}
      </div>
      <div className="space-y-6">{children}</div>
    </section>
  );
}

/** The highlighted economy of the URL (`c`), or null. */
export function useHighlight(model: SectorModel): GeoInfo | null {
  const { state } = useSectorUrl();
  return useMemo(() => (state.c ? (model.geoByCode.get(state.c) ?? null) : null), [model, state.c]);
}

/** The mark group of a geo for kit marks (unattributed is never an economy's group). */
export const markGroup = (g: GeoInfo | null | undefined): MarkGroup => {
  const p = partOf(g);
  return p === 'unattributed' ? 'rest' : p;
};

/** Value of a row for display (display_scale applied by fmtValue). */
export function fmtFact(model: SectorModel, v: VarInfo, item: ItemInfo | null, value: number, ctx: FormatContext = 'tile', lowerBound = false): string {
  return fmtValue(value, model.unitOf(v, item?.idx ?? null), { scale: v.display_scale, ctx, lowerBound });
}

/** Rows for LeagueBars from a concentration (largest first), with the highlight pinned when outside the top. */
export function leagueRows(model: SectorModel, v: VarInfo, item: ItemInfo | null, c: Concentration, opts: { top?: number; highlight?: GeoInfo | null; rows?: readonly FactRow[] } = {}): LeagueRow[] {
  const top = opts.top ?? 20;
  const byGeo = new Map((opts.rows ?? []).map((r) => [r.geo, r]));
  const out: LeagueRow[] = [];
  c.rows.forEach((r, i) => {
    const g = model.geos[r.geo];
    const isH = opts.highlight?.idx === r.geo;
    if (i >= top && !isH) return;
    const f = byGeo.get(r.geo);
    const st = f ? stateOf(f, g, v, null) : null;
    out.push({
      id: g.code,
      label: g.name,
      group: markGroup(g),
      value: r.value,
      display: `${fmtFact(model, v, item, r.value)}${st?.assumed ? ` ${ASSUMED_MARK}` : ''}${r.share !== null ? ` · ${fmtShare(r.share)}` : ''}`,
      pinned: i >= top && isH,
      state: st ? (st.old ? 'old' : (st.state as ValueState)) : 'obs',
      badge: st?.assumed ? ASSUMED_BADGE : g.re_export_hub ? 're-export hub' : f?.status === 'mirror' ? 'mirror' : st?.old && st.obsYear ? String(st.obsYear) : undefined,
    });
  });
  if (opts.highlight && !c.rows.some((r) => r.geo === opts.highlight!.idx)) {
    out.push({ id: opts.highlight.code, label: opts.highlight.name, group: markGroup(opts.highlight), value: NaN, display: 'no value', pinned: true, state: 'not_covered' });
  }
  return out;
}

/** Group split parts of a concentration for ShareSplitChart. */
export function splitParts(c: Concentration): Partial<Record<SplitPart, number>> {
  const out: Partial<Record<SplitPart, number>> = {};
  for (const k of GROUP_ORDER) if (Number.isFinite(c.split[k])) out[k] = c.split[k];
  return out;
}

/** The middle of a geo's slice along a split bar (0-1): its group's offset plus its place within the group. */
export function highlightOffset(model: SectorModel, c: Concentration, geo: number): number | null {
  const row = c.rows.find((r) => r.geo === geo);
  if (!row || row.share === null) return null;
  const part = partOf(model.geos[geo]);
  let off = 0;
  for (const k of GROUP_ORDER) {
    if (k === part) break;
    off += Number.isFinite(c.split[k]) ? c.split[k] : 0;
  }
  for (const r of c.rows) {
    if (r.geo === geo) break;
    if (partOf(model.geos[r.geo]) === part && r.share !== null) off += r.share;
  }
  return Math.min(1, off + row.share / 2);
}

/** "USA 4.8% (#4)" or "USA: no value". */
export function highlightLabel(c: Concentration, g: GeoInfo): string {
  const r = c.rows.find((x) => x.geo === g.idx);
  if (!r) return `${g.code}: no value`;
  return `${g.code} ${r.share !== null ? fmtShare(r.share) : ''} (${fmtRank(r.rank)})`.replace('  ', ' ');
}

/** A share-split row of one measure. */
export function shareSplitRow(model: SectorModel, v: VarInfo, item: ItemInfo | null, c: Concentration, opts: { id?: string; label?: string; sub?: string; year?: number; highlight?: GeoInfo | null; href?: string; projection?: boolean; outlined?: boolean; section?: string }): ShareSplitRow {
  const top1 = c.top1 ? model.geos[c.top1.geo] : null;
  const hOff = opts.highlight ? highlightOffset(model, c, opts.highlight.idx) : null;
  const label = opts.label ?? `${v.short}${opts.year ? ` ${opts.year}` : ''}`;
  const hhi = Number.isFinite(c.hhi) ? (c.hhi_lower_bound ? `≥ ${c.hhi.toFixed(2)}` : c.hhi.toFixed(3)) : '–';
  return {
    id: opts.id ?? `${v.id}|${item?.slug ?? ''}|${opts.year ?? ''}`,
    label,
    sub: opts.sub ?? `${v.source.short}${opts.year ? `, ${opts.year}` : ''}`,
    parts: splitParts(c),
    cols: {
      top1: top1 ? `${top1.name} ${fmtShare(c.top1?.share)}` : '–',
      top3: fmtShare(c.top3_share),
      hhi,
      effn: Number.isFinite(c.eff_n) ? `≈ ${c.eff_n.toFixed(1)}` : '–',
      n: String(c.n),
      year: opts.year ? String(opts.year) : v.data_year,
      source: v.source.short,
      total: c.W !== null ? fmtFact(model, v, item, c.W) : '–',
    },
    highlight: opts.highlight && hOff !== null ? { offset: hOff, label: highlightLabel(c, opts.highlight) } : undefined,
    href: opts.href,
    projection: opts.projection,
    outlined: opts.outlined,
    section: opts.section,
    // when the largest holder is China, the China group share is the same number: say it once
    ariaLabel: `${label}: ${top1 && top1.group !== 'china' ? `largest ${top1.name} ${(100 * (c.top1?.share ?? 0)).toFixed(1)} percent; ` : ''}China ${(100 * (c.split.china || 0)).toFixed(1)} percent${top1 && top1.group === 'china' ? ' (the largest)' : ''}, coalition core ${(100 * (c.split.core || 0)).toFixed(1)} percent, partners ${(100 * (c.split.partner || 0)).toFixed(1)} percent, rest of world ${(100 * (c.split.rest || 0)).toFixed(1)} percent, not attributed ${(100 * (c.split.unattributed || 0)).toFixed(1)} percent ${basisWords(c.basis, v.source.short)}.`,
  };
}

/** The table twin of a ShareSplitChart: the group shares and the text columns of every row. */
export function SplitTable({ rows, caption }: { rows: ShareSplitRow[]; caption: string }) {
  const pct = (r: ShareSplitRow, k: SplitPart) => (r.parts[k] ?? null) as number | null;
  return (
    <TableView
      rows={rows}
      caption={caption}
      rowKey={(r) => r.id}
      pageSize={30}
      cols={[
        { id: 'l', header: 'Row', value: (r) => `${r.label}${r.projection && !/\(projection\)$/.test(r.label) ? ' (projection)' : ''}`, minWidth: 160 },
        { id: 's', header: 'Source', value: (r) => r.sub ?? r.cols.source ?? null },
        ...GROUP_ORDER.map((k) => ({ id: k, header: k === 'unattributed' ? 'Not attributed' : k === 'china' ? 'China' : k === 'core' ? 'Core' : k === 'partner' ? 'Partners' : 'Rest', value: (r: ShareSplitRow) => pct(r, k), cell: (r: ShareSplitRow) => fmtShare(pct(r, k)), num: true })),
        { id: 'top1', header: 'Largest holder', value: (r) => r.cols.top1 ?? null },
        { id: 'top3', header: 'Top three', value: (r) => r.cols.top3 ?? null, num: true },
        { id: 'hhi', header: 'Concentration (HHI)', value: (r) => r.cols.hhi ?? null, num: true },
        { id: 'effn', header: 'Effective number of holders', value: (r) => r.cols.effn ?? null, num: true },
      ]}
    />
  );
}

/** CSV of share-split rows (one row per measure / item / year). */
export function splitRowsCsv(rows: readonly ShareSplitRow[]): string {
  return rowsToCsv(rows.map((r) => ({ row: r.label, projection: !!r.projection, source: r.sub ?? r.cols.source ?? null, ...Object.fromEntries(GROUP_ORDER.map((k) => [`share_${k}`, r.parts[k] ?? null])), top1: r.cols.top1 ?? null, top3: r.cols.top3 ?? null, hhi: r.cols.hhi ?? null, effective_n: r.cols.effn ?? null, n: r.cols.n ?? null, year: r.cols.year ?? null })));
}

export const SPLIT_COLUMNS = [
  { key: 'top1', label: 'Largest holder' },
  { key: 'top3', label: 'Top three', num: true },
  { key: 'hhi', label: 'Concentration (HHI)', num: true },
  { key: 'effn', label: 'Effective number', num: true },
  { key: 'n', label: 'Economies', num: true },
  { key: 'source', label: 'Source' },
];

/** Is a map cell of this fact hatched (no value to draw: nq, not-separately-reported without an assumed value, n/a growth)? */
function hatched(st: StateOf): boolean {
  return (st.state === 'nsr' && !st.assumed) || st.state === 'nq' || st.state === 'na_growth';
}

/** Is a fact drawn with the zero fill (an observed zero of an additive measure)? */
function zeroFilled(v: VarInfo, value: number, st: StateOf): boolean {
  return (value === 0 && v.agg === 'sum') || st.state === 'zero';
}

/**
 * The three-colour scale of a slice map (spec_country.md C1.5): the economies drawn with a value
 * (country kind, finite, not hatched, not the zero fill), white at their median; signed measures (net
 * balance, net trade, changes) centre on zero over all their values. Position by rank, so log or
 * linear makes no difference to the colours.
 */
export function sliceScale(model: SectorModel, v: VarInfo, rows: readonly FactRow[], opts: { diverging?: boolean; year?: number } = {}): TriScale {
  const roster = model.rosterOf(v);
  const vals: number[] = [];
  for (const f of rows) {
    const g = model.geos[f.geo];
    if (!g || g.kind !== 'country' || !Number.isFinite(f.value)) continue;
    const st = stateOf(f, g, v, roster, opts.year);
    if (hatched(st) || (!opts.diverging && zeroFilled(v, f.value, st))) continue;
    vals.push(f.value);
  }
  return triScale(vals, opts.diverging ? { center: 'zero' } : {});
}

/** Words for a tooltip: where a value sits on the slice's scale. */
export function sideWords(scale: TriScale, value: number, fmt: (x: number) => string, noun = 'economies'): string | null {
  const s = scale.side(value);
  if (!s || scale.center === 'zero' || scale.n < 2) return null;
  return `${s === 'above' ? 'Above' : s === 'below' ? 'Below' : 'At'} the median of the ${scale.n} ${noun} shown (${fmt(scale.mid)})`;
}

/**
 * Map cells of a slice (3.5 states; regional values never painted; zero solid; nq / nsr / not covered
 * hatched, except an assumed-zero balance, which is a value with a dagger and its note), filled on the
 * three-colour scale (orange lower, white the median of the economies shown, blue higher; signed
 * measures: orange negative, white zero, blue positive).
 */
export function mapCells(model: SectorModel, v: VarInfo, item: ItemInfo | null, rows: readonly FactRow[], opts: { highlight?: GeoInfo | null; dimOut?: (g: GeoInfo) => boolean; year?: number; diverging?: boolean }): { cells: Map<string, MapCell>; scale: TriScale; hatched: number } {
  const roster = model.rosterOf(v);
  const byGeo = new Map(rows.map((r) => [r.geo, r]));
  const scale = sliceScale(model, v, rows, { diverging: opts.diverging, year: opts.year });
  const fmt = (x: number) => fmtFact(model, v, item, x);
  const cells = new Map<string, MapCell>();
  let nHatch = 0;
  for (const g of model.countries) {
    if (!g.iso_n3) continue;
    const f = byGeo.get(g.idx) ?? null;
    const st = stateOf(f, g, v, roster, opts.year);
    let fill: MapCell['fill'] = 'hatch';
    if (f && Number.isFinite(f.value)) {
      if (!opts.diverging && zeroFilled(v, f.value, st)) fill = 'zero';
      else fill = scale.color(f.value) ?? 'hatch';
    }
    if (hatched(st)) fill = 'hatch';
    if (fill === 'hatch') nHatch++;
    const notes = noteWords(model, v.id, f).filter((n) => n !== stateWords(st, v, model, g));
    const side = f && Number.isFinite(f.value) && fill !== 'hatch' && fill !== 'zero' ? sideWords(scale, f.value, fmt) : null;
    cells.set(g.iso_n3, {
      iso3c: g.code,
      fill,
      outline: opts.highlight?.idx === g.idx,
      dim: opts.dimOut ? opts.dimOut(g) : undefined,
      tip: (
        <div className="space-y-0.5">
          <div className="text-sm font-semibold text-paper">{f && Number.isFinite(f.value) ? `${fmt(f.value)}${st.assumed ? ` ${ASSUMED_MARK}` : ''}` : stateShort(st, v)}</div>
          <div className="text-paper-dim">{g.name}</div>
          {side && <div className="text-paper-mute">{side}</div>}
          <div className="text-paper-mute">{stateWords(st, v, model, g)}</div>
          {notes.map((n) => (
            <div key={n} className="text-paper-mute">
              {n}
            </div>
          ))}
        </div>
      ),
    });
  }
  return { cells, scale, hatched: nHatch };
}

/** The legend of a slice map: the three-colour bar, its words, and the hatch and zero swatches. */
export function SliceMapLegend({ model, v, item, year, scale, poles, hatched, zero }: { model: SectorModel; v: VarInfo; item: ItemInfo | null; year: number | null; scale: TriScale; poles?: { negLabel: string; posLabel: string }; hatched?: boolean; zero?: boolean }) {
  const unit = model.unitOf(v, item?.idx ?? null);
  const strip = (s: string) => s.replace(/\s*[←→]\s*/g, '').trim();
  return (
    <TriLegend
      scale={scale}
      label={`${v.short}, ${unit.short || unit.long}${year ? `, ${year}` : ''}`}
      format={(x) => fmtFact(model, v, item, x)}
      noun="economies"
      lowWord={poles ? strip(poles.negLabel) : undefined}
      highWord={poles ? strip(poles.posLabel) : undefined}
      missing={hatched ? (ASSUMED_ZERO_VARS.has(v.id) ? 'No value or not covered' : 'No value, not reported separately or not covered') : undefined}
    >
      {zero && (
        <span className="inline-flex items-center gap-1.5" aria-hidden="true">
          <span className="inline-block h-2.5 w-3.5 bg-ink-600" />
          Zero recorded
        </span>
      )}
    </TriLegend>
  );
}

/** CSV text of a concentration (the 3.9 / 4.0 columns). */
export function concentrationCsv(model: SectorModel, v: VarInfo, item: ItemInfo | null, year: number | null, c: Concentration, rows: readonly FactRow[]): string {
  const byGeo = new Map(rows.map((r) => [r.geo, r]));
  const input: CsvInput[] = c.rows.map((r) => ({ geo: model.geos[r.geo], v, item, year, value: r.value, status: byGeo.get(r.geo)?.status ?? 'obs', share: r.share, rank: r.rank, note: noteLine(model, v.id, byGeo.get(r.geo)) }));
  for (const r of rows) {
    const g = model.geos[r.geo];
    if (g && g.kind === 'residual') input.push({ geo: null, economy: g.name, v, item, year, value: r.value, status: r.status, share: c.W ? r.value / c.W : null });
    if (g && !Number.isFinite(r.value)) input.push({ geo: g, v, item, year, value: null, status: r.status, note: noteLine(model, v.id, r) });
  }
  return sectorCsv(model, input);
}

/** CSV of plain fact rows. */
export function factsCsv(model: SectorModel, v: VarInfo, item: ItemInfo | null, year: number | null, rows: readonly FactRow[]): string {
  return sectorCsv(model, rows.map((r) => ({ geo: model.geos[r.geo], v, item, year: r.obs_year ?? year, value: Number.isFinite(r.value) ? r.value : null, status: r.status, note: noteLine(model, v.id, r) })));
}

export { csvRow };

/** A question card (the Home card format). */
export function QuestionCard({ q, headlines }: { q: Question; headlines: readonly SectorHeadline[] | null | undefined }) {
  const fact = questionFact(q, headlines);
  return (
    <Link to={fact?.href ?? q.href} className="card block h-full p-5" aria-label={q.question}>
      <h3 className="text-lg font-medium text-paper">{q.question}</h3>
      <p className="mt-2 min-h-[2.75rem] text-sm text-paper-dim">
        {fact && (
          <>
            {fact.text}
            <span className="mt-0.5 block text-xs text-paper-mute">
              {fact.source}, {fact.year}
            </span>
          </>
        )}
      </p>
    </Link>
  );
}

/** An external link with an icon and sr-only text (IEA policy pages). */
export function ExternalLink({ href, children, srText = 'opens in a new tab' }: { href: string; children: ReactNode; srText?: string }) {
  return (
    // relative: the sr-only text is positioned against the link, so it never escapes a scrolling table
    <a className="link relative" href={href} target="_blank" rel="noopener noreferrer">
      {children}
      <span aria-hidden="true"> ↗</span>
      <span className="sr-only"> ({srText})</span>
    </a>
  );
}

/** States of a slice for StateLegend. */
export function statesPresent(model: SectorModel, v: VarInfo, rows: readonly FactRow[], year?: number): ValueState[] {
  const s = new Set<ValueState>();
  for (const r of rows) {
    const g = model.geos[r.geo];
    if (!g || g.kind !== 'country') continue;
    const st: StateOf = stateOf(r, g, v, null, year);
    // an assumed-zero balance is drawn as a value (its dagger note is on the card), not as the nsr hatch
    if (st.assumed) continue;
    s.add(st.state === 'none' ? 'not_covered' : (st.state as ValueState));
    if (st.old) s.add('old');
  }
  if (model.rosterOf(v)) s.add('not_covered');
  return [...s];
}

/** Item label for headings ("Lithium", "Batteries — Midstream"). */
export const itemLabel = (model: SectorModel, it: ItemInfo | null): string => (it ? (it.axis === 'cell' ? model.cellLabel(it) : it.label) : '');

/** A Concentration from a meta.summary row (no per-economy rows: facts give those). */
export function concFromSummary(s: import('../data/model.ts').SummaryInfo): Concentration {
  return {
    W: Number.isFinite(s.W) ? s.W : null,
    basis: s.basis,
    n: s.n,
    rows: [],
    top1: s.top1_geo !== null ? { geo: s.top1_geo, share: s.top1_share } : null,
    top3_share: s.top3_share,
    hhi: s.hhi,
    hhi_lower_bound: s.hhi_lower_bound,
    eff_n: s.eff_n,
    split: { china: s.china, core: s.core, partner: s.partner, rest: s.rest, unattributed: s.unattributed },
  };
}

/**
 * The concentration of (var, item, year) in an EU mode: recomputed from facts when they are loaded
 * (so the highlight has its share and rank), else meta.summary. Trade vars in bloc mode use the
 * trade chunk when it is loaded, else meta.summary (which the exporter built from the dyads).
 */
export function measureConc(
  model: SectorModel,
  facts: import('../data/facts.ts').FactsIndex | null,
  v: VarInfo,
  item: ItemInfo,
  year: number,
  eu: 'members' | 'bloc',
  trade?: import('../contract.ts').SectorTradeChunk | null,
): { conc: Concentration; rows: FactRow[]; fromFacts: boolean } | null {
  const isTrade = TRADE_VARS.has(v.id);
  if (facts && facts.has(v.idx, item.idx, year) && !(eu === 'bloc' && isTrade && !trade)) {
    const s = summaryFromFacts(model, facts, v, item.idx, year, eu, trade ?? null);
    const sl = slice(model, facts, v, item.idx, year, eu, trade ?? null);
    return { conc: s.conc, rows: sl.rows, fromFacts: true };
  }
  const s = model.summaryOf(item.idx, v.idx, year, eu);
  return s ? { conc: concFromSummary(s), rows: [], fromFacts: false } : null;
}

const TRADE_VARS = new Set(['trade.exports', 'trade.imports', 'trade.net']);
