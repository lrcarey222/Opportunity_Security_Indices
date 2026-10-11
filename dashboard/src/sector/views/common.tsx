// Helpers shared by the Sector views: page container, chunk gates (loading with size, error with
// Retry, 3.0 "4.0 Errors"), highlight resolution, league / share-split / map row builders, section
// headers, CSV builders and the question card. Descriptive words only (1).

import { useMemo, type ReactNode } from 'react';
import { Link } from 'react-router';
import { Loading, Notice } from '../../components/ui.tsx';
import { TableView } from '../../components/TableView.tsx';
import { rowsToCsv } from '../../lib/csv.ts';
import type { MapCell } from '../../charts/WorldMap.tsx';
import { seqColor } from '../../charts/theme.ts';
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
import { csvRow, sectorCsv, type CsvInput } from '../derive/csv.ts';
import { slice, summaryFromFacts } from '../derive/summary.ts';
import { fmtShare, fmtSize, fmtValue, fmtRank, type FormatContext } from '../format.ts';
import { chunkWords } from './data.ts';
import type { LeagueRow, MarkGroup, ShareSplitRow, ValueState } from './kit.tsx';
import { useSectorUrl } from './state.tsx';

export const PAGE = 'mx-auto w-full max-w-7xl px-4 py-8 sm:px-6 lg:px-8';

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
      display: `${fmtFact(model, v, item, r.value)}${r.share !== null ? ` · ${fmtShare(r.share)}` : ''}`,
      pinned: i >= top && isH,
      state: st ? (st.old ? 'old' : (st.state as ValueState)) : 'obs',
      badge: g.re_export_hub ? 're-export hub' : f?.status === 'mirror' ? 'mirror' : st?.old && st.obsYear ? String(st.obsYear) : undefined,
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
        { id: 'top1', header: 'Top-1', value: (r) => r.cols.top1 ?? null },
        { id: 'top3', header: 'Top-3', value: (r) => r.cols.top3 ?? null, num: true },
        { id: 'hhi', header: 'HHI', value: (r) => r.cols.hhi ?? null, num: true },
        { id: 'effn', header: 'Effective n', value: (r) => r.cols.effn ?? null, num: true },
      ]}
    />
  );
}

/** CSV of share-split rows (one row per measure / item / year). */
export function splitRowsCsv(rows: readonly ShareSplitRow[]): string {
  return rowsToCsv(rows.map((r) => ({ row: r.label, projection: !!r.projection, source: r.sub ?? r.cols.source ?? null, ...Object.fromEntries(GROUP_ORDER.map((k) => [`share_${k}`, r.parts[k] ?? null])), top1: r.cols.top1 ?? null, top3: r.cols.top3 ?? null, hhi: r.cols.hhi ?? null, effective_n: r.cols.effn ?? null, n: r.cols.n ?? null, year: r.cols.year ?? null })));
}

export const SPLIT_COLUMNS = [
  { key: 'top1', label: 'Top-1' },
  { key: 'top3', label: 'Top-3', num: true },
  { key: 'hhi', label: 'HHI', num: true },
  { key: 'effn', label: 'Effective n', num: true },
  { key: 'n', label: 'N', num: true },
  { key: 'source', label: 'Source' },
];

/** Map cells of a slice (3.5 states; regional values never painted; zero solid; nq / nsr / not covered hatched). */
export function mapCells(model: SectorModel, v: VarInfo, item: ItemInfo | null, rows: readonly FactRow[], opts: { log?: boolean; highlight?: GeoInfo | null; dimOut?: (g: GeoInfo) => boolean; year?: number; diverging?: (v: number) => string }): { cells: Map<string, MapCell>; lo: number; hi: number } {
  const roster = model.rosterOf(v);
  const byGeo = new Map(rows.map((r) => [r.geo, r]));
  const vals = rows.filter((r) => Number.isFinite(r.value) && r.value > 0 && model.geos[r.geo]?.kind === 'country').map((r) => (opts.log ? Math.log10(r.value) : r.value));
  const lo = vals.length ? Math.min(...vals) : 0;
  const hi = vals.length ? Math.max(...vals) : 1;
  const color = seqColor(lo, hi);
  const cells = new Map<string, MapCell>();
  for (const g of model.countries) {
    if (!g.iso_n3) continue;
    const f = byGeo.get(g.idx) ?? null;
    const st = stateOf(f, g, v, roster, opts.year);
    let fill: MapCell['fill'] = 'hatch';
    if (f && Number.isFinite(f.value)) {
      // the zero fill is for additive measures; a bounded 0 takes the low end of the ramp
      if ((f.value === 0 && v.agg === 'sum') || st.state === 'zero') fill = 'zero';
      else if (opts.diverging) fill = opts.diverging(f.value);
      else if (f.value > 0) fill = color(opts.log ? Math.log10(f.value) : f.value);
      else if (f.value === 0 && !opts.log) fill = color(lo);
    }
    if (st.state === 'nsr' || st.state === 'nq' || st.state === 'na_growth') fill = 'hatch';
    cells.set(g.iso_n3, {
      iso3c: g.code,
      fill,
      outline: opts.highlight?.idx === g.idx,
      dim: opts.dimOut ? opts.dimOut(g) : undefined,
      tip: (
        <div className="space-y-0.5">
          <div className="text-sm font-semibold text-paper">{f && Number.isFinite(f.value) ? fmtFact(model, v, item, f.value) : stateShort(st, v)}</div>
          <div className="text-paper-dim">{g.name}</div>
          <div className="text-paper-mute">{stateWords(st, v, model, g)}</div>
        </div>
      ),
    });
  }
  return { cells, lo, hi };
}

/** CSV text of a concentration (the 3.9 / 4.0 columns). */
export function concentrationCsv(model: SectorModel, v: VarInfo, item: ItemInfo | null, year: number | null, c: Concentration, rows: readonly FactRow[]): string {
  const byGeo = new Map(rows.map((r) => [r.geo, r]));
  const input: CsvInput[] = c.rows.map((r) => ({ geo: model.geos[r.geo], v, item, year, value: r.value, status: byGeo.get(r.geo)?.status ?? 'obs', share: r.share, rank: r.rank }));
  for (const r of rows) {
    const g = model.geos[r.geo];
    if (g && g.kind === 'residual') input.push({ geo: null, economy: g.name, v, item, year, value: r.value, status: r.status, share: c.W ? r.value / c.W : null });
    if (g && !Number.isFinite(r.value)) input.push({ geo: g, v, item, year, value: null, status: r.status });
  }
  return sectorCsv(model, input);
}

/** CSV of plain fact rows. */
export function factsCsv(model: SectorModel, v: VarInfo, item: ItemInfo | null, year: number | null, rows: readonly FactRow[]): string {
  return sectorCsv(model, rows.map((r) => ({ geo: model.geos[r.geo], v, item, year: r.obs_year ?? year, value: Number.isFinite(r.value) ? r.value : null, status: r.status })));
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
