// Prop types of the Sector chart kit (spec_sector.md 5.3), written first so the views (builder C)
// can be built against them. The kit is presentational: components take pre-derived rows and
// pre-formatted strings (and, for axes, a tick formatter), so they need neither the dictionary nor
// src/sector/format.ts. Every chart is wrapped by its view in SectorChartCard (provenance required),
// has a table-twin row shape, hit targets >= 24 px, tooltips on every mark, and inserts labels as
// text, never HTML.

import type { ReactNode } from 'react';
import type { SplitPart, StageSlug, StatusCode } from '../contract.ts';

// ---- shared vocabulary -------------------------------------------------------------------------

/** The share-split parts in their fixed order (3.4): China | core | partners | rest | not attributed. */
export type GroupKey = SplitPart;
/** The group of one economy's mark (a single economy is never "not attributed"). */
export type MarkGroup = Exclude<GroupKey, 'unattributed'>;
/**
 * A value state (3.5): the fact statuses plus the three the client derives ("not covered" when the
 * roster knows the economy but the source has no row, "regional" for region / entity / EU values,
 * "old" when obs_year is older than the dictionary's stale_years).
 */
export type ValueState = StatusCode | 'not_covered' | 'regional' | 'old';

/** A number formatter for axes and tooltips (the views pass src/sector/format.ts functions). */
export type NumFormat = (v: number) => string;

// ---- ShareSplitChart / BlocBar -----------------------------------------------------------------

export interface ShareSplitRow {
  id: string;
  /** measure or item label ("Exports 2024"); text, never HTML */
  label: string;
  /** a second muted line ("UN Comtrade, reconciled matrix") */
  sub?: string;
  /** shares 0-1 per part; missing parts are 0; parts sum to <= 1 (the rest is drawn as not attributed) */
  parts: Partial<Record<GroupKey, number>>;
  /** pre-formatted text columns by column key ("top1": "China 43.7%") */
  cols: Record<string, string>;
  /** the highlighted economy: `offset` = the middle of its slice (0-1 along the bar), `label` = "USA 4.8% (#4)" */
  highlight?: { offset: number; label: string };
  /** a link (row click and the label) */
  href?: string;
  /** the current item: an orange outline around the bar */
  outlined?: boolean;
  /** projected values: outlined segments and the word "projection" */
  projection?: boolean;
  /** the key of the section (stage) the row sits under */
  section?: string;
  /** full text equivalent of the row (defaults to label + the parts in words) */
  ariaLabel?: string;
}

export interface ShareSplitColumn {
  key: string;
  /** header text ("Top-1", "HHI") */
  label: string;
  /** right-aligned tabular figures */
  num?: boolean;
}

export interface ShareSplitSection {
  key: string;
  label: string;
  /** draws the 3 px stage stripe beside the header */
  stage?: StageSlug | null;
}

export interface ShareSplitChartProps {
  rows: ShareSplitRow[];
  columns: ShareSplitColumn[];
  sections?: ShareSplitSection[];
  onRowClick?: (row: ShareSplitRow) => void;
  ariaLabel: string;
  /** label column width on wide screens, px (default 220) */
  labelWidth?: number;
  /** direct % labels inside segments at least this wide (default 0.09; spec: 9%, entity rows 6%) */
  minLabelShare?: number;
  /** the label of the highlight column when a highlight exists ("United States") */
  highlightHeader?: string;
}

export interface BlocBarProps {
  parts: Partial<Record<GroupKey, number>>;
  ariaLabel: string;
  label?: string;
  highlight?: { offset: number; label: string };
  minLabelShare?: number;
}

// ---- LeagueBars --------------------------------------------------------------------------------

export interface LeagueRow {
  id: string;
  /** economy name; the highlighted one is drawn bold */
  label: string;
  group: MarkGroup;
  /** value in the measure's unit (NaN for a state without a value) */
  value: number;
  /** end label, pre-formatted ("USD 88.7 bn · 43.7%") */
  display: string;
  /** pinned below the top rows after a gap ("…"), e.g. the highlighted economy */
  pinned?: boolean;
  state?: ValueState;
  /** a badge after the label ("mirror", "re-export hub", "(2014)") */
  badge?: string;
}

export interface LeagueBarsProps {
  /** rows in rank order */
  rows: LeagueRow[];
  /** x axis label with unit and year ("US dollars, 2024") */
  axisLabel: string;
  /** the highlighted row id (orange 2 px stroke + bold label) */
  selected?: string | null;
  onSelect?: (row: LeagueRow) => void;
  /** rows shown before the pinned ones (default 20) */
  top?: number;
  /** diverging bars around zero, with the poles in words ("net importer ←", "→ net exporter") */
  diverging?: { negLabel: string; posLabel: string };
  ariaLabel: string;
  tickFormat?: NumFormat;
  /** log x scale (positive values only) */
  log?: boolean;
}

// ---- GroupArea ---------------------------------------------------------------------------------

export interface GroupAreaPoint {
  x: number;
  group: GroupKey;
  value: number;
  /** the source flags the value (CIM 2025 includes estimated Q4): hollow marker */
  est?: boolean;
}

export interface GroupAreaProps {
  series: GroupAreaPoint[];
  /** 100% stacked (share of the total) */
  normalize?: boolean;
  yLabel: string;
  ariaLabel: string;
  yTickFormat?: NumFormat;
  /** tooltip values ("USD 32.9 bn") */
  valueFormat: NumFormat;
  /** x tick format (default: the year as an integer) */
  xTickFormat?: NumFormat;
  onPickX?: (x: number) => void;
  height?: number;
}

// ---- LabelledLines -----------------------------------------------------------------------------

export interface LinePoint {
  x: number;
  y: number;
  est?: boolean;
  projection?: boolean;
}

export interface LineSeries {
  id: string;
  label: string;
  /** a group colour, or neutral (world, rest of world, context lines) */
  group: GroupKey | 'neutral';
  points: LinePoint[];
}

export interface LabelledLinesProps {
  /** at most 7 lines */
  lines: LineSeries[];
  /** the highlighted line id: drawn last with a 2 px orange halo */
  highlight?: string | null;
  /** index every line to its first point = 100 */
  index?: boolean;
  yLabel: string;
  ariaLabel: string;
  yTickFormat?: NumFormat;
  valueFormat: NumFormat;
  xTickFormat?: NumFormat;
  onPickX?: (x: number) => void;
  log?: boolean;
  height?: number;
}

// ---- Dumbbell ----------------------------------------------------------------------------------

export interface DumbbellRow {
  id: string;
  label: string;
  /** the first value (hollow: the earlier year or the projection) */
  a: number;
  /** the second value (filled: the later year or the observation) */
  b: number;
  group: MarkGroup | 'neutral';
  aLabel: string;
  bLabel: string;
  href?: string;
}

export interface DumbbellProps {
  rows: DumbbellRow[];
  xLabel: string;
  /** what a and b are ("2025", "2040 STEPS (projection)") for the legend and tooltips */
  aName: string;
  bName: string;
  ariaLabel: string;
  selected?: string | null;
  onSelect?: (row: DumbbellRow) => void;
  tickFormat?: NumFormat;
  log?: boolean;
  /** a reference rule (e.g. 100 for "China = 100") */
  rule?: { x: number; label: string } | null;
}

// ---- Beeswarm ----------------------------------------------------------------------------------

export interface BeeswarmPoint {
  id: string;
  label: string;
  group: MarkGroup;
  value: number;
  display: string;
  state?: ValueState;
  /** draw the label (China, the largest coalition member, the highlight) */
  labelled?: boolean;
}

export interface BeeswarmProps {
  points: BeeswarmPoint[];
  /** median ticks ("World median", "Coalition median") */
  medians: { label: string; value: number }[];
  log?: boolean;
  xLabel: string;
  ariaLabel: string;
  selected?: string | null;
  onSelect?: (p: BeeswarmPoint) => void;
  tickFormat?: NumFormat;
  /** list zeros in the "0" strip instead of drawing them (additive measures; default true); a bounded 0 is a dot */
  zeroStrip?: boolean;
}

// ---- SmallMultiples ----------------------------------------------------------------------------

export interface SmallMultiplePanel<R> {
  id: string;
  title: string;
  sub?: string;
  rows: R[];
  /** an empty panel's reason ("no data for this cell"), drawn as a quiet tile */
  empty?: string;
}

export interface SmallMultiplesProps<R> {
  panels: SmallMultiplePanel<R>[];
  /** one y domain for every panel */
  shared: boolean;
  /** the y value of a row (for the shared domain) */
  yValue: (r: R) => number;
  /** Plot options of one panel; `yDomain` is set when `shared` */
  render: (panel: SmallMultiplePanel<R>, ctx: { width: number; height: number; yDomain: [number, number] | null }) => import('@observablehq/plot').PlotOptions | null;
  /** panel height in px (default 140) */
  height?: number;
  /** minimum panel width before the grid wraps (default 220) */
  minPanelWidth?: number;
  /** a fixed number of columns (1: every panel spans the card's width, e.g. monthly prices); default auto-fill */
  columns?: number;
  ariaLabel: (panel: SmallMultiplePanel<R>) => string;
  tooltip?: (d: R) => ReactNode;
}

// ---- HeatTable ---------------------------------------------------------------------------------

export interface HeatRow {
  key: string;
  label: string;
  sub?: string;
  /** sub-rows behind a disclosure chevron (e.g. a technology's minerals) */
  children?: HeatRow[];
}

export interface HeatCol {
  key: string;
  label: string;
  stage?: StageSlug | null;
}

export interface HeatCellValue {
  /** the shading value (null = no data: hatched) */
  value: number | null;
  /** printed text ("CHN 74%"); "" for none */
  display: string;
  /** a second line ("0.207", "≥ 0.21") */
  sub?: string;
  /** full text for the tooltip and the button's accessible name */
  title?: string;
  /** the cell is the current one (orange outline) */
  selected?: boolean;
  disabled?: boolean;
}

export interface HeatTableProps {
  rows: HeatRow[];
  cols: HeatCol[];
  cell: (rowKey: string, colKey: string) => HeatCellValue;
  /** cells are buttons when given */
  onCell?: (rowKey: string, colKey: string) => void;
  /**
   * the three-colour scale of the cell fills (charts/scale.ts triScale over the values the caller
   * shows; spec_country.md C1.5). Default: triScale over every cell value of the table (rows and their
   * sub-rows, so opening a row never repaints the others), white = their median.
   */
  scale?: import('../../charts/scale.ts').TriScale;
  /** @deprecated ignored since the three-colour scale (2026-10-12): pass `scale` */
  domain?: [number, number];
  /** reverse the default scale (e.g. effective n: smaller = more concentrated); ignored with `scale` */
  reverse?: boolean;
  caption: string;
  /** header of the first column */
  rowHeader?: string;
}

// ---- CellGrid ----------------------------------------------------------------------------------

export interface CellGridTech {
  slug: string;
  label: string;
  /** false for Semiconductors: drawn after a divider, under "Non-headline" */
  headline: boolean;
}

export interface CellGridProps {
  techs: CellGridTech[];
  stages: { slug: StageSlug; label: string }[];
  /** the tile of a cell, or null for a cell that does not exist (an empty slot) */
  renderTile: (tech: CellGridTech, stage: StageSlug, ctx: { selected: boolean }) => ReactNode;
  /** "batteries-midstream" */
  selected?: string | null;
  ariaLabel: string;
  /** below 768 px: one <details> per technology (the first open) instead of the grid (default true) */
  accordion?: boolean;
}

// ---- TopFlowsList ------------------------------------------------------------------------------

export interface TopFlow {
  id: string;
  from: string;
  to: string;
  fromGroup: MarkGroup;
  /** "USD 21.4 bn" */
  value: string;
  /** "10.5% of world trade" */
  share: string;
  /** "partner mirror ×1.06" */
  flag?: string;
}

export interface TopFlowsListProps {
  flows: TopFlow[];
  ariaLabel: string;
  /** ranked from 1 (default true) */
  numbered?: boolean;
}

// ---- legends and chips -------------------------------------------------------------------------

export interface GroupLegendProps {
  /** the parts to list (default all five, in order) */
  groups?: GroupKey[];
  /** adds "{name} (highlighted)" with the orange outline key */
  highlight?: string | null;
  /** adds the outlined "projection" key */
  projection?: boolean;
  /** adds the hollow "estimate" key */
  estimate?: boolean;
  /** "EU-27 counted as one" note (bloc mode) */
  euBloc?: boolean;
  /** regions whose members span groups (IEA EV regions) are shown: their share is not attributed */
  mixed?: boolean;
}

export interface StateLegendProps {
  /** the states present (only these are listed) */
  states: ValueState[];
  /** the source's short name for the "not separately reported" words */
  source?: string;
}

export type CoverageStatus = 'country' | 'region' | 'global' | 'licensed' | 'none';

export interface CoverageChip {
  family: string;
  /** "Trade" */
  label: string;
  status: CoverageStatus;
  /** "Comtrade 2024", "IEA, 6 entities", "not applicable" */
  text: string;
  /** chips with data link to their section ("#trade") */
  href?: string;
  /** the reason text shown in the popover of a chip without data */
  reason?: string;
}

export interface CoverageChipsProps {
  chips: CoverageChip[];
  ariaLabel?: string;
}

// ---- captions (3.9) ----------------------------------------------------------------------------

/** One measure shown in a card: its var id and the selected year, window or item. */
export interface VarRef {
  var: string;
  item?: string | null;
  year?: number | null;
  /** a series window [from, to] */
  window?: [number, number] | null;
}

/** What the caption contract renders, resolved from the dictionary by the views' resolver. */
export interface CaptionInfo {
  /** the sub line: "{label} · {unit long} · {year or window} · {source short} {edition} · {n} economies" */
  sub: string;
  /** the vintage badge: "2024", "2020 → 2024", "end-2020 · not updated", "2035 · projection" */
  badge?: string | null;
  /** dictionary caveats in words: the first two inline, the rest in the SectorBar "Data notes" */
  caveats?: string[];
  /** the coverage line ("143 economies declared their 2024 imports; …") */
  coverage?: string | null;
  /** when the vars differ in year or source: the note that says so */
  spanNote?: string | null;
}

/** Resolves vars to their caption (the views provide one from the dictionary: CaptionProvider). */
export type CaptionResolver = (vars: readonly VarRef[]) => CaptionInfo;
