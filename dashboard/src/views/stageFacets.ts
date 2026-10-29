// Stage panels for the Strategic views' faceted charts (2026-10-29; design E1 and S4): one panel per
// supply-chain stage, in the data's chain order (Extraction ... Recovery), wrapped into a grid that fits
// the width. All the stages sit side by side where each panel gets at least `minPanel` px; otherwise three
// a row (two rows), two a row (three rows) or one. Plot facets on the panel's column (fx) and row (fy)
// indices; the stage is named above its panel in text and is never a colour series (S4). The layout is
// pure (unit-tested in stageFacets.test.ts); the mark helpers only build Plot marks.

import * as Plot from '@observablehq/plot';
import { PAPER_DIM } from '../theme/palette.ts';
import { stageLabel } from '../lib/stages.ts';

export interface StagePanel {
  stage: string;
  /** the stage's display label (lib/stages STAGE_LABEL) */
  label: string;
  col: number;
  row: number;
}

export interface StageGrid {
  cols: number;
  rows: number;
  panels: StagePanel[];
  /** the panel of a stage (undefined: not one of the grid's stages) */
  at: (stage: string) => StagePanel | undefined;
}

/**
 * How many panels a row holds: every stage when each panel gets `minPanel` px of `inner` (the plot's
 * width less its margins), else 3, else 2, else 1. `choices` narrows the candidates (a chart that must
 * keep at least three a row passes [n, 3] and scrolls instead).
 */
export function stageCols(n: number, inner: number, minPanel: number, choices?: readonly number[]): number {
  if (n <= 1) return 1;
  const cands = (choices ?? [n, 3, 2, 1]).filter((c) => c >= 1 && c <= n);
  const ordered = [...new Set(cands)].sort((a, b) => b - a);
  for (const c of ordered) if (inner / c >= minPanel) return c;
  return ordered[ordered.length - 1] ?? 1;
}

/** The stages laid out `cols` a row, in order (left to right, then down). */
export function stageGrid(stages: readonly string[], cols: number): StageGrid {
  const c = Math.max(1, Math.min(Math.floor(cols), Math.max(1, stages.length)));
  const panels = stages.map((stage, i) => ({ stage, label: stageLabel(stage), col: i % c, row: Math.floor(i / c) }));
  const by = new Map(panels.map((p) => [p.stage, p]));
  return { cols: c, rows: Math.max(1, Math.ceil(stages.length / c)), panels, at: (s) => by.get(s) };
}

export interface StageFacetOptions {
  width: number;
  marginLeft: number;
  marginRight: number;
  marginTop: number;
  marginBottom: number;
  /** the height of one panel's plotting area */
  panelHeight: number;
  /** the narrowest a panel may be before the row wraps */
  minPanel: number;
  /** px between panels side by side (default 12) */
  gapX?: number;
  /** px between rows of panels: room for the next row's stage name and this row's tick labels (default 40) */
  gapY?: number;
  /** the allowed panels a row (default: all, 3, 2, 1) */
  choices?: readonly number[];
}

export interface StageFacetLayout {
  grid: StageGrid;
  /** the figure's height for the grid */
  height: number;
  panelWidth: number;
  panelHeight: number;
  /** Plot's fx and fy scale options (index facets, no facet axes: stageLabels names the panels) */
  fx: Plot.ScaleOptions;
  fy: Plot.ScaleOptions;
  /** the facet channels of a datum with a `stage` (spread into a mark's options) */
  facet: { fx: (d: { stage: string }) => number | undefined; fy: (d: { stage: string }) => number | undefined };
}

/**
 * The grid of stage panels for a figure `width` px wide: Plot's band scales for fx and fy are set so that
 * each panel is `panelWidth` x `panelHeight` with exact gaps (step = panel + gap, paddingInner = gap / step,
 * no outer padding), and the figure is as tall as its rows need.
 */
export function stageFacetLayout(stages: readonly string[], o: StageFacetOptions): StageFacetLayout {
  const gapX = o.gapX ?? 12;
  const gapY = o.gapY ?? 40;
  const inner = Math.max(1, o.width - o.marginLeft - o.marginRight);
  const cols = stageCols(stages.length, inner, o.minPanel, o.choices);
  const grid = stageGrid(stages, cols);
  const panelWidth = grid.cols > 1 ? (inner - gapX * (grid.cols - 1)) / grid.cols : inner;
  const height = o.marginTop + grid.rows * o.panelHeight + (grid.rows - 1) * gapY + o.marginBottom;
  const range = (n: number) => Array.from({ length: n }, (_, i) => i);
  const stepX = panelWidth + gapX;
  const stepY = o.panelHeight + gapY;
  return {
    grid,
    height,
    panelWidth,
    panelHeight: o.panelHeight,
    fx: { domain: range(grid.cols), axis: null, paddingInner: grid.cols > 1 ? gapX / stepX : 0, paddingOuter: 0 },
    fy: { domain: range(grid.rows), axis: null, paddingInner: grid.rows > 1 ? gapY / stepY : 0, paddingOuter: 0 },
    facet: { fx: (d) => grid.at(d.stage)?.col, fy: (d) => grid.at(d.stage)?.row },
  };
}

/** The stage's name above its panel (text, not a colour: S4). */
export function stageLabels(grid: StageGrid, o: { fill?: string; fontSize?: number; upper?: boolean } = {}): Plot.Markish {
  return Plot.text(grid.panels, {
    fx: 'col',
    fy: 'row',
    frameAnchor: 'top-left',
    lineAnchor: 'bottom',
    dy: -8,
    text: (p: StagePanel) => (o.upper ? p.label.toUpperCase() : p.label),
    fill: o.fill ?? PAPER_DIM,
    fontSize: o.fontSize ?? 11,
    fontWeight: 500,
  });
}

/**
 * An x axis on every panel (each row of a wrapped grid reads on its own) and the axis label once, below
 * the bottom row. With explicit axis marks Plot draws no implicit x axis or grid, so the grid lines are
 * here too (`grid`, default true).
 */
export function panelAxisX(o: {
  ticks?: number | readonly number[];
  tickFormat?: (v: never) => string;
  tickRotate?: number;
  tickSize?: number;
  label?: string | null;
  grid?: boolean;
}): Plot.Markish[] {
  const ticks = o.ticks as Plot.AxisOptions['ticks'];
  const tickFormat = o.tickFormat as Plot.AxisOptions['tickFormat'];
  return [
    o.grid === false ? null : Plot.gridX({ ticks }),
    Plot.axisX({ facetAnchor: null, ticks, tickFormat, tickRotate: o.tickRotate, tickSize: o.tickSize, label: null }),
    o.label ? Plot.axisX({ facetAnchor: 'bottom', ticks: [], tickSize: 0, label: o.label }) : null,
  ];
}
