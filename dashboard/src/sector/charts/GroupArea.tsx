// GroupArea (spec_sector.md 4.3 S7, 4.6 "Over time", 5.3): a stacked area of a measure by group over
// time (China | core | partners | rest | not attributed, in that order; the residual hatched), or the
// same normalised to shares. Right-edge labels name the bands that are tall enough; a crosshair tip
// lists every part and the total at the pointed year; estimated points (CIM 2025) get a hollow marker.

import { useCallback, useMemo } from 'react';
import * as Plot from '@observablehq/plot';
import { PlotFigure, Tip } from '../../charts/PlotFigure.tsx';
import { labelMargin } from '../../charts/theme.ts';
import { INK, PAPER_DIM, SIGNAL, ZERO_RULE } from '../../theme/palette.ts';
import { groupFill, groupLabel, SPLIT_ORDER } from './color.ts';
import type { GroupAreaPoint, GroupAreaProps, GroupKey } from './types.ts';

export interface AreaColumn {
  x: number;
  parts: Partial<Record<GroupKey, number>>;
  total: number;
  est: boolean;
}

/** One row per x: the parts, their total and whether any part is an estimate. */
export function areaColumns(series: readonly GroupAreaPoint[]): AreaColumn[] {
  const by = new Map<number, AreaColumn>();
  for (const p of series) {
    if (!Number.isFinite(p.x)) continue;
    const c = by.get(p.x) ?? { x: p.x, parts: {}, total: 0, est: false };
    const v = Number.isFinite(p.value) ? p.value : 0;
    c.parts[p.group] = (c.parts[p.group] ?? 0) + v;
    c.total += v;
    c.est = c.est || !!p.est;
    by.set(p.x, c);
  }
  return [...by.values()].sort((a, b) => a.x - b.x);
}

const defaultX = (v: number) => String(Math.round(v));

export function GroupArea({ series, normalize, yLabel, ariaLabel, yTickFormat, valueFormat, xTickFormat = defaultX, onPickX, height = 280 }: GroupAreaProps) {
  const cols = useMemo(() => areaColumns(series), [series]);
  const groups = useMemo(() => SPLIT_ORDER.filter((g) => series.some((p) => p.group === g && Number.isFinite(p.value) && p.value !== 0)), [series]);
  const build = useCallback(
    (width: number): Plot.PlotOptions | null => {
      if (!cols.length || !groups.length) return null;
      // dense points (every group at every x, missing = 0) so the stack never tears
      const data = cols.flatMap((c) => groups.map((g) => ({ x: c.x, group: g, value: c.parts[g] ?? 0 })));
      const last = cols[cols.length - 1];
      // right-edge labels at the middle of each band of the last column, when the band is tall enough
      let acc = 0;
      const denom = normalize ? last.total || 1 : 1;
      const yMax = normalize ? 1 : Math.max(...cols.map((c) => c.total)) || 1;
      const labels = groups
        .map((g) => {
          const v = (last.parts[g] ?? 0) / denom;
          const mid = acc + v / 2;
          acc += v;
          return { g, y: mid, h: v };
        })
        .filter((l) => l.h / yMax >= 0.06);
      const mr = labelMargin(labels.map((l) => groupLabel(l.g)), { min: 16, max: Math.min(150, Math.floor(width * 0.3)) });
      const estCols = cols.filter((c) => c.est);
      return {
        marginRight: mr,
        x: { label: null, tickFormat: xTickFormat, ticks: Math.min(cols.length, Math.max(2, Math.floor(width / 70))) },
        y: { label: normalize ? (/^share/i.test(yLabel) ? yLabel : `Share of the total (${yLabel})`) : yLabel, grid: true, tickFormat: normalize ? (v: number) => `${Math.round(v * 100)}%` : yTickFormat, domain: normalize ? [0, 1] : undefined },
        marks: [
          Plot.areaY(data, Plot.stackY({ x: 'x', y: 'value', z: 'group', fill: (d: { group: GroupKey }) => groupFill(d.group), order: groups, offset: normalize ? 'normalize' : null, curve: 'linear' })),
          // the surface line between bands, at the top of each band (stackY2; stackY's y is the band's
          // midpoint, which drew a dark line through the middle of every band)
          Plot.lineY(data, Plot.stackY2({ x: 'x', y: 'value', z: 'group', stroke: INK[800], strokeWidth: 1, order: groups, offset: normalize ? 'normalize' : null })),
          Plot.ruleY([0], { stroke: ZERO_RULE }),
          Plot.text(labels, { x: last.x, y: 'y', text: (l: { g: GroupKey }) => groupLabel(l.g), dx: 6, textAnchor: 'start', fill: PAPER_DIM, fontSize: 11 }),
          estCols.length ? Plot.dot(estCols, { x: 'x', y: (c: AreaColumn) => (normalize ? 1 : c.total), r: 4.5, fill: INK[800], stroke: PAPER_DIM, strokeWidth: 2 }) : null,
          Plot.ruleX(cols, Plot.pointerX({ x: 'x', stroke: SIGNAL, strokeWidth: 1.5 })),
        ],
      };
    },
    [cols, groups, normalize, yLabel, yTickFormat, xTickFormat],
  );
  return (
    <PlotFigure<AreaColumn>
      build={build}
      height={height}
      ariaLabel={ariaLabel}
      onSelect={onPickX ? (c) => onPickX(c.x) : undefined}
      tooltip={(c) => (
        <Tip
          value={`${xTickFormat(c.x)}${c.est ? ' (includes an estimate)' : ''}`}
          lines={[
            ...groups.map((g) => {
              const v = c.parts[g] ?? 0;
              return `${groupLabel(g)}: ${valueFormat(v)}${c.total > 0 ? ` (${((100 * v) / c.total).toFixed(1)}%)` : ''}`;
            }),
            `Total: ${valueFormat(c.total)}`,
          ]}
        />
      )}
    />
  );
}
