// The Sectoral Compacts strategic map (IA refactor, 2026-10-16; brief s.16): one bubble per supply-chain
// node, across = shock exposure (share of demand the shock removes), up = coordination dividend (coverage
// coordinated trade adds over existing relationships), area = lost supply (USD), colour and mark = the
// node's class (filled for the three intervention classes, hollow for LOW INTERVENTION, dashed for
// INSUFFICIENT DATA: the class marks of ClassChip). Dotted rules: the model's high-severity and
// coordination-dividend thresholds. Hover for the node; a click opens its compact.

import { useCallback } from 'react';
import * as Plot from '@observablehq/plot';
import { PlotFigure, Tip } from '../../charts/PlotFigure.tsx';
import { isNum } from '../../lib/format.ts';
import { SIGNAL } from '../../theme/palette.ts';
import type { AibNode } from '../contract.ts';
import { classInfo } from '../derive/classes.ts';
import { fmtPp, fmtShare, fmtUsd } from '../derive/format.ts';
import { dividendOf } from '../derive/partnerships.ts';

export interface MapPoint {
  node: AibNode;
  x: number;
  y: number;
  r: number;
}

/** The nodes the map can place (exposure and dividend both known). */
export function mapPoints(nodes: readonly AibNode[]): MapPoint[] {
  return nodes
    .map((n) => ({ node: n, x: n.exposure, y: dividendOf(n), r: isNum(n.lostUsd) ? n.lostUsd : 0 }))
    .filter((p): p is MapPoint => isNum(p.x) && isNum(p.y))
    .sort((a, b) => b.r - a.r);
}

export function StrategicMap({
  points, highSeverity, dividendRule, ariaLabel, onSelect, height = 380,
}: {
  points: MapPoint[];
  highSeverity: number | null;
  dividendRule: number | null;
  ariaLabel: string;
  onSelect?: (p: MapPoint) => void;
  height?: number;
}) {
  const build = useCallback(
    (width: number): Plot.PlotOptions | null => {
      if (!points.length) return null;
      const ys = points.map((p) => p.y);
      const yMax = Math.max(0.05, ...ys, dividendRule ?? 0) * 1.08;
      const yMin = Math.min(0, ...ys);
      const color = (p: MapPoint) => classInfo(p.node.cls).color;
      const filled = points.filter((p) => classInfo(p.node.cls).mark === 'fill');
      const hollow = points.filter((p) => classInfo(p.node.cls).mark === 'hollow');
      const hatch = points.filter((p) => classInfo(p.node.cls).mark === 'hatch');
      const small = width < 520;
      return {
        marginLeft: 44,
        marginBottom: 40,
        x: { domain: [0, 1], label: 'Shock exposure: share of demand lost →', tickFormat: (d: number) => `${Math.round(d * 100)}%`, grid: true, ticks: small ? 4 : 10 },
        y: { domain: [yMin, yMax], label: '↑ Coordination dividend (percentage points)', tickFormat: (d: number) => `${Math.round(d * 100)}`, grid: true },
        r: { range: [2.5, small ? 14 : 20] },
        marks: [
          isNum(highSeverity) ? Plot.ruleX([highSeverity], { stroke: 'rgb(255 255 255 / 0.35)', strokeDasharray: '2,4' }) : null,
          isNum(dividendRule) ? Plot.ruleY([dividendRule], { stroke: 'rgb(255 255 255 / 0.35)', strokeDasharray: '2,4' }) : null,
          isNum(highSeverity) ? Plot.text([highSeverity], { x: (d: number) => d, y: yMax, text: () => 'high severity →', textAnchor: 'start', dx: 4, dy: 6, fill: '#9ca3af', fontSize: 11 }) : null,
          isNum(dividendRule) ? Plot.text([dividendRule], { x: 1, y: (d: number) => d, text: () => 'coordination pays ↑', textAnchor: 'end', dy: -7, fill: '#9ca3af', fontSize: 11 }) : null,
          Plot.dot(hollow, { x: 'x', y: 'y', r: 'r', fill: 'none', stroke: color, strokeWidth: 1.5 }),
          Plot.dot(hatch, { x: 'x', y: 'y', r: 'r', fill: 'none', stroke: color, strokeWidth: 1.2, strokeDasharray: '2,2' }),
          Plot.dot(filled, { x: 'x', y: 'y', r: 'r', fill: color, fillOpacity: 0.78, stroke: '#0b1220', strokeWidth: 0.8 }),
          Plot.dot(points, Plot.pointer({ x: 'x', y: 'y', r: 'r', fill: 'none', stroke: SIGNAL, strokeWidth: 2, maxRadius: 30 })),
        ],
      };
    },
    [points, highSeverity, dividendRule],
  );
  return (
    <PlotFigure<MapPoint>
      build={build}
      height={height}
      ariaLabel={ariaLabel}
      onSelect={onSelect}
      tooltip={(p) => (
        <Tip
          value={p.node.label}
          title={`${p.node.techs.join(', ')} · ${p.node.stage}: ${classInfo(p.node.cls).label}`}
          lines={[
            `Shock exposure ${fmtShare(p.node.exposure, 0)} (${fmtUsd(p.node.lostUsd)} lost)`,
            `Coordination dividend ${fmtPp(p.y)}`,
            `Residual gap after trade ${fmtShare(p.node.residual, 0)}`,
            p.node.compact ? `Compact ${p.node.compact} (click to open)` : 'No compact',
          ]}
        />
      )}
    />
  );
}
