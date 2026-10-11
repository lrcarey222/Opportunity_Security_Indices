// Who supplies whom: a producer -> consumer Sankey (d3-sankey layout, React SVG). Producers on the
// left, consumers on the right; band width = share of the stage's coalition demand. Hovered bands
// turn orange; every value is in the table twin.
// Additive for Sector Insights (spec_sector.md 4.3 S4b, 5.2): valueFormat, nodeColor, linkColor,
// onSelectLink, sideLabels and tipNote; the defaults are the Strategic chart unchanged. SankeyFlow
// is unchanged (producer = exporter, consumer = importer in Sector).

import { useMemo, useState } from 'react';
import { sankey, sankeyLinkHorizontal, type SankeyGraph, type SankeyLink, type SankeyNode } from 'd3-sankey';
import { useWidth } from './WorldMap.tsx';
import { COMPARE_COLOR, FLOW, PAPER_DIM, SIGNAL } from '../theme/palette.ts';
import { fmtPct } from '../lib/format.ts';
import { fitLabel } from './theme.ts';

export interface SankeyFlow {
  producer: string;
  consumer: string;
  value: number;
}

interface N {
  id: string;
  label: string;
  side: 'p' | 'c';
}
type L = { source: string; target: string; value: number; i?: number };

export interface SankeyChartProps {
  flows: SankeyFlow[];
  label: (code: string) => string;
  ariaLabel: string;
  /** the tooltip value (default: a share, fmtPct(v, 1)) */
  valueFormat?: (v: number) => string;
  /** a node's colour by code and side (default: producers blue, consumers gray) */
  nodeColor?: (code: string, side: 'producer' | 'consumer') => string;
  /** a link's colour at rest (default: the muted flow blue); the hovered link is always orange */
  linkColor?: (flow: SankeyFlow) => string;
  /** a click on a link */
  onSelectLink?: (flow: SankeyFlow) => void;
  /** the column headings (default PRODUCERS / CONSUMERS) */
  sideLabels?: { left: string; right: string };
  /** the tooltip's last line (default: "share of the stage's coalition demand") */
  tipNote?: string;
}

export function SankeyChart({ flows, label, ariaLabel, valueFormat, nodeColor, linkColor, onSelectLink, sideLabels, tipNote }: SankeyChartProps) {
  const [ref, width] = useWidth<HTMLDivElement>();
  const [hover, setHover] = useState<{ i: number; x: number; y: number } | null>(null);
  // only positive flows: a node without a link has no height in the layout (NaN positions)
  const live = useMemo(() => flows.filter((f) => f.value > 0 && Number.isFinite(f.value)), [flows]);
  const nodesIn = useMemo(() => {
    const ps = [...new Set(live.map((f) => f.producer))];
    const cs = [...new Set(live.map((f) => f.consumer))];
    return { ps, cs };
  }, [live]);
  const height = Math.max(260, 18 * Math.max(nodesIn.ps.length, nodesIn.cs.length) + 56);
  // label columns: 130 px each on wide cards, narrower on phones (labels truncated, full text in the table)
  const side = Math.max(70, Math.min(130, Math.floor(width * 0.27)));
  const fs = width < 480 ? 10 : 11;
  const graph = useMemo(() => {
    if (width <= 0 || !live.length) return null;
    const nodes: N[] = [
      ...nodesIn.ps.map((p) => ({ id: `p:${p}`, label: label(p), side: 'p' as const })),
      ...nodesIn.cs.map((c) => ({ id: `c:${c}`, label: label(c), side: 'c' as const })),
    ];
    const links: L[] = live.map((f, i) => ({ source: `p:${f.producer}`, target: `c:${f.consumer}`, value: f.value, i }));
    const layout = sankey<N, L>()
      .nodeId((d) => d.id)
      .nodeWidth(10)
      .nodePadding(6)
      .extent([
        [side, 24],
        [Math.max(side + 60, width - side), height - 8],
      ]);
    return layout({ nodes: nodes.map((d) => ({ ...d })), links: links.map((d) => ({ ...d })) } as SankeyGraph<N, L>);
  }, [live, width, height, label, nodesIn, side]);
  const path = sankeyLinkHorizontal<N, L>();
  const hl = hover ? (graph?.links[hover.i] as (SankeyLink<N, L> & { width?: number }) | undefined) : undefined;
  return (
    <div ref={ref} className="relative w-full" style={{ minHeight: height }}>
      {graph && (
        <svg width={width} height={height} role="img" aria-label={ariaLabel} className="block">
          <g fill="none">
            {graph.links.map((l, i) => (
              <path
                key={i}
                d={path(l) ?? ''}
                stroke={hover?.i === i ? SIGNAL : linkColor ? linkColor(live[(l as L).i ?? 0]) : FLOW}
                strokeOpacity={hover && hover.i !== i ? 0.5 : 1}
                strokeWidth={Math.max(1, (l as { width?: number }).width ?? 1)}
                onPointerMove={(e) => {
                  const r = (e.currentTarget.ownerSVGElement as SVGSVGElement).getBoundingClientRect();
                  setHover({ i, x: e.clientX - r.left, y: e.clientY - r.top });
                }}
                onPointerLeave={() => setHover(null)}
                onClick={onSelectLink ? () => onSelectLink(live[(l as L).i ?? 0]) : undefined}
                style={{ cursor: onSelectLink ? 'pointer' : undefined }}
              />
            ))}
          </g>
          <g>
            {graph.nodes.map((n) => {
              const nn = n as SankeyNode<N, L>;
              const x0 = nn.x0 ?? 0;
              const x1 = nn.x1 ?? 0;
              const y0 = nn.y0 ?? 0;
              const y1 = nn.y1 ?? 0;
              return (
                <g key={n.id}>
                  <rect x={x0} y={y0} width={x1 - x0} height={Math.max(1, y1 - y0)} fill={nodeColor ? nodeColor(n.id.slice(2), n.side === 'p' ? 'producer' : 'consumer') : n.side === 'p' ? COMPARE_COLOR.scenario : '#6b7280'} />
                  <text
                    x={n.side === 'p' ? x0 - 6 : x1 + 6}
                    y={(y0 + y1) / 2}
                    dy="0.35em"
                    textAnchor={n.side === 'p' ? 'end' : 'start'}
                    fill={PAPER_DIM}
                    fontSize={fs}
                  >
                    {fitLabel(n.label, side - 10, fs)}
                    {fitLabel(n.label, side - 10, fs) !== n.label && <title>{n.label}</title>}
                  </text>
                </g>
              );
            })}
          </g>
          <text x={side - 6} y={12} textAnchor="end" fill="#9ca3af" fontSize={10} letterSpacing="0.08em">
            {sideLabels?.left ?? 'PRODUCERS'}
          </text>
          <text x={Math.max(side + 60, width - side) + 16} y={12} fill="#9ca3af" fontSize={10} letterSpacing="0.08em">
            {sideLabels?.right ?? 'CONSUMERS'}
          </text>
        </svg>
      )}
      {hover && hl && (
        <div
          className="pointer-events-none absolute z-20 border border-signal bg-ink-900 px-3 py-2 text-xs text-paper-dim"
          style={{ left: Math.min(hover.x + 14, width - 200), top: Math.max(0, hover.y - 10) }}
          role="status"
        >
          <div className="text-sm font-semibold text-paper">{valueFormat ? valueFormat(hl.value) : fmtPct(hl.value, 1)}</div>
          <div>
            {(hl.source as N).label} {'→'} {(hl.target as N).label}
          </div>
          <div className="text-paper-mute">{tipNote ?? 'share of the stage’s coalition demand'}</div>
        </div>
      )}
    </div>
  );
}
