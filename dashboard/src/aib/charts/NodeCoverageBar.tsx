// One node's coverage on a 0-100% track (SVG): the static shock as a gray tick, existing relationships
// as a hollow dot, the optimized trade solution as a filled dot, joined by a line (the coordination
// dividend); the empty track from the filled dot to 100% is the residual gap. Values are in the row's text
// and the table twin; the legend mirrors the marks.

import { isNum } from '../../lib/format.ts';
import { useWidth } from '../../charts/WorldMap.tsx';

export const NODE_MARK = { static: '#9ca3af', existing: '#d1d5db', optimized: '#1889b0' } as const;

export function NodeCoverageBar({ stat, existing, optimized, label }: { stat: number | null; existing: number | null; optimized: number | null; label: string }) {
  const [ref, w] = useWidth<HTMLDivElement>();
  const W = Math.max(120, w);
  const H = 22;
  const pad = 6;
  const x = (v: number) => pad + Math.max(0, Math.min(1, v)) * (W - 2 * pad);
  const y = H / 2;
  return (
    <div ref={ref} className="w-full">
      {w > 0 && (
        <svg width={W} height={H} role="img" aria-label={label} className="block">
          <line x1={x(0)} x2={x(1)} y1={y} y2={y} stroke="rgb(255 255 255 / 0.15)" strokeWidth={4} />
          {[0, 0.5, 1].map((t) => (
            <line key={t} x1={x(t)} x2={x(t)} y1={y - 6} y2={y + 6} stroke="rgb(255 255 255 / 0.22)" strokeWidth={1} />
          ))}
          {isNum(existing) && isNum(optimized) && <line x1={x(existing)} x2={x(optimized)} y1={y} y2={y} stroke={NODE_MARK.optimized} strokeWidth={2} />}
          {isNum(stat) && <line x1={x(stat)} x2={x(stat)} y1={y - 7} y2={y + 7} stroke={NODE_MARK.static} strokeWidth={2} />}
          {isNum(existing) && <circle cx={x(existing)} cy={y} r={5} fill="#1f2937" stroke={NODE_MARK.existing} strokeWidth={2} />}
          {isNum(optimized) && <circle cx={x(optimized)} cy={y} r={5} fill={NODE_MARK.optimized} stroke="#1f2937" strokeWidth={1.5} />}
        </svg>
      )}
    </div>
  );
}
