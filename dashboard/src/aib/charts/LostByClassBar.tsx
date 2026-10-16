// Where the lost supply sits: one horizontal bar of the shock's lost supply (USD) split by node class,
// segments in the class order with a 2 px surface gap, filled / hollow / hatched as the class marks.
// The shares are printed in the legend under it and in the table twin.

import { useWidth } from '../../charts/WorldMap.tsx';
import { HATCH_GROUND, HATCH_LINE } from '../../theme/palette.ts';
import { classInfo } from '../derive/classes.ts';
import type { ClassGroup } from '../derive/nodes.ts';

export function LostByClassBar({ groups, label }: { groups: ClassGroup[]; label: string }) {
  const [ref, w] = useWidth<HTMLDivElement>();
  const total = groups.reduce((s, g) => s + g.lostUsd, 0);
  const H = 22;
  const parts = groups.filter((g) => g.lostUsd > 0);
  let x0 = 0;
  const gap = 2;
  const usable = Math.max(0, w - gap * Math.max(0, parts.length - 1));
  return (
    <div ref={ref} className="w-full">
      {w > 0 && total > 0 && (
        <svg width={w} height={H} role="img" aria-label={label} className="block">
          <defs>
            <pattern id="aib-hatch" width="5" height="5" patternUnits="userSpaceOnUse" patternTransform="rotate(45)">
              <rect width="5" height="5" fill={HATCH_GROUND} />
              <line x1="0" y1="0" x2="0" y2="5" stroke={HATCH_LINE} strokeWidth="1" />
            </pattern>
          </defs>
          {parts.map((g) => {
            const width = (g.lostUsd / total) * usable;
            const info = classInfo(g.cls);
            const x = x0;
            x0 += width + gap;
            return info.mark === 'fill' ? (
              <rect key={g.cls} x={x} y={0} width={Math.max(1, width)} height={H} fill={info.color} />
            ) : info.mark === 'hollow' ? (
              <rect key={g.cls} x={x + 1} y={1} width={Math.max(1, width - 2)} height={H - 2} fill="none" stroke={info.color} strokeWidth={2} />
            ) : (
              <rect key={g.cls} x={x} y={0} width={Math.max(1, width)} height={H} fill="url(#aib-hatch)" />
            );
          })}
        </svg>
      )}
    </div>
  );
}
