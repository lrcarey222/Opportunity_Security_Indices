// Beeswarm (spec_sector.md 4.4 card 2, 4.6 "Distribution", 5.3): every economy as a dot dodged along y;
// group fill, "rest of world" as hollow gray rings; old observations at 50% opacity; the highlighted
// economy ringed orange and labelled, plus the points the view asks to label (China, the largest
// coalition member); median ticks with their words. Reported zeros are listed in a separate "0" strip
// under the swarm (they have no place on a log axis and would hide the origin).
//
// The layout is the generic one (charts/Beeswarm.tsx, spec_country.md C2): the height follows the
// stack, dots shrink when many coincide and, past 480 px, columns cap their stacks with a "+m" count,
// so no dot ever leaves the card (user edit 1b). Props are unchanged.

import { useMemo } from 'react';
import { BeeswarmChart, type SwarmPoint } from '../../charts/Beeswarm.tsx';
import { Tip } from '../../charts/PlotFigure.tsx';
import { GROUP_COLOR, INK } from '../../theme/palette.ts';
import { groupLabel } from './color.ts';
import type { BeeswarmPoint, BeeswarmProps } from './types.ts';

type GroupSwarmPoint = BeeswarmPoint & SwarmPoint;

/** The sector styling of a point: group fill, hollow gray "rest", old observations at half opacity. */
export function groupSwarmPoint(p: BeeswarmPoint): GroupSwarmPoint {
  const rest = p.group === 'rest';
  return {
    ...p,
    fill: rest ? 'none' : GROUP_COLOR[p.group],
    stroke: rest ? GROUP_COLOR.rest : INK[800],
    strokeWidth: rest ? 1.5 : 1,
    opacity: p.state === 'old' ? 0.5 : 1,
  };
}

export function Beeswarm({ points, medians, log, xLabel, ariaLabel, selected, onSelect, tickFormat, zeroStrip = true }: BeeswarmProps) {
  const { swarm, zeros } = useMemo(() => {
    // zeros go to the strip for additive measures (and always on a log axis); a bounded 0 is a dot
    const isZero = (p: BeeswarmPoint) => p.state === 'zero' || (Number.isFinite(p.value) && p.value === 0 && (zeroStrip || !!log));
    const zeros = points.filter(isZero);
    const swarm = points.filter((p) => Number.isFinite(p.value) && !isZero(p) && (!log || p.value > 0)).map(groupSwarmPoint);
    return { swarm, zeros };
  }, [points, log, zeroStrip]);
  return (
    <div>
      <BeeswarmChart<GroupSwarmPoint>
        points={swarm}
        ariaLabel={ariaLabel}
        xLabel={xLabel}
        log={log}
        tickFormat={tickFormat}
        medians={medians}
        selected={selected}
        onSelect={onSelect}
        minHeight={140}
        tooltip={(p) => <Tip value={p.display} title={p.label} lines={[groupLabel(p.group), p.state === 'old' ? 'older observation' : null]} />}
      />
      {zeros.length > 0 && (
        <p className="mt-2 border-t hairline pt-2 text-xs text-paper-mute" data-testid="zero-strip">
          <span className="mr-2 inline-flex items-center gap-1 font-medium text-paper-dim">
            <span aria-hidden="true" className="inline-block h-2.5 w-2.5" style={{ background: '#4b5563' }} />0
          </span>
          Reported zero ({zeros.length}): {zeros.map((z) => z.label).join(', ')}
        </p>
      )}
    </div>
  );
}
