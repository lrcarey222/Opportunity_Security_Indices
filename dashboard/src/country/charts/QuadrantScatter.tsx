// QuadrantScatter (spec_country.md A4.2 S2; a Legacy OSI chart since 2026-10-29, the corners keyed legacy_*):
// one country's placed sectors at their percentiles of Economic
// Opportunity (across) and Energy Security (up), 0-100 on both axes (the share of other economies the
// country beats in each sector), dashed crosshairs at the country's own median sector on each axis
// (summary.country_median; the user's rule of 2026-10-15), so the corners sort the country's relatively
// strong and weak sectors; the four named corners in the chart's margins (above and below the plot area,
// so no dot ever hides them; sectors crowd the corners), shape = stage (one of six, design S4: never a
// colour), filled = among the country's top five Strategic Index sectors. No colour encoding (position
// carries the corner). The selected sector gets the orange ring and a bold white label. Labels (six stages,
// 2026-10-29: up to 58 dots): the selected sector always; then the high-priority and top-five sectors, then
// the rest, each only where it fits without covering an earlier label (below 640 px the rest go unlabelled);
// with more than 33 dots the dots are a little smaller. Every sector is in the table twin. Since 2026-10-30 its
// look lives in charts/quadScatter.tsx (quadScatterOptions, the corner rows, the legend shapes), shared with
// the Country page's Prize x Position scatter.

import { useCallback } from 'react';
import type * as Plot from '@observablehq/plot';
import { PlotFigure, Tip } from '../../charts/PlotFigure.tsx';
import { DashedRuleSwatch, QUAD_MARK, QuadCorner, QuadCornerRow, quadScatterOptions, scatterHeight, scatterRadius, STAGE_SYMBOL, StageSymbol } from '../../charts/quadScatter.tsx';
import { useWidth } from './useWidth.ts';
import { SIGNAL } from '../../theme/palette.ts';
import { SUPPLY_STAGES, STAGE_LABEL } from '../../lib/stages.ts';
import type { FactSheet } from '../contract.ts';
import { LEGACY_CORNERS } from '../contract.ts';
import type { ScatterPoint } from '../derive/model.ts';
import { legacyCornerWord, proseName, poss, cap } from '../derive/text.ts';

const MARK = QUAD_MARK;
// the shared core's pieces, under the names this module has always exported
export { scatterHeight, scatterRadius, STAGE_SYMBOL };

// x = Economic Opportunity, y = Energy Security: Grow top right, Stable top left, Exposed bottom left,
// Upgrade bottom right (the Legacy OSI corners). The headings print the LEGACY_CORNERS labels.
const CORNERS = [
  { id: 'legacy_grow', x: 100, y: 100, anchor: 'end', top: true },
  { id: 'legacy_stable', x: 0, y: 100, anchor: 'start', top: true },
  { id: 'legacy_exposed', x: 0, y: 0, anchor: 'start', top: false },
  { id: 'legacy_upgrade', x: 100, y: 0, anchor: 'end', top: false },
] as const;

export interface QuadrantScatterProps {
  sheet: FactSheet;
  points: ScatterPoint[];
  selected: string | null;
  onSelect: (key: string) => void;
  ariaLabel: string;
}

export function QuadrantScatter({ sheet, points, selected, onSelect, ariaLabel }: QuadrantScatterProps) {
  const name = proseName(sheet.meta.country);
  const cm = sheet.summary.country_median;
  const medEo = cm?.eo.v ?? null;
  const medEs = cm?.es.v ?? null;
  const build = useCallback(
    (width: number): Plot.PlotOptions | null =>
      quadScatterOptions({
        width,
        height: scatterHeight(width),
        points,
        selected,
        priority: (p) => p.priority || p.top5,
        xLabel: `Economic Opportunity: share of economies ${name} beats (%) →`,
        yLabel: `↑ Energy Security: share of economies ${name} beats (%)`,
        // the country's median sector on each axis: the corners split here
        ruleX: medEo,
        ruleY: medEs,
        fill: (d) => (d.top5 ? MARK : 'none'),
        stroke: () => MARK,
      }),
    [points, selected, name, medEo, medEs],
  );
  const [ref, w] = useWidth<HTMLDivElement>();
  const height = scatterHeight(w || 440);
  const tip = (p: ScatterPoint) => {
    const c = p.cell;
    const es = c.es!;
    const eo = c.eo!;
    const few = (eo.n.v ?? 99) < 10 ? ` (only ${eo.n.d} economies)` : '';
    const vs = (high: boolean) => (high ? 'at or above' : 'below');
    return (
      <Tip
        value={legacyCornerWord(p.corner)}
        title={c.label}
        lines={[
          `${cap(name)}: Economic Opportunity ${vs(p.x >= (medEo ?? 0))} its median sector (${cm?.eo.d ?? '–'}), Energy Security ${vs(p.y >= (medEs ?? 0))} it (${cm?.es.d ?? '–'})`,
          `Economic Opportunity ${eo.d}: higher than ${eo.pct.d} of the other ${eo.n_others.d} economies${few} (median economy ${eo.med.d}; ${eo.side === 'above' ? 'above' : 'below'} it)`,
          `Energy Security ${es.d}: higher than ${es.pct.d} of them (median economy ${es.med.d}; ${es.side === 'above' ? 'above' : 'below'} it)`,
          c.si.in_sample ? `Strategic Index rank ${c.si.rank.d} of ${c.si.n.d}` : `Not in the Strategic Index${c.si.exclusion ? `: ${c.si.exclusion}` : ''}`,
        ]}
      />
    );
  };
  const corner = (id: (typeof CORNERS)[number]['id'], right: boolean) => {
    const q = LEGACY_CORNERS.find((x) => x.id === id)!;
    return (
      <QuadCorner title={legacyCornerWord(id)} right={right}>
        {q.meaning} <span className="whitespace-nowrap">({q.posture})</span>
      </QuadCorner>
    );
  };
  return (
    <div ref={ref}>
      <QuadCornerRow left={corner('legacy_stable', false)} right={corner('legacy_grow', true)} />
      <PlotFigure<ScatterPoint> build={build} height={height} ariaLabel={ariaLabel} tooltip={tip} onSelect={(p) => onSelect(p.key)} />
      <QuadCornerRow below left={corner('legacy_exposed', false)} right={corner('legacy_upgrade', true)} />
    </div>
  );
}

function Sym({ shape, filled, stroke = MARK }: { shape: (typeof STAGE_SYMBOL)[keyof typeof STAGE_SYMBOL]; filled?: boolean; stroke?: string }) {
  return <StageSymbol shape={shape} fill={filled ? MARK : 'none'} stroke={stroke} />;
}

/** The scatter's legend: the six stage shapes, filled vs hollow, the selection ring and the country's
 *  median-sector lines, in words. */
export function ScatterLegend({ name }: { name: string }) {
  const items: [React.ReactNode, string][] = [
    ...SUPPLY_STAGES.map((s): [React.ReactNode, string] => [<Sym key={s} shape={STAGE_SYMBOL[s]} />, STAGE_LABEL[s]]),
    [<Sym key="f" shape="circle" filled />, `Filled: among ${poss(name)} top five Strategic Index sectors`],
    [<Sym key="s" shape="circle" stroke={SIGNAL} />, 'Selected'],
    [<DashedRuleSwatch key="l" />, `Dashed lines: ${poss(name)} median sector`],
  ];
  return (
    <ul className="flex flex-wrap gap-x-4 gap-y-1.5 text-xs text-paper-dim" aria-label="How to read the dots">
      {items.map(([k, t]) => (
        <li key={t} className="inline-flex items-center gap-1.5">
          {k}
          {t}
        </li>
      ))}
    </ul>
  );
}
