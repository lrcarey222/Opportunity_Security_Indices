// LeagueBars (spec_sector.md 4.3 S3, S4a, 5.3): a ranking as horizontal bars. Plot barX, y = economy
// in the given rank order, x = value in its unit, fill = the economy's group colour; the highlighted
// economy gets a 2 px orange stroke and a bold label; end labels ("USD 88.7 bn · 43.7%") when they
// fit; the top N rows, then a gap row "…" and the pinned rows (the highlight). Bars have 4 px rounded
// data ends. Rows without a finite value (withheld, not covered) show their text and no bar.
// `diverging` draws a signed measure around zero in the diverging palette with the poles in words.
// `log` (Explore's Scale = Log): bars start at the decade below the smallest value (a log axis has no
// zero, and Plot's implicit x1 = 0 maps to -infinity, which drew no bars at all); ticks and grid lines
// at the decades only, each labelled with its own SI step.

import { useCallback, useMemo } from 'react';
import * as Plot from '@observablehq/plot';
import { PlotFigure, Tip } from '../../charts/PlotFigure.tsx';
import { compactSi, fitLabel, labelMargin, logTicks, textWidth } from '../../charts/theme.ts';
import { DIV_NEG, DIV_POS, PAPER, PAPER_DIM, PAPER_MUTE, SIGNAL, ZERO_RULE } from '../../theme/palette.ts';
import { groupColor, groupLabel } from './color.ts';
import type { LeagueBarsProps, LeagueRow } from './types.ts';

export const GAP_ID = '__gap__';
const ROW_PX = 22;
const AXIS_PX = 40;

interface Shown extends LeagueRow {
  gap?: boolean;
}

/** The rows drawn: the first `top`, then (after a gap) pinned or selected rows from further down. */
export function leagueRows(rows: LeagueRow[], top: number, selected?: string | null): Shown[] {
  const head = rows.slice(0, top);
  const extra = rows.slice(top).filter((r) => r.pinned || (selected && r.id === selected));
  if (!extra.length) return head;
  return [...head, { id: GAP_ID, label: '…', group: 'rest', value: NaN, display: '', gap: true }, ...extra];
}

export const STATE_WORDS: Record<string, string> = {
  zero: 'Reported zero',
  nsr: 'Not reported separately by the source',
  nq: 'Listed without a figure',
  mirror: "From partners' declarations (×1.06)",
  est: 'Includes an estimate',
  na_growth: 'Base year zero',
  not_covered: 'Not covered by the source',
  regional: 'A regional value',
  old: 'An old observation',
};

const rowLabel = (r: LeagueRow) => (r.badge ? `${r.label} (${r.badge})` : r.label);

export function LeagueBars({ rows, axisLabel, selected, onSelect, top = 20, diverging, ariaLabel, tickFormat, log }: LeagueBarsProps) {
  const shown = useMemo(() => leagueRows(rows, top, selected), [rows, top, selected]);
  const height = shown.length * ROW_PX + AXIS_PX;
  const build = useCallback(
    (width: number): Plot.PlotOptions => {
      const ids = shown.map((r) => r.id);
      const has = (r: LeagueRow) => Number.isFinite(r.value) && (!log || r.value > 0);
      const data = shown.filter((r) => !(r as Shown).gap && has(r));
      const blank = shown.filter((r) => !(r as Shown).gap && !has(r));
      const ml = labelMargin(shown.map(rowLabel), { max: Math.min(220, Math.floor(width * 0.38)) });
      const endW = Math.max(0, ...shown.map((r) => textWidth(r.display, 11))) + 8;
      // end labels only when they fit beside the bars without squeezing the plot below two thirds
      const showEnd = endW < width * 0.32;
      const mr = showEnd ? endW + 4 : 16;
      const xs = data.map((r) => r.value);
      const lim = Math.max(1e-12, ...xs.map(Math.abs));
      // the composed diverging label is long; on a phone it ran past the svg (page 3 px wider than 390):
      // keep only the direction words when it does not fit the plot (the card caption carries the unit)
      const divLabel = diverging
        ? textWidth(`${diverging.negLabel}   ${axisLabel}   ${diverging.posLabel}`, 11) <= width - ml - mr
          ? `${diverging.negLabel}   ${axisLabel}   ${diverging.posLabel}`
          : `${diverging.negLabel}   ${diverging.posLabel}`
        : '';
      // log: the bars' base is the decade at or below the smallest positive value
      const minPos = Math.min(...xs.filter((v) => v > 0));
      const maxPos = Math.max(...xs.filter((v) => v > 0));
      const base = log && Number.isFinite(minPos) ? Number(Math.pow(10, Math.floor(Math.log10(minPos))).toPrecision(12)) : 0;
      const logMax = log && Number.isFinite(maxPos) ? Math.max(maxPos, base * 10) : 1;
      const x: Plot.ScaleOptions = diverging
        ? { domain: [-lim, lim], label: divLabel, labelAnchor: 'center', grid: true, tickFormat, nice: true }
        : log
          ? { type: 'log', domain: [base, logMax], label: axisLabel, grid: true, ticks: logTicks(base, logMax, Math.max(3, Math.floor((width - ml - mr) / 90))), tickFormat: tickFormat ?? compactSi }
          : { type: 'linear', label: axisLabel, grid: true, tickFormat, nice: true, domain: [Math.min(0, ...xs), Math.max(0, ...xs) || 1] };
      // a bar from the base (log) or from zero (linear: Plot's implicit x1 = 0)
      const bar = log ? { x1: base, x2: 'value' } : { x: 'value' };
      const fill = (r: LeagueRow) => (diverging ? (r.value >= 0 ? DIV_POS[2] : DIV_NEG[1]) : groupColor(r.group));
      const sel = data.filter((r) => r.id === selected);
      const labelled = shown.filter((r) => r.id !== selected);
      const selLabel = shown.filter((r) => r.id === selected);
      const pos = data.filter((r) => r.value >= 0);
      const neg = data.filter((r) => r.value < 0);
      const end = { x: 'value', y: 'id', text: 'display', fill: PAPER_DIM, fontSize: 11 } as const;
      return {
        marginLeft: ml,
        marginRight: mr,
        marginTop: 8,
        x,
        y: { domain: ids, axis: null },
        marks: [
          // economy labels (the highlighted one bold and white), drawn as text so they can differ
          // cut to the margin (a long name such as "European Union (27) (EU's own measures)" ran off a
          // phone screen); the full name stays in the tooltip and the table twin
          Plot.text(labelled, { y: 'id', text: (r: LeagueRow) => fitLabel(rowLabel(r), ml - 10), frameAnchor: 'left', dx: -8, textAnchor: 'end', fill: PAPER_DIM, fontSize: 12 }),
          Plot.text(selLabel, { y: 'id', text: (r: LeagueRow) => fitLabel(rowLabel(r), ml - 10), frameAnchor: 'left', dx: -8, textAnchor: 'end', fill: PAPER, fontSize: 12, fontWeight: 700 }),
          Plot.barX(data, { ...bar, y: 'id', fill, insetTop: 4, insetBottom: 4, rx2: 4 }),
          sel.length ? Plot.barX(sel, { ...bar, y: 'id', fill: 'none', stroke: SIGNAL, strokeWidth: 2, insetTop: 3, insetBottom: 3, rx2: 4 }) : null,
          log ? null : Plot.ruleX([0], { stroke: ZERO_RULE }),
          showEnd ? Plot.text(pos, { ...end, dx: 5, textAnchor: 'start' }) : null,
          showEnd ? Plot.text(neg, { ...end, dx: -5, textAnchor: 'end' }) : null,
          // rows without a value: their words where the bar would start
          Plot.text(blank, { y: 'id', text: (r: LeagueRow) => r.display || (r.state ? STATE_WORDS[r.state] : ''), frameAnchor: 'left', dx: 5, textAnchor: 'start', fill: PAPER_MUTE, fontSize: 11 }),
          Plot.barX(data, Plot.pointerY({ ...bar, y: 'id', fill: 'none', stroke: SIGNAL, strokeWidth: 2, insetTop: 3, insetBottom: 3, rx2: 4 })),
        ],
      };
    },
    [shown, axisLabel, selected, diverging, tickFormat, log],
  );
  if (!shown.length) return <p className="py-6 text-center text-sm text-paper-mute">No economies to rank.</p>;
  return (
    <PlotFigure<LeagueRow>
      build={build}
      height={height}
      ariaLabel={ariaLabel}
      onSelect={onSelect}
      tooltip={(r) => (
        <Tip
          value={r.display}
          title={r.label}
          lines={[diverging ? (r.value >= 0 ? diverging.posLabel : diverging.negLabel) : groupLabel(r.group), r.badge ?? null, r.state && r.state !== 'obs' ? STATE_WORDS[r.state] : null]}
        />
      )}
    />
  );
}
