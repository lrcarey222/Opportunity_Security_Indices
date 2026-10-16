// The mini coverage strip of a compact card: the four coverage steps (shock -> existing -> trade ->
// trade + investment) as thin horizontal bars on one 0-100% scale, each with its value in text. HTML,
// so it reads as a list; the shock step is gray (the starting point), the responses blue.

import { isNum } from '../../lib/format.ts';
import type { CoverageStep } from '../derive/coverage.ts';
import { fmtShare } from '../derive/format.ts';

export const STEP_COLOR = { static: '#6b7280', response: '#1889b0' } as const;

export function CoverageStrip({ steps, label }: { steps: CoverageStep[]; label: string }) {
  return (
    <ul className="space-y-1" aria-label={label}>
      {steps.map((s) => {
        const w = isNum(s.value) ? Math.max(0, Math.min(1, s.value)) * 100 : 0;
        return (
          <li key={s.key} className="grid grid-cols-[5.5rem_minmax(0,1fr)_3rem] items-center gap-2 text-xs">
            <span className="truncate text-paper-mute" title={s.label}>
              {s.short}
            </span>
            <span className="relative block h-2 bg-ink-700/70" aria-hidden="true">
              {w > 0 && <span className="absolute inset-y-0 left-0" style={{ width: `${w}%`, background: s.key === 'static' ? STEP_COLOR.static : STEP_COLOR.response }} />}
            </span>
            <span className="text-right tabular text-paper-dim">
              {fmtShare(s.value, 0)}
              <span className="sr-only"> {s.label.toLowerCase()}</span>
            </span>
          </li>
        );
      })}
    </ul>
  );
}
