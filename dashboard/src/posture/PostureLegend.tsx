// The posture legend and the trade freshness line, shared by the Country, Sector and AIB headline charts.

import type { PostureCore } from './contract.ts';
import { freshnessLine } from './derive.ts';
import { ALIGNMENT_BORDER, ALIGNMENT_ICON, ALIGNMENT_LABEL, MODE_LABEL, MODE_MARK, POSTURE_COLOR, POSTURE_LABEL, POSTURE_ORDER } from './grammar.ts';

export function Swatch({ color, mark, size = 18 }: { color: string; mark?: string; size?: number }) {
  return (
    <span
      aria-hidden="true"
      className="inline-flex shrink-0 items-center justify-center rounded-[3px] text-[11px] font-semibold text-white"
      style={{ width: size, height: size, background: color }}
    >
      {mark}
    </span>
  );
}

/** Posture colours, Compete marks and (optionally) the alignment borders, in words. */
export function PostureLegend({ alignment = true, compact = false }: { alignment?: boolean; compact?: boolean }) {
  return (
    <div className="flex flex-wrap items-center gap-x-4 gap-y-2 text-xs text-paper-dim" aria-label="Legend">
      {POSTURE_ORDER.map((p) => (
        <span key={p} className="inline-flex items-center gap-1.5">
          <Swatch color={POSTURE_COLOR[p]} />
          {POSTURE_LABEL[p]}
        </span>
      ))}
      {(['preserve', 'catch_up', 'partner'] as const).map((m) => (
        <span key={m} className="inline-flex items-center gap-1.5">
          <Swatch color={POSTURE_COLOR.C} mark={MODE_MARK[m]} />
          {MODE_LABEL[m]}
        </span>
      ))}
      {alignment && !compact && (
        <>
          {(['under_prioritized', 'over_prioritized', 'mixed'] as const).map((a) => (
            <span key={a} className="inline-flex items-center gap-1.5">
              <span aria-hidden="true" className="inline-block h-4 w-4 rounded-[3px]" style={{ border: ALIGNMENT_BORDER[a] }} />
              {ALIGNMENT_LABEL[a]} policy {ALIGNMENT_ICON[a]}
            </span>
          ))}
          <span className="inline-flex items-center gap-1.5">
            <span aria-hidden="true" className="inline-block h-4 w-4 rounded-[3px] bg-paper-mute opacity-50" />
            Faded: weak evidence
          </span>
          <span className="inline-flex items-center gap-1.5">↑ → ↓ trade momentum (corner)</span>
        </>
      )}
    </div>
  );
}

/** "Structural trade benchmark: 2022–24 · Current signal through: Aug 2026" (or why there is none). */
export function TradeFreshness({ meta }: { meta: PostureCore['meta'] }) {
  const f = freshnessLine(meta);
  return (
    <p className="text-xs text-paper-mute" data-testid="trade-freshness">
      {f.benchmark} · {f.signal}
    </p>
  );
}
