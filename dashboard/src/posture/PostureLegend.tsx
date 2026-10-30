// The posture legend and the trade freshness line, shared by the Country, Sector and AIB headline charts
// (spec XX): the three backgrounds, the Secure, Compete and Deploy marks, the not-assessed hatch, the
// policy-alignment borders and the evidence fade, each in words.

import type { PostureCore } from './contract.ts';
import { freshnessLine } from './derive.ts';
import {
  ALIGNMENT_BORDER, ALIGNMENT_CLASS_LABEL, ALIGNMENT_ICON, DEPLOY_MARK, DEPLOY_MODE_LABEL, EVIDENCE_LEVELS, evidenceOpacity, MODE_LABEL, MODE_MARK, NOT_ASSESSED_BORDER, NOT_ASSESSED_HATCH,
  POSTURE_COLOR, POSTURE_LABEL, POSTURE_ORDER, RATIONALE_MARK, RATIONALE_ORDER, RATIONALE_SHORT,
} from './grammar.ts';

export function Swatch({ color, mark, size = 18, hatched = false }: { color?: string; mark?: string; size?: number; hatched?: boolean }) {
  return (
    <span
      aria-hidden="true"
      className="inline-flex shrink-0 items-center justify-center rounded-[3px] text-[11px] font-semibold text-white"
      style={{ width: size, height: size, background: hatched ? NOT_ASSESSED_HATCH : color, border: hatched ? NOT_ASSESSED_BORDER : undefined }}
    >
      {mark}
    </span>
  );
}

/** The three backgrounds and every mark, then (optionally) the alignment borders and the evidence fade, in words. */
export function PostureLegend({ alignment = true, compact = false, notAssessed = true }: { alignment?: boolean; compact?: boolean; notAssessed?: boolean }) {
  return (
    <div className="space-y-2 text-xs text-paper-dim" aria-label="Legend" data-testid="posture-legend">
      <div className="flex flex-wrap items-center gap-x-4 gap-y-2">
        {POSTURE_ORDER.map((p) => (
          <span key={p} className="inline-flex items-center gap-1.5">
            <Swatch color={POSTURE_COLOR[p]} />
            {POSTURE_LABEL[p]}
          </span>
        ))}
        {notAssessed && (
          <span className="inline-flex items-center gap-1.5">
            <Swatch hatched />
            Not assessed
          </span>
        )}
      </div>
      <div className="flex flex-wrap items-center gap-x-4 gap-y-2">
        {RATIONALE_ORDER.map((r) => (
          <span key={r} className="inline-flex items-center gap-1.5">
            <Swatch color={POSTURE_COLOR.S} mark={RATIONALE_MARK[r]} />
            {RATIONALE_SHORT[r][0].toUpperCase() + RATIONALE_SHORT[r].slice(1)}
          </span>
        ))}
        {(['preserve', 'catch_up', 'partner'] as const).map((m) => (
          <span key={m} className="inline-flex items-center gap-1.5">
            <Swatch color={POSTURE_COLOR.C} mark={MODE_MARK[m]} />
            {MODE_LABEL[m]}
          </span>
        ))}
        {(['accelerate', 'watch'] as const).map((m) => (
          <span key={m} className="inline-flex items-center gap-1.5">
            <Swatch color={POSTURE_COLOR.D} mark={DEPLOY_MARK[m]} />
            {DEPLOY_MODE_LABEL[m]}
          </span>
        ))}
      </div>
      {alignment && !compact && (
        <div className="flex flex-wrap items-center gap-x-4 gap-y-2">
          {(['aligned', 'under_prioritized', 'over_prioritized', 'misaligned', 'mixed'] as const).map((a) => (
            <span key={a} className="inline-flex items-center gap-1.5">
              <span aria-hidden="true" className="inline-block h-4 w-4 rounded-[3px]" style={{ border: ALIGNMENT_BORDER[a] }} />
              {ALIGNMENT_CLASS_LABEL[a]} policy {ALIGNMENT_ICON[a]}
              {a === 'under_prioritized' ? ' (incl. under-response, deployment)' : ''}
            </span>
          ))}
          <span className="inline-flex items-center gap-1.5" data-testid="legend-evidence">
            {EVIDENCE_LEVELS.map((e) => (
              <span key={e} aria-hidden="true" className="inline-block h-4 w-4 rounded-[3px] bg-paper-mute" style={{ opacity: evidenceOpacity(e) }} />
            ))}
            Evidence: strong, moderate, weak (fading)
          </span>
        </div>
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
