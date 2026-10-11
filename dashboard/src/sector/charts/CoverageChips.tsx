// CoverageChips (spec_sector.md 4.3 S0, 3.3): one tag per family, icon plus text (never colour
// alone): "✓ Trade · Comtrade 2024", "◐ Capacity · IEA, 6 entities", "○ Technology · global TRL",
// "Licensed · BNEF LCOE", "— Reserves · not applicable". Chips with data link to their section; the
// others open a small popover (a native <details>) with the reason.

import type { CoverageChipsProps, CoverageStatus } from './types.ts';

const ICON: Record<CoverageStatus, string> = { country: '✓', region: '◐', global: '○', licensed: '', none: '—' };
const WORDS: Record<CoverageStatus, string> = {
  country: 'country data',
  region: 'regional or entity data only',
  global: 'global data only',
  licensed: 'licensed: not in this build',
  none: 'no data',
};

export function CoverageChips({ chips, ariaLabel = 'Coverage by family' }: CoverageChipsProps) {
  return (
    <ul className="flex flex-wrap gap-2" aria-label={ariaLabel}>
      {chips.map((c) => {
        // one inline run after the icon, so a long source list wraps inside the chip on a phone
        // (.tag is nowrap) instead of widening the page
        const text = (
          <>
            {ICON[c.status] && (
              <span aria-hidden="true" className="w-3 shrink-0 text-center">
                {ICON[c.status]}
              </span>
            )}
            <span className="min-w-0">
              {c.status === 'licensed' && <span className="text-paper-mute">Licensed · </span>}
              <span className={c.status === 'country' ? 'text-paper' : undefined}>{c.label}</span>{' '}
              <span className="text-paper-mute">· {c.text}</span>
              <span className="sr-only"> ({WORDS[c.status]})</span>
            </span>
          </>
        );
        // (`!`: .tag is unlayered CSS in globals.css, which outranks Tailwind's layered utilities)
        const chip = 'tag min-h-8 max-w-full gap-1.5! whitespace-normal! py-1! text-left';
        return (
          <li key={c.family} data-status={c.status} className="max-w-full">
            {c.href ? (
              <a href={c.href} className={`${chip} hover:border-signal hover:text-paper`}>
                {text}
              </a>
            ) : (
              <details className="relative">
                <summary className={`${chip} cursor-pointer list-none [&::-webkit-details-marker]:hidden`}>{text}</summary>
                <div className="panel absolute left-0 z-30 mt-1 w-64 max-w-[calc(100vw-2rem)] p-3 text-xs text-paper-dim" role="note">
                  {c.reason ?? WORDS[c.status]}
                </div>
              </details>
            )}
          </li>
        );
      })}
    </ul>
  );
}
