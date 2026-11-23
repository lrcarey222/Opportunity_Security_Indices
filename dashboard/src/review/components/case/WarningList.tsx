// Data-quality warnings, shown where the evidence is (never in an appendix): a case's warnings under its brief
// and a fact's own warnings under the fact. "warn" warnings are marked as such in words, not by colour alone.

import type { BriefWarning } from '../../model.ts';

export function WarningList({ warnings, label = 'Data warnings', compact = false, testid = 'brief-warnings' }: { warnings: readonly BriefWarning[] | null | undefined; label?: string; compact?: boolean; testid?: string }) {
  const ws = (warnings ?? []).filter((w) => w && (w.title || w.text));
  if (!ws.length) return null;
  return (
    <ul className={compact ? 'mt-1 space-y-0.5' : 'space-y-1.5'} aria-label={label} data-testid={testid}>
      {ws.map((w, i) => (
        <li
          key={`${w.id}-${i}`}
          data-warning={w.id}
          data-level={w.level}
          className={compact ? 'text-xs text-paper-mute' : 'border-l-2 py-1 pl-3 text-sm text-paper-dim'}
          style={compact ? undefined : { borderLeftColor: w.level === 'warn' ? 'var(--color-signal)' : 'rgb(255 255 255 / 0.2)' }}
        >
          {w.level === 'warn' && <span className="mr-1 text-signal">Warning:</span>}
          <span className="font-medium text-paper">{w.title}</span>
          {w.text ? <span>{compact ? ': ' : '. '}{w.text}</span> : null}
        </li>
      ))}
    </ul>
  );
}

export default WarningList;
