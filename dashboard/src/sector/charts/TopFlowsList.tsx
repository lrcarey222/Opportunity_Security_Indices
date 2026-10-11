// TopFlowsList (spec_sector.md 4.3 S4b, 6): the phone replacement for the Sankey, a ranked list
// "China → United States · USD 21.4 bn · 10.5% of world trade", the exporter's group as a dot (and in
// words for screen readers), source flags as text.

import { groupColor, groupLabel } from './color.ts';
import type { TopFlowsListProps } from './types.ts';

export function TopFlowsList({ flows, ariaLabel, numbered = true }: TopFlowsListProps) {
  if (!flows.length) return <p className="py-6 text-center text-sm text-paper-mute">No flows for this selection.</p>;
  const List = numbered ? 'ol' : 'ul';
  return (
    <List aria-label={ariaLabel} className="divide-y divide-white/5 text-sm">
      {flows.map((f, i) => (
        <li key={f.id} className="flex min-h-11 items-start gap-3 py-2">
          {numbered && <span className="w-6 shrink-0 text-right text-xs text-paper-mute tabular">{i + 1}</span>}
          <span aria-hidden="true" className="mt-1.5 inline-block h-2.5 w-2.5 shrink-0 rounded-full" style={{ background: groupColor(f.fromGroup) }} />
          <span className="min-w-0 flex-1 text-paper-dim">
            <span className="text-paper">
              {f.from} {'→'} {f.to}
            </span>
            <span className="sr-only"> ({groupLabel(f.fromGroup)} exporter)</span>
            <span className="block text-xs text-paper-mute tabular">
              {f.value} · {f.share}
              {f.flag ? ` · ${f.flag}` : ''}
            </span>
          </span>
        </li>
      ))}
    </List>
  );
}
