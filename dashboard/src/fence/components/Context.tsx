// Context, not verdict: a quiet note of one to three measured facts beside a question, in plain
// sentences, with the full figures one click away. The framework asks the questions; data inform
// the reader's answer and never decide it.

import type { ReactNode } from 'react';
import type { YFEvidenceRecord } from '../data/contract.ts';
import { EvidenceMetric } from './kit.tsx';

/** The first sentence of a record's explanation (the rest is caveat). */
const lead = (r: YFEvidenceRecord) => (r.explanation ?? r.label).replace(/^(.*?[.:])\s.*$/s, '$1');

export function ContextNote({
  records, title = 'Context from the Opportunity & Security Indices', empty, children,
}: {
  records: (YFEvidenceRecord | undefined | null)[];
  title?: string;
  /** shown when no record applies */
  empty?: ReactNode;
  /** extra context lines (e.g. a curated assessment) */
  children?: ReactNode;
}) {
  const recs = records.filter((r): r is YFEvidenceRecord => !!r && (r.rawValue !== null || r.normalizedValue != null));
  if (!recs.length && !children && !empty) return null;
  return (
    <aside className="border-l-2 border-ink-600 py-1 pl-4" aria-label={title}>
      <div className="eyebrow mb-1.5">{title}</div>
      {children}
      {recs.length > 0 ? (
        <ul className="space-y-1 text-sm leading-relaxed text-paper-dim">
          {recs.map((r) => (
            <li key={`${r.scopeId}-${r.metricId}-${r.measure}`} title={`${r.source ?? ''} · ${r.year ?? ''}`}>
              {lead(r)}
              {r.scope === 'stage' && <span className="text-paper-mute"> (whole stage)</span>}
            </li>
          ))}
        </ul>
      ) : (
        !children && <p className="text-sm text-paper-mute">{empty}</p>
      )}
      {recs.length > 0 && (
        <details className="disclosure mt-1.5">
          <summary className="cursor-pointer text-xs text-paper-mute hover:text-paper">Show the figures and sources</summary>
          <div className="mt-1">
            {recs.map((r) => (
              <EvidenceMetric key={`m-${r.scopeId}-${r.metricId}-${r.measure}`} r={r} compact />
            ))}
          </div>
        </details>
      )}
    </aside>
  );
}
