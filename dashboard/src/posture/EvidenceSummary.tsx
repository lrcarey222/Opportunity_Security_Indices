// The compact evidence disclosure under a recommendation (Explorer usability pass, 2026-11-25; brief section 6):
// one visible "Evidence quality" line (posture/evidenceSummary.ts over the listed steps), then a collapsed
// "Why this conclusion?" with the 2-4 inputs and the data periods, and the links to the evidence. It never
// dominates the block: muted, small, below the answer.

import type { ReactNode } from 'react';

export interface EvidenceSummaryProps {
  /** "Evidence quality: ..." or null when nothing is listed */
  quality: string | null;
  /** the 2-4 inputs, one sentence each */
  inputs: readonly string[];
  /** the data periods ("Strategy built 21 November 2026", "trade benchmark 2022-2024", ...) */
  data: readonly string[];
  /** where the evidence is ("Explore the evidence ->" first) */
  links: readonly { label: string; href: string }[];
  /** an extra line (e.g. the deployment row) shown above the disclosure */
  children?: ReactNode;
  className?: string;
}

export function EvidenceSummary({ quality, inputs, data, links, children, className }: EvidenceSummaryProps) {
  return (
    <div className={`border-t border-white/10 pt-3 text-xs text-paper-mute ${className ?? ''}`} data-testid="evidence-summary">
      {children}
      {quality && (
        <p className="max-w-3xl" data-testid="evidence-quality">
          {quality}
        </p>
      )}
      <details className="mt-1" data-testid="why-conclusion">
        <summary className="cursor-pointer select-none">Why this conclusion?</summary>
        <ul className="mt-1 max-w-3xl list-disc space-y-0.5 pl-5 text-paper-dim">
          {inputs.map((t) => (
            <li key={t}>{t}</li>
          ))}
        </ul>
        {data.length > 0 && (
          <p className="mt-1 max-w-3xl" data-testid="evidence-data">
            Data: {data.join(' · ')}.
          </p>
        )}
      </details>
      {links.length > 0 && (
        <p className="mt-1 flex flex-wrap gap-x-4 gap-y-1">
          {links.map((l) => (
            <a key={l.href} className="link" href={l.href}>
              {l.label} &rarr;
            </a>
          ))}
        </p>
      )}
    </div>
  );
}
