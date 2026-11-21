// The neutral evidence of a case brief, kept visibly apart from the analysis's own choices (docs/QC_UX_REVIEW.md
// sections 4-5): "Evidence" lists observed or externally sourced facts, each with its value, a plain reading by
// generic conventions, its source and its vintage, and its own warnings inline; "Analytical assumptions" lists
// the modelling choices behind this kind of conclusion, each linked to its method card. A reviewer can then say
// whether they dispute a fact or an assumption. A model estimate shown as evidence is labelled "Estimate".
// A missing value reads "not available in this release", never a blank or a dash.

import { Link } from 'react-router';
import type { BriefAssumption, BriefDefinition, BriefFact, CaseFact } from '../../model.ts';
import { WarningList } from './WarningList.tsx';

/** Shown for a fact whose value the release does not carry. */
export const NOT_IN_RELEASE = 'not available in this release';

const BLANK = new Set(['', '—', '-', 'NA', 'null', 'undefined']);

/** A brief fact's shown value: its display, or NOT_IN_RELEASE when the release has no value. */
export function briefFactValue(f: Pick<BriefFact, 'value' | 'display'> | null | undefined): string {
  if (!f) return NOT_IN_RELEASE;
  const d = typeof f.display === 'string' ? f.display.trim() : '';
  if (d && !BLANK.has(d)) return d;
  if (f.value === null || f.value === undefined || (typeof f.value === 'number' && !Number.isFinite(f.value))) return NOT_IN_RELEASE;
  return String(f.value);
}

/** A case fact's shown value (NOT_IN_RELEASE when the release has no value). */
export function factValue(f: CaseFact | undefined): string {
  if (!f || f.value === null || f.value === undefined || (typeof f.value === 'number' && !Number.isFinite(f.value))) return NOT_IN_RELEASE;
  return f.display && f.display.trim() ? f.display : String(f.value);
}

/** "Source · vintage" of a fact (null when neither is known). */
export function sourceLine(f: Pick<BriefFact, 'source' | 'vintage'>): string | null {
  const parts = [f.source, f.vintage].filter((x): x is string => typeof x === 'string' && x.trim() !== '');
  return parts.length ? parts.join(' · ') : null;
}

export function FactRow({ f }: { f: BriefFact }) {
  const v = briefFactValue(f);
  const missing = v === NOT_IN_RELEASE;
  const src = sourceLine(f);
  return (
    <div className="grid gap-1 py-2.5 sm:grid-cols-[minmax(0,2fr)_minmax(0,3fr)] sm:gap-4" data-fact={f.id}>
      <dt className="text-sm text-paper-dim">
        {f.label}
        {f.estimate && (
          <span className="tag ml-1.5 text-[0.6875rem]" title="A model estimate, not an observation">
            Estimate
          </span>
        )}
      </dt>
      <dd className="min-w-0 text-sm">
        <span className={missing ? 'italic text-paper-mute' : 'text-paper tabular'} data-testid="fact-value">
          {v}
        </span>
        {f.interpretation && <span className="mt-0.5 block text-xs text-paper-dim">Reading: {f.interpretation}</span>}
        <span className="mt-0.5 block text-xs text-paper-mute" data-testid="fact-source">
          {src ? `Source: ${src}` : 'Source: not stated in this release'}
        </span>
        <WarningList warnings={f.warnings} compact label={`Warnings on ${f.label}`} testid="fact-warnings" />
      </dd>
    </div>
  );
}

export interface FactGroup {
  title: string | null;
  facts: BriefFact[];
}

export function EvidencePanel({ groups, title = 'Evidence', lede, testid = 'brief-evidence' }: { groups: readonly FactGroup[]; title?: string; lede?: string; testid?: string }) {
  const any = groups.some((g) => g.facts.length);
  return (
    <section className="panel px-4 py-3" aria-labelledby={`${testid}-h`} data-testid={testid}>
      <h4 id={`${testid}-h`} className="text-sm font-medium text-paper">
        {title}
      </h4>
      <p className="mt-0.5 text-xs text-paper-mute">{lede ?? 'Observed or externally sourced facts. If one of these is wrong, that is a data problem.'}</p>
      {!any && <p className="mt-2 text-sm italic text-paper-mute">No evidence for this case is {NOT_IN_RELEASE}.</p>}
      {groups
        .filter((g) => g.facts.length)
        .map((g, i) => (
          <div key={g.title ?? i} className="mt-2">
            {g.title && <div className="mt-1 text-xs font-medium uppercase tracking-wider text-paper-mute">{g.title}</div>}
            <dl className="divide-y divide-white/10">
              {g.facts.map((f) => (
                <FactRow key={f.id} f={f} />
              ))}
            </dl>
          </div>
        ))}
    </section>
  );
}

/** The method-card page of the review, opened on one card. */
export const methodCardHref = (id: string, campaign?: string | null): string => `/review/methods?${new URLSearchParams({ ...(campaign ? { campaign } : {}), card: id })}`;

/** The assumptions shown at once (the rest fold, to keep the brief to a minute or so of reading). */
export const SHOWN_ASSUMPTIONS = 3;

function AssumptionList({ items, campaign }: { items: readonly BriefAssumption[]; campaign?: string | null }) {
  return (
    <ul className="mt-2 space-y-2">
      {items.map((a) => (
        <li key={a.id} data-assumption={a.id} className="text-sm">
          <span className="font-medium text-paper">{a.label}.</span> <span className="text-paper-dim">{a.text}</span>
          {a.method_card_id && (
            <>
              {' '}
              <Link className="link text-xs" to={methodCardHref(a.method_card_id, campaign)} target="_blank" rel="noopener noreferrer">
                Read the method card
              </Link>
            </>
          )}
        </li>
      ))}
    </ul>
  );
}

export function AssumptionsPanel({ assumptions, campaign, testid = 'brief-assumptions' }: { assumptions: readonly BriefAssumption[]; campaign?: string | null; testid?: string }) {
  if (!assumptions.length) return null;
  return (
    // dashed edge: a different kind of thing from the evidence panel (choices, not observations)
    <section className="panel px-4 py-3" style={{ borderStyle: 'dashed' }} aria-labelledby={`${testid}-h`} data-testid={testid}>
      <h4 id={`${testid}-h`} className="text-sm font-medium text-paper">
        Analytical assumptions
      </h4>
      <p className="mt-0.5 text-xs text-paper-mute">Choices the analysis makes, not observations. If one of these is wrong, that is a method problem.</p>
      <AssumptionList items={assumptions.slice(0, SHOWN_ASSUMPTIONS)} campaign={campaign} />
      {assumptions.length > SHOWN_ASSUMPTIONS && (
        <details className="disclosure mt-2" data-testid="more-assumptions">
          <summary className="cursor-pointer py-1 text-xs font-medium text-paper-dim hover:text-paper">
            {assumptions.length - SHOWN_ASSUMPTIONS} more {assumptions.length - SHOWN_ASSUMPTIONS === 1 ? 'assumption' : 'assumptions'}
          </summary>
          <AssumptionList items={assumptions.slice(SHOWN_ASSUMPTIONS)} campaign={campaign} />
        </details>
      )}
    </section>
  );
}

export function MissingEvidence({ items }: { items: readonly string[] }) {
  if (!items.length) return null;
  return (
    <section aria-labelledby="brief-missing-h" data-testid="brief-missing">
      <h4 id="brief-missing-h" className="text-sm font-medium text-paper">
        What the data cannot show
      </h4>
      <ul className="mt-1 list-disc space-y-0.5 pl-5 text-sm text-paper-dim">
        {items.map((m) => (
          <li key={m}>{m}</li>
        ))}
      </ul>
    </section>
  );
}

export function Definitions({ items }: { items: readonly BriefDefinition[] }) {
  if (!items.length) return null;
  return (
    <section aria-labelledby="brief-defs-h" data-testid="brief-definitions">
      <h4 id="brief-defs-h" className="text-sm font-medium text-paper">
        Definitions
      </h4>
      <dl className="mt-1 grid gap-x-4 gap-y-1.5 text-sm sm:grid-cols-[minmax(0,1fr)_minmax(0,3fr)]">
        {items.map((d) => (
          <div key={d.term} className="contents">
            <dt className="text-paper">{d.term}</dt>
            <dd className="text-paper-dim">{d.text}</dd>
          </div>
        ))}
      </dl>
    </section>
  );
}

export default EvidencePanel;
