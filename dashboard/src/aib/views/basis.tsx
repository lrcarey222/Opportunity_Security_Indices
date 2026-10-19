// Small pieces of the Allied Industrial Base composition pages (IA refactor, 2026-10-16): the eyebrow that
// says whether a block is an observed / modelled fact or a preference-adjusted priority (plan.md 4: the
// AIB physical analysis never responds to the Perspective; partnership rankings that use preference-
// adjusted scores are shown at the NEIS baseline and say so), a disclosure that mounts its content only
// once opened (for blocks that start the engine), and a link card.

import { useState, type ReactNode } from 'react';
import { Link } from 'react-router';

export type Basis = 'facts' | 'baseline';

const BASIS_WORDS: Record<Basis, { label: string; title: string }> = {
  facts: {
    label: 'Observed / modelled facts',
    title: 'From the trade-network model under the headline shock: these do not change with the Perspective (preference) settings of a Country profile.',
  },
  baseline: {
    label: 'Preference-adjusted priorities · NEIS baseline',
    title: 'A ranking that uses preference-weighted scores; shown here at the NEIS baseline. A Country profile can change the Perspective.',
  },
};

/** The basis of a block, as a small eyebrow above its heading. */
export function BasisTag({ basis }: { basis: Basis }) {
  const w = BASIS_WORDS[basis];
  return (
    <span className="eyebrow block" title={w.title} data-basis={basis}>
      {w.label}
    </span>
  );
}

/** A disclosure whose content mounts only when first opened (and stays mounted). */
export function LazyDisclosure({ summary, children, defaultOpen = false }: { summary: ReactNode; children: () => ReactNode; defaultOpen?: boolean }) {
  const [opened, setOpened] = useState(defaultOpen);
  return (
    <details className="disclosure border-t hairline" open={defaultOpen} onToggle={(e) => (e.currentTarget as HTMLDetailsElement).open && setOpened(true)}>
      <summary className="flex items-center gap-2 py-2.5 text-sm font-medium text-paper-dim hover:text-paper">
        <span className="disclosure-chevron text-paper-mute" aria-hidden="true">
          ›
        </span>
        <span className="flex-1">{summary}</span>
      </summary>
      <div className="pb-3 pl-4">{opened ? children() : null}</div>
    </details>
  );
}

/** A card that answers one question and links to the page with the answer. */
export function QuestionCard({ n, question, answer, to, cta }: { n?: number; question: string; answer: ReactNode; to: string; cta: string }) {
  return (
    <li>
      <article className="card flex h-full flex-col p-5">
        {n !== undefined && <span className="eyebrow">Question {n}</span>}
        <h3 className="mt-1 text-lg font-medium leading-snug text-paper">{question}</h3>
        <div className="mt-2 text-sm leading-relaxed text-paper-dim">{answer}</div>
        <div className="mt-auto pt-4 text-sm">
          <Link className="link" to={to}>
            {cta} &rarr;
          </Link>
        </div>
      </article>
    </li>
  );
}
