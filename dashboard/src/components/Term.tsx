// Term (spec_country.md C3.3): the full name of a glossary term followed by its InfoTip, for the first
// use of each index, category or measure on a page (`<Term id="es" />` -> "Energy Security (i)").
// `plain` renders the name only; `children` replaces the displayed words ("energy security" in running
// text) while the tip keeps the glossary term. Phrasing content only (safe inside a sentence); not
// inside headings (see InfoTip).

import type { ReactNode } from 'react';
import { InfoTip } from './InfoTip.tsx';
import { useGlossary } from './glossaryVars.tsx';

export interface TermProps {
  id: string;
  /** the name only, no tip */
  plain?: boolean;
  /** the words shown (default: the glossary term) */
  children?: ReactNode;
  className?: string;
}

export function Term({ id, plain, children, className }: TermProps) {
  const { byId } = useGlossary();
  const entry = byId.get(id);
  const words = children ?? entry?.term ?? id;
  if (plain || !entry) return <span className={className}>{words}</span>;
  return (
    <span className={`whitespace-nowrap ${className ?? ''}`}>
      <span className="whitespace-normal">{words}</span>
      {' '}
      <InfoTip id={id} term={typeof words === 'string' ? words : entry.term} />
    </span>
  );
}
