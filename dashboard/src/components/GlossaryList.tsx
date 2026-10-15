// GlossaryList (spec_country.md C3.3): the glossary as a definition list, filtered by module or by ids,
// each entry anchored (`#term-{id}`, the target of InfoTip's "More in the glossary") with its short
// definition, the longer note where there is one, and links to related terms. Used by the /glossary
// page, Strategic Methods, Sector Sources and Country About.

import { glossaryFor, type GlossaryEntry, type GlossaryModule } from '../content/glossary.ts';
import { glossaryAnchor } from './InfoTip.tsx';
import { useGlossary } from './glossaryVars.tsx';

export interface GlossaryListProps {
  /** the entries of one module (default all) */
  module?: GlossaryModule | 'all';
  /** or exactly these ids, in this order */
  ids?: readonly string[];
  /** link related terms within the page (default true) */
  related?: boolean;
  /** anchor ids on the entries (default true; turn off when two lists share a page) */
  anchors?: boolean;
  /** the entry a link points at (marked with the orange rule) */
  current?: string | null;
  className?: string;
}

export function GlossaryList({ module = 'all', ids, related = true, anchors = true, current, className }: GlossaryListProps) {
  const { list, byId } = useGlossary();
  const entries: GlossaryEntry[] = ids ? ids.map((i) => byId.get(i)).filter((e): e is GlossaryEntry => !!e) : glossaryFor(module, list);
  const shown = new Set(entries.map((e) => e.id));
  return (
    <dl className={`divide-y divide-white/10 ${className ?? ''}`} data-testid="glossary-list">
      {entries.map((e) => (
        // the entry a link points at (#term-id) is marked with the orange rule at its left
        <div
          key={e.id}
          id={anchors ? glossaryAnchor(e.id) : undefined}
          className={`scroll-mt-24 border-l-2 py-3 pl-3 ${e.id === current ? 'border-l-signal' : 'border-l-transparent'}`}
          aria-current={e.id === current ? 'true' : undefined}
        >
          <dt className="text-base font-medium text-paper">
            {e.term}
            {e.abbr && !e.term.includes(`(${e.abbr})`) && <span className="ml-2 text-sm font-normal text-paper-mute">({e.abbr})</span>}
          </dt>
          <dd className="mt-1 max-w-3xl text-sm leading-relaxed text-paper-dim">
            {e.short}
            {e.more && <span className="mt-1.5 block text-paper-mute">{e.more}</span>}
            {related && e.related && e.related.some((r) => shown.has(r) && anchors) && (
              <span className="mt-1.5 block text-xs text-paper-mute">
                See also:{' '}
                {e.related
                  .filter((r) => shown.has(r))
                  .map((r, i) => (
                    <span key={r}>
                      {i > 0 && ', '}
                      <a className="link" href={`#${glossaryAnchor(r)}`}>
                        {byId.get(r)?.term ?? r}
                      </a>
                    </span>
                  ))}
              </span>
            )}
          </dd>
        </div>
      ))}
    </dl>
  );
}
