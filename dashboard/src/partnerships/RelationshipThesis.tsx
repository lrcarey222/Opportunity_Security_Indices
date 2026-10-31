// The relationship thesis card (2026-10-20; strategic task spec s.19), shared by the Allied Industrial Base
// Partnerships page (the Economy and Partner filters) and the Country profile's Partnerships section (its
// selected partner): above the Today / OSI recommends / AIB model detail, one sentence on the pair, then
//   Complementarity     what each side brings: its Strategic AIB Roles
//   Priority compacts   where cooperation matters most: each compact with what each side contributes there
//   AIB logic           what the relationship is about: the export's labels, each with its evidence
// from the strategic layer (thesis.ts: template-driven, no score). A pair without a thesis gets one plain
// line saying why, and the compacts both take part in. HTML only: lists and a two-column grid that stacks
// at phone width, names and compacts as links, every reading in text (no colour carries meaning).

import { useMemo, type ReactNode } from 'react';
import { Link } from 'react-router';
import type { AibStrategic } from '../aib/contract.ts';
import { PARTNERSHIP } from '../content/vocabulary.ts';
import { proseName } from '../country/derive/text.ts';
import { contributionWords, pairThesis, strategicRoleWords, thesisPartners, type ContributionRow, type ThesisSide } from './thesis.ts';

export interface RelationshipThesisProps {
  /** element ids start with it (unique on the page) */
  id: string;
  strategic: AibStrategic;
  /** the focus economy (ISO3) */
  a: string;
  /** the partner (ISO3), or null: the card asks for one */
  b: string | null;
  /** an economy's display name ("United States"; running text adds the article) */
  name: (iso3: string) => string;
  /** a strategic compact's page */
  compactHref: (id: string) => string;
  /** an economy's page (null or absent: plain text) */
  countryHref?: (iso3: string) => string | null;
  /** the level of the card's heading (its parts use the next one) */
  headingLevel: 2 | 3;
  /** controls under the heading (the partner picker of the Country profile) */
  tools?: ReactNode;
  /** without a partner: where a suggested partner links to (null: suggestions are plain text) */
  pairHref?: (iso3: string) => string | null;
  /** shown under the card (a link to every thesis of the economy) */
  footer?: ReactNode;
}

/** How many suggested partners the card lists when none is chosen. */
export const THESIS_SUGGESTIONS = 6;

const cap = (s: string): string => (s ? s[0].toUpperCase() + s.slice(1) : s);

function Name({ iso3, name, href }: { iso3: string; name: (iso3: string) => string; href?: (iso3: string) => string | null }) {
  const to = href?.(iso3) ?? null;
  return to ? (
    <Link className="link text-paper" to={to}>
      {name(iso3)}
    </Link>
  ) : (
    <span className="text-paper">{name(iso3)}</span>
  );
}

function Part({ id, level, title, children, testId }: { id: string; level: 3 | 4; title: string; children: ReactNode; testId: string }) {
  const H = level === 4 ? 'h4' : 'h3';
  return (
    <section aria-labelledby={id} data-testid={testId}>
      <H id={id} className="text-sm font-medium text-paper">
        {title}
      </H>
      <div className="mt-1.5 space-y-2 text-sm leading-relaxed text-paper-dim">{children}</div>
    </section>
  );
}

/** Each side's Strategic AIB Roles (two columns from small screens up). */
function Roles({ sides, name, href }: { sides: ThesisSide[]; name: (iso3: string) => string; href?: (iso3: string) => string | null }) {
  return (
    <dl className="grid gap-x-6 gap-y-2 sm:grid-cols-2">
      {sides.map((s) => (
        <div key={s.iso3} data-thesis-side={s.iso3}>
          <dt>
            <Name iso3={s.iso3} name={name} href={href} />
          </dt>
          <dd>{cap(strategicRoleWords(s))}.</dd>
        </div>
      ))}
    </dl>
  );
}

/** The compacts with what each side contributes there. */
function Contributions({ rows, a, b, name, compactHref, label }: { rows: ContributionRow[]; a: string; b: string; name: (iso3: string) => string; compactHref: (id: string) => string; label: string }) {
  return (
    <ul className="space-y-3" aria-label={label}>
      {rows.map((r) => (
        <li key={r.compact.id} data-compact={r.compact.id}>
          <Link className="link font-medium text-paper" to={compactHref(r.compact.id)}>
            {r.compact.name}
          </Link>
          <dl className="mt-1 grid gap-x-6 gap-y-1 text-xs sm:grid-cols-2">
            <div>
              <dt className="text-paper-mute">{cap(name(a))} brings</dt>
              <dd>{contributionWords(r.a)}</dd>
            </div>
            <div>
              <dt className="text-paper-mute">{cap(name(b))} brings</dt>
              <dd>{contributionWords(r.b)}</dd>
            </div>
          </dl>
        </li>
      ))}
    </ul>
  );
}

/** Without a partner: what to do, and the economy's relationships with the most priority compacts. */
function Suggestions({ strategic, a, name, pairHref }: { strategic: AibStrategic; a: string; name: (iso3: string) => string; pairHref?: (iso3: string) => string | null }) {
  const keys = useMemo(() => thesisPartners(strategic, a), [strategic, a]);
  const prose = (x: string) => proseName(name(x));
  if (!keys.length) {
    return (
      <p className="text-sm text-paper-dim" data-testid="thesis-prompt">
        The analysis writes relationship theses for pairs of allied economies it links by trade or investment; it has none for {prose(a)}.
      </p>
    );
  }
  const shown = keys.slice(0, THESIS_SUGGESTIONS);
  return (
    <p className="text-sm text-paper-dim" data-testid="thesis-prompt">
      Choose a partner to read the strategic case for the pair. The analysis has {keys.length === 1 ? 'one' : keys.length} for {prose(a)}; those with the most priority compacts:{' '}
      {shown.map((k, i) => (
        <span key={k}>
          {i > 0 && (i === shown.length - 1 && keys.length <= shown.length ? ' and ' : ', ')}
          <Name iso3={k} name={name} href={pairHref} />
        </span>
      ))}
      {keys.length > shown.length ? ` and ${keys.length - shown.length} more` : ''}.
    </p>
  );
}

export function RelationshipThesis({ id, strategic, a, b, name, compactHref, countryHref, headingLevel, tools, pairHref, footer }: RelationshipThesisProps) {
  const prose = useMemo(() => (x: string) => proseName(name(x)), [name]);
  const t = useMemo(() => pairThesis(strategic, a, b, prose), [strategic, a, b, prose]);
  const H = headingLevel === 3 ? 'h3' : 'h2';
  const sub = (headingLevel + 1) as 3 | 4;
  const title = t ? `Relationship thesis: ${prose(a)} and ${prose(t.b.iso3)}` : 'Relationship thesis';
  return (
    <section className="card p-4 sm:p-5" aria-labelledby={`${id}-h`} data-testid="relationship-thesis" data-state={t?.state ?? 'choose'}>
      <span className="eyebrow block" title="Read from the long-run division of labor in the Allied Industrial Base analysis: the same under every scenario.">
        AIB analysis · every scenario
      </span>
      <H id={`${id}-h`} className="rule-neis-sm text-base font-medium text-paper lg:text-lg">
        {title}
      </H>
      <p className="mt-1 text-sm text-paper-dim" data-testid="thesis-question">
        <span className="text-paper">{PARTNERSHIP.thesisHeading}</span> {PARTNERSHIP.thesisPrompt}
      </p>
      {tools && <div className="mt-3">{tools}</div>}
      <div className="mt-3 space-y-4">
        {!t ? (
          <Suggestions strategic={strategic} a={a} name={name} pairHref={pairHref} />
        ) : (
          <>
            <p className="text-sm leading-relaxed text-paper" data-testid="thesis-headline">
              {t.headline}
            </p>
            <Part id={`${id}-roles`} level={sub} title="Complementarity: what each side brings" testId="thesis-roles">
              <Roles sides={[t.a, t.b]} name={name} href={countryHref} />
            </Part>
            {t.compacts.length > 0 && (
              <Part
                id={`${id}-compacts`}
                level={sub}
                title={t.state === 'thesis' ? 'Priority compacts: where cooperation matters most' : 'Compacts both take part in'}
                testId="thesis-compacts"
              >
                <Contributions
                  rows={t.compacts}
                  a={a}
                  b={t.b.iso3}
                  name={prose}
                  compactHref={compactHref}
                  label={t.state === 'thesis' ? `What ${prose(a)} and ${prose(t.b.iso3)} bring to their priority compacts` : `What ${prose(a)} and ${prose(t.b.iso3)} bring to the compacts both take part in`}
                />
                {t.moreShared.length > 0 && (
                  <p className="text-xs text-paper-mute">
                    Both also take part in{' '}
                    {t.moreShared.map((c, i) => (
                      <span key={c.id}>
                        {i > 0 && (i === t.moreShared.length - 1 ? ' and ' : ', ')}
                        <Link className="link" to={compactHref(c.id)}>
                          {c.name}
                        </Link>
                      </span>
                    ))}
                    .
                  </p>
                )}
              </Part>
            )}
            {t.state === 'thesis' && (
              <Part id={`${id}-logic`} level={sub} title="AIB logic: what the relationship is about" testId="thesis-logic">
                {t.logic.length ? (
                  <ul className="space-y-1.5" aria-label={`What the relationship of ${prose(a)} and ${prose(t.b.iso3)} is about`}>
                    {t.logic.map((l) => (
                      <li key={l.label} data-logic={l.label}>
                        <span className="font-medium text-paper">{l.title}.</span> {l.evidence}
                      </li>
                    ))}
                  </ul>
                ) : (
                  <p>The analysis links them, but none of the six logics holds.</p>
                )}
              </Part>
            )}
          </>
        )}
        {t && (
          <p className="text-xs text-paper-mute" data-testid="thesis-basis">
            Written from the strategic layer by fixed templates: each economy&apos;s Strategic AIB Roles and compact roles, and the analysis&apos;s trade and investment relationships under
            Managed Interdependence and the major-supplier disruption. The same under every scenario: the compacts are the lasting division of labor. Priority compacts are those both take part in that matter most to both (at
            most three). Every logic whose evidence holds is listed, in a fixed order; there is no bilateral score.
          </p>
        )}
        {footer}
      </div>
    </section>
  );
}
