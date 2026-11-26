// The strategic case of a pair (Explorer usability pass, 2026-11-25): the first thing the Partnerships pair
// view answers, "What is the strategic case for these countries working together?", from strategicCase.ts
// (the thesis's logic regrouped under the five purposes, each with the priority compacts that support it),
// then the pair's partnership memo where one is shipped. A pair without a thesis says that no strategic case
// is identified; the relationship context follows below it. Rendered in the AIB Partnerships page only (the
// Country profile's thesis card does not carry it, so the profile's first view does not grow).

import { Fragment, useMemo } from 'react';
import { Link } from 'react-router';
import type { AibStrategic } from '../aib/contract.ts';
import { Term } from '../components/Term.tsx';
import { proseName } from '../country/derive/text.ts';
import { MemoCta } from './MemoCta.tsx';
import { pairMemoOf } from './memos.ts';
import { useMemoManifest } from './memoStore.ts';
import { NO_CASE_NOTE, strategicCase, type Purpose } from './strategicCase.ts';
import { pairThesis } from './thesis.ts';

const cap = (s: string): string => (s ? s[0].toUpperCase() + s.slice(1) : s);

export interface StrategicCaseProps {
  strategic: AibStrategic;
  a: string;
  b: string;
  /** an economy's display name ("United States"; running text adds the article) */
  name: (iso3: string) => string;
  compactHref: (id: string) => string;
  /** the data release the page reads (the memo says when it predates it) */
  dataCreated?: string | null;
}

function Compacts({ p, compactHref, words }: { p: Purpose; compactHref: (id: string) => string; words: boolean }) {
  return (
    <>
      {p.support.map((x, i) => (
        <Fragment key={x.compact.id}>
          {i > 0 && (words ? '; ' : i === p.support.length - 1 ? ' and ' : ', ')}
          <Link className="link" to={compactHref(x.compact.id)}>
            {x.compact.name}
          </Link>
          {words && <span className="text-paper-mute"> ({x.words})</span>}
        </Fragment>
      ))}
    </>
  );
}

export function StrategicCase({ strategic, a, b, name, compactHref, dataCreated }: StrategicCaseProps) {
  const prose = useMemo(() => (x: string) => proseName(name(x)), [name]);
  const t = useMemo(() => pairThesis(strategic, a, b, prose), [strategic, a, b, prose]);
  const sc = useMemo(() => (t ? strategicCase(strategic, t, prose) : null), [strategic, t, prose]);
  const memos = useMemoManifest();
  if (!t || !sc) return null;
  const lead = sc.purposes.filter((p) => sc.lead.includes(p.id));
  const rest = sc.purposes.filter((p) => !sc.lead.includes(p.id));
  return (
    <div className="border-l-2 border-signal pl-3" data-testid="strategic-case" data-state={sc.state}>
      <p className="text-sm font-medium leading-relaxed text-paper" data-testid="strategic-case-answer">
        {sc.sentence}
      </p>
      {sc.state === 'case' ? (
        <ul className="mt-1.5 space-y-1 text-sm text-paper-dim">
          {lead.map((p) => (
            <li key={p.id} data-purpose={p.id}>
              <span className="text-paper">{cap(p.words)}</span>:{' '}
              {p.support.length ? (
                <>
                  in <Compacts p={p} compactHref={compactHref} words />.
                </>
              ) : (
                p.meaning
              )}
            </li>
          ))}
          {rest.length > 0 && (
            <li data-purpose="also">
              <span className="text-paper">Also</span>:{' '}
              {rest.map((p, i) => (
                <Fragment key={p.id}>
                  {i > 0 && '; '}
                  {p.words}
                  {p.support.length > 0 && (
                    <>
                      {' '}
                      (<Compacts p={p} compactHref={compactHref} words={false} />)
                    </>
                  )}
                </Fragment>
              ))}
              .
            </li>
          )}
        </ul>
      ) : (
        <p className="mt-1 text-sm text-paper-dim">{NO_CASE_NOTE}</p>
      )}
      <p className="mt-1.5 text-xs text-paper-mute">
        Read from the two countries&apos; roles in their priority Sectoral Compacts and the trade and investment links the analysis finds between them; the purposes supported in the most compacts
        lead. Cases are written only for pairs in the <Term id="trusted_network">trusted network</Term>.
      </p>
      <MemoCta className="mt-3" entry={pairMemoOf(memos, a, b)} kind="pair" dataCreated={dataCreated} />
    </div>
  );
}
