// The Overview's first screen (external review readiness, 2026-11-23): the bottom line "Where does
// coordination actually change the outcome?" (three short lists from the toggled scenario's node results,
// derive/bottomLine.ts, and the low-intervention count of the class tiles), and the "Compare two countries"
// control that opens the existing pair view (/aib/partnerships?country=A&partner=B). No new data: the AIB
// document already on the page.

import { useMemo, useState } from 'react';
import { Link } from 'react-router';
import { CountrySelect, type CountryOption } from '../../components/CountrySelect.tsx';
import { useAibCtx } from '../AibLayout.tsx';
import { inStrategicScope, type AibNode } from '../contract.ts';
import { coordinationBottomLine } from '../derive/bottomLine.ts';
import { classInfo } from '../derive/classes.ts';
import { fmtShare, fmtUsd, plural } from '../derive/format.ts';
import { filterChoices } from '../derive/partnerships.ts';
import type { NodeScenario } from '../derive/scenarios.ts';
import { cellsInFilter, nodeWhere } from '../derive/taxonomy.ts';
import { compactHref, pairHref, workstreamsHref } from '../nav.ts';

export const BOTTOM_LINE_TITLE = 'Where does coordination actually change the outcome?';
export const COMPARE_TITLE = 'Compare two countries';

/** A step's page: its compact's nodes, or the Workstreams node list filtered on its first cell (as the division card links it). */
function stepTo(n: AibNode): string {
  if (n.compact) return `${compactHref(n.compact)}#nodes`;
  const c = cellsInFilter(n)[0];
  const q = new URLSearchParams(c ? { tech: c.tech, stage: c.stage } : {});
  if (!inStrategicScope(n)) q.set('scope', 'all');
  return `${workstreamsHref()}${q.size ? `?${q.toString()}` : ''}#nodes`;
}

function List({ id, title, rows, words, empty }: { id: string; title: string; rows: NodeScenario[]; words: (r: NodeScenario) => string; empty: string }) {
  const { href } = useAibCtx();
  return (
    <section aria-labelledby={`${id}-h`} data-testid={id}>
      <h3 id={`${id}-h`} className="text-sm font-medium text-paper">
        {title}
      </h3>
      {rows.length ? (
        <ol className="mt-1.5 space-y-2 text-sm">
          {rows.map((r) => (
            <li key={r.node.id} data-node={r.node.id}>
              <Link className="link text-paper" to={href(stepTo(r.node))}>
                {r.node.label}
              </Link>{' '}
              <span className="text-xs text-paper-mute">{nodeWhere(r.node)}</span>
              <span className="block text-paper-dim">{words(r)}</span>
            </li>
          ))}
        </ol>
      ) : (
        <p className="mt-1.5 text-sm text-paper-mute">{empty}</p>
      )}
    </section>
  );
}

/**
 * The bottom line under the toggled scenario. `low` and `of`: the Low intervention tile's count and the steps
 * the tiles count (the same numbers as the class tiles below).
 */
export function CoordinationBottomLine({ rows, low, of, managed }: { rows: NodeScenario[]; low: number; of: number; managed: boolean }) {
  const bl = useMemo(() => coordinationBottomLine(rows), [rows]);
  return (
    <section aria-labelledby="bottom-line-h" className="card p-4 sm:p-5" data-testid="overview-bottom-line" id="bottom-line">
      <h2 id="bottom-line-h" className="rule-neis-sm text-lg font-medium text-paper lg:text-xl">
        {BOTTOM_LINE_TITLE}
      </h2>
      {!bl ? (
        <p className="mt-2 text-sm text-paper-dim">Today&apos;s network removes no supplier, so there is nothing to recover. Choose a disruption scenario above to see where acting together matters.</p>
      ) : (
        <>
          <div className="mt-3 grid gap-5 md:grid-cols-3">
            <List
              id="bl-dividend"
              title="Where acting together adds the most supply"
              rows={bl.dividend}
              words={(r) => `Coordinated trade recovers another ${fmtShare(r.dividend, 0)} of allied demand beyond existing relationships.`}
              empty="Acting together recovers no extra supply at any step."
            />
            <List
              id="bl-trade"
              title="Where new trade links close the gap"
              rows={bl.trade}
              words={(r) =>
                `New or expanded trade recovers ${fmtShare(r.split?.trade, 0)} of allied demand; ${r.split && r.split.build > 0.005 ? `${fmtShare(r.split.build, 0)} is left for new capacity` : 'none is left for new capacity'}.`
              }
              empty={`No step needs ${classInfo('COORDINATE').label.toLowerCase()} under this scenario.`}
            />
            <List
              id="bl-build"
              title="Where new capacity is required"
              rows={bl.build}
              words={(r) => `Even with coordinated trade, ${fmtShare(r.residual, 0)} of allied demand still needs new capacity (${fmtUsd(r.residualUsd)} a year).`}
              empty={`No step needs ${classInfo('BUILD').label.toLowerCase()} under this scenario.`}
            />
          </div>
          <p className="mt-4 text-sm text-paper-dim" data-testid="bl-low">
            {plural(low, 'step')} of {of}
            {managed ? ' it applies to' : ''} {low === 1 ? 'needs' : 'need'} {classInfo('LOW INTERVENTION').label.toLowerCase()}: {classInfo('LOW INTERVENTION').meaning.charAt(0).toLowerCase()}
            {classInfo('LOW INTERVENTION').meaning.slice(1)}{' '}
            <a className="link" href="#where">
              Every treatment &darr;
            </a>
          </p>
          <p className="mt-1 text-xs text-paper-mute">Suppliers in coordinated trade can be any economy, not only trusted partners.</p>
        </>
      )}
    </section>
  );
}

/** "Compare two countries": two economy pickers and a link to the pair view. */
export function CompareTwoCountries() {
  const { doc, strategic, href } = useAibCtx();
  const [a, setA] = useState('');
  const [b, setB] = useState('');
  const options: CountryOption[] = useMemo(() => {
    const allied = new Set((strategic?.countries ?? []).filter((c) => c.allied).map((c) => c.iso3));
    return filterChoices(doc, { country: null, tech: null, stage: null, node: null })
      .countries.map((c) => ({ iso3c: c.iso3, country: c.name, group: allied.has(c.iso3) ? 'Allied economies' : 'Other economies' }))
      .sort((x, y) => Number(allied.has(y.iso3c)) - Number(allied.has(x.iso3c)) || x.country.localeCompare(y.country, 'en'));
  }, [doc, strategic]);
  const ready = !!a && !!b && a !== b;
  return (
    <div className="mt-4" role="group" aria-labelledby="compare-h" data-testid="compare-two">
      <p id="compare-h" className="text-sm font-medium text-paper">
        {COMPARE_TITLE}
      </p>
      <div className="mt-1 flex flex-wrap items-end gap-3">
        <CountrySelect label="First country" value={a} options={options} onChange={setA} className="w-full sm:w-56" />
        <CountrySelect label="Second country" value={b} options={options} onChange={setB} className="w-full sm:w-56" />
        {ready ? (
          <Link className="btn btn-primary" to={href(pairHref(a, b))} data-testid="compare-go">
            Compare &rarr;
          </Link>
        ) : (
          <span className="text-xs text-paper-mute" data-testid="compare-hint">
            {a && a === b ? 'Choose two different countries.' : 'Choose two countries.'}
          </span>
        )}
      </div>
    </div>
  );
}
