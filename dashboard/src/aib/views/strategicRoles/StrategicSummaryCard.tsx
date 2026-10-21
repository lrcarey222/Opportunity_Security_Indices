// "<Economy>'s role in the Allied Industrial Base" (2026-10-20; task spec s.17): the strategic summary of
// one economy on the AIB Country roles page, above the operational table. Its primary strategic roles,
// what it contributes, what it needs, its most important strategic compacts, its highest-value partners
// and where new capacity matters, then "How the model derives this": the two gates of every role and the
// rule, followed by the page's own operational evidence (`evidence`: the supplier / buyer / investor /
// host / bridge roles under the scenario). The strategic roles are structural: the same in every scenario.
// "Who holds each strategic role" lists the role holders; choosing one shows its summary.

import type { ReactNode, Ref } from 'react';
import { Disclosure } from '../../../components/ui.tsx';
import { cap } from '../../../country/derive/text.ts';
import type { AibStrategic } from '../../contract.ts';
import { listWords } from '../../derive/format.ts';
import { roleHoldersOf } from '../../derive/strategic.ts';
import type { AibToggle } from '../../scenarioUrl.ts';
import { focusWords, ROLE_MEANING, summaryHeading, type StrategicSummary } from './derive.ts';
import {
  CapacityBlock, CompactRoleList, emptyWords, EntryList, HeldRoles, headlineSentence, NeedList, PartnerList, Quiet, RoleEvidence, SummaryItem, type NameRenderer,
} from './parts.tsx';

/** The number of strategic compacts the summary names (the rest are in "How the model derives this"). */
export const TOP_COMPACTS = 3;

export function StrategicSummaryCard({
  sum, s, scenario, name, renderName, headingRef, evidence,
}: {
  sum: StrategicSummary;
  s: AibStrategic;
  /** the toggle, carried on every compact link */
  scenario: AibToggle;
  name: (iso: string) => string;
  renderName: NameRenderer;
  headingRef?: Ref<HTMLHeadingElement>;
  /** the operational evidence of the page, shown at the end of "How the model derives this" */
  evidence?: ReactNode;
}) {
  const nm = name(sum.iso3);
  const rest = sum.compacts.slice(TOP_COMPACTS);
  return (
    <article className="card p-5" aria-labelledby="strategic-summary-h" data-testid="strategic-summary" data-iso3={sum.iso3}>
      <h3 id="strategic-summary-h" ref={headingRef} tabIndex={-1} className="rule-neis-sm text-lg font-medium text-paper outline-none lg:text-xl">
        {summaryHeading(nm)}
      </h3>
      {!sum.allied && <p className="mt-1 text-xs text-paper-mute">A major non-allied supplier: assessed on the supply roles only, and never part of a compact&apos;s coalition.</p>}
      <p className="mt-2 text-[0.9375rem] leading-relaxed text-paper" data-testid="strategic-headline">
        {headlineSentence(sum)}
      </p>
      {sum.firstOnList.length > 0 && <p className="mt-1 text-sm text-paper-dim">First among allied economies in {listWords(sum.firstOnList)}.</p>}
      <dl className="mt-4 grid gap-x-8 gap-y-5 md:grid-cols-2">
        <SummaryItem term="Primary strategic roles" testId="slot-roles">
          <HeldRoles sum={sum} />
        </SummaryItem>
        <SummaryItem term="What it contributes" testId="slot-contributes">
          {sum.contributes.length ? <EntryList entries={sum.contributes} scenario={scenario} label="What it contributes" /> : <Quiet>{emptyWords(sum, 'contributes')}</Quiet>}
          {sum.focusList.length > 0 && <p className="mt-1 text-xs text-paper-mute">{focusWords(sum.focusList)}</p>}
        </SummaryItem>
        <SummaryItem term="What it needs" testId="slot-needs">
          {sum.needs.length ? <NeedList needs={sum.needs} scenario={scenario} name={name} renderName={renderName} /> : <Quiet>{emptyWords(sum, 'needs')}</Quiet>}
          {sum.dependency && <p className="mt-1 text-xs text-paper-mute">{cap(sum.dependency)}.</p>}
        </SummaryItem>
        <SummaryItem term="Most important strategic compacts" testId="slot-compacts">
          {sum.compacts.length ? <CompactRoleList roles={sum.compacts} s={s} scenario={scenario} limit={TOP_COMPACTS} /> : <Quiet>No role in a strategic compact.</Quiet>}
          {rest.length > 0 && <p className="mt-1 text-xs text-paper-mute">A role in {rest.length} more: see How the model derives this.</p>}
        </SummaryItem>
        <SummaryItem term="Highest-value partners" testId="slot-partners">
          {sum.partners.length ? (
            <>
              <PartnerList partners={sum.partners} scenario={scenario} self={sum.iso3} name={name} renderName={renderName} />
              <p className="mt-1 text-xs text-paper-mute">The largest relationships in the {sum.partnersScenario} network, the institutional lens; with each pair&apos;s relationship logic.</p>
            </>
          ) : (
            <Quiet>{emptyWords(sum, 'partners')}</Quiet>
          )}
        </SummaryItem>
        <SummaryItem term="Where new capacity matters" testId="slot-capacity">
          {sum.capacity.length || sum.hostIn.length ? <CapacityBlock sum={sum} scenario={scenario} /> : <Quiet>{emptyWords(sum, 'capacity')}</Quiet>}
        </SummaryItem>
      </dl>
      <div className="mt-4">
        <Disclosure summary="How the model derives this">
          <div className="space-y-5">
            <RoleEvidence sum={sum} s={s} />
            {rest.length > 0 && (
              <div className="text-sm">
                <p className="mb-1.5 font-medium text-paper">Every strategic compact where it has a role</p>
                <CompactRoleList roles={sum.compacts} s={s} scenario={scenario} detail />
              </div>
            )}
            {evidence}
          </div>
        </Disclosure>
      </div>
    </article>
  );
}

/** Who holds each strategic role: the primary holders, then the secondary ones, each a button that shows its summary. */
export function RoleHolders({ s, name, selected, onSelect }: { s: AibStrategic; name: (iso: string) => string; selected: string | null; onSelect: (iso: string) => void }) {
  const allied = new Set(s.countries.filter((c) => c.allied).map((c) => c.iso3));
  const who = (list: { iso3: string }[]) =>
    list.map((x, i) => (
      <span key={x.iso3}>
        {i > 0 && '; '}
        <button type="button" className="link" aria-pressed={selected === x.iso3} onClick={() => onSelect(x.iso3)}>
          {name(x.iso3)}
        </button>
        {!allied.has(x.iso3) && <span className="text-paper-mute"> (non-allied)</span>}
      </span>
    ));
  return (
    <section aria-labelledby="role-holders-h" className="mt-8" data-testid="role-holders">
      <h3 id="role-holders-h" className="rule-neis-sm mb-2 text-base font-medium text-paper lg:text-lg">
        Who holds each strategic role
      </h3>
      <p className="mb-4 max-w-3xl text-sm leading-relaxed text-paper-dim">
        The economies that hold each role as primary, then as secondary, the strongest evidence first. Choose one to read its summary above.
      </p>
      <ul className="grid gap-3 sm:grid-cols-2 lg:grid-cols-3">
        {s.roleDefs.map((d) => {
          const primary = roleHoldersOf(s, d.code, 'primary');
          const secondary = roleHoldersOf(s, d.code, 'secondary');
          return (
            <li key={d.code} className="card p-4" data-role={d.code}>
              <h4 className="text-base font-medium text-paper">{d.name}</h4>
              <p className="mt-0.5 text-xs leading-relaxed text-paper-mute">An economy that {ROLE_MEANING[d.code] ?? 'holds this role'}.</p>
              <p className="mt-2 text-sm text-paper-dim">
                <span className="text-paper">Primary: </span>
                {primary.length ? who(primary) : 'none'}
              </p>
              {secondary.length > 0 && (
                <p className="mt-1 text-xs text-paper-mute">
                  <span>Secondary: </span>
                  {who(secondary)}
                </p>
              )}
            </li>
          );
        })}
      </ul>
    </section>
  );
}
