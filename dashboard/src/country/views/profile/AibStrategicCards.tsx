// The strategic part of the Country profile's "Role in the Allied Industrial Base" (2026-10-20; task spec
// s.18): the economy's Strategic AIB Roles (with what it contributes, what it needs and where new capacity
// matters, and "How the model derives this"), its participation in the Strategic Sectoral Compacts with
// its role within each (each compact links to its page, the toggle carried), and its key bilateral
// relationships with each pair's relationship thesis. From country/derive/framing.ts aibStrategicOf() over
// the strategic layer; the same in every scenario. Used by AibRoleSection only.

import { Disclosure } from '../../../components/ui.tsx';
import type { AibToggle } from '../../../aib/scenarioUrl.ts';
import { listWords, plural } from '../../../aib/derive/format.ts';
import { strategicCompactsHref } from '../../../aib/nav.ts';
import type { AibStrategicPlace } from '../../derive/framing.ts';
import { Link } from 'react-router';
import {
  CapacityBlock, CompactRoleList, emptyWords, EntryList, HeldRoles, headlineSentence, NeedList, PartnerList, Quiet, RoleEvidence, SummaryItem,
} from '../../../aib/views/strategicRoles/parts.tsx';
import { focusWords } from '../../../aib/views/strategicRoles/derive.ts';
import { cap } from '../../derive/text.ts';
import { PartnerName } from './PartnersSection.tsx';

export function AibStrategicCards({ place, name, scenario, partnerName }: { place: AibStrategicPlace; name: string; scenario: AibToggle; partnerName: (iso: string) => string }) {
  const { s, sum } = place;
  const renderName = (iso: string | null, label: string) => (iso ? <PartnerName iso3={iso} name={label} /> : <span className="text-paper">{label}</span>);
  if (!sum) {
    return (
      <p className="text-sm text-paper-dim" data-testid="aib-strategic-none">
        {cap(name)} is not among the economies the strategic layer assesses (the allied economies and the major non-allied suppliers), so it has no strategic AIB role.
      </p>
    );
  }
  // the economy itself by the profile's own name ("South Korea", "the United States"), its partners by the AIB's
  const who = (iso: string) => (iso === sum.iso3 ? name : partnerName(iso));
  const core = sum.compacts.filter((r) => s.compacts.find((c) => c.id === r.compact)?.core.some((x) => x.iso3 === sum.iso3)).length;
  return (
    <div className="space-y-4" data-testid="aib-strategic">
      <div className="grid gap-4 lg:grid-cols-2">
        <section className="card p-5" aria-labelledby="aib-strategic-roles-h" data-testid="aib-strategic-roles">
          <span className="eyebrow block">Strategic layer · the same in every scenario</span>
          <h3 id="aib-strategic-roles-h" className="rule-neis-sm mt-1 text-base font-medium text-paper lg:text-lg">
            Strategic AIB roles
          </h3>
          {!sum.allied && <p className="mt-1 text-xs text-paper-mute">A major non-allied supplier: assessed on the supply roles only.</p>}
          <p className="mt-2 text-sm leading-relaxed text-paper" data-testid="aib-strategic-headline">
            {headlineSentence(sum)}
          </p>
          {sum.firstOnList.length > 0 && <p className="mt-1 text-xs text-paper-mute">First among allied economies in {listWords(sum.firstOnList)}.</p>}
          <div className="mt-3">
            <HeldRoles sum={sum} />
          </div>
          <dl className="mt-4 space-y-4">
            <SummaryItem term="What it contributes" testId="aib-slot-contributes">
              {sum.contributes.length ? <EntryList entries={sum.contributes} scenario={scenario} label="What it contributes" /> : <Quiet>{emptyWords(sum, 'contributes')}</Quiet>}
              {sum.focusList.length > 0 && <p className="mt-1 text-xs text-paper-mute">{focusWords(sum.focusList)}</p>}
            </SummaryItem>
            <SummaryItem term="What it needs" testId="aib-slot-needs">
              {sum.needs.length ? <NeedList needs={sum.needs} scenario={scenario} name={partnerName} renderName={renderName} /> : <Quiet>{emptyWords(sum, 'needs')}</Quiet>}
              {sum.dependency && <p className="mt-1 text-xs text-paper-mute">{cap(sum.dependency)}.</p>}
            </SummaryItem>
            <SummaryItem term="Where new capacity matters" testId="aib-slot-capacity">
              {sum.capacity.length || sum.hostIn.length ? <CapacityBlock sum={sum} scenario={scenario} /> : <Quiet>{emptyWords(sum, 'capacity')}</Quiet>}
            </SummaryItem>
          </dl>
          <div className="mt-4">
            <Disclosure summary="How the model derives this">
              <RoleEvidence sum={sum} s={s} />
            </Disclosure>
          </div>
        </section>
        <section className="card p-5" aria-labelledby="aib-strategic-compacts-h" data-testid="aib-strategic-compacts">
          <h3 id="aib-strategic-compacts-h" className="rule-neis-sm text-base font-medium text-paper lg:text-lg">
            Role in each strategic compact
          </h3>
          <p className="mb-3 mt-2 text-sm text-paper-dim">
            {sum.compacts.length
              ? `${cap(name)} has a role in ${plural(sum.compacts.length, 'strategic compact')} of ${s.compacts.length}${core ? `, as a core country in ${core}` : ''}. The most relevant first; a compact's page shows every member's role.`
              : `${cap(name)} has no role in a strategic compact.`}
          </p>
          {sum.compacts.length > 0 && <CompactRoleList roles={sum.compacts} s={s} scenario={scenario} detail />}
          <p className="mt-3 text-xs text-paper-mute">
            <Link className="link" to={strategicCompactsHref(scenario)}>
              Every Strategic Sectoral Compact &rarr;
            </Link>
          </p>
        </section>
      </div>
      <section className="card p-5" aria-labelledby="aib-bilateral-h" data-testid="aib-bilateral">
        <h3 id="aib-bilateral-h" className="rule-neis-sm text-base font-medium text-paper lg:text-lg">
          Key bilateral relationships
        </h3>
        {sum.partners.length ? (
          <>
            <p className="mb-3 mt-2 text-sm text-paper-dim">
              Its largest relationships in the {sum.partnersScenario} network, the institutional lens: what each relationship is about, the compacts where cooperation matters most and
              what each side contributes in them.
            </p>
            <PartnerList partners={sum.partners} scenario={scenario} self={sum.iso3} name={who} renderName={renderName} detail />
          </>
        ) : (
          <p className="mt-2 text-sm text-paper-mute">{emptyWords(sum, 'partners')}</p>
        )}
      </section>
    </div>
  );
}
