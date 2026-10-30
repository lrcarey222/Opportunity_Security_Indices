// Allied Strategy of the Country profile (2026-10-30; the user: "the Allied Strategy section should just show
// the compacts where the relevant country plays a role"): the compacts in which the economy holds a role, and
// nothing else.
//   Strategic Sectoral Compacts  its role within each strategic compact (the AIB document's strategic layer,
//                                country/derive/framing.ts aibStrategicOf(); the same in every scenario), each
//                                compact linking to its page with the toggle carried.
//   Sectoral Compacts            the compacts whose coalition it sits in, with the operational roles the
//                                AIB bundle names for it there (aibCountry() over the toggled scenario:
//                                Decoupling reads the main document, Managed Interdependence its scenario
//                                file, as /aib/roles does), each linking to its page. A line names the
//                                scenario shown and switches it.
// An economy with no role in either says so in one line, with a link to every economy's roles (/aib/roles).
// The per-node partner table, the strategic and operational role summaries and the network relationships are
// no longer shown here (the partnership analysis is the Medium fence's Partner sub-section; every economy's
// roles are on /aib/roles). The old #aib-role and #allied-aib-role links land on this section.

import { useMemo, type ReactNode } from 'react';
import { Link } from 'react-router';
import { useAibScenario, useAibScenarioDoc } from '../../../aib/data/hooks.ts';
import { compactTitle, listWords, plural } from '../../../aib/derive/format.ts';
import { effectiveScenario, hasScenarios, scenarioLabel } from '../../../aib/derive/scenarios.ts';
import { compactHref, rolesHref, strategicCompactsHref, withScenario } from '../../../aib/nav.ts';
import { CompactRoleList } from '../../../aib/views/strategicRoles/parts.tsx';
import { aibCountry, aibDocUnder, aibStrategicOf } from '../../derive/framing.ts';
import { STRATEGY_SECTIONS } from '../../derive/strategyPage.ts';
import { cap, poss, proseName } from '../../derive/text.ts';
import { BasisTag, LinkRow, Section, useProfile } from './common.tsx';

const SEC = STRATEGY_SECTIONS.find((s) => s.id === 'allied')!;

/** The operational roles of one compact in words: "Demand anchor, Bridge partner", else its seat ("coalition member", "associate"). */
function roleWords(x: { member: boolean; associate: boolean; roles: readonly string[] }): string {
  if (x.roles.length) return listWords([...x.roles]);
  return x.member ? 'coalition member' : x.associate ? 'associate' : 'named in the compact';
}

export function AlliedCompactsSection({ basis }: { basis?: ReactNode }) {
  const { sheet, aib } = useProfile();
  const name = proseName(sheet.meta.country);
  const iso = sheet.iso3;
  const main = aib?.doc ?? null;
  const toggle = useAibScenario();
  const available = main ? hasScenarios(main) : false;
  const sid = effectiveScenario(toggle.scenario, available);
  const managed = useAibScenarioDoc(sid === 'managed' ? 'managed' : null);
  const doc = useMemo(() => (main ? aibDocUnder(main, sid, managed.doc as Parameters<typeof aibDocUnder>[2]) : null), [main, sid, managed.doc]);
  const names = useMemo(() => (aib?.names && managed.doc ? { ...managed.doc.countries, ...aib.names } : (aib?.names ?? {})), [aib?.names, managed.doc]);
  const label = scenarioLabel(sid, main?.meta);
  // the strategic layer: structural, read from the main document whatever the scenario
  const strat = useMemo(() => (main ? aibStrategicOf(main, aib?.names ?? {}, iso) : null), [main, aib?.names, iso]);
  const a = useMemo(() => (doc ? aibCountry(doc, names, iso) : null), [doc, names, iso]);
  const strategic = strat?.sum?.compacts ?? [];
  const operational = a?.compacts ?? [];
  const managedPending = !!main && sid === 'managed' && !managed.doc;
  const under = available ? ` under ${label}` : '';
  const loading = !main && aib?.status !== 'error';

  let body: ReactNode;
  if (loading) body = <p className="text-sm text-paper-mute">Loading the Allied Industrial Base results…</p>;
  else if (!main) body = <p className="text-sm text-paper-mute">The Allied Industrial Base results are not available in this build.</p>;
  else if (!strategic.length && !operational.length && !managedPending) {
    body = (
      <p className="text-sm text-paper-dim" data-testid="allied-no-role">
        {cap(name)} holds no role in a Strategic Sectoral Compact or in a Sectoral Compact&apos;s coalition{under}.{' '}
        <Link className="link" to={withScenario('/aib/roles', sid)}>
          Every economy&apos;s roles in the Allied Industrial Base &rarr;
        </Link>
      </p>
    );
  } else {
    const s = strat?.s;
    const sum = strat?.sum;
    const core = s && sum ? sum.compacts.filter((r) => s.compacts.find((c) => c.id === r.compact)?.core.some((x) => x.iso3 === sum.iso3)).length : 0;
    body = (
      <div className="grid gap-4 lg:grid-cols-2">
        {s && strategic.length > 0 && (
          <section className="card p-5" aria-labelledby="allied-strategic-h" data-testid="allied-strategic-compacts">
            <span className="eyebrow block">Strategic layer · the same in every scenario</span>
            <h3 id="allied-strategic-h" className="rule-neis-sm mt-1 text-base font-medium text-paper lg:text-lg">
              Strategic Sectoral Compacts
            </h3>
            <p className="mb-3 mt-2 text-sm text-paper-dim">
              {cap(name)} has a role in {plural(strategic.length, 'strategic compact')} of {s.compacts.length}
              {core ? `, as a core country in ${core}` : ''}. The most relevant first; a compact&apos;s page shows every member&apos;s role.
            </p>
            <CompactRoleList roles={strategic} s={s} scenario={sid} detail />
            <p className="mt-3 text-xs text-paper-mute">
              <Link className="link" to={strategicCompactsHref(sid)}>
                Every Strategic Sectoral Compact &rarr;
              </Link>
            </p>
          </section>
        )}
        <section className="card p-5" aria-labelledby="allied-operational-h" data-testid="allied-operational-compacts">
          <span className="eyebrow block">{available ? `Under ${label}` : 'Under the headline shock'}</span>
          <h3 id="allied-operational-h" className="rule-neis-sm mt-1 text-base font-medium text-paper lg:text-lg">
            Sectoral Compacts
          </h3>
          {available && (
            <p className="mt-2 text-sm text-paper-dim" data-testid="allied-scenario">
              Shown under <span className="font-medium text-paper">{label}</span>, the scenario chosen for the Allied Industrial Base.{' '}
              <button type="button" className="link" onClick={() => toggle.setScenario(sid === 'managed' ? 'decoupling' : 'managed')}>
                Show {scenarioLabel(sid === 'managed' ? 'decoupling' : 'managed', main.meta)} instead
              </button>
            </p>
          )}
          {managedPending ? (
            <p className="mt-2 text-sm text-paper-mute">
              {managed.error ? (
                <>
                  The Managed Interdependence results could not be loaded.{' '}
                  <button type="button" className="link" onClick={managed.retry}>
                    Retry
                  </button>
                </>
              ) : managed.missing ? (
                'This data build has no Managed Interdependence results for the compacts.'
              ) : (
                'Loading the Managed Interdependence results…'
              )}
            </p>
          ) : operational.length ? (
            <>
              <p className="mt-2 text-sm text-paper-dim">
                In {plural(operational.length, 'compact')}
                {under}, with the roles the model names for {name} there:
              </p>
              <ul className="mt-2 space-y-2 text-sm" aria-label={`${cap(poss(name))} roles in the Sectoral Compacts`}>
                {operational.map((x) => (
                  <li key={x.compact.id} className="min-w-0 break-words leading-snug" data-compact={x.compact.id}>
                    <Link className="link" to={compactHref(x.compact.id, sid)}>
                      {x.compact.id}
                    </Link>{' '}
                    <span className="text-paper">{compactTitle(x.compact, doc?.nodes ?? [])}</span>
                    <span className="block text-paper-dim" data-testid="allied-compact-role">
                      {cap(roleWords(x))}
                      {x.roles.length && x.member ? ' · coalition member' : x.roles.length && x.associate ? ' · associate' : ''}
                    </span>
                  </li>
                ))}
              </ul>
            </>
          ) : (
            <p className="mt-2 text-sm text-paper-dim">{cap(name)} holds no role in a Sectoral Compact&apos;s coalition{under}.</p>
          )}
        </section>
      </div>
    );
  }

  return (
    <Section
      id="allied"
      aliases={SEC.aliases}
      title={SEC.label}
      basis={basis ?? <BasisTag kind="facts" note={available ? `the Allied Industrial Base results · ${label}` : 'the Allied Industrial Base results'} />}
      intro={
        <>
          The compacts in which {name} holds a role: its role within each Strategic Sectoral Compact (the same in every scenario) and the Sectoral Compacts whose coalition it sits in
          under the scenario chosen. Each compact links to its page.
        </>
      }
    >
      {body}
      <LinkRow>
        <Link className="link" to={rolesHref(iso, sid)}>
          See all country roles &rarr;
        </Link>
        <Link className="link" to={withScenario('/aib', sid)}>
          The Allied Industrial Base overview &rarr;
        </Link>
      </LinkRow>
    </Section>
  );
}
