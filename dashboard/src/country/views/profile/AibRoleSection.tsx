// "Role in the Allied Industrial Base" of the Country profile (IA refactor 2026-10-16, brief s.12E; plan.md
// 5.3 item 6; id `aib-role`, linked from /aib/roles): the economy's roles in the compacts (the model's
// own per-compact roles, aib/derive/countryRoles.ts), the compacts it sits in, its most important nodes,
// its largest bilateral relationships in the coordinated trade and the weight of trade against
// investment. Every number is a modelled fact of the AIB bundle; nothing here follows the Perspective.
// The paper's allied network design (preference-adjusted, NEIS baseline) closes the section.
// Data gap (plan.md 8): the economy-level summary of the AIB run (emergent_aib_country_summary) is not
// in the dashboard bundle, so roles, flows and nodes come from the compacts document.

import { useMemo } from 'react';
import { Link } from 'react-router';
import { ClassChip } from '../../../aib/charts/ClassChip.tsx';
import { compactTitle, fmtUsd, listWords, nameOf, plural } from '../../../aib/derive/format.ts';
import { compactHref, rolesHref } from '../../../aib/nav.ts';
import { aibCountry } from '../../derive/framing.ts';
import { cap, poss, proseName } from '../../derive/text.ts';
import { BasisTag, LinkRow, Section, useProfile } from './common.tsx';
import { NetworkCard, PartnerName } from './PartnersSection.tsx';

export function AibRoleSection() {
  const { sheet, aib } = useProfile();
  const name = proseName(sheet.meta.country);
  const iso = sheet.iso3;
  const doc = aib?.doc ?? null;
  const names = aib?.names;
  const a = useMemo(() => (doc && names ? aibCountry(doc, names, iso) : null), [doc, names, iso]);
  const nodeName = (id: string) => doc?.nodes.find((nd) => nd.id === id)?.label ?? id;
  const inAny = !!a && (a.roles.length > 0 || a.compacts.length > 0 || a.pairs.length > 0);
  return (
    <Section
      id="aib-role"
      title="Role in the Allied Industrial Base"
      basis={<BasisTag kind="facts" note="the Allied Industrial Base results" />}
      intro={
        <>
          The Allied Industrial Base analysis asks where coordination among allies adds supply that fragmented bilateral deals would not, and groups the supply-chain nodes that need it
          into Sectoral Compacts. The roles below are the model&apos;s own labels for {name} in each compact, not a ranking.
        </>
      }
    >
      {!a ? (
        <p className="text-sm text-paper-mute">
          {aib?.status === 'loading' ? 'Loading the Allied Industrial Base results…' : 'The Allied Industrial Base results are not available in this build.'}
        </p>
      ) : !inAny ? (
        <p className="text-sm text-paper-dim">{cap(name)} holds no role in any Sectoral Compact and has no relationship in the compacts&apos; coordinated trade.</p>
      ) : (
        <>
          <div className="grid gap-4 lg:grid-cols-2">
            <section className="card p-5" aria-labelledby="aib-roles-h" data-testid="aib-roles">
              <h3 id="aib-roles-h" className="rule-neis-sm text-base font-medium text-paper lg:text-lg">
                Roles and compacts
              </h3>
              {a.roles.length ? (
                <ul className="mt-2 space-y-1.5 text-sm text-paper-dim">
                  {a.roles.map((r) => (
                    <li key={r.role}>
                      <span className="text-paper">{r.role}</span> in {plural(r.compacts.length, 'compact')} (named below):{' '}
                      {r.compacts.map((c, i) => (
                        <span key={c.id}>
                          {i > 0 && ', '}
                          <Link className="link" to={compactHref(c.id)} title={c.name}>
                            {c.id}
                          </Link>
                        </span>
                      ))}
                    </li>
                  ))}
                </ul>
              ) : (
                <p className="mt-2 text-sm text-paper-dim">{cap(name)} holds no named role in the compacts.</p>
              )}
              <p className="mt-3 border-t hairline pt-2 text-sm text-paper-dim">
                In the minimum coalition of {plural(a.compacts.filter((x) => x.member).length, 'compact')}
                {a.compacts.some((x) => x.associate) ? `; an associate in ${a.compacts.filter((x) => x.associate).length} more` : ''}.
              </p>
              {a.compacts.length > 0 && (
                <ul className="mt-2 space-y-1 text-xs text-paper-mute">
                  {a.compacts.map(({ compact: c, member, associate }) => (
                    <li key={c.id}>
                      <Link className="link" to={compactHref(c.id)}>
                        {compactTitle(c)}
                      </Link>{' '}
                      {c.id}
                      {member ? ' · member' : associate ? ' · associate' : ''}
                    </li>
                  ))}
                </ul>
              )}
            </section>
            <section className="card p-5" aria-labelledby="aib-rel-h" data-testid="aib-relationships">
              <h3 id="aib-rel-h" className="rule-neis-sm text-base font-medium text-paper lg:text-lg">
                Major relationships and nodes
              </h3>
              <div className="mt-2 space-y-3 text-sm text-paper-dim">
                <p>
                  <span className="text-paper">Trade against investment:</span> {plural(a.trade.n, 'trade relationship')} ({fmtUsd(a.trade.usd)}) and{' '}
                  {plural(a.investment.n, 'investment relationship')}
                  {a.investment.n ? ` (${fmtUsd(a.investment.usd)})` : ''} in the compacts; listed host for new capacity in {plural(a.hostNodes.length, 'node')}.{' '}
                  {a.trade.n > 0 && a.investment.n === 0 ? `For ${name}, coordination runs through trade.` : ''}
                </p>
                {a.pairs.length > 0 && (
                  <div>
                    <div className="text-paper">Largest bilateral relationships</div>
                    <ol className="mt-1 space-y-1">
                      {a.pairs.slice(0, 5).map((p) => (
                        <li key={p.partner}>
                          <PartnerName iso3={p.partner} name={nameOf(names ?? {}, p.partner)} /> <span className="tabular text-xs text-paper-mute">{fmtUsd(p.usd)}</span>
                          <span className="block text-xs text-paper-mute">{listWords(p.nodes.slice(0, 3).map(nodeName))}</span>
                        </li>
                      ))}
                    </ol>
                  </div>
                )}
                {a.keyNodes.length > 0 && (
                  <div>
                    <div className="text-paper">Most important nodes (by {poss(name)} coordinated trade)</div>
                    <ul className="mt-1 space-y-1">
                      {a.keyNodes.slice(0, 5).map(({ node, usd }) => (
                        <li key={node.id} className="flex flex-wrap items-baseline gap-x-2">
                          <span>{node.label}</span>
                          <ClassChip cls={node.cls} />
                          <span className="tabular text-xs text-paper-mute">{fmtUsd(usd)}</span>
                        </li>
                      ))}
                    </ul>
                  </div>
                )}
              </div>
            </section>
          </div>
        </>
      )}
      <LinkRow>
        <Link className="link" to={rolesHref(iso)}>
          See all country roles &rarr;
        </Link>
        <Link className="link" to="/aib">
          The Allied Industrial Base overview &rarr;
        </Link>
      </LinkRow>
      <div>
        <div className="mb-1.5">
          <BasisTag kind="prefs" note="NEIS baseline" />
        </div>
        <NetworkCard />
      </div>
    </Section>
  );
}
