// "Role in the Allied Industrial Base" of the Country profile (IA refactor 2026-10-16, brief s.12E; plan.md
// 5.3 item 6; id `aib-role`, linked from /aib/roles; rebuilt 2026-10-17 on the emergent network): the
// economy's roles as the AIB Country roles page reads them (aib/derive/network.ts: the economy-level summary
// of the headline run beside the per-compact roles; a role is named only where the metrics support it),
// the compacts it sits in, and its main relationships in the emergent network (partners, nodes, trade
// against investment). Every number is a modelled fact of the AIB bundle; nothing here follows the
// Perspective. A bundle without the network summary falls back to the compacts' coordinated trade.

import { useMemo } from 'react';
import { Link } from 'react-router';
import { compactTitle, fmtUsd, listWords, nameOf, plural } from '../../../aib/derive/format.ts';
import { countryEdges, countryRolesOf, edgeWords, fmtPts, MATRIX_ROLES, ROLE_FLOOR, ROLE_READING } from '../../../aib/derive/network.ts';
import { compactHref, rolesHref } from '../../../aib/nav.ts';
import { cellWords, INVEST_COLOR, ModelledNote, TRADE_COLOR, TradeInvestBar } from '../../../aib/views/networkParts.tsx';
import { aibCountry } from '../../derive/framing.ts';
import { cap, poss, proseName } from '../../derive/text.ts';
import { BasisTag, LinkRow, Section, useProfile } from './common.tsx';
import { PartnerName } from './PartnersSection.tsx';

const LEVEL_WORD = { named: 'named', minor: 'minor', none: 'not held' } as const;

export function AibRoleSection() {
  const { sheet, aib } = useProfile();
  const name = proseName(sheet.meta.country);
  const iso = sheet.iso3;
  const doc = aib?.doc ?? null;
  const names = aib?.names;
  const a = useMemo(() => (doc && names ? aibCountry(doc, names, iso) : null), [doc, names, iso]);
  const roles = useMemo(() => (doc && names ? countryRolesOf(doc, names, iso) : null), [doc, names, iso]);
  const edges = useMemo(() => (doc ? countryEdges(doc, iso) : []), [doc, iso]);
  const nodeName = (id: string) => doc?.nodes.find((nd) => nd.id === id)?.label ?? id;
  const partner = (x: string) => nameOf(names ?? {}, x);
  const shock = doc ? (doc.meta.shocks.find((s) => s.id === doc.meta.headlineShock)?.label ?? doc.meta.headlineShock) : '';
  const hasNet = !!doc?.network;
  const anyRole = !!roles && MATRIX_ROLES.some((k) => roles.cells[k].level !== 'none');
  const inAny = !!a && (anyRole || a.compacts.length > 0 || a.pairs.length > 0 || edges.length > 0);
  const tradeSum = edges.reduce((s, e) => s + e.trade, 0);
  const invSum = edges.reduce((s, e) => s + e.investment, 0);
  const top = edges.slice(0, 6);
  const max = top.reduce((m, e) => Math.max(m, e.trade + e.investment), 0);
  return (
    <Section
      id="aib-role"
      title="Role in the Allied Industrial Base"
      basis={<BasisTag kind="facts" note="the Allied Industrial Base results" />}
      intro={
        <>
          The Allied Industrial Base analysis removes a major supplier, asks how much of the lost supply existing relationships, coordinated trade and new capacity can restore, and groups
          the supply-chain nodes that need coordination into Sectoral Compacts. The roles below are read from the model&apos;s results for {name}, not a ranking.
        </>
      }
    >
      {!a || !roles ? (
        <p className="text-sm text-paper-mute">
          {aib?.status === 'loading' ? 'Loading the Allied Industrial Base results…' : 'The Allied Industrial Base results are not available in this build.'}
        </p>
      ) : !inAny ? (
        <p className="text-sm text-paper-dim">{cap(name)} holds no role in the Allied Industrial Base network or in any Sectoral Compact.</p>
      ) : (
        <div className="grid gap-4 lg:grid-cols-2">
          <section className="card p-5" aria-labelledby="aib-roles-h" data-testid="aib-roles">
            <h3 id="aib-roles-h" className="rule-neis-sm text-base font-medium text-paper lg:text-lg">
              Roles and compacts
            </h3>
            <ul className="mt-2 space-y-2 text-sm text-paper-dim">
              {MATRIX_ROLES.map((k) => {
                const c = roles.cells[k];
                return (
                  <li key={k} data-level={c.level}>
                    <span className={c.level === 'named' ? 'font-medium text-paper' : 'text-paper-mute'}>{ROLE_READING[k].short}</span>
                    <span className="text-xs text-paper-mute"> · {LEVEL_WORD[c.level]}</span>
                    {c.level !== 'none' && <span className="block text-xs text-paper-mute">{cellWords(k, c)}</span>}
                  </li>
                );
              })}
            </ul>
            <p className="mt-3 border-t hairline pt-2 text-sm text-paper-dim">
              In the coalition of {plural(roles.member.length, 'compact')}
              {roles.associate.length ? `; an associate in ${roles.associate.length} more` : ''}.
            </p>
            {a.compacts.length > 0 && (
              <ul className="mt-2 space-y-1 text-xs text-paper-mute">
                {a.compacts.map(({ compact: c, member, associate }) => (
                  <li key={c.id}>
                    <Link className="link" to={compactHref(c.id)}>
                      {c.id}
                    </Link>{' '}
                    {compactTitle(c)}
                    {member ? ' · member' : associate ? ' · associate' : ''}
                  </li>
                ))}
              </ul>
            )}
            <p className="mt-3 text-xs leading-relaxed text-paper-mute">
              {hasNet
                ? `Trade and investment roles are the share of a node's demand ${poss(name)} relationships serve after the shock, summed over nodes (1 point = 1% of one node's demand); a role is named at ${fmtPts(ROLE_FLOOR)} or more under the headline shock (${shock}), minor below.`
                : 'This build has no economy-level network summary: the roles are the compacts’ own labels.'}{' '}
              <Link className="link" to={rolesHref(iso)}>
                How roles are read
              </Link>
            </p>
          </section>
          <section className="card p-5" aria-labelledby="aib-rel-h" data-testid="aib-relationships">
            <h3 id="aib-rel-h" className="rule-neis-sm text-base font-medium text-paper lg:text-lg">
              Main relationships in the network
            </h3>
            {hasNet ? (
              edges.length ? (
                <div className="mt-2 space-y-3 text-sm text-paper-dim">
                  <p>
                    <span className="text-paper">Trade against investment:</span> under the headline shock {poss(name)} {plural(edges.length, 'relationship')} serve {fmtPts(tradeSum)} of node
                    demand through trade and {fmtPts(invSum)} through new capacity.
                    {invSum === 0 ? ` For ${name}, coordination runs through trade.` : tradeSum === 0 ? ` For ${name}, it runs through investment.` : ''}
                  </p>
                  <p className="flex flex-wrap gap-x-4 text-xs text-paper-mute" aria-hidden="true">
                    <span>
                      <span className="mr-1 inline-block h-2 w-3 rounded-sm align-middle" style={{ background: TRADE_COLOR }} />
                      Trade
                    </span>
                    <span>
                      <span className="mr-1 inline-block h-2 w-3 rounded-sm align-middle" style={{ background: INVEST_COLOR }} />
                      Investment (new capacity)
                    </span>
                  </p>
                  <ol className="space-y-2.5" aria-label={`${cap(poss(name))} main relationships, largest first`}>
                    {top.map((e) => {
                      const w = edgeWords(e);
                      const parts = [
                        e.trade > 0 ? `${w.trade} ${partner(e.partner)}: trade ${fmtPts(e.trade)} (${fmtUsd(e.edge.tradeUsd)})` : '',
                        e.investment > 0 ? `${w.investment} ${partner(e.partner)}: investment ${fmtPts(e.investment)}` : '',
                      ].filter(Boolean);
                      return (
                        <li key={`${e.edge.from}|${e.edge.to}`}>
                          <div className="flex flex-wrap items-baseline justify-between gap-x-3">
                            <PartnerName iso3={e.partner} name={partner(e.partner)} />
                            <span className="tabular text-xs text-paper-mute">{fmtPts(e.trade + e.investment)}</span>
                          </div>
                          <TradeInvestBar trade={e.trade} investment={e.investment} max={max} label={partner(e.partner)} />
                          <span className="mt-0.5 block text-xs text-paper-mute">
                            {cap(name)} {parts.join('; ')} · {listWords(e.edge.nodes.slice(0, 3).map(nodeName))}
                            {e.edge.nodes.length > 3 ? ` and ${e.edge.nodes.length - 3} more` : ''}
                          </span>
                        </li>
                      );
                    })}
                  </ol>
                </div>
              ) : (
                <p className="mt-2 text-sm text-paper-dim">{cap(name)} has no relationship with a headline-shock contribution in the network.</p>
              )
            ) : (
              <div className="mt-2 space-y-3 text-sm text-paper-dim">
                <p>
                  <span className="text-paper">Trade against investment:</span> {plural(a.trade.n, 'trade relationship')} ({fmtUsd(a.trade.usd)}) and {plural(a.investment.n, 'investment relationship')}
                  {a.investment.n ? ` (${fmtUsd(a.investment.usd)})` : ''} in the compacts.
                </p>
                {a.pairs.length > 0 && (
                  <ol className="mt-1 space-y-1">
                    {a.pairs.slice(0, 5).map((p) => (
                      <li key={p.partner}>
                        <PartnerName iso3={p.partner} name={partner(p.partner)} /> <span className="tabular text-xs text-paper-mute">{fmtUsd(p.usd)}</span>
                        <span className="block text-xs text-paper-mute">{listWords(p.nodes.slice(0, 3).map(nodeName))}</span>
                      </li>
                    ))}
                  </ol>
                )}
              </div>
            )}
            <ModelledNote className="mt-3">{`${shock}, ${doc?.meta.headlineScenario ?? ''} scenario`}</ModelledNote>
          </section>
        </div>
      )}
      <LinkRow>
        <Link className="link" to={rolesHref(iso)}>
          See all country roles &rarr;
        </Link>
        <Link className="link" to="/aib">
          The Allied Industrial Base overview &rarr;
        </Link>
      </LinkRow>
    </Section>
  );
}
