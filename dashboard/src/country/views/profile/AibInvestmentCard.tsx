// The Allied Industrial Base card of a Country profile's Partnerships -> Investment tab (2026-10-17; owned
// with the AIB module): the investor -> host flows of the investment simulation (doc.investmentFlows) with
// the economy as investor (the hosts whose new capacity serves its lost supply) and as host (the investors
// its new capacity serves), best-ranked hosts only, as two bar lists; then the nodes where it is a ranked
// host. A bundle without the flows falls back to the nodes where it is a listed host. Modelled result: it
// does not move with the Perspective dials (the facts tag says so).

import { useMemo } from 'react';
import { Link } from 'react-router';
import { ClassChip } from '../../../aib/charts/ClassChip.tsx';
import { fmtPp, fmtUsd, listWords, nameOf, plural } from '../../../aib/derive/format.ts';
import { hostRanksOf, investmentPairs, partnerTotals, rankWords, type PartnerTotal } from '../../../aib/derive/investment.ts';
import { NO_FILTER } from '../../../aib/derive/partnerships.ts';
import { compactHref } from '../../../aib/nav.ts';
import { INVEST_COLOR, ModelledNote } from '../../../aib/views/networkParts.tsx';
import { aibCountry } from '../../derive/framing.ts';
import { cap, poss, proseName } from '../../derive/text.ts';
import { BasisTag, useProfile } from './common.tsx';
import { PartnerName } from './PartnersSection.tsx';

function Bars({ rows, label, names, nodeName }: { rows: PartnerTotal[]; label: string; names: Readonly<Record<string, string>>; nodeName: (id: string) => string }) {
  const max = rows.reduce((m, r) => Math.max(m, r.usd), 0);
  return (
    <ol className="space-y-1.5" aria-label={label}>
      {rows.slice(0, 5).map((r) => (
        <li key={r.iso3}>
          <div className="flex items-baseline justify-between gap-3">
            <PartnerName iso3={r.iso3} name={nameOf(names, r.iso3)} />
            <span className="tabular text-xs text-paper-mute">{fmtUsd(r.usd)}</span>
          </div>
          <span className="block h-1.5 overflow-hidden rounded-sm bg-white/5">
            <span className="block h-full" style={{ width: `${max > 0 ? (r.usd / max) * 100 : 0}%`, background: INVEST_COLOR }} />
          </span>
          <span className="block text-xs text-paper-mute">
            {listWords(r.nodes.slice(0, 2).map(nodeName))}
            {r.nodes.length > 2 ? ` and ${r.nodes.length - 2} more` : ''}
          </span>
        </li>
      ))}
    </ol>
  );
}

export function AibInvestmentCard() {
  const { sheet, aib } = useProfile();
  const name = proseName(sheet.meta.country);
  const iso = sheet.iso3;
  const doc = aib?.doc ?? null;
  const names = aib?.names ?? {};
  const pairs = useMemo(() => (doc ? investmentPairs(doc, { ...NO_FILTER, country: iso }) : []), [doc, iso]);
  const asInvestor = useMemo(() => partnerTotals(pairs, iso, 'investor'), [pairs, iso]);
  const asHost = useMemo(() => partnerTotals(pairs, iso, 'host'), [pairs, iso]);
  const ranks = useMemo(() => (doc ? hostRanksOf(doc, iso) : []), [doc, iso]);
  const a = useMemo(() => (doc && aib?.names ? aibCountry(doc, aib.names, iso) : null), [doc, aib?.names, iso]);
  const nodeName = (id: string) => doc?.nodes.find((nd) => nd.id === id)?.label ?? id;
  const nodeOf = (id: string) => doc?.nodes.find((nd) => nd.id === id);
  const shock = doc ? (doc.meta.shocks.find((s) => s.id === doc.meta.headlineShock)?.label ?? doc.meta.headlineShock) : '';
  const maxRank = doc?.investmentFlows?.variant.maxRank;
  const sum = (rows: PartnerTotal[]) => rows.reduce((s, r) => s + r.usd, 0);
  const best = ranks.filter((h) => h.rank === 1);
  return (
    <section className="card p-5" aria-labelledby="aib-host-h" data-testid="aib-host">
      <div className="mb-2">
        <BasisTag kind="facts" />
      </div>
      <h3 id="aib-host-h" className="rule-neis-sm text-base font-medium text-paper lg:text-lg">
        Investment flows in the Allied Industrial Base
      </h3>
      <div className="mt-2 space-y-3 text-sm text-paper-dim">
        {!doc ? (
          <p className="text-paper-mute">{aib?.status === 'loading' ? 'Loading the Allied Industrial Base results…' : 'The Allied Industrial Base results are not available in this build.'}</p>
        ) : !doc.investmentFlows ? (
          a && a.hostNodes.length ? (
            <p>
              {cap(name)} is a listed host for new capacity in {plural(a.hostNodes.length, 'supply-chain node')}: {listWords(a.hostNodes.slice(0, 4).map((nd) => nd.label))}.
            </p>
          ) : (
            <p>{cap(name)} is not a listed host for new capacity in any supply-chain node.</p>
          )
        ) : (
          <>
            <p>
              Where a gap remains after coordinated trade, the model adds new capacity at the best-ranked host of each node; the economies whose lost supply it replaces are its investors.
            </p>
            <div className="grid gap-4 sm:grid-cols-2">
              <div data-testid="aib-as-investor">
                <div className="text-paper">As investor</div>
                {asInvestor.length ? (
                  <>
                    <p className="mb-1 text-xs text-paper-mute">
                      New capacity at {plural(asInvestor.length, 'host')} serves {fmtUsd(sum(asInvestor))} a year of {poss(name)} lost supply:
                    </p>
                    <Bars rows={asInvestor} label={`Hosts whose new capacity serves ${name}`} names={names} nodeName={nodeName} />
                  </>
                ) : (
                  <p className="text-xs text-paper-mute">No new capacity is simulated for {poss(name)} gaps.</p>
                )}
              </div>
              <div data-testid="aib-as-host">
                <div className="text-paper">As host</div>
                {asHost.length ? (
                  <>
                    <p className="mb-1 text-xs text-paper-mute">
                      New capacity in {name} serves {plural(asHost.length, 'investor')}, {fmtUsd(sum(asHost))} a year:
                    </p>
                    <Bars rows={asHost} label={`Investors served by new capacity in ${name}`} names={names} nodeName={nodeName} />
                  </>
                ) : (
                  <p className="text-xs text-paper-mute">{cap(name)} is not the best-ranked host of any node.</p>
                )}
              </div>
            </div>
            {ranks.length > 0 && (
              <div>
                <div className="text-paper">Ranked host</div>
                <p className="text-xs text-paper-mute">
                  {cap(name)} is a ranked host in {plural(new Set(ranks.map((h) => h.node)).size, 'node')}
                  {best.length ? `, the best-ranked in ${best.length}` : ''}. Lower ranks are alternatives to the best host, not added supply.
                </p>
                <ul className="mt-1 space-y-1">
                  {ranks.slice(0, 5).map((h) => {
                    const nd = nodeOf(h.node);
                    return (
                      <li key={`${h.node}|${h.rank}`} className="flex flex-wrap items-baseline gap-x-2 text-xs">
                        <span className="text-paper">{nd?.label ?? h.node}</span>
                        {nd && <ClassChip cls={nd.cls} />}
                        <span className="tabular text-paper-mute">
                          {rankWords(h.rank, maxRank)} · resilience gain {fmtPp(h.gain)}
                        </span>
                        {nd?.compact && (
                          <Link className="link" to={compactHref(nd.compact)}>
                            {nd.compact}
                          </Link>
                        )}
                      </li>
                    );
                  })}
                </ul>
              </div>
            )}
            <p className="text-xs">
              <Link className="link" to={`/aib/partnerships?mode=investment&country=${iso}`}>
                Every investor-to-host flow of {name} &rarr;
              </Link>
            </p>
            <ModelledNote>{`${shock}, ${doc.investmentFlows.variant.scenario} scenario, an increment of ${Math.round(doc.investmentFlows.variant.increment * 100)}% of node demand per host`}</ModelledNote>
          </>
        )}
      </div>
    </section>
  );
}
