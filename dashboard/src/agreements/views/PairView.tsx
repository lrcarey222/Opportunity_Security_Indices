// The Agreements page with an economy and a partner: the relationship overview (the depth class with the rule
// that set it, for all agreements and for the electro-industrial ones), the strategic architecture (a strategic
// partnership? a trade agreement and its trade depth? sectoral partnerships?), the timeline of agreements,
// sector coverage, the instrument mix, implementation status (existence and delivery apart) and every agreement
// with its provenance.

import { useMemo } from 'react';
import { Link } from 'react-router';
import { ChartCard } from '../../charts/ChartCard.tsx';
import { StatTile } from '../../components/ui.tsx';
import type { AgreementsCountry, AgreementsManifest, AgrPartner, Depth, InstLevel } from '../contract.ts';
import {
  activeKeys, agreementsOfPartner, architectureOf, codeTallies, DEPTH_RULE, depthRuleWords, zeroDepthWords, type CountryIndex, filterAgreements, gapGroups, gapsInScope, indexCountry, labelOf, nameOf, plural, SCOPE_LABEL, statusCounts, timelineBins,
  type AgrFilter, type Scope,
} from '../derive.ts';
import { PairGapStrips } from './GapVisuals.tsx';
import { AbsenceCaveat, AgreementsTable, DepthChip, GradeLegend, GradeMark, NoneFound } from './parts.tsx';

export interface PairProps {
  manifest: AgreementsManifest;
  file: AgreementsCountry;
  partner: string;
  f: AgrFilter;
  names: Readonly<Record<string, string>>;
  hrefOf: (country: string, partner?: string | null) => string;
}

function DepthTile({ p, scope, manifest, ix }: { p: AgrPartner; scope: Scope; manifest: AgreementsManifest; ix: CountryIndex }) {
  const d = (scope === 'ei' ? p.depth_ei : p.depth_all) as Depth;
  const rule = depthRuleWords(scope === 'ei' ? p.depth_ei_rule : p.depth_all_rule, manifest.vocab, ix.byId);
  // class 0: none found in the scope, or agreements that no longer count
  const nInScope = agreementsOfPartner(ix, p.partner).filter((a) => scope === 'all' || a.ei).length;
  return (
    <div className="panel px-4 py-3" data-testid={`depth-${scope}`}>
      <div className="eyebrow">{SCOPE_LABEL[scope]}</div>
      <div className="mt-1.5 text-lg font-medium text-paper">
        <DepthChip d={d} v={manifest.vocab} />
      </div>
      <p className="mt-1 text-xs text-paper-dim">
        <span className="text-paper-mute">The rule that set it: </span>
        {rule || (d === 0 ? zeroDepthWords(nInScope, scope) : DEPTH_RULE[d])}
      </p>
      {rule && <p className="mt-0.5 text-xs text-paper-mute">Class {d} in general: {DEPTH_RULE[d]}</p>}
      {/* the absence caveat only where nothing was found (agreements that no longer count are found) */}
      {d === 0 && nInScope === 0 && <AbsenceCaveat className="mt-1" />}
    </div>
  );
}

export function PairView({ manifest, file, partner, f, names, hrefOf }: PairProps) {
  const v = manifest.vocab;
  const iso = file.iso3;
  const name = (x: string) => nameOf(names, x);
  const ix = useMemo(() => indexCountry(file), [file]);
  const p = ix.partner.get(partner) ?? null;
  const rows = useMemo(() => filterAgreements(file, { ...f, partner }, v, ix), [file, f, partner, v, ix]);
  const timeline = useMemo(() => timelineBins(rows, v), [rows, v]);
  const tallies = useMemo(() => codeTallies(rows, v), [rows, v]);
  const statuses = useMemo(() => statusCounts(rows, v), [rows, v]);
  const gaps = useMemo(() => gapGroups(gapsInScope(file.gaps, f.scope, v), partner, f.tech), [file.gaps, f.scope, v, partner, f.tech]);
  const pairGapRows = useMemo(() => gaps.flatMap((g) => g.rows), [gaps]);
  const pairName = `${name(iso)} and ${name(partner)}`;
  const eu = manifest.eu_members ?? [];
  const bothEu = eu.includes(iso) && eu.includes(partner);
  if (!p) {
    return (
      <section className="card p-5" data-testid="agreements-pair">
        <h2 className="rule-neis-sm text-base font-medium text-paper lg:text-lg">{pairName}</h2>
        <NoneFound>No agreement of {pairName} was found.</NoneFound>
      </section>
    );
  }
  const arch = architectureOf(p);
  const maxBin = Math.max(1, ...timeline.bins.map((b) => b.n));
  const scopeWords = SCOPE_LABEL[f.scope].toLowerCase();

  return (
    <div className="space-y-8" data-testid="agreements-pair">
      <section aria-labelledby="agr-pair-t">
        <h2 id="agr-pair-t" className="rule-neis-sm text-lg font-medium text-paper">
          {pairName}: the relationship
        </h2>
        <p className="mt-1 max-w-3xl text-sm text-paper-mute">
          {plural(p.n, 'agreement')} in these sources ({p.n_bilateral} bilateral, {p.n_plurilateral} plurilateral{p.n_via_eu ? `, ${p.n_via_eu} through the EU` : ''}); {p.n_active} active,{' '}
          {p.n_recent} in the last five years. The depth class is a labelled rule over the agreements, not a score; quantity never raises it.
        </p>
        {bothEu && (
          <p className="mt-2 max-w-3xl text-sm text-paper-dim" data-testid="intra-eu-note">
            Both are EU member states. Most of their industrial cooperation runs through EU law and EU programmes, which these sources do not
            record as agreements between members; agreements the EU signed with others are counted for each member.
          </p>
        )}
        <div className="mt-4 grid gap-4 sm:grid-cols-2">
          <DepthTile p={p} scope="all" manifest={manifest} ix={ix} />
          <DepthTile p={p} scope="ei" manifest={manifest} ix={ix} />
        </div>
      </section>

      <section className="card p-5" aria-labelledby="agr-arch-t" data-testid="architecture">
        <h2 id="agr-arch-t" className="rule-neis-sm text-base font-medium text-paper lg:text-lg">
          Strategic architecture
        </h2>
        <div className="mt-3 grid gap-4 sm:grid-cols-3">
          <StatTile label="Strategic partnership" value={arch.strategic ? 'Yes' : 'None found'} sub={arch.strategic ? 'a strategic-partnership agreement or a named strategic partnership' : 'no strategic partnership in these sources'} />
          <StatTile
            label="Trade agreement"
            value={arch.trade ? 'Yes' : 'None found'}
            sub={arch.trade ? (arch.tradeDepth !== null ? `trade depth ${arch.tradeDepth} of 7 (Design of Trade Agreements, DESTA)` : 'no trade-depth score in the trade data') : 'no free-trade or economic partnership agreement signed or in force'}
          />
          <StatTile label="Sectoral partnerships" value={arch.sectoral ? 'Yes' : 'None found'} sub={arch.sectoral ? 'an industrial or electro-industrial sector covered at an established level' : 'no sector covered at an established level'} />
        </div>
        {arch.sectors.length > 0 && (
          <ul className="mt-4 flex flex-wrap gap-x-4 gap-y-1 text-sm" aria-label="Sectors of the pair">
            {arch.sectors.map((s) => (
              <li key={s.id}>
                {labelOf(v, 'sectors', s.id)} <GradeMark grade={s.grade} />
              </li>
            ))}
          </ul>
        )}
        {!arch.strategic && !arch.trade && !arch.sectoral && <AbsenceCaveat className="mt-3" />}
      </section>

      <ChartCard
        title="Timeline"
        sub={`${plural(rows.length, 'agreement')} of the pair (${scopeWords}${activeKeys(f).length ? ', under the filters' : ''}) by year${timeline.undated ? `; ${timeline.undated} undated` : ''}.`}
        table={() => (
          <table className="data-table">
            <caption className="sr-only">Agreements of {pairName} by year</caption>
            <thead>
              <tr>
                <th className="num">Year</th>
                <th className="num">Agreements</th>
                <th className="num">Binding</th>
                <th className="num">Electro-industrial</th>
                <th className="num">With delivery evidence</th>
              </tr>
            </thead>
            <tbody>
              {timeline.bins.filter((b) => b.n).map((b) => (
                <tr key={b.year}>
                  <td className="num tabular">{b.year}</td>
                  <td className="num tabular">{b.n}</td>
                  <td className="num tabular">{b.binding}</td>
                  <td className="num tabular">{b.ei}</td>
                  <td className="num tabular">{b.delivery}</td>
                </tr>
              ))}
            </tbody>
          </table>
        )}
      >
        {timeline.bins.length ? (
          <div className="relative flex h-28 items-end gap-px overflow-x-auto" data-testid="timeline" role="list" aria-label={`Agreements of ${pairName} by year`}>
            {timeline.bins.map((b) => (
              <div key={b.year} role="listitem" className="flex min-w-2 flex-1 flex-col items-center justify-end" title={`${b.year}: ${plural(b.n, 'agreement')}`}>
                <span className="sr-only">
                  {b.year}: {plural(b.n, 'agreement')}
                </span>
                {b.n > 0 && <div className="w-full bg-brand-blue" style={{ height: `${Math.max(8, (b.n / maxBin) * 96)}px` }} aria-hidden="true" />}
                {(b.year % 10 === 0 || b.year === timeline.bins[0].year || b.year === timeline.bins[timeline.bins.length - 1].year) && (
                  <span className="mt-1 text-[0.625rem] text-paper-mute tabular" aria-hidden="true">
                    {b.year}
                  </span>
                )}
              </div>
            ))}
          </div>
        ) : (
          <NoneFound>No dated agreement matches these filters.</NoneFound>
        )}
      </ChartCard>

      <div className="grid gap-6 lg:grid-cols-3">
        <ChartCard title="Sector coverage" sub="Agreements of the pair covering each sector, established or only mentioned in text." notes={<GradeLegend />}>
          {tallies.sectors.length ? (
            <ul className="space-y-1 text-sm" data-testid="pair-sectors">
              {tallies.sectors.map((t) => (
                <li key={t.id} className="flex flex-wrap justify-between gap-2">
                  <span className="text-paper-dim">{labelOf(v, 'sectors', t.id)}</span>
                  <span className="tabular text-xs">
                    {t.specific > 0 && <GradeMark grade="specific" />} {t.specific > 0 && t.specific}
                    {t.specific > 0 && t.text > 0 && ' · '}
                    {t.text > 0 && <GradeMark grade="text" />} {t.text > 0 && t.text}
                  </span>
                </li>
              ))}
            </ul>
          ) : (
            <NoneFound>No sector recorded.</NoneFound>
          )}
        </ChartCard>
        <ChartCard title="Instrument mix" sub="Instrument groups across all the pair's agreements (pair profile), with the matching agreements that name each." notes={<GradeLegend />}>
          <ul className="space-y-1 text-sm" data-testid="pair-instruments">
            {Object.keys(v.instrument_groups ?? {}).map((g) => {
              const level = (p.inst?.[g] ?? 0) as InstLevel;
              const t = tallies.groups.find((x) => x.id === g);
              return (
                <li key={g} className="flex flex-wrap justify-between gap-2">
                  <span className={level ? 'text-paper-dim' : 'text-paper-mute'}>{labelOf(v, 'instrument_groups', g)}</span>
                  <span className="text-xs">
                    {level === 2 ? <GradeMark grade="specific" /> : level === 1 ? <GradeMark grade="text" /> : <span className="text-paper-mute">none</span>}
                    {t ? <span className="text-paper-mute"> ({plural(t.specific + t.text, 'agreement')})</span> : null}
                  </span>
                </li>
              );
            })}
          </ul>
        </ChartCard>
        <ChartCard title="Implementation status" sub="Existence (announced, signed, in force) and delivery (only where a source records it) are counted apart.">
          <div className="space-y-3 text-sm" data-testid="pair-status">
            <div>
              <div className="eyebrow">Exists</div>
              <ul className="mt-1 space-y-0.5">
                {statuses.filter((s) => !s.delivery).map((s) => (
                  <li key={s.status} className="flex justify-between gap-2">
                    <span className="text-paper-dim">{labelOf(v, 'implementation_statuses', s.status)}</span>
                    <span className="tabular">{s.n}</span>
                  </li>
                ))}
              </ul>
            </div>
            <div>
              <div className="eyebrow">Delivery</div>
              {statuses.some((s) => s.delivery) ? (
                <ul className="mt-1 space-y-0.5">
                  {statuses.filter((s) => s.delivery).map((s) => (
                    <li key={s.status} className="flex justify-between gap-2">
                      <span className="text-paper">{labelOf(v, 'implementation_statuses', s.status)}</span>
                      <span className="tabular">{s.n}</span>
                    </li>
                  ))}
                </ul>
              ) : (
                <p className="mt-1 text-paper-mute">No delivery evidence recorded for these agreements.</p>
              )}
            </div>
          </div>
        </ChartCard>
      </div>

      {pairGapRows.length > 0 && (
        <section className="card p-5" aria-labelledby="agr-pair-gaps-t" data-testid="pair-gaps">
          <h2 id="agr-pair-gaps-t" className="rule-neis-sm text-base font-medium text-paper lg:text-lg">
            Strategic gaps of the pair
          </h2>
          <p className="mt-1 max-w-3xl text-sm text-paper-mute">
            For each direction, the technologies where one could supply the other, stage by stage: where they complement each other and where an agreement covers it. Choose a
            technology to read it.
          </p>
          <div className="mt-4">
            <PairGapStrips gaps={pairGapRows} focal={iso} partner={partner} ctx={{ focal: iso, name, depthEi: () => p.depth_ei, compacts: v.compacts }} />
          </div>
          <AbsenceCaveat className="mt-3" />
        </section>
      )}

      <ChartCard title="The agreements" sub={`Every agreement of ${pairName} that matches the filters, newest first, with its sources. Existence and delivery are separate columns.`}>
        <AgreementsTable rows={rows} v={v} names={names} focal={iso} caption={`Agreements of ${pairName}`} pageSize={15} />
      </ChartCard>

      <p className="text-sm">
        <Link className="link" to={hrefOf(iso)}>
          Every partner of {name(iso)} &rarr;
        </Link>
      </p>
    </div>
  );
}
