// S0 and S1 of the Country profile (spec_country.md A4.2): the page head (eyebrow, the country's name,
// the generated lede), the country picker with the tier / EU / policy-basis tags, the memo and Strategic
// actions, and the seven "at a glance" tiles.

import { Link, useNavigate } from 'react-router';
import { CountrySelect } from '../../../components/CountrySelect.tsx';
import { InfoTip } from '../../../components/InfoTip.tsx';
import { PageHead, StatTile, Tag } from '../../../components/ui.tsx';
import { cellOf, dOf, type CountryManifest } from '../../contract.ts';
import { sectorCountryLink, strategicCountryLink } from '../../derive/links.ts';
import { countryOptions, quadrantLine } from '../../derive/model.ts';
import { cap, ledeOf, poss, proseName, tierTag } from '../../derive/text.ts';
import { countryHref } from '../../url.ts';
import { Section, useProfile } from './common.tsx';

export function memoReady(e: { tier: string; memo: { stale: boolean } | null }): boolean {
  return e.tier === 'core' && !!e.memo && !e.memo.stale;
}

export function ProfileHead({ manifest }: { manifest: CountryManifest }) {
  const { sheet, entry } = useProfile();
  const nav = useNavigate();
  const m = sheet.meta;
  const name = proseName(m.country);
  return (
    <>
      <PageHead eyebrow="COUNTRY INSIGHTS · PAPER BASELINE" title={m.country} lede={ledeOf(sheet)} />
      <div className="mb-4 flex flex-wrap items-end justify-between gap-4">
        <CountrySelect label="Choose a country" value={entry.iso3} options={countryOptions(manifest)} onChange={(iso) => nav(countryHref(iso))} className="w-72 max-w-full" />
        <div className="flex flex-wrap items-center gap-2">
          <Tag accent={m.tier === 'core'}>{tierTag(m.tier)}</Tag>
          {m.eu_member && (
            <span className="inline-flex items-center gap-1">
              <Tag>EU MEMBER</Tag>
              <InfoTip id="eu_node" term="the EU as one node" />
            </span>
          )}
          {m.policy_basis === 'IEA PAMS only' && (
            <span className="inline-flex items-center gap-1">
              <Tag>POLICY: RECORDS ONLY, NO CLIMATE RATING</Tag>
              <InfoTip id="policy_basis" />
            </span>
          )}
          {m.alignment_basis === 'inherited' && <Tag>TERRITORY</Tag>}
        </div>
      </div>
      <div className="mb-8 flex flex-wrap items-start gap-x-6 gap-y-3">
        {memoReady(entry) && (
          <div>
            <Link to={`/countries/${entry.iso3}/memo`} className="btn btn-primary">
              GENERATE MEMO
            </Link>
            <p className="mt-1 text-xs text-paper-mute">Opens the pre-written memo for {name}</p>
          </div>
        )}
        {m.status !== 'no_data' && (
          <div>
            <Link to={strategicCountryLink(entry.iso3)} className="btn btn-ghost">
              OPEN IN STRATEGIC INSIGHTS
            </Link>
            <p className="mt-1 text-xs text-paper-mute">Adjust the priority dials for {name}</p>
          </div>
        )}
        <Link to={sectorCountryLink(entry.iso3)} className="link self-center text-sm">
          {cap(poss(name))} raw data in Sector Insights &rarr;
        </Link>
      </div>
    </>
  );
}

export function Glance() {
  const { sheet } = useProfile();
  const s = sheet.summary;
  const m = sheet.meta;
  const top = s.top_si;
  const topCell = top ? cellOf(sheet, top.cell) : null;
  const nextCell = top?.next ? cellOf(sheet, top.next) : null;
  const pick = sheet.network.member ? sheet.network.pick : null;
  const pickCell = pick ? cellOf(sheet, pick.cell) : null;
  const cat = sheet.policy.cat;
  const ap = s.as_partner;
  const q = s.quadrants;
  const qLine = quadrantLine({ grow: q.grow.v ?? 0, upgrade: q.upgrade.v ?? 0, secure: q.secure.v ?? 0, watch: q.watch.v ?? 0 }, sheet.words?.quadrant);
  return (
    <Section id="glance" title="At a glance">
      <div className="grid gap-3 sm:grid-cols-2 lg:grid-cols-4">
        <StatTile
          label="SECTORS PLACED"
          value={`${dOf(s.n_placed)} of ${dOf(s.n_scored)}`}
          sub={
            <span>
              Sectors where both scores can be compared with other economies <InfoTip id="rankable" term="comparable (rankable)" />
            </span>
          }
        />
        <StatTile label="WHERE THEY SIT" value={<span className="text-base">{qLine}</span>} sub={(s.n_placed.v ?? 0) > 0 ? 'Click a sector in the chart below' : 'No sector can be placed'} />
        <StatTile
          label="TOP STRATEGIC INDEX SECTOR"
          value={m.has_si && topCell ? topCell.label : 'No Strategic Index'}
          sub={
            m.has_si && top
              ? `Score ${top.si.d}${nextCell ? `; next: ${nextCell.label}, ${top.margin.d} lower` : ''}`
              : 'No International Energy Agency policy records, so the paper computes no Strategic Index'
          }
          chip={m.has_si && top?.near_tie ? <Tag>NEAR-TIE</Tag> : undefined}
        />
        <StatTile
          label="ALLIED PICK"
          value={m.tier === 'none' ? 'Not a coalition member' : pickCell ? pickCell.label : 'No pick'}
          sub={
            m.tier === 'none' ? (
              'Allied picks are made for coalition members'
            ) : (
              <span>
                Top upstream or midstream sector after the endowment screen <InfoTip id="allied_pick" />
              </span>
            )
          }
          chip={pick?.near_tie ? <Tag>NEAR-TIE</Tag> : undefined}
        />
        <StatTile
          label="STRONGEST PARTNER"
          value={s.psi_1 ? s.psi_1.name : 'No ranked partner'}
          sub={s.psi_1 ? `Partnership Strength ${s.psi_1.psi.d}, 1st of ${s.psi_1.n_ranked.d}` : 'No partner can be ranked for this economy'}
        />
        <StatTile
          label="CLIMATE ACTION TRACKER"
          value={cat ? cat.overall : 'Not rated'}
          sub={cat ? `${cat.date_label ? `As of ${cat.date_label}` : ''}${cat.scope === 'eu_bloc' ? `${cat.date_label ? '; ' : ''}the EU's rating` : ''}` : 'Policy activity rests on International Energy Agency records only'}
        />
        <StatTile
          label="AS A PARTNER TO THE COALITION"
          value={ap && ap.rank.v !== null ? `${ap.rank.d} of ${ap.n.d}` : 'Not ranked'}
          sub={ap ? `Partnership Strength ${ap.psi.d}, averaged over the ${ap.view.replace(/^coalition core \((\d+) members\)$/, '$1 core members')}` : 'Not scored as a partner'}
        />
      </div>
    </Section>
  );
}
