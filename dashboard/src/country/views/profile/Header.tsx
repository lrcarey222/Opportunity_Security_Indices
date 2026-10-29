// S0 and S1 of the Country profile (spec_country.md A4.2; IA refactor 2026-10-16): the page head (eyebrow,
// the country's name, the generated lede as the short strategic summary), the country picker with the tier
// / EU / policy-basis tags, the memo action, and four "at a glance" tiles (where its sectors sit, the top
// Strategic Index sector under the reader's Perspective, the strongest partner, the climate rating). The dials
// sit on the page (Perspective), so the bridge to Strategic Insights is gone. Since 2026-10-29 (design L2) the
// first tile is the strategic posture's Secure / Compete / Deploy counts (CountryPostureTile, the posture
// bundle, linking to #posture); the others read the fact sheet's 66 six-stage sectors.

import { Link, useNavigate } from 'react-router';
import { CountrySelect } from '../../../components/CountrySelect.tsx';
import { InfoTip } from '../../../components/InfoTip.tsx';
import { PageHead, StatTile, Tag } from '../../../components/ui.tsx';
import { CountryPostureTile } from '../../../posture/index.ts';
import { cellOf, dOf, type CountryManifest } from '../../contract.ts';
import { sectorCountryLink } from '../../derive/links.ts';
import { siOrder } from '../../derive/framing.ts';
import { countryOptions, quadrantLine } from '../../derive/model.ts';
import { cap, ledeOf, poss, proseName, tierTag } from '../../derive/text.ts';
import { countryHref } from '../../url.ts';
import { usePerspectiveSi, useProfile } from './common.tsx';

export function memoReady(e: { tier: string; memo: { stale: boolean } | null }): boolean {
  return e.tier === 'core' && !!e.memo && !e.memo.stale;
}

/** A core member's memo that exists but is not shown: a later fact sheet changed a claim, or the memo is
 *  under revision (config memo_revision, 2026-10-17: the original allied network design is retired and
 *  the memos are being rewritten). The memo page, the profile's memo panel and GENERATE MEMO say so. */
export function memoRevised(e: { tier: string; memo: { stale: boolean } | null }): boolean {
  return e.tier === 'core' && !!e.memo && e.memo.stale;
}

export const MEMO_REVISED_TEXT = 'This memo is being revised. It will be available again once it has been rewritten.';
export const memoRevisedLine = (name: string) => `The memo for ${name} is being revised. It will be available again once it has been rewritten.`;

export function ProfileHead({ manifest }: { manifest: CountryManifest }) {
  const { sheet, entry } = useProfile();
  const nav = useNavigate();
  const m = sheet.meta;
  const name = proseName(m.country);
  return (
    <>
      <PageHead eyebrow="COUNTRIES · COUNTRY PROFILE" title={m.country} lede={ledeOf(sheet)} />
      <div className="mb-4 flex flex-wrap items-end justify-between gap-4">
        <CountrySelect label="Choose a country" value={entry.iso3} options={countryOptions(manifest)} onChange={(iso) => nav(countryHref(iso))} className="w-72 max-w-full" />
        <div className="flex flex-wrap items-center gap-2">
          <Tag accent={m.tier === 'core'}>{tierTag(m.tier)}</Tag>
          {m.eu_member && <Tag>EU MEMBER</Tag>}
          {m.policy_basis === 'IEA PAMS only' && (
            <span className="inline-flex items-center gap-1">
              <Tag>POLICY: RECORDS ONLY, NO CLIMATE RATING</Tag>
              <InfoTip id="policy_basis" />
            </span>
          )}
          {m.alignment_basis === 'inherited' && <Tag>TERRITORY</Tag>}
        </div>
      </div>
      <div className="mb-6 flex flex-wrap items-start gap-x-6 gap-y-3">
        {memoReady(entry) && (
          <div>
            <Link to={`/countries/${entry.iso3}/memo`} className="btn btn-primary">
              GENERATE MEMO
            </Link>
            <p className="mt-1 text-xs text-paper-mute">Opens the pre-written memo for {name}</p>
          </div>
        )}
        {memoRevised(entry) && (
          <div>
            <button type="button" className="btn btn-primary" disabled aria-describedby="memo-revised-head">
              GENERATE MEMO
            </button>
            <p id="memo-revised-head" className="mt-1 text-xs text-paper-mute">
              The memo for {name} is being revised
            </p>
          </div>
        )}
        <Link to={sectorCountryLink(entry.iso3)} className="link self-center text-sm">
          {cap(poss(name))} raw data in Sectors &rarr;
        </Link>
      </div>
    </>
  );
}

export function Glance() {
  const { sheet } = useProfile();
  const si = usePerspectiveSi();
  const s = sheet.summary;
  const m = sheet.meta;
  const top = s.top_si;
  // the top Strategic Index sector follows the Perspective (the fact sheet's at the NEIS baseline)
  const order = si.map ? siOrder(sheet, si.map) : null;
  const topCell = order ? (order[0] ?? null) : top ? cellOf(sheet, top.cell) : null;
  const nextCell = order ? (order[1] ?? null) : top?.next ? cellOf(sheet, top.next) : null;
  const cat = sheet.policy.cat;
  const q = s.quadrants;
  const qLine = quadrantLine({ grow: q.grow.v ?? 0, upgrade: q.upgrade.v ?? 0, secure: q.secure.v ?? 0, watch: q.watch.v ?? 0 });
  return (
    <section id="glance" aria-label="At a glance" className="scroll-mt-16">
      {/* five tiles: the posture counts across the row on small screens, then 3 + 2 */}
      <div className="grid gap-3 sm:grid-cols-2 lg:grid-cols-3">
        <div className="flex sm:col-span-2 lg:col-span-1 [&>*]:flex-1">
          <CountryPostureTile iso3={sheet.iso3} label="STRATEGIC POSTURE" link={{ href: '#posture', label: 'Strategic posture' }} />
        </div>
        <StatTile
          label="WHERE ITS SECTORS SIT"
          value={<span className="text-base">{qLine}</span>}
          sub={
            <span>
              {dOf(s.n_placed)} of {dOf(s.n_scored)} sectors can be compared with other economies <InfoTip id="rankable" term="comparable (rankable)" />
            </span>
          }
        />
        <StatTile
          label="TOP STRATEGIC INDEX SECTOR"
          value={m.has_si && topCell ? topCell.label : 'No Strategic Index'}
          sub={
            !m.has_si || !topCell
              ? 'No International Energy Agency policy records, so no Strategic Index is computed'
              : order
                ? `Under your perspective${nextCell ? `; next: ${nextCell.label}` : ''}`
                : top
                  ? `Strategic Index ${top.si.d}${nextCell ? `; next: ${nextCell.label}, ${top.margin.d} lower` : ''}`
                  : ''
          }
          chip={m.has_si && !order && top?.near_tie ? <Tag>NEAR-TIE</Tag> : undefined}
        />
        <StatTile
          label="STRONGEST PARTNER"
          value={s.psi_1 ? s.psi_1.name : 'No ranked partner'}
          sub={s.psi_1 ? `Partnership Strength ${s.psi_1.psi.d}, 1st of ${s.psi_1.n_ranked.d} (NEIS baseline)` : 'No partner can be ranked for this economy'}
        />
        <StatTile
          label="CLIMATE ACTION TRACKER"
          value={cat ? cat.overall : 'Not rated'}
          sub={cat ? `${cat.date_label ? `As of ${cat.date_label}` : ''}${cat.scope === 'eu_bloc' ? `${cat.date_label ? '; ' : ''}the EU's rating` : ''}` : 'Policy activity rests on International Energy Agency records only'}
        />
      </div>
    </section>
  );
}
