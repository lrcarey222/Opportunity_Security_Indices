// A Partner step's partners in the drawer (external review readiness, 2026-11-23): the engine's list split by the
// trusted network (partnerEligibility.ts). Eligible trusted partners first, in the engine's order, with their
// supply share and position; then the other ranked suppliers, named as outside the trusted network and not
// recommended. With no eligible partner it says so plainly. The engine's reason line (R partner_recs.R
// partner_reason, which calls every ranked supplier "trusted") shows only when every listed partner is in the
// network. While the detail loads, the core's first-ranked partner stands in.

import { TERMS } from '../content/vocabulary.ts';
import type { DetailPartner } from './detail.ts';
import { pct } from './derive.ts';
import { ELIGIBLE_PARTNERS_LABEL, networkOf, noEligibleWords, OUTSIDE_NETWORK, OUTSIDE_SUPPLIERS_LABEL, splitPartners } from './partnerEligibility.ts';
import type { PostureCore } from './contract.ts';

export interface PartnerEligibilityListProps {
  core: Pick<PostureCore, 'countries'>;
  /** the detail's partners (null while the detail loads) */
  partners: readonly DetailPartner[] | null;
  /** the core's first-ranked partner (`p1`) */
  partner1: string | null;
  /** the engine's partner gap (core or detail) */
  gap: boolean;
  /** the engine's reason line (detail partner_reason) */
  reason: string | null;
  names: Readonly<Record<string, string>>;
}

const RELATIONSHIP_STRONG = 0.6;

function partnerFacts(p: DetailPartner, eligible: boolean): string {
  const bits = [p.capability !== undefined ? `holds ${pct(p.capability)} of supply` : null, p.cp !== undefined ? `position ${pct(p.cp)}` : null];
  if (eligible && p.relationship !== undefined && p.relationship >= RELATIONSHIP_STRONG) bits.push('strong relationship');
  return bits.filter(Boolean).join(' · ');
}

export function PartnerEligibilityList({ core, partners, partner1, gap, reason, names }: PartnerEligibilityListProps) {
  const nm = (x: string) => names[x] ?? x;
  const why = reason ? <p className="mt-1 text-xs text-paper-mute">{reason}</p> : null;
  if (gap)
    return (
      <>
        <p className="mt-1 text-signal">No credible allied solution yet: no trusted supplier can cover it.</p>
        {why}
      </>
    );
  const net = networkOf(core.countries);
  if (!partners || !partners.length) {
    if (!partner1) return null;
    return net.has(partner1) ? (
      <>
        <p className="mt-1">
          {TERMS.bestPlacedPartner}: {nm(partner1)}
        </p>
        {why}
      </>
    ) : (
      <p className="mt-1" data-testid="partner-outside">
        First-ranked supplier: {nm(partner1)}, {OUTSIDE_NETWORK}
      </p>
    );
  }
  const { eligible, outside } = splitPartners(partners, net);
  return (
    <>
      {eligible.length ? (
        <>
          <p className="mt-1 text-xs text-paper-mute">{ELIGIBLE_PARTNERS_LABEL}</p>
          <ol className="mt-1 space-y-1" aria-label={ELIGIBLE_PARTNERS_LABEL} data-testid="partner-eligible">
            {eligible.map((p, i) => (
              <li key={p.iso3} className="flex justify-between gap-2">
                <span className="text-paper">
                  {i + 1}. {nm(p.iso3)}
                </span>
                <span className="tabular text-xs text-paper-mute">{partnerFacts(p, true)}</span>
              </li>
            ))}
          </ol>
        </>
      ) : (
        <p className="mt-1 text-paper" data-testid="partner-none-eligible">
          {noEligibleWords(outside.map((p) => nm(p.iso3)))}
        </p>
      )}
      {outside.length > 0 && (
        <div className="mt-2" data-testid="partner-outside">
          <p className="text-xs text-paper-mute">{OUTSIDE_SUPPLIERS_LABEL} (not recommended as partners)</p>
          <ul className="mt-1 space-y-1" aria-label={OUTSIDE_SUPPLIERS_LABEL}>
            {outside.map((p) => (
              <li key={p.iso3} className="flex justify-between gap-2">
                <span>{nm(p.iso3)}</span>
                <span className="tabular text-xs text-paper-mute">{partnerFacts(p, false)}</span>
              </li>
            ))}
          </ul>
        </div>
      )}
      {!outside.length && why}
    </>
  );
}
