// Partner eligibility (external review readiness, 2026-11-23): the engine's partner recommendations
// (R/strategic_position/partner_recs.R, shipped as posture detail `cells[].partners` and core `cells.p1`) are
// drawn from the suppliers each reporter "trusts". For a non-aligned reporter that is every non-coercive
// supplier (supply_structure.R sp_nonaligned_trust), so China or Russia can head the list. The Explorer frames
// partners as the trusted network (trustedNetwork.ts), so every partner list is split here, client side, with
// no data change:
//   eligible: partners in the trusted network, the only ones named as partners for the Allied Industrial Base;
//   outside:  the other partners, still shown, as major suppliers outside the trusted network, never as partners.
// Order within each list is the engine's. Nothing is dropped and nobody is relabelled as trusted.
// Docs: docs/PARTNER_ELIGIBILITY_AND_FRAMING.md.

import { TERMS } from '../content/vocabulary.ts';
import { trustedNetwork, type TrustedNetwork } from './trustedNetwork.ts';

/** What the split needs from the network: membership. */
export type Membership = Pick<TrustedNetwork, 'has'>;

export interface PartnerSplit<P> {
  /** partners in the trusted network, in the engine's order */
  eligible: P[];
  /** partners outside the trusted network, in the engine's order */
  outside: P[];
}

/** Split a partner list into eligible trusted partners and suppliers outside the trusted network. */
export function splitPartners<P extends { iso3: string }>(partners: readonly P[] | null | undefined, net: Membership): PartnerSplit<P> {
  const eligible: P[] = [];
  const outside: P[] = [];
  for (const p of partners ?? []) (net.has(p.iso3) ? eligible : outside).push(p);
  return { eligible, outside };
}

/** The first partner in the trusted network, or null. */
export function leadEligible<P extends { iso3: string }>(partners: readonly P[] | null | undefined, net: Membership): P | null {
  return (partners ?? []).find((p) => net.has(p.iso3)) ?? null;
}

const cache = new WeakMap<object, TrustedNetwork>();

/** The trusted network of a posture core's (or fact sheet's) country table, built once per table. */
export function networkOf(countries: readonly { iso3: string; name: string; tier: string }[]): TrustedNetwork {
  let net = cache.get(countries);
  if (!net) {
    net = trustedNetwork(countries);
    cache.set(countries, net);
  }
  return net;
}

export const OUTSIDE_NETWORK = 'outside the trusted network';
export const ELIGIBLE_PARTNERS_LABEL = 'Eligible trusted partners';
export const OUTSIDE_SUPPLIERS_LABEL = 'Major suppliers outside the trusted network';
export const NO_ELIGIBLE_PARTNER = 'No eligible trusted partner identified';

/** "China", "China and Russia", "China, Russia and Kazakhstan". */
export function joinNames(names: readonly string[]): string {
  if (names.length <= 1) return names[0] ?? '';
  return `${names.slice(0, -1).join(', ')} and ${names[names.length - 1]}`;
}

/**
 * The plain statement for a partner list with no eligible partner: "No eligible trusted partner identified;
 * the leading suppliers, China and Russia, are outside the trusted network." The engine's partners are ranked
 * by their share of world (or modelled allied) supply, not by the reporter's own imports, so the words say
 * "leading suppliers", not "current supply".
 */
export function noEligibleWords(outsideNames: readonly string[], end = '.'): string {
  return outsideNames.length
    ? `${NO_ELIGIBLE_PARTNER}; the leading ${outsideNames.length === 1 ? 'supplier' : 'suppliers'}, ${joinNames(outsideNames)}, ${outsideNames.length === 1 ? 'is' : 'are'} ${OUTSIDE_NETWORK}${end}`
    : `${NO_ELIGIBLE_PARTNER}${end}`;
}

/** The lead of a Partner step's list, after the split. */
export type PartnerLead =
  /** the engine's partner gap: no credible trusted supplier at all */
  | { kind: 'gap' }
  /** the first eligible trusted partner */
  | { kind: 'eligible'; iso3: string }
  /** the detail's list is known and has no eligible partner: the suppliers outside the network */
  | { kind: 'none_eligible'; outside: string[] }
  /** only the core's first partner is known (the detail not in), and it is outside the network */
  | { kind: 'first_outside'; iso3: string }
  /** nothing named */
  | { kind: 'none' };

/**
 * The lead of a Partner step: from the detail's partners where they are in (null while the detail loads),
 * else from the core's first partner (`p1`, the same engine list's first entry).
 */
export function partnerLead(partners: readonly { iso3: string }[] | null | undefined, partner1: string | null | undefined, gap: boolean, net: Membership): PartnerLead {
  if (gap) return { kind: 'gap' };
  if (partners && partners.length) {
    const lead = leadEligible(partners, net);
    return lead ? { kind: 'eligible', iso3: lead.iso3 } : { kind: 'none_eligible', outside: partners.map((p) => p.iso3) };
  }
  if (partner1) return net.has(partner1) ? { kind: 'eligible', iso3: partner1 } : { kind: 'first_outside', iso3: partner1 };
  return { kind: 'none' };
}

/**
 * A Partner step's lead in one line: "Best-placed partner: Japan"; "No eligible trusted partner identified; the
 * leading supplier, China, is outside the trusted network"; "First-ranked supplier: China, outside the trusted
 * network" while only the core is in; "No credible allied solution yet" for the engine's gap; null for none.
 */
export function partnerLeadWords(lead: PartnerLead, names: Readonly<Record<string, string>>): string | null {
  const nm = (x: string) => names[x] ?? x;
  switch (lead.kind) {
    case 'gap':
      return 'No credible allied solution yet';
    case 'eligible':
      return `${TERMS.bestPlacedPartner}: ${nm(lead.iso3)}`;
    case 'none_eligible':
      return noEligibleWords(lead.outside.map(nm), '');
    case 'first_outside':
      return `First-ranked supplier: ${nm(lead.iso3)}, ${OUTSIDE_NETWORK}`;
    case 'none':
      return null;
  }
}
