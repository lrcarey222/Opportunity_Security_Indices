// The Today and OSI lenses' derivations that both partnership pages share (2026-10-19): the economy's
// shared economic-security agreements and UN-vote alignment (the Strategic bundle's ties chunk), its
// direct-investment partners (IMF positions, conduit financial centres left out as Friendshore does) and
// the allied coalition's tiers (the Country manifest). Pure: filters and orderings of existing fields only.

import { alignmentOf, bySize, positionsOf, sharedAgreements, type Ties } from '../views/Partners/ties.ts';

/**
 * Names of the aggregates that sit among the economies in the partnership data (the European Union is a
 * party to economic-security agreements, "EUU"), for the namers that otherwise fall back to the code.
 */
export const AGGREGATE_NAMES: Readonly<Record<string, string>> = { EUU: 'European Union' };

/**
 * The economic-security agreements of the ties chunk in words (the chunk keeps their codes; the Country
 * fact sheets carry the same words as `words.agreement`, config/country_insights.yml). A code without
 * words is shown as it is.
 */
export const AGREEMENT_WORDS: Readonly<Record<string, string>> = {
  G7: 'G7',
  NATO: 'NATO',
  IPEF: 'Indo-Pacific Economic Framework',
  MSP: 'Minerals Security Partnership',
  'MSP Forum': 'Minerals Security Partnership Forum',
  'U.S. FTA': 'U.S. free-trade agreement',
};

/** Agreement codes in words, joined ("G7, Minerals Security Partnership"); `words` (a fact sheet's) for a code not listed here. */
export function agreementNames(codes: readonly string[], words?: Readonly<Record<string, string>> | null): string {
  return codes.map((a) => AGREEMENT_WORDS[a] ?? words?.[a] ?? a).join(', ');
}

export interface AgreementTie {
  iso3: string;
  /** the economic-security agreements the two share */
  agreements: string[];
  /** UN-vote alignment of the pair (1 same bloc, 0.5 one nonaligned, 0 opposite blocs; null not in the data) */
  alignment: number | null;
}

const byTies = (a: AgreementTie, b: AgreementTie) => b.agreements.length - a.agreements.length || (b.alignment ?? -1) - (a.alignment ?? -1) || a.iso3.localeCompare(b.iso3);

/** The economies that share at least one economic-security agreement with `iso`: most agreements first, then the most aligned. */
export function agreementTies(ties: Ties, iso: string): AgreementTie[] {
  const others = new Set(ties.agreements.map((g) => g.iso3).filter((x) => x !== iso));
  return [...others]
    .map((x) => ({ iso3: x, agreements: sharedAgreements(ties, iso, x), alignment: alignmentOf(ties, iso, x) }))
    .filter((r) => r.agreements.length > 0)
    .sort(byTies);
}

/** Every economy in an economic-security agreement, with its agreements: most agreements first. */
export function agreementMembers(ties: Ties): AgreementTie[] {
  const by = new Map<string, string[]>();
  for (const g of ties.agreements) by.set(g.iso3, [...new Set([...(by.get(g.iso3) ?? []), g.agreement])].sort());
  return [...by.entries()].map(([iso3, agreements]) => ({ iso3, agreements, alignment: null })).sort(byTies);
}

/** The agreements of the chunk with their members, most members first. */
export function agreementList(ties: Ties): { agreement: string; members: string[] }[] {
  const by = new Map<string, Set<string>>();
  for (const g of ties.agreements) by.set(g.agreement, (by.get(g.agreement) ?? new Set()).add(g.iso3));
  return [...by.entries()].map(([agreement, s]) => ({ agreement, members: [...s].sort() })).sort((a, b) => b.members.length - a.members.length || a.agreement.localeCompare(b.agreement));
}

/** How many economies vote with `iso` in the UN General Assembly at each alignment level (same bloc, one nonaligned, opposite blocs). */
export function alignmentCounts(ties: Ties, iso: string): { same: number; mixed: number; opposite: number } {
  const out = { same: 0, mixed: 0, opposite: 0 };
  for (const x of ties.U) {
    if (x === iso) continue;
    const a = alignmentOf(ties, iso, x);
    if (a === null) continue;
    if (a >= 1) out.same += 1;
    else if (a > 0) out.mixed += 1;
    else out.opposite += 1;
  }
  return out;
}

/** The economy's direct-investment partners by size of the two positions together, conduit financial centres left out. */
export function dipPartners(ties: Ties, iso: string): string[] {
  return bySize(positionsOf(ties, iso, 'all').filter((r) => !r.conduit)).map((r) => r.partner_iso);
}

export interface CoalitionTier {
  iso3: string;
  name: string;
  tier: 'core' | 'partner';
}

/** The allied coalition (core members first, then partners; by name within a tier) from a roster with tiers. */
export function coalitionOf(roster: readonly { iso3: string; name: string; tier: string }[]): CoalitionTier[] {
  return roster
    .filter((c): c is CoalitionTier => c.tier === 'core' || c.tier === 'partner')
    .map((c) => ({ iso3: c.iso3, name: c.name, tier: c.tier }))
    .sort((a, b) => (a.tier === b.tier ? a.name.localeCompare(b.name, 'en') : a.tier === 'core' ? -1 : 1));
}

/** "a core member of the allied coalition", "a coalition partner", "outside the allied coalition". */
export function tierWords(tier: string | null | undefined): string {
  return tier === 'core' ? 'a core member of the allied coalition' : tier === 'partner' ? 'a partner of the allied coalition' : 'outside the allied coalition';
}

/** UN-vote alignment in words. */
export function alignmentWords(a: number | null): string {
  if (a === null) return 'UN votes not in the data';
  return a >= 1 ? 'votes in the same bloc' : a > 0 ? 'one of the two is nonaligned' : 'votes in opposite blocs';
}
