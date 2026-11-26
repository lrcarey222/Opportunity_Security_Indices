// The trusted network (external review readiness, 2026-11-23): the set of economies the Explorer treats as
// eligible partners for the Allied Industrial Base strategy. Since 2026-11-25 it is the ONE allied set (user
// decision: "allied" always means the same 64 economies): the AIB stress tests protect the same economies' demand
// (config/aib.yml demand_scope: include_treat_as_allied), and every view that shows tiers or allied groupings decodes
// them with allianceTier() / allianceGroup() below. Derived from data, never listed by hand:
//   * core and partner: the two-tier allied coalition shipped in the posture core (`countries.tier`, built by
//     scripts/13 from config/coalition.yml: core = U.S.-leaning UN-vote bloc with GDP >= $100bn; partner = an
//     agreement route (U.S. FTA, IPEF, MSP, G7, NATO) or a supplier route that is not China-leaning);
//   * treated as allied: config/aib_strategic_layer.yml `treat_as_allied` (Taiwan, Switzerland, Slovenia, Iceland, United Arab Emirates), economies
//     outside the tiers that the AIB analysis treats as allies. TREAT_AS_ALLIED mirrors that list; a unit
//     test (trustedNetwork.test.ts) fails when the two differ.
// Docs: docs/PARTNER_ELIGIBILITY_AND_FRAMING.md.

/** config/aib_strategic_layer.yml `treat_as_allied` (checked against the file by a unit test). */
export const TREAT_AS_ALLIED: readonly string[] = ['TWN', 'CHE', 'SVN', 'ISL', 'ARE'];

export type NetworkTier = 'core' | 'partner' | 'allied' | 'outside';

/** One economy's place in the trusted network: core or partner tier, treated as allied, or none. */
export type AllianceTier = 'core' | 'partner' | 'allied' | 'none';

/**
 * The trusted-network tier of an economy from its raw coalition tier (the data's 'core' / 'partner' / 'none'):
 * a treat_as_allied economy outside the tiers counts as 'allied'. Every view that shows a tier uses this, so the
 * allied set is the same everywhere.
 */
export function allianceTier(iso3: string | null | undefined, rawTier: string | null | undefined): AllianceTier {
  if (rawTier === 'core') return 'core';
  if (rawTier === 'partner') return 'partner';
  return iso3 && TREAT_AS_ALLIED.includes(iso3) ? 'allied' : 'none';
}

/** Is the economy in the trusted network (core, partner or treated as allied)? */
export const isTrusted = (iso3: string | null | undefined, rawTier: string | null | undefined): boolean => allianceTier(iso3, rawTier) !== 'none';

/**
 * A geographic group (china / core / partner / rest / na) with the trusted network applied: a treat_as_allied
 * economy the data file under 'rest' counts with the trusted partners. Other groups pass through unchanged.
 */
export function allianceGroup<G extends string>(code: string | null | undefined, group: G): G | 'partner' {
  return group === 'rest' && !!code && TREAT_AS_ALLIED.includes(code) ? 'partner' : group;
}

/** The tag of an economy's trusted-network tier (Country profile head). */
export const ALLIANCE_TAG: Record<AllianceTier, string> = {
  core: 'CORE MEMBER',
  partner: 'PARTNER',
  allied: 'TREATED AS ALLIED',
  none: 'OUTSIDE THE TRUSTED NETWORK',
};

/** An economy's trusted-network tier in running text ("a core member of the trusted network"). */
export function allianceWords(t: AllianceTier): string {
  return t === 'core'
    ? 'a core member of the trusted network'
    : t === 'partner'
      ? 'a partner in the trusted network'
      : t === 'allied'
        ? 'in the trusted network (treated as allied)'
        : 'outside the trusted network';
}

export interface NetworkMember {
  iso3: string;
  name: string;
  tier: Exclude<NetworkTier, 'outside'>;
}

export interface TrustedNetwork {
  members: NetworkMember[];
  size: number;
  count: Record<Exclude<NetworkTier, 'outside'>, number>;
  has: (iso3: string | null | undefined) => boolean;
  tierOf: (iso3: string | null | undefined) => NetworkTier;
}

/** The trusted network from the posture core's country table ({iso3, name, tier}). */
export function trustedNetwork(countries: readonly { iso3: string; name: string; tier: string }[]): TrustedNetwork {
  const members: NetworkMember[] = [];
  const tierBy = new Map<string, NetworkMember['tier']>();
  for (const c of countries) {
    const t = allianceTier(c.iso3, c.tier);
    if (t === 'none') continue;
    const tier: NetworkMember['tier'] = t;
    members.push({ iso3: c.iso3, name: c.name, tier });
    tierBy.set(c.iso3, tier);
  }
  members.sort((a, b) => a.name.localeCompare(b.name));
  const count = { core: 0, partner: 0, allied: 0 };
  for (const m of members) count[m.tier] += 1;
  return {
    members,
    size: members.length,
    count,
    has: (iso3) => !!iso3 && tierBy.has(iso3),
    tierOf: (iso3) => (iso3 && tierBy.get(iso3)) || 'outside',
  };
}

/** How membership is decided, in plain words (shown beside the list). */
export const TRUSTED_NETWORK_METHOD =
  'Core members vote with the United States at the UN and have GDP of at least $100 billion. Partners belong to a U.S. free-trade agreement, IPEF, the Minerals Security Partnership, the G7 or NATO, or are a major supplier of a clean-energy technology and not China-leaning. Taiwan, Switzerland, Slovenia, Iceland and the United Arab Emirates are treated as allied. This is the one allied set: the stress tests protect the demand of these economies, and the same network is used in every scenario and on every page.';

export const NETWORK_TIER_LABEL: Record<NetworkTier, string> = {
  core: 'Core members',
  partner: 'Partners',
  allied: 'Treated as allied',
  outside: 'Outside the trusted network',
};
