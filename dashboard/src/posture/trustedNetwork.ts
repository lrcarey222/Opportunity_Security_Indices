// The trusted network (external review readiness, 2026-11-23): the set of economies the Explorer treats as
// eligible partners for the Allied Industrial Base strategy. Derived from data, never listed by hand:
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
    const tier: NetworkMember['tier'] | null =
      c.tier === 'core' ? 'core' : c.tier === 'partner' ? 'partner' : TREAT_AS_ALLIED.includes(c.iso3) ? 'allied' : null;
    if (!tier) continue;
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
  'Core members vote with the United States at the UN and have GDP of at least $100 billion. Partners belong to a U.S. free-trade agreement, IPEF, the Minerals Security Partnership, the G7 or NATO, or are a major supplier of a clean-energy technology and not China-leaning. Taiwan, Switzerland, Slovenia, Iceland and the United Arab Emirates are treated as allied in the Allied Industrial Base analysis. The same network is used in every scenario.';

export const NETWORK_TIER_LABEL: Record<NetworkTier, string> = {
  core: 'Core members',
  partner: 'Partners',
  allied: 'Treated as allied',
  outside: 'Outside the trusted network',
};
