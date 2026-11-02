// The three partnership lenses (2026-10-19; aib_scenarios_spec.md s.5 C and s.6), shared by the Allied
// Industrial Base Partnerships page (/aib/partnerships) and the Country profile's Partnerships section:
// four kinds of partnership (Trade, Investment, Development, Geopolitical), each read through three lenses,
//
//   Today            observed data as they are (2024 trade, investment positions and projects, aid, votes
//                    and agreements): no model and no preferences
//   OSI ranking   the Opportunity Security Indices at the default weights (preference-weighted scores)
//   AIB model        the Allied Industrial Base model under the toggled scenario (a counterfactual market
//                    structure, not a forecast)
//
// with the words of each panel. The cross-lens summary of each kind (which partners each lens names and
// where they agree) went on 2026-11-02, with what fed it. Pure (no React, no IO).

export const PARTNERSHIP_KINDS = ['trade', 'investment', 'development', 'geopolitical'] as const;
export type PartnershipKind = (typeof PARTNERSHIP_KINDS)[number];

export const KIND_LABEL: Readonly<Record<PartnershipKind, string>> = {
  trade: 'Trade',
  investment: 'Investment',
  development: 'Development',
  geopolitical: 'Geopolitical',
};

/** A kind from a URL value (Trade by default). */
export const kindOf = (v: string | null | undefined): PartnershipKind => ((PARTNERSHIP_KINDS as readonly string[]).includes(v ?? '') ? (v as PartnershipKind) : 'trade');

/** The lenses, in the order the panels show them. */
export const LENS_IDS = ['today', 'osi', 'aib'] as const;
export type LensId = (typeof LENS_IDS)[number];

/** Each lens's panel heading and what it is, in plain words. */
export const LENS_INFO: Readonly<Record<LensId, { title: string; short: string; words: string }>> = {
  today: { title: 'Today', short: 'today', words: 'Observed data as they are: no analysis and no preferences.' },
  osi: {
    title: 'Partner score ranking',
    short: 'the partner scores',
    words: 'The partner scores (Partnership Strength, Friendshore, Partnership Opportunity and Development Potential) at the default weights: rankings built on preference-weighted scores (a Country profile can change the Perspective).',
  },
  aib: {
    title: 'AIB analysis',
    short: 'the AIB',
    words: 'The Allied Industrial Base analysis under the chosen scenario: a stress test of how the market could be organized, not a forecast. Dollar values are 2024 trade values.',
  },
};

/** What each lens shows for each kind (the line under the panel heading). */
export const KIND_LENS_WORDS: Readonly<Record<PartnershipKind, Record<LensId, string>>> = {
  trade: {
    today: 'Who supplies whom today: observed 2024 trade.',
    osi: 'Partnership Opportunity: the partners the index rates the most attractive export markets.',
    aib: 'The new and expanded trade relationships that recover supply under the scenario when allies coordinate.',
  },
  investment: {
    today: 'Direct-investment positions (IMF) and clean-tech factory projects as they stand.',
    osi: 'Friendshore: the partners the index rates the most attractive for friendshoring production and investment.',
    aib: 'Where new capacity would help most under the scenario: the countries best placed to add it, their alternatives and the investors they serve.',
  },
  development: {
    today: 'Official development aid by donor and recipient (OECD). Export credits are not in this build yet.',
    osi: 'Development Potential: middle- and lower-income partners where building supply chains together pays off most.',
    aib: 'Where new capacity should go under the scenario: the countries that could add capacity at the supply-chain steps that need it.',
  },
  geopolitical: {
    today: 'UN General Assembly vote alignment, shared economic-security agreements and coalition membership today.',
    osi: 'The allied coalition the partner scores use (core members and partners) and the alignment part of Friendshore.',
    aib: 'The compact coalitions under the scenario: the smallest group of partners that meets the target, critical supply partners and critical dependencies outside.',
  },
};
