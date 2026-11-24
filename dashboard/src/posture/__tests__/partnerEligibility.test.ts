// Partner eligibility (partnerEligibility.ts): the engine's partner lists split by the trusted network.
// Fixtures mimic the shipped Sri Lanka lists (China, Russia, Vietnam, Indonesia, United Arab Emirates; only
// Vietnam and Indonesia are coalition partners) and the United States lists (Australia, Japan, Germany).

import { describe, expect, it } from 'vitest';
import { joinNames, leadEligible, networkOf, noEligibleWords, partnerLead, partnerLeadWords, splitPartners } from '../partnerEligibility.ts';
import { trustedNetwork } from '../trustedNetwork.ts';

const COUNTRIES = [
  { iso3: 'USA', name: 'United States', tier: 'core' },
  { iso3: 'AUS', name: 'Australia', tier: 'core' },
  { iso3: 'JPN', name: 'Japan', tier: 'core' },
  { iso3: 'DEU', name: 'Germany', tier: 'core' },
  { iso3: 'CAN', name: 'Canada', tier: 'core' },
  { iso3: 'VNM', name: 'Viet Nam', tier: 'partner' },
  { iso3: 'IDN', name: 'Indonesia', tier: 'partner' },
  { iso3: 'TWN', name: 'Taiwan', tier: 'none' },
  { iso3: 'CHN', name: 'China', tier: 'none' },
  { iso3: 'RUS', name: 'Russia', tier: 'none' },
  { iso3: 'ARE', name: 'United Arab Emirates', tier: 'none' },
  { iso3: 'KAZ', name: 'Kazakhstan', tier: 'none' },
  { iso3: 'LKA', name: 'Sri Lanka', tier: 'none' },
];
const net = trustedNetwork(COUNTRIES);
const names = Object.fromEntries(COUNTRIES.map((c) => [c.iso3, c.name]));
const P = (...iso: string[]) => iso.map((iso3, i) => ({ iso3, capability: 0.3 - i * 0.05 }));

// Sri Lanka-like cells (engine order kept)
const LKA = {
  'Batteries|Components': P('CHN'),
  'Coal|Extraction': P('IDN', 'RUS'),
  'Coal|Assembly': P('RUS', 'CHN'),
  'Electric Grid|Assembly': P('VNM', 'CHN'),
  'Nuclear|Components': P('RUS', 'KAZ', 'CHN'),
  'Oil|Refining': P('CHN', 'RUS', 'ARE'),
};
// United States-like cells
const USA = { 'Batteries|Extraction': P('AUS', 'CAN'), 'Electric Vehicles|Refining': P('JPN', 'AUS'), 'Wind|Refining': P('DEU') };

describe('splitPartners', () => {
  it('Sri Lanka: China, Russia and Kazakhstan are never eligible, and never dropped', () => {
    for (const [k, ps] of Object.entries(LKA)) {
      const { eligible, outside } = splitPartners(ps, net);
      expect(eligible.length + outside.length, k).toBe(ps.length);
      for (const p of eligible) expect(['CHN', 'RUS', 'KAZ'], k).not.toContain(p.iso3);
      for (const p of outside) expect(net.has(p.iso3), k).toBe(false);
    }
    expect(splitPartners(LKA['Electric Grid|Assembly'], net)).toEqual({ eligible: [LKA['Electric Grid|Assembly'][0]], outside: [LKA['Electric Grid|Assembly'][1]] });
    expect(splitPartners(LKA['Oil|Refining'], net).outside.map((p) => p.iso3)).toEqual(['CHN', 'RUS']);
    // the Emirates are treated as allied (config/aib_strategic_layer.yml treat_as_allied, 2026-11-23)
    expect(splitPartners(LKA['Oil|Refining'], net).eligible.map((p) => p.iso3)).toEqual(['ARE']);
  });

  it('United States: every partner is eligible, in the engine order', () => {
    for (const [k, ps] of Object.entries(USA)) expect(splitPartners(ps, net), k).toEqual({ eligible: ps, outside: [] });
  });

  it('Taiwan counts as a member (treated as allied); empty and missing lists split to nothing', () => {
    expect(splitPartners(P('TWN', 'CHN'), net).eligible.map((p) => p.iso3)).toEqual(['TWN']);
    expect(splitPartners(null, net)).toEqual({ eligible: [], outside: [] });
    expect(splitPartners([], net)).toEqual({ eligible: [], outside: [] });
  });
});

describe('leadEligible and partnerLead', () => {
  it('the lead is the first member, skipping a non-member ranked above it', () => {
    expect(leadEligible(P('CHN', 'VNM'), net)?.iso3).toBe('VNM');
    expect(leadEligible(LKA['Batteries|Components'], net)).toBeNull();
    expect(leadEligible(USA['Electric Vehicles|Refining'], net)?.iso3).toBe('JPN');
  });

  it('kinds: gap, eligible, none eligible (detail in), first-ranked outside (core only), none', () => {
    expect(partnerLead(P('CHN'), 'CHN', true, net)).toEqual({ kind: 'gap' });
    expect(partnerLead(P('RUS', 'IDN'), 'RUS', false, net)).toEqual({ kind: 'eligible', iso3: 'IDN' });
    expect(partnerLead(P('RUS', 'CHN'), 'RUS', false, net)).toEqual({ kind: 'none_eligible', outside: ['RUS', 'CHN'] });
    expect(partnerLead(null, 'CHN', false, net)).toEqual({ kind: 'first_outside', iso3: 'CHN' });
    expect(partnerLead(null, 'AUS', false, net)).toEqual({ kind: 'eligible', iso3: 'AUS' });
    expect(partnerLead([], null, false, net)).toEqual({ kind: 'none' });
  });

  it('words: a member is the best-placed partner; outside suppliers are named as outside the trusted network', () => {
    expect(partnerLeadWords(partnerLead(USA['Wind|Refining'], 'DEU', false, net), names)).toBe('Best-placed partner: Germany');
    expect(partnerLeadWords(partnerLead(LKA['Coal|Assembly'], 'RUS', false, net), names)).toBe(
      'No eligible trusted partner identified; the leading suppliers, Russia and China, are outside the trusted network',
    );
    expect(partnerLeadWords(partnerLead(null, 'CHN', false, net), names)).toBe('First-ranked supplier: China, outside the trusted network');
    expect(partnerLeadWords({ kind: 'gap' }, names)).toBe('No credible allied solution yet');
    expect(partnerLeadWords({ kind: 'none' }, names)).toBeNull();
  });
});

describe('words and the cached network', () => {
  it('noEligibleWords and joinNames', () => {
    expect(noEligibleWords(['China'])).toBe('No eligible trusted partner identified; the leading supplier, China, is outside the trusted network.');
    expect(noEligibleWords(['Russia', 'Kazakhstan', 'China'])).toBe(
      'No eligible trusted partner identified; the leading suppliers, Russia, Kazakhstan and China, are outside the trusted network.',
    );
    expect(noEligibleWords([])).toBe('No eligible trusted partner identified.');
    expect(joinNames([])).toBe('');
    expect(joinNames(['A', 'B'])).toBe('A and B');
  });

  it('networkOf builds the network once per country table, with the same members as trustedNetwork', () => {
    const a = networkOf(COUNTRIES);
    expect(networkOf(COUNTRIES)).toBe(a);
    expect(a.members.map((m) => m.iso3)).toEqual(net.members.map((m) => m.iso3));
  });
});
