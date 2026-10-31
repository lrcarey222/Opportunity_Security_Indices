// The shared vocabulary (content/vocabulary.ts; the NEIS copy refresh, 2026-10-30): the user's wording where the
// user gave it, a word for every code the data carry, no consulting language, and the central label files read it.

import { describe, expect, it } from 'vitest';
import { AIB_CLASSES, AIB_SCENARIO_IDS } from '../aib/contract.ts';
import { ROLE_ORDER } from '../aib/derive/roles.ts';
import { TIER_LABEL } from '../aib/derive/tiers.ts';
import { ALIGNMENT_CLASS_LABEL, ALIGNMENT_LABEL, FENCE_LABEL, RATIONALE_LABEL, VERB_LABEL, VERB_ORDER, VERB_SUBTITLE } from '../posture/grammar.ts';
import { YARD_LABEL } from '../posture/yards.ts';
import * as V from './vocabulary.ts';

/** Every string the module exports, however deeply nested. */
function strings(v: unknown): string[] {
  if (typeof v === 'string') return [v];
  if (Array.isArray(v)) return v.flatMap(strings);
  if (v && typeof v === 'object') return Object.values(v).flatMap(strings);
  return [];
}

describe('the shared vocabulary', () => {
  it("keeps the user's wording: the six choice names, the three yards, the canonical AIB explainer", () => {
    expect(VERB_ORDER.map((v) => V.CHOICE_NAME[v])).toEqual(['Secure', 'Preserve', 'Catch Up', 'Partner', 'Accelerate', 'Watch']);
    expect(V.YARD_DEFINITION).toEqual({
      small: 'Capabilities where foreign control or concentrated dependence could create an unacceptable security or resilience risk.',
      medium: 'Capabilities worth building or preserving because they shape competitiveness, industrial learning or future technological leadership.',
      large: 'Capabilities where affordable and diversified access generally matters more than domestic production.',
    });
    expect(V.YARD_AND_FENCE).toEqual({ small: 'Small yard / High fence', medium: 'Medium yard / medium, permeable fence', large: 'Large yard / Low fence' });
    expect(V.YARD_CHOICES.medium).toBe('Preserve, Catch Up, Partner, and Accelerate at Deployment & Use');
    expect(V.AIB_EXPLAINER).toMatch(/^The Allied Industrial Base is a strategy for managed interdependence\. No country should try to make everything\./);
    expect(V.AIB_EXPLAINER_SHORT).toBe('Build what matters. Specialize where allies are strongest. Keep enough redundancy that no single supplier can hold the system at risk.');
    expect(V.FRAMING.noCountryMakesEverything).toBe('No country needs to make everything.');
    expect(V.FRAMING.whatNotToBuild).toBe('A strong industrial strategy is as much about deciding what not to build as deciding what to build.');
    expect(V.AIB_SIDES.supply.question).toBe('Who has the strongest capability?');
    expect(V.AIB_SIDES.demand.question).toBe('Which countries need a partner?');
  });

  it('has a word for every code the data carry: choices, AIB classes, scenarios, compact roles and tiers', () => {
    for (const v of VERB_ORDER) for (const m of [V.CHOICE_ACTION, V.CHOICE_DEFINITION, V.CHOICE_PROSE]) expect(m[v], v).toBeTruthy();
    for (const c of AIB_CLASSES) expect(V.AIB_CLASS_NAME[c] && V.AIB_CLASS_DEFINITION[c], c).toBeTruthy();
    for (const s of AIB_SCENARIO_IDS) expect(V.SCENARIO_NAME[s] && V.SCENARIO_DEFINITION[s] && V.SCENARIO_METHOD_NAME[s], s).toBeTruthy();
    for (const r of ROLE_ORDER) expect(V.COMPACT_ROLE_DEFINITION[r], r).toBeTruthy();
    for (const t of ['core coordinating member', 'critical supply partner', 'prospective host', 'associate partner', 'critical external dependency', 'external dependency', 'development host']) {
      expect(TIER_LABEL[t], t).toBeTruthy();
      expect(V.COMPACT_TIER_DEFINITION[t], t).toBeTruthy();
    }
    // the all-caps codes are never the words
    expect(Object.values(V.AIB_CLASS_NAME).every((w) => w !== w.toUpperCase())).toBe(true);
    // the decoupling stress test names the supplier the data remove
    expect(V.SCENARIO_DEFINITION.decoupling).toContain('China');
  });

  it('the plain term differs from the technical one, and no exported sentence uses consulting language', () => {
    expect(V.TERMINOLOGY.length).toBeGreaterThanOrEqual(30);
    for (const t of V.TERMINOLOGY) expect(t.plain.toLowerCase(), t.technical).not.toBe(t.technical.toLowerCase());
    expect(V.TERMS.node).toBe('supply-chain step');
    expect(V.TERMS.coverage).toBe('share of allied demand that can be met');
    const all = Object.entries(V)
      // the terminology table names the technical words it replaces ("optimized coverage"): only its plain side counts
      .filter(([k]) => k !== 'AVOID_WORDS' && k !== 'AVOID_RE' && k !== 'TERMINOLOGY')
      .flatMap(([, v]) => strings(v))
      .concat(V.TERMINOLOGY.map((t) => t.plain));
    expect(all.length).toBeGreaterThan(150);
    for (const s of all) expect(s).not.toMatch(V.AVOID_RE);
    for (const w of ['optimize', 'leverages', 'unlocking', 'the model names']) expect(w).toMatch(V.AVOID_RE);
    // missing data is uncertainty, never a strategic call
    expect(V.STATE.notAssessed).toMatch(/^Not assessed/);
    expect(V.WATCH_DEPLOYMENT_DEFINITION).not.toMatch(/on track/);
  });

  it('the central label files read it, so every page shows the same words', () => {
    expect(VERB_LABEL).toBe(V.CHOICE_NAME);
    expect(VERB_SUBTITLE).toBe(V.CHOICE_ACTION);
    expect(FENCE_LABEL).toBe(V.FENCE_NAME);
    expect(RATIONALE_LABEL).toBe(V.RATIONALE_NAME);
    expect(ALIGNMENT_CLASS_LABEL).toBe(V.ALIGNMENT_NAME);
    expect(ALIGNMENT_LABEL.deployment_gap).toBe(V.ALIGNMENT_DEPLOYMENT_GAP);
    expect(YARD_LABEL).toBe(V.YARD_NAME);
    expect(RATIONALE_LABEL).toEqual({ operational_control: 'Control-system risk', coercive_chokepoint: 'Chokepoint risk', systemic_resilience: 'System resilience' });
    expect(ALIGNMENT_CLASS_LABEL.under_prioritized).toBe('Needs more attention');
  });
});
