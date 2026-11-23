// Case assignment (review/assign.ts; design 5 step 3): the expertise filter, anchors, determinism, the
// Korean-batteries rule, too few matches, the cap, self-selection and method-card relevance.

import { describe, expect, it } from 'vitest';
import type { Campaign, CaseDefinition, ReviewerProfile } from './model.ts';
import { assignCases, hashString, methodCardRelevance, seededRandom } from './assign.ts';

const profile = (p: Partial<ReviewerProfile>): ReviewerProfile => ({
  reviewer_id: 'rev-1',
  anonymous: true,
  countries: [],
  sectors: [],
  stages: [],
  methods: [],
  confidence: {},
  familiarity_neis: 1,
  familiarity_explorer: 1,
  advised_on: [],
  created_at: '2027-01-01T00:00:00Z',
  ...p,
});

let n = 0;
function kase(sectors: string[], countries: string[], extra: Partial<CaseDefinition> = {}): CaseDefinition {
  n++;
  const id = extra.case_id ?? `CASE_${String(n).padStart(3, '0')}`;
  return {
    case_id: id,
    kind: 'posture_cell',
    key: { iso3: countries[0] ?? 'USA', tech: 'Batteries', stage: 'Components' },
    title: id,
    question: 'q',
    sector_groups: sectors,
    country_groups: countries,
    stage: 'Components',
    assignment_group: sectors[0] ?? 'method',
    anchor: false,
    why_selected: 'test',
    explorer_path: '/countries/USA',
    context_fields: ['need'],
    reveal_fields: ['choice'],
    ...extra,
  };
}

const CASES: CaseDefinition[] = [
  kase(['batteries'], ['KOR'], { case_id: 'CASE_001' }),
  kase(['batteries'], ['KOR'], { case_id: 'CASE_002', stage: 'Refining', key: { iso3: 'KOR', tech: 'Batteries', stage: 'Refining' } }),
  kase(['batteries'], ['KOR'], { case_id: 'CASE_003', stage: 'Assembly' }),
  kase(['batteries'], ['JPN'], { case_id: 'CASE_004', anchor: true }),
  kase(['solar'], ['IND'], { case_id: 'CASE_005', anchor: true }),
  kase(['nuclear'], ['USA'], { case_id: 'CASE_006' }),
  kase(['batteries'], ['KOR'], { case_id: 'CASE_007', anchor: true }),
  kase([], [], { case_id: 'CASE_008', kind: 'sector', key: { tech: 'Solar' }, stage: null }),
  kase(['wind'], ['DEU'], { case_id: 'CASE_009', anchor: true }),
];

const campaign = (a: Partial<Campaign['assignment']> = {}, c: Partial<Campaign> = {}): Campaign => ({
  campaign_id: 'test_round',
  name: 'Test',
  status: 'active',
  review_types: ['case'],
  case_ids: CASES.map((x) => x.case_id),
  anchor_cases: CASES.filter((x) => x.anchor).map((x) => x.case_id),
  target_reviews_per_anchor: 4,
  assignment: { assigned_per_reviewer: { min: 2, max: 4 }, anchors_per_reviewer: 1, out_of_specialty: 'none', allow_self_selected: true, min_domain_confidence: 3, ...a },
  ...c,
});

const korBatteries = profile({ reviewer_id: 'kor-batt', sectors: ['batteries'], countries: ['KOR'], confidence: { 'sector:batteries': 5, 'country:KOR': 5 } });
const byId = new Map(CASES.map((c) => [c.case_id, c]));

describe('assignCases', () => {
  it('assigns only in-expertise cases to a Korean-batteries expert (no unrelated case, no methodology-wide case)', () => {
    const a = assignCases(korBatteries, campaign(), CASES);
    expect(a.length).toBeGreaterThanOrEqual(2);
    expect(a.length).toBeLessThanOrEqual(4);
    for (const x of a) {
      const c = byId.get(x.case_id)!;
      expect(c.sector_groups).toContain('batteries');
      expect(c.country_groups).toContain('KOR');
      expect(x.match).toBe('in');
    }
    expect(a.map((x) => x.case_id)).not.toContain('CASE_008');
  });

  it('gives at least one anchor, the in-expertise anchor first', () => {
    const a = assignCases(korBatteries, campaign(), CASES);
    expect(a[0]).toEqual({ case_id: 'CASE_007', reason: 'anchor', match: 'in', optional: false });
    expect(a.filter((x) => x.reason === 'anchor')).toHaveLength(1);
  });

  it('prefers the anchor with the fewest planned reviews among equally matched anchors', () => {
    const p = profile({ reviewer_id: 'batt-any', sectors: ['batteries'], stages: ['Components'], confidence: { 'sector:batteries': 4, 'stage:Components': 4 } });
    // CASE_004 (JPN) and CASE_007 (KOR) are both in (batteries + Components)
    const a1 = assignCases(p, campaign(), CASES, { plannedCounts: { CASE_007: 5, CASE_004: 1 } });
    expect(a1.find((x) => x.reason === 'anchor')?.case_id).toBe('CASE_004');
    const a2 = assignCases(p, campaign(), CASES, { plannedCounts: { CASE_007: 0, CASE_004: 3 } });
    expect(a2.find((x) => x.reason === 'anchor')?.case_id).toBe('CASE_007');
  });

  it('moves an anchor that has reached its target behind one still below it, even a better-matched one', () => {
    // for the KOR-batteries expert CASE_007 is in and CASE_004 (JPN) adjacent; CASE_007 already has its 4 reviews
    const a = assignCases(korBatteries, campaign(), CASES, { plannedCounts: { CASE_007: 4, CASE_004: 0 } });
    expect(a.find((x) => x.reason === 'anchor')).toEqual({ case_id: 'CASE_004', reason: 'anchor', match: 'adjacent', optional: false });
  });

  it('is deterministic for a reviewer and differs between reviewers only in tie-breaks', () => {
    const a = assignCases(korBatteries, campaign(), CASES);
    const b = assignCases({ ...korBatteries }, campaign(), [...CASES]);
    expect(b).toEqual(a);
    const ids = new Set<string>();
    for (let i = 0; i < 20; i++) for (const x of assignCases({ ...korBatteries, reviewer_id: `r${i}` }, campaign(), CASES)) ids.add(x.case_id);
    // other reviewers with the same expertise rotate through the same in-expertise pool, never outside it
    for (const id of ids) expect(byId.get(id)!.country_groups).toContain('KOR');
  });

  it('never assigns more than max, whatever the options', () => {
    const a = assignCases(korBatteries, campaign({ assigned_per_reviewer: { min: 1, max: 2 }, out_of_specialty: 'one' }), CASES, { selfSelected: 'CASE_006' });
    expect(a.length).toBeLessThanOrEqual(2);
    const many = assignCases(korBatteries, campaign({ assigned_per_reviewer: { min: 0, max: 0 } }), CASES);
    expect(many).toEqual([]);
  });

  it('adds an optional case beyond the specialty when the campaign asks, adjacent first and marked honestly', () => {
    const a = assignCases(korBatteries, campaign({ out_of_specialty: 'optional', assigned_per_reviewer: { min: 2, max: 4 } }), CASES);
    const out = a.filter((x) => x.reason === 'outside');
    expect(out).toHaveLength(1);
    expect(out[0].optional).toBe(true);
    expect(out[0].match).toBe('adjacent');
    expect(byId.get(out[0].case_id)!.sector_groups).toContain('batteries');
  });

  it('falls back to adjacent cases when too few match, and says so', () => {
    const jpn = profile({ reviewer_id: 'jpn-solar', sectors: ['solar'], countries: ['JPN'], confidence: { 'sector:solar': 5, 'country:JPN': 5 } });
    const a = assignCases(jpn, campaign(), CASES);
    expect(a.length).toBe(2);
    expect(a.every((x) => x.match === 'adjacent')).toBe(true);
    // only cases that share a tag: CASE_004 (JPN) and CASE_005 (solar)
    expect(a.map((x) => x.case_id).sort()).toEqual(['CASE_004', 'CASE_005']);
  });

  it('returns nothing rather than unrelated cases for a profile with no stated expertise', () => {
    expect(assignCases(profile({ reviewer_id: 'blank' }), campaign(), CASES)).toEqual([]);
  });

  it('adds the self-selected case when allowed (optional), ignores it otherwise', () => {
    const a = assignCases(korBatteries, campaign({ assigned_per_reviewer: { min: 2, max: 4 } }), CASES, { selfSelected: 'CASE_006' });
    expect(a.at(-1)).toEqual({ case_id: 'CASE_006', reason: 'self_selected', match: 'outside', optional: true });
    const b = assignCases(korBatteries, campaign({ allow_self_selected: false }), CASES, { selfSelected: 'CASE_006' });
    expect(b.map((x) => x.case_id)).not.toContain('CASE_006');
  });

  it('keeps to the campaign scope', () => {
    const a = assignCases(korBatteries, campaign({}, { case_ids: ['CASE_001', 'CASE_005'], anchor_cases: ['CASE_005'] }), CASES);
    expect(a.map((x) => x.case_id)).toEqual(['CASE_001']);
  });
});

describe('determinism helpers', () => {
  it('hashString and seededRandom are pure', () => {
    expect(hashString('abc')).toBe(hashString('abc'));
    expect(hashString('abc')).not.toBe(hashString('abd'));
    const r1 = seededRandom(42);
    const r2 = seededRandom(42);
    const xs = [r1(), r1(), r1()];
    expect([r2(), r2(), r2()]).toEqual(xs);
    for (const x of xs) expect(x >= 0 && x < 1).toBe(true);
  });
});

describe('methodCardRelevance', () => {
  it('is in for a rated method tag, outside otherwise (still answerable)', () => {
    const p = profile({ methods: ['trade_data'], confidence: { 'method:trade_data': 4 } });
    expect(methodCardRelevance(p, { method_tags: ['trade_data', 'indices_statistics'] })).toBe('in');
    expect(methodCardRelevance(p, { method_tags: ['development_finance'] })).toBe('outside');
    expect(methodCardRelevance(korBatteries, { method_tags: ['trade_data'] })).toBe('outside');
  });
});
