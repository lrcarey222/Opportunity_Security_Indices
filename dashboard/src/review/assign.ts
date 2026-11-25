// Case assignment (docs/QC_SYSTEM_DESIGN.md 5 step 3): a deterministic list of cases for one reviewer
// from their profile, the campaign rules and a seed derived from the reviewer id. In-expertise cases first
// (sector tag AND (country OR stage) rated >= min_domain_confidence), at least the campaign's anchors
// (fewest planned reviews first), an optional outside case, a self-selected slot when allowed; never more
// than assigned_per_reviewer.max. When too few cases match, adjacent cases fill the gap and say so: the
// `match` of every assignment is the honest one. Pure: no Math.random, no Date.

import type { Assignment, Campaign, CaseDefinition, ExpertiseMatch, MethodCard, ReviewerProfile } from './model.ts';
import { DEFAULT_MIN_CONFIDENCE, isExpertIn, normaliseProfile, profileMatchesCase } from './tags.ts';

/** FNV-1a 32-bit hash of a string (the seed of a reviewer). */
export function hashString(s: string): number {
  let h = 0x811c9dc5;
  for (let i = 0; i < s.length; i++) {
    h ^= s.charCodeAt(i);
    h = Math.imul(h, 0x01000193) >>> 0;
  }
  return h >>> 0;
}

/** mulberry32: a small deterministic PRNG in [0, 1). */
export function seededRandom(seed: number): () => number {
  let a = seed >>> 0;
  return () => {
    a = (a + 0x6d2b79f5) >>> 0;
    let t = a;
    t = Math.imul(t ^ (t >>> 15), t | 1);
    t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
}

export interface AssignOptions {
  /** Reviews already planned per case (other reviewers' assignments): anchors with the fewest come first. */
  plannedCounts?: Readonly<Record<string, number>>;
  /** A case the reviewer picked themselves (used only when the campaign allows it). */
  selfSelected?: string | null;
}

type Scored = { c: CaseDefinition; match: ExpertiseMatch; tiebreak: number };

/** A case labelled with no sector and no country group: a methodology-wide case. */
const isMethodWide = (c: CaseDefinition): boolean => (c.sector_groups ?? []).length === 0 && (c.country_groups ?? []).length === 0;

const MATCH_RANK: Readonly<Record<ExpertiseMatch, number>> = { in: 0, adjacent: 1, outside: 2 };

/**
 * The reviewer's assignments, in the order they should be worked: anchors, in-expertise, adjacent fill,
 * the outside case, the self-selected case. Deterministic for a (reviewer_id, campaign_id, inputs).
 */
export function assignCases(profile: ReviewerProfile, campaign: Campaign, cases: readonly CaseDefinition[], opts: AssignOptions = {}): Assignment[] {
  const p = normaliseProfile(profile);
  const rules = campaign.assignment;
  const max = Math.max(0, rules.assigned_per_reviewer.max);
  const min = Math.min(max, Math.max(0, rules.assigned_per_reviewer.min));
  const minConf = rules.min_domain_confidence;
  const planned = opts.plannedCounts ?? {};
  const inScope = campaign.case_ids.length ? new Set(campaign.case_ids) : null;
  const anchors = new Set(campaign.anchor_cases);
  const rand = seededRandom(hashString(`${p.reviewer_id}|${campaign.campaign_id}`));

  // a stable order before the random tiebreak: the catalogue order, filtered to the campaign
  const scored: Scored[] = cases.filter((c) => !inScope || inScope.has(c.case_id)).map((c) => ({ c, match: profileMatchesCase(p, c, minConf), tiebreak: rand() }));
  const byPref = (a: Scored, b: Scored) => MATCH_RANK[a.match] - MATCH_RANK[b.match] || (planned[a.c.case_id] ?? 0) - (planned[b.c.case_id] ?? 0) || a.tiebreak - b.tiebreak;

  const out: Assignment[] = [];
  const used = new Set<string>();
  const add = (s: Scored, reason: Assignment['reason'], optional: boolean) => {
    out.push({ case_id: s.c.case_id, reason, match: s.match, optional });
    used.add(s.c.case_id);
  };

  // optional slots are reserved only while the required ones still reach the minimum
  const selfId = rules.allow_self_selected && opts.selfSelected ? scored.find((s) => s.c.case_id === opts.selfSelected) ?? null : null;
  const wantOutside = rules.out_of_specialty !== 'none';
  const reserve = (wantOutside ? 1 : 0) + (selfId ? 1 : 0);
  const requiredCap = Math.max(min, max - reserve);

  // 1. anchors the reviewer can judge: those still below the campaign's target first, then in before adjacent,
  //    then the fewest planned reviews
  const target = Math.max(1, campaign.target_reviews_per_anchor);
  const full = (s: Scored) => ((planned[s.c.case_id] ?? 0) >= target ? 1 : 0);
  const anchorPool = scored
    .filter((s) => anchors.has(s.c.case_id) && s.match !== 'outside' && s.c.case_id !== selfId?.c.case_id)
    .sort((a, b) => full(a) - full(b) || byPref(a, b));
  for (const s of anchorPool) {
    if (out.length >= Math.min(rules.anchors_per_reviewer, requiredCap)) break;
    add(s, 'anchor', false);
  }

  // 2. in-expertise cases (anchors already taken excluded), fewest planned reviews first
  const inPool = scored.filter((s) => s.match === 'in' && !used.has(s.c.case_id) && s.c.case_id !== selfId?.c.case_id).sort(byPref);
  for (const s of inPool) {
    if (out.length >= requiredCap) break;
    add(s, 'assigned', false);
  }

  // 3. too few matches: fill to the minimum with adjacent cases, marked adjacent
  if (out.length < min) {
    const adj = scored.filter((s) => s.match === 'adjacent' && !used.has(s.c.case_id) && s.c.case_id !== selfId?.c.case_id).sort(byPref);
    for (const s of adj) {
      if (out.length >= min) break;
      add(s, 'assigned', false);
    }
  }

  // 4. one case beyond the stated specialty: an adjacent case first (it shares a sector or country tag), else
  //    an outside one. Method-wide cases (no sector and no country groups) are never handed to a reviewer
  //    without rated method expertise (design 5 step 3: a Korean-batteries expert gets no methodology-wide case).
  if (wantOutside && out.length < max) {
    const pool = scored
      .filter((s) => s.match !== 'in' && !used.has(s.c.case_id) && s.c.case_id !== selfId?.c.case_id && !(s.match === 'outside' && isMethodWide(s.c)))
      .sort(byPref);
    if (pool.length) add(pool[0], 'outside', rules.out_of_specialty === 'optional');
  }

  // 5. the self-selected case
  if (selfId && !used.has(selfId.c.case_id) && out.length < max) add(selfId, 'self_selected', true);

  return out.slice(0, max);
}

/** Whether a method card is within the reviewer's stated method expertise ('outside' cards stay answerable). */
export function methodCardRelevance(profile: ReviewerProfile, card: Pick<MethodCard, 'method_tags'>, minConfidence = DEFAULT_MIN_CONFIDENCE): 'in' | 'outside' {
  const p = normaliseProfile(profile);
  return card.method_tags.some((t) => isExpertIn(p, 'method', t, minConfidence)) ? 'in' : 'outside';
}
