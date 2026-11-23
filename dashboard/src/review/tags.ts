// The expertise tag taxonomy of the review system (docs/QC_SYSTEM_DESIGN.md 4.3, 5 step 3): the sector,
// stage, country and method tags a reviewer profile states and a case is labelled with, their labels, and
// the pure helpers that normalise free-ish input to canonical tags and test a profile against a case.
// One vocabulary for every review module (assign.ts, aggregate.ts, the profile form, the admin dashboard).
// Pure: no IO, no React. Stage tags are lib/stages.ts SUPPLY_STAGES; sector tags are the Explorer's
// technology slugs (lib/stages.ts techSlug of the posture technology names) plus two cross-cutting groups.

import type { CaseDefinition, ExpertiseMatch, ReviewerProfile } from './model.ts';
import { SUPPLY_STAGES, techSlug, type SupplyStage } from '../lib/stages.ts';

export type TagKind = 'sector' | 'country' | 'stage' | 'method';

export const SECTOR_TAGS = [
  'batteries',
  'coal',
  'electric-grid',
  'electric-vehicles',
  'gas',
  'geothermal',
  'green-hydrogen',
  'nuclear',
  'oil',
  'solar',
  'wind',
  'critical-minerals',
  'semiconductors',
] as const;
export type SectorTag = (typeof SECTOR_TAGS)[number];

export const SECTOR_TAG_LABEL: Readonly<Record<SectorTag, string>> = {
  batteries: 'Batteries',
  coal: 'Coal',
  'electric-grid': 'Electric Grid',
  'electric-vehicles': 'Electric Vehicles',
  gas: 'Gas',
  geothermal: 'Geothermal',
  'green-hydrogen': 'Green Hydrogen',
  nuclear: 'Nuclear',
  oil: 'Oil',
  solar: 'Solar',
  wind: 'Wind',
  'critical-minerals': 'Critical minerals',
  semiconductors: 'Semiconductors',
};

export const STAGE_TAGS: readonly SupplyStage[] = SUPPLY_STAGES;
export type StageTag = SupplyStage;

export const METHOD_TAGS = [
  'trade_data',
  'production_capacity_data',
  'industrial_policy',
  'supply_chain_security',
  'investment_finance',
  'development_finance',
  'geopolitics_alliances',
  'energy_systems',
  'input_output_modelling',
  'indices_statistics',
] as const;
export type MethodTag = (typeof METHOD_TAGS)[number];

export const METHOD_TAG_LABEL: Readonly<Record<MethodTag, string>> = {
  trade_data: 'Trade data',
  production_capacity_data: 'Production and capacity data',
  industrial_policy: 'Industrial policy',
  supply_chain_security: 'Supply-chain security',
  investment_finance: 'Investment and finance',
  development_finance: 'Development finance',
  geopolitics_alliances: 'Geopolitics and alliances',
  energy_systems: 'Energy systems',
  input_output_modelling: 'Input-output modelling',
  indices_statistics: 'Indices and statistics',
};

/** The self-rating (1-5) assumed by helpers that take no campaign rule (method cards, the admin's weighting). */
export const DEFAULT_MIN_CONFIDENCE = 3;

const SECTOR_ALIAS: Readonly<Record<string, SectorTag>> = {
  ev: 'electric-vehicles',
  evs: 'electric-vehicles',
  'electric-vehicle': 'electric-vehicles',
  grid: 'electric-grid',
  'grid-components': 'electric-grid',
  hydrogen: 'green-hydrogen',
  'clean-hydrogen': 'green-hydrogen',
  battery: 'batteries',
  'critical-mineral': 'critical-minerals',
  minerals: 'critical-minerals',
  'natural-gas': 'gas',
  pv: 'solar',
  'solar-pv': 'solar',
  semiconductor: 'semiconductors',
  chips: 'semiconductors',
};

const ISO3_RE = /^[A-Z]{3}$/;

/** A canonical sector tag from a slug, a display name ("Electric Vehicles") or a common alias; null otherwise. */
export function normaliseSectorTag(raw: unknown): SectorTag | null {
  if (typeof raw !== 'string' || !raw.trim()) return null;
  const s = techSlug(raw.replace(/_/g, ' '));
  if ((SECTOR_TAGS as readonly string[]).includes(s)) return s as SectorTag;
  return SECTOR_ALIAS[s] ?? null;
}

/** A canonical stage tag ("refining", "Refining & Processing" -> "Refining"); null otherwise. */
export function normaliseStageTag(raw: unknown): StageTag | null {
  if (typeof raw !== 'string' || !raw.trim()) return null;
  const s = raw.trim().toLowerCase();
  return SUPPLY_STAGES.find((x) => x.toLowerCase() === s || s.startsWith(`${x.toLowerCase()} `)) ?? null;
}

/** An upper-case ISO3 code; null when the input is not three letters. */
export function normaliseCountryTag(raw: unknown): string | null {
  if (typeof raw !== 'string') return null;
  const s = raw.trim().toUpperCase();
  return ISO3_RE.test(s) ? s : null;
}

/** A canonical method tag ("Trade data", "trade-data" -> "trade_data"); null otherwise. */
export function normaliseMethodTag(raw: unknown): MethodTag | null {
  if (typeof raw !== 'string' || !raw.trim()) return null;
  const s = raw.trim().toLowerCase().replace(/[\s-]+/g, '_');
  return (METHOD_TAGS as readonly string[]).includes(s) ? (s as MethodTag) : null;
}

/** The normaliser of a tag kind. */
export function normaliseTag(kind: TagKind, raw: unknown): string | null {
  switch (kind) {
    case 'sector':
      return normaliseSectorTag(raw);
    case 'stage':
      return normaliseStageTag(raw);
    case 'country':
      return normaliseCountryTag(raw);
    case 'method':
      return normaliseMethodTag(raw);
  }
}

/** The display label of a canonical tag (ISO3 codes are their own label). */
export function tagLabel(kind: TagKind, tag: string): string {
  if (kind === 'sector') return SECTOR_TAG_LABEL[tag as SectorTag] ?? tag;
  if (kind === 'method') return METHOD_TAG_LABEL[tag as MethodTag] ?? tag;
  return tag;
}

/** The profile confidence key of a tag: "sector:batteries", "country:KOR", "stage:Refining", "method:trade_data". */
export const confidenceKey = (kind: TagKind, tag: string): string => `${kind}:${tag}`;

function uniqueNormalised(kind: TagKind, xs: unknown): string[] {
  const out: string[] = [];
  for (const x of Array.isArray(xs) ? xs : []) {
    const t = normaliseTag(kind, x);
    if (t && !out.includes(t)) out.push(t);
  }
  return out;
}

const PLURAL: Readonly<Record<TagKind, 'sectors' | 'countries' | 'stages' | 'methods'>> = { sector: 'sectors', country: 'countries', stage: 'stages', method: 'methods' };
const KINDS: readonly TagKind[] = ['sector', 'country', 'stage', 'method'];

/**
 * A profile with every tag list normalised (unknown tags dropped, duplicates removed) and the confidence
 * map re-keyed to canonical keys with ratings clamped to 1-5 (non-numeric ratings dropped).
 */
export function normaliseProfile(p: ReviewerProfile): ReviewerProfile {
  const confidence: Record<string, number> = {};
  for (const [k, v] of Object.entries(p.confidence ?? {})) {
    const i = k.indexOf(':');
    if (i < 0 || typeof v !== 'number' || !Number.isFinite(v)) continue;
    const kind = k.slice(0, i) as TagKind;
    if (!KINDS.includes(kind)) continue;
    const t = normaliseTag(kind, k.slice(i + 1));
    if (t) confidence[confidenceKey(kind, t)] = Math.min(5, Math.max(1, Math.round(v)));
  }
  return {
    ...p,
    sectors: uniqueNormalised('sector', p.sectors),
    countries: uniqueNormalised('country', p.countries),
    stages: uniqueNormalised('stage', p.stages),
    methods: uniqueNormalised('method', p.methods),
    confidence,
  };
}

type TagHolder = Pick<ReviewerProfile, 'sectors' | 'countries' | 'stages' | 'methods' | 'confidence'>;

/** The self-rating of a listed tag (null when the tag is not listed or listed without a rating). */
export function tagConfidence(p: TagHolder, kind: TagKind, tag: string): number | null {
  const list = (p[PLURAL[kind]] ?? []) as string[];
  if (!list.includes(tag)) return null;
  const v = p.confidence?.[confidenceKey(kind, tag)];
  return typeof v === 'number' && Number.isFinite(v) ? v : null;
}

/** Whether a tag is listed (any rating). */
export const hasTag = (p: TagHolder, kind: TagKind, tag: string): boolean => ((p[PLURAL[kind]] ?? []) as string[]).includes(tag);

/**
 * Whether a tag counts as expertise: listed AND rated at or above `minConfidence`. A listed tag without a
 * rating does not count (the profile form asks for one); it still makes a case adjacent.
 */
export function isExpertIn(p: TagHolder, kind: TagKind, tag: string, minConfidence: number): boolean {
  const c = tagConfidence(p, kind, tag);
  return c !== null && c >= minConfidence;
}

/** The tags of a case, normalised (sector and country groups, the stage from `stage` or `key.stage`). */
export function caseTags(c: Pick<CaseDefinition, 'sector_groups' | 'country_groups' | 'stage' | 'key'>): { sectors: string[]; countries: string[]; stage: string | null } {
  return {
    sectors: uniqueNormalised('sector', c.sector_groups),
    countries: uniqueNormalised('country', c.country_groups),
    stage: normaliseStageTag(c.stage ?? c.key?.stage ?? null),
  };
}

/**
 * How a profile's stated expertise matches a case (design 5 step 3):
 * - `in`: a sector tag of the case rated >= min AND (a country tag rated >= min OR the case stage rated >= min).
 *   A case with no sector groups (a partnership) is `in` on a rated country match alone.
 * - `adjacent`: not `in`, but a sector or country tag of the case is listed (any rating), or the case has
 *   neither sector nor country groups (method-wide) and the profile rates a method tag >= min.
 * - `outside`: otherwise.
 */
export function profileMatchesCase(p: TagHolder, c: Pick<CaseDefinition, 'sector_groups' | 'country_groups' | 'stage' | 'key'>, minConfidence: number): ExpertiseMatch {
  const t = caseTags(c);
  const sectorIn = t.sectors.some((s) => isExpertIn(p, 'sector', s, minConfidence));
  const countryIn = t.countries.some((s) => isExpertIn(p, 'country', s, minConfidence));
  const stageIn = t.stage !== null && isExpertIn(p, 'stage', t.stage, minConfidence);
  if (t.sectors.length > 0 && sectorIn && (countryIn || stageIn)) return 'in';
  if (t.sectors.length === 0 && t.countries.length > 0 && countryIn) return 'in';
  const sectorListed = t.sectors.some((s) => hasTag(p, 'sector', s));
  const countryListed = t.countries.some((s) => hasTag(p, 'country', s));
  if (sectorListed || countryListed) return 'adjacent';
  if (t.sectors.length === 0 && t.countries.length === 0 && (p.methods ?? []).some((m) => isExpertIn(p, 'method', m, minConfidence))) return 'adjacent';
  return 'outside';
}

/**
 * The reporter's own confidence (0-1) for a case or a reported data point: the highest self-rating among
 * the matching sector / country / stage tags, divided by 5; null when no tag matches.
 */
export function reporterConfidence(p: TagHolder, tags: { sectors?: string[]; countries?: string[]; stage?: string | null }): number | null {
  const rs: number[] = [];
  for (const s of tags.sectors ?? []) {
    const c = tagConfidence(p, 'sector', s);
    if (c !== null) rs.push(c);
  }
  for (const s of tags.countries ?? []) {
    const c = tagConfidence(p, 'country', s);
    if (c !== null) rs.push(c);
  }
  if (tags.stage) {
    const c = tagConfidence(p, 'stage', tags.stage);
    if (c !== null) rs.push(c);
  }
  return rs.length ? Math.max(...rs) / 5 : null;
}
