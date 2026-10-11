// Policy activity (spec_sector.md 4.3 S10): IEA PAMS records as masks. Bit i of tech_mask is
// policy.tags[i]; bit i of stage_mask is STAGE_SLUGS[i]. Counts measure recorded activity, not
// stringency. Pure.

import { STAGE_SLUGS, type SectorPolicyChunk, type SectorPolicyTextChunk, type StageSlug } from '../contract.ts';
import { NA_INT } from '../../engine/numeric.ts';

export interface PolicyRecord {
  id: number;
  geo: number;
  year: number | null;
  status: string | null;
  jurisdiction: string | null;
  type: string | null;
  tech_mask: number;
  tags: string[];
  tech_src: 'iea' | 'text' | null;
  stage_mask: number;
  stages: StageSlug[];
  cross_cutting: boolean;
}

const recCache = new WeakMap<object, PolicyRecord[]>();

export function policyRecords(chunk: SectorPolicyChunk): PolicyRecord[] {
  const hit = recCache.get(chunk);
  if (hit) return hit;
  const t = chunk['policy.pams'];
  const tags = [...(chunk['policy.tags'] as (string | null)[])].map((x) => x ?? '');
  const c = t.cols;
  const out: PolicyRecord[] = [];
  for (let i = 0; i < t.nrow; i++) {
    const tm = c.tech_mask[i];
    const sm = c.stage_mask[i];
    out.push({
      id: c.id[i],
      geo: c.geo[i],
      year: c.year[i] === NA_INT ? null : c.year[i],
      status: c.status[i],
      jurisdiction: c.jurisdiction[i],
      type: c.type[i],
      tech_mask: tm,
      tags: tags.filter((_, b) => (tm >>> b) & 1),
      tech_src: (c.tech_src[i] as PolicyRecord['tech_src']) ?? null,
      stage_mask: sm,
      stages: STAGE_SLUGS.filter((_, b) => (sm >>> b) & 1),
      cross_cutting: c.cross_cutting[i] === 1,
    });
  }
  recCache.set(chunk, out);
  return out;
}

/** The bit of the policy tag of a technology label ("Batteries"), or -1. Matching ignores case and "&"/"and". */
export function tagBit(tags: readonly (string | null)[], techLabel: string): number {
  const norm = (s: string) => s.toLowerCase().replace(/&/g, 'and').replace(/[^a-z0-9]+/g, ' ').trim();
  const want = norm(techLabel);
  const alias: Record<string, string[]> = {
    'electric vehicles': ['electric vehicles', 'evs', 'ev', 'electric mobility'],
    'green hydrogen': ['green hydrogen', 'hydrogen'],
    'electric grid': ['electric grid', 'grid', 'electricity networks', 'networks'],
    batteries: ['batteries', 'battery', 'energy storage', 'storage'],
  };
  const options = alias[want] ?? [want];
  for (const o of options) {
    const i = tags.findIndex((t) => norm(t ?? '') === o);
    if (i >= 0) return i;
  }
  return -1;
}

export interface PolicyScope {
  /** the tech tag bit (or -1 for none) */
  bit: number;
  /** a stage, or null for any stage */
  stage: StageSlug | null;
  /** include economy-wide (cross-cutting) records */
  cross: boolean;
  /** one economy only */
  geo?: number | null;
}

/**
 * Records in scope (4.3 S10): records tagged to the technology (and, with a stage, tagged to that
 * stage), plus, when `cross` is on, the economy-wide (cross-cutting) records, which apply to every
 * stage.
 */
export function scopePolicies(records: readonly PolicyRecord[], scope: PolicyScope): PolicyRecord[] {
  const sb = scope.stage ? STAGE_SLUGS.indexOf(scope.stage) : -1;
  return records.filter((r) => {
    if (scope.geo !== undefined && scope.geo !== null && r.geo !== scope.geo) return false;
    if (scope.cross && r.cross_cutting) return true;
    if (scope.bit < 0 || ((r.tech_mask >>> scope.bit) & 1) !== 1) return false;
    return sb < 0 || ((r.stage_mask >>> sb) & 1) === 1;
  });
}

export type StatusClass = 'in_force' | 'ended' | 'other';

export function statusClass(s: string | null): StatusClass {
  const x = (s ?? '').toLowerCase();
  if (/in force|active|ongoing/.test(x)) return 'in_force';
  if (/ended|expired|superseded|repealed|closed|inactive/.test(x)) return 'ended';
  return 'other';
}

export interface CountryPolicyCount {
  geo: number;
  n: number;
  inForce: number;
  ended: number;
}

export function countsByCountry(records: readonly PolicyRecord[]): CountryPolicyCount[] {
  const m = new Map<number, CountryPolicyCount>();
  for (const r of records) {
    let c = m.get(r.geo);
    if (!c) m.set(r.geo, (c = { geo: r.geo, n: 0, inForce: 0, ended: 0 }));
    c.n++;
    const k = statusClass(r.status);
    if (k === 'in_force') c.inForce++;
    else if (k === 'ended') c.ended++;
  }
  return [...m.values()].sort((a, z) => z.n - a.n || a.geo - z.geo);
}

/** New policies per year in [from, to]; years >= announcedFrom are "announced or planned". */
export function countsByYear(records: readonly PolicyRecord[], from: number, to: number, announcedFrom: number): { year: number; n: number; announced: boolean }[] {
  const out: { year: number; n: number; announced: boolean }[] = [];
  for (let y = from; y <= to; y++) out.push({ year: y, n: 0, announced: y >= announcedFrom });
  for (const r of records) if (r.year !== null && r.year >= from && r.year <= to) out[r.year - from].n++;
  return out;
}

export const INSTRUMENTS = ['Financial incentive', 'Regulation/standard', 'R&D', 'Other'] as const;
export type Instrument = (typeof INSTRUMENTS)[number];

export function instrumentOf(type: string | null): Instrument {
  const x = (type ?? '').toLowerCase();
  if (/financ|fiscal|grant|loan|tax|subsid|incentive|tariff|rebate|guarantee/.test(x)) return 'Financial incentive';
  if (/regulat|standard|mandate|obligation|ban|code|label|permit|target/.test(x)) return 'Regulation/standard';
  if (/research|r&d|rd&d|development and deployment|innovation|demonstration/.test(x)) return 'R&D';
  return 'Other';
}

/** Records per instrument and year (small multiples with one shared y). */
export function countsByInstrument(records: readonly PolicyRecord[], from: number, to: number): Record<Instrument, { year: number; n: number }[]> {
  const out = {} as Record<Instrument, { year: number; n: number }[]>;
  for (const k of INSTRUMENTS) out[k] = Array.from({ length: to - from + 1 }, (_, i) => ({ year: from + i, n: 0 }));
  for (const r of records) if (r.year !== null && r.year >= from && r.year <= to) out[instrumentOf(r.type)][r.year - from].n++;
  return out;
}

/** id -> {title, url} of the policy text chunk. */
export function policyTexts(chunk: SectorPolicyTextChunk | null): Map<number, { title: string | null; url: string | null }> {
  const m = new Map<number, { title: string | null; url: string | null }>();
  if (!chunk) return m;
  const t = chunk['policy.text'];
  for (let i = 0; i < t.nrow; i++) m.set(t.cols.id[i], { title: t.cols.title[i], url: t.cols.url[i] });
  return m;
}

/** Is a URL safe to link (http(s) only)? */
export function safeUrl(u: string | null | undefined): string | null {
  if (!u) return null;
  try {
    const x = new URL(u);
    return x.protocol === 'https:' || x.protocol === 'http:' ? x.href : null;
  } catch {
    return null;
  }
}
