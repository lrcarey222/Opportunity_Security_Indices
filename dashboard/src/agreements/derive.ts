// Partnership agreements: the pure derivations of the Agreements page (/aib/agreements). Filters (URL keys),
// partner rankings by a transparent sort key the reader chooses (no composite score), sector and instrument
// coverage tallies, timeline bins, the global pair table and the network matrix, and the words of the
// strategic gaps and execution gaps (exact numbers from the data, plain words). No React, no IO.
//
// Two rules hold everywhere: existence (announced, signed, in force) is kept apart from delivery
// (implementation underway, operational: only a source that records delivery sets it), and a code mentioned
// in the body text ("mentioned in text") is never shown as established (coded by the source, recorded by
// hand, implied by the agreement type or named in the title).

import {
  DEFAULT_DELIVERY_STATUSES, DEFAULT_SPECIFIC_BASES, DEPTH_CLASSES, SEGMENTS, splitGraded, TITLE_BASIS,
  type AgrAgreement, type AgrExecutionGap, type AgrGap, type AgrPartner, type AgreementsCountry, type AgreementsGlobal, type AgreementsManifest, type AgrVocab, type CodeGrade, type Depth,
  type InstLevel, type Quadrant,
} from './contract.ts';
import { ZERO_FILL } from '../theme/palette.ts';

// ---- words ------------------------------------------------------------------------------------------------

/** The absence caveat: shown wherever an empty result or depth class 0 is shown. */
export const ABSENCE_CAVEAT =
  'No agreement found in these sources. There is no complete register of non-binding MOUs and joint statements, so this does not mean there is no cooperation.';

export const ESTABLISHED = 'established';
export const MENTIONED = 'mentioned in text';
/** The glyphs of the two coding grades (never colour alone). */
export const GRADE_GLYPH: Record<CodeGrade, string> = { specific: '●', text: '○' };
export const GRADE_WORD: Record<CodeGrade, string> = { specific: ESTABLISHED, text: MENTIONED };

/** How each coding basis reads next to a code (provenance: a text-coded or inferred value never reads as sourced). */
export const BASIS_WORD: Readonly<Record<string, string>> = {
  source_coded: 'coded by the source',
  curated: 'recorded by hand from the official text',
  type_implied: 'implied by the agreement type',
  text_title: 'keyword in the title',
  text_body: 'mentioned in the text',
};

/** Aggregates among the parties (the EU is a party to many agreements). */
export const AGGREGATE_NAMES: Readonly<Record<string, string>> = { EUU: 'European Union' };

/**
 * Display labels that replace the manifest's where its label is a bare acronym (the explorer spells every
 * acronym out: components/testing/acronyms.ts), keyed by vocabulary and id.
 */
const LABEL_OVERRIDE: Readonly<Record<string, Readonly<Record<string, string>>>> = {
  agreement_types: { mou: 'Memorandum of understanding (MOU)', fta: 'Free trade or other trade agreement' },
  sectors: { ccs: 'Carbon capture (CCS)', evs: 'Electric vehicles' },
  sources: {
    gsp: 'Global Strategic Partnership Dataset (GSP)',
    desta: 'Design of Trade Agreements (DESTA)',
    iisd: 'International Institute for Sustainable Development (IISD) Trade & Climate Tracker',
  },
};

/** "implementation_underway" -> "implementation underway" */
export const humanize = (id: string): string => id.replace(/_/g, ' ');

export type VocabKind = 'agreement_types' | 'partnership_labels' | 'legal_statuses' | 'implementation_statuses' | 'coding_bases' | 'confidence_levels' | 'instrument_groups';

/** A vocabulary id in words (an override, the manifest's label, or the id itself). */
export function labelOf(v: AgrVocab, kind: VocabKind | 'sectors' | 'instruments' | 'sources', id: string | null | undefined): string {
  if (!id) return '';
  const o = LABEL_OVERRIDE[kind]?.[id];
  if (o) return o;
  if (kind === 'sectors') return v.sectors.find((s) => s.id === id)?.label ?? humanize(id);
  if (kind === 'instruments') return v.instruments.find((s) => s.id === id)?.label ?? humanize(id);
  if (kind === 'sources') return v.sources.find((s) => s.id === id)?.label ?? id;
  const m = (v as unknown as Record<string, Record<string, string> | undefined>)[kind];
  return (m && !Array.isArray(m) ? m[id] : undefined) ?? humanize(id);
}

/** A source's short name for compact lists ("GSP", "DESTA", "Curated"); its label when it has none. */
export function sourceShort(v: AgrVocab, id: string): string {
  return v.sources.find((s) => s.id === id)?.short || id;
}

/** True when a short name is an acronym (shown in an <abbr> with the full name). */
export const isAcronym = (s: string): boolean => /^[A-Z]{2,6}$/.test(s);

/** An economy's name (the manifest's names, the aggregates, else the code). */
export function nameOf(names: Readonly<Record<string, string>>, iso3: string | null | undefined): string {
  if (!iso3) return '';
  return names[iso3] ?? AGGREGATE_NAMES[iso3] ?? iso3;
}

export const plural = (n: number, one: string, many = `${one}s`): string => `${n} ${n === 1 ? one : many}`;

export function listWords(items: readonly string[], conj: 'and' | 'or' = 'and'): string {
  if (items.length <= 1) return items.join('');
  return `${items.slice(0, -1).join(', ')} ${conj} ${items[items.length - 1]}`;
}

/** 0.82 -> "82nd percentile"; null -> null. */
export function percentileWords(x: number | null | undefined): string | null {
  if (x === null || x === undefined || !Number.isFinite(x)) return null;
  const n = Math.round(x * 100);
  const mod100 = n % 100;
  const suffix = mod100 >= 11 && mod100 <= 13 ? 'th' : n % 10 === 1 ? 'st' : n % 10 === 2 ? 'nd' : n % 10 === 3 ? 'rd' : 'th';
  return `${n}${suffix} percentile`;
}

const MONTHS = ['January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December'];

/** A normalised date in words at its precision: "2023", "May 2023", "12 May 2023"; null for none. */
export function dateWords(d: string | null | undefined): string | null {
  if (!d) return null;
  const m = /^(\d{4})(?:-(\d{2})(?:-(\d{2}))?)?$/.exec(d);
  if (!m) return d;
  if (!m[2]) return m[1];
  const month = MONTHS[Number(m[2]) - 1] ?? m[2];
  if (!m[3]) return `${month} ${m[1]}`;
  return `${Number(m[3])} ${month} ${m[1]}`;
}

/** The year of a normalised date. */
export function yearOf(d: string | null | undefined): number | null {
  const m = d ? /^(\d{4})/.exec(d) : null;
  return m ? Number(m[1]) : null;
}

/** A normalised date as a sortable number (2023-05 -> 20230500); -1 for none. */
export function dateKey(d: string | null | undefined): number {
  const m = d ? /^(\d{4})(?:-(\d{2})(?:-(\d{2}))?)?$/.exec(d) : null;
  return m ? Number(`${m[1]}${m[2] ?? '00'}${m[3] ?? '00'}`) : -1;
}

/** The precision of a date in words, for the table ("day", "month", "year only"). */
export const PRECISION_WORD: Readonly<Record<string, string>> = { day: 'exact day', month: 'month only', year: 'year only', none: 'no date' };

// ---- grades and statuses --------------------------------------------------------------------------------

/** The bases at which a SECTOR is established (config specific_bases; a keyword in the title counts). */
export function specificBases(v: AgrVocab): readonly string[] {
  return v.specific_bases?.length ? v.specific_bases : DEFAULT_SPECIFIC_BASES;
}

/** The bases at which an INSTRUMENT is established: vocab.established_instrument_bases, else the specific bases without the title keyword. */
export function instrumentBases(v: AgrVocab): readonly string[] {
  return v.established_instrument_bases?.length ? v.established_instrument_bases : specificBases(v).filter((b) => b !== TITLE_BASIS);
}

export function deliveryStatuses(v: AgrVocab): readonly string[] {
  return v.delivery_statuses?.length ? v.delivery_statuses : DEFAULT_DELIVERY_STATUSES;
}

/** Which codes: sectors or instruments (they are graded with different bases). */
export type CodeKind = 'sectors' | 'instruments';

/** The grade of a code from its basis: specific (established) or text (mentioned in the text); an instrument named only in the title is mentioned. */
export function gradeOf(basis: string, v: AgrVocab, kind: CodeKind = 'sectors'): CodeGrade {
  return (kind === 'instruments' ? instrumentBases(v) : specificBases(v)).includes(basis) ? 'specific' : 'text';
}

/** The best grade per code of a [code, basis] list (specific beats text). */
export function bestGrades(codes: readonly [string, string][], v: AgrVocab, kind: CodeKind = 'sectors'): Map<string, { grade: CodeGrade; bases: string[] }> {
  const out = new Map<string, { grade: CodeGrade; bases: string[] }>();
  for (const [code, basis] of codes) {
    const g = gradeOf(basis, v, kind);
    const cur = out.get(code);
    if (!cur) out.set(code, { grade: g, bases: [basis] });
    else {
      if (!cur.bases.includes(basis)) cur.bases.push(basis);
      if (g === 'specific') cur.grade = 'specific';
    }
  }
  return out;
}

export const isDelivery = (status: string, v: AgrVocab): boolean => deliveryStatuses(v).includes(status);

/** Existence in words: "Exists: signed 2023 · non-binding" (the status verb, its date, the legal status). */
export function existenceWords(a: AgrAgreement): string {
  const legal = a.legal === 'binding' ? 'binding' : a.legal === 'non_binding' ? 'non-binding' : a.legal === 'mixed' ? 'binding status mixed' : 'binding status unknown';
  const when = (d: string | null) => dateWords(d) ?? 'undated';
  let what: string;
  switch (a.status) {
    case 'announced':
      what = `announced ${when(a.date)}`;
      break;
    case 'signed':
      what = `signed ${when(a.signature_date || a.date)}`;
      break;
    case 'in_force':
      what = a.effective_date ? `in force since ${dateWords(a.effective_date)}` : `in force (dated ${when(a.date)})`;
      break;
    case 'inactive':
      what = `inactive or expired (dated ${when(a.date)})`;
      break;
    case 'cancelled':
      what = `cancelled (dated ${when(a.date)})`;
      break;
    default:
      // delivery statuses and unknown: the existence the dates show
      what = a.effective_date ? `in force since ${dateWords(a.effective_date)}` : a.signature_date ? `signed ${dateWords(a.signature_date)}` : `dated ${when(a.date)}`;
      if (a.status === 'unknown') what += ', status not recorded';
  }
  return `Exists: ${what} · ${legal}`;
}

/** The primary source of an agreement: the official text (primary_url), an official page (a source record's link), or none identified. */
export interface PrimarySource {
  kind: 'text' | 'page' | 'none';
  /** the link (null: none, or an official page whose record has no link) */
  url: string | null;
  /** "official text", "official page", "not identified" */
  words: string;
}

/** The datasets whose record links can be an official page, most official first (DESTA and IISD records link their own site). */
const OFFICIAL_RECORD_ORDER: readonly string[] = ['curated', 'memberships', 'gsp', 'pmemo'];

/**
 * The primary source: the official text when primary_url is set; when only primary_available is (a record's
 * link is on an official host), that record's link, labelled an official page (a curated, membership, GSP or
 * memo record before others; never the own site of a dataset that is not official, as DESTA's); else not identified.
 */
export function primarySource(a: AgrAgreement, v: AgrVocab): PrimarySource {
  if (a.primary_url) return { kind: 'text', url: a.primary_url, words: 'official text' };
  if (!a.primary_available) return { kind: 'none', url: null, words: 'not identified' };
  // the own sites of the datasets that are not official (DESTA's downloads page, IISD's tracker); a membership
  // record's link to an official page (USTR) stays even where it is that dataset's own link
  const sites = new Set(v.sources.filter((s) => !OFFICIAL_RECORD_ORDER.includes(s.id)).map((s) => s.url).filter(Boolean));
  const rank = (d: string) => (OFFICIAL_RECORD_ORDER.includes(d) ? OFFICIAL_RECORD_ORDER.indexOf(d) : OFFICIAL_RECORD_ORDER.length);
  const rec = a.sources.filter((s) => s.url && s.dataset !== 'desta' && !sites.has(s.url)).sort((x, y) => rank(x.dataset) - rank(y.dataset))[0];
  return { kind: 'page', url: rec?.url ?? null, words: 'official page' };
}

export interface DeliveryState {
  /** evidence of delivery recorded */
  has: boolean;
  words: string;
}

/** Delivery in words: "Delivery: implementation underway (2 items)", or "Delivery: no evidence recorded". */
export function deliveryWords(a: AgrAgreement, v: AgrVocab): DeliveryState {
  const n = a.delivery.length;
  if (isDelivery(a.status, v)) {
    const word = a.status === 'operational' ? 'implemented, operational' : 'implementation underway';
    return { has: true, words: `Delivery: ${word}${n ? ` (${plural(n, 'item')})` : ''}` };
  }
  if (n) return { has: true, words: `Delivery: ${plural(n, 'item')} of evidence recorded` };
  return { has: false, words: 'Delivery: no evidence recorded' };
}

// ---- depth -------------------------------------------------------------------------------------------------

export type Scope = 'all' | 'ei';
export const SCOPES: readonly Scope[] = ['all', 'ei'];
export const SCOPE_LABEL: Record<Scope, string> = { all: 'All partnerships', ei: 'Electro-industrial partnerships' };
export const DEFAULT_SCOPE: Scope = 'ei';

/** The class names when a manifest has none (config depth.classes). */
export const DEPTH_NAME: Record<Depth, string> = {
  0: 'No identified partnership',
  1: 'Dialogue',
  2: 'Framework',
  3: 'Sectoral cooperation',
  4: 'Operational partnership',
  5: 'Integrated partnership',
};

/** The general rule of each class (docs/partnership_agreements.md section 7), next to the rule the data name. */
export const DEPTH_RULE: Record<Depth, string> = {
  0: 'No agreement of the pair is found in these sources.',
  1: 'Any agreement: a joint statement, a dialogue or membership of the same club.',
  2: 'A framework agreement (a treaty, a trade agreement, a framework, a memorandum of understanding, a sectoral partnership or a financing agreement) or a named strategic partnership.',
  3: 'An industrial or electro-industrial sector covered at an established level, or in at least two sentences of one text.',
  4: 'A concrete instrument (finance, offtake, supply, trade preference and the like) established by the source, the official text or the agreement type.',
  5: 'Class 4, plus established instruments in trade and in finance, investment or manufacturing, sectoral cooperation, and something in effect (a binding agreement in force or delivery evidence).',
};

/**
 * Why a pair is at class 0 in a scope, given how many of its agreements fall in the scope: none found, or
 * agreements that no longer count (expired, cancelled, or an older signed or announced text outside the active
 * window of section 7).
 */
export function zeroDepthWords(nInScope: number, scope: Scope): string {
  const what = scope === 'ei' ? 'electro-industrial agreement' : 'agreement';
  if (!nInScope) return `No ${what} of the pair is found in these sources.`;
  return nInScope === 1
    ? `The pair's one ${what} does not count toward depth: it is expired, cancelled, or an older signed or announced text outside the active window.`
    : `None of the pair's ${nInScope} ${what}s counts toward depth: each is expired, cancelled, or an older signed or announced text outside the active window.`;
}

/** Class 0 in a few words (a tooltip, a cell): "3 agreements, none counts toward depth", or "No agreement found" when there is none. */
export function zeroDepthShort(nInScope: number, scope: Scope): string {
  const what = scope === 'ei' ? 'electro-industrial agreement' : 'agreement';
  if (!nInScope) return `No ${what} found`;
  return nInScope === 1 ? `1 ${what}, which does not count toward depth` : `${nInScope} ${what}s, none counts toward depth`;
}

/**
 * A partner's tooltip on the partner map: its class (`depth`: the pair's class when it has a matching agreement,
 * null when it has none), or, at class 0 with agreements in the scope, that none counts toward depth; `caveat`
 * (the absence caveat) only where nothing was found.
 */
export function partnerTipWords(o: { depth: number | null; nInScope: number; scope: Scope; scopeWords: string; filtered: boolean; v: AgrVocab }): { words: string; caveat: boolean } {
  const { depth: d, nInScope: n, scope, scopeWords, filtered, v } = o;
  if (d !== null && d > 0) return { words: `Class ${d} · ${depthName(v, d)} (${scopeWords})`, caveat: false };
  if (d !== null && n > 0) return { words: `Class 0 · ${zeroDepthShort(n, scope)} (${scopeWords})`, caveat: false };
  if (filtered) return { words: `No agreement that matches the filters (${scopeWords})`, caveat: true };
  return { words: `${zeroDepthShort(0, scope)} (${scopeWords})`, caveat: true };
}

export function depthName(v: AgrVocab | null | undefined, d: number): string {
  return v?.depth_classes?.[String(d)] ?? DEPTH_NAME[(DEPTH_CLASSES.includes(d as Depth) ? d : 0) as Depth];
}

/** The pair's depth in a scope. */
export const depthOf = (p: { depth_all: number; depth_ei: number }, scope: Scope): number => (scope === 'ei' ? p.depth_ei : p.depth_all);
export const depthRuleOf = (p: AgrPartner, scope: Scope): string | null => (scope === 'ei' ? p.depth_ei_rule : p.depth_all_rule);

/** An agreement id in words: its title in quotes and its year, or "an agreement" when the economy's file does not hold it. */
export function agreementRef(id: string, byId?: ReadonlyMap<string, AgrAgreement>): string {
  const a = byId?.get(id);
  if (!a) return 'an agreement';
  const y = yearOf(a.date);
  return `“${a.title}”${y ? ` (${y})` : ''}`;
}

const AGR_ID_RE = /\bagr-[0-9a-f]{10}\b/g;
/** R's rule head: "<class> <class label> (all agreements|electro-industrial agreements): " (agr_pairs.R agr_depth_classes). */
const RULE_HEAD_RE = /^\s*[0-5]\s[^:]*\((?:all|electro-industrial) agreements\)\s*:?\s*/;

/**
 * The rule that set a pair's depth class (depth_all_rule / depth_ei_rule as R writes it) in plain words: the
 * class head dropped (the chip shows it), instruments with the basis that established them, sectors with their
 * grade (established or mentioned in text), agreement types and instrument groups by their labels, and agreement
 * ids as their titles. Null when nothing is left (class 0: the general rule applies). A rule in another
 * wording passes through with only the ids replaced.
 */
export function depthRuleWords(rule: string | null | undefined, v: AgrVocab, byId?: ReadonlyMap<string, AgrAgreement>): string | null {
  if (!rule) return null;
  const typeIds = new Set(Object.keys(v.agreement_types ?? {}));
  const groupIds = new Set(Object.keys(v.instrument_groups ?? {}));
  const body = rule
    .replace(RULE_HEAD_RE, '')
    .replace(/\b([a-z][a-z0-9_]*) \[([a-z_]+)\]/g, (_m, c: string, b: string) => `${labelOf(v, 'instruments', c)} (${BASIS_WORD[b] ?? humanize(b)})`)
    .replace(/\b([a-z][a-z0-9_]*):(specific|text)\b/g, (_m, c: string, g: CodeGrade) => `${labelOf(v, 'sectors', c)} (${GRADE_WORD[g]})`)
    .replace(/\(([a-z_]+(?:\|[a-z_]+)*)\)/g, (m, s: string) => (s.split('|').every((x) => groupIds.has(x)) ? `(${s.split('|').map((g) => labelOf(v, 'instrument_groups', g)).join(', ')})` : m))
    .replace(/(^|: |; )([a-z][a-z0-9_]*)(?= in agr-)/g, (m, pre: string, t: string) => (typeIds.has(t) ? `${pre}${labelOf(v, 'agreement_types', t)}` : m))
    .replace('strategic partnership label', 'a named strategic partnership')
    .replace('only plurilaterals above max_parties_for_depth', 'only plurilateral agreements with too many parties to set a deeper class')
    .replace(/(^|; )sectoral /, '$1sectoral cooperation: ')
    .replace(AGR_ID_RE, (id) => agreementRef(id, byId))
    .trim();
  return body || null;
}

/**
 * The depth colours: one hue, dim -> bright = class 1 -> 5 on the dark ground (the palette's core ordinal ramp,
 * GROUP_ORDINAL.core reversed, with the lightest sequential step on top); class 0 is the observed-zero ink.
 * Every use also writes the class number or name (colour is never alone).
 */
export const DEPTH_FILL: Record<Depth, string> = { 0: ZERO_FILL, 1: '#00688e', 2: '#1889b0', 3: '#47abd4', 4: '#6dcff8', 5: '#b4e7fe' };
/** Text on a depth fill (white on the dark steps, ink on the light ones). */
export const DEPTH_INK: Record<Depth, string> = { 0: '#ffffff', 1: '#ffffff', 2: '#ffffff', 3: '#111827', 4: '#111827', 5: '#111827' };

/** The bins of the global map (number of partners at depth 3 or more): 0 is the observed zero. */
export const COUNT_BINS: readonly { min: number; max: number; label: string; fill: string }[] = [
  { min: 1, max: 1, label: '1 partner', fill: DEPTH_FILL[1] },
  { min: 2, max: 4, label: '2 to 4', fill: DEPTH_FILL[2] },
  { min: 5, max: 9, label: '5 to 9', fill: DEPTH_FILL[3] },
  { min: 10, max: 19, label: '10 to 19', fill: DEPTH_FILL[4] },
  { min: 20, max: Infinity, label: '20 or more', fill: DEPTH_FILL[5] },
];
export const countBin = (n: number) => COUNT_BINS.find((b) => n >= b.min && n <= b.max) ?? null;

// ---- filters ----------------------------------------------------------------------------------------------

export interface AgrFilter {
  country: string | null;
  partner: string | null;
  scope: Scope;
  from: number | null;
  to: number | null;
  type: string | null;
  sector: string | null;
  tech: string | null;
  segment: string | null;
  /** an instrument group */
  igroup: string | null;
  inst: string | null;
  legal: string | null;
  /** an implementation status, or 'delivery' (implementation underway or operational) */
  status: string | null;
  source: string | null;
  /** the minimum depth class of the pair (in the scope) */
  depth: number | null;
}

export const NO_FILTER: AgrFilter = {
  country: null, partner: null, scope: DEFAULT_SCOPE, from: null, to: null, type: null, sector: null, tech: null, segment: null, igroup: null, inst: null, legal: null, status: null, source: null, depth: null,
};

/** The URL keys of the filters (country, partner and scope included); `sort` and `group` are the page's own. */
export const FILTER_KEYS = ['country', 'partner', 'scope', 'from', 'to', 'type', 'sector', 'tech', 'segment', 'igroup', 'inst', 'legal', 'status', 'source', 'depth'] as const;
export type FilterKey = (typeof FILTER_KEYS)[number];
/** The filter keys that narrow the agreements (not who or the scope). */
export const NARROWING_KEYS: readonly FilterKey[] = ['from', 'to', 'type', 'sector', 'tech', 'segment', 'igroup', 'inst', 'legal', 'status', 'source', 'depth'];
/** Narrowing keys the global (all-economies) view never applies: a single instrument needs an economy's agreements. */
export const COUNTRY_ONLY_KEYS: readonly FilterKey[] = ['inst'];
/**
 * Narrowing keys the global view applies through an additive pair column (global.pairs.stages, igroups, legal,
 * status: a pair passes when any of its agreements has the value); without the column, only with an economy.
 */
export const PAIR_COLUMN_KEYS = { segment: 'stages', igroup: 'igroups', legal: 'legal', status: 'status' } as const satisfies Partial<Record<FilterKey, string>>;

const year = (s: string | null): number | null => (s && /^\d{4}$/.test(s) ? Number(s) : null);

/** The techs the manifest's sectors map to (the technology filter's choices). */
export function techsOf(v: AgrVocab): string[] {
  return [...new Set(v.sectors.flatMap((s) => s.techs))].sort();
}

/** The filter of a query string, each value checked against the manifest's vocabularies (unknown values dropped). */
export function filterOfSearch(q: URLSearchParams, m: AgreementsManifest | null): AgrFilter {
  const v = m?.vocab;
  const inMap = (x: string | null, map: Record<string, string> | undefined) => (x && map && !Array.isArray(map) && x in map ? x : null);
  const inIds = (x: string | null, ids: readonly { id: string }[] | undefined) => (x && ids?.some((s) => s.id === x) ? x : null);
  const iso = (x: string | null) => (x && /^[A-Za-z0-9]{3}$/.test(x) ? x.toUpperCase() : null);
  const scope = q.get('scope') === 'all' ? 'all' : 'ei';
  const country = iso(q.get('country'));
  const partner = country ? iso(q.get('partner')) : null;
  const depth = q.get('depth');
  const status = q.get('status');
  return {
    country,
    partner: partner && partner !== country ? partner : null,
    scope,
    from: year(q.get('from')),
    to: year(q.get('to')),
    type: inMap(q.get('type'), v?.agreement_types),
    sector: inIds(q.get('sector'), v?.sectors),
    tech: v && q.get('tech') && techsOf(v).includes(q.get('tech') as string) ? q.get('tech') : null,
    segment: q.get('segment') && (SEGMENTS as readonly string[]).includes(q.get('segment') as string) ? q.get('segment') : null,
    igroup: inMap(q.get('igroup'), v?.instrument_groups),
    inst: inIds(q.get('inst'), v?.instruments),
    legal: inMap(q.get('legal'), v?.legal_statuses),
    status: status === 'delivery' ? 'delivery' : inMap(status, v?.implementation_statuses),
    source: inIds(q.get('source'), v?.sources),
    depth: depth && /^[1-5]$/.test(depth) ? Number(depth) : null,
  };
}

/** The query string of a filter change: `k` set to `val` (empty: removed), the dependants dropped, other keys kept. */
export function setFilterSearch(q: URLSearchParams, k: string, val: string): URLSearchParams {
  const out = new URLSearchParams(q);
  // the default scope is not written
  if (val && !(k === 'scope' && val === DEFAULT_SCOPE)) out.set(k, val);
  else out.delete(k);
  // a partner needs an economy, and never the economy itself
  if (k === 'country' && (!val || val.toUpperCase() === out.get('partner')?.toUpperCase())) out.delete('partner');
  // an instrument belongs to its group
  if (k === 'igroup') out.delete('inst');
  return out;
}

/** The filter's narrowing keys that are set. */
export const activeKeys = (f: AgrFilter): FilterKey[] => NARROWING_KEYS.filter((k) => f[k] !== null);

/** Does an agreement pass the filter (the scope, every narrowing key; the partner and depth are the caller's: they are pair attributes)? */
export function agreementPasses(a: AgrAgreement, f: AgrFilter, v: AgrVocab): boolean {
  if (f.scope === 'ei' && !a.ei) return false;
  const y = yearOf(a.date);
  if (f.from !== null && (y === null || y < f.from)) return false;
  if (f.to !== null && (y === null || y > f.to)) return false;
  if (f.type && a.type !== f.type) return false;
  if (f.sector && !a.sectors.some(([c]) => c === f.sector)) return false;
  if (f.tech) {
    const mapped = v.sectors.filter((s) => s.techs.includes(f.tech as string)).map((s) => s.id);
    if (!a.techs.includes(f.tech) && !a.sectors.some(([c]) => mapped.includes(c))) return false;
  }
  if (f.segment && !a.segments.includes(f.segment)) return false;
  if (f.igroup) {
    const inGroup = v.instruments.filter((i) => i.group === f.igroup).map((i) => i.id);
    if (!a.instruments.some(([c]) => inGroup.includes(c))) return false;
  }
  if (f.inst && !a.instruments.some(([c]) => c === f.inst)) return false;
  if (f.legal && a.legal !== f.legal) return false;
  if (f.status) {
    if (f.status === 'delivery' ? !isDelivery(a.status, v) : a.status !== f.status) return false;
  }
  if (f.source && !a.sources.some((s) => s.dataset === f.source)) return false;
  return true;
}

// ---- the economy's file ---------------------------------------------------------------------------------

export interface CountryIndex {
  byId: Map<string, AgrAgreement>;
  /** agreement id -> the partners whose pair profile lists it */
  partnersOf: Map<string, string[]>;
  partner: Map<string, AgrPartner>;
}

export function indexCountry(file: AgreementsCountry): CountryIndex {
  const byId = new Map(file.agreements.map((a) => [a.id, a]));
  const partnersOf = new Map<string, string[]>();
  for (const p of file.partners) for (const id of p.agreement_ids) partnersOf.set(id, [...(partnersOf.get(id) ?? []), p.partner]);
  return { byId, partnersOf, partner: new Map(file.partners.map((p) => [p.partner, p])) };
}

/** The partner's agreements (its pair profile's ids, newest first as exported). */
export function agreementsOfPartner(ix: CountryIndex, partner: string): AgrAgreement[] {
  return (ix.partner.get(partner)?.agreement_ids ?? []).flatMap((id) => {
    const a = ix.byId.get(id);
    return a ? [a] : [];
  });
}

/** The economy's agreements that pass the filter (with a partner: that pair's only; with a depth: only pairs at that depth or deeper). */
export function filterAgreements(file: AgreementsCountry, f: AgrFilter, v: AgrVocab, ix: CountryIndex = indexCountry(file)): AgrAgreement[] {
  const base = f.partner ? agreementsOfPartner(ix, f.partner) : file.agreements;
  return base.filter((a) => {
    if (!agreementPasses(a, f, v)) return false;
    if (f.depth !== null) {
      const ps = f.partner ? [f.partner] : (ix.partnersOf.get(a.id) ?? []);
      if (!ps.some((p) => {
        const row = ix.partner.get(p);
        return !!row && depthOf(row, f.scope) >= (f.depth as number);
      })) return false;
    }
    return true;
  });
}

/** Newest first by date (undated last), then title. */
export const byNewest = (a: AgrAgreement, b: AgrAgreement): number => (b.date ?? '').localeCompare(a.date ?? '') || a.title.localeCompare(b.title);

// ---- partner ranking ----------------------------------------------------------------------------------------

export type SortKey = 'depth' | 'breadth' | 'sector' | 'implementation' | 'recency';
export const SORT_KEYS: readonly SortKey[] = ['depth', 'breadth', 'sector', 'implementation', 'recency'];
export const DEFAULT_SORT: SortKey = 'depth';

/** Each sort key: its button label and the order it applies, in words (shown with the table). */
export const SORT_INFO: Record<SortKey, { label: string; words: string }> = {
  depth: { label: 'Depth', words: 'the depth class of the pair, then pairs whose class rests on their own agreements before those whose class rests on EU agreements only, then the number of matching agreements' },
  breadth: { label: 'Breadth', words: 'the number of matching agreements, then the depth class' },
  sector: { label: 'Sector relevance', words: 'the number of electro-industrial sectors the pair covers at an established level, then the depth class' },
  implementation: { label: 'Implementation', words: 'the number of matching agreements with delivery evidence, then those in force, then the depth class' },
  recency: { label: 'Recency', words: 'the date of the newest matching agreement, then the depth class' },
};

export const sortKeyOf = (x: string | null): SortKey => (SORT_KEYS.includes(x as SortKey) ? (x as SortKey) : DEFAULT_SORT);

export interface PartnerRank {
  partner: string;
  name: string;
  /** the pair's depth class in the scope */
  depth: number;
  depthRule: string | null;
  /** matching agreements (the filter and the scope) */
  n: number;
  /** all the pair's agreements */
  nTotal: number;
  /** the pair's agreements in the scope (electro-industrial ones in that scope), whether they count toward depth or not */
  nScope: number;
  /** electro-industrial sectors the pair covers at an established level (pair profile) */
  eiSpecific: number;
  /** matching agreements with delivery evidence */
  nDelivery: number;
  /** matching agreements in force */
  nInForce: number;
  /** the newest matching agreement's date */
  newest: string | null;
  /** the depth class rests only on EU-level agreements counted for this member state (every agreement the rule cites is via the EU) */
  viaEuOnly: boolean;
  row: AgrPartner;
}

/** Electro-industrial sectors of a pair at an established level. */
export function eiSpecificSectors(p: AgrPartner, v: AgrVocab): string[] {
  const ei = new Set(v.sectors.filter((s) => s.ei).map((s) => s.id));
  return p.sectors.filter(([c, g]) => g === 'specific' && ei.has(c)).map(([c]) => c);
}

const cmpNum = (a: number, b: number) => b - a;

/**
 * The economy's partners ranked by one transparent key (SORT_INFO): only partners with at least one matching
 * agreement (and, with a depth filter, at that depth or deeper), ties broken by the next keys and the name.
 * The depth and the sector relevance are the pair's (its exported profile); the counts and the newest date
 * are over the matching agreements. No composite score.
 */
/** True when an agreement reaches the pair (focal country, partner) only through the EU: counted for the focal
 *  member state (via_eu), or signed by the EU without the partner, which is then a member state. */
export function viaEuForPair(a: AgrAgreement, partner: string): boolean {
  return a.via_eu || (!a.parties.includes(partner) && a.parties.includes('EUU'));
}

/** True when every agreement a depth rule cites reaches the pair only through the EU (false when it cites none). */
export function depthRestsOnEu(rule: string | null, ix: CountryIndex, partner: string): boolean {
  const ids = rule ? [...new Set(rule.match(/agr-[0-9a-f]{10}/g) ?? [])] : [];
  return ids.length > 0 && ids.every((id) => {
    const a = ix.byId.get(id);
    return a !== undefined && viaEuForPair(a, partner);
  });
}

export function rankPartners(file: AgreementsCountry, f: AgrFilter, sort: SortKey, v: AgrVocab, names: Readonly<Record<string, string>>): PartnerRank[] {
  const ix = indexCountry(file);
  const rows: PartnerRank[] = [];
  for (const p of file.partners) {
    const depth = depthOf(p, f.scope);
    if (f.depth !== null && depth < f.depth) continue;
    const theirs = agreementsOfPartner(ix, p.partner);
    const mine = theirs.filter((a) => agreementPasses(a, f, v));
    if (!mine.length) continue;
    rows.push({
      partner: p.partner,
      name: nameOf(names, p.partner),
      depth,
      depthRule: depthRuleOf(p, f.scope),
      n: mine.length,
      nTotal: p.n,
      nScope: theirs.filter((a) => f.scope === 'all' || a.ei).length,
      eiSpecific: eiSpecificSectors(p, v).length,
      nDelivery: mine.filter((a) => isDelivery(a.status, v)).length,
      nInForce: mine.filter((a) => a.status === 'in_force').length,
      newest: mine.reduce<string | null>((m, a) => (a.date && (!m || a.date > m) ? a.date : m), null),
      viaEuOnly: depthRestsOnEu(depthRuleOf(p, f.scope), ix, p.partner),
      row: p,
    });
  }
  const tie = (a: PartnerRank, b: PartnerRank) => a.name.localeCompare(b.name, 'en');
  const order: Record<SortKey, (a: PartnerRank, b: PartnerRank) => number> = {
    // at equal depth, a relationship of the pair's own before one that exists only through the EU
    depth: (a, b) => cmpNum(a.depth, b.depth) || Number(a.viaEuOnly) - Number(b.viaEuOnly) || cmpNum(a.n, b.n) || tie(a, b),
    breadth: (a, b) => cmpNum(a.n, b.n) || cmpNum(a.depth, b.depth) || tie(a, b),
    sector: (a, b) => cmpNum(a.eiSpecific, b.eiSpecific) || cmpNum(a.depth, b.depth) || cmpNum(a.n, b.n) || tie(a, b),
    implementation: (a, b) => cmpNum(a.nDelivery, b.nDelivery) || cmpNum(a.nInForce, b.nInForce) || cmpNum(a.depth, b.depth) || tie(a, b),
    recency: (a, b) => (b.newest ?? '').localeCompare(a.newest ?? '') || cmpNum(a.depth, b.depth) || tie(a, b),
  };
  return rows.sort(order[sort]);
}

// ---- coverage tallies -----------------------------------------------------------------------------------

export interface CoverageRow<C extends string | number> {
  partner: string;
  name: string;
  cells: Record<string, C | null>;
}

/**
 * Sectors x partners from the pair profiles: each cell the grade at which the pair's agreements cover the
 * sector (specific: established; text: mentioned in at least two sentences of one text), null when not covered.
 * Electro-industrial sectors only in that scope; the sectors no partner covers are left out.
 */
export function sectorCoverage(partners: readonly AgrPartner[], scope: Scope, v: AgrVocab, names: Readonly<Record<string, string>>): { sectors: string[]; rows: CoverageRow<CodeGrade>[] } {
  const allowed = v.sectors.filter((s) => scope === 'all' || s.ei).map((s) => s.id);
  const rows = partners.map((p) => {
    const cells: Record<string, CodeGrade | null> = {};
    for (const [c, g] of p.sectors) if (allowed.includes(c) && cells[c] !== 'specific') cells[c] = g;
    return { partner: p.partner, name: nameOf(names, p.partner), cells };
  });
  const sectors = allowed.filter((s) => rows.some((r) => r.cells[s]));
  return { sectors, rows };
}

/** The instrument groups of some agreements (0 none, 1 mentioned in text, 2 established), graded with the instrument bases. */
export function instrumentLevels(agreements: readonly AgrAgreement[], v: AgrVocab): Record<string, InstLevel> {
  const groupOf = new Map(v.instruments.map((i) => [i.id, i.group]));
  const out: Record<string, InstLevel> = {};
  for (const a of agreements)
    for (const [code, basis] of a.instruments) {
      const g = groupOf.get(code) ?? 'other';
      const level: InstLevel = gradeOf(basis, v, 'instruments') === 'specific' ? 2 : 1;
      if ((out[g] ?? 0) < level) out[g] = level;
    }
  return out;
}

/**
 * Instrument groups x partners (0 none, 1 mentioned in text, 2 established): all the pair's agreements from the
 * pair profiles; in the electro-industrial scope, when the economy's index is given, only the pair's
 * electro-industrial agreements of the economy's file.
 */
export function instrumentCoverage(
  partners: readonly AgrPartner[],
  v: AgrVocab,
  names: Readonly<Record<string, string>>,
  opts: { scope?: Scope; ix?: CountryIndex } = {},
): { groups: string[]; rows: CoverageRow<InstLevel>[] } {
  const all = Object.keys(v.instrument_groups ?? {});
  const ix = opts.ix;
  const rows = partners.map((p) => {
    const levels = opts.scope === 'ei' && ix ? instrumentLevels(agreementsOfPartner(ix, p.partner).filter((a) => a.ei), v) : (p.inst ?? {});
    const cells: Record<string, InstLevel | null> = {};
    for (const g of all) cells[g] = (levels[g] ?? 0) as InstLevel;
    return { partner: p.partner, name: nameOf(names, p.partner), cells };
  });
  const groups = all.filter((g) => rows.some((r) => (r.cells[g] ?? 0) > 0));
  return { groups, rows };
}

/** Tallies of a set of agreements: by sector and by instrument group, each split established / mentioned. */
export function codeTallies(agreements: readonly AgrAgreement[], v: AgrVocab): {
  sectors: { id: string; specific: number; text: number }[];
  groups: { id: string; specific: number; text: number }[];
} {
  const groupOf = new Map(v.instruments.map((i) => [i.id, i.group]));
  const sec = new Map<string, { specific: number; text: number }>();
  const grp = new Map<string, { specific: number; text: number }>();
  for (const a of agreements) {
    for (const [code, b] of bestGrades(a.sectors, v)) {
      const t = sec.get(code) ?? { specific: 0, text: 0 };
      t[b.grade] += 1;
      sec.set(code, t);
    }
    const byGroup = new Map<string, CodeGrade>();
    for (const [code, basis] of a.instruments) {
      const g = groupOf.get(code) ?? 'other';
      if (byGroup.get(g) !== 'specific') byGroup.set(g, gradeOf(basis, v, 'instruments'));
    }
    for (const [g, grade] of byGroup) {
      const t = grp.get(g) ?? { specific: 0, text: 0 };
      t[grade] += 1;
      grp.set(g, t);
    }
  }
  const sortT = (a: { id: string; specific: number; text: number }, b: { id: string; specific: number; text: number }) => b.specific - a.specific || b.text - a.text || a.id.localeCompare(b.id);
  return { sectors: [...sec].map(([id, t]) => ({ id, ...t })).sort(sortT), groups: [...grp].map(([id, t]) => ({ id, ...t })).sort(sortT) };
}

/** Counts by implementation status (existence and delivery apart). */
export function statusCounts(agreements: readonly AgrAgreement[], v: AgrVocab): { status: string; n: number; delivery: boolean }[] {
  const by = new Map<string, number>();
  for (const a of agreements) by.set(a.status, (by.get(a.status) ?? 0) + 1);
  const order = Object.keys(v.implementation_statuses ?? {});
  return [...by].map(([status, n]) => ({ status, n, delivery: isDelivery(status, v) })).sort((a, b) => order.indexOf(a.status) - order.indexOf(b.status));
}

// ---- timeline ---------------------------------------------------------------------------------------------

export interface TimelineBin {
  year: number;
  n: number;
  binding: number;
  ei: number;
  delivery: number;
}

/** Agreements per year, every year from the first to the last (empty years kept); undated agreements are counted apart. */
export function timelineBins(agreements: readonly AgrAgreement[], v: AgrVocab): { bins: TimelineBin[]; undated: number } {
  const by = new Map<number, TimelineBin>();
  let undated = 0;
  for (const a of agreements) {
    const y = yearOf(a.date);
    if (y === null) {
      undated += 1;
      continue;
    }
    const b = by.get(y) ?? { year: y, n: 0, binding: 0, ei: 0, delivery: 0 };
    b.n += 1;
    if (a.legal === 'binding') b.binding += 1;
    if (a.ei) b.ei += 1;
    if (isDelivery(a.status, v) || a.delivery.length) b.delivery += 1;
    by.set(y, b);
  }
  if (!by.size) return { bins: [], undated };
  const ys = [...by.keys()];
  const lo = Math.min(...ys);
  const hi = Math.max(...ys);
  const bins: TimelineBin[] = [];
  for (let y = lo; y <= hi; y++) bins.push(by.get(y) ?? { year: y, n: 0, binding: 0, ei: 0, delivery: 0 });
  return { bins, undated };
}

/** Agreements dated within `years` of the as-of date (the "recent" window, config pairs.recent_years). */
export function recentAgreements(agreements: readonly AgrAgreement[], asOf: string, years = 5): AgrAgreement[] {
  const ref = yearOf(asOf);
  if (ref === null) return [];
  const cut = `${ref - years}${asOf.slice(4)}`;
  return agreements.filter((a) => !!a.date && a.date >= cut.slice(0, a.date.length)).sort(byNewest);
}

// ---- the global file ----------------------------------------------------------------------------------------

export interface GlobalPair {
  a: string;
  b: string;
  n: number;
  n_recent: number;
  newest: string | null;
  depth_all: number;
  depth_ei: number;
  has_sp: boolean;
  has_trade: boolean;
  has_sectoral: boolean;
  n_delivery: number;
  /** sector ids */
  ei_sectors: string[];
  /** each sector's grade when the file carries it (`id:grade`, R's pair profile form); else absent */
  ei_grades: Record<string, CodeGrade>;
  types: string[];
  sources: string[];
  // the additive columns of the all-economies filters; null when the file does not carry the column
  /** the years of all the pair's agreements */
  years: number[] | null;
  /** the years of its electro-industrial agreements */
  years_ei: number[] | null;
  /** instrument groups established in any of its agreements */
  igroups: string[] | null;
  /** legal statuses of its agreements */
  legal: string[] | null;
  /** implementation statuses of its agreements */
  status: string[] | null;
  /** stages its agreements cover at an established grade */
  stages: string[] | null;
}

const split = (s: string | null | undefined): string[] => (s ? s.split('|').filter(Boolean) : []);
const yearsOfJoined = (s: string | null | undefined): number[] => split(s).flatMap((y) => (/^\d{4}$/.test(y) ? [Number(y)] : []));

/** An `ei_sectors` value (ids, or R's `id:grade`) as ids and the grades it carries (specific beats text). */
export function gradedSectors(parts: readonly string[]): { ids: string[]; grades: Record<string, CodeGrade> } {
  const ids: string[] = [];
  const grades: Record<string, CodeGrade> = {};
  for (const part of parts) {
    const [id, g] = splitGraded(part);
    if (!ids.includes(id)) ids.push(id);
    if ((g === 'specific' || g === 'text') && grades[id] !== 'specific') grades[id] = g;
  }
  return { ids, grades };
}

/** The columnar pairs as rows. */
export function globalPairs(g: AgreementsGlobal): GlobalPair[] {
  const c = g.pairs;
  const opt = <T>(col: (string | null)[] | undefined, i: number, f: (s: string | null) => T): T | null => (col ? f(col[i] ?? null) : null);
  return c.a.map((a, i) => {
    const ei = gradedSectors(split(c.ei_sectors[i]));
    return {
      a,
      b: c.b[i],
      n: c.n[i],
      n_recent: c.n_recent[i],
      newest: c.newest[i] || null,
      depth_all: c.depth_all[i],
      depth_ei: c.depth_ei[i],
      has_sp: c.has_sp[i],
      has_trade: c.has_trade[i],
      has_sectoral: c.has_sectoral[i],
      n_delivery: c.n_delivery[i],
      ei_sectors: ei.ids,
      ei_grades: ei.grades,
      types: split(c.types[i]),
      sources: split(c.sources[i]),
      years: opt(c.years, i, yearsOfJoined),
      years_ei: opt(c.years_ei, i, yearsOfJoined),
      igroups: opt(c.igroups, i, split),
      legal: opt(c.legal, i, split),
      status: opt(c.status, i, split),
      stages: opt(c.stages, i, split),
    };
  });
}

/** The dates of the global file the Years filter starts from: every agreement year when the file lists them, else each pair's newest. */
export function globalDates(g: AgreementsGlobal): (string | null)[] {
  return g.pairs.years ? g.pairs.years.flatMap((s) => split(s)) : g.pairs.newest;
}

/** The years of a pair in a scope (its electro-industrial agreements' in that scope); null when the file does not list them. */
export const pairYears = (p: GlobalPair, scope: Scope): number[] | null => (scope === 'ei' ? p.years_ei : p.years);

/** True when the pair has an agreement in the scope: any pair in all partnerships; an electro-industrial depth class above 0 or an electro-industrial agreement year. */
export function pairInScope(p: GlobalPair, scope: Scope): boolean {
  return scope === 'all' || p.depth_ei > 0 || (p.years_ei?.length ?? 0) > 0;
}

/** The narrowing keys the all-economies view checks against the pair's agreements one by one (each on its own). */
const PER_PAIR_KEYS: readonly FilterKey[] = ['from', 'to', 'type', 'sector', 'tech', 'segment', 'igroup', 'legal', 'status', 'source'];

export interface PairFilterResult {
  rows: GlobalPair[];
  /** narrowing keys not applied (an economy is needed: a single instrument, a non-electro-industrial sector, a column the file lacks) */
  unsupported: FilterKey[];
  /** the years filter read each pair's newest agreement (the file lists no years per pair) */
  yearsByNewest: boolean;
  /** the applied keys that were checked on their own against the pair's agreements (a pair passes when some agreement matches each) */
  perPair: FilterKey[];
}

/**
 * The global filter of the pairs: the scope (electro-industrial: a pair with an electro-industrial agreement),
 * the economy and partner, then each narrowing key on its own against the pair's agreements: the years (any
 * agreement year in the range, the electro-industrial ones in that scope; the newest agreement when the file
 * lists no years), the type, an electro-industrial sector or a technology (through its sectors), the stage, the
 * instrument group, the legal and implementation status (when the file carries those columns), the source, and
 * the pair's depth. A single instrument and a non-electro-industrial sector need an economy's file:
 * `unsupported` lists what was not applied.
 */
export function filterPairs(pairs: readonly GlobalPair[], f: AgrFilter, v: AgrVocab): PairFilterResult {
  const unsupported: FilterKey[] = COUNTRY_ONLY_KEYS.filter((k) => f[k] !== null);
  const eiSector = f.sector ? v.sectors.find((s) => s.id === f.sector)?.ei === true : true;
  if (f.sector && !eiSector) unsupported.push('sector');
  // a column the file does not carry: the filter applies only with an economy
  const first = pairs[0];
  for (const [k, col] of Object.entries(PAIR_COLUMN_KEYS) as [FilterKey, keyof GlobalPair][]) if (f[k] !== null && first && first[col] === null) unsupported.push(k);
  const skip = new Set(unsupported);
  const yearSet = f.from !== null || f.to !== null;
  const yearsByNewest = yearSet && !!first && pairYears(first, f.scope) === null;
  const techSectors = f.tech ? v.sectors.filter((s) => s.ei && s.techs.includes(f.tech as string)).map((s) => s.id) : [];
  const delivery = deliveryStatuses(v);
  const inRange = (y: number | null) => y !== null && (f.from === null || y >= f.from) && (f.to === null || y <= f.to);
  const rows = pairs.filter((p) => {
    if (!pairInScope(p, f.scope)) return false;
    if (f.country && p.a !== f.country && p.b !== f.country) return false;
    if (f.partner && p.a !== f.partner && p.b !== f.partner) return false;
    if (yearSet) {
      const ys = pairYears(p, f.scope);
      if (ys ? !ys.some(inRange) : !inRange(yearOf(p.newest))) return false;
    }
    if (f.type && !p.types.includes(f.type)) return false;
    if (f.sector && eiSector && !p.ei_sectors.includes(f.sector)) return false;
    if (f.tech && !p.ei_sectors.some((s) => techSectors.includes(s))) return false;
    if (f.segment && !skip.has('segment') && !(p.stages ?? []).includes(f.segment)) return false;
    if (f.igroup && !skip.has('igroup') && !(p.igroups ?? []).includes(f.igroup)) return false;
    if (f.legal && !skip.has('legal') && !(p.legal ?? []).includes(f.legal)) return false;
    if (f.status && !skip.has('status')) {
      const st = p.status ?? [];
      if (f.status === 'delivery' ? !st.some((s) => delivery.includes(s)) : !st.includes(f.status)) return false;
    }
    if (f.source && !p.sources.includes(f.source)) return false;
    if (f.depth !== null && depthOf(p, f.scope) < f.depth) return false;
    return true;
  });
  const perPair = PER_PAIR_KEYS.filter((k) => f[k] !== null && !skip.has(k));
  return { rows, unsupported, yearsByNewest, perPair };
}

/** Partners at depth 3 or more per economy, over the given pairs (scope-aware: the scope's depth). */
export function partnersAtDepth(pairs: readonly GlobalPair[], scope: Scope, min = 3): Map<string, number> {
  const out = new Map<string, number>();
  for (const p of pairs) {
    if (depthOf(p, scope) < min) continue;
    out.set(p.a, (out.get(p.a) ?? 0) + 1);
    out.set(p.b, (out.get(p.b) ?? 0) + 1);
  }
  return out;
}

/** The economies of the pairs, by the number of agreements they take part in (most first; the network's fallback group). */
export function topEconomies(g: AgreementsGlobal, n = 30): string[] {
  const c = g.countries;
  return c.iso3
    .map((iso, i) => ({ iso, n: c.n_agreements[i] }))
    .filter((x) => x.iso !== 'EUU')
    .sort((a, b) => b.n - a.n || a.iso.localeCompare(b.iso))
    .slice(0, n)
    .map((x) => x.iso);
}

/**
 * The network matrix of a group: the depth class of each pair in the scope (0: the pair has agreements, none of
 * which counts toward depth), null when no pair row passes the filter (no agreement found, or none matches), -1
 * on the diagonal.
 */
export function depthMatrix(pairs: readonly GlobalPair[], group: readonly string[], scope: Scope): (number | null)[][] {
  const at = new Map<string, number>();
  for (const p of pairs) at.set(`${p.a}|${p.b}`, depthOf(p, scope));
  return group.map((a) => group.map((b) => (a === b ? -1 : (at.get(a < b ? `${a}|${b}` : `${b}|${a}`) ?? null))));
}

/** Pairs ordered for the global table: deepest first, then most agreements, then newest. */
export function orderPairs(pairs: readonly GlobalPair[], scope: Scope): GlobalPair[] {
  return [...pairs].sort((x, y) => depthOf(y, scope) - depthOf(x, scope) || y.n - x.n || (y.newest ?? '').localeCompare(x.newest ?? '') || `${x.a}${x.b}`.localeCompare(`${y.a}${y.b}`));
}

// ---- gaps -------------------------------------------------------------------------------------------------

export const QUADRANT_ORDER: readonly Quadrant[] = ['opportunity', 'established', 'legacy'];
export const QUADRANT_TITLE: Record<Quadrant, string> = {
  opportunity: 'High complementarity, weak architecture',
  established: 'High complementarity, strong architecture',
  legacy: 'Low complementarity, strong architecture',
  none: 'Low complementarity, weak architecture',
};
export const QUADRANT_NAME: Record<Quadrant, string> = { opportunity: 'Opportunity', established: 'Established', legacy: 'Legacy', none: 'Neither' };

export interface GapContext {
  focal: string;
  name: (iso3: string) => string;
  /** the pair's electro-industrial depth class (strong architecture without coverage of the technology) */
  depthEi?: (partner: string) => number | null;
  /** strategic compact id -> name (manifest vocab.compacts) */
  compacts?: Readonly<Record<string, string>>;
}

/** "no agreement in these sources": every absence the gap words state is an absence in these sources. */
const NO_AGREEMENT = 'no agreement in these sources';

/** A strategic compact by its name (manifest vocab.compacts), else its id. */
export function compactName(id: string, compacts?: Readonly<Record<string, string>> | null): string {
  const n = compacts && !Array.isArray(compacts) ? compacts[id] : undefined;
  return n || id;
}

/** The words of a row whose complementarity could not be assessed. */
export const NOT_ASSESSED_WORDS = 'complementarity could not be assessed (need or capability data missing)';

/**
 * True when a gap row's complementarity could not be assessed: so flagged (R), or, in an older export, not high
 * with need or capability missing (and not a match blocked by opposite UN-vote blocs, which says so itself).
 */
export function complementarityNotAssessed(g: AgrGap): boolean {
  return g.complementarity === 'not_assessed' || (g.complementarity !== 'high' && !g.alignment_blocked && (g.need === null || g.capability === null));
}

/** The complementarity of a gap row in words ("high", "low", or the not-assessed words). */
export const complementarityWord = (g: AgrGap): string => (complementarityNotAssessed(g) ? NOT_ASSESSED_WORDS.replace('complementarity ', '') : humanize(g.complementarity));

/** UN-vote alignment in words (geo_alignment: 1 same bloc, 0.5 one side non-aligned, 0 opposite blocs). */
export function alignmentWords(a: number | null | undefined): string | null {
  if (a === null || a === undefined) return null;
  if (a >= 1) return 'same UN-vote bloc';
  if (a > 0) return 'one side non-aligned in UN votes';
  return 'opposite UN-vote blocs';
}

/** Stages as words in supply-chain order ("Refining and Components"). */
export function stageListWords(stages: readonly string[] | undefined): string {
  return stages && stages.length ? listWords([...stages]) : '';
}

/** The components of a gap row in words: recommended cells, capability and need percentiles, alignment, Friendshore, shared compacts (by name). */
export function gapComponents(g: AgrGap, compacts?: Readonly<Record<string, string>> | null): string {
  const parts = [plural(g.recommended, 'recommended cell')];
  const cap = percentileWords(g.capability);
  parts.push(cap ? `capability ${cap}` : 'capability not assessed');
  const need = percentileWords(g.need);
  parts.push(need ? `need ${need}` : 'need not assessed');
  const al = alignmentWords(g.alignment);
  if (al) parts.push(al);
  if (g.friendshore !== null) parts.push(`Friendshore ${g.friendshore.toFixed(2)}`);
  if (g.aib_shared.length) parts.push(`shared strategic compacts: ${g.aib_shared.map((c) => compactName(c, compacts)).join(', ')}`);
  if (g.stages_high?.length) parts.push(`complementary at ${stageListWords(g.stages_high)}`);
  return parts.join(', ');
}

/** The coverage of the technology by the pair's agreements, in words. */
export function coverageWords(g: AgrGap, depthEi: number | null | undefined): string {
  const at = g.stages_high?.length ? ` at ${stageListWords(g.stages_high)}` : '';
  const elsewhere = g.covered_grade !== 'specific' && g.stages_covered?.length ? ` (agreements cover ${g.tech} only at ${stageListWords(g.stages_covered)})` : '';
  if (g.covered_grade === 'specific') {
    const which = g.stages_covered?.length ? ` (at ${stageListWords(g.stages_covered)})` : '';
    return `an agreement covers ${g.tech}${which}`;
  }
  if (g.covered_grade === 'text') return `${g.tech}${at} is only mentioned in the text of an agreement, not established${elsewhere}`;
  if (g.architecture === 'strong' && depthEi !== null && depthEi !== undefined) return `${NO_AGREEMENT} covers ${g.tech}${at}, though the pair's electro-industrial partnership is class ${depthEi}${elsewhere}`;
  return `${NO_AGREEMENT} covers ${g.tech}${at}${elsewhere}`;
}

/** One gap row as a sentence (the quadrant, who supplies whom, the components, the coverage). */
export function gapText(g: AgrGap, ctx: GapContext): string {
  const p = ctx.name(g.partner);
  const f = ctx.name(ctx.focal);
  const [supplier, buyer] = g.side === 'needs' ? [p, f] : [f, p];
  const comps = gapComponents(g, ctx.compacts);
  const cover = coverageWords(g, ctx.depthEi?.(g.partner));
  // need or capability data missing: the row says so instead of reading the missing data as low complementarity
  if (complementarityNotAssessed(g)) {
    const arch = g.architecture === 'strong' ? 'strong' : 'weak';
    return `Complementarity not assessed, ${arch} architecture: ${supplier} as a supplier of ${g.tech} to ${buyer} could not be assessed, need or capability data missing (${comps}); ${cover}.`;
  }
  switch (g.quadrant) {
    case 'opportunity':
      return `${QUADRANT_TITLE.opportunity}: ${supplier} could supply ${g.tech} to ${buyer} (${comps}) but ${cover}.`;
    case 'established':
      return `${QUADRANT_TITLE.established}: ${supplier} could supply ${g.tech} to ${buyer} (${comps}) and ${cover}.`;
    case 'legacy': {
      // strong architecture: an agreement covers the technology, or the pair is operational without covering it
      const d = ctx.depthEi?.(g.partner);
      const arch =
        g.covered_grade === 'specific'
          ? `an agreement of ${f} and ${p} covers ${g.tech}`
          : `the electro-industrial partnership of ${f} and ${p} is ${d !== null && d !== undefined ? `class ${d}` : 'operational'}, though ${g.covered_grade === 'text' ? `${g.tech} is only mentioned in the text of an agreement` : `${NO_AGREEMENT} covers ${g.tech}`}`;
      // low complementarity: opposite UN-vote blocs, or not a recommended partner and need and capability not both high at one step
      if (g.alignment_blocked) return `${QUADRANT_TITLE.legacy}: ${arch}; ${supplier} could supply ${g.tech} to ${buyer}, but the two are in opposite UN-vote blocs, so the match does not count as complementarity (${comps}).`;
      return `${QUADRANT_TITLE.legacy}: ${arch}; but ${supplier} is not a recommended supplier of ${g.tech} to ${buyer}, and need and capability are not both high at the same step (${comps}).`;
    }
    default:
      return `${QUADRANT_TITLE.none}: ${supplier} and ${buyer} in ${g.tech} (${comps}); ${cover}.`;
  }
}

/** The technologies of the electro-industrial sectors (the vocabulary's sectors with ei true). Oil, Gas and Coal are not among them. */
export function eiTechs(v: AgrVocab): Set<string> {
  return new Set(v.sectors.filter((s) => s.ei).flatMap((s) => s.techs));
}

/** Gap and execution-gap rows in the scope: the electro-industrial scope keeps the electro-industrial technologies only. */
export function gapsInScope<T extends { tech: string }>(rows: readonly T[], scope: Scope, v: AgrVocab): T[] {
  if (scope !== 'ei') return [...rows];
  const keep = eiTechs(v);
  return rows.filter((r) => keep.has(r.tech));
}

/** Gap rows grouped by quadrant (opportunity, established, legacy), each group with the focal country's needs first, then by recommended cells, capability and need. */
export function gapGroups(gaps: readonly AgrGap[], partner?: string | null, tech?: string | null): { quadrant: Quadrant; rows: AgrGap[] }[] {
  const keep = gaps.filter((g) => (!partner || g.partner === partner) && (!tech || g.tech === tech));
  // the focal country's own needs first (who could supply it), then what it could supply to others
  const order = (a: AgrGap, b: AgrGap) => Number(a.side !== 'needs') - Number(b.side !== 'needs') || b.recommended - a.recommended || (b.capability ?? -1) - (a.capability ?? -1) || (b.need ?? -1) - (a.need ?? -1) || a.tech.localeCompare(b.tech) || a.partner.localeCompare(b.partner);
  return QUADRANT_ORDER.map((quadrant) => ({ quadrant, rows: keep.filter((g) => g.quadrant === quadrant).sort(order) })).filter((x) => x.rows.length);
}

const PRIORITY_BASIS: Readonly<Record<string, string>> = {
  policy_priority: 'policy priority',
  priority: 'policy priority',
  secure: 'strategic choice Secure',
  partner: 'strategic choice Partner',
  choice_secure: 'strategic choice Secure',
  choice_partner: 'strategic choice Partner',
};

/** The priority basis in words ("policy priority; strategic choice Secure"). */
export function priorityBasisWords(basis: string): string {
  return basis
    .split(/[|;,]/)
    .map((b) => b.trim())
    .filter(Boolean)
    .map((b) => PRIORITY_BASIS[b] ?? humanize(b))
    .join('; ');
}

/** How many suppliers an execution-gap sentence names before "and N others" (recommended first, then by capability). */
export const EXEC_NAMES_SHOWN = 6;

/** One execution gap as a sentence. */
export function executionGapText(g: AgrExecutionGap, ctx: GapContext): string {
  const f = ctx.name(ctx.focal);
  const pr = percentileWords(g.priority);
  const why = `${priorityBasisWords(g.priority_basis)}${pr ? `, priority ${pr} within ${f}` : ''}`;
  const n = g.suppliers_without_coverage.length;
  const what = g.stages?.length ? `${g.tech} at ${stageListWords(g.stages)}` : g.tech;
  if (!n) return `${what} is a priority for ${f} (${why}), but ${NO_AGREEMENT} with a capable supplier covers it at an established level.`;
  const shown = g.suppliers_without_coverage.slice(0, EXEC_NAMES_SHOWN).map(ctx.name);
  const names = n > EXEC_NAMES_SHOWN ? `${shown.join(', ')} and ${plural(n - EXEC_NAMES_SHOWN, 'other')}` : listWords(shown);
  return `${what} is a priority for ${f} (${why}), but none of its agreements in these sources with ${plural(n, 'capable, aligned supplier')} (${names}) covers it at an established level.`;
}

// ---- the pair -----------------------------------------------------------------------------------------------

export interface Architecture {
  strategic: boolean;
  trade: boolean;
  tradeDepth: number | null;
  sectoral: boolean;
  /** the pair's sectors, established first */
  sectors: { id: string; grade: CodeGrade }[];
}

export function architectureOf(p: AgrPartner): Architecture {
  const seen = new Map<string, CodeGrade>();
  for (const [c, g] of p.sectors) if (seen.get(c) !== 'specific') seen.set(c, g);
  const sectors = [...seen].map(([id, grade]) => ({ id, grade })).sort((a, b) => (a.grade === b.grade ? a.id.localeCompare(b.id) : a.grade === 'specific' ? -1 : 1));
  return { strategic: p.has_sp, trade: p.has_trade, tradeDepth: p.trade_depth_index, sectoral: p.has_sectoral, sectors };
}

/** The economies with a file, in name order (the Economy filter). */
export function countryChoices(m: AgreementsManifest): { iso3: string; name: string }[] {
  return Object.keys(m.files.countries ?? {})
    .map((iso3) => ({ iso3, name: nameOf(m.names, iso3) }))
    .sort((a, b) => a.name.localeCompare(b.name, 'en'));
}

/** The partners of an economy's file, in name order (the Partner filter). */
export function partnerChoices(file: AgreementsCountry, names: Readonly<Record<string, string>>): { iso3: string; name: string }[] {
  return file.partners.map((p) => ({ iso3: p.partner, name: nameOf(names, p.partner) })).sort((a, b) => a.name.localeCompare(b.name, 'en'));
}

/** The year range of the agreements of a set of files' pairs (the Years filter's choices), newest last. */
export function yearChoices(asOf: string, first = 1990): number[] {
  const last = yearOf(asOf) ?? new Date().getFullYear();
  const out: number[] = [];
  for (let y = first; y <= last; y++) out.push(y);
  return out;
}

/** The first year among some dates (the Years filter's first choice), or `fallback` when none is dated. */
export function firstYear(dates: Iterable<string | null | undefined>, fallback = 1990): number {
  let lo = Infinity;
  for (const d of dates) {
    const y = yearOf(d);
    if (y !== null && y < lo) lo = y;
  }
  return Number.isFinite(lo) ? lo : fallback;
}

/** The stages in words (the six stages, and cross_stage). */
export const SEGMENT_LABEL: Readonly<Record<string, string>> = { cross_stage: 'Across stages' };
export const segmentLabel = (s: string): string => SEGMENT_LABEL[s] ?? s;

// ---- the page URL ------------------------------------------------------------------------------------------

export const AGREEMENTS_PATH = '/aib/agreements';

/** The page's own URL keys beyond the filters: the partner ranking's sort key and the network's group. */
export const PAGE_KEYS = ['sort', 'group'] as const;

export type NetworkGroup = 'core' | 'coalition' | 'top';
export const NETWORK_GROUPS: readonly NetworkGroup[] = ['core', 'coalition', 'top'];
export const DEFAULT_GROUP: NetworkGroup = 'core';
export const networkGroupOf = (x: string | null): NetworkGroup => (NETWORK_GROUPS.includes(x as NetworkGroup) ? (x as NetworkGroup) : DEFAULT_GROUP);

/**
 * The href of the page with an economy (and a partner) chosen, every other key of `keep` (the scope, the
 * filters, the sort key, the scenario toggle) carried; no economy: the all-economies view.
 */
export function agreementsHref(country: string | null, partner?: string | null, keep?: URLSearchParams | string): string {
  const q = new URLSearchParams(keep);
  if (country) q.set('country', country.toUpperCase());
  else q.delete('country');
  if (country && partner && partner.toUpperCase() !== country.toUpperCase()) q.set('partner', partner.toUpperCase());
  else q.delete('partner');
  const s = q.toString();
  return `${AGREEMENTS_PATH}${s ? `?${s}` : ''}`;
}

/** The query string with every narrowing filter removed (who, the scope and the page keys kept). */
export function clearFiltersSearch(q: URLSearchParams): URLSearchParams {
  const out = new URLSearchParams(q);
  for (const k of NARROWING_KEYS) out.delete(k);
  return out;
}
