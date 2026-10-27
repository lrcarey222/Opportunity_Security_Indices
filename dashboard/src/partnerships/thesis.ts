// The bilateral relationship thesis (2026-10-20; strategic task spec s.19): for a pair of economies, a short
// reading of the strategic layer above the Today / OSI recommends / AIB model detail of the partnership
// pages. Template-driven from the export (doc.strategic: the theses, the compact roles and the Strategic
// AIB Roles); no score, no ranking of partners, nothing re-derived:
//
//   Complementarity   each side's Strategic AIB Roles, and in each priority compact its compact role (the
//                     export's detail, the step it is strongest in and that share)
//   Priority compacts the thesis's compacts (shared compacts by the smaller of the two relevances, at most
//                     three: aib_relationship_theses() in R/aib/aib_strategic.R)
//   AIB logic         the export's labels, in its fixed order, each with the compact-role evidence in the
//                     priority compacts that supports it, looked up with the same rules as the R function
//                     (a lookup that finds nothing falls back to what the label means: the label is never
//                     dropped or added)
//
// The model writes a thesis for every pair of allied economies with a modelled trade or investment
// relationship in Managed Interdependence or Decoupling. Any other pair gets one plain line saying why
// there is none, and the strategic compacts both take part in with each side's role. The thesis is
// structural: the same under every scenario. Pure (no React, no IO).

import { AIB_THESIS_LOGIC, type AibCompactRole, type AibStrategic, type AibStrategicCompact } from '../aib/contract.ts';
import { fmtShare, listWords } from '../aib/derive/format.ts';
import { compactRolesOfCountry, countryStrategicOf, roleName, thesesOf, thesisOf, type StrategicRole } from '../aib/derive/strategic.ts';

/** What each logic label is called and means, in plain words (the fallback when no evidence is found). */
export const LOGIC_INFO: Readonly<Record<string, { title: string; meaning: string }>> = {
  trade: { title: 'Trade', meaning: 'The model’s coordinated trade moves goods between them under Managed Interdependence or Decoupling.' },
  investment: { title: 'Investment', meaning: 'In the model’s investment simulation, new capacity in one of them serves the other’s demand.' },
  technology: { title: 'Technology', meaning: 'In a priority compact, one of them is the specialized technology supplier and the other is not.' },
  'development / capacity building': {
    title: 'Development / capacity building',
    meaning: 'In a priority compact, one of them can host new capacity and the other is an investment anchor or a Capital & Investment Anchor.',
  },
  'market access': { title: 'Market access', meaning: 'In a priority compact, one of them is the demand anchor and the other supplies it.' },
  'resilience / second sourcing': {
    title: 'Resilience / second sourcing',
    meaning: 'In a priority compact, one of them is the demand anchor and the other can second-source for it as a diversification or bridge partner.',
  },
};

/** A thesis keeps at most three priority compacts (aib_relationship_theses()); a pair without one shows as many. */
export const PRIORITY_MAX = 3;

/** The logic labels in the export's fixed order. */
export const LOGIC_ORDER: readonly string[] = AIB_THESIS_LOGIC;

/** From this many logics the headline calls the relationship broad and says how many of the six hold. */
export const BROAD_AT = 4;
const NUMBER_WORDS = ['no', 'one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight'];

/** The compact roles that supply (the R function's supply roles: market access). */
const SUPPLY_ROLES = new Set(['Resource supplier', 'Processing anchor', 'Specialized technology supplier', 'Scale manufacturer']);
const TECH = 'Specialized technology supplier';
const DEMAND = 'Demand anchor';
const HOST = 'Capacity-building host';
const INVESTOR = 'Investment anchor';
const DIVERSIFY = 'Diversification partner';
const BRIDGE = 'Bridge / coordination partner';

/** A compact role without the "(non-allied supplier)" mark (the lead role the rules compare). */
export const leadRole = (r: AibCompactRole | null | undefined): string | null => (r ? r.role.replace(/ \(non-allied supplier\)$/, '') : null);
const has = (r: AibCompactRole | null | undefined, f: string): boolean => !!r && r.functions.includes(f);
const lower = (s: string): string => (s ? s[0].toLowerCase() + s.slice(1) : s);
const cap = (s: string): string => (s ? s[0].toUpperCase() + s.slice(1) : s);
const an = (w: string): string => (/^[aeiou]/i.test(w) ? 'an' : 'a');

export interface ThesisSide {
  iso3: string;
  /** the strategic layer assesses it (every allied economy, and the major non-allied suppliers) */
  assessed: boolean;
  allied: boolean;
  primary: StrategicRole[];
  secondary: StrategicRole[];
}

/** One compact both take part in, with what each side contributes there (its compact role). */
export interface ContributionRow {
  compact: AibStrategicCompact;
  a: AibCompactRole | null;
  b: AibCompactRole | null;
}

/** One logic label of a thesis with its evidence in words. */
export interface LogicRow {
  label: string;
  title: string;
  /** the compact-role evidence, or what the label means when none is found */
  evidence: string;
  /** the compact the evidence is from (null: trade, investment, or no evidence found) */
  compact: AibStrategicCompact | null;
}

/**
 * thesis: the model writes one; no-relationship: both are allied but the model links them by no trade or
 * investment relationship; outside: at least one is not an allied economy.
 */
export type ThesisState = 'thesis' | 'no-relationship' | 'outside';

export interface PairThesis {
  a: ThesisSide;
  b: ThesisSide;
  state: ThesisState;
  /** the thesis's priority compacts; without a thesis, every compact both take part in (the most relevant to both first) */
  compacts: ContributionRow[];
  /** the compacts both take part in beyond `compacts` (a thesis keeps at most three) */
  moreShared: AibStrategicCompact[];
  logic: LogicRow[];
  /** one sentence: the relationship in a line, or why there is no thesis */
  headline: string;
}

function sideOf(s: AibStrategic, iso3: string): ThesisSide {
  const c = countryStrategicOf(s, iso3);
  return { iso3, assessed: !!c, allied: !!c?.allied, primary: c?.primary ?? [], secondary: c?.secondary ?? [] };
}

/** The compacts both take part in, as R orders them: by the smaller of the two relevances (most first), then id. */
function sharedRows(s: AibStrategic, a: string, b: string): ContributionRow[] {
  const ra = new Map(compactRolesOfCountry(s, a).map((r) => [r.compact, r]));
  const rows: (ContributionRow & { rel: number })[] = [];
  for (const rb of compactRolesOfCountry(s, b)) {
    const x = ra.get(rb.compact);
    const compact = x ? s.compacts.find((c) => c.id === rb.compact) : undefined;
    if (!x || !compact) continue;
    rows.push({ compact, a: x, b: rb, rel: Math.min(x.relevance ?? -1, rb.relevance ?? -1) });
  }
  rows.sort((p, q) => q.rel - p.rel || p.compact.id.localeCompare(q.compact.id));
  return rows.map(({ compact, a: ra2, b: rb2 }) => ({ compact, a: ra2, b: rb2 }));
}

/** "Japan" or "the United States": the caller's prose name. */
type Namer = (iso3: string) => string;

/** The first priority compact where `test` holds one way or the other; `x` is the side it holds for. */
function findEvidence(rows: readonly ContributionRow[], test: (x: AibCompactRole, y: AibCompactRole, xIsA: boolean) => boolean): { row: ContributionRow; x: 'a' | 'b'; n: number } | null {
  let first: { row: ContributionRow; x: 'a' | 'b' } | null = null;
  let n = 0;
  for (const row of rows) {
    if (!row.a || !row.b) continue;
    const ab = test(row.a, row.b, true);
    const ba = test(row.b, row.a, false);
    if (!ab && !ba) continue;
    n += 1;
    if (!first) first = { row, x: ab ? 'a' : 'b' };
  }
  return first ? { ...first, n } : null;
}

const moreWords = (n: number): string => (n > 1 ? ` (and in ${n - 1} more priority ${n - 1 === 1 ? 'compact' : 'compacts'})` : '');

/** The evidence of each logic label, the same rules as aib_relationship_theses() (read on the priority compacts only). */
function logicRows(s: AibStrategic, labels: readonly string[], rows: readonly ContributionRow[], a: string, b: string, name: Namer): LogicRow[] {
  const cia = (iso: string) => {
    const c = countryStrategicOf(s, iso);
    return !!c && [...c.primary, ...c.secondary].some((r) => r.code === 'CIA');
  };
  const isoOf = (side: 'a' | 'b') => (side === 'a' ? a : b);
  const other = (side: 'a' | 'b'): 'a' | 'b' => (side === 'a' ? 'b' : 'a');
  const ciaName = roleName(s, 'CIA');
  const ordered = [...labels].sort((p, q) => {
    const i = LOGIC_ORDER.indexOf(p);
    const j = LOGIC_ORDER.indexOf(q);
    return (i < 0 ? 99 : i) - (j < 0 ? 99 : j);
  });
  return ordered.map((label): LogicRow => {
    const info = LOGIC_INFO[label] ?? { title: cap(label), meaning: `The model’s evidence supports ${label}.` };
    const out = (evidence: string, compact: AibStrategicCompact | null = null): LogicRow => ({ label, title: info.title, evidence, compact });
    if (label === 'technology') {
      const e = findEvidence(rows, (x, y) => leadRole(x) === TECH && leadRole(y) !== TECH);
      if (!e) return out(info.meaning);
      const X = e.x;
      const Y = other(X);
      const y = e.row[Y];
      return out(
        `In ${e.row.compact.name}, ${name(isoOf(X))} is the specialized technology supplier; ${name(isoOf(Y))} takes part as ${an(leadRole(y) ?? '')} ${lower(leadRole(y) ?? 'partner')}${moreWords(e.n)}.`,
        e.row.compact,
      );
    }
    if (label === 'development / capacity building') {
      const e = findEvidence(rows, (x, y, xIsA) => has(x, HOST) && (has(y, INVESTOR) || cia(xIsA ? b : a)));
      if (!e) return out(info.meaning);
      const X = e.x;
      const Y = other(X);
      const investor = has(e.row[Y], INVESTOR) ? 'is an investment anchor there' : `is a ${ciaName}`;
      return out(`In ${e.row.compact.name}, ${name(isoOf(X))} can host new capacity and ${name(isoOf(Y))} ${investor}${moreWords(e.n)}.`, e.row.compact);
    }
    if (label === 'market access') {
      const e = findEvidence(rows, (x, y) => leadRole(x) === DEMAND && SUPPLY_ROLES.has(leadRole(y) ?? ''));
      if (!e) return out(info.meaning);
      const X = e.x;
      const Y = other(X);
      return out(`In ${e.row.compact.name}, ${name(isoOf(X))} is the demand anchor and ${name(isoOf(Y))} supplies it as ${an(leadRole(e.row[Y]) ?? '')} ${lower(leadRole(e.row[Y]) ?? '')}${moreWords(e.n)}.`, e.row.compact);
    }
    if (label === 'resilience / second sourcing') {
      const e = findEvidence(rows, (x, y) => leadRole(x) === DEMAND && (has(y, DIVERSIFY) || has(y, BRIDGE)));
      if (!e) return out(info.meaning);
      const X = e.x;
      const Y = other(X);
      const as = has(e.row[Y], DIVERSIFY) ? 'a diversification partner' : 'a bridge / coordination partner';
      return out(`In ${e.row.compact.name}, ${name(isoOf(Y))} can second-source for ${name(isoOf(X))}, the demand anchor, as ${as}${moreWords(e.n)}.`, e.row.compact);
    }
    return out(info.meaning);
  });
}

/**
 * "Japan and Korea: a trade, technology and market access relationship, with ... as the priority compacts.";
 * from four logics on, "a broad relationship on five of the six AIB logics (trade, ...)".
 */
function headlineOf(state: ThesisState, A: ThesisSide, B: ThesisSide, compacts: readonly ContributionRow[], logic: readonly LogicRow[], name: Namer, more = 0): string {
  const pair = `${cap(name(A.iso3))} and ${name(B.iso3)}`;
  const names = listWords(compacts.map((r) => r.compact.name));
  if (state === 'thesis') {
    const words = logic.map((l) => l.label);
    // four or more logics read as a list in brackets, with how many of the six hold (a count, not a score)
    const n = (k: number) => NUMBER_WORDS[k] ?? String(k);
    const what = !words.length
      ? ': a modelled relationship'
      : words.length >= BROAD_AT
        ? `: a broad relationship on ${words.length >= LOGIC_ORDER.length ? `all ${n(words.length)}` : `${n(words.length)} of the ${n(LOGIC_ORDER.length)}`} AIB logics (${listWords(words)})`
        : `: ${an(words[0])} ${listWords(words)} relationship`;
    const where = compacts.length
      ? `, with ${names} as ${compacts.length === 1 ? 'the priority compact' : 'the priority compacts'}`
      : ', in no strategic compact both take part in';
    return `${pair}${what}${where}.`;
  }
  const n = compacts.length + more;
  const both = !n
    ? ' They share no strategic compact.'
    : more
      ? ` Both take part in ${n} strategic compacts, among them ${names}.`
      : ` Both take part in ${n === 1 ? 'one strategic compact' : `${n} strategic compacts`}: ${names}.`;
  if (state === 'no-relationship') return `The model links ${name(A.iso3)} and ${name(B.iso3)} by no trade or investment relationship under Managed Interdependence or Decoupling, so it writes no relationship thesis for them.${both}`;
  const out = [A, B].filter((x) => !x.allied);
  const who = out.map((x) => `${name(x.iso3)} is ${x.assessed ? 'a major supplier outside the allied economies' : 'not one of the allied economies'}`);
  return `The model writes relationship theses for pairs of allied economies: ${listWords(who)}.${both}`;
}

/**
 * The relationship thesis of a pair in the order asked (a first), or null when either is missing or both
 * are the same economy. `name`: an economy's prose name ("the United States").
 */
export function pairThesis(s: AibStrategic, a: string | null | undefined, b: string | null | undefined, name: Namer): PairThesis | null {
  if (!a || !b) return null;
  const A = a.toUpperCase();
  const B = b.toUpperCase();
  if (A === B) return null;
  const sa = sideOf(s, A);
  const sb = sideOf(s, B);
  const shared = sharedRows(s, A, B);
  const t = thesisOf(s, A, B);
  if (t) {
    const ids = new Set(t.compacts.map((c) => c.id));
    const compacts: ContributionRow[] = t.complementarity.map((c) => ({ compact: c.compact, a: c.a, b: c.b }));
    // the evidence reads the priority compacts only, as R does (since 2026-10-21)
    const logic = logicRows(s, t.logic, compacts, A, B, name);
    return { a: sa, b: sb, state: 'thesis', compacts, moreShared: shared.filter((r) => !ids.has(r.compact.id)).map((r) => r.compact), logic, headline: headlineOf('thesis', sa, sb, compacts, logic, name) };
  }
  const state: ThesisState = sa.allied && sb.allied ? 'no-relationship' : 'outside';
  // as many compacts as a thesis keeps, the rest named after them
  const top = shared.slice(0, PRIORITY_MAX);
  const rest = shared.slice(PRIORITY_MAX);
  return { a: sa, b: sb, state, compacts: top, moreShared: rest.map((r) => r.compact), logic: [], headline: headlineOf(state, sa, sb, top, [], name, rest.length) };
}


/**
 * What a side brings to a compact, in words: the export's detail (its leading functions), the step of its
 * largest position and that share ("Processing anchor / scale manufacturer; largest position in Refined
 * chemicals (18% of world exports or production)").
 */
export function contributionWords(r: AibCompactRole | null | undefined): string {
  if (!r) return 'no role in this compact';
  const what = r.detail || r.role;
  if (!r.leadStep) return what;
  const share = r.leadShare !== null && r.leadShare !== undefined ? ` (${fmtShare(r.leadShare, 0)}${r.leadBasis ? ` of ${r.leadBasis.replace(/^share of /, '')}` : ''})` : '';
  return `${what}; largest position in ${r.leadStep}${share}`;
}

/** A side's Strategic AIB Roles in words ("Capital & Investment Anchor and Demand & Market Anchor; secondary: Coordination / Bridge Hub"). */
export function strategicRoleWords(side: ThesisSide): string {
  if (!side.assessed) return 'not assessed by the strategic layer (it is not an allied economy or a major supplier)';
  if (!side.primary.length && !side.secondary.length) return 'no Strategic AIB Role at the model’s bars (described by its compact roles)';
  const p = side.primary.length ? listWords(side.primary.map((r) => r.name)) : 'no primary Strategic AIB Role';
  const sec = side.secondary.length ? `; secondary: ${listWords(side.secondary.map((r) => r.name))}` : '';
  return `${p}${sec}`;
}

/**
 * The partners of an economy's theses, the ones with the most priority compacts first (then the most
 * logic labels, then by code: thesesOf's order), at most `n`.
 */
export function thesisPartners(s: AibStrategic | null | undefined, iso3: string | null | undefined, n = Infinity): string[] {
  if (!s || !iso3) return [];
  return thesesOf(s, iso3)
    .slice(0, n)
    .map((t) => t.b);
}
