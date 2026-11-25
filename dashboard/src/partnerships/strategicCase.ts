// The strategic case of a pair (Explorer usability pass, 2026-11-25; brief s.2A Partnership): "What is the
// strategic case for these countries working together?" answered from the relationship thesis already built
// (thesis.ts pairThesis: the export's logic labels and the two sides' compact roles in their priority
// compacts). Nothing is scored or re-derived; the six logic labels are only regrouped under the five
// purposes the brief names:
//
//   securing supply                       trade (coordinated trade moves goods between them)
//   combining complementary capabilities  technology (one side is the specialized technology supplier), and a
//                                         priority compact where both supply but in different roles
//   expanding a market                    market access (one side is the demand anchor the other supplies)
//   building new capacity                 investment; development / capacity building
//   reducing concentration                resilience / second sourcing
//
// The compact evidence of each label is looked up with the same role rules as thesis.ts logicRows() (and
// aib_relationship_theses() in R/aib/aib_strategic.R), on every priority compact rather than the first one:
// a test pins that the first compact found here is the one the thesis names. The principal purposes are the
// ones supported in the most priority compacts (a count, ties in the brief's order); a purpose resting only
// on the analysis's trade or investment relationships comes after those. A pair without a thesis, or a thesis
// with no logic, has no strategic case: friendly relations or voting alignment are never offered as one.
// Pure (no React, no IO).

import type { AibCompactRole, AibStrategic, AibStrategicCompact } from '../aib/contract.ts';
import { listWords } from '../aib/derive/format.ts';
import { countryStrategicOf } from '../aib/derive/strategic.ts';
import { leadRole, LOGIC_INFO, type ContributionRow, type PairThesis } from './thesis.ts';

export type PurposeId = 'secure' | 'complement' | 'market' | 'capacity' | 'concentration';

/** The brief's five purposes, in its order (the tie-break). */
export const PURPOSE_ORDER: readonly PurposeId[] = ['secure', 'complement', 'market', 'capacity', 'concentration'];

export const PURPOSE_WORDS: Readonly<Record<PurposeId, string>> = {
  secure: 'securing supply',
  complement: 'combining complementary capabilities',
  market: 'expanding a market',
  capacity: 'building new capacity',
  concentration: 'reducing concentration',
};

/** Which purpose each logic label of the export supports. */
export const LOGIC_PURPOSE: Readonly<Record<string, PurposeId>> = {
  trade: 'secure',
  technology: 'complement',
  'market access': 'market',
  investment: 'capacity',
  'development / capacity building': 'capacity',
  'resilience / second sourcing': 'concentration',
};

/** At most this many purposes lead the answer. */
export const LEAD_MAX = 2;

const SUPPLY_ROLES = new Set(['Resource supplier', 'Processing anchor', 'Specialized technology supplier', 'Scale manufacturer']);
const TECH = 'Specialized technology supplier';
const DEMAND = 'Demand anchor';
const HOST = 'Capacity-building host';
const INVESTOR = 'Investment anchor';
const DIVERSIFY = 'Diversification partner';
const BRIDGE = 'Bridge / coordination partner';

const has = (r: AibCompactRole | null | undefined, f: string): boolean => !!r && r.functions.includes(f);
const lower = (s: string): string => (s ? s[0].toLowerCase() + s.slice(1) : s);

type Namer = (iso3: string) => string;

/** One priority compact that supports a purpose, and what each side does there, in words. */
export interface PurposeSupport {
  compact: AibStrategicCompact;
  /** the logic label read here ("technology"), or null for complementary supply roles */
  label: string | null;
  /** "Japan supplies specialized technology (Transformers); the United States takes part as demand anchor" */
  words: string;
}

export interface Purpose {
  id: PurposeId;
  words: string;
  /** the export's logic labels behind it (in the export's order); empty for complementary supply roles alone */
  labels: string[];
  /** the priority compacts where the compact roles support it (each once, in the thesis's order) */
  support: PurposeSupport[];
  /** what the label means where no compact supports it (the analysis's trade or investment relationships) */
  meaning: string | null;
}

export interface StrategicCase {
  /** 'case': the thesis gives at least one purpose; 'none': no strategic case is identified */
  state: 'case' | 'none';
  /** every purpose that holds, the principal ones first */
  purposes: Purpose[];
  /** the principal purposes (at most LEAD_MAX) */
  lead: PurposeId[];
  /** the answer in one sentence */
  sentence: string;
}

/** A side's lead compact role in words ("specialized technology supplier"). */
const roleOf = (r: AibCompactRole | null): string => lower(leadRole(r) ?? 'partner');
/** " (Transformers)": the step of its largest position, for a supplying role only. */
const stepOf = (r: AibCompactRole | null): string => (r?.leadStep && SUPPLY_ROLES.has(leadRole(r) ?? '') ? ` (${r.leadStep})` : '');

/** What the two sides do in one priority compact, in the words of the label it supports (X: the side the rule holds for). */
type Words = (X: string, x: AibCompactRole, Y: string, y: AibCompactRole) => string;

/**
 * The priority compacts where `test` holds one way or the other (the same rules as thesis.ts logicRows()),
 * with the side it holds for first.
 */
function supportOf(rows: readonly ContributionRow[], label: string | null, a: string, b: string, words: Words, test: (x: AibCompactRole, y: AibCompactRole, xIsA: boolean) => boolean): PurposeSupport[] {
  const out: PurposeSupport[] = [];
  for (const row of rows) {
    if (!row.a || !row.b) continue;
    const ab = test(row.a, row.b, true);
    const ba = !ab && test(row.b, row.a, false);
    if (!ab && !ba) continue;
    const [X, x, Y, y] = ab ? [a, row.a, b, row.b] : [b, row.b, a, row.a];
    out.push({ compact: row.compact, label, words: words(X, x, Y, y) });
  }
  return out;
}

/**
 * The strategic case of a pair from its thesis. `s` is read only for the Capital & Investment Anchor role
 * (the development rule's alternative to an investment anchor), as in thesis.ts.
 */
export function strategicCase(s: AibStrategic, t: PairThesis, name: Namer): StrategicCase {
  const a = t.a.iso3;
  const b = t.b.iso3;
  const pair = `${name(a)} and ${name(b)}`;
  if (t.state !== 'thesis' || !t.logic.length) {
    return { state: 'none', purposes: [], lead: [], sentence: NO_CASE };
  }
  const cia = (iso: string) => {
    const c = countryStrategicOf(s, iso);
    return !!c && [...c.primary, ...c.secondary].some((r) => r.code === 'CIA');
  };
  const rows = t.compacts;
  const labels = t.logic.map((l) => l.label);
  const n = name;
  const rules: Record<string, { test: (x: AibCompactRole, y: AibCompactRole, xIsA: boolean) => boolean; words: Words }> = {
    technology: {
      test: (x, y) => leadRole(x) === TECH && leadRole(y) !== TECH,
      words: (X, x, Y, y) => `${n(X)} supplies specialized technology${stepOf(x)}; ${n(Y)} takes part as ${roleOf(y)}`,
    },
    'development / capacity building': {
      test: (x, y, xIsA) => has(x, HOST) && (has(y, INVESTOR) || cia(xIsA ? b : a)),
      words: (X, _x, Y) => `${n(X)} can host new capacity; ${n(Y)} can invest`,
    },
    'market access': {
      test: (x, y) => leadRole(x) === DEMAND && SUPPLY_ROLES.has(leadRole(y) ?? ''),
      words: (X, _x, Y, y) => `${n(X)} is the main buyer (demand anchor); ${n(Y)} supplies it as ${roleOf(y)}${stepOf(y)}`,
    },
    'resilience / second sourcing': {
      test: (x, y) => leadRole(x) === DEMAND && (has(y, DIVERSIFY) || has(y, BRIDGE)),
      words: (X, _x, Y) => `${n(Y)} can be a second source for ${n(X)}`,
    },
  };
  const by = new Map<PurposeId, Purpose>();
  const purpose = (id: PurposeId): Purpose => {
    let p = by.get(id);
    if (!p) {
      p = { id, words: PURPOSE_WORDS[id], labels: [], support: [], meaning: null };
      by.set(id, p);
    }
    return p;
  };
  const addSupport = (p: Purpose, xs: PurposeSupport[]) => {
    for (const x of xs) if (!p.support.some((y) => y.compact.id === x.compact.id)) p.support.push(x);
  };
  for (const label of labels) {
    const id = LOGIC_PURPOSE[label];
    if (!id) continue;
    const p = purpose(id);
    p.labels.push(label);
    const rule = rules[label];
    if (rule) addSupport(p, supportOf(rows, label, a, b, rule.words, rule.test));
    // as thesis.ts: a label whose lookup finds no compact keeps what it means
    if (!p.meaning) p.meaning = LOGIC_INFO[label]?.meaning ?? null;
  }
  // complementary capabilities from the compact roles: both supply in a priority compact, in different roles
  const both = supportOf(rows, null, a, b, (X, x, Y, y) => `${n(X)} as ${roleOf(x)}${stepOf(x)}; ${n(Y)} as ${roleOf(y)}${stepOf(y)}`, (x, y) => SUPPLY_ROLES.has(leadRole(x) ?? '') && SUPPLY_ROLES.has(leadRole(y) ?? '') && leadRole(x) !== leadRole(y));
  if (both.length) addSupport(purpose('complement'), both);
  for (const p of by.values()) {
    const order = new Map(rows.map((r, i) => [r.compact.id, i]));
    p.support.sort((x, y) => (order.get(x.compact.id) ?? 0) - (order.get(y.compact.id) ?? 0));
    if (p.support.length) p.meaning = null;
  }
  const purposes = [...by.values()].sort((x, y) => y.support.length - x.support.length || PURPOSE_ORDER.indexOf(x.id) - PURPOSE_ORDER.indexOf(y.id));
  if (!purposes.length) return { state: 'none', purposes: [], lead: [], sentence: NO_CASE };
  const withCompacts = purposes.filter((p) => p.support.length > 0);
  const lead = (withCompacts.length ? withCompacts : purposes).slice(0, LEAD_MAX).map((p) => p.id);
  const sentence = `For ${pair}, the strategic case is mainly ${listWords(lead.map((id) => PURPOSE_WORDS[id]))}.`;
  return { state: 'case', purposes, lead, sentence };
}

/** The answer for a pair the analysis writes no thesis for (or a thesis with no logic). */
export const NO_CASE = 'No strategic case is identified in the Explorer’s analysis for this pair.';

/** Why the context below is not a case on its own. */
export const NO_CASE_NOTE = 'Friendly relations or voting alignment alone are not a strategic case. What links them today is set out below.';
