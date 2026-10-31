// The Country strategy sentence (spec XI.2, XI.7; design section 4): one analytical sentence generated from
// the node choices, never from invented narrative. Each clause names the nodes behind it, so a clause exists
// only when at least one node backs it:
//   preserve: the Preserve nodes with the strongest Competitive Position;
//   secure:   the Secure nodes with the highest supply-chain need, each with its rationale;
//   catch up: the Catch Up nodes with the largest prize;
//   partner:  the Partner nodes with the highest need;
//   accelerate: the Deployment & Use technologies with the highest deployment need.
// Up to three nodes are named per clause; a longer group is counted and "led by" the three. Node names come
// from the core's phrase table (meta.phrases, "battery materials refining"), else "<technology> <stage word>".
// A list whose phrases carry their own "and" ("wafers and PV cells") writes those as "&", so the list's
// "and" stays the only one ("battery recycling, wafers & PV cells and PV modules & inverters").
// The NEIS copy refresh (2026-10-30, the guide's rules for generated sentences): plain verb phrases ("rely on
// partners for", vocabulary CHOICE_PROSE), "supply-chain steps" for nodes, the Secure reasons in plain words
// ("chokepoint risk"), deployment kept in its own clause; Watch is never a clause.
// Pure: no React.

import { CHOICE_PROSE } from '../content/vocabulary.ts';
import type { PostureCell, PostureMeta, SecurityRationale, Verb } from './contract.ts';
import { RATIONALE_SHORT, verbOf } from './grammar.ts';
import { listAnd, plural } from './derive.ts';

/** The stage word of a fallback phrase ("batteries refining", "solar recycling"). */
const STAGE_WORD: Record<string, string> = {
  Extraction: 'extraction', Refining: 'refining', Components: 'components', Assembly: 'assembly', Deployment: 'deployment', Recovery: 'recycling',
};

/** A node's noun phrase: the phrase table's, else "<technology> <stage word>"; at Deployment the technology alone (as in "accelerate deployment of solar"). */
export function nodePhrase(meta: Pick<PostureMeta, 'phrases'>, tech: string, stage: string): string {
  const p = meta.phrases[`${tech}|${stage}`];
  if (p) return p;
  if (stage === 'Deployment') return tech.toLowerCase();
  return `${tech.toLowerCase()} ${STAGE_WORD[stage] ?? stage.toLowerCase()}`;
}

/** The verbs of the sentence, in its order. */
export const SENTENCE_ORDER: Exclude<Verb, 'watch'>[] = ['preserve', 'secure', 'catch_up', 'partner', 'accelerate'];
const MAX_NAMED = 3;

export interface StrategyClause {
  verb: Exclude<Verb, 'watch'>;
  text: string;
  /** the cell keys of the nodes named in the clause ("Tech|Stage"), in order */
  keys: string[];
  /** how many nodes take this choice */
  total: number;
}

const desc = (v: number | null) => (v === null ? -Infinity : v);
const RANK: Record<Exclude<Verb, 'watch'>, (a: PostureCell, b: PostureCell) => number> = {
  preserve: (a, b) => desc(b.cp) - desc(a.cp) || desc(b.prize) - desc(a.prize),
  secure: (a, b) => desc(b.need) - desc(a.need),
  catch_up: (a, b) => desc(b.prize) - desc(a.prize) || desc(b.cp) - desc(a.cp),
  partner: (a, b) => desc(b.need) - desc(a.need),
  accelerate: (a, b) => desc(b.need) - desc(a.need),
};

const LEAD: Record<Exclude<Verb, 'watch'>, [whole: string, counted: (n: number) => string]> = {
  preserve: ['preserve its strongest positions in', (n) => `preserve ${n} strong positions, led by`],
  secure: ['secure', (n) => `secure ${n} vulnerable supply-chain steps, led by`],
  catch_up: [CHOICE_PROSE.catch_up, (n) => `${CHOICE_PROSE.catch_up} ${n} supply-chain steps, led by`],
  partner: [CHOICE_PROSE.partner, (n) => `${CHOICE_PROSE.partner} ${n} supply-chain steps, led by`],
  accelerate: ['accelerate deployment of', (n) => `accelerate deployment of ${n} technologies, led by`],
};

/**
 * Node phrases as a prose list ("A, B and C"). When any phrase of a list of two or more has its own "and",
 * every phrase's inner "and" becomes "&" (as in the stage names, "Refining & Processing"), so the list's
 * own "and" is unambiguous; one phrase alone keeps its words.
 */
export function listPhrases(words: readonly string[]): string {
  if (words.length < 2 || !words.some((w) => / and /.test(w))) return listAnd(words);
  return listAnd(words.map((w) => w.replace(/ and /g, ' & ')));
}

/** The clauses of one economy's sentence, in sentence order; a verb with no node has no clause. */
export function strategyClauses(cells: readonly PostureCell[], meta: Pick<PostureMeta, 'phrases'>): StrategyClause[] {
  const out: StrategyClause[] = [];
  for (const verb of SENTENCE_ORDER) {
    const group = cells.filter((c) => verbOf(c) === verb).sort(RANK[verb]);
    if (!group.length) continue;
    const named: PostureCell[] = [];
    const seen = new Set<string>();
    for (const c of group) {
      const ph = nodePhrase(meta, c.tech, c.stage);
      if (seen.has(ph)) continue;
      seen.add(ph);
      named.push(c);
      if (named.length === MAX_NAMED) break;
    }
    const words = named.map((c) => {
      const ph = nodePhrase(meta, c.tech, c.stage);
      return verb === 'secure' && c.rationale ? `${ph} (${RATIONALE_SHORT[c.rationale as SecurityRationale]})` : ph;
    });
    const [whole, counted] = LEAD[verb];
    const text = group.length > named.length ? `${counted(group.length)} ${listPhrases(words)}` : `${whole} ${listPhrases(words)}`;
    out.push({ verb, text, keys: named.map((c) => `${c.tech}|${c.stage}`), total: group.length });
  }
  return out;
}

/** One economy's strategy sentence ("Japan should preserve ...; secure ...; and accelerate deployment of ..."). */
export function strategySentence(name: string, cells: readonly PostureCell[], meta: Pick<PostureMeta, 'phrases'>): string {
  if (!cells.length) return `There is not enough comparable evidence to produce a strategy map for ${name}.`;
  const assessed = cells.filter((c) => c.posture !== 'N');
  if (!assessed.length) return `${name} has no assessed supply-chain step yet: need or prize is not measured in ${cells.length === 1 ? 'its one step' : `any of its ${plural(cells.length, 'step')}`}.`;
  const clauses = strategyClauses(cells, meta).map((c) => c.text);
  if (!clauses.length) {
    return `${name} has no supply-chain step that calls for securing, preserving, catching up, relying on partners or faster deployment: domestic production is a low priority in all ${assessed.length} assessed steps, so the strategy is to keep them open.`;
  }
  if (clauses.length === 1) return `${name} should ${clauses[0]}.`;
  if (clauses.length === 2) return `${name} should ${clauses[0]} and ${clauses[1]}.`;
  return `${name} should ${clauses.slice(0, -1).join('; ')}; and ${clauses[clauses.length - 1]}.`;
}
