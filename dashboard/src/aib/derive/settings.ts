// The run's settings in words: the demand scope (the importers whose lost supply is restored; "coalition"
// is the allied core and partner tiers of the coalition table, not a compact's coalition) and the
// candidate-partner universe (the exporters that may supply; "global": any economy), separate settings
// since 2026-10-16; the shock semantics (hold / release: a two-way cut); the supply constraint (production-constrained:
// physical evidence decides who can supply / trade-only); the strategic scope (any configured name); and
// the node taxonomy (2026-10-19: the electro-industrial dictionary, or the OSI basket of an older bundle);
// and the diversification scenario (2026-10-19: concentration (HHI), the benchmark and its fallback, the
// supplier cap on the allied market's demand, in words for About and the Diversification page).
// An older bundle carries only meta.universe, one set for both roles; the words then say that. Pure.

import { diversifyMetaOf, isDictionary, type AibMeta } from '../contract.ts';
import { listWords } from './format.ts';
import { NOT_IN_BASKET, taxonomyOf } from './taxonomy.ts';

// ---- The diversification scenario ----------------------------------------------------------------------

/** The scenario's name when the bundle gives none (meta.diversify.label, else its shock's label, else this). */
export const DIVERSIFY_LABEL = "Diversified to its sector's concentration";

/** What an HHI value means, in a phrase (the Diversification page's rule). */
export const HHI_SHORT = 'close to 0 when many suppliers each hold a little, 1 when a single supplier holds everything';

/** Concentration (HHI) in one sentence. */
export const HHI_WORDS = `Concentration (HHI, the Herfindahl-Hirschman Index) is the sum of the squared shares of a node's world exporters: ${HHI_SHORT} (Hong Kong and Macao are counted with China).`;

/** The benchmark rules (meta.diversify.benchmark) in words. */
export const DIVERSIFY_BENCHMARK_WORDS: Readonly<Record<string, { label: string; words: string }>> = {
  hs4: {
    label: 'Its customs heading',
    words:
      "the trade-weighted mean concentration of the other HS6 lines of the node's HS4 customs heading (electric cars against the petrol, diesel and hybrid cars of heading 8703; lithium-ion cells against the other accumulators of heading 8507), falling back to the other nodes of its electro-industrial dictionary sector (the dictionary's grouping of the node, not one of the Explorer's technologies) when the heading has no other line",
  },
  sector: { label: 'Its dictionary sector', words: "the trade-weighted mean concentration of the other nodes of the node's electro-industrial dictionary sector (the dictionary's grouping of the node, not one of the Explorer's technologies)" },
};

export interface DiversifySetting extends SettingWords {
  /** its id in meta.shocks and node.variants */
  shockId: string;
  /** the benchmark rule as written ("hs4") */
  benchmark: string;
}

/**
 * The diversification scenario of a bundle: its label, its shock id and the benchmark rule in words
 * (mode = the rule); null in an older bundle without meta.diversify.
 */
export function diversifySetting(m: AibMeta): DiversifySetting | null {
  const d = diversifyMetaOf(m);
  if (!d) return null;
  const benchmark = d.benchmark || 'hs4';
  const label = d.label || m.shocks.find((s) => s.id === d.shockId)?.label || DIVERSIFY_LABEL;
  return { mode: benchmark, shockId: d.shockId, benchmark, label, words: DIVERSIFY_BENCHMARK_WORDS[benchmark]?.words ?? benchmark };
}

/**
 * The scenario in plain sentences (About and the Diversification page): concentration, the benchmark and its
 * fallback, the cap on the allied market's demand, allies' own purchases, the two trade states under the
 * cap and the new capacity, the headline settings, and what it is not. `nDemand`: the economies of the
 * demand scope; `year`: the trade year.
 */
export function diversifyRules(m: AibMeta): string[] {
  const s = diversifySetting(m);
  const bench = s ? s.words : DIVERSIFY_BENCHMARK_WORDS.hs4.words;
  const n = typeof m.nDemandScope === 'number' ? ` (the demand scope: ${m.nDemandScope} economies)` : ' (the demand scope)';
  const sem = (m.shockSemantics && SEMANTICS_WORDS[m.shockSemantics]?.label.toLowerCase()) || m.shockSemantics || 'hold';
  const con = (m.supplyConstraint && CONSTRAINT_WORDS[m.supplyConstraint]?.label.toLowerCase()) || m.supplyConstraint?.replace(/_/g, '-') || 'production-constrained';
  const demand = m.demandScope === 'global' ? 'global demand' : 'allied demand';
  const supply = !m.candidateUniverse || m.candidateUniverse === 'global' ? 'any economy may supply' : 'only the candidate partners may supply';
  const tol = diversifyMetaOf(m)?.minExcess;
  const tolerance = typeof tol === 'number' && tol > 0 ? `a small tolerance (${tol} of HHI)` : 'a small tolerance';
  return [
    HHI_WORDS,
    `Each supply-chain node is compared with a benchmark: ${bench}.`,
    `A node more concentrated than its benchmark, by more than ${tolerance}, gets the supplier-share cap that would bring its concentration down to the benchmark. In the allied market${n} no supplier may then ship more than the cap times the allied market's demand: every supplier above it loses the excess. A capped supplier that is itself an allied economy (Germany in electric cars) loses only its excess sales: its own purchases stay protected like every allied buyer's.`,
    'Existing relationships, then optimized trade, replace what they can, with the cap holding for every supplier in both; what neither replaces is the new capacity diversification needs, and the investment simulation ranks the hosts for it. Coverage after trade is never below the coverage before it.',
    `The scenario removes no supplier. It runs under the headline settings: ${sem} semantics, ${con} supply, the ${m.headlineScenario} supply assumption, ${demand}, and ${supply}.`,
    `It is a stress test of how the market could be organized, not a forecast. Dollar values are ${m.tradeYear} trade values.`,
  ];
}

/** The dictionary scopes in words. */
export const DICTIONARY_SCOPE_WORDS: Record<string, { label: string; words: string; role: string }> = {
  core: {
    label: 'Core',
    words: "the dictionary's core codes: those dedicated or predominant in their sector (specificity 1-2), plus a few shared codes it keeps as core with a written justification",
    role: 'the headline',
  },
  extended: { label: 'Extended', words: 'the core codes plus the other shared codes (specificity 3)', role: 'a sensitivity' },
};

/** How the views show a node (2026-10-29, design A1): by the OSI technology x stage cells of its codes. */
export const OSI_PRESENTATION_WORDS =
  `the views show each node by technology and its stage: its technologies and stages are those of the sectors (technology × stage) its codes sit in, as equals (a node can sit in several), and a node none of whose codes is among the traded goods this Explorer covers is shown as ${NOT_IN_BASKET}`;

/**
 * Where the nodes come from, in words: the electro-industrial dictionary (its scope, primary rows; the
 * dictionary's own sectors and indices are provenance, not how the views group nodes) or, for an older
 * bundle, the OSI basket (technology x stage x sub-sector); then how the views show them.
 */
export function nodeTaxonomy(m: AibMeta): SettingWords & { dictionary: boolean } {
  const tx = taxonomyOf(m);
  if (!tx.dictionary) {
    return {
      mode: 'master',
      dictionary: false,
      label: 'Traded goods the Explorer covers',
      words: `nodes are the sub-sectors of the traded goods this Explorer covers: groups of HS6 goods at one of the six stages of a technology’s supply chain; ${OSI_PRESENTATION_WORDS}`,
    };
  }
  const sc = tx.scope ? DICTIONARY_SCOPE_WORDS[tx.scope] : null;
  return {
    mode: 'dictionary',
    dictionary: true,
    label: `Electro-industrial dictionary${tx.scope ? ` (${sc?.label.toLowerCase() ?? tx.scope} scope)` : ''}`,
    words:
      `nodes are groups of HS6 codes from the electro-industrial HS6 dictionary (RMI Electro-Industrial Stack), each at one of the six supply-chain stages: ` +
      `the stage holding most of its trade, a code’s stage set by its item type ` +
      `(raw material: Extraction, or Recovery for recycled feed; processed material: Refining; component: Components; final product or process equipment: Assembly), so no node is at Deployment` +
      `${tx.scope ? `; ${tx.scope} scope: ${sc ? `${sc.words}, ${sc.role}` : tx.scope}` : ''}; primary rows only, so each code sits in one node; ${OSI_PRESENTATION_WORDS}`,
  };
}

/**
 * The largest-supplier shocks whose level the model takes from the node's sector: in a dictionary run that
 * sector is the electro-industrial dictionary's (config/aib.yml level stage / technology), not an OSI
 * technology, so their labels are reworded (the model is not rerun; design A1).
 */
const DICTIONARY_SHOCK_LABELS: Readonly<Record<string, string>> = {
  largest_stage: 'Largest supplier of the node’s dictionary sector at its stage removed',
  largest_technology: 'Largest supplier of the node’s dictionary sector removed',
};

/** A shock's label (meta.shocks), reworded for a dictionary run where the model's level is the dictionary sector; the id when the bundle lacks it. */
export function shockLabelOf(m: Pick<AibMeta, 'shocks' | 'taxonomy'>, id: string | null | undefined): string {
  if (!id) return '';
  if (isDictionary(m) && DICTIONARY_SHOCK_LABELS[id]) return DICTIONARY_SHOCK_LABELS[id];
  return m.shocks.find((s) => s.id === id)?.label ?? id;
}

export interface SettingWords {
  /** the setting as written ("coalition") */
  mode: string;
  /** a short label ("Allied core and partner economies") */
  label: string;
  /** what it means, in one clause */
  words: string;
}

/** words; then the count of economies; then a note after a comma */
const DEMAND_WORDS: Record<string, { label: string; words: string; note?: string }> = {
  coalition: { label: 'Allied core and partner economies', words: 'the allied core and partner economies of the coalition table', note: "not a compact's coalition" },
  configured: { label: 'Configured economies', words: 'a configured list of economies' },
  global: { label: 'Global', words: 'every economy in the trade data' },
};

const PARTNER_WORDS: Record<string, { label: string; words: string }> = {
  coalition: { label: 'Allied core and partner economies', words: 'only the allied core and partner economies of the coalition table may add or redirect supply' },
  configured: { label: 'Configured economies', words: 'only the configured economies may add or redirect supply' },
  global: { label: 'Global', words: 'any economy in the trade data may add or redirect supply' },
};

export const SEMANTICS_WORDS: Record<string, { label: string; short: string; words: string }> = {
  hold: {
    label: 'Hold',
    short: 'hold: the shocked economy keeps buying',
    words:
      "an export (supply) shock: the shocked economy keeps buying, so other exporters' sales to it stay with it in both supply scenarios and nothing is freed for other buyers",
  },
  release: {
    label: 'Release',
    short: 'release: two-way cut',
    words: "a two-way cut: the shocked economy's purchases also fall, by the same fraction as its exports, and that share of other exporters' sales to it is freed for other buyers",
  },
};

export const CONSTRAINT_WORDS: Record<string, { label: string; words: string }> = {
  production_constrained: {
    label: 'Production-constrained',
    words:
      'where credible production or capacity data exist, physical evidence decides who can supply: an exporter the data list as a producer keeps its full trade response; one they do not list (a re-exporter, stock, or a different product under the same HS code, such as semiconductor wafers under HS 3818.00) keeps its current shipments to the demand scope but cannot expand or redirect supply. Production shares are not converted into trade values, and nodes without credible physical data keep their trade-only result',
  },
  trade_only: { label: 'Trade-only', words: 'observed exports alone bound what a supplier can provide' },
};

/** The strategic scope labels. */
export const STRATEGIC_WORDS: Record<string, string> = { electro_industrial: 'Electro-industrial', all: 'All nodes' };

const known = <T>(table: Record<string, T>, mode: string | null | undefined): T | null => (mode && table[mode] ? table[mode] : null);

/**
 * The demand scope ("the allied core and partner economies of the coalition table (57 economies), not a
 * compact's coalition: the importers whose lost supply is restored"); null in an older bundle.
 */
export function demandScope(m: AibMeta): SettingWords | null {
  if (!m.demandScope) return null;
  const w = known(DEMAND_WORDS, m.demandScope);
  const n = typeof m.nDemandScope === 'number' ? ` (${m.nDemandScope} economies)` : '';
  const note = w?.note ? `, ${w.note}` : '';
  return { mode: m.demandScope, label: w?.label ?? m.demandScope, words: `${w?.words ?? m.demandScope}${n}${note}: the importers whose lost supply is restored` };
}

/** The candidate-partner universe; null in an older bundle. */
export function candidateUniverse(m: AibMeta): SettingWords | null {
  if (!m.candidateUniverse) return null;
  const w = known(PARTNER_WORDS, m.candidateUniverse);
  return { mode: m.candidateUniverse, label: w?.label ?? m.candidateUniverse, words: w?.words ?? m.candidateUniverse };
}

/** The headline shock semantics; null in an older bundle. */
export function shockSemantics(m: AibMeta): SettingWords | null {
  if (!m.shockSemantics) return null;
  const w = known(SEMANTICS_WORDS, m.shockSemantics);
  return { mode: m.shockSemantics, label: w?.label ?? m.shockSemantics, words: w?.words ?? m.shockSemantics };
}

/** The headline supply constraint; null in an older bundle. */
export function supplyConstraint(m: AibMeta): SettingWords | null {
  if (!m.supplyConstraint) return null;
  const w = known(CONSTRAINT_WORDS, m.supplyConstraint);
  return { mode: m.supplyConstraint, label: w?.label ?? m.supplyConstraint, words: w?.words ?? m.supplyConstraint };
}

/**
 * The strategic scope: which nodes compacts can be formed from (meta.scopeTechs: the model's scope list,
 * dictionary sectors in a dictionary run, OSI technologies in a master one; relabelled in words, design
 * A1); null in an older bundle.
 */
export function strategicScope(m: AibMeta): (SettingWords & { techs: string[] }) | null {
  if (!m.strategicScope) return null;
  const tx = taxonomyOf(m);
  const techs = m.scopeTechs ?? [];
  const label = STRATEGIC_WORDS[m.strategicScope] ?? m.strategicScope;
  const words =
    m.strategicScope === 'all'
      ? tx.dictionary
        ? 'compacts can be formed from every node of the electro-industrial dictionary'
        : 'compacts can be formed from every node'
      : `compacts are formed only from the nodes of ${techs.length ? `the ${tx.dictionary ? 'dictionary sectors' : 'technologies'} ${listWords(techs)}` : 'the scope'}; other nodes stay in the data, outside the headline`;
  return { mode: m.strategicScope, label, words, techs };
}

/** The strategic scope in a few words for a variant line ("all nodes", "electro-industrial nodes"). */
export function strategicScopeShort(_m: AibMeta, mode: string | null | undefined = _m.strategicScope): string {
  if (!mode) return 'nodes';
  return mode === 'all' ? 'all nodes' : `${mode.replace(/_/g, '-')} nodes`;
}

/** The constraint label of a mode ("Trade-only"), the mode itself when unknown. */
export const constraintLabel = (mode: string): string => CONSTRAINT_WORDS[mode]?.label ?? mode;

/** The semantics label of a mode ("Release"), the mode itself when unknown. */
export const semanticsLabel = (mode: string): string => SEMANTICS_WORDS[mode]?.label ?? mode;

/**
 * One line on what the coverage shares are shares of: the demand scope's baseline traded demand (an older
 * bundle: the universe's).
 */
export function coverageBase(m: AibMeta): string {
  return m.demandScope ? "the demand scope's baseline traded demand" : "the universe's baseline traded demand";
}
