// The run's settings in words: the demand scope (the importers whose lost supply is restored; "coalition"
// is the allied core and partner tiers of the coalition table, not a compact's coalition) and the
// candidate-partner universe (the exporters that may supply; "global": any economy), separate settings
// since 2026-10-16; the shock semantics (hold / release); the supply constraint (production-constrained:
// physical evidence decides who can supply / trade-only); and the strategic scope (any configured name).
// An older bundle carries only meta.universe, one set for both roles; the words then say that. Pure.

import type { AibMeta } from '../contract.ts';
import { listWords } from './format.ts';

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
    short: 'release: decoupling',
    words: "decoupling: the shocked economy's purchases fall by the same fraction as its exports, and that share of other exporters' sales to it is freed for other buyers",
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

export const STRATEGIC_WORDS: Record<string, string> = { electro_industrial: 'Electro-industrial', all: 'All technologies' };

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

/** The strategic scope with its technologies; null in an older bundle. */
export function strategicScope(m: AibMeta): (SettingWords & { techs: string[] }) | null {
  if (!m.strategicScope) return null;
  const techs = m.scopeTechs ?? [];
  const label = STRATEGIC_WORDS[m.strategicScope] ?? m.strategicScope;
  const words =
    m.strategicScope === 'all'
      ? 'compacts can be formed from the nodes of every technology'
      : `compacts are formed only from the nodes of ${techs.length ? listWords(techs) : 'the scope technologies'}; other nodes stay in the data, outside the headline`;
  return { mode: m.strategicScope, label, words, techs };
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
