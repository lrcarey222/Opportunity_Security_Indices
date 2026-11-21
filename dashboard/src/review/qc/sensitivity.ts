// Per-cell robustness of the shipped conclusions under alternative specifications (docs/QC_SYSTEM_DESIGN.md 4.6),
// the shipped-bundle layer. Posture cells re-run the canonical choice rule (qc/choiceRule.ts) with one change
// at a time; AIB nodes read the class variants the bundle already ships (node.variants, node.growth).
//
// Posture specs (only those the shipped trace supports):
//   local       each decision threshold +/- the borderline band (need 0.67, prize 0.6, cp 0.75, deployment 0.6,
//               band 0.05), one at a time; strict instead of inclusive comparison at the cp bar.
//   structural  large-yard min_signals 4 and 6 (from the trace's signal count), position_first off,
//               deployment threshold 0.67 (the TS fallback DEFAULT_THRESHOLDS.deployment of posture/contract.ts,
//               which differs from the 0.6 every other source uses).
// Not runnable on the bundle (pipeline layer, R): market-only gate 0.33 / Unknown 0.5, need floor 0.05,
// deployment needing >= 2 families, need-floor-cap-aware need 0.60, prize weights.
//
// Classes: fragile = a local spec changes the choice; sensitive = no local spec does, a structural one does;
// stable = none does; not_testable = the inputs the specs move do not decide the cell (not assessed: need,
// prize or deployment need is missing; Secure: the posture engine's security override comes before every
// threshold the bundle carries and is not re-runnable from it) or the trace does not reproduce the shipped
// choice. fence_change is reported separately. A spec whose result depends on an input the bundle lacks (or on a
// value shipped exactly on a moved threshold) is listed as indeterminate, never counted as a flip, but the cell is then never
// classed stable (a possible local flip makes it fragile, a possible structural one sensitive). Checked against the
// pipeline layer (R, qc_sensitivity_pipeline.csv): every pipeline-fragile cell is bundle-fragile. Pure: no IO.

import type { AibNode } from '../../aib/contract.ts';
import { FENCE_OF_VERB, type Fence } from '../../posture/grammar.ts';
import type { PostureCore } from '../../posture/contract.ts';
import type { NodeDetail } from '../../posture/detail.ts';
import type { AibScenarioKey, CaseKey, RobustnessClass, RobustnessRecord, SpecResult } from '../model.ts';
import { cellKey } from './bundles.ts';
import {
  cappedNodes, choiceRule, extractChoiceInputs, roundTh, shippedChoice, thresholdsFromCore,
  type Choice, type ChoiceInputs, type ChoiceThresholds,
} from './choiceRule.ts';

export interface PostureSpec {
  spec_id: string;
  kind: 'local' | 'structural';
  label: string;
  /** the input or setting the spec moves (a driver id) */
  driver: string;
  apply: (th: ChoiceThresholds) => ChoiceThresholds;
}

const BAND = 0.05;
const local = (id: string, label: string, driver: string, apply: (th: ChoiceThresholds) => ChoiceThresholds): PostureSpec => ({ spec_id: id, kind: 'local', label, driver, apply });
const structural = (id: string, label: string, driver: string, apply: (th: ChoiceThresholds) => ChoiceThresholds): PostureSpec => ({ spec_id: id, kind: 'structural', label, driver, apply });
const sign = (d: number) => (d > 0 ? `+${d}` : `${d}`);

/** The posture specs, in order (labels say the value used). */
export function postureSpecs(base: ChoiceThresholds, fallbackDeployment = 0.67): PostureSpec[] {
  const out: PostureSpec[] = [];
  for (const d of [-BAND, BAND]) {
    out.push(local(`need_${d < 0 ? 'minus' : 'plus'}`, `Need threshold ${roundTh(base.need + d)} (${sign(d)})`, 'supply_chain_need', (th) => ({ ...th, need: roundTh(th.need + d) })));
  }
  for (const d of [-BAND, BAND]) {
    out.push(local(`prize_${d < 0 ? 'minus' : 'plus'}`, `Prize threshold ${roundTh(base.prize + d)} (${sign(d)})`, 'strategic_industrial_value', (th) => ({ ...th, prize: roundTh(th.prize + d) })));
  }
  for (const d of [-BAND, BAND]) {
    out.push(local(`cp_${d < 0 ? 'minus' : 'plus'}`, `Competitive Position bar ${roundTh(base.cp + d)} (${sign(d)})`, 'cp_contender_pct', (th) => ({ ...th, cp: roundTh(th.cp + d) })));
  }
  out.push(local('cp_strict', `Competitive Position strictly above ${base.cp} (not at or above)`, 'cp_inclusive_rule', (th) => ({ ...th, cpStrict: true })));
  for (const d of [-BAND, BAND]) {
    out.push(local(`deployment_${d < 0 ? 'minus' : 'plus'}`, `Deployment threshold ${roundTh(base.deployment + d)} (${sign(d)})`, 'deployment_need', (th) => ({ ...th, deployment: roundTh(th.deployment + d) })));
  }
  out.push(structural('large_min4', 'Large-yard case on 4 of 8 commodity signals (not 5)', 'large_yard_min_signals', (th) => ({ ...th, largeMin: 4 })));
  out.push(structural('large_min6', 'Large-yard case on 6 of 8 commodity signals (not 5)', 'large_yard_min_signals', (th) => ({ ...th, largeMin: 6 })));
  out.push(structural('position_first_off', 'Large-yard case before Competitive Position (position_first off)', 'position_first', (th) => ({ ...th, positionFirst: false })));
  if (Math.abs(fallbackDeployment - base.deployment) > 1e-9) {
    out.push(structural('deployment_fallback', `Deployment threshold ${fallbackDeployment} (the value of the TS fallback DEFAULT_THRESHOLDS.deployment, also in the R sensitivity grid)`, 'deployment_threshold_fallback', (th) => ({ ...th, deployment: fallbackDeployment })));
  }
  return out;
}

/** The specs the pipeline layer runs but the bundle cannot (said in every summary). */
export const SPECS_NOT_RUNNABLE_ON_BUNDLE: readonly string[] = [
  'market-only gate 0.33 / Unknown counted 0.5 (needs node attributes)',
  'need floor 0.05 (needs supply_chain_need_score and the uncapped percentile)',
  'deployment needing >= 2 families (the rule for a one-family cell is not defined in the bundle)',
  'need-floor-cap-aware need 0.60 (the bundle does not mark capped need)',
  'prize family weights (needs the family scores of every cell)',
];

export const fenceOf = (c: Choice | null): Fence | null => (c === null || c === 'not_assessed' ? null : FENCE_OF_VERB[c]);

export interface PostureRobustness extends RobustnessRecord {
  /** specs whose result the bundle cannot decide (possible flips), with what is missing */
  indeterminate: { spec_id: string; kind: 'local' | 'structural'; missing: string[] }[];
}

const NOT_ASSESSED_NOTE =
  'not_testable: not assessed (need, prize or deployment need is missing), so no threshold the bundle carries can change the call; the inputs the triage needs are not in the source.';
const SECURE_NOTE =
  "not_testable: Secure is the posture engine's security override (critical consequence, coercion, delivery tests), decided before every threshold the bundle carries and not re-runnable from it.";

/** One cell's robustness record. baseline: the shipped choice. */
export function cellRobustness(inp: ChoiceInputs, key: CaseKey, baseline: Choice | null, base: ChoiceThresholds, specs: PostureSpec[]): PostureRobustness {
  const rec = (o: Partial<PostureRobustness>): PostureRobustness => ({
    layer: 'bundle', case_key: key, baseline_choice: baseline ?? 'unknown', specs_run: 0, local_flips: [], structural_flips: [], fence_change: false,
    class: 'not_testable', drivers: [], note: null, indeterminate: [], ...o,
  });
  if (baseline === null) return rec({ note: 'not_testable: no shipped choice' });
  if (baseline === 'not_assessed') return rec({ note: NOT_ASSESSED_NOTE });
  if (baseline === 'secure' || inp.secure) return rec({ note: SECURE_NOTE });
  const b = choiceRule(inp, base, { mode: 'trace_at_base', base });
  if (b.choice !== baseline) {
    return rec({ note: `not_testable: the rule re-run on the trace gives ${b.choice ?? 'an unresolved choice'} where ${baseline} is shipped (see choice_reproduction)` });
  }
  const localFlips: SpecResult[] = [];
  const structFlips: SpecResult[] = [];
  const indeterminate: PostureRobustness['indeterminate'] = [];
  let run = 0;
  const drivers = new Set<string>();
  for (const s of specs) {
    const out = choiceRule(inp, s.apply(base), { mode: 'trace_at_base', base });
    if (out.choice === null) {
      // two or more choices remain possible, so at least one differs from the shipped one: a possible flip
      indeterminate.push({ spec_id: s.spec_id, kind: s.kind, missing: out.unresolved });
      drivers.add(s.driver);
      continue;
    }
    run++;
    if (out.choice === baseline) continue;
    const r: SpecResult = { spec_id: s.spec_id, kind: s.kind, label: s.label, choice: out.choice, changed: true, fence_changed: fenceOf(out.choice) !== fenceOf(baseline) };
    (s.kind === 'local' ? localFlips : structFlips).push(r);
    drivers.add(s.driver);
  }
  // a spec the bundle cannot decide may flip the choice: never read as "stable" (conservative); the note says so
  const possibleLocal = indeterminate.some((x) => x.kind === 'local');
  const possibleStruct = indeterminate.some((x) => x.kind === 'structural');
  const cls: RobustnessClass = localFlips.length || possibleLocal ? 'fragile' : structFlips.length || possibleStruct ? 'sensitive' : 'stable';
  const notes: string[] = [];
  if (indeterminate.length)
    notes.push(`possible flips the bundle cannot decide (counted in the class, not as flips): ${indeterminate.map((x) => `${x.spec_id} (${x.missing.join(', ')})`).join('; ')}`);
  return rec({
    specs_run: run, local_flips: localFlips, structural_flips: structFlips,
    fence_change: [...localFlips, ...structFlips].some((f) => f.fence_changed),
    class: cls, drivers: [...drivers], note: notes.length ? notes.join(' ') : null, indeterminate,
  });
}

export interface PostureRobustnessRun {
  records: PostureRobustness[];
  specs: PostureSpec[];
  thresholds: ChoiceThresholds;
}

/** Robustness of every core cell from its detail cell. */
export function postureRobustness(
  core: Pick<PostureCore, 'meta' | 'cells'>,
  details: Record<string, { cells: NodeDetail[] }>,
  opts: { fallbackDeployment?: number; thresholds?: ChoiceThresholds } = {},
): PostureRobustnessRun {
  const base = opts.thresholds ?? thresholdsFromCore(core);
  const specs = postureSpecs(base, opts.fallbackDeployment ?? 0.67);
  const caps = cappedNodes(core);
  const index = new Map<string, NodeDetail>();
  for (const [iso, d] of Object.entries(details)) for (const c of d.cells) index.set(cellKey(iso, c.tech, c.stage), c);
  const records = core.cells.map((c) => {
    const key: CaseKey = { iso3: c.iso3, tech: c.tech, stage: c.stage };
    const d = index.get(cellKey(c.iso3, c.tech, c.stage));
    const shipped = shippedChoice(c);
    if (!d) {
      return {
        layer: 'bundle', case_key: key, baseline_choice: shipped ?? 'unknown', specs_run: 0, local_flips: [], structural_flips: [], fence_change: false,
        class: 'not_testable', drivers: [], note: 'not_testable: no detail cell', indeterminate: [],
      } satisfies PostureRobustness;
    }
    const inp = extractChoiceInputs(d, c.stage, { capped: caps.get(`${c.tech}|${c.stage}`) ?? null, cpThreshold: base.cp });
    return cellRobustness(inp, key, shipped, base, specs);
  });
  return { records, specs, thresholds: base };
}

// ---- AIB nodes -------------------------------------------------------------------------------------------------

/** Variants read as local perturbations of the headline (the shock halved, and the growth bounds). */
const AIB_LOCAL_VARIANTS = ['china_50'];
/** Variants read as structural alternatives (release semantics, conservative supply, trade-only mode, other shocks). */
const AIB_STRUCTURAL_VARIANTS = ['release', 'conservative', 'trade_only', 'largest_subsector', 'largest_stage', 'largest_technology'];
const AIB_VARIANT_LABEL: Record<string, string> = {
  china_50: 'China supply halved (china_50) instead of removed',
  release: 'Release semantics (China stops buying too) instead of hold',
  conservative: 'Conservative supply (no diversion from outside buyers)',
  trade_only: 'Trade-only mode (physical production data not applied)',
  largest_subsector: 'Largest supplier of the sub-sector removed (a different shock)',
  largest_stage: 'Largest supplier of the stage removed (a different shock)',
  largest_technology: 'Largest supplier of the technology removed (a different shock)',
};
const HEADLINE_GROWTH = '2x';
const INTERVENTION = new Set(['BUILD', 'COORDINATE', 'MANAGE SPECIALIZATION']);
/** The AIB analogue of a fence: intervene / low intervention / no data. */
export const aibFence = (cls: string | null): 'intervene' | 'low' | 'no_data' | null =>
  cls === null ? null : INTERVENTION.has(cls) ? 'intervene' : cls === 'LOW INTERVENTION' ? 'low' : 'no_data';

/** One AIB node's robustness under the scenario (Decoupling only: the variants are of the headline shock). */
export function aibNodeRobustness(node: AibNode, scenario: AibScenarioKey = 'decoupling'): RobustnessRecord {
  const key: CaseKey = { node: node.id, scenario };
  const rec = (o: Partial<RobustnessRecord>): RobustnessRecord => ({
    layer: 'bundle', case_key: key, baseline_choice: node.cls, specs_run: 0, local_flips: [], structural_flips: [], fence_change: false, class: 'not_testable', drivers: [], note: null, ...o,
  });
  if (scenario !== 'decoupling') {
    return rec({
      baseline_choice: scenario === 'managed' ? (node.diversify?.cls ?? 'unknown') : 'none',
      note: `not_testable: the bundle's class variants (node.variants, node.growth) are of the Decoupling headline; none is exported for the ${scenario} scenario.`,
    });
  }
  if (node.cls === 'INSUFFICIENT DATA') return rec({ note: 'not_testable: INSUFFICIENT DATA, the stress test did not classify the node.' });
  const variants = node.variants && !Array.isArray(node.variants) ? node.variants : {};
  const localFlips: SpecResult[] = [];
  const structFlips: SpecResult[] = [];
  const drivers = new Set<string>();
  const notes: string[] = [];
  let run = 0;
  const consider = (id: string, kind: 'local' | 'structural', label: string, cls: string | null | undefined, driver: string) => {
    if (cls === null || cls === undefined) return;
    run++;
    if (cls === node.cls) return;
    const r: SpecResult = { spec_id: id, kind, label, choice: cls, changed: true, fence_changed: aibFence(cls) !== aibFence(node.cls) };
    (kind === 'local' ? localFlips : structFlips).push(r);
    drivers.add(driver);
  };
  for (const v of AIB_LOCAL_VARIANTS) consider(v, 'local', AIB_VARIANT_LABEL[v], variants[v], v);
  for (const g of node.growth ?? []) {
    if (g.label === HEADLINE_GROWTH) {
      if (g.cls !== null && g.cls !== node.cls) notes.push(`the headline growth bound (${HEADLINE_GROWTH}) gives ${g.cls}, not the shipped ${node.cls}`);
      continue;
    }
    consider(`growth_${g.label}`, 'local', `Existing relationships may grow ${g.label} (not ${HEADLINE_GROWTH})`, g.cls, 'existing_relationship_growth');
  }
  for (const v of AIB_STRUCTURAL_VARIANTS) consider(v, 'structural', AIB_VARIANT_LABEL[v], variants[v], v);
  const pc = variants.production_constrained;
  if (pc !== null && pc !== undefined && pc !== node.cls) notes.push(`the production_constrained variant (the headline mode) gives ${pc}, not the shipped ${node.cls}`);
  const missing = [...AIB_LOCAL_VARIANTS, ...AIB_STRUCTURAL_VARIANTS].filter((v) => variants[v] === null || variants[v] === undefined);
  if (missing.length) notes.push(`variants not run: ${missing.join(', ')}`);
  const cls: RobustnessClass = localFlips.length ? 'fragile' : structFlips.length ? 'sensitive' : run ? 'stable' : 'not_testable';
  return rec({
    specs_run: run, local_flips: localFlips, structural_flips: structFlips,
    fence_change: [...localFlips, ...structFlips].some((f) => f.fence_changed), class: cls, drivers: [...drivers], note: notes.length ? notes.join('; ') : null,
  });
}

// ---- summaries -----------------------------------------------------------------------------------------------------

export interface RobustnessSummary {
  records: number;
  by_class: Record<RobustnessClass, number>;
  fence_change: number;
  /** driver -> cells it flips */
  by_driver: Record<string, number>;
  /** spec id -> cells it flips */
  by_spec: Record<string, number>;
  /** spec id -> cells it cannot decide */
  indeterminate_by_spec: Record<string, number>;
  /** not_testable reasons, by their first words */
  not_testable_by_reason: Record<string, number>;
}

export function summariseRobustness(records: readonly (RobustnessRecord & { indeterminate?: { spec_id: string }[] })[]): RobustnessSummary {
  const by_class: Record<RobustnessClass, number> = { stable: 0, sensitive: 0, fragile: 0, not_testable: 0 };
  const by_driver: Record<string, number> = {};
  const by_spec: Record<string, number> = {};
  const ind: Record<string, number> = {};
  const nt: Record<string, number> = {};
  let fence = 0;
  for (const r of records) {
    by_class[r.class]++;
    if (r.fence_change) fence++;
    for (const d of r.drivers) by_driver[d] = (by_driver[d] ?? 0) + 1;
    for (const f of [...r.local_flips, ...r.structural_flips]) by_spec[f.spec_id] = (by_spec[f.spec_id] ?? 0) + 1;
    for (const x of r.indeterminate ?? []) ind[x.spec_id] = (ind[x.spec_id] ?? 0) + 1;
    if (r.class === 'not_testable') {
      const why = (r.note ?? 'unknown').replace(/^not_testable:\s*/, '').split(/[,:(]/)[0].trim();
      nt[why] = (nt[why] ?? 0) + 1;
    }
  }
  return { records: records.length, by_class, fence_change: fence, by_driver, by_spec, indeterminate_by_spec: ind, not_testable_by_reason: nt };
}

/** One-letter class codes for the compact robustness map (public/data/review/robustness.json). */
export const CLASS_CODE: Readonly<Record<RobustnessClass, string>> = { stable: 'S', sensitive: 'N', fragile: 'F', not_testable: 'X' };
