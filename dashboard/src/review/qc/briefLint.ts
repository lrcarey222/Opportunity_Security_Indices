// The leak lint of the case brief (UX v2; NEIS brief section 21): before the blind answer a reviewer sees neutral
// facts, the objective, the timeframe, the scenario and definitions, never the recommendation, a score, a
// persuasive interpretation, the expected answer or a model label that gives the outcome away. lintBriefNeutrality
// scans every text of a brief (identity, question, context, objective, horizon, scenario, facts, assumptions,
// warnings, missing evidence, definitions, partner sides, causal chain, sector-chain stages) and returns one error
// per leak:
//   - the case's own conclusion, by key or label;
//   - any canonical conclusion label of any kind (the six choices, "Watch (keep open)" and "keep open", the AIB
//     classes and their data codes, the compact decisions and types, the partnership boxes, "no stage needs
//     allied intervention") and the answer options' labels; the six stage names are exempt for a sector chain,
//     where every stage is named;
//   - model score field ids (cp_contender_pct, supply_chain_need ...) as fact ids or in text, and the score names
//     (Strategic Need, Strategic Industrial Value, Competitive Position, Deployment Need);
//   - outcome-giving model labels (the physical-capability warning, "coercive", "opposite bloc", "large yard",
//     fences, postures, "materiality floor");
//   - claims about the Explorer's answer ("the Explorer recommends / concludes / finds ...");
//   - the words content/vocabulary.ts AVOID_RE bans.
// The review data build fails on any error (review/qc/reviewData.ts), and review/manifest.ts validateCasesFile runs
// it on every built case. Pure: no IO.

import { AVOID_RE } from '../../content/vocabulary.ts';
import { AIB_CLASSES } from '../../aib/contract.ts';
import { STAGE_DISPLAY, SUPPLY_STAGES } from '../../lib/stages.ts';
import { REVIEWER_STATES, type AnswerOption, type CaseBrief, type CaseConclusion } from '../model.ts';
import { allConclusionLabels, NO_STAGE } from './answerOptions.ts';

/** Model score and decision-input field ids that must never name a brief fact or appear in its text. */
export const MODEL_SCORE_IDS: readonly string[] = [
  'need', 'prize', 'market', 'cp', 'import_dependence', 'import_hhi', 'china_bloc', 'trusted_alternatives', 'pathway', 'deployment_need', 'large_yard_signals',
  'cp_contender_pct', 'supply_chain_need', 'strategic_industrial_value', 'industrial_opportunity', 'large_yard_case', 'pathway_credible', 'secure_override',
  'existing', 'optimized', 'dividend', 'residual', 'residual_usd', 'after_hhi', 'after_largest', 'after_largest_share', 'n1_reoptimized', 'diversion', 'hosts',
  'coverage_coordinated', 'coverage_post_investment', 'class_counts', 'build_hosts', 'core', 'purpose', 'non_allied_relied_on', 'critical_external', 'physical_warning',
  'choice_possible', 'node_classes', 'explorer_stages', 'lead_stage', 'chokepoint', 'class', 'decision', 'missions', 'choice', 'fence', 'posture', 'reason',
  'confidence', 'borderline', 'partner', 'policy_alignment', 'choices_summary', 'headline',
];
/** The snake_case ids that are unmistakable in running text. */
const TEXT_IDS = MODEL_SCORE_IDS.filter((id) => id.includes('_'));
const SCORE_NAMES = ['Strategic Need', 'Strategic Industrial Value', 'Competitive Position', 'Deployment Need', 'Industrial Opportunity'];
const MODEL_LABELS = ['PHYSICAL CAPABILITY NOT VALIDATED', 'coercive', 'opposite bloc', 'opposite-bloc', 'large yard', 'large-yard', 'fence', 'fences', 'posture', 'postures', 'materiality floor', 'keep open'];
const CLAIM_RE = /\b(the explorer|the model|the analysis|neis)\s+(recommends?|concludes?|finds|found|rates?|rated|classif(?:ies|ied)|chooses|chose|calls|judges|says the answer)\b|\bwe recommend\b|\bthe (right|expected|correct) answer\b/i;

const esc = (s: string) => s.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
/** A whole-word (letters on both sides excluded) match of a phrase. */
const phraseRe = (p: string, flags = 'i') => new RegExp(`(^|[^A-Za-z])${esc(p)}(?=[^A-Za-z]|$)`, flags);

/** Every text of a brief with where it sits. */
export function briefTexts(b: CaseBrief): [string, string][] {
  const out: [string, string][] = [];
  const add = (where: string, t: string | null | undefined) => {
    if (typeof t === 'string' && t) out.push([where, t]);
  };
  add('identity.title', b.identity.title);
  b.identity.parts.forEach((p, i) => add(`identity.parts[${i}]`, p.label));
  add('decision_question', b.decision_question);
  add('decision_context', b.decision_context);
  add('objective', b.objective);
  add('time_horizon.label', b.time_horizon.label);
  add('time_horizon.note', b.time_horizon.note);
  if (b.scenario) {
    add('scenario.label', b.scenario.label);
    add('scenario.description', b.scenario.description);
  }
  const facts = (where: string, fs: CaseBrief['evidence']) =>
    fs.forEach((f) => {
      add(`${where}.${f.id}.label`, f.label);
      add(`${where}.${f.id}.display`, f.display);
      add(`${where}.${f.id}.interpretation`, f.interpretation);
      add(`${where}.${f.id}.source`, f.source);
      (f.warnings ?? []).forEach((w, i) => {
        add(`${where}.${f.id}.warnings[${i}].title`, w.title);
        add(`${where}.${f.id}.warnings[${i}].text`, w.text);
      });
    });
  facts('evidence', b.evidence);
  b.assumptions.forEach((a) => {
    add(`assumptions.${a.id}.label`, a.label);
    add(`assumptions.${a.id}.text`, a.text);
  });
  b.warnings.forEach((w, i) => {
    add(`warnings[${i}].title`, w.title);
    add(`warnings[${i}].text`, w.text);
  });
  b.missing_evidence.forEach((m, i) => add(`missing_evidence[${i}]`, m));
  b.definitions.forEach((d, i) => {
    add(`definitions[${i}].term`, d.term);
    add(`definitions[${i}].text`, d.text);
  });
  if (b.partnership) {
    for (const s of ['a', 'b'] as const) {
      add(`partnership.${s}.name`, b.partnership[s].name);
      b.partnership[s].brings.forEach((t, i) => add(`partnership.${s}.brings[${i}]`, t));
      b.partnership[s].exposed_on.forEach((t, i) => add(`partnership.${s}.exposed_on[${i}]`, t));
    }
    b.partnership.shared_problem.forEach((t, i) => add(`partnership.shared_problem[${i}]`, t));
  }
  (b.compact_chain ?? []).forEach((c) => {
    add(`compact_chain.${c.id}.label`, c.label);
    add(`compact_chain.${c.id}.display`, c.display);
    add(`compact_chain.${c.id}.text`, c.text);
  });
  (b.sector_chain ?? []).forEach((s) => {
    add(`sector_chain.${s.stage}.label`, s.label);
    facts(`sector_chain.${s.stage}`, s.facts);
  });
  return out;
}

/**
 * The leaks of a brief (empty when it is neutral). `conclusion` is the case's conclusion (null when unknown, e.g.
 * linting authored text); `answerOptions` the case's options (their labels are banned too, reviewer states aside).
 */
export function lintBriefNeutrality(brief: CaseBrief, conclusion: Pick<CaseConclusion, 'value' | 'label'> | null, answerOptions: readonly AnswerOption[] = []): string[] {
  const errors: string[] = [];
  const stageNames = new Set<string>([...SUPPLY_STAGES, ...SUPPLY_STAGES.map((s) => STAGE_DISPLAY[s])]);
  const sectorChain = answerOptions.some((o) => o.value === NO_STAGE) || brief.sector_chain != null;
  // labels: case-insensitive whole phrase
  const labels = new Set<string>();
  for (const l of allConclusionLabels()) labels.add(l);
  for (const o of answerOptions) if (!REVIEWER_STATES.includes(o.value)) labels.add(o.label);
  if (conclusion && conclusion.label) labels.add(conclusion.label);
  for (const s of stageNames) if (sectorChain || !conclusion || !stageNames.has(conclusion.label)) labels.delete(s);
  // keys: case-sensitive (data codes like BUILD, snake_case keys like catch_up)
  const keys = new Set<string>([...AIB_CLASSES]);
  for (const o of answerOptions) if (!REVIEWER_STATES.includes(o.value) && (/_/.test(o.value) || /^[A-Z][A-Z ]+$/.test(o.value))) keys.add(o.value);
  if (conclusion && (/_/.test(conclusion.value) || /^[A-Z][A-Z ]+$/.test(conclusion.value)) && !(sectorChain && stageNames.has(conclusion.value))) keys.add(conclusion.value);
  const labelRes = [...labels].filter(Boolean).map((l) => [l, phraseRe(l)] as const);
  const keyRes = [...keys].map((k) => [k, phraseRe(k, '')] as const);
  const idRes = TEXT_IDS.map((k) => [k, phraseRe(k, '')] as const);
  const scoreRes = SCORE_NAMES.map((k) => [k, phraseRe(k, '')] as const);
  const modelRes = MODEL_LABELS.map((k) => [k, phraseRe(k, k === k.toUpperCase() ? '' : 'i')] as const);
  for (const [where, text] of briefTexts(brief)) {
    for (const [l, re] of labelRes) if (re.test(text)) errors.push(`${where}: names the conclusion label "${l}" ("${text.slice(0, 80)}")`);
    for (const [k, re] of keyRes) if (re.test(text)) errors.push(`${where}: names the conclusion key "${k}"`);
    for (const [k, re] of idRes) if (re.test(text)) errors.push(`${where}: names the model field "${k}"`);
    for (const [k, re] of scoreRes) if (re.test(text)) errors.push(`${where}: names the model score "${k}"`);
    for (const [k, re] of modelRes) if (re.test(text)) errors.push(`${where}: uses the model label "${k}"`);
    if (CLAIM_RE.test(text)) errors.push(`${where}: states what the Explorer concludes ("${text.slice(0, 80)}")`);
    if (AVOID_RE.test(text)) errors.push(`${where}: uses a banned word (content/vocabulary.ts AVOID_RE)`);
  }
  const ids = [...brief.evidence.map((f) => f.id), ...(brief.sector_chain ?? []).flatMap((s) => s.facts.map((f) => f.id))];
  for (const id of ids) if (MODEL_SCORE_IDS.includes(id)) errors.push(`evidence.${id}: a model score or decision field shown before the blind answer`);
  return errors;
}

/** Authored brief text (review_cases.yml brief:) checked against every canonical label, before any build. */
export function lintAuthoredText(texts: readonly [string, string][]): string[] {
  const brief: CaseBrief = {
    product: 'country', identity: { title: '', parts: [] }, decision_question: '', decision_context: '', objective: '', time_horizon: { label: '' }, scenario: null,
    stage_location: null, evidence: [], assumptions: [], warnings: texts.map(([id, t]) => ({ id, level: 'info', title: '', text: t })), missing_evidence: [], definitions: [],
  };
  return lintBriefNeutrality(brief, null, []).map((e) => e.replace(/^warnings\[(\d+)\]\.text/, (_m, i: string) => texts[Number(i)]?.[0] ?? 'text'));
}
