// The evidence lines under a country's recommendations (Explorer usability pass, 2026-11-25): how sure the
// listed steps are, what the conclusion rests on, and what the page says about deployment. Counts only, read
// from the fields the posture engine ships through posture/confidence.ts (never a new score); a step that was
// not assessed is counted as not assessed, never as Watch, low confidence or zero.

import { DEPLOYMENT_STAGE } from '../lib/stages.ts';
import { confidenceSummary } from './confidence.ts';
import type { DeployCaseId, PostureCell, PostureMeta } from './contract.ts';
import { listAnd, plural } from './derive.ts';
import { ALIGNMENT_LABEL, DEPLOY_CASE_LABEL, deployCaseParts, verbOf } from './grammar.ts';

type Cell = Pick<
  PostureCell,
  'tech' | 'stage' | 'needType' | 'posture' | 'mode' | 'deployMode' | 'deployCase' | 'confidence' | 'evidence' | 'evidenceRaw' | 'borderline' | 'flag' | 'cp' | 'pathway' | 'alignment' | 'alignmentConfidence'
>;

export interface QualityCounts {
  steps: number;
  high: number;
  moderate: number;
  low: number;
  /** assessed steps where the engine wrote no confidence (never inferred) */
  unstated: number;
  /** what lowers confidence, by how many steps it touches, most first */
  limits: { label: string; n: number }[];
}

const NEAR = ' close to the decision threshold';
/** confidence.ts reasons as short limits; the one positive reason is not a limit */
const LIMIT_WORD: Record<string, string | null> = {
  'competitive position not measured': 'competitive position not measured',
  'domestic pathway unknown': 'domestic pathway unknown',
  'capability evidence is thin (largely proxies)': 'capability evidence thin',
  'no stage-specific capability evidence': 'no stage-specific capability evidence',
  'capability evidence is stage-specific': null,
  'flagged for review': 'flagged for review',
  'policy evidence is incomplete': 'policy evidence incomplete',
};
const limitOf = (reason: string): string | null => (reason.endsWith(NEAR) ? 'an input near a decision threshold' : reason in LIMIT_WORD ? LIMIT_WORD[reason]! : reason);

/** Confidence levels and limits over the assessed steps given (a not-assessed step is skipped, not counted as low). */
export function qualityCounts(cells: readonly Cell[]): QualityCounts {
  const q: QualityCounts = { steps: 0, high: 0, moderate: 0, low: 0, unstated: 0, limits: [] };
  const by = new Map<string, number>();
  for (const c of cells) {
    const s = confidenceSummary(c);
    if (s.insufficient) continue;
    q.steps += 1;
    if (s.level === 'High') q.high += 1;
    else if (s.level === 'Moderate') q.moderate += 1;
    else if (s.level === 'Low') q.low += 1;
    else q.unstated += 1;
    for (const l of new Set(s.reasons.map(limitOf))) if (l) by.set(l, (by.get(l) ?? 0) + 1);
  }
  q.limits = [...by.entries()].map(([label, n]) => ({ label, n })).sort((a, b) => b.n - a.n || a.label.localeCompare(b.label));
  return q;
}

/** "9 high and 3 moderate confidence" (levels with no step are left out). */
export function levelWords(q: Pick<QualityCounts, 'high' | 'moderate' | 'low' | 'unstated'>): string {
  const parts = [q.high ? `${q.high} high` : '', q.moderate ? `${q.moderate} moderate` : '', q.low ? `${q.low} low` : ''].filter(Boolean);
  const head = parts.length ? `${listAnd(parts)} confidence` : '';
  const none = q.unstated ? `${q.unstated} with no confidence stated` : '';
  return [head, none].filter(Boolean).join(', ');
}

/**
 * "Evidence quality: of 12 listed steps, 9 high and 3 moderate confidence; policy evidence incomplete for 4,
 * an input near a decision threshold for 3." Null when no step is listed.
 */
export function evidenceQualityLine(listed: readonly Cell[], maxLimits = 3): string | null {
  const q = qualityCounts(listed);
  if (!q.steps) return null;
  const lim = q.limits.slice(0, maxLimits).map((l) => `${l.label} for ${l.n}`);
  return `Evidence quality: of ${plural(q.steps, 'listed step')}, ${levelWords(q)}${lim.length ? `; ${lim.join(', ')}` : ''}.`;
}

/** The decision rule in one line, from the core's prize measure. */
export function decisionRuleLine(meta: Pick<PostureMeta, 'prize_measure'>): string {
  const prize = meta.prize_measure === 'market' ? 'the market prize' : 'strategic value (the prize)';
  return `Each choice follows fixed thresholds on three inputs: need (supply-chain vulnerability, or deployment need at Deployment & Use), ${prize} and competitive position. Current policy is not an input; it is compared afterwards.`;
}

/** The 2-4 inputs behind the block: coverage, the decision rule, what the list leaves out, and how many steps sit near a threshold. */
export function conclusionInputs(cells: readonly Cell[], meta: Pick<PostureMeta, 'prize_measure'>, o: { held: number; top: number }): string[] {
  const total = cells.length;
  const na = cells.filter((c) => c.posture === 'N').length;
  const assessed = total - na;
  if (!assessed) return [`None of ${plural(total, 'supply-chain step')} could be assessed: insufficient evidence.`];
  const out = [
    na
      ? `${assessed} of ${plural(total, 'supply-chain step')} assessed; ${na} not assessed (insufficient evidence), never counted as Watch.`
      : `All ${plural(total, 'supply-chain step')} assessed.`,
    decisionRuleLine(meta),
    `Up to ${o.top} steps are listed per choice, the strongest case first${o.held ? `; ${plural(o.held, 'step')} flagged for review or low confidence ${o.held === 1 ? 'is' : 'are'} counted, not listed` : ''}.`,
  ];
  const near = cells.filter((c) => c.posture !== 'N' && (c.borderline ?? '').trim() !== '').length;
  if (near) out.push(`${near} of ${assessed} assessed steps ${near === 1 ? 'has' : 'have'} an input near a decision threshold, so a small change in the data could change ${near === 1 ? 'it' : 'them'}.`);
  return out;
}

export interface DeploymentSummary {
  /** Deployment & Use steps assessed / not assessed */
  assessed: number;
  notAssessed: number;
  /** technologies with Accelerate, in the order given */
  accelerate: string[];
  /** the deployment cases of the Accelerate steps, by count */
  cases: { id: DeployCaseId; n: number }[];
  /** Accelerate steps where deployment policy lags (alignment deployment_gap) */
  gap: number;
  /** Accelerate steps with not enough policy evidence (alignment insufficient_data) */
  policyThin: number;
}

/** What the core says about deployment, Deployment & Use steps only. */
export function deploymentSummary(cells: readonly Cell[]): DeploymentSummary {
  const dep = cells.filter((c) => c.stage === DEPLOYMENT_STAGE);
  const acc = dep.filter((c) => verbOf(c) === 'accelerate');
  const by = new Map<DeployCaseId, number>();
  for (const c of acc) for (const k of deployCaseParts(c.deployCase)) by.set(k, (by.get(k) ?? 0) + 1);
  return {
    assessed: dep.filter((c) => c.posture !== 'N').length,
    notAssessed: dep.filter((c) => c.posture === 'N').length,
    accelerate: acc.map((c) => c.tech),
    cases: [...by.entries()].map(([id, n]) => ({ id, n })).sort((a, b) => b.n - a.n),
    gap: acc.filter((c) => c.alignment === 'deployment_gap').length,
    policyThin: acc.filter((c) => c.alignment === 'insufficient_data').length,
  };
}

/**
 * "Faster deployment (Accelerate) for Solar and Wind (2 of 9 assessed technologies); why: climate (2), energy
 * security (1); deployment policy lags for 1 of them. 2 not assessed." Only what the cells say.
 */
export function deploymentLine(d: DeploymentSummary): string {
  const na = d.notAssessed ? ` ${d.notAssessed} not assessed (insufficient evidence).` : '';
  if (!d.assessed) return `Deployment need not assessed for any technology (insufficient evidence).`;
  if (!d.accelerate.length) return `No technology needs faster deployment (Accelerate) among ${plural(d.assessed, 'assessed technology', 'assessed technologies')}.${na}`;
  const why = d.cases.length ? `; why: ${d.cases.map((c) => `${DEPLOY_CASE_LABEL[c.id]} (${c.n})`).join(', ')}` : '';
  const policy = [
    d.gap ? `${ALIGNMENT_LABEL.deployment_gap.replace(/^.*?:\s*/, '')} for ${d.gap} of them` : '',
    d.policyThin ? `not enough policy evidence for ${d.policyThin}` : '',
  ].filter(Boolean);
  return `Faster deployment (Accelerate) for ${listAnd(d.accelerate)} (${d.accelerate.length} of ${plural(d.assessed, 'assessed technology', 'assessed technologies')})${why}${policy.length ? `; ${policy.join(', ')}` : ''}.${na}`;
}
