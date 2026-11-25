// Confidence and evidence in words (Explorer usability pass, 2026-11-25). One reading of the fields the posture
// engine already ships, so every page states confidence the same way and never invents a score:
//   * posture confidence (cells.conf: high / moderate / low; docs/strategic_position.md: one point off per borderline
//     input, a still-possible Secure, an unknown pathway deciding the node, an unmeasured Competitive Position where
//     the rule reads it, weak capability evidence, a consistency flag, fewer than two observed need families);
//   * capability evidence (cells.ev, Competitive Position's stage_evidence_quality: strong / moderate / weak / none);
//   * policy-alignment confidence (cells.alc), stated only when it is low.
// The reasons list only what the fields show; when a field is missing the sentence says so instead of guessing.
// Trade momentum is not listed: it is shown beside the choice but does not enter it.

import type { PostureCell } from './contract.ts';

export type ConfidenceLevel = 'High' | 'Moderate' | 'Low';

export interface ConfidenceSummary {
  /** the posture confidence in words; null when the engine wrote none (never inferred) */
  level: ConfidenceLevel | null;
  /** short reasons, from the fields that lower or support it */
  reasons: string[];
  /** true when the step was not assessed: no conclusion, so no confidence line beyond "Insufficient evidence" */
  insufficient: boolean;
}

const LEVEL: Record<NonNullable<PostureCell['confidence']>, ConfidenceLevel> = { high: 'High', moderate: 'Moderate', low: 'Low' };

/** decision_borderline input names (as the engine writes them) in words */
const BORDERLINE_WORD: Record<string, string> = {
  supply_chain_need: 'supply-chain need',
  deployment_need: 'deployment need',
  strategic_industrial_value: 'strategic value',
  cp_contender_pct: 'competitive position',
  large_yard_case: 'the case for keeping it open',
};

/** The words for a cell's confidence, from the shipped fields only. */
export function confidenceSummary(
  c: Pick<PostureCell, 'posture' | 'confidence' | 'evidence' | 'evidenceRaw' | 'borderline' | 'flag' | 'cp' | 'pathway' | 'alignmentConfidence'>,
): ConfidenceSummary {
  if (c.posture === 'N') return { level: null, reasons: ['Insufficient evidence to assess this step'], insufficient: true };
  const reasons: string[] = [];
  const near = (c.borderline ?? '')
    .split(/[;|,]/)
    .map((s) => s.trim())
    .filter(Boolean)
    .map((k) => BORDERLINE_WORD[k] ?? k.replace(/_/g, ' '));
  if (near.length) reasons.push(`${near.join(' and ')} close to the decision threshold`);
  if (c.cp === null) reasons.push('competitive position not measured');
  if (c.pathway === 'unknown') reasons.push('domestic pathway unknown');
  if (c.evidence === 'weak' || c.evidenceRaw === 'none') reasons.push(c.evidenceRaw === 'none' ? 'no stage-specific capability evidence' : 'capability evidence is thin (largely proxies)');
  else if (c.evidence === 'strong') reasons.push('capability evidence is stage-specific');
  if (c.flag) reasons.push('flagged for review');
  if (c.alignmentConfidence === 'low') reasons.push('policy evidence is incomplete');
  return { level: c.confidence ? LEVEL[c.confidence] : null, reasons, insufficient: false };
}

/** "Confidence: Moderate — supply-chain need close to the decision threshold; capability evidence is stage-specific." */
export function confidenceSentence(s: ConfidenceSummary): string {
  if (s.insufficient) return 'Insufficient evidence: no conclusion is drawn for this step.';
  const head = s.level ? `Confidence: ${s.level}` : 'Confidence: not stated';
  return s.reasons.length ? `${head} — ${s.reasons.join('; ')}.` : `${head}.`;
}
