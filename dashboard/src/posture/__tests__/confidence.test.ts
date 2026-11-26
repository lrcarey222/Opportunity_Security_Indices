import { describe, expect, it } from 'vitest';
import { confidenceSentence, confidenceSummary } from '../confidence.ts';

const base = { posture: 'C' as const, needType: 'supply_chain' as const, confidence: 'moderate' as const, evidence: 'strong' as const, evidenceRaw: 'high', borderline: null, flag: null, cp: 0.8, pathway: 'yes' as const, alignmentConfidence: 'high' as const };

describe('confidenceSummary', () => {
  it('reads the shipped confidence and names the borderline inputs in words', () => {
    const s = confidenceSummary({ ...base, borderline: 'supply_chain_need;cp_contender_pct' });
    expect(s.level).toBe('Moderate');
    expect(confidenceSentence(s)).toBe('Confidence: Moderate — supply-chain need and competitive position close to the decision threshold; capability evidence is stage-specific.');
  });
  it('never infers a level the engine did not write, and states missing evidence as missing', () => {
    const s = confidenceSummary({ ...base, confidence: null, evidence: null, evidenceRaw: 'none', cp: null, pathway: 'unknown', alignmentConfidence: 'low' });
    expect(s.level).toBeNull();
    expect(confidenceSentence(s)).toBe('Confidence: not stated — competitive position not measured; domestic pathway unknown; no stage-specific capability evidence; policy evidence is incomplete.');
  });
  it('at Deployment & Use only the inputs of that choice are named (no capability inputs)', () => {
    const s = confidenceSummary({ ...base, posture: 'D', needType: 'deployment', confidence: 'high', cp: null, evidence: null, evidenceRaw: null, borderline: 'deployment_need' });
    expect(confidenceSentence(s)).toBe('Confidence: High — deployment need close to the decision threshold.');
  });
  it('a step that was not assessed is insufficient evidence, never low priority', () => {
    const s = confidenceSummary({ ...base, posture: 'N', confidence: 'low' });
    expect(s.insufficient).toBe(true);
    expect(s.level).toBeNull();
    expect(confidenceSentence(s)).toBe('Insufficient evidence: no conclusion is drawn for this step.');
  });
});
