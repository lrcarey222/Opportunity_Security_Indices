// The evidence lines under the Country block (Explorer usability pass, 2026-11-25): counts from the shipped
// posture fields through confidence.ts, a not-assessed step never counted as low confidence or Watch, the
// deployment row only from Deployment & Use cells, and the compact disclosure component.

import { afterEach, describe, expect, it } from 'vitest';
import { cleanup, render, screen, within } from '@testing-library/react';
import { AVOID_RE } from '../../content/vocabulary.ts';
import type { PostureCell } from '../contract.ts';
import { EvidenceSummary } from '../EvidenceSummary.tsx';
import { conclusionInputs, decisionRuleLine, deploymentLine, deploymentSummary, evidenceQualityLine, qualityCounts } from '../evidenceSummary.ts';

afterEach(cleanup);

type Cell = Parameters<typeof qualityCounts>[0][number];
const cell = (o: Partial<PostureCell>): Cell => ({
  tech: 'Solar', stage: 'Components', posture: 'C', mode: 'preserve', deployMode: null, deployCase: null, confidence: 'high', evidence: 'strong', evidenceRaw: 'high',
  borderline: null, flag: null, cp: 0.8, pathway: 'yes', alignment: 'aligned', alignmentConfidence: 'high', ...o,
  needType: o.needType ?? (o.stage === 'Deployment' ? 'deployment' : 'supply_chain'),
});

describe('evidence quality over the listed steps', () => {
  it('counts the levels and the limits from confidence.ts, most common first', () => {
    const listed = [
      cell({}),
      cell({ confidence: 'moderate', borderline: 'supply_chain_need', alignmentConfidence: 'low' }),
      cell({ confidence: 'moderate', alignmentConfidence: 'low' }),
      cell({ confidence: 'high', evidence: 'weak', evidenceRaw: 'low' }),
    ];
    const q = qualityCounts(listed);
    expect([q.steps, q.high, q.moderate, q.low, q.unstated]).toEqual([4, 2, 2, 0, 0]);
    expect(evidenceQualityLine(listed)).toBe(
      'Evidence quality: of 4 listed steps, 2 high and 2 moderate confidence; policy evidence incomplete for 2, an input near a decision threshold for 1, capability evidence thin for 1.',
    );
  });
  it('a not-assessed step is skipped, never counted as low; a missing confidence is stated as missing', () => {
    const q = qualityCounts([cell({ posture: 'N', confidence: 'low' }), cell({ confidence: null })]);
    expect([q.steps, q.low, q.unstated]).toEqual([1, 0, 1]);
    expect(evidenceQualityLine([cell({ confidence: null })])).toBe('Evidence quality: of 1 listed step, 1 with no confidence stated.');
    expect(evidenceQualityLine([cell({ posture: 'N' })])).toBeNull();
    expect(evidenceQualityLine([])).toBeNull();
  });
});

describe('why this conclusion', () => {
  it('coverage, the rule, what the list leaves out and the near-threshold count', () => {
    const cells = [cell({}), cell({ borderline: 'cp_contender_pct' }), cell({ posture: 'N', mode: null }), cell({ posture: 'D', mode: null, deployMode: 'watch', stage: 'Deployment' })];
    const lines = conclusionInputs(cells, { prize_measure: 'strategic_value' }, { held: 2, top: 3 });
    expect(lines).toEqual([
      '3 of 4 supply-chain steps assessed; 1 not assessed (insufficient evidence), never counted as Watch.',
      decisionRuleLine({ prize_measure: 'strategic_value' }),
      'Up to 3 steps are listed per choice, the strongest case first; 2 steps flagged for review or low confidence are counted, not listed.',
      '1 of 3 assessed steps has an input near a decision threshold, so a small change in the data could change it.',
    ]);
    expect(decisionRuleLine({ prize_measure: 'market' })).toContain('the market prize');
    for (const l of lines) expect(l).not.toMatch(AVOID_RE);
  });
  it('an economy with nothing assessed says so, and nothing else', () => {
    expect(conclusionInputs([cell({ posture: 'N' })], { prize_measure: 'strategic_value' }, { held: 0, top: 3 })).toEqual([
      'None of 1 supply-chain step could be assessed: insufficient evidence.',
    ]);
  });
});

describe('the deployment row', () => {
  const dep = (o: Partial<PostureCell>) => cell({ stage: 'Deployment', posture: 'D', mode: null, deployMode: 'accelerate', ...o });
  it('Accelerate technologies, their cases and the deployment policy gap, from Deployment & Use cells only', () => {
    const d = deploymentSummary([
      dep({ tech: 'Solar', deployCase: 'climate+security', alignment: 'deployment_gap' }),
      dep({ tech: 'Wind', deployCase: 'climate', alignment: 'insufficient_data' }),
      dep({ tech: 'Gas', deployMode: 'watch' }),
      dep({ tech: 'Nuclear', posture: 'N', deployMode: null }),
      cell({ tech: 'Batteries', posture: 'D', mode: null, deployMode: 'accelerate' }), // not a Deployment & Use cell
    ]);
    expect(d).toEqual({ assessed: 3, notAssessed: 1, accelerate: ['Solar', 'Wind'], cases: [{ id: 'climate', n: 2 }, { id: 'security', n: 1 }], gap: 1, policyThin: 1 });
    expect(deploymentLine(d)).toBe(
      'Faster deployment (Accelerate) for Solar and Wind (2 of 3 assessed technologies); why: climate (2), energy security (1); deployment policy lags for 1 of them, not enough policy evidence for 1. 1 not assessed (insufficient evidence).',
    );
  });
  it('no Accelerate, and nothing assessed, in words (never zero need)', () => {
    expect(deploymentLine(deploymentSummary([dep({ deployMode: 'watch' })]))).toBe('No technology needs faster deployment (Accelerate) among 1 assessed technology.');
    expect(deploymentLine(deploymentSummary([dep({ posture: 'N', deployMode: null })]))).toBe('Deployment need not assessed for any technology (insufficient evidence).');
  });
});

describe('<EvidenceSummary>', () => {
  it('the quality line visible, the inputs and data collapsed, the evidence links', () => {
    render(
      <EvidenceSummary
        quality="Evidence quality: of 2 listed steps, 2 high confidence."
        inputs={['Input one.', 'Input two.']}
        data={['Strategy built 21 November 2026', 'trade benchmark 2022-2024']}
        links={[{ label: 'Explore the evidence', href: '#evidence' }]}
      />,
    );
    const box = screen.getByTestId('evidence-summary');
    expect(within(box).getByTestId('evidence-quality').textContent).toBe('Evidence quality: of 2 listed steps, 2 high confidence.');
    const why = within(box).getByTestId('why-conclusion');
    expect(why.tagName).toBe('DETAILS');
    expect(why.hasAttribute('open')).toBe(false);
    expect(within(why).getByText('Why this conclusion?').tagName).toBe('SUMMARY');
    expect(within(why).getAllByRole('listitem').map((li) => li.textContent)).toEqual(['Input one.', 'Input two.']);
    expect(within(why).getByTestId('evidence-data').textContent).toBe('Data: Strategy built 21 November 2026 · trade benchmark 2022-2024.');
    expect(within(box).getByRole('link', { name: /Explore the evidence/ }).getAttribute('href')).toBe('#evidence');
  });
  it('no quality line when nothing is listed', () => {
    render(<EvidenceSummary quality={null} inputs={['x']} data={[]} links={[]} />);
    expect(screen.queryByTestId('evidence-quality')).toBeNull();
    expect(screen.queryByTestId('evidence-data')).toBeNull();
  });
});
