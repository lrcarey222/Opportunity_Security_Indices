// The Sector strategy chart on its own (design section 5; the shared quadrant-scatter look since 2026-10-30):
// one dot per economy in its choice colour; the
// Preserve bar and the need line at the producing stages, the deployment threshold alone at Deployment & Use;
// an economy without a measured Competitive Position in the left strip; a node where no economy has one
// spread out by choice, with no Preserve line and no region labels; a mark opens its economy (click or Enter).

import { afterEach, describe, expect, it, vi } from 'vitest';
import { cleanup, fireEvent, render, screen } from '@testing-library/react';
import type { Verb } from '../../posture/contract.ts';
import { VERB_COLOR, VERB_OUTLINE } from '../../posture/grammar.ts';
import { decodeCore } from '../../posture/contract.ts';
import { countryNames } from '../../posture/derive.ts';
import { fixtureCore } from '../../posture/__tests__/fixture.ts';
import { nodePrize, regionLabels, strategyPoints } from './derive.ts';
import { SectorStrategyScatter, VerbMarkLegend } from './SectorStrategyScatter.tsx';

const core = decodeCore(fixtureCore());
const names = countryNames(core);
const T = core.meta.thresholds;
const node = (tech: string, stage: string) => core.cells.filter((c) => c.tech === tech && c.stage === stage);

afterEach(cleanup);

function draw(tech: string, stage: string, edit = (c: (typeof core.cells)[number]) => c) {
  const cells = node(tech, stage).map(edit);
  const onOpen = vi.fn();
  render(<SectorStrategyScatter points={strategyPoints(cells, names)} stage={stage} thresholds={T} regions={regionLabels(stage, nodePrize(cells, T.prize)?.kind ?? null, T)} onOpen={onOpen} />);
  return { chart: screen.getByTestId('sector-strategy-scatter'), onOpen };
}

describe('the Sector strategy chart', () => {
  it('a producing node: a dot per assessed economy in its choice colour; the dashed Preserve bar and need line; a mark opens its economy', () => {
    const { chart, onOpen } = draw('Batteries', 'Components');
    expect(chart.getAttribute('data-cp')).toBe('all');
    const marks = [...chart.querySelectorAll('[data-verb]')];
    expect(marks.map((m) => [m.getAttribute('data-iso3'), m.getAttribute('data-verb')]).sort()).toEqual([['AAA', 'catch_up'], ['BBB', 'catch_up'], ['CCC', 'partner']]);
    // every mark is one dot of the shared size, filled with its choice colour, outlined; no shape code
    for (const m of marks) {
      const dots = m.querySelectorAll('circle[data-mark="dot"]');
      expect(dots).toHaveLength(1);
      const v = m.getAttribute('data-verb') as Verb;
      expect([dots[0].getAttribute('fill'), dots[0].getAttribute('stroke'), dots[0].getAttribute('r')]).toEqual([VERB_COLOR[v], VERB_OUTLINE[v], '6']);
      expect(m.querySelectorAll('path, rect')).toHaveLength(0);
    }
    // the dashed lines of the shared look: the Preserve bar and the need line, at their thresholds
    const cp = chart.querySelector('[data-testid="line-cp"]') as SVGLineElement;
    const need = chart.querySelector('[data-testid="line-need"]') as SVGLineElement;
    expect([cp.getAttribute('stroke-dasharray'), need.getAttribute('stroke-dasharray')]).toEqual(['4,3', '4,3']);
    expect(Number(cp.getAttribute('x1'))).toBeCloseTo(48 + T.cp * (640 - 48 - 20), 5);
    expect(Number(need.getAttribute('y1'))).toBeCloseTo(420 - 40 - T.need * (420 - 30 - 40), 5);
    expect(chart.querySelector('[data-testid="scatter-grid"]')).toBeTruthy();
    // the need in plain words (vocabulary MEASURE_PLAIN), both axes say which way is stronger or more exposed
    expect(screen.getByRole('button', { name: 'Cland: Partner · Medium fence, supply-chain vulnerability 85%, Competitive Position 20%' })).toBeTruthy();
    expect(chart.textContent).toContain('↑ Supply-chain vulnerability (higher = more vulnerable)');
    expect(chart.textContent).toContain('Competitive Position (higher = stronger, ranked among the main producers) →');
    expect(chart.textContent).not.toMatch(/percentile|contenders/);
    fireEvent.click(chart.querySelector('[data-iso3="CCC"]') as Element);
    fireEvent.keyDown(chart.querySelector('[data-iso3="BBB"]') as Element, { key: 'Enter' });
    expect(onOpen.mock.calls).toEqual([['CCC'], ['BBB']]);
    // hover or focus: the detail in words, and a white ring (signal orange is the Secure fill, so never the ring)
    fireEvent.focus(chart.querySelector('[data-iso3="CCC"]') as Element);
    expect(screen.getByRole('status').textContent).toContain('No credible allied solution yet');
    const ring = chart.querySelector('[data-iso3="CCC"] circle[data-mark="ring"]')?.getAttribute('stroke');
    expect(ring).toBe('#ffffff');
    expect(Object.values(VERB_COLOR)).not.toContain(ring);
  });

  it('the detail names the best-placed trusted partner and, for a mixed step, the yards its layers reach', () => {
    const { chart } = draw('Batteries', 'Components', (c) => (c.iso3 === 'CCC' ? { ...c, partnerGap: false, partner1: 'AAA', yards: 'mixed_high_medium' } : c));
    fireEvent.focus(chart.querySelector('[data-iso3="CCC"]') as Element);
    const tip = screen.getByRole('status');
    expect(tip.textContent).toContain('Best-placed partner: AAA');
    expect(tip.textContent).not.toContain('First trusted partner');
    expect(screen.getByTestId('scatter-tip-mixed').textContent).toBe('Mixed: this step in the Medium yard, its layers also in the Small yard');
    // a step whose layers agree says nothing about yards
    fireEvent.focus(chart.querySelector('[data-iso3="BBB"]') as Element);
    expect(screen.queryByTestId('scatter-tip-mixed')).toBeNull();
  });

  it('Deployment & Use: Accelerate (green) and Watch (ink) dots on deployment need, no Preserve bar', () => {
    const { chart } = draw('Batteries', 'Deployment');
    expect(chart.getAttribute('data-stage')).toBe('Deployment');
    expect(new Set([...chart.querySelectorAll('[data-verb]')].map((m) => m.getAttribute('data-verb')))).toEqual(new Set(['accelerate', 'watch']));
    expect(new Set([...chart.querySelectorAll('circle[data-mark="dot"]')].map((c) => c.getAttribute('fill')))).toEqual(new Set([VERB_COLOR.accelerate, VERB_COLOR.watch]));
    expect(chart.querySelector('[data-testid="line-cp"]')).toBeNull();
    expect([...chart.querySelectorAll('[data-testid="region-label"]')].map((t) => t.textContent)).toEqual(['ACCELERATE', 'WATCH']);
    expect(screen.getByRole('group').getAttribute('aria-label')).toBe('Deployment need against Competitive Position for 3 economies');
  });

  it('a missing position goes to the left strip; with none measured the economies are spread out, without the Preserve bar or region labels', () => {
    const partial = draw('Batteries', 'Components', (c) => (c.iso3 === 'CCC' ? { ...c, cp: null } : c)).chart;
    expect(partial.getAttribute('data-cp')).toBe('partial');
    expect(partial.querySelector('[data-testid="cp-missing-strip"]')?.textContent).toBe('no data');
    cleanup();
    const none = draw('Batteries', 'Components', (c) => ({ ...c, cp: null })).chart;
    expect(none.getAttribute('data-cp')).toBe('none');
    expect(none.querySelector('[data-testid="cp-missing-strip"]')).toBeNull();
    expect(none.querySelector('[data-testid="line-cp"]')).toBeNull();
    expect(none.querySelectorAll('[data-testid="region-label"]')).toHaveLength(0);
    expect(none.textContent).toContain('Competitive Position not measured at this stage: economies spread out by choice');
    expect(none.querySelectorAll('[data-verb]')).toHaveLength(3);
  });

  it('the legend names every verb the stage can take, grouped by fence in its colour', () => {
    render(<VerbMarkLegend verbs={['secure', 'preserve', 'catch_up', 'partner', 'watch']} notPlotted={2} />);
    const legend = screen.getByTestId('sector-strategy-legend');
    expect([...legend.querySelectorAll('[data-verb]')].map((e) => e.textContent)).toEqual(['Secure', 'Preserve,', 'Catch Up,', 'Partner', 'Watch']);
    expect([...legend.querySelectorAll('[data-fence]')].map((e) => e.getAttribute('data-fence'))).toEqual(['high', 'medium', 'low']);
    expect(legend.textContent).toContain('High fence:SecureMedium fence:Preserve,Catch Up,PartnerLow fence:Watch');
    expect(legend.textContent).toContain('Colour is the choice, grouped by fence.');
    cleanup();
    // Deployment & Use: Accelerate (medium fence) and Watch (low fence) only; no high fence
    render(<VerbMarkLegend verbs={['accelerate', 'watch']} />);
    expect([...screen.getByTestId('sector-strategy-legend').querySelectorAll('[data-fence]')].map((e) => e.textContent)).toEqual(['Medium fence:Accelerate', 'Low fence:Watch']);
    expect(legend.textContent).toContain('2 not assessed or without a measured need');
  });
});
