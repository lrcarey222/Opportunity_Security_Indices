// The Sector strategy chart on its own (design section 5): one mark per economy, its shape by verb; the
// Preserve bar and the need line at the producing stages, the deployment threshold alone at Deployment & Use;
// an economy without a measured Competitive Position in the left strip; a node where no economy has one
// spread out by choice, with no Preserve line and no region labels; a mark opens its economy (click or Enter).

import { afterEach, describe, expect, it, vi } from 'vitest';
import { cleanup, fireEvent, render, screen } from '@testing-library/react';
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
  it('a producing node: a mark per assessed economy, shaped by verb; the Preserve bar and the need line; a mark opens its economy', () => {
    const { chart, onOpen } = draw('Batteries', 'Components');
    expect(chart.getAttribute('data-cp')).toBe('all');
    const marks = [...chart.querySelectorAll('[data-verb]')];
    expect(marks.map((m) => [m.getAttribute('data-iso3'), m.getAttribute('data-verb')]).sort()).toEqual([['AAA', 'catch_up'], ['BBB', 'catch_up'], ['CCC', 'partner']]);
    // Catch Up is a triangle (a path), Partner a square
    expect(chart.querySelector('[data-iso3="CCC"] rect')).toBeTruthy();
    expect(chart.querySelector('[data-iso3="AAA"] path')).toBeTruthy();
    expect(chart.querySelector('[data-testid="line-cp"]')).toBeTruthy();
    expect(chart.querySelector('[data-testid="line-need"]')).toBeTruthy();
    expect(screen.getByRole('button', { name: 'Cland: Compete · Partner, supply-chain need 85%, Competitive Position 20%' })).toBeTruthy();
    fireEvent.click(chart.querySelector('[data-iso3="CCC"]') as Element);
    fireEvent.keyDown(chart.querySelector('[data-iso3="BBB"]') as Element, { key: 'Enter' });
    expect(onOpen.mock.calls).toEqual([['CCC'], ['BBB']]);
    // hover or focus: the detail in words
    fireEvent.focus(chart.querySelector('[data-iso3="CCC"]') as Element);
    expect(screen.getByRole('status').textContent).toContain('No credible allied solution yet');
  });

  it('Deployment & Use: Accelerate (an up arrow) and Watch (a ring) on deployment need, no Preserve bar', () => {
    const { chart } = draw('Batteries', 'Deployment');
    expect(chart.getAttribute('data-stage')).toBe('Deployment');
    expect(new Set([...chart.querySelectorAll('[data-verb]')].map((m) => m.getAttribute('data-verb')))).toEqual(new Set(['accelerate', 'watch']));
    expect(chart.querySelector('[data-testid="line-cp"]')).toBeNull();
    expect([...chart.querySelectorAll('[data-testid="region-label"]')].map((t) => t.textContent)).toEqual(['ACCELERATE', 'WATCH']);
    expect(screen.getByRole('group').getAttribute('aria-label')).toBe('Deployment need against Competitive Position for 3 economies');
  });

  it('a missing position goes to the left strip; with none measured the economies are spread out, without the Preserve bar or region labels', () => {
    const partial = draw('Batteries', 'Components', (c) => (c.iso3 === 'CCC' ? { ...c, cp: null } : c)).chart;
    expect(partial.getAttribute('data-cp')).toBe('partial');
    expect(partial.querySelector('[data-testid="cp-missing-strip"]')).toBeTruthy();
    cleanup();
    const none = draw('Batteries', 'Components', (c) => ({ ...c, cp: null })).chart;
    expect(none.getAttribute('data-cp')).toBe('none');
    expect(none.querySelector('[data-testid="cp-missing-strip"]')).toBeNull();
    expect(none.querySelector('[data-testid="line-cp"]')).toBeNull();
    expect(none.querySelectorAll('[data-testid="region-label"]')).toHaveLength(0);
    expect(none.textContent).toContain('Competitive Position not measured at this stage: economies spread out by choice');
    expect(none.querySelectorAll('[data-verb]')).toHaveLength(3);
  });

  it('the legend names every verb the stage can take, in the site grammar', () => {
    render(<VerbMarkLegend verbs={['secure', 'preserve', 'catch_up', 'partner', 'watch']} notPlotted={2} />);
    const legend = screen.getByTestId('sector-strategy-legend');
    expect([...legend.querySelectorAll('[data-verb]')].map((e) => e.textContent)).toEqual(['Secure', 'Compete · Preserve', 'Compete · Catch Up', 'Compete · Partner', 'Deploy · Watch']);
    expect(legend.textContent).toContain('2 not assessed or without a measured need');
  });
});
