// The case-review building blocks on their own (components/case/): a missing value reads "not available in this
// release"; a fact shows its source and vintage (or says none is stated); country evidence groups by topic and every
// other product keeps one list; the stage strip marks the case's stage, the Explorer's stages and its first stage in
// words, not by colour alone; more than three assumptions fold; the alternatives panel asks "Why X rather than Y?"
// and "What would make Y more appropriate?", and says so when none is computed.

import { afterEach, describe, expect, it } from 'vitest';
import { cleanup, render, screen, within } from '@testing-library/react';
import { createMemoryRouter, RouterProvider } from 'react-router';
import type { BriefFact } from '../../model.ts';
import { AlternativesPanel } from './AlternativesPanel.tsx';
import { evidenceGroups } from './CaseBrief.tsx';
import { AssumptionsPanel, briefFactValue, FactRow, NOT_IN_RELEASE, SHOWN_ASSUMPTIONS, sourceLine } from './EvidencePanel.tsx';
import { StageStrip } from './StageStrip.tsx';

afterEach(cleanup);

const f = (id: string, over: Partial<BriefFact> = {}): BriefFact => ({ id, label: id, value: 1, display: '1', interpretation: null, source: null, vintage: null, ...over });
const inRouter = (el: React.ReactNode) => render(<RouterProvider router={createMemoryRouter([{ path: '/', element: el }])} />);

describe('evidence', () => {
  it('a missing value says so; a blank display falls back to the value', () => {
    expect(briefFactValue(f('a', { value: null, display: '' }))).toBe(NOT_IN_RELEASE);
    expect(briefFactValue(f('a', { value: null, display: '—' }))).toBe(NOT_IN_RELEASE);
    expect(briefFactValue(f('a', { value: Number.NaN, display: '' }))).toBe(NOT_IN_RELEASE);
    expect(briefFactValue(f('a', { value: 0.5, display: '' }))).toBe('0.5');
    expect(briefFactValue(f('a', { value: null, display: 'Not measured for the United States' }))).toBe('Not measured for the United States');
    expect(sourceLine(f('a', { source: 'UN Comtrade', vintage: '2024' }))).toBe('UN Comtrade · 2024');
    expect(sourceLine(f('a'))).toBeNull();
  });

  it('a fact row shows its reading, source and estimate label, or that no source is stated', () => {
    render(
      <dl>
        <FactRow f={f('x', { label: 'Coverage', display: '42%', interpretation: 'a model estimate', source: 'UN Comtrade', vintage: '2025–26', estimate: true })} />
        <FactRow f={f('y', { label: 'Other' })} />
      </dl>,
    );
    const x = document.querySelector('[data-fact="x"]') as HTMLElement;
    expect(x.textContent).toContain('Estimate');
    expect(x.textContent).toContain('Reading: a model estimate');
    expect(within(x).getByTestId('fact-source').textContent).toBe('Source: UN Comtrade · 2025–26');
    expect(within(document.querySelector('[data-fact="y"]') as HTMLElement).getByTestId('fact-source').textContent).toBe('Source: not stated in this release');
  });

  it('groups country evidence by topic; other products keep one list', () => {
    const ev = [f('demand_outlook'), f('largest_supplier'), f('own_world_share'), f('something_new')];
    const g = evidenceGroups({ product: 'country', evidence: ev });
    expect(g.map((x) => [x.title, x.facts.map((y) => y.id)])).toEqual([
      ['Where supply comes from', ['largest_supplier']],
      ['Domestic capability', ['own_world_share']],
      ['Market outlook', ['demand_outlook']],
      ['Other evidence', ['something_new']],
    ]);
    expect(evidenceGroups({ product: 'sector', evidence: ev })).toEqual([{ title: null, facts: ev }]);
  });

  it('folds the assumptions after the first three', () => {
    const as = Array.from({ length: 5 }, (_, i) => ({ id: `a${i}`, label: `Assumption ${i}`, text: 't', method_card_id: i === 0 ? 'MC_001' : null }));
    inRouter(<AssumptionsPanel assumptions={as} campaign="c1" />);
    const panel = screen.getByTestId('brief-assumptions');
    expect(panel.querySelectorAll('[data-assumption]').length).toBe(5);
    const more = within(panel).getByTestId('more-assumptions') as HTMLDetailsElement;
    expect(more.open).toBe(false);
    expect(more.querySelectorAll('[data-assumption]').length).toBe(5 - SHOWN_ASSUMPTIONS);
    expect(within(panel).getByRole('link', { name: 'Read the method card' }).getAttribute('href')).toBe('/review/methods?campaign=c1&card=MC_001');
  });
});

describe('the stage strip', () => {
  it('marks the case stage, the Explorer stages and the first in words', () => {
    render(<StageStrip location={{ stages: [], highlight: ['Components'] }} label="Where this case sits" marked={['Refining', 'Components']} lead="Components" leadWord="acts here first" />);
    const strip = screen.getByTestId('stage-strip');
    expect(strip.querySelectorAll('li').length).toBe(6);
    const comp = strip.querySelector('[data-stage="Components"]') as HTMLElement;
    expect(comp.textContent).toContain('Component Manufacturing');
    expect(comp.textContent).toMatch(/this case · needs action · acts here first/);
    expect(strip.querySelector('[data-stage="Refining"]')?.textContent).toContain('needs action');
    expect(strip.querySelector('[data-stage="Extraction"]')?.hasAttribute('data-marked')).toBe(false);
  });
});

describe('the alternatives', () => {
  it('asks why this rather than the alternative and what would make the alternative right', () => {
    render(<AlternativesPanel conclusion="Partner" alternatives={[{ value: 'secure', label: 'Secure', why_not: 'Trusted supply can replace most of it.', would_need: 'Insufficient trusted alternatives.', margin: 'coverage 0.8 vs 0.9' }]} />);
    expect(screen.getByRole('heading', { name: 'Why Partner rather than Secure?' })).toBeTruthy();
    expect(screen.getByRole('heading', { name: 'What would make Secure more appropriate?' })).toBeTruthy();
    expect(screen.getByText('Decisive margin: coverage 0.8 vs 0.9')).toBeTruthy();
    cleanup();
    render(<AlternativesPanel conclusion="Partner" alternatives={[]} />);
    expect(screen.getByText(/No neighbouring alternative is computed/)).toBeTruthy();
  });
});
