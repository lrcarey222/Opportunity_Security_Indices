// The extras of a case review rendered alone: ThreatStep (review/views/ThreatStep.tsx) shows the before ->
// intervention -> after table with every null and the time to capacity as "not modelled", says when a case has no
// threat figures, and its "I don't know" skips the questions; DecisionUseStep (DecisionUseStep.tsx) offers the
// four scales and optional texts and its "I don't know" records DONT_KNOW. No bare acronyms.

import { afterEach, describe, expect, it, vi } from 'vitest';
import { cleanup, fireEvent, render, screen, within } from '@testing-library/react';
import { bareAcronyms } from '../../components/testing/acronyms.ts';
import { DONT_KNOW_LABEL } from '../flow.ts';
import { DONT_KNOW, type ThreatMetrics } from '../model.ts';
import { DecisionUseStep } from './DecisionUseStep.tsx';
import { NOT_MODELLED, ThreatStep, threatFigures, threatRows } from './ThreatStep.tsx';
import { CASES } from './__fixtures__/reviewFixture.ts';

afterEach(cleanup);

const threat = CASES.find((c) => c.case_id === 'CASE_005')?.snapshot.threat as ThreatMetrics;
const dk = () => screen.getByRole('checkbox', { name: new RegExp(DONT_KNOW_LABEL.replace(/[/]/g, '.')) });

describe('ThreatStep', () => {
  it('shows before -> intervention -> after, nulls and time to capacity as not modelled', () => {
    const { container } = render(<ThreatStep threat={threat} value={null} onChange={() => undefined} />);
    const table = screen.getByTestId('threat-table');
    expect(table.textContent).toContain('Before: the threat');
    expect(table.textContent).toContain('The intervention');
    const rows = threatRows(threat);
    expect(rows.find((r) => r[0].startsWith('Supplier concentration'))).toEqual(['Supplier concentration (Herfindahl-Hirschman index, 0-1)', '0.410', NOT_MODELLED]);
    expect(rows.find((r) => r[0] === 'Largest supplier')).toEqual(['Largest supplier', 'CHN', NOT_MODELLED]);
    const figures = Object.fromEntries(threatFigures(threat));
    expect(figures['Time to build the new capacity']).toMatch(/^not modelled/);
    expect(figures['Share met by simulated new capacity']).toBe(NOT_MODELLED);
    expect(figures['Supply lost to the shock']).toBe('US$2.5 billion');
    expect(figures['What happens to dependence']).toBe(NOT_MODELLED);
    expect(within(table).getAllByText(NOT_MODELLED).length).toBeGreaterThan(3);
    expect(bareAcronyms(container)).toEqual([]);
  });

  it('says when a case has no figures, and "I don\'t know" skips the questions', () => {
    const onChange = vi.fn();
    render(<ThreatStep threat={null} value={null} onChange={onChange} />);
    expect(screen.getByText(/not available in this release/)).toBeTruthy();
    expect(screen.getByRole('group', { name: 'Would this intervention reduce the threat?' })).toBeTruthy();
    fireEvent.click(dk());
    expect(onChange).toHaveBeenCalledWith(expect.objectContaining({ reduces_threat: null, missing_constraints: [], success_looks_like: DONT_KNOW }));
  });
});

describe('DecisionUseStep', () => {
  it('offers the four scales and optional texts; "I don\'t know" records DONT_KNOW and hides them', () => {
    const onChange = vi.fn();
    const { container, rerender } = render(<DecisionUseStep value={null} onChange={onChange} />);
    expect(screen.getAllByRole('group')).toHaveLength(4);
    fireEvent.click(screen.getAllByLabelText(/4 · Fully/)[1]);
    expect(onChange).toHaveBeenLastCalledWith({ identifies_problem: 4 });
    fireEvent.click(dk());
    expect(onChange).toHaveBeenLastCalledWith(expect.objectContaining({ would_do_differently: DONT_KNOW, actionable: null }));
    rerender(
      <DecisionUseStep
        value={{ would_do_differently: DONT_KNOW, actionable: null, identifies_problem: null, separates_diagnosis_from_prescription: null, right_geography: null, evidence_required: null, improvement: null, decision_use: null }}
        onChange={onChange}
      />,
    );
    expect(screen.queryAllByRole('group')).toHaveLength(0);
    expect(bareAcronyms(container)).toEqual([]);
  });
});
