// Accessible form primitives of the review pages (review/components/form.tsx): every control labelled,
// hints and errors wired with aria-describedby / aria-invalid, radio cards with swatches and the "I don't
// know" card, scales, a capped multi-check, the severity select and the don't-know switch.

import { afterEach, describe, expect, it, vi } from 'vitest';
import { cleanup, fireEvent, render, screen } from '@testing-library/react';
import { SEVERITY_LABEL } from '../model.ts';
import { DONT_KNOW_LABEL } from '../flow.ts';
import { ChoiceGrid, ConfidenceInput, DontKnowToggle, Field, Likert4Input, MultiCheck, SeveritySelect, TextArea } from './form.tsx';

afterEach(cleanup);

describe('Field and TextArea', () => {
  it('labels the control and wires the hint and the error', () => {
    const onChange = vi.fn();
    render(<TextArea label="What is wrong?" hint="One sentence" error="Required" value="" onChange={onChange} required />);
    const ta = screen.getByLabelText(/What is wrong\?/) as HTMLTextAreaElement;
    expect(ta.tagName).toBe('TEXTAREA');
    expect(ta.className).toContain('field');
    expect(ta.getAttribute('aria-invalid')).toBe('true');
    const ids = (ta.getAttribute('aria-describedby') ?? '').split(' ');
    expect(ids.map((id) => document.getElementById(id)?.textContent)).toEqual(['One sentence', 'Required']);
    expect(screen.getByRole('alert').textContent).toBe('Required');
    fireEvent.change(ta, { target: { value: 'Re-exports' } });
    expect(onChange).toHaveBeenCalledWith('Re-exports');
  });
  it('Field accepts a plain child', () => {
    render(
      <Field label="Name" id="nm">
        <input id="nm" />
      </Field>,
    );
    expect(screen.getByLabelText('Name').id).toBe('nm');
  });
});

describe('ChoiceGrid', () => {
  const options = [
    { value: 'secure', label: 'Secure', color: '#123456', definition: 'Protect' },
    { value: 'watch', label: 'Watch' },
  ];
  it('renders radio cards in a fieldset with swatches and the don’t-know card', () => {
    const onChange = vi.fn();
    const { container } = render(<ChoiceGrid legend="What should it be?" options={options} value="secure" onChange={onChange} dontKnowValue="dont_know" error="Pick one" />);
    const group = screen.getByRole('group', { name: 'What should it be?' });
    expect(group.getAttribute('aria-invalid')).toBe('true');
    const radios = screen.getAllByRole('radio') as HTMLInputElement[];
    expect(radios.map((r) => r.value)).toEqual(['secure', 'watch', 'dont_know']);
    expect(radios[0].checked).toBe(true);
    expect(screen.getByLabelText(new RegExp(DONT_KNOW_LABEL.replace(/[/]/g, '.')))).toBeTruthy();
    expect(container.querySelector('[style*="background"]')).toBeTruthy();
    fireEvent.click(screen.getByLabelText(/Watch/));
    expect(onChange).toHaveBeenCalledWith('watch');
  });
});

describe('scales', () => {
  it('ConfidenceInput offers 1-5 and Likert4Input 1-4, each a labelled radio', () => {
    const c = vi.fn();
    const l = vi.fn();
    render(
      <>
        <ConfidenceInput value={null} onChange={c} name="conf" />
        <Likert4Input legend="Is it actionable?" value={2} onChange={l} name="act" />
      </>,
    );
    const conf = screen.getByRole('group', { name: 'How confident are you?' });
    expect(conf.querySelectorAll('input[type=radio]')).toHaveLength(5);
    const act = screen.getByRole('group', { name: 'Is it actionable?' });
    expect(act.querySelectorAll('input[type=radio]')).toHaveLength(4);
    expect((act.querySelector('input[value="2"]') as HTMLInputElement).checked).toBe(true);
    fireEvent.click(screen.getByLabelText(/4 · High/));
    expect(c).toHaveBeenCalledWith(4);
    fireEvent.click(screen.getByLabelText(/4 · Fully/));
    expect(l).toHaveBeenCalledWith(4);
  });
});

describe('MultiCheck', () => {
  it('caps the selection at max', () => {
    const onChange = vi.fn();
    const options = ['a', 'b', 'c', 'd'].map((v) => ({ value: v, label: v.toUpperCase() }));
    render(<MultiCheck legend="Driving factors" options={options} value={['a', 'b', 'c']} onChange={onChange} max={3} />);
    const boxes = screen.getAllByRole('checkbox') as HTMLInputElement[];
    expect(boxes.map((b) => b.checked)).toEqual([true, true, true, false]);
    expect(boxes[3].disabled).toBe(true);
    fireEvent.click(boxes[0]);
    expect(onChange).toHaveBeenCalledWith(['b', 'c']);
  });
});

describe('SeveritySelect and DontKnowToggle', () => {
  it('lists the severities with their canonical labels', () => {
    const onChange = vi.fn();
    render(<SeveritySelect value={null} onChange={onChange} />);
    const sel = screen.getByLabelText(/How serious is it\?/) as HTMLSelectElement;
    expect([...sel.options].slice(1).map((o) => o.textContent)).toEqual(Object.values(SEVERITY_LABEL));
    fireEvent.change(sel, { target: { value: 'critical' } });
    expect(onChange).toHaveBeenCalledWith('critical');
  });
  it('toggles don’t-know', () => {
    const onChange = vi.fn();
    render(<DontKnowToggle checked={false} onChange={onChange} />);
    fireEvent.click(screen.getByRole('checkbox', { name: DONT_KNOW_LABEL }));
    expect(onChange).toHaveBeenCalledWith(true);
  });
});
