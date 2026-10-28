// TableView's sort key (2026-10-28): a column with sortValue sorts by it, not by its value as text, so a
// Stage column sorts in chain order (Extraction ... Recovery) instead of alphabetically.

import { afterEach, describe, expect, it } from 'vitest';
import { cleanup, fireEvent, render, screen } from '@testing-library/react';
import { TableView, type Col } from './TableView.tsx';
import { stageRank } from '../lib/stages.ts';

afterEach(cleanup);

interface R { id: string; stage: string }
const ROWS: R[] = [
  { id: 'a', stage: 'Recovery' },
  { id: 'b', stage: 'Assembly' },
  { id: 'c', stage: 'Extraction' },
  { id: 'd', stage: 'Refining' },
  { id: 'e', stage: 'Components' },
];

const bodyStages = () => screen.getAllByRole('row').slice(1).map((r) => r.querySelectorAll('td')[1]?.textContent);

describe('TableView sortValue', () => {
  it('sorts a Stage column in chain order, both ways; without sortValue it sorts alphabetically', () => {
    const cols: Col<R>[] = [
      { id: 'id', header: 'Id', value: (r) => r.id },
      { id: 'stage', header: 'Stage', value: (r) => r.stage, sortValue: (r) => stageRank(r.stage) },
    ];
    render(<TableView rows={ROWS} cols={cols} caption="stages" initialSort={{ id: 'stage' }} rowKey={(r) => r.id} />);
    expect(bodyStages()).toEqual(['Extraction', 'Refining', 'Components', 'Assembly', 'Recovery']);
    fireEvent.click(screen.getByRole('button', { name: /Stage/ }));
    expect(bodyStages()).toEqual(['Recovery', 'Assembly', 'Components', 'Refining', 'Extraction']);
    cleanup();
    const plain: Col<R>[] = [cols[0], { id: 'stage', header: 'Stage', value: (r) => r.stage }];
    render(<TableView rows={ROWS} cols={plain} caption="stages" initialSort={{ id: 'stage' }} rowKey={(r) => r.id} />);
    expect(bodyStages()).toEqual(['Assembly', 'Components', 'Extraction', 'Recovery', 'Refining']);
  });
});
