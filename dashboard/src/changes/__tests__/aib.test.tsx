import { cleanup, render, screen } from '@testing-library/react';
import { MemoryRouter } from 'react-router';
import { afterEach, describe, expect, it } from 'vitest';
import { checkAibChanges, latestAibChange, pairThesisChange } from '../aib.ts';
import { HomePartnershipsView } from '../HomePartnerships.tsx';

afterEach(cleanup);

const change = (a: string, b: string, direction: 'more' | 'less', delta: number, explanation: string) => ({
  a, b, kind: 'compacts_changed', direction, delta, n_compacts_prev: 1, n_compacts_cur: 1 + delta, compacts_added: '', compacts_removed: '', attribution: 'real_world', explanation,
});
const doc = () =>
  checkAibChanges({
    schema: 'osi-aib-changes', compact_names: {},
    vintages: [{ vintage_id: 'v1', created: '2026-10-30T00:00:00Z' }, { vintage_id: 'v2', created: '2026-11-17T00:00:00Z' }, { vintage_id: 'v3', created: '2026-11-21T00:00:00Z' }],
    pairs: [
      { prev: 'v1', cur: 'v2', method_changed: false, attribution: 'real_world', n_more: 2, n_less: 1, n_new: 0, n_dropped: 0, n_membership: 0, detail: true,
        changes: [change('JPN', 'USA', 'more', 1, 'Shared priority compacts 1 → 2 (added Grid).'), change('AUS', 'KOR', 'less', -1, 'Shared priority compacts 2 → 1.')], membership: null },
      { prev: 'v2', cur: 'v3', method_changed: false, attribution: 'real_world', n_more: 0, n_less: 0, n_new: 0, n_dropped: 0, n_membership: 0, detail: true, changes: [], membership: [] },
    ],
  });

describe('partnership changes', () => {
  it('finds the latest pair of vintages that changed a thesis, and one pair in either order', () => {
    const c = doc();
    expect(latestAibChange(c)?.cur).toBe('v2');
    expect(pairThesisChange(c, 'USA', 'JPN')?.change.delta).toBe(1);
    expect(pairThesisChange(c, 'USA', 'FRA')).toBeNull();
  });
  it('Home lists the pairs becoming more complementary with the attribution', () => {
    render(
      <MemoryRouter>
        <HomePartnershipsView c={doc()} />
      </MemoryRouter>,
    );
    const p = screen.getByTestId('home-partnerships');
    expect(p.textContent).toContain('2 pairs of economies gained shared priority compacts and 1');
    expect(p.textContent).toContain('New data (same method)');
    expect(p.querySelectorAll('li').length).toBe(1);
    expect(p.querySelector('a')?.getAttribute('href')).toBe('/aib/partnerships?country=JPN&partner=USA');
  });
});
