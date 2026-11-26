import { cleanup, render, screen } from '@testing-library/react';
import { MemoryRouter } from 'react-router';
import { afterEach, describe, expect, it } from 'vitest';
import { ChangedCellsNoteView, nodeLabel } from '../ChangedCellsNote.tsx';
import { checkChangesBundle, type ChangeRecord } from '../contract.ts';
import { HomeChangesView } from '../HomeChanges.tsx';
import { cellKeyOf, cellsOf, summarizeChanges } from '../summary.ts';

afterEach(cleanup);

const side = (choice: string) => ({ choice, posture: null, needType: 'supply_chain', need: 0.8, prize: 0.6, cp: 0.6, market: 0.4, share: 0.03, confidence: 'high', evidence: 'medium' });
const rec = (iso3: string, tech: string, stage: string, cur: string, types: string[], from: string, to: string, attribution = 'unattributed') => ({
  id: `aib:posture:${iso3}:${tech.toLowerCase().replace(/ /g, '-')}:${stage.toLowerCase()}`, iso3, tech, stage, prev: cur === 'v3' ? 'v2' : 'v1', cur,
  types, subtype: null, severity: 'major', magnitude: 0.2, attribution, methodChanged: true, methodUnverified: false, crossed: [], evidenceDirection: null,
  confidenceDirection: null, sources: [], impacts: [], changedInputs: [], before: side(from), after: side(to), delta: { need: 0, prize: 0, cp: 0.2 }, explanation: 'x',
});
const vint = (id: string, created: string) => ({ id, created, gitCommit: 'a', provisional: false, stampSource: 'bundle', coreFile: 'x', coreCommit: 'a', method: { id: 'm', label: 't', unverified: false }, data: { id: 'd' }, nCells: 1 });
const pair = (prev: string, cur: string) => ({ prev, cur, comparable: true, reason: null, methodChanged: true, methodUnverified: false, dataChanged: false, bridge: false, detail: true, nCells: 1, nUnchanged: 0, counts: { meaningful: 1 } });
const bundle = (changes: unknown[]) =>
  checkChangesBundle({
    schema: 'osi-changes', schema_version: 1, created: 'x', git_commit: 'a', impacts: [], sources: {},
    vintages: [vint('v1', '2026-11-01T00:00:00Z'), vint('v2', '2026-11-10T00:00:00Z'), vint('v3', '2026-11-21T00:00:00Z')],
    pairs: [pair('v1', 'v2'), pair('v2', 'v3')],
    changes,
  });

const B = bundle([
  rec('USA', 'Nuclear', 'Assembly', 'v2', ['recommendation'], 'catch_up', 'partner'),
  rec('USA', 'Nuclear', 'Assembly', 'v3', ['recommendation', 'score'], 'partner', 'preserve'),
  rec('JPN', 'Nuclear', 'Assembly', 'v3', ['score'], 'partner', 'partner'),
  rec('USA', 'Solar', 'Components', 'v3', ['recommendation'], 'watch', 'accelerate', 'real_world'),
]);

describe('change summary', () => {
  it('keeps each cell’s latest change, counts other moves and reads attribution', () => {
    const s = summarizeChanges(B, (r) => r.tech === 'Nuclear' && r.stage === 'Assembly');
    expect(s.recommendations.map((r) => `${r.iso3}:${r.after.choice}`)).toEqual(['USA:preserve']);
    expect(s.otherMoves).toBe(1);
    expect(s.attributable).toBe(false);
    expect(summarizeChanges(B, () => true).attributable).toBe(true);
    expect(s.window).toEqual({ from: '2026-11-01', to: '2026-11-21' });
  });
  it('builds a compact’s or partnership’s cell set', () => {
    const k = cellsOf(['USA', 'JPN'], [{ tech: 'Nuclear', stage: 'Assembly' }]);
    expect(k.has(cellKeyOf({ iso3: 'JPN', tech: 'Nuclear', stage: 'Assembly' }))).toBe(true);
    expect(k.size).toBe(2);
  });
});

describe('ChangedCellsNote', () => {
  it('names the recommendation changes among the card’s cells, with attribution', () => {
    render(
      <MemoryRouter>
        <ChangedCellsNoteView b={B} match={(r) => r.iso3 === 'USA'} label={nodeLabel} query="?country=USA" />
      </MemoryRouter>,
    );
    const n = screen.getByTestId('changed-cells');
    expect(n.dataset.state).toBe('changed');
    expect(n.textContent).toContain('the recommendation changed for 2 assessments');
    expect(n.textContent).toContain('Partner → Preserve');
    expect(n.textContent).toContain('Watch → Accelerate');
    expect(n.querySelector('a')?.getAttribute('href')).toBe('/changes?country=USA&type=recommendation');
  });
  it('says so quietly when no recommendation changed', () => {
    render(
      <MemoryRouter>
        <ChangedCellsNoteView b={B} match={(r: ChangeRecord) => r.iso3 === 'JPN'} label={nodeLabel} />
      </MemoryRouter>,
    );
    const n = screen.getByTestId('changed-cells');
    expect(n.dataset.state).toBe('none');
    expect(n.textContent).toContain('1 assessment moved in other ways');
  });
});

describe('Home: which recommendations changed recently', () => {
  it('lists the latest changes and their attribution', () => {
    render(
      <MemoryRouter>
        <HomeChangesView b={B} />
      </MemoryRouter>,
    );
    const h = screen.getByTestId('home-changes');
    expect(h.textContent).toContain('2 recommendations changed between the 2026-11-01 and 2026-11-21 vintages');
    expect(h.textContent).toContain('New data (same method)');
    expect(h.querySelectorAll('li').length).toBe(2);
  });
  it('does not headline unattributable history as if it were explained', () => {
    const only = bundle([rec('USA', 'Nuclear', 'Assembly', 'v3', ['recommendation'], 'partner', 'preserve')]);
    render(
      <MemoryRouter>
        <HomeChangesView b={only} />
      </MemoryRouter>,
    );
    expect(screen.getByTestId('home-changes').textContent).toContain('None of them can yet be credited to new data or to a method change alone');
  });
});
