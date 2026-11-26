import { cleanup, render, screen, within } from '@testing-library/react';
import { MemoryRouter } from 'react-router';
import { afterEach, describe, expect, it } from 'vitest';
import { ChangeSinceBlock } from '../ChangeSince.tsx';
import { ChangesTable } from '../ChangesView.tsx';
import { changesForCell, checkChangesBundle, checkChangesManifest, detailWindow, latestPair, scoreLines, type ChangeRecord } from '../contract.ts';

afterEach(cleanup);

const GRID = 'aib:posture:USA:electric-grid:components';

const side = (o: Partial<ChangeRecord['before']> = {}): ChangeRecord['before'] => ({
  choice: 'partner',
  posture: 'compete',
  needType: 'supply_chain',
  need: 0.8,
  prize: 0.65,
  cp: 0.62,
  market: 0.4,
  share: 0.03,
  confidence: 'moderate',
  evidence: 'medium',
  ...o,
});

const rec = (o: Record<string, unknown> = {}) => ({
  id: 'aib:posture:USA:electric-grid:components',
  iso3: 'USA',
  tech: 'Electric Grid',
  stage: 'Components',
  prev: 'v2',
  cur: 'v3',
  types: ['recommendation', 'score', 'evidence'],
  subtype: 'mode',
  severity: 'major',
  magnitude: 0.09,
  attribution: 'real_world',
  methodChanged: false,
  methodUnverified: false,
  crossed: ['cp 0.620->0.710 (bar 0.70)'],
  evidenceDirection: 'improved',
  confidenceDirection: null,
  sources: ['comtrade_energy_trade'],
  impacts: ['imp-1'],
  changedInputs: [],
  before: side(),
  after: side({ choice: 'catch_up', cp: 0.71, evidence: 'high' }),
  delta: { need: 0, prize: 0.01, cp: 0.09 },
  explanation: 'Recommendation changed from Partner to Catch Up. Position rose 0.62 → 0.71, crossing the 0.70 bar.',
  ...o,
});

const vint = (id: string, created: string) => ({ id, created, gitCommit: 'abc', provisional: false, stampSource: 'bundle', coreFile: 'x', coreCommit: 'abc', method: { id: 'm-1', label: 't', unverified: false }, data: { id: 'd-1' }, nCells: 3 });
const pair = (prev: string, cur: string, o: Record<string, unknown> = {}) => ({ prev, cur, comparable: true, reason: null, methodChanged: false, methodUnverified: false, dataChanged: true, bridge: false, detail: true, nCells: 3, nUnchanged: 2, counts: { meaningful: 1, recommendation: 1, real_world: 1 }, ...o });

const bundle = (changes: unknown[]) =>
  checkChangesBundle({
    schema: 'osi-changes',
    schema_version: 1,
    created: '2026-11-25T00:00:00Z',
    git_commit: 'abc',
    vintages: [vint('v1', '2026-11-01T00:00:00Z'), vint('v2', '2026-11-10T00:00:00Z'), vint('v3', '2026-11-21T00:00:00Z')],
    pairs: [pair('v1', 'v2'), pair('v2', 'v3')],
    changes,
    impacts: [{ impact_id: 'imp-1', event_id: 'e1', event_slug: 'transformer-plant-cancelled', assessment_id: 'aib:posture:USA:electric-grid:components', dimension: 'position', reason: 'A plant was cancelled.', reason_method: 'editorial', status: 'accepted', reviewer: 'analyst' }],
    sources: {},
  });

describe('change contract', () => {
  it('decodes columnar (schema 2) records back to rows', () => {
    const b = checkChangesBundle({
      schema: 'osi-changes', schema_version: 2, created: 'x', git_commit: 'abc', vintages: [], pairs: [], impacts: [], sources: {},
      changes: { n: 1, columns: { id: [GRID], iso3: ['USA'], tech: ['Electric Grid'], stage: ['Components'], types: [['score']], crossed: [[]],
        sources: [[]], impacts: [[]], changedInputs: [['data/processed/trade_concentration_tbl.rds']], before_choice: ['partner'], after_choice: ['catch_up'], delta_cp: [0.09] } },
    });
    expect(b.changes[0].before.choice).toBe('partner');
    expect(b.changes[0].after.choice).toBe('catch_up');
    expect(b.changes[0].delta.cp).toBe(0.09);
    expect(b.changes[0].changedInputs).toEqual(['data/processed/trade_concentration_tbl.rds']);
  });
  it('rejects a foreign manifest and normalises empty arrays', () => {
    expect(() => checkChangesManifest({ schema: 'other' })).toThrow();
    const b = bundle([rec({ crossed: {}, impacts: null })]);
    expect(b.changes[0].crossed).toEqual([]);
    expect(b.changes[0].impacts).toEqual([]);
  });
  it('orders a cell’s records newest first and finds the latest pair and window', () => {
    const b = bundle([rec({ prev: 'v1', cur: 'v2' }), rec()]);
    expect(changesForCell(b, 'USA', 'Electric Grid', 'Components').map((c) => c.cur)).toEqual(['v3', 'v2']);
    expect(latestPair(b)?.cur).toBe('v3');
    expect(detailWindow(b)).toEqual({ from: '2026-11-01', to: '2026-11-21' });
  });
  it('shows score lines past the threshold or across a bar only', () => {
    const lines = scoreLines(bundle([rec()]).changes[0]);
    expect(lines.map((l) => l.label)).toEqual(['Position']);
    expect(lines[0].crossed).toBe(true);
  });
});

describe('ChangeSinceBlock', () => {
  it('names the changed engine inputs behind a change', () => {
    render(
      <MemoryRouter>
        <ChangeSinceBlock b={bundle([rec({ changedInputs: ['data/processed/trade_concentration_tbl.rds'] })])} iso3="USA" tech="Electric Grid" stage="Components" />
      </MemoryRouter>,
    );
    expect(screen.getByTestId('change-inputs').textContent).toContain('trade_concentration_tbl.rds');
  });
  it('headlines a change in the latest pair and opens the why panel', () => {
    render(
      <MemoryRouter>
        <ChangeSinceBlock b={bundle([rec()])} iso3="USA" tech="Electric Grid" stage="Components" />
      </MemoryRouter>,
    );
    const block = screen.getByTestId('change-since');
    expect(within(block).getByText('Changed since last vintage')).toBeTruthy();
    expect(within(block).getByTestId('change-line-rec').textContent).toContain('Partner → Catch Up');
    expect(within(block).getByTestId('change-line-position').textContent).toContain('0.62 → 0.71');
    expect(within(block).getByTestId('change-line-evidence').textContent).toContain('Medium → High');
    expect(within(block).getByTestId('change-method').textContent).toContain('unchanged');
    expect(within(block).getByTestId('change-impacts').textContent).toContain('transformer-plant-cancelled');
  });
  it('shows a change of evidence basis even when the quality band holds', () => {
    const r = rec({ types: ['evidence'], before: side({ evidenceStatus: 'proxy' }), after: side({ evidenceStatus: 'exact' }) });
    render(
      <MemoryRouter>
        <ChangeSinceBlock b={bundle([r])} iso3="USA" tech="Electric Grid" stage="Components" />
      </MemoryRouter>,
    );
    expect(screen.getByTestId('change-line-evidence-basis').textContent).toContain('Proxy → Exact (stage-specific)');
    expect(screen.queryByTestId('change-line-evidence')).toBeNull();
  });
  it('says when the last change is older than the latest pair', () => {
    render(
      <MemoryRouter>
        <ChangeSinceBlock b={bundle([rec({ prev: 'v1', cur: 'v2' })])} iso3="USA" tech="Electric Grid" stage="Components" />
      </MemoryRouter>,
    );
    expect(screen.getByText('Last changed 2026-11-10')).toBeTruthy();
  });
  it('states no material change for an unchanged cell', () => {
    render(
      <MemoryRouter>
        <ChangeSinceBlock b={bundle([rec()])} iso3="JPN" tech="Solar" stage="Assembly" />
      </MemoryRouter>,
    );
    expect(screen.getByTestId('change-since-none').textContent).toContain('No material change');
  });
});

describe('ChangesTable', () => {
  it('filters by economy from the URL', () => {
    render(
      <MemoryRouter initialEntries={['/changes?country=JPN']}>
        <ChangesTable b={bundle([rec(), rec({ id: 'aib:posture:JPN:solar:assembly', iso3: 'JPN', tech: 'Solar', stage: 'Assembly' })])} />
      </MemoryRouter>,
    );
    const rows = screen.getAllByTestId('changes-row');
    expect(rows).toHaveLength(1);
    expect(rows[0].textContent).toContain('JPN');
  });
});
