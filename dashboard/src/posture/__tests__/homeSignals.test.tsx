import { cleanup, render, screen } from '@testing-library/react';
import { MemoryRouter } from 'react-router';
import { afterEach, describe, expect, it } from 'vitest';
import { HomeSignalsView } from '../HomeSignals.tsx';
import { checkPostureSignals } from '../signals.ts';

afterEach(cleanup);

const cell = { iso3: 'USA', name: 'United States', tech: 'Solar', stage: 'Components', trusted: true };
const signals = () =>
  checkPostureSignals({
    schema: 'osi-posture-signals', schema_version: 1, created: 'x',
    exports: { period: 'Sep 2025 - Aug 2026', previous_period: 'Sep 2024 - Aug 2025', band: 0.1, n_assessed: 10, n_up: 4, n_down: 2, n_up_trusted: 3, n_down_trusted: 1,
      growing: [], growing_trusted: [{ ...cell, growth: 0.42, log_growth: 0.351, world_share: 0.05 }], falling_trusted: [] },
    dependence: { window_end: 202608, n_refreshed: 50, n_china_up: 5, n_china_down: 2, n_china_up_trusted: 2, n_china_down_trusted: 1,
      rising_trusted: [{ ...cell, stage: 'Assembly', china_base: 0.1, china_latest: 0.4, coercive_base: 0.12, coercive_latest: 0.41, source: 'ember_fill' }], rising: [] },
  });

describe('Home: what is moving in trade', () => {
  it('answers both questions for the trusted network and links each line to its step', () => {
    render(
      <MemoryRouter>
        <HomeSignalsView s={signals()} />
      </MemoryRouter>,
    );
    const ex = screen.getByTestId('home-signals-exports');
    expect(ex.textContent).toContain('+42%');
    expect(ex.textContent).toContain('3 trusted-network cells');
    expect(ex.querySelector('a')?.getAttribute('href')).toBe('/countries/USA?node=Solar%7CComponents');
    const dep = screen.getByTestId('home-signals-dependence');
    expect(dep.textContent).toContain('China 10% → 40%');
    expect(dep.textContent).toContain('(estimated)');
    expect(dep.textContent).toContain('12 months to Aug 2026');
    expect(screen.getByTestId('home-signals').textContent).toContain('not changes to the assessment');
  });
  it('rejects a foreign document', () => {
    expect(() => checkPostureSignals({ schema: 'other' })).toThrow();
  });
});
