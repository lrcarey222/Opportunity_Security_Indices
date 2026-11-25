// "Why this result?" (review/components/WhyThisResult.tsx): collapsed by default with nothing of the chain in the
// page; opened, the provenance chain of the cell from the conclusion to the sources, each link with its status
// in words, the bar and the margin of a decision step, the source and vintage where the dictionary has them,
// and the gaps listed ("not in this release", "from configuration"), never hidden.

import { afterEach, describe, expect, it, vi } from 'vitest';
import { cleanup, fireEvent, render, screen } from '@testing-library/react';
import { nodeDetail } from '../../posture/detail.ts';
import type { PostureMeta } from '../../posture/contract.ts';
import { SUPPLY_STAGES } from '../../lib/stages.ts';
import { linkMargin, WhyThisResult } from './WhyThisResult.tsx';

afterEach(cleanup);

// a synthetic meta and Catch Up cell in the shipped shapes (qc/__tests__/fixtures.ts builds the same, but reads
// node:fs at import, which a jsdom test cannot)
const meta = {
  techs: ['Batteries'], stages: [...SUPPLY_STAGES], stage_display: {}, benchmark_period: '2022-2024', momentum_period: 'Jan 2025 - Dec 2025', momentum_previous_period: null,
  momentum_through: null, momentum_band: 0.1, thresholds: { need: 0.67, prize: 0.6, cp: 0.75, deployment: 0.6, borderline_band: 0.05 }, scenario_labels: {},
  family_scenarios: { demand: 'current_policy', policy_gap: 'national_commitments', system: 'current_policy' }, phrases: {}, core_members: 0, partner_members: 0,
  prize_measure: 'strategic_value', yard_codes: { S: 'small', M: 'medium', L: 'large' }, large_yard_signals: [],
  yf_nodes: {
    'Batteries|Refining|stage': { key: 'Batteries|Refining|stage', tech: 'Batteries', stage: 'Refining', layer: 'stage', label: null, largeYard: false, n: 2, signals: [], unknown: [], floor: false, commons: 0.33, option: 0, commonsN: 3, optionN: 4, cap: 'market_only' },
  },
} as unknown as PostureMeta;
const trace = [
  { step: 'secure', test: 'secure_override', result: 'no' },
  { step: 'large_yard', test: 'large_yard_case', value: '2', threshold: '5', result: 'no' },
  { step: 'need', test: 'supply_chain_need', value: 0.45, threshold: 0.67, result: 'low' },
  { step: 'prize', test: 'strategic_industrial_value', value: 0.83, threshold: 0.6, result: 'high' },
  { step: 'position', test: 'cp_contender_pct', value: 0.31, threshold: 0.75, result: 'not_top_quarter' },
  { step: 'pathway', test: 'pathway_credible', value: null, result: 'yes' },
  { step: 'choice', test: 'choice', result: 'catch_up' },
];
const cell = nodeDetail({
  tech: 'Batteries', stage: 'Refining', posture: 'compete', mode: 'catch_up', deploy_mode: null, need: 0.45, cp: 0.31, home_share: 0.01,
  prize: { value: 0.83, measure: 'strategic_value', market: 0.78 }, pathway_credible: 'yes', borderline: [], trace, reason: null, security_rationale: null,
  import_dependence: 0.44, china_share: 0.13, imports_to_gdp: 0.0002, cp_families: { export_position: 0.66 }, trade: { benchmark_source: 'comtrade', benchmark_share: 0.034 },
});
const dictionary = { sources: { comtrade_reported: { id: 'comtrade_reported', short: 'UN Comtrade', edition: '2026-10', licence: 'terms of use', accessed: null, transform: null } } };

describe('WhyThisResult', () => {
  it('is collapsed by default and loads nothing until opened', () => {
    const load = vi.fn(async () => dictionary);
    render(<WhyThisResult cell={cell} core={{ meta }} loadDictionary={load} />);
    const d = screen.getByTestId('why-this-result') as HTMLDetailsElement;
    expect(d.open).toBe(false);
    expect(screen.queryByTestId('why-chain')).toBeNull();
    expect(load).not.toHaveBeenCalled();
  });

  it('opened: the chain from the conclusion to the source, every status in words, gaps stated', async () => {
    render(<WhyThisResult cell={cell} core={{ meta }} dictionary={dictionary} />);
    const d = screen.getByTestId('why-this-result') as HTMLDetailsElement;
    d.open = true;
    fireEvent(d, new Event('toggle'));
    const chain = await screen.findByTestId('why-chain');
    await screen.findByText(/shipped · \d+ from configuration · \d+ not in this release/);
    const root = chain.querySelector('[data-link="choice"]')!;
    expect(root.textContent).toContain('Strategic conclusion');
    expect(root.textContent).toContain('Catch Up');
    // a decision step with its value, bar and margin
    const need = chain.querySelector('[data-link="trace:supply_chain_need"]')!;
    expect(need.textContent).toMatch(/0\.45.*bar 0\.67.*margin -0\.22/);
    // statuses: shipped, from configuration, not in this release
    const statuses = [...chain.querySelectorAll('[data-status]')].map((e) => e.getAttribute('data-status'));
    expect(statuses).toContain('shipped');
    expect(statuses).toContain('config_only');
    expect(statuses).toContain('not_in_release');
    const hhi = chain.querySelector('[data-link="raw:import_hhi"]')!;
    expect(hhi.querySelector('[data-status]')!.textContent).toBe('not in this release');
    expect(chain.querySelector('[data-link="config:score_floor"] [data-status]')!.textContent).toBe('from configuration');
    // the source and vintage of the trade benchmark
    const bench = chain.querySelector('[data-link="raw:benchmark_share"]')!;
    expect(bench.textContent).toContain('comtrade_reported (UN Comtrade)');
    expect(bench.textContent).toMatch(/Source year \/ vintage: 2022-2024/);
    expect(screen.getByTestId('why-gaps').textContent).toMatch(/not in this release/);
  });

  it('loads the source dictionary once when opened and shows its names', async () => {
    const load = vi.fn(async () => dictionary);
    render(<WhyThisResult cell={cell} core={{ meta }} loadDictionary={load} defaultOpen />);
    expect((await screen.findAllByText(/comtrade_reported \(UN Comtrade\)/)).length).toBeGreaterThan(0);
    expect(load).toHaveBeenCalledTimes(1);
  });

  it('says so when the source dictionary cannot be loaded', async () => {
    render(<WhyThisResult cell={cell} core={{ meta }} loadDictionary={() => Promise.reject(new Error('offline'))} defaultOpen />);
    await screen.findByText(/Source names could not be loaded/);
  });

  it('a step without its details says so', () => {
    render(<WhyThisResult cell={null} core={{ meta }} dictionary={dictionary} defaultOpen />);
    expect(screen.getByText('The details of this step are not loaded yet.')).toBeTruthy();
  });

  it('linkMargin is value minus bar, only for numbers', () => {
    expect(linkMargin({ value: 0.8, threshold: 0.75 })).toBeCloseTo(0.05);
    expect(linkMargin({ value: '3', threshold: '5' })).toBeNull();
  });
});
