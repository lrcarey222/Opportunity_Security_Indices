// The technology page's answer first (external review readiness, 2026-11-23): "Where are the real
// bottlenecks?" from the AIB nodes (pure derivation on synthetic nodes, invariants on the shipped document),
// the HHI band line, and, on the TS fixture, the page order (bottlenecks, the share split, the chain
// navigator, the stage grid) with stages lacking comparable evidence said once instead of one empty card each.

import { existsSync, readFileSync } from 'node:fs';
import { resolve } from 'node:path';
import { afterAll, afterEach, beforeAll, beforeEach, describe, expect, it } from 'vitest';
import { cleanup, render, screen, within } from '@testing-library/react';
import { createMemoryRouter, RouterProvider } from 'react-router';
import { routes } from '../../routes.tsx';
import type { AibDoc, AibNode } from '../../aib/contract.ts';
import { AVOID_RE } from '../../content/vocabulary.ts';
import { buildSectorFixture, fixtureSource } from '../__fixtures__/build.ts';
import { SectorStore, setSectorStoreForTests } from '../data/store.ts';
import { fixtureSource as aibFixtureSource, SYNTHETIC_DIR as AIB_DIR } from '../../aib/__tests__/fixture.ts';
import { AibStore, setAibStoreForTests } from '../../aib/data/store.ts';
import { ACTION_CLASSES, bottleneckWhy, nodeStageIn, techBottlenecks, topConcentration, type ConcentrationLine } from './Bottlenecks.tsx';
import { hhiBand, hhiBandThresholds } from './HhiExplainer.tsx';
import { DASH, fmtShare, fmtSig, fmtValue, NO_DATA_TITLE } from '../format.ts';
import { coverageChips } from '../derive/cells.ts';

const node = (o: Partial<AibNode> & { id: string; cls: string; yfCells: string[] }): AibNode =>
  ({ label: o.id, techs: [], stage: 'Refining', reason: null, quality: null, flags: [], exposure: null, static: null, existing: null, optimized: null, dividend: null, residual: null, demandUsd: null, lostUsd: null, baseLargest: null, baseLargestShare: null, largest: null, largestShare: null, hhi: null, hosts: [], bestGain: null, partners: null, treatment: null, compact: null, yfSubcomponents: [], frontier: [], ...o }) as AibNode;

describe('techBottlenecks (pure)', () => {
  const nodes = [
    node({ id: 'a', cls: 'BUILD', yfCells: ['batteries-assembly'], exposure: 0.7, residual: 0.69, baseLargest: 'CHN', baseLargestShare: 0.69, hhi: 0.08 }),
    node({ id: 'b', cls: 'COORDINATE', yfCells: ['electric-vehicles-components', 'batteries-refining'], exposure: 0.4, residual: 0.02, dividend: 0.3 }),
    node({ id: 'c', cls: 'LOW INTERVENTION', yfCells: ['batteries-components'], exposure: 0.9, residual: 0.9 }),
    node({ id: 'd', cls: 'MANAGE SPECIALIZATION', yfCells: ['batteries-refining'], exposure: 0.3, residual: 0.2, baseLargest: 'TWN', baseLargestShare: 0.55 }),
    node({ id: 'e', cls: 'BUILD', yfCells: ['electric-grid-assembly'], exposure: 0.9, residual: 0.9 }),
    node({ id: 'f', cls: 'BUILD', yfCells: ['batteries-extraction'], exposure: 0.5, residual: 0.2 }),
  ];
  it('keeps the technology’s nodes that need action, largest remaining gap first, then supply exposed', () => {
    const r = techBottlenecks(nodes, 'batteries');
    expect(r.assessed.map((n) => n.id)).toEqual(['a', 'b', 'c', 'd', 'f']);
    expect(r.rows.map((x) => x.node.id)).toEqual(['a', 'f', 'd', 'b']);
    expect(r.rows.every((x) => ACTION_CLASSES.has(x.node.cls))).toBe(true);
  });
  it('reads the stage from the technology’s own cell, never a prefix of another technology', () => {
    expect(nodeStageIn(nodes[1], 'batteries')).toBe('refining');
    expect(nodeStageIn(nodes[4], 'electric-vehicles')).toBeNull();
    expect(techBottlenecks(nodes, 'electric-vehicles').assessed.map((n) => n.id)).toEqual(['b']);
  });
  it('says why from the class and the stress-test shares, and leaves out a figure the node lacks', () => {
    expect(bottleneckWhy(nodes[0], 'China')).toBe('Build capacity: a major-supplier disruption would cut off 70% of allied supply, and even coordinated trade would leave a 69% gap.');
    expect(bottleneckWhy(nodes[1], null)).toBe('Coordinate trade: a major-supplier disruption would cut off 40% of allied supply; acting together would recover an extra 30%, leaving a 2% gap.');
    expect(bottleneckWhy(nodes[3], 'Taiwan')).toBe('Manage specialization: Taiwan supplies 55% of allied demand today; a major-supplier disruption would cut off 30% of allied supply, leaving a 20% gap even with coordinated trade.');
    expect(bottleneckWhy(node({ id: 'x', cls: 'BUILD', yfCells: [] }), null)).toBe('Build capacity: trade alone cannot close the gap.');
    for (const n of nodes) expect(bottleneckWhy(n, 'China')).not.toMatch(AVOID_RE);
  });
  it('the concentration-only fallback ranks by the largest holder’s share, then China’s', () => {
    const l = (id: string, top1Share: number | null, china: number): ConcentrationLine => ({ id, label: id, stage: 'refining', top1Name: 'X', top1Share, china });
    expect(topConcentration([l('a', 0.3, 0.1), l('b', 0.6, 0), l('c', null, 0.9), l('d', 0.6, 0.2)]).map((x) => x.id)).toEqual(['d', 'b', 'a']);
  });
});

describe('the shipped AIB document', () => {
  const dir = resolve(process.cwd(), 'public/data/aib');
  const has = existsSync(resolve(dir, 'manifest.json'));
  it.skipIf(!has)('every listed step needs action, is sorted by remaining gap and names today’s supplier, not the optimized network', () => {
    const m = JSON.parse(readFileSync(resolve(dir, 'manifest.json'), 'utf8')) as { files: { compacts: string } };
    const doc = JSON.parse(readFileSync(resolve(dir, m.files.compacts), 'utf8')) as AibDoc;
    for (const tech of ['batteries', 'solar', 'nuclear', 'electric-grid', 'green-hydrogen', 'geothermal'] as const) {
      const r = techBottlenecks(doc.nodes, tech);
      expect(r.rows.every((x) => ACTION_CLASSES.has(x.node.cls) && x.node.yfCells.includes(`${tech}-${x.stage}`))).toBe(true);
      for (let i = 1; i < r.rows.length; i++) expect((r.rows[i - 1].node.residual ?? -1) >= (r.rows[i].node.residual ?? -1)).toBe(true);
      for (const x of r.rows) {
        const why = bottleneckWhy(x.node, x.node.baseLargest);
        // the "today" share is the observed baseline, never node.hhi / largestShare (the post-shock state)
        if (x.node.cls === 'MANAGE SPECIALIZATION' && x.node.baseLargestShare !== null) expect(why).toContain(fmtShare(x.node.baseLargestShare, 0));
        expect(why).not.toMatch(AVOID_RE);
      }
    }
  });
});

describe('the HHI band line', () => {
  const refs = [
    { id: 'eu_2004_unlikely', value: 0.1, points: 1000, side: 'below' as const, label: 'concentration concerns unlikely', authority: 'European Commission', document: '', year: 2004 },
    { id: 'us_2010_moderate', value: 0.15, points: 1500, side: 'above' as const, label: 'moderately concentrated (to 0.25)', authority: 'US DOJ and FTC', document: '', year: 2010 },
    { id: 'us_2023_high', value: 0.18, points: 1800, side: 'above' as const, label: 'highly concentrated', authority: 'US DOJ and FTC', document: '', year: 2023 },
    { id: 'us_2010_high', value: 0.25, points: 2500, side: 'above' as const, label: 'highly concentrated (2010 guidelines)', authority: 'US DOJ and FTC', document: '', year: 2010 },
  ];
  it('takes both bounds from one set of guidelines in dict.hhi_refs (the 2023 value is not mixed in)', () => {
    expect(hhiBandThresholds(refs)).toMatchObject({ moderate: 0.15, high: 0.25 });
    expect(hhiBandThresholds([])).toMatchObject({ moderate: 0.15, high: 0.25 });
  });
  it('one plain line per band; a lower bound only moves up; no value, no line', () => {
    expect(hhiBand(0.31, false, refs)?.text).toBe('Highly concentrated: a few suppliers hold most of this market');
    expect(hhiBand(0.25, false, refs)?.key).toBe('high');
    expect(hhiBand(0.2, false, refs)?.key).toBe('moderate');
    expect(hhiBand(0.2, true, refs)?.text).toMatch(/^At least moderately concentrated/);
    expect(hhiBand(0.08, false, refs)?.text).toBe('Not concentrated: supply is spread across many suppliers');
    expect(hhiBand(0.08, true, refs)?.text).toMatch(/^Possibly more concentrated than shown/);
    expect(hhiBand(NaN, false, refs)).toBeNull();
    expect(hhiBand(null, false, refs)).toBeNull();
    expect(hhiBand(0.31, false, refs)?.title).toMatch(/0\.25 and above highly concentrated/);
  });
});

describe('zero, missing and not applicable never look the same', () => {
  it('a true zero prints a zero; a missing value prints the dash', () => {
    expect(fmtShare(0)).toBe('0.0%');
    expect(fmtSig(0)).toBe('0');
    expect(fmtValue(0, { short: 'kt', unit_kind: 'kt' })).toBe('0 kt');
    expect(fmtShare(NaN)).toBe(DASH);
    expect(fmtShare(null)).toBe(DASH);
    expect(NO_DATA_TITLE).toBe('No data');
  });
  it('a not-applicable coverage row says "not applicable", not "not available"', () => {
    const model = {
      coverageOf: () => [{ family: 'reserves', status: 'none', reason: 'not_applicable', n_geo: 0, sources: [] }],
      families: new Map([['reserves', { label: 'Reserves', section: 'resources' }]]),
      sources: new Map(),
    } as unknown as Parameters<typeof coverageChips>[0];
    const [chip] = coverageChips(model, { idx: 0 } as Parameters<typeof coverageChips>[1]);
    expect(chip.iconWords).toBe('not applicable');
    expect(chip.text).toBe('not applicable to this stage');
  });
});

// ---- the page on the TS fixture -----------------------------------------------------------------------

const restore: (() => void)[] = [];
beforeAll(() => {
  window.matchMedia = ((q: string) => ({
    matches: /min-width/.test(q),
    media: q, onchange: null,
    addEventListener: () => undefined, removeEventListener: () => undefined, addListener: () => undefined, removeListener: () => undefined, dispatchEvent: () => false,
  })) as unknown as typeof window.matchMedia;
  window.scrollTo = (() => undefined) as typeof window.scrollTo;
  Element.prototype.scrollIntoView = function () {} as typeof Element.prototype.scrollIntoView;
  const NodeRequest = globalThis.Request;
  globalThis.Request = class extends NodeRequest {
    constructor(input: RequestInfo | URL, init?: RequestInit) {
      const rest = { ...(init ?? {}) };
      delete (rest as { signal?: unknown }).signal;
      super(input, rest);
    }
  } as typeof Request;
  restore.push(() => (globalThis.Request = NodeRequest));
});
afterAll(() => {
  for (const r of restore.reverse()) r();
});
beforeEach(() => {
  setSectorStoreForTests(new SectorStore(fixtureSource(buildSectorFixture())));
  setAibStoreForTests(new AibStore(aibFixtureSource(AIB_DIR)));
});
afterEach(() => {
  cleanup();
  setSectorStoreForTests(null);
  setAibStoreForTests(null);
});

describe('the technology page leads with its answer', () => {
  it('Batteries: bottlenecks from the AIB nodes, then the share split, then the chain navigator', async () => {
    const router = createMemoryRouter(routes, { initialEntries: ['/sector/batteries'] });
    render(<RouterProvider router={router} />);
    await screen.findByRole('heading', { level: 1, name: 'Batteries: the supply chain' }, { timeout: 30_000 });
    const block = screen.getByTestId('bottlenecks');
    expect(within(block).getByRole('heading', { level: 2, name: 'Where are the real bottlenecks?' })).toBeTruthy();
    const list = await within(block).findByTestId('bottlenecks-list', {}, { timeout: 20_000 });
    const items = [...list.querySelectorAll('li')];
    expect(items.length).toBeGreaterThan(0);
    // each links to its stage profile and says why in plain words
    const first = within(items[0]).getByRole('link');
    expect(first.getAttribute('href')).toMatch(/^\/sector\/batteries\/(extraction|refining|components|assembly|deployment|recovery)$/);
    expect(items[0].textContent).toMatch(/largest supplier today/);
    expect(block.textContent).not.toMatch(AVOID_RE);
    // the order: bottlenecks, the share split, the chain navigator
    const chart = screen.getByText('Where concentration sits along the chain');
    const nav = screen.getByTestId('chain-nav');
    expect(block.compareDocumentPosition(chart) & Node.DOCUMENT_POSITION_FOLLOWING).toBeTruthy();
    expect(chart.compareDocumentPosition(nav) & Node.DOCUMENT_POSITION_FOLLOWING).toBeTruthy();
    // no empty stage card
    expect(document.body.textContent).not.toMatch(/No country-level primary measure for this stage/);
  });

  it('a technology with thin stages: one line names them, each linked to its profile; no empty card', async () => {
    const router = createMemoryRouter(routes, { initialEntries: ['/sector/geothermal'] });
    render(<RouterProvider router={router} />);
    await screen.findByRole('heading', { level: 1, name: 'Geothermal: the supply chain' }, { timeout: 30_000 });
    const line = await screen.findByTestId('no-evidence-stages', {}, { timeout: 20_000 });
    expect(line.textContent).toMatch(/^Comparable country-level evidence is not currently available for /);
    const links = within(line).getAllByRole('link');
    expect(links.length).toBeGreaterThan(0);
    for (const a of links) expect(a.getAttribute('href')).toMatch(/^\/sector\/geothermal\/[a-z]+$/);
    // a stage named in the line has no card
    const cards = screen.queryAllByRole('heading', { level: 3 }).map((h) => h.textContent);
    for (const a of links) expect(cards).not.toContain(a.textContent);
  });
});
