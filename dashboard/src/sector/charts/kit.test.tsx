// The Sector chart kit and chrome (spec_sector.md 5.3, 10.3 "Chart kit (B)"): each component renders
// the hatch, the zero state, the highlight and its table twin; legends appear for two or more series;
// the phone SectorBar opens a dialog that traps focus and closes on Escape.

import { afterAll, afterEach, beforeAll, describe, expect, it, vi } from 'vitest';
import { cleanup, fireEvent, render, screen, within } from '@testing-library/react';
import { createMemoryRouter, RouterProvider } from 'react-router';
import type { ReactNode } from 'react';
import {
  Beeswarm, BlocBar, CaptionProvider, CellGrid, contrast, CoverageChips, Dumbbell, GroupArea, GroupLegend, HeatTable, LabelledLines, LeagueBars, leagueRows,
  SectorChartCard, ShareSplitChart, sharedDomain, SmallMultiples, splitSegments, StateLegend, SvgDefs, textOn, TopFlowsList, GAP_ID,
  type LeagueRow, type ShareSplitRow,
} from './index.ts';
import { PageNotesProvider, SectionRail, SectorBar, ThemeChips } from '../chrome/index.ts';
import { GROUP_COLOR, GROUP_ORDINAL, SEQ, SIGNAL, VOLATILITY_ORDINAL } from '../../theme/palette.ts';
import * as Plot from '@observablehq/plot';
import { compactSi, logTicks } from '../../charts/theme.ts';
import { lineDashes, spreadLabels } from './LabelledLines.tsx';
import { RING_INNER, RING_OUTER, TRI, TRI_HIGH, TRI_LOW, TRI_MID } from '../../theme/palette.ts';
import { triScale } from '../../charts/scale.ts';

let width = 1440;
/** an element width override (a chart wider than the 900 px default) */
let clientW: number | null = null;
const restore: (() => void)[] = [];

/** matchMedia that answers (min-width: Npx) for the current test width. */
function setWidth(px: number) {
  width = px;
  window.matchMedia = ((q: string) => {
    const m = /min-width:\s*(\d+)px/.exec(q);
    return {
      matches: m ? px >= Number(m[1]) : false, media: q, onchange: null,
      addEventListener: () => undefined, removeEventListener: () => undefined, addListener: () => undefined, removeListener: () => undefined, dispatchEvent: () => false,
    };
  }) as unknown as typeof window.matchMedia;
}

beforeAll(() => {
  const cw = Object.getOwnPropertyDescriptor(HTMLElement.prototype, 'clientWidth');
  Object.defineProperty(HTMLElement.prototype, 'clientWidth', { configurable: true, get: () => clientW ?? Math.min(900, width) });
  restore.push(() => cw && Object.defineProperty(HTMLElement.prototype, 'clientWidth', cw));
  window.scrollTo = (() => undefined) as typeof window.scrollTo;
  const NodeRequest = globalThis.Request;
  globalThis.Request = class extends NodeRequest {
    constructor(input: RequestInfo | URL, init?: RequestInit) {
      const rest = { ...(init ?? {}) };
      delete rest.signal;
      super(input, rest);
    }
  } as typeof Request;
  restore.push(() => (globalThis.Request = NodeRequest));
});
afterEach(() => {
  cleanup();
  setWidth(1440);
  clientW = null;
});
afterAll(() => restore.forEach((f) => f()));
setWidth(1440);

/** Render inside a data router (Link needs one). */
function inRouter(ui: ReactNode) {
  const router = createMemoryRouter([{ path: '*', element: <>{ui}</> }], { initialEntries: ['/sector/batteries/assembly'] });
  return render(<RouterProvider router={router} />);
}

const rowA: ShareSplitRow = {
  id: 'exports',
  label: 'Exports 2024',
  sub: 'UN Comtrade, reconciled matrix',
  parts: { china: 0.437, core: 0.379, partner: 0.14, rest: 0.03 },
  cols: { top1: 'China 43.7%', hhi: '0.207' },
  highlight: { offset: 0.5, label: 'USA 4.8% (#4)' },
  section: 'assembly',
};
const rowB: ShareSplitRow = { id: 'inv', label: 'Investment 2025', parts: { china: 0.412, core: 0.527, partner: 0.061 }, cols: { top1: 'China 41.2%', hhi: '0.303' }, section: 'assembly', href: '/sector/batteries/assembly#investment' };
const rowP: ShareSplitRow = { id: 'p2040', label: '2040', parts: { china: 0.6, core: 0.1 }, cols: { top1: 'China 60.0%', hhi: '≥ 0.37' }, projection: true, section: 'assembly' };

describe('palette tokens', () => {
  it('text on every sequential step reads at 4.5:1 or better; ordinal ramps are four steps', () => {
    for (const s of SEQ) expect(contrast(textOn(s), s)).toBeGreaterThanOrEqual(4.5);
    // labels inside share-split segments (integrator, axe color-contrast: ink-900 on core blue was 4.42)
    for (const s of Object.values(GROUP_COLOR)) expect(contrast(textOn(s), s), s).toBeGreaterThanOrEqual(4.5);
    for (const r of Object.values(GROUP_ORDINAL)) expect(r).toHaveLength(4);
    expect(VOLATILITY_ORDINAL).toHaveLength(4);
  });
});

describe('ShareSplitChart and BlocBar', () => {
  it('splits in the fixed order, hatches the residual, ticks the highlight and labels wide segments', () => {
    expect(splitSegments(rowA.parts).map((s) => s.key)).toEqual(['china', 'core', 'partner', 'rest', 'unattributed']);
    expect(splitSegments({ china: 0.5, core: 0.5 }).map((s) => s.key)).toEqual(['china', 'core']);
    // the text columns sit beside the bars from a 1024 px chart width
    clientW = 1200;
    const onRow = vi.fn();
    inRouter(
      <ShareSplitChart
        rows={[rowA, rowB, rowP]}
        columns={[
          { key: 'top1', label: 'Top-1' },
          { key: 'hhi', label: 'HHI', num: true },
        ]}
        sections={[{ key: 'assembly', label: 'Assembly', stage: 'assembly' }]}
        ariaLabel="Who holds the market"
        highlightHeader="United States"
        onRowClick={onRow}
      />,
    );
    const table = screen.getByRole('table', { name: 'Who holds the market' });
    expect(within(table).getByRole('columnheader', { name: 'HHI' })).toBeTruthy();
    expect(within(table).getByRole('columnheader', { name: 'United States' })).toBeTruthy();
    expect(within(table).getByText('Assembly')).toBeTruthy();
    // a stage is never a colour (design S4): the section stripe is the neutral rule, with no inline colour
    const stripe = within(table).getByText('Assembly').parentElement?.querySelector('span[aria-hidden="true"]') as HTMLElement;
    expect(stripe.className).toContain('bg-ink-500');
    expect(stripe.style.background).toBe('');
    const bars = screen.getAllByTestId('split-bar');
    const parts = [...bars[0].querySelectorAll<HTMLElement>('[data-part]')];
    expect(parts.map((p) => p.dataset.part)).toEqual(['china', 'core', 'partner', 'rest', 'unattributed']);
    // the residual (1 - 0.986) is hatched
    expect(parts[4].dataset.hatch).toBe('');
    // labels inside segments >= 9% only
    expect(parts[0].textContent).toBe('44%');
    expect(parts[3].textContent).toBe('');
    expect(screen.getAllByTestId('split-highlight')).toHaveLength(1);
    expect(screen.getByText('USA 4.8% (#4)')).toBeTruthy();
    // projection: outlined segments and the word
    expect(screen.getByText('projection')).toBeTruthy();
    const proj = bars[2].querySelector<HTMLElement>('[data-part="china"]') as HTMLElement;
    expect(proj.style.background).toBe('transparent');
    expect(proj.style.border).toMatch(/2px solid/);
    // rows read in words; a row with a link links; others call onRowClick once
    expect(screen.getByRole('row', { name: /Exports 2024: China 43\.7%, Coalition core 37\.9%/ })).toBeTruthy();
    expect(screen.getByRole('link', { name: 'Investment 2025' }).getAttribute('href')).toBe('/sector/batteries/assembly#investment');
    fireEvent.click(screen.getByRole('button', { name: 'Exports 2024' }));
    expect(onRow).toHaveBeenCalledTimes(1);
  });

  it('drops the text columns below each bar in a chart narrower than 1024 px (a two-up card), and the source column', () => {
    clientW = 600;
    inRouter(<ShareSplitChart rows={[rowA]} columns={[{ key: 'top1', label: 'Top' }, { key: 'source', label: 'Source' }]} ariaLabel="Split" />);
    expect(screen.queryByRole('columnheader')).toBeNull();
    expect(screen.getByText('Top China 43.7% · USA 4.8% (#4)')).toBeTruthy();
  });

  it('drops the text columns below each bar on a phone', () => {
    setWidth(390);
    inRouter(<ShareSplitChart rows={[rowA]} columns={[{ key: 'top1', label: 'Top' }, { key: 'hhi', label: 'HHI' }]} ariaLabel="Split" />);
    expect(screen.getByText('Top China 43.7% · HHI 0.207 · USA 4.8% (#4)')).toBeTruthy();
  });

  it('BlocBar says the parts in words', () => {
    render(<BlocBar parts={{ china: 0.25, core: 0.5 }} ariaLabel="Holders" label="Holders 2024" />);
    expect(screen.getByRole('figure', { name: 'Holders' })).toBeTruthy();
    expect(screen.getByText('China 25.0%')).toBeTruthy();
    expect(screen.getByText('Not attributed 25.0%')).toBeTruthy();
  });
});

const league: LeagueRow[] = [
  { id: 'CHN', label: 'China', group: 'china', value: 88.67e9, display: 'USD 88.7 bn · 43.7%' },
  { id: 'KOR', label: 'Korea', group: 'core', value: 10.18e9, display: 'USD 10.2 bn · 5.0%' },
  { id: 'IDN', label: 'Indonesia', group: 'partner', value: 9.9e9, display: 'USD 9.9 bn · 4.9%' },
  { id: 'BRA', label: 'Brazil', group: 'partner', value: 0, display: '0', state: 'zero' },
  { id: 'GAB', label: 'Gabon', group: 'partner', value: NaN, display: '', state: 'nq' },
  { id: 'USA', label: 'United States', group: 'core', value: 9.78e9, display: 'USD 9.8 bn · 4.8%', pinned: true },
];

describe('LeagueBars', () => {
  it('shows the top rows, a gap, the pinned highlight bold, zero and withheld states', () => {
    const shown = leagueRows(league, 3, 'USA');
    expect(shown.map((r) => r.id)).toEqual(['CHN', 'KOR', 'IDN', GAP_ID, 'USA']);
    const { container } = render(<LeagueBars rows={league} top={5} selected="USA" axisLabel="US dollars, 2024" ariaLabel="Top exporters" />);
    const svg = container.querySelector('svg[aria-label="Top exporters"]') as SVGSVGElement;
    expect(svg).toBeTruthy();
    const texts = [...svg.querySelectorAll('text')].map((t) => t.textContent);
    expect(texts).toContain('USD 88.7 bn · 43.7%');
    expect(texts).toContain('Listed without a figure');
    expect(texts).toContain('0');
    const bold = [...svg.querySelectorAll('g[font-weight="700"] text, text[font-weight="700"]')].map((t) => t.textContent);
    expect(bold).toContain('United States');
    // the selected bar carries the orange stroke
    expect(svg.querySelector(`g[stroke="${SIGNAL}"] path, g[stroke="${SIGNAL}"] rect`)).toBeTruthy();
  });

  it('log scale: every finite row draws one bar (from the decade below the smallest), ticks at decades only', () => {
    const rows: LeagueRow[] = [
      { id: 'CHL', label: 'Chile', group: 'partner', value: 9.3e6, display: '9,300 kt' },
      { id: 'AUS', label: 'Australia', group: 'core', value: 7e6, display: '7,000 kt' },
      { id: 'ARG', label: 'Argentina', group: 'rest', value: 4e6, display: '4,000 kt' },
      { id: 'USA', label: 'United States', group: 'core', value: 1.8e5, display: '180 kt' },
      { id: 'PRT', label: 'Portugal', group: 'core', value: 60, display: '60 t' },
    ];
    const { container } = render(<LeagueBars rows={rows} axisLabel="tonnes" ariaLabel="Reserves (log)" log />);
    const svg = container.querySelector('svg[aria-label="Reserves (log)"]') as SVGSVGElement;
    // one filled bar per row (the fill group carries the group colours)
    // the bar mark's shapes (rounded data ends make them paths): one per row, each with a finite geometry
    const shapes = [...svg.querySelectorAll('rect, path')].filter((r) => {
      const f = r.getAttribute('fill');
      return !!f && f !== 'none' && f !== 'currentColor' && !(r.getAttribute('d') ?? '').includes('NaN');
    });
    expect(shapes.length).toBeGreaterThanOrEqual(rows.length);
    expect(svg.innerHTML).not.toMatch(/NaN|Infinity/);
    const ticks = logTicks(10, 9.3e6, 4);
    expect(ticks.every((t) => Number.isInteger(Math.log10(t)))).toBe(true);
    expect(ticks.length).toBeLessThanOrEqual(4);
    expect(logTicks(3, 80, 6)).toEqual([5, 10, 20, 50]);
    expect(compactSi(1e10)).toBe('10bn');
  });

  it('diverging bars name their poles', () => {
    const { container } = render(
      <LeagueBars rows={[league[0], { ...league[1], value: -3e9, display: '−USD 3.0 bn' }]} axisLabel="Net trade, US dollars" ariaLabel="Net trade" diverging={{ negLabel: 'net importer ←', posLabel: '→ net exporter' }} />,
    );
    expect(container.textContent).toMatch(/net importer ←.*Net trade, US dollars.*→ net exporter/);
  });
});

describe('time and comparison charts', () => {
  it('GroupArea stacks by group with the hatched residual and a hollow estimate', () => {
    const series = [2023, 2024, 2025].flatMap((x) => [
      { x, group: 'china' as const, value: 10 + x - 2023 },
      { x, group: 'core' as const, value: 20 },
      { x, group: 'unattributed' as const, value: 5, est: x === 2025 },
    ]);
    const { container } = render(<GroupArea series={series} yLabel="USD bn (2024$)" ariaLabel="Investment by group" valueFormat={(v) => `USD ${v} bn`} />);
    const svg = container.querySelector('svg[aria-label="Investment by group"]') as SVGSVGElement;
    expect(svg.innerHTML).toContain('url(#sector-hatch)');
    expect(svg.innerHTML).toContain('#c96f7a');
    expect([...svg.querySelectorAll('text')].map((t) => t.textContent)).toContain('China');
    expect(svg.querySelector('circle')).toBeTruthy();
  });

  it('LabelledLines keeps every end label (nudged apart) and tells same-group lines apart by dash', () => {
    expect(spreadLabels([100, 102, 104], 13, 200)).toEqual([100, 113, 126]);
    // pushed past the bottom: moved back up, still 13 px apart
    expect(spreadLabels([195, 196], 13, 200)).toEqual([187, 200]);
    const dashes = lineDashes([
      { id: 'RUS', label: 'Russia', group: 'rest', points: [] },
      { id: 'IRN', label: 'Iran', group: 'rest', points: [] },
      { id: 'USA', label: 'United States', group: 'core', points: [] },
      { id: 'IRQ', label: 'Iraq', group: 'rest', points: [] },
    ]);
    expect(dashes.get('RUS')).toBeNull();
    expect(dashes.get('USA')).toBeNull();
    expect(new Set([dashes.get('RUS'), dashes.get('IRN'), dashes.get('IRQ')]).size).toBe(3);
    const lines = ['RUS', 'IRN', 'IRQ'].map((id, i) => ({ id, label: id, group: 'rest' as const, points: [2020, 2021].map((x) => ({ x, y: 100 + i })) }));
    const { container } = render(<LabelledLines lines={lines} yLabel="kb/d" ariaLabel="Close lines" valueFormat={(v) => `${v}`} />);
    const svg = container.querySelector('svg[aria-label="Close lines"]') as SVGSVGElement;
    const labels = [...svg.querySelectorAll('text')].map((t) => t.textContent);
    for (const id of ['RUS', 'IRN', 'IRQ']) expect(labels).toContain(id);
  });

  it('LabelledLines: a legend for two or more lines, the highlight halo, none for one line', () => {
    const lines = [
      { id: 'CHN', label: 'China', group: 'china' as const, points: [{ x: 2020, y: 1 }, { x: 2025, y: 3 }] },
      { id: 'USA', label: 'United States', group: 'core' as const, points: [{ x: 2020, y: 2 }, { x: 2025, y: 2.5 }, { x: 2030, y: 2.8, projection: true }] },
      { id: 'ROW', label: 'Rest of world', group: 'neutral' as const, points: [{ x: 2020, y: 4 }, { x: 2025, y: 4.4 }] },
    ];
    const { container } = render(<LabelledLines lines={lines} highlight="USA" yLabel="EJ" ariaLabel="Production" valueFormat={(v) => `${v} EJ`} />);
    expect(screen.getByRole('list', { name: 'Lines' })).toBeTruthy();
    const svg = container.querySelector('svg[aria-label="Production"]') as SVGSVGElement;
    expect(svg.innerHTML).toContain(SIGNAL);
    expect(svg.innerHTML).toContain('stroke-dasharray="4 3"');
    cleanup();
    render(<LabelledLines lines={[lines[0]]} yLabel="EJ" ariaLabel="One line" valueFormat={(v) => `${v}`} />);
    expect(screen.queryByRole('list', { name: 'Lines' })).toBeNull();
  });

  it('Dumbbell: hollow first, filled second, the legend, a missing value left to the text', () => {
    const { container } = render(
      <Dumbbell
        rows={[
          { id: 'CHN', label: 'China', a: 0.8, b: 0.7, group: 'china', aLabel: '80%', bLabel: '70%' },
          { id: 'USA', label: 'United States', a: NaN, b: 0.1, group: 'core', aLabel: 'no 5 GW tranche', bLabel: '10%' },
        ]}
        xLabel="Share"
        aName="2025"
        bName="2040 STEPS (projection)"
        ariaLabel="Shares 2025 and 2040"
        selected="USA"
      />,
    );
    expect(screen.getByRole('list', { name: 'Legend' }).textContent).toMatch(/2025.*2040 STEPS \(projection\)/);
    const svg = container.querySelector('svg[aria-label="Shares 2025 and 2040"]') as SVGSVGElement;
    expect([...svg.querySelectorAll('text')].map((t) => t.textContent)).toContain('no 5 GW tranche → 10%');
  });

  it('Beeswarm: hollow rest, the zero strip, the ring on the highlight', () => {
    const { container } = render(
      <Beeswarm
        points={[
          { id: 'CHN', label: 'China', group: 'china', value: 100, display: '100', labelled: true },
          { id: 'USA', label: 'United States', group: 'core', value: 20, display: '20' },
          { id: 'ZAF', label: 'South Africa', group: 'rest', value: 5, display: '5', state: 'old' },
          { id: 'PER', label: 'Peru', group: 'partner', value: 0, display: '0', state: 'zero' },
        ]}
        medians={[{ label: 'World median', value: 12 }]}
        xLabel="kt"
        ariaLabel="Distribution"
        selected="USA"
      />,
    );
    expect(screen.getByTestId('zero-strip').textContent).toMatch(/Reported zero \(1\): Peru/);
    const svg = container.querySelector('svg[aria-label="Distribution"]') as SVGSVGElement;
    const texts = [...svg.querySelectorAll('text')].map((t) => t.textContent);
    expect(texts).toContain('China');
    expect(texts).toContain('United States');
    expect(texts).toContain('World median');
    expect(svg.innerHTML).toContain(SIGNAL);
  });

  it('SmallMultiples: a shared domain, and quiet tiles for empty panels', () => {
    const panels = [
      { id: 'a', title: 'Financial incentive', rows: [{ x: 2020, y: 3 }, { x: 2021, y: 9 }] },
      { id: 'b', title: 'Regulation', rows: [{ x: 2020, y: 1 }] },
      { id: 'c', title: 'R&D', rows: [], empty: 'No records' },
    ];
    expect(sharedDomain(panels, (r) => r.y)).toEqual([0, 9]);
    render(
      <SmallMultiples
        panels={panels}
        shared
        yValue={(r) => r.y}
        render={(p, ctx) => ({ y: { domain: ctx.yDomain ?? undefined }, marks: [Plot.barY(p.rows, { x: 'x', y: 'y' })] })}
        ariaLabel={(p) => `${p.title} by year`}
      />,
    );
    expect(screen.getByRole('figure', { name: 'R&D by year' }).textContent).toMatch(/No records/);
    expect(screen.getByRole('figure', { name: 'Financial incentive by year' }).querySelector('svg')).toBeTruthy();
  });
});

describe('grids, tables, lists, legends', () => {
  it('HeatTable: hatched no-data cells, printed values, buttons, sub-rows', () => {
    const onCell = vi.fn();
    render(
      <HeatTable
        caption="Concentration board"
        rowHeader="Technology"
        rows={[{ key: 'batteries', label: 'Batteries', children: [{ key: 'lithium', label: 'Lithium' }] }]}
        cols={[
          { key: 'asm', label: 'Exports: Assembly', stage: 'assembly' },
          { key: 'res', label: 'Reserves', stage: 'extraction' },
        ]}
        cell={(r, c) => (c === 'res' ? { value: null, display: '' , title: `${r}: no reserves data` } : { value: 0.437, display: 'CHN 44%', sub: '0.207', selected: r === 'batteries' })}
        onCell={onCell}
      />,
    );
    const table = screen.getByRole('table', { name: 'Concentration board' });
    expect(within(table).getByText('CHN 44%')).toBeTruthy();
    const empty = within(table).getByRole('button', { name: 'batteries: no reserves data' });
    expect((empty.closest('td') as HTMLElement).dataset.hatch).toBe('');
    fireEvent.click(within(table).getAllByRole('button', { name: /Batteries, Exports: Assembly/ })[0]);
    expect(onCell).toHaveBeenCalledWith('batteries', 'asm');
    // the column stripes are neutral (no stage colour, design S4)
    for (const s of table.querySelectorAll<HTMLElement>('thead span[aria-hidden="true"]')) {
      expect(s.className).toContain('bg-ink-500');
      expect(s.style.background).toBe('');
    }
    expect(within(table).queryByText('Lithium')).toBeNull();
    fireEvent.click(within(table).getByRole('button', { name: 'Show Batteries sub-rows' }));
    expect(within(table).getByText('Lithium')).toBeTruthy();
  });

  it('CellGrid: arrow keys move between tiles; an accordion on phones', () => {
    const techs = [
      { slug: 'batteries', label: 'Batteries', headline: true },
      { slug: 'coal', label: 'Coal', headline: true },
      { slug: 'semiconductors', label: 'Semiconductors', headline: false },
    ];
    const stages = [
      { slug: 'extraction' as const, label: 'Extraction' },
      { slug: 'refining' as const, label: 'Refining' },
      { slug: 'components' as const, label: 'Components' },
      { slug: 'assembly' as const, label: 'Assembly' },
      { slug: 'deployment' as const, label: 'Deployment' },
      { slug: 'recovery' as const, label: 'Recovery' },
    ];
    // Semiconductors: only the stages its basket has (Refining, Components, Assembly)
    const tile = (t: { slug: string; label: string }, s: string) => (t.slug === 'semiconductors' && !['refining', 'components', 'assembly'].includes(s) ? null : <button type="button">{`${t.label} ${s}`}</button>);
    render(<CellGrid techs={techs} stages={stages} renderTile={tile} ariaLabel="All cells" />);
    const grid = screen.getByRole('group', { name: 'All cells' });
    expect(grid.dataset.cols).toBe('6');
    const first = screen.getByRole('button', { name: 'Batteries extraction' });
    first.focus();
    fireEvent.keyDown(first, { key: 'ArrowRight' });
    expect(document.activeElement?.textContent).toBe('Batteries refining');
    fireEvent.keyDown(document.activeElement as Element, { key: 'ArrowDown' });
    expect(document.activeElement?.textContent).toBe('Coal refining');
    fireEvent.keyDown(document.activeElement as Element, { key: 'ArrowDown' });
    expect(document.activeElement?.textContent).toBe('Semiconductors refining');
    fireEvent.keyDown(document.activeElement as Element, { key: 'ArrowLeft' });
    expect(document.activeElement?.textContent).toBe('Semiconductors refining');
    fireEvent.keyDown(document.activeElement as Element, { key: 'End' });
    expect(document.activeElement?.textContent).toBe('Semiconductors assembly');
    expect(screen.getByText('Non-headline')).toBeTruthy();
    // the column heads: the six labels over a neutral stripe each (design S4)
    for (const s of stages) expect(within(grid).getByText(s.label)).toBeTruthy();
    for (const s of grid.querySelectorAll<HTMLElement>('span[aria-hidden="true"].block')) expect(s.style.background).toBe('');
    cleanup();
    setWidth(390);
    const { container } = render(<CellGrid techs={techs} stages={stages} renderTile={tile} ariaLabel="All cells" />);
    const details = container.querySelectorAll('details');
    expect(details).toHaveLength(3);
    expect(details[0].open).toBe(true);
    expect(details[1].open).toBe(false);
  });

  it('TopFlowsList, legends, chips', () => {
    inRouter(
      <>
        <TopFlowsList flows={[{ id: '1', from: 'China', to: 'United States', fromGroup: 'china', value: 'USD 21.4 bn', share: '10.5% of world trade', flag: 'importer reported' }]} ariaLabel="Top flows" />
        <GroupLegend highlight="United States" projection />
        <StateLegend states={['zero', 'nq', 'obs']} source="USGS" />
        <CoverageChips
          chips={[
            { family: 'exports', label: 'Trade', status: 'country', text: 'Comtrade 2024', href: '#trade' },
            { family: 'reserves', label: 'Reserves', status: 'none', text: 'not applicable', reason: 'No reserves for a manufacturing stage.' },
          ]}
        />
      </>,
    );
    expect(screen.getByRole('list', { name: 'Top flows' }).textContent).toMatch(/China → United States.*USD 21\.4 bn · 10\.5% of world trade · importer reported/);
    const groups = screen.getByRole('list', { name: 'Groups' });
    expect(within(groups).getByText('China')).toBeTruthy();
    expect(within(groups).getByText('Not attributed')).toBeTruthy();
    expect(within(groups).getByText('United States (highlighted)')).toBeTruthy();
    const states = screen.getByRole('list', { name: 'Value states' });
    expect(within(states).getAllByRole('listitem').map((l) => l.textContent)).toEqual(['Observed', 'Reported zero', 'Listed without a figure (withheld, or under 1% / not reported)']);
    expect(screen.getByRole('link', { name: /Trade · Comtrade 2024/ }).getAttribute('href')).toBe('#trade');
    expect(screen.getByText('No reserves for a manufacturing stage.')).toBeTruthy();
  });
});

describe('SectorChartCard', () => {
  it('requires vars, renders the resolved caption and badge, two caveats inline and the rest in Data notes', () => {
    const resolve = () => ({
      sub: 'Exports of the HS6 basket · US dollars, current · 2024 · UN Comtrade, reconciled matrix · 198 economies',
      badge: '2024',
      caveats: ['Technology-attributed.', 'Nominal USD.', 'HKG is a re-export hub.', 'Mirror ×1.06.'],
      coverage: '143 economies declared their 2024 imports.',
    });
    inRouter(
      <CaptionProvider resolve={resolve}>
        <PageNotesProvider>
          <SectorBar highlight={{ value: null, options: [], onChange: () => undefined }} eu={{ checked: false, onChange: () => undefined }} status="Sector data: export 10 Oct 2026" notes={['Scenario dials apply to Strategic Insights only.']} />
          <SvgDefs />
          <SectorChartCard vars={[{ var: 'trade.exports', item: 'batteries-assembly', year: 2024 }]} title="Top exporters" table={() => <table aria-label="twin" />} summary="China leads with 43.7 percent">
            <div>chart</div>
          </SectorChartCard>
        </PageNotesProvider>
      </CaptionProvider>,
    );
    expect(screen.getByRole('heading', { level: 3, name: 'Top exporters' })).toBeTruthy();
    expect(screen.getByText(/Exports of the HS6 basket · US dollars/)).toBeTruthy();
    expect(screen.getByText('143 economies declared their 2024 imports.')).toBeTruthy();
    expect(screen.getByText('2024', { selector: '.tag' })).toBeTruthy();
    expect(screen.getByText('Technology-attributed.')).toBeTruthy();
    expect(screen.getByText('Nominal USD.')).toBeTruthy();
    // overflow caveats in the SectorBar's Data notes, with the bar's own note
    expect(screen.getByText('Data notes (3)')).toBeTruthy();
    expect(screen.getByText('HKG is a re-export hub.', { selector: 'li' })).toBeTruthy();
    expect(screen.getByRole('status').textContent).toMatch(/Sector data: export 10 Oct 2026/);
    // the table twin
    fireEvent.click(screen.getByRole('button', { name: 'Table' }));
    expect(screen.getByRole('table', { name: 'twin' })).toBeTruthy();
    expect(screen.getByTestId('sector-svg-defs').querySelector('pattern#sector-hatch')).toBeTruthy();
  });

  it('without a resolver the var ids show (a missing caption is visible)', () => {
    inRouter(
      <SectorChartCard vars={[{ var: 'production.refining', year: 2025 }]} title="Refining">
        <div />
      </SectorChartCard>,
    );
    expect(screen.getByText('production.refining')).toBeTruthy();
  });
});

describe('chrome', () => {
  it('the phone SectorBar opens a dialog that traps focus and closes on Escape', () => {
    setWidth(390);
    const onChange = vi.fn();
    inRouter(
      <SectorBar
        summary="Batteries · Refining"
        picker={{
          tech: 'batteries',
          stage: 'refining',
          techOptions: [{ value: 'batteries', label: 'Batteries' }],
          stages: [
            { slug: 'extraction', label: 'Extraction', short: 'Ext', coverage: 'some' },
            { slug: 'refining', label: 'Refining', short: 'Ref', coverage: 'rich' },
            { slug: 'components', label: 'Components', short: 'Com', coverage: 'thin' },
            { slug: 'assembly', label: 'Assembly', short: 'Asm', coverage: 'rich' },
            { slug: 'deployment', label: 'Deployment', short: 'Dep', coverage: 'some' },
            { slug: 'recovery', label: 'Recovery', short: 'Rec', coverage: null, disabled: true, title: 'Batteries has no Recovery cell' },
          ],
          onTech: () => undefined,
          onStage: () => undefined,
        }}
        highlight={{ value: 'USA', options: [{ iso3c: 'USA', country: 'United States', group: 'Coalition core' }], onChange }}
        eu={{ checked: false, onChange: () => undefined, disabled: true, reason: 'Nothing additive on this page' }}
        status="Ready"
        notes={['A note.']}
      />,
    );
    const open = screen.getByRole('button', { name: /Batteries · Refining/ });
    fireEvent.click(screen.getByRole('button', { name: 'Clear the highlight (United States)' }));
    expect(onChange).toHaveBeenCalledWith(null);
    open.focus();
    fireEvent.click(open);
    const dialog = screen.getByRole('dialog', { name: 'Batteries · Refining: filters' });
    expect(within(dialog).getByRole('radio', { name: /Refining, rich coverage/ })).toBeTruthy();
    // six stages; the shorts for narrow widths, the full name for the screen reader and the title
    expect(within(dialog).getAllByRole('radio').map((r) => (r as HTMLInputElement).value)).toEqual(['extraction', 'refining', 'components', 'assembly', 'deployment', 'recovery']);
    expect(within(dialog).getByText('Ref')).toBeTruthy();
    expect((within(dialog).getByRole('radio', { name: 'Recovery' }) as HTMLInputElement).disabled).toBe(true);
    expect(within(dialog).getByText('A note.')).toBeTruthy();
    expect((within(dialog).getByRole('switch') as HTMLInputElement).disabled).toBe(true);
    expect(within(dialog).getByText('Nothing additive on this page')).toBeTruthy();
    // focus is inside; Tab from the last control wraps to the first
    expect(dialog.contains(document.activeElement)).toBe(true);
    const done = within(dialog).getByRole('button', { name: 'Done' });
    const focusables = [...dialog.querySelectorAll<HTMLElement>('a[href], button:not([disabled]), input:not([disabled]), select:not([disabled])')];
    focusables[focusables.length - 1].focus();
    fireEvent.keyDown(document, { key: 'Tab' });
    expect(document.activeElement).toBe(done);
    fireEvent.keyDown(document, { key: 'Escape' });
    expect(screen.queryByRole('dialog')).toBeNull();
    expect(document.activeElement).toBe(open);
  });

  it('SectionRail: a rail, chips or a select by width', () => {
    const sections = [
      { id: 'snapshot', label: 'Snapshot' },
      { id: 'trade', label: 'Trade' },
    ];
    render(<SectionRail sections={sections} current="trade" />);
    const nav = screen.getByRole('navigation', { name: 'Sections' });
    expect(within(nav).getByRole('link', { name: 'Trade' }).getAttribute('aria-current')).toBe('location');
    cleanup();
    setWidth(390);
    render(<SectionRail sections={sections} />);
    expect(screen.getByRole('combobox', { name: 'Sections' }).tagName).toBe('SELECT');
  });

  it('ThemeChips mark the active theme', () => {
    inRouter(<ThemeChips themes={[{ slug: 'trade', label: 'Trade', href: '/sector/explore/trade' }, { slug: 'policy', label: 'Policy', href: '/sector/explore/policy' }]} active="policy" />);
    const nav = screen.getByRole('navigation', { name: 'Themes' });
    expect(within(nav).getByRole('link', { name: 'Policy' }).getAttribute('aria-current')).toBe('page');
    expect(within(nav).getByRole('link', { name: 'Trade' }).getAttribute('aria-current')).toBeNull();
  });
});

describe('three-colour scale in the kit (spec_country.md C1.4, C1.5)', () => {
  const rgb = (hex: string) => {
    const n = parseInt(hex.slice(1), 16);
    return `rgb(${(n >> 16) & 255}, ${(n >> 8) & 255}, ${n & 255})`;
  };
  const cellOf = (table: HTMLElement, text: string) => within(table).getByText(text).closest('td') as HTMLElement;

  it('HeatTable: by default orange at the lowest cell, white at the median, blue at the highest; readable text', () => {
    const values: Record<string, number> = { a: 0.1, b: 0.2, c: 0.3, d: 0.6, e: 0.9 };
    render(
      <HeatTable
        caption="Tri table"
        rowHeader="Row"
        rows={Object.keys(values).map((k) => ({ key: k, label: k.toUpperCase() }))}
        cols={[{ key: 'v', label: 'Value', stage: 'assembly' }]}
        cell={(r) => ({ value: values[r], display: `v${values[r]}`, selected: r === 'a' })}
      />,
    );
    const table = screen.getByRole('table', { name: 'Tri table' });
    expect(cellOf(table, 'v0.1').style.background).toBe(rgb(TRI_LOW));
    expect(cellOf(table, 'v0.3').style.background).toBe(rgb(TRI_MID));
    expect(cellOf(table, 'v0.9').style.background).toBe(rgb(TRI_HIGH));
    // text: never white on the orange or the white end
    expect(cellOf(table, 'v0.1').style.color).not.toBe('rgb(255, 255, 255)');
    expect(cellOf(table, 'v0.3').style.color).not.toBe('rgb(255, 255, 255)');
    // the selected cell: white edge over ink (an orange outline would vanish on the low end)
    const sel = cellOf(table, 'v0.1');
    const either = (css: string, hex: string) => css.includes(hex) || css.includes(rgb(hex));
    expect(either(sel.style.outline, RING_INNER)).toBe(true);
    expect(either(sel.style.boxShadow, RING_OUTER)).toBe(true);
  });

  it("HeatTable: the caller's scale wins (signed values centred on zero; reverse for ranks)", () => {
    const vals: Record<string, number> = { a: -4, b: 0, c: 2 };
    const s = triScale(Object.values(vals), { center: 'zero' });
    render(
      <HeatTable
        caption="Signed table"
        rowHeader="Row"
        rows={Object.keys(vals).map((k) => ({ key: k, label: k }))}
        cols={[{ key: 'v', label: 'Change', stage: 'extraction' }]}
        cell={(r) => ({ value: vals[r], display: `c${vals[r]}` })}
        scale={s}
      />,
    );
    const table = screen.getByRole('table', { name: 'Signed table' });
    expect(cellOf(table, 'c-4').style.background).toBe(rgb(TRI[0]));
    expect(cellOf(table, 'c0').style.background).toBe(rgb(TRI_MID));
    expect(cellOf(table, 'c2').style.background).toBe(rgb(TRI[8]));
    cleanup();
    render(
      <HeatTable
        caption="Rank table"
        rows={[{ key: 'x', label: 'x' }, { key: 'y', label: 'y' }, { key: 'z', label: 'z' }]}
        cols={[{ key: 'r', label: 'Rank', stage: 'extraction' }]}
        cell={(r) => ({ value: { x: 1, y: 2, z: 3 }[r] as number, display: `r${r}` })}
        reverse
      />,
    );
    const ranks = screen.getByRole('table', { name: 'Rank table' });
    expect(cellOf(ranks, 'rx').style.background).toBe(rgb(TRI_HIGH));
    expect(cellOf(ranks, 'rz').style.background).toBe(rgb(TRI_LOW));
  });

  it('LeagueBars: signed bars in the scale ends, the selected bar ringed ink under white', () => {
    const rows = [
      { id: 'USA', label: 'United States', group: 'core' as const, value: 5e9, display: 'USD 5.0 bn' },
      { id: 'JPN', label: 'Japan', group: 'core' as const, value: -3e9, display: '−USD 3.0 bn' },
    ];
    const { container } = render(<LeagueBars rows={rows} selected="JPN" axisLabel="Net trade, US dollars" ariaLabel="Signed league" diverging={{ negLabel: 'net importer ←', posLabel: '→ net exporter' }} />);
    const svg = container.querySelector('svg[aria-label="Signed league"]') as SVGSVGElement;
    // (bars with rounded data ends are paths; a constant stroke sits on the mark's group)
    const html = svg.innerHTML;
    expect(html).toContain(`fill="${TRI_HIGH}"`);
    expect(html).toContain(`fill="${TRI_LOW}"`);
    expect(html).toContain(`stroke="${RING_OUTER}"`);
    expect(html).toContain(`stroke="${RING_INNER}"`);
    expect(html).not.toContain(`stroke="${SIGNAL}"`);
  });

  it('SmallMultiples: columns = 1 gives full-width panels', () => {
    render(
      <SmallMultiples
        panels={[
          { id: 'a', title: 'Brent', rows: [{ x: 1, y: 2 }] },
          { id: 'b', title: 'Henry Hub', rows: [{ x: 1, y: 3 }] },
        ]}
        shared={false}
        columns={1}
        yValue={(r) => r.y}
        render={(p) => ({ marks: [Plot.lineY(p.rows, { x: 'x', y: 'y' })] })}
        ariaLabel={(p) => `${p.title} monthly`}
      />,
    );
    expect(screen.getByTestId('small-multiples').style.gridTemplateColumns).toBe('repeat(1, minmax(0, 1fr))');
  });
});
