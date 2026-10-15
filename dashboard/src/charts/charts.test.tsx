// Shared chart components (spec_country.md C1.3, C1.4, C2): the generic beeswarm never lets a dot leave
// its frame (user edit 1b), switches to stack mode with its note when too many dots coincide, and the
// Sector wrapper keeps its zero strip; the three-colour legend's words line and aria-label; the double
// ring on three-colour fills (beeswarm and world map).

import { afterAll, afterEach, beforeAll, describe, expect, it } from 'vitest';
import { cleanup, render, screen, waitFor } from '@testing-library/react';
import { BeeswarmChart, SWARM_MAX_HEIGHT, type SwarmPoint } from './Beeswarm.tsx';
import { TriLegend, triWords } from './legends.tsx';
import { WorldMap, type MapCell } from './WorldMap.tsx';
import { triScale } from './scale.ts';
import { Beeswarm as SectorBeeswarm } from '../sector/charts/Beeswarm.tsx';
import { RING_INNER, RING_OUTER, SIGNAL, TRI } from '../theme/palette.ts';

let clientW = 640;
const restore: (() => void)[] = [];

beforeAll(() => {
  const cw = Object.getOwnPropertyDescriptor(HTMLElement.prototype, 'clientWidth');
  Object.defineProperty(HTMLElement.prototype, 'clientWidth', { configurable: true, get: () => clientW });
  restore.push(() => cw && Object.defineProperty(HTMLElement.prototype, 'clientWidth', cw));
});
afterEach(() => {
  cleanup();
  clientW = 640;
});
afterAll(() => {
  for (const f of restore) f();
});

const pts = (values: number[]): SwarmPoint[] => values.map((v, i) => ({ id: `C${i}`, value: v, label: `Country ${i}`, fill: TRI[(i * 3) % 9] }));

/** The circles of the dot marks with their centre (cx, cy) and radius, and the figure's frame. */
function geometry(svg: SVGSVGElement, marginTop: number, marginBottom: number) {
  const height = Number(svg.getAttribute('height'));
  const width = Number(svg.getAttribute('width'));
  const circles = [...svg.querySelectorAll('circle')].map((c) => ({
    cx: Number(c.getAttribute('cx')),
    cy: Number(c.getAttribute('cy')),
    r: Number(c.getAttribute('r')),
    fill: c.getAttribute('fill'),
    stroke: c.getAttribute('stroke'),
  }));
  return { height, width, circles, top: marginTop, bottom: height - marginBottom };
}

function figure(container: HTMLElement): { svg: SVGSVGElement; host: HTMLElement } {
  const host = container.querySelector('[data-testid="beeswarm"]') as HTMLElement;
  const svg = host.querySelector('svg[role="img"]') as SVGSVGElement;
  return { svg, host };
}

describe('BeeswarmChart (edit 1b: dots stay inside the chart)', () => {
  it('150 identical values: every dot inside the frame, height <= 480, the stack note and a +m count', () => {
    const { container } = render(<BeeswarmChart points={pts(new Array(150).fill(0.42))} ariaLabel="Dense" xLabel="Score" selected="C7" ringStyle="double" />);
    const { svg, host } = figure(container);
    expect(svg).toBeTruthy();
    expect(host.dataset.mode).toBe('stack');
    const h = Number(host.dataset.height);
    expect(h).toBeLessThanOrEqual(SWARM_MAX_HEIGHT);
    expect(Number(svg.getAttribute('height'))).toBe(h);
    // the frame is the figure minus the margins Plot was given: read it off the x axis (bottom) and
    // the swarm itself; every circle (rings included) lies within the figure
    const g = geometry(svg, 0, 0);
    expect(g.circles.length).toBeGreaterThan(10);
    for (const c of g.circles) {
      expect(c.cy - c.r).toBeGreaterThanOrEqual(0);
      expect(c.cy + c.r).toBeLessThanOrEqual(h - 30); // above the x axis
      expect(c.cx).toBeGreaterThanOrEqual(0);
      expect(c.cx).toBeLessThanOrEqual(640);
    }
    expect(screen.getByTestId('swarm-stack-note').textContent).toMatch(/too close to separate.*table lists every one/);
    const texts = [...svg.querySelectorAll('text')].map((t) => t.textContent ?? '');
    expect(texts.some((t) => /^\+\d+$/.test(t))).toBe(true);
    // the selected economy is kept, labelled and double-ringed (ink under white)
    expect(texts).toContain('Country 7');
    expect(svg.innerHTML).toContain(RING_OUTER);
    expect(svg.innerHTML).toContain(RING_INNER);
  });

  it('a dense bounded measure on a phone: dots shrink or stack, never spill', () => {
    clientW = 358;
    const values = [...new Array(80).fill(0), ...new Array(60).fill(1), ...Array.from({ length: 50 }, (_, i) => i / 50)];
    const { container } = render(<BeeswarmChart points={pts(values)} ariaLabel="Bounded" />);
    const { svg, host } = figure(container);
    const h = Number(host.dataset.height);
    expect(h).toBeLessThanOrEqual(SWARM_MAX_HEIGHT);
    for (const c of geometry(svg, 0, 0).circles) {
      expect(c.cy - c.r).toBeGreaterThanOrEqual(0);
      expect(c.cy + c.r).toBeLessThanOrEqual(h - 30);
    }
  });

  it('30 spread values: no stack mode, no note, every dot drawn at r 4', () => {
    const { container } = render(<BeeswarmChart points={pts(Array.from({ length: 30 }, (_, i) => i * 1.7))} ariaLabel="Spread" selected="C3" />);
    const { svg, host } = figure(container);
    expect(host.dataset.mode).toBe('dodge');
    expect(screen.queryByTestId('swarm-stack-note')).toBeNull();
    const dots = geometry(svg, 0, 0).circles.filter((c) => c.r === 4);
    expect(dots.length).toBe(30);
    // the dots at both ends of the axis lie wholly inside the frame (16 px margins), so the clip cuts none
    expect(Math.min(...dots.map((c) => c.cx - c.r))).toBeGreaterThanOrEqual(16);
    expect(Math.max(...dots.map((c) => c.cx + c.r))).toBeLessThanOrEqual(640 - 16);
    // the default selection ring is orange
    expect(svg.innerHTML).toContain(SIGNAL);
  });

  it('nothing to place: a quiet sentence, no figure', () => {
    render(<BeeswarmChart points={[]} ariaLabel="Empty" noun="partners" />);
    expect(screen.getByText('No partners with a value to place.')).toBeTruthy();
  });
});

describe('Sector Beeswarm wrapper', () => {
  it('a log axis with zeros keeps the zero strip; the swarm holds the positive values', () => {
    const points = [
      { id: 'CHN', label: 'China', group: 'china' as const, value: 1000, display: '1,000', labelled: true },
      { id: 'USA', label: 'United States', group: 'core' as const, value: 20, display: '20' },
      { id: 'JPN', label: 'Japan', group: 'core' as const, value: 0, display: '0' },
      { id: 'PER', label: 'Peru', group: 'partner' as const, value: 0, display: '0', state: 'zero' as const },
    ];
    const { container } = render(<SectorBeeswarm points={points} medians={[{ label: 'World median', value: 20 }]} log xLabel="kt" ariaLabel="Log swarm" selected="USA" />);
    expect(screen.getByTestId('zero-strip').textContent).toMatch(/Reported zero \(2\): Japan, Peru/);
    const svg = container.querySelector('svg[aria-label="Log swarm"]') as SVGSVGElement;
    const texts = [...svg.querySelectorAll('text')].map((t) => t.textContent);
    expect(texts).toContain('China');
    expect(texts).toContain('United States');
    expect(texts).toContain('World median');
  });

  it('150 coinciding economies stay inside the card', () => {
    const points = Array.from({ length: 150 }, (_, i) => ({ id: `E${i}`, label: `Economy ${i}`, group: (i % 2 ? 'core' : 'rest') as 'core' | 'rest', value: 0.5, display: '0.5' }));
    const { container } = render(<SectorBeeswarm points={points} medians={[]} xLabel="share" ariaLabel="Dense sector" zeroStrip={false} />);
    const host = container.querySelector('[data-testid="beeswarm"]') as HTMLElement;
    expect(Number(host.dataset.height)).toBeLessThanOrEqual(SWARM_MAX_HEIGHT);
    expect(host.dataset.mode).toBe('stack');
    expect(screen.getByTestId('swarm-stack-note')).toBeTruthy();
  });
});

describe('TriLegend (C1.3)', () => {
  const fmt = (v: number) => v.toFixed(2);

  it('median centre: nine hard blocks, the words line and the aria-label with both ends', () => {
    const s = triScale([0.1, 0.2, 0.3, 0.5, 0.9]);
    render(<TriLegend scale={s} label="Energy Security, Batteries Midstream" format={fmt} noun="economies" missing="Not ranked" />);
    const el = screen.getByTestId('tri-legend');
    expect(el.getAttribute('role')).toBe('group');
    expect(el.getAttribute('aria-label')).toBe(
      'Energy Security, Batteries Midstream: from 0.10 (orange) to 0.90 (blue). Orange: lower; white: median of the 5 economies shown (0.30); blue: higher.',
    );
    expect(el.textContent).toContain('Orange: lower · White: median of the 5 economies shown (0.30) · Blue: higher');
    expect(el.textContent).toContain('Not ranked');
    // the bar: nine blocks in TRI order
    const blocks = [...el.querySelectorAll('span[aria-hidden="true"] > span.flex-1')] as HTMLElement[];
    expect(blocks.map((b) => b.style.background)).toHaveLength(9);
  });

  it('reverse swaps the end words (rank 1 is best, blue)', () => {
    const s = triScale([1, 2, 3, 4, 5], { reverse: true });
    const w = triWords(s, fmt, 'ranks');
    expect(w.orange).toBe('higher');
    expect(w.blue).toBe('lower');
    render(<TriLegend scale={s} label="Strategic Index rank" format={(v) => String(v)} lowWord="worse rank" highWord="better rank" />);
    expect(screen.getByTestId('tri-legend').getAttribute('aria-label')).toMatch(/^Strategic Index rank: from 5 \(orange\) to 1 \(blue\)\. Orange: better rank; white: median of the 5 values shown \(3\); blue: worse rank\.$/);
  });

  it('zero centre: negative, zero, positive', () => {
    const s = triScale([-2, -1, 0, 1, 3], { center: 'zero' });
    render(<TriLegend scale={s} label="Change" format={fmt} />);
    const el = screen.getByTestId('tri-legend');
    expect(el.textContent).toContain('Orange: negative · White: zero · Blue: positive');
    expect(el.getAttribute('aria-label')).toMatch(/Orange: negative; white: zero; blue: positive\.$/);
  });

  it('a fixed ordinal domain names the middle of the scale', () => {
    const s = triScale([0, 1, 4], { domain: [0, 2, 4] });
    const w = triWords(s, (v) => String(v));
    expect(w.white).toBe('middle of the scale (2)');
  });
});

describe('WorldMap rings (C1.4)', () => {
  const cells = () =>
    new Map<string, MapCell>([
      ['840', { fill: TRI[8], iso3c: 'USA', tip: 'United States' }],
      ['392', { fill: TRI[0], iso3c: 'JPN', tip: 'Japan', outline: true }],
      ['276', { fill: TRI[4], iso3c: 'DEU', tip: 'Germany' }],
    ]);

  it('the default orange outline marks the selected country', async () => {
    const { container } = render(<WorldMap cells={cells()} ariaLabel="Orange map" selected="USA" />);
    await waitFor(() => expect(container.querySelector('svg[aria-label="Orange map"]')).toBeTruthy(), { timeout: 20_000 });
    const sel = container.querySelector('g[data-ring="selected"]') as SVGGElement;
    expect([...sel.querySelectorAll('path')].map((p) => [p.getAttribute('stroke'), p.getAttribute('stroke-width')])).toEqual([[SIGNAL, '1.5']]);
  });

  it('three-colour maps ring the selected and outlined countries ink under white (4 / 2 px), visible on orange', async () => {
    const { container } = render(<WorldMap cells={cells()} ariaLabel="Tri map" selected="USA" ringStyle="double" />);
    await waitFor(() => expect(container.querySelector('svg[aria-label="Tri map"]')).toBeTruthy(), { timeout: 20_000 });
    const layers = (g: string) => [...(container.querySelector(`g[data-ring="${g}"]`) as SVGGElement).querySelectorAll('path')].map((p) => [p.getAttribute('stroke'), p.getAttribute('stroke-width')]);
    expect(layers('selected')).toEqual([
      [RING_OUTER, '4'],
      [RING_INNER, '2'],
    ]);
    // Japan (outlined, orange fill) gets the same double ring
    expect(layers('outline')).toEqual([
      [RING_OUTER, '4'],
      [RING_INNER, '2'],
    ]);
    expect(container.querySelector('g[data-ring="selected"]')?.innerHTML).not.toContain(SIGNAL);
  });
});
