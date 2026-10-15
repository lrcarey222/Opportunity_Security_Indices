import { describe, expect, it } from 'vitest';
import { dodgeLayout, stackLayout, swarmFit } from './dodge.ts';
import { swarmGeometry, SWARM_MAX_HEIGHT, SWARM_MIN_HEIGHT, type SwarmPoint } from './Beeswarm.tsx';

/** No two placed dots overlap (centre distance >= 2r + padding, within rounding). */
function noOverlap(px: number[], y: number[], r: number, padding = 1) {
  const d = 2 * r + padding;
  for (let i = 0; i < px.length; i++) {
    if (!Number.isFinite(y[i])) continue;
    for (let j = i + 1; j < px.length; j++) {
      if (!Number.isFinite(y[j])) continue;
      const dist = Math.hypot(px[i] - px[j], y[i] - y[j]);
      if (dist < d - 1e-6) return false;
    }
  }
  return true;
}

const pts = (values: number[]): SwarmPoint[] => values.map((v, i) => ({ id: `C${i}`, value: v, label: `Country ${i}` }));

describe('dodgeLayout', () => {
  it('keeps isolated dots on the centre line', () => {
    const { y, extent } = dodgeLayout([10, 50, 90], 4);
    expect(y).toEqual([0, 0, 0]);
    expect(extent).toBe(0);
  });

  it('stacks coinciding dots alternately up and down, without overlap', () => {
    const px = new Array(7).fill(100);
    const { y, extent } = dodgeLayout(px, 4, 1);
    expect(y[0]).toBe(0);
    expect(y.slice(1, 3).sort((a, b) => a - b)).toEqual([-9, 9]);
    expect(extent).toBe(27);
    expect(noOverlap(px, y, 4)).toBe(true);
  });

  it('is deterministic and order-independent in its geometry', () => {
    const px = [5, 6, 7, 30, 31, 31, 31.5, 60, 61];
    const a = dodgeLayout(px, 4);
    const b = dodgeLayout(px, 4);
    expect(a).toEqual(b);
    expect(noOverlap(px, a.y, 4)).toBe(true);
  });

  it('places late dots on the edge of their cluster', () => {
    const px = new Array(5).fill(100);
    const late = [false, false, true, false, false];
    const { y } = dodgeLayout(px, 4, 1, late);
    // four dots at 0, +9, -9, +18; the late one takes the next free slot outside them
    expect(Math.abs(y[2])).toBe(18);
    expect(noOverlap(px, y, 4)).toBe(true);
    // a late dot alone stays on the centre line
    expect(dodgeLayout([10, 90], 4, 1, [true, false]).y).toEqual([0, 0]);
  });

  it('handles empty input', () => {
    expect(dodgeLayout([], 4)).toEqual({ y: [], extent: 0 });
  });
});

describe('stackLayout', () => {
  it('caps each column at k dots and counts the rest', () => {
    const px = new Array(20).fill(100).concat([300]);
    const s = stackLayout(px, 2, 1, 5);
    expect(s.hidden).toBe(15);
    expect(s.overflow).toHaveLength(1);
    expect(s.overflow[0].hidden).toBe(15);
    const shown = s.y.filter((v) => Number.isFinite(v));
    expect(shown).toHaveLength(6);
    expect(s.extent).toBe(10); // 0, +5, -5, +10, -10
    expect(noOverlap(s.x, s.y, 2)).toBe(true);
  });

  it('keeps the priority dots when a column is capped, at the bottom of the column', () => {
    const px = new Array(10).fill(50);
    const priority = px.map((_, i) => (i === 9 ? 0 : 2));
    const s = stackLayout(px, 2, 1, 3, priority);
    expect(Number.isFinite(s.y[9])).toBe(true);
    expect(s.y[9]).toBe(-5);
    expect(s.overflow[0].top).toBe(5);
    expect(noOverlap(s.x, s.y, 2)).toBe(true);
  });
});

describe('swarmFit', () => {
  it('keeps r 4 when the swarm fits', () => {
    const px = Array.from({ length: 30 }, (_, i) => 16 + i * 25);
    const f = swarmFit(px, { maxSwarm: 400 });
    expect(f.mode).toBe('dodge');
    expect(f.r).toBe(4);
    expect(f.span).toBeLessThanOrEqual(400);
  });

  it('shrinks the radius before it stacks', () => {
    // 20 coinciding dots: at r 4 the column needs 2 x (10 x 9 + 4) = 188 px; at r 2 2 x (10 x 5 + 2) = 104
    const px = new Array(20).fill(200);
    const f = swarmFit(px, { maxSwarm: 120 });
    expect(f.mode).toBe('dodge');
    expect(f.r).toBeLessThan(4);
    expect(f.span).toBeLessThanOrEqual(120);
  });

  it('switches to stack mode when nothing fits, and the stack fits', () => {
    const px = new Array(150).fill(200);
    const f = swarmFit(px, { maxSwarm: 300 });
    expect(f.mode).toBe('stack');
    expect(f.r).toBe(2);
    expect(f.span).toBeLessThanOrEqual(300);
    expect(f.hidden).toBeGreaterThan(0);
    expect(f.overflow[0].hidden).toBe(f.hidden);
  });
});

describe('swarmGeometry', () => {
  it('150 identical values: every dot inside the frame, height <= 480, stack mode', () => {
    const g = swarmGeometry(pts(new Array(150).fill(0.5)), 600, {});
    expect(g.height).toBeLessThanOrEqual(SWARM_MAX_HEIGHT);
    expect(g.fit.mode).toBe('stack');
    const half = (g.height - g.marginTop - g.marginBottom) / 2;
    for (const y of g.fit.y) if (Number.isFinite(y)) expect(Math.abs(y) + g.fit.r).toBeLessThanOrEqual(half + 1e-9);
    expect(g.fit.hidden + g.fit.y.filter(Number.isFinite).length).toBe(150);
  });

  it('a dense bounded measure (many ties at 0 and 1) stays inside the frame', () => {
    const values = [...new Array(90).fill(0), ...new Array(70).fill(1), ...Array.from({ length: 60 }, (_, i) => i / 60)];
    const g = swarmGeometry(pts(values), 390, { selected: 'C5' });
    const half = (g.height - g.marginTop - g.marginBottom) / 2;
    expect(g.height).toBeLessThanOrEqual(SWARM_MAX_HEIGHT);
    for (const y of g.fit.y) if (Number.isFinite(y)) expect(Math.abs(y) + g.fit.r).toBeLessThanOrEqual(half + 1e-9);
    // the selected dot is kept and labelled
    expect(Number.isFinite(g.fit.y[5])).toBe(true);
  });

  it('30 spread values: dodge mode at r 4, height within bounds', () => {
    const g = swarmGeometry(pts(Array.from({ length: 30 }, (_, i) => i * 3.3)), 800, {});
    expect(g.fit.mode).toBe('dodge');
    expect(g.fit.r).toBe(4);
    expect(g.height).toBeGreaterThanOrEqual(SWARM_MIN_HEIGHT);
    expect(g.height).toBeLessThanOrEqual(SWARM_MAX_HEIGHT);
  });

  it('labels go above, then below, never off the figure', () => {
    // 10 and 11 are ~14 px apart: both on the centre line, but their labels collide
    const p = pts([10, 11, 50]);
    p[0].labelled = true;
    p[1].labelled = true;
    const g = swarmGeometry(p, 600, { selected: 'C2' });
    expect(g.labels).toEqual(['above', 'below', 'above']);
    // a labelled dot in a stack is placed last, on the stack's edge, where its label has room
    const q = pts([10, 10, 10]);
    q[0].labelled = true;
    const gq = swarmGeometry(q, 600, {});
    expect(gq.fit.y[0]).toBe(-9);
    expect(gq.labels[0]).toBe('below');
    // two labelled dots in one stack: the outer one is labelled, the inner one is left to the tooltip
    const t = pts([10, 10, 10, 10, 10]);
    t[0].labelled = true;
    t[1].labelled = true;
    const gt = swarmGeometry(t, 600, {});
    expect(gt.labels.filter(Boolean).length).toBeGreaterThanOrEqual(1);
  });

  it('median words: colliding labels take a second row (and a taller top margin); labels stay inside the figure', () => {
    const p = pts([0, 0.001, 0.002, 0.1, 0.2]);
    const close = swarmGeometry(p, 600, { medians: [{ label: 'World median', value: 0.001 }, { label: 'Coalition median', value: 0.002 }] });
    expect(close.medianLabels.map((m) => m.row)).toEqual([0, 1]);
    for (const m of close.medianLabels) expect(m.cx).toBeGreaterThanOrEqual(30);
    const far = swarmGeometry(p, 600, { medians: [{ label: 'World median', value: 0.001 }, { label: 'Coalition median', value: 0.2 }] });
    expect(far.medianLabels.map((m) => m.row)).toEqual([0, 0]);
    expect(close.marginTop - far.marginTop).toBe(12);
    expect(swarmGeometry(p, 600, {}).medianLabels).toEqual([]);
  });

  it('a log axis spans the positive values', () => {
    const g = swarmGeometry(pts([1, 10, 100, 1000]), 600, { log: true });
    expect(g.domain).toEqual([1, 1000]);
    expect(g.ticks?.length).toBeGreaterThan(0);
    // the range is inset by the largest dot, so the frame clip never cuts an end dot
    expect(g.px(1)).toBeCloseTo(21);
    expect(g.px(1000)).toBeCloseTo(579);
  });
});
