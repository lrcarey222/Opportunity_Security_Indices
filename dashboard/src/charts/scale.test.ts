// The three-colour scale (spec_country.md C1.2, C1.6): the median maps to white exactly; pos is
// monotone; ties share a colour; n = 0, 1, 2; all equal; an empty arm; zero centre with mixed signs;
// reverse; value basis with a fixed domain; ticks are the quantiles; side() words; triText contrast.

import { describe, expect, it } from 'vitest';
import { TRI, TRI_HIGH, TRI_LOW, TRI_MID } from '../theme/palette.ts';
import { contrast, medianSorted, quantileSorted, triColorAt, triScale, triText } from './scale.ts';

describe('triScale: rank basis, centre median', () => {
  const xs = [5, 1, 4, 2, 3];
  const s = triScale(xs);

  it('maps the median to white exactly and the ends to the end stops', () => {
    expect(s.mid).toBe(3);
    expect(s.color(3)).toBe(TRI_MID);
    expect(s.color(1)).toBe(TRI_LOW);
    expect(s.color(5)).toBe(TRI_HIGH);
    expect(s.pos(3)).toBe(0.5);
    expect([1, 2, 3, 4, 5].map((v) => s.pos(v))).toEqual([0, 0.25, 0.5, 0.75, 1]);
    expect(s.step(2)).toBe(2);
  });

  it('spreads clustered values by rank, not by level', () => {
    const c = triScale([0.5, 0.51, 0.52, 0.53, 0.99]);
    expect(c.pos(0.51)).toBe(0.25);
    expect(c.pos(0.53)).toBe(0.75);
  });

  it('is monotone in v, including values outside the set', () => {
    const r = triScale([0.1, 0.4, 0.4, 0.7, 0.9, 1.3, 2.0, 2.0, 5]);
    let prev = -1;
    for (let v = -1; v <= 6; v += 0.01) {
      const p = r.pos(v) as number;
      expect(p).toBeGreaterThanOrEqual(prev - 1e-12);
      expect(p).toBeGreaterThanOrEqual(0);
      expect(p).toBeLessThanOrEqual(1);
      prev = p;
    }
  });

  it('gives ties one colour', () => {
    const t = triScale([1, 2, 2, 2, 3, 9]);
    expect(t.color(2)).toBe(t.color(2.0));
    const ties = [2, 2, 2].map((v) => t.pos(v));
    expect(new Set(ties).size).toBe(1);
  });

  it('even n: the median is the mean of the two middle values and lands on white', () => {
    const e = triScale([1, 2, 3, 4]);
    expect(e.mid).toBe(2.5);
    expect(e.color(2.5)).toBe(TRI_MID);
    expect(e.pos(1)).toBe(0);
    expect(e.pos(4)).toBe(1);
    expect(e.pos(2)).toBeCloseTo(1 / 3, 12);
    expect(e.pos(3)).toBeCloseTo(2 / 3, 12);
  });

  it('a tie group at the median is white and the arm below it is empty', () => {
    const z = triScale([0, 0, 0, 1]);
    expect(z.mid).toBe(0);
    expect(z.color(0)).toBe(TRI_MID);
    expect(z.color(1)).toBe(TRI_HIGH);
  });

  it('n = 0: every call is null, no ticks', () => {
    const n0 = triScale([]);
    expect(n0.n).toBe(0);
    expect(n0.pos(1)).toBeNull();
    expect(n0.color(1)).toBeNull();
    expect(n0.step(1)).toBeNull();
    expect(n0.side(1)).toBeNull();
    expect(n0.ticks).toEqual([]);
  });

  it('n = 1 and all-equal values are white', () => {
    expect(triScale([7]).color(7)).toBe(TRI_MID);
    const eq = triScale([2, 2, 2]);
    expect(eq.color(2)).toBe(TRI_MID);
    expect(eq.ticks.length).toBe(1);
  });

  it('n = 2: the two ends', () => {
    const two = triScale([1, 3]);
    expect(two.mid).toBe(2);
    expect(two.color(1)).toBe(TRI_LOW);
    expect(two.color(3)).toBe(TRI_HIGH);
    expect(two.color(2)).toBe(TRI_MID);
  });

  it('ignores non-finite values and returns null for them', () => {
    const s2 = triScale([1, null, undefined, NaN, Infinity, 2, 3]);
    expect(s2.n).toBe(3);
    expect(s2.color(null)).toBeNull();
    expect(s2.color(NaN)).toBeNull();
  });

  it('reverse puts the low values on the blue end (ranks where 1 is best)', () => {
    const r = triScale([1, 2, 3, 4, 5], { reverse: true });
    expect(r.color(1)).toBe(TRI_HIGH);
    expect(r.color(5)).toBe(TRI_LOW);
    expect(r.color(3)).toBe(TRI_MID);
    expect(r.side(1)).toBe('below');
  });

  it('ticks are lo, q25, median, q75, hi at their positions', () => {
    const v = [3, 9, 1, 7, 5, 11, 13, 2, 8];
    const sorted = [...v].sort((a, b) => a - b);
    const t = triScale(v);
    expect(t.ticks.map((k) => k.value)).toEqual([1, quantileSorted(sorted, 0.25), 7, quantileSorted(sorted, 0.75), 13]);
    expect(t.ticks[0].pos).toBe(0);
    expect(t.ticks[2].pos).toBe(0.5);
    expect(t.ticks[4].pos).toBe(1);
  });

  it('side() words', () => {
    expect(s.side(1)).toBe('below');
    expect(s.side(3)).toBe('median');
    expect(s.side(4)).toBe('above');
  });
});

describe('triScale: centre zero (signed values)', () => {
  const s = triScale([-4, -1, 0, 2, 3, 10], { center: 'zero' });

  it('negatives orange, zero white, positives blue; the largest |v| reaches each end', () => {
    expect(s.mid).toBe(0);
    expect(s.color(0)).toBe(TRI_MID);
    expect(s.color(-4)).toBe(TRI_LOW);
    expect(s.color(10)).toBe(TRI_HIGH);
    expect(s.pos(-1) as number).toBeLessThan(0.5);
    expect(s.pos(-1) as number).toBeGreaterThan(s.pos(-4) as number);
    expect(s.pos(2) as number).toBeGreaterThan(0.5);
    expect(s.pos(3) as number).toBeGreaterThan(s.pos(2) as number);
    expect(s.side(-1)).toBe('below');
    expect(s.side(0)).toBe('median');
    expect(s.side(2)).toBe('above');
  });

  it('a single positive value is blue, not white; the smallest |v| is already off white', () => {
    const one = triScale([5], { center: 'zero' });
    expect(one.color(5)).toBe(TRI_HIGH);
    const small = triScale([1, 2, 3, 4], { center: 'zero' });
    expect(small.pos(1) as number).toBeCloseTo(0.625, 12);
  });

  it('only negatives: zero is white, the most negative orange', () => {
    const neg = triScale([-3, -2, -1], { center: 'zero' });
    expect(neg.lo).toBe(-3);
    expect(neg.hi).toBe(0);
    expect(neg.color(-3)).toBe(TRI_LOW);
    expect(neg.color(0)).toBe(TRI_MID);
    expect(neg.ticks.map((t) => t.value)).toEqual([-3, 0]);
  });
});

describe('triScale: value basis', () => {
  it('a fixed domain (Climate Action Tracker ratings 0..4, mid 2)', () => {
    const cat = triScale([], { domain: [0, 2, 4] });
    expect(cat.basis).toBe('value');
    expect(cat.color(0)).toBe(TRI_LOW);
    expect(cat.color(2)).toBe(TRI_MID);
    expect(cat.color(4)).toBe(TRI_HIGH);
    expect(cat.color(1)).toBe(TRI[2]);
    expect(cat.step(3)).toBe(6);
    expect(cat.ticks.map((t) => t.value)).toEqual([0, 2, 4]);
  });

  it('piecewise linear around the median of the values', () => {
    const v = triScale([0, 1, 10], { basis: 'value' });
    expect(v.mid).toBe(1);
    expect(v.pos(0.5)).toBe(0.25);
    expect(v.pos(5.5)).toBe(0.75);
    expect(v.pos(20)).toBe(1);
  });
});

describe('colours and text', () => {
  it('triColorAt interpolates between the stops and hits them exactly', () => {
    TRI.forEach((c, i) => expect(triColorAt(i / 8)).toBe(c));
    expect(triColorAt(-1)).toBe(TRI_LOW);
    expect(triColorAt(2)).toBe(TRI_HIGH);
    expect(triColorAt(1 / 16)).toMatch(/^#[0-9a-f]{6}$/);
  });

  it('text on every stop reaches 4.5:1, and white text never sits on orange', () => {
    for (const c of TRI) {
      const t = triText(c);
      expect(contrast(t, c)).toBeGreaterThanOrEqual(4.5);
      if (c === TRI_LOW) expect(t).not.toBe('#ffffff');
    }
  });

  it('medianSorted and quantileSorted', () => {
    expect(medianSorted([1, 2, 3])).toBe(2);
    expect(medianSorted([1, 2, 3, 4])).toBe(2.5);
    expect(Number.isNaN(medianSorted([]))).toBe(true);
    expect(quantileSorted([1, 2, 3, 4, 5], 0.25)).toBe(2);
  });
});
