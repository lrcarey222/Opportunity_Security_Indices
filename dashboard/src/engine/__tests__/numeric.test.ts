import { describe, expect, it } from 'vitest';
import { readFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import {
  colSumsR, dyadicToDouble, fmtF, fmtG, formatR7, LdAcc, ldAddReference, ldDivToDouble, ldDivToDoubleExact, ldSumCertified, ldSumStrided, ldSumStridedExact, meanR, medianR,
  medianScurveVec, roundR, rankAvg, rowMeansR, rowSumsR,
  sdR, sqrtDyadic, sqrtR, sumR,
} from '../numeric.ts';

const fx = JSON.parse(
  readFileSync(fileURLToPath(new URL('../__fixtures__/numeric.json', import.meta.url)), 'utf8'),
) as Record<string, unknown>;
const nums = (k: string) => (fx[k] as (number | null)[]).map((v) => (v === null ? NaN : v));
const same = (a: number, b: number) => (a !== a && b !== b) || Object.is(a, b) || a === b;

describe('long-double sums (exact x87 emulation)', () => {
  it('reproduces the 64-bit significand', () => {
    expect(sumR([1, 2 ** -60, -1])).toBe(2 ** -60);
    expect(sumR([1, 2 ** -70, -1])).toBe(0);
    expect(sumR([1, NaN, 2])).toBeNaN();
    expect(sumR([1, NaN, 2], 0, 3, true)).toBe(3);
  });
  it('matches R sum() bit for bit on the fixture vectors', () => {
    const vecs = fx.vecs as number[][];
    const sums = nums('sums');
    let bad = 0;
    vecs.forEach((v, i) => {
      if (!same(sumR(v), sums[i])) bad++;
    });
    expect(bad).toBe(0);
  });
  it('matches rowSums / colSums / rowMeans (na.rm)', () => {
    const m = nums('mat');
    const nr = fx.mat_nrow as number;
    const nc = fx.mat_ncol as number;
    expect(Array.from(rowSumsR(m, nr, nc, true))).toEqual(nums('rowSums'));
    expect(Array.from(colSumsR(m, nr, nc, true))).toEqual(nums('colSums'));
    const rm = rowMeansR(m, nr, nc, true);
    const want = nums('rowMeans');
    for (let i = 0; i < nr; i++) expect(same(rm[i], want[i])).toBe(true);
  });
  it('the fast accumulator equals the reference implementation on adversarial sequences', () => {
    let seed = 12345;
    const rnd = () => {
      seed = (seed * 1103515245 + 12345) % 2147483648;
      return seed / 2147483648;
    };
    const gen = (kind: number): number => {
      switch (kind) {
        case 0:
          return rnd();
        case 1:
          return (rnd() - 0.5) * 2 ** Math.floor(rnd() * 40 - 20);
        case 2:
          return 2 ** Math.floor(rnd() * 10 - 5) * (rnd() < 0.5 ? -1 : 1);
        case 3:
          return (1 + Math.floor(rnd() * 2 ** 20) * 2 ** -52) * 2 ** Math.floor(rnd() * 6 - 3) * (rnd() < 0.3 ? -1 : 1);
        default:
          return Math.floor(rnd() * 1000) / 1024 + 2 ** -(53 + Math.floor(rnd() * 20));
      }
    };
    let bad = 0;
    for (let rep = 0; rep < 4000; rep++) {
      const kind = rep % 5;
      const acc = new LdAcc();
      let ref: [number, number] = [0, 0];
      const n = 2 + Math.floor(rnd() * 40);
      for (let i = 0; i < n; i++) {
        const x = gen(kind);
        acc.add(x);
        ref = ldAddReference(ref[0], ref[1], x);
        if (acc.hi !== ref[0] || acc.lo !== ref[1]) {
          bad++;
          break;
        }
      }
    }
    expect(bad).toBe(0);
  });
  it('the certified fast sum equals the exact emulation (contiguous, strided, NA, cancellation, ties)', () => {
    let seed = 987654321;
    const rnd = () => {
      seed = (seed * 1103515245 + 12345) % 2147483648;
      return seed / 2147483648;
    };
    const gen = (kind: number): number => {
      switch (kind) {
        case 0: // RAS-like: positive, wide range
          return rnd() * 2 ** Math.floor(rnd() * -30);
        case 1: // mixed signs, wide range
          return (rnd() - 0.5) * 2 ** Math.floor(rnd() * 60 - 30);
        case 2: // exact binary fractions (ties)
          return Math.floor(rnd() * 64 - 32) * 2 ** Math.floor(rnd() * 8 - 70);
        case 3: // near-cancellation around 1
          return (rnd() < 0.5 ? -1 : 1) * (1 + Math.floor(rnd() * 2 ** 30) * 2 ** -52);
        case 4: // one big, many tiny with a tie at the 64th bit
          return rnd() < 0.2 ? 1 : 2 ** -64 * (rnd() < 0.5 ? 1 : 3);
        default:
          return rnd() < 0.1 ? NaN : rnd() * 1e3;
      }
    };
    let bad = 0;
    let fast = 0;
    let total = 0;
    for (let rep = 0; rep < 20000; rep++) {
      const kind = rep % 6;
      const n = 1 + Math.floor(rnd() * 60);
      const stride = 1 + (rep % 3);
      const x = Float64Array.from({ length: n * stride }, () => gen(kind));
      for (const naRm of [false, true]) {
        const a = ldSumStrided(x, 0, stride, n, naRm);
        const b = ldSumStridedExact(x, 0, stride, n, naRm);
        if (!same(a, b)) bad++;
      }
      // the reference accumulator agrees too (naRm)
      const acc = new LdAcc();
      for (let k = 0; k < n; k++) if (x[k * stride] === x[k * stride]) acc.add(x[k * stride]);
      if (!same(acc.value(), ldSumStrided(x, 0, stride, n, true))) bad++;
      total++;
      if (ldSumCertified()) fast++;
    }
    expect(bad).toBe(0);
    expect(fast / total).toBeGreaterThan(0.5); // the fast path is the common case
    // R's examples: the 64-bit significand
    expect(ldSumStrided([1, 2 ** -60, -1], 0, 1, 3)).toBe(2 ** -60);
    expect(ldSumStrided([1, 2 ** -70, -1], 0, 1, 3)).toBe(0);
  });
  it('divides in long double', () => {
    const acc = new LdAcc().add(1).add(2 ** -60);
    expect(ldDivToDouble(acc.hi, acc.lo, 3)).toBe((1 + 2 ** -60) / 3);
  });
  it('the certified long-double division equals the exact (BigInt) one', () => {
    let seed = 777;
    const rnd = () => {
      seed = (seed * 1103515245 + 12345) % 2147483648;
      return seed / 2147483648;
    };
    let bad = 0;
    for (let rep = 0; rep < 30000; rep++) {
      const acc = new LdAcc();
      const m = 1 + Math.floor(rnd() * 7);
      for (let j = 0; j < m; j++) {
        const kind = rep % 4;
        acc.add(kind === 0 ? rnd() : kind === 1 ? Math.round(rnd() * 64) / 64 : kind === 2 ? (rnd() - 0.3) * 2 ** Math.floor(rnd() * 40 - 20) : 1 + Math.floor(rnd() * 8) * 2 ** -53);
      }
      for (const n of [m, 3, 5, 7, 10]) {
        if (!same(ldDivToDouble(acc.hi, acc.lo, n), ldDivToDoubleExact(acc.hi, acc.lo, n))) bad++;
      }
    }
    expect(bad).toBe(0);
  });
});

describe('mean / median / sd', () => {
  const vecs = fx.vecs as number[][];
  it('mean() bit for bit', () => {
    const want = nums('means');
    let bad = 0;
    vecs.forEach((v, i) => {
      if (!same(meanR(v), want[i])) bad++;
    });
    expect(bad).toBe(0);
  });
  it('median() bit for bit', () => {
    const want = nums('medians');
    let bad = 0;
    vecs.forEach((v, i) => {
      if (!same(medianR(v), want[i])) bad++;
    });
    expect(bad).toBe(0);
  });
  it('sd() to 1e-15 relative', () => {
    const want = nums('sds');
    vecs.forEach((v, i) => {
      const got = sdR(v);
      if (want[i] !== want[i]) expect(got).toBeNaN();
      else expect(Math.abs(got - want[i])).toBeLessThanOrEqual(1e-15 * Math.abs(want[i]) + 1e-300);
    });
  });
});

describe('round / format', () => {
  it('round(x, 9), round(x, 0), round(x, 2)', () => {
    const rx = nums('rx');
    const r9 = nums('r9');
    const r0 = nums('r0');
    const r2 = nums('r2');
    rx.forEach((x, i) => {
      expect(roundR(x, 9)).toBe(r9[i]);
      expect(roundR(x * 20 - 3, 0)).toBe(r0[i]);
      expect(roundR(x, 2)).toBe(r2[i]);
    });
  });
  it('format(tau)', () => {
    const taus = nums('taus');
    const fmt = fx.fmt as string[];
    taus.forEach((t, i) => expect(formatR7(t)).toBe(fmt[i]));
  });
  it('sprintf %.3g and %.2f', () => {
    const gx = nums('gx');
    (fx.g3 as string[]).forEach((s, i) => expect(fmtG(gx[i], 3)).toBe(s));
    const fx2 = nums('f2x');
    (fx.f2 as string[]).forEach((s, i) => expect(fmtF(fx2[i], 2)).toBe(s));
  });
});

describe('median_scurve', () => {
  it("R's x^0.5 (powl, double rounding) bit for bit, where Math.sqrt is not", () => {
    const px = nums('px');
    const want = nums('p_half');
    let bad = 0;
    let jsDiff = 0;
    px.forEach((x, i) => {
      if (!same(sqrtR(x), want[i])) bad++;
      if (!same(Math.sqrt(x), want[i])) jsDiff++;
    });
    expect(bad).toBe(0);
    expect(jsDiff).toBeGreaterThanOrEqual(2); // the fixture's first two roots
    // the fast path agrees with the exact double-rounded root wherever it is taken
    let seed = 99;
    const rnd = () => {
      seed = (seed * 1103515245 + 12345) % 2147483648;
      return seed / 2147483648;
    };
    for (let k = 0; k < 3000; k++) {
      const x = rnd() + rnd() * 2 ** -31 + (k % 7) * 1e-3;
      const [m, e] = sqrtDyadic(x, 64);
      expect(sqrtR(x)).toBe(dyadicToDouble(m, e));
    }
    expect(sqrtR(0)).toBe(0);
    expect(sqrtR(1)).toBe(1);
    expect(sqrtR(0.25)).toBe(0.5);
    expect(sqrtR(NaN)).toBeNaN();
  });
  it('matches R', () => {
    const ins = fx.sc_in as (number | null)[][];
    const outs = fx.sc_out as (number | null)[][];
    ins.forEach((v, i) => {
      const got = medianScurveVec(v.map((x) => (x === null ? NaN : x)));
      const want = outs[i].map((x) => (x === null ? NaN : x));
      want.forEach((w, k) => expect(same(got[k], w)).toBe(true));
    });
  });
  it('rankAvg (radix argsort) equals the definition: ties, NA, signed zeros, infinities, subsets', () => {
    let seed = 4242;
    const rnd = () => {
      seed = (seed * 1103515245 + 12345) % 2147483648;
      return seed / 2147483648;
    };
    const pool = [0, -0, 1, -1, 0.5, Infinity, -Infinity, 1e-300, -1e-300, 2 ** 52, NaN];
    for (let rep = 0; rep < 400; rep++) {
      const n = 1 + Math.floor(rnd() * 300);
      const x = Float64Array.from({ length: n }, () =>
        rnd() < 0.3 ? pool[Math.floor(rnd() * pool.length)] : rep % 2 ? Math.round(rnd() * 20) / 4 - 2 : (rnd() - 0.5) * 2 ** Math.floor(rnd() * 80 - 40));
      const idx = rep % 3 === 0 ? Int32Array.from({ length: n }, (_, i) => i).filter(() => rnd() < 0.6) : undefined;
      const out = new Float64Array(n).fill(-1);
      const got = rankAvg(x, out, idx);
      const ks = idx ? Array.from(idx) : Array.from({ length: n }, (_, i) => i);
      const vals = ks.map((k) => x[k]).filter((v) => v === v);
      expect(got).toBe(vals.length);
      for (const k of ks) {
        const v = x[k];
        if (v !== v) {
          expect(out[k]).toBeNaN();
          continue;
        }
        const less = vals.filter((w) => w < v).length;
        const eq = vals.filter((w) => w === v).length;
        expect(out[k]).toBe(less + (eq + 1) / 2);
      }
      if (idx) for (let k = 0; k < n; k++) if (!idx.includes(k)) expect(out[k]).toBe(-1);
    }
  });
});
