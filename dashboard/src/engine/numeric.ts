// R numeric semantics the engine reproduces (spec_react.md 6.2). Pure; no dependencies.
//
// R accumulates sum(), mean(), rowSums(), colSums(), rowMeans() in 80-bit x87 long double on
// x86-64 Windows and Linux (64-bit significand; verified: sum(c(1, 2^-60, -1)) = 2^-60 and
// sum(c(1, 2^-70, -1)) = 0 on the reference machine). The accumulator below emulates that exactly:
// the running value is kept as an unevaluated pair (hi, lo) of doubles holding a number with a
// 64-bit significand, every addition is computed exactly with error-free transformations and
// rounded to 64 bits (round half to even), and the final value is rounded to a double, as R's
// `(double) s` does. Means and medians use exact BigInt arithmetic (they are rare).

export const NA_INT = -2147483648;
export const NA_BOOL = 255;

export class EngineError extends Error {
  readonly code: string;
  constructor(code: string, message: string) {
    super(message);
    this.code = code;
    this.name = 'EngineError';
  }
}

// ---- Bit helpers ---------------------------------------------------------------------------------

const F64 = new Float64Array(1);
const U32 = new Uint32Array(F64.buffer);
const P64 = new Float64Array(1);
const P32 = new Uint32Array(P64.buffer);

/** 2^k exactly (normal range). */
export function pow2(k: number): number {
  if (k > 1023) return Infinity;
  if (k < -1022) return Math.pow(2, k); // subnormal range (never reached by the engine)
  P32[0] = 0;
  P32[1] = (k + 1023) << 20;
  return P64[0];
}

/** Unbiased binary exponent of a finite non-zero double (floor(log2|x|)). */
export function exponentOf(x: number): number {
  F64[0] = x;
  const e = (U32[1] >>> 20) & 0x7ff;
  if (e === 0) {
    // subnormal
    return Math.floor(Math.log2(Math.abs(x)));
  }
  return e - 1023;
}

// ---- Long-double accumulation --------------------------------------------------------------------

// Result registers of ldAdd (module-level, so the hot loops allocate nothing).
let LD_H = 0;
let LD_L = 0;

// Binade cache of ldAdd: for |u| in [C_LO, C_HI) the 64-bit grid is C_Q and the rounding constant
// (g + C_K) - C_K rounds g to a multiple of C_Q, half to even (C_K = 1.5 * 2^(E - 11)).
let C_LO = Infinity;
let C_HI = -Infinity;
let C_Q = 0;
let C_K = 0;

function setBinade(au: number): void {
  const E = exponentOf(au);
  C_LO = pow2(E);
  C_HI = pow2(E + 1);
  C_Q = pow2(E - 63);
  C_K = 1.5 * pow2(E - 11);
}

/**
 * (hi, lo) + x with x87 extended-precision rounding: the exact sum rounded to a 64-bit significand
 * (half to even), returned in LD_H / LD_L with LD_H = the nearest double and LD_L the exact rest.
 *
 * The exact sum is u + g + f (error-free transformations); u is a multiple of 2^11 q, where q is the
 * 64-bit grid of the result's binade, so rounding to 64 bits rounds g to a multiple of q (half to
 * even: u is an even multiple of q). |f| is below half an ulp of g's grid, so it only breaks exact
 * ties of g.
 */
function ldAdd(hi: number, lo: number, x: number): void {
  const s = hi + x;
  let bb = s - hi;
  const e = hi - (s - bb) + (x - bb);
  if (e === 0 && lo === 0) {
    LD_H = s;
    LD_L = 0;
    return;
  }
  if (s !== s) {
    LD_H = s;
    LD_L = 0;
    return;
  }
  const t = e + lo;
  bb = t - e;
  const f = e - (t - bb) + (lo - bb);
  const u = s + t;
  bb = u - s;
  const g = s - (u - bb) + (t - bb);
  if (g === 0 && f === 0) {
    LD_H = u;
    LD_L = 0;
    return;
  }
  if (u === 0) {
    LD_H = g + f;
    LD_L = 0;
    return;
  }
  const au = u < 0 ? -u : u;
  if (!(au >= C_LO && au < C_HI)) setBinade(au);
  let q = C_Q;
  let K = C_K;
  if (au === C_LO) {
    // u is a power of two: a remainder of the opposite sign puts the result one binade down
    const r = g !== 0 ? g : f;
    if (r < 0 === u > 0) {
      q *= 0.5;
      K *= 0.5;
    }
  }
  let gr = g + K - K;
  if (f !== 0) {
    const rem = g - gr;
    const half = 0.5 * q;
    if ((rem === half || rem === -half) && f > 0 === rem > 0) gr = gr + 2 * rem;
  }
  const H = u + gr;
  bb = H - u;
  LD_H = H;
  LD_L = u - (H - bb) + (gr - bb);
}

// ---- Certified fast path ---------------------------------------------------------------------------
//
// The x87 accumulator's result differs from the exact sum by at most 2^-64 * sum_k |s_k| over the
// steps that add a non-zero term (one rounding to a 64-bit significand per such partial sum s_k;
// adding a zero is exact). A compensated double-double sum (TwoSum per step) gives the exact sum to
// within n 2^-106 sum_k |s_k|. So when the exact sum, as the pair (H, L) with H = fl(exact), lies
// farther than these bounds from both rounding boundaries of H (the midpoints to its neighbours),
// the x87 sum rounded to double is H, whatever the intermediate roundings were. Otherwise (a few per
// cent of the sums: near-midpoints, exact cancellation to zero, non-finite values) the exact
// emulation runs.

let CERT_OK = false;

/** H = fl(s + c) if it is certified to be the x87 result (CERT_OK), given a ~ sum_k |s_k| over the
 * non-zero terms (a = 0: every term is zero, the x87 sum is +0). */
function certify(s: number, c: number, a: number, n: number): number {
  CERT_OK = false;
  const H = s + c;
  if (!(H - H === 0)) return H; // non-finite: not certified
  if (a === 0) {
    CERT_OK = true;
    return 0;
  }
  if (H === 0) return H; // cancellation to zero: not certified
  const bb = H - s;
  const L = s - (H - bb) + (c - bb);
  const aH = H < 0 ? -H : H;
  if (aH < 2.2250738585072014e-292) return H; // near the subnormal range: not certified
  const Lp = H > 0 ? L : -L; // the rest, positive away from zero
  // 2^-64 per rounding, with a margin for a's own rounding (relative n 2^-53); the double-double error
  const bound = a * (5.421010862427522e-20 * 1.000001 + n * 9.860761315262648e-32 /* 2^-103 */);
  // cheap test first, with lower bounds of the two half-ulps: aH 2^-54 <= half, aH 2^-55 <= halfDown
  if (aH * 5.551115123125783e-17 - Lp > bound && aH * 2.7755575615628914e-17 + Lp > bound) {
    CERT_OK = true;
    return H;
  }
  const e = exponentOf(aH);
  const half = pow2(e - 53); // half an ulp of H: the distance to the midpoint away from zero
  const halfDown = aH === pow2(e) ? half * 0.5 : half; // the midpoint toward zero (closer below a power of two)
  if (half - Lp > bound && halfDown + Lp > bound) CERT_OK = true;
  return H;
}

/**
 * Second chance when certify() fails (mostly sums that land exactly on a midpoint: two terms of one
 * binade add up to 54 bits). If the non-zero terms span at most 11 binades (exponent of sum |x| minus
 * exponent of min |x| <= 11), every partial sum is a multiple of 2^(e_min - 52) below 2^(e_max + 1),
 * i.e. fits in 64 bits: the x87 accumulator is exact, and so is the double-double pair, so the
 * result is fl(exact) = H (+0 for an exact cancellation). Elements as in the kernels (idx or a
 * strided range; NaN skipped, it only reaches here with naRm).
 */
function spanExact(x: ArrayLike<number>, start: number, stride: number, count: number, idx: ArrayLike<number> | null): boolean {
  let m = Infinity;
  let M = 0;
  const n = idx ? idx.length : count;
  for (let k = 0, p = start; k < n; k++, p += stride) {
    const v = x[idx ? idx[k] : p];
    if (v !== v) continue;
    const av = v < 0 ? -v : v;
    M += av;
    if (av !== 0 && av < m) m = av;
  }
  if (!(M < Infinity) || m === Infinity) return false;
  return exponentOf(M * 1.0000000001) - exponentOf(m) <= 11;
}

/** spanExact() for the Float64Array kernel (monomorphic). */
function spanExactF64(x: Float64Array, start: number, stride: number, count: number): boolean {
  let m = Infinity;
  let M = 0;
  for (let k = 0, p = start; k < count; k++, p += stride) {
    const v = x[p];
    const av = v < 0 ? -v : v;
    M += av;
    if (av !== 0 && av < m) m = av;
  }
  if (!(M < Infinity) || m === Infinity) return false;
  return exponentOf(M * 1.0000000001) - exponentOf(m) <= 11;
}

/** Whether the last certified sum took the fast path (tests and diagnostics). */
export function ldSumCertified(): boolean {
  return CERT_OK;
}

/**
 * (double) of the long-double sum of x[start + k * stride], k = 0..count-1: R's sum() / rowSums() /
 * colSums() accumulation. naRm skips NaN; otherwise a NaN gives NaN.
 */
export function ldSumStrided(x: ArrayLike<number>, start: number, stride: number, count: number, naRm = false): number {
  let s = 0;
  let c = 0;
  let a = 0;
  let n = 0;
  for (let k = 0, p = start; k < count; k++, p += stride) {
    const v = x[p];
    if (v !== v) {
      if (naRm) continue;
      return NaN;
    }
    const t = s + v;
    if ((s < 0 ? -s : s) >= (v < 0 ? -v : v)) c += s - t + v;
    else c += v - t + s;
    s = t;
    if (v !== 0) a += t < 0 ? -t : t;
    n++;
  }
  const H = certify(s, c, a, n);
  if (CERT_OK) return H;
  if (H - H === 0 && spanExact(x, start, stride, count, null)) {
    CERT_OK = true;
    return H === 0 ? 0 : H;
  }
  return ldSumStridedExact(x, start, stride, count, naRm);
}

/** Counters of the RAS sums (diagnostics): [certified, certified by span, exact emulation]. */
export const LD_SUM_STATS = [0, 0, 0];

/**
 * ldSumStrided() for a Float64Array without NA handling (a NaN gives NaN): the monomorphic kernel of
 * the RAS fits, which take most of the greedy network's time.
 */
export function ldSumF64(x: Float64Array, start: number, stride: number, count: number): number {
  let s = 0;
  let c = 0;
  let a = 0;
  for (let k = 0, p = start; k < count; k++, p += stride) {
    const v = x[p];
    const t = s + v;
    const z = t - s;
    c += s - (t - z) + (v - z);
    s = t;
    if (v !== 0) a += t < 0 ? -t : t;
  }
  return ldFinishF64(s, c, a, x, start, stride, count);
}

/**
 * The result of a fused accumulation of x[start + k * stride], k < count, into the double-double
 * state (s, c) with a = sum |s_k| over the non-zero terms (TwoSum per term, as ldSumF64() does):
 * certified, certified by span, or the exact emulation over the same elements. For kernels that
 * accumulate while they write the data (the RAS scaling passes).
 */
export function ldFinishF64(s: number, c: number, a: number, x: Float64Array, start: number, stride: number, count: number): number {
  const H = certify(s, c, a, count);
  if (CERT_OK) {
    LD_SUM_STATS[0]++;
    return H;
  }
  if (H - H === 0 && spanExactF64(x, start, stride, count)) {
    LD_SUM_STATS[1]++;
    CERT_OK = true;
    return H === 0 ? 0 : H;
  }
  LD_SUM_STATS[2]++;
  return ldSumF64Exact(x, start, stride, count);
}

/** The exact emulation for ldSumF64() (ldSumStridedExact() typed for Float64Array). */
function ldSumF64Exact(x: Float64Array, start: number, stride: number, count: number): number {
  let hi = 0;
  let lo = 0;
  for (let k = 0, p = start; k < count; k++, p += stride) {
    const v = x[p];
    const s = hi + v;
    let bb = s - hi;
    const e = hi - (s - bb) + (v - bb);
    if (e === 0 && lo === 0) {
      hi = s;
      continue;
    }
    if (s !== s) return s;
    const t = e + lo;
    bb = t - e;
    const f = e - (t - bb) + (lo - bb);
    const u = s + t;
    bb = u - s;
    const g = s - (u - bb) + (t - bb);
    if (g === 0 && f === 0) {
      hi = u;
      lo = 0;
      continue;
    }
    if (u === 0) {
      hi = g + f;
      lo = 0;
      continue;
    }
    const au = u < 0 ? -u : u;
    if (!(au >= C_LO && au < C_HI)) setBinade(au);
    let q = C_Q;
    let K = C_K;
    if (au === C_LO) {
      const r = g !== 0 ? g : f;
      if (r < 0 === u > 0) {
        q *= 0.5;
        K *= 0.5;
      }
    }
    let gr = g + K - K;
    if (f !== 0) {
      const rem = g - gr;
      const half = 0.5 * q;
      if ((rem === half || rem === -half) && f > 0 === rem > 0) gr = gr + 2 * rem;
    }
    const H = u + gr;
    bb = H - u;
    lo = u - (H - bb) + (gr - bb);
    hi = H;
  }
  return hi;
}

/** sum() of x[idx[k]] in idx order (certified fast path, exact fallback). */
function ldSumIdx(x: ArrayLike<number>, idx: ArrayLike<number>, naRm: boolean): number {
  let s = 0;
  let c = 0;
  let a = 0;
  let n = 0;
  for (let k = 0; k < idx.length; k++) {
    const v = x[idx[k]];
    if (v !== v) {
      if (naRm) continue;
      return NaN;
    }
    const t = s + v;
    if ((s < 0 ? -s : s) >= (v < 0 ? -v : v)) c += s - t + v;
    else c += v - t + s;
    s = t;
    if (v !== 0) a += t < 0 ? -t : t;
    n++;
  }
  const H = certify(s, c, a, n);
  if (CERT_OK) return H;
  if (H - H === 0 && spanExact(x, 0, 1, 0, idx)) {
    CERT_OK = true;
    return H === 0 ? 0 : H;
  }
  let hi = 0;
  let lo = 0;
  for (let k = 0; k < idx.length; k++) {
    const v = x[idx[k]];
    if (v !== v) continue; // NaN only reaches here with naRm
    ldAdd(hi, lo, v);
    hi = LD_H;
    lo = LD_L;
  }
  return hi;
}

/**
 * The exact x87 emulation of ldSumStrided() (ldAdd inlined with local state): always correct, about
 * four times slower than the certified fast path that calls it when needed.
 */
export function ldSumStridedExact(x: ArrayLike<number>, start: number, stride: number, count: number, naRm = false): number {
  let hi = 0;
  let lo = 0;
  for (let k = 0, p = start; k < count; k++, p += stride) {
    const v = x[p];
    if (v !== v) {
      if (naRm) continue;
      return NaN;
    }
    const s = hi + v;
    let bb = s - hi;
    const e = hi - (s - bb) + (v - bb);
    if (e === 0 && lo === 0) {
      hi = s;
      continue;
    }
    if (s !== s) return s; // inf - inf
    const t = e + lo;
    bb = t - e;
    const f = e - (t - bb) + (lo - bb);
    const u = s + t;
    bb = u - s;
    const g = s - (u - bb) + (t - bb);
    if (g === 0 && f === 0) {
      hi = u;
      lo = 0;
      continue;
    }
    if (u === 0) {
      hi = g + f;
      lo = 0;
      continue;
    }
    const au = u < 0 ? -u : u;
    if (!(au >= C_LO && au < C_HI)) setBinade(au);
    let q = C_Q;
    let K = C_K;
    if (au === C_LO) {
      const r = g !== 0 ? g : f;
      if (r < 0 === u > 0) {
        q *= 0.5;
        K *= 0.5;
      }
    }
    let gr = g + K - K;
    if (f !== 0) {
      const rem = g - gr;
      const half = 0.5 * q;
      if ((rem === half || rem === -half) && f > 0 === rem > 0) gr = gr + 2 * rem;
    }
    const H = u + gr;
    bb = H - u;
    lo = u - (H - bb) + (gr - bb);
    hi = H;
  }
  return hi;
}

/** The first, slower implementation of ldAdd (kept as a cross-check for the unit tests). */
export function ldAddReference(hi: number, lo: number, x: number): [number, number] {
  // TwoSum(hi, x)
  const s = hi + x;
  let bb = s - hi;
  const e = hi - (s - bb) + (x - bb);
  if (e === 0 && lo === 0) return [s, 0];
  if (s !== s) return [s, 0];
  // TwoSum(e, lo)
  const t = e + lo;
  bb = t - e;
  const f = e - (t - bb) + (lo - bb);
  // TwoSum(s, t)
  const u = s + t;
  bb = u - s;
  const g = s - (u - bb) + (t - bb);
  // exact value T = u + g + f, |f| << |g| when g != 0
  if (g === 0 && f === 0) return [u, 0];
  if (u === 0) return [g + f, 0];
  F64[0] = u;
  const eb = (U32[1] >>> 20) & 0x7ff;
  const E = eb === 0 ? exponentOf(u) : eb - 1023;
  const isPow2 = (U32[1] & 0xfffff) === 0 && U32[0] === 0;
  const rSign = g !== 0 ? g : f;
  let q = pow2(E - 63);
  if (isPow2 && rSign < 0 === u > 0) q *= 0.5; // |T| < 2^E: one binade down, finer grid
  const a = g / q;
  const b = f / q;
  const n0 = Math.floor(a);
  const d = a - n0;
  let n: number;
  if (d < 0.25) n = n0;
  else if (d > 0.75) n = n0 + 1;
  else {
    const c = d - 0.5 + b;
    if (c > 0) n = n0 + 1;
    else if (c < 0) n = n0;
    else n = n0 % 2 === 0 ? n0 : n0 + 1;
  }
  const nq = n * q;
  const H = u + nq;
  bb = H - u;
  return [H, u - (H - bb) + (nq - bb)];
}

/** An incremental long-double accumulator (R's LDOUBLE s; s += x). */
export class LdAcc {
  hi = 0;
  lo = 0;
  add(x: number): this {
    ldAdd(this.hi, this.lo, x);
    this.hi = LD_H;
    this.lo = LD_L;
    return this;
  }
  /** (double) s */
  value(): number {
    return this.hi;
  }
  reset(): this {
    this.hi = 0;
    this.lo = 0;
    return this;
  }
}

/** R sum() of x[from..to) (long double). naRm: skip NaN; else any NaN gives NaN. */
export function sumR(x: ArrayLike<number>, from = 0, to = x.length, naRm = false): number {
  return ldSumStrided(x, from, 1, to - from, naRm);
}

/** R sum() of x[idx[k]] in idx order. */
export function sumIdx(x: ArrayLike<number>, idx: ArrayLike<number>, naRm = false): number {
  return ldSumIdx(x, idx, naRm);
}

/** R sum() of a plain list of numbers. */
export function sumList(xs: number[], naRm = false): number {
  return sumR(xs, 0, xs.length, naRm);
}

/** rowSums() of a column-major matrix [nrow, ncol] (long double per row, columns in order). */
export function rowSumsR(m: ArrayLike<number>, nrow: number, ncol: number, naRm = false, out?: Float64Array): Float64Array {
  const res = out ?? new Float64Array(nrow);
  for (let i = 0; i < nrow; i++) res[i] = ldSumStrided(m, i, nrow, ncol, naRm);
  return res;
}

/** colSums() of a column-major matrix [nrow, ncol]. */
export function colSumsR(m: ArrayLike<number>, nrow: number, ncol: number, naRm = false, out?: Float64Array): Float64Array {
  const res = out ?? new Float64Array(ncol);
  for (let j = 0; j < ncol; j++) res[j] = sumR(m, j * nrow, (j + 1) * nrow, naRm);
  return res;
}

/** rowMeans(m, na.rm) of a column-major matrix: long-double sums divided in long double. */
export function rowMeansR(m: ArrayLike<number>, nrow: number, ncol: number, naRm = false): Float64Array {
  const res = new Float64Array(nrow);
  const hi = new Float64Array(nrow);
  const lo = new Float64Array(nrow);
  const cnt = new Int32Array(nrow);
  for (let j = 0; j < ncol; j++) {
    const off = j * nrow;
    for (let i = 0; i < nrow; i++) {
      const v = m[off + i];
      if (v !== v) {
        if (naRm) continue;
        hi[i] = NaN;
        continue;
      }
      ldAdd(hi[i], lo[i], v);
      hi[i] = LD_H;
      lo[i] = LD_L;
      cnt[i]++;
    }
  }
  for (let i = 0; i < nrow; i++) res[i] = ldDivToDouble(hi[i], lo[i], naRm ? cnt[i] : ncol);
  return res;
}

/**
 * (double)((long double)(hi + lo) / n) for a positive integer n: the exact quotient rounded to a
 * 64-bit significand, then to a double (R's rowMeans() / colMeans()).
 */
export function ldDivToDouble(hi: number, lo: number, n: number): number {
  if (hi !== hi) return NaN;
  if (n === 0) return hi === 0 && lo === 0 ? NaN : hi / 0;
  if (n === 1) return hi;
  if (hi === 0 && lo === 0) return 0;
  // Certified fast path: d = fl(hi / n). The exact quotient q = (hi + lo) / n is d + r / n with the
  // residual r = hi + lo - d n computed exactly enough (Dekker's TwoProduct; hi - p is exact by
  // Sterbenz). If q lies farther than the 64-bit rounding error (2^-64 |q|, doubled) from both
  // midpoints around d, the long double quotient rounds to d.
  const d = hi / n;
  const ad = d < 0 ? -d : d;
  if (ad > 1e-290 && ad < 1e290) {
    const p = d * n;
    const r = hi - p + (lo - twoProdErr(d, n, p));
    const dq = r / n;
    const dqp = d > 0 ? dq : -dq;
    const e = exponentOf(ad);
    const half = pow2(e - 53);
    const halfDown = ad === pow2(e) ? half * 0.5 : half;
    const margin = (ad + half) * 1.0842021724855044e-19; // 2^-63
    if (half - dqp > margin && halfDown + dqp > margin) return d;
  }
  return ldDivToDoubleExact(hi, lo, n);
}

const SPLITTER = 134217729; // 2^27 + 1

/** The rounding error of p = fl(a * b): a * b - p exactly (Dekker's TwoProduct, no FMA). */
function twoProdErr(a: number, b: number, p: number): number {
  let t = SPLITTER * a;
  const ah = t - (t - a);
  const al = a - ah;
  t = SPLITTER * b;
  const bh = t - (t - b);
  const bl = b - bh;
  return ah * bh - p + ah * bl + al * bh + al * bl;
}

/** ldDivToDouble() by exact dyadic arithmetic (BigInt): the reference and the fallback. */
export function ldDivToDoubleExact(hi: number, lo: number, n: number): number {
  if (hi !== hi) return NaN;
  if (n === 0) return hi === 0 && lo === 0 ? NaN : hi / 0;
  if (n === 1) return hi;
  if (hi === 0 && lo === 0) return 0;
  const [m1, e1] = toDyadic(hi);
  let m = m1;
  let e = e1;
  if (lo !== 0) {
    const [m2, e2] = toDyadic(lo);
    const ee = Math.min(e, e2);
    m = (m << BigInt(e - ee)) + (m2 << BigInt(e2 - ee));
    e = ee;
  }
  const [q, qe] = roundRational(m, BigInt(n), e, 64);
  return dyadicToDouble(q, qe);
}

// ---- Exact dyadic arithmetic (BigInt) ------------------------------------------------------------

/** x = m * 2^e exactly (x finite). */
export function toDyadic(x: number): [bigint, number] {
  if (x === 0) return [0n, 0];
  F64[0] = x;
  const hiw = U32[1];
  const low = U32[0];
  const neg = hiw >>> 31 === 1;
  const eb = (hiw >>> 20) & 0x7ff;
  let mant = (BigInt(hiw & 0xfffff) << 32n) | BigInt(low);
  let e: number;
  if (eb === 0) {
    e = -1074;
  } else {
    mant |= 1n << 52n;
    e = eb - 1075;
  }
  // strip trailing zeros (keeps numbers small)
  while (mant !== 0n && (mant & 1n) === 0n) {
    mant >>= 1n;
    e++;
  }
  return [neg ? -mant : mant, e];
}

function bitLength(m: bigint): number {
  if (m < 0n) m = -m;
  return m === 0n ? 0 : m.toString(2).length;
}

/** Round m * 2^e to `bits` significant bits (half to even). */
export function roundDyadic(m: bigint, e: number, bits: number): [bigint, number] {
  if (m === 0n) return [0n, 0];
  const neg = m < 0n;
  let a = neg ? -m : m;
  const L = bitLength(a);
  if (L <= bits) return [m, e];
  const s = L - bits;
  const S = BigInt(s);
  const low = a & ((1n << S) - 1n);
  const half = 1n << (S - 1n);
  a >>= S;
  if (low > half || (low === half && (a & 1n) === 1n)) a += 1n;
  return [neg ? -a : a, e + s];
}

/** Round (num / den) * 2^e to `bits` significant bits (half to even); den > 0. */
export function roundRational(num: bigint, den: bigint, e: number, bits: number): [bigint, number] {
  if (num === 0n) return [0n, 0];
  const neg = num < 0n;
  let a = neg ? -num : num;
  const k = Math.max(0, bits + 2 - (bitLength(a) - bitLength(den)));
  a <<= BigInt(k);
  let q = a / den;
  const r = a % den;
  const L = bitLength(q);
  const s = L - bits;
  let ex = e - k;
  if (s > 0) {
    const S = BigInt(s);
    const low = q & ((1n << S) - 1n);
    const half = 1n << (S - 1n);
    q >>= S;
    ex += s;
    if (low > half || (low === half && (r !== 0n || (q & 1n) === 1n))) q += 1n;
  }
  return [neg ? -q : q, ex];
}

/** The double nearest to m * 2^e (half to even). */
export function dyadicToDouble(m: bigint, e: number): number {
  if (m === 0n) return 0;
  const [r, re] = roundDyadic(m, e, 53);
  return Number(r) * pow2(re);
}

function dyAdd(a: [bigint, number], b: [bigint, number]): [bigint, number] {
  if (a[0] === 0n) return b;
  if (b[0] === 0n) return a;
  const e = Math.min(a[1], b[1]);
  return [(a[0] << BigInt(a[1] - e)) + (b[0] << BigInt(b[1] - e)), e];
}

function dyNeg(a: [bigint, number]): [bigint, number] {
  return [-a[0], a[1]];
}

function ld64(a: [bigint, number]): [bigint, number] {
  return roundDyadic(a[0], a[1], 64);
}

function ldDivN(a: [bigint, number], n: number): [bigint, number] {
  return roundRational(a[0], BigInt(n), a[1], 64);
}

/** R mean() of finite doubles (long double two-pass algorithm, exact emulation). */
export function meanR(x: ArrayLike<number>): number {
  const n = x.length;
  if (n === 0) return NaN;
  let s: [bigint, number] = [0n, 0];
  for (let i = 0; i < n; i++) {
    if (x[i] !== x[i]) return NaN;
    s = ld64(dyAdd(s, toDyadic(x[i])));
  }
  s = ldDivN(s, n);
  const sd = dyadicToDouble(s[0], s[1]);
  if (Number.isFinite(sd)) {
    let t: [bigint, number] = [0n, 0];
    for (let i = 0; i < n; i++) {
      const diff = ld64(dyAdd(toDyadic(x[i]), dyNeg(s)));
      t = ld64(dyAdd(t, diff));
    }
    s = ld64(dyAdd(s, ldDivN(t, n)));
  }
  return dyadicToDouble(s[0], s[1]);
}

/** R median(x, na.rm) (sort, middle value; even n: mean() of the two middle values). */
export function medianR(x: ArrayLike<number>, naRm = true): number {
  const v: number[] = [];
  for (let i = 0; i < x.length; i++) {
    const xi = x[i];
    if (xi !== xi) {
      if (!naRm) return NaN;
      continue;
    }
    v.push(xi);
  }
  const n = v.length;
  if (n === 0) return NaN;
  const a = Float64Array.from(v).sort();
  const half = (n + 1) >> 1; // R: half <- (n + 1L) %/% 2L (1-based)
  if (n % 2 === 1) return a[half - 1];
  return meanR([a[half - 1], a[half]]);
}

/** stats::sd() (var: long-double mean, then sum of squared deviations / (n - 1)); NA when n < 2. */
export function sdR(x: ArrayLike<number>): number {
  const n = x.length;
  if (n < 2) return NaN;
  for (let i = 0; i < n; i++) if (x[i] !== x[i]) return NaN;
  // cov.c MEAN(): long-double sum / n, refined by the mean of the deviations
  let s: [bigint, number] = [0n, 0];
  for (let i = 0; i < n; i++) s = ld64(dyAdd(s, toDyadic(x[i])));
  s = ldDivN(s, n);
  let t: [bigint, number] = [0n, 0];
  for (let i = 0; i < n; i++) t = ld64(dyAdd(t, ld64(dyAdd(toDyadic(x[i]), dyNeg(s)))));
  s = ld64(dyAdd(s, ldDivN(t, n)));
  const mean = dyadicToDouble(s[0], s[1]);
  // sum of squared deviations (double deviations, long-double accumulation)
  const acc = new LdAcc();
  for (let i = 0; i < n; i++) {
    const d = x[i] - mean;
    acc.add(d * d);
  }
  return Math.sqrt(ldDivToDouble(acc.hi, acc.lo, n - 1));
}

// ---- Ranks and the median S-curve ----------------------------------------------------------------

function lowerBound(a: Float64Array, n: number, v: number): number {
  let lo = 0;
  let hi = n;
  while (lo < hi) {
    const mid = (lo + hi) >>> 1;
    if (a[mid] < v) lo = mid + 1;
    else hi = mid;
  }
  return lo;
}

// Scratch of rankAvg()'s radix argsort (grown on demand, reused).
let R_LO = new Uint32Array(0);
let R_HI = new Uint32Array(0);
let R_P0 = new Int32Array(0);
let R_P1 = new Int32Array(0);
let R_V = new Float64Array(0);
let R_POS = new Int32Array(0);
const R_HIST = new Int32Array(6 * 2048);

/**
 * rank(x, na.last = "keep", ties.method = "average") over the elements x[idx[k]] (or all of x),
 * written to out at the same positions (NaN for NA). Returns the number of non-NA values.
 *
 * The non-NA values are argsorted with an LSD radix sort on their IEEE bits (mapped to unsigned
 * order; -0 as +0), six 11-bit digits with the passes whose digit is common to every key skipped;
 * each tie group (equal values, adjacent in that order) gets the average of its ranks. The ranks are
 * exact, as R's; the order within a tie group does not matter.
 */
export function rankAvg(x: ArrayLike<number>, out: Float64Array, idx?: ArrayLike<number>): number {
  const m = idx ? idx.length : x.length;
  if (R_LO.length < m) {
    const cap = Math.max(m, 1024);
    R_LO = new Uint32Array(cap);
    R_HI = new Uint32Array(cap);
    R_P0 = new Int32Array(cap);
    R_P1 = new Int32Array(cap);
    R_V = new Float64Array(cap);
    R_POS = new Int32Array(cap);
  }
  const KLO = R_LO;
  const KHI = R_HI;
  const V = R_V;
  const POS = R_POS;
  const H = R_HIST;
  H.fill(0);
  let n = 0;
  for (let k = 0; k < m; k++) {
    const p = idx ? idx[k] : k;
    const v = x[p];
    if (v !== v) {
      out[p] = NaN;
      continue;
    }
    F64[0] = v === 0 ? 0 : v;
    let lo = U32[0];
    let hi = U32[1];
    if (hi & 0x80000000) {
      hi = ~hi >>> 0;
      lo = ~lo >>> 0;
    } else {
      hi = (hi | 0x80000000) >>> 0;
    }
    KLO[n] = lo;
    KHI[n] = hi;
    V[n] = v;
    POS[n] = p;
    R_P0[n] = n;
    H[lo & 2047]++;
    H[2048 + ((lo >>> 11) & 2047)]++;
    H[4096 + (lo >>> 22)]++;
    H[6144 + (hi & 2047)]++;
    H[8192 + ((hi >>> 11) & 2047)]++;
    H[10240 + (hi >>> 22)]++;
    n++;
  }
  let src = R_P0;
  let dst = R_P1;
  for (let d = 0; d < 6; d++) {
    const base = d * 2048;
    let trivial = false;
    for (let b = 0; b < 2048; b++) {
      const c = H[base + b];
      if (c === n) trivial = true;
      if (c) break;
    }
    if (trivial) continue;
    let sum = 0;
    for (let b = 0; b < 2048; b++) {
      const c = H[base + b];
      H[base + b] = sum;
      sum += c;
    }
    const keys = d < 3 ? KLO : KHI;
    const shift = (d % 3) * 11;
    for (let q = 0; q < n; q++) {
      const i = src[q];
      dst[H[base + ((keys[i] >>> shift) & 2047)]++] = i;
    }
    const t = src;
    src = dst;
    dst = t;
  }
  for (let i = 0; i < n; ) {
    const v = V[src[i]];
    let j = i;
    while (j + 1 < n && V[src[j + 1]] === v) j++;
    const r = (i + j + 2) / 2;
    for (let q = i; q <= j; q++) out[POS[src[q]]] = r;
    i = j + 1;
  }
  return n;
}

/** rank(x, ties.method = "min") of the finite values x[idx[k]] (no NA expected), 1-based ints. */
export function rankMin(x: ArrayLike<number>, idx?: ArrayLike<number>): Int32Array {
  const m = idx ? idx.length : x.length;
  const buf = new Float64Array(m);
  for (let k = 0; k < m; k++) buf[k] = x[idx ? idx[k] : k];
  const sorted = Float64Array.from(buf).sort();
  const out = new Int32Array(m);
  for (let k = 0; k < m; k++) out[k] = lowerBound(sorted, m, buf[k]) + 1;
  return out;
}

// ---- R's x^0.5 ----------------------------------------------------------------------------------

/** floor(sqrt(n)) for n >= 0 (Newton's method on BigInt). */
function isqrt(n: bigint): bigint {
  if (n < 2n) return n;
  let x = 1n << BigInt(Math.ceil(bitLength(n) / 2));
  for (;;) {
    const y = (x + n / x) >> 1n;
    if (y >= x) return x;
    x = y;
  }
}

/** The exact square root of x > 0 rounded to `bits` significant bits (half to even), as m * 2^e. */
export function sqrtDyadic(x: number, bits: number): [bigint, number] {
  let [m, e] = toDyadic(x);
  if (e % 2 !== 0) {
    m <<= 1n;
    e -= 1;
  }
  // N = m * 2^k with an even k so that floor(sqrt(N)) has at least bits + 2 bits
  let k = Math.max(0, 2 * (bits + 2) - bitLength(m) + 2);
  if (k % 2 !== 0) k++;
  const N = m << BigInt(k);
  let q = isqrt(N);
  const exact = q * q === N;
  let ex = (e - k) / 2;
  const s = bitLength(q) - bits;
  if (s > 0) {
    const S = BigInt(s);
    const low = q & ((1n << S) - 1n);
    const half = 1n << (S - 1n);
    q >>= S;
    ex += s;
    // the true root is q.low... plus a positive remainder when not exact
    if (low > half || (low === half && (!exact || (q & 1n) === 1n))) q += 1n;
  }
  return [q, ex];
}

/** sqrtR() calls that needed the exact path (diagnostics). */
export const SQRT_R_STATS = [0, 0];

/**
 * R's x^0.5 on x86-64 (R_pow() calls powl(): the root rounded to the x87 64-bit significand, then
 * to a double), which is not always the correctly rounded Math.sqrt(x): when the exact root lies
 * within half a 64-bit unit of a midpoint between two doubles, the second rounding goes to even
 * (about 1 in 2^11 roots). median_scurve() computes r^gamma this way, so the S-curves (the
 * Friendshore and Opportunity arrays above all) are bit-identical to R's only with this emulation.
 * Verified against R 4.4.1 (Windows x86-64) on 483,376 roots.
 */
export function sqrtR(x: number): number {
  const s = Math.sqrt(x);
  if (!(x > 0) || x === Infinity) return s;
  SQRT_R_STATS[0]++;
  // delta = sqrt(x) - s ~ (x - s^2) / (2 s), with x - s^2 exact (Sterbenz; Dekker's product)
  const p = s * s;
  const d = x - p - twoProdErr(s, s, p);
  const delta = d / (2 * s);
  const u = pow2(exponentOf(s) - 52);
  // double rounding can differ from s only within u / 2^12 of a midpoint s +- u / 2 (margin x8)
  if (Math.abs(delta) < u / 2 - u / 512) return s;
  SQRT_R_STATS[1]++;
  const [m64, e64] = sqrtDyadic(x, 64);
  return dyadicToDouble(m64, e64);
}

/**
 * median_scurve(x, gamma = 0.5) (R/utils/scurve.R) over x[idx[k]] (or all of x), written to out at
 * the same positions: r = (rank - 1) / (n - 1) with average ranks over the n non-NA values
 * (r = 0 when n = 1), S = r^0.5 / (r^0.5 + (1 - r)^0.5) with R's x^0.5 (sqrtR()); all NA when n = 0.
 */
export function medianScurve(x: ArrayLike<number>, out: Float64Array, idx?: ArrayLike<number>, gamma = 0.5): void {
  const n = rankAvg(x, out, idx);
  const m = idx ? idx.length : x.length;
  const pw = gamma === 0.5 ? sqrtR : (v: number) => Math.pow(v, gamma);
  for (let k = 0; k < m; k++) {
    const p = idx ? idx[k] : k;
    const rk = out[p];
    if (rk !== rk) continue;
    const r = n === 1 ? 0 : (rk - 1) / (n - 1);
    const a = pw(r);
    out[p] = a / (a + pw(1 - r));
  }
}

/** median_scurve() of a whole vector, returning a new array. */
export function medianScurveVec(x: ArrayLike<number>): Float64Array {
  const out = new Float64Array(x.length);
  medianScurve(x, out);
  return out;
}

// ---- Rounding and formatting ---------------------------------------------------------------------

/** C rint / nearbyint: round half to even. */
export function rint(x: number): number {
  if (!Number.isFinite(x)) return x;
  const f = Math.floor(x);
  const d = x - f;
  let r: number;
  if (d < 0.5) r = f;
  else if (d > 0.5) r = f + 1;
  else r = f % 2 === 0 ? f : f + 1;
  return r === 0 && x < 0 ? -0 : r;
}

/** R round(x, digits) (R >= 4.0.0 algorithm: the closer of the two decimal candidates, ties to even). */
export function roundR(x: number, digits = 0): number {
  if (x !== x) return x;
  if (!Number.isFinite(x) || x === 0) return x;
  const dig = Math.floor(digits + 0.5);
  if (dig === 0) return rint(x);
  if (dig > 323) return x;
  const sgn = x < 0 ? -1 : 1;
  const ax = Math.abs(x);
  const l10x = 0.30102999566398119521 * (0.5 + exponentOf(ax));
  if (l10x + dig > 15) return x;
  if (dig > 0) {
    const p10 = Math.pow(10, dig);
    const x10 = p10 * ax;
    const i10 = Math.floor(x10);
    const xd = i10 / p10;
    const xu = Math.ceil(x10) / p10;
    const du = xu - ax;
    const dd = ax - xd;
    return sgn * (du < dd || (du === dd && i10 % 2 === 1) ? xu : xd);
  }
  const p10 = Math.pow(10, -dig);
  const x10 = ax / p10;
  const i10 = Math.floor(x10);
  const xd = i10 * p10;
  const xu = Math.ceil(x10) * p10;
  const du = xu - ax;
  const dd = ax - xd;
  return sgn * (du < dd || (du === dd && i10 % 2 === 1) ? xu : xd);
}

/** True when x * 10^f lies exactly half-way between two integers (an exact decimal tie). */
function isDecimalTie(x: number, f: number): boolean {
  if (x === 0 || !Number.isFinite(x)) return false;
  // x = m 2^e with m odd: x 10^f 2 = m 5^f 2^(e + f + 1) is an odd integer iff e = -(f + 1)
  const [, e] = toDyadic(x);
  return e === -(f + 1);
}

/** C sprintf("%.<f>f", x) (exact decimal ties round half to even, as glibc / msvcrt do). */
export function fmtF(x: number, f: number): string {
  if (x !== x) return 'NA';
  if (!Number.isFinite(x)) return x > 0 ? 'Inf' : '-Inf';
  if (isDecimalTie(x, f)) {
    const p = Math.pow(10, f);
    const lo = Math.floor(Math.abs(x) * p);
    const n = lo % 2 === 0 ? lo : lo + 1;
    const s = (n / p).toFixed(f);
    return x < 0 ? `-${s}` : s;
  }
  return x.toFixed(f);
}

/** C sprintf("%.<p>g", x). */
export function fmtG(x: number, p: number): string {
  if (x !== x) return 'NA';
  if (!Number.isFinite(x)) return x > 0 ? 'Inf' : '-Inf';
  if (x === 0) return '0';
  const P = p === 0 ? 1 : p;
  // exponent after rounding to P significant digits
  let s = x.toExponential(P - 1);
  let [mant, ex] = s.split('e');
  let X = parseInt(ex, 10);
  if (X < -4 || X >= P) {
    mant = stripZeros(mant);
    const sign = X < 0 ? '-' : '+';
    const ax = Math.abs(X);
    return `${mant}e${sign}${ax < 10 ? '0' : ''}${ax}`;
  }
  s = fmtF(x, P - 1 - X);
  return stripZeros(s);
}

function stripZeros(s: string): string {
  if (s.indexOf('.') < 0) return s;
  return s.replace(/0+$/, '').replace(/\.$/, '');
}

/** R format(x) with the default 7 significant digits, for a single finite number. */
export function formatR7(x: number): string {
  if (x !== x) return 'NA';
  if (!Number.isFinite(x)) return x > 0 ? 'Inf' : '-Inf';
  if (x === 0) return '0';
  // fewest significant digits (<= 7) that reproduce x at 7 digits
  const target = Number(x.toPrecision(7));
  let sig = 1;
  for (; sig < 7; sig++) if (Number(x.toPrecision(sig)) === target) break;
  const ex = Math.floor(Math.log10(Math.abs(target)));
  // fixed notation
  const decimals = Math.max(0, sig - 1 - ex);
  const fixed = fmtF(Math.abs(x), decimals);
  // scientific notation
  const mant = stripZeros(Math.abs(target).toExponential(sig - 1).split('e')[0]);
  const ae = Math.abs(ex);
  const sci = `${mant}e${ex < 0 ? '-' : '+'}${ae < 10 ? '0' : ''}${ae}`;
  const body = fixed.length <= sci.length ? fixed : sci;
  return x < 0 ? `-${body}` : body;
}

// ---- Ordering ------------------------------------------------------------------------------------

/** C-locale string comparison (UTF-16 code units; equals byte order for BMP text). NA last. */
export function cmpC(a: string | null, b: string | null): number {
  if (a === b) return 0;
  if (a === null) return 1;
  if (b === null) return -1;
  return a < b ? -1 : 1;
}

export type SortKey =
  | { num: ArrayLike<number>; desc?: boolean }
  | { str: ArrayLike<string | null>; desc?: boolean }
  | { bool: ArrayLike<number>; desc?: boolean };

/**
 * Stable order of 0..n-1 (or of `subset`) by several keys, like order(..., method = "radix") and
 * dplyr::arrange(): numbers ascending (desc: descending), NaN last either way; strings in C-locale
 * order, NA last; logical keys as 0 < 1, NA last.
 */
export function orderBy(keys: SortKey[], n: number, subset?: ArrayLike<number>): number[] {
  const idx: number[] = subset ? Array.from(subset) : Array.from({ length: n }, (_, i) => i);
  idx.sort((i, j) => {
    for (const k of keys) {
      let c = 0;
      if ('num' in k) {
        const a = k.num[i];
        const b = k.num[j];
        const an = a !== a;
        const bn = b !== b;
        if (an || bn) c = an && bn ? 0 : an ? 1 : -1;
        else if (a !== b) c = (a < b ? -1 : 1) * (k.desc ? -1 : 1);
      } else if ('str' in k) {
        const a = k.str[i];
        const b = k.str[j];
        if (a === null || b === null) c = a === b ? 0 : a === null ? 1 : -1;
        else if (a !== b) c = (a < b ? -1 : 1) * (k.desc ? -1 : 1);
      } else {
        const a = k.bool[i];
        const b = k.bool[j];
        const an = a === NA_BOOL;
        const bn = b === NA_BOOL;
        if (an || bn) c = an && bn ? 0 : an ? 1 : -1;
        else if (a !== b) c = (a < b ? -1 : 1) * (k.desc ? -1 : 1);
      }
      if (c !== 0) return c;
    }
    return i - j;
  });
  return idx;
}

/** order(x) for one numeric key: ascending, NaN last, ties in index order (a fast orderBy()). */
export function orderNum(x: ArrayLike<number>, n = x.length): Int32Array {
  const idx = new Int32Array(n);
  for (let i = 0; i < n; i++) idx[i] = i;
  idx.sort((i, j) => {
    const a = x[i];
    const b = x[j];
    if (a < b) return -1;
    if (a > b) return 1;
    if (a === b) return i - j;
    const an = a !== a;
    const bn = b !== b;
    if (an && bn) return i - j;
    return an ? 1 : -1;
  });
  return idx;
}

/** sort(unique(x), method = "radix") for strings (NA dropped). */
export function sortUniqueC(x: Iterable<string | null>): string[] {
  const s = new Set<string>();
  for (const v of x) if (v !== null) s.add(v);
  return Array.from(s).sort(cmpC);
}

/** unique() keeping first appearance. */
export function uniqueKeep<T>(x: Iterable<T>): T[] {
  return Array.from(new Set(x));
}

// ---- Misc ----------------------------------------------------------------------------------------

/** Deep equality for JSON-like values: NaN equals NaN, null equals null, key order ignored. */
export function deepEqual(a: unknown, b: unknown): boolean {
  if (a === b) return true;
  if (typeof a === 'number' && typeof b === 'number') return a !== a && b !== b;
  if (a === null || b === null || typeof a !== 'object' || typeof b !== 'object') return false;
  if (Array.isArray(a) !== Array.isArray(b)) return false;
  if (Array.isArray(a)) {
    const bb = b as unknown[];
    if (a.length !== bb.length) return false;
    for (let i = 0; i < a.length; i++) if (!deepEqual(a[i], bb[i])) return false;
    return true;
  }
  const ka = Object.keys(a as object);
  const kb = Object.keys(b as object);
  if (ka.length !== kb.length) return false;
  for (const k of ka) {
    if (!Object.prototype.hasOwnProperty.call(b, k)) return false;
    if (!deepEqual((a as Record<string, unknown>)[k], (b as Record<string, unknown>)[k])) return false;
  }
  return true;
}

/** Yield to the event loop (lets a worker notice a superseding message). */
export function yieldNow(): Promise<void> {
  if (typeof MessageChannel !== 'undefined') {
    return new Promise((resolve) => {
      const ch = new MessageChannel();
      ch.port1.onmessage = () => {
        ch.port1.close();
        resolve();
      };
      ch.port2.postMessage(0);
    });
  }
  return new Promise((resolve) => setTimeout(resolve, 0));
}
