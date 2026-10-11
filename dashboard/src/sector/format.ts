// Sector Insights number formats (spec_sector.md 3.9). Pure. Every value is formatted from its unit's
// `unit_kind` (dict.units) after the dictionary's `display_scale`; the unit words come from the unit row,
// never from the processed `explanation`. NA (NaN, null, undefined) prints as an en dash.
//
//   usd           SI-scaled, 3 significant figures: "USD 203 bn", "USD 6.1 bn", "USD 840 m"; axis "203bn"
//   usd_bn_real   values in USD bn (2024$): "USD 22.4 bn (2024$)"
//   usd_m         values in USD m (e.g. ODA): scaled like usd
//   share         .1%: "43.7%", "<0.1%"
//   pct           a value already in percent: "4.4%"
//   hhi           .3f: "0.207"; a lower bound "≥ 0.21"
//   quantities    grouped thousands; 3 significant figures on tiles, full in tables: "21,000 kb/d", "1.23 EJ"
//   gj_pc         "64 GJ per person"
//   price         "USD 79.4 /bbl"
//   count         integer
//   ratio         .2f
//   trl           1 decimal
//   signed        arrow + sign: "▲ 12.4%", "▼ 3.1 EJ"

export const DASH = '–';
export const MINUS = '−';
export const UP = '▲';
export const DOWN = '▼';

export const isFiniteNum = (x: unknown): x is number => typeof x === 'number' && Number.isFinite(x);

/** The unit of a measure as the formatter needs it (a dict.units row). */
export interface UnitSpec {
  /** dict.units id */
  id?: string;
  /** short label, e.g. "kb/d", "EJ", "USD/bbl" */
  short: string;
  /** long label for captions and aria, e.g. "thousand barrels a day" */
  long?: string;
  unit_kind: string;
}

/** Where a number is shown: tiles round to 3 significant figures; tables keep more digits. */
export type FormatContext = 'tile' | 'table' | 'axis' | 'text';

const QUANTITY_KINDS = new Set(['kbd', 'ej', 'pj', 'twh', 'gw', 'gwh', 'mw', 'kt', 't', 'tu', 'kg', 'potential', 'vehicles', 'number', 'w_m2', 'kwh_kwp_day', 'years', 'lcoe', 'index', 'mvehicles']);
const USD_KINDS = new Set(['usd', 'usd_bn_real', 'usd_m']);

/** True minus sign. */
const minus = (s: string): string => (s.startsWith('-') ? MINUS + s.slice(1) : s);

/** Round to `sig` significant figures (as a number). */
export function roundSig(x: number, sig = 3): number {
  if (x === 0 || !Number.isFinite(x)) return x;
  const d = Math.ceil(Math.log10(Math.abs(x)));
  const p = sig - d;
  const m = Math.pow(10, Math.abs(p));
  return p >= 0 ? Math.round(x * m) / m : Math.round(x / m) * m;
}

/** A number with grouped thousands and at most `maxFrac` decimals (trailing zeros dropped). */
export function fmtGrouped(x: number, maxFrac = 0, minFrac = 0): string {
  return minus(x.toLocaleString('en-US', { maximumFractionDigits: maxFrac, minimumFractionDigits: Math.min(minFrac, maxFrac) }));
}

/** 3 significant figures with grouping: 21034 -> "21,000", 1.2345 -> "1.23", 0.01234 -> "0.0123". */
export function fmtSig(x: number, sig = 3): string {
  if (!Number.isFinite(x)) return DASH;
  if (x === 0) return '0';
  const r = roundSig(x, sig);
  const d = Math.ceil(Math.log10(Math.abs(r)));
  const frac = Math.max(0, sig - d);
  return fmtGrouped(r, Math.min(frac, 10));
}

/** Full precision for tables: integers above 100, 2 decimals from 1, 3 significant figures below 1. */
export function fmtFull(x: number): string {
  if (!Number.isFinite(x)) return DASH;
  const a = Math.abs(x);
  if (a === 0) return '0';
  if (a >= 100) return fmtGrouped(x, 0);
  if (a >= 1) return fmtGrouped(x, 2);
  return fmtSig(x, 3);
}

const SI = [
  { v: 1e12, word: 'tn', axis: 'tn' },
  { v: 1e9, word: 'bn', axis: 'bn' },
  { v: 1e6, word: 'm', axis: 'm' },
  { v: 1e3, word: 'k', axis: 'k' },
] as const;

/** The SI step of a USD magnitude (one per axis). */
export function usdScale(maxAbs: number): { v: number; word: string; axis: string } {
  for (const s of SI) if (maxAbs >= s.v) return s;
  return { v: 1, word: '', axis: '' };
}

/** "USD 203 bn" (USD values in dollars). */
export function fmtUsd(x: number, ctx: FormatContext = 'tile'): string {
  if (!Number.isFinite(x)) return DASH;
  const s = usdScale(Math.abs(x));
  const v = x / s.v;
  const num = ctx === 'table' ? (Math.abs(v) >= 100 ? fmtGrouped(v, 1) : fmtGrouped(v, 2)) : fmtSig(v, 3);
  return `USD ${num}${s.word ? ` ${s.word}` : ''}`;
}

/** A share 0-1 as ".1%", with "<0.1%" for tiny positive shares. */
export function fmtShare(x: unknown, digits = 1): string {
  if (!isFiniteNum(x)) return DASH;
  if (x > 0 && x < 0.5 * Math.pow(10, -digits) / 100) return `<${(Math.pow(10, -digits)).toFixed(digits)}%`;
  return `${minus((x * 100).toFixed(digits))}%`;
}

/** HHI .3f; a lower bound prints "≥ 0.21". */
export function fmtHhi(x: unknown, lowerBound = false): string {
  if (!isFiniteNum(x)) return DASH;
  return lowerBound ? `≥ ${x.toFixed(2)}` : x.toFixed(3);
}

/** HHI on the 0-10,000 scale (tooltips). */
export function fmtHhiPoints(x: unknown): string {
  return isFiniteNum(x) ? fmtGrouped(Math.round(x * 10000), 0) : DASH;
}

/** "≈ 4.8" effective number. */
export function fmtEffN(x: unknown): string {
  return isFiniteNum(x) ? (x >= 100 ? fmtGrouped(x, 0) : x.toFixed(1)) : DASH;
}

/** The unit text after a number ("kb/d", "EJ"); empty for unit kinds whose number carries it. */
function unitSuffix(u: UnitSpec): string {
  const s = (u.short ?? '').trim();
  // a scale range ("0-1": the UN-vote axis) is no unit: it is said once in headers and captions, not
  // after every number ("0.265 0-1" read as a range)
  if (/^\d+\s*[-–]\s*\d+$/.test(s)) return '';
  return s ? ` ${s}` : '';
}

/** The number part of a value in a unit (no unit words). */
export function fmtNumber(x: number, kind: string, ctx: FormatContext = 'tile'): string {
  if (!Number.isFinite(x)) return DASH;
  switch (kind) {
    case 'share':
      return fmtShare(x);
    case 'pct':
      return `${minus(x.toFixed(1))}%`;
    case 'hhi':
      return x.toFixed(3);
    case 'count':
      return fmtGrouped(Math.round(x), 0);
    case 'ratio':
      return minus(x.toFixed(2));
    case 'trl':
      return minus(x.toFixed(1));
    case 'ordinal':
      return String(x);
    case 'years':
      return fmtGrouped(x, 0);
    default:
      return ctx === 'table' ? fmtFull(x) : fmtSig(x, 3);
  }
}

/**
 * A value with its unit, after the display scale: fmtValue(202.7e9, {unit_kind: 'usd', short: 'USD'})
 * -> "USD 203 bn". `scale` is the dictionary's display_scale (default 1).
 */
export function fmtValue(v: unknown, unit: UnitSpec, opts: { scale?: number; ctx?: FormatContext; lowerBound?: boolean } = {}): string {
  if (!isFiniteNum(v)) return DASH;
  const x = v * (opts.scale ?? 1);
  const ctx = opts.ctx ?? 'tile';
  const kind = unit.unit_kind;
  switch (kind) {
    case 'usd':
      return fmtUsd(x, ctx);
    case 'usd_m':
      return fmtUsd(x * 1e6, ctx);
    case 'usd_bn_real':
      return `${fmtUsd(x * 1e9, ctx)} (2024$)`;
    case 'usd_month':
      return `USD ${fmtGrouped(x, x >= 100 ? 0 : 2)} a month`;
    case 'share':
      return fmtShare(x);
    case 'pct':
      return `${minus(x.toFixed(1))}%`;
    case 'hhi':
      return fmtHhi(x, opts.lowerBound);
    case 'gj_pc':
      return `${ctx === 'table' ? fmtFull(x) : fmtSig(x, 3)} GJ per person`;
    case 'price': {
      const s = (unit.short ?? '').trim();
      const per = s.replace(/^USD\s*/i, '').replace(/^\//, '').trim();
      const num = Math.abs(x) >= 1000 ? fmtGrouped(x, 0) : Math.abs(x) >= 10 ? fmtGrouped(x, 1) : fmtSig(x, 3);
      return per ? `USD ${num} /${per}` : `USD ${num}`;
    }
    case 'count':
      return fmtGrouped(Math.round(x), 0);
    case 'ratio':
      return `${minus(x.toFixed(2))}${unitSuffix(unit).replace(/ ratio$/i, '')}`.trim();
    case 'trl':
      return `TRL ${minus(x.toFixed(1))}`;
    default: {
      const num = fmtNumber(x, kind, ctx);
      return `${num}${unitSuffix(unit)}`;
    }
  }
}

/** A signed change with an arrow ("▲ 12.4%", "▼ 3.1 EJ", "no change"); share/pct kinds read as percent. */
export function fmtSignedValue(v: unknown, unit: UnitSpec, opts: { scale?: number; ctx?: FormatContext } = {}): string {
  if (!isFiniteNum(v)) return DASH;
  const x = v * (opts.scale ?? 1);
  if (x === 0) return 'no change';
  const body = fmtValue(Math.abs(x), unit, { ...opts, scale: 1 });
  return `${x > 0 ? UP : DOWN} ${body}`;
}

/** A growth rate (fraction) with an arrow: 1.04 -> "▲ +104%". */
export function fmtGrowth(g: unknown, digits = 1): string {
  if (!isFiniteNum(g)) return DASH;
  const r = Number((g * 100).toFixed(digits));
  if (r === 0) return 'no change';
  return `${g > 0 ? `${UP} +` : `${DOWN} ${MINUS}`}${Math.abs(g * 100).toFixed(digits)}%`;
}

/** A plain signed percent of a fraction: "+27.5%", "−3.1%". */
export function fmtSignedPct(g: unknown, digits = 1): string {
  if (!isFiniteNum(g)) return DASH;
  const r = Number((g * 100).toFixed(digits));
  if (r === 0) return (0).toFixed(digits) + '%';
  return `${g > 0 ? '+' : MINUS}${Math.abs(g * 100).toFixed(digits)}%`;
}

/** Ordinal rank "#4". */
export const fmtRank = (r: unknown): string => (isFiniteNum(r) ? `#${r}` : DASH);

/**
 * One tick formatter for a whole axis (3.9: every tick on an axis uses one scale). USD axes pick the
 * SI step from the largest |tick|: 2.03e11 -> "203bn"; shares print as percent.
 */
export function axisFormatter(values: Iterable<number>, unit: UnitSpec, scale = 1): (v: number) => string {
  let maxAbs = 0;
  for (const v of values) if (Number.isFinite(v) && Math.abs(v * scale) > maxAbs) maxAbs = Math.abs(v * scale);
  const kind = unit.unit_kind;
  if (USD_KINDS.has(kind)) {
    const mult = kind === 'usd_m' ? 1e6 : kind === 'usd_bn_real' ? 1e9 : 1;
    const s = usdScale(maxAbs * mult);
    const decimals = (maxAbs * mult) / s.v < 10 ? 1 : 0;
    return (v) => {
      const x = (v * scale * mult) / s.v;
      const t = Math.abs(x - Math.round(x)) < 1e-9 ? String(Math.round(x)) : x.toFixed(decimals);
      return `${minus(t)}${s.axis}`;
    };
  }
  if (kind === 'share') {
    const decimals = maxAbs < 0.02 ? 1 : 0;
    return (v) => `${minus((v * scale * 100).toFixed(decimals))}%`;
  }
  if (kind === 'pct') return (v) => `${minus((v * scale).toFixed(maxAbs < 10 ? 1 : 0))}%`;
  if (kind === 'hhi') return (v) => (v * scale).toFixed(2);
  if (QUANTITY_KINDS.has(kind) || kind === 'gj_pc' || kind === 'price' || kind === 'count') {
    if (maxAbs >= 1e6) return (v) => `${minus(((v * scale) / 1e6).toFixed(maxAbs >= 1e7 ? 0 : 1))}M`;
    if (maxAbs >= 1e4) return (v) => `${minus(((v * scale) / 1e3).toFixed(0))}k`;
    return (v) => fmtGrouped(v * scale, maxAbs < 10 ? 2 : maxAbs < 100 ? 1 : 0);
  }
  return (v) => fmtGrouped(v * scale, maxAbs < 10 ? 2 : 0);
}

/**
 * The tick formatter of a log axis: every tick gets its own SI step (a log axis spans decades, so one
 * step per axis would print "0bn" at the low end): USD 1e10 -> "10bn", 2,000 kt -> "2k", 3 -> "3".
 */
export function logAxisFormatter(unit: UnitSpec, scale = 1): (v: number) => string {
  const kind = unit.unit_kind;
  const mult = kind === 'usd_m' ? 1e6 : kind === 'usd_bn_real' ? 1e9 : 1;
  const steps: [number, string][] = [
    [1e12, 'tn'],
    [1e9, 'bn'],
    [1e6, USD_KINDS.has(kind) ? 'm' : 'M'],
    [1e3, 'k'],
  ];
  return (v) => {
    const x = v * scale * mult;
    if (!Number.isFinite(x)) return '';
    const a = Math.abs(x);
    for (const [st, w] of steps) if (a >= st) return `${minus(String(Number((x / st).toPrecision(3))))}${w}`;
    return minus(String(Number(x.toPrecision(3))));
  };
}

/** Unit words for aria-labels: "US dollars, billions". */
export function unitAria(x: number, unit: UnitSpec, scale = 1): string {
  const kind = unit.unit_kind;
  if (USD_KINDS.has(kind)) {
    const mult = kind === 'usd_m' ? 1e6 : kind === 'usd_bn_real' ? 1e9 : 1;
    const s = usdScale(Math.abs(x * scale * mult));
    const word = s.word === 'tn' ? 'trillions' : s.word === 'bn' ? 'billions' : s.word === 'm' ? 'millions' : s.word === 'k' ? 'thousands' : '';
    return `US dollars${word ? `, ${word}` : ''}${kind === 'usd_bn_real' ? ' (2024 dollars)' : ''}`;
  }
  if (kind === 'share') return 'percent';
  return unit.long ?? unit.short;
}

/** A value spelled for screen readers: 2.027e11 usd -> "203 billion US dollars". */
export function fmtAria(v: unknown, unit: UnitSpec, scale = 1): string {
  if (!isFiniteNum(v)) return 'no value';
  const x = v * scale;
  const kind = unit.unit_kind;
  if (USD_KINDS.has(kind)) {
    const mult = kind === 'usd_m' ? 1e6 : kind === 'usd_bn_real' ? 1e9 : 1;
    const s = usdScale(Math.abs(x * mult));
    const word = s.word === 'tn' ? ' trillion' : s.word === 'bn' ? ' billion' : s.word === 'm' ? ' million' : s.word === 'k' ? ' thousand' : '';
    return `${fmtSig((x * mult) / s.v, 3)}${word} US dollars${kind === 'usd_bn_real' ? ' (2024 dollars)' : ''}`;
  }
  if (kind === 'share') return `${(x * 100).toFixed(1)} percent`;
  if (kind === 'pct') return `${x.toFixed(1)} percent`;
  return `${fmtSig(x, 3)} ${unit.long ?? unit.short}`.trim();
}

/** "1 Jan 2026"-style date of an ISO timestamp (the export date line). */
export function fmtExportDate(iso: unknown): string {
  if (typeof iso !== 'string' || !iso) return DASH;
  const d = new Date(iso);
  if (Number.isNaN(d.getTime())) return iso;
  const m = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'][d.getUTCMonth()];
  return `${d.getUTCDate()} ${m} ${d.getUTCFullYear()}`;
}

/** kB / MB of a byte count (chunk sizes in loading labels). */
export function fmtSize(n: unknown): string {
  if (!isFiniteNum(n)) return DASH;
  if (n >= 1024 * 1024) return `${(n / (1024 * 1024)).toFixed(1)} MB`;
  if (n >= 1024) return `${Math.round(n / 1024)} kB`;
  return `${n} B`;
}

/**
 * A label in running text: capitalised words lower-cased, acronyms and mixed-case units kept
 * ("Mining (USGS)" -> "mining (USGS)", "EV sales" -> "EV sales", "Exports" -> "exports").
 */
export function lc(s: string | null | undefined): string {
  return (s ?? '').replace(/\b([A-Z])([a-z][a-z'’-]*)\b/g, (w: string, a: string, b: string) => (PROPER.has(w) ? w : a.toLowerCase() + b));
}

/** Proper nouns that keep their capital in running text ("China share", not "china share"). */
const PROPER = new Set(['China', 'Chinese', 'Europe', 'European', 'America', 'American', 'Asia', 'Atlas', 'Comtrade', 'Brazil', 'India', 'Japan', 'Korea', 'Russia', 'Africa', 'Pacific']);

/** "a, b and c". */
export function listText(xs: string[]): string {
  if (xs.length <= 1) return xs.join('');
  if (xs.length === 2) return `${xs[0]} and ${xs[1]}`;
  return `${xs.slice(0, -1).join(', ')} and ${xs[xs.length - 1]}`;
}

/** "1st", "2nd", "4th". */
export function ordinal(n: number): string {
  const s = ['th', 'st', 'nd', 'rd'];
  const v = n % 100;
  return `${n}${s[(v - 20) % 10] ?? s[v] ?? s[0]}`;
}
