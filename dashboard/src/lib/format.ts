// Number and text formats for the UI (spec_react.md 8.7): indices .3f, shares .1%, HHI .3f, ranks
// integers, changes signed with an arrow. NA (null / NaN / undefined) prints as an en dash.

export const DASH = '–';
export const MINUS = '−';
export const TIMES = '×';

export const isNum = (x: unknown): x is number => typeof x === 'number' && Number.isFinite(x);

/** Fixed decimals, with a true minus sign. */
export function fmtNum(x: unknown, digits = 3): string {
  if (!isNum(x)) return DASH;
  const s = x.toFixed(digits);
  return s.startsWith('-') ? MINUS + s.slice(1) : s;
}

export function fmtPct(x: unknown, digits = 1): string {
  if (!isNum(x)) return DASH;
  const s = (x * 100).toFixed(digits);
  return (s.startsWith('-') ? MINUS + s.slice(1) : s) + '%';
}

export function fmtInt(x: unknown): string {
  if (!isNum(x)) return DASH;
  return Math.round(x).toLocaleString('en-US');
}

/** A signed change: "+0.012", "−0.040", "0.000". */
export function fmtSigned(x: unknown, digits = 3): string {
  if (!isNum(x)) return DASH;
  const r = Number(x.toFixed(digits));
  if (r === 0) return (0).toFixed(digits);
  return (r > 0 ? '+' : MINUS) + Math.abs(x).toFixed(digits);
}

/** A signed change with a direction arrow (text, so colour is never the only carrier). */
export function fmtDelta(x: unknown, digits = 3, pct = false): string {
  if (!isNum(x)) return DASH;
  const r = Number((pct ? x * 100 : x).toFixed(digits));
  if (r === 0) return `no change`;
  const v = (pct ? Math.abs(x * 100).toFixed(digits) + ' pp' : Math.abs(x).toFixed(digits));
  return r > 0 ? `↑ +${v}` : `↓ ${MINUS}${v}`;
}

/** Rank change where positive = moved up (rank_base - rank). */
export function fmtRankChange(x: unknown): string {
  if (!isNum(x)) return DASH;
  if (x === 0) return 'no change';
  return x > 0 ? `↑ up ${x}` : `↓ down ${-x}`;
}

/** The dial multiplier as "x2.30". */
export function fmtMult(m: number): string {
  return `${TIMES}${m.toFixed(2)}`;
}

export function fmtBytes(n: unknown): string {
  if (!isNum(n)) return DASH;
  if (n >= 1024 * 1024) return `${(n / (1024 * 1024)).toFixed(1)} MB`;
  if (n >= 1024) return `${Math.round(n / 1024)} kB`;
  return `${n} B`;
}

export function fmtMs(ms: unknown): string {
  if (!isNum(ms)) return DASH;
  return ms >= 1000 ? `${(ms / 1000).toFixed(1)} s` : `${Math.round(ms)} ms`;
}

export function fmtDate(iso: unknown): string {
  if (typeof iso !== 'string' || !iso) return DASH;
  const d = new Date(iso);
  if (Number.isNaN(d.getTime())) return iso;
  return d.toISOString().slice(0, 16).replace('T', ' ') + ' UTC';
}

export const yesNo = (x: unknown): string => (x === true ? 'yes' : x === false ? 'no' : DASH);

/** "Batteries – Midstream". */
export const cellLabel = (tech: string, stage: string): string => `${tech} ${DASH} ${stage}`;

/** URL slug of a network stage: "batteries-midstream". */
export const stageSlug = (tech: string, stage: string): string =>
  `${tech}-${stage}`.toLowerCase().replace(/[^a-z0-9]+/g, '-').replace(/^-|-$/g, '');

const STAGE_ORDER = ['Upstream', 'Midstream', 'Downstream'];

/**
 * The options of a sector select, shared by the Strategic Partners view and the Country partners
 * section: grouped by stage (Upstream, Midstream, Downstream), technologies alphabetical within each,
 * labelled cellLabel() ("Solar – Midstream"). `value` gives each option's value.
 */
export function cellOptions<T extends { tech: string }>(
  cells: readonly T[],
  stageOf: (x: T) => string,
  value: (x: T) => string,
): { value: string; label: string; group: string }[] {
  const rank = (s: string) => (STAGE_ORDER.includes(s) ? STAGE_ORDER.indexOf(s) : STAGE_ORDER.length);
  return [...cells]
    .sort((a, z) => rank(stageOf(a)) - rank(stageOf(z)) || a.tech.localeCompare(z.tech, 'en'))
    .map((x) => ({ value: value(x), label: cellLabel(x.tech, stageOf(x)), group: stageOf(x) }));
}

export const tierLabel = (t: unknown): string => (t === 'core' ? 'Core' : t === 'partner' ? 'Partner' : 'Other');
