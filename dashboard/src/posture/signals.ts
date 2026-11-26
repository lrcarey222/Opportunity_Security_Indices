// Landing signals (scripts/91 posture-signals.<md5>.json, R/dashboard/posture/posture_signals.R): two current-state
// trends in measured trade for Home, read without the posture core. Not changes to the assessment and not a score.

export const POSTURE_SIGNALS_SCHEMA = 'osi-posture-signals';

interface SignalCell {
  iso3: string;
  /** the economy's display name (its ISO3 where none) */
  name: string;
  tech: string;
  stage: string;
  trusted: boolean;
}

export interface ExportSignal extends SignalCell {
  /** exp(log growth) - 1: +0.42 is 42% more exports in the latest 12 months than in the 12 before */
  growth: number;
  log_growth: number;
  world_share: number | null;
}

export interface DependenceSignal extends SignalCell {
  china_base: number | null;
  china_latest: number | null;
  coercive_base: number | null;
  coercive_latest: number | null;
  source: string | null;
}

export interface PostureSignals {
  schema: typeof POSTURE_SIGNALS_SCHEMA;
  schema_version: number;
  created: string;
  exports: {
    period: string | null;
    previous_period: string | null;
    band: number;
    n_assessed: number;
    n_up: number;
    n_down: number;
    n_up_trusted: number;
    n_down_trusted: number;
    growing: ExportSignal[];
    growing_trusted: ExportSignal[];
    falling_trusted: ExportSignal[];
  };
  dependence: {
    window_end: number | null;
    n_refreshed: number;
    n_china_up: number;
    n_china_down: number;
    n_china_up_trusted: number;
    n_china_down_trusted: number;
    rising_trusted: DependenceSignal[];
    rising: DependenceSignal[];
  };
}

const isObj = (x: unknown): x is Record<string, unknown> => typeof x === 'object' && x !== null && !Array.isArray(x);
const arr = <T,>(v: unknown): T[] => (Array.isArray(v) ? (v as T[]) : []);

export function checkPostureSignals(x: unknown): PostureSignals {
  if (!isObj(x) || x.schema !== POSTURE_SIGNALS_SCHEMA || !isObj(x.exports) || !isObj(x.dependence)) {
    throw new Error('The posture signals document is not an osi-posture-signals document.');
  }
  const e = x.exports as Record<string, unknown>;
  const d = x.dependence as Record<string, unknown>;
  for (const k of ['growing', 'growing_trusted', 'falling_trusted']) e[k] = arr(e[k]);
  for (const k of ['rising_trusted', 'rising']) d[k] = arr(d[k]);
  return x as unknown as PostureSignals;
}

/** "+42%" */
export const pctChange = (v: number): string => `${v >= 0 ? '+' : ''}${Math.round(v * 100)}%`;
/** "31%" */
export const pct = (v: number | null): string => (v === null || v === undefined ? 'n/a' : `${Math.round(v * 100)}%`);
