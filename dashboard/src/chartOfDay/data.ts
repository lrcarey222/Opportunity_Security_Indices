// Chart of the Day data (scripts/chart_of_day/01_chart_of_day.R writes public/data/chart-of-day/): an index of
// every day and one JSON per day holding the chart, its data and how it was picked. The chart and its numbers are
// frozen when the day is generated, so the Explorer and the Datawrapper draft show the same thing.

export type CodFormat = 'usd' | 'pct' | 'pct2' | 'num' | 'usd_mwh' | 'count';

export interface CodIndexDay {
  date: string;
  title: string;
  template: string;
  chart: string;
  iso3: string;
  country: string;
  tech: string;
  stage: string;
  key: string;
  file: string;
}

interface CodBase {
  schema: 'osi-chart-of-day';
  version: number;
  key: string;
  date: string;
  template: string;
  variable: string;
  measure: string;
  tech: string;
  stage: string;
  focus: { iso3: string; name: string };
  title: string;
  subtitle: string;
  notes: string;
  source: string;
  explore: string;
  /** trade period of the data ("12 months to Aug 2026", "2024"); absent for non-trade charts */
  period?: string;
  pick?: { probability: number; pool: number; interest: number };
  datawrapper?: { id: string; edit_url: string; status: string };
}

export interface BarRow { label: string; iso3: string; value: number; highlight: boolean; group?: string }
export interface GroupedRow { category: string; stage: string; series: string; value: number }
export interface LineRow { series: string; iso3: string; x: number; value: number; highlight: boolean; level?: number }
export interface ScatterRow { label: string; iso3: string; x: number; y: number; highlight: boolean; show_label: boolean }
export interface TileRow { row: string; col: string; stage: string; value: number; imports: number }
export interface SeriesRow { x: number; series: string; value: number }
export interface DumbbellRow { label: string; from: number; to: number; tech: string; stage: string; imports: number }

export type CodSpec = CodBase &
  (
    | { chart: { type: 'bar'; format: CodFormat; value_label?: string; color_by?: 'group'; colors?: Record<string, string> }; rows: BarRow[] }
    | { chart: { type: 'grouped_bar'; format: CodFormat; series: string[] }; rows: GroupedRow[] }
    | { chart: { type: 'lines'; format: CodFormat; y_label?: string; level_format?: CodFormat; level_unit?: string }; rows: LineRow[] }
    | { chart: { type: 'stacked_bar'; format: CodFormat; y_label: string; series: string[]; colors: Record<string, string> }; rows: SeriesRow[] }
    | { chart: { type: 'scatter'; format: CodFormat; x_label: string; y_label: string; log: boolean }; rows: ScatterRow[] }
    | { chart: { type: 'tiles'; format: CodFormat; rows_order: string[]; cols_order: string[] }; rows: TileRow[] }
    | { chart: { type: 'dumbbell'; format: CodFormat; from_label: string; to_label: string }; rows: DumbbellRow[] }
  );

function base(): string {
  const b = (import.meta.env?.BASE_URL as string | undefined) ?? '/';
  return `${b.endsWith('/') ? b : `${b}/`}data/chart-of-day/`;
}

async function getJson<T>(file: string): Promise<T> {
  const r = await fetch(base() + file, { cache: 'no-cache' });
  if (!r.ok) throw new Error(`chart of the day: ${file} returned ${r.status}`);
  return (await r.json()) as T;
}

export async function loadIndex(): Promise<CodIndexDay[]> {
  const j = await getJson<{ days: CodIndexDay[] }>('index.json');
  return [...(j.days ?? [])].sort((a, b) => b.date.localeCompare(a.date));
}

export function loadDay(day: CodIndexDay): Promise<CodSpec> {
  return getJson<CodSpec>(day.file);
}

/** Local calendar date as YYYY-MM-DD. */
export function todayIso(now: Date = new Date()): string {
  const p = (n: number) => String(n).padStart(2, '0');
  return `${now.getFullYear()}-${p(now.getMonth() + 1)}-${p(now.getDate())}`;
}

/** Days up to today, newest first: future drafts stay hidden. */
export function publishedDays(days: CodIndexDay[], today: string): CodIndexDay[] {
  return days.filter((d) => d.date <= today);
}

/** The day to show: the requested date if it is published, else the latest published day, else null. */
export function pickDay(days: CodIndexDay[], today: string, requested?: string): CodIndexDay | null {
  const pub = publishedDays(days, today);
  if (requested) return pub.find((d) => d.date === requested) ?? null;
  return pub[0] ?? null;
}

export function fmtValue(v: number, f: CodFormat): string {
  if (!Number.isFinite(v)) return 'n/a';
  switch (f) {
    case 'pct':
      return `${(v * 100).toFixed(v < 0.01 && v > 0 ? 1 : 0)}%`;
    case 'pct2':
      return `${(v * 100).toFixed(2)}%`;
    case 'usd_mwh':
      return `$${Math.round(v)}/MWh`;
    case 'count':
      return Math.round(v).toLocaleString('en-US');
    case 'num':
      return v >= 100 ? Math.round(v).toLocaleString('en-US') : v.toLocaleString('en-US', { maximumFractionDigits: 1 });
    case 'usd': {
      const a = Math.abs(v);
      const s = v < 0 ? '-' : '';
      if (a >= 1e12) return `${s}$${(a / 1e12).toFixed(1)}trn`;
      if (a >= 1e9) return `${s}$${(a / 1e9).toFixed(1)}bn`;
      if (a >= 1e6) return `${s}$${Math.round(a / 1e6)}m`;
      if (a >= 1e3) return `${s}$${Math.round(a / 1e3)}k`;
      return `${s}$${Math.round(a)}`;
    }
  }
}

/** Long date for headers: "Wednesday 18 November 2026". */
export function longDate(iso: string): string {
  const [y, m, d] = iso.split('-').map(Number);
  return new Date(y, m - 1, d).toLocaleDateString('en-GB', { weekday: 'long', day: 'numeric', month: 'long', year: 'numeric' });
}
