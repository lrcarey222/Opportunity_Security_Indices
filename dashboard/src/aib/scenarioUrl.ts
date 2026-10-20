// The AIB scenario toggle in the URL (2026-10-19): the key `scn`. Absent: Decoupling (the default, so it is
// never written); `scn=managed`: Managed Interdependence. Any other value (baseline, decoupling, a typo) is
// ignored and dropped. Baseline is never a toggle choice: it is the reference every comparison shows.
//
// The scenario lives in the URL, not in React state: /aib and /compacts mount separate AibLayout instances.
// Every AIB link carries it (withScenario), and the hosts that embed Strategic views keep it as a host key
// (the Partnerships page's PARTNERSHIP_KEYS, the Country profile's COUNTRY_KEY_ORDER).
//
// Pure, no imports: the shell (Header, lastLocation) reads it without pulling the AIB module into the entry.

/** The query key of the scenario toggle. */
export const SCENARIO_KEY = 'scn';

/** The toggle's two choices, in the order the control shows them (the default first). */
export const SCENARIO_TOGGLES = ['decoupling', 'managed'] as const;
export type AibToggle = (typeof SCENARIO_TOGGLES)[number];
export const DEFAULT_TOGGLE: AibToggle = 'decoupling';

const params = (search: string | URLSearchParams): URLSearchParams =>
  typeof search === 'string' ? new URLSearchParams(search.startsWith('?') ? search.slice(1) : search) : search;

/** The toggled scenario of a query string ('decoupling' unless scn=managed). */
export function scenarioOfSearch(search: string | URLSearchParams): AibToggle {
  return params(search).get(SCENARIO_KEY)?.trim() === 'managed' ? 'managed' : 'decoupling';
}

/** The query has an scn the app does not write (any value but "managed", or a repeat): the URL should drop it. */
export function hasStrayScenario(search: string | URLSearchParams): boolean {
  const all = params(search).getAll(SCENARIO_KEY);
  return all.length > 1 || (all.length === 1 && all[0] !== 'managed');
}

/** The value written for a choice (null: the default, omitted). */
export const scenarioParam = (s: AibToggle): string | null => (s === 'managed' ? 'managed' : null);

/** A query string ('' or '?...') with scn set for `s` (removed for the default), the other keys in their order. */
export function setScenarioSearch(search: string, s: AibToggle): string {
  const q = params(search);
  const v = scenarioParam(s);
  q.delete(SCENARIO_KEY);
  if (v) q.append(SCENARIO_KEY, v);
  const t = q.toString();
  return t ? `?${t}` : '';
}

/**
 * An href ("/compacts/AIB-C01", "/aib/roles#role-USA", "/aib/partnerships?mode=investment") with the
 * scenario carried: scn=managed added (replacing any scn it had), or removed for Decoupling. The hash
 * stays last. `s` is the choice or a query string to read it from.
 */
export function withScenario(href: string, s: AibToggle | string): string {
  const choice: AibToggle = s === 'managed' || s === 'decoupling' ? s : scenarioOfSearch(s);
  const h = href.indexOf('#');
  const hash = h < 0 ? '' : href.slice(h);
  const rest = h < 0 ? href : href.slice(0, h);
  const i = rest.indexOf('?');
  const path = i < 0 ? rest : rest.slice(0, i);
  return `${path}${setScenarioSearch(i < 0 ? '' : rest.slice(i), choice)}${hash}`;
}

/** The scenario part of a query string to carry to another AIB view ('?scn=managed' or ''). */
export const scenarioCarry = (search: string): string => setScenarioSearch('', scenarioOfSearch(search));
