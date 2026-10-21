// The AIB scenario toggle in the URL (2026-10-19; default changed 2026-10-20): the key `scn`. Absent: Managed
// Interdependence (the default institutional lens, so it is never written); `scn=decoupling`: Decoupling (the
// stress test). Any other value (baseline, managed, a typo, a repeat) is ignored and dropped; a link written
// before 2026-10-20 with scn=managed therefore still opens Managed Interdependence. Baseline is never a toggle
// choice: it is the reference every comparison shows.
//
// The scenario lives in the URL, not in React state: /aib and /compacts mount separate AibLayout instances.
// Every AIB link carries it (withScenario), and the hosts that embed Strategic views keep it as a host key
// (the Partnerships page's PARTNERSHIP_KEYS, the Country profile's COUNTRY_KEY_ORDER).
//
// Pure, no imports: the shell (Header, lastLocation) reads it without pulling the AIB module into the entry.

/** The query key of the scenario toggle. */
export const SCENARIO_KEY = 'scn';

/** The toggle's two choices, in the order the control shows them (the default first). */
export const SCENARIO_TOGGLES = ['managed', 'decoupling'] as const;
export type AibToggle = (typeof SCENARIO_TOGGLES)[number];
/** The default choice: Managed Interdependence (never written to the URL). */
export const DEFAULT_TOGGLE: AibToggle = 'managed';
/** The one value the app writes: the choice that is not the default. */
const WRITTEN: AibToggle = 'decoupling';

const params = (search: string | URLSearchParams): URLSearchParams =>
  typeof search === 'string' ? new URLSearchParams(search.startsWith('?') ? search.slice(1) : search) : search;

/** The toggled scenario of a query string ('managed' unless scn=decoupling). */
export function scenarioOfSearch(search: string | URLSearchParams): AibToggle {
  return params(search).get(SCENARIO_KEY)?.trim() === WRITTEN ? WRITTEN : DEFAULT_TOGGLE;
}

/** The query has an scn the app does not write (any value but "decoupling", or a repeat): the URL should drop it. */
export function hasStrayScenario(search: string | URLSearchParams): boolean {
  const all = params(search).getAll(SCENARIO_KEY);
  return all.length > 1 || (all.length === 1 && all[0] !== WRITTEN);
}

/** The query carries any scn at all (a bundle without scenarios drops it). */
export const hasScenarioKey = (search: string | URLSearchParams): boolean => params(search).has(SCENARIO_KEY);

/** The value written for a choice (null: the default, omitted). */
export const scenarioParam = (s: AibToggle): string | null => (s === WRITTEN ? WRITTEN : null);

/** A query string ('' or '?...') with scn set for `s` (removed for the default), the other keys in their order. */
export function setScenarioSearch(search: string, s: AibToggle): string {
  const q = params(search);
  const v = scenarioParam(s);
  q.delete(SCENARIO_KEY);
  if (v) q.append(SCENARIO_KEY, v);
  const t = q.toString();
  return t ? `?${t}` : '';
}

/** A query string without any scn (the other keys in their order). */
export const clearScenarioSearch = (search: string): string => setScenarioSearch(search, DEFAULT_TOGGLE);

/**
 * An href ("/compacts/AIB-C01", "/aib/roles#role-USA", "/aib/partnerships?mode=investment") with the
 * scenario carried: scn=decoupling added (replacing any scn it had), or removed for Managed
 * Interdependence (the default). The hash stays last. `s` is the choice or a query string to read it from.
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

/** The scenario part of a query string to carry to another AIB view ('?scn=decoupling' or ''). */
export const scenarioCarry = (search: string): string => setScenarioSearch('', scenarioOfSearch(search));
