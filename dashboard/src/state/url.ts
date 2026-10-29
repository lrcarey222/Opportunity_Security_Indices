// URL state (spec_react.md 8.2). The query string is the single source of truth for the scenario
// (dials + advanced settings) and the view settings. Defaults are omitted, so the NEIS baseline URL
// is bare and canonical; keys are written in one fixed order. Pure functions (unit-tested).

import type { Advanced, Dials, Fossil, PsiView, ScenarioSpec } from '../engine/types.ts';
import { DIAL_NAMES } from '../engine/types.ts';
import { isLegacyStage, legacyStageNote, legacyStageTargetName } from '../lib/stages.ts';

export type ShowMode = 'scenario' | 'baseline' | 'change';
export type Metric = 'es' | 'eo' | 'si_rank' | 'si' | 'policy' | 'psi';

export interface ViewState {
  show: ShowMode;
  tech: string | null; // null = the view's default
  stage: string | null;
  metric: Metric;
  rankable: boolean;
  partner: string | null;
}

/** The full scenario the UI holds (every dial, every advanced setting). */
export interface Scenario {
  dials: Dials;
  advanced: Advanced;
}

export interface UrlCtx {
  esCategories: string[];
  eoCategories: string[];
  /** the baseline advanced settings (defaultAdvanced(bundle)) */
  defaults: Advanced;
  techs: string[];
  stages: string[];
}

export interface Parsed {
  scenario: Scenario;
  view: ViewState;
  problems: string[];
}

export const DIAL_KEYS: Record<keyof Dials, string> = { hawkishness: 'h', bullishness: 'b', populism: 'p', greenness: 'g' };
export const METRICS: Metric[] = ['es', 'eo', 'si_rank', 'si', 'policy', 'psi'];
export const SHOW_MODES: ShowMode[] = ['scenario', 'baseline', 'change'];

/** Every key the app writes, in the order it writes them. */
export const KEY_ORDER = [
  'h', 'b', 'p', 'g', 'es', 'eo', 'tau', 'fossil', 'screen', 'psi',
  'show', 'tech', 'stage', 'metric', 'rankable', 'partner',
] as const;
/** Keys of the retired allied network design (its settings and its comparison mode, 2026-10-17): an
 *  old link that carries them still opens, with the keys dropped and named in `problems`. */
export const RETIRED_KEYS = ['lambda', 'maxshare', 'minprod', 'caps', 'cmp'] as const;
export const SCENARIO_KEYS = KEY_ORDER.slice(0, KEY_ORDER.indexOf('show'));
/** View keys kept when navigating between views (tech / stage are shared by overview, space, country). */
export const SHARED_VIEW_KEYS = ['show', 'tech', 'stage'];

export const VIEW_DEFAULTS: ViewState = {
  show: 'scenario',
  tech: null,
  stage: null,
  metric: 'es',
  rankable: true,
  partner: null,
};

export function defaultDials(): Dials {
  return { hawkishness: 50, bullishness: 50, populism: 50, greenness: 50 };
}

export function defaultScenario(ctx: UrlCtx): Scenario {
  return { dials: defaultDials(), advanced: cloneAdvanced(ctx.defaults) };
}

export function cloneAdvanced(a: Advanced): Advanced {
  return { ...a, es_weights: { ...a.es_weights }, eo_weights: { ...a.eo_weights } };
}

const DECIMAL = /^-?(\d+(\.\d*)?|\.\d+)(e-?\d+)?$/i;

/** A decimal parsed from its text (never built by arithmetic), so it is the same double as R's literal. */
function parseDecimal(s: string): number | null {
  const t = s.trim();
  if (!DECIMAL.test(t)) return null;
  const v = Number(t);
  return Number.isFinite(v) ? v : null;
}

const numText = (x: number): string => String(x);

function sameWeights(a: Record<string, number>, b: Record<string, number>, cats: string[]): boolean {
  return cats.every((k) => a[k] === b[k]);
}

/** Read a query string. Invalid or unknown values are dropped and listed in `problems`. */
export function parseSearch(search: string, ctx: UrlCtx): Parsed {
  const q = new URLSearchParams(search.startsWith('?') ? search.slice(1) : search);
  const problems: string[] = [];
  const scenario = defaultScenario(ctx);
  const view: ViewState = { ...VIEW_DEFAULTS };
  const seen = new Set<string>();
  const bad = (k: string, v: string, why: string) => problems.push(`${k}=${v} (${why})`);

  for (const [k, raw] of q.entries()) {
    if (seen.has(k)) {
      bad(k, raw, 'repeated; the first value is used');
      continue;
    }
    seen.add(k);
    const v = raw.trim();
    switch (k) {
      case 'h':
      case 'b':
      case 'p':
      case 'g': {
        const name = DIAL_NAMES.find((d) => DIAL_KEYS[d] === k)!;
        const x = parseDecimal(v);
        if (x === null) {
          bad(k, raw, 'not a number');
          break;
        }
        const c = Math.min(100, Math.max(0, Math.round(x)));
        if (c !== x) bad(k, raw, x < 0 || x > 100 ? `out of range, set to ${c}` : `rounded to ${c}`);
        scenario.dials[name] = c;
        break;
      }
      case 'es':
      case 'eo': {
        const cats = k === 'es' ? ctx.esCategories : ctx.eoCategories;
        const parts = v.split(',');
        const xs = parts.map(parseDecimal);
        if (parts.length !== cats.length || xs.some((x) => x === null || x < 0)) {
          bad(k, raw, `needs ${cats.length} non-negative weights`);
          break;
        }
        if (!((xs as number[]).reduce((a, x) => a + x, 0) > 0)) {
          bad(k, raw, 'at least one weight must be positive');
          break;
        }
        const w: Record<string, number> = {};
        cats.forEach((c, i) => (w[c] = xs[i] as number));
        if (k === 'es') scenario.advanced.es_weights = w;
        else scenario.advanced.eo_weights = w;
        break;
      }
      case 'tau': {
        if (v === 'off') scenario.advanced.tau = null;
        else {
          const x = parseDecimal(v);
          if (x === null || x < 0 || x > 1) bad(k, raw, "'off' or a number from 0 to 1");
          else scenario.advanced.tau = x;
        }
        break;
      }
      case 'fossil':
        if (v === 'auto' || v === 'never' || v === 'always') scenario.advanced.fossil = v as Fossil;
        else bad(k, raw, 'auto, never or always');
        break;
      case 'screen':
        // the endowment screen of the allied picks (advanced require_resource_base)
        if (v === '0' || v === '1') scenario.advanced.require_resource_base = v === '1';
        else bad(k, raw, '0 or 1');
        break;
      case 'psi':
        if (v === 'coalition' || v === 'world') scenario.advanced.psi_view = v as PsiView;
        else bad(k, raw, 'coalition or world');
        break;
      case 'lambda':
      case 'maxshare':
      case 'minprod':
      case 'caps':
      case 'cmp':
        problems.push(`${k} (a setting of the original allied network design, which is retired)`);
        break;
      case 'show':
        if ((SHOW_MODES as string[]).includes(v)) view.show = v as ShowMode;
        else bad(k, raw, 'scenario, baseline or change');
        break;
      case 'tech':
        if (ctx.techs.includes(v)) view.tech = v;
        else bad(k, raw, 'not a technology in this data build');
        break;
      case 'stage':
        if (ctx.stages.includes(v)) view.stage = v;
        else if (isLegacyStage(v)) {
          // an old three-stage link (design U1): Downstream is Deployment exactly; Upstream and Midstream
          // split into several stages, so the stage is dropped (the view shows its default)
          const to = legacyStageTargetName(v);
          const note = legacyStageNote(v).replace(/\.$/, '');
          if (to && ctx.stages.includes(to)) {
            view.stage = to;
            bad(k, raw, `${note}; showing ${to}`);
          } else bad(k, raw, `${note}; the stage was dropped`);
        } else bad(k, raw, 'not a supply-chain stage');
        break;
      case 'metric':
        if ((METRICS as string[]).includes(v)) view.metric = v as Metric;
        else bad(k, raw, 'not a metric');
        break;
      case 'rankable':
        if (v === '0' || v === '1') view.rankable = v === '1';
        else bad(k, raw, '0 or 1');
        break;
      case 'partner':
        if (/^[A-Z]{3}$/.test(v)) view.partner = v;
        else bad(k, raw, 'an ISO3 code');
        break;
      default:
        problems.push(`${k} (unknown setting)`);
    }
  }
  return { scenario, view, problems };
}

/** Only the keys that differ from the defaults, in KEY_ORDER. */
export function searchEntries(scenario: Scenario, view: ViewState, ctx: UrlCtx): [string, string][] {
  const out: [string, string][] = [];
  const d = ctx.defaults;
  for (const name of DIAL_NAMES) if (scenario.dials[name] !== 50) out.push([DIAL_KEYS[name], numText(scenario.dials[name])]);
  const a = scenario.advanced;
  if (!sameWeights(a.es_weights, d.es_weights, ctx.esCategories)) out.push(['es', ctx.esCategories.map((c) => numText(a.es_weights[c])).join(',')]);
  if (!sameWeights(a.eo_weights, d.eo_weights, ctx.eoCategories)) out.push(['eo', ctx.eoCategories.map((c) => numText(a.eo_weights[c])).join(',')]);
  if (a.tau !== d.tau) out.push(['tau', a.tau === null ? 'off' : numText(a.tau)]);
  if (a.fossil !== d.fossil) out.push(['fossil', a.fossil]);
  if (a.require_resource_base !== d.require_resource_base) out.push(['screen', a.require_resource_base ? '1' : '0']);
  if (a.psi_view !== d.psi_view) out.push(['psi', a.psi_view]);
  if (view.show !== VIEW_DEFAULTS.show) out.push(['show', view.show]);
  if (view.tech !== null) out.push(['tech', view.tech]);
  if (view.stage !== null) out.push(['stage', view.stage]);
  if (view.metric !== VIEW_DEFAULTS.metric) out.push(['metric', view.metric]);
  if (view.rankable !== VIEW_DEFAULTS.rankable) out.push(['rankable', view.rankable ? '1' : '0']);
  if (view.partner !== null) out.push(['partner', view.partner]);
  return out;
}

/** The canonical query string ('' or '?h=80&...'). */
export function writeSearch(scenario: Scenario, view: ViewState, ctx: UrlCtx): string {
  const e = searchEntries(scenario, view, ctx);
  if (!e.length) return '';
  const q = new URLSearchParams();
  for (const [k, v] of e) q.append(k, v);
  return '?' + q.toString();
}

/** The scenario part of the query string: the identity of a scenario (cache key of results). */
export function scenarioKey(scenario: Scenario, ctx: UrlCtx): string {
  return writeSearch(scenario, VIEW_DEFAULTS, ctx);
}

/** The query string to carry to another view: scenario keys plus the shared view keys. */
export function navSearch(scenario: Scenario, view: ViewState, ctx: UrlCtx): string {
  return writeSearch(scenario, { ...VIEW_DEFAULTS, show: view.show, tech: view.tech, stage: view.stage }, ctx);
}

/**
 * Split a query string for a page that hosts Strategic views (IA refactor, 2026-10-16): `own` is the
 * query without the host's keys (what the Strategic parser reads), `host` the host's [key, value]
 * pairs in their order (kept when the Strategic setters rewrite the URL). With no keys, `own` is the
 * input unchanged.
 */
export function splitSearch(search: string, hostKeys: readonly string[]): { own: string; host: [string, string][] } {
  if (!hostKeys.length) return { own: search, host: [] };
  const q = new URLSearchParams(search.startsWith('?') ? search.slice(1) : search);
  const own = new URLSearchParams();
  const host: [string, string][] = [];
  for (const [k, v] of q) {
    if (hostKeys.includes(k)) host.push([k, v]);
    else own.append(k, v);
  }
  const s = own.toString();
  return { own: s ? `?${s}` : '', host };
}

/** `search` ('' or '?...') with `extra` pairs appended after its own keys. */
export function appendSearch(search: string, extra: readonly (readonly [string, string])[]): string {
  if (!extra.length) return search;
  const q = new URLSearchParams(search.startsWith('?') ? search.slice(1) : search);
  for (const [k, v] of extra) q.append(k, v);
  const s = q.toString();
  return s ? `?${s}` : '';
}

/** The Strategic scenario pairs of a query string (dials and advanced keys), for a host page to carry on its own writes. */
export function scenarioPairs(search: string): [string, string][] {
  const q = new URLSearchParams(search.startsWith('?') ? search.slice(1) : search);
  return [...q].filter(([k]) => (SCENARIO_KEYS as readonly string[]).includes(k));
}

/** The scenario as the engine reads it: every dial, and the advanced settings. */
export function toSpec(s: Scenario): ScenarioSpec {
  return { dials: { ...s.dials }, advanced: cloneAdvanced(s.advanced) };
}

export function isDefaultScenario(s: Scenario, ctx: UrlCtx): boolean {
  return scenarioKey(s, ctx) === '';
}

/** Which advanced groups differ from the baseline settings. */
export function advancedChanged(a: Advanced, ctx: UrlCtx): { es: boolean; eo: boolean; sample: boolean; fossil: boolean; screen: boolean; psi: boolean; any: boolean } {
  const d = ctx.defaults;
  const es = !sameWeights(a.es_weights, d.es_weights, ctx.esCategories);
  const eo = !sameWeights(a.eo_weights, d.eo_weights, ctx.eoCategories);
  const sample = a.tau !== d.tau;
  const fossil = a.fossil !== d.fossil;
  const screen = a.require_resource_base !== d.require_resource_base;
  const psi = a.psi_view !== d.psi_view;
  return { es, eo, sample, fossil, screen, psi, any: es || eo || sample || fossil || screen || psi };
}
