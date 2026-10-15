// Draw the Fence: the assessment state and its URL form. Pure.
//
// The whole assessment (what is assessed, every answer, scenario assumptions, strategies and the
// policy package) travels in one query parameter, `a`: compact JSON, base64url-encoded. A copied
// link reproduces the assessment exactly; nothing is stored anywhere else.

import { ALL_INPUT_IDS, INPUT_DEFS, isNumericId, POLICIES } from '../engine/index.ts';
import { DOMAINS } from '../engine/types.ts';
import type { CategoricalInputId, InputId, Intensity, PolicyId, Strategy } from '../engine/index.ts';
import type { Stage } from '../data/contract.ts';

export type Mode = 'analyst' | 'quick';
export type Answers = Partial<Record<InputId, string | number>>;
export type PremiumWhy = 'security' | 'learning' | 'diversification' | 'jobs' | 'none';

export interface PolicyState {
  levels: Partial<Record<PolicyId, Intensity>>;
  scope: 'control-layer' | 'whole-product';
}

export interface LayerState {
  /** the reader's answers (YOUR JUDGMENT); the value 'unknown' records an explicit Unknown */
  answers: Answers;
  /** scenario-lab assumptions (SCENARIO), never part of the observed baseline */
  scenario: Answers;
  strategies: Strategy[];
  why?: PremiumWhy;
  policy?: PolicyState;
}

export interface Selection {
  tech: string;
  stage: Stage;
  /** a curated component id */
  component?: string;
  /** a curated multi-layer system id */
  stack?: string;
}

export interface AssessState {
  country: string;
  sel: Selection | null;
  /** the layer being worked on (a component id, or a stageKey()) */
  focus?: string;
  mode: Mode;
  layers: Record<string, LayerState>;
  step: number;
  example?: string;
}

/** The prefix of a whole-stage assessment's key (no component chosen). */
export const STAGE_KEY = '_stage';

/** Each technology-stage keeps its own whole-stage answers: switching stage never carries them over. */
export const stageKey = (tech: string, stage: string): string => `${STAGE_KEY}:${tech.toLowerCase().replace(/\s+/g, '-')}-${stage.toLowerCase()}`;
export const STAGES: Stage[] = ['Upstream', 'Midstream', 'Downstream'];

export const emptyLayer = (): LayerState => ({ answers: {}, scenario: {}, strategies: [] });

export function initialState(country = 'USA'): AssessState {
  return { country, sel: null, mode: 'analyst', layers: {}, step: 0 };
}

export function layerOf(s: AssessState, key: string): LayerState {
  return s.layers[key] ?? emptyLayer();
}

export function withLayer(s: AssessState, key: string, f: (l: LayerState) => LayerState): AssessState {
  return { ...s, layers: { ...s.layers, [key]: f(layerOf(s, key)) } };
}

// ── URL codec ────────────────────────────────────────────────────────────────────────────────────

const STRAT_CODE: Record<Strategy, string> = { preserve: 'p', catchUp: 'c', leapAhead: 'l' };
const STRAT_FROM: Record<string, Strategy> = { p: 'preserve', c: 'catchUp', l: 'leapAhead' };
const WHY: PremiumWhy[] = ['security', 'learning', 'diversification', 'jobs', 'none'];
const POLICY_IDS = POLICIES.map((p) => p.id);

function b64urlEncode(text: string): string {
  const bytes = new TextEncoder().encode(text);
  let bin = '';
  for (const b of bytes) bin += String.fromCharCode(b);
  return btoa(bin).replace(/\+/g, '-').replace(/\//g, '_').replace(/=+$/, '');
}

function b64urlDecode(s: string): string {
  const b64 = s.replace(/-/g, '+').replace(/_/g, '/') + '==='.slice((s.length + 3) % 4);
  const bin = atob(b64);
  const bytes = Uint8Array.from(bin, (c) => c.charCodeAt(0));
  return new TextDecoder().decode(bytes);
}

function cleanAnswers(x: unknown): Answers {
  const out: Answers = {};
  if (!x || typeof x !== 'object') return out;
  for (const [k, v] of Object.entries(x as Record<string, unknown>)) {
    if (!(ALL_INPUT_IDS as string[]).includes(k)) continue;
    const id = k as InputId;
    if (v === 'unknown') out[id] = 'unknown';
    else if (isNumericId(id)) {
      const nm = INPUT_DEFS[id].numeric;
      if (typeof v === 'number' && Number.isFinite(v) && (!nm || (v >= nm.min && v <= nm.max))) out[id] = v;
    } else if (typeof v === 'string' && (DOMAINS[id as CategoricalInputId] as readonly string[]).includes(v)) out[id] = v;
  }
  return out;
}

function compactAnswers(a: Answers): Answers | undefined {
  const keys = Object.keys(a) as InputId[];
  return keys.length ? a : undefined;
}

export function encodeState(s: AssessState): string {
  const L: Record<string, unknown> = {};
  for (const [k, l] of Object.entries(s.layers)) {
    const o: Record<string, unknown> = {};
    const a = compactAnswers(l.answers);
    const z = compactAnswers(l.scenario);
    if (a) o.a = a;
    if (z) o.z = z;
    if (l.strategies.length) o.g = l.strategies.map((x) => STRAT_CODE[x]).join('');
    if (l.why) o.w = l.why;
    if (l.policy) {
      const lv = Object.fromEntries(Object.entries(l.policy.levels).filter(([, v]) => v));
      if (Object.keys(lv).length || l.policy.scope !== 'control-layer') o.p = { l: lv, s: l.policy.scope === 'whole-product' ? 'w' : 'c' };
    }
    if (Object.keys(o).length) L[k] = o;
  }
  const c: Record<string, unknown> = { v: 1, c: s.country, m: s.mode === 'quick' ? 'q' : 'a', n: s.step };
  if (s.sel) {
    c.t = s.sel.tech;
    c.s = s.sel.stage;
    if (s.sel.component) c.k = s.sel.component;
    if (s.sel.stack) c.x = s.sel.stack;
  }
  if (s.focus) c.f = s.focus;
  if (s.example) c.e = s.example;
  if (Object.keys(L).length) c.L = L;
  return b64urlEncode(JSON.stringify(c));
}

/** Decode the `a` parameter; anything malformed or unknown is dropped, never guessed. */
export function decodeState(param: string | null | undefined): AssessState {
  const s = initialState();
  if (!param) return s;
  let c: Record<string, unknown>;
  try {
    c = JSON.parse(b64urlDecode(param)) as Record<string, unknown>;
  } catch {
    return s;
  }
  if (!c || typeof c !== 'object' || c.v !== 1) return s;
  if (typeof c.c === 'string' && /^[A-Z]{3}$/.test(c.c)) s.country = c.c;
  s.mode = c.m === 'q' ? 'quick' : 'analyst';
  if (typeof c.n === 'number' && Number.isInteger(c.n) && c.n >= 0 && c.n < 20) s.step = c.n;
  if (typeof c.t === 'string' && typeof c.s === 'string' && (STAGES as string[]).includes(c.s)) {
    s.sel = { tech: c.t, stage: c.s as Stage };
    if (typeof c.k === 'string') s.sel.component = c.k;
    if (typeof c.x === 'string') s.sel.stack = c.x;
  }
  if (typeof c.f === 'string') s.focus = c.f;
  if (typeof c.e === 'string') s.example = c.e;
  if (c.L && typeof c.L === 'object') {
    for (const [k, raw] of Object.entries(c.L as Record<string, Record<string, unknown>>)) {
      if (!raw || typeof raw !== 'object') continue;
      const l = emptyLayer();
      l.answers = cleanAnswers(raw.a);
      l.scenario = cleanAnswers(raw.z);
      if (typeof raw.g === 'string') l.strategies = [...new Set([...raw.g].map((ch) => STRAT_FROM[ch]).filter(Boolean))];
      if (typeof raw.w === 'string' && (WHY as string[]).includes(raw.w)) l.why = raw.w as PremiumWhy;
      const p = raw.p as { l?: Record<string, unknown>; s?: string } | undefined;
      if (p && typeof p === 'object') {
        const levels: PolicyState['levels'] = {};
        for (const [pid, v] of Object.entries(p.l ?? {}))
          if ((POLICY_IDS as string[]).includes(pid) && (v === 1 || v === 2 || v === 3)) levels[pid as PolicyId] = v;
        l.policy = { levels, scope: p.s === 'w' ? 'whole-product' : 'control-layer' };
      }
      s.layers[k] = l;
    }
  }
  return s;
}

/** The search string for a state ("?a=..."), or "" for the empty state. */
export function stateSearch(s: AssessState): string {
  if (!s.sel && Object.keys(s.layers).length === 0) return '';
  return `?a=${encodeState(s)}`;
}
