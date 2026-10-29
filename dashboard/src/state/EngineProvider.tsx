// The engine, the URL scenario and the runs (spec_react.md 8.3; mounted by src/shell/StrategicLayout.tsx).
//
//   EngineProvider    one EngineClient for the page (module worker); its store (status, manifest,
//                     dims, shipped baseline, chunk states, self-check) through useSyncExternalStore,
//                     and the derived AppData once the core data are ready.
//   ScenarioProvider  (inside the router) the scenario and view state parsed from the query string,
//                     with setters that write the URL (dial drags replace the history entry; discrete
//                     actions push).
//   RunProvider       one debounced run with the union of what the mounted views need (core always;
//                     partner / contributions on demand); results per layer keyed by the
//                     scenario, so a view keeps its previous frame while a new result computes.

import { createContext, useCallback, useContext, useEffect, useMemo, useRef, useState, useSyncExternalStore, type ReactNode } from 'react';
import { useLocation, useNavigate } from 'react-router';
import { EngineClient, type EngineFailure, type EngineState, type ProgressMessage } from '../worker/client.ts';
import type { CorePayload, PartnerPayload } from '../worker/protocol.ts';
import type { Advanced, Dials } from '../engine/types.ts';
import { buildAppData, type AppData } from './appData.ts';
import {
  advancedChanged, appendSearch, cloneAdvanced, defaultScenario, navSearch, parseSearch, scenarioKey, splitSearch, toSpec, writeSearch,
  type Scenario, type ShowMode, type ViewState,
} from './url.ts';

// ---- engine -----------------------------------------------------------------------------------------

interface EngineCtx {
  client: EngineClient;
  state: EngineState;
  app: AppData | null;
}

const EngineContext = createContext<EngineCtx | null>(null);

let sharedClient: EngineClient | null = null;
function getSharedClient(): EngineClient {
  // one worker for the page, created outside React so StrictMode's double mount cannot kill it; the
  // provider is mounted by StrategicLayout only (spec_sector.md 2.5), so the worker starts on the
  // first Strategic visit (or a prefetch) and survives module switches
  if (!sharedClient) sharedClient = EngineClient.create();
  return sharedClient;
}

/** The page's engine client, created on first call (Home and the STRATEGIC tab prefetch with it). */
export function ensureEngineClient(): EngineClient {
  return getSharedClient();
}

/** The page's engine client if one has been created, else null (nothing is started). */
export function peekEngineClient(): EngineClient | null {
  return sharedClient;
}

/** Make `client` the page's engine client (tests inject an in-process engine); null forgets it. */
export function setEngineClient(client: EngineClient | null): void {
  sharedClient = client;
}

export function EngineProvider({ client: injected, children }: { client?: EngineClient; children: ReactNode }) {
  const client = useMemo(() => injected ?? getSharedClient(), [injected]);
  const state = useSyncExternalStore(client.subscribe, client.getSnapshot);
  const app = useMemo(
    () => (state.status === 'ready' && state.manifest && state.dims && state.baseline ? buildAppData(state.manifest, state.dims, state.baseline) : null),
    [state.status, state.manifest, state.dims, state.baseline],
  );
  const value = useMemo(() => ({ client, state, app }), [client, state, app]);
  return <EngineContext.Provider value={value}>{children}</EngineContext.Provider>;
}

export function useEngine(): EngineCtx {
  const v = useContext(EngineContext);
  if (!v) throw new Error('useEngine outside EngineProvider');
  return v;
}

/** The loaded build (only under a ready engine). */
export function useApp(): AppData {
  const { app } = useEngine();
  if (!app) throw new Error('useApp before the engine is ready');
  return app;
}

// ---- scenario (URL) ---------------------------------------------------------------------------------

interface ScenarioCtx {
  scenario: Scenario;
  view: ViewState;
  /** scenario part of the query string ('' at the NEIS baseline) */
  key: string;
  isBaseline: boolean;
  /** the Show mode in effect (always 'scenario' at the baseline) */
  show: ShowMode;
  problems: string[];
  dismissProblems: () => void;
  setDial: (name: keyof Dials, value: number, opts?: { replace?: boolean }) => void;
  setDials: (dials: Dials) => void;
  setAdvanced: (f: (a: Advanced) => Advanced, opts?: { replace?: boolean }) => void;
  setScenario: (s: Scenario) => void;
  reset: () => void;
  setView: (patch: Partial<ViewState>, opts?: { replace?: boolean }) => void;
  /** the query string to carry when navigating to another view */
  navQuery: string;
}

const ScenarioContext = createContext<ScenarioCtx | null>(null);

/**
 * `hostKeys` (IA refactor, 2026-10-16): when Strategic views are embedded in another module's page
 * (shell/EngineScope.tsx), the query keys that page owns (e.g. the Country profile's cell, partner,
 * all). The Strategic parser never sees them (no "unknown setting" problem) and every URL write keeps
 * them, and the hash. Without it the provider behaves exactly as before.
 */
export function ScenarioProvider({ children, hostKeys }: { children: ReactNode; hostKeys?: readonly string[] }) {
  const app = useApp();
  const ctx = app.urlCtx;
  const loc = useLocation();
  const navigate = useNavigate();
  const hostKey = hostKeys?.join('|') ?? '';
  const split = useMemo(() => splitSearch(loc.search, hostKey ? hostKey.split('|') : []), [loc.search, hostKey]);
  const parsed = useMemo(() => parseSearch(split.own, ctx), [split.own, ctx]);
  const latest = useRef({ parsed, pathname: loc.pathname, host: split.host, hash: loc.hash });
  latest.current = { parsed, pathname: loc.pathname, host: split.host, hash: loc.hash };
  const [problems, setProblems] = useState<string[]>([]);
  // the location to write: the host's keys and the hash travel only when there is a host
  const to = useCallback(
    (search: string) =>
      hostKey ? { pathname: latest.current.pathname, search: appendSearch(search, latest.current.host), hash: latest.current.hash } : { pathname: latest.current.pathname, search },
    [hostKey],
  );

  // invalid values: list them once and rewrite the URL in canonical form
  useEffect(() => {
    if (!parsed.problems.length) return;
    setProblems((p) => [...new Set([...p, ...parsed.problems])]);
    navigate(to(writeSearch(parsed.scenario, parsed.view, ctx)), { replace: true });
  }, [parsed, ctx, navigate, to]);

  const write = useCallback(
    (scenario: Scenario, view: ViewState, replace: boolean) => {
      const search = writeSearch(scenario, view, ctx);
      if (search === writeSearch(latest.current.parsed.scenario, latest.current.parsed.view, ctx)) return;
      navigate(to(search), { replace, preventScrollReset: true });
    },
    [ctx, navigate, to],
  );

  const value = useMemo<ScenarioCtx>(() => {
    const key = scenarioKey(parsed.scenario, ctx);
    const isBaseline = key === '';
    const cur = () => latest.current.parsed;
    return {
      scenario: parsed.scenario,
      view: parsed.view,
      key,
      isBaseline,
      show: isBaseline ? 'scenario' : parsed.view.show,
      problems,
      dismissProblems: () => setProblems([]),
      setDial: (name, v, opts) => {
        const p = cur();
        write({ ...p.scenario, dials: { ...p.scenario.dials, [name]: v } }, p.view, opts?.replace ?? true);
      },
      setDials: (dials) => {
        const p = cur();
        write({ ...p.scenario, dials: { ...dials } }, p.view, false);
      },
      setAdvanced: (f, opts) => {
        const p = cur();
        write({ ...p.scenario, advanced: f(cloneAdvanced(p.scenario.advanced)) }, p.view, opts?.replace ?? false);
      },
      setScenario: (s) => write(s, cur().view, false),
      reset: () => write(defaultScenario(ctx), cur().view, false),
      setView: (patch, opts) => {
        const p = cur();
        write(p.scenario, { ...p.view, ...patch }, opts?.replace ?? false);
      },
      navQuery: navSearch(parsed.scenario, parsed.view, ctx),
    };
  }, [parsed, ctx, problems, write]);

  return <ScenarioContext.Provider value={value}>{children}</ScenarioContext.Provider>;
}

export function useScenario(): ScenarioCtx {
  const v = useContext(ScenarioContext);
  if (!v) throw new Error('useScenario outside ScenarioProvider');
  return v;
}

/** The scenario when a ScenarioProvider is mounted, else null (a host page that mounts the engine on demand). */
export function useScenarioOptional(): ScenarioCtx | null {
  return useContext(ScenarioContext);
}

export function useAdvancedChanged() {
  const { scenario } = useScenario();
  const app = useApp();
  return useMemo(() => advancedChanged(scenario.advanced, app.urlCtx), [scenario.advanced, app.urlCtx]);
}

// ---- runs -------------------------------------------------------------------------------------------

export interface LayerResult<P> {
  key: string;
  payload: P;
  source: 'baseline' | 'computed';
  ms: number;
  cached: boolean;
  contrib?: boolean;
}

export interface RunStatus {
  phase: 'idle' | 'waiting' | 'running' | 'error';
  progress: ProgressMessage | null;
  error: EngineFailure | null;
  ms: number | null;
}

type Need = 'partner' | 'contrib';

interface RunCtx {
  core: LayerResult<CorePayload> | null;
  partner: LayerResult<PartnerPayload> | null;
  status: RunStatus;
  wants: Record<Need, boolean>;
  register: (need: Need) => () => void;
  /** run now if a run is waiting for its debounce (slider released) */
  flush: () => void;
  retry: () => void;
}

const RunContext = createContext<RunCtx | null>(null);

export const DEBOUNCE_MS = 150;

export function RunProvider({ children }: { children: ReactNode }) {
  const { client, state } = useEngine();
  const { scenario, key } = useScenario();
  const [needs, setNeeds] = useState<Record<Need, number>>({ partner: 0, contrib: 0 });
  const [res, setRes] = useState<Pick<RunCtx, 'core' | 'partner'>>({ core: null, partner: null });
  const [status, setStatus] = useState<RunStatus>({ phase: 'idle', progress: null, error: null, ms: null });
  const [attempt, setAttempt] = useState(0);
  const timer = useRef<ReturnType<typeof setTimeout> | null>(null);
  const pending = useRef<(() => void) | null>(null);
  const latest = useRef({ scenario, key });
  latest.current = { scenario, key };

  const wp = needs.partner > 0;
  const wc = needs.contrib > 0;
  const wants = useMemo(() => ({ partner: wp, contrib: wc }), [wp, wc]);
  const missing =
    !res.core ||
    res.core.key !== key ||
    (wants.contrib && !res.core.contrib) ||
    (wants.partner && res.partner?.key !== key);
  // read through a ref, so a result arriving mid-run never re-schedules (and supersedes) that run
  const delayRef = useRef(0);
  delayRef.current = res.core === null ? 0 : DEBOUNCE_MS;

  useEffect(() => {
    if (state.status !== 'ready' || !missing) return;
    const go = () => {
      timer.current = null;
      pending.current = null;
      const runKey = latest.current.key;
      const want = { ...wants };
      setStatus({ phase: 'running', progress: null, error: null, ms: null });
      const h = client.run(toSpec(latest.current.scenario), want, {
        onResult: (msg) => {
          const lr = { key: runKey, payload: msg.payload, source: msg.source, ms: msg.ms, cached: msg.cached };
          if (msg.layer === 'core') setRes((r) => ({ ...r, core: { ...lr, payload: msg.payload, contrib: want.contrib } }));
          else setRes((r) => ({ ...r, partner: { ...lr, payload: msg.payload } }));
        },
        onProgress: (p) => setStatus((s) => ({ ...s, progress: p })),
      });
      void h.done.then((o) => {
        if (o.status === 'superseded' || o.status === 'cancelled') return;
        setStatus({ phase: o.status === 'error' ? 'error' : 'idle', progress: null, error: o.error ?? null, ms: o.ms ?? null });
      });
    };
    const delay = delayRef.current;
    pending.current = go;
    setStatus((s) => (s.phase === 'running' ? s : { ...s, phase: 'waiting' }));
    timer.current = setTimeout(go, delay);
    return () => {
      if (timer.current) clearTimeout(timer.current);
      timer.current = null;
      pending.current = null;
    };
    // `missing` folds the result keys in; wants / key / attempt trigger new runs
  }, [state.status, key, wants, missing, attempt, client]);

  const register = useCallback((need: Need) => {
    setNeeds((n) => ({ ...n, [need]: n[need] + 1 }));
    return () => setNeeds((n) => ({ ...n, [need]: Math.max(0, n[need] - 1) }));
  }, []);

  const flush = useCallback(() => {
    if (timer.current && pending.current) {
      clearTimeout(timer.current);
      const go = pending.current;
      go();
    }
  }, []);

  const retry = useCallback(() => setAttempt((a) => a + 1), []);

  const value = useMemo<RunCtx>(() => ({ ...res, status, wants, register, flush, retry }), [res, status, wants, register, flush, retry]);
  return <RunContext.Provider value={value}>{children}</RunContext.Provider>;
}

export function useRun(): RunCtx {
  const v = useContext(RunContext);
  if (!v) throw new Error('useRun outside RunProvider');
  return v;
}

/** Register a need for the lifetime of the calling component (when enabled). */
export function useNeed(need: Need, enabled = true): void {
  const { register } = useRun();
  useEffect(() => (enabled ? register(need) : undefined), [register, need, enabled]);
}

export interface LayerView<P> {
  data: P | null;
  /** the result belongs to the current scenario */
  fresh: boolean;
  source: 'baseline' | 'computed' | null;
  ms: number | null;
}

function layerView<P>(r: LayerResult<P> | null, key: string): LayerView<P> {
  return { data: r?.payload ?? null, fresh: !!r && r.key === key, source: r?.source ?? null, ms: r?.ms ?? null };
}

export function useCore(opts: { contrib?: boolean } = {}): LayerView<CorePayload> {
  useNeed('contrib', opts.contrib === true);
  const { core } = useRun();
  const { key } = useScenario();
  const v = layerView(core, key);
  if (opts.contrib && v.data && !v.data.pillars.es_contrib) return { ...v, fresh: false };
  return v;
}

export function usePartner(enabled = true): LayerView<PartnerPayload> {
  useNeed('partner', enabled);
  const { partner } = useRun();
  const { key } = useScenario();
  return layerView(partner, key);
}
