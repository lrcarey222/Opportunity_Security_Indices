// EngineScope (IA refactor, 2026-10-16): the Strategic engine, scenario and run providers without the
// Strategic chrome (no status strip, no dial sidebar), so a Strategic view, a piece of one or the
// DialPanel can sit inside a Country, Allied Industrial Base or About page. StrategicLayout uses the same
// gate (EngineReady) around its chrome.
//
// One per page: the engine client runs one scenario at a time (a new run supersedes every older one), so
// two RunProviders on one page would starve each other. Mount it once around everything on the page that
// reads the engine.
//
//   hostKeys  the query keys the host page owns (the Country profile's cell / partner / all, an AIB
//             page's tab): the Strategic parser ignores them and every Strategic URL write keeps them,
//             and the hash (state/EngineProvider.tsx ScenarioProvider).
//   fallback  what shows while the engine loads (default: "Loading the model, 0.7 MB"). A host that
//             already shows NEIS-baseline content can pass that content, so the page does not collapse
//             when the engine arrives.
//
// The shared client survives module switches, so after the first page that loads it the gate is instant.

import { useEffect, type ReactNode } from 'react';
import { Loading } from '../components/ui.tsx';
import { EngineProvider, RunProvider, ScenarioProvider, useEngine } from '../state/EngineProvider.tsx';
import type { EngineState } from '../worker/client.ts';
import { fmtBytes } from '../lib/format.ts';
import { setStrategicBuild } from './buildInfo.ts';
import { EngineFailure } from './screens.tsx';

/** "Loading the model, 0.7 MB" (the start-up chunks, once the manifest names them). */
export function engineLoadingLabel(state: Pick<EngineState, 'manifest'>): string {
  const chunks = state.manifest?.chunks ?? [];
  const initial = chunks.filter((c) => c.initial);
  const bytes = initial.reduce((s, c) => s + (c.bytes_gz ?? 0), 0);
  return bytes > 0 ? `Loading the Legacy OSI scores, ${fmtBytes(bytes)}` : 'Loading the Legacy OSI scores';
}

/** Under an EngineProvider: the failure, the fallback while loading, else the children. */
export function EngineReady({ children, fallback }: { children: ReactNode; fallback?: ReactNode }) {
  const { state, app } = useEngine();
  useEffect(() => {
    if (app) setStrategicBuild(app.manifest);
  }, [app]);
  if (state.status === 'error' && !app) return <EngineFailure code={state.error?.code ?? 'ENGINE'} message={state.error?.message ?? 'Unknown error.'} />;
  if (!app) {
    return fallback !== undefined ? (
      <>{fallback}</>
    ) : (
      <div className="mx-auto w-full max-w-7xl px-4 py-16 sm:px-6 lg:px-8">
        <Loading label={engineLoadingLabel(state)} />
      </div>
    );
  }
  return <>{children}</>;
}

/** The engine, the URL scenario and one run provider around `children`, with no Strategic chrome. */
export function EngineScope({ children, hostKeys, fallback }: { children: ReactNode; hostKeys?: readonly string[]; fallback?: ReactNode }) {
  return (
    <EngineProvider>
      <EngineReady fallback={fallback}>
        <ScenarioProvider hostKeys={hostKeys}>
          <RunProvider>{children}</RunProvider>
        </ScenarioProvider>
      </EngineReady>
    </EngineProvider>
  );
}
