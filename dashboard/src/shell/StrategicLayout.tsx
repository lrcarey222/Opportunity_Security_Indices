// Strategic Insights' layout route (spec_sector.md 2.3, 2.5): today's Root minus the header and the
// footer, which AppRoot now owns. It is a lazy chunk of its own, so Home and Sector never load the
// engine. Order:
//
//   EngineProvider (the page's shared client: created here on the first Strategic visit, or earlier
//   by a prefetch; it survives module switches) -> in-content engine gate (Loading, or the failure
//   with the header kept) -> ScenarioProvider -> RunProvider -> registers the Strategic nav (with the
//   scenario carried) -> StrategicChrome (status strip, banners, dials) -> ViewErrorBoundary -> view.

import { useEffect } from 'react';
import { Outlet, useLocation } from 'react-router';
import { StrategicChrome } from '../components/AppShell.tsx';
import { Loading } from '../components/ui.tsx';
import { EngineProvider, RunProvider, ScenarioProvider, useEngine, useScenario } from '../state/EngineProvider.tsx';
import type { EngineState } from '../worker/client.ts';
import { fmtBytes } from '../lib/format.ts';
import { setStrategicBuild } from './buildInfo.ts';
import { STRATEGIC_NAV } from './modules.ts';
import { useRegisterModuleNav } from './ModuleNav.tsx';
import { EngineFailure } from './screens.tsx';
import { ViewErrorBoundary } from './ViewErrorBoundary.tsx';

/** "Loading the model, 0.7 MB" (the start-up chunks, once the manifest names them). */
export function engineLoadingLabel(state: Pick<EngineState, 'manifest'>): string {
  const chunks = state.manifest?.chunks ?? [];
  const initial = chunks.filter((c) => c.initial);
  const bytes = initial.reduce((s, c) => s + (c.bytes_gz ?? 0), 0);
  return bytes > 0 ? `Loading the model, ${fmtBytes(bytes)}` : 'Loading the model';
}

function StrategicNav() {
  const { navQuery } = useScenario();
  useRegisterModuleNav(
    'strategic',
    STRATEGIC_NAV.map((n) => ({ key: n.to, label: n.label, to: n.to + navQuery })),
  );
  return null;
}

function StrategicGate() {
  const { state, app } = useEngine();
  const loc = useLocation();
  useEffect(() => {
    if (app) setStrategicBuild(app.manifest);
  }, [app]);
  if (state.status === 'error' && !app) return <EngineFailure code={state.error?.code ?? 'ENGINE'} message={state.error?.message ?? 'Unknown error.'} />;
  if (!app) {
    return (
      <div className="mx-auto w-full max-w-7xl px-4 py-16 sm:px-6 lg:px-8">
        <Loading label={engineLoadingLabel(state)} />
      </div>
    );
  }
  return (
    <ScenarioProvider>
      <RunProvider>
        <StrategicNav />
        <StrategicChrome>
          <ViewErrorBoundary resetKey={loc.pathname}>
            <Outlet />
          </ViewErrorBoundary>
        </StrategicChrome>
      </RunProvider>
    </ScenarioProvider>
  );
}

export default function StrategicLayout() {
  return (
    <EngineProvider>
      <StrategicGate />
    </EngineProvider>
  );
}
