// The layout route of the legacy Strategic views (spec_sector.md 2.3, 2.5; IA refactor 2026-10-16):
// /overview, /country/:iso3, /space, /strategic/:iso3 and /partners/:iso3 keep their
// paths and still render here with the dial chrome, but "Strategic Insights" is no longer a module: the
// header files these paths under Countries or Allied Industrial Base (shell/modules.ts tabOf) and shows
// that tab's links, so this layout registers no nav of its own. /methods and /data moved to
// AboutEngineLayout (the engine and a one-line perspective, no dials).
// It is a lazy chunk of its own, so Home and Sector never load the engine. Order:
//
//   EngineProvider (the page's shared client: created here on the first visit, or earlier by a
//   prefetch; it survives module switches) -> EngineReady (Loading, or the failure with the header
//   kept) -> ScenarioProvider -> RunProvider -> StrategicChrome (status strip, banners, dials) ->
//   ViewErrorBoundary -> view. Pages of other modules that embed Strategic views use EngineScope
//   (same providers, no chrome).

import { Link, Outlet, useLocation } from 'react-router';
import { StrategicChrome } from '../components/AppShell.tsx';
import { EngineProvider, RunProvider, ScenarioProvider } from '../state/EngineProvider.tsx';
import { EngineReady } from './EngineScope.tsx';
import { ViewErrorBoundary } from './ViewErrorBoundary.tsx';

export { engineLoadingLabel } from './EngineScope.tsx';

/** Where an advanced page belongs: Partners under the AIB, the rest under Countries
 *  (/country/:iso3, /strategic/:iso3 and /partners/:iso3 already redirect to the profile in routes.tsx). */
export function advancedBack(pathname: string): { to: string; label: string } {
  if (/^\/partners(?:\/|$)/.test(pathname)) return { to: '/aib/partnerships', label: 'Allied Industrial Base partnerships' };
  return { to: '/countries', label: 'Countries' };
}

/** The band over every legacy Strategic page: it says what the page is and links back. */
function AdvancedBand() {
  const { pathname } = useLocation();
  const back = advancedBack(pathname);
  return (
    <p className="mb-4 flex flex-wrap items-baseline gap-x-3 gap-y-1 text-sm text-paper-mute" data-testid="advanced-band">
      <span className="eyebrow">Advanced view</span>
      <span>The full index model with its four dials, as on a country profile&rsquo;s Perspective control.</span>
      <Link className="link" to={back.to}>
        &larr; {back.label}
      </Link>
    </p>
  );
}

function StrategicGate() {
  const loc = useLocation();
  return (
    <EngineReady>
      <ScenarioProvider>
        <RunProvider>
          <StrategicChrome>
            <ViewErrorBoundary resetKey={loc.pathname}>
              <AdvancedBand />
              <Outlet />
            </ViewErrorBoundary>
          </StrategicChrome>
        </RunProvider>
      </ScenarioProvider>
    </EngineReady>
  );
}

export default function StrategicLayout() {
  return (
    <EngineProvider>
      <StrategicGate />
    </EngineProvider>
  );
}
