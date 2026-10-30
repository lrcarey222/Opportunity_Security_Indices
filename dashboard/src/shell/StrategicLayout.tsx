// The layout route of the legacy Strategic views (spec_sector.md 2.3, 2.5; IA refactor 2026-10-16):
// /overview, /country/:iso3, /space, /strategic/:iso3 and /partners/:iso3 keep their
// paths and still render here with the dial chrome, but "Strategic Insights" is no longer a module: the
// header files these paths under Countries or Allied Industrial Base (shell/modules.ts tabOf) and shows
// that tab's links, so this layout registers no nav of its own. /methods and /data moved to
// AboutEngineLayout (the engine and a one-line perspective, no dials). Since 2026-10-29 (design D10,
// section 5) every page here is labelled "Legacy OSI framework": the band over it says it is the original
// index model kept for replication and comparison, not a current recommendation, and links a Country
// profile's Evidence & Methodology (its Legacy OSI sub-section) and the Methods chapter.
// It is a lazy chunk of its own, so Home and Sector never load the engine. Order:
//
//   EngineProvider (the page's shared client: created here on the first visit, or earlier by a
//   prefetch; it survives module switches) -> EngineReady (Loading, or the failure with the header
//   kept) -> ScenarioProvider -> RunProvider -> StrategicChrome (status strip, banners, dials) ->
//   ViewErrorBoundary -> view. Pages of other modules that embed Strategic views use EngineScope
//   (same providers, no chrome).

import { Link, Outlet, useLocation } from 'react-router';
import { lastLocations } from './lastLocation.ts';
import { COUNTRY_DEFAULT_ISO } from './modules.ts';
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

/** The label of every legacy Strategic page (design D10). */
export const LEGACY_OSI_LABEL = 'Legacy OSI framework';

/** Where the band's links go: a Country profile's Legacy OSI sub-section (the last profile visited, else the default) and the Methods chapter. */
export function legacyOsiLinks(countryIso: string | null): { profile: string; methods: string } {
  return { profile: `/countries/${countryIso ?? COUNTRY_DEFAULT_ISO}#legacy-osi`, methods: '/methods#legacy-osi' };
}

/** The band over every legacy Strategic page: it says what the page is (Legacy OSI, not a current recommendation) and links the current framework, the legacy chapter and back. */
function AdvancedBand() {
  const { pathname } = useLocation();
  const back = advancedBack(pathname);
  const links = legacyOsiLinks(lastLocations().countryIso);
  return (
    <div className="mb-4 space-y-1 text-sm text-paper-mute" data-testid="advanced-band">
      <p className="flex flex-wrap items-baseline gap-x-3 gap-y-1">
        <span className="tag" data-basis="legacy-osi">
          {LEGACY_OSI_LABEL}
        </span>
        <span>
          Advanced view of the original index model (Energy Security, Economic Opportunity, the Strategic Index, the security and opportunity corners and the
          partner indices) with its four dials, kept for replication and comparison. It is not a current recommendation: each Country profile&rsquo;s Country
          Strategy Map is.
        </span>
      </p>
      <p className="flex flex-wrap items-baseline gap-x-4 gap-y-1">
        <Link className="link" to={links.profile}>
          Legacy OSI on a Country profile (Evidence &amp; Methodology)
        </Link>
        <Link className="link" to={links.methods}>
          Methods: the Legacy OSI framework
        </Link>
        <Link className="link" to={back.to}>
          &larr; {back.label}
        </Link>
      </p>
    </div>
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
