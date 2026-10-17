// The root route (spec_sector.md 2.3, 2.5): header (tiers 1 and 2, the mobile menu), the route
// notice, one <main> with a ViewErrorBoundary around the module outlet, and the footer; skip links to
// main and, on a Sector profile, to its section rail (spec_sector.md 7). It loads no
// engine and no data: Strategic Insights mounts its EngineProvider in StrategicLayout, Sector
// Insights its data provider in SectorLayout. A route-change effect records the last location of
// each module (lastLocation.ts).

import { useEffect, useState } from 'react';
import { Outlet, useLocation } from 'react-router';
import { Notice } from '../components/ui.tsx';
import { Header } from './Header.tsx';
import { Footer } from './Footer.tsx';
import { ModuleNavProvider } from './ModuleNav.tsx';
import { ViewErrorBoundary } from './ViewErrorBoundary.tsx';
import { Redirect } from './Redirect.tsx';
import { rememberLocation } from './lastLocation.ts';
import { isSectorProfilePath } from './modules.ts';

/** A one-off notice carried in the location state ({ notice }) by a redirect. */
function RouteNotice() {
  const loc = useLocation();
  const notice = (loc.state as { notice?: unknown } | null)?.notice;
  const [dismissed, setDismissed] = useState<string | null>(null);
  if (typeof notice !== 'string' || !notice || dismissed === loc.key) return null;
  return (
    <div className="mx-auto mt-4 w-full max-w-7xl px-4 sm:px-6 lg:px-8">
      <Notice kind="info" onDismiss={() => setDismissed(loc.key)}>
        {notice}
      </Notice>
    </div>
  );
}

export function AppRoot() {
  const loc = useLocation();
  useEffect(() => {
    rememberLocation(loc.pathname, loc.search);
  }, [loc.pathname, loc.search]);
  return (
    <ModuleNavProvider>
      <div className="flex min-h-screen flex-col bg-ink-900 text-paper">
        <a href="#main" className="skip-link">
          Skip to content
        </a>
        {isSectorProfilePath(loc.pathname) && (
          <a href="#sections" className="skip-link">
            Skip to sections
          </a>
        )}
        <Header pathname={loc.pathname} search={loc.search} />
        <main id="main" tabIndex={-1} className="flex w-full min-w-0 flex-1 flex-col outline-none">
          <RouteNotice />
          <ViewErrorBoundary resetKey={loc.pathname}>
            <Outlet />
          </ViewErrorBoundary>
        </main>
        <Footer />
      </div>
    </ModuleNavProvider>
  );
}

/** Unknown paths: Home with a notice (the query string is dropped: it belonged to the bad link). */
export function ToHome({ notice }: { notice?: string }) {
  return <Redirect to="/" notice={notice} />;
}
