// The layout route of the two About pages that read the engine, /methods and /data (IA refactor
// 2026-10-16, plan 5.5). They used to sit in StrategicLayout with the dial panel and status strip; now
// the dials live in the Country profile, so these pages get the engine through EngineScope (no Strategic
// chrome) and a one-line perspective (PerspectiveLine: "NEIS baseline", or the custom settings a link
// carried, with a reset), plus the same banners (problems in the link, chunk errors, failed self-checks).
// Paths, titles and the views themselves are unchanged. A lazy chunk of its own, like StrategicLayout.

import { Outlet, useLocation } from 'react-router';
import { Banners } from '../components/AppShell.tsx';
import { EngineScope } from './EngineScope.tsx';
import { PerspectiveLine } from './PerspectiveLine.tsx';
import { ViewErrorBoundary } from './ViewErrorBoundary.tsx';

export default function AboutEngineLayout() {
  const loc = useLocation();
  return (
    <EngineScope>
      <PerspectiveLine />
      <Banners />
      <div className="mx-auto w-full min-w-0 max-w-7xl flex-1 px-4 py-8 sm:px-6 lg:px-8 lg:py-10">
        <ViewErrorBoundary resetKey={loc.pathname}>
          <Outlet />
        </ViewErrorBoundary>
      </div>
    </EngineScope>
  );
}
