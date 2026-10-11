// The index route (spec_sector.md 2.4): the only code that decides between Home and the legacy
// redirect. A bare `/` carrying any Strategic query key (`/?h=80&tech=Solar`, a link made before
// Home existed) still lands on `/overview?…` (replace); otherwise Home renders. Home is its own lazy
// chunk, so the Strategic first load does not carry it.

import { lazy, Suspense } from 'react';
import { Navigate, useLocation } from 'react-router';
import { KEY_ORDER } from '../state/url.ts';

const Home = lazy(() => import('./Home.tsx'));

/** True when a query string carries a Strategic key (state/url.ts KEY_ORDER). */
export function isLegacyStrategicSearch(search: string): boolean {
  const q = new URLSearchParams(search);
  return KEY_ORDER.some((k) => q.has(k));
}

export function HomeOrLegacy() {
  const loc = useLocation();
  if (isLegacyStrategicSearch(loc.search)) return <Navigate to={{ pathname: '/overview', search: loc.search }} replace state={loc.state} />;
  return (
    <Suspense fallback={<div className="min-h-[60vh]" aria-hidden="true" />}>
      <Home />
    </Suspense>
  );
}
