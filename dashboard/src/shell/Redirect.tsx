// A replacing redirect that carries a one-off notice (shown by AppRoot's route notice).
//
// React Router's <Navigate> runs its navigation in an effect keyed on the `state` prop's identity. An
// inline `state={{ notice }}` is a new object on every render, so while the target route is still
// loading (a lazy chunk) each re-render starts another navigation, which re-renders again: an endless
// loop ("Maximum update depth exceeded"; seen going from a compact's role link to an economy the Country
// roster does not hold). <Redirect> keeps the state object stable for the same notice.

import { useMemo } from 'react';
import { Navigate, type To } from 'react-router';

export function Redirect({ to, notice }: { to: To; notice?: string | null }) {
  const state = useMemo(() => (notice ? { notice } : undefined), [notice]);
  return <Navigate to={to} replace state={state} />;
}
