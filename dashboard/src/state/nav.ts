// Navigation helpers: links and navigate() that carry the scenario (and the shared view keys).
// The module lists live in src/shell/modules.ts (no engine imports, so the shell can use them);
// they are re-exported here: STRATEGIC_NAV (alias NAV_ITEMS) and MODULES (spec_sector.md 5.2).

import { useCallback } from 'react';
import { useNavigate } from 'react-router';
import { useApp, useScenario } from './EngineProvider.tsx';
import { navSearch, VIEW_DEFAULTS, writeSearch, type ViewState } from './url.ts';
import { STRATEGIC_NAV } from '../shell/modules.ts';

export { MODULES, STRATEGIC_NAV, type ModuleId, type ModuleInfo } from '../shell/modules.ts';
export { useMediaQuery } from '../shell/useMediaQuery.ts';

/** The Strategic views (kept under its old name). */
export const NAV_ITEMS: { to: string; label: string }[] = STRATEGIC_NAV;

/** navigate(path, viewPatch?) keeping the scenario, Show and tech / stage. */
export function useGoto(): (pathname: string, view?: Partial<ViewState>) => void {
  const navigate = useNavigate();
  const app = useApp();
  const { scenario, view } = useScenario();
  return useCallback(
    (pathname: string, patch?: Partial<ViewState>) => {
      const search = patch
        ? writeSearch(scenario, { ...VIEW_DEFAULTS, show: view.show, tech: view.tech, stage: view.stage, ...patch }, app.urlCtx)
        : navSearch(scenario, view, app.urlCtx);
      navigate({ pathname, search });
    },
    [navigate, scenario, view, app.urlCtx],
  );
}
