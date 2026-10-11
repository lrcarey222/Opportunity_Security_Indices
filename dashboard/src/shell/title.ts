// Document titles (spec_react.md 8.1, spec_sector.md 2.3). Strategic views keep their titles
// ("Overview | Allied Industrial Base Explorer | NEIS Center"); Sector views pass the module, which
// is inserted before the app name ("Batteries — Midstream | Sector Insights | Allied ...").

import { useEffect } from 'react';

export const APP_TITLE = 'Allied Industrial Base Explorer | NEIS Center';

/** The document title of a view (empty view = the app title alone, as on Home). */
export function pageTitle(view: string, module?: string): string {
  const parts = [view, module].filter((p): p is string => !!p);
  return parts.length ? `${parts.join(' | ')} | ${APP_TITLE}` : APP_TITLE;
}

/** Sets the document title for a view; `module` is "Sector Insights" on Sector pages. */
export function useTitle(view: string, module?: string): void {
  useEffect(() => {
    document.title = pageTitle(view, module);
  }, [view, module]);
}
