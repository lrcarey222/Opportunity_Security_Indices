// Document titles (spec_react.md 8.1, spec_sector.md 2.3, spec_country.md A2.4). Strategic views keep
// their titles ("Overview | Allied Industrial Base Explorer | NEIS Center"); Sector and Country views
// pass the module (its label, or its id 'sector' / 'country'), which is inserted before the app name
// ("Batteries — Midstream | Sector Insights | Allied ...", "Japan | Country Insights | Allied ...").

import { useEffect } from 'react';

export const APP_TITLE = 'Allied Industrial Base Explorer | NEIS Center';

const MODULE_LABEL: Record<string, string> = { sector: 'Sector Insights', strategic: 'Strategic Insights', country: 'Country Insights' };

/** The document title of a view (empty view = the app title alone, as on Home). */
export function pageTitle(view: string, module?: string): string {
  const parts = [view, module ? (MODULE_LABEL[module] ?? module) : undefined].filter((p): p is string => !!p);
  return parts.length ? `${parts.join(' | ')} | ${APP_TITLE}` : APP_TITLE;
}

/** Sets the document title for a view; `module` is "Sector Insights" (or 'sector') on Sector pages. */
export function useTitle(view: string, module?: string): void {
  useEffect(() => {
    document.title = pageTitle(view, module);
  }, [view, module]);
}
