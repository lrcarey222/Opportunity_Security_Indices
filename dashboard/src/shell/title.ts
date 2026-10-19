// Document titles (spec_react.md 8.1, spec_sector.md 2.3, spec_country.md A2.4; IA refactor 2026-10-16).
// A view passes its own name and its module (the id 'sector' / 'country' / 'aib', or a label); the
// module's tab name goes before the app name ("Batteries — Midstream | Sectors | Allied ...", "Japan |
// Countries | Allied ...", "Compacts | Allied Industrial Base | Allied ..."). The labels of the modules
// before the refactor, which some views still pass ("Sector Insights", "Country Insights", "Sectoral
// Compacts"), map to the tab that holds them now. A tab name equal to the view's is not repeated
// ("Countries | Allied ..."). Legacy Strategic views, About pages and Home pass no module.

import { useEffect } from 'react';

export const APP_TITLE = 'Allied Industrial Base Explorer | NEIS Center';

const MODULE_LABEL: Record<string, string> = {
  sector: 'Sectors',
  country: 'Countries',
  aib: 'Allied Industrial Base',
  about: 'About',
  'Sector Insights': 'Sectors',
  'Country Insights': 'Countries',
  'Sectoral Compacts': 'Allied Industrial Base',
  // "Strategic Insights" is no longer a module: its views carry no module name
  strategic: '',
  'Strategic Insights': '',
};

/** The document title of a view (empty view = the app title alone, as on Home). */
export function pageTitle(view: string, module?: string): string {
  const label = module ? (MODULE_LABEL[module] ?? module) : undefined;
  const parts = [view, label !== view ? label : undefined].filter((p): p is string => !!p);
  return parts.length ? `${parts.join(' | ')} | ${APP_TITLE}` : APP_TITLE;
}

/** Sets the document title for a view; `module` is 'sector' (or "Sector Insights") on Sector pages. */
export function useTitle(view: string, module?: string): void {
  useEffect(() => {
    document.title = pageTitle(view, module);
  }, [view, module]);
}
