// The Country Insights layout (spec_country.md A4.0): loads data/country/manifest.json (validated),
// registers the tier-2 nav ("Country Insights views"), and renders the view. No engine, no Strategic
// or Sector chunk. A missing manifest shows the module-level "No country data" screen with the
// developer hint; a newer schema asks for a reload; other errors offer Retry. Views read the manifest
// through useCountryCtx().

import { createContext, useContext } from 'react';
import { Outlet, useLocation } from 'react-router';
import { Loading, Notice } from '../components/ui.tsx';
import { useRegisterModuleNav } from '../shell/ModuleNav.tsx';
import { useTitle } from '../shell/title.ts';
import type { CountryEntry, CountryManifest } from './contract.ts';
import { useCountryManifest } from './data/hooks.ts';
import { countryNavItems } from './nav.ts';

export const PAGE = 'mx-auto w-full max-w-7xl px-4 py-6 sm:px-6 lg:px-8 lg:py-8';
export const MODULE_NAME = 'Country Insights';

export interface CountryCtx {
  manifest: CountryManifest;
  entryOf: (iso3: string | null | undefined) => CountryEntry | null;
}

const Ctx = createContext<CountryCtx | null>(null);

export function useCountryCtx(): CountryCtx {
  const v = useContext(Ctx);
  if (!v) throw new Error('useCountryCtx outside CountryLayout');
  return v;
}

function ModuleTitle() {
  useTitle('', MODULE_NAME);
  return null;
}

function Nav() {
  const loc = useLocation();
  useRegisterModuleNav('country', countryNavItems(loc.pathname));
  return null;
}

export default function CountryLayout() {
  const m = useCountryManifest();
  if (m.status === 'error' && m.error) {
    const e = m.error;
    return (
      <div className={PAGE}>
        <ModuleTitle />
        <Nav />
        {e.code === 'NO_DATA' ? (
          <Notice kind="info" title="No country data">
            <p>{e.message}</p>
            <p className="mt-2 text-paper-mute">
              Developers: run <code>Rscript scripts/88_export_country_data.R</code> from the repository root (or <code>npm run dev:fixture</code> for the synthetic data). The Sector pages and the security and opportunity score pages are unaffected.
            </p>
          </Notice>
        ) : e.code === 'NEWER' ? (
          <Notice kind="warning" title="Please reload" action={<button type="button" className="btn btn-ghost btn-sm" onClick={() => window.location.reload()}>Reload</button>}>
            This data was exported for a newer version of the explorer; reload.
          </Notice>
        ) : (
          <Notice kind="error" title="The country data could not be loaded" action={<button type="button" className="btn btn-ghost btn-sm" onClick={m.retry}>Retry</button>}>
            {e.message}
          </Notice>
        )}
      </div>
    );
  }
  if (!m.data) {
    return (
      <div className={PAGE}>
        <ModuleTitle />
        <Nav />
        <Loading label="Loading the list of countries" />
      </div>
    );
  }
  const manifest = m.data;
  const byIso = new Map(manifest.countries.map((c) => [c.iso3, c]));
  const ctx: CountryCtx = { manifest, entryOf: (iso) => (iso ? (byIso.get(iso.toUpperCase()) ?? null) : null) };
  return (
    <Ctx.Provider value={ctx}>
      <Nav />
      <Outlet />
    </Ctx.Provider>
  );
}
