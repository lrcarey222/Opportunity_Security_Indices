// The shared footer (spec_sector.md 2.1 item 6, 2.5): citation, the version stamp (ReleaseStamp.tsx: the data
// release and, one click away, the method version and the app and bundle builds known to this page; from
// buildInfo, never forcing a load), and the links to Methods (Strategic), Sources
// & data (Sector), the country data notes, how compacts are derived, the shared Glossary and neiscenter.org. Methods / Data
// carry the scenario of the last Strategic location, as the old footer carried navQuery.
// How compacts are derived carries the AIB scenario toggle (scn) of the current page.

import { Link, useLocation } from 'react-router';
import { scenarioCarry } from '../aib/scenarioUrl.ts';
import { useBuildInfo } from './buildInfo.ts';
import { lastLocations, sectorCarry, strategicCarry } from './lastLocation.ts';
import { placeOf } from './modules.ts';
import { dataReleaseDate, VersionLine, versionParts } from './ReleaseStamp.tsx';

const PAPER_TITLE = 'Opportunity, Security, and the Allied Industrial Base';

const searchOf = (href: string | null): string => {
  const i = href ? href.indexOf('?') : -1;
  return href && i >= 0 ? href.slice(i) : '';
};

export function Footer() {
  const info = useBuildInfo();
  const { strategic, sector } = info;
  const release = dataReleaseDate(info);
  const parts = versionParts(info);
  const loc = useLocation();
  const place = placeOf(loc.pathname);
  const sq = strategicCarry(place === 'strategic' ? loc.search : searchOf(lastLocations().strategic));
  const cq = place === 'sector' ? sectorCarry(loc.search) : '';
  const year = (strategic?.created ?? sector?.created ?? '').slice(0, 4) || String(new Date().getFullYear());
  const title = strategic?.paper?.title ?? PAPER_TITLE;
  return (
    <footer className="mt-16 border-t hairline bg-ink-900">
      <div className="mx-auto grid max-w-7xl gap-6 px-4 py-10 text-sm text-paper-mute sm:px-6 lg:grid-cols-[2fr_1fr] lg:px-8">
        <div className="space-y-2">
          <p className="text-paper-dim">
            <span className="text-paper">Cite as:</span> NEIS Center ({year}). <em>{title}</em>. Allied Industrial Base Explorer.
            {strategic?.paper?.url && (
              <>
                {' '}
                <a className="link" href={strategic.paper.url}>
                  Read it online
                </a>
              </>
            )}
          </p>
          {/* the version stamp (external review readiness, 2026-11-23): the data release, and the method and builds one click away */}
          <details className="text-xs" data-testid="version-stamp">
            <summary className="cursor-pointer tabular">
              {release ? <>Data release {release}</> : <>Version</>}
            </summary>
            <p className="mt-1 tabular">
              <VersionLine parts={parts} />
            </p>
          </details>
          <p>
            Sector pages show measured values in their sources&rsquo; own units and years. The strategic choices, partner rankings and Allied Industrial Base
            results are analysis for exploring options, not forecasts.
          </p>
        </div>
        <div className="flex flex-wrap items-start gap-x-5 gap-y-2 lg:justify-end">
          <Link className="link" to={`/methods${sq}`}>
            Methods
          </Link>
          <Link className="link" to={`/data${sq}`}>
            Data and downloads
          </Link>
          <Link className="link" to={`/sector/sources${cq}`}>
            Sources &amp; data
          </Link>
          <Link className="link" to="/countries/about">
            Country data notes
          </Link>
          <Link className="link" to={`/compacts/about${scenarioCarry(loc.search)}`}>
            How compacts are derived
          </Link>
          <Link className="link" to="/glossary">
            Glossary
          </Link>
          <Link className="link" to="/how-to-use">
            How to use
          </Link>
          <Link className="link" to="/changes">
            What changed?
          </Link>
          <a className="link" href="https://neiscenter.org">
            neiscenter.org
          </a>
        </div>
      </div>
    </footer>
  );
}
