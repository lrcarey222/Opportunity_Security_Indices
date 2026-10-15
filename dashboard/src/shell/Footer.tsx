// The shared footer (spec_sector.md 2.1 item 6, 2.5): citation, the data vintage of each module
// when known (from buildInfo; never forces a load), and the links to Methods (Strategic), Sources
// & data (Sector), About Country Insights, the shared Glossary and neiscenter.org. Strategic links
// carry the scenario of the last Strategic location, as the old footer carried navQuery.

import { Link, useLocation } from 'react-router';
import { fmtDate } from '../lib/format.ts';
import { useBuildInfo } from './buildInfo.ts';
import { lastLocations, sectorCarry, strategicCarry } from './lastLocation.ts';
import { placeOf } from './modules.ts';

const PAPER_TITLE = 'Opportunity, Security, and the Allied Industrial Base';

const searchOf = (href: string | null): string => {
  const i = href ? href.indexOf('?') : -1;
  return href && i >= 0 ? href.slice(i) : '';
};

export function Footer() {
  const { strategic, sector } = useBuildInfo();
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
                  Read the paper
                </a>
              </>
            )}
          </p>
          {(sector || strategic) && (
            <p className="tabular">
              {sector && <>Sector data: export {fmtDate(sector.created)}, build {sector.bundle_id.slice(0, 8)}. </>}
              {strategic && (
                <>
                  Strategic data: OSI pipeline export {fmtDate(strategic.created)}, build {strategic.bundle_id.slice(0, 8)}
                  {strategic.git_commit ? `, git ${strategic.git_commit.slice(0, 10)}` : ''}.
                </>
              )}
            </p>
          )}
          <p>Strategic Insights results are model outputs for exploration, not forecasts. Sector Insights shows measured values in their sources&rsquo; own units and years. Country Insights shows the paper&rsquo;s baseline beside the raw data behind it.</p>
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
            About Country Insights
          </Link>
          <Link className="link" to="/glossary">
            Glossary
          </Link>
          <a className="link" href="https://neiscenter.org">
            neiscenter.org
          </a>
        </div>
      </div>
    </footer>
  );
}
