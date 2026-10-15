// The Country profile /countries/:iso3 (spec_country.md A4.2): one economy on the paper's baseline. Loads
// its fact sheet (and, only when a picked sector needs it, the companion drivers file), reads the URL
// state (cell, partner, all) and renders S0-S7 with the section navigation. States (A5): unknown ISO3 ->
// the landing with a notice; no index data -> the shell with a notice; not rankable -> every section
// with its "not available" words.

import { useCallback, useEffect, useMemo } from 'react';
import { Link, Navigate, useLocation, useNavigate, useParams } from 'react-router';
import { GlossaryVarsProvider } from '../../components/glossaryVars.tsx';
import { Loading, Notice, PageHead } from '../../components/ui.tsx';
import { useTitle } from '../../shell/title.ts';
import { PAGE, useCountryCtx } from '../CountryLayout.tsx';
import { cellOf, COUNTRY_RESERVED } from '../contract.ts';
import { useDriversDoc, useFactSheet } from '../data/hooks.ts';
import { countryStore } from '../data/store.ts';
import { SectionNav } from '../chrome/SectionNav.tsx';
import { defaultCell } from '../derive/model.ts';
import { sectorCountryLink } from '../derive/links.ts';
import { rememberCountry } from '../nav.ts';
import { parseCountrySearch, serializeCountrySearch, type CountryUrlState } from '../url.ts';
import { Glance, ProfileHead } from './profile/Header.tsx';
import { SpaceSection } from './profile/SpaceSection.tsx';
import { PrioritiesSection } from './profile/PrioritiesSection.tsx';
import { WhySection } from './profile/WhySection.tsx';
import { PartnersSection } from './profile/PartnersSection.tsx';
import { DataNotes, MemoCard } from './profile/MemoNotes.tsx';
import { ProfileProvider, type ProfileCtx } from './profile/common.tsx';

export default function CountryProfileView() {
  const params = useParams();
  const raw = (params.iso3 ?? '').toUpperCase();
  const { entryOf } = useCountryCtx();
  const entry = (COUNTRY_RESERVED as readonly string[]).includes(raw.toLowerCase()) ? null : entryOf(raw);
  useTitle(entry?.name ?? 'Countries', 'country');
  useEffect(() => {
    if (entry) rememberCountry(entry.iso3);
  }, [entry]);
  if (!entry) return <Navigate to="/countries" replace state={{ notice: `No country with the code '${raw}'; showing all countries.` }} />;
  if (entry.status === 'no_data' || !entry.file) {
    return (
      <div className={PAGE}>
        <PageHead eyebrow="COUNTRY INSIGHTS · PAPER BASELINE" title={entry.name} lede={`${entry.name} is in the roster of economies but has no index data on the paper's baseline.`} />
        <Notice kind="info" title="No index data">
          {entry.name} has no Energy Security or Economic Opportunity scores and no partner rows in the paper&apos;s data, so there is no profile to show.{' '}
          <Link className="link" to={sectorCountryLink(entry.iso3)}>
            Its raw data in Sector Insights &rarr;
          </Link>
        </Notice>
        <p className="mt-6 text-sm">
          <Link className="link" to="/countries">
            &larr; All countries
          </Link>
        </p>
      </div>
    );
  }
  return <Profile key={entry.iso3} iso3={entry.iso3} />;
}

function Profile({ iso3 }: { iso3: string }) {
  const { manifest, entryOf } = useCountryCtx();
  const entry = entryOf(iso3)!;
  const loc = useLocation();
  const nav = useNavigate();
  const sheetL = useFactSheet(entry);
  const sheet = sheetL.data;
  const parsed = useMemo(
    () => parseCountrySearch(loc.search, sheet ? { cells: new Set(sheet.cells.map((c) => c.key)), partners: new Set(sheet.partners.rows.map((r) => r.iso3)) } : {}),
    [loc.search, sheet],
  );
  const state = parsed.state;
  const write = useCallback(
    (next: Partial<CountryUrlState>) => {
      const s = { ...state, ...next };
      nav({ pathname: loc.pathname, search: serializeCountrySearch(s), hash: loc.hash }, { replace: true, preventScrollReset: true });
    },
    [state, nav, loc.pathname, loc.hash],
  );
  const cell = sheet ? (state.cell ?? defaultCell(sheet)) : null;
  const partner = sheet ? (state.partner ?? sheet.partners.psi_order[0] ?? sheet.partners.rows[0]?.iso3 ?? null) : null;
  const sel = sheet ? cellOf(sheet, cell) : null;
  const needDrivers = !!sel && sel.drivers_in_sheet === false;
  const drv = useDriversDoc(entry, sheet, needDrivers);

  // follow a deep link's #anchor once the sheet has rendered (#why, #memo, ...)
  useEffect(() => {
    if (!sheet || !loc.hash) return;
    document.getElementById(loc.hash.slice(1))?.scrollIntoView?.({ block: 'start' });
    // only when this sheet first renders
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [sheet]);

  if (sheetL.status === 'error' && sheetL.error) {
    return (
      <div className={PAGE}>
        <PageHead eyebrow="COUNTRY INSIGHTS · PAPER BASELINE" title={entry.name} lede="" />
        <Notice kind="error" title={`The fact sheet of ${entry.name} could not be loaded`} action={<button type="button" className="btn btn-ghost btn-sm" onClick={sheetL.retry}>Retry</button>}>
          {sheetL.error.message}
        </Notice>
      </div>
    );
  }
  if (!sheet) {
    const kb = entry.bytes_gz ? `, ${Math.max(1, Math.round(entry.bytes_gz / 1024))} kB` : '';
    return (
      <div className={PAGE}>
        <PageHead eyebrow="COUNTRY INSIGHTS · PAPER BASELINE" title={entry.name} lede="" />
        <div className="min-h-[60vh]">
          <Loading label={`Loading ${entry.name}${kb}`} />
        </div>
      </div>
    );
  }
  const ctx: ProfileCtx = {
    sheet,
    entry,
    manifest,
    state,
    cell,
    partner,
    setCell: (k) => write({ cell: k }),
    setPartner: (p) => write({ partner: p }),
    setAll: (a) => write({ all: a }),
    driversDoc: drv.data,
    driversStatus: needDrivers ? drv.status : 'idle',
    retryDrivers: drv.retry,
    fileUrl: (f) => countryStore().url(f),
  };
  return (
    <GlossaryVarsProvider vars={{ n_core: String(manifest.counts.core), n_partner: String(manifest.counts.partners), un_window: sheet.meta.un_window }}>
      <ProfileProvider value={ctx}>
        <div className={PAGE}>
          {parsed.problems.length > 0 && (
            <div className="mb-4">
              <Notice kind="warning" title="Ignored in the link">
                {parsed.problems.join('; ')}
              </Notice>
            </div>
          )}
          <ProfileHead manifest={manifest} />
          <div className="xl:grid xl:grid-cols-[200px_minmax(0,1fr)] xl:gap-8">
            <SectionNav />
            <div className="min-w-0 space-y-14">
              {sheet.meta.status === 'non_rankable' && (
                <Notice kind="info" title="Scores shown but not ranked">
                  {sheet.meta.country}&apos;s scores are shown but not compared with other economies: {sheet.meta.status_reason ?? 'most of its data are filled in'}. The charts that
                  compare sectors are empty; the tables list every value with its reason.
                </Notice>
              )}
              <Glance />
              <SpaceSection />
              <PrioritiesSection />
              <WhySection />
              <PartnersSection />
              <MemoCard />
              <DataNotes />
            </div>
          </div>
        </div>
      </ProfileProvider>
    </GlossaryVarsProvider>
  );
}
