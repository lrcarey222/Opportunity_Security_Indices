// The Country profile /countries/:iso3 (spec_country.md A4.2; IA refactor 2026-10-16, brief s.9-12 and
// s.25, plan.md 5.3): one economy's strategic position. Loads its fact sheet (and, only when a picked
// sector needs it, the companion drivers file) and the Allied Industrial Base document, reads the URL
// state (cell, partner, all, ptab) and renders: the head, the Perspective control (the four dials), then
// beside the section nav the "at a glance" tiles and the sections, all at the six supply-chain stages:
// Strategic posture (#posture, design L2: the posture bundle's card in a real Section, its counts in the
// first tile), Security and opportunity (#position; "Strategic position" until 2026-10-28), Opportunities &
// risks, Sector position, Policy & climate, Partnerships, Role in the Allied Industrial Base, the memo and
// the data notes. States (A5): unknown ISO3 -> the landing with
// a notice; no index data -> the shell with a notice; not rankable -> every section with its "not
// available" words.
//
// The model (engine) is mounted only when needed: the URL carries dial settings, the reader opens the
// Perspective or a disclosure that shows the model. Until then everything renders from the fact sheet
// (the NEIS baseline) and the engine never starts. Once mounted, one EngineScope wraps the whole body
// (one run per page) with the same body as its fallback, and the disclosures' open state lives here so
// it survives the switch. The country's own URL writes keep every other key (dials, the model's view).

import { useCallback, useEffect, useMemo, useState } from 'react';
import { Link, Navigate, useLocation, useNavigate, useParams } from 'react-router';
import { GlossaryVarsProvider } from '../../components/glossaryVars.tsx';
import { Loading, Notice, PageHead } from '../../components/ui.tsx';
import { useAibManifest, useCompactsDoc } from '../../aib/data/hooks.ts';
import { EngineScope } from '../../shell/EngineScope.tsx';
import { appendSearch, scenarioPairs, splitSearch } from '../../state/url.ts';
import { useTitle } from '../../shell/title.ts';
import { Redirect } from '../../shell/Redirect.tsx';
import { PAGE, useCountryCtx } from '../CountryLayout.tsx';
import { cellOf, COUNTRY_RESERVED } from '../contract.ts';
import { useDriversDoc, useFactSheet } from '../data/hooks.ts';
import { countryStore } from '../data/store.ts';
import { SectionNav } from '../chrome/SectionNav.tsx';
import { defaultCell } from '../derive/model.ts';
import { sectorCountryLink } from '../derive/links.ts';
import { rememberCountry } from '../nav.ts';
import { COUNTRY_KEY_ORDER, parseCountrySearch, serializeCountrySearch, type CountryUrlState } from '../url.ts';
import { Glance, ProfileHead } from './profile/Header.tsx';
import { SpaceSection } from './profile/SpaceSection.tsx';
import { OpportunitiesSection } from './profile/OpportunitiesSection.tsx';
import { SectorPositionSection } from './profile/SectorPositionSection.tsx';
import { PolicyClimateSection } from './profile/PrioritiesSection.tsx';
import { PartnershipsSection } from './profile/PartnersSection.tsx';
import { AibRoleSection } from './profile/AibRoleSection.tsx';
import { DataNotes, MemoCard } from './profile/MemoNotes.tsx';
import { EngineSi, PerspectiveControl, PerspectiveTag } from './profile/Perspective.tsx';
import { ProfileProvider, type ProfileCtx } from './profile/common.tsx';
import { PostureSection } from './profile/PostureSection.tsx';

/** The query keys the profile owns (the model's parser ignores them and its URL writes keep them). */
const HOST_KEYS: readonly string[] = [...COUNTRY_KEY_ORDER];
/** Old anchors that now sit inside a closed disclosure: the hash opens it. */
const FOLD_HASH: Readonly<Record<string, string>> = { why: 'why' };

export default function CountryProfileView() {
  const params = useParams();
  const raw = (params.iso3 ?? '').toUpperCase();
  const { entryOf } = useCountryCtx();
  const entry = (COUNTRY_RESERVED as readonly string[]).includes(raw.toLowerCase()) ? null : entryOf(raw);
  const loc = useLocation();
  useTitle(entry?.name ?? 'Countries', 'country');
  useEffect(() => {
    if (entry) rememberCountry(entry.iso3);
  }, [entry]);
  if (!entry) return <Redirect to="/countries" notice={`No country with the code '${raw}'; showing all countries.`} />;
  // one spelling of the path (/countries/jpn -> /countries/JPN): the embedded views read the code from it
  if (params.iso3 !== entry.iso3) return <Navigate to={{ pathname: `/countries/${entry.iso3}`, search: loc.search, hash: loc.hash }} replace />;
  if (entry.status === 'no_data' || !entry.file) {
    return (
      <div className={PAGE}>
        <PageHead eyebrow="COUNTRIES · COUNTRY PROFILE" title={entry.name} lede={`${entry.name} is in the roster of economies but has no index data on the NEIS baseline.`} />
        <Notice kind="info" title="No index data">
          {entry.name} has no Energy Security or Economic Opportunity scores and no partner rows in the data, so there is no profile to show.{' '}
          <Link className="link" to={sectorCountryLink(entry.iso3)}>
            Its raw data in Sectors &rarr;
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
  // the country's keys first, then every other key of the URL as it is (the dials, the model's view)
  const write = useCallback(
    (next: Partial<CountryUrlState>) => {
      const s = { ...state, ...next };
      const other = [...new URLSearchParams(splitSearch(loc.search, HOST_KEYS).own)];
      nav({ pathname: loc.pathname, search: appendSearch(serializeCountrySearch(s), other), hash: loc.hash }, { replace: true, preventScrollReset: true });
    },
    [state, nav, loc.pathname, loc.search, loc.hash],
  );
  const setStrategicView = useCallback(
    (pairs: [string, string][]) => {
      const q = new URLSearchParams(loc.search);
      for (const [k, v] of pairs) q.set(k, v);
      const t = q.toString();
      nav({ pathname: loc.pathname, search: t ? `?${t}` : '', hash: loc.hash }, { replace: true, preventScrollReset: true });
    },
    [nav, loc.pathname, loc.search, loc.hash],
  );

  // the model: mounted once wanted (or when the link carries dial settings), then kept
  const customInUrl = scenarioPairs(loc.search).length > 0;
  const [engineWanted, setEngineWanted] = useState(customInUrl);
  const engineOn = engineWanted || customInUrl;
  const wantEngine = useCallback(() => setEngineWanted(true), []);

  // the disclosures' open state (lifted: survives the engine mounting)
  const [open, setOpenMap] = useState<Readonly<Record<string, boolean>>>(() => {
    const f = FOLD_HASH[loc.hash.slice(1)];
    return f ? { [f]: true } : {};
  });
  const isOpen = useCallback((id: string) => !!open[id], [open]);
  const setOpen = useCallback((id: string, v: boolean) => setOpenMap((m) => ({ ...m, [id]: v })), []);
  // the open node of the Strategic posture card (lifted for the same reason)
  const [pcell, setPcell] = useState<string | null>(null);
  const reveal = useCallback(
    (id: string) => {
      if (id === 'perspective') setEngineWanted(true);
      setOpenMap((m) => ({ ...m, [id]: true }));
      window.setTimeout(() => (document.getElementById(`fold-${id}`) ?? document.getElementById(id))?.scrollIntoView?.({ block: 'start', behavior: 'smooth' }), 60);
    },
    [],
  );

  // the Allied Industrial Base document (fetched once and shared with the AIB pages)
  const aibM = useAibManifest();
  const aibD = useCompactsDoc(aibM.status === 'ready' ? aibM.data : null);
  const aibStatus: 'loading' | 'ready' | 'error' = aibM.status === 'error' || aibD.status === 'error' ? 'error' : aibD.data ? 'ready' : 'loading';
  const aibDoc = aibD.data;
  const aib = useMemo(() => ({ doc: aibDoc, status: aibStatus, names: aibDoc?.countries ?? {} }), [aibDoc, aibStatus]);
  const cell = sheet ? (state.cell ?? defaultCell(sheet)) : null;
  const partner = sheet ? (state.partner ?? sheet.partners.psi_order[0] ?? sheet.partners.rows[0]?.iso3 ?? null) : null;
  const sel = sheet ? cellOf(sheet, cell) : null;
  const needDrivers = !!sel && sel.drivers_in_sheet === false;
  const drv = useDriversDoc(entry, sheet, needDrivers);

  // follow a deep link's #anchor once the sheet has rendered (#aib-role, #memo, an old #why ...)
  useEffect(() => {
    if (!sheet || !loc.hash) return;
    const id = loc.hash.slice(1);
    (document.getElementById(FOLD_HASH[id] ? `fold-${FOLD_HASH[id]}` : id) ?? document.getElementById(id))?.scrollIntoView?.({ block: 'start' });
    // only when this sheet first renders
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [sheet]);

  if (sheetL.status === 'error' && sheetL.error) {
    return (
      <div className={PAGE}>
        <PageHead eyebrow="COUNTRIES · COUNTRY PROFILE" title={entry.name} lede="" />
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
        <PageHead eyebrow="COUNTRIES · COUNTRY PROFILE" title={entry.name} lede="" />
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
    setPtab: (t) => write({ ptab: t }),
    isOpen,
    setOpen,
    reveal,
    wantEngine,
    setStrategicView,
    aib,
    customInUrl,
    pcell,
    setPcell,
  };
  const body = (
    <div className={PAGE}>
      {parsed.problems.length > 0 && (
        <div className="mb-4">
          <Notice kind="warning" title="Ignored in the link">
            {parsed.problems.join('; ')}
          </Notice>
        </div>
      )}
      <ProfileHead manifest={manifest} />
      <PerspectiveControl />
      <div className="xl:grid xl:grid-cols-[200px_minmax(0,1fr)] xl:gap-8">
        <SectionNav footer={<PerspectiveTag />} />
        <div className="min-w-0 space-y-14">
          {sheet.meta.status === 'non_rankable' && (
            <Notice kind="info" title="Scores shown but not ranked">
              {sheet.meta.country}&apos;s index scores are shown but not compared with other economies: {sheet.meta.status_reason ?? 'most of its data are filled in'}.
              The charts that compare sectors are empty (the Strategic posture section is computed separately); the tables list every value with its reason.
            </Notice>
          )}
          <Glance />
          <PostureSection />
          <SpaceSection />
          <OpportunitiesSection />
          <SectorPositionSection />
          <PolicyClimateSection />
          <PartnershipsSection />
          <AibRoleSection />
          <MemoCard />
          <DataNotes />
        </div>
      </div>
    </div>
  );
  return (
    <GlossaryVarsProvider vars={{ n_core: String(manifest.counts.core), n_partner: String(manifest.counts.partners), un_window: sheet.meta.un_window }}>
      <ProfileProvider value={ctx}>
        {engineOn ? (
          <EngineScope hostKeys={HOST_KEYS} fallback={body}>
            <EngineSi iso3={sheet.iso3}>{body}</EngineSi>
          </EngineScope>
        ) : (
          body
        )}
      </ProfileProvider>
    </GlossaryVarsProvider>
  );
}
