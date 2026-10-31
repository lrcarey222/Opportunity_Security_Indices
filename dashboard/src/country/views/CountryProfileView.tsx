// The Country profile /countries/:iso3 (spec_country.md A4.2; IA refactor 2026-10-16; redesigned 2026-10-29
// around the canonical framework, spec XI-XIX, design 4 and 5b): one economy's strategy. Loads its fact sheet
// (and, only when a picked sector needs it, the companion drivers file), the Allied Industrial Base document
// and the strategic position framework (the posture core and the economy's detail document), reads the URL
// state (cell, partner, all, ptab, scn; node, the shared selection) and renders the head (name, the generated
// strategy sentence, data freshness, memo and export actions) and, beside the section nav, seven sections:
//   1 Strategy, 2-4 the High, Medium and Low fences (2026-10-30: views/profile/FenceSections.tsx, in place of
//   Need, Prize & Position and Choices), 5 Policy Alignment, 6 Allied Strategy (the compacts with a role)
//   (views/profile/StrategySections.tsx), 7 Evidence & Methodology with the Legacy OSI framework
//   (views/profile/EvidenceSection.tsx).
// One drawer (posture/PostureNodeDrawer) serves every entry point through ?node=<Tech>|<Stage> (design D8),
// focused on a layer of the node with ?layer=<layer_id> (a fence section's layer unit, 2026-10-30);
// a value the data do not know is reported in "Ignored in the link". Old anchors reach their new place:
// alias spans in the sections, and the old blocks now in closed disclosures (the Legacy OSI results) open on
// load (derive/strategyPage.ts hashPlan). States (A5): unknown ISO3 ->
// the landing with a notice; no index data -> the shell with a notice; not rankable -> the Legacy OSI blocks
// say so.
//
// The model (engine) is mounted only for the Legacy OSI results (the only blocks that follow the Perspective
// dials), and only when needed: the URL carries dial settings, or the reader opens the Perspective or a
// disclosure that shows the model. Sections 1-6 never read it. The disclosures' open state lives here, so it
// survives the engine mounting; the country's own URL writes keep every other key (dials, node, the model's view).

import { useCallback, useEffect, useMemo, useState } from 'react';
import { Link, Navigate, useLocation, useNavigate, useParams } from 'react-router';
import { GlossaryVarsProvider } from '../../components/glossaryVars.tsx';
import { Loading, Notice, PageHead } from '../../components/ui.tsx';
import { useAibManifest, useCompactsDoc } from '../../aib/data/hooks.ts';
import { countryCells, LAYER_KEY, NODE_KEY, PostureNodeDrawer, useNodeSelection, usePostureCore, usePostureDetail } from '../../posture/index.ts';
import { appendSearch, scenarioPairs, splitSearch } from '../../state/url.ts';
import { useTitle } from '../../shell/title.ts';
import { Redirect } from '../../shell/Redirect.tsx';
import { PAGE, useCountryCtx } from '../CountryLayout.tsx';
import { cellOf, COUNTRY_RESERVED } from '../contract.ts';
import { useDriversDoc, useFactSheet } from '../data/hooks.ts';
import { countryStore } from '../data/store.ts';
import { SectionNav } from '../chrome/SectionNav.tsx';
import { defaultCell } from '../derive/model.ts';
import { postureSectorLink, sectorCountryLink } from '../derive/links.ts';
import { hashPlan, LEGACY_FOLD } from '../derive/strategyPage.ts';
import { cap, proseName } from '../derive/text.ts';
import { rememberCountry } from '../nav.ts';
import { COUNTRY_KEY_ORDER, parseCountrySearch, serializeCountrySearch, type CountryUrlState } from '../url.ts';
import { ProfileHead } from './profile/Header.tsx';
import { EvidenceSection } from './profile/EvidenceSection.tsx';
import { HighFenceSection, LowFenceSection, MediumFenceSection } from './profile/FenceSections.tsx';
import { AlliedSection, PolicyAlignmentSection, StrategySection } from './profile/StrategySections.tsx';
import { ProfileProvider, type CountryPosture, type ProfileCtx } from './profile/common.tsx';

/** The query keys the profile owns (the model's parser ignores them and its URL writes keep them): the country's own and the node selection (with its layer). */
const HOST_KEYS: readonly string[] = [...COUNTRY_KEY_ORDER, NODE_KEY, LAYER_KEY];

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
  // the country's keys first, then every other key of the URL as it is (the node, the dials, the model's view)
  const write = useCallback(
    (next: Partial<CountryUrlState>) => {
      const s = { ...state, ...next };
      const other = [...new URLSearchParams(splitSearch(loc.search, COUNTRY_KEY_ORDER).own)];
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

  // the model: mounted once wanted (or when the link carries dial settings), then kept; it wraps only the Legacy OSI results
  const customInUrl = scenarioPairs(loc.search).length > 0;
  const [engineWanted, setEngineWanted] = useState(customInUrl);
  const engineOn = engineWanted || customInUrl;
  const wantEngine = useCallback(() => setEngineWanted(true), []);

  // the disclosures' open state (lifted: survives the engine mounting). An old anchor whose block now sits in a
  // closed disclosure opens it; a link with dial settings opens the Legacy OSI results (the dials apply there)
  const [open, setOpenMap] = useState<Readonly<Record<string, boolean>>>(() => {
    const o: Record<string, boolean> = {};
    for (const f of hashPlan(loc.hash).folds) o[f] = true;
    if (customInUrl) o[LEGACY_FOLD] = true;
    return o;
  });
  const isOpen = useCallback((id: string) => !!open[id], [open]);
  const setOpen = useCallback((id: string, v: boolean) => setOpenMap((m) => ({ ...m, [id]: v })), []);
  const reveal = useCallback((id: string) => {
    const plan = hashPlan(`#${id}`);
    if (id === 'perspective') setEngineWanted(true);
    setOpenMap((m) => ({ ...m, ...Object.fromEntries([...plan.folds, id].map((f) => [f, true])) }));
    window.setTimeout(() => (document.getElementById(`fold-${id}`) ?? document.getElementById(plan.target ?? id) ?? document.getElementById(id))?.scrollIntoView?.({ block: 'start', behavior: 'smooth' }), 60);
  }, []);

  // the strategic position framework: the core (shared with the other pages), the economy's detail, the ?node= selection
  const core = usePostureCore();
  const detail = usePostureDetail(iso3);
  const node = useNodeSelection(core.data?.meta ?? null);
  const cells = useMemo(() => (core.data ? countryCells(core.data, iso3) : []), [core.data, iso3]);
  const posture = useMemo<CountryPosture>(
    () => ({
      status: core.status, error: core.error, retry: core.retry, core: core.data, cells, detail: detail.data,
      detailPending: detail.status === 'loading', detailStatus: detail.status, detailError: detail.error, retryDetail: detail.retry, node,
    }),
    [core.status, core.error, core.retry, core.data, cells, detail.data, detail.status, detail.error, detail.retry, node],
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

  // follow a deep link's #anchor once the sheet has rendered (#allied, #legacy-osi, an old #aib-role, #why ...)
  useEffect(() => {
    if (!sheet || !loc.hash) return;
    const id = loc.hash.slice(1);
    const plan = hashPlan(loc.hash);
    (document.getElementById(plan.target ?? id) ?? document.getElementById(id))?.scrollIntoView?.({ block: 'start' });
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
    posture,
  };
  const problems = node.problem ? [...parsed.problems, node.problem] : parsed.problems;
  const name = proseName(sheet.meta.country);
  return (
    <GlossaryVarsProvider vars={{ n_core: String(manifest.counts.core), n_partner: String(manifest.counts.partners), un_window: sheet.meta.un_window }}>
      <ProfileProvider value={ctx}>
        <div className={PAGE}>
          {problems.length > 0 && (
            <div className="mb-4">
              <Notice kind="warning" title="Ignored in the link">
                {problems.join('; ')}
              </Notice>
            </div>
          )}
          {customInUrl && (
            <div className="mb-4">
              <Notice kind="info" title="A custom perspective in the link">
                The link sets the Perspective dials. They re-weight only the Legacy OSI indices, so they are applied in{' '}
                <a className="link" href="#legacy-osi">
                  Evidence &amp; methodology: Legacy OSI framework
                </a>
                ; the strategy does not change with them.
              </Notice>
            </div>
          )}
          <ProfileHead manifest={manifest} />
          <div className="xl:grid xl:grid-cols-[200px_minmax(0,1fr)] xl:gap-8">
            <SectionNav />
            <div className="min-w-0 space-y-14">
              <StrategySection />
              <HighFenceSection />
              <MediumFenceSection />
              <LowFenceSection />
              <PolicyAlignmentSection />
              <AlliedSection />
              <EvidenceSection engineOn={engineOn} hostKeys={HOST_KEYS} />
            </div>
          </div>
          <PostureNodeDrawer iso3={sheet.iso3} name={cap(name)} selected={node.selected} layer={node.layer} onClose={() => node.select(null)} sectorHref={(tech, stage) => postureSectorLink(tech, stage, sheet.iso3)} />
        </div>
      </ProfileProvider>
    </GlossaryVarsProvider>
  );
}
