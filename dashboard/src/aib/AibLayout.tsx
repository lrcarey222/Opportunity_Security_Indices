// The Sectoral Compacts layout: loads data/aib/manifest.json and the compacts document it names (both
// checked against the contract), registers the tier-2 nav ("Sectoral Compacts views") and renders the
// view. No engine, no other module's chunk. A missing manifest shows the module-level "No compacts data"
// screen with the developer hint; a newer schema asks for a reload; other errors offer Retry. Views read
// the data through useAibCtx().
//
// The scenario toggle (2026-10-19; Managed Interdependence the default since 2026-10-20): when the bundle
// carries the scenarios (hasScenarios), a shared header above every page (/aib/* and /compacts/* alike)
// holds the toggle "Managed Interdependence | Decoupling" (ScenarioToggle), which says that a strategic
// compact is the persistent architecture and the scenario the stress applied to it. The choice lives in
// the URL (`scn`: absent = Managed Interdependence, scn=decoupling; /aib and /compacts mount separate
// layouts) and is in the context as `scenario` (Decoupling for a bundle without scenarios, whose main
// document is Decoupling), with setScenario() and href() (an href with the choice carried; a bundle
// without scenarios writes no scn). A stray scn (any value but "decoupling", or any scn in a bundle
// without scenarios) is dropped from the URL. The Managed Interdependence file is prefetched when the
// toggle is hovered, focused or touched, or chosen; the views that need it load it themselves.

import { createContext, useCallback, useContext, useEffect, useLayoutEffect, useMemo, useRef } from 'react';
import { Outlet, useLocation, useNavigate } from 'react-router';
import { Loading, Notice } from '../components/ui.tsx';
import { STATE } from '../content/vocabulary.ts';
import { useRegisterModuleNav } from '../shell/ModuleNav.tsx';
import { useTitle } from '../shell/title.ts';
import { countryNamesOf, strategicOf, type AibCompact, type AibDoc, type AibManifest, type AibNode, type AibStrategic, type AibStrategicScenarioId } from './contract.ts';
import { useAibManifest, useCompactsDoc } from './data/hooks.ts';
import { aibStore, type AibLoadError } from './data/store.ts';
import { compactTitle, nameOf } from './derive/format.ts';
import { effectiveScenario, hasScenarios } from './derive/scenarios.ts';
import { shockLabelOf } from './derive/settings.ts';
import { strategicScenarioFor } from './derive/strategic.ts';
import { taxonomyOf, type Taxonomy } from './derive/taxonomy.ts';
import { aibNavItems } from './nav.ts';
import { clearScenarioSearch, DEFAULT_TOGGLE, hasScenarioKey, hasStrayScenario, scenarioOfSearch, setScenarioSearch, withScenario, type AibToggle } from './scenarioUrl.ts';
import { ScenarioToggle } from './ScenarioToggle.tsx';

export const PAGE = 'mx-auto w-full max-w-7xl px-4 py-6 sm:px-6 lg:px-8 lg:py-8';
export const MODULE_NAME = 'Sectoral Compacts';
export const EYEBROW = 'SECTORAL COMPACTS';

export interface AibCtx {
  manifest: AibManifest;
  doc: AibDoc;
  names: Readonly<Record<string, string>>;
  /** an economy's name (its code when the document has none) */
  name: (iso3: string | null | undefined) => string;
  /** a compact by id (case-insensitive), or null */
  compactOf: (id: string | null | undefined) => AibCompact | null;
  nodeOf: (id: string | null | undefined) => AibNode | null;
  /** the label of the headline shock ("China supply removed") */
  shockLabel: string;
  /** the node taxonomy: where the nodes come from (dictionary or master) and the OSI technology words every view groups and filters them by */
  tx: Taxonomy;
  /** a compact's display title from its nodes' OSI technologies and labels (compactTitle; `full`: every label) */
  title: (c: AibCompact, full?: boolean) => string;
  /** the toggled scenario the pages show (2026-10-19): the URL's scn (absent: Managed Interdependence); always 'decoupling' when the bundle has no scenarios */
  scenario: AibToggle;
  /** the bundle carries the scenarios (the toggle is shown; hasScenarios) */
  hasScenarios: boolean;
  /** choose a scenario (writes scn, replacing the history entry) */
  setScenario: (s: AibToggle) => void;
  /** an href with the toggled scenario carried (withScenario): use it for every link inside the AIB section */
  href: (to: string) => string;
  /** the strategic layer (2026-10-20; doc.strategic), null in a bundle without it */
  strategic: AibStrategic | null;
  /** the scenario a strategic view reads (strategicScenarioFor() of the toggle: 'managed' or 'decoupling'), null without the layer */
  strategicScenario: AibStrategicScenarioId | null;
}

/** The context without the scenario state (buildCtx's part, memoized on the data). */
type DataCtx = Omit<AibCtx, 'scenario' | 'hasScenarios' | 'setScenario' | 'href' | 'strategicScenario'>;

const Ctx = createContext<AibCtx | null>(null);

export function useAibCtx(): AibCtx {
  const v = useContext(Ctx);
  if (!v) throw new Error('useAibCtx outside AibLayout');
  return v;
}

function ModuleTitle() {
  useTitle('', MODULE_NAME);
  return null;
}

function Nav({ firstId }: { firstId: string | null }) {
  const loc = useLocation();
  useRegisterModuleNav('aib', aibNavItems(loc.pathname, firstId, loc.search));
  return null;
}

/** The shared header of every AIB page: the scenario toggle, aligned with the page content. */
function ScenarioHeader({ ctx }: { ctx: AibCtx }) {
  const prefetch = useCallback(() => aibStore().prefetchScenario(ctx.manifest, 'managed'), [ctx.manifest]);
  const choose = useCallback(
    (s: AibToggle) => {
      if (s === 'managed') prefetch();
      ctx.setScenario(s);
    },
    [ctx, prefetch],
  );
  return (
    <div className="mx-auto w-full max-w-7xl px-4 pt-4 sm:px-6 lg:px-8" data-testid="aib-scenario-header">
      <ScenarioToggle value={ctx.scenario} onChange={choose} meta={ctx.doc.meta} onIntent={prefetch} />
    </div>
  );
}

function LoadProblem({ error, retry }: { error: AibLoadError; retry: () => void }) {
  if (error.code === 'NO_DATA') {
    return (
      <Notice kind="info" title={STATE.aibUnavailable}>
        <p>{error.message}</p>
        <p className="mt-2 text-paper-mute">
          Developers: run <code>Rscript scripts/31_build_aib_trade_network.R</code>, <code>scripts/32_build_aib_partnerships.R</code> and{' '}
          <code>scripts/84_export_aib_data.R</code> from the repository root (or <code>npm run dev:fixture</code> for the synthetic data). The other modules are unaffected.
        </p>
      </Notice>
    );
  }
  if (error.code === 'NEWER') {
    return (
      <Notice kind="warning" title="Please reload" action={<button type="button" className="btn btn-ghost btn-sm" onClick={() => window.location.reload()}>Reload</button>}>
        This data was exported for a newer version of the explorer; reload.
      </Notice>
    );
  }
  return (
    <Notice kind="error" title="The Allied Industrial Base results could not be loaded" action={<button type="button" className="btn btn-ghost btn-sm" onClick={retry}>Retry</button>}>
      {error.message}
    </Notice>
  );
}

function buildCtx(manifest: AibManifest, doc: AibDoc): DataCtx {
  const names = countryNamesOf(doc);
  const byId = new Map(doc.compacts.map((c) => [c.id.toUpperCase(), c]));
  const nodes = new Map(doc.nodes.map((n) => [n.id, n]));
  const find = (id: string) => nodes.get(id) ?? null;
  return {
    manifest,
    doc,
    names,
    name: (iso) => nameOf(names, iso),
    compactOf: (id) => (id ? (byId.get(id.toUpperCase()) ?? null) : null),
    nodeOf: (id) => (id ? (nodes.get(id) ?? null) : null),
    shockLabel: shockLabelOf(doc.meta, doc.meta.headlineShock),
    tx: taxonomyOf(doc.meta),
    title: (c, full = false) => compactTitle(c, find, { full }),
    strategic: strategicOf(doc),
  };
}

export default function AibLayout() {
  const m = useAibManifest();
  const d = useCompactsDoc(m.data);
  const loc = useLocation();
  const nav = useNavigate();
  const data = useMemo(() => (m.data && d.data ? buildCtx(m.data, d.data) : null), [m.data, d.data]);
  const available = useMemo(() => (m.data && d.data ? hasScenarios(d.data) : false), [m.data, d.data]);
  const toggle = scenarioOfSearch(loc.search);
  const scenario = effectiveScenario(toggle, available);
  // the latest location, so setScenario (and the context) stay the same object while the URL changes
  const here = useRef(loc);
  useLayoutEffect(() => {
    here.current = loc;
  }, [loc]);
  const setScenario = useCallback(
    (s: AibToggle) => {
      const l = here.current;
      nav({ pathname: l.pathname, search: setScenarioSearch(l.search, s), hash: l.hash }, { replace: true, preventScrollReset: true });
    },
    [nav],
  );
  // drop a stray scn: a value the app does not write, or any scn in a bundle without scenarios
  const stray = hasStrayScenario(loc.search) || (!!data && !available && hasScenarioKey(loc.search));
  useEffect(() => {
    if (!stray) return;
    const search = data && !available ? clearScenarioSearch(loc.search) : setScenarioSearch(loc.search, toggle);
    nav({ pathname: loc.pathname, search, hash: loc.hash }, { replace: true, preventScrollReset: true });
  }, [stray, data, available, toggle, nav, loc.pathname, loc.search, loc.hash]);
  const ctx = useMemo<AibCtx | null>(
    // a bundle without scenarios shows Decoupling and writes no scn on its links
    () =>
      data
        ? {
            ...data,
            scenario,
            hasScenarios: available,
            setScenario,
            href: (to: string) => withScenario(to, available ? scenario : DEFAULT_TOGGLE),
            strategicScenario: data.strategic ? strategicScenarioFor(data.strategic, scenario) : null,
          }
        : null,
    [data, scenario, available, setScenario],
  );
  const failed = m.status === 'error' && m.error ? { error: m.error, retry: m.retry } : d.status === 'error' && d.error ? { error: d.error, retry: d.retry } : null;
  if (failed) {
    return (
      <div className={PAGE}>
        <ModuleTitle />
        <Nav firstId={null} />
        <LoadProblem error={failed.error} retry={failed.retry} />
      </div>
    );
  }
  if (!ctx) {
    return (
      <div className={PAGE}>
        <ModuleTitle />
        <Nav firstId={null} />
        <Loading label={STATE.aibLoading} />
      </div>
    );
  }
  return (
    <Ctx.Provider value={ctx}>
      <Nav firstId={ctx.doc.compacts[0]?.id ?? null} />
      {ctx.hasScenarios && <ScenarioHeader ctx={ctx} />}
      <Outlet />
    </Ctx.Provider>
  );
}
