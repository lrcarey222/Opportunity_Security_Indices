// The Sectoral Compacts layout: loads data/aib/manifest.json and the compacts document it names (both
// checked against the contract), registers the tier-2 nav ("Sectoral Compacts views") and renders the
// view. No engine, no other module's chunk. A missing manifest shows the module-level "No compacts data"
// screen with the developer hint; a newer schema asks for a reload; other errors offer Retry. Views read
// the data through useAibCtx().

import { createContext, useContext, useMemo } from 'react';
import { Outlet, useLocation } from 'react-router';
import { Loading, Notice } from '../components/ui.tsx';
import { useRegisterModuleNav } from '../shell/ModuleNav.tsx';
import { useTitle } from '../shell/title.ts';
import { countryNamesOf, type AibCompact, type AibDoc, type AibManifest, type AibNode } from './contract.ts';
import { useAibManifest, useCompactsDoc } from './data/hooks.ts';
import type { AibLoadError } from './data/store.ts';
import { nameOf } from './derive/format.ts';
import { aibNavItems } from './nav.ts';

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
}

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
  useRegisterModuleNav('aib', aibNavItems(loc.pathname, firstId));
  return null;
}

function LoadProblem({ error, retry }: { error: AibLoadError; retry: () => void }) {
  if (error.code === 'NO_DATA') {
    return (
      <Notice kind="info" title="No compacts data">
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
    <Notice kind="error" title="The compacts data could not be loaded" action={<button type="button" className="btn btn-ghost btn-sm" onClick={retry}>Retry</button>}>
      {error.message}
    </Notice>
  );
}

function buildCtx(manifest: AibManifest, doc: AibDoc): AibCtx {
  const names = countryNamesOf(doc);
  const byId = new Map(doc.compacts.map((c) => [c.id.toUpperCase(), c]));
  const nodes = new Map(doc.nodes.map((n) => [n.id, n]));
  const shock = doc.meta.shocks.find((s) => s.id === doc.meta.headlineShock);
  return {
    manifest,
    doc,
    names,
    name: (iso) => nameOf(names, iso),
    compactOf: (id) => (id ? (byId.get(id.toUpperCase()) ?? null) : null),
    nodeOf: (id) => (id ? (nodes.get(id) ?? null) : null),
    shockLabel: shock?.label ?? doc.meta.headlineShock,
  };
}

export default function AibLayout() {
  const m = useAibManifest();
  const d = useCompactsDoc(m.data);
  const ctx = useMemo(() => (m.data && d.data ? buildCtx(m.data, d.data) : null), [m.data, d.data]);
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
        <Loading label={m.data ? 'Loading the compacts' : 'Loading the compacts data'} />
      </div>
    );
  }
  return (
    <Ctx.Provider value={ctx}>
      <Nav firstId={ctx.doc.compacts[0]?.id ?? null} />
      <Outlet />
    </Ctx.Provider>
  );
}
