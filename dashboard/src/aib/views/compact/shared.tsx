// Shared pieces of one compact's page: its context (the compact, its nodes, names, meta) and an
// economy's name that never prints a bare code (an <abbr> with the code when the data have no name).
//
// The scenario toggle (2026-10-19, additive): the context also carries the toggled scenario, whether the
// bundle has the scenarios, the compact as that scenario solves it (`view`: Decoupling the compact itself;
// Managed Interdependence the compact with its scenario file's coalition, tiers, roles, metrics, path and
// relationships, or null while the file loads, when it fails or is missing, or when the scenario has no
// coalition for it) with its state and reason, the Managed Interdependence file when loaded, and href()
// (an href with the toggle carried). `c` stays the main document's compact (Decoupling): the sections that
// follow the toggle read `view`. ScenarioGate renders the loading, failed, missing and no-coalition states.

import { createContext, useContext, type ReactNode } from 'react';
import { Loading, Notice } from '../../../components/ui.tsx';
import type { AibCompact, AibManagedScenarioDoc, AibMeta, AibNode } from '../../contract.ts';
import { scenarioInfo } from '../../derive/scenarios.ts';
import type { Taxonomy } from '../../derive/taxonomy.ts';
import type { AibToggle } from '../../scenarioUrl.ts';

/** The state of the compact under the toggled scenario. */
export interface CompactViewState {
  /** ready: `view` is there; loading / error / missing: the scenario's file; none: the scenario has no coalition for this compact (`reason`) */
  status: 'ready' | 'loading' | 'error' | 'missing' | 'none';
  /** why there is no coalition (status none), in plain words */
  reason: string | null;
  retry: () => void;
}

export interface CompactCtx {
  c: AibCompact;
  /** the compact's nodes, in its own order */
  nodes: AibNode[];
  meta: AibMeta;
  names: Readonly<Record<string, string>>;
  name: (iso3: string | null | undefined) => string;
  nodeLabel: (id: string) => string;
  shockLabel: string;
  /** the node taxonomy (dictionary sectors or OSI technologies) */
  tx: Taxonomy;
  /** the toggled scenario (Decoupling for a bundle without the scenarios) */
  scenario: AibToggle;
  /** the bundle carries the scenarios (the comparisons are shown) */
  hasScenarios: boolean;
  /** the compact as the toggled scenario solves it (see viewState when null) */
  view: AibCompact | null;
  viewState: CompactViewState;
  /** the Managed Interdependence file, when the scenario is toggled and the file is loaded */
  managedDoc: AibManagedScenarioDoc | null;
  /** an href with the toggled scenario carried */
  href: (to: string) => string;
}

const Ctx = createContext<CompactCtx | null>(null);
export const CompactProvider = Ctx.Provider;

export function useCompact(): CompactCtx {
  const v = useContext(Ctx);
  if (!v) throw new Error('useCompact outside a compact page');
  return v;
}

/** An economy by name; its code in an <abbr> when the data have no name for it. */
export function Economy({ iso3 }: { iso3: string | null | undefined }) {
  const { names } = useCompact();
  if (!iso3) return <>–</>;
  const n = names[iso3];
  return n ? <>{n}</> : <abbr title={`Economy code ${iso3}`}>{iso3}</abbr>;
}

/**
 * The compact as the toggled scenario solves it, or the state that stands in for it: loading the scenario's
 * file, a failure (Retry), a file the export does not have, or no coalition under the scenario (its reason).
 * `what`: what the card would show ("the coalition").
 */
export function ScenarioGate({ what, children }: { what: string; children: (view: AibCompact) => ReactNode }) {
  const { view, viewState, scenario, meta } = useCompact();
  if (view) return <>{children(view)}</>;
  const label = scenarioInfo(meta, scenario).label;
  switch (viewState.status) {
    case 'loading':
      return <Loading label={`Loading ${what} under ${label}`} />;
    case 'error':
      return (
        <Notice
          kind="error"
          title={`${label} could not be loaded`}
          action={
            <button type="button" className="btn btn-ghost btn-sm" onClick={viewState.retry}>
              Retry
            </button>
          }
        >
          The scenario&apos;s file did not load, so {what} under it cannot be shown. The comparisons on this page still use the main data.
        </Notice>
      );
    case 'missing':
      return (
        <Notice kind="info" title={`Not in this export`}>
          This data build has no file for {label}, so {what} under it cannot be shown.
        </Notice>
      );
    default:
      return (
        <p className="text-sm text-paper-dim" data-testid="scenario-no-coalition">
          <span className="text-paper">No coalition under {label}:</span> {viewState.reason ?? 'the scenario does not run on this compact.'}
        </p>
      );
  }
}
