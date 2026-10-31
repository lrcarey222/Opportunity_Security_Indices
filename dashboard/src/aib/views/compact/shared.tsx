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
//
// The strategic layer (2026-10-20): a compact candidate is a Workstream of a Strategic Sectoral Compact;
// WorkstreamParent says which (a link up to it), how much of its allied demand is there, the others it
// spans, whether it is active under the toggled scenario and, when it was filed by review, why.

import { createContext, useContext, type ReactNode } from 'react';
import { Link } from 'react-router';
import { Disclosure, Loading, Notice } from '../../../components/ui.tsx';
import type { AibCompact, AibManagedScenarioDoc, AibMeta, AibNode, AibStrategic } from '../../contract.ts';
import { fmtShare } from '../../derive/format.ts';
import { scenarioInfo } from '../../derive/scenarios.ts';
import { strategicCompactOf, strategicScenarioDef, workstreamHomeOf, workstreamOf } from '../../derive/strategic.ts';
import type { Taxonomy } from '../../derive/taxonomy.ts';
import { strategicCompactHref } from '../../nav.ts';
import type { AibToggle } from '../../scenarioUrl.ts';

/**
 * A workstream's place in the strategic layer: the Strategic Sectoral Compact it is filed under (a link),
 * the share of its allied demand there, the other compacts it spans, whether it is active under the
 * toggled scenario (`scenario`: a strategic scenario id) and, when it was filed by review, why (in a
 * disclosure). Nothing when the bundle has no strategic layer or the layer does not list the workstream.
 */
export function WorkstreamParent({ s, id, scenario, href }: { s: AibStrategic | null; id: string; scenario: string | null; href: (to: string) => string }) {
  const ws = s ? workstreamOf(s, id) : null;
  if (!s || !ws) return null;
  const home = workstreamHomeOf(s, id);
  const also = ws.alsoSpans.flatMap((x) => {
    const k = strategicCompactOf(s, x);
    return k ? [k] : [];
  });
  const label = scenario ? (strategicScenarioDef(s, scenario)?.label ?? scenario) : null;
  const active = scenario ? ws.active.includes(scenario) : null;
  return (
    <div className="mt-3 max-w-3xl text-sm text-paper-dim" data-testid="workstream-parent">
      <p>
        {home ? (
          <>
            A workstream of the Strategic Sectoral Compact{' '}
            <Link className="link" to={href(strategicCompactHref(home.id))}>
              {home.name}
            </Link>
            {ws.share !== null ? `, which holds ${fmtShare(ws.share, 0)} of its allied demand` : ''}
            {also.length > 0 && (
              <>
                ; it also spans{' '}
                {also.map((k, i) => (
                  <span key={k.id}>
                    {i > 0 ? ', ' : ''}
                    <Link className="link" to={href(strategicCompactHref(k.id))}>
                      {k.name}
                    </Link>
                  </span>
                ))}
              </>
            )}
            .
          </>
        ) : (
          'A workstream filed under no strategic compact.'
        )}
        {label !== null && <> {active ? `Active under ${label}.` : `Not active under ${label}: none of its supply-chain steps needs action there.`}</>}
      </p>
      {ws.overrideReason && (
        <div className="mt-1">
          <Disclosure summary="Why it is filed under this compact">
            <p className="text-sm text-paper-dim">{ws.overrideReason}</p>
          </Disclosure>
        </div>
      )}
    </div>
  );
}

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
  /**
   * the compact's display title (compactTitle: the OSI technologies and labels of its nodes), for headings,
   * summaries, captions and aria-labels; c.name (built from dictionary sectors) is provenance only (design A1)
   */
  displayName: string;
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
