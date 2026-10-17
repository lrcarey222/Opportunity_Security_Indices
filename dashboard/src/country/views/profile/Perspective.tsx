// The Perspective of the Country profile (IA refactor 2026-10-16, brief s.10 and s.25; plan.md 4, 5.3):
// a compact "Perspective: NEIS baseline ▾" control in the head that opens the existing DialPanel (four
// dials, presets, "Reset to NEIS baseline", the advanced settings), with a live readout of the top
// priorities so a reader sees what the dials do without leaving the page. The dials re-weight
// preference-adjusted priorities only (the Strategic Index, partner rankings); facts never move.
//
// EngineSi sits inside the page's one EngineScope and turns the engine's compare.si rows into the
// PerspectiveSi context (null map at the NEIS baseline: the fact sheet's identical numbers are used).
// Before the scope is mounted (a baseline visit) nothing here touches the engine.

import { useMemo, type ReactNode } from 'react';
import { DialPanel } from '../../../components/DialPanel.tsx';
import { Loading, RankChip } from '../../../components/ui.tsx';
import { perspectiveSummary } from '../../../shell/PerspectiveLine.tsx';
import { useAdvancedChanged, useCore, useScenario, useScenarioOptional } from '../../../state/EngineProvider.tsx';
import { rowsOf } from '../../../views/common.tsx';
import { siMapOf, siOrder } from '../../derive/framing.ts';
import { proseName } from '../../derive/text.ts';
import { BASELINE_SI, BasisTag, LOADING_SI, perspectiveNote, PerspectiveSiProvider, usePerspectiveSi, useProfile, type PerspectiveSi } from './common.tsx';

/** Inside EngineScope: the Strategic Index of the economy under the reader's Perspective. */
export function EngineSi({ iso3, children }: { iso3: string; children: ReactNode }) {
  const { isBaseline } = useScenario();
  const core = useCore();
  const data = core.data;
  const fresh = core.fresh;
  const value = useMemo<PerspectiveSi>(() => {
    if (isBaseline) return BASELINE_SI;
    if (!data) return LOADING_SI;
    const map = siMapOf(rowsOf(data.compare.si), iso3);
    return { status: 'custom', map: map.size ? map : null, updating: !fresh };
  }, [isBaseline, data, fresh, iso3]);
  return <PerspectiveSiProvider value={value}>{children}</PerspectiveSiProvider>;
}

function LiveSummary({ open }: { open: boolean }) {
  const { scenario, isBaseline, reset } = useScenario();
  const ch = useAdvancedChanged();
  const summary = perspectiveSummary(scenario.dials, ch.any);
  if (isBaseline) return null;
  return (
    <>
      {summary && <span className="text-sm tabular text-paper-dim">{summary}</span>}
      {!open && (
        <button type="button" className="btn btn-ghost btn-sm" onClick={reset}>
          Reset to NEIS baseline
        </button>
      )}
    </>
  );
}

/** The top five priorities under the Perspective, with their rank change against the NEIS baseline. */
function PriorityReadout() {
  const { sheet } = useProfile();
  const si = usePerspectiveSi();
  const rows = useMemo(() => siOrder(sheet, si.map).slice(0, 5), [sheet, si.map]);
  const name = proseName(sheet.meta.country);
  return (
    <div className="mt-4 border-t hairline pt-3" aria-live="polite">
      <div className="mb-1.5 flex flex-wrap items-center gap-2">
        <BasisTag kind="prefs" note={perspectiveNote(si)} />
        {si.updating && si.status === 'custom' && <span className="text-xs text-paper-mute">updating…</span>}
      </div>
      <h3 className="text-sm font-medium text-paper">Top strategic priorities under this perspective</h3>
      {rows.length ? (
        <ol className="mt-1.5 grid gap-x-6 gap-y-1 text-sm sm:grid-cols-2 xl:grid-cols-5" data-testid="perspective-top">
          {rows.map((c, i) => {
            const o = si.map?.get(c.key);
            return (
              <li key={c.key} className="flex items-center gap-1.5 text-paper-dim">
                <span className="tabular text-paper-mute">{i + 1}.</span>
                <span className="text-paper">{c.label}</span>
                {o && <RankChip change={o.delta} isNew={o.isNew} />}
              </li>
            );
          })}
        </ol>
      ) : (
        <p className="mt-1 text-sm text-paper-mute">{name} has no Strategic Index (no International Energy Agency policy records), so the dials do not reorder its sectors.</p>
      )}
      <p className="mt-2 text-xs text-paper-mute">
        Energy Security, Economic Opportunity, the four corners, trade and the Allied Industrial Base shock and compacts results stay as they are. The Strategic priority column in Sector position and
        &ldquo;Make bets&rdquo; below follow the dials.
      </p>
    </div>
  );
}

/** The head's Perspective control. Opening it mounts the model (the page's EngineScope). */
export function PerspectiveControl() {
  const p = useProfile();
  const sc = useScenarioOptional();
  const open = p.isOpen?.('perspective') ?? false;
  const custom = sc ? !sc.isBaseline : !!p.customInUrl;
  const toggle = () => {
    if (!open) p.wantEngine?.();
    p.setOpen?.('perspective', !open);
  };
  return (
    <div id="perspective" className="card mb-8 scroll-mt-16 p-4">
      <div className="flex flex-wrap items-center gap-x-4 gap-y-2">
        <button type="button" className="btn btn-ghost" aria-expanded={open} aria-controls="perspective-panel" onClick={toggle}>
          Perspective: {custom ? 'custom' : 'NEIS baseline'} <span aria-hidden="true">{open ? '▴' : '▾'}</span>
        </button>
        {sc ? <LiveSummary open={open} /> : custom ? <span className="text-sm text-paper-mute">Loading the model to apply the dials in the link…</span> : null}
        <p className="basis-full text-xs leading-snug text-paper-mute">
          Four dials (Hawkishness, Bullishness, Populism, Climate) re-weight strategic priorities, sector rankings and how attractive partners look. They never change observed or
          modelled facts: trade flows, concentration, production, shock coverage, capacity gaps and the Allied Industrial Base shock and compacts results.
        </p>
      </div>
      {open && (
        <div id="perspective-panel" className="mt-3 border-t hairline pt-1">
          {sc ? (
            <>
              <DialPanel layout="wide" />
              <PriorityReadout />
            </>
          ) : (
            <Loading label="Loading the model" />
          )}
        </div>
      )}
    </div>
  );
}

/** The Perspective in the section rail (wide screens): the current state, Change and Reset. */
export function PerspectiveTag() {
  const p = useProfile();
  const sc = useScenarioOptional();
  const custom = sc ? !sc.isBaseline : !!p.customInUrl;
  return (
    <div className="mt-5 border-t hairline pt-3 text-xs">
      <div className="eyebrow mb-1.5">Perspective</div>
      <span className={custom ? 'tag tag-framework' : 'tag border-signal text-paper'}>{custom ? 'Custom' : 'NEIS baseline'}</span>
      <div className="mt-2 flex flex-wrap gap-x-3 gap-y-1">
        <button type="button" className="link" onClick={() => p.reveal?.('perspective')}>
          Change
        </button>
        {sc && custom && (
          <button type="button" className="link" onClick={sc.reset}>
            Reset
          </button>
        )}
      </div>
    </div>
  );
}
