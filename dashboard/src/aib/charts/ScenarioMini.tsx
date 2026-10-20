// The three-way mini bar of a compact card on the Sectoral Compacts landing (2026-10-19): one thin row per
// scenario, Baseline, Managed Interdependence and Decoupling in that order, each the compact's allied demand
// split to scale (kept / restored by trade / new capacity: the bar of the shared three-way charts), with
// the share served after optimized trade in text beside it; the toggled scenario at full strength and
// marked current, the others dimmed. Fits a narrow card at any width: a container query puts the label on
// its own line above the bar and figure where the card is narrower than 21rem, else in an 11rem column beside
// them (the bar keeps at least about 100 px either way); the full name always shows, never truncated. Each bar has a full aria-label and the scenario's words as a tooltip; the legend sits once above
// the cards (CoverageLegend), so colour is never the only carrier.

import { isNum } from '../../lib/format.ts';
import type { AibMeta } from '../contract.ts';
import type { DemandSplit } from '../derive/diversify.ts';
import { fmtShare } from '../derive/format.ts';
import { scenarioInfo, SCENARIO_ORDER, type AibScenarioId } from '../derive/scenarios.ts';
import { coverageText, ScenarioSwatch } from './ScenarioCompare.tsx';
import { DemandSplitBar } from './DiversifyChart.tsx';

export interface MiniRow {
  id: AibScenarioId;
  split: DemandSplit | null;
  /** a note for the aria-label and the tooltip ("unchanged: no node above its benchmark") */
  note?: string | null;
}

export function ScenarioMiniBars({ rows, highlight, label, meta }: { rows: readonly MiniRow[]; highlight: AibScenarioId; label: string; meta?: AibMeta | null }) {
  const ordered = SCENARIO_ORDER.flatMap((id) => rows.filter((r) => r.id === id));
  return (
    <ul className="@container space-y-1" aria-label={label} data-testid="scenario-mini">
      {ordered.map((r) => {
        const s = scenarioInfo(meta, r.id);
        const shown = r.id === highlight;
        const served = r.split ? r.split.kept + r.split.trade : null;
        const text = r.split ? coverageText(r.split) : 'no coverage figures';
        return (
          <li
            key={r.id}
            className="grid grid-cols-[minmax(0,1fr)_2.75rem] items-center gap-x-2 gap-y-0.5 text-xs @min-[21rem]:grid-cols-[11rem_minmax(0,1fr)_2.75rem]"
            data-scenario={r.id}
            aria-current={shown ? 'true' : undefined}
            title={`${s.label}: ${s.words}`}
          >
            <span className="col-span-2 inline-flex min-w-0 items-center gap-1.5 @min-[21rem]:col-span-1">
              <ScenarioSwatch id={r.id} size={8} />
              <span className={`min-w-0 break-words ${shown ? 'font-medium text-paper' : 'text-paper-mute'}`}>{s.label}</span>
            </span>
            <div className={shown ? '' : 'opacity-60'}>
              {r.split ? (
                <DemandSplitBar split={r.split} label={`${s.label}: ${text}${r.note ? ` (${r.note})` : ''}`} className="h-2" />
              ) : (
                <div className="h-2 w-full bg-ink-700/40" role="img" aria-label={`${s.label}: ${r.note ?? text}`} />
              )}
            </div>
            <span className={`text-right tabular ${shown ? 'text-paper' : 'text-paper-dim'}`}>
              {isNum(served) ? fmtShare(served, 0) : '–'}
              <span className="sr-only"> served after trade</span>
            </span>
          </li>
        );
      })}
    </ul>
  );
}
