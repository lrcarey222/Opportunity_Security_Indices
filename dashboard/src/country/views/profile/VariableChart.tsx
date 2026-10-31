// "Show a variable" under each variable table of the fence sections (2026-10-30, the user's request: choose one
// of the variables that make up a fence's analysis and see its chart from the Sectors section; reworked
// 2026-10-31: "it's only showing the composites. It should show the variables that make up the composites").
// Each option is one indicator (one Sector chart, posture/variableCharts.ts), grouped by the family it feeds in
// the composite's own structure ("Investment momentum (market prize)"); the composite and family scores are not
// options. Only the indicators with a chart at the step of the row the reader has selected (else the table's
// first row) are offered; one line under the picker names the families it cannot offer (no Sector measure, or
// none at this step) and where to see them instead (the row's details; the monthly trade card for trade
// momentum). A pick shows, beneath, that indicator's Sector explore chart for the step, the economy
// highlighted, with "Open in Sectors"; when the reader moves to a row where the pick has no chart, one line says
// so and the picker stays usable.
//
// Lazy: the embedded explorer is its own chunk (sector/views/explore/EmbeddedExplore.tsx) and the Sector data
// load only once an indicator is picked, so the Country profile's first view does not grow.

import { lazy, Suspense, useState } from 'react';
import { Loading, Select } from '../../../components/ui.tsx';
import type { FenceNode } from '../../../posture/fences.ts';
import { stageDisplay } from '../../../posture/index.ts';
import { chartlessAt, chartlessLine, familyGroup, fenceFamilies, findVariable, variableOptions, type VariableAnalysis } from '../../../posture/variableCharts.ts';
import { ViewErrorBoundary } from '../../../shell/ViewErrorBoundary.tsx';

const EmbeddedExplore = lazy(() => import('../../../sector/views/explore/EmbeddedExplore.tsx'));

export interface VariableChartProps {
  /** the picker's id (its table's), for test ids and labels */
  id: string;
  analysis: VariableAnalysis;
  /** the table's rows, in its order */
  rows: readonly FenceNode[];
  /** the step the drawer has open (?node=), if any */
  selected: string | null;
  /** the economy (ISO3), highlighted in the chart */
  iso3: string;
  /** the prize: Strategic Industrial Value (true) or the market measure, and its label */
  siv?: boolean;
  prizeLabel?: string;
}

/** The Show-a-variable picker and, once an indicator is picked, its Sector chart. */
export function VariableChart({ id, analysis, rows, selected, iso3, siv, prizeLabel }: VariableChartProps) {
  const [pick, setPick] = useState('');
  const chosen = rows.find((n) => n.nodeKey === selected) ?? null;
  const n = chosen ?? rows[0] ?? null;
  if (!n) return null;
  const families = fenceFamilies(analysis, { siv, prizeLabel });
  const options = variableOptions(families, n);
  const offered = options.find((o) => o.value === pick) ?? null;
  // a pick the step does not offer (the reader moved to another row): said, never shown
  const stale = offered ? null : findVariable(families, pick);
  const chartless = chartlessAt(families, n);
  const line = chartlessLine(chartless, { selected: !!chosen });
  const nodeLabel = `${n.tech} · ${stageDisplay(n.stage)}`;
  return (
    <div className="space-y-3 border-t hairline pt-3" data-testid={`${id}-variables`}>
      <div className="flex flex-wrap items-end gap-x-4 gap-y-2">
        <Select
          label="Show a variable"
          value={offered ? pick : ''}
          options={[
            { value: '', label: options.length ? 'Choose a variable for its Sector chart' : 'No Sector chart for this step' },
            ...options.map((o) => ({ value: o.value, label: o.label, group: o.group })),
          ]}
          onChange={setPick}
          className="min-w-[16rem] max-w-full"
        />
        <p className="pb-1 text-xs text-paper-mute" data-testid="variable-chart-node" data-node={n.nodeKey}>
          For {nodeLabel}
          {chosen ? ', the selected row.' : ', the first row; select a row to show another step.'}
        </p>
      </div>
      {line && (
        <p className="text-xs text-paper-mute" data-testid="variable-chart-chartless" title={chartless.map((c) => (c.family.none ? `${c.family.label}: ${c.family.none}` : null)).filter(Boolean).join(' ') || undefined}>
          {line}
        </p>
      )}
      {stale && (
        <p className="text-sm text-paper-dim" data-testid="variable-chart-stale">
          <span className="text-paper">{stale.chart.label}</span>, under {familyGroup(stale.family)}, has no Sector chart for {nodeLabel}; choose another variable.
        </p>
      )}
      {offered && (
        <ViewErrorBoundary resetKey={`${offered.value}|${n.nodeKey}`}>
          <Suspense fallback={<Loading label="Loading the Sector chart…" />}>
            {/* keyed by indicator and step: a new pick starts on its own measure, item and chart */}
            <EmbeddedExplore key={`${offered.value}|${n.nodeKey}`} chart={offered.chart} node={{ tech: n.tech, stage: n.stage, label: nodeLabel }} iso3={iso3} family={offered.family.label} />
          </Suspense>
        </ViewErrorBoundary>
      )}
    </div>
  );
}
