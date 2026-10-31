// "Show a variable" under each variable table of the fence sections (2026-10-30, the user's request: choose one
// of the variables that make up a fence's analysis and see its chart from the Sectors section). The picker
// lists the analysis' variables (the table's columns, then the ones behind it; posture/variableCharts.ts);
// picking one shows, beneath, the Sector explore chart of that variable for the node of the row the reader has
// selected (else the table's first row), with the economy highlighted, and "Open in Sectors". A variable with
// no Sector chart (a composite score, a node attribute, data the Sector bundle does not carry, or a measure
// that covers other stages or technologies) says so and why.
//
// Lazy: the embedded explorer is its own chunk (sector/views/explore/EmbeddedExplore.tsx) and the Sector data
// load only once a variable with a chart is picked, so the Country profile's first view does not grow.

import { lazy, Suspense, useState } from 'react';
import { Loading, Select } from '../../../components/ui.tsx';
import type { FenceNode } from '../../../posture/fences.ts';
import { stageDisplay } from '../../../posture/index.ts';
import { chartsAt, fenceVariables, type VariableAnalysis } from '../../../posture/variableCharts.ts';
import { ViewErrorBoundary } from '../../../shell/ViewErrorBoundary.tsx';

const EmbeddedExplore = lazy(() => import('../../../sector/views/explore/EmbeddedExplore.tsx'));

const GROUP_WORDS = { table: 'In the table', behind: 'Behind the analysis' } as const;

export interface VariableChartProps {
  /** the picker's id (its table's), for test ids and labels */
  id: string;
  analysis: VariableAnalysis;
  /** the table's rows, in its order */
  rows: readonly FenceNode[];
  /** the node the drawer has open (?node=), if any */
  selected: string | null;
  /** the economy (ISO3), highlighted in the chart */
  iso3: string;
  /** the prize: Strategic Industrial Value (true) or the market measure, and its label */
  siv?: boolean;
  prizeLabel?: string;
}

/** The Show-a-variable picker and, once a variable is picked, its Sector chart. */
export function VariableChart({ id, analysis, rows, selected, iso3, siv, prizeLabel }: VariableChartProps) {
  const [pick, setPick] = useState('');
  const vars = fenceVariables(analysis, { siv, prizeLabel });
  const variable = vars.find((v) => v.id === pick) ?? null;
  const chosen = rows.find((n) => n.nodeKey === selected) ?? null;
  const n = chosen ?? rows[0] ?? null;
  if (!n) return null;
  const nodeLabel = `${n.tech} · ${stageDisplay(n.stage)}`;
  const charts = variable ? chartsAt(variable, n) : [];
  return (
    <div className="space-y-3 border-t hairline pt-3" data-testid={`${id}-variables`}>
      <div className="flex flex-wrap items-end gap-x-4 gap-y-2">
        <Select
          label="Show a variable"
          value={pick}
          options={[{ value: '', label: 'Choose a variable for its Sector chart' }, ...vars.map((v) => ({ value: v.id, label: v.label, group: GROUP_WORDS[v.group] }))]}
          onChange={setPick}
          className="min-w-[16rem] max-w-full"
        />
        <p className="pb-1 text-xs text-paper-mute" data-testid="variable-chart-node" data-node={n.nodeKey}>
          For {nodeLabel}
          {chosen ? ', the selected row.' : ', the first row; select a row to show another step.'}
        </p>
      </div>
      {variable &&
        (variable.none || !charts.length ? (
          <p className="text-sm text-paper-dim" data-testid="variable-chart-none">
            <span className="text-paper">No Sector chart for this variable{variable.none ? '' : ` at ${nodeLabel}`}.</span>{' '}
            {variable.none ?? 'The Sector measures that chart it cover other stages or technologies.'}
          </p>
        ) : (
          <ViewErrorBoundary resetKey={`${variable.id}|${n.nodeKey}`}>
            <Suspense fallback={<Loading label="Loading the Sector chart…" />}>
              {/* keyed by variable and node: a new pick starts on its own measure, item and chart */}
              <EmbeddedExplore key={`${variable.id}|${n.nodeKey}`} charts={charts} node={{ tech: n.tech, stage: n.stage, label: nodeLabel }} iso3={iso3} variable={variable.label} />
            </Suspense>
          </ViewErrorBoundary>
        ))}
    </div>
  );
}
