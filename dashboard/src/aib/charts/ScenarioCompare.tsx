// The shared three-way scenario charts (2026-10-19): every comparison shows Baseline, Managed
// Interdependence and Decoupling, in that order, and highlights the toggled one (a ring, the word
// "shown" and full strength; the others are drawn at reduced strength, their values still printed).
//
//   ScenarioCoverageCompare  three rows of the demand split to scale: kept / restored by trade / new
//                            capacity (demandSplit; the Diversification chart's bar and colours). Baseline
//                            is a full "kept" bar (trade as it is today).
//   ScenarioBars             three bars of one scalar on a shared scale (lost supply, the gap in USD, ...)
//                            in the scenario's own colour (Baseline hollow).
//   ScenarioClassBars        three stacked bars of a USD total split by class (the class colours and
//                            marks), on a shared scale: where each scenario's lost supply sits.
//   ScenarioSwatch / ScenarioName  the scenario's mark and name.
//
// HTML with percentage widths, so each reflows at any width (on a phone the label, the bar and the values
// stack). Every value is printed beside its bar and each bar has a full aria-label, so colour is never the
// only carrier and the text is the table twin; each mark has a tooltip (title).

import type { ReactNode } from 'react';
import { HATCH_STYLE } from '../../theme/palette.ts';
import { SCENARIO_CHART_LABEL } from '../../content/vocabulary.ts';
import { isNum } from '../../lib/format.ts';
import type { AibMeta } from '../contract.ts';
import { classInfo } from '../derive/classes.ts';
import type { DemandSplit } from '../derive/diversify.ts';
import { fmtShare } from '../derive/format.ts';
import { classMixText, scenarioInfo, SCENARIO_ORDER, type AibScenarioId, type ScenarioFigures } from '../derive/scenarios.ts';
import { DemandSplitBar, DIVERSIFY_MARK } from './DiversifyChart.tsx';

/**
 * The parts of the coverage split in words (legend, text): content/vocabulary.ts SCENARIO_CHART_LABEL ("Recovered
 * through trade", not "allied trade": the suppliers that recover it can be any economy but the one removed).
 */
export const COVERAGE_WORDS = { kept: SCENARIO_CHART_LABEL.kept, trade: SCENARIO_CHART_LABEL.restoredByTrade, build: SCENARIO_CHART_LABEL.newCapacity } as const;

/** "still supplied 51% · recovered through trade 1% · requires new capacity 48%". */
export const coverageText = (s: DemandSplit): string =>
  `still supplied ${fmtShare(s.kept, 0)} · recovered through trade ${fmtShare(s.trade, 0)} · requires new capacity ${fmtShare(s.build, 0)}`;

const pct = (x: number, max = 1) => `${max > 0 ? Math.max(0, Math.min(1, x / max)) * 100 : 0}%`;

/** A scenario's mark: its colour filled, Baseline hollow. */
export function ScenarioSwatch({ id, size = 10 }: { id: AibScenarioId; size?: number }) {
  const s = scenarioInfo(null, id);
  const style = s.mark === 'fill' ? { background: s.color } : { border: `2px solid ${s.color}` };
  return <span aria-hidden="true" className="inline-block shrink-0" style={{ width: size + 4, height: size, boxSizing: 'border-box', ...style }} data-scenario-swatch={id} />;
}

/** A scenario's swatch and name; `shown`: the toggled one (the word "shown" beside it). */
export function ScenarioName({ id, meta, shown = false }: { id: AibScenarioId; meta?: AibMeta | null; shown?: boolean }) {
  const s = scenarioInfo(meta, id);
  return (
    <span className="inline-flex min-w-0 items-center gap-1.5" title={s.words}>
      <ScenarioSwatch id={id} />
      <span className={`min-w-0 leading-tight ${shown ? 'font-medium text-paper' : 'text-paper-dim'}`}>{s.label}</span>
      {shown && <span className="eyebrow shrink-0 text-paper-mute">shown</span>}
    </span>
  );
}

const ROW = 'grid gap-x-4 gap-y-1 px-2 py-1.5 sm:grid-cols-[minmax(9rem,13rem)_minmax(0,1fr)_minmax(0,15rem)] sm:items-center';
const rowClass = (shown: boolean) => `${ROW} ${shown ? 'bg-white/[0.04] ring-1 ring-inset ring-white/25' : ''}`;

/** Order three rows (or fewer) as the comparisons show them. */
const ordered = <T extends { id: AibScenarioId }>(rows: readonly T[]): T[] => SCENARIO_ORDER.flatMap((id) => rows.filter((r) => r.id === id));

/** The default note of a coverage row: Baseline before any shock; a scenario that did not run, unchanged; no figures. */
function coverageNote(r: Pick<ScenarioFigures, 'id' | 'ran' | 'split'>): string | null {
  if (!r.split) return 'no figures for the share of demand met';
  if (r.id === 'baseline') return 'trade as it is today, before any disruption';
  if (r.id === 'managed' && !r.ran) return 'unchanged: nothing is above its benchmark here';
  return null;
}

export interface ScenarioCoverageCompareProps {
  /** the scenarios' figures (nodeScenarios, compactScenarios, scenarioTotalsAll; any order) */
  rows: readonly Pick<ScenarioFigures, 'id' | 'ran' | 'split'>[];
  /** the toggled scenario */
  highlight: AibScenarioId;
  /** what the chart shows ("Coverage of the allied demand for lithium-ion cells") */
  label: string;
  meta?: AibMeta | null;
  /** a note per scenario in place of the default one (e.g. why Managed Interdependence did not run) */
  notes?: Partial<Record<AibScenarioId, string | null>>;
  /** hide the legend (when the page shows it once for several charts) */
  legend?: boolean;
  /** a thinner bar, for node lists */
  compact?: boolean;
}

/** The legend of the coverage split. */
export function CoverageLegend() {
  return (
    <ul className="flex flex-wrap gap-x-4 gap-y-1 text-xs text-paper-mute" aria-label="Coverage parts">
      {(['kept', 'trade', 'build'] as const).map((k) => (
        <li key={k} className="inline-flex items-center gap-1.5">
          <span aria-hidden="true" className="inline-block h-2.5 w-3.5" style={{ background: DIVERSIFY_MARK[k] }} />
          {COVERAGE_WORDS[k]}
        </li>
      ))}
    </ul>
  );
}

/**
 * The three-row coverage comparison: each scenario's allied demand split into kept / restored by trade /
 * new capacity, to scale (Baseline a full "kept" bar), the toggled scenario highlighted.
 */
export function ScenarioCoverageCompare({ rows, highlight, label, meta, notes, legend = true, compact = false }: ScenarioCoverageCompareProps) {
  return (
    <figure className="space-y-2" aria-label={label} data-testid="scenario-coverage">
      <ul className="space-y-1">
        {ordered(rows).map((r) => {
          const s = scenarioInfo(meta, r.id);
          const shown = r.id === highlight;
          const note = notes && r.id in notes ? (notes[r.id] ?? null) : coverageNote(r);
          const text = r.split ? coverageText(r.split) : '';
          return (
            <li key={r.id} className={rowClass(shown)} data-scenario={r.id} aria-current={shown ? 'true' : undefined} title={s.words}>
              <ScenarioName id={r.id} meta={meta} shown={shown} />
              <div className={shown ? '' : 'opacity-60'}>
                {r.split ? (
                  <DemandSplitBar split={r.split} label={`${s.label}: ${text}${note ? ` (${note})` : ''}`} className={compact ? 'h-2' : 'h-3'} />
                ) : (
                  <div className={`w-full bg-ink-700/40 ${compact ? 'h-2' : 'h-3'}`} role="img" aria-label={`${s.label}: ${note ?? 'no figures'}`} />
                )}
              </div>
              <span className="text-xs tabular text-paper-dim">
                {text}
                {note ? <span className="block text-paper-mute">{note}</span> : null}
              </span>
            </li>
          );
        })}
      </ul>
      {legend && (
        <figcaption>
          <CoverageLegend />
        </figcaption>
      )}
    </figure>
  );
}

export interface ScenarioValue {
  id: AibScenarioId;
  /** the value (null: not available) */
  value: number | null;
  /** the printed value in place of format(value) */
  text?: string;
  /** a note under the value ("before any shock") */
  note?: string | null;
}

/** The values of figures for ScenarioBars: `pick` reads one field ((f) => f.lostUsd). */
export const scenarioValues = <F extends { id: AibScenarioId }>(figs: readonly F[], pick: (f: F) => number | null | undefined): ScenarioValue[] =>
  figs.map((f) => {
    const v = pick(f);
    return { id: f.id, value: isNum(v) ? v : null };
  });

export interface ScenarioBarsProps {
  values: readonly ScenarioValue[];
  highlight: AibScenarioId;
  /** what the bars measure ("Supply lost or shifted, USD a year") */
  label: string;
  format: (v: number) => string;
  meta?: AibMeta | null;
  /** the scale's end (default: the largest value) */
  max?: number;
}

/** Three bars of one scalar on a shared scale, each in its scenario's colour (Baseline hollow), the toggled one highlighted. */
export function ScenarioBars({ values, highlight, label, format, meta, max }: ScenarioBarsProps) {
  const rows = ordered(values);
  const top = max ?? Math.max(0, ...rows.map((r) => (isNum(r.value) ? Math.abs(r.value) : 0)));
  return (
    <figure aria-label={label} data-testid="scenario-bars">
      <ul className="space-y-1">
        {rows.map((r) => {
          const s = scenarioInfo(meta, r.id);
          const shown = r.id === highlight;
          const text = r.text ?? (isNum(r.value) ? format(r.value) : 'not available');
          const w = isNum(r.value) ? Math.abs(r.value) : 0;
          const style = s.mark === 'fill' ? { background: s.color } : { border: `2px solid ${s.color}`, boxSizing: 'border-box' as const };
          return (
            <li key={r.id} className={rowClass(shown)} data-scenario={r.id} aria-current={shown ? 'true' : undefined}>
              <ScenarioName id={r.id} meta={meta} shown={shown} />
              <div className={`relative h-3 w-full bg-ink-700/40 ${shown ? '' : 'opacity-60'}`} role="img" aria-label={`${s.label}: ${text}${r.note ? ` (${r.note})` : ''}`} title={`${s.label}: ${text}`}>
                {w > 0 && top > 0 && <span className="absolute inset-y-0 left-0 block" style={{ width: pct(w, top), ...style }} />}
              </div>
              <span className="text-xs tabular text-paper-dim">
                {text}
                {r.note ? <span className="block text-paper-mute">{r.note}</span> : null}
              </span>
            </li>
          );
        })}
      </ul>
    </figure>
  );
}

export interface ScenarioClassRow {
  id: AibScenarioId;
  /** the total split by class, USD (e.g. scenarioTotals().byClass mapped to {cls, usd: lostUsd}) */
  groups: readonly { cls: string; usd: number }[];
  note?: string | null;
}

export interface ScenarioClassBarsProps {
  rows: readonly ScenarioClassRow[];
  highlight: AibScenarioId;
  /** what the bars measure ("Where the lost supply sits, by class") */
  label: string;
  format: (v: number) => string;
  meta?: AibMeta | null;
}

const classStyle = (cls: string) => {
  const info = classInfo(cls);
  return info.mark === 'fill' ? { background: info.color } : info.mark === 'hollow' ? { border: `2px solid ${info.color}`, boxSizing: 'border-box' as const } : HATCH_STYLE;
};

/** Three stacked bars of a USD total by class on a shared scale (class colours and marks), the toggled scenario highlighted. */
export function ScenarioClassBars({ rows, highlight, label, format, meta }: ScenarioClassBarsProps) {
  const rs = ordered(rows);
  const totals = rs.map((r) => r.groups.reduce((s, g) => s + Math.max(0, g.usd), 0));
  const top = Math.max(0, ...totals);
  const classes = [...new Set(rs.flatMap((r) => r.groups.filter((g) => g.usd > 0).map((g) => g.cls)))];
  return (
    <figure className="space-y-2" aria-label={label} data-testid="scenario-class-bars">
      <ul className="space-y-1">
        {rs.map((r, i) => {
          const s = scenarioInfo(meta, r.id);
          const shown = r.id === highlight;
          const total = totals[i];
          const mix = classMixText(r.groups);
          const note = r.note ?? (r.id === 'baseline' && total <= 0 ? 'nothing is lost before any disruption' : total <= 0 ? 'nothing lost' : null);
          const text = total > 0 ? `${format(total)}${mix ? `: ${mix}` : ''}` : '';
          const parts = r.groups.filter((g) => g.usd > 0);
          return (
            <li key={r.id} className={rowClass(shown)} data-scenario={r.id} aria-current={shown ? 'true' : undefined}>
              <ScenarioName id={r.id} meta={meta} shown={shown} />
              <div className={`h-3 w-full bg-ink-700/40 ${shown ? '' : 'opacity-60'}`} role="img" aria-label={`${s.label}: ${text || note || 'nothing'}`}>
                {total > 0 && top > 0 && (
                  <div className="flex h-full gap-[2px]" style={{ width: pct(total, top) }}>
                    {parts.map((g) => (
                      <span key={g.cls} className="block h-full" style={{ width: pct(g.usd, total), ...classStyle(g.cls) }} title={`${classInfo(g.cls).label}: ${format(g.usd)}`} data-cls={g.cls} />
                    ))}
                  </div>
                )}
              </div>
              <span className="text-xs tabular text-paper-dim">
                {text}
                {note ? <span className="block text-paper-mute">{note}</span> : null}
              </span>
            </li>
          );
        })}
      </ul>
      {classes.length > 0 && (
        <figcaption>
          <ul className="flex flex-wrap gap-x-4 gap-y-1 text-xs text-paper-mute" aria-label="Classes">
            {classes.map((c) => (
              <li key={c} className="inline-flex items-center gap-1.5">
                <span aria-hidden="true" className="inline-block h-2.5 w-3.5" style={classStyle(c)} />
                {classInfo(c).label}
              </li>
            ))}
          </ul>
        </figcaption>
      )}
    </figure>
  );
}

/** A heading-sized line naming the toggled scenario, for a card that follows the toggle ("Managed Interdependence: ..."). */
export function ScenarioTag({ id, meta, children }: { id: AibScenarioId; meta?: AibMeta | null; children?: ReactNode }) {
  const s = scenarioInfo(meta, id);
  return (
    <span className="tag inline-flex items-center gap-1.5 whitespace-nowrap" title={s.words} data-scenario-tag={id}>
      <ScenarioSwatch id={id} />
      {s.label}
      {children}
    </span>
  );
}
