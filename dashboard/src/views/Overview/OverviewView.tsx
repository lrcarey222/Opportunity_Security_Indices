// View 1: Overview (spec_react.md 8.6). A world choropleth, the coalition heatmap and the
// distribution strip of one metric for one technology and stage, in the Show mode.

import { useCallback, useMemo, useState } from 'react';
import * as Plot from '@observablehq/plot';
import { useTitle } from '../../App.tsx';
import { ChartCard } from '../../charts/ChartCard.tsx';
import { Legend, RampLegend } from '../../charts/legends.tsx';
import { PlotFigure, Tip } from '../../charts/PlotFigure.tsx';
import { divColor, divScale, labelMargin, seqColor, seqScale } from '../../charts/theme.ts';
import { WorldMap, type MapCell } from '../../charts/WorldMap.tsx';
import { TableView, type Col } from '../../components/TableView.tsx';
import { Checkbox, EmptyState, FilterRow, Loading, Notice, PageHead, Select } from '../../components/ui.tsx';
import { rowsToCsv } from '../../lib/csv.ts';
import { cellLabel, DASH, fmtNum, fmtRankChange, fmtSigned, isNum } from '../../lib/format.ts';
import { isoOf } from '../../state/appData.ts';
import { useApp, useCore, usePartner, useScenario } from '../../state/EngineProvider.tsx';
import { useGoto } from '../../state/nav.ts';
import { METRICS, type Metric, type ShowMode } from '../../state/url.ts';
import { COMPARE_COLOR, INK, SIGNAL, STAGE_COLOR } from '../../theme/palette.ts';
import { noteText, showWord, TechStageFilters, useTechStage } from '../common.tsx';
import { cellRows, colorDomain, metricRows, METRIC_LABEL, withShown, type MetricRow } from './derive.ts';

const fmtFor = (metric: Metric) => (v: number | null) => (metric === 'si_rank' ? (isNum(v) ? String(Math.round(v)) : DASH) : fmtNum(v, 3));
const fmtChange = (metric: Metric) => (v: number | null) => (metric === 'si_rank' ? fmtRankChange(v) : fmtSigned(v, 3));

function MetricTip({ r, metric, isBaseline, cell }: { r: MetricRow; metric: Metric; isBaseline: boolean; cell: boolean }) {
  const f = fmtFor(metric);
  return (
    <Tip
      value={f(r.shown)}
      title={r.Country}
      lines={[
        cell && r.tech ? cellLabel(r.tech, r.supply_chain) : null,
        !isBaseline ? `Scenario ${f(r.value)} · paper ${f(r.value_base)} · ${fmtChange(metric)(r.delta)}` : null,
        !r.shown_ok && r.reason ? r.reason : null,
      ]}
    />
  );
}

/** Deterministic jitter in [-0.3, 0.3] from a key. */
function jitter(key: string): number {
  let h = 2166136261;
  for (let i = 0; i < key.length; i++) h = Math.imul(h ^ key.charCodeAt(i), 16777619);
  return (((h >>> 0) % 1000) / 1000 - 0.5) * 0.6;
}

export default function OverviewView() {
  useTitle('Overview');
  const app = useApp();
  const { view, setView, show, isBaseline } = useScenario();
  const goto = useGoto();
  const { tech, stage } = useTechStage();
  const metric = view.metric;
  const core = useCore();
  const partner = usePartner(metric === 'psi');
  const [focus, setFocus] = useState<string>(app.defaultCountry);
  const mode: ShowMode = show;
  const isPsi = metric === 'psi';

  const all = useMemo(() => {
    if (!core.data) return null;
    if (isPsi && !partner.data) return null;
    return withShown(metricRows(metric, core.data, partner.data, (c) => isoOf(app, c)), mode);
  }, [core.data, partner.data, metric, mode, app, isPsi]);
  const cell = useMemo(() => (all ? cellRows(all, metric, tech, stage) : []), [all, metric, tech, stage]);
  const dom = useMemo(() => colorDomain(cell, metric, mode), [cell, metric, mode]);
  const color = useMemo(() => (dom.kind === 'div' ? divColor(dom.hi) : seqColor(dom.lo, dom.hi, dom.reverse)), [dom]);
  const shownRows = cell.filter((r) => r.shown !== null && (r.shown_ok || !view.rankable));
  const f = fmtFor(metric);

  const mapCells = useMemo(() => {
    const m = new Map<string, MapCell>();
    for (const r of cell) {
      const c = r.iso3c ? app.byIso.get(r.iso3c) : undefined;
      if (!c?.iso_n3) continue;
      const visible = r.shown !== null && (r.shown_ok || !view.rankable);
      m.set(c.iso_n3, {
        iso3c: c.iso3c,
        fill: visible ? color(r.shown as number) : 'hatch',
        tip: <MetricTip r={r} metric={metric} isBaseline={isBaseline} cell={!isPsi} />,
      });
    }
    return m;
  }, [cell, app, color, view.rankable, metric, isBaseline, isPsi]);

  const updating = !core.fresh || (isPsi && !partner.fresh);
  const title = isPsi ? METRIC_LABEL.psi : `${METRIC_LABEL[metric]} – ${tech}, ${stage}`;
  const sub = (
    <>
      {showWord(mode)}{' '}
      {metric === 'si_rank' &&
        "The Strategic Index ranks a country's own sectors (1 = its top sector); levels do not compare across countries, so the map shows each economy's rank for this sector."}
      {metric === 'si' && 'SI levels do not compare across countries; prefer the rank within country.'}
      {isPsi && `One value per partner, averaged over the ${partner.data?.psi.view === 'world' ? 'world (all reporters)' : 'coalition reporters'}.`}
      {mode === 'change' && metric === 'si_rank' && ' Positive = the sector moved up the economy’s ranking.'}
    </>
  );

  const tableCols: Col<MetricRow>[] = [
    { id: 'Country', header: 'Economy', value: (r) => r.Country, minWidth: 150 },
    { id: 'value', header: 'Scenario', value: (r) => r.value, cell: (r) => f(r.value), num: true },
    { id: 'value_base', header: 'Paper', value: (r) => r.value_base, cell: (r) => f(r.value_base), num: true },
    { id: 'delta', header: 'Change', value: (r) => r.delta, cell: (r) => fmtChange(metric)(r.delta), num: true },
    { id: 'ok', header: 'Rankable', value: (r) => (r.ok ? 'yes' : 'no') },
    { id: 'reason', header: 'Reason', value: (r) => (r.ok ? null : r.reason), minWidth: 180 },
  ];
  const csvOf = (rows: MetricRow[], name: string) => () => ({
    name,
    text: rowsToCsv(rows.map((r) => ({ iso3c: r.iso3c, Country: r.Country, tech: r.tech || null, supply_chain: r.supply_chain || null, value: r.value, value_base: r.value_base, delta: r.delta, rankable: r.ok, reason: r.reason }))),
  });

  // ---- heatmap ----------------------------------------------------------------------------------
  const heat = useMemo(() => {
    if (!all || isPsi) return null;
    const rows = app.coalition;
    const label = (c: (typeof rows)[number]) => `${c.country}${c.tier === 'partner' ? ' · partner' : ''}`;
    const byKey = new Map(all.map((r) => [`${r.iso3c}|${r.tech}|${r.supply_chain}`, r]));
    const data: (MetricRow & { label: string; stageName: string; missing: boolean })[] = [];
    for (const c of rows) {
      for (const cl of app.cells) {
        const r = byKey.get(`${c.iso3c}|${cl.tech}|${cl.supply_chain}`);
        const missing = !r || r.shown === null || (view.rankable && !r.shown_ok);
        data.push({
          ...(r ?? {
            iso3c: c.iso3c, Country: c.country, tech: cl.tech, supply_chain: cl.supply_chain, value: null, value_base: null, delta: null,
            ok: false, ok_base: false, reason: 'no data', shown: null, shown_ok: false,
          }),
          Country: c.country,
          label: label(c),
          stageName: cl.supply_chain,
          missing,
        });
      }
    }
    const hd = colorDomain(data.filter((d) => !d.missing), metric, mode);
    return { data, labels: rows.map(label), dom: hd };
  }, [all, isPsi, app, view.rankable, metric, mode]);

  const heatHeight = heat ? 110 + 14 * (heat.labels.length + 1) : 200;
  const buildHeat = useCallback(
    (width: number): Plot.PlotOptions | null => {
      if (!heat) return null;
      const STRIPE = ' stage';
      const stripe = app.cells.map((c) => ({ tech: c.tech, stageName: c.supply_chain, label: STRIPE }));
      const hd = heat.dom;
      return {
        width,
        height: heatHeight,
        marginLeft: labelMargin(heat.labels, { min: 120, max: 280 }),
        marginTop: 28,
        marginBottom: 100,
        marginRight: 8,
        padding: 0,
        fx: { domain: app.stages, label: null, padding: 0.03, axis: 'top', tickSize: 0 },
        x: { domain: app.techs, label: null, tickRotate: -50, tickSize: 0, axis: 'bottom' },
        y: { domain: [STRIPE, ...heat.labels], label: null, tickSize: 0, tickFormat: (d: string) => (d === STRIPE ? '' : d) },
        color: hd.kind === 'div' ? divScale(hd.hi) : seqScale([hd.lo, hd.hi], hd.reverse),
        marks: [
          () => {
            const ns = 'http://www.w3.org/2000/svg';
            const defs = document.createElementNS(ns, 'defs');
            defs.innerHTML =
              '<pattern id="ov-hatch" width="5" height="5" patternUnits="userSpaceOnUse" patternTransform="rotate(45)"><rect width="5" height="5" fill="#374151"/><line x1="0" y1="0" x2="0" y2="5" stroke="rgba(255,255,255,0.28)" stroke-width="1"/></pattern>';
            return defs as unknown as SVGElement;
          },
          Plot.cell(stripe, { fx: 'stageName', x: 'tech', y: 'label', fill: (d: { stageName: string }) => STAGE_COLOR[d.stageName], inset: 0.5 }),
          Plot.cell(heat.data.filter((d) => d.missing), { fx: 'stageName', x: 'tech', y: 'label', fill: 'url(#ov-hatch)', inset: 0.5 }),
          Plot.cell(heat.data.filter((d) => !d.missing), { fx: 'stageName', x: 'tech', y: 'label', fill: 'shown', inset: 0.5 }),
          Plot.cell(heat.data, Plot.pointer({ fx: 'stageName', x: 'tech', y: 'label', fill: 'none', stroke: SIGNAL, strokeWidth: 2, maxRadius: 12 })),
        ],
      };
    },
    [heat, heatHeight, app],
  );

  // ---- distribution strip -----------------------------------------------------------------------
  const strip = useMemo(() => {
    const pts = cell.filter((r) => r.shown_ok && r.shown !== null).map((r) => ({ ...r, y: jitter(r.iso3c ?? r.Country) }));
    const ghosts =
      mode === 'scenario' && !isBaseline
        ? cell
            .filter((r) => r.ok_base && r.value_base !== null && r.value !== null && Math.abs((r.value as number) - (r.value_base as number)) > 1e-12)
            .map((r) => ({ ...r, y: jitter(r.iso3c ?? r.Country) }))
        : [];
    return { pts, ghosts };
  }, [cell, mode, isBaseline]);
  const buildStrip = useCallback(
    (width: number): Plot.PlotOptions | null => {
      if (!strip.pts.length) return null;
      const sel = strip.pts.filter((p) => p.iso3c === focus);
      const reverse = metric === 'si_rank' && mode !== 'change';
      return {
        width,
        height: 200,
        marginLeft: 16,
        marginRight: 24,
        x: { label: mode === 'change' ? 'Change against the paper' : metric === 'si_rank' ? 'Rank within country (1 = top)' : 'Value', reverse, grid: true, nice: true },
        y: { axis: null, domain: [-0.5, 0.5] },
        marks: [
          mode === 'change' ? Plot.ruleX([0], { stroke: 'rgb(255 255 255 / 0.22)' }) : null,
          Plot.dot(strip.ghosts, { x: 'value_base', y: 'y', r: 4.5, stroke: COMPARE_COLOR.baseline, strokeWidth: 1.5, fill: 'none' }),
          Plot.dot(strip.pts, { x: 'shown', y: 'y', r: 4.5, fill: COMPARE_COLOR.scenario, stroke: INK[800], strokeWidth: 1.5 }),
          Plot.dot(sel, { x: 'shown', y: 'y', r: 6, fill: COMPARE_COLOR.scenario, stroke: SIGNAL, strokeWidth: 2.5 }),
          Plot.text(sel, { x: 'shown', y: 'y', text: 'Country', dy: -16, fill: '#ffffff', fontSize: 12 }),
          Plot.dot(strip.pts, Plot.pointer({ x: 'shown', y: 'y', r: 8, fill: 'none', stroke: SIGNAL, strokeWidth: 2, maxRadius: 24 })),
        ],
      };
    },
    [strip, focus, metric, mode],
  );

  const metricOptions = METRICS.map((m) => ({ value: m, label: METRIC_LABEL[m] }));
  const nShown = new Set(shownRows.map((r) => r.iso3c)).size;
  const filtersChanged = metric !== 'es' || view.tech !== null || view.stage !== null || !view.rankable;
  const notes = core.data?.notes ?? [];

  return (
    <div>
      <PageHead
        eyebrow="Allied Industrial Base Explorer"
        title="Overview"
        lede="Security, opportunity and strategic focus for every economy, technology and supply-chain stage. Move the dials to build a scenario; use Show to compare it with the paper."
      />
      <FilterRow
        result={
          <span>
            Showing {nShown} economies
            {filtersChanged && (
              <>
                {' · '}
                <button type="button" className="link" onClick={() => setView({ metric: 'es', tech: null, stage: null, rankable: true })}>
                  Clear filters
                </button>
              </>
            )}
          </span>
        }
      >
        <Select label="Metric" value={metric} options={metricOptions} onChange={(v) => setView({ metric: v as Metric })} className="min-w-[16rem]" />
        <TechStageFilters techDisabled={isPsi} stageDisabled={isPsi} title={isPsi ? 'The PSI has one value per partner' : undefined} />
        <Checkbox label="Rankable only" checked={view.rankable} onChange={(v) => setView({ rankable: v })} />
      </FilterRow>
      {notes.length > 0 && (
        <div className="mb-4">
          <Notice kind="info">{notes.map(noteText).join(' ')}</Notice>
        </div>
      )}
      {!all ? (
        <Loading label={isPsi ? 'Loading partnership data' : 'Computing'} />
      ) : (
        <div className="flex flex-col gap-6">
          <ChartCard
            title={title}
            sub={sub}
            updating={updating}
            summary={`World map of ${METRIC_LABEL[metric]}${isPsi ? '' : ` for ${tech}, ${stage}`}; ${nShown} economies shown. Click an economy to open its profile.`}
            table={() => (
              <TableView rows={cell} cols={tableCols} caption={title} search initialSort={{ id: 'value', desc: metric !== 'si_rank' }} />
            )}
            csv={csvOf(cell, `overview-${metric}${isPsi ? '' : `-${tech}-${stage}`}`)}
            legend={
              <div className="flex flex-wrap items-end gap-x-8 gap-y-3">
                <RampLegend
                  kind={dom.kind}
                  min={dom.kind === 'div' ? dom.lo : dom.reverse ? dom.hi : dom.lo}
                  max={dom.kind === 'div' ? dom.hi : dom.reverse ? dom.lo : dom.hi}
                  reverse={false}
                  label={mode === 'change' ? (metric === 'si_rank' ? 'Rank change (up = blue)' : 'Change (increase = blue)') : metric === 'si_rank' ? 'Rank (1 = brightest)' : 'Value'}
                  format={(v) => (metric === 'si_rank' && mode !== 'change' ? String(Math.round(v)) : fmtNum(v, 2))}
                />
                <Legend
                  items={[
                    { label: 'Not rankable, excluded or missing (hover for the reason)', kind: 'hatch' },
                    { label: 'No data', color: INK[700] },
                  ]}
                />
              </div>
            }
          >
            <WorldMap
              cells={mapCells}
              ariaLabel={`${title}. ${showWord(mode)}`}
              onSelect={(iso) => goto(`/country/${iso}`)}
              selected={focus}
            />
          </ChartCard>

          {isPsi ? (
            <ChartCard title="Coalition heatmap" sub="The PSI has one value per partner; choose a technology-and-stage metric for the heatmap.">
              <EmptyState>Pick ES, EO, the Strategic Index or the Policy index to see the coalition heatmap.</EmptyState>
            </ChartCard>
          ) : (
            <ChartCard
              title="Coalition heatmap"
              sub="Coalition members (rows: core, then partners) by technology and stage (columns, grouped by stage under a stage-coloured stripe). Click a row to open the country profile."
              updating={updating}
              summary={`Heatmap of ${METRIC_LABEL[metric]} for ${heat?.labels.length ?? 0} coalition members across ${app.cells.length} technology-stage cells.`}
              table={() => (
                <TableView
                  rows={(heat?.data ?? []).filter((d) => !d.missing || d.value !== null)}
                  cols={[
                    { id: 'Country', header: 'Economy', value: (r) => r.Country, minWidth: 150 },
                    { id: 'tech', header: 'Technology', value: (r) => r.tech },
                    { id: 'supply_chain', header: 'Stage', value: (r) => r.supply_chain },
                    ...tableCols.slice(1),
                  ]}
                  caption="Coalition heatmap"
                  search
                />
              )}
              csv={csvOf(heat?.data ?? [], `coalition-heatmap-${metric}`)}
              legend={
                <div className="flex flex-wrap items-end gap-x-8 gap-y-3">
                  {heat && (
                    <RampLegend
                      kind={heat.dom.kind}
                      min={heat.dom.kind === 'div' ? heat.dom.lo : heat.dom.reverse ? heat.dom.hi : heat.dom.lo}
                      max={heat.dom.kind === 'div' ? heat.dom.hi : heat.dom.reverse ? heat.dom.lo : heat.dom.hi}
                      label={mode === 'change' ? (metric === 'si_rank' ? 'Rank change (up = blue)' : 'Change (increase = blue)') : metric === 'si_rank' ? 'Rank (1 = brightest)' : 'Value'}
                      format={(v) => (metric === 'si_rank' && mode !== 'change' ? String(Math.round(v)) : fmtNum(v, 2))}
                    />
                  )}
                  <Legend
                    items={[
                      { label: 'Upstream', color: STAGE_COLOR.Upstream },
                      { label: 'Midstream', color: STAGE_COLOR.Midstream },
                      { label: 'Downstream', color: STAGE_COLOR.Downstream },
                      { label: 'Missing, excluded or not rankable', kind: 'hatch' },
                    ]}
                    label="Stage stripe and missing cells"
                  />
                </div>
              }
            >
              <div className="overflow-x-auto" tabIndex={0} role="region" aria-label={`Coalition heatmap of ${METRIC_LABEL[metric]} (scrolls sideways on narrow screens)`}>
                <PlotFigure<MetricRow & { label: string }>
                  build={buildHeat}
                  height={heatHeight}
                  minWidth={760}
                  ariaLabel={`Coalition heatmap of ${METRIC_LABEL[metric]}`}
                  tooltip={(d) => <MetricTip r={d} metric={metric} isBaseline={isBaseline} cell />}
                  onSelect={(d) => d.iso3c && goto(`/country/${d.iso3c}`)}
                />
              </div>
            </ChartCard>
          )}

          <ChartCard
            title="Distribution across economies"
            sub={`${METRIC_LABEL[metric]}${isPsi ? '' : ` for ${cellLabel(tech, stage)}`} across rankable economies. Click a dot to highlight it.${metric === 'si_rank' ? ' Rank 1 = the economy’s own top sector.' : ''}`}
            updating={updating}
            summary={`Dot strip of ${strip.pts.length} economies.`}
            legend={
              <Legend
                items={[
                  { label: mode === 'change' ? 'Change' : mode === 'baseline' ? 'Paper baseline' : 'Scenario', kind: 'dot', color: COMPARE_COLOR.scenario },
                  ...(strip.ghosts.length ? [{ label: 'Paper baseline (where different)', kind: 'hollow' as const, color: COMPARE_COLOR.baseline }] : []),
                  { label: `Highlighted: ${app.byIso.get(focus)?.country ?? focus}`, kind: 'outline' as const, color: SIGNAL },
                ]}
              />
            }
          >
            {strip.pts.length ? (
              <PlotFigure<MetricRow>
                build={buildStrip}
                height={200}
                ariaLabel="Distribution strip"
                tooltip={(d) => <MetricTip r={d} metric={metric} isBaseline={isBaseline} cell={!isPsi} />}
                onSelect={(d) => d.iso3c && setFocus(d.iso3c)}
              />
            ) : (
              <EmptyState>No rankable economies for this cell.</EmptyState>
            )}
          </ChartCard>
        </div>
      )}
    </div>
  );
}
