// View 1: Overview (spec_react.md 8.6; spec_country.md C4.2). A world map, the coalition table and the
// dot chart of one measure for one technology and stage, in the Show mode. All three use the
// three-colour scale (C1): orange lower than most economies shown, white their median, blue higher;
// changes centre on zero; Strategic Index ranks are reversed so that rank 1 (an economy's top sector)
// is blue. The coalition table has no stage stripe: stage groups are named by text headers, so every
// coloured cell carries the measure.

import { useCallback, useMemo, useState } from 'react';
import * as Plot from '@observablehq/plot';
import { useTitle } from '../../App.tsx';
import { BeeswarmChart, type SwarmPoint } from '../../charts/Beeswarm.tsx';
import { ChartCard } from '../../charts/ChartCard.tsx';
import { Legend, TriLegend } from '../../charts/legends.tsx';
import { PlotFigure, Tip } from '../../charts/PlotFigure.tsx';
import { ringMarks } from '../../charts/rings.ts';
import { triScale, type TriScale } from '../../charts/scale.ts';
import { labelMargin } from '../../charts/theme.ts';
import { WorldMap, type MapCell } from '../../charts/WorldMap.tsx';
import { TableView, type Col } from '../../components/TableView.tsx';
import { Term } from '../../components/Term.tsx';
import { Checkbox, EmptyState, FilterRow, Loading, Notice, PageHead, Select } from '../../components/ui.tsx';
import { rowsToCsv } from '../../lib/csv.ts';
import { cellLabel, DASH, fmtNum, fmtRankChange, fmtSigned, isNum } from '../../lib/format.ts';
import { isoOf } from '../../state/appData.ts';
import { useApp, useCore, usePartner, useScenario } from '../../state/EngineProvider.tsx';
import { useGoto } from '../../state/nav.ts';
import { METRICS, type Metric, type ShowMode } from '../../state/url.ts';
import { COMPARE_COLOR, INK, PAPER_DIM, PAPER_MUTE } from '../../theme/palette.ts';
import { noteText, showWord, TechStageFilters, useTechStage } from '../common.tsx';
import { cellRows, METRIC_LABEL, METRIC_NOUN, METRIC_TERM, metricRows, scaleValues, sideWords, triOptions, withShown, type MetricRow } from './derive.ts';

const fmtFor = (metric: Metric) => (v: number | null) => (metric === 'si_rank' ? (isNum(v) ? String(Math.round(v)) : DASH) : fmtNum(v, 3));
const fmtChange = (metric: Metric) => (v: number | null) => (metric === 'si_rank' ? fmtRankChange(v) : fmtSigned(v, 3));
/** The format of a shown value (Show mode aware). */
const fmtShown = (metric: Metric, mode: ShowMode) => (mode === 'change' ? fmtChange(metric) : fmtFor(metric));
/** Legend tick labels: ranks as integers, changes signed, levels to two decimals. */
const fmtTick = (metric: Metric, mode: ShowMode) => (v: number) =>
  mode === 'change' ? (metric === 'si_rank' ? fmtSigned(Math.round(v), 0) : fmtSigned(v, 2)) : metric === 'si_rank' ? String(Math.round(v)) : fmtNum(v, 2);

function MetricTip({ r, metric, mode, isBaseline, cell, scale, noun }: { r: MetricRow; metric: Metric; mode: ShowMode; isBaseline: boolean; cell: boolean; scale: TriScale; noun: string }) {
  const f = fmtFor(metric);
  const visible = r.shown !== null && r.shown_ok;
  return (
    <Tip
      value={fmtShown(metric, mode)(r.shown)}
      title={r.Country}
      lines={[
        cell && r.tech ? cellLabel(r.tech, r.supply_chain) : null,
        visible ? sideWords(scale.side(r.shown), metric, mode, fmtTick(metric, mode)(scale.mid), noun) : null,
        !isBaseline ? `Scenario ${f(r.value)} · paper ${f(r.value_base)} · change ${fmtChange(metric)(r.delta)}` : null,
        !r.shown_ok && r.reason ? `Not ranked: ${r.reason}` : null,
      ]}
    />
  );
}

/** The words of a three-colour legend for a measure (ranks and changes name what the ends mean). */
function legendWords(metric: Metric, mode: ShowMode): { lowWord?: string; highWord?: string } {
  if (mode === 'change') return metric === 'si_rank' ? { lowWord: 'moved down its own list', highWord: 'moved up its own list' } : { lowWord: 'fell', highWord: 'rose' };
  if (metric === 'si_rank') return { lowWord: 'near the top of its own list', highWord: 'further down its own list' };
  return {};
}

interface SwarmP extends SwarmPoint {
  row: MetricRow;
  ghost: boolean;
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
  const visible = useCallback((r: MetricRow) => r.shown !== null && (r.shown_ok || !view.rankable), [view.rankable]);
  // the map's scale: the economies drawn for this measure and sector (after "Rankable only")
  const mapScale = useMemo(() => triScale(scaleValues(cell, view.rankable), triOptions(metric, mode)), [cell, view.rankable, metric, mode]);
  const shownRows = cell.filter(visible);
  const f = fmtFor(metric);
  const fTick = fmtTick(metric, mode);
  const words = legendWords(metric, mode);
  const label = METRIC_LABEL[metric];
  const noun = METRIC_NOUN[metric];
  const sectorName = cellLabel(tech, stage);

  const mapCells = useMemo(() => {
    const m = new Map<string, MapCell>();
    for (const r of cell) {
      const c = r.iso3c ? app.byIso.get(r.iso3c) : undefined;
      if (!c?.iso_n3) continue;
      m.set(c.iso_n3, {
        iso3c: c.iso3c,
        fill: visible(r) ? (mapScale.color(r.shown) ?? 'hatch') : 'hatch',
        tip: <MetricTip r={r} metric={metric} mode={mode} isBaseline={isBaseline} cell={!isPsi} scale={mapScale} noun="economies shown" />,
      });
    }
    return m;
  }, [cell, app, mapScale, visible, metric, mode, isBaseline, isPsi]);

  const updating = !core.fresh || (isPsi && !partner.fresh);
  const title = isPsi ? label : `${label}: ${sectorName}`;
  const legendLabel = mode === 'change' ? `Change in ${noun} against the paper${isPsi ? '' : `, ${sectorName}`}` : isPsi ? label : `${label}, ${sectorName}`;
  const termWords = metric === 'si_rank' || metric === 'si' ? 'Strategic Index' : isPsi ? 'Partnership Strength' : label;
  const term = <Term id={METRIC_TERM[metric]}>{termWords}</Term>;
  const mapSub = (
    <>
      {showWord(mode)}{' '}
      {mode === 'change' ? (
        metric === 'si_rank' ? (
          <>Each economy is coloured by how far this sector moved in its own {term} ranking against the paper: blue moved up, white did not move, orange moved down.</>
        ) : (
          <>Each economy is coloured by how much its {term}{isPsi ? '' : ` in ${sectorName}`} moved against the paper: orange fell, white no change, blue rose.</>
        )
      ) : metric === 'si_rank' ? (
        <>
          Each economy is coloured by where {sectorName} sits in its own list of sectors on the {term}, which ranks an economy&rsquo;s sectors by how strongly the
          data point to prioritising them: blue near the top (rank 1 is its top sector), white the middle of the economies shown, orange further down. Ranks compare
          sectors within an economy, not economies with each other.
        </>
      ) : metric === 'si' ? (
        <>
          Each economy is coloured by its {term} score for {sectorName}: orange lower than most economies shown, white the middle, blue higher. Scores are built to
          compare sectors within one economy; across economies prefer the rank.
        </>
      ) : isPsi ? (
        <>
          Each economy is coloured by its {term} as a partner: how strongly the {partner.data?.psi.reporters.length ?? 0}{' '}
          {partner.data?.psi.view === 'world' ? 'reporting economies' : 'core coalition members'}, on average, rate it as a supplier, a market and a development
          partner. Orange is weaker than most economies shown, white the middle, blue stronger.
        </>
      ) : (
        <>
          Each economy is coloured by its {term} in {sectorName}: orange lower than most economies shown, white the middle (the median), blue higher.{' '}
          {metric === 'policy'
            ? 'Policy activity measures how much policy a government has in the sector; it does not judge whether the policies work.'
            : 'Scores run from 0 to 1 and compare an economy with all others in the same sector.'}
        </>
      )}{' '}
      Hatched: not ranked (hover for why). Click an economy for its scores.
    </>
  );

  const tableCols: Col<MetricRow>[] = [
    { id: 'Country', header: 'Economy', value: (r) => r.Country, minWidth: 150 },
    { id: 'value', header: 'Scenario', value: (r) => r.value, cell: (r) => f(r.value), num: true },
    { id: 'value_base', header: 'Paper', value: (r) => r.value_base, cell: (r) => f(r.value_base), num: true },
    { id: 'delta', header: 'Change', value: (r) => r.delta, cell: (r) => fmtChange(metric)(r.delta), num: true },
    { id: 'ok', header: 'Ranked', value: (r) => (r.ok ? 'yes' : 'no') },
    { id: 'reason', header: 'Why not ranked', value: (r) => (r.ok ? null : r.reason), minWidth: 180 },
  ];
  const mapCols: Col<MetricRow>[] = [
    ...tableCols.slice(0, 4),
    { id: 'side', header: 'Colour', value: (r) => (visible(r) ? sideWords(mapScale.side(r.shown), metric, mode, fTick(mapScale.mid), 'economies shown') : 'hatched'), minWidth: 220 },
    ...tableCols.slice(4),
  ];
  const csvOf = (rows: MetricRow[], name: string) => () => ({
    name,
    text: rowsToCsv(rows.map((r) => ({ iso3c: r.iso3c, Country: r.Country, tech: r.tech || null, supply_chain: r.supply_chain || null, value: r.value, value_base: r.value_base, delta: r.delta, rankable: r.ok, reason: r.reason }))),
  });

  // ---- coalition table (heatmap) -----------------------------------------------------------------
  const heat = useMemo(() => {
    if (!all || isPsi) return null;
    const rows = app.coalition;
    const rowLabel = (c: (typeof rows)[number]) => `${c.country}${c.tier === 'partner' ? ' · partner' : ''}`;
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
          label: rowLabel(c),
          stageName: cl.supply_chain,
          missing,
        });
      }
    }
    // one scale pooled over every coloured cell
    const scale = triScale(data.filter((d) => !d.missing).map((d) => d.shown as number), triOptions(metric, mode));
    return { data, labels: rows.map(rowLabel), scale, nCells: data.filter((d) => !d.missing).length };
  }, [all, isPsi, app, view.rankable, metric, mode]);

  const heatHeight = heat ? 130 + 14 * heat.labels.length : 200;
  const buildHeat = useCallback(
    (width: number): Plot.PlotOptions | null => {
      if (!heat) return null;
      const s = heat.scale;
      const marginLeft = labelMargin(heat.labels, { min: 120, max: 280 });
      const marginRight = 8;
      // an 8 px gap between the stage groups
      const frame = Math.max(240, width - marginLeft - marginRight);
      const gap = Math.min(0.1, 24 / frame);
      const cellCh = { fx: 'stageName', x: 'tech', y: 'label', inset: 0.5 };
      const sel = heat.data.filter((d) => d.iso3c === focus && d.tech === tech && d.supply_chain === stage);
      return {
        width,
        height: heatHeight,
        marginLeft,
        marginTop: 28,
        marginBottom: 100,
        marginRight,
        fx: { domain: app.stages, label: null, paddingInner: gap, paddingOuter: 0 },
        x: { domain: app.techs, label: null, tickRotate: -50, tickSize: 0, axis: 'bottom', padding: 0 },
        y: { domain: heat.labels, label: null, tickSize: 0, padding: 0 },
        marks: [
          () => {
            const ns = 'http://www.w3.org/2000/svg';
            const defs = document.createElementNS(ns, 'defs');
            defs.innerHTML =
              '<pattern id="ov-hatch" width="5" height="5" patternUnits="userSpaceOnUse" patternTransform="rotate(45)"><rect width="5" height="5" fill="#374151"/><line x1="0" y1="0" x2="0" y2="5" stroke="rgba(255,255,255,0.28)" stroke-width="1"/></pattern>';
            return defs as unknown as SVGElement;
          },
          // the stage groups are named by text headers (no colour stripe: every coloured cell is the measure)
          Plot.axisFx({ anchor: 'top', label: null, tickSize: 0, tickFormat: (d: string) => d.toUpperCase(), fill: PAPER_DIM, fontSize: 11, fontWeight: 500 }),
          Plot.cell(heat.data.filter((d) => d.missing), { ...cellCh, fill: 'url(#ov-hatch)' }),
          Plot.cell(heat.data.filter((d) => !d.missing), { ...cellCh, fill: (d: MetricRow) => s.color(d.shown) ?? 'none' }),
          ...(sel.length ? ringMarks(sel, cellCh, 'selected', 'cell') : []),
          ...ringMarks(heat.data, { ...cellCh, maxRadius: 12 }, 'hover', 'cell'),
        ],
      };
    },
    [heat, heatHeight, app, focus, tech, stage],
  );

  // ---- where each economy sits (beeswarm) -------------------------------------------------------
  // ranks are drawn negated so that rank 1 (the economy's top sector, blue) sits at the right, like the
  // high end of every other measure; the axis prints the rank itself
  const negRank = metric === 'si_rank' && mode !== 'change';
  const swarm = useMemo(() => {
    const val = (v: number) => (negRank ? -v : v);
    const vis = cell.filter(visible);
    const pts: SwarmP[] = vis.map((r) => ({
      id: r.iso3c ?? r.Country,
      value: val(r.shown as number),
      label: r.Country,
      fill: mapScale.color(r.shown) ?? PAPER_MUTE,
      stroke: INK[900],
      strokeWidth: 1,
      row: r,
      ghost: false,
    }));
    // Scenario mode: the paper's positions where they differ, as hollow gray rings in the same layout
    const ghosts: SwarmP[] =
      mode === 'scenario' && !isBaseline
        ? vis
            .filter((r) => r.ok_base && r.value_base !== null && r.value !== null && Math.abs(r.value - r.value_base) > 1e-12)
            .map((r) => ({ id: `${r.iso3c ?? r.Country}~paper`, value: val(r.value_base as number), label: `${r.Country} (paper)`, fill: 'none', stroke: COMPARE_COLOR.baseline, strokeWidth: 1.5, row: r, ghost: true }))
        : [];
    return { pts: [...ghosts, ...pts], n: pts.length, nGhost: ghosts.length };
  }, [cell, visible, mapScale, mode, isBaseline, negRank]);
  const swarmDomain = useMemo((): [number, number] | undefined => (mode !== 'change' && (metric === 'es' || metric === 'eo' || metric === 'policy') ? [0, 1] : undefined), [metric, mode]);
  const swarmTicks = useCallback((v: number) => String(Math.round(-v)), []);
  const swarmX =
    mode === 'change'
      ? metric === 'si_rank'
        ? 'Change in rank against the paper (right = moved up its own list) →'
        : `Change in ${noun} against the paper →`
      : metric === 'si_rank'
        ? 'Rank of this sector in the economy’s own list (1 = its top sector) →'
        : `${label} →`;
  const focusName = app.byIso.get(focus)?.country ?? focus;

  const metricOptions = METRICS.map((m) => ({ value: m, label: METRIC_LABEL[m] }));
  const nShown = new Set(shownRows.map((r) => r.iso3c)).size;
  const filtersChanged = metric !== 'es' || view.tech !== null || view.stage !== null || !view.rankable;
  const notes = core.data?.notes ?? [];
  const nSectors = app.cells.length;
  const heatWords =
    mode === 'change'
      ? ' against the paper: orange fell, white no change, blue rose'
      : metric === 'si_rank'
        ? ': blue near the top of the member’s own list, white the middle of the cells shown, orange further down'
        : ': orange is lower than most cells shown, white the middle, blue higher';

  return (
    <div>
      <PageHead
        eyebrow="Allied Industrial Base Explorer"
        title="Overview"
        lede={
          <>
            This page scores every economy, for each energy technology and supply-chain stage, on two questions. <Term id="es" />: how well can it rely on what it
            needs in that sector, given its own production and reserves, how much it must import, how concentrated world supply is and how stable prices are?{' '}
            <Term id="eo" />: how well placed is it to create value there, through exports, investment, costs, demand and know-how? Both run from 0 to 1 and
            compare an economy with all others in the same sector, so 0.8 means doing better than most. Pick a measure, a technology and a stage: the map, the
            coalition table and the dot chart show the same numbers three ways. The priority dials re-weight everything; at 50 they
            reproduce the paper.
          </>
        }
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
        <Select label="Measure" value={metric} options={metricOptions} onChange={(v) => setView({ metric: v as Metric })} className="min-w-[16rem]" />
        <TechStageFilters techDisabled={isPsi} stageDisabled={isPsi} title={isPsi ? 'Partnership Strength has one value per economy, across all sectors' : undefined} />
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
            sub={mapSub}
            updating={updating}
            summary={`World map of ${label}${isPsi ? '' : ` for ${sectorName}`}; ${nShown} economies shown, coloured from orange (lower) through white (the median, ${fTick(mapScale.mid)}) to blue (higher). Click an economy to open its scores.`}
            table={() => <TableView rows={cell} cols={mapCols} caption={title} search initialSort={{ id: 'value', desc: metric !== 'si_rank' }} />}
            csv={csvOf(cell, `overview-${metric}${isPsi ? '' : `-${tech}-${stage}`}`)}
            legend={
              <div className="flex flex-wrap items-end gap-x-8 gap-y-3">
                <TriLegend scale={mapScale} label={legendLabel} format={fTick} noun="economies" {...words} missing="Hatched: not ranked, excluded or missing (hover for the reason)" />
                <Legend items={[{ label: 'No data', color: INK[700] }]} />
              </div>
            }
          >
            <WorldMap cells={mapCells} ariaLabel={`${title}. ${showWord(mode)}`} onSelect={(iso) => goto(`/country/${iso}`)} selected={focus} ringStyle="double" />
          </ChartCard>

          {isPsi ? (
            <ChartCard title={`Coalition members across all ${nSectors} sectors`} sub="Partnership Strength has one value per economy, across all sectors; pick another measure to see the coalition table.">
              <EmptyState>Pick Energy Security, Economic Opportunity, the Strategic Index or Policy activity to see the coalition table.</EmptyState>
            </ChartCard>
          ) : (
            <ChartCard
              title={`Coalition members across all ${nSectors} sectors`}
              sub={`Rows are the ${heat?.labels.length ?? 0} coalition members (core first, then partners); columns are the ${app.techs.length} technologies at each stage. Each cell is the member's ${noun} in that sector${heatWords}. Hatched: missing or not ranked. Click a row for that country's scores.`}
              updating={updating}
              summary={`Table of ${label} for ${heat?.labels.length ?? 0} coalition members across ${nSectors} sectors, coloured from orange (lower) through white (the median of the ${heat?.nCells ?? 0} cells shown) to blue (higher).`}
              table={() => (
                <TableView
                  rows={(heat?.data ?? []).filter((d) => !d.missing || d.value !== null)}
                  cols={[
                    { id: 'Country', header: 'Economy', value: (r) => r.Country, minWidth: 150 },
                    { id: 'tech', header: 'Technology', value: (r) => r.tech },
                    { id: 'supply_chain', header: 'Stage', value: (r) => r.supply_chain },
                    ...tableCols.slice(1, 4),
                    { id: 'side', header: 'Colour', value: (r) => (r.missing || !heat ? 'hatched' : sideWords(heat.scale.side(r.shown), metric, mode, fTick(heat.scale.mid), 'cells shown')), minWidth: 220 },
                    ...tableCols.slice(4),
                  ]}
                  caption="Coalition members across all sectors"
                  search
                />
              )}
              csv={csvOf(heat?.data ?? [], `coalition-table-${metric}`)}
              legend={
                heat && (
                  <TriLegend
                    scale={heat.scale}
                    label={mode === 'change' ? `Change in ${noun} against the paper, all sectors` : `${label}, all sectors`}
                    format={fTick}
                    noun="cells"
                    {...words}
                    missing="Hatched: missing, excluded or not ranked"
                  />
                )
              }
            >
              <div className="overflow-x-auto" tabIndex={0} role="region" aria-label={`Coalition table of ${label} (scrolls sideways on narrow screens)`}>
                <PlotFigure<MetricRow & { label: string }>
                  build={buildHeat}
                  height={heatHeight}
                  minWidth={760}
                  ariaLabel={`Coalition table of ${label}`}
                  tooltip={(d) => heat && <MetricTip r={d} metric={metric} mode={mode} isBaseline={isBaseline} cell scale={heat.scale} noun="cells shown" />}
                  onSelect={(d) => d.iso3c && goto(`/country/${d.iso3c}`)}
                />
              </div>
            </ChartCard>
          )}

          <ChartCard
            title="Where each economy sits"
            sub={`Each dot is one economy, placed by its ${noun}${isPsi ? '' : ` in ${sectorName}`} and coloured as on the map; dots with close values are stacked so that none hides another.${
              negRank ? ' Rank 1 is the economy’s own top sector.' : ''
            }${swarm.nGhost ? ' Hollow gray rings are the paper’s values where the scenario differs.' : ''} Click a dot to highlight that economy here, on the map and in the coalition table.`}
            updating={updating}
            summary={`Dot chart of ${swarm.n} economies by ${label}${isPsi ? '' : ` in ${sectorName}`}; ${focusName} is highlighted.`}
            legend={
              <Legend
                items={[
                  ...(swarm.nGhost ? [{ label: 'Paper baseline (where the scenario differs)', kind: 'hollow' as const, color: COMPARE_COLOR.baseline }] : []),
                  { label: `Highlighted: ${focusName}`, kind: 'outline' as const, color: '#ffffff' },
                ]}
              />
            }
            table={() => <TableView rows={shownRows} cols={mapCols} caption="Where each economy sits" search initialSort={{ id: 'value', desc: metric !== 'si_rank' }} />}
          >
            {swarm.n ? (
              <BeeswarmChart<SwarmP>
                points={swarm.pts}
                ariaLabel={`Dot chart of ${label}`}
                xLabel={swarmX}
                domain={swarmDomain}
                tickFormat={negRank ? swarmTicks : undefined}
                selected={focus}
                ringStyle="double"
                onSelect={(p) => p.row.iso3c && setFocus(p.row.iso3c)}
                tooltip={(p) => (
                  <MetricTip
                    r={p.ghost ? { ...p.row, shown: p.row.value_base } : p.row}
                    metric={metric}
                    mode={p.ghost ? 'baseline' : mode}
                    isBaseline={isBaseline}
                    cell={!isPsi}
                    scale={mapScale}
                    noun="economies shown"
                  />
                )}
              />
            ) : (
              <EmptyState>No economy has a value to place for this sector.</EmptyState>
            )}
          </ChartCard>
        </div>
      )}
    </div>
  );
}
