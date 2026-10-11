// View 3: the opportunity-security space (spec_react.md 8.6; paper section 7). Every economy's ES
// (x) and EO (y) in one technology and stage, coloured by coalition tier, with the medians, the
// quadrant labels and, in Change mode, arrows from the paper's position to the scenario's; the
// selected economy's own cells faceted by stage; and the quadrant counts.

import { useCallback, useMemo, useState } from 'react';
import * as Plot from '@observablehq/plot';
import { useTitle } from '../../App.tsx';
import { ChartCard } from '../../charts/ChartCard.tsx';
import { Legend } from '../../charts/legends.tsx';
import { PlotFigure, Tip } from '../../charts/PlotFigure.tsx';
import { declutter, textWidth } from '../../charts/theme.ts';
import { CountrySelect } from '../../components/CountrySelect.tsx';
import { TableView } from '../../components/TableView.tsx';
import { Checkbox, EmptyState, FilterRow, Loading, Notice, PageHead } from '../../components/ui.tsx';
import { rowsToCsv } from '../../lib/csv.ts';
import { cellLabel, fmtNum, tierLabel } from '../../lib/format.ts';
import { useAdvancedChanged, useApp, useCore, useScenario } from '../../state/EngineProvider.tsx';
import type { ShowMode } from '../../state/url.ts';
import { COMPARE_COLOR, INK, PAPER_MUTE, SIGNAL, STAGE_COLOR, TIER_COLOR, tierKey } from '../../theme/palette.ts';
import { numOr, rowsOf, showWord, TechStageFilters, useCountryOptions, useTechStage } from '../common.tsx';
import { fitDomain, median, regionOf, REGIONS } from './derive.ts';

interface SpacePoint {
  iso3c: string;
  Country: string;
  tier: 'core' | 'partner' | 'other';
  tech: string;
  stage: string;
  es: number | null;
  eo: number | null;
  es_base: number | null;
  eo_base: number | null;
  ok: boolean;
  ok_base: boolean;
  x: number | null;
  y: number | null;
}

export function spacePoints(esRows: ReturnType<typeof rowsOf>, eoRows: ReturnType<typeof rowsOf>, tierOf: (iso: string) => string, mode: ShowMode): SpacePoint[] {
  const eoBy = new Map(eoRows.map((r) => [`${r.iso3c}|${r.tech}|${r.supply_chain}`, r]));
  const out: SpacePoint[] = [];
  for (const e of esRows) {
    const o = eoBy.get(`${e.iso3c}|${e.tech}|${e.supply_chain}`);
    if (!o || !e.iso3c) continue;
    const p: SpacePoint = {
      iso3c: String(e.iso3c),
      Country: String(e.Country),
      tier: tierKey(tierOf(String(e.iso3c))),
      tech: String(e.tech),
      stage: String(e.supply_chain),
      es: numOr(e.value),
      eo: numOr(o.value),
      es_base: numOr(e.value_base),
      eo_base: numOr(o.value_base),
      ok: e.rankable === true && o.rankable === true,
      ok_base: e.rankable_base === true && o.rankable_base === true,
      x: null,
      y: null,
    };
    p.x = mode === 'baseline' ? p.es_base : p.es;
    p.y = mode === 'baseline' ? p.eo_base : p.eo;
    out.push(p);
  }
  return out;
}

export default function SpaceView() {
  useTitle('Opportunity-security space');
  const app = useApp();
  const { show, isBaseline } = useScenario();
  const { view, setView } = useScenario();
  const { tech, stage } = useTechStage();
  const core = useCore();
  const ch = useAdvancedChanged();
  const [sel, setSel] = useState(app.defaultCountry);
  const options = useCountryOptions((x) => app.scored.some((s) => s.iso3c === x.iso3c));
  const tierOf = useCallback((iso: string) => app.byIso.get(iso)?.tier ?? 'none', [app]);

  const all = useMemo(
    () => (core.data ? spacePoints(rowsOf(core.data.compare.es), rowsOf(core.data.compare.eo), tierOf, show) : []),
    [core.data, tierOf, show],
  );
  const pts = useMemo(
    () => all.filter((p) => p.tech === tech && p.stage === stage && p.x !== null && p.y !== null && (!view.rankable || (show === 'baseline' ? p.ok_base : p.ok))),
    [all, tech, stage, view.rankable, show],
  );
  const med = useMemo(() => ({ es: median(pts.map((p) => p.x as number)), eo: median(pts.map((p) => p.y as number)) }), [pts]);
  const moved = pts.filter((p) => p.es_base !== null && p.eo_base !== null && (Math.abs((p.es as number) - p.es_base) > 1e-12 || Math.abs((p.eo as number) - p.eo_base) > 1e-12));
  const own = useMemo(() => all.filter((p) => p.iso3c === sel && p.x !== null && p.y !== null), [all, sel]);

  const counts = useMemo(() => {
    const cell = all.filter((p) => p.tech === tech && p.stage === stage);
    const scen = cell.filter((p) => p.es !== null && p.eo !== null && (!view.rankable || p.ok));
    const base = cell.filter((p) => p.es_base !== null && p.eo_base !== null && (!view.rankable || p.ok_base));
    const mS = { es: median(scen.map((p) => p.es as number)), eo: median(scen.map((p) => p.eo as number)) };
    const mB = { es: median(base.map((p) => p.es_base as number)), eo: median(base.map((p) => p.eo_base as number)) };
    return REGIONS.map((r) => ({
      region: r,
      scenario: scen.filter((p) => regionOf(p.es, p.eo, mS.es, mS.eo) === r).length,
      paper: base.filter((p) => regionOf(p.es_base, p.eo_base, mB.es, mB.eo) === r).length,
      coalition: scen.filter((p) => p.tier !== 'other' && regionOf(p.es, p.eo, mS.es, mS.eo) === r).length,
    }));
  }, [all, tech, stage, view.rankable]);

  const buildScatter = useCallback(
    (width: number): Plot.PlotOptions | null => {
      if (pts.length < 2) return null;
      const arrows = show === 'change' ? moved : [];
      const selP = pts.filter((p) => p.iso3c === sel);
      // fitted to the economies shown (and the paper's positions when arrows are drawn)
      const xd = fitDomain([...pts.map((p) => p.x), ...arrows.map((p) => p.es_base)]);
      const yd = fitDomain([...pts.map((p) => p.y), ...arrows.map((p) => p.eo_base)]);
      return {
        width,
        height: Math.min(560, Math.max(380, width * 0.62)),
        marginLeft: 52,
        marginTop: 24,
        marginBottom: 44,
        x: { domain: xd, label: 'Energy Security (ES) → more secure', grid: true },
        y: { domain: yd, label: '↑ Economic Opportunity (EO)', grid: true },
        color: { domain: ['core', 'partner', 'other'], range: [TIER_COLOR.core, TIER_COLOR.partner, TIER_COLOR.other] },
        marks: [
          Plot.ruleX([med.es], { stroke: 'rgb(255 255 255 / 0.35)', strokeDasharray: '4,3' }),
          Plot.ruleY([med.eo], { stroke: 'rgb(255 255 255 / 0.35)', strokeDasharray: '4,3' }),
          Plot.text([{ x: med.es, y: (yd[0] + yd[1]) / 2, t: `median ES ${fmtNum(med.es, 2)}` }], { x: 'x', y: 'y', text: 't', rotate: -90, dx: -8, fill: PAPER_MUTE, fontSize: 11 }),
          Plot.text([{ x: xd[1], y: med.eo, t: `median EO ${fmtNum(med.eo, 2)}` }], { x: 'x', y: 'y', text: 't', textAnchor: 'end', dy: -6, fill: PAPER_MUTE, fontSize: 11 }),
          ...(
            [
              { x: xd[0], y: yd[1], t: 'High opportunity, low security', a: 'start', dy: 10 },
              { x: xd[1], y: yd[1], t: 'High opportunity, high security', a: 'end', dy: 10 },
              { x: xd[0], y: yd[0], t: 'Low opportunity, low security', a: 'start', dy: -6 },
              { x: xd[1], y: yd[0], t: 'Low opportunity, high security', a: 'end', dy: -6 },
            ] as const
          ).map((q) => Plot.text([q], { x: 'x', y: 'y', text: 't', textAnchor: q.a, dy: q.dy, dx: q.a === 'start' ? 4 : -4, fill: '#d1d5db', fontSize: 11, fontWeight: 500 })),
          Plot.arrow(arrows, { x1: 'es_base', y1: 'eo_base', x2: 'es', y2: 'eo', stroke: COMPARE_COLOR.baseline, strokeWidth: 1, headLength: 6 }),
          Plot.dot(pts, { x: 'x', y: 'y', fill: 'tier', r: 4.5, stroke: INK[800], strokeWidth: 1.5 }),
          Plot.dot(selP, { x: 'x', y: 'y', fill: 'tier', r: 6, stroke: SIGNAL, strokeWidth: 2.5 }),
          Plot.text(selP, { x: 'x', y: 'y', text: 'Country', dy: -14, fill: '#ffffff', fontSize: 12 }),
          Plot.dot(pts, Plot.pointer({ x: 'x', y: 'y', r: 8, fill: 'none', stroke: SIGNAL, strokeWidth: 2, maxRadius: 30 })),
        ],
      };
    },
    [pts, moved, med, sel, show],
  );

  const buildOwn = useCallback(
    (width: number): Plot.PlotOptions | null => {
      if (!own.length) return null;
      const H = 320;
      const M = { left: 44, right: 16, top: 28, bottom: 36 };
      const xd = fitDomain(own.map((p) => p.x));
      const yd = fitDomain(own.map((p) => p.y));
      // label only where a label fits without overlapping another (outliers first); hover and the
      // table carry every value
      const n = app.stages.length;
      const pad = 0.08;
      const step = (width - M.left - M.right) / (n - pad + 2 * pad);
      const bw = step * (1 - pad);
      const sx = (v: number) => ((v - xd[0]) / (xd[1] - xd[0])) * bw;
      const sy = (v: number) => M.top + (1 - (v - yd[0]) / (yd[1] - yd[0])) * (H - M.top - M.bottom);
      const labelled: SpacePoint[] = [];
      for (const st of app.stages) {
        const ps = own.filter((p) => p.stage === st);
        const cx = ps.reduce((a, p) => a + sx(p.x as number), 0) / (ps.length || 1);
        const cy = ps.reduce((a, p) => a + sy(p.y as number), 0) / (ps.length || 1);
        const boxes = ps.map((p) => ({ x: sx(p.x as number), y: sy(p.y as number) - 10, w: textWidth(p.tech, 10) + 4, h: 12 }));
        const order = ps.map((_, i) => i).sort((a, b) => Math.hypot(boxes[b].x - cx, boxes[b].y - cy) - Math.hypot(boxes[a].x - cx, boxes[a].y - cy));
        const keep = declutter(boxes, order);
        ps.forEach((p, i) => {
          if (keep.has(i)) labelled.push(p);
        });
      }
      return {
        width,
        height: H,
        marginLeft: M.left,
        marginRight: M.right,
        marginTop: M.top,
        marginBottom: M.bottom,
        fx: { domain: app.stages, label: null, axis: 'top', padding: pad },
        x: { domain: xd, label: 'ES →', grid: true, ticks: 4 },
        y: { domain: yd, label: '↑ EO', grid: true, ticks: 4 },
        marks: [
          Plot.frame({ stroke: 'rgb(255 255 255 / 0.12)' }),
          Plot.dot(own, { fx: 'stage', x: 'x', y: 'y', r: 4.5, fill: (p: SpacePoint) => STAGE_COLOR[p.stage] ?? COMPARE_COLOR.scenario, stroke: INK[800], strokeWidth: 1.5 }),
          Plot.text(labelled, { fx: 'stage', x: 'x', y: 'y', text: 'tech', dy: -10, fill: '#d1d5db', fontSize: 10 }),
          Plot.dot(own, Plot.pointer({ fx: 'stage', x: 'x', y: 'y', r: 8, fill: 'none', stroke: SIGNAL, strokeWidth: 2, maxRadius: 24 })),
        ],
      };
    },
    [own, app.stages],
  );

  const tip = (p: SpacePoint) => (
    <Tip
      value={`ES ${fmtNum(p.x, 3)} · EO ${fmtNum(p.y, 3)}`}
      title={`${p.Country} (${tierLabel(p.tier)})`}
      lines={[
        cellLabel(p.tech, p.stage),
        regionOf(p.x, p.y, med.es, med.eo),
        !isBaseline ? `paper: ES ${fmtNum(p.es_base, 3)} · EO ${fmtNum(p.eo_base, 3)}` : null,
      ]}
    />
  );
  const updating = !core.fresh;
  const pillarsMove = ch.es || ch.eo || ch.sample;

  return (
    <div>
      <PageHead
        eyebrow="Opportunity and security"
        title="The opportunity–security space"
        lede="Each economy's Energy Security and Economic Opportunity in one technology and stage. The upper-left region, high opportunity with low security, is where building capacity both meets a need and creates value."
      />
      <FilterRow result={`Showing ${pts.length} economies`}>
        <TechStageFilters />
        <Checkbox label="Rankable only" checked={view.rankable} onChange={(v) => setView({ rankable: v })} />
      </FilterRow>
      {!core.data ? (
        <Loading label="Computing" />
      ) : (
        <div className="flex flex-col gap-6">
          {!isBaseline && !pillarsMove && (
            <Notice kind="info">The dials do not move the pillars (they are measurements, not preferences); only the advanced category weights and the rankability rule do, so this view matches the paper.</Notice>
          )}
          <div className="grid gap-6 2xl:grid-cols-[minmax(0,2fr)_minmax(0,1fr)]">
            <ChartCard
              title={`${cellLabel(tech, stage)}: Energy Security against Economic Opportunity`}
              sub={`${showWord(show)} Colour = coalition tier. Dashed lines are the medians of the economies shown.${show === 'change' ? " Arrows run from the paper's position to the scenario's." : ''} Click a point to select the economy.`}
              updating={updating}
              summary={`Scatter of ${pts.length} economies; median ES ${fmtNum(med.es, 2)}, median EO ${fmtNum(med.eo, 2)}.`}
              legend={
                <Legend
                  items={[
                    { label: 'Coalition core', kind: 'dot', color: TIER_COLOR.core },
                    { label: 'Coalition partner', kind: 'dot', color: TIER_COLOR.partner },
                    { label: 'Other economy', kind: 'dot', color: TIER_COLOR.other },
                    { label: `Selected: ${app.byIso.get(sel)?.country ?? sel}`, kind: 'outline', color: SIGNAL },
                    ...(show === 'change' && moved.length ? [{ label: 'Paper → scenario', kind: 'line' as const, color: COMPARE_COLOR.baseline }] : []),
                  ]}
                />
              }
              table={() => (
                <TableView
                  rows={pts}
                  caption="Opportunity-security space"
                  search
                  cols={[
                    { id: 'Country', header: 'Economy', value: (p) => p.Country, minWidth: 150 },
                    { id: 'tier', header: 'Tier', value: (p) => tierLabel(p.tier) },
                    { id: 'es', header: 'ES', value: (p) => p.es, cell: (p) => fmtNum(p.es, 3), num: true },
                    { id: 'es_base', header: 'ES paper', value: (p) => p.es_base, cell: (p) => fmtNum(p.es_base, 3), num: true },
                    { id: 'eo', header: 'EO', value: (p) => p.eo, cell: (p) => fmtNum(p.eo, 3), num: true },
                    { id: 'eo_base', header: 'EO paper', value: (p) => p.eo_base, cell: (p) => fmtNum(p.eo_base, 3), num: true },
                    { id: 'region', header: 'Region', value: (p) => regionOf(p.x, p.y, med.es, med.eo), minWidth: 200 },
                  ]}
                />
              )}
              csv={() => ({
                name: `space-${tech}-${stage}`,
                text: rowsToCsv(pts.map((p) => ({ iso3c: p.iso3c, Country: p.Country, tier: p.tier, tech: p.tech, supply_chain: p.stage, es: p.es, es_base: p.es_base, eo: p.eo, eo_base: p.eo_base, region: regionOf(p.x, p.y, med.es, med.eo) }))),
              })}
            >
              {pts.length > 1 ? (
                <PlotFigure<SpacePoint> build={buildScatter} height={420} ariaLabel="Opportunity-security scatter" tooltip={tip} onSelect={(p) => setSel(p.iso3c)} />
              ) : (
                <EmptyState>Too few economies with both pillars in this cell.</EmptyState>
              )}
            </ChartCard>
            <ChartCard title="Regions" sub="Economies in each region of this cell (split at the medians), scenario and paper." updating={updating}>
              <TableView
                rows={counts}
                caption="Region counts"
                pageSize={10}
                cols={[
                  { id: 'region', header: 'Region', value: (r) => r.region, minWidth: 170 },
                  { id: 'scenario', header: 'Scenario', value: (r) => r.scenario, num: true },
                  { id: 'paper', header: 'Paper', value: (r) => r.paper, num: true },
                  { id: 'coalition', header: 'Coalition', value: (r) => r.coalition, num: true, title: 'Coalition members, scenario' },
                ]}
              />
            </ChartCard>
          </div>
          <ChartCard
            title={`${app.byIso.get(sel)?.country ?? sel}: its own cells`}
            sub="The selected economy's cells in the same space, one panel per stage. Labels are drawn where they fit; hover a dot or open the table for every cell."
            tools={<CountrySelect label="Economy" value={sel} options={options} onChange={setSel} className="w-56" />}
            updating={updating}
            legend={
              <Legend
                items={app.stages.map((s) => ({ label: s, kind: 'dot' as const, color: STAGE_COLOR[s] }))}
                label="Stage (one per panel)"
              />
            }
            table={() => (
              <TableView
                rows={own}
                caption="Own cells"
                cols={[
                  { id: 'tech', header: 'Technology', value: (p) => p.tech },
                  { id: 'stage', header: 'Stage', value: (p) => p.stage },
                  { id: 'es', header: 'ES', value: (p) => p.x, cell: (p) => fmtNum(p.x, 3), num: true },
                  { id: 'eo', header: 'EO', value: (p) => p.y, cell: (p) => fmtNum(p.y, 3), num: true },
                  { id: 'ok', header: 'Rankable', value: (p) => (p.ok ? 'yes' : 'no') },
                ]}
              />
            )}
          >
            {own.length ? (
              <PlotFigure<SpacePoint> build={buildOwn} height={320} ariaLabel="The selected economy's cells by stage" tooltip={tip} />
            ) : (
              <EmptyState>No cells with both pillars for this economy.</EmptyState>
            )}
          </ChartCard>
        </div>
      )}
    </div>
  );
}

