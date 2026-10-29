// View 3: the opportunity-security space (spec_react.md 8.6; paper section 7). Every economy's Economic
// Opportunity (x) and Energy Security (y; the user's orientation of 2026-10-15) in one technology and
// stage, coloured by coalition tier,
// with the medians, the four named corners (Upgrade, Grow, Exposed, Stable: the names Country Insights
// uses; Exposed and Stable were Secure and Watch until 2026-10-28) and, in Change mode, arrows from the
// NEIS baseline position to the scenario's; the selected economy's own sectors in one panel per stage
// (six stages since 2026-10-29: the panels wrap on a narrow screen, views/stageFacets.ts; the stage is
// named above its panel, never a colour series, design S4); and the corner counts. The corners are kept
// at six stages (the user's decision, 2026-10-28).

import { useCallback, useMemo, useState } from 'react';
import * as Plot from '@observablehq/plot';
import { ChartCard } from '../../charts/ChartCard.tsx';
import { Legend } from '../../charts/legends.tsx';
import { PlotFigure, Tip } from '../../charts/PlotFigure.tsx';
import { declutter, textWidth } from '../../charts/theme.ts';
import { CountrySelect } from '../../components/CountrySelect.tsx';
import { Term } from '../../components/Term.tsx';
import { TableView } from '../../components/TableView.tsx';
import { Checkbox, EmptyState, FilterRow, Loading, Notice, PageHead } from '../../components/ui.tsx';
import { rowsToCsv } from '../../lib/csv.ts';
import { cellLabel, fmtNum, tierLabel } from '../../lib/format.ts';
import { useAdvancedChanged, useApp, useCore, useScenario } from '../../state/EngineProvider.tsx';
import type { ShowMode } from '../../state/url.ts';
import { COMPARE_COLOR, INK, PAPER_MUTE, SIGNAL, TIER_COLOR, tierKey } from '../../theme/palette.ts';
import { stageLabel, stageRank } from '../../lib/stages.ts';
import { numOr, rowsOf, showWord, TechStageFilters, useCountryOptions, useTechStage, useViewTitle } from '../common.tsx';
import { panelAxisX, stageFacetLayout, stageLabels } from '../stageFacets.ts';
import { cornerAnnotation, fitDomain, median, regionOf, REGIONS } from './derive.ts';

/** The own-sectors chart: one panel's plotting height and the margins around the grid of panels (the
 * top margin holds the y axis label above the first row's stage names). */
const OWN = { panel: 180, left: 44, right: 16, top: 44, bottom: 40 } as const;

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
    // x = Economic Opportunity, y = Energy Security
    p.x = mode === 'baseline' ? p.eo_base : p.eo;
    p.y = mode === 'baseline' ? p.es_base : p.es;
    out.push(p);
  }
  return out;
}

/** `embedded` (IA refactor 2026-10-16): inside the Country profile ("Compare with other economies"), which owns the title and the head; `iso3` starts the economy picker there. */
export default function SpaceView({ embedded = false, iso3 }: { embedded?: boolean; iso3?: string } = {}) {
  useViewTitle('Opportunity-security space', embedded);
  const app = useApp();
  const { show, isBaseline } = useScenario();
  const { view, setView } = useScenario();
  const { tech, stage } = useTechStage();
  const core = useCore();
  const ch = useAdvancedChanged();
  const [sel, setSel] = useState(iso3 && app.byIso.has(iso3) ? iso3 : app.defaultCountry);
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
  const med = useMemo(() => ({ es: median(pts.map((p) => p.y as number)), eo: median(pts.map((p) => p.x as number)) }), [pts]);
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
      baseline: base.filter((p) => regionOf(p.es_base, p.eo_base, mB.es, mB.eo) === r).length,
      coalition: scen.filter((p) => p.tier !== 'other' && regionOf(p.es, p.eo, mS.es, mS.eo) === r).length,
    }));
  }, [all, tech, stage, view.rankable]);

  const buildScatter = useCallback(
    (width: number): Plot.PlotOptions | null => {
      if (pts.length < 2) return null;
      const arrows = show === 'change' ? moved : [];
      const selP = pts.filter((p) => p.iso3c === sel);
      // fitted to the economies shown (and the NEIS baseline positions when arrows are drawn)
      const xd = fitDomain([...pts.map((p) => p.x), ...arrows.map((p) => p.eo_base)]);
      const yd = fitDomain([...pts.map((p) => p.y), ...arrows.map((p) => p.es_base)]);
      return {
        width,
        height: Math.min(560, Math.max(380, width * 0.62)),
        marginLeft: 52,
        marginTop: 24,
        marginBottom: 44,
        x: { domain: xd, label: 'Economic Opportunity (right = bigger opening) →', grid: true },
        y: { domain: yd, label: '↑ Energy Security (up = more secure)', grid: true },
        color: { domain: ['core', 'partner', 'other'], range: [TIER_COLOR.core, TIER_COLOR.partner, TIER_COLOR.other] },
        marks: [
          Plot.ruleX([med.eo], { stroke: 'rgb(255 255 255 / 0.35)', strokeDasharray: '4,3' }),
          Plot.ruleY([med.es], { stroke: 'rgb(255 255 255 / 0.35)', strokeDasharray: '4,3' }),
          Plot.text([{ x: med.eo, y: (yd[0] + yd[1]) / 2, t: `median Economic Opportunity ${fmtNum(med.eo, 2)}` }], { x: 'x', y: 'y', text: 't', rotate: -90, dx: -8, fill: PAPER_MUTE, fontSize: 11 }),
          Plot.text([{ x: xd[1], y: med.es, t: `median Energy Security ${fmtNum(med.es, 2)}` }], { x: 'x', y: 'y', text: 't', textAnchor: 'end', dy: -6, fill: PAPER_MUTE, fontSize: 11 }),
          ...(
            [
              // the corner names come from derive.ts CORNERS, as the table's and the Country profile's do
              { x: xd[0], y: yd[1], t: cornerAnnotation(3, width < 560), a: 'start', dy: 10 },
              { x: xd[1], y: yd[1], t: cornerAnnotation(1, width < 560), a: 'end', dy: 10 },
              { x: xd[0], y: yd[0], t: cornerAnnotation(2, width < 560), a: 'start', dy: -6 },
              { x: xd[1], y: yd[0], t: cornerAnnotation(0, width < 560), a: 'end', dy: -6 },
            ] as const
          ).map((q) => Plot.text([q], { x: 'x', y: 'y', text: 't', textAnchor: q.a, dy: q.dy, dx: q.a === 'start' ? 4 : -4, fill: '#d1d5db', fontSize: 11, fontWeight: 500 })),
          Plot.arrow(arrows, { x1: 'eo_base', y1: 'es_base', x2: 'eo', y2: 'es', stroke: COMPARE_COLOR.baseline, strokeWidth: 1, headLength: 6 }),
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
      const xd = fitDomain(own.map((p) => p.x));
      const yd = fitDomain(own.map((p) => p.y));
      // the stage panels side by side where each gets 120 px, else three or two a row (a phone shows 2 x 3)
      const L = stageFacetLayout(app.stages, {
        width, marginLeft: OWN.left, marginRight: OWN.right, marginTop: OWN.top, marginBottom: OWN.bottom, panelHeight: OWN.panel, minPanel: 120, gapX: 16, gapY: 52,
      });
      const narrow = L.panelWidth < 150;
      // label only where a label fits without overlapping another (outliers first); hover and the
      // table carry every value
      const sx = (v: number) => ((v - xd[0]) / (xd[1] - xd[0])) * L.panelWidth;
      const sy = (v: number) => (1 - (v - yd[0]) / (yd[1] - yd[0])) * L.panelHeight;
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
      // one economy, one tier: every dot takes its tier colour (the stage is the panel, not a colour)
      const fill = TIER_COLOR[own[0].tier];
      return {
        width,
        height: L.height,
        marginLeft: OWN.left,
        marginRight: OWN.right,
        marginTop: OWN.top,
        marginBottom: OWN.bottom,
        fx: L.fx,
        fy: L.fy,
        x: { domain: xd },
        // the y axis on the left panel of each row; its label once, above the first row
        y: { domain: yd, label: '↑ Energy Security', grid: true, ticks: 4 },
        marks: [
          // repeated in every panel that holds a dot (Plot skips empty facets)
          Plot.frame({ stroke: 'rgb(255 255 255 / 0.12)' }),
          ...panelAxisX({ ticks: narrow ? 3 : 4, label: 'Economic Opportunity →' }),
          stageLabels(L.grid),
          Plot.dot(own, { ...L.facet, x: 'x', y: 'y', r: 4.5, fill, stroke: INK[800], strokeWidth: 1.5 }),
          Plot.text(labelled, { ...L.facet, x: 'x', y: 'y', text: 'tech', dy: -10, fill: '#d1d5db', fontSize: 10 }),
          Plot.dot(own, Plot.pointer({ ...L.facet, x: 'x', y: 'y', r: 8, fill: 'none', stroke: SIGNAL, strokeWidth: 2, maxRadius: 24 })),
        ],
      };
    },
    [own, app.stages],
  );

  const tip = (p: SpacePoint) => (
    <Tip
      value={`Economic Opportunity ${fmtNum(p.x, 3)} · Energy Security ${fmtNum(p.y, 3)}`}
      title={`${p.Country} (${tierLabel(p.tier)})`}
      lines={[
        cellLabel(p.tech, p.stage),
        regionOf(p.y, p.x, med.es, med.eo),
        !isBaseline ? `NEIS baseline: Economic Opportunity ${fmtNum(p.eo_base, 3)} · Energy Security ${fmtNum(p.es_base, 3)}` : null,
      ]}
    />
  );
  const updating = !core.fresh;
  const pillarsMove = ch.es || ch.eo || ch.sample;

  return (
    <div>
      {!embedded && (
      <PageHead
        eyebrow="Security vs opportunity"
        title="The opportunity–security space"
        lede={
          <>
            Every economy in one sector, placed by <Term id="eo" /> (across) and <Term id="es" /> (up). The dashed lines are the middle economy in this sector. The lower right,
            high opportunity with low security, is where building capacity would both meet a need and create value; the Country profile calls it{' '}
            <Term id="upgrade">Upgrade</Term>. The other corners are <Term id="grow">Grow</Term> (high opportunity, high security),{' '}
            <Term id="exposed">Exposed</Term> (low opportunity, low security) and <Term id="stable">Stable</Term> (low opportunity, high security). The corners place an
            economy; they are not its strategic posture. Pick a technology and one of the {app.stages.length} supply-chain stages.
          </>
        }
      />
      )}
      <FilterRow result={`Showing ${pts.length} economies`}>
        <TechStageFilters />
        <Checkbox label="Rankable only" checked={view.rankable} onChange={(v) => setView({ rankable: v })} />
      </FilterRow>
      {!core.data ? (
        <Loading label="Computing" />
      ) : (
        <div className="flex flex-col gap-6">
          {!isBaseline && !pillarsMove && (
            <Notice kind="info">The dials do not move Energy Security or Economic Opportunity (they are measurements, not preferences); only the advanced category weights and the rankability rule do, so this view matches the NEIS baseline.</Notice>
          )}
          <div className="grid gap-6 2xl:grid-cols-[minmax(0,2fr)_minmax(0,1fr)]">
            <ChartCard
              title={`${cellLabel(tech, stage)}: Energy Security against Economic Opportunity`}
              sub={`${showWord(show)} Each dot is one economy: further right is a bigger economic opening in this sector, higher up is more secure. The dashed lines are the medians of the economies shown, so each corner holds the economies above or below the middle on both scores. Colour shows the economy's place in the coalition.${show === 'change' ? ' Arrows run from the NEIS baseline position to the scenario’s.' : ''} Click a dot to select the economy.`}
              updating={updating}
              summary={`Scatter of ${pts.length} economies; median Energy Security ${fmtNum(med.es, 2)}, median Economic Opportunity ${fmtNum(med.eo, 2)}.`}
              legend={
                <Legend
                  items={[
                    { label: 'Coalition core', kind: 'dot', color: TIER_COLOR.core },
                    { label: 'Coalition partner', kind: 'dot', color: TIER_COLOR.partner },
                    { label: 'Other economy', kind: 'dot', color: TIER_COLOR.other },
                    { label: `Selected: ${app.byIso.get(sel)?.country ?? sel}`, kind: 'outline', color: SIGNAL },
                    ...(show === 'change' && moved.length ? [{ label: 'NEIS baseline → scenario', kind: 'line' as const, color: COMPARE_COLOR.baseline }] : []),
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
                    { id: 'es', header: 'Energy Security', value: (p) => p.es, cell: (p) => fmtNum(p.es, 3), num: true },
                    { id: 'es_base', header: 'Energy Security, baseline', value: (p) => p.es_base, cell: (p) => fmtNum(p.es_base, 3), num: true },
                    { id: 'eo', header: 'Economic Opportunity', value: (p) => p.eo, cell: (p) => fmtNum(p.eo, 3), num: true },
                    { id: 'eo_base', header: 'Economic Opportunity, baseline', value: (p) => p.eo_base, cell: (p) => fmtNum(p.eo_base, 3), num: true },
                    { id: 'region', header: 'Corner', value: (p) => regionOf(p.y, p.x, med.es, med.eo), minWidth: 260 },
                  ]}
                />
              )}
              csv={() => ({
                name: `space-${tech}-${stage}`,
                text: rowsToCsv(pts.map((p) => ({ iso3c: p.iso3c, Country: p.Country, tier: p.tier, tech: p.tech, supply_chain: p.stage, es: p.es, es_base: p.es_base, eo: p.eo, eo_base: p.eo_base, region: regionOf(p.y, p.x, med.es, med.eo) }))),
              })}
            >
              {pts.length > 1 ? (
                <PlotFigure<SpacePoint> build={buildScatter} height={420} ariaLabel="Opportunity-security scatter" tooltip={tip} onSelect={(p) => setSel(p.iso3c)} />
              ) : (
                <EmptyState>Too few economies have both scores in this sector.</EmptyState>
              )}
            </ChartCard>
            <ChartCard title="Economies in each corner" sub="How many economies fall in each corner of this sector (split at the medians), in the scenario and at the NEIS baseline, and how many of them are coalition members." updating={updating}>
              <TableView
                rows={counts}
                caption="Economies in each corner"
                pageSize={10}
                cols={[
                  { id: 'region', header: 'Corner', value: (r) => r.region, minWidth: 220 },
                  { id: 'scenario', header: 'Scenario', value: (r) => r.scenario, num: true },
                  { id: 'baseline', header: 'Baseline', value: (r) => r.baseline, num: true },
                  { id: 'coalition', header: 'Coalition', value: (r) => r.coalition, num: true, title: 'Coalition members, scenario' },
                ]}
              />
            </ChartCard>
          </div>
          <ChartCard
            title={`${app.byIso.get(sel)?.country ?? sel}: its own sectors`}
            sub={`The selected economy's sectors in the same space, one panel for each of the ${app.stages.length} supply-chain stages, named above it (on a narrow screen the panels wrap into rows): each dot is one technology. Labels are drawn where they fit; hover a dot or open the table for every sector.`}
            tools={<CountrySelect label="Economy" value={sel} options={options} onChange={setSel} className="w-56" />}
            updating={updating}
            summary={`Scatter of ${own.length} sectors of ${app.byIso.get(sel)?.country ?? sel}, one panel per supply-chain stage.`}
            legend={
              own.length ? (
                <Legend items={[{ label: `${app.byIso.get(sel)?.country ?? sel} (${tierLabel(own[0].tier)})`, kind: 'dot' as const, color: TIER_COLOR[own[0].tier] }]} />
              ) : undefined
            }
            table={() => (
              <TableView
                rows={own}
                caption="Own sectors"
                cols={[
                  { id: 'tech', header: 'Technology', value: (p) => p.tech },
                  { id: 'stage', header: 'Stage', value: (p) => stageLabel(p.stage), sortValue: (p) => stageRank(p.stage) },
                  { id: 'eo', header: 'Economic Opportunity', value: (p) => p.x, cell: (p) => fmtNum(p.x, 3), num: true },
                  { id: 'es', header: 'Energy Security', value: (p) => p.y, cell: (p) => fmtNum(p.y, 3), num: true },
                  { id: 'ok', header: 'Ranked', value: (p) => (p.ok ? 'yes' : 'no') },
                ]}
              />
            )}
          >
            {own.length ? (
              <PlotFigure<SpacePoint> build={buildOwn} height={OWN.top + OWN.panel + OWN.bottom} ariaLabel="The selected economy's cells by stage" tooltip={tip} />
            ) : (
              <EmptyState>No sector of this economy has both scores.</EmptyState>
            )}
          </ChartCard>
        </div>
      )}
    </div>
  );
}

