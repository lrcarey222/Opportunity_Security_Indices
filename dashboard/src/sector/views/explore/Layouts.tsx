// Explore layout substitutions by native level (spec_sector.md 4.6 table), each behind its banner:
//   region (IEA EV regions): regional bars, members in the table, the highlight's region outlined;
//   entity (IEA manufacturing): entity shares now and 2035, plus the measure across sectors;
//   world (prices, WEO): the board (latest, YoY, volatility, used in) and a detail chart of the item;
//   cell_global (TRL): the 11 x 3 grid with values printed, fallback cells marked with a dagger;
// and three measures that need their own form: area-integrated potential (a scatter against quality,
// never ranked), IEA mineral demand by technology, and CIM manufacturing capacity by product.

import { useCallback, useMemo } from 'react';
import { Link, useNavigate } from 'react-router';
import * as Plot from '@observablehq/plot';
import { PlotFigure, Tip } from '../../../charts/PlotFigure.tsx';
import { TableView } from '../../../components/TableView.tsx';
import { rowsToCsv } from '../../../lib/csv.ts';
import { GROUP_COLOR, SIGNAL } from '../../../theme/palette.ts';
import { STAGE_LABELS, STAGE_SLUGS, TECH_LABELS, TECH_SLUGS } from '../../contract.ts';
import { seqTile } from '../../derive/contrast.ts';
import { sectorCsv } from '../../derive/csv.ts';
import { fallbackCells, geoMark, SCATTER_PAIRS } from '../../derive/explore.ts';
import { demandByTech } from '../../derive/minerals.ts';
import { regionOf } from '../../derive/states.ts';
import { lc, fmtShare, fmtValue } from '../../format.ts';
import { ChunkGate } from '../common.tsx';
import { useSeries } from '../data.ts';
import { CellGrid, Dumbbell, GroupLegend, LeagueBars, SectorChartCard, type CellGridTech, type DumbbellRow, type LeagueRow } from '../kit.tsx';
import { MonthlyPrices, PriceBoard } from '../profile/Prices.tsx';
import { CapacityByProductCard, EntitySharesCard, EvRegionsCard } from '../profile/Production.tsx';
import { useSectorUrl } from '../state.tsx';
import { AcrossSectors } from './CountryLayout.tsx';
import { indexFor, type ExploreCtx } from './ctx.ts';

const EV_REGION_VARS = new Set(['production.ev_region', 'production.ev_region_sales', 'manufacturing.ev_net_export_ratio']);

export function RegionLayout({ c }: { c: ExploreCtx }) {
  const { model, v, item, year, highlight, facts } = c;
  const hlRegion = highlight ? regionOf(model, highlight) : null;
  if (!item) return null;
  if (EV_REGION_VARS.has(v.id) && facts) return <EvRegionsCard cell={item} facts={facts} highlightRegion={hlRegion} />;
  const idx = indexFor(c, year);
  if (!idx || year === null) return <p className="text-sm text-paper-mute">No values for this selection.</p>;
  const unit = model.unitOf(v, item.idx);
  const rows = idx.rows(v.idx, item.idx, year).filter((r) => model.geos[r.geo]?.kind === 'region' && Number.isFinite(r.value)).sort((a, z) => z.value - a.value);
  const league: LeagueRow[] = rows.map((r) => ({ id: model.geos[r.geo].code, label: model.geos[r.geo].name, group: 'rest', value: r.value, display: fmtValue(r.value, unit, { scale: v.display_scale }) }));
  return (
    <SectorChartCard
      title={`${v.short} by region, ${year}`}
      vars={[{ var: v.id, item: item.slug, year }]}
      table={() => <TableView rows={rows} caption="Regions" rowKey={(r) => model.geos[r.geo].code} selectedKey={hlRegion?.code ?? null} cols={[{ id: 'r', header: 'Region', value: (r) => model.geos[r.geo].name }, { id: 'v', header: `Value (${unit.short})`, value: (r) => r.value, num: true }, { id: 'm', header: 'Members', value: (r) => (model.regionMembers.get(r.geo) ?? []).map((m) => model.geos[m]?.code).join(', ') || null }]} />}
      csv={() => ({ name: `${v.id}-${item.slug}-regions-${year}`, text: sectorCsv(model, rows.map((r) => ({ geo: null, economy: model.geos[r.geo].name, v, item, year, value: r.value, status: r.status }))) })}
      notes={hlRegion ? <p>{highlight?.name} is part of {hlRegion.name}.</p> : undefined}
      summary={rows[0] ? `${model.geos[rows[0].geo].name} is the largest region.` : 'No regions.'}
    >
      <LeagueBars rows={league} axisLabel={`${unit.long}, ${year}`} selected={hlRegion?.code ?? null} ariaLabel={`${v.short} by region`} />
    </SectorChartCard>
  );
}

export function EntityLayout({ c }: { c: ExploreCtx }) {
  if (!c.item) return null;
  return (
    <div className="space-y-6">
      <EntitySharesCard cell={c.item} facts={c.facts} />
      <AcrossSectors c={c} />
    </div>
  );
}

export function WorldLayout({ c }: { c: ExploreCtx }) {
  const { model, v, item, facts } = c;
  if (v.theme === 'prices' && v.item_axis === 'commodity') {
    const latest = model.varById.get('prices.latest');
    const items = latest ? model.itemsWithData(latest) : [];
    const sel = item && items.some((x) => x.idx === item.idx) ? item : items[0];
    return (
      <div className="space-y-6">
        <PriceBoard items={items} title="Price board" />
        {sel && (
          <div>
            <p className="mb-2 text-sm text-paper-dim">
              Detail: <span className="text-paper">{sel.label}</span> (pick another commodity with Item above).
            </p>
            <MonthlyPrices items={[sel]} />
          </div>
        )}
      </div>
    );
  }
  // other world-level measures (WEO): items x years, projections labelled
  const wld = model.world;
  if (!facts || !wld) return null;
  const items = model.itemsWithData(v);
  const years = [...new Set(items.flatMap((it) => facts.yearsOf(v.idx, it.idx)))].sort((a, z) => a - z);
  const proj = (y: number) => v.projection_from !== null && y >= v.projection_from;
  const val = (it: (typeof items)[number], y: number) => facts.value(v.idx, it.idx, y, wld.idx)?.value ?? NaN;
  const unit = (it: (typeof items)[number]) => model.unitOf(v, it.idx);
  const y0 = years.filter((y) => !proj(y)).pop() ?? years[0];
  const y1 = years.filter(proj).pop() ?? null;
  const dumb: DumbbellRow[] = y1 !== null ? items.map((it) => ({ id: it.slug, label: it.label, a: val(it, y1), b: val(it, y0), group: 'neutral', aLabel: `${fmtValue(val(it, y1), unit(it), { scale: v.display_scale })} (${y1}, projection)`, bLabel: `${fmtValue(val(it, y0), unit(it), { scale: v.display_scale })} (${y0})`, href: undefined })) : [];
  return (
    <SectorChartCard
      title={`${v.label}`}
      vars={[{ var: v.id, window: years.length > 1 ? [years[0], years[years.length - 1]] : null, year: years.length === 1 ? years[0] : null }]}
      table={() => <TableView rows={items} caption={v.label} cols={[{ id: 'i', header: 'Item', value: (it) => it.label }, ...years.map((y) => ({ id: String(y), header: proj(y) ? `${y} (projection)` : String(y), value: (it: (typeof items)[number]) => val(it, y), cell: (it: (typeof items)[number]) => fmtValue(val(it, y), unit(it), { scale: v.display_scale, ctx: 'table' }), num: true }))]} />}
      csv={() => ({ name: `${v.id}-world`, text: sectorCsv(model, items.flatMap((it) => years.map((y) => ({ geo: wld, v, item: it, year: y, value: val(it, y), status: proj(y) ? 'projection' : 'obs' })))) })}
      summary={`${v.label} for ${items.length} items.`}
    >
      {dumb.length ? <Dumbbell rows={dumb} xLabel={model.unitOf(v, null).long} aName={`${y1} (projection)`} bName={String(y0)} ariaLabel={v.label} /> : <p className="text-sm text-paper-mute">Use the table view for the values.</p>}
    </SectorChartCard>
  );
}

export function CellGlobalLayout({ c }: { c: ExploreCtx }) {
  const { model, v, facts } = c;
  const { link } = useSectorUrl();
  const wld = model.world;
  const fb = useMemo(() => fallbackCells(model, v), [model, v]);
  const vals = useMemo(() => {
    const m = new Map<string, { value: number; year: number }>();
    if (!facts || !wld) return m;
    for (const cell of model.cells) {
      const y = facts.latestYear(v.idx, cell.idx);
      const f = y !== null ? facts.value(v.idx, cell.idx, y, wld.idx) : null;
      if (f && Number.isFinite(f.value) && y !== null) m.set(cell.slug, { value: f.value, year: y });
    }
    return m;
  }, [facts, wld, model, v]);
  if (!vals.size) return <p className="text-sm text-paper-mute">No values in this build.</p>;
  const xs = [...vals.values()].map((x) => x.value);
  const lo = Math.min(...xs);
  const hi = Math.max(...xs);
  const unit = v.unit;
  const techs: CellGridTech[] = TECH_SLUGS.filter((t) => model.cells.some((x) => x.techSlug === t)).map((t) => ({ slug: t, label: TECH_LABELS[t], headline: t !== 'semiconductors' }));
  const rows = model.cells.filter((x) => vals.has(x.slug));
  const year = Math.max(...[...vals.values()].map((x) => x.year));
  return (
    <SectorChartCard
      title={`${v.label} by sector, ${year}`}
      vars={[{ var: v.id, year }]}
      table={() => <TableView rows={rows} caption={v.label} cols={[{ id: 'c', header: 'Sector', value: (x) => model.cellLabel(x) }, { id: 'v', header: `${v.short} (${unit.short})`, value: (x) => vals.get(x.slug)?.value ?? null, cell: (x) => fmtValue(vals.get(x.slug)?.value, unit, { scale: v.display_scale, ctx: 'table' }), num: true }, { id: 'f', header: 'Technology-wide value', value: (x) => (fb.has(x.slug) ? 'yes' : '') }]} />}
      csv={() => ({ name: `${v.id}-by-cell`, text: sectorCsv(model, rows.map((x) => ({ geo: wld, v, item: x, year: vals.get(x.slug)?.year ?? null, value: vals.get(x.slug)?.value ?? null, status: fb.has(x.slug) ? 'fallback' : 'obs' }))) })}
      notes={fb.size ? <p>† The technology-wide value: the IEA Guide has no items of its own for this cell.</p> : undefined}
      summary={`${v.label} of ${rows.length} sectors, from ${fmtValue(lo, unit, { scale: v.display_scale })} to ${fmtValue(hi, unit, { scale: v.display_scale })}.`}
    >
      <CellGrid
        techs={techs}
        stages={STAGE_SLUGS.map((s) => ({ slug: s, label: STAGE_LABELS[s] }))}
        ariaLabel={`${v.label} by technology and stage`}
        renderTile={(t, stage) => {
          const cell = model.cell(t.slug as never, stage);
          if (!cell) return <div className="min-h-[64px]" aria-hidden="true" />;
          const x = vals.get(cell.slug);
          if (!x) return <div className="min-h-[64px] border hairline p-2 text-xs text-paper-mute">no value</div>;
          const shade = seqTile(x.value, lo, hi === lo ? lo + 1 : hi);
          return (
            <Link to={link(`/sector/${t.slug}/${stage}`, {}, 'technology')} className="block min-h-[64px] p-2 text-sm font-medium" style={{ background: shade?.bg, color: shade?.fg }} aria-label={`${model.cellLabel(cell)}: ${v.short} ${fmtValue(x.value, unit, { scale: v.display_scale })}${fb.has(cell.slug) ? ', technology-wide value' : ''}`}>
              {fmtValue(x.value, unit, { scale: v.display_scale })}
              {fb.has(cell.slug) && <span aria-hidden="true"> †</span>}
            </Link>
          );
        }}
      />
    </SectorChartCard>
  );
}

export function ScatterLayout({ c }: { c: ExploreCtx }) {
  const { model, v, item, facts, highlight } = c;
  const q = model.varById.get(SCATTER_PAIRS[v.id] ?? '');
  const data = useMemo(() => {
    if (!facts || !item || !q) return [];
    const yq = facts.latestYear(q.idx, item.idx);
    const ya = facts.latestYear(v.idx, item.idx);
    if (yq === null || ya === null) return [];
    const out: { code: string; name: string; group: ReturnType<typeof geoMark>; x: number; y: number; hl: boolean }[] = [];
    for (const r of facts.rows(q.idx, item.idx, yq)) {
      const g = model.geos[r.geo];
      if (!g || g.kind !== 'country' || !Number.isFinite(r.value)) continue;
      const a = facts.value(v.idx, item.idx, ya, r.geo);
      if (!a || !(a.value > 0)) continue;
      out.push({ code: g.code, name: g.name, group: geoMark(g), x: a.value * v.display_scale, y: r.value * q.display_scale, hl: highlight?.idx === r.geo });
    }
    return out;
  }, [facts, item, q, v, model, highlight]);
  const build = useCallback(
    (width: number) => ({
      width,
      x: { type: 'log' as const, label: `${v.short} (${model.unitOf(v, item?.idx ?? null).short}, log scale)`, grid: true },
      y: { label: `${q?.short ?? ''} (${q ? model.unitOf(q, item?.idx ?? null).short : ''})`, grid: true },
      marks: [
        Plot.dot(data, { x: 'x', y: 'y', r: 4, fill: (d: (typeof data)[number]) => GROUP_COLOR[d.group], fillOpacity: 0.85 }),
        Plot.dot(data.filter((d) => d.hl), { x: 'x', y: 'y', r: 7, stroke: SIGNAL, strokeWidth: 2, fill: 'none' }),
        Plot.text(data.filter((d) => d.hl || d.code === 'CHN'), { x: 'x', y: 'y', text: 'name', dy: -10, fill: '#d1d5db', fontSize: 11 }),
        Plot.dot(data, Plot.pointer({ x: 'x', y: 'y', r: 9, stroke: SIGNAL, fill: 'none' })),
      ],
    }),
    [data, v, q, model, item],
  );
  if (!q || !item) return null;
  if (data.length < 2) return <p className="text-sm text-paper-mute">Too few economies with both values.</p>;
  return (
    <SectorChartCard
      title={`${v.short} against ${lc(q.short)}`}
      vars={[{ var: v.id, item: item.slug }, { var: q.id, item: item.slug }]}
      table={() => <TableView rows={data} caption="Potential and quality" search cols={[{ id: 'c', header: 'Economy', value: (d) => d.name }, { id: 'y', header: q.short, value: (d) => d.y, num: true }, { id: 'x', header: v.short, value: (d) => d.x, num: true }]} />}
      csv={() => ({ name: `${v.id}-scatter`, text: rowsToCsv(data.map((d) => ({ iso3c: d.code, economy: d.name, [v.id]: d.x, [q.id]: d.y, source: v.source.short }))) })}
      legend={<GroupLegend groups={['china', 'core', 'partner', 'rest']} highlight={highlight?.name ?? null} />}
      summary={`${data.length} economies by ${lc(q.short)} and ${lc(v.short)}.`}
    >
      <PlotFigure<(typeof data)[number]> build={build} height={340} ariaLabel={`Scatter of ${v.short} against ${q.short}`} tooltip={(d) => <Tip title={d.name} value={`${d.y.toLocaleString('en-US')} · ${d.x.toLocaleString('en-US')}`} />} onSelect={(d) => c.setHighlight(model.geoByCode.get(d.code) ?? null)} />
    </SectorChartCard>
  );
}

export function MineralDemandLayout({ c }: { c: ExploreCtx }) {
  const { model, item } = c;
  const { link } = useSectorUrl();
  const navigate = useNavigate();
  const minerals = model.items.filter((x) => x.axis === 'mineral' && model.mineralDemand.some((d) => d.mineral === x.idx));
  const sel = item && minerals.some((m) => m.idx === item.idx) ? item : minerals[0];
  const { rows, total } = useMemo(() => (sel ? demandByTech(model, sel) : { rows: [], total: null }), [model, sel]);
  if (!sel || !rows.length) return <p className="text-sm text-paper-mute">No IEA demand data in this build.</p>;
  const kt = (x: number) => `${x.toLocaleString('en-US', { maximumFractionDigits: 0 })} kt`;
  const projYear = rows.find((r) => r.projectedYear !== null)?.projectedYear ?? null;
  const dumb: DumbbellRow[] = rows.map((r) => ({ id: r.tech ?? r.label, label: r.label, a: r.projected, b: r.observed, group: 'neutral', aLabel: Number.isFinite(r.projected) ? `${kt(r.projected)} (${r.projectedYear} ${r.scenario}, projection)` : '–', bLabel: `${kt(r.observed)} (${r.observedYear})${r.share !== null ? ` · ${fmtShare(r.share)} of clean-energy demand` : ''}`, href: r.tech ? link(`/sector/${r.tech}`) : undefined }));
  return (
    <SectorChartCard
      title={`Clean-energy demand for ${lc(sel.label)} by technology`}
      vars={[{ var: 'consumption.mineral_demand', item: sel.slug, window: projYear ? [rows[0].observedYear, projYear] : null }]}
      table={() => <TableView rows={rows} caption="Demand by technology" cols={[{ id: 't', header: 'Technology', value: (r) => r.label }, { id: 'o', header: `Observed ${rows[0].observedYear}, kt`, value: (r) => r.observed, num: true }, { id: 'p', header: projYear ? `${projYear} (projection), kt` : 'Projection, kt', value: (r) => (Number.isFinite(r.projected) ? r.projected : null), num: true }, { id: 's', header: 'Share', value: (r) => r.share, cell: (r) => fmtShare(r.share), num: true }]} />}
      csv={() => ({ name: `mineral-demand-${sel.slug}`, text: rowsToCsv(rows.map((r) => ({ mineral: sel.slug, technology: r.label, observed_year: r.observedYear, observed_kt: r.observed, projected_year: r.projectedYear, scenario: r.scenario, projected_kt: Number.isFinite(r.projected) ? r.projected : null, share: r.share, source: 'IEA Critical Minerals Dataset' }))) })}
      notes={total ? <p>Total clean-energy demand {kt(total.observed)} ({total.observedYear}). <Link className="link" to={link(`/sector/minerals/${sel.slug}`)}>The {lc(sel.label)} page</Link>.</p> : undefined}
      summary={`${rows[0].label} uses the most ${lc(sel.label)}.`}
    >
      <Dumbbell rows={dumb} xLabel="Demand, kt" aName={projYear ? `${projYear} (projection)` : 'Projection'} bName={String(rows[0].observedYear)} ariaLabel={`Demand for ${lc(sel.label)} by technology`} onSelect={(r) => r.href && navigate(r.href)} />
    </SectorChartCard>
  );
}

export function CapacityLayout({ c }: { c: ExploreCtx }) {
  const { item, highlight } = c;
  const { h } = useSeries(true);
  if (!item) return null;
  return <ChunkGate h={h} name="sector-series">{() => <CapacityByProductCard cell={item} highlight={highlight} />}</ChunkGate>;
}
