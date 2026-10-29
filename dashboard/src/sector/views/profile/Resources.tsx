// Profile S6 Resources & potential (spec_sector.md 4.3): fuel reserves (EI, end-2020, not updated) and
// uranium resources (Red Book) for fuel and nuclear Extraction; USGS reserves of the minerals a clean
// technology uses, at Extraction (holders without a figure listed as such, never as 0); for Solar,
// Wind and Geothermal Deployment the resource quality (map + league), the scatter of area-integrated
// potential against quality, and the geothermal LCOE at 500 MW against 5 GW.

import { useCallback, useMemo } from 'react';
import { useNavigate } from 'react-router';
import * as Plot from '@observablehq/plot';
import { PlotFigure, Tip } from '../../../charts/PlotFigure.tsx';
import { TableView } from '../../../components/TableView.tsx';
import { GROUP_COLOR, SIGNAL } from '../../../theme/palette.ts';
import type { ItemInfo, SectorModel, VarInfo } from '../../data/model.ts';
import { sectorCsv } from '../../derive/csv.ts';
import { fmtValue, listText } from '../../format.ts';
import { HowToRead, markGroup, measureConc, SPLIT_COLUMNS, SPLIT_READING, shareSplitRow, SplitTable, splitRowsCsv } from '../common.tsx';
import { Dumbbell, GroupLegend, SectorChartCard, ShareSplitChart, type DumbbellRow, type ShareSplitRow } from '../kit.tsx';
import { useSectorUrl } from '../state.tsx';
import { baseYear } from '../TechChainView.tsx';
import { itemForCell, linkedWith, useProfile, vOf } from './ctx.tsx';
import { SliceLeague } from './Production.tsx';

const QUALITY: Record<string, { quality: string; area?: string; extra?: string[] }> = {
  solar: { quality: 'resources.pv_quality', area: 'resources.pv_area' },
  wind: { quality: 'resources.wind_density', area: 'resources.wind_area', extra: ['resources.wind_share'] },
  geothermal: { quality: 'resources.geo_potential' },
};

function qualityVars(model: SectorModel, cell: ItemInfo): { q: VarInfo | null; area: VarInfo | null; extra: VarInfo[] } {
  const spec = cell.techSlug && cell.stageSlug === 'deployment' ? QUALITY[cell.techSlug] : undefined;
  if (!spec) return { q: null, area: null, extra: [] };
  const ok = (id: string | undefined) => {
    const v = id ? vOf(model, id) : null;
    return v && itemForCell(model, v, cell) ? v : null;
  };
  return { q: ok(spec.quality), area: ok(spec.area), extra: (spec.extra ?? []).map(ok).filter((v): v is VarInfo => !!v) };
}

export function hasResources(model: SectorModel, cell: ItemInfo): boolean {
  const uran = vOf(model, 'resources.uranium_resources');
  if (cell.slug === 'nuclear-extraction' && uran && itemForCell(model, uran, cell)) return true;
  if (cell.stageSlug === 'extraction') {
    const fr = vOf(model, 'resources.fuel_reserves');
    if (fr && linkedWith(model, cell, 'fuel', fr).length) return true;
    const mr = vOf(model, 'resources.mineral_reserves');
    if (mr && linkedWith(model, cell, 'mineral', mr).length) return true;
  }
  const q = qualityVars(model, cell);
  if (q.q) return true;
  const g500 = vOf(model, 'resources.geo_lcoe_500');
  return !!(cell.slug === 'geothermal-deployment' && g500 && itemForCell(model, g500, cell));
}

function MineralReserves() {
  const { model, cell, facts, highlight, eu } = useProfile();
  const { link } = useSectorUrl();
  const navigate = useNavigate();
  const v = vOf(model, 'resources.mineral_reserves');
  if (!v || !facts) return null;
  const rows: ShareSplitRow[] = [];
  const noFigure: string[] = [];
  let year: number | null = null;
  for (const m of linkedWith(model, cell, 'mineral', v)) {
    const y = baseYear(model, facts, v, m, eu);
    if (y === null) continue;
    year = y;
    const c = measureConc(model, facts, v, m, y, eu);
    if (!c || c.conc.W === null) continue;
    rows.push(shareSplitRow(model, v, m, c.conc, { id: m.slug, label: `${m.label} reserves`, year: y, highlight, href: link(`/sector/minerals/${m.slug}`) }));
    const nf = facts.rows(v.idx, m.idx, y).filter((r) => r.status === 'nq').map((r) => model.geos[r.geo]?.name).filter(Boolean) as string[];
    if (nf.length) noFigure.push(`${m.label}: ${listText(nf)}`);
  }
  if (!rows.length) return null;
  return (
    <SectorChartCard
      title={`Reserves of the minerals ${cell.tech} uses`}
      vars={rows.map((r) => ({ var: v.id, item: r.id, year }))}
      subExtra={<HowToRead>{SPLIT_READING} Reserves are deposits known and economic to mine today, so they show where future supply could come from, not where it comes from now. Click a row for the mineral’s page.</HowToRead>}
      table={() => <SplitTable rows={rows} caption="Reserves by mineral" />}
      csv={() => ({ name: `${cell.slug}-mineral-reserves`, text: splitRowsCsv(rows) })}
      legend={<GroupLegend highlight={highlight?.name ?? null} />}
      notes={noFigure.length ? <p>Reported without a figure (never counted as 0): {noFigure.join('; ')}.</p> : undefined}
      summary={rows.map((r) => r.ariaLabel).slice(0, 2).join(' ')}
    >
      <ShareSplitChart rows={rows} columns={SPLIT_COLUMNS} ariaLabel="Reserves by mineral" onRowClick={(r) => r.href && navigate(r.href)} highlightHeader={highlight?.name} />
    </SectorChartCard>
  );
}

/** Area-integrated potential (x, log) against quality (y): "total = mean × land area, so it ranks mostly by size". */
function QualityScatter({ q, area }: { q: VarInfo; area: VarInfo }) {
  const { model, cell, facts, highlight } = useProfile();
  const { set, state } = useSectorUrl();
  const iq = itemForCell(model, q, cell);
  const ia = itemForCell(model, area, cell);
  const data = useMemo(() => {
    if (!facts || !iq || !ia) return [];
    const yq = facts.latestYear(q.idx, iq.idx);
    const ya = facts.latestYear(area.idx, ia.idx);
    if (yq === null || ya === null) return [];
    const out: { code: string; name: string; group: ReturnType<typeof markGroup>; x: number; y: number; hl: boolean }[] = [];
    for (const r of facts.rows(q.idx, iq.idx, yq)) {
      const g = model.geos[r.geo];
      if (!g || g.kind !== 'country' || !Number.isFinite(r.value)) continue;
      const a = facts.value(area.idx, ia.idx, ya, r.geo);
      if (!a || !(a.value > 0)) continue;
      out.push({ code: g.code, name: g.name, group: markGroup(g), x: a.value * area.display_scale, y: r.value * q.display_scale, hl: highlight?.idx === r.geo });
    }
    return out;
  }, [facts, iq, ia, q, area, model, highlight]);
  const uq = model.unitOf(q, iq?.idx ?? null);
  const ua = model.unitOf(area, ia?.idx ?? null);
  const build = useCallback(
    (width: number) => ({
      width,
      x: { type: 'log' as const, label: `${area.short} (${ua.short}, log scale)`, grid: true },
      y: { label: `${q.short} (${uq.short})`, grid: true },
      marks: [
        Plot.dot(data, { x: 'x', y: 'y', r: 4, fill: (d: (typeof data)[number]) => GROUP_COLOR[d.group], fillOpacity: 0.85 }),
        Plot.dot(data.filter((d) => d.hl), { x: 'x', y: 'y', r: 7, stroke: SIGNAL, strokeWidth: 2, fill: 'none' }),
        Plot.text(data.filter((d) => d.hl || d.code === 'CHN'), { x: 'x', y: 'y', text: 'name', dy: -10, fill: '#d1d5db', fontSize: 11 }),
        Plot.dot(data, Plot.pointer({ x: 'x', y: 'y', r: 9, stroke: SIGNAL, fill: 'none' })),
      ],
    }),
    [data, area, q, ua, uq],
  );
  if (data.length < 3) return null;
  return (
    <SectorChartCard
      title={`${q.short} against area-integrated potential`}
      vars={[{ var: q.id, item: iq?.slug ?? null }, { var: area.id, item: ia?.slug ?? null }]}
      subExtra={<HowToRead>Each dot is one economy. Higher means a better resource on average (more power from each solar panel, or stronger wind over each square metre); further right means more potential in total, the average times land area, so it mostly reflects how large the country is. A dot high and to the left is a small country with an excellent resource. Click a dot to highlight the economy.</HowToRead>}
      table={() => <TableView rows={data} caption="Quality and potential" search cols={[{ id: 'c', header: 'Economy', value: (d) => d.name }, { id: 'y', header: `${q.short} (${uq.short})`, value: (d) => d.y, num: true }, { id: 'x', header: `${area.short} (${ua.short})`, value: (d) => d.x, num: true }]} />}
      csv={() => ({ name: `${cell.slug}-quality-potential`, text: sectorCsv(model, data.flatMap((d) => { const g = model.geoByCode.get(d.code) ?? null; return [{ geo: g, v: q, item: iq, year: null, value: d.y / q.display_scale, status: 'obs' }, { geo: g, v: area, item: ia, year: null, value: d.x / area.display_scale, status: 'obs' }]; })) })}
      legend={<GroupLegend groups={['china', 'core', 'partner', 'rest']} highlight={highlight?.name ?? null} />}
      notes={<p>Total = mean × land area, so it ranks mostly by size: area-integrated potential is shown here only, never ranked.</p>}
      summary={`${data.length} economies by resource quality and area-integrated potential.`}
    >
      <PlotFigure<(typeof data)[number]>
        build={build}
        height={340}
        ariaLabel={`Scatter of ${q.short} against ${area.short}`}
        tooltip={(d) => <Tip value={`${fmtValue(d.y, uq)} · ${fmtValue(d.x, ua)}`} title={d.name} />}
        onSelect={(d) => set({ c: d.code === state.c ? null : d.code })}
      />
    </SectorChartCard>
  );
}

function GeoLcoe() {
  const { model, cell, facts, highlight } = useProfile();
  const v5 = vOf(model, 'resources.geo_lcoe_500');
  const v50 = vOf(model, 'resources.geo_lcoe_5000');
  if (!facts || !v5 || !v50) return null;
  const i5 = itemForCell(model, v5, cell);
  const i50 = itemForCell(model, v50, cell);
  if (!i5 || !i50) return null;
  const y5 = facts.latestYear(v5.idx, i5.idx);
  const y50 = facts.latestYear(v50.idx, i50.idx);
  if (y5 === null) return null;
  const u = model.unitOf(v5, i5.idx);
  const rows: DumbbellRow[] = facts
    .rows(v5.idx, i5.idx, y5)
    .filter((r) => model.geos[r.geo]?.kind === 'country' && Number.isFinite(r.value))
    .map((r) => {
      const b = y50 !== null ? facts.value(v50.idx, i50.idx, y50, r.geo) : null;
      const g = model.geos[r.geo];
      return { id: g.code, label: g.name, a: r.value, b: b && Number.isFinite(b.value) ? b.value : NaN, group: markGroup(g), aLabel: `${fmtValue(r.value, u)} at 500 MW`, bLabel: b && Number.isFinite(b.value) ? `${fmtValue(b.value, u)} at 5 GW` : 'no 5 GW tranche' };
    })
    .sort((a, z) => a.a - z.a)
    .slice(0, 25);
  if (!rows.length) return null;
  return (
    <SectorChartCard
      title="Enhanced geothermal: cost of electricity at 500 MW against 5 GW"
      vars={[{ var: v5.id, item: i5.slug }, { var: v50.id, item: i50.slug }]}
      subExtra={<HowToRead>Each row is one economy, cheapest first; the two dots are the estimated levelised cost of electricity (the average cost per unit of power over a plant’s life) of the first 500 MW of enhanced geothermal and of a 5 GW build-out. Further left is cheaper; a line running to the right means costs rise as the best sites are used up.</HowToRead>}
      table={() => <TableView rows={rows} caption="Geothermal cost of electricity" cols={[{ id: 'c', header: 'Economy', value: (r) => r.label }, { id: 'a', header: '500 MW', value: (r) => r.aLabel }, { id: 'b', header: '5 GW', value: (r) => r.bLabel }]} />}
      csv={() => ({ name: 'geothermal-lcoe', text: sectorCsv(model, rows.flatMap((r) => { const g = model.geoByCode.get(r.id) ?? null; return [{ geo: g, v: v5, item: i5, year: null, value: Number.isFinite(r.a) ? r.a : null, status: Number.isFinite(r.a) ? 'obs' : null }, { geo: g, v: v50, item: i50, year: null, value: Number.isFinite(r.b) ? r.b : null, status: Number.isFinite(r.b) ? 'obs' : null }]; })) })}
      notes={<p>The unit reads "per kWh, currency as published (unverified)": unit to confirm. "No 5 GW tranche" is shown as text, not as a value.</p>}
      summary={`${rows[0].label} has the lowest cost of electricity at 500 MW.`}
    >
      <Dumbbell rows={rows} xLabel={`Levelised cost of electricity (${u.short || u.long})`} aName="500 MW" bName="5 GW" ariaLabel="Geothermal cost of electricity at 500 MW and 5 GW" selected={highlight?.code ?? null} />
    </SectorChartCard>
  );
}

export function ResourcesSection() {
  const { model, cell, facts, eu } = useProfile();
  const fr = vOf(model, 'resources.fuel_reserves');
  const fuels = cell.stageSlug === 'extraction' && fr ? linkedWith(model, cell, 'fuel', fr) : [];
  const uran = vOf(model, 'resources.uranium_resources');
  const uItem = uran && cell.slug === 'nuclear-extraction' ? itemForCell(model, uran, cell) : null;
  const { q, area, extra } = qualityVars(model, cell);
  const qItem = q ? itemForCell(model, q, cell) : null;
  const yearOf = (v: VarInfo, it: ItemInfo) => baseYear(model, facts, v, it, eu);
  return (
    <div className="space-y-6">
      {fr &&
        fuels.map((f) => {
          const y = yearOf(fr, f);
          return y === null ? null : <SliceLeague key={f.slug} v={fr} item={f} year={y} title={`${f.label}: proved reserves`} notes={<p>{fr.data_year}: the Energy Institute stopped updating reserves after 2020.</p>} />;
        })}
      {uran && uItem && (() => {
        const y = yearOf(uran, uItem);
        return y === null ? null : <SliceLeague v={uran} item={uItem} year={y} title="Identified recoverable uranium" />;
      })()}
      {cell.stageSlug === 'extraction' && <MineralReserves />}
      {q && qItem && (() => {
        const y = yearOf(q, qItem) ?? facts?.latestYear(q.idx, qItem.idx) ?? null;
        return y === null ? null : <SliceLeague v={q} item={qItem} year={y} title={q.label} />;
      })()}
      {extra.map((v) => {
        const it = itemForCell(model, v, cell);
        const y = it ? (facts?.latestYear(v.idx, it.idx) ?? null) : null;
        return it && y !== null ? <SliceLeague key={v.id} v={v} item={it} year={y} title={v.label} notes={<p>Threshold and hub height are not recorded in the source: unit to confirm.</p>} /> : null;
      })}
      {q && area && <QualityScatter q={q} area={area} />}
      {cell.slug === 'geothermal-deployment' && <GeoLcoe />}
    </div>
  );
}
