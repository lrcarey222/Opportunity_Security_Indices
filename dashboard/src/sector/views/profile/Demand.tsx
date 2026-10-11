// Profile S8 Demand & deployment (spec_sector.md 4.3): EI consumption per person (top 25 + highlight)
// and the 2020 -> 2025 change ("new" / "n/a" in words), the EI consumption series (sector-series);
// EV sales, stock and sales share (GEVO), 2022 -> 2025 and the 2035 projection; the WEO world outlook
// as a tile pair; Midstream cells link to the importers ("who buys").

import { StatTile } from '../../../components/ui.tsx';
import { TableView } from '../../../components/TableView.tsx';
import type { ItemInfo, SectorModel, VarInfo } from '../../data/model.ts';
import { lc, fmtSignedValue, fmtValue } from '../../format.ts';
import { markGroup } from '../common.tsx';
import { Dumbbell, SectorChartCard, type DumbbellRow } from '../kit.tsx';
import { useSectorUrl } from '../state.tsx';
import { baseYear } from '../TechChainView.tsx';
import { itemForCell, linkedWith, useProfile, vOf } from './ctx.tsx';
import { SeriesBlock, SliceLeague } from './Production.tsx';
import { sectorCsv } from '../../derive/csv.ts';

const EV_VARS = ['consumption.ev_sales', 'consumption.ev_stock', 'consumption.ev_sales_share'];

function fuelsOf(model: SectorModel, cell: ItemInfo): ItemInfo[] {
  const pc = vOf(model, 'consumption.per_capita');
  return cell.stageSlug === 'downstream' && pc ? linkedWith(model, cell, 'fuel', pc) : [];
}

export function hasDemand(model: SectorModel, cell: ItemInfo): boolean {
  if (fuelsOf(model, cell).length) return true;
  if (EV_VARS.some((id) => { const v = vOf(model, id); return v && itemForCell(model, v, cell) && v.item_axis !== 'none'; })) return true;
  return cell.stageSlug === 'midstream' && cell.has_trade;
}

function WorldOutlook({ fuel }: { fuel: ItemInfo }) {
  const { model, facts } = useProfile();
  const v = vOf(model, 'consumption.weo');
  if (!v || !facts || !model.world) return null;
  const ys = facts.yearsOf(v.idx, fuel.idx);
  if (ys.length < 2) return null;
  const a = facts.value(v.idx, fuel.idx, ys[0], model.world.idx);
  const b = facts.value(v.idx, fuel.idx, ys[ys.length - 1], model.world.idx);
  if (!a || !b) return null;
  const u = model.unitOf(v, fuel.idx);
  const proj = v.projection_from !== null && ys[ys.length - 1] >= v.projection_from;
  return (
    <SectorChartCard
      title={`World ${lc(fuel.label)} supply: ${ys[0]} and ${ys[ys.length - 1]}${proj ? ' (STEPS projection)' : ''}`}
      vars={[{ var: v.id, item: fuel.slug, year: ys[0] }, { var: v.id, item: fuel.slug, year: ys[ys.length - 1] }]}
      table={() => <TableView rows={ys.map((y) => ({ y, v: facts.value(v.idx, fuel.idx, y, model.world!.idx)?.value ?? NaN }))} caption="World outlook" cols={[{ id: 'y', header: 'Year', value: (r) => r.y }, { id: 'v', header: u.short, value: (r) => r.v, num: true }]} />}
      csv={() => ({ name: `weo-${fuel.slug}`, text: sectorCsv(model, ys.map((y) => { const f = facts.value(v.idx, fuel.idx, y, model.world!.idx); return { geo: model.world, v, item: fuel, year: y, value: f?.value ?? null, status: v.projection_from !== null && y >= v.projection_from ? 'projection' : (f?.status ?? 'obs') }; })) })}
      summary={`World supply moves from ${fmtValue(a.value, u)} to ${fmtValue(b.value, u)}.`}
    >
      <div className="grid max-w-lg grid-cols-2 gap-3">
        <StatTile label={String(ys[0])} value={fmtValue(a.value, u, { scale: v.display_scale })} sub="World" />
        <StatTile label={`${ys[ys.length - 1]}${proj ? ' · projection' : ''}`} value={fmtValue(b.value, u, { scale: v.display_scale })} sub={`${fmtSignedValue(b.value - a.value, u, { scale: v.display_scale })} · world`} />
      </div>
      <p className="mt-2 text-xs text-paper-mute">A world value: the same for every country.</p>
    </SectorChartCard>
  );
}

function EvGrowth({ v }: { v: VarInfo }) {
  const { model, cell, facts, highlight } = useProfile();
  if (!facts) return null;
  const ys = facts.yearsOf(v.idx, cell.idx).filter((y) => v.projection_from === null || y < v.projection_from);
  if (ys.length < 2) return null;
  const y0 = ys[0];
  const y1 = ys[ys.length - 1];
  const u = model.unitOf(v, cell.idx);
  const rows: DumbbellRow[] = facts
    .rows(v.idx, cell.idx, y1)
    .filter((r) => model.geos[r.geo]?.kind === 'country' && Number.isFinite(r.value))
    .map((r) => {
      const a = facts.value(v.idx, cell.idx, y0, r.geo);
      const base = a && Number.isFinite(a.value) ? a.value : NaN;
      const g = model.geos[r.geo];
      const growthOk = Number.isFinite(base) && (v.min_base === null || base >= v.min_base) && base > 0;
      return { id: g.code, label: g.name, a: base, b: r.value, group: markGroup(g), aLabel: `${fmtValue(base, u)} (${y0})`, bLabel: `${fmtValue(r.value, u)} (${y1})${growthOk ? '' : ' · growth n/a (small base)'}` };
    })
    .sort((a, z) => z.b - a.b)
    .slice(0, 20);
  if (!rows.length) return null;
  return (
    <SectorChartCard
      title={`${v.short}, ${y0} and ${y1}`}
      vars={[{ var: v.id, item: cell.slug, year: y0 }, { var: v.id, item: cell.slug, year: y1 }]}
      table={() => <TableView rows={rows} caption={`${v.short}, ${y0} and ${y1}`} cols={[{ id: 'e', header: 'Economy', value: (r) => r.label }, { id: 'a', header: `${y0} (${u.short})`, value: (r) => r.a, cell: (r) => fmtValue(r.a, u, { ctx: 'table', scale: v.display_scale }), num: true }, { id: 'b', header: `${y1} (${u.short})`, value: (r) => r.b, cell: (r) => fmtValue(r.b, u, { ctx: 'table', scale: v.display_scale }), num: true }]} />}
      csv={() => ({ name: `${cell.slug}-${v.id}-${y0}-${y1}`, text: sectorCsv(model, rows.flatMap((r) => { const g = model.geoByCode.get(r.id) ?? null; return [{ geo: g, v, item: cell, year: y0, value: Number.isFinite(r.a) ? r.a : null, status: 'obs' }, { geo: g, v, item: cell, year: y1, value: r.b, status: 'obs' }]; })) })}
      summary={`${v.short} of the top ${rows.length} economies in ${y0} and ${y1}.`}
    >
      <Dumbbell rows={rows} xLabel={u.long || u.short} aName={String(y0)} bName={String(y1)} ariaLabel={`${v.short} ${y0} and ${y1}`} selected={highlight?.code ?? null} />
    </SectorChartCard>
  );
}

function EvProjection({ v }: { v: VarInfo }) {
  const { model, cell, facts } = useProfile();
  if (!facts || v.projection_from === null) return null;
  const ys = facts.yearsOf(v.idx, cell.idx).filter((y) => y >= (v.projection_from as number));
  if (!ys.length) return null;
  const y = ys[ys.length - 1];
  const u = model.unitOf(v, cell.idx);
  const rows = facts.rows(v.idx, cell.idx, y).filter((r) => Number.isFinite(r.value)).sort((a, z) => z.value - a.value);
  return (
    <SectorChartCard title={`${v.short}: ${y} projection (STEPS)`} vars={[{ var: v.id, item: cell.slug, year: y }]} csv={() => ({ name: `${cell.slug}-${v.id}-${y}-projection`, text: sectorCsv(model, rows.map((r) => ({ geo: model.geos[r.geo], v, item: cell, year: y, value: r.value, status: 'projection' }))) })} summary={`IEA projects ${lc(v.short)} for ${rows.length} countries in ${y}.`}>
      <TableView rows={rows} caption={`${v.short} ${y} projection`} cols={[{ id: 'c', header: 'Economy', value: (r) => model.geos[r.geo]?.name ?? '' }, { id: 'v', header: `${y} projection (${u.short})`, value: (r) => r.value, cell: (r) => fmtValue(r.value, u, { ctx: 'table', scale: v.display_scale }), num: true }]} />
      <p className="mt-2 text-xs text-paper-mute">Projection (STEPS): outlined in charts and labelled; IEA gives it for these countries only.</p>
    </SectorChartCard>
  );
}

export function DemandSection() {
  const { model, cell, facts, eu } = useProfile();
  const { link } = useSectorUrl();
  const pc = vOf(model, 'consumption.per_capita');
  const ch = vOf(model, 'consumption.pc_change');
  const tot = vOf(model, 'consumption.total_ej');
  const yearOf = (v: VarInfo, it: ItemInfo) => baseYear(model, facts, v, it, eu);
  return (
    <div className="space-y-6">
      {fuelsOf(model, cell).map((f) => (
        <div key={f.slug} className="space-y-6">
          {pc && (() => {
            const y = yearOf(pc, f);
            return y === null ? null : <SliceLeague v={pc} item={f} year={y} title={`${f.label}: consumption per person, ${y}`} notes={<p>Six fuels per person, not a full energy mix.</p>} />;
          })()}
          {ch && model.availOf(ch, f.idx).length > 0 && (() => {
            const y = yearOf(ch, f);
            return y === null ? null : <SliceLeague v={ch} item={f} year={y} title={`${f.label}: change in consumption per person`} diverging={{ negLabel: '← lower', posLabel: 'higher →' }} notes={<p>"new" where the base year is zero; "n/a" where both years are zero.</p>} />;
          })()}
          {tot && <SeriesBlock v={tot} item={f} title={`${f.label} consumption`} />}
          <WorldOutlook fuel={f} />
        </div>
      ))}
      {EV_VARS.map((id) => {
        const v = vOf(model, id);
        if (!v || v.item_axis === 'none') return null;
        const it = itemForCell(model, v, cell);
        if (!it) return null;
        const y = yearOf(v, it);
        return y === null ? null : (
          <div key={id} className="space-y-6">
            <SliceLeague v={v} item={it} year={y} title={`${v.label}, ${y}`} />
            {id === 'consumption.ev_sales' && <EvGrowth v={v} />}
            {id === 'consumption.ev_sales' && <EvProjection v={v} />}
          </div>
        );
      })}
      {cell.stageSlug === 'midstream' && cell.has_trade && (
        <p className="text-sm text-paper-dim">
          Who buys: the importers of this basket are in{' '}
          <a className="link" href={`${link(`/sector/${cell.techSlug}/${cell.stageSlug}`)}#trade`}>
            Trade
          </a>
          .
        </p>
      )}
    </div>
  );
}
