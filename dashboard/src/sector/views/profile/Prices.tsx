// Profile S9 Prices & costs (spec_sector.md 4.3): the price board of the cell's commodities (latest in
// the corrected unit, year-on-year change, 1/5/10/20-year volatility as four dots on one hue's ordinal
// ramp, "used in" tags) and the IMF monthly prices 2003-2025 as small multiples (one panel per
// commodity, since units differ). World prices: the same for every country.

import { useMemo } from 'react';
import { Link } from 'react-router';
import * as Plot from '@observablehq/plot';
import { TableView } from '../../../components/TableView.tsx';
import { NEUTRAL_SERIES, VOLATILITY_ORDINAL } from '../../../theme/palette.ts';
import { monthlyOf } from '../../data/facts.ts';
import type { ItemInfo, SectorModel } from '../../data/model.ts';
import { monthlyYoy } from '../../derive/series.ts';
import { sectorCsv } from '../../derive/csv.ts';
import { fmtGrowth, fmtShare, fmtValue } from '../../format.ts';
import { ChunkGate } from '../common.tsx';
import { useFacts, useSeries } from '../data.ts';
import { useModel } from '../../data/SectorDataProvider.tsx';
import { SectorChartCard, SmallMultiples } from '../kit.tsx';
import { useSectorUrl } from '../state.tsx';
import { linkedWith, useProfile, vOf } from './ctx.tsx';

export function commoditiesOf(model: SectorModel, cell: ItemInfo): ItemInfo[] {
  const v = vOf(model, 'prices.latest');
  return v ? linkedWith(model, cell, 'commodity', v) : [];
}

export function hasPrices(model: SectorModel, cell: ItemInfo): boolean {
  return commoditiesOf(model, cell).length > 0;
}

const VOLS = ['prices.vol_1y', 'prices.vol_5y', 'prices.vol_10y', 'prices.vol_20y'];
const VOL_WORDS = ['1 year', '5 years', '10 years', '20 years'];

export interface BoardRow {
  item: ItemInfo;
  latest: number;
  yoy: number;
  vols: number[];
  usedIn: ItemInfo[];
}

/** The board rows of a set of commodities (world values on the WLD row). */
export function boardRows(model: SectorModel, facts: import('../../data/facts.ts').FactsIndex, items: ItemInfo[]): BoardRow[] {
  const wld = model.world?.idx ?? -1;
  const val = (id: string, it: ItemInfo) => {
    const v = vOf(model, id);
    if (!v) return NaN;
    const y = facts.latestYear(v.idx, it.idx);
    return y === null ? NaN : (facts.value(v.idx, it.idx, y, wld)?.value ?? NaN);
  };
  return items.map((it) => ({ item: it, latest: val('prices.latest', it), yoy: val('prices.yoy', it), vols: VOLS.map((id) => val(id, it)), usedIn: model.cellsOfItem(it.idx).map((c) => c.cell) }));
}

export function PriceBoard({ items, title }: { items: ItemInfo[]; title: string }) {
  const { model, facts } = useProfileOrModel();
  const { link } = useSectorUrl();
  const vLatest = vOf(model, 'prices.latest');
  const rows = useMemo(() => (facts ? boardRows(model, facts, items) : []), [model, facts, items]);
  if (!vLatest || !rows.length) return null;
  const yoyV = vOf(model, 'prices.yoy');
  const pctOfYoy = (x: number) => (yoyV?.unit.unit_kind === 'pct' ? x / 100 : x);
  return (
    <SectorChartCard
      title={title}
      vars={[{ var: 'prices.latest', item: items[0]?.slug ?? null }, ...VOLS.map((id) => ({ var: id, item: items[0]?.slug ?? null }))]}
      csv={() => ({
        name: 'price-board',
        text: sectorCsv(model, rows.flatMap((r) => [
          { geo: model.world, v: vLatest, item: r.item, year: null, value: r.latest, status: 'obs', unit: model.unitOf(vLatest, r.item.idx) },
          ...(yoyV ? [{ geo: model.world, v: yoyV, item: r.item, year: null, value: r.yoy, status: 'obs' }] : []),
          ...VOLS.map((id, i) => ({ geo: model.world, v: vOf(model, id)!, item: r.item, year: null, value: r.vols[i], status: 'obs' })).filter((x) => x.v),
        ])),
      })}
      notes={
        <>
          <p>World prices: the same for every country. Volatility is the annualised standard deviation of monthly log returns.</p>
          {rows.some((r) => /coal/i.test(r.item.slug)) && <p>Coal: mean of the Australian and South African benchmarks.</p>}
          <ul className="mt-1 flex flex-wrap gap-x-3" aria-label="Volatility windows">
            {VOL_WORDS.map((w, i) => (
              <li key={w} className="inline-flex items-center gap-1">
                <span className="inline-block h-2.5 w-2.5 rounded-full" style={{ background: VOLATILITY_ORDINAL[i] }} aria-hidden="true" />
                {w}
              </li>
            ))}
          </ul>
        </>
      }
      summary={rows.map((r) => `${r.item.label} ${fmtValue(r.latest, model.unitOf(vLatest, r.item.idx))}`).join('; ')}
    >
      <TableView
        rows={rows}
        caption="Price board"
        cols={[
          { id: 'c', header: 'Commodity', value: (r) => r.item.label, minWidth: 140 },
          { id: 'p', header: 'Latest (unit)', value: (r) => r.latest, cell: (r) => fmtValue(r.latest, model.unitOf(vLatest, r.item.idx), { scale: vLatest.display_scale }), num: true },
          { id: 'y', header: 'Year on year', value: (r) => r.yoy, cell: (r) => fmtGrowth(pctOfYoy(r.yoy)), num: true },
          {
            id: 'v',
            header: 'Volatility 1/5/10/20 y',
            value: (r) => r.vols[1],
            cell: (r) => (
              <span className="inline-flex items-center gap-2" title={r.vols.map((x, i) => `${VOL_WORDS[i]}: ${fmtShare(x)}`).join(' · ')}>
                {r.vols.map((x, i) => (
                  <span key={i} className="inline-flex items-center gap-0.5 text-xs">
                    <span className="inline-block h-2.5 w-2.5 rounded-full" style={{ background: VOLATILITY_ORDINAL[i] }} aria-hidden="true" />
                    <span className="tabular">{fmtShare(x, 0)}</span>
                  </span>
                ))}
              </span>
            ),
          },
          {
            id: 'u',
            header: 'Used in',
            value: (r) => r.usedIn.map((c) => model.cellLabel(c)).join(', '),
            cell: (r) => (
              <span className="flex flex-wrap gap-1">
                {r.usedIn.map((c) => (
                  <Link key={c.slug} className="tag" to={link(`/sector/${c.techSlug}/${c.stageSlug}`)}>
                    {model.cellLabel(c)}
                  </Link>
                ))}
              </span>
            ),
          },
        ]}
      />
    </SectorChartCard>
  );
}

/** Monthly prices, one panel per commodity (units differ), neutral line, the latest value with YoY. */
export function MonthlyPrices({ items }: { items: ItemInfo[] }) {
  const { model } = useProfileOrModel();
  const v = vOf(model, 'prices.monthly');
  const has = !!v && items.some((it) => model.availOf(v, it.idx).some((a) => a.chunk === 'series'));
  const { h } = useSeries(has);
  if (!v || !has) return null;
  return (
    <ChunkGate h={h} name="sector-series">
      {(chunk) => {
        const panels = items
          .map((it) => {
            const pts = monthlyOf(chunk, v.idx, it.idx).filter((p) => Number.isFinite(p.value));
            const u = model.unitOf(v, it.idx);
            const yoy = monthlyYoy(pts);
            const last = pts[pts.length - 1];
            return { id: it.slug, title: it.label, sub: last ? `${fmtValue(last.value, u, { scale: v.display_scale })} (${last.year}-${String(last.month).padStart(2, '0')})${yoy !== null ? ` · ${fmtGrowth(yoy)} y/y` : ''}` : undefined, rows: pts, empty: pts.length ? undefined : 'no monthly series', u };
          })
          .filter((p) => p.rows.length || p.empty);
        const all = panels.flatMap((p) => p.rows);
        if (!all.length) return null;
        const win: [number, number] = [Math.min(...all.map((p) => p.year)), Math.max(...all.map((p) => p.year))];
        return (
          <SectorChartCard
            title={`Monthly prices, ${win[0]}-${win[1]}`}
            vars={[{ var: v.id, item: items[0].slug, window: win }]}
            table={() => {
              const months = [...new Set(all.map((p) => `${p.year}-${String(p.month).padStart(2, '0')}`))].sort().reverse();
              return <TableView rows={months} caption="Monthly prices" pageSize={24} cols={[{ id: 'm', header: 'Month', value: (m) => m }, ...panels.map((p) => ({ id: p.id, header: `${p.title} (${p.u.short})`, value: (m: string) => p.rows.find((r) => `${r.year}-${String(r.month).padStart(2, '0')}` === m)?.value ?? null, num: true }))]} />;
            }}
            csv={() => ({ name: 'monthly-prices', text: sectorCsv(model, panels.flatMap((p) => p.rows.map((r) => ({ geo: model.world, v, item: items.find((i) => i.slug === p.id) ?? null, year: `${r.year}-${String(r.month).padStart(2, '0')}`, value: r.value, status: r.status, unit: p.u })))) })}
            notes={<p>One panel per commodity because units differ; each panel has its own scale.</p>}
            summary={panels.map((p) => `${p.title}: ${p.sub ?? 'no data'}`).join('; ')}
          >
            <SmallMultiples
              panels={panels}
              shared={false}
              yValue={(d) => d.value}
              height={130}
              ariaLabel={(p) => `${p.title}: monthly price`}
              tooltip={(d) => `${d.year}-${String(d.month).padStart(2, '0')}: ${d.value.toLocaleString('en-US')}`}
              render={(panel, ctx) => ({
                x: { label: null, ticks: 4, tickFormat: (d: number) => String(Math.floor(d)) },
                y: { label: null, ticks: 3, grid: true, domain: ctx.yDomain ?? undefined },
                marks: [Plot.line(panel.rows, { x: 't', y: 'value', stroke: NEUTRAL_SERIES, strokeWidth: 1.5 })],
              })}
            />
          </SectorChartCard>
        );
      }}
    </ChunkGate>
  );
}

/** Model and facts wherever the board is shown (profile, mineral pages, explore). */
function useProfileOrModel() {
  const model = useModel();
  const { facts } = useFacts();
  return { model, facts };
}

export function PricesSection() {
  const { model, cell } = useProfile();
  const items = commoditiesOf(model, cell);
  return (
    <div className="space-y-6">
      <PriceBoard items={items} title="Price board" />
      <MonthlyPrices items={items} />
      <p className="text-sm text-paper-mute">Levelised costs and overcapacity data are licensed (BNEF) and not part of the public explorer.</p>
    </div>
  );
}
