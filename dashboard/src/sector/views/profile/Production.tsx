// Profile S5 Production & capacity (spec_sector.md 4.3), by the kind of cell:
//   * fuels at Extraction and power at Deployment: the share split and league of EI 2025; fuels:
//     production vs consumption pairs; the EI series 1965-2025 (top 6 + rest of world, 10 y | 25 y | All)
//     and the shares of world production by group (sector-series);
//   * Nuclear Extraction: uranium by group over the years, and the producers;
//   * manufacturing at Components and Assembly: IEA capacity shares by entity, now vs 2035 (projection;
//     the IEA gives one figure for the two stages, shown on both with a "same figure as" note); CIM
//     capacity by product (native units, never summed across products);
//   * EV Assembly: IEA regions, production and sales, 2021-2024, and the net-export ratio;
//   * clean technologies at Extraction: mining of the minerals the technology uses; at Refining: their
//     refining (design C3).

import { useMemo, useState } from 'react';
import { useNavigate } from 'react-router';
import { TableView } from '../../../components/TableView.tsx';
import { Term } from '../../../components/Term.tsx';
import { Segmented, Select } from '../../../components/ui.tsx';
import { rowsToCsv } from '../../../lib/csv.ts';
import { GROUP_ORDINAL } from '../../../theme/palette.ts';
import type { FactsIndex } from '../../data/facts.ts';
import type { GeoInfo, ItemInfo, SectorModel, VarInfo } from '../../data/model.ts';
import { MINERAL_STEP } from '../../derive/minerals.ts';
import { GROUP_SHORT, partOf } from '../../derive/groups.ts';
import { groupArea, normalizeArea, topLines, windowYears } from '../../derive/series.ts';
import { slice } from '../../derive/summary.ts';
import { sameFigureText } from '../../derive/pairs.ts';
import { sectorCsv } from '../../derive/csv.ts';
import { axisFormatter, fmtShare, fmtValue } from '../../format.ts';
import { ASSUMED_MARK, ASSUMED_ZERO_WORDS, assumedNote, isAssumedZero, noteLine } from '../../derive/notes.ts';
import { LeagueCard } from '../cards.tsx';
import { ChunkGate, HowToRead, markGroup, measureConc, SPLIT_COLUMNS, SPLIT_READING, shareSplitRow, SplitTable, useHighlight } from '../common.tsx';
import { useModel } from '../../data/SectorDataProvider.tsx';
import { useSeries } from '../data.ts';
import { Dumbbell, GroupArea, GroupLegend, LabelledLines, SectorChartCard, ShareSplitChart, type DumbbellRow, type ShareSplitRow } from '../kit.tsx';
import { useSectorUrl } from '../state.tsx';
import { baseYear } from '../TechChainView.tsx';
import { linkedWith, useProfile, vOf } from './ctx.tsx';

const PRODUCTION_FAMILIES = new Set(['production', 'capacity']);

/** Country-level production vars of the cell itself (generation, uranium, ...). */
function ownVars(model: SectorModel, cell: ItemInfo): VarInfo[] {
  return model.vars.filter((v) => PRODUCTION_FAMILIES.has(v.family) && v.native_level === 'country' && model.availOf(v, cell.idx).some((a) => a.chunk === 'facts'));
}

export function hasProduction(model: SectorModel, cell: ItemInfo): boolean {
  if (ownVars(model, cell).length) return true;
  if (model.vars.some((v) => PRODUCTION_FAMILIES.has(v.family) && (v.native_level === 'entity' || v.native_level === 'region') && model.availOf(v, cell.idx).length)) return true;
  const cap = vOf(model, 'manufacturing.capacity');
  if (cap && model.availOf(cap, cell.idx).length) return true;
  const fuels = vOf(model, 'production.fuels');
  if (cell.stageSlug === 'extraction' && fuels && linkedWith(model, cell, 'fuel', fuels).length) return true;
  const step = cell.stageSlug ? MINERAL_STEP[cell.stageSlug] : undefined;
  if (step) {
    const v = vOf(model, step.id);
    if (v && linkedWith(model, cell, 'mineral', v).length) return true;
  }
  return false;
}

/** EI-style series: the top producers' lines (+ rest of world) with a range, and the group shares of world production. */
export function SeriesBlock({ v, item, title }: { v: VarInfo; item: ItemInfo; title: string }) {
  const model = useModel();
  const highlight = useHighlight(model);
  const has = model.availOf(v, item.idx).some((a) => a.chunk === 'series');
  const { series, h } = useSeries(has);
  const [range, setRange] = useState<'10' | '25' | 'all'>('25');
  if (!has) return null;
  return (
    <ChunkGate h={h} name="sector-series">
      {() => {
        if (!series) return null;
        const all = series.yearsOf(v.idx, item.idx);
        if (!all.length) return null;
        const years = windowYears(all, range === 'all' ? 'all' : Number(range));
        const lines = topLines(series, model, v.idx, item.idx, { n: 6, years, extra: highlight ? [highlight.idx] : [], rest: false });
        const area = normalizeArea(groupArea(series, model, v.idx, item.idx, years));
        const unit = model.unitOf(v, item.idx);
        const fmt = (x: number) => fmtValue(x, unit, { scale: v.display_scale });
        const win: [number, number] = [years[0], years[years.length - 1]];
        const csv = () => ({
          name: `${v.id}-${item.slug}-series`,
          text: sectorCsv(
            model,
            lines.flatMap((l) => l.points.map((p) => ({ geo: l.geo !== null ? model.geos[l.geo] : null, economy: l.geo === null ? l.label : undefined, v, item, year: p.x, value: p.y, status: p.est ? 'est' : 'obs' }))),
          ),
        });
        return (
          <div className="space-y-6">
            <SectorChartCard
              title={`${title}: the largest economies, ${win[0]}-${win[1]}`}
              vars={[{ var: v.id, item: item.slug, window: win }]}
              subExtra={<HowToRead>Each line is one of the six largest economies in the last year, plus the one you highlighted, year by year; the higher the line, the larger the amount. Its colour shows the group: China, the coalition core, coalition partners or the rest of the world.</HowToRead>}
              tools={<Segmented label="Range" hideLabel value={range} options={[{ value: '10', label: '10 y' }, { value: '25', label: '25 y' }, { value: 'all', label: 'All' }]} onChange={(r) => setRange(r)} />}
              table={() => (
                <TableView rows={years} caption={`${title} by year`} cols={[{ id: 'y', header: 'Year', value: (y) => y }, ...lines.map((l) => ({ id: l.id, header: l.label, value: (y: number) => l.points.find((p) => p.x === y)?.y ?? null, cell: (y: number) => fmt(l.points.find((p) => p.x === y)?.y ?? NaN), num: true }))]} />
              )}
              csv={csv}
              legend={<GroupLegend groups={['china', 'core', 'partner', 'rest']} highlight={highlight?.name ?? null} />}
              summary={lines[0] ? `${lines[0].label} is the largest in ${win[1]}.` : 'No series.'}
            >
              <LabelledLines lines={lines} highlight={highlight?.code ?? null} yLabel={unit.long || unit.short} ariaLabel={`${title} of the largest economies`} valueFormat={fmt} yTickFormat={axisFormatter(lines.flatMap((l) => l.points.map((p) => p.y)), unit, v.display_scale)} />
            </SectorChartCard>
            <SectorChartCard
              title={`${title}: shares of the world total by group`}
              vars={[{ var: v.id, item: item.slug, window: win }]}
              subExtra={<HowToRead>Each band is one group’s share of the world total, year by year: China, the coalition core, coalition partners, the rest of the world and the part the source does not assign to any economy. The bands always add up to 100%, so a band that widens is gaining ground on the others.</HowToRead>}
              table={() => (
                <TableView rows={years} caption="Shares by group" cols={[{ id: 'y', header: 'Year', value: (y) => y }, ...(['china', 'core', 'partner', 'rest', 'unattributed'] as const).map((k) => ({ id: k, header: GROUP_SHORT[k], value: (y: number) => area.find((p) => p.x === y && p.group === k)?.value ?? null, cell: (y: number) => fmtShare(area.find((p) => p.x === y && p.group === k)?.value), num: true }))]} />
              )}
              csv={() => ({ name: `${v.id}-${item.slug}-group-shares`, text: rowsToCsv(years.map((y) => ({ year: y, var_id: v.id, item: item.slug, ...Object.fromEntries(area.filter((p) => p.x === y).map((p) => [`share_${p.group}`, p.value])), source: v.source.short }))) })}
              legend={<GroupLegend />}
              notes={<p>The not-attributed band is the source's own residual (for EI, "Other").</p>}
              summary={`Shares of world ${title.toLowerCase()} by group, ${win[0]}-${win[1]}.`}
            >
              <GroupArea series={area} normalize yLabel="Share of the world total" ariaLabel={`Shares of world ${title.toLowerCase()} by group`} yTickFormat={(x) => `${Math.round(x * 100)}%`} valueFormat={(x) => fmtShare(x)} />
            </SectorChartCard>
          </div>
        );
      }}
    </ChunkGate>
  );
}

/** A league of a slice in the profile's EU mode. */
export function SliceLeague({ v, item, year, title, diverging, notes }: { v: VarInfo; item: ItemInfo; year: number; title: string; diverging?: { negLabel: string; posLabel: string }; notes?: React.ReactNode }) {
  const { model, facts, highlight, eu } = useProfile();
  const sl = useMemo(() => (facts ? slice(model, facts, v, item.idx, year, eu) : null), [facts, model, v, item, year, eu]);
  if (!sl) return null;
  return <LeagueCard model={model} v={v} item={item} year={year} rows={sl.rows} world={sl.world} title={title} highlight={highlight} map={v.native_level === 'country'} diverging={diverging} notes={notes} />;
}

/** Production vs consumption pairs of the 25 largest consumers (fuels), when the units match. */
function ProdVsCons({ fuel, facts }: { fuel: ItemInfo; facts: FactsIndex }) {
  const { model, highlight } = useProfile();
  const vp = vOf(model, 'production.fuels');
  const vb = vOf(model, 'consumption.balance');
  const vs = vOf(model, 'consumption.self_sufficiency');
  if (!vp || !vb) return null;
  const up = model.unitOf(vp, fuel.idx);
  const ub = model.unitOf(vb, fuel.idx);
  if (up.id !== ub.id || vp.display_scale !== vb.display_scale) return null;
  const yp = facts.latestYear(vp.idx, fuel.idx);
  const yb = facts.latestYear(vb.idx, fuel.idx);
  if (yp === null || yb === null || yp !== yb) return null;
  const rows: (DumbbellRow & { bal: number; ss: number | null; assumed: boolean; note: string; pStatus: string; bStatus: string })[] = [];
  for (const b of facts.rows(vb.idx, fuel.idx, yb)) {
    const g = model.geos[b.geo];
    if (!g || g.kind !== 'country' || !Number.isFinite(b.value)) continue;
    const p = facts.value(vp.idx, fuel.idx, yp, b.geo);
    // production that no source reports separately counts as 0 (user rule 2026-10-11): drawn, marked
    const assumed = isAssumedZero(vb.id, b) || !p || !Number.isFinite(p.value);
    const prod = p && Number.isFinite(p.value) ? p.value : 0;
    const cons = prod - b.value;
    const ss = vs ? facts.value(vs.idx, fuel.idx, yb, b.geo) : null;
    const prodWords = `production ${fmtValue(prod, up)}${assumed ? ` ${ASSUMED_MARK} (assumed: not reported separately)` : ''}`;
    rows.push({ id: g.code, label: g.name, a: prod, b: cons, group: markGroup(g), aLabel: prodWords, bLabel: `consumption ${fmtValue(cons, up)}${ss && Number.isFinite(ss.value) ? ` · self-sufficiency ${ss.value > 300 ? `>300% (${ss.value.toFixed(0)}%)` : `${ss.value.toFixed(0)}%`}` : ''}`, bal: b.value, ss: ss?.value ?? null, assumed, note: [noteLine(model, vb.id, b), assumed && !noteLine(model, vb.id, b) ? ASSUMED_ZERO_WORDS : ''].filter(Boolean).join('; '), pStatus: p?.status ?? 'nsr', bStatus: b.status });
  }
  const top = rows.sort((a, z) => z.b - a.b).slice(0, 25).sort((a, z) => z.bal - a.bal);
  if (!top.length) return null;
  return (
    <SectorChartCard
      title={`${fuel.label}: production and consumption of the 25 largest consumers`}
      vars={[{ var: vp.id, item: fuel.slug, year: yp }, { var: vb.id, item: fuel.slug, year: yb }]}
      subExtra={
        <HowToRead>
          The <Term id="net_balance">net balance</Term> is production minus consumption: below zero, the economy imports the difference. <Term id="self_sufficiency">Self-sufficiency</Term> is production as a percentage of consumption. Where no source reports an economy&apos;s production separately, it counts as 0 and is marked {ASSUMED_MARK}.
        </HowToRead>
      }
      table={() => (
        <TableView rows={top} caption="Production and consumption" cols={[
          { id: 'c', header: 'Economy', value: (r) => r.label },
          { id: 'p', header: `Production (${up.short})`, value: (r) => r.a, num: true },
          { id: 'q', header: `Consumption (${up.short})`, value: (r) => r.b, num: true },
          { id: 'b', header: `Net balance (${up.short})`, value: (r) => r.bal, num: true },
          { id: 's', header: 'Self-sufficiency (%)', value: (r) => r.ss, num: true },
          { id: 'n', header: 'Source note', value: (r) => r.note || null },
        ]} />
      )}
      csv={() => ({ name: `${fuel.slug}-production-consumption`, text: sectorCsv(model, top.flatMap((r) => { const g = model.geoByCode.get(r.id) ?? null; return [{ geo: g, v: vp, item: fuel, year: yp, value: r.a, status: r.assumed ? 'nsr' : r.pStatus, note: r.assumed ? ASSUMED_ZERO_WORDS : null }, { geo: g, v: vb, item: fuel, year: yb, value: r.bal, status: r.bStatus, note: r.note || null }]; })) })}
      notes={
        <>
          <p>Each row is one of the 25 largest consumers: the hollow dot is what it produces, the filled dot what it uses. A filled dot to the right of the hollow one means it uses more than it produces and imports the difference. Rows are sorted by net balance (production minus consumption), net exporters first. Self-sufficiency is production as a percent of consumption (100 = balanced).</p>
          {top.some((r) => r.assumed) && <p>{assumedNote(vb.id, top.filter((r) => r.assumed).length)}</p>}
        </>
      }
      summary={`${top[0].label} has the largest net surplus; ${top[top.length - 1].label} the largest net deficit.`}
    >
      <Dumbbell rows={top} xLabel={up.long || up.short} aName="Production" bName="Consumption" ariaLabel="Production and consumption" selected={highlight?.code ?? null} />
    </SectorChartCard>
  );
}

function EntityShares() {
  const { cell, facts } = useProfile();
  return <EntitySharesCard cell={cell} facts={facts} />;
}

/** IEA entity shares of manufacturing capacity, current vs announced 2035 (profile S5, Explore entity layout). */
export function EntitySharesCard({ cell, facts }: { cell: ItemInfo; facts: FactsIndex | null }) {
  const model = useModel();
  const vNow = vOf(model, 'manufacturing.share_now');
  const v35 = vOf(model, 'manufacturing.share_2035');
  if (!facts || !vNow || !model.availOf(vNow, cell.idx).length) return null;
  const yN = facts.latestYear(vNow.idx, cell.idx);
  const y35 = v35 ? facts.latestYear(v35.idx, cell.idx) : null;
  if (yN === null) return null;
  const now = facts.rows(vNow.idx, cell.idx, yN);
  const later = v35 && y35 !== null ? facts.rows(v35.idx, cell.idx, y35) : [];
  const geos = [...new Set([...now, ...later].map((r) => r.geo))];
  const rows: DumbbellRow[] = geos.map((g) => {
    const gi = model.geos[g];
    const a = later.find((r) => r.geo === g);
    const b = now.find((r) => r.geo === g);
    // IEA's 0 means "under 1% or not reported" (status nq): words, and no dot at 0
    const noFigure = (r: typeof a) => !!r && (r.status === 'nq' || !Number.isFinite(r.value) || (r.value === 0 && gi.kind !== 'residual'));
    const word = (r: typeof a) => (!r ? 'not reported' : noFigure(r) ? '<1% or not reported' : fmtShare(r.value));
    const val = (r: typeof a) => (!r || noFigure(r) ? NaN : r.value);
    return { id: gi.code, label: gi.kind === 'residual' ? 'Other' : gi.name, a: val(a), b: val(b), group: gi.kind === 'residual' ? ('neutral' as const) : markGroup(gi), aLabel: `${word(a)} (${y35 ?? '2035'}, projection)`, bLabel: `${word(b)} (current)` };
  }).sort((a, z) => (Number.isFinite(z.b) ? z.b : -1) - (Number.isFinite(a.b) ? a.b : -1));
  // the IEA gives one figure for the technology's manufacturing: Components and Assembly both carry it (design C2)
  const same = sameFigureText(model, facts, vNow, cell, yN);
  return (
    <SectorChartCard
      title="Share of global manufacturing capacity: current and announced 2035"
      vars={[{ var: vNow.id, item: cell.slug, year: yN }, ...(v35 && y35 !== null ? [{ var: v35.id, item: cell.slug, year: y35 }] : [])]}
      subExtra={<HowToRead>Each row is one economy or the EU as the IEA reports it; the filled dot is its share of world factory capacity today, the hollow dot its share in 2035 if every announced plant is built (a projection, not a forecast). A line running to the right means its share would grow.</HowToRead>}
      table={() => <TableView rows={rows} caption="Manufacturing capacity shares" cols={[{ id: 'e', header: 'Entity', value: (r) => r.label }, { id: 'b', header: 'Current', value: (r) => r.bLabel }, { id: 'a', header: '2035 (projection)', value: (r) => r.aLabel }]} />}
      csv={() => ({ name: `${cell.slug}-capacity-shares`, text: sectorCsv(model, [...now.map((r) => ({ geo: null, economy: model.geos[r.geo].name, v: vNow, item: cell, year: yN, value: Number.isFinite(r.value) ? r.value : null, status: r.status, share: r.value })), ...(v35 ? later.map((r) => ({ geo: null, economy: model.geos[r.geo].name, v: v35, item: cell, year: y35, value: Number.isFinite(r.value) ? r.value : null, status: r.status, share: r.value })) : [])]) })}
      legend={<GroupLegend groups={['china', 'core', 'partner']} projection />}
      notes={
        <>
          <p>The IEA reports these shares for a few economies and the EU, not for every country. Where it gives 0, the share is under 1% or not reported, so the chart shows "&lt;1% or not reported" and no dot. Labels name every row because some colours repeat.</p>
          {same && <p data-testid="same-figure-note">{same}</p>}
        </>
      }
      summary={rows[0] ? `${rows[0].label} holds ${fmtShare(rows[0].b)} of current capacity and ${Number.isFinite(rows[0].a) ? fmtShare(rows[0].a) : 'an unreported share'} of announced 2035 capacity.` : 'No entity shares.'}
    >
      <Dumbbell rows={rows} xLabel="Share of global capacity" aName="2035 announced (projection)" bName="Current" ariaLabel="Manufacturing capacity shares by entity" tickFormat={(x) => `${Math.round(x * 100)}%`} />
    </SectorChartCard>
  );
}

const CAP_STATUSES = ['operational', 'construction', 'announced', 'paused'] as const;
const CAP_WORDS: Record<string, string> = { operational: 'operational', construction: 'under construction', announced: 'announced', paused: 'paused' };

function CapacityByProduct() {
  const { cell, highlight } = useProfile();
  return <CapacityByProductCard cell={cell} highlight={highlight} />;
}

/** CIM manufacturing capacity by product and status (native units, never summed). */
export function CapacityByProductCard({ cell, highlight }: { cell: ItemInfo; highlight: GeoInfo | null }) {
  const model = useModel();
  const v = vOf(model, 'manufacturing.capacity');
  const has = !!v && model.availOf(v, cell.idx).length > 0;
  const { h } = useSeries(has);
  const [product, setProduct] = useState<string | null>(null);
  if (!v || !has) return null;
  return (
    <ChunkGate h={h} name="sector-series">
      {(chunk) => {
        const t = chunk['series.capacity'];
        if (!t) return <p className="text-sm text-paper-mute">Manufacturing capacity by product is not in this data build.</p>;
        type R = { geo: number; product: string; unit: string; year: number; st: string; value: number };
        const all: R[] = [];
        for (let i = 0; i < t.nrow; i++) if (t.cols.item[i] === cell.idx && Number.isFinite(t.cols.value[i])) all.push({ geo: t.cols.geo[i], product: t.cols.product[i] ?? '', unit: t.cols.unit[i] ?? '', year: t.cols.year[i], st: t.cols.cap_status[i] ?? '', value: t.cols.value[i] });
        const products = [...new Set(all.map((r) => r.product))].sort();
        const p = product && products.includes(product) ? product : products[0];
        const rows = all.filter((r) => r.product === p);
        const unit = rows[0]?.unit ?? '';
        const year = Math.max(...rows.map((r) => r.year));
        const byGeo = new Map<number, Record<string, number>>();
        for (const r of rows.filter((x) => x.year === year)) {
          const m = byGeo.get(r.geo) ?? {};
          m[r.st] = (m[r.st] ?? 0) + r.value;
          byGeo.set(r.geo, m);
        }
        const ranked = [...byGeo.entries()].map(([geo, m]) => ({ geo, m, total: CAP_STATUSES.reduce((a, s) => a + (m[s] ?? 0), 0) })).sort((a, z) => z.total - a.total);
        const top = ranked.slice(0, 15);
        if (highlight && !top.some((x) => x.geo === highlight.idx)) {
          const hr = ranked.find((x) => x.geo === highlight.idx);
          if (hr) top.push(hr);
        }
        const max = Math.max(1e-9, ...top.map((x) => x.total));
        return (
          <SectorChartCard
            title={`Manufacturing capacity by product: ${p}`}
            vars={[{ var: v.id, item: cell.slug, year }]}
            subExtra={<HowToRead>Each bar is one economy’s factory capacity for this product, largest first, split by the state of the plants: operational, under construction, announced and paused, from light to dark in the economy’s group colour. The longer the bar, the more capacity; pick another product above.</HowToRead>}
            tools={products.length > 1 ? <Select label="Product" value={p} options={products.map((x) => ({ value: x, label: x }))} onChange={setProduct} /> : undefined}
            table={() => (
              <TableView rows={ranked} caption="Capacity by status" cols={[
                { id: 'c', header: 'Economy', value: (r) => model.geos[r.geo].name },
                ...CAP_STATUSES.map((s) => ({ id: s, header: `${CAP_WORDS[s]} (${unit})`, value: (r: (typeof ranked)[number]) => r.m[s] ?? null, num: true })),
              ]} />
            )}
            csv={() => ({ name: `${cell.slug}-capacity-${p}`, text: rowsToCsv(ranked.flatMap((r) => CAP_STATUSES.filter((s) => r.m[s] !== undefined).map((s) => ({ iso3c: model.geos[r.geo].code, economy: model.geos[r.geo].name, group: model.geos[r.geo].group, var_id: v.id, product: p, status_of_capacity: s, year, value: r.m[s], unit, source: v.source.short, licence: v.licence })))) })}
            notes={<p>Units differ by product ({unit} here); products are never summed. Bars split operational | under construction | announced | paused, in steps of the economy's group hue.</p>}
            summary={top[0] ? `${model.geos[top[0].geo].name} has the most ${p} capacity: ${top[0].total.toLocaleString('en-US')} ${unit} across all statuses.` : 'No capacity recorded.'}
          >
            <ol className="space-y-1" aria-label={`${p} capacity by economy and status`}>
              {top.map((r) => {
                const g = model.geos[r.geo];
                const ramp = GROUP_ORDINAL[markGroup(g)];
                return (
                  <li key={r.geo} className="grid grid-cols-[minmax(6rem,10rem)_1fr] items-center gap-2 text-xs">
                    <span className={highlight?.idx === r.geo ? 'font-semibold text-paper' : 'text-paper-dim'}>{g.name}</span>
                    <span className="flex h-3.5 items-center" title={CAP_STATUSES.map((s) => `${CAP_WORDS[s]} ${(r.m[s] ?? 0).toLocaleString('en-US')} ${unit}`).join(' · ')}>
                      {CAP_STATUSES.map((s, i) => (r.m[s] ? <span key={s} className="h-full" style={{ width: `${(80 * r.m[s]) / max}%`, background: ramp[i], marginRight: 1 }} /> : null))}
                      <span className="ml-2 tabular text-paper-mute">
                        {r.total.toLocaleString('en-US')} {unit}
                      </span>
                    </span>
                  </li>
                );
              })}
            </ol>
            <ul className="mt-3 flex flex-wrap gap-x-4 text-xs text-paper-dim" aria-label="Capacity status">
              {CAP_STATUSES.map((s, i) => (
                <li key={s} className="inline-flex items-center gap-1.5">
                  <span className="inline-block h-2.5 w-3.5" style={{ background: GROUP_ORDINAL.core[i] }} aria-hidden="true" />
                  {CAP_WORDS[s]}
                </li>
              ))}
            </ul>
          </SectorChartCard>
        );
      }}
    </ChunkGate>
  );
}

function EvRegions() {
  const { cell, facts } = useProfile();
  return <EvRegionsCard cell={cell} facts={facts} />;
}

/** IEA EV regions: production, sales and the net-export ratio, and production 2021-2024 (series). */
export function EvRegionsCard({ cell, facts, highlightRegion }: { cell: ItemInfo; facts: FactsIndex | null; highlightRegion?: GeoInfo | null }) {
  const model = useModel();
  const vp = vOf(model, 'production.ev_region');
  const vs = vOf(model, 'production.ev_region_sales');
  const vr = vOf(model, 'manufacturing.ev_net_export_ratio');
  const hasSeries = !!vp && model.availOf(vp, cell.idx).some((a) => a.chunk === 'series');
  const { series, h } = useSeries(hasSeries);
  if (!facts || !vp || !model.availOf(vp, cell.idx).length) return null;
  const y = facts.latestYear(vp.idx, cell.idx);
  if (y === null) return null;
  const prod = facts.rows(vp.idx, cell.idx, y);
  const ys = vs ? facts.latestYear(vs.idx, cell.idx) : null;
  const sales = vs && ys !== null ? facts.rows(vs.idx, cell.idx, ys) : [];
  const yr = vr ? facts.latestYear(vr.idx, cell.idx) : null;
  const ratio = vr && yr !== null ? facts.rows(vr.idx, cell.idx, yr) : [];
  const regions = [...new Set([...prod, ...sales].map((r) => r.geo))];
  const unit = model.unitOf(vp, cell.idx);
  const rows = regions.map((g) => ({ g: model.geos[g], prod: prod.find((r) => r.geo === g)?.value ?? NaN, sales: sales.find((r) => r.geo === g)?.value ?? NaN, ratio: ratio.find((r) => r.geo === g)?.value ?? NaN })).sort((a, z) => (Number.isFinite(z.prod) ? z.prod : 0) - (Number.isFinite(a.prod) ? a.prod : 0));
  return (
    <div className="space-y-6">
      <SectorChartCard
        title={`Electric vehicles by IEA region, ${y}`}
        subExtra={<HowToRead>Each row is one region as the IEA reports it: how many electric vehicles it built and sold. The last number compares the two: above zero the region builds more than it buys and so exports on balance (+0.50 means half as many again as it sells); below zero it relies on imports.</HowToRead>}
        vars={[{ var: vp.id, item: cell.slug, year: y }, ...(vs && ys !== null ? [{ var: vs.id, item: cell.slug, year: ys }] : []), ...(vr && yr !== null ? [{ var: vr.id, item: cell.slug, year: yr }] : [])]}
        csv={() => ({ name: `${cell.slug}-ev-regions`, text: rowsToCsv(rows.map((r) => ({ region: r.g.name, code: r.g.code, year: y, production: Number.isFinite(r.prod) ? r.prod : null, sales: Number.isFinite(r.sales) ? r.sales : null, unit: unit.short, net_export_ratio: Number.isFinite(r.ratio) ? r.ratio : null, source: vp.source.short })) ) })}
        notes={<p>Reported for regions; countries in a region share its value, so countries are not ranked. Net-export ratio = (production − sales) / sales; positive = net exporter.</p>}
        summary={rows[0] ? `${rows[0].g.name} produced ${fmtValue(rows[0].prod, unit)} in ${y}.` : 'No regions.'}
      >
        <TableView
          rows={rows}
          caption="EV production and sales by region"
          rowKey={(r) => r.g.code}
          selectedKey={highlightRegion?.code ?? null}
          cols={[
            { id: 'r', header: 'Region', value: (r) => r.g.name, minWidth: 160 },
            { id: 'p', header: `Production (${unit.short})`, value: (r) => r.prod, cell: (r) => fmtValue(r.prod, unit, { ctx: 'table', scale: vp.display_scale }), num: true },
            { id: 's', header: `Sales (${unit.short})`, value: (r) => r.sales, cell: (r) => (vs ? fmtValue(r.sales, model.unitOf(vs, cell.idx), { ctx: 'table', scale: vs.display_scale }) : '–'), num: true },
            { id: 'x', header: '(production − sales) / sales', value: (r) => r.ratio, cell: (r) => (Number.isFinite(r.ratio) ? `${r.ratio >= 0 ? '+' : '−'}${Math.abs(r.ratio).toFixed(2)}` : '–'), num: true },
            { id: 'm', header: 'Members', value: (r) => (model.regionMembers.get(r.g.idx) ?? []).map((m) => model.geos[m]?.code).join(', ') || null },
          ]}
        />
      </SectorChartCard>
      {hasSeries && (
        <ChunkGate h={h} name="sector-series">
          {() => {
            if (!series) return null;
            const yrs = series.yearsOf(vp.idx, cell.idx);
            // coloured by the region's group (China rose, the EU core); a mixed region (North America, Other
            // Asia Pacific, Rest of World) is neutral, told apart by its dash pattern and label
            const lines = regions.map((g) => ({ id: model.geos[g].code, label: model.geos[g].name.replace(/ \(IEA EV region\)$/, ''), group: partOf(model.geos[g]) === 'unattributed' ? ('neutral' as const) : partOf(model.geos[g]), points: yrs.flatMap((yy) => { const f = series.value(vp.idx, cell.idx, yy, g); return f && Number.isFinite(f.value) ? [{ x: yy, y: f.value }] : []; }) })).filter((l) => l.points.length);
            return (
              <SectorChartCard
                title={`Electric-vehicle production by IEA region, ${yrs[0]}-${yrs[yrs.length - 1]}`}
                vars={[{ var: vp.id, item: cell.slug, window: [yrs[0], yrs[yrs.length - 1]] }]}
                subExtra={<HowToRead>Each line is one region, year by year; the higher the line, the more vehicles it built. Regions that mix groups (for example North America) are grey and told apart by their dash pattern and label.</HowToRead>}
                table={() => <TableView rows={yrs} caption="EV production by region and year" cols={[{ id: 'y', header: 'Year', value: (yy) => yy }, ...lines.map((l) => ({ id: l.id, header: `${l.label} (${unit.short})`, value: (yy: number) => l.points.find((p) => p.x === yy)?.y ?? null, num: true }))]} />}
                csv={() => ({ name: `${cell.slug}-ev-regions-series`, text: sectorCsv(model, lines.flatMap((l) => l.points.map((p) => ({ geo: null, economy: l.label, v: vp, item: cell, year: p.x, value: p.y, status: 'obs' })))) })}
                summary="Electric-vehicle production by region over time."
              >
                <LabelledLines lines={lines} highlight={highlightRegion?.code ?? null} yLabel={unit.long} ariaLabel="EV production by region" valueFormat={(x) => fmtValue(x, unit, { scale: vp.display_scale })} />
              </SectorChartCard>
            );
          }}
        </ChunkGate>
      )}
    </div>
  );
}

function MineralRows() {
  const { model, cell, facts, highlight, eu } = useProfile();
  const { link } = useSectorUrl();
  const navigate = useNavigate();
  const step = cell.stageSlug ? MINERAL_STEP[cell.stageSlug] : undefined;
  const rows: ShareSplitRow[] = [];
  const refs: { var: string; item: string; year: number }[] = [];
  const v = step ? vOf(model, step.id) : null;
  if (step && v) {
    for (const { item: m } of model.itemsOfCell(cell.idx, 'mineral')) {
      const y = baseYear(model, facts, v, m, eu);
      if (y === null) continue;
      const c = measureConc(model, facts, v, m, y, eu);
      if (!c || c.conc.W === null) continue;
      rows.push(shareSplitRow(model, v, m, c.conc, { id: `${m.slug}-${step.id}`, label: `${m.label}: ${step.words} ${y}`, year: y, highlight, href: link(`/sector/minerals/${m.slug}`) }));
      refs.push({ var: step.id, item: m.slug, year: y });
    }
  }
  if (!step || !rows.length) return null;
  const other = step.words === 'mining' ? 'refining, under Refining' : 'mining, under Extraction';
  return (
    <SectorChartCard
      title={`${step.words === 'mining' ? 'Mining' : 'Refining'} of the minerals ${cell.tech} uses`}
      vars={refs}
      subExtra={<HowToRead>{SPLIT_READING} One row per mineral: where it is {step.words === 'mining' ? 'mined' : 'refined'} (its {other}, is often somewhere else). Click a row for the mineral’s page.</HowToRead>}
      table={() => <SplitTable rows={rows} caption={`${step.words === 'mining' ? 'Mining' : 'Refining'} by mineral`} />}
      legend={<GroupLegend highlight={highlight?.name ?? null} />}
      csv={() => ({ name: `${cell.slug}-minerals`, text: rowsToCsv(rows.map((r) => ({ measure: r.label, ...Object.fromEntries(Object.entries(r.parts).map(([k, x]) => [`share_${k}`, x])), top1: r.cols.top1, top3: r.cols.top3, hhi: r.cols.hhi, source: r.cols.source }))) })}
      notes={<p>IEA lists the top six producers; the rest is one "Rest of world" band, so the <Term id="hhi">concentration index (HHI)</Term> is "at least".</p>}
      summary={rows.map((r) => r.ariaLabel).slice(0, 2).join(' ')}
    >
      <ShareSplitChart rows={rows} columns={SPLIT_COLUMNS} ariaLabel={`${step.words === 'mining' ? 'Mining' : 'Refining'} by mineral`} onRowClick={(r) => r.href && navigate(r.href)} highlightHeader={highlight?.name} />
    </SectorChartCard>
  );
}

function UraniumOverTime({ v }: { v: VarInfo }) {
  const { model, cell, facts } = useProfile();
  if (!facts) return null;
  const years = facts.yearsOf(v.idx, cell.idx);
  if (years.length < 2) return null;
  const area = groupArea(facts, model, v.idx, cell.idx, years);
  const unit = model.unitOf(v, cell.idx);
  return (
    <SectorChartCard
      title={`${v.short} by group, ${years[0]}-${years[years.length - 1]}`}
      vars={[{ var: v.id, item: cell.slug, window: [years[0], years[years.length - 1]] }]}
      subExtra={<HowToRead>Each band is one group: China, the coalition core, coalition partners, the rest of the world and the part the source does not assign to any economy. Stacked, they make the world total, so a band that widens is gaining.</HowToRead>}
      table={() => <TableView rows={years} caption={`${v.short} by group`} cols={[{ id: 'y', header: 'Year', value: (y) => y }, ...(['china', 'core', 'partner', 'rest', 'unattributed'] as const).map((k) => ({ id: k, header: `${GROUP_SHORT[k]} (${unit.short})`, value: (y: number) => area.find((p) => p.x === y && p.group === k)?.value ?? null, num: true }))]} />}
      csv={() => ({ name: `${cell.slug}-${v.id}-by-group`, text: rowsToCsv(years.map((y) => ({ year: y, var_id: v.id, item: cell.slug, unit: unit.short, ...Object.fromEntries(area.filter((p) => p.x === y).map((p) => [p.group, p.value])), source: v.source.short }))) })}
      legend={<GroupLegend />}
      summary={`${v.short} by group over time.`}
    >
      <GroupArea series={area} yLabel={unit.long} ariaLabel={`${v.short} by group`} valueFormat={(x) => fmtValue(x, unit, { scale: v.display_scale })} />
    </SectorChartCard>
  );
}

export function ProductionSection() {
  const { model, cell, facts, eu } = useProfile();
  const own = ownVars(model, cell);
  const fuelsV = vOf(model, 'production.fuels');
  const fuels = cell.stageSlug === 'extraction' && fuelsV ? linkedWith(model, cell, 'fuel', fuelsV) : [];
  return (
    <div className="space-y-6">
      {own.map((v) => {
        const y = baseYear(model, facts, v, cell, eu);
        return y === null ? null : (
          <div key={v.id} className="space-y-6">
            <SliceLeague v={v} item={cell} year={y} title={`${v.short}, ${y}`} />
            <UraniumOverTime v={v} />
            <SeriesBlock v={v} item={cell} title={v.short} />
          </div>
        );
      })}
      {fuelsV &&
        fuels.map((f) => {
          const y = baseYear(model, facts, fuelsV, f, eu);
          return y === null ? null : (
            <div key={f.slug} className="space-y-6">
              <SliceLeague v={fuelsV} item={f} year={y} title={`${f.label} production, ${y}`} />
              {facts && <ProdVsCons fuel={f} facts={facts} />}
              <SeriesBlock v={fuelsV} item={f} title={`${f.label} production`} />
            </div>
          );
        })}
      <EntityShares />
      <CapacityByProduct />
      <EvRegions />
      <MineralRows />
    </div>
  );
}

