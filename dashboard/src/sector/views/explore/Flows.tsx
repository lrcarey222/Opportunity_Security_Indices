// Explore, dyad layout for trade.flow (spec_sector.md 4.6 table, "FlowsPanel"): the Sankey of the top
// exporters to the top importers (the top flows list on phones), the highlighted economy's partner mix
// as a bloc bar, the origin-destination heat table of the top 15 x 15 on a log ramp (an observed zero
// prints "0"; a pair nobody reported is hatched), and the table of every flow. Sums over the
// reconciled matrix; EU bloc mode removes intra-EU flows first.

import { useMemo } from 'react';
import { SankeyChart, type SankeyFlow } from '../../../charts/Sankey.tsx';
import { TableView } from '../../../components/TableView.tsx';
import { Segmented } from '../../../components/ui.tsx';
import { rowsToCsv } from '../../../lib/csv.ts';
import { useMediaQuery } from '../../../shell/useMediaQuery.ts';
import { GROUP_COLOR } from '../../../theme/palette.ts';
import type { SectorTradeChunk } from '../../contract.ts';
import { blocNote } from '../../derive/groups.ts';
import { blocFlows, countryTotals, declaredImporters, flowsOf, importGroupShares, matrixTotal, OTHER_EXPORTERS, OTHER_IMPORTERS, pairState, partners, sankeyLinks, type TradeYear } from '../../derive/trade.ts';
import { fmtShare, fmtUsd } from '../../format.ts';
import { ChunkGate, markGroup } from '../common.tsx';
import { useTrade } from '../data.ts';
import { BlocBar, GroupLegend, HeatTable, SectorChartCard, TopFlowsList } from '../kit.tsx';
import type { ExploreCtx } from './ctx.ts';

const usd = (x: number) => fmtUsd(x);

function FlowsBody({ c, trade }: { c: ExploreCtx; trade: SectorTradeChunk }) {
  const { model, v, item, highlight, eu } = c;
  const wide = useMediaQuery('(min-width: 640px)');
  const year = (c.year === 2020 ? 2020 : 2024) as TradeYear;
  const geos = model.geos;
  const flows = useMemo(() => {
    if (!item) return [];
    const raw = flowsOf(trade, item.idx, year);
    return eu === 'bloc' ? blocFlows(raw, model) : raw;
  }, [trade, item, year, eu, model]);
  const W = useMemo(() => matrixTotal(flows), [flows]);
  const totals = useMemo(() => countryTotals(flows), [flows]);
  const declared = useMemo(() => (item ? declaredImporters(trade, item.idx, year) : new Set<number>()), [trade, item, year]);
  const sank = useMemo(() => sankeyLinks(flows, { topE: 8, topI: 10, fold: 0.005 }), [flows]);
  if (!item) return null;
  const code = (g: number) => (g === OTHER_EXPORTERS ? '__OTHER_E' : g === OTHER_IMPORTERS ? '__OTHER_I' : geos[g].code);
  const nameOf = (cd: string) => (cd === '__OTHER_E' ? 'Other exporters' : cd === '__OTHER_I' ? 'Other importers' : (model.geoByCode.get(cd)?.name ?? cd));
  const sankeyFlows: SankeyFlow[] = sank.links.map((l) => ({ producer: code(l.e), consumer: code(l.i), value: l.v }));
  // OD matrix: top 15 exporters x top 15 importers
  const topE = [...totals.values()].filter((t) => t.exports > 0).sort((a, z) => z.exports - a.exports).slice(0, 15).map((t) => t.geo);
  const topI = [...totals.values()].filter((t) => t.imports > 0).sort((a, z) => z.imports - a.imports).slice(0, 15).map((t) => t.geo);
  const fv = new Map(flows.map((f) => [`${f.e}|${f.i}`, f]));
  const pos = flows.map((f) => f.v).filter((x) => x > 0);
  const lo = pos.length ? Math.log10(Math.min(...pos)) : 0;
  const hi = pos.length ? Math.log10(Math.max(...pos)) : 1;
  const mix = highlight ? importGroupShares(flows, geos, highlight.idx) : null;
  const own = highlight ? partners(flows, highlight.idx, 'imports') : [];
  const cellName = model.cellLabel(item);
  const csv = () => ({
    name: `${item.slug}-flows-${year}-${eu}`,
    text: rowsToCsv([...flows].sort((a, z) => z.v - a.v).map((f) => ({ exporter: geos[f.e].code, exporter_name: geos[f.e].name, importer: geos[f.i].code, importer_name: geos[f.i].name, var_id: v.id, item: item.slug, year, value: f.v, unit: 'USD', status: f.s === 'mirror' ? 'mirror' : 'obs', share_of_world: W > 0 ? f.v / W : null, source: v.source.short, licence: v.licence }))),
  });
  const yearTool = <Segmented<string> label="Year" hideLabel value={String(year)} options={(v.years.length ? v.years : [2020, 2024]).map((y) => ({ value: String(y), label: String(y) }))} onChange={(y) => c.setYear(Number(y))} />;
  return (
    <div className="space-y-6">
      <SectorChartCard
        title={`Who supplies whom: ${cellName}, ${year}`}
        vars={[{ var: v.id, item: item.slug, year }]}
        tools={yearTool}
        table={() => (
          <TableView rows={[...flows].sort((a, z) => z.v - a.v)} caption="Bilateral flows" search pageSize={25} cols={[
            { id: 'e', header: 'Exporter', value: (f) => geos[f.e].name },
            { id: 'i', header: 'Importer', value: (f) => geos[f.i].name },
            { id: 'v', header: 'Value (USD)', value: (f) => f.v, cell: (f) => usd(f.v), num: true },
            { id: 's', header: 'Share of world trade', value: (f) => (W > 0 ? f.v / W : null), cell: (f) => fmtShare(W > 0 ? f.v / W : NaN), num: true },
            { id: 'src', header: 'Source', value: (f) => (f.s === 'mirror' ? 'partner mirror ×1.06' : 'importer reported') },
          ]} />
        )}
        csv={csv}
        legend={<GroupLegend groups={['china', 'core', 'partner', 'rest']} euBloc={eu === 'bloc'} />}
        notes={
          <>
            <p>World trade in this basket: {usd(W)}. Flows under 0.5% of it fold into "Other importers"; every flow is in the table.</p>
            {eu === 'bloc' && <p>{blocNote(model)} Intra-EU flows are removed.</p>}
          </>
        }
        summary={sank.links[0] ? `The largest flow is ${nameOf(code(sank.links[0].e))} to ${nameOf(code(sank.links[0].i))}, ${usd(sank.links[0].v)} (${fmtShare(sank.links[0].share)} of world trade in this basket).` : 'No flows.'}
      >
        {!flows.length ? (
          <p className="text-sm text-paper-mute">No flows recorded.</p>
        ) : wide ? (
          <SankeyChart
            flows={sankeyFlows}
            label={nameOf}
            ariaLabel={`Flows from the top exporters to the top importers, ${year}`}
            valueFormat={(x) => `${usd(x)} · ${fmtShare(W > 0 ? x / W : NaN)}`}
            nodeColor={(cd, side) => (cd.startsWith('__') ? '#6b7280' : side === 'producer' ? GROUP_COLOR[markGroup(model.geoByCode.get(cd))] : '#6b7280')}
            linkColor={(f) => `${f.producer.startsWith('__') ? '#6b7280' : GROUP_COLOR[markGroup(model.geoByCode.get(f.producer))]}8c`}
            sideLabels={{ left: 'EXPORTERS', right: 'IMPORTERS' }}
            tipNote="of world trade in this basket"
          />
        ) : (
          <TopFlowsList flows={sank.links.slice(0, 12).map((l, i) => ({ id: String(i), from: nameOf(code(l.e)), to: nameOf(code(l.i)), fromGroup: l.e >= 0 ? markGroup(geos[l.e]) : 'rest', value: usd(l.v), share: `${fmtShare(l.share)} of world trade`, flag: l.mirror ? 'includes partner mirrors' : undefined }))} ariaLabel={`Largest flows, ${year}`} />
        )}
      </SectorChartCard>

      {highlight && (
        <SectorChartCard
          title={`${highlight.name}: where its imports come from, ${year}`}
          vars={[{ var: v.id, item: item.slug, year }]}
          csv={() => ({ name: `${highlight.code}-${item.slug}-import-partners-${year}`, text: rowsToCsv(own.map((p) => ({ importer: highlight.code, exporter: geos[p.geo].code, exporter_name: geos[p.geo].name, group: geos[p.geo].group, var_id: v.id, item: item.slug, year, value: p.value, unit: 'USD', share_of_its_imports: p.share, status: p.mirror ? 'mirror' : 'obs', source: v.source.short, licence: v.licence }))) })}
          table={() => <TableView rows={own} caption="Import partners" cols={[{ id: 'p', header: 'Partner', value: (p) => geos[p.geo].name }, { id: 'v', header: 'Value (USD)', value: (p) => p.value, cell: (p) => usd(p.value), num: true }, { id: 's', header: 'Share of its imports', value: (p) => p.share, cell: (p) => fmtShare(p.share), num: true }, { id: 'm', header: 'Source', value: (p) => (p.mirror ? 'partner mirror ×1.06' : 'importer reported') }]} />}
          notes={!declared.has(highlight.idx) && highlight.kind === 'country' ? <p>{highlight.name} did not declare its {year} imports; its flows come from partner reports.</p> : undefined}
          summary={mix ? `${highlight.name} imports ${fmtShare(mix.china)} from China, ${fmtShare(mix.core + mix.partner)} from coalition members.` : `${highlight.name} has no recorded imports.`}
        >
          {mix ? <BlocBar parts={mix} ariaLabel={`${highlight.name}: import sources by group`} label="Share of its imports" /> : <p className="text-sm text-paper-mute">No imports recorded for {highlight.name}.</p>}
        </SectorChartCard>
      )}

      {topE.length > 1 && topI.length > 1 && (
        <SectorChartCard
          title={`Origin and destination: the top ${topE.length} exporters and ${topI.length} importers`}
          vars={[{ var: v.id, item: item.slug, year }]}
          subExtra=" · Log colour scale. 0: an observed zero (the pair declared no trade); hatched: no one reported the pair."
          csv={csv}
          summary={`Flows between the ${topE.length} largest exporters and ${topI.length} largest importers.`}
        >
          <HeatTable
            rows={topE.map((g) => ({ key: String(g), label: geos[g].name }))}
            cols={topI.map((g) => ({ key: String(g), label: geos[g].code }))}
            rowHeader="Exporter"
            caption="Origin-destination flows, US dollars"
            domain={[lo, hi === lo ? lo + 1 : hi]}
            cell={(rk, ck) => {
              const e = Number(rk);
              const i = Number(ck);
              const f = fv.get(`${e}|${i}`);
              if (f) return { value: f.v > 0 ? Math.log10(f.v) : null, display: usd(f.v).replace('USD ', ''), title: `${geos[e].name} to ${geos[i].name}: ${usd(f.v)}${f.s === 'mirror' ? ' (partner mirror)' : ''}` };
              const st = e === i ? 'self' : pairState(flows, declared, e, i);
              return st === 'zero' ? { value: null, display: '0', title: `${geos[e].name} to ${geos[i].name}: observed zero` } : { value: null, display: '', title: st === 'self' ? '' : `${geos[e].name} to ${geos[i].name}: not reported` };
            }}
          />
        </SectorChartCard>
      )}
    </div>
  );
}

export function FlowsLayout({ c }: { c: ExploreCtx }) {
  const tech = c.item?.techSlug ?? null;
  const { h, has } = useTrade(tech, !!tech);
  if (!c.item) return null;
  if (!has) return <p className="text-sm text-paper-mute">No trade basket for {c.model.cellLabel(c.item)}.</p>;
  return <ChunkGate h={h} name={`sector-trade-${tech}`}>{(t) => <FlowsBody c={c} trade={t} />}</ChunkGate>;
}
