// Profile S4 Trade (spec_sector.md 4.3): the reconciled Comtrade matrix of the cell (chunk
// sector-trade-<tech>). (a) exporters and importers, 2024 | 2020, net trade, and exports 2020 vs 2024;
// (b) who supplies whom (Sankey, or the top flows on phones; a highlighted economy's own partners);
// (c) where coalition members buy from; (d) what is in the basket; (f) the data-quality strip. Every
// number is a sum over the matrix, so the cards agree to the dollar; EU bloc mode removes intra-EU
// flows first.

import { useMemo, useState } from 'react';
import { useNavigate } from 'react-router';
import { SankeyChart, type SankeyFlow } from '../../../charts/Sankey.tsx';
import { TableView } from '../../../components/TableView.tsx';
import { Disclosure, Notice, Segmented, Switch } from '../../../components/ui.tsx';
import { rowsToCsv } from '../../../lib/csv.ts';
import { useMediaQuery } from '../../../shell/useMediaQuery.ts';
import { GROUP_COLOR } from '../../../theme/palette.ts';
import type { SectorTradeChunk } from '../../contract.ts';
import type { GeoInfo } from '../../data/model.ts';
import { blocNote, GROUP_SHORT, isCoalition, partOf } from '../../derive/groups.ts';
import { blocFlows, countryTotals, declaredImporters, flowsOf, league, matrixTotal, netTrade, OTHER_EXPORTERS, OTHER_IMPORTERS, partners, sankeyLinks, supplierMixes, tradeQuality, type Flow, type TradeYear } from '../../derive/trade.ts';
import { sectorCsv } from '../../derive/csv.ts';
import { fmtShare, fmtSignedPct, fmtUsd, listText } from '../../format.ts';
import { ChunkGate, markGroup } from '../common.tsx';
import { useTrade } from '../data.ts';
import { Dumbbell, GroupLegend, LeagueBars, SectorChartCard, ShareSplitChart, TopFlowsList, type DumbbellRow, type LeagueRow, type ShareSplitRow } from '../kit.tsx';
import { useSectorUrl } from '../state.tsx';
import { useCardCsv, useProfile } from './ctx.tsx';

const usd = (x: number) => fmtUsd(x);

function leagueOf(rows: ReturnType<typeof league>, geos: readonly GeoInfo[], highlight: GeoInfo | null, top: number): LeagueRow[] {
  const out: LeagueRow[] = [];
  rows.forEach((r, i) => {
    const g = geos[r.geo];
    const isH = highlight?.idx === r.geo;
    if (i >= top && !isH) return;
    out.push({ id: g.code, label: g.name, group: markGroup(g), value: r.value, display: `${usd(r.value)} · ${fmtShare(r.share)}`, pinned: i >= top && isH, state: 'obs', badge: g.re_export_hub ? 're-export hub' : undefined });
  });
  if (highlight && !rows.some((r) => r.geo === highlight.idx)) out.push({ id: highlight.code, label: highlight.name, group: markGroup(highlight), value: NaN, display: 'none recorded', pinned: true, state: 'not_covered' });
  return out;
}

function TradeBody({ trade }: { trade: SectorTradeChunk }) {
  const { model, cell, highlight, eu } = useProfile();
  const { state, set, link } = useSectorUrl();
  const navigate = useNavigate();
  const wide = useMediaQuery('(min-width: 640px)');
  const [year, setYear] = useState<TradeYear>(2024);
  const [allImporters, setAllImporters] = useState(false);
  const [page, setPage] = useState(1);
  const geos = model.geos;
  const vEx = model.varById.get('trade.exports');
  const vIm = model.varById.get('trade.imports');
  const vFlow = model.varById.get('trade.flow');

  const flowsBy = useMemo(() => {
    const f = (y: TradeYear) => {
      const raw = flowsOf(trade, cell.idx, y);
      return eu === 'bloc' ? blocFlows(raw, model) : raw;
    };
    return { 2020: f(2020), 2024: f(2024) } as Record<TradeYear, Flow[]>;
  }, [trade, cell, eu, model]);
  const flows = flowsBy[year];
  const totals = useMemo(() => countryTotals(flows), [flows]);
  const W = useMemo(() => matrixTotal(flows), [flows]);
  const exL = useMemo(() => league(totals, 'exports', W), [totals, W]);
  const imL = useMemo(() => league(totals, 'imports', W), [totals, W]);
  const declared = useMemo(() => declaredImporters(trade, cell.idx, year), [trade, cell, year]);
  const quality = useMemo(() => tradeQuality(flows, declared), [flows, declared]);

  // basket
  const codes = useMemo(() => {
    const t = trade['trade.codes'];
    const out: { sub: string; hs6: string; description: string | null; shared: number; caveat: string | null }[] = [];
    for (let i = 0; i < t.nrow; i++) if (t.cols.item[i] === cell.idx) out.push({ sub: t.cols.sub_sector[i] ?? '', hs6: t.cols.hs6[i] ?? '', description: t.cols.description[i], shared: t.cols.shared_n[i], caveat: t.cols.caveat[i] });
    return out.sort((a, z) => a.sub.localeCompare(z.sub) || a.hs6.localeCompare(z.hs6));
  }, [trade, cell]);
  const excluded = useMemo(() => {
    const t = trade['trade.excluded'];
    const out: { sub: string | null; hs6: string; reason: string }[] = [];
    for (let i = 0; i < t.nrow; i++) if (t.cols.item[i] === cell.idx) out.push({ sub: t.cols.sub_sector[i], hs6: t.cols.hs6[i] ?? '', reason: t.cols.reason[i] ?? '' });
    return out;
  }, [trade, cell]);
  const nSub = new Set(codes.map((c) => c.sub)).size;
  const nShared = codes.filter((c) => c.shared > 0).length;

  // (a) leagues
  const top = wide ? 15 : 10;
  const exRows = leagueOf(exL, geos, highlight, top);
  const imRows = leagueOf(imL, geos, highlight, top);
  const csvA = useCardCsv(
    () =>
      vEx && vIm
        ? {
            name: `${cell.slug}-exporters-importers-${year}`,
            text: sectorCsv(model, [
              ...exL.map((r) => ({ geo: geos[r.geo], v: vEx, item: cell, year, value: r.value, status: 'obs', share: r.share, rank: r.rank })),
              ...imL.map((r) => ({ geo: geos[r.geo], v: vIm, item: cell, year, value: r.value, status: declared.has(r.geo) ? 'obs' : 'mirror', share: r.share, rank: r.rank })),
            ]),
          }
        : null,
    ['trade.exports', 'trade.imports'],
  );
  const net = useMemo(() => netTrade(totals, 25), [totals]);
  const netRows: LeagueRow[] = net.map((t) => ({ id: geos[t.geo].code, label: geos[t.geo].name, group: markGroup(geos[t.geo]), value: t.net, display: `${t.net >= 0 ? '+' : '−'}${usd(Math.abs(t.net)).replace('USD ', 'USD ')}` }));
  const csvNet = useCardCsv(() => {
    const vNet = model.varById.get('trade.net');
    return vNet ? { name: `${cell.slug}-net-trade-${year}`, text: sectorCsv(model, net.map((t) => ({ geo: geos[t.geo], v: vNet, item: cell, year, value: t.net, status: 'obs' }))) } : null;
  }, ['trade.net']);

  // 2020 vs 2024
  const t20 = useMemo(() => countryTotals(flowsBy[2020]), [flowsBy]);
  const t24 = useMemo(() => countryTotals(flowsBy[2024]), [flowsBy]);
  const W20 = matrixTotal(flowsBy[2020]);
  const W24 = matrixTotal(flowsBy[2024]);
  const dumb: DumbbellRow[] = useMemo(() => {
    const ex = [...t24.values()].filter((t) => t.exports > 0).sort((a, z) => z.exports - a.exports).slice(0, 15);
    if (highlight && !ex.some((t) => t.geo === highlight.idx) && t24.get(highlight.idx)) ex.push(t24.get(highlight.idx)!);
    return ex.map((t) => {
      const a = t20.get(t.geo)?.exports ?? 0;
      return { id: geos[t.geo].code, label: geos[t.geo].name, a, b: t.exports, group: markGroup(geos[t.geo]), aLabel: `${usd(a)} (2020)`, bLabel: `${usd(t.exports)} (2024)${a > 0 ? ` · ${fmtSignedPct(t.exports / a - 1)}` : a === 0 ? ' · new' : ''}` };
    });
  }, [t20, t24, geos, highlight]);
  const csvDumb = useCardCsv(() => (vEx ? { name: `${cell.slug}-exports-2020-2024`, text: sectorCsv(model, dumb.flatMap((d) => [{ geo: model.geoByCode.get(d.id) ?? null, v: vEx, item: cell, year: 2020, value: d.a, status: 'obs' }, { geo: model.geoByCode.get(d.id) ?? null, v: vEx, item: cell, year: 2024, value: d.b, status: 'obs' }])) } : null), ['trade.exports']);

  // (b) flows
  const sank = useMemo(() => sankeyLinks(flows, { topE: 8, topI: 10, fold: 0.005 }), [flows]);
  const code = (g: number) => (g === OTHER_EXPORTERS ? '__OTHER_E' : g === OTHER_IMPORTERS ? '__OTHER_I' : geos[g].code);
  const nameOf = (c: string) => (c === '__OTHER_E' ? 'Other exporters' : c === '__OTHER_I' ? 'Other importers' : (model.geoByCode.get(c)?.name ?? c));
  const sankeyFlows: SankeyFlow[] = sank.links.map((l) => ({ producer: code(l.e), consumer: code(l.i), value: l.v }));
  const own = highlight ? partners(flows, highlight.idx, state.tdir) : [];
  const csvFlows = useCardCsv(
    () =>
      vFlow
        ? {
            name: `${cell.slug}-flows-${year}`,
            text: rowsToCsv(
              [...flows].sort((a, z) => z.v - a.v).map((f) => ({ exporter: geos[f.e].code, exporter_name: geos[f.e].name, importer: geos[f.i].code, importer_name: geos[f.i].name, var_id: vFlow.id, item: cell.slug, year, value: f.v, unit: 'USD', status: f.s === 'mirror' ? 'mirror' : 'obs', share_of_world: W > 0 ? f.v / W : null, source: vFlow.source.short, licence: vFlow.licence })),
            ),
          }
        : null,
    ['trade.flow'],
  );

  // (c) supplier mixes
  const mixes = useMemo(() => {
    const set0 = allImporters ? undefined : new Set(geos.filter((g) => isCoalition(g) || (eu === 'bloc' && g.kind === 'eu')).map((g) => g.idx));
    return supplierMixes(flows, geos, set0);
  }, [flows, geos, allImporters, eu]);
  const shownMixes = allImporters ? mixes.slice(0, page * 30) : mixes;
  const mixRows: ShareSplitRow[] = shownMixes.map((m) => {
    const g = geos[m.importer];
    return {
      id: g.code,
      label: g.name,
      sub: declared.has(m.importer) || g.kind === 'eu' ? undefined : 'from partner reports',
      parts: m.parts,
      cols: { imports: usd(m.total), china: fmtShare(m.china), hhi: m.hhi.toFixed(3) },
      outlined: highlight?.idx === m.importer,
      href: g.kind === 'country' ? link(`/sector/${cell.techSlug}/${cell.stageSlug}/${g.code}`) : undefined,
      ariaLabel: `${g.name} imports ${usd(m.total)}: China ${(m.china * 100).toFixed(1)} percent, coalition core ${(m.parts.core * 100).toFixed(1)} percent, partners ${(m.parts.partner * 100).toFixed(1)} percent, rest ${(m.parts.rest * 100).toFixed(1)} percent`,
    };
  });
  const csvMix = useCardCsv(() => ({ name: `${cell.slug}-import-sources-${year}`, text: rowsToCsv(mixes.map((m) => ({ iso3c: geos[m.importer].code, economy: geos[m.importer].name, group: geos[m.importer].group, year, imports_usd: m.total, share_china: m.parts.china, share_core: m.parts.core, share_partner: m.parts.partner, share_rest: m.parts.rest, supplier_hhi: m.hhi, source: vFlow?.source.short ?? 'UN Comtrade' }))) }), ['trade.flow']);

  const nonDecl = quality.nonDeclarers.slice(0, 6).map((x) => geos[x.geo].code);
  const hNoDecl = highlight && highlight.kind === 'country' && !declared.has(highlight.idx);
  const yearTool = <Segmented<string> label="Year" hideLabel value={String(year)} options={[{ value: '2024', label: '2024' }, { value: '2020', label: '2020' }]} onChange={(y) => setYear(Number(y) as TradeYear)} />;
  const exTop = exL[0] ? geos[exL[0].geo] : null;

  return (
    <div className="space-y-6">
      <p className="text-sm text-paper-dim">
        The {model.cellLabel(cell)} basket: {nSub} sub-sector{nSub === 1 ? '' : 's'}, {codes.length} HS6 code{codes.length === 1 ? '' : 's'}
        {nShared ? `, ${nShared} of them also in other sectors` : ''}.
      </p>
      {hNoDecl && (
        <Notice kind="info">
          {highlight.name} did not report {year} imports in this sector; its flows come from partner reports.
        </Notice>
      )}
      <SectorChartCard
        title="Exporters and importers"
        vars={[{ var: 'trade.exports', item: cell.slug, year }, { var: 'trade.imports', item: cell.slug, year }]}
        tools={yearTool}
        table={() => (
          <TableView
            rows={[...totals.values()]}
            caption="Exports and imports by economy"
            search
            pageSize={25}
            initialSort={{ id: 'ex', desc: true }}
            cols={[
              { id: 'c', header: 'Economy', value: (t) => geos[t.geo].name, minWidth: 150 },
              { id: 'g', header: 'Group', value: (t) => GROUP_SHORT[partOf(geos[t.geo])] },
              { id: 'ex', header: 'Exports (USD)', value: (t) => t.exports, cell: (t) => usd(t.exports), num: true },
              { id: 'exs', header: 'Share of world exports', value: (t) => (W > 0 ? t.exports / W : null), cell: (t) => fmtShare(W > 0 ? t.exports / W : NaN), num: true },
              { id: 'im', header: 'Imports (USD)', value: (t) => t.imports, cell: (t) => usd(t.imports), num: true },
              { id: 'net', header: 'Net (USD)', value: (t) => t.net, cell: (t) => `${t.net >= 0 ? '' : '−'}${usd(Math.abs(t.net))}`, num: true },
              { id: 'm', header: 'Imports declared', value: (t) => (geos[t.geo].kind === 'eu' ? null : declared.has(t.geo) ? 'yes' : 'no: partner reports') },
            ]}
          />
        )}
        csv={csvA}
        legend={<GroupLegend groups={['china', 'core', 'partner', 'rest']} highlight={highlight?.name ?? null} euBloc={eu === 'bloc'} />}
        notes={eu === 'bloc' ? <p>{blocNote(model)} Intra-EU flows are removed before the EU is counted as one.</p> : undefined}
        summary={exTop ? `${exTop.name} leads ${year} exports with ${fmtShare(exL[0].share)} of the USD ${fmtUsd(W).replace('USD ', '')} world total; the largest importer is ${imL[0] ? geos[imL[0].geo].name : '–'}.` : 'No flows recorded.'}
      >
        <div className="grid gap-6 md:grid-cols-2">
          <div>
            <h4 className="mb-2 text-sm font-medium text-paper-dim">Top exporters</h4>
            <LeagueBars rows={exRows} axisLabel={`US dollars, ${year}`} selected={highlight?.code ?? null} onSelect={(r) => set({ c: r.id === state.c ? null : r.id })} top={top} ariaLabel={`Top exporters, ${year}`} tickFormat={(v) => fmtUsd(v).replace('USD ', '')} />
          </div>
          <div>
            <h4 className="mb-2 text-sm font-medium text-paper-dim">Top importers (who buys)</h4>
            <LeagueBars rows={imRows} axisLabel={`US dollars, ${year}`} selected={highlight?.code ?? null} onSelect={(r) => set({ c: r.id === state.c ? null : r.id })} top={top} ariaLabel={`Top importers, ${year}`} tickFormat={(v) => fmtUsd(v).replace('USD ', '')} />
          </div>
        </div>
      </SectorChartCard>

      <SectorChartCard
        title="Net trade of the largest traders"
        vars={[{ var: 'trade.net', item: cell.slug, year }]}
        tools={yearTool}
        table={() => (
          <TableView rows={net} caption="Net trade" cols={[
            { id: 'c', header: 'Economy', value: (t) => geos[t.geo].name },
            { id: 'ex', header: 'Exports (USD)', value: (t) => t.exports, cell: (t) => usd(t.exports), num: true },
            { id: 'im', header: 'Imports (USD)', value: (t) => t.imports, cell: (t) => usd(t.imports), num: true },
            { id: 'net', header: 'Net (USD)', value: (t) => t.net, cell: (t) => `${t.net >= 0 ? '+' : '−'}${usd(Math.abs(t.net))}`, num: true },
          ]} />
        )}
        csv={csvNet}
        summary={`Net trade (exports minus imports) of the ${net.length} largest traders; positive means a net exporter.`}
      >
        <LeagueBars rows={netRows} axisLabel={`Exports minus imports, US dollars, ${year}`} diverging={{ negLabel: 'net importer ←', posLabel: '→ net exporter' }} selected={highlight?.code ?? null} top={25} ariaLabel={`Net trade, ${year}`} tickFormat={(v) => `${v < 0 ? '−' : ''}${fmtUsd(Math.abs(v)).replace('USD ', '')}`} />
      </SectorChartCard>

      <SectorChartCard
        title="Exports, 2020 and 2024"
        vars={[{ var: 'trade.exports', item: cell.slug, year: 2020 }, { var: 'trade.exports', item: cell.slug, year: 2024 }]}
        table={() => (
          <TableView rows={dumb} caption="Exports 2020 and 2024" cols={[
            { id: 'c', header: 'Economy', value: (d) => d.label },
            { id: 'a', header: '2020 (USD)', value: (d) => d.a, cell: (d) => usd(d.a), num: true },
            { id: 'b', header: '2024 (USD)', value: (d) => d.b, cell: (d) => usd(d.b), num: true },
          ]} />
        )}
        csv={csvDumb}
        notes={<p>Nominal US dollars; 2020 was a COVID trough and fuel prices roughly doubled since. World total {fmtUsd(W20)} in 2020 and {fmtUsd(W24)} in 2024{W20 > 0 ? ` (${fmtSignedPct(W24 / W20 - 1)})` : ''}.</p>}
        summary={`Exports of the top ${dumb.length} exporters in 2020 and 2024.`}
      >
        <Dumbbell rows={dumb} xLabel="Exports, US dollars" aName="2020" bName="2024" ariaLabel="Exports 2020 and 2024" selected={highlight?.code ?? null} tickFormat={(v) => fmtUsd(v).replace('USD ', '')} />
      </SectorChartCard>

      <SectorChartCard
        title={highlight ? `${highlight.name}: ${state.tdir === 'imports' ? 'where its imports come from' : 'where its exports go'}` : 'Who supplies whom'}
        vars={[{ var: 'trade.flow', item: cell.slug, year }]}
        tools={
          <>
            {yearTool}
            {highlight && <Segmented label="Direction" hideLabel value={state.tdir} options={[{ value: 'exports', label: 'Exports' }, { value: 'imports', label: 'Imports' }]} onChange={(d) => set({ tdir: d })} />}
          </>
        }
        table={() => (
          <TableView
            rows={[...flows].sort((a, z) => z.v - a.v)}
            caption="Bilateral flows"
            search
            pageSize={25}
            cols={[
              { id: 'e', header: 'Exporter', value: (f) => geos[f.e].name },
              { id: 'i', header: 'Importer', value: (f) => geos[f.i].name },
              { id: 'v', header: 'Value (USD)', value: (f) => f.v, cell: (f) => usd(f.v), num: true },
              { id: 's', header: 'Share of world trade', value: (f) => (W > 0 ? f.v / W : null), cell: (f) => fmtShare(W > 0 ? f.v / W : NaN), num: true },
              { id: 'src', header: 'Source', value: (f) => (f.s === 'mirror' ? 'partner mirror ×1.06' : 'importer reported') },
            ]}
          />
        )}
        csv={csvFlows}
        legend={!highlight ? <GroupLegend groups={['china', 'core', 'partner', 'rest']} /> : undefined}
        notes={<p>Flows under 0.5% of world trade fold into "Other importers"; every flow is in the table.</p>}
        summary={sank.links[0] ? `The largest flow is ${nameOf(code(sank.links[0].e))} to ${nameOf(code(sank.links[0].i))}, ${usd(sank.links[0].v)} (${fmtShare(sank.links[0].share)} of world trade in this basket).` : 'No flows.'}
      >
        {highlight ? (
          own.length ? (
            <TopFlowsList
              flows={own.slice(0, 10).map((p) => ({
                id: geos[p.geo].code,
                from: state.tdir === 'imports' ? geos[p.geo].name : highlight.name,
                to: state.tdir === 'imports' ? highlight.name : geos[p.geo].name,
                fromGroup: markGroup(state.tdir === 'imports' ? geos[p.geo] : highlight),
                value: usd(p.value),
                share: `${fmtShare(p.share)} of its ${state.tdir}`,
                flag: p.mirror ? 'partner mirror ×1.06' : undefined,
              }))}
              ariaLabel={`${highlight.name}: top partners`}
            />
          ) : (
            <p className="text-sm text-paper-mute">No {state.tdir} recorded for {highlight.name} in {year}.</p>
          )
        ) : wide ? (
          <SankeyChart
            flows={sankeyFlows}
            label={nameOf}
            ariaLabel={`Flows from the top exporters to the top importers, ${year}`}
            valueFormat={(v) => `${usd(v)} · ${fmtShare(W > 0 ? v / W : NaN)}`}
            nodeColor={(c, side) => (c.startsWith('__') ? '#6b7280' : side === 'producer' ? GROUP_COLOR[markGroup(model.geoByCode.get(c))] : '#6b7280')}
            linkColor={(f) => `${f.producer.startsWith('__') ? '#6b7280' : GROUP_COLOR[markGroup(model.geoByCode.get(f.producer))]}8c`}
            sideLabels={{ left: 'EXPORTERS', right: 'IMPORTERS' }}
            tipNote="of world trade in this basket"
          />
        ) : (
          <TopFlowsList
            flows={sank.links.slice(0, 12).map((l, i) => ({ id: String(i), from: nameOf(code(l.e)), to: nameOf(code(l.i)), fromGroup: l.e >= 0 ? markGroup(geos[l.e]) : 'rest', value: usd(l.v), share: `${fmtShare(l.share)} of world trade`, flag: l.mirror ? 'includes partner mirrors' : undefined }))}
            ariaLabel={`Largest flows, ${year}`}
          />
        )}
      </SectorChartCard>

      <SectorChartCard
        title="Where coalition members buy from"
        vars={[{ var: 'trade.flow', item: cell.slug, year }]}
        tools={<Switch label="All importers" checked={allImporters} onChange={(b) => { setAllImporters(b); setPage(1); }} />}
        table={() => (
          <TableView rows={mixes} caption="Import sources by supplier group" search pageSize={30} cols={[
            { id: 'c', header: 'Importer', value: (m) => geos[m.importer].name },
            { id: 't', header: 'Imports (USD)', value: (m) => m.total, cell: (m) => usd(m.total), num: true },
            { id: 'ch', header: 'China', value: (m) => m.parts.china, cell: (m) => fmtShare(m.parts.china), num: true },
            { id: 'co', header: 'Core', value: (m) => m.parts.core, cell: (m) => fmtShare(m.parts.core), num: true },
            { id: 'pa', header: 'Partners', value: (m) => m.parts.partner, cell: (m) => fmtShare(m.parts.partner), num: true },
            { id: 're', header: 'Rest', value: (m) => m.parts.rest, cell: (m) => fmtShare(m.parts.rest), num: true },
            { id: 'h', header: 'Supplier HHI', value: (m) => m.hhi, cell: (m) => m.hhi.toFixed(3), num: true },
          ]} />
        )}
        csv={csvMix}
        legend={<GroupLegend groups={['china', 'core', 'partner', 'rest']} highlight={highlight?.name ?? null} euBloc={eu === 'bloc'} />}
        notes={eu === 'bloc' ? <p>Intra-EU flows are removed; the EU shows as one row.</p> : undefined}
        summary={mixes[0] ? `${geos[mixes[0].importer].name} buys the largest share from China: ${fmtShare(mixes[0].china)} of its imports.` : 'No coalition imports recorded.'}
      >
        {mixRows.length ? (
          <>
            <ShareSplitChart
              rows={mixRows}
              columns={[{ key: 'imports', label: 'Imports', num: true }, { key: 'china', label: 'China share', num: true }, { key: 'hhi', label: 'Supplier HHI', num: true }]}
              ariaLabel="Import sources of each importer, by supplier group"
              onRowClick={(r) => r.href && navigate(r.href)}
            />
            {allImporters && mixes.length > shownMixes.length && (
              <button type="button" className="btn btn-ghost btn-sm mt-3" onClick={() => setPage((p) => p + 1)}>
                Show 30 more ({mixes.length - shownMixes.length} left)
              </button>
            )}
          </>
        ) : (
          <p className="text-sm text-paper-mute">No imports recorded.</p>
        )}
      </SectorChartCard>

      <SectorChartCard
        title="What is in the basket"
        vars={[{ var: 'trade.exports', item: cell.slug }]}
        csv={() => ({ name: `${cell.slug}-basket`, text: rowsToCsv(codes.map((c) => ({ item: cell.slug, sub_sector: c.sub, hs6: c.hs6, description: c.description, shared_with_other_sectors: c.shared, caveat: c.caveat }))) })}
        notes={<p>Flows are cell-level; sub-sectors do not filter the cards above.</p>}
        summary={`${codes.length} HS6 codes in ${nSub} sub-sectors.`}
      >
        <TableView
          rows={codes}
          caption="HS6 codes in the basket"
          search
          pageSize={15}
          cols={[
            { id: 'hs6', header: 'HS6', value: (c) => c.hs6 },
            { id: 'd', header: 'Description', value: (c) => c.description, minWidth: 220 },
            { id: 's', header: 'Sub-sector', value: (c) => c.sub },
            { id: 'n', header: 'Shared with other sectors', value: (c) => c.shared, cell: (c) => (c.shared ? `${c.shared} cell${c.shared === 1 ? '' : 's'}` : 'no'), num: true },
            { id: 'cv', header: 'Caveat', value: (c) => c.caveat, cell: (c) => (c.caveat ? <span className="tag">{/kept with caveat/i.test(c.caveat) ? 'includes general-purpose goods' : c.caveat}</span> : '') },
          ]}
        />
        {excluded.length > 0 && (
          <div className="mt-3">
            <Disclosure summary={`Excluded from the basket (${excluded.length} code${excluded.length === 1 ? '' : 's'})`}>
              <ul className="space-y-1 text-sm text-paper-dim">
                {excluded.map((e) => (
                  <li key={e.hs6}>
                    <span className="tabular text-paper">{e.hs6}</span>
                    {e.sub ? ` (${e.sub})` : ''}: {e.reason}
                  </li>
                ))}
              </ul>
            </Disclosure>
          </div>
        )}
      </SectorChartCard>

      <p className="text-sm text-paper-mute" role="note">
        {year} flows: {fmtShare(quality.importerShare)} of value importer-declared, {fmtShare(quality.mirrorShare)} from partner reports (×1.06).
        {nonDecl.length ? ` Largest economies without ${year} import declarations: ${listText(nonDecl)}.` : ''}
      </p>
    </div>
  );
}

export function TradeSection() {
  const { tech } = useProfile();
  const { h } = useTrade(tech);
  return (
    <ChunkGate h={h} name={`sector-trade-${tech}`}>
      {(t) => <TradeBody trade={t} />}
    </ChunkGate>
  );
}
