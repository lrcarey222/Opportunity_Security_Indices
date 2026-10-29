// Profile S4 Trade (spec_sector.md 4.3): the reconciled Comtrade matrix of the cell (chunk
// sector-trade-<tech>). (a) exporters and importers, 2024 | 2020, net trade, and exports 2020 vs 2024;
// (b) who supplies whom (Sankey, or the top flows on phones; a highlighted economy's own partners);
// (c) where coalition members buy from; (d) what is in the basket; (f) the data-quality strip. Every
// number is a sum over the matrix, so the cards agree to the dollar; EU bloc mode removes intra-EU
// flows first.
// 2026-10-12 (spec_country.md C5.2-C5.4): the HHI explainer opens the section; "View by: Whole basket |
// Sub-component" re-scopes (a) to one group of the basket's codes (trade.cty_sub; the bilateral cards
// stay whole-basket, said so); "What the basket is made of" shows world exports by sub-component; the
// export-feasibility card (Atlas relatedness) closes the section.

import { useMemo, useState } from 'react';
import { useNavigate } from 'react-router';
import { SankeyChart, type SankeyFlow } from '../../../charts/Sankey.tsx';
import { TableView } from '../../../components/TableView.tsx';
import { Disclosure, Notice, Segmented, Select, Switch } from '../../../components/ui.tsx';
import { Term } from '../../../components/Term.tsx';
import { rowsToCsv } from '../../../lib/csv.ts';
import { useMediaQuery } from '../../../shell/useMediaQuery.ts';
import { GROUP_COLOR } from '../../../theme/palette.ts';
import type { SectorTradeChunk } from '../../contract.ts';
import type { GeoInfo } from '../../data/model.ts';
import { blocNote, GROUP_SHORT, isCoalition, partOf } from '../../derive/groups.ts';
import { blocFlows, countryTotals, declaredImporters, EXCLUDED_WORDS, excludedCodes, flowsOf, league, matrixTotal, netTrade, OTHER_EXPORTERS, OTHER_IMPORTERS, partners, sankeyLinks, supplierMixes, tradeQuality, type Flow, type TradeYear } from '../../derive/trade.ts';
import { sectorCsv } from '../../derive/csv.ts';
import { hasSubTrade, subComponents, subTotals, validSub, type SubComponent } from '../../derive/subs.ts';
import { fmtShare, fmtSignedPct, fmtUsd, listText } from '../../format.ts';
import { ChunkGate, HowToRead, markGroup } from '../common.tsx';
import { useTrade } from '../data.ts';
import { Dumbbell, GroupLegend, LeagueBars, SectorChartCard, ShareSplitChart, TopFlowsList, type DumbbellRow, type LeagueRow, type ShareSplitRow } from '../kit.tsx';
import { useSectorUrl } from '../state.tsx';
import { useCardCsv, useProfile } from './ctx.tsx';
import { HhiExplainer, type HhiHolder } from '../HhiExplainer.tsx';
import { FeasibilityCard } from './Feasibility.tsx';

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
  // trade by sub-component (C5.4): one group of the basket's codes, or the whole basket
  const subs = useMemo(() => subComponents(trade, model, cell.idx), [trade, model, cell]);
  const subData = hasSubTrade(trade) && subs.length > 0;
  const sub = subData && subs.length > 1 ? validSub(subs, state.sub) : null;
  const subBy = useMemo(() => (sub ? { 2020: subTotals(trade, model, cell.idx, sub, 2020, eu), 2024: subTotals(trade, model, cell.idx, sub, 2024, eu) } : null), [sub, trade, model, cell, eu]);
  /** the whole-basket matrix total (flows, Sankey) */
  const W = useMemo(() => matrixTotal(flows), [flows]);
  const totals = useMemo(() => (subBy ? subBy[year].totals : countryTotals(flows)), [subBy, year, flows]);
  /** the total behind the leagues: the sub-component's world total, or the matrix total */
  const WL = subBy ? subBy[year].W : W;
  const exL = useMemo(() => league(totals, 'exports', WL), [totals, WL]);
  const imL = useMemo(() => league(totals, 'imports', WL), [totals, WL]);
  const declared = useMemo(() => declaredImporters(trade, cell.idx, year), [trade, cell, year]);
  const quality = useMemo(() => tradeQuality(flows, declared), [flows, declared]);

  // basket
  const codes = useMemo(() => {
    const t = trade['trade.codes'];
    const out: { sub: string; hs6: string; description: string | null; shared: number; caveat: string | null }[] = [];
    for (let i = 0; i < t.nrow; i++) if (t.cols.item[i] === cell.idx) out.push({ sub: t.cols.sub_sector[i] ?? '', hs6: t.cols.hs6[i] ?? '', description: t.cols.description[i], shared: t.cols.shared_n[i], caveat: t.cols.caveat[i] });
    return out.sort((a, z) => a.sub.localeCompare(z.sub) || a.hs6.localeCompare(z.hs6));
  }, [trade, cell]);
  // excluded codes are listed per technology (schema 2, design C6): the reference master gives them no stage
  const excluded = useMemo(() => excludedCodes(trade, cell.tech ?? ''), [trade, cell]);
  const nSub = new Set(codes.map((c) => c.sub)).size;
  const nShared = codes.filter((c) => c.shared > 0).length;

  // (a) leagues
  const top = wide ? 15 : 10;
  const exRows = leagueOf(exL, geos, highlight, top);
  const imRows = leagueOf(imL, geos, highlight, top);
  const vExA = (sub && model.varById.get('trade.sub_exports')) || vEx;
  const vImA = (sub && model.varById.get('trade.sub_imports')) || vIm;
  const subSlug = sub ? `-${sub.toLowerCase().replace(/[^a-z0-9]+/g, '-').replace(/^-|-$/g, '')}` : '';
  const csvA = useCardCsv(
    () =>
      vExA && vImA
        ? {
            name: `${cell.slug}${subSlug}-exporters-importers-${year}`,
            text: sectorCsv(model, [
              ...exL.map((r) => ({ geo: geos[r.geo], v: vExA, item: cell, year, value: r.value, status: 'obs', share: r.share, rank: r.rank, note: sub ? `sub-component: ${sub}` : undefined })),
              ...imL.map((r) => ({ geo: geos[r.geo], v: vImA, item: cell, year, value: r.value, status: declared.has(r.geo) ? 'obs' : 'mirror', share: r.share, rank: r.rank, note: sub ? `sub-component: ${sub}` : undefined })),
            ]),
          }
        : null,
    sub ? ['trade.sub_exports', 'trade.sub_imports'] : ['trade.exports', 'trade.imports'],
  );
  const net = useMemo(() => netTrade(totals, 25), [totals]);
  const netRows: LeagueRow[] = net.map((t) => ({ id: geos[t.geo].code, label: geos[t.geo].name, group: markGroup(geos[t.geo]), value: t.net, display: `${t.net >= 0 ? '+' : '−'}${usd(Math.abs(t.net)).replace('USD ', 'USD ')}` }));
  const csvNet = useCardCsv(() => {
    const vNet = model.varById.get('trade.net');
    return vNet ? { name: `${cell.slug}${subSlug}-net-trade-${year}`, text: sectorCsv(model, net.map((t) => ({ geo: geos[t.geo], v: vNet, item: cell, year, value: t.net, status: 'obs', note: sub ? `sub-component: ${sub}` : undefined }))) } : null;
  }, ['trade.net']);

  // 2020 vs 2024
  const t20 = useMemo(() => (subBy ? subBy[2020].totals : countryTotals(flowsBy[2020])), [subBy, flowsBy]);
  const t24 = useMemo(() => (subBy ? subBy[2024].totals : countryTotals(flowsBy[2024])), [subBy, flowsBy]);
  const W20 = subBy ? subBy[2020].W : matrixTotal(flowsBy[2020]);
  const W24 = subBy ? subBy[2024].W : matrixTotal(flowsBy[2024]);
  const dumb: DumbbellRow[] = useMemo(() => {
    const ex = [...t24.values()].filter((t) => t.exports > 0).sort((a, z) => z.exports - a.exports).slice(0, 15);
    if (highlight && !ex.some((t) => t.geo === highlight.idx) && t24.get(highlight.idx)) ex.push(t24.get(highlight.idx)!);
    return ex.map((t) => {
      const a = t20.get(t.geo)?.exports ?? 0;
      return { id: geos[t.geo].code, label: geos[t.geo].name, a, b: t.exports, group: markGroup(geos[t.geo]), aLabel: `${usd(a)} (2020)`, bLabel: `${usd(t.exports)} (2024)${a > 0 ? ` · ${fmtSignedPct(t.exports / a - 1)}` : a === 0 ? ' · new' : ''}` };
    });
  }, [t20, t24, geos, highlight]);
  const csvDumb = useCardCsv(() => (vExA ? { name: `${cell.slug}${subSlug}-exports-2020-2024`, text: sectorCsv(model, dumb.flatMap((d) => [{ geo: model.geoByCode.get(d.id) ?? null, v: vExA, item: cell, year: 2020, value: d.a, status: 'obs' }, { geo: model.geoByCode.get(d.id) ?? null, v: vExA, item: cell, year: 2024, value: d.b, status: 'obs' }])) } : null), [sub ? 'trade.sub_exports' : 'trade.exports']);

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

  const nonDecl = quality.nonDeclarers.slice(0, 6).map((x) => geos[x.geo].name);
  const hNoDecl = highlight && highlight.kind === 'country' && !declared.has(highlight.idx);
  const yearTool = <Segmented<string> label="Year" hideLabel value={String(year)} options={[{ value: '2024', label: '2024' }, { value: '2020', label: '2020' }]} onChange={(y) => setYear(Number(y) as TradeYear)} />;
  const exTop = exL[0] ? geos[exL[0].geo] : null;
  // the HHI explainer describes the exporters of the scope shown (whole basket or one sub-component)
  const holders: HhiHolder[] = exL.map((r) => ({ name: geos[r.geo].name, share: r.share, group: markGroup(geos[r.geo]) }));
  const hhi = exL.length ? exL.reduce((a, r) => a + (Number.isFinite(r.share) ? r.share * r.share : 0), 0) : null;
  const subWords = sub ? ` (${sub})` : '';
  const vSuffix = (id: 'trade.exports' | 'trade.imports') => (sub ? (id === 'trade.exports' ? 'trade.sub_exports' : 'trade.sub_imports') : id);
  const cellCaveats = model.cellCaveats(cell.slug);

  return (
    <div className="space-y-6">
      <p className="text-sm text-paper-dim">
        The {model.cellLabel(cell)} <Term id="basket">product basket</Term>: {nSub} sub-component{nSub === 1 ? '' : 's'}, {codes.length} HS6 code{codes.length === 1 ? '' : 's'}
        {nShared ? `, ${nShared} of them also in other sectors` : ''}. Values come from the <Term id="matrix">reconciled trade matrix</Term>.
      </p>
      {cellCaveats.length > 0 && (
        <ul className="space-y-1 text-sm text-paper-mute" aria-label="Notes on this basket">
          {cellCaveats.map((c) => (
            <li key={c.id}>{c.text}</li>
          ))}
        </ul>
      )}
      <HhiExplainer model={model} hhi={hhi} holders={holders} noun="exporters" of={sub ? `world exports of ${sub.toLowerCase()}` : 'world exports'} context={`${model.cellLabel(cell)}${subWords}, ${year}`} />
      {subData && <SubPicker subs={subs} sub={sub} onChange={(x) => set({ sub: x })} />}
      {hNoDecl && (
        <Notice kind="info">
          {highlight.name} did not report {year} imports in this sector; its flows come from partner reports.
        </Notice>
      )}
      <SectorChartCard
        title={`Exporters and importers${subWords}`}
        vars={[{ var: vSuffix('trade.exports'), item: cell.slug, year }, { var: vSuffix('trade.imports'), item: cell.slug, year }]}
        subExtra={<HowToRead>Each bar is one economy: what it sells abroad (left) and buys from abroad (right) of these goods, with its share of world trade. Colours show the group: China, the coalition core, coalition partners or the rest of the world.</HowToRead>}
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
              { id: 'exs', header: 'Share of world exports', value: (t) => (WL > 0 ? t.exports / WL : null), cell: (t) => fmtShare(WL > 0 ? t.exports / WL : NaN), num: true },
              { id: 'im', header: 'Imports (USD)', value: (t) => t.imports, cell: (t) => usd(t.imports), num: true },
              { id: 'net', header: 'Net (USD)', value: (t) => t.net, cell: (t) => `${t.net >= 0 ? '' : '−'}${usd(Math.abs(t.net))}`, num: true },
              { id: 'm', header: 'Imports declared', value: (t) => (geos[t.geo].kind === 'eu' ? null : declared.has(t.geo) ? 'yes' : 'no: partner reports') },
            ]}
          />
        )}
        csv={csvA}
        legend={<GroupLegend groups={['china', 'core', 'partner', 'rest']} highlight={highlight?.name ?? null} euBloc={eu === 'bloc'} />}
        notes={eu === 'bloc' ? <p>{blocNote(model)} Intra-EU flows are removed before the EU is counted as one.</p> : undefined}
        summary={exTop ? `${exTop.name} leads ${year} exports${subWords} with ${fmtShare(exL[0].share)} of the USD ${fmtUsd(WL).replace('USD ', '')} world total; the largest importer is ${imL[0] ? geos[imL[0].geo].name : '–'}.` : 'No flows recorded.'}
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
        title={`Net trade of the largest traders${subWords}`}
        vars={[{ var: 'trade.net', item: cell.slug, year }]}
        subExtra={<HowToRead>Exports minus imports for the largest traders. A bar to the right means the economy sells more of these goods than it buys (a net exporter); to the left, it relies on imports (a net importer).</HowToRead>}
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
        notes={sub ? <p>Exports minus imports of {sub.toLowerCase()} only.</p> : undefined}
        summary={`Net trade (exports minus imports) of the ${net.length} largest traders${subWords}; positive means a net exporter.`}
      >
        <LeagueBars rows={netRows} axisLabel={`Exports minus imports, US dollars, ${year}`} diverging={{ negLabel: 'net importer ←', posLabel: '→ net exporter' }} selected={highlight?.code ?? null} top={25} ariaLabel={`Net trade, ${year}`} tickFormat={(v) => `${v < 0 ? '−' : ''}${fmtUsd(Math.abs(v)).replace('USD ', '')}`} />
      </SectorChartCard>

      <SectorChartCard
        title={`Exports, 2020 and 2024${subWords}`}
        vars={[{ var: vSuffix('trade.exports'), item: cell.slug, year: 2020 }, { var: vSuffix('trade.exports'), item: cell.slug, year: 2024 }]}
        subExtra={<HowToRead>Each line joins an exporter&apos;s 2020 value to its 2024 value: a longer line to the right means faster growth. Values are in current dollars, so price changes count too.</HowToRead>}
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
        subExtra={<HowToRead>{highlight ? `The largest partners of ${highlight.name}, with each partner's share of its ${state.tdir}.` : 'Bands run from the exporters (left) to the importers (right); the wider the band, the larger the trade between the two.'}</HowToRead>}
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
        notes={
          <>
            <p>Flows under 0.5% of world trade fold into "Other importers"; every flow is in the table.</p>
            {sub && <p>Bilateral flows are shown for the whole basket: sub-components are reported by economy, not by partner.</p>}
          </>
        }
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
        subExtra={<HowToRead>Each row is one importer; its bar splits what it buys by where the goods come from: China, the coalition core, coalition partners or the rest of the world. The supplier <Term id="hhi">concentration (HHI)</Term> is high when a few suppliers provide most of it.</HowToRead>}
        tools={<Switch label="All importers" checked={allImporters} onChange={(b) => { setAllImporters(b); setPage(1); }} />}
        table={() => (
          <TableView rows={mixes} caption="Import sources by supplier group" search pageSize={30} cols={[
            { id: 'c', header: 'Importer', value: (m) => geos[m.importer].name },
            { id: 't', header: 'Imports (USD)', value: (m) => m.total, cell: (m) => usd(m.total), num: true },
            { id: 'ch', header: 'China', value: (m) => m.parts.china, cell: (m) => fmtShare(m.parts.china), num: true },
            { id: 'co', header: 'Core', value: (m) => m.parts.core, cell: (m) => fmtShare(m.parts.core), num: true },
            { id: 'pa', header: 'Partners', value: (m) => m.parts.partner, cell: (m) => fmtShare(m.parts.partner), num: true },
            { id: 're', header: 'Rest', value: (m) => m.parts.rest, cell: (m) => fmtShare(m.parts.rest), num: true },
            { id: 'h', header: 'Supplier concentration (HHI)', value: (m) => m.hhi, cell: (m) => m.hhi.toFixed(3), num: true },
          ]} />
        )}
        csv={csvMix}
        legend={<GroupLegend groups={['china', 'core', 'partner', 'rest']} highlight={highlight?.name ?? null} euBloc={eu === 'bloc'} />}
        notes={
          eu === 'bloc' || sub ? (
            <>
              {eu === 'bloc' && <p>Intra-EU flows are removed; the EU shows as one row.</p>}
              {sub && <p>Import sources are shown for the whole basket.</p>}
            </>
          ) : undefined
        }
        summary={mixes[0] ? `${geos[mixes[0].importer].name} buys the largest share from China: ${fmtShare(mixes[0].china)} of its imports.` : 'No coalition imports recorded.'}
      >
        {mixRows.length ? (
          <>
            <ShareSplitChart
              rows={mixRows}
              columns={[{ key: 'imports', label: 'Imports', num: true }, { key: 'china', label: 'China share', num: true }, { key: 'hhi', label: 'Supplier concentration (HHI)', num: true }]}
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

      {subData && <CompositionCard subs={subs} sub={sub} onPick={(x) => set({ sub: subs.length > 1 ? x : null })} />}

      <SectorChartCard
        title="What is in the basket"
        vars={[{ var: 'trade.exports', item: cell.slug }]}
        subExtra={<HowToRead>Each row is one product code: the six-digit Harmonized System code (HS6), the finest level at which every country reports its trade. Together the codes make this sector’s trade <Term id="basket">basket</Term>; every trade figure above is the sum over them. Search for a product, or open the excluded codes and the reasons below.</HowToRead>}
        csv={() => ({ name: `${cell.slug}-basket`, text: rowsToCsv(codes.map((c) => ({ item: cell.slug, sub_sector: c.sub, hs6: c.hs6, description: c.description, shared_with_other_sectors: c.shared, caveat: c.caveat }))) })}
        notes={<p>Each code belongs to one sub-component of this sector. A code shared with other sectors counts fully in each of them, so baskets are never added across sectors.</p>}
        summary={`${codes.length} HS6 codes in ${nSub} sub-component${nSub === 1 ? '' : 's'}.`}
      >
        <TableView
          rows={codes}
          caption="HS6 codes in the basket"
          search
          pageSize={15}
          cols={[
            { id: 'hs6', header: 'Product code (HS6)', value: (c) => c.hs6 },
            { id: 'd', header: 'Description', value: (c) => c.description, minWidth: 220 },
            { id: 's', header: 'Sub-component', value: (c) => c.sub },
            { id: 'n', header: 'Shared with other sectors', value: (c) => c.shared, cell: (c) => (c.shared ? `${c.shared} cell${c.shared === 1 ? '' : 's'}` : 'no'), num: true },
            { id: 'cv', header: 'Caveat', value: (c) => c.caveat, cell: (c) => (c.caveat ? <span className="tag">{/kept with caveat/i.test(c.caveat) ? 'includes general-purpose goods' : c.caveat}</span> : '') },
          ]}
        />
        {excluded.length > 0 && (
          <div className="mt-3">
            <Disclosure summary={`Excluded from the ${cell.tech} baskets (${excluded.length} code${excluded.length === 1 ? '' : 's'})`}>
              <p className="mb-2 text-xs text-paper-mute">{EXCLUDED_WORDS}</p>
              <ul className="space-y-1 text-sm text-paper-dim">
                {excluded.map((e) => (
                  <li key={`${e.sub ?? ''}|${e.hs6}`}>
                    <span className="tabular text-paper">{e.hs6}</span>
                    {e.sub ? ` (${e.sub})` : ''}: {e.reason}
                  </li>
                ))}
              </ul>
            </Disclosure>
          </div>
        )}
      </SectorChartCard>

      <FeasibilityCard />

      <p className="text-sm text-paper-mute" role="note">
        {year} flows: {fmtShare(quality.importerShare)} of value as the importer declared it, {fmtShare(quality.mirrorShare)} from the exporter&apos;s report (×1.06), a <Term id="mirror">mirror</Term> value.
        {nonDecl.length ? ` Largest economies without ${year} import declarations: ${listText(nonDecl)}.` : ''}
      </p>
    </div>
  );
}

/** "View by: Whole basket | Sub-component" and the sub-component select (C5.4). */
function SubPicker({ subs, sub, onChange }: { subs: readonly SubComponent[]; sub: string | null; onChange: (sub: string | null) => void }) {
  if (subs.length < 2) return <p className="text-sm text-paper-mute">One group of codes: the whole basket.</p>;
  return (
    <div className="flex flex-wrap items-end gap-3" role="group" aria-label="View the trade cards by sub-component">
      <Segmented<'whole' | 'sub'> label="View by" value={sub ? 'sub' : 'whole'} options={[{ value: 'whole', label: 'Whole basket' }, { value: 'sub', label: 'Sub-component' }]} onChange={(x) => onChange(x === 'sub' ? (sub ?? subs[0].name) : null)} />
      {sub && <Select label="Sub-component" value={sub} options={subs.map((s) => ({ value: s.name, label: `${s.name}${Number.isFinite(s.share) ? ` (${fmtShare(s.share)} of the basket)` : ''}` }))} onChange={(x) => onChange(x)} className="min-w-[14rem]" />}
      <p className="basis-full text-xs text-paper-mute">
        A <Term id="sub_component">sub-component</Term> is one group of the basket&apos;s codes. The exporter, net-trade and 2020-2024 cards follow this choice; the cards of bilateral flows always show the whole basket.
      </p>
    </div>
  );
}

/** "What the basket is made of": world exports by sub-component, latest year, with shares (C5.4). */
function CompositionCard({ subs, sub, onPick }: { subs: readonly SubComponent[]; sub: string | null; onPick: (sub: string) => void }) {
  const { cell } = useProfile();
  const year = subs[0]?.year ?? null;
  const rows: LeagueRow[] = subs.map((s) => ({ id: s.name, label: s.name, group: 'rest', value: s.world, display: `${usd(s.world)} · ${fmtShare(s.share)}` }));
  const total = subs.reduce((a, s) => a + (Number.isFinite(s.world) ? s.world : 0), 0);
  return (
    <SectorChartCard
      title="What the basket is made of"
      vars={[{ var: 'trade.sub_exports', item: cell.slug, year }]}
      subExtra={<HowToRead>Each bar is one sub-component, a group of the basket’s product codes: its world exports in {year ?? 'the latest year'}, largest first, and its share of the whole basket. The longer the bar, the more of the sector’s trade it carries.</HowToRead>}
      table={() => (
        <TableView
          rows={[...subs]}
          caption="World exports by sub-component"
          rowKey={(s) => s.name}
          cols={[
            { id: 'n', header: 'Sub-component', value: (s) => s.name, minWidth: 160 },
            { id: 'w', header: `World exports, ${year ?? ''} (USD)`, value: (s) => s.world, cell: (s) => usd(s.world), num: true },
            { id: 's', header: 'Share of the basket', value: (s) => s.share, cell: (s) => fmtShare(s.share), num: true },
            { id: 'c', header: 'Product codes (HS6)', value: (s) => s.codes.join(', ') },
          ]}
        />
      )}
      csv={() => ({ name: `${cell.slug}-basket-by-sub-component-${year ?? ''}`, text: rowsToCsv(subs.map((s) => ({ item: cell.slug, sub_component: s.name, year, world_exports_usd: s.world, share_of_basket: s.share, hs6_codes: s.codes.join('|'), source: 'UN Comtrade, reconciled matrix' }))) })}
      notes={<p>Shares of this sector&apos;s world exports. A code counts in one sub-component here; sub-components are never added across sectors.{subs.length > 1 ? ' Click a bar to view the cards above by that sub-component.' : ''}</p>}
      summary={subs.length > 1 ? `${subs[0].name} is the largest of ${subs.length} sub-components: ${fmtShare(subs[0].share)} of world exports of ${usd(total)} in ${year ?? ''}.` : `One group of codes, the whole basket: world exports ${usd(total)} in ${year ?? ''}.`}
    >
      <LeagueBars rows={rows} axisLabel={`World exports, US dollars, ${year ?? ''}`} selected={sub} onSelect={subs.length > 1 ? (r) => onPick(r.id) : undefined} top={20} ariaLabel="World exports by sub-component" tickFormat={(v) => fmtUsd(v).replace('USD ', '')} />
    </SectorChartCard>
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
