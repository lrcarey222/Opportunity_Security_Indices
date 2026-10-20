// 4. Partnership network: the trade relationships behind the compact as a Sankey, exporter -> importer
// (the largest flows of added trade, coloured new or expanded relationship), and the investment
// relationships, investor -> host (supply from the simulated new capacity; since 2026-10-17 the flows of
// the investment simulation, doc.investmentFlows, for the compact's nodes: the best-ranked host of each
// node, every ranked host as alternatives; InvestmentFlowsCard, id #invest). One node filter scopes both
// cards. Table twins and CSVs list every relationship the export carries.
//
// The scenarios (2026-10-19): both cards follow the toggle. Decoupling reads the compact's own trade and
// the main document's flows; Managed Interdependence the compact as its scenario file solves it (loading,
// failed and missing states; the reason when the scenario ran on none of its nodes) and that file's flows.
// Today (Baseline) is the reference under the trade Sankey: who supplies the compact's nodes (or the chosen
// node) today, beside who adds supply under the scenario (SuppliersCompare).

import { useMemo, useState } from 'react';
import { ChartCard } from '../../../charts/ChartCard.tsx';
import { Legend } from '../../../charts/legends.tsx';
import { SankeyChart, type SankeyFlow } from '../../../charts/Sankey.tsx';
import { TableView, type Col } from '../../../components/TableView.tsx';
import { EmptyState, FilterRow, Select } from '../../../components/ui.tsx';
import { ScenarioTag } from '../../charts/ScenarioCompare.tsx';
import type { AibInvestmentEdge, AibTradeEdge } from '../../contract.ts';
import { addedSuppliers, investmentFlows, tradeFlows, type TradeFlow } from '../../derive/flows.ts';
import { fmtScore, fmtShare, fmtUsd, plural } from '../../derive/format.ts';
import { decoupledEconomy } from '../../derive/network.ts';
import {
  compactDetail, compactSuppliersToday, nodeScenario, nodeSuppliersToday, runMetaOf, scenarioCsvFields, scenarioInfo, scenarioInvestmentFlows, scenarioNameOf,
} from '../../derive/scenarios.ts';
import { useAibCtx } from '../../AibLayout.tsx';
import { NO_FILTER } from '../../derive/partnerships.ts';
import { csvWithRun, Section } from '../common.tsx';
import { InvestmentFlowsCard } from '../InvestmentFlowsCard.tsx';
import { ScenarioFileState, useBaselineDoc, useManagedDoc } from '../networkParts.tsx';
import { SuppliersCompare } from '../scenarioNetwork.tsx';
import { useCompact } from './shared.tsx';

const TOP = 15;
export const REL_COLOR = { new: '#8b9d17', expanded: '#1889b0' } as const;
export const REL_LINK = { new: 'rgb(139 157 23 / 0.6)', expanded: 'rgb(24 137 176 / 0.6)' } as const;
export const REL_WORDS = { new: 'New relationship', expanded: 'Expanded relationship' } as const;

export function NetworkSection() {
  const { c, meta, name: baseName, nodeLabel, shockLabel } = useCompact();
  const { doc, names, nodeOf, scenario, hasScenarios } = useAibCtx();
  const mi = useManagedDoc(scenario === 'managed');
  const today = useBaselineDoc(hasScenarios);
  const detail = useMemo(() => compactDetail(c, scenario, mi.doc), [c, scenario, mi.doc]);
  // the scenario files name the economies only they use (today's suppliers may be economies the model never selects)
  const name = useMemo(() => scenarioNameOf(scenarioNameOf(baseName, scenario === 'managed' ? mi.doc : null), today.doc), [scenario, mi.doc, today.doc, baseName]);
  const [node, setNode] = useState('');
  const sel = node || null;
  const trade = useMemo(() => tradeFlows(detail, sel, TOP), [detail, sel]);
  const invest = useMemo(() => investmentFlows(detail, sel, TOP), [detail, sel]);
  const tradeEdges = detail.trade.filter((e) => !sel || e.node === sel);
  const invEdges = detail.investment.filter((e) => !sel || e.node === sel);
  const invFilter = useMemo(() => ({ ...NO_FILTER, node: sel, nodes: c.nodes }), [sel, c.nodes]);
  const flowsAll = scenarioInvestmentFlows(doc, scenario, mi.doc);
  const nFlows = flowsAll ? flowsAll.pairs.filter((p) => p.rank === 1 && c.nodes.includes(p.node) && (!sel || p.node === sel)).length : invEdges.length;
  const scope = sel ? nodeLabel(sel) : 'all nodes';
  const options = [{ value: '', label: 'All nodes' }, ...c.nodes.map((id) => ({ value: id, label: nodeLabel(id) }))];
  const info = scenarioInfo(meta, scenario);
  const under = hasScenarios ? ` under ${info.label}` : '';
  // today's suppliers (the Baseline file's ten, else the main document's five) and the scenario's added ones
  const todayList = useMemo(() => {
    const n = sel ? nodeOf(sel) : null;
    return sel ? (n ? nodeSuppliersToday(n, today.doc) : []) : compactSuppliersToday(c, today.doc);
  }, [sel, nodeOf, today.doc, c]);
  const added = useMemo(() => addedSuppliers(detail.trade, sel), [detail.trade, sel]);
  const removed = useMemo(() => (scenario === 'decoupling' ? decoupledEconomy(meta, names) : null), [scenario, meta, names]);
  const capped = useMemo(() => {
    const m = new Map<string, string[]>();
    if (scenario !== 'managed') return m;
    for (const id of sel ? [sel] : c.nodes) {
      const n = nodeOf(id);
      if (!n || !nodeScenario(n, 'managed').ran) continue;
      for (const iso of n.diversify?.capped ?? []) m.set(iso, [...(m.get(iso) ?? []), id]);
    }
    return m;
  }, [scenario, sel, c.nodes, nodeOf]);

  // the run columns name the scenario's own shock; the scenario is named where the bundle has them
  const runMeta = runMetaOf(meta, scenario);
  const scnCols = hasScenarios ? scenarioCsvFields(meta, scenario) : {};
  const tradeCols: Col<AibTradeEdge>[] = [
    { id: 'from', header: 'Exporter', value: (e) => name(e.from) },
    { id: 'to', header: 'Importer', value: (e) => name(e.to) },
    { id: 'node', header: 'Node', value: (e) => nodeLabel(e.node) },
    { id: 'usd', header: 'Added trade', value: (e) => e.usd, cell: (e) => fmtUsd(e.usd), num: true },
    { id: 'share', header: 'Share of node demand', value: (e) => e.share, cell: (e) => fmtShare(e.share), num: true },
    { id: 'rel', header: 'Relationship', value: (e) => REL_WORDS[e.relationship] },
    { id: 'opp', header: 'Partnership Opportunity score', value: (e) => e.opportunity, cell: (e) => fmtScore(e.opportunity), num: true },
  ];
  const invCols: Col<AibInvestmentEdge>[] = [
    { id: 'investor', header: 'Investor', value: (e) => name(e.investor) },
    { id: 'host', header: 'Host', value: (e) => name(e.host) },
    { id: 'node', header: 'Node', value: (e) => nodeLabel(e.node) },
    { id: 'usd', header: 'Supply from new capacity', value: (e) => e.usd, cell: (e) => fmtUsd(e.usd), num: true },
    { id: 'share', header: 'Share of node demand', value: (e) => e.share, cell: (e) => fmtShare(e.share), num: true },
    { id: 'fs', header: 'Friendshore score', value: (e) => e.friendshore, cell: (e) => fmtScore(e.friendshore), num: true },
  ];
  const tradeCsv = () => ({
    name: `${c.id} trade relationships${sel ? ` ${scope}` : ''}${scenario === 'managed' ? ` ${info.label}` : ''}`,
    text: csvWithRun(
      tradeEdges.map((e) => ({
        compact: c.id, node_id: e.node, sub_sector: nodeLabel(e.node), exporter_iso3: e.from, exporter: name(e.from), importer_iso3: e.to, importer: name(e.to),
        added_trade_usd: e.usd, share_of_node_demand: e.share, relationship: e.relationship, partnership_opportunity: e.opportunity, ...scnCols,
      })),
      runMeta,
    ),
  });
  const invCsv = () => ({
    name: `${c.id} investment relationships${sel ? ` ${scope}` : ''}${scenario === 'managed' ? ` ${info.label}` : ''}`,
    text: csvWithRun(
      invEdges.map((e) => ({
        compact: c.id, node_id: e.node, sub_sector: nodeLabel(e.node), investor_iso3: e.investor, investor: name(e.investor), host_iso3: e.host, host: name(e.host),
        supply_from_new_capacity_usd: e.usd, share_of_node_demand: e.share, friendshore: e.friendshore, ...scnCols,
      })),
      runMeta,
    ),
  });
  const nNew = trade.filter((f) => f.relationship === 'new').length;
  const badge = hasScenarios ? <ScenarioTag id={scenario} meta={meta} /> : undefined;
  const intro = (
    <p>
      The trade and investment relationships the model&apos;s solution relies on{under}. Trade flows are the added shipments of the optimized solution (the export keeps the largest relationships of
      each compact); investment flows are the supply from the simulated new capacity, by the economy investing and the host.
      {hasScenarios ? ' Under the trade relationships, who supplies these nodes today (Baseline) stands beside who adds supply under the scenario.' : ''}
    </p>
  );
  // Managed Interdependence without its file: the loading, failed or missing state in place of the cards
  if (detail.status === 'pending') {
    return (
      <Section id="network" title="Partnership network" intro={intro}>
        <ScenarioFileState load={mi} what="the partnership network" label={info.label} />
      </Section>
    );
  }
  const why = detail.reason && scenario === 'managed' ? detail.reason : null;
  return (
    <Section id="network" title="Partnership network" intro={intro}>
      <FilterRow result={`${plural(tradeEdges.length, 'trade relationship')}, ${plural(nFlows, 'investment flow')}`}>
        <Select label="Node" value={node} options={options} onChange={setNode} className="min-w-[16rem]" />
      </FilterRow>
      {why && (
        <p className="max-w-3xl text-sm text-paper-dim" data-testid="network-reason">
          {info.label}: {why}
        </p>
      )}
      <ChartCard
        headingLevel={3}
        title="Trade relationships"
        badge={badge}
        sub={
          trade.length
            ? `Exporter to importer, ${scope}${under}: ${trade.length === 1 ? 'the one flow' : `the ${Math.min(TOP, trade.length)} largest flows`} of added trade${nNew ? `, ${nNew === 1 && trade.length === 1 ? 'a new relationship' : `${nNew} of them new relationships`}` : ''}. Band width is the added trade (USD a year).`
            : `Exporter to importer, ${scope}${under}: no added trade.`
        }
        legend={
          <Legend
            label="Relationship"
            items={[
              { label: REL_WORDS.new, color: REL_COLOR.new },
              { label: REL_WORDS.expanded, color: REL_COLOR.expanded },
            ]}
          />
        }
        summary={`Trade relationships of ${c.name}, ${scope}${under}: ${trade
          .slice(0, 5)
          .map((f) => `${name(f.producer)} to ${name(f.consumer)} ${fmtUsd(f.value)} (${f.relationship})`)
          .join('; ') || 'none'}.`}
        table={() => <TableView rows={tradeEdges} cols={tradeCols} caption={`${c.name}: trade relationships, ${scope}${under}`} search pageSize={20} initialSort={{ id: 'usd', desc: true }} />}
        csv={tradeCsv}
      >
        <div className="space-y-4">
          {trade.length ? (
            <SankeyChart
              flows={trade}
              label={(code) => name(code)}
              ariaLabel={`Trade relationships of ${c.name}, ${scope}${under}, exporter to importer`}
              valueFormat={fmtUsd}
              linkColor={(f: SankeyFlow) => REL_LINK[(f as TradeFlow).relationship] ?? REL_LINK.expanded}
              nodeColor={(_, side) => (side === 'producer' ? '#1889b0' : '#6b7280')}
              sideLabels={{ left: 'EXPORTERS', right: 'IMPORTERS' }}
              tipNote="added trade a year, optimized solution"
            />
          ) : (
            <EmptyState>
              No added trade relationships for {scope}
              {under}: {why ?? 'the solution keeps existing shipments.'}
            </EmptyState>
          )}
          {hasScenarios && (
            <SuppliersCompare
              today={todayList}
              added={added}
              scenario={scenario}
              meta={meta}
              name={(iso) => name(iso)}
              scope={sel ? nodeLabel(sel) : 'the compact’s nodes'}
              removed={removed}
              capped={capped}
              todayPartial={today.loading || (!today.doc && !today.missing && !today.error)}
            />
          )}
        </div>
      </ChartCard>
      {flowsAll || (scenario === 'decoupling' && doc.investmentFlows) ? (
        <div id="invest" className="scroll-mt-16">
          <InvestmentFlowsCard
            doc={doc}
            meta={meta}
            shockLabel={shockLabel}
            filter={invFilter}
            scope={sel ? scope : 'the compact’s nodes'}
            name={(iso) => name(iso)}
            nodeLabel={nodeLabel}
            headingLevel={3}
            csvName={c.id}
            sideChoice={false}
            scenario={scenario}
          />
        </div>
      ) : (
        <ChartCard
          headingLevel={3}
          title="Investment relationships"
          badge={badge}
          sub={`Investor to host, ${scope}${under}: supply from the standardized new capacity at each eligible host (USD a year). A host must show revealed capability; the Friendshore score only orders eligible hosts.`}
          summary={`Investment relationships of ${c.name}, ${scope}${under}: ${invest
            .slice(0, 5)
            .map((f) => `${name(f.producer)} in ${name(f.consumer)} ${fmtUsd(f.value)}`)
            .join('; ') || 'none'}.`}
          table={() => <TableView rows={invEdges} cols={invCols} caption={`${c.name}: investment relationships, ${scope}${under}`} search pageSize={20} initialSort={{ id: 'usd', desc: true }} />}
          csv={invCsv}
        >
          {invest.length ? (
            <SankeyChart
              flows={invest}
              label={(code) => name(code)}
              ariaLabel={`Investment relationships of ${c.name}, ${scope}${under}, investor to host`}
              valueFormat={fmtUsd}
              sideLabels={{ left: 'INVESTORS', right: 'HOSTS' }}
              tipNote="supply from the simulated new capacity a year"
            />
          ) : (
            <EmptyState>
              No investment relationships for {scope}
              {under}: {why ?? 'only nodes whose residual gap after optimized trade passes the investment threshold enter the investment simulation.'}
            </EmptyState>
          )}
        </ChartCard>
      )}
    </Section>
  );
}
