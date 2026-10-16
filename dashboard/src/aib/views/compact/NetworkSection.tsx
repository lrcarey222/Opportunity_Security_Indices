// 4. Partnership network: the trade relationships behind the compact as a Sankey, exporter -> importer
// (the largest flows of added trade, coloured new or expanded relationship), and the investment
// relationships, investor -> host (supply from the simulated new capacity). One node filter scopes both
// cards. Table twins and CSVs list every relationship the export carries.

import { useMemo, useState } from 'react';
import { ChartCard } from '../../../charts/ChartCard.tsx';
import { Legend } from '../../../charts/legends.tsx';
import { SankeyChart, type SankeyFlow } from '../../../charts/Sankey.tsx';
import { TableView, type Col } from '../../../components/TableView.tsx';
import { EmptyState, FilterRow, Select } from '../../../components/ui.tsx';
import type { AibInvestmentEdge, AibTradeEdge } from '../../contract.ts';
import { investmentFlows, tradeFlows, type TradeFlow } from '../../derive/flows.ts';
import { fmtScore, fmtShare, fmtUsd, plural } from '../../derive/format.ts';
import { csvWithRun, Section } from '../common.tsx';
import { useCompact } from './shared.tsx';

const TOP = 15;
export const REL_COLOR = { new: '#8b9d17', expanded: '#1889b0' } as const;
const REL_LINK = { new: 'rgb(139 157 23 / 0.6)', expanded: 'rgb(24 137 176 / 0.6)' } as const;
const REL_WORDS = { new: 'New relationship', expanded: 'Expanded relationship' } as const;

export function NetworkSection() {
  const { c, meta, name, nodeLabel } = useCompact();
  const [node, setNode] = useState('');
  const sel = node || null;
  const trade = useMemo(() => tradeFlows(c, sel, TOP), [c, sel]);
  const invest = useMemo(() => investmentFlows(c, sel, TOP), [c, sel]);
  const tradeEdges = c.trade.filter((e) => !sel || e.node === sel);
  const invEdges = c.investment.filter((e) => !sel || e.node === sel);
  const scope = sel ? nodeLabel(sel) : 'all nodes';
  const options = [{ value: '', label: 'All nodes' }, ...c.nodes.map((id) => ({ value: id, label: nodeLabel(id) }))];

  const tradeCols: Col<AibTradeEdge>[] = [
    { id: 'from', header: 'Exporter', value: (e) => name(e.from) },
    { id: 'to', header: 'Importer', value: (e) => name(e.to) },
    { id: 'node', header: 'Node', value: (e) => nodeLabel(e.node) },
    { id: 'usd', header: 'Added trade', value: (e) => e.usd, cell: (e) => fmtUsd(e.usd), num: true },
    { id: 'share', header: 'Share of node demand', value: (e) => e.share, cell: (e) => fmtShare(e.share), num: true },
    { id: 'rel', header: 'Relationship', value: (e) => REL_WORDS[e.relationship] },
    { id: 'opp', header: 'Export Opportunity score', value: (e) => e.opportunity, cell: (e) => fmtScore(e.opportunity), num: true },
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
    name: `${c.id} trade relationships${sel ? ` ${scope}` : ''}`,
    text: csvWithRun(
      tradeEdges.map((e) => ({
        compact: c.id, node_id: e.node, sub_sector: nodeLabel(e.node), exporter_iso3: e.from, exporter: name(e.from), importer_iso3: e.to, importer: name(e.to),
        added_trade_usd: e.usd, share_of_node_demand: e.share, relationship: e.relationship, export_opportunity: e.opportunity,
      })),
      meta,
    ),
  });
  const invCsv = () => ({
    name: `${c.id} investment relationships${sel ? ` ${scope}` : ''}`,
    text: csvWithRun(
      invEdges.map((e) => ({
        compact: c.id, node_id: e.node, sub_sector: nodeLabel(e.node), investor_iso3: e.investor, investor: name(e.investor), host_iso3: e.host, host: name(e.host),
        supply_from_new_capacity_usd: e.usd, share_of_node_demand: e.share, friendshore: e.friendshore,
      })),
      meta,
    ),
  });
  const nNew = trade.filter((f) => f.relationship === 'new').length;
  return (
    <Section
      id="network"
      title="Partnership network"
      intro={
        <p>
          The trade and investment relationships the model&apos;s solution relies on. Trade flows are the added shipments of the optimized solution (the export keeps the largest relationships of
          each compact); investment flows are the supply from the simulated new capacity, by the economy investing and the host.
        </p>
      }
    >
      <FilterRow result={`${plural(tradeEdges.length, 'trade relationship')}, ${plural(invEdges.length, 'investment relationship')}`}>
        <Select label="Node" value={node} options={options} onChange={setNode} className="min-w-[16rem]" />
      </FilterRow>
      <ChartCard
        headingLevel={3}
        title="Trade relationships"
        sub={`Exporter to importer, ${scope}: the ${Math.min(TOP, trade.length)} largest flows of added trade${nNew ? `, ${nNew} of them new relationships` : ''}. Band width is the added trade (USD a year).`}
        legend={
          <Legend
            label="Relationship"
            items={[
              { label: REL_WORDS.new, color: REL_COLOR.new },
              { label: REL_WORDS.expanded, color: REL_COLOR.expanded },
            ]}
          />
        }
        summary={`Trade relationships of ${c.name}, ${scope}: ${trade
          .slice(0, 5)
          .map((f) => `${name(f.producer)} to ${name(f.consumer)} ${fmtUsd(f.value)} (${f.relationship})`)
          .join('; ')}.`}
        table={() => <TableView rows={tradeEdges} cols={tradeCols} caption={`${c.name}: trade relationships, ${scope}`} search pageSize={20} initialSort={{ id: 'usd', desc: true }} />}
        csv={tradeCsv}
      >
        {trade.length ? (
          <SankeyChart
            flows={trade}
            label={(code) => name(code)}
            ariaLabel={`Trade relationships of ${c.name}, ${scope}, exporter to importer`}
            valueFormat={fmtUsd}
            linkColor={(f: SankeyFlow) => REL_LINK[(f as TradeFlow).relationship] ?? REL_LINK.expanded}
            nodeColor={(_, side) => (side === 'producer' ? '#1889b0' : '#6b7280')}
            sideLabels={{ left: 'EXPORTERS', right: 'IMPORTERS' }}
            tipNote="added trade a year, optimized solution"
          />
        ) : (
          <EmptyState>No added trade relationships for {scope}: the solution keeps existing shipments.</EmptyState>
        )}
      </ChartCard>
      <ChartCard
        headingLevel={3}
        title="Investment relationships"
        sub={`Investor to host, ${scope}: supply from the standardized new capacity at each eligible host (USD a year). A host must show revealed capability; the Friendshore score only orders eligible hosts.`}
        summary={`Investment relationships of ${c.name}, ${scope}: ${invest
          .slice(0, 5)
          .map((f) => `${name(f.producer)} in ${name(f.consumer)} ${fmtUsd(f.value)}`)
          .join('; ') || 'none'}.`}
        table={() => <TableView rows={invEdges} cols={invCols} caption={`${c.name}: investment relationships, ${scope}`} search pageSize={20} initialSort={{ id: 'usd', desc: true }} />}
        csv={invCsv}
      >
        {invest.length ? (
          <SankeyChart
            flows={invest}
            label={(code) => name(code)}
            ariaLabel={`Investment relationships of ${c.name}, ${scope}, investor to host`}
            valueFormat={fmtUsd}
            sideLabels={{ left: 'INVESTORS', right: 'HOSTS' }}
            tipNote="supply from the simulated new capacity a year"
          />
        ) : (
          <EmptyState>
            No investment relationships for {scope}: only nodes whose residual gap after optimized trade passes the investment threshold enter the investment simulation.
          </EmptyState>
        )}
      </ChartCard>
    </Section>
  );
}
