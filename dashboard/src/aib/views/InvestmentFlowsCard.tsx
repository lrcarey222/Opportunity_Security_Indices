// Investor -> host flows card (2026-10-17): the flows of the marginal investment simulation (doc.investmentFlows)
// for a scope given by the caller (the Partnerships filters, a compact's nodes, one economy), as a Sankey of
// the best-ranked hosts (summed by investor and host over the nodes), a ranked list of node-level flows and
// a table twin / CSV with the host rank, the new-capacity supply in USD, the share of node demand, the
// investor's lost share, Friendshore and Partnership Opportunity. "Every ranked host" lists the alternatives
// too (ranks 2..): each is a separate simulation of the increment, never added to rank 1, so the
// Sankey always shows rank 1 only. Modelled result: it does not move with the Perspective dials.
//
// The scenarios (2026-10-19): the card follows the toggle. Decoupling reads doc.investmentFlows; Managed
// Interdependence the flows of its lazily loaded scenario file (loading, failed and missing states in
// place of the chart). `scenario` fixes the choice; without it the card reads the URL (scn), so every page
// that embeds it follows the toggle. Baseline is the reference in words: no new capacity exists today.

import { useMemo, useState } from 'react';
import { ChartCard } from '../../charts/ChartCard.tsx';
import { Legend } from '../../charts/legends.tsx';
import { SankeyChart } from '../../charts/Sankey.tsx';
import { TableView, type Col } from '../../components/TableView.tsx';
import { EmptyState, FilterRow, Segmented } from '../../components/ui.tsx';
import { ScenarioTag } from '../charts/ScenarioCompare.tsx';
import { scenarioDefsOf, type AibDoc, type AibInvestmentPair, type AibMeta } from '../contract.ts';
import { useAibScenario } from '../data/hooks.ts';
import { fmtScore, fmtShare, fmtUsd, plural } from '../derive/format.ts';
import { flowsDocOf, investmentPairs, investorHostFlows, rankWords, variantPhrase, type InvestmentFilter, type RankChoice } from '../derive/investment.ts';
import { effectiveScenario, runMetaOf, scenarioCsvFields, scenarioInfo, scenarioNameOf, scenarioShockLabel } from '../derive/scenarios.ts';
import { nodeWhere, taxonomyOf } from '../derive/taxonomy.ts';
import type { AibToggle } from '../scenarioUrl.ts';
import { csvWithRun } from './common.tsx';
import { FlowList, INVEST_COLOR, ModelledNote, ScenarioFileState, useManagedDoc } from './networkParts.tsx';

const TOP = 15;
const RANKS: { value: RankChoice; label: string }[] = [
  { value: 'best', label: 'Best-ranked host' },
  { value: 'all', label: 'Every ranked host' },
];
const SIDES = [
  { value: 'either', label: 'Investor or host' },
  { value: 'investor', label: 'As investor' },
  { value: 'host', label: 'As host' },
] as const;
type Side = (typeof SIDES)[number]['value'];

export interface InvestmentFlowsCardProps {
  doc: Pick<AibDoc, 'nodes' | 'investmentFlows' | 'meta'>;
  meta: AibMeta;
  /** the headline shock in words (the modelled-result note under Decoupling) */
  shockLabel: string;
  /** the node filter (technology, stage, sub-sector, economy) and, for a compact, its node ids */
  filter: InvestmentFilter;
  /** "Japan, Solar" or "all economies and sub-sectors" */
  scope: string;
  name: (iso: string) => string;
  nodeLabel: (id: string) => string;
  title?: string;
  headingLevel?: 2 | 3;
  /** a CSV file name prefix */
  csvName: string;
  /** offer "as investor / as host" when the filter names an economy */
  sideChoice?: boolean;
  /** list rows to show */
  listN?: number;
  /** the scenario to show (2026-10-19); omitted: the toggle in the URL, where the bundle has the scenarios */
  scenario?: AibToggle;
}

export function InvestmentFlowsCard({ doc, meta, shockLabel, filter, scope, name, nodeLabel, title = 'Who invests where', headingLevel = 2, csvName, sideChoice = true, listN = 8, scenario }: InvestmentFlowsCardProps) {
  const fromUrl = useAibScenario().scenario;
  const available = scenarioDefsOf(meta).some((d) => d.id === 'managed');
  const id = effectiveScenario(scenario ?? fromUrl, available);
  const mi = useManagedDoc(id === 'managed');
  const flowsOf = id === 'managed' ? (mi.doc?.investmentFlows ?? null) : (doc.investmentFlows ?? null);
  const fdoc = useMemo(() => flowsDocOf(doc, flowsOf), [doc, flowsOf]);
  const nm = useMemo(() => (id === 'managed' && mi.doc ? scenarioNameOf((iso) => name(iso ?? ''), mi.doc) : (iso: string | null | undefined) => name(iso ?? '')), [id, mi.doc, name]);

  const [rank, setRank] = useState<RankChoice>('best');
  const [side, setSide] = useState<Side>('either');
  const maxRank = fdoc.investmentFlows?.variant.maxRank;
  const f = useMemo<InvestmentFilter>(() => ({ ...filter, rank, side: filter.country && side !== 'either' ? side : null }), [filter, rank, side]);
  const pairs = useMemo(() => investmentPairs(fdoc, f), [fdoc, f]);
  const best = useMemo(() => (rank === 'best' ? pairs : investmentPairs(fdoc, { ...f, rank: 'best' })), [fdoc, f, pairs, rank]);
  const flows = useMemo(() => investorHostFlows(best, TOP), [best]);
  const total = best.reduce((s, p) => s + p.usd, 0);
  // an older bundle without flows (Decoupling): the caller shows its fallback card
  if (id === 'decoupling' && !doc.investmentFlows) return null;
  const info = scenarioInfo(meta, id);
  const badge = available ? <ScenarioTag id={id} meta={meta} /> : undefined;
  const under = available ? ` under ${info.label}` : '';
  const todayNote = available ? ' Today (Baseline) there is no such new capacity: these flows exist only under a scenario.' : '';
  if (id === 'managed' && !mi.doc) {
    return (
      <ChartCard headingLevel={headingLevel} title={title} badge={badge} sub={`Investor to host, ${scope}${under}.`}>
        <ScenarioFileState load={mi} what="the investment flows" label={info.label} />
      </ChartCard>
    );
  }
  if (!fdoc.investmentFlows) {
    return (
      <ChartCard headingLevel={headingLevel} title={title} badge={badge} sub={`Investor to host, ${scope}${under}.`}>
        <EmptyState>The {info.label} data of this build have no investment flows.</EmptyState>
      </ChartCard>
    );
  }
  const nodeOf = new Map(doc.nodes.map((n) => [n.id, n]));
  const tx = taxonomyOf(meta);
  const techStage = (nid: string) => {
    const n = nodeOf.get(nid);
    return n ? nodeWhere(tx, n) : '';
  };
  const cols: Col<AibInvestmentPair>[] = [
    { id: 'investor', header: 'Investor', value: (p) => nm(p.investor), title: 'The economy whose lost supply the new capacity replaces' },
    { id: 'host', header: 'Host', value: (p) => nm(p.host) },
    { id: 'node', header: 'Sub-sector', value: (p) => nodeLabel(p.node), minWidth: 160 },
    { id: 'ts', header: `${tx.One} and stage`, value: (p) => techStage(p.node) },
    { id: 'rank', header: 'Host rank', value: (p) => p.rank, num: true, title: '1 = the best-ranked host of the sub-sector; higher ranks are alternatives' },
    { id: 'usd', header: 'Supply from new capacity', value: (p) => p.usd, cell: (p) => fmtUsd(p.usd), num: true },
    { id: 'share', header: 'Share of node demand', value: (p) => p.share, cell: (p) => fmtShare(p.share), num: true },
    { id: 'lost', header: 'Investor’s lost share', value: (p) => p.lostShare, cell: (p) => fmtShare(p.lostShare), num: true },
    { id: 'fs', header: 'Friendshore', value: (p) => p.friendshore, cell: (p) => fmtScore(p.friendshore), num: true, title: 'The investor’s Friendshore score for the host (NEIS defaults)' },
    { id: 'eo', header: 'Partnership Opportunity', value: (p) => p.opportunity, cell: (p) => fmtScore(p.opportunity), num: true, title: 'The host’s Partnership Opportunity score for the investor (NEIS defaults)' },
  ];
  // the run columns name the scenario's own shock (the Diversification page's pattern)
  const runMeta = runMetaOf(meta, id);
  const scn = available ? scenarioCsvFields(meta, id) : null;
  const csv = () => ({
    name: `${csvName} investor to host flows${scope ? ` ${scope}` : ''}${rank === 'all' ? ' every ranked host' : ''}${id === 'managed' ? ` ${info.label}` : ''}`,
    text: csvWithRun(
      pairs.map((p) => ({
        investor_iso3: p.investor, investor: nm(p.investor), host_iso3: p.host, host: nm(p.host), node_id: p.node, sub_sector: nodeLabel(p.node),
        host_rank: p.rank, supply_from_new_capacity_usd: p.usd, share_of_node_demand: p.share, investor_lost_share: p.lostShare,
        friendshore: p.friendshore, partnership_opportunity: p.opportunity, increment: fdoc.investmentFlows?.variant.increment ?? null,
        ...(scn ?? {}),
      })),
      runMeta,
    ),
  });
  const noteLabel = id === 'managed' ? scenarioShockLabel(meta, 'managed') : shockLabel;
  return (
    <ChartCard
      headingLevel={headingLevel}
      title={title}
      badge={badge}
      sub={
        best.length
          ? `Investor to host, ${scope}${under}: the supply from new capacity at the best-ranked host of each sub-sector (USD a year), ${plural(best.length, 'flow')} worth ${fmtUsd(total)}. The investor is the economy whose lost supply the new capacity replaces. Band width is the supply.`
          : `Investor to host, ${scope}${under}: no simulated flow.`
      }
      legend={<Legend label="Flow" items={[{ label: 'Supply from new capacity, best-ranked host', color: INVEST_COLOR }]} />}
      summary={`Investor to host flows, ${scope}${under}: ${flows.slice(0, 5).map((x) => `${nm(x.producer)} in ${nm(x.consumer)} ${fmtUsd(x.value)}`).join('; ') || 'none'}.`}
      table={() => <TableView rows={pairs} cols={cols} caption={`Investor to host flows, ${scope}${under}${rank === 'all' ? ', every ranked host' : ''}`} search pageSize={20} initialSort={{ id: 'usd', desc: true }} />}
      csv={csv}
      notes={
        <>
          <span>
            Each ranked host is a separate simulation of the increment of capacity (capped at the host’s current output in the node); only the best-ranked host counts, so the alternatives are listed but never added up. Friendshore and
            Partnership Opportunity are partner scores at the NEIS defaults.{todayNote}
          </span>
          <ModelledNote className="mt-1">{variantPhrase(fdoc, noteLabel)}</ModelledNote>
        </>
      }
    >
      <FilterRow result={rank === 'all' ? `${plural(pairs.length, 'flow')} at ${maxRank ? `up to ${maxRank}` : 'every'} ranked hosts` : undefined}>
        <Segmented label="Hosts" value={rank} options={RANKS} onChange={setRank} />
        {sideChoice && filter.country && <Segmented label={nm(filter.country)} value={side} options={[...SIDES]} onChange={(v) => setSide(v as Side)} />}
      </FilterRow>
      {pairs.length ? (
        <div className="space-y-4">
          {flows.length > 0 && (
            <SankeyChart
              flows={flows}
              label={(code) => nm(code)}
              ariaLabel={`Investor to host flows, ${scope}${under}`}
              valueFormat={fmtUsd}
              linkColor={() => 'rgb(251 163 107 / 0.55)'}
              nodeColor={(_, s) => (s === 'producer' ? '#6b7280' : '#1889b0')}
              sideLabels={{ left: 'INVESTORS', right: 'HOSTS' }}
              tipNote="supply from new capacity a year, best-ranked host"
            />
          )}
          <div>
            <div className="mb-1 text-sm text-paper">{rank === 'all' ? 'Largest flows, every ranked host' : 'Largest flows'}</div>
            <FlowList pairs={pairs} name={nm} nodeLabel={nodeLabel} maxRank={maxRank} n={listN} />
            {rank === 'all' && <p className="mt-1 text-xs text-paper-mute">Hosts ranked below the best ({rankWords(2, maxRank)} and lower) are alternatives to it, not added supply.</p>}
          </div>
        </div>
      ) : (
        <EmptyState>
          No simulated investment flow for {scope}
          {under}:{' '}
          {id === 'managed'
            ? 'new capacity is simulated only for sub-sectors above their concentration benchmark whose gap after coordinated trade passes the investment threshold.'
            : 'only sub-sectors whose gap after coordinated trade passes the investment threshold are simulated.'}
        </EmptyState>
      )}
    </ChartCard>
  );
}
