// Allied Industrial Base partnerships /aib/partnerships (IA refactor, 2026-10-16; brief s.15): "Who should
// work with whom?" One toggle, Trade | Investment | Development (URL `mode`), and four filters, economy /
// technology / stage / sub-sector (URL `country`, `tech`, `stage`, `node`), over existing outputs only:
//   Trade        the model's coordinated trade relationships across the compacts (Sankey + table, the
//                existing compact-page chart), the specialized suppliers of the nodes, the Partnership
//                Opportunity (export market) top partners of the ties chunk and, behind a disclosure,
//                the paper's allied production network view (engine, NEIS baseline).
//   Investment   the nodes where simulated new capacity buys most resilience and their ranked eligible
//                hosts, any investor -> host flows the export carries, the Friendshore top partners and,
//                for one economy, the existing direct-investment and factory-project cards.
//   Development  the BUILD nodes (capacity gaps) and the Development Potential partners of the ties chunk
//                (the engine's cell list maps its sector ids; the one EngineScope of the page).
// Facts never respond to the Perspective; the rankings that use preference-weighted scores say so and
// are the NEIS baseline.

import { lazy, Suspense, useEffect, useMemo, useState, type ReactNode } from 'react';
import { Link, useSearchParams } from 'react-router';
import { ChartCard } from '../../charts/ChartCard.tsx';
import { Legend } from '../../charts/legends.tsx';
import { SankeyChart, type SankeyFlow } from '../../charts/Sankey.tsx';
import { TableView, type Col } from '../../components/TableView.tsx';
import { EmptyState, FilterRow, Loading, Notice, PageHead, Segmented, Select } from '../../components/ui.tsx';
import { cellLabel, stageSlug } from '../../lib/format.ts';
import { EngineScope } from '../../shell/EngineScope.tsx';
import { useTitle } from '../../shell/title.ts';
import { useApp } from '../../state/EngineProvider.tsx';
import { InvestmentCards } from '../../views/Partners/InvestmentCard.tsx';
import { loadSectorTop, loadTies, type Ties, type TopRow } from '../../views/Partners/ties.ts';
import { PAGE, useAibCtx } from '../AibLayout.tsx';
import type { AibNode } from '../contract.ts';
import { ClassChip } from '../charts/ClassChip.tsx';
import { fmtPp, fmtScore, fmtShare, fmtUsd, listWords, plural } from '../derive/format.ts';
import {
  buildGaps,
  dpRows,
  filterChoices,
  hostRows,
  investmentNodes,
  pairInvestmentFlows,
  pairTradeFlows,
  specializedSuppliers,
  topPairs,
  type DpRow,
  type HostRow,
  type PairInvestment,
  type PairTrade,
  type PartnershipFilter,
  type SupplierRow,
} from '../derive/partnerships.ts';
import { compactHref } from '../nav.ts';
import { csvWithRun } from './common.tsx';
import { REL_COLOR, REL_LINK, REL_WORDS } from './compact/NetworkSection.tsx';
import { AIB_EYEBROW } from './AibOverviewView.tsx';
import { BasisTag, LazyDisclosure } from './basis.tsx';

const NetworkView = lazy(() => import('../../views/Network/NetworkView.tsx'));

/** The query keys this page owns (kept when an embedded Strategic view writes the URL). */
export const PARTNERSHIP_KEYS = ['mode', 'country', 'tech', 'stage', 'node'] as const;

export type PartnershipMode = 'trade' | 'investment' | 'development';
const MODES: { value: PartnershipMode; label: string }[] = [
  { value: 'trade', label: 'Trade' },
  { value: 'investment', label: 'Investment' },
  { value: 'development', label: 'Development' },
];
const TOP = 15;

/** The page's mode from the URL (Trade by default). */
export const modeOf = (v: string | null): PartnershipMode => (v === 'investment' || v === 'development' ? v : 'trade');

function useTies(): { ties: Ties | null; error: string | null } | null {
  const [s, set] = useState<{ ties: Ties | null; error: string | null } | null>(null);
  useEffect(() => {
    let live = true;
    loadTies().then(
      (t) => live && set({ ties: t, error: t ? null : 'This data build has no partner rankings.' }),
      (e: Error) => live && set({ ties: null, error: `Could not load the partner rankings: ${e.message}` }),
    );
    return () => {
      live = false;
    };
  }, []);
  return s;
}

/** The baseline top partners of one sector (null while loading or when the build has none). */
function useSectorTop(tech: string | null, stage: string | null): { key: string; top: TopRow[] | null } | null {
  const key = tech && stage ? stageSlug(tech, stage) : '';
  const [s, set] = useState<{ key: string; top: TopRow[] | null } | null>(null);
  useEffect(() => {
    if (!tech || !stage) return;
    let live = true;
    loadSectorTop(tech, stage).then(
      (t) => live && set({ key, top: t?.top ?? null }),
      () => live && set({ key, top: null }),
    );
    return () => {
      live = false;
    };
  }, [key, tech, stage]);
  return key && s?.key === key ? s : null;
}

/** Names for any economy: the compacts document's, else the ties chunk's (it covers every scored economy). */
function useNamer(ties: Ties | null | undefined): (iso: string) => string {
  const { names } = useAibCtx();
  return useMemo(() => {
    const m = new Map<string, string>();
    if (ties) ties.U.forEach((u, i) => m.set(u, ties.U_names[i] ?? u));
    return (iso: string) => names[iso] ?? m.get(iso) ?? iso;
  }, [ties, names]);
}

function Block({ basis, children }: { basis: 'facts' | 'baseline'; children: ReactNode }) {
  return (
    <div className="space-y-3">
      <BasisTag basis={basis} />
      {children}
    </div>
  );
}

/** A short ranked list (the chart of a card whose twin is a table). */
function RankList<T>({ rows, label, render }: { rows: T[]; label: string; render: (r: T) => ReactNode }) {
  return (
    <ol className="divide-y divide-white/5 text-sm" aria-label={label}>
      {rows.map((r, i) => (
        <li key={i} className="flex flex-wrap items-baseline gap-x-3 gap-y-1 py-2">
          {render(r)}
        </li>
      ))}
    </ol>
  );
}

export default function AibPartnershipsView() {
  useTitle('Partnerships', 'aib');
  const { doc } = useAibCtx();
  const [params, setParams] = useSearchParams();
  const mode = modeOf(params.get('mode'));
  const raw: PartnershipFilter = { country: params.get('country'), tech: params.get('tech'), stage: params.get('stage'), node: params.get('node') };
  const all = useMemo(() => filterChoices(doc, { country: null, tech: null, stage: null, node: null }), [doc]);
  const tech = raw.tech && all.techs.includes(raw.tech) ? raw.tech : null;
  const stage = raw.stage && all.stages.includes(raw.stage) ? raw.stage : null;
  const country = raw.country && all.countries.some((c) => c.iso3 === raw.country) ? raw.country : null;
  const scopedNodes = useMemo(() => filterChoices(doc, { country: null, tech, stage, node: null }).nodes, [doc, tech, stage]);
  const node = raw.node && scopedNodes.some((n) => n.id === raw.node) ? raw.node : null;
  const f = useMemo<PartnershipFilter>(() => ({ country, tech, stage, node }), [country, tech, stage, node]);

  const set = (k: string, v: string) => {
    const q = new URLSearchParams(params);
    if (v && !(k === 'mode' && v === 'trade')) q.set(k, v);
    else q.delete(k);
    if (k === 'tech' || k === 'stage') q.delete('node');
    setParams(q, { replace: true, preventScrollReset: true });
  };

  return (
    <div className={PAGE}>
      <PageHead
        eyebrow={AIB_EYEBROW}
        title="Partnerships"
        lede="Who should work with whom? Trade shows the relationships the model adds to restore supply after the shock; Investment, where new capacity buys most resilience and which hosts are able to provide it; Development, the capacity gaps and the partners with the most development potential."
      />
      <FilterRow>
        <Segmented label="Partnership type" value={mode} options={MODES} onChange={(v) => set('mode', v)} />
        <Select
          label="Economy"
          value={country ?? ''}
          options={[{ value: '', label: 'All economies' }, ...all.countries.map((c) => ({ value: c.iso3, label: c.name }))]}
          onChange={(v) => set('country', v)}
        />
        <Select label="Technology" value={tech ?? ''} options={[{ value: '', label: 'All technologies' }, ...all.techs.map((t) => ({ value: t, label: t }))]} onChange={(v) => set('tech', v)} />
        <Select label="Stage" value={stage ?? ''} options={[{ value: '', label: 'All stages' }, ...all.stages.map((s) => ({ value: s, label: s }))]} onChange={(v) => set('stage', v)} className="min-w-[9rem]" />
        <Select
          label="Sub-sector"
          value={node ?? ''}
          options={[{ value: '', label: 'All sub-sectors' }, ...scopedNodes.map((n) => ({ value: n.id, label: n.label }))]}
          onChange={(v) => set('node', v)}
          className="min-w-[14rem]"
        />
      </FilterRow>
      <div className="space-y-10">
        {mode === 'trade' && <TradeTab f={f} />}
        {mode === 'investment' && <InvestmentTab f={f} />}
        {mode === 'development' && <DevelopmentTab f={f} />}
      </div>
    </div>
  );
}

/** "Japan, Solar, Midstream" or "all economies and sub-sectors". */
function useScope(f: PartnershipFilter): string {
  const { name, nodeOf } = useAibCtx();
  const parts = [f.country ? name(f.country) : null, f.node ? (nodeOf(f.node)?.label ?? f.node) : null, !f.node && f.tech ? f.tech : null, !f.node && f.stage ? f.stage : null].filter(Boolean);
  return parts.length ? parts.join(', ') : 'all economies and sub-sectors';
}

function TradeTab({ f }: { f: PartnershipFilter }) {
  const { doc, name, nodeOf } = useAibCtx();
  const scope = useScope(f);
  const label = (id: string) => nodeOf(id)?.label ?? id;
  const pairs = useMemo(() => pairTradeFlows(doc, f), [doc, f]);
  const suppliers = useMemo(() => specializedSuppliers(doc.nodes, f), [doc, f]);
  const total = pairs.reduce((s, p) => s + p.value, 0);
  const nNew = pairs.filter((p) => p.relationship === 'new').length;
  const cols: Col<PairTrade>[] = [
    { id: 'from', header: 'Exporter', value: (p) => name(p.producer), cell: (p) => <Link className="link" to={`/countries/${p.producer}#partnerships`}>{name(p.producer)}</Link> },
    { id: 'to', header: 'Importer', value: (p) => name(p.consumer), cell: (p) => <Link className="link" to={`/countries/${p.consumer}#partnerships`}>{name(p.consumer)}</Link> },
    { id: 'usd', header: 'Added trade', value: (p) => p.value, cell: (p) => fmtUsd(p.value), num: true },
    { id: 'contribution', header: 'Share of node demand served', value: (p) => p.share, cell: (p) => fmtShare(p.share), num: true, title: 'The largest share of one node’s demand the relationship serves in the coordinated solution' },
    { id: 'rel', header: 'Relationship', value: (p) => REL_WORDS[p.relationship] },
    { id: 'opp', header: 'Export Opportunity score', value: (p) => p.opportunity, cell: (p) => fmtScore(p.opportunity), num: true },
    { id: 'nodes', header: 'Sub-sectors', value: (p) => p.nodes.map(label).join(', '), minWidth: 200 },
    { id: 'compacts', header: 'Compacts', value: (p) => p.compacts.join(', '), cell: (p) => <CompactLinks ids={p.compacts} /> },
  ];
  const csv = () => ({
    name: `Allied Industrial Base trade relationships ${scope}`,
    text: csvWithRun(
      pairs.map((p) => ({
        exporter_iso3: p.producer, exporter: name(p.producer), importer_iso3: p.consumer, importer: name(p.consumer), added_trade_usd: p.value,
        largest_share_of_node_demand: p.share, relationship: p.relationship, mean_export_opportunity: p.opportunity, sub_sectors: p.nodes.map(label).join('; '), compacts: p.compacts.join('; '),
      })),
      doc.meta,
    ),
  });
  const supCols: Col<SupplierRow>[] = [
    { id: 'name', header: 'Economy', value: (r) => name(r.iso3), cell: (r) => <Link className="link" to={`/countries/${r.iso3}#aib-role`}>{name(r.iso3)}</Link> },
    { id: 'n', header: 'Sub-sectors where it is the largest supplier', value: (r) => r.nodes.length, num: true },
    { id: 'top', header: 'Its largest share', value: (r) => r.top, cell: (r) => fmtShare(r.top), num: true },
    { id: 'nodes', header: 'Sub-sectors', value: (r) => r.nodes.map(label).join(', '), minWidth: 240 },
  ];
  return (
    <>
      <Block basis="facts">
        <ChartCard
          headingLevel={2}
          title="Coordinated trade relationships"
          sub={`Exporter to importer, ${scope}: the ${Math.min(TOP, pairs.length)} largest of ${plural(pairs.length, 'relationship')} the model adds or expands to restore supply after the shock (${fmtUsd(total)} a year${nNew ? `; ${nNew} new` : ''}). Band width is the added trade.`}
          legend={<Legend label="Relationship" items={[{ label: REL_WORDS.new, color: REL_COLOR.new }, { label: REL_WORDS.expanded, color: REL_COLOR.expanded }]} />}
          summary={`Trade relationships, ${scope}: ${pairs.slice(0, 5).map((p) => `${name(p.producer)} to ${name(p.consumer)} ${fmtUsd(p.value)}`).join('; ') || 'none'}.`}
          table={() => <TableView rows={pairs} cols={cols} caption={`Coordinated trade relationships, ${scope}`} search pageSize={20} initialSort={{ id: 'usd', desc: true }} />}
          csv={csv}
          notes={<span>Each compact&apos;s export keeps its largest relationships; the Export Opportunity score is the partner score the model uses to break ties among equally good solutions.</span>}
        >
          {pairs.length ? (
            <SankeyChart
              flows={pairs.slice(0, TOP)}
              label={(code) => name(code)}
              ariaLabel={`Coordinated trade relationships, ${scope}, exporter to importer`}
              valueFormat={fmtUsd}
              linkColor={(fl: SankeyFlow) => REL_LINK[(fl as PairTrade).relationship] ?? REL_LINK.expanded}
              nodeColor={(_, side) => (side === 'producer' ? '#1889b0' : '#6b7280')}
              sideLabels={{ left: 'EXPORTERS', right: 'IMPORTERS' }}
              tipNote="added trade a year, coordinated solution"
            />
          ) : (
            <EmptyState>No added trade relationship for {scope}: existing shipments carry the response, or no compact covers this selection.</EmptyState>
          )}
        </ChartCard>
        <ChartCard
          headingLevel={2}
          title="Specialized suppliers"
          sub={`Who the coordinated solution leans on, ${scope}: the economies that are the largest supplier of a sub-sector after the shock, with their largest share.`}
          table={() => <TableView rows={suppliers} cols={supCols} caption={`Specialized suppliers, ${scope}`} search pageSize={20} />}
        >
          {suppliers.length ? (
            <RankList
              rows={suppliers.slice(0, 8)}
              label="Largest suppliers of the coordinated solution"
              render={(r) => (
                <>
                  <Link className="link min-w-[9rem]" to={`/countries/${r.iso3}#aib-role`}>
                    {name(r.iso3)}
                  </Link>
                  <span className="text-paper-dim">
                    {plural(r.nodes.length, 'sub-sector')} (up to {fmtShare(r.top, 0)}): {listWords(r.nodes.slice(0, 3).map(label))}
                    {r.nodes.length > 3 ? ' and more' : ''}
                  </span>
                </>
              )}
            />
          ) : (
            <EmptyState>No optimized supplier shares for {scope}.</EmptyState>
          )}
        </ChartCard>
      </Block>
      <Block basis="baseline">
        <TopPartnersCard side="po" f={f} />
      </Block>
      <LazyDisclosure summary="The paper's allied production network (a design that responds to preferences; shown at the NEIS baseline)">
        {() => (
          <EngineScope hostKeys={PARTNERSHIP_KEYS}>
            <Suspense fallback={<Loading label="Loading the allied network" />}>
              <NetworkView />
            </Suspense>
          </EngineScope>
        )}
      </LazyDisclosure>
    </>
  );
}

function CompactLinks({ ids }: { ids: string[] }) {
  return (
    <>
      {ids.map((id, i) => (
        <span key={id}>
          {i > 0 && ', '}
          <Link className="link" to={compactHref(id)}>
            {id}
          </Link>
        </span>
      ))}
    </>
  );
}

const SIDE_WORDS = {
  po: { title: 'Export opportunity', score: 'Partnership Opportunity score', who: 'Exporter', partner: 'Market', what: 'how attractive each partner is as an export market' },
  fs: { title: 'Friendshore partners', score: 'Friendshore score', who: 'Economy', partner: 'Partner to build with', what: 'how attractive each partner is for friendshoring production and investment' },
} as const;

/** The baseline top partners (Partnership Opportunity or Friendshore) of the ties chunk, scoped. */
function TopPartnersCard({ side, f }: { side: 'po' | 'fs'; f: PartnershipFilter }) {
  const ties = useTies();
  const name = useNamer(ties?.ties);
  const sector = useSectorTop(f.tech, f.stage);
  const w = SIDE_WORDS[side];
  const bySector = !!(f.tech && f.stage && sector?.top);
  const rows = useMemo(() => {
    const src = bySector ? (sector?.top ?? []) : (ties?.ties?.top_all ?? []);
    return topPairs(src, side, f.country, TOP);
  }, [bySector, sector, ties, side, f.country]);
  const where = bySector ? cellLabel(f.tech as string, f.stage as string) : 'all sectors';
  const cols: Col<TopRow>[] = [
    { id: 'who', header: w.who, value: (r) => name(r.reporter_iso), cell: (r) => <Link className="link" to={`/countries/${r.reporter_iso}#partnerships`}>{name(r.reporter_iso)}</Link> },
    { id: 'partner', header: w.partner, value: (r) => name(r.partner_iso), cell: (r) => <Link className="link" to={`/countries/${r.partner_iso}#partnerships`}>{name(r.partner_iso)}</Link> },
    { id: 'value', header: w.score, value: (r) => r.value, cell: (r) => fmtScore(r.value), num: true },
    { id: 'rank', header: 'Rank among its partners', value: (r) => r.rank, num: true },
  ];
  const body = !ties ? (
    <Loading label="Loading the partner rankings" />
  ) : ties.error && !bySector ? (
    <Notice kind="info">{ties.error}</Notice>
  ) : rows.length ? (
    <RankList
      rows={rows.slice(0, 8)}
      label={`${w.title}: the strongest pairs`}
      render={(r) => (
        <>
          <span className="w-12 shrink-0 text-right font-medium tabular text-paper">{fmtScore(r.value)}</span>
          <span className="text-paper-dim">
            {name(r.reporter_iso)} &rarr; {name(r.partner_iso)}
          </span>
        </>
      )}
    />
  ) : (
    <EmptyState>No ranked partners for this selection.</EmptyState>
  );
  return (
    <ChartCard
      headingLevel={2}
      title={w.title}
      sub={`The strongest pairs by ${w.score} (${w.what}), ${where}${f.country ? `, ${name(f.country)}'s own ranking` : ''}. A preference-weighted score: a Country profile can change the Perspective; here it is the NEIS baseline.${f.tech && f.stage && !bySector && sector !== null ? ' No sector-level ranking for this selection: all sectors shown.' : ''}`}
      table={() => <TableView rows={rows} cols={cols} caption={`${w.title}, ${where}`} pageSize={20} />}
    >
      {body}
    </ChartCard>
  );
}

function InvestmentTab({ f }: { f: PartnershipFilter }) {
  const { doc, name, nodeOf } = useAibCtx();
  const scope = useScope(f);
  const label = (id: string) => nodeOf(id)?.label ?? id;
  const nodes = useMemo(() => investmentNodes(doc.nodes, f), [doc, f]);
  const hosts = useMemo(() => hostRows(doc.nodes, f), [doc, f]);
  const flows = useMemo(() => pairInvestmentFlows(doc, f), [doc, f]);
  const ties = useTies();
  const anyName = useNamer(ties?.ties);
  const nodeCols: Col<AibNode>[] = [
    { id: 'node', header: 'Sub-sector', value: (n) => n.label },
    { id: 'stage', header: 'Technology and stage', value: (n) => `${n.techs.join(', ')} · ${n.stage}` },
    { id: 'cls', header: 'Treatment', value: (n) => n.cls, cell: (n) => <ClassChip cls={n.cls} /> },
    { id: 'residual', header: 'Residual gap after trade', value: (n) => n.residual, cell: (n) => fmtShare(n.residual), num: true },
    { id: 'gain', header: 'Resilience gain of the best host', value: (n) => n.bestGain, cell: (n) => fmtPp(n.bestGain), num: true },
    { id: 'hosts', header: 'Eligible hosts, best first', value: (n) => n.hosts.map((h) => name(h)).join(', '), minWidth: 220 },
    { id: 'compact', header: 'Compact', value: (n) => n.compact, cell: (n) => (n.compact ? <CompactLinks ids={[n.compact]} /> : 'none') },
  ];
  const hostCols: Col<HostRow>[] = [
    { id: 'name', header: 'Host', value: (r) => name(r.iso3), cell: (r) => <Link className="link" to={`/countries/${r.iso3}#partnerships`}>{name(r.iso3)}</Link> },
    { id: 'first', header: 'Sub-sectors where it is the best host', value: (r) => r.first.length, num: true },
    { id: 'eligible', header: 'Sub-sectors where it is eligible', value: (r) => r.eligible.length, num: true },
    { id: 'nodes', header: 'Sub-sectors', value: (r) => r.eligible.map(label).join(', '), minWidth: 240 },
  ];
  const flowCols: Col<PairInvestment>[] = [
    { id: 'investor', header: 'Investor', value: (p) => name(p.producer) },
    { id: 'host', header: 'Host', value: (p) => name(p.consumer) },
    { id: 'usd', header: 'Supply from new capacity', value: (p) => p.value, cell: (p) => fmtUsd(p.value), num: true },
    { id: 'fs', header: 'Friendshore score', value: (p) => p.friendshore, cell: (p) => fmtScore(p.friendshore), num: true },
    { id: 'nodes', header: 'Sub-sectors', value: (p) => p.nodes.map(label).join(', ') },
  ];
  const csv = () => ({
    name: `Allied Industrial Base investment hosts ${scope}`,
    text: csvWithRun(
      nodes.map((n) => ({ node_id: n.id, sub_sector: n.label, technologies: n.techs.join('; '), stage: n.stage, classification: n.cls, residual_gap: n.residual, best_host_resilience_gain: n.bestGain, eligible_hosts_best_first: n.hosts.join('; '), compact: n.compact })),
      doc.meta,
    ),
  });
  const sector = f.tech && f.stage ? { tech: f.tech, stage: f.stage } : null;
  return (
    <>
      <Block basis="facts">
        <ChartCard
          headingLevel={2}
          title="Where new capacity buys most resilience"
          sub={`Where a gap remains after coordinated trade, the model adds a standardized increment of capacity at each eligible host in turn and ranks the hosts by the resilience it buys. ${scope.charAt(0).toUpperCase()}${scope.slice(1)}: ${plural(nodes.length, 'sub-sector')}, largest gain first. A host must already show the capability (revealed by its exports).`}
          table={() => <TableView rows={nodes} cols={nodeCols} caption={`Investment hosts by sub-sector, ${scope}`} search pageSize={20} initialSort={{ id: 'gain', desc: true }} />}
          csv={csv}
          notes={<span>The increment is a synthetic simulation, not a project or a cost estimate; a separate diversification gain is not in this export (the frontier on each compact page shows what diversification costs).</span>}
        >
          {nodes.length ? (
            <RankList
              rows={nodes.slice(0, 8)}
              label="Sub-sectors where new capacity buys most resilience"
              render={(n) => (
                <>
                  <span className="w-14 shrink-0 text-right font-medium tabular text-paper">{fmtPp(n.bestGain, 0)}</span>
                  <span className="min-w-0 flex-1 text-paper-dim">
                    <span className="text-paper">{n.label}</span> ({n.techs.join(', ')} · {n.stage}): best hosts {listWords(n.hosts.slice(0, 3).map((h) => name(h)))}
                  </span>
                  <ClassChip cls={n.cls} />
                </>
              )}
            />
          ) : (
            <EmptyState>No simulated investment for {scope}: only sub-sectors whose gap after coordinated trade passes the investment threshold are simulated.</EmptyState>
          )}
        </ChartCard>
        <ChartCard
          headingLevel={2}
          title="Investment hosts"
          sub={`Host capability, ${scope}: how often each economy is the best-ranked or an eligible host for new capacity.`}
          table={() => <TableView rows={hosts} cols={hostCols} caption={`Investment hosts, ${scope}`} search pageSize={20} />}
        >
          {hosts.length ? (
            <RankList
              rows={hosts.slice(0, 8)}
              label="Most frequent investment hosts"
              render={(r) => (
                <>
                  <Link className="link min-w-[9rem]" to={`/countries/${r.iso3}#partnerships`}>
                    {name(r.iso3)}
                  </Link>
                  <span className="text-paper-dim">
                    best host in {plural(r.first.length, 'sub-sector')}, eligible in {r.eligible.length}
                  </span>
                </>
              )}
            />
          ) : (
            <EmptyState>No eligible host for {scope}.</EmptyState>
          )}
        </ChartCard>
        {flows.length > 0 && (
          <ChartCard
            headingLevel={2}
            title="Investment relationships"
            sub={`Investor to host, ${scope}: supply from the simulated new capacity (USD a year).`}
            table={() => <TableView rows={flows} cols={flowCols} caption={`Investment relationships, ${scope}`} search pageSize={20} initialSort={{ id: 'usd', desc: true }} />}
          >
            <SankeyChart
              flows={flows.slice(0, TOP)}
              label={(code) => name(code)}
              ariaLabel={`Investment relationships, ${scope}, investor to host`}
              valueFormat={fmtUsd}
              sideLabels={{ left: 'INVESTORS', right: 'HOSTS' }}
              tipNote="supply from the simulated new capacity a year"
            />
          </ChartCard>
        )}
        {f.country ? (
          <InvestmentCards iso={f.country} name={name(f.country)} ties={ties?.ties ?? null} error={ties?.error ?? null} sector={sector} nameOf={anyName} headingLevel={2} />
        ) : (
          <p className="text-sm text-paper-mute">Choose an economy to see its direct-investment positions (IMF) and factory projects.</p>
        )}
      </Block>
      <Block basis="baseline">
        <TopPartnersCard side="fs" f={f} />
      </Block>
    </>
  );
}

function DevelopmentTab({ f }: { f: PartnershipFilter }) {
  const { doc, name } = useAibCtx();
  const scope = useScope(f);
  const gaps = useMemo(() => buildGaps(doc.nodes, f), [doc, f]);
  const cols: Col<AibNode>[] = [
    { id: 'node', header: 'Sub-sector', value: (n) => n.label },
    { id: 'stage', header: 'Technology and stage', value: (n) => `${n.techs.join(', ')} · ${n.stage}` },
    { id: 'residual', header: 'Residual gap after trade', value: (n) => n.residual, cell: (n) => fmtShare(n.residual), num: true },
    { id: 'lost', header: 'Lost supply', value: (n) => n.lostUsd, cell: (n) => fmtUsd(n.lostUsd), num: true },
    { id: 'hosts', header: 'Eligible hosts, best first', value: (n) => n.hosts.map((h) => name(h)).join(', '), minWidth: 220 },
    { id: 'compact', header: 'Compact', value: (n) => n.compact, cell: (n) => (n.compact ? <CompactLinks ids={[n.compact]} /> : 'none') },
  ];
  const csv = () => ({
    name: `Allied Industrial Base capacity gaps ${scope}`,
    text: csvWithRun(
      gaps.map((n) => ({ node_id: n.id, sub_sector: n.label, technologies: n.techs.join('; '), stage: n.stage, residual_gap: n.residual, lost_usd: n.lostUsd, eligible_hosts_best_first: n.hosts.join('; '), compact: n.compact })),
      doc.meta,
    ),
  });
  return (
    <>
      <Block basis="facts">
        <ChartCard
          headingLevel={2}
          title="Capacity gaps"
          sub={`The BUILD sub-sectors, ${scope}: even coordinated trade leaves a large share of demand unserved, so new capacity is needed. Largest residual gap first, with the hosts able to provide it.`}
          table={() => <TableView rows={gaps} cols={cols} caption={`Capacity gaps, ${scope}`} search pageSize={20} initialSort={{ id: 'residual', desc: true }} />}
          csv={csv}
        >
          {gaps.length ? (
            <RankList
              rows={gaps.slice(0, 8)}
              label="Largest capacity gaps"
              render={(n) => (
                <>
                  <span className="w-14 shrink-0 text-right font-medium tabular text-paper">{fmtShare(n.residual, 0)}</span>
                  <span className="min-w-0 flex-1 text-paper-dim">
                    <span className="text-paper">{n.label}</span> ({n.techs.join(', ')} · {n.stage}, {fmtUsd(n.lostUsd)} lost)
                    {n.hosts.length ? `: hosts ${listWords(n.hosts.slice(0, 3).map((h) => name(h)))}` : ''}
                  </span>
                </>
              )}
            />
          ) : (
            <EmptyState>No BUILD sub-sector for {scope}.</EmptyState>
          )}
        </ChartCard>
      </Block>
      <Block basis="baseline">
        <EngineScope hostKeys={PARTNERSHIP_KEYS} fallback={<Loading label="Loading the Development Potential scores" />}>
          <DevelopmentPartners f={f} scope={scope} />
        </EngineScope>
      </Block>
    </>
  );
}

function DevelopmentPartners({ f, scope }: { f: PartnershipFilter; scope: string }) {
  const app = useApp();
  const { doc, nodeOf } = useAibCtx();
  const ties = useTies();
  const name = useNamer(ties?.ties);
  const label = (id: string) => nodeOf(id)?.label ?? id;
  const rows = useMemo(() => (ties?.ties ? dpRows(ties.ties.dp, app.cells, doc.nodes, f) : []), [ties, app.cells, doc.nodes, f]);
  const cols: Col<DpRow>[] = [
    { id: 'name', header: 'Economy', value: (r) => name(r.iso3), cell: (r) => <Link className="link" to={`/countries/${r.iso3}#partnerships`}>{name(r.iso3)}</Link> },
    { id: 'value', header: 'Development Potential', value: (r) => r.value, cell: (r) => fmtScore(r.value), num: true },
    { id: 'sector', header: 'Highest in', value: (r) => cellLabel(r.tech, r.stage) },
    { id: 'host', header: 'Capacity gaps where it is an eligible host', value: (r) => r.hostIn.map(label).join(', '), minWidth: 220 },
  ];
  return (
    <ChartCard
      headingLevel={2}
      title="Development partners"
      sub={`The economies with the most Development Potential (where partnering builds capability in a middle- or lower-income economy), ${scope}; each with the capacity gaps where it is already an eligible host. A preference-weighted ranking at the NEIS baseline.`}
      table={() => <TableView rows={rows} cols={cols} caption={`Development partners, ${scope}`} search pageSize={20} />}
    >
      {!ties ? (
        <Loading label="Loading the Development Potential scores" />
      ) : ties.error ? (
        <Notice kind="info">{ties.error}</Notice>
      ) : rows.length ? (
        <RankList
          rows={rows.slice(0, 10)}
          label="Economies with the most Development Potential"
          render={(r) => (
            <>
              <span className="w-12 shrink-0 text-right font-medium tabular text-paper">{fmtScore(r.value)}</span>
              <Link className="link" to={`/countries/${r.iso3}#partnerships`}>
                {name(r.iso3)}
              </Link>
              <span className="text-paper-mute">
                {cellLabel(r.tech, r.stage)}
                {r.hostIn.length ? `; eligible host for ${listWords(r.hostIn.slice(0, 2).map(label))}` : ''}
              </span>
            </>
          )}
        />
      ) : (
        <EmptyState>No Development Potential scores for {scope}.</EmptyState>
      )}
    </ChartCard>
  );
}
