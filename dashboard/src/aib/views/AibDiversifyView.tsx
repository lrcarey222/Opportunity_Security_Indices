// Allied Industrial Base diversification /aib/diversification (2026-10-19): "What would trade and investment
// look like if each step of the supply chain were as diversified as its wider sector?" The scenario
// "Diversified to its sector's concentration" (meta.diversify, node.diversify; derive/diversify.ts): the
// question, concentration (HHI) in plain words and the benchmark rule; the totals (nodes above their
// benchmark by more than a small tolerance of those with one, the supply to shift in USD and as a share of
// the allied demand in those nodes, what trade restores, what is left for new capacity, the most-capped
// suppliers); the chart of the nodes where the scenario ran, largest capacity gap first (current vs
// benchmark concentration on one HHI axis; the allied demand split to scale: kept / restored by trade / new
// capacity), with its table twin; and the table of every node (filters by index and sector, URL `index` and
// `sector`, shared with the chart; `all=1` includes the nodes the scenario did not diversify, shown with
// their status and without the run columns; the class under the scenario with its physical-capability
// flag), CSV (shock: the scenario's id). Each node links
// to its compact (#nodes) and to the Sector Profiles of its three-stage OSI cells. Modelled facts:
// nothing follows the Perspective. An older bundle without the scenario says so.
//
// The Managed Interdependence explainer (2026-10-19, page work A): the scenario toggle's Managed
// Interdependence is this scenario, so the page is titled after it (nav "Managed Interdependence") and
// explains it whichever scenario is chosen: the benchmarks, the caps and the capped suppliers. Its totals
// are the toggle's numbers: the supply to shift, what trade restores and what is left for new capacity
// equal the Managed Interdependence row of the Overview's comparison (scenarioTotals; node.diversify.lostUsd
// where the export has it), which the page shows beside Baseline and Decoupling. Links carry the toggle.

import { useMemo, useState, type ReactNode } from 'react';
import { Link, useSearchParams } from 'react-router';
import { ChartCard } from '../../charts/ChartCard.tsx';
import { Legend } from '../../charts/legends.tsx';
import { TableView, type Col } from '../../components/TableView.tsx';
import { EmptyState, FilterRow, PageHead, Select, StatTile, Switch } from '../../components/ui.tsx';
import { DASH } from '../../lib/format.ts';
import { useTitle } from '../../shell/title.ts';
import { PAGE, useAibCtx } from '../AibLayout.tsx';
import { diversifyShockIdOf, type AibNode } from '../contract.ts';
import { DIVERSIFY_MARK, DiversifyChart, SPLIT_WORDS } from '../charts/DiversifyChart.tsx';
import { FlaggedClass } from '../charts/ClassChip.tsx';
import { ScenarioCoverageCompare } from '../charts/ScenarioCompare.tsx';
import {
  basisWords, cappedWords, diversifyClassText, diversifyCsvRow, diversifyIndices, diversifyLabel, diversifyRows, diversifySectorOptions, diversifyTotals, filterDiversify, hasDiversify, hhiAxisMax,
  orderDiversify, relationshipWords, rowStatusLabel, totalsWords, type DiversifyRow,
} from '../derive/diversify.ts';
import { fmtScore, fmtShare, fmtUsd, nameOf, plural } from '../derive/format.ts';
import { DIVERSIFY_BENCHMARK_WORDS, diversifySetting, HHI_SHORT, strategicScopeShort } from '../derive/settings.ts';
import { overviewRows } from '../derive/scenarioPages.ts';
import { scenarioCaveat, scenarioInfo } from '../derive/scenarios.ts';
import { indexLabel, nodeWhere, profiledCellsOfNode } from '../derive/taxonomy.ts';
import { compactHref } from '../nav.ts';
import { AIB_EYEBROW } from './AibOverviewView.tsx';
import { BasisTag } from './basis.tsx';
import { csvWithRun, Section } from './common.tsx';
import { ModelledNote } from './networkParts.tsx';

/** The chart shows this many nodes until "Show all". */
export const CHART_ROWS = 15;

/** A node's place, its compact (node section) and the Sector Profiles of the OSI cells its codes sit in. */
function NodePlace({ n }: { n: AibNode }) {
  const { tx, compactOf, title, href } = useAibCtx();
  const c = compactOf(n.compact);
  const cells = profiledCellsOfNode(n).slice(0, 2);
  return (
    <>
      {nodeWhere(tx, n)}
      {n.compact && (
        <>
          {' · '}
          <Link className="link" to={href(`${compactHref(n.compact)}#nodes`)} title={c ? `Sectoral Compact: ${title(c)}` : undefined}>
            {n.compact}
          </Link>
        </>
      )}
      {cells.length > 0 && (
        <>
          {' · Sector profile (the paper’s three stages): '}
          {cells.map((cell, i) => (
            <span key={cell.href}>
              {i > 0 && ', '}
              <Link className="link" to={cell.href}>
                {cell.label}
              </Link>
            </span>
          ))}
        </>
      )}
    </>
  );
}

/** The explainer's title: the scenario's label (Managed Interdependence). */
export const MI_PAGE_TITLE = 'Managed Interdependence';

export default function AibDiversifyView() {
  useTitle(MI_PAGE_TITLE, 'aib');
  const { doc, names, tx, href, scenario, hasScenarios } = useAibCtx();
  const m = doc.meta;
  const [params, setParams] = useSearchParams();
  const [showAll, setShowAll] = useState(false);
  const present = hasDiversify(doc);
  const setting = diversifySetting(m);
  const label = diversifyLabel(m);
  const shockId = diversifyShockIdOf(m);
  const rows = useMemo(() => diversifyRows(doc.nodes, shockId), [doc, shockId]);
  const totals = useMemo(() => diversifyTotals(doc.nodes), [doc]);
  // the three scenarios as the Overview compares them: this page's totals are the Managed Interdependence row
  const three = useMemo(() => (hasScenarios ? overviewRows(doc.nodes, m) : []), [doc, m, hasScenarios]);
  const managedRow = three.find((r) => r.id === 'managed') ?? null;
  const miLabel = scenarioInfo(m, 'managed').label;
  const indices = useMemo(() => diversifyIndices(rows, tx), [rows, tx]);
  const rawIndex = params.get('index');
  const index = rawIndex && indices.some((ix) => ix.code === rawIndex) ? rawIndex : null;
  const sectorOpts = useMemo(() => diversifySectorOptions(rows, tx, index), [rows, tx, index]);
  const rawSector = params.get('sector');
  const sector = rawSector && sectorOpts.some((o) => o.value === rawSector) ? rawSector : null;
  const all = params.get('all') === '1';
  const ran = useMemo(() => orderDiversify(filterDiversify(rows, { index, sector, all: false }, tx)), [rows, index, sector, tx]);
  const table = useMemo(() => orderDiversify(filterDiversify(rows, { index, sector, all }, tx)), [rows, index, sector, all, tx]);
  const max = useMemo(() => hhiAxisMax(ran), [ran]);
  const shown = showAll ? ran : ran.slice(0, CHART_ROWS);
  const setParam = (k: string, v: string | null) => {
    const q = new URLSearchParams(params);
    if (v) q.set(k, v);
    else q.delete(k);
    // a sector outside the chosen index no longer applies
    if (k === 'index') q.delete('sector');
    setParams(q, { replace: true, preventScrollReset: true });
  };
  const name = (iso3: string) => nameOf(names, iso3);
  // the CSV's run columns: the scenario's own shock id, also when only the nodes carry the scenario
  const runMeta = { ...m, headlineShock: shockId };
  const scope = `${index ? ` in ${indexLabel(indices.find((ix) => ix.code === index)!)}` : ''}${sector ? ` in ${sector}` : ''}`;
  const csvOf = (rs: DiversifyRow[], what: string) => () => ({
    name: `Allied Industrial Base diversification ${what}${index ? ` index ${index}` : ''}${sector ? ` ${sector}` : ''}`,
    text: csvWithRun(
      rs.map((r) => diversifyCsvRow(r, names, tx)),
      runMeta,
    ),
  });
  const pct = (x: number | null | undefined) => fmtShare(x, 1);
  // a run column: its figure where the scenario ran, a dash elsewhere
  const run = (r: DiversifyRow, s: string) => (r.ran ? s : DASH);
  const chartCols: Col<DiversifyRow>[] = [
    { id: 'node', header: 'Node', value: (r) => r.node.label, minWidth: 180 },
    { id: 'current', header: 'Current concentration (HHI)', value: (r) => r.d.currentHhi, cell: (r) => fmtScore(r.d.currentHhi), num: true },
    { id: 'benchmark', header: 'Benchmark concentration (HHI)', value: (r) => r.d.benchmarkHhi, cell: (r) => fmtScore(r.d.benchmarkHhi), num: true },
    { id: 'cap', header: 'Supplier cap', value: (r) => r.d.cap, cell: (r) => fmtShare(r.d.cap, 0), num: true },
    { id: 'kept', header: SPLIT_WORDS.kept, value: (r) => r.split?.kept ?? null, cell: (r) => pct(r.split?.kept), num: true },
    { id: 'trade', header: SPLIT_WORDS.trade, value: (r) => r.split?.trade ?? null, cell: (r) => pct(r.split?.trade), num: true },
    { id: 'build', header: SPLIT_WORDS.build, value: (r) => r.split?.build ?? null, cell: (r) => pct(r.split?.build), num: true },
    { id: 'gapUsd', header: 'Capacity gap (USD)', value: (r) => r.gapUsd, cell: (r) => fmtUsd(r.gapUsd), num: true },
  ];
  const tableCols: Col<DiversifyRow>[] = [
    { id: 'node', header: 'Node', value: (r) => r.node.label, cell: (r) => <span className="text-paper">{r.node.label}</span>, minWidth: 180 },
    {
      id: 'where',
      header: tx.dictionary ? 'Index › sector · stage' : `${tx.Many} · stage`,
      value: (r) => nodeWhere(tx, r.node),
      cell: (r) => (
        <span className="text-xs">
          <NodePlace n={r.node} />
        </span>
      ),
      minWidth: 260,
    },
    { id: 'status', header: 'Status', value: (r) => rowStatusLabel(r), minWidth: 180 },
    { id: 'current', header: 'Current concentration (HHI)', value: (r) => r.d.currentHhi, cell: (r) => fmtScore(r.d.currentHhi), num: true },
    { id: 'benchmark', header: 'Benchmark concentration (HHI)', value: (r) => r.d.benchmarkHhi, cell: (r) => fmtScore(r.d.benchmarkHhi), num: true },
    { id: 'basis', header: 'Benchmark', value: (r) => (r.d.basis ? basisWords(r.d.basis) : null), minWidth: 200 },
    { id: 'cap', header: 'Supplier cap', value: (r) => r.d.cap, cell: (r) => fmtShare(r.d.cap, 0), num: true },
    { id: 'capped', header: 'Capped suppliers', value: (r) => (r.ran ? r.d.capped.map(name).join(', ') || 'none' : null), minWidth: 160 },
    { id: 'shift', header: 'Supply to shift (share of allied demand)', value: (r) => (r.ran ? r.d.excess : null), cell: (r) => run(r, pct(r.d.excess)), num: true },
    { id: 'shiftUsd', header: 'Supply to shift (USD)', value: (r) => (r.ran ? r.shiftUsd : null), cell: (r) => run(r, fmtUsd(r.shiftUsd)), num: true },
    { id: 'existing', header: 'Coverage after existing relationships', value: (r) => (r.ran ? r.d.existing : null), cell: (r) => run(r, pct(r.d.existing)), num: true },
    { id: 'optimized', header: 'Coverage after optimized trade', value: (r) => (r.ran ? r.d.optimized : null), cell: (r) => run(r, pct(r.d.optimized)), num: true },
    { id: 'gap', header: 'Capacity gap (share of allied demand)', value: (r) => (r.ran ? r.d.residual : null), cell: (r) => run(r, pct(r.d.residual)), num: true },
    { id: 'gapUsd', header: 'Capacity gap (USD)', value: (r) => (r.ran ? r.gapUsd : null), cell: (r) => run(r, fmtUsd(r.gapUsd)), num: true },
    {
      id: 'cls',
      header: 'Class under the scenario',
      value: (r) => (r.d.cls ? diversifyClassText(r.d.cls, r.clsWarning) : null),
      cell: (r) => <FlaggedClass cls={r.d.cls ?? null} warning={r.clsWarning} />,
      minWidth: 160,
    },
    { id: 'hosts', header: 'Leading hosts', value: (r) => (r.ran ? r.d.hosts.map(name).join(', ') || 'none ranked' : null), minWidth: 160 },
    { id: 'partners', header: 'Largest new or expanded relationships', value: (r) => (r.ran ? relationshipWords(r.d.partners, names) || 'none' : null), minWidth: 220 },
  ];

  const head = (
    <PageHead
      eyebrow={AIB_EYEBROW}
      title={MI_PAGE_TITLE}
      lede={`How ${miLabel} works. What would trade and investment look like if each step of the supply chain were as diversified as its wider sector? This counterfactual removes no supplier: it caps the share any one supplier may hold of the allied market where a node is more concentrated than comparable goods, and asks how much supply has to shift, how much trade can replace, and how much new capacity diversification needs.`}
    >
      <p className="mt-3 max-w-3xl text-sm text-paper-mute" data-testid="diversify-rule">
        {present ? (
          <>
            {label}: each node&apos;s concentration of world exporters (HHI: {HHI_SHORT}) is compared with {setting ? setting.words : DIVERSIFY_BENCHMARK_WORDS.hs4.words}. A
            node more concentrated than its benchmark, by more than a small tolerance, gets the supplier-share cap that would bring its concentration down to the benchmark, applied to
            the allied market. A counterfactual market structure, not a forecast; dollar values are {m.tradeYear} trade values.{' '}
          </>
        ) : null}
        <Link className="link" to={href('/compacts/about#diversification')}>
          How the scenario works &rarr;
        </Link>
      </p>
      {hasScenarios && scenario !== 'managed' && (
        <p className="mt-2 max-w-3xl text-sm text-paper-mute" data-testid="explainer-note">
          This page explains {miLabel} whichever scenario is chosen above; the other pages show {scenarioInfo(m, scenario).label} until you switch.
        </p>
      )}
    </PageHead>
  );

  if (!present) {
    return (
      <div className={PAGE}>
        {head}
        <EmptyState>This data build has no diversification scenario. It appears once the model has run the scenario &ldquo;{label}&rdquo; and the data are exported again.</EmptyState>
      </div>
    );
  }

  const notAssessed = totals.nodes - totals.assessed;
  const legend = (
    <Legend
      label="Chart marks"
      items={[
        { label: 'Current concentration', color: DIVERSIFY_MARK.current, kind: 'dot' },
        { label: 'Benchmark concentration', color: DIVERSIFY_MARK.benchmark, kind: 'hollow' },
        { label: SPLIT_WORDS.kept, color: DIVERSIFY_MARK.kept, kind: 'rect' },
        { label: SPLIT_WORDS.trade, color: DIVERSIFY_MARK.trade, kind: 'rect' },
        { label: SPLIT_WORDS.build, color: DIVERSIFY_MARK.build, kind: 'rect' },
      ]}
    />
  );
  const filters: ReactNode = (
    <FilterRow result={`${plural(ran.length, 'node')} diversified${scope}`}>
      {indices.length > 0 && (
        <Select
          label="Index"
          value={index ?? ''}
          options={[{ value: '', label: 'All indices' }, ...indices.map((ix) => ({ value: ix.code, label: indexLabel(ix) }))]}
          onChange={(v) => setParam('index', v || null)}
          className="min-w-[14rem]"
        />
      )}
      <Select label={tx.One} value={sector ?? ''} options={[{ value: '', label: tx.all }, ...sectorOpts]} onChange={(v) => setParam('sector', v || null)} className="min-w-[14rem]" />
    </FilterRow>
  );

  return (
    <div className={PAGE}>
      {head}
      <div className="space-y-14">
        <Section
          id="totals"
          title={`What ${miLabel} moves`}
          intro={
            <>
              <BasisTag basis="facts" />
              <p className="mt-2" data-testid="diversify-totals">
                {totalsWords(totals)}
                {notAssessed > 0
                  ? ` ${plural(notAssessed, 'node')} ${notAssessed === 1 ? 'has' : 'have'} no result in the scenario: the trade model was not run for ${notAssessed === 1 ? 'it' : 'them'}, so ${notAssessed === 1 ? 'it is' : 'they are'} left out.`
                  : ''}
              </p>
            </>
          }
        >
          <ul className="grid gap-3 sm:grid-cols-2 lg:grid-cols-4" aria-label={`${miLabel} totals`}>
            <li>
              <StatTile
                label="Above their benchmark"
                value={`${totals.above} of ${totals.withBenchmark}`}
                sub={`nodes with a benchmark, by more than a small tolerance${totals.withinCap ? `; ${totals.withinCap} within the cap in the allied market` : ''}`}
              />
            </li>
            <li>
              <StatTile label="Supply to shift" value={fmtUsd(totals.shiftUsd)} sub={`${fmtShare(totals.shiftShare, 0)} of the allied demand in those nodes`} />
            </li>
            <li>
              <StatTile label="Restored by trade" value={fmtUsd(totals.tradeUsd)} sub="existing relationships and optimized trade, with the cap holding" />
            </li>
            <li>
              <StatTile
                label="Left for new capacity"
                value={fmtUsd(totals.gapUsd)}
                sub={totals.closedByTrade ? `${plural(totals.closedByTrade, 'node')} need${totals.closedByTrade === 1 ? 's' : ''} none: trade replaces everything` : 'the gap only new capacity fills'}
              />
            </li>
          </ul>
          {totals.capped.length > 0 && (
            <p className="text-sm text-paper-dim" data-testid="diversify-capped">
              <span className="text-paper-mute">Most often capped:</span> {cappedWords(totals.capped, names, 5)}.
            </p>
          )}
          {managedRow && (
            <div className="card space-y-3 p-5" data-testid="explainer-three-way">
              <h3 className="rule-neis-sm text-base font-medium text-paper">Beside the other scenarios</h3>
              <p className="max-w-3xl text-sm text-paper-dim" data-testid="explainer-toggle-numbers">
                These are the figures the scenario choice shows for {miLabel} across the dashboard: {fmtUsd(managedRow.lostUsd)} of supply shifts in{' '}
                {plural(managedRow.affectedNodes, 'node')} with {fmtUsd(managedRow.affectedDemandUsd)} of allied demand; trade restores {fmtUsd(managedRow.restoredUsd)} and{' '}
                {fmtUsd(managedRow.residualUsd)} is left for new capacity. {scenarioCaveat(m)}
              </p>
              <ScenarioCoverageCompare rows={three} highlight="managed" label={`Coverage of the allied demand: ${miLabel} beside Baseline and Decoupling`} meta={m} />
            </div>
          )}
        </Section>

        <Section
          id="nodes"
          title="Node by node"
          intro={
            <p>
              Where the scenario ran, each node&apos;s suppliers to the allied market are capped at the share that would bring its concentration down to its benchmark; the node&apos;s allied
              demand is then split into what the capped market keeps, what existing relationships and optimized trade restore under the cap, and the new capacity diversification still
              needs. The filters apply to the chart and the table.
            </p>
          }
        >
          {filters}
          <ChartCard
            headingLevel={3}
            title="Concentration and where the allied demand goes"
            sub="Nodes where the scenario ran, largest capacity gap first. Concentration: the node's concentration of world exporters (HHI, the dot) against its benchmark (the ring); the gap between them is what the cap is set to close, and the cap applies to the allied market. Allied demand: the node's allied demand to scale, kept under the cap, restored by trade and left for new capacity."
            legend={legend}
            table={() => <TableView rows={ran} cols={chartCols} caption={`${label}: concentration and the allied demand split${scope}`} rowKey={(r) => r.node.id} pageSize={25} />}
            csv={csvOf(ran, 'nodes diversified')}
            summary={`${plural(ran.length, 'node')} diversified${scope}: ${ran
              .slice(0, 5)
              .map((r) => `${r.node.label}, concentration ${fmtScore(r.d.currentHhi)} against ${fmtScore(r.d.benchmarkHhi)}, new capacity ${fmtShare(r.split?.build, 0)}`)
              .join('; ')}${ran.length > 5 ? '; and more' : ''}.`}
            notes={
              <ModelledNote>
                Managed Interdependence; supply assumption: {m.headlineScenario}; {strategicScopeShort(m)}
              </ModelledNote>
            }
          >
            {ran.length ? (
              <>
                <DiversifyChart rows={shown} max={max} name={name} where={(r) => <NodePlace n={r.node} />} label={`Nodes diversified to their benchmark${scope}, largest capacity gap first`} />
                {ran.length > CHART_ROWS && (
                  <button type="button" className="btn-quiet mt-3" aria-expanded={showAll} onClick={() => setShowAll((v) => !v)}>
                    {showAll ? `Show the first ${CHART_ROWS}` : `Show all ${ran.length} nodes`}
                  </button>
                )}
              </>
            ) : (
              <EmptyState>No node{scope} is diversified in the scenario: every one is already as diversified as its benchmark, within the cap, or without one.</EmptyState>
            )}
          </ChartCard>
          <ChartCard
            headingLevel={3}
            title="Every node against its benchmark"
            sub="Concentration, benchmark and cap of each node, the supply to shift, coverage after existing relationships and after optimized trade under the cap, the capacity gap, the class under the scenario, the leading investment hosts and the largest new or expanded relationships. Nodes the scenario did not diversify show their status, without the run's figures."
            tools={<Switch label="Include nodes not diversified" checked={all} onChange={(v) => setParam('all', v ? '1' : null)} />}
            csv={csvOf(table, all ? 'every node' : 'nodes diversified')}
          >
            <TableView rows={table} cols={tableCols} caption={`${label}: every node against its benchmark${scope}`} rowKey={(r) => r.node.id} search pageSize={25} />
          </ChartCard>
        </Section>
      </div>
    </div>
  );
}
