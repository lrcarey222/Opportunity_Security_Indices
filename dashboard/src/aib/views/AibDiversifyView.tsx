// Allied Industrial Base diversification /aib/diversification (2026-10-19): "What would trade and investment
// look like if each step of the supply chain were as diversified as its wider sector?" The scenario
// "Diversified to its sector's concentration" (meta.diversify, node.diversify; derive/diversify.ts): the
// question, concentration (HHI) in plain words and the benchmark rule; the totals (nodes above their
// benchmark by more than a small tolerance of those with one, the supply to shift in USD and as a share of
// the allied demand in those nodes, what trade restores, what is left for new capacity, the most-capped
// suppliers); the chart of the nodes where the scenario ran, largest capacity gap first (current vs
// benchmark concentration on one HHI axis; the allied demand split to scale: kept / restored by trade / new
// capacity), with its table twin; and the table of every node (filters by OSI technology and stage, URL
// `tech` and `stage`, shared with the chart; an old `index` / `sector` link is dropped; `all=1`
// includes the nodes the scenario did not diversify, shown with their status and without the run columns;
// the class under the scenario with its physical-capability flag), CSV (shock: the scenario's id). Each
// node links to its compact (#nodes) and to the Sector Profiles of its six-stage OSI cells. Modelled
// facts: nothing follows the Perspective. An older bundle without the scenario says so.
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
  basisWords, cappedWords, diversifyClassText, diversifyCsvRow, diversifyLabel, diversifyRows, diversifyStageOptions, diversifyTechOptions, diversifyTotals, filterDiversify, hasDiversify, hhiAxisMax,
  orderDiversify, relationshipWords, rowStatusLabel, totalsWords, type DiversifyRow,
} from '../derive/diversify.ts';
import { fmtScore, fmtShare, fmtUsd, nameOf, plural } from '../derive/format.ts';
import { DIVERSIFY_BENCHMARK_WORDS, diversifySetting, HHI_SHORT, strategicScopeShort } from '../derive/settings.ts';
import { overviewRows } from '../derive/scenarioPages.ts';
import { scenarioCaveat, scenarioInfo } from '../derive/scenarios.ts';
import { nodeWhere, profiledCellsOfNode, techStageWords } from '../derive/taxonomy.ts';
import { compactHref } from '../nav.ts';
import { AIB_EYEBROW } from './AibOverviewView.tsx';
import { BasisTag } from './basis.tsx';
import { MANAGED_INTERDEPENDENCE, SCENARIO_CHART_LABEL } from '../../content/vocabulary.ts';
import { csvWithRun, QuestionLede, Section } from './common.tsx';
import { ModelledNote } from './networkParts.tsx';

/** The chart shows this many nodes until "Show all". */
export const CHART_ROWS = 15;

/** A node's place, its compact (node section) and the Sector Profiles of the OSI cells its codes sit in. */
function NodePlace({ n }: { n: AibNode }) {
  const { compactOf, title, href } = useAibCtx();
  const c = compactOf(n.compact);
  const cells = profiledCellsOfNode(n);
  return (
    <>
      {nodeWhere(n)}
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
          {cells.length === 1 ? ' · Sector profile: ' : ' · Sector profiles: '}
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
/** The page's question (the core AIB question, content/vocabulary.ts MANAGED_INTERDEPENDENCE). */
export const MI_QUESTION = 'How much specialization can allies keep while reducing the dependencies that create coercive or systemic risk?';
/** The table card's title. */
export const MI_TABLE_TITLE = 'Every supply-chain step against its benchmark';

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
  // the filters: an OSI technology (a node in several passes each; 'Not in the OSI basket') and a stage, by the
  // nodes' cells (nodeInCell()); the stages offered are those of the chosen technology's cells
  const techOpts = useMemo(() => diversifyTechOptions(rows), [rows]);
  const rawTech = params.get('tech');
  const tech = rawTech && techOpts.some((o) => o.value === rawTech) ? rawTech : null;
  const stageOpts = useMemo(() => diversifyStageOptions(rows, tech), [rows, tech]);
  const rawStage = params.get('stage');
  const stage = rawStage && stageOpts.some((o) => o.value === rawStage) ? rawStage : null;
  const all = params.get('all') === '1';
  const ran = useMemo(() => orderDiversify(filterDiversify(rows, { tech, stage, all: false })), [rows, tech, stage]);
  const table = useMemo(() => orderDiversify(filterDiversify(rows, { tech, stage, all })), [rows, tech, stage, all]);
  const max = useMemo(() => hhiAxisMax(ran), [ran]);
  const shown = showAll ? ran : ran.slice(0, CHART_ROWS);
  const setParam = (k: string, v: string | null) => {
    const q = new URLSearchParams(params);
    if (v) q.set(k, v);
    else q.delete(k);
    setParams(q, { replace: true, preventScrollReset: true });
  };
  const name = (iso3: string) => nameOf(names, iso3);
  // the CSV's run columns: the scenario's own shock id, also when only the nodes carry the scenario
  const runMeta = { ...m, headlineShock: shockId };
  const scope = techStageWords(tech, stage);
  const csvOf = (rs: DiversifyRow[], what: string) => () => ({
    name: `Allied Industrial Base diversification ${what}${scope}`,
    text: csvWithRun(
      rs.map((r) => diversifyCsvRow(r, names, tx)),
      runMeta,
    ),
  });
  const pct = (x: number | null | undefined) => fmtShare(x, 1);
  // a run column: its figure where the scenario ran, a dash elsewhere
  const run = (r: DiversifyRow, s: string) => (r.ran ? s : DASH);
  const chartCols: Col<DiversifyRow>[] = [
    { id: 'node', header: 'Supply-chain step', value: (r) => r.node.label, minWidth: 180 },
    { id: 'current', header: 'Current concentration (HHI)', value: (r) => r.d.currentHhi, cell: (r) => fmtScore(r.d.currentHhi), num: true },
    { id: 'benchmark', header: 'Benchmark concentration (HHI)', value: (r) => r.d.benchmarkHhi, cell: (r) => fmtScore(r.d.benchmarkHhi), num: true },
    { id: 'cap', header: 'Supplier cap', value: (r) => r.d.cap, cell: (r) => fmtShare(r.d.cap, 0), num: true },
    { id: 'kept', header: SPLIT_WORDS.kept, value: (r) => r.split?.kept ?? null, cell: (r) => pct(r.split?.kept), num: true },
    { id: 'trade', header: SPLIT_WORDS.trade, value: (r) => r.split?.trade ?? null, cell: (r) => pct(r.split?.trade), num: true },
    { id: 'build', header: SPLIT_WORDS.build, value: (r) => r.split?.build ?? null, cell: (r) => pct(r.split?.build), num: true },
    { id: 'gapUsd', header: 'Capacity gap (USD)', value: (r) => r.gapUsd, cell: (r) => fmtUsd(r.gapUsd), num: true },
  ];
  const tableCols: Col<DiversifyRow>[] = [
    { id: 'node', header: 'Supply-chain step', value: (r) => r.node.label, cell: (r) => <span className="text-paper">{r.node.label}</span>, minWidth: 180 },
    {
      id: 'where',
      header: `${tx.Many} · stage`,
      value: (r) => nodeWhere(r.node),
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
    { id: 'optimized', header: 'Coverage after coordinated trade', value: (r) => (r.ran ? r.d.optimized : null), cell: (r) => run(r, pct(r.d.optimized)), num: true },
    { id: 'gap', header: 'Capacity gap (share of allied demand)', value: (r) => (r.ran ? r.d.residual : null), cell: (r) => run(r, pct(r.d.residual)), num: true },
    { id: 'gapUsd', header: 'Capacity gap (USD)', value: (r) => (r.ran ? r.gapUsd : null), cell: (r) => run(r, fmtUsd(r.gapUsd)), num: true },
    {
      id: 'cls',
      header: 'What it needs under the scenario',
      value: (r) => (r.d.cls ? diversifyClassText(r.d.cls, r.clsWarning) : null),
      cell: (r) => <FlaggedClass cls={r.d.cls ?? null} warning={r.clsWarning} />,
      minWidth: 160,
    },
    { id: 'hosts', header: 'Best-placed countries to add capacity', value: (r) => (r.ran ? r.d.hosts.map(name).join(', ') || 'none ranked' : null), minWidth: 160 },
    { id: 'partners', header: 'Largest new or expanded relationships', value: (r) => (r.ran ? relationshipWords(r.d.partners, names) || 'none' : null), minWidth: 220 },
  ];

  const head = (
    <PageHead
      eyebrow={AIB_EYEBROW}
      title={MI_PAGE_TITLE}
      lede={
        <QuestionLede question={MI_QUESTION}>
          {`${MANAGED_INTERDEPENDENCE.explainer} The scenario removes no supplier: where a supply-chain step is more concentrated than comparable goods, it caps the share any one supplier may hold of the allied market, and asks how much supply has to shift, how much trade can replace, and how much new capacity is needed.`}
        </QuestionLede>
      }
    >
      <p className="mt-3 max-w-3xl text-sm text-paper-mute" data-testid="diversify-rule">
        {present ? (
          <>
            {label}: each supply-chain step&apos;s concentration of world exporters (HHI: {HHI_SHORT}) is compared with {setting ? setting.words : DIVERSIFY_BENCHMARK_WORDS.hs4.words}. A
            step more concentrated than its benchmark, by more than a small tolerance, gets the supplier-share cap that would bring its concentration down to the benchmark, applied to
            the allied market. A stress test of how the market could be organized, not a forecast; dollar values are {m.tradeYear} trade values.{' '}
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
        <EmptyState>This build has no Managed Interdependence results. They appear once the scenario &ldquo;{label}&rdquo; has been run and the data are exported again.</EmptyState>
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
      <Select label={tx.One} value={tech ?? ''} options={[{ value: '', label: tx.all }, ...techOpts]} onChange={(v) => setParam('tech', v || null)} className="min-w-[14rem]" />
      <Select label="Stage" value={stage ?? ''} options={[{ value: '', label: 'All stages' }, ...stageOpts]} onChange={(v) => setParam('stage', v || null)} />
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
                  ? ` ${plural(notAssessed, 'supply-chain step')} ${notAssessed === 1 ? 'has' : 'have'} no result in the scenario: the trade analysis was not run for ${notAssessed === 1 ? 'it' : 'them'}, so ${notAssessed === 1 ? 'it is' : 'they are'} left out.`
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
                sub={`supply-chain steps with a benchmark, by more than a small tolerance${totals.withinCap ? `; ${totals.withinCap} within the cap in the allied market` : ''}`}
              />
            </li>
            <li>
              <StatTile label="Supply to shift" value={fmtUsd(totals.shiftUsd)} sub={`${fmtShare(totals.shiftShare, 0)} of the allied demand at those steps`} />
            </li>
            <li>
              <StatTile label="Recovered through trade" value={fmtUsd(totals.tradeUsd)} sub="existing relationships and coordinated trade, with the cap holding" />
            </li>
            <li>
              <StatTile
                label={SCENARIO_CHART_LABEL.leftForNewCapacity}
                value={fmtUsd(totals.gapUsd)}
                sub={totals.closedByTrade ? `${plural(totals.closedByTrade, 'step')} need${totals.closedByTrade === 1 ? 's' : ''} none: trade replaces everything` : 'the gap only new capacity fills'}
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
                These are the figures the scenario choice shows for {miLabel} across the dashboard: {fmtUsd(managedRow.lostUsd)} of supply shifts at{' '}
                {plural(managedRow.affectedNodes, 'supply-chain step')} with {fmtUsd(managedRow.affectedDemandUsd)} of allied demand; trade recovers {fmtUsd(managedRow.restoredUsd)} and{' '}
                {fmtUsd(managedRow.residualUsd)} still needs new capacity. {scenarioCaveat(m)}
              </p>
              <ScenarioCoverageCompare rows={three} highlight="managed" label={`How much allied demand can be met: ${miLabel} beside ${scenarioInfo(m, 'baseline').label} and ${scenarioInfo(m, 'decoupling').label}`} meta={m} />
            </div>
          )}
        </Section>

        <Section
          id="nodes"
          title="Supply-chain step by step"
          intro={
            <p>
              Where the scenario ran, each step&apos;s suppliers to the allied market are capped at the share that would bring its concentration down to its benchmark; the step&apos;s allied
              demand is then split into what stays supplied under the cap, what existing relationships and coordinated trade recover under the cap, and the new capacity still needed.
              The filters apply to the chart and the table.
            </p>
          }
        >
          {filters}
          <ChartCard
            headingLevel={3}
            title="Concentration and where the allied demand goes"
            sub="Supply-chain steps where the scenario ran, largest capacity gap first. Concentration: the step's concentration of world exporters (HHI, the dot) against its benchmark (the ring); the gap between them is what the cap is set to close, and the cap applies to the allied market. Allied demand: the step's allied demand to scale, still supplied under the cap, recovered through trade and requiring new capacity."
            legend={legend}
            table={() => <TableView rows={ran} cols={chartCols} caption={`${label}: concentration and the allied demand split${scope}`} rowKey={(r) => r.node.id} pageSize={25} />}
            csv={csvOf(ran, 'nodes diversified')}
            summary={`${plural(ran.length, 'supply-chain step')} diversified${scope}: ${ran
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
                <DiversifyChart rows={shown} max={max} name={name} where={(r) => <NodePlace n={r.node} />} label={`Supply-chain steps diversified to their benchmark${scope}, largest capacity gap first`} />
                {ran.length > CHART_ROWS && (
                  <button type="button" className="btn-quiet mt-3" aria-expanded={showAll} onClick={() => setShowAll((v) => !v)}>
                    {showAll ? `Show the first ${CHART_ROWS}` : `Show all ${ran.length} steps`}
                  </button>
                )}
              </>
            ) : (
              <EmptyState>No supply-chain step{scope} is diversified in the scenario: every one is already as diversified as its benchmark, within the cap, or without one.</EmptyState>
            )}
          </ChartCard>
          <ChartCard
            headingLevel={3}
            title={MI_TABLE_TITLE}
            sub="Concentration, benchmark and cap of each step, the supply to shift, coverage after existing relationships and after coordinated trade under the cap, the capacity gap, what the step needs under the scenario, the best-placed countries to add capacity and the largest new or expanded relationships. Steps the scenario did not diversify show their status, without the run's figures."
            tools={<Switch label="Include steps not diversified" checked={all} onChange={(v) => setParam('all', v ? '1' : null)} />}
            csv={csvOf(table, all ? 'every node' : 'nodes diversified')}
          >
            <TableView rows={table} cols={tableCols} caption={`${label}: every supply-chain step against its benchmark${scope}`} rowKey={(r) => r.node.id} search pageSize={25} />
          </ChartCard>
        </Section>
      </div>
    </div>
  );
}
