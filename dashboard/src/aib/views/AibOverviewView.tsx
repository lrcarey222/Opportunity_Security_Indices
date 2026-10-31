// Allied Industrial Base overview /aib (IA refactor, 2026-10-16; brief s.13-14): a light orientation page.
// It opens (2026-10-29, design L4) with "Allied coverage by technology and stage" (#division): the posture
// module's allied division of labour (who among trusted allies holds each OSI technology x stage cell, the
// gaps, partner demand: the economies whose canonical choice is Compete · Partner; Deploy · Accelerate is
// never counted), its three header metrics as tiles, the matrix card and its node detail, which lists
// and links the Allied Industrial Base nodes whose six-stage cells (node.yfCells) are in the cell, each link
// opening the Workstreams node list filtered on that cell (its technology and stage). It follows
// the strategic position framework, not the scenario toggle. Then the three scenarios side by side (Baseline, Managed
// Interdependence, Decoupling; the toggled one highlighted): the allied demand affected, the coverage of
// the allied demand (kept / restored by trade / new capacity), what is left for new capacity in USD and the
// class mix of the lost or shifted supply, with a table twin and a CSV. Then where coordination matters
// under the toggled scenario (node counts and shares of the lost supply by class, the class bar; the nodes
// that lose the largest share and the largest coordination dividends; Managed Interdependence counts the
// nodes it leaves unchanged apart), one card per remaining question of the module (Partnerships, Sectoral
// Compacts, Country roles; their counts follow the toggle once the scenario's file is loaded) and a pointer
// to the Managed Interdependence explainer. A bundle without the scenarios shows Decoupling alone, as
// before. Every number comes from the compacts document (and the scenario file); none responds to the
// Perspective (preference) settings.

import { useMemo } from 'react';
import { Link } from 'react-router';
import { ChartCard } from '../../charts/ChartCard.tsx';
import { TableView, type Col } from '../../components/TableView.tsx';
import { PageHead, StatTile } from '../../components/ui.tsx';
import { AibDivisionCard, AibDivisionStats, indexNodesByCell, PostureBasisTag } from '../../posture/index.ts';
import { Term } from '../../components/Term.tsx';
import { AIB_SIDES, MANAGED_INTERDEPENDENCE, MODULE_QUESTION, SCENARIO_CHART_LABEL, SCENARIO_SECTION_TITLE } from '../../content/vocabulary.ts';
import { POSTURE_TERMS } from '../../posture/index.ts';
import { useTitle } from '../../shell/title.ts';
import { PAGE, useAibCtx } from '../AibLayout.tsx';
import { AIB_CLASSES, inStrategicScope, type AibCompact } from '../contract.ts';
import { ClassChip, ClassLegend, ClassSwatch } from '../charts/ClassChip.tsx';
import { LostByClassBar } from '../charts/LostByClassBar.tsx';
import { CoverageLegend, ScenarioBars, ScenarioClassBars, ScenarioCoverageCompare, ScenarioSwatch, ScenarioTag } from '../charts/ScenarioCompare.tsx';
import { useAibScenarioDoc } from '../data/hooks.ts';
import { classInfo } from '../derive/classes.ts';
import { DIVERSIFY_HREF, hasDiversify } from '../derive/diversify.ts';
import { countryRoleRows } from '../derive/countryRoles.ts';
import { fmtPp, fmtShare, fmtUsd, plural } from '../derive/format.ts';
import { NO_FILTER, pairTradeFlows } from '../derive/partnerships.ts';
import { managedCompacts, managedOf, overviewRows, scenarioClassGroups, topScenarioNodes, type OverviewRow } from '../derive/scenarioPages.ts';
import { nodeScenario, scenarioCaveat, scenarioCsvFields, scenarioInfo, scenarioList, scenarioShockLabel, type NodeScenario } from '../derive/scenarios.ts';
import { cellsInFilter, nodeWhere } from '../derive/taxonomy.ts';
import { compactHref, workstreamsHref } from '../nav.ts';
import { COVERAGE_WORDS } from '../charts/ScenarioCompare.tsx';
import { csvWithRun, QuestionLede, Section } from './common.tsx';
import { BasisTag, QuestionCard } from './basis.tsx';

export const AIB_EYEBROW = 'ALLIED INDUSTRIAL BASE';

function NodeList({ rows, value, label }: { rows: NodeScenario[]; value: (r: NodeScenario) => string; label: string }) {
  const { href } = useAibCtx();
  return (
    <ol className="divide-y divide-white/5 text-sm" aria-label={label}>
      {rows.map((r) => {
        const n = r.node;
        return (
          <li key={n.id} className="flex flex-wrap items-baseline gap-x-3 gap-y-1 py-2">
            <span className="w-14 shrink-0 text-right font-medium tabular text-paper">{value(r)}</span>
            <span className="min-w-0 flex-1">
              <span className="text-paper">{n.label}</span>{' '}
              <span className="text-xs text-paper-mute">
                {nodeWhere(n)}
                {n.compact && (
                  <>
                    {' · '}
                    <Link className="link" to={href(compactHref(n.compact))}>
                      {n.compact}
                    </Link>
                  </>
                )}
              </span>
            </span>
            {r.cls && <ClassChip cls={r.cls} />}
          </li>
        );
      })}
    </ol>
  );
}

/**
 * What the division's demand counts (design section 5, the canonical framework): Partner demand is the
 * economies whose choice for the node is Partner, Secure demand those that must Secure it. An Accelerate
 * choice is a deployment priority, not a call on allied supply, so it is never counted (and Deployment & Use
 * has no column in the matrix).
 */
export const DIVISION_DEMAND_WORDS =
  'Partner demand counts the countries whose strategy relies on partners for a step (Partner); Secure demand, those that must secure it. Accelerate is a deployment priority, not a call on allied supply, so it is not counted.';

/** The id and title of the Overview's first section (design L4; the guide's question, MODULE_QUESTION.coverage). */
export const DIVISION_SECTION = { id: 'division', title: MODULE_QUESTION.coverage } as const;

/** The Overview's lede after its question (the guide's AIB overview lede; the suppliers that replace lost supply can be any economy, so not "trusted partners"). */
export const OVERVIEW_LEDE =
  'No ally should try to make everything. The Allied Industrial Base asks which capabilities must be secure, which countries are best placed to specialize in them, and where trade or new investment can close the remaining gaps. The analysis stress-tests today’s supply network, measures how much of the affected supply existing and coordinated trade can replace, and shows where acting together is worth the effort.';

/** The second paragraph of the Overview (guide): what the scenario changes and what it does not. */
export const OVERVIEW_GOAL =
  'The selected scenario changes the supply problem; it does not change the basic goal: the smallest credible network of partners that can keep critical supply available without recreating the whole stack in every country.';

/**
 * "Allied coverage by technology and stage": the posture module's division card in the page's Section,
 * its node detail listing and linking the model's nodes in each six-stage cell (a node in several cells
 * under each, never placed by node.stage): a node with a compact links to its compact's nodes, the others
 * to the Workstreams node list filtered on the cell (its technology and stage; with every node when the
 * node lies outside the strategic scope), which lists the node. It does not follow the scenario toggle.
 */
function DivisionSection() {
  const { doc, href } = useAibCtx();
  const nodesByCell = useMemo(
    () =>
      indexNodesByCell(
        doc.nodes.flatMap((n) =>
          cellsInFilter(n).map((c) => {
            const q = new URLSearchParams({ tech: c.tech, stage: c.stage });
            if (!inStrategicScope(n)) q.set('scope', 'all');
            const to = n.compact ? `${compactHref(n.compact)}#nodes` : `${workstreamsHref()}?${q.toString()}#nodes`;
            return { id: n.id, label: n.label, href: href(to), cells: [c.slug] };
          }),
        ),
      ),
    [doc, href],
  );
  return (
    <Section
      id={DIVISION_SECTION.id}
      title={DIVISION_SECTION.title}
      intro={
        <>
          <PostureBasisTag />
          <p className="mt-2" data-testid="division-intro">
            Select a cell to see which allies already have capability, which countries need a partner, and where the allied network still lacks a credible backup. For each
            technology at each producing stage it answers both sides of the division of labor: {AIB_SIDES.supply.question.toLowerCase()} (who supplies, and who backs them up) and{' '}
            {AIB_SIDES.demand.question.toLowerCase()} (<Term id={POSTURE_TERMS.partnerDemand}>Partner demand</Term>), with any allied <Term id={POSTURE_TERMS.aibGap}>gap</Term>. It
            reads the country strategies, not the scenario chosen above: switching scenarios does not change it. <span data-testid="division-demand">{DIVISION_DEMAND_WORDS}</span>
          </p>
        </>
      }
    >
      <AibDivisionStats />
      <AibDivisionCard nodesByCell={nodesByCell} />
    </Section>
  );
}

/** A figure's heading inside the three-way card. */
function H({ children, sub }: { children: string; sub: string }) {
  return (
    <div className="mb-2">
      <h4 className="text-sm font-medium text-paper">{children}</h4>
      <p className="text-xs text-paper-mute">{sub}</p>
    </div>
  );
}

/** The three scenarios side by side: the toggled one highlighted, every value in text, a table twin and a CSV. */
function ThreeWay({ rows }: { rows: OverviewRow[] }) {
  const { doc, scenario } = useAibCtx();
  const meta = doc.meta;
  const weighed = rows[0]?.nodes ?? 0;
  const note = (r: OverviewRow) =>
    r.id === 'baseline'
      ? 'no disruption: nothing is lost'
      : `${plural(r.affectedNodes, 'step')} of ${weighed}; ${fmtUsd(r.lostUsd)} ${r.id === 'managed' ? 'shifted' : 'lost'}`;
  const cols: Col<OverviewRow>[] = [
    { id: 'scenario', header: 'Scenario', value: (r) => r.label },
    { id: 'nodes', header: 'Supply-chain steps that lose or shift supply', value: (r) => r.affectedNodes, num: true },
    { id: 'affected', header: SCENARIO_CHART_LABEL.alliedDemandAffected, value: (r) => r.affectedDemandUsd, cell: (r) => fmtUsd(r.affectedDemandUsd), num: true },
    { id: 'lost', header: 'Supply lost or shifted', value: (r) => r.lostUsd, cell: (r) => fmtUsd(r.lostUsd), num: true },
    { id: 'kept', header: COVERAGE_WORDS.kept, value: (r) => r.split?.kept ?? null, cell: (r) => fmtShare(r.split?.kept, 0), num: true },
    { id: 'trade', header: COVERAGE_WORDS.trade, value: (r) => r.split?.trade ?? null, cell: (r) => fmtShare(r.split?.trade, 0), num: true },
    { id: 'build', header: COVERAGE_WORDS.build, value: (r) => r.split?.build ?? null, cell: (r) => fmtShare(r.split?.build, 0), num: true },
    { id: 'gap', header: SCENARIO_CHART_LABEL.leftForNewCapacity, value: (r) => r.residualUsd, cell: (r) => fmtUsd(r.residualUsd), num: true },
    { id: 'mix', header: 'What the affected supply needs', value: (r) => r.classMix || 'none', minWidth: 220 },
  ];
  const csv = () => ({
    name: 'Allied Industrial Base three scenarios',
    text: csvWithRun(
      rows.map((r) => ({
        ...scenarioCsvFields(meta, r.id),
        nodes_weighed: r.nodes,
        nodes_run: r.nodesRun,
        nodes_losing_or_shifting_supply: r.affectedNodes,
        allied_demand_usd: r.demandUsd,
        allied_demand_affected_usd: r.affectedDemandUsd,
        lost_or_shifted_usd: r.lostUsd,
        share_lost_or_shifted: r.exposure,
        kept: r.split?.kept ?? null,
        restored_by_trade: r.split?.trade ?? null,
        new_capacity: r.split?.build ?? null,
        restored_by_trade_usd: r.restoredUsd,
        left_for_new_capacity_usd: r.residualUsd,
        class_mix: r.classMix || null,
      })),
      meta,
    ),
  });
  const summary = rows
    .map((r) => `${r.label}: ${fmtUsd(r.affectedDemandUsd)} of demand must find a new supplier, still supplied ${fmtShare(r.split?.kept, 0)}, recovered through trade ${fmtShare(r.split?.trade, 0)}, requires new capacity ${fmtShare(r.split?.build, 0)}, ${fmtUsd(r.residualUsd)} still needs new capacity`)
    .join('; ');
  return (
    <ChartCard
      headingLevel={3}
      title="The three scenarios side by side"
      sub={`The ${weighed} supply-chain steps with demand and supply figures to compare${doc.nodes.length > weighed ? ` (of ${doc.nodes.length}; the others lack them)` : ''}, weighed the same in each scenario. Shares are of allied demand; dollar values are a year at ${meta.tradeYear} trade values.`}
      summary={summary}
      legend={<CoverageLegend />}
      notes={<span>{scenarioCaveat(meta)}</span>}
      table={() => <TableView rows={rows} cols={cols} caption="The three scenarios side by side" rowKey={(r) => r.id} />}
      csv={csv}
    >
      <div className="space-y-6" data-testid="overview-three-way">
        <div>
          <H sub="The allied demand at the steps that lose supply, or must shift it away from a capped supplier (USD a year).">{SCENARIO_CHART_LABEL.alliedDemandAffected}</H>
          <ScenarioBars
            values={rows.map((r) => ({ id: r.id, value: r.affectedDemandUsd, note: note(r) }))}
            highlight={scenario}
            label="Demand that must find a new supplier under each scenario"
            format={fmtUsd}
            meta={meta}
          />
        </div>
        <div>
          <H sub="Coverage: what stays supplied, what existing relationships and coordinated trade recover, and what only new capacity can fill.">How much allied demand can be met</H>
          <ScenarioCoverageCompare rows={rows} highlight={scenario} label="How much allied demand can be met under each scenario" meta={meta} legend={false} />
        </div>
        <div>
          <H sub="The supply trade cannot replace (USD a year): the remaining supply gap new capacity would have to fill.">{SCENARIO_CHART_LABEL.leftForNewCapacity}</H>
          <ScenarioBars values={rows.map((r) => ({ id: r.id, value: r.residualUsd }))} highlight={scenario} label="Gap that still needs new capacity under each scenario" format={fmtUsd} meta={meta} />
        </div>
        <div>
          <H sub="The lost or shifted supply, split by what each step needs under the scenario.">{SCENARIO_CHART_LABEL.whereLostSupplySits}</H>
          <ScenarioClassBars
            rows={rows.map((r) => ({ id: r.id, groups: r.byClass.map((g) => ({ cls: g.cls, usd: g.lostUsd })) }))}
            highlight={scenario}
            label="Lost or shifted supply by treatment under each scenario"
            format={fmtUsd}
            meta={meta}
          />
        </div>
      </div>
    </ChartCard>
  );
}

export default function AibOverviewView() {
  useTitle('Overview', 'aib');
  const { doc, shockLabel, names, scenario, hasScenarios, href } = useAibCtx();
  const m = doc.meta;
  const mi = scenario === 'managed';
  const sc = scenarioInfo(m, scenario);
  // the Managed Interdependence file, for the question cards' counts (loaded once the scenario is chosen)
  const managedDoc = managedOf(useAibScenarioDoc(mi ? 'managed' : null).doc);
  const three = useMemo(() => (hasScenarios ? overviewRows(doc.nodes, m) : []), [doc, m, hasScenarios]);
  const rows = useMemo(() => doc.nodes.map((n) => nodeScenario(n, scenario)), [doc, scenario]);
  const groups = useMemo(() => scenarioClassGroups(doc.nodes, scenario), [doc, scenario]);
  const classed = groups.filter((g) => !g.unchanged);
  const unchanged = groups.find((g) => g.unchanged)?.nodes.length ?? 0;
  const lostTotal = classed.reduce((s, g) => s + g.lostUsd, 0);
  const total = doc.nodes.length;
  const byCls = new Map(classed.map((g) => [g.cls, g]));
  const severity = useMemo(() => topScenarioNodes(rows, (r) => r.exposure), [rows]);
  const dividends = useMemo(() => topScenarioNodes(rows, (r) => r.dividend), [rows]);
  // the compacts as the toggled scenario solves them (Managed Interdependence: once its file is loaded)
  const compacts: AibCompact[] | null = useMemo(() => (!mi ? doc.compacts : managedDoc ? managedCompacts(doc.compacts, managedDoc) : null), [mi, doc, managedDoc]);
  const trade = useMemo(() => (compacts ? pairTradeFlows({ nodes: doc.nodes, compacts }, NO_FILTER) : null), [doc, compacts]);
  const hostNodes = rows.filter((r) => (mi ? r.ran && (r.node.diversify?.hosts.length ?? 0) > 0 : r.node.hosts.length > 0)).length;
  const roleRows = useMemo(() => (compacts ? countryRoleRows({ compacts }, names) : null), [compacts, names]);
  // the compact types under the toggled scenario (Managed Interdependence: its own, once its file is loaded;
  // the compacts it solves no coalition for counted apart)
  const types = { candidate: 0, bilateral: 0, unnecessary: 0 };
  for (const c of compacts ?? []) {
    if (c.type === 'candidate compact') types.candidate += 1;
    else if (c.type === 'bilateral arrangement sufficient') types.bilateral += 1;
    else types.unnecessary += 1;
  }
  const noCoalition = mi && compacts ? doc.compacts.length - compacts.length : 0;
  const high = m.thresholds.high_severity;
  const nHigh = !mi && typeof high === 'number' ? rows.filter((r) => (r.exposure ?? 0) >= high).length : null;
  const shown = AIB_CLASSES.filter((c) => c !== 'INSUFFICIENT DATA');
  const insufficient = byCls.get('INSUFFICIENT DATA')?.nodes.length ?? 0;
  const lostShare = (cls: string) => (lostTotal > 0 ? (byCls.get(cls)?.lostUsd ?? 0) / lostTotal : null);
  const biggest = [...classed].filter((g) => g.lostUsd > 0).sort((a, b) => b.lostUsd - a.lostUsd)[0];
  const nCoord = byCls.get('COORDINATE')?.nodes.length ?? 0;
  const ranNodes = mi ? rows.filter((r) => r.ran).length : total;
  const rare = nCoord <= Math.max(1, Math.round(ranNodes * 0.05));
  const lostWord = mi ? 'shifted supply' : 'lost supply';
  const shareLine = classed
    .filter((g) => g.nodes.length)
    .map((g) => `${g.info.label} ${plural(g.nodes.length, 'step')}${lostTotal > 0 ? `, ${fmtShare(g.lostUsd / lostTotal, 0)} of ${lostWord}` : ''}`)
    .join('; ');
  const coordWords = `${plural(nCoord, 'step')} ${nCoord === 1 ? 'needs' : 'need'} trade coordination: new trade relationships among partners recover most of the gap.`;
  const takeaway =
    total === 0
      ? 'This build has no classified supply-chain step.'
      : `${mi ? `Managed Interdependence applies to ${plural(ranNodes, 'step')} where one supplier holds more of the allied market than the cap allows, and leaves the other ${unchanged} unchanged. ` : ''}${
          rare ? `Trade coordination alone is rarely enough here: ${coordWords}` : coordWords
        }${
          biggest
            ? ` Most of the ${lostWord} (${fmtShare(biggest.lostUsd / lostTotal, 0)}) sits at steps marked ${biggest.info.label}: ${biggest.info.meaning.charAt(0).toLowerCase()}${biggest.info.meaning.slice(1)}`
            : ''
        }`;
  const shockLine = hasScenarios ? scenarioShockLabel(m, scenario) : shockLabel;

  return (
    <div className={PAGE}>
      <PageHead
        eyebrow={AIB_EYEBROW}
        title="Allied Industrial Base"
        lede={<QuestionLede question={MODULE_QUESTION.aib}>{OVERVIEW_LEDE}</QuestionLede>}
      >
        <p className="mt-3 max-w-3xl text-[0.9375rem] leading-relaxed text-paper-dim" data-testid="overview-goal">
          {OVERVIEW_GOAL}
        </p>
        <p className="mt-3 max-w-3xl text-sm text-paper-mute" data-testid="overview-run">
          {hasScenarios ? (
            <>
              Results below use <span className="text-paper-dim">{sc.label}</span> ({shockLine})
            </>
          ) : (
            <>
              Results below use <span className="text-paper-dim">{shockLabel}</span>
            </>
          )}
          ; trade in {m.tradeYear}; <span title={`Assumption about how much suppliers can expand: ${m.headlineScenario}`}>{plural(total, 'supply-chain step')}</span> (groups of traded goods, finer than the technology × stage cells of{' '}
          <a className="link" href={`#${DIVISION_SECTION.id}`}>
            who can cover each supply-chain step
          </a>
          ).{' '}
          <Link className="link" to={href('/compacts/about')}>
            How the analysis works &rarr;
          </Link>
        </p>
      </PageHead>

      <div className="space-y-14">
        <DivisionSection />

        {hasScenarios && three.length > 0 && (
          <Section
            id="scenarios"
            title={SCENARIO_SECTION_TITLE}
            intro={
              <>
                <BasisTag basis="model" scenario="every scenario" />
                <p className="mt-2" data-testid="overview-mi-question">
                  {MANAGED_INTERDEPENDENCE.question}
                </p>
                <dl className="mt-2 grid gap-x-4 gap-y-1 sm:grid-cols-[12rem_minmax(0,1fr)]" data-testid="overview-scenario-words">
                  {scenarioList(m).map((s) => (
                    <div key={s.id} className="contents">
                      <dt className="inline-flex items-center gap-1.5 text-paper">
                        <ScenarioSwatch id={s.id} />
                        {s.label}
                      </dt>
                      <dd className="text-paper-dim">{s.words}</dd>
                    </div>
                  ))}
                </dl>
                <p className="mt-2">The scenario chosen above is highlighted in every comparison; the rest of the page follows it.</p>
              </>
            }
          >
            <ThreeWay rows={three} />
          </Section>
        )}

        <Section
          id="where"
          title={MODULE_QUESTION.coordination}
          intro={
            <>
              <span className="flex flex-wrap items-center gap-2">
                <BasisTag basis="facts" />
                {hasScenarios && <ScenarioTag id={scenario} meta={m} />}
              </span>
              <p className="mt-2" data-testid="coordination-intro">
                Coordination matters when today’s bilateral relationships cannot absorb the disruption, but a broader group of suppliers can. Where even coordinated trade is not
                enough, the problem becomes an investment problem.
              </p>
              <p className="mt-2">
                Each supply-chain step (a group of traded goods at one stage) gets one recommended treatment from its own results{mi ? ' under the scenario' : ''}.{' '}
                {nHigh !== null && typeof high === 'number' ? `${plural(nHigh, 'step')} lose at least ${fmtShare(high, 0)} of their supply to the disruption. ` : ''}
                {takeaway}
              </p>
            </>
          }
        >
          <ul className="grid gap-3 sm:grid-cols-2 lg:grid-cols-4" aria-label="Supply-chain steps by treatment">
            {shown.map((cls) => {
              const n = byCls.get(cls)?.nodes.length ?? 0;
              const ls = lostShare(cls);
              return (
                <li key={cls}>
                  <StatTile
                    label={classInfo(cls).label}
                    value={plural(n, 'step')}
                    chip={<ClassSwatch cls={cls} />}
                    sub={`${ranNodes ? fmtShare(n / ranNodes, 0) : '–'} of ${mi ? 'the steps it applies to' : 'steps'}${ls !== null ? ` · ${fmtShare(ls, 0)} of ${lostWord}` : ''}`}
                  />
                  <p className="mt-1.5 px-1 text-xs leading-relaxed text-paper-mute">{classInfo(cls).meaning}</p>
                </li>
              );
            })}
          </ul>
          {mi && unchanged > 0 && (
            <p className="text-xs text-paper-mute" data-testid="overview-unchanged">
              {plural(unchanged, 'step')} {unchanged === 1 ? 'is' : 'are'} unchanged under Managed Interdependence: not more concentrated than comparable goods, within the cap in the
              allied market, without a benchmark, or not assessed.
            </p>
          )}
          {insufficient > 0 && (
            <p className="text-xs text-paper-mute">
              {plural(insufficient, 'step')} with too little trade to classify ({classInfo('INSUFFICIENT DATA').label}).
            </p>
          )}
          <div className="panel p-4">
            <p className="mb-2 text-sm text-paper-dim">Where the {lostWord} sits, by what each step needs</p>
            {lostTotal > 0 ? (
              <LostByClassBar groups={classed} label={`${mi ? 'Shifted' : 'Lost'} supply by treatment. ${shareLine}`} />
            ) : (
              <p className="text-xs text-paper-mute">No supply is {mi ? 'shifted' : 'lost'} under this scenario.</p>
            )}
            <div className="mt-2">
              <ClassLegend />
            </div>
          </div>
          <div className="grid gap-6 lg:grid-cols-2">
            <section aria-labelledby="sev-h" className="card p-5">
              <h3 id="sev-h" className="rule-neis-sm text-base font-medium text-paper">
                {mi ? 'Largest share of supply to shift' : 'Hardest-hit supply-chain steps'}
              </h3>
              <p className="mb-2 mt-1 text-sm text-paper-mute">
                {mi ? "Share of each step's allied demand its capped suppliers hold above the cap." : "Share of each step's allied demand the disruption removes."}
              </p>
              <NodeList rows={severity} value={(r) => fmtShare(r.exposure, 0)} label={mi ? 'Steps with the largest share of supply to shift' : 'Steps with the most supply exposed to the disruption'} />
            </section>
            <section aria-labelledby="div-h" className="card p-5">
              <h3 id="div-h" className="rule-neis-sm text-base font-medium text-paper">
                Where acting together recovers most
              </h3>
              <p className="mb-2 mt-1 text-sm text-paper-mute">
                The extra share of allied demand that coordinated trade recovers over existing relationships{mi ? ', with the cap holding' : ''} (the coordination dividend).
              </p>
              <NodeList rows={dividends} value={(r) => fmtPp(r.dividend, 0)} label="Steps where coordination recovers most" />
            </section>
          </div>
          <p className="text-sm">
            <Link className="link" to={href('/compacts#map')}>
              Every supply-chain step on the map &rarr;
            </Link>
          </p>
          {hasDiversify(doc) && (
            <p className="text-sm text-paper-dim" data-testid="diversify-pointer">
              How does Managed Interdependence decide which supply to shift? Each step is compared with comparable goods, and a cap brings it down to them.{' '}
              <Link className="link" to={href(DIVERSIFY_HREF)}>
                How Managed Interdependence works &rarr;
              </Link>
            </p>
          )}
        </Section>

        <Section
          id="questions"
          title="Who does what in the allied network"
          intro={
            <p>
              Each page answers one question about the allied division of labor{hasScenarios ? ` under the scenario chosen above (${sc.label})` : ''}.
            </p>
          }
        >
          <ul className="grid gap-4 md:grid-cols-3">
            <QuestionCard
              question={MODULE_QUESTION.partnerships}
              answer={
                trade
                  ? `${plural(trade.length, 'coordinated trade relationship')} across the compacts; countries that could add capacity at ${plural(hostNodes, 'supply-chain step')}; the best-placed partners by technology and stage (Friendshore, Partnership Opportunity, Development Potential).`
                  : `Coordinated trade relationships across the compacts; countries that could add capacity at ${plural(hostNodes, 'supply-chain step')}; the best-placed partners by technology and stage (Friendshore, Partnership Opportunity, Development Potential).`
              }
              to={href('/aib/partnerships')}
              cta="Partnerships"
            />
            <QuestionCard
              question={MODULE_QUESTION.compacts}
              answer={
                compacts
                  ? `${plural(types.candidate, 'potential Sectoral Compact')}${types.bilateral ? `, ${types.bilateral} where a bilateral arrangement is enough` : ''}${types.unnecessary ? `, ${types.unnecessary} where existing relationships do nearly as well (no compact needed)` : ''}${
                      noCoalition ? `; ${noCoalition} of the ${doc.compacts.length} compacts need no coalition under Managed Interdependence` : ''
                    }. Each names the bottleneck, the partners and what governments should do next.`
                  : `${plural(doc.compacts.length, 'compact')}, each solved again under Managed Interdependence. Each names the bottleneck, the partners and what governments should do next.`
              }
              to={href('/compacts')}
              cta="Sectoral Compacts"
            />
            <QuestionCard
              question={MODULE_QUESTION.roles}
              answer={
                roleRows
                  ? `${plural(roleRows.length, 'economy', 'economies')} hold a role in at least one compact: demand anchors, specialized and resource suppliers, countries that could add capacity, technology and bridge partners.`
                  : 'Demand anchors, specialized and resource suppliers, countries that could add capacity, technology and bridge partners, compact by compact.'
              }
              to={href('/aib/roles')}
              cta="Country roles"
            />
          </ul>
          <p className="max-w-3xl text-sm text-paper-mute">
            These are observed data and analysis results: they do not change with the Perspective (preference) settings of a Country profile. Where the Partnerships page ranks
            partners by preference-weighted scores it says so and shows the NEIS defaults.
          </p>
        </Section>
      </div>
    </div>
  );
}
