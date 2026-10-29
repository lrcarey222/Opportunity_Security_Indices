// Allied Industrial Base overview /aib (IA refactor, 2026-10-16; brief s.13-14): a light orientation page.
// It opens (2026-10-29, design L4) with "Allied coverage by technology and stage" (#division): the posture
// module's allied division of labour (who among trusted allies holds each OSI technology x stage cell, the
// gaps, partner demand), its three header metrics as tiles, the matrix card and its node detail, which lists
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
import { AibDivisionCard, AibDivisionIntro, AibDivisionStats, indexNodesByCell, PostureBasisTag } from '../../posture/index.ts';
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
import { csvWithRun, Section } from './common.tsx';
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

/** The id and title of the Overview's first section (design L4; not "Division of labour", the compact pages' own). */
export const DIVISION_SECTION = { id: 'division', title: 'Allied coverage by technology and stage' } as const;

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
            <AibDivisionIntro /> Select a cell for its allied holders and the model&apos;s supply-chain nodes whose traded goods sit there.
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
      ? 'no shock: nothing is lost'
      : `${plural(r.affectedNodes, 'node')} of ${weighed}; ${fmtUsd(r.lostUsd)} ${r.id === 'managed' ? 'shifted' : 'lost'}`;
  const cols: Col<OverviewRow>[] = [
    { id: 'scenario', header: 'Scenario', value: (r) => r.label },
    { id: 'nodes', header: 'Nodes that lose or shift supply', value: (r) => r.affectedNodes, num: true },
    { id: 'affected', header: 'Allied demand affected', value: (r) => r.affectedDemandUsd, cell: (r) => fmtUsd(r.affectedDemandUsd), num: true },
    { id: 'lost', header: 'Supply lost or shifted', value: (r) => r.lostUsd, cell: (r) => fmtUsd(r.lostUsd), num: true },
    { id: 'kept', header: 'Kept', value: (r) => r.split?.kept ?? null, cell: (r) => fmtShare(r.split?.kept, 0), num: true },
    { id: 'trade', header: 'Restored by trade', value: (r) => r.split?.trade ?? null, cell: (r) => fmtShare(r.split?.trade, 0), num: true },
    { id: 'build', header: 'New capacity', value: (r) => r.split?.build ?? null, cell: (r) => fmtShare(r.split?.build, 0), num: true },
    { id: 'gap', header: 'Left for new capacity', value: (r) => r.residualUsd, cell: (r) => fmtUsd(r.residualUsd), num: true },
    { id: 'mix', header: 'Class mix of the lost supply', value: (r) => r.classMix || 'none', minWidth: 220 },
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
    .map((r) => `${r.label}: allied demand affected ${fmtUsd(r.affectedDemandUsd)}, kept ${fmtShare(r.split?.kept, 0)}, restored by trade ${fmtShare(r.split?.trade, 0)}, new capacity ${fmtShare(r.split?.build, 0)}, ${fmtUsd(r.residualUsd)} left for new capacity`)
    .join('; ');
  return (
    <ChartCard
      headingLevel={3}
      title="The three scenarios side by side"
      sub={`Every supply-chain node the model optimizes (${weighed}), weighed the same in each scenario. Shares are of their allied demand; dollar values are a year at ${meta.tradeYear} trade values.`}
      summary={summary}
      legend={<CoverageLegend />}
      notes={<span>{scenarioCaveat(meta)}</span>}
      table={() => <TableView rows={rows} cols={cols} caption="The three scenarios side by side" rowKey={(r) => r.id} />}
      csv={csv}
    >
      <div className="space-y-6" data-testid="overview-three-way">
        <div>
          <H sub="The allied demand of the nodes that lose supply, or must shift it away from a capped supplier (USD a year).">Allied demand affected</H>
          <ScenarioBars
            values={rows.map((r) => ({ id: r.id, value: r.affectedDemandUsd, note: note(r) }))}
            highlight={scenario}
            label="Allied demand affected under each scenario"
            format={fmtUsd}
            meta={meta}
          />
        </div>
        <div>
          <H sub="What the market keeps, what existing relationships and optimized trade restore, and what only new capacity can fill.">Coverage of the allied demand</H>
          <ScenarioCoverageCompare rows={rows} highlight={scenario} label="Coverage of the allied demand under each scenario" meta={meta} legend={false} />
        </div>
        <div>
          <H sub="The supply trade cannot replace (USD a year): the gap new capacity would have to fill.">Left for new capacity</H>
          <ScenarioBars values={rows.map((r) => ({ id: r.id, value: r.residualUsd }))} highlight={scenario} label="Left for new capacity under each scenario" format={fmtUsd} meta={meta} />
        </div>
        <div>
          <H sub="The lost or shifted supply split by the class each node gets under the scenario.">Where the lost supply sits</H>
          <ScenarioClassBars
            rows={rows.map((r) => ({ id: r.id, groups: r.byClass.map((g) => ({ cls: g.cls, usd: g.lostUsd })) }))}
            highlight={scenario}
            label="Lost or shifted supply by class under each scenario"
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
    .map((g) => `${g.info.label} ${plural(g.nodes.length, 'node')}${lostTotal > 0 ? `, ${fmtShare(g.lostUsd / lostTotal, 0)} of ${lostWord}` : ''}`)
    .join('; ');
  const coordWords = `${plural(nCoord, 'node')} ${nCoord === 1 ? 'is' : 'are'} COORDINATE, where new trade relationships among partners restore most of the gap.`;
  const takeaway =
    total === 0
      ? 'The export carries no classified node.'
      : `${mi ? `Managed Interdependence runs on ${plural(ranNodes, 'node')} where a supplier holds more of the allied market than the cap allows, and leaves the other ${unchanged} unchanged. ` : ''}${
          rare ? `Coordination alone is rare in this run: ${coordWords}` : coordWords
        }${
          biggest
            ? ` Most of the ${lostWord} (${fmtShare(biggest.lostUsd / lostTotal, 0)}) sits in ${biggest.info.label} nodes: ${biggest.info.meaning.charAt(0).toLowerCase()}${biggest.info.meaning.slice(1)}`
            : ''
        }`;
  const shockLine = hasScenarios ? scenarioShockLabel(m, scenario) : shockLabel;

  return (
    <div className={PAGE}>
      <PageHead
        eyebrow={AIB_EYEBROW}
        title="Allied Industrial Base"
        lede={
          hasScenarios
            ? 'Where does international coordination add value, who needs to coordinate, and what form should it take? The model starts from the observed trade network and changes it in two counterfactual ways: Decoupling removes a major supplier, Managed Interdependence caps the share any one supplier may hold where a supply-chain step is more concentrated than comparable goods. It then asks how much of the supply affected existing relationships, coordinated trade and new investment can replace, node by node across the energy supply chains.'
            : 'Where does international coordination add value, who needs to coordinate, and what form should it take? The model removes a major supplier from the observed trade network and asks how much of the lost supply existing relationships, coordinated trade and new investment can restore, node by node across the energy supply chains.'
        }
      >
        <p className="mt-3 max-w-3xl text-sm text-paper-mute" data-testid="overview-run">
          {hasScenarios ? (
            <>
              Shown: <span className="text-paper-dim">{sc.label}</span> ({shockLine}); supply assumption: {m.headlineScenario}
            </>
          ) : (
            <>
              Headline: <span className="text-paper-dim">{shockLabel}</span>; supply assumption: {m.headlineScenario}
            </>
          )}
          ; trade in {m.tradeYear}; {plural(total, 'supply-chain node')} (groups of traded goods, finer than the technology × stage cells of{' '}
          <a className="link" href={`#${DIVISION_SECTION.id}`}>
            {DIVISION_SECTION.title.toLowerCase()}
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
            title="Three scenarios compared"
            intro={
              <>
                <BasisTag basis="model" scenario="every scenario" />
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
          title="Where does coordination matter?"
          intro={
            <>
              <span className="flex flex-wrap items-center gap-2">
                <BasisTag basis="facts" />
                {hasScenarios && <ScenarioTag id={scenario} meta={m} />}
              </span>
              <p className="mt-2">
                Every supply-chain node of the model (a group of traded goods at one stage, finer than the technology × stage cells of {DIVISION_SECTION.title.toLowerCase()}) gets one treatment from
                its own results{mi ? ' under the scenario' : ''}.{' '}
                {nHigh !== null && typeof high === 'number' ? `${plural(nHigh, 'node')} lose at least ${fmtShare(high, 0)} of their supply to the shock (high severity). ` : ''}
                {takeaway}
              </p>
            </>
          }
        >
          <ul className="grid gap-3 sm:grid-cols-2 lg:grid-cols-4" aria-label="Nodes by treatment">
            {shown.map((cls) => {
              const n = byCls.get(cls)?.nodes.length ?? 0;
              const ls = lostShare(cls);
              return (
                <li key={cls}>
                  <StatTile
                    label={classInfo(cls).label}
                    value={plural(n, 'node')}
                    chip={<ClassSwatch cls={cls} />}
                    sub={`${ranNodes ? fmtShare(n / ranNodes, 0) : '–'} of ${mi ? 'the nodes it runs on' : 'nodes'}${ls !== null ? ` · ${fmtShare(ls, 0)} of ${lostWord}` : ''}`}
                  />
                  <p className="mt-1.5 px-1 text-xs leading-relaxed text-paper-mute">{classInfo(cls).meaning}</p>
                </li>
              );
            })}
          </ul>
          {mi && unchanged > 0 && (
            <p className="text-xs text-paper-mute" data-testid="overview-unchanged">
              {plural(unchanged, 'node')} {unchanged === 1 ? 'is' : 'are'} unchanged under Managed Interdependence: not more concentrated than {unchanged === 1 ? 'its' : 'their'} benchmark, within the cap
              in the allied market, without a benchmark, or not assessed.
            </p>
          )}
          {insufficient > 0 && <p className="text-xs text-paper-mute">{plural(insufficient, 'node')} with too little trade to classify (INSUFFICIENT DATA).</p>}
          <div className="panel p-4">
            <p className="mb-2 text-sm text-paper-dim">Where the {lostWord} sits, by treatment</p>
            {lostTotal > 0 ? (
              <LostByClassBar groups={classed} label={`${mi ? 'Shifted' : 'Lost'} supply by class. ${shareLine}`} />
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
                {mi ? 'Largest share of supply to shift' : 'Highest shock severity'}
              </h3>
              <p className="mb-2 mt-1 text-sm text-paper-mute">
                {mi ? "Share of each node's allied demand its capped suppliers hold above the cap." : "Share of each node's demand the shock removes."}
              </p>
              <NodeList rows={severity} value={(r) => fmtShare(r.exposure, 0)} label={mi ? 'Nodes with the largest share of supply to shift' : 'Nodes with the highest shock exposure'} />
            </section>
            <section aria-labelledby="div-h" className="card p-5">
              <h3 id="div-h" className="rule-neis-sm text-base font-medium text-paper">
                Largest coordination dividends
              </h3>
              <p className="mb-2 mt-1 text-sm text-paper-mute">
                Coverage that coordinated (optimized) trade adds over existing relationships{mi ? ', with the cap holding' : ''}.
              </p>
              <NodeList rows={dividends} value={(r) => fmtPp(r.dividend, 0)} label="Nodes with the largest coordination dividend" />
            </section>
          </div>
          <p className="text-sm">
            <Link className="link" to={href('/compacts#map')}>
              Every node on the strategic map &rarr;
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
          title="Who coordinates, and how"
          intro={
            <p>
              Each page answers one question about the emerging Allied Industrial Base{hasScenarios ? ` under the scenario chosen above (${sc.label})` : ''}.
            </p>
          }
        >
          <ul className="grid gap-4 md:grid-cols-3">
            <QuestionCard
              question="Which trade, investment and development relationships matter?"
              answer={
                trade
                  ? `${plural(trade.length, 'coordinated trade relationship')} across the compacts; eligible investment hosts in ${plural(hostNodes, 'node')}; the partner scores by technology and stage (Friendshore, Partnership Opportunity, Development Potential).`
                  : `Coordinated trade relationships across the compacts; eligible investment hosts in ${plural(hostNodes, 'node')}; the partner scores by technology and stage (Friendshore, Partnership Opportunity, Development Potential).`
              }
              to={href('/aib/partnerships')}
              cta="Partnerships"
            />
            <QuestionCard
              question="Which supply chains justify a Sectoral Compact, and who belongs?"
              answer={
                compacts
                  ? `${plural(types.candidate, 'candidate compact')}${types.bilateral ? `, ${types.bilateral} where a bilateral arrangement is enough` : ''}${types.unnecessary ? `, ${types.unnecessary} where existing relationships do nearly as well` : ''}${
                      noCoalition ? `; ${noCoalition} of the ${doc.compacts.length} compacts need no coalition under Managed Interdependence` : ''
                    }. Each compact names its nodes, members and what governments should do.`
                  : `${plural(doc.compacts.length, 'compact')}, each solved again under Managed Interdependence. Each compact names its nodes, members and what governments should do.`
              }
              to={href('/compacts')}
              cta="Sectoral Compacts"
            />
            <QuestionCard
              question="What roles do different countries play?"
              answer={
                roleRows
                  ? `${plural(roleRows.length, 'economy', 'economies')} hold a role in at least one compact: demand anchors, specialized and resource suppliers, investment hosts, technology and bridge partners.`
                  : 'Demand anchors, specialized and resource suppliers, investment hosts, technology and bridge partners, compact by compact.'
              }
              to={href('/aib/roles')}
              cta="Country roles"
            />
          </ul>
          <p className="max-w-3xl text-sm text-paper-mute">
            These are observed and modelled facts: they do not change with the Perspective (preference) settings of a Country profile. Where the Partnerships page ranks partners by
            preference-weighted scores it says so and shows the NEIS defaults.
          </p>
        </Section>
      </div>
    </div>
  );
}
