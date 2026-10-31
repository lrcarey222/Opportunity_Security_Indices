// 1. Compact overview (the included nodes with their classes, policy needs and any physical-capability
//    warning; the archetype with the class mix of the lost supply and the secondary class; the type,
//    "compact unnecessary" muted with its explanation; the policy needs, by compact and by node; the
//    coalition: the exact minimum coalition (its tie-break among minimum-size coalitions said), the
//    greedy heuristic beside it when it differs, or the widest greedy set when the middle target is out
//    of reach; and the six tiers (core coordinating members with their single substitutes as
//    alternatives, critical supply partners with how many economies together would replace them,
//    associate partners, critical, marginal critical and other external dependencies); the country x
//    role matrix with each economy's tier) and 2. Why it exists (shock exposure, compact vs fragmented
//    action for the demand anchors, coordination dividend, residual gaps, concentration). Each card has a
//    table twin and a CSV.
//
// The scenario toggle (2026-10-19): the class mix, the classes of the included nodes, the critical supply
// partners, the external dependency (Managed Interdependence removes no supplier: the suppliers it caps),
// the coalition and its tiers, the roles and the case for coordination follow the toggled scenario (the
// compact as the scenario solves it, `view`; ScenarioGate while its file loads, on a failure, or when the
// scenario has no coalition for the compact); the compact's suppliers today (compact.baseline) are listed
// beside them as the reference.

import { Fragment } from 'react';
import { Link } from 'react-router';
import { ChartCard } from '../../../charts/ChartCard.tsx';
import { TableView, type Col } from '../../../components/TableView.tsx';
import { EmptyState, Notice, StatTile, Tag } from '../../../components/ui.tsx';
import { useAibCtx } from '../../AibLayout.tsx';
import { ClassChip, ClassLegend, ClassSwatch, NodeClass } from '../../charts/ClassChip.tsx';
import { ScenarioTag } from '../../charts/ScenarioCompare.tsx';
import { TypeTag } from '../../charts/TypeTag.tsx';
import { AIB_MEASURE_DEFINITION, AIB_MEASURE_LABEL, COMPACT_IMPORTANCE, TERMS } from '../../../content/vocabulary.ts';
import { classInfo, classMix, parseClassMix, parseNodePolicyNeeds, physicalWarningWords, policyNeedsText, policyNeedsWords } from '../../derive/classes.ts';
import { targetLabel } from '../../derive/coalition.ts';
import { compactVsFragmented } from '../../derive/coverage.ts';
import { compactTypeInfo } from '../../derive/types.ts';
import { fmtPp, fmtShare, fmtUsd, listWords, plural } from '../../derive/format.ts';
import { concentration } from '../../derive/nodes.ts';
import { compactUnchangedReasonOf } from '../../derive/scenarioPages.ts';
import { compactScenario, compactSuppliersToday, nodeScenario, runMetaOf, scenarioCsvFields, scenarioInfo, scenarioLabel, type AibScenarioId } from '../../derive/scenarios.ts';
import { ROLE_ORDER, roleMatrix, type RoleRow } from '../../derive/roles.ts';
import { additionsNeeded, coalitionMethod, coalitionTiers, replacementWords, tierEconomies, tierName, tierShares, type TierEconomy } from '../../derive/tiers.ts';
import { AIB_PHYSICAL_WARNING, coalitionMembersOf, type AibCompact, type AibMeta, type AibNode } from '../../contract.ts';
import { cellStagesOfNodes, nodeCellWords, nodeStageRank, nodeStageWords, nodeTechWords, nodeWhere, osiFields, technologiesOfNodes } from '../../derive/taxonomy.ts';
import { csvWithRun, Section } from '../common.tsx';
import { Economy, ScenarioGate, useCompact } from './shared.tsx';

/** The coalition card's title: who has to take part (the minimum coalition and its tiers). */
export const COALITION_TITLE = 'Who needs to take part, and how important each partner is';
/** The roles card's title. */
export const ROLES_TITLE = 'Countries and their roles in the compact';
/** The compact-vs-fragmented tile ("fragmented action" in plain words, content/vocabulary.ts TERMS). */
export const COMPACT_VS_TODAY = `Compact vs ${TERMS.fragmentedAction}`;
/** "Compact unnecessary" in plain words (TERMS.compactUnnecessary). */
export const NO_COMPACT_NEEDED = TERMS.compactUnnecessary.charAt(0).toUpperCase() + TERMS.compactUnnecessary.slice(1);

/**
 * A CSV of one scenario's rows: the scenario named (aib_scenario, aib_scenario_shock) where the bundle has
 * the scenarios, and the run columns naming that scenario's shock (runMetaOf()).
 */
function scenarioCsv(rows: Record<string, unknown>[], meta: AibMeta, scenario: AibScenarioId, hasScenarios: boolean): string {
  const scn = hasScenarios ? scenarioCsvFields(meta, scenario) : null;
  return csvWithRun(scn ? rows.map((r) => ({ ...r, ...scn })) : rows, runMetaOf(meta, scenario));
}

/** " (Managed Interdependence)", " (Decoupling)" in a CSV's file name: every scenario named where the bundle has them (none is implied by default). */
const scenarioFileTag = (scenario: AibScenarioId, meta: AibMeta, hasScenarios: boolean): string => (hasScenarios ? ` (${scenarioLabel(scenario, meta)})` : '');

/** The toggled scenario as a tag beside a card's title (only when the bundle has the scenarios). */
export function ScenarioBadge() {
  const { scenario, hasScenarios, meta } = useCompact();
  return hasScenarios ? <ScenarioTag id={scenario} meta={meta} /> : null;
}

/** The suppliers Managed Interdependence caps in the compact's nodes (where it ran), in the order first met. */
function cappedSuppliers(nodes: readonly AibNode[]): string[] {
  const out: string[] = [];
  for (const n of nodes) {
    if (!nodeScenario(n, 'managed').ran) continue;
    for (const iso of n.diversify?.capped ?? []) if (!out.includes(iso)) out.push(iso);
  }
  return out;
}

/** The compact's nodes whose class carries the physical-capability warning (the exported list, else the nodes' own). */
function warnedNodes(physicalWarnings: readonly string[] | undefined, nodes: readonly AibNode[]): string[] {
  return physicalWarnings ? [...physicalWarnings] : nodes.filter((n) => n.warning).map((n) => n.label);
}

/**
 * `title`: the section heading (the compact page asks "Who and what?"); `policy`: list the policy needs
 * here, by compact and by node (false when the page shows them under "What should governments do?").
 * Critical supply partners are the members no single other candidate can replace (the coalition's
 * criticalPartners; the compact's suppliers in a bundle without it).
 */
export function OverviewSection({ title = 'Compact overview', policy = true }: { title?: string; policy?: boolean } = {}) {
  const { c, displayName, nodes, meta, name, shockLabel, tx, scenario, hasScenarios, view, viewState } = useCompact();
  const mi = scenario === 'managed';
  const critical = view ? (view.coalition.criticalPartners ?? view.suppliers).map((x) => name(x)) : null;
  const miRows = nodes.map((n) => nodeScenario(n, 'managed'));
  const mix = classMix(mi ? miRows.filter((r) => r.ran && r.cls).map((r) => r.cls as string) : nodes.map((n) => n.cls));
  const unchanged = mi ? miRows.filter((r) => !r.ran || !r.cls).length : 0;
  // The type, archetype, secondary class and policy needs of the compact as the toggled scenario solves it
  // (Managed Interdependence: its own; none without a coalition there or while its file loads).
  const typed = mi ? view : c;
  const type = typed ? compactTypeInfo(typed.type, compactVsFragmented(typed.metrics)?.gain) : null;
  const noneWords = viewState.status === 'loading' ? 'loading…' : viewState.status === 'none' ? 'none: no coalition under this scenario' : '–';
  const lostMix = parseClassMix(mi ? c.managed?.classMix : c.classMix);
  const capped = mi ? cappedSuppliers(nodes) : [];
  const today = compactSuppliersToday(c);
  const byNode = parseNodePolicyNeeds(c.nodePolicyNeeds);
  const warned = warnedNodes(c.physicalWarnings, nodes);
  const warning = nodes.find((n) => n.warning)?.warning ?? AIB_PHYSICAL_WARNING;
  const nodeCols: Col<AibNode>[] = [
    { id: 'node', header: 'Supply-chain step', value: (n) => n.label },
    { id: 'techs', header: tx.Many, value: (n) => nodeTechWords(n) },
    { id: 'stage', header: 'Stage', value: (n) => nodeStageWords(n), sortValue: (n) => nodeStageRank(n), title: "The stages of the sectors (technology × stage) the step's codes sit in; none when no code is among the traded goods this Explorer covers." },
    { id: 'cells', header: 'Sectors', value: (n) => nodeCellWords(n), minWidth: 200 },
    { id: 'cls', header: hasScenarios ? `What it needs under ${scenarioInfo(meta, 'decoupling').label}` : 'What it needs', value: (n) => classInfo(n.cls).label },
    ...(hasScenarios
      ? [
          {
            id: 'miCls',
            header: `What it needs under ${scenarioInfo(meta, 'managed').label}`,
            value: (n: AibNode) => {
              const r = nodeScenario(n, 'managed');
              return r.ran && r.cls ? classInfo(r.cls).label : 'unchanged';
            },
          },
        ]
      : []),
    { id: 'needs', header: 'Actions needed', value: (n) => policyNeedsWords(n.policyNeeds) ?? '–' },
    { id: 'warning', header: 'Warning', value: (n) => (n.warning ? physicalWarningWords(n.warning) : 'none'), minWidth: 200 },
    { id: 'treatment', header: 'Recommended treatment', value: (n) => n.treatment, minWidth: 220 },
  ];
  const nodesCsv = () => ({
    name: `${c.id} included nodes`,
    text: csvWithRun(
      nodes.map((n) => ({
        compact: c.id, node_id: n.id, sub_sector: n.label,
        ...((p) => ({ technologies: p.technologies, osi_cells: p.osiCells, dictionary_sector: p.dictionarySector, stage: p.stages, aib_model_stage: p.modelStage }))(osiFields(tx, n)),
        classification: n.cls,
        ...(hasScenarios ? { class_managed_interdependence: nodeScenario(n, 'managed').ran ? nodeScenario(n, 'managed').cls : null } : {}),
        policy_needs: policyNeedsText(n.policyNeeds), classification_warning: n.warning ?? null, physical_validation: n.physicalValidation ?? null, recommended_treatment: n.treatment,
      })),
      meta,
    ),
  });
  return (
    <Section
      id="overview"
      title={title}
      intro={
        <p>
          The supply-chain steps the compact covers, what each needs, and which countries take part in what role
          {hasScenarios ? ' under the scenario chosen above' : ''}. {mix.length ? `Steps by what they need: ${listWords(mix.map((m) => `${classInfo(m.cls).label} ${m.n}`))}` : 'No step changes'}
          {unchanged ? `; ${unchanged} unchanged under Managed Interdependence` : ''}.
        </p>
      }
    >
      {warned.length > 0 && (
        <Notice kind="warning" title={`${physicalWarningWords(warning)}: ${listWords(warned)}`}>
          <span data-testid="compact-physical-warnings">
            Production evidence is weak: the treatment of {warned.length === 1 ? 'this step' : 'these steps'} ({classInfo('LOW INTERVENTION').label} or {classInfo('COORDINATE').label}) rests on
            trade data that production data do not confirm. The production data are incomplete, or production and trade concentration diverge while the production rule was not applied
            or the treatment rests on low trade exposure. Each step&apos;s production check is under Supply-chain step by step.
          </span>
        </Notice>
      )}
      <ChartCard
        headingLevel={3}
        title="The supply-chain steps covered and the compact's design"
        badge={<ScenarioBadge />}
        sub={`What the compact covers, what each step needs and which policy tools follow.${type ? ` ${type.short}.` : ''}`}
        legend={<ClassLegend classes={mix.map((m) => m.cls)} />}
        table={() => <TableView rows={nodes} cols={nodeCols} caption={`${displayName}: supply-chain steps covered`} rowKey={(n) => n.id} pageSize={50} />}
        csv={nodesCsv}
      >
        <dl className="grid gap-x-6 gap-y-3 text-sm sm:grid-cols-[10rem_minmax(0,1fr)]">
          <dt className="text-paper-mute">Type of compact</dt>
          <dd className="flex flex-wrap items-center gap-x-4 gap-y-2">
            {typed ? <Tag>{typed.archetype}</Tag> : <span className="text-paper-dim">{noneWords}</span>}
            {lostMix.length > 0 && (
              <span className="inline-flex flex-wrap items-center gap-x-3 gap-y-1 text-paper-dim" data-testid="class-mix">
                <span className="text-paper-mute">What the {mi ? 'shifted' : 'lost'} supply needs:</span>
                {lostMix.map((m) => (
                  <span key={m.cls} className="inline-flex items-center gap-1.5">
                    <ClassSwatch cls={m.cls} />
                    {classInfo(m.cls).label} {Math.round(m.share * 100)}%
                  </span>
                ))}
              </span>
            )}
            {typed?.secondaryClass && (
              <span className="text-paper-dim" data-testid="secondary-class">
                <span className="text-paper-mute">Secondary treatment:</span> {classInfo(typed.secondaryClass).label}
              </span>
            )}
          </dd>
          <dt className="text-paper-mute">Type</dt>
          <dd className="flex flex-wrap items-center gap-x-3 gap-y-1">
            {type ? (
              <>
                <TypeTag info={type} />
                <span className="text-paper-dim">{type.explanation}</span>
              </>
            ) : (
              <span className="text-paper-dim">{noneWords}</span>
            )}
          </dd>
          <dt className="text-paper-mute">{tx.Many}</dt>
          <dd className="text-paper-dim" data-testid="compact-technologies">{technologiesOfNodes(nodes).join(', ') || '–'}</dd>
          <dt className="text-paper-mute">Stages</dt>
          <dd className="text-paper-dim">{cellStagesOfNodes(nodes).join(', ') || '–'}</dd>
          <dt className="text-paper-mute">Supply-chain steps</dt>
          <dd>
            <ul className="flex flex-wrap gap-2">
              {nodes.map((n, i) => (
                <li key={n.id} className="inline-flex flex-wrap items-center gap-2 border hairline px-2 py-1">
                  {mi ? miRows[i].ran && miRows[i].cls ? <ClassChip cls={miRows[i].cls as string} /> : <span className="tag text-paper-mute">Unchanged</span> : <NodeClass n={n} />}
                  <span className="text-paper">{n.label}</span>
                  <span className="text-xs text-paper-mute">{nodeWhere(n)}</span>
                </li>
              ))}
            </ul>
          </dd>
          <dt className="text-paper-mute">Critical supply partners</dt>
          <dd className="text-paper-dim">
            {critical ? (critical.length ? listWords(critical) : 'none named') : viewState.status === 'loading' ? 'loading…' : viewState.status === 'none' ? 'none: no coalition under this scenario' : '–'}
          </dd>
          <dt className="text-paper-mute">External dependency</dt>
          <dd className="text-paper-dim" data-testid="external-dependency">
            {mi
              ? `Managed Interdependence removes no supplier; ${capped.length ? `it caps ${listWords(capped.map((x) => name(x)))} in the allied market` : 'it caps no supplier in this compact'}`
              : `The supplier the disruption removes: ${shockLabel}`}
          </dd>
          {today.length > 0 && (
            <>
              <dt className="text-paper-mute">Suppliers today</dt>
              <dd className="text-paper-dim" data-testid="suppliers-today">
                {listWords(today.map((t) => `${name(t.iso3)} ${fmtShare(t.share, 0)}`))}{' '}
                <span className="text-paper-mute">(shares of the compact&apos;s allied demand, {meta.tradeYear}, before any disruption)</span>
              </dd>
            </>
          )}
          {policy && (
            <>
              {byNode.length > 0 && (
                <>
                  <dt className="text-paper-mute">Actions needed at each step</dt>
                  <dd>
                    <NodePolicyNeeds byNode={byNode} />
                  </dd>
                </>
              )}
              <dt className="text-paper-mute">{lostMix.length > 1 ? 'Policy tools (for each major treatment)' : 'Policy tools'}</dt>
              <dd>
                {typed ? <PolicyNeeds needs={typed.policyNeeds} /> : <span className="text-paper-mute">{noneWords}</span>}
              </dd>
            </>
          )}
        </dl>
      </ChartCard>
      <CoalitionCard />
      <RolesCard />
    </Section>
  );
}

/** The compact's policy tools (the instruments of its major treatments). */
export function PolicyNeeds({ needs }: { needs: readonly string[] }) {
  return needs.length ? (
    <ul className={`list-disc space-y-0.5 pl-5 text-paper-dim ${needs.length > 6 ? 'sm:columns-2 lg:columns-3' : ''}`}>
      {needs.map((p) => (
        <li key={p}>{p}</li>
      ))}
    </ul>
  ) : (
    <span className="text-paper-mute">None listed</span>
  );
}

/** The policy needs of each node (multi-label: a node can need several instruments). */
export function NodePolicyNeeds({ byNode }: { byNode: readonly { node: string; needs: string }[] }) {
  return (
    <ul className="space-y-0.5 text-paper-dim" data-testid="node-policy-needs">
      {byNode.map((x) => (
        <li key={x.node}>
          <span className="text-paper">{x.node}</span>: {x.needs || '–'}
        </li>
      ))}
    </ul>
  );
}

/** Tiers listed inline (names separated by commas) rather than one per line: they can run to dozens. */
const INLINE_TIERS = ['critical external dependency (marginal)'];

function CoalitionCard() {
  const { view } = useCompact();
  if (!view) {
    return (
      <ChartCard headingLevel={3} title={COALITION_TITLE} badge={<ScenarioBadge />}>
        <ScenarioGate what="the coalition">{() => null}</ScenarioGate>
      </ChartCard>
    );
  }
  return <CoalitionCardOf c={view} />;
}

function CoalitionCardOf({ c }: { c: AibCompact }) {
  const { displayName, meta, names, name, scenario, hasScenarios } = useCompact();
  const { doc } = useAibCtx();
  const mt = coalitionMethod(c, meta.targets);
  const mid = targetLabel(mt.mid);
  const groups = coalitionTiers(c, names, meta.targets, tierShares(doc.compacts));
  const econ = tierEconomies(c, names);
  const members = mt.members;
  const greedy = mt.greedy;
  const extra = greedy && mt.greedyDiffers ? greedy.length - members.length : 0;
  const tieBreak = 'among coalitions of that size the exact search keeps the one with the widest margin over the target at its weakest step, then the one that adds the most supply';
  const cols: Col<TierEconomy>[] = [
    { id: 'name', header: 'Economy', value: (e) => e.name },
    { id: 'tier', header: COMPACT_IMPORTANCE, value: (e) => tierName(e.tier) },
    { id: 'critical', header: 'Critical', value: (e) => (e.critical ? 'yes' : 'no') },
    { id: 'loo', header: 'Needed with the coalition fixed', value: (e) => e.looCritical, title: 'Leave one out: losing its supply pushes a supply-chain step below the target with the rest of the coalition unchanged' },
    {
      id: 'subs',
      header: 'Single substitutes (any one could replace it)',
      value: (e) => (e.substituteNames.length ? e.substituteNames.join(', ') : '–'),
      minWidth: 200,
    },
    { id: 'needed', header: 'Economies needed to replace it', value: (e) => additionsNeeded(e, mt.reached), minWidth: 120 },
    { id: 'roles', header: 'Roles', value: (e) => e.roles.join('; ') || '–', minWidth: 220 },
  ];
  const csv = () => ({
    name: `${c.id} coalition tiers${scenarioFileTag(scenario, meta, hasScenarios)}`,
    text: scenarioCsv(
      econ.map((e) => ({
        compact: c.id, iso3: e.iso3, economy: e.name, tier: e.tier, critical: e.critical, loo_critical: e.looCritical, single_substitutes: e.substitutes.join(' '),
        substitutes_needed: e.substitutesNeeded ?? null, roles: e.roles.join('; '),
        in_coalition: members.includes(e.iso3), in_greedy_coalition: greedy ? greedy.includes(e.iso3) : null,
      })),
      meta,
      scenario,
      hasScenarios,
    ),
  });
  const lead = mt.exact
    ? `The members are the smallest group of partners that meets the target: the fewest countries whose coordination brings every supply-chain step to the ${mid} target for the demand anchors' demand, the anchors always among them.${
        mt.tieBreakSolved === false ? ' Among groups of that size the search ran out of time before finding the one with the widest margin over the target.' : ''
      }`
    : mt.reached
      ? !mt.proven
        ? `The exact search hit its time limit at the ${mid} target: the members are the best coalition it found, and that it is the smallest is not proven.`
        : `The members are the smallest coalition that meets the ${mid} target.`
      : `No coalition brings every supply-chain step to the ${mid} target; the members are the widest coalition found by adding partners one at a time (its weakest step reaches ${fmtShare(c.coalition.maxCoverage)}). With the target out of reach, no member is tested for substitutes and none is called a critical supply partner.`;
  // the search method (exact program, tie-break, greedy heuristic) stays one click away (guide section 12)
  const searchNote = mt.exact ? `${tieBreak[0].toUpperCase()}${tieBreak.slice(1)}.${mt.tieBreakSolved === false ? ' Here that tie-break hit its time limit: the members are a coalition of the smallest size, not necessarily the one with the widest margin.' : ''}` : null;
  const greedyLine =
    greedy === null
      ? null
      : mt.greedyDiffers
        ? `A greedy heuristic (adding, one at a time, the economy that most reduces the shortfall) reaches the ${mid} target with ${plural(greedy.length, 'economy', 'economies')}: ${listWords(greedy.map((x) => name(x)))}. It is a different coalition${
            extra > 0
              ? `, ${plural(extra, 'economy', 'economies')} larger than the exact minimum.`
              : extra === 0
                ? ` of the same size${
                    !mt.exact
                      ? '.'
                      : mt.tieBreakSolved === false
                        ? `; the exact program's tie-break among coalitions of that size hit its time limit, so neither is shown to have the wider margin.`
                        : `; ${tieBreak}.`
                  }`
                : '.'
          }`
        : greedy.length
          ? 'The greedy heuristic (adding, one at a time, the economy that most reduces the shortfall) finds the same coalition.'
          : mt.reached
            ? `The greedy heuristic does not reach the ${mid} target.`
            : null;
  const shown = groups.filter(
    (g) =>
      g.economies.length ||
      g.tier === 'core coordinating member' ||
      (mt.reached && g.tier === 'critical supply partner') ||
      (mt.reached && g.tier === 'associate partner' && c.coalition.associatesReach99 === false),
  );
  return (
    <ChartCard
      headingLevel={3}
      title={COALITION_TITLE}
      badge={<ScenarioBadge />}
      sub={`${mt.label}: ${plural(members.length, 'country', 'countries')}. ${AIB_MEASURE_DEFINITION.minimumCoalition}${mt.method ? ` Method: ${mt.method}.` : ''}`}
      table={() => <TableView rows={econ} cols={cols} caption={`${displayName}: who takes part and how important each partner is`} pageSize={50} />}
      csv={csv}
    >
      <p className="max-w-3xl text-sm leading-relaxed text-paper-dim" data-testid="coalition-method">
        {lead}
      </p>
      {(searchNote || greedyLine) && (
        <details className="mt-2 max-w-3xl text-sm text-paper-dim" data-testid="coalition-search">
          <summary className="cursor-pointer text-paper-mute">How this group was found</summary>
          {searchNote && <p className="mt-1 leading-relaxed">{searchNote}</p>}
          {greedyLine && (
            <p className="mt-1 leading-relaxed" data-testid="greedy-coalition">
              {greedyLine}
            </p>
          )}
        </details>
      )}
      <dl className="mt-4 grid gap-x-6 gap-y-3 text-sm sm:grid-cols-[14rem_minmax(0,1fr)]" data-testid="coalition-tiers">
        {shown.map((g) => (
          <div key={g.tier} className="contents">
            <dt className="text-paper">
              {g.label} <span className="text-paper-mute">({g.economies.length})</span>
              <span className="mt-0.5 block text-xs font-normal text-paper-mute">{g.meaning}</span>
            </dt>
            <dd className="text-paper-dim">
              {!g.economies.length ? (
                <span className="text-paper-mute">none</span>
              ) : INLINE_TIERS.includes(g.tier) ? (
                <p data-testid="inline-tier">
                  {g.economies.map((e, i) => (
                    <Fragment key={e.iso3}>
                      {i > 0 && ', '}
                      <Economy iso3={e.iso3} />
                    </Fragment>
                  ))}
                </p>
              ) : (
                <ul className="space-y-0.5">
                  {g.economies.map((e) => {
                    const why = replacementWords(e, mt.mid, mt.reached);
                    return (
                      <li key={e.iso3}>
                        <Economy iso3={e.iso3} />
                        {e.anchor && <span className="ml-1 text-xs text-paper-mute">demand anchor</span>}
                        {why && <span className="text-paper-mute">: {why}</span>}
                      </li>
                    );
                  })}
                </ul>
              )}
            </dd>
          </div>
        ))}
      </dl>
    </ChartCard>
  );
}

function RolesCard() {
  const { view } = useCompact();
  if (!view) {
    return (
      <ChartCard headingLevel={3} title={ROLES_TITLE} badge={<ScenarioBadge />}>
        <ScenarioGate what="the roles">{() => null}</ScenarioGate>
      </ChartCard>
    );
  }
  return <RolesCardOf c={view} />;
}

function RolesCardOf({ c }: { c: AibCompact }) {
  const { displayName, meta, names, name, href, scenario, hasScenarios } = useCompact();
  const rows = roleMatrix(c, names);
  const tiers = new Map(tierEconomies(c, names).map((e) => [e.iso3, e]));
  const flat = c.roles.map((r) => ({ ...r, name: name(r.iso3), tierName: tierName(tiers.get(r.iso3)?.tier ?? r.tier) }));
  const cols: Col<(typeof flat)[number]>[] = [
    { id: 'name', header: 'Economy', value: (r) => r.name },
    { id: 'role', header: 'Role', value: (r) => r.role },
    { id: 'basis', header: 'Why', value: (r) => r.basis, minWidth: 240 },
    { id: 'tier', header: COMPACT_IMPORTANCE, value: (r) => r.tierName },
    { id: 'member', header: 'In the coalition', value: (r) => (r.member ? 'yes' : 'no') },
  ];
  const csv = () => ({
    name: `${c.id} members and roles${scenarioFileTag(scenario, meta, hasScenarios)}`,
    text: scenarioCsv(
      c.roles.map((r) => ({
        compact: c.id, iso3: r.iso3, economy: name(r.iso3), role: r.role, basis: r.basis, in_coalition: r.member,
        tier: tiers.get(r.iso3)?.tier ?? r.tier ?? null, critical: r.critical ?? null, loo_critical: r.looCritical ?? null,
        single_substitutes: (r.substitutes ?? []).join(' '), substitutes_needed: r.substitutesNeeded ?? null,
      })),
      meta,
      scenario,
      hasScenarios,
    ),
  });
  const nMembers = coalitionMembersOf(c).length;
  const nOutside = rows.filter((r) => !r.member).length;
  return (
    <ChartCard
      headingLevel={3}
      title={ROLES_TITLE}
      badge={<ScenarioBadge />}
      sub={`${plural(nMembers, 'country', 'countries')} in the coalition${nOutside ? `; ${plural(nOutside, 'country', 'countries')} outside it (associates or external dependencies)` : ''}. Roles come from the compact's results and describe what each country contributes; a country can hold several. Hover a mark for the reason.`}
      table={() => <TableView rows={flat} cols={cols} caption={`${displayName}: economies and roles`} pageSize={50} />}
      csv={csv}
    >
      {rows.length ? <RoleMatrix rows={rows} label={displayName} href={href} /> : <EmptyState>No country holds a role in this compact under this scenario.</EmptyState>}
    </ChartCard>
  );
}

/** Row headers read as names, not column labels, and stay in view when the matrix scrolls sideways. */
const ROW_HEAD = {
  position: 'sticky',
  left: 0,
  zIndex: 2,
  background: 'var(--color-ink-800)',
  color: 'var(--color-paper)',
  textTransform: 'none',
  letterSpacing: 'normal',
  fontSize: '0.8125rem',
  fontWeight: 400,
  borderBottom: '1px solid rgb(255 255 255 / 0.08)',
} as const;

function RoleMatrix({ rows, label, href }: { rows: RoleRow[]; label: string; href: (to: string) => string }) {
  return (
    <div className="relative overflow-x-auto">
      <table className="data-table">
        <caption className="sr-only">{label}: the role of each economy</caption>
        <thead>
          <tr>
            <th scope="col">Economy</th>
            <th scope="col" className="align-bottom" style={{ minWidth: 120, whiteSpace: 'normal', letterSpacing: '0.04em' }}>
              {COMPACT_IMPORTANCE}
            </th>
            {ROLE_ORDER.map((r) => (
              <th key={r} scope="col" className="align-bottom" style={{ minWidth: 76, maxWidth: 120, whiteSpace: 'normal', letterSpacing: '0.04em', textAlign: 'center' }}>
                {r}
              </th>
            ))}
          </tr>
        </thead>
        <tbody>
          {rows.map((row) => (
            <tr key={row.iso3}>
              <th scope="row" className="whitespace-nowrap text-left" style={ROW_HEAD}>
                <Link className="link" to={href(`/countries/${row.iso3}#aib-role`)} title={`${row.name}: Country profile, role in the Allied Industrial Base`}>
                  <Economy iso3={row.iso3} />
                </Link>
              </th>
              <td className={`whitespace-nowrap text-xs ${row.member ? 'text-paper-dim' : 'text-paper-mute'}`}>
                {tierName(row.tier)}
              </td>
              {ROLE_ORDER.map((r) => {
                const basis = row.roles[r];
                return (
                  <td key={r} className="text-center">
                    {basis !== undefined ? (
                      <span title={basis} className={row.member ? 'text-paper' : 'text-paper-mute'}>
                        <span aria-hidden="true">●</span>
                        <span className="sr-only">yes: {basis}</span>
                      </span>
                    ) : (
                      <span className="sr-only">no</span>
                    )}
                  </td>
                );
              })}
            </tr>
          ))}
        </tbody>
      </table>
      {rows.some((r) => r.other.length) && (
        <p className="mt-2 text-xs text-paper-mute">
          Other roles: {rows.flatMap((r) => r.other.map((o) => `${o.role} (${r.name})`)).join('; ')}.
        </p>
      )}
    </div>
  );
}

/** The largest supplier share after optimized trade under Managed Interdependence (today's share where it does not run). */
function miConcentration(nodes: readonly AibNode[]): { node: AibNode; share: number; supplier: string | null } | null {
  let best: { node: AibNode; share: number; supplier: string | null } | null = null;
  for (const n of nodes) {
    const r = nodeScenario(n, 'managed');
    if (typeof r.largestShare !== 'number') continue;
    if (!best || r.largestShare > best.share) best = { node: n, share: r.largestShare, supplier: r.largest };
  }
  return best;
}

/** `title`: the section heading (the compact page asks "Why this compact?"). */
export function WhySection({ title = 'Why it exists' }: { title?: string } = {}) {
  const { c, displayName, view, viewState, scenario, hasScenarios, nodes, meta, name } = useCompact();
  const { nodeOf } = useAibCtx();
  const mi = scenario === 'managed';
  // Managed Interdependence: the compact as its file solves it, else its figures from compact.managed (loading, or no coalition)
  const s = mi ? compactScenario(c, 'managed', nodeOf) : null;
  const m = view
    ? view.metrics
    : s
      ? { ...c.metrics, lostUsd: s.lostUsd, exposure: s.exposure, static: s.static, existing: s.existing, trade: s.optimized, investment: null, dividend: s.dividend, residual: s.residual, residualAfterInvestment: null, globalOptimum: s.optimized, anchorFragmented: null, anchorCompact: null, compactGain: null }
      : c.metrics;
  const conc = mi ? miConcentration(nodes) : concentration(nodes);
  const vs = compactVsFragmented(m);
  // The compact's type under the toggled scenario (Managed Interdependence: its own; none without a coalition there).
  const typed = mi ? view : c;
  const type = typed ? compactTypeInfo(typed.type, compactVsFragmented(typed.metrics)?.gain) : null;
  const gap = typeof m.residual === 'number' && m.residual < 0.005 ? 'closing the gap' : `leaving a remaining gap of ${fmtShare(m.residual, 0)}`;
  const tradeWords = view ? 'coordinated trade among the members' : 'coordinated trade among every potential supplier';
  const answer =
    mi && s && !s.ran
      ? `Managed Interdependence leaves this compact unchanged (${viewState.reason ?? compactUnchangedReasonOf(nodes)}): no supply has to shift and its demand stays served as it is today.`
      : `${mi ? `Managed Interdependence shifts ${fmtShare(m.exposure, 0)} of the compact's demand away from capped suppliers` : `The disruption removes ${fmtShare(m.exposure, 0)} of the compact's demand`}. After it, existing relationships keep ${fmtShare(m.existing, 0)} of that demand supplied and ${tradeWords} ${fmtShare(m.trade, 0)}, ${gap}${
          vs ? `. Acting together, the demand anchors cover ${fmtPp(vs.gain).replace(/^\+/, '')} more of their own demand than through existing relationships alone` : ''
        }${conc ? `; ${name(conc.supplier)} alone supplies up to ${fmtShare(conc.share, 0)} of one step (${conc.node.label})` : ''}.`;
  const tiles = [
    mi
      ? { key: 'exposure', label: 'Share shifted', value: fmtShare(m.exposure), sub: `${fmtUsd(m.lostUsd)} of ${fmtUsd(m.demandUsd)} demand shifts away from capped suppliers`, raw: m.exposure }
      : { key: 'exposure', label: 'Supply exposed to the disruption', value: fmtShare(m.exposure), sub: `${fmtUsd(m.lostUsd)} of ${fmtUsd(m.demandUsd)} demand must find a new supplier`, raw: m.exposure },
    ...(vs
      ? [
          {
            key: 'compactVsFragmented',
            label: COMPACT_VS_TODAY,
            value: fmtPp(vs.gain),
            sub: `the share of the demand anchors' demand met: ${fmtShare(vs.fragmented)} through existing relationships, ${fmtShare(vs.compact)} with the compact's smallest coalition`,
            raw: vs.gain,
          },
        ]
      : []),
    { key: 'dividend', label: AIB_MEASURE_LABEL.dividend, value: fmtPp(m.dividend), sub: "the extra demand the members can meet by opening or expanding trade relationships, over existing relationships alone", raw: m.dividend },
    { key: 'residual', label: 'Remaining gap after trade', value: fmtShare(m.residual), sub: "demand the members' coordinated trade still cannot meet", raw: m.residual },
    { key: 'residualInv', label: 'Remaining gap after new capacity', value: fmtShare(m.residualAfterInvestment), sub: 'after a simulated capacity addition (capped at each host’s current output) in member countries, at the steps that need new capacity', raw: m.residualAfterInvestment },
    {
      key: 'concentration',
      label: 'Concentration',
      value: conc ? fmtShare(conc.share) : '–',
      sub: conc ? `${name(conc.supplier)} in ${conc.node.label}: the largest supplier share after coordinated trade, across the steps${mi ? ' under the scenario' : ''}` : 'no supplier shares after coordinated trade',
      raw: conc?.share ?? null,
    },
  ];
  const csv = () => ({
    name: `${c.id} why it exists${scenarioFileTag(scenario, meta, hasScenarios)}`,
    text: scenarioCsv(
      tiles.map((t) => ({ compact: c.id, measure: t.label, value: t.raw, meaning: t.sub })),
      meta,
      scenario,
      hasScenarios,
    ),
  });
  return (
    <Section
      id="why"
      title={title}
      intro={
        <p>
          <span className="text-paper" data-testid="why-answer">
            {answer}
          </span>{' '}
          {mi
            ? 'The compacts are formed under the major-supplier disruption, where it removes a material share of supply and acting together recovers more of it than existing relationships would; here each is solved again under the concentration cap.'
            : 'A compact forms where the disruption removes a material share of supply and acting together recovers more of it than existing relationships would.'}{' '}
          Shares are of the compact&apos;s traded
          demand today, weighted by each step&apos;s demand; the comparison with today&apos;s uncoordinated response is for the demand anchors&apos; own demand.
          {mi && !view ? ' The figures below are the compact’s steps under the scenario; the coalition’s own figures appear once its data are loaded.' : ''}
        </p>
      }
    >
      {type?.tone === 'unnecessary' && (
        <Notice kind="info" title={NO_COMPACT_NEEDED}>
          {type.explanation} The group is listed because its supply chains hang together and {mi ? 'the cap still shifts supply' : 'the disruption still removes supply'}; existing relationships, not a
          new coalition, carry most of the response.
        </Notice>
      )}
      <ChartCard
        headingLevel={3}
        title="The case for coordination"
        badge={<ScenarioBadge />}
        table={() => (
          <TableView
            rows={tiles}
            cols={[
              { id: 'label', header: 'Measure', value: (t) => t.label },
              { id: 'value', header: 'Value', value: (t) => t.value, num: true },
              { id: 'sub', header: 'What it is', value: (t) => t.sub, minWidth: 260 },
            ]}
            caption={`${displayName}: why the compact exists`}
          />
        )}
        csv={csv}
      >
        <div className="grid gap-3 sm:grid-cols-2 lg:grid-cols-3">
          {tiles.map((t) => (
            <StatTile key={t.key} label={t.label} value={t.value} sub={t.sub} />
          ))}
        </div>
      </ChartCard>
    </Section>
  );
}
