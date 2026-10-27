// 5. Coverage comparison (shock -> existing relationships -> trade compact -> trade + investment, in %
// of demand, with the global optimum, "if every economy coordinated", as a reference rule) and 6. How
// resilience changes as countries join (the greedy path against the 90 / 95 / 99% targets, with the
// exact minimum coalition of each target beside it: the greedy path can need more, or other,
// economies). Table twins and CSVs.
//
// The scenario toggle (2026-10-19): the coverage comparison opens with the compact under the three scenarios
// (Baseline, Managed Interdependence, Decoupling: kept / restored by trade / new capacity, the toggled one
// highlighted; ScenarioCoverageCompare), then the steps, who invests where and the coalition path of the
// compact as the toggled scenario solves it (`view`; ScenarioGate while its file loads, on a failure, or
// when the scenario has no coalition for the compact).

import { useMemo } from 'react';
import { ChartCard } from '../../../charts/ChartCard.tsx';
import { Legend } from '../../../charts/legends.tsx';
import { TableView, type Col } from '../../../components/TableView.tsx';
import { EmptyState } from '../../../components/ui.tsx';
import { CoalitionChart, CURVE_COLOR } from '../../charts/CoalitionChart.tsx';
import { CoverageBars, REFERENCE_COLOR } from '../../charts/CoverageBars.tsx';
import { STEP_COLOR } from '../../charts/CoverageStrip.tsx';
import { CoverageLegend, ScenarioCoverageCompare } from '../../charts/ScenarioCompare.tsx';
import type { AibCompact } from '../../contract.ts';
import { coalitionCurve, targetLabel, targetMarks, type CurvePoint } from '../../derive/coalition.ts';
import { coverageSteps, GLOBAL_OPTIMUM_LABEL, globalOptimum, largestGain, type CoverageStep } from '../../derive/coverage.ts';
import { fmtPp, fmtShare, fmtUsd, listWords, plural } from '../../derive/format.ts';
import { useAibCtx } from '../../AibLayout.tsx';
import { investmentPairs, partnerTotals } from '../../derive/investment.ts';
import { NO_FILTER } from '../../derive/partnerships.ts';
import { compactUnchangedReasonOf } from '../../derive/scenarioPages.ts';
import { compactScenarios, runMetaOf, scenarioCsvFields, scenarioInvestmentFlows, scenarioLabel, scenarioNameOf, type CompactScenario } from '../../derive/scenarios.ts';
import { csvWithRun, Section } from '../common.tsx';
import { ScenarioBadge } from './OverviewSections.tsx';
import { ScenarioGate, useCompact } from './shared.tsx';

/** The compact under the three scenarios: its allied demand kept, restored by trade and left for new capacity, the toggled scenario highlighted; a table twin and a CSV. */
function ScenarioCoverageCard() {
  const { c, meta, scenario, nodes, viewState } = useCompact();
  const { nodeOf } = useAibCtx();
  const rows = useMemo(() => compactScenarios(c, nodeOf), [c, nodeOf]);
  const managed = rows.find((r) => r.id === 'managed');
  const notes = managed && !managed.ran ? { managed: `unchanged: ${viewState.reason ?? compactUnchangedReasonOf(nodes)}` } : undefined;
  const label = `${c.name}: its allied demand under each scenario`;
  const cols: Col<CompactScenario>[] = [
    { id: 'scenario', header: 'Scenario', value: (r) => scenarioCsvFields(meta, r.id).aib_scenario },
    { id: 'nodes', header: 'Nodes the scenario changes', value: (r) => (r.id === 'baseline' ? 0 : r.nodesRun), num: true },
    { id: 'lost', header: 'Supply lost or shifted', value: (r) => r.lostUsd, cell: (r) => fmtUsd(r.lostUsd), num: true },
    { id: 'kept', header: 'Kept', value: (r) => r.split?.kept ?? null, cell: (r) => fmtShare(r.split?.kept, 0), num: true },
    { id: 'trade', header: 'Restored by trade', value: (r) => r.split?.trade ?? null, cell: (r) => fmtShare(r.split?.trade, 0), num: true },
    { id: 'build', header: 'New capacity', value: (r) => r.split?.build ?? null, cell: (r) => fmtShare(r.split?.build, 0), num: true },
    { id: 'gap', header: 'Left for new capacity', value: (r) => r.residualUsd, cell: (r) => fmtUsd(r.residualUsd), num: true },
    { id: 'mix', header: 'Class mix of the lost supply', value: (r) => r.classMix ?? 'none', minWidth: 200 },
  ];
  const csv = () => ({
    name: `${c.id} under the three scenarios`,
    text: csvWithRun(
      rows.map((r) => ({
        compact: c.id,
        ...scenarioCsvFields(meta, r.id),
        nodes: r.nodes,
        nodes_changed: r.id === 'baseline' ? 0 : r.nodesRun,
        allied_demand_usd: r.demandUsd,
        lost_or_shifted_usd: r.lostUsd,
        share_lost_or_shifted: r.exposure,
        kept: r.split?.kept ?? null,
        restored_by_trade: r.split?.trade ?? null,
        new_capacity: r.split?.build ?? null,
        left_for_new_capacity_usd: r.residualUsd,
        class_mix: r.classMix,
        baseline_hhi: r.id === 'baseline' ? r.hhi : null,
      })),
      meta,
    ),
  });
  return (
    <ChartCard
      headingLevel={3}
      title="Demand served under each scenario"
      sub="The compact's allied demand split to scale: what the market keeps after the shock or the cap, what existing relationships and optimized trade among every partner restore, and what only new capacity fills. Baseline is trade as it is today."
      summary={rows.map((r) => `${scenarioCsvFields(meta, r.id).aib_scenario}: ${r.split ? `kept ${fmtShare(r.split.kept, 0)}, restored by trade ${fmtShare(r.split.trade, 0)}, new capacity ${fmtShare(r.split.build, 0)}` : 'no figures'}`).join('; ')}
      legend={<CoverageLegend />}
      table={() => <TableView rows={rows} cols={cols} caption={label} rowKey={(r) => r.id} />}
      csv={csv}
    >
      <ScenarioCoverageCompare rows={rows} highlight={scenario} label={label} meta={meta} notes={notes} legend={false} />
    </ChartCard>
  );
}

export function CoverageSection() {
  const { view, hasScenarios } = useCompact();
  return (
    <Section
      id="coverage"
      title="Coverage comparison"
      intro={
        view ? (
          <CoverageIntro c={view} />
        ) : (
          <p>How much of the compact&apos;s traded demand is served after the shock or the cap, from that alone to the full response. Values are demand-weighted over the compact&apos;s nodes.</p>
        )
      }
    >
      {hasScenarios && <ScenarioCoverageCard />}
      {view ? (
        <CoverageStepsCard c={view} />
      ) : (
        <ChartCard headingLevel={3} title="Demand served at each step" badge={<ScenarioBadge />}>
          <ScenarioGate what="the coverage steps">{() => null}</ScenarioGate>
        </ChartCard>
      )}
      <InvestmentStep />
    </Section>
  );
}

function CoverageIntro({ c }: { c: AibCompact }) {
  const { scenario } = useCompact();
  const steps = coverageSteps(c.metrics, scenario === 'managed');
  const gain = largestGain(steps);
  const optimum = globalOptimum(c.metrics);
  return (
    <p>
      How much of the compact&apos;s traded demand is served after the {scenario === 'managed' ? 'cap' : 'shock'}, from that alone to the full response. Values are demand-weighted over the
      compact&apos;s nodes.
      {gain && gain.gain > 0 ? ` The largest step is from ${gain.from.label.toLowerCase()} to ${gain.to.label.toLowerCase()} (${fmtPp(gain.gain)}).` : ''} The trade compact is every
      economy&apos;s existing-relationship response plus the new and expanded relationships of the compact&apos;s minimum-coalition members, so it never falls below existing relationships
      {optimum !== null ? `; the dashed line is what coordination among every economy would reach (${fmtShare(optimum)}), a reference rather than a step` : ''}.
    </p>
  );
}

function CoverageStepsCard({ c }: { c: AibCompact }) {
  const { meta, scenario, hasScenarios } = useCompact();
  const steps = coverageSteps(c.metrics, scenario === 'managed');
  const optimum = globalOptimum(c.metrics);
  const rows = [
    ...steps.map((s, i) => ({ ...s, change: i > 0 && s.value !== null && steps[i - 1].value !== null ? (s.value as number) - (steps[i - 1].value as number) : null })),
    ...(optimum !== null
      ? [{ key: 'global', label: `${GLOBAL_OPTIMUM_LABEL} (reference)`, short: 'Every economy', meaning: 'every candidate partner coordinates its trade toward the demand scope: the global optimum, not a compact', value: optimum, change: null }]
      : []),
  ];
  const cols: Col<(typeof rows)[number]>[] = [
    { id: 'label', header: 'Step', value: (r) => r.label },
    { id: 'value', header: 'Demand served', value: (r) => r.value, cell: (r) => fmtShare(r.value), num: true },
    { id: 'change', header: 'Change on the step before', value: (r) => r.change, cell: (r) => (r.change === null ? '–' : fmtPp(r.change)), num: true },
    { id: 'meaning', header: 'What it assumes', value: (r) => r.meaning, minWidth: 280 },
  ];
  const csv = () => ({
    name: `${c.id} coverage comparison${hasScenarios ? ` ${scenarioLabel(scenario, meta).toLowerCase()}` : ''}`,
    text: csvWithRun(
      rows.map((r) => ({ compact: c.id, ...scenarioCsvFields(meta, scenario), step: r.label, demand_served: r.value, change_on_step_before: r.change, assumption: r.meaning })),
      runMetaOf(meta, scenario),
    ),
  });
  const summary = `${c.name}: ${steps.map((s: CoverageStep) => `${s.label} ${fmtShare(s.value)}`).join(', ')} of demand served${optimum !== null ? `; ${GLOBAL_OPTIMUM_LABEL.toLowerCase()}: ${fmtShare(optimum)}` : ''}.`;
  return (
      <ChartCard
        headingLevel={3}
        title="Demand served at each step"
        badge={<ScenarioBadge />}
        sub={`In % of the compact's demand. Gray: ${scenario === 'managed' ? 'the cap alone (Managed Interdependence removes no supplier)' : 'the shock alone'}; blue: the responses. The figure above each column is the change on the step before.${optimum !== null ? ' Dashed line: if every economy coordinated.' : ''}`}
        summary={summary}
        legend={
          <Legend
            label="Coverage steps"
            items={[
              { label: scenario === 'managed' ? 'Cap alone' : 'Shock alone', color: STEP_COLOR.static },
              { label: 'Responses', color: STEP_COLOR.response },
              ...(optimum !== null ? [{ label: GLOBAL_OPTIMUM_LABEL, color: REFERENCE_COLOR, kind: 'line' as const, dash: '6,4' }] : []),
            ]}
          />
        }
        table={() => <TableView rows={rows} cols={cols} caption={`${c.name}: coverage at each step`} />}
        csv={csv}
      >
        <CoverageBars steps={steps} ariaLabel={summary} reference={optimum} referenceLabel={GLOBAL_OPTIMUM_LABEL} />
      </ChartCard>
  );
}

/** The trade + investment step in words: who invests where for the compact's nodes (best-ranked hosts). */
function InvestmentStep() {
  const { c, name: baseName, nodeLabel, scenario, managedDoc } = useCompact();
  const { doc } = useAibCtx();
  const flows = scenarioInvestmentFlows(doc, scenario, managedDoc);
  const name = useMemo(() => scenarioNameOf(baseName, managedDoc), [baseName, managedDoc]);
  const pairs = useMemo(() => (flows ? investmentPairs({ nodes: doc.nodes, investmentFlows: flows }, { ...NO_FILTER, nodes: c.nodes }) : []), [doc.nodes, flows, c.nodes]);
  if (scenario === 'managed' && !managedDoc) return null;
  if (!flows) return null;
  const hosts = [...new Set(pairs.map((p) => p.host))]
    .map((h) => ({ host: h, investors: partnerTotals(pairs, h, 'host'), nodes: [...new Set(pairs.filter((p) => p.host === h).map((p) => p.node))] }))
    .map((x) => ({ ...x, usd: x.investors.reduce((s, t) => s + t.usd, 0) }))
    .sort((a, b) => b.usd - a.usd || a.host.localeCompare(b.host));
  return (
    <p className="max-w-3xl text-sm leading-relaxed text-paper-dim" data-testid="investment-step">
      <span className="text-paper">Trade + investment: who invests where. </span>
      {hosts.length ? (
        <>
          New capacity at the best-ranked host of each node with a remaining gap:{' '}
          {hosts.slice(0, 3).map((x, i) => (
            <span key={x.host}>
              {i > 0 && '; '}
              {name(x.host)} ({listWords(x.nodes.slice(0, 2).map(nodeLabel))}{x.nodes.length > 2 ? ' and more' : ''}) serving {listWords(x.investors.slice(0, 3).map((t) => `${name(t.iso3)} ${fmtUsd(t.usd)}`))}
              {x.investors.length > 3 ? ` and ${x.investors.length - 3} more` : ''}
            </span>
          ))}
          {hosts.length > 3 ? `; ${hosts.length - 3} more ${hosts.length - 3 === 1 ? 'host' : 'hosts'}` : ''}. The investor is the economy whose lost supply the capacity replaces;{' '}
          <a className="link" href="#invest">
            every flow below
          </a>
          .
        </>
      ) : (
        'No node of this compact has a gap that passes the investment threshold, so no investment flow is simulated for it.'
      )}
    </p>
  );
}

export function CoalitionSection() {
  const { view } = useCompact();
  if (!view) {
    return (
      <Section id="coalition" title="How resilience changes as countries join">
        <ScenarioGate what="the coalition path">{() => null}</ScenarioGate>
      </Section>
    );
  }
  return <CoalitionSectionOf c={view} />;
}

function CoalitionSectionOf({ c }: { c: AibCompact }) {
  const { meta, names, name, nodeLabel, scenario, hasScenarios } = useCompact();
  const points = coalitionCurve(c, names);
  const marks = targetMarks(points, meta.targets);
  const cols: Col<CurvePoint & { reaches: string }>[] = [
    { id: 'step', header: 'Step', value: (p) => p.step, num: true },
    { id: 'n', header: 'Members', value: (p) => p.n, num: true },
    { id: 'added', header: 'Economy added', value: (p) => p.addedName },
    { id: 'min', header: 'Minimum coverage', value: (p) => p.min, cell: (p) => fmtShare(p.min), num: true },
    { id: 'mean', header: 'Mean coverage', value: (p) => p.mean, cell: (p) => fmtShare(p.mean), num: true },
    { id: 'reaches', header: 'First to reach', value: (p) => p.reaches || null },
  ];
  const rows = points.map((p) => ({
    ...p,
    reaches: marks
      .filter((m) => m.point?.step === p.step)
      .map((m) => `${targetLabel(m.target)} target`)
      .join(', '),
  }));
  const nodeIds = [...new Set(points.flatMap((p) => Object.keys(p.nodes)))];
  const csv = () => ({
    name: `${c.id} coalition path${hasScenarios ? ` ${scenarioLabel(scenario, meta).toLowerCase()}` : ''}`,
    text: csvWithRun(
      rows.map((p) => ({
        compact: c.id,
        step: p.step,
        members_count: p.n,
        added_iso3: p.added,
        added: p.added ? p.addedName : null,
        members: p.members.join(' '),
        min_coverage: p.min,
        mean_coverage: p.mean,
        first_to_reach: p.reaches || null,
        ...Object.fromEntries(nodeIds.map((id) => [`coverage_${id}`, p.nodes[id] ?? null])),
        ...scenarioCsvFields(meta, scenario),
      })),
      runMetaOf(meta, scenario),
    ),
  });
  const coalitions: { t: number; list: string[] }[] = [];
  const lists = [c.coalition.t90, c.coalition.t95, c.coalition.t99];
  [...meta.targets].sort((a, b) => a - b).forEach((t, i) => coalitions.push({ t, list: lists[i] ?? [] }));
  const exact = /^exact/i.test(c.coalition.method ?? '');
  const summary = `${c.name}: minimum coverage of the anchors' demand from ${fmtShare(points[0]?.min ?? null)} with the demand anchors to ${fmtShare(points[points.length - 1]?.min ?? null)} with ${plural(points[points.length - 1]?.n ?? 0, 'member')}.`;
  return (
    <Section
      id="coalition"
      title="How resilience changes as countries join"
      intro={
        <p>
          Starting from the demand anchors (the economies that lose most of the compact&apos;s supply), a greedy path adds one economy at a time, the one that most reduces the shortfall, and
          measures the coverage of the anchors&apos; demand in every node. The weakest node sets the minimum. Fewer members means less coordination to organise.
          {exact
            ? " The path is a heuristic: the coalitions below are the exact minimum for each target (a binary program), and the path can need more, or other, economies to reach the same target."
            : ''}
        </p>
      }
    >
      <ChartCard
        headingLevel={3}
        title="Coverage as members join"
        badge={<ScenarioBadge />}
        sub="Across: members in the coalition (each tick names the economy added at that step; anchors for the start). Up: coverage of the anchors' demand, the minimum over the nodes (solid) and the mean (dashed). Dotted rules: the coverage targets; a ring marks the first step of the greedy path to reach each."
        summary={summary}
        legend={
          <Legend
            label="Coalition path"
            items={[
              { label: 'Minimum coverage over the nodes', color: CURVE_COLOR.min, kind: 'line' },
              { label: 'Mean coverage', color: CURVE_COLOR.mean, kind: 'line', dash: '5,4' },
              { label: 'Coverage target', color: CURVE_COLOR.target, kind: 'line', dash: '2,3' },
              { label: 'First step of the path to reach a target', color: '#ffffff', kind: 'hollow' },
            ]}
          />
        }
        table={() => <TableView rows={rows} cols={cols} caption={`${c.name}: the coalition path`} pageSize={50} />}
        csv={csv}
        notes={
          nodeIds.length ? (
            <span>Per-node coverage at each step is in the CSV ({listWords(nodeIds.map((id) => nodeLabel(id)))}).</span>
          ) : undefined
        }
      >
        {points.length ? (
          <CoalitionChart points={points} marks={marks} ariaLabel={summary} />
        ) : (
          <EmptyState>The export carries no coalition path for this compact.</EmptyState>
        )}
        <ul className="mt-4 grid gap-2 text-sm sm:grid-cols-3">
          {coalitions.map(({ t, list }) => (
            <li key={t} className="panel px-3 py-2">
              <span className="eyebrow block">
                {exact ? 'Exact minimum, ' : ''}
                {targetLabel(t)} target
              </span>
              <span className="mt-1 block text-paper-dim">
                {list.length ? `${plural(list.length, 'member')}: ${listWords(list.map((x) => name(x)))}` : 'not reached'}
              </span>
            </li>
          ))}
        </ul>
        {marks.some((m) => m.point?.step === 0) && (
          <p className="mt-2 text-xs text-paper-mute">
            The demand anchors alone already reach the{' '}
            {listWords(marks.filter((m) => m.point?.step === 0).map((m) => targetLabel(m.target)))} {marks.filter((m) => m.point?.step === 0).length === 1 ? 'target' : 'targets'}: the
            economies that lose the supply can restore it among themselves.
          </p>
        )}
        {!c.coalition.reached99 && (
          <p className="mt-2 text-xs text-paper-mute">
            The highest target is not reached: along the greedy path, until no economy helps further, the anchors&apos; coverage in the weakest node peaks at{' '}
            {fmtShare(c.coalition.maxCoverage)}.
          </p>
        )}
      </ChartCard>
    </Section>
  );
}
