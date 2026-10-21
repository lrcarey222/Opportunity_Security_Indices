// The landing's "same compacts under every scenario" card (2026-10-20, task s.20): one row per strategic
// compact, one cell per scenario the section lists (Baseline, Managed Interdependence, Partial supplier
// disruption, Decoupling), each the compact's nodes as small squares in value-chain order coloured by
// their status and the policy mix in words. The rows never change between cells: the compact is the
// persistent architecture and only the stress applied to it differs. The toggled scenario's cell is
// marked. A table twin (the node counts by status) and a CSV of every compact under every scenario (the
// fields of aib_strategic_compacts.csv the bundle carries).

import { Link } from 'react-router';
import { ChartCard } from '../../../charts/ChartCard.tsx';
import { TableView, type Col } from '../../../components/TableView.tsx';
import { useAibCtx } from '../../AibLayout.tsx';
import { SpineLegend } from '../../charts/ValueChainSpine.tsx';
import type { AibStrategic, AibStrategicCompact, AibStrategicScenarioId } from '../../contract.ts';
import { fmtUsd, fmtShare } from '../../derive/format.ts';
import { compactNodeIds, compactScenarioOf, policyMixWords, strategicCompacts, strategicScenarios } from '../../derive/strategic.ts';
import { strategicCompactHref } from '../../nav.ts';
import { csvWithRun } from '../common.tsx';
import { countsWords, interventionCount, interventionWords, MiniSpine } from './shared.tsx';

/** Every compact under every listed scenario, one CSV row each (the counts, missions, coverage and partners). */
export function strategicCsvRows(s: AibStrategic, name: (iso3: string) => string): Record<string, unknown>[] {
  const names = (l: readonly string[] | null | undefined) => (l ? l.map(name).join('; ') : null);
  return strategicCompacts(s).flatMap((c) =>
    strategicScenarios(s).map((d) => {
      const sc = compactScenarioOf(c, d.id);
      const m = (slot: string) => sc?.missions.find((x) => x.slot === slot)?.text ?? null;
      return {
        strategic_compact_id: c.id,
        strategic_compact_name: c.name,
        compact_family: c.family,
        scenario: d.label,
        value_chain: c.steps.map((x) => x.label).join(' > '),
        nodes: compactNodeIds(c).length,
        workstreams: c.workstreams.length,
        active_workstreams: sc ? sc.activeWorkstreams.length : null,
        build_nodes: sc?.counts.build ?? null,
        coordinate_nodes: sc?.counts.coordinate ?? null,
        manage_specialization_nodes: sc?.counts.manage ?? null,
        low_intervention_nodes: sc?.counts.low ?? null,
        insufficient_data_nodes: sc?.counts.insufficient ?? null,
        not_stressed_nodes: sc?.counts.notStressed ?? null,
        not_tested_nodes: sc ? (sc.counts.notTested ?? 0) : null,
        no_shock_nodes: sc?.counts.noShock ?? null,
        policy_mix: sc ? policyMixWords(sc) : null,
        primary_mission: m('primary'),
        secondary_mission: m('secondary'),
        supporting_mission: m('supporting'),
        allied_demand_usd: sc?.demandUsd ?? null,
        lost_supply_usd: sc?.lostUsd ?? null,
        share_lost: sc?.exposure ?? null,
        coverage_shock_alone: sc?.coverage.static ?? null,
        coverage_existing: sc?.coverage.existing ?? null,
        coverage_coordinated: sc?.coverage.coordinated ?? null,
        coverage_post_investment: sc?.coverage.postInvestment ?? null,
        core_countries: c.core.map((x) => `${name(x.iso3)} (${x.role})`).join('; '),
        critical_supply_partners: names(sc?.criticalPartners),
        critical_external_dependencies: names(sc?.criticalExternal),
        allied_outside_core: names(sc?.alliedOutsideCore),
        non_allied_relied_on: names(sc?.nonAlliedReliedOn),
        build_hosts: names(sc?.buildHosts),
        build_nodes_with_tied_hosts: sc?.buildHostTies ?? null,
        headline: sc?.headline ?? null,
        stress_note: sc?.stressNote ?? null,
      };
    }),
  );
}

export function ScenarioMatrix({ s, sid }: { s: AibStrategic; sid: AibStrategicScenarioId }) {
  const { doc, name, href } = useAibCtx();
  const scenarios = strategicScenarios(s);
  const compacts = strategicCompacts(s);
  const cols: Col<AibStrategicCompact>[] = [
    { id: 'compact', header: 'Strategic compact', value: (c) => c.name, minWidth: 180 },
    { id: 'nodes', header: 'Nodes', value: (c) => compactNodeIds(c).length, num: true },
    ...scenarios.flatMap((d): Col<AibStrategicCompact>[] => [
      { id: `${d.id}-n`, header: `${d.label}: nodes needing intervention`, value: (c) => interventionCount(compactScenarioOf(c, d.id)), num: true },
      { id: `${d.id}-mix`, header: `${d.label}: policy mix`, value: (c) => policyMixWords(compactScenarioOf(c, d.id)), minWidth: 160 },
      { id: `${d.id}-lost`, header: `${d.label}: supply lost`, value: (c) => compactScenarioOf(c, d.id)?.lostUsd ?? null, cell: (c) => fmtUsd(compactScenarioOf(c, d.id)?.lostUsd), num: true },
      {
        id: `${d.id}-cov`,
        header: `${d.label}: served with coordinated trade`,
        value: (c) => compactScenarioOf(c, d.id)?.coverage.coordinated ?? null,
        cell: (c) => fmtShare(compactScenarioOf(c, d.id)?.coverage.coordinated, 0),
        num: true,
      },
    ]),
  ];
  const shown = scenarios.find((d) => d.id === sid)?.label ?? sid;
  return (
    <ChartCard
      title="The same compacts under every scenario"
      sub="Each square is one node of the compact, in value-chain order; its colour is the node’s status under the scenario. The compacts and their nodes do not change from one scenario to the next: only how many nodes need intervention, and which."
      summary={compacts.map((c) => `${c.name}: ${scenarios.map((d) => `${d.label} ${policyMixWords(compactScenarioOf(c, d.id))}`).join(', ')}`).join('; ')}
      legend={<SpineLegend />}
      table={() => <TableView rows={compacts} cols={cols} caption="Every strategic compact under every scenario" rowKey={(c) => c.id} pageSize={25} />}
      csv={() => ({ name: 'Strategic sectoral compacts under every scenario', text: csvWithRun(strategicCsvRows(s, name), doc.meta) })}
    >
      <ul aria-label="Every strategic compact under every scenario" data-testid="scenario-matrix">
        {compacts.map((c) => (
          <li key={c.id} className="grid gap-x-4 gap-y-2 border-t hairline py-3 first:border-t-0 lg:grid-cols-[13rem_minmax(0,1fr)] lg:items-start" data-compact={c.id}>
            <p className="text-sm">
              <Link className="link" to={href(strategicCompactHref(c.id))}>
                {c.name}
              </Link>
            </p>
            <dl className="grid grid-cols-1 gap-2 min-[30rem]:grid-cols-2 xl:grid-cols-4">
              {scenarios.map((d) => {
                const sc = compactScenarioOf(c, d.id);
                const current = d.id === sid;
                const counts = countsWords(sc);
                return (
                  <div key={d.id} className={`min-w-0 px-2 py-1.5 ${current ? 'bg-white/[0.04] ring-1 ring-inset ring-white/25' : ''}`} data-scenario={d.id} aria-current={current ? 'true' : undefined}>
                    <dt className={`text-xs ${current ? 'text-paper' : 'text-paper-mute'}`} title={d.words ?? undefined}>
                      {d.label}
                      {current && <span className="sr-only"> (shown above)</span>}
                    </dt>
                    <dd className="mt-1 flex flex-col gap-1 text-xs text-paper-dim" title={counts}>
                      <MiniSpine c={c} sc={sc} />
                      <span>
                        {policyMixWords(sc)}
                        {d.id !== 'baseline' && <span className="text-paper-mute"> · {interventionWords(c, sc)}</span>}
                      </span>
                      <span className="sr-only">{counts}</span>
                    </dd>
                  </div>
                );
              })}
            </dl>
          </li>
        ))}
      </ul>
      <p className="mt-2 text-xs text-paper-mute">The outlined cell is the scenario shown above ({shown}).</p>
    </ChartCard>
  );
}
