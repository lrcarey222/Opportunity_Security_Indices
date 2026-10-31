// "What should governments do?" on a compact page (IA refactor, 2026-10-16; brief s.17): the model's
// treatment of each node grouped by class (COORDINATE, BUILD, MANAGE SPECIALIZATION, LOW INTERVENTION),
// each with the node's recommended treatment, then the compact's policy needs (the instruments of its
// major classes) and, where the export has them, each node's policy needs (multi-label: a node can need
// several instruments; credibility pass). Existing fields only; table twin and CSV.
//
// The scenario toggle (2026-10-19): under Managed Interdependence the nodes are grouped by their class under
// the scenario (the nodes it leaves unchanged apart), each with its gap and leading hosts under the cap; the
// recommended treatments and the compact's instruments are those written for Decoupling, and say so.

import { ChartCard } from '../../../charts/ChartCard.tsx';
import { TableView, type Col } from '../../../components/TableView.tsx';
import type { AibNode } from '../../contract.ts';
import { ClassChip, ClassLegend } from '../../charts/ClassChip.tsx';
import { AIB_CLASS_HEADING } from '../../../content/vocabulary.ts';
import { classInfo, classMix, parseNodePolicyNeeds, policyNeedsText, policyNeedsWords } from '../../derive/classes.ts';
import { fmtShare, plural } from '../../derive/format.ts';
import { nodeScenarioNote, UNCHANGED } from '../../derive/scenarioPages.ts';
import { nodeScenario, runMetaOf, scenarioCsvFields, scenarioInfo, scenarioLabel } from '../../derive/scenarios.ts';
import { nodeStageRank, nodeStagesOf, nodeStageWords, nodeWhere } from '../../derive/taxonomy.ts';
import { csvWithRun } from '../common.tsx';
import { NodePolicyNeeds, PolicyNeeds, ScenarioBadge } from './OverviewSections.tsx';
import { useCompact } from './shared.tsx';

export function PolicySection() {
  const { c, displayName, nodes, meta, name, scenario, hasScenarios, view } = useCompact();
  const mi = scenario === 'managed';
  const miLabel = scenarioInfo(meta, 'managed').label;
  const dLabel = scenarioInfo(meta, 'decoupling').label;
  // the class of each node under the toggled scenario (Managed Interdependence: UNCHANGED where it does not run)
  const clsOf = (n: AibNode): string => {
    if (!mi) return n.cls;
    const r = nodeScenario(n, 'managed');
    return r.ran && r.cls ? r.cls : UNCHANGED;
  };
  const mix = classMix(nodes.map(clsOf));
  const byNode = parseNodePolicyNeeds(c.nodePolicyNeeds);
  // the hosts of each node under the toggled scenario (Managed Interdependence: its own where it ran, none elsewhere)
  const hostsOf = (n: AibNode): string[] => {
    if (!mi) return n.hosts;
    return nodeScenario(n, 'managed').ran ? (n.diversify?.hosts ?? []) : [];
  };
  const clsLabel = (n: AibNode): string => (clsOf(n) === UNCHANGED ? 'Unchanged' : classInfo(clsOf(n)).label);
  // the compact's instruments under the toggled scenario (Managed Interdependence: its own where it has a coalition)
  const needs = mi && view ? view.policyNeeds : c.policyNeeds;
  const cols: Col<AibNode>[] = [
    { id: 'node', header: 'Supply-chain step', value: (n) => n.label },
    { id: 'stage', header: 'Stage', value: (n) => nodeStageWords(n), sortValue: (n) => nodeStageRank(n), title: "The stages of the sectors (technology × stage) the step's codes sit in; none when no code is among the traded goods this Explorer covers." },
    { id: 'cls', header: mi ? `What it needs under ${miLabel}` : 'What it needs', value: clsLabel },
    { id: 'needs', header: mi ? `Actions needed under ${dLabel}` : 'Actions needed', value: (n) => policyNeedsWords(n.policyNeeds) ?? '–' },
    { id: 'treatment', header: mi ? `Recommended treatment under ${dLabel}` : 'Recommended treatment', value: (n) => n.treatment, minWidth: 260 },
    { id: 'hosts', header: mi ? `Best-placed countries to add capacity under ${miLabel}` : 'Best-placed countries to add capacity', value: (n) => hostsOf(n).slice(0, 3).map((h) => name(h)).join(', ') || null },
  ];
  const csv = () => {
    const scn = hasScenarios ? scenarioCsvFields(meta, scenario) : null;
    return {
      name: `${c.id} node treatments${hasScenarios ? ` (${scenarioLabel(scenario, meta)})` : ''}`,
      text: csvWithRun(
        nodes.map((n) => ({
          compact: c.id, node_id: n.id, sub_sector: n.label, stage: nodeStagesOf(n).join('; '), aib_model_stage: n.stage,
          classification: mi ? (clsOf(n) === UNCHANGED ? 'unchanged' : clsOf(n)) : n.cls,
          ...(mi ? { classification_decoupling: n.cls, policy_needs_decoupling: policyNeedsText(n.policyNeeds), recommended_treatment_decoupling: n.treatment } : { policy_needs: policyNeedsText(n.policyNeeds), recommended_treatment: n.treatment }),
          eligible_hosts_best_first: hostsOf(n).join('; '),
          ...(scn ?? {}),
        })),
        runMetaOf(meta, scenario),
      ),
    };
  };
  return (
    <div id="policy" className="scroll-mt-16 space-y-6">
      <ChartCard
        headingLevel={3}
        title={AIB_CLASS_HEADING}
        badge={<ScenarioBadge />}
        sub={`One treatment per supply-chain step from its own results${mi ? ' under the cap' : ''}: ${mix.map((m) => `${m.cls === UNCHANGED ? 'unchanged' : classInfo(m.cls).label} ${plural(m.n, 'step')}`).join(', ')}.${mi ? ` The recommended treatments are written for the ${dLabel.toLowerCase()}.` : ''}`}
        legend={<ClassLegend classes={mix.map((m) => m.cls).filter((x) => x !== UNCHANGED)} />}
        table={() => <TableView rows={nodes} cols={cols} caption={`${displayName}: what each supply-chain step needs`} rowKey={(n) => n.id} pageSize={50} />}
        csv={csv}
      >
        <div className="space-y-5">
          {mix.map((m) => (
            <div key={m.cls}>
              <p className="flex flex-wrap items-center gap-2 text-sm">
                {m.cls === UNCHANGED ? <span className="tag text-paper-mute">Unchanged</span> : <ClassChip cls={m.cls} />}
                <span className="text-paper-mute">
                  {m.cls === UNCHANGED ? 'Managed Interdependence leaves these steps as they are today: nothing has to shift.' : classInfo(m.cls).meaning}
                </span>
              </p>
              <ul className="mt-2 space-y-1.5 pl-1 text-sm">
                {nodes
                  .filter((n) => clsOf(n) === m.cls)
                  .map((n) => {
                    const r = mi ? nodeScenario(n, 'managed') : null;
                    const hosts = r ? (r.ran ? (n.diversify?.hosts ?? []) : []) : n.hosts;
                    return (
                      <li key={n.id} className="leading-relaxed">
                        <span className="text-paper">{n.label}</span>{' '}
                        <span className="text-xs text-paper-mute">
                          ({nodeWhere(n)}
                          {r && !r.ran ? `, ${nodeScenarioNote(r) ?? 'unchanged'}` : `, remaining gap ${fmtShare(r ? r.residual : n.residual, 0)}`})
                        </span>
                        {!mi && n.treatment && <span className="text-paper-dim">: {n.treatment}</span>}
                        {m.cls === 'BUILD' && hosts.length > 0 && <span className="text-paper-dim"> Best-placed to add capacity: {hosts.slice(0, 3).map((h) => name(h)).join(', ')}.</span>}
                      </li>
                    );
                  })}
              </ul>
            </div>
          ))}
        </div>
      </ChartCard>
      <section aria-labelledby="needs-h" className="card p-5">
        <h3 id="needs-h" className="text-base font-medium text-paper">
          Policy tools the compact needs
        </h3>
        <p className="mb-3 mt-1 text-sm text-paper-mute">
          The tools for each treatment that covers a material share of the {mi ? 'shifted' : 'lost'} supply: trade rules where capacity exists, investment and finance where it is
          missing, safeguards where specialization is too concentrated
          {mi ? (view ? `, under ${miLabel}` : `, as the compact is formed under the ${dLabel.toLowerCase()} (${miLabel} forms no coalition for it)`) : ''}.
        </p>
        <div className="text-sm">
          <PolicyNeeds needs={needs} />
        </div>
        {byNode.length > 0 && (
          <>
            <h4 className="mt-4 text-sm font-medium text-paper">Actions needed at each step</h4>
            <div className="mt-1 text-sm">
              <NodePolicyNeeds byNode={byNode} />
            </div>
          </>
        )}
        <p className="mt-4 text-xs text-paper-mute">
          Gates and fences (which partners to open trade to and where to screen or restrict it) are not part of these results, so no step-level trade-defense treatment is shown.
        </p>
      </section>
    </div>
  );
}
