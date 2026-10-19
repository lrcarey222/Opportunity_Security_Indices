// "What should governments do?" on a compact page (IA refactor, 2026-10-16; brief s.17): the model's
// treatment of each node grouped by class (COORDINATE, BUILD, MANAGE SPECIALIZATION, LOW INTERVENTION),
// each with the node's recommended treatment, then the compact's policy needs (the instruments of its
// major classes) and, where the export has them, each node's policy needs (multi-label: a node can need
// several instruments; credibility pass). Existing fields only; table twin and CSV.

import { ChartCard } from '../../../charts/ChartCard.tsx';
import { TableView, type Col } from '../../../components/TableView.tsx';
import type { AibNode } from '../../contract.ts';
import { ClassChip, ClassLegend } from '../../charts/ClassChip.tsx';
import { classInfo, classMix, parseNodePolicyNeeds, policyNeedsText } from '../../derive/classes.ts';
import { fmtShare, plural } from '../../derive/format.ts';
import { csvWithRun } from '../common.tsx';
import { NodePolicyNeeds, PolicyNeeds } from './OverviewSections.tsx';
import { useCompact } from './shared.tsx';

export function PolicySection() {
  const { c, nodes, meta, name } = useCompact();
  const mix = classMix(nodes.map((n) => n.cls));
  const byNode = parseNodePolicyNeeds(c.nodePolicyNeeds);
  const cols: Col<AibNode>[] = [
    { id: 'node', header: 'Sub-sector', value: (n) => n.label },
    { id: 'stage', header: 'Stage', value: (n) => n.stage },
    { id: 'cls', header: 'Treatment class', value: (n) => classInfo(n.cls).label },
    { id: 'needs', header: 'Policy needs', value: (n) => policyNeedsText(n.policyNeeds) },
    { id: 'treatment', header: 'Recommended treatment', value: (n) => n.treatment, minWidth: 260 },
    { id: 'hosts', header: 'Best investment hosts', value: (n) => n.hosts.slice(0, 3).map((h) => name(h)).join(', ') || null },
  ];
  const csv = () => ({
    name: `${c.id} node treatments`,
    text: csvWithRun(
      nodes.map((n) => ({ compact: c.id, node_id: n.id, sub_sector: n.label, stage: n.stage, classification: n.cls, policy_needs: policyNeedsText(n.policyNeeds), recommended_treatment: n.treatment, eligible_hosts_best_first: n.hosts.join('; ') })),
      meta,
    ),
  });
  return (
    <div id="policy" className="scroll-mt-16 space-y-6">
      <ChartCard
        headingLevel={3}
        title="Treatment node by node"
        sub={`One treatment per node from its own results: ${mix.map((m) => `${plural(m.n, 'node')} ${classInfo(m.cls).label}`).join(', ')}.`}
        legend={<ClassLegend classes={mix.map((m) => m.cls)} />}
        table={() => <TableView rows={nodes} cols={cols} caption={`${c.name}: node treatments`} rowKey={(n) => n.id} pageSize={50} />}
        csv={csv}
      >
        <div className="space-y-5">
          {mix.map((m) => (
            <div key={m.cls}>
              <p className="flex flex-wrap items-center gap-2 text-sm">
                <ClassChip cls={m.cls} />
                <span className="text-paper-mute">{classInfo(m.cls).meaning}</span>
              </p>
              <ul className="mt-2 space-y-1.5 pl-1 text-sm">
                {nodes
                  .filter((n) => n.cls === m.cls)
                  .map((n) => (
                    <li key={n.id} className="leading-relaxed">
                      <span className="text-paper">{n.label}</span> <span className="text-xs text-paper-mute">({n.stage}, residual gap {fmtShare(n.residual, 0)})</span>
                      {n.treatment && <span className="text-paper-dim">: {n.treatment}</span>}
                      {m.cls === 'BUILD' && n.hosts.length > 0 && <span className="text-paper-dim"> Best hosts: {n.hosts.slice(0, 3).map((h) => name(h)).join(', ')}.</span>}
                    </li>
                  ))}
              </ul>
            </div>
          ))}
        </div>
      </ChartCard>
      <section aria-labelledby="needs-h" className="card p-5">
        <h3 id="needs-h" className="text-base font-medium text-paper">
          Policy instruments the compact needs
        </h3>
        <p className="mb-3 mt-1 text-sm text-paper-mute">The instruments of each class that holds a material share of the lost supply.</p>
        <div className="text-sm">
          <PolicyNeeds needs={c.policyNeeds} />
        </div>
        {byNode.length > 0 && (
          <>
            <h4 className="mt-4 text-sm font-medium text-paper">Policy needs by node</h4>
            <div className="mt-1 text-sm">
              <NodePolicyNeeds byNode={byNode} />
            </div>
          </>
        )}
        <p className="mt-4 text-xs text-paper-mute">
          Gates and fences (which partners to open trade to and where to screen or restrict it) are not part of this export, so no node-level trade-defence treatment is shown.
        </p>
      </section>
    </div>
  );
}
