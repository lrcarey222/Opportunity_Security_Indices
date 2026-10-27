// The supply-chain spine of one strategic compact (2026-10-20, task s.15): the main organizing device of
// the page. The value chain in order with each node's status under the toggled scenario (the shared
// ValueChainSpine), and, for the node selected on it (or on the division of labour's column headers), the
// existing node evidence: its status, coordinated coverage and best new-capacity host in every scenario,
// its figures under the toggled scenario from the main document (share lost or shifted, the coverage
// track of NodeCoverageBar, its class and policy needs, the physical-capability warning), who supplies,
// buys and could host it, its class under other assumptions (NodeRobustness, opened on demand) and a link
// to the node-by-node detail of its workstream. The panel is a live region, so a screen reader hears it.

import { Link } from 'react-router';
import { Disclosure } from '../../../components/ui.tsx';
import { useAibCtx } from '../../AibLayout.tsx';
import { ClassChip, PhysicalWarning, PolicyNeeds } from '../../charts/ClassChip.tsx';
import { NodeCoverageBar } from '../../charts/NodeCoverageBar.tsx';
import { SpineStatusSwatch, ValueChainSpine } from '../../charts/ValueChainSpine.tsx';
import type { AibStrategic, AibStrategicCompact, AibStrategicScenarioId } from '../../contract.ts';
import { fmtShare, fmtUsd, listWords } from '../../derive/format.ts';
import { nodeScenario } from '../../derive/scenarios.ts';
import { robustnessSummary } from '../../derive/sensitivity.ts';
import { divisionCells, nodeHomeOf, nodeStatusAcross, spineOf, spineStatusInfo, type DivisionCell } from '../../derive/strategic.ts';
import { nodeWhere } from '../../derive/taxonomy.ts';
import { compactHref } from '../../nav.ts';
import { Section } from '../common.tsx';
import { NodeRobustness } from '../NodeRobustness.tsx';
import { Fact, scenarioLabelOf } from './shared.tsx';

const FN_WORDS: Readonly<Record<string, string>> = {
  resource: 'resource step',
  processing: 'processing step',
  components: 'component making',
  systems: 'system assembly',
};

/** The top economies of a node on one measure, in words ("Zembla 62%, Carpania 12%"). */
function topWords(cells: readonly DivisionCell[], pick: (c: DivisionCell) => number | null, name: (iso3: string) => string, n = 4, min = 0.02): string | null {
  const rows = cells
    .map((c) => ({ iso3: c.iso3, v: pick(c) }))
    .filter((r): r is { iso3: string; v: number } => r.v !== null && r.v >= min)
    .sort((a, b) => b.v - a.v || a.iso3.localeCompare(b.iso3))
    .slice(0, n);
  return rows.length ? rows.map((r) => `${name(r.iso3)} ${fmtShare(r.v, 0)}`).join(', ') : null;
}

export function NodeEvidence({ s, c, nodeId, sid, onClose }: { s: AibStrategic; c: AibStrategicCompact; nodeId: string; sid: AibStrategicScenarioId; onClose: () => void }) {
  const { doc, names, name, nodeOf, tx, scenario, hasScenarios, href } = useAibCtx();
  const n = nodeOf(nodeId);
  const home = nodeHomeOf(s, nodeId);
  const label = n?.label ?? nodeId;
  const shown = scenarioLabelOf(s, sid);
  const across = nodeStatusAcross(s, c, nodeId);
  const r = n ? nodeScenario(n, scenario) : null;
  const mi = scenario === 'managed';
  const cells = divisionCells(c).filter((x) => x.node === nodeId);
  const suppliers = topWords(cells, (x) => x.supply, name);
  const buyers = topWords(cells, (x) => x.demand, name);
  const hosts = cells
    .filter((x) => x.hostRank !== null)
    .sort((a, b) => (a.hostRank ?? 99) - (b.hostRank ?? 99) || a.iso3.localeCompare(b.iso3))
    .slice(0, 3)
    .map((x) => `${name(x.iso3)} (rank ${x.hostRank})`);
  const here = across.find((x) => x.scenario.id === sid) ?? null;
  const untested = here?.status === 'NOT TESTED';
  const cls = r?.ran ? r.cls : null;
  const hid = `node-evidence-${nodeId}`;
  return (
    <section className="mt-5 border-t hairline pt-4" aria-labelledby={hid} data-testid="node-evidence" data-node={nodeId}>
      <div className="flex flex-wrap items-start justify-between gap-x-4 gap-y-2">
        <div className="min-w-0">
          <h3 id={hid} className="text-base font-medium text-paper" tabIndex={-1}>
            {label}
          </h3>
          <p className="mt-0.5 text-xs text-paper-mute">
            {home ? `Step ${c.steps.findIndex((st) => st.order === home.order) + 1} of ${c.steps.length}: ${home.step}` : ''}
            {home?.fn ? ` · ${FN_WORDS[home.fn] ?? home.fn}` : ''}
            {n ? ` · ${nodeWhere(tx, n)}` : ''}
          </p>
        </div>
        <button type="button" className="btn btn-ghost btn-sm" onClick={onClose} aria-label={`Close the evidence for ${label}`}>
          Close
        </button>
      </div>
      <div className="mt-3 grid gap-6 lg:grid-cols-2">
        <div className="min-w-0">
          <h4 className="text-sm font-medium text-paper">Its status in each scenario</h4>
          <ul className="mt-2 space-y-1.5 text-sm" aria-label={`${label}: status in each scenario`}>
            {across.map((x) => {
              const info = spineStatusInfo(x.status);
              const current = x.scenario.id === sid;
              return (
                <li key={x.scenario.id} className={`flex flex-wrap items-center gap-x-2 gap-y-0.5 ${current ? 'text-paper' : 'text-paper-dim'}`} aria-current={current ? 'true' : undefined}>
                  <SpineStatusSwatch status={x.status} size={8} />
                  <span className="min-w-0">
                    {x.scenario.label}: <span className={current ? 'font-medium' : ''}>{x.status ? info.label : 'no status'}</span>
                    {current && <span className="eyebrow ml-1.5 text-paper-mute">shown</span>}
                  </span>
                  <span className="w-full pl-5 text-xs text-paper-mute">
                    {x.coverage !== null ? `served with coordinated trade ${fmtShare(x.coverage, 0)}` : 'no coverage figure'}
                    {x.host
                      ? `; best-ranked host for new capacity ${name(x.host)} (no other host ties)`
                      : x.ties !== null && x.ties > 1
                        ? `; ${x.ties} hosts tie for new capacity (no single best host)`
                        : ''}
                  </span>
                </li>
              );
            })}
          </ul>
        </div>
        <div className="min-w-0">
          <h4 className="text-sm font-medium text-paper">Under {shown}</h4>
          {!n ? (
            <p className="mt-2 text-sm text-paper-dim">The main data have no figures for this node.</p>
          ) : r && r.ran ? (
            <div className="mt-2 space-y-2 text-sm text-paper-dim">
              <p>
                {fmtShare(r.exposure, 0)} of its allied demand ({fmtUsd(r.lostUsd)}) {mi ? 'has to shift away from suppliers above the cap' : 'is lost to the shock'}. Served:{' '}
                {mi ? 'the cap alone' : 'the shock alone'} {fmtShare(r.static, 0)}, existing relationships {fmtShare(r.existing, 0)}, coordinated trade {fmtShare(r.optimized, 0)}.
              </p>
              <NodeCoverageBar
                stat={r.static}
                existing={r.existing}
                optimized={r.optimized}
                label={`${label} under ${shown}: ${mi ? 'cap' : 'shock'} alone ${fmtShare(r.static, 0)}, existing relationships ${fmtShare(r.existing, 0)}, coordinated trade ${fmtShare(r.optimized, 0)} of demand served`}
              />
              {cls && (
                <p className="flex flex-wrap items-center gap-2">
                  <ClassChip cls={cls} needs={mi ? null : n.policyNeeds} warning={mi ? null : n.warning} />
                  {!mi && <PolicyNeeds cls={cls} needs={n.policyNeeds} />}
                  {!mi && n.warning && <PhysicalWarning warning={n.warning} validation={n.physicalValidation} />}
                </p>
              )}
            </div>
          ) : (
            <p className="mt-2 text-sm text-paper-dim">
              {untested
                ? 'Not tested: the node is concentrated, but its benchmark heading is as concentrated, so Managed Interdependence shifts no supply here. Its Decoupling status above is the test.'
                : mi
                  ? 'No supplier is above the cap here, so no supply has to shift.'
                  : 'The model has too little data to test this node under the shock.'}
            </p>
          )}
        </div>
      </div>
      <dl className="mt-4 grid grid-cols-1 gap-x-3 gap-y-1.5 text-sm sm:grid-cols-[13rem_minmax(0,1fr)]" aria-label={`Who does what in ${label}`}>
        <Fact term="Largest suppliers (world supply)">{suppliers ?? 'no economy above 2%'}</Fact>
        <Fact term="Largest allied buyers (imports)">{buyers ?? 'no economy above 2%'}</Fact>
        <Fact term="Hosts for new capacity (Managed Interdependence or Decoupling, the better rank)">
          {hosts.length ? `${listWords(hosts)}; they tie with the model’s best on simulated gain, so the rank is its capability tie-break` : 'none on a node that needs new capacity'}
        </Fact>
      </dl>
      <p className="mt-3 text-sm">
        {n?.compact ? (
          <Link className="link" to={href(`${compactHref(n.compact)}#nodes`)}>
            Full node evidence in workstream {n.compact} &rarr;
          </Link>
        ) : (
          <span className="text-paper-mute">This node is in no workstream: the model found no cluster of partners around it.</span>
        )}
      </p>
      {n && (n.variants !== undefined || n.growth !== undefined) && (
        <div className="mt-2">
          <Disclosure summary={robustnessSummary(n, doc.meta)}>
            <NodeRobustness n={n} meta={doc.meta} names={names} scenario={hasScenarios ? scenario : null} />
          </Disclosure>
        </div>
      )}
    </section>
  );
}

export function ChainSection({
  s, c, sid, node, onNode,
}: {
  s: AibStrategic;
  c: AibStrategicCompact;
  sid: AibStrategicScenarioId;
  node: string | null;
  onNode: (id: string | null) => void;
}) {
  const { nodeOf, name } = useAibCtx();
  const label = scenarioLabelOf(s, sid);
  const steps = spineOf(c, sid, nodeOf);
  return (
    <Section
      id="chain"
      title="Which nodes it covers"
      intro={
        <p>
          The supply chain in order, step by step, with the nodes (groups of traded goods) at each step. A node’s colour and label give its policy status under {label}: BUILD
          (new capacity is needed), COORDINATE (trade among partners can redirect supply), MANAGE SPECIALIZATION (keep and back up a specialized supplier) or LOW INTERVENTION.
          Select a node to see the model’s evidence for it.
        </p>
      }
    >
      <div className="card p-4 sm:p-5">
        <ValueChainSpine steps={steps} label={`${c.name} under ${label}`} selected={node} onSelect={(id) => onNode(node === id ? null : id)} name={name} />
        <div aria-live="polite">
          {node ? (
            <NodeEvidence s={s} c={c} nodeId={node} sid={sid} onClose={() => onNode(null)} />
          ) : (
            <p className="mt-4 text-xs text-paper-mute" data-testid="spine-hint">
              Select a node above, or a node’s name in the division of labour, for its evidence.
            </p>
          )}
        </div>
      </div>
    </Section>
  );
}
