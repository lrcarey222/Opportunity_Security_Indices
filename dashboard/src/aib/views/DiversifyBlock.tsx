// A node under the scenarios (2026-10-19). NodeScenarioRows: the three-row comparison of a node's allied
// demand (Baseline, Managed Interdependence, Decoupling: kept / restored by trade / new capacity, to scale,
// the toggled scenario highlighted; a node Managed Interdependence leaves unchanged says why). DiversifyBlock
// ("Managed Interdependence · <status>"): what the scenario does to the node, worded per status (diversified
// to the benchmark: the benchmark, the cap, the capped suppliers and the supply to shift, then the class
// under the scenario with its physical-capability flag, its hosts and largest new or expanded relationships;
// within the cap in the allied market; already as diversified as its benchmark, or within a small tolerance
// of it; no benchmark), and a link to the explainer. Where the three-row comparison stands beside it
// (`scenarios`), the block leaves out what the rows already show (coverage after trade, the gap, the split
// bar); a bundle without the scenarios keeps them. Nothing when the node has no node.diversify (an older
// bundle, a node the scenario did not assess). Shown in a compact page's node section and in the robustness
// disclosure of a node outside every compact.

import { Link } from 'react-router';
import { diversifyShockIdOf, type AibMeta, type AibNode } from '../contract.ts';
import { DemandSplitBar, splitText } from '../charts/DiversifyChart.tsx';
import { ScenarioCoverageCompare } from '../charts/ScenarioCompare.tsx';
import { ScenarioMiniBars } from '../charts/ScenarioMini.tsx';
import { useAibScenario } from '../data/hooks.ts';
import { DIVERSIFY_HREF, diversifyRow, diversifyWords, rowStatusLabel } from '../derive/diversify.ts';
import { nodeScenarioNote } from '../derive/scenarioPages.ts';
import { nodeScenarios, type AibScenarioId } from '../derive/scenarios.ts';

/** The link to the Managed Interdependence explainer. */
export const MI_EXPLAINER_LINK = 'How Managed Interdependence works';

export function DiversifyBlock({
  n, meta, names, scenarios = false,
}: {
  n: AibNode;
  meta: Pick<AibMeta, 'diversify'>;
  names: Readonly<Record<string, string>>;
  /** the three-row comparison stands beside the block: leave out the coverage it shows */
  scenarios?: boolean;
}) {
  const { href } = useAibScenario();
  const r = diversifyRow(n, diversifyShockIdOf(meta));
  if (!r) return null;
  const words = diversifyWords(r, names, { coverage: !scenarios });
  return (
    <div className="mt-3 border-l-2 border-l-ink-500 pl-3 text-xs" data-testid="diversify-block" data-status={r.d.status}>
      <h5 className="text-xs font-medium text-paper">
        Managed Interdependence <span className="font-normal text-paper-mute">· {rowStatusLabel(r).toLowerCase()}</span>
      </h5>
      {words.map((w) => (
        <p key={w} className="mt-1 leading-relaxed text-paper-dim">
          {w}
        </p>
      ))}
      {!scenarios && r.split && (
        <div className="mt-1.5 max-w-md">
          <DemandSplitBar split={r.split} label={`${n.label}: ${splitText(r.split)} of allied demand`} className="h-2" />
        </div>
      )}
      <p className="mt-1.5">
        <Link className="link" to={href(DIVERSIFY_HREF)}>
          {MI_EXPLAINER_LINK} &rarr;
        </Link>
      </p>
    </div>
  );
}

/** The managed row's note where the scenario leaves the node unchanged (by its status). */
function notesOf(n: AibNode): Partial<Record<AibScenarioId, string | null>> | undefined {
  const m = nodeScenarios(n).find((r) => r.id === 'managed');
  const note = m ? nodeScenarioNote(m) : null;
  return note ? { managed: note } : undefined;
}

/** A node's allied demand under the three scenarios, the toggled one highlighted (`mini`: the narrow form for a node chip). */
export function NodeScenarioRows({ n, highlight, meta, legend = false, mini = false }: { n: AibNode; highlight: AibScenarioId; meta?: AibMeta | null; legend?: boolean; mini?: boolean }) {
  const rows = nodeScenarios(n);
  const label = `${n.label}: coverage of its allied demand under each scenario`;
  if (mini) {
    const notes = notesOf(n);
    return (
      <div className="min-w-[16rem]" data-testid="node-scenarios">
        <ScenarioMiniBars rows={rows.map((r) => ({ id: r.id, split: r.split, note: notes?.[r.id] ?? null }))} highlight={highlight} label={label} meta={meta} />
      </div>
    );
  }
  return (
    <div data-testid="node-scenarios">
      <ScenarioCoverageCompare rows={rows} highlight={highlight} label={label} meta={meta} notes={notesOf(n)} legend={legend} compact />
    </div>
  );
}
