// The sector-chain reading of a strategic compact (UX v2 kind `sector_chain`, docs/qc_audit/QC_EVIDENCE_CATALOGUE.md
// section 4): one strategic compact's value chain under one stress, read stage by stage. The Explorer has no single
// "bottleneck stage" output, so the review derives two faithful answers from the export:
// - the set: the six-stage stages (node.stage, not yfCells) holding at least one product whose status under the
//   scenario is BUILD, COORDINATE or MANAGE SPECIALIZATION;
// - the lead stage: where the primary mission's largest lost (or shifted) supply sits. R aib_strategic.R
//   (missions, steps_of) orders a mission's steps by lost US$, so the lead is the stage of the product with the
//   most lost supply among the primary mission's products.
// and a chokepoint view the reveal shows beside them (the highest pre-shock China share under Decoupling, or the
// highest largest-supplier share under Managed Interdependence, plus the production-versus-trade flags), because
// dollar ordering favours high-value downstream stages. Pure: no IO.

import type { AibDoc, AibNode, AibStrategicCompact, AibStrategicCompactScenario } from '../../aib/contract.ts';
import { isSupplyStage, SUPPLY_STAGES, type SupplyStage } from '../../lib/stages.ts';
import type { AibScenarioKey } from '../model.ts';
import { primaryMission } from './answerOptions.ts';

export const INTERVENTION_CLASSES: readonly string[] = ['BUILD', 'COORDINATE', 'MANAGE SPECIALIZATION'];

export interface ChainNode {
  id: string;
  label: string;
  stage: SupplyStage | null;
  step: string;
  stepOrder: number;
  /** the status under the scenario (a class, NOT STRESSED, NOT TESTED, NO SHOCK) */
  status: string;
  /** supply lost (Decoupling) or that must shift (Managed), US$ a year */
  lostUsd: number | null;
  demandUsd: number | null;
  chinaShare: number | null;
  largest: string | null;
  largestShare: number | null;
  physicalData: string | null;
  /** production-versus-trade flags, verbatim */
  productionFlags: string[];
  node: AibNode | null;
}

export interface ChainChokepoint {
  node: string;
  label: string;
  stage: SupplyStage | null;
  basis: 'china_share' | 'largest_share';
  supplier: string | null;
  share: number;
}

export interface ChainAnalysis {
  compact: AibStrategicCompact;
  scenario: AibScenarioKey;
  rec: AibStrategicCompactScenario | null;
  nodes: ChainNode[];
  /** stages holding an intervention-class product, in chain order */
  explorerStages: SupplyStage[];
  /** the stage of the primary mission's largest lost supply; null when no mission */
  leadStage: SupplyStage | null;
  leadNode: ChainNode | null;
  /** the primary mission's products, with their total lost supply and its share of the chain's */
  mission: { cls: string; nodes: ChainNode[]; lostUsd: number; share: number | null; targeted: boolean; text: string; basis: string | null } | null;
  chokepoint: ChainChokepoint | null;
}

const nn = (v: unknown): number | null => (typeof v === 'number' && Number.isFinite(v) ? v : null);

function lostOf(n: AibNode | null, scenario: AibScenarioKey): number | null {
  if (!n) return null;
  if (scenario === 'managed') return nn(n.diversify?.lostUsd);
  if (scenario === 'decoupling') return nn(n.lostUsd);
  return 0;
}

/** The chain of a strategic compact under a scenario; null when the compact is not in the export. */
export function chainAnalysis(doc: AibDoc, sid: string, scenario: AibScenarioKey): ChainAnalysis | null {
  const compact = doc.strategic?.compacts.find((c) => c.id.toUpperCase() === sid.toUpperCase());
  if (!compact) return null;
  const rec = (compact.scenarios as Record<string, AibStrategicCompactScenario | undefined>)[scenario] ?? null;
  const status = new Map((rec?.nodes ?? []).map((t) => [t[0], t[1]]));
  const byId = new Map(doc.nodes.map((n) => [n.id, n]));
  const nodes: ChainNode[] = [];
  for (const st of [...compact.steps].sort((a, b) => a.order - b.order))
    for (const id of st.nodes) {
      const n = byId.get(id) ?? null;
      const sup = n?.baseline?.suppliers ?? [];
      const chn = sup.find(([iso]) => iso === 'CHN');
      nodes.push({
        id,
        label: n?.label ?? id,
        stage: n && isSupplyStage(n.stage) ? n.stage : null,
        step: st.label,
        stepOrder: st.order,
        status: status.get(id) ?? 'NOT IN SCENARIO',
        lostUsd: lostOf(n, scenario),
        demandUsd: nn(n?.demandUsd),
        chinaShare: chn ? nn(chn[1]) : sup.length ? 0 : null,
        largest: n?.baseline?.largest ?? n?.baseLargest ?? null,
        largestShare: nn(n?.baseline?.largestShare) ?? nn(n?.baseLargestShare),
        physicalData: n?.physicalData ?? null,
        productionFlags: (n?.flags ?? []).filter((f) => /production more concentrated than trade/i.test(f)),
        node: n,
      });
    }
  const explorerStages = SUPPLY_STAGES.filter((s) => nodes.some((n) => n.stage === s && INTERVENTION_CLASSES.includes(n.status)));
  const pm = primaryMission(rec);
  let mission: ChainAnalysis['mission'] = null;
  let leadNode: ChainNode | null = null;
  if (pm) {
    const mine = nodes.filter((n) => n.status === pm.cls);
    const tot = nodes.reduce((a, n) => a + (n.lostUsd ?? 0), 0);
    const lost = mine.reduce((a, n) => a + (n.lostUsd ?? 0), 0);
    mission = { cls: pm.cls, nodes: mine, lostUsd: lost, share: tot > 0 ? lost / tot : null, targeted: pm.targeted === true, text: pm.text, basis: pm.basis };
    // the step with the most lost supply in the mission's products (ties: chain order), then its largest product
    const bySteps = new Map<number, { lost: number; nodes: ChainNode[] }>();
    for (const n of mine) {
      const e = bySteps.get(n.stepOrder) ?? { lost: 0, nodes: [] };
      e.lost += n.lostUsd ?? 0;
      e.nodes.push(n);
      bySteps.set(n.stepOrder, e);
    }
    const lead = [...bySteps.entries()].sort((a, b) => b[1].lost - a[1].lost || a[0] - b[0])[0];
    if (lead) leadNode = [...lead[1].nodes].sort((a, b) => (b.lostUsd ?? 0) - (a.lostUsd ?? 0))[0] ?? null;
  }
  const basis: ChainChokepoint['basis'] = scenario === 'decoupling' ? 'china_share' : 'largest_share';
  let chokepoint: ChainChokepoint | null = null;
  for (const n of nodes) {
    const share = basis === 'china_share' ? n.chinaShare : n.largestShare;
    if (share === null) continue;
    if (!chokepoint || share > chokepoint.share) chokepoint = { node: n.id, label: n.label, stage: n.stage, basis, supplier: basis === 'china_share' ? 'CHN' : n.largest, share };
  }
  return { compact, scenario, rec, nodes, explorerStages, leadStage: leadNode?.stage ?? null, leadNode, mission, chokepoint };
}
