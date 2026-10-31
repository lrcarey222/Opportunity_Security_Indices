// The fences of the Country page (2026-10-30, the Yards & Fences framing; the user's design "Only that
// fence's nodes"): which of an economy's nodes sit behind each fence, the top-five rankings and the variables
// each fence's tables show.
//   High fence     the Secure nodes, ranked by supply-chain need (highest first).
//   Medium fence   the producing-stage Preserve, Catch Up and Partner nodes, ranked by the Strategic Industrial Value
//                  (ties by Competitive Position); the Accelerate nodes, ranked by deployment need.
//   Low fence      the producing-stage Watch nodes, ranked by the Strategic Industrial Value (the biggest prizes the
//                  country is not chasing); the Deployment & Use Watch nodes, ranked by deployment need
//                  (closest to Accelerate).
// The fence of a node is grammar.ts nodeFence (its choice and stage). Every value is read through the
// drawer's accessors (the core cell; detail.ts for the components): nothing is recomputed or invented, and a
// value the data do not carry stays null (the page shows "not measured"). Pure: no React, no IO.
//
// Layers are yard units of their own (2026-10-30, R yard_rollup.R): a child layer whose own choice differs
// from its node's is listed behind the fence of its own choice, beside the nodes (a Secure grid SCADA layer
// in the High fence while its Electric Grid Assembly node is Medium). It ranks with its node's ranking value
// (the core carries the node's need, prize and position, not the layer's), right after its node on a tie, and
// opens its node's drawer focused on the layer. A layer that shares its node's choice is the node's own row
// (the drawer lists every layer). Layers come from the economy's detail document, so they join once it is in.

import { isProducingStage, stageRank } from '../lib/stages.ts';
import type { PostureCell, PostureCore, PostureDetail, PostureMeta, PostureThresholds, Verb, Yard } from './contract.ts';
import { pct, score100 } from './derive.ts';
import { ratioVsCurrentWords, type Family, type NodeDetail } from './detail.ts';
import { basisWords, EVIDENCE_LABEL, MOMENTUM_LABEL, nodeFence, PATHWAY_LABEL, pathwayEvidenceWords, RATIONALE_LABEL, verbOf } from './grammar.ts';
import { postureMomentum } from './momentum.ts';
import { cellKeyOf, detailByKey, watchReasons } from './sections.ts';
import { layerUnits, sivFamilies } from './yards.ts';

/** A layer unit's layer: its id, name, yard, engine variant and one-line reason (yards.ts layerReason). */
export interface FenceLayer {
  id: string;
  label: string;
  yard: Yard | null;
  variant: string | null;
  reason: string;
}

/** One assessed node (or a layer unit of one) behind a fence: its core cell and, once the economy's detail document is in, its detail. */
export interface FenceNode {
  /** the row's own key: "Tech|Stage", or "Tech|Stage#layer_id" for a layer unit */
  key: string;
  /** the node the drawer opens (?node=): "Tech|Stage" (a layer unit's parent node) */
  nodeKey: string;
  tech: string;
  stage: string;
  /** the unit's own choice (a layer's may differ from its node's) */
  verb: Verb;
  /** the node's core cell (a layer unit's parent's: its need, prize and position rank the layer) */
  cell: PostureCell;
  detail: NodeDetail | null;
  /** a layer unit's layer; null for the node itself */
  layer: FenceLayer | null;
}

export interface FenceGroups {
  /** High fence: Secure */
  secure: FenceNode[];
  /** Medium fence at the producing stages: Preserve, Catch Up, Partner */
  compete: FenceNode[];
  /** Medium fence at Deployment & Use: Accelerate */
  accelerate: FenceNode[];
  /** Low fence at the producing stages: Watch */
  watchSupply: FenceNode[];
  /** Low fence at Deployment & Use: Watch */
  watchDeploy: FenceNode[];
}

/** The number of nodes in each top list. */
export const FENCE_TOP_N = 5;

const COMPETE: ReadonlySet<Verb> = new Set<Verb>(['preserve', 'catch_up', 'partner']);
/** a missing value sorts last (every value is 0-1) */
const v1 = (v: number | null): number => (v === null || !Number.isFinite(v) ? -1 : v);
/** chain order; within one node, the node before its layers, the layers by name */
const chain = (a: FenceNode, b: FenceNode) =>
  stageRank(a.stage) - stageRank(b.stage) || a.tech.localeCompare(b.tech) || (a.layer ? 1 : 0) - (b.layer ? 1 : 0) || (a.layer?.label ?? '').localeCompare(b.layer?.label ?? '');
const byNeed = (a: FenceNode, b: FenceNode) => v1(b.cell.need) - v1(a.cell.need) || chain(a, b);
const byPrize = (a: FenceNode, b: FenceNode) => v1(b.cell.prize) - v1(a.cell.prize) || v1(b.cell.cp) - v1(a.cell.cp) || chain(a, b);
const byPosition = (a: FenceNode, b: FenceNode) => v1(b.cell.cp) - v1(a.cell.cp) || v1(b.cell.prize) - v1(a.cell.prize) || chain(a, b);

/** Nodes in Competitive Position order (highest first, ties by the prize; a missing position last): the position tables. */
export const byPositionOrder = (nodes: readonly FenceNode[]): FenceNode[] => [...nodes].sort(byPosition);

/**
 * The economy's assessed nodes behind each fence, with the layer units whose own choice differs from their
 * node's, each list in its ranking order (not assessed nodes and layers sit behind no fence). `meta` names the
 * layers and composes their reasons (meta.yf_nodes); without it a layer is named by its id.
 */
export function fenceGroups(cells: readonly PostureCell[], detail: PostureDetail | null, meta: Pick<PostureMeta, 'yf_nodes'> = { yf_nodes: {} }): FenceGroups {
  const d = detailByKey(detail);
  const g: FenceGroups = { secure: [], compete: [], accelerate: [], watchSupply: [], watchDeploy: [] };
  const place = (n: FenceNode) => {
    const fence = nodeFence(n.verb, n.stage);
    if (fence === 'high') g.secure.push(n);
    else if (fence === 'medium') (COMPETE.has(n.verb) ? g.compete : g.accelerate).push(n);
    else if (fence === 'low') (isProducingStage(n.stage) ? g.watchSupply : g.watchDeploy).push(n);
  };
  for (const c of cells) {
    const verb = verbOf(c);
    const key = cellKeyOf(c);
    const nd = d.get(key) ?? null;
    if (verb && nodeFence(verb, c.stage)) place({ key, nodeKey: key, tech: c.tech, stage: c.stage, verb, cell: c, detail: nd, layer: null });
    for (const u of layerUnits(meta, nd)) {
      if (!u.verb || u.verb === verb) continue;
      place({
        key: `${key}#${u.layerId}`, nodeKey: key, tech: c.tech, stage: c.stage, verb: u.verb, cell: c, detail: nd,
        layer: { id: u.layerId, label: u.label, yard: u.yard, variant: u.variant, reason: u.reason },
      });
    }
  }
  g.secure.sort(byNeed);
  g.compete.sort(byPrize);
  g.accelerate.sort(byNeed);
  g.watchSupply.sort(byPrize);
  g.watchDeploy.sort(byNeed);
  return g;
}

// ---- the variables of each table (the drawer's accessors) ---------------------------------------------------

/** High fence: supply-chain need and its four families (0-1; null: not measured). */
export interface SupplyNeedVars {
  need: number | null;
  importDependence: number | null;
  supplierConcentration: number | null;
  riskyExposure: number | null;
  trustedAlternatives: number | null;
}
export function supplyNeedVars(n: FenceNode): SupplyNeedVars {
  const s = n.detail?.supplyChain ?? null;
  return {
    need: n.cell.need,
    importDependence: s?.importDependence ?? null,
    supplierConcentration: s?.supplierConcentration ?? null,
    riskyExposure: s?.riskyExposure ?? null,
    trustedAlternatives: s?.trustedAlternatives ?? null,
  };
}

/** A Catch Up node's credible pathway in the drawer's words: "Credible (feasibility, pipeline)"; null when not measured. */
export function pathwayWords(c: Pick<PostureCell, 'pathway'>, d: NodeDetail | null): string | null {
  const legs = d?.pathway.basis.length ? pathwayEvidenceWords(d.pathway.basis) : null;
  if (c.pathway) return `${PATHWAY_LABEL[c.pathway]}${legs ? ` (${legs})` : ''}`;
  return legs;
}

/**
 * Medium and low fence, the prize: the Strategic Industrial Value and its three families (the market prize,
 * from the core or the detail; industrial commons, future option and the market-only cap, from the node's or
 * the layer's evidence in meta.yf_nodes), the market prize's components, supply-chain need as context and the
 * Catch Up pathway.
 */
export interface PrizeVars {
  /** the prize the choice read: the Strategic Industrial Value (a layer unit: its node's) */
  opportunity: number | null;
  /** the market prize (Industrial Opportunity) */
  market: number | null;
  /** industrial-commons value (the layer's own for a layer unit) */
  commons: number | null;
  /** future-option value (the layer's own for a layer unit) */
  option: number | null;
  /** "market_only" where neither commons nor option reaches the gate: the unit's own (a layer unit's layer) */
  cap: string | null;
  /** the cap of the value shown (the step's: a layer unit ranks with its step's value) */
  valueCap: string | null;
  /** node prize: expected world demand growth */
  nodePrize: number | null;
  /** home-market growth (national data only) */
  homeMarket: number | null;
  investmentMomentum: number | null;
  outlook: string | null;
  /** the basis of the prize, in words */
  basis: string | null;
  need: number | null;
  /** Catch Up only: the credible pathway and its legs */
  pathway: string | null;
}
export function prizeVars(n: FenceNode, meta: Pick<PostureMeta, 'yf_nodes'> = { yf_nodes: {} }): PrizeVars {
  const p = n.detail?.prize ?? null;
  const f = sivFamilies(meta, n.cell, n.detail, n.layer?.id ?? 'stage');
  return {
    opportunity: n.cell.prize,
    market: f.market,
    commons: f.commons,
    option: f.option,
    cap: f.cap,
    valueCap: n.layer ? sivFamilies(meta, n.cell, n.detail).cap : f.cap,
    nodePrize: p?.nodePrize ?? null,
    homeMarket: p?.homeMarket ?? null,
    investmentMomentum: p?.investmentMomentum ?? null,
    outlook: p?.outlook ?? null,
    basis: basisWords(p?.basis),
    need: n.cell.need,
    pathway: n.verb === 'catch_up' ? pathwayWords(n.cell, n.detail) : null,
  };
}

/** A Competitive Position family of the detail (0-1), or null. */
const cpFamily = (d: NodeDetail | null, id: string): number | null => d?.cpFamilies.find((f) => f.id === id)?.value ?? null;

/**
 * Trade momentum in words, as the drawer reads it: from the detail's trade evidence (log export growth against
 * the band) where the detail has it, else the core cell's arrow; null when not measured.
 */
export function tradeMomentumWords(c: Pick<PostureCell, 'momentum'>, d: NodeDetail | null, band: number | null | undefined): string | null {
  if (d?.trade) {
    const m = postureMomentum(d.trade.momentum_raw, band ?? 0.1);
    return m ? MOMENTUM_LABEL[m] : null;
  }
  return c.momentum ? MOMENTUM_LABEL[c.momentum] : null;
}

/** Medium and low fence, the position: Competitive Position among the node's contenders and its evidence. */
export interface PositionVars {
  cp: number | null;
  /** the economy's share of world supply */
  share: number | null;
  exportPosition: number | null;
  /**
   * structural trade share (benchmark). R writes it only where trade momentum moved the export position
   * (posture_export.R drops it where the two are identical), so where it is absent and the export position is
   * present the structural share is the export position (structuralSame).
   */
  structural: number | null;
  /** the structural share is the export position (R dropped the identical value) */
  structuralSame: boolean;
  /** current investment */
  investment: number | null;
  momentum: string | null;
  evidence: string | null;
}
export function positionVars(n: FenceNode, band: number | null | undefined): PositionVars {
  const d = n.detail;
  const c = n.cell;
  const exportPosition = cpFamily(d, 'export_position');
  const structuralOwn = cpFamily(d, 'export_position_structural');
  const structuralSame = structuralOwn === null && exportPosition !== null;
  return {
    cp: c.cp,
    share: c.share ?? d?.homeShare ?? null,
    exportPosition,
    structural: structuralOwn ?? exportPosition,
    structuralSame,
    investment: cpFamily(d, 'investment_position'),
    momentum: tradeMomentumWords(c, d, band),
    evidence: c.evidence ? EVIDENCE_LABEL[c.evidence] : null,
  };
}

/** Deployment & Use: deployment need and its families, each with its basis and scenario (null while the detail loads). */
export interface DeploymentVars {
  need: number | null;
  demand: Family | null;
  policyGap: Family | null;
  system: Family | null;
  /** the climate-aligned alternative: shown only as a labelled alternative, never used in the choice */
  alternative: Family | null;
  trend: string | null;
}
export function deploymentVars(n: FenceNode): DeploymentVars {
  const dn = n.detail?.deployment ?? null;
  const t = dn?.trend ?? null;
  return {
    need: n.cell.need,
    demand: dn?.demand ?? null,
    policyGap: dn?.policyGap ?? null,
    system: dn?.system ?? null,
    alternative: dn?.alternative ?? null,
    trend: t === null ? null : typeof t === 'number' ? `${t >= 0 ? '+' : ''}${(t * 100).toFixed(0)}% a year` : t,
  };
}

/** The deployment families the tables and the top lists name, in the drawer's order and words. */
export const DEPLOYMENT_FAMILIES = [
  { id: 'demand', label: 'Demand adequacy' },
  { id: 'policyGap', label: 'Target / climate gap' },
  { id: 'system', label: 'System need' },
] as const;

/** A family in words: "90 · Current policy · national data" (a ratio to current policies in its own words); null when not measured. */
export function familyWords(f: Family | null | undefined, scenarios: Record<string, string>): string | null {
  if (!f || f.value === null) return null;
  const ratio = ratioVsCurrentWords(f);
  return [ratio ?? score100(f.value), f.scenario ? (scenarios[f.scenario] ?? f.scenario) : null, ratio ? f.note : null, basisWords(f.basis)].filter(Boolean).join(' · ');
}

/** The deployment family with the highest score (0-1 scores only, never the climate-aligned ratio), or null. */
export function topDeploymentFamily(n: FenceNode): { label: string; family: Family } | null {
  const v = deploymentVars(n);
  let best: { label: string; family: Family } | null = null;
  for (const f of DEPLOYMENT_FAMILIES) {
    const fam = v[f.id];
    if (!fam || fam.value === null || fam.unit !== null) continue;
    if (!best || fam.value > (best.family.value as number)) best = { label: f.label, family: fam };
  }
  return best;
}

// ---- the one field that explains a choice ----------------------------------------------------------------

/**
 * The field of a top-list entry that explains its choice (what the Strategic Choices showed, compactly):
 * Secure the security rationale (and the routes met, where more than one); Preserve its position; Catch Up the
 * pathway and its legs; Partner the leading recommended partner (or no credible allied solution yet);
 * Accelerate the top deployment family; Watch the engine's reasons. Null when the data do not carry it.
 */
export function fenceReason(n: FenceNode, core: Pick<PostureCore, 'meta'>, names: Readonly<Record<string, string>>): string | null {
  const c = n.cell;
  const d = n.detail;
  const t: PostureThresholds = core.meta.thresholds;
  // a layer unit: its own choice's reason, from its variant and the layer's evidence (yards.ts layerReason)
  if (n.layer) return n.layer.reason;
  switch (n.verb) {
    case 'secure': {
      if (!c.rationale) return null;
      const routes = (d?.routes ?? []).map((r) => RATIONALE_LABEL[r as keyof typeof RATIONALE_LABEL] ?? r.replace(/_/g, ' '));
      return routes.length > 1 ? `${RATIONALE_LABEL[c.rationale]} (routes met: ${routes.join(', ')})` : RATIONALE_LABEL[c.rationale];
    }
    case 'preserve':
      return c.cp === null ? null : `Competitive Position ${score100(c.cp)}`;
    case 'catch_up': {
      const w = pathwayWords(c, d);
      return w ? `Pathway: ${w}` : null;
    }
    case 'partner': {
      if (c.partnerGap || d?.partnerGap) return 'No credible allied solution yet';
      const lead = d?.partners[0]?.iso3 ?? c.partner1;
      return lead ? `Best-placed partner: ${names[lead] ?? lead}` : null;
    }
    case 'accelerate': {
      const top = topDeploymentFamily(n);
      return top ? `Top need: ${top.label} ${familyWords(top.family, core.meta.scenario_labels)}` : null;
    }
    case 'watch': {
      const why = watchReasons(c, t, d);
      return why.length ? why.join('; ') : null;
    }
  }
}

/** A 0-1 share of world supply in words ("12.0%"), or null. */
export const shareWords = (v: number | null): string | null => (v === null ? null : pct(v, 1));
