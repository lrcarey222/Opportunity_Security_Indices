// The fences of the Country page (2026-10-30, the Yards & Fences framing; the user's design "Only that
// fence's nodes"): which of an economy's nodes sit behind each fence, the top-five rankings and the variables
// each fence's tables show.
//   High fence     the Secure nodes, ranked by supply-chain need (highest first).
//   Medium fence   the producing-stage Preserve, Catch Up and Partner nodes, ranked by Industrial Opportunity
//                  (ties by Competitive Position); the Accelerate nodes, ranked by deployment need.
//   Low fence      the producing-stage Watch nodes, ranked by Industrial Opportunity (the biggest prizes the
//                  country is not chasing); the Deployment & Use Watch nodes, ranked by deployment need
//                  (closest to Accelerate).
// The fence of a node is grammar.ts nodeFence (its choice and stage). Every value is read through the
// drawer's accessors (the core cell; detail.ts for the components): nothing is recomputed or invented, and a
// value the data do not carry stays null (the page shows "not measured"). Pure: no React, no IO.

import { isProducingStage, stageRank } from '../lib/stages.ts';
import type { PostureCell, PostureCore, PostureDetail, PostureThresholds, Verb } from './contract.ts';
import { pct, score100 } from './derive.ts';
import { ratioVsCurrentWords, type Family, type NodeDetail } from './detail.ts';
import { basisWords, EVIDENCE_LABEL, MOMENTUM_LABEL, nodeFence, PATHWAY_LABEL, pathwayEvidenceWords, RATIONALE_LABEL, verbOf } from './grammar.ts';
import { postureMomentum } from './momentum.ts';
import { cellKeyOf, detailByKey, watchReasons } from './sections.ts';

/** One assessed node behind a fence: its core cell and, once the economy's detail document is in, its detail. */
export interface FenceNode {
  /** "Tech|Stage" */
  key: string;
  tech: string;
  stage: string;
  verb: Verb;
  cell: PostureCell;
  detail: NodeDetail | null;
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
const chain = (a: FenceNode, b: FenceNode) => stageRank(a.stage) - stageRank(b.stage) || a.tech.localeCompare(b.tech);
const byNeed = (a: FenceNode, b: FenceNode) => v1(b.cell.need) - v1(a.cell.need) || chain(a, b);
const byPrize = (a: FenceNode, b: FenceNode) => v1(b.cell.prize) - v1(a.cell.prize) || v1(b.cell.cp) - v1(a.cell.cp) || chain(a, b);
const byPosition = (a: FenceNode, b: FenceNode) => v1(b.cell.cp) - v1(a.cell.cp) || v1(b.cell.prize) - v1(a.cell.prize) || chain(a, b);

/** Nodes in Competitive Position order (highest first, ties by Industrial Opportunity; a missing position last): the position tables. */
export const byPositionOrder = (nodes: readonly FenceNode[]): FenceNode[] => [...nodes].sort(byPosition);

/** The economy's assessed nodes behind each fence, each list in its ranking order (not assessed nodes sit behind no fence). */
export function fenceGroups(cells: readonly PostureCell[], detail: PostureDetail | null): FenceGroups {
  const d = detailByKey(detail);
  const g: FenceGroups = { secure: [], compete: [], accelerate: [], watchSupply: [], watchDeploy: [] };
  for (const c of cells) {
    const verb = verbOf(c);
    const fence = nodeFence(verb, c.stage);
    if (!verb || !fence) continue;
    const key = cellKeyOf(c);
    const n: FenceNode = { key, tech: c.tech, stage: c.stage, verb, cell: c, detail: d.get(key) ?? null };
    if (fence === 'high') g.secure.push(n);
    else if (fence === 'medium') (COMPETE.has(verb) ? g.compete : g.accelerate).push(n);
    else (isProducingStage(c.stage) ? g.watchSupply : g.watchDeploy).push(n);
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

/** Medium and low fence, the prize: Industrial Opportunity and its components, supply-chain need as context, the Catch Up pathway. */
export interface PrizeVars {
  opportunity: number | null;
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
export function prizeVars(n: FenceNode): PrizeVars {
  const p = n.detail?.prize ?? null;
  return {
    opportunity: n.cell.prize,
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
      return lead ? `Leading partner: ${names[lead] ?? lead}` : null;
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
