// Synthetic posture / AIB / memo fixtures for the review QC tests (qc/*.test.ts). Test-only: compiled by
// tsconfig.node.json, never imported by browser code. Every builder writes the shapes the shipped bundles use
// (posture detail cells go through the app's own nodeDetail normaliser) so the QC functions read them exactly
// as they read the real bundles. Pure: no IO.

import { existsSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import type { AibDoc, AibNode } from '../../../aib/contract.ts';
import { decodeBundles } from '../bundles.ts';
import { loadRawBundles, type LoadResult } from '../node/loadBundles.ts';
import { SUPPLY_STAGES } from '../../../lib/stages.ts';
import type { PostureCell, PostureCore, PostureMeta, YfNode } from '../../../posture/contract.ts';
import { nodeDetail, type NodeDetail } from '../../../posture/detail.ts';
import type { DecodedBundles } from '../bundles.ts';

export const TH = { need: 0.67, prize: 0.6, cp: 0.75, deployment: 0.6, borderline_band: 0.05 };

type Step = { step: string; test: string; value?: number | string | null; threshold?: number | string | null; result: string };

const lvl = (v: number | null, t: number) => (v === null ? 'not_assessed' : v >= t ? 'high' : 'low');

/** The R trace of a producing-stage triage cell (secure no, large-yard no, need, prize, [position], [pathway], choice). */
export function triageTrace(o: { need: number | null; prize: number | null; cp?: number | null; pos?: string; pathway?: string; n?: number; large?: string; choice: string; secure?: string }): Step[] {
  const s: Step[] = [{ step: 'secure', test: 'secure_override', result: o.secure ?? 'no' }];
  s.push({ step: 'large_yard', test: 'large_yard_case', value: String(o.n ?? 2), threshold: '5', result: o.large ?? 'no' });
  s.push({ step: 'need', test: 'supply_chain_need', value: o.need, threshold: TH.need, result: lvl(o.need, TH.need) });
  s.push({ step: 'prize', test: 'strategic_industrial_value', value: o.prize, threshold: TH.prize, result: lvl(o.prize, TH.prize) });
  if (o.pos !== undefined) s.push({ step: 'position', test: 'cp_contender_pct', value: o.cp ?? null, threshold: TH.cp, result: o.pos });
  if (o.pathway !== undefined) s.push({ step: 'pathway', test: 'pathway_credible', value: null, result: o.pathway });
  s.push({ step: 'choice', test: 'choice', result: o.choice });
  return s;
}

export interface CellSpec {
  iso3?: string;
  tech?: string;
  stage?: string;
  choice: string;
  need?: number | null;
  prize?: number | null;
  market?: number | null;
  cp?: number | null;
  share?: number | null;
  pathway?: 'yes' | 'no' | 'unknown' | null;
  borderline?: string[];
  trace?: Step[];
  reason?: string | null;
  extra?: Record<string, unknown>;
}

const POSTURE_OF: Record<string, 'secure' | 'compete' | 'deploy' | 'not_assessed'> = {
  secure: 'secure', preserve: 'compete', catch_up: 'compete', partner: 'compete', accelerate: 'deploy', watch: 'deploy', not_assessed: 'not_assessed',
};
const CODE_OF = { secure: 'S', compete: 'C', deploy: 'D', not_assessed: 'N' } as const;

/** A normalised detail cell (through the app's nodeDetail). */
export function mkDetail(s: CellSpec): NodeDetail {
  const stage = s.stage ?? 'Refining';
  const posture = POSTURE_OF[s.choice];
  const raw: Record<string, unknown> = {
    tech: s.tech ?? 'Batteries',
    stage,
    posture,
    mode: posture === 'compete' ? s.choice : null,
    deploy_mode: posture === 'deploy' ? s.choice : null,
    need: s.need ?? null,
    cp: s.cp ?? null,
    home_share: s.share ?? null,
    prize: stage === 'Deployment' ? undefined : { value: s.prize ?? null, measure: 'strategic_value', market: s.market ?? null },
    pathway_credible: s.pathway ?? null,
    borderline: s.borderline ?? [],
    trace: s.trace ?? [],
    reason: s.reason ?? null,
    security_rationale: s.choice === 'secure' ? 'coercive_chokepoint' : null,
    ...(s.extra ?? {}),
  };
  return nodeDetail(raw);
}

/** The core row of the same cell. */
export function mkCell(s: CellSpec): PostureCell {
  const posture = POSTURE_OF[s.choice];
  const stage = (s.stage ?? 'Refining') as PostureCell['stage'];
  const yard = s.choice === 'secure' ? 'small' : s.choice === 'watch' ? 'large' : s.choice === 'not_assessed' ? null : 'medium';
  return {
    iso3: s.iso3 ?? 'AAA', tech: s.tech ?? 'Batteries', stage, posture: CODE_OF[posture], variant: null,
    mode: posture === 'compete' ? (s.choice as PostureCell['mode']) : null,
    deployMode: posture === 'deploy' ? (s.choice as PostureCell['deployMode']) : null,
    rationale: s.choice === 'secure' ? 'coercive_chokepoint' : null,
    needType: stage === 'Deployment' ? 'deployment' : 'supply_chain',
    need: s.need ?? null, prize: stage === 'Deployment' ? null : (s.prize ?? null), market: s.market ?? null, yards: yard as PostureCell['yards'],
    cp: s.cp ?? null, share: s.share ?? null, pathway: s.pathway ?? null, borderline: s.borderline?.length ? s.borderline.join(';') : null,
    confidence: 'high', flag: null, evidence: null, evidenceRaw: null, alignment: null, alignmentConfidence: null, alignmentFlag: null,
    partner1: null, partnerGap: null, momentum: null, momentumQuality: null,
  };
}

export function mkMeta(techs: string[], yf: Record<string, Partial<YfNode>> = {}): PostureMeta {
  const yf_nodes: Record<string, YfNode> = {};
  for (const [k, v] of Object.entries(yf)) {
    const [tech, stage, layer] = k.split('|');
    yf_nodes[k] = {
      key: k, tech, stage, layer, label: null, largeYard: false, n: 2, signals: [], unknown: [], floor: false, commons: 0.5, option: 0, commonsN: 3, optionN: 4, cap: null, ...v,
    };
  }
  return {
    techs, stages: [...SUPPLY_STAGES], stage_display: {}, benchmark_period: '2022-2024', momentum_period: 'Jan 2025 - Dec 2025', momentum_previous_period: null,
    momentum_through: null, momentum_band: 0.1, thresholds: { ...TH }, scenario_labels: {}, family_scenarios: { demand: 'current_policy', policy_gap: 'national_commitments', system: 'current_policy' },
    phrases: {}, core_members: 0, partner_members: 0, prize_measure: 'strategic_value', yard_codes: { S: 'small', M: 'medium', L: 'large' }, large_yard_signals: [], yf_nodes,
  };
}

/** A posture core and its detail documents from cell specs. */
export function mkPosture(specs: CellSpec[], meta?: PostureMeta): { core: PostureCore; details: Record<string, { iso3: string; country: string; created: string | null; cells: NodeDetail[] }> } {
  const techs = [...new Set(specs.map((s) => s.tech ?? 'Batteries'))];
  const isos = [...new Set(specs.map((s) => s.iso3 ?? 'AAA'))];
  const core: PostureCore = {
    created: '2026-01-01T00:00:00Z', gitCommit: 'abc1234', meta: meta ?? mkMeta(techs, Object.fromEntries(specs.map((s) => [`${s.tech ?? 'Batteries'}|${s.stage ?? 'Refining'}|stage`, {}]))), countries: isos.map((i) => ({ iso3: i, name: i, tier: 'none' })),
    cells: specs.map(mkCell), division: [], aibHeader: { core_members: 0, members: 0, priority_nodes: 0, covered_share: null, unresolved_gaps: 0 }, design: {},
  };
  const details: Record<string, { iso3: string; country: string; created: string | null; cells: NodeDetail[] }> = {};
  for (const s of specs) {
    const iso = s.iso3 ?? 'AAA';
    (details[iso] ??= { iso3: iso, country: iso, created: '2026-01-01T00:00:00Z', cells: [] }).cells.push(mkDetail(s));
  }
  return { core, details };
}

/** An AIB node with every exported field (overridable). */
export function mkNode(o: Partial<AibNode> & { id: string }): AibNode {
  return {
    label: o.id, techs: ['Batteries'], stage: 'Refining', cls: 'BUILD', reason: 'r', quality: 'good', flags: [],
    exposure: 0.5, static: 0.5, existing: 0.6, optimized: 0.8, dividend: 0.2, residual: 0.2, demandUsd: 1e9, lostUsd: 5e8,
    baseLargest: 'CHN', baseLargestShare: 0.5, largest: 'AUS', largestShare: 0.3, hhi: 0.2, hosts: [], bestGain: null, partners: null,
    treatment: null, compact: null, yfCells: [], yfSubcomponents: [], frontier: [],
    baseline: { hhi: 0.35, largest: 'CHN', largestShare: 0.5, top3Share: 0.8, suppliers: [['CHN', 0.5], ['AUS', 0.2], ['CHL', 0.1]] },
    ...o,
  } as AibNode;
}

/** A minimal AIB document around nodes (cast: only the fields the QC reads are written). */
export function mkAibDoc(nodes: AibNode[], extra: Record<string, unknown> = {}): AibDoc {
  return {
    schema: 'osi-aib', version: 3,
    meta: {
      headlineShock: 'china_100', shockSemantics: 'hold', shocks: [{ id: 'china_100', label: 'China supply removed' }],
      thresholds: { low_exposure: 0.15, adequate_coverage: 0.9, build_gap: 0.1, coordinate_dividend: 0.05, manage_largest_share: 0.4, manage_top3_share: 0.75, manage_cap: 0.4, manage_coverage_loss: 0.05, high_severity: 0.4 },
    },
    countries: {}, compacts: [], nodes, network: { edges: [] }, investmentFlows: { hosts: [] }, strategic: { compacts: [] },
    ...extra,
  } as unknown as AibDoc;
}

/** DecodedBundles around a posture core, an AIB doc and memos (stamps complete unless overridden). */
export function mkDecoded(o: { posture?: ReturnType<typeof mkPosture> | null; aib?: AibDoc | null; memos?: DecodedBundles['partnerships'] } = {}): DecodedBundles {
  return {
    posture: o.posture ? { manifest: { schema: 'osi-posture', schema_version: 2, created: o.posture.core.created, git_commit: o.posture.core.gitCommit }, core: o.posture.core, details: o.posture.details } : null,
    aib: o.aib ? { manifest: { created: '2026-01-01T00:00:00Z' }, doc: o.aib, managed: null, baseline: null, problems: [] } : null,
    partnerships: o.memos ?? null,
    manifests: { country: { created: 'c', git_commit: 'g' }, sector: { created: 's', git_commit: 'g' }, strategic: { created: 'e', git_commit: 'g' } },
    stamps: {
      posture: { created: '2026-01-01T00:00:00Z', git_commit: 'abc1234' }, country: { created: 'c', git_commit: 'g' }, sector: { bundle_id: 'b' }, aib: { created: 'a' },
      strategic: { bundle_id: 'e' }, memos: { manifest_sha256: 'f'.repeat(64) },
    },
    files: [],
    problems: [],
  };
}

// ---- the shipped bundles (tests that read them skip when public/data/posture is absent) ------------------------------

/** dashboard/public/data */
export const REAL_DATA_DIR = fileURLToPath(new URL('../../../../public/data/', import.meta.url));
export const HAVE_REAL = existsSync(`${REAL_DATA_DIR}posture/manifest.json`) && existsSync(`${REAL_DATA_DIR}aib/manifest.json`);

let realCache: { load: LoadResult; decoded: DecodedBundles } | null = null;
/** The shipped bundles, read and decoded once per test worker. */
export function loadReal(): { load: LoadResult; decoded: DecodedBundles } {
  if (!realCache) {
    const load = loadRawBundles(REAL_DATA_DIR);
    realCache = { load, decoded: decodeBundles(load.raw) };
  }
  return realCache;
}
