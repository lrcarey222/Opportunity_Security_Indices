// Pure derivations for the posture cards: one country's cells, posture counts (and the words of a page's
// tile), the generated takeaway sentences, the trade freshness line, the AIB header, the node of the
// Sector card (?pnode, checked), and the rows of the table twins and CSVs with their provenance (source,
// as_of = the core's creation date, git_commit last). No React here.

import { rowsToCsv } from '../lib/csv.ts';
import { DEPLOYMENT_STAGE, isLegacyStage, isProducingStage, legacyStageTargetName, legacyStageWords } from '../lib/stages.ts';
import type { DivisionNode, PostureCell, PostureCode, PostureCore, PostureMeta } from './contract.ts';
import { ALIGNMENT_LABEL, GAP_LABEL, MODE_LABEL, MOMENTUM_LABEL, MOMENTUM_UNAVAILABLE, POSTURE_LABEL, POSTURE_ORDER, QUADRANT_LABEL, RATIONALE_LABEL } from './grammar.ts';

export interface PostureCounts {
  S: number;
  C: number;
  D: number;
  catch_up: number;
  preserve: number;
  partner: number;
  watch: number;
}

export function countryCells(core: PostureCore, iso3: string): PostureCell[] {
  return core.cells.filter((c) => c.iso3 === iso3);
}

export function cellKey(tech: string, stage: string): string {
  return `${tech}|${stage}`;
}

/** The technology and stage of a cell key ("Batteries|Refining") when both are in the data, else null (a page checks a URL value with it). */
export function cellKeyParts(key: string | null | undefined, meta: Pick<PostureMeta, 'techs' | 'stages'>): { tech: string; stage: string } | null {
  const parts = (key ?? '').split('|');
  if (parts.length !== 2) return null;
  const [tech, stage] = parts;
  return meta.techs.includes(tech) && meta.stages.includes(stage) ? { tech, stage } : null;
}

export function countPostures(cells: PostureCell[]): PostureCounts {
  const n: PostureCounts = { S: 0, C: 0, D: 0, catch_up: 0, preserve: 0, partner: 0, watch: 0 };
  for (const c of cells) {
    n[c.posture]++;
    if (c.mode) n[c.mode]++;
    else if (c.posture === 'D' && c.quadrant === 'watch') n.watch++;
  }
  return n;
}

const plural = (n: number, one: string, many = `${one}s`) => `${n} ${n === 1 ? one : many}`;
const listAnd = (xs: string[]) => (xs.length <= 1 ? xs.join('') : `${xs.slice(0, -1).join(', ')} and ${xs[xs.length - 1]}`);

/** The Country view's one-sentence takeaway. */
export function countryTakeaway(name: string, cells: PostureCell[]): string {
  if (!cells.length) return `${name} has no classified supply-chain nodes.`;
  const n = countPostures(cells);
  const secure = cells.filter((c) => c.posture === 'S');
  const byTech = (xs: PostureCell[]) => {
    const m = new Map<string, number>();
    for (const c of xs) m.set(c.tech, (m.get(c.tech) ?? 0) + 1);
    return [...m.entries()].sort((a, b) => b[1] - a[1] || a[0].localeCompare(b[0])).map(([t]) => t);
  };
  const parts: string[] = [];
  if (secure.length) {
    const nodes = secure.slice(0, 2).map((c) => `${c.tech} ${c.stage.toLowerCase()}`);
    const why = new Map<string, number>();
    for (const c of secure) if (c.rationale) why.set(RATIONALE_LABEL[c.rationale].toLowerCase(), (why.get(RATIONALE_LABEL[c.rationale].toLowerCase()) ?? 0) + 1);
    const whyText = [...why.entries()].sort((a, b) => b[1] - a[1]).map(([r, k]) => (why.size > 1 ? `${k} ${r}` : r)).join(', ');
    parts.push(`secure ${plural(n.S, 'node')} (${listAnd(secure.length > 2 ? [...nodes, `${secure.length - 2} more`] : nodes)}${whyText ? `; ${whyText}` : ''})`);
  }
  const modes = (['partner', 'catch_up', 'preserve'] as const).filter((m) => n[m] > 0).sort((a, b) => n[b] - n[a]);
  if (modes.length) {
    const techs = byTech(cells.filter((c) => c.posture === 'C')).slice(0, 2);
    const mix = modes.map((m) => `${n[m]} ${MODE_LABEL[m].toLowerCase()}`).join(', ');
    parts.push(`compete in ${plural(n.C, 'node')} (${mix}; most in ${listAnd(techs)})`);
  }
  if (!parts.length) return `${name} should deploy openly in all ${plural(n.D, 'node')}: none shows the need or opportunity that would justify intervention.`;
  return `${name} should ${parts.join(', ')} and deploy openly in the other ${n.D}.`;
}

/** The Sector view's takeaway for one technology x stage node across countries. */
export function sectorTakeaway(tech: string, stage: string, cells: PostureCell[], names: Record<string, string>): string {
  const n = countPostures(cells);
  const node = `${tech} ${stage.toLowerCase()}`;
  if (!cells.length) return `No country is classified in ${node}.`;
  if (stage === DEPLOYMENT_STAGE && n.D === cells.length) {
    return `In ${node}, every country should deploy openly: deployment is where the technology is used, and its supply risks are read at the stages that make or supply it.`;
  }
  const top = (m: 'catch_up' | 'preserve' | 'partner') =>
    cells.filter((c) => c.mode === m).sort((a, b) => (b.opp ?? 0) - (a.opp ?? 0)).slice(0, 2).map((c) => names[c.iso3] ?? c.iso3);
  const bits: string[] = [];
  if (n.S) bits.push(`${plural(n.S, 'country', 'countries')} must secure it`);
  // the two named countries are the whole group only when it has no more than two; else they lead it
  if (n.preserve) bits.push(`${n.preserve} hold an advantage to preserve (${n.preserve > 2 ? 'led by ' : ''}${listAnd(top('preserve'))})`);
  if (n.catch_up) bits.push(`${n.catch_up} have the need and opportunity to catch up`);
  if (n.partner) bits.push(`${n.partner} need it without the opportunity, and should partner`);
  if (!bits.length) return `In ${node}, no country shows the need or opportunity that would justify intervention: deploy openly.`;
  return `In ${node}, ${listAnd(bits)}.`;
}

export interface AibHeader {
  coreMembers: number;
  coveredShare: number | null;
  unresolvedGaps: number;
  priorityNodes: number;
}
export function aibHeader(core: PostureCore): AibHeader {
  const h = core.aibHeader;
  return { coreMembers: h.core_members, coveredShare: h.covered_share, unresolvedGaps: h.unresolved_gaps, priorityNodes: h.priority_nodes };
}

/** The AIB view's takeaway: where the allied base is thinnest against what members ask of it. */
export function aibTakeaway(division: DivisionNode[], tech: string | null): string {
  const nodes = (tech ? division.filter((d) => d.tech === tech) : division).filter((d) => d.partnerDemand + d.secureDemand > 0);
  const scope = tech ? `in ${tech}` : 'across the allied base';
  if (!nodes.length) return `No country's posture asks for partner or Secure supply ${scope}.`;
  const worst = [...nodes].sort((a, b) => sev(a) - sev(b) || b.partnerDemand + b.secureDemand - (a.partnerDemand + a.secureDemand));
  const gaps = nodes.filter((d) => d.gap !== 'none');
  const w = worst[0];
  const demand = w.partnerDemand + w.secureDemand;
  if (!gaps.length) return `Every node that countries rely on partners for ${scope} has a credible trusted holder and backups.`;
  return `${gaps.length} of the ${plural(nodes.length, 'node')} that countries rely on partners for ${scope} ${gaps.length === 1 ? 'has' : 'have'} a gap; the most pressing is ${w.tech} ${w.stage.toLowerCase()} (${gapWord(w)}), relied on by ${plural(demand, 'country', 'countries')}.`;
}
const SEV: Record<string, number> = { no_trusted_supplier: 0, single_point_of_failure: 1, insufficient_redundancy: 2, weak_evidence: 3, none: 4 };
const sev = (d: DivisionNode) => SEV[d.gap] ?? 4;
function gapWord(d: DivisionNode): string {
  return d.gap === 'no_trusted_supplier' ? 'no trusted supplier' : d.gap === 'single_point_of_failure' ? `${d.primary ?? 'one holder'} is a single point of failure` : d.gap === 'insufficient_redundancy' ? 'too few trusted holders' : 'weak evidence';
}

/** The trade freshness line shared by the three views. */
export function freshnessLine(meta: PostureCore['meta']): { benchmark: string; signal: string } {
  const b = meta.benchmark_period ? meta.benchmark_period.replace(/^(\d{4})-\d{2}(\d{2})$/, '$1–$2') : 'single year';
  return {
    benchmark: `Structural trade benchmark: ${b}`,
    signal: meta.momentum_through ? `Current signal through: ${meta.momentum_through}` : MOMENTUM_UNAVAILABLE,
  };
}

export function postureWord(p: PostureCode, mode: PostureCell['mode']): string {
  return p === 'C' && mode ? `${POSTURE_LABEL.C} · ${MODE_LABEL[mode]}` : POSTURE_LABEL[p];
}

export function pct(v: number | null | undefined, digits = 0): string {
  return v === null || v === undefined || !Number.isFinite(v) ? '—' : `${(v * 100).toFixed(digits)}%`;
}

// ---- names, stages and the counts of a page's tile --------------------------------------------------------

/** iso3 -> the economy's name in the posture data. */
export function countryNames(core: Pick<PostureCore, 'countries'>): Record<string, string> {
  return Object.fromEntries(core.countries.map((c) => [c.iso3, c.name]));
}

/** The stages of the data that make or supply something (every stage but Deployment), in the data's order. */
export function producingStagesOf(stages: readonly string[]): string[] {
  return stages.filter(isProducingStage);
}

/** The words of a posture-counts tile: "Secure 1 · Compete 2 · Deploy 63" and the Compete mix. */
export function postureCountsText(n: PostureCounts): { value: string; sub: string } {
  return {
    value: POSTURE_ORDER.map((p) => `${POSTURE_LABEL[p]} ${n[p]}`).join(' · '),
    sub: `Compete: ${(['preserve', 'catch_up', 'partner'] as const).map((m) => `${n[m]} ${MODE_LABEL[m].toLowerCase()}`).join(' · ')}`,
  };
}

/** One tile of a page's header (the AIB section's three metrics). */
export interface PostureTile {
  id: string;
  label: string;
  value: string;
  sub: string;
}

/** The AIB division's three header metrics, as tiles. */
export function aibHeaderTiles(core: PostureCore): PostureTile[] {
  const h = aibHeader(core);
  return [
    { id: 'core', label: 'Core coalition members', value: String(h.coreMembers), sub: `${core.aibHeader.members} members in all` },
    { id: 'covered', label: 'Priority nodes with credible trusted coverage', value: pct(h.coveredShare), sub: `of ${h.priorityNodes} nodes members ask partners to supply or secure` },
    { id: 'gaps', label: 'Unresolved gaps', value: String(h.unresolvedGaps), sub: 'priority nodes with a gap' },
  ];
}

// ---- the node of the Sector card (?pnode=Tech|Stage) ----------------------------------------------------

/** The node the Sector card opens on (design E6). */
export const DEFAULT_POSTURE_NODE = { tech: 'Batteries', stage: 'Refining' } as const;

export interface PostureNodeParse {
  tech: string;
  stage: string;
  /** "pnode=Batteries|Midstream (why; showing Batteries — Refining)" when the value was not used as written */
  problem: string | null;
}

/**
 * The node of a ?pnode=Tech|Stage value, checked against the data: a technology of meta.techs and one of
 * the `offered` stages (default: the producing stages). A bad value is not used silently: the result
 * falls back (the default node, keeping a valid technology) and says why in `problem`, worded like the
 * Sector link problems ("key=value (why)"). A legacy stage (Upstream, Midstream, Downstream) is reported
 * with the stages that replaced it; Downstream is used as Deployment when Deployment is offered.
 */
export function parsePostureNode(
  raw: string | null,
  meta: Pick<PostureMeta, 'techs' | 'stages'>,
  offered: readonly string[] = producingStagesOf(meta.stages),
  key = 'pnode',
): PostureNodeParse {
  const fallbackTech = meta.techs.includes(DEFAULT_POSTURE_NODE.tech) ? DEFAULT_POSTURE_NODE.tech : (meta.techs[0] ?? DEFAULT_POSTURE_NODE.tech);
  const fallbackStage = offered.includes(DEFAULT_POSTURE_NODE.stage) ? DEFAULT_POSTURE_NODE.stage : (offered[0] ?? DEFAULT_POSTURE_NODE.stage);
  if (raw === null || raw.trim() === '') return { tech: fallbackTech, stage: fallbackStage, problem: null };
  const bad = (why: string, tech = fallbackTech, stage = fallbackStage): PostureNodeParse => ({ tech, stage, problem: `${key}=${raw} (${why}; showing ${tech} — ${stage})` });
  const parts = raw.split('|').map((x) => x.trim());
  if (parts.length !== 2) return bad('not a technology|stage pair');
  const [tech, stage] = parts;
  if (!meta.techs.includes(tech)) return bad(`${tech || 'an empty name'} is not a technology of the posture data`);
  if (isLegacyStage(stage)) {
    const exact = legacyStageTargetName(stage);
    if (exact && offered.includes(exact)) return { tech, stage: exact, problem: `${key}=${raw} (${stage} is now ${exact}; showing ${tech} — ${exact})` };
    const words = exact ? `${stage} is now ${exact}, which this chart leaves out` : `${stage} is no longer a stage: it is now ${legacyStageWords(stage)}`;
    return bad(words, tech);
  }
  if (!offered.includes(stage)) {
    return bad(stage === DEPLOYMENT_STAGE ? `${stage} is left out here: every country deploys openly there` : `${stage || 'an empty name'} is not a supply-chain stage`, tech);
  }
  return { tech, stage, problem: null };
}

// ---- table twins and CSVs ------------------------------------------------------------------------------

/** The source named in every posture CSV. */
export const POSTURE_SOURCE = 'Opportunity Security Indices, strategic position framework';

export interface PostureProvenance {
  source: string;
  /** the date the core was written (YYYY-MM-DD; the timestamp as written when it is not ISO) */
  as_of: string | null;
  git_commit: string | null;
}

export function postureProvenance(core: Pick<PostureCore, 'created' | 'gitCommit'>, source = POSTURE_SOURCE): PostureProvenance {
  const c = core.created;
  return { source, as_of: c && /^\d{4}-\d{2}-\d{2}/.test(c) ? c.slice(0, 10) : c, git_commit: core.gitCommit };
}

const PROVENANCE_COLS = ['source', 'as_of', 'git_commit'] as const;

/** CSV text (no BOM; ChartCard adds it) of plain rows in `cols` order, with source, as_of and git_commit as the last three columns. */
export function postureCsvText(rows: Record<string, unknown>[], cols: readonly string[], prov: PostureProvenance): string {
  return rowsToCsv(rows.map((r) => ({ ...r, ...prov })), [...cols, ...PROVENANCE_COLS]);
}

/** One technology x stage node of one economy, for the Country and Sector table twins and CSVs. */
export interface PostureRow {
  /** the row key: the cell key "Tech|Stage" in a country's rows, the iso3 in a node's rows */
  key: string;
  iso3: string;
  country: string;
  /** coalition tier: "core", "partner", or null */
  tier: string | null;
  tech: string;
  stage: string;
  /** false: the node has no posture for this economy (kept, never dropped) */
  classified: boolean;
  posture: PostureCode | null;
  postureWord: string | null;
  mode: string | null;
  rationale: string | null;
  quadrant: string | null;
  need: number | null;
  opp: number | null;
  cp: number | null;
  share: number | null;
  alignment: string | null;
  evidence: string | null;
  momentum: string | null;
  partner1: string | null;
  partner1Name: string | null;
  partnerGap: boolean | null;
  borderline: string | null;
  flag: string | null;
  /** need and opportunity are both measured, so the node's scatter places it */
  plotted: boolean;
}

function postureRow(key: string, iso3: string, tech: string, stage: string, c: PostureCell | null, names: Record<string, string>, tiers: Record<string, string>): PostureRow {
  const tier = tiers[iso3];
  return {
    key, iso3, country: names[iso3] ?? iso3, tier: tier === 'core' || tier === 'partner' ? tier : null, tech, stage,
    classified: c !== null,
    posture: c?.posture ?? null,
    postureWord: c ? POSTURE_LABEL[c.posture] : null,
    mode: c?.posture === 'C' && c.mode ? MODE_LABEL[c.mode] : null,
    rationale: c?.posture === 'S' && c.rationale ? RATIONALE_LABEL[c.rationale] : null,
    quadrant: c?.quadrant ? (QUADRANT_LABEL[c.quadrant] ?? c.quadrant) : null,
    need: c?.need ?? null, opp: c?.opp ?? null, cp: c?.cp ?? null, share: c?.share ?? null,
    alignment: c?.alignment ? ALIGNMENT_LABEL[c.alignment] : null,
    evidence: c?.evidence ?? null,
    momentum: c?.momentum ? MOMENTUM_LABEL[c.momentum] : null,
    partner1: c?.partner1 ?? null,
    partner1Name: c?.partner1 ? (names[c.partner1] ?? c.partner1) : null,
    partnerGap: c?.partnerGap ?? null,
    borderline: c?.borderline ?? null,
    flag: c?.flag ? c.flag.replace(/_/g, ' ') : null,
    plotted: c !== null && c.need !== null && c.opp !== null,
  };
}

const tiersOf = (core: PostureCore) => Object.fromEntries(core.countries.map((c) => [c.iso3, c.tier]));

/** Every technology x stage node of one economy (meta.techs x meta.stages, in chain order): the 66-cell grid, unclassified nodes kept. */
export function countryPostureRows(core: PostureCore, iso3: string): PostureRow[] {
  const names = countryNames(core);
  const tiers = tiersOf(core);
  const by = new Map(countryCells(core, iso3).map((c) => [cellKey(c.tech, c.stage), c]));
  return core.meta.techs.flatMap((t) => core.meta.stages.map((s) => postureRow(cellKey(t, s), iso3, t, s, by.get(cellKey(t, s)) ?? null, names, tiers)));
}

/** Every economy classified in one node, Secure first, then Compete and Deploy; within each, highest Industrial Opportunity first (unmeasured last), then by name. */
export function sectorPostureRows(core: PostureCore, tech: string, stage: string): PostureRow[] {
  const names = countryNames(core);
  const tiers = tiersOf(core);
  const rank = (p: PostureCode | null) => (p ? POSTURE_ORDER.indexOf(p) : 3);
  return core.cells
    .filter((c) => c.tech === tech && c.stage === stage)
    .map((c) => postureRow(c.iso3, c.iso3, tech, stage, c, names, tiers))
    .sort((a, b) => rank(a.posture) - rank(b.posture) || (b.opp ?? -1) - (a.opp ?? -1) || a.country.localeCompare(b.country));
}

/** The CSV columns of the Country and Sector posture tables (provenance follows). */
export const POSTURE_CSV_COLS = [
  'country', 'iso3', 'coalition_tier', 'technology', 'stage', 'classified', 'posture', 'compete_mode', 'secure_rationale', 'quadrant',
  'strategic_need', 'industrial_opportunity', 'competitive_position', 'world_supply_share', 'policy_alignment', 'evidence', 'trade_momentum',
  'first_partner', 'first_partner_iso3', 'partner_gap', 'borderline', 'check',
] as const;

/** The posture rows as CSV records (POSTURE_CSV_COLS; shares and percentiles as fractions, 0 to 1). */
export function postureCsvRows(rows: PostureRow[]): Record<string, unknown>[] {
  return rows.map((r) => ({
    country: r.country, iso3: r.iso3, coalition_tier: r.tier, technology: r.tech, stage: r.stage, classified: r.classified, posture: r.postureWord,
    compete_mode: r.mode, secure_rationale: r.rationale, quadrant: r.quadrant, strategic_need: r.need, industrial_opportunity: r.opp,
    competitive_position: r.cp, world_supply_share: r.share, policy_alignment: r.alignment, evidence: r.evidence, trade_momentum: r.momentum,
    first_partner: r.partner1Name, first_partner_iso3: r.partner1, partner_gap: r.partnerGap, borderline: r.borderline, check: r.flag,
  }));
}

/** One allied division node, for the AIB table twin and CSV. */
export interface DivisionRow {
  key: string;
  tech: string;
  stage: string;
  primary: string | null;
  primaryName: string | null;
  primaryShare: number | null;
  /** the backups' names, "Bland (10%); Cland (5%)" */
  backups: string | null;
  backupsIso3: string[];
  redundancy: number;
  alliedShare: number | null;
  coerciveShare: number | null;
  leader: string | null;
  leaderName: string | null;
  leaderShare: number | null;
  gap: DivisionNode['gap'];
  gapLabel: string;
  /** the node's other gaps, in words ("Insufficient redundancy; Weak evidence") */
  otherGaps: string | null;
  partnerDemand: number;
  secureDemand: number;
  memberPartnerDemand: number;
  partnerGaps: number;
  capabilitySource: string;
  aibNodes: number | null;
}

/** The division nodes (one technology, or all with null), in the data's technology order, then chain order. */
export function divisionRows(core: PostureCore, tech: string | null): DivisionRow[] {
  const names = countryNames(core);
  const ti = (t: string) => core.meta.techs.indexOf(t);
  const si = (s: string) => core.meta.stages.indexOf(s);
  return core.division
    .filter((d) => tech === null || d.tech === tech)
    .slice()
    .sort((a, b) => ti(a.tech) - ti(b.tech) || si(a.stage) - si(b.stage))
    .map((d) => {
      const other = d.gaps.filter((g) => g !== d.gap && g !== 'none');
      return {
        key: cellKey(d.tech, d.stage), tech: d.tech, stage: d.stage,
        primary: d.primary, primaryName: d.primary ? (names[d.primary] ?? d.primary) : null, primaryShare: d.primaryShare,
        backups: d.backups.length ? d.backups.map((b) => `${names[b.iso3] ?? b.iso3} (${pct(b.share)})`).join('; ') : null,
        backupsIso3: d.backups.map((b) => b.iso3),
        redundancy: d.redundancy, alliedShare: d.alliedShare, coerciveShare: d.coerciveShare,
        leader: d.leader, leaderName: d.leader ? (names[d.leader] ?? d.leader) : null, leaderShare: d.leaderShare,
        gap: d.gap, gapLabel: GAP_LABEL[d.gap], otherGaps: other.length ? other.map((g) => GAP_LABEL[g]).join('; ') : null,
        partnerDemand: d.partnerDemand, secureDemand: d.secureDemand, memberPartnerDemand: d.memberPartnerDemand, partnerGaps: d.partnerGaps,
        capabilitySource: d.source === 'aib_position_share' ? 'Allied Industrial Base positions' : 'World supply shares',
        aibNodes: d.aibNodes,
      };
    });
}

/** The CSV columns of the division table (provenance follows). */
export const DIVISION_CSV_COLS = [
  'technology', 'stage', 'primary_holder', 'primary_holder_iso3', 'primary_share', 'backups', 'backups_iso3', 'redundancy', 'allied_share',
  'coercive_share', 'world_leader', 'world_leader_iso3', 'world_leader_share', 'gap', 'other_gaps', 'partner_demand', 'secure_demand',
  'coalition_members_asking', 'partner_gaps', 'capability_source', 'aib_nodes',
] as const;

export function divisionCsvRows(rows: DivisionRow[]): Record<string, unknown>[] {
  return rows.map((r) => ({
    technology: r.tech, stage: r.stage, primary_holder: r.primaryName, primary_holder_iso3: r.primary, primary_share: r.primaryShare, backups: r.backups,
    backups_iso3: r.backupsIso3.length ? r.backupsIso3.join(';') : null, redundancy: r.redundancy, allied_share: r.alliedShare, coercive_share: r.coerciveShare,
    world_leader: r.leaderName, world_leader_iso3: r.leader, world_leader_share: r.leaderShare, gap: r.gapLabel, other_gaps: r.otherGaps,
    partner_demand: r.partnerDemand, secure_demand: r.secureDemand, coalition_members_asking: r.memberPartnerDemand, partner_gaps: r.partnerGaps,
    capability_source: r.capabilitySource, aib_nodes: r.aibNodes,
  }));
}
