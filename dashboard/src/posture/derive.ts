// Pure derivations for the posture headline charts: one country's cells, posture counts, the generated
// takeaway sentences, the trade freshness line and the AIB header. No React here.

import type { DivisionNode, PostureCell, PostureCode, PostureCore } from './contract.ts';
import { MODE_LABEL, MOMENTUM_UNAVAILABLE, POSTURE_LABEL, RATIONALE_LABEL } from './grammar.ts';

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
  const top = (m: 'catch_up' | 'preserve' | 'partner') =>
    cells.filter((c) => c.mode === m).sort((a, b) => (b.opp ?? 0) - (a.opp ?? 0)).slice(0, 2).map((c) => names[c.iso3] ?? c.iso3);
  const bits: string[] = [];
  if (n.S) bits.push(`${plural(n.S, 'country', 'countries')} must secure it`);
  if (n.preserve) bits.push(`${n.preserve} hold an advantage to preserve (${listAnd(top('preserve'))})`);
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
