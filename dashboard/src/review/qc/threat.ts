// Threat amelioration (before -> intervention -> after) for AIB nodes and compacts, from the fields the AIB bundle
// exports and nothing else (docs/qc_audit/audit_aib.md section 3; model.ts ThreatMetrics). The threat is the
// scenario's shock (Decoupling: the headline shock; Managed Interdependence: the benchmark supplier cap); the
// intervention is coordinated (optimized) trade plus the simulated new capacity at the listed hosts; "after" is
// the optimized state. A field the bundle does not export is null with a note saying so (risky share after the
// shock, the post-shock top-3 share, credible-supplier counts, time to capacity): nothing is modelled here.
// A host whose simulated gain is zero is never counted as an intervention. dependence_outcome compares the
// largest supplier before and after against the bundle's concentration line (meta.thresholds.manage_largest_share,
// 0.40). Pure: no IO.

import { tradeWhen, type AibCompact, type AibDoc, type AibInvestmentHost, type AibManagedScenarioDoc, type AibNode, type AibStrategicCompact, type AibStrategicCompactScenario } from '../../aib/contract.ts';
import { SCENARIO_NAME } from '../../content/vocabulary.ts';
import type { AibScenarioKey, ThreatMetrics, ThreatSide } from '../model.ts';

const nn = (v: unknown): number | null => (typeof v === 'number' && Number.isFinite(v) ? v : null);
const emptySide = (): ThreatSide => ({ hhi: null, largest_supplier: null, largest_share: null, top3_share: null, risky_share: null, coverage: null, n_credible_suppliers: null, n1_coverage: null });
const pct = (v: number | null): string => (v === null ? 'n/a' : `${Math.round(v * 1000) / 10}%`);
const usd = (v: number | null): string => (v === null ? 'n/a' : v >= 1e9 ? `US$${(v / 1e9).toFixed(1)}bn` : `US$${(v / 1e6).toFixed(0)}m`);

export const TIME_TO_CAPACITY_NOTE = 'Time to build new capacity is not modelled anywhere in the Explorer (there are no lead-time data), so it is never estimated.';
/** Country names from the AIB document (doc.countries), the code where it has none. Lists join with "; ": some names carry a comma ("Korea, Rep."). */
const namesOf = (doc: Pick<AibDoc, 'countries'>) => (iso3: string): string => doc.countries?.[iso3] ?? iso3;

/** The concentration line of the bundle (meta.thresholds.manage_largest_share); 0.40 when absent (with a note). */
export function concentrationLine(doc: Pick<AibDoc, 'meta'>, notes?: string[]): number {
  const v = nn(doc.meta?.thresholds?.manage_largest_share);
  if (v === null) notes?.push('meta.thresholds.manage_largest_share is missing: the 0.40 line of config/aib.yml is used');
  return v ?? 0.4;
}

/**
 * diversified: the largest share falls and ends below the line; shifted: a different single supplier ends at or
 * above the line; unchanged: neither; unknown: a share or supplier is not exported.
 */
export function dependenceOutcome(before: Pick<ThreatSide, 'largest_supplier' | 'largest_share'>, after: Pick<ThreatSide, 'largest_supplier' | 'largest_share'>, line: number): ThreatMetrics['dependence_outcome'] {
  const b = before.largest_share;
  const a = after.largest_share;
  if (b === null || a === null) return 'unknown';
  if (a < b - 1e-9 && a < line) return 'diversified';
  if (after.largest_supplier === null || before.largest_supplier === null) return a >= line ? 'unknown' : 'unchanged';
  if (after.largest_supplier !== before.largest_supplier && a >= line) return 'shifted';
  return 'unchanged';
}

/** The ranked hosts of a node with a positive simulated gain (a zero-gain host is not an intervention). */
export function effectiveHosts(hosts: readonly AibInvestmentHost[] | undefined, nodeId: string): { kept: AibInvestmentHost[]; zeroGain: AibInvestmentHost[] } {
  const mine = (hosts ?? []).filter((h) => h.node === nodeId).sort((x, y) => x.rank - y.rank);
  return { kept: mine.filter((h) => (h.gain ?? 0) > 0), zeroGain: mine.filter((h) => !((h.gain ?? 0) > 0)) };
}

function shockLabel(doc: AibDoc, shockId: string | null): string {
  const s = (doc.meta?.shocks ?? []).find((x) => x.id === shockId);
  return s ? s.label : (shockId ?? 'no shock');
}

/** Threat metrics of one AIB node under a scenario. extras.managed: the Managed Interdependence scenario file (its host gains). */
export function deriveNodeThreat(doc: AibDoc, nodeId: string, scenario: AibScenarioKey, extras: { managed?: AibManagedScenarioDoc | null } = {}): ThreatMetrics | null {
  const node: AibNode | undefined = doc.nodes.find((n) => n.id === nodeId);
  if (!node) return null;
  const notes: string[] = [];
  const line = concentrationLine(doc, notes);
  const bl = node.baseline ?? null;
  const before = emptySide();
  before.hhi = nn(bl?.hhi);
  before.largest_supplier = bl?.largest ?? node.baseLargest ?? null;
  before.largest_share = nn(bl?.largestShare) ?? nn(node.baseLargestShare);
  before.top3_share = nn(bl?.top3Share);
  notes.push(`before: the observed ${tradeWhen(doc.meta)} supply to the allied demand scope (node.baseline, scope-supply basis); n_credible_suppliers and N-1 coverage before the intervention are not exported (null).`);
  const after = emptySide();
  const subject = { kind: 'aib_node' as const, id: node.id, scenario, label: node.label };
  const base: Omit<ThreatMetrics, 'threat' | 'intervention' | 'before' | 'after' | 'dependence_outcome'> = {
    subject, lost_supply_usd: null, lost_supply_replaced_share: null, allied_coverage: null, remaining_gap: null, remaining_gap_usd: null, new_capacity_share: null,
    time_to_capacity_years: null, notes,
  };
  notes.push(TIME_TO_CAPACITY_NOTE);
  if (scenario === 'baseline') {
    notes.push(`${SCENARIO_NAME.baseline}: no shock is applied, so there is no intervention and no "after"; the after side repeats the observed state.`);
    Object.assign(after, before);
    before.coverage = 1;
    after.coverage = 1;
    return { ...base, threat: `${SCENARIO_NAME.baseline}: no shock`, intervention: 'none', before, after, dependence_outcome: 'unchanged' };
  }
  if (scenario === 'decoupling') {
    const shock = doc.meta?.headlineShock ?? null;
    const exposure = nn(node.exposure);
    const stat = nn(node.static);
    const opt = nn(node.optimized);
    const residual = nn(node.residual);
    const demand = nn(node.demandUsd);
    before.coverage = stat;
    before.risky_share = exposure;
    notes.push(`before.risky_share = exposure: the share of allied traded demand the shocked supplier (${shock ?? 'headline shock'}) supplied.`);
    before.n1_coverage = null;
    after.hhi = nn(node.hhi);
    after.largest_supplier = node.largest ?? null;
    after.largest_share = nn(node.largestShare);
    after.top3_share = null;
    after.risky_share = null;
    after.coverage = opt;
    after.n1_coverage = nn(node.n1Reoptimized);
    notes.push('after.top3_share: the top-3 share after coordinated trade is in the CSV (optimized_top3_share) but not in the bundle (null).');
    notes.push('after.risky_share: non-allied shares of the post-shock supply are not exported (null); see compact nonAlliedReliedOn / criticalExternal.');
    notes.push('n_credible_suppliers: optimized_n_suppliers is CSV-only (null).');
    const { kept, zeroGain } = effectiveHosts(doc.investmentFlows?.hosts, node.id);
    if (zeroGain.length) notes.push(`host(s) with zero simulated gain not counted as intervention: ${zeroGain.map((h) => `${h.host} (rank ${h.rank})`).join(', ')}`);
    const listed = (node.hosts ?? []).filter((h) => !zeroGain.some((z) => z.host === h));
    const gain = kept.length ? nn(kept[0].gain) : null;
    const replaced = exposure !== null && exposure > 0 && opt !== null && stat !== null ? (opt - stat) / exposure : null;
    if (nn(node.diversion) !== null) notes.push(`diversion: ${pct(nn(node.diversion))} of demand is served by exports redirected from buyers outside the scope.`);
    const interv = [
      `coordinated trade among allied and global suppliers (dividend ${pct(nn(node.dividend))} over existing relationships)`,
      residual !== null && residual > 0 ? (listed.length ? `new capacity at ${listed.join(', ')}${gain !== null ? ` (best simulated gain ${pct(gain)} of demand)` : ''}` : 'no host with a positive simulated gain') : 'no new capacity needed',
    ].join('; ');
    if (residual !== null && residual > 0 && gain === null) notes.push('new_capacity_share: no ranked host with a positive gain in investmentFlows (null).');
    return {
      ...base,
      threat: `${shockLabel(doc, shock)} (${doc.meta?.shockSemantics ?? 'hold'} semantics): ${pct(exposure)} of allied traded demand lost (${usd(nn(node.lostUsd))})`,
      intervention: interv,
      before,
      after,
      lost_supply_usd: nn(node.lostUsd),
      lost_supply_replaced_share: replaced,
      allied_coverage: opt,
      remaining_gap: residual,
      remaining_gap_usd: residual !== null && demand !== null ? residual * demand : null,
      new_capacity_share: residual !== null && residual > 0 ? gain : 0,
      dependence_outcome: dependenceOutcome(before, after, line),
    };
  }
  // managed interdependence
  const dv = node.diversify;
  if (!dv) {
    notes.push('the node has no Managed Interdependence record (node.diversify absent): every managed field is null.');
    return { ...base, threat: `${SCENARIO_NAME.managed}: not assessed for this node`, intervention: 'none', before, after, dependence_outcome: 'unknown' };
  }
  const stat = nn(dv.static);
  const opt = nn(dv.optimized);
  const residual = nn(dv.residual);
  const excess = nn(dv.excess);
  before.coverage = stat;
  before.risky_share = null;
  notes.push(`${SCENARIO_NAME.managed}: the threat is concentration, not a removed supplier; risky_share is not exported (null). diversify.currentHhi (${dv.currentHhi ?? 'n/a'}) is the world-export HHI, a different basis from before.hhi (scope supply).`);
  after.hhi = nn(dv.hhi);
  after.largest_supplier = dv.largest ?? null;
  after.largest_share = nn(dv.largestShare);
  after.top3_share = nn(dv.top3Share);
  after.coverage = opt;
  after.n1_coverage = null;
  notes.push('after.n1_coverage: no N-1 test is exported for the Managed scenario (null).');
  const hosts = extras.managed?.investmentFlows?.hosts;
  const { kept, zeroGain } = effectiveHosts(hosts, node.id);
  if (!extras.managed) notes.push('the Managed scenario file was not supplied: host gains cannot be checked, hosts are listed as exported.');
  if (zeroGain.length) notes.push(`host(s) with zero simulated gain not counted as intervention: ${zeroGain.map((h) => `${h.host} (rank ${h.rank}, ${h.rankBasis ?? 'no rank basis'})`).join(', ')}`);
  const listed = (dv.hosts ?? []).filter((h) => !zeroGain.some((z) => z.host === h));
  const gain = kept.length ? nn(kept[0].gain) : null;
  const status = dv.status;
  return {
    ...base,
    threat: `${SCENARIO_NAME.managed}: supplier share capped at ${pct(nn(dv.cap))} (benchmark HHI ${dv.benchmarkHhi ?? 'n/a'}); ${pct(excess)} of allied demand must shift (${usd(nn(dv.lostUsd ?? null))}); status: ${status}`,
    intervention:
      opt === null
        ? 'none: the scenario did not run on this node'
        : `coordinated trade under the cap${residual !== null && residual > 0 ? (listed.length ? `; new capacity at ${listed.join(', ')}` : '; no host with a positive simulated gain') : ''}`,
    before,
    after,
    lost_supply_usd: nn(dv.lostUsd ?? null),
    lost_supply_replaced_share: excess !== null && excess > 0 && opt !== null && stat !== null ? (opt - stat) / excess : null,
    allied_coverage: opt,
    remaining_gap: residual,
    remaining_gap_usd: nn(dv.residualUsd),
    new_capacity_share: residual !== null && residual > 0 ? gain : residual === null ? null : 0,
    dependence_outcome: opt === null ? 'unknown' : dependenceOutcome(before, after, line),
  };
}

/** Threat metrics of a compact: a strategic compact (SC-*) by its scenario record, or an AIB-Cxx compact by its metrics. */
export function deriveCompactThreat(doc: AibDoc, sid: string, scenario: AibScenarioKey): ThreatMetrics | null {
  const notes: string[] = [TIME_TO_CAPACITY_NOTE];
  const before = emptySide();
  const after = emptySide();
  const sc: AibStrategicCompact | undefined = doc.strategic?.compacts?.find((c) => c.id === sid);
  if (sc) {
    const s: AibStrategicCompactScenario | undefined = (sc.scenarios as Record<string, AibStrategicCompactScenario | undefined>)[scenario];
    const subject = { kind: 'compact' as const, id: sid, scenario, label: sc.name };
    if (!s) {
      notes.push(`the compact has no ${scenario} scenario record`);
      return blankCompact(subject, notes, before, after);
    }
    const cov = s.coverage ?? { static: null, existing: null, coordinated: null, postInvestment: null };
    const coord = nn(cov.coordinated);
    const stat = nn(cov.static);
    const post = nn(cov.postInvestment);
    const exposure = nn(s.exposure);
    const demand = nn(s.demandUsd);
    before.coverage = stat;
    before.risky_share = scenario === 'decoupling' ? exposure : null;
    after.coverage = coord;
    notes.push('Supplier concentration (the Herfindahl-Hirschman index and the largest supplier) is not published for a whole chain, so concentration before and after, and what happens to dependence, are not known.');
    if (post !== null && coord !== null) notes.push(`The ${pct(post - coord)} met by new capacity is arithmetic, not a finding: the simulated addition at each step, capped at that step's gap, is added to coordinated trade (methodology section 16.3).`);
    const ext = s.criticalExternal ?? [];
    const non = s.nonAlliedReliedOn ?? [];
    const name = namesOf(doc);
    if (ext.length || non.length) notes.push(`After the intervention the coalition still relies on suppliers outside the allied group: ${[...new Set([...ext, ...non])].map(name).join('; ')}.`);
    const gap = coord === null ? null : Math.max(1 - coord, 0);
    return {
      subject,
      threat: `${SCENARIO_NAME[scenario]}: ${pct(exposure)} of the compact's allied demand lost (${usd(nn(s.lostUsd))})`,
      intervention: `coordinated trade${s.buildHosts?.length ? `; new capacity in ${s.buildHosts.map(name).join('; ')}` : ''}${s.buildHostTies ? ` (no host named for ${s.buildHostTies} product${s.buildHostTies === 1 ? '' : 's'} needing new capacity, where hosts tie)` : ''}`,
      before,
      after,
      lost_supply_usd: nn(s.lostUsd),
      lost_supply_replaced_share: exposure !== null && exposure > 0 && coord !== null && stat !== null ? (coord - stat) / exposure : null,
      allied_coverage: coord,
      remaining_gap: gap,
      remaining_gap_usd: gap !== null && demand !== null ? gap * demand : null,
      new_capacity_share: post !== null && coord !== null ? Math.max(post - coord, 0) : null,
      time_to_capacity_years: null,
      dependence_outcome: 'unknown',
      notes,
    };
  }
  const c: AibCompact | undefined = doc.compacts?.find((x) => x.id === sid);
  if (!c) return null;
  const subject = { kind: 'compact' as const, id: sid, scenario, label: c.name ?? sid };
  if (scenario !== 'decoupling' && scenario !== 'managed') {
    notes.push('an AIB-Cxx compact has no record for the baseline scenario');
    return blankCompact(subject, notes, before, after);
  }
  const m = scenario === 'decoupling' ? c.metrics : null;
  const mg = scenario === 'managed' ? c.managed : null;
  before.hhi = nn(c.baseline?.hhi);
  const top = c.baseline?.suppliers?.[0];
  before.largest_supplier = top ? top[0] : null;
  before.largest_share = top ? nn(top[1]) : null;
  notes.push('after-intervention concentration is not exported per AIB-Cxx compact: dependence_outcome is unknown.');
  const stat = nn(m?.static ?? mg?.static);
  const coord = nn(m?.trade ?? mg?.optimized);
  const exposure = nn(m?.exposure ?? null) ?? (mg && nn(mg.lostUsd) !== null && nn(mg.demandUsd) ? (mg.lostUsd as number) / (mg.demandUsd as number) : null);
  before.coverage = stat;
  before.risky_share = scenario === 'decoupling' ? exposure : null;
  after.coverage = coord;
  const residual = nn(m?.residual ?? mg?.residual);
  const demand = nn(m?.demandUsd ?? mg?.demandUsd);
  if (m) notes.push('metrics.trade is the trade compact (members may open or expand relationships); metrics.investment adds the standard increment at member hosts.');
  return {
    subject,
    threat: `${SCENARIO_NAME[scenario]}: ${pct(exposure)} of the compact's allied demand lost (${usd(nn(m?.lostUsd ?? mg?.lostUsd))})`,
    intervention: m ? 'the trade compact among its members, then the investment increment at member hosts' : 'coordinated trade under the supplier cap',
    before,
    after,
    lost_supply_usd: nn(m?.lostUsd ?? mg?.lostUsd),
    lost_supply_replaced_share: exposure !== null && exposure > 0 && coord !== null && stat !== null ? (coord - stat) / exposure : null,
    allied_coverage: coord,
    remaining_gap: residual,
    remaining_gap_usd: nn(mg?.residualUsd) ?? (residual !== null && demand !== null ? residual * demand : null),
    new_capacity_share: m && nn(m.investment) !== null && nn(m.trade) !== null ? Math.max((m.investment as number) - (m.trade as number), 0) : null,
    time_to_capacity_years: null,
    dependence_outcome: 'unknown',
    notes,
  };
}

function blankCompact(subject: ThreatMetrics['subject'], notes: string[], before: ThreatSide, after: ThreatSide): ThreatMetrics {
  return {
    subject, threat: 'not exported', intervention: 'none', before, after, lost_supply_usd: null, lost_supply_replaced_share: null, allied_coverage: null,
    remaining_gap: null, remaining_gap_usd: null, new_capacity_share: null, time_to_capacity_years: null, dependence_outcome: 'unknown', notes,
  };
}
