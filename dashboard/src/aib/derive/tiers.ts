// A compact's coalition and its tiers. The minimum coalition is found exactly (a binary program: the
// fewest economies whose coordination brings every node to the middle coverage target for the demand
// anchors, the anchors always in; among coalitions of that size, the one with the widest margin over the
// target in its weakest node, then the one that adds the most supply); a greedy heuristic (add the economy
// that most reduces the shortfall, one at a time) is kept beside it and can need more, or other,
// economies. When no coalition reaches the middle target the method starts with
// "greedy_sufficient_coalition" and the members are the widest coalition of the greedy path. The tiers:
//   core coordinating member                 a member that is not a critical supply partner or a prospective
//                                            host: the demand anchors, and suppliers one other candidate
//                                            could replace
//   prospective host                         a member in only for the simulated capacity it would host as a
//                                            leading investment host: its current supply is critical nowhere
//   critical supply partner                  a needed member no SINGLE other candidate can replace at the
//                                            middle target (substitutesNeeded: how many additions the exact
//                                            program needs instead; null: nothing restores it)
//   associate partner                        the fewest additions to the core (held in) that reach the
//                                            highest target; the highest target's own minimum coalition
//                                            need not contain the core
//   critical external dependency             not a member, but losing it pushes a node below the middle
//                                            target with the coalition fixed; supplies at least the critical
//                                            minimum share (1%) of the anchors' demand there
//   critical external dependency (marginal)  the same, under that share: critical only because the coalition
//                                            sits near the target
//   external dependency                      not a member; supplies a material share (5%) in a node
//   development host                         not a member; a leading development host of a BUILD node: future
//                                            capacity, never counted toward the target
// A member's substitutes are SINGLE substitutes: any one of them could replace it. An older bundle has no
// tiers: members read as core, other role holders as associates. Pure.

import { AIB_TIERS, coalitionMembersOf, type AibCompact } from '../contract.ts';
import { listWords, nameOf } from './format.ts';
import { targetLabel } from './coalition.ts';

export const TIER_LABEL: Readonly<Record<string, string>> = {
  'core coordinating member': 'Core coordinating members',
  'critical supply partner': 'Critical supply partners',
  'prospective host': 'Prospective hosts',
  'associate partner': 'Associate partners',
  'critical external dependency': 'Critical external dependencies',
  'critical external dependency (marginal)': 'Critical external dependencies (marginal)',
  'external dependency': 'External dependencies',
  'development host': 'Development hosts',
};

/** The singular label of a tier ("Critical supply partner"); the tier as written when unknown. */
export function tierName(tier: string | null | undefined): string {
  if (!tier) return '–';
  return tier[0].toUpperCase() + tier.slice(1);
}

/** The role names that mark a tier rather than a function (shown as the tier, not as a matrix column). */
export const TIER_ROLES = ['Critical supply partner', 'Critical external dependency', 'Critical external dependency (marginal)', 'External dependency'] as const;
export const isTierRole = (role: string): boolean => (TIER_ROLES as readonly string[]).some((r) => r.toLowerCase() === role.trim().toLowerCase());

const tierRank = (t: string) => {
  const i = (AIB_TIERS as readonly string[]).indexOf(t);
  return i < 0 ? AIB_TIERS.length : i;
};

/** The method of a coalition whose middle target is out of reach (its members are the widest greedy set). */
export const isGreedySetMethod = (method: string | null | undefined): boolean => typeof method === 'string' && /^greedy_sufficient_coalition/i.test(method);

/** The middle and the highest coverage targets (95% and 99% by default). */
export function coalitionTargets(targets: readonly number[]): { mid: number; top: number } {
  const t = [...targets].sort((a, b) => a - b);
  return { mid: t.length >= 2 ? t[1] : (t[0] ?? 0.95), top: t[t.length - 1] ?? 0.99 };
}

export interface TierShares {
  /** the critical minimum supply share ("1%") below which a critical external dependency is marginal */
  criticalMin: string | null;
  /** the share ("5%") from which a non-member is an external dependency */
  externalMin: string | null;
}

/** The supply shares behind the external tiers, as the exporter words them in the roles' basis; null when no role says. */
export function tierShares(compacts: readonly AibCompact[]): TierShares {
  let criticalMin: string | null = null;
  let externalMin: string | null = null;
  for (const c of compacts) {
    for (const r of c.roles) {
      const role = r.role.trim().toLowerCase();
      if (criticalMin === null && role === 'critical external dependency (marginal)') criticalMin = /under (\d+(?:\.\d+)?%)/.exec(r.basis)?.[1] ?? null;
      if (externalMin === null && role === 'external dependency') externalMin = /at least (\d+(?:\.\d+)?%)/.exec(r.basis)?.[1] ?? null;
    }
  }
  return { criticalMin, externalMin };
}

const NO_SHARES: TierShares = { criticalMin: null, externalMin: null };

/** What each tier means (the coalition card and About), sentence case. */
export function tierMeanings(o: { mid: number; top: number; reached?: boolean; associatesReach99?: boolean | null; shares?: TierShares }): Record<string, string> {
  const { mid, top, reached = true, associatesReach99 = null, shares = NO_SHARES } = o;
  const m = targetLabel(mid);
  const t = targetLabel(top);
  return {
    'core coordinating member': reached
      ? `Members of the exact minimum coalition (the fewest economies that bring every node to the ${m} target for the demand anchors' demand, the anchors always among them) that are not critical supply partners or prospective hosts: the demand anchors, and suppliers one other candidate could replace.`
      : `No coalition reaches the ${m} target: the members are the widest coalition found by adding partners one at a time: the demand anchors and the economies that bring the weakest step closest to the target.`,
    'critical supply partner': reached
      ? `Members the coalition needs at the ${m} target, for the supply they have today, that no single other candidate can replace: without one, only several other economies together restore the target, or none does.`
      : `Not tested: with the ${m} target out of reach, no member is called irreplaceable for it.`,
    'prospective host': `Members that are in the coalition only for the new capacity the model simulates at them as leading investment hosts (at most their current output in the node): the coalition does not depend on what they supply today, so they are neither core members nor critical. The capacity is hypothetical.`,
    'associate partner': reached
      ? `The fewest additions to the core coalition, held in, that reach the ${t} target${associatesReach99 === false ? ' (here no additions reach it)' : ''}. The ${t} minimum coalition solved on its own need not contain the core.`
      : `Not computed: the core coalition does not reach the ${m} target.`,
    'critical external dependency': `Not members, but losing one pushes a node below the ${m} target with the coalition fixed (where a node is below it already, lowers its coverage by a point or more); each supplies ${shares.criticalMin ? `at least ${shares.criticalMin}` : 'at least a set minimum share'} of the demand anchors' demand there.`,
    'critical external dependency (marginal)': `The same test, but each supplies under ${shares.criticalMin ?? 'that minimum share'} of the anchors' demand there: critical only because the coalition sits near the target.`,
    'external dependency': `Other non-members that each supply ${shares.externalMin ? `at least ${shares.externalMin}` : 'a material share'} of the demand anchors' demand in a node.`,
    'development host': `Middle-income economies with revealed capability, below scale, that lead a node's development hosts (by the Development Potential Index): where development finance could build future capacity. Their capacity never counts toward the ${m} target, so they are not members for it.`,
  };
}

export interface TierEconomy {
  iso3: string;
  name: string;
  tier: string;
  /** in the coalition (the exact minimum, or the widest greedy set when the target is out of reach) */
  member: boolean;
  /** a critical supply partner or a critical external dependency (marginal ones included) */
  critical: boolean;
  /** needed in the leave-one-out with the coalition fixed (null: the export does not say) */
  looCritical: boolean | null;
  /** SINGLE substitutes: any one of them, added to the rest, restores the middle target without this member */
  substitutes: string[];
  substituteNames: string[];
  /** how many additions restore the target without this member (null: none does, or not tested; undefined: the export predates it) */
  substitutesNeeded: number | null | undefined;
  /** its functional roles (demand anchor, specialized supplier, ...) */
  roles: string[];
  anchor: boolean;
}

export interface TierGroup {
  tier: string;
  label: string;
  meaning: string;
  economies: TierEconomy[];
}

const sameSet = (a: readonly string[], b: readonly string[]) => a.length === b.length && a.every((x) => b.includes(x));

/** Every economy of the compact's coalition and dependencies with its tier, by tier then anchor then name. */
export function tierEconomies(c: AibCompact, names: Readonly<Record<string, string>>): TierEconomy[] {
  const co = c.coalition;
  const members = coalitionMembersOf(c);
  const crit = co.criticalPartners ?? [];
  const prospective = co.prospectiveHosts ?? [];
  const critExt = co.criticalExternal ?? [];
  const marginal = co.criticalExternalMarginal ?? [];
  const ext = co.external ?? [];
  const codes = [...new Set([...c.roles.map((r) => r.iso3), ...members, ...c.associates, ...crit, ...critExt, ...marginal, ...ext])];
  const out = codes.map((iso3): TierEconomy => {
    const rs = c.roles.filter((r) => r.iso3 === iso3);
    const written = rs.map((r) => r.tier).find((t): t is string => typeof t === 'string' && t.length > 0);
    const member = members.includes(iso3) || rs.some((r) => r.member);
    const tier =
      written ??
      (crit.includes(iso3)
        ? 'critical supply partner'
        : prospective.includes(iso3)
          ? 'prospective host'
          : member
            ? 'core coordinating member'
            : critExt.includes(iso3)
              ? 'critical external dependency'
              : marginal.includes(iso3)
                ? 'critical external dependency (marginal)'
                : ext.includes(iso3)
                  ? 'external dependency'
                  : 'associate partner');
    const critical = rs.some((r) => r.critical === true) || crit.includes(iso3) || critExt.includes(iso3) || marginal.includes(iso3);
    const loo = rs.find((r) => typeof r.looCritical === 'boolean')?.looCritical;
    const substitutes = rs.map((r) => r.substitutes ?? []).find((s) => s.length > 0) ?? [];
    const needed = rs.find((r) => r.substitutesNeeded !== undefined);
    const roles = [...new Set(rs.map((r) => r.role).filter((r) => !isTierRole(r)))];
    return {
      iso3,
      name: nameOf(names, iso3),
      tier,
      member,
      critical,
      looCritical: typeof loo === 'boolean' ? loo : null,
      substitutes,
      substituteNames: substitutes.map((s) => nameOf(names, s)),
      substitutesNeeded: needed ? (needed.substitutesNeeded ?? null) : undefined,
      roles,
      anchor: c.anchors.includes(iso3) || roles.includes('Demand anchor'),
    };
  });
  return out.sort((a, b) => tierRank(a.tier) - tierRank(b.tier) || Number(b.anchor) - Number(a.anchor) || a.name.localeCompare(b.name, 'en'));
}

/** At most this many single substitutes are exported (most supply first). */
export const MAX_SUBSTITUTES = 5;

/**
 * What would replace a member's supply, in words ('' when there is nothing to say): for a critical supply
 * partner, how many other economies together restore the middle target without it (substitutesNeeded), or
 * that none does (null); for a core member, its single substitutes as alternatives ("any one of A or B
 * could replace its supply"), or that the rest of the coalition does without it. An export without
 * substitutesNeeded wrote the economies the exact program added instead; they are worded that way.
 */
export function replacementWords(e: TierEconomy, mid: number, reached: boolean): string {
  const m = targetLabel(mid);
  const n = e.substitutesNeeded;
  if (e.tier === 'critical supply partner') {
    if (typeof n === 'number' && n >= 2) return `no single other economy can replace it: without it, the coalition needs ${n} other economies together to reach the ${m} target`;
    if (typeof n === 'number') return `without it, the coalition needs ${n === 1 ? 'one other economy' : 'no other economy'} to reach the ${m} target`;
    if (n === null) return `without it, no coalition reaches the ${m} target`;
    return e.substituteNames.length ? `without it, the exact program adds ${listWords(e.substituteNames)}` : 'no single other economy can replace it';
  }
  if (!e.member || !reached) return '';
  if (e.substituteNames.length) {
    if (n === undefined) return `without its supply, the exact program adds ${listWords(e.substituteNames)}`;
    const more = e.substituteNames.length >= MAX_SUBSTITUTES ? ' (the five that supply most)' : '';
    return e.substituteNames.length === 1 ? `${e.substituteNames[0]} could replace its supply` : `any one of ${listWords(e.substituteNames, 'or')}${more} could replace its supply`;
  }
  if (n === 0 && !e.anchor) return `the rest of the coalition reaches the ${m} target without its supply`;
  return '';
}

/** The additions a member needs replacing, for the table ("0", "2", "none suffices"; null when not tested). */
export function additionsNeeded(e: TierEconomy, reached: boolean): string | null {
  const n = e.substitutesNeeded;
  if (!e.member || !reached || n === undefined) return null;
  if (n === null) return 'none suffices';
  return String(n);
}

/** The six tiers in order with what each means (empty tiers included), then any unknown tier. */
export function coalitionTiers(c: AibCompact, names: Readonly<Record<string, string>>, targets: readonly number[], shares: TierShares = NO_SHARES): TierGroup[] {
  const { mid, top } = coalitionTargets(targets);
  const meaning = tierMeanings({ mid, top, reached: coalitionMethod(c, targets).reached, associatesReach99: c.coalition.associatesReach99 ?? null, shares });
  const econ = tierEconomies(c, names);
  const tiers = [...new Set([...AIB_TIERS, ...econ.map((e) => e.tier)])];
  return tiers.map((t) => ({ tier: t, label: TIER_LABEL[t] ?? tierName(t), meaning: meaning[t] ?? '', economies: econ.filter((e) => e.tier === t) }));
}

export interface CoalitionMethod {
  /** the method as the exporter writes it (null in an older bundle) */
  method: string | null;
  /** the members are the exact minimum coalition at the middle target, minimality proven */
  exact: boolean;
  /** the exact program finished (false: it hit its time limit and minimality is not proven) */
  proven: boolean;
  /** some coalition reaches the middle target */
  reached: boolean;
  /** the members are the widest greedy set (the middle target is out of reach) */
  widest: boolean;
  mid: number;
  /** the coalition the pages show (coalitionMembersOf) */
  members: string[];
  /** the greedy sufficient coalition (null: not in the export; [] when it does not reach the target) */
  greedy: string[] | null;
  /** the greedy coalition is a different set from the members */
  greedyDiffers: boolean;
  /** the tie-break among minimum-size coalitions was solved (null: the export does not say) */
  tieBreakSolved: boolean | null;
  /** what the members are, as a heading */
  label: string;
}

/** How the compact's members were found, and how the greedy heuristic compares. */
export function coalitionMethod(c: AibCompact, targets: readonly number[]): CoalitionMethod {
  const { mid } = coalitionTargets(targets);
  const co = c.coalition;
  const method = co.method ?? null;
  const members = coalitionMembersOf(c);
  const widest = isGreedySetMethod(method);
  const midList = co.t95;
  const reached = !widest && midList.length > 0;
  const isExact = method !== null && /^exact/i.test(method);
  const proven = !(method !== null && /time limit/i.test(method));
  const exact = isExact && proven && reached && sameSet(midList, members);
  const greedy = co.greedy ?? null;
  const greedyDiffers = greedy !== null && greedy.length > 0 && !sameSet(greedy, members);
  const label = exact
    ? 'Exact minimum coalition'
    : reached
      ? isExact && !proven
        ? 'Best coalition found (minimality not proven)'
        : 'Minimum coalition'
      : `Widest coalition found (the ${targetLabel(mid)} target is not reached)`;
  const tieBreakSolved = typeof co.robustTieBreakSolved === 'boolean' ? co.robustTieBreakSolved : null;
  return { method, exact, proven, reached, widest, mid, members, greedy, greedyDiffers, tieBreakSolved, label };
}
