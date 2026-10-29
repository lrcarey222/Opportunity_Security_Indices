// Country roles in the emergent Allied Industrial Base network (2026-10-17): the economy-level summary of
// the headline run (doc.network, scripts/32 emergent_aib_country_summary + emergent_aib_edges) read
// beside the model's own per-compact roles (countryRoles.ts) and the largest optimized supplier of each
// node. No new score: every cell is one of the model's own metrics, and a role is NAMED only where they
// support it (ROLE_READING says how each is read). Without doc.network (an older bundle) the cells fall
// back to the per-compact roles alone. Pure.

import { DASH, isNum } from '../../lib/format.ts';
import {
  diversifyShockIdOf, inStrategicScope, type AibCompact, type AibDoc, type AibManagedScenarioDoc, type AibMeta, type AibNetwork, type AibNetworkCountry, type AibNetworkEdge,
  type AibNetworkSide, type AibNode, type AibRole,
} from '../contract.ts';
import { countryRoleRows, type CountryRoleRow } from './countryRoles.ts';
import { nameOf, plural } from './format.ts';
import { headlineNetwork, nodeScenario, scenarioInfo } from './scenarios.ts';
import { shockLabelOf } from './settings.ts';

/**
 * The floor for naming a network role: the economy's contribution under the headline shock is at least
 * 1 point of one node's demand (0.01, summed over the nodes and partners of the role). Below it the role
 * is "minor": the solution selects the economy, but for very little supply.
 */
export const ROLE_FLOOR = 0.01;

export const MATRIX_ROLES = ['supplier', 'specialized', 'host', 'investor', 'buyer', 'bridge'] as const;
export type MatrixRole = (typeof MATRIX_ROLES)[number];

/** How each role column is read: the header, the metric shown and when the role is named. */
export const ROLE_READING: Record<MatrixRole, { short: string; meaning: string; named: string }> = {
  supplier: {
    short: 'Resource / Supplier',
    meaning: 'exports to the economies that lose supply in the coordinated solution (resource partners: raw materials, mined or recycled)',
    named: 'named when its trade serves at least 1 point of node demand under the headline shock, or a compact names it a resource supplier',
  },
  specialized: {
    short: 'Specialized supplier',
    meaning: 'is the largest supplier of a node after coordination, or carries a material share of a compact’s supply with revealed specialization',
    named: 'named when it is the largest optimized supplier of at least one node, or a compact names it a specialized supplier',
  },
  host: {
    short: 'Investment host',
    meaning: 'is the best-ranked host for new capacity that serves other economies’ gaps',
    named: 'named when the new capacity it hosts serves at least 1 point of node demand under the headline shock, or a compact names it a scaling host',
  },
  investor: {
    short: 'Investor',
    meaning: 'has a gap that new capacity at a host serves: the economy that would invest',
    named: 'named when the new capacity that serves it is at least 1 point of node demand under the headline shock',
  },
  buyer: {
    short: 'Demand anchor / Buyer',
    meaning: 'imports the supply that coordination restores (a demand anchor imports most of a compact’s lost supply)',
    named: 'named when the trade it receives is at least 1 point of node demand under the headline shock, or a compact names it a demand anchor',
  },
  bridge: {
    short: 'Bridge',
    meaning: 'supplies one node of a compact and depends on another, spans its stages or sits in several compacts',
    named: 'named when a compact names it a bridge partner',
  },
};

export type RoleLevel = 'named' | 'minor' | 'none';

export interface RoleCell {
  level: RoleLevel;
  /** the network side behind the cell (supplier, host, investor, buyer) */
  side: AibNetworkSide | null;
  /** compacts that give the economy the matching per-compact role */
  compacts: string[];
  /** specialized: the nodes where it is the largest optimized supplier */
  nodes: string[];
  /**
   * Decoupling read on the headline only (scenarioRoleSource()): the distinct partners of the side among the
   * relationships with a headline contribution. The side's own `partners` and `selected` then count every
   * shock of the network (its "every shock" totals). Absent elsewhere: the side's counts are the scenario's.
   */
  headlinePartners?: number;
}

export interface CountryRoles {
  iso3: string;
  name: string;
  cells: Record<MatrixRole, RoleCell>;
  /** compacts whose coalition includes the economy, and those it joins only as an associate */
  member: string[];
  associate: string[];
  /** the network row (null: the economy is in no selected relationship of the network) */
  net: AibNetworkCountry | null;
}

const EMPTY: RoleCell = { level: 'none', side: null, compacts: [], nodes: [] };

/**
 * A network side's level: named at or above the floor, minor when selected below it, none otherwise.
 * `headlineOnly` (the scenario views): minor only where the scenario's own shock contributes (0 < headline
 * < floor); a relationship selected under another shock alone does not count.
 */
export function sideLevel(s: AibNetworkSide | null | undefined, floor = ROLE_FLOOR, headlineOnly = false): RoleLevel {
  if (!s) return 'none';
  if (s.headline >= floor) return 'named';
  if (headlineOnly) return s.headline > 0 ? 'minor' : 'none';
  return s.selected > 0 || s.cumulative > 0 ? 'minor' : 'none';
}

const lift = (level: RoleLevel, byCompact: boolean): RoleLevel => (byCompact ? 'named' : level);

function rolesOf(
  iso3: string, name: string, net: AibNetworkCountry | null, cr: CountryRoleRow | null, largestOf: readonly string[], headlineOnly = false,
  partners?: Readonly<Record<SideRole, number>> | null,
): CountryRoles {
  const r = cr?.roles ?? {};
  const resource = r['Resource supplier'] ?? [];
  const other = r['Supplier'] ?? [];
  const spec = r['Specialized supplier'] ?? [];
  const host = r['Scaling host'] ?? [];
  const anchor = r['Demand anchor'] ?? [];
  const bridge = r['Bridge partner'] ?? [];
  const uniq = (xs: string[]) => [...new Set(xs)];
  const level = (x: AibNetworkSide | null | undefined) => sideLevel(x, ROLE_FLOOR, headlineOnly);
  const supplierLevel = net ? level(net.supplier) : other.length ? 'minor' : 'none';
  // the headline partners where the source counts them (Decoupling), on the sides the network carries
  const hp = (k: SideRole): { headlinePartners?: number } => (partners !== undefined && net ? { headlinePartners: partners?.[k] ?? 0 } : {});
  const cells: Record<MatrixRole, RoleCell> = {
    supplier: { level: lift(supplierLevel, resource.length > 0), side: net?.supplier ?? null, compacts: uniq([...resource, ...other]), nodes: [], ...hp('supplier') },
    specialized: { level: spec.length || largestOf.length ? 'named' : 'none', side: null, compacts: spec, nodes: [...largestOf] },
    host: { level: lift(net ? level(net.host) : 'none', host.length > 0), side: net?.host ?? null, compacts: host, nodes: [], ...hp('host') },
    investor: { level: net ? level(net.investor) : 'none', side: net?.investor ?? null, compacts: [], nodes: [], ...hp('investor') },
    buyer: { level: lift(net ? level(net.buyer) : 'none', anchor.length > 0), side: net?.buyer ?? null, compacts: anchor, nodes: [], ...hp('buyer') },
    bridge: { level: bridge.length ? 'named' : 'none', side: null, compacts: bridge, nodes: [] },
  };
  return { iso3, name, cells, member: cr?.member ?? [], associate: cr?.associate ?? [], net };
}

/** The four network sides of an economy. */
export type SideRole = 'supplier' | 'buyer' | 'host' | 'investor';

/**
 * Each economy's distinct partners per side among a network's relationships with a headline contribution:
 * trade (trade > 0) makes the exporter a supplier of the importer and the importer a buyer; new capacity
 * (investment > 0) runs host -> investor. The headline counterpart of the sides' `partners`, which count
 * every shock of the network.
 */
export function headlinePartnersOf(net: Pick<AibNetwork, 'edges'>): Map<string, Record<SideRole, number>> {
  const sets = new Map<string, Record<SideRole, Set<string>>>();
  const of = (iso: string) => {
    let s = sets.get(iso);
    if (!s) sets.set(iso, (s = { supplier: new Set(), buyer: new Set(), host: new Set(), investor: new Set() }));
    return s;
  };
  for (const e of net.edges) {
    if (e.trade > 0) {
      of(e.from).supplier.add(e.to);
      of(e.to).buyer.add(e.from);
    }
    if (e.investment > 0) {
      of(e.from).host.add(e.to);
      of(e.to).investor.add(e.from);
    }
  }
  return new Map([...sets].map(([iso, s]) => [iso, { supplier: s.supplier.size, buyer: s.buyer.size, host: s.host.size, investor: s.investor.size }]));
}

/** The node ids (in the strategic scope) where each economy is the largest supplier after coordination. */
function largestByEconomy(doc: Pick<AibDoc, 'nodes'>): Map<string, string[]> {
  const m = new Map<string, string[]>();
  for (const n of doc.nodes) {
    if (!n.largest || n.inStrategicScope === false) continue;
    m.set(n.largest, [...(m.get(n.largest) ?? []), n.id]);
  }
  return m;
}

/**
 * One row per economy that holds a network role, a compact role or a compact seat: by compact
 * participation, then by name (no ranking of roles). `src` (scenarioRoleSource()): the network, compacts
 * and largest suppliers of a scenario in place of the document's own, read on the headline only.
 */
export function countryRoleMatrix(doc: Pick<AibDoc, 'compacts' | 'nodes' | 'network'>, names: Readonly<Record<string, string>>, src?: RoleSource | null): CountryRoles[] {
  const compacts: readonly AibCompact[] = src ? src.compacts.map(asRoleCompact) : doc.compacts;
  const cr = new Map(countryRoleRows({ compacts: compacts as AibCompact[] }, names).map((r) => [r.iso3, r]));
  const net = new Map(((src ? src.network : doc.network)?.countries ?? []).map((c) => [c.iso3, c]));
  const largest = src ? src.largestOf : largestByEconomy(doc);
  const isos = new Set([...cr.keys(), ...net.keys(), ...largest.keys()]);
  const hp = src?.partnersOf;
  const rows = [...isos].map((iso) =>
    rolesOf(iso, nameOf(names, iso), net.get(iso) ?? null, cr.get(iso) ?? null, largest.get(iso) ?? [], src?.headlineOnly ?? false, hp ? (hp.get(iso) ?? null) : undefined),
  );
  const part = (r: CountryRoles) => new Set([...r.member, ...r.associate]).size;
  return rows
    .filter((r) => part(r) > 0 || MATRIX_ROLES.some((k) => r.cells[k].level !== 'none'))
    .sort((a, b) => part(b) - part(a) || a.name.localeCompare(b.name, 'en'));
}

/** One economy's roles (every cell 'none' when it holds nothing); `src`: under a scenario (scenarioRoleSource()). */
export function countryRolesOf(doc: Pick<AibDoc, 'compacts' | 'nodes' | 'network'>, names: Readonly<Record<string, string>>, iso3: string, src?: RoleSource | null): CountryRoles {
  const iso = iso3.toUpperCase();
  const row = countryRoleMatrix(doc, names, src).find((r) => r.iso3 === iso);
  if (row) return row;
  const cells = Object.fromEntries(MATRIX_ROLES.map((k) => [k, EMPTY])) as Record<MatrixRole, RoleCell>;
  return { iso3: iso, name: nameOf(names, iso), cells, member: [], associate: [], net: null };
}

/** The value a role is ordered by: the headline contribution of a network side, else the count of compacts / nodes. */
export function roleValue(r: CountryRoles, k: MatrixRole): number {
  const c = r.cells[k];
  if (c.side) return c.side.headline;
  return k === 'specialized' ? c.nodes.length + c.compacts.length : c.compacts.length;
}

/** The economies holding a role (named first, then minor), largest value first. */
export function roleHolders(rows: readonly CountryRoles[], k: MatrixRole, level: RoleLevel | 'any' = 'any'): CountryRoles[] {
  const rank = (l: RoleLevel) => (l === 'named' ? 0 : 1);
  return rows
    .filter((r) => (level === 'any' ? r.cells[k].level !== 'none' : r.cells[k].level === level))
    .sort((a, b) => rank(a.cells[k].level) - rank(b.cells[k].level) || roleValue(b, k) - roleValue(a, k) || a.name.localeCompare(b.name, 'en'));
}

/** The named roles of an economy, in column order. */
export const namedRoles = (r: CountryRoles): MatrixRole[] => MATRIX_ROLES.filter((k) => r.cells[k].level === 'named');

// ---- An economy's edges -----------------------------------------------------------------------------

export interface CountryEdge {
  partner: string;
  /** the economy is the supplier (exporter, or host of the new capacity) */
  supplies: boolean;
  edge: AibNetworkEdge;
  /** headline shock: trade and investment contributions (shares of node demand) */
  trade: number;
  investment: number;
}

/**
 * The economy's relationships in the network with a headline-shock contribution, largest first by the
 * share of node demand the relationship serves (trade plus investment: the same unit, the model's own
 * ordering). Each direction is its own relationship.
 */
export function countryEdges(doc: Pick<AibDoc, 'network'>, iso3: string): CountryEdge[] {
  const iso = iso3.toUpperCase();
  return (doc.network?.edges ?? [])
    .filter((e) => (e.from === iso || e.to === iso) && e.trade + e.investment > 0)
    .map((e) => ({ partner: e.from === iso ? e.to : e.from, supplies: e.from === iso, edge: e, trade: e.trade, investment: e.investment }))
    .sort((a, b) => b.trade + b.investment - (a.trade + a.investment) || a.partner.localeCompare(b.partner));
}

/** What a relationship means in words, from the economy's side ("supplies", "invests in", ...). */
export function edgeWords(e: CountryEdge): { trade: string; investment: string } {
  return e.supplies
    ? { trade: 'supplies', investment: 'hosts new capacity for' }
    : { trade: 'buys from', investment: 'invests in' };
}

/** What the network's "every shock" figures sum. */
export interface EveryShock {
  /** the supply shocks of network.variant.shocks (every one but the diversification scenario) */
  supplyShocks: number;
  /** the export counts the diversification scenario, which removes no supplier, among them */
  diversify: boolean;
  /** completes "across ...": "every shock", or "every shock and the diversification scenario" */
  phrase: string;
  /** the run note: '"every shock" sums the 5 supply shocks and the diversification scenario, which removes no supplier' */
  note: string;
}

/**
 * What the network's cumulative ("every shock") figures sum: the shocks of network.variant.shocks (side
 * cumulative, the edges' cum* and nShocks). Since 2026-10-19 the export may list the diversification
 * scenario among them (scripts/32 sums every shock id), though it removes no supplier: the words then name
 * it apart from the supply shocks.
 */
export function everyShock(doc: Pick<AibDoc, 'meta' | 'network'>): EveryShock {
  const shocks = doc.network?.variant.shocks ?? [];
  const dv = diversifyShockIdOf(doc.meta);
  const diversify = shocks.includes(dv);
  const supplyShocks = shocks.filter((s) => s !== dv).length;
  return diversify
    ? {
        supplyShocks,
        diversify,
        phrase: 'every shock and the diversification scenario',
        note: `"every shock" sums the ${plural(supplyShocks, 'supply shock')} and the diversification scenario, which removes no supplier`,
      }
    : { supplyShocks, diversify, phrase: 'every shock', note: `"every shock" sums the ${plural(shocks.length, 'configured shock')}` };
}

/** A contribution (share of node demand summed over nodes) as points: "24.6 pts", "<0.1 pts", "0 pts". */
export function fmtPts(x: number | null | undefined): string {
  if (!isNum(x)) return DASH;
  if (x === 0) return '0 pts';
  const v = x * 100;
  if (v < 0.1) return '<0.1 pts';
  // today's network sums whole supplies: thousands of points read with a separator
  if (v >= 1000) return `${Math.round(v).toLocaleString('en-US')} pts`;
  return `${v.toFixed(v >= 10 ? 0 : 1)} pts`;
}

// ---- Scenarios (2026-10-19) -------------------------------------------------------------------------
// The roles and the network follow the toggle. Decoupling reads doc.network through headlineNetwork()
// (only the edges with a headline contribution; a role is minor only where the headline shock contributes
// below the floor), the main document's compacts and each node's largest optimized supplier; its "every
// shock" totals stay, labelled as Decoupling's supply shocks (everyShockLabel). Managed Interdependence
// reads its scenario file: the network of its shock alone (cum* = headline), the compacts solved under it
// and node.diversify's largest supplier (a node it did not run on keeps today's). Baseline, today's
// observed 2024 trade, is the reference: its network carries the whole observed supply (supplier and buyer
// sides only; no new capacity exists today), while a scenario's network carries the supply it adds or
// redirects, so the two are compared role by role, never subtracted.

/** A compact's roles and seats as the role matrix reads them (a scenario file's compact may have no coalition). */
export interface RoleCompact {
  id: string;
  coalition: { members: string[] } | null;
  associates: string[];
  roles: AibRole[];
}

/** The compact shape countryRoleRows() reads (coalition members, associates, roles); a null coalition seats nobody. */
const asRoleCompact = (c: RoleCompact): AibCompact => ({ ...c, coalition: c.coalition ?? { members: [] } }) as unknown as AibCompact;

/** What the role matrix of a scenario reads. */
export interface RoleSource {
  scenario: 'decoupling' | 'managed';
  network: AibNetwork | null;
  compacts: readonly RoleCompact[];
  /** economy -> the strategic-scope nodes where it is the largest supplier under the scenario */
  largestOf: ReadonlyMap<string, string[]>;
  /** minor only where the scenario's own shock contributes (sideLevel's headlineOnly) */
  headlineOnly: boolean;
  /**
   * Decoupling: each economy's partners per side among the headline relationships (headlinePartnersOf()),
   * since the network's own partner and relationship counts sum every shock. Absent for Managed
   * Interdependence, whose network is its shock alone.
   */
  partnersOf?: ReadonlyMap<string, Record<SideRole, number>>;
}

/** economy -> nodes, from (node, largest supplier) pairs (pairs without a supplier left out). */
function byEconomy(pairs: readonly (readonly [string, string | null | undefined])[]): Map<string, string[]> {
  const m = new Map<string, string[]>();
  for (const [node, iso] of pairs) if (iso) m.set(iso, [...(m.get(iso) ?? []), node]);
  return m;
}

/**
 * The role matrix's inputs under a scenario: Decoupling the headline network of the main document, its
 * compacts and each node's largest optimized supplier; Managed Interdependence its file's network and
 * compacts, and node.diversify's largest supplier where it ran (today's elsewhere). Null for Managed
 * Interdependence while its file is not loaded.
 */
export function scenarioRoleSource(doc: Pick<AibDoc, 'compacts' | 'nodes' | 'network'>, id: 'decoupling' | 'managed', managed?: AibManagedScenarioDoc | null): RoleSource | null {
  const scope = doc.nodes.filter(inStrategicScope);
  if (id === 'decoupling') {
    const network = doc.network ? headlineNetwork(doc.network) : null;
    return {
      scenario: id,
      network,
      compacts: doc.compacts,
      largestOf: byEconomy(scope.map((n) => [n.id, n.largest])),
      headlineOnly: true,
      ...(network ? { partnersOf: headlinePartnersOf(network) } : {}),
    };
  }
  if (!managed) return null;
  return {
    scenario: id,
    network: managed.network ?? null,
    compacts: managed.compacts,
    largestOf: byEconomy(scope.map((n) => [n.id, nodeScenario(n, 'managed').largest])),
    headlineOnly: true,
  };
}

/** "Decoupling's five supply shocks" and the like: what the Decoupling network's cumulative ("every shock") figures sum. */
export interface EveryShockLabel {
  /** completes "across ...": "Decoupling's five supply shocks" */
  phrase: string;
  /** a column header: "Decoupling's 5 supply shocks" */
  header: string;
  /** a sentence of its own: "The every-shock totals sum Decoupling's five supply shocks (China supply removed; ...)." */
  note: string;
}

const NUMBER_WORDS = ['no', 'one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine', 'ten'];
const numberWord = (n: number): string => NUMBER_WORDS[n] ?? String(n);

/**
 * The words for the Decoupling network's cumulative figures: they sum the supply shocks of
 * network.variant.shocks (Decoupling's stress tests), and the diversification scenario apart where an
 * export counted it there. The labels come from meta.shocks.
 */
export function everyShockLabel(doc: Pick<AibDoc, 'meta' | 'network'>): EveryShockLabel {
  const e = everyShock(doc);
  const dv = diversifyShockIdOf(doc.meta);
  const label = scenarioInfo(doc.meta, 'decoupling').label;
  const supply = (doc.network?.variant.shocks ?? []).filter((s) => s !== dv);
  const tail = e.diversify ? ' and the diversification scenario' : '';
  const shockWords = (n: number) => (n === 1 ? 'supply shock' : 'supply shocks');
  const labels = supply.map((s) => shockLabelOf(doc.meta, s));
  return {
    phrase: `${label}'s ${numberWord(e.supplyShocks)} ${shockWords(e.supplyShocks)}${tail}`,
    header: `${label}'s ${e.supplyShocks} ${shockWords(e.supplyShocks)}${tail}`,
    note: `The every-shock totals sum ${label}'s ${numberWord(e.supplyShocks)} ${shockWords(e.supplyShocks)}${labels.length ? ` (${labels.join('; ')})` : ''}${e.diversify ? ', and the diversification scenario, which removes no supplier' : ''}.`,
  };
}

/** "under Decoupling", "under Managed Interdependence": the words that replace "under the headline shock". */
export const underScenario = (meta: AibMeta | null | undefined, id: 'decoupling' | 'managed'): string => `under ${scenarioInfo(meta, id).label}`;

/** When a role is named, in the words of a scenario ("... under Managed Interdependence"). */
export const roleNamedWords = (k: MatrixRole, under: string): string => ROLE_READING[k].named.replace(/under the headline shock/g, under);

/**
 * The economy whose supply Decoupling removes: the "<economy> supply removed" label of its shock, matched
 * to a name of the document (null when the label does not say, or the name is not found).
 */
export function decoupledEconomy(meta: AibMeta, names: Readonly<Record<string, string>>): string | null {
  const sid = scenarioInfo(meta, 'decoupling').shockId;
  const label = meta.shocks.find((s) => s.id === sid)?.label;
  const m = label ? /^(.+?) supply removed$/i.exec(label.trim()) : null;
  if (!m) return null;
  const who = m[1].trim().toLowerCase();
  return Object.keys(names).find((iso) => names[iso].trim().toLowerCase() === who) ?? null;
}

// ---- Today against a scenario --------------------------------------------------------------------------

/** One supplier -> buyer pair, today and under the scenario (null where the pair is not in that network). */
export interface PairCompare {
  from: string;
  to: string;
  today: AibNetworkEdge | null;
  scenario: AibNetworkEdge | null;
}

const pairKey = (e: Pick<AibNetworkEdge, 'from' | 'to'>) => `${e.from}|${e.to}`;
/** A scenario edge's contribution: trade plus new capacity (shares of node demand summed over nodes). */
export const edgeTotal = (e: Pick<AibNetworkEdge, 'trade' | 'investment'>): number => e.trade + e.investment;

/**
 * Who supplies whom today (Baseline network: observed supply) and under a scenario (its network: the supply
 * it adds or redirects), every pair of either, `iso3` keeping those the economy is in. `today` is the list
 * of today's pairs, largest observed supply (USD) first; `scenario` the scenario's, largest contribution
 * (trade plus new capacity, shares of node demand: new capacity has no USD in the network) first.
 */
export function supplyPairs(today: AibNetwork | null, scenario: AibNetwork | null, iso3?: string | null): { all: PairCompare[]; today: PairCompare[]; scenario: PairCompare[] } {
  const iso = iso3 ? iso3.toUpperCase() : null;
  const keep = (e: AibNetworkEdge) => !iso || e.from === iso || e.to === iso;
  const by = new Map<string, PairCompare>();
  for (const e of today?.edges ?? []) if (keep(e) && e.trade > 0) by.set(pairKey(e), { from: e.from, to: e.to, today: e, scenario: null });
  for (const e of scenario?.edges ?? []) {
    if (!keep(e) || edgeTotal(e) <= 0) continue;
    const k = pairKey(e);
    by.set(k, { ...(by.get(k) ?? { from: e.from, to: e.to, today: null }), scenario: e });
  }
  const all = [...by.values()];
  const tie = (a: PairCompare, b: PairCompare) => a.from.localeCompare(b.from) || a.to.localeCompare(b.to);
  return {
    all,
    today: all.filter((p) => p.today).sort((a, b) => b.today!.tradeUsd - a.today!.tradeUsd || b.today!.trade - a.today!.trade || tie(a, b)),
    scenario: all.filter((p) => p.scenario).sort((a, b) => edgeTotal(b.scenario!) - edgeTotal(a.scenario!) || tie(a, b)),
  };
}

/** The roles compared with today: a supplier, the largest supplier of a node, a host of new capacity, an investor. */
export const CHANGE_ROLES = ['supplier', 'specialized', 'host', 'investor'] as const;
export type ChangeRole = (typeof CHANGE_ROLES)[number];

/** One economy's roles today and under a scenario. Points are shares of node demand summed over the nodes. */
export interface RoleChange {
  iso3: string;
  name: string;
  today: {
    /** observed supply to the allied market (null: not in today's network) */
    supplier: number | null;
    buyer: number | null;
    /** the compared nodes where it is the largest supplier today */
    largestOf: string[];
  };
  scenario: {
    /** the supply it adds or redirects (null: not in the scenario's network) */
    supplier: number | null;
    buyer: number | null;
    /** new capacity it hosts, and new capacity that serves it */
    host: number | null;
    investor: number | null;
    largestOf: string[];
  };
  /** roles it holds under the scenario and not today: a new supplier, the largest supplier of a node, a host, an investor */
  gained: ChangeRole[];
  /** roles it holds today and not under the scenario: its supply removed (Decoupling), no longer the largest supplier of any node */
  lost: ChangeRole[];
  /** Decoupling: the economy whose supply to the allied market the scenario removes */
  removed: boolean;
  /** Managed Interdependence: the compared nodes where its share is held to the cap */
  capped: string[];
}

export interface RoleComparison {
  /** every economy with a role gained or lost, a removal or a cap; by name */
  rows: RoleChange[];
  /** the strategic-scope nodes compared for the largest supplier (both today's and the scenario's known) */
  nodes: number;
}

/**
 * The roles gained and lost under a scenario against today. A supplier is NEW when the scenario's added
 * supply names it (at least the floor) and today's observed supply does not; Decoupling's shocked economy
 * LOSES its supplier role (its supply is removed); the largest supplier of a node is gained or lost when the
 * count of compared nodes goes from none to some, or from some to none; hosting and investing in new
 * capacity are always gains (no new capacity exists today). Managed Interdependence removes no supplier:
 * its capped suppliers keep their role and supply less (`capped`).
 */
export function roleChanges(input: {
  scenario: 'decoupling' | 'managed';
  /** today's network (the Baseline scenario file) */
  today: AibNetwork | null;
  /** the scenario's network (scenarioRoleSource().network) */
  network: AibNetwork | null;
  nodes: readonly AibNode[];
  names: Readonly<Record<string, string>>;
  /** Decoupling: the economy its shock removes (decoupledEconomy()) */
  removed?: string | null;
}): RoleComparison {
  const { scenario, names } = input;
  const scope = input.nodes.filter(inStrategicScope);
  const pairs = scope
    .map((n) => ({ n, t: nodeScenario(n, 'baseline').largest, s: nodeScenario(n, scenario).largest }))
    .filter((x) => !!x.t && !!x.s);
  const todayLargest = byEconomy(pairs.map((x) => [x.n.id, x.t]));
  const scenLargest = byEconomy(pairs.map((x) => [x.n.id, x.s]));
  const capped = new Map<string, string[]>();
  if (scenario === 'managed') {
    for (const n of scope) {
      if (!nodeScenario(n, 'managed').ran) continue;
      for (const iso of n.diversify?.capped ?? []) capped.set(iso, [...(capped.get(iso) ?? []), n.id]);
    }
  }
  const t = new Map((input.today?.countries ?? []).map((c) => [c.iso3, c]));
  const s = new Map((input.network?.countries ?? []).map((c) => [c.iso3, c]));
  const removed = scenario === 'decoupling' && input.removed ? input.removed : null;
  const isos = new Set([...t.keys(), ...s.keys(), ...todayLargest.keys(), ...scenLargest.keys(), ...capped.keys()]);
  const named = (x: AibNetworkSide | null | undefined) => !!x && x.headline >= ROLE_FLOOR;
  const pts = (x: AibNetworkSide | null | undefined) => (x ? x.headline : null);
  const rows: RoleChange[] = [];
  for (const iso of isos) {
    const tc = t.get(iso) ?? null;
    const sc = s.get(iso) ?? null;
    const tl = todayLargest.get(iso) ?? [];
    const sl = scenLargest.get(iso) ?? [];
    const isRemoved = iso === removed;
    const gained: ChangeRole[] = [];
    const lost: ChangeRole[] = [];
    if (named(sc?.supplier) && !named(tc?.supplier) && !isRemoved) gained.push('supplier');
    if (isRemoved && named(tc?.supplier)) lost.push('supplier');
    if (sl.length > 0 && tl.length === 0) gained.push('specialized');
    if (tl.length > 0 && sl.length === 0) lost.push('specialized');
    if (named(sc?.host)) gained.push('host');
    if (named(sc?.investor)) gained.push('investor');
    const cap = capped.get(iso) ?? [];
    if (!gained.length && !lost.length && !cap.length && !(isRemoved && tc)) continue;
    rows.push({
      iso3: iso,
      name: nameOf(names, iso),
      today: { supplier: pts(tc?.supplier), buyer: pts(tc?.buyer), largestOf: tl },
      scenario: { supplier: pts(sc?.supplier), buyer: pts(sc?.buyer), host: pts(sc?.host), investor: pts(sc?.investor), largestOf: sl },
      gained,
      lost,
      removed: isRemoved,
      capped: cap,
    });
  }
  return { rows: rows.sort((a, b) => a.name.localeCompare(b.name, 'en')), nodes: pairs.length };
}

/** The economies that gain (or lose) one role, largest first: the new suppliers by added supply, the hosts and investors by their points, the largest suppliers by nodes. */
export function changeHolders(rows: readonly RoleChange[], k: ChangeRole, side: 'gained' | 'lost'): RoleChange[] {
  const v = (r: RoleChange): number => {
    if (k === 'specialized') return side === 'gained' ? r.scenario.largestOf.length : r.today.largestOf.length;
    if (k === 'supplier') return side === 'gained' ? (r.scenario.supplier ?? 0) : (r.today.supplier ?? 0);
    return r.scenario[k] ?? 0;
  };
  return rows.filter((r) => r[side].includes(k)).sort((a, b) => v(b) - v(a) || a.name.localeCompare(b.name, 'en'));
}
