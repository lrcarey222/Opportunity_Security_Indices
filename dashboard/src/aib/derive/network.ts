// Country roles in the emergent Allied Industrial Base network (2026-10-17): the economy-level summary of
// the headline run (doc.network, scripts/32 emergent_aib_country_summary + emergent_aib_edges) read
// beside the model's own per-compact roles (countryRoles.ts) and the largest optimized supplier of each
// node. No new score: every cell is one of the model's own metrics, and a role is NAMED only where they
// support it (ROLE_READING says how each is read). Without doc.network (an older bundle) the cells fall
// back to the per-compact roles alone. Pure.

import { DASH, isNum } from '../../lib/format.ts';
import type { AibDoc, AibNetworkCountry, AibNetworkEdge, AibNetworkSide } from '../contract.ts';
import { countryRoleRows, type CountryRoleRow } from './countryRoles.ts';
import { nameOf } from './format.ts';

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
    meaning: 'exports to the economies that lose supply in the coordinated solution (upstream partners: resources)',
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
    named: 'named when the new capacity it hosts serves at least 1 point of node demand under the headline shock, or a compact names it a capacity-building host',
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

/** A network side's level: named at or above the floor, minor when selected below it, none otherwise. */
export function sideLevel(s: AibNetworkSide | null | undefined, floor = ROLE_FLOOR): RoleLevel {
  if (!s) return 'none';
  if (s.headline >= floor) return 'named';
  return s.selected > 0 || s.cumulative > 0 ? 'minor' : 'none';
}

const lift = (level: RoleLevel, byCompact: boolean): RoleLevel => (byCompact ? 'named' : level);

function rolesOf(iso3: string, name: string, net: AibNetworkCountry | null, cr: CountryRoleRow | null, largestOf: readonly string[]): CountryRoles {
  const r = cr?.roles ?? {};
  const resource = r['Resource supplier'] ?? [];
  const other = r['Supplier'] ?? [];
  const spec = r['Specialized supplier'] ?? [];
  const host = r['Capacity-building / investment host'] ?? [];
  const anchor = r['Demand anchor'] ?? [];
  const bridge = r['Bridge partner'] ?? [];
  const uniq = (xs: string[]) => [...new Set(xs)];
  const supplierLevel = net ? sideLevel(net.supplier) : other.length ? 'minor' : 'none';
  const cells: Record<MatrixRole, RoleCell> = {
    supplier: { level: lift(supplierLevel, resource.length > 0), side: net?.supplier ?? null, compacts: uniq([...resource, ...other]), nodes: [] },
    specialized: { level: spec.length || largestOf.length ? 'named' : 'none', side: null, compacts: spec, nodes: [...largestOf] },
    host: { level: lift(net ? sideLevel(net.host) : 'none', host.length > 0), side: net?.host ?? null, compacts: host, nodes: [] },
    investor: { level: net ? sideLevel(net.investor) : 'none', side: net?.investor ?? null, compacts: [], nodes: [] },
    buyer: { level: lift(net ? sideLevel(net.buyer) : 'none', anchor.length > 0), side: net?.buyer ?? null, compacts: anchor, nodes: [] },
    bridge: { level: bridge.length ? 'named' : 'none', side: null, compacts: bridge, nodes: [] },
  };
  return { iso3, name, cells, member: cr?.member ?? [], associate: cr?.associate ?? [], net };
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
 * participation, then by name (no ranking of roles).
 */
export function countryRoleMatrix(doc: Pick<AibDoc, 'compacts' | 'nodes' | 'network'>, names: Readonly<Record<string, string>>): CountryRoles[] {
  const cr = new Map(countryRoleRows(doc, names).map((r) => [r.iso3, r]));
  const net = new Map((doc.network?.countries ?? []).map((c) => [c.iso3, c]));
  const largest = largestByEconomy(doc);
  const isos = new Set([...cr.keys(), ...net.keys(), ...largest.keys()]);
  const rows = [...isos].map((iso) => rolesOf(iso, nameOf(names, iso), net.get(iso) ?? null, cr.get(iso) ?? null, largest.get(iso) ?? []));
  const part = (r: CountryRoles) => new Set([...r.member, ...r.associate]).size;
  return rows
    .filter((r) => part(r) > 0 || MATRIX_ROLES.some((k) => r.cells[k].level !== 'none'))
    .sort((a, b) => part(b) - part(a) || a.name.localeCompare(b.name, 'en'));
}

/** One economy's roles (every cell 'none' when it holds nothing). */
export function countryRolesOf(doc: Pick<AibDoc, 'compacts' | 'nodes' | 'network'>, names: Readonly<Record<string, string>>, iso3: string): CountryRoles {
  const iso = iso3.toUpperCase();
  const row = countryRoleMatrix(doc, names).find((r) => r.iso3 === iso);
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

/** A contribution (share of node demand summed over nodes) as points: "24.6 pts", "<0.1 pts", "0 pts". */
export function fmtPts(x: number | null | undefined): string {
  if (!isNum(x)) return DASH;
  if (x === 0) return '0 pts';
  const v = x * 100;
  if (v < 0.1) return '<0.1 pts';
  return `${v.toFixed(v >= 10 ? 0 : 1)} pts`;
}
