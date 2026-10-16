// The country x role matrix of a compact. The model assigns roles from its results (a country can hold
// several): demand anchors import most of the lost supply; specialized and resource suppliers carry a
// material share of the anchors' optimized supply with revealed specialization; capacity-building hosts
// lead a BUILD node's investment ranking; technology partners show capability in BUILD or MANAGE nodes;
// bridge partners supply one node and depend on another (or span stages, or sit in several compacts);
// associate partners join only for the highest target; any other member is a supplier. Pure.

import type { AibCompact } from '../contract.ts';
import { nameOf } from './format.ts';

/** The roles in the matrix's column order. */
export const ROLE_ORDER = [
  'Demand anchor',
  'Specialized supplier',
  'Resource supplier',
  'Capacity-building / investment host',
  'Technology partner',
  'Bridge partner',
  'Associate partner',
  'Supplier',
] as const;
export type RoleName = (typeof ROLE_ORDER)[number];

/** A model role as one of the matrix columns ("Supplier (coalition member)" -> "Supplier"); null when unknown. */
export function canonicalRole(role: string): RoleName | null {
  const r = role.trim().toLowerCase();
  const exact = ROLE_ORDER.find((x) => x.toLowerCase() === r);
  if (exact) return exact;
  // the model's long forms: "Supplier (coalition member)"
  return ROLE_ORDER.find((x) => r.startsWith(`${x.toLowerCase()} `) || r.startsWith(`${x.toLowerCase()}(`)) ?? null;
}

export interface RoleRow {
  iso3: string;
  name: string;
  /** in the minimum coalition (false: an associate only) */
  member: boolean;
  /** role -> the basis the model gives for it (absent when the country does not hold the role) */
  roles: Partial<Record<RoleName, string>>;
  /** roles the matrix has no column for (shown in the table twin) */
  other: { role: string; basis: string }[];
}

/**
 * One row per economy holding any role: demand anchors first, then the other members, then associates;
 * by name within each group.
 */
export function roleMatrix(c: AibCompact, names: Readonly<Record<string, string>>): RoleRow[] {
  const rows = new Map<string, RoleRow>();
  for (const r of c.roles) {
    const row = rows.get(r.iso3) ?? { iso3: r.iso3, name: nameOf(names, r.iso3), member: false, roles: {}, other: [] };
    row.member = row.member || r.member;
    const k = canonicalRole(r.role);
    if (k) row.roles[k] = row.roles[k] ? `${row.roles[k]}; ${r.basis}` : r.basis;
    else row.other.push({ role: r.role, basis: r.basis });
    rows.set(r.iso3, row);
  }
  const group = (r: RoleRow) => (r.roles['Demand anchor'] ? 0 : r.member ? 1 : 2);
  return [...rows.values()].sort((a, b) => group(a) - group(b) || a.name.localeCompare(b.name, 'en'));
}

/** The roles held by at least one economy of the matrix, in column order. */
export function rolesUsed(rows: readonly RoleRow[]): RoleName[] {
  return ROLE_ORDER.filter((r) => rows.some((x) => x.roles[r] !== undefined));
}
