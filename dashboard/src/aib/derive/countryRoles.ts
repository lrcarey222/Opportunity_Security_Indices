// Country roles across every compact (IA refactor, 2026-10-16): the model's own per-compact roles
// (compacts[].roles; see roles.ts) gathered by economy, for the Allied Industrial Base "Country roles"
// view and a Country profile's "Role in the Allied Industrial Base". No new score: a cell is the list
// of compacts in which the economy holds that role, and membership is the compact's minimum coalition
// (coalitionMembersOf: the exact minimum, or the widest greedy set when the middle target is out of
// reach) or its associates. Pure.

import { coalitionMembersOf, type AibDoc } from '../contract.ts';
import { nameOf } from './format.ts';
import { canonicalRole, ROLE_ORDER, type RoleName } from './roles.ts';

export interface CountryRoleRow {
  iso3: string;
  name: string;
  /** role -> ids of the compacts where the economy holds it, in document order */
  roles: Partial<Record<RoleName, string[]>>;
  /** compacts whose minimum coalition includes the economy */
  member: string[];
  /** compacts the economy joins only as an associate (for the highest target) */
  associate: string[];
}

/** One row per economy that holds a role or a seat in any compact: most compacts first, then by name. */
export function countryRoleRows(doc: Pick<AibDoc, 'compacts'>, names: Readonly<Record<string, string>>): CountryRoleRow[] {
  const rows = new Map<string, CountryRoleRow>();
  const row = (iso3: string): CountryRoleRow => {
    let r = rows.get(iso3);
    if (!r) {
      r = { iso3, name: nameOf(names, iso3), roles: {}, member: [], associate: [] };
      rows.set(iso3, r);
    }
    return r;
  };
  for (const c of doc.compacts) {
    for (const iso of coalitionMembersOf(c)) if (!row(iso).member.includes(c.id)) row(iso).member.push(c.id);
    for (const iso of c.associates) if (!row(iso).associate.includes(c.id)) row(iso).associate.push(c.id);
    for (const r of c.roles) {
      const k = canonicalRole(r.role);
      if (!k) continue;
      const list = (row(r.iso3).roles[k] ??= []);
      if (!list.includes(c.id)) list.push(c.id);
    }
  }
  const n = (r: CountryRoleRow) => new Set([...r.member, ...r.associate]).size;
  return [...rows.values()].sort((a, b) => n(b) - n(a) || a.name.localeCompare(b.name, 'en'));
}

/** One economy's row, or null when it holds no role and no seat. */
export function countryRoleRow(doc: Pick<AibDoc, 'compacts'>, names: Readonly<Record<string, string>>, iso3: string): CountryRoleRow | null {
  const iso = iso3.toUpperCase();
  return countryRoleRows(doc, names).find((r) => r.iso3 === iso) ?? null;
}

/** The roles held by at least one row, in the matrix's column order. */
export function countryRolesUsed(rows: readonly CountryRoleRow[]): RoleName[] {
  return ROLE_ORDER.filter((r) => rows.some((x) => (x.roles[r]?.length ?? 0) > 0));
}
