// The Country profile's framing (IA refactor 2026-10-16, brief s.11-12): "Make bets", "Avoid dangerous
// dependencies", "Preserve capabilities", the sector-position rows and the economy's place in the
// Allied Industrial Base. No new score: every list is a filter and an ordering of existing outputs (the
// four corners of the fact sheet, the Strategic Index rank, the fact sheet's trade suppliers, the AIB
// compacts' roles, trade relationships and host nodes). Pure.
//
// Facts vs preferences (plan.md 4): the corners, Energy Security, Economic Opportunity, suppliers and the
// whole AIB document are observed / modelled facts. Only the Strategic Index rank is preference-adjusted;
// it comes from `SiMap` (the engine's run for the reader's Perspective) when one is given, else from the
// fact sheet (the NEIS baseline: the same numbers).

import type { AibCompact, AibDoc, AibNode } from '../../aib/contract.ts';
import { countryRoleRow, type CountryRoleRow } from '../../aib/derive/countryRoles.ts';
import { ROLE_ORDER, type RoleName } from '../../aib/derive/roles.ts';
import { cap } from './text.ts';
import { cellKey, cellOf, COUNTRY_STAGES, COUNTRY_TECHS, isCellKey, type CellFacts, type FactSheet, type TradeParty } from '../contract.ts';

// ---- the preference-adjusted Strategic Index ---------------------------------------------------------

/** One sector's Strategic Index under the reader's Perspective (an engine row). */
export interface SiOverride {
  rank: number | null;
  /** rank change against the baseline (positive = moved up), null when not comparable */
  delta: number | null;
  si: number | null;
  inSample: boolean;
  /** in the index now but not at the baseline */
  isNew: boolean;
}
export type SiMap = ReadonlyMap<string, SiOverride>;

const TECH_SLUG = new Map<string, string>(COUNTRY_TECHS.map((t) => [t.label.toLowerCase(), t.slug]));
const STAGE_SLUG = new Map<string, string>(COUNTRY_STAGES.map((s) => [s.label.toLowerCase(), s.slug]));

/** The cell key of an engine row (tech "Electric Grid", supply_chain "Midstream"), or null. */
export function engineCellKey(tech: unknown, stage: unknown): string | null {
  if (typeof tech !== 'string' || typeof stage !== 'string') return null;
  const t = TECH_SLUG.get(tech.trim().toLowerCase());
  const s = STAGE_SLUG.get(stage.trim().toLowerCase());
  if (!t || !s) return null;
  const k = cellKey(t, s);
  return isCellKey(k) ? k : null;
}

type EngineRow = Record<string, string | number | boolean | null>;
const num = (v: unknown): number | null => (typeof v === 'number' && Number.isFinite(v) ? v : null);

/** The economy's rows of the engine's compare.si table as a map by cell key. */
export function siMapOf(rows: readonly EngineRow[], iso3: string): Map<string, SiOverride> {
  const out = new Map<string, SiOverride>();
  for (const r of rows) {
    if (r.iso3c !== iso3) continue;
    const k = engineCellKey(r.tech, r.supply_chain);
    if (!k) continue;
    const inSample = r.in_sample === true && num(r.rank_in_country) !== null;
    out.set(k, {
      rank: inSample ? num(r.rank_in_country) : null,
      delta: inSample ? num(r.rank_delta) : null,
      si: num(r.strategic_index),
      inSample,
      isNew: inSample && r.in_sample_base !== true,
    });
  }
  return out;
}

/** A sector's Strategic Index rank: the Perspective's when a map is given, else the fact sheet's. */
export function siRankOf(c: CellFacts, over: SiMap | null): number | null {
  if (over) {
    const o = over.get(c.key);
    return o && o.inSample ? o.rank : null;
  }
  return c.si?.in_sample ? c.si.rank.v : null;
}

/** In-index sectors by Strategic Index rank (the Perspective's when a map is given). */
export function siOrder(sheet: FactSheet, over: SiMap | null): CellFacts[] {
  return sheet.cells
    .filter((c) => siRankOf(c, over) !== null)
    .sort((a, b) => (siRankOf(a, over) as number) - (siRankOf(b, over) as number));
}

const byRank = (over: SiMap | null) => (a: CellFacts, b: CellFacts) => {
  const ra = siRankOf(a, over);
  const rb = siRankOf(b, over);
  if (ra !== null && rb !== null) return ra - rb;
  if (ra !== null) return -1;
  if (rb !== null) return 1;
  return (b.eo?.pct.v ?? -1) - (a.eo?.pct.v ?? -1);
};

// ---- Opportunities and risks -------------------------------------------------------------------------

/**
 * Make bets: placed sectors in the Grow or Upgrade corner (Economic Opportunity at or above the
 * economy's own median sector), by Strategic Index rank (preference-adjusted); sectors outside the index
 * after them, highest Economic Opportunity first.
 */
export function makeBets(sheet: FactSheet, over: SiMap | null, n = 5): CellFacts[] {
  return sheet.cells.filter((c) => c.placed && (c.quadrant === 'grow' || c.quadrant === 'upgrade')).sort(byRank(over)).slice(0, n);
}

export interface Dependency {
  cell: CellFacts;
  /** the largest supplier of the economy's imports in the sector (fact sheet trade), when known */
  supplier: TradeParty | null;
  /** compacts covering the sector in which the economy is a demand anchor */
  compacts: { id: string; name: string }[];
}

/** The compacts (id, name) that cover a cell: a node of the compact maps to the cell (nodes[].yfCells). */
export function compactsOfCell(doc: Pick<AibDoc, 'compacts' | 'nodes'>, key: string): AibCompact[] {
  const nodeIds = new Set(doc.nodes.filter((nd) => nd.yfCells.includes(key)).map((nd) => nd.id));
  return doc.compacts.filter((c) => c.nodes.some((id) => nodeIds.has(id)));
}

/**
 * Avoid dangerous dependencies: placed sectors in the Upgrade or Secure corner (Energy Security below
 * the economy's own median sector), lowest Energy Security first (the fact sheet's es_order), each with
 * its largest supplier and the compacts covering it in which the economy is a demand anchor.
 */
export function dependencies(sheet: FactSheet, doc: Pick<AibDoc, 'compacts' | 'nodes'> | null, n = 5): Dependency[] {
  const low = new Set(sheet.cells.filter((c) => c.placed && (c.quadrant === 'upgrade' || c.quadrant === 'secure')).map((c) => c.key));
  return sheet.summary.es_order
    .filter((k) => low.has(k))
    .map((k) => cellOf(sheet, k))
    .filter((c): c is CellFacts => !!c)
    .slice(0, n)
    .map((c) => ({
      cell: c,
      supplier: c.trade?.sources?.[0] ?? null,
      compacts: doc
        ? compactsOfCell(doc, c.key)
            .filter((cp) => cp.roles.some((r) => r.iso3 === sheet.iso3 && r.role.toLowerCase().startsWith('demand anchor')))
            .map((cp) => ({ id: cp.id, name: cp.name }))
        : [],
    }));
}

/** The AIB roles that mark a capability worth keeping. */
export const CAPABILITY_ROLES: readonly RoleName[] = ['Specialized supplier', 'Resource supplier', 'Technology partner'];

/**
 * Preserve capabilities: placed sectors in the Grow corner (both scores at or above the economy's own
 * median sector), highest Economic Opportunity first, and the economy's supplier and technology roles
 * in the compacts.
 */
export function capabilities(sheet: FactSheet, roles: CountryRoleRow | null, n = 6): { cells: CellFacts[]; roles: { role: RoleName; compacts: string[] }[] } {
  const grow = new Set(sheet.cells.filter((c) => c.placed && c.quadrant === 'grow').map((c) => c.key));
  const cells = sheet.summary.eo_order
    .filter((k) => grow.has(k))
    .map((k) => cellOf(sheet, k))
    .filter((c): c is CellFacts => !!c)
    .slice(0, n);
  const held = CAPABILITY_ROLES.map((role) => ({ role, compacts: roles?.roles[role] ?? [] })).filter((r) => r.compacts.length > 0);
  return { cells, roles: held };
}

// ---- Role in the Allied Industrial Base ----------------------------------------------------------

export interface AibPair {
  partner: string;
  /** USD the economy supplies to the partner / receives from it in the compacts' trade relationships */
  outUsd: number;
  inUsd: number;
  usd: number;
  /** relationships (edges) and how many of them are new */
  n: number;
  nNew: number;
  /** node ids, by USD */
  nodes: string[];
  /** compacts the relationships belong to */
  compacts: string[];
}

export interface AibCountry {
  row: CountryRoleRow | null;
  /** the roles held, in the matrix's column order, with their compacts */
  roles: { role: RoleName; compacts: AibCompact[] }[];
  /** compacts the economy sits in (member, associate or a role) */
  compacts: { compact: AibCompact; member: boolean; associate: boolean; roles: RoleName[] }[];
  /** bilateral trade relationships touching the economy, aggregated by partner, largest first */
  pairs: AibPair[];
  trade: { n: number; usd: number; nNew: number };
  /** investment relationships (compacts[].investment) touching the economy */
  investment: { n: number; usd: number };
  /** nodes where the economy is a listed capacity-building host, largest marginal gain first */
  hostNodes: AibNode[];
  /** the AIB nodes behind the economy's roles: nodes of its compacts where it trades, by USD */
  keyNodes: { node: AibNode; usd: number }[];
}

/** The economy's place in the AIB document (null fields when it does not appear). Pure. */
export function aibCountry(doc: AibDoc, names: Readonly<Record<string, string>>, iso3: string): AibCountry {
  const iso = iso3.toUpperCase();
  const row = countryRoleRow(doc, names, iso);
  const byId = new Map(doc.compacts.map((c) => [c.id, c]));
  const nodeById = new Map(doc.nodes.map((nd) => [nd.id, nd]));
  const roles = ROLE_ORDER.map((role) => ({ role, compacts: (row?.roles[role] ?? []).map((id) => byId.get(id)).filter((c): c is AibCompact => !!c) })).filter(
    (r) => r.compacts.length > 0,
  );
  const ids = new Set([...(row?.member ?? []), ...(row?.associate ?? []), ...roles.flatMap((r) => r.compacts.map((c) => c.id))]);
  const compacts = doc.compacts
    .filter((c) => ids.has(c.id))
    .map((c) => ({
      compact: c,
      member: row?.member.includes(c.id) ?? false,
      associate: row?.associate.includes(c.id) ?? false,
      roles: roles.filter((r) => r.compacts.some((x) => x.id === c.id)).map((r) => r.role),
    }));
  const pairs = new Map<string, AibPair & { nodeUsd: Map<string, number> }>();
  const nodeUsd = new Map<string, number>();
  let tradeN = 0;
  let tradeUsd = 0;
  let tradeNew = 0;
  for (const c of doc.compacts) {
    for (const e of c.trade) {
      if (e.from !== iso && e.to !== iso) continue;
      const partner = e.from === iso ? e.to : e.from;
      const usd = e.usd ?? 0;
      let p = pairs.get(partner);
      if (!p) {
        p = { partner, outUsd: 0, inUsd: 0, usd: 0, n: 0, nNew: 0, nodes: [], compacts: [], nodeUsd: new Map() };
        pairs.set(partner, p);
      }
      if (e.from === iso) p.outUsd += usd;
      else p.inUsd += usd;
      p.usd += usd;
      p.n += 1;
      if (e.relationship === 'new') {
        p.nNew += 1;
        tradeNew += 1;
      }
      p.nodeUsd.set(e.node, (p.nodeUsd.get(e.node) ?? 0) + usd);
      if (!p.compacts.includes(c.id)) p.compacts.push(c.id);
      nodeUsd.set(e.node, (nodeUsd.get(e.node) ?? 0) + usd);
      tradeN += 1;
      tradeUsd += usd;
    }
  }
  const pairList = [...pairs.values()]
    .map(({ nodeUsd: nu, ...p }) => ({ ...p, nodes: [...nu.entries()].sort((a, b) => b[1] - a[1]).map(([k]) => k) }))
    .sort((a, b) => b.usd - a.usd || a.partner.localeCompare(b.partner));
  let invN = 0;
  let invUsd = 0;
  for (const c of doc.compacts) {
    for (const e of c.investment) {
      if (e.investor !== iso && e.host !== iso) continue;
      invN += 1;
      invUsd += e.usd ?? 0;
    }
  }
  const hostNodes = doc.nodes.filter((nd) => nd.hosts.includes(iso)).sort((a, b) => (b.bestGain ?? -1) - (a.bestGain ?? -1));
  const keyNodes = [...nodeUsd.entries()]
    .map(([id, usd]) => ({ node: nodeById.get(id), usd }))
    .filter((x): x is { node: AibNode; usd: number } => !!x.node)
    .sort((a, b) => b.usd - a.usd);
  return { row, roles, compacts, pairs: pairList, trade: { n: tradeN, usd: tradeUsd, nNew: tradeNew }, investment: { n: invN, usd: invUsd }, hostNodes, keyNodes };
}

/** BUILD nodes of the compacts the economy sits in, with their listed hosts (development candidates). */
export function buildNodesOf(doc: AibDoc, compactIds: readonly string[]): AibNode[] {
  const ids = new Set(compactIds);
  return doc.nodes.filter((nd) => nd.cls === 'BUILD' && nd.compact !== null && ids.has(nd.compact)).sort((a, b) => (b.residual ?? -1) - (a.residual ?? -1));
}

// ---- Policy and climate ----------------------------------------------------------------------------

/**
 * How the economy behaves on policy and climate, in plain sentences from existing facts: its Climate
 * Action Tracker rating, how its policy activity lines up with the data's priority (the agreement
 * classes of the fact sheet) and its industrial-policy measures in force (Global Trade Alert). No score.
 */
export function policyBehaviour(sheet: FactSheet, name: string): string[] {
  const out: string[] = [];
  const cat = sheet.policy.cat;
  out.push(
    cat
      ? `The Climate Action Tracker rates ${name}${cat.scope === 'eu_bloc' ? " (through the EU's rating)" : ''} "${cat.overall}"${cat.date_label ? ` (as of ${cat.date_label})` : ''}.`
      : `${cap(name)} has no Climate Action Tracker rating, so its policy activity rests on International Energy Agency policy records only.`,
  );
  const ag = sheet.policy.agreement;
  if (ag && ag.n > 0) {
    const n = (k: keyof typeof ag.by_class) => ag.by_class[k]?.length ?? 0;
    const aligned = n('aligned_high') + n('aligned_low');
    out.push(
      `Across ${ag.n} ranked sectors, its policy activity matches the data's priority in ${aligned}, runs ahead of it in ${n('policy_ahead')} (much policy, low priority) and trails it in ${n('gap')} (high priority, little policy).`,
    );
  }
  const nipo = sheet.policy.nipo;
  if (nipo && nipo.covered && nipo.n_active.v !== null) {
    out.push(`Global Trade Alert records ${nipo.n_active.d} industrial-policy measures in force for ${name} (context, not part of the Policy index).`);
  }
  return out;
}
