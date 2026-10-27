// The node taxonomy of a bundle in words and structure (2026-10-19). Two sources of nodes:
//   dictionary  the electro-industrial HS6 dictionary (RMI Electro-Industrial Stack): index (A-F) > sector >
//               stage; node.techs holds the node's EIS sector ("Cells/packs", "Heat pumps/cooling"), never an
//               OSI technology; meta.indices lists the indices with their sectors; node.indexCode and
//               compact.indices name them. Partner indices (Export Opportunity, Friendshore, Development
//               Potential) are kept by OSI technology x stage, so a node reaches them only through the OSI
//               cells its codes sit in (node.yfCells, a trade-weighted crosswalk; [] where it has no basket
//               code).
//   master      the OSI basket (an older bundle, or meta.taxonomy "master"): node.techs are OSI technologies.
// The words ("sector" / "technology") and the index lookups every view uses, the sector choices grouped by
// index, and the OSI cell of a crosswalk id ("electric-vehicles-midstream"). Pure.

import { isShellStageSlug, isShellTechSlug } from '../../shell/sectorSlugs.ts';
import { indicesOf, isDictionary, type AibCompact, type AibIndex, type AibMeta, type AibNode } from '../contract.ts';

export interface Taxonomy {
  /** the nodes come from the electro-industrial dictionary */
  dictionary: boolean;
  /** the dictionary scope ("core" / "extended"); null for master */
  scope: string | null;
  indices: AibIndex[];
  /** what a node's techs are: "sector" (dictionary) or "technology" (master) */
  one: string;
  many: string;
  One: string;
  Many: string;
  /** the empty choice of a filter: "All sectors" / "All technologies" */
  all: string;
  /** the CSV column of a node's techs: "sectors" / "technologies" */
  csv: string;
}

const SECTOR_WORDS = { one: 'sector', many: 'sectors', One: 'Sector', Many: 'Sectors', all: 'All sectors', csv: 'sectors' };
const TECH_WORDS = { one: 'technology', many: 'technologies', One: 'Technology', Many: 'Technologies', all: 'All technologies', csv: 'technologies' };

/** The taxonomy of a bundle (an older bundle without the fields: master). */
export function taxonomyOf(m: AibMeta): Taxonomy {
  const dictionary = isDictionary(m);
  return {
    dictionary,
    scope: dictionary ? (m.dictionaryScope ?? null) : null,
    indices: indicesOf(m),
    ...(dictionary ? SECTOR_WORDS : TECH_WORDS),
  };
}

/** "B Electrotechnologies". */
export const indexLabel = (ix: AibIndex): string => `${ix.code} ${ix.name}`;

/** The index a sector belongs to (null for master, or a sector meta.indices does not list). */
export function indexOfSector(tx: Taxonomy, sector: string): AibIndex | null {
  return tx.indices.find((ix) => ix.sectors.includes(sector)) ?? null;
}

/** The index of a node: its indexCode, else the index of its first sector (null for master). */
export function indexOfNode(tx: Taxonomy, n: Pick<AibNode, 'indexCode' | 'techs'>): AibIndex | null {
  if (!tx.indices.length) return null;
  if (n.indexCode) {
    const ix = tx.indices.find((x) => x.code === n.indexCode);
    if (ix) return ix;
  }
  for (const s of n.techs) {
    const ix = indexOfSector(tx, s);
    if (ix) return ix;
  }
  return null;
}

/** The indices of a compact in code order: compact.indices, else those of its sectors ([] for master). */
export function indicesOfCompact(tx: Taxonomy, c: Pick<AibCompact, 'indices' | 'technologies'>): AibIndex[] {
  if (!tx.indices.length) return [];
  const codes = new Set(c.indices?.length ? c.indices : c.technologies.map((t) => indexOfSector(tx, t)?.code).filter((x): x is string => !!x));
  return tx.indices.filter((ix) => codes.has(ix.code)).sort((a, b) => a.code.localeCompare(b.code));
}

export interface SectorGroup {
  /** null: sectors no index lists (and every sector of a master bundle) */
  index: AibIndex | null;
  sectors: string[];
}

/**
 * Sectors grouped by their index, in index order (A..F), sectors alphabetical within a group; sectors no
 * index lists come last. A master bundle (no indices): one group of every technology, alphabetical.
 */
export function sectorGroups(tx: Taxonomy, sectors: readonly string[]): SectorGroup[] {
  const uniq = [...new Set(sectors)].sort((a, b) => a.localeCompare(b, 'en'));
  if (!tx.indices.length) return uniq.length ? [{ index: null, sectors: uniq }] : [];
  const out: SectorGroup[] = [];
  const placed = new Set<string>();
  for (const ix of [...tx.indices].sort((a, b) => a.code.localeCompare(b.code))) {
    const own = uniq.filter((s) => ix.sectors.includes(s) && !placed.has(s));
    own.forEach((s) => placed.add(s));
    if (own.length) out.push({ index: ix, sectors: own });
  }
  const rest = uniq.filter((s) => !placed.has(s));
  if (rest.length) out.push({ index: null, sectors: rest });
  return out;
}

/** The options of a sector (technology) filter: grouped by index in a dictionary bundle (the group is the index label). */
export function techOptions(tx: Taxonomy, techs: readonly string[]): { value: string; label: string; group?: string }[] {
  const groups = sectorGroups(tx, techs);
  if (!tx.indices.length) return groups.flatMap((g) => g.sectors.map((s) => ({ value: s, label: s })));
  return groups.flatMap((g) => g.sectors.map((s) => ({ value: s, label: s, group: g.index ? indexLabel(g.index) : 'Other sectors' })));
}

/** The sectors in words, grouped by index: "A Materials & Inputs: Graphite, Rare earths; B Electrotechnologies: Magnets". */
export function groupedSectorWords(tx: Taxonomy, sectors: readonly string[]): string {
  return sectorGroups(tx, sectors)
    .map((g) => (g.index ? `${indexLabel(g.index)}: ${g.sectors.join(', ')}` : g.sectors.join(', ')))
    .join('; ');
}

/**
 * Where a node sits, in one line: master "Batteries, Electric Vehicles · Midstream"; dictionary "B
 * Electrotechnologies › Cells/packs · Downstream" (index > sector > stage).
 */
export function nodeWhere(tx: Taxonomy, n: Pick<AibNode, 'indexCode' | 'techs' | 'stage'>): string {
  const techs = n.techs.join(', ') || '–';
  const ix = tx.dictionary ? indexOfNode(tx, n) : null;
  return `${ix ? `${indexLabel(ix)} › ` : ''}${techs} · ${n.stage}`;
}

/** The plain-text columns of a node's place for tables and CSVs: its sectors (technologies) and, in a dictionary bundle, its index. */
export function nodeIndexFields(tx: Taxonomy, n: Pick<AibNode, 'indexCode' | 'techs'>): { indexCode: string | null; indexName: string | null } {
  const ix = tx.dictionary ? indexOfNode(tx, n) : null;
  return { indexCode: ix?.code ?? (tx.dictionary ? (n.indexCode ?? null) : null), indexName: ix?.name ?? null };
}

// ---- The OSI crosswalk ---------------------------------------------------------------------------------

/** One OSI technology x stage cell ("electric-vehicles-midstream" = Electric Vehicles, Midstream). */
export interface OsiCell {
  slug: string;
  tech: string;
  stage: string;
}

const OSI_STAGES: Readonly<Record<string, string>> = { upstream: 'Upstream', midstream: 'Midstream', downstream: 'Downstream' };
const titleWords = (s: string) =>
  s
    .split('-')
    .filter(Boolean)
    .map((w) => w.charAt(0).toUpperCase() + w.slice(1))
    .join(' ');

/**
 * The OSI cell of a crosswalk id (node.yfCells: "<tech slug>-<stage slug>"; the OSI technology names are
 * the title-cased slugs: "electric-grid" -> Electric Grid). Null when the id does not end in a stage.
 */
export function osiCellOf(slug: string): OsiCell | null {
  const m = /^([a-z0-9-]+)-(upstream|midstream|downstream)$/.exec(slug);
  if (!m) return null;
  return { slug, tech: titleWords(m[1]), stage: OSI_STAGES[m[2]] };
}

/** The OSI cells a node's codes sit in, in the exported order (the largest share of its trade first). */
export const osiCellsOfNode = (n: Pick<AibNode, 'yfCells'>): OsiCell[] => n.yfCells.map(osiCellOf).filter((c): c is OsiCell => c !== null);

/**
 * The Sector Profile of an OSI cell (/sector/<tech slug>/<stage slug>), or null when its technology is not
 * one the Sector module profiles.
 */
export function osiCellHref(c: OsiCell): string | null {
  const stage = c.stage.toLowerCase();
  const tech = c.slug.slice(0, c.slug.length - stage.length - 1);
  return isShellTechSlug(tech) && isShellStageSlug(stage) ? `/sector/${tech}/${stage}` : null;
}

/** The OSI cells of a node that have a Sector Profile, with its href (the largest share of its trade first). */
export const profiledCellsOfNode = (n: Pick<AibNode, 'yfCells'>): (OsiCell & { href: string })[] =>
  osiCellsOfNode(n)
    .map((c) => ({ ...c, href: osiCellHref(c) }))
    .filter((c): c is OsiCell & { href: string } => c.href !== null);
