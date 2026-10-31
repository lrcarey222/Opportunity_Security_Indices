// The node taxonomy of a bundle in words and structure. Pure.
//
// Since 2026-10-29 (design A1/A2 of the six-stage migration) every view presents a node by OSI technology
// x six-stage stage, read from the node's own six-stage cells (node.yfCells, "batteries-refining": the OSI
// basket cells its HS6 codes sit in, a trade-weighted crosswalk; [] where the node has no basket code):
//   technologies  the distinct technologies of its cells, as equals (no "primary" technology: for most
//                 nodes in several technologies every basket code is filed under each of them, so the
//                 export's order is a tie-break, not a signal); a node with no cell is 'Not in the OSI
//                 basket' (NOT_IN_BASKET), one group and one filter option;
//   stage         the stages of its cells (2026-10-29, cell placement): a node is placed, grouped and
//                 filtered by its cells, never by node.stage. A node matches a Technology T / Stage S
//                 filter when one of its cells is T x S (nodeInCell(): only T, any cell of T; only S, any
//                 cell at S); a node with no cell is matched only by "All stages". node.stage, the AIB
//                 model's own six-stage stage (the stage holding most of its trade), can differ from the
//                 stages of its cells (electric motors: Assembly in the model, codes in Electric Vehicles
//                 – Components; overhead conductors: Components, codes in Assembly cells): it is shown
//                 only as the model's classification, labelled as such (the CSVs' aib_model_stage).
// Where the nodes come from stays a provenance fact: meta.taxonomy "dictionary" (the electro-industrial HS6
// dictionary; node.techs then holds the node's dictionary sector, node.indexCode its index) or "master"
// (the OSI basket of an older bundle, node.techs OSI technologies). The views no longer group, filter or
// title by the dictionary's sectors and indices; the CSVs keep one dictionary_sector column (osiFields())
// and About one sentence. The model concepts the dictionary sector still defines (the diversification
// benchmark's fallback, the largest-supplier shocks of the sector and stage, the strategic scope) are
// relabelled in words in settings.ts.
// The joins with the other bundles (the Sector profiles, the partner rankings and factory projects of the
// Strategic data, the aid of the Sector data) read the same six-stage cells: those bundles are six-stage
// since 2026-10-28. The node's three-stage cells (node.yfCellsLegacy) are no longer read.

import { cellLabel } from '../../lib/format.ts';
import { parseCellSlug, stageRank, stagesInChainOrder, stageSlug, SUPPLY_STAGES, type SupplyStage } from '../../lib/stages.ts';
import { isShellStageSlug, isShellTechSlug } from '../../shell/sectorSlugs.ts';
import { isDictionary, type AibMeta, type AibNode } from '../contract.ts';

export interface Taxonomy {
  /** the nodes come from the electro-industrial dictionary (provenance only: About, the run line, the CSV's dictionary_sector) */
  dictionary: boolean;
  /** the dictionary scope ("core" / "extended"); null for master */
  scope: string | null;
  /** what the views group and filter nodes by: OSI technologies, in every bundle */
  one: string;
  many: string;
  One: string;
  Many: string;
  /** the empty choice of a filter: "All technologies" */
  all: string;
  /** the CSV column of a node's technologies: "technologies" */
  csv: string;
}

const TECH_WORDS = { one: 'technology', many: 'technologies', One: 'Technology', Many: 'Technologies', all: 'All technologies', csv: 'technologies' };

/** The taxonomy of a bundle (an older bundle without the fields: master). The words are the OSI technology words in every bundle. */
export function taxonomyOf(m: AibMeta): Taxonomy {
  const dictionary = isDictionary(m);
  return { dictionary, scope: dictionary ? (m.dictionaryScope ?? null) : null, ...TECH_WORDS };
}

// ---- OSI technologies of a node ---------------------------------------------------------------------------

/** The group, filter option and table value of a node with no OSI basket cell (none of its codes is in the OSI basket). */
export const NOT_IN_BASKET = 'Not among the traded goods this Explorer covers';

const titleWords = (s: string) =>
  s
    .split('-')
    .filter(Boolean)
    .map((w) => w.charAt(0).toUpperCase() + w.slice(1))
    .join(' ');

/** The OSI technology name of a technology slug: the title-cased slug ("electric-grid" -> "Electric Grid"). */
export const osiTechName = (slug: string): string => titleWords(slug);

const byName = (a: string, b: string) => a.localeCompare(b, 'en');

/**
 * The OSI technologies of a node: the distinct technologies of its six-stage cells, alphabetical, as
 * equals; [] for a node with no basket code.
 */
export function osiTechsOfNode(n: Pick<AibNode, 'yfCells'>): string[] {
  return [...new Set(osiCellsOfNode(n).map((c) => c.tech))].sort(byName);
}

/** The technology groups a node belongs to: its OSI technologies, or [NOT_IN_BASKET]. */
export function techKeysOfNode(n: Pick<AibNode, 'yfCells'>): string[] {
  const t = osiTechsOfNode(n);
  return t.length ? t : [NOT_IN_BASKET];
}

/** True when the node belongs to the technology (an OSI technology name, or NOT_IN_BASKET). */
export const nodeInTech = (n: Pick<AibNode, 'yfCells'>, tech: string): boolean => techKeysOfNode(n).includes(tech);

/** A node's technologies in words: "Batteries, Electric Vehicles", or "Not in the OSI basket". */
export const nodeTechWords = (n: Pick<AibNode, 'yfCells'>): string => techKeysOfNode(n).join(', ');

/** OSI technologies alphabetical, NOT_IN_BASKET last. */
export const byTechName = (a: string, b: string): number => Number(a === NOT_IN_BASKET) - Number(b === NOT_IN_BASKET) || byName(a, b);

/** The technologies of a set of nodes: the OSI technologies of their cells, alphabetical, then NOT_IN_BASKET when a node has no cell. */
export function technologiesOfNodes(nodes: readonly Pick<AibNode, 'yfCells'>[]): string[] {
  return [...new Set(nodes.flatMap(techKeysOfNode))].sort(byTechName);
}

/** The options of a Technology filter: the technologies as given, alphabetical, NOT_IN_BASKET last. */
export function techOptions(techs: readonly string[]): { value: string; label: string }[] {
  return [...new Set(techs)].sort(byTechName).map((t) => ({ value: t, label: t }));
}

/**
 * A Technology / Stage filter in words for captions and file names: " in Batteries", " at Refining", " in
 * Batteries at Refining", " not in the OSI basket"; "" with neither.
 */
export function techStageWords(tech: string | null | undefined, stage: string | null | undefined): string {
  const t = !tech ? '' : tech === NOT_IN_BASKET ? ' not among the traded goods this Explorer covers' : ` in ${tech}`;
  return `${t}${stage ? ` at ${stage}` : ''}`;
}

/**
 * The options of a Stage filter: the stages given (cellStagesOfNodes(): those that hold the nodes' cells),
 * in chain order (no AIB node has a Deployment cell, so it is not offered: design A3).
 */
export function stageOptions(stages: Iterable<string>): { value: string; label: string }[] {
  return stagesInChainOrder(stages).map((s) => ({ value: s, label: s }));
}

// ---- Placing and filtering nodes by their cells (2026-10-29) -------------------------------------------
// One rule for every view: a node is where its six-stage cells are. node.stage (the AIB model's own stage)
// is never used to place, group or filter a node.

/**
 * A node's OSI cells within a Technology / Stage filter (either empty: no constraint), each once, in the
 * exported order (the largest share of its trade first); [] for 'Not in the OSI basket', which has no cell.
 */
export function cellsInFilter(n: Pick<AibNode, 'yfCells'>, tech?: string | null, stage?: string | null): OsiCell[] {
  if (tech === NOT_IN_BASKET) return [];
  const out: OsiCell[] = [];
  const seen = new Set<string>();
  for (const c of osiCellsOfNode(n)) {
    if ((tech && c.tech !== tech) || (stage && c.stage !== stage) || seen.has(c.slug)) continue;
    seen.add(c.slug);
    out.push(c);
  }
  return out;
}

/**
 * True when a node matches a Technology / Stage filter, read from its cells (never node.stage): with both,
 * one of its cells is that technology at that stage; with a technology only, any cell of it; with a stage
 * only, any cell at it; with neither, every node. 'Not in the OSI basket' (NOT_IN_BASKET) matches the nodes
 * with no cell, and only under "All stages": they sit at no stage of the basket. Electric motors (Assembly
 * in the model, codes in Electric Vehicles – Components) match Electric Vehicles x Components, not x
 * Assembly.
 */
export function nodeInCell(n: Pick<AibNode, 'yfCells'>, tech?: string | null, stage?: string | null): boolean {
  if (tech === NOT_IN_BASKET) return !stage && osiCellsOfNode(n).length === 0;
  if (!tech && !stage) return true;
  return cellsInFilter(n, tech, stage).length > 0;
}

/** The stages of a node's cells (of `tech` when given), in chain order; [] for a node with no cell. */
export function nodeStagesOf(n: Pick<AibNode, 'yfCells'>, tech?: string | null): SupplyStage[] {
  return stagesInChainOrder(cellsInFilter(n, tech).map((c) => c.stage)) as SupplyStage[];
}

/**
 * The stages that hold the nodes' cells (of `tech` when given), in chain order: the choices of a Stage
 * filter, of the chosen technology's cells once one is chosen (none for 'Not in the OSI basket').
 */
export function cellStagesOfNodes(nodes: readonly Pick<AibNode, 'yfCells'>[], tech?: string | null): SupplyStage[] {
  return stagesInChainOrder(nodes.flatMap((n) => cellsInFilter(n, tech).map((c) => c.stage))) as SupplyStage[];
}

/**
 * The technologies of the nodes' cells at `stage`, alphabetical: the choices of a Technology filter once a
 * stage is chosen; with no stage, technologiesOfNodes() (NOT_IN_BASKET last when a node has no cell).
 */
export function cellTechsOfNodes(nodes: readonly Pick<AibNode, 'yfCells'>[], stage?: string | null): string[] {
  if (!stage) return technologiesOfNodes(nodes);
  return [...new Set(nodes.flatMap((n) => cellsInFilter(n, null, stage).map((c) => c.tech)))].sort(byName);
}

/** A node's stages in words for a table: the stages of its cells, "Components, Assembly"; null for a node with no cell. */
export const nodeStageWords = (n: Pick<AibNode, 'yfCells'>): string | null => nodeStagesOf(n).join(', ') || null;

/** A node's place in the chain for sorting by stage: its earliest cell's stage; a node with no cell after every stage. */
export function nodeStageRank(n: Pick<AibNode, 'yfCells'>): number {
  const s = nodeStagesOf(n);
  return s.length ? stageRank(s[0]) : SUPPLY_STAGES.length;
}

/**
 * Where a node sits, in one line: its cells by stage in chain order, the technologies at a stage
 * alphabetical: "Electric Vehicles, Wind · Components", "Electric Vehicles · Components; Batteries ·
 * Assembly"; "Not in the OSI basket" for a node with no basket code. Never node.stage.
 */
export function nodeWhere(n: Pick<AibNode, 'yfCells'>): string {
  const cells = cellsInFilter(n);
  if (!cells.length) return NOT_IN_BASKET;
  return nodeStagesOf(n)
    .map((s) => `${[...new Set(cells.filter((c) => c.stage === s).map((c) => c.tech))].sort(byName).join(', ')} · ${s}`)
    .join('; ');
}

/**
 * The plain-text columns of a node's place for tables and CSVs: its OSI technologies ("; "-joined, or
 * NOT_IN_BASKET), its OSI cells (the crosswalk ids, "; "-joined; "" when none), the stages of its cells
 * ("; "-joined in chain order; "" when none), the AIB model's own stage for it (node.stage, labelled as the
 * model's: the CSVs' aib_model_stage) and, in a dictionary bundle, its dictionary sector (provenance; null
 * for master).
 */
export function osiFields(
  tx: Pick<Taxonomy, 'dictionary'>,
  n: Pick<AibNode, 'yfCells' | 'techs' | 'stage'>,
): { technologies: string; osiCells: string; stages: string; modelStage: string; dictionarySector: string | null } {
  return {
    technologies: techKeysOfNode(n).join('; '),
    osiCells: osiCellsOfNode(n).map((c) => c.slug).join('; '),
    stages: nodeStagesOf(n).join('; '),
    modelStage: n.stage,
    dictionarySector: tx.dictionary ? n.techs.join('; ') : null,
  };
}

// ---- The OSI crosswalk ---------------------------------------------------------------------------------

/** One OSI technology x six-stage cell ("electric-vehicles-components" = Electric Vehicles, Components). */
export interface OsiCell {
  slug: string;
  tech: string;
  stage: SupplyStage;
}

/**
 * The OSI cell of a six-stage crosswalk id ("<tech slug>-<stage slug>", as node.yfCells; the OSI technology
 * names are the title-cased slugs: "electric-grid" -> Electric Grid). Null when the id does not end in one
 * of the six stages (a three-stage cell, "batteries-midstream", is no cell of the six stages).
 */
export function osiCellOf(slug: string): OsiCell | null {
  const c = parseCellSlug(slug);
  return c && !c.legacy ? { slug, tech: osiTechName(c.techSlug), stage: c.stage as SupplyStage } : null;
}

/** The OSI cells a node's codes sit in, six-stage, in the exported order (the largest share of its trade first; a tie for most multi-technology nodes). */
export const osiCellsOfNode = (n: Pick<AibNode, 'yfCells'>): OsiCell[] => n.yfCells.map(osiCellOf).filter((c): c is OsiCell => c !== null);

/**
 * The OSI sub-components of one of a node's cells (node.yfSubcomponents "batteries-refining::Cathode
 * Materials & Battery Precursors": the master's sub-sectors its codes sit in), the names only, in the
 * exported order; [] when the export names none.
 */
export function subcomponentsOfCell(n: Pick<AibNode, 'yfSubcomponents'>, slug: string): string[] {
  const pre = `${slug}::`;
  return (n.yfSubcomponents ?? []).filter((x) => x.startsWith(pre)).map((x) => x.slice(pre.length));
}

/** A node's OSI cells in words for a table: "Batteries – Refining; Electric Vehicles – Refining"; null when it has none. */
export const nodeCellWords = (n: Pick<AibNode, 'yfCells'>): string | null => osiCellsOfNode(n).map((c) => cellLabel(c.tech, c.stage)).join('; ') || null;

/** The Sector Profile of an OSI cell (/sector/<tech slug>/<stage slug>), or null when its technology is not one the Sector module profiles. */
export function osiCellHref(c: Pick<OsiCell, 'slug'>): string | null {
  const p = osiCellOf(c.slug);
  if (!p) return null;
  const tech = parseCellSlug(c.slug)!.techSlug;
  const stage = stageSlug(p.stage);
  return isShellTechSlug(tech) && isShellStageSlug(stage) ? `/sector/${tech}/${stage}` : null;
}

/** A Sector Profile a node links to: its cell, the href and the label ("Batteries – Refining"). */
export interface ProfiledCell extends OsiCell {
  href: string;
  label: string;
}

/** The Sector Profiles of a node: those of its six-stage cells the Sector module profiles, the largest share of its trade first. */
export function profiledCellsOfNode(n: Pick<AibNode, 'yfCells'>): ProfiledCell[] {
  const out: ProfiledCell[] = [];
  const seen = new Set<string>();
  for (const c of osiCellsOfNode(n)) {
    const href = osiCellHref(c);
    if (!href || seen.has(href)) continue;
    seen.add(href);
    out.push({ ...c, href, label: cellLabel(c.tech, c.stage) });
  }
  return out;
}
