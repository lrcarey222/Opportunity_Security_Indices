// The supply-chain stages every module shares (2026-10-28). Pure: no IO, no React.
//
// Since 2026-10-26 the OSI scores a technology at six stages, in chain order: Extraction, Refining,
// Components, Assembly, Deployment, Recovery (R/utils/supply_chain_stages.R SUPPLY_CHAIN_STAGES;
// config/supply_chain_stages.yml; docs/supply_chain_stages.md). The paper's three stages survive as
// roll-ups: Upstream = Extraction, Refining and Recovery (materials, refining included); Midstream =
// Components and Assembly (manufacturing); Downstream = Deployment. STAGE_ROLLUP is an exact mirror of
// SUPPLY_CHAIN_STAGE_ROLLUP, and src/lib/stages.test.ts reads the R file to check that it stays one.
//
// Cells are "<tech slug>-<stage slug>" ("batteries-refining"; the legacy form "batteries-midstream"),
// sub-components "<cell>::<sub-component name>" ("batteries-refining::Anode Materials"). The slugs are
// those of R aib_slug() (R/aib/aib_nodes.R): lower case, "&" as "and", every other run of characters
// that are not letters or digits as "-". The bundles written before the six stages (the Country, Sector
// and Strategic exports of 2026-10-22) use the legacy cells; the roll-up helpers below map a six-stage
// cell onto one of them. That roll-up is approximate: the paper filed some codes under a stage the
// roll-up does not give back (refined battery materials, polysilicon and electrical steel were
// Midstream, while Refining rolls up to Upstream), so a node's own three-stage cells, where the export
// writes them (the AIB node's yfCellsLegacy / yfSubcomponentsLegacy), win over the roll-up
// (legacyCellsOf(), legacySubcomponentsOf()).
//
// Display (design S2, 2026-10-28): the stage ids are the labels (STAGE_LABEL), with the shorts Ext, Ref,
// Com, Asm, Dep, Rec for tight axes (STAGE_SHORT; moved here from the posture module, which imports it).
// Since 2026-10-29 (design D9) the Country Strategy Map, the drawer and the posture cards show the long
// display names of config/supply_chain_stages.yml (STAGE_DISPLAY: "Refining & Processing" ...); STAGE_LABEL
// keeps the ids, because it is also the data vocabulary of the Country and Sector contracts.
// SUPPLY_PRODUCING_STAGES is every stage but Deployment (R SUPPLY_CHAIN_SUPPLY_STAGES), for the views
// that leave Deployment out. The legacy-URL helpers at the end map an old three-stage link onto the six
// stages: Downstream is exactly Deployment; Upstream and Midstream split, so their links land on the
// technology's chain page with a notice naming the stages that replaced them.

/** The six supply-chain stages, in chain order (R SUPPLY_CHAIN_STAGES). */
export const SUPPLY_STAGES = ['Extraction', 'Refining', 'Components', 'Assembly', 'Deployment', 'Recovery'] as const;
export type SupplyStage = (typeof SUPPLY_STAGES)[number];

/** The paper's three stages, the roll-ups of the six (R SUPPLY_CHAIN_LEGACY_STAGES). */
export const LEGACY_STAGES = ['Upstream', 'Midstream', 'Downstream'] as const;
export type LegacyStage = (typeof LEGACY_STAGES)[number];

/** The stage slugs of cells, in the same order: "extraction" ... "recovery", and "upstream" ... "downstream". */
export const SUPPLY_STAGE_SLUGS = ['extraction', 'refining', 'components', 'assembly', 'deployment', 'recovery'] as const;
export type SupplyStageSlug = (typeof SUPPLY_STAGE_SLUGS)[number];
export const LEGACY_STAGE_SLUGS = ['upstream', 'midstream', 'downstream'] as const;
export type LegacyStageSlug = (typeof LEGACY_STAGE_SLUGS)[number];

/** Each six-stage stage's three-stage roll-up (an exact mirror of R SUPPLY_CHAIN_STAGE_ROLLUP). */
export const STAGE_ROLLUP: Readonly<Record<SupplyStage, LegacyStage>> = {
  Extraction: 'Upstream',
  Refining: 'Upstream',
  Components: 'Midstream',
  Assembly: 'Midstream',
  Deployment: 'Downstream',
  Recovery: 'Upstream',
};

export const isSupplyStage = (x: unknown): x is SupplyStage => (SUPPLY_STAGES as readonly unknown[]).includes(x);
export const isLegacyStage = (x: unknown): x is LegacyStage => (LEGACY_STAGES as readonly unknown[]).includes(x);

// ---- Display ---------------------------------------------------------------------------------------------

/** The label of each stage on screen: the stage id itself (design S2; not the config's long labels). */
export const STAGE_LABEL: Readonly<Record<SupplyStage, string>> = {
  Extraction: 'Extraction',
  Refining: 'Refining',
  Components: 'Components',
  Assembly: 'Assembly',
  Deployment: 'Deployment',
  Recovery: 'Recovery',
};

/**
 * The display name of each stage (design D9, 2026-10-29): the `label`s of config/supply_chain_stages.yml, for
 * the Country Strategy Map, the cell drawer, the posture cards and Country page text. STAGE_LABEL (the ids)
 * stays the data vocabulary: the Country and Sector contracts compare and look up data values with it.
 */
export const STAGE_DISPLAY: Readonly<Record<SupplyStage, string>> = {
  Extraction: 'Extraction',
  Refining: 'Refining & Processing',
  Components: 'Component Manufacturing',
  Assembly: 'Final Assembly & Equipment',
  Deployment: 'Deployment & Use',
  Recovery: 'Recovery & Recycling',
};

/** The display name of a stage name (a name that is not one of the six is returned as it is). */
export const stageDisplay = (name: string): string => (isSupplyStage(name) ? STAGE_DISPLAY[name] : name);

/** The stage id of a display name ("Refining & Processing" -> "Refining"); an id is returned as itself; anything else null. */
export function stageOfDisplay(name: string): SupplyStage | null {
  if (isSupplyStage(name)) return name;
  const hit = SUPPLY_STAGES.find((s) => STAGE_DISPLAY[s] === name);
  return hit ?? null;
}

/** The short label of each stage, for tight axes (phone column headers). */
export const STAGE_SHORT: Readonly<Record<SupplyStage, string>> = {
  Extraction: 'Ext',
  Refining: 'Ref',
  Components: 'Com',
  Assembly: 'Asm',
  Deployment: 'Dep',
  Recovery: 'Rec',
};

/** The label of a stage name (a name that is not one of the six is returned as it is). */
export const stageLabel = (name: string): string => (isSupplyStage(name) ? STAGE_LABEL[name] : name);

/** The short label of a stage name (a name that is not one of the six is returned as it is). */
export const stageShort = (name: string): string => (isSupplyStage(name) ? STAGE_SHORT[name] : name);

// ---- Deployment and the producing stages ---------------------------------------------------------------

/** Where the technology is used. Every other stage makes or supplies something. */
export const DEPLOYMENT_STAGE = 'Deployment' satisfies SupplyStage;

/** The stages that make or supply something: every stage but Deployment, in chain order (R SUPPLY_CHAIN_SUPPLY_STAGES). */
export const SUPPLY_PRODUCING_STAGES = ['Extraction', 'Refining', 'Components', 'Assembly', 'Recovery'] as const satisfies readonly SupplyStage[];
export type ProducingStage = (typeof SUPPLY_PRODUCING_STAGES)[number];

export const isProducingStage = (x: unknown): x is ProducingStage => (SUPPLY_PRODUCING_STAGES as readonly unknown[]).includes(x);

/** A stage's place in the chain (0 = Extraction ... 5 = Recovery); any other name ranks last (6). */
export function stageRank(name: string): number {
  const i = (SUPPLY_STAGES as readonly string[]).indexOf(name);
  return i < 0 ? SUPPLY_STAGES.length : i;
}

/** A sort comparator in chain order (unknown names last, in their given order: Array.prototype.sort is stable). */
export const byStageRank = (a: string, b: string): number => stageRank(a) - stageRank(b);

/** The distinct stages of a list, in chain order (unknown names last). */
export const stagesInChainOrder = (xs: Iterable<string>): string[] => [...new Set(xs)].sort(byStageRank);

/** The three-stage roll-up of a stage name: a six-stage name maps through STAGE_ROLLUP, a legacy one is itself, anything else null. */
export function rollUpStage(stage: string): LegacyStage | null {
  if (isSupplyStage(stage)) return STAGE_ROLLUP[stage];
  return isLegacyStage(stage) ? stage : null;
}

// ---- Cell slugs ----------------------------------------------------------------------------------------

/** The slug of a technology or sub-sector name, as R aib_slug() writes it ("Electric Vehicles" -> "electric-vehicles"). */
export function techSlug(name: string): string {
  return name
    .toLowerCase()
    .replace(/&/g, 'and')
    .replace(/[^a-z0-9]+/g, '-')
    .replace(/^-+|-+$/g, '');
}

/** A cell slug: "<tech slug>-<stage slug>" ("Batteries", "Refining" -> "batteries-refining"; a legacy stage gives the legacy form). */
export function cellSlug(tech: string, stage: SupplyStage | LegacyStage): string {
  return `${techSlug(tech)}-${stage.toLowerCase()}`;
}

/** A sub-component id: "<cell>::<sub-component name>" (the name as written, not slugged). */
export const subcomponentId = (cell: string, name: string): string => `${cell}::${name}`;

/** A parsed cell slug: its technology slug and stage, and whether the stage is a legacy one. */
export interface CellSlugParts {
  slug: string;
  techSlug: string;
  /** the stage name ("Refining"; "Midstream" for a legacy cell) */
  stage: SupplyStage | LegacyStage;
  legacy: boolean;
}

const STAGE_OF_SLUG: Readonly<Record<string, SupplyStage | LegacyStage>> = Object.fromEntries([
  ...SUPPLY_STAGES.map((s) => [s.toLowerCase(), s]),
  ...LEGACY_STAGES.map((s) => [s.toLowerCase(), s]),
]);
const CELL_RE = new RegExp(`^([a-z0-9]+(?:-[a-z0-9]+)*)-(${[...SUPPLY_STAGE_SLUGS, ...LEGACY_STAGE_SLUGS].join('|')})$`);

/** The slug of a stage name ("Refining" -> "refining"; a legacy name gives the legacy slug). */
export const stageSlug = (stage: SupplyStage | LegacyStage): SupplyStageSlug | LegacyStageSlug => stage.toLowerCase() as SupplyStageSlug | LegacyStageSlug;

export const isSupplyStageSlug = (x: unknown): x is SupplyStageSlug => (SUPPLY_STAGE_SLUGS as readonly unknown[]).includes(x);
export const isLegacyStageSlug = (x: unknown): x is LegacyStageSlug => (LEGACY_STAGE_SLUGS as readonly unknown[]).includes(x);

/** The six-stage name of a stage slug ("refining" -> "Refining"); null for a legacy slug or anything else. */
export const supplyStageOfSlug = (slug: string): SupplyStage | null => (isSupplyStageSlug(slug) ? (STAGE_OF_SLUG[slug] as SupplyStage) : null);

/** The legacy name of a legacy stage slug ("midstream" -> "Midstream"); null for anything else. */
export const legacyStageOfSlug = (slug: string): LegacyStage | null => (isLegacyStageSlug(slug) ? (STAGE_OF_SLUG[slug] as LegacyStage) : null);

/**
 * The parts of a cell slug, six-stage ("batteries-refining") or legacy ("batteries-midstream"); null when
 * the slug does not end in a stage slug or its technology part is not a slug.
 */
export function parseCellSlug(slug: string): CellSlugParts | null {
  const m = CELL_RE.exec(slug);
  if (!m) return null;
  const stage = STAGE_OF_SLUG[m[2]];
  return { slug, techSlug: m[1], stage, legacy: isLegacyStage(stage) };
}

/** The parts of a sub-component id ("<cell>::<name>", the name not empty); null when it is not one. */
export function parseSubcomponentId(id: string): { cell: CellSlugParts; name: string } | null {
  const i = id.indexOf('::');
  if (i < 0) return null;
  const cell = parseCellSlug(id.slice(0, i));
  const name = id.slice(i + 2);
  return cell && name ? { cell, name } : null;
}

/** True for a legacy cell slug ("<tech slug>-<upstream|midstream|downstream>"). */
export const isLegacyCellSlug = (slug: string): boolean => parseCellSlug(slug)?.legacy === true;

/** True for a sub-component id of a legacy cell ("<legacy cell>::<name>"). */
export const isLegacySubcomponentId = (id: string): boolean => parseSubcomponentId(id)?.cell.legacy === true;

/**
 * The legacy cell a cell slug rolls up to ("batteries-refining" -> "batteries-upstream",
 * "wind-components" -> "wind-midstream"); a legacy slug is returned unchanged; null when it is not a cell
 * slug. Approximate for the joins with the three-stage bundles (see the header).
 */
export function rollUpCellSlug(slug: string): string | null {
  const c = parseCellSlug(slug);
  if (!c) return null;
  if (c.legacy) return slug;
  return `${c.techSlug}-${STAGE_ROLLUP[c.stage as SupplyStage].toLowerCase()}`;
}

/** The same for a sub-component id: its cell rolled up, the name kept ("batteries-refining::X" -> "batteries-upstream::X"). */
export function rollUpSubcomponentId(id: string): string | null {
  const s = parseSubcomponentId(id);
  if (!s) return null;
  return subcomponentId(rollUpCellSlug(s.cell.slug)!, s.name);
}

const unique = (xs: (string | null)[]): string[] => [...new Set(xs.filter((x): x is string => x !== null))];

/**
 * A node's three-stage cells, for the joins with the three-stage bundles: its own (yfCellsLegacy, written by
 * the export since 2026-10-28; [] is an answer too) when present, else the roll-up of its six-stage cells
 * (distinct, first appearance kept). The roll-up is approximate (see the header).
 */
export function legacyCellsOf(n: { yfCells: readonly string[]; yfCellsLegacy?: readonly string[] | null }): string[] {
  if (Array.isArray(n.yfCellsLegacy)) return [...n.yfCellsLegacy];
  return unique(n.yfCells.map(rollUpCellSlug));
}

/** The same for a node's sub-components: yfSubcomponentsLegacy when present, else the roll-up of yfSubcomponents. */
export function legacySubcomponentsOf(n: { yfSubcomponents: readonly string[]; yfSubcomponentsLegacy?: readonly string[] | null }): string[] {
  if (Array.isArray(n.yfSubcomponentsLegacy)) return [...n.yfSubcomponentsLegacy];
  return unique(n.yfSubcomponents.map(rollUpSubcomponentId));
}

// ---- Legacy URLs (design U1) -----------------------------------------------------------------------------
// An old link names one of the three stages. Downstream became Deployment exactly. Upstream and Midstream
// split into several stages, so no one six-stage page answers them: their links land on the technology's
// chain page with a notice naming the stages that replaced them. The roll-up (STAGE_ROLLUP) gives those
// stages; the paper also filed some refined materials under Midstream (they are Refining now), so the
// words for Midstream say "mostly".

/** The six-stage stages that replaced each legacy stage, in chain order (the inverse of STAGE_ROLLUP). */
export const LEGACY_STAGE_REPLACED_BY: Readonly<Record<LegacyStage, readonly SupplyStage[]>> = Object.fromEntries(
  LEGACY_STAGES.map((l) => [l, SUPPLY_STAGES.filter((s) => STAGE_ROLLUP[s] === l)]),
) as unknown as Record<LegacyStage, readonly SupplyStage[]>;

/** The one six-stage stage a legacy stage became, by name ("Downstream" -> "Deployment"); null when it split or is not a legacy name. */
export function legacyStageTargetName(stage: string): SupplyStage | null {
  if (!isLegacyStage(stage)) return null;
  const to = LEGACY_STAGE_REPLACED_BY[stage];
  return to.length === 1 ? to[0] : null;
}

/**
 * The one six-stage stage slug a legacy stage slug became ("downstream" -> "deployment"); null for
 * "upstream" and "midstream", which split (their links go to the technology's chain page), and for
 * anything that is not a legacy slug (check isLegacyStageSlug first).
 */
export function legacyStageTarget(slug: string): SupplyStageSlug | null {
  const l = legacyStageOfSlug(slug);
  const to = l ? legacyStageTargetName(l) : null;
  return to ? (stageSlug(to) as SupplyStageSlug) : null;
}

const listAnd = (xs: readonly string[]): string => (xs.length <= 1 ? xs.join('') : `${xs.slice(0, -1).join(', ')} and ${xs[xs.length - 1]}`);

/** The stages that replaced a legacy stage, in words: "Extraction, Refining and Recovery", "mostly Components and Assembly", "Deployment". */
export function legacyStageWords(stage: LegacyStage): string {
  const words = listAnd(LEGACY_STAGE_REPLACED_BY[stage].map(stageLabel));
  return stage === 'Midstream' ? `mostly ${words}` : words;
}

/**
 * One sentence for the notice of a legacy link, optionally naming the technology:
 * "Batteries — Midstream is no longer a stage: it is now mostly Components and Assembly (some refined
 * materials are under Refining)." / "Solar — Downstream is now Solar — Deployment."
 */
export function legacyStageNote(stage: LegacyStage, tech?: string): string {
  const subject = tech ? `${tech} — ${stage}` : stage;
  const exact = legacyStageTargetName(stage);
  if (exact) return `${subject} is now ${tech ? `${tech} — ` : ''}${stageLabel(exact)}.`;
  const extra = stage === 'Midstream' ? ' (some refined materials are under Refining)' : '';
  return `${subject} is no longer a stage: it is now ${legacyStageWords(stage)}${extra}.`;
}

/** Where an old /sector/<tech>/<legacy stage>[/<economy>] link goes. */
export interface LegacySectorTarget {
  /** /sector/<tech>/deployment[/<economy>] for Downstream; the chain page /sector/<tech> for Upstream and Midstream */
  pathname: string;
  legacy: LegacyStage;
  /** the stage the link maps onto exactly (Deployment), or null when the stage split */
  exact: SupplyStage | null;
  techSlug: string;
  /** the economy segment of the old link, as written (kept on the Deployment page; the chain page has no economy route) */
  economy: string | null;
}

const LEGACY_SECTOR_RE = new RegExp(`^/sector/([a-z0-9]+(?:-[a-z0-9]+)*)/(${LEGACY_STAGE_SLUGS.join('|')})(?:/([^/]+))?/?$`);

/** The six-stage target of a legacy Sector path (no query or hash; the caller keeps those); null when the path is not one. */
export function legacySectorTarget(pathname: string): LegacySectorTarget | null {
  const m = LEGACY_SECTOR_RE.exec(pathname);
  if (!m) return null;
  const legacy = legacyStageOfSlug(m[2]) as LegacyStage;
  const exact = legacyStageTargetName(legacy);
  const economy = m[3] ?? null;
  const pathnameTo = exact ? `/sector/${m[1]}/${stageSlug(exact)}${economy ? `/${economy}` : ''}` : `/sector/${m[1]}`;
  return { pathname: pathnameTo, legacy, exact, techSlug: m[1], economy };
}
