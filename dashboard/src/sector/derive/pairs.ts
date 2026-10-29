// Copied values at six stages (design C2, 2026-10-28): some sources give one figure for a technology's
// manufacturing, not one per stage, and the bundle carries it on both Components and Assembly (CIM
// manufacturing capex, the IEA manufacturing capacity shares): every such fact has the facts.notes id
// `same_figure_pair`. Batteries — Refining and Electric Vehicles — Refining split one critical-minerals
// capex series (`cim_minerals_split`). Each cell shows the figure with a "same figure as" note; a total
// over cells counts a pair once (R sector_count_once(): drop the Assembly copy). Pure.

import { cellSlug, STAGE_LABELS, type StageSlug } from '../contract.ts';
import type { FactsIndex } from '../data/facts.ts';
import type { ItemInfo, SectorModel, VarInfo } from '../data/model.ts';

/** facts.notes id of a figure Components and Assembly both carry. */
export const SAME_FIGURE_NOTE = 'same_figure_pair';
/** facts.notes id of the critical-minerals capex split between two Refining cells. */
export const SPLIT_NOTE = 'cim_minerals_split';
/** Data caveats a caption shows only when the measure's facts on the item carry the note (R SECTOR_DATA_CAVEATS). */
export const DATA_CAVEAT_IDS: ReadonlySet<string> = new Set([SAME_FIGURE_NOTE, SPLIT_NOTE]);

/** The two stages of a manufacturing pair, in chain order; the second carries the copy totals drop. */
export const PAIR_STAGES: readonly [StageSlug, StageSlug] = ['components', 'assembly'];

/** The other cell of a manufacturing pair (Components <-> Assembly of the same technology), or null. */
export function pairCellOf(model: SectorModel, cell: ItemInfo): ItemInfo | null {
  if (cell.axis !== 'cell' || !cell.techSlug || !cell.stageSlug) return null;
  const i = PAIR_STAGES.indexOf(cell.stageSlug);
  if (i < 0) return null;
  return model.item('cell', cellSlug(cell.techSlug, PAIR_STAGES[1 - i]));
}

/** Does any fact of (var, item) carry a facts.notes id (in `year`, else in any year)? */
export function hasFactNote(facts: FactsIndex | null, v: VarInfo, item: ItemInfo, note: string, year?: number | null): boolean {
  if (!facts) return false;
  const years = year !== null && year !== undefined ? [year] : facts.yearsOf(v.idx, item.idx);
  return years.some((y) => facts.rows(v.idx, item.idx, y).some((r) => r.notes?.includes(note)));
}

/**
 * The "same figure as" note of a measure on a cell: "Same figure as Batteries — Assembly: the source
 * gives one figure for the technology's manufacturing, not one per stage. Never add the two." Null when
 * the cell's facts of the measure do not carry `same_figure_pair`.
 */
export function sameFigureText(model: SectorModel, facts: FactsIndex | null, v: VarInfo, cell: ItemInfo, year?: number | null): string | null {
  if (!hasFactNote(facts, v, cell, SAME_FIGURE_NOTE, year)) return null;
  const other = pairCellOf(model, cell);
  const name = other ? model.cellLabel(other) : 'the other manufacturing stage';
  return `Same figure as ${name}: the source gives one figure for the technology’s manufacturing, not one per stage. Never add the two; totals count it once.`;
}

/**
 * The note of a split series on a cell: "Batteries — Refining and Electric Vehicles — Refining split one
 * critical-minerals investment series by demand shares: each holds part of it, and the two add up to
 * it." Null when the cell's facts of the measure do not carry `cim_minerals_split`.
 */
export function splitText(model: SectorModel, facts: FactsIndex | null, v: VarInfo, cell: ItemInfo, year?: number | null): string | null {
  if (!hasFactNote(facts, v, cell, SPLIT_NOTE, year)) return null;
  const others = model.cells.filter((c) => c.idx !== cell.idx && hasFactNote(facts, v, c, SPLIT_NOTE, year));
  const names = [model.cellLabel(cell), ...others.map((c) => model.cellLabel(c))];
  const who = names.length > 1 ? `${names.slice(0, -1).join(', ')} and ${names[names.length - 1]}` : names[0];
  return `${who} split one critical-minerals investment series (plants making battery chemicals) by the technologies’ demand shares: each holds part of it, and they add up to it.`;
}

/** Is a fact a copy that a total over cells drops (the Assembly side of a `same_figure_pair`)? */
export function isPairCopy(cell: ItemInfo, notes: readonly string[] | undefined): boolean {
  return !!notes?.includes(SAME_FIGURE_NOTE) && cell.stageSlug === PAIR_STAGES[1];
}

/** The stage words of a pair ("Components and Assembly"). */
export const PAIR_WORDS = `${STAGE_LABELS[PAIR_STAGES[0]]} and ${STAGE_LABELS[PAIR_STAGES[1]]}`;
