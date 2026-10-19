// Node classes: order, colour, mark and words. The three intervention classes take the explorer's
// validated categorical triple (blue, olive, rose; theme/palette.ts GROUP_COLOR, dark surface, all pairs
// PASS); LOW INTERVENTION is a hollow mark and INSUFFICIENT DATA the shared no-data hatch, so the two
// non-intervention classes never depend on a fourth hue. Every chip also prints the class name, so
// colour is never the only carrier. Pure.

import { AIB_CLASSES, type AibClass } from '../contract.ts';

export type ClassMark = 'fill' | 'hollow' | 'hatch';

export interface ClassInfo {
  cls: AibClass;
  /** the class name as printed (the model's own words) */
  label: string;
  color: string;
  mark: ClassMark;
  /** what the class means, in one line */
  meaning: string;
}

export const CLASS_INFO: Readonly<Record<AibClass, ClassInfo>> = {
  COORDINATE: {
    cls: 'COORDINATE',
    label: 'COORDINATE',
    color: '#1889b0',
    mark: 'fill',
    meaning: 'New trade relationships add materially to the coverage existing ones restore: the coordination dividend is large.',
  },
  BUILD: {
    cls: 'BUILD',
    label: 'BUILD',
    color: '#8b9d17',
    mark: 'fill',
    meaning: 'Even optimized trade leaves a large residual gap: new capacity is needed.',
  },
  'MANAGE SPECIALIZATION': {
    cls: 'MANAGE SPECIALIZATION',
    label: 'MANAGE SPECIALIZATION',
    color: '#c96f7a',
    mark: 'fill',
    meaning: 'Supply can be restored, but the efficient solution leans on one dominant supplier, or diversifying away from it costs coverage.',
  },
  'LOW INTERVENTION': {
    cls: 'LOW INTERVENTION',
    label: 'LOW INTERVENTION',
    color: '#9ca3af',
    mark: 'hollow',
    meaning: 'The shock removes little, existing relationships already restore most of the demand, or optimization adds little.',
  },
  'INSUFFICIENT DATA': {
    cls: 'INSUFFICIENT DATA',
    label: 'INSUFFICIENT DATA',
    color: '#9ca3af',
    mark: 'hatch',
    meaning: 'Too little trade, or too few exporters and importers, to optimize with confidence.',
  },
};

const isClass = (c: string): c is AibClass => (AIB_CLASSES as readonly string[]).includes(c);

/** The class info of a class name (unknown names read as INSUFFICIENT DATA, never a new colour). */
export function classInfo(cls: string): ClassInfo {
  return isClass(cls) ? CLASS_INFO[cls] : { ...CLASS_INFO['INSUFFICIENT DATA'], label: cls };
}

/** The position of a class in the view order (unknown classes last). */
export function classOrder(cls: string): number {
  const i = (AIB_CLASSES as readonly string[]).indexOf(cls);
  return i < 0 ? AIB_CLASSES.length : i;
}

/** The classes of the order that intervene (a compact is built from these). */
export const INTERVENTION_CLASSES: readonly AibClass[] = ['COORDINATE', 'BUILD', 'MANAGE SPECIALIZATION'];

/** "2 COORDINATE, 1 BUILD": the mix of classes of some nodes, in the view order. */
export function classMix(classes: readonly string[]): { cls: string; n: number }[] {
  const counts = new Map<string, number>();
  for (const c of classes) counts.set(c, (counts.get(c) ?? 0) + 1);
  return [...counts.entries()].map(([cls, n]) => ({ cls, n })).sort((a, b) => classOrder(a.cls) - classOrder(b.cls) || a.cls.localeCompare(b.cls));
}

/**
 * A node's multi-label policy needs in words, in the class order: "BUILD + MANAGE SPECIALIZATION", "none"
 * for an empty profile, null when the export has none (an older bundle).
 */
export function policyNeedsText(needs: readonly string[] | null | undefined): string | null {
  if (!needs) return null;
  if (!needs.length) return 'none';
  return [...new Set(needs)].sort((a, b) => classOrder(a) - classOrder(b) || a.localeCompare(b)).join(' + ');
}

/** True when the needs say more than the class: several needs, or one that is not the class itself. */
export function needsAddToClass(cls: string, needs: readonly string[] | null | undefined): boolean {
  if (!needs || !needs.length) return false;
  return needs.length > 1 || needs[0] !== cls;
}

/**
 * The compact's policy needs by node as the exporter writes them ("Battery Cells: BUILD; Lithium
 * Chemicals: BUILD + MANAGE SPECIALIZATION"; "none" when a node has none) as { node, needs }.
 */
export function parseNodePolicyNeeds(s: string | null | undefined): { node: string; needs: string }[] {
  if (!s) return [];
  return s
    .split('; ')
    .map((part) => {
      const i = part.lastIndexOf(': ');
      return i < 0 ? { node: part.trim(), needs: '' } : { node: part.slice(0, i).trim(), needs: part.slice(i + 2).trim() };
    })
    .filter((x) => x.node);
}

/**
 * The compact's class mix as the exporter writes it ("BUILD 78%; COORDINATE 22%": lost-supply shares of
 * the intervention classes) as { cls, share } in the written order; malformed parts are skipped.
 */
export function parseClassMix(mix: string | null | undefined): { cls: string; share: number }[] {
  if (!mix) return [];
  return mix
    .split(';')
    .map((part) => /^\s*(.+?)\s+(\d+(?:\.\d+)?)%\s*$/.exec(part))
    .filter((m): m is RegExpExecArray => m !== null)
    .map((m) => ({ cls: m[1], share: Number(m[2]) / 100 }));
}
