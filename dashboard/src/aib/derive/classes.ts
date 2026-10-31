// Node classes: order, colour, mark and words. The three intervention classes take the explorer's
// validated categorical triple (blue, olive, rose; theme/palette.ts GROUP_COLOR, dark surface, all pairs
// PASS); LOW INTERVENTION is a hollow mark and INSUFFICIENT DATA the shared no-data hatch, so the two
// non-intervention classes never depend on a fourth hue. Every chip also prints the class name, so
// colour is never the only carrier. Pure.

import { AIB_CLASS_DEFINITION, AIB_CLASS_NAME, STATE } from '../../content/vocabulary.ts';
import { AIB_CLASSES, AIB_PHYSICAL_WARNING, type AibClass } from '../contract.ts';

export type ClassMark = 'fill' | 'hollow' | 'hatch';

export interface ClassInfo {
  cls: AibClass;
  /** the class name as printed: what the step needs, in plain words (content/vocabulary.ts AIB_CLASS_NAME; the data's code is `cls`) */
  label: string;
  color: string;
  mark: ClassMark;
  /** what the class means, in one plain line (AIB_CLASS_DEFINITION) */
  meaning: string;
  /** the rule behind the class in the method's words (About's method detail, chip tooltips) */
  method: string;
}

export const CLASS_INFO: Readonly<Record<AibClass, ClassInfo>> = {
  COORDINATE: {
    cls: 'COORDINATE',
    label: AIB_CLASS_NAME.COORDINATE,
    color: '#1889b0',
    mark: 'fill',
    meaning: AIB_CLASS_DEFINITION.COORDINATE,
    method: 'New trade relationships add materially to the coverage existing ones recover: the coordination dividend is large.',
  },
  BUILD: {
    cls: 'BUILD',
    label: AIB_CLASS_NAME.BUILD,
    color: '#8b9d17',
    mark: 'fill',
    meaning: AIB_CLASS_DEFINITION.BUILD,
    method: 'Even coordinated trade with every potential supplier leaves a large remaining gap: new capacity is needed.',
  },
  'MANAGE SPECIALIZATION': {
    cls: 'MANAGE SPECIALIZATION',
    label: AIB_CLASS_NAME['MANAGE SPECIALIZATION'],
    color: '#c96f7a',
    mark: 'fill',
    meaning: AIB_CLASS_DEFINITION['MANAGE SPECIALIZATION'],
    method: 'Supply can be recovered, but the cheapest solution leans on one dominant supplier, or diversifying away from it costs coverage.',
  },
  'LOW INTERVENTION': {
    cls: 'LOW INTERVENTION',
    label: AIB_CLASS_NAME['LOW INTERVENTION'],
    color: '#9ca3af',
    mark: 'hollow',
    meaning: AIB_CLASS_DEFINITION['LOW INTERVENTION'],
    method: 'The disruption removes little, existing relationships already recover most of the demand, or coordinated trade adds little.',
  },
  'INSUFFICIENT DATA': {
    cls: 'INSUFFICIENT DATA',
    label: AIB_CLASS_NAME['INSUFFICIENT DATA'],
    color: '#9ca3af',
    mark: 'hatch',
    meaning: AIB_CLASS_DEFINITION['INSUFFICIENT DATA'],
    method: 'Too little trade, or too few exporters and importers, to test a coordinated response with confidence.',
  },
};

const isClass = (c: string): c is AibClass => (AIB_CLASSES as readonly string[]).includes(c);

/** The class info of a class name (unknown names read as INSUFFICIENT DATA, never a new colour). */
/**
 * The export's physical-capability warning in plain words ("Production evidence is weak"); the code itself
 * (PHYSICAL CAPABILITY NOT VALIDATED) stays in tooltips, CSVs and the AIB About page. Another warning text passes
 * through unchanged.
 */
export const physicalWarningWords = (warning: string): string => (warning === AIB_PHYSICAL_WARNING ? STATE.weakProductionEvidence : warning);
/** The same warning inside a sentence or after a treatment name, lower case: "Coordinate trade (production evidence is weak)". */
export const PHYSICAL_WARNING_INLINE = STATE.weakProductionEvidence.toLowerCase();

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

/**
 * policyNeedsText in plain words for the page ("Build capacity + Manage specialization"; "none"; null when
 * the export has none). The CSVs keep the data's codes (policyNeedsText).
 */
export function policyNeedsWords(needs: readonly string[] | null | undefined): string | null {
  if (!needs) return null;
  if (!needs.length) return 'none';
  return [...new Set(needs)]
    .sort((a, b) => classOrder(a) - classOrder(b) || a.localeCompare(b))
    .map((c) => classInfo(c).label)
    .join(' + ');
}

/** True when the needs say more than the class: several needs, or one that is not the class itself. */
export function needsAddToClass(cls: string, needs: readonly string[] | null | undefined): boolean {
  if (!needs || !needs.length) return false;
  return needs.length > 1 || needs[0] !== cls;
}

/** One node's needs as the exporter writes them: class names joined with " + ", or "none". */
const NEEDS_RE = /^(.*): ((?:[A-Z][A-Z ]*[A-Z])(?: \+ [A-Z][A-Z ]*[A-Z])*|none)$/;

/**
 * The compact's policy needs by node as the exporter writes them ("Battery Cells: BUILD; Lithium
 * Chemicals: BUILD + MANAGE SPECIALIZATION"; "none" when a node has none) as { node, needs }. Node labels
 * are never split: a part ends only where the needs (upper-case class names or "none") end, so a label
 * holding "; " or ": " survives (the dictionary's labels hold ", " and "/", neither "; " nor ": ").
 */
export function parseNodePolicyNeeds(s: string | null | undefined): { node: string; needs: string }[] {
  if (!s) return [];
  const out: { node: string; needs: string }[] = [];
  let buf = '';
  for (const part of s.split('; ')) {
    buf = buf ? `${buf}; ${part}` : part;
    const m = NEEDS_RE.exec(buf);
    if (m) {
      if (m[1].trim()) out.push({ node: m[1].trim(), needs: m[2] });
      buf = '';
    }
  }
  // a tail without recognisable needs: kept whole, as before
  if (buf.trim()) {
    const i = buf.lastIndexOf(': ');
    out.push(i < 0 ? { node: buf.trim(), needs: '' } : { node: buf.slice(0, i).trim(), needs: buf.slice(i + 2).trim() });
  }
  return out.filter((x) => x.node);
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
