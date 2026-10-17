// How robust a node's class is: the class under the other variants the model runs (each supply
// constraint, each shock semantics, the other supply scenario, the other shocks), and the existing-
// relationship growth-bound sensitivity (existing coverage, coordination dividend and class when an
// existing relationship may grow to about 1.25x, 2x (the headline), 4x or any multiple of its observed
// size). Every class shown carries its own PHYSICAL CAPABILITY NOT VALIDATED flag (node.variantWarnings,
// growth[].warning; null in an export that predates them). A node that is COORDINATE under some bounds and
// another class under others depends on the bound, whatever its headline class: the exporter flags it
// (coordinateSensitiveToGrowth) and the words start from the headline class. Pure.

import { isNum } from '../../lib/format.ts';
import { AIB_CONSTRAINTS, AIB_PHYSICAL_WARNING, AIB_SEMANTICS, variantsOf, variantWarningsOf, type AibMeta, type AibNode } from '../contract.ts';
import { listWords } from './format.ts';
import { constraintLabel, SEMANTICS_WORDS } from './settings.ts';

export type VariantGroup = 'Supply constraint' | 'Shock semantics' | 'Supply scenario' | 'Shock' | 'Other';

export interface VariantRow {
  group: VariantGroup;
  /** the variant's key in the export ("trade_only", "release", "conservative", a shock id) */
  key: string;
  /** the setting in words */
  setting: string;
  cls: string | null;
  /** the class under this variant carries the physical-capability warning (null: the export does not say) */
  warning: boolean | null;
  /** the headline setting of its group */
  headline: boolean;
  /** a class other than the headline one */
  differs: boolean;
}

const cap = (s: string) => (s ? s[0].toUpperCase() + s.slice(1) : s);
const uniq = (xs: readonly (string | null | undefined)[]): string[] => [...new Set(xs.filter((x): x is string => typeof x === 'string' && x.length > 0))];

/**
 * The warning of the node's class under one variant: the exported flag; for a headline setting the export
 * does not list as a variant, the headline warning; null when the export predates the flags.
 */
export function variantWarning(n: AibNode, key: string, headline = false): boolean | null {
  const vw = variantWarningsOf(n);
  if (vw && key in vw) return vw[key] === true;
  if (headline && !(key in variantsOf(n))) return n.warning !== undefined ? !!n.warning : null;
  return vw ? false : null;
}

/**
 * The node's class under every variant the export carries, grouped (supply constraint, shock semantics,
 * scenario, shock, then anything else), each group with its headline setting (the node's own class), each
 * with its physical-capability flag; [] when the export has no variants.
 */
export function classVariants(n: AibNode, meta: AibMeta): VariantRow[] {
  const v = variantsOf(n);
  if (!Object.keys(v).length) return [];
  const rows: VariantRow[] = [];
  const used = new Set<string>();
  const add = (group: VariantGroup, key: string, setting: string, headline: boolean) => {
    if (used.has(key)) return;
    const cls = key in v ? v[key] : headline ? n.cls : undefined;
    if (cls === undefined) return;
    used.add(key);
    rows.push({ group, key, setting, cls, warning: variantWarning(n, key, headline), headline, differs: cls !== null && cls !== n.cls });
  };
  for (const k of uniq([meta.supplyConstraint, ...(meta.constraints ?? []), ...AIB_CONSTRAINTS])) add('Supply constraint', k, constraintLabel(k), k === meta.supplyConstraint);
  const headSem = meta.shockSemantics ?? ('release' in v ? 'hold' : null);
  for (const k of uniq([headSem, ...(meta.semantics ?? []), ...AIB_SEMANTICS])) add('Shock semantics', k, SEMANTICS_WORDS[k] ? cap(SEMANTICS_WORDS[k].short) : k, k === headSem);
  for (const k of uniq([meta.headlineScenario, ...meta.scenarios])) add('Supply scenario', k, `${cap(k)} scenario`, k === meta.headlineScenario);
  const shocks = [...meta.shocks].sort((a, b) => Number(b.id === meta.headlineShock) - Number(a.id === meta.headlineShock));
  for (const s of shocks) add('Shock', s.id, s.label, s.id === meta.headlineShock);
  for (const k of Object.keys(v)) add('Other', k, k, false);
  return rows;
}

export interface ConstraintClasses {
  productionConstrained: string | null;
  tradeOnly: string | null;
  /** each class carries the physical-capability warning (null: the export does not say) */
  productionConstrainedWarning: boolean | null;
  tradeOnlyWarning: boolean | null;
  differs: boolean;
}

/** The class under production-constrained and trade-only supply, with their warnings (null when the export has neither). */
export function constraintClasses(n: AibNode): ConstraintClasses | null {
  const v = variantsOf(n);
  if (!('production_constrained' in v) && !('trade_only' in v)) return null;
  const pc = v.production_constrained ?? null;
  const to = v.trade_only ?? null;
  return {
    productionConstrained: pc,
    tradeOnly: to,
    productionConstrainedWarning: pc === null ? null : variantWarning(n, 'production_constrained'),
    tradeOnlyWarning: to === null ? null : variantWarning(n, 'trade_only'),
    differs: pc !== null && to !== null && pc !== to,
  };
}

/** The variant keys whose class carries the warning ([] when none; null when the export predates the flags). */
export function warnedVariants(n: AibNode): string[] | null {
  const vw = variantWarningsOf(n);
  return vw ? Object.keys(vw).filter((k) => vw[k] === true) : null;
}

/** The headline growth bound (config/aib.yml existing_relationship_growth = 1: up to 2x the observed size). */
export const HEADLINE_GROWTH = '2x';

export interface GrowthRow {
  label: string;
  /** the bound in words */
  bound: string;
  existing: number | null;
  dividend: number | null;
  cls: string | null;
  /** the class at this bound carries the physical-capability warning (null: the export does not say) */
  warning: boolean | null;
  headline: boolean;
}

/** "up to 1.25× its observed size", "no bound". */
export function growthBoundWords(label: string): string {
  if (/^unlimited$/i.test(label)) return 'no bound';
  const m = /^([\d.]+)x$/i.exec(label);
  return m ? `up to ${m[1]}× its observed size` : label;
}

/** The growth-bound sensitivity of a node, tightest bound first as the export writes it ([] when none). */
export function growthRows(n: AibNode): GrowthRow[] {
  return (n.growth ?? []).map((g) => ({
    label: g.label,
    bound: growthBoundWords(g.label),
    existing: isNum(g.existing) ? g.existing : null,
    dividend: isNum(g.dividend) ? g.dividend : null,
    cls: g.cls,
    warning: typeof g.warning === 'boolean' ? g.warning : null,
    headline: g.label === HEADLINE_GROWTH,
  }));
}

/** The growth bounds whose class carries the warning ([] when none; null when the export predates the flags). */
export function warnedGrowthBounds(n: AibNode): string[] | null {
  const rows = growthRows(n);
  if (n.growth === undefined || (rows.length && rows.every((r) => r.warning === null))) return null;
  return rows.filter((r) => r.warning === true).map((r) => r.label);
}

/**
 * The summary line of a node's robustness tables: how many variants change the class, whether the class
 * depends on the growth bound, and under how many non-headline settings the class carries the warning.
 */
export function robustnessSummary(n: AibNode, meta: AibMeta): string {
  const variants = classVariants(n, meta);
  const growth = growthRows(n);
  const differs = variants.filter((v) => v.differs).length;
  const warned = variants.filter((v) => v.warning === true && !v.headline).length + growth.filter((g) => g.warning === true && !g.headline).length;
  return [
    'Class under other assumptions and growth bounds',
    differs ? `differs under ${differs} ${differs === 1 ? 'variant' : 'variants'}` : null,
    n.coordinateSensitiveToGrowth ? 'depends on the growth bound' : null,
    warned ? `physical capability not validated under ${warned} other ${warned === 1 ? 'setting' : 'settings'}` : null,
  ]
    .filter(Boolean)
    .join(' · ');
}

/**
 * When the node is COORDINATE under some growth bounds and another class under others (the exporter's
 * flag, whatever the headline class): the sentence that says so, starting from the headline class
 * ("Headline LOW INTERVENTION; COORDINATE only at 1.25x: the class depends on the existing-relationship
 * growth bound." / "Headline COORDINATE; LOW INTERVENTION at 4x and unlimited: ..."), else null. Each
 * class named under other bounds carries its own PHYSICAL CAPABILITY NOT VALIDATED flag (growth[].warning):
 * "COORDINATE (PHYSICAL CAPABILITY NOT VALIDATED) only at 1.25x", or, warned at some of its bounds only,
 * "LOW INTERVENTION at 4x and unlimited (PHYSICAL CAPABILITY NOT VALIDATED at 4x)". The headline class
 * carries its warning beside it wherever the flag is shown.
 */
export function growthFlag(n: AibNode): string | null {
  if (!n.coordinateSensitiveToGrowth) return null;
  const byCls = new Map<string, { labels: string[]; warned: string[] }>();
  for (const r of growthRows(n)) {
    const k = r.cls ?? 'no class';
    const g = byCls.get(k) ?? { labels: [], warned: [] };
    g.labels.push(r.label);
    if (r.warning === true) g.warned.push(r.label);
    byCls.set(k, g);
  }
  const parts = [...byCls.entries()]
    .filter(([cls]) => cls !== n.cls)
    .map(([cls, { labels, warned }]) => {
      const all = warned.length > 0 && warned.length === labels.length;
      const tag = all ? ` (${AIB_PHYSICAL_WARNING})` : '';
      const some = !all && warned.length ? ` (${AIB_PHYSICAL_WARNING} at ${listWords(warned)})` : '';
      return `${cls}${tag}${cls === 'COORDINATE' ? ' only' : ''} at ${listWords(labels)}${some}`;
    });
  return `Headline ${n.cls}; ${parts.join('; ') || 'another class under other bounds'}: the class depends on the existing-relationship growth bound.`;
}
