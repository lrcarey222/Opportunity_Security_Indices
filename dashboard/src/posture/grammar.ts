// The posture visual grammar, one for the whole site: the same colour, mark and words for a posture,
// a Compete mode, a Secure rationale, a policy alignment, evidence quality and trade momentum on the
// Country, Sector and AIB views.
//
// Posture colours are a categorical triad validated with the dataviz validator on the dark surface
// #1f2937 (2026-10-28): "#d1497e,#1889b0,#9a8f3d" passes lightness band, chroma floor, CVD (worst
// protan dE 10.3), the normal-vision floor (19.8) and 3:1 contrast. Posture is never carried by colour
// alone: every cell and point also carries its letter or mode mark, and the legend names all three.

import type { AibGap, Alignment, CompeteMode, Momentum, PostureCode, SecurityRationale } from './contract.ts';

// The stage shorts (Ext, Ref, Com, Asm, Dep, Rec) live in src/lib/stages.ts since 2026-10-28, so every module
// uses one map; re-exported here for the posture views.
export { STAGE_SHORT, stageShort } from '../lib/stages.ts';

export const POSTURE_COLOR: Record<PostureCode, string> = { S: '#d1497e', C: '#1889b0', D: '#9a8f3d' };
export const POSTURE_LABEL: Record<PostureCode, string> = { S: 'Secure', C: 'Compete', D: 'Deploy' };
export const POSTURE_ORDER: PostureCode[] = ['S', 'C', 'D'];
export const POSTURE_FROM_WORD: Record<string, PostureCode> = { secure: 'S', compete: 'C', deploy: 'D' };

/** The mark a Compete cell carries: P = Preserve, C = Catch Up, the link = Partner (arrows are reserved for trade momentum). */
export const MODE_MARK: Record<CompeteMode, string> = { preserve: 'P', catch_up: 'C', partner: '🔗︎' };
export const MODE_LABEL: Record<CompeteMode, string> = { preserve: 'Preserve', catch_up: 'Catch up', partner: 'Partner' };
export const QUADRANT_LABEL: Record<string, string> = {
  catch_up: 'Catch up', preserve: 'Preserve', partner: 'Partner', watch: 'Watch', insufficient_data: 'Insufficient data',
};

export const RATIONALE_LABEL: Record<SecurityRationale, string> = {
  operational_control: 'Operational control',
  coercive_chokepoint: 'Coercive chokepoint',
  systemic_resilience: 'Systemic resilience',
};
export const RATIONALE_MARK: Record<SecurityRationale, string> = { operational_control: 'O', coercive_chokepoint: 'K', systemic_resilience: 'R' };
export const RATIONALE_WORDS: Record<SecurityRationale, string> = {
  operational_control: 'a foreign actor could command, update or disable it, or it serves national-security end uses',
  coercive_chokepoint: 'a potentially coercive supplier could withhold it, substitution is hard and trusted supply too slow to restore',
  systemic_resilience: 'it carries an essential function, the country depends on imports, supply takes years to rebuild and trusted partners cannot replace it',
};

/** Policy alignment: a border style (never colour alone; the tooltip and drawer say it in words). */
export const ALIGNMENT_LABEL: Record<Alignment, string> = {
  aligned: 'Aligned', under_prioritized: 'Under-prioritized', over_prioritized: 'Over-prioritized', mixed: 'Mixed', insufficient_data: 'Insufficient data',
};
export const ALIGNMENT_BORDER: Record<Alignment, string> = {
  aligned: '1px solid rgb(255 255 255 / 0.18)',
  under_prioritized: '2px dashed #ffffff',
  over_prioritized: '2px dotted #ffffff',
  mixed: '2px double #ffffff',
  insufficient_data: '1px solid transparent',
};
export const ALIGNMENT_ICON: Partial<Record<Alignment, string>> = { under_prioritized: '▽', over_prioritized: '△', mixed: '≈' };

/** Evidence quality as opacity: well-evidenced cells are solid, weak ones fade (and say so in words). */
export function evidenceOpacity(ev: string | null | undefined): number {
  return ev === 'high' ? 1 : ev === 'moderate' ? 0.85 : ev === 'low' ? 0.6 : ev === 'none' ? 0.45 : 0.75;
}
export const EVIDENCE_HATCH = 'repeating-linear-gradient(135deg, rgb(255 255 255 / 0.18) 0 2px, transparent 2px 6px)';

export const MOMENTUM_ARROW: Record<Momentum, string> = { up: '↑', flat: '→', down: '↓' };
export const MOMENTUM_LABEL: Record<Momentum, string> = { up: 'rising', flat: 'flat', down: 'falling' };
export const MOMENTUM_UNAVAILABLE = 'Current momentum unavailable — incomplete matched-period reporting';

export const GAP_LABEL: Record<AibGap, string> = {
  none: 'Covered',
  no_trusted_supplier: 'No trusted supplier',
  single_point_of_failure: 'Single point of failure',
  insufficient_redundancy: 'Insufficient redundancy',
  weak_evidence: 'Weak evidence',
};
/** Gap severity order (worst first) and a fill for the AIB matrix: a gap is drawn as a ring and a word. */
export const GAP_ORDER: AibGap[] = ['no_trusted_supplier', 'single_point_of_failure', 'insufficient_redundancy', 'weak_evidence', 'none'];
export const GAP_RING: Record<AibGap, string> = {
  no_trusted_supplier: '#e2574c',
  single_point_of_failure: '#f58229',
  insufficient_redundancy: '#d1d5db',
  weak_evidence: '#6b7280',
  none: 'transparent',
};
