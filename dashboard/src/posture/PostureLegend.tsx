// The choice legend and the trade freshness line, shared by the Country, Sector and AIB headline charts
// (spec XX; the fence grouping of 2026-10-30): one colour per choice, grouped by fence ("High fence:
// Secure", "Medium fence: Preserve, Catch Up, Partner, Accelerate", "Low fence: Watch"), then the hatched
// "Not assessed"; each swatch can carry its node count. The legend is text (a list), not decoration, so a
// screen reader hears the same grouping and counts. The Strategy Map adds "Mixed: this step's layers sit in
// different yards" (2026-10-30, yards.ts) with a sample of its diagonal split.

import type { PostureCore, Verb } from './contract.ts';
import { freshnessLine } from './derive.ts';
import { FENCE_LABEL, FENCE_ORDER, FENCE_VERBS, NOT_ASSESSED_BORDER, NOT_ASSESSED_HATCH, VERB_COLOR, VERB_LABEL, VERB_OUTLINE } from './grammar.ts';
import { MIXED_LEGEND, mixedFill } from './yards.ts';

/** The legend's sample of a mixed node: a Catch Up node with layers in the Large yard (the map's diagonal split). */
const MIXED_SAMPLE = mixedFill({ posture: 'C', mode: 'catch_up', deployMode: null, yards: 'mixed_medium_low' }) as string;

/** The swatch of a mixed node (the map's diagonal split, here a Medium-yard choice with a Large-yard layer). */
export function MixedSwatch({ size = 18 }: { size?: number }) {
  return (
    <span
      aria-hidden="true"
      className="inline-block shrink-0 rounded-[3px]"
      style={{ width: size, height: size, background: MIXED_SAMPLE, border: `1px solid ${VERB_OUTLINE.watch}` }}
      data-swatch="mixed"
    />
  );
}

/** A colour swatch: a choice colour (Watch with its outline) or the not-assessed hatch. */
export function Swatch({ color, size = 18, hatched = false, verb }: { color?: string; size?: number; hatched?: boolean; verb?: Verb }) {
  const fill = verb ? VERB_COLOR[verb] : color;
  return (
    <span
      aria-hidden="true"
      className="inline-block shrink-0 rounded-[3px]"
      style={{
        width: size,
        height: size,
        background: hatched ? NOT_ASSESSED_HATCH : fill,
        border: hatched ? NOT_ASSESSED_BORDER : verb === 'watch' ? `1px solid ${VERB_OUTLINE.watch}` : undefined,
      }}
      data-swatch={hatched ? 'not_assessed' : (verb ?? undefined)}
    />
  );
}

export type ChoiceLegendCounts = Partial<Record<Verb | 'N', number>>;

export interface ChoiceLegendProps {
  /** the node count of each choice (and N, not assessed); omit for a legend without counts */
  counts?: ChoiceLegendCounts | null;
  /** the choices this chart can show (default all six); a fence with none of them is left out */
  verbs?: readonly Verb[];
  /** the hatched "Not assessed" entry (default on) */
  notAssessed?: boolean;
  /**
   * the "Mixed" entry of the Strategy Map (a node whose layers sit in different yards, drawn as a diagonal
   * split): its count, or true without one; off by default and when 0
   */
  mixed?: number | boolean | null;
  className?: string;
}

/** The choices grouped by fence, each with its colour and (optionally) its count; then "Mixed" (where asked) and "Not assessed". */
export function ChoiceLegend({ counts, verbs, notAssessed = true, mixed = null, className }: ChoiceLegendProps) {
  const shown = (v: Verb) => !verbs || verbs.includes(v);
  const n = (k: Verb | 'N') => (counts ? (counts[k] ?? 0) : null);
  return (
    <ul className={`flex flex-wrap items-center gap-x-5 gap-y-2 text-xs text-paper-dim ${className ?? ''}`} aria-label="Legend: choices by fence" data-testid="posture-legend">
      {FENCE_ORDER.filter((f) => FENCE_VERBS[f].some(shown)).map((f) => (
        <li key={f} className="inline-flex flex-wrap items-center gap-x-2.5 gap-y-1" data-fence={f}>
          <span className="font-medium text-paper">{FENCE_LABEL[f]}:</span>
          {FENCE_VERBS[f].filter(shown).map((v, i, all) => (
            <span key={v} className="inline-flex items-center gap-1.5" data-verb={v}>
              <Swatch verb={v} size={14} />
              <span>
                {VERB_LABEL[v]}
                {n(v) !== null && <span className="tabular text-paper-mute"> {n(v)}</span>}
                {i < all.length - 1 ? <span className="sr-only">,</span> : null}
              </span>
            </span>
          ))}
        </li>
      ))}
      {mixed ? (
        <li className="inline-flex items-center gap-1.5" data-verb="mixed" data-testid="legend-mixed">
          <MixedSwatch size={14} />
          <span>
            {MIXED_LEGEND}
            {typeof mixed === 'number' && <span className="tabular text-paper-mute"> {mixed}</span>}
          </span>
        </li>
      ) : null}
      {notAssessed && (
        <li className="inline-flex items-center gap-1.5" data-verb="not_assessed">
          <Swatch hatched size={14} />
          <span>
            Not assessed
            {n('N') !== null && <span className="tabular text-paper-mute"> {n('N')}</span>}
          </span>
        </li>
      )}
    </ul>
  );
}

/** "Structural trade benchmark: 2022–24 · Current signal through: Aug 2026" (or why there is none). */
export function TradeFreshness({ meta }: { meta: PostureCore['meta'] }) {
  const f = freshnessLine(meta);
  return (
    <p className="text-xs text-paper-mute" data-testid="trade-freshness">
      {f.benchmark} · {f.signal}
    </p>
  );
}
