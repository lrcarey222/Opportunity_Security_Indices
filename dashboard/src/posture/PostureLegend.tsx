// The choice legend and the trade freshness line, shared by the Country, Sector and AIB headline charts
// (spec XX; the fence grouping of 2026-10-30): one colour per choice, grouped by fence ("High fence:
// Secure", "Medium fence: Preserve, Catch Up, Partner, Accelerate", "Low fence: Watch"), then the hatched
// "Not assessed"; each swatch can carry its node count. The legend is text (a list), not decoration, so a
// screen reader hears the same grouping and counts. The Strategy Map adds "Mixed: this step's layers sit in
// different yards" (2026-10-30, yards.ts) with a sample of its diagonal split.
// The Country Strategy Map's own legend (StrategyMapLegend, 2026-10-31) defines every term: each fence headed by
// its yard in the user's words, each choice with one plain line, Not assessed and Mixed (the split cell).

import type { ReactNode } from 'react';
import { CHOICE_ACTION, STATE, WATCH_DEPLOYMENT_DEFINITION, YARD_AND_FENCE, YARD_DEFINITION } from '../content/vocabulary.ts';
import type { PostureCore, Verb, Yard } from './contract.ts';
import { freshnessLine } from './derive.ts';
import { FENCE_LABEL, FENCE_ORDER, FENCE_VERBS, NOT_ASSESSED_BORDER, NOT_ASSESSED_HATCH, VERB_COLOR, VERB_LABEL, VERB_OUTLINE, type Fence } from './grammar.ts';
import { MIXED_LEGEND, mixedFill } from './yards.ts';

const cap1 = (s: string): string => s.charAt(0).toUpperCase() + s.slice(1);

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

// ---- the Country Strategy Map's legend: every term defined (2026-10-31) ------------------------------------------

/** The not-assessed entry's one line (vocabulary STATE.notAssessed, without its lead). */
export const NOT_ASSESSED_DEFINITION = cap1(STATE.notAssessed.replace(/^Not assessed:\s*/, ''));
/** The mixed entry's name and one line: a split cell, whose parts sit behind different fences. */
export const MIXED_NAME = 'Mixed';
export const MIXED_DEFINITION = 'A cell split on the diagonal: parts of the step sit behind different fences. Hover over it, or tap it, to see which.';
/** The Medium-fence choice that belongs to the Deployment & Use column only. */
const DEPLOYMENT_ONLY: Partial<Record<Verb, string>> = { accelerate: 'Deployment & Use only' };
/** Each choice's one line: its action (vocabulary CHOICE_ACTION); Watch adds what it means in the Deployment & Use column. */
export const MAP_CHOICE_DEFINITION: Record<Verb, string> = {
  ...(Object.fromEntries(Object.entries(CHOICE_ACTION).map(([v, a]) => [v, `${a}.`])) as Record<Verb, string>),
  watch: `${CHOICE_ACTION.watch}. At Deployment & Use, ${WATCH_DEPLOYMENT_DEFINITION.split(':')[0].replace(/^D/, 'd')}.`,
};

/** The fence group of each yard: High fence, medium (permeable) fence, Low fence. */
const YARD_OF_FENCE: Record<Fence, Yard> = { high: 'small', medium: 'medium', low: 'large' };

const stepsWord = (n: number) => `${n} ${n === 1 ? 'step' : 'steps'}`;

function DefinitionEntry({ swatch, name, count, definition, note, ...data }: { swatch: ReactNode; name: string; count: number | null; definition: string; note?: string } & Record<`data-${string}`, string>) {
  return (
    <li className="flex items-start gap-2" {...data}>
      <span className="mt-px inline-flex shrink-0">{swatch}</span>
      <span className="min-w-0">
        <span className="font-medium text-paper">{name}</span>
        {count !== null && <span className="tabular text-paper-mute"> · {stepsWord(count)}</span>}
        {note && <span className="text-paper-mute"> · {note}</span>}
        <span className="text-paper-dim"> — {definition}</span>
      </span>
    </li>
  );
}

export interface StrategyMapLegendProps {
  /** the economy's step count of each choice (and N, not assessed); omit for a legend without counts */
  counts?: ChoiceLegendCounts | null;
  /** the Mixed entry (a split cell): its count, or true without one; off when 0 or null */
  mixed?: number | boolean | null;
  className?: string;
}

/**
 * The Country Strategy Map's one legend: the colours grouped by fence, each fence headed by its yard in the
 * user's words (vocabulary YARD_AND_FENCE, YARD_DEFINITION) and each choice with its swatch, its step count
 * and one plain line (CHOICE_ACTION); then Not assessed and Mixed (the split cell). Three columns on wide
 * screens (the Medium fence the widest), one on a phone.
 */
export function StrategyMapLegend({ counts, mixed = null, className }: StrategyMapLegendProps) {
  const n = (k: Verb | 'N') => (counts ? (counts[k] ?? 0) : null);
  const group = (f: Fence) => {
    const y = YARD_OF_FENCE[f];
    return (
      <div className="min-w-0" data-fence={f} data-yard={y} data-testid="legend-fence">
        <p className="font-medium text-paper">{YARD_AND_FENCE[y]}</p>
        <p className="mt-0.5 text-paper-mute">{YARD_DEFINITION[y]}</p>
        <ul className="mt-1.5 space-y-1" aria-label={`${FENCE_LABEL[f]}: ${FENCE_VERBS[f].map((v) => VERB_LABEL[v]).join(', ')}`}>
          {FENCE_VERBS[f].map((v) => (
            <DefinitionEntry key={v} data-verb={v} swatch={<Swatch verb={v} size={14} />} name={VERB_LABEL[v]} count={n(v)} note={DEPLOYMENT_ONLY[v]} definition={MAP_CHOICE_DEFINITION[v]} />
          ))}
        </ul>
      </div>
    );
  };
  return (
    <div
      role="group"
      aria-label="Legend: what each color means, by fence"
      className={`grid gap-x-6 gap-y-3 text-xs lg:grid-cols-[minmax(0,1fr)_minmax(0,1.45fr)_minmax(0,1fr)] ${className ?? ''}`}
      data-testid="posture-legend"
    >
      {group('high')}
      {group('medium')}
      <div className="min-w-0 space-y-3">
        {group('low')}
        <ul className="space-y-1 border-t border-white/10 pt-2" aria-label="Other cells">
          <DefinitionEntry data-verb="not_assessed" swatch={<Swatch hatched size={14} />} name="Not assessed" count={n('N')} note="no fence" definition={NOT_ASSESSED_DEFINITION} />
          {mixed ? (
            <DefinitionEntry
              data-verb="mixed"
              data-testid="legend-mixed"
              swatch={<MixedSwatch size={14} />}
              name={MIXED_NAME}
              count={typeof mixed === 'number' ? mixed : null}
              definition={MIXED_DEFINITION}
            />
          ) : null}
        </ul>
      </div>
    </div>
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
