// Draw the Fence: input controls. A judgment question (always with "Unknown"), the three strategy
// paths, and the domestic premium slider. Every control shows where its current value came from.

import { useId, type ReactNode } from 'react';
import { DOMAINS, INPUT_DEFS, optionLabel, UNKNOWN } from '../engine/index.ts';
import type { CategoricalInputId, InputValue, Strategy } from '../engine/index.ts';
import { InputTip } from './InfoTip.tsx';
import { ProvBadge } from './kit.tsx';

export function Question({
  id, iv, baseline, onChange, hint, compact,
}: {
  id: CategoricalInputId;
  iv: InputValue<string | number>;
  baseline: InputValue<string | number>;
  /** a value, 'unknown', or undefined to return to the baseline (data or curated assessment) */
  onChange: (v: string | undefined) => void;
  hint?: ReactNode;
  compact?: boolean;
}) {
  const def = INPUT_DEFS[id];
  const name = useId();
  const helpId = useId();
  const opts = [...(DOMAINS[id] as readonly string[])];
  // questions read best from the open end on the left for consequence/time; keep domain order
  const value = String(iv.value);
  const overridden = iv.provenance === 'judgment' || (iv.provenance === 'unknown' && baseline.provenance !== 'unknown');
  return (
    <fieldset className={`min-w-0 ${compact ? 'py-3' : 'py-4'} border-b hairline last:border-b-0`} aria-describedby={def.help ? helpId : undefined}>
      <legend className="sr-only">{def.question}</legend>
      <div className="flex flex-wrap items-start justify-between gap-x-4 gap-y-2">
        <p className="max-w-3xl text-[0.9375rem] font-medium leading-snug text-paper">
          <span aria-hidden="true">{def.question}</span>
          <InputTip id={id} />
        </p>
        <ProvBadge p={iv.provenance} />
      </div>
      {def.help && (
        <p id={helpId} className="mt-1 text-sm text-paper-mute">
          {def.help}
        </p>
      )}
      {hint && <div className="mt-2 text-sm text-paper-dim">{hint}</div>}
      <div className="mt-3 flex flex-wrap gap-2" role="radiogroup" aria-label={def.label}>
        {[...opts, UNKNOWN].map((o) => (
          <label key={o} className="yf-choice min-w-[7.5rem] text-sm text-paper">
            <input type="radio" name={name} value={o} checked={value === o} onChange={() => onChange(o)} />
            <span>{o === UNKNOWN ? 'Unknown' : optionLabel(id, o)}</span>
          </label>
        ))}
      </div>
      <div className="mt-2 flex flex-wrap items-center gap-x-3 gap-y-1 text-xs text-paper-mute">
        {baseline.provenance === 'assessment' && (
          <span>
            Curated assessment: <span className="text-paper-dim">{optionLabel(id, baseline.value)}</span>
            {baseline.assessmentConfidence && <> · {baseline.assessmentConfidence} confidence</>}
          </span>
        )}
        {baseline.provenance === 'unknown' && iv.provenance !== 'judgment' && <span>No curated value: evidence is insufficient.</span>}
        {overridden && (
          <button type="button" className="link text-xs" onClick={() => onChange(undefined)}>
            {baseline.provenance === 'assessment' ? 'Return to the curated assessment' : 'Clear my answer'}
          </button>
        )}
      </div>
    </fieldset>
  );
}

const PATHS: { id: Strategy; title: string; text: string }[] = [
  { id: 'preserve', title: 'Preserve', text: 'Protect or deepen an existing competitive capability.' },
  { id: 'catchUp', title: 'Catch up', text: 'Build toward the current technological or production frontier.' },
  { id: 'leapAhead', title: 'Leap ahead', text: 'Back a successor technology or production method that could change the basis of competition.' },
];

export function PathCards({ value, onChange, notes }: { value: Strategy[]; onChange: (v: Strategy[]) => void; notes?: Partial<Record<Strategy, ReactNode>> }) {
  const toggle = (s: Strategy) => onChange(value.includes(s) ? value.filter((x) => x !== s) : [...value, s]);
  return (
    <fieldset>
      <legend className="sr-only">Strategy: preserve, catch up or leap ahead (choose any combination)</legend>
      <div className="grid gap-3 md:grid-cols-3">
        {PATHS.map((p) => {
          const on = value.includes(p.id);
          return (
            <label key={p.id} className={`yf-choice flex-col p-5 ${on ? '' : ''}`}>
              <span className="flex items-center gap-2">
                <input type="checkbox" checked={on} onChange={() => toggle(p.id)} />
                <span className="text-lg font-semibold uppercase tracking-[0.1em] text-paper">{p.title}</span>
              </span>
              <span className="mt-2 text-sm leading-relaxed text-paper-dim">{p.text}</span>
              {notes?.[p.id] && <span className="mt-3 border-t hairline pt-2 text-xs leading-relaxed text-paper-mute">{notes[p.id]}</span>}
            </label>
          );
        })}
      </div>
      <p className="mt-2 text-sm text-paper-mute">
        {value.length === 0
          ? 'No path chosen yet. Paths can be combined.'
          : `Chosen: ${value.map((v) => PATHS.find((p) => p.id === v)?.title).join(' + ')}`}
      </p>
    </fieldset>
  );
}

export function PremiumSlider({
  iv, onChange, label = 'Domestic price premium',
}: {
  iv: InputValue<string | number>;
  onChange: (v: number | 'unknown' | undefined) => void;
  label?: string;
}) {
  const id = useId();
  const known = iv.value !== UNKNOWN;
  const v = known ? Number(iv.value) : 0;
  return (
    <div>
      <div className="flex flex-wrap items-center justify-between gap-3">
        <span className="flex items-center">
          <label htmlFor={id} className="label mb-0">
            {label}
          </label>
          <InputTip id="premium" />
        </span>
        <span className="flex items-center gap-2">
          <span className="tabular text-2xl font-medium text-paper" aria-live="polite">
            {known ? `${Math.round(v)}%` : 'Unknown'}
          </span>
          <ProvBadge p={iv.provenance} />
        </span>
      </div>
      <input
        id={id}
        type="range"
        className="slider mt-3 w-full"
        min={0}
        max={75}
        step={1}
        value={v}
        style={{ ['--fill' as string]: `${(100 * v) / 75}%` }}
        aria-valuetext={known ? `${Math.round(v)} percent` : 'Unknown'}
        onChange={(e) => onChange(Number(e.target.value))}
      />
      <div className="mt-1 flex justify-between text-xs text-paper-mute" aria-hidden="true">
        <span>0%</span>
        <span>10%</span>
        <span>30%</span>
        <span>75%</span>
      </div>
      <div className="mt-2 flex gap-3 text-xs">
        {known && (
          <button type="button" className="link" onClick={() => onChange('unknown')}>
            Set to Unknown
          </button>
        )}
        {iv.provenance !== 'unknown' && (
          <button type="button" className="link" onClick={() => onChange(undefined)}>
            Clear
          </button>
        )}
      </div>
    </div>
  );
}

/** Directional consequences of a premium. No elasticities are claimed: directions only. */
export function PremiumConsequences({ premium }: { premium: number | null }) {
  const p = premium ?? 0;
  const rows: [string, string, string][] = [
    ['Domestic production incentive', p > 0 ? '↑' : '→', p >= 30 ? 'strong' : p >= 10 ? 'moderate' : p > 0 ? 'small' : 'none'],
    ['Consumer and developer cost', p > 0 ? '↑' : '→', p >= 30 ? 'large' : p >= 10 ? 'noticeable' : p > 0 ? 'small' : 'none'],
    ['Deployment affordability', p > 0 ? '↓' : '→', p >= 30 ? 'materially lower' : p > 0 ? 'lower' : 'unchanged'],
    ['Supply diversification', p > 0 ? '↑?' : '→', p > 0 ? 'may rise, if new supply is not itself concentrated' : 'unchanged'],
  ];
  return (
    <table className="w-full text-sm">
      <caption className="sr-only">Directional consequences of the premium</caption>
      <tbody>
        {rows.map(([k, arrow, txt]) => (
          <tr key={k} className="border-b hairline last:border-b-0">
            <th scope="row" className="py-2 pr-3 text-left font-normal text-paper-dim">
              {k}
            </th>
            <td className="w-10 py-2 text-center text-lg text-paper" aria-hidden="true">
              {arrow}
            </td>
            <td className="py-2 text-paper-mute">{txt}</td>
          </tr>
        ))}
      </tbody>
    </table>
  );
}
