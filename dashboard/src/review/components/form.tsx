// Accessible form primitives for the review pages (docs/QC_SYSTEM_DESIGN.md 5): the app had no textarea or
// form-field primitive. They reuse the design-system classes (.field, .btn, .tag, .label, .checkbox) and
// label every control: Field (label + hint + error, wiring aria-describedby / aria-invalid), TextArea,
// ChoiceGrid (radio cards with an optional colour swatch and definition), ConfidenceInput (1-5),
// Likert4Input (1-4), MultiCheck (with an optional maximum), SeveritySelect (SEVERITY_LABEL) and
// DontKnowToggle. Labels for choices come from the callers (the vocabulary constants), never typed here.

import { useId, type ReactNode } from 'react';
import { SEVERITY_LABEL, type Severity } from '../model.ts';
import { DONT_KNOW_LABEL } from '../flow.ts';

export interface FieldIds {
  id: string;
  describedBy: string | undefined;
  invalid: boolean;
}

/** A labelled control: `children` is the control, or a function given the ids to wire into it. */
export function Field({ label, hint, error, required, children, id: given }: { label: ReactNode; hint?: ReactNode; error?: string | null; required?: boolean; children: ReactNode | ((ids: FieldIds) => ReactNode); id?: string }) {
  const auto = useId();
  const id = given ?? auto;
  const hintId = hint ? `${id}-hint` : undefined;
  const errId = error ? `${id}-error` : undefined;
  const describedBy = [hintId, errId].filter(Boolean).join(' ') || undefined;
  return (
    <div className="mb-4">
      <label htmlFor={id} className="mb-1 block text-sm font-medium text-paper">
        {label}
        {required && (
          <span className="text-paper-mute" aria-hidden="true">
            {' '}
            *
          </span>
        )}
      </label>
      {hint && (
        <p id={hintId} className="mb-1.5 text-xs text-paper-mute">
          {hint}
        </p>
      )}
      {typeof children === 'function' ? children({ id, describedBy, invalid: !!error }) : children}
      {error && (
        <p id={errId} role="alert" className="mt-1 text-xs text-alert">
          {error}
        </p>
      )}
    </div>
  );
}

/** A multi-line text field. */
export function TextArea({
  label,
  value,
  onChange,
  hint,
  error,
  required,
  rows = 4,
  maxLength,
  placeholder,
}: {
  label: ReactNode;
  value: string;
  onChange: (v: string) => void;
  hint?: ReactNode;
  error?: string | null;
  required?: boolean;
  rows?: number;
  maxLength?: number;
  placeholder?: string;
}) {
  return (
    <Field label={label} hint={hint} error={error} required={required}>
      {({ id, describedBy, invalid }) => (
        <textarea
          id={id}
          className="field"
          rows={rows}
          value={value}
          maxLength={maxLength}
          placeholder={placeholder}
          required={required}
          aria-required={required || undefined}
          aria-invalid={invalid || undefined}
          aria-describedby={describedBy}
          onChange={(e) => onChange(e.target.value)}
        />
      )}
    </Field>
  );
}

export interface ChoiceOption {
  value: string;
  label: string;
  definition?: string;
  /** A colour swatch (e.g. VERB_COLOR) shown before the label. */
  color?: string;
}

/** Radio cards in a fieldset; `includeDontKnow` adds the "I don't know" card (value DONT_KNOW from model.ts). */
export function ChoiceGrid({
  legend,
  options,
  value,
  onChange,
  hint,
  error,
  name,
  disabled,
  dontKnowValue,
}: {
  legend: ReactNode;
  options: readonly ChoiceOption[];
  value: string | null;
  onChange: (v: string) => void;
  hint?: ReactNode;
  error?: string | null;
  name?: string;
  disabled?: boolean;
  /** When given, an extra card with DONT_KNOW_LABEL and this value. */
  dontKnowValue?: string;
}) {
  const auto = useId();
  const group = name ?? auto;
  const hintId = hint ? `${group}-hint` : undefined;
  const errId = error ? `${group}-error` : undefined;
  const all: ChoiceOption[] = dontKnowValue ? [...options, { value: dontKnowValue, label: DONT_KNOW_LABEL }] : [...options];
  return (
    <fieldset className="mb-4" aria-describedby={[hintId, errId].filter(Boolean).join(' ') || undefined} aria-invalid={error ? true : undefined} disabled={disabled}>
      <legend className="mb-1 text-sm font-medium text-paper">{legend}</legend>
      {hint && (
        <p id={hintId} className="mb-2 text-xs text-paper-mute">
          {hint}
        </p>
      )}
      <div className="grid gap-2 sm:grid-cols-2">
        {all.map((o) => {
          const id = `${group}-${o.value}`;
          const checked = value === o.value;
          return (
            // `.card` is unlayered (a border utility loses to it): the checked border is inline
            <label key={o.value} htmlFor={id} className="card flex cursor-pointer items-start gap-2.5 p-3" data-checked={checked || undefined} style={checked ? { borderColor: 'var(--color-signal)' } : undefined}>
              <input id={id} type="radio" name={group} value={o.value} checked={checked} onChange={() => onChange(o.value)} className="checkbox mt-0.5" />
              {o.color && <span aria-hidden="true" className="mt-1 inline-block h-3 w-3 flex-none" style={{ background: o.color }} />}
              <span className="text-sm">
                <span className="block text-paper">{o.label}</span>
                {o.definition && <span className="block text-xs text-paper-mute">{o.definition}</span>}
              </span>
            </label>
          );
        })}
      </div>
      {error && (
        <p id={errId} role="alert" className="mt-1 text-xs text-alert">
          {error}
        </p>
      )}
    </fieldset>
  );
}

function Scale<T extends number>({ legend, values, labels, value, onChange, hint, name }: { legend: ReactNode; values: readonly T[]; labels: Readonly<Record<number, string>>; value: T | null; onChange: (v: T) => void; hint?: ReactNode; name?: string }) {
  const auto = useId();
  const group = name ?? auto;
  const hintId = hint ? `${group}-hint` : undefined;
  return (
    <fieldset className="mb-4" aria-describedby={hintId}>
      <legend className="mb-1 text-sm font-medium text-paper">{legend}</legend>
      {hint && (
        <p id={hintId} className="mb-1.5 text-xs text-paper-mute">
          {hint}
        </p>
      )}
      <div className="flex flex-wrap gap-1.5">
        {values.map((v) => {
          const id = `${group}-${v}`;
          return (
            // `.tag` is unlayered (utilities lose to it): the checked border is inline; the radio carries the state
            <label key={v} htmlFor={id} className="tag cursor-pointer has-[:focus-visible]:outline-2 has-[:focus-visible]:outline-offset-2 has-[:focus-visible]:outline-signal" data-checked={value === v || undefined} style={value === v ? { borderColor: 'var(--color-signal)', color: 'var(--color-paper)' } : undefined}>
              <input id={id} type="radio" name={group} value={v} checked={value === v} onChange={() => onChange(v)} className="sr-only" />
              {v} · {labels[v]}
            </label>
          );
        })}
      </div>
    </fieldset>
  );
}

export const CONFIDENCE_LABEL: Readonly<Record<1 | 2 | 3 | 4 | 5, string>> = { 1: 'Guess', 2: 'Low', 3: 'Moderate', 4: 'High', 5: 'Certain' };
export const LIKERT4_LABEL: Readonly<Record<1 | 2 | 3 | 4, string>> = { 1: 'Not at all', 2: 'Somewhat', 3: 'Mostly', 4: 'Fully' };

/** Confidence 1-5. */
export function ConfidenceInput({ legend = 'How confident are you?', value, onChange, hint, name }: { legend?: ReactNode; value: 1 | 2 | 3 | 4 | 5 | null; onChange: (v: 1 | 2 | 3 | 4 | 5) => void; hint?: ReactNode; name?: string }) {
  return <Scale legend={legend} values={[1, 2, 3, 4, 5] as const} labels={CONFIDENCE_LABEL} value={value} onChange={onChange} hint={hint} name={name} />;
}

/** A four-point agreement scale (1 = not at all, 4 = fully). */
export function Likert4Input({ legend, value, onChange, hint, name }: { legend: ReactNode; value: 1 | 2 | 3 | 4 | null; onChange: (v: 1 | 2 | 3 | 4) => void; hint?: ReactNode; name?: string }) {
  return <Scale legend={legend} values={[1, 2, 3, 4] as const} labels={LIKERT4_LABEL} value={value} onChange={onChange} hint={hint} name={name} />;
}

/** Checkboxes; with `max`, unchecked boxes are disabled once `max` are checked. */
export function MultiCheck({
  legend,
  options,
  value,
  onChange,
  max,
  hint,
  error,
}: {
  legend: ReactNode;
  options: readonly { value: string; label: string }[];
  value: readonly string[];
  onChange: (v: string[]) => void;
  max?: number;
  hint?: ReactNode;
  error?: string | null;
}) {
  const group = useId();
  const full = max !== undefined && value.length >= max;
  const hintId = hint ? `${group}-hint` : undefined;
  const errId = error ? `${group}-error` : undefined;
  return (
    <fieldset className="mb-4" aria-describedby={[hintId, errId].filter(Boolean).join(' ') || undefined}>
      <legend className="mb-1 text-sm font-medium text-paper">{legend}</legend>
      {hint && (
        <p id={hintId} className="mb-1.5 text-xs text-paper-mute">
          {hint}
        </p>
      )}
      {options.map((o) => {
        const id = `${group}-${o.value}`;
        const checked = value.includes(o.value);
        return (
          <div key={o.value} className="flex items-center gap-2 py-1">
            <input
              id={id}
              type="checkbox"
              className="checkbox"
              checked={checked}
              disabled={!checked && full}
              onChange={(e) => onChange(e.target.checked ? [...value, o.value] : value.filter((v) => v !== o.value))}
            />
            <label htmlFor={id} className="text-sm text-paper-dim">
              {o.label}
            </label>
          </div>
        );
      })}
      {error && (
        <p id={errId} role="alert" className="mt-1 text-xs text-alert">
          {error}
        </p>
      )}
    </fieldset>
  );
}

const SEVERITIES: Severity[] = ['cosmetic', 'minor', 'material', 'critical'];

/** Severity (SEVERITY_LABEL), as a select. */
export function SeveritySelect({ label = 'How serious is it?', value, onChange, hint, error }: { label?: ReactNode; value: Severity | null; onChange: (v: Severity) => void; hint?: ReactNode; error?: string | null }) {
  return (
    <Field label={label} hint={hint} error={error} required>
      {({ id, describedBy, invalid }) => (
        <select id={id} className="field" value={value ?? ''} aria-describedby={describedBy} aria-invalid={invalid || undefined} onChange={(e) => onChange(e.target.value as Severity)}>
          <option value="" disabled>
            Choose a severity
          </option>
          {SEVERITIES.map((s) => (
            <option key={s} value={s}>
              {SEVERITY_LABEL[s]}
            </option>
          ))}
        </select>
      )}
    </Field>
  );
}

/** The "I don't know / outside my expertise" switch. */
export function DontKnowToggle({ checked, onChange, label = DONT_KNOW_LABEL }: { checked: boolean; onChange: (v: boolean) => void; label?: string }) {
  const id = useId();
  return (
    <div className="flex items-center gap-2 py-1.5">
      <input id={id} type="checkbox" className="checkbox" checked={checked} onChange={(e) => onChange(e.target.checked)} />
      <label htmlFor={id} className="text-sm text-paper-dim">
        {label}
      </label>
    </div>
  );
}
