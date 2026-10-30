// Small NEIS-styled building blocks (spec_react.md 8.8): PageHead, Select, Segmented, Switch,
// Notice, ProgressBar, EmptyState, StatTile, RankChip, Tag, Disclosure, FilterRow, Field.

import { useId, type ReactNode } from 'react';
import { fmtRankChange, isNum } from '../lib/format.ts';

export function PageHead({ eyebrow, title, lede, wideLede, children }: { eyebrow: string; title: string; lede: ReactNode; wideLede?: boolean; children?: ReactNode }) {
  return (
    <header className="mb-6">
      <div className="eyebrow mb-3">{eyebrow}</div>
      <h1 className="rule-neis text-3xl font-medium text-paper lg:text-4xl">{title}</h1>
      <p className={`mt-3 ${wideLede ? 'max-w-5xl' : 'max-w-3xl'} text-[0.9375rem] leading-relaxed text-paper-dim`}>{lede}</p>
      {children}
    </header>
  );
}

/** One left-aligned row of filters above everything it scopes. */
export function FilterRow({ children, result }: { children: ReactNode; result?: ReactNode }) {
  return (
    <div className="mb-6 flex flex-wrap items-end gap-x-5 gap-y-3 border-b hairline pb-5">
      {children}
      {result && <div className="ml-auto text-sm text-paper-mute">{result}</div>}
    </div>
  );
}

export interface Option {
  value: string;
  label: string;
  disabled?: boolean;
  group?: string;
}

export function Select({
  label, value, options, onChange, disabled, className, hideLabel, id: idIn, title,
}: {
  label: string;
  value: string;
  options: Option[];
  onChange: (v: string) => void;
  disabled?: boolean;
  className?: string;
  hideLabel?: boolean;
  id?: string;
  title?: string;
}) {
  const auto = useId();
  const id = idIn ?? auto;
  // options without a group before the first grouped one (an "All ..." choice) stay outside the groups
  const firstGrouped = options.findIndex((o) => o.group !== undefined);
  const lead = firstGrouped < 0 ? [] : options.slice(0, firstGrouped);
  const rest = firstGrouped < 0 ? options : options.slice(firstGrouped);
  const groups = [...new Set(rest.map((o) => o.group ?? ''))];
  const opt = (o: Option) => (
    <option key={o.value} value={o.value} disabled={o.disabled}>
      {o.label}
    </option>
  );
  return (
    <div className={className ?? 'min-w-[11rem]'}>
      <label htmlFor={id} className={hideLabel ? 'sr-only' : 'label'}>
        {label}
      </label>
      <select id={id} className="field field-sm" value={value} onChange={(e) => onChange(e.target.value)} disabled={disabled} title={title}>
        {lead.map(opt)}
        {groups.length > 1 || groups[0]
          ? groups.map((g) => (
              <optgroup key={g} label={g || 'Other'}>
                {rest.filter((o) => (o.group ?? '') === g).map(opt)}
              </optgroup>
            ))
          : rest.map(opt)}
      </select>
    </div>
  );
}

/** A radio group drawn as a segmented control. */
export function Segmented<T extends string>({
  label, value, options, onChange, disabled, hideLabel, title, name: nameIn,
}: {
  label: string;
  value: T;
  options: { value: T; label: string; disabled?: boolean; title?: string }[];
  onChange: (v: T) => void;
  disabled?: boolean;
  hideLabel?: boolean;
  title?: string;
  name?: string;
}) {
  const auto = useId();
  const name = nameIn ?? auto;
  return (
    <fieldset className="min-w-0" title={title}>
      <legend className={hideLabel ? 'sr-only' : 'label'}>{label}</legend>
      <div className="segmented" role="radiogroup" aria-label={label}>
        {options.map((o) => (
          <label key={o.value} title={o.title}>
            <input
              type="radio"
              name={name}
              value={o.value}
              checked={value === o.value}
              disabled={disabled || o.disabled}
              onChange={() => onChange(o.value)}
            />
            {o.label}
          </label>
        ))}
      </div>
    </fieldset>
  );
}

export function Switch({ label, checked, onChange, disabled, hint }: { label: string; checked: boolean; onChange: (v: boolean) => void; disabled?: boolean; hint?: string }) {
  const id = useId();
  return (
    <div className="flex items-start gap-2.5">
      <input
        id={id}
        type="checkbox"
        role="switch"
        aria-checked={checked}
        className="switch mt-0.5"
        checked={checked}
        disabled={disabled}
        onChange={(e) => onChange(e.target.checked)}
      />
      <label htmlFor={id} className="text-sm text-paper-dim">
        {label}
        {hint && <span className="block text-xs text-paper-mute">{hint}</span>}
      </label>
    </div>
  );
}

export function Checkbox({ label, checked, onChange, disabled }: { label: string; checked: boolean; onChange: (v: boolean) => void; disabled?: boolean }) {
  const id = useId();
  return (
    <div className="flex items-center gap-2 py-1.5">
      <input id={id} type="checkbox" className="checkbox" checked={checked} disabled={disabled} onChange={(e) => onChange(e.target.checked)} />
      <label htmlFor={id} className="text-sm text-paper-dim">
        {label}
      </label>
    </div>
  );
}

export function Notice({
  kind = 'info', title, children, onDismiss, action,
}: {
  kind?: 'info' | 'warning' | 'error';
  title?: string;
  children: ReactNode;
  onDismiss?: () => void;
  action?: ReactNode;
}) {
  const edge = kind === 'error' ? 'border-l-alert' : kind === 'warning' ? 'border-l-signal' : 'border-l-ink-500';
  const word = kind === 'error' ? 'Error' : kind === 'warning' ? 'Note' : 'Info';
  return (
    <div className={`panel flex items-start gap-3 border-l-2 ${edge} px-4 py-3 text-sm text-paper-dim`} role={kind === 'error' ? 'alert' : 'status'}>
      <span className="eyebrow mt-0.5 shrink-0">{word}</span>
      <div className="min-w-0 flex-1">
        {title && <div className="font-medium text-paper">{title}</div>}
        <div>{children}</div>
      </div>
      {action}
      {onDismiss && (
        <button type="button" className="btn-quiet shrink-0" onClick={onDismiss} aria-label="Dismiss">
          Dismiss
        </button>
      )}
    </div>
  );
}

export function ProgressBar({ value, max, label }: { value: number; max: number; label: string }) {
  const pct = max > 0 ? Math.max(0, Math.min(100, (100 * value) / max)) : 0;
  return (
    <div className="w-full">
      <div className="mb-1 flex justify-between text-xs text-paper-mute tabular">
        <span>{label}</span>
        <span>{Math.round(pct)}%</span>
      </div>
      <div className="h-1 w-full bg-ink-700" role="progressbar" aria-valuemin={0} aria-valuemax={max} aria-valuenow={value} aria-label={label}>
        <div className="h-1 bg-signal transition-[width] duration-300" style={{ width: `${pct}%` }} />
      </div>
    </div>
  );
}

export function EmptyState({ children }: { children: ReactNode }) {
  return <div className="flex min-h-[8rem] items-center justify-center px-4 py-8 text-center text-sm text-paper-mute">{children}</div>;
}

export function Loading({ label }: { label: string }) {
  return (
    <div className="flex min-h-[8rem] items-center justify-center px-4 py-8 text-sm text-paper-mute" role="status" aria-live="polite">
      <span className="mr-2 inline-block h-2 w-2 animate-pulse bg-signal" aria-hidden="true" />
      {label}
    </div>
  );
}

export function StatTile({ label, value, sub, chip }: { label: string; value: ReactNode; sub?: ReactNode; chip?: ReactNode }) {
  return (
    <div className="panel px-4 py-3">
      <div className="eyebrow">{label}</div>
      <div className="mt-1.5 text-xl font-medium text-paper">{value}</div>
      {(sub || chip) && (
        <div className="mt-1 flex flex-wrap items-center gap-2 text-xs text-paper-mute">
          {chip}
          {sub}
        </div>
      )}
    </div>
  );
}

/** Rank change chip: arrow + number + words (colour never alone). up = rank_base - rank > 0. */
export function RankChip({ change, isNew }: { change: number | null | undefined; isNew?: boolean }) {
  if (isNew) return <span className="tag">new</span>;
  if (!isNum(change)) return null;
  if (change === 0) return <span className="tag text-paper-mute">no change</span>;
  const up = change > 0;
  return (
    <span className="tag tabular" style={{ borderColor: up ? 'rgb(63 165 205 / 0.6)' : 'rgb(226 87 76 / 0.6)' }}>
      {fmtRankChange(change)}
    </span>
  );
}

export function Tag({ children, accent, title }: { children: ReactNode; accent?: boolean; title?: string }) {
  return (
    <span className={accent ? 'tag border-signal text-paper' : 'tag'} title={title}>
      {children}
    </span>
  );
}

export function Disclosure({ summary, children, defaultOpen, badge }: { summary: ReactNode; children: ReactNode; defaultOpen?: boolean; badge?: ReactNode }) {
  return (
    <details className="disclosure border-t hairline" open={defaultOpen}>
      <summary className="flex items-center gap-2 py-2.5 text-sm font-medium text-paper-dim hover:text-paper">
        <span className="disclosure-chevron text-paper-mute" aria-hidden="true">
          ›
        </span>
        <span className="flex-1">{summary}</span>
        {badge}
      </summary>
      <div className="pb-3 pl-4">{children}</div>
    </details>
  );
}

/** "changed" marker: an orange dot plus the word (colour not alone). */
export function ChangedDot() {
  return (
    <span className="inline-flex items-center gap-1 text-xs text-paper-mute">
      <span className="inline-block h-2 w-2 rounded-full bg-signal" aria-hidden="true" />
      changed
    </span>
  );
}

export function KeyValue({ rows }: { rows: [string, ReactNode][] }) {
  return (
    <table className="data-table">
      <tbody>
        {rows.map(([k, v]) => (
          <tr key={k}>
            <td className="w-1/2 text-paper-mute">{k}</td>
            <td>{v}</td>
          </tr>
        ))}
      </tbody>
    </table>
  );
}
