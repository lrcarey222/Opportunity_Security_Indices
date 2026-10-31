// The supply-chain spine of a strategic compact (2026-10-20): its value-chain steps in order, left to right
// (top to bottom on a phone), each with its nodes as chips coloured by their status under the toggled
// scenario (the five classes as their chips; NOT STRESSED and NO SHOCK as a dashed outline), the status
// printed on every chip so colour is never the only carrier. Each chip is a button: Tab reaches it,
// Enter or Space (or a click) selects the node (onSelect: the page opens its node evidence), the arrow
// keys move between chips along the chain, and the selected chip is aria-pressed. A legend of the statuses
// present follows. The same steps and nodes in every scenario: only the colours change. Data from
// derive/strategic.ts spineOf().

import { useRef, type KeyboardEvent } from 'react';
import { HATCH_STYLE } from '../../theme/palette.ts';
import { fmtShare } from '../derive/format.ts';
import { SPINE_STATUS_ORDER, spineStatusInfo, type SpineNode, type SpineStep } from '../derive/strategic.ts';

/** A status's mark: filled, hollow, hatched (INSUFFICIENT DATA, NOT TESTED) or a dashed outline (NOT STRESSED, NO SHOCK). */
export function SpineStatusSwatch({ status, size = 10 }: { status: string | null; size?: number }) {
  const info = spineStatusInfo(status);
  const style =
    info.mark === 'fill'
      ? { background: info.color }
      : info.mark === 'hollow'
        ? { border: `2px solid ${info.color}` }
        : info.mark === 'dashed'
          ? { border: `1.5px dashed ${info.color}` }
          : { ...HATCH_STYLE, border: '1px solid #9ca3af' };
  return <span aria-hidden="true" className="inline-block shrink-0" style={{ width: size + 4, height: size, boxSizing: 'border-box', ...style }} />;
}

/** The statuses as a legend (`statuses`: those to list, in legend order; default every status). */
export function SpineLegend({ statuses }: { statuses?: readonly (string | null)[] }) {
  const want = statuses ? new Set(statuses.filter((x): x is string => !!x)) : null;
  const shown = SPINE_STATUS_ORDER.filter((s) => !want || want.has(s));
  return (
    <ul className="flex flex-wrap gap-x-4 gap-y-1.5 text-xs text-paper-dim" aria-label="Step status" data-testid="spine-legend">
      {shown.map((s) => (
        <li key={s} className="inline-flex items-center gap-1.5" title={spineStatusInfo(s).meaning}>
          <SpineStatusSwatch status={s} />
          {spineStatusInfo(s).label}
        </li>
      ))}
    </ul>
  );
}

export interface ValueChainSpineProps {
  /** the compact's steps under the scenario (spineOf()) */
  steps: SpineStep[];
  /** what the spine shows, for assistive technology ("Permanent Magnets & Drive Systems under Managed Interdependence") */
  label: string;
  /** the selected node's id (its chip is pressed) */
  selected?: string | null;
  /** a chip clicked, or Enter / Space on it */
  onSelect?: (nodeId: string) => void;
  /** an economy's name (the rank-1 host in a chip's details) */
  name?: (iso3: string) => string;
  /** show the status legend under the spine (default true) */
  legend?: boolean;
}

function chipWords(n: SpineNode, name?: (iso3: string) => string): string {
  const st = spineStatusInfo(n.status);
  const parts = [`${n.label}: ${n.status ? st.label : 'no status in this scenario'}`];
  if (n.coverage !== null) parts.push(`${fmtShare(n.coverage, 0)} met with coordinated trade`);
  if (n.host) parts.push(`best-ranked new-capacity host ${name ? name(n.host) : n.host}`);
  else if (n.ties !== null && n.ties > 1) parts.push(`${n.ties} hosts tie for new capacity`);
  return parts.join('; ');
}

export function ValueChainSpine({ steps, label, selected = null, onSelect, name, legend = true }: ValueChainSpineProps) {
  const root = useRef<HTMLDivElement>(null);
  // the arrow keys move along the chain (every chip in spine order)
  const onKeyDown = (e: KeyboardEvent<HTMLDivElement>) => {
    const keys: Record<string, number> = { ArrowRight: 1, ArrowDown: 1, ArrowLeft: -1, ArrowUp: -1 };
    const step = keys[e.key];
    if (!step && e.key !== 'Home' && e.key !== 'End') return;
    const chips = [...(root.current?.querySelectorAll<HTMLButtonElement>('button[data-node]') ?? [])];
    const i = chips.findIndex((b) => b === document.activeElement);
    if (i < 0) return;
    const next = e.key === 'Home' ? 0 : e.key === 'End' ? chips.length - 1 : Math.min(chips.length - 1, Math.max(0, i + step));
    e.preventDefault();
    chips[next]?.focus();
  };
  const statuses = steps.flatMap((s) => s.nodes.map((n) => n.status));
  return (
    <div className="flex flex-col gap-3" data-testid="value-chain-spine">
      <div ref={root} role="group" aria-label={label} onKeyDown={onKeyDown} className="overflow-x-auto pb-1">
        <ol className="flex min-w-0 flex-col gap-2 sm:flex-row sm:items-stretch sm:gap-0">
          {steps.map((st, i) => (
            <li key={st.order} className="flex min-w-0 flex-col sm:flex-row sm:items-stretch" data-testid="spine-step">
              {i > 0 && (
                <span aria-hidden="true" className="self-center px-1 text-paper-mute sm:px-2">
                  <span className="sm:hidden">↓</span>
                  <span className="hidden sm:inline">→</span>
                </span>
              )}
              <div className="flex min-w-0 flex-col gap-1.5 rounded border hairline p-2 sm:w-44">
                <p className="text-xs font-medium uppercase tracking-wide text-paper-mute" title={st.description ?? undefined}>
                  <span className="sr-only">Step {i + 1}: </span>
                  {st.label}
                </p>
                <ul className="flex flex-col gap-1.5">
                  {st.nodes.map((n) => {
                    const info = spineStatusInfo(n.status);
                    const pressed = selected === n.id;
                    return (
                      <li key={n.id}>
                        <button
                          type="button"
                          data-node={n.id}
                          aria-pressed={pressed}
                          aria-label={chipWords(n, name)}
                          title={`${chipWords(n, name)}. ${n.status ? info.meaning : ''}`.trim()}
                          onClick={() => onSelect?.(n.id)}
                          className={`flex w-full min-w-0 flex-col items-start gap-0.5 rounded border px-2 py-1.5 text-left text-sm ${
                            pressed ? 'border-signal bg-white/10 text-paper' : 'hairline text-paper-dim hover:text-paper'
                          }`}
                          style={{ borderLeft: `4px ${info.mark === 'dashed' ? 'dashed' : 'solid'} ${info.color}` }}
                        >
                          <span className="leading-snug text-paper">{n.label}</span>
                          <span className="inline-flex items-center gap-1.5 text-xs text-paper-mute">
                            <SpineStatusSwatch status={n.status} size={8} />
                            {n.status ? info.label : '–'}
                          </span>
                        </button>
                      </li>
                    );
                  })}
                </ul>
              </div>
            </li>
          ))}
        </ol>
      </div>
      {legend && <SpineLegend statuses={statuses} />}
    </div>
  );
}
