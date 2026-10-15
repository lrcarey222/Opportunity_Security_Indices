// The component stack: a system drawn as its layers, each with its own yard and its own fence. The
// fence column on the right reads as one fence line whose height follows the diagnosis: tall around
// the control layer, gated around capabilities, low around commodities. The point of the picture is
// that the fence surrounds the vulnerability, not automatically the whole product.

import { YARD_NAME } from '../engine/index.ts';
import type { FenceShape, Yard, YardOutcome, YardResult } from '../engine/index.ts';
import { YARD_COPY, YardGlyph, yardClass } from './kit.tsx';

export interface StackLayer {
  key: string;
  label: string;
  layer: string;
  result: YardResult;
  changed?: boolean;
}

const LAYER_LABEL: Record<string, string> = {
  control: 'Control layer',
  hardware: 'Hardware',
  material: 'Material',
  fuel: 'Fuel',
  structure: 'Passive structure',
  stage: 'Whole stage',
};

function FenceSegment({ yard }: { yard: YardOutcome }) {
  const h = yard === 'small' ? 36 : yard === 'medium' ? 22 : yard === 'large' ? 9 : 18;
  const color = yard === 'small' || yard === 'medium' || yard === 'large' ? `var(--yard-${yard})` : 'var(--color-paper-mute)';
  const posts = [6, 26, 46, 66, 86];
  const top = 44 - h;
  return (
    <svg viewBox="0 0 92 46" className="yf-grow h-11 w-24" aria-hidden="true">
      <line x1="0" y1="45" x2="92" y2="45" stroke="var(--hairline)" />
      {posts.map((x) => (
        <line key={x} x1={x} y1={45} x2={x} y2={top} stroke={color} strokeWidth="2.5" strokeDasharray={yard === 'indeterminate' || yard === 'mixed' ? '3 3' : undefined} />
      ))}
      {yard === 'medium' ? (
        <>
          <line x1="6" y1={top + 5} x2="36" y2={top + 5} stroke={color} strokeWidth="2" />
          <line x1="56" y1={top + 5} x2="86" y2={top + 5} stroke={color} strokeWidth="2" />
        </>
      ) : yard !== 'large' ? (
        <>
          <line x1="6" y1={top + 4} x2="86" y2={top + 4} stroke={color} strokeWidth="2" />
          {yard === 'small' && <line x1="6" y1={top + 14} x2="86" y2={top + 14} stroke={color} strokeWidth="2" />}
        </>
      ) : (
        <line x1="6" y1={top + 2} x2="86" y2={top + 2} stroke={color} strokeWidth="1.5" />
      )}
    </svg>
  );
}

export function ComponentStack({
  title, layers, focusKey, onFocus,
}: {
  title: string;
  layers: StackLayer[];
  focusKey?: string;
  onFocus?: (key: string) => void;
}) {
  const groups: { yard: YardOutcome; items: StackLayer[] }[] = (['small', 'medium', 'large', 'indeterminate'] as YardOutcome[])
    .map((y) => ({ yard: y, items: layers.filter((l) => l.result.yard === y) }))
    .filter((g) => g.items.length);
  return (
    <div className="grid gap-6 lg:grid-cols-[minmax(0,3fr)_minmax(0,2fr)]">
      <figure className="panel p-4" aria-label={`${title}: layers and their yards`}>
        <figcaption className="eyebrow mb-3">{title} · top to bottom, from control to passive structure</figcaption>
        <ol className="space-y-1.5">
          {layers.map((l) => {
            const y = l.result.yard;
            const focused = l.key === focusKey;
            const inner = (
              <>
                <span className="w-9 shrink-0">
                  <YardGlyph yard={y} className="h-6 w-9" />
                </span>
                <span className="min-w-0 flex-1 text-left">
                  <span className="block text-sm font-medium text-paper">{l.label}</span>
                  <span className="block text-xs text-paper-mute">
                    {LAYER_LABEL[l.layer] ?? l.layer} · <span className="yf-yard-text font-semibold uppercase tracking-wider">{YARD_NAME[y]}</span>
                    {y === 'small' && l.result.scope === 'control-layer' && ' · control layer only'}
                    {y === 'indeterminate' && ` · ${l.result.possible.map((p) => YARD_NAME[p]).join(' or ')}`}
                  </span>
                </span>
                <FenceSegment yard={y} />
              </>
            );
            return (
              <li key={l.key} className={`${yardClass(y)} yf-layer yf-yard-edge yf-yard-fill ${focused ? 'outline outline-2 outline-signal' : ''} ${l.changed ? 'yf-pulse' : ''}`}>
                {onFocus ? (
                  <button type="button" className="flex w-full items-center gap-3 px-3 py-2" aria-pressed={focused} onClick={() => onFocus(l.key)}>
                    {inner}
                  </button>
                ) : (
                  <div className="flex w-full items-center gap-3 px-3 py-2">{inner}</div>
                )}
              </li>
            );
          })}
        </ol>
      </figure>
      <div className="space-y-4">
        {groups.map((g) => (
          <section key={g.yard} className={`${yardClass(g.yard)} yf-yard-edge pl-4`}>
            <h3 className="yf-yard-text text-sm font-semibold uppercase tracking-[0.14em]">{YARD_COPY[g.yard].name}</h3>
            <p className="text-xs text-paper-mute">{YARD_COPY[g.yard].fence}</p>
            <p className="mt-1 text-sm text-paper">{g.items.map((i) => i.label).join(' · ')}</p>
          </section>
        ))}
      </div>
    </div>
  );
}

const HEIGHT = ['No fence', 'Low', 'Medium', 'High'];

/** Two fences side by side: the one the diagnosis calls for, and the one the package builds. */
export function FenceDiagram({ diagnosed, chosen, yard }: { diagnosed: FenceShape; chosen: FenceShape; yard: Yard | 'mixed' | 'indeterminate' }) {
  const draw = (f: FenceShape, color: string, label: string) => {
    const w = f.breadth === 'narrow' ? 90 : 200;
    const x0 = (220 - w) / 2;
    const h = f.height === 0 ? 0 : 14 + f.height * 18;
    const posts = Math.max(3, Math.round(w / 22));
    const gaps = f.gates;
    const top = 100 - h;
    return (
      <figure className="flex-1">
        <svg viewBox="0 0 220 112" className="w-full" role="img" aria-label={`${label}: ${HEIGHT[f.height]} fence, around ${f.breadth === 'narrow' ? 'one layer' : 'the whole product'}, ${gaps === 0 ? 'no gates' : `${gaps} of 3 gates open`}`}>
          <rect x={x0} y="100" width={w} height="8" fill={color} opacity="0.25" />
          <line x1="4" y1="100" x2="216" y2="100" stroke="var(--hairline-strong)" />
          {h > 0 &&
            Array.from({ length: posts }, (_, i) => x0 + (i * w) / (posts - 1)).map((x, i) => (
              <line key={i} x1={x} y1="100" x2={x} y2={top} stroke={color} strokeWidth="3" />
            ))}
          {h > 0 &&
            Array.from({ length: posts - 1 }, (_, i) => i).map((i) => {
              const openCount = Math.round((gaps / 3) * Math.floor((posts - 1) / 2));
              const stride = openCount > 0 ? Math.floor((posts - 1) / openCount) : 0;
              const open = stride > 0 && i % stride === Math.floor(stride / 2);
              if (open) return null;
              const x1 = x0 + (i * w) / (posts - 1);
              const x2 = x0 + ((i + 1) * w) / (posts - 1);
              return <line key={i} x1={x1} y1={top + 6} x2={x2} y2={top + 6} stroke={color} strokeWidth="2.5" />;
            })}
        </svg>
        <figcaption className="mt-1 text-center text-sm">
          <span className="block font-medium text-paper">{label}</span>
          <span className="block text-xs text-paper-mute">
            {HEIGHT[f.height]} · {f.breadth === 'narrow' ? 'around one layer or input' : 'around the whole product'} · {gaps === 0 ? 'closed' : gaps >= 3 ? 'open gates' : 'some gates'}
          </span>
        </figcaption>
      </figure>
    );
  };
  const c = yard === 'small' || yard === 'medium' || yard === 'large' ? `var(--yard-${yard})` : 'var(--color-paper-mute)';
  return (
    <div className="flex flex-col gap-6 sm:flex-row">
      {draw(diagnosed, c, 'The diagnosis calls for')}
      {draw(chosen, 'var(--color-signal)', 'Your package builds')}
    </div>
  );
}
