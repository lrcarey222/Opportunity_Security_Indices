// Renders an Observable Plot figure into a ref (spec_react.md 8.7): re-renders on option change and
// on width change (ResizeObserver), removes the old node. Hover: charts add a mark with Plot's
// pointer transform; Plot then sets `figure.value` and fires `input`, and this component shows an
// HTML tooltip (values first, labels as text) at the pointer. Click on a pointed mark calls onSelect.

import { useEffect, useRef, useState, type ReactNode } from 'react';
import * as Plot from '@observablehq/plot';
import { plotBase } from './theme.ts';

export interface PlotFigureProps<T> {
  /** builds the options for a width (memoise it with useCallback in the caller) */
  build: (width: number) => Plot.PlotOptions | null;
  height: number;
  ariaLabel: string;
  tooltip?: (d: T) => ReactNode;
  onSelect?: (d: T) => void;
  /** fixed minimum width (wide heatmaps scroll inside the card) */
  minWidth?: number;
}

type Figure = (SVGSVGElement | HTMLElement) & { value?: unknown };

export function PlotFigure<T>({ build, height, ariaLabel, tooltip, onSelect, minWidth }: PlotFigureProps<T>) {
  const hostRef = useRef<HTMLDivElement>(null);
  const plotRef = useRef<HTMLDivElement>(null);
  const [width, setWidth] = useState(0);
  const [hover, setHover] = useState<{ d: T; x: number; y: number } | null>(null);
  const pos = useRef({ x: 0, y: 0 });
  const selectRef = useRef(onSelect);
  selectRef.current = onSelect;

  useEffect(() => {
    const el = hostRef.current;
    if (!el) return;
    const measure = () => setWidth(Math.max(minWidth ?? 0, Math.floor(el.clientWidth)));
    measure();
    if (typeof ResizeObserver === 'undefined') return;
    const ro = new ResizeObserver(measure);
    ro.observe(el);
    return () => ro.disconnect();
  }, [minWidth]);

  useEffect(() => {
    const target = plotRef.current;
    if (!target || width <= 0) return;
    let opts: Plot.PlotOptions | null = null;
    try {
      opts = build(width);
    } catch (e) {
      console.error(e);
    }
    if (!opts) {
      target.replaceChildren();
      return;
    }
    const base = plotBase(width, height);
    // a vertical y axis with a label draws the label ("↑ thousand barrels per day") in the top margin,
    // on top of the highest tick label at the default 16 px: give it a row of its own
    const y = opts.y as { label?: unknown; axis?: unknown } | undefined;
    if (opts.marginTop === undefined && y && typeof y.label === 'string' && y.label && y.axis !== null) base.marginTop = 30;
    const fig = Plot.plot({ ...base, ...opts, style: { ...(base.style as object), ...((opts.style as object) ?? {}) } }) as Figure;
    fig.setAttribute('aria-label', ariaLabel);
    fig.setAttribute('role', 'img');
    // Plot labels its mark groups (<g aria-label="bar">) and single-element marks (the frame's
    // <rect aria-label="frame">); inside role="img" they are presentational, and ARIA prohibits
    // aria-label on an element without a role (axe: aria-prohibited-attr).
    fig.querySelectorAll('g[aria-label]:not([role]), rect[aria-label]:not([role])').forEach((g) => g.removeAttribute('aria-label'));
    const onInput = () => {
      const v = fig.value as T | null | undefined;
      setHover(v === null || v === undefined ? null : { d: v, x: pos.current.x, y: pos.current.y });
    };
    const onClick = () => {
      const v = fig.value as T | null | undefined;
      if (v !== null && v !== undefined) selectRef.current?.(v);
    };
    fig.addEventListener('input', onInput);
    fig.addEventListener('click', onClick);
    target.replaceChildren(fig);
    setHover(null);
    return () => {
      fig.removeEventListener('input', onInput);
      fig.removeEventListener('click', onClick);
      fig.remove();
    };
  }, [build, width, height, ariaLabel]);

  const onMove = (e: React.PointerEvent) => {
    const r = hostRef.current?.getBoundingClientRect();
    if (!r) return;
    pos.current = { x: e.clientX - r.left, y: e.clientY - r.top };
    if (hover) setHover((h) => (h ? { ...h, x: pos.current.x, y: pos.current.y } : h));
  };

  const hostW = hostRef.current?.clientWidth ?? width;
  const flip = hover ? hover.x > hostW * 0.6 : false;
  return (
    <div
      ref={hostRef}
      className="plot-host relative w-full"
      // minWidth keeps a wide chart at its own width, so it scrolls inside its card (the caller's
      // overflow-x-auto wrapper) instead of Plot's max-width: 100% shrinking it below legibility
      style={{ minHeight: height, minWidth, cursor: onSelect && hover ? 'pointer' : undefined }}
      onPointerMove={onMove}
      onPointerLeave={() => setHover(null)}
    >
      <div ref={plotRef} />
      {hover && tooltip && (
        <div
          className="pointer-events-none absolute z-20 max-w-[18rem] border border-signal bg-ink-900 px-3 py-2 text-xs text-paper-dim shadow-none"
          style={{
            left: flip ? undefined : hover.x + 14,
            right: flip ? hostW - hover.x + 14 : undefined,
            top: Math.max(0, hover.y - 10),
          }}
          role="status"
        >
          {tooltip(hover.d)}
        </div>
      )}
    </div>
  );
}

/** Tooltip body: the value first and strong, then the labels. */
export function Tip({ value, title, lines }: { value?: ReactNode; title?: ReactNode; lines?: ReactNode[] }) {
  return (
    <div className="space-y-0.5 tabular">
      {value !== undefined && <div className="text-sm font-semibold text-paper">{value}</div>}
      {title !== undefined && <div className="text-paper-dim">{title}</div>}
      {lines?.filter((l) => l !== null && l !== undefined && l !== false).map((l, i) => (
        <div key={i} className="text-paper-mute">
          {l}
        </div>
      ))}
    </div>
  );
}
