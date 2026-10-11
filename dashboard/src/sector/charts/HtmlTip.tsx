// A tooltip for HTML marks (share-split segments, heat-table cells, grid tiles), styled as
// PlotFigure's: values first, labels as text, an orange hairline. Shown on pointer move and on
// focus; hidden on leave and blur. Content is React text, never HTML.

import { useCallback, useRef, useState, type ReactNode } from 'react';

export interface TipState {
  x: number;
  y: number;
  content: ReactNode;
}

export function useHtmlTip<T extends HTMLElement = HTMLDivElement>() {
  const hostRef = useRef<T>(null);
  const [tip, setTip] = useState<TipState | null>(null);
  const atPointer = useCallback((e: { clientX: number; clientY: number }, content: ReactNode) => {
    const r = hostRef.current?.getBoundingClientRect();
    if (!r) return;
    setTip({ x: e.clientX - r.left, y: e.clientY - r.top, content });
  }, []);
  const atElement = useCallback((el: Element, content: ReactNode) => {
    const r = hostRef.current?.getBoundingClientRect();
    const b = el.getBoundingClientRect();
    if (!r) return;
    setTip({ x: b.left - r.left + b.width / 2, y: b.top - r.top, content });
  }, []);
  const hide = useCallback(() => setTip(null), []);
  return { hostRef, tip, atPointer, atElement, hide };
}

export function TipBox({ tip, hostWidth }: { tip: TipState | null; hostWidth: number }) {
  if (!tip) return null;
  const flip = tip.x > hostWidth * 0.6;
  return (
    <div
      className="pointer-events-none absolute z-20 max-w-[18rem] border border-signal bg-ink-900 px-3 py-2 text-xs text-paper-dim"
      style={{ left: flip ? undefined : tip.x + 14, right: flip ? hostWidth - tip.x + 14 : undefined, top: Math.max(0, tip.y - 10) }}
      role="status"
    >
      {tip.content}
    </div>
  );
}
