// A small "i" button that explains a term in plain language. It opens on hover, on keyboard focus
// and on tap (touch screens), closes on Escape, blur, scroll or a tap elsewhere, and is fixed-positioned
// in a portal on <body>, so neither a scrolling table nor an animated (transformed) ancestor can clip or
// shift it. Screen readers get the text through aria-describedby.

import { useCallback, useEffect, useId, useRef, useState, type ReactNode } from 'react';
import { createPortal } from 'react-dom';
import { explainOf, INPUT_DEFS } from '../engine/index.ts';
import type { InputId } from '../engine/index.ts';

const WIDTH = 320;

export function InfoTip({ label, children, className = '' }: { label: string; children: ReactNode; className?: string }) {
  const id = useId();
  const btn = useRef<HTMLButtonElement>(null);
  const [pos, setPos] = useState<{ top: number; left: number; width: number } | null>(null);

  const open = useCallback(() => {
    const b = btn.current?.getBoundingClientRect();
    if (!b) return;
    const width = Math.min(WIDTH, window.innerWidth - 16);
    const left = Math.max(8, Math.min(b.left + b.width / 2 - width / 2, window.innerWidth - width - 8));
    const below = b.bottom + 8;
    setPos({ top: below, left, width });
  }, []);
  const close = useCallback(() => setPos(null), []);

  useEffect(() => {
    if (!pos) return;
    const onKey = (e: KeyboardEvent) => e.key === 'Escape' && close();
    // a tap anywhere else closes it (touch browsers do not always move focus off a tapped button)
    const onDown = (e: PointerEvent) => !btn.current?.contains(e.target as Node) && close();
    window.addEventListener('keydown', onKey);
    window.addEventListener('pointerdown', onDown);
    window.addEventListener('scroll', close, true);
    window.addEventListener('resize', close);
    return () => {
      window.removeEventListener('keydown', onKey);
      window.removeEventListener('pointerdown', onDown);
      window.removeEventListener('scroll', close, true);
      window.removeEventListener('resize', close);
    };
  }, [pos, close]);

  // a tooltip that would run off the bottom opens above the button instead
  const tip = useRef<HTMLSpanElement>(null);
  useEffect(() => {
    const t = tip.current;
    const b = btn.current?.getBoundingClientRect();
    if (!t || !b || !pos) return;
    const h = t.getBoundingClientRect().height;
    if (pos.top + h > window.innerHeight - 8 && b.top - h - 8 > 8) setPos({ ...pos, top: b.top - h - 8 });
  }, [pos]);

  return (
    <span className={`yf-no-print inline-flex align-middle ${className}`}>
      <button
        ref={btn}
        type="button"
        className="yf-info"
        aria-label={`What is “${label}”?`}
        aria-describedby={pos ? id : undefined}
        aria-expanded={!!pos}
        onMouseEnter={open}
        onMouseLeave={close}
        onFocus={open}
        onBlur={close}
        // a click or tap arrives after hover and focus have opened it: it only ever opens (a toggle
        // would close it again at once); Escape, blur, leaving or tapping elsewhere closes it
        onClick={(e) => {
          e.preventDefault();
          e.stopPropagation();
          open();
        }}
      >
        <span aria-hidden="true">i</span>
      </button>
      {pos &&
        createPortal(
          <span ref={tip} id={id} role="tooltip" className="yf-tip yf-no-print" style={{ top: pos.top, left: pos.left, width: pos.width }}>
            <span className="mb-1 block text-xs font-semibold uppercase tracking-wider text-paper">{label}</span>
            {children}
          </span>,
          document.body,
        )}
    </span>
  );
}

/** The tooltip for one of the engine's assumptions. */
export function InputTip({ id, className }: { id: InputId; className?: string }) {
  return (
    <InfoTip label={INPUT_DEFS[id].label} className={className}>
      {explainOf(id)}
    </InfoTip>
  );
}
