// InfoTip (spec_country.md C3.3): an "i" button beside a term that opens a small non-modal popover with
// the glossary's plain-language definition and a link to the glossary page. 20 x 20 px icon with a
// 24 px hit area, aria-label "What is “{term}”?", aria-expanded; click, Enter, Space or a tap toggles;
// Escape closes and returns focus to the button; an outside click or focus leaving the popover closes
// it. The popover is position: fixed (so a scrolling table or a card never clips it), below the button,
// flipped above near the bottom of the viewport. Never the only way to reach the text: the glossary
// page has every entry.
//
// Markup is phrasing content only (spans), so a tip may sit inside a sentence. Do not put one inside a
// heading: the button's name would become part of the heading's accessible name (use ChartCard's
// `badge` slot or the sub line instead).

import { useCallback, useEffect, useId, useLayoutEffect, useRef, useState, type CSSProperties } from 'react';
import { Link } from 'react-router';
import { useGlossary } from './glossaryVars.tsx';

export interface InfoTipProps {
  /** a glossary id (content/glossary.ts) */
  id: string;
  /** the term as named on the page (default: the glossary term) */
  term?: string;
  className?: string;
}

const GAP = 6;
const MARGIN = 8;
const MAX_W = 320;

export function InfoTip({ id, term, className }: InfoTipProps) {
  const { byId } = useGlossary();
  const entry = byId.get(id);
  const uid = useId();
  const [open, setOpen] = useState(false);
  const [style, setStyle] = useState<CSSProperties>({ visibility: 'hidden' });
  const wrapRef = useRef<HTMLSpanElement>(null);
  const btnRef = useRef<HTMLButtonElement>(null);
  const popRef = useRef<HTMLSpanElement>(null);

  const place = useCallback(() => {
    const b = btnRef.current?.getBoundingClientRect();
    const p = popRef.current;
    if (!b || !p) return;
    // the narrower of the layout width and the window: on a phone, content wider than the screen widens
    // innerWidth, and a popover placed against it ran off the right edge (render review, 2026-10-31)
    const narrowest = (xs: number[], fallback: number) => {
      const ok = xs.filter((x) => x > 0);
      return ok.length ? Math.min(...ok) : fallback;
    };
    const vw = narrowest([document.documentElement.clientWidth, window.innerWidth], 1024);
    const vh = narrowest([document.documentElement.clientHeight, window.innerHeight], 768);
    const w = Math.min(MAX_W, vw - 2 * MARGIN);
    const h = p.offsetHeight || 0;
    const left = Math.max(MARGIN, Math.min(b.left - 12, vw - w - MARGIN));
    const below = b.bottom + GAP;
    const above = b.top - GAP - h;
    const top = below + h > vh - MARGIN && above >= MARGIN ? above : below;
    setStyle({ position: 'fixed', left, top, width: w, visibility: 'visible' });
  }, []);

  useLayoutEffect(() => {
    if (open) place();
    else setStyle({ visibility: 'hidden' });
  }, [open, place]);

  useEffect(() => {
    if (!open) return;
    const outside = (e: Event) => {
      if (wrapRef.current && e.target instanceof Node && !wrapRef.current.contains(e.target)) setOpen(false);
    };
    const key = (e: KeyboardEvent) => {
      if (e.key === 'Escape') {
        setOpen(false);
        btnRef.current?.focus();
      }
    };
    document.addEventListener('pointerdown', outside);
    document.addEventListener('keydown', key);
    window.addEventListener('resize', place);
    window.addEventListener('scroll', place, true);
    return () => {
      document.removeEventListener('pointerdown', outside);
      document.removeEventListener('keydown', key);
      window.removeEventListener('resize', place);
      window.removeEventListener('scroll', place, true);
    };
  }, [open, place]);

  if (!entry) {
    if (import.meta.env.DEV) console.warn(`InfoTip: no glossary entry '${id}'`);
    return null;
  }
  const name = term ?? entry.term;
  return (
    <span
      ref={wrapRef}
      className={`relative inline-flex align-middle ${className ?? ''}`}
      onBlur={(e) => {
        if (open && wrapRef.current && !(e.relatedTarget instanceof Node && wrapRef.current.contains(e.relatedTarget))) setOpen(false);
      }}
    >
      <button
        ref={btnRef}
        type="button"
        className="infotip-btn"
        // the term quoted, so a plural or a phrase still reads as a question ("What is “yards”?")
        aria-label={`What is “${name}”?`}
        aria-expanded={open}
        aria-controls={open ? `${uid}-pop` : undefined}
        onClick={() => setOpen((o) => !o)}
        data-glossary={id}
      >
        <svg width="14" height="14" viewBox="0 0 14 14" aria-hidden="true" focusable="false">
          <circle cx="7" cy="7" r="6.25" fill="none" stroke="currentColor" strokeWidth="1.5" />
          <rect x="6.25" y="6" width="1.5" height="4.5" fill="currentColor" />
          <rect x="6.25" y="3.4" width="1.5" height="1.5" fill="currentColor" />
        </svg>
      </button>
      {open && (
        <span ref={popRef} id={`${uid}-pop`} role="dialog" aria-labelledby={`${uid}-term`} className="infotip-pop" style={style}>
          <span className="rule-neis-sm block" aria-hidden="true" />
          <span id={`${uid}-term`} className="block text-sm font-medium text-paper">
            {entry.term}
          </span>
          <span className="mt-1 block text-[0.8125rem] leading-snug text-paper-dim">{entry.short}</span>
          <Link className="link mt-2 block text-[0.8125rem]" to={`/glossary#${glossaryAnchor(id)}`} onClick={() => setOpen(false)}>
            More in the glossary <span aria-hidden="true">{'→'}</span>
          </Link>
        </span>
      )}
    </span>
  );
}

/** The anchor of an entry on the glossary page (prefixed so it never collides with a page's own ids). */
export function glossaryAnchor(id: string): string {
  return `term-${id}`;
}
