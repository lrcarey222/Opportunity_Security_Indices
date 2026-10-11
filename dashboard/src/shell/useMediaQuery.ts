// A media query as React state. Lives in the shell (no engine imports) so the header can use it;
// src/state/nav.ts re-exports it for the Strategic views.

import { useEffect, useState } from 'react';

/** A media query as state (true in environments without matchMedia). */
export function useMediaQuery(q: string): boolean {
  const get = () => (typeof window !== 'undefined' && typeof window.matchMedia === 'function' ? window.matchMedia(q).matches : true);
  const [m, setM] = useState(get);
  useEffect(() => {
    if (typeof window === 'undefined' || typeof window.matchMedia !== 'function') return;
    const mq = window.matchMedia(q);
    const on = () => setM(mq.matches);
    on();
    mq.addEventListener('change', on);
    return () => mq.removeEventListener('change', on);
  }, [q]);
  return m;
}
