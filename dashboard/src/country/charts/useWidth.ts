// The width of an element, tracked with ResizeObserver (0 before the first measure). Country-owned copy
// so the profile chunk does not import the world map module for it.

import { useEffect, useRef, useState } from 'react';

export function useWidth<T extends HTMLElement>(): [React.RefObject<T | null>, number] {
  const ref = useRef<T>(null);
  const [w, setW] = useState(0);
  useEffect(() => {
    const el = ref.current;
    if (!el) return;
    const m = () => setW(Math.floor(el.clientWidth));
    m();
    if (typeof ResizeObserver === 'undefined') return;
    const ro = new ResizeObserver(m);
    ro.observe(el);
    return () => ro.disconnect();
  }, []);
  return [ref, w];
}
