// Prefetches (spec_sector.md 2.1). Neither blocks anything and a failure is silent.
//
//   * prefetchStrategic(): on pointer-enter, focus or touchstart on the Strategic card or the
//     STRATEGIC INSIGHTS tab, load the StrategicLayout chunk and start the engine worker. The
//     engine is never prefetched on idle.
//   * onIdle(): requestIdleCallback with a 1.5 s fallback; returns a cancel function.
//   * saveData(): the reader asked the browser to save data (no idle prefetch then).

let strategic: Promise<void> | null = null;

export function prefetchStrategic(): void {
  if (strategic) return;
  strategic = import('./StrategicLayout.tsx')
    .then(() => import('../state/EngineProvider.tsx'))
    .then((m) => {
      m.ensureEngineClient();
    })
    .catch(() => {
      strategic = null;
    });
}

type IdleWindow = Window & {
  requestIdleCallback?: (cb: () => void, opts?: { timeout: number }) => number;
  cancelIdleCallback?: (id: number) => void;
};

export function onIdle(fn: () => void, fallbackMs = 1500): () => void {
  const w = (typeof window !== 'undefined' ? window : undefined) as IdleWindow | undefined;
  if (w && typeof w.requestIdleCallback === 'function') {
    const id = w.requestIdleCallback(fn, { timeout: 5000 });
    return () => w.cancelIdleCallback?.(id);
  }
  const t = setTimeout(fn, fallbackMs);
  return () => clearTimeout(t);
}

export function saveData(): boolean {
  const n = (typeof navigator !== 'undefined' ? navigator : undefined) as (Navigator & { connection?: { saveData?: boolean } }) | undefined;
  return n?.connection?.saveData === true;
}
