// React hooks over CountryStore: the manifest, one fact sheet, one memo. Each returns a LoadState and a
// retry(); a settled value is read synchronously on the next mount (no flash of "Loading").

import { useCallback, useEffect, useState } from 'react';
import type { CountryEntry, CountryManifest, DriversDoc, FactSheet, MemoDoc, TradeMonthlyDoc } from '../contract.ts';
import { countryStore, CountryLoadError, type LoadState } from './store.ts';

export interface Loaded<T> extends LoadState<T> {
  retry: () => void;
}

function useLoad<T>(key: string | null, load: (() => Promise<T>) | null): Loaded<T> {
  const store = countryStore();
  const initial = (): LoadState<T> => {
    if (!key) return { status: 'loading', data: null, error: null };
    const v = store.peek<T>(key);
    return v !== undefined ? { status: 'ready', data: v, error: null } : { status: 'loading', data: null, error: null };
  };
  const [state, setState] = useState<LoadState<T> & { key: string | null }>(() => ({ ...initial(), key }));
  const [attempt, setAttempt] = useState(0);
  useEffect(() => {
    if (!key || !load) return;
    let alive = true;
    const peeked = store.peek<T>(key);
    if (peeked !== undefined) {
      setState({ status: 'ready', data: peeked, error: null, key });
      return;
    }
    setState({ status: 'loading', data: null, error: null, key });
    load().then(
      (data) => alive && setState({ status: 'ready', data, error: null, key }),
      (e: unknown) =>
        alive &&
        setState({ status: 'error', data: null, error: e instanceof CountryLoadError ? e : new CountryLoadError('NETWORK', String((e as Error)?.message ?? e)), key }),
    );
    return () => {
      alive = false;
    };
    // load is derived from key; attempt re-runs after a failure
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [key, attempt]);
  const retry = useCallback(() => setAttempt((a) => a + 1), []);
  // a key change shows loading until the effect runs (never the previous country's data)
  const s = state.key === key ? state : { ...initial(), key };
  return { status: s.status, data: s.data, error: s.error, retry };
}

export function useCountryManifest(): Loaded<CountryManifest> {
  return useLoad<CountryManifest>('manifest', () => countryStore().manifest());
}

export function useFactSheet(entry: CountryEntry | null): Loaded<FactSheet> {
  const ok = entry && entry.file ? entry : null;
  return useLoad<FactSheet>(ok ? `sheet:${ok.iso3}` : null, ok ? () => countryStore().sheet(ok) : null);
}

export function useMemoDoc(entry: CountryEntry | null): Loaded<MemoDoc> {
  const ok = entry && entry.memo && !entry.memo.stale ? entry : null;
  return useLoad<MemoDoc>(ok ? `memo:${ok.iso3}` : null, ok ? () => countryStore().memo(ok) : null);
}

/** The companion drivers file, loaded only when `needed` (a selected sector whose drivers the sheet does not carry). */
export function useDriversDoc(entry: CountryEntry | null, sheet: FactSheet | null, needed: boolean): Loaded<DriversDoc> {
  const ok = needed && entry && entry.drivers && sheet ? entry : null;
  return useLoad<DriversDoc>(ok ? `drivers:${ok.iso3}` : null, ok ? () => countryStore().driversDoc(ok, sheet ?? undefined) : null);
}

/** The companion monthly trade file (additive 2026-10-19), loaded only when `needed` (the card is near the viewport). */
export function useTradeMonthly(entry: CountryEntry | null, needed: boolean): Loaded<TradeMonthlyDoc> {
  const ok = needed && entry && entry.trade_monthly ? entry : null;
  return useLoad<TradeMonthlyDoc>(ok ? `trade:${ok.iso3}` : null, ok ? () => countryStore().tradeMonthly(ok) : null);
}
