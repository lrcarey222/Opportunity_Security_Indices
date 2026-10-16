// Draw the Fence: loading the bundle written by scripts/89 (contract.ts). Each file is fetched at
// most once per page; a failed request is dropped from the cache so it can be retried. The manifest
// is always revalidated; the hashed files are immutable.

import { useEffect, useState } from 'react';
import { YF_SCHEMA, type YFCountryBundle, type YFManifest, type YFReference } from './contract.ts';

export const YF_DATA_DIR = 'data/yards-fences/';

export function yfDataUrl(file: string): string {
  const base = (import.meta.env?.BASE_URL as string | undefined) ?? '/';
  return `${base}${YF_DATA_DIR}${file}`;
}

const cache = new Map<string, Promise<unknown>>();

function getJson<T>(file: string, noCache = false): Promise<T> {
  let p = cache.get(file) as Promise<T> | undefined;
  if (!p) {
    const q = fetch(yfDataUrl(file), noCache ? { cache: 'no-cache' } : undefined).then(async (r) => {
      if (!r.ok) throw new Error(`${file}: HTTP ${r.status}`);
      return (await r.json()) as T;
    });
    p = q;
    cache.set(file, q);
    q.catch(() => {
      if (cache.get(file) === q) cache.delete(file);
    });
  }
  return p;
}

export function isYFManifest(x: unknown): x is YFManifest {
  const m = x as YFManifest;
  return !!m && m.schema === YF_SCHEMA && typeof m.reference === 'string' && Array.isArray(m.countries);
}

export interface YFData {
  manifest: YFManifest;
  reference: YFReference;
  bundle: YFCountryBundle;
}

export async function loadYFData(country: string): Promise<YFData> {
  const manifest = await getJson<unknown>('manifest.json', true);
  if (!isYFManifest(manifest)) throw new Error('Draw the Fence data manifest is not in the expected format.');
  const entry = manifest.countries.find((c) => c.iso3 === country) ?? manifest.countries[0];
  if (!entry) throw new Error('The Draw the Fence bundle has no country.');
  const [reference, bundle] = await Promise.all([getJson<YFReference>(manifest.reference), getJson<YFCountryBundle>(entry.file)]);
  return { manifest, reference, bundle };
}

export type YFLoad = { status: 'loading' } | { status: 'error'; message: string } | { status: 'ready'; data: YFData };

/** The bundle for one country. */
export function useYFData(country: string): YFLoad {
  const [state, setState] = useState<YFLoad>({ status: 'loading' });
  useEffect(() => {
    let live = true;
    setState((s) => (s.status === 'ready' && s.data.bundle.country.iso3 === country ? s : { status: 'loading' }));
    loadYFData(country).then(
      (data) => live && setState({ status: 'ready', data }),
      (e: unknown) => live && setState({ status: 'error', message: e instanceof Error ? e.message : String(e) }),
    );
    return () => {
      live = false;
    };
  }, [country]);
  return state;
}

/** Tests: forget every cached request. */
export function resetYFCache(): void {
  cache.clear();
}
