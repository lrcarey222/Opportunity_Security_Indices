// Loads data/partnerships/manifest.json (about 10 kB) once per page life, only when a page that offers a memo
// asks for it (the Partnerships pair view, a Strategic Sectoral Compact page). A failure or a missing file
// is no memo action, never an error on the page. Tests swap the loader with setMemoManifestForTests().

import { useEffect, useState } from 'react';
import { readMemoManifest, type MemoManifest } from './memos.ts';

/** The memo directory under the app's base URL ("/data/partnerships/"). */
export function memoBase(): string {
  const base = (import.meta.env?.BASE_URL as string | undefined) ?? '/';
  return `${base.endsWith('/') ? base : `${base}/`}data/partnerships/`;
}

/** A memo file's URL (its name as the manifest lists it). */
export const memoFileUrl = (file: string): string => `${memoBase()}${file}`;

let cached: Promise<MemoManifest | null> | null = null;

function load(): Promise<MemoManifest | null> {
  if (!cached) {
    cached =
      typeof fetch !== 'function'
        ? Promise.resolve(null)
        : fetch(`${memoBase()}manifest.json`, { cache: 'no-cache' })
            .then((r) => (r.ok ? r.json() : null))
            .then((x: unknown) => readMemoManifest(x))
            .catch(() => null);
  }
  return cached;
}

/** Tests: a manifest to serve (null: none), or undefined to go back to the network. */
export function setMemoManifestForTests(m: unknown): void {
  cached = m === undefined ? null : Promise.resolve(readMemoManifest(m));
}

/** The memo manifest once loaded (null while loading or when there is none). */
export function useMemoManifest(): MemoManifest | null {
  const [m, setM] = useState<MemoManifest | null>(null);
  useEffect(() => {
    let live = true;
    void load().then((x) => {
      if (live) setM(x);
    });
    return () => {
      live = false;
    };
  }, []);
  return m;
}
