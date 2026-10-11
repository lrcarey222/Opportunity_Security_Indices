// The Sector data bundle's manifest and chunk bytes, fetched at most once per page (spec_sector.md
// 2.1, 8.10). Home's idle prefetch calls loadSectorManifest() (headlines, coverage glyph) and
// prefetchSectorChunk('sector-meta'); the Sector data layer (builder C, src/sector/data/) may call
// the same functions so a prefetched manifest or chunk is never requested twice. Every failure is
// retryable: the cached promise is dropped when it rejects.

import { chunkInfo, isSectorManifest, SECTOR_DATA_DIR, SECTOR_MANIFEST_FILE, sectorManifestProblems, type SectorChunkName, type SectorManifest } from '../sector/contract.ts';
import { setSectorBuild } from './buildInfo.ts';

/** The URL of a file in the Sector data directory, under the app's base URL. */
export function sectorDataUrl(file: string): string {
  return `${import.meta.env.BASE_URL ?? '/'}${SECTOR_DATA_DIR}${file}`;
}

let manifest: Promise<SectorManifest> | null = null;
let manifestValue: SectorManifest | null = null;

/** manifest.json (cache: no-cache), checked with isSectorManifest(); publishes the build stamp. */
export function loadSectorManifest(): Promise<SectorManifest> {
  if (!manifest) {
    const p = fetch(sectorDataUrl(SECTOR_MANIFEST_FILE), { cache: 'no-cache' }).then(async (r) => {
      if (!r.ok) throw new Error(`Sector data manifest: HTTP ${r.status}`);
      const j: unknown = await r.json();
      if (!isSectorManifest(j)) throw new Error(`Sector data manifest: ${sectorManifestProblems(j).slice(0, 3).join('; ')}`);
      manifestValue = j;
      setSectorBuild(j);
      return j;
    });
    manifest = p;
    p.catch(() => {
      if (manifest === p) manifest = null;
    });
  }
  return manifest;
}

/** The manifest when it has already loaded (synchronous; null before). */
export function peekSectorManifest(): SectorManifest | null {
  return manifestValue;
}

const chunks = new Map<string, Promise<Uint8Array>>();

/** The raw bytes of a chunk file (immutable, default HTTP cache), shared by every caller. */
export function fetchSectorChunkBytes(file: string): Promise<Uint8Array> {
  let p = chunks.get(file);
  if (!p) {
    const q = fetch(sectorDataUrl(file)).then(async (r) => {
      if (!r.ok) throw new Error(`${file}: HTTP ${r.status}`);
      return new Uint8Array(await r.arrayBuffer());
    });
    p = q;
    chunks.set(file, q);
    q.catch(() => {
      if (chunks.get(file) === q) chunks.delete(file);
    });
  }
  return p;
}

/** Prefetch one chunk named in the manifest (no-op when it is not listed). */
export function prefetchSectorChunk(m: SectorManifest, name: SectorChunkName): Promise<void> {
  const info = chunkInfo(m, name);
  return info ? fetchSectorChunkBytes(info.file).then(() => undefined) : Promise.resolve();
}

/** Forget the cached manifest and chunks (tests). */
export function resetSectorManifestCache(): void {
  manifest = null;
  manifestValue = null;
  chunks.clear();
}
