// What each module's data build is (spec_sector.md 2.5): a tiny module-level store the modules publish
// into once their manifest is loaded (setStrategicBuild from StrategicLayout, setSectorBuild from the Sector
// data provider or Home's idle prefetch; setPostureBuild, setCountryBuild and setAibBuild from their stores,
// docs/QC_SYSTEM_DESIGN.md 3.1; setMemosBuild for a memo manifest, which main does not load yet). The footer
// reads it, and the review system computes the page's data-release ID from it (review/appInfo.ts); it never
// forces a load.

import { useSyncExternalStore } from 'react';

export interface BuildStamp {
  bundle_id: string;
  created: string;
  git_commit: string | null;
}

export interface StrategicBuild extends BuildStamp {
  paper: { title: string | null; url: string | null } | null;
}

/** A bundle stamped by export time and (where the manifest has one) the pipeline's git commit. */
export interface CreatedStamp {
  created: string | null;
  git_commit: string | null;
}

export interface BuildInfo {
  strategic: StrategicBuild | null;
  sector: BuildStamp | null;
  /** data/posture/manifest.json */
  posture: CreatedStamp | null;
  /** data/country/manifest.json */
  country: CreatedStamp | null;
  /** data/aib/manifest.json: its export time and (since PR #370) the pipeline's git commit. `git_commit` is
   *  optional so older callers that build the shape by hand stay valid; setAibBuild always records it. */
  aib: { created: string | null; git_commit?: string | null } | null;
  /** sha256 of the partnership memos manifest text (null until a memo manifest is loaded) */
  memos: { manifest_sha256: string | null } | null;
}

const EMPTY: BuildInfo = { strategic: null, sector: null, posture: null, country: null, aib: null, memos: null };

let state: BuildInfo = EMPTY;
const listeners = new Set<() => void>();

function emit(next: BuildInfo) {
  state = next;
  for (const l of listeners) l();
}

const same = (a: BuildStamp | null, b: BuildStamp | null) => !!a && !!b && a.bundle_id === b.bundle_id && a.created === b.created && a.git_commit === b.git_commit;

/** Publish the Strategic manifest's stamp (structural: any object with these fields). */
export function setStrategicBuild(m: { bundle_id: string; created: string; git_commit: string | null; paper?: { title: string | null; url: string | null } | null }): void {
  const next: StrategicBuild = { bundle_id: m.bundle_id, created: m.created, git_commit: m.git_commit, paper: m.paper ? { title: m.paper.title, url: m.paper.url } : null };
  if (same(state.strategic, next) && state.strategic?.paper?.title === next.paper?.title && state.strategic?.paper?.url === next.paper?.url) return;
  emit({ ...state, strategic: next });
}

/** Publish the Sector manifest's stamp. */
export function setSectorBuild(m: { bundle_id: string; created: string; git_commit: string | null }): void {
  const next: BuildStamp = { bundle_id: m.bundle_id, created: m.created, git_commit: m.git_commit };
  if (same(state.sector, next)) return;
  emit({ ...state, sector: next });
}

const str = (x: unknown): string | null => (typeof x === 'string' && x.trim() ? x : null);

function createdStamp(m: unknown): CreatedStamp {
  const o = (typeof m === 'object' && m !== null ? m : {}) as { created?: unknown; git_commit?: unknown };
  return { created: str(o.created), git_commit: str(o.git_commit) };
}

/** Publish the Posture manifest's stamp (structural: reads `created` and, when present, `git_commit`). */
export function setPostureBuild(m: unknown): void {
  const next = createdStamp(m);
  if (state.posture && state.posture.created === next.created && state.posture.git_commit === next.git_commit) return;
  emit({ ...state, posture: next });
}

/** Publish the Country manifest's stamp. */
export function setCountryBuild(m: unknown): void {
  const next = createdStamp(m);
  if (state.country && state.country.created === next.created && state.country.git_commit === next.git_commit) return;
  emit({ ...state, country: next });
}

/** Publish the AIB (Sectoral Compacts) manifest's stamp: its export time and git commit. */
export function setAibBuild(m: unknown): void {
  const next = createdStamp(m);
  if (state.aib && state.aib.created === next.created && (state.aib.git_commit ?? null) === next.git_commit) return;
  emit({ ...state, aib: next });
}

/** Publish the partnership memos manifest's hash (sha256 hex of its text; null when it could not be read). */
export function setMemosBuild(manifestSha256: string | null): void {
  const v = str(manifestSha256);
  if (state.memos && state.memos.manifest_sha256 === v) return;
  emit({ ...state, memos: { manifest_sha256: v } });
}

/** The current stamps, outside React (the review system's submission envelope). */
export const getBuildInfo = (): BuildInfo => state;

/** Forget every stamp (tests). */
export function resetBuildInfo(): void {
  emit(EMPTY);
}

const subscribe = (l: () => void) => {
  listeners.add(l);
  return () => listeners.delete(l);
};
const snapshot = () => state;

export function useBuildInfo(): BuildInfo {
  return useSyncExternalStore(subscribe, snapshot, snapshot);
}
