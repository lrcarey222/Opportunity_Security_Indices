// What each module's data build is (spec_sector.md 2.5): a tiny module-level store both modules
// publish into once their manifest is loaded (setStrategicBuild from StrategicLayout, setSectorBuild
// from the Sector data provider or Home's idle prefetch). The footer reads it; it never forces a load.

import { useSyncExternalStore } from 'react';

export interface BuildStamp {
  bundle_id: string;
  created: string;
  git_commit: string | null;
}

export interface StrategicBuild extends BuildStamp {
  paper: { title: string | null; url: string | null } | null;
}

export interface BuildInfo {
  strategic: StrategicBuild | null;
  sector: BuildStamp | null;
}

let state: BuildInfo = { strategic: null, sector: null };
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

/** Forget both stamps (tests). */
export function resetBuildInfo(): void {
  emit({ strategic: null, sector: null });
}

const subscribe = (l: () => void) => {
  listeners.add(l);
  return () => listeners.delete(l);
};
const snapshot = () => state;

export function useBuildInfo(): BuildInfo {
  return useSyncExternalStore(subscribe, snapshot, snapshot);
}
