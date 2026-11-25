// The version stamp (external review readiness, 2026-11-23): one data-release date and one method/version
// line, shared by the footer and the Methods and Data pages, so a reader (or a reviewer quoting a page) can
// say exactly which data and which code they saw.
//   * Data release = the newest export of the posture and Allied Industrial Base bundles this page knows
//     (shell/buildInfo); those two carry the strategic choices and the allied analysis.
//   * Version = the decision framework's method version, the app commit (VITE_APP_COMMIT, read directly so
//     the shell chunk never pulls in the review code) and each bundle's commit or build id.
// The footer never forces a load (buildInfo's rule); the Methods and Data pages call useReleaseManifests() so
// they read the two small manifests themselves.

import { useEffect } from 'react';
import type { BuildInfo } from './buildInfo.ts';

/** The decision framework version the shipped choices follow (docs/strategic_position.md, 2026-10-30). */
export const METHOD_VERSION = 'decision framework (2026-10-30)';

const clean = (x: unknown): string | null => (typeof x === 'string' && x.trim() && x.trim() !== 'unknown' ? x.trim() : null);

/** The app's git short SHA injected at build time, or null (tests, Node). */
export function appCommitShort(): string | null {
  try {
    const v = clean(import.meta.env.VITE_APP_COMMIT);
    return v ? v.slice(0, 8) : null;
  } catch {
    return null;
  }
}

const validDate = (x: string | null | undefined): x is string => !!x && !Number.isNaN(new Date(x).getTime());

/** The data release: the newest posture or Allied Industrial Base export this page knows (YYYY-MM-DD), or null. */
export function dataReleaseDate(info: Pick<BuildInfo, 'posture' | 'aib'>): string | null {
  const ds = [info.posture?.created, info.aib?.created].filter(validDate).map((d) => new Date(d).toISOString());
  if (!ds.length) return null;
  return ds.sort()[ds.length - 1].slice(0, 10);
}

const short = (s: string | null | undefined, n = 8): string | null => (s ? s.slice(0, n) : null);
/** A bundle's identity: its git commit, else its export date. */
const stampOf = (s: { created: string | null; git_commit?: string | null } | null): string | null =>
  s ? (short(s.git_commit) ?? (validDate(s.created) ? s.created.slice(0, 10) : null)) : null;

export interface VersionPart {
  key: 'method' | 'app' | 'posture' | 'aib' | 'sector' | 'country' | 'strategic';
  /** the label in running text */
  label: string;
  /** the label in a table row */
  rowLabel: string;
  value: string;
}

/**
 * The version parts this page can state, in order. The footer line uses method, app, posture, aib and sector;
 * the Methods and Data pages list them all (`all`). A bundle this page has not loaded is left out.
 */
export function versionParts(info: BuildInfo, app: string | null = appCommitShort(), all = false): VersionPart[] {
  const parts: VersionPart[] = [{ key: 'method', label: 'Method version', rowLabel: 'Method version', value: METHOD_VERSION }];
  parts.push({ key: 'app', label: 'app', rowLabel: 'App build', value: app ?? 'unknown' });
  const posture = stampOf(info.posture);
  if (posture) parts.push({ key: 'posture', label: 'posture', rowLabel: 'Strategic choices (posture)', value: posture });
  const aib = stampOf(info.aib);
  if (aib) parts.push({ key: 'aib', label: 'Allied Industrial Base', rowLabel: 'Allied Industrial Base', value: aib });
  if (info.sector) parts.push({ key: 'sector', label: 'sector', rowLabel: 'Sector data', value: info.sector.bundle_id.slice(0, 8) });
  if (all) {
    const country = stampOf(info.country);
    if (country) parts.push({ key: 'country', label: 'country', rowLabel: 'Country data', value: country });
    if (info.strategic) parts.push({ key: 'strategic', label: 'scores', rowLabel: 'Security and opportunity scores', value: info.strategic.bundle_id.slice(0, 8) });
  }
  return parts;
}

/** The one-line version text ("Method version: decision framework (2026-10-30) · app abc1234 · posture ..."). */
export function versionLine(parts: readonly VersionPart[]): string {
  return parts.map((p) => (p.key === 'method' ? `${p.label}: ${p.value}` : `${p.label} ${p.value}`)).join(' · ');
}

/** The version line as inline content: the Allied Industrial Base part as an <abbr>AIB</abbr>, to keep the line short. */
export function VersionLine({ parts }: { parts: readonly VersionPart[] }) {
  return (
    <>
      {parts.map((p, i) => (
        <span key={p.key}>
          {i > 0 && ' · '}
          {p.key === 'method' ? (
            `${p.label}: ${p.value}`
          ) : p.key === 'aib' ? (
            <>
              <abbr title="Allied Industrial Base">AIB</abbr> {p.value}
            </>
          ) : (
            `${p.label} ${p.value}`
          )}
        </span>
      ))}
    </>
  );
}

/**
 * Read the posture and Allied Industrial Base manifests (two small files) so the page's stamp is complete.
 * Each store publishes its stamp into buildInfo; a failure is silent (the stamp just leaves the part out).
 */
export function useReleaseManifests(): void {
  useEffect(() => {
    let live = true;
    import('../posture/store.ts').then(
      ({ postureStore }) => {
        if (live) postureStore().manifest().catch(() => undefined);
      },
      () => undefined,
    );
    import('../aib/data/store.ts').then(
      ({ aibStore }) => {
        if (live) aibStore().manifest().catch(() => undefined);
      },
      () => undefined,
    );
    return () => {
      live = false;
    };
  }, []);
}

/** The stamp as table rows (Methods and Data pages): the data release first, then every version part. */
export function releaseRows(info: BuildInfo, app: string | null = appCommitShort()): [string, string][] {
  const release = dataReleaseDate(info);
  return [['Data release', release ?? 'not loaded'], ...versionParts(info, app, true).map((p): [string, string] => [p.rowLabel, p.value])];
}
