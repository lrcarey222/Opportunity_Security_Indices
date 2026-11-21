// The app build's identity and the review UI switches (docs/QC_SYSTEM_DESIGN.md 3.1, 5): the code SHA and
// build date injected by vite.config.ts (`define`; 'unknown' in tests and in Node), the data-release stamps
// read from shell/buildInfo, whether a reviewer session is active (a stored profile) and whether the
// review controls show (VITE_REVIEW_UI=1 or an active session). No IO beyond the review store's read.
//
// Main's app has no partnership memo loader, so the memos stamp stays null (releaseCompleteness lists it as
// missing) unless a review page reads data/partnerships/manifest.json and calls publishMemosManifest(text). The
// composite ID of a page is therefore only comparable with a case file's data_release when the page has
// published the same set of stamps; compare bundle stamps one by one (page_context.bundle_stamps) otherwise.

import { getBuildInfo, setMemosBuild, type BuildInfo } from '../shell/buildInfo.ts';
import { dataReleaseId, memosManifestSha256, releaseCompleteness, type ReleaseStamps } from './release.ts';
import { reviewStorage, type ReviewStorage } from './storage.ts';

// Each read is the literal `import.meta.env.X` so Vite's define replaces it; Node has no import.meta.env.
function envString(read: () => unknown): string | null {
  try {
    const v = read();
    return typeof v === 'string' && v.trim() ? v.trim() : null;
  } catch {
    return null;
  }
}

/** The app's git short SHA at build time, or 'unknown'. */
export function appCommit(): string {
  return envString(() => import.meta.env.VITE_APP_COMMIT) ?? 'unknown';
}

/** The build's ISO date-time, or 'unknown'. */
export function buildDate(): string {
  return envString(() => import.meta.env.VITE_BUILD_DATE) ?? 'unknown';
}

/** The release stamps of the bundles this page has loaded (null for a bundle not loaded yet). */
export function releaseStampsFromBuildInfo(info: BuildInfo): ReleaseStamps {
  return {
    posture: info.posture ? { created: info.posture.created, git_commit: info.posture.git_commit } : null,
    country: info.country ? { created: info.country.created, git_commit: info.country.git_commit } : null,
    sector: info.sector ? { bundle_id: info.sector.bundle_id } : null,
    aib: info.aib ? { created: info.aib.created } : null,
    strategic: info.strategic ? { bundle_id: info.strategic.bundle_id } : null,
    memos: info.memos ? { manifest_sha256: info.memos.manifest_sha256 } : null,
  };
}

/**
 * sha256 of a partnership memos manifest's text with line endings normalised to LF, so a Windows checkout
 * (core.autocrlf) and the Pages copy of the same manifest give the same hash. Defined in release.ts (pure, no
 * React) so the Node review-data build (qc/node/loadBundles.ts) computes the memos stamp with the same function.
 */
export { memosManifestSha256 };

/** Publish the memos stamp from the manifest text a page read (null when it could not be read). */
export function publishMemosManifest(text: string | null): void {
  setMemosBuild(typeof text === 'string' && text.length ? memosManifestSha256(text) : null);
}

/** The data-release ID of the stamps this page has published, with the bundles missing from it. */
export async function pageRelease(info: BuildInfo = getBuildInfo()): Promise<{ id: string; complete: boolean; missing: string[] }> {
  const s = releaseStampsFromBuildInfo(info);
  return { id: await dataReleaseId(s), ...releaseCompleteness(s) };
}

/** Whether this browser holds a reviewer profile (the review toolbar and issue buttons then show). */
export function reviewSessionActive(store: ReviewStorage = reviewStorage()): boolean {
  return store.loadProfile() !== null;
}

/** Whether the review controls show: the build sets VITE_REVIEW_UI=1, or a reviewer session is active. */
export function reviewUiEnabled(store: ReviewStorage = reviewStorage()): boolean {
  return envString(() => import.meta.env.VITE_REVIEW_UI) === '1' || reviewSessionActive(store);
}
