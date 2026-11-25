// Data release identity (docs/QC_SYSTEM_DESIGN.md 3.1). One ID for the set of bundles a page reads,
// computed the same way in the browser (from shell/buildInfo stamps) and in Node (from the manifests,
// dashboard/scripts/review/build-review-data.mjs), so a submission and a case snapshot can be matched.
// Pure; uses Web Crypto (globalThis.crypto.subtle exists in browsers and Node >= 19).

import { sha256HexSync } from './privacy.ts';

export interface ReleaseStamps {
  posture: { created: string | null; git_commit: string | null } | null;
  country: { created: string | null; git_commit: string | null } | null;
  sector: { bundle_id: string | null } | null;
  aib: { created: string | null } | null;
  strategic: { bundle_id: string | null } | null;
  /** sha256 of the partnerships manifest text, or null when not loaded. */
  memos: { manifest_sha256: string | null } | null;
}

/** Hex sha256 of a UTF-8 string. */
export async function sha256Hex(text: string): Promise<string> {
  const bytes = new TextEncoder().encode(text);
  const digest = await globalThis.crypto.subtle.digest('SHA-256', bytes);
  return Array.from(new Uint8Array(digest), (b) => b.toString(16).padStart(2, '0')).join('');
}

/** Hex sha256 of raw bytes (bundle files). */
export async function sha256Bytes(bytes: Uint8Array): Promise<string> {
  const digest = await globalThis.crypto.subtle.digest('SHA-256', bytes as unknown as ArrayBuffer);
  return Array.from(new Uint8Array(digest), (b) => b.toString(16).padStart(2, '0')).join('');
}

/** The canonical string the release ID hashes: a fixed field order, '-' for anything missing. */
export function releaseKey(s: ReleaseStamps): string {
  const v = (x: string | null | undefined) => (x === null || x === undefined || x === '' ? '-' : x);
  return [
    `posture:${v(s.posture?.created)}@${v(s.posture?.git_commit)}`,
    `country:${v(s.country?.created)}@${v(s.country?.git_commit)}`,
    `sector:${v(s.sector?.bundle_id)}`,
    `aib:${v(s.aib?.created)}`,
    `strategic:${v(s.strategic?.bundle_id)}`,
    `memos:${v(s.memos?.manifest_sha256)}`,
  ].join('|');
}

/** `rel-<12 hex>`. Partial stamps still give an ID; `releaseCompleteness` says which parts were missing. */
export async function dataReleaseId(s: ReleaseStamps): Promise<string> {
  return `rel-${(await sha256Hex(releaseKey(s))).slice(0, 12)}`;
}

/** The bundles whose stamp is missing from a release key (shown next to the ID; flagged by QC). */
export function releaseCompleteness(s: ReleaseStamps): { complete: boolean; missing: string[] } {
  const missing: string[] = [];
  if (!s.posture?.created) missing.push('posture');
  if (!s.posture?.git_commit) missing.push('posture.git_commit');
  if (!s.country?.created) missing.push('country');
  if (!s.sector?.bundle_id) missing.push('sector');
  if (!s.aib?.created) missing.push('aib');
  if (!s.strategic?.bundle_id) missing.push('strategic');
  if (!s.memos?.manifest_sha256) missing.push('memos');
  return { complete: missing.length === 0, missing };
}

/**
 * sha256 of a partnership memos manifest's text with any BOM dropped and line endings normalised to LF, so a
 * Windows checkout (core.autocrlf), the Node review-data build and the Pages copy of the same manifest give the
 * same memos stamp (and so the same release ID). Synchronous: review/privacy.ts sha256HexSync.
 */
export function memosManifestSha256(text: string): string {
  return sha256HexSync(text.replace(/^\uFEFF/, '').replace(/\r\n?/g, '\n'));
}

/** The bundles of a release key, in its order. */
export const RELEASE_BUNDLES = ['posture', 'country', 'sector', 'aib', 'strategic', 'memos'] as const;
export type ReleaseBundle = (typeof RELEASE_BUNDLES)[number];

/** One bundle's stamp as the release key writes it ('-' for a missing part), or null when the bundle is absent. */
function stampText(s: ReleaseStamps, b: ReleaseBundle): string | null {
  const one = s[b];
  if (!one) return null;
  const v = (x: string | null | undefined) => (x === null || x === undefined || x === '' ? '-' : x);
  switch (b) {
    case 'posture':
    case 'country': {
      const c = one as { created: string | null; git_commit: string | null };
      return `${v(c.created)}@${v(c.git_commit)}`;
    }
    case 'sector':
    case 'strategic':
      return v((one as { bundle_id: string | null }).bundle_id);
    case 'aib':
      return v((one as { created: string | null }).created);
    case 'memos':
      return v((one as { manifest_sha256: string | null }).manifest_sha256);
  }
}

/**
 * A page's stamps against a full release's, bundle by bundle: `loaded` = the bundles the page has a stamp for,
 * `differing` = those whose stamp differs from the release's, `matches` = the page loaded at least one bundle and
 * none differs (the bundles it did not load are not evidence either way).
 */
export function compareReleaseStamps(page: ReleaseStamps, release: ReleaseStamps): { matches: boolean; loaded: ReleaseBundle[]; differing: ReleaseBundle[] } {
  const loaded = RELEASE_BUNDLES.filter((b) => stampText(page, b) !== null);
  const differing = loaded.filter((b) => stampText(page, b) !== stampText(release, b));
  return { matches: loaded.length > 0 && differing.length === 0, loaded, differing };
}

/**
 * The data release a report from an Explorer page records (docs/QC_SYSTEM_DESIGN.md 3.1). A page publishes only the
 * stamps of the bundles it has loaded, so its own ID (pageId) is partial. When the review manifest's release
 * (data/review/manifest.json) agrees with every stamp the page has loaded, the report records the review release ID,
 * which joins to the cases files; otherwise it keeps the page's partial ID and says why in release_note (which
 * bundles differ, or that nothing could be compared), so the admin sees the mismatch.
 */
export function resolveReportRelease(
  pageId: string | null,
  page: ReleaseStamps,
  review: { id: string; stamps: ReleaseStamps } | null,
): { data_release: string | null; release_note: string | null } {
  if (!review) return { data_release: pageId, release_note: 'the review manifest could not be read: the ID covers only the bundles this page loaded' };
  const c = compareReleaseStamps(page, review.stamps);
  if (c.matches) return { data_release: review.id, release_note: null };
  if (!c.loaded.length) return { data_release: pageId, release_note: `no bundle loaded on this page: review release ${review.id} not confirmed` };
  return { data_release: pageId, release_note: `page stamps differ from review release ${review.id} in: ${c.differing.join(', ')}` };
}
