// Data release identity (docs/QC_SYSTEM_DESIGN.md 3.1). One ID for the set of bundles a page reads,
// computed the same way in the browser (from shell/buildInfo stamps) and in Node (from the manifests,
// dashboard/scripts/review/build-review-data.mjs), so a submission and a case snapshot can be matched.
// Pure; uses Web Crypto (globalThis.crypto.subtle exists in browsers and Node >= 19).

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
