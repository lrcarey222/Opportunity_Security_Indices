// Release identity across Node and the browser (review/release.ts): the memos stamp hashes the partnerships
// manifest LF-normalised in both (so a CRLF checkout gives the Pages release ID), and a report from an Explorer
// page records the review manifest's release only when every stamp the page loaded agrees with it.

import { mkdtempSync, mkdirSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { afterEach, describe, expect, it } from 'vitest';
import { compareReleaseStamps, dataReleaseId, memosManifestSha256, resolveReportRelease, sha256Hex, type ReleaseStamps } from './release.ts';
import { memosManifestSha256 as appMemosSha } from './appInfo.ts';
import { loadRawBundles } from './qc/node/loadBundles.ts';
import { releaseStampsOf } from './qc/bundles.ts';

const FULL: ReleaseStamps = {
  posture: { created: '2026-11-17T20:33:31Z', git_commit: '0496e7ba' },
  country: { created: '2026-10-31T15:58:15Z', git_commit: '4bb2adc' },
  sector: { bundle_id: 'sec1' },
  aib: { created: '2026-11-17T19:07:29Z' },
  strategic: { bundle_id: 'str1' },
  memos: { manifest_sha256: 'abc' },
};
const PAGE: ReleaseStamps = { ...FULL, sector: null, strategic: null, memos: null };

describe('memosManifestSha256', () => {
  it('is the same for LF, CRLF and a BOM, and is what appInfo exports', async () => {
    const lf = '{\n  "schema": "osi-partnership-memos"\n}\n';
    expect(memosManifestSha256(lf.replace(/\n/g, '\r\n'))).toBe(memosManifestSha256(lf));
    expect(memosManifestSha256(`\uFEFF${lf}`)).toBe(memosManifestSha256(lf));
    expect(memosManifestSha256(lf)).toBe(await sha256Hex(lf));
    expect(appMemosSha).toBe(memosManifestSha256);
  });
});

describe('the Node bundle loader stamps the memos manifest like the browser', () => {
  let dir: string | null = null;
  afterEach(() => {
    if (dir) rmSync(dir, { recursive: true, force: true });
    dir = null;
  });
  it('a CRLF partnerships manifest on disk gives the LF hash (and the same release ID)', async () => {
    const lf = '{\n  "schema": "osi-partnership-memos",\n  "pairs": {},\n  "compacts": {}\n}\n';
    const ids: string[] = [];
    for (const text of [lf, lf.replace(/\n/g, '\r\n')]) {
      dir = mkdtempSync(join(tmpdir(), 'osi-release-'));
      mkdirSync(join(dir, 'partnerships'));
      writeFileSync(join(dir, 'partnerships', 'manifest.json'), text);
      const { raw } = loadRawBundles(dir, { details: false });
      expect(raw.partnerships!.manifestSha256).toBe(memosManifestSha256(lf));
      ids.push(await dataReleaseId(releaseStampsOf(raw)));
      rmSync(dir, { recursive: true, force: true });
      dir = null;
    }
    expect(ids[0]).toBe(ids[1]);
  });
});

describe('compareReleaseStamps / resolveReportRelease', () => {
  it('a page whose loaded stamps all match records the review release', () => {
    expect(compareReleaseStamps(PAGE, FULL)).toEqual({ matches: true, loaded: ['posture', 'country', 'aib'], differing: [] });
    expect(resolveReportRelease('rel-partial00000', PAGE, { id: 'rel-full00000000', stamps: FULL })).toEqual({ data_release: 'rel-full00000000', release_note: null });
  });
  it('a differing stamp keeps the partial ID and names the bundle', () => {
    const page = { ...PAGE, posture: { created: '2026-11-18T00:00:00Z', git_commit: '0496e7ba' } };
    expect(compareReleaseStamps(page, FULL).differing).toEqual(['posture']);
    expect(resolveReportRelease('rel-partial00000', page, { id: 'rel-full00000000', stamps: FULL })).toEqual({
      data_release: 'rel-partial00000',
      release_note: 'page stamps differ from review release rel-full00000000 in: posture',
    });
    // a bundle the release lacks but the page loaded is a difference too
    expect(compareReleaseStamps(FULL, { ...FULL, memos: null }).differing).toEqual(['memos']);
  });
  it('nothing loaded, or no review manifest: the partial ID with a note', () => {
    const none: ReleaseStamps = { posture: null, country: null, sector: null, aib: null, strategic: null, memos: null };
    expect(resolveReportRelease('rel-p', none, { id: 'rel-f', stamps: FULL })).toEqual({ data_release: 'rel-p', release_note: 'no bundle loaded on this page: review release rel-f not confirmed' });
    expect(resolveReportRelease('rel-p', PAGE, null).data_release).toBe('rel-p');
    expect(resolveReportRelease('rel-p', PAGE, null).release_note).toMatch(/could not be read/);
  });
});
