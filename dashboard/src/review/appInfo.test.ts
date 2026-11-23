// The build identity and review UI switches (review/appInfo.ts): 'unknown' outside a Vite build, release
// stamps from shell/buildInfo, and the review controls shown only with a session or VITE_REVIEW_UI=1.

import { afterEach, describe, expect, it, vi } from 'vitest';
import { getBuildInfo, resetBuildInfo, setAibBuild, setCountryBuild, setMemosBuild, setPostureBuild, setSectorBuild, setStrategicBuild } from '../shell/buildInfo.ts';
import type { ReviewerProfile } from './model.ts';
import { dataReleaseId, releaseCompleteness, releaseKey, sha256Hex } from './release.ts';
import { appCommit, buildDate, memosManifestSha256, pageRelease, publishMemosManifest, releaseStampsFromBuildInfo, reviewSessionActive, reviewUiEnabled } from './appInfo.ts';
import { createReviewStorage } from './storage.ts';

afterEach(() => {
  resetBuildInfo();
  vi.unstubAllEnvs();
});

describe('build identity', () => {
  it('is unknown when the build did not inject it (tests, Node)', () => {
    expect(appCommit()).toBe('unknown');
    expect(buildDate()).toBe('unknown');
  });
  it('reads VITE_APP_COMMIT / VITE_BUILD_DATE when set', () => {
    vi.stubEnv('VITE_APP_COMMIT', 'abc1234');
    vi.stubEnv('VITE_BUILD_DATE', '2027-01-10T00:00:00Z');
    expect(appCommit()).toBe('abc1234');
    expect(buildDate()).toBe('2027-01-10T00:00:00Z');
  });
});

describe('releaseStampsFromBuildInfo', () => {
  it('is all null before any bundle loads, and the release ID says what is missing', async () => {
    const s = releaseStampsFromBuildInfo(getBuildInfo());
    expect(s).toEqual({ posture: null, country: null, sector: null, aib: null, strategic: null, memos: null });
    expect(releaseCompleteness(s).missing).toContain('posture');
    expect(await dataReleaseId(s)).toMatch(/^rel-[0-9a-f]{12}$/);
  });
  it('carries every published stamp into the release key (memos recorded as missing on main)', async () => {
    setPostureBuild({ created: '2026-11-17T20:33:31Z', git_commit: '0496e7ba', core: 'x' });
    setCountryBuild({ created: '2026-10-31T15:58:15Z', git_commit: '4bb2adc' });
    setAibBuild({ created: '2026-11-17T19:07:29Z' });
    setSectorBuild({ bundle_id: 'sec1', created: '2026-10-31', git_commit: '4bb2adc' });
    setStrategicBuild({ bundle_id: 'str1', created: '2026-10-30', git_commit: 'a54a9d3' });
    const s = releaseStampsFromBuildInfo(getBuildInfo());
    expect(releaseKey(s)).toBe('posture:2026-11-17T20:33:31Z@0496e7ba|country:2026-10-31T15:58:15Z@4bb2adc|sector:sec1|aib:2026-11-17T19:07:29Z|strategic:str1|memos:-');
    expect(releaseCompleteness(s)).toEqual({ complete: false, missing: ['memos'] });
    setMemosBuild('f'.repeat(64));
    expect(releaseCompleteness(releaseStampsFromBuildInfo(getBuildInfo())).complete).toBe(true);
    const a = await dataReleaseId(releaseStampsFromBuildInfo(getBuildInfo()));
    setPostureBuild({ created: '2026-11-17T17:12:00Z', git_commit: '8021770d' });
    expect(await dataReleaseId(releaseStampsFromBuildInfo(getBuildInfo()))).not.toBe(a);
  });
});

describe('memo manifest stamp and the page release', () => {
  it('hashes the manifest text with LF line endings, so a CRLF checkout gives the Pages hash', async () => {
    const lf = '{\n  "schema": "osi-partnership-memos"\n}\n';
    const crlf = lf.replace(/\n/g, '\r\n');
    expect(memosManifestSha256(crlf)).toBe(memosManifestSha256(lf));
    expect(memosManifestSha256(lf)).toBe(await sha256Hex(lf));
    publishMemosManifest(crlf);
    expect(getBuildInfo().memos).toEqual({ manifest_sha256: await sha256Hex(lf) });
    publishMemosManifest(null);
    expect(getBuildInfo().memos).toEqual({ manifest_sha256: null });
  });
  it('gives the page release ID and what it lacks', async () => {
    setPostureBuild({ created: '2026-11-17T20:33:31Z', git_commit: '0496e7ba' });
    const r = await pageRelease();
    expect(r.id).toMatch(/^rel-[0-9a-f]{12}$/);
    expect(r.complete).toBe(false);
    expect(r.missing).toEqual(['country', 'sector', 'aib', 'strategic', 'memos']);
  });
});

describe('review UI switches', () => {
  const profile = { reviewer_id: 'r', anonymous: true, countries: [], sectors: [], stages: [], methods: [], confidence: {}, familiarity_neis: 0, familiarity_explorer: 0, advised_on: [], created_at: '2027-01-01T00:00:00Z' } as ReviewerProfile;
  it('is off for ordinary visitors, on with a stored profile or VITE_REVIEW_UI=1', () => {
    const store = createReviewStorage(null);
    expect(reviewSessionActive(store)).toBe(false);
    expect(reviewUiEnabled(store)).toBe(false);
    store.saveProfile(profile);
    expect(reviewSessionActive(store)).toBe(true);
    expect(reviewUiEnabled(store)).toBe(true);
    vi.stubEnv('VITE_REVIEW_UI', '1');
    expect(reviewUiEnabled(createReviewStorage(null))).toBe(true);
    vi.stubEnv('VITE_REVIEW_UI', 'true');
    expect(reviewUiEnabled(createReviewStorage(null))).toBe(false);
  });
});
