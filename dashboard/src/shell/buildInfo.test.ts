// The build-stamp store (shell/buildInfo.ts): the Strategic and Sector stamps as before, plus the Posture,
// Country, AIB and memo stamps the review system's data-release ID reads (docs/QC_SYSTEM_DESIGN.md 3.1).

import { afterEach, describe, expect, it } from 'vitest';
import { getBuildInfo, resetBuildInfo, setAibBuild, setCountryBuild, setMemosBuild, setPostureBuild, setSectorBuild, setStrategicBuild } from './buildInfo.ts';

afterEach(() => resetBuildInfo());

describe('buildInfo', () => {
  it('starts empty and resets every stamp', () => {
    expect(getBuildInfo()).toEqual({ strategic: null, sector: null, posture: null, country: null, aib: null, memos: null });
    setPostureBuild({ created: 'a', git_commit: 'b' });
    setStrategicBuild({ bundle_id: 'x', created: 'c', git_commit: null });
    resetBuildInfo();
    expect(getBuildInfo()).toEqual({ strategic: null, sector: null, posture: null, country: null, aib: null, memos: null });
  });

  it('keeps the Strategic and Sector stamps as before', () => {
    setStrategicBuild({ bundle_id: 'x', created: 'c', git_commit: null, paper: { title: 't', url: null } });
    setSectorBuild({ bundle_id: 's', created: 'd', git_commit: 'g' });
    expect(getBuildInfo().strategic).toEqual({ bundle_id: 'x', created: 'c', git_commit: null, paper: { title: 't', url: null } });
    expect(getBuildInfo().sector).toEqual({ bundle_id: 's', created: 'd', git_commit: 'g' });
  });

  it('reads created and git_commit structurally from a manifest, null when absent or malformed', () => {
    setPostureBuild({ schema: 'osi-posture', created: '2026-11-17T20:33:31Z', git_commit: '0496e7ba', core: 'c.json' });
    setCountryBuild({ created: '2026-10-31T15:58:15Z' });
    setAibBuild({ created: '2026-11-17T19:07:29Z', git_commit: '03560172' });
    setMemosBuild('');
    const b = getBuildInfo();
    expect(b.posture).toEqual({ created: '2026-11-17T20:33:31Z', git_commit: '0496e7ba' });
    expect(b.country).toEqual({ created: '2026-10-31T15:58:15Z', git_commit: null });
    expect(b.aib).toEqual({ created: '2026-11-17T19:07:29Z', git_commit: '03560172' });
    expect(b.memos).toEqual({ manifest_sha256: null });
    setPostureBuild(null);
    expect(getBuildInfo().posture).toEqual({ created: null, git_commit: null });
  });

  it('does not emit a new state for an unchanged stamp', () => {
    setAibBuild({ created: 'x' });
    const before = getBuildInfo();
    setAibBuild({ created: 'x' });
    expect(getBuildInfo()).toBe(before);
  });
});
