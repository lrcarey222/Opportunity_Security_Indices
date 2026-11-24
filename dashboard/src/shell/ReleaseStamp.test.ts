// The version stamp (shell/ReleaseStamp.tsx): the data-release date is the newest posture or Allied Industrial
// Base export; the version line names the method, the app and each bundle the page knows, and leaves out a
// bundle not loaded; the AIB commit is kept.

import { describe, expect, it } from 'vitest';
import type { BuildInfo } from './buildInfo.ts';
import { dataReleaseDate, METHOD_VERSION, releaseRows, versionLine, versionParts } from './ReleaseStamp.tsx';

const EMPTY: BuildInfo = { strategic: null, sector: null, posture: null, country: null, aib: null, memos: null };
const FULL: BuildInfo = {
  strategic: { bundle_id: 'c1736c4a25c7', created: '2026-10-30T21:17:20Z', git_commit: 'a54a9d3', paper: null },
  sector: { bundle_id: '8f4910df5216', created: '2026-10-31T15:58:13Z', git_commit: '4bb2adc' },
  posture: { created: '2026-11-21T17:46:30Z', git_commit: '17f0737e' },
  country: { created: '2026-10-31T15:58:15Z', git_commit: '4bb2adc' },
  aib: { created: '2026-11-21T14:34:52Z', git_commit: '03560172' },
  memos: null,
};

describe('the version stamp', () => {
  it('the data release is the newest posture or AIB export, or null when neither is known', () => {
    expect(dataReleaseDate(FULL)).toBe('2026-11-21');
    expect(dataReleaseDate({ posture: { created: '2026-11-17T20:33:31Z', git_commit: null }, aib: { created: '2026-11-21T14:34:52Z' } })).toBe('2026-11-21');
    expect(dataReleaseDate({ posture: null, aib: { created: 'not a date' } })).toBeNull();
    expect(dataReleaseDate(EMPTY)).toBeNull();
  });

  it('the footer line: method, app, posture, AIB and sector; the AIB commit is shown', () => {
    expect(versionLine(versionParts(FULL, 'abc1234'))).toBe(
      `Method version: ${METHOD_VERSION} · app abc1234 · posture 17f0737e · Allied Industrial Base 03560172 · sector 8f4910df`,
    );
    // a bundle the page has not loaded is left out; an unknown app build says so
    expect(versionLine(versionParts({ ...EMPTY, sector: FULL.sector }, null))).toBe(`Method version: ${METHOD_VERSION} · app unknown · sector 8f4910df`);
    // a stamp without a commit falls back to its export date
    expect(versionParts({ ...EMPTY, aib: { created: '2026-11-21T14:34:52Z' } }, null).find((p) => p.key === 'aib')?.value).toBe('2026-11-21');
  });

  it('the Methods and Data rows add the country bundle and the security and opportunity scores build', () => {
    expect(releaseRows(FULL, 'abc1234')).toEqual([
      ['Data release', '2026-11-21'],
      ['Method version', METHOD_VERSION],
      ['App build', 'abc1234'],
      ['Strategic choices (posture)', '17f0737e'],
      ['Allied Industrial Base', '03560172'],
      ['Sector data', '8f4910df'],
      ['Country data', '4bb2adc'],
      ['Security and opportunity scores', 'c1736c4a'],
    ]);
    expect(releaseRows(EMPTY, null)[0]).toEqual(['Data release', 'not loaded']);
  });
});
