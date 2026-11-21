// Page context and submission envelopes (review/context.ts): what a report records from main's routes
// (Countries profile with ?node= / ?layer=, Sector pages, AIB and compact pages, the redesign-only routes
// parsed for old packets), the bundle stamps, and stable unique submission IDs with the reviewer stamp.

import { describe, expect, it } from 'vitest';
import type { BuildInfo } from '../shell/buildInfo.ts';
import { SUBMISSION_SCHEMA, type DataIssuePayload, type ReviewerProfile } from './model.ts';
import { capturePageContext, matchRoute, newSubmission, profileDigest, reviewerStamp } from './context.ts';

const INFO: BuildInfo = {
  strategic: { bundle_id: 'str1', created: '2026-10-30', git_commit: 'a54a9d3', paper: null },
  sector: { bundle_id: 'sec1', created: '2026-10-31', git_commit: '4bb2adc' },
  posture: { created: '2026-11-17T20:33:31Z', git_commit: '0496e7ba' },
  country: { created: '2026-10-31T15:58:15Z', git_commit: '4bb2adc' },
  aib: { created: '2026-11-17T19:07:29Z' },
  memos: null,
};
const AT = { captured_at: '2027-01-20T09:00:00.000Z' };
const cap = (url: string, params: Record<string, string> = {}, info: BuildInfo | null = INFO) => {
  const u = new URL(url, 'https://x.test');
  return capturePageContext({ pathname: u.pathname, search: u.search, hash: u.hash }, params, info, AT);
};

describe('capturePageContext on main routes', () => {
  it('the Country profile with the cell drawer open on a layer', () => {
    const c = cap('/countries/kor?node=Batteries%7CRefining&layer=cathode-precursor#decision');
    expect(c).toMatchObject({ route: '/countries/:iso3', view: 'country', country: 'KOR', tech: 'batteries', stage: 'Refining', node: 'Batteries|Refining', layer: 'cathode-precursor', scenario: null });
    expect(c.url).toBe('/countries/kor?node=Batteries%7CRefining&layer=cathode-precursor#decision');
  });
  it('a Sector country page and a Sector profile with ?c= and ?pnode=', () => {
    expect(cap('/sector/electric-vehicles/assembly/IND')).toMatchObject({ route: '/sector/:tech/:stage/:iso3', view: 'sector-country', country: 'IND', tech: 'electric-vehicles', stage: 'Assembly' });
    expect(cap('/sector/solar/refining?c=chn&pnode=Solar|Refining')).toMatchObject({ route: '/sector/:tech/:stage', country: 'CHN', tech: 'solar', stage: 'Refining', node: 'Solar|Refining' });
    expect(cap('/sector/electric-grid')).toMatchObject({ route: '/sector/:tech', view: 'sector-chain', tech: 'electric-grid', stage: null });
    expect(cap('/sector/minerals')).toMatchObject({ route: '/sector/minerals', tech: null });
  });
  it('router params win over the pathname parse', () => {
    expect(cap('/sector/solar/refining/CHN', { tech: 'wind', stage: 'components', iso3: 'deu' })).toMatchObject({ tech: 'wind', stage: 'Components', country: 'DEU' });
  });
  it('strategic and operational compacts, with the AIB default scenario', () => {
    expect(cap('/compacts/s/SC-BATTERIES?scn=decoupling')).toMatchObject({ route: '/compacts/s/:sid', view: 'compact-strategic', sid: 'SC-BATTERIES', scenario: 'decoupling' });
    expect(cap('/compacts/s/SC-GRID')).toMatchObject({ sid: 'SC-GRID', scenario: 'managed' });
    expect(cap('/compacts/batteries-cells')).toMatchObject({ route: '/compacts/:id', sid: 'batteries-cells' });
    expect(cap('/compacts/workstreams')).toMatchObject({ route: '/compacts/workstreams', sid: null });
    expect(cap('/aib/roles')).toMatchObject({ route: '/aib/roles', view: 'aib-roles', scenario: 'managed' });
    expect(cap('/aib?scn=decoupling').scenario).toBe('decoupling');
  });
  it('redesign-only routes (not on main) still yield pair and compact', () => {
    expect(cap('/partnerships/usa_jpn')).toMatchObject({ route: '/partnerships/:pair', pair: 'USA_JPN' });
    expect(cap('/compacts/brief/SC-NUCLEAR')).toMatchObject({ route: '/compacts/brief/:sid', sid: 'SC-NUCLEAR' });
  });
  it('About and legacy pages, the home page', () => {
    expect(cap('/methods')).toMatchObject({ route: '/methods', view: 'methods', country: null, scenario: null });
    expect(cap('/strategic/JPN')).toMatchObject({ route: '/strategic/*', country: 'JPN' });
    expect(cap('/')).toMatchObject({ route: '/', view: 'home' });
    expect(matchRoute('/chart-of-the-day/2026-11-17')).toMatchObject({ route: '/chart-of-the-day/:date' });
  });
  it('records the bundle stamps, the release, the result shown and the metrics', () => {
    const c = capturePageContext({ pathname: '/countries/USA' }, {}, INFO, { ...AT, app_commit: 'abc1234', data_release: 'rel-0123456789ab', displayed_result: 'Partner · Medium fence', metric_ids: ['cp', 'need'], view: 'cell-drawer' });
    expect(c.bundle_stamps.posture).toEqual({ created: '2026-11-17T20:33:31Z', git_commit: '0496e7ba' });
    expect(c.bundle_stamps.aib).toEqual({ created: '2026-11-17T19:07:29Z', git_commit: null });
    expect(c.bundle_stamps.memos).toBeNull();
    expect(c).toMatchObject({ app_commit: 'abc1234', data_release: 'rel-0123456789ab', displayed_result: 'Partner · Medium fence', metric_ids: ['cp', 'need'], view: 'cell-drawer', captured_at: AT.captured_at });
    const none = capturePageContext({ pathname: '/countries/USA' }, {}, null, AT);
    expect(none.app_commit).toBe('unknown');
    expect(Object.values(none.bundle_stamps).every((v) => v === null)).toBe(true);
  });
  it('survives malformed input', () => {
    expect(() => cap('/countries/%E0%A4%A?node=%7C&layer=')).not.toThrow();
    const c = cap('/countries/12?node=|');
    expect(c.country).toBeNull();
    expect(c.layer).toBeNull();
  });
});

const profile: ReviewerProfile = {
  reviewer_id: 'rev-1',
  name: 'Ada Example',
  anonymous: false,
  organization: 'Ministry',
  countries: ['kor'],
  sectors: ['Batteries'],
  stages: [],
  methods: ['trade_data'],
  confidence: { 'sector:batteries': 5, 'country:KOR': 4, 'method:trade_data': 3 },
  familiarity_neis: 2,
  familiarity_explorer: 1,
  advised_on: ['governments'],
  created_at: '2027-01-01T00:00:00Z',
};
const payload: DataIssuePayload = { metric: null, what_is_wrong: 'x', severity: 'minor' };

describe('newSubmission and the reviewer stamp', () => {
  it('wraps the payload with a stable unique id, the release and the reviewer stamp', () => {
    let i = 0;
    const uuid = () => `00000000-0000-4000-8000-00000000000${++i}`;
    const opts = { profile, campaign_id: 'round1', page_context: null, release: { data_release: 'rel-0123456789ab', app_commit: 'abc1234' }, now: '2027-01-20T09:00:00.000Z', uuid };
    const a = newSubmission('data_issue', payload, opts);
    const b = newSubmission('data_issue', payload, opts);
    expect(a.submission_id).toBe('SUB-00000000-0000-4000-8000-000000000001');
    expect(b.submission_id).not.toBe(a.submission_id);
    expect(a).toMatchObject({ schema: SUBMISSION_SCHEMA, schema_version: 1, campaign_id: 'round1', kind: 'data_issue', created_at: opts.now, submitted_at: opts.now, app_commit: 'abc1234', data_release: 'rel-0123456789ab', payload });
    expect(a.reviewer).toMatchObject({ reviewer_id: 'rev-1', name: 'Ada Example', anonymous: false, expertise: { sectors: ['batteries'], countries: ['KOR'], methods: ['trade_data'] } });
    expect(newSubmission('data_issue', payload, { ...opts, submitted: false }).submitted_at).toBeNull();
    // the default random source gives distinct ids
    const { uuid: _u, ...noUuid } = opts;
    void _u;
    expect(newSubmission('data_issue', payload, noUuid).submission_id).not.toBe(newSubmission('data_issue', payload, noUuid).submission_id);
  });
  it('the profile digest ignores the name and tag spelling, not the expertise', () => {
    const d = profileDigest(profile);
    expect(d).toMatch(/^[0-9a-f]{64}$/);
    expect(profileDigest({ ...profile, name: 'Someone Else' })).toBe(d);
    expect(profileDigest({ ...profile, sectors: ['batteries'], countries: ['KOR'] })).toBe(d);
    expect(profileDigest({ ...profile, confidence: { ...profile.confidence, 'sector:batteries': 2 } })).not.toBe(d);
  });
  it('an anonymous reviewer stamp never carries a name', () => {
    expect(reviewerStamp({ ...profile, anonymous: true }).name).toBeNull();
    expect(JSON.stringify(reviewerStamp({ ...profile, anonymous: true }))).not.toMatch(/Ada Example/);
  });
});
