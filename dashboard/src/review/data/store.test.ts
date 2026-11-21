// The review data store (review/data/store.ts): the HTTP source's error codes, one shared promise per file,
// a retry after a failure, the shape checks, the campaign built from a manifest entry (and its fallback rules,
// kept equal to the campaign YAML), and the committed public/data/review files passing the checks.

import { describe, expect, it, vi } from 'vitest';
import { existsSync, readFileSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { parse } from 'yaml';
import {
  campaignOf,
  checkCasesFile,
  checkReviewManifest,
  FALLBACK_ASSIGNMENT,
  FALLBACK_TARGET_REVIEWS_PER_ANCHOR,
  httpReviewSource,
  memoryReviewSource,
  pickCampaign,
  ReviewLoadError,
  ReviewStore,
  type ReviewCampaignEntry,
  type ReviewManifestDoc,
} from './store.ts';

const here = dirname(fileURLToPath(import.meta.url));
const DASH = join(here, '..', '..', '..');
const DATA = join(DASH, 'public', 'data', 'review');
const REPO = join(DASH, '..');

const entry = (over: Partial<ReviewCampaignEntry> = {}): ReviewCampaignEntry => ({
  campaign_id: 'c1',
  name: 'Round 1',
  status: 'draft',
  opens: null,
  closes: null,
  review_types: ['case'],
  n_cases: 1,
  anchor_cases: ['CASE_001'],
  cases_file: 'cases.c1.json',
  method_card_ids: [],
  framework_scenario_ids: [],
  ...over,
});

const manifest = (campaigns: ReviewCampaignEntry[] = [entry()]): ReviewManifestDoc => ({
  schema: 'aib-review-manifest',
  schema_version: 1,
  built_at: '2026-11-20T00:00:00Z',
  app_commit: 'abc',
  data_release: { id: 'rel-000000000000', stamps: { posture: null, country: null, sector: null, aib: null, strategic: null, memos: null }, complete: false, missing: [] },
  campaigns,
  files: [],
});

const res = (status: number, body: string) => ({ status, ok: status >= 200 && status < 300, text: async () => body }) as unknown as Response;

describe('httpReviewSource', () => {
  it('fetches under the base, revalidates the manifest only, and maps 404 / non-JSON to codes', async () => {
    const f = vi.fn(async (url: string) => (url.endsWith('manifest.json') ? res(200, '{"a":1}') : url.endsWith('missing.json') ? res(404, '') : res(200, '<html>')));
    const src = httpReviewSource('/x/data/review/', { fetch: f });
    expect(await src.json('manifest.json')).toEqual({ a: 1 });
    expect(f.mock.calls[0]).toEqual(['/x/data/review/manifest.json', { cache: 'no-cache' }]);
    await expect(src.json('missing.json')).rejects.toMatchObject({ code: 'NETWORK' });
    await expect(src.json('cases.json')).rejects.toMatchObject({ code: 'CONTRACT' });
    const none = httpReviewSource('/', { fetch: async () => res(404, '') });
    await expect(none.json('manifest.json')).rejects.toMatchObject({ code: 'NO_DATA' });
    const down = httpReviewSource('/', { fetch: async () => { throw new Error('offline'); } });
    await expect(down.json('manifest.json')).rejects.toMatchObject({ code: 'NETWORK' });
  });
});

describe('ReviewStore', () => {
  it('shares one promise per file and keeps the settled value', async () => {
    const store = new ReviewStore(memoryReviewSource({ 'manifest.json': manifest() }));
    const [a, b] = await Promise.all([store.manifest(), store.manifest()]);
    expect(a).toBe(b);
    expect(store.calls).toEqual(['manifest.json']);
    expect(store.peek('manifest.json')).toBe(a);
  });

  it('drops a failed load so a retry refetches', async () => {
    let n = 0;
    const store = new ReviewStore({
      json: async () => {
        n += 1;
        if (n === 1) throw new Error('flaky');
        return manifest();
      },
    });
    await expect(store.manifest()).rejects.toBeInstanceOf(ReviewLoadError);
    await expect(store.manifest()).resolves.toMatchObject({ schema: 'aib-review-manifest' });
    expect(store.calls).toEqual(['manifest.json', 'manifest.json']);
  });

  it('rejects a file that does not match the shape, and a missing manifest as NO_DATA', async () => {
    const bad = new ReviewStore(memoryReviewSource({ 'manifest.json': { schema: 'nope' }, 'cases.c1.json': { schema: 'aib-review-cases', campaign_id: 'c1', cases: [{ case_id: 'CASE_001' }] } }));
    await expect(bad.manifest()).rejects.toMatchObject({ code: 'CONTRACT' });
    await expect(bad.cases(entry())).rejects.toMatchObject({ code: 'CONTRACT' });
    await expect(new ReviewStore(memoryReviewSource({})).manifest()).rejects.toMatchObject({ code: 'NO_DATA' });
  });
});

describe('campaigns', () => {
  it('picks the named campaign, else the first active, else the first', () => {
    const m = manifest([entry({ campaign_id: 'a' }), entry({ campaign_id: 'b', status: 'active' })]);
    expect(pickCampaign(m)?.campaign_id).toBe('b');
    expect(pickCampaign(m, 'a')?.campaign_id).toBe('a');
    expect(pickCampaign(m, 'zzz')?.campaign_id).toBe('b');
    expect(pickCampaign(manifest([]))).toBeNull();
  });

  it('builds the Campaign from the entry, with the fallback rules named when the build does not carry them', () => {
    const { campaign, fallback } = campaignOf(entry(), { cases: [{ case_id: 'CASE_001' }, { case_id: 'CASE_002' }] as never });
    expect(campaign.case_ids).toEqual(['CASE_001', 'CASE_002']);
    expect(campaign.assignment).toEqual(FALLBACK_ASSIGNMENT);
    expect(campaign.target_reviews_per_anchor).toBe(FALLBACK_TARGET_REVIEWS_PER_ANCHOR);
    expect(fallback).toEqual(['assignment', 'target_reviews_per_anchor']);
    const own = { ...FALLBACK_ASSIGNMENT, out_of_specialty: 'none' as const };
    const r = campaignOf(entry({ assignment: own, target_reviews_per_anchor: 6, case_ids: ['CASE_002'] }), null);
    expect(r.fallback).toEqual([]);
    expect(r.campaign.assignment.out_of_specialty).toBe('none');
    expect(r.campaign.case_ids).toEqual(['CASE_002']);
  });

  const yml = join(REPO, 'config', 'review', 'campaigns', 'aib_qc_2027_round1.yml');
  it.skipIf(!existsSync(yml))('the fallback rules equal the round-1 campaign YAML (a drift guard while the manifest omits them)', () => {
    const c = parse(readFileSync(yml, 'utf8')) as { assignment: unknown; target_reviews_per_anchor: number };
    expect(c.assignment).toEqual(FALLBACK_ASSIGNMENT);
    expect(c.target_reviews_per_anchor).toBe(FALLBACK_TARGET_REVIEWS_PER_ANCHOR);
  });
});

describe.skipIf(!existsSync(join(DATA, 'manifest.json')))('the committed review data', () => {
  it('passes the manifest and cases checks', () => {
    const m = JSON.parse(readFileSync(join(DATA, 'manifest.json'), 'utf8')) as ReviewManifestDoc;
    expect(checkReviewManifest(m)).toEqual([]);
    for (const c of m.campaigns) {
      const f = join(DATA, c.cases_file);
      expect(existsSync(f), c.cases_file).toBe(true);
      expect(checkCasesFile(JSON.parse(readFileSync(f, 'utf8')))).toEqual([]);
    }
  });
});
