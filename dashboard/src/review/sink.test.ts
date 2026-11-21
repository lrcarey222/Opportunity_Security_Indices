// Where submissions go (review/sink.ts): local storage, a downloaded packet, or a POST to the configured
// endpoint (no credentials, a timeout, the local copy kept). No payload is logged.

import { afterEach, describe, expect, it, vi } from 'vitest';
import type { Submission } from './model.ts';
import { HttpSink, LocalSink, PACKET_SCHEMA, PacketSink, buildPacket, getSink, packetFileName, sinkMode } from './sink.ts';
import { createReviewStorage } from './storage.ts';

const sub = (id = 'SUB-1', reviewer = 'rev-1'): Submission =>
  ({
    submission_id: id,
    schema: 'aib-review-submission',
    schema_version: 1,
    campaign_id: 'c',
    kind: 'data_issue',
    reviewer: { reviewer_id: reviewer, name: 'Private Name', anonymous: false, profile_digest: 'x' },
    created_at: '2027-01-20T09:00:00.000Z',
    submitted_at: '2027-01-20T09:05:00.000Z',
    app_commit: 'unknown',
    data_release: null,
    page_context: null,
    payload: { metric: null, what_is_wrong: 'secret comment', severity: 'minor' },
  }) as Submission;

afterEach(() => vi.restoreAllMocks());

describe('packets', () => {
  it('holds one reviewer and is named by the pseudonymous code and time, never the name', () => {
    const p = buildPacket([sub('SUB-1'), sub('SUB-2')]);
    expect(p).toMatchObject({ schema: PACKET_SCHEMA, schema_version: 1, reviewer_id: 'rev-1' });
    expect(p.submissions).toHaveLength(2);
    const name = packetFileName(p);
    expect(name).toMatch(/^aib-review-R-[0-9A-F]{8}-20270120T090500\.json$/);
    expect(name).not.toMatch(/Private|rev-1/);
    expect(() => buildPacket([])).toThrow();
    expect(() => buildPacket([sub('A', 'r1'), sub('B', 'r2')])).toThrow();
  });
});

describe('LocalSink and PacketSink', () => {
  it('LocalSink stores the submission in the review store', async () => {
    const store = createReviewStorage(null);
    const r = await new LocalSink(store).submit(sub());
    expect(r).toMatchObject({ ok: true, id: 'SUB-1' });
    expect(store.listSubmissions().map((s) => s.submission_id)).toEqual(['SUB-1']);
  });
  it('PacketSink downloads a JSON packet and keeps a local copy', async () => {
    const store = createReviewStorage(null);
    const download = vi.fn();
    const r = await new PacketSink({ store, download }).submit(sub());
    expect(r.ok).toBe(true);
    expect(download).toHaveBeenCalledTimes(1);
    const [text, filename, type] = download.mock.calls[0];
    expect(JSON.parse(text)).toMatchObject({ schema: 'aib-review-packet', reviewer_id: 'rev-1', submissions: [{ submission_id: 'SUB-1' }] });
    expect(filename).toMatch(/\.json$/);
    expect(type).toBe('application/json');
    expect(store.listSubmissions()).toHaveLength(1);
  });
  it('PacketSink reports a failed download instead of throwing', async () => {
    const r = await new PacketSink({ store: null, download: () => { throw new Error('blocked'); } }).submit(sub());
    expect(r.ok).toBe(false);
    expect(r.message).toMatch(/blocked/);
  });
});

describe('HttpSink', () => {
  it('POSTs JSON without credentials or referrer', async () => {
    const fetcher = vi.fn(async () => ({ ok: true, status: 201 }));
    const r = await new HttpSink({ endpoint: 'https://example.org/review', fetcher }).submit(sub());
    expect(r).toMatchObject({ ok: true, id: 'SUB-1' });
    const [url, init] = fetcher.mock.calls[0] as unknown as [string, RequestInit];
    expect(url).toBe('https://example.org/review');
    expect(init.method).toBe('POST');
    expect(init.credentials).toBe('omit');
    expect(init.referrerPolicy).toBe('no-referrer');
    expect((init.headers as Record<string, string>)['Content-Type']).toBe('application/json');
    expect(JSON.parse(init.body as string).submission_id).toBe('SUB-1');
  });
  it('refuses without an http(s) endpoint, reports HTTP errors and timeouts, and logs nothing', async () => {
    const log = vi.spyOn(console, 'log');
    const err = vi.spyOn(console, 'error');
    expect((await new HttpSink({ endpoint: null }).submit(sub())).ok).toBe(false);
    expect((await new HttpSink({ endpoint: 'javascript:alert(1)' }).submit(sub())).ok).toBe(false);
    const bad = await new HttpSink({ endpoint: 'https://x.test', fetcher: async () => ({ ok: false, status: 429 }) }).submit(sub());
    expect(bad).toMatchObject({ ok: false });
    expect(bad.message).toMatch(/429/);
    const slow = new HttpSink({
      endpoint: 'https://x.test',
      timeoutMs: 10,
      fetcher: (_u, init) =>
        new Promise((_res, rej) => {
          init.signal?.addEventListener('abort', () => rej(Object.assign(new Error('aborted'), { name: 'AbortError' })));
        }),
    });
    const t = await slow.submit(sub());
    expect(t.ok).toBe(false);
    expect(t.message).toMatch(/in time/);
    for (const spy of [log, err]) for (const call of spy.mock.calls) expect(JSON.stringify(call)).not.toMatch(/secret comment|Private Name/);
  });
});

describe('getSink', () => {
  it('chooses by VITE_REVIEW_SINK, defaulting to local', () => {
    expect(sinkMode({})).toBe('local');
    expect(sinkMode({ VITE_REVIEW_SINK: 'packet' })).toBe('packet');
    expect(sinkMode({ VITE_REVIEW_SINK: 'ftp' })).toBe('local');
    const store = createReviewStorage(null);
    expect(getSink({}, store).mode).toBe('local');
    expect(getSink({ VITE_REVIEW_SINK: 'packet' }, store).mode).toBe('packet');
    expect(getSink({ VITE_REVIEW_SINK: 'http', VITE_REVIEW_ENDPOINT: 'https://x.test' }, store).mode).toBe('http');
  });
  it('the http sink keeps a local copy even when the POST fails', async () => {
    const store = createReviewStorage(null);
    vi.stubGlobal('fetch', vi.fn(async () => { throw new Error('offline'); }));
    const r = await getSink({ VITE_REVIEW_SINK: 'http', VITE_REVIEW_ENDPOINT: 'https://x.test' }, store).submit(sub());
    vi.unstubAllGlobals();
    expect(r.ok).toBe(false);
    expect(store.listSubmissions()).toHaveLength(1);
  });
});
