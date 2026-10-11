import { describe, expect, it } from 'vitest';
import { gunzipSync, gzipSync } from 'node:zlib';
import { buildSectorFixture, fixtureSource } from '../__fixtures__/build.ts';
import { SectorLoadError, SectorStore, httpSource } from './store.ts';

describe('SectorStore (8.10)', () => {
  const fx = buildSectorFixture();

  it('loads the manifest and validated chunks; concurrent loads share one request', async () => {
    const src = fixtureSource(fx);
    const store = new SectorStore(src);
    const [a, b] = await Promise.all([store.load('sector-facts'), store.load('sector-facts')]);
    expect(a).toBe(b);
    expect(src.calls.filter((c) => c.startsWith('sector-facts'))).toHaveLength(1);
    expect(src.calls.filter((c) => c === 'manifest.json')).toHaveLength(1);
    expect(store.chunkState('sector-facts').status).toBe('ready');
    expect(store.chunkState('sector-meta').status).toBe('ready');
    expect(store.chunkState('sector-facts').bytes).toBe(store.chunkBytes('sector-facts'));
  });

  it('a chunk not in the bundle is NOT_IN_BUNDLE', async () => {
    const store = new SectorStore(fixtureSource(fx));
    await expect(store.load('sector-atlas')).rejects.toMatchObject({ code: 'NOT_IN_BUNDLE' });
  });

  it('a failure is retryable', async () => {
    const fail = new Set(['sector-policy']);
    const store = new SectorStore(fixtureSource(fx, { fail }));
    await expect(store.load('sector-policy')).rejects.toBeInstanceOf(SectorLoadError);
    expect(store.chunkState('sector-policy').status).toBe('error');
    fail.clear();
    store.retry('sector-policy');
    await new Promise((r) => setTimeout(r, 0));
    await store.load('sector-policy');
    expect(store.chunkState('sector-policy').status).toBe('ready');
  });

  it('no manifest is NO_DATA', async () => {
    const store = new SectorStore(fixtureSource(fx, { noManifest: true }));
    await expect(store.loadManifest()).rejects.toMatchObject({ code: 'NO_DATA' });
    expect(store.manifestState().status).toBe('error');
  });

  it('a chunk that breaks the contract is CONTRACT', async () => {
    const bad = { ...fx, chunks: { ...fx.chunks, 'sector-policy-text': { 'policy.text': { nrow: 1, cols: { id: new Int32Array([1]) } } } } };
    const store = new SectorStore(fixtureSource(bad));
    await expect(store.load('sector-policy-text')).rejects.toMatchObject({ code: 'CONTRACT' });
  });

  it('notifies subscribers', async () => {
    const store = new SectorStore(fixtureSource(fx));
    let n = 0;
    const off = store.subscribe(() => n++);
    await store.load('sector-meta');
    off();
    expect(n).toBeGreaterThan(1);
  });

  it('the HTTP source: 404 is NO_DATA, a dev-server HTML fallback is NO_DATA, gzip is inflated', async () => {
    const res = (status: number, body: BodyInit) => new Response(body, { status });
    const s404 = httpSource('/data/sector/', { fetch: async () => res(404, 'nope') });
    await expect(s404.manifest()).rejects.toMatchObject({ code: 'NO_DATA' });
    const html = httpSource('/data/sector/', { fetch: async () => res(200, '<!doctype html>') });
    await expect(html.manifest()).rejects.toMatchObject({ code: 'NO_DATA' });
    const notOsb = httpSource('/data/sector/', { fetch: async () => res(200, gzipSync(Buffer.from('hello world'))) , inflate: async (b) => new Uint8Array(gunzipSync(b)) });
    await expect(notOsb.chunk('x.osb')).rejects.toMatchObject({ code: 'FORMAT' });
  });
});
