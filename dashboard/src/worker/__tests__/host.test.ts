// The worker's host and the main-thread client, in process (Node), on the committed synthetic
// bundle s1: loading, baseline and scenario runs (checked against the R fixtures through the whole
// worker path), caching, superseding, requests, the self-check and errors.

import { describe, expect, it } from 'vitest';
import { readFileSync } from 'node:fs';
import { join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { gunzipSync } from 'node:zlib';
import { EngineHost, type FetchBytes } from '../host.ts';
import { EngineClient, type WorkerLike } from '../client.ts';
import type { FromWorker, ToWorker } from '../protocol.ts';
import { compareTable, readOsbFile, get, report, type Diff } from '../../engine/__tests__/parity/harness.ts';
import { num, str } from '../../engine/table.ts';
import type { ScenarioSpec, Table } from '../../engine/types.ts';

const setDir = fileURLToPath(new URL('../../engine/__fixtures__/synthetic/s1/', import.meta.url));
const dataBase = join(setDir, 'data') + '/';

const fetchDisk: FetchBytes = async (url) => {
  const b = readFileSync(url);
  const out = new Uint8Array(b.length);
  out.set(b);
  return out;
};
const inflate = async (b: Uint8Array) => new Uint8Array(gunzipSync(b));

function makeHost(opts: { selfCheck?: boolean; fetch?: FetchBytes } = {}) {
  const msgs: FromWorker[] = [];
  const host = new EngineHost((m) => msgs.push(structuredClone(m)), opts.fetch ?? fetchDisk, inflate, { selfCheck: opts.selfCheck ?? false });
  return { host, msgs };
}

const of = <T extends FromWorker['type']>(msgs: FromWorker[], type: T) => msgs.filter((m) => m.type === type) as Extract<FromWorker, { type: T }>[];
const results = (msgs: FromWorker[], id: number) => msgs.filter((m): m is Extract<FromWorker, { type: 'result' }> => m.type === 'result' && m.id === id);

function fixture(id: string): Record<string, unknown> {
  return readOsbFile(join(setDir, `${id}.osb`)).objects;
}

function specOf(id: string): ScenarioSpec {
  const index = JSON.parse(readFileSync(join(setDir, 'index.json'), 'utf8')) as { scenarios: { id: string; dials: ScenarioSpec['dials']; advanced: unknown }[] };
  const s = index.scenarios.find((x) => x.id === id)!;
  return { dials: s.dials, advanced: Array.isArray(s.advanced) ? {} : (s.advanced as ScenarioSpec['advanced']) };
}

function noDiffs(diffs: Diff[]) {
  if (diffs.length) throw new Error('\n' + report(diffs));
}

/** Producer sets per stage of a specialization table. */
function producers(t: Table): string[] {
  const T = str(t, 'tech');
  const S = str(t, 'supply_chain');
  const I = str(t, 'iso3c');
  const sh = num(t, 'production_share');
  const out: string[] = [];
  for (let i = 0; i < t.nrow; i++) if (sh[i] > 0) out.push(`${T[i]}|${S[i]}|${I[i]}`);
  return out.sort();
}

describe('EngineHost', () => {
  it('init: loads the manifest and the core chunk, posts ready with the shipped baseline', async () => {
    const { host, msgs } = makeHost();
    await host.handle({ type: 'init', dataBase });
    const ready = of(msgs, 'ready');
    expect(ready).toHaveLength(1);
    expect(ready[0].manifest.schema_version).toBe(1);
    expect(ready[0].dims.U.length).toBeGreaterThan(0);
    expect(ready[0].baseline.si.cells.nrow).toBeGreaterThan(0);
    expect(ready[0].baseline.readout.nrow).toBeGreaterThan(0);
    expect(of(msgs, 'chunk').map((c) => `${c.name}:${c.state}`)).toEqual(['core:loading', 'core:loaded']);
  });

  it('baseline run: the shipped tables (exact), without computing', async () => {
    const { host, msgs } = makeHost();
    await host.handle({ type: 'init', dataBase });
    await host.handle({ type: 'run', id: 1, spec: { dials: {} }, want: { partner: true, network: true } });
    const r = results(msgs, 1);
    expect(r.map((m) => `${m.layer}:${m.source}`)).toEqual(['core:baseline', 'partner:baseline', 'network:baseline']);
    const fx = fixture('S00');
    const diffs: Diff[] = [];
    const core = r[0].payload as Extract<FromWorker, { type: 'result'; layer: 'core' }>['payload'];
    expect(core.is_baseline).toBe(true);
    compareTable('L3.cells', core.si.cells, get(fx, 'L3.cells') as Table, 0, diffs);
    const partner = r[1].payload as Extract<FromWorker, { type: 'result'; layer: 'partner' }>['payload'];
    compareTable('L4.psi_partner', partner.psi.partner, get(fx, 'L4.psi_partner') as Table, 0, diffs);
    const net = r[2].payload as Extract<FromWorker, { type: 'result'; layer: 'network' }>['payload'];
    expect(net.method).toBe('greedy');
    expect(producers(net.solution.specialization)).toEqual(producers(get(fx, 'L5.solution.specialization') as Table));
    noDiffs(diffs);
    expect(of(msgs, 'done').map((m) => m.id)).toEqual([1]);
  });

  it('scenario run: every layer through the worker path equals the R fixture (S11, all four dials)', async () => {
    const { host, msgs } = makeHost();
    await host.handle({ type: 'init', dataBase });
    await host.handle({ type: 'run', id: 1, spec: specOf('S11'), want: { partner: true, network: true, contrib: true } });
    const err = of(msgs, 'error');
    if (err.length) throw new Error(err.map((e) => e.message).join('\n'));
    const r = results(msgs, 1);
    expect(r.map((m) => `${m.layer}:${m.source}`)).toEqual(['core:computed', 'partner:computed', 'network:computed']);
    const fx = fixture('S11');
    const diffs: Diff[] = [];
    const core = r[0].payload as Extract<FromWorker, { type: 'result'; layer: 'core' }>['payload'];
    expect(core.is_baseline).toBe(false);
    compareTable('L1.es', core.pillars.es, get(fx, 'L1.es') as Table, 1e-9, diffs);
    compareTable('L1.es_contrib', core.pillars.es_contrib, get(fx, 'L1.es_contrib') as Table, 1e-9, diffs);
    compareTable('L2.index', core.policy.index, get(fx, 'L2.index') as Table, 1e-9, diffs);
    compareTable('L3.cells', core.si.cells, get(fx, 'L3.cells') as Table, 1e-9, diffs);
    compareTable('compare.si', core.compare.si, get(fx, 'compare.si') as Table, 1e-9, diffs);
    const partner = r[1].payload as Extract<FromWorker, { type: 'result'; layer: 'partner' }>['payload'];
    compareTable('L4.psi_partner', partner.psi.partner, get(fx, 'L4.psi_partner') as Table, 1e-12, diffs);
    compareTable('compare.psi_partner', partner.compare.psi_partner, get(fx, 'compare.psi_partner') as Table, 1e-12, diffs);
    const net = r[2].payload as Extract<FromWorker, { type: 'result'; layer: 'network' }>['payload'];
    expect(producers(net.solution.specialization)).toEqual(producers(get(fx, 'L5.solution.specialization') as Table));
    compareTable('L5.solution.specialization', net.solution.specialization, get(fx, 'L5.solution.specialization') as Table, 1e-6, diffs);
    compareTable('L5.compare.stage', net.compare!.stage, get(fx, 'L5.compare.stage') as Table, 1e-6, diffs);
    compareTable('L5.concentration', net.concentration, get(fx, 'L5.concentration') as Table, 1e-6, diffs);
    noDiffs(diffs);
    const prog = of(msgs, 'progress');
    expect(prog.some((p) => p.layer === 'partner')).toBe(true);
    expect(prog.some((p) => p.layer === 'network' && p.tech)).toBe(true);
  });

  it('caches layers: the same scenario again is served from the cache', async () => {
    const { host, msgs } = makeHost();
    await host.handle({ type: 'init', dataBase });
    const spec = specOf('S02');
    await host.handle({ type: 'run', id: 1, spec, want: { partner: true, network: true } });
    await host.handle({ type: 'run', id: 2, spec, want: { partner: true, network: true } });
    expect(results(msgs, 1).map((m) => m.cached)).toEqual([false, false, false]);
    expect(results(msgs, 2).map((m) => m.cached)).toEqual([true, true, true]);
    // a populism move keeps the FS / PO cells (no partner progress for the cells)
    msgs.length = 0;
    await host.handle({ type: 'run', id: 3, spec: { dials: { ...spec.dials, populism: 70 } }, want: { partner: true } });
    expect(of(msgs, 'progress').filter((p) => p.phase === 'friendshore')).toHaveLength(0);
  });

  it('a newer run supersedes an older one at its next yield point', async () => {
    const { host, msgs } = makeHost();
    await host.handle({ type: 'init', dataBase });
    const a = host.handle({ type: 'run', id: 1, spec: specOf('S01'), want: { partner: true, network: true } });
    const b = host.handle({ type: 'run', id: 2, spec: specOf('S04'), want: { partner: true, network: true } });
    await Promise.all([a, b]);
    expect(of(msgs, 'done').map((m) => m.id)).toEqual([2]);
    expect(results(msgs, 1).filter((m) => m.layer !== 'core')).toHaveLength(0);
    expect(results(msgs, 2).map((m) => m.layer)).toEqual(['core', 'partner', 'network']);
    expect(of(msgs, 'error')).toHaveLength(0);
  });

  it('cancel drops a run', async () => {
    const { host, msgs } = makeHost();
    await host.handle({ type: 'init', dataBase });
    const a = host.handle({ type: 'run', id: 1, spec: specOf('S01'), want: { partner: true, network: true } });
    await host.handle({ type: 'cancel', id: 1 });
    await a;
    expect(of(msgs, 'done')).toHaveLength(0);
  });

  it('requests: variables, decompose, dyads, tables, load', async () => {
    const { host, msgs } = makeHost();
    await host.handle({ type: 'init', dataBase });
    const fx = fixture('S11');
    const spec = specOf('S11');
    const reply = (id: number) => of(msgs, 'reply').find((m) => m.id === id)?.payload;
    const want = get(fx, 'L1.var.es') as Table;
    const country = str(want, 'Country')[0] as string;
    await host.handle({ type: 'variables', id: 1, spec, pillar: 'es', country });
    const v = reply(1) as Table;
    expect(v.nrow).toBeGreaterThan(0);
    expect(new Set(str(v, 'Country'))).toEqual(new Set([country]));
    const dy = get(fx, 'L4.decompose_dyads') as Table;
    const rep = str(dy, 'reporter_iso')[0] as string;
    const par = str(dy, 'partner_iso')[0] as string;
    await host.handle({ type: 'decompose', id: 2, spec, reporter: rep, partner: par });
    const dec = reply(2) as Table;
    const wantDec = get(fx, 'L4.decompose') as Table;
    const first = Array.from({ length: wantDec.nrow }, (_, i) => i).filter((i) => str(wantDec, 'reporter_iso')[i] === rep && str(wantDec, 'partner_iso')[i] === par);
    expect(dec.nrow).toBe(first.length);
    await host.handle({ type: 'dyads', id: 3, spec, reporter: rep });
    const d = reply(3) as Table;
    expect(d.nrow).toBeGreaterThan(0);
    expect(d.cols.delta).toBeInstanceOf(Float64Array);
    await host.handle({ type: 'tables', id: 4, spec, include: ['core', 'partner', 'network'] });
    const t = reply(4) as Record<string, Table>;
    expect(Object.keys(t)).toEqual(expect.arrayContaining(['parameters', 'strategic_index', 'psi_partner', 'network_specialization']));
    await host.handle({ type: 'load', id: 5, chunk: 'network' });
    expect(reply(5)).toEqual({ chunk: 'network', loaded: true });
    expect(of(msgs, 'error')).toHaveLength(0);
  });

  it('self-check: core, partner and network pass against the shipped baseline', async () => {
    const { host, msgs } = makeHost({ selfCheck: true });
    await host.handle({ type: 'init', dataBase });
    await host.handle({ type: 'run', id: 1, spec: specOf('S03'), want: { partner: true, network: true } });
    const deadline = Date.now() + 30_000;
    while (of(msgs, 'selfcheck').length < 3 && Date.now() < deadline) await new Promise((r) => setTimeout(r, 25));
    const sc = of(msgs, 'selfcheck');
    expect(sc.map((s) => s.layer).sort()).toEqual(['core', 'network', 'partner']);
    for (const s of sc) expect(s.ok, `${s.layer}: ${s.detail}`).toBe(true);
  });

  it("the paper's stored exact design: shipped with its portfolio tables, and the network self-check still runs", async () => {
    const { host, msgs } = makeHost({ selfCheck: true });
    await host.handle({ type: 'init', dataBase });
    await host.handle({ type: 'load', id: 1, chunk: 'network' });
    // turn the synthetic build into one whose stored design is consistent (the real export's MILP case):
    // the stored tables are the shipped greedy baseline's, labelled 'milp'
    const b = (host as unknown as { bundle: { manifest: { flags: Record<string, boolean> }; network: Record<string, unknown> } }).bundle;
    const gb = b.network.greedy_baseline as Record<string, unknown>;
    const pf = gb.portfolio as Record<string, unknown>;
    const diag = gb.diagnostics as Table;
    const n = diag.nrow;
    b.manifest.flags = { ...b.manifest.flags, network_consistent: true, milp_baseline: true };
    b.network.stored = {
      specialization: gb.specialization, flows: gb.flows, member_shares: gb.member_shares,
      diagnostics: { ...diag, cols: { ...diag.cols, method: Array.from({ length: n }, () => 'milp') } },
      caps: pf.caps, counts: pf.counts, topk: pf.topk,
    };
    await host.handle({ type: 'run', id: 2, spec: { dials: {} }, want: { partner: true, network: true } });
    const net = results(msgs, 2).find((m) => m.layer === 'network')!.payload as Extract<FromWorker, { type: 'result'; layer: 'network' }>['payload'];
    expect(net.method).toBe('milp');
    expect(net.solution.portfolio).not.toBeNull();
    expect(net.solution.portfolio!.counts.nrow).toBe((pf.counts as Table).nrow);
    expect(net.solution.portfolio!.converged).toBe(pf.converged as boolean);
    const deadline = Date.now() + 30_000;
    while (!of(msgs, 'selfcheck').some((s) => s.layer === 'network') && Date.now() < deadline) await new Promise((r) => setTimeout(r, 25));
    const sc = of(msgs, 'selfcheck').find((s) => s.layer === 'network');
    expect(sc, 'network self-check queued at the exact baseline').toBeDefined();
    expect(sc!.ok, sc!.detail).toBe(true);
  });

  it('errors: missing data and a newer schema stop at init; a bad request answers with an error', async () => {
    const missing = makeHost({ fetch: async () => { throw new Error('HTTP 404'); } });
    await missing.host.handle({ type: 'init', dataBase });
    expect(of(missing.msgs, 'error')[0].code).toBe('DATA_LOAD');
    const newer = makeHost({
      fetch: async (url) => (url.endsWith('manifest.json') ? new TextEncoder().encode(JSON.stringify({ schema_version: 99, chunks: [] })) : fetchDisk(url, {})),
    });
    await newer.host.handle({ type: 'init', dataBase });
    expect(of(newer.msgs, 'error')[0].code).toBe('SCHEMA');
    const { host, msgs } = makeHost();
    await host.handle({ type: 'init', dataBase });
    await host.handle({ type: 'run', id: 1, spec: { dials: { hawkishness: 140 } }, want: {} });
    const e = of(msgs, 'error');
    expect(e[0].id).toBe(1);
    expect(e[0].code).toBe('PARAMS');
  });
});

/** An in-process stand-in for the module worker (structured clones both ways, async delivery). */
function fakeWorker(fetch: FetchBytes = fetchDisk): WorkerLike {
  const w: WorkerLike = {
    onmessage: null,
    postMessage(msg: ToWorker) {
      const m = structuredClone(msg);
      setTimeout(() => void host.handle(m), 0);
    },
  };
  const host = new EngineHost((m) => setTimeout(() => w.onmessage?.({ data: structuredClone(m) } as MessageEvent<FromWorker>), 0), fetch, inflate, { selfCheck: false });
  return w;
}

describe('EngineClient', () => {
  it('becomes ready, runs, supersedes, answers requests', async () => {
    const client = new EngineClient(fakeWorker(), dataBase);
    const seen: string[] = [];
    client.subscribe(() => seen.push(client.getSnapshot().status));
    const st = await client.whenReady();
    expect(st.status).toBe('ready');
    expect(st.chunks.core.state).toBe('loaded');
    expect(seen).toContain('ready');
    const first = client.run(specOf('S01'), { partner: true, network: true });
    const progress: string[] = [];
    const second = client.run(specOf('S04'), { partner: true }, { onProgress: (p) => progress.push(p.phase) });
    expect((await first.done).status).toBe('superseded');
    const o = await second.done;
    expect(o.status).toBe('done');
    expect(o.core?.source).toBe('computed');
    expect(o.partner?.payload.psi.partner.nrow).toBeGreaterThan(0);
    expect(o.network).toBeUndefined();
    expect(progress.length).toBeGreaterThan(0);
    expect(client.getSnapshot().chunks.partner.state).toBe('loaded');
    const t = await client.tables(specOf('S04'), ['core']);
    expect(t.strategic_index.nrow).toBeGreaterThan(0);
    const bad = client.run({ dials: { greenness: -1 } });
    const ob = await bad.done;
    expect(ob.status).toBe('error');
    expect(ob.error?.code).toBe('PARAMS');
    await expect(client.variables(specOf('S04'), 'es', 'Nowhere')).resolves.toBeDefined();
  });

  it('reports a fatal error when the data are missing', async () => {
    const client = new EngineClient(fakeWorker(async () => { throw new Error('HTTP 404'); }), dataBase);
    await expect(client.whenReady()).rejects.toThrow(/No data/);
    expect(client.getSnapshot().status).toBe('error');
    expect((await client.run({ dials: {} }).done).status).toBe('error');
  });
});
