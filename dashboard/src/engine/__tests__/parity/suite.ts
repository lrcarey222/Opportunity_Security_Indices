// The parity suite (spec_react.md 6.9), shared by the synthetic and the real fixture sets: for each
// scenario, makeParams() against the fixture's params, then each layer on the fixture's own upstream
// outputs (so one failing layer does not cascade) and end to end.

import { afterEach, describe, expect, it } from 'vitest';
import { deepEqual, LdAcc } from '../../numeric.ts';
import { takeRows } from '../../table.ts';
import { dialReadout, makeParams, paramsToRJson } from '../../params.ts';
import { pillars, variableContrib } from '../../pillars.ts';
import { policy } from '../../policy.ts';
import { alliedSelection, si } from '../../si.ts';
import { fsCells, partnerDecompose, partnerTop5, poCells, psi } from '../../partner.ts';
import { compare } from '../../engine.ts';
import type { Bundle, NDArray, Params, PillarsResult, PolicyResult, ScenarioSpec, SiResult, Table, Top5Result } from '../../types.ts';
import {
  asTable, compareArray, compareColumn, compareTable, get, loadFixture, loadIndex, report, type Diff, type FixtureScenario,
} from './harness.ts';

export const TOL = {
  core: 1e-9,
  partner: 1e-12,
  share: 1e-6,
};

export interface SuiteOptions {
  name: string;
  fixtureDir: string;
  bundle: () => Bundle;
}

export function specOf(s: FixtureScenario): ScenarioSpec {
  const adv = Array.isArray(s.advanced) ? {} : (s.advanced as ScenarioSpec['advanced']);
  return { dials: s.dials, advanced: adv };
}

function nd(x: unknown): NDArray<Float64Array> {
  return x as NDArray<Float64Array>;
}

function expectNoDiffs(diffs: Diff[]): void {
  if (diffs.length) throw new Error('\n' + report(diffs));
  expect(diffs.length).toBe(0);
}

/** Let the event loop turn between long synchronous tests, so vitest's worker RPC (60 s timeout)
 * is served ("Timeout calling onTaskUpdate" otherwise, on the real set). */
export const turnEventLoop = () => new Promise<void>((resolve) => setImmediate(resolve));

export function paritySuite(opts: SuiteOptions): void {
  const index = loadIndex(opts.fixtureDir);
  describe.skipIf(!index)(`parity ${opts.name}`, () => {
    afterEach(turnEventLoop);
    let bundle: Bundle;
    const getBundle = () => (bundle ??= opts.bundle());
    it('fixtures belong to the bundle', () => {
      expect(getBundle().manifest.bundle_id).toBe(index!.bundle_id);
    });
    for (const s of index?.scenarios ?? []) {
      describe(`${s.id} ${s.label}`, () => {
        let fx: Record<string, unknown>;
        let params: Params;
        const load = () => {
          if (!fx) fx = loadFixture(opts.fixtureDir, s.file);
          if (!params) params = makeParams(specOf(s), getBundle());
          return { fx, params, b: getBundle() };
        };

        it('params and readout', () => {
          const { fx, params, b } = load();
          const want = get(fx, 'params');
          const got = paramsToRJson(params);
          if (!deepEqual(got, want)) {
            throw new Error(`params differ:\n got ${JSON.stringify(got)}\nwant ${JSON.stringify(want)}`);
          }
          const diffs: Diff[] = [];
          compareTable('readout', dialReadout(params, b), asTable(get(fx, 'readout')), 0, diffs);
          expectNoDiffs(diffs);
        });

        it('L1 pillars', () => {
          const { fx, params, b } = load();
          const res = pillars(b, params, { contributions: true });
          const diffs: Diff[] = [];
          compareTable('L1.es', res.es, asTable(get(fx, 'L1.es')), TOL.core, diffs);
          compareTable('L1.eo', res.eo, asTable(get(fx, 'L1.eo')), TOL.core, diffs);
          compareArray('L1.es_cw', res.es_category_weight.data, get(fx, 'L1.es_cw'), TOL.core, diffs);
          compareArray('L1.eo_cw', res.eo_category_weight.data, get(fx, 'L1.eo_cw'), TOL.core, diffs);
          if (get(fx, 'L1.es_contrib')) {
            compareTable('L1.es_contrib', res.es_contrib, asTable(get(fx, 'L1.es_contrib')), TOL.core, diffs);
            compareTable('L1.eo_contrib', res.eo_contrib, asTable(get(fx, 'L1.eo_contrib')), TOL.core, diffs);
          }
          for (const p of ['es', 'eo'] as const) {
            const want = get(fx, `L1.var.${p}`) as Table | undefined;
            if (!want || !b.pillars[p].var) continue;
            const countries = Array.from(new Set(want.cols.Country as string[]));
            const got = variableContrib(b, res, p, countries[0]);
            const parts = countries.map((c) => variableContrib(b, res, p, c));
            const merged = parts.length > 1 ? concat(parts) : got;
            compareTable(`L1.var.${p}`, merged, want, TOL.core, diffs);
          }
          expectNoDiffs(diffs);
        });

        it('L2 policy', () => {
          const { fx, params, b } = load();
          const res = policy(b, params);
          const diffs: Diff[] = [];
          compareTable('L2.index', res.index, asTable(get(fx, 'L2.index')), TOL.core, diffs);
          compareTable('L2.eligibility', res.eligibility, asTable(get(fx, 'L2.eligibility')), 0, diffs);
          if (!deepEqual(res.weights, get(fx, 'L2.weights'))) diffs.push({ path: 'L2.weights', problem: 'differ' });
          expectNoDiffs(diffs);
        });

        it('L3 strategic index (on the fixture L1 / L2)', () => {
          const { fx, params, b } = load();
          const pil = {
            es: asTable(get(fx, 'L1.es')), eo: asTable(get(fx, 'L1.eo')),
            es_category_weight: nd(get(fx, 'L1.es_cw')), eo_category_weight: nd(get(fx, 'L1.eo_cw')),
          } as PillarsResult;
          const pol = { index: asTable(get(fx, 'L2.index')), eligibility: asTable(get(fx, 'L2.eligibility')), weights: get(fx, 'L2.weights') } as PolicyResult;
          const res = si(b, params, pil, pol);
          const diffs: Diff[] = [];
          compareTable('L3.cells', res.cells, asTable(get(fx, 'L3.cells')), TOL.core, diffs);
          compareTable('L3.excluded', res.excluded, asTable(get(fx, 'L3.excluded')), 0, diffs);
          const deg = asTable(get(fx, 'L3.degenerate'));
          if (deg.nrow || res.degenerate.nrow) compareTable('L3.degenerate', res.degenerate, deg, 1e-12, diffs);
          if (!deepEqual(res.weights, get(fx, 'L3.weights'))) diffs.push({ path: 'L3.weights', problem: 'differ' });
          if (!deepEqual(res.stage_weights, get(fx, 'L3.stage_weights'))) diffs.push({ path: 'L3.stage_weights', problem: 'differ' });
          const fxSi = { cells: asTable(get(fx, 'L3.cells')) } as SiResult;
          compareTable('L3.allied', alliedSelection(fxSi, b, params), asTable(get(fx, 'L3.allied')), TOL.core, diffs);
          expectNoDiffs(diffs);
        });

        it('L1-L3 end to end and compare tables', () => {
          const { fx, params, b } = load();
          const pil = pillars(b, params, { contributions: false });
          const pol = policy(b, params);
          const res = si(b, params, pil, pol);
          const diffs: Diff[] = [];
          compareTable('L3.cells (e2e)', res.cells, asTable(get(fx, 'L3.cells')), TOL.core, diffs);
          const allied = alliedSelection(res, b, params);
          const base = baselineCore(b);
          const cmp = compare({ si: res, pillars: pil, policy: pol, allied, partner: null }, { ...base, partner: null });
          for (const k of ['si', 'es', 'eo', 'policy', 'allied'] as const) {
            const want = get(fx, `compare.${k}`) as Table | undefined;
            if (want) compareTable(`compare.${k}`, cmp[k], want, TOL.core, diffs);
          }
          expectNoDiffs(diffs);
        });

        it('L4 FS / PO cells', () => {
          const { fx, params, b } = load();
          const diffs: Diff[] = [];
          const fs = fsCells(b, params);
          const po = poCells(b, params);
          const U = b.dims.U.length;
          for (const [nm, arr] of [['fs', fs], ['po', po]] as const) {
            const full = get(fx, `L4.${nm}_index`);
            if (full) compareArray(`L4.${nm}_index`, arr, full, TOL.partner, diffs);
            const sample = get(fx, `L4.${nm}_sample`) as Table | undefined;
            if (sample) {
              const r = sample.cols.r as Int32Array;
              const p = sample.cols.p as Int32Array;
              const c = sample.cols.c as Int32Array;
              const got = Float64Array.from(r, (_, i) => arr[r[i] - 1 + U * (p[i] - 1) + U * U * (c[i] - 1)]);
              compareColumn(`L4.${nm}_sample`, got, sample.cols.value, TOL.partner, diffs);
            }
            const stats = asTable(get(fx, `L4.${nm}_stats`));
            const C = stats.nrow;
            const n = new Int32Array(C);
            const mn = new Float64Array(C).fill(NaN);
            const mx = new Float64Array(C).fill(NaN);
            for (let cc = 0; cc < C; cc++) {
              for (let i = 0; i < U * U; i++) {
                const v = arr[i + U * U * cc];
                if (v !== v) continue;
                n[cc]++;
                if (!(v >= mn[cc])) mn[cc] = v;
                if (!(v <= mx[cc])) mx[cc] = v;
              }
            }
            // sums in R's order with long-double accumulation (sumR's emulation)
            const sum = new Float64Array(C);
            const sumSq = new Float64Array(C);
            for (let cc = 0; cc < C; cc++) {
              const acc = new LdAcc();
              const acc2 = new LdAcc();
              for (let i = 0; i < U * U; i++) {
                const v = arr[i + U * U * cc];
                if (v !== v) continue;
                acc.add(v);
                acc2.add(v * v);
              }
              sum[cc] = n[cc] ? acc.value() : NaN;
              sumSq[cc] = n[cc] ? acc2.value() : NaN;
            }
            compareColumn(`L4.${nm}_stats$n_non_na`, n, stats.cols.n_non_na, 0, diffs);
            compareColumn(`L4.${nm}_stats$sum`, sum, stats.cols.sum, 1e-9, diffs);
            compareColumn(`L4.${nm}_stats$sum_sq`, sumSq, stats.cols.sum_sq, 1e-9, diffs);
            compareColumn(`L4.${nm}_stats$min`, mn, stats.cols.min, TOL.partner, diffs);
            compareColumn(`L4.${nm}_stats$max`, mx, stats.cols.max, TOL.partner, diffs);
          }
          expectNoDiffs(diffs);
        });

        it('L4 best-five, PSI and decomposition (on the fixture cells)', () => {
          const { fx, params, b } = load();
          const diffs: Diff[] = [];
          const fsFull = get(fx, 'L4.fs_index') as NDArray<Float64Array> | undefined;
          const cells = fsFull
            ? { fs_index: fsFull.data, po_index: (get(fx, 'L4.po_index') as NDArray<Float64Array>).data }
            : { fs_index: fsCells(b, params), po_index: poCells(b, params) };
          const top5 = partnerTop5(b, cells, params);
          compareArray('L4.top5_fs', top5.fs, get(fx, 'L4.top5_fs'), TOL.partner, diffs);
          compareArray('L4.top5_po', top5.po, get(fx, 'L4.top5_po'), TOL.partner, diffs);
          compareArray('L4.fs_exists', top5.fs_exists, get(fx, 'L4.fs_exists'), 0, diffs);
          compareArray('L4.po_exists', top5.po_exists, get(fx, 'L4.po_exists'), 0, diffs);
          const fxTop5: Top5Result = {
            fs: nd(get(fx, 'L4.top5_fs')).data, po: nd(get(fx, 'L4.top5_po')).data,
            fs_exists: (get(fx, 'L4.fs_exists') as NDArray<Uint8Array>).data, po_exists: (get(fx, 'L4.po_exists') as NDArray<Uint8Array>).data,
          };
          const p = psi(b, fxTop5, params);
          // the real set keeps the dyads of a subset of reporters (L4.psi_dyad_reporters) outside S00 / S13
          const subset = get(fx, 'L4.psi_dyad_reporters') as string[] | undefined;
          let dyad = p.dyad;
          if (subset) {
            const keep = new Set(subset);
            const rep = dyad.cols.reporter_iso as (string | null)[];
            dyad = takeRows(dyad, rep.flatMap((r, i) => (r !== null && keep.has(r) ? [i] : [])));
          }
          compareTable('L4.psi_dyad', dyad, asTable(get(fx, 'L4.psi_dyad')), TOL.partner, diffs);
          compareTable('L4.psi_partner', p.partner, asTable(get(fx, 'L4.psi_partner')), TOL.partner, diffs);
          const wantRep = get(fx, 'L4.psi_reporters') as string[];
          if (!deepEqual(p.reporters, wantRep)) diffs.push({ path: 'L4.psi_reporters', problem: `${p.reporters.length} vs ${wantRep?.length}` });
          const dy = get(fx, 'L4.decompose_dyads') as Table | undefined;
          if (dy && dy.nrow) {
            const parts = Array.from({ length: dy.nrow }, (_, i) =>
              partnerDecompose(b, params, cells, (dy.cols.reporter_iso as string[])[i], (dy.cols.partner_iso as string[])[i]));
            compareTable('L4.decompose', concat(parts), asTable(get(fx, 'L4.decompose')), TOL.partner, diffs);
          }
          const base = baselineCore(b);
          const basePartner = baselinePartner(b);
          if (get(fx, 'compare.psi_partner') && basePartner) {
            const cmp = compare(
              { ...base, partner: { cells, top5: fxTop5, psi: p } },
              { ...base, partner: { cells, top5: fxTop5, psi: basePartner } },
            );
            compareTable('compare.psi_partner', cmp.psi_partner, asTable(get(fx, 'compare.psi_partner')), TOL.partner, diffs);
          }
          expectNoDiffs(diffs);
        });
      });
    }
  });
}

function concat(parts: Table[]): Table {
  const names = Object.keys(parts[0].cols);
  const cols: Table['cols'] = {};
  for (const k of names) {
    const first = parts[0].cols[k];
    if (Array.isArray(first)) cols[k] = parts.flatMap((t) => t.cols[k] as (string | null)[]);
    else {
      const Ctor = first.constructor as { new (n: number): Float64Array };
      const n = parts.reduce((a, t) => a + t.nrow, 0);
      const out = new Ctor(n);
      let off = 0;
      for (const t of parts) {
        out.set(t.cols[k] as Float64Array, off);
        off += t.nrow;
      }
      cols[k] = out as unknown as Table['cols'][string];
    }
  }
  return { nrow: parts.reduce((a, t) => a + t.nrow, 0), cols };
}

/** The shipped baseline L1-L3 tables (core chunk) in result shape. */
export function baselineCore(b: Bundle): { si: SiResult; pillars: PillarsResult; policy: PolicyResult; allied: Table } {
  const bc = get(b.baseline, 'core') as Record<string, unknown>;
  return {
    si: { cells: get(bc, 'si.cells') as Table } as SiResult,
    pillars: { es: get(bc, 'es') as Table, eo: get(bc, 'eo') as Table } as PillarsResult,
    policy: { index: get(bc, 'policy.index') as Table } as PolicyResult,
    allied: get(bc, 'allied') as Table,
  };
}

export function baselinePartner(b: Bundle): { dyad: Table; partner: Table; reporters: string[]; view: 'coalition' | 'world' } | null {
  const partner = get(b.baseline, 'psi_partner') as Table | undefined;
  if (!partner) return null;
  return { dyad: get(b.baseline, 'partner.psi_dyad') as Table, partner, reporters: [], view: 'coalition' };
}
