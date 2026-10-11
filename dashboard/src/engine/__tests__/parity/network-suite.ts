// L5 network parity (spec_react.md 6.9): inputs and scores to 1e-12; per stage the producer set and
// `selected` identical, shares / flows / hhi / objective to 1e-6; portfolio counts, convergence and
// the forbidden sets identical. Each step runs on the fixture's own upstream outputs and end to end.

import { expect, it } from 'vitest';
import { deepEqual } from '../../numeric.ts';
import {
  membersInfo, networkCompare, networkConcentration, networkInputs, networkScore, networkSolve, networkStages, type NetInputs, type NetScored,
  type NetworkSolution,
} from '../../network/index.ts';
import { fsCells, poCells } from '../../partner.ts';
import { lgl, num, str } from '../../table.ts';
import type { Bundle, NDArray, Params, PolicyResult, Table } from '../../types.ts';
import { asTable, compareTable, get, report, type Diff } from './harness.ts';

type Load = () => { fx: Record<string, unknown>; params: Params; b: Bundle };

function check(diffs: Diff[]): void {
  if (diffs.length) throw new Error('\n' + report(diffs));
  expect(diffs.length).toBe(0);
}

function cellsOf(fx: Record<string, unknown>, b: Bundle, params: Params) {
  const fsFull = get(fx, 'L4.fs_index') as NDArray<Float64Array> | undefined;
  if (fsFull) return { fs_index: fsFull.data, po_index: (get(fx, 'L4.po_index') as NDArray<Float64Array>).data };
  return { fs_index: fsCells(b, params), po_index: poCells(b, params) };
}

/** The fixture's L5 inputs as the engine's object. */
function fixtureInputs(fx: Record<string, unknown>, b: Bundle, params: Params): NetInputs {
  const nb = b.network!;
  const mi = membersInfo(nb.members);
  return {
    raw_nodes: asTable(get(fx, 'L5.inputs.raw_nodes')),
    edges: asTable(get(fx, 'L5.inputs.edges')),
    member_weights: mi.aggregate ? nb.member_weights : null,
    stages: networkStages(b, params),
    members: nb.members,
    mi,
    nodes: mi.aggregate ? mi.nodes.map((n) => n.network_node) : [...mi.iso3c],
    node_size: mi.aggregate ? nb.node_size : null,
    aggregate: mi.aggregate,
    notes: [],
  };
}

function fixtureScored(fx: Record<string, unknown>, b: Bundle, params: Params): NetScored {
  return {
    nodes: asTable(get(fx, 'L5.scored.nodes')),
    edges: asTable(get(fx, 'L5.scored.edges')),
    self_edges: null,
    stages: networkStages(b, params),
    inputs: fixtureInputs(fx, b, params),
  };
}

/** The greedy baseline design shipped in the network chunk. */
export function shippedGreedy(b: Bundle): Pick<NetworkSolution, 'specialization' | 'flows' | 'diagnostics'> | null {
  const gb = b.network?.greedy_baseline as Record<string, unknown> | undefined | null;
  if (!gb) return null;
  return { specialization: gb.specialization as Table, flows: gb.flows as Table, diagnostics: gb.diagnostics as Table };
}

/** Per-stage producer sets and selected flags of two specialization tables. */
function compareProducers(path: string, a: Table, e: Table, diffs: Diff[]): void {
  const sets = (t: Table) => {
    const T = str(t, 'tech');
    const S = str(t, 'supply_chain');
    const I = str(t, 'iso3c');
    const sh = num(t, 'production_share');
    const sel = lgl(t, 'selected');
    const prod = new Map<string, string[]>();
    const selm = new Map<string, string[]>();
    for (let i = 0; i < t.nrow; i++) {
      const k = `${T[i]}||${S[i]}`;
      if (!prod.has(k)) prod.set(k, []);
      if (!selm.has(k)) selm.set(k, []);
      if (sh[i] > 0) prod.get(k)!.push(I[i] as string);
      if (sel[i] === 1) selm.get(k)!.push(I[i] as string);
    }
    return { prod, selm };
  };
  const A = sets(a);
  const E = sets(e);
  for (const [k, v] of E.prod) {
    const got = (A.prod.get(k) ?? []).slice().sort();
    const want = v.slice().sort();
    if (!deepEqual(got, want)) diffs.push({ path: `${path} producers ${k}`, problem: `${got.join(' ')} vs ${want.join(' ')}` });
    const gs = (A.selm.get(k) ?? []).slice().sort();
    const ws = (E.selm.get(k) ?? []).slice().sort();
    if (!deepEqual(gs, ws)) diffs.push({ path: `${path} selected ${k}`, problem: `${gs.join(' ')} vs ${ws.join(' ')}` });
  }
}

export function compareSolution(path: string, sol: NetworkSolution, fx: Record<string, unknown>, tol: Record<string, number>, diffs: Diff[]): void {
  const spec = asTable(get(fx, `${path}.specialization`));
  compareProducers(`${path}`, sol.specialization, spec, diffs);
  compareTable(`${path}.specialization`, sol.specialization, spec, tol.share, diffs);
  compareTable(`${path}.flows`, sol.flows, asTable(get(fx, `${path}.flows`)), tol.share, diffs);
  compareTable(`${path}.diagnostics`, sol.diagnostics, asTable(get(fx, `${path}.diagnostics`)), tol.share, diffs);
  const ms = get(fx, `${path}.member_shares`) as Table | undefined;
  if (ms) compareTable(`${path}.member_shares`, sol.member_shares ?? undefined, ms, tol.share, diffs);
  compareTable(`${path}.solver_by_stage`, sol.solver_by_stage, asTable(get(fx, `${path}.solver_by_stage`)), 0, diffs);
  const pf = get(fx, `${path}.portfolio`) as Record<string, unknown> | undefined;
  if (pf) {
    if (!sol.portfolio) diffs.push({ path: `${path}.portfolio`, problem: 'missing' });
    else {
      compareTable(`${path}.portfolio.counts`, sol.portfolio.counts, pf.counts as Table, 0, diffs);
      compareTable(`${path}.portfolio.caps`, sol.portfolio.caps, pf.caps as Table, 0, diffs);
      compareTable(`${path}.portfolio.topk`, sol.portfolio.topk, pf.topk as Table, tol.share, diffs);
      for (const k of ['converged', 'iters', 'violations_remaining'] as const) {
        if (!deepEqual(sol.portfolio[k], pf[k])) diffs.push({ path: `${path}.portfolio.${k}`, problem: `${sol.portfolio[k]} vs ${pf[k]}` });
      }
    }
  }
  const fb = get(fx, `${path}.forbidden`) as Record<string, string[]>;
  if (fb && !deepEqual(sol.forbidden, fb)) diffs.push({ path: `${path}.forbidden`, problem: `${JSON.stringify(sol.forbidden)} vs ${JSON.stringify(fb)}` });
}

export function networkParity(load: Load, tol: Record<string, number>): void {
  it('L5 inputs (on the fixture cells / policy)', () => {
    const { fx, params, b } = load();
    if (!get(fx, 'L5')) return;
    const cells = cellsOf(fx, b, params);
    const pol = { index: asTable(get(fx, 'L2.index')) } as PolicyResult;
    const inp = networkInputs(b, params, cells, pol);
    const diffs: Diff[] = [];
    compareTable('L5.inputs.raw_nodes', inp.raw_nodes, asTable(get(fx, 'L5.inputs.raw_nodes')), tol.partner, diffs);
    compareTable('L5.inputs.edges', inp.edges, asTable(get(fx, 'L5.inputs.edges')), tol.partner, diffs);
    const st = asTable(get(fx, 'L5.stages'));
    if (!deepEqual(inp.stages.map((s) => s.tech), str(st, 'tech')) || !deepEqual(inp.stages.map((s) => s.supply_chain), str(st, 'supply_chain'))) {
      diffs.push({ path: 'L5.stages', problem: 'differ' });
    }
    check(diffs);
  });

  it('L5 scores (on the fixture inputs)', () => {
    const { fx, params, b } = load();
    if (!get(fx, 'L5')) return;
    const sc = networkScore(fixtureInputs(fx, b, params), params);
    const diffs: Diff[] = [];
    compareTable('L5.scored.nodes', sc.nodes, asTable(get(fx, 'L5.scored.nodes')), tol.partner, diffs);
    compareTable('L5.scored.edges', sc.edges, asTable(get(fx, 'L5.scored.edges')), tol.partner, diffs);
    check(diffs);
  });

  it('L5 greedy solution (on the fixture scores)', async () => {
    const { fx, params, b } = load();
    if (!get(fx, 'L5')) return;
    const sol = await networkSolve(fixtureScored(fx, b, params), params, b, { yieldBetweenStages: false });
    const diffs: Diff[] = [];
    compareSolution('L5.solution', sol!, fx, tol, diffs);
    const ref = shippedGreedy(b);
    if (ref && get(fx, 'L5.compare')) {
      const cmp = networkCompare(sol!, ref);
      compareTable('L5.compare.stage', cmp.stage, asTable(get(fx, 'L5.compare.stage')), tol.share, diffs);
      compareTable('L5.compare.node', cmp.node, asTable(get(fx, 'L5.compare.node')), tol.share, diffs);
      compareTable('L5.compare.node_summary', cmp.node_summary, asTable(get(fx, 'L5.compare.node_summary')), tol.share, diffs);
    }
    const conc = networkConcentration(sol!, b, ref);
    compareTable('L5.concentration', conc, asTable(get(fx, 'L5.concentration')), tol.share, diffs);
    check(diffs);
  });

  it('L5 end to end (cells -> inputs -> scores -> solve)', async () => {
    const { fx, params, b } = load();
    if (!get(fx, 'L5')) return;
    const cells = cellsOf(fx, b, params);
    const pol = { index: asTable(get(fx, 'L2.index')) } as PolicyResult;
    const sc = networkScore(networkInputs(b, params, cells, pol), params);
    const sol = await networkSolve(sc, params, b, { yieldBetweenStages: false });
    const diffs: Diff[] = [];
    compareSolution('L5.solution', sol!, fx, tol, diffs);
    check(diffs);
  });
}
