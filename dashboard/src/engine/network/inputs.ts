// L5a: network inputs (port of dash_network_stages(), dash_network_policy_raw(),
// allied_network_aggregate_node_inputs(), dash_network_dyads(), dash_aggregate_edge_component(),
// dash_extract_edges() and dash_network_inputs(); spec_react.md 6.6). Pure.

import { cmpC, EngineError, meanR, NA_BOOL, NA_INT, orderBy, sumList } from '../numeric.ts';
import { lgl, num, str, table } from '../table.ts';
import type { Bundle, NetworkInputs, Params, PartnerCells, PolicyResult, Str, Table } from '../types.ts';

export function requireNetwork(bundle: Pick<Bundle, 'network'>): NetworkInputs {
  if (!bundle.network) throw new EngineError('CHUNK', 'The network chunk is not loaded.');
  return bundle.network;
}

export interface Stage {
  tech: string;
  supply_chain: string;
}

export const stageKey = (tech: string | null, sc: string | null) => `${tech}||${sc}`;

/** dash_network_stages(): member stages in supply_chains, outside the excluded technologies, C-sorted. */
export function networkStages(bundle: Pick<Bundle, 'network'>, params: Params): Stage[] {
  const raw = requireNetwork(bundle).node_raw_member;
  const T = str(raw, 'tech');
  const S = str(raw, 'supply_chain');
  const sc = new Set(params.network.supply_chains.map((s) => s.toLowerCase()));
  const ex = new Set(params.network.exclude_techs);
  const seen = new Set<string>();
  const out: Stage[] = [];
  for (let i = 0; i < raw.nrow; i++) {
    const k = stageKey(T[i], S[i]);
    if (seen.has(k)) continue;
    seen.add(k);
    if (sc.has(String(S[i]).toLowerCase()) && !ex.has(T[i] as string)) out.push({ tech: T[i] as string, supply_chain: S[i] as string });
  }
  out.sort((a, b) => cmpC(a.tech, b.tech) || cmpC(a.supply_chain, b.supply_chain));
  return out;
}

/** dash_network_policy_raw(): scenario Policy index collapsed to members (canonical spelling first). */
export function networkPolicyRaw(bundle: Pick<Bundle, 'network'>, policyIndex: Table, raw: Table): Float64Array {
  const nb = requireNetwork(bundle);
  const cm = nb.country_map;
  const cmC = str(cm, 'Country');
  const cmIso = str(cm, 'iso3c');
  const cmCan = lgl(cm, 'is_canonical');
  const cmIdx = new Map<string | null, number>();
  for (let i = cm.nrow - 1; i >= 0; i--) cmIdx.set(cmC[i], i);
  const rawIso = new Set(str(raw, 'iso3c'));
  const pC = str(policyIndex, 'Country');
  const pT = str(policyIndex, 'tech');
  const pS = str(policyIndex, 'supply_chain');
  const pV = num(policyIndex, 'value');
  const rows: { key: string; fin: number; nc: number; v: number }[] = [];
  for (let i = 0; i < policyIndex.nrow; i++) {
    const k = cmIdx.get(pC[i]);
    if (k === undefined) continue;
    const iso = cmIso[k];
    if (iso === null || !rawIso.has(iso)) continue;
    rows.push({ key: `${iso}\r${pT[i]}\r${pS[i]}`, fin: Number.isFinite(pV[i]) ? 0 : 1, nc: cmCan[k] === 1 ? 0 : 1, v: pV[i] });
  }
  const ord = orderBy(
    [{ str: rows.map((r) => r.key) }, { num: rows.map((r) => r.fin) }, { num: rows.map((r) => r.nc) }],
    rows.length,
  );
  const first = new Map<string, number>();
  for (const i of ord) if (!first.has(rows[i].key)) first.set(rows[i].key, rows[i].v);
  const rI = str(raw, 'iso3c');
  const rT = str(raw, 'tech');
  const rS = str(raw, 'supply_chain');
  return Float64Array.from({ length: raw.nrow }, (_, i) => {
    const v = first.get(`${rI[i]}\r${rT[i]}\r${rS[i]}`);
    return v === undefined ? NaN : v;
  });
}

/** allied_network_wmean(): over finite x with positive finite weights; NA if none. */
export function wmean(x: ArrayLike<number>, w: ArrayLike<number>): number {
  const xs: number[] = [];
  const ws: number[] = [];
  for (let i = 0; i < x.length; i++) {
    if (Number.isFinite(x[i]) && Number.isFinite(w[i]) && w[i] > 0) {
      xs.push(x[i] * w[i]);
      ws.push(w[i]);
    }
  }
  if (!xs.length) return NaN;
  return sumList(xs) / sumList(ws);
}

export interface MembersInfo {
  iso3c: string[];
  tier: Str;
  network_node: string[];
  node_tier: Str;
  nodeOf: Map<string, string>;
  multi: Set<string>;
  aggregate: boolean;
  /** node table: network_node, tier, n_members, members, node_order (first appearance) */
  nodes: { network_node: string; tier: string | null; n_members: number }[];
}

export function membersInfo(members: Table): MembersInfo {
  const iso3c = str(members, 'iso3c') as string[];
  const node = str(members, 'network_node') as string[];
  const tier = str(members, 'tier');
  const nodeTier = members.cols.node_tier ? str(members, 'node_tier') : tier;
  const nodeOf = new Map<string, string>();
  const sizes = new Map<string, number>();
  iso3c.forEach((i, k) => {
    nodeOf.set(i, node[k]);
    sizes.set(node[k], (sizes.get(node[k]) ?? 0) + 1);
  });
  const multi = new Set(Array.from(sizes.entries()).filter(([, n]) => n > 1).map(([k]) => k));
  const order: string[] = [];
  for (const n of node) if (!order.includes(n)) order.push(n);
  const nodes = order.map((n) => {
    const tiers = new Set<string | null>();
    node.forEach((m, k) => {
      if (m === n) tiers.add(nodeTier[k]);
    });
    return { network_node: n, tier: tiers.size === 1 ? Array.from(tiers)[0] : 'mixed', n_members: sizes.get(n) as number };
  });
  return {
    iso3c, tier, network_node: node, node_tier: nodeTier, nodeOf, multi,
    aggregate: iso3c.some((i, k) => node[k] !== i), nodes,
  };
}

/** Member weight lookup by (iso3c, tech, supply_chain). */
export function memberWeightMap(mw: Table): Map<string, { w: number; basis: string | null; node: string | null }> {
  const I = str(mw, 'iso3c');
  const T = str(mw, 'tech');
  const S = str(mw, 'supply_chain');
  const W = num(mw, 'member_weight');
  const B = mw.cols.weight_basis ? str(mw, 'weight_basis') : null;
  const N = str(mw, 'network_node');
  const m = new Map<string, { w: number; basis: string | null; node: string | null }>();
  for (let i = 0; i < mw.nrow; i++) {
    const k = `${I[i]}\r${T[i]}\r${S[i]}`;
    if (!m.has(k)) m.set(k, { w: W[i], basis: B ? B[i] : null, node: N[i] });
  }
  return m;
}

// Lookup maps of the bundle's fixed tables, built once per table object.
const MW_MAPS = new WeakMap<Table, ReturnType<typeof memberWeightMap>>();
const TRADE_MAPS = new WeakMap<Table, Map<string, number>>();

function memberWeightMapCached(mw: Table): ReturnType<typeof memberWeightMap> {
  let m = MW_MAPS.get(mw);
  if (!m) MW_MAPS.set(mw, (m = memberWeightMap(mw)));
  return m;
}

/** node_trade value by exporter, importer, tech, stage (the first row of a key, as R's match()). */
function tradeMapCached(nodeTrade: Table): Map<string, number> {
  let m = TRADE_MAPS.get(nodeTrade);
  if (m) return m;
  m = new Map<string, number>();
  const E = str(nodeTrade, 'exporter_iso');
  const Im = str(nodeTrade, 'importer_iso');
  const T = str(nodeTrade, 'tech');
  const S = str(nodeTrade, 'supply_chain');
  const V = num(nodeTrade, 'value');
  for (let i = 0; i < nodeTrade.nrow; i++) {
    const k = `${E[i]}\r${Im[i]}\r${T[i]}\r${S[i]}`;
    if (!m.has(k)) m.set(k, V[i]);
  }
  TRADE_MAPS.set(nodeTrade, m);
  return m;
}

/** allied_network_aggregate_node_inputs(): member inputs aggregated to nodes, by (iso3c, tech, supply_chain). */
export function aggregateNodeInputs(
  raw: Table, mi: MembersInfo, mw: Table, nodeLabels: Record<string, string> | null | undefined, countryInfo: Table | null, nodeSize: Table | null,
): Table {
  const I = str(raw, 'iso3c');
  const Cn = str(raw, 'country');
  const T = str(raw, 'tech');
  const S = str(raw, 'supply_chain');
  const eo = num(raw, 'eo_raw');
  const es = num(raw, 'es_raw');
  const po = num(raw, 'policy_raw');
  const dev = num(raw, 'dev_potential');
  const gdp = num(raw, 'gdp_usd');
  const gimp = lgl(raw, 'gdp_imputed');
  const mwMap = memberWeightMap(mw);
  type Row = {
    iso3c: string; country: string | null; tech: string | null; supply_chain: string | null; eo_raw: number; es_raw: number;
    policy_raw: number; dev_potential: number; gdp_usd: number; gdp_imputed: number; es_need_raw: number; n_members_inputs: number;
  };
  const rows: Row[] = [];
  const groups = new Map<string, number[]>();
  for (let i = 0; i < raw.nrow; i++) {
    const node = mi.nodeOf.get(I[i] as string) ?? null;
    if (node === null || !mi.multi.has(node)) {
      rows.push({
        iso3c: node as string, country: Cn[i], tech: T[i], supply_chain: S[i], eo_raw: eo[i], es_raw: es[i], policy_raw: po[i],
        dev_potential: dev[i], gdp_usd: gdp[i], gdp_imputed: gimp[i], es_need_raw: es[i], n_members_inputs: 1,
      });
    } else {
      const k = `${node}\r${T[i]}\r${S[i]}`;
      let g = groups.get(k);
      if (!g) groups.set(k, (g = []));
      g.push(i);
    }
  }
  const labelOf = (node: string) => {
    let lab: string | null = nodeLabels && node in nodeLabels ? nodeLabels[node] : null;
    if (lab === null && countryInfo) {
      const ci = str(countryInfo, 'iso3c');
      const k = ci.indexOf(node);
      lab = k >= 0 ? str(countryInfo, 'country')[k] : null;
    }
    return lab ?? node;
  };
  const nsIso = nodeSize ? str(nodeSize, 'iso3c') : [];
  for (const [k, idx] of groups) {
    const [node, tech, sc] = k.split('\r');
    const w = idx.map((i) => {
      const m = mwMap.get(`${I[i]}\r${T[i]}\r${S[i]}`);
      if (!m || m.node !== node) throw new EngineError('NETWORK', `No member weights for node ${node} in ${T[i]} / ${S[i]}.`);
      return m.w;
    });
    const g = idx.map((i) => gdp[i]);
    const esv = idx.map((i) => es[i]);
    const anyGdp = g.some((v) => Number.isFinite(v) && v > 0);
    const row: Row = {
      iso3c: node, country: labelOf(node), tech: T[idx[0]], supply_chain: S[idx[0]],
      es_need_raw: anyGdp ? wmean(esv, g) : wmean(esv, esv.map(() => 1)),
      eo_raw: wmean(idx.map((i) => eo[i]), w),
      es_raw: wmean(esv, w),
      policy_raw: wmean(idx.map((i) => po[i]), w),
      dev_potential: wmean(idx.map((i) => dev[i]), w),
      gdp_imputed: idx.some((i) => gimp[i] === 1) ? 1 : 0,
      gdp_usd: g.some((v) => Number.isFinite(v)) ? sumList(g.filter((v) => v === v)) : NaN,
      n_members_inputs: idx.length,
    };
    void tech;
    void sc;
    if (nodeSize) {
      const q = nsIso.indexOf(node);
      if (q >= 0) {
        row.gdp_usd = num(nodeSize, 'gdp_usd')[q];
        row.gdp_imputed = lgl(nodeSize, 'gdp_imputed')[q] === 1 ? 1 : 0;
      }
    }
    rows.push(row);
  }
  const ord = orderBy(
    [{ str: rows.map((r) => r.iso3c) }, { str: rows.map((r) => r.tech) }, { str: rows.map((r) => r.supply_chain) }],
    rows.length,
  );
  const o = ord.map((i) => rows[i]);
  return table({
    iso3c: o.map((r) => r.iso3c),
    country: o.map((r) => r.country),
    tech: o.map((r) => r.tech),
    supply_chain: o.map((r) => r.supply_chain),
    eo_raw: Float64Array.from(o, (r) => r.eo_raw),
    es_raw: Float64Array.from(o, (r) => r.es_raw),
    policy_raw: Float64Array.from(o, (r) => r.policy_raw),
    dev_potential: Float64Array.from(o, (r) => r.dev_potential),
    gdp_usd: Float64Array.from(o, (r) => r.gdp_usd),
    gdp_imputed: Uint8Array.from(o, (r) => r.gdp_imputed),
    es_need_raw: Float64Array.from(o, (r) => r.es_need_raw),
    n_members_inputs: Int32Array.from(o, (r) => r.n_members_inputs),
  }, o.length);
}

/** One long edge component: rows of (reporter_iso, partner_iso, tech, supply_chain, value). */
export interface Long {
  reporter: string[];
  partner: string[];
  tech: string[];
  sc: string[];
  value: number[];
}

/** dash_network_dyads(): member-pair rows of one dyad index (mask order, reporter fastest). */
export function networkDyads(arr: Float64Array, mask: Uint8Array, bundle: Pick<Bundle, 'dims'>, isoNetwork: string[], stages: Stage[]): Long {
  const U = bundle.dims.U;
  const nU = U.length;
  const uIdx = new Map(U.map((u, i) => [u, i] as [string, number]));
  const ri = isoNetwork.map((x) => uIdx.get(x)).filter((x): x is number => x !== undefined);
  const cT = str(bundle.dims.cells, 'tech');
  const cS = str(bundle.dims.cells, 'supply_chain');
  const cellKey = new Map<string, number>();
  for (let c = cT.length - 1; c >= 0; c--) cellKey.set(`${cT[c]} ${cS[c]}`, c);
  const ci = stages.map((s) => cellKey.get(`${s.tech} ${s.supply_chain}`)).filter((x): x is number => x !== undefined);
  const out: Long = { reporter: [], partner: [], tech: [], sc: [], value: [] };
  const UU = nU * nU;
  for (const c of ci) {
    for (const p of ri) {
      for (const r of ri) {
        const i = r + nU * p + UU * c;
        if (mask[i] !== 1) continue;
        out.reporter.push(U[r]);
        out.partner.push(U[p]);
        out.tech.push(cT[c] as string);
        out.sc.push(cS[c] as string);
        out.value.push(arr[i]);
      }
    }
  }
  return out;
}

/** dash_aggregate_edge_component(): one edge component (producer = reporter) aggregated to node pairs. */
export function aggregateEdgeComponent(comp: Long, mi: MembersInfo, mw: Table, nodeTrade: Table | null | undefined): Long {
  const n = comp.reporter.length;
  const pass: Long = { reporter: [], partner: [], tech: [], sc: [], value: [] };
  type A = { r: string; p: string; pn: string; cn: string; t: string; s: string; v: number };
  const agg: A[] = [];
  for (let i = 0; i < n; i++) {
    const pn = mi.nodeOf.get(comp.reporter[i]) as string;
    const cn = mi.nodeOf.get(comp.partner[i]) as string;
    if (pn === cn) continue;
    const multi = mi.multi.has(pn) || mi.multi.has(cn);
    if (!multi) {
      pass.reporter.push(pn);
      pass.partner.push(cn);
      pass.tech.push(comp.tech[i]);
      pass.sc.push(comp.sc[i]);
      pass.value.push(comp.value[i]);
    } else if (Number.isFinite(comp.value[i])) {
      agg.push({ r: comp.reporter[i], p: comp.partner[i], pn, cn, t: comp.tech[i], s: comp.sc[i], v: comp.value[i] });
    }
  }
  if (!agg.length) return pass;
  const mwMap = memberWeightMapCached(mw);
  const tradeMap = nodeTrade ? tradeMapCached(nodeTrade) : null;
  type B = A & { wp: number; wc: number; tr: number };
  // rows grouped by (producer node, consumer node, tech, stage); the groups, and the rows within a
  // group by (producer member, consumer member), in C-locale order: R's order(..., method = "radix")
  const byGroup = new Map<string, B[]>();
  for (const a of agg) {
    const wp = mwMap.get(`${a.r}\r${a.t}\r${a.s}`);
    const wc = mwMap.get(`${a.p}\r${a.t}\r${a.s}`);
    if (!wp || !wc || wp.w !== wp.w || wc.w !== wc.w) continue;
    const t0 = tradeMap ? tradeMap.get(`${a.r}\r${a.p}\r${a.t}\r${a.s}`) : undefined;
    const t1 = t0 === undefined || t0 !== t0 ? 0 : t0;
    const row: B = { r: a.r, p: a.p, pn: a.pn, cn: a.cn, t: a.t, s: a.s, v: a.v, wp: wp.w, wc: wc.w, tr: Math.max(0, t1) };
    // joined with U+0000, which no field contains, the key orders as the tuple
    const g = `${a.pn}\u0000${a.cn}\u0000${a.t}\u0000${a.s}`;
    let arr = byGroup.get(g);
    if (!arr) byGroup.set(g, (arr = []));
    arr.push(row);
  }
  const groupKeys = [...byGroup.keys()].sort((x, y) => (x < y ? -1 : x > y ? 1 : 0));
  const groups: B[][] = groupKeys.map((k) => {
    const g = byGroup.get(k) as B[];
    if (g.length > 1) g.sort((x, y) => cmpC(x.r, y.r) || cmpC(x.p, y.p));
    return g;
  });
  const out: Long = { reporter: [...pass.reporter], partner: [...pass.partner], tech: [...pass.tech], sc: [...pass.sc], value: [...pass.value] };
  for (const g of groups) {
    let val: number;
    if (sumList(g.map((x) => x.tr)) > 0) val = wmean(g.map((x) => x.v), g.map((x) => x.tr));
    else {
      const ww = g.map((x) => x.wp * x.wc);
      if (sumList(ww) > 0) val = wmean(g.map((x) => x.v), ww);
      else val = meanR(g.map((x) => x.v));
    }
    out.reporter.push(g[0].pn);
    out.partner.push(g[0].cn);
    out.tech.push(g[0].t);
    out.sc.push(g[0].s);
    out.value.push(val);
  }
  return out;
}

/** dash_extract_edges(): node-pair edges with both components (missing component 0). */
export function extractEdges(fsLong: Long, poLong: Long, mi: MembersInfo, mw: Table | null, nodeTrade: Table | null | undefined): Table {
  const fin = (v: number) => (Number.isFinite(v) ? v : NaN);
  let fs: Long = { reporter: fsLong.partner, partner: fsLong.reporter, tech: fsLong.tech, sc: fsLong.sc, value: fsLong.value.map(fin) };
  let po: Long = { reporter: poLong.reporter, partner: poLong.partner, tech: poLong.tech, sc: poLong.sc, value: poLong.value.map(fin) };
  const keyOf = (l: Long) => l.reporter.map((r, i) => `${r}\r${l.partner[i]}\r${l.tech[i]}\r${l.sc[i]}`);
  for (const l of [fs, po]) {
    const ks = keyOf(l);
    if (new Set(ks).size !== ks.length) throw new EngineError('NETWORK', 'dash_extract_edges: dyad rows must be unique per key.');
  }
  if (mi.aggregate) {
    if (!mw) throw new EngineError('NETWORK', 'Member weights are needed to aggregate the network nodes.');
    fs = aggregateEdgeComponent(fs, mi, mw, nodeTrade);
    po = aggregateEdgeComponent(po, mi, mw, nodeTrade);
  }
  const kf = keyOf(fs);
  const kp = keyOf(po);
  const fsIdx = new Map<string, number>();
  kf.forEach((k, i) => {
    if (!fsIdx.has(k)) fsIdx.set(k, i);
  });
  const poIdx = new Map<string, number>();
  kp.forEach((k, i) => {
    if (!poIdx.has(k)) poIdx.set(k, i);
  });
  const tech: Str = [];
  const sc: Str = [];
  const rep: Str = [];
  const par: Str = [];
  const fsi: number[] = [];
  const opi: number[] = [];
  const push = (l: Long, i: number, k: string) => {
    tech.push(l.tech[i]);
    sc.push(l.sc[i]);
    rep.push(l.reporter[i]);
    par.push(l.partner[i]);
    const a = fsIdx.get(k);
    const b = poIdx.get(k);
    const f = a === undefined ? NaN : fs.value[a];
    const o = b === undefined ? NaN : po.value[b];
    fsi.push(f !== f ? 0 : f);
    opi.push(o !== o ? 0 : o);
  };
  kf.forEach((k, i) => push(fs, i, k));
  kp.forEach((k, i) => {
    if (!fsIdx.has(k)) push(po, i, k);
  });
  return table({
    tech, supply_chain: sc, reporter_iso: rep, partner_iso: par,
    friendshore_index: Float64Array.from(fsi), opportunity_index: Float64Array.from(opi),
  });
}

export interface NetInputs {
  raw_nodes: Table;
  edges: Table;
  member_weights: Table | null;
  stages: Stage[];
  members: Table;
  mi: MembersInfo;
  nodes: string[];
  node_size: Table | null;
  aggregate: boolean;
  notes: string[];
}

/** Rows of a table whose (tech, supply_chain) is among the stages. */
function filterStages(raw: Table, stages: Stage[]): number[] {
  const keys = new Set(stages.map((s) => `${s.tech} ${s.supply_chain}`));
  const T = str(raw, 'tech');
  const S = str(raw, 'supply_chain');
  const idx: number[] = [];
  for (let i = 0; i < raw.nrow; i++) if (keys.has(`${T[i]} ${S[i]}`)) idx.push(i);
  return idx;
}

function takeTable(t: Table, idx: number[]): Table {
  const cols: Table['cols'] = {};
  for (const [k, c] of Object.entries(t.cols)) {
    if (Array.isArray(c)) cols[k] = idx.map((i) => c[i]);
    else {
      const Ctor = c.constructor as { new (n: number): typeof c };
      const o = new Ctor(idx.length);
      idx.forEach((i, q) => (o[q] = c[i]));
      cols[k] = o;
    }
  }
  return { nrow: idx.length, cols };
}

/** dash_network_inputs(): node inputs aggregated to network nodes and the node-pair edges. */
export function networkInputs(bundle: Pick<Bundle, 'network' | 'dims' | 'partner'>, params: Params, cells: PartnerCells, policy?: PolicyResult | null): NetInputs {
  const nb = requireNetwork(bundle);
  if (!bundle.partner) throw new EngineError('CHUNK', 'The partner chunk is not loaded.');
  const np = params.network;
  const mi = membersInfo(nb.members);
  const stages = networkStages(bundle, params);
  if (!stages.length) throw new EngineError('NETWORK', 'No network stages to solve under these settings.');
  let raw = takeTable(nb.node_raw_member, filterStages(nb.node_raw_member, stages));
  const notes: string[] = [];
  if (np.node_policy && policy) {
    raw = { nrow: raw.nrow, cols: { ...raw.cols, policy_raw: networkPolicyRaw(bundle, policy.index, raw) } };
    notes.push('producer score includes the scenario Policy index');
  }
  let rawNodes: Table;
  let nodesVec: string[];
  if (mi.aggregate) {
    rawNodes = aggregateNodeInputs(raw, mi, nb.member_weights, nb.node_labels ?? null, nb.country_info ?? null, nb.node_size ?? null);
    nodesVec = mi.nodes.map((n) => n.network_node);
  } else {
    rawNodes = raw;
    nodesVec = [...mi.iso3c];
  }
  const fsLong = networkDyads(cells.fs_index, bundle.partner.fs.mask.data, bundle, mi.iso3c, stages);
  const poLong = networkDyads(cells.po_index, bundle.partner.po.mask.data, bundle, mi.iso3c, stages);
  let nodeTrade: Table | null = null;
  if (mi.aggregate && nb.node_trade) {
    // allied_network_member_trade(): flows between members, no self flows, finite values
    const t = nb.node_trade;
    const E = str(t, 'exporter_iso');
    const Im = str(t, 'importer_iso');
    const V = num(t, 'value');
    const mem = new Set(mi.iso3c);
    const idx: number[] = [];
    for (let i = 0; i < t.nrow; i++) if (mem.has(E[i] as string) && mem.has(Im[i] as string) && E[i] !== Im[i] && Number.isFinite(V[i])) idx.push(i);
    nodeTrade = takeTable(t, idx);
  }
  const edges = extractEdges(fsLong, poLong, mi, mi.aggregate ? nb.member_weights : null, nodeTrade);
  return {
    raw_nodes: rawNodes,
    edges,
    member_weights: mi.aggregate ? nb.member_weights : null,
    stages,
    members: nb.members,
    mi,
    nodes: nodesVec,
    node_size: mi.aggregate ? nb.node_size : null,
    aggregate: mi.aggregate,
    notes,
  };
}

export { NA_BOOL, NA_INT };
