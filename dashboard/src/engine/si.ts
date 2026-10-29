// L3: the Strategic Index (port of R/dashboard/dash_si.R; spec_react.md 6.4). Pure.
//
//   SI = w_pol pol + w_eo EO + w_need (1 - ES) + w_sc lambda_s + w_tech g_t
//
// over the cells with EO, ES and Policy observed and both pillar cells rankable; pol / eo / es_need
// are within-country median S-curves; fossil exclusion and the degenerate-sector rule as in R.
//
// Each cell also carries the country's policy_basis ("IEA PAMS + CAT" / "IEA PAMS only"; NA without
// a Policy value) and the endowment screen of the allied picks (flag_strategic_resource_base()):
// resource_base TRUE / FALSE in a screened Extraction stage of bundle.si.resource_base, NA elsewhere,
// and pick_exclusion "no resource base" where FALSE. alliedSelection() skips those cells while
// params.si.require_resource_base is on (the default), and picks only in the stages of
// baselineInputs.allied_stages (every stage but Deployment).

import { cmpC, EngineError, medianScurve, meanR, NA_BOOL, orderBy, rankMin, sdR } from './numeric.ts';
import { has, lgl, matchIdx, num, rowKeys, str, table } from './table.ts';
import type { Bundle, NumRecord, Params, PillarsResult, PolicyResult, SiResult, Str, Table } from './types.ts';

function filterTechs(t: Table, techs: Set<string>): Table {
  const tech = str(t, 'tech');
  const idx: number[] = [];
  for (let i = 0; i < t.nrow; i++) if (techs.has(tech[i] as string)) idx.push(i);
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

/** Median S-curve within groups over the rows flagged keep (NA elsewhere). */
function scurveBy(x: Float64Array, keep: Uint8Array, group: Str): Float64Array {
  const out = new Float64Array(x.length).fill(NaN);
  const groups = new Map<string | null, number[]>();
  for (let i = 0; i < x.length; i++) {
    if (keep[i] === 1 && x[i] === x[i]) {
      let g = groups.get(group[i]);
      if (!g) groups.set(group[i], (g = []));
      g.push(i);
    }
  }
  for (const idx of groups.values()) medianScurve(x, out, idx);
  return out;
}

/** Row-wise "; "-join of the non-NA reasons. */
function pasteReasons(parts: Str[], n: number): Str {
  const out: Str = new Array(n).fill(null);
  for (const r of parts) {
    for (let i = 0; i < n; i++) {
      const v = r[i];
      if (v === null) continue;
      out[i] = out[i] === null ? v : `${out[i]}; ${v}`;
    }
  }
  return out;
}

interface SiBuild {
  Country: Str;
  tech: Str;
  supply_chain: Str;
  sector: Str;
  EOI: Float64Array;
  ESI: Float64Array;
  need: Float64Array;
  value: Float64Array;
  policy_basis: Str;
  ghg: Float64Array;
  trl: Float64Array;
  eo: Float64Array;
  es_need: Float64Array;
  pol: Float64Array;
  sc_weight: Float64Array;
  tech_weight: Float64Array;
  strategic_index: Float64Array;
  si_exclusion: Str;
  in_sample: Uint8Array;
  n: number;
}

function siBuild(
  eo: Table, es: Table, pol: Table, techGhg: Table, trl: Table | null, weights: NumRecord, scw: NumRecord, excluded: Set<string>,
): SiBuild {
  const keyEo = rowKeys([str(eo, 'Country'), str(eo, 'tech'), str(eo, 'supply_chain')], eo.nrow);
  const mEs = matchIdx(keyEo, rowKeys([str(es, 'Country'), str(es, 'tech'), str(es, 'supply_chain')], es.nrow));
  const mPol = matchIdx(keyEo, rowKeys([str(pol, 'Country'), str(pol, 'tech'), str(pol, 'supply_chain')], pol.nrow));
  // ghg: first row per tech
  const gTech = str(techGhg, 'tech');
  const gVal = num(techGhg, 'ghg_index');
  const ghgBy = new Map<string | null, number>();
  for (let i = 0; i < techGhg.nrow; i++) if (!ghgBy.has(gTech[i])) ghgBy.set(gTech[i], gVal[i]);
  const ghgVals = Array.from(ghgBy.values()).filter((v) => v === v);
  const ghgMean = ghgVals.length ? meanR(ghgVals) : NaN;
  const trlKey = trl ? rowKeys([str(trl, 'Country'), str(trl, 'tech'), str(trl, 'supply_chain')], trl.nrow) : null;
  const mTrl = trlKey ? matchIdx(keyEo, trlKey) : null;
  const trlVal = trl ? num(trl, 'trl_index') : null;

  const eoC = str(eo, 'Country');
  const eoT = str(eo, 'tech');
  const eoS = str(eo, 'supply_chain');
  const eoV = num(eo, 'value');
  const eoR = lgl(eo, 'rankable');
  const eoX = str(eo, 'rank_exclusion');
  const esV = num(es, 'value');
  const esR = lgl(es, 'rankable');
  const esX = str(es, 'rank_exclusion');
  const polC = str(pol, 'Country');
  const polV = num(pol, 'value');
  const polB: Str | null = has(pol, 'policy_basis') ? str(pol, 'policy_basis') : null;
  const policyCountries = new Set<string | null>();
  for (let i = 0; i < pol.nrow; i++) if (polV[i] === polV[i]) policyCountries.add(polC[i]);

  const rows: number[] = [];
  for (let i = 0; i < eo.nrow; i++) {
    const sector = `${eoT[i] ?? 'NA'} - ${eoS[i] ?? 'NA'}`;
    if (!excluded.has(sector)) rows.push(i);
  }
  const n = rows.length;
  const b: SiBuild = {
    Country: [], tech: [], supply_chain: [], sector: [], policy_basis: [],
    EOI: new Float64Array(n), ESI: new Float64Array(n), need: new Float64Array(n), value: new Float64Array(n),
    ghg: new Float64Array(n), trl: new Float64Array(n), eo: new Float64Array(n), es_need: new Float64Array(n),
    pol: new Float64Array(n), sc_weight: new Float64Array(n), tech_weight: new Float64Array(n),
    strategic_index: new Float64Array(n), si_exclusion: [], in_sample: new Uint8Array(n), n,
  };
  const eoRank = new Uint8Array(n);
  const esRank = new Uint8Array(n);
  const r1: Str = [], r2: Str = [], r3: Str = [], r4: Str = [], r5: Str = [], r6: Str = [], r7: Str = [];
  rows.forEach((i, q) => {
    const c = eoC[i];
    b.Country.push(c);
    b.tech.push(eoT[i]);
    b.supply_chain.push(eoS[i]);
    b.sector.push(`${eoT[i] ?? 'NA'} - ${eoS[i] ?? 'NA'}`);
    const eoi = eoV[i];
    const me = mEs[i];
    const esi = me >= 0 ? esV[me] : NaN;
    const mp = mPol[i];
    const pv = mp >= 0 ? polV[mp] : NaN;
    const g = ghgBy.has(eoT[i]) ? (ghgBy.get(eoT[i]) as number) : NaN;
    b.EOI[q] = eoi;
    b.ESI[q] = esi;
    b.value[q] = pv;
    b.policy_basis.push(polB && mp >= 0 ? polB[mp] : null);
    b.ghg[q] = g;
    b.trl[q] = mTrl && trlVal ? (mTrl[i] >= 0 ? trlVal[mTrl[i]] : NaN) : NaN;
    const eoRk = eoR[i] === 1;
    const esRk = me >= 0 && esR[me] === 1;
    eoRank[q] = eoRk ? 1 : 0;
    esRank[q] = esRk ? 1 : 0;
    let eoEx = eoX[i];
    if (!eoRk && eoEx === null) eoEx = 'flagged not rankable';
    let esEx = me >= 0 ? esX[me] : null;
    if (!esRk && esEx === null) esEx = 'flagged not rankable';
    b.need[q] = 1 - esi;
    const scw0 = scw[eoS[i] as string];
    b.sc_weight[q] = scw0 === undefined || scw0 === null ? NaN : scw0;
    b.tech_weight[q] = g !== g ? ghgMean : g;
    const inPol = policyCountries.has(c);
    r1.push(!inPol ? 'country not policy-eligible' : null);
    r2.push(inPol && pv !== pv ? 'missing policy index' : null);
    r3.push(eoi !== eoi ? 'missing economic opportunity' : null);
    r4.push(esi !== esi ? 'missing energy security' : null);
    r5.push(eoi === eoi && !eoRk ? `economic opportunity not rankable (${eoEx ?? 'NA'})` : null);
    r6.push(esi === esi && !esRk ? `energy security not rankable (${esEx ?? 'NA'})` : null);
    r7.push(b.sc_weight[q] !== b.sc_weight[q] ? 'unknown supply chain stage' : null);
  });
  b.si_exclusion = pasteReasons([r1, r2, r3, r4, r5, r6, r7], n);
  for (let q = 0; q < n; q++) b.in_sample[q] = b.si_exclusion[q] === null ? 1 : 0;
  b.pol = scurveBy(b.value, b.in_sample, b.Country);
  b.eo = scurveBy(b.EOI, eoRank, b.Country);
  b.es_need = scurveBy(b.need, esRank, b.Country);
  const wp = weights.policy;
  const we = weights.economic_opportunity;
  const wn = weights.energy_security_need;
  const ws = weights.supply_chain;
  const wt = weights.technology;
  for (let q = 0; q < n; q++) {
    const si = wp * b.pol[q] + we * b.EOI[q] + wn * b.need[q] + ws * b.sc_weight[q] + wt * b.tech_weight[q];
    b.strategic_index[q] = b.in_sample[q] ? si : NaN;
  }
  return b;
}

/** strategic_index_degenerate_sectors(): sectors where ES or EO has sd <= tol over >= 2 countries. */
function siDegenerate(b: SiBuild, tol = 1e-12): Table {
  const bySector = new Map<string, number[]>();
  for (let q = 0; q < b.n; q++) {
    if (b.strategic_index[q] !== b.strategic_index[q]) continue;
    const s = b.sector[q] as string;
    let g = bySector.get(s);
    if (!g) bySector.set(s, (g = []));
    g.push(q);
  }
  const secs = Array.from(bySector.keys()).sort(cmpC);
  const out = { sector: [] as Str, n_countries: [] as number[], es_constant: [] as number[], eo_constant: [] as number[], es_sd: [] as number[], eo_sd: [] as number[] };
  for (const s of secs) {
    const idx = bySector.get(s)!;
    const n = new Set(idx.map((q) => b.Country[q])).size;
    const esSd = sdR(idx.map((q) => b.ESI[q]));
    const eoSd = sdR(idx.map((q) => b.EOI[q]));
    const esC = n > 1 && esSd <= tol;
    const eoC = n > 1 && eoSd <= tol;
    if (esC || eoC) {
      out.sector.push(s);
      out.n_countries.push(n);
      out.es_constant.push(esC ? 1 : 0);
      out.eo_constant.push(eoC ? 1 : 0);
      out.es_sd.push(esSd);
      out.eo_sd.push(eoSd);
    }
  }
  return table({
    sector: out.sector,
    n_countries: Int32Array.from(out.n_countries),
    es_constant: Uint8Array.from(out.es_constant),
    eo_constant: Uint8Array.from(out.eo_constant),
    es_sd: Float64Array.from(out.es_sd),
    eo_sd: Float64Array.from(out.eo_sd),
  });
}

/** dash_si_resource_base(): resource_base / resource_evidence / pick_exclusion of the cells against the
 * screen's table (a screened stage without a row for the country = FALSE; unscreened stages and a
 * bundle without the table = NA). */
function siResourceBase(b: SiBuild, rb: Table | null | undefined): { base: Uint8Array; evidence: Str; exclusion: Str } {
  const n = b.n;
  const base = new Uint8Array(n).fill(NA_BOOL);
  const evidence: Str = new Array(n).fill(null);
  const exclusion: Str = new Array(n).fill(null);
  if (!rb || !rb.nrow || !n) return { base, evidence, exclusion };
  const rC = str(rb, 'Country');
  const rT = str(rb, 'tech');
  const rS = str(rb, 'supply_chain');
  const rV = lgl(rb, 'resource_base');
  const rE: Str | null = has(rb, 'resource_evidence') ? str(rb, 'resource_evidence') : null;
  const screened = new Set<string>();
  for (let i = 0; i < rb.nrow; i++) screened.add(`${rT[i]}\r${rS[i]}`);
  const k = matchIdx(rowKeys([b.Country, b.tech, b.supply_chain], n), rowKeys([rC, rT, rS], rb.nrow));
  for (let q = 0; q < n; q++) {
    if (!screened.has(`${b.tech[q]}\r${b.supply_chain[q]}`)) continue;
    const j = k[q];
    const v = j >= 0 && rV[j] === 1;
    base[q] = v ? 1 : 0;
    evidence[q] = j >= 0 && rE ? rE[j] : null;
    if (!v) exclusion[q] = 'no resource base';
  }
  return { base, evidence, exclusion };
}

/** dash_si(): the Strategic Index cells, excluded sectors, weights and degenerate sectors. */
export function si(bundle: Pick<Bundle, 'dims' | 'si' | 'pillars'>, params: Params, pil: PillarsResult, pol: PolicyResult): SiResult {
  const sp = params.si;
  const techs = new Set(bundle.dims.techs);
  const eo = filterTechs(pil.eo, techs);
  const es = filterTechs(pil.es, techs);
  const po = filterTechs(pol.index, techs);
  const trl = bundle.si.trl;
  const fossilSectors: string[] = [];
  if (sp.fossil_excluded) {
    for (const st of bundle.dims.stages) for (const t of sp.fossil_techs) fossilSectors.push(`${t} - ${st}`);
  }
  const excl1 = Array.from(new Set([...sp.excluded_sectors, ...fossilSectors]));
  let b = siBuild(eo, es, po, bundle.si.tech_ghg, trl, sp.weights, sp.stage_weights, new Set(excl1));
  const degenerate = siDegenerate(b);
  let excl = excl1;
  if (degenerate.nrow) {
    excl = Array.from(new Set([...excl1, ...(degenerate.cols.sector as string[])]));
    b = siBuild(eo, es, po, bundle.si.tech_ghg, trl, sp.weights, sp.stage_weights, new Set(excl));
  }
  const w = sp.weights;
  const n = b.n;
  const cPol = new Float64Array(n), cEo = new Float64Array(n), cEs = new Float64Array(n), cSc = new Float64Array(n), cGhg = new Float64Array(n);
  for (let q = 0; q < n; q++) {
    cPol[q] = w.policy * b.pol[q];
    cEo[q] = w.economic_opportunity * b.EOI[q];
    cEs[q] = w.energy_security_need * b.need[q];
    cSc[q] = w.supply_chain * b.sc_weight[q];
    cGhg[q] = w.technology * b.tech_weight[q];
  }
  // rank within country (ties = min) over the scored cells
  const rankIn = new Int32Array(n).fill(-2147483648);
  const nIn = new Int32Array(n).fill(-2147483648);
  const groups = new Map<string | null, number[]>();
  for (let q = 0; q < n; q++) {
    if (b.strategic_index[q] !== b.strategic_index[q]) continue;
    let g = groups.get(b.Country[q]);
    if (!g) groups.set(b.Country[q], (g = []));
    g.push(q);
  }
  const neg = new Float64Array(n);
  for (let q = 0; q < n; q++) neg[q] = -b.strategic_index[q];
  for (const g of groups.values()) {
    const r = rankMin(neg, g);
    g.forEach((q, k) => {
      rankIn[q] = r[k];
      nIn[q] = g.length;
    });
  }
  const cm = bundle.pillars.eo.cells;
  const mIso = matchIdx(b.Country, str(cm, 'Country'));
  const cmIso = str(cm, 'iso3c');
  const iso3c: Str = Array.from(mIso, (k) => (k >= 0 ? cmIso[k] : null));
  const rb = siResourceBase(b, bundle.si.resource_base);
  // order(-strategic_index, method = "radix", na.last = TRUE)
  const ord = orderBy([{ num: b.strategic_index, desc: true }], n);
  const pickS = (x: Str) => ord.map((i) => x[i]);
  const pickF = (x: Float64Array) => Float64Array.from(ord, (i) => x[i]);
  const pickI = (x: Int32Array) => Int32Array.from(ord, (i) => x[i]);
  const cells = table({
    Country: pickS(b.Country),
    iso3c: pickS(iso3c),
    tech: pickS(b.tech),
    supply_chain: pickS(b.supply_chain),
    sector: pickS(b.sector),
    Economic_Opportunity_Index: pickF(b.EOI),
    Energy_Security_Index: pickF(b.ESI),
    energy_security_need: pickF(b.need),
    value: pickF(b.value),
    policy_basis: pickS(b.policy_basis),
    ghg_index: pickF(b.ghg),
    trl_index: pickF(b.trl),
    eo: pickF(b.eo),
    es_need: pickF(b.es_need),
    es: pickF(b.es_need),
    pol: pickF(b.pol),
    sc_weight: pickF(b.sc_weight),
    tech_weight: pickF(b.tech_weight),
    strategic_index: pickF(b.strategic_index),
    si_exclusion: pickS(b.si_exclusion),
    in_sample: Uint8Array.from(ord, (i) => b.in_sample[i]),
    resource_base: Uint8Array.from(ord, (i) => rb.base[i]),
    resource_evidence: pickS(rb.evidence),
    pick_exclusion: pickS(rb.exclusion),
    contrib_policy: pickF(cPol),
    contrib_eo: pickF(cEo),
    contrib_es_need: pickF(cEs),
    contrib_sc: pickF(cSc),
    contrib_ghg: pickF(cGhg),
    rank_in_country: pickI(rankIn),
    n_in_country: pickI(nIn),
  });
  const fossilSet = new Set(fossilSectors);
  const degSet = new Set(degenerate.cols.sector as string[]);
  const excluded = table({
    sector: excl,
    reason: excl.map((s) =>
      fossilSet.has(s)
        ? 'excluded: fossil technology (greenness)'
        : degSet.has(s)
          ? 'excluded: a pillar is constant across countries'
          : 'excluded: not reported (pipeline rule)',
    ),
  });
  return { cells, excluded, weights: { ...w }, stage_weights: { ...sp.stage_weights }, degenerate };
}

/** select_allied_strategic_sectors(): each coalition member's top sector (ties kept) in the stages of
 * baselineInputs.allied_stages (the pipeline's STRATEGIC_ALLIED_STAGES: every stage but Deployment),
 * outside Geothermal and Green Hydrogen. With params.si.require_resource_base (the default; params
 * absent = on) a cell flagged resource_base FALSE is not eligible. The picks carry the cell's
 * policy_basis and screen flags. A bundle without allied_stages throws (no stage list is assumed). */
export function alliedSelection(siRes: SiResult, bundle: Pick<Bundle, 'si' | 'baselineInputs'>, params?: Pick<Params, 'si'>): Table {
  const allies = bundle.si.allies;
  const ex = new Set(bundle.baselineInputs.allied_excluded_techs ?? ['Geothermal', 'Green Hydrogen']);
  const stagesIn = bundle.baselineInputs.allied_stages;
  if (!stagesIn) throw new EngineError('BUNDLE', 'baseline_inputs.allied_stages is missing; rebuild the data with scripts/85.');
  const alliedStages = new Set(stagesIn);
  const screen = !params || params.si.require_resource_base !== false;
  const x = siRes.cells;
  const aC = str(allies, 'Country');
  const allySet = new Set(aC);
  const C = str(x, 'Country');
  const T = str(x, 'tech');
  const S = str(x, 'supply_chain');
  const SI = num(x, 'strategic_index');
  const RB = has(x, 'resource_base') ? lgl(x, 'resource_base') : new Uint8Array(x.nrow).fill(NA_BOOL);
  const PB: Str = has(x, 'policy_basis') ? str(x, 'policy_basis') : new Array(x.nrow).fill(null);
  const PX: Str = has(x, 'pick_exclusion') ? str(x, 'pick_exclusion') : new Array(x.nrow).fill(null);
  const sel: number[] = [];
  for (let i = 0; i < x.nrow; i++) {
    if (screen && RB[i] === 0) continue;
    if (allySet.has(C[i]) && alliedStages.has(S[i] as string) && SI[i] === SI[i] && !ex.has(T[i] as string)) sel.push(i);
  }
  const best = new Map<string | null, number>();
  for (const i of sel) {
    const v = best.get(C[i]);
    if (v === undefined || SI[i] > v) best.set(C[i], SI[i]);
  }
  const keep = sel.filter((i) => SI[i] === best.get(C[i]));
  const ma = matchIdx(keep.map((i) => C[i]), aC);
  const aIso = str(allies, 'iso3');
  const aTier = str(allies, 'tier');
  const aNode = str(allies, 'network_node');
  const sector = str(x, 'sector');
  const rows = keep.map((i, k) => ({
    iso3c: ma[k] >= 0 ? aIso[ma[k]] : null,
    Country: C[i],
    tier: ma[k] >= 0 ? aTier[ma[k]] : null,
    network_node: ma[k] >= 0 ? aNode[ma[k]] : null,
    tech: T[i],
    supply_chain: S[i],
    sector: sector[i],
    strategic_index: SI[i],
    policy_basis: PB[i],
    resource_base: RB[i],
    pick_exclusion: PX[i],
  }));
  const ord = orderBy([{ str: rows.map((r) => r.Country) }, { str: rows.map((r) => r.sector) }], rows.length);
  const o = ord.map((k) => rows[k]);
  return table({
    iso3c: o.map((r) => r.iso3c),
    Country: o.map((r) => r.Country),
    tier: o.map((r) => r.tier),
    network_node: o.map((r) => r.network_node),
    tech: o.map((r) => r.tech),
    supply_chain: o.map((r) => r.supply_chain),
    sector: o.map((r) => r.sector),
    strategic_index: Float64Array.from(o.map((r) => r.strategic_index)),
    policy_basis: o.map((r) => r.policy_basis),
    resource_base: Uint8Array.from(o.map((r) => r.resource_base)),
    pick_exclusion: o.map((r) => r.pick_exclusion),
  }, o.length);
}
