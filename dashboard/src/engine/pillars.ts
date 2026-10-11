// L1: Energy Security (ES) and Economic Opportunity (EO) pillars recomputed from the stored
// category scores (port of R/dashboard/dash_pillars.R; spec_react.md 6.4). Pure.

import { formatR7, LdAcc, NA_BOOL } from './numeric.ts';
import { lgl, str, table } from './table.ts';
import type { Bundle, NDArray, NumRecord, Params, PillarInputs, PillarsResult, Str, Table } from './types.ts';

interface PillarOne {
  index: Table;
  category_weight: NDArray<Float64Array>;
  contribution: Float64Array;
}

function pillarOne(pb: PillarInputs, weights: NumRecord, tau: number | null): PillarOne {
  const cats = pb.categories;
  const n = pb.cells.nrow;
  const k = cats.length;
  const inData = new Set(pb.categories_in_data);
  const w = new Float64Array(k);
  for (let j = 0; j < k; j++) {
    const v = weights[cats[j]];
    w[j] = v === undefined || v === null || !inData.has(cats[j]) ? NaN : v;
  }
  const score = pb.score.data;
  const present = pb.present.data;
  const obs = new Uint8Array(n * k);
  for (let j = 0; j < k; j++) {
    for (let i = 0; i < n; i++) {
      const p = i + n * j;
      obs[p] = present[p] === 1 && score[p] === score[p] && w[j] === w[j] ? 1 : 0;
    }
  }
  // weight_sum = rowSums(ifelse(obs, W, 0)) (long double, categories in order)
  const weightSum = new Float64Array(n);
  const acc = new LdAcc();
  for (let i = 0; i < n; i++) {
    acc.reset();
    for (let j = 0; j < k; j++) acc.add(obs[i + n * j] ? w[j] : 0);
    weightSum[i] = acc.value();
  }
  const cw = new Float64Array(n * k);
  const contrib = new Float64Array(n * k);
  const value = new Float64Array(n);
  for (let i = 0; i < n; i++) {
    const scored = weightSum[i] > 0;
    acc.reset();
    let nan = false;
    for (let j = 0; j < k; j++) {
      const p = i + n * j;
      if (!scored) {
        cw[p] = NaN;
        contrib[p] = NaN;
        if (present[p] === 1) nan = true;
        continue;
      }
      const c = obs[p] ? w[j] / weightSum[i] : 0;
      cw[p] = c;
      const ct = c === 0 ? 0 : score[p] * c;
      contrib[p] = ct;
      const c0 = present[p] === 1 ? ct : 0;
      if (c0 !== c0) nan = true;
      else acc.add(c0);
    }
    value[i] = !scored || nan ? NaN : acc.value();
  }
  if (!pb.allow_partial_categories) {
    const expected: number[] = [];
    for (let j = 0; j < k; j++) if (w[j] === w[j] && inData.has(cats[j])) expected.push(j);
    for (let i = 0; i < n; i++) {
      let cnt = 0;
      for (const j of expected) cnt += obs[i + n * j];
      if (cnt !== expected.length) value[i] = NaN;
    }
  }

  // Imputed weight share
  const cwImp = pb.cw_imp.data;
  const cwTot = pb.cw_tot.data;
  const nVar = pb.n_var;
  const iws = new Float64Array(n);
  const accD = new LdAcc();
  for (let i = 0; i < n; i++) {
    if (!(weightSum[i] > 0) || nVar[i] === 0) {
      iws[i] = NaN;
      continue;
    }
    acc.reset();
    accD.reset();
    for (let j = 0; j < k; j++) {
      const p = i + n * j;
      const z = present[p] === 1 ? cw[p] : 0;
      acc.add(z * cwImp[p]);
      accD.add(z * cwTot[p]);
    }
    iws[i] = acc.value() / accD.value();
  }

  // Rankability
  const rankable = new Uint8Array(n);
  const reason: Str = new Array(n).fill(null);
  if (tau === null || tau !== tau) {
    for (let i = 0; i < n; i++) {
      const ok = weightSum[i] > 0 && value[i] === value[i];
      rankable[i] = ok ? 1 : 0;
      reason[i] = ok ? null : 'not scored';
    }
  } else {
    const sov = lgl(pb.cells, 'sovereign');
    const t7 = formatR7(tau);
    for (let i = 0; i < n; i++) {
      const sovereign = sov[i] === 1;
      const known = iws[i] === iws[i];
      const over = known && iws[i] > tau + 1e-9;
      let r: string | null;
      if (value[i] !== value[i]) r = 'not scored';
      else if (!sovereign && over) r = `not a sovereign economy and imputed weight share above ${t7}`;
      else if (!sovereign) r = 'not a sovereign economy';
      else if (!known) r = 'imputed weight share unknown';
      else if (over) r = `imputed weight share above ${t7}`;
      else r = null;
      reason[i] = r;
      rankable[i] = r === null ? 1 : 0;
    }
  }

  const tech = str(pb.cells, 'tech');
  const sc = str(pb.cells, 'supply_chain');
  const sector: Str = tech.map((t, i) => `${t ?? 'NA'} - ${sc[i] ?? 'NA'}`);
  const index = table({
    Country: str(pb.cells, 'Country'),
    iso3c: str(pb.cells, 'iso3c'),
    tech,
    supply_chain: sc,
    sector,
    value,
    imputed_weight_share: iws,
    rankable,
    rank_exclusion: reason,
  });
  return { index, category_weight: { shape: [n, k], data: cw }, contribution: contrib };
}

/** Long contribution table of one pillar (cells with a category row, by cell then category). */
function contribLong(pb: PillarInputs, one: PillarOne): Table {
  const n = pb.cells.nrow;
  const k = pb.categories.length;
  const present = pb.present.data;
  const idx: number[] = [];
  const rows: number[] = [];
  const cols: number[] = [];
  for (let i = 0; i < n; i++) {
    for (let j = 0; j < k; j++) {
      const p = i + n * j;
      if (present[p] === 1) {
        idx.push(p);
        rows.push(i);
        cols.push(j);
      }
    }
  }
  const m = idx.length;
  const Country = str(pb.cells, 'Country');
  const tech = str(pb.cells, 'tech');
  const sc = str(pb.cells, 'supply_chain');
  const cs = new Float64Array(m);
  const cwt = new Float64Array(m);
  const ct = new Float64Array(m);
  const imp = new Uint8Array(m);
  const mr: Str = new Array(m);
  const score = pb.score.data;
  const imputed = pb.imputed.data;
  const missing = pb.missing_rule.data;
  for (let q = 0; q < m; q++) {
    const p = idx[q];
    cs[q] = score[p];
    cwt[q] = one.category_weight.data[p];
    ct[q] = one.contribution[p];
    imp[q] = imputed ? imputed[p] : NA_BOOL;
    mr[q] = missing ? missing[p] : null;
  }
  return table({
    Country: rows.map((i) => Country[i]),
    tech: rows.map((i) => tech[i]),
    supply_chain: rows.map((i) => sc[i]),
    category: cols.map((j) => pb.categories[j]),
    category_score: cs,
    category_weight: cwt,
    contribution: ct,
    imputed: imp,
    missing_rule_applied: mr,
  });
}

/** dash_pillars(): both pillars for the scenario. */
export function pillars(bundle: Pick<Bundle, 'pillars'>, params: Params, opts: { contributions?: boolean } = {}): PillarsResult {
  const pp = params.pillars;
  const tau = pp.tau === undefined ? null : pp.tau;
  const es = pillarOne(bundle.pillars.es, pp.es_weights, tau);
  const eo = pillarOne(bundle.pillars.eo, pp.eo_weights, tau);
  const out: PillarsResult = {
    es: es.index,
    eo: eo.index,
    es_category_weight: es.category_weight,
    eo_category_weight: eo.category_weight,
  };
  if (opts.contributions !== false) {
    out.es_contrib = contribLong(bundle.pillars.es, es);
    out.eo_contrib = contribLong(bundle.pillars.eo, eo);
  }
  return out;
}

/**
 * dash_variable_contrib(): variable rows of one pillar for a country (optionally one cell) with the
 * scenario category weights. Needs the vars chunk (bundle.pillars.<p>.var).
 */
export function variableContrib(
  bundle: Pick<Bundle, 'pillars'>,
  res: PillarsResult,
  pillar: 'es' | 'eo',
  country: string | string[],
  tech?: string | null,
  stage?: string | null,
): Table {
  const pb = bundle.pillars[pillar];
  if (!pb.var) throw new Error('The vars chunk is not loaded.');
  const cw = pillar === 'es' ? res.es_category_weight : res.eo_category_weight;
  const n = pb.cells.nrow;
  const countries = new Set(Array.isArray(country) ? country : [country]);
  const C = str(pb.cells, 'Country');
  const T = str(pb.cells, 'tech');
  const S = str(pb.cells, 'supply_chain');
  const keepCell = new Uint8Array(n);
  for (let i = 0; i < n; i++) {
    keepCell[i] = countries.has(C[i] as string) && (tech == null || T[i] === tech) && (stage == null || S[i] === stage) ? 1 : 0;
  }
  const v = pb.var;
  const cell = v.cols.cell as Int32Array | Float64Array;
  const vcat = str(v, 'category');
  const sel: number[] = [];
  for (let r = 0; r < v.nrow; r++) {
    const c = cell[r];
    if (c >= 1 && c <= n && keepCell[c - 1]) sel.push(r);
  }
  const m = sel.length;
  const catIdx = new Map(pb.categories.map((c, j) => [c, j] as [string, number]));
  const cv = v.cols.component_value as Float64Array;
  const cwwo = v.cols.cwwo as Float64Array;
  const imputed = v.cols.imputed as Uint8Array;
  const mra = str(v, 'missing_rule_applied');
  const sv = str(v, 'score_variable');
  const vv = str(v, 'variable');
  const out = {
    Country: [] as Str, tech: [] as Str, supply_chain: [] as Str, category: [] as Str, score_variable: [] as Str, variable: [] as Str,
    component_value: new Float64Array(m), component_weight_within_overall: new Float64Array(m), category_weight: new Float64Array(m),
    weighted_variable_contribution: new Float64Array(m), imputed: new Uint8Array(m), missing_rule_applied: [] as Str,
  };
  sel.forEach((r, q) => {
    const i = cell[r] - 1;
    const j = catIdx.get(vcat[r] as string);
    const catw = j === undefined ? NaN : cw.data[i + n * j];
    const x = cv[r];
    const wo = cwwo[r];
    let wvc: number;
    if (catw !== catw) wvc = NaN;
    else if (x !== x || wo === 0 || catw === 0) wvc = 0;
    else wvc = x * wo * catw;
    out.Country.push(C[i]);
    out.tech.push(T[i]);
    out.supply_chain.push(S[i]);
    out.category.push(vcat[r]);
    out.score_variable.push(sv[r]);
    out.variable.push(vv[r]);
    out.component_value[q] = x;
    out.component_weight_within_overall[q] = wo;
    out.category_weight[q] = catw;
    out.weighted_variable_contribution[q] = wvc;
    out.imputed[q] = imputed[r];
    out.missing_rule_applied.push(mra[r]);
  });
  return table(out, m);
}
