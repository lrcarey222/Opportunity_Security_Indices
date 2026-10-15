// L4: Friendshore (FS) and Opportunity (PO) dyad cells, best-five summaries, the Partnership
// Strength Index (PSI) and the per-dyad decomposition (port of R/dashboard/dash_partner.R;
// spec_react.md 6.5). Pure.
//
// Arrays are [reporter, partner, cell] over the world economies U and the C cells, column-major:
// element (r, p, c) at r + U * (p + U * c). The weighted mean accumulates the components in the
// pipeline's order with the same operations, so the cells are bit-identical to R's.
//
// Friendshore's CM-FDI parts (since October 2026): in the cells whose outbound_source is 'cm_fdi'
// (Solar, Wind, Batteries and Electric Vehicles Midstream) the outbound component is the CM-FDI
// investment tie outbound_cmfdi[r, p, k] instead of the IMF position outbound[r, p] (NA where the
// reporter's parents have no project there: the component drops out), and chinese_parent_exposure
// [p, c] enters the mean as 1 - share (a risk; NA = undefined, dropped, never 0).

import { cmpC, EngineError, formatR7, LdAcc, ldDivToDouble, medianScurve, NA_INT, orderBy, rankMin, sumR } from './numeric.ts';
import { matchIdx, num, str, table } from './table.ts';
import type { Bundle, NDArray, Params, PartnerCells, PartnerInputs, PsiResult, PsiView, Str, Table, Top5Result } from './types.ts';

function requirePartner(bundle: Pick<Bundle, 'partner'>): PartnerInputs {
  if (!bundle.partner) throw new EngineError('CHUNK', 'The partner chunk is not loaded.');
  return bundle.partner;
}

export const FS_COMPONENTS = [
  'imp_trade_index', 'econ_opp_raw', 'es_need', 'eo_partner', 'outbound_index', 'geo_alignment', 'chinese_parent_exposure',
] as const;
/** Friendshore components stored as risks (higher = worse supplier), scored 1 - value. */
export const FS_RISK_COMPONENTS: readonly string[] = ['chinese_parent_exposure'];
const PO_COMPONENTS = ['trade_index', 'econ_opp_index', 'partner_es_need'] as const;

function weightOf(w: Record<string, number | null | undefined>, k: string): number {
  const v = w[k];
  return v === undefined || v === null ? NaN : v;
}

/** dash_fs_outbound_source(): 'imf_dip' / 'cm_fdi' by cell ('imf_dip' everywhere without the vector). */
export function fsOutboundSource(fs: PartnerInputs['fs'], C: number): string[] {
  const s = fs.outbound_source;
  return Array.from({ length: C }, (_, c) => (s && s[c] === 'cm_fdi' ? 'cm_fdi' : 'imf_dip'));
}

/** The slab k of outbound_cmfdi for each cell (-1: an IMF cell). */
function cmfdiSlot(fs: PartnerInputs['fs'], C: number): Int32Array {
  const src = fsOutboundSource(fs, C);
  const out = new Int32Array(C).fill(-1);
  let k = 0;
  for (let c = 0; c < C; c++) if (src[c] === 'cm_fdi') out[c] = k++;
  if (k > 0 && (!fs.outbound_cmfdi || fs.outbound_cmfdi.shape[2] !== k)) {
    throw new EngineError('BUNDLE', 'partner.fs.outbound_cmfdi does not match outbound_source.');
  }
  return out;
}

/** Median S-curve of each cell's slice over the mask-true dyads with a value (in place on adj). */
function scurveCells(adj: Float64Array, mask: Uint8Array, U: number, C: number, onCell?: (c: number) => void): Float64Array {
  const UU = U * U;
  const out = new Float64Array(U * U * C);
  const idx = new Int32Array(UU);
  for (let c = 0; c < C; c++) {
    const off = c * UU;
    let m = 0;
    for (let i = 0; i < UU; i++) {
      const p = off + i;
      if (mask[p] === 1) {
        idx[m++] = p;
      } else {
        adj[p] = NaN;
      }
    }
    out.fill(NaN, off, off + UU);
    if (m > 0) medianScurve(adj, out, idx.subarray(0, m));
    if (onCell) onCell(c);
  }
  return out;
}

/** Friendshore index array [U, U, C] for the scenario (dash_fs_cells). */
export function fsCells(bundle: Pick<Bundle, 'partner'>, params: Params, onCell?: (c: number) => void): Float64Array {
  const pa = requirePartner(bundle);
  const fs = pa.fs;
  const U = fs.cpi.length;
  const C = fs.gh.length;
  const UU = U * U;
  const w = params.partner.fs_weights;
  const coef = params.partner.fs_penalty;
  const pos = (k: string) => {
    const v = w[k];
    return v !== undefined && v !== null && v > 0;
  };
  const geoOk = !!fs.geo && Array.prototype.some.call(fs.geo.data, (v: number) => v === v);
  const slot = cmfdiSlot(fs, C);
  const anyCm = slot.some((k) => k >= 0);
  // component list in the pipeline's order; null = absent; weight 0 / NA = skipped. 'ob': the
  // outbound component (IMF [r, p] or the CM-FDI slab of the cell); 'pcr': a [p, c] risk, 1 - value
  type Kind = 'rpc' | 'rc' | 'pc' | 'rp' | 'ob' | 'pcr';
  const comps: { kind: Kind; data: Float64Array; data2?: Float64Array; w: number }[] = [];
  const add = (k: string, kind: Kind, data: Float64Array | undefined, data2?: Float64Array) => {
    const wk = weightOf(w, k);
    if (!data || wk !== wk || wk === 0) return;
    comps.push({ kind, data, data2, w: wk });
  };
  const noImf = new Float64Array(0);
  add('imp_trade_index', 'rpc', fs.imp_trade.data);
  if (pos('econ_opp_raw')) add('econ_opp_raw', 'rc', fs.econ_opp_raw?.data);
  if (pos('es_need')) add('es_need', 'rc', fs.es_need?.data);
  if (pos('eo_partner')) add('eo_partner', 'pc', fs.eo_partner?.data);
  if (pos('outbound_index') && (fs.outbound || anyCm)) add('outbound_index', 'ob', fs.outbound?.data ?? noImf, fs.outbound_cmfdi?.data);
  if (pos('geo_alignment') && geoOk) add('geo_alignment', 'rp', fs.geo?.data);
  if (pos('chinese_parent_exposure') && fs.cpe) add('chinese_parent_exposure', 'pcr', fs.cpe.data);
  const adj = new Float64Array(UU * C);
  const cpi = fs.cpi;
  const gh = fs.gh;
  for (let c = 0; c < C; c++) {
    const g1 = 1 - gh[c];
    for (let p = 0; p < U; p++) {
      const penP = g1 * cpi[p] * coef;
      for (let r = 0; r < U; r++) {
        const i = r + U * p + UU * c;
        let numv = 0;
        let den = 0;
        for (let k = 0; k < comps.length; k++) {
          const cp = comps[k];
          let x: number;
          switch (cp.kind) {
            case 'rpc':
              x = cp.data[i];
              break;
            case 'rc':
              x = cp.data[r + U * c];
              break;
            case 'pc':
              x = cp.data[p + U * c];
              break;
            case 'pcr':
              x = 1 - cp.data[p + U * c];
              break;
            case 'ob': {
              const k = slot[c];
              x = k >= 0 ? (cp.data2 as Float64Array)[r + U * p + UU * k] : cp.data.length ? cp.data[r + U * p] : NaN;
              break;
            }
            default:
              x = cp.data[r + U * p];
          }
          if (x === x) {
            numv = numv + x * cp.w;
            den = den + cp.w;
          } else {
            numv = numv + 0;
            den = den + 0;
          }
        }
        const raw = den > 0 ? numv / den : NaN;
        const penR = g1 * cpi[r] * coef;
        const v = raw - (penR + penP);
        adj[i] = v !== v ? NaN : v < 0 ? 0 : v;
      }
    }
  }
  return scurveCells(adj, fs.mask.data, U, C, onCell);
}

/** Opportunity index array [U, U, C] for the scenario (dash_po_cells). */
export function poCells(bundle: Pick<Bundle, 'partner'>, params: Params, onCell?: (c: number) => void): Float64Array {
  const pa = requirePartner(bundle);
  const po = pa.po;
  const U = po.cpi.length;
  const C = po.gh.length;
  const UU = U * U;
  const w = params.partner.po_weights;
  const coef = params.partner.po_penalty;
  const wt = weightOf(w, 'trade_index');
  const we = weightOf(w, 'econ_opp_index');
  const wn = weightOf(w, 'partner_es_need');
  const useT = wt === wt && wt !== 0;
  const useE = we === we && we !== 0;
  const useN = wn === wn && wn !== 0;
  void PO_COMPONENTS;
  const trade = po.trade.data;
  const eoi = po.econ_opp_index.data;
  const pen = po.partner_es_need.data;
  const adj = new Float64Array(UU * C);
  for (let c = 0; c < C; c++) {
    const g1 = 1 - po.gh[c];
    for (let p = 0; p < U; p++) {
      const penP = g1 * po.cpi[p] * coef;
      const xn = pen[p + U * c];
      for (let r = 0; r < U; r++) {
        const i = r + U * p + UU * c;
        let numv = 0;
        let den = 0;
        if (useT) {
          const x = trade[i];
          if (x === x) {
            numv = numv + x * wt;
            den = den + wt;
          }
        }
        if (useE) {
          const x = eoi[r + U * c];
          if (x === x) {
            numv = numv + x * we;
            den = den + we;
          }
        }
        if (useN) {
          if (xn === xn) {
            numv = numv + xn * wn;
            den = den + wn;
          }
        }
        const raw = den > 0 ? numv / den : NaN;
        const v = raw - penP;
        adj[i] = v !== v ? NaN : v < 0 ? 0 : v;
      }
    }
  }
  return scurveCells(adj, po.mask.data, U, C, onCell);
}

/** Cells kept for the best-five (fossil technologies dropped when excluded). */
export function keepCells(bundle: Pick<Bundle, 'dims'>, params: Params): Uint8Array {
  const tech = str(bundle.dims.cells, 'tech');
  const ex = new Set(params.partner.exclude_techs);
  return Uint8Array.from(tech, (t) => (ex.has(t as string) ? 0 : 1));
}

/** Mean of the k largest non-missing values of each dyad over the kept cells; NA without any. */
export function topkMean(A: Float64Array, U: number, C: number, k: number, keep: Uint8Array): Float64Array {
  const UU = U * U;
  const out = new Float64Array(UU);
  const kc: number[] = [];
  for (let c = 0; c < C; c++) if (keep[c]) kc.push(c);
  const top = new Float64Array(Math.max(k, 1));
  const acc = new LdAcc();
  for (let i = 0; i < UU; i++) {
    // k largest (ties: first in cell order), kept in descending order
    let m = 0;
    for (const c of kc) {
      const v = A[i + UU * c];
      if (v !== v) continue;
      if (m < k) {
        let j = m++;
        while (j > 0 && top[j - 1] < v) {
          top[j] = top[j - 1];
          j--;
        }
        top[j] = v;
      } else if (v > top[k - 1]) {
        let j = k - 1;
        while (j > 0 && top[j - 1] < v) {
          top[j] = top[j - 1];
          j--;
        }
        top[j] = v;
      }
    }
    if (m === 0) {
      out[i] = NaN;
      continue;
    }
    acc.reset();
    for (let j = 0; j < m; j++) acc.add(top[j]);
    out[i] = ldDivToDouble(acc.hi, acc.lo, m);
  }
  return out;
}

/** TRUE for the dyads [reporter, partner] with a row in any cell. */
export function anyCell(mask: Uint8Array, U: number, C: number): Uint8Array {
  const UU = U * U;
  const out = new Uint8Array(UU);
  for (let c = 0; c < C; c++) {
    const off = c * UU;
    for (let i = 0; i < UU; i++) if (mask[off + i]) out[i] = 1;
  }
  return out;
}

/** Best-five summaries [U * U] and dyad existence (dash_partner_top5). */
export function partnerTop5(bundle: Pick<Bundle, 'partner' | 'dims'>, cells: PartnerCells, params: Params): Top5Result {
  const pa = requirePartner(bundle);
  const U = pa.U.length;
  const C = pa.fs.gh.length;
  const keep = keepCells(bundle, params);
  const k = params.partner.top_k;
  return {
    fs: topkMean(cells.fs_index, U, C, k, keep),
    po: topkMean(cells.po_index, U, C, k, keep),
    fs_exists: anyCell(pa.fs.mask.data, U, C),
    po_exists: anyCell(pa.po.mask.data, U, C),
  };
}

interface Flag {
  rankable: Uint8Array;
  rank_exclusion: Str;
  rank: Int32Array;
}

/** psi_flag_rankable(); tau null = no imputed-share condition. */
function psiFlag(value: Float64Array, share: Float64Array, sovereign: Uint8Array, tau: number | null): Flag {
  const n = value.length;
  const reason: Str = new Array(n);
  const t7 = tau === null ? '' : formatR7(tau);
  for (let i = 0; i < n; i++) {
    const scored = value[i] === value[i];
    if (tau === null) {
      reason[i] = !scored ? 'not scored' : !sovereign[i] ? 'not a sovereign economy' : null;
    } else {
      const known = share[i] === share[i];
      const over = known && share[i] > tau + 1e-9;
      if (!scored) reason[i] = 'not scored';
      else if (!sovereign[i] && over) reason[i] = `not a sovereign economy and imputed weight share above ${t7}`;
      else if (!sovereign[i]) reason[i] = 'not a sovereign economy';
      else if (!known) reason[i] = 'imputed weight share unknown';
      else if (over) reason[i] = `imputed weight share above ${t7}`;
      else reason[i] = null;
    }
  }
  const rankable = Uint8Array.from(reason, (r) => (r === null ? 1 : 0));
  const rank = new Int32Array(n).fill(NA_INT);
  const ok: number[] = [];
  for (let i = 0; i < n; i++) if (rankable[i]) ok.push(i);
  const neg = Float64Array.from(value, (v) => -v);
  const r = rankMin(neg, ok);
  ok.forEach((i, q) => (rank[i] = r[q]));
  return { rankable, rank_exclusion: reason, rank };
}

/** dash_psi(): dyad table (all world dyads) and the partner-level view. */
export function psi(bundle: Pick<Bundle, 'partner'>, top5: Top5Result, params: Params): PsiResult {
  const pa = requirePartner(bundle);
  const Ucodes = pa.U;
  const U = Ucodes.length;
  const w = params.partner.psi_weights;
  const wts = [w.friendshore, w.opportunity, w.development];
  const dpIdx = matchIdx(Ucodes, str(pa.dp, 'partner_iso'));
  const dpDev = num(pa.dp, 'development');
  const dpShare = num(pa.dp, 'imputed_weight_share');
  const dv = Float64Array.from(dpIdx, (k) => (k >= 0 ? dpDev[k] : NaN));
  const dvShare = Float64Array.from(dpIdx, (k) => (k >= 0 ? dpShare[k] : NaN));
  const tau = pa.psi.tau;
  const shFs = pa.top5_share.fs.data;
  const shPo = pa.top5_share.po.data;

  // Dyads: which(exists, arr.ind = TRUE) order (reporter fastest), diagonal dropped
  const rr: number[] = [];
  const pp: number[] = [];
  for (let p = 0; p < U; p++) {
    for (let r = 0; r < U; r++) {
      if (r === p) continue;
      const i = r + U * p;
      if (top5.fs_exists[i] || top5.po_exists[i]) {
        rr.push(r);
        pp.push(p);
      }
    }
  }
  const m = rr.length;
  const fsV = new Float64Array(m);
  const opV = new Float64Array(m);
  const dV = new Float64Array(m);
  const nCat = new Int32Array(m);
  const psiV = new Float64Array(m);
  const shareV = new Float64Array(m);
  for (let q = 0; q < m; q++) {
    const i = rr[q] + U * pp[q];
    const f = top5.fs_exists[i] ? top5.fs[i] : NaN;
    const o = top5.po_exists[i] ? top5.po[i] : NaN;
    const d = dv[pp[q]];
    fsV[q] = f;
    opV[q] = o;
    dV[q] = d;
    nCat[q] = (f === f ? 1 : 0) + (o === o ? 1 : 0) + (d === d ? 1 : 0);
    // dash_row_wmean(list(fs, op, d), wts)
    const vals = [f, o, d];
    let nm = 0;
    let dn = 0;
    for (let k = 0; k < 3; k++) {
      const wk = wts[k];
      if (wk === undefined || wk === null || wk !== wk || wk === 0) continue;
      const x = vals[k];
      if (x === x) {
        nm = nm + x * wk;
        dn = dn + wk;
      } else {
        nm = nm + 0;
        dn = dn + 0;
      }
    }
    psiV[q] = dn > 0 ? nm / dn : NaN;
    // dash_weighted_share()
    const shares = [shFs[i], shPo[i], dvShare[pp[q]]];
    let sn = 0;
    let sd = 0;
    let unknown = false;
    for (let k = 0; k < 3; k++) {
      const wk = wts[k];
      const present = vals[k] === vals[k] && wk > 0;
      const s = shares[k];
      if (present && s !== s) unknown = true;
      sn = sn + (present ? wk * (s !== s ? 0 : s) : 0);
      sd = sd + (present ? wk : 0);
    }
    shareV[q] = sd > 0 && !unknown ? sn / sd : NaN;
  }
  const repIso: Str = rr.map((r) => Ucodes[r]);
  const parIso: Str = pp.map((p) => Ucodes[p]);
  const ord = orderBy([{ str: repIso }, { str: parIso }], m);
  const O = <T extends Float64Array | Int32Array>(x: T, Ctor: { from(a: number[]): T }) => Ctor.from(ord.map((i) => x[i]));
  const dRep = ord.map((i) => repIso[i]);
  const dPar = ord.map((i) => parIso[i]);
  const dPsi = O(psiV, Float64Array);
  const dShare = O(shareV, Float64Array);
  const uIdx = new Map(Ucodes.map((u, i) => [u, i] as [string, number]));
  const sovP = Uint8Array.from(dPar, (p) => (pa.sovereign[uIdx.get(p as string) as number] === 1 ? 1 : 0));
  const fl = psiFlag(dPsi, dShare, sovP, tau);
  const dyRank = new Int32Array(m).fill(NA_INT);
  const groups = new Map<string | null, number[]>();
  for (let q = 0; q < m; q++) {
    if (!fl.rankable[q]) continue;
    let g = groups.get(dRep[q]);
    if (!g) groups.set(dRep[q], (g = []));
    g.push(q);
  }
  const negPsi = Float64Array.from(dPsi, (v) => -v);
  for (const g of groups.values()) {
    const r = rankMin(negPsi, g);
    g.forEach((q, k) => (dyRank[q] = r[k]));
  }
  const dyad = table({
    reporter_iso: dRep,
    partner_iso: dPar,
    friendshore: O(fsV, Float64Array),
    opportunity: O(opV, Float64Array),
    development: O(dV, Float64Array),
    n_categories: O(nCat, Int32Array),
    psi: dPsi,
    imputed_weight_share: dShare,
    rankable: fl.rankable,
    rank_exclusion: fl.rank_exclusion,
    rank: dyRank,
  });

  // Partner view
  const view: PsiView = params.partner.psi_view === 'world' ? 'world' : 'coalition';
  const reporters = view === 'world' ? [...Ucodes] : pa.psi.coalition_reporters.filter((x, i, a) => uIdx.has(x) && a.indexOf(x) === i);
  const rsel = new Uint8Array(U);
  for (const r of reporters) rsel[uIdx.get(r) as number] = 1;
  const colmean = (M: Float64Array, ex: Uint8Array, shareM: Float64Array) => {
    const value = new Float64Array(U);
    const n = new Int32Array(U);
    const sh = new Float64Array(U);
    for (let p = 0; p < U; p++) {
      const acc = new LdAcc();
      const accS = new LdAcc();
      let cnt = 0;
      let shUnknown = false;
      for (let r = 0; r < U; r++) {
        if (!rsel[r]) continue;
        const i = r + U * p;
        const v = ex[i] && r !== p ? M[i] : NaN;
        if (v !== v) continue;
        cnt++;
        acc.add(v);
        const s = shareM[i];
        if (s !== s) shUnknown = true;
        else accS.add(s);
      }
      n[p] = cnt;
      value[p] = cnt > 0 ? acc.value() / cnt : NaN;
      sh[p] = cnt > 0 && !shUnknown ? accS.value() / cnt : NaN;
    }
    return { value, n, share: sh };
  };
  const cf = colmean(top5.fs, top5.fs_exists, shFs);
  const co = colmean(top5.po, top5.po_exists, shPo);
  // categories in C-locale order: development, friendshore, opportunity
  const Wv = [w.development, w.friendshore, w.opportunity].map((x) => (x === undefined || x === null ? NaN : x));
  const cols = [dv, cf.value, co.value];
  const shs = [dvShare, cf.share, co.share];
  for (let j = 0; j < 3; j++) {
    let any = false;
    for (let p = 0; p < U; p++) if (cols[j][p] === cols[j][p]) any = true;
    if (!any) Wv[j] = NaN;
  }
  const pv = new Float64Array(U);
  const piws = new Float64Array(U);
  const scored = new Uint8Array(U);
  for (let p = 0; p < U; p++) {
    const obs = [0, 1, 2].map((j) => cols[j][p] === cols[j][p] && Wv[j] === Wv[j]);
    const ws = new LdAcc();
    for (let j = 0; j < 3; j++) ws.add(obs[j] ? Wv[j] : 0);
    const wsum = ws.value();
    scored[p] = wsum > 0 ? 1 : 0;
    const acc = new LdAcc();
    for (let j = 0; j < 3; j++) {
      const cw = obs[j] ? Wv[j] / wsum : 0;
      acc.add(cw === 0 ? 0 : cols[j][p] * cw);
    }
    pv[p] = wsum > 0 ? acc.value() : NaN;
    const nmA = new LdAcc();
    const dnA = new LdAcc();
    let unknown = false;
    for (let j = 0; j < 3; j++) {
      const presentW = obs[j] && Wv[j] > 0;
      const s = shs[j][p];
      if (presentW && s !== s) unknown = true;
      nmA.add(presentW ? Wv[j] * (s !== s ? 0 : s) : 0);
      dnA.add(presentW ? Wv[j] : 0);
    }
    piws[p] = !(wsum > 0) || unknown ? NaN : nmA.value() / dnA.value();
  }
  const keepP: number[] = [];
  for (let p = 0; p < U; p++) if (scored[p]) keepP.push(p);
  const Pv = Float64Array.from(keepP, (p) => pv[p]);
  const Ps = Float64Array.from(keepP, (p) => piws[p]);
  const Psov = Uint8Array.from(keepP, (p) => (pa.sovereign[p] === 1 ? 1 : 0));
  const pfl = psiFlag(Pv, Ps, Psov, tau);
  const country: Str = keepP.map((p) => pa.names[p]);
  const pord = orderBy([{ str: country }], keepP.length);
  const P = (i: number) => keepP[i];
  const partner = table({
    partner_iso: pord.map((i) => Ucodes[P(i)]),
    Country: pord.map((i) => country[i]),
    friendshore: Float64Array.from(pord, (i) => cf.value[P(i)]),
    opportunity: Float64Array.from(pord, (i) => co.value[P(i)]),
    development: Float64Array.from(pord, (i) => dv[P(i)]),
    psi: Float64Array.from(pord, (i) => Pv[i]),
    n_reporters_friendshore: Int32Array.from(pord, (i) => cf.n[P(i)]),
    n_reporters_opportunity: Int32Array.from(pord, (i) => co.n[P(i)]),
    imputed_weight_share: Float64Array.from(pord, (i) => Ps[i]),
    rankable: Uint8Array.from(pord, (i) => pfl.rankable[i]),
    rank_exclusion: pord.map((i) => pfl.rank_exclusion[i]),
    rank: Int32Array.from(pord, (i) => pfl.rank[i]),
  });
  return { dyad, partner, reporters, view };
}

/** dash_partner_decompose(): per-cell decomposition of one dyad. */
export function partnerDecompose(
  bundle: Pick<Bundle, 'partner' | 'dims'>, params: Params, cells: PartnerCells, reporterIso: string, partnerIso: string,
): Table {
  const pa = requirePartner(bundle);
  const U = pa.U.length;
  const r = pa.U.indexOf(reporterIso);
  const p = pa.U.indexOf(partnerIso);
  if (r < 0 || p < 0) throw new EngineError('PARAMS', 'Unknown reporter or partner ISO3.');
  const fs = pa.fs;
  const po = pa.po;
  const wf = params.partner.fs_weights;
  const wp = params.partner.po_weights;
  const C = pa.cells.nrow;
  const UU = U * U;
  const col = (f: (c: number) => number) => Float64Array.from({ length: C }, (_, c) => f(c));
  const obSrc = fsOutboundSource(fs, C);
  const slot = cmfdiSlot(fs, C);
  // component values as stored (chinese_parent_exposure: the share); scoreFs, the values entering
  // the mean, differ only for the risk component (1 - share)
  const compFs: Record<string, Float64Array> = {
    imp_trade_index: col((c) => fs.imp_trade.data[r + U * p + UU * c]),
    econ_opp_raw: col((c) => (fs.econ_opp_raw ? fs.econ_opp_raw.data[r + U * c] : NaN)),
    es_need: col((c) => (fs.es_need ? fs.es_need.data[r + U * c] : NaN)),
    eo_partner: col((c) => (fs.eo_partner ? fs.eo_partner.data[p + U * c] : NaN)),
    outbound_index: col((c) =>
      slot[c] >= 0 ? (fs.outbound_cmfdi as NDArray<Float64Array>).data[r + U * p + UU * slot[c]] : fs.outbound ? fs.outbound.data[r + U * p] : NaN,
    ),
    geo_alignment: col(() => (fs.geo ? fs.geo.data[r + U * p] : NaN)),
    chinese_parent_exposure: col((c) => (fs.cpe ? fs.cpe.data[p + U * c] : NaN)),
  };
  const scoreFs: Record<string, Float64Array> = { ...compFs };
  for (const k of FS_RISK_COMPONENTS) scoreFs[k] = Float64Array.from(compFs[k], (x) => 1 - x);
  const wfs = Object.keys(compFs).map((k) => (wf[k] === undefined || wf[k] === null ? 0 : wf[k]));
  const rowWmean = (comps: Float64Array[], ws: number[]) => {
    const out = new Float64Array(C);
    for (let c = 0; c < C; c++) {
      let nm = 0;
      let dn = 0;
      for (let k = 0; k < comps.length; k++) {
        const wk = ws[k];
        if (wk === undefined || wk === null || wk !== wk || wk === 0) continue;
        const x = comps[k][c];
        if (x === x) {
          nm = nm + x * wk;
          dn = dn + wk;
        } else {
          nm = nm + 0;
          dn = dn + 0;
        }
      }
      out[c] = dn > 0 ? nm / dn : NaN;
    }
    return out;
  };
  const denOf = (comps: Float64Array[], ws: number[]) =>
    Float64Array.from({ length: C }, (_, c) => {
      // Reduce(`+`, Map(function(x, wk) (!is.na(x)) * wk, ...)): plain double additions in order
      let d = 0;
      for (let k = 0; k < comps.length; k++) d = (k === 0 ? 0 : d) + (comps[k][c] === comps[k][c] ? 1 : 0) * ws[k];
      return d;
    });
  const fsList = Object.values(compFs);
  const denFs = denOf(fsList, wfs);
  const fsRaw = rowWmean(Object.keys(compFs).map((k) => scoreFs[k]), wfs);
  const penR = col((c) => (1 - fs.gh[c]) * fs.cpi[r] * params.partner.fs_penalty);
  const penP = col((c) => (1 - fs.gh[c]) * fs.cpi[p] * params.partner.fs_penalty);
  const fsAdj = col((c) => {
    const v = fsRaw[c] - (penR[c] + penP[c]);
    const a = v !== v ? NaN : Math.max(0, v);
    return fs.mask.data[r + U * p + UU * c] ? a : NaN;
  });
  const compPo: Record<string, Float64Array> = {
    trade_index: col((c) => po.trade.data[r + U * p + UU * c]),
    econ_opp_index: col((c) => po.econ_opp_index.data[r + U * c]),
    partner_es_need: col((c) => po.partner_es_need.data[p + U * c]),
  };
  const wpo = Object.keys(compPo).map((k) => (wp[k] === undefined || wp[k] === null ? NaN : wp[k]));
  const poList = Object.values(compPo);
  const denPo = denOf(poList, wpo);
  const poRaw = rowWmean(poList, wpo);
  const penPo = col((c) => (1 - po.gh[c]) * po.cpi[p] * params.partner.po_penalty);
  const poAdj = col((c) => {
    const v = poRaw[c] - penPo[c];
    const a = v !== v ? NaN : Math.max(0, v);
    return po.mask.data[r + U * p + UU * c] ? a : NaN;
  });
  const fsi = col((c) => cells.fs_index[r + U * p + UU * c]);
  const opi = col((c) => cells.po_index[r + U * p + UU * c]);
  const keep = keepCells(bundle, params);
  const topflag = (x: Float64Array) => {
    // order(-x2, na.last = NA): kept, non-NA cells by value desc (stable), first top_k flagged
    const idx: number[] = [];
    for (let c = 0; c < C; c++) if (keep[c] && x[c] === x[c]) idx.push(c);
    idx.sort((a, b) => (x[a] === x[b] ? a - b : x[b] - x[a]));
    const flag = new Uint8Array(C);
    for (const c of idx.slice(0, params.partner.top_k)) flag[c] = 1;
    return flag;
  };
  const out: Record<string, Table['cols'][string]> = {
    reporter_iso: new Array(C).fill(reporterIso),
    partner_iso: new Array(C).fill(partnerIso),
    tech: str(pa.cells, 'tech').slice(),
    supply_chain: str(pa.cells, 'supply_chain').slice(),
  };
  Object.keys(compFs).forEach((k, q) => {
    const x = compFs[k];
    const sc = scoreFs[k];
    out[`fs_${k}`] = x;
    out[`fs_${k}_share`] = col((c) => (x[c] !== x[c] || denFs[c] === 0 ? NaN : (sc[c] * wfs[q]) / denFs[c]));
  });
  out.fs_outbound_source = obSrc;
  out.fs_penalty_reporter = penR;
  out.fs_penalty_partner = penP;
  out.fsi_raw = fsRaw;
  out.fsi_adj = fsAdj;
  out.friendshore_index = fsi;
  Object.keys(compPo).forEach((k, q) => {
    const x = compPo[k];
    out[`po_${k}`] = x;
    out[`po_${k}_share`] = col((c) => (x[c] !== x[c] || denPo[c] === 0 ? NaN : (x[c] * wpo[q]) / denPo[c]));
  });
  out.po_penalty = penPo;
  out.opportunity_raw = poRaw;
  out.opportunity_adj = poAdj;
  out.opportunity_index = opi;
  out.in_top5_fs = topflag(fsi);
  out.in_top5_po = topflag(opi);
  out.excluded = Uint8Array.from(keep, (k) => (k ? 0 : 1));
  return table(out, C);
}

/** Sum of a column restricted to rows (helper for summaries). */
export function sumCol(x: Float64Array): number {
  return sumR(x, 0, x.length, true);
}

export { cmpC };
