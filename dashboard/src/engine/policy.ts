// L2: the composite Policy index for any component weights (port of R/dashboard/dash_policy.R;
// spec_react.md 6.4). Pure.
//
// Eligibility follows bundle.policy.required (since 9 Oct 2026: IEA PAMS required, CAT where it
// rates), so an eligible country's index rests on "IEA PAMS + CAT" or "IEA PAMS only": policy_basis,
// on the eligibility rows and the index rows (and, through si(), the SI cells and allied picks). The
// labels and the missing-component lists follow the pipeline's component order
// (bundle.policy.components).

import { cmpC, EngineError, LdAcc } from './numeric.ts';
import { str, table } from './table.ts';
import type { Bundle, NumRecord, Params, PolicyResult, Str } from './types.ts';

/** policy_component_label(): "IEA PAMS Policy Index" -> "IEA PAMS". */
export function policyComponentLabel(x: string): string {
  return x.replace(/ (Policy Index|policy score)$/, '');
}

/** policy_basis_label(): the components an index rests on, in the pipeline's order ("IEA PAMS + CAT", "IEA PAMS only"). */
export function policyBasisLabel(components: string[]): string {
  const labels = components.map(policyComponentLabel);
  return labels.length === 1 ? `${labels[0]} only` : labels.join(' + ');
}

export function policy(bundle: Pick<Bundle, 'policy'>, params: Params): PolicyResult {
  const pb = bundle.policy;
  const w = params.policy.weights;
  const vars = pb.variables;
  const missing = Object.keys(w).filter((k) => !vars.includes(k));
  if (missing.length) throw new EngineError('PARAMS', `Policy weights name components the bundle lacks: ${missing.join(', ')}.`);
  const useIdx: number[] = [];
  vars.forEach((v, j) => {
    if (v in w) useIdx.push(j);
  });
  const use = useIdx.map((j) => vars[j]);
  const n = pb.cells.nrow;
  const V = pb.values.data;
  const nr = pb.values.shape[0];
  const country = str(pb.cells, 'Country');

  // Eligibility: each required component observed somewhere for the country (bundle.policy.required,
  // the pipeline's required_components; absent, every weighted component is required)
  const reqQ = pb.required ? use.map((v, q) => (pb.required!.includes(v) ? q : -1)).filter((q) => q >= 0) : [];
  const req = reqQ.length ? reqQ : use.map((_, q) => q);
  const byCountry = new Map<string, boolean[]>();
  for (let i = 0; i < n; i++) {
    const c = country[i] as string;
    let h = byCountry.get(c);
    if (!h) {
      h = use.map(() => false);
      byCountry.set(c, h);
    }
    useIdx.forEach((j, q) => {
      const v = V[i + nr * j];
      if (v === v) h![q] = true;
    });
  }
  // the pipeline's component order (its component_weights), for the basis label and the missing lists
  const comp = pb.components ?? [];
  const ordQ = [...comp.filter((v) => use.includes(v)), ...use.filter((v) => !comp.includes(v))].map((v) => use.indexOf(v));
  const reqSet = new Set(req);
  const countries = Array.from(byCountry.keys());
  const elig = countries.map((c) => {
    const h = byCountry.get(c)!;
    const nComp = h.filter(Boolean).length;
    const eligible = req.every((q) => h[q]);
    const lacking = ordQ.filter((q) => !h[q]);
    return {
      Country: c,
      n_components: nComp,
      eligible,
      basis: eligible ? policyBasisLabel(ordQ.filter((q) => h[q]).map((q) => use[q])) : null,
      missing: lacking.length ? lacking.map((q) => use[q]).join('; ') : null,
      missingRequired: eligible ? null : lacking.filter((q) => reqSet.has(q)).map((q) => use[q]).join('; '),
    };
  });
  // order(!eligible, Country, method = "radix")
  elig.sort((a, b) => (a.eligible === b.eligible ? cmpC(a.Country, b.Country) : a.eligible ? -1 : 1));
  const eligibleSet = new Set(elig.filter((e) => e.eligible).map((e) => e.Country));
  const basisOf = new Map(elig.map((e) => [e.Country, e.basis]));
  const eligibility = table({
    Country: elig.map((e) => e.Country),
    n_components: Int32Array.from(elig.map((e) => e.n_components)),
    eligible: Uint8Array.from(elig.map((e) => (e.eligible ? 1 : 0))),
    policy_basis: elig.map((e) => e.basis),
    missing_components: elig.map((e) => e.missing),
    missing_required: elig.map((e) => e.missingRequired),
  });

  // value = sum_v w_v x_v / sum_v w_v over observed components (long-double row sums)
  const accN = new LdAcc();
  const accD = new LdAcc();
  const keep: number[] = [];
  const vals: number[] = [];
  const ncomp: number[] = [];
  for (let i = 0; i < n; i++) {
    accN.reset();
    accD.reset();
    let k = 0;
    for (let q = 0; q < useIdx.length; q++) {
      const x = V[i + nr * useIdx[q]];
      const wq = w[use[q]];
      const p = x * wq;
      if (p === p) accN.add(p);
      if (x === x) {
        accD.add(wq);
        k++;
      } else accD.add(0);
    }
    const den = accD.value();
    const value = den === 0 ? NaN : accN.value() / den;
    if (eligibleSet.has(country[i] as string) && value === value) {
      keep.push(i);
      vals.push(value);
      ncomp.push(k);
    }
  }
  const tech = str(pb.cells, 'tech');
  const sc = str(pb.cells, 'supply_chain');
  const index = table({
    Country: keep.map((i) => country[i]) as Str,
    tech: keep.map((i) => tech[i]) as Str,
    supply_chain: keep.map((i) => sc[i]) as Str,
    value: Float64Array.from(vals),
    n_components: Int32Array.from(ncomp),
    policy_basis: keep.map((i) => basisOf.get(country[i] as string) ?? null),
  });
  const weights: NumRecord = {};
  for (const v of use) weights[v] = w[v];
  return { index, eligibility, weights };
}
