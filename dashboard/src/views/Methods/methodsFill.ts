// Fill the Methods placeholders from the data build (port of shiny mod_methods.R methods_fill()),
// and render Markdown with TeX math: math is cut out before Markdown and rendered with KaTeX after,
// so Markdown never touches it. Pure (the renderers are passed in).

import { toRows } from '../../engine/table.ts';
import type { BaselineInputs, Manifest, Table } from '../../engine/types.ts';
import { DASH } from '../../lib/format.ts';

const fmtw = (x: unknown): string => {
  if (typeof x !== 'number' || !Number.isFinite(x)) return DASH;
  return String(Number(x.toPrecision(3)));
};

const escapeMd = (s: string): string => s.replace(/[\\`*_{}[\]<>|]/g, (c) => `\\${c}`);

/** What the build facts are computed from: the countries (tier, network node), the network nodes and
 * the shipped baseline tables (policy eligibility, Strategic Index cells, allied picks). */
export interface BuildFactsInput {
  countries: { tier: string; network_node: string | null }[];
  nNodes: number;
  eligibility: Table | null | undefined;
  siCells: Table | null | undefined;
  allied: Table | null | undefined;
}

/** Counts of this data build for the Methods text: coalition tiers and network nodes, Policy index
 * bases, the endowment screen's flags on scored cells and the allied picks. */
export function buildFacts(x: BuildFactsInput): Record<string, string> {
  const n = (v: number) => String(v);
  const core = x.countries.filter((c) => c.tier === 'core').length;
  const partner = x.countries.filter((c) => c.tier === 'partner').length;
  const el = x.eligibility ? toRows(x.eligibility) : [];
  const scored = el.filter((r) => r.eligible === true);
  const byBasis = (b: string) => scored.filter((r) => r.policy_basis === b).length;
  const cells = x.siCells ? toRows(x.siCells).filter((r) => typeof r.strategic_index === 'number') : [];
  const screened = cells.filter((r) => typeof r.resource_base === 'boolean');
  const allied = x.allied ? toRows(x.allied) : [];
  const members = new Set(allied.map((r) => r.Country));
  const upstream = new Set(allied.filter((r) => r.supply_chain === 'Upstream').map((r) => r.Country));
  return {
    n_coalition: n(core + partner),
    n_core: n(core),
    n_partners: n(partner),
    n_nodes: n(x.nNodes),
    n_policy: n(scored.length),
    n_policy_both: n(byBasis('IEA PAMS + CAT')),
    n_policy_pams_only: n(byBasis('IEA PAMS only')),
    n_policy_none: n(el.length - scored.length),
    n_si_cells: n(cells.length),
    n_si_countries: n(new Set(cells.map((r) => r.Country)).size),
    n_screened: n(screened.length),
    n_screened_out: n(screened.filter((r) => r.resource_base === false).length),
    n_picked: n(members.size),
    n_picked_upstream: n(upstream.size),
  };
}

export function methodsValues(bi: BaselineInputs, manifest: Pick<Manifest, 'flags'>, facts: Record<string, string> = {}): Record<string, string> {
  const pw = bi.policy_weights ?? {};
  const pick = (re: RegExp) => {
    const ks = Object.keys(pw).filter((k) => re.test(k));
    return ks.length ? ks.reduce((a, k) => a + pw[k], 0) : NaN;
  };
  const lam = bi.lambda;
  const geo = bi.fs_weights?.geo_alignment;
  const geoIn = manifest.flags?.geo_alignment_in_fs === true;
  return {
    geo_weight: geo === undefined || geo === null ? 'not in this build' : geoIn ? fmtw(geo) : `${fmtw(geo)} (no effect: this build's Friendshore has no alignment component)`,
    lambda0: fmtw(lam),
    lambda_lo: fmtw(lam * 0.25),
    lambda_hi: fmtw(lam * 4),
    iea_weight: fmtw(pick(/iea/i)),
    cat_weight: fmtw(pick(/(^|[^a-z])cat([^a-z]|$)|climate action/i)),
    fs_penalty: fmtw(bi.fs_penalty),
    po_penalty: fmtw(bi.po_penalty),
    policy_weights: Object.keys(pw).length ? Object.entries(pw).map(([k, v]) => `${escapeMd(k)} ${fmtw(v)}`).join(', ') : DASH,
    tau_text: bi.tau === null || bi.tau === undefined ? 'rule off (the stored pillars predate it)' : fmtw(bi.tau),
    psi_view: bi.psi_view === 'world' ? 'All reporters (world), as the stored partner-level PSI' : 'Coalition reporters',
    max_share: fmtw(bi.max_share),
    min_producers: fmtw(bi.min_producers),
    min_share: fmtw(bi.min_share),
    eps_share: typeof bi.epsilon_supplier_share === 'number' ? `${Math.round(100 * bi.epsilon_supplier_share)}%` : DASH,
    screen_default: bi.require_resource_base === false ? 'off (this build)' : 'on',
    // build counts (buildFacts()); a placeholder without a value reads as a dash
    ...Object.fromEntries(
      ['n_coalition', 'n_core', 'n_partners', 'n_nodes', 'n_policy', 'n_policy_both', 'n_policy_pams_only', 'n_policy_none', 'n_si_cells',
        'n_si_countries', 'n_screened', 'n_screened_out', 'n_picked', 'n_picked_upstream'].map((k) => [k, DASH]),
    ),
    ...facts,
  };
}

export function fillPlaceholders(md: string, values: Record<string, string>): string {
  return md.replace(/\{\{(\w+)\}\}/g, (m, k: string) => values[k] ?? m);
}

/** Markdown + math: `$$...$$` display and `$...$` inline math rendered by `tex`, the rest by `markdown`. */
export function renderMath(md: string, markdown: (s: string) => string, tex: (s: string, display: boolean) => string): string {
  const parts: string[] = [];
  const keep = (html: string) => {
    parts.push(html);
    return `MATHTOKEN${parts.length - 1}X`;
  };
  let s = md.replace(/\$\$([\s\S]+?)\$\$/g, (_, t: string) => keep(tex(t.trim(), true)));
  s = s.replace(/(^|[^\\$])\$([^$\n]+?)\$/g, (_, pre: string, t: string) => pre + keep(tex(t, false)));
  const html = markdown(s);
  return html.replace(/MATHTOKEN(\d+)X/g, (_, i: string) => parts[Number(i)]);
}
