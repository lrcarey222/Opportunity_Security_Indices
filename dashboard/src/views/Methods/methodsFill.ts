// Fill the Methods placeholders from the data build (port of shiny mod_methods.R methods_fill()),
// and render Markdown with TeX math: math is cut out before Markdown and rendered with KaTeX after,
// so Markdown never touches it. Pure (the renderers are passed in).

import { toRows } from '../../engine/table.ts';
import type { BaselineInputs, Manifest, Table } from '../../engine/types.ts';
import { DASH } from '../../lib/format.ts';
import { byStageRank, stageRank } from '../../lib/stages.ts';

const fmtw = (x: unknown): string => {
  if (typeof x !== 'number' || !Number.isFinite(x)) return DASH;
  return String(Number(x.toPrecision(3)));
};

const escapeMd = (s: string): string => s.replace(/[\\`*_{}[\]<>|]/g, (c) => `\\${c}`);

/** What the build facts are computed from: the countries (tier) and the shipped baseline tables
 * (policy eligibility, Strategic Index cells, allied picks). */
export interface BuildFactsInput {
  countries: { tier: string }[];
  eligibility: Table | null | undefined;
  siCells: Table | null | undefined;
  allied: Table | null | undefined;
}

/** Counts of this data build for the Methods text: coalition tiers, Policy index
 * bases, the endowment screen's flags on scored cells and the allied picks (the members with a pick,
 * and those with a pick at Extraction, the stage the endowment screen applies to). */
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
  const extraction = new Set(allied.filter((r) => r.supply_chain === 'Extraction').map((r) => r.Country));
  return {
    n_coalition: n(core + partner),
    n_core: n(core),
    n_partners: n(partner),
    n_policy: n(scored.length),
    n_policy_both: n(byBasis('IEA PAMS + CAT')),
    n_policy_pams_only: n(byBasis('IEA PAMS only')),
    n_policy_none: n(el.length - scored.length),
    n_si_cells: n(cells.length),
    n_si_countries: n(new Set(cells.map((r) => r.Country)).size),
    n_screened: n(screened.length),
    n_screened_out: n(screened.filter((r) => r.resource_base === false).length),
    n_picked: n(members.size),
    n_picked_extraction: n(extraction.size),
  };
}

const listAnd = (xs: readonly string[]): string => (xs.length <= 1 ? xs.join('') : `${xs.slice(0, -1).join(', ')} and ${xs[xs.length - 1]}`);

/**
 * Stage weights in words, stages of equal (formatted) weight together, in chain order of their first
 * stage: "Extraction, Refining and Recovery 0.167; Components and Assembly 0.375; Deployment 0.25".
 */
export function stageWeightsText(sw: Readonly<Record<string, number>> | null | undefined, f: (w: number) => number = (w) => w): string {
  const stages = Object.keys(sw ?? {}).filter((s) => Number.isFinite((sw as Record<string, number>)[s])).sort(byStageRank);
  if (!stages.length) return DASH;
  const groups = new Map<string, string[]>();
  for (const s of stages) {
    const k = fmtw(f((sw as Record<string, number>)[s]));
    groups.set(k, [...(groups.get(k) ?? []), s]);
  }
  return [...groups.entries()].map(([w, ss]) => `${listAnd(ss)} ${w}`).join('; ');
}

/** The populism stage transform at a dial setting d: lambda_max (w / lambda_max)^m(d), m(d) = 4^((d - 50) / 50). */
const populismStage = (sw: Readonly<Record<string, number>>, d: number) => {
  const lmax = Math.max(...Object.values(sw));
  const m = Math.pow(4, (d - 50) / 50);
  return (w: number) => lmax * Math.pow(w / lmax, m);
};

/**
 * The excluded sectors ("Tech - Stage") in words, technologies with the same stages together:
 * "Geothermal (Extraction, Refining, Components, Assembly and Recovery); Coal and Gas (Recovery)".
 */
export function excludedSectorsText(xs: readonly string[] | null | undefined): string {
  const byTech = new Map<string, string[]>();
  for (const x of xs ?? []) {
    const i = x.lastIndexOf(' - ');
    if (i < 0) continue;
    const tech = x.slice(0, i);
    byTech.set(tech, [...(byTech.get(tech) ?? []), x.slice(i + 3)]);
  }
  if (!byTech.size) return DASH;
  // technologies (alphabetical) with the same stages (chain order) together; the groups with more stages first
  const groups = new Map<string, { stages: string[]; techs: string[] }>();
  for (const tech of [...byTech.keys()].sort((a, b) => a.localeCompare(b, 'en'))) {
    const stages = [...new Set(byTech.get(tech))].sort(byStageRank);
    const g = groups.get(stages.join('|')) ?? { stages, techs: [] };
    g.techs.push(tech);
    groups.set(stages.join('|'), g);
  }
  const order = [...groups.values()].sort((a, b) => b.stages.length - a.stages.length || stageRank(a.stages[0]) - stageRank(b.stages[0]));
  return order.map((g) => `${listAnd(g.techs)} (${listAnd(g.stages)})`).join('; ');
}

export function methodsValues(bi: BaselineInputs, manifest: Pick<Manifest, 'flags'>, facts: Record<string, string> = {}): Record<string, string> {
  const pw = bi.policy_weights ?? {};
  const pick = (re: RegExp) => {
    const ks = Object.keys(pw).filter((k) => re.test(k));
    return ks.length ? ks.reduce((a, k) => a + pw[k], 0) : NaN;
  };
  const geo = bi.fs_weights?.geo_alignment;
  const geoIn = manifest.flags?.geo_alignment_in_fs === true;
  // the CM-FDI ownership component of Friendshore (scaled by hawkishness like alignment)
  const cpe = bi.fs_weights?.chinese_parent_exposure;
  const cpeIn = typeof cpe === 'number' && Number.isFinite(cpe) && cpe > 0 && manifest.flags?.fs_cmfdi !== false;
  return {
    geo_weight: geo === undefined || geo === null ? 'not in this build' : geoIn ? fmtw(geo) : `${fmtw(geo)} (no effect: this build's Friendshore has no alignment component)`,
    cpe_weight: cpeIn ? fmtw(cpe) : 'not in this build',
    cpe_lo: cpeIn ? fmtw((cpe as number) * 0.25) : DASH,
    cpe_hi: cpeIn ? fmtw((cpe as number) * 4) : DASH,
    iea_weight: fmtw(pick(/iea/i)),
    cat_weight: fmtw(pick(/(^|[^a-z])cat([^a-z]|$)|climate action/i)),
    fs_penalty: fmtw(bi.fs_penalty),
    po_penalty: fmtw(bi.po_penalty),
    policy_weights: Object.keys(pw).length ? Object.entries(pw).map(([k, v]) => `${escapeMd(k)} ${fmtw(v)}`).join(', ') : DASH,
    tau_text: bi.tau === null || bi.tau === undefined ? 'rule off (the stored pillars predate it)' : fmtw(bi.tau),
    psi_view: bi.psi_view === 'world' ? 'All reporters (world), as the stored partner-level PSI' : 'Coalition reporters',
    screen_default: bi.require_resource_base === false ? 'off (this build)' : 'on',
    // the Strategic Index stage term and the populism transform of it (the build's six-stage weights)
    stage_weights: stageWeightsText(bi.stage_weights),
    stage_lambda_max: bi.stage_weights && Object.keys(bi.stage_weights).length ? fmtw(Math.max(...Object.values(bi.stage_weights))) : DASH,
    stage_weights_lo: bi.stage_weights ? stageWeightsText(bi.stage_weights, populismStage(bi.stage_weights, 0)) : DASH,
    stage_weights_hi: bi.stage_weights ? stageWeightsText(bi.stage_weights, populismStage(bi.stage_weights, 100)) : DASH,
    n_excluded: Array.isArray(bi.excluded_sectors) ? String(bi.excluded_sectors.length) : DASH,
    excluded_sectors: excludedSectorsText(bi.excluded_sectors),
    // build counts (buildFacts()); a placeholder without a value reads as a dash
    ...Object.fromEntries(
      ['n_coalition', 'n_core', 'n_partners', 'n_policy', 'n_policy_both', 'n_policy_pams_only', 'n_policy_none', 'n_si_cells',
        'n_si_countries', 'n_screened', 'n_screened_out', 'n_picked', 'n_picked_extraction'].map((k) => [k, DASH]),
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
