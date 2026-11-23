// The outputs of a shipped-bundle QC run (qc/run.ts): the CSV rows the Node CLI writes to qc/results/
// (bundle_qc_checks, choice_reproduction, qc_sensitivity_bundle, robustness_bundle_summary,
// qc_golden_case_status) and the two JSON documents the review data build ships for the admin dashboard
// (public/data/review/robustness.json: a compact class map of every posture cell and AIB node;
// qc_summary.json: check rows, reproduction, robustness and golden status). Every row carries the data release
// it describes and the layer ('bundle'), so the pipeline layer's CSVs (R, layer 'pipeline') can sit beside them.
// CSV text is written with LF line ends. Pure: no IO.

import { SUPPLY_STAGES } from '../../lib/stages.ts';
import type { RobustnessClass, RobustnessRecord } from '../model.ts';
import type { PostureRobustness } from './sensitivity.ts';
import { CLASS_CODE, SPECS_NOT_RUNNABLE_ON_BUNDLE } from './sensitivity.ts';
import type { ShippedQcRun } from './run.ts';

export type CsvRow = Record<string, string | number | boolean | null | undefined>;

const quote = (s: string): string => (/[",\r\n]/.test(s) || /^\s|\s$/.test(s) ? `"${s.replace(/"/g, '""')}"` : s);
const fmt = (v: unknown): string =>
  v === null || v === undefined || (typeof v === 'number' && !Number.isFinite(v)) ? '' : typeof v === 'boolean' ? (v ? 'TRUE' : 'FALSE') : typeof v === 'number' ? String(v) : quote(String(v));

/** CSV text (header + rows, LF line ends, RFC 4180 quoting). */
export function toCsv(rows: readonly CsvRow[], cols: readonly string[]): string {
  return [cols.map(quote).join(','), ...rows.map((r) => cols.map((c) => fmt(r[c])).join(','))].join('\n') + '\n';
}

// ---- bundle_qc_checks.csv ----------------------------------------------------------------------------------------

export const BUNDLE_QC_COLS = ['data_release', 'layer', 'check_id', 'area', 'status', 'n', 'detail', 'examples'] as const;
export function bundleQcRows(run: ShippedQcRun): CsvRow[] {
  return run.rows.map((r) => ({ data_release: run.releaseId, layer: r.layer, check_id: r.check_id, area: r.area, status: r.status, n: r.n, detail: r.detail, examples: r.examples.join(' | ') }));
}

// ---- choice_reproduction.csv ---------------------------------------------------------------------------------------

export const REPRODUCTION_COLS = ['data_release', 'layer', 'row_type', 'iso3', 'tech', 'stage', 'path', 'shipped', 'recomputed', 'recomputed_from_trace', 'category', 'reason'] as const;
/** The non-reproduced cells, then one summary row (reproduced / cells and the rate, by path). */
export function reproductionRows(run: ShippedQcRun): CsvRow[] {
  const rep = run.reproduction;
  if (!rep) return [{ data_release: run.releaseId, layer: 'bundle', row_type: 'summary', category: 'not_run', reason: 'no posture bundle' }];
  const s = rep.summary;
  const out: CsvRow[] = rep.rows
    .filter((r) => !r.reproduced)
    .map((r) => ({
      data_release: run.releaseId, layer: 'bundle', row_type: 'exception', iso3: r.iso3, tech: r.tech, stage: r.stage, path: r.path, shipped: r.shipped,
      recomputed: r.recomputed, recomputed_from_trace: r.recomputed_from_trace, category: r.category, reason: r.reason,
    }));
  out.push({
    data_release: run.releaseId, layer: 'bundle', row_type: 'summary', iso3: 'ALL', category: 'summary',
    reason: `reproduced ${s.reproduced} of ${s.cells} (${(s.rate * 100).toFixed(2)}%); by path ${Object.entries(s.by_path).map(([k, v]) => `${k} ${v.reproduced}/${v.cells}`).join(', ')}; by category ${Object.entries(s.by_category).map(([k, v]) => `${k} ${v}`).join(', ')}. Secure and the large-yard case are read from the trace; cp_high reads the trace's position word (the capability floor is not shipped).`,
  });
  return out;
}

// ---- qc_sensitivity_bundle.csv -------------------------------------------------------------------------------------

export const SENSITIVITY_COLS = [
  'data_release', 'layer', 'subject_kind', 'iso3', 'tech', 'stage', 'node', 'scenario', 'baseline_choice', 'row_type', 'spec_id', 'spec_kind', 'spec_label', 'alt_choice', 'fence_changed', 'class', 'drivers', 'note',
] as const;

function recordRows(run: ShippedQcRun, r: RobustnessRecord & { indeterminate?: PostureRobustness['indeterminate'] }, kind: 'posture_cell' | 'aib_node', labels: Record<string, string>): CsvRow[] {
  const key = { iso3: r.case_key.iso3 ?? null, tech: r.case_key.tech ?? null, stage: r.case_key.stage ?? null, node: r.case_key.node ?? null, scenario: r.case_key.scenario ?? null };
  const base = { data_release: run.releaseId, layer: 'bundle', subject_kind: kind, ...key, baseline_choice: r.baseline_choice };
  const out: CsvRow[] = [{ ...base, row_type: 'class', class: r.class, fence_changed: r.fence_change, drivers: r.drivers.join(';'), note: r.class === 'stable' ? null : (r.note ?? null) }];
  for (const f of [...r.local_flips, ...r.structural_flips])
    out.push({ ...base, row_type: 'flip', spec_id: f.spec_id, spec_kind: f.kind, spec_label: f.label, alt_choice: f.choice, fence_changed: f.fence_changed, class: r.class });
  for (const x of r.indeterminate ?? [])
    out.push({ ...base, row_type: 'possible', spec_id: x.spec_id, spec_kind: x.kind, spec_label: labels[x.spec_id] ?? x.spec_id, class: r.class, note: `undecidable on the bundle: ${x.missing.join('; ')}` });
  return out;
}

/**
 * Per cell / node: one class row, then one row per spec that changes the choice (flip) or may (possible).
 * `compact` (the committed qc/results/qc_sensitivity_bundle.csv, kept under ~1 MB): only the cell x spec flip and
 * possible-flip rows and the class rows of NON-stable cells and nodes; a not-testable posture cell's class row
 * drops its note (it is not assessed or Secure, as its baseline_choice says, and the reason is counted in
 * robustness_bundle_summary.csv). The full per-cell classes stay in public/data/review/robustness.json.
 */
export function sensitivityRows(run: ShippedQcRun, opts: { compact?: boolean } = {}): CsvRow[] {
  const labels = Object.fromEntries((run.posture?.specs ?? []).map((s) => [s.spec_id, s.label]));
  const rows = [...(run.posture?.records ?? []).flatMap((r) => recordRows(run, r, 'posture_cell', labels)), ...run.aib.flatMap((r) => recordRows(run, r, 'aib_node', {}))];
  if (!opts.compact) return rows;
  return rows
    .filter((r) => r.row_type !== 'class' || r.class !== 'stable')
    .map((r) => (r.row_type === 'class' && r.class === 'not_testable' && r.subject_kind === 'posture_cell' ? { ...r, note: null } : r));
}

// ---- robustness_bundle_summary.csv ---------------------------------------------------------------------------------

export const ROBUSTNESS_SUMMARY_COLS = ['data_release', 'layer', 'subject_kind', 'dimension', 'key', 'n', 'stable', 'sensitive', 'fragile', 'not_testable', 'fence_change', 'flips', 'possible', 'note'] as const;

function tally(records: readonly RobustnessRecord[]): Pick<CsvRow, 'n' | 'stable' | 'sensitive' | 'fragile' | 'not_testable' | 'fence_change'> {
  const c: Record<RobustnessClass, number> = { stable: 0, sensitive: 0, fragile: 0, not_testable: 0 };
  for (const r of records) c[r.class]++;
  return { n: records.length, ...c, fence_change: records.filter((r) => r.fence_change).length };
}

/** Class counts overall and by stage, technology and shipped choice; flips and possible flips by spec and by driver. */
export function robustnessSummaryRows(run: ShippedQcRun): CsvRow[] {
  const out: CsvRow[] = [];
  const base = { data_release: run.releaseId, layer: 'bundle' };
  const groups = (kind: 'posture_cell' | 'aib_node', recs: readonly RobustnessRecord[], dims: [string, (r: RobustnessRecord) => string][]) => {
    out.push({ ...base, subject_kind: kind, dimension: 'all', key: 'all', ...tally(recs) });
    for (const [dim, f] of dims) {
      const by = new Map<string, RobustnessRecord[]>();
      for (const r of recs) (by.get(f(r)) ?? by.set(f(r), []).get(f(r))!).push(r);
      for (const k of [...by.keys()].sort()) out.push({ ...base, subject_kind: kind, dimension: dim, key: k, ...tally(by.get(k)!) });
    }
  };
  const post = run.posture?.records ?? [];
  if (run.posture) {
    groups('posture_cell', post, [['stage', (r) => r.case_key.stage ?? ''], ['tech', (r) => r.case_key.tech ?? ''], ['baseline_choice', (r) => r.baseline_choice]]);
    const s = run.postureSummary!;
    for (const sp of run.posture.specs)
      out.push({ ...base, subject_kind: 'posture_cell', dimension: 'spec', key: sp.spec_id, flips: s.by_spec[sp.spec_id] ?? 0, possible: s.indeterminate_by_spec[sp.spec_id] ?? 0, note: `${sp.kind}: ${sp.label}` });
    for (const [d, v] of Object.entries(s.by_driver).sort()) out.push({ ...base, subject_kind: 'posture_cell', dimension: 'driver', key: d, flips: v });
    for (const why of SPECS_NOT_RUNNABLE_ON_BUNDLE) out.push({ ...base, subject_kind: 'posture_cell', dimension: 'not_runnable', key: why.split(' (')[0], note: why });
  }
  if (run.aibSummary) {
    groups('aib_node', run.aib, [['baseline_choice', (r) => r.baseline_choice]]);
    for (const [k, v] of Object.entries(run.aibSummary.by_spec).sort()) out.push({ ...base, subject_kind: 'aib_node', dimension: 'spec', key: k, flips: v });
  }
  return out;
}

// ---- qc_golden_case_status.csv -------------------------------------------------------------------------------------

export const GOLDEN_COLS = ['data_release', 'layer', 'id', 'category', 'kind', 'key', 'status', 'exact', 'change_requires_review', 'expected', 'observed', 'failures'] as const;
export function goldenRows(run: ShippedQcRun): CsvRow[] {
  const g = run.golden;
  if (!g) return [];
  const byId = new Map(g.cases.map((c) => [c.id, c]));
  return g.results.map((r) => {
    const c = byId.get(r.id);
    return {
      data_release: run.releaseId, layer: 'bundle', id: r.id, category: c?.category ?? null, kind: c?.kind ?? null,
      key: c ? Object.entries(c.key).map(([k, v]) => `${k}=${v}`).join(';') : null, status: r.status, exact: c?.exact ?? null,
      change_requires_review: c?.change_requires_review ?? null, expected: c ? JSON.stringify(c.expect) : null, observed: JSON.stringify(r.observed), failures: r.failures.join('; '),
    };
  });
}

// ---- public/data/review/robustness.json ----------------------------------------------------------------------------

export const ROBUSTNESS_SCHEMA = 'aib-review-robustness';

export interface RobustnessMap {
  schema: typeof ROBUSTNESS_SCHEMA;
  schema_version: 1;
  data_release: string | null;
  layer: 'bundle';
  /** one-letter code -> class */
  codes: Record<string, RobustnessClass>;
  posture: {
    techs: string[];
    stages: string[];
    /** ISO3 -> one code per cell, technology-major: index = techIndex * stages.length + stageIndex; '-' = no record */
    cells: Record<string, string>;
    /** "ISO|Tech|Stage" of the cells whose flips change the fence */
    fence_change: string[];
    summary: { by_class: Record<RobustnessClass, number>; fence_change: number };
  } | null;
  aib: { scenario: 'decoupling'; nodes: Record<string, string>; fence_change: string[]; summary: { by_class: Record<RobustnessClass, number>; fence_change: number } } | null;
  not_runnable_on_bundle: readonly string[];
}

/** The compact class map of every posture cell and AIB node (the reveal screens and the Robustness view read it). */
export function robustnessMap(run: ShippedQcRun, techs: readonly string[]): RobustnessMap {
  const codes = Object.fromEntries(Object.entries(CLASS_CODE).map(([cls, code]) => [code, cls as RobustnessClass]));
  let posture: RobustnessMap['posture'] = null;
  if (run.posture && run.postureSummary) {
    const stages = [...SUPPLY_STAGES];
    const cells: Record<string, string[]> = {};
    const fence: string[] = [];
    for (const r of run.posture.records) {
      const { iso3, tech, stage } = r.case_key;
      if (!iso3 || !tech || !stage) continue;
      const ti = techs.indexOf(tech);
      const si = stages.indexOf(stage as (typeof stages)[number]);
      if (ti < 0 || si < 0) continue;
      (cells[iso3] ??= new Array<string>(techs.length * stages.length).fill('-'))[ti * stages.length + si] = CLASS_CODE[r.class];
      if (r.fence_change) fence.push(`${iso3}|${tech}|${stage}`);
    }
    posture = {
      techs: [...techs], stages,
      cells: Object.fromEntries(Object.keys(cells).sort().map((k) => [k, cells[k].join('')])),
      fence_change: fence.sort(),
      summary: { by_class: run.postureSummary.by_class, fence_change: run.postureSummary.fence_change },
    };
  }
  const aib: RobustnessMap['aib'] = run.aibSummary
    ? {
        scenario: 'decoupling',
        nodes: Object.fromEntries(run.aib.map((r) => [r.case_key.node ?? '', CLASS_CODE[r.class]]).sort(([a], [b]) => (a < b ? -1 : a > b ? 1 : 0))),
        fence_change: run.aib.filter((r) => r.fence_change).map((r) => r.case_key.node ?? '').sort(),
        summary: { by_class: run.aibSummary.by_class, fence_change: run.aibSummary.fence_change },
      }
    : null;
  return { schema: ROBUSTNESS_SCHEMA, schema_version: 1, data_release: run.releaseId, layer: 'bundle', codes, posture, aib, not_runnable_on_bundle: SPECS_NOT_RUNNABLE_ON_BUNDLE };
}

// ---- public/data/review/qc_summary.json ----------------------------------------------------------------------------

export const QC_SUMMARY_SCHEMA = 'aib-review-qc-summary';

/** The admin dashboard's Data QC and Robustness views: check rows, reproduction, robustness, golden status. */
export function qcSummaryDoc(run: ShippedQcRun, stamp: { built_at: string; app_commit: string }, maxExceptions = 50) {
  const rep = run.reproduction;
  return {
    schema: QC_SUMMARY_SCHEMA,
    schema_version: 1 as const,
    data_release: run.releaseId,
    built_at: stamp.built_at,
    app_commit: stamp.app_commit,
    layer: 'bundle' as const,
    status: run.status,
    checks: run.rows,
    reproduction: rep
      ? { ...rep.summary, exceptions: rep.rows.filter((r) => !r.reproduced).slice(0, maxExceptions), exceptions_total: rep.rows.filter((r) => !r.reproduced).length }
      : null,
    robustness: {
      posture: run.postureSummary,
      aib: run.aibSummary,
      specs: (run.posture?.specs ?? []).map((s) => ({ spec_id: s.spec_id, kind: s.kind, label: s.label, driver: s.driver })),
      not_runnable_on_bundle: SPECS_NOT_RUNNABLE_ON_BUNDLE,
    },
    golden: run.golden ? { summary: run.golden.summary, results: run.golden.results } : null,
  };
}
