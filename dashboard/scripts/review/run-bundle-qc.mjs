#!/usr/bin/env node
// The shipped-bundle QC run (docs/QC_SYSTEM_DESIGN.md 6.1, 6.3): reads the bundles the Explorer serves
// (dashboard/public/data), runs the bundle checks, the choice-rule reproduction, the per-cell and per-node
// robustness classification and the golden cases, and writes qc/results/:
//   bundle_qc_checks.csv          one row per check {check_id, layer bundle, area, status, n, detail, examples}
//   choice_reproduction.csv       the non-reproduced cells (every one with its reason) + a summary row
//   qc_sensitivity_bundle.csv     a row per cell x spec that changes the choice (flip) or may (possible: the
//                                 bundle cannot decide), and a class row per NON-stable posture cell / AIB node
//                                 (stable cells are left out to keep the file small; every cell's class is in
//                                 public/data/review/robustness.json and the counts in robustness_bundle_summary.csv)
//   robustness_bundle_summary.csv class counts overall / by stage / tech / choice; flips by spec and driver
//   qc_golden_case_status.csv     config/qc/golden_cases.yml evaluated on the bundles
// IO only: the logic is src/review/qc/*.ts, imported as TypeScript (Node >= 24 strips the types).
// Exit code 1 when any check fails (golden failures included), 2 on a usage or read error, else 0.
//
//   node scripts/review/run-bundle-qc.mjs [--data-dir <dashboard/public/data>] [--out <qc/results>]
//        [--golden <config/qc/golden_cases.yml> | --no-golden] [--quiet]

import { existsSync, mkdirSync, readFileSync, writeFileSync } from 'node:fs';
import { dirname, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { parse } from 'yaml';

import { validateGoldenCases } from '../../src/review/manifest.ts';
import { dataReleaseId } from '../../src/review/release.ts';
import { parseDefaultThresholds } from '../../src/review/qc/bundleQc.ts';
import { decodeBundles } from '../../src/review/qc/bundles.ts';
import { loadRawBundles } from '../../src/review/qc/node/loadBundles.ts';
import {
  BUNDLE_QC_COLS, bundleQcRows, GOLDEN_COLS, goldenRows, REPRODUCTION_COLS, reproductionRows, ROBUSTNESS_SUMMARY_COLS, robustnessSummaryRows,
  SENSITIVITY_COLS, sensitivityRows, toCsv,
} from '../../src/review/qc/reports.ts';
import { qcFailed, runShippedQc } from '../../src/review/qc/run.ts';

const HERE = dirname(fileURLToPath(import.meta.url));
const DASHBOARD = resolve(HERE, '..', '..');
const REPO = resolve(DASHBOARD, '..');
const USAGE = 'usage: node scripts/review/run-bundle-qc.mjs [--data-dir <dir>] [--out <dir>] [--golden <file> | --no-golden] [--quiet]';

/** Parse argv into options (defaults relative to this script's checkout); throws with the usage on a bad command line. */
export function parseArgs(argv) {
  const o = { dataDir: resolve(DASHBOARD, 'public', 'data'), out: resolve(REPO, 'qc', 'results'), golden: resolve(REPO, 'config', 'qc', 'golden_cases.yml'), quiet: false };
  for (let i = 0; i < argv.length; i++) {
    const a = argv[i];
    const value = () => {
      const v = argv[i + 1];
      if (v === undefined || v.startsWith('--')) throw new Error(`${a} needs a value\n${USAGE}`);
      i++;
      return v;
    };
    if (a === '--data-dir') o.dataDir = resolve(value());
    else if (a === '--out') o.out = resolve(value());
    else if (a === '--golden') o.golden = resolve(value());
    else if (a === '--no-golden') o.golden = null;
    else if (a === '--quiet') o.quiet = true;
    else if (a === '--help' || a === '-h') throw new Error(USAGE);
    else throw new Error(`unknown argument ${a}\n${USAGE}`);
  }
  return o;
}

/** Read and validate the golden cases; null when no file is used. */
export function readGolden(path) {
  if (!path) return null;
  if (!existsSync(path)) return { cases: [], errors: [`golden file not found: ${path}`] };
  let doc;
  try {
    doc = parse(readFileSync(path, 'utf8'));
  } catch (e) {
    return { cases: [], errors: [`golden file is not valid YAML: ${e instanceof Error ? e.message : String(e)}`] };
  }
  const v = validateGoldenCases(doc);
  const list = doc && typeof doc === 'object' && Array.isArray(doc.cases) ? doc.cases : Array.isArray(doc) ? doc : [];
  return { cases: v.errors.length ? [] : list, errors: v.errors };
}

/** Load, decode and run the whole shipped-bundle layer. */
export async function runQc(o) {
  if (!existsSync(resolve(o.dataDir, 'posture', 'manifest.json'))) throw new Error(`no posture bundle under ${o.dataDir}`);
  const load = loadRawBundles(o.dataDir);
  const decoded = decodeBundles(load.raw);
  const releaseId = await dataReleaseId(decoded.stamps);
  const contract = resolve(DASHBOARD, 'src', 'posture', 'contract.ts');
  const fallbackThresholds = existsSync(contract) ? parseDefaultThresholds(readFileSync(contract, 'utf8')) : null;
  const run = runShippedQc({ decoded, raw: load.raw, loadProblems: load.problems, fallbackThresholds, releaseId, golden: readGolden(o.golden) });
  return { load, decoded, run };
}

/** Write the five CSVs; returns their paths. */
export function writeResults(run, out) {
  mkdirSync(out, { recursive: true });
  const files = [
    ['bundle_qc_checks.csv', toCsv(bundleQcRows(run), BUNDLE_QC_COLS)],
    ['choice_reproduction.csv', toCsv(reproductionRows(run), REPRODUCTION_COLS)],
    // compact: flips + non-stable class rows only (the full per-cell classes are in public/data/review/robustness.json)
    ['qc_sensitivity_bundle.csv', toCsv(sensitivityRows(run, { compact: true }), SENSITIVITY_COLS)],
    ['robustness_bundle_summary.csv', toCsv(robustnessSummaryRows(run), ROBUSTNESS_SUMMARY_COLS)],
    ['qc_golden_case_status.csv', toCsv(goldenRows(run), GOLDEN_COLS)],
  ];
  for (const [name, text] of files) writeFileSync(resolve(out, name), text);
  return files.map(([name]) => resolve(out, name));
}

/** The console summary. */
export function summaryText(run) {
  const lines = [`data release ${run.releaseId}: checks pass ${run.status.pass}, warn ${run.status.warn}, fail ${run.status.fail}, info ${run.status.info}`];
  const rep = run.reproduction?.summary;
  if (rep) lines.push(`choice-rule reproduction: ${rep.reproduced} of ${rep.cells} (${(rep.rate * 100).toFixed(2)}%)`);
  const ps = run.postureSummary;
  if (ps) lines.push(`posture robustness: stable ${ps.by_class.stable}, sensitive ${ps.by_class.sensitive}, fragile ${ps.by_class.fragile}, not testable ${ps.by_class.not_testable}, fence change ${ps.fence_change}`);
  const as = run.aibSummary;
  if (as) lines.push(`AIB node robustness: stable ${as.by_class.stable}, sensitive ${as.by_class.sensitive}, fragile ${as.by_class.fragile}, not testable ${as.by_class.not_testable}`);
  if (run.golden) lines.push(`golden cases: pass ${run.golden.summary.pass}, fail ${run.golden.summary.fail}, missing ${run.golden.summary.missing}, not applicable ${run.golden.summary.not_applicable}`);
  for (const r of run.rows.filter((x) => x.status === 'fail' || x.status === 'warn')) lines.push(`  ${r.status.toUpperCase()} ${r.check_id} (${r.n}): ${r.detail}`);
  return lines.join('\n');
}

async function main() {
  let o;
  try {
    o = parseArgs(process.argv.slice(2));
  } catch (e) {
    console.error(e instanceof Error ? e.message : String(e));
    process.exit(2);
  }
  let res;
  try {
    res = await runQc(o);
  } catch (e) {
    console.error(`run-bundle-qc: ${e instanceof Error ? e.message : String(e)}`);
    process.exit(2);
  }
  const paths = writeResults(res.run, o.out);
  if (!o.quiet) {
    console.log(summaryText(res.run));
    console.log(`wrote ${paths.length} files to ${o.out}`);
  }
  process.exit(qcFailed(res.run) ? 1 : 0);
}

if (process.argv[1] && resolve(process.argv[1]) === fileURLToPath(import.meta.url)) await main();
