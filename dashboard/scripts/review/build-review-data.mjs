#!/usr/bin/env node
// The review data build (docs/QC_SYSTEM_DESIGN.md 3, 4): authored YAML (config/review/campaigns/*.yml,
// review_cases.yml, method_cards.yml, framework_scenarios.yml) + the shipped bundles (dashboard/public/data,
// every file read is sha256-hashed) -> dashboard/public/data/review/:
//   manifest.json                 schema aib-review-manifest: campaigns, the data release (ID, stamps,
//                                 completeness), every file written (sha256, bytes) and every bundle file read
//   cases.<campaign_id>.json      CasesFile: each case's snapshot, its sha256, bundle stamps, release, commit
//   method_cards.json, framework_scenarios.json (canonical option sets filled from the vocabulary constants)
//   robustness.json               compact class map of every posture cell and AIB node (bundle layer)
//   qc_summary.json               bundle checks, reproduction, robustness and golden status (admin dashboard)
//   fixtures/demo_submissions.json  SYNTHETIC demo submissions (review/demo.ts), labelled so
// Every authored file is validated (src/review/manifest.ts) and the built cases again; any error stops the build
// with exit code 1 and nothing is written. Deterministic: the same inputs, app commit and --built-at give
// byte-identical files. IO only: the logic is src/review/qc/reviewData.ts (TypeScript; Node >= 24 strips types).
//
//   node scripts/review/build-review-data.mjs [--config-dir <repo/config>] [--data-dir <dashboard/public/data>]
//        [--out <data-dir/review>] [--golden <config/qc/golden_cases.yml> | --no-golden]
//        [--built-at <ISO time>] [--app-commit <sha>] [--seed <demo seed>] [--no-demo] [--quiet]

import { execFileSync } from 'node:child_process';
import { createHash } from 'node:crypto';
import { existsSync, mkdirSync, readdirSync, readFileSync, writeFileSync } from 'node:fs';
import { dirname, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { parse } from 'yaml';

import { makeDemoSubmissions, SYNTHETIC } from '../../src/review/demo.ts';
import { dataReleaseId, releaseCompleteness } from '../../src/review/release.ts';
import { buildReviewData, reviewManifest, validateReviewContent } from '../../src/review/qc/reviewData.ts';
import { qcFailed } from '../../src/review/qc/run.ts';
import { parseArgs as parseQcArgs, runQc } from './run-bundle-qc.mjs';

const HERE = dirname(fileURLToPath(import.meta.url));
const DASHBOARD = resolve(HERE, '..', '..');
const REPO = resolve(DASHBOARD, '..');
const USAGE =
  'usage: node scripts/review/build-review-data.mjs [--config-dir <dir>] [--data-dir <dir>] [--out <dir>] [--golden <file> | --no-golden] [--built-at <ISO>] [--app-commit <sha>] [--seed <s>] [--no-demo] [--quiet]';
export const DEMO_SCHEMA = 'aib-review-demo-submissions';

/** Parse argv into options (defaults relative to this script's checkout); throws with the usage on a bad command line. */
export function parseArgs(argv) {
  const o = {
    configDir: resolve(REPO, 'config'),
    dataDir: resolve(DASHBOARD, 'public', 'data'),
    out: null,
    golden: resolve(REPO, 'config', 'qc', 'golden_cases.yml'),
    builtAt: null,
    appCommit: null,
    seed: 'aib-review-demo',
    demo: true,
    quiet: false,
  };
  for (let i = 0; i < argv.length; i++) {
    const a = argv[i];
    const value = () => {
      const v = argv[i + 1];
      if (v === undefined || v.startsWith('--')) throw new Error(`${a} needs a value\n${USAGE}`);
      i++;
      return v;
    };
    if (a === '--config-dir') o.configDir = resolve(value());
    else if (a === '--data-dir') o.dataDir = resolve(value());
    else if (a === '--out') o.out = resolve(value());
    else if (a === '--golden') o.golden = resolve(value());
    else if (a === '--no-golden') o.golden = null;
    else if (a === '--built-at') {
      o.builtAt = value();
      if (Number.isNaN(Date.parse(o.builtAt))) throw new Error(`--built-at is not an ISO time: ${o.builtAt}`);
    } else if (a === '--app-commit') o.appCommit = value();
    else if (a === '--seed') o.seed = value();
    else if (a === '--no-demo') o.demo = false;
    else if (a === '--quiet') o.quiet = true;
    else if (a === '--help' || a === '-h') throw new Error(USAGE);
    else throw new Error(`unknown argument ${a}\n${USAGE}`);
  }
  o.out ??= resolve(o.dataDir, 'review');
  return o;
}

/** The short commit of the checkout this script runs in ('unknown' outside git). */
export function appCommit() {
  try {
    return execFileSync('git', ['rev-parse', '--short', 'HEAD'], { cwd: DASHBOARD, encoding: 'utf8', stdio: ['ignore', 'pipe', 'ignore'] }).trim() || 'unknown';
  } catch {
    return 'unknown';
  }
}

const readYaml = (path) => {
  if (!existsSync(path)) throw new Error(`missing file: ${path}`);
  try {
    return parse(readFileSync(path, 'utf8'));
  } catch (e) {
    throw new Error(`${path} is not valid YAML: ${e instanceof Error ? e.message : String(e)}`);
  }
};

/** Read the authored review content. */
export function readContent(configDir) {
  const dir = resolve(configDir, 'review');
  const cdir = resolve(dir, 'campaigns');
  const files = existsSync(cdir) ? readdirSync(cdir).filter((f) => /\.ya?ml$/.test(f)).sort() : [];
  return {
    campaigns: files.map((f) => ({ file: f, doc: readYaml(resolve(cdir, f)) })),
    catalogue: readYaml(resolve(dir, 'review_cases.yml')),
    cards: readYaml(resolve(dir, 'method_cards.yml')),
    scenarios: readYaml(resolve(dir, 'framework_scenarios.yml')),
  };
}

const json = (x, indent = 2) => JSON.stringify(x, null, indent) + '\n';
const sha = (text) => createHash('sha256').update(text, 'utf8').digest('hex');

/** Build every document (nothing written); throws with every error when the content or the build is invalid. */
export async function build(o) {
  const raw = readContent(o.configDir);
  const v = validateReviewContent(raw);
  if (!v.content) throw Object.assign(new Error(`invalid review content:\n  ${v.errors.join('\n  ')}`), { warnings: v.warnings });
  const { load, decoded, run } = await runQc({ ...parseQcArgs([]), dataDir: o.dataDir, golden: o.golden });
  const releaseId = await dataReleaseId(decoded.stamps);
  const completeness = releaseCompleteness(decoded.stamps);
  const commit = o.appCommit ?? appCommit();
  const builtAt = o.builtAt ?? new Date().toISOString();
  const built = buildReviewData({ content: v.content, decoded, run, releaseId, appCommit: commit, builtAt });
  if (built.errors.length) throw Object.assign(new Error(`the review data build failed:\n  ${built.errors.join('\n  ')}`), { warnings: [...v.warnings, ...built.warnings] });
  const texts = new Map();
  // compact JSON (UX v2): the case briefs and reasoning reveals more than doubled the file, and indented JSON
  // gzips about twice as large as compact JSON at this size (147 kB vs 66 kB for round 1)
  for (const f of built.casesFiles) texts.set(`cases.${f.campaign_id}.json`, json(f, 0));
  texts.set('method_cards.json', json(built.methodCards));
  texts.set('framework_scenarios.json', json(built.frameworkScenarios));
  texts.set('robustness.json', json(built.robustness, 0));
  texts.set('qc_summary.json', json(built.qcSummary));
  if (o.demo) {
    const cases = built.casesFiles.flatMap((f) => f.cases);
    const camp = built.casesFiles[0]?.campaign_id ?? 'demo';
    const subs = makeDemoSubmissions(cases, built.methodCards.cards, built.frameworkScenarios.scenarios, o.seed, camp);
    texts.set(
      'fixtures/demo_submissions.json',
      json({
        schema: DEMO_SCHEMA, schema_version: 1, synthetic: true, marker: SYNTHETIC,
        note: 'SYNTHETIC DEMO DATA generated by review/demo.ts from a fixed seed: not expert judgements, never to be quoted or counted as reviews.',
        seed: o.seed, data_release: releaseId, built_at: builtAt, submissions: subs,
      }),
    );
  }
  const outputs = [...texts].map(([file, text]) => ({ file, sha256: sha(text), bytes: Buffer.byteLength(text, 'utf8') }));
  const manifest = reviewManifest({ releaseId, stamps: decoded.stamps, completeness, appCommit: commit, builtAt, content: v.content, casesFiles: built.casesFiles, outputs, bundleFiles: built.bundleFiles, run });
  texts.set('manifest.json', json(manifest));
  return { texts, manifest, run, warnings: [...v.warnings, ...built.warnings, ...load.problems.map((p) => `bundle load: ${p}`)] };
}

/** Write the built documents under out. */
export function writeAll(texts, out) {
  for (const [file, text] of texts) {
    const path = resolve(out, file);
    mkdirSync(dirname(path), { recursive: true });
    writeFileSync(path, text);
  }
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
    res = await build(o);
  } catch (e) {
    for (const w of e?.warnings ?? []) console.error(`warning: ${w}`);
    console.error(`build-review-data: ${e instanceof Error ? e.message : String(e)}`);
    process.exit(1);
  }
  writeAll(res.texts, o.out);
  if (!o.quiet) {
    for (const w of res.warnings) console.warn(`warning: ${w}`);
    const m = res.manifest;
    console.log(`data release ${m.data_release.id}${m.data_release.complete ? '' : ` (stamps missing: ${m.data_release.missing.join(', ')})`}; app ${m.app_commit}; built ${m.built_at}`);
    for (const c of m.campaigns) console.log(`campaign ${c.campaign_id} (${c.status}): ${c.n_cases} cases -> ${c.cases_file}`);
    console.log(`bundle QC: pass ${m.qc.status.pass}, warn ${m.qc.status.warn}, fail ${m.qc.status.fail}, info ${m.qc.status.info}${qcFailed(res.run) ? ' (FAILING CHECKS: see qc_summary.json)' : ''}`);
    console.log(`wrote ${res.texts.size} files to ${o.out}`);
  }
}

if (process.argv[1] && resolve(process.argv[1]) === fileURLToPath(import.meta.url)) await main();
