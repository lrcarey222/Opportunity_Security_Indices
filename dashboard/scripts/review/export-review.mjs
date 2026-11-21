#!/usr/bin/env node
// Expert-review exports (docs/QC_SYSTEM_DESIGN.md 6.5): reviewer submissions and packets -> the qc_*.csv
// files (qc_review_responses, qc_case_results, qc_method_feedback, qc_data_issues, qc_framework_answers,
// qc_decision_use, qc_threat_reviews, qc_priority_fixes) plus export_summary.json. IO only: the row logic is
// src/review/exports.ts and the validation src/review/manifest.ts, imported as TypeScript (Node >= 24 strips
// the types). Reviewers appear as pseudonymous codes; names only with --include-names, never an anonymous
// reviewer's.
//
// Privacy guard: the output directory must be outside any git work tree, or inside the work tree's
// qc/private/ AND git-ignored there. Anything else (a tracked or trackable directory) is refused with exit
// code 2, so private review data cannot be committed by accident.
//
//   node scripts/review/export-review.mjs --submissions <dir|file> [<file> ...] --out <dir>
//        [--cases <cases.json>] [--adjudications <file>] [--cards <method_cards.json>]
//        [--scenarios <framework_scenarios.json>] [--include-names] [--strict]
//
// --submissions reads every *.json below a directory, or the files given: a submission, an array of them,
// a packet ({schema: "aib-review-packet", submissions: [...]}) or {submissions: [...]}. Invalid submissions
// are skipped with a warning (with --strict: nothing is written and the exit code is 1).

import { execFileSync } from 'node:child_process';
import { existsSync, mkdirSync, readdirSync, readFileSync, statSync, writeFileSync } from 'node:fs';
import { dirname, isAbsolute, join, relative, resolve, sep } from 'node:path';
import { pathToFileURL } from 'node:url';

import { csvFiles } from '../../src/review/exports.ts';
import { validateCasesFile, validateFrameworkScenarios, validateMethodCards, validateSubmission } from '../../src/review/manifest.ts';
import { PACKET_SCHEMA } from '../../src/review/sink.ts';

const USAGE =
  'usage: node scripts/review/export-review.mjs --submissions <dir|file> [<file> ...] --out <dir> [--cases <cases.json>] [--adjudications <file>] [--cards <file>] [--scenarios <file>] [--include-names] [--strict]';

/** Parse argv (after the script) into options; throws with the usage on a malformed command line. */
export function parseArgs(argv) {
  const o = { submissions: [], out: null, cases: null, adjudications: null, cards: null, scenarios: null, includeNames: false, strict: false };
  for (let i = 0; i < argv.length; i++) {
    const a = argv[i];
    const value = () => {
      const v = argv[i + 1];
      if (v === undefined || v.startsWith('--')) throw new Error(`${a} needs a value\n${USAGE}`);
      i++;
      return v;
    };
    switch (a) {
      case '--submissions':
        o.submissions.push(value());
        while (argv[i + 1] !== undefined && !argv[i + 1].startsWith('--')) o.submissions.push(argv[++i]);
        break;
      case '--out':
        o.out = value();
        break;
      case '--cases':
        o.cases = value();
        break;
      case '--adjudications':
        o.adjudications = value();
        break;
      case '--cards':
        o.cards = value();
        break;
      case '--scenarios':
        o.scenarios = value();
        break;
      case '--include-names':
        o.includeNames = true;
        break;
      case '--strict':
        o.strict = true;
        break;
      case '--help':
      case '-h':
        throw new Error(USAGE);
      default:
        throw new Error(`unknown argument ${a}\n${USAGE}`);
    }
  }
  if (o.submissions.length === 0) throw new Error(`--submissions is required\n${USAGE}`);
  if (!o.out) throw new Error(`--out is required\n${USAGE}`);
  return o;
}

const samePath = (a, b) => (process.platform === 'win32' ? a.toLowerCase() === b.toLowerCase() : a === b);

function git(args, cwd) {
  try {
    return execFileSync('git', args, { cwd, stdio: ['ignore', 'pipe', 'ignore'] }).toString().trim();
  } catch {
    return null;
  }
}

/**
 * Whether `outDir` may receive private exports: outside any git work tree, or under <top>/qc/private and
 * git-ignored. Returns {ok, reason}.
 */
export function checkOutputDir(outDir) {
  const abs = resolve(outDir);
  let probe = abs;
  while (!existsSync(probe)) {
    const up = dirname(probe);
    if (up === probe) break;
    probe = up;
  }
  const top = git(['rev-parse', '--show-toplevel'], probe);
  if (!top) return { ok: true, reason: 'outside any git work tree' };
  const topAbs = resolve(top);
  const allowed = join(topAbs, 'qc', 'private');
  const rel = relative(allowed, abs);
  const insidePrivate = samePath(abs, allowed) || rel === '' || (!rel.startsWith('..') && !isAbsolute(rel));
  if (!insidePrivate) {
    return { ok: false, reason: `${abs} is inside the git work tree ${topAbs} but not under qc/private/: review exports hold private data and must never be committed. Use --out ${join(topAbs, 'qc', 'private', 'exports')} or a directory outside the repository.` };
  }
  // qc/private must be ignored, so nothing written there can be staged
  const probeFile = relative(topAbs, join(abs, '__export_probe__.csv')).split(sep).join('/');
  const ignored = git(['check-ignore', '-q', '--no-index', probeFile], topAbs);
  if (ignored === null) {
    return { ok: false, reason: `${abs} is under qc/private/ but git does not ignore it: add "qc/private/" to .gitignore before exporting.` };
  }
  return { ok: true, reason: 'under qc/private/ (git-ignored)' };
}

function listJson(p) {
  const abs = resolve(p);
  if (!existsSync(abs)) throw new Error(`${p}: no such file or directory`);
  if (statSync(abs).isFile()) return [abs];
  const out = [];
  for (const e of readdirSync(abs, { withFileTypes: true })) {
    const f = join(abs, e.name);
    if (e.isDirectory()) out.push(...listJson(f));
    else if (e.isFile() && e.name.toLowerCase().endsWith('.json')) out.push(f);
  }
  return out.sort();
}

const readJson = (f) => JSON.parse(readFileSync(f, 'utf8').replace(/^﻿/, ''));

/** The submissions a parsed file holds (a submission, an array, a packet or {submissions}). */
export function submissionsOf(doc) {
  if (Array.isArray(doc)) return doc;
  if (doc && typeof doc === 'object') {
    if (doc.schema === PACKET_SCHEMA || Array.isArray(doc.submissions)) return Array.isArray(doc.submissions) ? doc.submissions : [];
    return [doc];
  }
  return [];
}

/** Read and validate every submission under the given paths. */
export function collectSubmissions(paths) {
  const valid = [];
  const problems = [];
  let files = 0;
  for (const p of paths) {
    for (const f of listJson(p)) {
      files++;
      let doc;
      try {
        doc = readJson(f);
      } catch (e) {
        problems.push(`${f}: not JSON (${e instanceof Error ? e.message : String(e)})`);
        continue;
      }
      submissionsOf(doc).forEach((s, i) => {
        const v = validateSubmission(s);
        if (v.ok) valid.push(s);
        else problems.push(`${f} [${i}]: ${v.errors.join('; ')}`);
      });
    }
  }
  return { valid, problems, files };
}

function listOf(doc, key) {
  if (Array.isArray(doc)) return doc;
  if (doc && typeof doc === 'object' && Array.isArray(doc[key])) return doc[key];
  if (doc && typeof doc === 'object') return [doc];
  return [];
}

/** Run the export; returns the exit code (0 ok, 1 invalid input, 2 refused output directory). */
export function run(argv, log = console) {
  let o;
  try {
    o = parseArgs(argv);
  } catch (e) {
    log.error(e instanceof Error ? e.message : String(e));
    return 1;
  }
  const guard = checkOutputDir(o.out);
  if (!guard.ok) {
    log.error(`refused: ${guard.reason}`);
    return 2;
  }

  let subs;
  try {
    subs = collectSubmissions(o.submissions);
  } catch (e) {
    log.error(e instanceof Error ? e.message : String(e));
    return 1;
  }
  for (const p of subs.problems) log.warn(`skipped: ${p}`);
  if (o.strict && subs.problems.length) {
    log.error(`--strict: ${subs.problems.length} invalid submission(s); nothing written`);
    return 1;
  }

  let cases = [];
  let casesMeta = null;
  if (o.cases) {
    const doc = readJson(resolve(o.cases));
    const v = validateCasesFile(doc);
    for (const w of v.warnings) log.warn(`cases: ${w}`);
    if (!v.ok) {
      log.error(`cases file ${o.cases} is invalid:\n  ${v.errors.join('\n  ')}`);
      return 1;
    }
    cases = doc.cases;
    casesMeta = { campaign_id: doc.campaign_id, data_release: doc.data_release, built_at: doc.built_at };
  }
  const cards = o.cards ? listOf(readJson(resolve(o.cards)), 'cards') : [];
  if (o.cards) {
    const v = validateMethodCards(cards);
    if (!v.ok) log.warn(`method cards: ${v.errors.length} problem(s); exported anyway (${v.errors[0]})`);
  }
  const scenarios = o.scenarios ? listOf(readJson(resolve(o.scenarios)), 'scenarios') : [];
  if (o.scenarios) {
    const v = validateFrameworkScenarios(scenarios);
    if (!v.ok) log.warn(`framework scenarios: ${v.errors.length} problem(s); exported anyway (${v.errors[0]})`);
  }
  const adjudications = o.adjudications ? listOf(readJson(resolve(o.adjudications)), 'adjudications') : [];

  const files = csvFiles(subs.valid, cases, adjudications, { includeNames: o.includeNames, cards, scenarios });
  const outAbs = resolve(o.out);
  mkdirSync(outAbs, { recursive: true });
  for (const f of files) writeFileSync(join(outAbs, f.name), f.text, 'utf8');
  const summary = {
    schema: 'aib-review-export',
    files: files.map((f) => ({ name: f.name, rows: f.rows })),
    n_input_files: subs.files,
    n_submissions: subs.valid.length,
    n_skipped: subs.problems.length,
    cases: casesMeta,
    n_adjudications: adjudications.length,
    data_releases: [...new Set(subs.valid.map((s) => s.data_release).filter(Boolean))].sort(),
    include_names: o.includeNames,
  };
  writeFileSync(join(outAbs, 'export_summary.json'), `${JSON.stringify(summary, null, 2)}\n`, 'utf8');
  log.log(`wrote ${files.length} CSV files (${subs.valid.length} submissions${subs.problems.length ? `, ${subs.problems.length} skipped` : ''}) to ${outAbs} [${guard.reason}]`);
  if (summary.data_releases.length > 1) log.warn(`the submissions span ${summary.data_releases.length} data releases: ${summary.data_releases.join(', ')}`);
  return 0;
}

if (process.argv[1] && import.meta.url === pathToFileURL(resolve(process.argv[1])).href) {
  process.exitCode = run(process.argv.slice(2));
}
