#!/usr/bin/env node
// npm run budget (spec_react.md 10.3; spec_sector.md 8.11, 11): what each first visit downloads, and the
// data, against the limits. Sizes are gzip sizes (level 9 here, close to what Netlify serves); OSB data
// chunks are gzip containers already and count as they travel. Run after `npm run build`.
//
//   Home first visit        entry JS + CSS + the Latin Roboto face + the Home route chunk
//                           (no data, no worker, no world atlas)                               <= 400 kB
//   Strategic /overview     entry + StrategicLayout + Overview + world atlas + engine worker
//                           + data/manifest.json + the `initial` data chunks                    <= 3 MB
//   Sector landing /sector  entry + SectorLayout + the Sector home view + sector manifest + meta <= 700 kB
//   Sector profile          the landing + the profile view + facts                             <= 1.3 MB
//   each sector chunk       meta 150 kB, facts 600 kB, trade 450 kB each, policy 250 kB,
//                           policy-text 700 kB, series 900 kB, atlas 1.3 MB, dev 1.2 MB
//   Sector data             public/data/sector/* (manifest + chunks)                           <= 9 MB
//   All data                Strategic + Sector                                                 <= 25 MB
//
// Fails above a limit, when a manifest lists a chunk file that does not exist, or when a route chunk
// the figures need cannot be found in the build. A data bundle that is absent (CI without the
// exports) is reported and skipped; its code figures are still checked. Warns when the app shell's own
// JS + CSS exceeds 350 kB.

import { existsSync, readdirSync, readFileSync, statSync } from 'node:fs';
import { dirname, join, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { gzipSync } from 'node:zlib';

const root = join(dirname(fileURLToPath(import.meta.url)), '..');
// BUDGET_DIST: check another build directory (e.g. vite build --outDir elsewhere); BUDGET_SECTOR_DIR:
// another Sector bundle (e.g. tests/fixtures/sector/synthetic, to exercise the Sector figures)
const dist = process.env.BUDGET_DIST ?? join(root, 'dist');
const dataDir = join(root, 'public', 'data');
const sectorDir = process.env.BUDGET_SECTOR_DIR ? resolve(root, process.env.BUDGET_SECTOR_DIR) : join(dataDir, 'sector');

const KB = 1024;
const MB = 1024 * 1024;
const LIMITS = {
  home: 400 * KB,
  strategic: 3 * MB,
  sectorLanding: 700 * KB,
  sectorProfile: 1.3 * MB,
  sectorData: 9 * MB,
  allData: 25 * MB,
};
const TARGET_APP = 350 * KB;
const SECTOR_CHUNK_LIMIT = {
  'sector-meta': 150 * KB,
  'sector-facts': 600 * KB,
  'sector-trade': 450 * KB,
  'sector-policy': 250 * KB,
  'sector-policy-text': 700 * KB,
  'sector-series': 900 * KB,
  'sector-atlas': 1.3 * MB,
  'sector-dev': 1.2 * MB,
};

const kb = (n) => `${(n / KB).toFixed(1)} kB`;
const mb = (n) => `${(n / MB).toFixed(2)} MB`;
const size = (n) => (n >= MB ? mb(n) : kb(n));
const gzCache = new Map();
const gz = (file) => {
  if (!gzCache.has(file)) gzCache.set(file, gzipSync(readFileSync(file), { level: 9 }).length);
  return gzCache.get(file);
};
/** a gzip container (OSB) travels as is */
const isGzip = (file) => {
  const b = readFileSync(file);
  return b.length > 2 && b[0] === 0x1f && b[1] === 0x8b;
};
const wire = (file) => (isGzip(file) ? statSync(file).size : gz(file));

let failed = false;
const fail = (msg) => {
  console.error(`budget: FAIL, ${msg}`);
  failed = true;
};

// ---- the build ---------------------------------------------------------------------------------------
const manifestPath = join(dist, '.vite', 'manifest.json');
if (!existsSync(manifestPath)) {
  console.error('budget: dist/.vite/manifest.json not found; run `npm run build` first.');
  process.exit(1);
}
const vm = JSON.parse(readFileSync(manifestPath, 'utf8'));
const entryKey = Object.keys(vm).find((k) => vm[k].isEntry);
const findKey = (re) => Object.keys(vm).find((k) => re.test(k)) ?? null;

/** The files a module key loads up front: its JS, its CSS and its static imports (recursively). */
function closure(keys) {
  const seen = new Set();
  const files = new Set();
  const walk = (key) => {
    if (!key || seen.has(key)) return;
    seen.add(key);
    const c = vm[key];
    if (!c) return;
    files.add(c.file);
    for (const css of c.css ?? []) files.add(css);
    for (const imp of c.imports ?? []) walk(imp);
  };
  for (const k of keys) walk(k);
  return files;
}

const entryFiles = closure([entryKey]);
const worker = readdirSync(join(dist, 'assets'))
  .filter((f) => /^engine\.worker-.*\.js$/.test(f))
  .map((f) => `assets/${f}`);
// a Latin page needs one face of the variable font; the other subsets load only for their scripts
const fonts = (vm[entryKey].assets ?? []).filter((f) => /roboto-latin-wght-normal/.test(f));
const atlasKey = findKey(/countries-50m/);

/**
 * Route chunks by source path. A lazy route module that another module also imports statically (the
 * profile view, which Country in sector reuses) is emitted as a shared chunk: its manifest key is
 * `_<Name>-<hash>.js`, with no `src` but `isDynamicEntry` and `name`; it is found by `name`. A missing
 * one is a FAIL when `required` (the code moved?).
 */
function routeKey(label, re, name, required = true) {
  const k =
    Object.keys(vm).find((x) => re.test(x) || re.test(vm[x].src ?? '')) ??
    Object.keys(vm).find((x) => vm[x].isDynamicEntry && !vm[x].src && vm[x].name === name) ??
    null;
  if (!k && required) fail(`no build chunk for ${label} (${re}); update scripts/check-budget.mjs if the file moved`);
  return k;
}
const keys = {
  home: routeKey('Home', /src\/shell\/Home\.tsx$/, 'Home'),
  strategicLayout: routeKey('StrategicLayout', /src\/shell\/StrategicLayout\.tsx$/, 'StrategicLayout'),
  overview: routeKey('the Overview view', /src\/views\/Overview\/OverviewView\.tsx$/, 'OverviewView'),
  // the Sector layout and views (builder C); optional while the module is a stub
  sectorLayout: routeKey('SectorLayout', /src\/sector\/SectorLayout\.tsx$/, 'SectorLayout', false),
  sectorHome: routeKey('the Sector home view', /src\/sector\/views\/(.*\/)?SectorHomeView\.tsx$/, 'SectorHomeView', false),
  sectorProfile: routeKey('the Sector profile view', /src\/sector\/views\/(.*\/)?SectorProfileView\.tsx$/, 'SectorProfileView', false),
};

const codeSize = (files) => [...files].reduce((s, f) => s + gz(join(dist, f)), 0);
const fontSize = fonts.reduce((s, f) => s + statSync(join(dist, f)).size, 0); // woff2 is compressed already

// ---- the data ----------------------------------------------------------------------------------------
function readManifest(path) {
  return existsSync(path) ? JSON.parse(readFileSync(path, 'utf8')) : null;
}
const strategicManifest = readManifest(join(dataDir, 'manifest.json'));
const sectorManifest = readManifest(join(sectorDir, 'manifest.json'));

/** Every chunk of a manifest with its wire size; a listed file that is missing is a FAIL. */
function dataChunks(m, dir, label) {
  const out = [];
  for (const c of m?.chunks ?? []) {
    const file = join(dir, c.file);
    if (!existsSync(file)) {
      fail(`${label} chunk ${c.file} is listed in its manifest but missing`);
      continue;
    }
    out.push({ ...c, wire: wire(file) });
  }
  return out;
}
const strategicChunks = strategicManifest ? dataChunks(strategicManifest, dataDir, 'Strategic') : [];
const sectorChunks = sectorManifest ? dataChunks(sectorManifest, sectorDir, 'Sector') : [];
const strategicManifestSize = strategicManifest ? gz(join(dataDir, 'manifest.json')) : 0;
const sectorManifestSize = sectorManifest ? gz(join(sectorDir, 'manifest.json')) : 0;
const sectorChunk = (name) => sectorChunks.find((c) => c.name === name)?.wire ?? 0;

// ---- the figures -------------------------------------------------------------------------------------
const figures = [];
function figure(name, parts, limit, note = '') {
  const total = parts.reduce((s, [, n]) => s + n, 0);
  figures.push({ name, total, limit, note, parts });
  if (total > limit) fail(`${name} ${size(total)} > ${size(limit)}`);
  return total;
}

const app = codeSize(entryFiles);
const homeOnly = keys.home ? new Set([...closure([keys.home])].filter((f) => !entryFiles.has(f))) : new Set();
figure('Home first visit', [['entry JS + CSS', app], ['Home chunk', codeSize(homeOnly)], ['font', fontSize]], LIMITS.home, 'no data, no worker, no world atlas');

const strategicCode = closure([entryKey, keys.strategicLayout, keys.overview].filter(Boolean));
if (atlasKey) strategicCode.add(vm[atlasKey].file);
for (const w of worker) strategicCode.add(w);
const strategicInitialData = strategicChunks.filter((c) => c.initial).reduce((s, c) => s + c.wire, 0);
figure(
  'Strategic deep link /overview',
  [['code + atlas + worker', codeSize(strategicCode)], ['font', fontSize], ['data/manifest.json', strategicManifestSize], ['initial data chunks', strategicInitialData]],
  LIMITS.strategic,
  strategicManifest ? '' : 'Strategic data absent (scripts/85): code only',
);

const landingCode = closure([entryKey, keys.sectorLayout, keys.sectorHome].filter(Boolean));
const landingData = sectorManifestSize + sectorChunk('sector-meta');
figure('Sector landing /sector', [['code', codeSize(landingCode)], ['font', fontSize], ['sector manifest + meta', landingData]], LIMITS.sectorLanding, sectorManifest ? '' : 'Sector data absent (scripts/87): code only');

const profileCode = closure([entryKey, keys.sectorLayout, keys.sectorHome, keys.sectorProfile].filter(Boolean));
figure(
  'Sector profile first view',
  [['code', codeSize(profileCode)], ['font', fontSize], ['sector manifest + meta + facts', landingData + sectorChunk('sector-facts')]],
  LIMITS.sectorProfile,
  sectorManifest ? '' : 'Sector data absent: code only',
);

// per-chunk sector limits
for (const c of sectorChunks) {
  const kind = c.name.startsWith('sector-trade-') ? 'sector-trade' : c.name;
  const limit = SECTOR_CHUNK_LIMIT[kind];
  if (limit !== undefined && c.wire > limit) fail(`${c.file} ${size(c.wire)} > ${size(limit)} (${kind})`);
}

const sectorData = sectorManifestSize + sectorChunks.reduce((s, c) => s + c.wire, 0);
const strategicData = strategicManifestSize + strategicChunks.reduce((s, c) => s + c.wire, 0);
if (sectorManifest) figure('Sector data', [['manifest + chunks', sectorData]], LIMITS.sectorData);
figure('All data', [['Strategic', strategicData], ['Sector', sectorData]], LIMITS.allData);

// ---- report ------------------------------------------------------------------------------------------
console.log('Data chunks');
const rows = [
  ...(strategicManifest ? [['data/manifest.json', strategicManifestSize, '']] : []),
  ...strategicChunks.map((c) => [`data/${c.file}`, c.wire, c.initial ? 'initial' : 'on demand']),
  ...(sectorManifest ? [['data/sector/manifest.json', sectorManifestSize, '']] : []),
  ...sectorChunks.map((c) => {
    const kind = c.name.startsWith('sector-trade-') ? 'sector-trade' : c.name;
    return [`data/sector/${c.file}`, c.wire, SECTOR_CHUNK_LIMIT[kind] ? `limit ${size(SECTOR_CHUNK_LIMIT[kind])}` : ''];
  }),
];
const w = Math.max(20, ...rows.map((r) => r[0].length));
for (const [f, n, note] of rows) console.log(`  ${f.padEnd(w)} ${size(n).padStart(10)}  ${note}`);
if (!strategicManifest) console.log('  (public/data/manifest.json not found: export the Strategic bundle with scripts/85)');
if (!sectorManifest) console.log('  (public/data/sector/manifest.json not found: export the Sector bundle with scripts/87)');
console.log('');
console.log('Figures');
for (const f of figures) {
  const status = f.total > f.limit ? 'FAIL' : 'ok  ';
  console.log(`  ${status} ${f.name.padEnd(30)} ${size(f.total).padStart(10)}  (limit ${size(f.limit)})${f.note ? `  ${f.note}` : ''}`);
  for (const [label, n] of f.parts) console.log(`         ${label.padEnd(32)} ${size(n).padStart(10)}`);
}
console.log('');
console.log(`app shell JS + CSS (entry): ${kb(app)}  (target ${kb(TARGET_APP)})`);
if (app > TARGET_APP) console.warn(`budget: warning, the app's entry JS + CSS (${kb(app)}) is above the ${kb(TARGET_APP)} target`);
if (!keys.sectorLayout) console.log('budget: note, no SectorLayout chunk in this build (the Sector module is a stub or eagerly bundled): the Sector figures count the entry only');
if (failed) process.exit(1);
console.log('budget: OK');
