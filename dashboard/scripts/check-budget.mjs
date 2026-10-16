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
//   Country landing         entry + CountryLayout + landing view + world atlas + manifest      <= 700 kB
//   Country profile         entry + CountryLayout + profile view + the largest fact sheet      <= 900 kB
//   Country memo view       entry + CountryLayout + memo view + the largest memo JSON          <= 450 kB
//   each Country file       fact sheet 40 kB, manifest 20 kB, memo JSON 25 kB / PDF 400 kB / DOCX 100 kB
//   Country data            public/data/country/** (every file)                                <= 11 MB
//   Compacts landing        entry + AibLayout + the compacts index view + manifest + document  <= 700 kB
//   Compact view            entry + AibLayout + the compact view + manifest + document         <= 900 kB
//   Compacts data           public/data/aib/* (manifest + the compacts document)               <= 2 MB
//   All data                Strategic + Sector + Country + Compacts                            <= 25 MB
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
// BUDGET_COUNTRY_DIR: another Country bundle (e.g. src/country/__fixtures__/synthetic/data/country)
const countryDir = process.env.BUDGET_COUNTRY_DIR ? resolve(root, process.env.BUDGET_COUNTRY_DIR) : join(dataDir, 'country');
// BUDGET_AIB_DIR: another Sectoral Compacts bundle (e.g. src/aib/__fixtures__/synthetic/data/aib)
const aibDir = process.env.BUDGET_AIB_DIR ? resolve(root, process.env.BUDGET_AIB_DIR) : join(dataDir, 'aib');

const KB = 1024;
const MB = 1024 * 1024;
const LIMITS = {
  home: 400 * KB,
  strategic: 3 * MB,
  sectorLanding: 700 * KB,
  sectorProfile: 1.3 * MB,
  sectorData: 9 * MB,
  countryLanding: 700 * KB,
  countryProfile: 900 * KB,
  countryMemo: 450 * KB,
  // 10 MB until 2026-10-14; 11 MB once the 31 memos (PDF, Word, Markdown, JSON) were published (10.23 MB)
  countryData: 11 * MB,
  aibLanding: 700 * KB,
  aibCompact: 900 * KB,
  aibData: 2 * MB,
  allData: 25 * MB,
};
// spec_country.md A10 (the same numbers as COUNTRY_BUDGET_GZ in src/country/contract.ts)
// manifest: 15 kB until 2026-10-14; 20 kB once the 31 memo entries (2.8 kB gz) were added (16.9 kB)
const COUNTRY_FILE_LIMIT = { manifest: 20 * KB, sheet: 40 * KB, memoJson: 25 * KB, memoPdf: 400 * KB, memoDocx: 100 * KB };
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
  countryLayout: routeKey('CountryLayout', /src\/country\/CountryLayout\.tsx$/, 'CountryLayout'),
  countryHome: routeKey('the Country landing view', /src\/country\/views\/CountryHomeView\.tsx$/, 'CountryHomeView'),
  countryProfile: routeKey('the Country profile view', /src\/country\/views\/CountryProfileView\.tsx$/, 'CountryProfileView'),
  countryMemo: routeKey('the Country memo view', /src\/country\/views\/MemoView\.tsx$/, 'MemoView'),
  // Sectoral Compacts (src/aib/); optional, so a build without the module still checks the others
  aibLayout: routeKey('AibLayout', /src\/aib\/AibLayout\.tsx$/, 'AibLayout', false),
  aibIndex: routeKey('the compacts index view', /src\/aib\/views\/CompactsIndexView\.tsx$/, 'CompactsIndexView', false),
  aibCompact: routeKey('the compact view', /src\/aib\/views\/CompactView\.tsx$/, 'CompactView', false),
  aibAbout: routeKey('the compacts about view', /src\/aib\/views\/AibAboutView\.tsx$/, 'AibAboutView', false),
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

// Country (scripts/88): JSON and Markdown files travel gzipped; PDF and Word files are compressed already
// (counted as they are). Every file under public/data/country counts towards the Country total.
const countryManifest = readManifest(join(countryDir, 'manifest.json'));
const countryWire = (file) => (/\.(pdf|docx)$/.test(file) ? statSync(file).size : gz(file));
function walkFiles(dir) {
  if (!existsSync(dir)) return [];
  return readdirSync(dir, { withFileTypes: true }).flatMap((d) => (d.isDirectory() ? walkFiles(join(dir, d.name)) : [join(dir, d.name)]));
}
const countryFiles = countryManifest ? walkFiles(countryDir) : [];
const countryManifestSize = countryManifest ? gz(join(countryDir, 'manifest.json')) : 0;
const countryListed = (name) => {
  if (!name) return 0;
  const file = join(countryDir, name);
  if (!existsSync(file)) {
    fail(`Country file ${name} is listed in its manifest but missing`);
    return 0;
  }
  return countryWire(file);
};
const countrySheets = (countryManifest?.countries ?? []).filter((c) => c.file).map((c) => [c.file, countryListed(c.file)]);
const countryMemos = (countryManifest?.countries ?? [])
  .filter((c) => c.memo)
  .map((c) => ({ iso3: c.iso3, json: countryListed(c.memo.json), pdf: countryListed(c.memo.pdf), docx: countryListed(c.memo.docx) }));
const maxOf = (xs) => xs.reduce((m, x) => Math.max(m, x), 0);

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

// Country Insights (spec_country.md A10)
const countryLandingCode = closure([entryKey, keys.countryLayout, keys.countryHome].filter(Boolean));
if (atlasKey) countryLandingCode.add(vm[atlasKey].file);
figure(
  'Country landing /countries',
  [['code + atlas', codeSize(countryLandingCode)], ['font', fontSize], ['country manifest', countryManifestSize]],
  LIMITS.countryLanding,
  countryManifest ? '' : 'Country data absent (scripts/88): code only',
);
const maxSheet = maxOf(countrySheets.map(([, n]) => n));
figure(
  'Country profile first view',
  [
    ['code', codeSize(closure([entryKey, keys.countryLayout, keys.countryProfile].filter(Boolean)))],
    ['font', fontSize],
    ['country manifest', countryManifestSize],
    ['largest fact sheet', maxSheet],
  ],
  LIMITS.countryProfile,
  countryManifest ? '' : 'Country data absent: code only',
);
figure(
  'Country memo view',
  [
    ['code', codeSize(closure([entryKey, keys.countryLayout, keys.countryMemo].filter(Boolean)))],
    ['font', fontSize],
    ['country manifest', countryManifestSize],
    ['largest memo JSON', maxOf(countryMemos.map((m) => m.json))],
  ],
  LIMITS.countryMemo,
  countryManifest ? `${countryMemos.length} memos` : 'Country data absent: code only',
);
if (countryManifest && countryManifestSize > COUNTRY_FILE_LIMIT.manifest) fail(`Country manifest ${size(countryManifestSize)} > ${size(COUNTRY_FILE_LIMIT.manifest)}`);
for (const [f, n] of countrySheets) if (n > COUNTRY_FILE_LIMIT.sheet) fail(`${f} ${size(n)} > ${size(COUNTRY_FILE_LIMIT.sheet)} (fact sheet)`);
for (const m of countryMemos) {
  for (const [k, lim] of [['json', COUNTRY_FILE_LIMIT.memoJson], ['pdf', COUNTRY_FILE_LIMIT.memoPdf], ['docx', COUNTRY_FILE_LIMIT.memoDocx]]) {
    if (m[k] > lim) fail(`memo ${m.iso3} ${k.toUpperCase()} ${size(m[k])} > ${size(lim)}`);
  }
}
const countryData = countryFiles.reduce((s, f) => s + countryWire(f), 0);
if (countryManifest) figure('Country data', [['manifest + sheets + drivers + memos', countryData]], LIMITS.countryData);

// Sectoral Compacts (scripts/84): the manifest and the one document it names, both JSON (gzipped on the wire)
const aibManifest = readManifest(join(aibDir, 'manifest.json'));
const aibManifestSize = aibManifest ? gz(join(aibDir, 'manifest.json')) : 0;
let aibDocSize = 0;
if (aibManifest) {
  const f = aibManifest.files?.compacts ? join(aibDir, aibManifest.files.compacts) : null;
  if (!f || !existsSync(f)) fail(`Compacts document ${aibManifest.files?.compacts ?? '(none)'} is listed in its manifest but missing`);
  else aibDocSize = gz(f);
}
const aibNote = aibManifest ? '' : 'Compacts data absent (scripts/84): code only';
figure(
  'Compacts landing /compacts',
  [['code', codeSize(closure([entryKey, keys.aibLayout, keys.aibIndex].filter(Boolean)))], ['font', fontSize], ['compacts manifest + document', aibManifestSize + aibDocSize]],
  LIMITS.aibLanding,
  aibNote,
);
figure(
  'Compact view',
  [['code', codeSize(closure([entryKey, keys.aibLayout, keys.aibCompact].filter(Boolean)))], ['font', fontSize], ['compacts manifest + document', aibManifestSize + aibDocSize]],
  LIMITS.aibCompact,
  aibNote,
);
const aibData = aibManifestSize + aibDocSize;
if (aibManifest) figure('Compacts data', [['manifest + document', aibData]], LIMITS.aibData);
figure('All data', [['Strategic', strategicData], ['Sector', sectorData], ['Country', countryData], ['Compacts', aibData]], LIMITS.allData);

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
if (countryManifest) {
  const sheets = countrySheets.map(([, n]) => n).sort((a, b) => a - b);
  const largest = (k) => size(maxOf(countryMemos.map((m) => m[k])));
  console.log(
    `  data/country/: ${countryFiles.length} files, ${size(countryData)}; ${sheets.length} fact sheets (largest ${size(maxSheet)}, ` +
      `median ${size(sheets[Math.floor(sheets.length / 2)] ?? 0)}); ${countryMemos.length} memos (largest JSON ${largest('json')}, PDF ${largest('pdf')}, DOCX ${largest('docx')})`,
  );
} else console.log('  (public/data/country/manifest.json not found: export the Country bundle with scripts/88)');
if (aibManifest) console.log(`  data/aib/: manifest ${size(aibManifestSize)}, ${aibManifest.files.compacts} ${size(aibDocSize)}`);
else console.log('  (public/data/aib/manifest.json not found: export the Sectoral Compacts bundle with scripts/84)');
console.log('');
console.log('Figures');
for (const f of figures) {
  const status = f.total > f.limit ? 'FAIL' : 'ok  ';
  console.log(`  ${status} ${f.name.padEnd(30)} ${size(f.total).padStart(10)}  (limit ${size(f.limit)})${f.note ? `  ${f.note}` : ''}`);
  for (const [label, n] of f.parts) console.log(`         ${label.padEnd(32)} ${size(n).padStart(10)}`);
}
console.log('');
console.log(`app shell JS + CSS (entry): ${kb(app)}  (target ${kb(TARGET_APP)})`);
// the Sectoral Compacts route chunks beyond the entry (shared chunks such as Plot counted once per route)
const aibRoutes = [['AibLayout', keys.aibLayout], ['compacts index view', keys.aibIndex], ['compact view', keys.aibCompact], ['about view', keys.aibAbout]].filter(([, k]) => k);
if (aibRoutes.length) {
  const own = (k) => codeSize(new Set([vm[k].file, ...(vm[k].css ?? [])]));
  const beyond = (k) => codeSize(new Set([...closure([k])].filter((f) => !entryFiles.has(f))));
  console.log(`Sectoral Compacts route chunks: ${aibRoutes.map(([n, k]) => `${n} ${kb(own(k))} (with its imports ${kb(beyond(k))})`).join('; ')}`);
}
if (app > TARGET_APP) console.warn(`budget: warning, the app's entry JS + CSS (${kb(app)}) is above the ${kb(TARGET_APP)} target`);
if (!keys.sectorLayout) console.log('budget: note, no SectorLayout chunk in this build (the Sector module is a stub or eagerly bundled): the Sector figures count the entry only');
if (failed) process.exit(1);
console.log('budget: OK');
