#!/usr/bin/env node
// Prepare a Draw the Fence build for GitHub Pages (a project site served under /<repo>/).
//
//   VITE_BASE=/draw-the-fence/ npm run build:fence
//   node scripts/prepare-fence-pages.mjs <out-dir>
//
// Copies fence/dist/ to <out-dir> and makes it Pages-ready:
//   * 404.html = index.html, so a reload of a deep link (/draw-the-fence/result?a=...) boots the app;
//   * .nojekyll, so Pages serves files as they are;
//   * a robots noindex tag and robots.txt (the site is shared by link, not listed in search);
//   * no source maps.
// It refuses to run on a build whose base is "/" (assets would load from the wrong path).
import { cpSync, existsSync, readFileSync, readdirSync, rmSync, statSync, writeFileSync } from 'node:fs';
import { dirname, join, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';

const here = dirname(fileURLToPath(import.meta.url));
const dist = resolve(here, '..', 'fence', 'dist');
const out = process.argv[2] ? resolve(process.argv[2]) : null;
if (!out) {
  console.error('usage: node scripts/prepare-fence-pages.mjs <out-dir>');
  process.exit(2);
}
if (!existsSync(join(dist, 'index.html'))) {
  console.error(`no build at ${dist}: run VITE_BASE=/<repo>/ npm run build:fence first`);
  process.exit(1);
}
const index = readFileSync(join(dist, 'index.html'), 'utf8');
const base = index.match(/src="(\/[^"]*?)assets\//)?.[1];
if (!base || base === '/') {
  console.error('the build was made with base "/"; rebuild with VITE_BASE=/<repo>/ for a project site');
  process.exit(1);
}
// Git Bash rewrites "/repo/" into a Windows path unless MSYS_NO_PATHCONV=1: catch it here
if (!/^\/[A-Za-z0-9._-]+\/$/.test(base)) {
  console.error(`unexpected base "${base}": expected /<repo>/ (in Git Bash, set MSYS_NO_PATHCONV=1 when building)`);
  process.exit(1);
}
if (!existsSync(join(dist, 'data', 'yards-fences', 'manifest.json'))) {
  console.error('the build has no data/yards-fences/manifest.json: run scripts/89_export_yards_fences_data.R first');
  process.exit(1);
}

// keep a .git directory in <out-dir> (the Pages repo), replace everything else
if (existsSync(out)) for (const f of readdirSync(out)) if (f !== '.git') rmSync(join(out, f), { recursive: true, force: true });
cpSync(dist, out, { recursive: true });

const removeMaps = (dir) => {
  for (const f of readdirSync(dir)) {
    const p = join(dir, f);
    if (statSync(p).isDirectory()) removeMaps(p);
    else if (f.endsWith('.map')) rmSync(p);
  }
};
removeMaps(out);

const html = index
  .replace('<meta charset="UTF-8" />', '<meta charset="UTF-8" />\n    <meta name="robots" content="noindex, nofollow" />')
  .replace(/\n\/\/# sourceMappingURL=.*$/m, '');
writeFileSync(join(out, 'index.html'), html);
writeFileSync(join(out, '404.html'), html);
writeFileSync(join(out, '.nojekyll'), '');
writeFileSync(join(out, 'robots.txt'), 'User-agent: *\nDisallow: /\n');
writeFileSync(
  join(out, 'README.md'),
  `# Draw the Fence\n\nThe built Draw the Fence site (NEIS Center, Yards & Fences decision tool), served by GitHub Pages at\n\`${base}\`. This repository holds build output only: the source is \`dashboard/src/fence/\` in\nOpportunity_Security_Indices (branch \`feat/yards-fences\`). Rebuild with \`VITE_BASE=${base} npm run build:fence\`\nand \`node scripts/prepare-fence-pages.mjs <this-folder>\`, then commit and push.\n`,
);
console.log(`prepared ${out} for GitHub Pages under ${base}`);
