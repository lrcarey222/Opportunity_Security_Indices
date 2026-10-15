import { defineConfig, type Plugin, type ViteDevServer } from 'vite';
import react from '@vitejs/plugin-react';
import tailwindcss from '@tailwindcss/vite';
import { createReadStream, existsSync, statSync } from 'node:fs';
import { join, normalize, resolve, sep } from 'node:path';
import { fileURLToPath } from 'node:url';

const here = fileURLToPath(new URL('.', import.meta.url));

/** Serve the files of `root` under the URL prefix `prefix` (404 for anything else). */
function serveDir(server: ViteDevServer, prefix: string, root: string): void {
  server.middlewares.use(prefix, (req, res, next) => {
    const rel = decodeURIComponent((req.url ?? '/').split('?')[0]).replace(/^\/+/, '');
    const file = normalize(join(root, rel));
    if (!file.startsWith(root + sep) || !existsSync(file) || !statSync(file).isFile()) {
      res.statusCode = 404;
      res.end('not found');
      return;
    }
    const types: Record<string, string> = {
      '.json': 'application/json',
      '.pdf': 'application/pdf',
      '.docx': 'application/vnd.openxmlformats-officedocument.wordprocessingml.document',
      '.md': 'text/markdown; charset=utf-8',
    };
    res.setHeader('Content-Type', types[file.slice(file.lastIndexOf('.'))] ?? 'application/octet-stream');
    res.setHeader('Cache-Control', 'no-cache');
    createReadStream(file).pipe(res);
    void next;
  });
}

// `npm run dev:fixture`: serve the committed synthetic bundles instead of public/data/, so the app
// runs without the real exports: the Strategic one (scripts/86) at <base>data/ and the Sector one
// (scripts/87 --synthetic, tests/fixtures/sector/synthetic/) at <base>data/sector/, and the Country one
// (scripts/88 --synthetic, src/country/__fixtures__/synthetic/data/country/) at <base>data/country/.
function fixtureData(): Plugin {
  const root = resolve(here, 'src/engine/__fixtures__/synthetic/s1/data');
  const sectorRoot = resolve(here, 'tests/fixtures/sector/synthetic');
  const countryRoot = resolve(here, 'src/country/__fixtures__/synthetic/data/country');
  return {
    name: 'osi-fixture-data',
    apply: 'serve',
    configureServer(server) {
      const base = server.config.base.endsWith('/') ? server.config.base : `${server.config.base}/`;
      // the more specific prefix first: connect matches mounts in order
      serveDir(server, `${base}data/sector`, sectorRoot);
      serveDir(server, `${base}data/country`, countryRoot);
      serveDir(server, `${base}data`, root);
    },
  };
}

export default defineConfig(({ mode }) => ({
  base: process.env.VITE_BASE ?? '/',
  plugins: [react(), tailwindcss(), ...(mode === 'fixture' ? [fixtureData()] : [])],
  worker: { format: 'es' },
  // the world map's TopoJSON (~0.76 MB, 0.23 MB gz) is its own lazy chunk; nothing else comes close
  build: { target: 'es2022', sourcemap: true, manifest: true, chunkSizeWarningLimit: 800 },
}));
