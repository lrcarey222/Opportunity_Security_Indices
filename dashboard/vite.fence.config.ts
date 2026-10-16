// Draw the Fence: a standalone site, built from this package so it shares the Explorer's design
// tokens, UI primitives, fonts and data without copying them.
//
//   npm run dev:fence      dev server (root: fence/, code: src/fence/)
//   npm run build:fence    fence/dist/ (its own deployable site; see fence/netlify.toml)
//
// The Allied Industrial Base Explorer neither imports nor links to it.
import { defineConfig } from 'vite';
import react from '@vitejs/plugin-react';
import tailwindcss from '@tailwindcss/vite';
import { fileURLToPath } from 'node:url';

const here = fileURLToPath(new URL('.', import.meta.url));

export default defineConfig({
  root: 'fence',
  base: process.env.VITE_BASE ?? '/',
  publicDir: 'public',
  plugins: [react(), tailwindcss()],
  // the app code lives in src/fence (outside the fence/ root), next to the shared design system:
  // `/src/...` in fence/index.html resolves to this package's src/
  resolve: { alias: [{ find: /^\/src\/(.*)$/, replacement: `${here}src/$1` }] },
  server: { fs: { allow: [here] } },
  build: { outDir: 'dist', emptyOutDir: true, target: 'es2022', sourcemap: true },
});
