import { defineConfig } from 'vitest/config';

// Projects (spec_react.md 10.3; spec_sector.md 10): unit (engine numerics, codec, worker host, layers on
// hand-made cases; the UI's pure derivations: URL state, Show modes, chart helpers, formats, CSV; the
// Sector contract, derivations and formats), ui (jsdom; the app shell, every view, the Sector views
// and chart kit), parity-synthetic (committed fixtures, CI), parity-real (local real fixtures from
// scripts/86; skipped when absent) and sector-real (the integrator's parity of the decoded real Sector
// bundle with Appendix B and meta.summary; skipped when public/data/sector/manifest.json is absent) and
// country-real (the real Country bundle: contract on every sheet, values against the Strategic chunks, memo files).
export default defineConfig({
  test: {
    passWithNoTests: true,
    projects: [
      {
        test: {
          name: 'unit',
          environment: 'node',
          include: ['src/**/*.test.ts'],
          exclude: ['src/**/*.parity.test.ts', 'src/**/*.sector.test.ts', 'src/**/*.country.test.ts', 'node_modules/**'],
          testTimeout: 60_000,
        },
      },
      {
        // the React 17+ JSX runtime, as @vitejs/plugin-react compiles it for the app
        esbuild: { jsx: 'automatic' },
        test: {
          name: 'ui',
          environment: 'jsdom',
          include: ['src/**/*.test.tsx'],
          testTimeout: 60_000,
          hookTimeout: 60_000,
        },
      },
      {
        test: {
          name: 'parity-synthetic',
          environment: 'node',
          pool: 'forks',
          include: ['src/engine/__tests__/parity/synthetic.parity.test.ts'],
          testTimeout: 120_000,
        },
      },
      {
        test: {
          name: 'parity-real',
          environment: 'node',
          pool: 'forks',
          include: ['src/engine/__tests__/parity/real.parity.test.ts'],
          testTimeout: 600_000,
          hookTimeout: 600_000,
        },
      },
      {
        test: {
          name: 'sector-real',
          environment: 'node',
          pool: 'forks',
          include: ['src/sector/__tests__/real.sector.test.ts'],
          testTimeout: 600_000,
          hookTimeout: 600_000,
        },
      },
      {
        // the Country builder's checks of the real Country bundle (public/data/country/; skipped without it)
        test: {
          name: 'country-real',
          environment: 'node',
          pool: 'forks',
          include: ['src/country/__tests__/real.country.test.ts'],
          env: { COUNTRY_REAL: '1' },
          testTimeout: 600_000,
          hookTimeout: 600_000,
        },
      },
    ],
  },
});
