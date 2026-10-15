# Draw the Fence

A standalone NEIS Center site: the Yards & Fences decision tool. It is separate from the Allied
Industrial Base Explorer but built from the same package (`dashboard/`), so it shares the Explorer's
design tokens, UI primitives, fonts and data pipeline.

```bash
cd dashboard
npm run dev:fence      # dev server
npm run build:fence    # fence/dist/
```

* Entry: `fence/index.html`; code: `src/fence/`; build config: `vite.fence.config.ts`.
* Data: `fence/public/data/yards-fences/`, written by `Rscript scripts/89_export_yards_fences_data.R`
  (after scripts/85 and 87). Without it the site shows "No Yards & Fences data".
* Deploy: a Netlify site with Base directory `dashboard/fence` (see `netlify.toml`).

Method, data contract, tests and decisions: `docs/yards_fences.md`.
