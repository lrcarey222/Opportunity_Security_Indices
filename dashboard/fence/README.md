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
* Deploy: a Netlify site with Base directory `dashboard/fence` (see `netlify.toml`), or GitHub Pages
  (below).

Method, data contract, tests and decisions: `docs/yards_fences.md`.

## Publish to GitHub Pages

The team link is <https://lrcarey222.github.io/draw-the-fence/>: public to anyone with the link, and
tagged `noindex` so search engines leave it alone. The `lrcarey222/draw-the-fence` repository holds
build output only. To update it after a change (Git Bash; `MSYS_NO_PATHCONV=1` stops Git Bash
rewriting the base path into a Windows path):

```bash
cd dashboard
MSYS_NO_PATHCONV=1 VITE_BASE=/draw-the-fence/ npm run build:fence
node scripts/prepare-fence-pages.mjs ../../fence-pages   # a clone of lrcarey222/draw-the-fence
cd ../../fence-pages && git add -A && git commit -m "Update Draw the Fence" && git push
```

`scripts/prepare-fence-pages.mjs` adds `404.html` (so deep links such as `/result?a=...` survive a
reload), `.nojekyll`, the noindex tag and `robots.txt`, drops source maps, and refuses a build made
with the wrong base.
