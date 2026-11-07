# Country strategy briefs

"[Country] and the Allied Industrial Base — What to secure, build, source from partners, and deploy".
These briefs replace the Country Insights policy memos, which reported the Energy Security, Economic
Opportunity and Strategic Index scores. A brief is a short strategy document (about 2,000 words, five to
seven pages plus a technical appendix). It is built on the Country page's current method: the Yards &
Fences choices, Strategic Industrial Value, Competitive Position, supply-chain and deployment need, and
policy alignment. It also draws on the Allied Industrial Base (AIB) roles and compacts.

**Status (2026-11-06):** nine briefs are published:
- Australia, the pilot, approved after review;
- Japan, Korea, India and the United States, on 2026-11-04;
- Germany, the United Kingdom, France and Canada, on 2026-11-05.

The legacy memos stay hidden. The pilot report has the implementation notes and open questions:
[country_briefs_pilot_report.md](country_briefs_pilot_report.md).

## Structure

The structure follows the user's full specification of 2026-11-02.

1. **Bottom line — [Country]'s strategic role.** A thesis of 3 to 5 sentences, then the choice box (Secure,
   Build / preserve, Partner, Deploy / stay open) and Figure 1, the Strategy Map.
2. **Where [Country] should secure, compete and stay open.**
   - The High fence, with a table of 3 to 5 capabilities.
   - The Medium fence: Preserve, Catch Up and Partner, with Figure 2 (Strategic Industrial Value against
     Competitive Position).
   - The Low fence: what not to prioritize, 3 to 5 cases.
3. **[Country]'s allied bargain.** The bargain table and Figure 3, the compact × role matrix.
4. **Development strategy** (added 2026-11-03), titled by perspective:
   - a donor (high income, in the AIB demand scope) gets "Development strategy — build trusted capacity by
     helping partners Catch Up";
   - a developing economy gets "Development partnerships — what support could help [Country] Catch Up?".

   The section holds the generated development table of 3 to 5 partnerships (`@table development`; the
   inverse columns for a recipient), the commercial and review cases, and current aid against the pairs. A
   donor with no credible pair must say "No credible development-led supply solution identified." (lint).
   The method is in [development_partnerships.md](development_partnerships.md).
5. **Energy-system priorities.** Figure 4, the deployment ranking (conditional). Review cases sit apart.
6. **Is current policy pointed in the right direction?** The generated strategy–policy gap table.
7. **Strategic agenda — what should [Country] do next?** 4 to 6 moves, one of them development where credible
   pairs exist.
8. **Appendix — evidence and method.** Rules, caveats, and generated tables:
   - review cases
   - development partnerships by step
   - every step
   - layers
   - partner recommendations
   - compact roles
   - coverage

## Template rules added 2026-11-03

- **Evidence gating** (`country_brief_review()`). A call is a *review case* when it has:
  - low confidence;
  - an engine review flag;
  - a Catch Up whose pathway only context signals corroborate (a thin anchor);
  - a known data artifact (`config/development_partnerships.yml` `review_cells`);
  - at deployment, a single indicator of low confidence.

  Review cases never enter the Bottom line, the Agenda (the lint fails on a `cells.<review key>` token
  there), the policy-gap table, the low-fence list or a chart's headline marks. They are marked instead:
  - Figure 1: a dotted outline;
  - Figure 2: hollow and unlabelled;
  - Figure 4: a separate grey group.

  They are listed in the appendix as "Review / insufficient evidence".
- **National choice and AIB role apart.** The `@table shared_table` in section 2 names each physical
  capability (AIB node) that sits in several steps with conflicting choices. It shows the steps, the
  national reading and, apart from them, what allies rely on the economy for.
- **Provenance.** The title area carries a quiet line: "Generated from Allied Industrial Base Explorer data;
  evidence, sources and caveats in the appendix. Data as of …". The AI disclosure closes the brief, on
  screen, in the PDF and in Word.
- **Length.** The main text is about 8 to 9 pages, then the appendix. The PDF check allows 4 to 16 pages.

Figures and generated tables are first-class objects of the document. `@figure <id>` and `@table <id>` in
the source are built from the facts, and a generated table's cells are facts, so they carry claims. The
writer's source supplies the judgments and the prose.

## Pipeline

```
Rscript scripts/92_export_country_briefs.R --iso3 AUS --facts-only     # brief.facts.json for the writer
Rscript scripts/92_export_country_briefs.R --iso3 AUS --lint-only      # the writer's loop
Rscript scripts/92_export_country_briefs.R --iso3 AUS --preview <dir>  # review copies, unverified
Rscript scripts/92_export_country_briefs.R --iso3 AUS                  # verify, render, publish
```

Run it after scripts/84, 88, 91 and 93. Its inputs are the published bundles: `dashboard/public/data/posture`,
`dashboard/public/data/aib`, the Country manifest and the economy's development-partnership file
(`devpart-<ISO3>`, scripts/93). A brief therefore says what the dashboard shows.

The script writes these files into `dashboard/country-memos/<ISO3>/`:
- `brief.facts.json`: every citable fact, as `{v, d, src, m}` (value, display, sources, meaning).
- `brief.lint.json`
- `brief.claims.json`: one claim per token.
- `brief.ast.json` and `brief.print.html`: audit copies.

The writer's file is `brief.src.md`. The independent verifier's file is `brief.verify.json`: every claim
and sentence with its verdict, plus the hashes. Publication requires a verifier record that passes.

Publication writes these files into `dashboard/public/data/country`:
- `memo-<ISO3>.<h8>.{json,pdf,docx,md}`
- the figures, `memo-<ISO3>-f<n>.<h8>.{svg,png}`

It also writes the manifest's memo entry, with `format: "brief"`. scripts/88 keeps brief entries as they
are. The code lives in these files:

| File | What it does |
|---|---|
| `R/dashboard/country/country_brief.R` | facts, High-fence groups, AIB facts |
| `R/dashboard/country/country_brief_doc.R` | grammar, lint, claims, document, HTML, Markdown |
| `R/dashboard/country/country_brief_charts.R` | the figures |
| `R/dashboard/country/country_brief_dev.R` | evidence gating, shared capabilities, the development section's facts and tables |
| `R/dashboard/country/country_devpart.R` | the development-partnership tests and pairs (scripts/93) |
| `scripts/utils/country_memo_docx.py` | Word builder, read-back check |
| `dashboard/src/country/memo/MemoDocument.tsx` | screen rendering |
| `dashboard/src/country/contract.ts` | `checkMemoDoc`, format `brief` |

## Writer's rules

### Numbers and names

- Every number is a `{{token}}`. Lint rejects any digit outside a token, except a figure or table number.
- Economy names should come from tokens (partners, suppliers, compact cores). Lint warns on a plain name.

### Judgments, not score reports

Scores belong in the appendix. The body states the choice and why.

### Secure

Never write Secure as self-sufficiency. The answer is qualified trusted supply, stocks, redundancy or
control of the command path, delivered at home, with allies, diversified, or mixed.

### High fence

Choose capabilities by an acute problem:
- control-system risk
- a coercive chokepoint
- system resilience

Do not choose them by need score alone. The config groups Secure steps and layers into capabilities
(`high_fence_groups`).

### Catch Up and other choices

- A Catch Up needs an anchor.
- Say so when the anchor is thin or inherited, and do not build the strategy on it. Examples from the
  pilot: uranium conversion, which rests on a 1% trade share; electrical steel, which inherits its step's
  anchor.
- Low-confidence and single-indicator choices are named as such. An example is oil use read as
  Accelerate.

### Reading the data

- **Policy alignment** compares a choice with recorded measures within the country. It is not a level of
  protection.
- **Partner lists** show where allied capacity sits, not who owns it.
- **AIB scenario names** come from the bundle: Managed Interdependence is the default, and Decoupling is
  the other scenario cited.

### Style

US spelling. Never write "OSI", "Legacy", "paper baseline", "NEIS baseline", or upstream, midstream or
downstream (VOCABULARY.md).

## Open questions for the pilot review

1. **Sections 3-6.** Are the section titles and their order right? The specification ends at Chart 2.
2. **Subtitle.** It is kept as given.
3. **Label.** It now reads "...; data as of [date]." instead of "NEIS baseline as of". The dashboard checks
   briefs against `BRIEF_LABEL_PREFIX`.
4. **Context from outside the data.** Some points the analysis cannot see would change a judgment. One
   example is Australia's legal prohibition of enrichment plants, which the reviewer raised and which
   needs a source. The brief now says that such cases "depend on settings this analysis does not score".
   Should briefs allow a short, sourced context note?
5. **Print colours.** Figure 1 shows Watch in mid grey rather than the dashboard's dark grey. The corner
   flags of mixed steps use violet for the medium fence, so they are not read as Preserve blue.
