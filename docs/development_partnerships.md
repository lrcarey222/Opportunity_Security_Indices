# Development partnerships

The development arm of the Allied Industrial Base (AIB), added 2026-11-03. A development partnership pairs a
real supply-chain or allied-capacity need in a high-income economy with a developing economy's credible path
to the same productive capability. The aim is that development cooperation creates trusted capacity the
allied system needs, and durable capability in the partner economy. It is not a way to buy supply abroad.

- Rules: `R/dashboard/country/country_devpart.R`, `config/development_partnerships.yml`
- Export: `scripts/93_export_development_partnerships.R`
- Dashboard: `dashboard/src/country/devpart.ts`, `views/profile/DevelopmentPartnerships.tsx`
- Brief: `R/dashboard/country/country_brief_dev.R`, plus the development section of `config/country_brief.yml`

The analysis has no composite score. Each pair carries its reasons (the donor reason, the recipient reason,
the relationship ties and the tool), and pairs are listed in a fixed, lexicographic order.

## Unit and tests

The unit is a donor × recipient × technology × supply-chain step (the posture engine's 66 steps). A pair
needs evidence on both sides.

### Donor test

The donor is a high-income economy in the AIB demand scope (`aib_host_screen.csv` `in_demand_scope`, else the
Core or Partner tier).

A step qualifies on any one of these signals:

| Signal | Source |
|---|---|
| Partner | posture choice |
| Secure with allied or mixed delivery | posture detail `delivery` |
| High fence with few trusted alternatives | Secure, and the `trusted_alternatives` need family at or above the high-need bar |
| High supply-chain need | need ≥ the engine's bar, `meta.thresholds.need` = 0.67 |
| Allied supply falls short | posture detail `partner_aib_status = gap` |
| The AIB needs new capacity | a BUILD node of the step, via AIB `nodes[].yfCells` |

A Watch step with none of these signals, or economic attractiveness alone, is never a reason.

The donor tier orders the reasons:

| Tier | Reasons |
|---|---|
| 1 | a choice-based reason, and allied supply falls short |
| 2 | a choice-based reason and a BUILD node |
| 3 | a choice-based reason only |
| 4 | high need only |
| 5 | allied reasons only |

A low-confidence, flagged or known-artifact donor call cannot carry a national reason (tiers 1–4). Its national
signals are dropped (`donor_national_dropped`). If it has an allied reason, the pair goes on at tier 5, since
allied reasons do not rest on the donor's own call. Without one, the pair is a review case.

### Recipient test

The recipient is a non-high-income economy, and its same step must be Catch Up:
- with the engine's credible pathway: a capability anchor plus corroboration from another evidence family
  (`R/strategic_position/pathway.R`);
- corroborated by more than context signals. A pathway that only "industrial learning" corroborates, which the
  engine itself calls "context rather than capability", is a *thin anchor*;
- with evidence high or medium and confidence high or moderate;
- with no review flag and no known data artifact;
- inside the AIB trust screen (UNGA bloc rule, `aib_host_screen.csv`).

A Preserve recipient is not tested for a pathway: it is already competitive. It still takes the trust,
capability, evidence, confidence, review-flag and artifact tests, and failing one makes the pair a review
case, not a commercial one.

**Capability checks** (`capability_checks`, from the 2026-11-03 review). On some steps the donor's need and the
recipient's anchor measure different products. There a Catch Up or Preserve counts only when its anchor
measures the product the need is about. Otherwise the pair is a review case ("capability unverified"), and its
caveat says what the anchor measures instead.

| Step | The need is about | Check |
|---|---|---|
| EV refining | magnet rare earths (trade basket) | an export-share anchor; battery-metal capacity data do not count |
| Wind refining | rare earths (the BUILD nodes) | the recipient's share of the rare-earth codes, its EV refining trade share, at the engine's 0.5% bar; copper wire rod and mining of other minerals do not count |
| Nuclear refining | conversion and enrichment (HS 2844.20) | an export-share anchor; the Atlas uranium-compound share does not count (Namibia joins Australia here) |
| Batteries refining | refined battery materials | a supply- or export-share anchor; GCIM critical-minerals capacity, which includes mines, does not count alone |
| Nuclear components and assembly | reactor and fuel parts | unverifiable: the baskets mix them with boiler and steam-turbine parts, so every pair is a review case |

### Route

- **Development**: the recipient passes the test.
- **Commercial** (additionality): the recipient already hosts the step's BUILD capacity in the AIB, or it
  Preserves the step. The tools are trade, offtake, private investment, export finance, development finance
  and guarantees. There are no grants.
- **Review**: the recipient fails on evidence (thin anchor, capability unverified, evidence, confidence,
  flag, artifact), or the donor's call needs review.
- **Excluded**: the recipient fails a hard rule (income, not Catch Up, trust, pathway, anchor). These pairs
  are kept in the export for the audit.

### Tools

Tools follow the stage (the families in the config) and the recipient's observed constraints. A constraint is
a pathway signal the recipient lacks, but only where the engine observed its input (the `pw_*` values of
`strategic_position_tbl.csv`, joined by scripts/93). An input the data do not cover goes in
`recipient_unobserved` ("not covered by the data"). It is never stated as a constraint and never leads the
tools.

| Constraint | Meaning | Raised only when |
|---|---|---|
| `no_capacity` | no operating or committed plant in the capacity data | both capacity shares are observed and 0 |
| `small_capacity` | operating or committed plant below the capacity bar | a capacity share is above 0 |
| `not_operating` | committed projects that are not yet operating | operating capacity is observed |
| `know_how` | feasibility outside the top quarter | feasibility is observed |
| `input_costs` | input costs outside the cheapest quarter | input costs are observed |
| `trade_only` | a trade or output share with no plant data behind it | the capacity data cover the step |

Upper-middle-income recipients lead with development finance, guarantees, export finance and joint ventures.
Public support is limited to public goods. Lower-income recipients get project preparation, infrastructure
and skills first, then commercial instruments at scale.

### Order

The order is lexicographic, with no weights:
1. route;
2. donor tier;
3. an anchor measured on the step's own supply or export share;
4. recipient confidence, then evidence;
5. number of anchor families;
6. number of relationship ties;
7. lower income first;
8. Competitive Position.

## Relationship evidence

Relationship evidence describes and orders the pairs; it never makes or breaks one. Missing data stay
missing.

- **OECD CRS aid** (`oecd_crs_purpose.csv` with the six-stage purpose crosswalk; ODA only, 2020–2024
  disbursements, constant USD):
  - Aid at a step is aid in the codes the crosswalk files under that step for the technology, including
    pooled stage-wide codes.
  - Specificity says where most of the money is (`aid.technology_share_min`, 0.5):
    - *technology*: codes naming only that technology hold that share;
    - *shared technology*: those codes plus codes naming it with others do;
    - *proxy*: an approximate code does. 32172, transport equipment, is mapped to EVs. It is reported as
      `oda_proxy` and never as technology-coded. The shared crosswalk is unchanged, because Development
      Potential reads it too.
    - *stage*: otherwise (the technology is not identified).

    The export carries `oda_named`, `oda_shared`, `oda_proxy` and `oda_pooled`. Audit check 5 tests the label
    against them.
  - The current-aid comparison counts a pair as *aligned* (or, on the commercial route, *graduate*) only when
    the aid at the step is technology-coded. Stage-wide or proxy aid is its own class, *stage_wide*.
  - Amounts are totals over the 2020–2024 window, not annual means.
  - Components and Assembly share every code, so the same aid appears at both, labelled.
  - No code funds recycling. Recovery aid is NA, and the Extraction + Refining aid of the technology is
    given as a flagged proxy.
  - A donor that does not report purpose-coded aid has NA, never zero. A reporting donor with no record for
    the pair has an observed zero, labelled "no aid recorded".
- **OCI** export-credit and lender finance (not ODA): per lender, through Development Potential's own
  institution, mechanism and crosswalk rules (`partnership_strength_oci_amounts`). The value is NA at a step
  the OCI crosswalk never maps, and an observed zero only at a mapped step.
- **IMF direct investment**: the Friendshore measure (reported official data). Where the investor reports
  none, the position derived from the counterpart's reports is used, flagged.
- **CM-FDI factory projects** at Components and Assembly. The database does not separate these two stages.
  - Only the projects the table itself counts are included (`index_use`). R&D, logistics, paused and ceased
    sites are not.
  - The value is NA for technologies the table does not cover: it covers batteries, EVs, solar and wind.
  - Capital is NA when no project discloses it, and `cm_fdi_capital_disclosed` gives the number that do.
- **AIB**: compact roles from the brief's step-to-compact mapping, and simulated investment pairings. The brief
  and the dashboard cite a role only in the compact of the pair's own step, by its name.
- **Deployment demand** (context only): each side's Deployment choice for the technology. `demand_anchor` is
  allied, local or both where that choice is Accelerate on a call that is not low confidence. It never enters
  the tier, the route or the order.
- **Legacy Development Potential**: shown as context ("Development Potential — supporting indicator"). A test
  checks that it changes no pair, route or rank.

## Outputs

- `data/processed/development_partnerships/development_partnerships.{csv,rds}`: every pair considered, with all
  the spec's fields.
- `development_partnership_steps.csv`: every step with its donor signals and recipient tests.
- `development_partnerships_<ISO3>.csv` and `_audit.csv` (`--audit <ISO3>`): the eight checks per development
  pair.
- `dashboard/public/data/country/devpart-<ISO3>.<h8>.json`: one per economy with a perspective, listed as
  `devpart` in the Country manifest entry. scripts/88 carries the entry while the file exists.

Run order: scripts/88 → 91 → **93** → 92 (the brief reads the devpart file).

## Data limitations

- CRS purpose codes mostly name a stage, not a clean-energy technology. Components and Assembly cannot be
  separated, and recycling has no code.
- Australia reports no outward IMF positions under the Friendshore measure, so its ties are partner-reported.
- CM-FDI does not distinguish Components from Assembly or exact products.
- Capacity anchors (GCIM) cover the "most material product" of a step. On mixed steps this can be a different
  product from the traded one. GCIM also routes every critical-minerals facility, mines included, to Refining.
  The five capability checks cover the cases the review found; other steps may need one.
- **Market tie.** The donor's imports from the recipient in the step's basket (spec 13, "market / offtake
  connection") are not yet a pair field. This needs the bilateral trade matrix in scripts/93.
- The bankability test uses the AIB capacity-host lists as a proxy. There is no direct measure of commercial
  bankability.
- Matching is at the step level. Layer-level choices (for example magnets inside EV components) are not
  matched yet.
