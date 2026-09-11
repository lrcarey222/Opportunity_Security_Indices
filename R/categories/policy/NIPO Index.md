# NIPO Policy Index (Domestic Intervention Score)

This repository/script constructs a **Domestic Intervention Score (DIS)** from the **NIPO (GTA-derived)** policy inventory. DIS is intended to represent the **strength of policy support / intervention** affecting a technology and value-chain stage.

The goal is to translate heterogeneous policy events (subsidies, tariffs, procurement rules, localisation, FDI rules, etc.) into a **comparable strength measure**, then aggregate it into sectoral and time-based indicators (e.g., **country × tech × supply_chain**, and **country × tech × supply_chain × announce_year with a rolling 3-year window**).

DIS is **not a welfare measure** and it is **not “good” or “bad” by definition**. It’s a structured proxy for the **intensity and scale** of state intervention/support.

---

## Inputs

### 1) `raw_nipo` (NIPO export)
Expected columns include (as in your `names(nipo_raw2)`):

- IDs: `State Act ID`, `Entry ID`, `Title`, `URL`
- Geography: `Implementing Jurisdiction`, `Level of Government Implementation`, `Affected Jurisdiction`
- Status: `Initial Assessment (Change Relative to 1 Jan 2009)`
- Trade flow: `Affected Trade Flow` (values observed: `inward`, `outward`, `outward subsidy`)
- Dates: `Announcement Date`, `Implementation Date`, `Removal Date`
- Product/Sector tags: `Product: HS 6-digit (2022)`, `Sector: CPC 3-digit (v2.1)`
- Policy family booleans: `Is Subsidy`, `Is Import Policy`, `Is Export Policy`, `Is FDI Policy`, `Is Procurement Policy`, `Is Localisation Policy`, `Is Trade Defence`, `Is Other Policy`, etc.
- Sector validation flags:  
  `Sector: Low Carbon Technology`, `Sector: Dual-Use Products`, `Sector: Critical Minerals`, `Sector: Advanced Technology Products`
- Scale: `Trade Covered (USD Million)`, `Size of Subsidy (USD Million)`

### 2) HS6 → tech × supply_chain mapping table (`hs6_categories_raw`)
Required columns:
- `Technology` (e.g., Solar, Electric Vehicles, Electric Grid…)
- `Value Chain` (Upstream / Midstream / Downstream)
- `HS6`

Recommended additional columns:
- `hs6_name` (HS6 description)
- `essential_for_tech_sc` (boolean): whether this HS6 code is **diagnostic** for classifying tech×stage.  
  This is used to prevent overly broad matches (e.g., generic plastics/steel/wood) from driving mapping.

### 3) `country_info` (optional but recommended)
A lookup table providing standardized country names and `iso3`, used to normalize `Implementing Jurisdiction` → `iso3`.

---

## Outputs

The function returns a list of tables (names may vary slightly by script version, but conceptually):

1) **`by_policy`**  
   One row per policy (`Entry ID`), including:
   - per-policy strength measures (`bite_strength`, `scale_strength`, packaged versions)
   - list/mapped fields (HS6 codes, CPC codes/names, tech, supply_chain)
   - validation diagnostics (HS6 coverage, CPC agreement, keyword/sector evidence)
   - mapping confidence metrics (see below)

2) **`by_tech_sc`**  
   Aggregated by **country × tech × supply_chain**, summarizing overall domestic intervention strength.

3) **`by_tech_sc_year`**  
   Aggregated by **country × tech × supply_chain × announce_year**, using a **rolling 3-year window** to capture changing priorities over time.

4) Optional: `by_hs6`, `by_cpc`  
   Useful for drilling down to product level or reconciling sector tagging.

---

## Method overview

### Step 1 — Clean and normalize NIPO
- Parse dates (`Announcement Date`, `Implementation Date`, `Removal Date`)
- Parse HS6 and CPC lists into list-columns
- Normalize `Affected Trade Flow` into `flow_norm`:
  - contains “inward” → `inward`
  - contains “outward” → `outward` (includes “outward subsidy”)
  - else → `unknown`
- Normalize status labels into `status_norm` (Distortive/Discriminatory, Liberalising, Neutral, Unclear, Unknown)

### Step 2 — Compute per-policy strength (DIS building blocks)

We separate “strength” into **bite** (how binding) and **scale** (how large/wide).

#### 2.1 Instrument weight (`w_tool`)
A policy’s baseline bite depends on **instrument family/type**.
Weights are derived from:
- `GTA Intervention Type` and/or
- the family booleans (`Is Subsidy`, `Is Procurement Policy`, etc.)

**Design intent:** subsidies, procurement, localisation, FDI policy typically represent **strong intervention**; trade defence and border measures are medium; “other” is lower.

#### 2.2 Status weight (`w_status`) — conditional on trade flow
This is a key methodological choice.

**Flow-conditional liberalising:**
- “Liberalising” should be **negative** if `Affected Trade Flow == inward`  
  (reduces domestic intervention/protection on inward-facing measures)
- “Liberalising” should be **positive** if `Affected Trade Flow == outward`  
  (supports outward expansion / export facilitation)

Example weights:

- Distortive: `+1.00`
- Liberalising + inward: `−0.20`
- Liberalising + outward: `+0.20`
- Neutral: `+0.50`
- Unclear: `+0.30`
- Unknown: `+0.30`

Tune these magnitudes based on your interpretation of “support” vs “intervention.”

#### 2.3 Scope multiplier (`m_scope`)
Captures how economy-wide the measure is:
- Horizontal measures generally score higher than firm-targeted measures.
- `Levels of Policy Intervention` and `Firm: Beneficiary` inform this.

#### 2.4 Duration multiplier (`m_duration`)
Policies that exist longer are more consequential, but duration is capped to avoid domination:
- If `Removal Date` exists, use the duration.
- If not, assume it persists up to a cap.

Use a bounded concave scaling function (e.g., sqrt of months / norm).

#### 2.5 Breadth multiplier (`m_breadth`)
Uses how many products/sectors are covered:
- #HS6 and #CPC codes (log-scaled to p95 and capped)

#### 2.6 Geographic reach multiplier (`m_geo`)
Uses breadth of affected partner jurisdictions (log-scaled, capped).

#### 2.7 Monetary/coverage scale multiplier (`m_scale`)
Uses the larger of:
- `Trade Covered (USD Million)`
- `Size of Subsidy (USD Million)`

Log scaling + cap prevents large subsidies from overwhelming the entire index.

#### 2.8 Package multiplier (`m_package`)
Policies often come in **packages** (same `State Act ID` spanning multiple families).
Apply a capped multiplier increasing with the number of policy families present.

---

## Mapping policies to tech × supply_chain

Policies carry HS6 codes; the HS6 mapping table provides `Technology` and `Value Chain` = Upstream/Midstream/Downstream.

**Important:** mapping can be over-inclusive if generic HS6 codes are included (e.g., plastics, steel).  
That’s why we recommend an `essential_for_tech_sc` filter:

- `TRUE`: HS6 is diagnostic of tech×stage classification
- `FALSE`: HS6 is too generic; keep for context but do not use to drive mapping

---

## Validation and mapping confidence

To avoid letting weak or noisy mappings dominate, compute a **mapping confidence** signal based on multiple evidence sources.

### Evidence channels
1) **HS6 coverage**  
   `mapped_share = (# HS6 matched to mapping) / (# HS6 in policy)`
2) **CPC validation (HS6 ↔ CPC agreement)**  
   Cross-check implied tech×stage against policy CPC tags.
3) **Keyword validation in `Title` and `Source`**  
   Tech and stage keyword dictionaries (e.g., “electrolyser”, “HVDC”, “gigafactory”, “mining”, “installation”).
4) **Binary sector flags**  
   - Low carbon technology
   - Critical minerals
   - Dual-use products
   - Advanced technology products

### How confidence affects the index

**This section previously presented confidence-as-a-strength-multiplier as a deliberate design choice. That was wrong, and the behaviour has changed.**

Mapping confidence is an **epistemic** quantity: it says how sure we are that a policy belongs in a tech × stage cell. It says nothing about how interventionist the policy is. Using it as a multiplier on strength conflated the two, so a well-documented minor measure could outscore a thinly-documented major one. Because documentation richness is not randomly distributed across countries, that injected a systematic bias into a cross-country index.

There was also double counting. `alloc = mapped_share * combo_weight / sum(combo_weight)` already carries `mapped_share`, and `mapping_confidence = 0.75 + 0.75 * mapped_share * evidence_mean` carries it again, so under the old behaviour `mapped_share` entered the product roughly quadratically.

`confidence_mode` now controls this explicitly:

| value | behaviour |
|---|---|
| `"none"` | **default.** `domestic_ts = scale_strength_pkg * alloc`. Confidence does not scale strength. |
| `"filter"` | zero the contribution when `mapping_confidence < conf_threshold` (default 0.75) |
| `"downweight"` | multiply by `pmin(1, mapping_confidence)` — never amplifies |
| `"legacy"` | multiply by `mapping_confidence` as computed, which can exceed 1 |

`mapping_confidence` remains a **reported** column in every mode, as `mapping_confidence_mean` and `mapping_confidence_min` on `by_tech_sc` and `by_tech_sc_year`, and the allocation table carries `confidence_weight` so the epistemic term is always inspectable. `by_policy` reports `mapped_share` and `evidence_mean` so the confidence figure can be decomposed.

#### What the nominal range is, versus the realised one

The constants `CONFIDENCE_FLOOR = 0.25` and `CONFIDENCE_CAP = 2` suggest an 8× dynamic range. The realised range is narrower and differently shaped. For a **mapped** row, `mapped_share > 0` and `evidence_mean >= 1`, so `0.75 + 0.75 * mapped_share * evidence_mean` cannot fall below 0.75: mapped rows occupy `[0.75, 2.00]`, a 2.7× spread. The floor is only reached by rows that bypass the formula entirely — cross-cutting rows are pinned at 0.25 and unmapped rows at 0.10.

So the 8× swing is mostly *cross-cutting versus well-documented mapped*, which is the cross-cutting attribution problem (see `crosscutting_mode`) rather than a property of the confidence formula. The 2.7× spread among mapped rows still rivals the 3.3× spread of `DOMESTIC_FAMILY_WEIGHTS` (0.30 to 1.00), which is why `"none"` is the default.

#### Measured effect

Switching from `"legacy"` to `"none"` changes **levels** substantially but **rankings** very little. Across 20 tech × stage cells the Spearman correlation of country rankings has a median of **0.9974** (minimum 0.9448), and 72 of 1,381 country-cells move more than three rank places. Holding cross-cutting handling fixed, the median rises to **0.9987** and only 18 of 1,062 country-cells move. The reason is that within a cell the multiplier is close to a monotone rescaling across countries, and the published index is a percentile *within* the cell.

See `diagnostics/confidence_sensitivity.csv` and `diagnostics/confidence_sensitivity_nocc.csv`.

---

## Aggregation: country × tech × supply_chain

Aggregate policy contributions to each (country, tech, stage).

### Balancing sum vs average
To reflect both:
- **extensive margin** (lots of policies) → sum
- **intensive margin** (few but very strong policies) → average

Compute both:

- `strength_sum`
- `strength_avg`

Then combine with a stable blend (recommended: log-blend):

`balanced = exp(alpha * log(1 + sum) + (1 - alpha) * log(1 + avg)) - 1`

Where:
- `alpha = 1.0` → pure sum (policy activism)
- `alpha = 0.0` → pure average (typical strength)
- `alpha = 0.5` → equal blend (recommended default)

---

## Time aggregation: rolling 3-year window by announcement year

Construct **country × tech × supply_chain × announce_year** series using:

For each year `Y`:
- include policies with `announce_year ∈ [Y-2, Y]`
- include only if the policy is active during `Y` (implementation/removal overlap)
- optionally weight by fraction of the year active
- aggregate using the same **sum/avg blended** strength measure

This produces a “priorities at time t” series rather than an accumulated stock series.

---

## Subjective parameters (what to tune and why)

These knobs encode your analytical intent; there is no single “correct” setting.

### 1) Instrument weights (type/family weights)
Tune to reflect your view of which instruments are more interventionist (e.g., procurement/localisation vs tariffs).

### 2) Status weights (`w_status`)
Tune magnitudes and keep the **flow-conditional liberalising** logic consistent with your definition of DIS.

### 3) Caps (breadth/geo/scale/package)
Caps prevent dominance by extremely broad or large measures:
- raise caps → more sensitivity to large/broad measures
- lower caps → more equalized index

### 4) Duration normalization & cap
Lower the cap if you want recent priorities to dominate; raise if persistence is central.

### 5) Essential HS6 filtering (`essential_for_tech_sc`)
The strongest lever for reducing false positives from generic inputs.

### 6) Validation/confidence parameters
Increase keyword/sector bonus if text/flags are reliable; increase CPC penalty if CPC tags are high quality.

### 7) Sum vs average blend (`balance_alpha`)
Controls whether you reward volume (sum) or typical strength (avg).

### 8) Rolling window length (`rolling_window_years`)
Longer window smooths noise but blurs priorities; shorter window is more responsive.

---

## Argument reference

All of these are arguments to `nipo_policy_outputs()`. Every one defaults to the **new** behaviour; `dis_legacy_mode = TRUE` restores all of them at once.

| argument | default | legacy | what it controls |
|---|---|---|---|
| `confidence_mode` | `"none"` | `"legacy"` | whether `mapping_confidence` scales strength |
| `conf_threshold` | `0.75` | `0.75` | cut-off for `confidence_mode = "filter"` |
| `include_geo_in_strength` | `FALSE` | `TRUE` | whether `m_geo` enters `scale_strength_base` |
| `strength_constant` | `1` | `2` | flat multiplier on `scale_strength_base` |
| `scale_mode` | `"exposure"` | `"max"` | which monetary term becomes `m_scale` |
| `scope_mode` | `"firm_first"` | `"legacy"` | ordering of the `m_scope` firm test |
| `unclear_status_weight` | `0.30` | `0.30` | weight for amber / likely-distortive |
| `unknown_status_weight` | `NA_real_` | `0.30` | weight for genuinely absent status |
| `neutral_status_weight` | `0.50` | `0.50` | weight for `Neutral` |
| `pctile_singleton_value` | `0.5` | `1` | `*_xcountry_pctile` for a one-country cell |
| `crosscutting_mode` | `"report_only"` | `"uniform"` | how HS6-less policies are attributed |
| `eu_mode` | `"both_flagged"` | `"both_flagged"` | whether either side of the EU split is dropped |
| `clamp_future_as_of` | `TRUE` | `FALSE` | ignore future dates when inferring `as_of_date` |
| `include_neis_panel` | `TRUE` | `TRUE` | build the NEIS framework layers |
| `dis_legacy_mode` | `FALSE` | — | restore every behaviour above at once |

`dis_legacy_mode` **overrides** the individual arguments rather than sitting alongside them. Runs return a `dis_settings` element recording what was actually applied.

### New reported columns

None of these enter any strength product.

**On `policy_base` / `policy_asof` / `by_policy`:**

| column | meaning |
|---|---|
| `m_geo_applied` | the geo multiplier that actually entered the product (1 when excluded) |
| `m_scale_exposure`, `m_scale_fiscal` | the two scale terms, separately |
| `scale_exposure_available`, `scale_fiscal_available` | whether each underlying field was populated |
| `status_missing` | status genuinely absent, as distinct from amber |
| `pending_implementation` | announced but not yet in force |
| `impl_lag_days` | announcement → implementation, `NA` if negative or either date missing |
| `observed_duration_days` | uncensored lifetime, `NA` while still in force |
| `exposure_days` | censored at removal or `as_of_date`, whichever is first |
| `duration_censored` | whether `exposure_days` is right-censored |
| `removal_before_impl` | incoherent timeline (removal recorded before implementation) |
| `mapped_share`, `evidence_mean` | the two inputs to `mapping_confidence` |

**On `by_tech_sc` / `by_tech_sc_year`:**

| column | meaning |
|---|---|
| `mapping_confidence_mean`, `mapping_confidence_min` | confidence, reported not applied |
| `n_policies_status_missing`, `share_policies_status_missing` | missingness denominator for status |
| `n_policies_strength_undefined` | policies whose strength was `NA` and so dropped by `na.rm` |
| `n_countries_in_cell` | size of the `xcountry` comparison group |
| `confidence_mode`, `conf_threshold` | how the score was built |
| `eu_view` | `eu_wide` / `eu_member` / `non_eu` |

**New list element:** `neis_panel` (Objective Structure, Delivery Conversion, Taper Profile, Fence Posture, Inbound Exposure, Rival Direction, Sequencing Position). Existing list elements are unchanged.

## Practical tips

### Memory / performance
Mapping can explode row counts (policy × HS6 × tech × stage). If you hit memory errors:
- filter HS6 mapping to essential codes first
- process `raw_nipo` in country chunks and bind outputs
- avoid returning huge intermediate tables unless debugging

### Diagnostics
Sanity checks that should always be run:
- distribution of `mapped_share`
- share of policies downweighted for low confidence
- sensitivity tests across key knobs (caps, alpha, status weights)

---

## Structural limits

These are properties of the GTA/NIPO dataset, not of the code. No parameter fixes them, and any reading of DIS has to live with them.

### GTA excludes product standards, TBT and SPS

Technical barriers to trade, sanitary and phytosanitary measures, and product standards are **out of scope** for the GTA inventory. For several technologies these are among the most consequential instruments — efficiency standards, grid codes, certification regimes — and DIS is structurally blind to all of them. A country that regulates through standards rather than subsidies will score low for reasons that have nothing to do with its actual interventionism.

### GTA records only unilateral action, so "gates" are unobservable

The inventory covers measures a jurisdiction takes on its own. Bilateral, plurilateral and multilateral **agreed** measures are excluded. In a Gates & Fences framing, this dataset can measure **fences** — unilateral restriction and support — but the **gate** side is not observable in it at all. `neis_fence_posture()` is named accordingly and is documented as measuring fences only. A register of gates has to be maintained separately by hand.

### Subsidy values are incomplete, and not in the way often assumed

`Size of Subsidy (USD Million)` is populated for a minority of policies in every year. It is **not** true that values are systematic only from 2023, nor that the historical extension records none:

| announcement year | 2009 | 2015 | 2019 | 2023 | 2025 |
|---|---|---|---|---|---|
| subsidy populated | 13.2% | 26.0% | **45.1%** | 31.4% | 12.5% |
| trade covered populated | 56.4% | 55.5% | 39.9% | 44.6% | 35.4% |

Coverage **peaks in 2019 and declines into the present**, consistent with reporting lag on recent measures rather than a change in collection regime. Among *active* policies the series is smoother, with trade covered at 44–56% and subsidy at 16–35%, and no break anywhere.

The consequential defect is different and stable: **22–29% of active policies have neither scale field populated** in every year, and `log_mult()` maps `NA → 0 → multiplier 1`. "Unknown scale" is therefore numerically identical to "smallest scale" for roughly a quarter of the stock. `scale_exposure_available` and `scale_fiscal_available` expose this; nothing corrects it. See `diagnostics/scale_coverage_by_year.csv`.

### The status field carries only two values

On the July 2026 export, `Initial Assessment` is only ever `Distortive` (44,108) or `Liberalising` (11,163). There is no amber, `Neutral`, `Unclear` or `Unknown`. The corresponding branches of `w_status` are dead code on this vintage. `unclear_status_weight` and `unknown_status_weight` exist for a future vintage that does carry amber, and a test pins the observed two-value domain so such a vintage fails loudly rather than changing numbers silently.

### The sector flags only ever appear on rows that have HS6 codes

Of the 3,285 policies with no HS6 codes, **zero** carry any of the four `Sector: ...` flags, while 82.6% of HS6-bearing rows do. GTA evidently derives those flags from the product codes. Consequently `crosscutting_mode = "sector_flagged"` is **inert on this data** — exactly equivalent to `"report_only"` — because the corroborating evidence it depends on is definitionally absent from the rows it would apply to.

### The policy-level field has three values, and two regex branches are unreachable

`Levels of Policy Intervention` takes only `Policy or regulation` (34,728), `Firm-specific` (20,188) and `Industrial strategy or plan` (355). In `m_scope`, neither `"economy|cross|horizontal"` nor `"sector|industry"` matches any of them — `"industrial"` does not contain `"industry"` — so both branches match zero rows. They are preserved unchanged because their intent cannot be inferred from behaviour that never occurs. Only three `m_scope` values are reachable: 0.40, 0.75 and 1.00.

Relatedly, `Firm: Beneficiary` is populated on 11,990 rows and **every one of them is already `Firm-specific`**, so it carries no scope information independent of the level field. `Firm: Targeted` (7.3% populated) is unused by the pipeline and is the one field that might add signal, since it captures measures aimed *at* a firm rather than support flowing *to* one.

### `m_duration` carries almost no information

When `Removal Date` is absent, `planned_end` is `impl_date + duration_cap_months`, so `m_duration = min(1, sqrt(60/24)) = 1.0` for **every** in-force measure — all 38,311 of them, 100%. Only measures that have been *removed* can score below 1, and they average 0.70. A taper therefore reads as weakness. `observed_duration_days`, `exposure_days` and `duration_censored` carry the real duration signal instead, and are reported rather than folded into the product.

### One EU act is recorded once per member state

807 State Act IDs span more than one EU member state, up to 26 of them, and **82.9% of all EU-member policy strength sits in such replicated acts**. Deduplicating would cut EU-member strength by roughly 79%.

**This is retained deliberately, not overlooked.** For a country-level index the question is how strong a country's policy toward a capability is, and EU policy that a member state has effectively outsourced to Brussels *is* part of that member's policy stance. The replication is the correct behaviour under that reading. It does mean cross-country comparisons between EU members and non-members are comparing different things: an EU member is credited with the whole EU regulatory stack alongside its national measures. `eu_view` (`eu_wide` / `eu_member` / `non_eu`) is attached to every output so the bloc can be identified, and `eu_mode` can drop either side.

Note that `eu_wide` is **empty** on this export: no implementing jurisdiction is the EU itself, and `EUU` does not appear in `country_info`. The `eu_wide`-versus-`eu_member` double count that `neis_consolidate_eu()` was designed to catch does not arise in this data. See `diagnostics/eu_act_replication.csv`.

### The as-of date is inferred from the data, and the data contains future dates

241 records carry implementation dates up to 2028-10-01 — staged phase-ins of a single EU sanctions package. The default inference, `max(announce_date, impl_date)`, therefore put the as-of date more than two years past the end of the data, which treated not-yet-in-force measures as active stock and left the flow window covering a period with almost no events. `clamp_future_as_of = TRUE` (default) restricts the inference to dates at or before today. Passing `as_of_date` explicitly is still the right thing to do for a reproducible published run.

### Keyword evidence is driven mostly by `Source`, not `Title`

Of 5,463 records matching `\bwind\b`, only 809 match in the title; the remaining 4,654 match in the source/URL blob, which contains arbitrary path words. Since `source_text` falls back to `URL` when `Source` is missing, a large share of all keyword corroboration is earned by URLs rather than by descriptions of the measure. This is not corrected.

---

## Interpretation caveats
- DIS is a proxy; many policies have missing scale fields.
- GTA/NIPO assessments and trade-flow fields are useful but imperfect.
- Use results as **comparative signals**, not absolute levels.
- The default index is `*_xcountry`, a percentile **within** `tech × supply_chain`. It is not comparable across cells: 0.8 where four countries act is a different claim from 0.8 where forty do. Every output carrying an `xcountry` index also carries `n_countries_in_cell` — check it before comparing across technologies.
- A cell containing one country scores the neutral singleton value of 0.5 on both the index and the percentile, not 1. A percentile over one observation is not a percentile.
