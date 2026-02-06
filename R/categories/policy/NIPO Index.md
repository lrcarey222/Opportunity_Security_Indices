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
Key design choice:

> Policies with stronger tech×stage validation should count more toward the index.

So mapping confidence should be used as a **multiplier on contributions**, not only as a reallocation mechanism:

- low confidence → contribution is downweighted
- high confidence → contribution is upweighted (capped)

Subjective parameters control:
- maximum boost (confidence cap)
- penalty for weak validation
- relative importance of CPC vs keywords vs sector flags

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

## Interpretation caveats
- DIS is a proxy; many policies have missing scale fields.
- GTA/NIPO assessments and trade-flow fields are useful but imperfect.
- Use results as **comparative signals**, not absolute levels.
