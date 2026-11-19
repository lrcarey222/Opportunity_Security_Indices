"""US fence tools by supply-chain stage (grid equipment, batteries, solar) against the whole economy.

Tools, measured per HS6 product with 2024 US imports (the whole-economy distribution):
  tariff                  applied tariff: Census calculated duty / value, August 2026, 2024-import-weighted over the
                          product's partner lines (goods.parquet from build_tri_cells.py)
  trade_defence           NIPO measures in force: anti-dumping, anti-subsidy, anti-circumvention, safeguard
  import_ban              NIPO import bans in force
  other_import_restriction NIPO quotas, import licensing, import monitoring, other import NTMs in force
  localisation_requirement NIPO local-content and other localisation requirements in force
  local_content_incentive NIPO local-content incentives in force (e.g. domestic-content bonuses)
  procurement_preference  NIPO public-procurement localisation, access limits and preference margins in force
  investment_restriction  NIPO FDI entry/ownership/operations rules and investment controls in force
  export_restriction      NIPO export bans, licensing and other export controls in force (an outward fence)
NIPO measures count when the United States implements them (national or sub-national), they are assessed
Distortive, they name the product's HS6 code, and they are in force on AS_OF (implemented on or before it and
not removed). Each measure counts once per product. Measures that name more than BROAD_MAX HS6 codes (about 9% of
traded products) are economy-wide: the Russia/Belarus sanction lists and a few programme-wide rules. They do not
tell one product from another, so they are left out of the counts and listed separately.

NIPO sometimes codes a trade-defence case to whole HS headings or chapters (a solar-cell order naming transformers
and lithium-ion cells). td_scope_review.csv reviews every trade-defence case that names a product in these three
supply chains: "restrict" keeps only the listed basket codes for matching entries (other basket codes are dropped,
codes outside the basket are left as NIPO has them); "exclude" drops entries that are no longer in force. The
dropped pairs leave the whole-economy distribution too, so stage and benchmark are counted the same way.

Every product gets a percentile in the whole-economy distribution of each tool (mid-rank: ties split evenly) and a
band: High = top 10% (percentile >= 0.90), Medium = next 30% (>= 0.60), Low = the remaining 60%. For tariffs a
stage's percentile is the rank of its 2024-import-weighted average rate in that distribution. For the measure-count
tools it is the 2024-import-weighted mean of its products' percentiles (where its typical import dollar ranks),
banded the same way; the stage also reports the share of its imports in High/Medium/Low products. (Ranking the
stage's average measure count instead would put a stage in the top 10% whenever one product had a single ban,
because most products have none of most tools.)

Usage: python fence_tools.py <scratch dir> <basket master csv> <nipo instrument csv>
"""
import json
import sys
from pathlib import Path

import numpy as np
import pandas as pd

out, master_path, fam_path = Path(sys.argv[1]), sys.argv[2], sys.argv[3]
AS_OF = pd.Timestamp("2026-10-31")
BROAD_MAX = 500
TECHS = {"Electric Grid": "Grid equipment", "Batteries": "Batteries", "Solar": "Solar"}
STAGES = ["Extraction", "Refining", "Components", "Assembly", "Deployment", "Recovery"]
TOOLS = {
    "tariff": ("Tariffs (applied rate)", "%", "Census calculated duty / customs value, August 2026"),
    "trade_defence": ("Anti-dumping, countervailing and safeguard duties", "measures", "NIPO: Anti-dumping, Anti-subsidy, Anti-circumvention, Safeguard"),
    "import_ban": ("Import bans", "measures", "NIPO: Import ban"),
    "other_import_restriction": ("Quotas, licensing and other import restrictions", "measures", "NIPO: Import quota, Import licensing requirement, Import monitoring, Import-related non-tariff measure nes"),
    "localisation_requirement": ("Local-content requirements", "measures", "NIPO: Local content / value added / operations / labour requirement, Localisation nes"),
    "local_content_incentive": ("Local-content incentives", "measures", "NIPO: Local content / value added / operations / labour incentive"),
    "procurement_preference": ("Buy-domestic procurement rules", "measures", "NIPO: Public procurement localisation, access, preference margin, nes"),
    "investment_restriction": ("Investment and ownership restrictions", "measures", "NIPO: FDI entry and ownership rule, FDI treatment and operations nes, Corporate control order, Controls on commercial transactions and investment instruments, Foreign customer limit"),
    "export_restriction": ("Export controls (outward fence)", "measures", "NIPO: Export ban, quota, licensing requirement, tax, other export NTMs"),
}
FAMILY_TOOL = {"trade_defence": "trade_defence", "localisation_requirement": "localisation_requirement",
               "localisation_unspecified": "localisation_requirement", "localisation_incentive": "local_content_incentive",
               "procurement_preference": "procurement_preference", "fdi_restriction": "investment_restriction",
               "investment_control": "investment_restriction", "export_restriction": "export_restriction"}

# --- whole-economy products and the applied tariff -------------------------------------------------------
g = pd.read_parquet(out / "goods.parquet", columns=["HS10", "hs6", "w", "tau_collected", "matched", "tau_adcvd_100", "matched_adcvd"])
prod = g.groupby("hs6")["w"].sum().rename("imports_2024").to_frame()
prod = prod[prod["imports_2024"] > 0]
m = g[g["matched"]]
prod["tariff"] = (m["w"] * m["tau_collected"]).groupby(m["hs6"]).sum() / m.groupby("hs6")["w"].sum()
a = g[g["matched_adcvd"]]
prod["tariff_with_adcvd"] = (a["w"] * a["tau_adcvd_100"]).groupby(a["hs6"]).sum() / a.groupby("hs6")["w"].sum()

# --- NIPO measures -----------------------------------------------------------------------------------------
fam = pd.read_csv(fam_path).set_index("gta_intervention_type")["family"]
u = pd.read_csv(out / "nipo_us_oct2026.csv", low_memory=False)
u["family"] = u["GTA Intervention Type"].map(fam)
u["tool"] = u["family"].map(FAMILY_TOOL)
u.loc[u["GTA Intervention Type"] == "Import ban", "tool"] = "import_ban"
u.loc[(u["family"] == "import_restriction") & (u["GTA Intervention Type"] != "Import ban"), "tool"] = "other_import_restriction"
for c in ["Implementation Date", "Removal Date"]:
    u[c] = pd.to_datetime(u[c], errors="coerce")
live = u[u["tool"].notna()
         & u["Level of Government Implementation"].isin(["National", "Sub-national"])
         & (u["Initial Assessment (Change Relative to 1 Jan 2009)"] == "Distortive")
         & (u["Implementation Date"] <= AS_OF)
         & (u["Removal Date"].isna() | (u["Removal Date"] > AS_OF))
         & u["Product: HS 6-digit (2022)"].notna()].copy()
live["codes"] = live["Product: HS 6-digit (2022)"].astype(str).str.split(r"\s*,\s*")
live["n_codes"] = live["codes"].str.len()
broad = live[live["n_codes"] > BROAD_MAX].copy()
narrow = live[live["n_codes"] <= BROAD_MAX].copy()
master = pd.read_csv(master_path, dtype={"hs6": str})
is_t = lambda x: x.astype(str).str.upper().isin(["TRUE", "T", "1"])
master = master[~is_t(master["excluded"]) & ~is_t(master["legacy_hs"]) & master["tech"].isin(TECHS)]
cells = master[["tech", "stage", "hs6", "sub_sector"]].drop_duplicates(["tech", "stage", "hs6"])
basket_codes = set(cells["hs6"])

pairs = narrow[["Entry ID", "tool", "Title", "codes"]].explode("codes").reset_index(drop=True)
pairs["codes"] = pairs["codes"].str.replace(r"\D", "", regex=True).str.zfill(6)
pairs = pairs[pairs["codes"].str.len() == 6].drop_duplicates(["Entry ID", "tool", "codes"]).reset_index(drop=True)

# Trade-defence scope review (see td_scope_review.csv).
review = pd.read_csv(Path(__file__).with_name("td_scope_review.csv"), dtype=str).fillna("")
td = pairs["tool"] == "trade_defence"
pairs["rule"] = ""
for _, r in review.iterrows():
    hit_rule = td & (pairs["rule"] == "") & pairs["Title"].str.contains(r["pattern"], case=False, regex=True)
    pairs.loc[hit_rule, "rule"] = r["family"]
rv = review.set_index("family")
in_basket = pairs["codes"].isin(basket_codes)
kept = pairs.apply(lambda x: x["codes"] in set(rv.loc[x["rule"], "kept_codes"].split()) if x["rule"] else True, axis=1)
decision = pairs["rule"].map(rv["decision"]).fillna("")
drop = (decision == "exclude") | ((decision == "restrict") & in_basket & ~kept)
unreviewed = pairs[td & in_basket & (pairs["rule"] == "")]
if len(unreviewed):
    print("UNREVIEWED trade-defence entries naming basket codes:", sorted(unreviewed["Entry ID"].unique()))
review_out = []
for fam_name, r in rv.iterrows():
    x = pairs[pairs["rule"] == fam_name]
    xb = x[x["codes"].isin(basket_codes)]
    review_out.append({"Order family": fam_name, "Decision": {"exclude": "Not in force: entries dropped",
                                                              "restrict": "Basket codes restricted"}[r["decision"]],
                       "NIPO entries": x["Entry ID"].nunique(),
                       "Entry IDs": ", ".join(str(e) for e in sorted(x["Entry ID"].unique())),
                       "Basket codes NIPO names": ", ".join(sorted(set(xb["codes"]))),
                       "Basket codes kept": ", ".join(sorted(set(xb.loc[~drop.loc[xb.index], "codes"]))),
                       "Basket codes dropped": ", ".join(sorted(set(xb.loc[drop.loc[xb.index], "codes"]))),
                       "Reason": r["reason"]})
review_tbl = pd.DataFrame(review_out)
excluded_entries = set(pairs.loc[decision == "exclude", "Entry ID"])
pairs = pairs[~drop]
counts = pairs.groupby(["codes", "tool"])["Entry ID"].nunique().unstack(fill_value=0)
for t in TOOLS:
    if t != "tariff":
        prod[t] = counts[t].reindex(prod.index).fillna(0) if t in counts else 0.0

# --- the whole-economy distribution and the bands ----------------------------------------------------------
def pct_series(d):
    """Mid-rank percentile of each value in d (ties split evenly); NaN stays NaN."""
    ok = d.dropna()
    return ((ok.rank(method="average") - 0.5) / len(ok)).reindex(d.index)


def band(p):
    return "High" if p >= 0.90 else ("Medium" if p >= 0.60 else "Low")


def coverage(v, w):
    """Share of import value in products with the tool applied (> 0)."""
    ok = v.notna()
    return float((w[ok] * (v[ok] > 0)).sum() / w[ok].sum()) if w[ok].sum() > 0 else np.nan


dist_rows = []
for t, (label, unit, src) in TOOLS.items():
    d = prod[t].dropna()
    q = d.quantile([0.5, 0.6, 0.75, 0.9, 0.95, 0.99])
    wd = prod.loc[d.index, "imports_2024"]
    dist_rows.append({"tool": t, "Tool": label, "Unit": unit, "Products": len(d), "Share of products with none": (d == 0).mean(),
                      "Share of imports covered": coverage(d, wd), "Mean (products)": d.mean(),
                      "Import-weighted mean": np.average(d, weights=wd), "Median": q[0.5],
                      "60th percentile (Medium from)": q[0.6], "75th percentile": q[0.75],
                      "90th percentile (High from)": q[0.9], "95th percentile": q[0.95], "99th percentile": q[0.99], "Measured as": src})
dist = pd.DataFrame(dist_rows)
for t in TOOLS:
    prod[f"{t}_pct"] = pct_series(prod[t])
    prod[f"{t}_band"] = prod[f"{t}_pct"].map(lambda p: band(p) if pd.notna(p) else np.nan)


def summarise(present, rec):
    """Fill rec with each tool's stage value, percentile, band, coverage and band mix for products `present`."""
    w = present["imports_2024"]
    for t in TOOLS:
        v = present[t]
        ok = v.notna()
        wt = w[ok] if w[ok].sum() > 0 else pd.Series(1.0, index=v[ok].index)
        val = float(np.average(v[ok], weights=wt))
        if t == "tariff":  # rates are continuous: rank the stage's average rate itself
            d = prod[t].dropna()
            p = float(((d < val).sum() + 0.5 * (d == val).sum()) / len(d))
        else:
            p = float(np.average(present.loc[ok, f"{t}_pct"], weights=wt))
        rec[f"{t}|value"] = val
        rec[f"{t}|pct"] = p
        rec[f"{t}|band"] = band(p)
        rec[f"{t}|covered"] = int((v[ok] > 0).sum())
        rec[f"{t}|coverage"] = coverage(v, w)
        for b in ["High", "Medium", "Low"]:
            rec[f"{t}|mix_{b}"] = float((wt * (present.loc[ok, f"{t}_band"] == b)).sum() / wt.sum())
    a = present["tariff_with_adcvd"].notna()
    rec["tariff_with_adcvd|value"] = float(np.average(present.loc[a, "tariff_with_adcvd"], weights=w[a]))
    return rec

rows, prod_rows = [], []
for tech, tname in TECHS.items():
    for st in STAGES:
        src_stage = "Assembly" if st == "Deployment" else st
        codes = cells[(cells["tech"] == tech) & (cells["stage"] == src_stage)]
        sub = prod.reindex(codes["hs6"])
        present = sub.dropna(subset=["imports_2024"])
        note = "Deployment has no products of its own: the Assembly goods the build-out buys." if st == "Deployment" else ""
        if present.empty:
            rows.append({"Supply chain": tname, "Stage": st, "Products (HS6)": len(codes), "US imports 2024 ($bn)": np.nan,
                         "Note": "No products in the basket at this stage." if codes.empty else "No 2024 US imports of this stage's products."})
            continue
        w = present["imports_2024"]
        rec = summarise(present, {"Supply chain": tname, "Stage": st, "Products (HS6)": len(present),
                                  "US imports 2024 ($bn)": w.sum() / 1e9, "Note": note})
        stage_codes = set(present.index)
        rec["broad|n"] = int(broad["codes"].apply(lambda cs: bool({c.zfill(6) for c in cs} & stage_codes)).sum())
        rows.append(rec)
        if st != "Deployment":
            for code, r in present.iterrows():
                pr = {"Supply chain": tname, "Stage": st, "HS6": code,
                      "Product group": codes.set_index("hs6").loc[code, "sub_sector"], "US imports 2024 ($m)": r["imports_2024"] / 1e6}
                for t in TOOLS:
                    pr[f"{t}|value"] = r[t]
                    pr[f"{t}|pct"] = r[f"{t}_pct"]
                    pr[f"{t}|band"] = r[f"{t}_band"]
                prod_rows.append(pr)

rows.append(summarise(prod, {"Supply chain": "Whole economy", "Stage": "All traded products", "Products (HS6)": len(prod),
                             "US imports 2024 ($bn)": prod["imports_2024"].sum() / 1e9,
                             "Note": "Benchmark: by construction about 10% / 30% / 60% of products sit in the High / Medium / Low bands."}))
stage_tbl = pd.DataFrame(rows)
products = pd.DataFrame(prod_rows)

# Measures that touch these stages' products, for the reader to check.
bc = pairs[pairs["codes"].isin(basket_codes)].groupby("Entry ID")["codes"].apply(lambda s: sorted(set(s)))
hit = narrow.drop_duplicates("Entry ID").set_index("Entry ID").loc[lambda d: d.index.isin(bc.index)].copy()
hit["basket_codes"] = bc
hit = hit.reset_index()
code_cells = cells.assign(lbl=cells["tech"].map(TECHS) + " " + cells["stage"]).groupby("hs6")["lbl"].apply(lambda s: sorted(set(s)))
hit["Stages touched"] = hit["basket_codes"].apply(lambda cs: "; ".join(sorted({l for c in cs for l in code_cells.get(c, [])})))
measures = pd.DataFrame({
    "Tool": hit["tool"].map(lambda t: TOOLS[t][0]), "GTA intervention type": hit["GTA Intervention Type"],
    "Entry ID": hit["Entry ID"], "Title": hit["Title"], "Level": hit["Level of Government Implementation"],
    "Implemented": hit["Implementation Date"].dt.date, "Removal date": hit["Removal Date"].dt.date,
    "Sanction": hit["Is Sanction"], "Affected jurisdictions": hit["Affected Jurisdiction"],
    "Products named (HS6)": hit["codes"].str.len(), "Basket products named": hit["basket_codes"].str.len(),
    "Stages touched": hit["Stages touched"], "URL": hit["URL"],
}).sort_values(["Tool", "Implemented"], ascending=[True, False])

broad_tbl = pd.DataFrame({
    "Tool": broad["tool"].map(lambda t: TOOLS[t][0]), "GTA intervention type": broad["GTA Intervention Type"],
    "Entry ID": broad["Entry ID"], "Title": broad["Title"], "Implemented": broad["Implementation Date"].dt.date,
    "Sanction": broad["Is Sanction"], "Products named (HS6)": broad["n_codes"],
    "Basket products named": broad["codes"].apply(lambda cs: len({c.zfill(6) for c in cs} & basket_codes)), "URL": broad["URL"],
}).sort_values(["Tool", "Products named (HS6)"], ascending=[True, False])

for name, df in [("fence_stage_tbl", stage_tbl), ("fence_products", products), ("fence_measures", measures),
                 ("fence_distribution", dist), ("fence_broad", broad_tbl), ("fence_td_review", review_tbl)]:
    df.to_csv(out / f"{name}.csv", index=False)
(out / "fence_meta.json").write_text(json.dumps({"as_of": str(AS_OF.date()), "n_products": int(len(prod)),
                                                   "n_live_measures": int(live["Entry ID"].nunique()), "broad_max": BROAD_MAX,
                                                   "n_broad": int(broad["Entry ID"].nunique()), "n_counted": int(narrow["Entry ID"].nunique()),
                                                   "n_basket_measures": int(hit["Entry ID"].nunique()),
                                                   "n_excluded_not_in_force": len(excluded_entries),
                                                   "tools": {t: list(v) for t, v in TOOLS.items()}}, indent=1))
pd.set_option("display.width", 250)
print(dist[["tool", "Products", "Share of products with none", "Share of imports covered", "Median", "60th percentile (Medium from)", "90th percentile (High from)"]].round(3).to_string(index=False))
show = ["Supply chain", "Stage", "Products (HS6)"] + [f"{t}|band" for t in TOOLS]
print(stage_tbl[show].to_string(index=False))
print("live measures", live["Entry ID"].nunique(), "| broad", broad["Entry ID"].nunique(), "| counted touching basket", hit["Entry ID"].nunique())

# Product-level inputs for the workbook (values, not percentiles: the workbook ranks them with formulas).
econ = prod[["imports_2024", "tariff_with_adcvd"] + list(TOOLS)].copy()
econ.index.name = "hs6"
econ.to_csv(out / "fence_economy_products.csv")
narrow_codes = hit[["Entry ID", "basket_codes"]].assign(basket_codes=lambda d: d["basket_codes"].str.join(", "))
narrow_codes.to_csv(out / "fence_measures_codes.csv", index=False)
