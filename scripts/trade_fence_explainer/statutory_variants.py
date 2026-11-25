"""Statutory US tariff rates per HS10 line and partner on chosen dates, from the Trade War Tracker's
tariff-action engine, with counterfactual variants and extra actions (scripts/trade_fence_explainer/).

Extends scripts/utils/tracker_statutory_rates.py (same engine, same loading) in three ways:
  - variants: "full" (every action in force), and "no232", "no301", "no338", which drop every action of that
    legal authority. "no232" also empties the engine's Section 232 carve-out (_mask_all_232), because without
    Section 232 those goods would no longer be exempt from the stacked Section 301 and 338 actions: the
    counterfactual is the rate that would apply if the authority's duties did not exist, not the rate minus
    its duty;
  - extra actions from a CSV (actions in force by the snapshot date that the engine does not model yet, such
    as the December 2026 Section 232 polysilicon action), applied in phase 2 and, when carve_out is TRUE,
    added to the Section 232 carve-out of the stacked actions;
  - an HS6 filter, so variants other than "full" are written only for the basket's lines.

Needs a full clone of github.com/tradewartracker/trade-war-redux-2025 (the engine notebook, its tariff lists
and cached Census files) and Python 3 with pandas, pyarrow and nbformat.

Extra-action CSV columns: id, effective_date, end_date, authority, reference, description, hs_codes
(semicolon-separated HTS prefixes, dots allowed), countries_include and countries_exclude (semicolon-separated
Census codes; empty include = every partner), rate (percentage points), mode (replace | add | max), carve_out
(TRUE/FALSE: the codes count as Section 232 goods for the stacked actions' carve-out).

Output: <out_dir>/statutory_<date>_<variant>.csv with date, variant, cty_code, hs10, imports, duty (the
weight-year import value and calculated duty) and tariff (the engine's added duty in force, percentage points
since January 2025), and <out_dir>/statutory_check.csv with each partner's import-weighted added duty.

Usage: python statutory_variants.py <clone> <out_dir> --dates 2026-08-15 2026-12-04
           [--variants full no232 no301 no338] [--extra extra_actions.csv] [--hs6 basket_hs6.txt]
"""
import argparse
import os
from pathlib import Path

import nbformat
import pandas as pd

ap = argparse.ArgumentParser()
ap.add_argument("clone")
ap.add_argument("out_dir")
ap.add_argument("--dates", nargs="+", required=True)
ap.add_argument("--variants", nargs="+", default=["full"])
ap.add_argument("--extra", default="")
ap.add_argument("--hs6", default="")
a = ap.parse_args()

repo, out = Path(a.clone).resolve(), Path(a.out_dir).resolve()
out.mkdir(parents=True, exist_ok=True)
hs6_keep = set()
if a.hs6:
    hs6_keep = {ln.strip() for ln in Path(a.hs6).read_text().splitlines() if ln.strip()}
extra = pd.read_csv(a.extra, dtype=str).fillna("") if a.extra else pd.DataFrame()

nb = nbformat.read(repo / "code" / "make-tariff-by-country-by-time.ipynb", as_version=4)
os.chdir(repo)
ns: dict = {}
for cell in nb.cells:
    if cell.cell_type != "code":
        continue
    exec(compile(cell.source, "<engine>", "exec"), ns)
    if "def tariff_imports" in cell.source:
        break

country_list = ns["country_list"]
tariff_imports, load_imports = ns["tariff_imports"], ns["load_imports"]
BASE_ACTIONS = list(ns["TARIFF_ACTIONS"])
BASE_MASK_232 = ns["_mask_all_232"]


def codes_of(s):
    return tuple(c.strip().replace(".", "") for c in s.split(";") if c.strip())


def make_extra_apply(row):
    codes = codes_of(row["hs_codes"])
    inc = set(codes_of(row["countries_include"]))
    exc = set(codes_of(row["countries_exclude"]))
    rate, mode = float(row["rate"]), row["mode"].strip().lower()

    def _apply(df, cty, r):
        if (inc and cty not in inc) or cty in exc:
            return df
        m = df["HS10"].astype(str).str.startswith(codes)
        if mode == "replace":
            df.loc[m, "tariff"] = rate
        elif mode == "max":
            df.loc[m, "tariff"] = df.loc[m, "tariff"].clip(lower=rate)
        else:
            df.loc[m, "tariff"] = df.loc[m, "tariff"] + rate
        return df
    return _apply


EXTRA_ACTIONS, EXTRA_232_CODES = [], ()
for _, row in extra.iterrows():
    act = {"effective_date": row["effective_date"], "authority": row["authority"], "reference": row["reference"],
           "description": row["description"], "phase": 2, "apply": make_extra_apply(row)}
    if row.get("end_date", ""):
        act["end_date"] = row["end_date"]
    EXTRA_ACTIONS.append(act)
    if str(row.get("carve_out", "")).upper() == "TRUE":
        EXTRA_232_CODES += codes_of(row["hs_codes"])


def mask_232_with_extra(df):
    m = BASE_MASK_232(df)
    if EXTRA_232_CODES:
        m = m | df["HS10"].astype(str).str.startswith(EXTRA_232_CODES)
    return m


def set_variant(variant):
    acts = BASE_ACTIONS + EXTRA_ACTIONS
    mask = mask_232_with_extra
    if variant == "no232":
        acts = [x for x in acts if "232" not in x["authority"]]
        mask = lambda df: pd.Series(False, index=df.index)
    elif variant == "no301":
        acts = [x for x in acts if "301" not in x["authority"]]
    elif variant == "no338":
        acts = [x for x in acts if "338" not in x["authority"]]
    elif variant != "full":
        raise SystemExit(f"unknown variant {variant}")
    ns["TARIFF_ACTIONS"] = acts
    ns["_mask_all_232"] = mask


imports = {row["CTY_CODE"]: load_imports(row["CTY_CODE"], year=2024) for _, row in country_list.iterrows()}
checks = []
for d in a.dates:
    date = pd.to_datetime(d)
    for variant in a.variants:
        set_variant(variant)
        rows = []
        for _, row in country_list.iterrows():
            df = imports[row["CTY_CODE"]]
            if variant != "full" and hs6_keep:
                df = df[df["HS10"].astype(str).str[:6].isin(hs6_keep)]
            rate = row["rtariff-aug"] if date >= pd.to_datetime("2025-08-07") else row["rtariff"]
            res = tariff_imports(df.copy(), row["CTY_CODE"], rate, date)
            keep = res[["HS10", "imports", "duty", "tariff"]].rename(columns={"HS10": "hs10"})
            keep.insert(0, "cty_code", row["CTY_CODE"])
            rows.append(keep)
        panel = pd.concat(rows, ignore_index=True)
        panel.insert(0, "variant", variant)
        panel.insert(0, "date", d)
        panel.to_csv(out / f"statutory_{d}_{variant}.csv", index=False)
        chk = panel.assign(w=panel["tariff"] * panel["imports"]).groupby(["date", "variant", "cty_code"])[["w", "imports"]].sum()
        chk["effective_added_duty"] = chk["w"] / chk["imports"]
        checks.append(chk.reset_index())
        print(d, variant, panel.shape, flush=True)
pd.concat(checks).to_csv(out / "statutory_check.csv", index=False)
