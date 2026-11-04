"""Statutory US tariff rates per HS10 line and partner from the Trade War Tracker's tariff-action engine.

Optional input of scripts/28_build_us_trade_restrictiveness.R (config/us_trade_restrictiveness.yml
statutory_rates, or OPSI_TWT_STATUTORY). Needs a full clone of
github.com/tradewartracker/trade-war-redux-2025 (the engine notebook, its tariff lists and its cached Census
files) and Python 3 with pandas, pyarrow and nbformat.

Executes the code cells of code/make-tariff-by-country-by-time.ipynb up to and including the engine
(`tariff_imports`), the way the repository's own analysis notebooks load it, then evaluates every partner on
each date. Writes one row per (date, partner, HS10): the weight-year import value and calculated duty, and the
engine's added duty in force on that date (percentage points since January 2025). Also writes
<out>.check.csv with each partner's import-weighted added duty, to compare with the tracker's published
country-by-time.csv.

Usage: python tracker_statutory_rates.py <trade-war-redux clone> <out.csv> <YYYY-MM-DD> [<YYYY-MM-DD> ...]
"""
import os
import sys
from pathlib import Path

import nbformat
import pandas as pd

repo, out, dates = Path(sys.argv[1]).resolve(), Path(sys.argv[2]).resolve(), sys.argv[3:]
if not dates:
    sys.exit("give at least one evaluation date (YYYY-MM-DD)")
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

rows = []
for d in dates:
    date = pd.to_datetime(d)
    for _, row in country_list.iterrows():
        df = load_imports(row["CTY_CODE"], year=2024)
        rate = row["rtariff-aug"] if date >= pd.to_datetime("2025-08-07") else row["rtariff"]
        res = tariff_imports(df.copy(), row["CTY_CODE"], rate, date)
        keep = res[["HS10", "imports", "duty", "tariff"]].rename(columns={"HS10": "hs10"})
        keep.insert(0, "cty_code", row["CTY_CODE"])
        keep.insert(0, "date", d)
        rows.append(keep)

panel = pd.concat(rows, ignore_index=True)
panel.to_csv(out, index=False)

chk = panel.assign(w=panel["tariff"] * panel["imports"]).groupby(["date", "cty_code"])[["w", "imports"]].sum()
chk["effective_tariff"] = chk["w"] / chk["imports"]
chk.reset_index().to_csv(out.with_suffix(".check.csv"), index=False)
print(panel.shape)
