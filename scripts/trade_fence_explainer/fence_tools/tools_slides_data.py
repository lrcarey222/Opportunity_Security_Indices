"""Band grid per technology for the three fence-tools summary slides (deck/build_deck.js).

Reads fence_tools.py outputs (fence_stage_tbl.csv, fence_measures.csv) and writes fence_tools_slides.json:
for each technology, stage rows with each tool's band, whether that band rests on a single measure, and the
tariff rates. Deployment is folded into Assembly (same goods); stages with no basket products are dropped.

Usage: python tools_slides_data.py <scratch dir>
"""
import json
import sys
from pathlib import Path

import pandas as pd

src = Path(sys.argv[1])
st = pd.read_csv(src / "fence_stage_tbl.csv")
ms = pd.read_csv(src / "fence_measures.csv").drop_duplicates("Entry ID")
TOOLS = [("tariff", "Tariffs (applied rate)"), ("trade_defence", "Anti-dumping, countervailing and safeguard duties"),
         ("import_ban", "Import bans"), ("other_import_restriction", "Quotas, licensing and other import restrictions"),
         ("localisation_requirement", "Local-content requirements"), ("local_content_incentive", "Local-content incentives"),
         ("procurement_preference", "Buy-domestic procurement rules"), ("investment_restriction", "Investment and ownership restrictions"),
         ("export_restriction", "Export controls (outward fence)")]
CHAINS = {"Electric Grid": "Grid equipment", "Solar": "Solar", "Batteries": "Batteries"}
WHAT = {  # what each stage's basket holds, largest import lines first
    ("Grid equipment", "Extraction"): "bauxite, copper ore",
    ("Grid equipment", "Refining"): "copper, aluminium, electrical steel",
    ("Grid equipment", "Components"): "converter parts, power chips, insulators",
    ("Grid equipment", "Assembly"): "switchgear, converters, transformers, cable",
    ("Solar", "Extraction"): "quartz",
    ("Solar", "Refining"): "copper wire, silicon metal, polysilicon",
    ("Solar", "Components"): "cells, wafers, inverter parts, cell-making tools",
    ("Solar", "Assembly"): "modules, inverters, racking, cable",
    ("Batteries", "Extraction"): "graphite, manganese, nickel, cobalt ores",
    ("Batteries", "Refining"): "nickel, cobalt, foils, anode materials",
    ("Batteries", "Components"): "battery parts",
    ("Batteries", "Assembly"): "lithium-ion cells and packs",
    ("Batteries", "Recovery"): "nickel, cobalt and battery scrap",
}
out = {}
for tech, chain in CHAINS.items():
    rows = []
    for stage in ["Extraction", "Refining", "Components", "Assembly", "Recovery"]:
        r = st[(st["Supply chain"] == chain) & (st["Stage"] == stage)]
        if r.empty or pd.isna(r["tariff|band"].iloc[0]):
            continue
        r = r.iloc[0]
        label = f"{chain} {stage}"
        cells = []
        for k, lab in TOOLS:
            n = None if k == "tariff" else int(ms[(ms["Tool"] == lab) & ms["Stages touched"].str.contains(label, regex=False)]["Entry ID"].nunique())
            cells.append({"tool": k, "band": r[f"{k}|band"], "pct": round(float(r[f"{k}|pct"]), 3),
                          "coverage": round(float(r[f"{k}|coverage"]), 3), "measures": n,
                          "single": bool(n == 1 and r[f"{k}|band"] in ("High", "Medium"))})
        rows.append({"stage": "Assembly & deployment" if stage == "Assembly" else stage, "what": WHAT[(chain, stage)],
                     "imports_bn": round(float(r["US imports 2024 ($bn)"]), 2), "tariff": round(float(r["tariff|value"]), 1),
                     "tariff_adcvd": round(float(r["tariff_with_adcvd|value"]), 1),
                     "tariff_high_share": round(float(r["tariff|mix_High"]), 3), "cells": cells})
    out[tech] = {"chain": chain, "rows": rows,
                 "no_recovery": not any(x["stage"] == "Recovery" for x in rows)}
(src / "fence_tools_slides.json").write_text(json.dumps(out, indent=1))
for tech, v in out.items():
    print(tech)
    for x in v["rows"]:
        print(f"  {x['stage']:22s} {x['tariff']:5.1f}/{x['tariff_adcvd']:5.1f} " + " ".join(f"{c['band'][0]}{'†' if c['single'] else ' '}" for c in x["cells"]))
