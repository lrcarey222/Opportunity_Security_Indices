"""Build US_Fence_Tools_by_Stage.xlsx from the fence_tools.py outputs.

The workbook ranks every basket product against the whole economy with live formulas (COUNTIF mid-ranks on the
'Economy data' sheet) and bands it against the thresholds on 'Read me', so changing a threshold re-bands everything.
Measure counts, tariffs and imports are data inputs (blue); everything derived from them is a formula.

Usage: python build_fence_xlsx.py <scratch dir> <EID dictionary csv> <output xlsx>
"""
import json
import re
import sys
from pathlib import Path

import pandas as pd
from openpyxl import Workbook
from openpyxl.formatting.rule import CellIsRule
from openpyxl.styles import Alignment, Border, Font, PatternFill, Side
from openpyxl.utils import column_index_from_string as CI
from openpyxl.utils import get_column_letter as L

src, dict_path, out_path = Path(sys.argv[1]), sys.argv[2], sys.argv[3]
meta = json.loads((src / "fence_meta.json").read_text())
econ = pd.read_csv(src / "fence_economy_products.csv", dtype={"hs6": str})
prods = pd.read_csv(src / "fence_products.csv", dtype={"HS6": str})
stage_py = pd.read_csv(src / "fence_stage_tbl.csv")
measures = pd.read_csv(src / "fence_measures.csv").drop_duplicates("Entry ID")
mcodes = pd.read_csv(src / "fence_measures_codes.csv", dtype=str).drop_duplicates("Entry ID")
mcodes["Entry ID"] = mcodes["Entry ID"].astype(int)
measures = measures.merge(mcodes, on="Entry ID", how="left")
broad = pd.read_csv(src / "fence_broad.csv").drop_duplicates("Entry ID")
review = pd.read_csv(src / "fence_td_review.csv").fillna("")
findings = json.loads((src / "fence_findings.json").read_text(encoding="utf-8")) if (src / "fence_findings.json").exists() else []

TOOLS = [  # key, label, short label for headers, unit, what counts
    ("tariff", "Tariffs (applied rate)", "Tariffs", "rate",
     "Applied tariff: Census calculated duty divided by customs value, August 2026, weighted by 2024 imports across the product's HS10 x partner lines. Includes Section 232, Section 301 and reciprocal tariffs; excludes AD/CVD cash deposits."),
    ("trade_defence", "Anti-dumping, countervailing and safeguard duties", "AD/CVD and safeguards", "count",
     "NIPO intervention types Anti-dumping, Anti-subsidy, Anti-circumvention and Safeguard, after the trade-defence scope review."),
    ("import_ban", "Import bans", "Import bans", "count", "NIPO intervention type Import ban."),
    ("other_import_restriction", "Quotas, licensing and other import restrictions", "Quotas and licensing", "count",
     "NIPO intervention types Import quota, Import licensing requirement, Import monitoring, Import-related non-tariff measure nes."),
    ("localisation_requirement", "Local-content requirements", "Local-content rules", "count",
     "NIPO intervention types Local content / value added / operations / labour requirement and Localisation nes."),
    ("local_content_incentive", "Local-content incentives", "Local-content incentives", "count",
     "NIPO intervention types Local content / value added / operations / labour incentive (for example domestic-content bonuses on tax credits)."),
    ("procurement_preference", "Buy-domestic procurement rules", "Buy-domestic procurement", "count",
     "NIPO intervention types Public procurement localisation, Public procurement access, Public procurement preference margin, Public procurement nes."),
    ("investment_restriction", "Investment and ownership restrictions", "Investment restrictions", "count",
     "NIPO intervention types FDI entry and ownership rule, FDI treatment and operations nes, Corporate control order, Controls on commercial transactions and investment instruments, Foreign customer limit."),
    ("export_restriction", "Export controls (outward fence)", "Export controls", "count",
     "NIPO intervention types Export ban, Export quota, Export licensing requirement, Export tax, Export-related non-tariff measure nes."),
]
TKEYS = [t[0] for t in TOOLS]
LABEL = {t[0]: t[1] for t in TOOLS}
CHAINS = ["Grid equipment", "Batteries", "Solar"]
STAGES = ["Extraction", "Refining", "Components", "Assembly", "Deployment", "Recovery"]

# ---- styles --------------------------------------------------------------------------------------------------
FONT = "Arial"
NAVY = "1F3A4D"
f_base = Font(name=FONT, size=10)
f_title = Font(name=FONT, size=14, bold=True, color=NAVY)
f_sub = Font(name=FONT, size=10, italic=True, color="595959")
f_head = Font(name=FONT, size=10, bold=True, color="FFFFFF")
f_sect = Font(name=FONT, size=11, bold=True, color=NAVY)
f_bold = Font(name=FONT, size=10, bold=True)
f_input = Font(name=FONT, size=10, color="0000FF")
f_input_b = Font(name=FONT, size=11, bold=True, color="0000FF")
f_link = Font(name=FONT, size=10, color="0563C1", underline="single")
f_note = Font(name=FONT, size=9, italic=True, color="595959")
fill_head = PatternFill("solid", fgColor=NAVY)
fill_group = PatternFill("solid", fgColor="D9E1E8")
fill_input = PatternFill("solid", fgColor="FFFF00")
fill_na = PatternFill("solid", fgColor="F2F2F2")
thin = Side(style="thin", color="BFBFBF")
b_bottom = Border(bottom=thin)
wrap = Alignment(wrap_text=True, vertical="top")
center = Alignment(horizontal="center", vertical="center", wrap_text=True)
BAND_STYLE = {"High": ("B2182B", "FFFFFF"), "Medium": ("F4A582", "000000"), "Low": ("E8EEF4", "44546A")}
PCT0, PCT1, NUM1, NUM2, USD = "0%", "0.0%", "0.0", "0.00", "#,##0.0"

wb = Workbook()
ws_read = wb.active
ws_read.title = "Read me"
ws_sum = wb.create_sheet("Summary")
ws_sd = wb.create_sheet("Stage detail")
ws_bp = wb.create_sheet("Basket products")
ws_dist = wb.create_sheet("Economy distribution")
ws_mc = wb.create_sheet("Measures counted")
ws_rev = wb.create_sheet("Trade-defence scope review")
ws_br = wb.create_sheet("Economy-wide measures")
ws_meth = wb.create_sheet("Method and sources")
ws_ec = wb.create_sheet("Economy data")
for ws in wb.worksheets:
    ws.sheet_view.showGridLines = False


def put(ws, row, col, value, font=f_base, fmt=None, fill=None, align=None, border=None):
    c = ws.cell(row=row, column=col, value=value)
    c.font = font
    if fmt:
        c.number_format = fmt
    if fill:
        c.fill = fill
    if align:
        c.alignment = align
    if border:
        c.border = border
    return c


def header_row(ws, row, labels, start=1, height=30):
    for i, lab in enumerate(labels):
        put(ws, row, start + i, lab, f_head, fill=fill_head, align=center)
    ws.row_dimensions[row].height = height


def title(ws, text, sub):
    put(ws, 1, 1, text, f_title)
    put(ws, 2, 1, sub, f_sub)


def band_rules(ws, rng):
    for b, (bg, fg) in BAND_STYLE.items():
        ws.conditional_formatting.add(rng, CellIsRule(operator="equal", formula=[f'"{b}"'],
                                                      fill=PatternFill(fill_type="solid", start_color=bg, end_color=bg, bgColor=bg),
                                                      font=Font(name=FONT, size=10, bold=(b == "High"), color=fg)))


def widths(ws, spec):
    for col, w in spec.items():
        ws.column_dimensions[col].width = w


# ---- Economy data --------------------------------------------------------------------------------------------
EC0 = 5
EC1 = EC0 + len(econ) - 1
ECOL = {"imports": "B", "tariff": "C", "trade_defence": "D", "import_ban": "E", "other_import_restriction": "F",
        "localisation_requirement": "G", "local_content_incentive": "H", "procurement_preference": "I",
        "investment_restriction": "J", "export_restriction": "K", "adcvd": "L"}
title(ws_ec, "Economy data: every HS6 product the United States imported in 2024",
      f"{len(econ):,} products. Inputs (blue): 2024 imports, the August 2026 applied tariff and counts of NIPO measures in force on {meta['as_of']} that name the product. Every other sheet ranks against these columns.")
header_row(ws_ec, 4, ["HS6", "US imports 2024 ($m)", "Applied tariff, Aug 2026"] + [LABEL[k] + " (measures)" for k in TKEYS[1:]]
           + ["Applied tariff incl. AD/CVD (basket orders only)", "In the three supply chains"], height=54)
basket_set = set(prods["HS6"])
for i, r in enumerate(econ.itertuples(index=False)):
    row = EC0 + i
    put(ws_ec, row, 1, r.hs6)
    put(ws_ec, row, 2, round(r.imports_2024 / 1e6, 6), f_input, USD)
    if pd.notna(r.tariff):
        put(ws_ec, row, 3, round(r.tariff / 100, 8), f_input, PCT1)
    for j, k in enumerate(TKEYS[1:]):
        put(ws_ec, row, 4 + j, int(getattr(r, k)), f_input, "0")
    if pd.notna(r.tariff_with_adcvd):
        put(ws_ec, row, 12, round(r.tariff_with_adcvd / 100, 8), f_input, PCT1)
    put(ws_ec, row, 13, "Yes" if r.hs6 in basket_set else None)
ws_ec.freeze_panes = "B5"
ws_ec.auto_filter.ref = f"A4:M{EC1}"
widths(ws_ec, {"A": 9, "B": 12, "C": 11, **{L(c): 13 for c in range(4, 12)}, "L": 14, "M": 11})


def erng(key):
    col = ECOL[key]
    return f"'Economy data'!${col}${EC0}:${col}${EC1}"


E_HS = f"'Economy data'!$A${EC0}:$A${EC1}"

# ---- Read me (thresholds first: other sheets reference them) -------------------------------------------------
HIGH, MED = "'Read me'!$C$9", "'Read me'!$C$10"
title(ws_read, "US fence tools by supply-chain stage: grid equipment, batteries and solar",
      f"Built 8 November 2026. Tariffs: Census calculated duties, August 2026, on 2024 import weights. Other tools: GTA New Industrial Policy Observatory, measures in force on 31 October 2026.")
put(ws_read, 4, 1, "What this workbook shows", f_sect)
lines = [
    "For each of the six stages of the grid-equipment, battery and solar supply chains, the US 'fence' is split into nine tools: tariffs and eight kinds of non-tariff measure from NIPO.",
    "Each tool is measured product by product (HS6) for every good the US imported in 2024, which gives a whole-economy distribution. Each basket product, and each stage, is then placed in that distribution.",
    "High = top 10% most restrictive products in the economy; Medium = the next 30% (top 40%); Low = the remaining 60%.",
]
for i, t in enumerate(lines):
    put(ws_read, 5 + i, 1, t, align=Alignment(wrap_text=True, vertical="top"))
    ws_read.merge_cells(start_row=5 + i, start_column=1, end_row=5 + i, end_column=8)
    ws_read.row_dimensions[5 + i].height = 28
put(ws_read, 8, 1, "Band settings (edit the blue cells: every band in the workbook updates)", f_sect)
put(ws_read, 9, 1, "High band starts at percentile", f_bold)
put(ws_read, 9, 3, 0.90, f_input_b, PCT0, fill_input, Alignment(horizontal="center"))
put(ws_read, 9, 4, "Top 10% most restrictive (user specification)", f_note)
put(ws_read, 10, 1, "Medium band starts at percentile", f_bold)
put(ws_read, 10, 3, 0.60, f_input_b, PCT0, fill_input, Alignment(horizontal="center"))
put(ws_read, 10, 4, "Top 40% (user specification); Low = the remaining 60%", f_note)

r0 = 12
put(ws_read, r0, 1, "How to read a band", f_sect)
how = [
    "Product: its percentile is the share of the economy's products with a lower value (ties split evenly). So where most products have none of a tool, a product with no measures sits near the middle (Low) and any product with one sits near the top.",
    "Stage, tariffs: the stage's import-weighted average rate, ranked among the economy's products. Stage, other tools: the import-weighted average of its products' percentiles, i.e. where its typical import dollar ranks (ranking an average measure count would put any stage with one ban in the top 10%). Stage detail also shows how much of each stage's imports sit in High, Medium and Low products.",
    "Value: tariffs show the import-weighted applied rate. Other tools show the share of the stage's 2024 imports in products named by at least one measure of that kind; the measure count per product is on Stage detail.",
    "A NIPO measure names HS6 codes. Many measures bite on only part of that trade (one country, one firm, one end use), so a High band means the products are often named, not that all trade in them is blocked. Stage detail lists the largest measures behind each cell.",
    "Deployment has no products of its own: it uses the Assembly goods the build-out buys. Grid equipment and solar have no recovery products in the basket.",
]
for i, t in enumerate(how):
    put(ws_read, r0 + 1 + i, 1, t, align=wrap)
    ws_read.merge_cells(start_row=r0 + 1 + i, start_column=1, end_row=r0 + 1 + i, end_column=8)
    ws_read.row_dimensions[r0 + 1 + i].height = 40
r1 = r0 + 2 + len(how)
put(ws_read, r1, 1, "Key findings", f_sect)
for i, t in enumerate(findings):
    put(ws_read, r1 + 1 + i, 1, t, align=wrap)
    ws_read.merge_cells(start_row=r1 + 1 + i, start_column=1, end_row=r1 + 1 + i, end_column=8)
    ws_read.row_dimensions[r1 + 1 + i].height = 54
r2 = r1 + 2 + len(findings)
put(ws_read, r2, 1, "Sheets", f_sect)
guide = [
    ("Summary", "Band and headline value for every stage x tool, with the whole economy as a benchmark."),
    ("Stage detail", "One row per stage x tool: value, coverage, percentile, band, band mix of imports, the whole-economy benchmark, the number of measures and the largest ones."),
    ("Basket products", "Every HS6 product in the three supply chains, with its value, percentile and band for each tool."),
    ("Economy distribution", "The whole-economy distribution of each tool: share with none, import coverage, mean, percentiles."),
    ("Measures counted", "Every NIPO measure in force that names a product in the three supply chains, after the scope review."),
    ("Trade-defence scope review", "Where NIPO's HS coding of an AD/CVD case was corrected, and why."),
    ("Economy-wide measures", f"The {meta['n_broad']} measures naming more than {meta['broad_max']} HS6 codes (sanction lists, programme-wide rules), left out of the counts."),
    ("Method and sources", "Definitions, rules, caveats and sources."),
    ("Economy data", "The input data: every product's imports, tariff and measure counts."),
]
for i, (s, d) in enumerate(guide):
    put(ws_read, r2 + 1 + i, 1, s, f_bold)
    put(ws_read, r2 + 1 + i, 2, d)
widths(ws_read, {"A": 30, "B": 14, "C": 12, "D": 14, "E": 14, "F": 14, "G": 14, "H": 40})

# ---- Economy distribution ------------------------------------------------------------------------------------
DIST_ROW = {}
title(ws_dist, "Whole-economy distribution of each fence tool",
      f"Across all {len(econ):,} HS6 products the US imported in 2024 (tariffs: {int(econ['tariff'].notna().sum()):,} products with a matched duty). Thresholds follow the band settings on Read me.")
header_row(ws_dist, 4, ["Tool", "Unit", "Products", "Share of products with none", "Share of imports covered",
                        "Mean across products", "Import-weighted mean", "Median", "Value at Medium threshold",
                        "Value at High threshold", "95th percentile", "99th percentile", "Maximum", "Measured as"], height=42)
IMP = erng("imports")
for i, (k, lab, short, unit, what) in enumerate(TOOLS):
    row = 5 + i
    DIST_ROW[k] = row
    R = erng(k)
    fmt = PCT1 if unit == "rate" else NUM2
    put(ws_dist, row, 1, lab, f_bold)
    put(ws_dist, row, 2, "Applied rate" if unit == "rate" else "Measures per product")
    put(ws_dist, row, 3, f"=COUNT({R})", fmt="#,##0")
    put(ws_dist, row, 4, f"=COUNTIF({R},0)/C{row}", fmt=PCT1)
    put(ws_dist, row, 5, f'=SUMIFS({IMP},{R},">0")/SUMIFS({IMP},{R},">=0")', fmt=PCT1)
    put(ws_dist, row, 6, f"=AVERAGE({R})", fmt=fmt)
    put(ws_dist, row, 7, f'=SUMPRODUCT({IMP},{R})/SUMIFS({IMP},{R},">=0")', fmt=fmt)
    put(ws_dist, row, 8, f"=MEDIAN({R})", fmt=fmt)
    put(ws_dist, row, 9, f"=PERCENTILE({R},{MED})", fmt=fmt)
    put(ws_dist, row, 10, f"=PERCENTILE({R},{HIGH})", fmt=fmt)
    put(ws_dist, row, 11, f"=PERCENTILE({R},0.95)", fmt=fmt)
    put(ws_dist, row, 12, f"=PERCENTILE({R},0.99)", fmt=fmt)
    put(ws_dist, row, 13, f"=MAX({R})", fmt=fmt)
    put(ws_dist, row, 14, what, align=wrap)
    ws_dist.row_dimensions[row].height = 40
nrow = 5 + len(TOOLS) + 1
put(ws_dist, nrow, 1, "Reading the thresholds: for most non-tariff tools more than 60% of products have no measure, so the value at the Medium threshold is 0. "
    "Bands come from percentile ranks with ties split evenly, so a product with none sits at half the share with none (Low), and any product with at least one measure ranks at or above the share with none: "
    "for import bans that is about 91%, so a single ban puts a product in the High band.", f_note, align=wrap)
ws_dist.merge_cells(start_row=nrow, start_column=1, end_row=nrow, end_column=14)
ws_dist.row_dimensions[nrow].height = 42
ws_dist.freeze_panes = "B5"
widths(ws_dist, {"A": 34, "B": 18, "C": 10, "D": 12, "E": 12, "F": 11, "G": 11, "H": 9, "I": 11, "J": 11, "K": 11, "L": 11, "M": 10, "N": 70})

# ---- Basket products -----------------------------------------------------------------------------------------
eid = pd.read_csv(dict_path, dtype={"hs6": str}).drop_duplicates("hs6").set_index("hs6")["hs6_desc"]
MANUAL_DESC = {  # HS 2022 heading text, abridged, for basket codes outside the EID dictionary
    "260200": "Manganese ores and concentrates", "282739": "Chlorides, other (excl. ammonium, calcium, magnesium, aluminium, iron, cobalt, nickel)",
    "730890": "Structures and parts of structures, of iron or steel, nes", "731210": "Stranded wire, ropes and cables, of iron or steel, not insulated",
    "741012": "Copper foil, not backed, of copper alloys, <= 0.15 mm", "741021": "Copper foil, backed, of refined copper, <= 0.15 mm",
    "741022": "Copper foil, backed, of copper alloys, <= 0.15 mm", "760719": "Aluminium foil, not backed, <= 0.2 mm, other than rolled only",
    "761090": "Structures and parts of structures, of aluminium, nes", "853690": "Electrical apparatus for switching or connecting circuits, <= 1,000 V, nes",
    "854442": "Electric conductors, <= 1,000 V, fitted with connectors",
}
prods["Product group"] = prods["Product group"].str.replace("Cpper", "Copper", regex=False)  # typo in the basket's sub_sector
prods["Description"] = prods["HS6"].map(lambda c: eid.get(c, MANUAL_DESC.get(c, "")))
assert prods["Description"].ne("").all(), prods.loc[prods["Description"].eq(""), "HS6"].tolist()
ord_chain = {c: i for i, c in enumerate(CHAINS)}
ord_stage = {s: i for i, s in enumerate(STAGES)}
prods = prods.sort_values(["Supply chain", "Stage", "US imports 2024 ($m)"],
                          key=lambda s: s.map(ord_chain) if s.name == "Supply chain" else (s.map(ord_stage) if s.name == "Stage" else -s))
BP0 = 6
BP1 = BP0 + len(prods) - 1
title(ws_bp, "Basket products: each HS6 product in the three supply chains, ranked against the whole economy",
      "Value, percentile and band per tool. Percentile = share of the economy's products with a lower value (ties split evenly). Deployment uses the Assembly products, so it has no rows of its own.")
fixed = ["Supply chain", "Stage", "HS6", "Description", "Product group", "US imports 2024 ($m)"]
header_row(ws_bp, 5, fixed + ["Value", "Percentile", "Band"] * len(TOOLS) + ["Applied tariff incl. AD/CVD"], height=30)
for i, (k, lab, short, unit, what) in enumerate(TOOLS):
    c0 = 7 + 3 * i
    put(ws_bp, 4, c0, lab + ("" if unit == "rate" else " (measures)"), f_bold, fill=fill_group, align=center)
    ws_bp.merge_cells(start_row=4, start_column=c0, end_row=4, end_column=c0 + 2)
ws_bp.row_dimensions[4].height = 30
BP_COL = {}
for i, k in enumerate(TKEYS):
    BP_COL[k] = (L(7 + 3 * i), L(8 + 3 * i), L(9 + 3 * i))
BP_ADC = L(7 + 3 * len(TOOLS))
for i, r in enumerate(prods.itertuples(index=False)):
    row = BP0 + i
    put(ws_bp, row, 1, r[0])
    put(ws_bp, row, 2, r[1])
    put(ws_bp, row, 3, r[2])
    put(ws_bp, row, 4, prods["Description"].iloc[i])
    put(ws_bp, row, 5, r[3])
    put(ws_bp, row, 6, f"=INDEX({IMP},MATCH($C{row},{E_HS},0))", fmt=USD)
    for k, (cv, cp, cb) in BP_COL.items():
        R = erng(k)
        unit_fmt = PCT1 if k == "tariff" else "0"
        look = f"INDEX({R},MATCH($C{row},{E_HS},0))"
        put(ws_bp, row, CI(cv), f'=IF({look}="","",{look})', fmt=unit_fmt)
        put(ws_bp, row, CI(cp), f'=IF({cv}{row}="","",(COUNTIF({R},"<"&{cv}{row})+0.5*COUNTIF({R},{cv}{row}))/COUNT({R}))', fmt=PCT0)
        put(ws_bp, row, CI(cb), f'=IF({cp}{row}="","",IF({cp}{row}>={HIGH},"High",IF({cp}{row}>={MED},"Medium","Low")))', align=Alignment(horizontal="center"))
    look = f"INDEX({erng('adcvd')},MATCH($C{row},{E_HS},0))"
    put(ws_bp, row, 7 + 3 * len(TOOLS), f'=IF({look}="","",{look})', fmt=PCT1)
for k, (cv, cp, cb) in BP_COL.items():
    band_rules(ws_bp, f"{cb}{BP0}:{cb}{BP1}")
ws_bp.freeze_panes = "G6"
ws_bp.auto_filter.ref = f"A5:{BP_ADC}{BP1}"
widths(ws_bp, {"A": 14, "B": 11, "C": 8, "D": 44, "E": 26, "F": 11, **{L(c): 10 for c in range(7, 7 + 3 * len(TOOLS))}, BP_ADC: 11})


def bp(col):
    return f"'Basket products'!${col}${BP0}:${col}${BP1}"


# ---- Measures counted ----------------------------------------------------------------------------------------
MC0 = 5
measures = measures.sort_values(["Tool", "Basket products named"], ascending=[True, False])
MC1 = MC0 + len(measures) - 1
title(ws_mc, "Measures counted: NIPO measures in force that name a product in the three supply chains",
      f"{len(measures):,} measures in force on {meta['as_of']} (implemented by the United States, assessed Distortive), after dropping economy-wide measures and applying the trade-defence scope review.")
mc_cols = ["Tool", "GTA intervention type", "Entry ID", "Title", "Level", "Implemented", "Removal date", "Sanction",
           "Affected jurisdictions", "Products named (HS6)", "Basket products named", "Basket HS6 codes named", "Stages touched", "URL"]
header_row(ws_mc, 4, mc_cols)
mc = measures.rename(columns={"basket_codes": "Basket HS6 codes named"})[mc_cols].copy()
for c in ["Implemented", "Removal date"]:
    mc[c] = pd.to_datetime(mc[c], errors="coerce").dt.date
for i, vals in enumerate(mc.itertuples(index=False)):
    row = MC0 + i
    for j, v in enumerate(vals):
        if j == 7:
            v = bool(v)
        elif not isinstance(v, str) and pd.isna(v):
            v = None
        put(ws_mc, row, 1 + j, v, fmt="yyyy-mm-dd" if j in (5, 6) else None)
    url = vals[13]
    if isinstance(url, str) and url.startswith("http"):
        ws_mc.cell(row=row, column=14).hyperlink = url
        ws_mc.cell(row=row, column=14).font = f_link
ws_mc.freeze_panes = "D5"
ws_mc.auto_filter.ref = f"A4:N{MC1}"
widths(ws_mc, {"A": 30, "B": 26, "C": 9, "D": 70, "E": 11, "F": 11, "G": 11, "H": 9, "I": 30, "J": 10, "K": 10, "L": 30, "M": 40, "N": 30})
MC_TOOL, MC_SANC, MC_STAGES = f"'Measures counted'!$A${MC0}:$A${MC1}", f"'Measures counted'!$H${MC0}:$H${MC1}", f"'Measures counted'!$M${MC0}:$M${MC1}"

# Largest measures per stage x tool (by the stage's 2024 imports in the products each measure names).
short = lambda t: re.sub(r"^United States of America(/[^:]+)?:\s*", "", str(t))
mx = measures.assign(code=measures["basket_codes"].str.split(", ")).explode("code")
top = {}
for (sc, st), g in prods.groupby(["Supply chain", "Stage"]):
    imp = g.set_index("HS6")["US imports 2024 ($m)"]
    y = mx[mx["code"].isin(imp.index)].assign(v=lambda d: d["code"].map(imp))
    for tool, h in y.groupby("Tool"):
        agg = h.groupby(["Entry ID", "Title"])["v"].sum().reset_index().sort_values(["v", "Entry ID"], ascending=[False, True])
        top[(sc, st, tool)] = "; ".join(f"{short(t)[:95]} [{e}]" for e, t in zip(agg["Entry ID"].head(3), agg["Title"].head(3)))

# ---- Stage detail --------------------------------------------------------------------------------------------
SD0 = 5
title(ws_sd, "Stage detail: each stage x tool against the whole-economy distribution",
      "Stage percentile: tariffs = rank of the stage's import-weighted average rate; other tools = import-weighted average of its products' percentiles. Coverage = share of the stage's 2024 imports in products with the tool (> 0). Measure counts come from 'Measures counted'.")
sd_cols = ["Supply chain", "Stage", "Products drawn from", "Tool", "Unit", "Products with data", "US imports 2024 ($m)",
           "Stage value (import-weighted)", "Products with the tool", "Share of stage imports covered", "Stage percentile", "Band",
           "Imports in High products", "Imports in Medium products", "Imports in Low products",
           "Whole economy: import-weighted value", "Whole economy: share of imports covered",
           "Applied tariff incl. AD/CVD cash deposits", "Measures naming the stage's products", "of which sanctions",
           "Largest measures (by stage imports named) [NIPO Entry ID]", "Note"]
header_row(ws_sd, 4, sd_cols, height=68)
SD_ROW = {}
row = SD0
selA, selB, impR = bp("A"), bp("B"), bp("F")
for sc in CHAINS:
    for st in STAGES:
        srcst = "Assembly" if st == "Deployment" else st
        has = ((prods["Supply chain"] == sc) & (prods["Stage"] == srcst)).any()
        note = ("Deployment has no products of its own: the Assembly goods the build-out buys." if st == "Deployment" else "")
        if not has:
            note = "No products in the basket at this stage."
        for k, lab, sh, unit, what in TOOLS:
            SD_ROW[(sc, st, k)] = row
            put(ws_sd, row, 1, sc)
            put(ws_sd, row, 2, st)
            put(ws_sd, row, 3, srcst)
            put(ws_sd, row, 4, lab)
            put(ws_sd, row, 5, "Applied rate" if unit == "rate" else "Measures per product")
            if not has:
                for c in range(6, 22):
                    ws_sd.cell(row=row, column=c).fill = fill_na
                put(ws_sd, row, 22, note, f_note)
                row += 1
                continue
            cv, cp, cb = (bp(x) for x in BP_COL[k])
            sel = f"({selA}=$A{row})*({selB}=$C{row})"
            fmtv = PCT1 if unit == "rate" else NUM2
            put(ws_sd, row, 6, f'=COUNTIFS({selA},$A{row},{selB},$C{row},{cv},">=0")', fmt="0")
            put(ws_sd, row, 7, f'=SUMIFS({impR},{selA},$A{row},{selB},$C{row},{cv},">=0")', fmt=USD)
            put(ws_sd, row, 8, f'=IF($G{row}>0,SUMPRODUCT({sel}*{impR},{cv})/$G{row},"")', fmt=fmtv)
            put(ws_sd, row, 9, f'=COUNTIFS({selA},$A{row},{selB},$C{row},{cv},">0")', fmt="0")
            put(ws_sd, row, 10, f'=IF($G{row}>0,SUMIFS({impR},{selA},$A{row},{selB},$C{row},{cv},">0")/$G{row},"")', fmt=PCT0)
            if unit == "rate":
                R = erng(k)
                put(ws_sd, row, 11, f'=IF($G{row}>0,(COUNTIF({R},"<"&H{row})+0.5*COUNTIF({R},H{row}))/COUNT({R}),"")', fmt=PCT0)
            else:
                put(ws_sd, row, 11, f'=IF($G{row}>0,SUMPRODUCT({sel}*{impR},{cp})/$G{row},"")', fmt=PCT0)
            put(ws_sd, row, 12, f'=IF(K{row}="","",IF(K{row}>={HIGH},"High",IF(K{row}>={MED},"Medium","Low")))', align=Alignment(horizontal="center"))
            for j, b in enumerate(["High", "Medium", "Low"]):
                put(ws_sd, row, 13 + j, f'=IF($G{row}>0,SUMIFS({impR},{selA},$A{row},{selB},$C{row},{cb},"{b}")/$G{row},"")', fmt=PCT0)
            put(ws_sd, row, 16, f"='Economy distribution'!$G${DIST_ROW[k]}", fmt=fmtv)
            put(ws_sd, row, 17, f"='Economy distribution'!$E${DIST_ROW[k]}", fmt=PCT0)
            if unit == "rate":
                adc = bp(BP_ADC)
                put(ws_sd, row, 18, f'=SUMPRODUCT({sel}*{impR},{adc})/SUMIFS({impR},{selA},$A{row},{selB},$C{row},{adc},">=0")', fmt=PCT1)
            else:
                lbl = f'$A{row}&" "&$C{row}'
                put(ws_sd, row, 19, f"=SUMPRODUCT(ISNUMBER(SEARCH({lbl},{MC_STAGES}))*({MC_TOOL}=$D{row}))", fmt="0")
                put(ws_sd, row, 20, f"=SUMPRODUCT(ISNUMBER(SEARCH({lbl},{MC_STAGES}))*({MC_TOOL}=$D{row})*({MC_SANC}=TRUE))", fmt="0")
                put(ws_sd, row, 21, top.get((sc, srcst, lab), ""), align=Alignment(vertical="top"))
            put(ws_sd, row, 22, (note + " " if note else "") + ("Ranked on the stage's average rate." if unit == "rate" else ""), f_note)
            row += 1
SD1 = row - 1
band_rules(ws_sd, f"L{SD0}:L{SD1}")
ws_sd.freeze_panes = "E5"
ws_sd.auto_filter.ref = f"A4:V{SD1}"
widths(ws_sd, {"A": 14, "B": 11, "C": 11, "D": 44, "E": 19, "F": 9, "G": 11, "H": 11, "I": 9, "J": 10, "K": 10, "L": 9,
               "M": 9, "N": 9, "O": 9, "P": 12, "Q": 12, "R": 12, "S": 10, "T": 10, "U": 100, "V": 40})

# ---- Summary -------------------------------------------------------------------------------------------------
title(ws_sum, "Summary: how fenced each stage is, tool by tool, relative to the whole US economy",
      "Band: where the stage ranks among all US imported products (tariffs: its average rate; other tools: its typical import dollar). High = top 10%, Medium = next 30%, Low = bottom 60%; thresholds on Read me. "
      "Value: tariff = import-weighted applied rate, Aug 2026 (excl. AD/CVD); other tools = share of the stage's 2024 imports in products named by at least one measure in force.")
fixed = ["Supply chain", "Stage", "Products (HS6)", "US imports 2024 ($bn)"]
header_row(ws_sum, 5, fixed + sum((["Band", "Rate" if u == "rate" else "Imports covered"] for _, _, _, u, _ in TOOLS), []) + ["Tariff incl. AD/CVD"], height=44)
for i, (k, lab, sh, unit, what) in enumerate(TOOLS):
    c0 = 5 + 2 * i
    put(ws_sum, 4, c0, lab, f_bold, fill=fill_group, align=center)
    ws_sum.merge_cells(start_row=4, start_column=c0, end_row=4, end_column=c0 + 1)
ws_sum.row_dimensions[4].height = 42
ADC_COL = 5 + 2 * len(TOOLS)
row = 6
for sc in CHAINS:
    first = row
    for st in STAGES:
        srcst = "Assembly" if st == "Deployment" else st
        has = ((prods["Supply chain"] == sc) & (prods["Stage"] == srcst)).any()
        put(ws_sum, row, 1, sc, f_bold if row == first else f_base)
        put(ws_sum, row, 2, st + (" (Assembly goods)" if st == "Deployment" else ""))
        put(ws_sum, row, 3, f'=COUNTIFS({selA},$A{row},{selB},"{srcst}")', fmt="0")
        if not has:
            put(ws_sum, row, 5, "No products in the basket at this stage", f_note)
            for c in range(4, ADC_COL + 1):
                ws_sum.cell(row=row, column=c).fill = fill_na
            row += 1
            continue
        put(ws_sum, row, 4, f'=SUMIFS({impR},{selA},$A{row},{selB},"{srcst}")/1000', fmt=USD)
        for i, (k, lab, sh, unit, what) in enumerate(TOOLS):
            r_sd = SD_ROW[(sc, st, k)]
            put(ws_sum, row, 5 + 2 * i, f"='Stage detail'!L{r_sd}", align=Alignment(horizontal="center"))
            put(ws_sum, row, 6 + 2 * i, f"='Stage detail'!{'H' if unit == 'rate' else 'J'}{r_sd}", fmt=PCT1 if unit == "rate" else PCT0)
        put(ws_sum, row, ADC_COL, f"='Stage detail'!R{SD_ROW[(sc, st, 'tariff')]}", fmt=PCT1)
        row += 1
    for c in range(1, ADC_COL + 1):
        ws_sum.cell(row=row - 1, column=c).border = b_bottom
econ_row = row
put(ws_sum, econ_row, 1, "Whole economy", f_bold)
put(ws_sum, econ_row, 2, "All imported products")
put(ws_sum, econ_row, 3, f"=COUNTA({E_HS})", fmt="#,##0")
put(ws_sum, econ_row, 4, f"=SUM({IMP})/1000", fmt=USD)
for i, (k, lab, sh, unit, what) in enumerate(TOOLS):
    put(ws_sum, econ_row, 5 + 2 * i, "Benchmark", f_note, align=Alignment(horizontal="center"))
    put(ws_sum, econ_row, 6 + 2 * i, f"='Economy distribution'!{'G' if unit == 'rate' else 'E'}{DIST_ROW[k]}", fmt=PCT1 if unit == "rate" else PCT0)
put(ws_sum, econ_row, ADC_COL, f'=SUMPRODUCT({IMP},{erng("adcvd")})/SUMIFS({IMP},{erng("adcvd")},">=0")', fmt=PCT1)
for c in range(1, ADC_COL + 1):
    ws_sum.cell(row=econ_row, column=c).fill = PatternFill("solid", fgColor="F7F7F7")
for i in range(len(TOOLS)):
    col = L(5 + 2 * i)
    band_rules(ws_sum, f"{col}6:{col}{econ_row - 1}")
notes = [
    "High bands rest on how often US measures name these products, not on how much trade they stop: many NIPO measures apply to one country, one firm or one end use. See 'Largest measures' on Stage detail before quoting a band.",
    "Trade defence counts NIPO's AD/CVD and safeguard cases after a scope review of every case naming these products ('Trade-defence scope review'). The last column adds AD/CVD cash deposits from the Federal Register orders on basket products to the applied tariff (whole economy: basket orders only).",
    f"Left out of the counts: {meta['n_broad']} economy-wide measures naming more than {meta['broad_max']} HS6 codes, mostly Russia sanction lists ('Economy-wide measures').",
]
for i, t in enumerate(notes):
    put(ws_sum, econ_row + 2 + i, 1, t, f_note, align=wrap)
    ws_sum.merge_cells(start_row=econ_row + 2 + i, start_column=1, end_row=econ_row + 2 + i, end_column=ADC_COL)
    ws_sum.row_dimensions[econ_row + 2 + i].height = 26
ws_sum.freeze_panes = "E6"
widths(ws_sum, {"A": 15, "B": 26, "C": 9, "D": 10, **{L(c): 9.5 for c in range(5, ADC_COL)}, L(ADC_COL): 10})

# ---- Trade-defence scope review ------------------------------------------------------------------------------
title(ws_rev, "Trade-defence scope review",
      "NIPO sometimes codes an AD/CVD case to whole HS headings or chapters. Every case naming a product in the three supply chains was checked: "
      "codes the case's product cannot occupy were dropped (from the stage and from the whole-economy distribution), and entries no longer in force were removed.")
rev_cols = ["Order family", "Decision", "NIPO entries", "Entry IDs", "Basket codes NIPO names", "Basket codes kept", "Basket codes dropped", "Reason"]
header_row(ws_rev, 4, rev_cols)
for i, r in enumerate(review.itertuples(index=False)):
    for j, v in enumerate(r):
        put(ws_rev, 5 + i, 1 + j, v if v != "" else None, align=Alignment(wrap_text=True, vertical="top"))
ws_rev.freeze_panes = "B5"
ws_rev.auto_filter.ref = f"A4:H{4 + len(review)}"
widths(ws_rev, {"A": 30, "B": 22, "C": 9, "D": 30, "E": 34, "F": 26, "G": 34, "H": 70})

# ---- Economy-wide measures -----------------------------------------------------------------------------------
title(ws_br, f"Economy-wide measures left out of the counts (naming more than {meta['broad_max']} HS6 codes)",
      "They name most of the tariff schedule, so they do not tell one product from another. Most are Russia and Belarus sanction lists.")
br_cols = ["Tool", "GTA intervention type", "Entry ID", "Title", "Implemented", "Sanction", "Products named (HS6)", "Basket products named", "URL"]
header_row(ws_br, 4, br_cols)
broad = broad.sort_values(["Tool", "Products named (HS6)"], ascending=[True, False])
broad["Implemented"] = pd.to_datetime(broad["Implemented"], errors="coerce").dt.date
for i, r in enumerate(broad.itertuples(index=False)):
    vals = list(r)
    for j, v in enumerate(vals):
        put(ws_br, 5 + i, 1 + j, bool(v) if j == 5 else v, fmt="yyyy-mm-dd" if j == 4 else None)
    if isinstance(vals[-1], str) and vals[-1].startswith("http"):
        ws_br.cell(row=5 + i, column=9).hyperlink = vals[-1]
        ws_br.cell(row=5 + i, column=9).font = f_link
ws_br.freeze_panes = "D5"
ws_br.auto_filter.ref = f"A4:I{4 + len(broad)}"
widths(ws_br, {"A": 30, "B": 30, "C": 9, "D": 80, "E": 11, "F": 9, "G": 10, "H": 10, "I": 30})

# ---- Method and sources --------------------------------------------------------------------------------------
title(ws_meth, "Method and sources", "How each tool is measured, the rules applied to NIPO, and caveats.")
put(ws_meth, 4, 1, "Tools", f_sect)
header_row(ws_meth, 5, ["Tool", "Product-level measure", "What counts"])
for i, (k, lab, sh, unit, what) in enumerate(TOOLS):
    put(ws_meth, 6 + i, 1, lab, f_bold, align=Alignment(vertical="top"))
    put(ws_meth, 6 + i, 2, "Applied tariff rate" if unit == "rate" else "Number of measures in force naming the product", align=wrap)
    put(ws_meth, 6 + i, 3, what, align=wrap)
    ws_meth.row_dimensions[6 + i].height = 40
r = 6 + len(TOOLS) + 1
put(ws_meth, r, 1, "Rules", f_sect)
rules = [
    f"Products: the {len(econ):,} HS6 codes with US imports in 2024 (Census, the Trade War Tracker's monthly HS10 x country panel). Basket: data/reference/energy_hs6_master.csv, technologies Electric Grid, Batteries and Solar, excluding codes flagged excluded or legacy.",
    "Tariff: Census calculated duty / customs value for each HS10 x partner line in August 2026, weighted by that line's 2024 imports; HS10 lines re-coded since 2024 fall back to HS8, then HS6. AD/CVD cash deposits are not in Census duties; the 'incl. AD/CVD' column adds the all-others or country-wide cash deposits of the Federal Register orders in data/reference/adcvd_orders.csv (capped at 100%).",
    f"NIPO measures count when the United States implements them (national or sub-national), GTA assesses them Distortive, they name the product's HS6 code (HS 2022), and they are in force on {meta['as_of']}: implemented on or before that date and not removed. Each measure counts once per product.",
    f"Economy-wide measures: {meta['n_broad']} measures naming more than {meta['broad_max']} HS6 codes (about 9% of imported products) are left out and listed on 'Economy-wide measures'. Import tariff measures are not counted at all: the applied rate already captures them.",
    "Trade-defence scope review: NIPO codes some AD/CVD cases to whole headings or chapters. Each case naming a basket product was reviewed; codes the product cannot occupy were dropped and entries no longer in force (the Section 201 solar safeguard, the graphite anode material case) removed. Solar-cell, wind-tower and silicon-metal scopes follow the Federal Register orders. Codes outside the basket keep NIPO's coding.",
    "Percentile: share of the economy's products with a lower value plus half of those with the same value. Band: High from the High threshold, Medium from the Medium threshold, Low below (Read me). Stage percentile: for tariffs, the rank of the stage's 2024-import-weighted average rate; for the other tools, the 2024-import-weighted average of its products' percentiles (ranking an average measure count would put any stage with a single ban in the top 10%).",
    "Deployment uses the Assembly products (the goods the build-out buys). Recovery has products only for batteries.",
]
for i, t in enumerate(rules):
    put(ws_meth, r + 1 + i, 1, f"{i + 1}.", align=Alignment(vertical="top"))
    put(ws_meth, r + 1 + i, 2, t, align=wrap)
    ws_meth.merge_cells(start_row=r + 1 + i, start_column=2, end_row=r + 1 + i, end_column=3)
    ws_meth.row_dimensions[r + 1 + i].height = 54
r = r + 2 + len(rules)
put(ws_meth, r, 1, "Caveats", f_sect)
caveats = [
    "A measure that names an HS6 code may bite on a small part of that trade: one partner (Russian aluminium), one firm (UFLPA entity list additions, a DOE loan guarantee), or one end use (the FCC Covered List entry for drones names lithium-ion cells). The bands measure how often products are named, not trade blocked.",
    "Counts treat each NIPO entry as one measure. AD and CVD on the same product are separate entries, and sunset extensions can appear as new entries, so products with long-running orders count several times.",
    "NIPO coverage is GTA's. Measures GTA has not recorded, or has recorded without HS codes, are missing; GTA's in-force status can lag (the Section 201 solar safeguard is listed as in force after it ended).",
    "Procurement, local-content and investment measures often name the same broad product lists (Buy America programmes name iron, steel and manufactured goods). A High band there reflects the reach of general programmes as well as energy-specific rules; check 'Largest measures'.",
    "Tariffs are August 2026 collections on 2024 trade weights, so they show the rate on the goods the US used to buy, not on today's mix.",
]
for i, t in enumerate(caveats):
    put(ws_meth, r + 1 + i, 1, "-", align=Alignment(vertical="top"))
    put(ws_meth, r + 1 + i, 2, t, align=wrap)
    ws_meth.merge_cells(start_row=r + 1 + i, start_column=2, end_row=r + 1 + i, end_column=3)
    ws_meth.row_dimensions[r + 1 + i].height = 42
r = r + 2 + len(caveats)
put(ws_meth, r, 1, "Sources", f_sect)
sources = [
    ("Global Trade Alert, New Industrial Policy Observatory (NIPO), October 2026 extract", "https://www.globaltradealert.org/"),
    ("US Census Bureau international trade data (imports and calculated duties by HS10 and country), via the Trade War Tracker's data pipeline", "https://www.tradewartracker.com/"),
    ("US Department of Commerce AD/CVD orders, Federal Register (cash deposit rates and HTS scope)", "https://www.federalregister.gov/"),
    ("Basket: Opportunity Security Indices, data/reference/energy_hs6_master.csv; product descriptions from data/reference/electro_industrial_hs6_dictionary.csv and HS 2022 heading text", None),
]
for i, (t, u) in enumerate(sources):
    put(ws_meth, r + 1 + i, 2, t, align=wrap)
    if u:
        c = put(ws_meth, r + 1 + i, 3, u, f_link)
        c.hyperlink = u
widths(ws_meth, {"A": 34, "B": 60, "C": 80})

for ws, fit_tall in [(ws_read, False), (ws_sum, True), (ws_sd, False), (ws_bp, False), (ws_dist, True), (ws_meth, False), (ws_rev, False)]:
    ws.page_setup.orientation = "landscape"
    ws.page_setup.fitToWidth = 1
    ws.page_setup.fitToHeight = 1 if fit_tall else 0
    ws.sheet_properties.pageSetUpPr.fitToPage = True
wb.save(out_path)
print("saved", out_path, "| basket rows", len(prods), "| stage-detail rows", SD1 - SD0 + 1, "| measures", len(measures))
json.dump({"SD_ROW": {"|".join(k): v for k, v in SD_ROW.items()}, "BP0": BP0, "BP1": BP1, "SD0": SD0, "SD1": SD1},
          open(src / "fence_xlsx_layout.json", "w"))
