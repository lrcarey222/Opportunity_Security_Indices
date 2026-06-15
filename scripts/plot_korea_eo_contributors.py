#!/usr/bin/env python3
"""Chart the key contributors to Korea's Economic Opportunity Index score
in Batteries, Electric Grid, and Nuclear.

The Economic Opportunity Index is computed per Country x technology x
supply-chain stage. For a given technology, the three value-chain stages
(Upstream / Midstream / Downstream) are the building blocks that combine
into that technology's overall opportunity score, so they are the concrete
"contributors" we can report from the committed pipeline output.

Real, pipeline-computed EO scores are read from the dashboard data layer
(`dashboard/js/data.js`, generated from the processed RDS tables). The
per-category contributor breakdown (Trade, Cost Competitiveness, Technology
Demand, ...) is NOT reconstructable in this checkout because most of the
EO-category input tables are empty placeholders, so we decompose by the
value-chain stages that are available with real numbers.

Output: docs/korea_eo_contributors_battery_grid_nuclear.png
"""

import json
import os
import re

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np

REPO = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
DATA_JS = os.path.join(REPO, "dashboard", "js", "data.js")
OUT_PNG = os.path.join(REPO, "docs", "korea_eo_contributors_battery_grid_nuclear.png")

TECHS = ["Batteries", "Electric Grid", "Nuclear"]
STAGES = ["Upstream", "Midstream", "Downstream"]
STAGE_COLORS = {
    "Upstream": "#003A61",
    "Midstream": "#0989B1",
    "Downstream": "#0BD0D9",
}


def parse_country(js_text, country_name):
    i = js_text.find(f'name:"{country_name}"')
    if i < 0:
        raise RuntimeError(f"{country_name} not found in data.js")
    overall_eo = float(re.search(r"eo:([0-9.]+)", js_text[i : i + 200]).group(1))

    start = js_text.find("scores:{", i) + len("scores:")
    depth = 0
    end = start
    for j in range(start, len(js_text)):
        if js_text[j] == "{":
            depth += 1
        elif js_text[j] == "}":
            depth -= 1
            if depth == 0:
                end = j + 1
                break
    scores = json.loads(js_text[start:end])
    eo = {}
    for key, vals in scores.items():
        if "eo" in vals:
            tech, stage = key.split("|")
            eo[(tech, stage)] = vals["eo"]
    return overall_eo, eo


def main():
    with open(DATA_JS) as fh:
        text = fh.read()
    overall_eo, eo = parse_country(text, "South Korea")

    n = len(TECHS)
    x = np.arange(n)
    bar_w = 0.25

    fig, ax = plt.subplots(figsize=(10, 6.5))

    for k, stage in enumerate(STAGES):
        vals = [eo.get((t, stage), np.nan) for t in TECHS]
        offset = (k - 1) * bar_w
        bars = ax.bar(
            x + offset,
            vals,
            width=bar_w,
            color=STAGE_COLORS[stage],
            label=stage,
            edgecolor="white",
            linewidth=0.6,
        )
        for rect, v in zip(bars, vals):
            if not np.isnan(v):
                ax.text(
                    rect.get_x() + rect.get_width() / 2,
                    v + 0.012,
                    f"{v:.2f}",
                    ha="center",
                    va="bottom",
                    fontsize=9,
                    color="#222222",
                )

    ax.axhline(
        overall_eo,
        color="#F8931D",
        linestyle="--",
        linewidth=1.5,
        label=f"Korea overall EO ({overall_eo:.2f})",
    )

    ax.set_xticks(x)
    ax.set_xticklabels(TECHS, fontsize=11)
    ax.set_ylabel("Economic Opportunity Index  (higher = more opportunity)")
    ax.set_ylim(0, 0.8)
    ax.set_title(
        "South Korea — Economic Opportunity Index contributors\nby value-chain stage: Batteries, Electric Grid, Nuclear",
        fontsize=13,
        fontweight="bold",
        pad=12,
    )
    ax.legend(loc="upper left", frameon=True, fontsize=9, ncol=2)
    ax.grid(axis="y", linestyle=":", alpha=0.4)
    ax.set_axisbelow(True)
    fig.text(
        0.01,
        0.005,
        "Source: Opportunity & Security Indices pipeline (dashboard data layer). "
        "EO Index = weighted average of category scores (config/weights.yml).",
        fontsize=7,
        color="#666666",
    )

    fig.tight_layout(rect=(0, 0.03, 1, 1))
    os.makedirs(os.path.dirname(OUT_PNG), exist_ok=True)
    fig.savefig(OUT_PNG, dpi=150)
    print(f"Wrote {OUT_PNG}")


if __name__ == "__main__":
    main()
