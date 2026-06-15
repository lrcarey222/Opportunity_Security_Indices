#!/usr/bin/env python3
"""Chart Japan's energy security indices across sectors (technologies).

Reads the pipeline-computed energy security scores embedded in the
dashboard data layer (`dashboard/js/data.js`, generated from the processed
RDS tables) and renders a comparison of Japan's Energy Security Index by
technology sector and supply-chain stage.

Output: docs/japan_energy_security_by_sector.png

Higher Energy Security Index = more secure (weighted average of the nine
energy-security category scores defined in config/weights.yml).
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
OUT_DIR = os.path.join(REPO, "docs")
OUT_PNG = os.path.join(OUT_DIR, "japan_energy_security_by_sector.png")

# NEIS brand palette (from dashboard/js/data.js)
STAGE_COLORS = {
    "Upstream": "#003A61",
    "Midstream": "#0989B1",
    "Downstream": "#0BD0D9",
}
STAGES = ["Upstream", "Midstream", "Downstream"]


def parse_japan(js_text):
    """Return (overall_es, {(tech, stage): es}) for Japan."""
    i = js_text.find('name:"Japan"')
    if i < 0:
        raise RuntimeError("Japan record not found in data.js")
    overall_es = float(re.search(r"es:([0-9.]+)", js_text[i : i + 200]).group(1))

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

    es = {}
    for key, vals in scores.items():
        if "es" in vals:
            tech, stage = key.split("|")
            es[(tech, stage)] = vals["es"]
    return overall_es, es


def main():
    with open(DATA_JS) as fh:
        overall_es, es = parse_japan(fh.read())

    techs = sorted({t for (t, _) in es})
    # Order sectors by mean energy-security score (descending) for readability
    tech_mean = {
        t: np.mean([es[(t, s)] for s in STAGES if (t, s) in es]) for t in techs
    }
    techs = sorted(techs, key=lambda t: tech_mean[t], reverse=True)

    n = len(techs)
    y = np.arange(n)
    bar_h = 0.26

    fig, ax = plt.subplots(figsize=(11, 8))

    for k, stage in enumerate(STAGES):
        vals = [es.get((t, stage), np.nan) for t in techs]
        offset = (k - 1) * bar_h
        bars = ax.barh(
            y + offset,
            vals,
            height=bar_h,
            color=STAGE_COLORS[stage],
            label=stage,
            edgecolor="white",
            linewidth=0.5,
        )
        for rect, v in zip(bars, vals):
            if not np.isnan(v):
                ax.text(
                    v + 0.008,
                    rect.get_y() + rect.get_height() / 2,
                    f"{v:.2f}",
                    va="center",
                    ha="left",
                    fontsize=7,
                    color="#333333",
                )

    ax.axvline(
        overall_es,
        color="#F8931D",
        linestyle="--",
        linewidth=1.5,
        label=f"Japan overall ({overall_es:.2f})",
    )

    ax.set_yticks(y)
    ax.set_yticklabels(techs)
    ax.invert_yaxis()
    ax.set_xlabel("Energy Security Index  (higher = more secure)")
    ax.set_xlim(0, 1.0)
    ax.set_title(
        "Japan — Energy Security Index by Sector and Supply-Chain Stage",
        fontsize=14,
        fontweight="bold",
        pad=12,
    )
    ax.legend(loc="lower right", frameon=True, fontsize=9)
    ax.grid(axis="x", linestyle=":", alpha=0.4)
    ax.set_axisbelow(True)
    fig.text(
        0.01,
        0.01,
        "Source: Opportunity & Security Indices pipeline (config/weights.yml). "
        "Energy Security Index = weighted average of 9 category scores.",
        fontsize=7,
        color="#666666",
    )

    fig.tight_layout(rect=(0, 0.03, 1, 1))
    os.makedirs(OUT_DIR, exist_ok=True)
    fig.savefig(OUT_PNG, dpi=150)
    print(f"Wrote {OUT_PNG}")


if __name__ == "__main__":
    main()
