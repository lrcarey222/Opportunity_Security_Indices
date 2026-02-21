# Energy Transition Intelligence Hub — Dashboard

An interactive policy dashboard for exploring the **Opportunity Security Indices (OSI)** — a set of
country-level metrics measuring Energy Security (ES), Economic Opportunity (EO), and Partnership
Strength (PSI) across clean energy technology supply chains.

## Opening the dashboard

Simply open `dashboard/index.html` in any modern browser.
An internet connection is required on first load to fetch the world map topology and fonts.

```
dashboard/
├── index.html          Main dashboard (open this)
├── css/
│   └── style.css       Styles
└── js/
    ├── data.js         Country data, index scores, partnership scoring
    └── app.js          Application logic (map, charts, interactions)
```

## Features

### World Overview
- **Choropleth world map** — countries coloured by ES, EO or PSI index
- **Technology & supply chain filters** — disaggregate scores to Solar, Wind, EV, Batteries or Green Hydrogen × Upstream / Midstream / Downstream
- **Region filter & country search** — focus the map on specific geographies
- **Zoom & pan** — explore the map at any scale

### Country Detail Panel (click any country)
| Tab | Content |
|-----|---------|
| **Overview** | Radar chart of all categories + key insight cards |
| **Energy Security** | Weighted bar chart + donut contribution chart for 9 ES categories |
| **Econ. Opportunity** | Ranked bar chart for 10 EO categories |
| **By Technology** | ES/EO heatmap × 5 technologies × 3 supply chains; grouped bar chart |
| **Advanced** | Full data table with all category scores; CSV export |
| **Methodology** | Index definitions, weights, normalisation method, data sources |

### Partnerships View
Select a **home country** and explore three partnership dimensions:

| Type | Description |
|------|-------------|
| 🛡 **Friendshore** | Reliable, strategically aligned partners — weighted by alignment, ES need, and partner EO |
| 📦 **Export Partners** | High-demand markets for your country's clean energy exports — weighted by partner demand and trade compatibility |
| 🌱 **Development** | Countries where investment creates strategic value — weighted by partner need, resource potential, and home capacity |

Each partner shows:
- Ranked list with scores
- Partnership dimension radar
- Global ES vs EO scatter plot with home/partner highlighted
- ES and EO category comparison bars

### Advanced Features
- **⚙ Customise weights** — adjust relative category weights; scores update in real time
- **⬇ Export all scores** — download the full country index table as CSV
- **Per-country CSV export** — from the Advanced tab in the country panel

## Connecting to live pipeline data

The dashboard currently uses synthetic representative data. To connect to live pipeline outputs:

1. Run the R pipeline: `Rscript run_pipeline.R`
2. Export index outputs from `data/processed/outputs/index_outputs.rds` to CSV
3. Replace the data generation in `js/data.js` with a fetch from the CSV, or embed directly

The data schema expected is documented in `docs/data_dictionary.md`.

## Technology

| Library | Version | Purpose |
|---------|---------|---------|
| D3.js | v7 | World map (choropleth, zoom) |
| TopoJSON Client | v3 | World topology decoding |
| Chart.js | v4 | Radar, bar, doughnut, bubble charts |
| Google Fonts | — | Inter + JetBrains Mono |

No build step required. All dependencies loaded via CDN.
