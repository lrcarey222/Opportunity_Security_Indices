# ⚡ Allied Industrial Policy — Fantasy Draft

A playful, ESPN-style **fantasy draft** for the clean-energy supply chain. Instead of
picking football players, you're an **allied nation** drafting sub-sectors from the
**Electro-Industrial Stack** — minerals, chips, cells, wires, turbines and molecules.

A Solarpunk / anime–themed companion to the **Opportunity Security Indices (OSI)**
project (Energy Security · Economic Opportunity · Partnership Strength).

![theme](https://img.shields.io/badge/theme-solarpunk%20%C2%B7%20anime-2dd4bf) ·
12 allies · 39 sub-sectors · snake draft

## Play it

Open `index.html` in any modern browser — **no build step, no dependencies, no network calls.**

```bash
# from the repo root
open fantasy-draft/index.html          # macOS
xdg-open fantasy-draft/index.html      # Linux
# ...or serve it
python3 -m http.server -d fantasy-draft 8080  # then visit localhost:8080
```

## How it works

1. **Pick your Ally** — 12 nations, each with a hand-drawn anime mascot, a signature
   title, home-turf synergies, and signature sub-sectors that reflect real industrial
   strengths (Taiwan → advanced logic, South Korea → battery cells, Australia →
   critical minerals, France → nuclear, Germany → machines, and so on).
2. **Snake draft** against the other 11 allies (AI). Each of the 39 sub-sectors is a
   "player" card with an anime industry icon, an **OVR** rating, tier (S/A/B/C), and
   four scouting stats drawn from the OSI scoring framework: **National Security ·
   Energy & Economic Security · Climate Salience · Economic Opportunity** (OVR is
   their mean).
3. **Salary cap** forces trade-offs — you can't just draft every S-tier chokepoint.
4. **Score** = fantasy points (OVR + home synergy + signature fit) **+ sector coverage**
   (breadth across the six stack categories) **+ vertical-stack combos** (e.g. owning
   *Silicon → Materials → Chips* or the *full battery stack*).
5. **Draft grades** and a podium at the end. Run it back with a different ally.

## The Electro-Industrial Stack (the six "positions")

| | Category | Draftable sub-sectors |
|---|---|---|
| **A** | Upstream Materials & Processed Inputs | 1–9 |
| **B** | Semiconductors, Electrochemistry & Machines | 10–19 |
| **C** | Grid & Electricity-System Equipment | 20–25 |
| **D** | Power-Generation Technologies | 26–31 |
| **E** | Electrified End-Use Systems | 32–36 |
| **F** | Molecules, Materials & Carbon Management | 37–39 |

## Files

| File | Purpose |
|---|---|
| `index.html` | App shell + screens (setup / draft / results) |
| `styles.css` | Solarpunk/anime theme, fully responsive |
| `data.js` | The 12 allies, 39 sub-sectors, stats, synergies, scoring |
| `art.js` | Hand-built inline **SVG**: chibi country mascots + industry icons |
| `app.js` | Draft engine (snake order, salary cap, AI, scoring, rendering) |

Everything is self-contained inline SVG — there are **no external image, font, or
script dependencies**, so it runs offline and can be hosted on GitHub Pages as-is.

> Stats are for fantasy fun, not investment advice. 🌱⚡
