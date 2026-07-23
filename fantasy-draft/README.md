# ⚡ Allied Industrial Policy — Fantasy Draft

A playful, **Street Fighter "PLAYER SELECT"–style** fantasy draft for the clean-energy
supply chain. Instead of picking fighters, you're an **allied nation** drafting
sub-sectors from the **Electro-Industrial Stack** — minerals, chips, cells, wires,
turbines and molecules.

A companion to the **Opportunity Security Indices (OSI)** project
(Energy Security · Economic Opportunity · Partnership Strength).

![theme](https://img.shields.io/badge/theme-street%20fighter%20arcade-e02440) ·
up to 21 fighters · 39 sub-sectors · snake draft

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

1. **Select your fighter** — a full 21-nation PLAYER SELECT roster, each with a
   Street-Fighter-style portrait, a signature title (THE LEADER, THE CHIP MASTER, …),
   home-turf synergies, and signature sub-sectors reflecting real industrial strengths
   (Taiwan → advanced logic, South Korea → battery cells, Australia → critical minerals,
   France → nuclear, Germany → machines, and so on).
2. **Choose the field** — pick how many allies play (4 → 21) and the number of rounds.
   The rounds available scale automatically so `allies × rounds ≤ 39` sub-sectors. Your
   chosen fighter is always in the draft.
3. **Snake draft** against the AI allies — a pure snake draft, no salary cap. Each of the
   39 sub-sectors is a "player" card with an industry icon, an **OVR** rating, tier
   (S/A/B/C), and four scouting stats rendered as HP life-bars, drawn from the OSI scoring
   framework: **National Security · Energy & Economic Security · Climate Salience ·
   Economic Opportunity** (OVR is their mean).
4. **Score** = fantasy points (OVR + home synergy + signature fit) **+ sector coverage**
   (breadth across the six stack categories) **+ vertical-stack combos** (e.g. owning
   *Silicon → Materials → Chips* or the *full battery stack*).
5. **Draft Priorities board** — the final screen reveals what the *group* valued: every
   sub-sector ranked by the overall draft order (pick #1 = top alliance priority, with the
   ones nobody took shown as "passed over"), a per-category demand summary, and the final
   standings. Rematch with a new fighter.
6. **Final boss: FIGHT China.** The board shows the alliance's **power** — how close the
   collective draft got to the maximum-possible stack (the highest-OVR sub-sectors). Hit
   **FIGHT!** and a six-frame **storyboard animation** plays out the bout against China, the
   big boss. Reach the win threshold (**85%** of the max stack) and the *Allies Win* sequence
   plays (China attacks → allies defend → China returns to human → allies win); fall short and
   the *China Wins* sequence plays (China powers up → transforms into a dragon → dragon
   attack → China wins). Prioritize high-value sub-sectors to win.

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
| `index.html` | App shell + screens (select / draft / results) |
| `styles.css` | Street Fighter arcade theme, fully responsive |
| `data.js` | The 21 allies, 39 sub-sectors, stats, synergies, scoring |
| `avatars.js` | Cropped fighter portraits (WebP data-URIs) per ally |
| `fight-frames.js` | 12 boss-fight keyframes (WebP data-URIs) cropped from the storyboard |
| `art.js` | Portrait pipeline + inline **SVG** industry icons and fallback mascots |
| `app.js` | Draft engine (ally-count/rounds setup, snake order, salary cap, AI, scoring) |

The fighter portraits in `avatars.js` are cropped from the roster art
(`cc86eb27-…png`) and embedded as data-URIs, so the game stays **fully self-contained**
— no external image, font, or script dependencies — and runs offline or on GitHub Pages
as-is. If a portrait is ever missing, a hand-built SVG mascot is used as a fallback.

> Stats are for fantasy fun, not investment advice. ⚡
