# ⚡ Allied Industrial Policy — two games, one stack

A playful, **Street Fighter "PLAYER SELECT"–style** companion to the Opportunity
Security Indices, with two game modes chosen on the home screen:

| | Mode | Players | The question it asks |
|---|---|---|---|
| **1** | [**Fantasy Draft**](#mode-1--fantasy-draft) | 1–21 | *What sectors should **my country** prioritize?* |
| **2** | [**Alliance Architect**](#mode-2--alliance-architect-coordination-mode) | 3–6 | *Given what **every** country wants to prioritize, what negotiated bargains beat acting alone?* |

The Fantasy Draft is a competitive snake draft. Alliance Architect is a
cooperative-competitive negotiation game about **coordinative optimisation** —
it shares the same countries, comparative advantages, art and multiplayer rooms.

Both modes are companions to the **OSI** pillars — Energy Security · Economic
Opportunity · Partnership Strength — and share one taxonomy and one country roster.

![theme](https://img.shields.io/badge/theme-street%20fighter%20arcade-e02440) ·
21 fighters · 39 sub-sectors · snake draft · 9-node battery negotiation

## Play it

Open `index.html` in any modern browser — **no build step, no dependencies, no network calls.**
Pick your game mode on the home screen.

```bash
# from the repo root
open fantasy-draft/index.html          # macOS
xdg-open fantasy-draft/index.html      # Linux
```

```bash
python3 -m http.server -d fantasy-draft 8080  # then visit localhost:8080
```

---

## Mode 1 — Fantasy Draft

Instead of picking fighters, you're an **allied nation** drafting
sub-sectors from the **Electro-Industrial Stack** — minerals, chips, cells, wires,
turbines and molecules.

### How it works

1. **Select your fighter** — a full 21-nation PLAYER SELECT roster, each with a
   Street-Fighter-style portrait, a signature title (THE LEADER, THE CHIP MASTER, …),
   home-turf synergies, and signature sub-sectors reflecting real industrial strengths
   (Taiwan → advanced logic, South Korea → battery cells, Australia → critical minerals,
   France → nuclear, Germany → machines, and so on).
2. **Choose the field** — pick how many allies play (4 → 21) and the number of rounds.
   The rounds available scale automatically so `allies × rounds ≤ 39` sub-sectors. Your
   chosen fighter is always in the draft.
   - **Difficulty** controls how much scouting info you see while drafting:
     **Easy** shows everything (sector OVR/stats + home-turf synergy numbers);
     **Normal** (default) shows only the home-turf synergies and hides sector scores;
     **Hard** hides all numbers, so you draft on names and judgement alone. Sort options
     adapt so you can't sort by a hidden number.
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
6. **Final boss vs China.** The board shows the alliance's **power** — how close the
   collective draft got to the maximum-possible stack (the highest-OVR sub-sectors).
   - **▶ Watch cinematic** — a six-frame storyboard animation whose outcome is decided by the
     draft: reach the win threshold (**85%** of the max stack) and the *Allies Win* sequence
     plays; fall short and the *China Wins* sequence plays.

### Multiplayer leagues (draft with friends)

One person creates a **League**, shares the link, and everyone who opens it joins the
**same draft room** and drafts together in real time. The host decides when to start.

- On the home screen: pick your fighter, type a name, and hit **Create League** — you'll
  get a shareable link + code. Others open the link, pick their own fighter, and **Join**.
- The **host** sees rounds/difficulty controls and a **Start Draft** button; everyone else
  sees "waiting for the host". When the host starts, all players draft in a synchronized
  snake order (with a 45-second auto-pick so nobody can stall the room; the host also
  auto-picks for anyone who disconnects). The final priorities board + standings are shared.

**Enabling real cross-device play (one-time, free):** multiplayer uses Firebase. Until you
add your Firebase config it runs in **local mode** (works only across tabs of the *same*
browser — fine for trying it out). To play with friends on other devices, follow the
step-by-step instructions in **`firebase-config.js`** (create a free Firebase project, turn
on the Realtime Database, paste the web config). Then deploy on **GitHub Pages** (or any
static host) — the multiplayer link won't work inside the Claude artifact preview, which
can't reach outside services.

### The Electro-Industrial Stack (the six "positions")

| | Category | Draftable sub-sectors |
|---|---|---|
| **A** | Upstream Materials & Processed Inputs | 1–9 |
| **B** | Semiconductors, Electrochemistry & Machines | 10–19 |
| **C** | Grid & Electricity-System Equipment | 20–25 |
| **D** | Power-Generation Technologies | 26–31 |
| **E** | Electrified End-Use Systems | 32–36 |
| **F** | Molecules, Materials & Carbon Management | 37–39 |

---

## Mode 2 — Alliance Architect (coordination mode)

> **The question:** given what every country wants to prioritize, what negotiated
> bargains produce a better *allied* industrial strategy than countries acting
> independently?

Alliance Architect is a **3–6 player negotiation game** about the politics of
coordinated specialization. Pick it on the home screen next to the Fantasy Draft.

### What it is, and how it differs from the Fantasy Draft

The Fantasy Draft is **rivalrous**: sub-sectors are exclusive, and you win by
assembling the best national portfolio. Alliance Architect is **not a draft with
more players**. Nothing is exclusive — every country can build cells if it wants
to, and that is exactly the problem. You each run one country's industrial
policy, allocating a limited budget of **policy tokens** across the nodes of a
supply chain, and you are scored on two numbers at once:

- a **national score** (your own industrial and political interest), and
- a shared **alliance score** (whether the allied system actually works).

Countries acting rationally in their own interest reliably produce excessive
duplication in attractive industries, missing nodes in unattractive ones,
subsidy competition, sub-scale plants, single-source dependencies, and a
distribution of benefits that makes cooperation politically fragile. The game
makes you negotiate your way out of that — while still delivering enough at home
to survive politically.

### Coordinative optimisation, in the game

Everyone privately builds a plan that looks sensible for their own country. The
coordinator then aggregates the plans and shows the alliance what it collectively
built. In a typical four-country battery game the first report looks like this:

```
CELLS              303% of requirement   3 countries investing   EXCESS DUPLICATION
PACKS              256%                  3 countries             EXCESS DUPLICATION
Li REFINING         58%                  1 country               UNDER-SUPPLIED
GRAPHITE/ANODE       0%                  0 countries             NOT PLANNED
RECYCLING            0%                  0 countries             NOT PLANNED

UNCOORDINATED ALLIANCE SCORE: 56 / 100
```

Nobody played badly. Every national score in that game sat between 69 and 75 —
those were *good* national plans. The alliance is still broken, because
nobody's plan was the alliance's plan. That gap is the whole game, and closing it
requires real concessions: whoever takes graphite gives up a gigafactory, and
will want compensating for it.

### The five phases (× 2–4 rounds)

| Phase | What happens |
|---|---|
| **1 · National Planning** | Allocate your policy tokens privately across the chain's nodes. Edit freely until you submit. |
| **2 · Coordinator Report** | The app aggregates every plan and publishes system-level problems — *never* anyone's plan. Establishes the **Uncoordinated Baseline**. |
| **3 · Negotiation** | The **Deal Room**: propose, accept, reject, counter or withdraw structured agreements. |
| **4 · Commitment** | Lock your final plan. Accepted agreements are now obligations. You *may* break them. |
| **5 · Results** | Before/after coordination, national outcomes, coalition stability, trust, and who built what. |

The host advances phases; everyone sees the current phase and who is still
working. Players who don't finish (and all AI countries) get a plan generated for
them, so a round can always close.

### What the coordinator does — and deliberately does not do

It is a **coordinating information broker**, not an optimiser. It publishes:

- planned allied capacity per node, as a % of requirement;
- **how many** countries are investing in each node (a count, never a list);
- status flags: `NOT PLANNED`, `SUB-SCALE`, `CRITICAL SHORTFALL`,
  `UNDER-SUPPLIED`, `ADEQUATE`, `ADEQUATE + RESILIENT`, `EXCESS DUPLICATION`,
  plus `SINGLE-SOURCE`, `INPUTS MISSING`, `SUBSIDY WAR`;
- **unused complementarities** — "2 members have strong comparative advantage in
  recycling; allied capacity there is 0%" — derived from *publicly known*
  comparative advantage, not from anyone's plan.

It never states the correct answer, never ranks players, and never reveals a
plan. It gives you strategic intelligence and leaves the bargaining to you.

### How deals work

A proposal is a data object, not a chat message:

```js
{ id, title, proposer, participants: [uid], round,
  commitments: [ { party, type, nodeId, amount } ],
  responses: { uid: "accept" | "reject" }, status, createdAt }
```

Commitment types, and the role each implies:

| Type | Role | Breachable? |
|---|---|---|
| `invest` — invest ≥ N tokens in a node | producer | yes |
| `reduce` — invest ≤ N tokens in a node | restraint | yes |
| `maintain` — hold ≥ N% of allied requirement | secondary / resilience supplier | yes |
| `lead` — act as lead supplier | lead producer | yes |
| `offtake` — guarantee procurement | offtaker | enacted by the engine |
| `finance` — finance partners' capacity | financier | enacted by the engine |
| `rnd` — joint R&D | R&D partner | enacted by the engine |
| `access` — guarantee input access | upstream supplier | enacted by the engine |
| `pact` — join a sectoral pact | member | enacted by the engine |

This is why the game isn't "Japan owns batteries". A single node can have a lead
producer, a resilience second source, a financier and a guaranteed offtaker —
four countries in four different roles. Deals change the physics a little
(finance and joint R&D make a partner's tokens go further; offtake improves
utilisation) and create obligations a lot.

**Defection.** In the Commitment phase your plan is checked against your
promises before you lock. You can break one: you keep the tokens and get a
short-term national gain, but that agreement's benefits are voided for everyone
in it, the alliance score falls, and your reputation drops by 22 (honouring gains
6). Trust carries between rounds and scales the alliance benefit you receive, so
repeated cooperation has strategic value.

### Scoring model

Deterministic, inspectable, and entirely in code — **no LLM is involved in
scoring**. Parameters live in `coord-scenarios.js`; the functions live in
`coord-model.js` (`scoreAlliance`, `scoreCountry`, `calculateSectorCapacity`,
`calculateDuplicationPenalty`, `calculateResilienceBonus`,
`calculateSupplyChainCompleteness`, `generateCoordinatorReport`,
`evaluateShock`, `runStressTest`, `optimalBenchmark`, …).

#### Capacity

```
capacity(node) = installedBase(node) + Σ_countries  tokens × capacityPerToken × efficiency
efficiency     = clamp(0.80 + 0.03 × home[node.category] + affinity[country], 0.60, 1.60)
```

`home[...]` is the *same* home-turf synergy table the Fantasy Draft uses
(`data.js`), so comparative advantage is legible rather than hidden. Capacity and
demand are expressed as **% of allied requirement**, which is why the coordinator
can say "214% of requirement" directly.

#### Alliance score (0–100) — six visible components

| Component | Weight | What it measures |
|---|---|---|
| Supply-chain completeness | 0.22 | Is each node present *and* fed by its dependencies? Credit ramps from minimum viable scale to 85% of requirement — a pilot plant is not a link in a chain. |
| Scale efficiency | 0.18 | 0 at zero capacity → 25 at minimum viable → 100 at ideal → flat to maximum useful → decays beyond. |
| Resilience | 0.18 | Distinct allied suppliers vs the node's `strategicRedundancyTarget`, minus a penalty above 70% concentration. |
| Duplication discipline | 0.14 | 100 = no wasted capital. Penalises overshoot past maximum useful capacity, sub-scale slivers, and more investors than the redundancy target + 1 (the subsidy-war term). Weighted by the tokens at stake. |
| Strategic coverage | 0.16 | Are the strategically important nodes actually supplied? |
| Complementarity | 0.12 | Specialization (Herfindahl over each country's own spend) + whether each dependency edge links *different* countries' leads + declared pacts. |

**Duplication is not automatically bad.** Every node has a
`strategicRedundancyTarget`; reaching it scores full resilience marks, and a node
supplied 100% by one ally is penalised even when its total capacity is fine. Two
or three allied suppliers of a strategic node beats exactly one. Five suppliers
racing each other past the useful ceiling is what gets punished.

#### National score (0–100)

| Component | Cap | Notes |
|---|---|---|
| Comparative-advantage fit | 25 | Did you invest where you are actually good? |
| Domestic industrial value | 30 | `tokens × valueAdd × efficiency`. A *duplicate* plant is only mildly discounted (0.85) — it still employs people and still gets a ribbon cut. Private returns to duplication exceed the alliance's returns, and that gap is the game. A *sub-scale* plant is a national fiasco (0.50). |
| Private political objectives | 16 | 8 points each, two per player (below). |
| Strategic autonomy | 12 | Are your chain needs met at home, or secured by an agreement, or merely available from a concentrated ally? |
| Leadership positions | 9 | Being the alliance's largest supplier of a working node. |
| Alliance benefit | 12 | Spillover from the collective result × your reputation, plus concrete deal benefits received. |
| Costs | −18 max | Tokens spent under obligation in nodes you're weak at, finance/offtake you provided, and defection blowback (net of the defector's short-term gain). |

So you can raise the alliance and lower yourself. That is intentional, and it is
what makes compensation — finance, offtake, R&D, a pact — worth negotiating for.

#### Private national objectives

Two per player, assigned deterministically from `(room code, player)` so every
client derives the same ones with no extra sync. Examples: *keep the factories*,
*alliance leader*, *two domestic nodes*, *employment-intensive build*, *no single
point of failure*, *complete a domestic mini-chain*, *retain the sensitive node*,
*build a scale champion*. They are revealed to everyone at Results, which is
where the distributional politics becomes legible.

#### Coalition stability

A bargain that lifts the alliance by wrecking one member is not a stable bargain.
Stability is 100 while the worst-off member is within 3 points of their own
uncoordinated baseline, then falls linearly, with an extra penalty for anyone
driven below 40. Verdicts: `STABLE` / `WORKABLE` / `FRAGILE` / `UNSTABLE`.

#### Price of non-coordination

```
gap        = benchmark − uncoordinated
captured   = negotiated − uncoordinated
avoided %  = captured / gap
```

The **benchmark** is a greedy allocator that repeatedly hands the next token to
whichever (country, node) pair most improves the alliance score. It is a strong,
reproducible upper reference — not a proof of optimality. If the benchmark fails
to beat the uncoordinated baseline on a measure, the screen says so rather than
printing a meaningless 100%.

> These are **game-model assumptions**, not empirical estimates or welfare
> calculations. They are tuned so the battery scenario is tight enough to force
> specialization, and they can later be calibrated against real OSI
> trade/production data.

### Shocks

From round 2 a shock lands and stays in force (shocks are cumulative). Each one
multiplies scenario parameters — node demand, strategic or resilience importance,
capacity per token — and can change everyone's budget:

| Shock | Effect |
|---|---|
| China imposes graphite & anode export controls | graphite requirement ×1.6, recycling ×1.2, crash-building is less efficient |
| AI electricity demand rips upward | cells/packs/BMS demand ×1.3–1.35, **+2 tokens** each |
| Solid-state / LFP breakthrough | nickel ×0.65, cathode ×1.20 — yesterday's specialization can strand |
| New governments pull back subsidies | **−3 tokens** each |
| Global recession | demand ×0.85 across much of the chain, **−2 tokens** |
| Critical-mineral price collapse | mining requirement ×0.8, refining margins squeezed |

Between rounds, **35% of the previous round's capacity carries forward** as the
installed industrial base, and accepted agreements stay in force. Trust persists.
Cooperation therefore has to be *adaptive*, not a one-shot optimisation.

### End game: the Alliance Stress Test

Not another boss fight. The alliance's final industrial structure is run through
three shocks it did not choose, and compared three ways:

```
Independent national strategies    69% system resilience   (alliance score 57)
Your negotiated alliance           73%                     (alliance score 67)
Best achievable benchmark          89%                     (alliance score 96)
```

`resilience % = share of strategically-weighted allied demand still met`, where:

- **Non-allied export restrictions** — any node with a single allied supplier
  takes a 40% disruption haircut, sub-scale national plants shut, and nodes with
  missing upstream inputs are throttled by their dependency factor.
- **Demand surge** — requirement ×1.4; only plants already at scale can respond.
- **Recession / retrenchment** — output cut 20%, then anything left below plant
  viability closes for good.

"Independent" means the **round-1 submitted plans** evaluated against the *final*
round's conditions — unadapted independent strategies meeting the world as it
ended up, which is the point.

### Battery scenario (the built MVP)

Nine nodes, with dependencies: lithium mining → lithium refining; nickel/cobalt
processing; synthetic graphite & anodes; cathode active materials (needs refining
+ nickel); cells (needs cathode + graphite); modules & packs (needs cells);
battery management systems; recycling (needs packs, feeds refining). Grid,
nuclear, semiconductor and full-39-sector scenarios are declared in
`coord-scenarios.js` but marked `available: false` until their node sets are
authored — the battery game was built end-to-end first, deliberately.

### Playing it

- **Practice vs AI allies** — pick a country, choose scenario/rounds/tokens/AI
  count, and play the whole loop solo. AI countries plan in their own narrow
  national interest and answer proposals with a deterministic cost/benefit rule.
- **With friends** — pick Alliance Architect, hit **Create Alliance Room**, share
  the link. The host picks the scenario, rounds, tokens and how many empty seats
  the AI fills, then opens the room. One country per seat: two players on the
  same country is blocked, since identical comparative advantage would make the
  negotiation meaningless.

### Architectural notes

- **Additive.** The Fantasy Draft's engine, screens and scoring are untouched.
  Alliance Architect reuses the same NET adapters, the same league create/join/
  presence flow, the same country roster and home-turf synergies, the same
  portraits and industry icons, and the same arcade theme.
- **The host is the only writer of scores.** All clients render from the league
  snapshot, and every score is computed once by the host and written to
  `coord/results/<round>`, so no two clients can display different numbers.
- **Determinism instead of extra sync.** Objectives, shock order and AI plans are
  all derived from hashes of the room code and player id — never `Math.random()`.
- **⚠️ Private plans are UI-private, not cryptographically private.** Firebase
  Realtime Database hands every joined client the whole league node, so a
  determined player can open the console and read
  `coord/plans/<round>/<uid>`. No screen renders another player's allocation
  before the coordinator phase, panels that would leak it (your objectives'
  progress, your commitment checks) are computed against *your plan alone*, and
  the coordinator publishes only aggregates and counts. But this is a UI
  guarantee, not a security one. The path shape is chosen so a database rule
  (`".read": "auth.uid === $uid"`) plus server-side aggregation can make it
  genuinely private later, without changing the engine.
- **Triple flexibility** is accommodated in the data model even where the MVP
  content is thin: *compositional* (a proposal carries its own `participants`, so
  a four-country pact inside a six-country game already works), *longitudinal*
  (per-round plans, an installed base that carries forward, persistent trust) and
  *directional* (shocks re-weight which nodes matter).

### Testing

The scoring engine has an 89-assertion suite that runs in **both** Node and the
browser:

```bash
node fantasy-draft/scripts/test-coordination.mjs
```

```bash
python3 -m http.server -d fantasy-draft 8080   # then open localhost:8080/coord-tests.html
```

It covers independent overbuilding, coordinated specialization, strategic
redundancy (two suppliers beating one), excessive redundancy, national sacrifice
(alliance up while one country falls), unstable bargains, the export-control
shock, AI planner determinism, deal compliance and trust, coordinator privacy
(the report is asserted to contain no player id), the benchmark and stress tests,
and two regressions worth naming:

- the "best achievable" benchmark must not be *more brittle* than what players
  build (it was, because a plant big enough to count as a supplier could still be
  too small to survive a stress test — one threshold now governs both);
- the stress test must count the inherited installed base (it rebuilds capacity
  plant-by-plant so shutdown rules can bite, and used to drop the un-owned
  installed base, making every round after the first look artificially fragile);
- a `maintain N%` promise has to be *fundable* — adopting your commitments now
  converts it into the tokens it actually costs at your efficiency, instead of
  silently producing a plan that breaches a deal you just accepted.

Append **`?local=1`** to the URL to force the same-browser adapter even when
Firebase is configured — playtest without writing to the live database.

---

## Files

| File | Purpose |
|---|---|
| `index.html` | App shell + screens (mode select / draft / lobby / results / coordination) |
| `styles.css` | Street Fighter arcade theme, fully responsive |
| `styles-coord.css` | Alliance Architect theme (reuses `styles.css` tokens) |
| `data.js` | The 21 allies, 39 sub-sectors, stats, synergies, scoring |
| `avatars.js` | Cropped fighter portraits (WebP data-URIs) per ally |
| `fight-frames.js` | 12 boss-fight keyframes (WebP data-URIs) cropped from the storyboard |
| `net.js` | Realtime sync layer (Firebase adapter + local BroadcastChannel fallback, `?local=1` override) |
| `lobby.js` | Multiplayer room flow: create / join / lobby / synchronized draft or coordination game |
| `firebase-config.js` | Your Firebase keys + setup instructions (enables cross-device play) |
| `art.js` | Portrait pipeline + inline **SVG** industry icons and fallback mascots |
| `app.js` | Draft engine (ally-count/rounds setup, snake order, AI, scoring) + game-mode picker |
| `coord-scenarios.js` | **Alliance Architect** configuration: scenario nodes, shocks, objectives, stress tests, all weights |
| `coord-model.js` | **Alliance Architect** engine: capacity, alliance/national scoring, coordinator report, deals, trust, AI planner, benchmark, stress tests (pure, no DOM) |
| `coord.js` | **Alliance Architect** UI + phase loop + host orchestration |
| `coord-tests.js` / `coord-tests.html` / `scripts/test-coordination.mjs` | Engine test suite (browser + Node) |

The fighter portraits in `avatars.js` are cropped from the roster art
(`cc86eb27-…png`) and embedded as data-URIs, so the game stays **fully self-contained**
— no external image, font, or script dependencies — and runs offline or on GitHub Pages
as-is. If a portrait is ever missing, a hand-built SVG mascot is used as a fallback.

> Stats are for fantasy fun, not investment advice. ⚡
