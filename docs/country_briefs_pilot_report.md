# Country strategy brief: Australia pilot report

Pilot of the redesigned country memo, "Australia and the Allied Industrial Base — What to secure, build,
source from partners, and deploy" (2026-11-03). Australia only. The other Core Coalition briefs have not been
generated, as the specification asks. Method and pipeline: [country_briefs.md](country_briefs.md).

**Revision of 2026-11-03 (development cooperation).** The brief gained section 4, "Development strategy —
build trusted capacity by helping partners Catch Up", and a sixth Agenda move built on it. The other changes:
- review cases are kept out of the headlines and marked on the figures;
- a shared-capability table keeps the national choice apart from the AIB role;
- a quiet provenance line replaces the AI label in the title area.

**Review fixes of 2026-11-04.** An exhaustive review found 44 problems, and all are applied except one.
- **Constraints.** A partner's constraints are now stated only where the data observe their input.
- **Preserve partners.** These now take the evidence tests.
- **Capability checks.** Five checks keep different-product anchors out of the recommendations: Mozambique,
  Colombia and Tanzania leave battery materials refining, and South Africa leaves rare-earth refining.
- **Aid.** Aid is labelled by where the money is.
- **Coverage.** CM-FDI and OCI read missing, not zero, outside their coverage.
- **Lint.** It now tracks review cases through lists, development rows and plain text.
- **New table.** Section 4 gained a relationship-and-durability table.
- **Not done.** The market-tie field is left as a data limitation.

Australia now has 49 development pairs with 16 partners (from 53 with 19). Verification round 7 checked the
changed sentences.

**Rollout of 2026-11-04 and 2026-11-05.** Nine briefs are now published: Japan, Korea, India and the United
States first, then Germany, the United Kingdom, France and Canada. Each was built the same way:
1. an AI writer drafted it;
2. an independent verifier checked every sentence;
3. the verifier's fixes were applied;
4. a second verifier pass checked every changed sentence.

The second batch also fixed four things in the generator:
- **Demand-anchor shares.** A demand anchor's lead share in the AIB is a share of world imports. Figure 3 had
  shown it in the supplier cell, marked essential, so the United States read as an essential supplier in solar,
  wind, grid and industry. The share now sits in the demand cell, and the fact's meaning text says "share of
  world imports".
- **Figure titles** are capitalized.
- **"1 product group"** is singular.
- **The matrix meaning text** no longer names Australia for every country.

The affected briefs were re-signed and republished. No prose changed.

The PDF is 16 pages: the main text, then the appendix. The Australia development audit, validation
lists and data limitations are in
[development_partnerships_australia_audit.md](development_partnerships_australia_audit.md). Verification
round 6 checked the 178 new or changed sentences in three passes. It found 2 errors, 7 warnings and 12 notes,
all fixed, and none remain open.

## 1. The rendered memo

The brief is published in `dashboard/public/data/country/` in the dashboard's four formats:
- the screen view (`memo-AUS.<h8>.json`, at `/countries/AUS/memo`)
- PDF
- Word
- Markdown

There are also four figures, each as SVG and PNG. The PDF runs to about 12 pages: about 7½ pages of main text
and sources, then a 4-page appendix. The main text is about 1,900 words. The 12 tables add about 700 words.
One analytical object drives every format: the facts, the source and the document AST.

| Spec item | Where it is |
|---|---|
| Bottom line: thesis and the choice box | Section 1 |
| Chart 1: Strategy Map, mixed steps flagged | Figure 1 |
| High fence table: 4 capabilities | Section 2 |
| Medium fence: Preserve, Catch Up, Partner | Section 2 |
| Chart 2: value × position | Figure 2 |
| Low fence: 5 restraint cases | Section 2 |
| Allied bargain table: 4 bargains | Section 3 |
| Chart 3: compact × role matrix | Figure 3 |
| Energy-system priorities | Section 4 |
| Chart 4: deployment ranking (conditional; included) | Figure 4 |
| Policy direction | Section 5 |
| Chart 5: strategy–policy gaps (generated table, 10 rows) | Section 5 |
| Strategic agenda: 6 moves | Section 6 |
| Appendix | Every step, layers, partner recommendations, compact roles, coverage, method and caveats |

## 2. The content trace

`dashboard/country-memos/AUS/brief.trace.md` and `brief.trace.csv` trace every claim (864). Each claim
carries:
- the sentence it supports
- the displayed value
- the fact path
- the dashboard field it comes from, for example `posture core cells.prize` or
  `posture detail cells[].partners`
- the source datasets
- the evidence quality: position evidence, choice confidence, deployment-need confidence, partner evidence,
  or "AIB model; thin trade line"

An independent verifier checks every claim and sentence. Its record is `brief.verify.json`, and publication
requires that record to pass.

## 3. Implementation report

### What changed from the old memo structure

The old memos had five fixed sections:
1. Energy security risks
2. Economic opportunities
3. Sectoral priorities and policy posture
4. Partnership priorities
5. Data notes

They were written around the Energy Security and Economic Opportunity scores, the four corners, Strategic
Index ranks, the allied pick and Partnership Strength ranks. They had no figures or tables.

The brief is organized instead by the Yards & Fences choices, the Allied Industrial Base and deployment.
It makes recommendations; the old memos' rule was "inform, don't advocate". It also carries figures and
tables. The old analytical narrative is gone. These parts were kept:
- every number as a `{{token}}` resolved against the data
- the claims file
- an independent verifier
- the staleness hashes
- the generated source list
- the "AI-generated from the data and checked against it" label, now "data as of" instead of "NEIS
  baseline as of"
- the Edge-PDF and python-docx renderers, with read-back checks

### Elements removed

- The Energy Security and Economic Opportunity percentiles, and the four corners (Grow, Upgrade, Exposed,
  Stable).
- Strategic Index ranks and the allied pick.
- The Partnership Strength ranking as the organizing logic of partners.
- The retired allied-network design shares.
- Counts of policy records used as context.
- The old tone rule that banned recommendations.

### How narrative selection works

Selection rules in R, `R/dashboard/country/country_brief*.R`, pick the evidence. The writer then makes
judgments on what they pick.

- **High fence.** Secure steps and Secure layers are grouped into capabilities that one policy answer covers
  (config `high_fence_groups`). They are ranked by their highest need. The table takes capabilities with an
  acute route: a chokepoint or a remote command path.
- **Medium fence.** Preserve, Catch Up and Partner are listed by Strategic Industrial Value. Catch Up shows
  its anchors, and the brief says when an anchor is thin.
- **Low fence.** The cases are processing and manufacturing steps left open (Watch). They are ranked first by
  whether the energy system needs the technology (its deployment is Accelerate), then by whether recorded
  policy over-prioritizes the step, then by value.
- **Policy gaps.** These are mismatches between the choice and recorded policy. There are five kinds:
  - Secure with weak policy
  - Partner with localization
  - a build choice with weak attention
  - Accelerate with weak deployment policy
  - Watch with protection

  The table takes at most 3 of each kind and 10 in all. Choices of low confidence are left out; the oil-use
  Accelerate is one.
- **Allied bargain.** Each bargain draws on four sources:
  - AIB strategic compact roles and core seats
  - the allied division of labor
  - posture partner recommendations, including whether allied supply covers the need
  - investment flows

  Every mechanism is one the posture policy design configures for that choice, from
  `core.design[variant|delivery]` and `config/posture_policy_design.csv`. Examples:
  - offtake and co-investment with the recommended partners
  - joint ventures that transfer capability
  - time-limited production support tied to benchmarks
  - stockpiles until trusted supply qualifies
  - trusted-source and certification rules
  - infrastructure, permitting and procurement for deployment

  Mechanisms in an earlier draft that are not configured, such as price floors and price support, were
  removed.

### Charts built

1. The Strategy Map. Corner flags mark mixed steps in fence colors: High orange, Medium violet, Low grey. Every
   cell carries a text label, so it reads in grayscale.
2. Strategic Industrial Value × Competitive Position. Markers are by choice, Secure is ringed, the bars are
   dashed and 10 points are labelled. Steps on the same spot are merged into one label, for example "wind
   components and assembly".
3. The compact × role matrix. It shows Supplier, Capacity host, Demand anchor, Bridge / coordination and
   Relies on partners, at four levels: essential, holds the role, relies, and relies where allied supply
   falls short.
4. The deployment ranking. It shows Accelerate against Watch, the main driver of each step's need, and
   lighter bars where the measure rests on a single indicator with low confidence. It is included because
   deployment need differs widely across technologies, from 83 to 18.
5. Strategy–policy gaps. This is a generated table, since a table communicates it better than a chart.

### Data that limited the ideal structure

- **Capital / investor role.** The AIB has no compact-level investor role for Australia. It narrowly misses
  the Capital & Investment Anchor role. The investment flows are new capacity abroad that would serve
  Australia's demand, not Australian capital. So Figure 3 has no capital column, and capital appears only
  in the text.
- **Development host.** This role is for middle-income economies only, so it does not apply to Australia.
- **Compacts and posture steps.** The strategic compacts are AIB node groups, and the posture is technology ×
  stage. Nothing in the bundles maps one to the other, so the matrix's "relies on partners" column uses an
  editorial mapping (config `compacts`).
- **Not assessed.** 18 steps cannot be assessed: all of green hydrogen and geothermal manufacturing, and most
  recycling. That gap matters most for hydrogen, which recorded policy ranks among Australia's highest
  priorities.
- **Uranium.** The uranium figure (75% of world exports or production) comes from an aggregated, thinly
  covered trade line. Australia's core seat in the nuclear compact rests on it.

### Where judgment rules were required

- **Uranium conversion and enrichment.** The map reads it Catch Up, but one of its anchors is a 1% trade share
  that more likely reflects uranium compounds than conversion capacity. The brief does not treat it as a
  priority.
- **Grain-oriented electrical steel.** It reads Catch Up only by inheriting its step's anchor, so the brief
  treats it as an input to source.
- **Oil use.** It reads Accelerate on one indicator, so the brief treats it as a fuel-security warning.
- **The two mining steps.** EV mineral mining is Preserve and battery mineral mining is Watch, on the same
  trade data. The brief reads them as one allied supply position.
- **Solar modules.** The PV module layer is Watch but inverters are Partner or Secure, so the brief names
  inverters, not modules, as the Partner case.

## 4. Open questions

1. **Methodology against narrative.**
   - The mining steps disagree on identical trade data.
   - The Secure refining step and battery materials refining share the same China share of world supply,
     which suggests overlapping trade codes.
   - The large-yard rule leaves high-value nuclear and gas equipment as Watch on low need.

   Should the method merge the two mining steps, or give each its own basket?
2. **Missing AIB roles.**
   - There is no capital role at compact level.
   - No investor field credits Australian outward investment.
   - The "capacity-building host" function is not credited as hosting.

   A derived field, "economy as investor", would let Figure 3 show capital.
3. **Sparse evidence.** The policy gap for uranium conversion rests on a thin anchor. Hydrogen and geothermal
   cannot be assessed. Seven of the ten Partner steps have only moderate partner evidence.
4. **New derived fields the generator would need.**
   - A compact ↔ posture-step crosswalk in the data, to replace the editorial config mapping.
   - A layer-level partner recommendation, for example for magnets.
   - A flag for an inherited layer anchor.
5. **Recommendations that rest on inference rather than a modeled result.**
   - **The bargains.** The pairing of what Australia gives and what it gets is the brief's judgment. The
     partners and mechanisms come from the data.
   - **"Price cycles".** The point that mining reliability depends on price cycles is not modeled.
   - **The legal setting of enrichment.** Australian law prohibits enrichment plants. This is outside the
     data; the brief says only that the case depends on settings the analysis does not score.

   Should briefs allow a short, sourced context note?
6. **Length.** The main text is about 7½ pages because of the five figures and three main-text tables;
   1,900 words is within the spec. Should the policy-gap table move to the appendix, or Figure 4 become a
   compact table?
7. **Subtitle.** It is kept as given. "What to secure, build, source from partners, and deploy" fits the
   content.
