/* ============================================================================
   ALLIANCE ARCHITECT — SCENARIO CONFIGURATION (pure data, no logic)
   ----------------------------------------------------------------------------
   Every number in this file is a GAME-MODEL ASSUMPTION, not an empirical
   estimate. They are tuned so that the battery scenario is *tight*: an alliance
   of 4-6 countries has just about enough policy tokens to build a complete,
   at-scale, resilient chain — but only if it specializes. Independent national
   plans reliably overbuild the glamorous nodes (cells, packs) and starve the
   unglamorous ones (graphite, recycling), which is the whole point of the game.
   Later these can be calibrated against OSI trade/production data.

   UNITS
     • capacity + demand are expressed in "% of alliance requirement", so a node
       at 100 means allied plans exactly cover allied need. This makes the
       coordinator report directly readable ("214% of requirement").
     • a policy token is one unit of national industrial-policy effort per round.
       capacityPerToken converts tokens → capacity at efficiency 1.0.

   NODE FIELDS
     id, name, short      identifiers / labels
     cat                  Electro-Industrial Stack category (reuses data.js CATEGORIES)
     subN                 the closest of the 39 draft sub-sectors (icon + lineage)
     glyph                art.js icon key
     stage                1..6, left→right position in the supply-chain map
     demand               base requirement before alliance-size + shock scaling
     minViableCapacity    below this a facility is sub-scale (no useful output)
     idealCapacity        the economically efficient target
     maximumUsefulCapacity above this, extra capital is waste / subsidy war
     strategicRedundancyTarget  how many distinct allied suppliers we *want*
     dependencies         upstream node ids required for this node to function
     feeds                downstream nodes this node supplies (circularity)
     strategicImportance  0..1 weight in strategic-coverage scoring
     resilienceImportance 0..1 weight in resilience scoring
     valueAdd             0..1 national economic value per unit of capacity
     employmentIntensity  0..1 jobs per unit of capacity (used by objectives)
     chinaExposure        0..1 share of today's world capacity outside the alliance
     capacityPerToken     capacity bought by 1 token at efficiency 1.0
     affinity             { countryId: bonus } comparative-advantage bonus 0..0.6
   ============================================================================ */
(function (root) {
  "use strict";

  /* ---------------------------------------------------------------------------
     GLOBAL MODEL CONSTANTS  (all tunable; documented in the README)
     --------------------------------------------------------------------------- */
  const COORD_CONFIG = {
    title: "Alliance Architect",          // ← rename the game mode here
    tagline: "Negotiate the strongest collective industrial strategy.",
    tokensPerRound: 10,
    roundsDefault: 3,
    minPlayers: 3,
    maxPlayers: 6,

    /* THE single plant-viability threshold, as a share of a node's requirement.
       One number, used in three places that must agree with each other:
         • a country counts as a "supplier" of a node (resilience scoring)
         • below it, a national plant is a wasteful "sliver" (duplication scoring)
         • below it, a national plant shuts down under stress (stress tests)
       Keeping these identical matters: if the score rewarded suppliers the
       stress test then closed, the score-optimal alliance would be brittle by
       construction — which is a bug in the model, not an insight. */
    meaningfulShare: 0.15,

    /* Fraction of last round's capacity that survives as installed base.
       Gives the game longitudinal texture without letting round 3 trivially
       overshoot every requirement. */
    inheritanceFactor: 0.35,

    /* Requirement scaling: allied demand grows with membership, but sub-linearly
       (more members = more consumers AND more builders). 4p→1.01, 6p→1.24. */
    demandScale: (nPlayers) => 0.55 + 0.115 * Math.max(1, nPlayers),

    /* countryEfficiency = clamp(effBase + effHomeWeight*home[cat] + affinity) */
    effBase: 0.80,
    effHomeWeight: 0.03,
    effMin: 0.60,
    effMax: 1.60,

    /* Alliance score component weights (sum = 1). */
    allianceWeights: {
      completeness: 0.22,      // are the chain's nodes present & fed?
      scale: 0.18,             // is capacity near economically useful scale?
      resilience: 0.18,        // are strategic nodes multi-sourced?
      duplication: 0.14,       // is allied capital being wasted?
      strategic: 0.16,         // are strategically important nodes covered?
      complementarity: 0.12,   // do national plans interlock across borders?
    },

    /* National score component caps (sum of positives = 104, clamped to 100). */
    nationalCaps: {
      fit: 25,                 // did you invest where you are actually good?
      domestic: 30,            // industrial value captured at home
      objectives: 16,          // private political objectives (8 each)
      autonomy: 12,            // chain needs met at home or by secured allies
      leadership: 9,           // being the alliance's lead supplier somewhere
      allianceBenefit: 12,     // spillover + deal benefits (× trust)
    },
    objectivePoints: 8,
    objectivesPerPlayer: 2,

    /* Coalition stability: a bargain is unstable if any member ends up worse
       than their own uncoordinated plan by more than this, or below the floor. */
    stabilityTolerance: 3,
    stabilityFloor: 40,

    /* Trust / reputation. */
    trustStart: 60,
    trustHonorGain: 6,
    trustDefectLoss: 22,
    trustMin: 0,
    trustMax: 100,

    /* Deal effects (deterministic, additive). */
    dealEffects: {
      financeEfficiencyBonus: 0.15,   // financier → recipient efficiency in node
      rndEfficiencyBonus: 0.10,       // joint R&D → both parties in node
      offtakeScaleBonus: 0.12,        // guaranteed demand → utilization credit
      financeCostTokens: 1,           // financier's national cost per finance leg
      pactComplementarityBonus: 4,    // points on the complementarity component
    },
  };

  /* ---------------------------------------------------------------------------
     BATTERY SCENARIO — the MVP scenario, built end-to-end.
     --------------------------------------------------------------------------- */
  const BATTERY_NODES = [
    {
      id: "lithium_mining", name: "Lithium Mining", short: "Li MINING",
      cat: "A", subN: 1, glyph: "pickaxe", stage: 1,
      demand: 100, minViableCapacity: 25, idealCapacity: 100, maximumUsefulCapacity: 135,
      strategicRedundancyTarget: 2, dependencies: [], feeds: ["lithium_refining"],
      strategicImportance: 0.70, resilienceImportance: 0.70,
      valueAdd: 0.45, employmentIntensity: 0.80, chinaExposure: 0.30,
      capacityPerToken: 26,
      affinity: { AU: 0.55, CA: 0.35, US: 0.25, BR: 0.30, PT: 0.20, FI: 0.15 },
      note: "Brine and hard-rock extraction. Cheap tokens, low value-add, big jobs footprint.",
    },
    {
      id: "lithium_refining", name: "Lithium Refining & Chemical Conversion", short: "Li REFINING",
      cat: "A", subN: 2, glyph: "flask", stage: 2,
      demand: 100, minViableCapacity: 30, idealCapacity: 100, maximumUsefulCapacity: 130,
      strategicRedundancyTarget: 2, dependencies: ["lithium_mining"], feeds: ["cathode_cam"],
      strategicImportance: 0.85, resilienceImportance: 0.90,
      valueAdd: 0.60, employmentIntensity: 0.50, chinaExposure: 0.65,
      capacityPerToken: 22,
      affinity: { FI: 0.35, KR: 0.30, JP: 0.30, AU: 0.25, US: 0.25, CA: 0.20, DE: 0.15 },
      note: "The step China quietly owns. Mining without refining leaves the value offshore.",
    },
    {
      id: "nickel_processing", name: "Nickel & Cobalt Processing", short: "Ni/Co PROCESSING",
      cat: "A", subN: 5, glyph: "ingot", stage: 1,
      demand: 100, minViableCapacity: 30, idealCapacity: 100, maximumUsefulCapacity: 130,
      strategicRedundancyTarget: 2, dependencies: [], feeds: ["cathode_cam"],
      strategicImportance: 0.60, resilienceImportance: 0.60,
      valueAdd: 0.55, employmentIntensity: 0.60, chinaExposure: 0.55,
      capacityPerToken: 20,
      affinity: { CA: 0.40, FI: 0.35, AU: 0.30, JP: 0.25, NO: 0.20, US: 0.15 },
      note: "High-nickel chemistries need it; LFP chemistries don't. Exposed to technology shifts.",
    },
    {
      id: "graphite_anode", name: "Synthetic Graphite & Anode Materials", short: "GRAPHITE/ANODE",
      cat: "A", subN: 3, glyph: "layers", stage: 2,
      demand: 100, minViableCapacity: 30, idealCapacity: 100, maximumUsefulCapacity: 145,
      strategicRedundancyTarget: 2, dependencies: [], feeds: ["cells"],
      strategicImportance: 0.95, resilienceImportance: 1.00,
      valueAdd: 0.70, employmentIntensity: 0.50, chinaExposure: 0.92,
      capacityPerToken: 18,
      affinity: { JP: 0.45, KR: 0.30, SE: 0.30, NO: 0.30, CA: 0.25, US: 0.20, FI: 0.20 },
      note: "The alliance's sharpest chokepoint: ~90% non-allied today, and nobody's favourite project.",
      chokepoint: true,
    },
    {
      id: "cathode_cam", name: "Cathode Active Materials", short: "CATHODE (CAM)",
      cat: "B", subN: 14, glyph: "cathode", stage: 3,
      demand: 100, minViableCapacity: 30, idealCapacity: 100, maximumUsefulCapacity: 135,
      strategicRedundancyTarget: 2, dependencies: ["lithium_refining", "nickel_processing"], feeds: ["cells"],
      strategicImportance: 0.80, resilienceImportance: 0.80,
      valueAdd: 0.80, employmentIntensity: 0.50, chinaExposure: 0.80,
      capacityPerToken: 18,
      affinity: { JP: 0.50, KR: 0.45, FI: 0.35, DE: 0.25, US: 0.20, SE: 0.20, PL: 0.20 },
      note: "Where cell chemistry — and most of the cell's material cost — is decided.",
    },
    {
      id: "cells", name: "Battery Cells", short: "CELLS",
      cat: "B", subN: 16, glyph: "battery", stage: 4,
      demand: 100, minViableCapacity: 35, idealCapacity: 100, maximumUsefulCapacity: 130,
      strategicRedundancyTarget: 3, dependencies: ["cathode_cam", "graphite_anode"], feeds: ["packs"],
      strategicImportance: 0.90, resilienceImportance: 0.85,
      valueAdd: 1.00, employmentIntensity: 0.90, chinaExposure: 0.75,
      capacityPerToken: 14,
      affinity: { KR: 0.55, JP: 0.40, PL: 0.35, US: 0.30, DE: 0.30, SE: 0.30, IN: 0.25, HU: 0.25 },
      note: "The gigafactory trophy. Everyone wants it, it is the most token-hungry node in the chain.",
    },
    {
      id: "packs", name: "Modules, Packs & Integration", short: "PACKS",
      cat: "B", subN: 16, glyph: "storage", stage: 5,
      demand: 100, minViableCapacity: 25, idealCapacity: 100, maximumUsefulCapacity: 140,
      strategicRedundancyTarget: 3, dependencies: ["cells"], feeds: ["recycling"],
      strategicImportance: 0.50, resilienceImportance: 0.40,
      valueAdd: 0.75, employmentIntensity: 1.00, chinaExposure: 0.70,
      capacityPerToken: 24,
      affinity: { DE: 0.45, MX: 0.40, US: 0.35, PL: 0.35, KR: 0.30, IN: 0.30, JP: 0.25, ES: 0.25, TR: 0.25 },
      note: "Assembly close to the vehicle plant. Jobs-rich, politically easy, strategically shallow.",
    },
    {
      id: "bms", name: "Battery Management Systems & Power Electronics", short: "BMS",
      cat: "B", subN: 13, glyph: "sensor", stage: 5,
      demand: 100, minViableCapacity: 20, idealCapacity: 100, maximumUsefulCapacity: 140,
      strategicRedundancyTarget: 2, dependencies: [], feeds: ["packs"],
      strategicImportance: 0.60, resilienceImportance: 0.50,
      valueAdd: 0.85, employmentIntensity: 0.30, chinaExposure: 0.40,
      capacityPerToken: 30,
      affinity: { US: 0.45, TW: 0.40, DE: 0.40, JP: 0.35, NL: 0.30, SG: 0.30, KR: 0.25, IT: 0.20 },
      note: "Cheap in tokens, high in value-add. Chronically under-planned because it isn't a ribbon-cutting.",
    },
    {
      id: "recycling", name: "Battery Recycling & Black Mass", short: "RECYCLING",
      cat: "F", subN: 39, glyph: "carbon", stage: 6,
      demand: 100, minViableCapacity: 25, idealCapacity: 100, maximumUsefulCapacity: 135,
      strategicRedundancyTarget: 2, dependencies: ["packs"], feeds: ["lithium_refining", "nickel_processing"],
      strategicImportance: 0.55, resilienceImportance: 0.50,
      valueAdd: 0.60, employmentIntensity: 0.70, chinaExposure: 0.70,
      capacityPerToken: 22,
      affinity: { DE: 0.35, US: 0.30, KR: 0.30, SE: 0.30, FI: 0.30, JP: 0.25, NO: 0.25, CA: 0.20 },
      note: "The alliance's only domestic mine that grows over time. Always last in the queue.",
      circular: true,
    },
  ];

  /* ---------------------------------------------------------------------------
     SHOCKS — applied between rounds. Effects are multiplicative on scenario
     parameters, plus optional budget deltas. Deterministic and inspectable.
     --------------------------------------------------------------------------- */
  const COORD_SHOCKS = [
    {
      id: "graphite_controls",
      title: "China imposes graphite & anode export controls",
      kicker: "Directional shock",
      blurb: "Licences now required for synthetic graphite and anode-grade material. Allied cell lines have 90 days of inventory.",
      effects: {
        demandMultiplier: { graphite_anode: 1.60, recycling: 1.20 },
        strategicMultiplier: { graphite_anode: 1.15, recycling: 1.15 },
        resilienceMultiplier: { graphite_anode: 1.10 },
        capacityPerTokenMultiplier: { graphite_anode: 0.85 },   // crash-building is inefficient
        budgetDelta: 0,
      },
      teaches: "A single chokepoint node nobody wanted to build now sets the ceiling for the whole chain.",
    },
    {
      id: "ai_demand",
      title: "AI electricity demand rips upward",
      kicker: "Demand shock",
      blurb: "Data-centre buildouts pull storage, cells and power electronics forward by five years. Treasuries open the taps.",
      effects: {
        demandMultiplier: { cells: 1.35, packs: 1.30, bms: 1.35, cathode_cam: 1.15 },
        strategicMultiplier: { bms: 1.20, cells: 1.05 },
        budgetDelta: +2,
      },
      teaches: "More money does not fix a chain whose upstream is still missing.",
    },
    {
      id: "new_chemistry",
      title: "Solid-state / LFP breakthrough reaches commercial scale",
      kicker: "Technology shock",
      blurb: "A licensed chemistry cuts nickel intensity and rewrites cathode and anode specifications.",
      effects: {
        demandMultiplier: { nickel_processing: 0.65, cathode_cam: 1.20, graphite_anode: 0.85 },
        strategicMultiplier: { nickel_processing: 0.75, cathode_cam: 1.10 },
        capacityPerTokenMultiplier: { cathode_cam: 0.90 },
        budgetDelta: 0,
      },
      teaches: "Yesterday's optimal specialization can become tomorrow's stranded asset.",
    },
    {
      id: "subsidy_retrenchment",
      title: "New governments pull back industrial subsidies",
      kicker: "Political shock",
      blurb: "Two members' legislatures cut industrial-policy appropriations. Everyone's budget shrinks.",
      effects: {
        demandMultiplier: {},
        budgetDelta: -3,
      },
      teaches: "Coordination is worth more when money is short, and is hardest to sustain then.",
    },
    {
      id: "recession",
      title: "Global recession cuts public spending and demand",
      kicker: "Macro shock",
      blurb: "EV sales slow, treasuries retrench, and sub-scale plants start losing money.",
      effects: {
        demandMultiplier: { cells: 0.85, packs: 0.85, lithium_mining: 0.85, nickel_processing: 0.85 },
        strategicMultiplier: { recycling: 1.10 },
        budgetDelta: -2,
      },
      teaches: "Overbuilt capacity becomes a fiscal liability, not an insurance policy.",
    },
    {
      id: "mineral_collapse",
      title: "Critical-mineral prices collapse",
      kicker: "Price shock",
      blurb: "Lithium spot prices fall 70%. Mines are cheap to run and impossible to finance; refining margins vanish.",
      effects: {
        demandMultiplier: { lithium_mining: 0.80 },
        capacityPerTokenMultiplier: { lithium_mining: 1.30, lithium_refining: 0.85 },
        strategicMultiplier: { lithium_refining: 1.10 },
        budgetDelta: 0,
      },
      teaches: "Price cycles hit the upstream first — the partner who took the unglamorous node needs compensating.",
    },
  ];

  /* ---------------------------------------------------------------------------
     PRIVATE NATIONAL POLITICAL OBJECTIVES
     Each is checked deterministically against the final round state.
     `fits(country)` filters the pool so objectives are country-appropriate.
     --------------------------------------------------------------------------- */
  const COORD_OBJECTIVES = [
    {
      id: "keep_manufacturing",
      label: "Keep the factories",
      detail: "Hold at-scale capacity in at least one manufacturing node (cathode, cells, packs or BMS).",
      why: "Your industry minister has promised a ribbon-cutting.",
      fits: () => true,
    },
    {
      id: "sector_leader",
      label: "Alliance leader",
      detail: "Be the single largest allied supplier in at least one node.",
      why: "Leadership in a node is leverage in every future negotiation.",
      fits: () => true,
    },
    {
      id: "two_nodes",
      label: "Two domestic nodes",
      detail: "Hold meaningful capacity (≥15% of allied requirement) in at least two different nodes.",
      why: "Your cabinet does not want the country reduced to a single-industry economy.",
      fits: () => true,
    },
    {
      id: "jobs",
      label: "Employment-intensive build",
      detail: "Reach a jobs-weighted capacity score of 55+ (favours mining, cells and packs).",
      why: "An election is 14 months away in the industrial heartland.",
      fits: () => true,
    },
    {
      id: "no_single_source",
      label: "No single point of failure",
      detail: "No strategically important node may be more than 65% supplied by one country.",
      why: "Your security council has read the graphite cable traffic.",
      fits: () => true,
    },
    {
      id: "mini_chain",
      label: "Complete a domestic mini-chain",
      detail: "Hold meaningful capacity in a node AND in one of its direct upstream dependencies.",
      why: "Sovereign capability means owning two links, not one.",
      fits: () => true,
    },
    {
      id: "retain_sensitive",
      label: "Retain the sensitive node",
      detail: "Keep meaningful capacity in your country's strongest node — the one your incumbents already own.",
      why: "The national champion has the trade minister's phone number.",
      fits: () => true,
    },
    {
      id: "scale_champion",
      label: "Build a scale champion",
      detail: "Supply at least 40% of the alliance's requirement in one single node.",
      why: "Export-scale plants, not boutique ones, pay for themselves.",
      fits: () => true,
    },
  ];

  /* ---------------------------------------------------------------------------
     ALLIANCE STRESS TESTS — the end-game, replacing the "fight China" boss.
     Each returns a resilience % : the share of weighted allied demand still met
     after the stress is applied. Formulas live in coord-model.js (runStressTest)
     and are documented in the README.
     --------------------------------------------------------------------------- */
  const COORD_STRESS_TESTS = [
    {
      id: "export_restrictions",
      title: "Non-allied export restrictions",
      blurb: "Every non-allied supply route closes. Allied capacity is all you have — and any node supplied by a single ally takes a disruption haircut.",
      kind: "export_restrictions",
      params: { singleSourceHaircut: 0.40, subScaleShutdown: true },
    },
    {
      id: "demand_surge",
      title: "AI + electrification demand surge",
      blurb: "Allied requirement jumps 40% in two years. Only capacity that is already at scale can respond.",
      kind: "demand_surge",
      params: { demandMultiplier: 1.40 },
    },
    {
      id: "fiscal_retrenchment",
      title: "Recession & subsidy retrenchment",
      blurb: "Budgets are cut, output is trimmed 20%, and any plant left below minimum viable scale closes for good.",
      kind: "fiscal_retrenchment",
      params: { capacityHaircut: 0.20, subScaleShutdown: true },
    },
  ];

  /* ---------------------------------------------------------------------------
     SCENARIOS. Only "batteries" is fully built for the MVP; the others are
     declared so the picker (and future work) has a home. `available:false`
     keeps them out of the UI until their node sets are authored.
     --------------------------------------------------------------------------- */
  const COORD_SCENARIOS = {
    batteries: {
      id: "batteries",
      name: "Battery Chain",
      kicker: "Lithium to recycling · 9 nodes",
      blurb: "The alliance needs a complete battery supply chain: mines, refineries, anodes, cathodes, cells, packs, brains and recycling. Nobody can build all nine.",
      available: true,
      nodes: BATTERY_NODES,
      shocks: ["graphite_controls", "ai_demand", "new_chemistry", "subsidy_retrenchment", "recession", "mineral_collapse"],
      /* Shock order is fixed per game so all clients agree without extra sync;
         coord-model picks by round index from this list. */
      shockOrder: ["graphite_controls", "ai_demand", "new_chemistry", "recession"],
      stressTests: ["export_restrictions", "demand_surge", "fiscal_retrenchment"],
    },
    grid: { id: "grid", name: "Grid Equipment", kicker: "Transformers to HVDC", available: false,
      blurb: "Electrical steel, transformers, switchgear, cables, HVDC and grid software.", nodes: [] },
    nuclear: { id: "nuclear", name: "Nuclear Fuel & Reactors", kicker: "Conversion to components", available: false,
      blurb: "Uranium conversion, enrichment, fuel fabrication, forgings, SMR components.", nodes: [] },
    semiconductors: { id: "semiconductors", name: "Semiconductors", kicker: "Wafers to advanced logic", available: false,
      blurb: "Polysilicon, wafers, electronic materials, tools, mature and leading-edge logic.", nodes: [] },
    full_stack: { id: "full_stack", name: "Full Electro-Industrial Stack", kicker: "All 39 sub-sectors", available: false,
      blurb: "The whole stack at once — the long-term destination for this mode.", nodes: [] },
  };

  const API = {
    COORD_CONFIG, COORD_SCENARIOS, COORD_SHOCKS, COORD_OBJECTIVES, COORD_STRESS_TESTS,
    BATTERY_NODES,
    getScenario: (id) => COORD_SCENARIOS[id] || COORD_SCENARIOS.batteries,
    getShock: (id) => COORD_SHOCKS.find(s => s.id === id) || null,
    getStressTest: (id) => COORD_STRESS_TESTS.find(s => s.id === id) || null,
    getObjective: (id) => COORD_OBJECTIVES.find(o => o.id === id) || null,
  };

  if (typeof module !== "undefined" && module.exports) module.exports = API;
  if (typeof window !== "undefined") { window.COORD_DATA = API; Object.assign(window, API); }
})(typeof globalThis !== "undefined" ? globalThis : this);
