/* ============================================================================
   ALLIANCE ARCHITECT — GAME + SCORING ENGINE
   ----------------------------------------------------------------------------
   Pure, deterministic, inspectable. NO DOM, NO randomness, NO LLM.
   Runs identically in the browser (window.COORD) and in Node (module.exports),
   which is how coord-tests.js can be executed in both places.

   The engine never invents its own numbers: every parameter comes from
   coord-scenarios.js (COORD_CONFIG / COORD_SCENARIOS / COORD_SHOCKS / ...).

   ── VOCABULARY ───────────────────────────────────────────────────────────────
   member      { uid, name, countryId }               a seat at the table
   country     { id, name, flag, home:{cat:0..10} }   from data.js ALLIES
   plan        { alloc:{nodeId:tokens}, submitted }   one country's private plan
   plans       { uid: plan }
   node        a supply-chain node (resolved = after alliance-size + shock scaling)
   nodeStat    computed capacity/status for one node
   alliance    { headline, components{}, nodes[], summary{} }
   agreement   { id, title, proposer, participants[], commitments[], status, ... }
   commitment  { party, type, nodeId, amount, role, note }

   ── THE SIX ALLIANCE COMPONENTS ──────────────────────────────────────────────
   completeness   are the chain's nodes present AND fed by their dependencies?
   scale          is capacity near economically useful scale (not sub-scale)?
   resilience     are strategic nodes supplied by enough distinct allies?
   duplication    100 = no wasted allied capital (overbuild / sub-scale slivers)
   strategic      are the strategically important nodes actually covered?
   complementarity do national plans interlock across borders, or replicate?
   ============================================================================ */
(function (root, factory) {
  const DATA = (typeof require === "function" && typeof module !== "undefined")
    ? require("./coord-scenarios.js")
    : (root.COORD_DATA || {});
  const API = factory(DATA);
  if (typeof module !== "undefined" && module.exports) module.exports = API;
  if (typeof window !== "undefined") window.COORD = API;
})(typeof globalThis !== "undefined" ? globalThis : this, function (DATA) {
  "use strict";

  const CFG = DATA.COORD_CONFIG;
  const clamp = (v, lo, hi) => Math.max(lo, Math.min(hi, v));
  const sum = (a) => a.reduce((x, y) => x + y, 0);
  const round1 = (v) => Math.round(v * 10) / 10;

  /* ==========================================================================
     1. DETERMINISTIC PSEUDO-RANDOMNESS
     Every client must derive the same objectives / shock order from the same
     league state, without an extra sync round-trip. So: hash strings, never
     Math.random().
     ========================================================================== */
  function hashString(s) {
    let h = 2166136261;
    for (let i = 0; i < String(s).length; i++) {
      h ^= String(s).charCodeAt(i);
      h = Math.imul(h, 16777619);
    }
    return (h >>> 0);
  }

  /* ==========================================================================
     2. SCENARIO RESOLUTION — alliance-size scaling + cumulative shock effects
     ========================================================================== */

  /**
   * evaluateShock — turn a list of shock ids into a single multiplier bundle.
   * Shocks are CUMULATIVE across rounds (multiplied together), so a graphite
   * embargo followed by a recession leaves both fingerprints on the model.
   * @returns {{demand:{},strategic:{},resilience:{},cpt:{},budgetDelta:number,applied:[]}}
   */
  function evaluateShock(shockIds) {
    const out = { demand: {}, strategic: {}, resilience: {}, cpt: {}, budgetDelta: 0, applied: [] };
    (shockIds || []).forEach(id => {
      const shock = DATA.getShock(id);
      if (!shock) return;
      out.applied.push(shock);
      const e = shock.effects || {};
      const mul = (bucket, src) => Object.keys(src || {}).forEach(k => { bucket[k] = (bucket[k] == null ? 1 : bucket[k]) * src[k]; });
      mul(out.demand, e.demandMultiplier);
      mul(out.strategic, e.strategicMultiplier);
      mul(out.resilience, e.resilienceMultiplier);
      mul(out.cpt, e.capacityPerTokenMultiplier);
      out.budgetDelta += (e.budgetDelta || 0);
    });
    return out;
  }

  /**
   * resolveNodes — the scenario's nodes with every parameter scaled for the
   * current alliance size and the shocks in force. All capacity thresholds are
   * scaled by the SAME factor as demand, so ratios (min-viable as a share of
   * requirement) are invariant to alliance size.
   */
  function resolveNodes(scenarioId, nPlayers, shockIds) {
    const scenario = DATA.getScenario(scenarioId);
    const shock = evaluateShock(shockIds);
    const sizeScale = CFG.demandScale(nPlayers);
    return scenario.nodes.map(n => {
      const dm = shock.demand[n.id] == null ? 1 : shock.demand[n.id];
      const f = sizeScale * dm;
      return Object.assign({}, n, {
        requirement: n.demand * f,
        minViable: n.minViableCapacity * f,
        ideal: n.idealCapacity * f,
        maxUseful: n.maximumUsefulCapacity * f,
        capacityPerToken: n.capacityPerToken * (shock.cpt[n.id] == null ? 1 : shock.cpt[n.id]),
        strategicImportance: clamp(n.strategicImportance * (shock.strategic[n.id] == null ? 1 : shock.strategic[n.id]), 0, 1),
        resilienceImportance: clamp(n.resilienceImportance * (shock.resilience[n.id] == null ? 1 : shock.resilience[n.id]), 0, 1),
        sizeScale: f,
      });
    });
  }

  /** Policy-token budget per player for a round, after shock budget deltas. */
  function budgetFor(shockIds, base) {
    const shock = evaluateShock(shockIds);
    return Math.max(5, (base == null ? CFG.tokensPerRound : base) + shock.budgetDelta);
  }

  /* ==========================================================================
     3. COMPARATIVE ADVANTAGE
     ========================================================================== */

  /**
   * countryEfficiency — how much capacity a country gets per policy token in a
   * node, relative to a generic member. Built from the SAME public data the
   * fantasy draft uses (ALLIES[].home category synergies) plus a node-level
   * affinity table, so a player's comparative advantage is legible, not hidden.
   *
   *   eff = clamp(effBase + effHomeWeight·home[node.cat] + affinity[country], effMin, effMax)
   *
   * @param {object} country   { id, home:{cat:0..10} }
   * @param {object} node      resolved node
   * @param {object} [bonuses] { finance:bool, rnd:bool } from honoured agreements
   */
  function countryEfficiency(country, node, bonuses) {
    if (!country) return CFG.effBase;
    const home = (country.home && country.home[node.cat]) || 0;
    const aff = (node.affinity && node.affinity[country.id]) || 0;
    let eff = CFG.effBase + CFG.effHomeWeight * home + aff;
    if (bonuses && bonuses.finance) eff += CFG.dealEffects.financeEfficiencyBonus;
    if (bonuses && bonuses.rnd) eff += CFG.dealEffects.rndEfficiencyBonus;
    return clamp(eff, CFG.effMin, CFG.effMax);
  }

  /* ==========================================================================
     4. AGREEMENT EFFECTS
     Deals do two things: (a) they change the physics slightly (finance and
     joint R&D make a partner's tokens go further, offtake improves utilisation),
     and (b) they create obligations that can be honoured or broken.
     ========================================================================== */

  const BREACHABLE = ["invest", "reduce", "maintain", "lead"];

  const COMMITMENT_TYPES = [
    { type: "invest",   label: "Invest at least N tokens",       needsNode: true,  needsAmount: true,  role: "producer",   verb: "invests ≥{amount} tokens in" },
    { type: "reduce",   label: "Invest no more than N tokens",   needsNode: true,  needsAmount: true,  role: "restraint",  verb: "caps investment at {amount} tokens in" },
    { type: "maintain", label: "Maintain N% of allied capacity", needsNode: true,  needsAmount: true,  role: "secondary",  verb: "maintains ≥{amount}% of allied requirement in" },
    { type: "lead",     label: "Act as lead supplier",           needsNode: true,  needsAmount: false, role: "lead",       verb: "acts as lead supplier for" },
    { type: "offtake",  label: "Provide procurement / offtake",  needsNode: true,  needsAmount: false, role: "offtaker",   verb: "guarantees offtake for" },
    { type: "finance",  label: "Provide finance",                needsNode: true,  needsAmount: false, role: "financier",  verb: "finances partners' capacity in" },
    { type: "rnd",      label: "Joint R&D partnership",          needsNode: true,  needsAmount: false, role: "rnd",        verb: "joins R&D partnership on" },
    { type: "access",   label: "Guarantee input access",         needsNode: true,  needsAmount: false, role: "supplier",   verb: "guarantees partners access to" },
    { type: "pact",     label: "Join sectoral pact",             needsNode: true,  needsAmount: false, role: "member",     verb: "joins the sectoral pact on" },
  ];
  const commitmentType = (t) => COMMITMENT_TYPES.find(c => c.type === t) || COMMITMENT_TYPES[0];

  /**
   * agreementEffects — collapse a set of agreements into per-country lookups.
   * @param {array} agreements  only status==="accepted" entries are considered
   * @param {array} [honoredIds] if given, only these agreements' effects apply
   *                             (this is how defection voids a deal's benefits)
   */
  function agreementEffects(agreements, honoredIds) {
    const eff = {
      bonuses: {},     // uid -> nodeId -> {finance, rnd}
      offtake: {},     // uid -> nodeId -> true   (this uid RECEIVES guaranteed demand)
      access: {},      // uid -> nodeId -> true   (this uid has secured supply)
      given: {},       // uid -> {finance:n, offtake:n}   (what it costs the giver)
      received: {},    // uid -> count of benefits received
      pacts: {},       // nodeId -> Set of uids
      memberOf: {},    // uid -> count of agreements
    };
    const list = (agreements || []).filter(a => a && a.status === "accepted" &&
      (!honoredIds || honoredIds.indexOf(a.id) >= 0));
    list.forEach(a => {
      const parties = a.participants || [];
      parties.forEach(p => { eff.memberOf[p] = (eff.memberOf[p] || 0) + 1; });
      (a.commitments || []).forEach(c => {
        if (!c || !c.nodeId) return;
        const others = parties.filter(p => p !== c.party);
        const mark = (uid, bucket, key) => {
          eff[bucket][uid] = eff[bucket][uid] || {};
          eff[bucket][uid][key] = true;
        };
        if (c.type === "finance") {
          // the financier pays; every other participant investing in that node gets cheaper capital
          others.forEach(o => { mark(o, "bonuses", c.nodeId); eff.bonuses[o][c.nodeId] = Object.assign({}, eff.bonuses[o][c.nodeId], { finance: true }); eff.received[o] = (eff.received[o] || 0) + 1; });
          eff.given[c.party] = eff.given[c.party] || { finance: 0, offtake: 0 };
          eff.given[c.party].finance++;
        } else if (c.type === "rnd") {
          parties.forEach(o => { eff.bonuses[o] = eff.bonuses[o] || {}; eff.bonuses[o][c.nodeId] = Object.assign({}, eff.bonuses[o][c.nodeId], { rnd: true }); });
          eff.received[c.party] = (eff.received[c.party] || 0) + 1;
        } else if (c.type === "offtake") {
          // the buyer guarantees demand: producers in that node get utilisation certainty
          others.forEach(o => { mark(o, "offtake", c.nodeId); eff.received[o] = (eff.received[o] || 0) + 1; });
          eff.given[c.party] = eff.given[c.party] || { finance: 0, offtake: 0 };
          eff.given[c.party].offtake++;
        } else if (c.type === "access") {
          others.forEach(o => { mark(o, "access", c.nodeId); eff.received[o] = (eff.received[o] || 0) + 1; });
        } else if (c.type === "pact") {
          eff.pacts[c.nodeId] = eff.pacts[c.nodeId] || [];
          parties.forEach(p => { if (eff.pacts[c.nodeId].indexOf(p) < 0) eff.pacts[c.nodeId].push(p); });
        }
      });
    });
    eff.agreements = list;
    return eff;
  }

  /* ==========================================================================
     5. CAPACITY
     ========================================================================== */

  /**
   * calculateSectorCapacity — capacity built in ONE node by the whole alliance.
   *
   *   capacity = installedBase(node) + Σ_countries tokens · capacityPerToken · efficiency
   *
   * @returns {object} nodeStat
   */
  function calculateSectorCapacity(node, ctx) {
    const { members, plans, countries, effects, installed } = ctx;
    const byUid = {}, byCountry = {}, tokensByUid = {};
    let capacity = (installed && installed[node.id]) || 0;
    let tokens = 0, investors = 0;
    members.forEach(m => {
      const plan = (plans && plans[m.uid]) || {};
      const t = Math.max(0, Number((plan.alloc || {})[node.id] || 0));
      tokensByUid[m.uid] = t;
      if (t <= 0) { byUid[m.uid] = 0; return; }
      investors++;
      tokens += t;
      const bonuses = effects && effects.bonuses && effects.bonuses[m.uid] ? effects.bonuses[m.uid][node.id] : null;
      const eff = countryEfficiency(countries[m.countryId], node, bonuses);
      const cap = t * node.capacityPerToken * eff;
      byUid[m.uid] = cap;
      byCountry[m.countryId] = (byCountry[m.countryId] || 0) + cap;
      capacity += cap;
    });
    const meaningful = CFG.meaningfulShare * node.requirement;
    const suppliers = Object.keys(byUid).filter(u => byUid[u] >= meaningful);
    const supplierCountries = members.filter(m => (byUid[m.uid] || 0) >= meaningful).map(m => m.countryId);
    const lead = Object.keys(byUid).sort((a, b) => byUid[b] - byUid[a])[0];
    const leadCap = lead ? byUid[lead] : 0;
    return {
      nodeId: node.id, node,
      capacity, requirement: node.requirement,
      pct: node.requirement > 0 ? capacity / node.requirement : 0,
      tokens, investors, byUid, byCountry, tokensByUid,
      suppliers, supplierCountries,
      lead: leadCap > 0 ? lead : null,
      leadShare: capacity > 0 ? leadCap / capacity : 0,
      concentration: capacity > 0 ? leadCap / capacity : 0,
      atScale: capacity >= node.minViable,
      // sub-scale slivers: money spent on plants too small to ever produce.
      // Same threshold as "counts as a supplier" and "survives stress".
      slivers: members.filter(m => (byUid[m.uid] || 0) > 0 && byUid[m.uid] < meaningful).length,
    };
  }

  /** All nodes at once. Returns { nodes:[nodeStat], byId:{} } */
  function calculateAllCapacity(nodes, ctx) {
    const stats = nodes.map(n => calculateSectorCapacity(n, ctx));
    const byId = {};
    stats.forEach(s => byId[s.nodeId] = s);
    // dependency satisfaction needs the full picture, so it's a second pass
    stats.forEach(s => {
      const deps = s.node.dependencies || [];
      const okDeps = deps.filter(d => byId[d] && byId[d].capacity >= byId[d].node.minViable);
      s.depsSatisfied = okDeps.length;
      s.depsTotal = deps.length;
      s.depFactor = deps.length ? okDeps.length / deps.length : 1;
      s.brokenDeps = deps.filter(d => !(byId[d] && byId[d].capacity >= byId[d].node.minViable));
    });
    return { nodes: stats, byId };
  }

  /* ==========================================================================
     6. ALLIANCE SCORE COMPONENTS  (each 0..100, higher = better)
     ========================================================================== */

  /** Weight used whenever "how much does this node matter" is needed. */
  const nodeWeight = (n) => 0.5 + n.strategicImportance;

  /**
   * calculateScaleScore — piecewise, continuous:
   *   0 at zero capacity → 25 at minimum viable scale → 100 at ideal
   *   → flat 100 up to maximum useful → decays toward 30 beyond that.
   * Sub-scale capacity scores badly on purpose: three half-plants are worse
   * than one real one.
   */
  function calculateScaleScore(cap, node) {
    if (cap <= 0) return 0;
    if (cap < node.minViable) return 25 * (cap / node.minViable);
    if (cap < node.ideal) return 25 + 75 * (cap - node.minViable) / Math.max(1e-9, node.ideal - node.minViable);
    if (cap <= node.maxUseful) return 100;
    const over = (cap - node.maxUseful) / Math.max(1e-9, node.maxUseful);
    return Math.max(30, 100 - 70 * over);
  }

  /**
   * calculateSupplyChainCompleteness — is each node present, and is it fed?
   * A node with capacity but broken upstream gets partial credit (you can
   * import the input — it works, but it is exactly the fragility the alliance
   * is trying to escape).
   */
  function calculateSupplyChainCompleteness(stats) {
    let num = 0, den = 0;
    stats.forEach(s => {
      const w = nodeWeight(s.node);
      // "present" is a ramp, not a switch: a node at minimum viable scale is a
      // pilot plant, not a link in a chain. Full credit needs ~85% of requirement.
      const full = 0.85 * s.node.requirement;
      const own = clamp((s.capacity - s.node.minViable) / Math.max(1e-9, full - s.node.minViable), 0, 1);
      const credit = own * (0.45 + 0.55 * s.depFactor);
      num += w * credit; den += w;
    });
    return den ? 100 * num / den : 0;
  }

  /**
   * calculateResilienceBonus — are strategic nodes multi-sourced?
   * NOT all duplication is bad: reaching the node's strategicRedundancyTarget
   * scores full marks, and a node supplied 100% by one ally is penalised even
   * if its total capacity is fine.
   */
  function calculateResilienceBonus(stats) {
    let num = 0, den = 0;
    const detail = [];
    stats.forEach(s => {
      const n = s.node;
      const w = 0.4 + n.resilienceImportance;
      let score;
      if (s.capacity < n.minViable) {
        score = 0;                                   // nothing to be resilient about
      } else {
        const target = Math.max(1, n.strategicRedundancyTarget);
        const base = Math.min(1, s.supplierCountries.length / target);
        const concPenalty = Math.max(0, s.concentration - 0.70) / 0.30 * 35;
        score = clamp(100 * base - concPenalty, 0, 100);
      }
      detail.push({ nodeId: n.id, score, suppliers: s.supplierCountries.length, target: n.strategicRedundancyTarget });
      num += w * score; den += w;
    });
    return { score: den ? num / den : 0, detail };
  }

  /**
   * calculateDuplicationPenalty — 100 means no allied capital was wasted.
   * Three distinct wastes, each named so players can see what they did:
   *   overshoot  capacity beyond the node's maximum useful capacity
   *   slivers    plants too small to ever reach minimum viable scale
   *   crowding   more distinct investors than the redundancy target + 1
   *              (the subsidy-war term)
   */
  function calculateDuplicationPenalty(stats) {
    let total = 0, den = 0;
    const detail = [];
    stats.forEach(s => {
      const n = s.node;
      const overshoot = Math.max(0, s.capacity - n.maxUseful) / Math.max(1e-9, n.requirement);
      const crowding = Math.max(0, s.investors - (n.strategicRedundancyTarget + 1));
      const waste = clamp(90 * Math.min(1, overshoot / 0.60) + 12 * s.slivers + 10 * crowding, 0, 100);
      detail.push({ nodeId: n.id, waste, overshoot: round1(overshoot * 100), slivers: s.slivers, crowding });
      // Weight by the capital actually at stake: wasting 12 tokens on a fifth
      // gigafactory matters more than a rounding error in a cheap node.
      const w = Math.max(0.5, s.tokens);
      total += w * waste; den += w;
    });
    return { score: den ? 100 - total / den : 100, detail };
  }

  /** Strategic coverage: are the nodes that matter actually supplied? */
  function calculateStrategicCoverage(stats) {
    let num = 0, den = 0;
    stats.forEach(s => {
      const w = Math.pow(s.node.strategicImportance, 1.5);
      num += w * Math.min(1, s.pct); den += w;
    });
    return den ? 100 * num / den : 0;
  }

  /**
   * calculateComplementarity — do national plans interlock, or replicate?
   *   specialization  each country's token spend concentrated (Herfindahl)
   *   crossBorder     for every dependency edge, is the upstream lead a
   *                   different country from the downstream lead (a real
   *                   cross-border value chain) and are both nodes working?
   *   pacts           declared sectoral pacts add a small bonus
   */
  function calculateComplementarity(stats, ctx) {
    const { members, plans, effects } = ctx;
    const nNodes = stats.length || 1;
    // 1. specialization
    const specs = members.map(m => {
      const alloc = ((plans && plans[m.uid]) || {}).alloc || {};
      const t = sum(Object.keys(alloc).map(k => Math.max(0, Number(alloc[k]) || 0)));
      if (t <= 0) return 0;
      return sum(Object.keys(alloc).map(k => Math.pow(Math.max(0, Number(alloc[k]) || 0) / t, 2)));
    });
    const activeSpecs = specs.filter(s => s > 0);
    const meanSpec = activeSpecs.length ? sum(activeSpecs) / activeSpecs.length : 0;
    const floor = 1 / nNodes;
    const specScore = 100 * clamp((meanSpec - floor) / (0.60 - floor), 0, 1);
    // 2. cross-border chain edges
    const byId = {}; stats.forEach(s => byId[s.nodeId] = s);
    let credits = 0, edges = 0;
    stats.forEach(s => {
      (s.node.dependencies || []).forEach(d => {
        const up = byId[d]; if (!up) return;
        edges++;
        const bothWork = s.capacity >= s.node.minViable && up.capacity >= up.node.minViable;
        if (!bothWork) return;
        credits += (up.lead && s.lead && up.lead !== s.lead) ? 1 : 0.5;
      });
    });
    const chainScore = edges ? 100 * credits / edges : 50;
    // 3. pacts
    const pactCount = effects ? Object.keys(effects.pacts || {}).length : 0;
    const pactBonus = Math.min(12, pactCount * CFG.dealEffects.pactComplementarityBonus);
    return {
      score: clamp(0.45 * specScore + 0.55 * chainScore + pactBonus, 0, 100),
      specialization: specScore, crossBorder: chainScore, pactBonus,
    };
  }

  /* ==========================================================================
     7. scoreAlliance
     ========================================================================== */

  /**
   * scoreAlliance — the collective score, 0..100, with every component exposed.
   * @param {object} ctx { nodes, members, plans, countries, agreements, installed }
   */
  function scoreAlliance(ctx) {
    const effects = ctx.effects || agreementEffects(ctx.agreements, ctx.honoredIds);
    const c = Object.assign({}, ctx, { effects });
    const cap = calculateAllCapacity(ctx.nodes, c);
    const stats = cap.nodes;

    const completeness = calculateSupplyChainCompleteness(stats);
    // scale, with an offtake utilisation credit for producers who have a buyer
    let sNum = 0, sDen = 0;
    stats.forEach(s => {
      const w = nodeWeight(s.node);
      let sc = calculateScaleScore(s.capacity, s.node);
      const hasOfftake = ctx.members.some(m => effects.offtake[m.uid] && effects.offtake[m.uid][s.nodeId] && (s.byUid[m.uid] || 0) > 0);
      if (hasOfftake) sc = Math.min(100, sc * (1 + CFG.dealEffects.offtakeScaleBonus));
      s.scaleScore = sc;
      sNum += w * sc; sDen += w;
    });
    const scale = sDen ? sNum / sDen : 0;
    const res = calculateResilienceBonus(stats);
    const dup = calculateDuplicationPenalty(stats);
    const strategic = calculateStrategicCoverage(stats);
    const comp = calculateComplementarity(stats, c);

    const components = {
      completeness, scale, resilience: res.score, duplication: dup.score,
      strategic, complementarity: comp.score,
    };
    const W = CFG.allianceWeights;
    const headline = clamp(
      components.completeness * W.completeness + components.scale * W.scale +
      components.resilience * W.resilience + components.duplication * W.duplication +
      components.strategic * W.strategic + components.complementarity * W.complementarity, 0, 100);

    // headline summary counters used by the before/after screens
    const summary = {
      shortfalls: stats.filter(s => s.pct < 0.85).length,
      criticalShortfalls: stats.filter(s => s.pct < 0.50 && s.node.strategicImportance >= 0.55).length,
      duplications: stats.filter(s => s.capacity > s.node.maxUseful).length,
      singleSource: stats.filter(s => s.capacity >= s.node.minViable && s.supplierCountries.length <= 1 && s.node.resilienceImportance >= 0.6).length,
      chainBreaks: stats.filter(s => s.capacity >= s.node.minViable && s.brokenDeps.length > 0).length,
      chainComplete: stats.every(s => s.capacity >= s.node.minViable),
      nodesAtScale: stats.filter(s => s.capacity >= s.node.minViable).length,
      nodeCount: stats.length,
      unusedTokens: 0,
    };
    return {
      headline: Math.round(headline), headlineRaw: headline,
      components, weights: W, nodes: stats, byId: cap.byId, summary,
      resilienceDetail: res.detail, duplicationDetail: dup.detail, complementarityDetail: comp,
      effects,
    };
  }

  /* ==========================================================================
     8. COORDINATOR REPORT — aggregated intelligence, never private plans.
     ========================================================================== */

  const STATUS = {
    MISSING:      { key: "MISSING",      label: "NOT PLANNED",        sev: 3, tone: "crit" },
    SUBSCALE:     { key: "SUBSCALE",     label: "SUB-SCALE",          sev: 3, tone: "crit" },
    CRITICAL:     { key: "CRITICAL",     label: "CRITICAL SHORTFALL", sev: 3, tone: "crit" },
    UNDER:        { key: "UNDER",        label: "UNDER-SUPPLIED",     sev: 2, tone: "warn" },
    ADEQUATE:     { key: "ADEQUATE",     label: "ADEQUATE",           sev: 0, tone: "ok" },
    RESILIENT:    { key: "RESILIENT",    label: "ADEQUATE + RESILIENT", sev: 0, tone: "good" },
    EXCESS:       { key: "EXCESS",       label: "EXCESS DUPLICATION", sev: 2, tone: "warn" },
  };

  function nodeStatus(s) {
    const n = s.node;
    if (s.capacity <= 0) return STATUS.MISSING;
    if (s.capacity < n.minViable) return STATUS.SUBSCALE;
    if (s.capacity > n.maxUseful) return STATUS.EXCESS;
    if (s.pct < 0.50) return STATUS.CRITICAL;
    if (s.pct < 0.85) return STATUS.UNDER;
    if (s.supplierCountries.length >= n.strategicRedundancyTarget && s.concentration <= 0.70) return STATUS.RESILIENT;
    return STATUS.ADEQUATE;
  }

  /**
   * generateCoordinatorReport — the alliance secretariat's view.
   * IT DOES NOT REVEAL WHO PLANNED WHAT. Every line is either an aggregate
   * (capacity %, number of countries investing) or a statement about publicly
   * known comparative advantage. It states problems, never solutions.
   */
  function generateCoordinatorReport(ctx) {
    const alliance = ctx.alliance || scoreAlliance(ctx);
    const stats = alliance.nodes;
    const nodeLines = stats.map(s => {
      const st = nodeStatus(s);
      const flags = [];
      if (s.capacity >= s.node.minViable && s.supplierCountries.length <= 1 && s.node.resilienceImportance >= 0.6)
        flags.push("SINGLE-SOURCE");
      if (s.brokenDeps.length && s.capacity >= s.node.minViable) flags.push("INPUTS MISSING");
      if (s.investors > s.node.strategicRedundancyTarget + 1 && s.pct > 1.10) flags.push("SUBSIDY WAR");
      if (s.slivers > 0) flags.push("SUB-SCALE PLANTS");
      return {
        nodeId: s.nodeId, name: s.node.name, short: s.node.short, glyph: s.node.glyph,
        cat: s.node.cat, stage: s.node.stage,
        pct: Math.round(s.pct * 100), capacity: round1(s.capacity), requirement: round1(s.requirement),
        investors: s.investors, suppliers: s.supplierCountries.length,
        redundancyTarget: s.node.strategicRedundancyTarget,
        status: st.key, statusLabel: st.label, tone: st.tone, sev: st.sev, flags,
        strategicImportance: s.node.strategicImportance,
        chokepoint: !!s.node.chokepoint, chinaExposure: s.node.chinaExposure,
      };
    });

    /* --- system-level findings, ordered by severity × strategic importance --- */
    const findings = [];
    const push = (sev, kind, title, detail, nodeId) => findings.push({ sev, kind, title, detail, nodeId });

    stats.forEach(s => {
      const n = s.node, pctTxt = Math.round(s.pct * 100) + "%";
      if (s.capacity <= 0 && n.strategicImportance >= 0.5) {
        push(3, "shortage", `${n.short}: nobody is building it`, `A strategically important node with zero planned allied capacity. Everything downstream of it stays dependent on non-allied supply.`, n.id);
      } else if (s.pct < 0.50 && n.strategicImportance >= 0.5) {
        push(3, "shortage", `${n.short}: critical shortfall at ${pctTxt}`, `Planned allied capacity covers ${pctTxt} of requirement${n.chokepoint ? ". This node is the alliance's sharpest external chokepoint." : "."}`, n.id);
      } else if (s.capacity > 0 && s.capacity < n.minViable) {
        push(3, "subscale", `${n.short}: sub-scale investment`, `${s.investors} member(s) are investing, but the combined capacity is below minimum viable scale — the money produces nothing usable.`, n.id);
      } else if (s.pct < 0.85) {
        push(2, "shortage", `${n.short}: under-supplied at ${pctTxt}`, `Short of requirement. Downstream nodes will run below capacity or import the gap.`, n.id);
      }
      if (s.capacity > n.maxUseful) {
        // one member overbuilding is a different problem from several members
        // racing each other — name them differently.
        if (s.investors <= 1) {
          push(1, "overbuild", `${n.short}: overbuilt at ${pctTxt}`, `A single member has built past the point of usefulness here. Those tokens buy more capacity in a node the alliance is short of.`, n.id);
        } else {
          push(2, "duplication", `${n.short}: excess duplication at ${pctTxt}`, `${s.investors} members are building the same node past the point of usefulness. Allied capital is competing with itself${s.investors >= 3 ? " — this is what a subsidy war looks like" : ""}.`, n.id);
        }
      }
      if (s.capacity >= n.minViable && s.supplierCountries.length <= 1 && n.resilienceImportance >= 0.6) {
        push(2, "single_source", `${n.short}: single allied supplier`, `Capacity is adequate but concentrated in one member. The alliance wants ${n.strategicRedundancyTarget} suppliers here — one disruption takes the whole chain down.`, n.id);
      }
      if (s.capacity >= n.minViable && s.brokenDeps.length) {
        const names = s.brokenDeps.map(d => (alliance.byId[d] ? alliance.byId[d].node.short : d)).join(", ");
        push(3, "chain_break", `${n.short}: inputs missing`, `This node is being built but its upstream input(s) — ${names} — are not viable inside the alliance. The chain is incomplete.`, n.id);
      }
    });

    /* Complementarity hints: which members COULD do the missing nodes well.
       Uses only publicly-visible comparative advantage, plus the count of
       members already investing. It flags an opportunity; it does not name a
       plan or prescribe an answer. */
    const hints = [];
    stats.filter(s => s.pct < 0.85).forEach(s => {
      const capable = ctx.members
        .map(m => ({ m, eff: countryEfficiency(ctx.countries[m.countryId], s.node) }))
        .filter(x => x.eff >= 1.15)
        .sort((a, b) => b.eff - a.eff);
      if (capable.length) {
        hints.push({
          nodeId: s.nodeId, node: s.node.short,
          countries: capable.slice(0, 3).map(x => x.m.countryId),
          detail: `${capable.length} member(s) have strong comparative advantage in ${s.node.short} — allied capacity there is at ${Math.round(s.pct * 100)}%.`,
        });
      }
    });
    stats.filter(s => s.capacity > s.node.maxUseful).forEach(s => {
      const weak = ctx.members
        .filter(m => (s.byUid[m.uid] || 0) > 0)
        .map(m => countryEfficiency(ctx.countries[m.countryId], s.node))
        .filter(e => e < 1.10).length;
      if (weak > 0) hints.push({
        nodeId: s.nodeId, node: s.node.short, countries: [],
        detail: `${weak} of the members building ${s.node.short} have below-average comparative advantage there. Those tokens buy more capacity somewhere else.`,
      });
    });

    findings.sort((a, b) => b.sev - a.sev ||
      ((alliance.byId[b.nodeId] ? alliance.byId[b.nodeId].node.strategicImportance : 0) -
       (alliance.byId[a.nodeId] ? alliance.byId[a.nodeId].node.strategicImportance : 0)));

    return {
      nodes: nodeLines, findings, hints,
      alliance: { headline: alliance.headline, components: alliance.components, summary: alliance.summary },
      totals: {
        tokensPlanned: sum(stats.map(s => s.tokens)),
        membersReporting: ctx.members.length,
      },
    };
  }

  /* ==========================================================================
     9. PRIVATE NATIONAL OBJECTIVES
     ========================================================================== */

  /**
   * assignObjectives — deterministic from (seed, uid), so every client derives
   * the same objectives with no extra synchronisation.
   */
  function assignObjectives(seed, uid, countryId, n) {
    const pool = DATA.COORD_OBJECTIVES.filter(o => o.fits(countryId));
    const want = n || CFG.objectivesPerPlayer;
    const picked = [];
    let h = hashString(seed + "|" + uid);
    for (let i = 0; i < 40 && picked.length < want; i++) {
      const idx = h % pool.length;
      const id = pool[idx].id;
      if (picked.indexOf(id) < 0) picked.push(id);
      h = hashString(h + ":" + i);
    }
    return picked;
  }

  /** Which node is this country's natural champion (highest efficiency)? */
  function bestNodeFor(country, nodes) {
    return nodes.slice().sort((a, b) => countryEfficiency(country, b) - countryEfficiency(country, a))[0];
  }

  /**
   * checkObjectives — evaluate one player's private objectives against the
   * realised round. Returns [{id,label,detail,met,progress}]
   */
  function checkObjectives(objIds, ctx) {
    const { uid, country, alliance, nodes } = ctx;
    const byId = alliance.byId;
    const stat = (id) => byId[id];
    const myCap = (id) => (stat(id) ? (stat(id).byUid[uid] || 0) : 0);
    const meaningful = (id) => stat(id) ? myCap(id) >= CFG.meaningfulShare * stat(id).node.requirement : false;
    const manufacturing = ["cathode_cam", "cells", "packs", "bms"];

    return (objIds || []).map(id => {
      const o = DATA.getObjective(id);
      if (!o) return null;
      let met = false, progress = "";
      switch (id) {
        case "keep_manufacturing": {
          const hits = nodes.filter(n => manufacturing.indexOf(n.id) >= 0 && myCap(n.id) >= n.minViable * 0.6);
          met = hits.length > 0;
          progress = met ? hits.map(n => n.short).join(", ") : "no at-scale manufacturing node";
          break;
        }
        case "sector_leader": {
          const leads = nodes.filter(n => stat(n.id) && stat(n.id).lead === uid && stat(n.id).capacity >= n.minViable);
          met = leads.length > 0;
          progress = met ? `lead in ${leads.map(n => n.short).join(", ")}` : "not the lead supplier anywhere";
          break;
        }
        case "two_nodes": {
          const c = nodes.filter(n => meaningful(n.id)).length;
          met = c >= 2; progress = `${c} node(s) with meaningful capacity`;
          break;
        }
        case "jobs": {
          const score = sum(nodes.map(n => myCap(n.id) * n.employmentIntensity));
          met = score >= 55; progress = `jobs-weighted score ${Math.round(score)} / 55`;
          break;
        }
        case "no_single_source": {
          const bad = nodes.filter(n => n.strategicImportance >= 0.6 && stat(n.id) && stat(n.id).capacity > 0 && stat(n.id).concentration > 0.65);
          met = bad.length === 0;
          progress = met ? "no strategic node over-concentrated" : `${bad.map(n => n.short).join(", ")} over 65% single-country`;
          break;
        }
        case "mini_chain": {
          const pair = nodes.find(n => meaningful(n.id) && (n.dependencies || []).some(d => meaningful(d)));
          met = !!pair;
          progress = met ? `${pair.short} + upstream` : "no two connected domestic links";
          break;
        }
        case "retain_sensitive": {
          const best = bestNodeFor(country, nodes);
          met = best ? meaningful(best.id) : false;
          progress = best ? `${best.short}: ${met ? "retained" : "abandoned"}` : "";
          break;
        }
        case "scale_champion": {
          const best = nodes.map(n => ({ n, share: stat(n.id) ? myCap(n.id) / stat(n.id).node.requirement : 0 }))
            .sort((a, b) => b.share - a.share)[0];
          met = best && best.share >= 0.40;
          progress = best ? `best node: ${Math.round(best.share * 100)}% of allied requirement / 40%` : "";
          break;
        }
        default: met = false;
      }
      return { id, label: o.label, detail: o.detail, why: o.why, met, progress };
    }).filter(Boolean);
  }

  /* ==========================================================================
     10. scoreCountry
     ========================================================================== */

  /**
   * scoreCountry — one member's national score, 0..100, components exposed.
   * A country can raise the alliance score while lowering its own: taking the
   * unglamorous node hurts `domestic`, and honouring a commitment in a node you
   * are bad at shows up as `commitmentCost`. Compensation (finance, offtake,
   * R&D, pacts) arrives through `allianceBenefit`. That is the negotiation.
   */
  function scoreCountry(uid, ctx) {
    const { members, plans, countries, alliance, nodes, objectives, trust, compliance } = ctx;
    const me = members.find(m => m.uid === uid) || { uid, countryId: "US" };
    const country = countries[me.countryId];
    const effects = alliance.effects || agreementEffects(ctx.agreements, ctx.honoredIds);
    const alloc = ((plans && plans[uid]) || {}).alloc || {};
    const caps = CFG.nationalCaps;
    const budget = ctx.budget || CFG.tokensPerRound;

    const myTokens = sum(nodes.map(n => Math.max(0, Number(alloc[n.id]) || 0)));

    /* --- fit: did you invest where you are actually good? --- */
    let effWeighted = 0;
    nodes.forEach(n => {
      const t = Math.max(0, Number(alloc[n.id]) || 0);
      if (!t) return;
      const b = effects.bonuses[uid] ? effects.bonuses[uid][n.id] : null;
      effWeighted += t * countryEfficiency(country, n, b);
    });
    const avgEff = myTokens ? effWeighted / myTokens : CFG.effBase;
    const fit = caps.fit * clamp((avgEff - CFG.effMin) / (CFG.effMax - CFG.effMin), 0, 1);

    /* --- domestic industrial value captured at home --- */
    let value = 0, maxRate = 0;
    nodes.forEach(n => {
      const rate = n.valueAdd * countryEfficiency(country, n);
      if (rate > maxRate) maxRate = rate;
    });
    nodes.forEach(n => {
      const t = Math.max(0, Number(alloc[n.id]) || 0);
      if (!t) return;
      const s = alliance.byId[n.id];
      const b = effects.bonuses[uid] ? effects.bonuses[uid][n.id] : null;
      const eff = countryEfficiency(country, n, b);
      // A plant that never reaches minimum scale is a national fiasco (0.5).
      // But a *duplicate* plant is only mildly bad nationally — it still employs
      // people, still gets subsidised, still gets a ribbon cut. Private returns
      // to duplication exceed the alliance's returns: that gap IS the game.
      let factor = 1;
      if (s) {
        if (s.capacity < n.minViable) factor = 0.50;
        else if (s.capacity > n.maxUseful) factor = Math.max(0.85, n.maxUseful / s.capacity);
      }
      if (effects.offtake[uid] && effects.offtake[uid][n.id]) factor = Math.min(1.15, factor * (1 + CFG.dealEffects.offtakeScaleBonus));
      value += t * n.valueAdd * eff * factor;
    });
    const domestic = caps.domestic * clamp(value / Math.max(1e-9, budget * maxRate), 0, 1);

    /* --- private political objectives --- */
    const objs = checkObjectives(objectives, { uid, country, alliance, nodes });
    const objectiveScore = Math.min(caps.objectives, objs.filter(o => o.met).length * CFG.objectivePoints);

    /* --- strategic autonomy: is your chain need actually secured? --- */
    let aNum = 0, aDen = 0;
    nodes.forEach(n => {
      const w = n.strategicImportance; if (w <= 0) return;
      const s = alliance.byId[n.id];
      let credit = 0;
      if (s) {
        const mine = (s.byUid[uid] || 0) >= CFG.meaningfulShare * n.requirement;
        const secured = !!(effects.access[uid] && effects.access[uid][n.id]) ||
                        !!(effects.pacts[n.id] && effects.pacts[n.id].indexOf(uid) >= 0);
        const allianceOk = s.capacity >= n.minViable && s.pct >= 0.85;
        if (mine) credit = 1;
        else if (secured && allianceOk) credit = 0.9;
        else if (allianceOk && s.concentration <= 0.70) credit = 0.65;
        else if (allianceOk) credit = 0.4;
      }
      aNum += w * credit; aDen += w;
    });
    const autonomy = caps.autonomy * (aDen ? aNum / aDen : 0);

    /* --- leadership positions --- */
    const leads = nodes.filter(n => alliance.byId[n.id] && alliance.byId[n.id].lead === uid && alliance.byId[n.id].capacity >= n.minViable);
    const leadership = Math.min(caps.leadership, leads.length * 4.5);

    /* --- alliance benefit: spillover from the collective result, scaled by
           your own reputation, plus concrete benefits received from deals --- */
    const rep = (trust && trust[uid] && trust[uid].rep != null) ? trust[uid].rep : CFG.trustStart;
    const trustFactor = 0.6 + 0.4 * (rep / 100);
    const spill = 7 * (alliance.headline / 100) * trustFactor;
    const received = Math.min(6, (effects.received[uid] || 0) * 3);
    const allianceBenefit = Math.min(caps.allianceBenefit, spill + received);

    /* --- costs: tokens spent under obligation in nodes you're bad at, the
           price of the finance/offtake you provided, and defection blowback --- */
    let obligationCost = 0;
    (effects.agreements || []).forEach(a => {
      if ((a.participants || []).indexOf(uid) < 0) return;
      (a.commitments || []).forEach(c => {
        if (c.party !== uid || c.type !== "invest" || !c.nodeId) return;
        const n = nodes.find(x => x.id === c.nodeId); if (!n) return;
        const eff = countryEfficiency(country, n);
        if (eff < 1.0) obligationCost += (Math.max(0, Number(alloc[c.nodeId]) || 0)) * (1.0 - eff) * 2.2;
      });
    });
    const given = effects.given[uid] || { finance: 0, offtake: 0 };
    const givingCost = given.finance * 2 + given.offtake * 1;
    const defected = compliance && compliance.defectorsByUid && compliance.defectorsByUid[uid] ? compliance.defectorsByUid[uid].length : 0;
    const defectionCost = defected ? 4 + 2 * defected : 0;
    // a defector keeps the tokens it promised elsewhere: a real short-term gain
    const defectionGain = defected ? 3 * defected : 0;
    const costs = -Math.min(18, obligationCost + givingCost + defectionCost) + defectionGain;

    const total = clamp(fit + domestic + objectiveScore + autonomy + leadership + allianceBenefit + costs, 0, 100);
    return {
      uid, countryId: me.countryId,
      total: Math.round(total), totalRaw: total,
      components: {
        fit: round1(fit), domestic: round1(domestic), objectives: objectiveScore,
        autonomy: round1(autonomy), leadership: round1(leadership),
        allianceBenefit: round1(allianceBenefit), costs: round1(costs),
      },
      objectives: objs, avgEfficiency: round1(avgEff), tokens: myTokens,
      leads: leads.map(n => n.id), reputation: rep,
    };
  }

  /** scoreCountry for every member. */
  function scoreAllCountries(ctx) {
    const out = {};
    ctx.members.forEach(m => {
      out[m.uid] = scoreCountry(m.uid, Object.assign({}, ctx, {
        objectives: (ctx.objectivesByUid || {})[m.uid] || [],
      }));
    });
    return out;
  }

  /* ==========================================================================
     11. COALITION STABILITY
     ========================================================================== */

  /**
   * coalitionStability — a bargain that raises the alliance score by wrecking
   * one member is not a stable bargain. 100 = nobody is worse off than their
   * own uncoordinated plan; it falls as the worst-off member's loss grows.
   */
  function coalitionStability(baselineNational, finalNational, members) {
    const rows = members.map(m => {
      const b = baselineNational[m.uid] ? baselineNational[m.uid].total : 0;
      const f = finalNational[m.uid] ? finalNational[m.uid].total : 0;
      return { uid: m.uid, countryId: m.countryId, before: b, after: f, delta: f - b, belowFloor: f < CFG.stabilityFloor };
    });
    const worst = rows.slice().sort((a, b) => a.delta - b.delta)[0] || { delta: 0 };
    const losers = rows.filter(r => r.delta < -CFG.stabilityTolerance);
    const floorBreaches = rows.filter(r => r.belowFloor);
    // score: full marks while the worst loss is inside tolerance, then linear to 0 at -25
    let score = 100;
    if (worst.delta < -CFG.stabilityTolerance) {
      score = clamp(100 * (1 - (Math.abs(worst.delta) - CFG.stabilityTolerance) / 25), 0, 100);
    }
    score -= floorBreaches.length * 15;
    score = clamp(score, 0, 100);
    return {
      score: Math.round(score),
      stable: score >= 70 && floorBreaches.length === 0,
      rows, worst, losers: losers.map(l => l.uid), floorBreaches: floorBreaches.map(l => l.uid),
      verdict: score >= 85 ? "STABLE" : score >= 70 ? "WORKABLE" : score >= 45 ? "FRAGILE" : "UNSTABLE",
    };
  }

  /* ==========================================================================
     12. COMMITMENTS & COMPLIANCE
     ========================================================================== */

  /** Human-readable text for a commitment. */
  function describeCommitment(c, nodesById, nameOf) {
    const t = commitmentType(c.type);
    const node = nodesById[c.nodeId];
    const verb = t.verb.replace("{amount}", c.amount == null ? "" : c.amount);
    return `${nameOf ? nameOf(c.party) : c.party} ${verb} ${node ? node.short : c.nodeId}`;
  }

  /**
   * evaluateCompliance — did each party do what it promised?
   * Only invest / reduce / maintain / lead are breachable; finance, offtake,
   * R&D, access and pacts are enacted by the engine and cannot be "missed"
   * (documented simplification for the MVP).
   */
  function evaluateCompliance(agreements, ctx) {
    const { alliance, nodes } = ctx;
    const byId = alliance.byId;
    const nodesById = {}; nodes.forEach(n => nodesById[n.id] = n);
    const results = [], honoredIds = [], breachedIds = [];
    const defectorsByUid = {};

    (agreements || []).filter(a => a && a.status === "accepted").forEach(a => {
      const checks = [];
      (a.commitments || []).forEach(c => {
        if (!c || BREACHABLE.indexOf(c.type) < 0) { checks.push({ c, breachable: false, met: true }); return; }
        const s = byId[c.nodeId];
        const alloc = ((ctx.plans || {})[c.party] || {}).alloc || {};
        const tokens = Math.max(0, Number(alloc[c.nodeId]) || 0);
        let met = true, actual = tokens;
        if (c.type === "invest") met = tokens >= Number(c.amount || 0);
        else if (c.type === "reduce") met = tokens <= Number(c.amount || 0);
        else if (c.type === "maintain") {
          const share = s ? (s.byUid[c.party] || 0) / Math.max(1e-9, s.node.requirement) * 100 : 0;
          actual = Math.round(share); met = share >= Number(c.amount || 0);
        } else if (c.type === "lead") { met = !!(s && s.lead === c.party); actual = s && s.lead === c.party ? "lead" : "not lead"; }
        checks.push({ c, breachable: true, met, actual });
        if (!met) {
          defectorsByUid[c.party] = defectorsByUid[c.party] || [];
          defectorsByUid[c.party].push({ agreementId: a.id, commitment: c, actual });
        }
      });
      const breaches = checks.filter(x => x.breachable && !x.met);
      const honored = breaches.length === 0;
      if (honored) honoredIds.push(a.id); else breachedIds.push(a.id);
      results.push({ agreementId: a.id, title: a.title, parties: (a.participants || []).slice(), honored, checks, breaches });
    });
    return { results, honoredIds, breachedIds, defectorsByUid };
  }

  /**
   * applyTrust — update reputations after a round.
   * Honouring is worth less than defecting costs, so cooperation compounds
   * slowly and betrayal is felt immediately. Trust carries between rounds.
   */
  function applyTrust(trust, members, compliance) {
    const next = {};
    members.forEach(m => {
      const prev = (trust && trust[m.uid]) || { rep: CFG.trustStart, honored: 0, defected: 0 };
      next[m.uid] = { rep: prev.rep, honored: prev.honored || 0, defected: prev.defected || 0 };
    });
    (compliance.results || []).forEach(r => {
      if (!r.honored) return;
      (r.parties || []).forEach(p => { if (next[p]) { next[p].rep += CFG.trustHonorGain; next[p].honored++; } });
    });
    Object.keys(compliance.defectorsByUid || {}).forEach(uid => {
      if (!next[uid]) return;
      next[uid].rep -= CFG.trustDefectLoss;
      next[uid].defected += compliance.defectorsByUid[uid].length;
    });
    Object.keys(next).forEach(u => next[u].rep = clamp(Math.round(next[u].rep), CFG.trustMin, CFG.trustMax));
    return next;
  }

  /**
   * commitmentsFor — the obligations one player carries into the commitment
   * phase, with their current compliance state, so the UI can show
   * "honours 2 of 3".
   */
  function commitmentsFor(uid, agreements, ctx) {
    const out = [];
    (agreements || []).filter(a => a && a.status === "accepted").forEach(a => {
      (a.commitments || []).forEach(c => {
        if (c.party !== uid) return;
        const alloc = ((ctx.plans || {})[uid] || {}).alloc || {};
        const tokens = Math.max(0, Number(alloc[c.nodeId]) || 0);
        const s = ctx.alliance ? ctx.alliance.byId[c.nodeId] : null;
        let met = true;
        if (c.type === "invest") met = tokens >= Number(c.amount || 0);
        else if (c.type === "reduce") met = tokens <= Number(c.amount || 0);
        else if (c.type === "maintain") met = s ? ((s.byUid[uid] || 0) / Math.max(1e-9, s.node.requirement) * 100) >= Number(c.amount || 0) : false;
        else if (c.type === "lead") met = !!(s && s.lead === uid);
        out.push({ agreementId: a.id, title: a.title, commitment: c, met, breachable: BREACHABLE.indexOf(c.type) >= 0, tokens });
      });
    });
    return out;
  }

  /**
   * applyAgreementsToPlan — pre-fill a plan so it satisfies the player's
   * accepted commitments, respecting the budget. Used by the Commitment phase
   * ("adopt my commitments") and by the AI planner.
   */
  function applyAgreementsToPlan(alloc, uid, agreements, nodes, budget, country) {
    const next = Object.assign({}, alloc || {});
    const nodesById = {}; (nodes || []).forEach(n => nodesById[n.id] = n);
    const obligations = [];
    (agreements || []).filter(a => a && a.status === "accepted").forEach(a => {
      (a.commitments || []).forEach(c => {
        if (c.party !== uid) return;
        if (c.type === "invest" || c.type === "reduce") obligations.push(c);
        else if (c.type === "maintain" && country && nodesById[c.nodeId]) {
          // "hold N% of allied requirement" → the tokens that actually buys,
          // given this country's efficiency in that node. Without this, a
          // maintain promise is impossible to fund and reads as a breach.
          const n = nodesById[c.nodeId];
          const perToken = n.capacityPerToken * countryEfficiency(country, n);
          const needed = Math.ceil((Number(c.amount || 0) / 100 * n.requirement) / Math.max(1e-9, perToken));
          obligations.push({ party: uid, type: "invest", nodeId: c.nodeId, amount: needed, derivedFrom: "maintain" });
        }
      });
    });
    // caps first (free tokens), then minimums
    obligations.filter(o => o.type === "reduce").forEach(o => {
      next[o.nodeId] = Math.min(Math.max(0, Number(next[o.nodeId]) || 0), Number(o.amount || 0));
    });
    obligations.filter(o => o.type === "invest").forEach(o => {
      next[o.nodeId] = Math.max(Math.max(0, Number(next[o.nodeId]) || 0), Number(o.amount || 0));
    });
    // if we blew the budget, shave from nodes that carry no minimum obligation
    const mins = {}; obligations.filter(o => o.type === "invest").forEach(o => mins[o.nodeId] = Math.max(mins[o.nodeId] || 0, Number(o.amount || 0)));
    let spent = sum(nodes.map(n => Math.max(0, Number(next[n.id]) || 0)));
    const shaveOrder = nodes.slice().sort((a, b) => (Number(next[b.id]) || 0) - (Number(next[a.id]) || 0));
    let guard = 0;
    while (spent > budget && guard++ < 500) {
      let moved = false;
      for (const n of shaveOrder) {
        const cur = Math.max(0, Number(next[n.id]) || 0);
        if (cur > (mins[n.id] || 0)) { next[n.id] = cur - 1; spent--; moved = true; if (spent <= budget) break; }
      }
      if (!moved) break;   // everything left is contractually locked
    }
    Object.keys(next).forEach(k => { if (!next[k]) delete next[k]; });   // no zero entries
    return next;
  }

  /* ==========================================================================
     13. AI NATIONAL PLANNER  (bots + auto-submit for idle players)
     ========================================================================== */

  /**
   * nodeAppeal — how attractive a node looks to a country acting in its own
   * narrow interest. Deliberately weights value-added and jobs heavily and
   * strategic/chokepoint value lightly: filling the alliance's chokepoint is a
   * public good, and a purely national planner under-provides public goods.
   * This is what generates the "everyone built cells, nobody built graphite"
   * uncoordinated baseline.
   */
  function nodeAppeal(country, node) {
    const eff = countryEfficiency(country, node);
    return Math.pow(eff, 1.3) *
      (0.35 + node.valueAdd) *
      (0.85 + 0.30 * node.employmentIntensity);
    // NOTE: strategicImportance is deliberately absent. Filling the alliance's
    // chokepoint is a public good; a purely national planner under-provides it.
  }

  /**
   * autoPlan — a deterministic "rationally selfish" national plan.
   * Industrial policy is lumpy, so tokens are committed in project-sized chunks
   * of 2 with only mild diminishing returns. The result is a concentrated plan
   * that piles into whatever the country is personally best at — which is what
   * produces the classic uncoordinated failure (four gigafactories, no anodes).
   */
  const AI_CHUNK = 2, AI_DECAY = 0.93;
  function autoPlan(country, nodes, budget, opts) {
    const o = opts || {};
    const alloc = {};
    const appeal = {};
    nodes.forEach(n => appeal[n.id] = nodeAppeal(country, n) * (o.bias && o.bias[n.id] ? o.bias[n.id] : 1));
    const maxPerNode = Math.max(2, Math.ceil(budget * 0.5));
    let left = budget, guard = 0;
    while (left > 0 && guard++ < 200) {
      const step = Math.min(AI_CHUNK, left);
      let best = null, bestV = -1;
      nodes.forEach(n => {
        const cur = alloc[n.id] || 0;
        if (cur >= maxPerNode) return;
        const v = appeal[n.id] * Math.pow(AI_DECAY, cur);
        if (v > bestV + 1e-12) { bestV = v; best = n; }
      });
      if (!best) break;
      alloc[best.id] = (alloc[best.id] || 0) + step;
      left -= step;
    }
    return o.agreements
      ? applyAgreementsToPlan(alloc, o.uid, o.agreements, nodes, budget, country)
      : alloc;
  }

  /**
   * aiRespondToProposal — a bot's deterministic accept/reject.
   * It compares the tokens the deal asks it to move against its own
   * preferences, and prices the compensation it receives. Not clever; legible.
   */
  function aiRespondToProposal(proposal, uid, country, nodes, budget, agreements) {
    const preferred = autoPlan(country, nodes, budget);
    const nodesById = {}; nodes.forEach(n => nodesById[n.id] = n);
    let cost = 0, benefit = 0;
    (proposal.commitments || []).forEach(c => {
      const n = nodesById[c.nodeId]; if (!n) return;
      const pref = preferred[c.nodeId] || 0;
      if (c.party === uid) {
        if (c.type === "invest") cost += Math.max(0, Number(c.amount || 0) - pref) * (1.35 - countryEfficiency(country, n) / CFG.effMax);
        else if (c.type === "reduce") cost += Math.max(0, pref - Number(c.amount || 0)) * 0.9;
        else if (c.type === "maintain") cost += 0.6;
        else if (c.type === "lead") benefit += 1.6;
        else if (c.type === "finance") cost += 1.6;
        else if (c.type === "offtake") cost += 0.8;
        else if (c.type === "rnd") benefit += 0.8;
      } else {
        if (c.type === "finance" || c.type === "offtake" || c.type === "access") benefit += 1.7;
        if (c.type === "rnd") benefit += 0.7;
        if (c.type === "invest") {
          // a partner taking a node I'm weak in is worth something to me
          if (countryEfficiency(country, n) < 1.05) benefit += 0.5;
          const iBuildIt = (preferred[c.nodeId] || 0) > 0;
          if (!iBuildIt) {
            // ...and a partner supplying an input I need but wasn't going to
            // build myself is worth a lot more. Without this, bots refuse
            // perfectly reasonable offers to fill the alliance's own gaps.
            const feedsMe = (n.feeds || []).some(d => (preferred[d] || 0) > 0);
            const iDependOnIt = nodes.some(x => (preferred[x.id] || 0) > 0 && (x.dependencies || []).indexOf(c.nodeId) >= 0);
            if (feedsMe || iDependOnIt) benefit += 1.4;
            // allied supply security in a strategic node is worth something too
            benefit += 0.6 * n.strategicImportance;
          }
        }
      }
    });
    if ((proposal.commitments || []).some(c => c.type === "pact")) benefit += 0.8;
    const accept = benefit >= cost * 0.55;
    return { accept, cost: round1(cost), benefit: round1(benefit) };
  }

  /* ==========================================================================
     14. OPTIMAL BENCHMARK — "best achievable" for the stress-test comparison.
     A greedy allocator: repeatedly hand the next token to the (member, node)
     pair with the largest marginal gain in the alliance score. Not a proof of
     optimality — a strong, reproducible upper reference the players can beat.
     ========================================================================== */
  function optimalBenchmark(ctx) {
    const { nodes, members, countries, budget, installed } = ctx;
    const plans = {}; members.forEach(m => plans[m.uid] = { alloc: {} });
    const spent = {}; members.forEach(m => spent[m.uid] = 0);
    const base = { nodes, members, countries, installed, agreements: [] };
    const totalTokens = members.length * budget;
    for (let i = 0; i < totalTokens; i++) {
      let best = null, bestGain = -Infinity;
      const current = scoreAlliance(Object.assign({}, base, { plans })).headlineRaw;
      members.forEach(m => {
        if (spent[m.uid] >= budget) return;
        nodes.forEach(n => {
          const a = plans[m.uid].alloc;
          a[n.id] = (a[n.id] || 0) + 1;
          const gain = scoreAlliance(Object.assign({}, base, { plans })).headlineRaw - current;
          a[n.id] -= 1; if (!a[n.id]) delete a[n.id];
          if (gain > bestGain + 1e-9) { bestGain = gain; best = { uid: m.uid, nodeId: n.id }; }
        });
      });
      if (!best || bestGain <= 0.0005) break;      // no more useful spending
      const a = plans[best.uid].alloc;
      a[best.nodeId] = (a[best.nodeId] || 0) + 1;
      spent[best.uid]++;
    }
    const alliance = scoreAlliance(Object.assign({}, base, { plans }));
    return { plans, alliance };
  }

  /* ==========================================================================
     15. ALLIANCE STRESS TEST — the end-game, in place of a boss fight.
     resilience% = share of strategically-weighted allied demand still met once
     the stress is applied. Formulas:
       export_restrictions  single-allied-supplier nodes lose `singleSourceHaircut`;
                            sub-scale national plants shut; broken upstream
                            multiplies the node down by its dependency factor.
       demand_surge         requirement × multiplier; only at-scale national
                            plants can respond.
       fiscal_retrenchment  every plant is cut by `capacityHaircut`, then any
                            national plant below minimum viable scale closes.
     ========================================================================== */
  function runStressTest(testId, ctx) {
    const test = DATA.getStressTest(testId);
    const alliance = ctx.alliance || scoreAlliance(ctx);
    const p = (test && test.params) || {};
    let num = 0, den = 0;
    const rows = [];
    alliance.nodes.forEach(s => {
      const n = s.node;
      const w = 0.5 + n.strategicImportance;
      let req = n.requirement;
      // The inherited installed base (carried from earlier rounds) is existing
      // at-scale industry with no single owner: it is not subject to the
      // sub-scale shutdown rule, but it does take the retrenchment haircut.
      // It must be counted — rebuilding capacity from per-country plants alone
      // would silently delete it and make every later round look brittle.
      const installedBase = ((ctx.installed || {})[n.id] || 0) *
        (test && test.kind === "fiscal_retrenchment" ? (1 - (p.capacityHaircut || 0)) : 1);
      let cap = installedBase;
      // Rebuild capacity plant-by-plant so shutdown rules can bite. A plant is
      // "viable" on the same threshold the scoring model uses for a supplier
      // (CFG.meaningfulShare of the node's requirement) — see the note in
      // coord-scenarios.js.
      const viable = CFG.meaningfulShare * n.requirement;
      Object.keys(s.byUid).forEach(uid => {
        let c = s.byUid[uid] || 0;
        if (c <= 0) return;
        if (test && test.kind === "fiscal_retrenchment") c *= (1 - (p.capacityHaircut || 0));
        const subScale = c < viable;
        if (p.subScaleShutdown && subScale) c = 0;
        if (test && test.kind === "demand_surge" && subScale) c = 0;
        cap += c;
      });
      if (test && test.kind === "export_restrictions") {
        if (s.supplierCountries.length <= 1) cap *= (1 - (p.singleSourceHaircut || 0));
        cap *= (0.45 + 0.55 * s.depFactor);         // missing inputs throttle output
      }
      if (test && test.kind === "demand_surge") req *= (p.demandMultiplier || 1);
      const met = req > 0 ? Math.min(1, cap / req) : 1;
      rows.push({ nodeId: n.id, short: n.short, met: Math.round(met * 100), capacity: round1(cap), requirement: round1(req) });
      num += w * met; den += w;
    });
    return {
      testId, title: test ? test.title : testId, blurb: test ? test.blurb : "",
      resilience: Math.round(100 * (den ? num / den : 0)), rows,
    };
  }

  /** Run every stress test in the scenario and average them. */
  function runAllStressTests(scenarioId, ctx) {
    const scenario = DATA.getScenario(scenarioId);
    const ids = scenario.stressTests || DATA.COORD_STRESS_TESTS.map(t => t.id);
    const tests = ids.map(id => runStressTest(id, ctx));
    return { tests, overall: Math.round(sum(tests.map(t => t.resilience)) / Math.max(1, tests.length)) };
  }

  /**
   * priceOfNonCoordination — the headline number for the end screen.
   * gap        = how much better the benchmark is than uncoordinated plans
   * captured   = how much of that gap the negotiated alliance actually captured
   * avoidedPct = captured / gap  ("price of non-coordination avoided: 78%")
   * This is a game metric, not a welfare estimate.
   */
  function priceOfNonCoordination(independent, negotiated, benchmark) {
    const gap = benchmark - independent;
    const captured = negotiated - independent;
    // If the benchmark didn't beat the uncoordinated baseline on this measure
    // there is no gap to express a share of. Say so rather than printing a
    // meaningless 100%.
    const measurable = gap > 0.5;
    return {
      independent, negotiated, benchmark,
      gain: Math.round(captured * 10) / 10,
      gap: Math.round(gap * 10) / 10,
      measurable,
      avoidedPct: measurable ? clamp(Math.round(100 * captured / gap), -100, 200) : null,
    };
  }

  /* ==========================================================================
     16. ROUND ORCHESTRATION HELPERS
     ========================================================================== */

  const PHASES = [
    { id: "planning",   n: 1, name: "National Planning",  short: "PLAN",     blurb: "Allocate your policy tokens privately. Nobody sees your plan." },
    { id: "coordinator",n: 2, name: "Coordinator Report", short: "REPORT",   blurb: "The secretariat aggregates every plan and reports what the alliance built — and didn't." },
    { id: "negotiation",n: 3, name: "Negotiation",        short: "DEALS",    blurb: "Propose, counter and accept structured agreements in the Deal Room." },
    { id: "commitment", n: 4, name: "Commitment",         short: "COMMIT",   blurb: "Lock your final plan. Keep your promises — or don't, and pay for it." },
    { id: "results",    n: 5, name: "Results",            short: "RESULTS",  blurb: "Before vs after coordination, national outcomes, and coalition stability." },
  ];
  const phaseIndex = (id) => Math.max(0, PHASES.findIndex(p => p.id === id));
  const nextPhase = (id) => PHASES[Math.min(PHASES.length - 1, phaseIndex(id) + 1)].id;

  /** Which shock hits at the start of `round` (round 1 has none). */
  function shockForRound(scenarioId, round) {
    const scenario = DATA.getScenario(scenarioId);
    const order = scenario.shockOrder || [];
    if (round <= 1) return null;
    return order[(round - 2) % Math.max(1, order.length)] || null;
  }
  /** All shocks in force at `round` (cumulative). */
  function shocksThrough(scenarioId, round) {
    const out = [];
    for (let r = 2; r <= round; r++) { const s = shockForRound(scenarioId, r); if (s) out.push(s); }
    return out;
  }

  /**
   * installedFrom — carry a share of last round's capacity forward as the
   * installed industrial base (CFG.inheritanceFactor).
   */
  function installedFrom(allianceResult) {
    const out = {};
    if (!allianceResult) return out;
    (allianceResult.nodes || []).forEach(s => out[s.nodeId] = s.capacity * CFG.inheritanceFactor);
    return out;
  }

  /**
   * buildRoundContext — the single entry point the UI uses. Assembles a fully
   * resolved scoring context for one round from the synchronized game state.
   */
  function buildRoundContext(game, opts) {
    const o = opts || {};
    const members = game.members || [];
    const round = o.round || game.round || 1;
    const shockIds = shocksThrough(game.scenario, round);
    const nodes = resolveNodes(game.scenario, members.length, shockIds);
    const budget = budgetFor(shockIds, game.tokensPerRound);
    const plans = o.plans || ((game.plans || {})[round] || {});
    const agreements = o.agreements || (game.agreements || []);
    return {
      nodes, members, plans, countries: game.countries, agreements,
      installed: o.installed || (game.installed || {}), budget, round, shockIds,
      objectivesByUid: game.objectives || {}, trust: game.trust || {},
    };
  }

  /* ==========================================================================
     17. PUBLIC API
     ========================================================================== */
  return {
    /* config passthrough */
    CFG, DATA, PHASES, STATUS, COMMITMENT_TYPES, BREACHABLE,
    phaseIndex, nextPhase, commitmentType,
    /* scenario */
    resolveNodes, evaluateShock, budgetFor, shockForRound, shocksThrough, installedFrom,
    buildRoundContext,
    /* physics */
    countryEfficiency, calculateSectorCapacity, calculateAllCapacity, agreementEffects,
    /* alliance scoring */
    scoreAlliance, calculateScaleScore, calculateSupplyChainCompleteness,
    calculateResilienceBonus, calculateDuplicationPenalty, calculateStrategicCoverage,
    calculateComplementarity, nodeStatus, generateCoordinatorReport,
    /* national scoring */
    scoreCountry, scoreAllCountries, assignObjectives, checkObjectives, bestNodeFor,
    coalitionStability,
    /* deals */
    evaluateCompliance, applyTrust, commitmentsFor, applyAgreementsToPlan, describeCommitment,
    /* ai + benchmarks */
    nodeAppeal, autoPlan, aiRespondToProposal, optimalBenchmark,
    runStressTest, runAllStressTests, priceOfNonCoordination,
    /* utils */
    hashString, clamp,
  };
});
