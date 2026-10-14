/* ============================================================================
   ALLIANCE ARCHITECT — SCORING ENGINE TESTS
   ----------------------------------------------------------------------------
   Dual-mode. Runs in:
     • Node    →  node scripts/test-coordination.mjs
     • Browser →  open coord-tests.html  (or COORD_TESTS.run() in the console)
   No test framework, matching the repo's existing scripts/test.mjs style.
   ============================================================================ */
(function (root, factory) {
  const MODEL = (typeof require === "function" && typeof module !== "undefined")
    ? require("./coord-model.js") : root.COORD;
  const DATA = (typeof require === "function" && typeof module !== "undefined")
    ? require("./coord-scenarios.js") : root.COORD_DATA;
  const API = factory(MODEL, DATA);
  if (typeof module !== "undefined" && module.exports) module.exports = API;
  if (typeof window !== "undefined") window.COORD_TESTS = API;
})(typeof globalThis !== "undefined" ? globalThis : this, function (M, DATA) {
  "use strict";

  /* ---------- country fixtures (mirror data.js ALLIES home synergies) ---------- */
  const FIXTURE_COUNTRIES = {
    US: { id: "US", name: "United States", flag: "🇺🇸", home: { B: 7, D: 6, E: 6, F: 5, A: 3 } },
    JP: { id: "JP", name: "Japan",         flag: "🇯🇵", home: { B: 8, A: 6, E: 6, F: 5 } },
    KR: { id: "KR", name: "South Korea",   flag: "🇰🇷", home: { B: 9, E: 6, D: 4 } },
    AU: { id: "AU", name: "Australia",     flag: "🇦🇺", home: { A: 9, D: 7, F: 6 } },
    CA: { id: "CA", name: "Canada",        flag: "🇨🇦", home: { A: 9, D: 6, F: 5 } },
    DE: { id: "DE", name: "Germany",       flag: "🇩🇪", home: { B: 8, E: 7, C: 6, F: 4 } },
  };
  const countries = () => (typeof ALLY_BY_ID !== "undefined" && ALLY_BY_ID.US) ? ALLY_BY_ID : FIXTURE_COUNTRIES;

  const MEMBERS4 = [
    { uid: "u_us", name: "US player", countryId: "US" },
    { uid: "u_jp", name: "JP player", countryId: "JP" },
    { uid: "u_kr", name: "KR player", countryId: "KR" },
    { uid: "u_au", name: "AU player", countryId: "AU" },
  ];

  function ctxFor(members, plans, opts) {
    const o = opts || {};
    const nodes = M.resolveNodes("batteries", members.length, o.shocks || []);
    return {
      nodes, members, countries: countries(),
      plans, agreements: o.agreements || [], installed: o.installed || {},
      budget: M.budgetFor(o.shocks || [], null),
      trust: o.trust || {},
    };
  }
  const plansOf = (map) => {
    const p = {};
    Object.keys(map).forEach(u => p[u] = { alloc: map[u], submitted: true });
    return p;
  };

  /* ---------- runner ---------- */
  function run(log) {
    const out = [];
    let pass = 0, fail = 0;
    const say = (s) => { out.push(s); if (log) log(s); };
    const ok = (cond, msg) => {
      if (cond) { pass++; } else { fail++; say("  ✗ " + msg); }
    };
    const group = (name) => say("• " + name);

    /* ======================================================================
       1. INDEPENDENT OVERBUILDING
       Three countries pile into cells; graphite and recycling are empty.
       The alliance score must suffer, and the coordinator must name it.
       ====================================================================== */
    group("independent overbuilding");
    const overbuild = plansOf({
      u_us: { cells: 6, packs: 4 },
      u_jp: { cells: 6, cathode_cam: 4 },
      u_kr: { cells: 7, packs: 3 },
      u_au: { lithium_mining: 6, nickel_processing: 4 },
    });
    const cOver = ctxFor(MEMBERS4, overbuild);
    const aOver = M.scoreAlliance(cOver);
    const repOver = M.generateCoordinatorReport(Object.assign({}, cOver, { alliance: aOver }));
    const cellsRow = repOver.nodes.find(n => n.nodeId === "cells");
    const graphiteRow = repOver.nodes.find(n => n.nodeId === "graphite_anode");
    ok(cellsRow.pct > 150, "cells overbuilt (>150% of requirement), got " + cellsRow.pct + "%");
    ok(cellsRow.status === "EXCESS", "cells flagged EXCESS DUPLICATION, got " + cellsRow.status);
    ok(graphiteRow.status === "MISSING", "graphite flagged NOT PLANNED, got " + graphiteRow.status);
    ok(aOver.summary.chainBreaks >= 1, "at least one chain break reported, got " + aOver.summary.chainBreaks);
    ok(repOver.findings.some(f => f.kind === "duplication"), "coordinator reports a duplication finding");
    ok(repOver.findings.some(f => f.kind === "shortage"), "coordinator reports a shortage finding");
    ok(aOver.headline < 60, "overbuilt alliance scores below 60, got " + aOver.headline);

    /* ======================================================================
       2. COORDINATED SPECIALIZATION
       Same 40 tokens, reallocated to fill the missing nodes → higher score.
       ====================================================================== */
    group("coordinated specialization");
    const coordinated = plansOf({
      u_us: { cells: 3, bms: 3, recycling: 4 },
      u_jp: { graphite_anode: 5, cathode_cam: 5 },
      u_kr: { cells: 5, cathode_cam: 2, packs: 3 },
      u_au: { lithium_mining: 4, lithium_refining: 4, nickel_processing: 2 },
    });
    const cCoord = ctxFor(MEMBERS4, coordinated);
    const aCoord = M.scoreAlliance(cCoord);
    ok(aCoord.headline > aOver.headline,
      `coordinated (${aCoord.headline}) beats uncoordinated (${aOver.headline})`);
    ok(aCoord.headline - aOver.headline >= 8,
      `coordination gain is material (+${aCoord.headline - aOver.headline})`);
    ok(aCoord.components.completeness > aOver.components.completeness, "completeness improves");
    ok(aCoord.components.duplication > aOver.components.duplication, "duplication waste falls");
    ok(aCoord.summary.criticalShortfalls <= aOver.summary.criticalShortfalls, "fewer critical shortfalls");

    /* Same token count in both plans — this is a reallocation, not more money. */
    const tokensIn = (p) => Object.keys(p).reduce((t, u) =>
      t + Object.keys(p[u].alloc).reduce((s, k) => s + p[u].alloc[k], 0), 0);
    ok(tokensIn(overbuild) === tokensIn(coordinated),
      "both plans spend the same tokens (" + tokensIn(overbuild) + " vs " + tokensIn(coordinated) + ")");

    /* ======================================================================
       3. STRATEGIC REDUNDANCY
       graphite_anode has strategicRedundancyTarget 2: two suppliers at the same
       TOTAL capacity must beat one supplier.
       ====================================================================== */
    group("strategic redundancy (2 suppliers > 1)");
    const single = plansOf({ u_jp: { graphite_anode: 8 }, u_us: {}, u_kr: {}, u_au: {} });
    const dual = plansOf({ u_jp: { graphite_anode: 4 }, u_kr: { graphite_anode: 4 }, u_us: {}, u_au: {} });
    const sSingle = M.calculateSectorCapacity(M.resolveNodes("batteries", 4, [])[3], ctxFor(MEMBERS4, single));
    const rSingle = M.scoreAlliance(ctxFor(MEMBERS4, single));
    const rDual = M.scoreAlliance(ctxFor(MEMBERS4, dual));
    const gS = rSingle.byId.graphite_anode, gD = rDual.byId.graphite_anode;
    ok(sSingle.nodeId === "graphite_anode", "resolveNodes index 3 is graphite_anode");
    ok(gS.supplierCountries.length === 1 && gD.supplierCountries.length === 2,
      `supplier counts 1 vs 2, got ${gS.supplierCountries.length} vs ${gD.supplierCountries.length}`);
    const resS = M.calculateResilienceBonus([gS]).score, resD = M.calculateResilienceBonus([gD]).score;
    ok(resD > resS, `resilience 2-supplier (${Math.round(resD)}) > 1-supplier (${Math.round(resS)})`);
    ok(rDual.components.resilience > rSingle.components.resilience, "alliance resilience component higher with two suppliers");
    ok(rDual.headline >= rSingle.headline, `dual-source headline ${rDual.headline} >= single-source ${rSingle.headline}`);

    /* ======================================================================
       4. EXCESSIVE REDUNDANCY
       Five suppliers each building a big cells plant → waste must bite.
       ====================================================================== */
    group("excessive redundancy is penalised");
    const MEMBERS5 = MEMBERS4.concat([{ uid: "u_de", name: "DE player", countryId: "DE" }]);
    const three = plansOf({ u_us: { cells: 4 }, u_jp: { cells: 4 }, u_kr: { cells: 4 }, u_au: {}, u_de: {} });
    const five = plansOf({ u_us: { cells: 4 }, u_jp: { cells: 4 }, u_kr: { cells: 4 }, u_au: { cells: 4 }, u_de: { cells: 4 } });
    const a3 = M.scoreAlliance(ctxFor(MEMBERS5, three));
    const a5 = M.scoreAlliance(ctxFor(MEMBERS5, five));
    ok(a5.byId.cells.investors === 5, "five investors counted, got " + a5.byId.cells.investors);
    ok(a5.components.duplication < a3.components.duplication,
      `duplication component falls ${Math.round(a3.components.duplication)} → ${Math.round(a5.components.duplication)}`);
    const dup5 = M.calculateDuplicationPenalty([a5.byId.cells]).detail[0];
    ok(dup5.crowding >= 1, "crowding term triggered, got " + dup5.crowding);
    ok(a5.headline <= a3.headline, `five-way pile-in does not out-score three-way (${a5.headline} <= ${a3.headline})`);
    ok(M.calculateScaleScore(a5.byId.cells.capacity, a5.byId.cells.node) < 100, "cells scale score decays past maximum useful capacity");

    /* ======================================================================
       5. NATIONAL SACRIFICE
       Japan drops cells (its best node) and takes graphite + recycling instead.
       Alliance up, Japan's own score down. That tension must exist.
       ====================================================================== */
    group("national sacrifice: alliance up, one nation down");
    /* Only Japan's plan differs. In `selfish` it builds the two nodes it is
       personally best at (cells + cathode, both already well supplied by Korea
       and the US). In `sacrifice` it takes the two nodes nobody wants
       (graphite + recycling), which are worth less at home. */
    const selfish = plansOf({
      u_us: { cells: 4, bms: 3, packs: 3 },
      u_jp: { cells: 6, cathode_cam: 4 },
      u_kr: { cells: 5, cathode_cam: 3, packs: 2 },
      u_au: { lithium_mining: 5, lithium_refining: 3, nickel_processing: 2 },
    });
    const sacrifice = plansOf({
      u_us: { cells: 4, bms: 3, packs: 3 },
      u_jp: { graphite_anode: 6, recycling: 4 },
      u_kr: { cells: 5, cathode_cam: 3, packs: 2 },
      u_au: { lithium_mining: 5, lithium_refining: 3, nickel_processing: 2 },
    });
    const cSelf = ctxFor(MEMBERS4, selfish), cSac = ctxFor(MEMBERS4, sacrifice);
    const aSelf = M.scoreAlliance(cSelf), aSac = M.scoreAlliance(cSac);
    const objsByUid = {}; MEMBERS4.forEach(m => objsByUid[m.uid] = M.assignObjectives("seed-test", m.uid, m.countryId));
    const nSelf = M.scoreAllCountries(Object.assign({}, cSelf, { alliance: aSelf, objectivesByUid: objsByUid }));
    const nSac = M.scoreAllCountries(Object.assign({}, cSac, { alliance: aSac, objectivesByUid: objsByUid }));
    ok(aSac.headline > aSelf.headline, `alliance rises ${aSelf.headline} → ${aSac.headline}`);
    ok(nSac.u_jp.total < nSelf.u_jp.total, `Japan's national score falls ${nSelf.u_jp.total} → ${nSac.u_jp.total}`);
    ok(nSac.u_jp.components.domestic < nSelf.u_jp.components.domestic, "the loss shows up in domestic industrial value");

    /* ======================================================================
       6. UNSTABLE BARGAIN
       A negotiated outcome that badly harms one participant must score lower on
       coalition stability than one that spreads the pain.
       ====================================================================== */
    group("coalition stability");
    /* Synthetic before/after tables so the property under test is isolated. */
    const before = { u_us: { total: 70 }, u_jp: { total: 70 }, u_kr: { total: 70 }, u_au: { total: 70 } };
    const evenGains = { u_us: { total: 74 }, u_jp: { total: 73 }, u_kr: { total: 76 }, u_au: { total: 75 } };
    const oneCarriesIt = { u_us: { total: 80 }, u_jp: { total: 56 }, u_kr: { total: 84 }, u_au: { total: 82 } };
    const stabEven = M.coalitionStability(before, evenGains, MEMBERS4);
    const stabHurt = M.coalitionStability(before, oneCarriesIt, MEMBERS4);
    ok(stabEven.score > stabHurt.score,
      `even gains (${stabEven.score}) more stable than one member carrying the loss (${stabHurt.score})`);
    ok(stabEven.stable === true && stabEven.verdict === "STABLE", "an all-gain outcome is stable");
    ok(stabHurt.losers.indexOf("u_jp") >= 0 && stabHurt.worst.uid === "u_jp", "Japan identified as the worst-off member");
    ok(stabHurt.stable === false, "a bargain that costs one member 14 points is not stable");
    const brutal = M.coalitionStability(before, Object.assign({}, evenGains, { u_jp: { total: 12 } }), MEMBERS4);
    ok(brutal.score < 45 && brutal.verdict === "UNSTABLE", "a member driven to 12 makes the bargain UNSTABLE, got " + brutal.verdict);
    ok(brutal.floorBreaches.indexOf("u_jp") >= 0, "the floor breach is reported");
    /* and the real sacrifice case must register as a distributional problem */
    const stabReal = M.coalitionStability(nSelf, nSac, MEMBERS4);
    ok(stabReal.worst.uid === "u_jp" && stabReal.worst.delta < 0, "the realised sacrifice shows Japan carrying the loss");

    /* ======================================================================
       7. SHOCK
       The graphite export-control shock must punish an alliance that depends on
       a thin graphite node, and must not punish one that built it out.
       ====================================================================== */
    group("shock: graphite export controls");
    const thin = plansOf({
      u_us: { cells: 4, bms: 3, packs: 3 }, u_jp: { cathode_cam: 5, graphite_anode: 2, cells: 3 },
      u_kr: { cells: 6, packs: 4 }, u_au: { lithium_mining: 4, lithium_refining: 4, nickel_processing: 2 },
    });
    const thick = plansOf({
      u_us: { cells: 3, bms: 3, recycling: 4 }, u_jp: { graphite_anode: 5, cathode_cam: 5 },
      u_kr: { cells: 5, graphite_anode: 2, packs: 3 }, u_au: { lithium_mining: 4, lithium_refining: 4, nickel_processing: 2 },
    });
    const shock = ["graphite_controls"];
    const thinPre = M.scoreAlliance(ctxFor(MEMBERS4, thin));
    const thinPost = M.scoreAlliance(ctxFor(MEMBERS4, thin, { shocks: shock }));
    const thickPre = M.scoreAlliance(ctxFor(MEMBERS4, thick));
    const thickPost = M.scoreAlliance(ctxFor(MEMBERS4, thick, { shocks: shock }));
    ok(thinPost.headline < thinPre.headline, `thin-graphite alliance is hurt by the shock (${thinPre.headline} → ${thinPost.headline})`);
    ok((thinPre.headline - thinPost.headline) > (thickPre.headline - thickPost.headline),
      "the thin-graphite alliance is hurt more than the built-out one");
    const shockNodes = M.resolveNodes("batteries", 4, shock);
    const gNode = shockNodes.find(n => n.id === "graphite_anode");
    ok(gNode.requirement > M.resolveNodes("batteries", 4, []).find(n => n.id === "graphite_anode").requirement,
      "the shock raises graphite requirement");
    ok(M.budgetFor(["ai_demand"], 10) === 12 && M.budgetFor(["subsidy_retrenchment"], 10) === 7,
      "shock budget deltas apply (+2 / -3)");

    /* ======================================================================
       8. AI PLANNER produces the designed uncoordinated failure
       ====================================================================== */
    group("AI national planner");
    const nodes4 = M.resolveNodes("batteries", 4, []);
    const aiPlans = {};
    MEMBERS4.forEach(m => aiPlans[m.uid] = { alloc: M.autoPlan(countries()[m.countryId], nodes4, 10), submitted: true });
    MEMBERS4.forEach(m => {
      const t = Object.values(aiPlans[m.uid].alloc).reduce((a, b) => a + b, 0);
      ok(t === 10, `${m.countryId} bot spends its whole budget, got ${t}`);
    });
    const aAI = M.scoreAlliance(ctxFor(MEMBERS4, aiPlans));
    ok(aAI.byId.cells.investors >= 3, "independent bots pile into cells, got " + aAI.byId.cells.investors + " investors");
    ok(aAI.byId.recycling.capacity < aAI.byId.recycling.node.minViable, "independent bots leave recycling below minimum viable scale");
    ok(aAI.headline < 70, "the purely national baseline is mediocre, got " + aAI.headline);
    ok(JSON.stringify(M.autoPlan(countries().KR, nodes4, 10)) === JSON.stringify(M.autoPlan(countries().KR, nodes4, 10)),
      "autoPlan is deterministic");

    /* AI deal responses: a bot must accept an offer that fills a gap it depends
       on, and refuse one that only costs it something. */
    const fillsMyGap = {
      id: "prop_good", title: "You take the anodes", proposer: "u_kr", participants: ["u_kr", "u_jp"],
      commitments: [{ party: "u_kr", type: "invest", nodeId: "graphite_anode", amount: 5 },
                    { party: "u_jp", type: "offtake", nodeId: "graphite_anode" }],
    };
    const allCostNoGain = {
      id: "prop_bad", title: "Fund my factory", proposer: "u_kr", participants: ["u_kr", "u_jp"],
      commitments: [{ party: "u_jp", type: "finance", nodeId: "cells" },
                    { party: "u_jp", type: "reduce", nodeId: "cells", amount: 0 }],
    };
    const rGood = M.aiRespondToProposal(fillsMyGap, "u_jp", countries().JP, nodes4, 10, []);
    const rBad = M.aiRespondToProposal(allCostNoGain, "u_jp", countries().JP, nodes4, 10, []);
    ok(rGood.accept === true, `a bot accepts an offer to fill an input it depends on (benefit ${rGood.benefit} vs cost ${rGood.cost})`);
    ok(rBad.accept === false, `a bot refuses a deal that is all cost (benefit ${rBad.benefit} vs cost ${rBad.cost})`);
    ok(JSON.stringify(M.aiRespondToProposal(fillsMyGap, "u_jp", countries().JP, nodes4, 10, [])) === JSON.stringify(rGood),
      "AI deal responses are deterministic");

    /* ======================================================================
       9. AGREEMENTS, COMPLIANCE AND TRUST
       ====================================================================== */
    group("deals, compliance and trust");
    const pact = {
      id: "ag1", title: "Allied Battery Compact", proposer: "u_us", status: "accepted",
      participants: ["u_us", "u_jp", "u_kr", "u_au"],
      commitments: [
        { party: "u_au", type: "invest", nodeId: "lithium_refining", amount: 4 },
        { party: "u_jp", type: "invest", nodeId: "graphite_anode", amount: 5 },
        { party: "u_kr", type: "lead", nodeId: "cells" },
        { party: "u_us", type: "offtake", nodeId: "cells" },
        { party: "u_us", type: "finance", nodeId: "graphite_anode" },
        { party: "u_us", type: "invest", nodeId: "recycling", amount: 3 },
      ],
    };
    const honoring = plansOf({
      u_us: { recycling: 3, bms: 3, cells: 4 },
      u_jp: { graphite_anode: 5, cathode_cam: 5 },
      u_kr: { cells: 6, packs: 4 },
      u_au: { lithium_refining: 4, lithium_mining: 4, nickel_processing: 2 },
    });
    const cDeal = ctxFor(MEMBERS4, honoring, { agreements: [pact] });
    const aDeal = M.scoreAlliance(cDeal);
    const compOk = M.evaluateCompliance([pact], Object.assign({}, cDeal, { alliance: aDeal }));
    ok(compOk.honoredIds.length === 1 && compOk.breachedIds.length === 0, "a kept compact is honoured");
    ok(Object.keys(compOk.defectorsByUid).length === 0, "no defectors when everyone delivers");

    const defecting = plansOf({
      u_us: { recycling: 3, bms: 3, cells: 4 },
      u_jp: { cells: 6, cathode_cam: 4 },        // promised graphite 5, delivered 0
      u_kr: { cells: 6, packs: 4 },
      u_au: { lithium_refining: 4, lithium_mining: 4, nickel_processing: 2 },
    });
    const cDef = ctxFor(MEMBERS4, defecting, { agreements: [pact] });
    const aDef = M.scoreAlliance(cDef);
    const compBad = M.evaluateCompliance([pact], Object.assign({}, cDef, { alliance: aDef }));
    ok(compBad.breachedIds.length === 1, "the broken compact is flagged breached");
    ok(compBad.defectorsByUid.u_jp && compBad.defectorsByUid.u_jp.length === 1, "Japan named as the defector");
    ok(aDef.headline < aDeal.headline, `defection lowers the alliance score (${aDeal.headline} → ${aDef.headline})`);
    const trust0 = {}; MEMBERS4.forEach(m => trust0[m.uid] = { rep: 60, honored: 0, defected: 0 });
    const trustAfterGood = M.applyTrust(trust0, MEMBERS4, compOk);
    const trustAfterBad = M.applyTrust(trust0, MEMBERS4, compBad);
    ok(trustAfterGood.u_jp.rep === 66, "honouring raises reputation to 66, got " + trustAfterGood.u_jp.rep);
    ok(trustAfterBad.u_jp.rep === 38, "defecting drops reputation to 38, got " + trustAfterBad.u_jp.rep);
    ok(trustAfterBad.u_us.rep === 60, "a non-party to the breach is unaffected, got " + trustAfterBad.u_us.rep);
    /* finance makes a partner's tokens go further */
    const effWith = M.countryEfficiency(countries().JP, nodes4.find(n => n.id === "graphite_anode"), { finance: true });
    const effWithout = M.countryEfficiency(countries().JP, nodes4.find(n => n.id === "graphite_anode"));
    ok(effWith > effWithout, "finance raises the recipient's efficiency");
    /* honouring a commitment in a weak node shows up as a cost */
    const auObjs = M.assignObjectives("seed-test", "u_au", "AU");
    const auHonor = M.scoreCountry("u_au", Object.assign({}, cDeal, { alliance: aDeal, objectives: auObjs, compliance: compOk }));
    ok(auHonor.components.costs <= 0, "commitment costs are never positive, got " + auHonor.components.costs);

    /* applyAgreementsToPlan must respect the budget and the obligations */
    const forced = M.applyAgreementsToPlan({ cells: 8, packs: 4 }, "u_jp", [pact], nodes4, 10, countries().JP);
    const forcedTotal = Object.keys(forced).reduce((a, k) => a + forced[k], 0);
    ok(forced.graphite_anode >= 5, "adopting commitments funds the promised node, got " + forced.graphite_anode);
    ok(forcedTotal <= 10, "adopting commitments respects the budget, got " + forcedTotal);
    ok(!Object.keys(forced).some(k => !forced[k]), "no zero-token entries are left behind");

    /* REGRESSION: a "maintain N% of allied capacity" promise has to be fundable.
       It used to be ignored when adopting commitments, so bots and the "fund my
       commitments" button produced plans that breached deals they had accepted. */
    const maintainDeal = {
      id: "ag2", title: "Cathode floor", proposer: "u_kr", status: "accepted",
      participants: ["u_kr", "u_jp"],
      commitments: [{ party: "u_jp", type: "maintain", nodeId: "cathode_cam", amount: 35 }],
    };
    const funded = M.applyAgreementsToPlan({ cells: 10 }, "u_jp", [maintainDeal], nodes4, 10, countries().JP);
    const fundedPlans = plansOf({ u_jp: funded, u_us: {}, u_kr: {}, u_au: {} });
    const cFunded = ctxFor(MEMBERS4, fundedPlans, { agreements: [maintainDeal] });
    const aFunded = M.scoreAlliance(cFunded);
    const compMaintain = M.evaluateCompliance([maintainDeal], Object.assign({}, cFunded, { alliance: aFunded }));
    ok(funded.cathode_cam > 0, "a maintain promise is translated into tokens, got " + funded.cathode_cam);
    ok(compMaintain.honoredIds.length === 1,
      "the funded plan honours the maintain promise (share " +
      Math.round((aFunded.byId.cathode_cam.byUid.u_jp || 0) / aFunded.byId.cathode_cam.node.requirement * 100) + "% vs 35%)");
    /* and a plan that ignores it is still caught */
    const ignored = M.evaluateCompliance([maintainDeal], Object.assign({}, ctxFor(MEMBERS4, plansOf({ u_jp: { cells: 10 }, u_us: {}, u_kr: {}, u_au: {} }), { agreements: [maintainDeal] }),
      { alliance: M.scoreAlliance(ctxFor(MEMBERS4, plansOf({ u_jp: { cells: 10 }, u_us: {}, u_kr: {}, u_au: {} }), { agreements: [maintainDeal] })) }));
    ok(ignored.breachedIds.length === 1, "ignoring a maintain promise is still a breach");

    /* ======================================================================
       10. COORDINATOR PRIVACY
       The report must not leak any per-player plan.
       ====================================================================== */
    group("coordinator privacy");
    const repJson = JSON.stringify(repOver);
    ok(MEMBERS4.every(m => repJson.indexOf(m.uid) < 0), "the coordinator report contains no player uid");
    ok(repOver.nodes.every(n => typeof n.investors === "number" && n.investors >= 0), "it reports investor COUNTS only");
    ok(repOver.hints.every(h => Array.isArray(h.countries)), "capability hints reference countries, not plans");

    /* ======================================================================
       11. BENCHMARK, STRESS TEST AND PRICE OF NON-COORDINATION
       ====================================================================== */
    group("benchmark + stress tests");
    const bench = M.optimalBenchmark({ nodes: nodes4, members: MEMBERS4, countries: countries(), budget: 10, installed: {} });
    ok(bench.alliance.headline >= aAI.headline, `benchmark (${bench.alliance.headline}) >= AI baseline (${aAI.headline})`);
    ok(bench.alliance.headline >= aCoord.headline - 12, "benchmark is in the same league as a good hand-built plan");
    const stThin = M.runAllStressTests("batteries", ctxFor(MEMBERS4, thin));
    const stThick = M.runAllStressTests("batteries", ctxFor(MEMBERS4, thick));
    ok(stThick.overall > stThin.overall, `a complete chain survives stress better (${stThick.overall}% vs ${stThin.overall}%)`);
    ok(stThin.tests.length === 3, "three stress tests run, got " + stThin.tests.length);
    /* Same total graphite capacity, one supplier vs two: the single-sourced
       alliance takes the disruption haircut. (Sized so the haircut bites — at
       200% of requirement a 40% haircut is invisible.) */
    const singleThin = plansOf({ u_jp: { graphite_anode: 4 }, u_us: {}, u_kr: {}, u_au: {} });
    const dualThin = plansOf({ u_jp: { graphite_anode: 2 }, u_kr: { graphite_anode: 2 }, u_us: {}, u_au: {} });
    const exS = M.runStressTest("export_restrictions", ctxFor(MEMBERS4, singleThin));
    const exD = M.runStressTest("export_restrictions", ctxFor(MEMBERS4, dualThin));
    ok(exS.resilience < exD.resilience,
      `single-sourced capacity is more exposed to export restrictions (${exS.resilience}% vs ${exD.resilience}%)`);
    const price = M.priceOfNonCoordination(61, 78, 91);
    ok(price.gain === 17 && price.avoidedPct === 57, "price-of-non-coordination maths (gain 17, 57% of the gap), got " + price.gain + "/" + price.avoidedPct);
    const noGap = M.priceOfNonCoordination(70, 74, 68);
    ok(noGap.measurable === false && noGap.avoidedPct === null,
      "a benchmark that fails to beat the baseline reports no percentage instead of a fake 100%");

    /* REGRESSION: the "best achievable" benchmark must not be more brittle than
       what players build. It was, because the score counted a supplier the
       stress test then shut down — a plant-viability threshold used
       inconsistently in two places. One threshold (CFG.meaningfulShare) now
       governs supplier counting, sliver waste and stress shutdown alike. */
    const benchStress = M.runAllStressTests("batteries", ctxFor(MEMBERS4, bench.plans));
    const aiStress = M.runAllStressTests("batteries", ctxFor(MEMBERS4, aiPlans));
    const goodStress = M.runAllStressTests("batteries", ctxFor(MEMBERS4, coordinated));
    ok(benchStress.overall >= goodStress.overall,
      `benchmark resilience (${benchStress.overall}%) >= a good hand-built plan (${goodStress.overall}%)`);
    ok(goodStress.overall > aiStress.overall,
      `a coordinated plan is more resilient than the uncoordinated baseline (${goodStress.overall}% vs ${aiStress.overall}%)`);
    ok(bench.alliance.headline >= M.scoreAlliance(ctxFor(MEMBERS4, coordinated)).headline,
      "benchmark alliance score is an upper reference, not a lower one");
    /* REGRESSION: the stress test must count the inherited installed base.
       It rebuilds capacity plant-by-plant so shutdown rules can bite, and used
       to drop the un-owned installed base entirely — which made every round
       after the first look far more brittle than it was. */
    const installed = {}; nodes4.forEach(n => installed[n.id] = n.requirement * 0.4);
    const withBase = M.runAllStressTests("batteries", ctxFor(MEMBERS4, thin, { installed }));
    const withoutBase = M.runAllStressTests("batteries", ctxFor(MEMBERS4, thin));
    ok(withBase.overall > withoutBase.overall,
      `an inherited installed base raises resilience (${withBase.overall}% vs ${withoutBase.overall}%)`);
    ok(M.runStressTest("export_restrictions", ctxFor(MEMBERS4, plansOf({ u_us: {}, u_jp: {}, u_kr: {}, u_au: {} }), { installed })).resilience > 0,
      "a pure installed base still produces something under stress");
    /* the thresholds that must agree */
    const nCells = nodes4.find(n => n.id === "cells");
    ok(M.CFG.meaningfulShare * nCells.requirement < nCells.minViable,
      "a single viable plant is still smaller than a viable NODE (a node needs several)");

    /* ======================================================================
       12. DETERMINISM AND CONFIG HYGIENE
       ====================================================================== */
    group("determinism + config");
    ok(M.assignObjectives("L1", "u_us", "US").join() === M.assignObjectives("L1", "u_us", "US").join(),
      "objective assignment is deterministic per (league, player)");
    ok(M.assignObjectives("L1", "u_us", "US").length === 2, "two objectives per player");
    ok(new Set(M.assignObjectives("L1", "u_kr", "KR")).size === 2, "objectives are distinct");
    const wsum = Object.values(DATA.COORD_CONFIG.allianceWeights).reduce((a, b) => a + b, 0);
    ok(Math.abs(wsum - 1) < 1e-9, "alliance component weights sum to 1, got " + wsum);
    ok(M.shockForRound("batteries", 1) === null, "round 1 has no shock");
    ok(M.shockForRound("batteries", 2) === "graphite_controls", "round 2 shock is the graphite embargo");
    ok(M.shocksThrough("batteries", 3).length === 2, "shocks are cumulative across rounds");
    ok(DATA.getScenario("batteries").nodes.length === 9, "the battery scenario has 9 nodes");
    ok(DATA.getScenario("batteries").nodes.every(n => n.dependencies.every(d =>
      DATA.getScenario("batteries").nodes.some(x => x.id === d))), "every dependency points at a real node");

    say(`\n${pass} passed, ${fail} failed`);
    return { pass, fail, lines: out };
  }

  return { run, FIXTURE_COUNTRIES, MEMBERS4, ctxFor, plansOf };
});
