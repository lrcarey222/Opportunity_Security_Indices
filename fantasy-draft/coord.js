/* ============================================================================
   ALLIANCE ARCHITECT — multiplayer UI + phase loop
   ----------------------------------------------------------------------------
   Reuses the fantasy draft's infrastructure wholesale: NET adapters (net.js),
   the league create/join/presence flow (lobby.js), the country roster and
   home-turf synergies (data.js), the portrait + icon art (art.js) and the
   arcade theme (styles.css). Everything here is the coordination game on top.

   RESPONSIBILITIES
     • startGame()  — host builds the initial synchronized game state
     • onSnap()     — every client renders from the league snapshot
     • hostTick()   — the host fills in for bots and disconnected players
     • host phase advance — the ONLY place scores are computed and written, so
       every client displays identical numbers (no client-side divergence)

   PRIVACY (important, and deliberately not glossed over)
     Firebase Realtime Database hands every joined client the whole league node,
     so national plans written to coord/plans/<round>/<uid> are visible to a
     determined player who opens the console. They are *UI-private*: no screen
     ever renders another player's allocation before the coordinator phase, and
     the coordinator report is built from aggregates only. The path shape is
     chosen so a security rule (".read": "auth.uid === $uid") can make it
     genuinely private later, with a server/Cloud Function doing aggregation.
   ============================================================================ */
window.COORD_UI = (function () {
  "use strict";

  const M = () => window.COORD;                 // engine
  const D = () => window.COORD_DATA;            // scenario config
  const $c = (s) => document.querySelector(s);
  const esc = (s) => String(s == null ? "" : s).replace(/[&<>"']/g, c =>
    ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&#39;" }[c]));
  const pct = (v) => Math.round(v * 100);

  let ADAPTER = null, SNAP = null, busy = false;
  /* Local-only UI state. `alloc` is the source of truth for the plan you are
     editing until you submit/lock it, so a snapshot arriving mid-edit (someone
     else submitting, a bot answering a proposal) can never clobber your work. */
  const LOCAL = { alloc: null, allocKey: "", draft: null, builderOpen: false };

  /* ======================================================================
     STATE DERIVATION — snapshot → engine inputs
     ====================================================================== */
  const game = () => (SNAP && SNAP.coord) || null;
  const me = () => (ADAPTER ? ADAPTER.me : null);
  const isHost = () => !!(SNAP && ADAPTER && SNAP.host === ADAPTER.me);
  const seats = () => (game() && game().seats) || {};
  const order = () => (game() && game().order) || Object.keys(seats());
  const members = () => order().map(uid => {
    const s = seats()[uid] || {};
    return { uid, name: s.name || "Player", countryId: s.countryId || "US", bot: !!s.bot };
  });
  const countryOf = (uid) => ALLY_BY_ID[(seats()[uid] || {}).countryId] || ALLY_BY_ID.US;
  const nameOf = (uid) => (seats()[uid] || {}).name || "Player";
  const flagOf = (uid) => countryOf(uid).flag;
  const agreements = () => Object.values((game() && game().proposals) || {})
    .filter(p => p && p.status === "accepted")
    .map(p => Object.assign({}, p, { participants: p.participants || [], commitments: p.commitments || [] }));
  const proposals = () => Object.values((game() && game().proposals) || {})
    .sort((a, b) => (a.createdAt || 0) - (b.createdAt || 0));

  const shocks = () => M().shocksThrough(game().scenario, game().round);
  const nodesNow = () => M().resolveNodes(game().scenario, members().length, shocks());
  const budgetNow = () => M().budgetFor(shocks(), game().tokensPerRound);

  /** plans for a round: which = "submitted" | "final" */
  function plansFor(round, which) {
    const raw = ((game().plans || {})[String(round)]) || {};
    const out = {};
    members().forEach(m => {
      const p = raw[m.uid] || {};
      const alloc = (which === "final" && p.finalAlloc) ? p.finalAlloc : (p.alloc || {});
      out[m.uid] = { alloc: alloc || {}, submitted: !!p.submitted, locked: !!p.locked, auto: !!p.auto };
    });
    return out;
  }
  const myPlanRec = () => (((game().plans || {})[String(game().round)]) || {})[me()] || {};

  /** A full engine context for the current round. */
  function ctxFor(which, opts) {
    const o = opts || {};
    const mem = members();
    return {
      nodes: o.nodes || nodesNow(), members: mem, countries: ALLY_BY_ID,
      plans: o.plans || plansFor(game().round, which || "submitted"),
      agreements: o.agreements || agreements(),
      installed: game().installed || {},
      budget: budgetNow(), round: game().round,
      trust: game().trust || {},
      objectivesByUid: game().objectives || {},
    };
  }

  /* ======================================================================
     HOST: build the initial game
     ====================================================================== */
  const BOT_POOL = ["US", "JP", "KR", "AU", "DE", "CA", "FR", "UK", "IN", "MX", "SE", "FI", "PL", "NL", "IT"];

  function startGame(snap, adapter, cfg) {
    ADAPTER = adapter;
    const humans = Object.entries(snap.players || {});
    const taken = humans.map(([, p]) => p.allyId);
    const seatMap = {}, ord = [];
    humans.forEach(([uid, p]) => { seatMap[uid] = { name: p.name || "Player", countryId: p.allyId, bot: false }; ord.push(uid); });
    // fill the remaining seats with AI-run countries (deterministic pick order)
    let botN = 0;
    for (let i = 0; i < BOT_POOL.length && botN < (cfg.bots || 0); i++) {
      const cid = BOT_POOL[i];
      if (taken.indexOf(cid) >= 0) continue;
      const uid = "bot_" + cid;
      seatMap[uid] = { name: (ALLY_BY_ID[cid] ? ALLY_BY_ID[cid].name : cid) + " (AI)", countryId: cid, bot: true };
      taken.push(cid); ord.push(uid); botN++;
    }
    const objectives = {}, trust = {};
    ord.forEach(uid => {
      objectives[uid] = M().assignObjectives(snap.id + "|" + (cfg.scenario || "batteries"), uid, seatMap[uid].countryId);
      trust[uid] = { rep: D().COORD_CONFIG.trustStart, honored: 0, defected: 0 };
    });
    const g = {
      title: D().COORD_CONFIG.title,
      scenario: cfg.scenario || "batteries",
      rounds: cfg.rounds || D().COORD_CONFIG.roundsDefault,
      tokensPerRound: cfg.tokens || D().COORD_CONFIG.tokensPerRound,
      round: 1, phase: "planning",
      seats: seatMap, order: ord, objectives, trust,
      plans: {}, baseline: {}, results: {}, proposals: {}, installed: {},
      stress: null, createdAt: Date.now(),
    };
    adapter.startCoord(g, Object.assign({}, snap.settings || {}, {
      gameMode: "coordination", scenario: g.scenario, coordRounds: g.rounds, tokens: g.tokensPerRound, bots: botN,
    }));
  }

  /* ======================================================================
     SNAPSHOT ENTRY POINT
     ====================================================================== */
  const seated = () => !!(game() && game().seats && game().seats[me()]);

  function onSnap(snap, adapter) {
    SNAP = snap; ADAPTER = adapter;
    if (!snap.coord) return;
    showOnly("coordScreen");
    if (!seated()) { withFocus(paintSpectator); wire(); return; }   // arrived after the room opened
    const g = game();
    const key = g.round + ":" + (g.phase === "commitment" ? "final" : "plan");
    if (LOCAL.allocKey !== key) {
      LOCAL.allocKey = key;
      const rec = myPlanRec();
      LOCAL.alloc = Object.assign({}, (g.phase === "commitment" ? (rec.finalAlloc || rec.alloc) : rec.alloc) || {});
      if (g.phase === "commitment") {
        // start the final plan from your submitted plan, honouring your deals
        LOCAL.alloc = M().applyAgreementsToPlan(LOCAL.alloc, me(), agreements(), nodesNow(), budgetNow(), countryOf(me()));
      }
      LOCAL.builderOpen = false; LOCAL.draft = null;
    }
    render();
  }

  /* ======================================================================
     HOST WATCHDOG — bots and no-shows
     ====================================================================== */
  /* Called by lobby.js's host interval (~3s). Human no-shows are handled at
     phase advance instead, so the only standing job here is letting AI
     countries answer proposals. */
  function hostTick(snap, adapter) {
    SNAP = snap; ADAPTER = adapter;
    const g = game(); if (!g || busy) return;
    if (g.phase === "negotiation") botsRespondToProposals();
  }

  /** Bots answer any open proposal they're party to. */
  function botsRespondToProposals() {
    const g = game(); const nodes = nodesNow(); const budget = budgetNow();
    proposals().forEach(p => {
      if (p.status !== "open") return;
      (p.participants || []).forEach(uid => {
        const seat = seats()[uid];
        if (!seat || !seat.bot) return;
        if (p.responses && p.responses[uid]) return;
        const verdict = M().aiRespondToProposal(p, uid, countryOf(uid), nodes, budget, agreements());
        respond(p.id, verdict.accept ? "accept" : "reject", uid);
      });
    });
  }

  /* ======================================================================
     PLAYER ACTIONS
     ====================================================================== */
  const spent = (alloc) => Object.keys(alloc || {}).reduce((a, k) => a + Math.max(0, Number(alloc[k]) || 0), 0);

  function bump(nodeId, delta) {
    const g = game();
    if (g.phase !== "planning" && g.phase !== "commitment") return;
    const rec = myPlanRec();
    if (g.phase === "planning" && rec.submitted) { toast("Plan already submitted"); return; }
    if (g.phase === "commitment" && rec.locked) { toast("Final plan already locked"); return; }
    const cur = Math.max(0, Number(LOCAL.alloc[nodeId]) || 0);
    const next = Math.max(0, cur + delta);
    if (delta > 0 && spent(LOCAL.alloc) + delta > budgetNow()) { toast("No policy tokens left"); return; }
    if (next === 0) delete LOCAL.alloc[nodeId]; else LOCAL.alloc[nodeId] = next;
    render();
  }

  function adoptCommitments() {
    LOCAL.alloc = M().applyAgreementsToPlan(LOCAL.alloc, me(), agreements(), nodesNow(), budgetNow(), countryOf(me()));
    render(); toast("Commitments funded");
  }
  function clearAlloc() { LOCAL.alloc = {}; render(); }
  function suggestAlloc() {
    LOCAL.alloc = M().autoPlan(countryOf(me()), nodesNow(), budgetNow(), { uid: me(), agreements: agreements() });
    render(); toast("Drafted a national-interest plan — edit it");
  }

  async function submitPlan() {
    const g = game(), r = String(g.round);
    if (spent(LOCAL.alloc) === 0) { toast("Allocate at least one token"); return; }
    await ADAPTER.setCoordAt(`plans/${r}/${me()}`, { alloc: LOCAL.alloc, submitted: true, ts: Date.now() });
    toast("National plan submitted — it stays private");
  }
  async function unsubmitPlan() {
    const g = game(), r = String(g.round);
    const rec = myPlanRec();
    await ADAPTER.setCoordAt(`plans/${r}/${me()}`, { alloc: rec.alloc || {}, submitted: false, ts: Date.now() });
    toast("Plan reopened for editing");
  }
  async function lockPlan() {
    const g = game(), r = String(g.round), rec = myPlanRec();
    if (spent(LOCAL.alloc) === 0) { toast("Allocate at least one token"); return; }
    await ADAPTER.setCoordAt(`plans/${r}/${me()}`, {
      alloc: rec.alloc || {}, submitted: true, finalAlloc: LOCAL.alloc, locked: true, ts: Date.now(),
    });
    toast("Final plan locked");
  }
  async function unlockPlan() {
    const g = game(), r = String(g.round), rec = myPlanRec();
    await ADAPTER.setCoordAt(`plans/${r}/${me()}`, {
      alloc: rec.alloc || {}, submitted: true, finalAlloc: rec.finalAlloc || null, locked: false, ts: Date.now(),
    });
  }

  /* ---------- deal room ---------- */
  function newDraft() {
    LOCAL.draft = { title: "", participants: [me()], commitments: [{ party: me(), type: "invest", nodeId: nodesNow()[0].id, amount: 3 }] };
    LOCAL.builderOpen = true; render();
  }
  function draftPatch(fn) { fn(LOCAL.draft); render(); }

  async function sendProposal() {
    const d = LOCAL.draft; if (!d) return;
    if ((d.participants || []).length < 2) { toast("A deal needs at least two countries"); return; }
    const commitments = (d.commitments || []).filter(c => c.party && c.type && c.nodeId)
      .map(c => ({ party: c.party, type: c.type, nodeId: c.nodeId, amount: M().commitmentType(c.type).needsAmount ? Math.max(0, Number(c.amount) || 0) : null }));
    if (!commitments.length) { toast("Add at least one commitment"); return; }
    if (!commitments.every(c => d.participants.indexOf(c.party) >= 0)) { toast("Every commitment must belong to a participant"); return; }
    const id = "p" + Date.now().toString(36) + Math.random().toString(36).slice(2, 6);
    const responses = {}; responses[me()] = "accept";          // proposing is accepting
    const prop = {
      id, title: d.title || "Untitled agreement", proposer: me(), round: game().round,
      participants: d.participants.slice(), commitments, responses,
      status: (d.participants.length === 1) ? "accepted" : "open",
      counterOf: d.counterOf || null, createdAt: Date.now(),
    };
    await ADAPTER.setCoordAt(`proposals/${id}`, prop);
    LOCAL.builderOpen = false; LOCAL.draft = null;
    toast("Proposal sent to the deal room");
  }

  async function respond(pid, verdict, asUid) {
    const g = game(); const p = (g.proposals || {})[pid]; if (!p) return;
    const uid = asUid || me();
    await ADAPTER.setCoordAt(`proposals/${pid}/responses/${uid}`, verdict);
    const responses = Object.assign({}, p.responses || {}); responses[uid] = verdict;
    const parts = p.participants || [];
    let status = "open";
    if (parts.some(u => responses[u] === "reject")) status = "rejected";
    else if (parts.every(u => responses[u] === "accept")) status = "accepted";
    if (status !== p.status) await ADAPTER.setCoordAt(`proposals/${pid}/status`, status);
  }
  async function withdraw(pid) {
    await ADAPTER.setCoordAt(`proposals/${pid}/status`, "withdrawn");
    toast("Proposal withdrawn");
  }
  function counter(pid) {
    const p = (game().proposals || {})[pid]; if (!p) return;
    LOCAL.draft = {
      title: "Counter: " + (p.title || ""), participants: (p.participants || []).slice(),
      commitments: (p.commitments || []).map(c => Object.assign({}, c)), counterOf: pid,
    };
    LOCAL.builderOpen = true;
    respond(pid, "reject");
    render();
    toast("Countering — edit the terms and send");
  }

  /* ======================================================================
     HOST: PHASE ADVANCE (the only writer of scores)
     ====================================================================== */
  async function advance() {
    if (!isHost() || busy) return;
    busy = true;
    try {
      const g = game(), r = String(g.round);
      if (g.phase === "planning") {
        // use the map fillMissingPlans returns — a Firebase write may not have
        // round-tripped through the snapshot listener yet
        const plans = await fillMissingPlans("submitted");
        const ctx = ctxFor("submitted", { plans });
        // Same two-pass treatment computeResults uses, so "before" and "after"
        // are measured the same way. From round 2 there are standing agreements,
        // and a submitted plan that ignores one is already a breach — scoring
        // the baseline without that would make every later round look like a
        // loss for whoever was in breach, even in a round with no new deals.
        const pass1 = M().scoreAlliance(ctx);
        const compliance = M().evaluateCompliance(agreements(), Object.assign({}, ctx, { alliance: pass1 }));
        const alliance = M().scoreAlliance(Object.assign({}, ctx, { honoredIds: compliance.honoredIds }));
        const national = M().scoreAllCountries(Object.assign({}, ctx, {
          alliance, honoredIds: compliance.honoredIds, compliance,
        }));
        await ADAPTER.setCoordAt(`baseline/${r}`, {
          headline: alliance.headline,
          components: roundComps(alliance.components),
          summary: alliance.summary,
          national: mapVals(national, v => v.total),
          nodePct: mapNodePct(alliance),
        });
        await ADAPTER.setCoordAt("phase", "coordinator");
      } else if (g.phase === "coordinator") {
        await ADAPTER.setCoordAt("phase", "negotiation");
      } else if (g.phase === "negotiation") {
        await ADAPTER.setCoordAt("phase", "commitment");
      } else if (g.phase === "commitment") {
        const plans = await fillMissingPlans("final");
        await computeResults(plans);
        await ADAPTER.setCoordAt("phase", "results");
      } else if (g.phase === "results") {
        if (g.round >= g.rounds) { await runStress(); await ADAPTER.setCoordAt("phase", "stress"); }
        else await nextRound();
      }
    } finally { busy = false; }
  }

  const mapVals = (o, f) => { const x = {}; Object.keys(o).forEach(k => x[k] = f(o[k])); return x; };
  const roundComps = (c) => mapVals(c, v => Math.round(v * 10) / 10);
  const mapNodePct = (a) => { const x = {}; a.nodes.forEach(s => x[s.nodeId] = pct(s.pct)); return x; };

  /**
   * fillMissingPlans — bots and no-shows get a plan so a round can always
   * close. Returns the complete plans map for the phase (engine-shaped), so
   * callers never have to re-read a snapshot that may not have arrived yet.
   */
  async function fillMissingPlans(which) {
    const g = game(), r = String(g.round);
    const nodes = nodesNow(), budget = budgetNow();
    const out = {};
    for (const m of members()) {
      const rec = ((g.plans || {})[r] || {})[m.uid] || {};
      if (which === "submitted") {
        if (rec.submitted) { out[m.uid] = { alloc: rec.alloc || {}, submitted: true }; continue; }
        const alloc = (m.bot || !rec.alloc || !spent(rec.alloc))
          ? M().autoPlan(countryOf(m.uid), nodes, budget, { uid: m.uid, agreements: agreements() })
          : rec.alloc;
        await ADAPTER.setCoordAt(`plans/${r}/${m.uid}`, { alloc, submitted: true, auto: true, ts: Date.now() });
        out[m.uid] = { alloc, submitted: true, auto: true };
      } else {
        if (rec.locked) { out[m.uid] = { alloc: rec.finalAlloc || rec.alloc || {}, locked: true }; continue; }
        const start = rec.finalAlloc || rec.alloc ||
          M().autoPlan(countryOf(m.uid), nodes, budget, { uid: m.uid });
        const finalAlloc = M().applyAgreementsToPlan(start, m.uid, agreements(), nodes, budget, countryOf(m.uid));
        await ADAPTER.setCoordAt(`plans/${r}/${m.uid}`, {
          alloc: rec.alloc || start, submitted: true, finalAlloc, locked: true, auto: true, ts: Date.now(),
        });
        out[m.uid] = { alloc: finalAlloc, locked: true, auto: true };
      }
    }
    return out;
  }

  /** Score the round: compliance → trust → alliance/national → stability. */
  async function computeResults(finalPlansIn) {
    const g = game(), r = String(g.round);
    const finalPlans = finalPlansIn || plansFor(g.round, "final");
    const ags = agreements();
    const ctxAll = ctxFor("final", { plans: finalPlans, agreements: ags });

    // pass 1: capacity with all accepted deals' effects, to check compliance
    const pass1 = M().scoreAlliance(ctxAll);
    const compliance = M().evaluateCompliance(ags, Object.assign({}, ctxAll, { alliance: pass1 }));
    // pass 2: only honoured deals confer benefits
    const alliance = M().scoreAlliance(Object.assign({}, ctxAll, { honoredIds: compliance.honoredIds }));
    const trust = M().applyTrust(g.trust || {}, members(), compliance);
    const national = M().scoreAllCountries(Object.assign({}, ctxAll, {
      alliance, honoredIds: compliance.honoredIds, compliance, trust: g.trust || {},
    }));

    const base = (g.baseline || {})[r] || { headline: alliance.headline, national: {}, components: {}, summary: {} };
    const baseNat = {}; members().forEach(m => baseNat[m.uid] = { total: (base.national || {})[m.uid] || 0 });
    const stability = M().coalitionStability(baseNat, national, members());

    const nodeRows = alliance.nodes.map(s => ({
      nodeId: s.nodeId, short: s.node.short, beforePct: (base.nodePct || {})[s.nodeId] || 0,
      afterPct: pct(s.pct), status: M().nodeStatus(s).key,
      suppliers: s.supplierCountries.length, target: s.node.strategicRedundancyTarget,
      byUid: mapVals(s.byUid, v => Math.round(v)),
      tokensByUid: s.tokensByUid,
    }));

    await ADAPTER.setCoordAt(`results/${r}`, {
      before: { headline: base.headline, components: base.components || {}, summary: base.summary || {} },
      after: { headline: alliance.headline, components: roundComps(alliance.components), summary: alliance.summary },
      gain: alliance.headline - base.headline,
      national: mapVals(national, v => ({
        total: v.total, components: v.components, avgEfficiency: v.avgEfficiency,
        objectives: v.objectives.map(o => ({ label: o.label, met: o.met, progress: o.progress })),
        before: (base.national || {})[v.uid] || 0, delta: v.total - ((base.national || {})[v.uid] || 0),
        leads: v.leads || [],
      })),
      stability: { score: stability.score, verdict: stability.verdict, rows: stability.rows, worst: stability.worst },
      compliance: {
        honored: compliance.honoredIds, breached: compliance.breachedIds,
        defectors: mapVals(compliance.defectorsByUid, list => list.map(d => ({
          agreementId: d.agreementId, type: d.commitment.type, nodeId: d.commitment.nodeId,
          promised: d.commitment.amount, actual: d.actual,
        }))),
        results: compliance.results.map(x => ({ agreementId: x.agreementId, title: x.title, honored: x.honored })),
      },
      nodes: nodeRows,
      trust,
    });
    await ADAPTER.setCoordAt("trust", trust);
  }

  /** Carry the installed base forward, apply the next shock, reopen planning. */
  async function nextRound() {
    const g = game(), r = String(g.round);
    const finalPlans = plansFor(g.round, "final");
    const alliance = M().scoreAlliance(ctxFor("final", { plans: finalPlans }));
    await ADAPTER.updateCoord({
      installed: M().installedFrom(alliance),
      round: g.round + 1,
      phase: "planning",
    });
  }

  /**
   * runStress — the end-game comparison.
   *   independent : round-1 submitted plans (never negotiated, never adapted)
   *   negotiated  : the alliance's final locked plans
   *   benchmark   : the greedy optimiser's allocation
   * All three are evaluated against the SAME final-round conditions (including
   * every shock in force), which is exactly the point: unadapted independent
   * strategies meet the world as it ended up, not as it was when they were made.
   */
  async function runStress() {
    const g = game();
    const nodes = nodesNow();
    const indPlans = plansFor(1, "submitted");
    const finPlans = plansFor(g.round, "final");
    const cInd = ctxFor("submitted", { nodes, plans: indPlans, agreements: [] });
    const cFin = ctxFor("final", { nodes, plans: finPlans });
    const bench = M().optimalBenchmark({ nodes, members: members(), countries: ALLY_BY_ID, budget: budgetNow(), installed: g.installed || {} });
    const cBen = ctxFor("final", { nodes, plans: bench.plans, agreements: [] });

    const aInd = M().scoreAlliance(cInd), aFin = M().scoreAlliance(cFin);
    const sInd = M().runAllStressTests(g.scenario, cInd);
    const sFin = M().runAllStressTests(g.scenario, cFin);
    const sBen = M().runAllStressTests(g.scenario, cBen);
    const price = M().priceOfNonCoordination(aInd.headline, aFin.headline, bench.alliance.headline);

    await ADAPTER.setCoordAt("stress", {
      independent: { headline: aInd.headline, resilience: sInd.overall, tests: sInd.tests },
      negotiated: { headline: aFin.headline, resilience: sFin.overall, tests: sFin.tests },
      benchmark: { headline: bench.alliance.headline, resilience: sBen.overall, tests: sBen.tests },
      price,
      resiliencePrice: M().priceOfNonCoordination(sInd.overall, sFin.overall, sBen.overall),
    });
  }

  /* ======================================================================
     RENDER
     ====================================================================== */
  function withFocus(fn) {
    const a = document.activeElement;
    const id = a && a.id ? a.id : null;
    const pos = a && a.selectionStart != null ? a.selectionStart : null;
    fn();
    if (id) {
      const el = document.getElementById(id);
      if (el) { el.focus(); try { if (pos != null && el.setSelectionRange) el.setSelectionRange(pos, pos); } catch (e) {} }
    }
  }

  function render() { withFocus(seated() ? paint : paintSpectator); wire(); }

  /**
   * paintSpectator — the seats were fixed when the host opened the room, so a
   * player who arrives later has no country to run. Show them the public state
   * rather than a broken planning screen with someone else's defaults.
   */
  function paintSpectator() {
    const g = game();
    const res = (g.results || {})[String(g.round)];
    const base = (g.baseline || {})[String(g.round)];
    $c("#coordScreen").innerHTML = header() + seatStrip() + `<div class="coord-body">
      <div class="privacy-note">You joined after this room opened, so there is no country left for you to run.
        Seats are fixed when the host starts the game. You can watch this round, or start your own room from the
        home screen.</div>
      ${base ? `<div class="baseline-hero"><div class="bh-main">
        <div class="bh-tag">Uncoordinated alliance score · round ${g.round}</div>
        <div class="bh-num">${base.headline}<span>/100</span></div>
        ${res ? `<div class="bh-sub">After coordination: <b>${res.after.headline}</b> (${res.gain >= 0 ? "+" : ""}${res.gain}).</div>` : ""}
      </div><div class="bh-comps">${componentBars((res ? res.after.components : base.components) || {})}</div></div>` : ""}
    </div>` + `<div class="coord-foot"><span class="hint">Spectating.</span>
      <span class="spacer"></span><button class="btn ghost" id="coordLeave">Leave room</button></div>`;
  }

  function paint() {
    const g = game(); if (!g) return;
    const phase = g.phase;
    const body =
      phase === "planning" ? planningView() :
      phase === "coordinator" ? coordinatorView() :
      phase === "negotiation" ? negotiationView() :
      phase === "commitment" ? commitmentView() :
      phase === "results" ? resultsView() :
      phase === "stress" ? stressView() : "";
    $c("#coordScreen").innerHTML = header() + seatStrip() + `<div class="coord-body">${body}</div>` + footerBar();
  }

  /* ---------- header: round, phase strip, shock ---------- */
  function header() {
    const g = game();
    const scen = D().getScenario(g.scenario);
    const shockId = M().shockForRound(g.scenario, g.round);
    const shock = shockId ? D().getShock(shockId) : null;
    const phases = M().PHASES;
    const cur = g.phase === "stress" ? 5 : M().phaseIndex(g.phase);
    const info = phases[Math.min(cur, phases.length - 1)];
    return `
      <div class="hero coord-hero">
        <div class="kicker">${esc(scen.name)} · Round ${g.round} of ${g.rounds} · ${members().length} countries</div>
        <div class="player-select">${esc(g.title || "Alliance Architect")}</div>
        <div class="phase-strip">
          ${phases.map((p, i) => `<div class="ph ${i === cur ? "on" : i < cur ? "done" : ""}">
            <span class="ph-n">${p.n}</span><span class="ph-name">${p.name}</span></div>`).join("<i class='ph-arrow'>›</i>")}
          ${g.phase === "stress" ? `<i class='ph-arrow'>›</i><div class="ph on"><span class="ph-n">★</span><span class="ph-name">Stress Test</span></div>` : ""}
        </div>
        <p class="lede">${g.phase === "stress"
          ? "The alliance's final industrial structure meets three shocks it did not choose."
          : esc(info.blurb)}</p>
        ${shock && (g.phase !== "stress") ? `<div class="shock-card">
          <div class="sc-tag">${esc(shock.kicker)} · in force from round ${g.round}</div>
          <div class="sc-title">⚡ ${esc(shock.title)}</div>
          <div class="sc-blurb">${esc(shock.blurb)}</div>
          <div class="sc-teach">${esc(shock.teaches || "")}</div>
        </div>` : ""}
      </div>`;
  }

  /* ---------- seat strip: who's in, and are they done ---------- */
  function seatStrip() {
    const g = game();
    const plans = plansFor(g.round, "final");
    const res = (g.results || {})[String(g.round)];
    return `<div class="coord-seats">${members().map(m => {
      const c = countryOf(m.uid);
      const rec = ((g.plans || {})[String(g.round)] || {})[m.uid] || {};
      let state = "", cls = "";
      if (g.phase === "planning") { state = rec.submitted ? "plan submitted" : "planning…"; cls = rec.submitted ? "done" : "wait"; }
      else if (g.phase === "commitment") { state = rec.locked ? "locked" : "deciding…"; cls = rec.locked ? "done" : "wait"; }
      else if (g.phase === "negotiation") {
        const open = proposals().filter(p => p.status === "open" && (p.participants || []).indexOf(m.uid) >= 0 && !(p.responses || {})[m.uid]).length;
        state = open ? `${open} to answer` : "no open deals"; cls = open ? "wait" : "done";
      } else if (res && res.national && res.national[m.uid]) {
        const n = res.national[m.uid];
        state = `${n.total} (${n.delta >= 0 ? "+" : ""}${n.delta})`; cls = n.delta >= 0 ? "done" : "bad";
      } else if (g.phase === "coordinator") {
        // national baselines stay private here — showing them would leak how
        // well each member's plan did before anyone has negotiated
        state = rec.submitted ? "plan in" : "no plan"; cls = rec.submitted ? "done" : "wait";
      } else state = c.name;
      const rep = ((g.trust || {})[m.uid] || {}).rep;
      return `<div class="cseat ${m.uid === me() ? "you" : ""}" style="--cc1:${c.c1};--cc2:${c.c2}">
        <div class="cs-flag">${c.flag}</div>
        <div class="cs-main">
          <div class="cs-name">${esc(m.name)}${m.bot ? " <span class='cs-bot'>AI</span>" : ""}${m.uid === me() ? " <span class='cs-you'>YOU</span>" : ""}</div>
          <div class="cs-state ${cls}">${esc(state)}</div>
        </div>
        ${rep != null ? `<div class="cs-trust" title="Trust / reputation">${rep}</div>` : ""}
      </div>`;
    }).join("")}</div>`;
  }

  /* ---------- footer: host controls ---------- */
  function footerBar() {
    const g = game();
    const labels = {
      planning: "Close planning → Coordinator Report ▶",
      coordinator: "Open the Deal Room ▶",
      negotiation: "Close negotiation → Commitment ▶",
      commitment: "Lock the round → Results ▶",
      results: g.round >= g.rounds ? "Run the Alliance Stress Test ★" : "Next round (with shock) ▶",
      stress: "",
    };
    const waiting = pendingCount();
    return `<div class="coord-foot">
      ${isHost() && labels[g.phase]
        ? `<button class="btn red" id="coordAdvance">${labels[g.phase]}</button>
           ${waiting ? `<span class="hint">${waiting} member(s) haven't finished — advancing plans for them automatically.</span>` : ""}`
        : `<span class="hint">${g.phase === "stress" ? "" : "The host advances the phase when everyone is ready."}</span>`}
      <span class="spacer"></span>
      <button class="btn ghost" id="coordLeave">Leave room</button>
    </div>`;
  }
  function pendingCount() {
    const g = game();
    if (g.phase !== "planning" && g.phase !== "commitment") return 0;
    return members().filter(m => {
      const rec = ((g.plans || {})[String(g.round)] || {})[m.uid] || {};
      return g.phase === "planning" ? !rec.submitted : !rec.locked;
    }).length;
  }

  /* ======================================================================
     PHASE 1 & 4 — allocation UI
     ====================================================================== */
  function effLabel(e) {
    return e >= 1.45 ? ["world-class", "s"] : e >= 1.25 ? ["strong", "a"] : e >= 1.05 ? ["capable", "b"] : e >= 0.9 ? ["weak", "c"] : ["poor", "d"];
  }

  function allocRows(nodes, opts) {
    const o = opts || {};
    const country = countryOf(me());
    const budget = budgetNow();
    const commits = M().commitmentsFor(me(), agreements(), { plans: { [me()]: { alloc: LOCAL.alloc } }, alliance: null });
    const byNode = {};
    commits.forEach(c => { (byNode[c.commitment.nodeId] = byNode[c.commitment.nodeId] || []).push(c); });
    return nodes.map(n => {
      const t = Math.max(0, Number(LOCAL.alloc[n.id]) || 0);
      const eff = M().countryEfficiency(country, n);
      const [lbl, cls] = effLabel(eff);
      const myCap = t * n.capacityPerToken * eff;
      const myShare = pct(myCap / n.requirement);
      const cat = CATEGORIES[n.cat];
      const deps = (n.dependencies || []).map(d => {
        const dn = nodes.find(x => x.id === d);
        return `<span class="dep-chip">needs ${esc(dn ? dn.short : d)}</span>`;
      }).join("");
      const ob = (byNode[n.id] || []).map(c => {
        const tt = M().commitmentType(c.commitment.type);
        const undecided = c.commitment.type === "lead";
        return `<span class="ob-chip ${undecided ? "" : c.met ? "ok" : "bad"}">${esc(tt.label)}${c.commitment.amount != null ? " " + c.commitment.amount : ""} ${undecided ? "•" : c.met ? "✓" : "✗"}</span>`;
      }).join("");
      return `<div class="alloc-row ${t ? "has" : ""}" style="--cc1:${cat.c1};--cc2:${cat.c2}">
        ${iconSVG(n.glyph, cat.c1, cat.c2, 44)}
        <div class="ar-main">
          <div class="ar-name">${esc(n.name)}</div>
          <div class="ar-meta">
            <span class="eff ${cls}" title="Capacity per policy token, from your home-turf synergies and node affinity">×${eff.toFixed(2)} · ${lbl}</span>
            <span class="ar-req">requirement ${Math.round(n.requirement)}</span>
            <span class="ar-req">min viable ${Math.round(n.minViable)}</span>
            <span class="ar-req" title="How many allied suppliers the alliance wants here">wants ${n.strategicRedundancyTarget} suppliers</span>
            ${n.chokepoint ? `<span class="ar-choke">CHOKEPOINT · ${pct(n.chinaExposure)}% non-allied today</span>` : ""}
          </div>
          <div class="ar-deps">${deps}${ob}</div>
        </div>
        <div class="ar-mine">${t ? `you build <b>${myShare}%</b>` : `<span class="ar-none">not in your plan</span>`}</div>
        <div class="stepper ${o.readonly ? "off" : ""}">
          <button class="st-btn" data-bump="-1" data-node="${n.id}" ${o.readonly || !t ? "disabled" : ""}>−</button>
          <span class="st-n">${t}</span>
          <button class="st-btn" data-bump="1" data-node="${n.id}" ${o.readonly || spent(LOCAL.alloc) >= budget ? "disabled" : ""}>+</button>
        </div>
      </div>`;
    }).join("");
  }

  /**
   * soloAlliance — scoring context containing ONLY your own plan.
   * Used for every panel shown during planning/commitment. Checking your
   * objectives against everyone's live plans would leak exactly what the
   * coordinator is careful not to publish (who leads which node), so panels
   * are computed against your plan alone and labelled as such. The real check
   * happens once, in computeResults, on the full alliance.
   */
  function soloAlliance() {
    const empty = {}; members().forEach(m => empty[m.uid] = { alloc: {} });
    empty[me()] = { alloc: LOCAL.alloc || {} };
    return M().scoreAlliance(ctxFor("submitted", { plans: empty, agreements: agreements() }));
  }

  function objectivesPanel() {
    const g = game();
    const ids = (g.objectives || {})[me()] || [];
    const nodes = nodesNow();
    const objs = M().checkObjectives(ids, { uid: me(), country: countryOf(me()), alliance: soloAlliance(), nodes });
    return `<div class="panel"><h4>🎯 Your private objectives <span class="panel-sub">only you see these</span></h4>
      <div class="body">${objs.map(o => `<div class="obj ${o.met ? "met" : ""}">
        <div class="obj-h">${o.met ? "✓" : "○"} ${esc(o.label)}</div>
        <div class="obj-d">${esc(o.detail)}</div>
        <div class="obj-w">“${esc(o.why)}”</div>
        <div class="obj-p">${esc(o.progress || "")}</div>
      </div>`).join("") || `<div class="hint">No objectives assigned.</div>`}
      <div class="hint" style="margin-top:8px">Each completed objective is worth ${D().COORD_CONFIG.objectivePoints} national points. They are
        deliberately in tension with what the alliance needs. <b>Checked here against your plan alone</b> —
        anything that depends on the others (like being the alliance's lead supplier) is settled when the round closes.</div></div></div>`;
  }

  function budgetPanel(actionsHtml) {
    const budget = budgetNow(), used = spent(LOCAL.alloc);
    return `<div class="panel budget-panel"><h4>🪙 Policy tokens</h4><div class="body">
      <div class="budget-num"><b>${budget - used}</b> <span>of ${budget} left</span></div>
      <div class="budget-bar"><i style="width:${pct(used / budget)}%"></i></div>
      <div class="hint">One token = one unit of national industrial-policy effort this round.</div>
      ${actionsHtml || ""}
    </div></div>`;
  }

  function planningView() {
    const nodes = nodesNow(), rec = myPlanRec(), submitted = !!rec.submitted;
    return `<div class="coord-grid">
      <div>
        <div class="section-title"><h3>Your national plan</h3><div class="rule"></div>
          <span class="hint">Private until the coordinator aggregates it</span></div>
        <div class="privacy-note">🔒 Nobody sees your allocation. The coordinator will only publish
          alliance-wide totals and the number of countries investing in each node.</div>
        <div class="alloc-list">${allocRows(nodes, { readonly: submitted })}</div>
      </div>
      <aside class="rail">
        ${budgetPanel(`<div class="bp-actions">
          ${submitted
            ? `<button class="btn gold" id="unsubmit">Reopen my plan</button>`
            : `<button class="btn red" id="submitPlan">Submit national plan ▶</button>
               <button class="btn ghost" id="suggest">Draft for me</button>
               <button class="btn ghost" id="clearAlloc">Clear</button>`}
        </div>
        ${submitted ? `<div class="ok-note">✓ Submitted. Waiting for the other countries…</div>` : ""}`)}
        ${objectivesPanel()}
        <div class="panel"><h4>📐 How this is scored</h4><div class="body hint">
          Capacity from a token = <b>tokens × node capacity-per-token × your efficiency</b>.
          Your efficiency comes from the same home-turf synergies the Fantasy Draft uses, plus a
          node-level affinity. Building where you're weak wastes allied capital — which is
          exactly what a negotiation can fix.
        </div></div>
      </aside>
    </div>`;
  }

  function commitmentView() {
    const nodes = nodesNow(), rec = myPlanRec(), locked = !!rec.locked;
    // solo context: "maintain" and "invest" are checks on your own capacity, so
    // they resolve correctly here. "lead" depends on the others and is shown as
    // undecided rather than leaking their plans.
    const commits = M().commitmentsFor(me(), agreements(), {
      plans: { [me()]: { alloc: LOCAL.alloc } }, alliance: soloAlliance(),
    }).map(c => c.commitment.type === "lead" ? Object.assign({}, c, { met: null }) : c);
    const decidable = commits.filter(c => c.met !== null);
    const kept = decidable.filter(c => c.met).length;
    const broken = decidable.filter(c => !c.met && c.breachable);
    return `<div class="coord-grid">
      <div>
        <div class="section-title"><h3>Lock your final plan</h3><div class="rule"></div>
          <span class="hint">Accepted agreements are obligations now</span></div>
        ${commits.length ? `<div class="commit-check ${broken.length ? "bad" : "ok"}">
          <b>${kept} of ${decidable.length}</b> commitments honoured by this plan${commits.length > decidable.length
            ? ` (${commits.length - decidable.length} lead-supplier promise${commits.length - decidable.length === 1 ? "" : "s"} settled when the round closes)` : ""}.
          ${broken.length ? `Breaking a promise costs <b>${D().COORD_CONFIG.trustDefectLoss} trust</b>,
            voids that agreement's benefits for everyone in it, and hurts the alliance score —
            but you keep the tokens. Your call.` : "Everyone in your deals gets what they were promised."}
        </div>` : `<div class="privacy-note">You have no binding commitments this round.</div>`}
        <div class="alloc-list">${allocRows(nodes, { readonly: locked })}</div>
      </div>
      <aside class="rail">
        ${budgetPanel(`<div class="bp-actions">
          ${locked
            ? `<button class="btn gold" id="unlock">Reopen final plan</button>`
            : `<button class="btn red" id="lockPlan">Lock final plan ▶</button>
               ${commits.length ? `<button class="btn ghost" id="adopt">Fund my commitments</button>` : ""}`}
        </div>${locked ? `<div class="ok-note">✓ Locked. Waiting for the rest of the alliance…</div>` : ""}`)}
        ${commits.length ? `<div class="panel"><h4>🤝 Your commitments</h4><div class="body">
          ${commits.map(c => {
            const t = M().commitmentType(c.commitment.type);
            const n = nodes.find(x => x.id === c.commitment.nodeId);
            return `<div class="obj ${c.met === null ? "" : c.met ? "met" : "broken"}">
              <div class="obj-h">${c.met === null ? "•" : c.met ? "✓" : "✗"} ${esc(t.label)}${c.commitment.amount != null ? " · " + c.commitment.amount : ""}</div>
              <div class="obj-d">${esc(n ? n.name : c.commitment.nodeId)} — from “${esc(c.title)}”</div>
              <div class="obj-p">you have ${c.tokens} token(s) there</div>
            </div>`;
          }).join("")}
        </div></div>` : ""}
        ${objectivesPanel()}
      </aside>
    </div>`;
  }

  /* ======================================================================
     PHASE 2 — coordinator report
     ====================================================================== */
  const TONE = { crit: "crit", warn: "warn", ok: "ok", good: "good" };

  function capacityBar(row, node) {
    const scale = Math.max(160, row.pct + 20);      // % axis
    const at = (v) => Math.min(100, (v / scale) * 100);
    return `<div class="ncap">
      <i class="ncap-fill ${TONE[row.tone]}" style="width:${at(row.pct)}%"></i>
      <span class="ncap-mk minv" style="left:${at(pct(node.minViable / node.requirement))}%" title="minimum viable scale"></span>
      <span class="ncap-mk req" style="left:${at(100)}%" title="alliance requirement (100%)"></span>
      <span class="ncap-mk maxu" style="left:${at(pct(node.maxUseful / node.requirement))}%" title="maximum useful capacity"></span>
    </div>`;
  }

  function chainMap(report, nodes) {
    const stages = {};
    nodes.forEach(n => { (stages[n.stage] = stages[n.stage] || []).push(n); });
    const byId = {}; report.nodes.forEach(r => byId[r.nodeId] = r);
    return `<div class="chainmap">${Object.keys(stages).sort().map((st, i) => `
      ${i ? `<div class="cm-arrow">→</div>` : ""}
      <div class="cm-stage">
        ${stages[st].map(n => {
          const r = byId[n.id];
          return `<div class="cm-node ${TONE[r.tone]}" title="${esc(n.name)} — ${r.pct}% of requirement, ${r.investors} investing">
            <div class="cm-pct">${r.pct}%</div>
            <div class="cm-name">${esc(n.short)}</div>
            <div class="cm-sup">${r.suppliers}/${r.redundancyTarget} suppliers</div>
          </div>`;
        }).join("")}
      </div>`).join("")}</div>`;
  }

  function componentBars(components, compare) {
    const W = D().COORD_CONFIG.allianceWeights;
    const names = {
      completeness: "Supply-chain completeness", scale: "Scale efficiency", resilience: "Resilience",
      duplication: "Duplication discipline", strategic: "Strategic coverage", complementarity: "Complementarity",
    };
    return `<div class="comps">${Object.keys(names).map(k => {
      const v = components[k] || 0;
      const d = compare ? v - (compare[k] || 0) : null;
      return `<div class="comp">
        <div class="comp-h"><span>${names[k]}</span>
          <b>${Math.round(v)}</b>${d != null ? `<i class="${d >= 0 ? "up" : "down"}">${d >= 0 ? "+" : ""}${Math.round(d)}</i>` : ""}
          <span class="comp-w">×${W[k]}</span></div>
        <div class="comp-bar"><i style="width:${Math.round(v)}%"></i>${compare ? `<span class="ghost-mk" style="left:${Math.round(compare[k] || 0)}%"></span>` : ""}</div>
      </div>`;
    }).join("")}</div>`;
  }

  function nationalBaselineCard() {
    const g = game();
    const base = (g.baseline || {})[String(g.round)];
    if (!base) return "";
    const mine = (base.national || {})[me()];
    const c = countryOf(me());
    return `<div class="panel"><h4>${c.flag} Your national baseline</h4><div class="body">
      <div class="nat-num">${mine == null ? "—" : mine}<span>/100</span></div>
      <div class="hint">Your score if the alliance stopped here. Watch what negotiation does to it —
        and to everyone else's. A bargain that lifts the alliance by crushing one member is not stable.</div>
    </div></div>`;
  }

  function coordinatorView() {
    const g = game();
    const nodes = nodesNow();
    const ctx = ctxFor("submitted");
    const alliance = M().scoreAlliance(ctx);
    const report = M().generateCoordinatorReport(Object.assign({}, ctx, { alliance }));
    const base = (g.baseline || {})[String(g.round)] || { headline: alliance.headline };
    const sev = { 3: "crit", 2: "warn", 1: "ok" };
    return `
      <div class="section-title"><h3>Coordinator report</h3><div class="rule"></div>
        <span class="hint">Aggregated from ${report.totals.membersReporting} national plans · ${report.totals.tokensPlanned} tokens committed</span></div>

      <div class="baseline-hero">
        <div class="bh-main">
          <div class="bh-tag">Uncoordinated alliance score</div>
          <div class="bh-num">${base.headline}<span>/100</span></div>
          <div class="bh-sub">This is what ${report.totals.membersReporting} individually sensible national strategies add up to.
            It is the benchmark every deal you strike will be measured against.</div>
          <div class="bh-counters">
            <span class="bhc crit">${alliance.summary.criticalShortfalls} critical shortfall${alliance.summary.criticalShortfalls === 1 ? "" : "s"}</span>
            <span class="bhc warn">${alliance.summary.duplications} duplicated node${alliance.summary.duplications === 1 ? "" : "s"}</span>
            <span class="bhc warn">${alliance.summary.singleSource} single-source dependenc${alliance.summary.singleSource === 1 ? "y" : "ies"}</span>
            <span class="bhc ${alliance.summary.chainBreaks ? "crit" : "ok"}">${alliance.summary.chainBreaks} chain break${alliance.summary.chainBreaks === 1 ? "" : "s"}</span>
            <span class="bhc ${alliance.summary.chainComplete ? "good" : "crit"}">chain ${alliance.summary.chainComplete ? "complete" : "incomplete"}</span>
          </div>
        </div>
        <div class="bh-comps">${componentBars(alliance.components)}</div>
      </div>

      <div class="section-title"><h3>Supply-chain map</h3><div class="rule"></div>
        <span class="hint">Left to right: dirt to recycling. Colour = status, not ownership.</span></div>
      ${chainMap(report, nodes)}

      <div class="coord-grid">
        <div>
          <div class="section-title"><h3>Node by node</h3><div class="rule"></div>
            <span class="hint">Counts only — no plan is revealed</span></div>
          <div class="noderows">${report.nodes.map(r => {
            const n = nodes.find(x => x.id === r.nodeId);
            return `<div class="noderow ${TONE[r.tone]}">
              <div class="nr-top">
                ${iconSVG(r.glyph, CATEGORIES[r.cat].c1, CATEGORIES[r.cat].c2, 38)}
                <div class="nr-name">${esc(r.name)}<span class="nr-sub">${r.investors} countr${r.investors === 1 ? "y" : "ies"} investing · wants ${r.redundancyTarget} suppliers</span></div>
                <div class="nr-pct">${r.pct}%<span>of requirement</span></div>
                <div class="nr-status ${TONE[r.tone]}">${esc(r.statusLabel)}</div>
              </div>
              ${capacityBar(r, n)}
              ${r.flags.length ? `<div class="nr-flags">${r.flags.map(f => `<span class="flag">${esc(f)}</span>`).join("")}</div>` : ""}
            </div>`;
          }).join("")}</div>
        </div>
        <aside class="rail">
          <div class="panel"><h4>⚠️ What the secretariat sees</h4><div class="body">
            ${report.findings.length ? report.findings.slice(0, 12).map(f => `<div class="finding ${sev[f.sev] || "ok"}">
              <div class="fd-t">${esc(f.title)}</div><div class="fd-d">${esc(f.detail)}</div></div>`).join("")
              : `<div class="hint">No system-level problems detected. Unlikely, but possible.</div>`}
            <div class="hint" style="margin-top:10px">The coordinator states problems. It does not tell you the answer —
              that is what the Deal Room is for.</div>
          </div></div>
          <div class="panel"><h4>💡 Unused complementarities</h4><div class="body">
            ${report.hints.length ? report.hints.slice(0, 8).map(h => `<div class="finding ok">
              <div class="fd-d">${esc(h.detail)}${h.countries.length ? ` <b>${h.countries.map(c => (ALLY_BY_ID[c] ? ALLY_BY_ID[c].flag + " " + c : c)).join(" · ")}</b>` : ""}</div>
            </div>`).join("") : `<div class="hint">No obvious unused capability.</div>`}
            <div class="hint" style="margin-top:8px">Based on publicly-known comparative advantage, not on anyone's plan.</div>
          </div></div>
          ${nationalBaselineCard()}
        </aside>
      </div>`;
  }

  /* ======================================================================
     PHASE 3 — deal room
     ====================================================================== */
  function commitmentLine(c) {
    const nodes = nodesNow();
    const t = M().commitmentType(c.type);
    const n = nodes.find(x => x.id === c.nodeId);
    const verb = t.verb.replace("{amount}", c.amount == null ? "" : c.amount);
    return `<li><b>${flagOf(c.party)} ${esc(nameOf(c.party))}</b> ${esc(verb)} <b>${esc(n ? n.short : c.nodeId)}</b>
      <span class="role-chip">${esc(t.role)}</span></li>`;
  }

  /** Which coordinator findings would this proposal actually address? */
  function dealAddresses(prop) {
    const ctx = ctxFor("submitted");
    const alliance = M().scoreAlliance(ctx);
    const report = M().generateCoordinatorReport(Object.assign({}, ctx, { alliance }));
    const touched = new Set((prop.commitments || []).map(c => c.nodeId));
    return report.findings.filter(f => touched.has(f.nodeId)).slice(0, 4);
  }

  function proposalCard(p) {
    const isParty = (p.participants || []).indexOf(me()) >= 0;
    const mine = p.proposer === me();
    const resp = p.responses || {};
    const pending = (p.participants || []).filter(u => !resp[u]);
    const addressed = dealAddresses(p);
    return `<div class="deal ${p.status}">
      <div class="deal-head">
        <div class="deal-title">${esc(p.title)}</div>
        <div class="deal-status ${p.status}">${p.status.toUpperCase()}</div>
      </div>
      <div class="deal-parties">${(p.participants || []).map(u => `<span class="dp ${resp[u] === "accept" ? "yes" : resp[u] === "reject" ? "no" : ""}">
        ${flagOf(u)} ${esc(nameOf(u))}${resp[u] === "accept" ? " ✓" : resp[u] === "reject" ? " ✗" : " …"}</span>`).join("")}</div>
      <ul class="deal-commits">${(p.commitments || []).map(commitmentLine).join("")}</ul>
      ${addressed.length ? `<div class="deal-fix">Addresses: ${addressed.map(f => `<span class="fixchip">${esc(f.title)}</span>`).join("")}</div>` : ""}
      <div class="deal-actions">
        ${p.status === "open" && isParty && !resp[me()]
          ? `<button class="btn red sm" data-accept="${p.id}">Accept</button>
             <button class="btn ghost sm" data-reject="${p.id}">Reject</button>
             <button class="btn gold sm" data-counter="${p.id}">Counter…</button>` : ""}
        ${p.status === "open" && mine ? `<button class="btn ghost sm" data-withdraw="${p.id}">Withdraw</button>` : ""}
        ${p.status === "open" && isParty && resp[me()] ? `<span class="hint">You answered ${resp[me()]}. Waiting on ${pending.map(u => nameOf(u)).join(", ") || "nobody"}.</span>` : ""}
        ${p.status === "accepted" ? `<span class="hint">In force. It becomes binding when plans are locked in the Commitment phase.</span>` : ""}
      </div>
    </div>`;
  }

  function builder() {
    const nodes = nodesNow();
    const d = LOCAL.draft;
    if (!LOCAL.builderOpen || !d) return `<button class="btn red" id="newDeal">＋ Propose an agreement</button>`;
    const types = M().COMMITMENT_TYPES;
    return `<div class="builder">
      <div class="bd-row"><label>Title</label>
        <input id="bdTitle" type="text" maxlength="48" placeholder="e.g. Allied Battery Compact" value="${esc(d.title)}" /></div>
      <div class="bd-row"><label>Participants</label>
        <div class="bd-parts">${members().map(m => `<label class="bd-chk ${d.participants.indexOf(m.uid) >= 0 ? "on" : ""}">
          <input type="checkbox" data-part="${m.uid}" ${d.participants.indexOf(m.uid) >= 0 ? "checked" : ""} ${m.uid === me() ? "disabled" : ""} />
          ${flagOf(m.uid)} ${esc(m.name)}</label>`).join("")}</div></div>
      <div class="bd-row"><label>Commitments</label><div class="bd-commits">
        ${d.commitments.map((c, i) => {
          const t = M().commitmentType(c.type);
          return `<div class="bd-commit">
            <select data-ci="${i}" data-f="party">${d.participants.map(u => `<option value="${u}" ${u === c.party ? "selected" : ""}>${flagOf(u)} ${esc(nameOf(u))}</option>`).join("")}</select>
            <select data-ci="${i}" data-f="type">${types.map(tt => `<option value="${tt.type}" ${tt.type === c.type ? "selected" : ""}>${esc(tt.label)}</option>`).join("")}</select>
            <select data-ci="${i}" data-f="nodeId">${nodes.map(n => `<option value="${n.id}" ${n.id === c.nodeId ? "selected" : ""}>${esc(n.short)}</option>`).join("")}</select>
            ${t.needsAmount ? `<input class="bd-amt" type="number" min="0" max="20" data-ci="${i}" data-f="amount" value="${c.amount == null ? 3 : c.amount}" />` : `<span class="bd-amt-off">—</span>`}
            <button class="st-btn" data-rmci="${i}" title="Remove">✕</button>
          </div>`;
        }).join("")}
        <button class="btn ghost sm" id="bdAdd">＋ Add commitment</button>
      </div></div>
      <div class="bd-actions">
        <button class="btn red" id="bdSend">Send proposal ▶</button>
        <button class="btn ghost" id="bdCancel">Cancel</button>
      </div>
      <div class="hint">Roles, not territories: someone can lead a node while another is the resilience
        second source, a third finances it and a fourth guarantees the offtake.</div>
    </div>`;
  }

  function negotiationView() {
    const g = game();
    const ctx = ctxFor("submitted");
    const alliance = M().scoreAlliance(ctx);
    const report = M().generateCoordinatorReport(Object.assign({}, ctx, { alliance }));
    const base = (g.baseline || {})[String(g.round)] || {};
    const open = proposals().filter(p => p.status === "open");
    const done = proposals().filter(p => p.status !== "open");
    return `<div class="coord-grid">
      <div>
        <div class="section-title"><h3>Deal Room</h3><div class="rule"></div>
          <span class="hint">Structured agreements — propose, accept, reject, counter</span></div>
        ${builder()}
        ${open.length ? `<div class="deal-group"><h5>On the table</h5>${open.map(proposalCard).join("")}</div>` : ""}
        ${done.length ? `<div class="deal-group"><h5>Settled</h5>${done.map(proposalCard).join("")}</div>` : ""}
        ${!proposals().length ? `<div class="privacy-note">No proposals yet. Someone has to move first —
          and whoever fills the alliance's worst gap will want compensating for it.</div>` : ""}
      </div>
      <aside class="rail">
        <div class="panel"><h4>📉 Uncoordinated baseline</h4><div class="body">
          <div class="nat-num">${base.headline == null ? "—" : base.headline}<span>/100</span></div>
          <div class="hint">Beat this. Every accepted agreement changes what gets built when plans are locked.</div>
        </div></div>
        <div class="panel"><h4>⚠️ Open problems</h4><div class="body">
          ${report.findings.slice(0, 8).map(f => `<div class="finding ${f.sev === 3 ? "crit" : f.sev === 2 ? "warn" : "ok"}">
            <div class="fd-t">${esc(f.title)}</div></div>`).join("") || `<div class="hint">Nothing outstanding.</div>`}
        </div></div>
        <div class="panel"><h4>🧾 Commitment types</h4><div class="body hint">
          ${M().COMMITMENT_TYPES.map(t => `<div><b>${esc(t.label)}</b> — ${esc(t.role)}${M().BREACHABLE.indexOf(t.type) >= 0 ? " · breachable" : " · enacted by the engine"}</div>`).join("")}
        </div></div>
      </aside>
    </div>`;
  }

  /* ======================================================================
     PHASE 5 — results
     ====================================================================== */
  function countryMatrix(res) {
    const nodes = nodesNow();
    return `<div class="panel"><h4>🗺️ Negotiated division of labour</h4><div class="body" style="overflow-x:auto">
      <table class="matrix">
        <tr><th></th>${nodes.map(n => `<th title="${esc(n.name)}">${esc(n.short)}</th>`).join("")}</tr>
        ${members().map(m => `<tr class="${m.uid === me() ? "you" : ""}">
          <th>${flagOf(m.uid)} ${esc(m.name)}</th>
          ${nodes.map(n => {
            const row = (res.nodes || []).find(x => x.nodeId === n.id) || {};
            const t = ((row.tokensByUid || {})[m.uid]) || 0;
            const capShare = row.byUid && row.afterPct ? Math.round(((row.byUid[m.uid] || 0) / Math.max(1, n.requirement)) * 100) : 0;
            const lead = row.byUid && Object.keys(row.byUid).sort((a, b) => row.byUid[b] - row.byUid[a])[0] === m.uid && t > 0;
            return `<td class="${t ? "on" : ""} ${lead ? "lead" : ""}" title="${t} tokens · ${capShare}% of allied requirement">${t ? capShare + "%" : "·"}</td>`;
          }).join("")}
        </tr>`).join("")}
      </table>
      <div class="hint">Cell = share of the alliance's requirement that country supplies. Gold outline = lead supplier.</div>
    </div></div>`;
  }

  function resultsView() {
    const g = game();
    const res = (g.results || {})[String(g.round)];
    if (!res) return `<div class="privacy-note">Waiting for the host to close the round…</div>`;
    const before = res.before || {}, after = res.after || {};
    const bs = before.summary || {}, as = after.summary || {};
    const bullets = (s) => `
      <li>${s.duplications || 0} duplicated investment${(s.duplications || 0) === 1 ? "" : "s"}</li>
      <li>${s.criticalShortfalls || 0} critical shortfall${(s.criticalShortfalls || 0) === 1 ? "" : "s"}</li>
      <li>${s.singleSource || 0} strategic single-source dependenc${(s.singleSource || 0) === 1 ? "y" : "ies"}</li>
      <li>chain ${s.chainComplete ? "complete" : "incomplete"} (${s.nodesAtScale || 0}/${s.nodeCount || 0} nodes at scale)</li>`;
    const stab = res.stability || {};
    const natRows = members().map(m => {
      const n = (res.national || {})[m.uid] || { total: 0, before: 0, delta: 0 };
      return `<tr class="${m.uid === me() ? "you" : ""}">
        <td><b>${flagOf(m.uid)} ${esc(m.name)}</b></td>
        <td>${n.before}</td><td>${n.total}</td>
        <td class="${n.delta >= 0 ? "up" : "down"}">${n.delta >= 0 ? "+" : ""}${n.delta}</td>
        <td class="mini">${(n.objectives || []).map(o => `<span class="ochip ${o.met ? "met" : ""}">${o.met ? "✓" : "○"} ${esc(o.label)}</span>`).join("")}</td>
        <td class="mini">${((res.trust || {})[m.uid] || {}).rep != null ? ((res.trust || {})[m.uid]).rep : "—"}</td>
      </tr>`;
    }).join("");
    const defectors = Object.keys(res.compliance && res.compliance.defectors || {});
    return `
      <div class="section-title"><h3>Round ${g.round} results</h3><div class="rule"></div>
        <span class="hint">Same countries, same budgets — different bargain</span></div>

      <div class="ba-grid">
        <div class="ba-card before">
          <div class="ba-tag">Before coordination</div>
          <div class="ba-num">${before.headline == null ? "—" : before.headline}</div>
          <ul class="ba-list">${bullets(bs)}</ul>
        </div>
        <div class="ba-mid">
          <div class="gain ${res.gain >= 0 ? "up" : "down"}">
            <div class="g-lbl">Coordination gain</div>
            <div class="g-num">${res.gain >= 0 ? "+" : ""}${res.gain}</div>
          </div>
          <div class="stab ${stab.verdict === "STABLE" ? "ok" : stab.verdict === "WORKABLE" ? "warn" : "crit"}">
            <div class="s-lbl">Coalition stability</div>
            <div class="s-num">${stab.score == null ? "—" : stab.score}</div>
            <div class="s-v">${esc(stab.verdict || "")}</div>
            ${stab.worst && stab.worst.uid ? `<div class="s-d">worst off: ${flagOf(stab.worst.uid)} ${esc(nameOf(stab.worst.uid))} (${stab.worst.delta >= 0 ? "+" : ""}${stab.worst.delta})</div>` : ""}
          </div>
        </div>
        <div class="ba-card after">
          <div class="ba-tag">After coordination</div>
          <div class="ba-num">${after.headline == null ? "—" : after.headline}</div>
          <ul class="ba-list">${bullets(as)}</ul>
        </div>
      </div>

      <div class="section-title"><h3>Why it changed</h3><div class="rule"></div>
        <span class="hint">Ghost marker = before · bar = after</span></div>
      <div class="panel"><div class="body">${componentBars(after.components || {}, before.components || {})}</div></div>

      <div class="section-title"><h3>National outcomes</h3><div class="rule"></div>
        <span class="hint">Distributional politics, made visible</span></div>
      <div class="panel"><div class="body" style="overflow-x:auto">
        <table class="results-table nat-table">
          <tr><th>Country</th><th>Before</th><th>After</th><th>Change</th><th>Private objectives</th><th>Trust</th></tr>
          ${natRows}
        </table>
        ${stab.verdict && stab.verdict !== "STABLE" ? `<div class="warn-note">
          A high alliance score built on one member's losses will not survive the next round —
          they can refuse to renew, and their trust is now a variable in everyone's payoff.</div>` : ""}
      </div></div>

      ${(res.compliance && res.compliance.results || []).length ? `<div class="section-title"><h3>Agreements</h3><div class="rule"></div></div>
      <div class="panel"><div class="body">
        ${(res.compliance.results).map(x => `<div class="finding ${x.honored ? "ok" : "crit"}">
          <div class="fd-t">${x.honored ? "✓ Honoured" : "✗ Breached"} — ${esc(x.title)}</div></div>`).join("")}
        ${defectors.length ? `<div class="warn-note">Defection this round: ${defectors.map(u => `${flagOf(u)} ${esc(nameOf(u))}`).join(", ")}.
          Benefits from those agreements were voided and trust fell by ${D().COORD_CONFIG.trustDefectLoss}.</div>` : ""}
      </div></div>` : ""}

      <div class="section-title"><h3>Who built what</h3><div class="rule"></div>
        <span class="hint">Plans are public once the round is scored</span></div>
      ${countryMatrix(res)}

      <div class="section-title"><h3>Node outcomes</h3><div class="rule"></div></div>
      <div class="panel"><div class="body">
        <table class="results-table">
          <tr><th>Node</th><th>Before</th><th>After</th><th>Suppliers</th><th>Status</th></tr>
          ${(res.nodes || []).map(n => `<tr>
            <td>${esc(n.short)}</td><td>${n.beforePct}%</td>
            <td class="${n.afterPct >= n.beforePct ? "up" : "down"}">${n.afterPct}%</td>
            <td>${n.suppliers}/${n.target}</td>
            <td>${esc(n.status)}</td></tr>`).join("")}
        </table>
      </div></div>`;
  }

  /* ======================================================================
     END GAME — alliance stress test
     ====================================================================== */
  function stressView() {
    const g = game(), s = g.stress;
    if (!s) return `<div class="privacy-note">Running the stress test…</div>`;
    const rows = [
      { key: "independent", label: "Independent national strategies", note: "Round-1 plans, never negotiated, never adapted", tone: "crit" },
      { key: "negotiated", label: "Your negotiated alliance", note: "What you actually built together", tone: "good" },
      { key: "benchmark", label: "Best achievable benchmark", note: "Greedy optimiser given the same tokens", tone: "ok" },
    ];
    const tests = (s.negotiated.tests || []).map(t => t.testId);
    return `
      <div class="section-title"><h3>Alliance stress test</h3><div class="rule"></div>
        <span class="hint">Three shocks the alliance did not choose</span></div>

      <div class="stress-grid">${rows.map(r => {
        const v = s[r.key];
        return `<div class="stress-card ${r.tone}">
          <div class="st-lbl">${r.label}</div>
          <div class="st-num">${v.resilience}<span>%</span></div>
          <div class="st-sub">system resilience</div>
          <div class="st-score">alliance score ${v.headline}</div>
          <div class="st-note">${r.note}</div>
        </div>`;
      }).join("")}</div>

      <div class="price-card">
        <div class="pc-lbl">Price of non-coordination avoided</div>
        <div class="pc-num">${s.price.avoidedPct == null ? "—" : s.price.avoidedPct + "%"}</div>
        <div class="pc-sub">
          Independent strategies scored <b>${s.independent.headline}</b>; you reached <b>${s.negotiated.headline}</b>;
          the benchmark reached <b>${s.benchmark.headline}</b>.
          ${s.price.avoidedPct == null
            ? `The benchmark did not beat the uncoordinated baseline here, so there is no gap to express a share of.`
            : `You captured <b>${s.price.avoidedPct}%</b> of the available coordination gain on alliance score`}${s.resiliencePrice.avoidedPct == null
            ? `. On resilience under stress you moved ${s.independent.resilience}% → <b>${s.negotiated.resilience}%</b>;
               the score-optimal benchmark managed only ${s.benchmark.resilience}%, because maximising the alliance
               score is not the same thing as being robust — a lesson worth keeping.`
            : s.resiliencePrice.avoidedPct > 100
            ? `. Under stress you reached <b>${s.negotiated.resilience}%</b> resilience — past the score-optimal
               benchmark's ${s.benchmark.resilience}% (up from ${s.independent.resilience}%). You built something more
               robust than score-maximising alone would have produced.`
            : `, and <b>${s.resiliencePrice.avoidedPct}%</b> of it on resilience under stress
               (${s.independent.resilience}% → ${s.negotiated.resilience}%, benchmark ${s.benchmark.resilience}%).`}
        </div>
      </div>

      <div class="section-title"><h3>Shock by shock</h3><div class="rule"></div>
        <span class="hint">Share of strategically-weighted allied demand still met</span></div>
      <div class="panel"><div class="body" style="overflow-x:auto">
        <table class="results-table">
          <tr><th>Stress</th><th>Independent</th><th>Negotiated</th><th>Benchmark</th></tr>
          ${tests.map((tid, i) => {
            const t = s.negotiated.tests[i];
            return `<tr><td><b>${esc(t.title)}</b><br><span class="mini">${esc(t.blurb)}</span></td>
              <td class="down">${s.independent.tests[i].resilience}%</td>
              <td class="up">${t.resilience}%</td>
              <td>${s.benchmark.tests[i].resilience}%</td></tr>`;
          }).join("")}
        </table>
        <div class="hint" style="margin-top:10px">
          Formulas: export restrictions apply a ${pct(D().getStressTest("export_restrictions").params.singleSourceHaircut)}%
          haircut to any node with a single allied supplier, shut sub-scale plants, and throttle nodes whose
          upstream inputs are missing. The demand surge raises requirement
          ${D().getStressTest("demand_surge").params.demandMultiplier}× and only at-scale plants can respond.
          Retrenchment cuts output ${pct(D().getStressTest("fiscal_retrenchment").params.capacityHaircut)}% and closes
          anything left below minimum viable scale. These are game-model assumptions, not welfare estimates.
        </div>
      </div></div>

      <div style="text-align:center;margin:22px 0">
        <button class="btn red" id="coordFinish">Back to the home screen</button>
      </div>`;
  }

  /* ======================================================================
     EVENT WIRING (re-bound after every paint)
     ====================================================================== */
  function wire() {
    const on = (sel, ev, fn) => { const el = $c(sel); if (el) el.addEventListener(ev, fn); };
    const all = (sel, ev, fn) => Array.from(document.querySelectorAll(sel)).forEach(el => el.addEventListener(ev, fn));

    on("#coordAdvance", "click", advance);
    on("#coordLeave", "click", () => { if (typeof leaveLeague === "function") leaveLeague(); });
    on("#coordFinish", "click", () => { if (typeof leaveLeague === "function") leaveLeague(); });

    all("#coordScreen .st-btn[data-bump]", "click", (e) => {
      const b = e.currentTarget;
      bump(b.dataset.node, parseInt(b.dataset.bump, 10));
    });
    on("#submitPlan", "click", submitPlan);
    on("#unsubmit", "click", unsubmitPlan);
    on("#lockPlan", "click", lockPlan);
    on("#unlock", "click", unlockPlan);
    on("#adopt", "click", adoptCommitments);
    on("#suggest", "click", suggestAlloc);
    on("#clearAlloc", "click", clearAlloc);

    /* deal room */
    on("#newDeal", "click", newDraft);
    on("#bdCancel", "click", () => { LOCAL.builderOpen = false; LOCAL.draft = null; render(); });
    on("#bdSend", "click", sendProposal);
    on("#bdTitle", "input", (e) => { LOCAL.draft.title = e.target.value; });
    on("#bdAdd", "click", () => draftPatch(d => d.commitments.push({
      party: d.participants[0] || me(), type: "invest", nodeId: nodesNow()[0].id, amount: 3,
    })));
    all("#coordScreen [data-part]", "change", (e) => draftPatch(d => {
      const uid = e.target.dataset.part;
      if (e.target.checked) { if (d.participants.indexOf(uid) < 0) d.participants.push(uid); }
      else {
        d.participants = d.participants.filter(u => u !== uid);
        d.commitments = d.commitments.filter(c => c.party !== uid);
      }
    }));
    all("#coordScreen [data-ci]", "change", (e) => draftPatch(d => {
      const i = parseInt(e.target.dataset.ci, 10), f = e.target.dataset.f;
      d.commitments[i][f] = f === "amount" ? Math.max(0, parseInt(e.target.value, 10) || 0) : e.target.value;
    }));
    all("#coordScreen [data-rmci]", "click", (e) =>
      draftPatch(d => d.commitments.splice(parseInt(e.currentTarget.dataset.rmci, 10), 1)));
    all("#coordScreen [data-accept]", "click", (e) => respond(e.currentTarget.dataset.accept, "accept"));
    all("#coordScreen [data-reject]", "click", (e) => respond(e.currentTarget.dataset.reject, "reject"));
    all("#coordScreen [data-counter]", "click", (e) => counter(e.currentTarget.dataset.counter));
    all("#coordScreen [data-withdraw]", "click", (e) => withdraw(e.currentTarget.dataset.withdraw));
  }

  return { startGame, onSnap, hostTick, advance };
})();
