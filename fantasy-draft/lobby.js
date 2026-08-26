/* ============================================================================
   LOBBY — multiplayer league flow (create / join / lobby / synchronized draft).
   Uses NET (net.js) for realtime sync and drives the app.js draft renderers.
   ============================================================================ */
(function () {
  let ADAPTER = null, currentSnap = null, joinCode = null, hostIv = null, connectedId = null;
  let offlineTicks = 0, lastPickIdx = -1;

  const $ = (s) => document.querySelector(s);
  const playerName = () => { const el = $("#playerName"); const v = el ? el.value.trim() : ""; return v || "Player"; };

  function setUrl(id) { try { const u = new URL(location.href); u.searchParams.set("league", id); history.replaceState(null, "", u); } catch (e) {} }
  function shareLink() { try { const u = new URL(location.href); u.searchParams.set("league", (STATE.league && STATE.league.id) || (currentSnap && currentSnap.id) || ""); return u.toString(); } catch (e) { return location.href; } }

  function ensureAdapter() {
    if (ADAPTER) return ADAPTER;
    ADAPTER = NET.make();
    STATE.league = { adapter: ADAPTER, resultsShown: false, id: null };
    ADAPTER.onLeague(onSnap);
    if (!hostIv) hostIv = setInterval(hostTick, 3000);
    return ADAPTER;
  }

  function onSnap(snap) {
    currentSnap = snap;
    if (!snap) return;
    // A visitor who opened the league link but hasn't joined yet: don't hijack their
    // screen — show the join gate so they can pick a fighter and enter. (Also covers
    // the brief moment during create/join before setPlayer lands.)
    const isMember = !!(snap.players && snap.players[ADAPTER.me]);
    if (!isMember) {
      // match the home screen to the room you're about to join
      if (snap.settings && snap.settings.gameMode === "coordination" && typeof setGameMode === "function") setGameMode("coordination");
      enterJoinMode(snap.id); return;
    }
    STATE.mode = "league";
    STATE.league.snapshot = snap;
    STATE.league.id = snap.id;
    STATE.league.host = snap.host;
    STATE.league.isHost = snap.host === ADAPTER.me;
    STATE.you = ADAPTER.me;
    STATE.difficulty = (snap.settings && snap.settings.difficulty) || "normal";
    STATE.rounds = (snap.settings && snap.settings.rounds) || 3;
    STATE.pickSeconds = (snap.settings && snap.settings.pickSeconds != null) ? snap.settings.pickSeconds : 60;
    STATE.seatMeta = {};
    Object.entries(snap.players || {}).forEach(([uid, p]) => STATE.seatMeta[uid] = { allyId: p.allyId, name: p.name, ts: p.ts });
    if (snap.status === "coord") {                     // Alliance Architect
      STATE.gameMode = "coordination";
      if (window.COORD_UI) COORD_UI.onSnap(snap, ADAPTER);
      return;
    }
    if (snap.status === "lobby") showLobby(snap);
    else applyDraftSnapshot(snap);
  }

  function applyDraftSnapshot(snap) {
    const order = (snap.draft && snap.draft.order) || [];
    STATE.order = order;
    STATE.teams = order.map(uid => ({ ally: uid, roster: [], points: 0 }));
    STATE.drafted = {}; STATE.picks = []; STATE.log = [];
    const seq = (snap.draft && snap.draft.seq) || {};
    Object.keys(seq).map(Number).sort((a, b) => a - b).forEach(k => {
      const e = seq[k], sub = SUB_BY_N[e.subN], team = teamOf(e.uid);
      if (!team || !sub) return;
      STATE.drafted[e.subN] = e.uid; team.roster.push(e.subN);
      const pts = scoreFor(seatCountry(e.uid), sub); team.points += pts;
      const pickNo = STATE.picks.length + 1;
      STATE.picks.push({ pickNo, round: e.round, ally: e.uid, subN: e.subN });
      STATE.log.unshift({ pickNo, round: e.round, ally: e.uid, subN: e.subN, you: e.uid === STATE.you, syn: pts - sub.ovr });
    });
    STATE.pickIndex = (snap.draft && snap.draft.pickIndex) || 0;

    if (snap.status === "results" || snap.status === "season") {
      finishDraft();                               // populate the draft board once
      clearTurnTimer();
      if (snap.status === "season") { if (typeof renderSeason === "function") renderSeason(snap); }
      else { showOnly("resultsScreen"); if (typeof renderSeasonCta === "function") renderSeasonCta(snap); }
      return;
    }
    if (!$("#draftScreen").classList.contains("active")) showOnly("draftScreen");
    renderFilters(); renderRail(); renderPool(); updateOnClock(currentAllyId());
    armTurnTimer();
  }

  /* ---------- create / join / resume ---------- */
  // Bind to a league's realtime feed exactly once (idempotent across resume + join).
  async function connect(code) {
    ensureAdapter();
    if (connectedId === code) return { ok: true };
    const r = await ADAPTER.joinLeague(code);
    if (r && r.error) return r;
    connectedId = code;
    STATE.league.id = code;
    return { ok: true };
  }
  // On page load with ?league=CODE: subscribe. If we're already a member (returning
  // player), onSnap routes us straight to the league home; if not, it shows the join gate.
  async function autoResume(code) {
    const r = await connect(code);
    if (r && r.error) { enterJoinMode(code); return; }
  }

  async function createLeague() {
    if (!STATE.pickAlly) { toast("Pick your fighter first"); return; }
    ensureAdapter();
    const nm = playerName();
    const coord = STATE.gameMode === "coordination";
    const id = await ADAPTER.createLeague({
      name: nm + (coord ? "'s Alliance" : "'s League"),
      settings: { rounds: 3, difficulty: $("#diffSel").value, gameMode: coord ? "coordination" : "draft" },
    });
    connectedId = id;
    STATE.league.id = id;
    await ADAPTER.setPlayer({ name: nm, allyId: STATE.pickAlly });
    setUrl(id);
  }
  async function doJoin(code) {
    code = (code || "").trim().toUpperCase();
    if (!code) { toast("Enter a league code"); return; }
    if (!STATE.pickAlly) { toast("Pick your fighter first"); return; }
    const r = await connect(code);
    if (r && r.error) { toast("League not found — check the link/code"); return; }
    await ADAPTER.setPlayer({ name: playerName(), allyId: STATE.pickAlly });
    setUrl(code);
  }

  function leaveLeague() {
    if (ADAPTER) ADAPTER.leave();
    ADAPTER = null; currentSnap = null; connectedId = null; joinCode = null;
    if (hostIv) { clearInterval(hostIv); hostIv = null; }
    STATE.mode = "solo"; STATE.league = null; STATE.seatMeta = {};
    STATE.you = STATE.pickAlly;
    clearTurnTimer();
    try { const u = new URL(location.href); u.searchParams.delete("league"); u.hash = ""; history.replaceState(null, "", u); } catch (e) {}
    // restore the setup-screen join controls to their default (create) state
    const cb = $("#createLeagueBtn"), jb = $("#joinLeagueBtn"), hint = $("#joinHint");
    if (cb) cb.style.display = "";
    if (jb) jb.style.display = "none";
    if (hint) hint.style.display = "none";
    showOnly("setupScreen");
  }

  /* ---------- host: start draft ---------- */
  function startLeagueDraft() {
    const snap = currentSnap; if (!snap) return;
    const uids = Object.keys(snap.players || {});
    if (uids.length < 2) { toast("Need at least 2 players to start"); return; }
    const order = uids.slice();
    for (let i = order.length - 1; i > 0; i--) { const j = Math.floor(Math.random() * (i + 1)); [order[i], order[j]] = [order[j], order[i]]; }
    const maxR = Math.max(1, Math.min(6, Math.floor(SUBSECTORS.length / uids.length)));
    const rounds = Math.min(parseInt($("#lobbyRounds") ? $("#lobbyRounds").value : 3, 10) || 3, maxR);
    const diff = $("#lobbyDiff") ? $("#lobbyDiff").value : "normal";
    const pickSeconds = $("#lobbyClock") ? parseInt($("#lobbyClock").value, 10) : 60;
    ADAPTER.startDraft(order, { rounds, difficulty: diff, pickSeconds });
  }

  /* ---------- host watchdog: auto-pick for a disconnected player ---------- */
  function isOnline(snap, uid) {
    const p = (snap.presence || {})[uid];
    if (p == null) return false;
    return ADAPTER.kind === "local" ? (Date.now() - p) < 12000 : true;
  }
  function hostTick() {
    const snap = currentSnap;
    if (!snap || snap.host !== ADAPTER.me) return;
    if (snap.status === "coord") {
      if (window.COORD_UI) COORD_UI.hostTick(snap, ADAPTER);
      return;
    }
    if (snap.status !== "drafting") return;
    if (STATE.pickIndex >= totalPicks()) return;
    const cur = currentAllyId();
    if (STATE.pickIndex !== lastPickIdx) { lastPickIdx = STATE.pickIndex; offlineTicks = 0; }
    if (cur === ADAPTER.me) return;                 // own turn handled by armTurnTimer
    offlineTicks = isOnline(snap, cur) ? 0 : offlineTicks + 1;
    if (offlineTicks >= 2) {                          // ~6s offline → auto-pick for them
      offlineTicks = 0;
      const country = seatCountry(cur);
      const avail = SUBSECTORS.filter(s => !STATE.drafted[s.n]);
      if (!avail.length) return;
      avail.sort((a, b) => (synergyOf(country, b) + b.ovr * 0.01) - (synergyOf(country, a) + a.ovr * 0.01));
      ADAPTER.makePick(avail[0].n, { seat: cur, pickIndex: STATE.pickIndex, total: totalPicks(), round: currentRound() });
    }
  }

  /* ---------- lobby UI ---------- */
  function showLobby(snap) {
    showOnly("lobbyScreen");
    STATE.league.resultsShown = false;
    const players = Object.entries(snap.players || {});
    const isHost = snap.host === ADAPTER.me;
    const nPlay = players.length;
    const maxR = Math.max(1, Math.min(6, Math.floor(SUBSECTORS.length / Math.max(1, nPlay))));
    const curRounds = Math.min((snap.settings && snap.settings.rounds) || 3, maxR);
    const curDiff = (snap.settings && snap.settings.difficulty) || "normal";
    const curClock = (snap.settings && snap.settings.pickSeconds != null) ? snap.settings.pickSeconds : 60;
    const clockLabel = (s) => s <= 0 ? "no clock" : s < 60 ? s + "s per pick" : (s / 60) + " min per pick";
    const online = (uid) => isOnline(snap, uid);

    const pcards = players.map(([uid, p]) => {
      const c = ALLY_BY_ID[p.allyId] || ALLY_BY_ID.US;
      const you = uid === ADAPTER.me, host = uid === snap.host;
      return `<div class="lp-card" style="--cc1:${c.c1};--cc2:${c.c2}">
        <div class="lp-portrait"><div class="flagbg">${c.flag}</div>${portrait(c, 96)}
          <span class="lp-dot ${online(uid) ? "on" : "off"}"></span></div>
        <div class="lp-name">${escapeHtml(p.name)}</div>
        <div class="lp-sub">${c.flag} ${c.name}</div>
        <div class="lp-badges">${host ? `<span class="lp-badge host">HOST</span>` : ""}${you ? `<span class="lp-badge you">YOU</span>` : ""}</div>
      </div>`;
    }).join("");

    /* ---- Alliance Architect lobby (coordination mode) ---- */
    const isCoord = (snap.settings && snap.settings.gameMode) === "coordination";
    if (isCoord) {
      const scen = Object.values(COORD_DATA.COORD_SCENARIOS);
      const curScen = (snap.settings && snap.settings.scenario) || "batteries";
      const curCR = (snap.settings && snap.settings.coordRounds) || COORD_DATA.COORD_CONFIG.roundsDefault;
      const curTok = (snap.settings && snap.settings.tokens) || COORD_DATA.COORD_CONFIG.tokensPerRound;
      const curBots = (snap.settings && snap.settings.bots != null) ? snap.settings.bots : Math.max(0, 3 - nPlay);
      const seats = nPlay + curBots;
      /* One country per seat. Two players running the same country would have
         identical comparative advantage, which makes the whole negotiation
         meaningless — so block the start instead of quietly allowing it. */
      const claimed = players.map(([, p]) => p.allyId);
      const dupes = [...new Set(claimed.filter((c, i) => claimed.indexOf(c) !== i))];
      const seatsOk = seats >= COORD_DATA.COORD_CONFIG.minPlayers &&
        seats <= COORD_DATA.COORD_CONFIG.maxPlayers && !dupes.length;
      const coordCtl = isHost ? `
        <div class="lobby-controls">
          <div class="field">Scenario
            <select id="cScen">${scen.map(s => `<option value="${s.id}" ${s.id === curScen ? "selected" : ""} ${s.available ? "" : "disabled"}>${s.name}${s.available ? "" : " — soon"}</option>`).join("")}</select>
          </div>
          <div class="field">Rounds
            <select id="cRounds">${[2, 3, 4].map(r => `<option value="${r}" ${r === curCR ? "selected" : ""}>${r} rounds</option>`).join("")}</select>
          </div>
          <div class="field">Policy tokens
            <select id="cTokens">${[8, 10, 12].map(t => `<option value="${t}" ${t === curTok ? "selected" : ""}>${t} per round</option>`).join("")}</select>
          </div>
          <div class="field">AI allies
            <select id="cBots" title="Fill empty seats with AI-run countries">${[0, 1, 2, 3, 4, 5].map(b => `<option value="${b}" ${b === curBots ? "selected" : ""}>${b}</option>`).join("")}</select>
          </div>
          <button class="btn red" id="coordStart" ${seatsOk ? "" : "disabled"}>Open ${COORD_DATA.COORD_CONFIG.title} ▶</button>
        </div>
        <div class="hint" style="text-align:center">${dupes.length
          ? `Two players have picked <b>${dupes.map(d => (ALLY_BY_ID[d] ? ALLY_BY_ID[d].name : d)).join(", ")}</b>.
             One country per seat — ask one of them to change fighter.`
          : seatsOk
          ? `${nPlay} human${nPlay === 1 ? "" : "s"} + ${curBots} AI = <b>${seats} countries</b> at the table.`
          : `Needs ${COORD_DATA.COORD_CONFIG.minPlayers}–${COORD_DATA.COORD_CONFIG.maxPlayers} countries — currently ${seats}. Invite allies or add AI allies.`}</div>`
        : `<div class="lobby-waiting">Waiting for the host to open the negotiation room…<br><span class="hint">${COORD_DATA.getScenario(curScen).name} · ${curCR} rounds · ${curTok} policy tokens per round</span></div>`;

      $("#lobbyScreen").innerHTML = `
        <div class="hero">
          <div class="kicker">${NET.backend === "firebase" ? "Online alliance" : "Local alliance (this browser only)"} · ${nPlay} in the room</div>
          <div class="player-select">${COORD_DATA.COORD_CONFIG.title}</div>
          <p class="lede">${escapeHtml(snap.name || "Allied Coordination")} — share the link so allies can join.
            Each of you runs one country's industrial policy. You will plan privately, then discover together
            what the alliance actually built.</p>
        </div>
        <div class="share-row">
          <input id="shareLink" readonly value="${shareLink()}" />
          <button class="btn gold" id="copyLink">Copy link</button>
          <span class="code-chip">Code <b>${snap.id}</b></span>
        </div>
        <div class="section-title"><h3>Countries at the table</h3><div class="rule"></div><span class="hint">One player, one country</span></div>
        <div class="lobby-players">${pcards}</div>
        ${coordCtl}
        <div style="text-align:center;margin-top:22px"><button class="btn ghost" id="lobbyLeave">Leave room</button></div>`;

      $("#copyLink").addEventListener("click", copyShare);
      $("#lobbyLeave").addEventListener("click", leaveLeague);
      if (isHost) {
        const push = () => ADAPTER.setSettings({
          scenario: $("#cScen").value, coordRounds: parseInt($("#cRounds").value, 10),
          tokens: parseInt($("#cTokens").value, 10), bots: parseInt($("#cBots").value, 10),
        });
        ["#cScen", "#cRounds", "#cTokens", "#cBots"].forEach(s => $(s).addEventListener("change", push));
        $("#coordStart").addEventListener("click", () => {
          const cfg = {
            scenario: $("#cScen").value, rounds: parseInt($("#cRounds").value, 10),
            tokens: parseInt($("#cTokens").value, 10), bots: parseInt($("#cBots").value, 10),
          };
          COORD_UI.startGame(snap, ADAPTER, cfg);
        });
      }
      return;
    }

    const hostCtl = isHost ? `
      <div class="lobby-controls">
        <div class="field">Rounds
          <select id="lobbyRounds">${Array.from({ length: maxR }, (_, i) => i + 1).map(r => `<option value="${r}" ${r === curRounds ? "selected" : ""}>${r} · ${r * nPlay} picks</option>`).join("")}</select>
        </div>
        <div class="field">Difficulty
          <select id="lobbyDiff">
            <option value="easy" ${curDiff === "easy" ? "selected" : ""}>Easy — all numbers</option>
            <option value="normal" ${curDiff === "normal" ? "selected" : ""}>Normal — synergies only</option>
            <option value="hard" ${curDiff === "hard" ? "selected" : ""}>Hard — no numbers</option>
          </select>
        </div>
        <div class="field">Pick clock
          <select id="lobbyClock" title="Time each player gets per pick before it auto-picks">
            <option value="30" ${curClock === 30 ? "selected" : ""}>30 sec</option>
            <option value="60" ${curClock === 60 ? "selected" : ""}>1 min</option>
            <option value="120" ${curClock === 120 ? "selected" : ""}>2 min</option>
            <option value="300" ${curClock === 300 ? "selected" : ""}>5 min</option>
            <option value="0" ${curClock === 0 ? "selected" : ""}>Off</option>
          </select>
        </div>
        <button class="btn red" id="lobbyStart" ${nPlay < 2 ? "disabled" : ""}>Start Draft ▶</button>
      </div>
      <div class="hint" style="text-align:center">${nPlay < 2 ? "Waiting for at least one more player to join…" : "You're the host — start when everyone's in."}</div>`
      : `<div class="lobby-waiting">Waiting for the host to start the draft…<br><span class="hint">${curRounds} rounds · ${curDiff} difficulty · ${clockLabel(curClock)}</span></div>`;

    $("#lobbyScreen").innerHTML = `
      <div class="hero">
        <div class="kicker">${NET.backend === "firebase" ? "Online league" : "Local league (this browser only)"} · ${nPlay} in the room</div>
        <div class="player-select">Draft Lobby</div>
        <p class="lede">${escapeHtml(snap.name || "Allied Draft")} — share the link so allies can join. Everyone drafts together in real time.</p>
      </div>
      <div class="share-row">
        <input id="shareLink" readonly value="${shareLink()}" />
        <button class="btn gold" id="copyLink">Copy link</button>
        <span class="code-chip">Code <b>${snap.id}</b></span>
      </div>
      <div class="section-title"><h3>Players</h3><div class="rule"></div><span class="hint">Each ally is one fighter</span></div>
      <div class="lobby-players">${pcards}</div>
      ${hostCtl}
      <div style="text-align:center;margin-top:22px"><button class="btn ghost" id="lobbyLeave">Leave league</button></div>`;

    $("#copyLink").addEventListener("click", copyShare);
    $("#lobbyLeave").addEventListener("click", leaveLeague);
    if (isHost) {
      $("#lobbyStart").addEventListener("click", startLeagueDraft);
      const push = () => ADAPTER.setSettings({ rounds: parseInt($("#lobbyRounds").value, 10), difficulty: $("#lobbyDiff").value, pickSeconds: parseInt($("#lobbyClock").value, 10) });
      $("#lobbyRounds").addEventListener("change", push);
      $("#lobbyDiff").addEventListener("change", push);
      $("#lobbyClock").addEventListener("change", push);
    }
  }

  function copyShare() {
    const inp = $("#shareLink"); if (!inp) return;
    inp.select();
    (navigator.clipboard ? navigator.clipboard.writeText(inp.value) : Promise.reject())
      .then(() => toast("Link copied!"), () => { document.execCommand && document.execCommand("copy"); toast("Link copied!"); });
  }

  function escapeHtml(s) { return String(s || "").replace(/[&<>"']/g, c => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&#39;" }[c])); }

  /* ---------- setup-screen wiring ---------- */
  function onFighterPicked() {
    const cb = $("#createLeagueBtn"), jb = $("#joinLeagueBtn");
    if (cb) cb.disabled = false; if (jb) jb.disabled = false;
  }
  function enterJoinMode(code) {
    const hint = $("#joinHint"); if (hint) { hint.style.display = "block"; hint.innerHTML = `You're joining league <b>${escapeHtml(code)}</b> — pick your fighter and enter a name, then <b>Join</b>.`; }
    const cb = $("#createLeagueBtn"), jb = $("#joinLeagueBtn");
    if (cb) cb.style.display = "none";
    if (jb) jb.style.display = "";
  }

  /* ---------- Alliance Architect: single-player practice ----------
     Creates a room (local adapter unless Firebase is configured), seats the
     player, fills the rest of the table with AI-run countries and opens the
     game immediately. Same code path as multiplayer — bots are just seats the
     host client plans for. */
  async function coordSolo() {
    if (!STATE.pickAlly) { toast("Pick your country first"); return; }
    ensureAdapter();
    const nm = playerName();
    const id = await ADAPTER.createLeague({
      name: nm + "'s Alliance (practice)",
      settings: { gameMode: "coordination", difficulty: "normal" },
    });
    connectedId = id; STATE.league.id = id;
    await ADAPTER.setPlayer({ name: nm, allyId: STATE.pickAlly });
    setUrl(id);
    const cfg = {
      scenario: ($("#coordScenarioSel") && $("#coordScenarioSel").value) || "batteries",
      rounds: parseInt(($("#coordRoundsSel") || {}).value || 3, 10),
      tokens: parseInt(($("#coordTokensSel") || {}).value || 10, 10),
      bots: parseInt(($("#coordBotsSel") || {}).value || 3, 10),
    };
    // the snapshot may not have landed yet — synthesise the minimum COORD_UI needs
    const seed = currentSnap || { id, host: ADAPTER.me, players: { [ADAPTER.me]: { name: nm, allyId: STATE.pickAlly } } };
    COORD_UI.startGame(seed, ADAPTER, cfg);
  }

  window.addEventListener("DOMContentLoaded", () => {
    const cb = $("#createLeagueBtn"), jb = $("#joinLeagueBtn");
    if (cb) cb.addEventListener("click", createLeague);
    const sb = $("#coordSoloBtn"); if (sb) sb.addEventListener("click", coordSolo);
    const ss = $("#coordScenarioSel");
    if (ss && typeof COORD_DATA !== "undefined") {
      ss.innerHTML = Object.values(COORD_DATA.COORD_SCENARIOS)
        .map(s => `<option value="${s.id}" ${s.available ? "" : "disabled"}>${s.name}${s.available ? "" : " — soon"}</option>`).join("");
    }
    if (jb) jb.addEventListener("click", () => doJoin(joinCode || (prompt("Enter league code:") || "")));
    try { const code = new URL(location.href).searchParams.get("league"); if (code) { joinCode = code.toUpperCase(); enterJoinMode(joinCode); autoResume(joinCode); } } catch (e) {}
  });

  // expose for app.js
  window.onFighterPicked = onFighterPicked;
  window.leaveLeague = leaveLeague;
})();
