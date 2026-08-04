/* ============================================================================
   LOBBY — multiplayer league flow (create / join / lobby / synchronized draft).
   Uses NET (net.js) for realtime sync and drives the app.js draft renderers.
   ============================================================================ */
(function () {
  let ADAPTER = null, currentSnap = null, joinCode = null, hostIv = null;
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
    STATE.mode = "league";
    STATE.league.snapshot = snap;
    STATE.league.id = snap.id;
    STATE.league.host = snap.host;
    STATE.league.isHost = snap.host === ADAPTER.me;
    STATE.you = ADAPTER.me;
    STATE.difficulty = (snap.settings && snap.settings.difficulty) || "normal";
    STATE.rounds = (snap.settings && snap.settings.rounds) || 3;
    STATE.seatMeta = {};
    Object.entries(snap.players || {}).forEach(([uid, p]) => STATE.seatMeta[uid] = { allyId: p.allyId, name: p.name, ts: p.ts });
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

  /* ---------- create / join ---------- */
  async function createLeague() {
    if (!STATE.pickAlly) { toast("Pick your fighter first"); return; }
    ensureAdapter();
    const nm = playerName();
    const id = await ADAPTER.createLeague({ name: nm + "'s League", settings: { rounds: 3, difficulty: $("#diffSel").value } });
    STATE.league.id = id;
    await ADAPTER.setPlayer({ name: nm, allyId: STATE.pickAlly });
    setUrl(id);
  }
  async function doJoin(code) {
    code = (code || "").trim().toUpperCase();
    if (!code) { toast("Enter a league code"); return; }
    if (!STATE.pickAlly) { toast("Pick your fighter first"); return; }
    ensureAdapter();
    const r = await ADAPTER.joinLeague(code);
    if (r && r.error) { toast("League not found — check the link/code"); return; }
    STATE.league.id = code;
    await ADAPTER.setPlayer({ name: playerName(), allyId: STATE.pickAlly });
    setUrl(code);
  }

  function leaveLeague() {
    if (ADAPTER) ADAPTER.leave();
    ADAPTER = null; currentSnap = null;
    if (hostIv) { clearInterval(hostIv); hostIv = null; }
    STATE.mode = "solo"; STATE.league = null; STATE.seatMeta = {};
    STATE.you = STATE.pickAlly;
    clearTurnTimer();
    try { const u = new URL(location.href); u.searchParams.delete("league"); history.replaceState(null, "", u); } catch (e) {}
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
    ADAPTER.startDraft(order, { rounds, difficulty: diff });
  }

  /* ---------- host watchdog: auto-pick for a disconnected player ---------- */
  function isOnline(snap, uid) {
    const p = (snap.presence || {})[uid];
    if (p == null) return false;
    return ADAPTER.kind === "local" ? (Date.now() - p) < 12000 : true;
  }
  function hostTick() {
    const snap = currentSnap;
    if (!snap || snap.status !== "drafting" || snap.host !== ADAPTER.me) return;
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
        <button class="btn red" id="lobbyStart" ${nPlay < 2 ? "disabled" : ""}>Start Draft ▶</button>
      </div>
      <div class="hint" style="text-align:center">${nPlay < 2 ? "Waiting for at least one more player to join…" : "You're the host — start when everyone's in."}</div>`
      : `<div class="lobby-waiting">Waiting for the host to start the draft…<br><span class="hint">${curRounds} rounds · ${curDiff} difficulty</span></div>`;

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

    $("#copyLink").addEventListener("click", () => {
      const inp = $("#shareLink"); inp.select();
      (navigator.clipboard ? navigator.clipboard.writeText(inp.value) : Promise.reject()).then(() => toast("Link copied!"), () => { document.execCommand && document.execCommand("copy"); toast("Link copied!"); });
    });
    $("#lobbyLeave").addEventListener("click", leaveLeague);
    if (isHost) {
      $("#lobbyStart").addEventListener("click", startLeagueDraft);
      const push = () => ADAPTER.setSettings({ rounds: parseInt($("#lobbyRounds").value, 10), difficulty: $("#lobbyDiff").value });
      $("#lobbyRounds").addEventListener("change", push);
      $("#lobbyDiff").addEventListener("change", push);
    }
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

  window.addEventListener("DOMContentLoaded", () => {
    const cb = $("#createLeagueBtn"), jb = $("#joinLeagueBtn");
    if (cb) cb.addEventListener("click", createLeague);
    if (jb) jb.addEventListener("click", () => doJoin(joinCode || (prompt("Enter league code:") || "")));
    try { const code = new URL(location.href).searchParams.get("league"); if (code) { joinCode = code.toUpperCase(); enterJoinMode(joinCode); } } catch (e) {}
  });

  // expose for app.js
  window.onFighterPicked = onFighterPicked;
  window.leaveLeague = leaveLeague;
})();
