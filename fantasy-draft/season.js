/* ============================================================================
   SEASON — 12-week head-to-head fantasy competition driven by news events.
   Weekly points come from scoring.js applied to the league's Firebase events.
   ============================================================================ */
(function () {
  const $ = (s) => document.querySelector(s);
  const WEEK_MS = 7 * 864e5;

  const players = (snap) => snap.players || {};
  const orderUids = (snap) => (snap.draft && snap.draft.order) || Object.keys(players(snap));
  function rosterOf(snap, uid) { const seq = (snap.draft && snap.draft.seq) || {}; const o = []; Object.values(seq).forEach(e => { if (e.uid === uid) o.push(Number(e.subN)); }); return o; }
  const leagueCountries = (snap) => Object.values(players(snap)).map(p => p.allyId);
  function ctxFor(snap, uid) { const p = players(snap)[uid] || {}; return { country: p.allyId, sectors: rosterOf(snap, uid), leagueCountries: leagueCountries(snap) }; }
  const allEvents = (snap) => Object.values(snap.events || {});
  function currentWeek(season) { if (!season || !season.startTs) return 0; const w = Math.floor((Date.now() - season.startTs) / WEEK_MS); return Math.max(0, Math.min((season.weeks || 12) - 1, w)); }
  function weeklyPoints(snap, uid, week) { const c = ctxFor(snap, uid); let p = 0; allEvents(snap).forEach(ev => { if ((ev.week || 0) === week) p += SCORING.scoreForPlayer(ev, c); }); return p; }
  function totalPoints(snap, uid) { const c = ctxFor(snap, uid); let p = 0; allEvents(snap).forEach(ev => p += SCORING.scoreForPlayer(ev, c)); return p; }

  /* round-robin (circle method), repeated to fill `weeks` */
  function buildSchedule(uids, weeks) {
    let arr = uids.slice(); const BYE = "__bye__"; if (arr.length % 2) arr.push(BYE);
    const n = arr.length, rounds = [];
    for (let r = 0; r < n - 1; r++) {
      const pairs = [];
      for (let i = 0; i < n / 2; i++) { const a = arr[i], b = arr[n - 1 - i]; if (a !== BYE && b !== BYE) pairs.push([a, b]); }
      rounds.push(pairs);
      arr.splice(1, 0, arr.pop());
    }
    const sched = []; for (let w = 0; w < weeks; w++) sched.push(rounds[rounds.length ? w % rounds.length : 0] || []);
    return sched;
  }
  function matchupFor(snap, uid, week) { const pairs = ((snap.season && snap.season.schedule) || [])[week] || []; for (const [a, b] of pairs) { if (a === uid) return b; if (b === uid) return a; } return null; }

  function startSeason() {
    const snap = STATE.league && STATE.league.snapshot; if (!snap) return;
    const uids = orderUids(snap);
    if (uids.length < 2) { toast("Need at least 2 players for a season"); return; }
    STATE.league.adapter.setSeason({ startTs: Date.now(), weeks: 12, schedule: buildSchedule(uids, 12) });
  }

  function standings(snap) {
    const uids = orderUids(snap), wk = currentWeek(snap.season), sched = (snap.season && snap.season.schedule) || [];
    const rec = {}; uids.forEach(u => rec[u] = { w: 0, l: 0, t: 0, pts: totalPoints(snap, u) });
    for (let w = 0; w < wk; w++) (sched[w] || []).forEach(([a, b]) => {
      const pa = weeklyPoints(snap, a, w), pb = weeklyPoints(snap, b, w);
      if (pa > pb) { rec[a].w++; rec[b].l++; } else if (pb > pa) { rec[b].w++; rec[a].l++; } else { rec[a].t++; rec[b].t++; }
    });
    return uids.map(u => ({ uid: u, ...rec[u] })).sort((x, y) => y.w - x.w || y.pts - x.pts);
  }

  /* ---------- CTA on the results screen (before season starts) ---------- */
  function renderSeasonCta(snap) {
    const el = $("#seasonCta"); if (!el) return;
    if (snap.season) { el.innerHTML = ""; return; }
    if (STATE.league && STATE.league.isHost) {
      el.innerHTML = `<div class="season-cta"><div class="hint" style="margin-bottom:8px">Turn this draft into a live fantasy season — real news gives your roster points each week.</div>
        <button class="btn gold" id="startSeasonBtn">Start 12-Week Season ▶</button></div>`;
      $("#startSeasonBtn").addEventListener("click", startSeason);
    } else {
      el.innerHTML = `<div class="hint" style="text-align:center">Waiting for the host to start the 12-week season…</div>`;
    }
  }

  /* ---------- feed helpers ---------- */
  const PILL = { NAT: "Nat. security", ENSC: "Energy/econ", CLIM: "Climate", OPP: "Economic opp." };
  function evTags(ev) {
    const cs = (ev.countries || []).map(id => (ALLY_BY_ID[id] || { flag: id === "CN" ? "🇨🇳" : "🏳️", name: id }).flag).join(" ");
    const secs = (ev.sectors || []).slice(0, 3).map(n => SUB_BY_N[n] ? (n + "·" + CATEGORIES[SUB_BY_N[n].cat].short) : n).join(", ");
    return `<span class="ev-tag">${ev.sentiment < 0 ? "▼" : "▲"} ${ev.type}</span>${cs ? `<span class="ev-tag">${cs}</span>` : ""}${secs ? `<span class="ev-tag">${secs}</span>` : ""}<span class="ev-tag">${PILL[ev.pillar] || ev.pillar}</span>`;
  }

  function renderSeason(snap) {
    showOnly("seasonScreen");
    const me = STATE.you, wk = currentWeek(snap.season), weeks = (snap.season && snap.season.weeks) || 12;
    const opp = matchupFor(snap, me, wk);
    const myPts = weeklyPoints(snap, me, wk), oppPts = opp ? weeklyPoints(snap, opp, wk) : 0;
    const meC = seatCountry(me);
    const side = (uid, pts, lead) => {
      const c = seatCountry(uid);
      return `<div class="mu-side ${lead ? "lead" : ""}" style="--cc1:${c.c1};--cc2:${c.c2}">
        <div class="mu-portrait"><div class="flagbg">${c.flag}</div>${portrait(c, 96)}</div>
        <div class="mu-name">${seatName(uid)}</div><div class="mu-country">${c.flag} ${c.name}</div>
        <div class="mu-pts">${pts >= 0 ? pts : pts}</div><div class="mu-lbl">week pts</div></div>`;
    };
    const oppSide = opp ? side(opp, oppPts, oppPts > myPts)
      : `<div class="mu-side"><div class="mu-portrait" style="display:grid;place-items:center;font-size:40px">🛌</div><div class="mu-name">Bye week</div><div class="mu-country">No opponent</div><div class="mu-pts">—</div></div>`;

    const st = standings(snap);
    const stRows = st.map((r, i) => `<tr class="${r.uid === me ? "you" : ""}"><td>${i + 1}</td>
      <td><b>${seatFlag(r.uid)} ${seatName(r.uid)}</b><br><span style="color:var(--ink-faint);font-size:11px">${seatCountry(r.uid).name}</span></td>
      <td>${r.w}–${r.l}${r.t ? "–" + r.t : ""}</td><td class="sc">${r.pts}</td></tr>`).join("");

    const myCtx = ctxFor(snap, me);
    const evs = allEvents(snap).filter(e => (e.week || 0) === wk).sort((a, b) => (b.ts || 0) - (a.ts || 0)).slice(0, 40);
    const feed = evs.length ? evs.map(ev => {
      const pts = SCORING.scoreForPlayer(ev, myCtx), touch = SCORING.touches(ev, myCtx);
      return `<div class="feed-row ${touch ? "" : "dim"} ${pts < 0 ? "neg" : pts > 0 ? "pos" : ""}">
        <div class="fr-main"><div class="fr-head">${ev.url ? `<a href="${ev.url}" target="_blank" rel="noopener">${escHtml(ev.headline)}</a>` : escHtml(ev.headline)}</div>
          <div class="fr-tags">${evTags(ev)}${ev.source ? `<span class="ev-src">${escHtml(ev.source)}</span>` : ""}</div></div>
        <div class="fr-pts ${pts < 0 ? "neg" : pts > 0 ? "pos" : "zero"}">${pts > 0 ? "+" + pts : pts}</div></div>`;
    }).join("") : `<div class="roster-empty">No scoring news yet this week — the bot runs every few hours and points will appear here.</div>`;

    $("#seasonScreen").innerHTML = `
      <div class="hero">
        <div class="kicker">${escHtml(snap.name || "League")} · Week ${wk + 1} of ${weeks}</div>
        <div class="player-select">Season</div>
        <p class="lede">Your country and your drafted sub-sectors earn fantasy points from real headlines all week — a policy, partnership or investment that boosts (or harms) national security, energy/economic security, competitiveness or climate. Beat your weekly opponent; take the season.</p>
      </div>
      <div class="section-title"><h3>Week ${wk + 1} matchup</h3><div class="rule"></div><span class="hint">Higher weekly points wins the week</span></div>
      <div class="matchup">${side(me, myPts, myPts >= oppPts)}<div class="mu-vs">VS</div>${oppSide}</div>
      <div class="section-title"><h3>Standings</h3><div class="rule"></div><span class="hint">Season winner crowned after week ${weeks}</span></div>
      <div class="panel"><div class="body" style="overflow-x:auto"><table class="results-table"><tr><th>#</th><th>Player</th><th>W–L</th><th>Season pts</th></tr>${stRows}</table></div></div>
      <div class="section-title"><h3>Scoring wire — week ${wk + 1}</h3><div class="rule"></div><span class="hint">${evs.length} headlines · yours highlighted</span></div>
      <div class="feed">${feed}</div>
      <div style="text-align:center;margin-top:22px">
        <button class="btn ghost" id="seasonBoardBtn">View draft board</button>
        <button class="btn ghost" id="seasonLeaveBtn">Leave league</button>
      </div>`;
    $("#seasonBoardBtn").addEventListener("click", () => showOnly("resultsScreen"));
    $("#seasonLeaveBtn").addEventListener("click", leaveLeague);
  }

  function escHtml(s) { return String(s || "").replace(/[&<>"']/g, c => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&#39;" }[c])); }

  window.renderSeason = renderSeason;
  window.renderSeasonCta = renderSeasonCta;
  window.startSeason = startSeason;
})();
