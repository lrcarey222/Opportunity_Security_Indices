/* ============================================================================
   SEASON / LEAGUE HOME — the persistent, URL-addressable hub for a league once
   the draft is done. 12-week head-to-head fantasy competition driven by news.

   Hash-routed sub-views (shareable, static-Pages friendly):
     #home            → standings (season pts + weekly W–L) + this week's wire   [default]
     #matchup         → your weekly matchup + per-sector score breakdown
     #sector/<n>      → the news items that gave a sector its points this week
     #sector/country  → the national news that scored for your country
     #board           → the draft priorities / final board (results screen)

   Weekly points come from scoring.js applied to the league's Firebase events.
   ============================================================================ */
(function () {
  const $ = (s) => document.querySelector(s);
  const $$ = (s, r) => [...(r || document).querySelectorAll(s)];
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

  /* per-week score attribution: how each drafted sector (and your country) earned points */
  function weekBreakdown(snap, uid, week) {
    const c = ctxFor(snap, uid);
    const evs = allEvents(snap).filter(e => (e.week || 0) === week);
    const sectorMap = {};
    const country = { pts: 0, events: [] };
    evs.forEach(ev => {
      const d = SCORING.scoreDetail(ev, c);
      if (d.total === 0) return;
      if (d.countryPortion) { country.pts += d.countryPortion; country.events.push({ ev, pts: d.countryPortion }); }
      if (d.sectorPortion && d.matched.length) {
        const share = d.sectorPortion / d.matched.length;
        d.matched.forEach(sn => {
          const m = sectorMap[sn] || (sectorMap[sn] = { subN: sn, pts: 0, events: [] });
          m.pts += share; m.events.push({ ev, pts: share });
        });
      }
    });
    const sectors = Object.values(sectorMap)
      .map(s => ({ subN: s.subN, pts: Math.round(s.pts), events: s.events.sort((a, b) => Math.abs(b.pts) - Math.abs(a.pts)) }))
      .sort((a, b) => Math.abs(b.pts) - Math.abs(a.pts) || b.pts - a.pts);
    country.pts = Math.round(country.pts);
    country.events.sort((a, b) => Math.abs(b.pts) - Math.abs(a.pts));
    return { sectors, country, total: weeklyPoints(snap, uid, week) };
  }

  /* drafted sub-sectors for a team, in draft-pick order (slot 1..R) */
  function orderedRoster(snap, uid) {
    const seq = (snap.draft && snap.draft.seq) || {};
    return Object.keys(seq).map(Number).sort((a, b) => a - b).filter(k => seq[k].uid === uid).map(k => Number(seq[k].subN));
  }
  /* {subN: weekly points} for a team this week (0 for sectors that didn't score) */
  function weekSectorMap(snap, uid, week) { const m = {}; weekBreakdown(snap, uid, week).sectors.forEach(s => m[s.subN] = s.pts); return m; }

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

  /* ---------- feed / tag helpers ---------- */
  const PILL = { NAT: "Nat. security", ENSC: "Energy/econ", CLIM: "Climate", OPP: "Economic opp." };
  function evTags(ev) {
    const cs = (ev.countries || []).map(id => (ALLY_BY_ID[id] || { flag: id === "CN" ? "🇨🇳" : "🏳️" }).flag).join(" ");
    const secs = (ev.sectors || []).slice(0, 3).map(n => SUB_BY_N[n] ? (n + "·" + CATEGORIES[SUB_BY_N[n].cat].short) : n).join(", ");
    return `<span class="ev-tag">${ev.sentiment < 0 ? "▼" : "▲"} ${ev.type}</span>${cs ? `<span class="ev-tag">${cs}</span>` : ""}${secs ? `<span class="ev-tag">${secs}</span>` : ""}<span class="ev-tag">${PILL[ev.pillar] || ev.pillar}</span>`;
  }
  function feedRow(ev, pts, touch) {
    return `<div class="feed-row ${touch ? "" : "dim"} ${pts < 0 ? "neg" : pts > 0 ? "pos" : ""}">
      <div class="fr-main"><div class="fr-head">${ev.url ? `<a href="${ev.url}" target="_blank" rel="noopener">${escHtml(ev.headline)}</a>` : escHtml(ev.headline)}</div>
        <div class="fr-tags">${evTags(ev)}${ev.source ? `<span class="ev-src">${escHtml(ev.source)}</span>` : ""}</div></div>
      <div class="fr-pts ${pts < 0 ? "neg" : pts > 0 ? "pos" : "zero"}">${pts > 0 ? "+" + pts : pts}</div></div>`;
  }
  const sgn = (n) => (n > 0 ? "+" + n : "" + n);

  function matchPortrait(uid, pts, lead) {
    const c = seatCountry(uid);
    return `<div class="mu-side ${lead ? "lead" : ""}" style="--cc1:${c.c1};--cc2:${c.c2}">
      <div class="mu-portrait"><div class="flagbg">${c.flag}</div>${portrait(c, 96)}</div>
      <div class="mu-name">${escHtml(seatName(uid))}</div><div class="mu-country">${c.flag} ${c.name}</div>
      <div class="mu-pts">${pts}</div><div class="mu-lbl">week pts</div></div>`;
  }

  /* ---------- navigation ---------- */
  function go(hash) {
    if (location.hash === "#" + hash) renderLeague(STATE.league && STATE.league.snapshot);
    else location.hash = hash;
  }
  function homeLink() {
    try { const u = new URL(location.href); u.searchParams.set("league", (STATE.league && STATE.league.id) || ""); u.hash = "home"; return u.toString(); } catch (e) { return location.href; }
  }
  function navBar(active) {
    const tab = (h, label) => `<button class="lnav-tab ${active === h ? "on" : ""}" data-h="${h}">${label}</button>`;
    return `<div class="lnav">${tab("home", "🏆 Home")}${tab("matchup", "⚔ Matchup")}${tab("board", "📋 Draft board")}
      <span class="spacer"></span><button class="lnav-tab ghost" id="lnavCopy">🔗 Copy home link</button></div>`;
  }
  function wireNav(scope) {
    $$(".lnav-tab[data-h]", scope).forEach(b => b.addEventListener("click", () => go(b.dataset.h)));
    const copy = (scope || document).querySelector("#lnavCopy");
    if (copy) copy.addEventListener("click", () => {
      const link = homeLink();
      (navigator.clipboard ? navigator.clipboard.writeText(link) : Promise.reject()).then(() => toast("League home link copied!"), () => toast(link));
    });
  }

  /* ============================ HOME ============================ */
  function renderHome(snap) {
    showOnly("seasonScreen");
    const me = STATE.you, wk = currentWeek(snap.season), weeks = (snap.season && snap.season.weeks) || 12;
    const st = standings(snap);
    const leader = st[0];
    const opp = matchupFor(snap, me, wk);
    const myWk = weeklyPoints(snap, me, wk), oppWk = opp != null ? weeklyPoints(snap, opp, wk) : null;

    const stRows = st.map((r, i) => {
      const c = seatCountry(r.uid);
      return `<tr class="${r.uid === me ? "you" : ""}">
        <td>${i === 0 ? "👑" : i + 1}</td>
        <td><span class="st-team"><span class="st-flag">${c.flag}</span><span><b>${escHtml(seatName(r.uid))}</b><br><span class="st-sub">${c.name}</span></span></span></td>
        <td>${r.w}–${r.l}${r.t ? "–" + r.t : ""}</td>
        <td>${sgn(weeklyPoints(snap, r.uid, wk))}</td>
        <td class="sc">${r.pts}</td></tr>`;
    }).join("");

    const myCtx = ctxFor(snap, me);
    const evs = allEvents(snap).filter(e => (e.week || 0) === wk).sort((a, b) => (b.ts || 0) - (a.ts || 0)).slice(0, 30);
    const feed = evs.length ? evs.map(ev => feedRow(ev, SCORING.scoreForPlayer(ev, myCtx), SCORING.touches(ev, myCtx))).join("")
      : `<div class="roster-empty">No scoring news yet this week — the bot runs every few hours and points will appear here.</div>`;

    const myMatch = opp != null
      ? `<div class="hm-matchcard" id="hmMatch"><div class="hm-mc-lbl">Your Week ${wk + 1} matchup — tap for the breakdown</div>
           <div class="hm-mc-body"><b>${seatFlag(me)} ${escHtml(seatName(me))}</b> <span class="hm-mc-pts ${myWk >= oppWk ? "lead" : ""}">${myWk}</span>
           <span class="hm-mc-vs">vs</span> <span class="hm-mc-pts ${oppWk > myWk ? "lead" : ""}">${oppWk}</span> <b>${seatFlag(opp)} ${escHtml(seatName(opp))}</b> ▸</div></div>`
      : `<div class="hm-matchcard"><div class="hm-mc-lbl">Week ${wk + 1}</div><div class="hm-mc-body">Bye week — no opponent. Bank season points.</div></div>`;

    $("#seasonScreen").innerHTML = `
      ${navBar("home")}
      <div class="hero">
        <div class="kicker">${escHtml(snap.name || "League")} · Week ${wk + 1} of ${weeks}</div>
        <div class="player-select">League Home</div>
        <p class="lede">Every player's country and drafted sub-sectors earn fantasy points from real headlines all season. Leader takes the crown after week ${weeks}. Current leader: <b>${seatFlag(leader.uid)} ${escHtml(seatName(leader.uid))}</b>.</p>
      </div>
      ${myMatch}
      <div class="section-title"><h3>Standings</h3><div class="rule"></div><span class="hint">Season points + weekly head-to-head record</span></div>
      <div class="panel"><div class="body" style="overflow-x:auto">
        <table class="results-table standings-table"><tr><th>#</th><th>Country</th><th>W–L</th><th>Wk ${wk + 1}</th><th>Season pts</th></tr>${stRows}</table>
      </div></div>
      <div class="section-title"><h3>News wire — week ${wk + 1}</h3><div class="rule"></div><span class="hint">${evs.length} headlines · your points shown</span></div>
      <div class="feed">${feed}</div>
      <div style="text-align:center;margin-top:22px">
        <button class="btn gold" id="goMatchup">See your matchup breakdown ▸</button>
        <button class="btn ghost" id="goBoard">Draft board</button>
        <button class="btn ghost" id="leaveBtn">Leave league</button>
      </div>`;
    wireNav($("#seasonScreen"));
    const mc = $("#hmMatch"); if (mc) mc.addEventListener("click", () => go("matchup"));
    $("#goMatchup").addEventListener("click", () => go("matchup"));
    $("#goBoard").addEventListener("click", () => go("board"));
    $("#leaveBtn").addEventListener("click", leaveLeague);
  }

  /* ============================ MATCHUP ============================ */
  function renderMatchup(snap) {
    showOnly("seasonScreen");
    const me = STATE.you, wk = currentWeek(snap.season), weeks = (snap.season && snap.season.weeks) || 12;
    const opp = matchupFor(snap, me, wk);
    const myPts = weeklyPoints(snap, me, wk), oppPts = opp != null ? weeklyPoints(snap, opp, wk) : 0;
    const oppSide = opp != null ? matchPortrait(opp, oppPts, oppPts > myPts)
      : `<div class="mu-side"><div class="mu-portrait" style="display:grid;place-items:center;font-size:40px">🛌</div><div class="mu-name">Bye week</div><div class="mu-country">No opponent</div><div class="mu-pts">—</div></div>`;

    // classic head-to-head box score: each team's drafted sub-sectors (like roster players)
    // with the points they scored this week, aligned by draft slot, national-news + totals rows.
    const myR = orderedRoster(snap, me), opR = opp != null ? orderedRoster(snap, opp) : [];
    const mySec = weekSectorMap(snap, me, wk), opSec = opp != null ? weekSectorMap(snap, opp, wk) : {};
    const myNat = weekBreakdown(snap, me, wk).country.pts, opNat = opp != null ? weekBreakdown(snap, opp, wk).country.pts : 0;
    const meC = seatCountry(me), opC = opp != null ? seatCountry(opp) : null;
    const maxLen = Math.max(myR.length, opR.length, 1);

    const ptsCell = (pts, side) => `<td class="mb-pts ${side} ${pts < 0 ? "neg" : pts > 0 ? "pos" : "zero"}">${sgn(pts)}</td>`;
    const playerCell = (subN, side, uid) => {
      if (subN == null) return `<td class="mb-player ${side} empty"></td>`;
      const sub = SUB_BY_N[subN], cat = sub ? CATEGORIES[sub.cat] : null;
      return `<td class="mb-player ${side}" data-sec="${subN}" data-uid="${uid}" style="--cc2:${cat ? cat.c2 : "#888"}">
        <span class="mb-nm">${sub ? escHtml(sub.name) : "Sector " + subN}</span><span class="mb-cat">${sub ? sub.cat + " · " + cat.short : ""}</span></td>`;
    };
    const natCell = (uid, c, side) => c
      ? `<td class="mb-player ${side}" data-sec="country" data-uid="${uid}" style="--cc2:${c.c2}"><span class="mb-nm">National news</span><span class="mb-cat">${c.flag} ${c.name}</span></td>`
      : `<td class="mb-player ${side} empty"></td>`;

    let body = "";
    for (let i = 0; i < maxLen; i++) {
      const ls = myR[i], rs = opR[i];
      body += `<tr class="mb-row">${playerCell(ls, "left", me)}${ptsCell(ls != null ? (mySec[ls] || 0) : 0, "left")}
        <td class="mb-slot">R${i + 1}</td>${ptsCell(rs != null ? (opSec[rs] || 0) : 0, "right")}${playerCell(rs, "right", opp)}</tr>`;
    }
    body += `<tr class="mb-row mb-nat">${natCell(me, meC, "left")}${ptsCell(myNat, "left")}<td class="mb-slot">NAT</td>${ptsCell(opNat, "right")}${natCell(opp, opC, "right")}</tr>`;
    const resultLbl = opp == null ? "—" : myPts === oppPts ? "TIE" : myPts > oppPts ? "◀ WIN" : "WIN ▶";
    body += `<tr class="mb-tot"><td class="mb-player left"><span class="mb-nm">TOTAL</span></td>
      <td class="mb-pts left ${myPts < 0 ? "neg" : "pos"}">${sgn(myPts)}</td><td class="mb-slot">${resultLbl}</td>
      <td class="mb-pts right ${oppPts < 0 ? "neg" : "pos"}">${opp != null ? sgn(oppPts) : "—"}</td><td class="mb-player right"><span class="mb-nm">TOTAL</span></td></tr>`;

    $("#seasonScreen").innerHTML = `
      ${navBar("matchup")}
      <div class="hero">
        <div class="kicker">${escHtml(snap.name || "League")} · Week ${wk + 1} of ${weeks}</div>
        <div class="player-select">Matchup</div>
        <p class="lede">Head-to-head box score — every sub-sector you drafted and the points it scored this week. Higher total wins the week. Tap any sub-sector for the headlines behind its points.</p>
      </div>
      <div class="matchup">${matchPortrait(me, myPts, myPts >= oppPts)}<div class="mu-vs">VS</div>${oppSide}</div>
      <div class="section-title"><h3>Box score — week ${wk + 1}</h3><div class="rule"></div><span class="hint">Points scored per drafted sub-sector</span></div>
      <div class="mbox-wrap"><table class="mbox">
        <thead><tr><th class="mb-team left" colspan="2">${meC.flag} ${escHtml(seatName(me))}</th><th class="mb-slot">SLOT</th><th class="mb-team right" colspan="2">${opp != null ? (opC.flag + " " + escHtml(seatName(opp))) : "Bye week"}</th></tr></thead>
        <tbody>${body}</tbody>
      </table></div>
      <div style="text-align:center;margin-top:22px">
        <button class="btn ghost" id="backHome">◂ Back to league home</button>
      </div>`;
    wireNav($("#seasonScreen"));
    $$(".mb-player[data-sec]", $("#seasonScreen")).forEach(td => td.addEventListener("click", () => {
      const sec = td.dataset.sec, uid = td.dataset.uid;
      go("sector/" + sec + (uid === STATE.you ? "" : "/" + uid));
    }));
    $("#backHome").addEventListener("click", () => go("home"));
  }

  /* ============================ SECTOR DETAIL ============================ */
  function renderSectorDetail(snap, key, uid) {
    showOnly("seasonScreen");
    const me = STATE.you, who = uid || me, forOther = who !== me;
    const wk = currentWeek(snap.season), weeks = (snap.season && snap.season.weeks) || 12;
    const bd = weekBreakdown(snap, who, wk);
    const whoName = forOther ? escHtml(seatName(who)) + "’s" : "your";
    const isCountry = key === "country";
    let title, subtitle, c1 = "#888", c2 = "#aaa", ico = "", pts = 0, list = [];
    if (isCountry) {
      const c = seatCountry(who);
      title = "National news — " + c.name; subtitle = `Points ${whoName} country earned across all sub-sectors, plus double-points for partnerships with league allies.`;
      c1 = c.c1; c2 = c.c2; ico = `<div class="sd-flag">${c.flag}</div>`;
      pts = bd.country.pts; list = bd.country.events;
    } else {
      const subN = parseInt(key, 10), sub = SUB_BY_N[subN], cat = sub ? CATEGORIES[sub.cat] : null;
      const row = bd.sectors.find(s => s.subN === subN) || { pts: 0, events: [] };
      title = sub ? sub.name : "Sector " + subN; subtitle = sub ? `${sub.cat} · ${cat.short} — “${sub.tagline}”` : "";
      if (cat) { c1 = cat.c1; c2 = cat.c2; ico = iconSVG(sub.glyph, cat.c1, cat.c2, 46); }
      pts = row.pts; list = row.events;
    }
    const myCtx = ctxFor(snap, who);
    const SEC_LABELS = new Set(["Your sub-sector", "Sector × country (extra)"]);
    const items = list.length ? list.map(({ ev, pts: p }) => {
      // show only the reasons attributable to THIS bucket, so the chips reconcile with the row's points
      const det = SCORING.scoreDetail(ev, myCtx);
      const share = Math.max(1, det.matched.length);
      const parts = SCORING.breakdown(ev, myCtx)
        .filter(x => isCountry ? !SEC_LABELS.has(x.label) : SEC_LABELS.has(x.label))
        .map(x => { const v = isCountry ? x.pts : Math.round(x.pts / share); return `<span class="why-part ${v < 0 ? "neg" : "pos"}">${escHtml(x.label)} ${sgn(v)}</span>`; }).join("");
      const rp = Math.round(p);
      return `<div class="feed-row ${rp < 0 ? "neg" : rp > 0 ? "pos" : ""}">
        <div class="fr-main"><div class="fr-head">${ev.url ? `<a href="${ev.url}" target="_blank" rel="noopener">${escHtml(ev.headline)}</a>` : escHtml(ev.headline)}</div>
          <div class="fr-tags">${evTags(ev)}${ev.source ? `<span class="ev-src">${escHtml(ev.source)}</span>` : ""}</div>
          <div class="why-parts">${parts}</div></div>
        <div class="fr-pts ${rp < 0 ? "neg" : rp > 0 ? "pos" : "zero"}">${sgn(rp)}</div></div>`;
    }).join("") : `<div class="roster-empty">No headlines scored here this week.</div>`;

    $("#seasonScreen").innerHTML = `
      ${navBar("matchup")}
      <div class="sd-hero" style="--cc1:${c1};--cc2:${c2}">
        <div class="sd-ico">${ico}</div>
        <div class="sd-meta"><div class="sd-kick">Week ${wk + 1} of ${weeks} · ${whoName} contribution</div>
          <h2 class="sd-title">${escHtml(title)}</h2><div class="sd-sub">${escHtml(subtitle)}</div></div>
        <div class="sd-pts ${pts < 0 ? "neg" : pts > 0 ? "pos" : "zero"}">${sgn(pts)}<span>week pts</span></div>
      </div>
      <div class="section-title"><h3>Headlines that scored${isCountry ? "" : " here"}</h3><div class="rule"></div><span class="hint">${list.length} item${list.length === 1 ? "" : "s"} · why each counted</span></div>
      <div class="feed">${items}</div>
      <div style="text-align:center;margin-top:22px">
        <button class="btn gold" id="backMatch">◂ Back to matchup</button>
        <button class="btn ghost" id="backHome2">League home</button>
      </div>`;
    wireNav($("#seasonScreen"));
    $("#backMatch").addEventListener("click", () => go("matchup"));
    $("#backHome2").addEventListener("click", () => go("home"));
  }

  /* ---------- router ---------- */
  function parseHash() {
    const h = (location.hash || "").replace(/^#/, "");
    if (h.indexOf("sector/") === 0) { const parts = h.slice(7).split("/"); return { view: "sector", key: parts[0], uid: parts[1] }; }
    if (h === "matchup") return { view: "matchup" };
    if (h === "board") return { view: "board" };
    return { view: "home" };
  }
  function renderLeague(snap) {
    if (!snap || snap.status !== "season") return;
    const r = parseHash();
    if (r.view === "board") {
      showOnly("resultsScreen");
      const cta = $("#seasonCta");
      if (cta) { cta.innerHTML = `<div class="season-cta"><button class="btn gold" id="boardHome">◂ Back to League Home</button></div>`; const bh = document.getElementById("boardHome"); if (bh) bh.addEventListener("click", () => go("home")); }
      return;
    }
    if (r.view === "matchup") return renderMatchup(snap);
    if (r.view === "sector") return renderSectorDetail(snap, r.key, r.uid);
    return renderHome(snap);
  }
  window.addEventListener("hashchange", () => {
    const snap = STATE.league && STATE.league.snapshot;
    if (STATE.mode === "league" && snap && snap.status === "season") renderLeague(snap);
  });

  function escHtml(s) { return String(s || "").replace(/[&<>"']/g, c => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&#39;" }[c])); }

  /* back-compat + exports */
  window.renderLeague = renderLeague;
  window.renderSeason = renderLeague;   // lobby.js calls this on a season snapshot
  window.renderSeasonCta = renderSeasonCta;
  window.startSeason = startSeason;
})();
