/* ============================================================================
   ALLIED INDUSTRIAL POLICY — FANTASY DRAFT · game engine + rendering
   ============================================================================ */
const $  = (s, r = document) => r.querySelector(s);
const $$ = (s, r = document) => [...r.querySelectorAll(s)];

/* ---------- difficulty: how much scouting info is visible while drafting ----------
   easy   → sector scores + home-turf synergies both shown (full info)
   normal → home-turf synergies shown, sector scores hidden (default)
   hard   → all numerical info hidden                                              */
function difficulty() {
  if (STATE.mode === "league") return STATE.difficulty || "normal";
  const el = $("#diffSel"); return el ? el.value : (STATE.difficulty || "normal");
}
function showScores()  { return difficulty() === "easy"; }
function showSynergy() { return difficulty() !== "hard"; }
function synergyOf(ally, sub) { return (ally.home[sub.cat] || 0) + (ally.picks.includes(sub.n) ? 6 : 0); }

/* ---------- seat helpers (a "seat" is a draft slot: a country in solo, a player in league) ---------- */
function seatCountry(seatId) {
  if (STATE.mode === "league") { const m = STATE.seatMeta[seatId]; return ALLY_BY_ID[(m && m.allyId) || "US"] || ALLY_BY_ID.US; }
  return ALLY_BY_ID[seatId];
}
function seatName(seatId) {
  if (STATE.mode === "league") { const m = STATE.seatMeta[seatId]; return (m && m.name) || "Player"; }
  const a = ALLY_BY_ID[seatId]; return a ? a.name : seatId;
}
function seatFlag(seatId) { const c = seatCountry(seatId); return c ? c.flag : "🏳️"; }
function myCountry() { return seatCountry(STATE.you); }

/* Alliance must reach this share of the max-possible stack to beat the big boss.
   Near-optimal collective drafting lands ~0.89-0.99; sloppy drafting ~0.63-0.80,
   so 0.85 rewards prioritizing high-value sub-sectors. */
const WIN_THRESHOLD = 0.85;

const STATE = {
  mode: "solo",           // "solo" (vs AI) | "league" (multiplayer)
  you: null,              // seat id (country id in solo, my uid in league)
  pickAlly: null,         // the country the local player chose on the fighter grid
  teams: [],              // {ally:seatId, roster:[subN], points}
  order: [],              // draft order (seat ids)
  numAllies: 12,          // teams in the draft (solo)
  rounds: 3,
  pickIndex: 0,           // global pick counter
  drafted: {},            // subN -> seat id
  seatMeta: {},           // league: seatId(uid) -> {allyId, name}
  league: null,           // league: {adapter, snapshot, host, isHost, id, resultsShown}
  filter: "ALL",
  sort: "value",
  log: [],
  picks: [],
  autodelay: 620,
  pickSeconds: 60,        // seconds allowed per pick (0 = no clock)
};

/* ---------- floating spores ---------- */
function makeSky() {
  const sky = $("#sky");
  const n = window.innerWidth < 620 ? 14 : 26;
  for (let i = 0; i < n; i++) {
    const s = document.createElement("i");
    s.className = "spore";
    const size = 5 + (i * 7 % 13);
    s.style.left = (i * 97 % 100) + "%";
    s.style.width = s.style.height = size + "px";
    s.style.animationDuration = (11 + (i * 13 % 16)) + "s";
    s.style.animationDelay = (-(i * 17 % 20)) + "s";
    s.style.opacity = 0.2 + (i % 4) * 0.08;
    sky.appendChild(s);
  }
}

/* ========================= SETUP SCREEN ========================= */
function renderSetup() {
  const grid = $("#allyGrid");
  grid.innerHTML = ALLIES.map(a => {
    return `<div class="ally-card" data-id="${a.id}" style="--cc1:${a.c1};--cc2:${a.c2}">
      <div class="ready-stamp">Ready!</div>
      <div class="tile-portrait">
        <div class="flagbg">${a.flag}</div>
        ${portrait(a, 118)}
      </div>
      <div class="label">
        <div class="cname">${a.name}</div>
        <div class="ctitle">${a.title}</div>
      </div>
    </div>`;
  }).join("");

  $$(".ally-card", grid).forEach(card =>
    card.addEventListener("click", () => selectAlly(card.dataset.id)));
}

function selectAlly(id) {
  STATE.you = id;
  STATE.pickAlly = id;
  if (typeof onFighterPicked === "function") onFighterPicked(id);
  $$(".ally-card").forEach(c => c.classList.toggle("selected", c.dataset.id === id));
  const a = ALLY_BY_ID[id];
  const detail = $("#allyDetail");
  const syn = showSynergy();
  const strengths = Object.entries(a.home).sort((x, y) => y[1] - x[1])
    .map(([c, v]) => `<span class="chipcat" style="background:${CATEGORIES[c].c2}">${CATEGORIES[c].short}${syn ? " +" + v : ""}</span>`).join(" ");
  const sigs = a.picks.map(n => SUB_BY_N[n].name).join(" · ");
  detail.style.setProperty("--cc1", a.c1); detail.style.setProperty("--cc2", a.c2);
  detail.innerHTML = `
    <div class="dportrait"><div class="flagbg">${a.flag}</div>${portrait(a, 170)}</div>
    <div class="dbody">
      <h3>${a.flag} ${a.name}</h3>
      <div class="dtitle">${a.title} · <span style="color:var(--ink-dim)">${a.mascot}</span></div>
      <p class="bio">${a.bio}</p>
      <div style="margin:10px 0 4px;font-size:11px;letter-spacing:1.5px;text-transform:uppercase;color:var(--ink-faint);font-family:var(--font-disp);font-style:italic">Home-turf synergies</div>
      <div class="strengths" style="justify-content:flex-start">${strengths}</div>
      <div style="margin:10px 0 4px;font-size:11px;letter-spacing:1.5px;text-transform:uppercase;color:var(--ink-faint);font-family:var(--font-disp);font-style:italic">Signature sub-sectors${syn ? " (+6 fit)" : ""}</div>
      <div class="bio" style="color:var(--red-2);font-size:12.5px">${sigs}</div>
    </div>`;
  detail.style.display = "grid";
  $("#startBtn").disabled = false;
  detail.scrollIntoView({ behavior: "smooth", block: "nearest" });
}

/* Rounds available depend on ally count (allies × rounds must fit 39 sub-sectors) */
function populateRounds() {
  const n = parseInt($("#alliesSel").value, 10);
  const maxR = Math.min(6, Math.max(1, Math.floor(SUBSECTORS.length / n)));
  const prev = parseInt($("#roundsSel").value, 10) || Math.min(3, maxR);
  const keep = Math.min(prev, maxR);
  let opts = "";
  for (let r = 1; r <= maxR; r++)
    opts += `<option value="${r}" ${r === keep ? "selected" : ""}>${r} · ${n * r} total picks</option>`;
  $("#roundsSel").innerHTML = opts;
}

/* Letter grade scaled to any field size (rank i of n, 0-based) */
function gradeFor(i, n) {
  const g = ["A+", "A", "A-", "B+", "B", "B-", "C+", "C", "C-", "D+", "D"];
  const p = n <= 1 ? 0 : i / (n - 1);
  return g[Math.min(g.length - 1, Math.round(p * (g.length - 1)))];
}

/* ========================= DRAFT SETUP ========================= */
function startDraft() {
  STATE.mode = "solo";
  STATE.you = STATE.pickAlly;
  STATE.numAllies = parseInt($("#alliesSel").value, 10);
  STATE.rounds   = parseInt($("#roundsSel").value, 10);
  STATE.difficulty = $("#diffSel").value;
  STATE.autodelay = $("#speedSel").value === "fast" ? 230 : $("#speedSel").value === "slow" ? 1000 : 620;
  STATE.pickSeconds = $("#clockSel") ? parseInt($("#clockSel").value, 10) : 60;

  // participants: always include your fighter, then fill from roster order
  const rest = ALLIES.map(a => a.id).filter(id => id !== STATE.you);
  const participants = [STATE.you, ...rest].slice(0, STATE.numAllies);

  // randomized-but-deterministic draft order (seeded by 'you'; no Math.random)
  const seed = ALLIES.findIndex(a => a.id === STATE.you) + 3;
  const shuffled = [...participants];
  for (let i = shuffled.length - 1; i > 0; i--) {
    const j = (i * seed + 7) % (i + 1);
    [shuffled[i], shuffled[j]] = [shuffled[j], shuffled[i]];
  }
  STATE.order = shuffled;
  STATE.teams = shuffled.map(id => ({ ally: id, roster: [], points: 0 }));
  STATE.pickIndex = 0;
  STATE.drafted = {};
  STATE.log = [];
  STATE.picks = [];

  $("#setupScreen").classList.remove("active");
  $("#draftScreen").classList.add("active");
  window.scrollTo({ top: 0 });
  renderFilters();
  renderRail();
  renderPool();
  advance();
}

/* current pick: snake order */
function currentAllyId() {
  const T = STATE.teams.length;
  const round = Math.floor(STATE.pickIndex / T);       // 0-based
  const posInRound = STATE.pickIndex % T;
  const idx = round % 2 === 0 ? posInRound : (T - 1 - posInRound);
  return STATE.order[idx];
}
function currentRound() { return Math.floor(STATE.pickIndex / STATE.teams.length) + 1; }
function totalPicks() { return STATE.teams.length * STATE.rounds; }
function teamOf(id) { return STATE.teams.find(t => t.ally === id); }

/* ========================= DRAFT FLOW ========================= */
function advance() {
  clearTurnTimer();
  if (STATE.pickIndex >= totalPicks()) return finishDraft();
  const id = currentAllyId();
  updateOnClock(id);
  if (id === STATE.you) {
    renderPool();          // enable your pick buttons
    armTurnTimer();        // start your pick clock
  } else {
    setTimeout(() => aiPick(id), STATE.autodelay);
    renderPool();
  }
}

function aiPick(id) {
  const team = teamOf(id);
  const ally = ALLY_BY_ID[id];
  const avail = SUBSECTORS.filter(s => !STATE.drafted[s.n]);
  if (!avail.length) { commitPick(id, null); return; }
  // AI values: synergy score + slight category-need + a touch of variety
  const catCount = {};
  team.roster.forEach(n => { const c = SUB_BY_N[n].cat; catCount[c] = (catCount[c] || 0) + 1; });
  let best = null, bestV = -1;
  avail.forEach(s => {
    let v = scoreFor(ally, s);
    v -= (catCount[s.cat] || 0) * 4;                 // encourage diversification
    v += ((s.n * (id.charCodeAt(0) + id.charCodeAt(1))) % 5) * 0.6;  // deterministic jitter
    if (v > bestV) { bestV = v; best = s; }
  });
  commitPick(id, best.n);
}

function draftByYou(subN) {
  const id = STATE.you;
  if (currentAllyId() !== id) return;
  if (STATE.drafted[subN]) return;
  if (STATE.mode === "league") {
    clearTurnTimer();
    const ctx = { seat: id, pickIndex: STATE.pickIndex, total: totalPicks(), round: currentRound() };
    STATE.league.adapter.makePick(subN, ctx);   // snapshot listener applies the result
    return;
  }
  commitPick(id, subN);
}

function commitPick(id, subN) {
  const team = teamOf(id);
  const ally = ALLY_BY_ID[id];
  if (subN != null) {
    const sub = SUB_BY_N[subN];
    STATE.drafted[subN] = id;
    team.roster.push(subN);
    const pts = scoreFor(ally, sub);
    team.points += pts;
    const syn = pts - sub.ovr;
    const pickNo = STATE.picks.length + 1;
    STATE.picks.push({ pickNo, round: currentRound(), ally: id, subN });
    STATE.log.unshift({ pickNo, round: currentRound(), ally: id, subN, you: id === STATE.you, syn });
  }
  STATE.pickIndex++;
  renderRail();
  renderPool();
  advance();
}

/* ========================= RENDER: on-clock ========================= */
function updateOnClock(id) {
  const a = seatCountry(id);
  const nm = STATE.mode === "league" ? seatName(id) : a.name;
  const oc = $("#onclock");
  oc.classList.toggle("you", id === STATE.you);
  oc.innerHTML = `
    ${portrait(a, 52, "chip")}
    <div><div class="lbl">${id === STATE.you ? "Your pick — go!" : "On the clock"} · Round ${currentRound()}/${STATE.rounds}</div>
    <div class="who">${a.flag} ${nm} ${id === STATE.you ? "<span class='you-tag'>· YOU</span>" : ""}</div></div>`;
  $("#roundPill").innerHTML = `Pick <b>${Math.min(STATE.pickIndex + 1, totalPicks())}</b> / ${totalPicks()}`;
}

/* ========================= RENDER: filters ========================= */
function renderFilters() {
  const bar = $("#filters");
  const cats = ["ALL", ...Object.keys(CATEGORIES)];
  // sort options depend on difficulty — never expose a sort that reveals hidden numbers
  let opts;
  if (showScores()) {
    opts = [["value", "Best fit for you"], ["ovr", "Overall (OVR)"], ["nat", "National Security"],
            ["ensc", "Energy &amp; Economic Security"], ["clim", "Climate Salience"], ["opp", "Economic Opportunity"], ["num", "By sector #"]];
  } else if (showSynergy()) {
    opts = [["fit", "Best home-fit for you"], ["num", "By sector #"]];
  } else {
    opts = [["num", "By sector #"], ["cat", "By stack category"]];
  }
  if (!opts.some(o => o[0] === STATE.sort)) STATE.sort = opts[0][0];
  bar.innerHTML = cats.map(c => {
    const label = c === "ALL" ? "ALL SECTORS" : `${c} · ${CATEGORIES[c].short}`;
    return `<button class="fbtn ${c === STATE.filter ? "on" : ""}" data-f="${c}">${label}</button>`;
  }).join("") + `<span class="spacer"></span>
    <select id="sortSel" title="Sort">
      ${opts.map(([v, l]) => `<option value="${v}">Sort: ${l}</option>`).join("")}
    </select>`;
  $$(".fbtn", bar).forEach(b => b.addEventListener("click", () => {
    STATE.filter = b.dataset.f; renderFilters(); renderPool();
  }));
  $("#sortSel").value = STATE.sort;
  $("#sortSel").addEventListener("change", e => { STATE.sort = e.target.value; renderPool(); });
}

/* ========================= RENDER: sub-sector pool ========================= */
function renderPool() {
  const pool = $("#pool");
  const you = myCountry();
  const yourTurn = currentAllyId() === STATE.you && STATE.pickIndex < totalPicks();

  let list = SUBSECTORS.filter(s => STATE.filter === "ALL" || s.cat === STATE.filter);
  const sorters = {
    value:  (a, b) => scoreFor(you, b) - scoreFor(you, a),
    fit:    (a, b) => synergyOf(you, b) - synergyOf(you, a) || a.n - b.n,
    ovr:    (a, b) => b.ovr - a.ovr,
    nat:    (a, b) => b.NAT - a.NAT,
    ensc:   (a, b) => b.ENSC - a.ENSC,
    clim:   (a, b) => b.CLIM - a.CLIM,
    opp:    (a, b) => b.OPP - a.OPP,
    num:    (a, b) => a.n - b.n,
    cat:    (a, b) => (a.cat < b.cat ? -1 : a.cat > b.cat ? 1 : a.n - b.n),
  };
  list = [...list].sort(sorters[STATE.sort] || sorters.num);
  // drafted cards sink to the bottom
  list.sort((a, b) => (STATE.drafted[a.n] ? 1 : 0) - (STATE.drafted[b.n] ? 1 : 0));

  const scores = showScores(), syn = showSynergy();
  pool.innerHTML = list.map(s => {
    const cat = CATEGORIES[s.cat];
    const drafted = STATE.drafted[s.n];
    const fit = synergyOf(you, s);
    const owner = drafted ? seatCountry(drafted) : null;
    const ownerNm = drafted ? seatName(drafted) : "";
    const bars = [
      ["NAT SEC", s.NAT, "National Security"],
      ["E&E SEC", s.ENSC, "Energy & Economic Security"],
      ["CLIMATE", s.CLIM, "Climate Salience"],
      ["ECON OPP", s.OPP, "Economic Opportunity"],
    ].map(([k, v, full]) => `<div class="statrow" title="${full}"><span>${k}</span><span class="bar"><i style="width:${v}%"></i></span><b>${v}</b></div>`).join("");
    const footLeft = syn
      ? (fit > 0 ? `<div class="synergy">↑ +${fit} home-fit for ${you.flag} ${you.name}</div>` : `<div class="synergy" style="color:var(--ink-faint)">Open pick</div>`)
      : "";
    return `<div class="pcard ${drafted ? "drafted" : ""} ${scores ? "" : "noscore"}" style="--cc1:${cat.c1};--cc2:${cat.c2}">
      <div class="pcard-top">
        ${iconSVG(s.glyph, cat.c1, cat.c2, 54)}
        <div class="meta">
          <div class="catline">${s.cat} · ${cat.short}</div>
          <div class="pname">${s.n}. ${s.name}</div>
          <div class="tagline">“${s.tagline}”</div>
        </div>
        ${scores ? `<div class="ovr-badge"><div class="num">${s.ovr}</div><div class="lbl">OVR</div><div><span class="tier ${s.tier}">TIER ${s.tier}</span></div></div>` : ""}
      </div>
      ${scores ? `<div class="statbars">${bars}</div>` : ""}
      <div class="pcard-foot">
        <div>${footLeft}</div>
        ${drafted
          ? `<div class="drafted-by">Drafted by<br><b>${owner.flag} ${ownerNm}</b></div>`
          : `<button class="btn pick red" data-n="${s.n}" ${yourTurn ? "" : "disabled"}>${yourTurn ? "SELECT ▸" : "Wait…"}</button>`}
      </div>
    </div>`;
  }).join("");

  $$(".btn.pick", pool).forEach(b => b.addEventListener("click", () => draftByYou(parseInt(b.dataset.n, 10))));
}

/* ========================= RENDER: right rail ========================= */
function renderRail() {
  const team = teamOf(STATE.you);
  const you = myCountry();

  // roster
  const rl = $("#rosterList");
  if (!team.roster.length) {
    rl.innerHTML = `<div class="roster-empty">Your dynasty is empty.<br>Draft your first sub-sector →</div>`;
  } else {
    rl.innerHTML = team.roster.map(n => {
      const s = SUB_BY_N[n], cat = CATEGORIES[s.cat];
      return `<li style="border-left:3px solid ${cat.c2}">
        <span class="ovrmini" ${showScores() ? "" : `style="color:${cat.c2}"`}>${showScores() ? s.ovr : s.cat}</span>
        <span class="rn"><div class="t">${s.name}</div><div class="c">${s.cat} · ${cat.short}</div></span>
      </li>`;
    }).join("");
  }
  $("#rosterCount").textContent = `${team.roster.length}/${STATE.rounds}`;
  $("#teamPoints").textContent = team.points;

  // log
  $("#logList").innerHTML = STATE.log.slice(0, 40).map(e => {
    const flag = seatFlag(e.ally), nm = seatName(e.ally);
    if (e.subN == null) return `<li class="${e.you ? "you" : ""}">R${e.round} — <b>${flag} ${nm}</b> passed</li>`;
    const s = SUB_BY_N[e.subN];
    return `<li class="${e.you ? "you" : ""}">R${e.round} — <b>${flag} ${nm}</b> drafted <b>${s.name}</b>${(showSynergy() && e.syn > 0) ? ` <span style="color:var(--red-2)">(+${e.syn} fit)</span>` : ""}</li>`;
  }).join("");

  // live standings
  const sorted = [...STATE.teams].sort((x, y) => y.points - x.points);
  $("#standings").innerHTML = sorted.map((t, i) => {
    return `<li class="${t.ally === STATE.you ? "you" : ""}"><span class="rk">${i + 1}</span>${seatFlag(t.ally)} ${seatName(t.ally)}<span class="sc">${t.points}</span></li>`;
  }).join("");
}

/* ========================= RESULTS ========================= */
function finishDraft() {
  if (STATE.mode === "league") {
    if (STATE.league.resultsShown) return;
    STATE.league.resultsShown = true;
  }
  const sorted = [...STATE.teams].map(t => {
    // portfolio bonuses: coverage across categories + a couple of "stack combos"
    const cats = new Set(t.roster.map(n => SUB_BY_N[n].cat));
    const coverage = cats.size * 5;
    const combo = comboBonus(t.roster);
    t.final = t.points + coverage + combo;
    t.coverage = coverage; t.combo = combo; t.cats = cats.size;
    return t;
  }).sort((a, b) => b.final - a.final);

  const N = sorted.length;
  $("#setupScreen").classList.remove("active");
  $("#draftScreen").classList.remove("active");
  $("#resultsScreen").classList.add("active");
  window.scrollTo({ top: 0 });

  const champSeat = sorted[0].ally;
  const yourRank = sorted.findIndex(t => t.ally === STATE.you);
  const drafted = STATE.picks.length;
  const top1 = STATE.picks[0] ? SUB_BY_N[STATE.picks[0].subN] : null;
  $("#resultHead").innerHTML =
    `The alliance drafted <b>${drafted}</b> of ${SUBSECTORS.length} sub-sectors.` +
    (top1 ? ` The #1 overall priority was <b>${top1.name}</b>.` : "") +
    `<br><span style="font-size:.9em;color:var(--ink-dim)">🏆 Draft champion: <b style="color:var(--gold)">${seatFlag(champSeat)} ${seatName(champSeat)}</b> · You finished <b>#${yourRank + 1}</b> of ${N} (grade ${gradeFor(yourRank, N)})</span>`;

  // Alliance "power": how close the collective draft got to the maximum-OVR stack
  const K = STATE.picks.length;
  const ovrDesc = SUBSECTORS.map(s => s.ovr).sort((a, b) => b - a);
  const maxSum = ovrDesc.slice(0, K).reduce((a, b) => a + b, 0) || 1;
  const minSum = ovrDesc.slice(-K).reduce((a, b) => a + b, 0) || 0;
  const actualSum = STATE.picks.reduce((a, p) => a + SUB_BY_N[p.subN].ovr, 0);
  STATE.powerRatio = (actualSum - minSum) / (maxSum - minSum || 1);   // normalized 0..1
  STATE.powerPct = Math.round(STATE.powerRatio * 100);
  STATE.allianceWin = STATE.powerRatio >= WIN_THRESHOLD;

  renderCategoryPriority();
  renderSectorBoard();
  renderMetricBoard();
  renderBossChallenge();

  $("#resultsTable").innerHTML = `
    <tr><th>#</th><th>Ally</th><th>Roster</th><th>Sectors</th><th>Coverage</th><th>Stack combo</th><th>Total</th><th>Grade</th></tr>
    ${sorted.map((t, i) => {
      const a = seatCountry(t.ally);
      const sub2 = STATE.mode === "league" ? seatName(t.ally) : a.mascot;
      const top = t.roster.slice(0, 3).map(n => SUB_BY_N[n].name.split(",")[0]).join(", ");
      return `<tr class="${t.ally === STATE.you ? "you" : ""}">
        <td>${i + 1}</td>
        <td><b>${a.flag} ${seatName(t.ally)}</b><br><span style="color:var(--ink-faint);font-size:11px">${STATE.mode === "league" ? a.name : a.mascot}</span></td>
        <td style="color:var(--ink-dim);font-size:12px">${top}${t.roster.length > 3 ? " …" : ""}</td>
        <td>${t.cats}/6</td>
        <td>+${t.coverage}</td>
        <td style="color:var(--leaf-2)">+${t.combo}</td>
        <td class="sc">${t.final}</td>
        <td class="grade">${gradeFor(i, N)}</td>
      </tr>`;
    }).join("")}`;
}

/* Category demand: how much of each stack tier the alliance claimed */
function renderCategoryPriority() {
  const pickMap = {}; STATE.picks.forEach(p => pickMap[p.subN] = p);
  const rows = Object.values(CATEGORIES).map(cat => {
    const subs = SUBSECTORS.filter(s => s.cat === cat.id);
    const dsubs = subs.filter(s => pickMap[s.n]);
    const first = dsubs.length ? Math.min(...dsubs.map(s => pickMap[s.n].pickNo)) : null;
    const avg = dsubs.length ? dsubs.reduce((a, s) => a + pickMap[s.n].pickNo, 0) / dsubs.length : Infinity;
    return { cat, cnt: dsubs.length, total: subs.length, first, avg, share: dsubs.length / subs.length };
  }).sort((a, b) => b.share - a.share || a.avg - b.avg);
  $("#catPriority").innerHTML = rows.map(r => `
    <div class="catprio-item" style="--cc1:${r.cat.c1};--cc2:${r.cat.c2}">
      <div class="cp-head"><span class="cp-key">${r.cat.id}</span><span class="cp-name">${r.cat.short}</span>
        <span class="cp-count">${r.cnt}/${r.total}</span></div>
      <div class="cp-bar"><i style="width:${Math.round(r.share * 100)}%"></i></div>
      <div class="cp-foot">${r.first ? `first taken at pick #${r.first}` : "not drafted"}</div>
    </div>`).join("");
}

/* Sector priority board: every sub-sector ranked by draft order */
function renderSectorBoard() {
  const pickMap = {}; STATE.picks.forEach(p => pickMap[p.subN] = p);
  const listed = [...SUBSECTORS].sort((a, b) => {
    const pa = pickMap[a.n], pb = pickMap[b.n];
    if (pa && pb) return pa.pickNo - pb.pickNo;
    if (pa) return -1; if (pb) return 1;
    return b.ovr - a.ovr;
  });
  let rank = 0;
  $("#sectorBoard").innerHTML = listed.map(s => {
    const cat = CATEGORIES[s.cat];
    const p = pickMap[s.n];
    const owner = p ? seatCountry(p.ally) : null;
    if (p) rank++;
    const isYou = p && p.ally === STATE.you;
    return `<div class="prio-row ${p ? "" : "undrafted"} ${isYou ? "you" : ""}" style="--cc1:${cat.c1};--cc2:${cat.c2}">
      <div class="prio-rank">${p ? "#" + rank : "—"}</div>
      ${iconSVG(s.glyph, cat.c1, cat.c2, 38)}
      <div class="prio-main">
        <div class="prio-name">${s.name}</div>
        <div class="prio-cat">${s.cat} · ${cat.short} · OVR ${s.ovr}</div>
      </div>
      ${p
        ? `<div class="prio-pick">PICK ${p.pickNo}<small>Round ${p.round}</small></div>`
        : `<div class="prio-pick undrafted-tag">Passed over</div>`}
      <div class="prio-ally">${p ? `${seatFlag(p.ally)} ${seatName(p.ally)}` : "—"}</div>
    </div>`;
  }).join("");
}

/* Metric board: every sub-sector ranked by OVR with its four OSI pillar scores,
   so the group's draft-order priorities can be compared against the raw numbers. */
function renderMetricBoard() {
  const el = $("#metricBoard"); if (!el) return;
  const pickMap = {}; STATE.picks.forEach(p => pickMap[p.subN] = p);
  const listed = [...SUBSECTORS].sort((a, b) => b.ovr - a.ovr || a.n - b.n);
  el.innerHTML = listed.map((s, i) => {
    const cat = CATEGORIES[s.cat];
    const p = pickMap[s.n];
    const isYou = p && p.ally === STATE.you;
    const bars = [["NAT", s.NAT, "National Security"], ["E&E", s.ENSC, "Energy & Economic Security"], ["CLI", s.CLIM, "Climate Salience"], ["OPP", s.OPP, "Economic Opportunity"]]
      .map(([k, v, full]) => `<span class="ms-stat" title="${full}: ${v}"><span class="ms-k">${k}</span><span class="ms-bar"><i style="width:${v}%"></i></span><b>${v}</b></span>`).join("");
    return `<div class="metric-row ${p ? "" : "undrafted"} ${isYou ? "you" : ""}" style="--cc1:${cat.c1};--cc2:${cat.c2}">
      <div class="mr-rank">${i + 1}</div>
      <div class="mr-main">
        <div class="mr-name">${s.name}<span class="mr-ovr">OVR ${s.ovr}</span></div>
        <div class="mr-cat">${s.cat} · ${cat.short} · ${p ? `<span class="mr-drafted">drafted #${p.pickNo}</span>` : "undrafted"}</div>
        <div class="mr-stats">${bars}</div>
      </div>
    </div>`;
  }).join("");
}

/* ========================= BIG BOSS FIGHT ========================= */
function renderBossChallenge() {
  const you = myCountry();
  const el = $("#bossChallenge");
  el.innerHTML = `
    <div class="bc-inner">
      <div class="bc-side">
        <div class="bc-portrait" style="--cc1:${you.c1};--cc2:${you.c2}"><div class="flagbg">${you.flag}</div>${portrait(you, 130)}</div>
        <div class="bc-name">${you.flag} ${STATE.mode === "league" ? seatName(STATE.you) : you.name}</div>
      </div>
      <div class="bc-mid">
        <div class="bc-power">Alliance power<br><b>${STATE.powerPct}%</b><span>boosts your fighter's health</span></div>
        <button class="btn red bc-fight" id="arcadeBtn">🕹 Fight the Dragon</button>
        <button class="btn ghost bc-cine" id="fightBtn">▶ Watch cinematic</button>
        <div class="bc-hint">Playable · WASD + arrow keys</div>
      </div>
      <div class="bc-side">
        <div class="bc-portrait boss"><img class="portrait-fill" src="${FIGHT_FRAMES.L2}" alt="China — the big boss"></div>
        <div class="bc-name">🇨🇳 China · <span style="color:var(--red-2)">BIG BOSS</span></div>
      </div>
    </div>`;
  $("#fightBtn").addEventListener("click", runFight);
  $("#arcadeBtn").addEventListener("click", () => startArcade(myCountry().id, STATE.powerPct));
}

/* ---------- pick clock: visible countdown during your pick (solo + league) ----------
   Duration = STATE.pickSeconds (host-set in league, setup-set in solo; 0 = no clock).
   On expiry it auto-picks your best available sub-sector so a slow/idle player can't
   stall the room. */
let _turnTimer = null, _pickTick = null, _pickDeadline = 0;
function fmtClock(s) { const m = Math.floor(s / 60), ss = s % 60; return m + ":" + (ss < 10 ? "0" : "") + ss; }
function clearTurnTimer() {
  if (_turnTimer) { clearTimeout(_turnTimer); _turnTimer = null; }
  if (_pickTick) { clearInterval(_pickTick); _pickTick = null; }
  const el = $("#pickClock"); if (el) { el.hidden = true; el.classList.remove("low"); }
}
function armTurnTimer() {
  clearTurnTimer();
  const secs = STATE.pickSeconds || 0;
  if (secs <= 0) return;                                             // clock disabled
  if (currentAllyId() !== STATE.you || STATE.pickIndex >= totalPicks()) return;
  const el = $("#pickClock"), t = $("#pickClockTime");
  _pickDeadline = Date.now() + secs * 1000;
  const paint = () => {
    const rem = Math.max(0, Math.round((_pickDeadline - Date.now()) / 1000));
    if (t) t.textContent = fmtClock(rem);
    if (el) el.classList.toggle("low", rem <= 10);
  };
  if (el) el.hidden = false;
  paint();
  _pickTick = setInterval(paint, 250);
  _turnTimer = setTimeout(() => {
    clearTurnTimer();
    if (currentAllyId() !== STATE.you || STATE.pickIndex >= totalPicks()) return;
    const you = myCountry();
    const avail = SUBSECTORS.filter(s => !STATE.drafted[s.n]);
    if (!avail.length) return;
    avail.sort((a, b) => (synergyOf(you, b) + b.ovr * 0.01) - (synergyOf(you, a) + a.ovr * 0.01));
    draftByYou(avail[0].n);
    toast("Auto-picked (time up)");
  }, secs * 1000);
}

const FIGHT_CAPS = {
  win:  ["Round 1 — Idle / Standoff", "China Attacks", "Allies Defend Together", "China Weakens", "China Returns to Human", "Allies Win!"],
  lose: ["Round 1 — Idle / Standoff", "China Powers Up", "Transform into Dragon", "Full Dragon", "Dragon Attack", "China Wins"],
};

function runFight() {
  const win = STATE.allianceWin;
  const frames = win ? ["W1", "W2", "W3", "W4", "W5", "W6"] : ["L1", "L2", "L3", "L4", "L5", "L6"];
  const caps = win ? FIGHT_CAPS.win : FIGHT_CAPS.lose;
  const ov = $("#fightOverlay");
  ov.innerHTML = `
    <div class="fight-arena">
      <div class="film">
        <img class="framebg" id="frameBg" alt="">
        <img class="frame" id="frameA" alt="">
        <img class="frame" id="frameB" alt="">
      </div>
      <div class="film-cap" id="filmCap"></div>
      <div class="film-dots" id="filmDots">${frames.map(() => "<i></i>").join("")}</div>
      <button class="fight-x" id="fightX" title="Skip to result" aria-label="Skip to result">✕</button>
      <div class="fight-result" id="fightResult"></div>
    </div>`;
  ov.classList.add("show");
  document.body.classList.add("scrolllock");

  const A = $("#frameA"), B = $("#frameB"), bg = $("#frameBg"), cap = $("#filmCap"), dots = $$("#filmDots i");
  let showA = true;
  const setFrame = (i) => {
    const incoming = showA ? A : B, outgoing = showA ? B : A;
    incoming.src = FIGHT_FRAMES[frames[i]];
    bg.src = FIGHT_FRAMES[frames[i]];
    incoming.classList.add("on"); outgoing.classList.remove("on");
    showA = !showA;
    cap.textContent = caps[i];
    dots.forEach((d, di) => d.classList.toggle("done", di <= i));
  };

  let ended = false;
  const end = () => { if (ended) return; ended = true; showFightResult(win); };
  $("#fightX").addEventListener("click", () => { setFrame(frames.length - 1); end(); });

  setFrame(0);
  const reduce = window.matchMedia("(prefers-reduced-motion: reduce)").matches;
  if (reduce) { setTimeout(() => { setFrame(frames.length - 1); end(); }, 400); return; }

  let i = 1;
  const tick = () => {
    if (ended) return;
    if (i >= frames.length) { setTimeout(end, 1200); return; }
    setFrame(i);
    // punch-zoom on the action frames
    const cur = showA ? B : A;
    cur.classList.remove("punch"); void cur.offsetWidth; cur.classList.add("punch");
    i++;
    setTimeout(tick, 1050);
  };
  setTimeout(tick, 1050);
}

function showFightResult(win) {
  const r = $("#fightResult");
  if (r.classList.contains("show")) return;
  r.innerHTML = `
    <div class="fr-tag ${win ? "win" : "lose"}">${win ? "Victory" : "Defeat"}</div>
    <div class="fr-power">Alliance power <b>${STATE.powerPct}%</b> · needed <b>${Math.round(WIN_THRESHOLD * 100)}%</b> to beat China</div>
    <div class="fr-actions">
      <button class="btn gold" id="fightRematch">Fight again ▶</button>
      <button class="btn ghost" id="fightClose">Back to the board</button>
    </div>`;
  r.classList.add("show");
  $("#fightRematch").addEventListener("click", runFight);
  $("#fightClose").addEventListener("click", closeFight);
}

function closeFight() {
  const ov = $("#fightOverlay");
  ov.classList.remove("show"); ov.innerHTML = "";
  document.body.classList.remove("scrolllock");
}

/* stack-combo synergies: reward vertically-integrated portfolios */
const COMBOS = [
  { name: "Full battery stack", cats: [], subs: [2, 14, 16], bonus: 14 },
  { name: "Silicon-to-chip", subs: [7, 8, 10], bonus: 16 },
  { name: "Magnet-to-motor", subs: [4, 18, 19], bonus: 12 },
  { name: "Grid backbone", subs: [20, 22, 23], bonus: 12 },
  { name: "Firm-power core", subs: [9, 29], bonus: 10 },
  { name: "Solar chain", subs: [7, 26], bonus: 8 },
  { name: "Hydrogen chain", subs: [37, 38], bonus: 8 },
  { name: "EV chain", subs: [16, 32], bonus: 8 },
];
function comboBonus(roster) {
  const set = new Set(roster);
  let b = 0;
  COMBOS.forEach(c => { if (c.subs.every(n => set.has(n))) b += c.bonus; });
  return b;
}

/* ========================= misc ========================= */
let toastT;
function toast(msg) {
  const t = $("#toast"); t.textContent = msg; t.classList.add("show");
  clearTimeout(toastT); toastT = setTimeout(() => t.classList.remove("show"), 2200);
}

function showOnly(screenId) {
  ["setupScreen", "lobbyScreen", "draftScreen", "resultsScreen", "seasonScreen"].forEach(s => {
    const el = $("#" + s); if (el) el.classList.toggle("active", s === screenId);
  });
  window.scrollTo({ top: 0 });
}
function restart() {
  if (STATE.mode === "league" && typeof leaveLeague === "function") { leaveLeague(); return; }
  STATE.mode = "solo";
  showOnly("setupScreen");
  STATE.you = STATE.pickAlly = null; $("#startBtn").disabled = true;
  $("#allyDetail").style.display = "none";
  $$(".ally-card").forEach(c => c.classList.remove("selected"));
}

/* ========================= boot ========================= */
window.addEventListener("DOMContentLoaded", () => {
  makeSky();
  renderSetup();
  populateRounds();
  $("#alliesSel").addEventListener("change", populateRounds);
  $("#diffSel").addEventListener("change", () => { if (STATE.you) selectAlly(STATE.you); });
  $("#startBtn").addEventListener("click", startDraft);
  $("#restartBtn").addEventListener("click", restart);
  $("#draftAgainBtn").addEventListener("click", restart);
});
