/* ============================================================================
   ALLIED INDUSTRIAL POLICY — FANTASY DRAFT · game engine + rendering
   ============================================================================ */
const $  = (s, r = document) => r.querySelector(s);
const $$ = (s, r = document) => [...r.querySelectorAll(s)];

/* ---------- difficulty: how much scouting info is visible while drafting ----------
   easy   → card stats + your fence on every card (full info)
   normal → your fence on every card shown, card stats hidden (default)
   hard   → no stats and no fences: draft on names and judgement alone          */
function difficulty() {
  if (STATE.mode === "league") return STATE.difficulty || "normal";
  const el = $("#diffSel"); return el ? el.value : (STATE.difficulty || "normal");
}
function showScores()  { return difficulty() === "easy"; }
function showSynergy() { return difficulty() !== "hard"; }
function synergyOf(ally, sub) { return fitOf(ally, sub); }

/* a draft replayed from a league drafted before the switch holds legacy sub-sectors (ids 1-39) */
function draftIsLegacy() { return STATE.picks.some(p => p.subN != null && p.subN < 100); }
function draftPool() { return draftIsLegacy() ? LEGACY_SUBSECTORS : SUBSECTORS; }
const STAGE_TINT = { EXT: ["#78350f", "#d97706"], REF: ["#7c2d12", "#f97316"], CMP: ["#1e3a8a", "#60a5fa"],
                     ASM: ["#312e81", "#a78bfa"], DEP: ["#14532d", "#4ade80"], REC: ["#134e4a", "#2dd4bf"] };
/* the six positions of the current draft: the stages, or the legacy stack categories */
function draftGroups() {
  if (draftIsLegacy()) return Object.values(CATEGORIES).map(c => ({ id: c.id, key: c.id, short: c.short, c1: c.c1, c2: c.c2 }));
  return STAGE_ORDER.map(id => ({ id, key: STAGES[id].short, short: STAGES[id].name, c1: STAGE_TINT[id][0], c2: STAGE_TINT[id][1] }));
}

/* the fence chip of a country's rating of a card */
function fenceChip(ally, sub, withPts) {
  if (!isCard(sub)) return "";
  const r = ratingOf(ally.id, sub.n);
  if (!r || !r.choice) return `<span class="fence-chip none" title="This step is not assessed for ${ally.name}">Not rated</span>`;
  const pts = FENCE_FIT[r.choice] || 0;
  return `<span class="fence-chip f-${r.fence}" style="--fc:${CHOICE_COLOR[r.choice]}" title="${CHOICE_MEANING[r.choice]}">` +
    `${CHOICE_LABEL[r.choice]} · ${FENCE_LABEL[r.fence]}${withPts ? ` <b>${pts >= 0 ? "+" : ""}${pts}</b>` : ""}</span>`;
}

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
  gameMode: "draft",      // "draft" (Fantasy Draft) | "coordination" (Alliance Architect)
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
  const head = (t) => `<div style="margin:10px 0 4px;font-size:11px;letter-spacing:1.5px;text-transform:uppercase;color:var(--ink-faint);font-family:var(--font-disp);font-style:italic">${t}</div>`;
  // the fighter's fences: how many cards it rates High / Medium / Low, and its High and Preserve cards by name
  const rated = SUBSECTORS.map(s => ({ s, r: ratingOf(a.id, s.n) })).filter(x => x.r && x.r.fence);
  const count = (f) => rated.filter(x => x.r.fence === f).length;
  const named = (choice) => rated.filter(x => x.r.choice === choice).sort((p, q) => q.s.ovr - p.s.ovr).map(x => x.s.name);
  const fenceRow = ["high", "medium", "low"].map(f =>
    `<span class="chipcat fence-${f}">${FENCE_LABEL[f]}${syn ? " · " + count(f) : ""}</span>`).join(" ");
  const secure = named("secure"), preserve = named("preserve");
  const fenceDetail = syn
    ? head("High fence · Secure (+" + FENCE_FIT.secure + ")") +
      `<div class="bio" style="color:#f58229;font-size:12.5px">${secure.length ? secure.join(" · ") : "None: no step is a security must-have"}</div>` +
      head("Preserve (+" + FENCE_FIT.preserve + ")") +
      `<div class="bio" style="color:var(--red-2);font-size:12.5px">${preserve.length ? preserve.slice(0, 6).join(" · ") + (preserve.length > 6 ? " …" : "") : "None yet: build or partner instead"}</div>`
    : head("Hard mode") + `<div class="bio" style="font-size:12.5px">Fences hidden: draft on names and judgement alone.</div>`;
  detail.style.setProperty("--cc1", a.c1); detail.style.setProperty("--cc2", a.c2);
  detail.innerHTML = `
    <div class="dportrait"><div class="flagbg">${a.flag}</div>${portrait(a, 170)}</div>
    <div class="dbody">
      <h3>${a.flag} ${a.name}</h3>
      <div class="dtitle">${a.title} · <span style="color:var(--ink-dim)">${a.mascot}</span></div>
      <p class="bio">${a.bio}</p>
      ${head("Your fences across the " + SUBSECTORS.length + " supply-chain cards")}
      <div class="strengths" style="justify-content:flex-start">${fenceRow}</div>
      ${fenceDetail}
    </div>`;
  detail.style.display = "grid";
  $("#startBtn").disabled = false;
  const csb = $("#coordSoloBtn"); if (csb) csb.disabled = false;
  detail.scrollIntoView({ behavior: "smooth", block: "nearest" });
}

/* Rounds available depend on ally count (allies × rounds must fit the cards) */
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
  // AI values: its own fence fit + spreading across stages + a touch of variety
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
  const cats = ["ALL", ...STAGE_ORDER];
  // sort options depend on difficulty — never expose a sort that reveals hidden numbers
  let opts;
  if (showScores()) {
    opts = [["value", "Best fit for you"], ["ovr", "Overall (OVR)"], ["need", "Supply-chain need"],
            ["prize", "Prize"], ["demand", "Allied demand"], ["tech", "By technology"]];
  } else if (showSynergy()) {
    opts = [["fit", "Best fence fit for you"], ["tech", "By technology"], ["num", "By stage"]];
  } else {
    opts = [["tech", "By technology"], ["num", "By stage"]];
  }
  if (!opts.some(o => o[0] === STATE.sort)) STATE.sort = opts[0][0];
  bar.innerHTML = cats.map(c => {
    const label = c === "ALL" ? "ALL STAGES" : STAGES[c].short;
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
  const stageIdx = (s) => STAGE_ORDER.indexOf(s.cat);
  const byTech = (a, b) => (a.tech < b.tech ? -1 : a.tech > b.tech ? 1 : stageIdx(a) - stageIdx(b));
  const nz = (v) => (v == null ? -1 : v);
  const sorters = {
    value:  (a, b) => scoreFor(you, b) - scoreFor(you, a),
    fit:    (a, b) => synergyOf(you, b) - synergyOf(you, a) || byTech(a, b),
    ovr:    (a, b) => b.ovr - a.ovr,
    need:   (a, b) => nz(b.NEED) - nz(a.NEED),
    prize:  (a, b) => nz(b.PRIZE) - nz(a.PRIZE),
    demand: (a, b) => nz(b.DEMAND) - nz(a.DEMAND),
    tech:   byTech,
    num:    (a, b) => stageIdx(a) - stageIdx(b) || byTech(a, b),
  };
  list = [...list].sort(sorters[STATE.sort] || sorters.tech);
  // drafted cards sink to the bottom
  list.sort((a, b) => (STATE.drafted[a.n] ? 1 : 0) - (STATE.drafted[b.n] ? 1 : 0));

  const scores = showScores(), syn = showSynergy();
  pool.innerHTML = list.map(s => {
    const cat = catMeta(s);
    const drafted = STATE.drafted[s.n];
    const owner = drafted ? seatCountry(drafted) : null;
    const ownerNm = drafted ? seatName(drafted) : "";
    const bars = [
      ["NEED", s.NEED, "Supply-chain need: how exposed the 21 countries are here (deployment need at Deployment)"],
      ["PRIZE", s.PRIZE, "Prize: the strategic industrial value of producing here"],
      ["DEMAND", s.DEMAND, "Allied demand: share of the 21 countries whose fence is High or Medium"],
    ].filter(([, v]) => v != null)
      .map(([k, v, full]) => `<div class="statrow" title="${full}"><span>${k}</span><span class="bar"><i style="width:${v}%"></i></span><b>${v}</b></div>`).join("");
    const footLeft = syn ? `<div class="synergy">${fenceChip(you, s, true)}</div>` : "";
    return `<div class="pcard ${drafted ? "drafted" : ""} ${scores ? "" : "noscore"}" style="--cc1:${cat.c1};--cc2:${cat.c2}">
      <div class="pcard-top">
        ${iconSVG(s.glyph, cat.c1, cat.c2, 54)}
        <div class="meta">
          <div class="catline">${cat.label}</div>
          <div class="pname">${s.name}</div>
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
    rl.innerHTML = `<div class="roster-empty">Your dynasty is empty.<br>Draft your first card →</div>`;
  } else {
    rl.innerHTML = team.roster.map(n => {
      const s = SUB_BY_N[n], cat = catMeta(s);
      return `<li style="border-left:3px solid ${cat.c2}">
        <span class="ovrmini" ${showScores() ? "" : `style="color:${cat.c2}"`}>${showScores() ? s.ovr : cat.key}</span>
        <span class="rn"><div class="t">${s.name}</div><div class="c">${cat.label}</div>${showSynergy() ? fenceChip(you, s, false) : ""}</span>
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
  const legacy = draftIsLegacy();
  const sorted = [...STATE.teams].map(t => {
    if (legacy) {
      // leagues drafted before the switch: coverage across stack categories + the hand-written combos
      const cats = new Set(t.roster.map(n => SUB_BY_N[n].cat));
      t.coverage = cats.size * 5; t.combo = comboBonus(t.roster); t.partner = 0; t.cats = cats.size;
    } else {
      // portfolio bonuses: stage coverage, technology chains, and Partner cards an ally holds for you
      const others = STATE.teams.filter(o => o !== t).flatMap(o => o.roster);
      const b = portfolioBonus(seatCountry(t.ally).id, t.roster, others);
      t.coverage = b.coverage; t.combo = b.combo; t.partner = b.partner; t.cats = b.stages; t.chains = b.chains;
    }
    t.final = t.points + t.coverage + t.combo + t.partner;
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
  // model alignment: the share of picks that were the drafter's own High or Medium fence
  const fenced = legacy ? [] : STATE.picks.map(p => ratingOf(seatCountry(p.ally).id, p.subN)).filter(Boolean);
  const aligned = fenced.filter(r => r.fence === "high" || r.fence === "medium").length;
  STATE.alignPct = fenced.length ? Math.round(100 * aligned / fenced.length) : null;
  $("#resultHead").innerHTML =
    `The alliance drafted <b>${drafted}</b> of ${draftPool().length} ${legacy ? "sub-sectors" : "supply-chain cards"}.` +
    (top1 ? ` The #1 overall priority was <b>${top1.name}</b>.` : "") +
    (STATE.alignPct != null ? `<br><span style="font-size:.9em;color:var(--ink-dim)">🧭 Model alignment: <b>${STATE.alignPct}%</b> of picks were the drafter's own High or Medium fence.</span>` : "") +
    `<br><span style="font-size:.9em;color:var(--ink-dim)">🏆 Draft champion: <b style="color:var(--gold)">${seatFlag(champSeat)} ${seatName(champSeat)}</b> · You finished <b>#${yourRank + 1}</b> of ${N} (grade ${gradeFor(yourRank, N)})</span>`;

  // Alliance "power": how close the collective draft got to the maximum-OVR stack
  const K = STATE.picks.length;
  const ovrDesc = draftPool().map(s => s.ovr).sort((a, b) => b - a);
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
    <tr><th>#</th><th>Ally</th><th>Roster</th><th>${legacy ? "Sectors" : "Stages"}</th><th>Coverage</th><th>${legacy ? "Stack combo" : "Chains"}</th>${legacy ? "" : "<th>Partner</th>"}<th>Total</th><th>Grade</th></tr>
    ${sorted.map((t, i) => {
      const a = seatCountry(t.ally);
      const sub2 = STATE.mode === "league" ? seatName(t.ally) : a.mascot;
      const top = t.roster.slice(0, 3).map(n => isCard(SUB_BY_N[n]) ? SUB_BY_N[n].name : SUB_BY_N[n].name.split(",")[0]).join(" · ");
      return `<tr class="${t.ally === STATE.you ? "you" : ""}">
        <td>${i + 1}</td>
        <td><b>${a.flag} ${seatName(t.ally)}</b><br><span style="color:var(--ink-faint);font-size:11px">${STATE.mode === "league" ? a.name : a.mascot}</span></td>
        <td style="color:var(--ink-dim);font-size:12px">${top}${t.roster.length > 3 ? " …" : ""}</td>
        <td>${t.cats}/6</td>
        <td>+${t.coverage}</td>
        <td style="color:var(--leaf-2)" title="${(t.chains || []).map(c => `${c.tech}: ${c.links} link${c.links === 1 ? "" : "s"}${c.full ? " + full chain" : ""}`).join("; ")}">+${t.combo}</td>
        ${legacy ? "" : `<td style="color:#55c4c5" title="Partner cards another team holds for you">+${t.partner}</td>`}
        <td class="sc">${t.final}</td>
        <td class="grade">${gradeFor(i, N)}</td>
      </tr>`;
    }).join("")}`;
}

/* Stage demand: how much of each supply-chain stage the alliance claimed (legacy: stack category) */
function renderCategoryPriority() {
  const pickMap = {}; STATE.picks.forEach(p => pickMap[p.subN] = p);
  const pool = draftPool();
  const rows = draftGroups().map(cat => {
    const subs = pool.filter(s => s.cat === cat.id);
    const dsubs = subs.filter(s => pickMap[s.n]);
    const first = dsubs.length ? Math.min(...dsubs.map(s => pickMap[s.n].pickNo)) : null;
    const avg = dsubs.length ? dsubs.reduce((a, s) => a + pickMap[s.n].pickNo, 0) / dsubs.length : Infinity;
    return { cat, cnt: dsubs.length, total: subs.length, first, avg, share: subs.length ? dsubs.length / subs.length : 0 };
  }).sort((a, b) => b.share - a.share || a.avg - b.avg);
  $("#catPriority").innerHTML = rows.map(r => `
    <div class="catprio-item" style="--cc1:${r.cat.c1};--cc2:${r.cat.c2}">
      <div class="cp-head"><span class="cp-key">${r.cat.key}</span><span class="cp-name">${r.cat.short}</span>
        <span class="cp-count">${r.cnt}/${r.total}</span></div>
      <div class="cp-bar"><i style="width:${Math.round(r.share * 100)}%"></i></div>
      <div class="cp-foot">${r.first ? `first taken at pick #${r.first}` : "not drafted"}</div>
    </div>`).join("");
}

/* Sector priority board: every sub-sector ranked by draft order */
function renderSectorBoard() {
  const pickMap = {}; STATE.picks.forEach(p => pickMap[p.subN] = p);
  const listed = [...draftPool()].sort((a, b) => {
    const pa = pickMap[a.n], pb = pickMap[b.n];
    if (pa && pb) return pa.pickNo - pb.pickNo;
    if (pa) return -1; if (pb) return 1;
    return b.ovr - a.ovr;
  });
  let rank = 0;
  $("#sectorBoard").innerHTML = listed.map(s => {
    const cat = catMeta(s);
    const p = pickMap[s.n];
    const owner = p ? seatCountry(p.ally) : null;
    if (p) rank++;
    const isYou = p && p.ally === STATE.you;
    return `<div class="prio-row ${p ? "" : "undrafted"} ${isYou ? "you" : ""}" style="--cc1:${cat.c1};--cc2:${cat.c2}">
      <div class="prio-rank">${p ? "#" + rank : "—"}</div>
      ${iconSVG(s.glyph, cat.c1, cat.c2, 38)}
      <div class="prio-main">
        <div class="prio-name">${s.name}</div>
        <div class="prio-cat">${cat.label} · OVR ${s.ovr}${owner && isCard(s) ? " · " + fenceChip(owner, s, false) : ""}</div>
      </div>
      ${p
        ? `<div class="prio-pick">PICK ${p.pickNo}<small>Round ${p.round}</small></div>`
        : `<div class="prio-pick undrafted-tag">Passed over</div>`}
      <div class="prio-ally">${p ? `${seatFlag(p.ally)} ${seatName(p.ally)}` : "—"}</div>
    </div>`;
  }).join("");
}

/* Metric board: every card ranked by OVR with its stats and how the 21 countries fence it,
   so the group's draft-order priorities can be compared against the model. */
function renderMetricBoard() {
  const el = $("#metricBoard"); if (!el) return;
  const pickMap = {}; STATE.picks.forEach(p => pickMap[p.subN] = p);
  const listed = [...draftPool()].sort((a, b) => b.ovr - a.ovr || a.n - b.n);
  el.innerHTML = listed.map((s, i) => {
    const cat = catMeta(s);
    const p = pickMap[s.n];
    const isYou = p && p.ally === STATE.you;
    const stats = isCard(s)
      ? [["NEED", s.NEED, "Supply-chain need"], ["PRIZE", s.PRIZE, "Prize"], ["DEM", s.DEMAND, "Allied demand"]].filter(([, v]) => v != null)
      : [["NAT", s.NAT, "National Security"], ["E&E", s.ENSC, "Energy & Economic Security"], ["CLI", s.CLIM, "Climate Salience"], ["OPP", s.OPP, "Economic Opportunity"]];
    const bars = stats
      .map(([k, v, full]) => `<span class="ms-stat" title="${full}: ${v}"><span class="ms-k">${k}</span><span class="ms-bar"><i style="width:${v}%"></i></span><b>${v}</b></span>`).join("") +
      (isCard(s) ? `<span class="ms-fences" title="How the 21 countries fence this step">H ${s.nHigh} · M ${s.nMedium} · L ${s.nLow}</span>` : "");
    return `<div class="metric-row ${p ? "" : "undrafted"} ${isYou ? "you" : ""}" style="--cc1:${cat.c1};--cc2:${cat.c2}">
      <div class="mr-rank">${i + 1}</div>
      <div class="mr-main">
        <div class="mr-name">${s.name}<span class="mr-ovr">OVR ${s.ovr}</span></div>
        <div class="mr-cat">${cat.label} · ${p ? `<span class="mr-drafted">drafted #${p.pickNo}</span>` : "undrafted"}</div>
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
        <button class="btn red bc-fight" id="fightBtn">▶ Watch cinematic</button>
        <div class="bc-hint">Outcome decided by your draft</div>
      </div>
      <div class="bc-side">
        <div class="bc-portrait boss"><img class="portrait-fill" src="${FIGHT_FRAMES.L2}" alt="China — the big boss"></div>
        <div class="bc-name">🇨🇳 China · <span style="color:var(--red-2)">BIG BOSS</span></div>
      </div>
    </div>`;
  $("#fightBtn").addEventListener("click", runFight);
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
    avail.sort((a, b) => scoreFor(you, b) - scoreFor(you, a));
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

/* stack-combo synergies of the legacy sub-sectors (leagues drafted before the switch; the cards'
   chains are computed in cards.js portfolioBonus) */
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
  ["setupScreen", "lobbyScreen", "draftScreen", "resultsScreen", "seasonScreen", "coordScreen"].forEach(s => {
    const el = $("#" + s); if (el) el.classList.toggle("active", s === screenId);
  });
  window.scrollTo({ top: 0 });
}
function restart() {
  if (STATE.mode === "league" && typeof leaveLeague === "function") { leaveLeague(); return; }
  STATE.mode = "solo";
  showOnly("setupScreen");
  STATE.you = STATE.pickAlly = null; $("#startBtn").disabled = true;
  const csb = $("#coordSoloBtn"); if (csb) csb.disabled = true;
  $("#allyDetail").style.display = "none";
  $$(".ally-card").forEach(c => c.classList.remove("selected"));
}

/* ========================= game-mode picker =========================
   Two modes share the whole setup screen (fighter grid, name, league create /
   join). Only the mode-specific settings row swaps out. */
function setGameMode(mode) {
  STATE.gameMode = mode === "coordination" ? "coordination" : "draft";
  const coord = STATE.gameMode === "coordination";
  $$(".mode-card").forEach(c => c.classList.toggle("selected", c.dataset.mode === STATE.gameMode));
  const d = $("#draftSetupActions"), c = $("#coordSetupActions");
  if (d) d.style.display = coord ? "none" : "";
  if (c) c.style.display = coord ? "" : "none";
  const hint = $("#mpHint");
  if (hint) hint.textContent = coord
    ? "Create a room · share the link · negotiate together live (3–6 countries)"
    : "Create a league · share the link · draft together live";
  const cb = $("#createLeagueBtn");
  if (cb) cb.textContent = coord ? "Create Alliance Room ▶" : "Create League ▶";
  document.body.classList.toggle("coord-mode", coord);
  // the marquee follows the mode
  const bt = $("#brandTitle"), bs = $("#brandSub"), bp = $("#brandPill");
  const title = (typeof COORD_DATA !== "undefined") ? COORD_DATA.COORD_CONFIG.title : "Alliance Architect";
  if (bt) bt.textContent = coord ? "Allied Industrial Policy — " + title : "Allied Industrial Policy — Fantasy Draft";
  if (bs) bs.textContent = coord ? "Coordinative optimisation · negotiated specialization" : "Six-stage supply chains · Season 2";
  if (bp) {
    const cfg = (typeof COORD_DATA !== "undefined") ? COORD_DATA.COORD_CONFIG : { minPlayers: 3, maxPlayers: 6 };
    const nNodes = (typeof COORD_DATA !== "undefined") ? COORD_DATA.getScenario("batteries").nodes.length : 9;
    bp.innerHTML = coord
      ? `${cfg.minPlayers}–${cfg.maxPlayers} countries · <b>${nNodes}</b> chain nodes`
      : `21 Fighters · <b>${SUBSECTORS.length}</b> supply-chain cards`;
  }
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
  $$(".mode-card").forEach(card => card.addEventListener("click", () => setGameMode(card.dataset.mode)));
  if (typeof COORD_DATA !== "undefined") {
    const t = $("#coordModeTitle"); if (t) t.textContent = COORD_DATA.COORD_CONFIG.title;
  }
});
