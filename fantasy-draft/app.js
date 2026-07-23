/* ============================================================================
   ALLIED INDUSTRIAL POLICY — FANTASY DRAFT · game engine + rendering
   ============================================================================ */
const $  = (s, r = document) => r.querySelector(s);
const $$ = (s, r = document) => [...r.querySelectorAll(s)];

const STATE = {
  you: null,              // ally id
  teams: [],              // {ally, roster:[subN], spent, pointsBudget}
  order: [],              // draft order (ally ids) for round 1
  rounds: 6,
  cap: 220,               // salary cap credits per team
  pickIndex: 0,           // global pick counter
  drafted: {},            // subN -> ally id
  filter: "ALL",
  sort: "value",
  log: [],
  autodelay: 620,
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
  $$(".ally-card").forEach(c => c.classList.toggle("selected", c.dataset.id === id));
  const a = ALLY_BY_ID[id];
  const detail = $("#allyDetail");
  const strengths = Object.entries(a.home).sort((x, y) => y[1] - x[1])
    .map(([c, v]) => `<span class="chipcat" style="background:${CATEGORIES[c].c2}">${CATEGORIES[c].short} +${v}</span>`).join(" ");
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
      <div style="margin:10px 0 4px;font-size:11px;letter-spacing:1.5px;text-transform:uppercase;color:var(--ink-faint);font-family:var(--font-disp);font-style:italic">Signature sub-sectors (+6 fit)</div>
      <div class="bio" style="color:var(--red-2);font-size:12.5px">${sigs}</div>
    </div>`;
  detail.style.display = "grid";
  $("#startBtn").disabled = false;
  detail.scrollIntoView({ behavior: "smooth", block: "nearest" });
}

/* ========================= DRAFT SETUP ========================= */
function startDraft() {
  STATE.rounds   = parseInt($("#roundsSel").value, 10);
  STATE.cap      = parseInt($("#capSel").value, 10);
  STATE.autodelay = $("#speedSel").value === "fast" ? 230 : $("#speedSel").value === "slow" ? 1000 : 620;

  // teams in a randomized-but-deterministic order (seeded by 'you' position)
  const ids = ALLIES.map(a => a.id);
  // rotate so 'you' isn't always first; simple shuffle without Math.random
  const seed = ids.indexOf(STATE.you) + 3;
  const shuffled = [...ids];
  for (let i = shuffled.length - 1; i > 0; i--) {
    const j = (i * seed + 7) % (i + 1);
    [shuffled[i], shuffled[j]] = [shuffled[j], shuffled[i]];
  }
  STATE.order = shuffled;
  STATE.teams = shuffled.map(id => ({ ally: id, roster: [], spent: 0, points: 0 }));
  STATE.pickIndex = 0;
  STATE.drafted = {};
  STATE.log = [];

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

/* affordability: this pick must still leave enough cap to fill remaining slots
   with the cheapest sub-sectors currently on the board (prevents cap soft-lock) */
function canAfford(team, sub) {
  const slotsLeft = STATE.rounds - team.roster.length;      // includes this pick
  if (slotsLeft <= 0) return false;
  const others = SUBSECTORS
    .filter(s => !STATE.drafted[s.n] && s.n !== sub.n)
    .map(s => s.cost).sort((a, b) => a - b);
  let reserve = 0;
  for (let i = 0; i < slotsLeft - 1 && i < others.length; i++) reserve += others[i];
  return team.spent + sub.cost + reserve <= STATE.cap;
}
/* any affordable, undrafted pick left for this team? */
function hasAffordable(team) {
  return SUBSECTORS.some(s => !STATE.drafted[s.n] && canAfford(team, s));
}

/* ========================= DRAFT FLOW ========================= */
function advance() {
  if (STATE.pickIndex >= totalPicks()) return finishDraft();
  const id = currentAllyId();
  updateOnClock(id);
  if (id === STATE.you) {
    renderPool();          // enable your pick buttons
  } else {
    setTimeout(() => aiPick(id), STATE.autodelay);
    renderPool();
  }
}

function aiPick(id) {
  const team = teamOf(id);
  const ally = ALLY_BY_ID[id];
  const avail = SUBSECTORS.filter(s => !STATE.drafted[s.n] && canAfford(team, s));
  if (!avail.length) { commitPick(id, null); return; }
  // AI values: synergy score + slight category-need + a touch of variety
  const catCount = {};
  team.roster.forEach(n => { const c = SUB_BY_N[n].cat; catCount[c] = (catCount[c] || 0) + 1; });
  let best = null, bestV = -1;
  avail.forEach(s => {
    let v = scoreFor(ally, s);
    v -= (catCount[s.cat] || 0) * 4;                 // encourage diversification
    v -= s.cost * 0.05;                              // mild value sense
    v += ((s.n * (id.charCodeAt(0) + id.charCodeAt(1))) % 5) * 0.6;  // deterministic jitter
    if (v > bestV) { bestV = v; best = s; }
  });
  commitPick(id, best.n);
}

function draftByYou(subN) {
  const id = STATE.you;
  if (currentAllyId() !== id) return;
  const team = teamOf(id);
  const sub = SUB_BY_N[subN];
  if (STATE.drafted[subN]) return;
  if (!canAfford(team, sub)) { toast("Not enough cap — keep 6 credits per remaining pick."); return; }
  commitPick(id, subN);
}

function commitPick(id, subN) {
  const team = teamOf(id);
  const ally = ALLY_BY_ID[id];
  if (subN != null) {
    const sub = SUB_BY_N[subN];
    STATE.drafted[subN] = id;
    team.roster.push(subN);
    team.spent += sub.cost;
    const pts = scoreFor(ally, sub);
    team.points += pts;
    const syn = pts - sub.ovr;
    STATE.log.unshift({
      round: currentRound(), ally: id, subN,
      you: id === STATE.you, syn,
    });
  } else {
    STATE.log.unshift({ round: currentRound(), ally: id, subN: null, you: id === STATE.you });
  }
  STATE.pickIndex++;
  renderRail();
  renderPool();
  advance();
}

/* ========================= RENDER: on-clock ========================= */
function updateOnClock(id) {
  const a = ALLY_BY_ID[id];
  const oc = $("#onclock");
  oc.classList.toggle("you", id === STATE.you);
  oc.innerHTML = `
    ${portrait(a, 52, "chip")}
    <div><div class="lbl">${id === STATE.you ? "Your pick — Fight!" : "On the clock"} · Round ${currentRound()}/${STATE.rounds}</div>
    <div class="who">${a.flag} ${a.name} ${id === STATE.you ? "<span class='you-tag'>· YOU</span>" : ""}</div></div>`;
  $("#roundPill").innerHTML = `Pick <b>${Math.min(STATE.pickIndex + 1, totalPicks())}</b> / ${totalPicks()}`;
}

/* ========================= RENDER: filters ========================= */
function renderFilters() {
  const bar = $("#filters");
  const cats = ["ALL", ...Object.keys(CATEGORIES)];
  bar.innerHTML = cats.map(c => {
    const label = c === "ALL" ? "ALL SECTORS" : `${c} · ${CATEGORIES[c].short}`;
    return `<button class="fbtn ${c === STATE.filter ? "on" : ""}" data-f="${c}">${label}</button>`;
  }).join("") + `<span class="spacer"></span>
    <select id="sortSel" title="Sort">
      <option value="value">Sort: Best fit for you</option>
      <option value="ovr">Sort: Overall (OVR)</option>
      <option value="nat">Sort: National Security</option>
      <option value="ensc">Sort: Energy &amp; Economic Security</option>
      <option value="clim">Sort: Climate Salience</option>
      <option value="opp">Sort: Economic Opportunity</option>
      <option value="cost">Sort: Cheapest</option>
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
  const you = ALLY_BY_ID[STATE.you];
  const yourTurn = currentAllyId() === STATE.you && STATE.pickIndex < totalPicks();
  const team = teamOf(STATE.you);

  let list = SUBSECTORS.filter(s => STATE.filter === "ALL" || s.cat === STATE.filter);
  const sorters = {
    value:  (a, b) => scoreFor(you, b) - scoreFor(you, a),
    ovr:    (a, b) => b.ovr - a.ovr,
    nat:    (a, b) => b.NAT - a.NAT,
    ensc:   (a, b) => b.ENSC - a.ENSC,
    clim:   (a, b) => b.CLIM - a.CLIM,
    opp:    (a, b) => b.OPP - a.OPP,
    cost:   (a, b) => a.cost - b.cost,
  };
  list = [...list].sort(sorters[STATE.sort]);
  // drafted cards sink to the bottom
  list.sort((a, b) => (STATE.drafted[a.n] ? 1 : 0) - (STATE.drafted[b.n] ? 1 : 0));

  pool.innerHTML = list.map(s => {
    const cat = CATEGORIES[s.cat];
    const drafted = STATE.drafted[s.n];
    const fit = scoreFor(you, s) - s.ovr;
    const affordable = team && canAfford(team, s);
    const owner = drafted ? ALLY_BY_ID[drafted] : null;
    const bars = [
      ["NAT SEC", s.NAT, "National Security"],
      ["E&E SEC", s.ENSC, "Energy & Economic Security"],
      ["CLIMATE", s.CLIM, "Climate Salience"],
      ["ECON OPP", s.OPP, "Economic Opportunity"],
    ].map(([k, v, full]) => `<div class="statrow" title="${full}"><span>${k}</span><span class="bar"><i style="width:${v}%"></i></span><b>${v}</b></div>`).join("");
    return `<div class="pcard ${drafted ? "drafted" : ""}" style="--cc1:${cat.c1};--cc2:${cat.c2}">
      <div class="pcard-top">
        ${iconSVG(s.glyph, cat.c1, cat.c2, 54)}
        <div class="meta">
          <div class="catline">${s.cat} · ${cat.short}</div>
          <div class="pname">${s.n}. ${s.name}</div>
          <div class="tagline">“${s.tagline}”</div>
        </div>
        <div class="ovr-badge"><div class="num">${s.ovr}</div><div class="lbl">OVR</div><div><span class="tier ${s.tier}">TIER ${s.tier}</span></div></div>
      </div>
      <div class="statbars">${bars}</div>
      <div class="pcard-foot">
        <div>
          <div class="cost">◈ ${s.cost} <small>credits</small></div>
          ${fit > 0 ? `<div class="synergy">↑ +${fit} home-fit for ${you.flag}</div>` : ""}
        </div>
        ${drafted
          ? `<div style="font-size:12px;color:var(--ink-faint);text-align:right">Drafted by<br><b style="color:var(--ink)">${owner.flag} ${owner.name}</b></div>`
          : `<button class="btn pick ${affordable ? "red" : "ghost"}" data-n="${s.n}" ${yourTurn && affordable ? "" : "disabled"}>${yourTurn ? (affordable ? "SELECT ▸" : "Over cap") : "Wait…"}</button>`}
      </div>
    </div>`;
  }).join("");

  // pass fallback: your turn but nothing affordable / nothing left
  if (yourTurn && !hasAffordable(team)) {
    const banner = document.createElement("div");
    banner.style.cssText = "grid-column:1/-1;display:flex;gap:14px;align-items:center;justify-content:space-between;flex-wrap:wrap;padding:16px 18px;border-radius:16px;background:rgba(255,176,32,.12);border:1px solid var(--line-2)";
    banner.innerHTML = `<span style="color:var(--ink-dim);font-size:14px">No sub-sector fits your remaining cap of <b style="color:var(--gold)">◈ ${STATE.cap - team.spent}</b>. You can pass this pick.</span>`;
    const pb = document.createElement("button");
    pb.className = "btn gold"; pb.textContent = "Pass this pick ▸";
    pb.addEventListener("click", () => commitPick(STATE.you, null));
    banner.appendChild(pb);
    pool.prepend(banner);
  }

  $$(".btn.pick", pool).forEach(b => b.addEventListener("click", () => draftByYou(parseInt(b.dataset.n, 10))));
}

/* ========================= RENDER: right rail ========================= */
function renderRail() {
  const team = teamOf(STATE.you);
  const you = ALLY_BY_ID[STATE.you];
  const capPct = Math.min(100, (team.spent / STATE.cap) * 100);
  $("#capFill").style.width = capPct + "%";
  $("#capNums").innerHTML = `Spent <b>◈ ${team.spent}</b> · Cap ◈ ${STATE.cap} · Left ◈ ${STATE.cap - team.spent}`;

  // roster
  const rl = $("#rosterList");
  if (!team.roster.length) {
    rl.innerHTML = `<div class="roster-empty">Your dynasty is empty.<br>Draft your first sub-sector →</div>`;
  } else {
    rl.innerHTML = team.roster.map(n => {
      const s = SUB_BY_N[n], cat = CATEGORIES[s.cat];
      return `<li style="border-left:3px solid ${cat.c2}">
        <span class="ovrmini">${s.ovr}</span>
        <span class="rn"><div class="t">${s.name}</div><div class="c">${s.cat} · ${cat.short}</div></span>
      </li>`;
    }).join("");
  }
  $("#rosterCount").textContent = `${team.roster.length}/${STATE.rounds}`;
  $("#teamPoints").textContent = team.points;

  // log
  $("#logList").innerHTML = STATE.log.slice(0, 40).map(e => {
    const a = ALLY_BY_ID[e.ally];
    if (e.subN == null) return `<li class="${e.you ? "you" : ""}">R${e.round} — <b>${a.flag} ${a.name}</b> passed (cap)</li>`;
    const s = SUB_BY_N[e.subN];
    return `<li class="${e.you ? "you" : ""}">R${e.round} — <b>${a.flag} ${a.name}</b> drafted <b>${s.name}</b>${e.syn > 0 ? ` <span style="color:var(--leaf-2)">(+${e.syn} fit)</span>` : ""}</li>`;
  }).join("");

  // live standings
  const sorted = [...STATE.teams].sort((x, y) => y.points - x.points);
  $("#standings").innerHTML = sorted.map((t, i) => {
    const a = ALLY_BY_ID[t.ally];
    return `<li class="${t.ally === STATE.you ? "you" : ""}"><span class="rk">${i + 1}</span>${a.flag} ${a.name}<span class="sc">${t.points}</span></li>`;
  }).join("");
}

/* ========================= RESULTS ========================= */
function finishDraft() {
  updateOnClock(STATE.you);
  const sorted = [...STATE.teams].map(t => {
    // portfolio bonuses: coverage across categories + a couple of "stack combos"
    const cats = new Set(t.roster.map(n => SUB_BY_N[n].cat));
    const coverage = cats.size * 5;
    const combo = comboBonus(t.roster);
    t.final = t.points + coverage + combo;
    t.coverage = coverage; t.combo = combo; t.cats = cats.size;
    return t;
  }).sort((a, b) => b.final - a.final);

  const grades = ["A+", "A", "A-", "B+", "B", "B", "B-", "C+", "C", "C", "C-", "D"];
  $("#setupScreen").classList.remove("active");
  $("#draftScreen").classList.remove("active");
  $("#resultsScreen").classList.add("active");
  window.scrollTo({ top: 0 });

  const podium = [sorted[1], sorted[0], sorted[2]];
  const posClass = ["p2", "p1", "p3"];
  $("#podium").innerHTML = podium.map((t, i) => {
    const a = ALLY_BY_ID[t.ally];
    return `<div class="slot ${posClass[i]}" style="--cc1:${a.c1};--cc2:${a.c2}">
      ${i === 1 ? '<div class="winner-banner">Winner</div>' : ""}
      <div class="pod-portrait"><div class="flagbg">${a.flag}</div>${portrait(a, i === 1 ? 150 : 120)}</div>
      <div class="plabel">
        <div class="rank">${i === 1 ? "🥇" : i === 0 ? "🥈" : "🥉"}</div>
        <div class="pname2">${a.flag} ${a.name}</div>
        <div class="score">${t.final} PTS</div>
      </div>
    </div>`;
  }).join("");

  const yourRank = sorted.findIndex(t => t.ally === STATE.you);
  $("#resultHead").innerHTML = yourRank === 0
    ? `🏆 <b>${ALLY_BY_ID[STATE.you].name}</b> wins the Allied Industrial Draft!`
    : `You finished <b>#${yourRank + 1}</b> of ${sorted.length} — grade <span class="grade" style="color:var(--gold)">${grades[yourRank]}</span>`;

  $("#resultsTable").innerHTML = `
    <tr><th>#</th><th>Ally</th><th>Roster</th><th>Sectors</th><th>Coverage</th><th>Stack combo</th><th>Total</th><th>Grade</th></tr>
    ${sorted.map((t, i) => {
      const a = ALLY_BY_ID[t.ally];
      const top = t.roster.slice(0, 3).map(n => SUB_BY_N[n].name.split(",")[0]).join(", ");
      return `<tr class="${t.ally === STATE.you ? "you" : ""}">
        <td>${i + 1}</td>
        <td><b>${a.flag} ${a.name}</b><br><span style="color:var(--ink-faint);font-size:11px">${a.mascot}</span></td>
        <td style="color:var(--ink-dim);font-size:12px">${top}${t.roster.length > 3 ? " …" : ""}</td>
        <td>${t.cats}/6</td>
        <td>+${t.coverage}</td>
        <td style="color:var(--leaf-2)">+${t.combo}</td>
        <td class="sc">${t.final}</td>
        <td class="grade">${grades[i]}</td>
      </tr>`;
    }).join("")}`;
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

function restart() {
  $("#resultsScreen").classList.remove("active");
  $("#setupScreen").classList.add("active");
  STATE.you = null; $("#startBtn").disabled = true;
  $("#allyDetail").style.display = "none";
  $$(".ally-card").forEach(c => c.classList.remove("selected"));
  window.scrollTo({ top: 0 });
}

/* ========================= boot ========================= */
window.addEventListener("DOMContentLoaded", () => {
  makeSky();
  renderSetup();
  $("#startBtn").addEventListener("click", startDraft);
  $("#restartBtn").addEventListener("click", restart);
  $("#draftAgainBtn").addEventListener("click", restart);
});
