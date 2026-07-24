/* ============================================================================
   ARCADE DUEL — a playable 90s-style fighting mini-game.
   Your Allied fighter vs the Chinese Dragon boss.
   Controls:  A/D move · W jump · S crouch · ← punch · → kick · ↑ energy blast · ↓ block
   Exposed:   window.startArcade(allyId, powerPct)
   Self-contained Canvas 2D; uses the ally's colors + portrait (from AVATARS) for the HUD.
   ============================================================================ */
(function () {
  const W = 960, H = 540, GROUND = 476;
  const GAMEKEYS = new Set(["KeyA", "KeyD", "KeyW", "KeyS", "ArrowLeft", "ArrowRight", "ArrowUp", "ArrowDown"]);

  let cv, ctx, raf, overlay, endPanel, last = 0;
  let ally, avatarImg = null, powerPct = 80;
  let P, D, blasts, fires, parts, G;
  const held = Object.create(null);

  /* ---------------- helpers ---------------- */
  const clamp = (v, a, b) => v < a ? a : v > b ? b : v;
  const sign = (v) => v < 0 ? -1 : v > 0 ? 1 : 0;
  function circleRect(cx, cy, cr, rx, ry, rw, rh) {
    const nx = clamp(cx, rx, rx + rw), ny = clamp(cy, ry, ry + rh);
    const dx = cx - nx, dy = cy - ny; return dx * dx + dy * dy <= cr * cr;
  }
  function rectsOverlap(a, b) {
    return a.x < b.x + b.w && a.x + a.w > b.x && a.y < b.y + b.h && a.y + a.h > b.y;
  }
  const lerpHex = (h) => h; // colors passed through

  /* ---------------- lifecycle ---------------- */
  function startArcade(allyId, pPct) {
    ally = (typeof ALLY_BY_ID !== "undefined" && ALLY_BY_ID[allyId]) || { name: "Ally", flag: "🏳️", c1: "#2b6fff", c2: "#7db4ff" };
    powerPct = clamp(pPct == null ? 80 : pPct, 0, 100);
    overlay = document.getElementById("arcadeOverlay");
    overlay.innerHTML = `
      <div class="arc-wrap">
        <canvas id="arcCanvas" width="${W}" height="${H}" tabindex="0"></canvas>
        <button class="arc-x" id="arcX" aria-label="Quit fight">✕</button>
        <div class="arc-controls">
          <span><b>A</b>/<b>D</b> Move</span><span><b>W</b> Jump</span><span><b>S</b> Crouch</span>
          <span><b>←</b> Punch</span><span><b>→</b> Kick</span><span><b>↑</b> Blast</span><span><b>↓</b> Block</span>
        </div>
        <div class="arc-touch" id="arcTouch"></div>
        <div class="arc-end" id="arcEnd"></div>
      </div>`;
    overlay.classList.add("show");
    document.body.classList.add("scrolllock");
    cv = document.getElementById("arcCanvas"); ctx = cv.getContext("2d");
    endPanel = document.getElementById("arcEnd");
    document.getElementById("arcX").onclick = stopArcade;
    avatarImg = null;
    if (typeof AVATARS !== "undefined" && AVATARS[allyId]) { avatarImg = new Image(); avatarImg.src = AVATARS[allyId]; }
    buildTouch();
    initGame();
    window.addEventListener("keydown", onKey, true);
    window.addEventListener("keyup", onKey, true);
    last = performance.now();
    raf = requestAnimationFrame(loop);
    setTimeout(() => cv && cv.focus(), 30);
  }
  function stopArcade() {
    cancelAnimationFrame(raf);
    window.removeEventListener("keydown", onKey, true);
    window.removeEventListener("keyup", onKey, true);
    for (const k in held) held[k] = false;
    if (overlay) { overlay.classList.remove("show"); overlay.innerHTML = ""; }
    document.body.classList.remove("scrolllock");
  }

  /* ---------------- input ---------------- */
  function onKey(e) {
    if (!GAMEKEYS.has(e.code)) return;
    e.preventDefault();
    if (e.type === "keydown") { if (!held[e.code]) { held[e.code] = true; press(e.code); } }
    else held[e.code] = false;
  }
  function press(code) {
    if (!G || G.phase !== "fight" || P.hp <= 0 || D.hp <= 0) return;
    if (code === "KeyW") jump();
    else if (code === "ArrowLeft") attack("punch");
    else if (code === "ArrowRight") attack("kick");
    else if (code === "ArrowUp") attack("blast");
  }
  function buildTouch() {
    const t = document.getElementById("arcTouch");
    const btns = [
      ["KeyA", "◀"], ["KeyD", "▶"], ["KeyW", "▲"], ["KeyS", "▼"],
      ["ArrowLeft", "P"], ["ArrowRight", "K"], ["ArrowUp", "✦"], ["ArrowDown", "⛊"],
    ];
    t.innerHTML = btns.map(([c, l]) => `<button class="tbtn" data-c="${c}">${l}</button>`).join("");
    t.querySelectorAll(".tbtn").forEach(b => {
      const c = b.dataset.c;
      const on = (e) => { e.preventDefault(); if (!held[c]) { held[c] = true; press(c); } };
      const off = (e) => { e.preventDefault(); held[c] = false; };
      b.addEventListener("touchstart", on, { passive: false });
      b.addEventListener("touchend", off, { passive: false });
      b.addEventListener("touchcancel", off, { passive: false });
      b.addEventListener("mousedown", on); b.addEventListener("mouseup", off); b.addEventListener("mouseleave", off);
    });
  }

  /* ---------------- init ---------------- */
  function initGame() {
    const bonus = Math.round(Math.max(0, powerPct - 55) * 0.7);   // draft-power HP bonus
    P = {
      x: 260, y: GROUND, vx: 0, vy: 0, facing: 1, onGround: true,
      maxHp: 120 + bonus, hp: 120 + bonus,
      state: "idle", stT: 0, atk: null, atkT: 0, active: false, hasHit: false,
      hitstun: 0, invuln: 0, blastCd: 0, blastDmg: 14 + Math.round(bonus / 3),
      walk: 0, flash: 0,
    };
    D = {
      hx: 740, hy: GROUND - 210, vx: 0, vy: 0, maxHp: 168, hp: 168,
      segs: [], state: "approach", stT: 0, mouth: 0, hasHit: false, hitFlash: 0,
      cd: 90, bob: 0, facing: -1,
    };
    for (let i = 0; i < 16; i++) D.segs.push({ x: D.hx + i * 22, y: D.hy });
    blasts = []; fires = []; parts = [];
    G = { phase: "intro", t: 0, timer: 60, timerF: 0, shake: 0, announce: "ROUND 1", flash: 0, result: null };
  }

  /* ---------------- player actions ---------------- */
  function canAct() { return P.hitstun <= 0 && P.state !== "ko" && (P.atk === null); }
  function jump() {
    if (P.onGround && canAct() && !held.KeyS) { P.vy = -13.2; P.onGround = false; P.state = "jump"; }
  }
  function attack(type) {
    if (type === "blast") {
      if (P.blastCd > 0 || !canAct()) return;
      P.atk = "blast"; P.atkT = 0; P.hasHit = false; P.active = false; P.blastCd = 78;
      return;
    }
    if (!canAct()) return;
    P.atk = type; P.atkT = 0; P.hasHit = false; P.active = false;
  }

  /* ---------------- update ---------------- */
  function update(dt) {
    G.t += dt;
    if (G.shake > 0) G.shake = Math.max(0, G.shake - dt * 2.2);
    if (G.flash > 0) G.flash = Math.max(0, G.flash - dt * 3);

    if (G.phase === "intro") {
      if (G.t > 55 && G.announce === "ROUND 1") { G.announce = "FIGHT!"; G.flash = 1; }
      if (G.t > 105) { G.phase = "fight"; G.announce = ""; }
      updateDragonIdle(dt);
      return;
    }
    if (G.phase === "over") { updateParticles(dt); return; }

    // timer
    G.timerF += dt; if (G.timerF >= 60) { G.timerF -= 60; G.timer = Math.max(0, G.timer - 1); }
    updatePlayer(dt);
    updateDragon(dt);
    updateBlasts(dt);
    updateFires(dt);
    updateParticles(dt);
    checkEnd();
  }

  function updatePlayer(dt) {
    if (P.blastCd > 0) P.blastCd -= dt;
    if (P.invuln > 0) P.invuln -= dt;
    if (P.flash > 0) P.flash -= dt * 3;
    // face the dragon when free
    if (canAct() && P.onGround) P.facing = sign(D.hx - P.x) || P.facing;

    if (P.hitstun > 0) { P.hitstun -= dt; }
    const acting = P.atk !== null;
    const blocking = held.ArrowDown && P.onGround && !acting && P.hitstun <= 0;
    const crouching = held.KeyS && P.onGround && !acting && !blocking && P.hitstun <= 0;

    // horizontal movement
    if (P.hitstun <= 0 && !acting && !blocking && !crouching && P.state !== "ko") {
      let mv = 0;
      if (held.KeyA) mv -= 1;
      if (held.KeyD) mv += 1;
      P.vx = mv * 3.4;
      if (P.onGround) P.state = mv ? "walk" : "idle";
      if (mv) P.walk += dt * 0.35;
    } else if (P.onGround && P.hitstun <= 0) {
      P.vx *= 0.6;
    }
    if (blocking) { P.state = "block"; P.vx = 0; }
    if (crouching) { P.state = "crouch"; P.vx = 0; }

    // gravity
    P.vy += 0.62 * dt;
    P.x += P.vx * dt; P.y += P.vy * dt;
    P.x = clamp(P.x, 40, W - 40);
    if (P.y >= GROUND) { P.y = GROUND; P.vy = 0; if (!P.onGround) { P.onGround = true; if (!acting) P.state = "idle"; } }
    else P.onGround = false;

    // attack timeline
    if (acting) {
      P.atkT += dt;
      const air = !P.onGround;
      if (P.atk === "punch") {
        P.active = P.atkT >= 4 && P.atkT <= 10;
        if (P.active && !P.hasHit) meleeHit(66, -84, 30, "punch", 7, 3.5);
        if (P.atkT > 17) endAttack();
      } else if (P.atk === "kick") {
        P.active = P.atkT >= 6 && P.atkT <= 16;
        const reach = air ? 60 : 90, yo = air ? -70 : -58;
        if (P.active && !P.hasHit) meleeHit(reach, yo, 34, "kick", air ? 12 : 11, air ? 5 : 6.5);
        if (P.atkT > (air ? 22 : 26)) endAttack();
      } else if (P.atk === "blast") {
        if (P.atkT >= 10 && !P.hasHit) {
          P.hasHit = true;
          blasts.push({ x: P.x + P.facing * 44, y: P.y - 78, vx: P.facing * 8.4, r: 17, dmg: P.blastDmg, life: 110, dir: P.facing });
          G.shake = Math.max(G.shake, 0.5);
        }
        if (P.atkT > 26) endAttack();
      }
      P.state = P.atk;
    }
    if (P.hp <= 0 && P.state !== "ko") { P.state = "ko"; P.atk = null; }
  }
  function endAttack() { P.atk = null; P.active = false; P.state = P.onGround ? "idle" : "jump"; }

  function meleeHit(reach, yoff, r, kind, dmg, kb) {
    const hx = P.x + P.facing * reach, hy = P.y + yoff;
    // vs dragon head + front segments
    let hit = false;
    if (dist2(hx, hy, D.hx, D.hy) < (r + 34) * (r + 34)) hit = true;
    else for (let i = 0; i < 5 && !hit; i++) { const s = D.segs[i]; if (dist2(hx, hy, s.x, s.y) < (r + 24) * (r + 24)) hit = true; }
    if (hit && D.hitFlash <= 0.35) {
      P.hasHit = true;
      damageDragon(dmg, kb * P.facing, kind);
      spawnHits(hx, hy, kind === "kick" ? "#ffd23e" : "#fff");
    }
  }
  function dist2(ax, ay, bx, by) { const dx = ax - bx, dy = ay - by; return dx * dx + dy * dy; }

  function damageDragon(dmg, kbx, kind) {
    D.hp = Math.max(0, D.hp - dmg);
    D.hitFlash = 1; D.vx += kbx * 0.6; G.shake = Math.max(G.shake, kind === "kick" ? 0.9 : 0.6);
    // interrupt light attacks sometimes
    if (D.state !== "ko" && Math.random() < 0.3 && D.state !== "breathe") { D.state = "recover"; D.stT = 0; D.cd = 46; }
    if (D.hp <= 0) { D.state = "ko"; D.stT = 0; }
  }

  /* ---------------- dragon ---------------- */
  function updateDragonIdle(dt) {
    D.bob += dt * 0.05;
    D.hy = GROUND - 210 + Math.sin(D.bob) * 14;
    trailSegs(dt);
  }
  function trailSegs(dt) {
    let px = D.hx, py = D.hy;
    for (let i = 0; i < D.segs.length; i++) {
      const s = D.segs[i]; const dx = px - s.x, dy = py - s.y; const d = Math.hypot(dx, dy) || 1;
      const target = 22; const f = (d - target) / d;
      s.x += dx * f * 0.5; s.y += dy * f * 0.5;
      px = s.x; py = s.y;
    }
  }
  function updateDragon(dt) {
    if (D.hitFlash > 0) D.hitFlash -= dt * 0.09;
    D.facing = sign(P.x - D.hx) || -1;
    D.bob += dt * 0.05;
    D.mouth += ((D.state === "breathe" || D.state === "bite" ? 1 : 0) - D.mouth) * 0.2 * dt;
    const dist = Math.abs(P.x - D.hx);
    const aggro = 1 - D.hp / D.maxHp; // 0..1

    if (D.state === "ko") { D.hy += 2 * dt; D.hx += D.vx * dt; D.vx *= 0.9; trailSegs(dt); return; }

    D.stT += dt;
    if (D.state === "approach") {
      const homeY = GROUND - 200 + Math.sin(D.bob) * 16;
      const tx = P.x + (D.facing > 0 ? -230 : 230);
      D.vx += (tx - D.hx) * 0.006 * dt;
      D.hy += (homeY - D.hy) * 0.06 * dt;
      D.vx = clamp(D.vx, -6, 6);
      D.hx += D.vx * dt; D.vx *= 0.9;
      D.cd -= dt * (1 + aggro * 0.9);
      if (D.cd <= 0 && P.hp > 0) {
        if (dist < 210) startDragonAtk("bite");
        else if (Math.random() < 0.5) startDragonAtk("breathe");
        else startDragonAtk("swoop");
      }
    } else if (D.state === "bite") {
      // windup then lunge head toward player
      if (D.stT < 20) { D.hy += ((GROUND - 250) - D.hy) * 0.1 * dt; D.vx += (sign(P.x - D.hx) * 0.2) * dt; }
      else if (D.stT < 34) {
        D.hasHit = D.hasHit || false;
        const tx = P.x + sign(P.x - D.hx) * -10, ty = P.y - 70;
        D.hx += (tx - D.hx) * 0.28 * dt; D.hy += (ty - D.hy) * 0.28 * dt;
        if (!D.hasHit && circleRect(D.hx, D.hy, 30, P.x - 22, P.y - 100, 44, 100)) hitPlayer(12, sign(D.hx - P.x) ? -sign(P.x - D.hx) : 1, false);
      } else if (D.stT > 56) { D.state = "recover"; D.stT = 0; D.hasHit = false; D.cd = 60 - aggro * 24; }
      D.hx += D.vx * dt; D.vx *= 0.85;
    } else if (D.state === "swoop") {
      if (D.stT < 16) { D.hy += ((GROUND - 250) - D.hy) * 0.12 * dt; }
      else if (D.stT < 42) {
        D.hy += ((P.y - 60) - D.hy) * 0.12 * dt;
        D.hx += D.facing * 7.5 * dt;
        if (!D.hasHit && circleRect(D.hx, D.hy, 28, P.x - 22, P.y - 100, 44, 100)) { hitPlayer(11, D.facing, false); D.hasHit = true; }
      } else if (D.stT > 60) { D.state = "recover"; D.stT = 0; D.hasHit = false; D.cd = 64 - aggro * 26; }
    } else if (D.state === "breathe") {
      const homeY = GROUND - 240;
      D.hy += (homeY - D.hy) * 0.08 * dt;
      if (D.stT > 24 && D.stT < 66 && Math.floor(D.stT) % 3 === 0) {
        const a = Math.atan2((P.y - 60) - D.hy, P.x - D.hx) + (Math.random() - 0.5) * 0.18;
        const sp = 7 + Math.random() * 2;
        fires.push({ x: D.hx + D.facing * 34, y: D.hy + 8, vx: Math.cos(a) * sp, vy: Math.sin(a) * sp, r: 9 + Math.random() * 6, life: 70 });
      }
      if (D.stT > 86) { D.state = "recover"; D.stT = 0; D.cd = 70 - aggro * 26; }
    } else if (D.state === "recover") {
      const homeY = GROUND - 200 + Math.sin(D.bob) * 16;
      D.hy += (homeY - D.hy) * 0.06 * dt;
      if (D.stT > 20) { D.state = "approach"; D.stT = 0; }
    }
    D.hx = clamp(D.hx, 120, W - 60);
    D.hy = clamp(D.hy, 60, GROUND - 60);
    trailSegs(dt);
  }
  function startDragonAtk(kind) { D.state = kind; D.stT = 0; D.hasHit = false; }

  function hitPlayer(dmg, dir, chipOnly) {
    if (P.invuln > 0 || P.hp <= 0) return;
    const blocking = P.state === "block";
    if (blocking) { dmg = Math.max(1, Math.round(dmg * 0.25)); P.vx = dir * 3; G.shake = Math.max(G.shake, 0.4); spawnHits(P.x + P.facing * 24, P.y - 70, "#7db4ff"); P.invuln = 10; }
    else {
      P.hp = Math.max(0, P.hp - dmg);
      P.hitstun = 22; P.invuln = 30; P.atk = null; P.active = false;
      P.vx = dir * 5.5; P.vy = -4.5; P.onGround = false; P.state = "hit"; P.flash = 1;
      G.shake = Math.max(G.shake, 1); spawnHits(P.x, P.y - 70, "#ff5f74");
    }
    if (P.hp <= 0) P.state = "ko";
  }

  /* ---------------- projectiles / particles ---------------- */
  function updateBlasts(dt) {
    for (let i = blasts.length - 1; i >= 0; i--) {
      const b = blasts[i]; b.x += b.vx * dt; b.life -= dt;
      let hit = false;
      if (dist2(b.x, b.y, D.hx, D.hy) < (b.r + 32) * (b.r + 32)) hit = true;
      else for (let k = 0; k < 6 && !hit; k++) { const s = D.segs[k]; if (dist2(b.x, b.y, s.x, s.y) < (b.r + 22) * (b.r + 22)) hit = true; }
      if (hit && D.hitFlash <= 0.4) { damageDragon(b.dmg, b.dir * 4, "blast"); spawnHits(b.x, b.y, "#8be9ff"); blasts.splice(i, 1); continue; }
      if (b.x < -30 || b.x > W + 30 || b.life <= 0) blasts.splice(i, 1);
    }
  }
  function updateFires(dt) {
    for (let i = fires.length - 1; i >= 0; i--) {
      const f = fires[i]; f.x += f.vx * dt; f.y += f.vy * dt; f.vy += 0.12 * dt; f.life -= dt; f.r *= 0.995;
      if (P.invuln <= 0 && circleRect(f.x, f.y, f.r, P.x - 20, P.y - 96, 40, 96)) {
        hitPlayer(P.state === "block" ? 2 : 6, sign(f.vx) || 1, false);
        fires.splice(i, 1); continue;
      }
      if (f.y > GROUND || f.life <= 0 || f.x < -20 || f.x > W + 20) { if (f.y > GROUND) for (let n = 0; n < 3; n++) parts.push({ x: f.x, y: GROUND, vx: (Math.random() - .5) * 3, vy: -Math.random() * 3, life: 20, c: "#ff8a1f", r: 3 }); fires.splice(i, 1); }
    }
  }
  function spawnHits(x, y, c) { for (let i = 0; i < 8; i++) parts.push({ x, y, vx: (Math.random() - .5) * 7, vy: (Math.random() - .7) * 7, life: 16 + Math.random() * 8, c, r: 2 + Math.random() * 3 }); }
  function updateParticles(dt) {
    for (let i = parts.length - 1; i >= 0; i--) { const p = parts[i]; p.x += p.vx * dt; p.y += p.vy * dt; p.vy += 0.35 * dt; p.life -= dt; if (p.life <= 0) parts.splice(i, 1); }
  }

  /* ---------------- end ---------------- */
  function checkEnd() {
    if (G.phase !== "fight") return;
    if (D.hp <= 0) finish("win");
    else if (P.hp <= 0) finish("lose");
    else if (G.timer <= 0) finish(P.hp >= D.hp ? "win" : "lose");
  }
  function finish(res) {
    G.phase = "over"; G.result = res; G.announce = res === "win" ? "K.O.!" : "DEFEAT"; G.flash = 1;
    setTimeout(() => {
      const win = res === "win";
      endPanel.innerHTML = `
        <div class="ae-tag ${win ? "win" : "lose"}">${win ? "YOU WIN" : "YOU LOSE"}</div>
        <div class="ae-sub">${win ? `${ally.flag} ${ally.name} slew the dragon!` : `The dragon prevailed. Try again, challenger.`}</div>
        <div class="ae-actions">
          <button class="btn gold" id="aeAgain">Play again ▶</button>
          <button class="btn ghost" id="aeBack">Back to the board</button>
        </div>`;
      endPanel.classList.add("show");
      document.getElementById("aeAgain").onclick = () => { endPanel.classList.remove("show"); endPanel.innerHTML = ""; initGame(); };
      document.getElementById("aeBack").onclick = stopArcade;
    }, 900);
  }

  /* ---------------- loop ---------------- */
  function loop(now) {
    let dt = (now - last) / 16.667; last = now; if (dt > 3) dt = 3;
    update(dt);
    render();
    raf = requestAnimationFrame(loop);
  }

  /* ---------------- render ---------------- */
  function render() {
    ctx.save();
    let sx = 0, sy = 0;
    if (G.shake > 0) { sx = (Math.random() - .5) * 16 * G.shake; sy = (Math.random() - .5) * 16 * G.shake; }
    ctx.clearRect(0, 0, W, H);
    ctx.translate(sx, sy);
    drawBackground();
    drawDragon();
    drawFighter();
    drawBlasts();
    drawFires();
    drawParts();
    ctx.restore();
    drawHUD();
    if (G.flash > 0) { ctx.fillStyle = `rgba(255,255,255,${G.flash * 0.5})`; ctx.fillRect(0, 0, W, H); }
    if (G.announce) drawAnnounce(G.announce);
  }
  function drawBackground() {
    const g = ctx.createLinearGradient(0, 0, 0, H);
    g.addColorStop(0, "#3a0a12"); g.addColorStop(.55, "#1a060c"); g.addColorStop(1, "#0a0305");
    ctx.fillStyle = g; ctx.fillRect(0, 0, W, H);
    // sun/moon
    ctx.fillStyle = "rgba(255,120,40,.25)"; ctx.beginPath(); ctx.arc(W * .5, 150, 130, 0, 7); ctx.fill();
    // skyline
    ctx.fillStyle = "#12060a";
    for (let i = 0; i < 16; i++) { const bw = 40 + (i * 53 % 60); const bh = 90 + (i * 71 % 150); const bx = (i * 118) % (W + 60) - 30; ctx.fillRect(bx, GROUND - bh, bw, bh); }
    // floor
    ctx.fillStyle = "#1c0d10"; ctx.fillRect(0, GROUND, W, H - GROUND);
    ctx.strokeStyle = "rgba(255,130,60,.12)"; ctx.lineWidth = 2;
    for (let i = 0; i < 14; i++) { const yy = GROUND + i * 6 + 4; ctx.beginPath(); ctx.moveTo(0, yy); ctx.lineTo(W, yy); ctx.stroke(); }
    ctx.strokeStyle = "rgba(255,190,40,.25)"; ctx.beginPath(); ctx.moveTo(0, GROUND); ctx.lineTo(W, GROUND); ctx.stroke();
  }

  function shadow(x, w) { ctx.fillStyle = "rgba(0,0,0,.4)"; ctx.beginPath(); ctx.ellipse(x, GROUND + 4, w, 8, 0, 0, 7); ctx.fill(); }

  function drawFighter() {
    const s = P;
    shadow(s.x, 30);
    const c1 = ally.c1 || "#2b6fff", c2 = ally.c2 || "#7db4ff";
    let crouch = s.state === "crouch" ? 22 : 0;
    let lean = (s.state === "hit") ? -s.facing * 12 : (s.state === "block" ? -s.facing * 6 : 0);
    const feetY = s.y;
    const hipY = feetY - 46 + crouch;
    const shX = s.x + lean, shY = hipY - 44 + crouch * .4;
    const headY = shY - 22, headX = shX + s.facing * 3;
    // limbs endpoints
    let frontHandX = shX + s.facing * 16, frontHandY = shY + 6;
    let backHandX = shX - s.facing * 12, backHandY = shY + 8;
    let frontFootX = s.x + s.facing * 14, backFootX = s.x - s.facing * 15;
    let frontFootY = feetY, backFootY = feetY;
    const wk = Math.sin(s.walk * 6) * 10;
    if (s.state === "walk") { frontFootX += wk; backFootX -= wk; }
    if (s.state === "block") { frontHandX = shX + s.facing * 22; frontHandY = shY - 6; backHandX = shX + s.facing * 18; backHandY = shY + 10; }
    if (s.atk === "punch") { const e = clamp(s.atkT / 8, 0, 1); frontHandX = shX + s.facing * (16 + 52 * e); frontHandY = shY + 2; }
    if (s.atk === "kick") { const e = clamp(s.atkT / 10, 0, 1); const reach = s.onGround ? 78 : 66; frontFootX = s.x + s.facing * (14 + reach * e); frontFootY = feetY - (s.onGround ? 34 : 46) * e; }
    if (s.atk === "blast") { const e = clamp(s.atkT / 12, 0, 1); frontHandX = shX + s.facing * (16 + 34 * e); frontHandY = shY + 4; backHandX = shX + s.facing * 8; }
    if (!s.onGround && s.atk !== "kick") { frontFootX = s.x + s.facing * 8; backFootX = s.x - s.facing * 6; frontFootY = feetY - 10; backFootY = feetY - 6; }

    ctx.lineCap = "round";
    // back leg + arm (darker)
    ctx.strokeStyle = shade(c1, -30); ctx.lineWidth = 11;
    line(s.x, hipY, backFootX, backFootY);
    ctx.strokeStyle = shade(c2, -20); ctx.lineWidth = 9;
    line(shX, shY + 4, backHandX, backHandY);
    // torso
    ctx.fillStyle = c2; roundRectPath(shX - 12, shY, 24, hipY - shY + 6, 7); ctx.fill();
    ctx.fillStyle = shade(c1, -10); roundRectPath(s.x - 12, hipY, 24, 14, 5); ctx.fill(); // belt/hips
    // front leg + arm
    ctx.strokeStyle = c1; ctx.lineWidth = 12;
    line(s.x, hipY, frontFootX, frontFootY);
    if (s.atk === "kick") { ctx.strokeStyle = "#ffe6d0"; ctx.lineWidth = 12; line(s.x, hipY, frontFootX, frontFootY); }
    ctx.strokeStyle = "#ffe6d0"; ctx.lineWidth = 10;   // front arm (skin)
    line(shX, shY + 4, frontHandX, frontHandY);
    // fist glow on punch
    if (s.atk === "punch" && s.active) { ctx.fillStyle = "rgba(255,255,255,.7)"; ctx.beginPath(); ctx.arc(frontHandX, frontHandY, 9, 0, 7); ctx.fill(); }
    // head
    ctx.fillStyle = "#ffe0c4"; ctx.beginPath(); ctx.arc(headX, headY, 15, 0, 7); ctx.fill();
    // headband
    ctx.strokeStyle = c1; ctx.lineWidth = 6; ctx.beginPath(); ctx.arc(headX, headY - 2, 15, Math.PI * 1.05, Math.PI * 1.95); ctx.stroke();
    ctx.strokeStyle = c1; ctx.lineWidth = 4; line(headX - s.facing * 13, headY - 4, headX - s.facing * 26, headY + 4 + Math.sin(G.t * .3) * 3);
    // eyes
    ctx.fillStyle = "#1a1a1a"; ctx.beginPath(); ctx.arc(headX + s.facing * 5, headY, 2.2, 0, 7); ctx.fill();
    // flash overlay when hit
    if (s.flash > 0) { ctx.fillStyle = `rgba(255,255,255,${s.flash * .7})`; roundRectPath(shX - 16, headY - 18, 34, hipY - headY + 30, 8); ctx.fill(); }
    // flag tag
    ctx.font = "16px sans-serif"; ctx.textAlign = "center"; ctx.fillText(ally.flag || "", headX, headY - 24);
  }

  function drawDragon() {
    const flash = D.hitFlash > 0;
    // body from tail to head
    for (let i = D.segs.length - 1; i >= 0; i--) {
      const s = D.segs[i]; const r = 10 + (D.segs.length - i) / D.segs.length * 18;
      ctx.fillStyle = flash ? "#ffffff" : (i % 2 ? "#b3121f" : "#e0242f");
      ctx.beginPath(); ctx.arc(s.x, s.y, r, 0, 7); ctx.fill();
      // belly/scale
      ctx.fillStyle = flash ? "#fff" : "rgba(255,200,60,.5)"; ctx.beginPath(); ctx.arc(s.x, s.y + r * .3, r * .4, 0, 7); ctx.fill();
      // spine spike
      if (i % 2 === 0) { ctx.fillStyle = flash ? "#fff" : "#ffb020"; ctx.beginPath(); ctx.moveTo(s.x, s.y - r); ctx.lineTo(s.x - 5, s.y - r - 8); ctx.lineTo(s.x + 5, s.y - r - 8); ctx.fill(); }
    }
    // head
    const hx = D.hx, hy = D.hy, f = D.facing;
    ctx.save(); ctx.translate(hx, hy); ctx.scale(f, 1);
    // horns
    ctx.fillStyle = flash ? "#fff" : "#ffce4a";
    ctx.beginPath(); ctx.moveTo(-6, -22); ctx.lineTo(-14, -46); ctx.lineTo(2, -26); ctx.fill();
    ctx.beginPath(); ctx.moveTo(8, -20); ctx.lineTo(4, -44); ctx.lineTo(18, -22); ctx.fill();
    // skull
    ctx.fillStyle = flash ? "#fff" : "#e0242f";
    ctx.beginPath(); ctx.ellipse(0, 0, 32, 26, 0, 0, 7); ctx.fill();
    // snout
    ctx.beginPath(); ctx.moveTo(18, -8); ctx.quadraticCurveTo(52, -6, 50, 4); ctx.quadraticCurveTo(48, 8, 20, 10); ctx.fill();
    // mouth open
    const mo = D.mouth * 16;
    ctx.fillStyle = "#2a0206"; ctx.beginPath(); ctx.moveTo(24, 6); ctx.lineTo(52, 6 + mo); ctx.lineTo(24, 14 + mo * .4); ctx.fill();
    // teeth
    ctx.fillStyle = "#fff"; ctx.beginPath(); ctx.moveTo(46, 6); ctx.lineTo(50, 12); ctx.lineTo(42, 8); ctx.fill();
    // whiskers
    ctx.strokeStyle = flash ? "#fff" : "#ffce4a"; ctx.lineWidth = 3; ctx.lineCap = "round";
    ctx.beginPath(); ctx.moveTo(46, 2); ctx.quadraticCurveTo(80, -6, 92, 14); ctx.stroke();
    // eye
    ctx.fillStyle = flash ? "#111" : "#ffe14d"; ctx.beginPath(); ctx.arc(2, -6, 6, 0, 7); ctx.fill();
    ctx.fillStyle = "#2a0206"; ctx.beginPath(); ctx.ellipse(3, -6, 2, 5, 0, 0, 7); ctx.fill();
    // glow when breathing
    if (D.state === "breathe" && D.stT > 20) { ctx.fillStyle = "rgba(255,160,40,.6)"; ctx.beginPath(); ctx.arc(46, 8, 10 + Math.random() * 6, 0, 7); ctx.fill(); }
    ctx.restore();
  }

  function drawBlasts() {
    for (const b of blasts) {
      const g = ctx.createRadialGradient(b.x, b.y, 2, b.x, b.y, b.r + 8);
      g.addColorStop(0, "#eaffff"); g.addColorStop(.4, "#8be9ff"); g.addColorStop(1, "rgba(60,120,255,0)");
      ctx.fillStyle = g; ctx.beginPath(); ctx.arc(b.x, b.y, b.r + 8, 0, 7); ctx.fill();
      ctx.fillStyle = "#eaffff"; ctx.beginPath(); ctx.arc(b.x - b.dir * 6, b.y, b.r * .5, 0, 7); ctx.fill();
    }
  }
  function drawFires() {
    for (const f of fires) {
      const g = ctx.createRadialGradient(f.x, f.y, 1, f.x, f.y, f.r);
      g.addColorStop(0, "#fff3b0"); g.addColorStop(.4, "#ff8a1f"); g.addColorStop(1, "rgba(224,36,20,0)");
      ctx.fillStyle = g; ctx.beginPath(); ctx.arc(f.x, f.y, f.r, 0, 7); ctx.fill();
    }
  }
  function drawParts() {
    for (const p of parts) { ctx.globalAlpha = clamp(p.life / 16, 0, 1); ctx.fillStyle = p.c; ctx.beginPath(); ctx.arc(p.x, p.y, p.r, 0, 7); ctx.fill(); }
    ctx.globalAlpha = 1;
  }

  /* ---------- HUD ---------- */
  function drawHUD() {
    // player bar (left, depletes to the left)
    bar(28, 26, 380, P.hp / P.maxHp, true, "#a4ff5c", "#39a026");
    // dragon bar (right)
    bar(W - 28 - 380, 26, 380, D.hp / D.maxHp, false, "#ff6a5a", "#c0121f");
    // portraits / names
    ctx.textAlign = "left"; ctx.fillStyle = "#fff8f0"; ctx.font = "italic 700 20px 'Arial Narrow',sans-serif";
    ctx.fillText(((ally.flag || "") + " " + (ally.name || "ALLY")).toUpperCase(), 30, 66);
    ctx.textAlign = "right"; ctx.fillText("🐉 CHINA DRAGON", W - 30, 66);
    // portrait chip
    if (avatarImg && avatarImg.complete) { ctx.save(); roundRectPath(28, 74, 44, 44, 6); ctx.clip(); ctx.drawImage(avatarImg, 28, 70, 44, 52); ctx.restore(); ctx.strokeStyle = ally.c2 || "#7db4ff"; ctx.lineWidth = 2; roundRectPath(28, 74, 44, 44, 6); ctx.stroke(); }
    // timer
    ctx.textAlign = "center"; ctx.fillStyle = "#ffd23e"; ctx.font = "italic 900 40px 'Arial Black',sans-serif";
    ctx.fillText(String(G.timer).padStart(2, "0"), W / 2, 58);
    // blast cooldown pip
    ctx.textAlign = "left"; ctx.font = "12px sans-serif"; ctx.fillStyle = P.blastCd > 0 ? "rgba(139,233,255,.4)" : "#8be9ff";
    ctx.fillText(P.blastCd > 0 ? "✦ charging…" : "✦ blast ready", 30, 134);
  }
  function bar(x, y, w, pct, leftToRight, hi, lo) {
    pct = clamp(pct, 0, 1);
    ctx.fillStyle = "#000"; ctx.fillRect(x - 2, y - 2, w + 4, 22);
    ctx.fillStyle = "#2a0d10"; ctx.fillRect(x, y, w, 18);
    const g = ctx.createLinearGradient(0, y, 0, y + 18); g.addColorStop(0, hi); g.addColorStop(1, lo);
    ctx.fillStyle = g;
    const fw = w * pct;
    if (leftToRight) ctx.fillRect(x, y, fw, 18); else ctx.fillRect(x + w - fw, y, fw, 18);
    ctx.strokeStyle = "rgba(255,210,60,.6)"; ctx.lineWidth = 2; ctx.strokeRect(x, y, w, 18);
  }
  function drawAnnounce(txt) {
    ctx.save(); ctx.textAlign = "center"; ctx.textBaseline = "middle";
    ctx.font = "italic 900 clamp(48px,9vw,110px) 'Arial Black',sans-serif";
    const y = H * .42;
    ctx.lineWidth = 8; ctx.strokeStyle = "#2a0206"; ctx.strokeText(txt, W / 2, y);
    ctx.fillStyle = G.result === "lose" ? "#ff5f74" : "#ffd23e"; ctx.fillText(txt, W / 2, y);
    ctx.restore();
  }

  /* color shade */
  function shade(hex, amt) {
    const h = hex.replace("#", ""); const n = h.length === 3 ? h.split("").map(c => c + c).join("") : h;
    let r = parseInt(n.slice(0, 2), 16), g = parseInt(n.slice(2, 4), 16), b = parseInt(n.slice(4, 6), 16);
    r = clamp(r + amt, 0, 255); g = clamp(g + amt, 0, 255); b = clamp(b + amt, 0, 255);
    return `rgb(${r},${g},${b})`;
  }
  function line(x1, y1, x2, y2) { ctx.beginPath(); ctx.moveTo(x1, y1); ctx.lineTo(x2, y2); ctx.stroke(); }
  function roundRectPath(x, y, w, h, r) { ctx.beginPath(); ctx.moveTo(x + r, y); ctx.arcTo(x + w, y, x + w, y + h, r); ctx.arcTo(x + w, y + h, x, y + h, r); ctx.arcTo(x, y + h, x, y, r); ctx.arcTo(x, y, x + w, y, r); ctx.closePath(); }

  window.startArcade = startArcade;
  window.stopArcade = stopArcade;
  window.arcadeState = () => G ? { phase: G.phase, php: P.hp, pmax: P.maxHp, dhp: D.hp, dmax: D.maxHp } : null;
})();
