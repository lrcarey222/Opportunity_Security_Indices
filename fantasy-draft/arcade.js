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
  let ally, avatarImg = null, dragonImg = null, dragonFireImg = null, powerPct = 80;
  let featherCache = {};
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
  /* Draw an image into an offscreen canvas with an elliptical alpha feather so the
     baked-in rectangular background fades out and the character reads as a sprite. */
  function feather(img, key, targetH, cx, cy, rx, ry) {
    if (!img || !img.complete || !img.naturalWidth) return null;
    if (featherCache[key]) return featherCache[key];
    const aspect = img.naturalWidth / img.naturalHeight;
    const h = targetH, w = Math.round(h * aspect);
    const oc = document.createElement("canvas"); oc.width = w; oc.height = h;
    const o = oc.getContext("2d");
    o.drawImage(img, 0, 0, w, h);
    o.globalCompositeOperation = "destination-in";
    o.save();
    o.translate(w * (cx || 0.5), h * (cy || 0.48));
    o.scale(w * (rx || 0.6), h * (ry || 0.6));
    const g = o.createRadialGradient(0, 0, 0.2, 0, 0, 1);
    g.addColorStop(0, "rgba(0,0,0,1)"); g.addColorStop(0.72, "rgba(0,0,0,1)"); g.addColorStop(1, "rgba(0,0,0,0)");
    o.fillStyle = g; o.beginPath(); o.arc(0, 0, 1, 0, 7); o.fill();
    o.restore();
    o.globalCompositeOperation = "source-over";
    featherCache[key] = { canvas: oc, w, h };
    return featherCache[key];
  }
  function dragonBox() {
    const dw = 190, dh = 250;
    return { x: D.hx - dw * 0.30, y: D.hy - dh * 0.42, w: dw * 0.6, h: dh * 0.84 };
  }

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
    featherCache = {};
    avatarImg = null;
    if (typeof AVATARS !== "undefined" && AVATARS[allyId]) { avatarImg = new Image(); avatarImg.src = AVATARS[allyId]; }
    dragonImg = null; dragonFireImg = null;
    if (typeof FIGHT_FRAMES !== "undefined") {
      if (FIGHT_FRAMES.L4) { dragonImg = new Image(); dragonImg.src = FIGHT_FRAMES.L4; }
      if (FIGHT_FRAMES.L5) { dragonFireImg = new Image(); dragonFireImg.src = FIGHT_FRAMES.L5; }
    }
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
      state: "approach", stT: 0, mouth: 0, hasHit: false, hitFlash: 0,
      cd: 90, bob: 0, facing: -1,
    };
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
    // soft pushbox so the fighter can't walk through the dragon (except while it lunges)
    if (D.state === "approach" || D.state === "recover") {
      const gap = 96;
      if (D.hx > P.x) P.x = Math.min(P.x, D.hx - gap);
      else P.x = Math.max(P.x, D.hx + gap);
    }
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
    const box = dragonBox();
    const hit = circleRect(hx, hy, r + 8, box.x, box.y, box.w, box.h);
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
  }
  function updateDragon(dt) {
    if (D.hitFlash > 0) D.hitFlash -= dt * 0.09;
    D.facing = sign(P.x - D.hx) || -1;
    D.bob += dt * 0.05;
    D.mouth += ((D.state === "breathe" || D.state === "bite" ? 1 : 0) - D.mouth) * 0.2 * dt;
    const dist = Math.abs(P.x - D.hx);
    const aggro = 1 - D.hp / D.maxHp; // 0..1

    if (D.state === "ko") { D.hy += 2 * dt; D.hx += D.vx * dt; D.vx *= 0.9; return; }

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
        fires.push({ x: D.hx + D.facing * 64, y: D.hy + 6, vx: Math.cos(a) * sp, vy: Math.sin(a) * sp, r: 9 + Math.random() * 6, life: 70 });
      }
      if (D.stT > 86) { D.state = "recover"; D.stT = 0; D.cd = 70 - aggro * 26; }
    } else if (D.state === "recover") {
      const homeY = GROUND - 200 + Math.sin(D.bob) * 16;
      D.hy += (homeY - D.hy) * 0.06 * dt;
      if (D.stT > 20) { D.state = "approach"; D.stT = 0; }
    }
    D.hx = clamp(D.hx, 150, W - 90);
    D.hy = clamp(D.hy, 120, GROUND - 60);
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
      const box = dragonBox();
      const hit = circleRect(b.x, b.y, b.r + 6, box.x, box.y, box.w, box.h);
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

  /* circular head cut-out from the avatar portrait (the ally's real face) */
  function headCut(img) {
    if (!img || !img.complete || !img.naturalWidth) return null;
    if (featherCache.__head) return featherCache.__head;
    const S = 128, oc = document.createElement("canvas"); oc.width = S; oc.height = S;
    const o = oc.getContext("2d");
    const iw = img.naturalWidth, ih = img.naturalHeight;
    const cx = iw * 0.5, cy = ih * 0.30, rr = iw * 0.36;
    o.save(); o.beginPath(); o.arc(S / 2, S / 2, S / 2 - 1, 0, 7); o.clip();
    o.drawImage(img, cx - rr, cy - rr, rr * 2, rr * 2, 0, 0, S, S);
    o.restore();
    featherCache.__head = { canvas: oc, S };
    return featherCache.__head;
  }
  // 2-bone IK: joint between (x1,y1)->target given segment lengths + bend direction
  function ik(x1, y1, x2, y2, l1, l2, bend) {
    let dx = x2 - x1, dy = y2 - y1, d = Math.hypot(dx, dy) || 0.001;
    const maxd = l1 + l2 - 0.5;
    if (d > maxd) { x2 = x1 + dx / d * maxd; y2 = y1 + dy / d * maxd; dx = x2 - x1; dy = y2 - y1; d = maxd; }
    const a = Math.atan2(dy, dx);
    const A = Math.acos(clamp((l1 * l1 + d * d - l2 * l2) / (2 * l1 * d), -1, 1));
    return { jx: x1 + Math.cos(a + A * bend) * l1, jy: y1 + Math.sin(a + A * bend) * l1, ex: x2, ey: y2 };
  }
  function limb(x1, y1, j, x3, y3, wUp, wLo, col, outline) {
    ctx.lineCap = "round";
    ctx.strokeStyle = outline; ctx.lineWidth = wUp + 4; seg(x1, y1, j.jx, j.jy); ctx.lineWidth = wLo + 4; seg(j.jx, j.jy, x3, y3);
    ctx.strokeStyle = col; ctx.lineWidth = wUp; seg(x1, y1, j.jx, j.jy); ctx.lineWidth = wLo; seg(j.jx, j.jy, x3, y3);
  }
  function seg(a, b, c, d) { ctx.beginPath(); ctx.moveTo(a, b); ctx.lineTo(c, d); ctx.stroke(); }

  function drawFighter() {
    const s = P;
    shadow(s.x, 30);
    const c1 = ally.c1 || "#2b6fff", c2 = ally.c2 || "#7db4ff";
    const skin = "#e7b489", ol = "rgba(10,4,8,.9)";
    const pants = c1, sleeve = c2, boot = shade(c1, -46), glove = shade(c2, -20);
    const hc = headCut(avatarImg);
    const t = G.t, f = 1; // draw in facing-right space, flip via transform

    const crouch = s.state === "crouch";
    const breath = (s.onGround && !s.atk && s.hitstun <= 0 && s.state !== "block") ? Math.sin(t * 0.08) * 2 : 0;
    let hipY = s.y - (crouch ? 34 : 52);
    let lean = 0;
    if (s.state === "hit") lean = -13;
    else if (s.state === "block") lean = -5;
    else if (s.atk === "punch") lean = 6 * Math.sin(clamp(s.atkT / 12, 0, 1) * Math.PI);
    else if (s.atk === "kick") lean = -7;
    else if (s.atk === "blast") lean = 4 * Math.sin(clamp(s.atkT / 16, 0, 1) * Math.PI);
    const shY = hipY - (crouch ? 38 : 48) + breath;
    const shX = s.x + lean;
    const headY = shY - 17, headX = shX + 3 + lean * 0.35, headR = 19;

    // ---- foot targets ----
    let ffX = s.x + 15, fbX = s.x - 16, ffY = s.y, fbY = s.y;
    if (s.state === "walk") { const c = Math.sin(s.walk * 6); ffX = s.x + 16 + c * 16; fbX = s.x - 16 - c * 16; ffY = s.y - Math.max(0, c) * 9; fbY = s.y - Math.max(0, -c) * 9; }
    else if (!s.onGround) { ffX = s.x + 12; fbX = s.x - 12; ffY = fbY = hipY + 30; }
    else if (crouch) { ffX = s.x + 20; fbX = s.x - 20; }
    // ---- hand targets ----
    let fhX = shX + 17, fhY = shY + 12, bhX = shX - 13, bhY = shY + 12;   // guard
    if (s.state === "block") { fhX = shX + 20; fhY = shY - 4; bhX = shX + 15; bhY = shY + 14; }
    if (s.atk === "punch") { const e = Math.sin(clamp(s.atkT / 11, 0, 1) * Math.PI); fhX = shX + 20 + 58 * e; fhY = shY + 6; bhX = shX - 16 - 4 * e; }
    else if (s.atk === "blast") { const e = Math.sin(clamp(s.atkT / 14, 0, 1) * Math.PI); fhX = shX + 22 + 30 * e; fhY = shY + 10; bhX = shX + 10 * e; bhY = shY + 12; }
    else if (s.atk === "kick") { fhX = shX + 8; fhY = shY - 4; bhX = shX - 18; bhY = shY + 4; }
    else if (s.state === "hit") { fhX = shX + 8; fhY = shY - 8; bhX = shX - 20; bhY = shY - 4; }
    if (s.atk === "kick") { const e = Math.sin(clamp(s.atkT / 12, 0, 1) * Math.PI); ffX = s.x + 26 + 62 * e; ffY = s.y - 22 - 22 * e; fbX = s.x - 12; }

    ctx.save();
    ctx.translate(s.x, 0); ctx.scale(s.facing, 1); ctx.translate(-s.x, 0);

    const hipFX = s.x + 8, hipBX = s.x - 8;
    // back limbs (behind torso, darker)
    limb(hipBX, hipY, ik(hipBX, hipY, fbX, fbY, 28, 28, 1), fbX, fbY, 13, 11, shade(pants, -22), ol);
    ctx.strokeStyle = shade(boot, -10); ctx.lineWidth = 12; seg(fbX - 6, fbY, fbX + 8, fbY);
    limb(shX - 9, shY + 2, ik(shX - 9, shY + 2, bhX, bhY, 20, 20, 1), bhX, bhY, 10, 8, shade(skin, -26), ol);

    // torso
    ctx.beginPath();
    ctx.moveTo(shX - 15, shY);
    ctx.quadraticCurveTo(shX - 19, (shY + hipY) / 2, s.x - 13, hipY);
    ctx.lineTo(s.x + 13, hipY);
    ctx.quadraticCurveTo(shX + 19, (shY + hipY) / 2, shX + 15, shY);
    ctx.closePath();
    ctx.fillStyle = ol; ctx.strokeStyle = ol; ctx.lineWidth = 4; ctx.stroke();
    const tg = ctx.createLinearGradient(0, shY, 0, hipY); tg.addColorStop(0, c2); tg.addColorStop(1, shade(c2, -34));
    ctx.fillStyle = tg; ctx.fill();
    // chest shading + belt
    ctx.strokeStyle = "rgba(0,0,0,.18)"; ctx.lineWidth = 2; seg(shX, shY + 6, s.x, hipY - 6);
    ctx.fillStyle = pants; roundRectPath(s.x - 14, hipY - 5, 28, 12, 4); ctx.fill();
    // neck
    ctx.strokeStyle = skin; ctx.lineWidth = 11; seg(headX, headY + 10, shX, shY + 2);

    // front limbs
    limb(hipFX, hipY, ik(hipFX, hipY, ffX, ffY, 28, 28, 1), ffX, ffY, 14, 12, pants, ol);
    ctx.strokeStyle = boot; ctx.lineWidth = 13; seg(ffX - 7, ffY, ffX + 10, ffY);
    // front arm: upper=sleeve, fore=skin
    const armJ = ik(shX + 9, shY + 2, fhX, fhY, 20, 20, 1);
    ctx.lineCap = "round";
    ctx.strokeStyle = ol; ctx.lineWidth = 15; seg(shX + 9, shY + 2, armJ.jx, armJ.jy); ctx.lineWidth = 13; seg(armJ.jx, armJ.jy, armJ.ex, armJ.ey);
    ctx.strokeStyle = sleeve; ctx.lineWidth = 12; seg(shX + 9, shY + 2, armJ.jx, armJ.jy);
    ctx.strokeStyle = skin; ctx.lineWidth = 10; seg(armJ.jx, armJ.jy, armJ.ex, armJ.ey);
    ctx.fillStyle = glove; ctx.beginPath(); ctx.arc(armJ.ex, armJ.ey, 6.5, 0, 7); ctx.fill();
    // shoulder cap
    ctx.fillStyle = sleeve; ctx.strokeStyle = ol; ctx.lineWidth = 2; ctx.beginPath(); ctx.arc(shX + 12, shY + 1, 8, 0, 7); ctx.fill(); ctx.stroke();

    // head: real face cut-out + headband
    ctx.fillStyle = ol; ctx.beginPath(); ctx.arc(headX, headY, headR + 2, 0, 7); ctx.fill();
    if (hc) { ctx.save(); ctx.beginPath(); ctx.arc(headX, headY, headR, 0, 7); ctx.clip(); ctx.drawImage(hc.canvas, headX - headR, headY - headR, headR * 2, headR * 2); ctx.restore(); }
    else { ctx.fillStyle = skin; ctx.beginPath(); ctx.arc(headX, headY, headR, 0, 7); ctx.fill(); }
    // small trailing ribbon behind the head (motion flair; avatar keeps its own headwear)
    ctx.strokeStyle = c1; ctx.lineCap = "round"; ctx.lineWidth = 4;
    seg(headX - headR + 3, headY - 6, headX - headR - 16, headY - 3 + Math.sin(t * 0.3) * 4);
    // hit flash over whole body
    if (s.flash > 0) { ctx.globalCompositeOperation = "lighter"; ctx.globalAlpha = clamp(s.flash, 0, 1) * 0.5; ctx.fillStyle = "#fff"; ctx.beginPath(); ctx.ellipse(shX, (shY + hipY) / 2, 26, (hipY - headY) / 1.4, 0, 0, 7); ctx.fill(); ctx.globalAlpha = 1; ctx.globalCompositeOperation = "source-over"; }

    ctx.restore();

    // block shield + attack effects (world space)
    if (s.state === "block") { ctx.save(); ctx.globalCompositeOperation = "lighter"; ctx.strokeStyle = "rgba(120,180,255,.6)"; ctx.lineWidth = 5; ctx.beginPath(); ctx.arc(s.x + s.facing * 26, s.y - 92, 58, s.facing > 0 ? -1.1 : Math.PI - -1.1, s.facing > 0 ? 1.1 : Math.PI - 1.1, s.facing < 0); ctx.stroke(); ctx.restore(); }
    if (s.atk === "punch" && s.active) impact(s.x + s.facing * 86, s.y - 106, "#ffffff", 22);
    if (s.atk === "kick" && s.active) arcSlash(s.x + s.facing * 96, s.y - 96, s.facing, "#ffd23e");
  }

  function drawDragon() {
    const breathing = D.state === "breathe" && D.stT > 16;
    let img = dragonImg, key = "dragon";
    if (breathing && dragonFireImg && dragonFireImg.complete && dragonFireImg.naturalWidth) { img = dragonFireImg; key = "dfire"; }
    const fe = feather(img, key, 320, 0.5, 0.5, 0.56, 0.58);
    ctx.save();
    ctx.translate(D.hx, D.hy + Math.sin(D.bob) * 6);
    ctx.scale(D.facing, 1);
    if (fe) {
      ctx.drawImage(fe.canvas, -fe.w / 2, -fe.h * 0.5, fe.w, fe.h);
      if (D.hitFlash > 0.4) { ctx.save(); ctx.globalCompositeOperation = "lighter"; ctx.globalAlpha = 0.55; ctx.drawImage(fe.canvas, -fe.w / 2, -fe.h * 0.5, fe.w, fe.h); ctx.restore(); }
    } else {
      ctx.fillStyle = D.hitFlash > 0.4 ? "#fff" : "#e0242f"; ctx.beginPath(); ctx.ellipse(0, 0, 80, 130, 0, 0, 7); ctx.fill();
    }
    ctx.restore();
  }

  function impact(x, y, c, r) {
    ctx.save(); ctx.globalCompositeOperation = "lighter";
    const g = ctx.createRadialGradient(x, y, 1, x, y, r); g.addColorStop(0, c); g.addColorStop(1, "rgba(255,255,255,0)");
    ctx.fillStyle = g; ctx.beginPath(); ctx.arc(x, y, r, 0, 7); ctx.fill();
    ctx.strokeStyle = c; ctx.lineWidth = 3;
    for (let i = 0; i < 6; i++) { const a = i / 6 * Math.PI * 2 + G.t * 0.3; ctx.beginPath(); ctx.moveTo(x + Math.cos(a) * r * 0.5, y + Math.sin(a) * r * 0.5); ctx.lineTo(x + Math.cos(a) * r * 1.35, y + Math.sin(a) * r * 1.35); ctx.stroke(); }
    ctx.restore();
  }
  function arcSlash(x, y, f, c) {
    ctx.save(); ctx.globalCompositeOperation = "lighter"; ctx.strokeStyle = c; ctx.lineWidth = 7; ctx.globalAlpha = 0.85;
    ctx.beginPath(); ctx.arc(x - f * 26, y, 44, f > 0 ? -1.15 : Math.PI + 1.15, f > 0 ? 1.15 : Math.PI - 1.15, f < 0); ctx.stroke();
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
