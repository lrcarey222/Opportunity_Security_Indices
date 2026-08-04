/* ============================================================================
   NET — realtime league sync layer.
   Two interchangeable adapters emit the same normalized league snapshot:
     • FirebaseAdapter  — real cross-device multiplayer (needs firebase-config.js)
     • LocalAdapter     — same-origin multi-tab (BroadcastChannel) for testing/fallback
   League snapshot shape:
     { id, host, name, status:'lobby'|'drafting'|'results',
       settings:{rounds,difficulty}, players:{uid:{name,allyId,ts}},
       presence:{uid:true}, draft:{order:[uid],pickIndex,picks:{subN:uid},seq:{n:{subN,uid,round}}} }
   ============================================================================ */
const NET = (function () {
  const UIDKEY = "osi_uid";
  function myUid() {
    let u = null;
    try { u = localStorage.getItem(UIDKEY); } catch (e) {}
    if (!u) { u = "u" + Math.random().toString(36).slice(2, 10); try { localStorage.setItem(UIDKEY, u); } catch (e) {} }
    return u;
  }
  function newCode() {
    const c = "ABCDEFGHJKLMNPQRSTUVWXYZ23456789"; let s = "";
    for (let i = 0; i < 5; i++) s += c[Math.floor(Math.random() * c.length)];
    return s;
  }
  /* pure pick application (shared logic) */
  function applyPick(draft, subN, seat, pickIndex, round) {
    if (!draft || draft.pickIndex !== pickIndex) return null;
    if (draft.picks && draft.picks[subN]) return null;
    draft.picks = draft.picks || {};
    draft.seq = draft.seq || {};
    draft.picks[subN] = seat;
    draft.seq[pickIndex] = { subN, uid: seat, round };
    draft.pickIndex = pickIndex + 1;
    return draft;
  }

  /* ---------------- Local adapter (BroadcastChannel + localStorage) ---------------- */
  function LocalAdapter() {
    const me = myUid();
    let id = null, bc = null, cb = null, hb = null;
    const key = (i) => "league:" + i;
    const read = (i) => { try { return JSON.parse(localStorage.getItem(key(i)) || "null"); } catch (e) { return null; } };
    const emit = () => { if (cb && id) cb(read(id)); };
    const write = (i, d) => { localStorage.setItem(key(i), JSON.stringify(d)); emit(); if (bc) bc.postMessage(1); };
    const listen = () => { if (bc) bc.close(); bc = new BroadcastChannel(key(id)); bc.onmessage = emit; };
    const heartbeat = () => { hb = setInterval(() => { const d = read(id); if (d) { d.presence = d.presence || {}; d.presence[me] = Date.now(); localStorage.setItem(key(id), JSON.stringify(d)); if (bc) bc.postMessage(1); } }, 4000); };
    return {
      kind: "local", me,
      isConfigured: () => true,
      async createLeague(meta) {
        id = newCode();
        write(id, { id, host: me, name: meta.name, status: "lobby", settings: meta.settings, players: {}, presence: {}, draft: null });
        listen(); heartbeat(); return id;
      },
      async joinLeague(lid) { id = lid; if (!read(id)) return { error: "not_found" }; listen(); heartbeat(); emit(); return { ok: true }; },
      async setPlayer(p) { const d = read(id); if (!d) return; d.players = d.players || {}; d.players[me] = { name: p.name, allyId: p.allyId, ts: Date.now() }; d.presence = d.presence || {}; d.presence[me] = Date.now(); write(id, d); },
      async setSettings(s) { const d = read(id); if (!d) return; d.settings = Object.assign(d.settings || {}, s); write(id, d); },
      async removePlayer(uid) { const d = read(id); if (!d) return; if (d.players) delete d.players[uid]; if (d.presence) delete d.presence[uid]; write(id, d); },
      async startDraft(order, settings) { const d = read(id); if (!d) return; d.settings = Object.assign(d.settings || {}, settings); d.status = "drafting"; d.draft = { order, pickIndex: 0, picks: {}, seq: {} }; write(id, d); },
      async setSeason(season) { const d = read(id); if (!d) return; d.season = season; d.status = "season"; write(id, d); },
      async makePick(subN, ctx) { const d = read(id); if (!d || !d.draft) return { error: "no_draft" }; const nd = applyPick(d.draft, subN, ctx.seat, ctx.pickIndex, ctx.round); if (!nd) return { error: "stale" }; d.draft = nd; if (nd.pickIndex >= ctx.total) d.status = "results"; write(id, d); return { ok: true }; },
      onLeague(fn) { cb = fn; emit(); },
      leave() { const d = read(id); if (d && d.presence) { delete d.presence[me]; write(id, d); } if (bc) bc.close(); if (hb) clearInterval(hb); },
    };
  }

  /* ---------------- Firebase adapter ---------------- */
  function FirebaseAdapter(config) {
    const me = myUid();
    let db = null, id = null, ref = null, cb = null;
    try {
      if (!window.firebase) return null;
      if (!window.firebase.apps || !window.firebase.apps.length) window.firebase.initializeApp(config);
      db = window.firebase.database();
    } catch (e) { console.warn("Firebase init failed", e); return null; }
    const setPresence = () => {
      const p = db.ref("leagues/" + id + "/presence/" + me);
      p.set(Date.now()); p.onDisconnect().remove();
    };
    const bind = () => { ref = db.ref("leagues/" + id); ref.on("value", (s) => { if (cb) cb(s.val()); }); };
    return {
      kind: "firebase", me,
      isConfigured: () => true,
      async createLeague(meta) {
        id = newCode();
        await db.ref("leagues/" + id).set({ id, host: me, name: meta.name, status: "lobby", settings: meta.settings, players: {}, presence: {}, draft: null });
        bind(); setPresence(); return id;
      },
      async joinLeague(lid) {
        id = lid;
        const snap = await db.ref("leagues/" + id).once("value");
        if (!snap.exists()) return { error: "not_found" };
        bind(); setPresence(); return { ok: true };
      },
      async setPlayer(p) { await db.ref("leagues/" + id + "/players/" + me).set({ name: p.name, allyId: p.allyId, ts: Date.now() }); setPresence(); },
      async setSettings(s) { await db.ref("leagues/" + id + "/settings").update(s); },
      async removePlayer(uid) { await db.ref("leagues/" + id + "/players/" + uid).remove(); await db.ref("leagues/" + id + "/presence/" + uid).remove(); },
      async startDraft(order, settings) {
        await db.ref("leagues/" + id).update({ status: "drafting", settings, draft: { order, pickIndex: 0, picks: {}, seq: {} } });
      },
      async setSeason(season) { await db.ref("leagues/" + id).update({ season, status: "season" }); },
      async makePick(subN, ctx) {
        const res = await db.ref("leagues/" + id + "/draft").transaction((dr) => {
          const nd = applyPick(dr, subN, ctx.seat, ctx.pickIndex, ctx.round);
          return nd == null ? undefined : nd;   // undefined aborts
        });
        if (res.committed) {
          const dr = res.snapshot.val();
          if (dr && dr.pickIndex >= ctx.total) await db.ref("leagues/" + id + "/status").set("results");
          return { ok: true };
        }
        return { error: "stale" };
      },
      onLeague(fn) { cb = fn; },
      leave() { if (ref) ref.off(); if (id) db.ref("leagues/" + id + "/presence/" + me).remove(); },
    };
  }

  function make() {
    const cfg = (typeof FIREBASE_CONFIG !== "undefined") ? FIREBASE_CONFIG : null;
    const configured = cfg && cfg.apiKey && !/PASTE|YOUR_/.test(cfg.apiKey) && cfg.databaseURL;
    if (configured) { const fa = FirebaseAdapter(cfg); if (fa) return fa; }
    return LocalAdapter();
  }

  return { make, myUid, newCode, get backend() { const cfg = (typeof FIREBASE_CONFIG !== "undefined") ? FIREBASE_CONFIG : null; const configured = cfg && cfg.apiKey && !/PASTE|YOUR_/.test(cfg.apiKey) && cfg.databaseURL; return (configured && typeof window !== "undefined" && window.firebase) ? "firebase" : "local"; } };
})();
