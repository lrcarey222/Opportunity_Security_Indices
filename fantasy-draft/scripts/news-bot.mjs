#!/usr/bin/env node
/* ============================================================================
   NEWS BOT — the "fantasy points from the news" engine.
   Runs on a schedule (GitHub Actions). For every active league it:
     1. pulls headlines from free Google-News RSS feeds across the stack,
     2. tags each headline with countries / sub-sectors / type / sentiment /
        pillar / magnitude (keyword rules; optional Anthropic AI upgrade),
     3. keeps the ones relevant to that league's countries or drafted sectors,
     4. writes them as point-events into Firebase Realtime DB (deduped).

   The browser client then tallys each player's weekly points with scoring.js.

   Env:
     FIREBASE_DB_URL     Realtime DB base url (else read from firebase-config.js)
     ANTHROPIC_API_KEY   optional — enables AI tagging for better accuracy
     ANTHROPIC_MODEL     optional — model id (default claude-3-5-haiku-latest)
     DRY_RUN=1           don't fetch leagues / don't write; print what it'd do
   ============================================================================ */
import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { dirname, join } from "node:path";
import { COUNTRY, SECTOR, TYPE, POS, NEG, PILLAR, BIGNUM } from "./lexicon.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));

/* ---------- feeds ---------- */
const FEEDS = [
  "energy transition supply chain OR clean energy manufacturing",
  "semiconductor OR chip export controls OR foundry OR chipmaker",
  "critical minerals OR rare earth OR lithium refining",
  "EV battery OR gigafactory OR cathode OR battery plant",
  "power grid transformer OR HVDC OR transmission cable",
  "solar manufacturing OR offshore wind OR onshore wind",
  "green hydrogen OR nuclear reactor OR SMR OR geothermal",
  "data center power OR heat pump OR industrial electrification",
].map(q => `https://news.google.com/rss/search?q=${encodeURIComponent(q)}&hl=en-US&gl=US&ceid=US:en`);

/* ---------- text matching ---------- */
const reCache = new Map();
function phraseRe(p) {
  if (!reCache.has(p)) reCache.set(p, new RegExp(`(?:^|[^a-z0-9])${p.replace(/[.*+?^${}()|[\]\\]/g, "\\$&")}s?(?:[^a-z0-9]|$)`, "i"));
  return reCache.get(p);
}
const has = (text, phrases) => phrases.some(p => phraseRe(p).test(text));
function countHits(text, phrases) { let n = 0; for (const p of phrases) if (phraseRe(p).test(text)) n++; return n; }

/* ---------- tag one headline (rules) ---------- */
export function tagHeadline(title) {
  const t = " " + title.toLowerCase() + " ";
  const countries = Object.keys(COUNTRY).filter(id => has(t, COUNTRY[id]));
  const sectors = Object.keys(SECTOR).map(Number).filter(n => has(t, SECTOR[n]));
  if (!sectors.length && !countries.length) return null;

  let type = "other";
  if (has(t, TYPE.partnership)) type = "partnership";
  else if (has(t, TYPE.policy)) type = "policy";
  else if (has(t, TYPE.investment)) type = "investment";
  else if (has(t, TYPE.milestone)) type = "milestone";

  const pos = countHits(t, POS), neg = countHits(t, NEG);
  if (type === "other" && pos + neg === 0) return null;   // not newsworthy enough
  const sentiment = neg > pos ? -1 : 1;
  const magnitude = Math.min(3, 1 + (has(t, BIGNUM) ? 1 : 0) + (type === "partnership" && countries.length >= 2 ? 1 : 0));

  let pillar = "ENSC", best = -1;
  for (const k in PILLAR) { const c = countHits(t, PILLAR[k]); if (c > best) { best = c; pillar = k; } }

  return { countries, sectors, type, sentiment, magnitude, pillar };
}

/* ---------- RSS parse (no XML dep) ---------- */
export function parseRss(xml) {
  const items = [];
  const blocks = xml.split(/<item>/).slice(1);
  for (const b of blocks) {
    const body = b.split(/<\/item>/)[0];
    const pick = (tag) => {
      const m = body.match(new RegExp(`<${tag}[^>]*>([\\s\\S]*?)</${tag}>`, "i"));
      if (!m) return "";
      return m[1].replace(/<!\[CDATA\[([\s\S]*?)\]\]>/g, "$1").replace(/<[^>]+>/g, "").trim();
    };
    let title = decodeEntities(pick("title"));
    const link = pick("link");
    const pubDate = pick("pubDate");
    let source = decodeEntities(pick("source"));
    // Google News titles are "Headline - Publisher"
    if (!source && / - [^-]+$/.test(title)) { const i = title.lastIndexOf(" - "); source = title.slice(i + 3); title = title.slice(0, i); }
    if (title) items.push({ title, link, pubDate, source });
  }
  return items;
}
function decodeEntities(s) {
  return (s || "").replace(/&amp;/g, "&").replace(/&lt;/g, "<").replace(/&gt;/g, ">").replace(/&quot;/g, '"').replace(/&#39;/g, "'").replace(/&apos;/g, "'").replace(/&nbsp;/g, " ");
}
function hashId(s) { let h = 5381; for (let i = 0; i < s.length; i++) h = ((h << 5) + h + s.charCodeAt(i)) | 0; return "e" + (h >>> 0).toString(36); }

/* ---------- optional AI tagging (Anthropic) ---------- */
async function aiTag(titles, key) {
  const model = process.env.ANTHROPIC_MODEL || "claude-3-5-haiku-latest";
  const prompt = `You score clean-energy / industrial supply-chain news for a fantasy game.
For each headline return JSON only: an array of objects with fields:
  i (index), countries (ISO-like ally codes from this set: US CA MX UK DE FR IT JP KR AU IN TW NL ES SE FI PL NO NZ SG TR CN; [] if none),
  sectors (integers 1-39 from the Electro-Industrial Stack that the news is about; [] if none),
  type ("policy"|"partnership"|"investment"|"milestone"|"other"),
  sentiment (1 if it boosts national/energy/economic/climate outcomes, -1 if it harms them),
  pillar ("NAT"|"ENSC"|"CLIM"|"OPP"), magnitude (1-3).
Headlines:\n${titles.map((h, i) => `${i}. ${h}`).join("\n")}`;
  const res = await fetch("https://api.anthropic.com/v1/messages", {
    method: "POST",
    headers: { "content-type": "application/json", "x-api-key": key, "anthropic-version": "2023-06-01" },
    body: JSON.stringify({ model, max_tokens: 2000, messages: [{ role: "user", content: prompt }] }),
  });
  if (!res.ok) throw new Error("anthropic " + res.status);
  const data = await res.json();
  const text = (data.content || []).map(c => c.text || "").join("");
  const m = text.match(/\[[\s\S]*\]/);
  return m ? JSON.parse(m[0]) : [];
}

/* ---------- Firebase REST ---------- */
function dbUrl() {
  if (process.env.FIREBASE_DB_URL) return process.env.FIREBASE_DB_URL.replace(/\/$/, "");
  try {
    const cfg = readFileSync(join(HERE, "..", "firebase-config.js"), "utf8");
    const m = cfg.match(/databaseURL:\s*"([^"]+)"/);
    if (m && !/PASTE/.test(m[1])) return m[1].replace(/\/$/, "");
  } catch (e) {}
  return null;
}
async function jget(url) { const r = await fetch(url); if (!r.ok) throw new Error("GET " + r.status); return r.json(); }
async function jpost(url, body) { const r = await fetch(url, { method: "POST", body: JSON.stringify(body) }); if (!r.ok) throw new Error("POST " + r.status); return r.json(); }
async function jput(url, body) { const r = await fetch(url, { method: "PUT", body: JSON.stringify(body) }); if (!r.ok) throw new Error("PUT " + r.status); return r.json(); }

/* league helpers */
function leagueTargets(lg) {
  const players = lg.players || {};
  const countries = new Set(), sectors = new Set();
  Object.values(players).forEach(p => p.allyId && countries.add(p.allyId));
  const seq = (lg.draft && lg.draft.seq) || {};
  Object.values(seq).forEach(e => sectors.add(Number(e.subN)));
  return { countries, sectors, leagueCountries: [...countries] };
}
function currentWeek(lg) {
  const s = lg.season; if (!s || !s.startTs) return 0;
  const wk = Math.floor((Date.now() - s.startTs) / (7 * 864e5));
  return Math.max(0, Math.min((s.weeks || 12) - 1, wk));
}

/* ---------- main ---------- */
async function main() {
  const dry = process.env.DRY_RUN === "1" || process.argv.includes("--dry");
  const key = process.env.ANTHROPIC_API_KEY;

  // 1. fetch + parse feeds
  let items = [];
  for (const url of FEEDS) {
    try { const xml = await (await fetch(url, { headers: { "user-agent": "Mozilla/5.0 osi-news-bot" } })).text(); items.push(...parseRss(xml)); }
    catch (e) { console.warn("feed failed", e.message); }
  }
  // dedup by link/title
  const seenLink = new Set();
  items = items.filter(it => { const k = it.link || it.title; if (seenLink.has(k)) return false; seenLink.add(k); return true; });
  console.log(`fetched ${items.length} unique headlines`);

  // 2. tag
  let tags = items.map(it => ({ it, tag: tagHeadline(it.title) }));
  if (key) {
    try {
      const cand = tags.filter(x => x.tag);              // AI-refine only the shortlisted ones
      const ai = await aiTag(cand.map(x => x.it.title), key);
      ai.forEach(a => { if (cand[a.i]) cand[a.i].tag = { countries: a.countries || [], sectors: (a.sectors || []).map(Number), type: a.type || "other", sentiment: a.sentiment < 0 ? -1 : 1, magnitude: a.magnitude || 1, pillar: a.pillar || "ENSC" }; });
      console.log(`AI-tagged ${ai.length} headlines`);
    } catch (e) { console.warn("AI tagging failed, using rules:", e.message); }
  }
  const events = tags.filter(x => x.tag).map(x => ({
    id: hashId(x.it.link || x.it.title),
    ts: Date.parse(x.it.pubDate) || Date.now(),
    headline: x.it.title, url: x.it.link, source: x.it.source || "",
    ...x.tag,
  }));
  console.log(`scored ${events.length} candidate events`);

  if (dry) { console.log(JSON.stringify(events.slice(0, 12), null, 2)); return; }

  // 3. write per league
  const DB = dbUrl();
  if (!DB) { console.error("No FIREBASE_DB_URL / databaseURL — cannot write. (set it or run with --dry)"); process.exit(1); }
  const leagues = (await jget(`${DB}/leagues.json`)) || {};
  let written = 0;
  for (const [id, lg] of Object.entries(leagues)) {
    if (!lg || !lg.players) continue;
    const { countries, sectors } = leagueTargets(lg);
    if (!countries.size && !sectors.size) continue;
    const week = currentWeek(lg);
    const seen = (await jget(`${DB}/leagues/${id}/seen.json`)) || {};
    for (const ev of events) {
      const relevant = ev.sectors.some(s => sectors.has(s)) || ev.countries.some(c => countries.has(c));
      if (!relevant || seen[ev.id]) continue;
      await jpost(`${DB}/leagues/${id}/events.json`, { ...ev, week });
      await jput(`${DB}/leagues/${id}/seen/${ev.id}.json`, ev.ts);
      written++;
    }
  }
  console.log(`wrote ${written} events across ${Object.keys(leagues).length} leagues`);
}

if (process.argv[1] && process.argv[1].endsWith("news-bot.mjs")) {
  main().catch(e => { console.error(e); process.exit(1); });
}
