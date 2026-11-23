/* Offline tests for the six-stage cards (data-osi.js + data.js + cards.js) and their news tagging.
   Run: node fantasy-draft/scripts/test-cards.mjs */
import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { dirname, join } from "node:path";
import vm from "node:vm";
import { cardsFor, loadCardIds, seasonOver } from "./news-bot.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const ctx = { window: {}, console };
ctx.window = ctx;
vm.createContext(ctx);
// the browser loads these as classic scripts sharing one global scope; concatenating keeps their consts visible
const src = ["data-osi.js", "data.js", "cards.js"].map(f => readFileSync(join(HERE, "..", f), "utf8")).join("\n;\n") +
  "\n;Object.assign(globalThis, { CARDS, SUB_BY_N, STAGE_ORDER, ALLY_BY_ID, ratingOf, fitOf, scoreFor, portfolioBonus, catMeta, FENCE_FIT, LEGACY_SUBSECTORS });";
vm.runInContext(src, ctx);

let pass = 0, fail = 0;
const ok = (cond, msg) => { if (cond) pass++; else { fail++; console.log("  ✗ " + msg); } };

const { CARDS, SUB_BY_N, ratingOf, fitOf, scoreFor, portfolioBonus, catMeta, FENCE_FIT, ALLY_BY_ID, LEGACY_SUBSECTORS } = ctx;

/* --- the card set --- */
ok(CARDS.length >= 30 && CARDS.length <= 66, `card count plausible (${CARDS.length})`);
ok(CARDS.every(c => c.n >= 100 && c.n < 300), "card ids are 100+ (never collide with legacy 1-39)");
ok(new Set(CARDS.map(c => c.n)).size === CARDS.length, "card ids unique");
ok(CARDS.every(c => c.name && c.glyph && c.tagline !== undefined), "every card has a name and icon");
ok(CARDS.every(c => c.nHigh + c.nMedium > 0), "no all-Low card");
ok(CARDS.filter(c => c.stage === "Deployment").every(c => c.PRIZE == null), "Deployment cards carry no prize");
ok(CARDS.some(c => c.tech === "Oil") && CARDS.some(c => c.tech === "Gas") && CARDS.some(c => c.tech === "Coal"), "fossil cards kept");
ok(LEGACY_SUBSECTORS.every(s => SUB_BY_N[s.n] === s), "legacy sub-sectors still resolve for old leagues");

/* --- ratings and fit --- */
const allies = Object.keys(ALLY_BY_ID);
ok(allies.every(a => CARDS.every(c => ratingOf(a, c.n))), "every game country rates every card");
const evRef = CARDS.find(c => c.tech === "Electric Vehicles" && c.stage === "Refining");
const us = ALLY_BY_ID.US;
const rUS = ratingOf("US", evRef.n);
ok(rUS && rUS.fence === "high" && rUS.choice === "secure", "US: EV Refining (rare earths) is High fence · Secure");
ok(fitOf(us, evRef) === FENCE_FIT.secure && scoreFor(us, evRef) === evRef.ovr + FENCE_FIT.secure, "Secure fit adds its points");
const legacy = SUB_BY_N[10];
ok(fitOf(us, legacy) === (us.home[legacy.cat] || 0) + (us.picks.includes(10) ? 6 : 0), "legacy fit unchanged for old leagues");
ok(catMeta(evRef).label.includes("Refining") && catMeta(legacy).label.startsWith("B"), "catMeta labels both kinds");

/* --- portfolio bonuses --- */
const id = (tech, stage) => CARDS.find(c => c.tech === tech && c.stage === stage).n;
const chain = [id("Batteries", "Extraction"), id("Batteries", "Refining"), id("Batteries", "Components"), id("Batteries", "Assembly")];
const b = portfolioBonus("US", chain, []);
ok(b.stages === 4 && b.coverage === 20, "coverage counts stages");
ok(b.combo === 3 * 6 + 10, `full battery chain: 3 links + full-chain bonus (${b.combo})`);
const split = portfolioBonus("US", [id("Batteries", "Extraction"), id("Batteries", "Components")], []);
ok(split.combo === 0, "non-adjacent stages are not a chain");
const partnerCard = CARDS.find(c => (ratingOf("US", c.n) || {}).choice === "partner");
ok(partnerCard && portfolioBonus("US", [], [partnerCard.n]).partner === 4, "a Partner card an ally holds pays the US");
ok(portfolioBonus("US", [partnerCard.n], []).partner === 0, "holding your own Partner card pays no ally bonus");

/* --- news tagging onto cards --- */
const ids = loadCardIds();
const T = (h) => cardsFor(" " + h.toLowerCase() + " ", ids);
ok(T("China curbs rare earth exports").includes(id("Electric Vehicles", "Refining")), "rare earths -> EV Refining");
ok(T("Australia approves new lithium mine").includes(id("Batteries", "Extraction")), "lithium mine -> Batteries Extraction");
ok(T("Korean battery maker opens recycling plant").includes(id("Batteries", "Recovery")), "battery recycling -> Batteries Recovery");
ok(T("India auctions record solar capacity").includes(id("Solar", "Deployment")), "solar auction -> Solar Deployment");
ok(T("Germany expands solar module factory").includes(id("Solar", "Assembly")), "solar module factory -> Solar Assembly");
ok(T("Saudi crude output cut lifts oil prices").includes(id("Oil", "Extraction")), "crude -> Oil Extraction");
ok(T("Local bakery wins award").length === 0, "irrelevant -> no card");

/* --- ended seasons take no events --- */
ok(seasonOver({ season: { startTs: Date.parse("2026-08-04"), weeks: 12 } }, Date.parse("2026-11-18")), "season that ended is over");
ok(!seasonOver({ season: { startTs: Date.parse("2026-11-01"), weeks: 12 } }, Date.parse("2026-11-18")), "running season is not over");
ok(!seasonOver({}), "league without a season is not over");

console.log(`cards: ${pass} passed, ${fail} failed`);
if (fail) process.exit(1);
