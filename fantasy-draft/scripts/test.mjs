/* Offline unit tests for the news pipeline + scoring model (no network). */
import { createRequire } from "node:module";
import { tagHeadline, parseRss } from "./news-bot.mjs";
const require = createRequire(import.meta.url);
const SCORING = require("../scoring.js");

let pass = 0, fail = 0;
const ok = (cond, msg) => { if (cond) { pass++; } else { fail++; console.log("  ✗ " + msg); } };

/* --- tagging --- */
const t1 = tagHeadline("US and Japan sign critical minerals partnership to boost supply chains");
ok(t1 && t1.type === "partnership", "t1 type partnership");
ok(t1 && t1.countries.includes("US") && t1.countries.includes("JP"), "t1 countries US+JP");
ok(t1 && t1.sectors.includes(1), "t1 sector 1 (critical minerals)");
ok(t1 && t1.sentiment === 1, "t1 positive");

const t2 = tagHeadline("China imposes export controls on rare earth magnets, disrupting supply");
ok(t2 && t2.sentiment === -1, "t2 negative");
ok(t2 && (t2.sectors.includes(4) || t2.sectors.includes(18)), "t2 sector rare-earth/magnet");
ok(t2 && t2.countries.includes("CN"), "t2 China");

const t3 = tagHeadline("Germany invests 2 billion in offshore wind manufacturing");
ok(t3 && t3.type === "investment", "t3 investment");
ok(t3 && t3.sectors.includes(28), "t3 offshore wind");
ok(t3 && t3.magnitude >= 2, "t3 magnitude >=2 (billion)");

ok(tagHeadline("Local bakery wins award for sourdough") === null, "irrelevant headline -> null");

/* --- scoring --- */
const player = { country: "US", sectors: [1, 18, 28], leagueCountries: ["US", "JP", "DE"] };
const s1 = SCORING.scoreForPlayer({ ...t1 }, player);
ok(s1 > 0, "s1 positive for US player");
// t1: base = partnership(4)*mag(2)=8; sector(1)+country(US)+extra+bilateral(JP)*2 = 8*(1+1+1) + 16 = 40
ok(s1 === 40, "s1 == 40 (sector+country+extra+bilateral), got " + s1);

const s2 = SCORING.scoreForPlayer({ ...t2 }, player);   // magnets, not US -> sector only, negative
ok(s2 < 0, "s2 negative (bad news for a rostered sector), got " + s2);

const s3 = SCORING.scoreForPlayer({ ...t3 }, player);   // DE offshore wind; player has 28 but not DE
ok(s3 > 0, "s3 positive (own sector, other country), got " + s3);
const demander = { country: "DE", sectors: [28], leagueCountries: ["US", "JP", "DE"] };
const s3b = SCORING.scoreForPlayer({ ...t3 }, demander); // DE + sector 28 -> sector+country+extra
ok(s3b === s3 * 3 / 1 || s3b > s3, "s3b (DE owner) > s3, got " + s3b + " vs " + s3);

const none = SCORING.scoreForPlayer({ ...t3 }, { country: "FR", sectors: [10], leagueCountries: ["FR"] });
ok(none === 0, "unrelated player scores 0");

/* --- scoreDetail: decomposition must reconcile with scoreForPlayer --- */
for (const [ev, pl, label] of [[t1, player, "t1/US"], [t2, player, "t2/US"], [t3, demander, "t3/DE"]]) {
  const d = SCORING.scoreDetail({ ...ev }, pl);
  const total = SCORING.scoreForPlayer({ ...ev }, pl);
  ok(d.sectorPortion + d.countryPortion === total, `scoreDetail reconciles (${label}): ${d.sectorPortion}+${d.countryPortion} vs ${total}`);
}
const dd = SCORING.scoreDetail({ ...t1 }, player); // partnership: sector part = base(sector)+base(extra)=16; country part = base(country)+2*base(bilateral)=24
ok(dd.sectorPortion === 16 && dd.countryPortion === 24, `t1 split 16/24, got ${dd.sectorPortion}/${dd.countryPortion}`);
ok(dd.matched.includes(1), "t1 matched sector 1");

/* --- rss parse --- */
const xml = `<rss><channel>
<item><title>US, Australia sign lithium deal - Reuters</title><link>http://x/1</link><pubDate>Mon, 21 Jul 2026 10:00:00 GMT</pubDate></item>
<item><title><![CDATA[Solar module prices tumble amid glut]]></title><link>http://x/2</link><source>FT</source></item>
</channel></rss>`;
const items = parseRss(xml);
ok(items.length === 2, "parsed 2 items, got " + items.length);
ok(items[0].title === "US, Australia sign lithium deal" && items[0].source === "Reuters", "split ' - Publisher' from title");
ok(items[1].title === "Solar module prices tumble amid glut", "CDATA title parsed");

console.log(`\n${pass} passed, ${fail} failed`);
process.exit(fail ? 1 : 0);
