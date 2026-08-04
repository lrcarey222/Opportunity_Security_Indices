/* ============================================================================
   FANTASY SCORING MODEL  (shared by the news bot [Node] and the browser client)
   ----------------------------------------------------------------------------
   An "event" is a scored news headline:
     { id, ts, week, headline, url, source,
       countries: ["US","JP",...],   // ally ids the news involves
       sectors:   [10, 16, ...],       // sub-sector numbers the news involves
       pillar:    "NAT"|"ENSC"|"CLIM"|"OPP",   // which pillar it moves
       type:      "policy"|"partnership"|"investment"|"milestone"|"other",
       sentiment: +1 | -1,             // boosts (+) or harms (-)
       magnitude: 1..3 }               // how big a deal

   A "player" is a league seat:
     { country: "US", sectors: [subN...], leagueCountries: ["US","JP",...] }

   Points (per the league rules):
     • sector anywhere ....... base            (a sub-sector you drafted got news)
     • your country .......... base            (your country got news)
     • your sector in your
       country (EXTRA) ....... + base
     • partnership between you
       and a league ally
       (DOUBLE-EXTRA) ........ + 2·base
     base = sign(sentiment) · typeWeight · magnitude   (positive or negative)
   ============================================================================ */
(function (root) {
  const TYPE_WEIGHT = { policy: 3, partnership: 4, investment: 3, milestone: 2, other: 2 };

  function baseValue(ev) {
    const w = TYPE_WEIGHT[ev.type] || 2;
    const mag = Math.max(1, Math.min(3, ev.magnitude || 1));
    const sign = ev.sentiment < 0 ? -1 : 1;
    return sign * w * mag;   // ±2 .. ±12
  }

  /* points this event awards a given player (can be negative) */
  function scoreForPlayer(ev, player) {
    const base = baseValue(ev);
    const evSectors = ev.sectors || [], evCountries = ev.countries || [];
    const roster = player.sectors || [];
    const sectorHit = evSectors.some(s => roster.includes(s));
    const countryHit = evCountries.includes(player.country);
    if (!sectorHit && !countryHit) return 0;
    let pts = 0;
    if (sectorHit) pts += base;                     // your sub-sector, anywhere
    if (countryHit) pts += base;                    // your country, any sector
    if (sectorHit && countryHit) pts += base;       // EXTRA: your sector in your country
    if (countryHit && ev.type === "partnership") {  // DOUBLE-EXTRA: bilateral with a league ally
      const others = evCountries.filter(c => c !== player.country && (player.leagueCountries || []).includes(c));
      if (others.length) pts += base * 2;
    }
    return Math.round(pts);
  }

  /* labelled contributions, for the UI "why did I get these points" breakdown */
  function breakdown(ev, player) {
    const base = baseValue(ev);
    const evSectors = ev.sectors || [], evCountries = ev.countries || [];
    const roster = player.sectors || [];
    const sectorHit = evSectors.some(s => roster.includes(s));
    const countryHit = evCountries.includes(player.country);
    const parts = [];
    if (sectorHit) parts.push({ label: "Your sub-sector", pts: base });
    if (countryHit) parts.push({ label: "Your country", pts: base });
    if (sectorHit && countryHit) parts.push({ label: "Sector × country (extra)", pts: base });
    if (countryHit && ev.type === "partnership") {
      const others = evCountries.filter(c => c !== player.country && (player.leagueCountries || []).includes(c));
      if (others.length) parts.push({ label: "League-ally deal (double-extra)", pts: base * 2 });
    }
    return parts;
  }

  function touches(ev, player) {
    const roster = player.sectors || [];
    return (ev.sectors || []).some(s => roster.includes(s)) || (ev.countries || []).includes(player.country);
  }

  const API = { TYPE_WEIGHT, baseValue, scoreForPlayer, breakdown, touches };
  if (typeof module !== "undefined" && module.exports) module.exports = API;
  if (typeof window !== "undefined") window.SCORING = API;
})(typeof globalThis !== "undefined" ? globalThis : this);
