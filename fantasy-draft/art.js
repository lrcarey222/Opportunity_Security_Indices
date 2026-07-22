/* ============================================================================
   ART LIBRARY — hand-built inline SVG.
   mascotSVG(ally)  -> a chibi anime "country mascot" badge.
   iconSVG(glyph, c1, c2) -> an anime-styled industry icon badge.
   All self-contained; colors come from the data model.
   ============================================================================ */

/* ---- national emblem glyphs (drawn behind the mascot's face) ---- */
const EMBLEMS = {
  star:    `<path d="M0,-9 2.6,-2.8 9,-2.8 3.9,1.1 5.8,7.4 0,3.5 -5.8,7.4 -3.9,1.1 -9,-2.8 -2.6,-2.8Z"/>`,
  leaf:    `<path d="M0,-9 L1.4,-3 5,-4 3.2,-0.5 8,0.5 3.2,2 4,5 1,4 0.6,7 -0.6,7 -1,4 -4,5 -3.2,2 -8,0.5 -3.2,-0.5 -5,-4 -1.4,-3Z"/>`,
  eagle:   `<path d="M-8,-1 C-4,-4 -2,-2 0,-3 2,-2 4,-4 8,-1 4,0 3,3 0,2 -3,3 -4,0 -8,-1Z M-1,2 h2 v5 h-2Z"/>`,
  crown:   `<path d="M-8,4 -8,-4 -4,0 0,-6 4,0 8,-4 8,4Z"/>`,
  gear:    `<path d="M0,-9 1.6,-6.4 4.5,-7.8 4.6,-4.6 7.8,-4.5 6.4,-1.6 9,0 6.4,1.6 7.8,4.5 4.6,4.6 4.5,7.8 1.6,6.4 0,9 -1.6,6.4 -4.5,7.8 -4.6,4.6 -7.8,4.5 -6.4,1.6 -9,0 -6.4,-1.6 -7.8,-4.5 -4.6,-4.6 -4.5,-7.8 -1.6,-6.4Z M0,-3.4 A3.4,3.4 0 1 0 0.01,-3.4Z" fill-rule="evenodd"/>`,
  fleur:   `<path d="M0,-9 C2,-5 2,-3 0,-2 -2,-3 -2,-5 0,-9Z M0,-2 C3,-4 6,-2 5,2 3,1 1,0 0,-2Z M0,-2 C-3,-4 -6,-2 -5,2 -3,1 -1,0 0,-2Z M-3,3 h6 v2 h-6Z"/>`,
  cable:   `<path d="M-8,-4 C-2,-4 -2,4 4,4 M-8,4 C-2,4 -2,-4 4,-4" fill="none" stroke-width="2.4"/>`,
  sun:     `<circle r="6"/>`,
  trigram: `<rect x="-8" y="-6" width="16" height="2.4"/><rect x="-8" y="-1.2" width="6.5" height="2.4"/><rect x="1.5" y="-1.2" width="6.5" height="2.4"/><rect x="-8" y="3.6" width="16" height="2.4"/>`,
  sun2:    `<circle r="4.5"/><g stroke-width="1.6"><path d="M0,-8V-6M0,8V6M-8,0H-6M8,0H6M-5.6,-5.6l1.4,1.4M5.6,5.6l-1.4,-1.4M5.6,-5.6l-1.4,1.4M-5.6,5.6l1.4,-1.4" fill="none"/></g>`,
  chakra:  `<circle r="7" fill="none" stroke-width="1.6"/><g stroke-width="1"><path d="M0,-7V7M-7,0H7M-5,-5L5,5M5,-5L-5,5" fill="none"/></g>`,
  shield:  `<path d="M0,-8 7,-5 7,1 C7,5 4,7 0,8 -4,7 -7,5 -7,1 L-7,-5Z"/>`,
};

/* Build a chibi anime mascot badge for a country.
   Each call gets a unique id-suffix so paint servers never collide across
   multiple mascots on the same page (e.g. hidden setup screen + podium). */
let _mascotUID = 0;
function mascotSVG(ally, size = 128) {
  const id = ally.id + "-" + (++_mascotUID);
  return `
  <svg viewBox="0 0 128 128" width="${size}" height="${size}" class="mascot-svg" role="img" aria-label="${ally.mascot} mascot">
    <defs>
      <radialGradient id="mg-${id}" cx="38%" cy="30%" r="80%">
        <stop offset="0%" stop-color="${ally.c2}"/>
        <stop offset="100%" stop-color="${ally.c1}"/>
      </radialGradient>
      <linearGradient id="mh-${id}" x1="0" y1="0" x2="0" y2="1">
        <stop offset="0%" stop-color="${lighten(ally.hair, 28)}"/>
        <stop offset="100%" stop-color="${ally.hair}"/>
      </linearGradient>
      <clipPath id="mc-${id}"><circle cx="64" cy="64" r="60"/></clipPath>
    </defs>

    <circle cx="64" cy="64" r="62" fill="url(#mg-${id})" stroke="rgba(255,255,255,.55)" stroke-width="3"/>
    <g clip-path="url(#mc-${id})">
      <!-- emblem behind -->
      <g transform="translate(64 58) scale(3.1)" fill="rgba(255,255,255,.14)" stroke="rgba(255,255,255,.14)">${EMBLEMS[ally.emblem] || EMBLEMS.star}</g>
      <!-- sunrays -->
      <g opacity=".18" stroke="#fff" stroke-width="3">
        <path d="M64,64 L64,-10 M64,64 L120,8 M64,64 L138,64 M64,64 L120,120 M64,64 L8,120 M64,64 L-10,64 M64,64 L8,8"/>
      </g>

      <!-- hair back -->
      <path d="M22,70 C18,34 40,14 64,14 C88,14 110,34 106,70 C104,52 92,44 92,44 C94,58 88,66 88,66 L40,66 C40,66 34,58 36,44 C36,44 24,52 22,70Z" fill="url(#mh-${id})"/>

      <!-- face -->
      <ellipse cx="64" cy="70" rx="30" ry="31" fill="#ffe6d0"/>
      <ellipse cx="64" cy="70" rx="30" ry="31" fill="none" stroke="rgba(0,0,0,.06)" stroke-width="1"/>

      <!-- hair front bangs -->
      <path d="M34,58 C34,32 50,22 64,22 C78,22 94,32 94,58 C90,44 80,40 80,40 C82,50 78,54 78,54 C74,46 70,44 70,44 C71,50 68,53 68,53 C64,46 60,45 60,45 C61,50 58,53 58,53 C54,46 49,45 49,45 C50,50 48,54 48,54 C48,54 44,50 46,40 C46,40 38,44 34,58Z" fill="url(#mh-${id})"/>

      <!-- eyes -->
      <g>
        <ellipse cx="52" cy="72" rx="6.4" ry="8" fill="#fff"/>
        <ellipse cx="76" cy="72" rx="6.4" ry="8" fill="#fff"/>
        <ellipse cx="52.5" cy="73" rx="4.6" ry="6.2" fill="${ally.eyes}"/>
        <ellipse cx="76.5" cy="73" rx="4.6" ry="6.2" fill="${ally.eyes}"/>
        <circle cx="53.5" cy="70.5" r="1.9" fill="#fff"/>
        <circle cx="77.5" cy="70.5" r="1.9" fill="#fff"/>
        <circle cx="51" cy="75" r="1" fill="rgba(255,255,255,.7)"/>
        <circle cx="75" cy="75" r="1" fill="rgba(255,255,255,.7)"/>
      </g>
      <!-- brows -->
      <path d="M46,62 q6,-3 12,-1 M70,61 q6,-2 12,1" stroke="${ally.hair}" stroke-width="2" fill="none" stroke-linecap="round"/>
      <!-- blush -->
      <ellipse cx="45" cy="82" rx="5" ry="3" fill="#ff9db0" opacity=".55"/>
      <ellipse cx="83" cy="82" rx="5" ry="3" fill="#ff9db0" opacity=".55"/>
      <!-- nose + smile -->
      <path d="M63,80 q1,1.5 2,0" stroke="rgba(0,0,0,.25)" stroke-width="1.4" fill="none" stroke-linecap="round"/>
      <path d="M56,86 q8,7 16,0" stroke="#b5476b" stroke-width="2.2" fill="none" stroke-linecap="round"/>
    </g>
    <!-- flag chip -->
    <g transform="translate(96 96)">
      <circle r="16" fill="rgba(10,14,20,.72)" stroke="rgba(255,255,255,.5)" stroke-width="1.5"/>
      <text x="0" y="6" font-size="18" text-anchor="middle">${ally.flag}</text>
    </g>
  </svg>`;
}

/* -------------------- INDUSTRY ICON GLYPHS -------------------- */
/* Each returns SVG inner markup drawn in a 48x48 box, stroke=#fff style. */
const GLYPHS = {
  crystal:`<path d="M24 6 34 18 24 42 14 18Z M14 18H34 M24 6V42"/>`,
  chip:`<rect x="14" y="14" width="20" height="20" rx="2"/><rect x="20" y="20" width="8" height="8"/><path d="M18 14V8M30 14V8M18 34v6M30 34v6M14 18H8M14 30H8M34 18h6M34 30h6"/>`,
  tower:`<path d="M24 6V40 M14 40 24 8 34 40 M17 22H31 M15 30H33"/>`,
  sun:`<circle cx="24" cy="24" r="8"/><path d="M24 6V12M24 36v6M6 24h6M36 24h6M11 11l4 4M33 33l4 4M37 11l-4 4M15 33l-4 4"/>`,
  bolt:`<path d="M26 6 12 26h10l-4 16 18-22H24Z"/>`,
  atom:`<circle cx="24" cy="24" r="3.5"/><ellipse cx="24" cy="24" rx="18" ry="7"/><ellipse cx="24" cy="24" rx="18" ry="7" transform="rotate(60 24 24)"/><ellipse cx="24" cy="24" rx="18" ry="7" transform="rotate(120 24 24)"/>`,
  pickaxe:`<path d="M8 40 40 8 M10 12 C24 8 36 14 40 22 M6 18 C14 34 22 40 30 40"/>`,
  flask:`<path d="M20 8v12L10 38a3 3 0 0 0 3 4h22a3 3 0 0 0 3-4L28 20V8 M17 8h14 M15 30h18"/>`,
  layers:`<path d="M24 8 42 17 24 26 6 17Z M6 24 24 33 42 24 M6 31 24 40 42 31"/>`,
  prism:`<path d="M20 8 8 40h6l6-16 6 16h6L20 8Z M30 14l10 6-6 8"/>`,
  ingot:`<path d="M10 30 14 22h20l4 8Z M6 40 10 30h28l4 10Z"/>`,
  coil:`<path d="M12 16c0 8 24 8 24 0M12 24c0 8 24 8 24 0M12 32c0 8 24 8 24 0M12 16c0-8 24-8 24 0"/>`,
  droplet:`<path d="M24 6C14 20 12 28 12 32a12 12 0 0 0 24 0c0-4-2-12-12-26Z M24 34a4 4 0 0 1-4-4"/>`,
  wafer:`<circle cx="24" cy="24" r="16"/><path d="M32 10 14 40"/><path d="M14 20H30M14 28H34"/>`,
  hex:`<path d="M24 6 40 15V33L24 42 8 33V15Z M24 16v16M17 20l14 8M31 20l-14 8"/>`,
  cpu:`<rect x="12" y="12" width="24" height="24" rx="2"/><rect x="19" y="19" width="10" height="10" rx="1"/><path d="M17 12V6M24 12V6M31 12V6M17 36v6M24 36v6M31 36v6M12 17H6M12 24H6M12 31H6M36 17h6M36 24h6M36 31h6"/>`,
  sic:`<rect x="12" y="12" width="24" height="24" rx="3"/><path d="M20 20l8 8M28 20l-8 8"/><path d="M24 12V6M24 42v-6"/>`,
  inverter:`<rect x="8" y="14" width="32" height="20" rx="2"/><path d="M24 14v20"/><path d="M12 24h6l0-4M30 24h-6v4"/>`,
  sensor:`<circle cx="24" cy="24" r="5"/><circle cx="24" cy="24" r="11" fill="none"/><circle cx="24" cy="24" r="17" fill="none"/>`,
  cathode:`<rect x="10" y="14" width="28" height="20" rx="2"/><path d="M18 14v20M26 14v20M34 14v20"/><path d="M6 20v8"/>`,
  membrane:`<path d="M14 8v32M24 8v32M34 8v32"/><path d="M14 18h10M24 30h10M14 30h10"/>`,
  battery:`<rect x="10" y="14" width="26" height="20" rx="2"/><rect x="36" y="20" width="4" height="8"/><path d="M20 24l-3 0M28 20v8M25 24h6"/>`,
  storage:`<rect x="8" y="12" width="14" height="24" rx="2"/><rect x="26" y="12" width="14" height="24" rx="2"/><path d="M15 20v8M12 24h6M33 24h6"/>`,
  magnet:`<path d="M14 10v14a10 10 0 0 0 20 0V10 M14 10h8v14a2 2 0 0 0 4 0V10h8"/><path d="M14 34h8M26 34h8"/>`,
  motor:`<circle cx="22" cy="24" r="12"/><path d="M22 16v16M14 24h16" transform="rotate(45 22 24)"/><rect x="34" y="20" width="8" height="8"/>`,
  transformer:`<circle cx="18" cy="24" r="8"/><circle cx="30" cy="24" r="8"/><path d="M18 8v8M18 32v8M30 8v8M30 32v8"/>`,
  switch:`<circle cx="14" cy="16" r="3"/><circle cx="34" cy="32" r="3"/><path d="M14 16 32 30"/><path d="M14 16H8M40 32h-6"/>`,
  cable:`<path d="M8 16c8 0 8 16 16 16s8-16 16-16M8 32c8 0 8-16 16-16s8 16 16 16"/>`,
  hvdc:`<path d="M8 24h10l4-8 4 16 4-8h10"/><path d="M6 14v20M42 14v20"/>`,
  digitalgrid:`<path d="M12 12h10v10H12zM26 26h10v10H26z"/><path d="M22 17h8v9M17 22v8h9"/>`,
  microgrid:`<rect x="10" y="10" width="28" height="28" rx="4"/><path d="M24 16v6l-4 4h8l-4 4v6"/>`,
  solar:`<rect x="8" y="16" width="32" height="20" rx="2"/><path d="M14 16v20M24 16v20M34 16v20M8 26h32"/><path d="M24 6v6M16 10l3 4M32 10l-3 4"/>`,
  wind:`<circle cx="24" cy="16" r="3"/><path d="M24 16 24 6M24 16 33 21M24 16 15 21"/><path d="M22 19 20 42h8l-2-23"/>`,
  offshore:`<path d="M24 8v10M24 18 30 22M24 18 18 22M24 18v14"/><circle cx="24" cy="8" r="2.5"/><path d="M6 36c4 0 4 3 8 3s4-3 8-3 4 3 8 3 4-3 8-3"/>`,
  reactor:`<circle cx="24" cy="24" r="5"/><ellipse cx="24" cy="24" rx="16" ry="7"/><ellipse cx="24" cy="24" rx="16" ry="7" transform="rotate(60 24 24)"/><ellipse cx="24" cy="24" rx="16" ry="7" transform="rotate(120 24 24)"/>`,
  geo:`<path d="M8 40h32M14 40V24M24 40V16M34 40V24"/><path d="M14 24c0-6 4-6 4-12M24 16c0-6 5-6 5-12M34 24c0-5 3-5 3-10"/>`,
  turbine:`<circle cx="24" cy="24" r="6"/><path d="M24 18c-6-6-14-4-14-4s2 8 8 10M24 30c6 6 14 4 14 4s-2-8-8-10"/>`,
  ev:`<path d="M8 30l3-9a4 4 0 0 1 4-3h18a4 4 0 0 1 4 3l3 9v6h-6v-4H14v4H8Z"/><circle cx="16" cy="36" r="3"/><circle cx="32" cy="36" r="3"/><path d="M22 12l-3 5h5l-3 5"/>`,
  charger:`<rect x="14" y="10" width="16" height="30" rx="3"/><path d="M22 18l-3 6h6l-3 6"/><path d="M30 16h5v10a4 4 0 0 1-4 4"/>`,
  heatpump:`<rect x="10" y="12" width="28" height="24" rx="3"/><circle cx="24" cy="24" r="8"/><path d="M24 24l6-4M24 24l-2 7M24 24l-6-4"/>`,
  furnace:`<path d="M12 40V16a12 12 0 0 1 24 0v24 M18 40V26a6 6 0 0 1 12 0v14"/><path d="M24 24c2-2 2-4 0-6"/>`,
  datacenter:`<rect x="10" y="10" width="28" height="9" rx="1"/><rect x="10" y="21" width="28" height="9" rx="1"/><rect x="10" y="32" width="28" height="6" rx="1"/><path d="M15 14.5h.01M15 25.5h.01M30 14.5h4M30 25.5h4"/>`,
  electrolyser:`<rect x="10" y="12" width="12" height="24" rx="2"/><rect x="26" y="12" width="12" height="24" rx="2"/><path d="M16 20l-2 4h4l-2 4M32 20l-2 4h4l-2 4"/><path d="M22 24h4"/>`,
  fuel:`<path d="M18 10h8v6l4 4v18H14V20l4-4Z"/><path d="M14 28h16"/><path d="M22 40v-6"/>`,
  carbon:`<circle cx="24" cy="24" r="6"/><circle cx="12" cy="16" r="3"/><circle cx="36" cy="16" r="3"/><circle cx="24" cy="40" r="3"/><path d="M19 21 14 18M29 21 34 18M24 30v7"/>`,
};

function iconSVG(glyph, c1, c2, size = 48) {
  const gid = "ic-" + glyph;
  return `<svg viewBox="0 0 48 48" width="${size}" height="${size}" class="ind-icon" aria-hidden="true">
    <defs><linearGradient id="${gid}" x1="0" y1="0" x2="1" y2="1">
      <stop offset="0%" stop-color="${c2}"/><stop offset="100%" stop-color="${c1}"/>
    </linearGradient></defs>
    <rect x="2" y="2" width="44" height="44" rx="13" fill="url(#${gid})" opacity=".16"/>
    <rect x="2" y="2" width="44" height="44" rx="13" fill="none" stroke="url(#${gid})" stroke-width="1.5" opacity=".7"/>
    <g fill="none" stroke="${c2}" stroke-width="2.1" stroke-linecap="round" stroke-linejoin="round">
      ${GLYPHS[glyph] || GLYPHS.chip}
    </g>
  </svg>`;
}

/* tiny color helper: lighten a hex by pct toward white */
function lighten(hex, pct) {
  const h = hex.replace("#", "");
  const n = h.length === 3 ? h.split("").map(c => c + c).join("") : h;
  const r = parseInt(n.slice(0, 2), 16), g = parseInt(n.slice(2, 4), 16), b = parseInt(n.slice(4, 6), 16);
  const f = pct / 100;
  const mix = v => Math.round(v + (255 - v) * f);
  return `#${[mix(r), mix(g), mix(b)].map(v => v.toString(16).padStart(2, "0")).join("")}`;
}
