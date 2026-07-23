/* ============================================================================
   ALLIED INDUSTRIAL POLICY — FANTASY DRAFT
   Data model: Allies (countries), Sub-sectors (the Electro-Industrial Stack),
   category metadata, and the SVG art library (anime mascots + industry icons).
   Everything is self-contained — no external assets.
   ============================================================================ */

/* ---------------------------------------------------------------------------
   CATEGORY METADATA  (the six "positions" of the Electro-Industrial Stack)
   --------------------------------------------------------------------------- */
const CATEGORIES = {
  A: { id: "A", name: "Upstream Materials & Processed Inputs", short: "MATERIALS",
       c1: "#8b5cf6", c2: "#c084fc", glyph: "crystal",
       blurb: "The dirt-to-metal foundation. Own the mine, own the stack." },
  B: { id: "B", name: "Semiconductors, Electrochemistry & Machines", short: "SILICON & CELLS",
       c1: "#0ea5e9", c2: "#38bdf8", glyph: "chip",
       blurb: "Brains, batteries and motion. The compounding-returns tier." },
  C: { id: "C", name: "Grid & Electricity-System Equipment", short: "THE GRID",
       c1: "#f59e0b", c2: "#fbbf24", glyph: "tower",
       blurb: "Copper spine of the electro-state. Boring, unglamorous, decisive." },
  D: { id: "D", name: "Power-Generation Technologies", short: "GENERATION",
       c1: "#10b981", c2: "#34d399", glyph: "sun",
       blurb: "Electrons at the source — sun, wind, atom, heat." },
  E: { id: "E", name: "Electrified End-Use Systems", short: "END-USE",
       c1: "#ef4444", c2: "#fb7185", glyph: "bolt",
       blurb: "Where demand becomes real: wheels, heat, and data." },
  F: { id: "F", name: "Molecules, Materials & Carbon Management", short: "MOLECULES",
       c1: "#14b8a6", c2: "#2dd4bf", glyph: "atom",
       blurb: "Hydrogen, e-fuels and near-zero materials. The frontier stack." },
};

/* ---------------------------------------------------------------------------
   SUB-SECTORS  (the 39 draftable "players")
   stats (0-100), from the OSI scoring table:
     NAT  = National Security
     ENSC = Energy & Economic Security
     CLIM = Climate Salience
     OPP  = Economic Opportunity
   ovr is the mean of the four pillars. cost is a salary-cap value (credits).
   glyph = key into the icon art library.
   --------------------------------------------------------------------------- */
const SUBSECTORS = [
  // A — Upstream materials and processed inputs
  { n: 1,  cat: "A", name: "Critical-Mineral Extraction", glyph: "pickaxe",
    tagline: "Dig where the future is buried.", NAT: 80, ENSC: 85, CLIM: 75, OPP: 75 },
  { n: 2,  cat: "A", name: "Battery-Mineral Refining & Chemical Conversion", glyph: "flask",
    tagline: "The step China quietly owns.", NAT: 85, ENSC: 95, CLIM: 85, OPP: 85 },
  { n: 3,  cat: "A", name: "Graphite & Anode Feedstocks", glyph: "layers",
    tagline: "Every cell needs an anode.", NAT: 85, ENSC: 95, CLIM: 85, OPP: 80 },
  { n: 4,  cat: "A", name: "Rare-Earth Separation & Metallization", glyph: "prism",
    tagline: "Seventeen elements, one chokepoint.", NAT: 100, ENSC: 95, CLIM: 80, OPP: 80 },
  { n: 5,  cat: "A", name: "Copper & Aluminum Materials", glyph: "ingot",
    tagline: "You cannot electrify without them.", NAT: 75, ENSC: 90, CLIM: 85, OPP: 75 },
  { n: 6,  cat: "A", name: "Electrical & Specialty Steels", glyph: "coil",
    tagline: "Grain-oriented steel = every transformer.", NAT: 85, ENSC: 90, CLIM: 80, OPP: 75 },
  { n: 7,  cat: "A", name: "Polysilicon & High-Purity Silicon", glyph: "droplet",
    tagline: "9-nines pure or bust.", NAT: 70, ENSC: 85, CLIM: 90, OPP: 70 },
  { n: 8,  cat: "A", name: "Semiconductor & Specialty Electronic Materials", glyph: "wafer",
    tagline: "Photoresists, gases, and quiet monopolies.", NAT: 95, ENSC: 90, CLIM: 60, OPP: 85 },
  { n: 9,  cat: "A", name: "Uranium Conversion, Enrichment & Nuclear Fuels", glyph: "hex",
    tagline: "Firm power starts here.", NAT: 100, ENSC: 95, CLIM: 85, OPP: 70 },

  // B — Semiconductors, electrochemical components, electric machines
  { n: 10, cat: "B", name: "Logic, Memory, Analog & Mature-Node Semiconductors", glyph: "cpu",
    tagline: "The most valuable rocks on Earth.", NAT: 100, ENSC: 100, CLIM: 60, OPP: 95 },
  { n: 11, cat: "B", name: "Power Semiconductors", glyph: "sic",
    tagline: "SiC & GaN: efficiency, weaponized.", NAT: 95, ENSC: 95, CLIM: 90, OPP: 95 },
  { n: 12, cat: "B", name: "Power Electronics & Conversion Equipment", glyph: "inverter",
    tagline: "Inverters translate the whole stack.", NAT: 90, ENSC: 95, CLIM: 90, OPP: 90 },
  { n: 13, cat: "B", name: "Sensors, Controls & Industrial Electronics", glyph: "sensor",
    tagline: "The nervous system of the factory.", NAT: 100, ENSC: 95, CLIM: 65, OPP: 90 },
  { n: 14, cat: "B", name: "Battery Cathode Precursors & Active Materials", glyph: "cathode",
    tagline: "Where cell chemistry is decided.", NAT: 85, ENSC: 95, CLIM: 90, OPP: 90 },
  { n: 15, cat: "B", name: "Electrolytes, Separators & Current Collectors", glyph: "membrane",
    tagline: "The unglamorous cell internals.", NAT: 80, ENSC: 90, CLIM: 85, OPP: 80 },
  { n: 16, cat: "B", name: "Battery Cells, Modules, Packs & BMS", glyph: "battery",
    tagline: "The gigafactory trophy pick.", NAT: 95, ENSC: 95, CLIM: 95, OPP: 95 },
  { n: 17, cat: "B", name: "Stationary & Long-Duration Energy Storage", glyph: "storage",
    tagline: "Firming the intermittent future.", NAT: 75, ENSC: 95, CLIM: 95, OPP: 90 },
  { n: 18, cat: "B", name: "Permanent Magnets", glyph: "magnet",
    tagline: "No magnet, no motor.", NAT: 100, ENSC: 95, CLIM: 85, OPP: 85 },
  { n: 19, cat: "B", name: "Electric Motors, Generators & Actuators", glyph: "motor",
    tagline: "Torque is destiny.", NAT: 95, ENSC: 95, CLIM: 90, OPP: 95 },

  // C — Grid and electricity-system equipment
  { n: 20, cat: "C", name: "Transformers", glyph: "transformer",
    tagline: "36-month lead times. Draft early.", NAT: 90, ENSC: 100, CLIM: 90, OPP: 85 },
  { n: 21, cat: "C", name: "Switchgear, Breakers & Protection Systems", glyph: "switch",
    tagline: "The grid's circuit-breaker moat.", NAT: 90, ENSC: 100, CLIM: 90, OPP: 85 },
  { n: 22, cat: "C", name: "Cables, Conductors & HV Transmission", glyph: "cable",
    tagline: "Copper by the continent.", NAT: 80, ENSC: 100, CLIM: 95, OPP: 90 },
  { n: 23, cat: "C", name: "HVDC & Flexible Transmission Systems", glyph: "hvdc",
    tagline: "Move gigawatts across a continent.", NAT: 80, ENSC: 95, CLIM: 95, OPP: 90 },
  { n: 24, cat: "C", name: "Digital Grids & Grid-Enhancing Tech", glyph: "digitalgrid",
    tagline: "Squeeze 30% more from the wires you have.", NAT: 95, ENSC: 100, CLIM: 95, OPP: 95 },
  { n: 25, cat: "C", name: "Microgrids & Resilient Power Systems", glyph: "microgrid",
    tagline: "Islands of power when the grid fails.", NAT: 95, ENSC: 95, CLIM: 80, OPP: 80 },

  // D — Power-generation technologies
  { n: 26, cat: "D", name: "Solar Photovoltaic Manufacturing", glyph: "solar",
    tagline: "Cheapest electrons in history.", NAT: 50, ENSC: 90, CLIM: 100, OPP: 80 },
  { n: 27, cat: "D", name: "Onshore Wind Turbine Manufacturing", glyph: "wind",
    tagline: "The mature-tech workhorse.", NAT: 50, ENSC: 80, CLIM: 90, OPP: 65 },
  { n: 28, cat: "D", name: "Offshore & Floating Wind Systems", glyph: "offshore",
    tagline: "Gigawatts from the deep water.", NAT: 65, ENSC: 80, CLIM: 90, OPP: 75 },
  { n: 29, cat: "D", name: "Nuclear Reactor Systems & Components", glyph: "reactor",
    tagline: "Firm, dense, and back in fashion.", NAT: 95, ENSC: 95, CLIM: 95, OPP: 90 },
  { n: 30, cat: "D", name: "Geothermal & Subsurface Energy", glyph: "geo",
    tagline: "Drill for heat, not oil.", NAT: 70, ENSC: 90, CLIM: 90, OPP: 85 },
  { n: 31, cat: "D", name: "Other Firm & Flexible Power Equipment", glyph: "turbine",
    tagline: "Turbines that fill the gaps.", NAT: 80, ENSC: 95, CLIM: 60, OPP: 75 },

  // E — Electrified end-use systems
  { n: 32, cat: "E", name: "Electric Vehicles & Electric Drivetrains", glyph: "ev",
    tagline: "The consumer face of the transition.", NAT: 80, ENSC: 85, CLIM: 95, OPP: 100 },
  { n: 33, cat: "E", name: "Charging & Grid-Interactive Mobility", glyph: "charger",
    tagline: "Every EV needs a plug.", NAT: 50, ENSC: 80, CLIM: 90, OPP: 85 },
  { n: 34, cat: "E", name: "Heat Pumps, Compressors & Advanced Cooling", glyph: "heatpump",
    tagline: "3x efficiency, quietly enormous.", NAT: 40, ENSC: 75, CLIM: 95, OPP: 85 },
  { n: 35, cat: "E", name: "Industrial Electrification & Process Heat", glyph: "furnace",
    tagline: "Decarbonize the hard-to-abate.", NAT: 75, ENSC: 90, CLIM: 100, OPP: 95 },
  { n: 36, cat: "E", name: "Data-Center Power & Cooling Systems", glyph: "datacenter",
    tagline: "The AI-era load monster.", NAT: 100, ENSC: 100, CLIM: 65, OPP: 100 },

  // F — Molecules, materials, and carbon management
  { n: 37, cat: "F", name: "Electrolysers & Hydrogen Production", glyph: "electrolyser",
    tagline: "Split water, store sunshine.", NAT: 65, ENSC: 75, CLIM: 85, OPP: 75 },
  { n: 38, cat: "F", name: "Hydrogen Derivatives & Low-Emissions Fuels", glyph: "fuel",
    tagline: "Ammonia, e-methanol, SAF.", NAT: 75, ENSC: 80, CLIM: 95, OPP: 80 },
  { n: 39, cat: "F", name: "Near-Zero Materials & Carbon Management", glyph: "carbon",
    tagline: "Green steel, cement, and carbon capture.", NAT: 75, ENSC: 85, CLIM: 100, OPP: 90 },
];

// Derive OVR + cost + tier for each sub-sector (OVR = mean of the four pillars).
SUBSECTORS.forEach(s => {
  s.ovr = Math.round((s.NAT + s.ENSC + s.CLIM + s.OPP) / 4);
  s.cost = Math.max(6, Math.round((s.ovr - 62) * 3.4));   // salary-cap credits
  s.tier = s.ovr >= 93 ? "S" : s.ovr >= 88 ? "A" : s.ovr >= 80 ? "B" : "C";
});

/* ---------------------------------------------------------------------------
   ALLIES (the 12 drafting teams) + their anime mascots.
   home: per-category synergy bonuses (reflecting real industrial strengths).
   picks[]: specific sub-sector numbers that trigger a "signature" bonus.
   --------------------------------------------------------------------------- */
const ALLIES = [
  { id: "US", name: "United States", flag: "🇺🇸", mascot: "Liberty Volt",
    title: "The Leader", c1: "#3c5cc4", c2: "#e23b52", hair: "#2b3a8f",
    emblem: "star", eyes: "#2b6fff",
    home: { B: 7, D: 6, E: 6, F: 5, A: 3 }, picks: [10, 29, 36],
    bio: "Deep capital markets, frontier fabs, and an AI-load appetite that reshapes the grid. Wins on breadth." },
  { id: "CA", name: "Canada", flag: "🇨🇦", mascot: "Maple Kaede",
    title: "The Provider", c1: "#d52b1e", c2: "#ffffff", hair: "#b81c1c",
    emblem: "leaf", eyes: "#e23b52",
    home: { A: 9, D: 6, F: 5 }, picks: [1, 9, 5],
    bio: "Critical minerals, uranium, and hydro to spare. The upstream powerhouse of the alliance." },
  { id: "MX", name: "Mexico", flag: "🇲🇽", mascot: "Sol Nearshora",
    title: "The Manufacturer", c1: "#116149", c2: "#c8102e", hair: "#0e7a4f",
    emblem: "eagle", eyes: "#16a34a",
    home: { E: 8, B: 5, A: 4, C: 3 }, picks: [32, 22, 5],
    bio: "The assembly floor of North America. Auto muscle plus a copper backbone and a friendly border." },
  { id: "UK", name: "United Kingdom", flag: "🇬🇧", mascot: "Britannia Aoi",
    title: "The Strategist", c1: "#012169", c2: "#c8102e", hair: "#1b2a63",
    emblem: "crown", eyes: "#3b6fe0",
    home: { D: 8, F: 6, B: 4, C: 4 }, picks: [28, 39, 37],
    bio: "Offshore-wind pioneer with world-class finance and carbon-management ambitions." },
  { id: "DE", name: "Germany", flag: "🇩🇪", mascot: "Elektra Falkenberg",
    title: "The Engineer", c1: "#111111", c2: "#dd0000", hair: "#c9a227",
    emblem: "gear", eyes: "#d4af37",
    home: { B: 8, E: 7, C: 6, F: 4 }, picks: [12, 19, 34],
    bio: "The Mittelstand's precision machines, power electronics, and grid gear. Engineering incarnate." },
  { id: "FR", name: "France", flag: "🇫🇷", mascot: "Marianne Lumière",
    title: "The Visionary", c1: "#0055a4", c2: "#ef4135", hair: "#1e40af",
    emblem: "fleur", eyes: "#2563eb",
    home: { D: 9, C: 6, F: 5, A: 3 }, picks: [29, 9, 20],
    bio: "Nuclear royalty. A firm-power grid and rare-earth ambitions anchor the continent." },
  { id: "IT", name: "Italy", flag: "🇮🇹", mascot: "Aurora Cavo",
    title: "The Craftsman", c1: "#008c45", c2: "#cd212a", hair: "#0a7a3f",
    emblem: "cable", eyes: "#16a34a",
    home: { C: 8, E: 6, B: 4, D: 4 }, picks: [22, 23, 34],
    bio: "Home of the world's great cable-layers. Threads the alliance together, HVDC line by line." },
  { id: "JP", name: "Japan", flag: "🇯🇵", mascot: "Hikari Denki",
    title: "The Perfectionist", c1: "#bc002d", c2: "#ffffff", hair: "#2b2b3a",
    emblem: "sun", eyes: "#d11a3a",
    home: { B: 8, A: 6, E: 6, F: 5 }, picks: [8, 11, 34],
    bio: "Quiet monopolies in electronic materials, power semis, and precision. Never draft against Japan on quality." },
  { id: "KR", name: "South Korea", flag: "🇰🇷", mascot: "Bateri Haneul",
    title: "The Powerhouse", c1: "#003478", c2: "#c60c30", hair: "#12245c",
    emblem: "trigram", eyes: "#2447a8",
    home: { B: 9, E: 6, D: 4 }, picks: [16, 14, 10],
    bio: "Battery gigafactories and memory fabs. The electrochemistry MVP of the alliance." },
  { id: "AU", name: "Australia", flag: "🇦🇺", mascot: "Sunny Wattle",
    title: "The Resource", c1: "#00247d", c2: "#ffcd00", hair: "#c9a227",
    emblem: "sun2", eyes: "#f5b301",
    home: { A: 9, D: 7, F: 6 }, picks: [1, 26, 37],
    bio: "Lithium, rare earths, and endless sun. The raw-materials and green-hydrogen quarry of the Pacific." },
  { id: "IN", name: "India", flag: "🇮🇳", mascot: "Surya Shakti",
    title: "The Emerging Giant", c1: "#ff9933", c2: "#138808", hair: "#1a1a1a",
    emblem: "chakra", eyes: "#f97316",
    home: { D: 8, A: 5, E: 5, C: 4 }, picks: [26, 16, 32],
    bio: "Gigascale solar ambition and a billion-person demand base. The alliance's rising growth engine." },
  { id: "TW", name: "Taiwan", flag: "🇹🇼", mascot: "Silica Tienchi",
    title: "The Chip Master", c1: "#000095", c2: "#fe0000", hair: "#111827",
    emblem: "shield", eyes: "#1d4ed8",
    home: { B: 10, C: 4, E: 4 }, picks: [10, 11, 8],
    bio: "The most concentrated chokepoint on Earth: advanced logic. Draft Taiwan, own the brains." },

  // ----- Extended roster (the full 21-fighter card) -----
  { id: "NL", name: "Netherlands", flag: "🇳🇱", mascot: "Volt van Oranje",
    title: "The Connector", c1: "#1b3a8f", c2: "#ff7a1a", hair: "#d9a441",
    emblem: "crown", eyes: "#2b6fff",
    home: { B: 8, C: 5, D: 4, F: 4 }, picks: [10, 8, 28],
    bio: "ASML's lithography monopoly and Rotterdam's logistics muscle. The chip-equipment linchpin of the West." },
  { id: "ES", name: "Spain", flag: "🇪🇸", mascot: "Sol Ibérico",
    title: "The Integrator", c1: "#c60b1e", c2: "#ffc400", hair: "#2a1a12",
    emblem: "crown", eyes: "#b8860b",
    home: { D: 7, C: 5, E: 5, F: 4 }, picks: [26, 22, 32],
    bio: "Utility-grade grids, abundant sun and wind, and a growing EV base. Ties the whole system together." },
  { id: "SE", name: "Sweden", flag: "🇸🇪", mascot: "Nord Volt",
    title: "The Innovator", c1: "#005293", c2: "#fecb00", hair: "#e8c85a",
    emblem: "sun2", eyes: "#2b7bff",
    home: { B: 7, F: 6, A: 5, E: 4 }, picks: [16, 39, 19],
    bio: "Battery gigafactories, fossil-free steel, and deep engineering. The clean-industry lab of Europe." },
  { id: "FI", name: "Finland", flag: "🇫🇮", mascot: "Aava Sähkö",
    title: "The Purifier", c1: "#003580", c2: "#7db4e6", hair: "#caa24a",
    emblem: "star", eyes: "#2b6fff",
    home: { A: 6, B: 6, D: 5, C: 4 }, picks: [2, 14, 29],
    bio: "Battery chemicals, clean refining and new nuclear. Purifies the upstream of the stack." },
  { id: "PL", name: "Poland", flag: "🇵🇱", mascot: "Iskra Biała",
    title: "The Producer", c1: "#dc143c", c2: "#ff5a6e", hair: "#b8863f",
    emblem: "eagle", eyes: "#c0392b",
    home: { B: 6, A: 6, E: 5, C: 3 }, picks: [16, 5, 32],
    bio: "KGHM copper plus Europe's fastest-growing battery and EV assembly base. The workshop of the alliance." },
  { id: "NO", name: "Norway", flag: "🇳🇴", mascot: "Fjord Kraft",
    title: "The Energy Keeper", c1: "#ba0c2f", c2: "#3a6fd0", hair: "#c9a24a",
    emblem: "star", eyes: "#2b6fff",
    home: { D: 7, F: 7, A: 6 }, picks: [28, 39, 7],
    bio: "Hydro, offshore wind, silicon and carbon capture. Keeper of firm, clean energy." },
  { id: "NZ", name: "New Zealand", flag: "🇳🇿", mascot: "Aotea Ranginui",
    title: "The Supporter", c1: "#012169", c2: "#c8102e", hair: "#1a1a1a",
    emblem: "star", eyes: "#1d4ed8",
    home: { D: 7, F: 4, A: 4, E: 3 }, picks: [30, 26, 37],
    bio: "Geothermal mastery and near-total renewable power. A steady hand supporting the alliance." },
  { id: "SG", name: "Singapore", flag: "🇸🇬", mascot: "Merlion Volt",
    title: "The Hub", c1: "#ef3340", c2: "#ff7a86", hair: "#141414",
    emblem: "star", eyes: "#b22234",
    home: { B: 7, E: 6, C: 4, F: 3 }, picks: [10, 36, 13],
    bio: "Semiconductor fabs, electronics, and the trade-and-data crossroads of Asia. The connective hub." },
  { id: "TR", name: "Türkiye", flag: "🇹🇷", mascot: "Boğaz Şimşek",
    title: "The Bridge", c1: "#e30a17", c2: "#ff5a63", hair: "#2a1a12",
    emblem: "sun", eyes: "#8b1a1a",
    home: { C: 6, E: 6, A: 5, D: 4 }, picks: [20, 32, 26],
    bio: "Steel, transformers, EVs and solar spanning two continents. The bridge between the blocs." },
];

/* Fast lookup helpers */
const ALLY_BY_ID = Object.fromEntries(ALLIES.map(a => [a.id, a]));
const SUB_BY_N = Object.fromEntries(SUBSECTORS.map(s => [s.n, s]));

/* Fantasy score of a sub-sector for a given ally (OVR + home synergy + signature) */
function scoreFor(ally, sub) {
  let score = sub.ovr;
  score += (ally.home[sub.cat] || 0);
  if (ally.picks.includes(sub.n)) score += 6;   // signature-fit bonus
  return score;
}
