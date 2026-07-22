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
   stats: STRAT (strategic value), CHOKE (chokepoint control),
          GROWTH (growth upside), CAPEX (capital intensity), MOAT (tech moat)
   ovr is derived. cost is a salary-cap value (in "GW$" credits).
   glyph = key into the icon art library.
   --------------------------------------------------------------------------- */
const SUBSECTORS = [
  // A — Upstream materials and processed inputs
  { n: 1,  cat: "A", name: "Critical-Mineral Extraction", glyph: "pickaxe",
    tagline: "Dig where the future is buried.", STRAT: 95, CHOKE: 88, GROWTH: 78, CAPEX: 86, MOAT: 62 },
  { n: 2,  cat: "A", name: "Battery-Mineral Refining & Chemical Conversion", glyph: "flask",
    tagline: "The step China quietly owns.", STRAT: 96, CHOKE: 94, GROWTH: 84, CAPEX: 80, MOAT: 74 },
  { n: 3,  cat: "A", name: "Graphite & Anode Feedstocks", glyph: "layers",
    tagline: "Every cell needs an anode.", STRAT: 84, CHOKE: 90, GROWTH: 80, CAPEX: 70, MOAT: 66 },
  { n: 4,  cat: "A", name: "Rare-Earth Separation & Metallization", glyph: "prism",
    tagline: "Seventeen elements, one chokepoint.", STRAT: 97, CHOKE: 96, GROWTH: 76, CAPEX: 82, MOAT: 80 },
  { n: 5,  cat: "A", name: "Copper & Aluminum Materials", glyph: "ingot",
    tagline: "You cannot electrify without them.", STRAT: 92, CHOKE: 70, GROWTH: 74, CAPEX: 84, MOAT: 55 },
  { n: 6,  cat: "A", name: "Electrical & Specialty Steels", glyph: "coil",
    tagline: "Grain-oriented steel = every transformer.", STRAT: 86, CHOKE: 82, GROWTH: 68, CAPEX: 78, MOAT: 72 },
  { n: 7,  cat: "A", name: "Polysilicon & High-Purity Silicon", glyph: "droplet",
    tagline: "9-nines pure or bust.", STRAT: 88, CHOKE: 86, GROWTH: 82, CAPEX: 88, MOAT: 78 },
  { n: 8,  cat: "A", name: "Semiconductor & Specialty Electronic Materials", glyph: "wafer",
    tagline: "Photoresists, gases, and quiet monopolies.", STRAT: 90, CHOKE: 92, GROWTH: 80, CAPEX: 74, MOAT: 88 },
  { n: 9,  cat: "A", name: "Uranium Conversion, Enrichment & Nuclear Fuels", glyph: "hex",
    tagline: "Firm power starts here.", STRAT: 91, CHOKE: 89, GROWTH: 70, CAPEX: 90, MOAT: 84 },

  // B — Semiconductors, electrochemical components, electric machines
  { n: 10, cat: "B", name: "Logic, Memory, Analog & Mature-Node Semiconductors", glyph: "cpu",
    tagline: "The most valuable rocks on Earth.", STRAT: 99, CHOKE: 95, GROWTH: 90, CAPEX: 96, MOAT: 97 },
  { n: 11, cat: "B", name: "Power Semiconductors", glyph: "sic",
    tagline: "SiC & GaN: efficiency, weaponized.", STRAT: 93, CHOKE: 84, GROWTH: 94, CAPEX: 82, MOAT: 88 },
  { n: 12, cat: "B", name: "Power Electronics & Conversion Equipment", glyph: "inverter",
    tagline: "Inverters translate the whole stack.", STRAT: 88, CHOKE: 72, GROWTH: 88, CAPEX: 68, MOAT: 76 },
  { n: 13, cat: "B", name: "Sensors, Controls & Industrial Electronics", glyph: "sensor",
    tagline: "The nervous system of the factory.", STRAT: 82, CHOKE: 66, GROWTH: 84, CAPEX: 58, MOAT: 74 },
  { n: 14, cat: "B", name: "Battery Cathode Precursors & Active Materials", glyph: "cathode",
    tagline: "Where cell chemistry is decided.", STRAT: 90, CHOKE: 88, GROWTH: 89, CAPEX: 78, MOAT: 79 },
  { n: 15, cat: "B", name: "Electrolytes, Separators & Current Collectors", glyph: "membrane",
    tagline: "The unglamorous cell internals.", STRAT: 83, CHOKE: 85, GROWTH: 82, CAPEX: 66, MOAT: 77 },
  { n: 16, cat: "B", name: "Battery Cells, Modules, Packs & BMS", glyph: "battery",
    tagline: "The gigafactory trophy pick.", STRAT: 95, CHOKE: 80, GROWTH: 93, CAPEX: 92, MOAT: 82 },
  { n: 17, cat: "B", name: "Stationary & Long-Duration Energy Storage", glyph: "storage",
    tagline: "Firming the intermittent future.", STRAT: 87, CHOKE: 60, GROWTH: 95, CAPEX: 74, MOAT: 70 },
  { n: 18, cat: "B", name: "Permanent Magnets", glyph: "magnet",
    tagline: "No magnet, no motor.", STRAT: 92, CHOKE: 95, GROWTH: 80, CAPEX: 72, MOAT: 86 },
  { n: 19, cat: "B", name: "Electric Motors, Generators & Actuators", glyph: "motor",
    tagline: "Torque is destiny.", STRAT: 85, CHOKE: 68, GROWTH: 83, CAPEX: 70, MOAT: 72 },

  // C — Grid and electricity-system equipment
  { n: 20, cat: "C", name: "Transformers", glyph: "transformer",
    tagline: "36-month lead times. Draft early.", STRAT: 94, CHOKE: 86, GROWTH: 79, CAPEX: 80, MOAT: 74 },
  { n: 21, cat: "C", name: "Switchgear, Breakers & Protection Systems", glyph: "switch",
    tagline: "The grid's circuit-breaker moat.", STRAT: 84, CHOKE: 74, GROWTH: 76, CAPEX: 70, MOAT: 72 },
  { n: 22, cat: "C", name: "Cables, Conductors & HV Transmission", glyph: "cable",
    tagline: "Copper by the continent.", STRAT: 88, CHOKE: 70, GROWTH: 82, CAPEX: 84, MOAT: 68 },
  { n: 23, cat: "C", name: "HVDC & Flexible Transmission Systems", glyph: "hvdc",
    tagline: "Move gigawatts across a continent.", STRAT: 90, CHOKE: 82, GROWTH: 88, CAPEX: 86, MOAT: 85 },
  { n: 24, cat: "C", name: "Digital Grids & Grid-Enhancing Tech", glyph: "digitalgrid",
    tagline: "Squeeze 30% more from the wires you have.", STRAT: 80, CHOKE: 55, GROWTH: 92, CAPEX: 52, MOAT: 70 },
  { n: 25, cat: "C", name: "Microgrids & Resilient Power Systems", glyph: "microgrid",
    tagline: "Islands of power when the grid fails.", STRAT: 78, CHOKE: 52, GROWTH: 90, CAPEX: 60, MOAT: 64 },

  // D — Power-generation technologies
  { n: 26, cat: "D", name: "Solar Photovoltaic Manufacturing", glyph: "solar",
    tagline: "Cheapest electrons in history.", STRAT: 93, CHOKE: 90, GROWTH: 91, CAPEX: 82, MOAT: 66 },
  { n: 27, cat: "D", name: "Onshore Wind Turbine Manufacturing", glyph: "wind",
    tagline: "The mature-tech workhorse.", STRAT: 82, CHOKE: 64, GROWTH: 72, CAPEX: 78, MOAT: 68 },
  { n: 28, cat: "D", name: "Offshore & Floating Wind Systems", glyph: "offshore",
    tagline: "Gigawatts from the deep water.", STRAT: 86, CHOKE: 72, GROWTH: 87, CAPEX: 92, MOAT: 80 },
  { n: 29, cat: "D", name: "Nuclear Reactor Systems & Components", glyph: "reactor",
    tagline: "Firm, dense, and back in fashion.", STRAT: 95, CHOKE: 84, GROWTH: 88, CAPEX: 97, MOAT: 90 },
  { n: 30, cat: "D", name: "Geothermal & Subsurface Energy", glyph: "geo",
    tagline: "Drill for heat, not oil.", STRAT: 79, CHOKE: 58, GROWTH: 89, CAPEX: 80, MOAT: 76 },
  { n: 31, cat: "D", name: "Other Firm & Flexible Power Equipment", glyph: "turbine",
    tagline: "Turbines that fill the gaps.", STRAT: 80, CHOKE: 62, GROWTH: 70, CAPEX: 76, MOAT: 66 },

  // E — Electrified end-use systems
  { n: 32, cat: "E", name: "Electric Vehicles & Electric Drivetrains", glyph: "ev",
    tagline: "The consumer face of the transition.", STRAT: 94, CHOKE: 68, GROWTH: 92, CAPEX: 90, MOAT: 78 },
  { n: 33, cat: "E", name: "Charging & Grid-Interactive Mobility", glyph: "charger",
    tagline: "Every EV needs a plug.", STRAT: 78, CHOKE: 50, GROWTH: 90, CAPEX: 58, MOAT: 60 },
  { n: 34, cat: "E", name: "Heat Pumps, Compressors & Advanced Cooling", glyph: "heatpump",
    tagline: "3x efficiency, quietly enormous.", STRAT: 85, CHOKE: 66, GROWTH: 90, CAPEX: 62, MOAT: 72 },
  { n: 35, cat: "E", name: "Industrial Electrification & Process Heat", glyph: "furnace",
    tagline: "Decarbonize the hard-to-abate.", STRAT: 83, CHOKE: 60, GROWTH: 86, CAPEX: 74, MOAT: 74 },
  { n: 36, cat: "E", name: "Data-Center Power & Cooling Systems", glyph: "datacenter",
    tagline: "The AI-era load monster.", STRAT: 91, CHOKE: 62, GROWTH: 98, CAPEX: 84, MOAT: 76 },

  // F — Molecules, materials, and carbon management
  { n: 37, cat: "F", name: "Electrolysers & Hydrogen Production", glyph: "electrolyser",
    tagline: "Split water, store sunshine.", STRAT: 84, CHOKE: 64, GROWTH: 93, CAPEX: 80, MOAT: 78 },
  { n: 38, cat: "F", name: "Hydrogen Derivatives & Low-Emissions Fuels", glyph: "fuel",
    tagline: "Ammonia, e-methanol, SAF.", STRAT: 80, CHOKE: 58, GROWTH: 88, CAPEX: 78, MOAT: 70 },
  { n: 39, cat: "F", name: "Near-Zero Materials & Carbon Management", glyph: "carbon",
    tagline: "Green steel, cement, and capture.", STRAT: 82, CHOKE: 56, GROWTH: 85, CAPEX: 82, MOAT: 74 },
];

// Derive OVR + cost for each sub-sector.
SUBSECTORS.forEach(s => {
  s.ovr = Math.round((s.STRAT * 0.30) + (s.CHOKE * 0.24) + (s.GROWTH * 0.24) + (s.MOAT * 0.16) + (s.CAPEX * 0.06));
  s.cost = Math.max(6, Math.round((s.ovr - 62) * 3.4));   // salary-cap credits
  s.tier = s.ovr >= 92 ? "S" : s.ovr >= 87 ? "A" : s.ovr >= 82 ? "B" : "C";
});

/* ---------------------------------------------------------------------------
   ALLIES (the 12 drafting teams) + their anime mascots.
   home: per-category synergy bonuses (reflecting real industrial strengths).
   picks[]: specific sub-sector numbers that trigger a "signature" bonus.
   --------------------------------------------------------------------------- */
const ALLIES = [
  { id: "US", name: "United States", flag: "🇺🇸", mascot: "Liberty Volt",
    title: "The Full-Stack Superpower", c1: "#3c5cc4", c2: "#e23b52", hair: "#2b3a8f",
    emblem: "star", eyes: "#2b6fff",
    home: { B: 7, D: 6, E: 6, F: 5, A: 3 }, picks: [10, 29, 36],
    bio: "Deep capital markets, frontier fabs, and an AI-load appetite that reshapes the grid. Wins on breadth." },
  { id: "CA", name: "Canada", flag: "🇨🇦", mascot: "Maple Kaede",
    title: "The Resource Vanguard", c1: "#d52b1e", c2: "#ffffff", hair: "#b81c1c",
    emblem: "leaf", eyes: "#e23b52",
    home: { A: 9, D: 6, F: 5 }, picks: [1, 9, 5],
    bio: "Critical minerals, uranium, and hydro to spare. The upstream powerhouse of the alliance." },
  { id: "MX", name: "Mexico", flag: "🇲🇽", mascot: "Sol Nearshora",
    title: "The Nearshore Engine", c1: "#116149", c2: "#c8102e", hair: "#0e7a4f",
    emblem: "eagle", eyes: "#16a34a",
    home: { E: 8, B: 5, A: 4, C: 3 }, picks: [32, 22, 5],
    bio: "The assembly floor of North America. Auto muscle plus a copper backbone and a friendly border." },
  { id: "UK", name: "United Kingdom", flag: "🇬🇧", mascot: "Britannia Aoi",
    title: "The Offshore Strategist", c1: "#012169", c2: "#c8102e", hair: "#1b2a63",
    emblem: "crown", eyes: "#3b6fe0",
    home: { D: 8, F: 6, B: 4, C: 4 }, picks: [28, 39, 37],
    bio: "Offshore-wind pioneer with world-class finance and carbon-management ambitions." },
  { id: "DE", name: "Germany", flag: "🇩🇪", mascot: "Elektra Falkenberg",
    title: "The Machine Master", c1: "#111111", c2: "#dd0000", hair: "#c9a227",
    emblem: "gear", eyes: "#d4af37",
    home: { B: 8, E: 7, C: 6, F: 4 }, picks: [12, 19, 34],
    bio: "The Mittelstand's precision machines, power electronics, and grid gear. Engineering incarnate." },
  { id: "FR", name: "France", flag: "🇫🇷", mascot: "Marianne Lumière",
    title: "The Atomic Baroness", c1: "#0055a4", c2: "#ef4135", hair: "#1e40af",
    emblem: "fleur", eyes: "#2563eb",
    home: { D: 9, C: 6, F: 5, A: 3 }, picks: [29, 9, 20],
    bio: "Nuclear royalty. A firm-power grid and rare-earth ambitions anchor the continent." },
  { id: "IT", name: "Italy", flag: "🇮🇹", mascot: "Aurora Cavo",
    title: "The Grid Weaver", c1: "#008c45", c2: "#cd212a", hair: "#0a7a3f",
    emblem: "cable", eyes: "#16a34a",
    home: { C: 8, E: 6, B: 4, D: 4 }, picks: [22, 23, 34],
    bio: "Home of the world's great cable-layers. Threads the alliance together, HVDC line by line." },
  { id: "JP", name: "Japan", flag: "🇯🇵", mascot: "Hikari Denki",
    title: "The Materials Sensei", c1: "#bc002d", c2: "#ffffff", hair: "#2b2b3a",
    emblem: "sun", eyes: "#d11a3a",
    home: { B: 8, A: 6, E: 6, F: 5 }, picks: [8, 11, 34],
    bio: "Quiet monopolies in electronic materials, power semis, and precision. Never draft against Japan on quality." },
  { id: "KR", name: "South Korea", flag: "🇰🇷", mascot: "Bateri Haneul",
    title: "The Cell Champion", c1: "#003478", c2: "#c60c30", hair: "#12245c",
    emblem: "trigram", eyes: "#2447a8",
    home: { B: 9, E: 6, D: 4 }, picks: [16, 14, 10],
    bio: "Battery gigafactories and memory fabs. The electrochemistry MVP of the alliance." },
  { id: "AU", name: "Australia", flag: "🇦🇺", mascot: "Sunny Wattle",
    title: "The Lithium Sunlord", c1: "#00247d", c2: "#ffcd00", hair: "#c9a227",
    emblem: "sun2", eyes: "#f5b301",
    home: { A: 9, D: 7, F: 6 }, picks: [1, 26, 37],
    bio: "Lithium, rare earths, and endless sun. The raw-materials and green-hydrogen quarry of the Pacific." },
  { id: "IN", name: "India", flag: "🇮🇳", mascot: "Surya Shakti",
    title: "The Scale Ascendant", c1: "#ff9933", c2: "#138808", hair: "#1a1a1a",
    emblem: "chakra", eyes: "#f97316",
    home: { D: 8, A: 5, E: 5, C: 4 }, picks: [26, 16, 32],
    bio: "Gigascale solar ambition and a billion-person demand base. The alliance's rising growth engine." },
  { id: "TW", name: "Taiwan", flag: "🇹🇼", mascot: "Silica Tienchi",
    title: "The Silicon Shield", c1: "#000095", c2: "#fe0000", hair: "#111827",
    emblem: "shield", eyes: "#1d4ed8",
    home: { B: 10, C: 4, E: 4 }, picks: [10, 11, 8],
    bio: "The most concentrated chokepoint on Earth: advanced logic. Draft Taiwan, own the brains." },
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
