/* ============================================================================
   LEXICON — keyword maps used by the news bot to tag a headline with
   countries (ally ids), sub-sectors (numbers), event type, sentiment and pillar.
   Deliberately conservative: better to skip an ambiguous headline than mis-score.
   ============================================================================ */

/* ally id -> matching phrases (lowercased). Word-boundary matched by the bot. */
export const COUNTRY = {
  US: ["united states", "u.s.", "u.s", "usa", "us", "america", "american", "washington", "white house"],
  CA: ["canada", "canadian", "ottawa"],
  MX: ["mexico", "mexican"],
  UK: ["united kingdom", "britain", "british", "u.k.", "uk", "england", "london", "scotland"],
  DE: ["germany", "german", "berlin"],
  FR: ["france", "french", "paris"],
  IT: ["italy", "italian", "rome"],
  JP: ["japan", "japanese", "tokyo"],
  KR: ["south korea", "korean", "seoul"],
  AU: ["australia", "australian", "canberra"],
  IN: ["india", "indian", "new delhi"],
  TW: ["taiwan", "taiwanese", "taipei"],
  NL: ["netherlands", "dutch", "amsterdam", "the hague"],
  ES: ["spain", "spanish", "madrid"],
  SE: ["sweden", "swedish", "stockholm"],
  FI: ["finland", "finnish", "helsinki"],
  PL: ["poland", "polish", "warsaw"],
  NO: ["norway", "norwegian", "oslo"],
  NZ: ["new zealand", "wellington"],
  SG: ["singapore", "singaporean"],
  TR: ["turkey", "türkiye", "turkiye", "turkish", "ankara", "istanbul"],
  CN: ["china", "chinese", "beijing"],   // adversary — tagged for sector attribution, not a league seat
};

/* sub-sector number -> matching phrases */
export const SECTOR = {
  1: ["critical mineral", "critical minerals", "lithium mine", "cobalt", "nickel mine", "mineral extraction", "mining project"],
  2: ["battery-mineral refining", "lithium refining", "battery materials", "chemical conversion", "lithium hydroxide", "refining plant"],
  3: ["graphite", "anode material"],
  4: ["rare earth", "rare-earth", "rare earths", "neodymium", "dysprosium", "magnet metals"],
  5: ["copper", "aluminum", "aluminium"],
  6: ["electrical steel", "specialty steel", "grain-oriented steel"],
  7: ["polysilicon", "high-purity silicon"],
  8: ["photoresist", "semiconductor materials", "specialty gases", "electronic materials"],
  9: ["uranium", "enrichment", "nuclear fuel"],
  10: ["semiconductor", "chip ", "chips", "logic chip", "memory chip", "foundry", "wafer fab", "chipmaker", "tsmc", "nanometer"],
  11: ["power semiconductor", "silicon carbide", "sic chip", "gallium nitride", " gan "],
  12: ["power electronics", "inverter", "power converter"],
  13: ["sensors", "industrial electronics", "control systems"],
  14: ["cathode", "precursor", "active material", "cam plant"],
  15: ["electrolyte", "separator", "current collector"],
  16: ["battery cell", "gigafactory", "battery plant", "ev battery", "cell manufacturing", "battery pack", "battery factory"],
  17: ["energy storage", "grid storage", "long-duration storage", "battery storage", "bess"],
  18: ["permanent magnet", "magnet", "magnet maker", "magnet plant"],
  19: ["electric motor", "traction motor", "generator", "actuator"],
  20: ["transformer"],
  21: ["switchgear", "circuit breaker", "protection system"],
  22: ["power cable", "subsea cable", "transmission line", "conductor", "cabling"],
  23: ["hvdc", "high-voltage direct current", "interconnector"],
  24: ["smart grid", "grid-enhancing", "digital grid", "grid technology"],
  25: ["microgrid", "resilient power"],
  26: ["solar", "photovoltaic", " pv ", "solar panel", "solar module", "solar cell"],
  27: ["onshore wind", "wind turbine", "wind farm"],
  28: ["offshore wind", "floating wind"],
  29: ["nuclear reactor", "small modular reactor", " smr ", "nuclear plant", "nuclear power"],
  30: ["geothermal"],
  31: ["gas turbine", "firm power", "flexible power"],
  32: ["electric vehicle", " ev ", "evs", "electric car", "drivetrain", "electric truck"],
  33: ["ev charging", "charging network", "charger", "charging station"],
  34: ["heat pump", "advanced cooling", "compressor"],
  35: ["industrial electrification", "process heat", "electrify industry"],
  36: ["data center", "data centre", "datacenter", "ai data"],
  37: ["electrolyser", "electrolyzer", "green hydrogen", "hydrogen production"],
  38: ["ammonia", "e-fuel", "e-methanol", "sustainable aviation fuel", " saf ", "low-emissions fuel"],
  39: ["green steel", "carbon capture", " ccs ", "low-carbon cement", "direct air capture"],
};

export const TYPE = {
  partnership: ["partnership", "agreement", "deal", "pact", "alliance", "mou", "memorandum", "cooperation", "joint venture", "treaty", "accord", "sign", "signed", "bilateral"],
  policy: ["policy", "subsidy", "subsidies", "tariff", "tariffs", "export control", "sanction", "sanctions", "ban", "regulation", "act", "bill", "mandate", "incentive", "tax credit", "grant", "permit", "law", "ruling"],
  investment: ["invest", "investment", "funding", "raise", "build a", "new plant", "new factory", "expansion", "stake", "acquisition", "acquire", "billion", "million"],
  milestone: ["launch", "first", "breakthrough", "record", "milestone", "begins production", "opens", "online", "unveil"],
};

export const POS = ["boost", "boosts", "surge", "expand", "expands", "grow", "growth", "approve", "approves", "approved", "breakthrough", "record", "invest", "invests", "sign", "signs", "deal", "partnership", "support", "accelerate", "wins", "secure", "secures", "ramp", "launch", "launches", "agreement", "funding", "subsidy", "incentive", "greenlight", "backs", "unlocks"];
export const NEG = ["ban", "bans", "sanction", "sanctions", "tariff", "tariffs", "restrict", "restricts", "cut", "cuts", "delay", "delays", "cancel", "cancels", "halt", "halts", "shortage", "disruption", "block", "blocks", "curb", "curbs", "export control", "layoff", "layoffs", "bankruptcy", "decline", "threat", "risk", "fails", "failure", "dispute", "tension", "probe", "warns", "scrap", "scraps", "loses"];

export const PILLAR = {
  NAT:  ["security", "defense", "defence", "military", "sanction", "export control", "chokepoint", "sovereignty", "espionage", "critical", "strategic"],
  ENSC: ["supply chain", "energy security", "competitiveness", "jobs", "manufacturing", "reshoring", "onshoring", "tariff", "trade", "domestic"],
  CLIM: ["climate", "emissions", "decarbon", "net zero", "net-zero", "carbon", "clean energy", "renewable", "green"],
  OPP:  ["investment", "market", "revenue", "billion", "expansion", "factory", "growth", "exports"],
};

export const BIGNUM = ["billion", "record", "largest", "biggest", "major", "gigafactory", "massive"];
