/**
 * Energy Transition Intelligence Hub — Data Layer
 * Provides country metadata, index scores, category breakdowns,
 * technology-level data, and partnership scoring.
 *
 * Data are representative synthetic values aligned with the
 * Opportunity Security Indices methodology (see config/weights.yml).
 * Replace this file with pipeline outputs (index_outputs.rds → CSV export)
 * when live processed data are available.
 */

// ─── Seeded PRNG (LCG) for reproducible synthetic data ────────────────────
function hashCode(str) {
  let h = 5381;
  for (let i = 0; i < str.length; i++) h = ((h << 5) + h) ^ str.charCodeAt(i);
  return Math.abs(h);
}
function seededRng(seed) {
  let s = seed >>> 0;
  return () => { s = (Math.imul(s, 1664525) + 1013904223) >>> 0; return s / 4294967296; };
}
function clamp(v, lo = 0.05, hi = 0.95) { return Math.max(lo, Math.min(hi, v)); }

// ─── Country master data ───────────────────────────────────────────────────
// Fields: name, iso3, isoN (numeric), region, es, eo, psi
// ES = Energy Security, EO = Economic Opportunity, PSI = Partnership Strength
// alignment: 'west' | 'brics' | 'neutral' | 'opec' | 'rival'
// eType: dominant energy profile keys (used for category bias generation)
//   'fossil_x' = fossil exporter, 'fossil_i' = fossil importer,
//   'renew' = renewable-heavy, 'industrial' = industrial/manufacturing powerhouse
//   'mineral_x' = critical mineral exporter, 'developing' = developing
const RAW_COUNTRIES = [
  // North America
  { iso3:'USA', isoN:840, name:'United States',       region:'North America',  es:0.72, eo:0.74, psi:0.78, align:'west',    eTypes:['fossil_x','industrial','renew'] },
  { iso3:'CAN', isoN:124, name:'Canada',              region:'North America',  es:0.69, eo:0.63, psi:0.82, align:'west',    eTypes:['fossil_x','mineral_x','renew'] },
  { iso3:'MEX', isoN:484, name:'Mexico',              region:'North America',  es:0.51, eo:0.49, psi:0.61, align:'neutral', eTypes:['fossil_x','developing'] },
  // South America
  { iso3:'BRA', isoN: 76, name:'Brazil',              region:'South America',  es:0.59, eo:0.53, psi:0.56, align:'brics',  eTypes:['fossil_x','renew','mineral_x'] },
  { iso3:'ARG', isoN: 32, name:'Argentina',           region:'South America',  es:0.54, eo:0.46, psi:0.50, align:'neutral',eTypes:['fossil_x','renew'] },
  { iso3:'CHL', isoN:152, name:'Chile',               region:'South America',  es:0.55, eo:0.58, psi:0.63, align:'west',   eTypes:['mineral_x','renew','fossil_i'] },
  { iso3:'PER', isoN:604, name:'Peru',                region:'South America',  es:0.48, eo:0.52, psi:0.53, align:'neutral',eTypes:['mineral_x','developing'] },
  { iso3:'COL', isoN:170, name:'Colombia',            region:'South America',  es:0.53, eo:0.45, psi:0.52, align:'west',   eTypes:['fossil_x','developing'] },
  { iso3:'BOL', isoN: 68, name:'Bolivia',             region:'South America',  es:0.48, eo:0.44, psi:0.43, align:'neutral',eTypes:['fossil_x','mineral_x','developing'] },
  // Europe
  { iso3:'DEU', isoN:276, name:'Germany',             region:'Europe',         es:0.68, eo:0.76, psi:0.81, align:'west',   eTypes:['industrial','renew','fossil_i'] },
  { iso3:'FRA', isoN:250, name:'France',              region:'Europe',         es:0.70, eo:0.73, psi:0.80, align:'west',   eTypes:['industrial','renew'] },
  { iso3:'GBR', isoN:826, name:'United Kingdom',      region:'Europe',         es:0.66, eo:0.74, psi:0.84, align:'west',   eTypes:['industrial','renew','fossil_x'] },
  { iso3:'ITA', isoN:380, name:'Italy',               region:'Europe',         es:0.58, eo:0.66, psi:0.72, align:'west',   eTypes:['industrial','fossil_i'] },
  { iso3:'ESP', isoN:724, name:'Spain',               region:'Europe',         es:0.60, eo:0.64, psi:0.73, align:'west',   eTypes:['renew','industrial','fossil_i'] },
  { iso3:'NOR', isoN:578, name:'Norway',              region:'Europe',         es:0.83, eo:0.68, psi:0.85, align:'west',   eTypes:['fossil_x','renew'] },
  { iso3:'SWE', isoN:752, name:'Sweden',              region:'Europe',         es:0.74, eo:0.72, psi:0.84, align:'west',   eTypes:['renew','mineral_x','industrial'] },
  { iso3:'NLD', isoN:528, name:'Netherlands',         region:'Europe',         es:0.63, eo:0.70, psi:0.80, align:'west',   eTypes:['industrial','fossil_i'] },
  { iso3:'POL', isoN:616, name:'Poland',              region:'Europe',         es:0.58, eo:0.61, psi:0.71, align:'west',   eTypes:['fossil_i','industrial'] },
  { iso3:'AUT', isoN: 40, name:'Austria',             region:'Europe',         es:0.65, eo:0.67, psi:0.77, align:'west',   eTypes:['renew','industrial'] },
  { iso3:'DNK', isoN:208, name:'Denmark',             region:'Europe',         es:0.67, eo:0.69, psi:0.83, align:'west',   eTypes:['renew','industrial'] },
  { iso3:'FIN', isoN:246, name:'Finland',             region:'Europe',         es:0.65, eo:0.68, psi:0.82, align:'west',   eTypes:['renew','industrial','mineral_x'] },
  { iso3:'CHE', isoN:756, name:'Switzerland',         region:'Europe',         es:0.64, eo:0.77, psi:0.81, align:'west',   eTypes:['renew','industrial'] },
  { iso3:'PRT', isoN:620, name:'Portugal',            region:'Europe',         es:0.57, eo:0.59, psi:0.73, align:'west',   eTypes:['renew','fossil_i'] },
  { iso3:'UKR', isoN:804, name:'Ukraine',             region:'Europe',         es:0.46, eo:0.46, psi:0.55, align:'west',   eTypes:['fossil_i','mineral_x','developing'] },
  { iso3:'TUR', isoN:792, name:'Turkey',              region:'Europe/Asia',    es:0.51, eo:0.57, psi:0.52, align:'neutral',eTypes:['fossil_i','renew','industrial'] },
  // Africa
  { iso3:'ZAF', isoN:710, name:'South Africa',        region:'Africa',         es:0.44, eo:0.49, psi:0.56, align:'brics',  eTypes:['mineral_x','fossil_x','developing'] },
  { iso3:'NGA', isoN:566, name:'Nigeria',             region:'Africa',         es:0.43, eo:0.38, psi:0.39, align:'neutral',eTypes:['fossil_x','developing'] },
  { iso3:'EGY', isoN:818, name:'Egypt',               region:'Africa',         es:0.47, eo:0.43, psi:0.46, align:'neutral',eTypes:['fossil_x','renew','developing'] },
  { iso3:'MAR', isoN:504, name:'Morocco',             region:'Africa',         es:0.42, eo:0.48, psi:0.55, align:'west',   eTypes:['renew','developing','fossil_i'] },
  { iso3:'KEN', isoN:404, name:'Kenya',               region:'Africa',         es:0.37, eo:0.41, psi:0.49, align:'west',   eTypes:['renew','developing'] },
  { iso3:'ETH', isoN:231, name:'Ethiopia',            region:'Africa',         es:0.32, eo:0.36, psi:0.40, align:'neutral',eTypes:['renew','developing'] },
  { iso3:'TZA', isoN:834, name:'Tanzania',            region:'Africa',         es:0.34, eo:0.37, psi:0.41, align:'neutral',eTypes:['mineral_x','developing'] },
  { iso3:'GHA', isoN:288, name:'Ghana',               region:'Africa',         es:0.38, eo:0.39, psi:0.45, align:'west',   eTypes:['fossil_x','developing'] },
  { iso3:'ZMB', isoN:894, name:'Zambia',              region:'Africa',         es:0.40, eo:0.45, psi:0.47, align:'neutral',eTypes:['mineral_x','developing'] },
  { iso3:'COD', isoN:180, name:'DR Congo',            region:'Africa',         es:0.28, eo:0.40, psi:0.34, align:'neutral',eTypes:['mineral_x','developing'] },
  { iso3:'AGO', isoN: 24, name:'Angola',              region:'Africa',         es:0.48, eo:0.36, psi:0.37, align:'neutral',eTypes:['fossil_x','developing'] },
  { iso3:'DZA', isoN: 12, name:'Algeria',             region:'Africa',         es:0.56, eo:0.38, psi:0.39, align:'neutral',eTypes:['fossil_x','developing'] },
  { iso3:'MOZ', isoN:508, name:'Mozambique',          region:'Africa',         es:0.29, eo:0.38, psi:0.40, align:'neutral',eTypes:['mineral_x','fossil_x','developing'] },
  // Middle East
  { iso3:'SAU', isoN:682, name:'Saudi Arabia',        region:'Middle East',    es:0.82, eo:0.48, psi:0.51, align:'opec',   eTypes:['fossil_x'] },
  { iso3:'ARE', isoN:784, name:'United Arab Emirates',region:'Middle East',    es:0.75, eo:0.56, psi:0.59, align:'opec',   eTypes:['fossil_x','industrial'] },
  { iso3:'IRN', isoN:364, name:'Iran',                region:'Middle East',    es:0.78, eo:0.37, psi:0.18, align:'rival',  eTypes:['fossil_x'] },
  { iso3:'IRQ', isoN:368, name:'Iraq',                region:'Middle East',    es:0.70, eo:0.33, psi:0.29, align:'neutral',eTypes:['fossil_x','developing'] },
  { iso3:'KWT', isoN:414, name:'Kuwait',              region:'Middle East',    es:0.73, eo:0.42, psi:0.46, align:'opec',   eTypes:['fossil_x'] },
  { iso3:'QAT', isoN:634, name:'Qatar',               region:'Middle East',    es:0.76, eo:0.45, psi:0.48, align:'opec',   eTypes:['fossil_x'] },
  // Asia-Pacific
  { iso3:'CHN', isoN:156, name:'China',               region:'Asia-Pacific',   es:0.53, eo:0.83, psi:0.28, align:'rival',  eTypes:['industrial','fossil_x','mineral_x','renew'] },
  { iso3:'JPN', isoN:392, name:'Japan',               region:'Asia-Pacific',   es:0.61, eo:0.72, psi:0.79, align:'west',   eTypes:['industrial','fossil_i','renew'] },
  { iso3:'KOR', isoN:410, name:'South Korea',         region:'Asia-Pacific',   es:0.59, eo:0.75, psi:0.76, align:'west',   eTypes:['industrial','fossil_i','renew'] },
  { iso3:'IND', isoN:356, name:'India',               region:'Asia-Pacific',   es:0.50, eo:0.63, psi:0.56, align:'brics',  eTypes:['fossil_i','renew','industrial','developing'] },
  { iso3:'IDN', isoN:360, name:'Indonesia',           region:'Asia-Pacific',   es:0.57, eo:0.52, psi:0.53, align:'neutral',eTypes:['fossil_x','mineral_x','developing'] },
  { iso3:'AUS', isoN: 36, name:'Australia',           region:'Asia-Pacific',   es:0.75, eo:0.67, psi:0.84, align:'west',   eTypes:['fossil_x','mineral_x','renew'] },
  { iso3:'MYS', isoN:458, name:'Malaysia',            region:'Asia-Pacific',   es:0.58, eo:0.59, psi:0.62, align:'neutral',eTypes:['fossil_x','industrial'] },
  { iso3:'VNM', isoN:704, name:'Vietnam',             region:'Asia-Pacific',   es:0.46, eo:0.59, psi:0.59, align:'neutral',eTypes:['industrial','renew','developing'] },
  { iso3:'THA', isoN:764, name:'Thailand',            region:'Asia-Pacific',   es:0.47, eo:0.58, psi:0.61, align:'neutral',eTypes:['industrial','fossil_i'] },
  { iso3:'PHL', isoN:608, name:'Philippines',         region:'Asia-Pacific',   es:0.40, eo:0.49, psi:0.59, align:'west',   eTypes:['renew','developing','fossil_i'] },
  { iso3:'SGP', isoN:702, name:'Singapore',           region:'Asia-Pacific',   es:0.41, eo:0.77, psi:0.76, align:'west',   eTypes:['industrial','fossil_i'] },
  { iso3:'NZL', isoN:554, name:'New Zealand',         region:'Asia-Pacific',   es:0.62, eo:0.61, psi:0.83, align:'west',   eTypes:['renew','fossil_i'] },
  { iso3:'BGD', isoN: 50, name:'Bangladesh',          region:'Asia-Pacific',   es:0.33, eo:0.42, psi:0.46, align:'neutral',eTypes:['fossil_i','developing'] },
  { iso3:'PAK', isoN:586, name:'Pakistan',            region:'Asia-Pacific',   es:0.35, eo:0.39, psi:0.39, align:'neutral',eTypes:['fossil_i','developing'] },
  { iso3:'TWN', isoN:158, name:'Taiwan',              region:'Asia-Pacific',   es:0.51, eo:0.78, psi:0.73, align:'west',   eTypes:['industrial','fossil_i'] },
  // Central/Eastern Europe / Former Soviet
  { iso3:'RUS', isoN:643, name:'Russia',              region:'Europe/Asia',    es:0.86, eo:0.42, psi:0.13, align:'rival',  eTypes:['fossil_x','mineral_x'] },
  { iso3:'KAZ', isoN:398, name:'Kazakhstan',          region:'Central Asia',   es:0.74, eo:0.44, psi:0.43, align:'neutral',eTypes:['fossil_x','mineral_x'] },
  { iso3:'MNG', isoN:496, name:'Mongolia',            region:'Central Asia',   es:0.59, eo:0.44, psi:0.49, align:'neutral',eTypes:['fossil_x','mineral_x','developing'] },
  { iso3:'UZB', isoN:860, name:'Uzbekistan',          region:'Central Asia',   es:0.52, eo:0.41, psi:0.42, align:'neutral',eTypes:['fossil_x','developing'] },
  // Other
  { iso3:'ISR', isoN:376, name:'Israel',              region:'Middle East',    es:0.56, eo:0.72, psi:0.69, align:'west',   eTypes:['renew','industrial','fossil_i'] },
];

// ─── Index category definitions ────────────────────────────────────────────
const ES_CATEGORIES = [
  { key:'foreign_dependency', label:'Foreign Dependency', weight:6, description:'Exposure to foreign control of critical energy supply chain inputs.' },
  { key:'energy_imports',     label:'Energy Imports',     weight:4, description:'Production-consumption balance across key fuel types.' },
  { key:'reserves',           label:'Reserves',           weight:4, description:'Domestic reserves depth across fossil fuels and critical minerals.' },
  { key:'trade_risk',         label:'Trade Risk',         weight:4, description:'Trade concentration and structural import dependence (HHI-based).' },
  { key:'minerals_trade',     label:'Minerals Trade',     weight:4, description:'Critical minerals export positioning and trade security.' },
  { key:'production',         label:'Production',         weight:6, description:'Depth and momentum of domestic energy and mineral production.' },
  { key:'energy_access',      label:'Energy Access',      weight:4, description:'Per-capita energy consumption enabling conditions.' },
  { key:'consumption',        label:'Consumption',        weight:5, description:'Installed capacity base and electricity growth momentum.' },
  { key:'energy_prices',      label:'Energy Prices',      weight:2, description:'Commodity input price stability (lower volatility = higher score).' },
];

const EO_CATEGORIES = [
  { key:'trade',                  label:'Trade',                  weight:3, description:'Export competitiveness, revealed comparative advantage and feasibility.' },
  { key:'production',             label:'Production',             weight:3, description:'Domestic productive capability and manufacturing momentum.' },
  { key:'technology_demand',      label:'Technology Demand',      weight:4, description:'Forward demand growth and addressable market size for clean tech.' },
  { key:'tech_readiness',         label:'Tech Readiness',         weight:2, description:'Technology maturity (TRL) — bell-curve: optimal at mid-TRL.' },
  { key:'energy_prices',          label:'Energy Prices',          weight:3, description:'Price stability as a bankability signal for investors.' },
  { key:'investment',             label:'Investment',             weight:3, description:'Market investment flows and clean energy finance momentum.' },
  { key:'energy_access',          label:'Energy Access',          weight:3, description:'Market size and deployment runway for clean energy.' },
  { key:'foreign_dependency',     label:'Foreign Dependency',     weight:3, description:'Market positioning advantage from reduced import dependency.' },
  { key:'cost_competitiveness',   label:'Cost Competitiveness',   weight:3, description:'LCOE, labour and capital cost positioning (manufacturing/deployment).' },
  { key:'consumption',            label:'Consumption',            weight:3, description:'Domestic market size and growth for clean energy products.' },
];

// Category bias by energy profile type (offsets from overall score)
const CAT_BIAS = {
  // ES biases
  es_foreign_dependency:  { fossil_x:+0.15, fossil_i:-0.18, renew:+0.05, industrial: 0, mineral_x:+0.10, developing:-0.10, rival:+0.05 },
  es_energy_imports:      { fossil_x:+0.22, fossil_i:-0.22, renew:+0.05, industrial:+0.02, mineral_x:+0.05, developing:-0.08, opec:+0.25 },
  es_reserves:            { fossil_x:+0.20, fossil_i:-0.15, renew:+0.03, industrial:-0.05, mineral_x:+0.15, developing:-0.10, opec:+0.22 },
  es_trade_risk:          { fossil_x:+0.12, fossil_i:-0.12, renew:+0.05, industrial:+0.08, mineral_x:+0.08, developing:-0.12, rival: 0 },
  es_minerals_trade:      { fossil_x:+0.05, fossil_i:-0.05, renew:+0.02, industrial:+0.05, mineral_x:+0.22, developing:-0.08, opec: 0 },
  es_production:          { fossil_x:+0.18, fossil_i:-0.12, renew:+0.08, industrial:+0.12, mineral_x:+0.10, developing:-0.15, rival:+0.05 },
  es_energy_access:       { fossil_x:+0.05, fossil_i:+0.02, renew:+0.03, industrial:+0.12, mineral_x: 0, developing:-0.20, rival: 0 },
  es_consumption:         { fossil_x:+0.08, fossil_i:+0.05, renew:+0.05, industrial:+0.15, mineral_x: 0, developing:-0.15, rival:+0.10 },
  es_energy_prices:       { fossil_x:+0.15, fossil_i:-0.10, renew:+0.10, industrial:+0.03, mineral_x:+0.05, developing:-0.08, opec:+0.18 },
  // EO biases
  eo_trade:               { fossil_x:-0.05, fossil_i:-0.05, renew:+0.05, industrial:+0.18, mineral_x:+0.12, developing:-0.12, rival:+0.15 },
  eo_production:          { fossil_x:-0.05, fossil_i:-0.05, renew:+0.08, industrial:+0.20, mineral_x:+0.08, developing:-0.15, rival:+0.18 },
  eo_technology_demand:   { fossil_x:-0.08, fossil_i:+0.05, renew:+0.12, industrial:+0.12, mineral_x:+0.05, developing:+0.15, rival:+0.08 },
  eo_tech_readiness:      { fossil_x:-0.08, fossil_i:+0.02, renew:+0.05, industrial:+0.18, mineral_x: 0, developing:-0.12, rival:+0.10 },
  eo_energy_prices:       { fossil_x:+0.15, fossil_i:-0.08, renew:+0.10, industrial:+0.05, mineral_x:+0.05, developing:-0.05, opec:+0.18 },
  eo_investment:          { fossil_x:-0.05, fossil_i:+0.02, renew:+0.10, industrial:+0.15, mineral_x:+0.08, developing:-0.10, rival:+0.05 },
  eo_energy_access:       { fossil_x:+0.05, fossil_i:+0.05, renew:+0.05, industrial:+0.12, mineral_x: 0, developing:+0.10, rival:+0.08 },
  eo_foreign_dependency:  { fossil_x:+0.10, fossil_i:-0.08, renew:+0.05, industrial:+0.10, mineral_x:+0.12, developing:-0.08, rival:+0.05 },
  eo_cost_competitiveness:{ fossil_x:+0.08, fossil_i:-0.02, renew:+0.10, industrial:+0.12, mineral_x:+0.05, developing:+0.10, rival:+0.20 },
  eo_consumption:         { fossil_x:+0.05, fossil_i:+0.05, renew:+0.05, industrial:+0.15, mineral_x:+0.02, developing:+0.08, rival:+0.15 },
};

function biasFromTypes(biasMap, eTypes) {
  let total = 0;
  const seen = new Set(eTypes);
  for (const et of seen) { total += (biasMap[et] || 0); }
  return total;
}

function genCategoryScore(iso3, catKey, baseScore, eTypes, biasMapKey) {
  const rng = seededRng(hashCode(iso3 + catKey));
  const noise = (rng() - 0.5) * 0.18;
  const bias = biasFromTypes(CAT_BIAS[biasMapKey] || {}, eTypes);
  return clamp(baseScore + bias + noise);
}

// ─── Technologies and supply chains ────────────────────────────────────────
const TECHNOLOGIES = [
  { key:'Solar',           label:'Solar PV' },
  { key:'Wind',            label:'Wind' },
  { key:'Electric Vehicles', label:'Electric Vehicles' },
  { key:'Batteries',       label:'Batteries' },
  { key:'Green Hydrogen',  label:'Green Hydrogen' },
];

const SUPPLY_CHAINS = [
  { key:'Upstream',   label:'Upstream',   desc:'Raw materials, mining, resource extraction' },
  { key:'Midstream',  label:'Midstream',  desc:'Manufacturing, processing, components' },
  { key:'Downstream', label:'Downstream', desc:'Deployment, installation, end-use' },
];

// Tech-specific bias on ES/EO (country × tech interaction)
const TECH_ES_BIAS = {
  'Solar':           { renew:+0.12, fossil_x:-0.05, mineral_x:+0.08, developing:-0.10, industrial:+0.05, rival:+0.08 },
  'Wind':            { renew:+0.15, fossil_x:-0.05, mineral_x:+0.06, developing:-0.08, industrial:+0.05, rival:+0.06 },
  'Electric Vehicles':{ industrial:+0.18, mineral_x:+0.12, fossil_i:+0.08, rival:+0.15, developing:-0.12 },
  'Batteries':       { industrial:+0.15, mineral_x:+0.18, fossil_i:+0.05, rival:+0.18, developing:-0.10 },
  'Green Hydrogen':  { renew:+0.15, fossil_x:+0.08, industrial:+0.08, fossil_i:-0.08, developing:-0.12 },
};
const TECH_EO_BIAS = {
  'Solar':           { renew:+0.15, industrial:+0.10, developing:+0.12, rival:+0.12, fossil_x:-0.08 },
  'Wind':            { renew:+0.12, industrial:+0.10, developing:+0.08, rival:+0.10, fossil_x:-0.05 },
  'Electric Vehicles':{ industrial:+0.20, rival:+0.22, mineral_x:+0.10, developing:-0.10, fossil_x:-0.12 },
  'Batteries':       { industrial:+0.18, rival:+0.20, mineral_x:+0.12, developing:-0.08, fossil_x:-0.10 },
  'Green Hydrogen':  { renew:+0.18, industrial:+0.12, fossil_x:+0.10, developing:-0.05 },
};
const SC_ES_BIAS = { Upstream:+0.05, Midstream: 0, Downstream:-0.05 };
const SC_EO_BIAS = { Upstream:-0.05, Midstream:+0.08, Downstream: 0 };

// ─── Build full country objects ────────────────────────────────────────────
function buildCountries() {
  return RAW_COUNTRIES.map(c => {
    const { iso3, eTypes, es, eo } = c;

    // ES categories
    const esCategories = {};
    for (const cat of ES_CATEGORIES) {
      esCategories[cat.key] = genCategoryScore(iso3, cat.key, es, eTypes, `es_${cat.key}`);
    }

    // EO categories
    const eoCategories = {};
    for (const cat of EO_CATEGORIES) {
      eoCategories[cat.key] = genCategoryScore(iso3, cat.key, eo, eTypes, `eo_${cat.key}`);
    }

    // Technology × Supply Chain ES/EO
    const techBreakdown = {};
    for (const tech of TECHNOLOGIES) {
      techBreakdown[tech.key] = {};
      for (const sc of SUPPLY_CHAINS) {
        const rngEs = seededRng(hashCode(iso3 + tech.key + sc.key + 'es'));
        const rngEo = seededRng(hashCode(iso3 + tech.key + sc.key + 'eo'));
        const esBias = biasFromTypes(TECH_ES_BIAS[tech.key] || {}, eTypes) + SC_ES_BIAS[sc.key];
        const eoBias = biasFromTypes(TECH_EO_BIAS[tech.key] || {}, eTypes) + SC_EO_BIAS[sc.key];
        techBreakdown[tech.key][sc.key] = {
          es: clamp(es + esBias + (rngEs() - 0.5) * 0.20),
          eo: clamp(eo + eoBias + (rngEo() - 0.5) * 0.20),
        };
      }
    }

    return { ...c, esCategories, eoCategories, techBreakdown };
  });
}

const COUNTRIES = buildCountries();
const COUNTRY_MAP = Object.fromEntries(COUNTRIES.map(c => [c.iso3, c]));
const ISONUM_MAP  = Object.fromEntries(COUNTRIES.map(c => [c.isoN, c]));

// ─── Partnership scoring ───────────────────────────────────────────────────
// Alignment compatibility matrix (higher = better friendshore)
const ALIGN_COMPAT = {
  west:    { west:0.90, brics:0.50, neutral:0.60, opec:0.50, rival:0.15 },
  brics:   { west:0.50, brics:0.70, neutral:0.60, opec:0.55, rival:0.55 },
  neutral: { west:0.60, brics:0.60, neutral:0.65, opec:0.55, rival:0.40 },
  opec:    { west:0.50, brics:0.55, neutral:0.55, opec:0.75, rival:0.35 },
  rival:   { west:0.15, brics:0.55, neutral:0.40, opec:0.35, rival:0.70 },
};

/**
 * Compute partnership scores from home → partner.
 * Returns { friendshore, export_partner, development } each in [0,1].
 */
function computePartnership(home, partner) {
  if (home.iso3 === partner.iso3) return null;

  const rng = seededRng(hashCode(home.iso3 + partner.iso3 + 'psi'));
  const noise = (rng() - 0.5) * 0.08;

  // ── Friendshore (0.4 weight in PSI) ─────────────────────────────────────
  // Rewards alignment, low-risk partner, complementary EO
  const alignScore = ALIGN_COMPAT[home.align]?.[partner.align] ?? 0.5;
  const esNeed = 1 - home.es;       // home's security need
  const eoPart = partner.eo;        // partner's economic capability
  const outbound = partner.psi;     // partner's general partnership quality
  // Weights per weights.yml: imp_trade:2, es_need:3, eo_partner:3, outbound_index:2
  const friendshore = clamp(
    (alignScore * 2 + esNeed * 3 + eoPart * 3 + outbound * 2) / 10 + noise
  );

  // ── Export Partnership ───────────────────────────────────────────────────
  // Home exports to partner: partner has demand (high tech_demand, lower eo)
  // Rewards: partner high consumption/demand, home has EO advantage
  const partnerDemand = clamp((partner.eoCategories.technology_demand + partner.eoCategories.consumption) / 2);
  const homeEoAdv = home.eo;
  const tradeCompat = alignScore;
  const exportPartner = clamp(
    (partnerDemand * 3 + homeEoAdv * 2 + tradeCompat * 2 + partner.eoCategories.energy_access * 2) / 9 + noise
  );

  // ── Development Partnership ──────────────────────────────────────────────
  // Home assists partner: partner has high need but low capacity
  const partnerNeed = clamp((1 - partner.eo + 1 - partner.es) / 2);
  const partnerPotential = clamp((partner.esCategories.reserves + partner.esCategories.minerals_trade) / 2);
  const homeCapacity = home.eo;
  const development = clamp(
    (partnerNeed * 3 + partnerPotential * 3 + homeCapacity * 2 + alignScore * 2) / 10 + noise
  );

  return {
    friendshore:   +friendshore.toFixed(3),
    export_partner:+exportPartner.toFixed(3),
    development:   +development.toFixed(3),
    composite:     +clamp((friendshore * 0.4 + exportPartner * 0.4 + development * 0.2) + noise / 2).toFixed(3),
  };
}

/** Get ranked partners for a given home country and partnership type. */
function getPartners(homeIso3, type = 'friendshore') {
  const home = COUNTRY_MAP[homeIso3];
  if (!home) return [];
  return COUNTRIES
    .filter(p => p.iso3 !== homeIso3)
    .map(partner => ({
      ...partner,
      scores: computePartnership(home, partner),
    }))
    .filter(p => p.scores)
    .sort((a, b) => (b.scores[type] || 0) - (a.scores[type] || 0));
}

// ─── Utility: describe score quality ──────────────────────────────────────
function scoreLabel(v) {
  if (v >= 0.75) return { text:'Strong',   cls:'score-strong' };
  if (v >= 0.55) return { text:'Moderate', cls:'score-moderate' };
  if (v >= 0.35) return { text:'Weak',     cls:'score-weak' };
  return              { text:'Very Weak', cls:'score-vweak' };
}

function fmtScore(v) { return (v * 100).toFixed(1); }
function fmtPct(v)   { return (v * 100).toFixed(0) + '%'; }

// ─── Region list (for filter) ──────────────────────────────────────────────
const REGIONS = [...new Set(COUNTRIES.map(c => c.region))].sort();

// ─── Methodology descriptions ──────────────────────────────────────────────
const METHODOLOGY = {
  es: `The Energy Security Index (ESI) measures a country's resilience across nine
    categories weighted by their contribution to supply-chain security. Scores are
    normalised using a median-centred S-curve (γ = 0.5) so that outliers are preserved
    at the tails. A higher score indicates greater domestic energy security.`,
  eo: `The Economic Opportunity Index (EOI) measures a country's ability to capture
    economic value from the clean energy transition — through export competitiveness,
    manufacturing capacity, market demand, and cost positioning. Higher scores indicate
    stronger competitive positioning.`,
  psi: `The Partnership Strength Index (PSI) assesses bilateral partnership quality
    through three lenses: Friendshore alignment (40%), Economic Opportunity fit (40%)
    and Development absorptive capacity (20%). It is directional — scores reflect the
    partnership from the perspective of the home country.`,
  weights_es: `ES category weights (from config/weights.yml):
    Foreign Dependency×6, Production×6, Consumption×5, Energy Imports×4,
    Reserves×4, Trade Risk×4, Minerals Trade×4, Energy Access×4, Energy Prices×2`,
  weights_eo: `EO category weights: Technology Demand×4, Trade×3, Production×3,
    Energy Prices×3, Investment×3, Energy Access×3, Foreign Dependency×3,
    Cost Competitiveness×3, Consumption×3, Tech Readiness×2`,
};

// Expose globally
window.OSI_DATA = {
  COUNTRIES, COUNTRY_MAP, ISONUM_MAP,
  ES_CATEGORIES, EO_CATEGORIES,
  TECHNOLOGIES, SUPPLY_CHAINS,
  REGIONS, METHODOLOGY,
  getPartners, computePartnership, scoreLabel, fmtScore, fmtPct,
};
