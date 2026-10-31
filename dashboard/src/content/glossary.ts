// The shared glossary (spec_country.md C3.2; the content there is normative): a plain-language
// definition of every index, category and acronym, for a reader who knows energy and economics but has
// not read the paper. Since the NEIS copy refresh (2026-10-30) each `short` says first what a term means for a
// decision, in the shared vocabulary's words (content/vocabulary.ts: the six choices, the yards, the Allied
// Industrial Base), and `more` keeps the technical definition; the Allied Industrial Base terms are a module of
// their own ('aib'). Since 2026-10-28 every entry reads the six supply-chain stages (Extraction,
// Refining, Components, Assembly, Deployment, Recovery; lib/stages.ts), and the strategic position
// framework's terms (the posture cards' Term ids, posture/basis.tsx POSTURE_TERMS) are here too. Since
// 2026-10-29 the framework is canonical (Need -> Prize -> Position -> Choice -> Policy alignment); the
// indices (ES, EO, SI), the corners and the dials are the Legacy OSI framework (the paper baseline). Used by InfoTip (the `short` text), Term (the name on first use), GlossaryList
// and the /glossary page (`more`), in all three modules. Pure data and helpers.
//
// Numbers that change with the data are written as {{placeholders}} and filled from GLOSSARY_DEFAULTS,
// or from the values a module's manifest provides (GlossaryVarsProvider in components/glossaryVars.tsx).

import { AIB_CLASS_DEFINITION, AIB_EXPLAINER, AIB_MEASURE_DEFINITION, CHOICE_DEFINITION, COMPACT, MANAGED_INTERDEPENDENCE, YARD_DEFINITION } from './vocabulary.ts';

/** Where a term is used: the Sector, Country and Allied Industrial Base pages, and 'strategic' (the Methods page and the Legacy OSI views). */
export type GlossaryModule = 'sector' | 'strategic' | 'country' | 'aib';

export interface GlossaryEntry {
  id: string;
  term: string;
  abbr?: string;
  /** <= 35 words: the InfoTip text */
  short: string;
  /** 1-3 sentences: the glossary page and About pages */
  more?: string;
  modules: GlossaryModule[];
  related?: string[];
}

/** Values filled into {{placeholders}} unless a manifest supplies newer ones (the six-stage build,
 *  2026-10-28: 11 headline technologies x 6 stages = 66 sectors; the Sector pages add Semiconductors'
 *  Refining, Components and Assembly: 69 cells; the bundles of 2026-10-30: the coalition of 31 core members
 *  and 28 partners, a basket of 261 HS6 codes with the quartz and silver ores). */
export const GLOSSARY_DEFAULTS = {
  n_core: '31',
  n_partner: '28',
  n_codes: '261',
  un_window: '2019-2023',
  n_tech: '11',
  n_sectors: '66',
  n_sector_cells: '69',
} as const;

export type GlossaryVars = Partial<Record<keyof typeof GLOSSARY_DEFAULTS, string>>;

const ALL: GlossaryModule[] = ['sector', 'strategic', 'country'];
const SC: GlossaryModule[] = ['strategic', 'country'];
/** the strategy's terms: every page, the Allied Industrial Base included */
const EVERY: GlossaryModule[] = [...ALL, 'aib'];
/** the Allied Industrial Base's own terms (the Country profile's Allied Strategy uses them too) */
const AIB: GlossaryModule[] = ['aib', 'country'];
/** the partnership measures: the Country and Allied Industrial Base pages and the Legacy OSI views */
const SCA: GlossaryModule[] = [...SC, 'aib'];

/** A sentence with its first letter lower-cased, to follow a colon ("Small yard: capabilities where ..."). */
const lc = (t: string): string => t.charAt(0).toLowerCase() + t.slice(1);

/** The entries as written, with {{placeholders}}. */
export const GLOSSARY_TEMPLATES: readonly GlossaryEntry[] = [
  // ---- the indices ---------------------------------------------------------------------------
  {
    id: 'es',
    term: 'Energy Security',
    abbr: 'ES',
    short: 'How well a country can rely on what it needs in a sector: own production and reserves, import needs, concentration of world supply, price stability. 0 to 1; higher is more secure.',
    more: 'Built for each technology and supply-chain stage from up to ten kinds of data: production, reserves, the gap between production and consumption, market position, trade balance, how concentrated world exports and critical-mineral trade are, price volatility, energy access, consumption growth and investment. Each measure is ranked against every other economy in the same sector, so 0.8 means doing better than most there. It is a relative score, not a probability of disruption, and it does not measure how many suppliers the country itself buys from. One of the security and opportunity scores, a descriptive view: the strategic choices do not read it.',
    modules: ALL,
    related: ['eo', 'need', 's_curve', 'rankable'],
  },
  {
    id: 'eo',
    term: 'Economic Opportunity',
    abbr: 'EO',
    short: 'How well placed a country is to create value in a sector: exports and market share, related capabilities, investment, costs, demand and technology readiness. 0 to 1; higher is a bigger opening.',
    more: 'Built like Energy Security from ten kinds of data (trade, including export feasibility from the Atlas of Economic Complexity; production; demand; technology readiness; costs; prices; investment; access; consumption; market position) and ranked within the sector. A high score means the country already has, or is close to having, what it takes to compete there. One of the security and opportunity scores, a descriptive view: not the same as Industrial Opportunity, the prize the strategic choices read.',
    modules: ALL,
    related: ['es', 'feasibility', 's_curve'],
  },
  {
    id: 'si',
    term: 'Strategic Index',
    abbr: 'SI',
    short: "Ranks a country's own sectors by how strongly the data point to prioritising them: opportunity, security need, existing policy activity, the stage and the technology's low-emission value.",
    more: 'Baseline weights: Economic Opportunity 30%, security need (1 minus Energy Security) 30%, policy activity 20%, stage 10%, technology 10%. The stage term favours manufacturing: Components and Assembly 0.375 each, Deployment 0.25, Extraction, Refining and Recovery about 0.167 each. It compares sectors within one country: compare countries by rank, not by level. One of the security and opportunity scores, a descriptive view and not a recommendation: the strategic choices are the recommendation.',
    modules: SC,
    related: ['need', 'policy_index', 'allied_pick', 'priority_no_policy'],
  },
  {
    id: 'need',
    term: 'Security need',
    short: '1 minus Energy Security: how exposed a country is in a sector. The Strategic Index rewards it, so exposed sectors rank higher.',
    more: 'Part of the Strategic Index. Not the same as supply-chain vulnerability in the country strategy, which reads import dependence, supplier concentration, exposure to China and opposite-bloc suppliers and trusted alternatives.',
    modules: SC,
    related: ['es', 'si', 'supply_chain_need'],
  },
  {
    id: 'psi',
    term: 'Partnership Strength Index',
    abbr: 'PSI',
    short: 'Where two economies have the strongest case to deepen an energy-industrial relationship, across sourcing (Friendshore), markets (Partnership Opportunity) and investment (Development Potential). Not a call to cooperate on everything.',
    more: "Weights 40%, 40% and 20% (Development Potential only for middle- and lower-income partners). Each pair is summarised by its five strongest sectors. A country's profile shows it from that country's viewpoint; the coalition headline averages the viewpoints of the {{n_core}} core coalition members.",
    modules: SCA,
    related: ['fs', 'po', 'dp', 'best_five'],
  },
  {
    id: 'fs',
    term: 'Friendshore',
    abbr: 'FS',
    short: 'Where country A could source from or invest in country B to reduce a strategic dependency in a sector: B has room to build there, and the two already trade and invest.',
    more: "Components: A's security need in the sector (1 minus its Energy Security), B's Economic Opportunity there (its room to build production and value, not its standing as a supplier today), A's imports from B, A's direct investment in B, UN-vote alignment and 1 minus the Chinese-parent share of B's foreign-owned factories, less a climate penalty for emission-intensive technologies. A's need is the same for every partner in one sector: it sets how much the sector matters to A, while the ranking of partners within the sector turns on B's opportunity, the trade and investment ties, alignment and ownership. It shows where investment and sourcing could go, not today's largest suppliers (the import shares show those).",
    modules: SCA,
    related: ['psi', 'alignment', 'investment_ties', 'chinese_parent'],
  },
  {
    id: 'po',
    term: 'Partnership Opportunity',
    abbr: 'PO',
    short: "Where country B offers an attractive market for what country A is well placed to supply in a sector: A already sells there, A is competitive, and B needs the goods.",
    more: "Components: A's exports to B, A's economic opportunity and B's security need (weights 3, 3, 1), less a climate penalty.",
    modules: SCA,
    related: ['psi', 'fs'],
  },
  {
    id: 'dp',
    term: 'Development Potential',
    abbr: 'DP',
    short: 'Where joint investment could create a new supply base while building productive capacity in a middle- or lower-income partner, from development, investment-climate, aid and energy-use data. Not scored for high-income partners.',
    modules: SCA,
    related: ['psi'],
  },
  {
    id: 'policy_index',
    term: 'Policy activity',
    short: 'How much policy a government has in a sector: IEA policy records and, where rated, its Climate Action Tracker rating read against the technology. It does not judge whether policies work.',
    more: "Weights: IEA records 3, Climate Action Tracker 2. The Tracker part depends on the technology: a strong climate rating scores high for low-emission technologies and low for coal, oil and gas (a weak rating the reverse). Countries the Tracker does not rate are scored on IEA records alone; EU members without their own rating carry the EU's. In the Strategic Index it enters as a rank among the country's own sectors.",
    modules: SC,
    related: ['pams', 'cat', 'policy_basis', 'si'],
  },
  {
    id: 'policy_basis',
    term: 'Policy basis',
    short: "Which sources a country's policy activity rests on: IEA records plus the Climate Action Tracker, or IEA records only. Compare policy levels across countries only within one basis.",
    modules: SC,
    related: ['policy_index'],
  },
  {
    id: 'pams',
    term: 'IEA Policies and Measures',
    abbr: 'PAMS',
    short: "The International Energy Agency's database of national energy and climate policies. Here, records are counted by the technology and stage they concern.",
    modules: ALL,
    related: ['policy_index'],
  },
  {
    id: 'cat',
    term: 'Climate Action Tracker rating',
    abbr: 'CAT',
    short: 'An independent rating of a country\'s climate policies and targets against the Paris Agreement, from "Critically insufficient" to "1.5°C compatible".',
    modules: ALL,
    related: ['policy_index'],
  },
  // ---- how scores are built ------------------------------------------------------------------
  {
    id: 'rankable',
    term: 'Rankable',
    short: "A score is ranked only when the economy is sovereign and at most half of the score's weight rests on filled-in data. Other scores are shown but marked.",
    modules: SC,
    related: ['imputed'],
  },
  {
    id: 'imputed',
    term: 'Filled-in data',
    short: 'Where a measure is missing, the pipeline fills it (with zero when a source lists every holder, else with the world average). The filled-in share is the part of a score resting on such fills.',
    modules: SC,
    related: ['rankable'],
  },
  {
    id: 's_curve',
    term: 'Rank scaling',
    short: 'Each measure is turned into a rank among economies in the same sector and gently stretched so middle values spread out. Scores therefore run from 0 to 1 and compare countries, not quantities.',
    modules: SC,
    related: ['es', 'eo'],
  },
  {
    id: 'percentile',
    term: 'Share of economies beaten',
    short: 'The share of the other economies scored in the same sector with a lower score: 80% means higher than 80% of them.',
    more: "Ties count half. A country's profile uses the economies whose two scores can both be compared in that sector.",
    modules: ['country'],
    related: ['median'],
  },
  {
    id: 'median',
    term: 'Median',
    short: "The middle value: half are above it, half below. A country's profile splits its corners at the country's median sector; colour scales use white for the median.",
    modules: ALL,
    related: ['colour_scale', 'percentile'],
  },
  {
    id: 'colour_scale',
    term: 'Colour scale',
    short: 'Orange is lower, white is the median of the values shown, blue is higher. For changes and signed values, white is zero.',
    modules: ALL,
    related: ['median'],
  },
  // ---- the frame -----------------------------------------------------------------------------
  {
    id: 'sector',
    term: 'Sector',
    short: 'One technology at one supply-chain stage, for example Batteries — Refining. The indices cover {{n_tech}} technologies at six stages: {{n_sectors}} sectors; the Sector pages add three Semiconductors stages ({{n_sector_cells}} in all).',
    more: 'A few sectors have no value for an index (most geothermal stages, or the recovery of coal, gas and oil): they are shown as not scored, never dropped.',
    modules: ALL,
    related: ['stage'],
  },
  {
    id: 'stage',
    term: 'Supply-chain stage',
    short: 'One of six links of a supply chain, in order: Extraction (mining and fuels), Refining (processed materials), Components (parts), Assembly (finished equipment), Deployment (installation and use) and Recovery (recycling).',
    more: "Stage is relative to the technology: a battery pack is Assembly for Batteries and Components for Electric Vehicles. Each traded product code sits at the stage of what it physically is, so refined battery chemicals and polysilicon are Refining. Deployment is where the technology is used: measures of who supplies the chain (allied picks, the allied division of labour) leave it out.",
    modules: ALL,
    related: ['sector', 'basket'],
  },
  {
    id: 'coalition',
    term: 'Allied coalition',
    short: 'The group of economies the analysis studies: {{n_core}} core members (U.S.-leaning in UN votes) and {{n_partner}} partners (in U.S. trade or security agreements, or major suppliers not leaning towards China).',
    more: 'Core members: U.S.-leaning in UN votes, with economies of at least $100 billion. Partners: members of U.S. trade or security agreements with economies of at least $100 billion, or major suppliers that do not lean towards China (at least 5% of world supply at a stage other than deployment, or 10% of the reserves of a headline mineral or fuel).',
    modules: EVERY,
    related: ['alignment'],
  },
  {
    id: 'alignment',
    term: 'UN-vote alignment',
    short: 'Where a country sits between the United States and China in UN General Assembly votes, {{un_window}}. Two countries are aligned when they are in the same bloc: U.S.-leaning, nonaligned or China-leaning.',
    modules: ALL,
    related: ['coalition', 'fs'],
  },
  {
    id: 'allied_pick',
    term: 'Allied pick',
    short: "Each coalition member's top sector on the Strategic Index at any stage but deployment, leaving out geothermal and green hydrogen and, after the endowment screen, extraction sectors without recorded resources.",
    modules: SC,
    related: ['si', 'endowment_screen'],
  },
  {
    id: 'endowment_screen',
    term: 'Endowment screen',
    short: "An extraction sector counts as a member's allied pick only if the country has recorded reserves or production of that sector's resources. The index itself does not change.",
    modules: ['strategic'],
    related: ['allied_pick'],
  },
  {
    id: 'best_five',
    term: 'Best five sectors',
    short: 'A pair of countries is summarised by the average of its five strongest sectors, so a few strong links count more than many weak ones.',
    modules: SC,
    related: ['psi'],
  },
  // ---- trade and market measures (Sector Insights) ---------------------------------------------
  {
    id: 'hhi',
    term: 'Concentration (HHI)',
    abbr: 'HHI',
    short: "How concentrated a market is: square each supplier's share and add them up. 1 means one supplier has everything; 0.25 is like four equal suppliers; lower is more spread out.",
    more: 'The Herfindahl-Hirschman Index. Its inverse is the effective number of suppliers. Where a source leaves part of the market unattributed, the value shown is a lower bound ("at least").',
    modules: ALL,
    related: ['effective_n'],
  },
  {
    id: 'effective_n',
    term: 'Effective number of suppliers',
    short: '1 divided by the concentration index: how many equal-sized suppliers would give the same concentration.',
    modules: ['sector', 'strategic'],
    related: ['hhi'],
  },
  {
    id: 'rca',
    term: 'Revealed comparative advantage',
    abbr: 'RCA',
    short: "A country's share of world exports of a product group divided by its share of all world exports. Above 1, it exports more of these goods than its size suggests.",
    modules: ['sector', 'country'],
  },
  {
    id: 'feasibility',
    term: 'Export feasibility',
    short: "How close a sector's products are to what a country already exports well, from the Harvard Atlas of Economic Complexity's product space. Higher means the needed capabilities are nearer at hand.",
    more: "Computed as 1 minus the average of the Atlas's distance over the sector's products, where distance measures how far a product lies from the country's current export strengths.",
    modules: ALL,
    related: ['eo'],
  },
  {
    id: 'matrix',
    term: 'Reconciled trade matrix',
    short: "One table of who trades with whom in each sector's goods: the importer's report where it exists, otherwise the exporter's report raised by 6% to value it like imports.",
    modules: ALL,
    related: ['mirror', 'basket'],
  },
  {
    id: 'mirror',
    term: 'Mirror trade',
    short: "Trade read from the partner's report because a country did not report it.",
    modules: ['sector', 'country'],
    related: ['matrix'],
  },
  {
    id: 'basket',
    term: 'Product basket',
    short: "The six-digit customs codes (HS6) that stand for a sector's traded goods: {{n_codes}} codes in all. Some codes count in more than one sector, so baskets are never added up across sectors.",
    modules: ALL,
    related: ['sub_component', 'matrix'],
  },
  {
    id: 'sub_component',
    term: 'Sub-component',
    short: 'A group of codes inside a basket, such as cells versus modules in solar.',
    modules: ['sector', 'country'],
    related: ['basket'],
  },
  {
    id: 'net_balance',
    term: 'Net balance',
    short: "Production minus consumption of a fuel. Negative means a net importer. Where no source reports a country's production separately, it is taken as zero and marked.",
    modules: ['sector', 'country'],
    related: ['self_sufficiency'],
  },
  {
    id: 'self_sufficiency',
    term: 'Self-sufficiency',
    short: 'Production as a percentage of consumption; 100% means producing as much as is used.',
    modules: ['sector', 'country'],
    related: ['net_balance'],
  },
  {
    id: 'cmfdi',
    term: 'Factory projects (CM-FDI)',
    abbr: 'CM-FDI',
    short: "Cross-border manufacturing projects in solar, wind, batteries and electric vehicles, with the parent company's home country, from the Cleantech Manufacturing FDI dataset (Net Zero Industrial Policy Lab, Johns Hopkins University).",
    more: "Announced projects are included and may never be built; a company's investment at home is not covered. The projects make components and finished equipment, so the same projects sit under both the Components and the Assembly stage of each technology, and are counted once.",
    modules: ALL,
    related: ['chinese_parent', 'investment_ties'],
  },
  {
    id: 'chinese_parent',
    term: 'Chinese-parent ownership',
    short: "The share of a country's foreign-owned clean-tech manufacturing whose parent company is headquartered in China, Hong Kong or Macao. Friendshore treats a lower share as more secure.",
    modules: ALL,
    related: ['cmfdi', 'fs'],
  },
  {
    id: 'investment_ties',
    term: 'Direct investment ties',
    short: 'Investment links between two countries: factory projects (CM-FDI) in four clean-tech manufacturing sectors, IMF direct-investment positions elsewhere.',
    modules: SC,
    related: ['cmfdi', 'fs'],
  },
  {
    id: 'trl',
    term: 'Technology readiness',
    abbr: 'TRL',
    short: "How mature a technology is, on the IEA's scale from 1 (basic research) to 11 (proven at scale); a global value, not a national one.",
    modules: ALL,
  },
  {
    id: 'unattributed',
    term: 'Not attributed',
    short: 'The part of a total that a source does not assign to any named economy (its "rest of world").',
    modules: ['sector', 'country'],
  },
  // ---- baseline, scenarios and dials ---------------------------------------------------------
  {
    id: 'paper_baseline',
    term: 'Baseline',
    short: "Every dial at 50 and the default settings: the pipeline's own six-stage weights. Every index starts from it; only a custom perspective on a country moves priorities and partner rankings away from it.",
    more: 'The dials re-weight only the security and opportunity scores (the Strategic Index and the partner scores); the strategic choices (Secure, Preserve, Catch Up, Partner, Accelerate, Watch) do not move with them.',
    modules: SC,
    related: ['scenario', 'dials'],
  },
  {
    id: 'scenario',
    term: 'Custom perspective',
    short: 'Any other setting of the four dials or the advanced options, set on a country\'s profile and recomputed in your browser. It moves priorities and partner rankings, never the observed facts.',
    modules: ['strategic'],
    related: ['paper_baseline', 'dials'],
  },
  {
    id: 'dials',
    term: 'Priority dials',
    short: 'Four sliders behind a country\'s Perspective that re-weight priorities and partner rankings: hawkishness, bullishness, populism and greenness (climate). 50 is the baseline.',
    modules: SC,
    related: ['hawkishness', 'bullishness', 'populism', 'greenness'],
  },
  {
    id: 'hawkishness',
    term: 'Hawkishness',
    short: 'Security first: more weight on where countries are exposed and on aligned partners.',
    modules: ['strategic'],
    related: ['dials'],
  },
  {
    id: 'bullishness',
    term: 'Bullishness',
    short: 'Opportunity first: more weight on economic opportunity and export markets.',
    modules: ['strategic'],
    related: ['dials'],
  },
  {
    id: 'populism',
    term: 'Populism',
    short: "Policy signals and manufacturing: more weight on governments' policy activity and on the components and assembly stages.",
    modules: ['strategic'],
    related: ['dials'],
  },
  {
    id: 'greenness',
    term: 'Greenness',
    short: 'Climate first: more weight on low-emission technologies and climate policy; at 90 or above, coal, oil and gas drop out.',
    modules: ['strategic'],
    related: ['dials'],
  },
  // ---- Country profile: the four corners (Legacy OSI) and the comparisons --------------------------
  // The corners of the Security and opportunity chart (country/contract.ts), kept and recomputed at the six
  // stages (user, 2026-10-28) as the Legacy OSI framework (2026-10-29: the paper baseline, under a Country
  // profile's Evidence & Methodology). Since 2026-10-28 the corners once keyed secure and watch are shown as
  // Exposed and Stable, so they are not read as the posture words; their glossary ids follow the names.
  // Each names its closest current choice (Grow ~ Preserve, Upgrade ~ Catch Up, Exposed ~ Partner,
  // Stable ~ Watch), and says the results differ.
  {
    id: 'grow',
    term: 'Grow',
    short: "High opportunity, high energy security: build on strength. A corner of the Security and opportunity chart; not a strategic choice.",
    more: "Placed against the country's median sector; a sector exactly at the median counts as high. In the Security vs opportunity comparison, which shows every economy in one sector, high and low are against the sector's median economy instead. Its closest strategic choice is Preserve, but the results differ (see Security and opportunity scores).",
    modules: SC,
    related: ['upgrade', 'exposed', 'stable', 'quadrant', 'legacy_osi'],
  },
  {
    id: 'upgrade',
    term: 'Upgrade',
    short: "High opportunity, low energy security: building capability could close a gap and create value. A corner of the Security and opportunity chart; not a strategic choice.",
    more: "Placed against the country's median sector; a sector exactly at the median counts as high. In the Security vs opportunity comparison, which shows every economy in one sector, high and low are against the sector's median economy instead. Its closest strategic choice is Catch Up, but the results differ (see Security and opportunity scores).",
    modules: SC,
    related: ['grow', 'exposed', 'stable', 'quadrant', 'legacy_osi'],
  },
  {
    id: 'exposed',
    term: 'Exposed',
    short: "Low opportunity, low energy security: lean on trusted partners for supply. A corner of the Security and opportunity chart; not the Secure choice.",
    more: "Placed against the country's median sector; a sector exactly at the median counts as high. In the Security vs opportunity comparison, which shows every economy in one sector, high and low are against the sector's median economy instead. Its closest strategic choice is Partner, but the results differ (see Security and opportunity scores). The country memos call this corner Secure; it is not the Secure choice.",
    modules: SC,
    related: ['grow', 'upgrade', 'stable', 'quadrant', 'legacy_osi'],
  },
  {
    id: 'stable',
    term: 'Stable',
    short: "Low opportunity, high energy security: little pressure to act; value may lie elsewhere. A corner of the Security and opportunity chart; not a strategic choice.",
    more: "Placed against the country's median sector; a sector exactly at the median counts as high. In the Security vs opportunity comparison, which shows every economy in one sector, high and low are against the sector's median economy instead. Its closest strategic choice is Watch, but the results differ (see Security and opportunity scores). The country memos call this corner Watch; it is not the Watch choice.",
    modules: SC,
    related: ['grow', 'upgrade', 'exposed', 'quadrant', 'legacy_osi'],
  },
  {
    id: 'quadrant',
    term: 'Corner (quadrant)',
    short: "One of the four corners of a profile's Security and opportunity chart. The dashed lines sit at the country's median sector on each axis, so the corners show its relatively strong and weak sectors.",
    more:
      "The median sector is the middle of the country's own sectors on each axis; a sector exactly at it counts as high. The axes still compare the country with other economies: each sector sits at the share of them it beats. The corners are Grow, Upgrade, Exposed and Stable, over the country's sectors at the six stages; they belong to the security and opportunity scores, a descriptive view, not to the strategic choices.",
    modules: ['country'],
    related: ['grow', 'upgrade', 'exposed', 'stable', 'percentile', 'median', 'posture', 'legacy_osi'],
  },
  {
    id: 'legacy_osi',
    term: 'Security and opportunity scores',
    short: 'A descriptive index view: Energy Security, Economic Opportunity, the Strategic Index built on them, the partner scores and the four corners. They describe relative security and opportunity; the strategic choices are the recommendation.',
    more:
      'The corners map loosely onto the strategic choices: Grow to Preserve, Upgrade to Catch Up, Exposed to Partner, Stable to Watch. Results differ because each index mixes capability, vulnerability, production, trade, investment, demand, reserves, concentration and deployment in one score, which the strategic position framework separates into need, prize and position. On a Country profile they sit under Evidence & Methodology, with the dials as scenarios to explore.',
    modules: EVERY,
    related: ['es', 'eo', 'si', 'quadrant', 'posture', 'dials'],
  },
  {
    id: 'priority_no_policy',
    term: 'Priority without policy',
    short: 'The Strategic Index without its policy term, used only to compare data-based priorities with current policy without counting policy twice.',
    modules: ['country'],
    related: ['si', 'policy_index'],
  },
  {
    id: 'ties_today',
    term: 'Trade ties today',
    short: "A partner's average share of a country's imports or exports across its energy-goods baskets in 2024. Shares are averaged, never added across baskets.",
    modules: ['country'],
    related: ['matrix', 'basket'],
  },
  {
    id: 'memo',
    term: 'AI-generated memo',
    short: "A short brief an AI model wrote from the country's fact sheet only. Software fills in every number from the data; automatic checks and a second AI model check every sentence against it.",
    modules: ['country'],
  },
  // Development & finance raw data (Sector Insights; development data change 2026-10-15, additive)
  {
    id: 'oda',
    term: 'Official development assistance',
    short: 'Grants and concessional loans from donor governments to developing economies, as reported to the OECD Creditor Reporting System; shown by aid sector (energy, industry, mining) in constant 2023 US dollars.',
    more: 'Bilateral donors only: multilateral lenders such as the World Bank and private flows are not in the data. Development Potential uses the aid an economy received for the stage over 2013-2024.',
    modules: ['sector'],
    related: ['dp'],
  },
  {
    id: 'dip_position',
    term: 'Direct-investment position',
    short: "What firms in one economy hold in their related firms in another, from the IMF's Direct Investment Positions; here the debt part (loans between related firms), as the investing economy reports it.",
    more: 'A stock, not a yearly flow: the mean of the years reported in 2020-2024. It is the investment tie behind Friendshore outside the four factory-project (CM-FDI) sectors.',
    modules: ['sector'],
    related: ['investment_ties', 'conduit_centre', 'fs'],
  },
  {
    id: 'conduit_centre',
    term: 'Conduit financial centre',
    short: 'An economy through which investment passes on its way elsewhere, often via shell companies (the Netherlands, Luxembourg, Hong Kong, Singapore and others). Positions with one are flagged and left out of Friendshore.',
    more: 'The list follows Damgaard, Elkjaer and Johannesen (IMF, 2019): ten economies host more than 85% of such pass-through investment; six other offshore centres are treated the same way.',
    modules: ['sector'],
    related: ['dip_position'],
  },
  {
    id: 'wdi',
    term: 'World Development Indicators',
    short: "The World Bank's statistics on each economy's development: income, energy access and use, credit, debt, tariffs, governance and business regulation. Development Potential ranks them; the Sector pages show the values.",
    modules: ['sector'],
    related: ['dp'],
  },
  // ---- the strategy (canonical since 2026-10-29; plain words first since the NEIS copy refresh, 2026-10-30) -------
  // Need -> Prize -> Position -> Choice -> Policy alignment. The ids of posture/basis.tsx POSTURE_TERMS.
  // 'policy_alignment' is not UN-vote 'alignment', and 'strategic_need' is not the Strategic Index's
  // security 'need'. Each `short` says what the term means for a decision; `more` says how it is measured.
  {
    id: 'posture',
    term: 'Strategic choice',
    short: 'What a country should do at one technology and supply-chain stage: Secure, Preserve, Catch Up, Partner, Accelerate or Watch. Set from need, prize and position at fixed thresholds; current policy is compared afterwards.',
    more:
      'Secure comes first and overrides the rest. At Deployment & Use, high deployment need means Accelerate, otherwise Watch; at the producing stages need and prize triage the step and Competitive Position decides the response (see each choice). In the data the six choices sit in three families: Secure, Compete (Preserve, Catch Up or Partner) and Deploy (Accelerate or Watch). Observed policy never feeds the choice, and the Perspective dials do not change it.',
    modules: EVERY,
    related: ['choice_secure', 'choice_preserve', 'choice_catch_up', 'choice_partner', 'choice_accelerate', 'choice_watch', 'strategic_need', 'strategic_industrial_value', 'competitive_position', 'policy_alignment', 'yard', 'legacy_osi'],
  },
  {
    id: 'strategic_need',
    term: 'Strategic Need',
    short: 'The problem a country has to solve at a supply-chain step, measured two separate ways: supply-chain vulnerability at the producing stages and deployment need at Deployment & Use. They are never combined into one score.',
    more: "Only the relevant one appears for each step. Not the same as the Strategic Index's security need (1 minus Energy Security).",
    modules: EVERY,
    related: ['supply_chain_need', 'deployment_need', 'need_type', 'posture', 'need'],
  },
  {
    id: 'supply_chain_need',
    term: 'Supply-chain need (vulnerability)',
    short: 'How vulnerable a country is at a producing-stage capability: import dependence, supplier concentration, exposure to risky suppliers and the lack of trusted alternatives, compared with other countries. Producing stages only.',
    more:
      'Each part is a percentile among the countries at the same technology and stage; higher is more vulnerable. Risky suppliers are China and suppliers in the opposite UN-vote bloc. A percentile counts as high only where the need score itself reaches 0.10, so a tiny exposure in a thinly traded step does not read as high need; a country holding at least 10% of world supply scores no need from the make-up of its imports. Substitutability, rebuild time, inventories, essentiality and operational control feed the Secure tests and are shown beside it.',
    modules: EVERY,
    related: ['strategic_need', 'deployment_need', 'security_rationale', 'posture'],
  },
  {
    id: 'deployment_need',
    term: 'Deployment need',
    short: 'How urgently a country needs more of a technology in use: expected demand, the gap to its national targets and energy-system requirements. Deployment & Use only; each part names its scenario.',
    more:
      "Demand adequacy reads current policy and the target gap national commitments (Ember's 2030 targets); a climate-aligned value is shown only as a labelled alternative. Each part says whether it rests on national data, a regional proxy, an observed trend or global context; a missing part is not assessed, never zero. It is not supply-chain vulnerability: high deployment need means Accelerate, never Partner.",
    modules: EVERY,
    related: ['strategic_need', 'supply_chain_need', 'deploy_mode', 'choice_accelerate', 'posture'],
  },
  {
    id: 'need_type',
    term: 'Need type',
    short: 'Which need a step reads: supply-chain vulnerability at Extraction, Refining & Processing, Component Manufacturing, Final Assembly & Equipment and Recovery & Recycling; deployment need at Deployment & Use.',
    modules: EVERY,
    related: ['supply_chain_need', 'deployment_need'],
  },
  {
    id: 'strategic_industrial_value',
    term: 'Strategic Industrial Value',
    short:
      'The prize: how valuable it would be to build or keep productive capability in a supply-chain step. It combines the market prize (Industrial Opportunity), industrial-commons value and future-option value.',
    more:
      'Industrial commons reads process learning, the workforce and supplier ecosystem and spillovers to other sectors; future option reads whether a credible leap ahead is in reach. A large market alone never makes the prize high: where neither commons nor option reaches the gate, the value is capped just below the prize bar (market only). It reads no current capability and no need variable, and it is a percentile across the producing supply-chain steps.',
    modules: EVERY,
    related: ['posture', 'industrial_opportunity', 'competitive_position', 'yard', 'large_yard_case'],
  },
  {
    id: 'industrial_opportunity',
    term: 'Industrial Opportunity',
    short: 'The market prize: is there a market worth pursuing at a supply-chain step? Expected world demand growth to 2035 under current policy and investment momentum, lifted by national home-market growth where measured.',
    more:
      'One of the three families of the Strategic Industrial Value, the prize the choice reads. It reads no current capability (exports, production, capacity, investment, reserves) and no need variable, so it stays distinct from Competitive Position and Strategic Need; a declining market is capped below the bar. Not the Economic Opportunity score of the security and opportunity view.',
    modules: EVERY,
    related: ['strategic_industrial_value', 'posture', 'competitive_position', 'pathway', 'eo'],
  },
  {
    id: 'competitive_position',
    term: 'Competitive Position',
    short: "How strong a country already is at a supply-chain step: exports and specialization, production, operating capacity, investment, costs and resources where relevant, with trade momentum. Compared with the step's main producers.",
    more:
      'Demonstrated capability only: no market growth, need, policy or climate goal. Its trade evidence is a 2022-24 benchmark of exports, world share and revealed comparative advantage; current trade momentum takes 15% of the trade evidence where a matched 12-month comparison exists. The main producers (contenders) are the economies with at least 0.5% of world supply; with a high prize, the top quarter of them preserve, and the others catch up, partner or watch.',
    modules: EVERY,
    related: ['posture', 'industrial_opportunity', 'trade_momentum', 'rca'],
  },
  {
    id: 'pathway',
    term: 'Credible pathway',
    short: 'The Catch Up test: a high prize without a strong position is Catch Up only with a real base to build on (a capability anchor) and a corroborating signal. Unknown counts as not credible.',
    more:
      'Anchors are absolute: at least 0.5% of world supply or exports, 0.5% of world operating capacity, 1% in committed capacity, an adjacent stage with 5% of world supply or, at Extraction, a recorded resource base (which Extraction always needs). Supporting signals (product-space feasibility, an input-cost advantage, learning inputs, a relatively strong adjacent stage, announced capacity) corroborate an anchor of another kind but never qualify alone. Without a credible pathway, high need means Partner and low need Watch.',
    modules: EVERY,
    related: ['choice_catch_up', 'posture', 'industrial_opportunity', 'competitive_position', 'feasibility'],
  },
  {
    id: 'deploy_mode',
    term: 'Accelerate or Watch',
    short: 'At Deployment & Use, Accelerate where deployment need is high (deployment itself is the priority), Watch where it is not. Watch also ends the triage at the producing stages: low domestic priority.',
    more:
      'Accelerate says nothing about making the equipment at home: a technology can be a low manufacturing priority and a high deployment priority. Watch is a positive conclusion, not a failure: no major domestic industrial intervention is needed (low need, a low prize, ample diversified supply or a weak domestic case).',
    modules: EVERY,
    related: ['choice_accelerate', 'choice_watch', 'deployment_need'],
  },
  {
    id: 'security_rationale',
    term: 'Secure rationale',
    short: 'Why a step is Secure: control-system risk (a foreign actor could command or disable it), chokepoint risk (a coercive supplier could withhold it) or system resilience (essential, import-dependent and slow to rebuild).',
    more: 'Each route also needs an acute consequence. Concentration alone does not make a chokepoint: substitution must be hard and trusted supply too slow to restore. Secure does not mean reshoring.',
    modules: EVERY,
    related: ['choice_secure', 'delivery_mode', 'high_fence'],
  },
  {
    id: 'delivery_mode',
    term: 'Delivery mode',
    short: 'How a Secure, Preserve, Catch Up or Partner choice is delivered: at home, with allies, from a diversified global market or a mix. Stockpiles, trusted sourcing, substitution and long-term contracts all count.',
    modules: EVERY,
    related: ['security_rationale', 'posture'],
  },
  {
    id: 'consistency_flag',
    term: 'Consistency review',
    short: 'A supply-chain step whose evidence pulls against its choice, flagged for review rather than silently overridden: for example a strong position with high need but a low prize, kept as Preserve rather than Partner.',
    more: "A Catch Up or Partner step whose position is in the main producers' top quarter but below the capability floor is flagged too.",
    modules: EVERY,
    related: ['posture', 'competitive_position'],
  },
  {
    id: 'policy_alignment',
    term: 'Policy alignment',
    short: 'Whether government action matches the recommended choice: broadly aligned, needs more attention, too much intervention, wrong tool for the problem or mixed signals, with a confidence. Policy never feeds the choice.',
    more:
      'Secure, Preserve or Catch Up with weak policy needs more attention, as does Accelerate with weak deployment policy (deployment policy lags); Partner met with heavy localization is the wrong tool for the problem; Watch under heavy protection is too much intervention. Where policy records are thin it says not enough policy evidence or low confidence, never a guess. Not the same as UN-vote alignment.',
    modules: EVERY,
    related: ['posture', 'policy_index'],
  },
  {
    id: 'partner_gap',
    term: 'Partner gap',
    short: "A supply-chain step where a country's choice is Partner but no credible allied answer exists yet: no trusted supplier, only one, weak capability evidence, too little allied capacity or concentration that stays excessive.",
    more:
      "Elsewhere a Partner step names up to three best-placed trusted partners, ranked on capability, relationship strength and how much they would diversify the country's supply. A genuine gap is reported, never forced into a solution.",
    modules: EVERY,
    related: ['choice_partner', 'aib_gap'],
  },
  {
    id: 'trade_momentum',
    term: 'Trade momentum',
    short: "Whether a country's exports at a step are rising, flat or falling: the latest 12 months against the 12 before, in growth and in the basket's share of its exports, where monthly reporting is complete.",
    more: "Ranked against the step's other reporters. A reporter without the 24 matched months has no momentum (never zero).",
    modules: EVERY,
    related: ['competitive_position'],
  },
  {
    id: 'aib_gap',
    term: 'Allied gap',
    short: 'Where trusted allied supply of a step is thin: no trusted supplier, a single point of failure, too little redundancy or allied capacity, too much concentration, or weak evidence.',
    more:
      'From the allied division of labor: per step the primary allied holder, two backups and the number of allied holders with at least 2% of supply (fewer than three is too little redundancy). Deployment is left out: no allied supply step sits there.',
    modules: EVERY,
    related: ['partner_demand', 'partner_gap', 'allied_industrial_base'],
  },
  {
    id: 'partner_demand',
    term: 'Partner demand',
    short: "How many countries' choice for a step is Partner: they look to trusted partners for it. Secure demand counts Secure choices; Accelerate is a deployment priority and is not counted.",
    more: 'Shown beside the allied gap, so a gap many countries rely on stands out.',
    modules: EVERY,
    related: ['aib_gap', 'choice_partner'],
  },
  // ---- the six choices, as actions (the NEIS copy refresh, 2026-10-30; vocabulary CHOICE_DEFINITION) ------------
  // Ids carry a prefix: 'secure' and 'watch' were the Legacy OSI corners' ids before 2026-10-28.
  {
    id: 'choice_secure',
    term: 'Secure',
    short: CHOICE_DEFINITION.secure,
    more: 'Secure comes first and overrides the other tests. It needs an acute consequence and one of three routes: control-system risk, chokepoint risk or system resilience (see Secure rationale). It is the one choice behind the High fence.',
    modules: EVERY,
    related: ['security_rationale', 'delivery_mode', 'high_fence', 'posture'],
  },
  {
    id: 'choice_preserve',
    term: 'Preserve',
    short: CHOICE_DEFINITION.preserve,
    more: "A high prize with a position in the top quarter of the step's main producers and above the capability floor. A strong position with high need but a low prize is also kept as Preserve, and flagged for review.",
    modules: EVERY,
    related: ['competitive_position', 'strategic_industrial_value', 'medium_fence', 'posture'],
  },
  {
    id: 'choice_catch_up',
    term: 'Catch Up',
    short: CHOICE_DEFINITION.catch_up,
    more: 'A high prize without a top-quarter position, and a credible pathway: a capability anchor plus a corroborating signal (see Credible pathway).',
    modules: EVERY,
    related: ['pathway', 'strategic_industrial_value', 'medium_fence', 'posture'],
  },
  {
    id: 'choice_partner',
    term: 'Partner',
    short: CHOICE_DEFINITION.partner,
    more:
      'High need without a strong position or a credible pathway. The page names up to three best-placed trusted partners, ranked on capability, relationship strength and how much they would diversify supply; where none is credible it reports a partner gap.',
    modules: EVERY,
    related: ['partner_gap', 'partner_demand', 'medium_fence', 'posture'],
  },
  {
    id: 'choice_accelerate',
    term: 'Accelerate',
    short: CHOICE_DEFINITION.accelerate,
    more: 'Deployment & Use only: high deployment need. A technology can be a low manufacturing priority and a high deployment priority.',
    modules: EVERY,
    related: ['deployment_need', 'deploy_mode', 'medium_fence', 'posture'],
  },
  {
    id: 'choice_watch',
    term: 'Watch (keep open)',
    short: CHOICE_DEFINITION.watch,
    more:
      'A positive conclusion, not a failure: low need, a low prize, ample diversified supply, a weak domestic case or the large-yard case. At Deployment & Use it means deployment need is below the bar for acceleration. Missing data is never Watch: such a step is not assessed.',
    modules: EVERY,
    related: ['deploy_mode', 'large_yard_case', 'low_fence', 'posture'],
  },
  // ---- Yards & Fences (2026-10-30): the yards in the user's words, layers as yard units, the large-yard case -----
  {
    id: 'yard',
    term: 'Yards & Fences',
    short: 'Which of three policy problems a capability is: Small yard, High fence (Secure); Medium yard, permeable fence (Preserve, Catch Up, Partner, Accelerate); Large yard, Low fence (Watch).',
    more:
      'The yards are different problems, not low, medium and high scores on one scale. A technology can sit in more than one yard: capability layers, such as grid-control software or battery-management firmware, take their own choice, and a step whose layers differ is mixed (the Strategy Map splits its cell diagonally). A control layer is in the Small yard only where the data show coercive foreign control for that country.',
    modules: EVERY,
    related: ['high_fence', 'medium_fence', 'low_fence', 'large_yard_case', 'posture'],
  },
  {
    id: 'high_fence',
    term: 'High fence (Small yard)',
    short: `Small yard: ${lc(YARD_DEFINITION.small)} Its choice is Secure.`,
    more: 'A narrow set of capabilities where disruption, control or coercion would have serious consequences and stronger safeguards are justified. The answer is not always reshoring: trusted allied supply, stockpiles, substitution and redundancy can also solve the problem.',
    modules: EVERY,
    related: ['yard', 'choice_secure', 'security_rationale'],
  },
  {
    id: 'medium_fence',
    term: 'Medium fence (Medium yard)',
    short: `Medium yard: ${lc(YARD_DEFINITION.medium)} Its choices are Preserve, Catch Up, Partner, and Accelerate at Deployment & Use.`,
    more: 'A medium, permeable fence: build industrial capability, learning and competitiveness without needlessly cutting off useful foreign technology or partners. Where specialization is the better answer, the choice is Partner.',
    modules: EVERY,
    related: ['yard', 'choice_preserve', 'choice_catch_up', 'choice_partner', 'choice_accelerate'],
  },
  {
    id: 'low_fence',
    term: 'Low fence (Large yard)',
    short: `Large yard: ${lc(YARD_DEFINITION.large)} Its choice is Watch.`,
    more: 'Low-fence capabilities are not unimportant. The right strategy may be to buy competitively, keep suppliers diversified and focus policy elsewhere.',
    modules: EVERY,
    related: ['yard', 'choice_watch', 'large_yard_case'],
  },
  {
    id: 'large_yard_case',
    term: 'Large-yard case',
    short: 'The test that a capability is a commodity where affordable, diversified access should dominate, whatever the size of the market: mature and passive, with most of eight commodity signals holding.',
    more:
      'The eight signals: standardized, substitutable, quick to qualify, diversified world supply, little process learning, little cross-sector spillover, little product differentiation, commoditized. A credible leap ahead or strong industrial learning vetoes it; moderate learning keeps a capability floor. Market size is never a signal.',
    modules: EVERY,
    related: ['yard', 'low_fence', 'strategic_industrial_value'],
  },
  // ---- the Allied Industrial Base (the NEIS copy refresh, 2026-10-30; vocabulary.ts) ------------------------------
  {
    id: 'allied_industrial_base',
    term: 'Allied Industrial Base',
    short: 'A network of trusted countries that deliberately divide production, investment and demand across strategic supply chains, so no country has to make everything and no single supplier can hold the system at risk.',
    more: AIB_EXPLAINER,
    modules: AIB,
    related: ['managed_interdependence', 'sectoral_compact', 'coalition', 'aib_gap'],
  },
  {
    id: 'managed_interdependence',
    term: 'Managed Interdependence',
    short: 'Deliberate specialization with safeguards: keep the economic benefits of relying on partners while limiting the dependencies that create coercive or systemic risk.',
    more: `${MANAGED_INTERDEPENDENCE.explainer} As a stress test it caps how much of allied demand any one supplier may serve where a step is unusually concentrated, and asks which trusted suppliers would take the excess.`,
    modules: AIB,
    related: ['concentration_cap', 'aib_scenario', 'allied_industrial_base'],
  },
  {
    id: 'aib_scenario',
    term: 'Stress test (scenario)',
    short: "A what-if applied to today's supply network: today's network (no disruption), Managed Interdependence (a cap on any one supplier's share) or a major-supplier disruption (China stops supplying allied markets).",
    more: 'The scenario changes the supply problem, not the goal: the smallest credible network of partners that keeps critical supply available. Scenarios are counterfactual market structures, not forecasts.',
    modules: AIB,
    related: ['managed_interdependence', 'allied_coverage', 'remaining_gap'],
  },
  {
    id: 'aib_treatment',
    term: 'What a step needs from allies',
    short: 'The response a supply-chain step needs under a stress test: coordinate trade, build capacity, manage specialization, low intervention, or not enough evidence to say.',
    more: `Coordinate trade: ${lc(AIB_CLASS_DEFINITION.COORDINATE)} Build capacity: even the best coordinated trade response leaves a material gap, so new production or processing capacity is required. Manage specialization: too much supply sits with one supplier, so keep the specialization and add enough redundancy to reduce the risk.`,
    modules: AIB,
    related: ['coordination_dividend', 'remaining_gap', 'managed_interdependence'],
  },
  {
    id: 'sectoral_compact',
    term: 'Sectoral Compact',
    short: 'A targeted agreement among the countries needed to solve a shared supply-chain problem, aligning trade, investment and specialization around a specific value chain.',
    more: `${COMPACT.notAClub} ${COMPACT.reciprocity}`,
    modules: AIB,
    related: ['strategic_compact', 'minimum_coalition', 'demand_anchor', 'allied_industrial_base'],
  },
  {
    id: 'strategic_compact',
    term: 'Strategic Sectoral Compact',
    short: `${COMPACT.strategic} It does not change with the stress test.`,
    more: 'It describes where a country is structurally important to an allied supply chain: as a major supplier, demand anchor, technology specialist, investor, finance source or capacity host.',
    modules: AIB,
    related: ['sectoral_compact', 'allied_industrial_base'],
  },
  {
    id: 'allied_coverage',
    term: 'Allied coverage',
    short: AIB_MEASURE_DEFINITION.coverage,
    more: 'Read as a sequence: the supply that survives the stress test, what existing partners can replace, what coordinated trade adds, and the gap that needs new capacity.',
    modules: AIB,
    related: ['coordination_dividend', 'remaining_gap', 'aib_scenario'],
  },
  {
    id: 'coordination_dividend',
    term: 'Coordination dividend',
    short: AIB_MEASURE_DEFINITION.coordinationDividend,
    modules: AIB,
    related: ['allied_coverage', 'aib_treatment'],
  },
  {
    id: 'remaining_gap',
    term: 'Remaining supply gap',
    short: AIB_MEASURE_DEFINITION.remainingGap,
    modules: AIB,
    related: ['allied_coverage', 'scaling_host', 'development_host'],
  },
  {
    id: 'workstream',
    term: 'Workstream',
    short: 'A Sectoral Compact for one supply problem: a group of related supply-chain steps and the partners needed to solve it under the selected scenario.',
    more: 'Each workstream is filed under the Strategic Sectoral Compact of its value chain, and can touch others. Its partners can change with the stress test; the strategic compact does not.',
    modules: AIB,
    related: ['sectoral_compact', 'strategic_compact', 'minimum_coalition', 'compact_tiers'],
  },
  {
    id: 'compact_tiers',
    term: 'Importance to the compact',
    short: 'How much a compact depends on each country, from the core members it is built around to the partners it could do without and the countries outside it that it still relies on.',
    more: 'Inside the compact: core coordinating members (central to making it work), critical supply partners (no single other country could replace them) and associate partners (useful, but not required to meet the target). Prospective and development hosts are included for the new capacity they could host. Outside it: critical external dependencies (their loss would push supply below the target) and external dependencies (they supply a material share).',
    modules: AIB,
    related: ['minimum_coalition', 'workstream', 'demand_anchor'],
  },
  {
    id: 'minimum_coalition',
    term: 'Minimum coalition',
    short: AIB_MEASURE_DEFINITION.minimumCoalition,
    more: 'Found exactly where possible (the fewest economies that bring every step to the resilience target, the demand anchors always in); a one-at-a-time search is shown beside it and can need more, or other, economies.',
    modules: AIB,
    related: ['sectoral_compact', 'demand_anchor', 'n1_resilience'],
  },
  {
    id: 'n1_resilience',
    term: 'N-1 resilience',
    short: AIB_MEASURE_DEFINITION.n1Resilience,
    more: 'Specialization is valuable until it creates a new single point of failure, so the analysis tests how much allied demand can still be met if the largest remaining supplier is lost.',
    modules: AIB,
    related: ['minimum_coalition', 'concentration_cap'],
  },
  {
    id: 'concentration_cap',
    term: 'Concentration cap',
    short: AIB_MEASURE_DEFINITION.concentrationCap,
    more: 'The cap is the share that would bring the step to the concentration of comparable goods; no supplier is banned.',
    modules: AIB,
    related: ['managed_interdependence', 'n1_resilience'],
  },
  {
    id: 'demand_anchor',
    term: 'Demand anchor',
    short: 'A major buyer whose market gives a compact scale and helps make new investment bankable.',
    modules: AIB,
    related: ['sectoral_compact', 'minimum_coalition'],
  },
  {
    id: 'scaling_host',
    term: 'Scaling host',
    short: 'An established producer that could add meaningful new capacity at scale.',
    modules: AIB,
    related: ['specialty_host', 'development_host', 'remaining_gap'],
  },
  {
    id: 'specialty_host',
    term: 'Specialty host',
    short: 'A smaller high-income specialist with strong capability in a narrow part of the value chain.',
    modules: AIB,
    related: ['scaling_host', 'development_host'],
  },
  {
    id: 'development_host',
    term: 'Development host',
    short: 'A developing-economy partner with potential to build new supply over time, but not yet counted as existing resilient capacity.',
    modules: AIB,
    related: ['scaling_host', 'specialty_host', 'dp'],
  },
];

/** Fill {{placeholders}} (unknown keys are left as written, so a test can catch them). */
export function fillGlossaryText(text: string, vars: GlossaryVars = {}): string {
  return text.replace(/\{\{(\w+)\}\}/g, (m, k: string) => {
    const v = (vars as Record<string, string | undefined>)[k] ?? (GLOSSARY_DEFAULTS as Record<string, string>)[k];
    return v ?? m;
  });
}

/** The entries with their placeholders filled. */
export function glossaryWith(vars: GlossaryVars = {}): GlossaryEntry[] {
  return GLOSSARY_TEMPLATES.map((e) => ({ ...e, short: fillGlossaryText(e.short, vars), ...(e.more ? { more: fillGlossaryText(e.more, vars) } : {}) }));
}

/** The glossary at the defaults. */
export const GLOSSARY: readonly GlossaryEntry[] = glossaryWith();

export const glossaryById: ReadonlyMap<string, GlossaryEntry> = new Map(GLOSSARY.map((e) => [e.id, e]));

/** The ids, for typed references (`<Term id="es" />`). */
export type GlossaryId = (typeof GLOSSARY_TEMPLATES)[number]['id'];

/** The entries of a module (alphabetical by term), or all. */
export function glossaryFor(module: GlossaryModule | 'all', entries: readonly GlossaryEntry[] = GLOSSARY): GlossaryEntry[] {
  return entries.filter((e) => module === 'all' || e.modules.includes(module)).sort((a, b) => a.term.localeCompare(b.term, 'en'));
}

/** Words in a text (for the 35-word limit of `short`). */
export function wordCount(text: string): number {
  return text.split(/\s+/).filter((w) => /[\p{L}\p{N}]/u.test(w)).length;
}
