// The shared glossary (spec_country.md C3.2; the content there is normative): a plain-language
// definition of every index, category and acronym, for a reader who knows energy and economics but has
// not read the paper. Since 2026-10-28 every entry reads the six supply-chain stages (Extraction,
// Refining, Components, Assembly, Deployment, Recovery; lib/stages.ts), and the strategic position
// framework's terms (the posture cards' Term ids, posture/basis.tsx POSTURE_TERMS) are here too. Used by InfoTip (the `short` text), Term (the name on first use), GlossaryList
// and the /glossary page (`more`), in all three modules. Pure data and helpers.
//
// Numbers that change with the data are written as {{placeholders}} and filled from GLOSSARY_DEFAULTS,
// or from the values a module's manifest provides (GlossaryVarsProvider in components/glossaryVars.tsx).

export type GlossaryModule = 'sector' | 'strategic' | 'country';

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
 *  2026-10-28: the coalition of 31 core members and 29 partners; 11 headline technologies x 6 stages
 *  = 66 sectors; the Sector pages add Semiconductors' Refining, Components and Assembly: 69 cells). */
export const GLOSSARY_DEFAULTS = {
  n_core: '31',
  n_partner: '29',
  n_codes: '259',
  un_window: '2019-2023',
  n_tech: '11',
  n_sectors: '66',
  n_sector_cells: '69',
} as const;

export type GlossaryVars = Partial<Record<keyof typeof GLOSSARY_DEFAULTS, string>>;

const ALL: GlossaryModule[] = ['sector', 'strategic', 'country'];
const SC: GlossaryModule[] = ['strategic', 'country'];

/** The entries as written, with {{placeholders}}. */
export const GLOSSARY_TEMPLATES: readonly GlossaryEntry[] = [
  // ---- the indices ---------------------------------------------------------------------------
  {
    id: 'es',
    term: 'Energy Security',
    abbr: 'ES',
    short: 'How well a country can rely on what it needs in a sector: own production and reserves, import needs, concentration of world supply, price stability. 0 to 1; higher is more secure.',
    more: 'Built for each technology and supply-chain stage from up to ten kinds of data: production, reserves, the gap between production and consumption, market position, trade balance, how concentrated world exports and critical-mineral trade are, price volatility, energy access, consumption growth and investment. Each measure is ranked against every other economy in the same sector, so 0.8 means doing better than most there. It is a relative score, not a probability of disruption, and it does not measure how many suppliers the country itself buys from.',
    modules: ALL,
    related: ['eo', 'need', 's_curve', 'rankable'],
  },
  {
    id: 'eo',
    term: 'Economic Opportunity',
    abbr: 'EO',
    short: 'How well placed a country is to create value in a sector: exports and market share, related capabilities, investment, costs, demand and technology readiness. 0 to 1; higher is a bigger opening.',
    more: 'Built like Energy Security from ten kinds of data (trade, including export feasibility from the Atlas of Economic Complexity; production; demand; technology readiness; costs; prices; investment; access; consumption; market position) and ranked within the sector. A high score means the country already has, or is close to having, what it takes to compete there.',
    modules: ALL,
    related: ['es', 'feasibility', 's_curve'],
  },
  {
    id: 'si',
    term: 'Strategic Index',
    abbr: 'SI',
    short: "Ranks a country's own sectors by how strongly the data point to prioritising them: opportunity, security need, existing policy activity, the stage and the technology's low-emission value.",
    more: 'Baseline weights: Economic Opportunity 30%, security need (1 minus Energy Security) 30%, policy activity 20%, stage 10%, technology 10%. The stage term favours manufacturing: Components and Assembly 0.375 each, Deployment 0.25, Extraction, Refining and Recovery about 0.167 each. It compares sectors within one country: compare countries by rank, not by level.',
    modules: SC,
    related: ['need', 'policy_index', 'allied_pick', 'priority_no_policy'],
  },
  {
    id: 'need',
    term: 'Security need',
    short: '1 minus Energy Security: how exposed a country is in a sector. The Strategic Index rewards it, so exposed sectors rank higher.',
    more: 'Not the same as Strategic Need on the strategic posture charts, which reads import dependence, supplier concentration and exposure to China and opposite-bloc suppliers.',
    modules: SC,
    related: ['es', 'si', 'strategic_need'],
  },
  {
    id: 'psi',
    term: 'Partnership Strength Index',
    abbr: 'PSI',
    short: 'How strong a partner one economy is for another in energy supply chains: as a friendshoring opportunity (Friendshore), an export market (Partnership Opportunity) and a development partner.',
    more: "Weights 40%, 40% and 20% (Development Potential only for middle- and lower-income partners). Each pair is summarised by its five strongest sectors. A country's profile shows it from that country's viewpoint; the coalition headline averages the viewpoints of the {{n_core}} core coalition members.",
    modules: SC,
    related: ['fs', 'po', 'dp', 'best_five'],
  },
  {
    id: 'fs',
    term: 'Friendshore',
    abbr: 'FS',
    short: 'A friendshoring opportunity: a partner B where country A could invest and source to meet its security need in a sector. B has an economic opportunity there; the two already trade and invest.',
    more: "Components: A's security need in the sector (1 minus its Energy Security), B's Economic Opportunity there (its room to build production and value, not its standing as a supplier today), A's imports from B, A's direct investment in B, UN-vote alignment and 1 minus the Chinese-parent share of B's foreign-owned factories, less a climate penalty for emission-intensive technologies. A's need is the same for every partner in one sector: it sets how much the sector matters to A, while the ranking of partners within the sector turns on B's opportunity, the trade and investment ties, alignment and ownership. It shows where investment and sourcing could go, not today's largest suppliers (the import shares show those).",
    modules: SC,
    related: ['psi', 'alignment', 'investment_ties', 'chinese_parent'],
  },
  {
    id: 'po',
    term: 'Partnership Opportunity',
    abbr: 'PO',
    short: "How good a market partner B is for country A's exports in a sector: A already sells there, A is competitive, and B needs the goods.",
    more: "Components: A's exports to B, A's economic opportunity and B's security need (weights 3, 3, 1), less a climate penalty.",
    modules: SC,
    related: ['psi', 'fs'],
  },
  {
    id: 'dp',
    term: 'Development Potential',
    abbr: 'DP',
    short: 'For middle- and lower-income partners: how much room there is to build supply chains together, from development, investment-climate, aid and energy-use data. Not scored for high-income partners.',
    modules: SC,
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
    modules: ALL,
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
    term: 'NEIS baseline',
    short: "Every dial at 50 and the default settings: the pipeline's own six-stage weights. Every index starts from it; only a custom perspective on a country moves priorities and partner rankings away from it.",
    more: 'The strategic posture (Secure, Compete, Deploy) does not move with the dials.',
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
    short: 'Four sliders behind a country\'s Perspective that re-weight priorities and partner rankings: hawkishness, bullishness, populism and greenness (climate). 50 is the NEIS baseline.',
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
  // ---- Country profile: the four corners and the comparisons ----------------------------------
  // The corners of the Security and opportunity chart (country/contract.ts QUADRANTS), kept and recomputed
  // at the six stages (user, 2026-10-28). Since 2026-10-28 the corners with the keys secure and watch are
  // shown as Exposed and Stable, so they are not read as the posture words (Secure / Compete / Deploy,
  // Catch up / Preserve / Partner, Watch); their glossary ids follow the names.
  {
    id: 'grow',
    term: 'Grow',
    short: "High opportunity, high energy security: build on strength (RMI 2025: export promotion). A corner of the Security and opportunity chart, against the country's median sector; not a strategic posture.",
    more: "A sector exactly at the median counts as high. In the Security vs opportunity comparison, which shows every economy in one sector, high and low are against the sector's median economy instead.",
    modules: SC,
    related: ['upgrade', 'exposed', 'stable', 'quadrant'],
  },
  {
    id: 'upgrade',
    term: 'Upgrade',
    short: "High opportunity, low energy security: building capability could close a gap and create value (RMI 2025: leapfrog). A corner of the Security and opportunity chart, against the country's median sector; not a strategic posture.",
    more: "In the Security vs opportunity comparison, which shows every economy in one sector, high and low are against the sector's median economy instead.",
    modules: SC,
    related: ['grow', 'exposed', 'stable', 'quadrant'],
  },
  {
    id: 'exposed',
    term: 'Exposed',
    short: "Low opportunity, low energy security: lean on trusted partners for supply (RMI 2025: friendshore). A corner of the Security and opportunity chart, against the country's median sector; not the Secure posture.",
    more: "In the Security vs opportunity comparison, which shows every economy in one sector, high and low are against the sector's median economy instead. Called Secure before 28 October 2026, as in the memos written before then.",
    modules: SC,
    related: ['grow', 'upgrade', 'stable', 'quadrant'],
  },
  {
    id: 'stable',
    term: 'Stable',
    short: "Low opportunity, high energy security: little pressure to act; value may lie elsewhere (RMI 2025: alternative value creation). A corner of the Security and opportunity chart, against the country's median sector; not a strategic posture.",
    more: "In the Security vs opportunity comparison, which shows every economy in one sector, high and low are against the sector's median economy instead. Called Watch before 28 October 2026, as in the memos written before then.",
    modules: SC,
    related: ['grow', 'upgrade', 'exposed', 'quadrant'],
  },
  {
    id: 'quadrant',
    term: 'Corner (quadrant)',
    short: "One of the four corners of a profile's Security and opportunity chart. The dashed lines sit at the country's median sector on each axis, so the corners show its relatively strong and weak sectors.",
    more:
      "The median sector is the middle of the country's own sectors on each axis; a sector exactly at it counts as high. The axes still compare the country with other economies: each sector sits at the share of them it beats. The corners are Grow, Upgrade, Exposed and Stable, over the country's sectors at the six stages; they are not the need × opportunity quadrant of the strategic posture charts.",
    modules: ['country'],
    related: ['grow', 'upgrade', 'exposed', 'stable', 'percentile', 'median', 'posture'],
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
  // ---- the strategic position framework (the Strategic posture sections, 2026-10-28) ------------
  // The ids of posture/basis.tsx POSTURE_TERMS. 'policy_alignment' is not UN-vote 'alignment', and
  // 'strategic_need' is not the Strategic Index's security 'need'.
  {
    id: 'posture',
    term: 'Strategic posture',
    short: "The strategic position framework's recommendation for a country at one technology and stage: Secure the node, Compete in it (preserve, catch up or partner) or Deploy openly. Fixed thresholds: the dials do not change it.",
    more: 'Secure comes first and overrides the rest: it needs an acute consequence and one of three routes (see Secure rationale). Elsewhere at the producing stages, Strategic Need and Industrial Opportunity split Compete into catch up (both high), preserve (opportunity only) and partner (need only); neither high is Deploy. Deployment itself stays Deploy unless a control layer is Secure.',
    modules: ALL,
    related: ['strategic_need', 'industrial_opportunity', 'security_rationale', 'competitive_position', 'policy_alignment'],
  },
  {
    id: 'strategic_need',
    term: 'Strategic Need',
    short: 'How exposed a country is if supply of a node is disrupted: its import dependence, how concentrated its suppliers are and its imports from China and opposite-bloc suppliers, ranked against other countries.',
    more: "Each part is a percentile among the countries in the same technology and stage; higher is more need. Deployment reads the technology's Assembly need. Not the same as the Strategic Index's security need (1 minus Energy Security).",
    modules: ALL,
    related: ['posture', 'industrial_opportunity', 'need'],
  },
  {
    id: 'industrial_opportunity',
    term: 'Industrial Opportunity',
    short: "Economic Opportunity placed among a node's contenders, the economies holding at least 0.5% of world supply there: how well placed a country is to build or keep the industry, against those already in it.",
    more: 'With fewer than five contenders it is placed among the coalition members and the large economies instead. It is a percentile across countries, never within one country; coal, a declining market, is capped below the thresholds.',
    modules: ALL,
    related: ['posture', 'strategic_need', 'eo'],
  },
  {
    id: 'competitive_position',
    term: 'Competitive Position',
    short: 'How strong a country already is in a node, against every other country: its export share and specialization, production share and manufacturing plants, with trade momentum. A consistency check on the posture, never an override.',
    more: 'Its trade evidence is a 2022-24 benchmark of exports, world share and revealed comparative advantage; current trade momentum takes 15% of the trade evidence where a matched 12-month comparison exists.',
    modules: ALL,
    related: ['posture', 'trade_momentum', 'rca'],
  },
  {
    id: 'security_rationale',
    term: 'Secure rationale',
    short: 'Why a node is Secure: operational control (a foreign actor could command or disable it), a coercive chokepoint (a coercive supplier could withhold it) or systemic resilience (essential, import-dependent and slow to rebuild).',
    more: 'Each route also needs an acute consequence. Concentration alone does not make a chokepoint: substitution must be hard and trusted supply too slow to restore. Secure does not mean reshoring.',
    modules: ALL,
    related: ['posture'],
  },
  {
    id: 'policy_alignment',
    term: 'Policy alignment',
    short: 'Observed policy against the recommended posture: aligned, under-prioritized (Secure or Compete with little policy effort), over-prioritized (heavy support where Deploy is recommended) or mixed. Observed policy never enters the recommendation.',
    more: "Policy effort is the country's revealed policy priority, compared among its own nodes. Not the same as UN-vote alignment.",
    modules: ALL,
    related: ['posture', 'policy_index'],
  },
  {
    id: 'partner_gap',
    term: 'Partner gap',
    short: "A node where a country's posture is to partner with trusted suppliers, but no trusted supplier holds at least 2% of world supply.",
    more: "Elsewhere a Partner node names up to three trusted partners, ranked on capability, relationship strength and how much they would diversify the country's supply.",
    modules: ALL,
    related: ['posture', 'aib_gap'],
  },
  {
    id: 'trade_momentum',
    term: 'Trade momentum',
    short: "Whether a country's exports in a node are rising, flat or falling: the latest 12 months against the 12 before, in growth and in the basket's share of its exports, where monthly reporting is complete.",
    more: "Ranked against the node's other reporters. A reporter without the 24 matched months has no momentum (never zero).",
    modules: ALL,
    related: ['competitive_position'],
  },
  {
    id: 'aib_gap',
    term: 'Allied gap',
    short: 'Where trusted allied supply of a node is thin: no trusted supplier, a single point of failure (one holder with half of allied supply), fewer than three holders, or weak evidence.',
    more: 'From the allied division of labour: per node the primary allied holder, two backups and the number of allied holders with at least 2% of supply. Deployment is left out: no allied supply node sits there.',
    modules: ALL,
    related: ['partner_demand', 'partner_gap'],
  },
  {
    id: 'partner_demand',
    term: 'Partner demand',
    short: "How many countries' postures ask trusted partners for a node: those whose posture there is Compete, partner. Shown beside the allied gap, so a gap many countries rely on stands out.",
    modules: ALL,
    related: ['aib_gap', 'posture'],
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
