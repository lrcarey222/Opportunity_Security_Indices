// The shared vocabulary of the Explorer (the NEIS copy refresh, 2026-10-30; the editorial guide
// NEIS_dashboard_copy_redraft.txt and the user's instructions). One source for every user-facing word that more
// than one page uses: the product's framing and the canonical Allied Industrial Base explainer, the plain
// replacements for model vocabulary, the six choices as actions, the three yards and fences, the five steps of
// the strategy (Need, Prize, Position, Choice, Policy alignment), policy-alignment and Secure-rationale labels,
// the AIB treatments, scenarios, chart labels, compact roles and partnership measures, and the empty, error and
// warning states. Pure data: no React, no IO, no runtime import (type-only imports are erased).
//
// HOW PAGE CODE USES IT
// - Import the constant; never retype the sentence. If a page needs a variant, add it here (with a comment
//   naming where it is used) rather than writing it inline, so the words stay the same on every page.
// - Lead with the strategic question (MODULE_QUESTION, the *_QUESTION constants), then what it means, then the
//   method. Technical detail (score definitions, thresholds, data provenance, caveats) goes in tooltips
//   (InfoTip / Term with the glossary), drawers, Evidence & Methodology and advanced views, never in a lede.
// - Say "supply-chain step" or "capability" in visible copy (TERMS.node); keep "node" for methods and data views.
// - The six choice NAMES do not change (CHOICE_NAME: Secure, Preserve, Catch Up, Partner, Accelerate, Watch);
//   explain each as an action (CHOICE_ACTION, CHOICE_DEFINITION) and use CHOICE_PROSE in generated sentences
//   ("rely on partners for", "leave open"), not the bare name.
// - Explain the yards with YARD_DEFINITION (the user's wording). A technology is never "in" one yard: its
//   steps and their layers can sit in different yards (MIXED_YARD_NOTE).
// - Data limitations are uncertainty, never a strategic conclusion: use STATE (not assessed, not enough
//   evidence), never Watch or "low priority", for missing data.
// - posture/grammar.ts (choice, fence, alignment and rationale labels) and posture/yards.ts (yard names) read
//   their words from here, so the Country, Sector and AIB pages already show them.
// The term list and the rules are summarised for page authors in content/VOCABULARY.md.

import type { AibClass, AibScenarioId } from '../aib/contract.ts';
import type { SecurityRationale, Verb, Yard } from '../posture/contract.ts';

// ---- the product and its framing ---------------------------------------------------------------------------

export const PRODUCT_NAME = 'Allied Industrial Base Explorer';
export const PRODUCT_EYEBROW = 'NEIS Center · Allied Industrial Base Explorer';
export const PAPER_TITLE = 'Opportunity, Security, and the Allied Industrial Base';

/** The four questions the whole Explorer answers (guide: purpose). */
export const EXPLORER_QUESTIONS = [
  'What actually matters?',
  'What should a country build, secure, deploy, or leave open?',
  'Where should allies rely on one another rather than duplicate the whole stack?',
  'What trade, investment, finance, demand, or security action would make that division of labor work?',
] as const;

/** The framing lines every page may reuse. */
export const FRAMING = {
  /** the anti-autarky line (user) */
  noCountryMakesEverything: 'No country needs to make everything.',
  /** the Country view's line (user) */
  whatNotToBuild: 'A strong industrial strategy is as much about deciding what not to build as deciding what to build.',
  /** dependence is not bad by default (guide rule 13) */
  dependenceIsNotBadByDefault: 'The aim is to tell dangerous dependence from useful specialization.',
  /** domestic production is not always the goal (guide rule 14) */
  partnerIsAStrategy: 'Relying on trusted partners, or on open and diversified supply, is a legitimate strategic choice, not a failure to build at home.',
  /** what the strategy identifies (user) */
  whatTheStrategyFinds:
    'It identifies which capabilities need domestic or trusted control, which strengths are worth preserving, where building is plausible, where allies are better placed, where diversified global sourcing is enough, and where the allied system lacks capacity or redundancy.',
} as const;

/** The canonical NEIS narrative, in four lengths (guide section 13). */
export const NEIS_NARRATIVE = {
  long: 'Energy security does not require every country to make everything. It requires being precise about which dependencies are dangerous, which capabilities create lasting economic value, and where trusted interdependence is an advantage. The Explorer first identifies what a country should secure, build, preserve, partner for or deploy. It then asks how allies can divide those capabilities across a network: using trade where capacity already exists, investment where it does not, and stronger safeguards only where concentration creates genuine risk.',
  aib: 'The Allied Industrial Base turns national priorities into a division of labor among trusted partners. It identifies where specialization is efficient, where redundancy is necessary, and where governments need to align trade, finance and demand to make the bargain credible.',
  compacts:
    'Sectoral Compacts are the delivery mechanism. They bring together the smallest useful group of countries around a specific bottleneck and define what each partner contributes, what each receives, and what action closes the remaining gap.',
  oneLine: 'Secure what cannot fail; build where capability matters; specialize with trusted partners everywhere else.',
} as const;

/** The canonical Allied Industrial Base explainer (guide), long and short, and the outcome it aims at. */
export const AIB_EXPLAINER =
  'The Allied Industrial Base is a strategy for managed interdependence. No country should try to make everything. The goal is to identify the capabilities that must be secure, decide which trusted partners are best placed to provide them, and align trade, investment, finance and demand so that specialization produces both resilience and scale.';
export const AIB_EXPLAINER_SHORT =
  'Build what matters. Specialize where allies are strongest. Keep enough redundancy that no single supplier can hold the system at risk.';
export const AIB_OUTCOME =
  'The aim is an allied network that is harder to coerce, cheaper than everyone duplicating everything, and strong enough to compete at scale.';

/** The two sides of the allied division of labor (user): make both explicit wherever a page shows allied supply. */
export const AIB_SIDES = {
  supply: {
    question: 'Who has the strongest capability?',
    words: 'The best-placed producers: who already supplies the step, and who could add trusted capacity.',
  },
  demand: {
    question: 'Which countries need a partner?',
    words: 'Partner demand: the countries whose strategy relies on trusted partners for the step.',
  },
} as const;

/** The AIB's working vocabulary (user): use these words for the allied system, not model terms. */
export const AIB_LANGUAGE = [
  'division of labor',
  'trusted capacity',
  'allied specialization',
  'shared resilience',
  'best-placed producers',
  'critical gaps',
  'single points of failure',
  'redundancy',
  'partner demand',
  'co-investment',
  'offtake',
  'standards',
  'export finance',
  'joint capacity building',
] as const;

// ---- the strategic question of each page (shown first, before any method) ------------------------------------

export const MODULE_QUESTION = {
  home: 'Where should countries build, partner and rely on allies?',
  sector: 'Where are the real bottlenecks and opportunities?',
  sectorLanding: 'What matters in this supply chain?',
  sectorProfile: 'Who is best placed to do this, and what makes the step strategic?',
  concentration: 'Where are the chokepoints?',
  country: 'What should this country secure, build, partner for or deploy?',
  aib: 'How should allies divide the work?',
  coverage: 'Who can cover each supply-chain step?',
  coordination: 'Where do allies need to act together?',
  partnerships: 'Who should work with whom, and for what?',
  compacts: 'Which bottlenecks justify a Sectoral Compact?',
  roles: 'What should each country contribute?',
  about: 'How the Explorer turns evidence into strategic choices',
  methods: 'How the Explorer moves from evidence to a strategic choice',
  glossary: "What the Explorer's words mean",
} as const;

// ---- plain words for model terms (guide: GLOBAL TERMINOLOGY CHANGES; the user's examples) --------------------

/**
 * The user-facing phrase for each technical term. Visible copy uses the plain phrase; the technical word may
 * stay as a short chart label, in a tooltip or on a methods page.
 */
export const TERMS = {
  node: 'supply-chain step',
  nodes: 'supply-chain steps',
  capability: 'capability',
  nodeClass: 'what this step needs',
  coverage: 'share of allied demand that can be met',
  coverageShort: 'coverage',
  staticCoverage: 'supply that remains available without a new response',
  existingResponseCoverage: 'supply that existing partners can replace',
  optimizedCoverage: 'supply that allies could replace if they coordinate',
  residualGap: 'remaining supply gap',
  shock: 'stress test',
  shockScenario: 'disruption scenario',
  shockExposure: 'supply exposed to the disruption',
  candidatePartner: 'potential partner',
  candidatePartners: 'best-placed partners',
  candidateSupplier: 'potential supplier',
  demandScope: 'countries whose supply needs to be replaced',
  investmentHost: 'country that could add capacity',
  marginalInvestment: 'where new capacity would help most',
  frontier: 'trade-off between resilience and concentration',
  coordinationDividend: 'extra supply allies can recover by acting together',
  minimumCoalition: 'smallest group of partners that meets the resilience target',
  coalitionTier: 'role in the compact',
  candidateCompact: 'potential Sectoral Compact',
  compactUnnecessary: 'no compact needed',
  fragmentedAction: "today's uncoordinated response",
  managedSpecialization: 'specialization with safeguards',
  alliedDemandAffected: 'demand that must find a new supplier',
  restoredByTrade: 'recovered through allied trade',
  leftForNewCapacity: 'gap that requires new capacity',
  policyNeed: 'action needed',
  supplyAssumption: 'assumption about how much suppliers can expand',
  physicalCapabilityWarning: 'production evidence is weak',
  // the user's examples (2026-10-30)
  coalitionCoverage: 'where allies are strong',
  residualUncoveredNodes: 'remaining allied gaps',
  roleAllocation: 'who should build what',
  highStrategicNeed: 'supply-chain vulnerability',
  highPercentile: 'strong competitive position',
  posture: 'what this country should do',
  postureClassification: 'What should this country do?',
} as const satisfies Record<string, string>;

/** The terminology table (technical -> plain) for VOCABULARY.md, reviews and tests. */
export const TERMINOLOGY: readonly { technical: string; plain: string }[] = [
  { technical: 'node', plain: TERMS.node },
  { technical: 'node class', plain: TERMS.nodeClass },
  { technical: 'coverage', plain: TERMS.coverage },
  { technical: 'static coverage', plain: TERMS.staticCoverage },
  {
    technical: 'existing-response coverage',
    plain: TERMS.existingResponseCoverage,
  },
  { technical: 'optimized coverage', plain: TERMS.optimizedCoverage },
  { technical: 'residual gap', plain: TERMS.residualGap },
  { technical: 'shock', plain: `${TERMS.shock} / ${TERMS.shockScenario}` },
  { technical: 'shock exposure', plain: TERMS.shockExposure },
  {
    technical: 'candidate partner(s)',
    plain: `${TERMS.candidatePartner}; ranked: ${TERMS.candidatePartners}`,
  },
  { technical: 'demand scope', plain: TERMS.demandScope },
  { technical: 'investment host', plain: TERMS.investmentHost },
  { technical: 'marginal investment', plain: TERMS.marginalInvestment },
  { technical: 'frontier', plain: TERMS.frontier },
  { technical: 'coordination dividend', plain: TERMS.coordinationDividend },
  { technical: 'minimum coalition', plain: TERMS.minimumCoalition },
  { technical: 'coalition tier', plain: TERMS.coalitionTier },
  { technical: 'candidate compact', plain: TERMS.candidateCompact },
  { technical: 'compact unnecessary', plain: TERMS.compactUnnecessary },
  { technical: 'fragmented action', plain: TERMS.fragmentedAction },
  { technical: 'managed specialization', plain: TERMS.managedSpecialization },
  { technical: 'allied demand affected', plain: TERMS.alliedDemandAffected },
  { technical: 'restored by trade', plain: TERMS.restoredByTrade },
  { technical: 'left for new capacity', plain: TERMS.leftForNewCapacity },
  { technical: 'policy need', plain: TERMS.policyNeed },
  { technical: 'supply assumption', plain: TERMS.supplyAssumption },
  {
    technical: 'physical-capability warning',
    plain: TERMS.physicalCapabilityWarning,
  },
  { technical: 'coalition coverage', plain: TERMS.coalitionCoverage },
  {
    technical: 'residual uncovered nodes',
    plain: TERMS.residualUncoveredNodes,
  },
  { technical: 'role allocation', plain: TERMS.roleAllocation },
  { technical: 'high strategic-need score', plain: TERMS.highStrategicNeed },
  { technical: 'unexplained percentile', plain: TERMS.highPercentile },
  {
    technical: 'strategic posture classification',
    plain: TERMS.postureClassification,
  },
];

/** Words visible copy avoids (guide; user): jargon and consulting language. Reviews and tests scan for them. */
export const AVOID_WORDS = ['optimize', 'optimise', 'leverage', 'unlock', 'the model names', 'synergy', 'best-in-class', 'game-changing'] as const;
export const AVOID_RE =
  /\b(optimi[sz]e[sd]?|optimi[sz]ing|leverag(?:e|es|ed|ing)|unlock(?:s|ed|ing)?|synerg(?:y|ies)|best-in-class|game-changing)\b|the model names/i;

// ---- the six choices -------------------------------------------------------------------------------------------

/** The choice names (unchanged; posture/grammar.ts VERB_LABEL reads them). */
export const CHOICE_NAME: Record<Verb, string> = {
  secure: 'Secure',
  preserve: 'Preserve',
  catch_up: 'Catch Up',
  partner: 'Partner',
  accelerate: 'Accelerate',
  watch: 'Watch',
};

/** Each choice as an action, in a few words (subtitles, legends, counts lines; grammar.ts VERB_SUBTITLE). */
export const CHOICE_ACTION: Record<Verb, string> = {
  secure: 'Protect what cannot be allowed to fail',
  preserve: 'Keep and strengthen an existing strength',
  catch_up: 'Build from a credible industrial base',
  partner: 'Rely on trusted partners or diversified supply',
  accelerate: 'Deploy faster at home',
  watch: 'Keep open: buy competitively, do not force localization',
};

/** Each choice in one or two plain sentences (glossary, legends, drawers). */
export const CHOICE_DEFINITION: Record<Verb, string> = {
  secure:
    'Protect an acute vulnerability. The answer may be domestic capacity, trusted allied supply, stockpiles, substitution or redundancy, not necessarily self-sufficiency.',
  preserve: 'Keep and strengthen a capability the country already does well and that still has strategic value.',
  catch_up:
    'Build capability where the strategic prize is high and the country has a credible base to build from. Catch Up means there is something real to build on, not a greenfield ambition.',
  partner:
    'The capability matters, but duplicating it at home is not the best answer. Rely on trusted partners or diversified supply, and make that reliance durable.',
  accelerate: 'Deploy a technology faster because the domestic energy system needs more of it. It says nothing about making the equipment at home.',
  watch: 'Do not make domestic production a major policy priority. Keep access affordable and diversified, and watch for changes in risk or opportunity.',
};

/** The verb phrase of a choice in a generated sentence ("Japan should preserve ...; rely on partners for ..."). */
export const CHOICE_PROSE: Record<Verb, string> = {
  secure: 'secure',
  preserve: 'preserve',
  catch_up: 'catch up in',
  partner: 'rely on partners for',
  accelerate: 'deploy faster',
  watch: 'leave open',
};

/** Watch in prose, where the name alone would read as "do nothing" (guide: "Low domestic priority"). */
export const WATCH_PROSE_LABEL = 'Low domestic priority';
/** Watch at Deployment & Use: the deployment-need score is below the bar for acceleration (not "on track"). */
export const WATCH_DEPLOYMENT_DEFINITION =
  'Deployment need is below the bar for acceleration: the current demand, target and system evidence do not call for a major push.';
/** Partner, as the user names the choice where diversification is the answer. */
export const PARTNER_LONG_NAME = 'Partner / Diversify';

// ---- the yards and fences (the user's mapping, 2026-10-30) ---------------------------------------------------

export type FenceKey = 'high' | 'medium' | 'low';

export const YARD_NAME: Record<Yard, string> = {
  small: 'Small yard',
  medium: 'Medium yard',
  large: 'Large yard',
};
export const FENCE_NAME: Record<FenceKey, 'High fence' | 'Medium fence' | 'Low fence'> = { high: 'High fence', medium: 'Medium fence', low: 'Low fence' };
/** A yard with its fence, as the user names them. */
export const YARD_AND_FENCE: Record<Yard, string> = {
  small: 'Small yard / High fence',
  medium: 'Medium yard / medium, permeable fence',
  large: 'Large yard / Low fence',
};
/** The three yards in the user's words: use these wherever the yards are explained. */
export const YARD_DEFINITION: Record<Yard, string> = {
  small: 'Capabilities where foreign control or concentrated dependence could create an unacceptable security or resilience risk.',
  medium: 'Capabilities worth building or preserving because they shape competitiveness, industrial learning or future technological leadership.',
  large: 'Capabilities where affordable and diversified access generally matters more than domestic production.',
};
/** The choices behind each fence. */
export const YARD_CHOICES: Record<Yard, string> = {
  small: 'Secure',
  medium: 'Preserve, Catch Up, Partner, and Accelerate at Deployment & Use',
  large: 'Watch',
};
/** The section title of each fence on a Country profile. */
export const FENCE_TITLE: Record<FenceKey, string> = {
  high: 'High fence · Secure',
  medium: 'Medium fence · Build, preserve, partner and deploy',
  low: 'Low fence · Keep open',
};
/** Each fence in one line (guide). */
export const FENCE_DEFINITION: Record<FenceKey, string> = {
  high: 'Protect the capabilities where dependence could create an unacceptable security or resilience risk.',
  medium: 'Focus industrial policy where productive capability has strategic value, and use allies where specialization is the better answer.',
  low: 'Do not spend scarce industrial-policy effort where domestic production adds little strategic value.',
};
/** The opening paragraph of each fence (guide). */
export const FENCE_INTRO: Record<FenceKey, string> = {
  high: 'High-fence capabilities are deliberately narrow: control systems, genuine chokepoints and other supply-chain steps where disruption would be hard to absorb quickly. The answer is not always reshoring: trusted allied supply, stockpiles, substitution and redundancy can also solve the problem.',
  medium:
    'The medium fence is about competitiveness, learning and future capability. A country may preserve an existing strength, catch up where it has a credible base, rely on a partner where duplication makes little sense, or accelerate deployment where the energy system needs the technology.',
  low: 'Low-fence capabilities are not unimportant. They are places where affordable, diversified access generally matters more than domestic production. The right strategy may be to buy competitively, keep suppliers diversified and focus policy elsewhere.',
};
/** A technology is never in one yard: its steps and their layers can differ. */
export const MIXED_YARD_NOTE =
  'A technology can sit in more than one yard. Its supply-chain steps, and the layers within a step (control software, firmware, a critical input), can each need a different fence.';
/** Control layers are country-specific (user decision, 2026-10-30). */
export const CONTROL_LAYER_NOTE = 'A control layer is in the Small yard only where the data show coercive foreign control for this country.';
/** The yards are different problems, not a score scale. */
export const YARDS_NOT_A_SCALE = 'The three yards are different policy problems, not low, medium and high scores on one scale.';

// ---- the strategy: Need -> Prize -> Position -> Choice -> Policy alignment -----------------------------------

/** The canonical strategy explainer (guide, Methods). */
export const STRATEGY_EXPLAINER =
  'The strategy starts with evidence, not policy. Need asks whether dependence or deployment creates a real problem. Prize asks whether productive capability has strategic industrial value. Position asks how strong the country already is. Those three determine the choice. Only then do we compare the recommendation with what government is actually doing.';

export type StrategyStep = 'need' | 'prize' | 'position' | 'choice' | 'policy';
export const STRATEGY_STEPS: readonly StrategyStep[] = ['need', 'prize', 'position', 'choice', 'policy'];
/** Each step: its short name, the measure's name, the question it answers and what it means in plain words. */
export const STRATEGY_STEP: Record<StrategyStep, { name: string; measure: string; question: string; plain: string }> = {
  need: {
    name: 'Need',
    measure: 'Strategic Need',
    question: 'What problem has to be solved?',
    plain:
      'At producing stages, Need means supply-chain vulnerability. At Deployment & Use, it means how urgently the domestic energy system needs more deployment.',
  },
  prize: {
    name: 'Prize',
    measure: 'Strategic Industrial Value',
    question: 'Is productive capability here strategically valuable?',
    plain: 'Market growth matters, but so do process learning, supplier ecosystems and future leadership.',
  },
  position: {
    name: 'Position',
    measure: 'Competitive Position',
    question: 'How much real capability does the country have today?',
    plain: 'Exports and specialization, production, capacity, investment, costs and resources where they apply.',
  },
  choice: {
    name: 'Choice',
    measure: 'Strategic choice',
    question: 'What should the country do?',
    plain: 'Secure, Preserve, Catch Up, Partner, Accelerate or Watch (keep open).',
  },
  policy: {
    name: 'Policy alignment',
    measure: 'Policy alignment',
    question: 'Is policy focused on the right places?',
    plain: 'Is government action matched to the problem, or using the wrong amount or type of intervention?',
  },
};

/** The measures in plain words, for KPI labels and axis titles (with the formal name in a tooltip). */
export const MEASURE_PLAIN = {
  supplyChainNeed: 'Supply-chain vulnerability',
  deploymentNeed: 'Deployment need',
  prize: 'Strategic Industrial Value',
  prizeShort: 'Prize',
  marketPrize: 'Market prize (Industrial Opportunity)',
  competitivePosition: 'Competitive Position',
  strongPosition: 'strong competitive position',
} as const;

/** One-line definitions of the measures (guide). */
export const MEASURE_DEFINITION = {
  supplyChainNeed:
    'How vulnerable a country is at a producing-stage capability: import dependence, supplier concentration, exposure to risky suppliers and the availability of trusted alternatives.',
  deploymentNeed: 'How urgently a country needs more of a technology in use, based on expected demand, national targets and energy-system requirements.',
  prize:
    'How much strategic value there is in building or keeping productive capability here. It combines the size of the market with learning, supplier ecosystems and future leadership potential, not just market growth.',
  competitivePosition: 'How strong the country is today: exports and specialization, production, capacity, investment, costs and resources where they apply.',
} as const;

// ---- policy alignment and the Secure rationales -----------------------------------------------------------------

export type AlignmentKey = 'aligned' | 'under_prioritized' | 'over_prioritized' | 'misaligned' | 'mixed' | 'insufficient_data';
/** The alignment classes in plain words (guide; grammar.ts ALIGNMENT_CLASS_LABEL). */
export const ALIGNMENT_NAME: Record<AlignmentKey, string> = {
  aligned: 'Broadly aligned',
  under_prioritized: 'Needs more attention',
  over_prioritized: 'Too much intervention',
  misaligned: 'Wrong tool for the problem',
  mixed: 'Mixed signals',
  insufficient_data: 'Not enough policy evidence',
};
/** A deployment gap: Accelerate met by weak deployment policy (shown under Needs more attention). */
export const ALIGNMENT_DEPLOYMENT_GAP = 'Needs more attention: deployment policy lags';
/** The section's question and opening line (guide). */
export const POLICY_ALIGNMENT_TITLE = 'Is policy focused on the right places?';
export const POLICY_ALIGNMENT_INTRO =
  'The strategy is derived without using current policy. This section then asks whether government action matches the recommended choice, or is spending effort in the wrong place.';
/** The guide's example sentences, by the case they describe. */
export const ALIGNMENT_EXAMPLE = {
  secureLowResponse: 'This vulnerability is not receiving enough policy attention.',
  watchHeavyProtection: 'Policy is trying to localize a capability the strategy does not treat as a domestic priority.',
  partnerLocalization: 'The need is real, but policy is trying to solve it through domestic duplication rather than trusted specialization.',
  accelerateWeakPolicy: 'The energy system needs faster deployment than current policy appears to support.',
} as const;

/** Why a step is Secure, in plain words (guide; grammar.ts RATIONALE_LABEL). */
export const RATIONALE_NAME: Record<SecurityRationale, string> = {
  operational_control: 'Control-system risk',
  coercive_chokepoint: 'Chokepoint risk',
  systemic_resilience: 'System resilience',
};

// ---- the Allied Industrial Base -------------------------------------------------------------------------------

/** What each supply-chain step needs from allies: the AIB treatments in plain words (the data's codes as keys). */
export const AIB_CLASS_NAME: Record<AibClass, string> = {
  COORDINATE: 'Coordinate trade',
  BUILD: 'Build capacity',
  'MANAGE SPECIALIZATION': 'Manage specialization',
  'LOW INTERVENTION': 'Low intervention',
  'INSUFFICIENT DATA': 'Not enough evidence',
};
export const AIB_CLASS_DEFINITION: Record<AibClass, string> = {
  COORDINATE: 'Existing allied capacity is enough, but countries need new or expanded trade relationships to use it.',
  BUILD: 'Even the best allied trade response leaves a material gap. New production or processing capacity is required.',
  'MANAGE SPECIALIZATION':
    'Specialization creates economic value, but too much supply sits with one supplier. Keep the specialization while adding enough redundancy to reduce the risk.',
  'LOW INTERVENTION': 'The existing network is already resilient enough that major coordination is unlikely to add much.',
  'INSUFFICIENT DATA': 'The data are too thin to make a confident recommendation.',
};
export const AIB_CLASS_HEADING = 'What each supply-chain step needs';

/** The scenarios in plain words (the data's ids as keys). Decoupling names China in this build's data. */
export const SCENARIO_NAME: Record<AibScenarioId, string> = {
  baseline: "Today's network",
  managed: 'Managed Interdependence',
  decoupling: 'Major-supplier disruption',
};
/** The method name of a scenario where a methods page needs it. */
export const SCENARIO_METHOD_NAME: Record<AibScenarioId, string> = {
  baseline: 'Baseline',
  managed: 'Managed Interdependence',
  decoupling: 'Decoupling stress test',
};
export const SCENARIO_DEFINITION: Record<AibScenarioId, string> = {
  baseline: 'The suppliers and trade relationships that exist today. No disruption is imposed.',
  managed:
    'No supplier is banned. Where a supply-chain step is unusually concentrated, the analysis caps how much of allied demand any one supplier can serve and asks which trusted suppliers would need to take the excess.',
  decoupling:
    'A major supplier (China, in this analysis) stops supplying allied markets. The analysis asks what existing partners can replace, what coordinated trade adds, and what still requires new capacity.',
};
export const SCENARIO_SECTION_TITLE = 'How resilient is the network under different assumptions?';
/** Managed Interdependence, the core AIB question and its alternative to both extremes (guide). */
export const MANAGED_INTERDEPENDENCE = {
  question: 'This is the core AIB question: how much specialization can allies keep while reducing the dependencies that create coercive or systemic risk?',
  explainer:
    "Managed Interdependence is the alternative to both laissez-faire concentration and blanket decoupling. It keeps the benefits of specialization but sets limits where one supplier's position creates unacceptable risk. The question is not whether dependence can be eliminated, but which dependencies are worth keeping, and what backup is enough.",
} as const;

/** Scenario chart labels (guide). */
export const SCENARIO_CHART_LABEL = {
  alliedDemandAffected: 'Demand that must find a new supplier',
  kept: 'Still supplied',
  restoredByTrade: 'Recovered through allied trade',
  newCapacity: 'Requires new capacity',
  coverage: 'How much demand the allied network can meet',
  leftForNewCapacity: 'Gap that still needs new capacity',
  whereLostSupplySits: 'Where the remaining vulnerability sits',
} as const;

/** Card titles for the AIB summary (guide). */
export const AIB_SUMMARY_QUESTIONS = [
  "What can today's allies already replace?",
  'What does coordination add?',
  'Where does trade stop being enough?',
  'Where should new capacity be built?',
  'Which countries are indispensable, and where do we need a backup?',
] as const;

/** Allied-system measures in plain words (glossary and tooltips). */
export const AIB_MEASURE_DEFINITION = {
  coverage: 'The share of allied demand that the available supplier network can meet under a given scenario.',
  coordinationDividend: "The extra demand allies can meet when they coordinate trade rather than relying only on today's bilateral relationships.",
  remainingGap: 'Demand that cannot be met even after the best available allied trade response. This is where new capacity may be needed.',
  minimumCoalition: "The smallest group of partners that can meet the compact's resilience target while keeping the main buyer markets inside the agreement.",
  n1Resilience: 'How much demand the network can still meet if its largest remaining supplier is lost.',
  concentrationCap:
    'A limit on how much of allied demand one supplier may serve in the Managed Interdependence scenario. Supply above the cap has to shift to alternatives.',
} as const;

/** Sectoral Compacts (guide). */
export const COMPACT = {
  definition:
    'A Sectoral Compact is a practical bargain among the countries needed to solve a shared supply-chain problem. It identifies the bottleneck, the smallest useful group of partners, the role each partner should play, and the mix of trade, investment and specialization needed to close the gap.',
  notAClub:
    'A compact is not a club of every friendly country. It is a targeted agreement around a specific value chain where coordination produces a measurable benefit over countries acting alone. It should be no larger than necessary, and every member should have a reason to take part.',
  reciprocity:
    'The point is reciprocal specialization: partners accept some dependence on one another because each gets reliable access, a meaningful economic role, and a more resilient market in return.',
  strategic: 'The longer-run division of labor for a strategically important value chain: which countries supply, buy, invest, finance or host capacity.',
  operational: 'The partnerships needed to solve a specific supply-chain problem under the selected scenario.',
  /** what every compact must answer (guide rule 15) */
  questions: ['Why this bottleneck?', 'Why these partners?', 'What changes if they coordinate?', 'What action follows?'],
  sectionTitles: {
    why: 'Why this compact?',
    what: 'What does the compact cover?',
    together: 'What can the partners solve together?',
    next: 'What should governments do next?',
  },
} as const;

/** The roles a country can hold in a compact (the data's role names as keys; aib/derive/roles.ts ROLE_ORDER). */
export const COMPACT_ROLE_DEFINITION: Readonly<Record<string, string>> = {
  'Demand anchor': 'A major buyer whose demand gives the compact scale and a reason for suppliers to invest.',
  'Specialized supplier': 'Already supplies a material share of what allies need, from a revealed specialization.',
  'Resource supplier': 'Already supplies a material share of what allies need, from its own resources.',
  'Scaling host': 'An established producer that could add meaningful capacity at scale.',
  'Specialty host': 'A smaller high-income specialist with strong capability in a narrow part of the value chain.',
  'Development host': 'A developing-economy partner with potential to build new capacity, not yet counted as existing resilient supply.',
  'Technology partner': 'Brings capability that matters where new capacity is needed or specialization must be managed.',
  'Bridge partner': 'Connects parts of the compact: supplies one step and depends on another, or spans stages or compacts.',
  'Associate partner': 'A useful partner that improves the compact but is not required to meet the core target.',
  Supplier: 'A compact member that supplies part of what allies need.',
};
/** The coalition tiers in plain words (the data's tier names as keys; aib/derive/tiers.ts). */
export const COMPACT_TIER_DEFINITION: Readonly<Record<string, string>> = {
  'core coordinating member': 'A country whose capability or demand is central to making the compact work.',
  'critical supply partner': 'A member no single other country could replace.',
  'prospective host': 'A member included for the new capacity it could host.',
  'associate partner': 'A useful partner that improves the compact but is not required to meet the core target.',
  'critical external dependency': 'A country outside the compact whose loss would push supply below the target.',
  'external dependency': 'A country outside the compact that supplies a material share.',
  'development host': 'A developing-economy partner with potential to build new capacity, not yet counted as existing supply.',
};
/** What a country's roles mean (guide, Country roles). */
export const ROLES_FRAMING = {
  lede: 'A resilient allied system depends on countries doing different things well. This view shows what each economy contributes to the network (supply, demand, technology, capital or new capacity) and where its role is essential rather than interchangeable.',
  key: 'The goal is not to reward the country with the most roles. It is to make specialization explicit enough that partners know what they are relying on one another for.',
  derived:
    'Roles are derived from the trade, capacity and compact results. They describe what a country contributes to a specific value chain, not a permanent geopolitical label.',
} as const;

/** Partnerships (guide, section 7). */
export const PARTNERSHIP = {
  lede: 'A good partnership is not simply a friendly relationship. It should solve a specific strategic problem: reduce a dangerous dependency, reinforce a competitive capability, open a market, or help build capacity where the allied network needs it.',
  heading: 'Which partners are best placed to help?',
  thesisHeading: 'What is the strategic case for this partnership?',
  thesisPrompt:
    'Is the relationship mainly about securing supply, expanding a market, building new capacity, or combining complementary strengths? The answer should determine the tools.',
  psi: 'A summary of where two economies have the strongest case to deepen an energy-industrial relationship across sourcing, markets and investment.',
  psiCaveat: 'Use the component scores to understand the reason. A high partnership score is not a recommendation to cooperate on every technology.',
  friendshore: 'Where country A could source from or invest in country B to reduce a strategic dependency.',
  opportunity: 'Where country B offers an attractive market for capabilities country A is well placed to supply.',
  development: 'Where joint investment could create a new supply base while building productive capacity in a middle- or lower-income partner.',
  ties: 'What already connects the two economies: trade, manufacturing investment, agreements and geopolitical alignment.',
} as const;

// ---- evidence, Legacy OSI and states ----------------------------------------------------------------------------

export const EVIDENCE_TITLE = 'Evidence & methodology';
export const EVIDENCE_INTRO =
  'See the data behind each choice, where evidence is thin, and how the current strategy differs from the earlier Legacy OSI framework.';
export const LEGACY_OSI_NOTE =
  'The Legacy OSI framework is kept for comparison and reproducibility. It describes relative security and opportunity; it is not the current strategic recommendation.';

/** Empty, loading, warning and error states (guide section 11). Missing data is uncertainty, never a strategic call. */
export const STATE = {
  notAssessed: 'Not assessed: the available data are not strong enough to make a strategic call.',
  notAssessedShort: 'Not assessed',
  notEnoughEvidence: 'Not enough evidence',
  noStrategyMap: 'There is not enough comparable evidence to produce a strategy map for this country.',
  aibLoading: 'Loading the Allied Industrial Base results…',
  aibUnavailable: 'Allied Industrial Base results are not available in this build. Country and sector evidence are still available.',
  noAibRole: 'This country is not needed in a modeled compact under the selected scenario.',
  noAibRoleWhy: 'That does not mean it is unimportant; it means the current stress test does not require its capability to close the identified gaps.',
  noManagedResult: 'Managed Interdependence does not change this compact under the current concentration benchmark.',
  compactUnchanged: "No supply needs to shift under this scenario, so the compact keeps today's supplier network.",
  noCoalition: 'No coalition is needed under this scenario: the identified supply problem is already covered or does not meet the compact threshold.',
  weakProductionEvidence: 'Production evidence is weak',
  dataLimitation: 'Missing or thin data are shown as uncertainty, never as a low priority.',
} as const;
