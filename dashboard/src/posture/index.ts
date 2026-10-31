// The posture module's public surface for the pages that mount it (Country, Sector, AIB). posture/
// imports nothing from country/, sector/ or aib/; the pages wrap these cards in their own Sections (with
// PostureBasisTag as the Section's basis) and pass data and links in as props.
//
// The Country page (design section 4, 5b; the fences of 2026-10-30) builds its sections from these pieces:
// the strategy sentence (strategySentence) for the header; CountryStrategyMap (Strategy, #strategy); the
// fence groups, rankings and variables (fences.ts) for the High, Medium and Low fence sections, with
// PrizePositionScatter in the Medium fence; PolicyAlignmentTable (#policy-alignment); one PostureNodeDrawer
// for every entry point, driven by useNodeSelection (?node=Tech|Stage, design D8).
// NeedPanels, StrategicChoices and AlliedStrategy are the committed page's Need, Choices and Allied Strategy
// sections (e82f631), which the uncommitted fence sections replace (2026-10-30). No page mounts them now; they
// are kept, with their tests, only until the user settles the fence design, so the page can go back without a
// rewrite (unmounted, they add nothing to a bundle). Delete them, with their tests in sections.test.tsx, once it is settled.

export { AibDivisionCard, AIB_DIVISION_TITLE, indexNodesByCell, type AibDivisionCardProps, type PostureLinkedNode } from './AibDivisionCard.tsx';
export { ALLIED_STRATEGY_TITLE, AlliedStrategy, alliedStatusWords, EXPLORE_PARTNERSHIPS, type AlliedStrategyProps } from './AlliedStrategy.tsx';
export { POSTURE_BASIS_LABEL, POSTURE_BASIS_NOTE, POSTURE_BASIS_TITLE, POSTURE_TERMS, PostureBasisTag } from './basis.tsx';
export { CellDrawer, DRAWER_QUESTIONS, PostureNodeDrawer, type CellDrawerProps, type DrawerDetailStatus, type PostureNodeDrawerProps } from './CellDrawer.tsx';
export {
  POSTURE_SCHEMA_VERSION, type Alignment, type CompeteMode, type DeployMode, type PostureCell, type PostureCode, type PostureCore, type PostureDetail, type PostureMeta,
  type SecurityRationale, type Verb, type Yard, type YardRollup, type YfNode,
} from './contract.ts';
export {
  COUNTRY_POSTURE_TITLE, COUNTRY_STRATEGY_MAP_TITLE, CountryPostureCard, CountryStrategyMap, type CountryPostureCardProps, type CountryStrategyMapProps,
} from './CountryPostureCard.tsx';
export {
  aibHeaderTiles, cellKey, cellKeyParts, countPostures, countryCells, countryNames, countryPostureRows, DEFAULT_POSTURE_NODE, divisionRows, parsePostureNode,
  postureCountsText, postureCsvText, postureProvenance, POSTURE_SOURCE, sectorPostureRows, strategyCountsText, type PostureCounts, type PostureNodeParse,
  type PostureRow, type PostureTile,
} from './derive.ts';
export type { Family, NodeDetail } from './detail.ts';
export {
  byPositionOrder, DEPLOYMENT_FAMILIES, deploymentVars, familyWords, FENCE_TOP_N, fenceGroups, fenceReason, pathwayWords, positionVars, prizeVars, shareWords, supplyNeedVars, topDeploymentFamily, unitCountWords,
  tradeMomentumWords, type DeploymentVars, type FenceGroups, type FenceNode, type PositionVars, type PrizeVars, type SupplyNeedVars,
} from './fences.ts';
export {
  ALIGNMENT_LABEL, choiceWord, FENCE_LABEL, MARKET_PRIZE_LABEL, PRIZE_LABEL, prizeLabel, FENCE_OF_VERB, FENCE_ORDER, FENCE_VERBS, fenceChoiceWord, nodeFence, nodeFill, POSTURE_COLOR, POSTURE_LABEL, RATIONALE_LABEL, STAGE_DISPLAY,
  stageDisplay, VERB_COLOR, VERB_LABEL, VERB_LONG, VERB_ORDER, VERB_OUTLINE, VERB_SUBTITLE, verbOf, type Fence,
} from './grammar.ts';
export { AibDivisionIntro, CountryPostureIntro, SectorPostureIntro } from './intros.tsx';
export { DEPLOYMENT_NEED_TITLE, NEED_HEADING, NeedPanels, SUPPLY_NEED_TITLE, type NeedPanelsProps } from './NeedPanels.tsx';
export { POLICY_ALIGNMENT_TITLE, PolicyAlignmentTable, policyMomentumWords, type PolicyAlignmentTableProps } from './PolicyAlignmentTable.tsx';
export { AibDivisionStats, countryPostureCounts, CountryPostureTile, PostureCountsList, useCountryPostureCounts } from './PostureCounts.tsx';
export { ChoiceLegend, Swatch, TradeFreshness, type ChoiceLegendCounts, type ChoiceLegendProps } from './PostureLegend.tsx';
export { MAP_HELPER } from './PostureMatrix.tsx';
export { PRIZE_POSITION_TITLE, PrizePositionScatter, type PrizePositionScatterProps } from './PrizePositionScatter.tsx';
export { postureProfileHref, SECTOR_NODE_KEY, SECTOR_POSTURE_TITLE, SectorPostureCard, type SectorPostureCardProps } from './SectorPostureCard.tsx';
export { alliedRows, alliedSentence, choiceGroups, detailByKey, policyRows, supplyNeedRanking, deploymentNeedRanking, type AlliedRow } from './sections.ts';
export { LAYER_KEY, NODE_KEY, parseNodeParam, useNodeSelection, type NodeParse, type NodeSelection } from './selection.ts';
export { postureStore, setPostureStore, usePostureCore, usePostureDetail } from './store.ts';
export {
  capWords, familyValueWords, isMixedRollup, largeYardOf, largeYardWords, layerLabel, layerReason, layerUnits, MIXED_LEGEND, mixedFill, mixedYards, rollupWords, sivFamilies,
  signalWords, YARD_COLOR, YARD_LABEL, YARD_ORDER, yardWords, yfNode, type LargeYardCase, type LayerUnit, type SivFamilies,
} from './yards.ts';
export { STRATEGIC_CHOICES_TITLE, StrategicChoices, type StrategicChoicesProps } from './StrategicChoices.tsx';
export { listPhrases, nodePhrase, strategyClauses, strategySentence, type StrategyClause } from './strategy.ts';
