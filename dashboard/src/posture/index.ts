// The posture module's public surface for the pages that mount it (Country, Sector, AIB). posture/
// imports nothing from country/, sector/ or aib/; the pages wrap these cards in their own Sections.

export { AibDivisionCard, AIB_DIVISION_TITLE, indexNodesByCell, type AibDivisionCardProps, type PostureLinkedNode } from './AibDivisionCard.tsx';
export { POSTURE_BASIS_LABEL, POSTURE_BASIS_NOTE, POSTURE_BASIS_TITLE, POSTURE_TERMS, PostureBasisTag } from './basis.tsx';
export { COUNTRY_POSTURE_TITLE, CountryPostureCard, type CountryPostureCardProps } from './CountryPostureCard.tsx';
export {
  aibHeaderTiles, cellKey, cellKeyParts, countPostures, countryCells, countryNames, countryPostureRows, DEFAULT_POSTURE_NODE, divisionRows, parsePostureNode,
  postureCountsText, postureProvenance, POSTURE_SOURCE, sectorPostureRows, type PostureCounts, type PostureNodeParse, type PostureRow, type PostureTile,
} from './derive.ts';
export { AibDivisionIntro, CountryPostureIntro, SectorPostureIntro } from './intros.tsx';
export { AibDivisionStats, countryPostureCounts, CountryPostureTile, PostureCountsList, useCountryPostureCounts } from './PostureCounts.tsx';
export { postureProfileHref, SECTOR_NODE_KEY, SECTOR_POSTURE_TITLE, SectorPostureCard, type SectorPostureCardProps } from './SectorPostureCard.tsx';
export { postureStore, setPostureStore, usePostureCore, usePostureDetail } from './store.ts';
