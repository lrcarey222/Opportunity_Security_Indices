// Sector chrome (spec_sector.md 4.0, 4.3, 4.6, 6): the sticky SectorBar, the profile's SectionRail, the
// phone BottomSheet, the explorer's ThemeChips and the page's data notes.

export { BottomSheet, type BottomSheetProps } from './BottomSheet.tsx';
export { COVERAGE_GLYPH, SectorBar, type SectorBarPicker, type SectorBarProps, type SectorBarStage, type StageCoverage } from './SectorBar.tsx';
export { SectionRail, useScrollSpy, type RailSection, type SectionRailProps } from './SectionRail.tsx';
export { ThemeChips, type ThemeChip, type ThemeChipsProps } from './ThemeChips.tsx';
export { PageNotesProvider, useAddPageNotes, usePageNotes } from './pageNotes.tsx';
