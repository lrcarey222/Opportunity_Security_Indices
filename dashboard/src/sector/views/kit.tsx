// The Sector chart kit and chrome as the views use them (spec_sector.md 5.3). Builder B owns the kit
// (src/sector/charts/**) and the chrome (src/sector/chrome/**); views import them only from here.
// (Until they landed, this file re-exported table-twin placeholders with the same props.)

export * from '../charts/index.ts';
export { BottomSheet, PageNotesProvider, SectionRail, SectorBar, ThemeChips, useAddPageNotes, usePageNotes, useScrollSpy, COVERAGE_GLYPH } from '../chrome/index.ts';
export type { RailSection, SectorBarPicker, SectorBarProps, SectorBarStage, StageCoverage, ThemeChip } from '../chrome/index.ts';
