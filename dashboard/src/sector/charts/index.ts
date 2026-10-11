// The Sector chart kit (spec_sector.md 5.3), presentational: pre-derived rows and pre-formatted text
// in, figures with table twins out. Prop types: ./types.ts.

export * from './types.ts';
export { SPLIT_ORDER, contrast, groupColor, groupCss, groupFill, groupLabel, seqStep, textOn } from './color.ts';
export { BlocBar, ShareSplitChart, splitSegments, splitText } from './ShareSplitChart.tsx';
export { GAP_ID, LeagueBars, leagueRows, STATE_WORDS } from './LeagueBars.tsx';
export { areaColumns, GroupArea } from './GroupArea.tsx';
export { indexLine, LabelledLines, MAX_LINES } from './LabelledLines.tsx';
export { Dumbbell } from './Dumbbell.tsx';
export { Beeswarm } from './Beeswarm.tsx';
export { sharedDomain, SmallMultiples } from './SmallMultiples.tsx';
export { HeatTable } from './HeatTable.tsx';
export { CellGrid } from './CellGrid.tsx';
export { TopFlowsList } from './TopFlowsList.tsx';
export { GroupLegend, StateLegend } from './GroupLegend.tsx';
export { CoverageChips } from './CoverageChips.tsx';
export { CaptionProvider, fallbackCaption, useCaption } from './caption.tsx';
export { INLINE_CAVEATS, MeasureCaption, SectorChartCard, VintageBadge, type SectorChartCardProps } from './SectorChartCard.tsx';
export { SvgDefs } from './SvgDefs.tsx';
