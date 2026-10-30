// The table twins of the posture cards: TableView columns over the pure rows of derive.ts (one economy's
// 66 nodes, one node's economies, the allied division). Headers spell every term out (no bare acronym);
// the stage shows its display name and sorts in chain order; shares and percentiles show as percentages,
// the CSV keeps fractions. Trade momentum is a column here (it left the map's cells, design D7).

import type { Col } from '../components/TableView.tsx';
import { stageRank } from '../lib/stages.ts';
import { pct, type DivisionRow, type PostureRow } from './derive.ts';
import { borderlineWords } from './grammar.ts';

const pctCol = <R,>(id: string, header: string, v: (r: R) => number | null, digits = 0): Col<R> => ({ id, header, value: v, cell: (r) => pct(v(r), digits), num: true });

const FAMILY_SORT: Record<string, number> = { S: 0, C: 1, D: 2, N: 3 };
const choiceCol: Col<PostureRow> = {
  id: 'choice', header: 'Choice', value: (r) => r.choice, cell: (r) => r.choice ?? 'Not classified', sortValue: (r) => (r.posture ? FAMILY_SORT[r.posture] : 4),
};
const rationaleCol: Col<PostureRow> = { id: 'rationale', header: 'Secure rationale', value: (r) => r.rationale };
const partnerCol: Col<PostureRow> = {
  id: 'partner',
  header: 'First trusted partner',
  value: (r) => (r.mode === 'Partner' && r.partnerGap ? 'No credible allied solution yet' : r.partner1Name),
};
const borderlineCol: Col<PostureRow> = { id: 'borderline', header: 'Borderline on', value: (r) => borderlineWords(r.borderline) };
const measures: Col<PostureRow>[] = [
  { id: 'need_type', header: 'Need type', value: (r) => r.needType },
  pctCol<PostureRow>('need', 'Need', (r) => r.need),
  pctCol<PostureRow>('prize', 'Industrial Opportunity', (r) => r.prize),
  pctCol<PostureRow>('cp', 'Competitive Position', (r) => r.cp),
  pctCol<PostureRow>('share', 'World supply share', (r) => r.share, 1),
];

/** One economy's nodes: technology, stage (chain order), the choice and its reasons, the three dimensions, policy, evidence, momentum, partner. */
export const COUNTRY_POSTURE_COLS: Col<PostureRow>[] = [
  { id: 'tech', header: 'Technology', value: (r) => r.tech },
  { id: 'stage', header: 'Stage', value: (r) => r.stageName, sortValue: (r) => stageRank(r.stage) },
  choiceCol,
  rationaleCol,
  ...measures,
  { id: 'pathway', header: 'Credible pathway', value: (r) => r.pathway },
  { id: 'alignment', header: 'Policy alignment', value: (r) => r.alignment },
  { id: 'evidence', header: 'Evidence', value: (r) => r.evidence },
  { id: 'momentum', header: 'Trade momentum', value: (r) => r.momentum },
  partnerCol,
  borderlineCol,
  { id: 'flag', header: 'Consistency review', value: (r) => r.flag },
];

const TIER_WORD: Record<string, string> = { core: 'Core member', partner: 'Partner' };

/** One node's economies: country, coalition tier, the choice and its reasons, the dimensions, partner, and whether the scatter places it. */
export const SECTOR_POSTURE_COLS: Col<PostureRow>[] = [
  { id: 'country', header: 'Economy', value: (r) => r.country },
  { id: 'tier', header: 'Coalition', value: (r) => (r.tier ? TIER_WORD[r.tier] : null) },
  choiceCol,
  rationaleCol,
  ...measures,
  partnerCol,
  borderlineCol,
  { id: 'plotted', header: 'On the chart', value: (r) => (r.plotted ? 'yes' : 'no: need or prize not measured') },
];

/** The allied division: one row per node. */
export const DIVISION_COLS: Col<DivisionRow>[] = [
  { id: 'tech', header: 'Technology', value: (r) => r.tech },
  { id: 'stage', header: 'Stage', value: (r) => r.stage, sortValue: (r) => stageRank(r.stage) },
  { id: 'primary', header: 'Primary allied holder', value: (r) => r.primaryName },
  pctCol<DivisionRow>('primary_share', 'Primary share', (r) => r.primaryShare),
  { id: 'backups', header: 'Backups', value: (r) => r.backups },
  { id: 'redundancy', header: 'Redundancy', value: (r) => r.redundancy, num: true },
  pctCol<DivisionRow>('allied', 'Allied supply', (r) => r.alliedShare),
  pctCol<DivisionRow>('coercive', 'Coercive powers', (r) => r.coerciveShare),
  { id: 'leader', header: 'World leader', value: (r) => (r.leaderName ? `${r.leaderName} (${pct(r.leaderShare)})` : null) },
  { id: 'gap', header: 'Gap', value: (r) => r.gapLabel },
  { id: 'other_gaps', header: 'Other gaps', value: (r) => r.otherGaps },
  { id: 'partner_demand', header: 'Partner demand', value: (r) => r.partnerDemand, num: true },
  { id: 'secure_demand', header: 'Secure demand', value: (r) => r.secureDemand, num: true },
  { id: 'members', header: 'Coalition members asking', value: (r) => r.memberPartnerDemand, num: true },
  { id: 'source', header: 'Capability from', value: (r) => r.capabilitySource },
];
