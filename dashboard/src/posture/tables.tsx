// The table twins of the posture cards: TableView columns over the pure rows of derive.ts (one economy's
// 66 nodes, one node's economies, the allied division). Headers spell every term out (no bare acronym);
// the stage sorts in chain order; shares and percentiles show as percentages, the CSV keeps fractions.

import type { Col } from '../components/TableView.tsx';
import { stageRank } from '../lib/stages.ts';
import { pct, type DivisionRow, type PostureRow } from './derive.ts';

const pctCol = <R,>(id: string, header: string, v: (r: R) => number | null, digits = 0): Col<R> => ({ id, header, value: v, cell: (r) => pct(v(r), digits), num: true });

const postureCol: Col<PostureRow> = { id: 'posture', header: 'Posture', value: (r) => r.postureWord, cell: (r) => r.postureWord ?? 'Not classified', sortValue: (r) => (r.posture ? 'SCD'.indexOf(r.posture) : 3) };
const modeCol: Col<PostureRow> = { id: 'mode', header: 'Compete mode or Secure rationale', value: (r) => r.mode ?? r.rationale };
const quadrantCol: Col<PostureRow> = { id: 'quadrant', header: 'Quadrant', value: (r) => r.quadrant };
const partnerCol: Col<PostureRow> = {
  id: 'partner',
  header: 'First trusted partner',
  value: (r) => (r.mode === 'Partner' && r.partnerGap ? 'Partner gap' : r.partner1Name),
};
const borderlineCol: Col<PostureRow> = { id: 'borderline', header: 'Borderline on', value: (r) => r.borderline };
const measures: Col<PostureRow>[] = [
  pctCol<PostureRow>('need', 'Strategic Need', (r) => r.need),
  pctCol<PostureRow>('opp', 'Industrial Opportunity', (r) => r.opp),
  pctCol<PostureRow>('cp', 'Competitive Position', (r) => r.cp),
  pctCol<PostureRow>('share', 'World supply share', (r) => r.share, 1),
];

/** One economy's nodes: technology, stage (chain order), posture and its reasons, the axes, policy, evidence, momentum, partner. */
export const COUNTRY_POSTURE_COLS: Col<PostureRow>[] = [
  { id: 'tech', header: 'Technology', value: (r) => r.tech },
  { id: 'stage', header: 'Stage', value: (r) => r.stage, sortValue: (r) => stageRank(r.stage) },
  postureCol,
  modeCol,
  quadrantCol,
  ...measures,
  { id: 'alignment', header: 'Policy alignment', value: (r) => r.alignment },
  { id: 'evidence', header: 'Evidence', value: (r) => r.evidence },
  { id: 'momentum', header: 'Trade momentum', value: (r) => r.momentum },
  partnerCol,
  borderlineCol,
];

const TIER_WORD: Record<string, string> = { core: 'Core member', partner: 'Partner' };

/** One node's economies: country, coalition tier, posture and its reasons, the axes, partner, and whether the scatter places it. */
export const SECTOR_POSTURE_COLS: Col<PostureRow>[] = [
  { id: 'country', header: 'Economy', value: (r) => r.country },
  { id: 'tier', header: 'Coalition', value: (r) => (r.tier ? TIER_WORD[r.tier] : null) },
  postureCol,
  modeCol,
  quadrantCol,
  ...measures,
  partnerCol,
  borderlineCol,
  { id: 'plotted', header: 'On the chart', value: (r) => (r.plotted ? 'yes' : 'no: need or opportunity not measured') },
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
