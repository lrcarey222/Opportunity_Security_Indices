// Pure helpers of the Country page's canonical framework (design 2026-10-29, sections 4 and 5b; spec XI-XIX;
// the fences of 2026-10-30): the seven sections and their anchors (with the aliases of the old ones), the
// Medium fence's sub-sections, how an old #anchor that now sits in a closed disclosure is reached, the bridge
// between the posture node key ("Batteries|Refining") and the fact sheet's cell key ("batteries-refining"),
// the header's data-freshness words, the evidence and coverage counts of Evidence & Methodology, and the
// Legacy OSI mapping (spec Part II: Grow ~ Preserve, Upgrade ~ Catch Up, Exposed ~ Partner, Stable ~ Watch).
// No React, no IO.

import { cellKey, COUNTRY_STAGES, COUNTRY_TECHS, LEGACY_CORNERS, type LegacyCorner } from '../contract.ts';

// ---- sections and anchors ------------------------------------------------------------------------------

export interface ProfileSection {
  id: string;
  /** the h2 */
  label: string;
  /** the section nav's shorter words, when the h2 is long */
  nav?: string;
  /** old anchors that scroll here (alias spans in the section, always in the page) */
  aliases: readonly string[];
}

/**
 * The seven sections in reading order (spec XI.1; design 4; the fences of 2026-10-30): the nav, the h2s and
 * the anchors. The three fence sections replace Need, Prize & Position and Choices (their old anchors are
 * aliases: #need the High fence, #prize-position and #choices the Medium fence). Allied Strategy keeps the old
 * #aib-role anchors; the partnership analysis moved to the Medium fence's Partner sub-section.
 */
export const STRATEGY_SECTIONS: readonly ProfileSection[] = [
  { id: 'strategy', label: 'Strategy', aliases: ['posture', 'glance'] },
  { id: 'high-fence', label: 'High fence · Secure', aliases: ['need'] },
  { id: 'medium-fence', label: 'Medium fence · Preserve, Catch Up, Partner, Accelerate', aliases: ['prize-position', 'choices'] },
  { id: 'low-fence', label: 'Low fence · Watch', aliases: [] },
  { id: 'policy-alignment', label: 'Policy Alignment', aliases: ['policy', 'priorities'] },
  { id: 'allied', label: 'Allied Strategy', aliases: ['aib-role', 'allied-aib-role', 'allies'] },
  { id: 'evidence', label: 'Evidence & Methodology', aliases: ['notes', 'methods'] },
];

/** The Medium fence's sub-sections (h3s with their own anchors), in order; the partnership analysis's old anchors land on Partner. */
export const MEDIUM_SUBSECTIONS: readonly ProfileSection[] = [
  { id: 'medium-prize', label: 'Prize', aliases: [] },
  { id: 'medium-position', label: 'Competitive Position', aliases: [] },
  { id: 'medium-partner', label: 'Partner', aliases: ['partnerships', 'partners', 'allied-partnerships'] },
  { id: 'medium-accelerate', label: 'Accelerate', aliases: [] },
];

/** The Legacy OSI sub-section of Evidence & Methodology and the old anchors of its blocks (#why is the Why section itself, inside its disclosure: hashPlan opens it). */
export const LEGACY_SECTION: ProfileSection = { id: 'legacy-osi', label: 'Legacy OSI framework', aliases: ['position', 'space', 'opportunities', 'sectors', 'si'] };

/** The disclosure of the page an old link may need open: the Legacy OSI results. */
export const LEGACY_FOLD = 'legacy';

/**
 * How the page reaches an old #anchor whose content now sits in a closed disclosure: the disclosures to
 * open (outermost first) and the element to scroll to once they are open. Anchors not listed are plain
 * (a section id or an alias span).
 */
const HASH_PLAN: Readonly<Record<string, { folds: readonly string[]; target: string }>> = {
  position: { folds: [LEGACY_FOLD], target: 'legacy-space' },
  space: { folds: [LEGACY_FOLD], target: 'legacy-space' },
  opportunities: { folds: [LEGACY_FOLD], target: 'legacy-opportunities' },
  why: { folds: [LEGACY_FOLD, 'why'], target: 'fold-why' },
  si: { folds: [LEGACY_FOLD, 'si'], target: 'fold-si' },
  sectors: { folds: [LEGACY_FOLD], target: 'legacy-sectors' },
  perspective: { folds: [LEGACY_FOLD, 'perspective'], target: 'perspective' },
};

/** The plan for a location hash ("#why"): disclosures to open and the element to scroll to (the anchor itself when none). */
export function hashPlan(hash: string): { folds: readonly string[]; target: string | null } {
  const id = hash.replace(/^#/, '');
  if (!id) return { folds: [], target: null };
  return HASH_PLAN[id] ?? { folds: [], target: id };
}

// ---- node <-> cell -------------------------------------------------------------------------------------

/** The fact sheet's cell key of a posture node ("Batteries|Refining" -> "batteries-refining"), or null. */
export function cellKeyOfNode(node: string | null | undefined): string | null {
  if (!node) return null;
  const [t, s, ...rest] = node.split('|');
  if (rest.length) return null;
  const tech = COUNTRY_TECHS.find((x) => x.label === t);
  const stage = COUNTRY_STAGES.find((x) => x.label === s);
  return tech && stage ? cellKey(tech.slug, stage.slug) : null;
}

/** The posture node key of a fact-sheet cell ({tech: "Batteries", stage: "Refining"} -> "Batteries|Refining"). */
export const nodeKeyOfCell = (c: { tech: string; stage: string }): string => `${c.tech}|${c.stage}`;

// ---- freshness -----------------------------------------------------------------------------------------

const MONTHS = ['January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December'];

/** "2026-10-29T07:14:00Z" -> "29 October 2026" (UTC date); null for a missing or unreadable value. */
export function dateLabel(iso: string | null | undefined): string | null {
  if (!iso) return null;
  const m = /^(\d{4})-(\d{2})-(\d{2})/.exec(iso);
  if (!m) return null;
  const mo = Number(m[2]);
  if (mo < 1 || mo > 12) return null;
  return `${Number(m[3])} ${MONTHS[mo - 1]} ${m[1]}`;
}

export interface FreshnessInput {
  /** the posture core's `created` */
  created: string | null;
  /** meta.benchmark_period ("2022-2024") */
  benchmark: string | null;
  /** meta.momentum_through ("Aug 2026") */
  momentumThrough: string | null;
  /** the fact sheet's NEIS baseline label ("12 October 2026") */
  asOfLabel: string;
}

/** The header's data-freshness items, in order; each says what it dates. */
export function freshnessItems(f: FreshnessInput): string[] {
  const out: string[] = [];
  const built = dateLabel(f.created);
  if (built) out.push(`Strategy built ${built}`);
  if (f.benchmark) out.push(`trade benchmark ${f.benchmark}`);
  if (f.momentumThrough) out.push(`trade momentum through ${f.momentumThrough}`);
  // the fact sheet's own date (the facts sections 1-6 read: policy records, trade, partners); "index" words stay in Legacy OSI
  out.push(`country facts as of ${f.asOfLabel}`);
  return out;
}

// ---- Evidence & Methodology ------------------------------------------------------------------------------

export interface CoverageCounts {
  nodes: number;
  assessed: number;
  notAssessed: number;
  strong: number;
  moderate: number;
  weak: number;
  /** assessed nodes without an evidence grade */
  ungraded: number;
  /** nodes within the borderline band of a threshold */
  borderline: number;
  /** nodes with a consistency flag */
  flagged: number;
}

/** Coverage and evidence specificity of an economy's nodes (posture cells). */
export function coverageCounts(cells: readonly { posture: string; evidence: string | null; borderline: string | null; flag: string | null }[]): CoverageCounts {
  const n: CoverageCounts = { nodes: cells.length, assessed: 0, notAssessed: 0, strong: 0, moderate: 0, weak: 0, ungraded: 0, borderline: 0, flagged: 0 };
  for (const c of cells) {
    if (c.posture === 'N') n.notAssessed++;
    else {
      n.assessed++;
      if (c.evidence === 'strong') n.strong++;
      else if (c.evidence === 'moderate') n.moderate++;
      else if (c.evidence === 'weak') n.weak++;
      else n.ungraded++;
    }
    if (c.borderline) n.borderline++;
    if (c.flag) n.flagged++;
  }
  return n;
}

// ---- Legacy OSI ------------------------------------------------------------------------------------------

/** What the legacy indices mix together, which the canonical framework separates (spec Part II). */
export const LEGACY_MIXES: readonly string[] = ['capability', 'vulnerability', 'production', 'trade', 'investment', 'demand', 'reserves', 'concentration', 'deployment'];

export interface LegacyMappingRow {
  corner: LegacyCorner;
  label: string;
  meaning: string;
  /** the canonical choice it maps to conceptually */
  mapsTo: string;
  /** why a node in this corner may take another choice in the canonical framework */
  differs: string;
}

const DIFFERS: Record<LegacyCorner, string> = {
  legacy_grow: 'Preserve needs a demonstrated Competitive Position in the top quarter of the node’s contenders; a high Economic Opportunity index alone does not give it.',
  legacy_upgrade: 'Catch Up needs a real prize and a credible pathway; low Energy Security there may be a supply-chain need the Secure tests or Partner answer instead.',
  legacy_exposed: 'Partner needs a high supply-chain need with a weak domestic case; a low index can come from trade or reserves without a dependence problem.',
  legacy_stable:
    'Watch is a positive conclusion of low need, with a low prize and also with a high prize where there is neither a strong position nor a credible pathway; at Deployment & Use the canonical framework reads deployment need instead and may say Accelerate.',
};

/** The Legacy OSI corners with their conceptual canonical choice (Grow ~ Preserve, Upgrade ~ Catch Up, Exposed ~ Partner, Stable ~ Watch). */
export const LEGACY_MAPPING_ROWS: readonly LegacyMappingRow[] = LEGACY_CORNERS.map((q) => ({ corner: q.id, label: q.label, meaning: q.meaning, mapsTo: q.maps_to, differs: DIFFERS[q.id] }));
