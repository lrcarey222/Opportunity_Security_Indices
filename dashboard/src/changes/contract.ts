// Change layer contract (docs/change-layer-architecture.md): dashboard/public/data/changes/{manifest.json,
// changes.<md5-8>.json}, written by scripts/45_build_posture_vintages.R (R/change/change_export.R). Every value
// here is computed deterministically in R; the UI only reads and displays it. Absent files mean "no change
// history" and the pages render as before.

export const CHANGES_DATA_DIR = 'data/changes/';
export const CHANGES_SCHEMA = 'osi-changes';

export type ChangeType = 'recommendation' | 'score' | 'data' | 'evidence' | 'confidence';
export type Severity = 'minor' | 'moderate' | 'major';
/** real_world: same method; methodology: method alone (verified data, or a bridge run); mixed: both changed;
 *  unattributed: method changed and the data could not be verified; both: a bridge run split it */
export type Attribution = 'real_world' | 'methodology' | 'mixed' | 'unattributed' | 'both';

export interface ChangeSide {
  choice: string | null;
  posture: string | null;
  needType: string | null;
  need: number | null;
  prize: number | null;
  cp: number | null;
  market: number | null;
  share: number | null;
  confidence: string | null;
  evidence: string | null;
}

export interface ChangeRecord {
  /** aib:posture:<ISO3>:<tech-slug>:<stage-slug> */
  id: string;
  iso3: string;
  tech: string;
  stage: string;
  prev: string;
  cur: string;
  types: ChangeType[];
  subtype: 'posture' | 'mode' | 'coverage' | null;
  severity: Severity;
  magnitude: number | null;
  attribution: Attribution;
  methodChanged: boolean;
  methodUnverified: boolean;
  crossed: string[];
  evidenceDirection: 'improved' | 'worsened' | null;
  confidenceDirection: 'improved' | 'worsened' | null;
  sources: string[];
  impacts: string[];
  before: ChangeSide;
  after: ChangeSide;
  delta: { need: number | null; prize: number | null; cp: number | null };
  explanation: string;
}

export interface Vintage {
  id: string;
  created: string;
  gitCommit: string;
  provisional: boolean;
  stampSource: string;
  coreFile: string;
  coreCommit: string | null;
  method: { id: string; label: string; unverified: boolean };
  data: { id: string; verified?: boolean; [k: string]: unknown };
  nCells: number;
}

export interface VintagePair {
  prev: string;
  cur: string;
  comparable: boolean;
  reason: string | null;
  methodChanged: boolean;
  methodUnverified: boolean;
  dataChanged: boolean;
  bridge: boolean;
  /** the bundle carries this pair's records (older pairs: summary only) */
  detail: boolean;
  nCells: number | null;
  nUnchanged: number | null;
  counts: Partial<Record<ChangeType | Attribution | 'meaningful', number>>;
}

export interface ImpactRecord {
  impact_id: string;
  event_id: string;
  event_slug?: string;
  assessment_id: string;
  dimension: string;
  direction?: string;
  reason: string;
  reason_method: string;
  status: string;
  reviewer?: string;
  review_timestamp?: string;
}

export interface ChangesBundle {
  schema: typeof CHANGES_SCHEMA;
  schema_version: number;
  created: string;
  git_commit: string;
  vintages: Vintage[];
  pairs: VintagePair[];
  changes: ChangeRecord[];
  impacts: ImpactRecord[];
  sources: Record<string, { source_ids: string[]; doc: string }>;
}

export interface ChangesManifest {
  schema: typeof CHANGES_SCHEMA;
  schema_version: number;
  created: string;
  changes: string;
  latest: string;
}

export class ChangesContractError extends Error {}

const isObj = (x: unknown): x is Record<string, unknown> => typeof x === 'object' && x !== null && !Array.isArray(x);

export function checkChangesManifest(x: unknown): ChangesManifest {
  if (!isObj(x) || x.schema !== CHANGES_SCHEMA || typeof x.changes !== 'string') throw new ChangesContractError('changes/manifest.json is not an osi-changes manifest.');
  if (x.schema_version !== 1) throw new ChangesContractError(`changes schema_version ${String(x.schema_version)} is not supported.`);
  return x as unknown as ChangesManifest;
}

export function checkChangesBundle(x: unknown): ChangesBundle {
  if (!isObj(x) || x.schema !== CHANGES_SCHEMA) throw new ChangesContractError('The changes document is not an osi-changes bundle.');
  for (const k of ['vintages', 'pairs', 'changes'] as const) if (!Array.isArray(x[k])) throw new ChangesContractError(`changes.${k} is missing.`);
  const arr = <T,>(v: unknown): T[] => (Array.isArray(v) ? (v as T[]) : []);
  // jsonlite writes an empty character vector as [] and a missing value as null: normalise the arrays
  for (const c of x.changes as Record<string, unknown>[]) {
    if (typeof c.id !== 'string' || !isObj(c.before) || !isObj(c.after)) throw new ChangesContractError('A change record lacks its id or sides.');
    c.types = arr(c.types);
    c.crossed = arr(c.crossed);
    c.sources = arr(c.sources);
    c.impacts = arr(c.impacts);
  }
  x.impacts = arr(x.impacts);
  x.sources = isObj(x.sources) ? x.sources : {};
  return x as unknown as ChangesBundle;
}

// ---- derivations (pure) -------------------------------------------------------------------------------------

export const CHOICE_LABEL: Record<string, string> = {
  secure: 'Secure',
  preserve: 'Preserve',
  catch_up: 'Catch Up',
  partner: 'Partner',
  accelerate: 'Accelerate',
  watch: 'Watch',
  not_assessed: 'Not assessed',
};
export const choiceLabel = (c: string | null | undefined): string => (c ? (CHOICE_LABEL[c] ?? c.replace(/_/g, ' ')) : 'Not assessed');

export const ATTRIBUTION_LABEL: Record<Attribution, string> = {
  real_world: 'New data (same method)',
  methodology: 'Method change only',
  mixed: 'Method and data both changed',
  unattributed: 'Method changed; data not verified',
  both: 'Split by a bridge run',
};

export const TYPE_LABEL: Record<ChangeType, string> = {
  recommendation: 'Recommendation',
  score: 'Score',
  data: 'Data',
  evidence: 'Evidence',
  confidence: 'Confidence',
};

export const SCORE_FIELDS = [
  { key: 'need', label: 'Need' },
  { key: 'prize', label: 'Prize' },
  { key: 'cp', label: 'Position' },
] as const;

/** Minimum score move shown on the card (config/change_layer.yml thresholds.score.min_delta). */
export const SCORE_MIN_DELTA = 0.05;

export const vintageDate = (b: ChangesBundle, id: string): string => (b.vintages.find((v) => v.id === id)?.created ?? id).slice(0, 10);

/** The latest comparable pair (the "last vintage" comparison), or null. */
export function latestPair(b: ChangesBundle): VintagePair | null {
  const comp = b.pairs.filter((p) => p.comparable);
  return comp.length ? comp[comp.length - 1] : null;
}

/** A cell's change records, newest first. */
export function changesForCell(b: ChangesBundle, iso3: string, tech: string, stage: string): ChangeRecord[] {
  const order = new Map(b.vintages.map((v, i) => [v.id, i]));
  return b.changes
    .filter((c) => c.iso3 === iso3 && c.tech === tech && c.stage === stage)
    .sort((a, z) => (order.get(z.cur) ?? 0) - (order.get(a.cur) ?? 0));
}

/** The pairs whose records the bundle carries (the window a "no change" statement covers). */
export function detailWindow(b: ChangesBundle): { from: string; to: string } | null {
  const d = b.pairs.filter((p) => p.comparable && p.detail);
  if (!d.length) return null;
  return { from: vintageDate(b, d[0].prev), to: vintageDate(b, d[d.length - 1].cur) };
}

/** The score lines worth showing: moved by at least the minimum, or crossed a bar. */
export function scoreLines(c: ChangeRecord): { label: string; from: number | null; to: number | null; crossed: boolean }[] {
  return SCORE_FIELDS.flatMap(({ key, label }) => {
    const from = c.before[key];
    const to = c.after[key];
    const d = c.delta[key];
    const crossed = c.crossed.some((s) => s.startsWith(`${key} `));
    if (!crossed && (d === null || Math.abs(d) < SCORE_MIN_DELTA - 1e-9) && !((from === null) !== (to === null))) return [];
    return [{ label: key === 'need' && c.after.needType === 'deployment' ? 'Deployment need' : label, from, to, crossed }];
  });
}

export const fmtScore = (v: number | null): string => (v === null || v === undefined ? 'n/a' : (Math.round(v * 100) / 100).toFixed(2));
export const fmtShare = (v: number | null): string => (v === null || v === undefined ? 'n/a' : `${(v * 100).toFixed(1)}%`);
