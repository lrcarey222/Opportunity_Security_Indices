// The six-stage posture bundle (data/posture, written by scripts/91_export_posture_data.R from the
// strategic position framework, docs/strategic_position.md): the recommended posture of every country x
// technology x six-stage node, its decision quadrant, Secure rationale, partner recommendations and trade
// freshness, and the allied division of labour. The core document is columnar; decodeCore() turns it
// into rows. Checks throw PostureContractError with every problem found.

export const POSTURE_SCHEMA = 'osi-posture';
export const POSTURE_SCHEMA_VERSION = 1;
export const POSTURE_DATA_DIR = 'data/posture/';
export const POSTURE_STAGES = ['Extraction', 'Refining', 'Components', 'Assembly', 'Deployment', 'Recovery'] as const;
export type PostureStage = (typeof POSTURE_STAGES)[number];

export type PostureCode = 'S' | 'C' | 'D';
export type CompeteMode = 'catch_up' | 'preserve' | 'partner';
export type Quadrant = CompeteMode | 'watch' | 'insufficient_data';
export type SecurityRationale = 'operational_control' | 'coercive_chokepoint' | 'systemic_resilience';
export type Alignment = 'aligned' | 'under_prioritized' | 'over_prioritized' | 'mixed' | 'insufficient_data';
export type Momentum = 'up' | 'flat' | 'down';
export type AibGap = 'none' | 'no_trusted_supplier' | 'single_point_of_failure' | 'insufficient_redundancy' | 'weak_evidence';

export class PostureContractError extends Error {
  readonly problems: string[];
  constructor(problems: string[]) {
    super(`Posture data does not match the contract: ${problems.slice(0, 3).join('; ')}`);
    this.name = 'PostureContractError';
    this.problems = problems;
  }
}

export interface PostureManifest {
  schema: string;
  schema_version: number;
  created: string;
  core: string;
  details: Record<string, string>;
  counts: { countries: number; cells: number; division_nodes: number };
}

export interface PostureMeta {
  techs: string[];
  stages: string[];
  benchmark_period: string | null;
  momentum_period: string | null;
  momentum_previous_period: string | null;
  momentum_through: string | null;
  momentum_band: number;
  need_threshold: number;
  opportunity_threshold: number;
  borderline_band: number;
  core_members: number;
  partner_members: number;
}

export interface PostureCell {
  iso3: string;
  tech: string;
  stage: PostureStage;
  posture: PostureCode;
  variant: string;
  mode: CompeteMode | null;
  quadrant: Quadrant | null;
  need: number | null;
  opp: number | null;
  cp: number | null;
  share: number | null;
  alignment: Alignment | null;
  alignmentFlag: string | null;
  confidence: 'high' | 'moderate' | 'low' | null;
  evidence: string | null;
  rationale: SecurityRationale | null;
  borderline: string | null;
  flag: string | null;
  partner1: string | null;
  partnerGap: boolean | null;
  momentum: Momentum | null;
  momentumQuality: string | null;
}

export interface DivisionNode {
  tech: string;
  stage: PostureStage;
  primary: string | null;
  primaryShare: number | null;
  backups: { iso3: string; share: number | null }[];
  redundancy: number;
  alliedShare: number | null;
  coerciveShare: number | null;
  leader: string | null;
  leaderShare: number | null;
  gap: AibGap;
  gaps: AibGap[];
  source: string | null;
  aibNodes: number | null;
  partnerDemand: number;
  secureDemand: number;
  partnerGaps: number;
  memberPartnerDemand: number;
}

export interface PostureCore {
  meta: PostureMeta;
  countries: { iso3: string; name: string; tier: string }[];
  cells: PostureCell[];
  division: DivisionNode[];
  aibHeader: { core_members: number; members: number; priority_nodes: number; covered_share: number | null; unresolved_gaps: number };
  /** policy design text keyed variant|delivery */
  design: Record<string, { fence?: string; fence_level?: string; gate?: string; build?: string; benchmark?: string; exit?: string }>;
}

export interface DetailPartner {
  iso3: string;
  capability?: number;
  cp?: number;
  relationship?: number;
  diversification?: number;
}
export interface DetailTest {
  id: string;
  state: string | null;
  detail: string | null;
}
export interface DetailCell {
  tech: string;
  stage: PostureStage;
  posture: 'secure' | 'compete' | 'deploy';
  variant: string;
  mode?: CompeteMode;
  quadrant?: Quadrant;
  confidence?: string;
  possible?: string;
  pivotal?: string;
  delivery?: string;
  security_rationale?: SecurityRationale;
  security_routes?: string;
  need?: number;
  opportunity?: number;
  opportunity_basis?: string;
  cp?: number;
  home_share?: number;
  trusted_share?: number;
  coercive_top?: string;
  coercive_top_share?: number;
  coercive_import_share?: number;
  borderline?: string;
  consistency_flag?: string;
  alignment?: string;
  alignment_flag?: string;
  policy_priority?: number;
  fence?: string;
  fence_level?: string;
  gate?: string;
  build?: string;
  benchmark?: string;
  exit?: string;
  partners?: DetailPartner[];
  partner_gap?: boolean;
  partner_reason?: string;
  partner_evidence?: string;
  partner_source?: string;
  trade?: {
    benchmark_period?: string;
    benchmark_source?: string;
    benchmark_share?: number;
    benchmark_rca?: number;
    benchmark_exports?: number;
    momentum_period?: string;
    momentum_previous_period?: string;
    momentum_quality?: string;
    momentum_raw?: number;
    momentum_specialization?: number;
    momentum_score?: number;
    months_observed?: number;
    exports_current?: number;
    exports_previous?: number;
  };
  tests?: DetailTest[];
  /** the deciding test's words */
  reason?: string;
}
export interface PostureDetail {
  iso3: string;
  country: string;
  cells: DetailCell[];
}

const isObj = (v: unknown): v is Record<string, unknown> => typeof v === 'object' && v !== null && !Array.isArray(v);
const asArr = (v: unknown): unknown[] => (Array.isArray(v) ? v : []);
const str = (v: unknown): string | null => (typeof v === 'string' ? v : null);
const num = (v: unknown): number | null => (typeof v === 'number' && Number.isFinite(v) ? v : null);

function checkHeader(doc: unknown, what: string, problems: string[]): doc is Record<string, unknown> {
  if (!isObj(doc)) {
    problems.push(`${what} is not an object`);
    return false;
  }
  if (doc.schema !== POSTURE_SCHEMA) problems.push(`${what}: schema is not ${POSTURE_SCHEMA}`);
  if (typeof doc.schema_version !== 'number') problems.push(`${what}: no schema_version`);
  else if (doc.schema_version > POSTURE_SCHEMA_VERSION) problems.push(`${what}: schema_version ${doc.schema_version} needs a newer app`);
  return true;
}

export function checkPostureManifest(doc: unknown): PostureManifest {
  const p: string[] = [];
  if (checkHeader(doc, 'manifest', p)) {
    if (typeof doc.core !== 'string') p.push('manifest: no core file');
    if (!isObj(doc.details)) p.push('manifest: no details map');
  }
  if (p.length) throw new PostureContractError(p);
  return doc as unknown as PostureManifest;
}

const POSTURE_CODES = new Set(['S', 'C', 'D']);
const MODES = new Set(['catch_up', 'preserve', 'partner']);
const QUADRANTS = new Set(['catch_up', 'preserve', 'partner', 'watch', 'insufficient_data']);
const GAPS = new Set(['none', 'no_trusted_supplier', 'single_point_of_failure', 'insufficient_redundancy', 'weak_evidence']);

/** Check and decode the columnar core document. */
export function decodeCore(doc: unknown): PostureCore {
  const p: string[] = [];
  if (!checkHeader(doc, 'core', p)) throw new PostureContractError(p);
  const meta = doc.meta as PostureMeta | undefined;
  if (!isObj(meta)) p.push('core: no meta');
  const techs = asArr(meta?.techs).map(String);
  const stages = asArr(meta?.stages).map(String);
  if (stages.join('|') !== POSTURE_STAGES.join('|')) p.push(`core: stages are not the six stages (${stages.join(', ')})`);
  const c = isObj(doc.countries) ? doc.countries : {};
  const iso = asArr(c.iso3).map(String);
  const names = asArr(c.name);
  const tiers = asArr(c.tier);
  const countries = iso.map((i, k) => ({ iso3: i, name: str(names[k]) ?? i, tier: str(tiers[k]) ?? 'none' }));
  const k = isObj(doc.cells) ? doc.cells : {};
  const col = (n: string) => asArr(k[n]);
  const n = col('p').length;
  for (const name of ['c', 't', 's', 'v', 'm', 'q', 'need', 'opp', 'cp', 'al', 'conf', 'sec', 'p1', 'gap', 'mom']) {
    if (col(name).length !== n) p.push(`core: cells.${name} has ${col(name).length} values, not ${n}`);
  }
  const cells: PostureCell[] = [];
  if (!p.length) {
    const [cc, tt, ss, pp, vv, mm, qq] = ['c', 't', 's', 'p', 'v', 'm', 'q'].map(col);
    const [need, opp, cp, share, al, alf, conf, ev, sec, bd, fl, p1, gap, mom, mq] = ['need', 'opp', 'cp', 'share', 'al', 'alf', 'conf', 'ev', 'sec', 'bd', 'fl', 'p1', 'gap', 'mom', 'mq'].map(col);
    let bad = 0;
    for (let i = 0; i < n; i++) {
      const code = pp[i];
      const ci = num(cc[i]);
      const ti = num(tt[i]);
      const si = num(ss[i]);
      if (!POSTURE_CODES.has(code as string) || ci === null || ti === null || si === null || !iso[ci] || !techs[ti] || !stages[si]) {
        bad++;
        continue;
      }
      const m = str(mm[i]);
      const q = str(qq[i]);
      if ((m && !MODES.has(m)) || (q && !QUADRANTS.has(q))) bad++;
      cells.push({
        iso3: iso[ci], tech: techs[ti], stage: stages[si] as PostureStage, posture: code as PostureCode, variant: str(vv[i]) ?? '',
        mode: (m as CompeteMode) ?? null, quadrant: (q as Quadrant) ?? null, need: num(need[i]), opp: num(opp[i]), cp: num(cp[i]),
        share: num(share[i]), alignment: (str(al[i]) as Alignment) ?? null, alignmentFlag: str(alf[i]),
        confidence: (str(conf[i]) as PostureCell['confidence']) ?? null, evidence: str(ev[i]), rationale: (str(sec[i]) as SecurityRationale) ?? null,
        borderline: str(bd[i]), flag: str(fl[i]), partner1: str(p1[i]), partnerGap: typeof gap[i] === 'boolean' ? (gap[i] as boolean) : null,
        momentum: (str(mom[i]) as Momentum) ?? null, momentumQuality: str(mq[i]),
      });
    }
    if (bad) p.push(`core: ${bad} cells with an unknown posture, mode, quadrant or index`);
  }
  const d = isObj(doc.division) ? doc.division : {};
  const dc = (n2: string) => asArr(d[n2]);
  const division: DivisionNode[] = dc('t').map((t, i) => {
    const gapsStr = str(dc('gaps')[i]);
    const gap = str(dc('gap')[i]) ?? 'none';
    if (!GAPS.has(gap)) p.push(`core: division gap '${gap}' is not known`);
    const backups = [1, 2]
      .map((b) => ({ iso3: str(dc(`backup_${b}`)[i]), share: num(dc(`backup_${b}_share`)[i]) }))
      .filter((b): b is { iso3: string; share: number | null } => b.iso3 !== null);
    return {
      tech: techs[num(t) ?? -1] ?? '?', stage: (stages[num(dc('s')[i]) ?? -1] ?? 'Extraction') as PostureStage,
      primary: str(dc('primary')[i]), primaryShare: num(dc('primary_share')[i]), backups,
      redundancy: num(dc('redundancy')[i]) ?? 0, alliedShare: num(dc('allied_share')[i]), coerciveShare: num(dc('coercive_share')[i]),
      leader: str(dc('leader')[i]), leaderShare: num(dc('leader_share')[i]), gap: gap as AibGap,
      gaps: gapsStr ? (gapsStr.split(';') as AibGap[]) : [], source: str(dc('source')[i]), aibNodes: num(dc('aib_nodes')[i]),
      partnerDemand: num(dc('partner_demand')[i]) ?? 0, secureDemand: num(dc('secure_demand')[i]) ?? 0,
      partnerGaps: num(dc('partner_gaps')[i]) ?? 0, memberPartnerDemand: num(dc('member_partner_demand')[i]) ?? 0,
    };
  });
  if (p.length) throw new PostureContractError(p);
  const h = (isObj(doc.aib_header) ? doc.aib_header : {}) as PostureCore['aibHeader'];
  const design = (isObj(doc.design) ? doc.design : {}) as PostureCore['design'];
  return { meta: { ...(meta as PostureMeta), techs, stages }, countries, cells, division, aibHeader: h, design };
}

export function checkPostureDetail(doc: unknown): PostureDetail {
  const p: string[] = [];
  if (checkHeader(doc, 'detail', p)) {
    if (typeof doc.iso3 !== 'string') p.push('detail: no iso3');
    if (!Array.isArray(doc.cells)) p.push('detail: no cells');
    else
      for (const c of doc.cells) {
        if (!isObj(c) || typeof c.tech !== 'string' || !POSTURE_STAGES.includes(c.stage as PostureStage) || !['secure', 'compete', 'deploy'].includes(String(c.posture))) {
          p.push('detail: a cell without tech, six-stage stage or posture');
          break;
        }
      }
  }
  if (p.length) throw new PostureContractError(p);
  return doc as unknown as PostureDetail;
}
