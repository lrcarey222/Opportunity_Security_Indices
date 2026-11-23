// ReviewStore: the reviewer app's data (docs/QC_SYSTEM_DESIGN.md 3, 5): public/data/review/manifest.json
// (no-cache) and the files it names: cases.<campaign>.json, method_cards.json, framework_scenarios.json,
// robustness.json, qc_summary.json and the synthetic fixtures/demo_submissions.json. All of them are built by
// dashboard/scripts/review/build-review-data.mjs from config/review/*.yml and the shipped bundles; nothing
// private is ever here. Same pattern as aib/data/store.ts: one promise per file shared by concurrent callers,
// dropped on failure so a retry refetches, a light shape check per file, setReviewStoreForTests() for tests.
// No React here.
//
// The manifest's campaign entries carry each campaign's own rules (reviewData.ts reviewManifest writes
// `assignment`, `target_reviews_per_anchor`, `case_ids`, `description` and the pre-registered `reviewers` codes from
// config/review/campaigns/*.yml), and campaignOf() applies them. FALLBACK_ASSIGNMENT is used only for an older
// manifest that lacks them (the rules of design 4.1), and the fallback is reported so the page can say so.

import { CASES_SCHEMA, REVIEW_MANIFEST_SCHEMA, type AssignmentRules, type Campaign, type CampaignStatus, type CasesFile, type FrameworkScenario, type MethodCard, type ReviewType, type Submission } from '../model.ts';
import type { ReleaseStamps } from '../release.ts';

export const REVIEW_DATA_DIR = 'data/review/';
export const REVIEW_MANIFEST_FILE = 'manifest.json';
export const DEMO_SUBMISSIONS_FILE = 'fixtures/demo_submissions.json';

export type ReviewLoadCode = 'NO_DATA' | 'CONTRACT' | 'NETWORK';

export class ReviewLoadError extends Error {
  readonly code: ReviewLoadCode;
  constructor(code: ReviewLoadCode, message: string) {
    super(message);
    this.name = 'ReviewLoadError';
    this.code = code;
  }
}

/** One campaign as the review manifest lists it (the optional fields are read when a build writes them). */
export interface ReviewCampaignEntry {
  campaign_id: string;
  name: string;
  status: CampaignStatus;
  opens: string | null;
  closes: string | null;
  review_types: ReviewType[];
  n_cases: number;
  anchor_cases: string[];
  cases_file: string;
  method_card_ids: string[];
  framework_scenario_ids: string[];
  description?: string | null;
  case_ids?: string[];
  target_reviews_per_anchor?: number;
  assignment?: AssignmentRules;
  /** pre-registered reviewer codes (never names) */
  reviewers?: { code: string; groups?: string[] }[];
}

export interface ReviewManifestDoc {
  schema: typeof REVIEW_MANIFEST_SCHEMA;
  schema_version: 1;
  built_at: string;
  app_commit: string;
  data_release: { id: string; stamps: ReleaseStamps; complete: boolean; missing: string[] };
  campaigns: ReviewCampaignEntry[];
  files: { file: string; sha256: string; bytes: number }[];
  qc?: { status?: string; reproduction?: { cells: number; reproduced: number; rate: number } | null; golden?: unknown } | null;
}

export interface MethodCardsDoc {
  schema: string;
  cards: MethodCard[];
}
export interface FrameworkScenariosDoc {
  schema: string;
  scenarios: FrameworkScenario[];
}
export interface DemoSubmissionsDoc {
  schema: string;
  synthetic: true;
  marker: string;
  submissions: Submission[];
}

/** Where the files come from: HTTP in the app, an in-memory map in tests. */
export interface ReviewSource {
  /** A parsed JSON file of data/review/ (path relative to it); throws ReviewLoadError. */
  json(file: string): Promise<unknown>;
}

type FetchLike = (url: string, init?: RequestInit) => Promise<Response>;

/** The review data directory under the app's base URL ("/data/review/", "/explorer/data/review/"). */
export function reviewDataBase(): string {
  const base = (import.meta.env?.BASE_URL as string | undefined) ?? '/';
  return `${base.endsWith('/') ? base : `${base}/`}${REVIEW_DATA_DIR}`;
}

/** The HTTP source (same origin only). The manifest is revalidated; the other files use the default cache. */
export function httpReviewSource(base = reviewDataBase(), opts: { fetch?: FetchLike } = {}): ReviewSource {
  const f: FetchLike = opts.fetch ?? ((u, i) => fetch(u, i));
  return {
    async json(file) {
      const isManifest = file === REVIEW_MANIFEST_FILE;
      const what = `data/review/${file}`;
      let res: Response;
      try {
        res = await f(`${base}${file}`, isManifest ? { cache: 'no-cache' } : undefined);
      } catch (e) {
        throw new ReviewLoadError('NETWORK', `${what} could not be fetched (${String((e as Error)?.message ?? e)}).`);
      }
      if (res.status === 404) throw new ReviewLoadError(isManifest ? 'NO_DATA' : 'NETWORK', `${what} does not exist (HTTP 404).`);
      if (!res.ok) throw new ReviewLoadError('NETWORK', `${what} could not be fetched (HTTP ${res.status}).`);
      const text = await res.text();
      try {
        return JSON.parse(text) as unknown;
      } catch {
        // a dev server's index.html fallback for a missing file is not JSON
        throw new ReviewLoadError(isManifest ? 'NO_DATA' : 'CONTRACT', `${what} is not a JSON file.`);
      }
    },
  };
}

/** A source over an in-memory map of parsed files (tests, the artifact build). A missing file is a 404. */
export function memoryReviewSource(files: Readonly<Record<string, unknown>>): ReviewSource {
  return {
    async json(file) {
      if (!(file in files)) throw new ReviewLoadError(file === REVIEW_MANIFEST_FILE ? 'NO_DATA' : 'NETWORK', `data/review/${file} does not exist (HTTP 404).`);
      return structuredClone(files[file]);
    },
  };
}

const isObj = (x: unknown): x is Record<string, unknown> => typeof x === 'object' && x !== null && !Array.isArray(x);
const isStr = (x: unknown): x is string => typeof x === 'string' && x.length > 0;

/** Problems with a review manifest (empty when usable). */
export function checkReviewManifest(x: unknown): string[] {
  const e: string[] = [];
  if (!isObj(x)) return ['not an object'];
  if (x.schema !== REVIEW_MANIFEST_SCHEMA) e.push(`schema: expected ${REVIEW_MANIFEST_SCHEMA}`);
  if (x.schema_version !== 1) e.push('schema_version: expected 1');
  if (!isObj(x.data_release) || !isStr(x.data_release.id)) e.push('data_release.id: required');
  if (!Array.isArray(x.campaigns)) e.push('campaigns: a list');
  else
    x.campaigns.forEach((c, i) => {
      if (!isObj(c) || !isStr(c.campaign_id) || !isStr(c.cases_file)) e.push(`campaigns[${i}]: {campaign_id, cases_file} required`);
    });
  return e;
}

/** Problems with a cases file (empty when usable): the shape the reviewer pages read. */
export function checkCasesFile(x: unknown): string[] {
  const e: string[] = [];
  if (!isObj(x)) return ['not an object'];
  if (x.schema !== CASES_SCHEMA) e.push(`schema: expected ${CASES_SCHEMA}`);
  if (!isStr(x.campaign_id)) e.push('campaign_id: required');
  if (!Array.isArray(x.cases)) return [...e, 'cases: a list'];
  x.cases.forEach((c, i) => {
    if (!isObj(c) || !isStr(c.case_id)) {
      e.push(`cases[${i}]: case_id required`);
      return;
    }
    const s = c.snapshot;
    if (!isObj(s) || !isObj(s.conclusion) || !isObj(s.facts)) e.push(`${c.case_id}: snapshot {conclusion, facts} required`);
    if (!isStr(c.snapshot_sha256)) e.push(`${c.case_id}: snapshot_sha256 required`);
    if (!Array.isArray(c.answer_options) || c.answer_options.length === 0) e.push(`${c.case_id}: answer_options required`);
    if (!Array.isArray(c.context_fields) || !Array.isArray(c.reveal_fields)) e.push(`${c.case_id}: context_fields and reveal_fields required`);
  });
  return e;
}

/**
 * The assignment rules used only for an older manifest that does not carry a campaign's own (design 4.1; the values
 * of config/review/campaigns/aib_qc_2027_round1.yml when it was first built).
 */
export const FALLBACK_ASSIGNMENT: AssignmentRules = {
  assigned_per_reviewer: { min: 2, max: 4 },
  anchors_per_reviewer: 1,
  out_of_specialty: 'optional',
  allow_self_selected: true,
  min_domain_confidence: 3,
};
export const FALLBACK_TARGET_REVIEWS_PER_ANCHOR = 4;

/** The Campaign of a manifest entry and its cases file; `fallback` lists the rules not read from the build. */
export function campaignOf(entry: ReviewCampaignEntry, cases: Pick<CasesFile, 'cases'> | null): { campaign: Campaign; fallback: string[] } {
  const fallback: string[] = [];
  const assignment = entry.assignment ?? (fallback.push('assignment'), FALLBACK_ASSIGNMENT);
  const target = typeof entry.target_reviews_per_anchor === 'number' ? entry.target_reviews_per_anchor : (fallback.push('target_reviews_per_anchor'), FALLBACK_TARGET_REVIEWS_PER_ANCHOR);
  const caseIds = Array.isArray(entry.case_ids) && entry.case_ids.length ? [...entry.case_ids] : (cases?.cases ?? []).map((c) => c.case_id);
  return {
    campaign: {
      campaign_id: entry.campaign_id,
      name: entry.name,
      status: entry.status,
      opens: entry.opens ?? null,
      closes: entry.closes ?? null,
      description: entry.description ?? null,
      review_types: [...(entry.review_types ?? [])],
      case_ids: caseIds,
      anchor_cases: [...(entry.anchor_cases ?? [])],
      target_reviews_per_anchor: target,
      assignment: { ...assignment, assigned_per_reviewer: { ...assignment.assigned_per_reviewer } },
      method_card_ids: [...(entry.method_card_ids ?? [])],
      framework_scenario_ids: [...(entry.framework_scenario_ids ?? [])],
      ...(Array.isArray(entry.reviewers) && entry.reviewers.length ? { reviewers: entry.reviewers.map((r) => ({ code: r.code, groups: [...(r.groups ?? [])] })) } : {}),
    },
    fallback,
  };
}

/**
 * The campaign a page shows: the one named (?campaign=), else the first active one, else the first listed.
 * Null when the manifest lists none.
 */
export function pickCampaign(m: Pick<ReviewManifestDoc, 'campaigns'>, wanted?: string | null): ReviewCampaignEntry | null {
  const cs = m.campaigns ?? [];
  return (wanted ? cs.find((c) => c.campaign_id === wanted) : undefined) ?? cs.find((c) => c.status === 'active') ?? cs[0] ?? null;
}

/** Any failure as a ReviewLoadError. */
export function toReviewLoadError(e: unknown, what: string): ReviewLoadError {
  if (e instanceof ReviewLoadError) return e;
  return new ReviewLoadError('NETWORK', `${what} could not be loaded (${String((e as Error)?.message ?? e)}).`);
}

export class ReviewStore {
  readonly source: ReviewSource;
  private readonly pending = new Map<string, Promise<unknown>>();
  private readonly done = new Map<string, unknown>();
  /** the files requested, in order (tests) */
  readonly calls: string[] = [];

  constructor(source: ReviewSource = httpReviewSource()) {
    this.source = source;
  }

  /** A loaded file, synchronously (undefined before it settles). */
  peek<T>(file: string): T | undefined {
    return this.done.get(file) as T | undefined;
  }

  private load<T>(file: string, what: string, check: (x: unknown) => string[]): Promise<T> {
    let p = this.pending.get(file) as Promise<T> | undefined;
    if (!p) {
      this.calls.push(file);
      p = this.source
        .json(file)
        .then((x) => {
          const problems = check(x);
          if (problems.length) throw new ReviewLoadError('CONTRACT', `${what} does not match the review contract (${problems.slice(0, 3).join('; ')}).`);
          this.done.set(file, x);
          return x as T;
        })
        .catch((e) => {
          this.pending.delete(file);
          throw toReviewLoadError(e, what);
        });
      this.pending.set(file, p);
    }
    return p;
  }

  manifest(): Promise<ReviewManifestDoc> {
    return this.load(REVIEW_MANIFEST_FILE, 'The review manifest', checkReviewManifest);
  }

  /** The cases file of a campaign entry. */
  cases(entry: Pick<ReviewCampaignEntry, 'cases_file'>): Promise<CasesFile> {
    return this.load(entry.cases_file, 'The review cases', checkCasesFile);
  }

  methodCards(): Promise<MethodCardsDoc> {
    return this.load('method_cards.json', 'The method cards', (x) => (isObj(x) && Array.isArray(x.cards) ? [] : ['cards: a list']));
  }

  frameworkScenarios(): Promise<FrameworkScenariosDoc> {
    return this.load('framework_scenarios.json', 'The framework scenarios', (x) => (isObj(x) && Array.isArray(x.scenarios) ? [] : ['scenarios: a list']));
  }

  robustness(): Promise<unknown> {
    return this.load('robustness.json', 'The robustness map', (x) => (isObj(x) ? [] : ['not an object']));
  }

  qcSummary(): Promise<unknown> {
    return this.load('qc_summary.json', 'The QC summary', (x) => (isObj(x) ? [] : ['not an object']));
  }

  /** The synthetic demo submissions (the admin's demo; never real reviewer data). */
  demoSubmissions(): Promise<DemoSubmissionsDoc> {
    return this.load(DEMO_SUBMISSIONS_FILE, 'The demo submissions', (x) =>
      isObj(x) && x.synthetic === true && Array.isArray(x.submissions) ? [] : ['a synthetic {submissions} document required'],
    );
  }
}

let shared: ReviewStore | null = null;
let forTests: ReviewStore | null = null;

/** The page-wide store (tests can swap it). */
export function reviewStore(): ReviewStore {
  if (forTests) return forTests;
  if (!shared) shared = new ReviewStore();
  return shared;
}

export function setReviewStoreForTests(s: ReviewStore | null): void {
  forTests = s;
}
