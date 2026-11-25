// Reviewer state in the browser (docs/QC_SYSTEM_DESIGN.md 5 step 8, 7): the profile, case and card drafts,
// local submissions and (admin) adjudications, under the namespace 'aibx-review:v1:'. Every access is
// wrapped in try/catch: when localStorage is missing, blocked (private mode, an artifact sandbox) or full,
// values live in an in-memory map for the page life and `persistent()` says so. Never throws.
// UX v2 adds keys beside the v1 ones (whose names and formats are unchanged): v2 case drafts under the draft kind
// 'case_v2' (a v1 draft under 'case' is still read, for migration), per-case reviewer notes, a per-campaign
// calibration-seen time and the reviewer's review mode.

import type { Adjudication, ReviewerNote, ReviewerProfile, ReviewMode, Submission } from './model.ts';
import { sanitizeNotes } from './flow.ts';

export const STORAGE_PREFIX = 'aibx-review:v1:';
/** The draft kind of a v1 case flow state (unchanged) and of a v2 one. */
export const CASE_DRAFT_KIND_V1 = 'case';
export const CASE_DRAFT_KIND_V2 = 'case_v2';
/** The calibration screen is per campaign; this id stands for "any campaign". */
export const ANY_CAMPAIGN = '*';

/** The subset of the Web Storage API used here (tests pass a fake). */
export interface KeyValueBackend {
  readonly length: number;
  key(i: number): string | null;
  getItem(k: string): string | null;
  setItem(k: string, v: string): void;
  removeItem(k: string): void;
}

export interface DraftRef {
  campaign_id: string;
  kind: string;
  id: string;
  key: string;
}

export interface ReviewStorage {
  /** false once any write fell back to memory (the reviewer should export a packet before leaving). */
  persistent(): boolean;
  loadProfile(): ReviewerProfile | null;
  saveProfile(p: ReviewerProfile): boolean;
  clearProfile(): void;
  loadDraft<T = unknown>(campaignId: string, kind: string, id: string): T | null;
  saveDraft(campaignId: string, kind: string, id: string, data: unknown): boolean;
  deleteDraft(campaignId: string, kind: string, id: string): void;
  listDrafts(campaignId?: string): DraftRef[];
  saveSubmission(s: Submission): boolean;
  listSubmissions(campaignId?: string | null): Submission[];
  deleteSubmission(submissionId: string): void;
  saveAdjudication(a: Adjudication): boolean;
  listAdjudications(): Adjudication[];
  deleteAdjudication(adjudicationId: string): void;
  /** UX v2: the case draft to restore (the v2 draft, else the v1 one for migration; raw, checked by flow.ts restoreFlowStateV2). */
  loadCaseDraft(campaignId: string, caseId: string): unknown | null;
  /** UX v2: save a v2 case draft (never overwrites the v1 draft key). */
  saveCaseDraftV2(campaignId: string, caseId: string, state: unknown): boolean;
  /** Remove a case's drafts (v2 and v1), e.g. to start the case again. */
  deleteCaseDraft(campaignId: string, caseId: string): void;
  /** UX v2: the reviewer's notes on a case, sanitised ([] when none or unreadable). */
  loadNotes(campaignId: string, caseId: string): ReviewerNote[];
  saveNotes(campaignId: string, caseId: string, notes: readonly ReviewerNote[]): boolean;
  deleteNotes(campaignId: string, caseId: string): void;
  /** UX v2: when the calibration screen was seen (ISO time), or null. */
  loadCalibrationSeen(campaignId?: string): string | null;
  markCalibrationSeen(at: string, campaignId?: string): boolean;
  /** UX v2: the review mode the reviewer chose (null when never chosen). */
  loadReviewMode(): ReviewMode | null;
  saveReviewMode(mode: ReviewMode): boolean;
  /** Remove every key of this namespace (and the memory fallback). */
  clearAll(): void;
}

const enc = encodeURIComponent;
const PROFILE = `${STORAGE_PREFIX}profile`;
const draftKey = (c: string, k: string, i: string) => `${STORAGE_PREFIX}draft:${enc(c)}:${enc(k)}:${enc(i)}`;
const subKey = (id: string) => `${STORAGE_PREFIX}sub:${enc(id)}`;
const adjKey = (id: string) => `${STORAGE_PREFIX}adj:${enc(id)}`;
const notesKey = (c: string, i: string) => `${STORAGE_PREFIX}notes:${enc(c)}:${enc(i)}`;
const calibrationKey = (c: string) => `${STORAGE_PREFIX}calibration_seen:${enc(c)}`;
const REVIEW_MODE = `${STORAGE_PREFIX}review_mode`;

function defaultBackend(): KeyValueBackend | null {
  try {
    const ls = (globalThis as { localStorage?: KeyValueBackend }).localStorage;
    return ls ?? null;
  } catch {
    return null;
  }
}

/** A review store over `backend` (default: window.localStorage when reachable), with an in-memory fallback. */
export function createReviewStorage(backend: KeyValueBackend | null = defaultBackend()): ReviewStorage {
  const memory = new Map<string, string>();
  const removed = new Set<string>();
  let durable = backend !== null;

  const get = (k: string): string | null => {
    if (memory.has(k)) return memory.get(k) ?? null;
    if (removed.has(k)) return null;
    try {
      return backend ? backend.getItem(k) : null;
    } catch {
      return null;
    }
  };
  const set = (k: string, v: string): boolean => {
    removed.delete(k);
    try {
      if (!backend) throw new Error('no storage');
      backend.setItem(k, v);
      memory.delete(k);
      return true;
    } catch {
      durable = false;
      memory.set(k, v);
      return false;
    }
  };
  const del = (k: string): void => {
    memory.delete(k);
    try {
      backend?.removeItem(k);
    } catch {
      removed.add(k);
    }
  };
  const keys = (prefix: string): string[] => {
    const out = new Set<string>();
    try {
      if (backend)
        for (let i = 0; i < backend.length; i++) {
          const k = backend.key(i);
          if (k && k.startsWith(prefix) && !removed.has(k)) out.add(k);
        }
    } catch {
      // unreadable backend: the memory keys are all there is
    }
    for (const k of memory.keys()) if (k.startsWith(prefix)) out.add(k);
    return [...out].sort();
  };
  const read = <T>(k: string): T | null => {
    const raw = get(k);
    if (raw === null) return null;
    try {
      return JSON.parse(raw) as T;
    } catch {
      return null;
    }
  };
  const write = (k: string, v: unknown): boolean => {
    let text: string;
    try {
      text = JSON.stringify(v);
    } catch {
      return false;
    }
    return set(k, text);
  };

  return {
    persistent: () => durable,
    loadProfile: () => read<ReviewerProfile>(PROFILE),
    saveProfile: (p) => write(PROFILE, p),
    clearProfile: () => del(PROFILE),
    loadDraft: <T>(c: string, k: string, i: string) => read<T>(draftKey(c, k, i)),
    saveDraft: (c, k, i, data) => write(draftKey(c, k, i), data),
    deleteDraft: (c, k, i) => del(draftKey(c, k, i)),
    listDrafts: (c) =>
      keys(c === undefined ? `${STORAGE_PREFIX}draft:` : `${STORAGE_PREFIX}draft:${enc(c)}:`).map((key) => {
        const [campaign_id, kind, id] = key
          .slice(`${STORAGE_PREFIX}draft:`.length)
          .split(':')
          .map((x) => {
            try {
              return decodeURIComponent(x);
            } catch {
              return x;
            }
          });
        return { campaign_id, kind, id, key };
      }),
    saveSubmission: (s) => write(subKey(s.submission_id), s),
    listSubmissions: (c) =>
      keys(`${STORAGE_PREFIX}sub:`)
        .map((k) => read<Submission>(k))
        .filter((s): s is Submission => !!s && typeof s === 'object' && (c === undefined || s.campaign_id === c))
        .sort((a, b) => (a.created_at < b.created_at ? -1 : a.created_at > b.created_at ? 1 : 0)),
    deleteSubmission: (id) => del(subKey(id)),
    saveAdjudication: (a) => write(adjKey(a.adjudication_id), a),
    listAdjudications: () =>
      keys(`${STORAGE_PREFIX}adj:`)
        .map((k) => read<Adjudication>(k))
        .filter((a): a is Adjudication => !!a && typeof a === 'object'),
    deleteAdjudication: (id) => del(adjKey(id)),
    loadCaseDraft: (c, i) => read<unknown>(draftKey(c, CASE_DRAFT_KIND_V2, i)) ?? read<unknown>(draftKey(c, CASE_DRAFT_KIND_V1, i)),
    saveCaseDraftV2: (c, i, state) => write(draftKey(c, CASE_DRAFT_KIND_V2, i), state),
    deleteCaseDraft: (c, i) => {
      del(draftKey(c, CASE_DRAFT_KIND_V2, i));
      del(draftKey(c, CASE_DRAFT_KIND_V1, i));
    },
    loadNotes: (c, i) => sanitizeNotes(read<unknown>(notesKey(c, i))),
    saveNotes: (c, i, notes) => write(notesKey(c, i), sanitizeNotes(notes)),
    deleteNotes: (c, i) => del(notesKey(c, i)),
    loadCalibrationSeen: (c = ANY_CAMPAIGN) => {
      const v = read<unknown>(calibrationKey(c));
      return typeof v === 'string' && v ? v : null;
    },
    markCalibrationSeen: (at, c = ANY_CAMPAIGN) => write(calibrationKey(c), at),
    loadReviewMode: () => {
      const v = read<unknown>(REVIEW_MODE);
      return v === 'decision' || v === 'technical' ? v : null;
    },
    saveReviewMode: (mode) => (mode === 'decision' || mode === 'technical' ? write(REVIEW_MODE, mode) : false),
    clearAll: () => {
      for (const k of keys(STORAGE_PREFIX)) del(k);
      memory.clear();
    },
  };
}

let shared: ReviewStorage | null = null;
/** The page's review store (created on first use). */
export function reviewStorage(): ReviewStorage {
  shared ??= createReviewStorage();
  return shared;
}
/** The review mode in force: the stored choice, else the profile's, else decision review. */
export function effectiveReviewMode(store: Pick<ReviewStorage, 'loadReviewMode'>, profile?: Pick<ReviewerProfile, 'review_mode'> | null): ReviewMode {
  return store.loadReviewMode() ?? (profile?.review_mode === 'technical' ? 'technical' : 'decision');
}

/** Tests: replace (or with null, reset) the shared store. */
export function setReviewStorage(s: ReviewStorage | null): void {
  shared = s;
}
