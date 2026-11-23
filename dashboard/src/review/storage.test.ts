// Reviewer state in the browser (review/storage.ts): namespaced keys, drafts per (campaign, kind, id),
// submissions and adjudications, and the in-memory fallback when storage is missing or throws. Never throws.

import { describe, expect, it } from 'vitest';
import type { Adjudication, ReviewerProfile, Submission } from './model.ts';
import { STORAGE_PREFIX, createReviewStorage, type KeyValueBackend } from './storage.ts';

class MemoryBackend implements KeyValueBackend {
  m = new Map<string, string>();
  get length() {
    return this.m.size;
  }
  key(i: number) {
    return [...this.m.keys()][i] ?? null;
  }
  getItem(k: string) {
    return this.m.get(k) ?? null;
  }
  setItem(k: string, v: string) {
    this.m.set(k, v);
  }
  removeItem(k: string) {
    this.m.delete(k);
  }
}

/** A backend whose every call throws (blocked storage, an artifact sandbox). */
const throwing: KeyValueBackend = {
  get length(): number {
    throw new Error('SecurityError');
  },
  key() {
    throw new Error('SecurityError');
  },
  getItem() {
    throw new Error('SecurityError');
  },
  setItem() {
    throw new Error('QuotaExceededError');
  },
  removeItem() {
    throw new Error('SecurityError');
  },
};

const profile: ReviewerProfile = {
  reviewer_id: 'r1',
  name: 'A Reviewer',
  anonymous: false,
  countries: ['KOR'],
  sectors: ['batteries'],
  stages: [],
  methods: [],
  confidence: { 'sector:batteries': 5 },
  familiarity_neis: 2,
  familiarity_explorer: 1,
  advised_on: ['governments'],
  created_at: '2027-01-01T00:00:00Z',
};
const sub = (id: string, campaign: string | null, at: string) => ({ submission_id: id, campaign_id: campaign, created_at: at }) as unknown as Submission;

describe('createReviewStorage', () => {
  it('saves and loads the profile, drafts, submissions and adjudications under the namespace', () => {
    const b = new MemoryBackend();
    b.setItem('other-app', 'x');
    const s = createReviewStorage(b);
    expect(s.saveProfile(profile)).toBe(true);
    expect(s.loadProfile()).toEqual(profile);
    expect(s.saveDraft('round:1', 'case', 'CASE_001', { step: 'blind' })).toBe(true);
    expect(s.loadDraft('round:1', 'case', 'CASE_001')).toEqual({ step: 'blind' });
    expect(s.loadDraft('round:1', 'case', 'CASE_002')).toBeNull();
    s.saveDraft('round:1', 'card', 'MC_001', { stance: 'modify' });
    s.saveDraft('round2', 'case', 'CASE_001', {});
    expect(s.listDrafts('round:1').map((d) => [d.campaign_id, d.kind, d.id])).toEqual([
      ['round:1', 'card', 'MC_001'],
      ['round:1', 'case', 'CASE_001'],
    ]);
    expect(s.listDrafts()).toHaveLength(3);
    s.saveSubmission(sub('SUB-2', 'round2', '2027-01-02T00:00:00Z'));
    s.saveSubmission(sub('SUB-1', 'round:1', '2027-01-01T00:00:00Z'));
    expect(s.listSubmissions().map((x) => x.submission_id)).toEqual(['SUB-1', 'SUB-2']);
    expect(s.listSubmissions('round2').map((x) => x.submission_id)).toEqual(['SUB-2']);
    s.saveAdjudication({ adjudication_id: 'ADJ-1', submission_id: 'SUB-1' } as Adjudication);
    expect(s.listAdjudications()).toHaveLength(1);
    for (const k of b.m.keys()) if (k !== 'other-app') expect(k.startsWith(STORAGE_PREFIX)).toBe(true);
    s.deleteDraft('round:1', 'card', 'MC_001');
    expect(s.loadDraft('round:1', 'card', 'MC_001')).toBeNull();
    s.clearAll();
    expect(s.loadProfile()).toBeNull();
    expect(s.listSubmissions()).toEqual([]);
    expect(b.getItem('other-app')).toBe('x');
    expect(s.persistent()).toBe(true);
  });

  it('survives a backend that throws on every call, keeping values in memory', () => {
    const s = createReviewStorage(throwing);
    expect(() => s.loadProfile()).not.toThrow();
    expect(s.loadProfile()).toBeNull();
    expect(s.saveProfile(profile)).toBe(false);
    expect(s.persistent()).toBe(false);
    expect(s.loadProfile()).toEqual(profile);
    expect(s.saveDraft('c', 'case', 'CASE_001', { a: 1 })).toBe(false);
    expect(s.loadDraft('c', 'case', 'CASE_001')).toEqual({ a: 1 });
    expect(s.listDrafts('c')).toHaveLength(1);
    s.saveSubmission(sub('SUB-1', 'c', '2027-01-01T00:00:00Z'));
    expect(s.listSubmissions('c')).toHaveLength(1);
    expect(() => s.deleteDraft('c', 'case', 'CASE_001')).not.toThrow();
    expect(s.loadDraft('c', 'case', 'CASE_001')).toBeNull();
    expect(() => s.clearAll()).not.toThrow();
  });

  it('works with no backend at all and falls back when only writes fail (quota)', () => {
    const none = createReviewStorage(null);
    expect(none.persistent()).toBe(false);
    none.saveDraft('c', 'case', 'X', 1);
    expect(none.loadDraft('c', 'case', 'X')).toBe(1);
    const b = new MemoryBackend();
    const full = createReviewStorage({ ...b, get length() { return b.length; }, key: (i) => b.key(i), getItem: (k) => b.getItem(k), removeItem: (k) => b.removeItem(k), setItem: () => { throw new Error('QuotaExceededError'); } });
    expect(full.saveDraft('c', 'case', 'X', { v: 2 })).toBe(false);
    expect(full.loadDraft('c', 'case', 'X')).toEqual({ v: 2 });
  });

  it('ignores corrupt JSON and values that cannot be serialised', () => {
    const b = new MemoryBackend();
    b.setItem(`${STORAGE_PREFIX}profile`, '{not json');
    const s = createReviewStorage(b);
    expect(s.loadProfile()).toBeNull();
    const cyclic: Record<string, unknown> = {};
    cyclic.self = cyclic;
    expect(s.saveDraft('c', 'case', 'X', cyclic)).toBe(false);
  });
});
