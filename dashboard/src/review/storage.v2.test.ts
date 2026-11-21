// UX v2 storage (review/storage.ts): v2 case drafts beside v1 ones (v1 keys untouched), notes, the calibration
// flag and the review mode; never throws, with the in-memory fallback.

import { describe, expect, it } from 'vitest';
import { initialFlowState, initialFlowStateV2, restoreFlowStateV2 } from './flow.ts';
import { ANY_CAMPAIGN, CASE_DRAFT_KIND_V1, CASE_DRAFT_KIND_V2, STORAGE_PREFIX, createReviewStorage, effectiveReviewMode, type KeyValueBackend } from './storage.ts';

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

const C = { case_id: 'CASE_001', kind: 'posture_cell' as const };
const T0 = '2027-01-20T09:00:00.000Z';

describe('v2 case drafts', () => {
  it('saves v2 drafts under their own key and never touches the v1 draft', () => {
    const b = new MemoryBackend();
    const st = createReviewStorage(b);
    const v1 = initialFlowState({ case_id: 'CASE_001' });
    st.saveDraft('camp', CASE_DRAFT_KIND_V1, 'CASE_001', v1);
    const v1Raw = b.getItem(`${STORAGE_PREFIX}draft:camp:case:CASE_001`);
    // no v2 draft yet: the v1 draft is what loads (for migration)
    expect(restoreFlowStateV2(st.loadCaseDraft('camp', 'CASE_001'), C)?.migrated_from).toBe(1);
    const v2 = initialFlowStateV2(C);
    expect(st.saveCaseDraftV2('camp', 'CASE_001', v2)).toBe(true);
    expect(b.getItem(`${STORAGE_PREFIX}draft:camp:${CASE_DRAFT_KIND_V2}:CASE_001`)).toBe(JSON.stringify(v2));
    expect(b.getItem(`${STORAGE_PREFIX}draft:camp:case:CASE_001`)).toBe(v1Raw);
    expect(st.loadCaseDraft('camp', 'CASE_001')).toEqual(v2);
    expect(st.listDrafts('camp').map((d) => d.kind).sort()).toEqual([CASE_DRAFT_KIND_V1, CASE_DRAFT_KIND_V2]);
    st.deleteCaseDraft('camp', 'CASE_001');
    expect(st.loadCaseDraft('camp', 'CASE_001')).toBeNull();
  });
});

describe('notes, calibration and review mode', () => {
  it('round-trips sanitised notes per case', () => {
    const st = createReviewStorage(new MemoryBackend());
    expect(st.loadNotes('camp', 'CASE_001')).toEqual([]);
    st.saveNotes('camp', 'CASE_001', [{ id: 'n1', text: 'x', created_at: T0, updated_at: T0, converted_to: 'comment' }, { id: 'n1', text: 'dup', created_at: T0, updated_at: T0 }]);
    expect(st.loadNotes('camp', 'CASE_001')).toEqual([{ id: 'n1', text: 'x', created_at: T0, updated_at: T0, converted_to: 'comment' }]);
    expect(st.loadNotes('camp', 'CASE_002')).toEqual([]);
    st.deleteNotes('camp', 'CASE_001');
    expect(st.loadNotes('camp', 'CASE_001')).toEqual([]);
  });

  it('a corrupted notes value loads as no notes', () => {
    const b = new MemoryBackend();
    b.setItem(`${STORAGE_PREFIX}notes:camp:CASE_001`, '{not json');
    expect(createReviewStorage(b).loadNotes('camp', 'CASE_001')).toEqual([]);
    b.setItem(`${STORAGE_PREFIX}notes:camp:CASE_001`, '{"id":"x"}');
    expect(createReviewStorage(b).loadNotes('camp', 'CASE_001')).toEqual([]);
  });

  it('records when the calibration screen was seen, per campaign or for any campaign', () => {
    const st = createReviewStorage(new MemoryBackend());
    expect(st.loadCalibrationSeen('camp')).toBeNull();
    st.markCalibrationSeen(T0, 'camp');
    expect(st.loadCalibrationSeen('camp')).toBe(T0);
    expect(st.loadCalibrationSeen('other')).toBeNull();
    st.markCalibrationSeen(T0);
    expect(st.loadCalibrationSeen(ANY_CAMPAIGN)).toBe(T0);
  });

  it('stores the review mode; the effective mode falls back to the profile and then to decision review', () => {
    const st = createReviewStorage(new MemoryBackend());
    expect(st.loadReviewMode()).toBeNull();
    expect(effectiveReviewMode(st, null)).toBe('decision');
    expect(effectiveReviewMode(st, { review_mode: 'technical' })).toBe('technical');
    expect(st.saveReviewMode('bogus' as never)).toBe(false);
    st.saveReviewMode('decision');
    expect(effectiveReviewMode(st, { review_mode: 'technical' })).toBe('decision');
  });

  it('never throws on a blocked backend and keeps v2 values in memory; clearAll removes them', () => {
    const st = createReviewStorage(throwing);
    expect(st.saveCaseDraftV2('camp', 'CASE_001', initialFlowStateV2(C))).toBe(false);
    expect(st.persistent()).toBe(false);
    expect((st.loadCaseDraft('camp', 'CASE_001') as { version: number }).version).toBe(2);
    st.saveNotes('camp', 'CASE_001', [{ id: 'n1', text: 'x', created_at: T0, updated_at: T0 }]);
    expect(st.loadNotes('camp', 'CASE_001')).toHaveLength(1);
    st.markCalibrationSeen(T0);
    st.saveReviewMode('technical');
    expect(st.loadReviewMode()).toBe('technical');
    st.clearAll();
    expect(st.loadNotes('camp', 'CASE_001')).toEqual([]);
    expect(st.loadCalibrationSeen()).toBeNull();
    expect(st.loadReviewMode()).toBeNull();
  });
});
