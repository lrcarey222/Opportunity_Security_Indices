// Reviewer privacy (review/privacy.ts): the synchronous SHA-256 agrees with Web Crypto, pseudonymous
// codes, and names / organizations dropped from exports unless asked for, never for an anonymous reviewer.

import { describe, expect, it } from 'vitest';
import type { Submission } from './model.ts';
import { sha256Hex } from './release.ts';
import { canonicalJson, exportableName, pseudonymReviewerCode, redactSubmission, sha256HexSync } from './privacy.ts';

const sub = (reviewer: Record<string, unknown>) => ({ submission_id: 'SUB-1', reviewer: { reviewer_id: 'rev-1', profile_digest: 'd', ...reviewer }, payload: {} }) as unknown as Submission;

describe('sha256HexSync', () => {
  it('matches the FIPS test vectors and Web Crypto on multi-block and non-ASCII input', async () => {
    expect(sha256HexSync('')).toBe('e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855');
    expect(sha256HexSync('abc')).toBe('ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad');
    for (const s of ['x'.repeat(55), 'x'.repeat(56), 'x'.repeat(64), 'x'.repeat(1000), 'Économie — 経済 🔋']) expect(sha256HexSync(s)).toBe(await sha256Hex(s));
  });
  it('canonicalJson sorts keys recursively and drops undefined', () => {
    expect(canonicalJson({ b: 1, a: { d: [3, { z: 1, y: 2 }], c: undefined } })).toBe('{"a":{"d":[3,{"y":2,"z":1}]},"b":1}');
  });
});

describe('pseudonymReviewerCode', () => {
  it('is R- and 8 upper-case hex of sha256(reviewer_id), stable', () => {
    const c = pseudonymReviewerCode('rev-1');
    expect(c).toMatch(/^R-[0-9A-F]{8}$/);
    expect(c).toBe(`R-${sha256HexSync('rev-1').slice(0, 8).toUpperCase()}`);
    expect(pseudonymReviewerCode('rev-2')).not.toBe(c);
  });
});

describe('redactSubmission', () => {
  it('drops the name and organization by default, keeps them only with includeNames', () => {
    const s = sub({ name: 'Ada Example', organization: 'Ministry', anonymous: false });
    const r = redactSubmission(s);
    expect(r.reviewer).not.toHaveProperty('name');
    expect(r.reviewer).not.toHaveProperty('organization');
    expect(r.reviewer.reviewer_id).toBe('rev-1');
    expect(s.reviewer.name).toBe('Ada Example');
    expect(redactSubmission(s, { includeNames: true }).reviewer.name).toBe('Ada Example');
    expect(exportableName(s)).toBeNull();
    expect(exportableName(s, { includeNames: true })).toBe('Ada Example');
  });
  it('never exports an anonymous reviewer name, even with includeNames', () => {
    const s = sub({ name: 'Hidden Person', anonymous: true });
    expect(redactSubmission(s, { includeNames: true }).reviewer).not.toHaveProperty('name');
    expect(exportableName(s, { includeNames: true })).toBeNull();
    expect(JSON.stringify(redactSubmission(s, { includeNames: true }))).not.toMatch(/Hidden Person/);
  });
});
