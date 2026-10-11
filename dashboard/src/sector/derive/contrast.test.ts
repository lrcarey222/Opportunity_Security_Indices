import { describe, expect, it } from 'vitest';
import { SEQ } from '../../theme/palette.ts';
import { contrast, seqStep, seqTile, textOn } from './contrast.ts';

describe('Atlas tile text contrast (4.1, 7)', () => {
  it('every SEQ step gets a text colour at >= 4.5:1', () => {
    for (const bg of SEQ) expect(contrast(bg, textOn(bg)), bg).toBeGreaterThanOrEqual(4.5);
  });
  it('the lightest steps use ink-900 and the darkest white', () => {
    expect(textOn(SEQ[SEQ.length - 1])).toBe('#111827');
    expect(textOn(SEQ[0])).toBe('#ffffff');
  });
  it('values map to discrete steps', () => {
    expect(seqStep(0)).toBe(0);
    expect(seqStep(1)).toBe(SEQ.length - 1);
    expect(seqStep(NaN)).toBe(-1);
    expect(seqTile(0.5)?.step).toBe(3);
    expect(seqTile(0, 0, 1, true)?.step).toBe(SEQ.length - 1);
  });
});
