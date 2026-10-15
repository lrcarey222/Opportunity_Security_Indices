import { describe, expect, it } from 'vitest';
import { TRI } from '../../theme/palette.ts';
import { contrast, sideWords, textOn, tileScale, triTile } from './contrast.ts';

describe('tile colours on the three-colour scale (C1.5, 7)', () => {
  it('every TRI step gets a text colour at >= 4.5:1', () => {
    for (const bg of TRI) expect(contrast(bg, textOn(bg)), bg).toBeGreaterThanOrEqual(4.5);
  });
  it('the median is white, the ends orange and blue, the side in words', () => {
    const s = tileScale([0.1, 0.2, 0.3, 0.4, 0.5]);
    expect(triTile(s, 0.3)?.bg.toLowerCase()).toBe(TRI[4].toLowerCase());
    expect(triTile(s, 0.3)?.side).toBe('median');
    expect(triTile(s, 0.1)?.bg.toLowerCase()).toBe(TRI[0].toLowerCase());
    expect(triTile(s, 0.5)?.bg.toLowerCase()).toBe(TRI[8].toLowerCase());
    expect(triTile(s, 0.5)?.side).toBe('above');
    expect(triTile(s, NaN)).toBeNull();
    expect(sideWords('above', 5)).toBe('above the median of the 5 tiles shown');
    expect(sideWords('median', 5, 'sectors')).toBe('at the median of the 5 sectors shown');
  });
  it('reverse puts the low values on the blue end (effective number of suppliers)', () => {
    const s = tileScale([1, 2, 3], { reverse: true });
    expect(triTile(s, 1)?.bg.toLowerCase()).toBe(TRI[8].toLowerCase());
  });
});
