import { describe, expect, it } from 'vitest';
import { chinaShareWords, currentTradeSourceWords, tradeShifts } from '../currentTrade.ts';
import { monthLabel, nodeDetail, type CurrentTrade, type NodeDetail } from '../detail.ts';

const ct = (over: Partial<CurrentTrade> = {}): CurrentTrade => ({
  source: 'own_monthly', windowEnd: 202608, weight: 0.5, chinaShareBase: 0.689, chinaShareLatest: 0.438,
  coerciveShareBase: 0.7, coerciveShareLatest: 0.47, flag: 'china_down', ...over,
});

describe('current trade', () => {
  it('labels YYYYMM months', () => {
    expect(monthLabel(202608)).toBe('Aug 2026');
    expect(monthLabel(202513)).toBeNull();
    expect(monthLabel(null)).toBeNull();
  });

  it('describes the China share move and its source', () => {
    expect(chinaShareWords(ct())).toBe('44% in the 12 months to Aug 2026, down from 69% in 2024');
    expect(chinaShareWords(ct({ chinaShareLatest: 0.8, chinaShareBase: 0.5 }))).toBe('80% in the 12 months to Aug 2026, up from 50% in 2024');
    expect(chinaShareWords(ct({ chinaShareLatest: 0.5, chinaShareBase: 0.501 }))).toBe('50% in the 12 months to Aug 2026, unchanged from 2024');
    expect(chinaShareWords(null)).toBeNull();
    expect(chinaShareWords(ct({ chinaShareBase: null }))).toBeNull();
    expect(currentTradeSourceWords(ct())).toContain('own monthly customs reports; the assessment weighs them equally with 2024');
    expect(currentTradeSourceWords(ct({ source: 'ember_fill', weight: 1 }))).toMatch(/^Estimated: .*100% weight against 2024\.$/);
  });

  it('parses the export block and lists flagged steps, largest move first', () => {
    const raw = (tech: string, stage: string, block: unknown) => ({ tech, stage, posture: 'watch', need_type: 'supply_chain', current_trade: block });
    const cells = [
      raw('Batteries', 'Assembly', { source: 'own_monthly', window_end: 202608, weight: 0.5, china_share_base: 0.689, china_share_latest: 0.438, flag: 'china_down' }),
      raw('Solar', 'Assembly', { source: 'ember_fill', window_end: 202608, china_share_base: 0.5, china_share_latest: 0.62, flag: 'china_up' }),
      raw('Wind', 'Assembly', { source: 'own_monthly', window_end: 202608, china_share_base: 0.2, china_share_latest: 0.22 }),
      raw('Coal', 'Assembly', undefined),
    ].map((r) => nodeDetail(r)) as NodeDetail[];
    expect(cells[0].supplyChain?.currentTrade?.flag).toBe('china_down');
    expect(cells[3].supplyChain?.currentTrade).toBeNull();
    const s = tradeShifts(cells);
    expect(s.map((x) => x.tech)).toEqual(['Batteries', 'Solar']);
    expect(s[0].direction).toBe('down');
    expect(s[1].source).toBe('ember_fill');
  });
});
