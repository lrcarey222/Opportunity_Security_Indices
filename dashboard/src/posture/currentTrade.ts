// The words for a cell's current-trade refresh (scripts/26 current_trade; R posture_detail_doc current_trade):
// China's import share in the base year and the latest 12 months, where the latest figures came from, and the
// cells whose China share moved enough to flag. Pure: no React.

import { pct } from './derive.ts';
import { monthLabel, type CurrentTrade, type NodeDetail } from './detail.ts';

/** "44% in the 12 months to Aug 2026, down from 69% in 2024"; null without both shares. */
export function chinaShareWords(ct: CurrentTrade | null, baseYear = 2024): string | null {
  if (!ct || ct.chinaShareLatest === null || ct.chinaShareBase === null) return null;
  const to = monthLabel(ct.windowEnd);
  const window = to ? `in the 12 months to ${to}` : 'in the latest 12 months';
  const a = Math.round(ct.chinaShareBase * 100);
  const b = Math.round(ct.chinaShareLatest * 100);
  const move = b === a ? `unchanged from ${baseYear}` : `${b > a ? 'up' : 'down'} from ${a}% in ${baseYear}`;
  return `${pct(ct.chinaShareLatest)} ${window}, ${move}`;
}

/** Where the latest figures came from and how the assessment uses them. */
export function currentTradeSourceWords(ct: CurrentTrade | null, baseYear = 2024): string | null {
  if (!ct) return null;
  const w = ct.weight !== null ? Math.round(ct.weight * 100) : null;
  const use = w === null ? '' : w === 50 ? `; the assessment weighs them equally with ${baseYear}` : `; the assessment gives them ${w}% weight against ${baseYear}`;
  if (ct.source === 'ember_fill') {
    return `Estimated: China's leg from Ember's China export data, other suppliers from their own monthly reports (this country publishes no monthly data)${use}.`;
  }
  return `From the country's own monthly customs reports${use}.`;
}

export interface TradeShift {
  /** "Tech|Stage", the map's selection key */
  key: string;
  tech: string;
  stage: string;
  base: number;
  latest: number;
  direction: 'up' | 'down';
  windowEnd: number | null;
  source: CurrentTrade['source'];
}

/** The cells flagged for a 10+ point move in China's import share, largest move first. */
export function tradeShifts(cells: readonly NodeDetail[]): TradeShift[] {
  const out: TradeShift[] = [];
  for (const c of cells) {
    const ct = c.supplyChain?.currentTrade;
    if (!ct?.flag || ct.chinaShareBase === null || ct.chinaShareLatest === null) continue;
    out.push({
      key: c.key,
      tech: c.tech,
      stage: c.stage,
      base: ct.chinaShareBase,
      latest: ct.chinaShareLatest,
      direction: ct.flag === 'china_up' ? 'up' : 'down',
      windowEnd: ct.windowEnd,
      source: ct.source,
    });
  }
  return out.sort((a, b) => Math.abs(b.latest - b.base) - Math.abs(a.latest - a.base));
}
