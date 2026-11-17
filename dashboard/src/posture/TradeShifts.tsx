// The steps where China's share of the economy's imports moved 10+ points between the base year and the latest
// 12 months of trade (scripts/26 current_trade_flag), under the Country Strategy Map. Each item opens its step
// in the map's drawer. Renders nothing for an economy without flagged steps or before its detail loads.

import { stageDisplay } from '../lib/stages.ts';
import { pct } from './derive.ts';
import type { PostureDetail } from './contract.ts';
import { tradeShifts } from './currentTrade.ts';
import { monthLabel } from './detail.ts';

export function TradeShifts({ detail, onSelect, max = 6 }: { detail: PostureDetail | null; onSelect?: (key: string) => void; max?: number }) {
  if (!detail) return null;
  const shifts = tradeShifts(detail.cells);
  if (!shifts.length) return null;
  const to = monthLabel(shifts[0].windowEnd);
  const shown = shifts.slice(0, max);
  return (
    <section className="mt-4 rounded border border-paper-rule p-3 text-sm" data-testid="trade-shifts" aria-labelledby="trade-shifts-title">
      <h4 id="trade-shifts-title" className="font-semibold">
        Recent trade shifts{to ? ` (12 months to ${to})` : ''}
      </h4>
      <p className="mt-1 text-xs text-paper-mute">
        Steps where China&apos;s share of imports moved by 10 points or more since 2024, on at least $100m of imports. The assessment blends the latest 12 months
        with 2024.
      </p>
      <ul className="mt-2 space-y-1">
        {shown.map((s) => (
          <li key={s.key}>
            <button type="button" className="text-left underline-offset-2 hover:underline" onClick={() => onSelect?.(s.key)} data-testid="trade-shift">
              <span className="font-medium">
                {s.tech}, {stageDisplay(s.stage)}
              </span>
              : China {pct(s.base)} → {pct(s.latest)}
              <span className="sr-only">{s.direction === 'up' ? ' (up)' : ' (down)'}</span>
              {s.source === 'ember_fill' && <span className="text-xs text-paper-mute"> (estimated)</span>}
            </button>
          </li>
        ))}
      </ul>
      {shifts.length > shown.length && <p className="mt-1 text-xs text-paper-mute">and {shifts.length - shown.length} more; open a step on the map for its figures.</p>}
    </section>
  );
}
