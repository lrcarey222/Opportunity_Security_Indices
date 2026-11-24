// The Overview's bottom line (derive/bottomLine.ts) on the synthetic and the real bundle: each list keeps the
// scenario's own classes and figures (never re-classified), is ordered largest first, holds only positive
// values and at most three steps; Today's network has none.

import { existsSync } from 'node:fs';
import { resolve } from 'node:path';
import { describe, expect, it } from 'vitest';
import { coordinationBottomLine } from '../derive/bottomLine.ts';
import { nodeScenario } from '../derive/scenarios.ts';
import type { AibDoc } from '../contract.ts';
import { readDoc, REAL_DIR, SYNTHETIC_DIR } from './fixture.ts';

const sorted = (xs: number[]) => xs.every((x, i) => i === 0 || xs[i - 1] >= x);

function check(doc: AibDoc) {
  for (const id of ['decoupling', 'managed'] as const) {
    const rows = doc.nodes.map((n) => nodeScenario(n, id));
    const bl = coordinationBottomLine(rows);
    if (!bl) {
      expect(rows.some((r) => r.ran && r.cls)).toBe(false);
      continue;
    }
    for (const list of [bl.dividend, bl.trade, bl.build]) {
      expect(list.length).toBeLessThanOrEqual(3);
      for (const r of list) expect(r.ran).toBe(true);
    }
    expect(sorted(bl.dividend.map((r) => r.dividend as number))).toBe(true);
    expect(bl.dividend.every((r) => (r.dividend as number) > 0)).toBe(true);
    // the largest dividends are the same steps "Where acting together recovers most" leads with
    const best = Math.max(...rows.filter((r) => r.ran && typeof r.dividend === 'number').map((r) => r.dividend as number));
    if (best > 0) expect(bl.dividend[0].dividend).toBe(best);
    expect(bl.trade.every((r) => r.cls === 'COORDINATE' && (r.split?.trade ?? 0) > 0)).toBe(true);
    expect(sorted(bl.trade.map((r) => r.split!.trade))).toBe(true);
    expect(bl.build.every((r) => r.cls === 'BUILD' && (r.residual ?? 0) > 0)).toBe(true);
    expect(sorted(bl.build.map((r) => r.residual as number))).toBe(true);
    // every Coordinate trade step is a candidate: none with more recovered trade is left out
    const coord = rows.filter((r) => r.ran && r.cls === 'COORDINATE' && (r.split?.trade ?? 0) > 0);
    expect(bl.trade.length).toBe(Math.min(3, coord.length));
  }
  expect(coordinationBottomLine(doc.nodes.map((n) => nodeScenario(n, 'baseline')))).toBeNull();
}

describe('coordinationBottomLine', () => {
  it('the synthetic bundle', () => check(readDoc(SYNTHETIC_DIR)));
  it.skipIf(!existsSync(resolve(REAL_DIR, 'manifest.json')))('the real bundle', () => check(readDoc(REAL_DIR)));
});
