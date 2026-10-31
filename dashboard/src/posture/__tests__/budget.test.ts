// The posture bundle's size budget (public/data/posture, scripts/91). A page loads the core once and one
// economy's drawer document when a cell is opened, so the budget is per file, not the folder's sum (215
// drawers): the Sector test's Strategic + Sector budget leaves this folder out. Skipped without an export.

import { existsSync, readdirSync, readFileSync, statSync } from 'node:fs';
import { join, resolve } from 'node:path';
import { describe, expect, it } from 'vitest';

const DIR = resolve(process.cwd(), 'public/data/posture');
const CORE_MAX = 3 * 2 ** 20;
// 160 KiB since 2026-10-30 (128 KiB before), the R exporter's limit (R/dashboard/posture/posture_export.R
// POSTURE_BUDGET_DETAIL): the yard children, the strategic-value families and the large-yard case add about
// 25 KiB to the largest drawer documents (the largest is now 136 KiB). The core and the posture data total keep
// their limits (scripts/check-budget.mjs postureData).
const DETAIL_MAX = 160 * 2 ** 10;

describe.skipIf(!existsSync(join(DIR, 'manifest.json')))('posture bundle budget (public/data/posture)', () => {
  const manifest = JSON.parse(readFileSync(join(DIR, 'manifest.json'), 'utf8')) as { core: string; details: Record<string, string> };

  it('the core is at most 3 MB and each drawer document at most 160 kB', () => {
    expect(statSync(join(DIR, manifest.core)).size).toBeLessThanOrEqual(CORE_MAX);
    for (const [iso3, file] of Object.entries(manifest.details)) expect(statSync(join(DIR, file)).size, iso3).toBeLessThanOrEqual(DETAIL_MAX);
  });

  it('the folder holds only the files the manifest names', () => {
    const named = new Set(['manifest.json', manifest.core, ...Object.values(manifest.details)]);
    expect(readdirSync(DIR).filter((f) => !named.has(f))).toEqual([]);
  });
});
