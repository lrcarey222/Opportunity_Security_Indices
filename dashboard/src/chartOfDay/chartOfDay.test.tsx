// Chart of the Day: every generated day (public/data/chart-of-day/*.json) draws an SVG with its focus country
// visible, and the day-picking rules hide future drafts.

import { afterAll, afterEach, beforeAll, describe, expect, it } from 'vitest';
import { cleanup, render, waitFor } from '@testing-library/react';
import { readdirSync, readFileSync, existsSync } from 'node:fs';
import { join } from 'node:path';
import { ChartOfDayFigure } from './ChartOfDayFigure.tsx';
import { fmtValue, pickDay, publishedDays, type CodIndexDay, type CodSpec } from './data.ts';

const dir = join(__dirname, '..', '..', 'public', 'data', 'chart-of-day');
const files = existsSync(dir) ? readdirSync(dir).filter((f) => /^\d{4}-\d{2}-\d{2}\.json$/.test(f)) : [];

let restore: PropertyDescriptor | undefined;
beforeAll(() => {
  restore = Object.getOwnPropertyDescriptor(HTMLElement.prototype, 'clientWidth');
  Object.defineProperty(HTMLElement.prototype, 'clientWidth', { configurable: true, get: () => 720 });
});
afterEach(() => cleanup());
afterAll(() => {
  if (restore) Object.defineProperty(HTMLElement.prototype, 'clientWidth', restore);
});

describe('day picking', () => {
  const days = ['2026-11-19', '2026-11-18', '2026-11-17', '2026-11-16'].map((date) => ({ date, file: `${date}.json` }) as CodIndexDay);
  it('hides days after today and defaults to the latest published', () => {
    expect(publishedDays(days, '2026-11-17').map((d) => d.date)).toEqual(['2026-11-17', '2026-11-16']);
    expect(pickDay(days, '2026-11-17')?.date).toBe('2026-11-17');
    expect(pickDay(days, '2026-11-17', '2026-11-16')?.date).toBe('2026-11-16');
    expect(pickDay(days, '2026-11-17', '2026-11-19')).toBeNull();
  });
  it('formats values', () => {
    expect(fmtValue(2.13e9, 'usd')).toBe('$2.1bn');
    expect(fmtValue(4.5e8, 'usd')).toBe('$450m');
    expect(fmtValue(0.613, 'pct')).toBe('61%');
    expect(fmtValue(0.004, 'pct')).toBe('0.4%');
  });
});

describe.skipIf(!files.length)('generated days render', () => {
  it.each(files)('%s', async (f) => {
    const spec = JSON.parse(readFileSync(join(dir, f), 'utf8')) as CodSpec;
    expect(spec.title.length).toBeGreaterThan(20);
    expect(spec.rows.length).toBeGreaterThan(1);
    if (spec.template !== 'leaderboard' && spec.template !== 'investment_trend') expect(spec.period, 'charts carry their data period').toBeTruthy();
    const { container } = render(<ChartOfDayFigure spec={spec} />);
    await waitFor(() => expect(container.querySelector('svg')).not.toBeNull());
    const text = container.textContent ?? '';
    // in charts that compare countries, the focus country is one of them (elsewhere it is named in the title)
    const compares = ['leaderboard', 'policy_leaders', 'investment_trend', 'ei_trend', 'export_share_trend', 'trade_map'];
    if (compares.includes(spec.template)) expect(text).toContain(spec.focus.name);
    expect(text).not.toContain('NaN');
  });
});
