// The vintage badge of a measure without a year (spec_sector.md 3.9): a long dictionary data_year is
// shortened to the years it names (a badge does not wrap; the PAMS text overflowed a 390 px screen),
// the full text staying in the caption's sub line.

import { describe, expect, it } from 'vitest';
import type { SectorModel, VarInfo } from '../data/model.ts';
import { dataYearBadge, factYearText, makeCaptionResolver, periodText, titleYear } from './caption.ts';
import { buildSectorFixture } from '../__fixtures__/build.ts';
import { validateChunk } from '../contract.ts';
import { modelOf } from '../data/model.ts';
import { factsIndexOf } from '../data/facts.ts';

const v = (data_year: string, years: number[] = []) => ({ data_year, years }) as unknown as VarInfo;

describe('dataYearBadge', () => {
  it('keeps short texts as written', () => {
    expect(dataYearBadge(v('2025 (2035 projection for five countries)'))).toBe('2025 (2035 projection for five countries)');
    expect(dataYearBadge(v('1996-2026 (year announced)'))).toBe('1996-2026 (year announced)');
  });
  it('shortens long texts to the years they name, else the measure years', () => {
    expect(dataYearBadge(v('IEA PAMS year (in force or announced; not always the promulgation date), to 2030', [2025]))).toBe('to 2030');
    expect(dataYearBadge(v('2025 operational; under construction, announced and paused: planned to 2030'))).toBe('2025 → 2030');
    expect(dataYearBadge(v('a long description of the vintage without any year in it at all, really', [2018, 2025]))).toBe('2018 → 2025');
    expect(dataYearBadge(v('a long description of the vintage without any year in it at all, really')).endsWith('…')).toBe(true);
  });
});

describe('caption sub line: the economy count', () => {
  // a country-level CM-FDI measure: its cards filter the projects (basis, window, economy), so the
  // build's destination count (avail n_geo) is left out; any other country-level measure keeps it
  const mk = (id: string) =>
    ({ id, label: id, native_level: 'country', item_axis: 'cell', source: { id: 's', short: 'Src', edition: null }, data_year: '2024', years: [2024], year_field: 0, projection_from: null }) as unknown as VarInfo;
  const item = { idx: 0, slug: 'batteries-assembly' };
  const vars = [mk('investment.fdi_parent_share'), mk('trade.exports')];
  const model = {
    varById: new Map(vars.map((x) => [x.id, x])),
    item: () => item,
    items: [item],
    unitOf: () => ({ short: 'share', long: 'share (0-1)' }),
    caveatTexts: () => [],
    availOf: () => [{ n_geo: 34 }],
    countries: [],
  } as unknown as SectorModel;
  const resolve = makeCaptionResolver(model, () => null);
  it('omits it for CM-FDI measures and keeps it for the others', () => {
    expect(resolve([{ var: 'investment.fdi_parent_share', item: item.slug, window: [2006, 2026] }]).sub).not.toMatch(/economies/);
    expect(resolve([{ var: 'trade.exports', item: item.slug, year: 2024 }]).sub).toMatch(/34 economies/);
  });
});

// A value averaged or summed over a span of years is labelled as such, never as one year (repair
// 2026-10-15: World Bank indicators read "…, 2024" and "2023" for a 2022-2023 mean).
describe('periodText / factYearText / titleYear', () => {
  const wdi = v('2022-2024 (mean of the years reported)', [2024]);
  const total = v('2013-2024 total / 2023 GDP', [2024]);
  const annual = v('2017-2024', [2024]);
  it('names the averaging window or the total', () => {
    expect(periodText(wdi, 'short')).toBe('2022–2024 average');
    expect(periodText(wdi, 'long')).toBe('average of the years reported, 2022–2024');
    expect(periodText(total, 'short')).toBe('2013–2024 total');
    expect(periodText(total, 'long')).toBe('2013–2024 total / 2023 GDP');
    expect(periodText(annual, 'short')).toBeNull();
  });
  it('gives each fact the years it averages', () => {
    expect(factYearText(wdi, { obs_year: 2023, obs_from: 2022 }, 2024)).toBe('2022–2023 average');
    expect(factYearText(wdi, { obs_year: 2023, obs_from: 2023 }, 2024)).toBe('2023');
    expect(factYearText(total, { obs_year: null }, 2024)).toBe('2013–2024 total');
    expect(factYearText(annual, { obs_year: null }, 2024)).toBe('2024');
  });
  it('puts the window in titles instead of the nominal year', () => {
    expect(titleYear(wdi, 2024)).toBe('2022–2024 average');
    expect(titleYear(annual, 2021)).toBe('2021');
  });
});

// The copied manufacturing figures (design C2, 2026-10-28): the same_figure_pair caveat is a data caveat, shown
// only on a card whose item has such facts (Batteries — Components and — Assembly), never on another cell's card.
describe('caption caveats: a data caveat only where its facts are', () => {
  const fx = buildSectorFixture();
  const model = modelOf(validateChunk('sector-meta', fx.chunks['sector-meta']));
  const facts = factsIndexOf(validateChunk('sector-facts', fx.chunks['sector-facts']));
  const resolve = makeCaptionResolver(model, () => facts);
  const same = model.caveats.get('same_figure_pair')!;
  it('on Components and Assembly of Batteries, not on Solar — Deployment; a card over every item keeps it', () => {
    expect(same).toMatch(/^Same figure on Components and Assembly/);
    expect(resolve([{ var: 'investment.annual', item: 'batteries-components', year: 2025 }]).caveats).toContain(same);
    expect(resolve([{ var: 'investment.annual', item: 'batteries-assembly', year: 2025 }]).caveats).toContain(same);
    const solar = resolve([{ var: 'investment.annual', item: 'solar-deployment', year: 2025 }]).caveats ?? [];
    expect(solar).not.toContain(same);
    expect(solar).toContain(model.caveats.get('est_q4'));
    expect(resolve([{ var: 'investment.annual' }]).caveats).toContain(same);
  });
});
