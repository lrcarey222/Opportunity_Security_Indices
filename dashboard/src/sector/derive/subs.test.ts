import { describe, expect, it } from 'vitest';
import { buildSectorFixture, fixtureItem } from '../__fixtures__/build.ts';
import type { SectorFactsChunk, SectorMetaChunk, SectorTradeChunk } from '../contract.ts';
import { factsIndexOf } from '../data/facts.ts';
import { SectorModel } from '../data/model.ts';
import { slice } from './summary.ts';
import { hasSubTrade, subComponents, subNet, subSlice, validSub } from './subs.ts';

const fx = buildSectorFixture();
const model = new SectorModel(fx.chunks['sector-meta'] as unknown as SectorMetaChunk);
const facts = factsIndexOf(fx.chunks['sector-facts'] as unknown as SectorFactsChunk);
const trade = fx.chunks['sector-trade-batteries'] as unknown as SectorTradeChunk;
const solar = fx.chunks['sector-trade-solar'] as unknown as SectorTradeChunk;
const bm = fixtureItem('cell', 'batteries-assembly');

describe('trade by sub-component (C5.4)', () => {
  it('lists the sub-components of a cell, largest first, with their codes and shares', () => {
    expect(hasSubTrade(trade)).toBe(true);
    const subs = subComponents(trade, model, bm);
    expect(subs.map((s) => s.name)).toEqual(['Cells and packs', 'Materials']);
    expect(subs[0].codes).toEqual(['850760']);
    expect(subs[0].share).toBeCloseTo(0.7, 12);
    expect(subs[0].year).toBe(2024);
    expect(subComponents(solar, model, fixtureItem('cell', 'solar-refining')).map((s) => s.name)).toEqual(['Silicon metal']);
  });

  it('sub-components add up to the cell for every economy, year and side', () => {
    const subs = subComponents(trade, model, bm);
    for (const year of [2020, 2024] as const) {
      for (const side of ['exports', 'imports'] as const) {
        const v = model.varById.get(`trade.${side}`)!;
        const cellRows = facts.rows(v.idx, bm, year);
        const sum = new Map<number, number>();
        let W = 0;
        for (const s of subs) {
          const sl = subSlice(trade, model, bm, s.name, year, side);
          W += sl.world?.value ?? 0;
          for (const r of sl.rows) sum.set(r.geo, (sum.get(r.geo) ?? 0) + r.value);
        }
        for (const r of cellRows) if (r.value > 0 && model.geos[r.geo].kind === 'country') expect(sum.get(r.geo) ?? 0).toBeCloseTo(r.value, 0);
        expect(W).toBeCloseTo(facts.world(v.idx, bm, year)?.value ?? NaN, 0);
      }
    }
  });

  it('EU bloc: members merge into one row without the intra-EU flows, and the world loses them too', () => {
    const members = subSlice(trade, model, bm, 'Cells and packs', 2024, 'exports', 'members');
    const bloc = subSlice(trade, model, bm, 'Cells and packs', 2024, 'exports', 'bloc');
    const eu = bloc.rows.find((r) => r.geo === model.eu!.idx)!;
    const memberSum = members.rows.filter((r) => model.geos[r.geo].eu_member).reduce((a, r) => a + r.value, 0);
    expect(eu.value).toBeLessThan(memberSum);
    expect(bloc.rows.some((r) => model.geos[r.geo].eu_member)).toBe(false);
    expect(bloc.world!.value).toBeCloseTo(members.world!.value - (memberSum - eu.value), 3);
    // the cell's bloc slice equals the sum of its sub-components in bloc mode
    const v = model.varById.get('trade.exports')!;
    const cellBloc = slice(model, facts, v, bm, 2024, 'bloc', trade);
    const cellEu = cellBloc.rows.find((r) => r.geo === model.eu!.idx)!.value;
    const subEu = subComponents(trade, model, bm).reduce((a, s) => a + (subSlice(trade, model, bm, s.name, 2024, 'exports', 'bloc').rows.find((r) => r.geo === model.eu!.idx)?.value ?? 0), 0);
    expect(subEu).toBeCloseTo(cellEu, 0);
  });

  it('imports of a non-declarer are mirrors; net trade pairs both sides', () => {
    const im = subSlice(trade, model, bm, 'Materials', 2024, 'imports');
    expect(im.rows.find((r) => model.geos[r.geo].code === 'VNM')?.status).toBe('mirror');
    expect(im.rows.find((r) => model.geos[r.geo].code === 'USA')?.status).toBe('obs');
    const net = subNet(trade, model, bm, 'Materials', 2024);
    const chn = net.find((x) => model.geos[x.geo].code === 'CHN')!;
    expect(chn.net).toBeCloseTo(chn.exports - chn.imports, 6);
  });

  it('an unknown sub-component name falls back to the whole basket', () => {
    const subs = subComponents(trade, model, bm);
    expect(validSub(subs, 'Materials')).toBe('Materials');
    expect(validSub(subs, 'Nope')).toBeNull();
    expect(validSub(subs, null)).toBeNull();
  });
});
