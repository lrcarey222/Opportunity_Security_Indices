import { describe, expect, it } from 'vitest';
import { buildSectorFixture, fixtureGeo, fixtureItem } from '../__fixtures__/build.ts';
import { validateChunk } from '../contract.ts';
import { factsIndexOf } from '../data/facts.ts';
import { modelOf } from '../data/model.ts';
import {
  blocFlows, countryTotals, declaredImporters, flowsOf, league, matrixTotal, netTrade, OTHER_IMPORTERS, pairState, partners, sankeyLinks, supplierMixes, tradeItems, tradeQuality,
} from './trade.ts';

const fx = buildSectorFixture();
const model = modelOf(validateChunk('sector-meta', fx.chunks['sector-meta']));
const facts = factsIndexOf(validateChunk('sector-facts', fx.chunks['sector-facts']));
const trade = validateChunk('sector-trade-batteries', fx.chunks['sector-trade-batteries']);
const bm = fixtureItem('cell', 'batteries-assembly');
const g = fixtureGeo;

describe('trade on the matrix (3.8, 4.3 S4)', () => {
  const flows = flowsOf(trade, bm, 2024);

  it('reads the positive flows of one cell and year', () => {
    expect(tradeItems(trade)).toEqual([fixtureItem('cell', 'batteries-extraction'), bm].sort((a, z) => a - z));
    expect(flows.every((f) => f.v > 0)).toBe(true);
    expect(flowsOf(trade, bm, 2020).length).toBe(flows.length - 1); // BRA->USA has no 2020 value
  });

  it('country totals equal the shipped facts (one basis everywhere)', () => {
    const t = countryTotals(flows);
    const ex = facts.rows(model.varById.get('trade.exports')!.idx, bm, 2024);
    for (const r of ex) expect(t.get(r.geo)?.exports).toBeCloseTo(r.value, 0);
    const W = facts.world(model.varById.get('trade.exports')!.idx, bm, 2024)!.value;
    expect(matrixTotal(flows)).toBeCloseTo(W, 0);
    const lg = league(t, 'exports', W);
    expect(lg[0].geo).toBe(g('CHN'));
    expect(lg[0].rank).toBe(1);
    const net = netTrade(t, 25);
    expect(net.find((x) => x.geo === g('USA'))!.net).toBeLessThan(0);
  });

  it('the Sankey folds flows under 0.5% into Other importers and keeps exporter totals', () => {
    const s = sankeyLinks(flows, { topE: 3, topI: 3, fold: 0.05 });
    const W = matrixTotal(flows);
    expect(s.total).toBeCloseTo(W, 0);
    expect(s.links.reduce((a, l) => a + l.v, 0)).toBeCloseTo(W, 0);
    expect(s.folded).toBeGreaterThan(0);
    const small = s.links.filter((l) => l.i !== OTHER_IMPORTERS);
    for (const l of small) expect(l.share).toBeGreaterThanOrEqual(0.05);
    const chn = s.links.filter((l) => l.e === g('CHN')).reduce((a, l) => a + l.v, 0);
    expect(chn).toBeCloseTo(countryTotals(flows).get(g('CHN'))!.exports, 0);
  });

  it("each importer's supplier mix sums to 1 and is sorted by China share", () => {
    const m = supplierMixes(flows, model.geos);
    for (const x of m) expect(Object.values(x.parts).reduce((a, b) => a + b, 0)).toBeCloseTo(1, 12);
    for (let i = 1; i < m.length; i++) expect(m[i - 1].china).toBeGreaterThanOrEqual(m[i].china);
    const usa = m.find((x) => x.importer === g('USA'))!;
    expect(usa.china).toBeCloseTo(21.4 / (21.4 + 6 + 4 + 1.4 + 0.3 + 1.1), 12);
  });

  it('bloc mode removes intra-EU flows and merges members', () => {
    const b = blocFlows(flows, model);
    const eu = g('EUU');
    expect(b.some((f) => f.e === eu && f.i === eu)).toBe(false);
    expect(b.some((f) => f.e === g('DEU') || f.i === g('DEU'))).toBe(false);
    const toEu = b.filter((f) => f.i === eu).reduce((a, f) => a + f.v, 0);
    expect(toEu).toBeCloseTo((18 + 3 + 3) * 1e9, 0); // CHN->DEU, KOR->DEU, CHN->FRA
    expect(matrixTotal(flows) - matrixTotal(b)).toBeCloseTo((2.5 + 1.2 + 0.02) * 1e9, 0);
  });

  it('zeros are rebuilt from declarations; non-declarers are unobserved', () => {
    const d = declaredImporters(trade, bm, 2024);
    expect(d.has(g('VNM'))).toBe(false);
    expect(pairState(flows, d, g('CHN'), g('USA'))).toBe('flow');
    expect(pairState(flows, d, g('BRA'), g('JPN'))).toBe('zero');
    expect(pairState(flows, d, g('BRA'), g('VNM'))).toBe('unobserved');
  });

  it('the data-quality strip', () => {
    const q = tradeQuality(flows, declaredImporters(trade, bm, 2024));
    expect(q.importerShare + q.mirrorShare).toBeCloseTo(1, 12);
    expect(q.nonDeclarers[0].geo).toBe(g('VNM'));
  });

  it('partners of one economy', () => {
    const p = partners(flows, g('USA'), 'imports');
    expect(p[0].geo).toBe(g('CHN'));
    expect(p.reduce((a, x) => a + x.share, 0)).toBeCloseTo(1, 12);
  });
});
