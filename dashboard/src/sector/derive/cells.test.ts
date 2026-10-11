// Cells and their measures (spec_sector.md 3.1, 3.3): a cell's measure sits on the cell itself or on its
// linked fuel when the dictionary ties the var to the cell (dict.cells.section_cards / primary_for), so
// Oil Upstream's primary measure is production of fuel:oil and Oil Downstream never shows it; the thin
// rule and the coverage chips. TS fixture, plus builder A's synthetic bundle when present.

import { describe, expect, it } from 'vitest';
import { buildSectorFixture } from '../__fixtures__/build.ts';
import { haveSynthetic, readSyntheticChunk } from '../__fixtures__/__tests__/synthetic.ts';
import { validateChunk } from '../contract.ts';
import { modelOf, type SectorModel } from '../data/model.ts';
import { countryFamilies, countryVars, coverageChips, familyMeasure, isThin, LENS_FAMILIES, measureItem, primaryMeasure, richness, varsOfCell } from './cells.ts';

const cellOf = (model: SectorModel, slug: string) => model.item('cell', slug)!;

describe('cell measures on the TS fixture', () => {
  const model = modelOf(validateChunk('sector-meta', buildSectorFixture().chunks['sector-meta']));

  it('Oil Upstream: the primary measure is fuel production on fuel:oil', () => {
    const pm = primaryMeasure(model, cellOf(model, 'oil-upstream'));
    expect(pm?.v.id).toBe('production.fuels');
    expect(pm?.item.axis).toBe('fuel');
    expect(pm?.item.slug).toBe('oil');
    expect(countryVars(model, cellOf(model, 'oil-upstream')).map((v) => v.id)).toContain('production.fuels');
  });

  it('Oil Downstream does not show oil production (not in its section cards), but shows consumption per person', () => {
    const od = cellOf(model, 'oil-downstream');
    expect(measureItem(model, od, model.varById.get('production.fuels')!)).toBeNull();
    expect(measureItem(model, od, model.varById.get('consumption.per_capita')!)?.slug).toBe('oil');
  });

  it('a measure on the cell itself resolves to the cell; family measures follow', () => {
    const bm = cellOf(model, 'batteries-midstream');
    expect(primaryMeasure(model, bm)).toMatchObject({ v: { id: 'trade.exports' }, item: { slug: 'batteries-midstream' } });
    expect(familyMeasure(model, bm, 'investment')?.v.id).toBe('investment.annual');
    expect(varsOfCell(model, bm).some((v) => v.item_axis === 'fuel')).toBe(false);
  });

  it('minerals never stand in for a cell (they are shown per mineral)', () => {
    const bu = cellOf(model, 'batteries-upstream');
    expect(measureItem(model, bu, model.varById.get('production.mining')!)).toBeNull();
  });

  it('thin cells and coverage chips', () => {
    expect(isThin(model, cellOf(model, 'geothermal-upstream'))).toBe(true);
    const chips = coverageChips(model, cellOf(model, 'batteries-midstream'));
    expect(chips.find((c) => c.family === 'exports')?.hasData).toBe(true);
  });
});

describe.skipIf(!haveSynthetic())('cell measures on the R synthetic bundle', () => {
  it('fuel cells resolve to their EI fuel items; trade cells to themselves', () => {
    const model = modelOf(validateChunk('sector-meta', readSyntheticChunk('sector-meta')!));
    expect(primaryMeasure(model, cellOf(model, 'oil-upstream'))).toMatchObject({ v: { id: 'production.fuels' }, item: { axis: 'fuel', slug: 'oil' } });
    expect(primaryMeasure(model, cellOf(model, 'solar-downstream'))).toMatchObject({ v: { id: 'production.generation' }, item: { axis: 'fuel', slug: 'solar' } });
    expect(primaryMeasure(model, cellOf(model, 'batteries-midstream'))?.item.slug).toBe('batteries-midstream');
    expect(measureItem(model, cellOf(model, 'oil-downstream'), model.varById.get('production.fuels')!)).toBeNull();
  });
});

describe('the thin rule counts lens families only (3.3)', () => {
  const model = modelOf(validateChunk('sector-meta', buildSectorFixture().chunks['sector-meta']));
  it('policy and stage context never make a cell rich', () => {
    for (const c of model.cells) {
      expect(countryFamilies(model, c).every((f) => LENS_FAMILIES.includes(f))).toBe(true);
    }
    // batteries-downstream has policy + context country coverage only in the fixture
    expect(isThin(model, cellOf(model, 'batteries-downstream'))).toBe(true);
    expect(richness(model, cellOf(model, 'batteries-midstream'))).not.toBe('thin');
  });
});

describe.skipIf(!haveSynthetic())('lens families agree with the manifest coverage glyph (R bundle)', () => {
  it('country_data = at least one lens family with country data', async () => {
    const { readSyntheticManifest } = await import('../__fixtures__/__tests__/synthetic.ts');
    const model = modelOf(validateChunk('sector-meta', readSyntheticChunk('sector-meta')!));
    const m = readSyntheticManifest();
    expect(m.coverage_glyph.length).toBeGreaterThan(0);
    for (const g of m.coverage_glyph) {
      const cell = model.cell(g.tech, g.stage);
      expect(cell, `${g.tech}-${g.stage}`).toBeTruthy();
      expect(countryFamilies(model, cell!).length > 0, `${g.tech}-${g.stage}`).toBe(g.country_data);
    }
  });
});
