// Country derive helpers on the synthetic fixture (spec_country.md A12 "TS unit"): the generated lede
// (scored, near-tie, no Strategic Index, not rankable), the "why" sentence (drivers named only in the
// direction they push; "despite" otherwise), prose names and possessives, the slope-chart model, the
// priorities matrix and the partner table rows, the quadrant counts line.

import { describe, expect, it } from 'vitest';
import { glossaryById } from '../../content/glossary.ts';
import { SUPPLY_STAGES } from '../../lib/stages.ts';
import { cellOf, NO_ES_CELLS, QUADRANT_IDS, QUADRANTS, type CellFacts, type Driver, type FactSheet } from '../contract.ts';
import { coverageRows, isNoEsCell, matrixCells, partnerTableRows, priorityRows, quadrantLine, scatterPoints, slopeModel, supplierModel } from '../derive/model.ts';
import {
  editionSuffix, euRegionSentence, ledeOf, notPlacedReason, poss, proseName, quadrantWord, rawNoteLine, sectorsByTechText, shortCellLabel, whyIntro, whySentence,
} from '../derive/text.ts';
import { haveFixture, readSheet } from './fixture.ts';

const fv = (v: number | null, d = String(v)) => ({ v, d });

function drv(key: string, impact: number, top = true): Driver {
  return {
    key, category: key[0].toUpperCase() + key.slice(1), score: fv(0.5), median: fv(0.5), weight: fv(0.5), impact: fv(impact),
    imputed: false, rule: null, top, constant: false, vars: [],
  } as unknown as Driver;
}

describe('prose names', () => {
  it('adds the article where the name takes one, and the possessive', () => {
    expect(proseName('United States')).toBe('the United States');
    expect(proseName('Netherlands')).toBe('the Netherlands');
    expect(proseName('Czech Republic')).toBe('the Czech Republic');
    expect(proseName('Solomon Islands')).toBe('the Solomon Islands');
    expect(proseName('Japan')).toBe('Japan');
    expect(proseName('the United States')).toBe('the United States');
    expect(poss('the United States')).toBe("the United States'");
    expect(poss('Japan')).toBe("Japan's");
  });
});

describe('raw-figure notes (2026-10-14)', () => {
  const euRaw = { note: 'eu_region_share', note_d: "the EU's announced share, carried by each EU member" };
  it('says an EU member carries the EU figure, in the note words', () => {
    expect(rawNoteLine(euRaw, 'Finland')).toBe("The EU's announced share, carried by each EU member, not Finland's own figure.");
    expect(rawNoteLine({ note: 'eia_production', note_d: 'production from the EIA' }, 'Belgium')).toBe('Production from the EIA.');
    expect(rawNoteLine({ note: 'production_assumed_zero' }, 'Estonia')).toMatch(/taken as zero/);
    expect(rawNoteLine({ note: null }, 'Japan')).toBeNull();
    expect(rawNoteLine(null, 'Japan')).toBeNull();
  });

  it('a source edition never nests brackets (as the memo source list)', () => {
    expect(editionSuffix('Energy Institute, Statistical Review of World Energy 2026', '2026 (data to 2025)')).toBe(' (data to 2025)');
    expect(editionSuffix('Voting Data, ideal points (Harvard Dataverse V39.0)', 'V39.0 (sessions to 2023)')).toBe(' (sessions to 2023)');
    expect(editionSuffix('Global Trade Alert, New Industrial Policy Observatory', 'October 2026 extract (as of 5 October 2026)')).toBe(
      ', October 2026 extract (as of 5 October 2026)',
    );
    expect(editionSuffix('Atlas', 'data to 2023')).toBe(' (data to 2023)');
    expect(editionSuffix('Atlas', null)).toBe('');
  });

  it('names the top drivers that rest on the EU figure, and none otherwise', () => {
    const d = drv('foreign dependency', 0.1);
    d.vars = [{ key: 'overall-market-share', label: 'Share of world manufacturing capacity, 2035 (announced)', raw: { v: 0.15, d: '15.0%', ...euRaw } }] as unknown as Driver['vars'];
    expect(euRegionSentence({ es: [], eo: [d] }, 'Finland')).toBe(
      "The share of world manufacturing capacity, 2035 (announced) behind foreign dependency is the EU's figure, which every EU member carries, not Finland's own.",
    );
    expect(euRegionSentence({ es: [], eo: [{ ...d, top: false }] }, 'Finland')).toBeNull();
    expect(euRegionSentence({ es: [drv('trade', -0.1)], eo: [] }, 'Finland')).toBeNull();
  });
});

describe.skipIf(!haveFixture())('derive helpers on the synthetic sheets', () => {
  const usa = readSheet('USA');
  const deu = readSheet('DEU');
  const nga = readSheet('NGA');
  const pri = readSheet('PRI');

  it('the lede names the tier, the Upgrade count, the top Strategic Index sector and the top partner', () => {
    const l = ledeOf(usa);
    expect(l.startsWith('The United States is a core member of the allied coalition.')).toBe(true);
    expect(l).toMatch(/On the NEIS baseline, none of its \d+ placed sectors combines high economic opportunity with low energy security/);
    const top = usa.cells.find((c) => c.key === usa.summary.top_si!.cell)!;
    if (usa.summary.top_si!.near_tie) expect(l).toContain('are almost tied');
    else expect(l).toContain(`its top Strategic Index sector is ${top.label_prose}.`);
    expect(l).toContain(`rates highest for it is ${proseName(usa.summary.psi_1!.name)}.`);
    expect(ledeOf(deu)).toMatch(/^Germany is a core member/);
    expect(ledeOf(nga)).toContain('It has no Strategic Index because it has no International Energy Agency policy records.');
    expect(ledeOf(pri)).toMatch(/Puerto Rico's index scores are shown but not ranked against other economies: it is not a sovereign economy\.$/);
    // six stages (2026-10-28): no stage-count or paper qualifier in any lede
    for (const x of [usa, deu, nga, pri]) expect(ledeOf(x), x.iso3).not.toMatch(/three-stage|paper|upstream|midstream|downstream/i);
  });

  it('six stages: cell labels, running text, the scatter and the short labels, the coverage grid, the sectors without Energy Security', () => {
    // labels and running text from R (COUNTRY_TECH_PROSE as a modifier, then the stage)
    const bat = usa.cells.find((c) => c.key === 'batteries-refining')!;
    expect(bat).toMatchObject({ tech: 'Batteries', stage: 'Refining', label: 'Batteries — Refining', label_prose: 'battery refining' });
    expect(usa.cells.find((c) => c.key === 'electric-vehicles-deployment')!.label_prose).toBe('electric-vehicle deployment');
    expect(shortCellLabel(bat)).toBe('Batteries · Ref');
    expect(SUPPLY_STAGES.map((s) => shortCellLabel({ ...bat, stage: s }))).toEqual(['Batteries · Ext', 'Batteries · Ref', 'Batteries · Com', 'Batteries · Asm', 'Batteries · Dep', 'Batteries · Rec']);
    // the scatter: every placed sector, at most 58, never a sector without Energy Security
    const pts = scatterPoints(usa);
    expect(pts.length).toBe(usa.cells.filter((c) => c.placed).length);
    expect(pts.length).toBeLessThanOrEqual(58);
    for (const p of pts) {
      expect(isNoEsCell(usa, p.key)).toBe(false);
      expect(p.short).toBe(shortCellLabel(p.cell));
    }
    expect(NO_ES_CELLS.every((k) => isNoEsCell(usa, k))).toBe(true);
    expect(notPlacedReason({ ...bat, quadrant_reason: null, es: null })).toBe('no Energy Security score for any economy in this sector');
    expect(usa.cells.find((c) => c.key === 'geothermal-extraction')!.quadrant_reason).toMatch(/Energy Security/);
    // the policy coverage grid: 11 technologies x the six stages, cell keys of the sheet
    const cov = coverageRows(usa);
    expect(cov).toHaveLength(11);
    for (const r of cov) {
      expect(r.cells.map((c) => c.stage)).toEqual([...SUPPLY_STAGES]);
      for (const c of r.cells) expect(cellOf(usa, c.key)?.stage).toBe(c.stage);
    }
    // the supplier matrix: six-stage sectors with a trade basket (never Deployment in the fixture)
    const sm = supplierModel(usa);
    expect(sm.cells.length).toBeGreaterThan(0);
    for (const c of sm.cells) expect(SUPPLY_STAGES).toContain(c.stage);
    // sectors in words, grouped by technology (the About page's no-Energy-Security sentence)
    expect(sectorsByTechText(NO_ES_CELLS)).toBe('Coal (recovery), Electric Grid (deployment), Gas (recovery) and Geothermal (extraction, refining, components, assembly and recovery)');
    expect(sectorsByTechText(['solar-assembly', 'oil-upstream', 'solar-refining'])).toBe('Solar (refining and assembly)');
  });

  it('a near-tie at the top is said as such', () => {
    const s = structuredClone(usa) as FactSheet;
    s.summary.top_si = { ...s.summary.top_si!, near_tie: true };
    expect(ledeOf(s)).toMatch(/its top two Strategic Index sectors, .+ and .+, are almost tied\./);
  });

  it('the why sentence names drivers only in the direction they push', () => {
    const c = structuredClone(usa.cells.find((x) => x.placed)!) as CellFacts;
    c.es = { ...c.es!, pct: fv(80, '80%'), side: 'above' };
    c.eo = { ...c.eo!, pct: fv(20, '20%'), side: 'below' };
    const s = whySentence(c, { es: [drv('reserves', 0.1), drv('consumption', -0.05)], eo: [drv('trade', -0.08), drv('investment', -0.02)] });
    expect(s).toContain('Energy Security is high here (higher than 80% of the other');
    expect(s).toContain('mainly because of reserves, despite consumption');
    expect(s).toContain('Economic Opportunity is low here (higher than 20% of them) mainly because of trade and investment');
    const t = whySentence(c, { es: [drv('consumption', -0.05)], eo: [] });
    expect(t).toContain('Energy Security is high here (higher than 80% of the other');
    expect(t).toContain(' despite consumption');
    expect(t).not.toContain('mainly because of consumption');
  });

  it('the why intro follows the priority rule', () => {
    expect(whyIntro(usa)).toMatch(/^None of the United States' placed sectors is in the Upgrade corner/);
    expect(whyIntro(deu)).toMatch(/of Germany's sectors combine high economic opportunity with low energy security/);
    expect(whyIntro(nga)).toMatch(/^Nigeria has no Strategic Index/);
  });

  it('the sheets carry the allied pick, not the retired network design (2026-10-17)', () => {
    for (const s of [usa, deu, nga]) expect('network' in s, s.iso3).toBe(false);
    expect(nga.pick).toBeNull();
    expect(deu.pick?.cell).toBeTruthy();
    expect(deu.cells.find((c) => c.key === deu.pick?.cell)?.si.pick).toBe(true);
  });

  it('the slope model takes the first ten of each order and cross-references ranks', () => {
    const m = slopeModel(usa);
    expect(m.left.map((x) => x.iso3)).toEqual(usa.partners.psi_order.slice(0, m.left.length));
    expect(m.right.map((x) => x.iso3)).toEqual(usa.partners.tie_order.slice(0, m.right.length));
    for (const l of m.left) {
      const r = usa.partners.rows.find((x) => x.iso3 === l.iso3)!;
      expect(l.other).toBe(r.ties.tie_rank.v ?? null);
    }
  });

  it('priorities, the matrix and the partner table rows', () => {
    const rows = priorityRows(usa);
    expect(rows.map((c) => c.key)).toEqual(usa.summary.si_order);
    const grid = matrixCells(usa);
    expect(grid).toHaveLength(9);
    expect(grid.filter((g) => g.diagonal)).toHaveLength(3);
    const inGrid = grid.flatMap((g) => g.cells.map((c) => c.key)).sort();
    expect(inGrid).toEqual([...usa.summary.si_order].sort());
    const pt = partnerTableRows(usa);
    const ranked = pt.filter((r) => !r.extra);
    expect(ranked.map((r) => r.rank.v)).toEqual([...ranked.map((r) => r.rank.v)].sort((a, b) => (a as number) - (b as number)));
    expect(pt.filter((r) => r.extra).every((r) => r.in_tie_top)).toBe(true);
  });

  it("the quadrant counts line and corner words use the app's names, not the sheet's words (2026-10-28)", () => {
    // the synthetic sheets were exported with the older words (Secure, Watch); the app prints its own
    expect(usa.words.quadrant.secure).toBeTruthy();
    expect(quadrantLine({ grow: 4, upgrade: 6, secure: 9, watch: 3 })).toBe('Grow 4 · Upgrade 6 · Exposed 9 · Stable 3');
    expect(QUADRANT_IDS.map((q) => quadrantWord(q))).toEqual(['Grow', 'Upgrade', 'Exposed', 'Stable']);
    expect(quadrantWord(null)).toBe('');
    // the glossary id of each corner follows its name and exists
    for (const q of QUADRANTS) expect(glossaryById.get(q.glossary)?.term, q.id).toBe(q.label);
  });
});
