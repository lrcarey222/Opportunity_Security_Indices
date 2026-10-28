// The shared supply-chain stages (src/lib/stages.ts): the six stages, the legacy three and the roll-up equal
// the R constants of R/utils/supply_chain_stages.R (read from the file, so the two cannot drift); the chain
// rank puts unknown names last; cell slugs parse in both forms, build as R aib_slug() writes them and roll
// up to their legacy cell, sub-component ids too; a node's own legacy cells win over the roll-up, which
// differs from them where the paper filed codes elsewhere (refined materials: Refining rolls up to
// Upstream, the paper's Midstream).

import { readFileSync } from 'node:fs';
import { resolve } from 'node:path';
import { describe, expect, it } from 'vitest';
import {
  byStageRank, cellSlug, isLegacyCellSlug, isLegacyStage, isLegacySubcomponentId, isSupplyStage, LEGACY_STAGE_SLUGS, LEGACY_STAGES, legacyCellsOf, legacySubcomponentsOf,
  parseCellSlug, parseSubcomponentId, rollUpCellSlug, rollUpStage, rollUpSubcomponentId, STAGE_ROLLUP, stageRank, stagesInChainOrder, SUPPLY_STAGE_SLUGS, SUPPLY_STAGES, subcomponentId,
  techSlug,
} from './stages.ts';

const R_FILE = resolve(process.cwd(), '../R/utils/supply_chain_stages.R');
const rSource = readFileSync(R_FILE, 'utf8');

/** The body of `NAME <- c(...)` in the R file (the first balanced parenthesis after the assignment). */
function rVector(name: string): string {
  const start = rSource.indexOf(`${name} <- c(`);
  if (start < 0) throw new Error(`${name} not found in ${R_FILE}`);
  const open = rSource.indexOf('(', start);
  let depth = 0;
  for (let i = open; i < rSource.length; i++) {
    if (rSource[i] === '(') depth++;
    if (rSource[i] === ')' && --depth === 0) return rSource.slice(open + 1, i);
  }
  throw new Error(`${name}: unbalanced parentheses`);
}
const rStrings = (name: string): string[] => [...rVector(name).matchAll(/"([^"]*)"/g)].map((m) => m[1]);
const rNamed = (name: string): Record<string, string> =>
  Object.fromEntries([...rVector(name).matchAll(/([A-Za-z]+)\s*=\s*"([^"]*)"/g)].map((m) => [m[1], m[2]]));

describe('the stages mirror R/utils/supply_chain_stages.R', () => {
  it('six stages and three legacy stages, in the same order', () => {
    expect([...SUPPLY_STAGES]).toEqual(rStrings('SUPPLY_CHAIN_STAGES'));
    expect([...LEGACY_STAGES]).toEqual(rStrings('SUPPLY_CHAIN_LEGACY_STAGES'));
    expect([...SUPPLY_STAGE_SLUGS]).toEqual(SUPPLY_STAGES.map((s) => s.toLowerCase()));
    expect([...LEGACY_STAGE_SLUGS]).toEqual(LEGACY_STAGES.map((s) => s.toLowerCase()));
  });

  it('STAGE_ROLLUP equals SUPPLY_CHAIN_STAGE_ROLLUP, entry for entry', () => {
    const r = rNamed('SUPPLY_CHAIN_STAGE_ROLLUP');
    expect(Object.keys(r)).toEqual([...SUPPLY_STAGES]);
    expect({ ...STAGE_ROLLUP }).toEqual(r);
    // refining rolls up to Upstream (user decision 2026-10-26), manufacturing is Midstream
    expect(STAGE_ROLLUP.Refining).toBe('Upstream');
    expect(STAGE_ROLLUP.Recovery).toBe('Upstream');
    expect([STAGE_ROLLUP.Components, STAGE_ROLLUP.Assembly, STAGE_ROLLUP.Deployment]).toEqual(['Midstream', 'Midstream', 'Downstream']);
  });
});

describe('stage names', () => {
  it('rank in chain order, unknown names last; sort and de-duplicate', () => {
    expect(SUPPLY_STAGES.map(stageRank)).toEqual([0, 1, 2, 3, 4, 5]);
    expect(stageRank('Upstream')).toBe(6);
    expect(stageRank('refining')).toBe(6);
    expect(['Recovery', 'Midstream', 'Assembly', 'Extraction', 'Sidestream'].sort(byStageRank)).toEqual(['Extraction', 'Assembly', 'Recovery', 'Midstream', 'Sidestream']);
    expect(stagesInChainOrder(['Assembly', 'Refining', 'Assembly', 'Extraction'])).toEqual(['Extraction', 'Refining', 'Assembly']);
  });

  it('type guards and the roll-up of a name', () => {
    expect(isSupplyStage('Components')).toBe(true);
    expect(isSupplyStage('Midstream')).toBe(false);
    expect(isLegacyStage('Midstream')).toBe(true);
    expect(isLegacyStage('midstream')).toBe(false);
    expect(rollUpStage('Refining')).toBe('Upstream');
    expect(rollUpStage('Deployment')).toBe('Downstream');
    expect(rollUpStage('Midstream')).toBe('Midstream');
    expect(rollUpStage('Sidestream')).toBeNull();
  });
});

describe('cell slugs', () => {
  it('build as R aib_slug() writes them', () => {
    expect(techSlug('Electric Vehicles')).toBe('electric-vehicles');
    expect(techSlug('  Oil & Gas ')).toBe('oil-and-gas');
    expect(cellSlug('Electric Grid', 'Assembly')).toBe('electric-grid-assembly');
    expect(cellSlug('Batteries', 'Midstream')).toBe('batteries-midstream');
    expect(subcomponentId('batteries-refining', 'Anode Materials')).toBe('batteries-refining::Anode Materials');
  });

  it('parse both forms; anything else is null', () => {
    expect(parseCellSlug('electric-vehicles-components')).toEqual({ slug: 'electric-vehicles-components', techSlug: 'electric-vehicles', stage: 'Components', legacy: false });
    expect(parseCellSlug('wind-midstream')).toEqual({ slug: 'wind-midstream', techSlug: 'wind', stage: 'Midstream', legacy: true });
    for (const bad of ['batteries', 'batteries-', '-refining', 'Batteries-Refining', 'batteries-sidestream', 'batteries--refining', '']) expect(parseCellSlug(bad), bad).toBeNull();
    expect(parseSubcomponentId('wind-components::Generator Parts')).toEqual({ cell: parseCellSlug('wind-components'), name: 'Generator Parts' });
    expect(parseSubcomponentId('wind-components::')).toBeNull();
    expect(parseSubcomponentId('wind-components')).toBeNull();
    expect(parseSubcomponentId('wind-sidestream::X')).toBeNull();
    expect(isLegacyCellSlug('solar-midstream')).toBe(true);
    expect(isLegacyCellSlug('solar-assembly')).toBe(false);
    expect(isLegacySubcomponentId('solar-midstream::Solar Modules')).toBe(true);
    expect(isLegacySubcomponentId('solar-assembly::Solar Modules')).toBe(false);
    expect(isLegacySubcomponentId('solar-midstream')).toBe(false);
  });

  it('roll up to the legacy cell; a legacy slug is unchanged; the sub-component name is kept', () => {
    expect(rollUpCellSlug('batteries-refining')).toBe('batteries-upstream');
    expect(rollUpCellSlug('batteries-recovery')).toBe('batteries-upstream');
    expect(rollUpCellSlug('wind-components')).toBe('wind-midstream');
    expect(rollUpCellSlug('solar-assembly')).toBe('solar-midstream');
    expect(rollUpCellSlug('solar-deployment')).toBe('solar-downstream');
    expect(rollUpCellSlug('solar-midstream')).toBe('solar-midstream');
    expect(rollUpCellSlug('not a cell')).toBeNull();
    expect(rollUpSubcomponentId('batteries-refining::Cathode Materials & Battery Precursors')).toBe('batteries-upstream::Cathode Materials & Battery Precursors');
    expect(rollUpSubcomponentId('wind-midstream::Generator Parts')).toBe('wind-midstream::Generator Parts');
    expect(rollUpSubcomponentId('Generator Parts')).toBeNull();
  });

  it("a node's own legacy cells win over the roll-up, which differs from them for refined materials", () => {
    // the anode node: Refining at six stages, Midstream in the paper's basket
    const anode = { yfCells: ['batteries-refining'], yfSubcomponents: ['batteries-refining::Anode Materials'] };
    expect(legacyCellsOf(anode)).toEqual(['batteries-upstream']);
    expect(legacySubcomponentsOf(anode)).toEqual(['batteries-upstream::Anode Materials']);
    const own = { ...anode, yfCellsLegacy: ['batteries-midstream'], yfSubcomponentsLegacy: ['batteries-midstream::Anode Materials'] };
    expect(legacyCellsOf(own)).toEqual(['batteries-midstream']);
    expect(legacySubcomponentsOf(own)).toEqual(['batteries-midstream::Anode Materials']);
    // [] is an answer (a node with no basket code), not a reason to roll up
    expect(legacyCellsOf({ yfCells: ['batteries-refining'], yfCellsLegacy: [] })).toEqual([]);
    // two six-stage cells of one technology roll up to one legacy cell, listed once, in first-appearance order
    expect(legacyCellsOf({ yfCells: ['solar-assembly', 'electric-grid-assembly', 'solar-components', 'junk'] })).toEqual(['solar-midstream', 'electric-grid-midstream']);
    expect(legacyCellsOf({ yfCells: [], yfCellsLegacy: null })).toEqual([]);
  });
});
