// The Yards & Fences words and marks (posture/yards.ts; 2026-10-30) on the posture fixture with its layers: the
// user's mapping (Small yard · High fence = Secure; Medium yard · Medium fence = Preserve, Catch Up, Partner and
// Accelerate; Large yard · Low fence = Watch), the mixed roll-up and its diagonal split (the node's own choice
// colour, then the fence colour of each other yard), each child layer's name and one-line reason from its choice,
// variant and the layer's evidence, the large-yard case (the node's, or the economy's own where it differs) and
// the three families of the Strategic Industrial Value.

import { describe, expect, it } from 'vitest';
import { checkPostureDetail, decodeCore, type PostureCell } from '../contract.ts';
import { countryCells } from '../derive.ts';
import { POSTURE_COLOR, VERB_COLOR } from '../grammar.ts';
import {
  capWords, countWords, isMixedRollup, largeYardOf, largeYardWords, layerLabel, layerReason, layerUnits, MIXED_LEGEND, mixedFill, mixedYards, nodeYard, partChoiceWords, partFences, partsFenceWords,
  partsMixed, REST_OF_STEP, rollupWords, showsLargeYard, signalWords, sivFamilies, splitSignals, stepParts, YARD_COLOR, yardWords,
} from '../yards.ts';
import { fixtureCore, fixtureDetail } from './fixture.ts';

const core = decodeCore(fixtureCore(true, { yards: true }));
const detail = checkPostureDetail(fixtureDetail({ yards: true }));
const cells = countryCells(core, 'AAA');
const cell = (tech: string, stage: string) => cells.find((c) => c.tech === tech && c.stage === stage) as PostureCell;
const node = (key: string) => detail.cells.find((c) => c.key === key) ?? null;

describe('the yards in the user mapping', () => {
  it('each choice has its yard and fence; a node not assessed has none', () => {
    expect(['Refining', 'Components', 'Deployment'].map((s) => nodeYard(cell('Batteries', s)))).toEqual(['small', 'medium', 'medium']);
    expect([nodeYard(cell('Solar', 'Extraction')), nodeYard(cell('Batteries', 'Recovery'))]).toEqual(['large', null]);
    expect((['small', 'medium', 'large'] as const).map(yardWords)).toEqual(['Small yard · High fence', 'Medium yard · Medium fence', 'Large yard · Low fence']);
    // the fence colours: Secure orange, the Compete family blue, Watch ink-600 (NEIS tokens)
    expect(YARD_COLOR).toEqual({ small: VERB_COLOR.secure, medium: POSTURE_COLOR.C, large: VERB_COLOR.watch });
    expect(YARD_COLOR.medium).toBe('#1889b0');
  });

  it('a mixed roll-up: the node own yard first, the yards its layers reach, in words', () => {
    const asm = cell('Batteries', 'Assembly');
    expect([isMixedRollup(asm.yards), mixedYards(asm)]).toEqual([true, { own: 'medium', others: ['small', 'large'] }]);
    expect(rollupWords(asm)).toBe('Mixed: this step in the Medium yard, its layers also in the Small and Large yards');
    expect(rollupWords(cell('Solar', 'Assembly'))).toBe('Mixed: this step in the Medium yard, its layers also in the Large yard');
    // a node whose layers agree (or that has none) is its yard, never mixed
    expect([rollupWords(cell('Batteries', 'Refining')), mixedYards(cell('Batteries', 'Refining')), rollupWords(cell('Batteries', 'Recovery'))]).toEqual(['Small yard', null, null]);
    expect(MIXED_LEGEND).toBe("Mixed: this step's layers sit in different yards");
  });

  it('the diagonal split: the own choice colour upper left, each other yard fence colour lower right, a card hairline between; no split for a node that is not mixed', () => {
    const two = mixedFill(cell('Solar', 'Assembly')) as string;
    expect(two).toMatch(/^linear-gradient\(135deg, /);
    // Preserve blue, the hairline, then the Large yard grey
    const order = (s: string, ...xs: string[]) => xs.map((x) => s.indexOf(x));
    expect(order(two, VERB_COLOR.preserve, '#1f2937', VERB_COLOR.watch).every((i, k, a) => i >= 0 && (k === 0 || i > a[k - 1]))).toBe(true);
    const three = mixedFill(cell('Batteries', 'Assembly')) as string;
    expect(order(three, VERB_COLOR.preserve, VERB_COLOR.secure, VERB_COLOR.watch).every((i, k, a) => i >= 0 && (k === 0 || i > a[k - 1]))).toBe(true);
    expect(three).toContain('calc(75% + 1px) 100%');
    expect(mixedFill(cell('Batteries', 'Refining'))).toBeNull();
    // a Watch fill or a Large-yard part needs the light hairline on the card
    expect([showsLargeYard(cell('Solar', 'Assembly')), showsLargeYard(cell('Solar', 'Extraction')), showsLargeYard(cell('Batteries', 'Components'))]).toEqual([true, true, false]);
  });
});

describe('the child layers', () => {
  it('each layer named from the core evidence, with its yard, choice and a one-line reason', () => {
    const units = layerUnits(core.meta, node('Batteries|Assembly'));
    expect(units.map((u) => [u.layerId, u.yard, u.verb])).toEqual([['bess-cells', 'medium', 'preserve'], ['bess-control', 'small', 'secure'], ['bess-enclosure', 'large', 'watch']]);
    expect(units.map((u) => u.label)).toEqual([
      'Battery cells (LFP and NMC)', 'Battery-management firmware, energy-management software and remote updates', 'Enclosures, racking, thermal management and balance of system',
    ]);
    expect(units.map((u) => u.reason)).toEqual([
      'A strong position worth keeping: industrial commons 100, future option 50.',
      'Control built partly into the hardware: the high fence extends to the hardware.',
      'Large-yard case: mature, passive, standardized, substitutable, quick to qualify, little process learning, little cross-sector spillover, little product differentiation; kept with a capability floor.',
    ]);
    expect(layerUnits(core.meta, null)).toEqual([]);
    expect(layerUnits(core.meta, node('Batteries|Components'))).toEqual([]);
  });

  it('the other reasons: the commons and option of a Medium-yard layer (with its cap and how much was observed), a Secure scope that is not established, a layer not assessed', () => {
    const r = (layerId: string, choice: Parameters<typeof layerReason>[3]['choice'], variant: string | null) => layerReason(core.meta, 'Batteries', 'Assembly', { layerId, choice, variant });
    expect(r('bess-control', 'partner', 'compete_partner')).toBe('Rely on trusted partners rather than duplicate at home: industrial commons 33 (1 of 3 observed), no future option; market-only value, capped below the prize bar.');
    expect(r('bess-control', 'secure', 'secure_unclear')).toBe('A control layer whose separability from the hardware is not established.');
    expect(r('bess-cells', 'catch_up', null)).toBe('Catch Up: industrial commons 100, future option 50.');
    expect(r('bess-cells', 'not_assessed', null)).toBe('Not assessed: the layer has no choice of its own.');
    // a Watch layer without a large-yard case: its commons and option
    expect(layerReason(core.meta, 'Batteries', 'Assembly', { layerId: 'bess-control', choice: 'watch', variant: 'deploy' })).toBe(
      'Low domestic priority: industrial commons 33 (1 of 3 observed), no future option; market-only value, capped below the prize bar.',
    );
    // a layer the core has no evidence for (a Deployment & Use layer): its id in words
    expect(layerLabel(core.meta, 'Electric Vehicles', 'Deployment', 'ev-charging-mgmt')).toBe('ev charging mgmt');
  });
});

describe('the parts of a step (the map tooltip and name, the drawer; 2026-10-31)', () => {
  it("the rest of the step (its own choice) first, then the layers behind another fence (High to Low), then those behind the step's own, each with its choice, fence and one line", () => {
    const parts = stepParts(core.meta, cell('Batteries', 'Assembly'), node('Batteries|Assembly'));
    expect(parts.map((p) => [p.layerId, p.label, p.verb, p.yard, p.fence])).toEqual([
      [null, REST_OF_STEP, 'preserve', 'medium', 'medium'],
      ['bess-control', 'Battery-management firmware, energy-management software and remote updates', 'secure', 'small', 'high'],
      ['bess-enclosure', 'Enclosures, racking, thermal management and balance of system', 'watch', 'large', 'low'],
      ['bess-cells', 'Battery cells (LFP and NMC)', 'preserve', 'medium', 'medium'],
    ]);
    expect(REST_OF_STEP).toBe('Rest of the step');
    // the rest of the step's line is its kind of choice; a layer's is its own reason (layerReason)
    expect(parts[0].reason).toBe('A strong position worth keeping.');
    expect(parts.slice(1).map((p) => p.reason)).toEqual(['bess-control', 'bess-enclosure', 'bess-cells'].map((id) => layerUnits(core.meta, node('Batteries|Assembly')).find((u) => u.layerId === id)?.reason));
    expect(parts.map(partChoiceWords)).toEqual(['Preserve · Medium fence', 'Secure · High fence', 'Watch · Low fence', 'Preserve · Medium fence']);
    expect([partFences(parts), partsMixed(parts)]).toEqual([['high', 'medium', 'low'], true]);
    // no layers, or the detail not in yet: no parts
    expect(stepParts(core.meta, cell('Batteries', 'Components'), node('Batteries|Components'))).toEqual([]);
    expect(stepParts(core.meta, cell('Batteries', 'Assembly'), null)).toEqual([]);
  });

  it("the parts' fences in brief for the accessible name: grouped by fence, capped, or one line when they agree; a part not assessed says so", () => {
    const asm = stepParts(core.meta, cell('Batteries', 'Assembly'), node('Batteries|Assembly'));
    expect(partsFenceWords(asm)).toBe(
      'parts behind different fences: High fence: Battery-management firmware, energy-management software and remote updates; Medium fence: the rest of the step, Battery cells (LFP and NMC); Low fence: Enclosures, racking, thermal management and balance of system',
    );
    expect(partsFenceWords(asm, 2)).toBe('parts behind different fences: High fence: Battery-management firmware, energy-management software and remote updates; Medium fence: the rest of the step, 1 more; Low fence: 1 part');
    // the cap names the parts behind another fence before those behind the step's own
    expect(partsFenceWords(asm, 3)).toBe(
      'parts behind different fences: High fence: Battery-management firmware, energy-management software and remote updates; Medium fence: the rest of the step, 1 more; Low fence: Enclosures, racking, thermal management and balance of system',
    );
    const d = node('Batteries|Refining');
    const one = { ...d!, yardChildren: [{ layerId: 'x-control', yard: 'small' as const, choice: 'secure' as const, variant: 'secure_control' }] };
    const agree = stepParts(core.meta, cell('Batteries', 'Refining'), one);
    expect([partsMixed(agree), partsFenceWords(agree)]).toEqual([false, 'both parts behind the High fence']);
    // the rest of the step's line: its kind of choice, else (no variant) its Secure rationale
    expect(agree[0].reason).toBe('A physical input a coercive supplier could withhold.');
    expect(stepParts(core.meta, { ...cell('Batteries', 'Refining'), variant: null }, { ...one, variant: null })[0].reason).toBe('Chokepoint risk.');
    const three = { ...one, yardChildren: [...one.yardChildren, { layerId: 'x-input', yard: null, choice: 'not_assessed' as const, variant: null }] };
    const withNa = stepParts(core.meta, cell('Batteries', 'Refining'), three);
    expect(withNa.map((p) => [p.layerId, p.fence])).toEqual([[null, 'high'], ['x-control', 'high'], ['x-input', null]]);
    expect([partChoiceWords(withNa[2]), partsFenceWords(withNa)]).toEqual(['Not assessed', 'parts behind the High fence, x input not assessed']);
    // a step not assessed, with a layer not assessed
    const na = stepParts(core.meta, cell('Batteries', 'Recovery'), { ...node('Batteries|Recovery')!, yardChildren: [{ layerId: 'x', yard: null, choice: 'not_assessed', variant: null }] });
    expect([na[0].verb, na[0].fence, na[0].reason, partsFenceWords(na)]).toEqual([null, null, 'Not assessed: the available data are not strong enough to make a strategic call.', 'both parts not assessed']);
  });
});

describe('the large-yard case and the prize families', () => {
  it("the node's case, its count against the trace's bar, its signals in plain words", () => {
    const ly = largeYardOf(core.meta, 'Batteries', 'Assembly', node('Batteries|Assembly'))!;
    expect(ly).toEqual({ holds: false, n: 2, bar: 5, signals: ['passive', 'substitutable', 'easy_qualification'], unknown: [], floor: false, own: false, reason: null });
    expect(largeYardWords(ly)).toBe('Does not hold: 2 commodity signals hold (5 needed).');
    // the count reads the commodity signals: the required ones are listed apart, so the two agree
    expect(splitSignals(ly.signals)).toEqual({ required: ['passive'], commodity: ['substitutable', 'easy_qualification'] });
    expect(splitSignals(ly.signals).commodity).toHaveLength(ly.n!);
    expect(signalWords(ly.signals)).toBe('passive, substitutable, quick to qualify');
    expect([countWords(1, null), countWords(null, 5)]).toEqual(['1 commodity signal holds', null]);
    // an undecided node; a node the core has no evidence for
    const und = largeYardOf(core.meta, 'Batteries', 'Extraction', node('Batteries|Extraction'))!;
    expect([und.holds, largeYardWords(und), und.unknown]).toEqual([null, 'Undecided: too little evidence observed (3 commodity signals hold).', ['diversified', 'low_learning']]);
    expect(largeYardOf(core.meta, 'Solar', 'Recovery', node('Solar|Recovery'))).toBeNull();
  });

  it("the economy's own case where it differs from the node's: a capability floor the node does not have", () => {
    const own = largeYardOf(core.meta, 'Solar', 'Extraction', node('Solar|Extraction'))!;
    expect([own.own, own.holds, own.floor, own.n, own.signals.length]).toEqual([true, true, true, 6, 8]);
    expect(largeYardWords(own)).toBe(
      'Holds: affordable, diversified access should dominate, whatever the size of the market (6 commodity signals hold); kept with a capability floor (moderate industrial learning).',
    );
    // the node's own case (no exception) has no floor
    expect(largeYardOf(core.meta, 'Solar', 'Extraction', null)).toMatchObject({ own: false, holds: true, floor: false });
  });

  it('the Strategic Industrial Value and its families: the market prize from the core (else the detail), commons, option and the cap from the node or layer evidence', () => {
    expect(sivFamilies(core.meta, cell('Batteries', 'Assembly'), node('Batteries|Assembly'))).toEqual({ value: 0.9, market: 0.85, commons: 1, option: 0.5, commonsN: 3, optionN: 4, cap: null });
    expect(sivFamilies(core.meta, cell('Batteries', 'Assembly'), null, 'bess-enclosure')).toMatchObject({ value: 0.9, commons: 0.5, option: 0, cap: 'market_only' });
    // no market prize in the core: the detail's
    expect(sivFamilies(core.meta, { ...cell('Batteries', 'Assembly'), market: null }, node('Batteries|Assembly')).market).toBe(0.85);
    expect(capWords('market_only')).toMatch(/^Market only: neither industrial commons nor future option reaches the gate/);
    expect(capWords(null)).toBeNull();
  });
});
