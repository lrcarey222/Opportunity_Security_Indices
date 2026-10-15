// facts.notes and the assumed-zero rule (user edit 2b, 2026-10-11): an nsr balance or self-sufficiency
// with a value is drawn as a value with its note; notes travel with the fact rows; the CSV note column.

import { describe, expect, it } from 'vitest';
import { buildSectorFixture } from '../__fixtures__/build.ts';
import { factsIndexOf } from '../data/facts.ts';
import { modelOf } from '../data/model.ts';
import { validateChunk, refSizes, type SectorFactsChunk } from '../contract.ts';
import { stateOf, stateShort, stateWords } from './states.ts';
import { ASSUMED_ZERO_WORDS, assumedCount, assumedNote, isAssumedZero, noteLine, noteText, noteWords } from './notes.ts';
import { sectorCsv } from './csv.ts';

describe('assumed-zero facts', () => {
  it('isAssumedZero needs nsr, a finite value and a balance measure or the note', () => {
    expect(isAssumedZero('consumption.balance', { status: 'nsr', value: -0.4 })).toBe(true);
    expect(isAssumedZero('consumption.self_sufficiency', { status: 'nsr', value: 0 })).toBe(true);
    expect(isAssumedZero('consumption.balance', { status: 'nsr', value: NaN })).toBe(false);
    expect(isAssumedZero('consumption.balance', { status: 'obs', value: -0.4 })).toBe(false);
    expect(isAssumedZero('production.fuels', { status: 'nsr', value: 0 })).toBe(false);
    expect(isAssumedZero('production.fuels', { status: 'nsr', value: 0, notes: ['production_assumed_zero'] })).toBe(true);
  });

  it('note words: the assumed note first, EIA with its year for production, caveat fallback', () => {
    const caveats = new Map([['other_note', 'First sentence. Second sentence.']]);
    expect(noteWords({ caveats }, 'consumption.balance', { status: 'nsr', value: -1, obs_year: null })).toEqual([ASSUMED_ZERO_WORDS]);
    expect(noteWords({ caveats }, 'consumption.balance', { status: 'nsr', value: -1, obs_year: null, notes: ['fossil_assumed_zero', 'production_assumed_zero'] })[0]).toBe(ASSUMED_ZERO_WORDS);
    expect(noteText({ caveats }, 'eia_production', { varId: 'production.fuels', obsYear: 2024 })).toBe('EIA International Energy Statistics, 2024');
    expect(noteText({ caveats }, 'other_note')).toBe('First sentence');
    expect(noteLine({ caveats }, 'trade.exports', { status: 'obs', value: 1, obs_year: null })).toBe('');
    expect(assumedCount('consumption.balance', [{ status: 'nsr', value: -1 }, { status: 'obs', value: 2 }])).toBe(1);
    expect(assumedNote('consumption.self_sufficiency', 2)).toMatch(/2 economies.*0%/);
    expect(assumedNote('consumption.balance', 1)).toMatch(/1 economy.*minus consumption/);
  });

  it('the fixture: notes travel with the rows; the state reads "assumed 0"; the CSV has the note', () => {
    const fx = buildSectorFixture();
    const meta = validateChunk('sector-meta', fx.chunks['sector-meta']);
    const chunk = validateChunk('sector-facts', fx.chunks['sector-facts'], refSizes(meta)) as SectorFactsChunk;
    const facts = factsIndexOf(chunk);
    const m = modelOf(meta);
    const v = m.varById.get('consumption.balance')!;
    const gas = m.item('fuel', 'gas')!;
    const vnm = m.geoByCode.get('VNM')!;
    const row = facts.value(v.idx, gas.idx, 2025, vnm.idx)!;
    expect(row.status).toBe('nsr');
    expect(row.value).toBeCloseTo(-0.4);
    expect(row.notes).toEqual(['production_assumed_zero']);
    expect(facts.rows(v.idx, gas.idx, 2025).find((r) => r.geo === vnm.idx)?.notes).toEqual(['production_assumed_zero']);
    const st = stateOf(row, vnm, v, null, 2025);
    expect(st.assumed).toBe(true);
    expect(stateWords(st, v, m, vnm)).toBe(ASSUMED_ZERO_WORDS);
    expect(stateShort(st, v)).toBe('production assumed 0');
    const csv = sectorCsv(m, [{ geo: vnm, v, item: gas, year: 2025, value: row.value, status: row.status, note: noteLine(m, v.id, row) }]);
    expect(csv.split(/\r?\n/)[0].endsWith(',note')).toBe(true);
    expect(csv).toContain(ASSUMED_ZERO_WORDS);
    const deu = m.geoByCode.get('DEU')!;
    const p = facts.value(m.varById.get('production.fuels')!.idx, gas.idx, 2025, deu.idx)!;
    expect(noteLine(m, 'production.fuels', p)).toBe('EIA International Energy Statistics');
  });
});
