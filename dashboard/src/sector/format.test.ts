import { describe, expect, it } from 'vitest';
import { UNIT_KINDS } from './contract.ts';
import {
  lc,
  axisFormatter, fmtAria, fmtGrowth, fmtHhi, fmtShare, fmtSig, fmtSignedValue, fmtUsd, fmtValue, listText, ordinal, roundSig, unitAria, type UnitSpec,
} from './format.ts';

const U = (unit_kind: string, short = ''): UnitSpec => ({ unit_kind, short });

describe('format (3.9)', () => {
  it('usd is SI-scaled at 3 significant figures', () => {
    expect(fmtUsd(202.7e9)).toBe('USD 203 bn');
    expect(fmtUsd(6.1e9)).toBe('USD 6.1 bn');
    expect(fmtUsd(840e6)).toBe('USD 840 m');
    expect(fmtUsd(88.67e9)).toBe('USD 88.7 bn');
    expect(fmtUsd(1006e9)).toBe('USD 1.01 tn');
    expect(fmtValue(9.78e9, U('usd', 'USD'))).toBe('USD 9.78 bn');
    expect(fmtValue(21.4e9, U('usd', 'USD'))).toBe('USD 21.4 bn');
  });

  it('usd_bn_real carries 2024$', () => {
    expect(fmtValue(22.4, U('usd_bn_real', 'USD bn (2024$)'))).toBe('USD 22.4 bn (2024$)');
    expect(fmtValue(80.1, U('usd_bn_real'))).toBe('USD 80.1 bn (2024$)');
    expect(fmtValue(0.84, U('usd_bn_real'))).toBe('USD 840 m (2024$)');
  });

  it('shares and HHI', () => {
    expect(fmtShare(0.437)).toBe('43.7%');
    expect(fmtShare(0.0003)).toBe('<0.1%');
    expect(fmtShare(0)).toBe('0.0%');
    expect(fmtHhi(0.207)).toBe('0.207');
    expect(fmtHhi(0.2071, true)).toBe('≥ 0.21');
    expect(fmtValue(0.737, U('share'))).toBe('73.7%');
  });

  it('quantities use grouped thousands and 3 significant figures on tiles, full in tables', () => {
    expect(fmtValue(21034, U('kbd', 'kb/d'))).toBe('21,000 kb/d');
    expect(fmtValue(1.2345, U('ej', 'EJ'))).toBe('1.23 EJ');
    expect(fmtValue(21034.4, U('kbd', 'kb/d'), { ctx: 'table' })).toBe('21,034 kb/d');
    expect(fmtValue(1.2345, U('ej', 'EJ'), { ctx: 'table' })).toBe('1.23 EJ');
    expect(fmtValue(64.2, U('gj_pc', 'GJ per person'))).toBe('64.2 GJ per person');
    expect(fmtValue(0.064, U('gj_pc'), { scale: 1000 })).toBe('64 GJ per person');
  });

  it('prices, counts, ratios, TRL', () => {
    expect(fmtValue(79.4, U('price', 'USD/bbl'))).toBe('USD 79.4 /bbl');
    expect(fmtValue(33000, U('price', 'USD/t'))).toBe('USD 33,000 /t');
    expect(fmtValue(1234.4, U('count', 'records'))).toBe('1,234');
    expect(fmtValue(3.14159, U('ratio', 'ratio'))).toBe('3.14');
    expect(fmtValue(8.5, U('trl', 'TRL'))).toBe('TRL 8.5');
    expect(fmtValue(4.36, U('pct', '%'))).toBe('4.4%');
  });

  it('every unit kind formats a number without throwing and NA prints a dash', () => {
    for (const k of UNIT_KINDS) {
      expect(typeof fmtValue(12.5, U(k, 'u'))).toBe('string');
      expect(fmtValue(NaN, U(k, 'u'))).toBe('–');
      expect(fmtValue(null, U(k, 'u'))).toBe('–');
    }
    expect(fmtValue(5, U('some_new_kind', 'zz'))).toBe('5 zz');
  });

  it('signed values carry an arrow and a sign', () => {
    expect(fmtGrowth(0.124)).toBe('▲ +12.4%');
    expect(fmtGrowth(-0.031)).toBe('▼ −3.1%');
    expect(fmtSignedValue(-3.1, U('ej', 'EJ'))).toBe('▼ 3.1 EJ');
    expect(fmtSignedValue(0, U('ej', 'EJ'))).toBe('no change');
  });

  it('one scale per axis', () => {
    const f = axisFormatter([0, 50e9, 100e9, 203e9], U('usd'));
    expect([0, 50e9, 100e9, 200e9].map(f)).toEqual(['0bn', '50bn', '100bn', '200bn']);
    const g = axisFormatter([0, 2e9, 6.1e9], U('usd'));
    expect(g(2.5e9)).toBe('2.5bn');
    expect(g(500e6)).toBe('0.5bn');
    const s = axisFormatter([0, 0.5], U('share'));
    expect(s(0.25)).toBe('25%');
  });

  it('aria words spell units', () => {
    expect(unitAria(203e9, U('usd'))).toBe('US dollars, billions');
    expect(fmtAria(202.7e9, U('usd'))).toBe('203 billion US dollars');
    expect(fmtAria(0.437, U('share'))).toBe('43.7 percent');
  });

  it('helpers', () => {
    expect(roundSig(21034, 3)).toBe(21000);
    expect(fmtSig(0.012345)).toBe('0.0123');
    expect(listText(['a', 'b', 'c'])).toBe('a, b and c');
    expect(ordinal(1)).toBe('1st');
    expect(ordinal(4)).toBe('4th');
    expect(ordinal(22)).toBe('22nd');
    expect(ordinal(13)).toBe('13th');
  });
});

describe('labels in running text', () => {
  it('lower-cases capitalised words and keeps acronyms and units', () => {
    expect(lc('Mining (USGS)')).toBe('mining (USGS)');
    expect(lc('EV sales')).toBe('EV sales');
    expect(lc('Exports of the HS6 basket')).toBe('exports of the HS6 basket');
    expect(lc('Rare earths (magnet REEs)')).toBe('rare earths (magnet REEs)');
    expect(lc('Mean PV output, kWh/kWp per day')).toBe('mean PV output, kWh/kWp per day');
    expect(lc(null)).toBe('');
  });
});
