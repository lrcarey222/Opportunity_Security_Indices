// Plain-language helpers of derive/text.ts (spec_country.md C3.1: no bare acronym in titles, legends or headers).

import { describe, expect, it } from 'vitest';
import { scenarioWords } from './text.ts';

describe('scenarioWords', () => {
  it('spells out the IEA scenario codes and keeps an unknown one', () => {
    expect(scenarioWords('STEPS')).toBe('stated-policies');
    expect(scenarioWords('APS')).toBe('announced-pledges');
    expect(scenarioWords(null)).toBe('stated-policies');
    expect(scenarioWords('Other')).toBe('Other');
  });
});
