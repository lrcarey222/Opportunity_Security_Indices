// The expertise tag taxonomy (review/tags.ts): normalisation to the shared vocabulary and the
// profile-vs-case match rule of design 5 step 3.

import { describe, expect, it } from 'vitest';
import type { CaseDefinition, ReviewerProfile } from './model.ts';
import {
  METHOD_TAGS,
  SECTOR_TAGS,
  STAGE_TAGS,
  normaliseCountryTag,
  normaliseMethodTag,
  normaliseProfile,
  normaliseSectorTag,
  normaliseStageTag,
  profileMatchesCase,
  reporterConfidence,
  tagLabel,
} from './tags.ts';

const profile = (p: Partial<ReviewerProfile>): ReviewerProfile => ({
  reviewer_id: 'r1',
  anonymous: true,
  countries: [],
  sectors: [],
  stages: [],
  methods: [],
  confidence: {},
  familiarity_neis: 0,
  familiarity_explorer: 0,
  advised_on: [],
  created_at: '2027-01-01T00:00:00Z',
  ...p,
});
type Tagged = Pick<CaseDefinition, 'sector_groups' | 'country_groups' | 'stage' | 'key'>;
const kase = (sectors: string[], countries: string[], stage: string | null = null): Tagged => ({ sector_groups: sectors, country_groups: countries, stage, key: {} });

describe('the shared vocabulary', () => {
  it('has exactly the agreed sector, stage and method tags', () => {
    expect([...SECTOR_TAGS]).toEqual(['batteries', 'coal', 'electric-grid', 'electric-vehicles', 'gas', 'geothermal', 'green-hydrogen', 'nuclear', 'oil', 'solar', 'wind', 'critical-minerals', 'semiconductors']);
    expect([...STAGE_TAGS]).toEqual(['Extraction', 'Refining', 'Components', 'Assembly', 'Deployment', 'Recovery']);
    expect(METHOD_TAGS).toHaveLength(10);
    expect(METHOD_TAGS).toContain('input_output_modelling');
  });

  it('normalises display names, slugs and aliases; rejects anything else', () => {
    expect(normaliseSectorTag('Electric Vehicles')).toBe('electric-vehicles');
    expect(normaliseSectorTag('electric_grid')).toBe('electric-grid');
    expect(normaliseSectorTag('EV')).toBe('electric-vehicles');
    expect(normaliseSectorTag('Green Hydrogen')).toBe('green-hydrogen');
    expect(normaliseSectorTag('steel')).toBeNull();
    expect(normaliseSectorTag(3)).toBeNull();
    expect(normaliseStageTag('refining')).toBe('Refining');
    expect(normaliseStageTag('Midstream')).toBeNull();
    expect(normaliseCountryTag(' kor ')).toBe('KOR');
    expect(normaliseCountryTag('Korea')).toBeNull();
    expect(normaliseMethodTag('Trade data')).toBe('trade_data');
    expect(normaliseMethodTag('input-output modelling')).toBe('input_output_modelling');
    expect(tagLabel('sector', 'electric-grid')).toBe('Electric Grid');
    expect(tagLabel('country', 'KOR')).toBe('KOR');
  });

  it('normalises a profile: unknown tags dropped, confidence keys canonical and clamped', () => {
    const p = normaliseProfile(profile({ sectors: ['Batteries', 'steel', 'batteries'], countries: ['kor'], stages: ['refining'], methods: ['Trade data'], confidence: { 'sector:Batteries': 7, 'country:kor': 4.4, 'stage:refining': 0, 'bogus:x': 5, nokind: 3 } }));
    expect(p.sectors).toEqual(['batteries']);
    expect(p.countries).toEqual(['KOR']);
    expect(p.stages).toEqual(['Refining']);
    expect(p.methods).toEqual(['trade_data']);
    expect(p.confidence).toEqual({ 'sector:batteries': 5, 'country:KOR': 4, 'stage:Refining': 1 });
  });
});

describe('profileMatchesCase (sector AND (country OR stage), rated >= min)', () => {
  const korBatteries = profile({ sectors: ['batteries'], countries: ['KOR'], confidence: { 'sector:batteries': 5, 'country:KOR': 5 } });

  it('is in on a rated sector and a rated country', () => {
    expect(profileMatchesCase(korBatteries, kase(['batteries'], ['KOR'], 'Components'), 3)).toBe('in');
  });
  it('is adjacent on the sector alone, outside on nothing shared', () => {
    expect(profileMatchesCase(korBatteries, kase(['batteries'], ['JPN'], 'Refining'), 3)).toBe('adjacent');
    expect(profileMatchesCase(korBatteries, kase(['solar'], ['KOR']), 3)).toBe('adjacent');
    expect(profileMatchesCase(korBatteries, kase(['nuclear'], ['USA']), 3)).toBe('outside');
  });
  it('counts a rated stage in place of a country', () => {
    const p = profile({ sectors: ['batteries'], stages: ['Refining'], confidence: { 'sector:batteries': 4, 'stage:Refining': 4 } });
    expect(profileMatchesCase(p, kase(['batteries'], ['JPN'], 'Refining'), 3)).toBe('in');
    expect(profileMatchesCase(p, kase(['batteries'], ['JPN'], 'Assembly'), 3)).toBe('adjacent');
  });
  it('needs the rating: a listed tag below the minimum is only adjacent', () => {
    const low = profile({ sectors: ['batteries'], countries: ['KOR'], confidence: { 'sector:batteries': 2, 'country:KOR': 5 } });
    expect(profileMatchesCase(low, kase(['batteries'], ['KOR']), 3)).toBe('adjacent');
    const unrated = profile({ sectors: ['batteries'], countries: ['KOR'] });
    expect(profileMatchesCase(unrated, kase(['batteries'], ['KOR']), 3)).toBe('adjacent');
  });
  it('treats a partnership (no sector groups) as in on a rated country', () => {
    expect(profileMatchesCase(korBatteries, kase([], ['KOR', 'USA']), 3)).toBe('in');
  });
  it('makes a methodology-wide case adjacent only for rated method expertise', () => {
    expect(profileMatchesCase(korBatteries, kase([], []), 3)).toBe('outside');
    const m = profile({ methods: ['trade_data'], confidence: { 'method:trade_data': 4 } });
    expect(profileMatchesCase(m, kase([], []), 3)).toBe('adjacent');
  });
  it('reads case tags written as display names', () => {
    expect(profileMatchesCase(korBatteries, kase(['Batteries'], ['kor']), 3)).toBe('in');
  });
});

describe('reporterConfidence', () => {
  it('is the highest matching self-rating / 5, null when nothing matches', () => {
    const p = profile({ sectors: ['batteries'], countries: ['KOR'], confidence: { 'sector:batteries': 3, 'country:KOR': 5 } });
    expect(reporterConfidence(p, { sectors: ['batteries'], countries: ['KOR'] })).toBe(1);
    expect(reporterConfidence(p, { sectors: ['batteries'] })).toBeCloseTo(0.6);
    expect(reporterConfidence(p, { sectors: ['solar'] })).toBeNull();
  });
});
