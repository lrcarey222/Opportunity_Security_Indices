// The Methods page's placeholders (content/methods.md) are all filled from the data build, and the
// build counts (coalition tiers, Policy bases, the endowment screen) are computed from
// the shipped tables as stated.

import { readFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import { describe, expect, it } from 'vitest';
import { NA_BOOL } from '../../engine/numeric.ts';
import { table } from '../../engine/table.ts';
import type { BaselineInputs } from '../../engine/types.ts';
import { buildFacts, excludedSectorsText, fillPlaceholders, methodsValues, stageWeightsText } from './methodsFill.ts';

const md = readFileSync(fileURLToPath(new URL('../../content/methods.md', import.meta.url)), 'utf8');

const bi = {
  policy_weights: { 'CAT Policy Index': 2, 'IEA PAMS Policy Index': 3 },
  fs_weights: { geo_alignment: 1, chinese_parent_exposure: 1 },
  fs_penalty: 0.1,
  po_penalty: 0.2,
  tau: 0.5,
  psi_view: 'coalition',
  require_resource_base: true,
  // the six-stage baseline (R SUPPLY_CHAIN_STAGE_SI_WEIGHTS; the shipped manifest's baseline_inputs)
  stage_weights: { Extraction: 0.5 / 3, Refining: 0.5 / 3, Components: 0.375, Assembly: 0.375, Deployment: 0.25, Recovery: 0.5 / 3 },
  excluded_sectors: ['Coal - Recovery', 'Geothermal - Assembly', 'Geothermal - Extraction', 'Green Hydrogen - Components', 'Green Hydrogen - Recovery', 'Wind - Recovery'],
} as unknown as BaselineInputs;

const facts = buildFacts({
  countries: [
    { tier: 'core' },
    { tier: 'core' },
    { tier: 'core' },
    { tier: 'partner' },
    { tier: 'none' },
    { tier: 'none' },
  ],
  eligibility: table({
    Country: ['A', 'B', 'C', 'D'],
    eligible: Uint8Array.from([1, 1, 1, 0]),
    policy_basis: ['IEA PAMS + CAT', 'IEA PAMS only', 'IEA PAMS + CAT', null],
  }),
  siCells: table({
    Country: ['A', 'A', 'B', 'C'],
    supply_chain: ['Extraction', 'Assembly', 'Extraction', 'Extraction'],
    strategic_index: Float64Array.from([0.5, 0.4, 0.3, NaN]),
    resource_base: Uint8Array.from([0, NA_BOOL, 1, 0]),
  }),
  allied: table({ Country: ['A', 'B', 'B'], supply_chain: ['Refining', 'Extraction', 'Components'] }),
});

describe('Methods placeholders', () => {
  it('every {{placeholder}} of methods.md has a value', () => {
    const values = methodsValues(bi, { flags: { geo_alignment_in_fs: true } }, facts);
    const keys = Array.from(md.matchAll(/\{\{(\w+)\}\}/g), (m) => m[1]);
    expect(keys.length).toBeGreaterThan(10);
    expect(keys.filter((k) => !(k in values))).toEqual([]);
    expect(fillPlaceholders(md, values)).not.toMatch(/\{\{\w+\}\}/);
  });

  it('counts the coalition, the Policy bases, the screen and the picks from the build', () => {
    expect(facts).toMatchObject({
      n_coalition: '4', n_core: '3', n_partners: '1',
      n_policy: '3', n_policy_both: '2', n_policy_pams_only: '1', n_policy_none: '1',
      n_si_cells: '3', n_si_countries: '2', n_screened: '2', n_screened_out: '1',
      n_picked: '2', n_picked_extraction: '1',
    });
    expect(facts).not.toHaveProperty('n_picked_upstream');
    const v = methodsValues(bi, { flags: {} }, facts);
    expect(v.lambda0).toBeUndefined(); // the retired network design's lambda is gone
    expect(v.screen_default).toBe('on');
    // the ownership component's weight and its hawkishness range (x 0.25 / x 4)
    expect([v.cpe_weight, v.cpe_lo, v.cpe_hi]).toEqual(['1', '0.25', '4']);
    expect(methodsValues({ ...bi, fs_weights: { geo_alignment: 1 } } as BaselineInputs, { flags: {} }).cpe_weight).toBe('not in this build');
    expect(methodsValues(bi, { flags: { fs_cmfdi: false } }).cpe_weight).toBe('not in this build');
    expect(methodsValues({ ...bi, require_resource_base: false } as BaselineInputs, { flags: {} }).screen_default).toBe('off (this build)');
  });

  it('the method facts of the final build are in the text', () => {
    for (const s of ['IEA PAMS is required', 'IEA PAMS + CAT', 'IEA PAMS only', 'endowment screen', 'Mineral Commodity Summaries 2026',
      'copper and aluminium', 'HS 851762', 'Hong Kong', '11 headline technologies', 'adds Brazil',
      // the final run (12 October 2026) and the CM-FDI parts of Friendshore (spec_country.md C4.4)
      '## The indices in plain words', 'original allied network design', '259 six-digit basket codes', 'EIA International Energy Statistics',
      'Chinese-parent ownership weight inside Friendshore', '{{cpe_weight}}', 'Semiconductors (gallium, germanium and indium']) {
      expect(md).toContain(s);
    }
  });

  it('the stage weights and their populism transform come from the build (six stages, chain order)', () => {
    const v = methodsValues(bi, { flags: {} }, facts);
    expect(v.stage_weights).toBe('Extraction, Refining and Recovery 0.167; Components and Assembly 0.375; Deployment 0.25');
    expect(v.stage_lambda_max).toBe('0.375');
    // lambda_max (w / lambda_max)^m: m(0) = 0.25 narrows the gap, m(100) = 4 widens it; the largest weight stays
    expect(v.stage_weights_lo).toBe('Extraction, Refining and Recovery 0.306; Components and Assembly 0.375; Deployment 0.339');
    expect(v.stage_weights_hi).toBe('Extraction, Refining and Recovery 0.0146; Components and Assembly 0.375; Deployment 0.0741');
    expect(stageWeightsText(null)).toBe('–');
    expect(stageWeightsText({ Recovery: 1, Extraction: 2 })).toBe('Extraction 2; Recovery 1');
    const none = methodsValues({ ...bi, stage_weights: undefined } as unknown as BaselineInputs, { flags: {} });
    expect([none.stage_weights, none.stage_weights_lo, none.stage_weights_hi, none.stage_lambda_max]).toEqual(['–', '–', '–', '–']);
  });

  it('the excluded sectors in words: technologies with the same stages together, more stages first', () => {
    const v = methodsValues(bi, { flags: {} }, facts);
    expect(v.n_excluded).toBe('6');
    expect(v.excluded_sectors).toBe('Geothermal (Extraction and Assembly); Green Hydrogen (Components and Recovery); Coal and Wind (Recovery)');
    expect(excludedSectorsText([])).toBe('–');
    expect(excludedSectorsText(['Oil - Recovery'])).toBe('Oil (Recovery)');
  });

  it('opens with the canonical framework (need, prize, position, choice, policy alignment), then the Legacy OSI chapter before the indices (design 5)', () => {
    const at = (x: string) => md.indexOf(x);
    expect(at('<h2 id="framework">')).toBe(0);
    const steps = ['**Need: what problem', '**Prize: is there', '**Position: how strong', '**Choice: what should', '**Policy alignment: does current policy'];
    expect(steps.map(at).every((v, k, a) => v > 0 && (k === 0 || v > a[k - 1]))).toBe(true);
    expect(md).toContain('Two separate measures, never combined into one score');
    expect(md).toContain('**Deploy · Accelerate**');
    expect(md).toContain('a deployment node is never Partner');
    expect(md).toContain('It is not the legacy Economic Opportunity index');
    expect(md).toContain('Observed policy is compared afterwards and never enters the choice');
    // the retired need x opportunity rule is gone
    expect(md).not.toMatch(/Competitive Position is a consistency check|high need and high opportunity, \*\*catch up\*\*|## Four steps/);
    expect(at('<h2 id="legacy-osi">')).toBeGreaterThan(at('**Policy alignment: does current policy'));
    expect(at('<h2 id="legacy-osi">')).toBeLessThan(at('## The indices in plain words'));
    expect(md).toContain('Grow ≈ Preserve, Upgrade ≈ Catch Up, Exposed ≈ Partner, Stable ≈ Watch');
    expect(md).toContain('mixes capability, vulnerability, production, trade, investment, demand, reserves, concentration and deployment');
    expect(md).toContain('## The indices in plain words (Legacy OSI)');
  });

  it('six stages only: no three-stage word, and no claim that the baseline is the paper exported exactly (S1, E3)', () => {
    expect(md).not.toMatch(/\b(upstream|midstream|downstream)\b/i);
    expect(md).not.toMatch(/three[- ]stage/i);
    expect(md).not.toMatch(/paper's (numbers|settings|weights|table|pillar)|reproduces the paper|the paper exactly|output for the paper/i);
    expect(md).toContain("At 50 every dial reproduces the pipeline's six-stage baseline exactly (the NEIS baseline)");
    expect(md).toContain('each of 66 sectors (11 energy technologies, each at the six stages of its supply chain');
    expect(md).toContain('{{n_picked_extraction}} of them at extraction');
    expect(md).toContain('an extraction sector qualifies only where the economy has recorded reserves or production');
    expect(md).toContain('at the components and assembly stages of Solar, Wind, Batteries and Electric Vehicles');
    expect(md).toContain('both stages carry the same projects, counted once');
    for (const k of ['{{stage_weights}}', '{{stage_weights_lo}}', '{{stage_weights_hi}}', '{{stage_lambda_max}}', '{{excluded_sectors}}']) expect(md).toContain(k);
  });
});
