// The Methods page's placeholders (content/methods.md) are all filled from the data build, and the
// build counts (coalition tiers, Policy bases, the endowment screen) are computed from
// the shipped tables as stated.

import { readFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import { describe, expect, it } from 'vitest';
import { NA_BOOL } from '../../engine/numeric.ts';
import { table } from '../../engine/table.ts';
import type { BaselineInputs } from '../../engine/types.ts';
import { buildFacts, fillPlaceholders, methodsValues } from './methodsFill.ts';

const md = readFileSync(fileURLToPath(new URL('../../content/methods.md', import.meta.url)), 'utf8');

const bi = {
  policy_weights: { 'CAT Policy Index': 2, 'IEA PAMS Policy Index': 3 },
  fs_weights: { geo_alignment: 1, chinese_parent_exposure: 1 },
  fs_penalty: 0.1,
  po_penalty: 0.2,
  tau: 0.5,
  psi_view: 'coalition',
  require_resource_base: true,
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
    supply_chain: ['Upstream', 'Midstream', 'Upstream', 'Upstream'],
    strategic_index: Float64Array.from([0.5, 0.4, 0.3, NaN]),
    resource_base: Uint8Array.from([0, NA_BOOL, 1, 0]),
  }),
  allied: table({ Country: ['A', 'B'], supply_chain: ['Midstream', 'Upstream'] }),
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
      n_picked: '2', n_picked_upstream: '1',
    });
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
      '## In plain words', 'original allied network design', '259 six-digit basket codes', 'EIA International Energy Statistics',
      'Chinese-parent ownership weight inside Friendshore', '{{cpe_weight}}', 'Semiconductors (gallium, germanium and indium']) {
      expect(md).toContain(s);
    }
  });
});
