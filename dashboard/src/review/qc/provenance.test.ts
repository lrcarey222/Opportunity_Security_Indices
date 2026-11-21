// Tests of the "Why this result?" provenance chain (qc/provenance.ts): the hand-authored recipe map against the
// real config files (every key and measure it names must exist in config/strategic_position.yml or
// config/deployment_need.yml; every source id in config/sector_dictionary.yml), and the chain of a synthetic
// cell: every link has a status, gaps are listed, never hidden.

import { readFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import { parse } from 'yaml';
import { describe, expect, it } from 'vitest';
import { dictionaryFromYaml, provenanceChain, recipeKeys, recipeSourceIds } from './provenance.ts';
import { HAVE_REAL, loadReal, mkMeta, mkDetail, triageTrace } from './__tests__/fixtures.ts';

const repo = (rel: string) => fileURLToPath(new URL(`../../../../${rel}`, import.meta.url));
const yml = (rel: string): unknown => parse(readFileSync(repo(rel), 'utf8'));

/** The value at a dotted key, or undefined. */
function at(doc: unknown, key: string): unknown {
  let cur: unknown = doc;
  for (const part of key.split('.')) {
    if (cur === null || typeof cur !== 'object' || !(part in (cur as Record<string, unknown>))) return undefined;
    cur = (cur as Record<string, unknown>)[part];
  }
  return cur;
}

/** Every key and every string value in a subtree (a measure may be named as a key or as a value, e.g. a variable name). */
function names(node: unknown, out = new Set<string>()): Set<string> {
  if (Array.isArray(node)) for (const x of node) names(x, out);
  else if (node && typeof node === 'object')
    for (const [k, v] of Object.entries(node)) {
      out.add(k);
      names(v, out);
    }
  else if (typeof node === 'string') out.add(node);
  return out;
}

describe('the recipe map against the config files', () => {
  const docs: Record<string, unknown> = {
    'config/strategic_position.yml': yml('config/strategic_position.yml'),
    'config/deployment_need.yml': yml('config/deployment_need.yml'),
  };
  it.each(recipeKeys())('$id: $file $key exists with its measures', ({ file, key, measures }) => {
    const node = at(docs[file], key);
    expect(node, `${file} ${key}`).not.toBeUndefined();
    const inside = names(node);
    for (const m of measures) expect(inside.has(m), `${file} ${key} names ${m}`).toBe(true);
  });
  it('every source id the recipes cite is a source of config/sector_dictionary.yml', () => {
    const dict = dictionaryFromYaml(yml('config/sector_dictionary.yml'));
    const missing = recipeSourceIds().filter((id) => !dict.sources[id]);
    expect(missing).toEqual([]);
  });
});

describe('provenanceChain on a synthetic cell', () => {
  const meta = mkMeta(['Batteries'], { 'Batteries|Refining|stage': { cap: 'market_only', commons: 0.33, option: 0 } });
  const cell = mkDetail({
    choice: 'catch_up', need: 0.45, prize: 0.83, market: 0.78, cp: 0.31, share: 0.01, pathway: 'yes',
    trace: triageTrace({ need: 0.45, prize: 0.83, cp: 0.31, pos: 'not_top_quarter', pathway: 'yes', choice: 'catch_up' }),
    extra: { import_dependence: 0.44, china_share: 0.13, imports_to_gdp: 0.0002, cp_families: { export_position: 0.66 }, trade: { benchmark_source: 'comtrade', benchmark_share: 0.034 } },
  });
  const chain = provenanceChain(cell, { meta }, { sources: { comtrade_reported: { id: 'comtrade_reported', short: 'Comtrade', edition: '2026-10', licence: 'terms', accessed: null, transform: null } } }, 'AAA');
  it('runs choice -> trace -> components -> families -> raw -> transform -> source, every link with a status', () => {
    expect(chain.choice).toBe('catch_up');
    const levels = new Set(chain.links.map((l) => l.level));
    for (const l of ['choice', 'trace', 'component', 'family', 'raw', 'transform', 'source']) expect(levels.has(l as never), l).toBe(true);
    for (const l of chain.links) expect(['shipped', 'config_only', 'not_in_release']).toContain(l.status);
    const ids = chain.links.map((l) => l.id);
    expect(new Set(ids).size).toBe(ids.length);
    for (const l of chain.links) if (l.parent) expect(ids, `${l.id} -> ${l.parent}`).toContain(l.parent);
  });
  it('carries values, thresholds and results of the trace', () => {
    expect(chain.links.find((l) => l.id === 'trace:supply_chain_need')).toMatchObject({ value: 0.45, threshold: 0.67, result: 'low', status: 'shipped' });
    expect(chain.links.find((l) => l.id === 'raw:benchmark_share')).toMatchObject({ value: 0.034, status: 'shipped', vintage: expect.stringMatching(/2022-2024/) });
  });
  it('states every gap instead of hiding it', () => {
    expect(chain.links.find((l) => l.id === 'raw:import_hhi')!.status).toBe('not_in_release');
    expect(chain.links.find((l) => l.id === 'raw:contenders')!.status).toBe('not_in_release');
    expect(chain.links.find((l) => l.id === 'config:score_floor')!.status).toBe('config_only');
    expect(chain.gaps.length).toBe(chain.links.filter((l) => l.status !== 'shipped').length);
    expect(chain.gaps.join(' ')).toMatch(/not in this release/);
  });
});

describe.skipIf(!HAVE_REAL)('provenanceChain on the shipped bundle', () => {
  it('builds a chain for every cell of one economy', () => {
    const { decoded } = loadReal();
    const p = decoded.posture!;
    const dict = dictionaryFromYaml(yml('config/sector_dictionary.yml'));
    for (const c of p.details.KOR.cells) {
      const ch = provenanceChain(c, p.core, dict, 'KOR');
      expect(ch.links[0].level).toBe('choice');
      expect(ch.links.length).toBeGreaterThan(5);
    }
  });
});
