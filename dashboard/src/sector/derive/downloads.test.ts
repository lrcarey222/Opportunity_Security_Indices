// The Sources downloads (spec_sector.md 4.8): the "All sector data" ZIP has one CSV per family in the
// 17 sector columns plus dictionary.csv, sources.csv, coverage.csv and README.txt (status codes, the
// licensed-data line); series years facts do not hold are added once; the policy list and the flows of
// one technology carry their source and licence.

import { describe, expect, it } from 'vitest';
import { buildSectorFixture } from '../__fixtures__/build.ts';
import { refSizes, validateChunk } from '../contract.ts';
import { factsIndexOf, seriesIndexOf } from '../data/facts.ts';
import { modelOf } from '../data/model.ts';
import { SECTOR_CSV_HEADER } from './csv.ts';
import { allDataFiles, coverageRows, dictionaryRows, familyCsvs, flowsCsv, policyCsv, RESTRICTED_LINE } from './downloads.ts';

const fx = buildSectorFixture();
const meta = validateChunk('sector-meta', fx.chunks['sector-meta']);
const model = modelOf(meta);
const ref = refSizes(meta);
const facts = factsIndexOf(validateChunk('sector-facts', fx.chunks['sector-facts'], ref));
const series = seriesIndexOf(validateChunk('sector-series', fx.chunks['sector-series'], ref));

describe('downloads', () => {
  it('the ZIP files: one CSV per family in the sector columns, the dictionary, sources, coverage and README', () => {
    const files = allDataFiles(model, fx.manifest, facts, series, 'https://example.org/sector/sources');
    const names = Object.keys(files);
    for (const n of ['dictionary.csv', 'sources.csv', 'coverage.csv', 'README.txt', 'exports.csv', 'production.csv']) expect(names).toContain(n);
    for (const n of names.filter((x) => x.endsWith('.csv') && !['dictionary.csv', 'sources.csv', 'coverage.csv'].includes(x))) {
      expect(files[n].split(/\r?\n/)[0], n).toBe(SECTOR_CSV_HEADER);
    }
    expect(files['README.txt']).toContain('nq: listed without a figure');
    expect(files['README.txt']).toContain(RESTRICTED_LINE);
    expect(files['README.txt']).toContain(fx.manifest.bundle_id);
    expect(dictionaryRows(model)).toHaveLength(model.vars.length);
    expect(coverageRows(model).length).toBeGreaterThan(0);
  });

  it('series years outside the facts are added once; years in both are not duplicated', () => {
    const withS = familyCsvs(model, facts, series)['production.csv'].split(/\r?\n/).filter(Boolean).length;
    const withoutS = familyCsvs(model, facts, null)['production.csv'].split(/\r?\n/).filter(Boolean).length;
    expect(withS).toBeGreaterThan(withoutS);
    const lines = familyCsvs(model, facts, series)['production.csv'].split(/\r?\n/).filter(Boolean);
    // a row present in both chunks would be an identical line
    expect(lines.slice(1).filter((l, i, a) => a.indexOf(l) !== i)).toEqual([]);
  });

  it('the policy list and the flows of one technology', () => {
    const p = policyCsv(model, validateChunk('sector-policy', fx.chunks['sector-policy'], ref), validateChunk('sector-policy-text', fx.chunks['sector-policy-text'], ref));
    expect(p.split(/\r?\n/)[0]).toContain('title');
    expect(p).toContain('EU Batteries Regulation');
    expect(p).toContain('IEA Policies and Measures');
    const t = flowsCsv(model, validateChunk('sector-trade-batteries', fx.chunks['sector-trade-batteries'], ref));
    const head = t.split(/\r?\n/)[0];
    expect(head).toBe('exporter,exporter_name,importer,importer_name,var_id,item,year,value,unit,status,share_of_world,source,licence');
    expect(t).toContain(',2020,');
    expect(t).toContain(',2024,');
  });
});
