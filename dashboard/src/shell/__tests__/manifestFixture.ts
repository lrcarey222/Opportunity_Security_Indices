// A hand-made Sector manifest that passes isSectorManifest(), for the shell tests (Home's facts and
// coverage glyph) before and independently of builder A's synthetic bundle. It follows the contract: its schema
// version and stage slugs (six since schema 2, 2026-10-28: a 66-row coverage glyph).

import { HEADLINE_TECH_SLUGS, SECTOR_SCHEMA_VERSION, STAGE_SLUGS, type SectorManifest } from '../../sector/contract.ts';

const MD5 = 'd'.repeat(32);

export function sectorManifestFixture(over: Partial<SectorManifest> = {}): SectorManifest {
  return {
    schema: 'osi-sector',
    schema_version: SECTOR_SCHEMA_VERSION,
    bundle_id: 'fixture-bundle-0001',
    created: '2026-10-10T18:00:00Z',
    git_commit: null,
    generator: 'shell test fixture',
    phase: 1,
    strategic_bundle_id: null,
    processed_mtime_max: '2026-10-10T14:55:00Z',
    pipeline_marker: null,
    flags: { include_restricted: false, trade: false, atlas: false, policy_text: false, series: false },
    chunks: [
      { name: 'sector-meta', file: 'sector-meta.0123abcd.osb', bytes: 1000, bytes_gz: 500, md5: MD5, tech: null },
      { name: 'sector-facts', file: 'sector-facts.0123abcd.osb', bytes: 1000, bytes_gz: 500, md5: MD5, tech: null },
      { name: 'sector-policy', file: 'sector-policy.0123abcd.osb', bytes: 1000, bytes_gz: 500, md5: MD5, tech: null },
    ],
    headlines: [
      {
        id: 'supply',
        template: 'supply@1',
        values: { share: 0.737, value_kt: 235, total_kt: 319 },
        labels: { geo: 'CHN', geo_name: 'China', item: 'lithium', item_text: 'lithium' },
        source: 'IEA Critical Minerals Dataset 2026',
        year: '2025',
        href: '/sector/minerals/lithium',
      },
      // a headline of another rule version: its question shows no fact
      { id: 'reserves', template: 'reserves@0', values: { share: 0.5 }, labels: {}, source: 'USGS', year: 'end-2025', href: '/sector/explore/resources' },
    ],
    coverage_glyph: HEADLINE_TECH_SLUGS.flatMap((tech, i) => STAGE_SLUGS.map((stage, j) => ({ tech, stage, country_data: (i + j) % 3 !== 0 }))),
    notes: [],
    validation: [],
    sources: [],
    budget: { sector_gz: 1500, combined_gz: null, limit_sector_gz: 9 * 1024 * 1024, limit_combined_gz: 25 * 1024 * 1024 },
    encoding: { lossy: false },
    ...over,
  };
}
