// The Sources & data downloads (spec_sector.md 4.8 "Downloads", 4.0 CSV rule). Pure text builders:
// one CSV per family in the 17 sector columns (facts, plus the series years facts do not hold),
// dictionary.csv, sources.csv, coverage.csv, README.txt (citations, licences, vintage, status codes),
// the policy list and the bilateral flows of one technology. UTF-8 BOM is added at download time.

import { noteLine } from './notes.ts';
import { rowsToCsv } from '../../lib/csv.ts';
import { FAMILY_IDS, type SectorManifest, type SectorPolicyChunk, type SectorTradeChunk } from '../contract.ts';
import type { FactsIndex } from '../data/facts.ts';
import type { SectorModel, VarInfo } from '../data/model.ts';
import { sectorCsv, type CsvInput } from './csv.ts';
import { policyRecords, policyTexts, safeUrl } from './policy.ts';
import type { SectorPolicyTextChunk } from '../contract.ts';
import { flowsOf, tradeItems, type TradeYear } from './trade.ts';

export const STATUS_WORDS: Record<string, string> = {
  obs: 'observed',
  zero: 'reported zero',
  nsr: 'not reported separately (inside the source residual)',
  nq: 'listed without a figure (withheld, or under 1% / not reported)',
  mirror: "from partners' declarations (exporter report x1.06)",
  est: 'estimate flagged by the source (CIM 2025 includes estimated Q4)',
  na_growth: 'base year zero (growth not defined)',
};

export const RESTRICTED_LINE = 'Licensed data (BNEF, BCG) are not part of the public explorer.';

/** The dictionary rows of a set of vars (all shipped vars by default). */
export function dictionaryRows(model: SectorModel, vars: readonly VarInfo[] = model.vars): Record<string, unknown>[] {
  return vars.map((v) => ({
    var_id: v.id,
    theme: v.theme,
    family: v.family,
    label: v.label,
    unit: v.unit.long || v.unit.short,
    data_year: v.data_year,
    level: v.native_level,
    item_axis: v.item_axis,
    additive: v.agg,
    world_basis: v.world_basis,
    source: v.source.short,
    licence: v.licence,
    phase: v.phase,
    provenance: v.prov,
    caveats: model.caveatTexts(v).join(' | '),
  }));
}

/** One row per source (citation, publisher, edition, URL, licence, accessed, attribution, transform, issues). */
export function sourcesRows(model: SectorModel, ids?: readonly string[]): Record<string, unknown>[] {
  const want = ids ? new Set(ids) : null;
  return [...model.sources.values()]
    .filter((s) => !want || want.has(s.id))
    .map((s) => ({ id: s.id, short: s.short, citation: s.citation, publisher: s.publisher, edition: s.edition, url: safeUrl(s.url), licence: s.licence, accessed: s.accessed, attribution: s.attribution, transform: s.transform, issues: s.issues }));
}

/** Coverage of every item x family (status, number of economies, reason, sources). */
export function coverageRows(model: SectorModel): Record<string, unknown>[] {
  const out: Record<string, unknown>[] = [];
  for (const it of model.items) {
    for (const f of FAMILY_IDS) {
      const c = model.coverage.get(`${it.idx}|${f}`);
      if (!c) continue;
      out.push({ item: it.slug, item_axis: it.axis, family: f, status: c.status, n_economies: c.n_geo, reason: c.reason, sources: c.sources.map((s) => model.sources.get(s)?.short ?? s).join('; ') });
    }
  }
  return out;
}

/** README.txt of the "All sector data" ZIP. */
export function readmeText(model: SectorModel, manifest: SectorManifest | null, url: string, extraLines: readonly string[] = []): string {
  const cites = [...model.sources.values()].map((s) => `  - ${s.short}${s.edition ? ` (${s.edition})` : ''}: ${s.citation}${s.licence === 'attribution' && s.attribution ? ` Attribution: ${s.attribution}` : ''}`);
  return [
    'Allied Industrial Base Explorer: Sector Insights, all sector data',
    `From: ${url}`,
    `Sector data bundle: ${manifest?.bundle_id ?? 'unknown'} (export ${manifest?.created ?? 'unknown'}${manifest?.git_commit ? `, git ${manifest.git_commit.slice(0, 10)}` : ''})`,
    '',
    'Measured values in each source’s own units and years. No index, weight or score values.',
    'One CSV per measure family; columns: iso3c, economy, group, tier, eu_member, var_id, measure, item, year, value, unit, status, share_of_world, world_basis, rank, source, licence, note. NA is empty.',
    'note: the fact’s own notes, e.g. "Production not reported separately; assumed 0" (a net balance or self-sufficiency with status nsr and a value: the production no source reports separately counts as 0) or "EIA International Energy Statistics, 2024".',
    'dictionary.csv: every measure (unit, data year, level, source, licence, caveats). sources.csv: every source. coverage.csv: items x families.',
    ...extraLines,
    '',
    'Status codes:',
    ...Object.entries(STATUS_WORDS).map(([k, w]) => `  ${k}: ${w}`),
    '',
    'Sources and licences (attribution required where stated):',
    ...cites,
    '',
    RESTRICTED_LINE,
    '',
  ].join('\r\n');
}

/**
 * One CSV per family: every fact row (and every series row in a year the facts do not hold), in the
 * 18 sector columns. Returns { "trade.csv": text, ... } for families with rows.
 */
export function familyCsvs(model: SectorModel, facts: FactsIndex, series: FactsIndex | null): Record<string, string> {
  const byFam = new Map<string, CsvInput[]>();
  const push = (v: VarInfo, it: number, year: number, idx: FactsIndex) => {
    const item = model.items[it] ?? null;
    let list = byFam.get(v.family);
    if (!list) byFam.set(v.family, (list = []));
    const w = idx.world(v.idx, it, year);
    for (const r of idx.rows(v.idx, it, year)) {
      const g = model.geos[r.geo] ?? null;
      list.push({ geo: g && g.kind !== 'residual' ? g : null, economy: g && g.kind === 'residual' ? g.name : undefined, v, item, year: r.obs_year ?? year, value: Number.isFinite(r.value) ? r.value : null, status: r.status, share: w && Number.isFinite(r.value) && w.value > 0 ? r.value / w.value : null, note: noteLine(model, v.id, r) });
    }
  };
  for (const v of model.vars) {
    for (const it of facts.itemsOf(v.idx)) for (const y of facts.yearsOf(v.idx, it)) push(v, it, y, facts);
    if (series) for (const it of series.itemsOf(v.idx)) for (const y of series.yearsOf(v.idx, it)) if (!facts.has(v.idx, it, y)) push(v, it, y, series);
  }
  const out: Record<string, string> = {};
  for (const [fam, rows] of byFam) if (rows.length) out[`${fam}.csv`] = sectorCsv(model, rows);
  return out;
}

/** The README line of the CM-FDI projects file in the ZIP (C5.8: CC BY, changes indicated). */
export const CMFDI_ZIP_LINE = 'cm-fdi-projects.csv: every cross-border clean-tech manufacturing project of the Cleantech Manufacturing FDI dataset (CM-FDI) as published, one row per project (CC BY 4.0; changes: project counts and sums of disclosed capital elsewhere in this explorer were computed by the OSI authors; no record altered).';

/**
 * The files of "All sector data (.zip)". `extra` adds files built from other chunks (the CM-FDI
 * projects CSV) with their README lines.
 */
export function allDataFiles(model: SectorModel, manifest: SectorManifest | null, facts: FactsIndex, series: FactsIndex | null, url: string, extra: { files: Record<string, string>; readme: string[] } | null = null): Record<string, string> {
  return {
    ...familyCsvs(model, facts, series),
    ...(extra?.files ?? {}),
    'dictionary.csv': rowsToCsv(dictionaryRows(model)),
    'sources.csv': rowsToCsv(sourcesRows(model)),
    'coverage.csv': rowsToCsv(coverageRows(model)),
    'README.txt': readmeText(model, manifest, url, extra?.readme ?? []),
  };
}

/** The policy list (IEA PAMS records with titles and IEA links when the text chunk is given). */
export function policyCsv(model: SectorModel, chunk: SectorPolicyChunk, text: SectorPolicyTextChunk | null): string {
  const texts = policyTexts(text);
  return rowsToCsv(
    policyRecords(chunk).map((r) => ({
      id: r.id,
      iso3c: model.geos[r.geo]?.code ?? null,
      jurisdiction: model.geos[r.geo]?.name ?? null,
      level: r.jurisdiction,
      title: texts.get(r.id)?.title ?? null,
      url: safeUrl(texts.get(r.id)?.url),
      year: r.year,
      status: r.status,
      type: r.type,
      tech_tags: r.tags.join('|'),
      stage_tags: r.stages.join('|'),
      tag_source: r.tech_src,
      cross_cutting: r.cross_cutting,
      source: 'IEA Policies and Measures',
      licence: 'attribution',
    })),
  );
}

/** Every bilateral flow of a technology's trade chunk (each cell, 2020 and 2024). */
export function flowsCsv(model: SectorModel, trade: SectorTradeChunk): string {
  const v = model.varById.get('trade.flow');
  const rows: Record<string, unknown>[] = [];
  for (const it of tradeItems(trade)) {
    for (const y of [2020, 2024] as TradeYear[]) {
      const fl = flowsOf(trade, it, y);
      const W = fl.reduce((a, f) => a + f.v, 0);
      for (const f of [...fl].sort((a, z) => z.v - a.v)) {
        rows.push({ exporter: model.geos[f.e]?.code, exporter_name: model.geos[f.e]?.name, importer: model.geos[f.i]?.code, importer_name: model.geos[f.i]?.name, var_id: 'trade.flow', item: model.items[it]?.slug, year: y, value: f.v, unit: 'USD', status: f.s === 'mirror' ? 'mirror' : 'obs', share_of_world: W > 0 ? f.v / W : null, source: v?.source.short ?? 'UN Comtrade', licence: v?.licence ?? 'open' });
      }
    }
  }
  return rowsToCsv(rows);
}
