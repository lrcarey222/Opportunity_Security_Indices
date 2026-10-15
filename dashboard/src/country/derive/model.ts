// View models of Country Insights (spec_country.md A4): pure functions from the manifest and one fact
// sheet to the rows the cards draw. Nothing here recomputes an index value: the R exporter derived every
// number (A9); these only select, order and lay out what the sheet carries.

import {
  cellOf, COUNTRY_TECHS, NO_ES_CELLS, THIRDS, type CellFacts, type CountryEntry, type CountryManifest, type Driver, type DriversDoc,
  type FactSheet, type NipoPolicy, type PartnerRow, type Quadrant, type Third,
} from '../contract.ts';
import { QUADRANTS } from '../contract.ts';

// ---- the roster ------------------------------------------------------------------------------------

export const GROUP_LABEL = { core: 'Coalition core', partner: 'Coalition partners', none: 'Other economies' } as const;

export interface RosterGroups {
  core: CountryEntry[];
  partner: CountryEntry[];
  none: CountryEntry[];
}

const byName = (a: CountryEntry, b: CountryEntry) => a.name.localeCompare(b.name, 'en');

export function rosterGroups(m: CountryManifest): RosterGroups {
  const g: RosterGroups = { core: [], partner: [], none: [] };
  for (const c of m.countries) g[c.tier].push(c);
  g.core.sort(byName);
  g.partner.sort(byName);
  g.none.sort(byName);
  return g;
}

/** Options for the shared CountrySelect, grouped Coalition core / partners / other. */
export function countryOptions(m: CountryManifest): { iso3c: string; country: string; group: string }[] {
  const g = rosterGroups(m);
  return (['core', 'partner', 'none'] as const).flatMap((t) =>
    g[t].map((c) => ({ iso3c: c.iso3, country: c.status === 'no_data' ? `${c.name} (no index data)` : c.name, group: GROUP_LABEL[t] })),
  );
}

/** "Grow 4 · Upgrade 6 · Secure 9 · Watch 3" */
export function quadrantLine(q: Record<Quadrant, number> | null | undefined, words?: Record<string, string>): string | null {
  if (!q) return null;
  return QUADRANTS.map((x) => `${words?.[x.id] ?? x.label} ${q[x.id] ?? 0}`).join(' · ');
}

// ---- the scatter ---------------------------------------------------------------------------------

/** One placed sector on the quadrant scatter: x = Economic Opportunity, y = Energy Security (the plotted
 *  positions eo.pos and es.pos, 2026-10-15). */
export interface ScatterPoint {
  key: string;
  label: string;
  short: string;
  tech: string;
  stage: string;
  x: number;
  y: number;
  quadrant: Quadrant;
  top5: boolean;
  priority: boolean;
  cell: CellFacts;
}

export function scatterPoints(sheet: FactSheet): ScatterPoint[] {
  const prio = new Set(sheet.priority_sectors);
  const out: ScatterPoint[] = [];
  for (const c of sheet.cells) {
    if (!c.placed || !c.es || !c.eo || c.es.pos === null || c.eo.pos === null || !c.quadrant) continue;
    const st = c.stage === 'Upstream' ? 'Up' : c.stage === 'Midstream' ? 'Mid' : 'Down';
    out.push({
      key: c.key, label: c.label, short: `${c.tech} · ${st}`, tech: c.tech, stage: c.stage, x: c.eo.pos, y: c.es.pos,
      quadrant: c.quadrant, top5: !!c.si?.top5, priority: prio.has(c.key), cell: c,
    });
  }
  return out;
}

/** Cells by corner, each ordered by Strategic Index rank (sectors outside the sample last, in cell order). */
export function cellsByQuadrant(sheet: FactSheet): Record<Quadrant, CellFacts[]> {
  const out: Record<Quadrant, CellFacts[]> = { grow: [], upgrade: [], secure: [], watch: [] };
  const rank = (c: CellFacts) => (c.si?.in_sample && c.si.rank.v !== null ? c.si.rank.v : 1e9);
  sheet.cells.forEach((c) => {
    if (c.placed && c.quadrant) out[c.quadrant].push(c);
  });
  for (const q of Object.keys(out) as Quadrant[]) out[q].sort((a, b) => rank(a) - rank(b));
  return out;
}

// ---- priorities and policy -------------------------------------------------------------------------

export const SI_PART_KEYS = ['eo', 'need', 'pol', 'stage', 'tech'] as const;
export type SiPartKey = (typeof SI_PART_KEYS)[number];
export const SI_PART_LABEL: Record<SiPartKey, string> = {
  eo: 'Opportunity',
  need: 'Security need',
  pol: 'Policy activity',
  stage: 'Stage',
  tech: 'Technology',
};
export const SI_PART_SHORT: Record<SiPartKey, string> = { eo: 'Opportunity', need: 'Need', pol: 'Policy', stage: 'Stage', tech: 'Tech' };
export const SI_PART_GLOSSARY: Record<SiPartKey, string> = { eo: 'eo', need: 'need', pol: 'policy_index', stage: 'stage', tech: 'si' };

/** In-sample sectors by Strategic Index rank. */
export function priorityRows(sheet: FactSheet): CellFacts[] {
  return sheet.summary.si_order.map((k) => cellOf(sheet, k)).filter((c): c is CellFacts => !!c && !!c.si?.in_sample);
}

/** The largest single SI part among the rows (the shared x scale of the facet bars). */
export function maxPart(rows: CellFacts[]): number {
  let m = 0;
  for (const c of rows) for (const k of SI_PART_KEYS) m = Math.max(m, c.si.parts?.[k]?.v ?? 0);
  return m > 0 ? m : 1;
}

/** Thirds in display order with their words. */
export const PRIORITY_THIRD_LABEL: Record<Third, string> = { high: 'Top third', middle: 'Middle third', low: 'Bottom third' };
export const POLICY_THIRD_LABEL: Record<Third, string> = { high: 'Most policy', middle: 'Middle', low: 'Least policy' };

/** The caption of a matrix cell (descriptive, never judging). */
export function matrixCaption(p: Third, q: Third): string {
  if (p === 'high' && q === 'high') return 'TOP PRIORITY, MOST POLICY';
  if (p === 'high' && q === 'low') return 'TOP PRIORITY, LEAST POLICY';
  if (p === 'low' && q === 'high') return 'BOTTOM PRIORITY, MOST POLICY';
  if (p === 'low' && q === 'low') return 'BOTTOM PRIORITY, LEAST POLICY';
  return 'IN BETWEEN';
}

export function matrixCells(sheet: FactSheet): { p: Third; q: Third; key: string; cells: CellFacts[]; diagonal: boolean }[] {
  const ag = sheet.policy.agreement;
  const out: { p: Third; q: Third; key: string; cells: CellFacts[]; diagonal: boolean }[] = [];
  for (const p of THIRDS) {
    for (const q of THIRDS) {
      const key = `${p}_${q}`;
      const keys = ag?.matrix?.[key] ?? [];
      out.push({ p, q, key, cells: keys.map((k) => cellOf(sheet, k)).filter((c): c is CellFacts => !!c), diagonal: p === q });
    }
  }
  return out;
}

/** The PolicyCoverage table: technologies x stages of records_specific, plus "any stage". */
export function coverageRows(sheet: FactSheet): { tech: string; slug: string; cells: { stage: string; key: string; v: number | null; d: string }[]; any: { v: number | null; d: string } }[] {
  const cov = sheet.policy.coverage;
  return cov.rows.map((tech, i) => {
    const slug = COUNTRY_TECHS.find((t) => t.label === tech)?.slug ?? tech.toLowerCase().replace(/[^a-z0-9]+/g, '-');
    return {
      tech,
      slug,
      cells: cov.cols.map((st, j) => ({ stage: st, key: `${slug}-${st.toLowerCase()}`, v: cov.specific[i]?.[j]?.v ?? null, d: cov.specific[i]?.[j]?.d ?? '–' })),
      any: { v: cov.tech_any_stage[i]?.v ?? null, d: cov.tech_any_stage[i]?.d ?? '–' },
    };
  });
}

// ---- NIPO ----------------------------------------------------------------------------------------

/** The top NIPO measures of a cell (strongest first). */
export function nipoOfCell(sheet: FactSheet, key: string | null | undefined): NipoPolicy[] {
  const n = sheet.policy.nipo;
  if (!n || !key) return [];
  const ids = n.cells?.[key] ?? [];
  const byId = new Map(n.policies.map((p) => [p.id, p]));
  return ids.map((i) => byId.get(i)).filter((p): p is NipoPolicy => !!p);
}

/** Cells with at least one listed measure, in the Sector cell order. */
export function nipoCells(sheet: FactSheet): CellFacts[] {
  const n = sheet.policy.nipo;
  if (!n) return [];
  return sheet.cells.filter((c) => (n.cells?.[c.key]?.length ?? 0) > 0);
}

// ---- drivers ---------------------------------------------------------------------------------------

/** The drivers of a cell: from the sheet, else from the companion drivers file. */
export function driversOf(sheet: FactSheet, key: string | null | undefined, doc: DriversDoc | null): { es: Driver[]; eo: Driver[] } | null {
  const c = cellOf(sheet, key);
  if (!c) return null;
  if (c.drivers_in_sheet === false) {
    const d = doc?.cells?.[c.key];
    return d ?? null;
  }
  return c.drivers;
}

/** Driver categories for the bars: ES ascending by impact, EO descending. */
export function driverBars(ds: Driver[], pillar: 'es' | 'eo'): Driver[] {
  const imp = (d: Driver) => d.impact.v ?? 0;
  return [...ds].sort((a, b) => (pillar === 'es' ? imp(a) - imp(b) : imp(b) - imp(a)));
}

/** The default sector of the "why" panel: the first high-priority sector, else the top SI sector, else the first placed. */
export function defaultCell(sheet: FactSheet): string | null {
  return sheet.priority_sectors[0] ?? sheet.summary.si_order[0] ?? sheet.cells.find((c) => c.placed)?.key ?? null;
}

// ---- partners --------------------------------------------------------------------------------------

export interface SlopeSide {
  iso3: string;
  name: string;
  group: PartnerRow['group'];
  rank: number;
  value: string;
  other: number | null;
}

/** The two columns of the PartnerSlope card: top n by Partnership Strength, top n by average trade share. */
export function slopeModel(sheet: FactSheet, n = 10): { left: SlopeSide[]; right: SlopeSide[] } {
  const rows = new Map(sheet.partners.rows.map((r) => [r.iso3, r]));
  const leftIso = sheet.partners.psi_order.slice(0, n);
  const rightIso = sheet.partners.tie_order.slice(0, n);
  const left: SlopeSide[] = [];
  leftIso.forEach((iso, i) => {
    const r = rows.get(iso);
    if (!r) return;
    left.push({ iso3: iso, name: r.name, group: r.group, rank: i + 1, value: r.psi.d, other: r.ties.tie_rank.v ?? null });
  });
  const right: SlopeSide[] = [];
  rightIso.forEach((iso, i) => {
    const r = rows.get(iso);
    if (!r) return;
    right.push({
      iso3: iso, name: r.name, group: r.group, rank: i + 1,
      value: `${r.ties.imp_mean.d} of imports · ${r.ties.exp_mean.d} of exports on average`,
      other: r.rankable ? (r.rank.v ?? null) : null,
    });
  });
  return { left, right };
}

/** The partner table rows: the top 15 ranked plus any top-10 trade partner not among them (marked). */
export function partnerTableRows(sheet: FactSheet, nTop = 15): (PartnerRow & { extra: boolean })[] {
  const rows = sheet.partners.rows;
  const ranked = rows.filter((r) => r.rankable && r.rank.v !== null).sort((a, b) => (a.rank.v as number) - (b.rank.v as number));
  const top = ranked.slice(0, nTop);
  const inTop = new Set(top.map((r) => r.iso3));
  const extra = rows.filter((r) => !inTop.has(r.iso3) && r.in_tie_top).sort((a, b) => (a.ties.tie_rank.v ?? 1e9) - (b.ties.tie_rank.v ?? 1e9));
  return [...top.map((r) => ({ ...r, extra: false })), ...extra.map((r) => ({ ...r, extra: true }))];
}

export const GROUP_WORD: Record<PartnerRow['group'], string> = { china: 'China', core: 'Core', partner: 'Partner', rest: 'Other' };

/** The supplier matrix as rows (partners) x columns (cells) with shares (null = no imports from it). */
export function supplierModel(sheet: FactSheet): {
  partners: { iso3: string; name: string; group: PartnerRow['group'] }[];
  cells: CellFacts[];
  share: (number | null)[][];
  value: (number | null)[][];
  status: (string | null)[][];
} {
  const sm = sheet.partners.supplier_matrix;
  const rows = new Map(sheet.partners.rows.map((r) => [r.iso3, r]));
  const cells = sm.cells.map((k) => cellOf(sheet, k)).filter((c): c is CellFacts => !!c);
  const keep = sm.cells.map((k) => !!cellOf(sheet, k));
  const col = <T,>(xs: T[][]) => xs.map((r) => r.filter((_, j) => keep[j]));
  return {
    partners: sm.partners.map((iso) => ({ iso3: iso, name: rows.get(iso)?.name ?? iso, group: rows.get(iso)?.group ?? 'rest' })),
    cells,
    share: col(sm.share ?? []),
    value: col(sm.value ?? sm.share.map((r) => r.map(() => null))),
    status: col((sm.status ?? sm.share.map((r) => r.map(() => null))) as (string | null)[][]),
  };
}

// ---- misc ------------------------------------------------------------------------------------------

/** Every cell that has no Energy Security score in the paper (A9.1). */
export const isNoEsCell = (k: string): boolean => NO_ES_CELLS.includes(k);

/** "facts-JPN.3f2a9c1e.json" -> "japan-fact-sheet-2026-10-12.json" */
export function factSheetDownloadName(sheet: FactSheet): string {
  return `${sheet.meta.country.toLowerCase().replace(/[^a-z0-9]+/g, '-').replace(/^-|-$/g, '')}-fact-sheet-${sheet.meta.as_of}.json`;
}

/** The memo download name: "{Country}-memo-{as_of}.pdf". */
export function memoDownloadName(country: string, asOf: string, ext: 'pdf' | 'docx' | 'md'): string {
  return `${country.replace(/[^A-Za-z0-9]+/g, '-').replace(/^-|-$/g, '')}-memo-${asOf}.${ext}`;
}
