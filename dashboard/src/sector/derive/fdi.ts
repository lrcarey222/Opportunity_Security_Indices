// CM-FDI cross-border investment projects (sector-fdi; build_log "sector-data track: CM-FDI"). Pure:
// the projects as objects (an undisclosed capital investment is null, never 0), their source cells
// with the http(s) links CM-FDI cites, project filters (technology, basis, status, activity, economies)
// and the sums the views draw: home -> destination flows, projects by destination or by investor
// economy, the split of a set of projects by the parent's headquarters group, and announcements by
// year and group. Sums run over the projects in table order, so re-summing the projects of a (cell,
// basis) reproduces the chunk's fdi.flows / fdi.groups / fdi.years exactly (fdi.test.ts checks it).
// Raw counts and disclosed sums only: nothing here is an index, a weight or a score.

import { NA_BOOL, NA_INT } from '../../engine/numeric.ts';
import { rowsToCsv } from '../../lib/csv.ts';
import { FDI_GROUPS, FDI_SOURCE_FIELDS, FDI_SOURCE_LABELS, FDI_VAR_IDS, type FdiBasis, type FdiSourceField, type SectorFdiChunk } from '../contract.ts';
import type { GeoInfo, ItemInfo, SectorModel, SourceInfo } from '../data/model.ts';

export type FdiGroup = (typeof FDI_GROUPS)[number];
/** What a card counts: projects, or their disclosed capital investment (US$ m). */
export type FdiShow = 'n' | 'value';

export const FDI_GROUP_ORDER: readonly FdiGroup[] = FDI_GROUPS;

/** Legend words of the parent-headquarters groups (China includes Hong Kong and Macao here). */
export const FDI_GROUP_WORDS: Record<FdiGroup, string> = {
  china: 'China (incl. Hong Kong, Macao)',
  core: 'Coalition core',
  partner: 'Coalition partners',
  rest: 'Rest of world',
};

/** Short words of the groups for table headers and cells. */
export const FDI_GROUP_SHORT: Record<FdiGroup, string> = {
  china: 'China',
  core: 'Coalition core',
  partner: 'Coalition partner',
  rest: 'Rest of world',
};

/** The CM-FDI measures (dictionary ids; their data live in sector-fdi). */
export const FDI_VAR_SET: ReadonlySet<string> = new Set<string>(FDI_VAR_IDS);

/** Is a measure one of the CM-FDI measures? */
export const isFdiVar = (id: string): boolean => FDI_VAR_SET.has(id);

/** Does a cell have CM-FDI projects in this build (meta.avail rows in the fdi chunk)? */
export function hasFdi(model: SectorModel, item: ItemInfo | null | undefined): boolean {
  if (!item) return false;
  return FDI_VAR_IDS.some((id) => {
    const v = model.varById.get(id);
    return !!v && model.availOf(v, item.idx).some((a) => a.chunk === 'fdi');
  });
}

/** The words of a value that CM-FDI does not disclose (never 0). */
export const NOT_DISCLOSED = 'not disclosed';

export interface FdiProject {
  /** 0-based row of fdi.projects */
  row: number;
  pid: string;
  /** dims.items row of the Midstream cell */
  item: number;
  /** CM-FDI's technology label ("Battery", "EV", "Solar", "Wind") */
  technology: string;
  parent: string;
  /** dims.geo row of the parent's headquarters economy */
  home: number;
  /** dims.geo row of the destination */
  dest: number;
  city: string | null;
  status: string;
  operational: string;
  activity: string;
  yearAnnounced: number;
  /** the planned year when not complete; null = "N/A" or blank */
  yearCompleted: number | null;
  /** capital investment, US$ m as published; null = not disclosed */
  value: number | null;
  expansion: boolean;
  phase: string | null;
  jv: boolean;
  jvLocal: boolean;
  /** null = blank in CM-FDI */
  jvLocalState: boolean | null;
  jvCompany: string | null;
  manufacturing: boolean;
  active: boolean;
  /** one of the projects the Partnership indices use */
  indexUse: boolean;
  homeGroup: FdiGroup;
}

export interface FdiSourceCell {
  field: FdiSourceField;
  /** CM-FDI's own heading ("Project status") */
  label: string;
  /** the cell verbatim */
  text: string;
  /** the text without its links and without the [UNVERIFIED] mark ("(year only)", "fDi Markets (FT)") */
  note: string;
  /** the http(s) links of the cell (only these are ever rendered as links) */
  urls: string[];
  unverified: boolean;
}

export interface FdiData {
  chunk: SectorFdiChunk;
  projects: FdiProject[];
  /** the source cells of a project row, in CM-FDI's column order */
  sourcesOf: (row: number) => FdiSourceCell[];
}

const i32 = (x: number): number | null => (x === NA_INT ? null : x);
const b = (x: number): boolean => x === 1;
const bn = (x: number): boolean | null => (x === NA_BOOL ? null : x === 1);
const URL_RE = /https?:\/\/\S+/gi;

/** A link CM-FDI cites, or null unless it is a clean http(s) URL. */
export function safeHttpUrl(u: string | null | undefined): string | null {
  if (!u) return null;
  try {
    const x = new URL(u);
    return x.protocol === 'https:' || x.protocol === 'http:' ? x.href : null;
  } catch {
    return null;
  }
}

/** The host of a link for its text ("www.reuters.com" -> "reuters.com"). */
export function hostOf(u: string): string {
  try {
    return new URL(u).hostname.replace(/^www\./, '');
  } catch {
    return u;
  }
}

/** The words of a source cell once its links and the [UNVERIFIED] mark are taken out. */
export function sourceNote(text: string): string {
  return text.replace(URL_RE, ' ').replace(/\[UNVERIFIED\]/gi, ' ').replace(/\s+/g, ' ').trim();
}

const cache = new WeakMap<SectorFdiChunk, FdiData>();

/** The chunk as project objects and source cells (built once per chunk object). */
export function fdiData(chunk: SectorFdiChunk): FdiData {
  const hit = cache.get(chunk);
  if (hit) return hit;
  const p = chunk['fdi.projects'].cols;
  const projects: FdiProject[] = [];
  for (let i = 0; i < chunk['fdi.projects'].nrow; i++) {
    const v = p.value[i];
    projects.push({
      row: i,
      pid: p.pid[i] ?? '',
      item: p.item[i],
      technology: p.technology[i] ?? '',
      parent: p.parent[i] ?? '',
      home: p.home[i],
      dest: p.dest[i],
      city: p.city[i],
      status: p.status[i] ?? '',
      operational: p.operational[i] ?? '',
      activity: p.activity[i] ?? '',
      yearAnnounced: p.year_announced[i],
      yearCompleted: i32(p.year_completed[i]),
      value: Number.isFinite(v) && p.value_status[i] !== 'nq' ? v : null,
      expansion: b(p.expansion[i]),
      phase: p.phase[i],
      jv: b(p.jv[i]),
      jvLocal: b(p.jv_local[i]),
      jvLocalState: bn(p.jv_local_state[i]),
      jvCompany: p.jv_company[i],
      manufacturing: b(p.manufacturing[i]),
      active: b(p.active[i]),
      indexUse: b(p.index_use[i]),
      homeGroup: ((p.home_group[i] ?? 'rest') as FdiGroup),
    });
  }
  const s = chunk['fdi.sources'].cols;
  const byProject = new Map<number, FdiSourceCell[]>();
  for (let i = 0; i < chunk['fdi.sources'].nrow; i++) {
    const field = (s.field[i] ?? 'parent') as FdiSourceField;
    const text = s.text[i] ?? '';
    const urls = (s.url[i] ?? '').split('|').map((u) => safeHttpUrl(u.trim())).filter((u): u is string => !!u);
    const cell: FdiSourceCell = { field, label: FDI_SOURCE_LABELS[field] ?? field, text, note: sourceNote(text), urls, unverified: b(s.unverified[i]) };
    const list = byProject.get(s.project[i]);
    if (list) list.push(cell);
    else byProject.set(s.project[i], [cell]);
  }
  const order = (f: FdiSourceField) => FDI_SOURCE_FIELDS.indexOf(f);
  for (const list of byProject.values()) list.sort((a, z) => order(a.field) - order(z.field));
  const out: FdiData = { chunk, projects, sourcesOf: (row) => byProject.get(row) ?? [] };
  cache.set(chunk, out);
  return out;
}

// ---- filters -----------------------------------------------------------------------------------

/** Is a project in a basis (`all`, or the projects the indices use)? */
export const inBasis = (p: FdiProject, basis: FdiBasis): boolean => basis === 'all' || p.indexUse;

export interface FdiFilter {
  /** dims.items rows (the Midstream cells) */
  items?: readonly number[] | null;
  basis?: FdiBasis;
  statuses?: readonly string[] | null;
  activities?: readonly string[] | null;
  home?: number | null;
  dest?: number | null;
  /** home or destination (a country page: its inbound and outbound projects) */
  geo?: number | null;
  /** announced from this year on */
  from?: number | null;
}

/** The projects of a filter, in table order. */
export function filterProjects(projects: readonly FdiProject[], f: FdiFilter = {}): FdiProject[] {
  const items = f.items ? new Set(f.items) : null;
  const statuses = f.statuses ? new Set(f.statuses) : null;
  const activities = f.activities ? new Set(f.activities) : null;
  return projects.filter(
    (p) =>
      (!items || items.has(p.item)) &&
      inBasis(p, f.basis ?? 'all') &&
      (!statuses || statuses.has(p.status)) &&
      (!activities || activities.has(p.activity)) &&
      (f.home === null || f.home === undefined || p.home === f.home) &&
      (f.dest === null || f.dest === undefined || p.dest === f.dest) &&
      (f.geo === null || f.geo === undefined || p.home === f.geo || p.dest === f.geo) &&
      (f.from === null || f.from === undefined || p.yearAnnounced >= f.from),
  );
}

/** The cells (dims.items rows) that have projects, in item order. */
export function fdiItems(projects: readonly FdiProject[]): number[] {
  return [...new Set(projects.map((p) => p.item))].sort((a, z) => a - z);
}

/**
 * The dims.geo rows an economy page stands for: itself, or for the EU-27 node its member states
 * (CM-FDI records countries, never the EU).
 */
export function geoMembers(model: SectorModel, g: GeoInfo): Set<number> {
  if (g.kind === 'eu') return new Set(model.geos.filter((x) => x.eu_member && x.kind === 'country').map((x) => x.idx));
  return new Set([g.idx]);
}

/**
 * An economy's inbound projects (destination inside, parent's headquarters outside) and outbound
 * projects (headquarters inside, destination outside), in table order. For the EU node a project
 * between two member states is neither (it does not cross the bloc's border).
 */
export function countryProjects(projects: readonly FdiProject[], members: ReadonlySet<number>): { inbound: FdiProject[]; outbound: FdiProject[] } {
  const inbound: FdiProject[] = [];
  const outbound: FdiProject[] = [];
  for (const p of projects) {
    const hIn = members.has(p.home);
    const dIn = members.has(p.dest);
    if (dIn && !hIn) inbound.push(p);
    else if (hIn && !dIn) outbound.push(p);
  }
  return { inbound, outbound };
}

/**
 * The rank of an economy among rows of fdiByGeo by project count (null when absent), the number of
 * rows, and how many others share its count. Ties share a rank (competition ranking: two economies
 * with 21 projects behind two larger ones are both #3), so a tie is never broken by disclosed capital.
 */
export function geoRank(rows: readonly { geo: number; n: number }[], geo: number): { rank: number | null; n: number; tied: number } {
  const me = rows.find((r) => r.geo === geo);
  if (!me) return { rank: null, n: rows.length, tied: 0 };
  return { rank: 1 + rows.filter((r) => r.n > me.n).length, n: rows.length, tied: rows.filter((r) => r.n === me.n && r.geo !== geo).length };
}

/** "#3 of 34 destinations", "#12 of 32 destinations (tied with 3 others)". */
export function rankWords(r: { rank: number | null; n: number; tied: number }, what: string): string | null {
  if (r.rank === null) return null;
  return `#${r.rank} of ${r.n} ${what}${r.tied ? ` (tied with ${r.tied === 1 ? '1 other' : `${r.tied} others`})` : ''}`;
}

/** The announcement windows the views offer: every year, or from a year on. */
export const FDI_FROM_OPTIONS = [null, 2015, 2020] as const;
export type FdiFrom = (typeof FDI_FROM_OPTIONS)[number];

// ---- sums --------------------------------------------------------------------------------------

/** A count with the disclosed part of its capital: value = sum of disclosed values (US$ m), null when none is disclosed. */
export interface FdiTally {
  n: number;
  nValue: number;
  value: number | null;
}

function tally(): FdiTally {
  return { n: 0, nValue: 0, value: null };
}

function add(t: FdiTally, p: FdiProject) {
  t.n++;
  if (p.value !== null) {
    t.nValue++;
    t.value = (t.value ?? 0) + p.value;
  }
}

/** The measure of a tally: its count, or its disclosed sum (null = nothing disclosed). */
export const shown = (t: FdiTally, show: FdiShow): number | null => (show === 'n' ? t.n : t.value);

/** Totals of a set of projects. */
export function fdiTotals(projects: readonly FdiProject[]): FdiTally & { homes: number; dests: number; parents: number; yearMin: number | null; yearMax: number | null } {
  const t = tally();
  let lo = Infinity;
  let hi = -Infinity;
  for (const p of projects) {
    add(t, p);
    lo = Math.min(lo, p.yearAnnounced);
    hi = Math.max(hi, p.yearAnnounced);
  }
  return {
    ...t,
    homes: new Set(projects.map((p) => p.home)).size,
    dests: new Set(projects.map((p) => p.dest)).size,
    parents: new Set(projects.map((p) => p.parent)).size,
    yearMin: Number.isFinite(lo) ? lo : null,
    yearMax: Number.isFinite(hi) ? hi : null,
  };
}

export interface FdiFlow extends FdiTally {
  item: number;
  home: number;
  dest: number;
  homeGroup: FdiGroup;
  yearMin: number;
  yearMax: number;
}

/** Home -> destination flows (per cell), sorted like fdi.flows (item, home, dest). */
export function fdiFlows(projects: readonly FdiProject[]): FdiFlow[] {
  const m = new Map<string, FdiFlow>();
  for (const p of projects) {
    const k = `${p.item}|${p.home}|${p.dest}`;
    let f = m.get(k);
    if (!f) m.set(k, (f = { ...tally(), item: p.item, home: p.home, dest: p.dest, homeGroup: p.homeGroup, yearMin: p.yearAnnounced, yearMax: p.yearAnnounced }));
    add(f, p);
    f.yearMin = Math.min(f.yearMin, p.yearAnnounced);
    f.yearMax = Math.max(f.yearMax, p.yearAnnounced);
  }
  return [...m.values()].sort((a, z) => a.item - z.item || a.home - z.home || a.dest - z.dest);
}

/** Flows of several cells summed per (home, dest) (a country page over every cell). */
export function pairFlows(projects: readonly FdiProject[]): Omit<FdiFlow, 'item'>[] {
  const m = new Map<string, Omit<FdiFlow, 'item'>>();
  for (const p of projects) {
    const k = `${p.home}|${p.dest}`;
    let f = m.get(k);
    if (!f) m.set(k, (f = { ...tally(), home: p.home, dest: p.dest, homeGroup: p.homeGroup, yearMin: p.yearAnnounced, yearMax: p.yearAnnounced }));
    add(f, p);
    f.yearMin = Math.min(f.yearMin, p.yearAnnounced);
    f.yearMax = Math.max(f.yearMax, p.yearAnnounced);
  }
  return [...m.values()];
}

export interface FdiGeoRow extends FdiTally {
  geo: number;
  /** projects by the parent's headquarters group */
  groups: Record<FdiGroup, number>;
  /** the investor group of a home economy (side 'home'), else null */
  homeGroup: FdiGroup | null;
}

const zeroGroups = (): Record<FdiGroup, number> => ({ china: 0, core: 0, partner: 0, rest: 0 });

/**
 * Projects by destination (`dest`) or by investor economy (`home`), largest first by the measure
 * shown (ties: the other measure, then dims.geo order). In value mode an economy whose projects
 * disclose nothing sorts last (its value is "not disclosed", never 0).
 */
export function fdiByGeo(projects: readonly FdiProject[], side: 'dest' | 'home', show: FdiShow = 'n'): FdiGeoRow[] {
  const m = new Map<number, FdiGeoRow>();
  for (const p of projects) {
    const g = side === 'dest' ? p.dest : p.home;
    let r = m.get(g);
    if (!r) m.set(g, (r = { ...tally(), geo: g, groups: zeroGroups(), homeGroup: side === 'home' ? p.homeGroup : null }));
    add(r, p);
    r.groups[p.homeGroup]++;
  }
  const key = (r: FdiGeoRow) => (show === 'n' ? r.n : (r.value ?? -1));
  const other = (r: FdiGeoRow) => (show === 'n' ? (r.value ?? -1) : r.n);
  return [...m.values()].sort((a, z) => key(z) - key(a) || other(z) - other(a) || a.geo - z.geo);
}

export interface FdiGroupPart extends FdiTally {
  /** share of the projects (0-1) */
  shareN: number;
  /** share of the disclosed capital (0-1); NaN when nothing is disclosed */
  shareValue: number;
}

export interface FdiGroupSplit extends FdiTally {
  parts: Record<FdiGroup, FdiGroupPart>;
}

/**
 * The split of a set of projects by the parent's headquarters group (raw counts: a joint venture
 * counts wholly under its listed parent). As fdi.groups: a group without projects has value 0; a
 * group with projects but no disclosed value has value null (NaN in the chunk).
 */
export function fdiGroupSplit(projects: readonly FdiProject[]): FdiGroupSplit {
  const all = tally();
  const parts = Object.fromEntries(FDI_GROUP_ORDER.map((g) => [g, { ...tally(), shareN: 0, shareValue: NaN }])) as Record<FdiGroup, FdiGroupPart>;
  for (const p of projects) {
    add(all, p);
    add(parts[p.homeGroup], p);
  }
  for (const g of FDI_GROUP_ORDER) {
    const x = parts[g];
    if (x.n === 0) x.value = 0;
    x.shareN = all.n > 0 ? x.n / all.n : NaN;
    x.shareValue = all.value !== null && all.value > 0 && x.value !== null ? x.value / all.value : NaN;
  }
  return { ...all, parts };
}

/**
 * The coalition (core + partners) share of a split, by count or by disclosed capital. A coalition group
 * with projects but nothing disclosed adds 0 to the disclosed capital (it never turns the sum into NaN);
 * NaN only when the split itself discloses nothing.
 */
export function coalitionShare(s: FdiGroupSplit, show: FdiShow): number {
  if (show === 'n') return s.n > 0 ? (s.parts.core.n + s.parts.partner.n) / s.n : NaN;
  return s.value !== null && s.value > 0 ? ((s.parts.core.value ?? 0) + (s.parts.partner.value ?? 0)) / s.value : NaN;
}

/** The share parts of a split for ShareSplitChart / BlocBar (by count or by disclosed capital). */
export function splitShares(s: FdiGroupSplit, show: FdiShow): Partial<Record<FdiGroup, number>> {
  const out: Partial<Record<FdiGroup, number>> = {};
  for (const g of FDI_GROUP_ORDER) {
    const x = show === 'n' ? s.parts[g].shareN : s.parts[g].shareValue;
    if (Number.isFinite(x)) out[g] = x;
  }
  return out;
}

export interface FdiYearRow extends FdiTally {
  year: number;
  group: FdiGroup;
}

/** Announcements by (year, parent group), only combinations with a project (as fdi.years), sorted by year then group. */
export function fdiYears(projects: readonly FdiProject[]): FdiYearRow[] {
  const m = new Map<string, FdiYearRow>();
  for (const p of projects) {
    const k = `${p.yearAnnounced}|${p.homeGroup}`;
    let r = m.get(k);
    if (!r) m.set(k, (r = { ...tally(), year: p.yearAnnounced, group: p.homeGroup }));
    add(r, p);
  }
  return [...m.values()].sort((a, z) => a.year - z.year || FDI_GROUP_ORDER.indexOf(a.group) - FDI_GROUP_ORDER.indexOf(z.group));
}

export interface FdiYearBar {
  year: number;
  total: FdiTally;
  groups: Record<FdiGroup, FdiTally>;
}

/** Every year of a window with its projects by group (a year without an announcement is 0 projects). */
export function fdiYearBars(rows: readonly FdiYearRow[], from: number, to: number): FdiYearBar[] {
  const out: FdiYearBar[] = [];
  for (let y = from; y <= to; y++) {
    const groups = Object.fromEntries(FDI_GROUP_ORDER.map((g) => [g, tally()])) as Record<FdiGroup, FdiTally>;
    const total = tally();
    for (const r of rows) {
      if (r.year !== y) continue;
      groups[r.group] = { n: r.n, nValue: r.nValue, value: r.value };
      total.n += r.n;
      total.nValue += r.nValue;
      if (r.value !== null) total.value = (total.value ?? 0) + r.value;
    }
    out.push({ year: y, total, groups });
  }
  return out;
}

// ---- Sankey ------------------------------------------------------------------------------------

/** Folded node codes of the Sankey: investors and destinations outside the top ones. */
export const OTHER_HOMES = -1;
export const OTHER_DESTS = -2;

export interface FdiLink {
  home: number;
  dest: number;
  /** projects, or disclosed US$ m */
  v: number;
  share: number;
}

/**
 * Sankey links of the flows: the top `topH` investor economies to the top `topD` destinations by the
 * measure shown; the rest fold into "Other investors" / "Other destinations". In value mode a flow
 * without a disclosed value is left out (its projects are counted in the table, not drawn).
 */
export function fdiSankey(flows: readonly Omit<FdiFlow, 'item'>[], show: FdiShow, opts: { topH?: number; topD?: number } = {}): { links: FdiLink[]; total: number } {
  const val = (f: Omit<FdiFlow, 'item'>) => (show === 'n' ? f.n : (f.value ?? 0));
  const byHome = new Map<number, number>();
  const byDest = new Map<number, number>();
  let total = 0;
  for (const f of flows) {
    const x = val(f);
    if (!(x > 0)) continue;
    byHome.set(f.home, (byHome.get(f.home) ?? 0) + x);
    byDest.set(f.dest, (byDest.get(f.dest) ?? 0) + x);
    total += x;
  }
  const top = (m: Map<number, number>, n: number) => new Set([...m.entries()].sort((a, z) => z[1] - a[1] || a[0] - z[0]).slice(0, n).map(([g]) => g));
  const H = top(byHome, opts.topH ?? 8);
  const D = top(byDest, opts.topD ?? 10);
  const m = new Map<string, FdiLink>();
  for (const f of flows) {
    const x = val(f);
    if (!(x > 0)) continue;
    const home = H.has(f.home) ? f.home : OTHER_HOMES;
    const dest = D.has(f.dest) ? f.dest : OTHER_DESTS;
    const k = `${home}|${dest}`;
    const l = m.get(k);
    if (l) l.v += x;
    else m.set(k, { home, dest, v: x, share: 0 });
  }
  const links = [...m.values()].map((l) => ({ ...l, share: total > 0 ? l.v / total : 0 })).sort((a, z) => z.v - a.v);
  return { links, total };
}

// ---- words -------------------------------------------------------------------------------------

/** "all 314 projects (every status and activity)" / "the 271 projects the Partnership indices use". */
export function basisWords(basis: FdiBasis, n: number): string {
  return basis === 'all' ? `all ${n.toLocaleString('en-US')} projects (every status and activity)` : `the ${n.toLocaleString('en-US')} projects the Partnership indices use`;
}

/** The joint-venture words of a project ("no"; "Ultium Cells · with a local partner (state-owned)"). */
export function jvWords(p: FdiProject): string {
  if (!p.jv) return 'no';
  const local = p.jvLocal ? ` · with a local partner${p.jvLocalState ? ' (state-owned)' : ''}` : '';
  return `${p.jvCompany ?? 'joint venture'}${local}`;
}

/** "12 projects", "1 project". */
export const projectsWord = (n: number): string => `${n.toLocaleString('en-US')} project${n === 1 ? '' : 's'}`;

/** A disclosed sum in US$ m as text ("USD 1.8 bn", "USD 240 m"), or "not disclosed". */
export function fmtUsdM(x: number | null | undefined, ctx: 'tile' | 'table' = 'tile'): string {
  if (x === null || x === undefined || !Number.isFinite(x)) return NOT_DISCLOSED;
  const usd = x * 1e6;
  if (ctx === 'table') return x.toLocaleString('en-US', { maximumFractionDigits: 1 });
  if (usd >= 1e9) return `USD ${(usd / 1e9).toLocaleString('en-US', { maximumSignificantDigits: 3 })} bn`;
  return `USD ${x.toLocaleString('en-US', { maximumSignificantDigits: 3 })} m`;
}

/** "USD 1.8 bn (12 of 14 projects disclose a value)". */
export function disclosedWords(t: FdiTally): string {
  if (t.n === 0) return 'no projects';
  if (t.nValue === 0) return `${NOT_DISCLOSED} (none of ${projectsWord(t.n)} discloses a value)`;
  return `${fmtUsdM(t.value)} (${t.nValue === t.n ? `all ${projectsWord(t.n)} disclose a value` : `${t.nValue} of ${projectsWord(t.n)} disclose a value`})`;
}

/** The measure of a tally as text: "12 projects" or "USD 1.8 bn" / "not disclosed". */
export function shownText(t: FdiTally, show: FdiShow): string {
  return show === 'n' ? projectsWord(t.n) : fmtUsdM(t.value);
}

/**
 * The edition date of the source ("2026.10.05" -> 5 October 2026), when the last announcement year
 * is the edition's year: that year is partial.
 */
export function partialYearNote(src: SourceInfo | null | undefined, lastYear: number | null): string | null {
  const m = /^(\d{4})\.(\d{2})\.(\d{2})$/.exec(src?.edition ?? '');
  if (!m || lastYear === null || Number(m[1]) !== lastYear) return null;
  const d = new Date(Date.UTC(Number(m[1]), Number(m[2]) - 1, Number(m[3])));
  const words = d.toLocaleDateString('en-GB', { day: 'numeric', month: 'long', year: 'numeric', timeZone: 'UTC' });
  return `${lastYear} is a partial year: announcements up to the edition of ${words}.`;
}

// ---- CSV ---------------------------------------------------------------------------------------

/** The source, licence and attribution columns every CM-FDI CSV ends with. */
function srcCols(src: SourceInfo | null | undefined): Record<string, string | null> {
  return {
    source: src ? `${src.short}${src.edition ? ` ${src.edition}` : ''}` : 'CM-FDI',
    licence: src?.licence === 'attribution' ? 'CC BY 4.0 (attribution required)' : (src?.licence ?? null),
    attribution: src?.attribution ?? null,
  };
}

/** The CM-FDI source of the dictionary. */
export const cmfdiSource = (model: SectorModel): SourceInfo | null => model.varById.get('investment.fdi_projects')?.source ?? model.sources.get('cmfdi') ?? null;

/** One row per project: every field, the value or empty with value_status "not disclosed", the source links by field. */
export function projectsCsv(model: SectorModel, data: FdiData, projects: readonly FdiProject[], direction?: (p: FdiProject) => string): string {
  const src = cmfdiSource(model);
  const geo = (g: number) => model.geos[g];
  return rowsToCsv(
    projects.map((p) => ({
      ...(direction ? { direction: direction(p) } : {}),
      project_id: p.pid,
      item: model.items[p.item]?.slug ?? null,
      technology: p.technology,
      parent: p.parent,
      hq_iso3c: geo(p.home)?.code ?? null,
      hq_economy: geo(p.home)?.name ?? null,
      parent_hq_group: p.homeGroup,
      destination_iso3c: geo(p.dest)?.code ?? null,
      destination: geo(p.dest)?.name ?? null,
      city: p.city,
      status: p.status,
      operational: p.operational,
      activity: p.activity,
      year_announced: p.yearAnnounced,
      year_completed: p.yearCompleted,
      capital_investment_usd_m: p.value,
      value_status: p.value === null ? NOT_DISCLOSED : 'disclosed',
      expansion: p.expansion,
      phase: p.phase,
      joint_venture: p.jv,
      jv_company: p.jvCompany,
      jv_local_partner: p.jvLocal,
      jv_local_state_partner: p.jvLocalState,
      used_by_the_indices: p.indexUse,
      source_links: data.sourcesOf(p.row).filter((c) => c.urls.length).map((c) => `${c.field}: ${c.urls.join(' ')}`).join(' | ') || null,
      source_notes: data.sourcesOf(p.row).filter((c) => c.note || c.unverified).map((c) => `${c.field}: ${[c.unverified ? '[UNVERIFIED]' : null, c.note || null].filter(Boolean).join(' ')}`).join(' | ') || null,
      ...srcCols(src),
    })),
  );
}

/** Home -> destination flows of one cell and basis (projects, disclosed projects, disclosed sum, years). */
export function flowsCsv(model: SectorModel, flows: readonly Omit<FdiFlow, 'item'>[], item: string, basis: FdiBasis): string {
  const src = cmfdiSource(model);
  return rowsToCsv(
    [...flows]
      .sort((a, z) => z.n - a.n || (z.value ?? -1) - (a.value ?? -1))
      .map((f) => ({
        hq_iso3c: model.geos[f.home]?.code ?? null,
        hq_economy: model.geos[f.home]?.name ?? null,
        parent_hq_group: f.homeGroup,
        destination_iso3c: model.geos[f.dest]?.code ?? null,
        destination: model.geos[f.dest]?.name ?? null,
        item,
        basis,
        projects: f.n,
        projects_disclosing_value: f.nValue,
        capital_investment_usd_m: f.value,
        value_status: f.value === null ? NOT_DISCLOSED : 'disclosed (sum of the projects that disclose a value)',
        year_announced_first: f.yearMin,
        year_announced_last: f.yearMax,
        ...srcCols(src),
      })),
  );
}

/** Projects by economy (destination or investor). */
export function byGeoCsv(model: SectorModel, rows: readonly FdiGeoRow[], side: 'dest' | 'home', item: string, basis: FdiBasis): string {
  const src = cmfdiSource(model);
  return rowsToCsv(
    rows.map((r, i) => ({
      iso3c: model.geos[r.geo]?.code ?? null,
      economy: model.geos[r.geo]?.name ?? null,
      role: side === 'dest' ? 'destination' : 'parent headquarters',
      group: side === 'home' ? r.homeGroup : (model.geos[r.geo]?.group ?? null),
      item,
      basis,
      rank: i + 1,
      projects: r.n,
      projects_disclosing_value: r.nValue,
      capital_investment_usd_m: r.value,
      value_status: r.value === null ? NOT_DISCLOSED : 'disclosed (sum)',
      projects_parent_china: r.groups.china,
      projects_parent_core: r.groups.core,
      projects_parent_partner: r.groups.partner,
      projects_parent_rest: r.groups.rest,
      ...srcCols(src),
    })),
  );
}

/** Group splits (one row per destination, WLD = every destination). */
export function groupsCsv(model: SectorModel, rows: readonly { dest: number | null; split: FdiGroupSplit }[], item: string, basis: FdiBasis): string {
  const src = cmfdiSource(model);
  return rowsToCsv(
    rows.map((r) => ({
      destination_iso3c: r.dest === null ? 'WLD' : (model.geos[r.dest]?.code ?? null),
      destination: r.dest === null ? 'All destinations' : (model.geos[r.dest]?.name ?? null),
      item,
      basis,
      projects: r.split.n,
      projects_disclosing_value: r.split.nValue,
      capital_investment_usd_m: r.split.value,
      value_status: r.split.value === null ? NOT_DISCLOSED : 'disclosed (sum)',
      ...Object.fromEntries(FDI_GROUP_ORDER.flatMap((g) => [
        [`projects_parent_${g}`, r.split.parts[g].n],
        [`share_projects_parent_${g}`, r.split.parts[g].shareN],
        [`share_disclosed_value_parent_${g}`, Number.isFinite(r.split.parts[g].shareValue) ? r.split.parts[g].shareValue : null],
      ])),
      ...srcCols(src),
    })),
  );
}

/**
 * Announcements by year and parent group. A (year, group) without a project has capital 0 (as
 * fdi.groups); one whose projects disclose nothing is empty with value_status "not disclosed".
 */
export function yearsCsv(model: SectorModel, bars: readonly FdiYearBar[], item: string, basis: FdiBasis): string {
  const src = cmfdiSource(model);
  return rowsToCsv(
    bars.flatMap((b) =>
      FDI_GROUP_ORDER.map((g) => ({
        year_announced: b.year,
        parent_hq_group: g,
        item,
        basis,
        projects: b.groups[g].n,
        projects_disclosing_value: b.groups[g].nValue,
        capital_investment_usd_m: b.groups[g].n === 0 ? 0 : b.groups[g].value,
        value_status: b.groups[g].n === 0 ? 'no project' : b.groups[g].value === null ? NOT_DISCLOSED : 'disclosed (sum)',
        ...srcCols(src),
      })),
    ),
  );
}
