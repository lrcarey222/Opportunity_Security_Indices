// The sector-meta chunk as lookups (spec_sector.md 8.6). Pure: a validated SectorMetaChunk in, plain
// objects and maps out, built once per chunk object (WeakMap cache). Views and derivations read the
// dictionary only through this model, so captions, units, years and sources always come from
// dict.* and never from the processed tables.

import {
  FAMILY_IDS, STAGE_SLUGS, stageSlugOf, TECH_SLUGS, techSlugOf,
  type FamilyId, type GeoGroup, type GeoKind, type ItemAxis, type NativeLevel, type SectorMetaChunk, type StageSlug, type TechSlug, type Tier, type WorldBasis,
} from '../contract.ts';
import type { UnitSpec } from '../format.ts';
import { NA_INT } from '../../engine/numeric.ts';

export interface GeoInfo {
  idx: number;
  code: string;
  name: string;
  kind: GeoKind;
  iso_n3: string | null;
  tier: Tier;
  group: GeoGroup;
  eu_member: boolean;
  re_export_hub: boolean;
  bloc: string | null;
  wb_region: string | null;
  income: string | null;
  reason: string | null;
  /** UN-vote position basis: "own votes", "inherited" (a territory takes its sovereign's position), "neutral" */
  alignment_basis?: string | null;
}

export interface ItemInfo {
  idx: number;
  axis: ItemAxis;
  slug: string;
  label: string;
  /** Strategic labels (cell items only) */
  tech: string | null;
  stage: string | null;
  techSlug: TechSlug | null;
  stageSlug: StageSlug | null;
  headline: boolean;
  sort: number;
  has_trade: boolean;
}

export interface UnitInfo extends UnitSpec {
  id: string;
  long: string;
}

export interface SourceInfo {
  id: string;
  short: string;
  citation: string;
  publisher: string | null;
  edition: string | null;
  url: string | null;
  licence: string;
  attribution: string | null;
  accessed: string | null;
  transform: string | null;
  issues: string | null;
}

export interface VarInfo {
  idx: number;
  id: string;
  theme: string;
  family: FamilyId;
  label: string;
  short: string;
  definition: string | null;
  unit: UnitInfo;
  data_year: string;
  years: number[];
  year_field: boolean;
  native_level: NativeLevel;
  item_axis: ItemAxis;
  agg: 'sum' | 'ratio' | 'none';
  world_basis: WorldBasis;
  norms: string[];
  scale: 'linear' | 'log' | null;
  higher_is: string | null;
  sign_note: string | null;
  display_scale: number;
  source: SourceInfo;
  roster: string | null;
  licence: string;
  caveats: string[];
  primary_for: string[];
  section: string | null;
  stale_years: number | null;
  min_base: number | null;
  feeds: string[];
  prov: string;
  prov_detail: string | null;
  phase: string;
  projection_from: number | null;
  series_window: string | null;
}

export interface CoverageInfo {
  item: number;
  family: FamilyId;
  status: 'country' | 'region' | 'global' | 'licensed' | 'none';
  n_geo: number;
  reason: string | null;
  sources: string[];
}

export interface AvailInfo {
  var: number;
  item: number;
  chunk: string;
  year_min: number | null;
  year_max: number | null;
  n_years: number;
  n_geo: number;
}

/** One meta.summary row (3.6). Shares are 0-1; NaN where undefined. */
export interface SummaryInfo {
  row: number;
  item: number;
  var: number;
  year: number;
  eu_mode: 'members' | 'bloc';
  W: number;
  basis: WorldBasis;
  n: number;
  top1_geo: number | null;
  top1_share: number;
  top3_share: number;
  hhi: number;
  hhi_lower_bound: boolean;
  eff_n: number;
  china: number;
  core: number;
  partner: number;
  rest: number;
  unattributed: number;
  growth: number;
  growth_kind: string | null;
}

export interface ReconcileInfo {
  item: number;
  year: number;
  matrix_total: number;
  matrix_china: number;
  matrix_hhi: number;
  reported_total: number;
  reported_china: number;
  reported_hhi: number;
  n_reporters: number | null;
  warn: boolean;
}

export interface ThemeInfo {
  slug: string;
  label: string;
  description: string | null;
  order: number;
  default_var: string;
}

export interface MineralDemandRow {
  mineral: number;
  tech: string | null;
  label: string;
  scenario: string;
  year: number;
  kt: number;
}

const i32 = (x: number): number | null => (x === NA_INT ? null : x);
const b = (x: number): boolean => x === 1;
export const splitPipe = (s: string | null | undefined): string[] => (s ? s.split('|').map((v) => v.trim()).filter((v) => v !== '') : []);

const FALLBACK_UNIT: UnitInfo = { id: '', short: '', long: '', unit_kind: 'number' };
const FALLBACK_SOURCE: SourceInfo = { id: '', short: 'source', citation: '', publisher: null, edition: null, url: null, licence: 'open', attribution: null, accessed: null, transform: null, issues: null };

/** A competition-authority concentration threshold (dict.hhi_refs): context, never a verdict. */
export interface HhiRef {
  id: string;
  /** on the 0-1 scale of the summaries */
  value: number;
  /** the same on the authorities' 0-10,000 scale */
  points: number;
  /** the label describes markets above (or below) the value */
  side: 'above' | 'below';
  label: string;
  authority: string;
  document: string;
  year: number;
}

/**
 * Short names that would print a bare acronym in a title, axis, header or caption (acronym policy,
 * spec_country.md C3.1): the dictionary's text is kept everywhere else (labels, definitions), and the
 * display short is spelled out here, once, for every view.
 */
export const PLAIN_SHORT: Readonly<Record<string, string>> = {
  RCA: 'Comparative advantage',
  TRL: 'Readiness',
  'TRL change': 'Change in readiness',
  'ΔTRL': 'Change in readiness',
  'CAT rating': 'Climate Action Tracker rating',
  'EGS potential': 'Enhanced geothermal potential',
  'EGS LCOE (500 MW)': 'Enhanced geothermal cost (500 MW)',
  'EGS LCOE (5 GW)': 'Enhanced geothermal cost (5 GW)',
  'PPI y/y': 'Producer prices, yearly change',
  'FDI projects': 'Cross-border projects',
  'FDI capital': 'Cross-border capital',
  'Parent HQ group': 'Parent headquarters group',
};

/** Unit shorts that are acronyms ("1.35 RCA", "(TRL)" in a header): the long name says the rest. */
export const PLAIN_UNIT_SHORT: Readonly<Record<string, string>> = {
  RCA: 'ratio',
  TRL: 'level',
  'TRL points': 'levels',
};

/** Labels spelled out the same way (the label shows in the Explore measure list and in result headings). */
export const PLAIN_LABEL: Readonly<Record<string, string>> = {
  'Enhanced geothermal LCOE, cheapest 500 MW': 'Enhanced geothermal: levelised cost of electricity, cheapest 500 MW',
  'Enhanced geothermal LCOE, cheapest 5 GW': 'Enhanced geothermal: levelised cost of electricity, cheapest 5 GW',
  'World energy supply (WEO, Stated Policies)': 'World energy supply (IEA World Energy Outlook, stated policies)',
};

/** Source short names spelled out the same way (the full name first, the acronym in parentheses). */
export const PLAIN_SOURCE_SHORT: Readonly<Record<string, string>> = {
  'CM-FDI (Net Zero Industrial Policy Lab)': 'Cleantech Manufacturing FDI (CM-FDI), Net Zero Industrial Policy Lab',
  'Franzmann et al. 2025 (EGS)': 'Franzmann et al. 2025 (enhanced geothermal)',
};

export class SectorModel {
  readonly meta: SectorMetaChunk;
  readonly geos: GeoInfo[];
  readonly geoByCode: Map<string, GeoInfo>;
  /** kind country, sorted by name */
  readonly countries: GeoInfo[];
  readonly world: GeoInfo | null;
  readonly eu: GeoInfo | null;
  readonly items: ItemInfo[];
  readonly itemByKey: Map<string, ItemInfo>;
  /** axis cell, in dims order (sort) */
  readonly cells: ItemInfo[];
  readonly none: ItemInfo | null;
  readonly units: Map<string, UnitInfo>;
  readonly sources: Map<string, SourceInfo>;
  readonly caveats: Map<string, string>;
  readonly vars: VarInfo[];
  readonly varById: Map<string, VarInfo>;
  readonly themes: ThemeInfo[];
  readonly families: Map<FamilyId, { label: string; section: string | null }>;
  readonly cellText: Map<string, { cell_blurb: string | null; section_cards: string[] }>;
  readonly techText: Map<string, { label: string; tech_blurb: string | null }>;
  /** dict.cell_caveats: cell slug -> the dict.caveats ids of its basket notes (optional table; [] when absent) */
  readonly cellCaveatIds: Map<string, string[]>;
  /** dict.hhi_refs: competition-authority HHI thresholds (context for the HHI explainer; [] when absent) */
  readonly hhiRefs: HhiRef[];
  readonly itemUnits: Map<string, UnitInfo>;
  readonly rosters: Map<string, Set<number>>;
  readonly coverage: Map<string, CoverageInfo>;
  readonly avail: AvailInfo[];
  readonly availByVar: Map<number, AvailInfo[]>;
  readonly summary: SummaryInfo[];
  readonly summaryByKey: Map<string, SummaryInfo>;
  readonly reconcile: ReconcileInfo[];
  /** item -> linked cells (minerals, fuels, commodities) and cell -> linked items, with weights */
  readonly linksByItem: Map<number, { cell: number; weight: number }[]>;
  readonly linksByCell: Map<number, { item: number; weight: number }[]>;
  readonly regionMembers: Map<number, number[]>;
  readonly mineralDemand: MineralDemandRow[];
  readonly euOutsideCoalition: string[];

  constructor(meta: SectorMetaChunk) {
    this.meta = meta;
    const g = meta['dims.geo'].cols;
    this.geos = [];
    for (let i = 0; i < meta['dims.geo'].nrow; i++) {
      this.geos.push({
        idx: i,
        code: g.code[i] ?? '',
        name: g.name[i] ?? '',
        kind: (g.kind[i] ?? 'country') as GeoKind,
        iso_n3: g.iso_n3[i] === NA_INT ? null : String(g.iso_n3[i]).padStart(3, '0'),
        tier: (g.tier[i] ?? 'none') as Tier,
        group: (g.group[i] ?? 'rest') as GeoGroup,
        eu_member: b(g.eu_member[i]),
        re_export_hub: b(g.re_export_hub[i]),
        bloc: g.bloc[i],
        wb_region: g.wb_region[i],
        income: g.income[i],
        reason: g.reason[i],
        alignment_basis: g.alignment_basis?.[i] ?? null,
      });
    }
    this.geoByCode = new Map(this.geos.map((x) => [x.code, x]));
    this.countries = this.geos.filter((x) => x.kind === 'country').sort((a, z) => a.name.localeCompare(z.name, 'en'));
    this.world = this.geoByCode.get('WLD') ?? null;
    this.eu = this.geoByCode.get('EUU') ?? null;

    const it = meta['dims.items'].cols;
    this.items = [];
    for (let i = 0; i < meta['dims.items'].nrow; i++) {
      const tech = it.tech[i];
      const stage = it.stage[i];
      this.items.push({
        idx: i,
        axis: (it.axis[i] ?? 'none') as ItemAxis,
        slug: it.slug[i] ?? '',
        label: it.label[i] ?? '',
        tech,
        stage,
        techSlug: tech ? techSlugOf(tech) : null,
        stageSlug: stage ? stageSlugOf(stage) : null,
        headline: b(it.headline[i]),
        sort: it.sort[i],
        has_trade: b(it.has_trade[i]),
      });
    }
    this.itemByKey = new Map(this.items.map((x) => [`${x.axis}:${x.slug}`, x]));
    this.cells = this.items.filter((x) => x.axis === 'cell').sort((a, z) => cellOrder(a) - cellOrder(z) || a.sort - z.sort);
    this.none = this.items.find((x) => x.axis === 'none') ?? null;

    const u = meta['dict.units'].cols;
    this.units = new Map();
    for (let i = 0; i < meta['dict.units'].nrow; i++) {
      const id = u.id[i] ?? '';
      this.units.set(id, { id, short: PLAIN_UNIT_SHORT[u.short[i] ?? ''] ?? u.short[i] ?? '', long: u.long[i] ?? u.short[i] ?? '', unit_kind: u.unit_kind[i] ?? 'number' });
    }
    const s = meta['dict.sources'].cols;
    this.sources = new Map();
    for (let i = 0; i < meta['dict.sources'].nrow; i++) {
      const id = s.id[i] ?? '';
      this.sources.set(id, {
        id,
        short: PLAIN_SOURCE_SHORT[s.short[i] ?? ''] ?? s.short[i] ?? id,
        citation: s.citation[i] ?? '',
        publisher: s.publisher[i],
        edition: s.edition[i],
        url: s.url[i],
        licence: s.licence[i] ?? 'open',
        attribution: s.attribution[i],
        accessed: s.accessed[i],
        transform: s.transform[i],
        issues: s.issues[i],
      });
    }
    const c = meta['dict.caveats'].cols;
    this.caveats = new Map();
    for (let i = 0; i < meta['dict.caveats'].nrow; i++) this.caveats.set(c.id[i] ?? '', c.text[i] ?? '');

    const v = meta['dict.vars'].cols;
    this.vars = [];
    for (let i = 0; i < meta['dict.vars'].nrow; i++) {
      const years = splitPipe(v.years[i]).map(Number).filter((x) => Number.isFinite(x));
      const ds = v.display_scale[i];
      this.vars.push({
        idx: i,
        id: v.id[i] ?? '',
        theme: v.theme[i] ?? '',
        family: (v.family[i] ?? 'context') as FamilyId,
        label: PLAIN_LABEL[v.label[i] ?? ''] ?? v.label[i] ?? '',
        short: PLAIN_SHORT[v.short[i] ?? ''] ?? v.short[i] ?? v.label[i] ?? '',
        definition: v.definition[i],
        unit: this.units.get(v.unit[i] ?? '') ?? { ...FALLBACK_UNIT, id: v.unit[i] ?? '' },
        data_year: v.data_year[i] ?? '',
        years,
        year_field: b(v.year_field[i]),
        native_level: (v.native_level[i] ?? 'country') as NativeLevel,
        item_axis: (v.item_axis[i] ?? 'none') as ItemAxis,
        agg: (v.agg[i] ?? 'none') as VarInfo['agg'],
        world_basis: (v.world_basis[i] ?? 'none') as WorldBasis,
        norms: splitPipe(v.norms[i]),
        scale: (v.scale[i] as VarInfo['scale']) ?? null,
        higher_is: v.higher_is[i],
        sign_note: v.sign_note[i],
        display_scale: Number.isFinite(ds) ? ds : 1,
        source: this.sources.get(v.source[i] ?? '') ?? { ...FALLBACK_SOURCE, id: v.source[i] ?? '' },
        roster: v.roster[i],
        licence: v.licence[i] ?? 'open',
        caveats: splitPipe(v.caveats[i]),
        primary_for: splitPipe(v.primary_for[i]),
        section: v.section[i],
        stale_years: i32(v.stale_years[i]),
        min_base: Number.isFinite(v.min_base[i]) ? v.min_base[i] : null,
        feeds: splitPipe(v.feeds[i]),
        prov: v.prov[i] ?? 'X',
        prov_detail: v.prov_detail[i],
        phase: v.phase[i] ?? '1',
        projection_from: i32(v.projection_from[i]),
        series_window: v.series_window[i],
      });
    }
    this.varById = new Map(this.vars.map((x) => [x.id, x]));

    const t = meta['dict.themes'].cols;
    this.themes = [];
    for (let i = 0; i < meta['dict.themes'].nrow; i++) {
      this.themes.push({ slug: t.slug[i] ?? '', label: t.label[i] ?? '', description: t.description[i], order: t.order[i], default_var: t.default_var[i] ?? '' });
    }
    this.themes.sort((a, z) => a.order - z.order);

    const f = meta['dict.families'].cols;
    this.families = new Map();
    for (let i = 0; i < meta['dict.families'].nrow; i++) this.families.set((f.id[i] ?? 'context') as FamilyId, { label: f.label[i] ?? '', section: f.section[i] });
    for (const id of FAMILY_IDS) if (!this.families.has(id)) this.families.set(id, { label: id, section: null });

    const dc = meta['dict.cells'].cols;
    this.cellText = new Map();
    for (let i = 0; i < meta['dict.cells'].nrow; i++) this.cellText.set(dc.slug[i] ?? '', { cell_blurb: dc.cell_blurb[i], section_cards: splitPipe(dc.section_cards[i]) });
    this.cellCaveatIds = new Map();
    const cc = meta['dict.cell_caveats'];
    if (cc) {
      for (let i = 0; i < cc.nrow; i++) {
        const slug = cc.cols.slug[i] ?? '';
        const l = this.cellCaveatIds.get(slug) ?? [];
        l.push(cc.cols.caveat[i] ?? '');
        this.cellCaveatIds.set(slug, l);
      }
    }
    const hr = meta['dict.hhi_refs'];
    this.hhiRefs = [];
    if (hr) {
      for (let i = 0; i < hr.nrow; i++) this.hhiRefs.push({ id: hr.cols.id[i] ?? '', value: hr.cols.value[i], points: hr.cols.points[i], side: (hr.cols.side[i] ?? 'above') as 'above' | 'below', label: hr.cols.label[i] ?? '', authority: hr.cols.authority[i] ?? '', document: hr.cols.document[i] ?? '', year: hr.cols.year[i] });
      this.hhiRefs.sort((a, z) => a.value - z.value);
    }
    const dt = meta['dict.techs'].cols;
    this.techText = new Map();
    for (let i = 0; i < meta['dict.techs'].nrow; i++) this.techText.set(dt.slug[i] ?? '', { label: dt.label[i] ?? '', tech_blurb: dt.tech_blurb[i] });

    const iu = meta['dict.item_units'].cols;
    this.itemUnits = new Map();
    for (let i = 0; i < meta['dict.item_units'].nrow; i++) {
      const unit = this.units.get(iu.unit[i] ?? '');
      if (unit) this.itemUnits.set(`${iu.var[i]}|${iu.item[i]}`, unit);
    }

    const r = meta.rosters.cols;
    this.rosters = new Map();
    for (let i = 0; i < meta.rosters.nrow; i++) {
      const k = r.roster[i] ?? '';
      let set = this.rosters.get(k);
      if (!set) this.rosters.set(k, (set = new Set()));
      set.add(r.geo[i]);
    }

    const cv = meta.coverage.cols;
    this.coverage = new Map();
    for (let i = 0; i < meta.coverage.nrow; i++) {
      const row: CoverageInfo = {
        item: cv.item[i],
        family: (cv.family[i] ?? 'context') as FamilyId,
        status: (cv.status[i] ?? 'none') as CoverageInfo['status'],
        n_geo: cv.n_geo[i],
        reason: cv.reason[i],
        // the coverage rows name sources by their short text: spelled out like the dictionary's (acronym policy)
        sources: splitPipe(cv.sources[i]).map((x) => PLAIN_SOURCE_SHORT[x] ?? x),
      };
      this.coverage.set(`${row.item}|${row.family}`, row);
    }

    const av = meta.avail.cols;
    this.avail = [];
    this.availByVar = new Map();
    for (let i = 0; i < meta.avail.nrow; i++) {
      const row: AvailInfo = {
        var: av.var[i],
        item: av.item[i],
        chunk: av.chunk[i] ?? 'facts',
        year_min: i32(av.year_min[i]),
        year_max: i32(av.year_max[i]),
        n_years: av.n_years[i],
        n_geo: av.n_geo[i],
      };
      this.avail.push(row);
      let list = this.availByVar.get(row.var);
      if (!list) this.availByVar.set(row.var, (list = []));
      list.push(row);
    }

    const sm = meta.summary.cols;
    this.summary = [];
    this.summaryByKey = new Map();
    for (let i = 0; i < meta.summary.nrow; i++) {
      const row: SummaryInfo = {
        row: i,
        item: sm.item[i],
        var: sm.var[i],
        year: sm.year[i],
        eu_mode: (sm.eu_mode[i] ?? 'members') as SummaryInfo['eu_mode'],
        W: sm.W[i],
        basis: (sm.basis[i] ?? 'none') as WorldBasis,
        n: sm.n[i],
        top1_geo: i32(sm.top1_geo[i]),
        top1_share: sm.top1_share[i],
        top3_share: sm.top3_share[i],
        hhi: sm.hhi[i],
        hhi_lower_bound: b(sm.hhi_lower_bound[i]),
        eff_n: sm.eff_n[i],
        china: sm.china[i],
        core: sm.core[i],
        partner: sm.partner[i],
        rest: sm.rest[i],
        unattributed: sm.unattributed[i],
        growth: sm.growth[i],
        growth_kind: sm.growth_kind[i],
      };
      this.summary.push(row);
      this.summaryByKey.set(`${row.item}|${row.var}|${row.year}|${row.eu_mode}`, row);
    }

    const rc = meta.reconcile.cols;
    this.reconcile = [];
    for (let i = 0; i < meta.reconcile.nrow; i++) {
      this.reconcile.push({
        item: rc.item[i],
        year: rc.year[i],
        matrix_total: rc.matrix_total[i],
        matrix_china: rc.matrix_china[i],
        matrix_hhi: rc.matrix_hhi[i],
        reported_total: rc.reported_total[i],
        reported_china: rc.reported_china[i],
        reported_hhi: rc.reported_hhi[i],
        n_reporters: i32(rc.n_reporters[i]),
        warn: b(rc.warn[i]),
      });
    }

    const l = meta['dims.item_links'].cols;
    this.linksByItem = new Map();
    this.linksByCell = new Map();
    for (let i = 0; i < meta['dims.item_links'].nrow; i++) {
      const item = l.item[i];
      const cell = l.cell[i];
      const weight = l.weight[i];
      push(this.linksByItem, item, { cell, weight });
      push(this.linksByCell, cell, { item, weight });
    }
    const rm = meta['dims.region_members'].cols;
    this.regionMembers = new Map();
    for (let i = 0; i < meta['dims.region_members'].nrow; i++) push(this.regionMembers, rm.geo[i], rm.member[i]);

    const md = meta['dims.mineral_demand'].cols;
    this.mineralDemand = [];
    for (let i = 0; i < meta['dims.mineral_demand'].nrow; i++) {
      this.mineralDemand.push({ mineral: md.mineral[i], tech: md.tech[i], label: md.label[i] ?? '', scenario: md.scenario[i] ?? '', year: md.year[i], kt: md.kt[i] });
    }
    this.euOutsideCoalition = [...(meta['dims.eu_outside_coalition'] as (string | null)[])].filter((x): x is string => typeof x === 'string');
  }

  // ---- lookups ---------------------------------------------------------------------------------

  item(axis: ItemAxis, slug: string): ItemInfo | null {
    return this.itemByKey.get(`${axis}:${slug}`) ?? null;
  }

  cell(tech: TechSlug, stage: StageSlug): ItemInfo | null {
    return this.item('cell', `${tech}-${stage}`);
  }

  /** The unit of (var, item): the per-item override (dict.item_units) or the var's. */
  unitOf(v: VarInfo, item: number | null): UnitInfo {
    if (item !== null) {
      const o = this.itemUnits.get(`${v.idx}|${item}`);
      if (o) return o;
    }
    return v.unit;
  }

  geo(idx: number | null | undefined): GeoInfo | null {
    return idx === null || idx === undefined || idx < 0 ? null : (this.geos[idx] ?? null);
  }

  /** Display name of a geo index ("China", "European Union (27)"). */
  geoName(idx: number | null | undefined): string {
    return this.geo(idx)?.name ?? '';
  }

  summaryOf(item: number, v: number, year: number, eu: 'members' | 'bloc'): SummaryInfo | null {
    return this.summaryByKey.get(`${item}|${v}|${year}|${eu}`) ?? null;
  }

  /** The latest observed summary year of (item, var) for an EU mode (projection years skipped), or null. */
  summaryLatest(item: number, v: number, eu: 'members' | 'bloc'): SummaryInfo | null {
    const from = this.vars[v]?.projection_from ?? null;
    let best: SummaryInfo | null = null;
    for (const s of this.summary) if (s.item === item && s.var === v && s.eu_mode === eu && (from === null || s.year < from) && (!best || s.year > best.year)) best = s;
    return best;
  }

  /** Availability rows of a var (every item), or of one (var, item). */
  availOf(v: VarInfo, item?: number): AvailInfo[] {
    const rows = this.availByVar.get(v.idx) ?? [];
    return item === undefined ? rows : rows.filter((r) => r.item === item);
  }

  /** Items of a var that have data, in item order. */
  itemsWithData(v: VarInfo): ItemInfo[] {
    const ids = new Set((this.availByVar.get(v.idx) ?? []).map((r) => r.item));
    return this.items.filter((x) => ids.has(x.idx)).sort((a, z) => a.sort - z.sort);
  }

  /** Vars of a theme (dictionary order). */
  varsOfTheme(theme: string): VarInfo[] {
    return this.vars.filter((v) => v.theme === theme);
  }

  /** Caveat texts of a var (dictionary wording). */
  /** The basket notes of a cell (dict.cell_caveats), as caveat texts. */
  cellCaveats(slug: string): { id: string; text: string }[] {
    return (this.cellCaveatIds.get(slug) ?? []).map((id) => ({ id, text: this.caveats.get(id) ?? '' })).filter((x) => x.text);
  }

  caveatTexts(v: VarInfo): string[] {
    return v.caveats.map((c) => this.caveats.get(c)).filter((x): x is string => !!x);
  }

  /** The roster set of a var (the economies the source covers), or null when unknown. */
  rosterOf(v: VarInfo): Set<number> | null {
    return v.roster ? (this.rosters.get(v.roster) ?? null) : null;
  }

  /** Coverage rows of an item (one per family present). */
  coverageOf(item: number): CoverageInfo[] {
    const out: CoverageInfo[] = [];
    for (const f of FAMILY_IDS) {
      const c = this.coverage.get(`${item}|${f}`);
      if (c) out.push(c);
    }
    return out;
  }

  /** The vars whose primary_for lists this cell slug. */
  primaryVarsOf(cellSlug: string): VarInfo[] {
    return this.vars.filter((v) => v.primary_for.includes(cellSlug));
  }

  /** Cells linked to a non-cell item (mineral -> the cells that use it), heaviest first. */
  cellsOfItem(item: number): { cell: ItemInfo; weight: number }[] {
    return (this.linksByItem.get(item) ?? [])
      .map((l) => ({ cell: this.items[l.cell], weight: l.weight }))
      .filter((x) => !!x.cell)
      .sort((a, z) => (z.weight || 0) - (a.weight || 0));
  }

  /** Items linked to a cell (Batteries Extraction -> lithium, nickel, ...), heaviest first. */
  itemsOfCell(cell: number, axis?: ItemAxis): { item: ItemInfo; weight: number }[] {
    return (this.linksByCell.get(cell) ?? [])
      .map((l) => ({ item: this.items[l.item], weight: l.weight }))
      .filter((x) => !!x.item && (!axis || x.item.axis === axis))
      .sort((a, z) => (z.weight || 0) - (a.weight || 0));
  }

  /** "Batteries — Refining". */
  cellLabel(item: ItemInfo): string {
    return item.tech && item.stage ? `${item.tech} — ${item.stage}` : item.label;
  }
}

function push<K, V>(m: Map<K, V[]>, k: K, v: V) {
  let l = m.get(k);
  if (!l) m.set(k, (l = []));
  l.push(v);
}

/** Cells ordered by TECH_SLUGS then stage. */
function cellOrder(x: ItemInfo): number {
  const t = x.techSlug ? TECH_SLUGS.indexOf(x.techSlug) : 99;
  const s = x.stageSlug ? STAGE_SLUGS.indexOf(x.stageSlug) : 9;
  return t * 10 + s;
}

const cache = new WeakMap<SectorMetaChunk, SectorModel>();

/** The model of a meta chunk (built once per object). */
export function modelOf(meta: SectorMetaChunk): SectorModel {
  let m = cache.get(meta);
  if (!m) {
    m = new SectorModel(meta);
    cache.set(meta, m);
  }
  return m;
}

