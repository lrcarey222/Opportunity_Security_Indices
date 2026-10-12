// The caption contract (spec_sector.md 3.9) resolved from the dictionary: SectorChartCard takes
// required `vars` and the layout provides this resolver once (CaptionProvider). Pure apart from the
// facts getter (the counts use the loaded facts when present, else the avail table).

import type { CaptionInfo, CaptionResolver, VarRef } from '../charts/types.ts';
import type { FactsIndex } from '../data/facts.ts';
import type { ItemInfo, SectorModel, VarInfo } from '../data/model.ts';
import { isFdiVar } from '../derive/fdi.ts';

/** The item of a var ref (by slug on the var's item axis), or the var's only item. */
export function refItem(model: SectorModel, v: VarInfo, slug: string | null | undefined): ItemInfo | null {
  if (slug) return model.item(v.item_axis, slug) ?? model.items.find((x) => x.slug === slug) ?? null;
  if (v.item_axis === 'none') return model.none;
  const items = model.itemsWithData(v);
  return items.length === 1 ? items[0] : null;
}

/**
 * The dictionary's data_year in place of the stored year key (8.1.4: the processed Year is never
 * shown) for a single-year measure whose year is static ("static (2025 dataset)"), unconfirmed
 * ("current (year to confirm)") or qualified ("2025 operational; ... planned to 2030").
 */
export function dataYearOverride(v: VarInfo, year: number | null | undefined): string | null {
  if (!v.data_year || year === null || year === undefined || v.years.length !== 1 || v.years[0] !== year) return null;
  if (/^static|year to confirm/i.test(v.data_year)) return v.data_year;
  if (v.data_year.startsWith(`${year} `) && /operational|planned/i.test(v.data_year)) return v.data_year;
  return null;
}

/** "2024", "2018-2025", "2035 · projection", or the dictionary's data_year. */
export function yearText(v: VarInfo, r: VarRef): string {
  if (r.window) return `${r.window[0]}-${r.window[1]}`;
  const override = dataYearOverride(v, r.year);
  if (override) return override;
  if (r.year !== null && r.year !== undefined) {
    if (v.projection_from !== null && r.year >= v.projection_from) return `${r.year} · projection`;
    if (v.year_field) return `latest available up to ${r.year}`;
    return String(r.year);
  }
  return v.data_year;
}

/**
 * The dictionary's data_year as a badge: as written when short, else the years it names ("to 2030",
 * "2018 → 2025"), else the span of the measure's years; the full text stays in the sub line. (A badge
 * does not wrap: the 82-character PAMS text pushed the Policy card 57 px past a 390 px screen.)
 */
export function dataYearBadge(v: VarInfo): string {
  const t = v.data_year;
  if (t.length <= 44) return t;
  const ys = (t.match(/\b(?:19|20)\d{2}\b/g) ?? []).map(Number);
  if (ys.length >= 2) return `${Math.min(...ys)} → ${Math.max(...ys)}`;
  if (ys.length === 1) return /\bto\s+\d{4}\b/.test(t) ? `to ${ys[0]}` : String(ys[0]);
  if (v.years.length) {
    const lo = Math.min(...v.years);
    const hi = Math.max(...v.years);
    return lo === hi ? String(lo) : `${lo} → ${hi}`;
  }
  return `${t.slice(0, 40)}…`;
}

function badgeText(v: VarInfo, r: VarRef): string {
  if (r.window) return `${r.window[0]} → ${r.window[1]}`;
  if (/not updated/i.test(v.data_year)) return v.data_year;
  const override = dataYearOverride(v, r.year);
  if (override) return /^static/i.test(override) ? 'static' : /year to confirm/i.test(override) ? 'year to confirm' : String(r.year);
  if (r.year !== null && r.year !== undefined) {
    if (v.projection_from !== null && r.year >= v.projection_from) return `${r.year} · projection`;
    return v.year_field ? 'latest per economy' : String(r.year);
  }
  return dataYearBadge(v);
}

/** Economies (kind country) with a value in a slice, or the avail count. */
function nEconomies(model: SectorModel, facts: FactsIndex | null, v: VarInfo, item: ItemInfo | null, year: number | null | undefined): number | null {
  if (!item) return null;
  if (facts && year !== null && year !== undefined && facts.has(v.idx, item.idx, year)) {
    return facts.rows(v.idx, item.idx, year).filter((r) => model.geos[r.geo]?.kind === 'country' && Number.isFinite(r.value)).length;
  }
  const a = model.availOf(v, item.idx);
  return a.length ? Math.max(...a.map((x) => x.n_geo)) : null;
}

/**
 * "All 57 coalition members appear." / "48 of 57 coalition members have a value." Null when the slice
 * is not in the loaded facts (a series-only measure: capacity, power additions, EI 1965-2025), so
 * the line never counts an absent slice as zero members.
 */
function coalitionLine(model: SectorModel, facts: FactsIndex | null, v: VarInfo, item: ItemInfo | null, year: number | null | undefined): string | null {
  if (!facts || !item || year === null || year === undefined || v.native_level !== 'country') return null;
  if (!facts.has(v.idx, item.idx, year)) return null;
  const coal = model.countries.filter((g) => g.group === 'core' || g.group === 'partner');
  if (!coal.length) return null;
  const have = new Set(facts.rows(v.idx, item.idx, year).filter((r) => Number.isFinite(r.value)).map((r) => r.geo));
  const n = coal.filter((g) => have.has(g.idx)).length;
  return n === coal.length ? `All ${coal.length} coalition members appear.` : `${n} of ${coal.length} coalition members have a value.`;
}

export function makeCaptionResolver(model: SectorModel, getFacts: () => FactsIndex | null): CaptionResolver {
  return (refs: readonly VarRef[]): CaptionInfo => {
    const facts = getFacts();
    const xs = refs.map((r) => ({ r, v: model.varById.get(r.var) })).filter((x): x is { r: VarRef; v: VarInfo } => !!x.v);
    if (!xs.length) return { sub: '' };
    const subs = xs.map(({ r, v }) => {
      const item = refItem(model, v, r.item);
      const unit = model.unitOf(v, item?.idx ?? null);
      // CM-FDI cards filter the projects (basis, announcement window, economy): the build's destination
      // count would not match them, so those cards state their own counts
      const n = v.native_level === 'country' && !isFdiVar(v.id) ? nEconomies(model, facts, v, item, r.year) : null;
      const src = `${v.source.short}${v.source.edition ? ` ${v.source.edition}` : ''}`;
      return [v.label, unit.long || unit.short, yearText(v, r), src, n ? `${n} economies` : null].filter(Boolean).join(' · ');
    });
    const uniqueSubs = [...new Set(subs)];
    const badges = [...new Set(xs.map(({ r, v }) => badgeText(v, r)))];
    const sources = [...new Set(xs.map(({ v }) => v.source.id))];
    const caveats = [...new Set(xs.flatMap(({ v }) => model.caveatTexts(v)))];
    for (const { r, v } of xs) {
      const unit = model.unitOf(v, refItem(model, v, r.item)?.idx ?? null);
      if (/to confirm|unverified/i.test(`${unit.short} ${unit.long}`) && !caveats.some((c) => /to confirm|unverified/i.test(c))) caveats.push(`Unit to confirm: ${unit.long || unit.short}.`);
    }
    const first = xs[0];
    const coverage = xs.length === 1 ? coalitionLine(model, facts, first.v, refItem(model, first.v, first.r.item), first.r.year) : null;
    const numericYears = xs.map(({ r }) => r.year).filter((y): y is number => typeof y === 'number');
    let badge: string | null = badges.length === 1 ? badges[0] : null;
    if (!badge && numericYears.length) {
      const lo = Math.min(...numericYears);
      const hi = Math.max(...numericYears);
      badge = lo === hi ? String(lo) : `${lo} → ${hi}`;
    }
    const spanNote = badges.length > 1 || sources.length > 1 ? 'The measures in this card differ in source or year; each is shown in its own units and year, and they do not add up.' : null;
    return { sub: uniqueSubs.join('; '), badge, caveats, coverage, spanNote };
  };
}
