// Explore /sector/explore/:theme? (spec_sector.md 4.6): pick a measure, compare countries, across every
// theme including the measures that are not technology-specific. Theme chips; the filter row (Measure
// with optgroups = families; Item: Technology + Stage with a cell grid at >= 1024 px, or one select;
// Year, or "latest available up to" for per-row years; Show as; Scale; Economies; on phones the last
// four sit behind "More filters (n)"); the result line; then the layout of the measure's native level
// (country, region, entity, world, cell_global, dyad) or its special form (policy, area-integrated
// potential, mineral demand, manufacturing capacity), and the notes card. The default theme is trade;
// an unknown theme goes to /sector/explore with a notice.

import { useMemo } from 'react';
import { Navigate, useParams } from 'react-router';
import { Disclosure, FilterRow, Notice, PageHead, Segmented, Select } from '../../../components/ui.tsx';
import { useMediaQuery } from '../../../shell/useMediaQuery.ts';
import { useTitle } from '../../../shell/title.ts';
import { STAGE_LABELS, STAGE_SLUGS, TECH_LABELS, TECH_SLUGS, type StageSlug, type TechSlug } from '../../contract.ts';
import { useModel } from '../../data/SectorDataProvider.tsx';
import type { GeoInfo, ItemInfo, VarInfo } from '../../data/model.ts';
import { itemsOf, layoutBanner, layoutOf, pickItem, pickVar, pickYear, resultLine, SCOPE_LABEL, shareLegal, themeVars, type ExploreLayout } from '../../derive/explore.ts';
import { slice } from '../../derive/summary.ts';
import { GEO_SCOPES, type GeoScope, type ScaleParam, type ShowAs } from '../../url.ts';
import { ChunkGate, useHighlight } from '../common.tsx';
import { useFacts, useSeries } from '../data.ts';
import { ThemeChips } from '../kit.tsx';
import { useSectorUrl } from '../state.tsx';
import { CountryLayout } from './CountryLayout.tsx';
import { indexFor, type ExploreCtx } from './ctx.ts';
import { FlowsLayout } from './Flows.tsx';
import { CapacityLayout, CellGlobalLayout, EntityLayout, MineralDemandLayout, RegionLayout, ScatterLayout, WorldLayout } from './Layouts.tsx';
import { NotesCard } from './Notes.tsx';
import { PolicyExplore } from './PolicyExplore.tsx';

const FACTS_LAYOUTS: ReadonlySet<ExploreLayout> = new Set(['country', 'region', 'entity', 'world', 'cell_global', 'scatter']);

const AXIS_WORDS: Record<string, string> = { mineral: 'Mineral', fuel: 'Fuel', commodity: 'Commodity', activity: 'Activity', policy_tag: 'Policy tag', cell: 'Sector' };

/** The 11 x 3 picker of cells (>= 1024 px): options without data disabled, the current one outlined. */
function CellPicker({ v, item, onPick }: { v: VarInfo; item: ItemInfo | null; onPick: (it: ItemInfo) => void }) {
  const model = useModel();
  const has = new Set(itemsOf(model, v).map((x) => x.idx));
  const techs = TECH_SLUGS.filter((t) => model.cells.some((c) => c.techSlug === t));
  return (
    <div className="grid w-fit grid-cols-[auto_repeat(3,2rem)] items-center gap-1 text-xs" role="group" aria-label="Pick a sector">
      <span />
      {STAGE_SLUGS.map((s) => (
        <span key={s} className="text-center text-paper-mute" aria-hidden="true">
          {STAGE_LABELS[s][0]}
        </span>
      ))}
      {techs.map((t) => (
        <div key={t} className="contents">
          <span className="pr-2 text-paper-dim">{TECH_LABELS[t]}</span>
          {STAGE_SLUGS.map((s) => {
            const cell = model.cell(t, s);
            const ok = !!cell && has.has(cell.idx);
            const cur = !!cell && item?.idx === cell.idx;
            return (
              <button
                key={s}
                type="button"
                className={`h-6 w-8 border ${cur ? 'border-signal bg-ink-700' : 'hairline'} ${ok ? 'bg-ink-700/60 hover:bg-ink-600' : 'opacity-30'}`}
                disabled={!ok}
                aria-pressed={cur}
                aria-label={`${TECH_LABELS[t]} — ${STAGE_LABELS[s]}${ok ? '' : ': no data'}`}
                title={`${TECH_LABELS[t]} — ${STAGE_LABELS[s]}${ok ? '' : ': no data'}`}
                onClick={() => cell && onPick(cell)}
              />
            );
          })}
        </div>
      ))}
    </div>
  );
}

function ItemPicker({ v, item, onPick }: { v: VarInfo; item: ItemInfo | null; onPick: (it: ItemInfo) => void }) {
  const model = useModel();
  const wide = useMediaQuery('(min-width: 1024px)');
  const items = itemsOf(model, v);
  if (v.item_axis === 'none' || items.length <= 1) return items.length === 1 && v.item_axis !== 'none' ? <p className="text-sm text-paper-dim">{items[0].axis === 'cell' ? model.cellLabel(items[0]) : items[0].label}</p> : null;
  if (v.item_axis === 'cell') {
    const has = new Set(items.map((x) => x.idx));
    const tech = (item?.techSlug ?? items[0].techSlug) as TechSlug;
    const stage = (item?.stageSlug ?? items[0].stageSlug) as StageSlug;
    const techOk = (t: TechSlug) => STAGE_SLUGS.some((s) => { const c = model.cell(t, s); return !!c && has.has(c.idx); });
    const pickTech = (t: TechSlug) => {
      const s = [stage, ...STAGE_SLUGS].find((x) => { const c = model.cell(t, x); return !!c && has.has(c.idx); });
      const c = s ? model.cell(t, s) : null;
      if (c) onPick(c);
    };
    return (
      <>
        <Select label="Technology" value={tech} options={TECH_SLUGS.filter((t) => model.cells.some((c) => c.techSlug === t)).map((t) => ({ value: t, label: techOk(t) ? TECH_LABELS[t] : `${TECH_LABELS[t]} (no data)`, disabled: !techOk(t) }))} onChange={(t) => pickTech(t as TechSlug)} className="min-w-[10rem]" />
        <Select label="Stage" value={stage} options={STAGE_SLUGS.map((s) => { const c = model.cell(tech, s); const ok = !!c && has.has(c.idx); return { value: s, label: ok ? STAGE_LABELS[s] : `${STAGE_LABELS[s]} (no data)`, disabled: !ok }; })} onChange={(s) => { const c = model.cell(tech, s as StageSlug); if (c) onPick(c); }} className="min-w-[8rem]" />
        {wide && (
          <div className="w-full">
            <Disclosure summary="Pick on the sector grid">
              <CellPicker v={v} item={item} onPick={onPick} />
            </Disclosure>
          </div>
        )}
      </>
    );
  }
  return <Select label={AXIS_WORDS[v.item_axis] ?? 'Item'} value={item?.slug ?? items[0].slug} options={items.map((x) => ({ value: x.slug, label: x.label }))} onChange={(slug) => { const it = items.find((x) => x.slug === slug); if (it) onPick(it); }} className="min-w-[10rem]" />;
}

function Body({ c, layout }: { c: ExploreCtx; layout: ExploreLayout }) {
  switch (layout) {
    case 'country':
      return <CountryLayout c={c} />;
    case 'region':
      return <RegionLayout c={c} />;
    case 'entity':
      return <EntityLayout c={c} />;
    case 'world':
      return <WorldLayout c={c} />;
    case 'cell_global':
      return <CellGlobalLayout c={c} />;
    case 'dyad':
      return <FlowsLayout c={c} />;
    case 'policy':
      return <PolicyExplore c={c} />;
    case 'scatter':
      return <ScatterLayout c={c} />;
    case 'mineral_demand':
      return <MineralDemandLayout c={c} />;
    case 'capacity':
      return <CapacityLayout c={c} />;
  }
}

export default function ExploreView() {
  const { theme: rawTheme } = useParams();
  const model = useModel();
  const { state, set, link } = useSectorUrl();
  const phone = !useMediaQuery('(min-width: 768px)');
  const themeInfo = rawTheme ? model.themes.find((t) => t.slug === rawTheme) : (model.themes.find((t) => t.slug === 'trade') ?? model.themes[0]);
  const theme = themeInfo?.slug ?? 'trade';
  useTitle(themeInfo ? `Explore ${themeInfo.label}` : 'Explore', 'Sector Insights');
  const highlight = useHighlight(model);
  const v = pickVar(model, theme, state.m);
  const layout = v ? layoutOf(v) : null;
  const item = v ? (v.item_axis === 'none' ? model.none : pickItem(model, v, state)) : null;
  const { facts, h: fh } = useFacts(!!layout && FACTS_LAYOUTS.has(layout));
  const needSeries = !!v && !!item && layout !== 'capacity' && model.availOf(v, item.idx).some((a) => a.chunk === 'series');
  const { series } = useSeries(needSeries);
  const years = useMemo(() => {
    if (!v || !item) return [];
    if (layout === 'dyad') return [...v.years];
    return [...new Set([...(facts?.yearsOf(v.idx, item.idx) ?? []), ...(series?.yearsOf(v.idx, item.idx) ?? [])])].sort((a, z) => a - z);
  }, [v, item, layout, facts, series]);
  const year = v ? pickYear(v, years, state.y) : null;

  if (rawTheme && !themeInfo) return <Navigate to={link('/sector/explore')} replace state={{ notice: `No theme called '${rawTheme}'; showing Trade.` }} />;

  const ctx: ExploreCtx | null = v
    ? {
        model,
        theme,
        v,
        item,
        year,
        years,
        facts,
        series,
        highlight,
        eu: state.eu,
        scope: state.geo,
        as: state.as === 'share' && shareLegal(v) ? 'share' : 'value',
        scale: state.scale,
        setYear: (y) => set({ y }, { replace: true }),
        setItem: (it) => (it.axis === 'cell' ? set({ tech: it.tech, stage: it.stage, item: null }, { replace: true }) : set({ item: it.slug, tech: null, stage: null }, { replace: true })),
        setHighlight: (g: GeoInfo | null) => set({ c: g ? g.code : null }),
      }
    : null;

  const idx = ctx ? indexFor(ctx, year) : null;
  const result = ctx && layout === 'country' && idx && item && year !== null ? resultLine(model, ctx.v, slice(model, idx, ctx.v, item.idx, year, state.eu).rows, state.geo) : null;
  const countryish = layout === 'country';
  const vars = themeVars(model, theme);
  const extra: { key: string; node: React.ReactNode; changed: boolean }[] = [];
  if (v && years.length > 1 && layout !== 'policy' && !v.year_field) {
    extra.push({
      key: 'y',
      changed: state.y !== null,
      node: <Select label="Year" value={String(year ?? '')} options={[...years].reverse().map((y) => ({ value: String(y), label: v.projection_from !== null && y >= v.projection_from ? `${y} (projection)` : String(y) }))} onChange={(y) => set({ y: Number(y) }, { replace: true })} className="min-w-[7rem]" />,
    });
  }
  if (v && countryish) {
    extra.push({
      key: 'as',
      changed: state.as !== 'value',
      node: (
        <Segmented<ShowAs>
          label="Show as"
          value={ctx?.as ?? 'value'}
          options={[
            { value: 'value', label: 'Value' },
            { value: 'share', label: 'Share of world', disabled: !shareLegal(v), title: shareLegal(v) ? undefined : 'No world total for this measure: shares are not defined.' },
            { value: 'gdp', label: '% of GDP', disabled: true, title: 'GDP is not part of this build.' },
          ]}
          onChange={(a) => set({ as: a }, { replace: true })}
        />
      ),
    });
    extra.push({
      key: 'scale',
      changed: state.scale !== null,
      node: <Segmented<ScaleParam> label="Scale" value={state.scale ?? v.scale ?? 'linear'} options={[{ value: 'linear', label: 'Linear' }, { value: 'log', label: 'Log' }]} onChange={(s) => set({ scale: s }, { replace: true })} />,
    });
    extra.push({
      key: 'geo',
      changed: state.geo !== 'all',
      node: <Segmented<GeoScope> label="Economies" value={state.geo} options={GEO_SCOPES.map((g) => ({ value: g, label: SCOPE_LABEL[g] }))} onChange={(g) => set({ geo: g })} />,
    });
  }
  const nChanged = extra.filter((x) => x.changed).length;
  const banner = v ? layoutBanner(v) : null;

  return (
    <div className="space-y-6">
      <PageHead eyebrow="SECTOR INSIGHTS · EXPLORE" title={themeInfo?.label ?? 'Explore'} lede={themeInfo?.description ?? 'One measure across every country and every sector.'}>
        <div className="mt-4">
          <ThemeChips themes={model.themes.map((t) => ({ slug: t.slug, label: t.label, href: link(`/sector/explore/${t.slug}`) }))} active={theme} />
        </div>
      </PageHead>
      {!v || !ctx || !layout ? (
        <Notice kind="info">No measures for {themeInfo?.label ?? 'this theme'} in this data build.</Notice>
      ) : (
        <>
          <FilterRow result={result}>
            <Select
              label="Measure"
              value={v.id}
              options={vars.map((x) => ({ value: x.id, label: x.label, group: model.families.get(x.family)?.label ?? x.family }))}
              onChange={(id) => set({ m: id, y: null, as: 'value', scale: null })}
              className="min-w-[14rem]"
            />
            <ItemPicker v={v} item={item} onPick={ctx.setItem} />
            {v.year_field && year !== null && <p className="pb-1 text-sm text-paper-dim">Latest available up to {year}</p>}
            {phone && extra.length > 0 ? (
              <div className="w-full">
                <Disclosure summary={`More filters (${nChanged})`}>
                  <div className="flex flex-wrap items-end gap-x-5 gap-y-3">
                    {extra.map((x) => (
                      <div key={x.key}>{x.node}</div>
                    ))}
                  </div>
                </Disclosure>
              </div>
            ) : (
              extra.map((x) => <div key={x.key}>{x.node}</div>)
            )}
          </FilterRow>
          {banner && <Notice kind="info">{banner}</Notice>}
          {FACTS_LAYOUTS.has(layout) ? (
            <ChunkGate h={fh} name="sector-facts">
              {() => <Body c={ctx} layout={layout} />}
            </ChunkGate>
          ) : (
            <Body c={ctx} layout={layout} />
          )}
          <NotesCard v={v} />
        </>
      )}
    </div>
  );
}
