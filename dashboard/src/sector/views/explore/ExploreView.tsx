// Explore /sector/explore/:theme? (spec_sector.md 4.6): pick a measure, compare countries, across every
// theme including the measures that are not technology-specific. Theme chips; the filter row (Measure
// with optgroups = families; Item: Technology + Stage with a cell grid at >= 1024 px, or one select;
// Year, or "latest available up to" for per-row years; Show as; Scale; Economies; on phones the last
// four sit behind "More filters (n)"); the result line; then the layout of the measure's native level
// (country, region, entity, world, cell_global, dyad) or its special form (policy, area-integrated
// potential, mineral demand, manufacturing capacity, the CM-FDI cross-border projects), and the notes
// card. The default theme is trade; an unknown theme goes to /sector/explore with a notice.

import { useMemo } from 'react';
import { useParams } from 'react-router';
import { Redirect } from '../../../shell/Redirect.tsx';
import { Disclosure, FilterRow, Notice, PageHead, Segmented, Select } from '../../../components/ui.tsx';
import { useMediaQuery } from '../../../shell/useMediaQuery.ts';
import { useTitle } from '../../../shell/title.ts';
import { useModel } from '../../data/SectorDataProvider.tsx';
import type { GeoInfo } from '../../data/model.ts';
import { layoutBanner, layoutOf, pickItem, pickVar, pickYear, resultLine, SCOPE_LABEL, shareLegal, themeVars } from '../../derive/explore.ts';
import { slice } from '../../derive/summary.ts';
import { GEO_SCOPES, type GeoScope, type ScaleParam, type ShowAs } from '../../url.ts';
import { ChunkGate, MeasureTip, useHighlight } from '../common.tsx';
import { useFacts, useSeries } from '../data.ts';
import { ThemeChips } from '../kit.tsx';
import { useSectorUrl } from '../state.tsx';
import { indexFor, type ExploreCtx } from './ctx.ts';
import { periodText } from '../caption.ts';
import { NotesCard } from './Notes.tsx';
import { Body, FACTS_LAYOUTS, ItemPicker } from './parts.tsx';

const cap1 = (s: string): string => s.charAt(0).toUpperCase() + s.slice(1);

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
    if (layout === 'dyad' || layout === 'devflows') return [...v.years];
    // CM-FDI: every announcement year is in the cards (with their own window switch), no year select
    if (layout === 'fdi') return [];
    return [...new Set([...(facts?.yearsOf(v.idx, item.idx) ?? []), ...(series?.yearsOf(v.idx, item.idx) ?? [])])].sort((a, z) => a - z);
  }, [v, item, layout, facts, series]);
  const year = v ? pickYear(v, years, state.y) : null;

  if (rawTheme && !themeInfo) return <Redirect to={link('/sector/explore')} notice={`No theme called '${rawTheme}'; showing Trade.`} />;

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
        setItem: (it) => (it.axis === 'cell' ? set({ tech: it.tech, stage: it.stage, item: null, sub: null }, { replace: true }) : set({ item: it.slug, tech: null, stage: null, sub: null }, { replace: true })),
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
      <PageHead eyebrow="SECTORS · EXPLORE" title={themeInfo?.label ?? 'Explore'} lede={`One measure across every economy and every sector: a map, a ranking, the spread of values and the trend. Pick a theme and a measure.${themeInfo?.description ? ` This theme: ${themeInfo.description.charAt(0).toLowerCase()}${themeInfo.description.slice(1)}` : ''}`}>
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
            <MeasureTip v={v} />
            <ItemPicker v={v} item={item} onPick={ctx.setItem} />
            {v.year_field && year !== null && <p className="pb-1 text-sm text-paper-dim">{periodText(v, 'long') ? `${cap1(periodText(v, 'long') as string)} (each economy's own years in the table)` : `Latest available up to ${year}`}</p>}
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
          {/* heading order h1 -> h2 -> h3 (the cards are h3): the results heading for screen readers */}
          <h2 className="sr-only">
            Results: {v.label}
            {item && item.axis !== 'none' ? `, ${item.axis === 'cell' ? model.cellLabel(item) : item.label}` : ''}
          </h2>
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
