// The Sector explorer embedded in the Country profile's fence sections (2026-10-30, the user's request: pick a
// variable of a fence's analysis and see "the relevant chart ... from the Sectors section"). A lazy chunk: the
// Country page loads it, and with it the Sector data (manifest, sector-meta, then the chunks the measure needs),
// only when the reader picks a variable (src/country/views/profile/VariableChart.tsx).
//
// It is the Explore page (ExploreView) for one measure without the page chrome: the measures that chart the
// variable at the node (posture/variableCharts.ts; a select when there are several, a measure without data for
// the node marked so), preset to the node (tech + stage, the linked mineral or fuel) and the economy
// highlighted (c); the item, the year, the chart ("Chart": one of the layout's charts, sector/derive/explore.ts
// ExplorePart) and, for country measures, Show as / Scale / Economies under "More filters". The state lives in
// memory (SectorStateProvider): the Country page's URL never changes. "Open in Sectors" opens the same view as
// the full /sector/explore/<theme> page. Where the node has no data for the measure, a note says so and names
// what is shown instead; nothing is invented.

import { useEffect, useMemo, useState, useSyncExternalStore, type ReactNode } from 'react';
import { Link } from 'react-router';
import { Disclosure, Loading, Notice, Segmented, Select } from '../../../components/ui.tsx';
import { chartState, type ChartNode, type SectorChart } from '../../../posture/variableCharts.ts';
import { CaptionProvider } from '../../charts/caption.tsx';
import { SvgDefs } from '../../charts/SvgDefs.tsx';
import { stageSlugOf, techSlugOf } from '../../contract.ts';
import { factsIndexOf } from '../../data/facts.ts';
import { useModel, useSectorMeta, useSectorStore } from '../../data/SectorDataProvider.tsx';
import type { ItemInfo, SectorModel, VarInfo } from '../../data/model.ts';
import { EXPLORE_PART_LABEL, layoutBanner, layoutOf, partsOf, pickItem, pickPart, pickYear, SCOPE_LABEL, shareLegal, themeVars, type ExplorePart } from '../../derive/explore.ts';
import { lc } from '../../format.ts';
import { GEO_SCOPES, type GeoScope, type ScaleParam, type SectorUrlState, type ShowAs } from '../../url.ts';
import { makeCaptionResolver, periodText } from '../caption.ts';
import { ChunkGate, MeasureTip, useHighlight } from '../common.tsx';
import { useFacts, useSeries } from '../data.ts';
import { SectorStateProvider, useSectorUrl } from '../state.tsx';
import type { ExploreCtx } from './ctx.ts';
import { Body, FACTS_LAYOUTS, ItemPicker } from './parts.tsx';

export interface EmbeddedExploreProps {
  /** the Sector measures that chart the variable at the node, in order of preference (variableCharts.ts chartsAt) */
  charts: readonly SectorChart[];
  /** the node: its technology and stage, and its words ("Solar · Final Assembly & Equipment") */
  node: ChartNode & { label: string };
  /** the economy of the profile, highlighted */
  iso3: string;
  /** the variable's label ("Investment momentum") */
  variable: string;
}

// ---- the hatch pattern, once on the page however many explorers are open ----------------------------------

const hatchOwners: number[] = [];
const hatchSubs = new Set<() => void>();
let hatchSeq = 0;
const hatchSubscribe = (f: () => void) => {
  hatchSubs.add(f);
  return () => hatchSubs.delete(f);
};
/** True for the first mounted explorer: it draws SvgDefs (the hatch's id is fixed). */
function useHatchOwner(): boolean {
  const [id] = useState(() => ++hatchSeq);
  useEffect(() => {
    hatchOwners.push(id);
    hatchSubs.forEach((f) => f());
    return () => {
      hatchOwners.splice(hatchOwners.indexOf(id), 1);
      hatchSubs.forEach((f) => f());
    };
  }, [id]);
  return useSyncExternalStore(hatchSubscribe, () => hatchOwners[0] === id, () => false);
}

// ---- the candidates ---------------------------------------------------------------------------------------

/** One measure that charts the variable, resolved against the Sector model for the node. */
interface Candidate {
  chart: SectorChart;
  /** null: the measure has no data in this Sector build */
  v: VarInfo | null;
  /** the state the explorer opens on */
  initial: Partial<SectorUrlState>;
  /** the node (its cell, linked item or fuel) has data for the measure */
  covered: boolean;
  /** what the item is, where the explorer had to choose or fall back (null: the node's own) */
  itemNote: string | null;
}

/** The highlight code of the economy (null when the Sector data have no such economy). */
function highlightOf(model: SectorModel, iso3: string): string | null {
  const g = model.geoByCode.get(iso3);
  return g && (g.kind === 'country' || g.kind === 'eu') ? g.code : null;
}

function resolve(model: SectorModel, chart: SectorChart, node: ChartNode & { label: string }, iso3: string): Candidate {
  const v = model.varById.get(chart.m) ?? null;
  const live = !!v && themeVars(model, chart.theme).some((x) => x.id === chart.m);
  const initial = chartState(chart, node, highlightOf(model, iso3));
  if (!v || !live) return { chart, v: null, initial, covered: false, itemNote: null };
  const has = new Set(model.itemsWithData(v).map((x) => x.idx));
  const tech = techSlugOf(node.tech);
  const cellOf = (stage: string): ItemInfo | null => {
    const s = stageSlugOf(stage);
    return tech && s ? model.cell(tech, s) : null;
  };
  const shown = (it: ItemInfo | null) => (it ? (it.axis === 'cell' ? model.cellLabel(it) : it.label) : 'another item');
  const fallback = () => shown(pickItem(model, v, { tech: null, stage: null, item: null }));
  switch (chart.item.kind) {
    case 'none':
      return { chart, v, initial, covered: true, itemNote: null };
    case 'cell':
    case 'deployment': {
      const cell = cellOf(chart.item.kind === 'cell' ? node.stage : 'Deployment');
      const covered = !!cell && has.has(cell.idx);
      const where = chart.item.kind === 'cell' ? node.label : `${node.tech} · Deployment & Use`;
      return { chart, v, initial, covered, itemNote: covered ? (chart.item.kind === 'deployment' ? `Shown for ${where}, the technology’s deployment.` : null) : `The Sector data have no ${lc(v.label)} for ${where}; showing ${fallback()}.` };
    }
    case 'linked': {
      const cell = cellOf(node.stage);
      const links = cell ? model.itemsOfCell(cell.idx, v.item_axis) : [];
      const hit = links.find((l) => has.has(l.item.idx))?.item ?? null;
      const axis = v.item_axis === 'mineral' || v.item_axis === 'mineral_stage' ? 'mineral' : v.item_axis === 'activity' ? 'activity' : 'item';
      if (hit) {
        const others = links.filter((l) => l.item.idx !== hit.idx && has.has(l.item.idx)).length;
        return {
          chart, v, initial: { ...initial, item: hit.slug }, covered: true,
          itemNote: `Showing ${hit.label}, ${links[0].item.idx === hit.idx ? `the ${axis} linked to ${node.label}` : `the ${axis} with data most linked to ${node.label}`}${others ? `; pick another of its ${others + 1} with the ${axis === 'item' ? 'Item' : axis === 'mineral' ? 'Mineral' : 'Activity'} select` : ''}.`,
        };
      }
      return { chart, v, initial, covered: false, itemNote: `None of the ${axis === 'item' ? 'items' : `${axis}s`} linked to ${node.label} has ${lc(v.label)} in the Sector data; showing ${fallback()}.` };
    }
    case 'tech': {
      const slug = chart.item.items[node.tech];
      const it = slug ? model.item(v.item_axis, slug) : null;
      const covered = !!it && has.has(it.idx);
      return { chart, v, initial, covered, itemNote: covered ? `Shown for ${it?.label}.` : `The Sector data have no ${lc(v.label)} for ${slug ?? node.tech}; showing ${fallback()}.` };
    }
  }
}

// ---- the explorer -----------------------------------------------------------------------------------------

/** The body: the filter row, the notes on the measure, the chart, "Open in Sectors". */
function Embedded({ cand, measureSelect }: { cand: Candidate & { v: VarInfo }; measureSelect: ReactNode }) {
  const model = useModel();
  const { state, set, link } = useSectorUrl();
  const { chart, v } = cand;
  const highlight = useHighlight(model);
  const layout = layoutOf(v);
  const item = v.item_axis === 'none' ? model.none : pickItem(model, v, state);
  const { facts, h: fh } = useFacts(FACTS_LAYOUTS.has(layout));
  const needSeries = !!item && layout !== 'capacity' && model.availOf(v, item.idx).some((a) => a.chunk === 'series');
  const { series } = useSeries(needSeries);
  const years = useMemo(() => {
    if (!item) return [];
    if (layout === 'dyad' || layout === 'devflows') return [...v.years];
    if (layout === 'fdi') return [];
    return [...new Set([...(facts?.yearsOf(v.idx, item.idx) ?? []), ...(series?.yearsOf(v.idx, item.idx) ?? [])])].sort((a, z) => a - z);
  }, [v, item, layout, facts, series]);
  const year = pickYear(v, years, state.y);
  const parts = partsOf(layout);
  const [part, setPart] = useState<ExplorePart>(() => pickPart(layout, chart.chart));
  const ctx: ExploreCtx = {
    model, theme: chart.theme, v, item, year, years, facts, series, highlight,
    eu: state.eu,
    scope: state.geo,
    as: state.as === 'share' && shareLegal(v) ? 'share' : 'value',
    scale: state.scale,
    setYear: (y) => set({ y }),
    setItem: (it) => (it.axis === 'cell' ? set({ tech: it.tech, stage: it.stage, item: null, sub: null }) : set({ item: it.slug, tech: null, stage: null, sub: null })),
    setHighlight: (g) => set({ c: g ? g.code : null }),
    part,
  };
  // the note on the item holds while the reader keeps the item the explorer chose
  const sameItem = state.tech === (cand.initial.tech ?? null) && state.stage === (cand.initial.stage ?? null) && state.item === (cand.initial.item ?? null);
  const banner = layoutBanner(v);
  const href = link(`/sector/explore/${chart.theme}`, { m: v.id, y: state.y, tech: state.tech, stage: state.stage, item: state.item, sub: state.sub, as: state.as, scale: state.scale, geo: state.geo });
  const countryish = layout === 'country';
  const period = v.year_field && year !== null ? periodText(v, 'long') : null;
  return (
    <div className="space-y-4" data-testid="variable-chart-explore" data-m={v.id} data-tech={item?.tech ?? ''} data-stage={item?.stage ?? ''} data-item={item?.slug ?? ''} data-c={state.c ?? ''} data-part={part}>
      <p className="text-xs text-paper-mute" data-testid="variable-chart-basis">
        <span className="text-paper-dim">{chart.basis === 'source' ? 'Same source as the variable.' : 'A related Sector measure, not the variable’s own input.'}</span> {chart.note}
      </p>
      <div className="flex flex-wrap items-end gap-x-4 gap-y-3">
        {measureSelect}
        <MeasureTip v={v} />
        <ItemPicker v={v} item={item} onPick={ctx.setItem} grid={false} />
        {years.length > 1 && layout !== 'policy' && !v.year_field && (
          <Select label="Year" value={String(year ?? '')} options={[...years].reverse().map((y) => ({ value: String(y), label: v.projection_from !== null && y >= v.projection_from ? `${y} (projection)` : String(y) }))} onChange={(y) => set({ y: Number(y) })} className="min-w-[7rem]" />
        )}
        {period && <p className="pb-1 text-sm text-paper-dim">{period}</p>}
        {parts.length > 1 && <Select label="Chart" value={part} options={parts.map((p) => ({ value: p, label: EXPLORE_PART_LABEL[p] }))} onChange={(p) => setPart(p as ExplorePart)} className="min-w-[12rem]" />}
      </div>
      {countryish && (
        <Disclosure summary="More filters: show as, scale, economies">
          <div className="flex flex-wrap items-end gap-x-5 gap-y-3">
            <Segmented<ShowAs>
              label="Show as"
              value={ctx.as}
              options={[
                { value: 'value', label: 'Value' },
                { value: 'share', label: 'Share of world', disabled: !shareLegal(v), title: shareLegal(v) ? undefined : 'No world total for this measure: shares are not defined.' },
              ]}
              onChange={(a) => set({ as: a })}
            />
            <Segmented<ScaleParam> label="Scale" value={state.scale ?? v.scale ?? 'linear'} options={[{ value: 'linear', label: 'Linear' }, { value: 'log', label: 'Log' }]} onChange={(s) => set({ scale: s })} />
            <Segmented<GeoScope> label="Economies" value={state.geo} options={GEO_SCOPES.map((g) => ({ value: g, label: SCOPE_LABEL[g] }))} onChange={(g) => set({ geo: g })} />
          </div>
        </Disclosure>
      )}
      {sameItem && cand.itemNote && (
        <p className={`text-sm ${cand.covered ? 'text-paper-dim' : 'text-paper'}`} data-testid="variable-chart-item-note" data-covered={cand.covered ? 'yes' : 'no'}>
          {cand.itemNote}
        </p>
      )}
      {banner && <Notice kind="info">{banner}</Notice>}
      {FACTS_LAYOUTS.has(layout) ? (
        <ChunkGate h={fh} name="sector-facts">
          {() => <Body c={ctx} layout={layout} />}
        </ChunkGate>
      ) : (
        <Body c={ctx} layout={layout} />
      )}
      <p className="text-sm">
        <Link className="link" to={href} data-testid="variable-chart-open">
          Open in Sectors
        </Link>{' '}
        <span className="text-paper-mute">for every chart of {lc(v.label)}, its notes and its source ({v.source.short}).</span>
      </p>
    </div>
  );
}

/** The measures of the variable, resolved; the chosen one's explorer. */
function Picker({ model, charts, node, iso3, variable }: EmbeddedExploreProps & { model: SectorModel }) {
  const cands = useMemo(() => charts.map((c) => resolve(model, c, node, iso3)), [model, charts, node, iso3]);
  const first = Math.max(0, cands.findIndex((c) => c.v && c.covered));
  const [pick, setPick] = useState<number | null>(null);
  const i = pick !== null && pick < cands.length ? pick : first;
  const cand = cands[i];
  if (!cand) return null;
  const measureSelect =
    cands.length > 1 ? (
      <Select
        label="Sector measure"
        value={String(i)}
        options={cands.map((c, k) => ({
          value: String(k),
          label: c.v ? `${c.v.label}${c.covered ? '' : ' (no data for this step)'}` : `${c.chart.m} (not in this data build)`,
          disabled: !c.v,
        }))}
        onChange={(k) => setPick(Number(k))}
        className="min-w-[14rem]"
      />
    ) : null;
  if (!cand.v) {
    return (
      <div className="space-y-3">
        {measureSelect}
        <p className="text-sm text-paper-mute" data-testid="variable-chart-missing">
          The Sector measure for {lc(variable)} ({cand.chart.m}) has no data in this Sector build.
        </p>
      </div>
    );
  }
  return (
    <SectorStateProvider key={`${i}|${cand.chart.m}|${node.tech}|${node.stage}`} initial={cand.initial}>
      <Embedded cand={cand as Candidate & { v: VarInfo }} measureSelect={measureSelect} />
    </SectorStateProvider>
  );
}

export default function EmbeddedExplore(props: EmbeddedExploreProps) {
  const meta = useSectorMeta();
  const store = useSectorStore();
  const hatch = useHatchOwner();
  const model = meta.model;
  const captions = useMemo(
    () =>
      model
        ? makeCaptionResolver(model, () => {
            const f = store.chunkState('sector-facts').data;
            return f ? factsIndexOf(f) : null;
          })
        : null,
    [model, store],
  );
  if (meta.status === 'error') {
    return (
      <Notice kind="error" title="The sector data could not be loaded" action={<button type="button" className="btn btn-ghost btn-sm" onClick={meta.retry}>Retry</button>}>
        {meta.error?.message ?? ''}
      </Notice>
    );
  }
  if (!model || !captions) return <Loading label="Loading the sector data…" />;
  return (
    <CaptionProvider resolve={captions}>
      {hatch && <SvgDefs />}
      <Picker model={model} {...props} />
    </CaptionProvider>
  );
}
