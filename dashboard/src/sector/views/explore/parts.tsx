// The pieces of Explore that the Sector page (ExploreView) and the embedded explorer of the Country profile's
// variable charts (EmbeddedExplore, 2026-10-30) share: the item picker (Technology + Stage with the sector
// grid at >= 1024 px, or one select) and the layout body of a measure's native level or special form.

import { Disclosure, Select } from '../../../components/ui.tsx';
import { useMediaQuery } from '../../../shell/useMediaQuery.ts';
import { STAGE_LABELS, STAGE_SHORTS, STAGE_SLUGS, TECH_LABELS, TECH_SLUGS, type StageSlug, type TechSlug } from '../../contract.ts';
import { useModel } from '../../data/SectorDataProvider.tsx';
import type { ItemInfo, VarInfo } from '../../data/model.ts';
import { itemsOf, type ExploreLayout } from '../../derive/explore.ts';
import { CountryLayout } from './CountryLayout.tsx';
import type { ExploreCtx } from './ctx.ts';
import { DevFlowsLayout } from './DevFlowsLayout.tsx';
import { FdiLayout } from './FdiLayout.tsx';
import { FlowsLayout } from './Flows.tsx';
import { CapacityLayout, CellGlobalLayout, EntityLayout, MineralDemandLayout, RegionLayout, ScatterLayout, WorldLayout } from './Layouts.tsx';
import { PolicyExplore } from './PolicyExplore.tsx';

/** The layouts that read the facts chunk (gated on it). */
export const FACTS_LAYOUTS: ReadonlySet<ExploreLayout> = new Set(['country', 'region', 'entity', 'world', 'cell_global', 'scatter']);

const AXIS_WORDS: Record<string, string> = { mineral: 'Mineral', mineral_stage: 'Mineral and stage', fuel: 'Fuel', commodity: 'Commodity', activity: 'Activity', policy_tag: 'Policy tag', cell: 'Sector', indicator: 'Series', aid_sector: 'Aid sector' };

/**
 * The 11 x 6 picker of cells (>= 1024 px): columns headed by the stage shorts (Ext, Ref, Com, Asm, Dep,
 * Rec: initials would clash, Refining and Recovery), options without data disabled, the current one outlined.
 */
function CellPicker({ v, item, onPick }: { v: VarInfo; item: ItemInfo | null; onPick: (it: ItemInfo) => void }) {
  const model = useModel();
  const has = new Set(itemsOf(model, v).map((x) => x.idx));
  const techs = TECH_SLUGS.filter((t) => model.cells.some((c) => c.techSlug === t));
  return (
    <div className="grid w-fit grid-cols-[auto_repeat(6,2.25rem)] items-center gap-1 text-xs" role="group" aria-label="Pick a sector">
      <span />
      {STAGE_SLUGS.map((s) => (
        <span key={s} className="text-center text-paper-mute" aria-hidden="true" title={STAGE_LABELS[s]}>
          {STAGE_SHORTS[s]}
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
                className={`h-6 w-9 border ${cur ? 'border-signal bg-ink-700' : 'hairline'} ${ok ? 'bg-ink-700/60 hover:bg-ink-600' : 'opacity-30'}`}
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

/** The item of a measure: Technology + Stage for a cell measure (the sector grid at >= 1024 px unless `grid` is false), else one select. */
export function ItemPicker({ v, item, onPick, grid = true }: { v: VarInfo; item: ItemInfo | null; onPick: (it: ItemInfo) => void; grid?: boolean }) {
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
        {wide && grid && (
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

/** The layout of a measure (ctx.part: one chart of it, in an embedded explorer). */
export function Body({ c, layout }: { c: ExploreCtx; layout: ExploreLayout }) {
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
    case 'fdi':
      return <FdiLayout c={c} />;
    case 'devflows':
      return <DevFlowsLayout c={c} />;
  }
}
