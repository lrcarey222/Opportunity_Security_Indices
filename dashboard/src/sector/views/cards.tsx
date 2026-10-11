// Reusable Sector cards: a ranking of one measure (LeagueBars or WorldMap, table twin, CSV, summary,
// value states, highlight pinned) and a share-split card of several measures. Every card goes through
// SectorChartCard, so the caption contract (unit, year, source) is always rendered.

import { useMemo, useState, type ReactNode } from 'react';
import { RampLegend } from '../../charts/legends.tsx';
import { WorldMap } from '../../charts/WorldMap.tsx';
import { divColor } from '../../charts/theme.ts';
import { TableView, type Col } from '../../components/TableView.tsx';
import { Segmented } from '../../components/ui.tsx';
import { useMediaQuery } from '../../shell/useMediaQuery.ts';
import type { FactRow } from '../data/facts.ts';
import type { GeoInfo, ItemInfo, SectorModel, VarInfo } from '../data/model.ts';
import { concentration, type Concentration } from '../derive/concentration.ts';
import { GROUP_SHORT, partOf } from '../derive/groups.ts';
import { stateOf, stateShort } from '../derive/states.ts';
import { concentrationSummary } from '../derive/text.ts';
import { lc, axisFormatter, fmtShare, fmtValue } from '../format.ts';
import { concentrationCsv, fmtFact, leagueRows, mapCells, statesPresent } from './common.tsx';
import { GroupLegend, LeagueBars, SectorChartCard, StateLegend, type VarRef } from './kit.tsx';
import { useCardCsv } from './profile/ctx.tsx';
import { useSectorUrl } from './state.tsx';

export interface LeagueCardProps {
  model: SectorModel;
  v: VarInfo;
  item: ItemInfo | null;
  year: number | null;
  /** the slice rows (EU mode applied) */
  rows: readonly FactRow[];
  /** world total of the slice */
  world?: { value: number } | null;
  title: ReactNode;
  highlight?: GeoInfo | null;
  top?: number;
  /** offer the map view (country-level measures) */
  map?: boolean;
  /** diverging bars around zero (net trade, balances, changes), with the poles in words */
  diverging?: { negLabel: string; posLabel: string };
  tools?: ReactNode;
  notes?: ReactNode;
  /** extra var refs for the caption */
  extraVars?: VarRef[];
  /** a sub line added under the caption */
  sub?: ReactNode;
  /** only these economies are ranked (geo scope); others are shown dimmed on the map */
  scope?: (g: GeoInfo) => boolean;
  /** the economies' count line under the chart */
  footnote?: string | null;
  /** a value per economy shown as "change" in the table (e.g. growth since 2020) */
  change?: Map<number, string>;
  headingLevel?: 2 | 3;
  /** clicking a bar or a country sets the highlight (default) or calls this */
  onPick?: (g: GeoInfo) => void;
  csvName?: string;
}

/** Ranked economies of a slice: the concentration (with shares when a world basis exists). */
export function rankSlice(model: SectorModel, v: VarInfo, rows: readonly FactRow[], world: { value: number } | null | undefined, scope?: (g: GeoInfo) => boolean): Concentration {
  const scoped = scope ? rows.filter((r) => { const g = model.geos[r.geo]; return !g || g.kind !== 'country' || scope(g); }) : rows;
  return concentration(scoped, model.geos, world ?? null, v.world_basis);
}

export function LeagueCard(p: LeagueCardProps) {
  const { model, v, item, year, rows, highlight } = p;
  const { set, state } = useSectorUrl();
  const phone = !useMediaQuery('(min-width: 768px)');
  const [view, setView] = useState<'bars' | 'map'>('bars');
  const conc = useMemo(() => rankSlice(model, v, rows, p.world, p.scope), [model, v, rows, p.world, p.scope]);
  const values = conc.rows.map((r) => r.value).filter((x) => Number.isFinite(x) && x > 0).sort((a, z) => a - z);
  const median = values.length ? values[Math.floor(values.length / 2)] : 0;
  const heavy = values.length > 2 && median > 0 && values[values.length - 1] / median > 100;
  const [scaleLocal, setScale] = useState<'linear' | 'log' | null>(null);
  const scale = scaleLocal ?? state.scale ?? (heavy ? 'log' : (v.scale ?? 'linear'));
  const unit = model.unitOf(v, item?.idx ?? null);
  const league = useMemo(() => leagueRows(model, v, item, conc, { top: p.top ?? (phone ? 10 : 20), highlight, rows }), [model, v, item, conc, p.top, phone, highlight, rows]);
  const fmtAxis = useMemo(() => axisFormatter(conc.rows.map((r) => r.value), unit, v.display_scale), [conc, unit, v.display_scale]);
  const pick = (g: GeoInfo) => (p.onPick ? p.onPick(g) : set({ c: g.code === state.c ? null : g.code }));
  const csv = useCardCsv(() => ({ name: p.csvName ?? `${v.id}-${item?.slug ?? 'all'}-${year ?? ''}`, text: concentrationCsv(model, v, item, year, conc, rows) }), [v.id]);
  const map = useMemo(() => {
    if (!p.map || view !== 'map') return null;
    const div = p.diverging ? divColor(Math.max(...conc.rows.map((r) => Math.abs(r.value)).filter(Number.isFinite), 1e-9)) : undefined;
    return mapCells(model, v, item, rows, { log: scale === 'log' && !p.diverging, highlight, year: year ?? undefined, diverging: div, dimOut: p.scope ? (g) => !p.scope!(g) : undefined });
  }, [p.map, view, model, v, item, rows, scale, highlight, year, p.diverging, conc, p.scope]);
  const roster = model.rosterOf(v);
  // "recorded" = a value > 0 (observed, mirror or estimate); a reported zero is "none recorded" (spec
  // 4.3 S3: "198 economies with recorded 2024 exports · 10 with none recorded")
  // (additive measures only; a 0 of a bounded or ratio measure is an observation)
  const additive = v.agg === 'sum';
  const nRecorded = additive ? conc.rows.filter((r) => r.value > 0).length : conc.rows.filter((r) => Number.isFinite(r.value)).length;
  const nZero = additive ? conc.rows.filter((r) => r.value === 0).length : 0;
  const nNone = nZero + (roster ? [...roster].filter((g) => model.geos[g]?.kind === 'country' && !rows.some((r) => r.geo === g)).length : 0);
  const notCovered = roster ? model.countries.filter((g) => !roster.has(g.idx)).length : 0;
  const coal = model.countries.filter((g) => g.group === 'core' || g.group === 'partner');
  const coalHave = coal.filter((g) => rows.some((r) => r.geo === g.idx && Number.isFinite(r.value))).length;
  const footnote =
    p.footnote !== undefined
      ? p.footnote
      : `Ranked: ${nRecorded} economies with recorded ${year ?? ''} ${lc(v.short)}`.replace('  ', ' ') +
        (nNone ? ` · ${nNone} with none recorded` : '') +
        (notCovered ? ` · ${notCovered} not covered by ${v.source.short}` : '') +
        (coal.length ? ` · coalition coverage ${coalHave}/${coal.length}` : '');
  const cols: Col<(typeof conc.rows)[number]>[] = [
    { id: 'rank', header: 'Rank', value: (r) => r.rank, num: true },
    { id: 'country', header: 'Economy', value: (r) => model.geos[r.geo]?.name ?? '', minWidth: 150 },
    { id: 'group', header: 'Group', value: (r) => GROUP_SHORT[partOf(model.geos[r.geo])] },
    { id: 'value', header: `Value (${unit.short || unit.long})`, value: (r) => r.value, cell: (r) => fmtValue(r.value, unit, { scale: v.display_scale, ctx: 'table' }), num: true },
    ...(conc.W !== null ? [{ id: 'share', header: 'Share', value: (r: (typeof conc.rows)[number]) => r.share, cell: (r: (typeof conc.rows)[number]) => fmtShare(r.share), num: true }] : []),
    ...(p.change ? [{ id: 'change', header: 'Change', value: (r: (typeof conc.rows)[number]) => p.change?.get(r.geo) ?? null }] : []),
    {
      id: 'state',
      header: 'State',
      value: (r) => {
        const f = rows.find((x) => x.geo === r.geo);
        return f ? stateShort(stateOf(f, model.geos[r.geo], v, null, year ?? undefined), v) : 'observed';
      },
    },
    { id: 'note', header: 'Source note', value: (r) => (model.geos[r.geo]?.re_export_hub ? 're-export hub' : rows.find((x) => x.geo === r.geo)?.status === 'mirror' ? "partners' declarations" : null) },
  ];
  const summary = concentrationSummary(conc, model, v, highlight);
  return (
    <SectorChartCard
      headingLevel={p.headingLevel ?? 3}
      title={p.title}
      vars={[{ var: v.id, item: item?.slug ?? null, year }, ...(p.extraVars ?? [])]}
      subExtra={p.sub}
      tools={
        <>
          {p.tools}
          {p.map && (
            <Segmented label="View" hideLabel value={view} options={[{ value: 'bars', label: 'Bars' }, { value: 'map', label: 'Map' }]} onChange={(x) => setView(x)} />
          )}
          {p.map && view === 'map' && !p.diverging && heavy && (
            <Segmented label="Scale" hideLabel value={scale} options={[{ value: 'linear', label: 'Linear' }, { value: 'log', label: 'Log' }]} onChange={(x) => setScale(x)} />
          )}
        </>
      }
      table={() => <TableView rows={conc.rows} cols={cols} caption={`${v.label} ranking`} search pageSize={25} rowKey={(r) => String(r.geo)} selectedKey={highlight ? String(highlight.idx) : null} onRowClick={(r) => pick(model.geos[r.geo])} />}
      csv={csv}
      legend={
        <div className="space-y-2">
          <GroupLegend groups={['china', 'core', 'partner', 'rest']} highlight={highlight?.name ?? null} euBloc={state.eu === 'bloc'} />
          <StateLegend states={statesPresent(model, v, rows, year ?? undefined)} source={v.source.short} />
        </div>
      }
      notes={
        <>
          {p.notes}
          {footnote && <p>{footnote}</p>}
          {phone && p.map && <p>Bars are easier to read on a small screen.</p>}
        </>
      }
      summary={summary}
    >
      {view === 'map' && map ? (
        <div>
          <WorldMap cells={map.cells} ariaLabel={`Map of ${v.label}${year ? `, ${year}` : ''}`} selected={highlight?.code ?? null} onSelect={(iso) => { const g = model.geoByCode.get(iso); if (g) pick(g); }} />
          {!p.diverging && (
            <div className="mt-2">
              <RampLegend kind="seq" min={map.lo} max={map.hi} label={`${unit.short || unit.long}${year ? `, ${year}` : ''}${scale === 'log' ? ' (log scale)' : ''}`} format={(x) => fmtValue(scale === 'log' ? Math.pow(10, x) : x, unit, { scale: v.display_scale })} />
            </div>
          )}
        </div>
      ) : league.length ? (
        <LeagueBars
          rows={league}
          axisLabel={`${unit.long || unit.short}${year ? `, ${year}` : ''}`}
          selected={highlight?.code ?? null}
          onSelect={(r) => { const g = model.geoByCode.get(r.id); if (g) pick(g); }}
          top={p.top ?? (phone ? 10 : 20)}
          diverging={p.diverging}
          ariaLabel={`${v.label}, ranked${year ? `, ${year}` : ''}`}
          tickFormat={fmtAxis}
        />
      ) : (
        <p className="text-sm text-paper-mute">No values for this selection.</p>
      )}
    </SectorChartCard>
  );
}

/** fmtFact re-export for cards that format a single value. */
export { fmtFact };
