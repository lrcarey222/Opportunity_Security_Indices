// Profile S7 Investment (spec_sector.md 4.3), shown before production for a finance audience:
// (i) annual investment by group 2018-2025 (Value | Share), (ii) the top investors table, (iii) small
// multiples of the top 6; and, from sector-series, power capacity additions 2018-2025 (Solar, Wind and
// Nuclear Deployment) in the same forms. The manufacturing capex Components and Assembly both carry
// says it is the same figure as the other stage's (design C2), and the critical-minerals capex says
// which Refining cells split it. In the Components and Assembly cells CM-FDI covers (Solar, Wind,
// Batteries, Electric Vehicles; the same projects on both stages, design E5), a "Cross-border
// manufacturing projects" subsection follows (sector-fdi, loaded when it mounts): see
// views/fdi/FdiBlock.tsx.

import { useMemo, useState } from 'react';
import * as Plot from '@observablehq/plot';
import { TableView } from '../../../components/TableView.tsx';
import { Segmented } from '../../../components/ui.tsx';
import { GROUP_COLOR, NEUTRAL_SERIES } from '../../../theme/palette.ts';
import type { FactsIndex } from '../../data/facts.ts';
import type { VarInfo } from '../../data/model.ts';
import { growth } from '../../derive/concentration.ts';
import { hasFdi } from '../../derive/fdi.ts';
import { GROUP_SHORT, partOf } from '../../derive/groups.ts';
import { groupArea, topLines } from '../../derive/series.ts';
import { sameFigureText, splitText } from '../../derive/pairs.ts';
import { sectorCsv } from '../../derive/csv.ts';
import { axisFormatter, fmtShare, fmtSignedPct, fmtValue, lc } from '../../format.ts';
import { AREA_READING, ChunkGate, HowToRead, markGroup } from '../common.tsx';
import { useNear, useSeries } from '../data.ts';
import { FdiBlock } from '../fdi/FdiBlock.tsx';
import { useSectorUrl } from '../state.tsx';
import { GroupArea, GroupLegend, LabelledLines, SectorChartCard, SmallMultiples } from '../kit.tsx';
import { useCardCsv, useProfile } from './ctx.tsx';

const CELL_NOTES: Record<string, string> = {
  'electric-vehicles-deployment': 'Electric Vehicles — Deployment investment is consumer purchases of electric vehicles.',
  'geothermal-deployment': 'Geothermal covers enhanced geothermal only.',
  'nuclear-deployment': 'Nuclear covers advanced nuclear only.',
};

interface InvRow {
  geo: number;
  values: Map<number, number>;
  last: number;
  prev: number | null;
  c18: number | null;
  c22: number | null;
  share: number | null;
}

/** Group area + top investors + small multiples of one annual series (facts or series index). */
function AnnualBlock({ idx, v, title, kindNote }: { idx: FactsIndex; v: VarInfo; title: string; kindNote?: string }) {
  const { model, cell, highlight } = useProfile();
  const [norm, setNorm] = useState(false);
  const [indep, setIndep] = useState(false);
  const years = idx.yearsOf(v.idx, cell.idx);
  const unit = model.unitOf(v, cell.idx);
  const fmt = (x: number) => fmtValue(x, unit, { scale: v.display_scale });
  const area = useMemo(() => groupArea(idx, model, v.idx, cell.idx, years), [idx, model, v, cell, years]);
  const last = years[years.length - 1];
  const rows: InvRow[] = useMemo(() => {
    if (last === undefined) return [];
    const W = area.filter((p) => p.x === last).reduce((a, p) => a + p.value, 0);
    return idx
      .rows(v.idx, cell.idx, last)
      .filter((r) => model.geos[r.geo]?.kind === 'country' && Number.isFinite(r.value))
      .map((r) => {
        const values = new Map(idx.seriesOf(v.idx, cell.idx, r.geo).map((p) => [p.year, p.value] as [number, number]));
        const g18 = values.has(2018) ? growth('cagr_2018_2025', values.get(2018) as number, r.value) : null;
        const g22 = values.has(2022) ? growth('cagr_2022_2025', values.get(2022) as number, r.value) : null;
        return { geo: r.geo, values, last: r.value, prev: values.get(last - 1) ?? null, c18: g18?.value ?? null, c22: g22?.value ?? null, share: W > 0 ? r.value / W : null };
      })
      .sort((a, z) => z.last - a.last);
  }, [idx, v, cell, last, area, model]);
  const top6 = rows.slice(0, 6);
  const csv = useCardCsv(() => ({ name: `${cell.slug}-${v.id}`, text: sectorCsv(model, rows.flatMap((r) => [...r.values.entries()].map(([y, val]) => ({ geo: model.geos[r.geo], v, item: cell, year: y, value: val, status: idx.value(v.idx, cell.idx, y, r.geo)?.status ?? 'obs' })))) }), [v.id]);
  const yFmt = axisFormatter(area.map((p) => p.value), unit, v.display_scale);
  const est = area.some((p) => p.est);
  // copied and split figures (design C2): said on each cell, never added across cells
  const same = sameFigureText(model, idx, v, cell);
  const split = splitText(model, idx, v, cell);
  return (
    <div className="space-y-6">
      <SectorChartCard
        title={`${title} by group, ${years[0]}-${last}`}
        vars={[{ var: v.id, item: cell.slug, window: [years[0], last] }]}
        subExtra={<HowToRead>{AREA_READING}</HowToRead>}
        tools={<Segmented label="Show" hideLabel value={norm ? 'share' : 'value'} options={[{ value: 'value', label: 'Value' }, { value: 'share', label: 'Share' }]} onChange={(x) => setNorm(x === 'share')} />}
        table={() => (
          <TableView
            rows={years}
            caption={`${title} by group`}
            cols={[{ id: 'y', header: 'Year', value: (y) => y }, ...(['china', 'core', 'partner', 'rest', 'unattributed'] as const).map((k) => ({ id: k, header: GROUP_SHORT[k], value: (y: number) => area.find((p) => p.x === y && p.group === k)?.value ?? null, cell: (y: number) => fmt(area.find((p) => p.x === y && p.group === k)?.value ?? NaN), num: true }))]}
          />
        )}
        csv={csv}
        legend={<GroupLegend estimate={est} />}
        notes={
          <>
            {est && <p>The {last} point is hollow: it includes estimated Q4 construction starts.</p>}
            {CELL_NOTES[cell.slug] && <p>{CELL_NOTES[cell.slug]}</p>}
            {same && <p data-testid="same-figure-note">{same}</p>}
            {split && <p data-testid="split-note">{split}</p>}
            {kindNote && <p>{kindNote}</p>}
          </>
        }
        summary={rows[0] ? `${model.geos[rows[0].geo].name} leads ${last} with ${fmt(rows[0].last)} (${fmtShare(rows[0].share)}).` : `No ${title.toLowerCase()} recorded.`}
      >
        <GroupArea series={area} normalize={norm} yLabel={norm ? 'Share of the total' : `${unit.long || unit.short}`} ariaLabel={`${title} by group`} yTickFormat={norm ? (x) => `${Math.round(x * 100)}%` : yFmt} valueFormat={norm ? (x) => fmtShare(x) : fmt} />
      </SectorChartCard>
      <SectorChartCard
        title={`Top economies: ${lc(title)}`}
        vars={[{ var: v.id, item: cell.slug, window: [years[0], last] }]}
        subExtra={<HowToRead>Each row is one economy, largest in {last} first: the amount in the last two years, how fast it grew a year on average since 2018 and since 2022 (compound growth), and its share of the world total.</HowToRead>}
        csv={csv}
        summary={`${rows.length} economies with ${title.toLowerCase()} in ${last}.`}
      >
        <TableView
          rows={rows}
          caption={`Top economies, ${title.toLowerCase()}`}
          search
          pageSize={15}
          rowKey={(r) => model.geos[r.geo].code}
          selectedKey={highlight?.code ?? null}
          cols={[
            { id: 'c', header: 'Economy', value: (r) => model.geos[r.geo].name, minWidth: 140 },
            { id: 'g', header: 'Group', value: (r) => GROUP_SHORT[partOf(model.geos[r.geo])] },
            { id: 'l', header: `${last} (${unit.short})`, value: (r) => r.last, cell: (r) => fmtValue(r.last, unit, { scale: v.display_scale, ctx: 'table' }), num: true },
            { id: 'p', header: `${last - 1} (${unit.short})`, value: (r) => r.prev, cell: (r) => (r.prev === null ? '–' : fmtValue(r.prev, unit, { scale: v.display_scale, ctx: 'table' })), num: true },
            { id: 'c18', header: 'Growth a year, 2018-25', value: (r) => r.c18, cell: (r) => fmtSignedPct(r.c18), num: true },
            { id: 'c22', header: 'Growth a year, 2022-25', value: (r) => r.c22, cell: (r) => fmtSignedPct(r.c22), num: true },
            { id: 's', header: `Share ${last}`, value: (r) => r.share, cell: (r) => fmtShare(r.share), num: true },
          ]}
        />
      </SectorChartCard>
      {top6.length > 1 && (
        <SectorChartCard
          title={`The top ${top6.length} year by year: ${lc(title)}, ${years[0]}-${last}`}
          vars={[{ var: v.id, item: cell.slug, window: [years[0], last] }]}
          subExtra={<HowToRead>One small chart per economy, year by year, in its group colour; the higher the line, the larger the amount. With “Shared scale” the panels can be compared with each other; “Independent scales” shows each economy’s own trend more clearly.</HowToRead>}
          tools={<Segmented label="Scales" hideLabel value={indep ? 'indep' : 'shared'} options={[{ value: 'shared', label: 'Shared scale' }, { value: 'indep', label: 'Independent scales' }]} onChange={(x) => setIndep(x === 'indep')} />}
          table={() => (
            <TableView rows={top6} caption="Top economies by year" cols={[{ id: 'c', header: 'Economy', value: (r) => model.geos[r.geo].name }, ...years.map((y) => ({ id: String(y), header: String(y), value: (r: InvRow) => r.values.get(y) ?? null, cell: (r: InvRow) => fmt(r.values.get(y) ?? NaN), num: true }))]} />
          )}
          csv={csv}
          summary={`Small multiples of the top ${top6.length} economies.`}
        >
          <SmallMultiples
            panels={top6.map((r) => ({ id: model.geos[r.geo].code, title: model.geos[r.geo].name, rows: years.map((y) => ({ x: y, y: r.values.get(y) ?? NaN, group: markGroup(model.geos[r.geo]) })) }))}
            shared={!indep}
            yValue={(d) => d.y}
            height={130}
            ariaLabel={(p) => `${p.title}: ${title.toLowerCase()} by year`}
            tooltip={(d) => `${d.x}: ${fmt(d.y)}`}
            render={(panel, ctx) => ({
              x: { tickFormat: (d: number) => String(d), ticks: 3, label: null },
              y: { domain: ctx.yDomain ?? undefined, ticks: 3, tickFormat: yFmt, label: null, grid: true },
              marks: [
                Plot.line(panel.rows.filter((d) => Number.isFinite(d.y)), { x: 'x', y: 'y', stroke: GROUP_COLOR[panel.rows[0]?.group ?? 'rest'] ?? NEUTRAL_SERIES, strokeWidth: 2 }),
                Plot.dot(panel.rows.filter((d) => Number.isFinite(d.y)), { x: 'x', y: 'y', r: 2.5, fill: GROUP_COLOR[panel.rows[0]?.group ?? 'rest'] ?? NEUTRAL_SERIES, tip: false }),
              ],
            })}
          />
        </SectorChartCard>
      )}
    </div>
  );
}

function PowerAdditions() {
  const { model, cell, highlight } = useProfile();
  const v = model.varById.get('investment.power_additions');
  const { series, h } = useSeries(!!v && model.availOf(v, cell.idx).length > 0);
  if (!v || !model.availOf(v, cell.idx).some((a) => a.chunk === 'series')) return null;
  return (
    <ChunkGate h={h} name="sector-series">
      {() => {
        if (!series || !series.yearsOf(v.idx, cell.idx).length) return <p className="text-sm text-paper-mute">No capacity additions in the series for this sector.</p>;
        const yrs = series.yearsOf(v.idx, cell.idx);
        const lines = topLines(series, model, v.idx, cell.idx, { n: 6, extra: highlight ? [highlight.idx] : [], rest: false });
        const u = model.unitOf(v, cell.idx);
        return (
          <div className="space-y-6">
            <AnnualBlock idx={series} v={v} title="Power capacity additions" kindNote="Capacity added each year, gigawatts (Clean Investment Monitor)." />
            <SectorChartCard
              title="Additions of the largest builders"
              vars={[{ var: v.id, item: cell.slug, window: [yrs[0], yrs[yrs.length - 1]] }]}
              subExtra={<HowToRead>Each line is one of the largest builders: the capacity it added each year, in gigawatts; the higher the line, the more it built that year.</HowToRead>}
              table={() => <TableView rows={yrs} caption="Additions by economy and year" cols={[{ id: 'y', header: 'Year', value: (y) => y }, ...lines.map((l) => ({ id: l.id, header: `${l.label} (${u.short})`, value: (y: number) => l.points.find((p) => p.x === y)?.y ?? null, num: true }))]} />}
              csv={() => ({ name: `${cell.slug}-power-additions-builders`, text: sectorCsv(model, lines.flatMap((l) => l.points.map((p) => ({ geo: l.geo !== null ? model.geos[l.geo] : null, economy: l.geo === null ? l.label : undefined, v, item: cell, year: p.x, value: p.y, status: p.est ? 'est' : 'obs' })))) })}
              summary="Annual capacity additions of the largest builders, with the rest of the world as a neutral line."
            >
              <LabelledLines
                lines={lines}
                highlight={highlight?.code ?? null}
                yLabel={model.unitOf(v, cell.idx).long}
                ariaLabel="Power capacity additions by economy"
                valueFormat={(x) => fmtValue(x, model.unitOf(v, cell.idx), { scale: v.display_scale })}
              />
            </SectorChartCard>
          </div>
        );
      }}
    </ChunkGate>
  );
}

/** The CM-FDI subsection of a covered Components or Assembly cell (mounts, and loads sector-fdi, near the viewport). */
function CrossBorderProjects() {
  const { model, cell, highlight, tech, stage } = useProfile();
  const { set, state, link } = useSectorUrl();
  const [ref, near] = useNear<HTMLDivElement>(600);
  return (
    <div ref={ref} id="investment-projects" className="scroll-mt-[calc(var(--sector-bar-h,4.75rem)+4rem)] space-y-4 border-t hairline pt-6 xl:scroll-mt-[calc(var(--sector-bar-h,4.75rem)+1.5rem)]">
      <h3 className="text-lg font-medium text-paper">Cross-border manufacturing projects (CM-FDI)</h3>
      {near ? (
        <FdiBlock
          model={model}
          item={cell}
          highlight={highlight}
          onPick={(g) => set({ c: g && g.code !== state.c ? g.code : null })}
          geoHref={(g) => (g.kind === 'country' || g.kind === 'eu' ? link(`/sector/${tech}/${stage}/${g.code}`) : null)}
        />
      ) : (
        <div style={{ minHeight: 240 }} aria-hidden="true" />
      )}
    </div>
  );
}

export function InvestmentSection() {
  const { model, cell, facts } = useProfile();
  const v = model.varById.get('investment.annual');
  const has = !!v && !!facts && facts.yearsOf(v.idx, cell.idx).length > 0;
  return (
    <div className="space-y-6">
      {has && v && facts && <AnnualBlock idx={facts} v={v} title="Annual investment" />}
      <PowerAdditions />
      {hasFdi(model, cell) && <CrossBorderProjects key={cell.slug} />}
    </div>
  );
}
