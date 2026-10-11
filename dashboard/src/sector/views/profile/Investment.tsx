// Profile S7 Investment (spec_sector.md 4.3), shown before production for a finance audience:
// (i) annual investment by group 2018-2025 (Value | Share), (ii) the top investors table, (iii) small
// multiples of the top 6; and, from sector-series, power capacity additions 2018-2025 (Solar, Wind and
// Nuclear Downstream) in the same forms.

import { useMemo, useState } from 'react';
import * as Plot from '@observablehq/plot';
import { TableView } from '../../../components/TableView.tsx';
import { Segmented } from '../../../components/ui.tsx';
import { GROUP_COLOR, NEUTRAL_SERIES } from '../../../theme/palette.ts';
import type { FactsIndex } from '../../data/facts.ts';
import type { VarInfo } from '../../data/model.ts';
import { growth } from '../../derive/concentration.ts';
import { GROUP_SHORT, partOf } from '../../derive/groups.ts';
import { groupArea, topLines } from '../../derive/series.ts';
import { sectorCsv } from '../../derive/csv.ts';
import { axisFormatter, fmtShare, fmtSignedPct, fmtValue } from '../../format.ts';
import { ChunkGate, markGroup } from '../common.tsx';
import { useSeries } from '../data.ts';
import { GroupArea, GroupLegend, LabelledLines, SectorChartCard, SmallMultiples } from '../kit.tsx';
import { useCardCsv, useProfile } from './ctx.tsx';

const CELL_NOTES: Record<string, string> = {
  'electric-vehicles-downstream': 'EV Downstream investment is consumer purchases of electric vehicles.',
  'batteries-upstream': 'Batteries and EV Upstream are one critical-minerals series split by a constant: never add them.',
  'electric-vehicles-upstream': 'Batteries and EV Upstream are one critical-minerals series split by a constant: never add them.',
  'geothermal-downstream': 'Geothermal covers enhanced geothermal only.',
  'nuclear-downstream': 'Nuclear covers advanced nuclear only.',
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
  return (
    <div className="space-y-6">
      <SectorChartCard
        title={`${title} by group, ${years[0]}-${last}`}
        vars={[{ var: v.id, item: cell.slug, window: [years[0], last] }]}
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
            {kindNote && <p>{kindNote}</p>}
          </>
        }
        summary={rows[0] ? `${model.geos[rows[0].geo].name} leads ${last} with ${fmt(rows[0].last)} (${fmtShare(rows[0].share)}).` : `No ${title.toLowerCase()} recorded.`}
      >
        <GroupArea series={area} normalize={norm} yLabel={norm ? 'Share of the total' : `${unit.long || unit.short}`} ariaLabel={`${title} by group`} yTickFormat={norm ? (x) => `${Math.round(x * 100)}%` : yFmt} valueFormat={norm ? (x) => fmtShare(x) : fmt} />
      </SectorChartCard>
      <SectorChartCard
        title={`Top ${title.toLowerCase().startsWith('investment') ? 'investors' : 'economies'}`}
        vars={[{ var: v.id, item: cell.slug, window: [years[0], last] }]}
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
            { id: 'c18', header: 'CAGR 2018-25', value: (r) => r.c18, cell: (r) => fmtSignedPct(r.c18), num: true },
            { id: 'c22', header: 'CAGR 2022-25', value: (r) => r.c22, cell: (r) => fmtSignedPct(r.c22), num: true },
            { id: 's', header: `Share ${last}`, value: (r) => r.share, cell: (r) => fmtShare(r.share), num: true },
          ]}
        />
      </SectorChartCard>
      {top6.length > 1 && (
        <SectorChartCard
          title={`The top ${top6.length}, ${years[0]}-${last}`}
          vars={[{ var: v.id, item: cell.slug, window: [years[0], last] }]}
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
            <AnnualBlock idx={series} v={v} title="Power capacity additions" kindNote="Capacity added each year, gigawatts (CIM)." />
            <SectorChartCard
              title="Additions of the largest builders"
              vars={[{ var: v.id, item: cell.slug, window: [yrs[0], yrs[yrs.length - 1]] }]}
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

export function InvestmentSection() {
  const { model, cell, facts } = useProfile();
  const v = model.varById.get('investment.annual');
  const has = !!v && !!facts && facts.yearsOf(v.idx, cell.idx).length > 0;
  return (
    <div className="space-y-6">
      {has && v && facts && <AnnualBlock idx={facts} v={v} title="Annual investment" />}
      <PowerAdditions />
    </div>
  );
}
