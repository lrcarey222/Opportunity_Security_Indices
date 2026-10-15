// "How close is each economy to exporting these goods?" (user edit 2a-i-2; spec_country.md C5.2): the
// Harvard Atlas export feasibility of the cell (trade.feasibility: 1 minus the mean product-space
// distance over the basket's HS4 headings, also called relatedness or density), a 0-1 index. A
// distribution (beeswarm, the highlight ringed) and the top 15. It is a different source and year from
// the Comtrade matrix, so it never shares a chart with trade values.

import { useMemo } from 'react';
import { TableView } from '../../../components/TableView.tsx';
import { Term } from '../../../components/Term.tsx';
import { medianSorted } from '../../../charts/scale.ts';
import { geoMark } from '../../derive/explore.ts';
import { GROUP_SHORT } from '../../derive/groups.ts';
import { isCoalition } from '../../derive/groups.ts';
import { sectorCsv } from '../../derive/csv.ts';
import { fmtFact, HowToRead } from '../common.tsx';
import { Beeswarm, GroupLegend, LeagueBars, SectorChartCard, type BeeswarmPoint, type LeagueRow } from '../kit.tsx';
import { useSectorUrl } from '../state.tsx';
import { useCardCsv, useProfile, vOf } from './ctx.tsx';

export function FeasibilityCard() {
  const { model, cell, facts, highlight } = useProfile();
  const { state, set } = useSectorUrl();
  const v = vOf(model, 'trade.feasibility');
  const year = v && facts ? facts.latestYear(v.idx, cell.idx) : null;
  const rows = useMemo(() => (v && facts && year !== null ? facts.rows(v.idx, cell.idx, year).filter((r) => Number.isFinite(r.value) && model.geos[r.geo]?.kind === 'country') : []), [v, facts, year, cell, model]);
  const csv = useCardCsv(() => (v && rows.length ? { name: `${cell.slug}-export-feasibility-${year ?? ''}`, text: sectorCsv(model, rows.map((r) => ({ geo: model.geos[r.geo], v, item: cell, year, value: r.value, status: r.status }))) } : null), ['trade.feasibility']);
  if (!v || year === null || !rows.length) return null;
  const fmt = (x: number) => fmtFact(model, v, cell, x);
  const sorted = [...rows].sort((a, z) => z.value - a.value || a.geo - z.geo);
  const median = (xs: number[]) => medianSorted([...xs].sort((a, z) => a - z));
  const med = median(rows.map((r) => r.value));
  const coal = rows.filter((r) => isCoalition(model.geos[r.geo])).map((r) => r.value);
  const cmed = coal.length ? median(coal) : NaN;
  const top = 15;
  const league: LeagueRow[] = [];
  sorted.forEach((r, i) => {
    const g = model.geos[r.geo];
    const isH = highlight?.idx === r.geo;
    if (i >= top && !isH) return;
    league.push({ id: g.code, label: g.name, group: geoMark(g), value: r.value, display: fmt(r.value), pinned: i >= top && isH });
  });
  const pts: BeeswarmPoint[] = rows.map((r) => {
    const g = model.geos[r.geo];
    return { id: g.code, label: g.name, group: geoMark(g), value: r.value, display: fmt(r.value), labelled: g.code === 'CHN' || g.idx === highlight?.idx };
  });
  const lead = sorted[0] ? model.geos[sorted[0].geo] : null;
  const h = highlight ? sorted.findIndex((r) => r.geo === highlight.idx) : -1;
  const pick = (code: string) => set({ c: code === state.c ? null : code });
  return (
    <SectorChartCard
      title="How close is each economy to exporting these goods?"
      vars={[{ var: v.id, item: cell.slug, year }]}
      subExtra={
        <HowToRead>
          <Term id="feasibility">Export feasibility</Term>, also called relatedness: the share of the know-how these products need that an economy already shows in what it exports well. 0 means none of the related capabilities are in place, 1 means all of them. Each dot is one economy, coloured by its group; further right is closer to exporting these goods, and the ticks mark the world and coalition medians. The bars beside it list the closest economies. It describes capabilities, not costs, demand or policy, and it is not a forecast.
        </HowToRead>
      }
      table={() => (
        <TableView
          rows={sorted}
          caption="Export feasibility by economy"
          search
          pageSize={25}
          rowKey={(r) => String(r.geo)}
          selectedKey={highlight ? String(highlight.idx) : null}
          cols={[
            { id: 'r', header: 'Rank', value: (r) => sorted.indexOf(r) + 1, num: true },
            { id: 'e', header: 'Economy', value: (r) => model.geos[r.geo].name, minWidth: 150 },
            { id: 'g', header: 'Group', value: (r) => GROUP_SHORT[geoMark(model.geos[r.geo])] },
            { id: 'v', header: 'Export feasibility (0-1)', value: (r) => r.value, cell: (r) => fmt(r.value), num: true },
          ]}
        />
      )}
      csv={csv}
      legend={<GroupLegend groups={['china', 'core', 'partner', 'rest']} highlight={highlight?.name ?? null} />}
      summary={`${lead ? `${lead.name} is closest (${fmt(sorted[0].value)})` : ''}; median of ${rows.length} economies ${fmt(med)}${highlight && h >= 0 ? `; ${highlight.name} ${fmt(sorted[h].value)}, ${h + 1} of ${rows.length}` : ''}.`}
    >
      <div className="grid gap-6 lg:grid-cols-5">
        <div className="lg:col-span-3">
          <Beeswarm
            points={pts}
            medians={[...(Number.isFinite(med) ? [{ label: 'World median', value: med }] : []), ...(Number.isFinite(cmed) ? [{ label: 'Coalition median', value: cmed }] : [])]}
            zeroStrip={false}
            xLabel={`Export feasibility, 0 (none) to 1 (all related capabilities), ${year}`}
            ariaLabel="Distribution of export feasibility"
            selected={highlight?.code ?? null}
            onSelect={(p) => pick(p.id)}
            tickFormat={(x) => x.toFixed(1)}
          />
        </div>
        <div className="lg:col-span-2">
          <LeagueBars rows={league} axisLabel={`Export feasibility, ${year}`} selected={highlight?.code ?? null} onSelect={(r) => pick(r.id)} top={top} ariaLabel="Export feasibility, top economies" tickFormat={(x) => x.toFixed(1)} />
        </div>
      </div>
    </SectorChartCard>
  );
}
