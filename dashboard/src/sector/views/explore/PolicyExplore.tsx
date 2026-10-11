// Explore, the policy theme (spec_sector.md 4.6; chunk sector-policy): IEA PAMS records by jurisdiction
// for one technology tag (or the critical-minerals or economy-wide records), with the cross-cutting
// toggle (`xc`); new policies by year (announced or planned years hatched) and by instrument; the
// policy list (titles from sector-policy-text, with attribution). Climate Action Tracker: the overall
// rating and the five sub-ratings as the source states them (never the CAT index); members of the EU
// show the EU's rating with "rating of the EU". Counts measure recorded activity, not stringency.

import { useMemo } from 'react';
import { WorldMap, type MapCell } from '../../../charts/WorldMap.tsx';
import { TableView } from '../../../components/TableView.tsx';
import { Checkbox } from '../../../components/ui.tsx';
import { rowsToCsv } from '../../../lib/csv.ts';
import { SEQ } from '../../../theme/palette.ts';
import { seqColor } from '../../../charts/theme.ts';
import type { SectorPolicyChunk } from '../../contract.ts';
import { useSectorManifest } from '../../data/SectorDataProvider.tsx';
import type { GeoInfo, ItemInfo } from '../../data/model.ts';
import { countsByCountry, policyRecords, scopePolicies, tagBit, type PolicyRecord } from '../../derive/policy.ts';
import { ChunkGate, markGroup } from '../common.tsx';
import { usePolicy } from '../data.ts';
import { GroupLegend, LeagueBars, SectorChartCard, type LeagueRow } from '../kit.tsx';
import { announcedFrom, ByInstrument, ByYear, PolicyList } from '../profile/Policy.tsx';
import { useSectorUrl } from '../state.tsx';
import type { ExploreCtx } from './ctx.ts';

/** CAT's rating words in their order (worst-to-best as CAT states them; the order only sorts, it grades nothing here). */
const CAT_ORDER = ['critically insufficient', 'highly insufficient', 'insufficient', 'almost sufficient', '1.5°c compatible', '1.5°c paris agreement compatible'];
const catRank = (s: string | null): number => {
  const i = CAT_ORDER.indexOf((s ?? '').toLowerCase().trim());
  return i < 0 ? 99 : i;
};

/** Records of a policy-tag item: tagged records, or the economy-wide ones for the cross-cutting item. */
export function recordsOfTag(chunk: SectorPolicyChunk, item: ItemInfo | null, xc: boolean): PolicyRecord[] {
  const recs = policyRecords(chunk);
  if (!item) return xc ? recs.filter((r) => r.cross_cutting) : [];
  if (item.slug === 'cross-cutting') return recs.filter((r) => r.cross_cutting);
  const tags = chunk['policy.tags'];
  let bit = tagBit(tags, item.label);
  if (bit < 0) bit = tagBit(tags, item.slug.replace(/-/g, ' '));
  return scopePolicies(recs, { bit, stage: null, cross: xc });
}

function Records({ c, chunk }: { c: ExploreCtx; chunk: SectorPolicyChunk }) {
  const { model, item, highlight } = c;
  const { state, set } = useSectorUrl();
  const manifest = useSectorManifest();
  const recs = useMemo(() => recordsOfTag(chunk, item, state.xc), [chunk, item, state.xc]);
  const byC = useMemo(() => countsByCountry(recs), [recs]);
  const from = announcedFrom(model, manifest.data?.created);
  const announced = recs.filter((r) => r.year !== null && r.year >= from).length;
  const max = Math.max(1, ...byC.map((x) => x.n));
  const color = seqColor(0, max);
  const cells = useMemo(() => {
    const m = new Map<string, MapCell>();
    const by = new Map(byC.map((x) => [x.geo, x]));
    const roster = model.rosterOf(model.varById.get('policy.records')!);
    for (const g of model.countries) {
      if (!g.iso_n3) continue;
      const x = by.get(g.idx);
      m.set(g.iso_n3, { iso3c: g.code, fill: x ? color(x.n) : roster && !roster.has(g.idx) ? 'hatch' : 'zero', outline: highlight?.idx === g.idx, tip: <div><div className="text-sm font-semibold text-paper">{x ? `${x.n} records` : roster && !roster.has(g.idx) ? 'Not in IEA PAMS' : 'No records in scope'}</div><div className="text-paper-dim">{g.name}</div></div> });
    }
    return m;
  }, [byC, model, color, highlight]);
  const league: LeagueRow[] = byC.slice(0, 20).map((x) => {
    const g = model.geos[x.geo];
    return { id: g.code, label: g.name, group: markGroup(g), value: x.n, display: `${x.n} · in force ${x.inForce}`, badge: g.kind === 'eu' ? "EU's own measures" : undefined };
  });
  if (highlight && !league.some((r) => r.id === highlight.code)) {
    const x = byC.find((r) => r.geo === highlight.idx);
    league.push({ id: highlight.code, label: highlight.name, group: markGroup(highlight), value: x?.n ?? 0, display: x ? `${x.n} · in force ${x.inForce}` : 'no records', pinned: true });
  }
  const pick = (code: string) => {
    const g = model.geoByCode.get(code);
    c.setHighlight(g && g.idx !== highlight?.idx ? (g as GeoInfo) : null);
  };
  const tag = item ? item.label : 'Economy-wide';
  return (
    <div className="space-y-6">
      <div className="flex flex-wrap items-center gap-4">
        {item?.slug !== 'cross-cutting' && <Checkbox label="Include economy-wide (cross-cutting) measures" checked={state.xc} onChange={(b) => set({ xc: b })} />}
        <p className="text-sm text-paper-dim" role="status">
          {recs.length} records tagged {tag}
          {announced ? `; ${announced} announced or planned for ${from} or later` : ''}.
        </p>
      </div>
      <SectorChartCard
        title={`Recorded policies by jurisdiction: ${tag}`}
        vars={[{ var: 'policy.records', item: item?.slug ?? null }]}
        table={() => <TableView rows={byC} caption="Policies by jurisdiction" search cols={[{ id: 'c', header: 'Jurisdiction', value: (x) => model.geos[x.geo]?.name ?? '' }, { id: 'n', header: 'Records', value: (x) => x.n, num: true }, { id: 'f', header: 'In force', value: (x) => x.inForce, num: true }, { id: 'e', header: 'Ended', value: (x) => x.ended, num: true }]} />}
        csv={() => ({ name: `policies-${item?.slug ?? 'cross-cutting'}${state.xc ? '-with-cross-cutting' : ''}`, text: rowsToCsv(byC.map((x) => ({ iso3c: model.geos[x.geo]?.code, jurisdiction: model.geos[x.geo]?.name, var_id: 'policy.records', item: item?.slug ?? null, records: x.n, in_force: x.inForce, ended: x.ended, cross_cutting_included: state.xc, source: 'IEA Policies and Measures', licence: 'attribution' }))) })}
        legend={<GroupLegend groups={['china', 'core', 'partner', 'rest']} highlight={highlight?.name ?? null} />}
        notes={<p>The EU's own measures appear as "European Union" and are not painted onto its members. Counts measure recorded activity, not stringency or spending.</p>}
        summary={byC[0] ? `${model.geos[byC[0].geo]?.name} has the most recorded ${tag} policies: ${byC[0].n}.` : 'No records in scope.'}
      >
        <div className="grid gap-6 lg:grid-cols-5">
          <div className="lg:col-span-3">
            <WorldMap cells={cells} ariaLabel={`Recorded ${tag} policies by country`} selected={highlight?.code ?? null} onSelect={pick} />
          </div>
          <div className="lg:col-span-2">{league.length ? <LeagueBars rows={league} axisLabel="Recorded policies (count)" selected={highlight?.code ?? null} onSelect={(r) => pick(r.id)} top={20} ariaLabel="Recorded policies by jurisdiction" /> : <p className="text-sm text-paper-mute">No records in scope.</p>}</div>
        </div>
      </SectorChartCard>
      <ByYear records={highlight ? recs.filter((r) => r.geo === highlight.idx) : recs} from={from} />
      <ByInstrument records={highlight ? recs.filter((r) => r.geo === highlight.idx) : recs} />
      <PolicyList records={highlight ? recs.filter((r) => r.geo === highlight.idx) : recs} model={model} title={highlight ? `${highlight.name}: policy list` : 'Policy list'} />
    </div>
  );
}

function CatRatings({ c, chunk }: { c: ExploreCtx; chunk: SectorPolicyChunk }) {
  const { model, highlight } = c;
  const t = chunk['policy.cat'];
  const rows = useMemo(() => {
    const out: { g: GeoInfo; overall: string | null; pa: string | null; dt: string | null; fs: string | null; cf: string | null; nz: string | null; updated: string | null }[] = [];
    for (let i = 0; i < t.nrow; i++) {
      const g = model.geos[t.cols.geo[i]];
      if (!g) continue;
      out.push({ g, overall: t.cols.overall[i], pa: t.cols.policies_action[i], dt: t.cols.domestic_target[i], fs: t.cols.fair_share[i], cf: t.cols.climate_finance[i], nz: t.cols.ndc[i], updated: t.cols.updated[i] });
    }
    return out.sort((a, z) => catRank(a.overall) - catRank(z.overall) || a.g.name.localeCompare(z.g.name, 'en'));
  }, [t, model]);
  const ratings = [...new Set(rows.map((r) => r.overall).filter((x): x is string => !!x))].sort((a, z) => catRank(a) - catRank(z));
  const shade = (r: string | null) => (r ? SEQ[Math.min(SEQ.length - 1, Math.round((ratings.indexOf(r) / Math.max(1, ratings.length - 1)) * (SEQ.length - 1)))] : null);
  const eu = rows.find((r) => r.g.kind === 'eu');
  const cells = useMemo(() => {
    const m = new Map<string, MapCell>();
    const by = new Map(rows.map((r) => [r.g.idx, r]));
    for (const g of model.countries) {
      if (!g.iso_n3) continue;
      const own = by.get(g.idx);
      const r = own ?? (g.eu_member ? eu : undefined);
      const fill = r ? shade(r.overall) : null;
      m.set(g.iso_n3, { iso3c: g.code, fill: fill ?? 'hatch', outline: highlight?.idx === g.idx, tip: <div><div className="text-sm font-semibold text-paper">{r?.overall ?? 'Not rated by CAT'}</div><div className="text-paper-dim">{g.name}</div>{!own && r && <div className="text-paper-mute">rating of the EU</div>}</div> });
    }
    return m;
  }, [rows, model, eu, highlight]);
  return (
    <SectorChartCard
      title="Climate Action Tracker ratings"
      vars={[{ var: 'policy.cat' }]}
      table={() => (
        <TableView rows={rows} caption="Climate Action Tracker ratings" search rowKey={(r) => r.g.code} selectedKey={highlight?.code ?? null} cols={[
          { id: 'c', header: 'Economy', value: (r) => r.g.name, minWidth: 140 },
          { id: 'o', header: 'Overall', value: (r) => r.overall },
          { id: 'pa', header: 'Policies and action', value: (r) => r.pa },
          { id: 'dt', header: 'Domestic target', value: (r) => r.dt },
          { id: 'fs', header: 'Fair share', value: (r) => r.fs },
          { id: 'cf', header: 'Climate finance', value: (r) => r.cf },
          { id: 'nz', header: 'Net zero target', value: (r) => r.nz },
          { id: 'u', header: 'Updated', value: (r) => r.updated },
        ]} />
      )}
      csv={() => ({ name: 'cat-ratings', text: rowsToCsv(rows.map((r) => ({ iso3c: r.g.code, economy: r.g.name, var_id: 'policy.cat', overall: r.overall, policies_action: r.pa, domestic_target: r.dt, fair_share: r.fs, climate_finance: r.cf, net_zero_target: r.nz, updated: r.updated, source: 'Climate Action Tracker', licence: 'attribution' }))) })}
      legend={
        <ul className="flex flex-wrap gap-x-4 gap-y-1 text-xs text-paper-dim" aria-label="Ratings">
          {ratings.map((r) => (
            <li key={r} className="inline-flex items-center gap-1.5">
              <span className="inline-block h-2.5 w-3.5" style={{ background: shade(r) ?? undefined }} aria-hidden="true" />
              {r} ({rows.filter((x) => x.overall === r).length})
            </li>
          ))}
        </ul>
      }
      notes={<p>CAT's own ratings, shown as published (the last column is CAT's "Net zero target" rating). EU members without their own rating show the EU's.</p>}
      summary={`${rows.length} economies rated by the Climate Action Tracker.`}
    >
      <WorldMap cells={cells} ariaLabel="Climate Action Tracker overall rating by country" selected={highlight?.code ?? null} onSelect={(code) => { const g = model.geoByCode.get(code); c.setHighlight(g && g.idx !== highlight?.idx ? g : null); }} />
    </SectorChartCard>
  );
}

export function PolicyExplore({ c }: { c: ExploreCtx }) {
  const h = usePolicy(true);
  return <ChunkGate h={h} name="sector-policy">{(chunk) => (c.v.id === 'policy.cat' ? <CatRatings c={c} chunk={chunk} /> : <Records c={c} chunk={chunk} />)}</ChunkGate>;
}
