// "Emissions in the mapped sectors" (climate data change 2026-10-17): the Country profile's Policy & climate
// section shows one economy's Climate TRACE emissions in every subsector config/climate_trace_map.yml maps
// to a technology x stage, with its share of the world, its rank, the change since the first year and the
// sectors each subsector maps to with the match quality. Reads the Sector data's sector-meta and lazy
// sector-climate chunks through the shared Sector store (src/sector/data/store.ts); observed facts.

import { useMemo } from 'react';
import { Link } from 'react-router';
import { TableView } from '../../../components/TableView.tsx';
import { Notice } from '../../../components/ui.tsx';
import { useSectorMeta } from '../../data/SectorDataProvider.tsx';
import { countryClimateRows, fmtTonnes, MATCH_WORDS, type CountryClimateRow } from '../../derive/climate.ts';
import { fmtShare, fmtSignedPct } from '../../format.ts';
import { ChunkGate } from '../common.tsx';
import { useClimate, useNear } from '../data.ts';
import { CLIMATE_TRACE_CREDIT } from '../profile/Climate.tsx';

/** The card; mounts (and loads the Sector data) once near the viewport. `name`: the economy in prose. */
export function CountryClimateCard({ iso3, name }: { iso3: string; name: string }) {
  const [ref, near] = useNear<HTMLDivElement>();
  return (
    <div ref={ref} className="card p-5" data-testid="policy-emissions">
      <h3 className="rule-neis-sm text-base font-medium text-paper lg:text-lg">Emissions in the mapped sectors</h3>
      <p className="mt-1 max-w-3xl text-sm text-paper-dim">
        Climate TRACE’s estimates of {name}’s direct emissions in the subsectors the explorer maps to a technology and stage, with how well each matches. Observed facts: no perspective setting changes them.
      </p>
      {near ? <Body iso3={iso3} name={name} /> : <div className="min-h-[120px]" />}
    </div>
  );
}

function Body({ iso3, name }: { iso3: string; name: string }) {
  const meta = useSectorMeta();
  const { climate, h, has } = useClimate(!!meta.model);
  const model = meta.model;
  const g = model?.geoByCode.get(iso3) ?? null;
  const rows = useMemo(() => (climate && model && g ? countryClimateRows(climate, model, g) : []), [climate, model, g]);
  if (meta.status === 'error') return <Notice kind="error" title="The sector data could not be loaded" action={<button type="button" className="btn btn-ghost btn-sm" onClick={meta.retry}>Retry</button>}>{meta.error?.message ?? ''}</Notice>;
  if (!model) return <p className="mt-3 text-sm text-paper-mute">Loading the sector data…</p>;
  if (!has) return <p className="mt-3 text-sm text-paper-mute">This sector data build has no emissions figures.</p>;
  return (
    <ChunkGate h={h} name="sector-climate">
      {() => {
        if (!climate) return null;
        if (!g) return <p className="mt-3 text-sm text-paper-mute">The sector data have no row for {name}.</p>;
        const withValue = rows.filter((r) => r.value !== null);
        return (
          <div className="mt-3 space-y-2">
            {withValue.length === 0 ? (
              <p className="text-sm text-paper-mute">Climate TRACE reports no emissions for {name} in these subsectors in {climate.latest}.</p>
            ) : (
              <TableView
                rows={rows}
                caption={`${name}: Climate TRACE emissions in the mapped sectors, ${climate.latest}`}
                rowKey={(r) => r.src.id}
                cols={[
                  { id: 's', header: 'Climate TRACE subsector', value: (r) => r.src.label, minWidth: 170 },
                  { id: 'v', header: `t CO2e, ${climate.latest}`, value: (r) => r.value, cell: (r) => fmtTonnes(r.value), num: true },
                  { id: 'w', header: 'Share of world', value: (r) => r.share, cell: (r) => fmtShare(r.share), num: true },
                  { id: 'k', header: 'Rank', value: (r) => r.rank, cell: (r) => <span className="whitespace-nowrap">{r.rank === null ? '–' : `${r.rank} of ${r.n}`}</span>, num: true },
                  { id: 'g', header: `Change since ${climate.first}`, value: (r) => r.change, cell: (r) => fmtSignedPct(r.change), num: true },
                  { id: 'c', header: 'Sectors (match)', value: (r) => r.cells.map((c) => c.label).join(', '), cell: (r) => <Cells r={r} />, minWidth: 220 },
                ]}
                initialSort={{ id: 'v', desc: true }}
              />
            )}
            <p className="text-xs text-paper-mute">
              Direct emissions where they happen (the electricity a factory buys is counted in power), tonnes CO2e on a 100-year basis. Power by fuel sums the plants Climate TRACE tracks. A proxy is shown for scale; context is a material the sector’s products are made of, not its emissions. Where the economy has a Strategic Index, its Climate Action Tracker rating is the tile further down this section. Source: {CLIMATE_TRACE_CREDIT}.
            </p>
          </div>
        );
      }}
    </ChunkGate>
  );
}

function Cells({ r }: { r: CountryClimateRow }) {
  const { model } = useSectorMeta();
  return (
    <span className="text-sm">
      {r.cells.map((c, i) => {
        const it = model?.items[c.idx];
        const words = c.role === 'context' ? 'context' : c.match ? MATCH_WORDS[c.match].label.toLowerCase() : '';
        return (
          <span key={c.idx}>
            {i > 0 && ', '}
            {it?.techSlug && it.stageSlug ? <Link className="link" to={`/sector/${it.techSlug}/${it.stageSlug}#climate`}>{c.label}</Link> : c.label}
            <span className="text-xs text-paper-mute"> ({words})</span>
          </span>
        );
      })}
    </span>
  );
}
