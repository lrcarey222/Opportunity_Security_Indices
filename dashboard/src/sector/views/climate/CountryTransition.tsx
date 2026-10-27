// The Country profile's Policy & climate cards from the climate sources (user request 2026-10-17: seven
// files added to data/raw for the Climate section): the economy's fossil CO2 since 1990 (Global Carbon
// Project: territorial, consumption-based and the net transfer between them), its transition indicators
// against the median economy (Energy Institute Country Transition Tracker), and the IEA's pathway for its
// WEO region (Stated and Current Policies; the mapping stated), with the World pathway as context. Reads
// the Sector data's sector-meta and lazy sector-climate-src (and sector-climate, for the World line)
// chunks through the shared Sector store. GCP and EI values are observed estimates; IEA values are
// scenario projections, labelled as such. Each card leads with one sentence; details sit in disclosures.

import { useMemo } from 'react';
import { ChartCard } from '../../../charts/ChartCard.tsx';
import { TableView } from '../../../components/TableView.tsx';
import { Disclosure, Notice } from '../../../components/ui.tsx';
import { rowsToCsv } from '../../../lib/csv.ts';
import { LabelledLines } from '../../charts/LabelledLines.tsx';
import type { LineSeries } from '../../charts/types.ts';
import type { SectorModel } from '../../data/model.ts';
import { useSectorMeta } from '../../data/SectorDataProvider.tsx';
import { nzeWorldLine, SCENARIO_WORDS } from '../../derive/nze.ts';
import {
  EI_TOTALS, eiRows, fmtEi, fmtMt, fmtPct, gcpCountry, gcpHeadline, regionWords, weoRegionLine, weoRegionLines, weoRegionOf, weoRegionRows,
  type ClimateSrc, type EiRow, type WeoRow,
} from '../../derive/sources.ts';
import { fmtSig } from '../../format.ts';
import { ChunkGate } from '../common.tsx';
import { useClimate, useClimateSrc, useNear } from '../data.ts';
import { ieaWeoCredit, ScenarioTag } from '../profile/Nze.tsx';

const credit = (model: SectorModel, id: string, fallback: string) => model.sources.get(id)?.attribution?.replace(/^Source:\s*/, '').replace(/\.$/, '') ?? fallback;
export const GCP_CREDIT = 'Global Carbon Project, Global Carbon Budget 2025 (Friedlingstein et al., 2025); consumption-based emissions after Peters et al. (2012)';
export const EI_CTT_CREDIT = 'Energy Institute Statistical Review of World Energy 2025, Country Transition Tracker';
export const WEO_REGIONS_CREDIT = 'IEA (2025), World Energy Outlook 2025 free dataset (Regions), CC BY-NC-SA 4.0';

/** The three cards; they mount (and load the Sector data) once near the viewport. `name`: the economy in prose. */
export function CountryTransitionCards({ iso3, name }: { iso3: string; name: string }) {
  const [ref, near] = useNear<HTMLDivElement>();
  return (
    <div ref={ref} className="space-y-4" data-testid="policy-transition">
      {near ? <Body iso3={iso3} name={name} /> : <div className="card min-h-[160px] p-5" />}
    </div>
  );
}

function Body({ iso3, name }: { iso3: string; name: string }) {
  const meta = useSectorMeta();
  const { src, h, has } = useClimateSrc(!!meta.model);
  const model = meta.model;
  if (meta.status === 'error') return <Notice kind="error" title="The sector data could not be loaded" action={<button type="button" className="btn btn-ghost btn-sm" onClick={meta.retry}>Retry</button>}>{meta.error?.message ?? ''}</Notice>;
  if (!model) return <p className="card p-5 text-sm text-paper-mute">Loading the sector data…</p>;
  if (!has) return null;
  const g = model.geoByCode.get(iso3) ?? null;
  // While the chunk loads (or fails), its notice sits in a card like the cards it stands in for.
  return (
    <div className={src ? 'space-y-4' : 'card p-5'}>
    <ChunkGate h={h} name="sector-climate-src">
      {() => {
        if (!src) return null;
        if (!g) return <p className="card p-5 text-sm text-paper-mute">The sector data have no row for {name}.</p>;
        return (
          <>
            <Co2Card d={src} model={model} geo={g.idx} name={name} />
            <TransitionCard d={src} model={model} geo={g.idx} name={name} />
            <RegionCard d={src} model={model} geo={g.idx} name={name} />
          </>
        );
      }}
    </ChunkGate>
    </div>
  );
}

// ---- Fossil CO2 (GCP) ----------------------------------------------------------------------------------

function Co2Card({ d, model, geo, name }: { d: ClimateSrc; model: SectorModel; geo: number; name: string }) {
  const world = model.geoByCode.get('WLD')?.idx ?? null;
  const c = useMemo(() => gcpCountry(d, geo, world), [d, geo, world]);
  const cite = credit(model, 'gcp', GCP_CREDIT);
  if (!c || !c.last) {
    return (
      <div className="card p-5" data-testid="policy-co2">
        <h3 className="rule-neis-sm text-base font-medium text-paper lg:text-lg">Fossil CO2 emissions</h3>
        <p className="mt-2 text-sm text-paper-mute">The Global Carbon Project reports no fossil CO2 emissions for {name}.</p>
      </div>
    );
  }
  const head = gcpHeadline(c, name);
  const lines: LineSeries[] = [
    { id: 'territorial', label: 'Territorial', group: 'neutral', points: c.territorial.map(([x, y]) => ({ x, y })) },
    ...(c.consumption.length ? [{ id: 'consumption', label: 'Consumption-based', group: 'neutral' as const, points: c.consumption.map(([x, y]) => ({ x, y })) }] : []),
  ];
  const years = [...new Set([...c.territorial, ...c.consumption].map((p) => p[0]))].sort((a, z) => a - z);
  const rows = years.map((y) => ({
    year: y,
    territorial: c.territorial.find((p) => p[0] === y)?.[1] ?? null,
    consumption: c.consumption.find((p) => p[0] === y)?.[1] ?? null,
    transfers: c.transfers.find((p) => p[0] === y)?.[1] ?? null,
  }));
  const first = c.territorial[0]?.[0];
  return (
    <div data-testid="policy-co2">
      <ChartCard
        headingLevel={3}
        title={`${name}’s fossil CO2 emissions, ${first}–${c.last.year}`}
        badge={<span className="tag">Observed estimates</span>}
        sub={<span data-testid="policy-co2-head">{head}</span>}
        csv={() => ({
          name: `fossil-co2-${name.toLowerCase().replace(/[^a-z0-9]+/g, '-')}`,
          text: rowsToCsv(rows.map((r) => ({ economy: name, year: r.year, territorial_mt_co2: r.territorial, consumption_based_mt_co2: r.consumption, transfers_mt_co2: r.transfers, source: cite }))),
        })}
        summary={head ?? ''}
        table={() => (
          <TableView
            rows={rows}
            caption={`${name}: fossil CO2 emissions, Mt CO2 (Global Carbon Project)`}
            rowKey={(r) => String(r.year)}
            cols={[
              { id: 'y', header: 'Year', value: (r) => r.year },
              { id: 't', header: 'Territorial, Mt', value: (r) => r.territorial, cell: (r) => fmtSig(r.territorial ?? NaN), num: true },
              { id: 'c', header: 'Consumption-based, Mt', value: (r) => r.consumption, cell: (r) => fmtSig(r.consumption ?? NaN), num: true },
              { id: 'x', header: 'Transfers (territorial − consumption), Mt', value: (r) => r.transfers, cell: (r) => fmtSig(r.transfers ?? NaN), num: true },
            ]}
            initialSort={{ id: 'y', desc: true }}
          />
        )}
        notes={
          <div className="space-y-1">
            <p>{model.caveats.get('src_gcp_basis')}</p>
            <p>Source: {cite}.</p>
          </div>
        }
      >
        <LabelledLines lines={lines} highlight="territorial" yLabel="Mt CO2" ariaLabel={`${name}: territorial and consumption-based fossil CO2 emissions, Mt CO2`} valueFormat={(v) => fmtMt(v)} yTickFormat={(v) => fmtSig(v)} height={220} />
      </ChartCard>
    </div>
  );
}

// ---- Transition indicators (EI) ---------------------------------------------------------------------------

const VS: Record<NonNullable<EiRow['vsMedian']>, string> = { better: 'ahead of the median', worse: 'behind the median', level: 'at the median' };

function TransitionCard({ d, model, geo, name }: { d: ClimateSrc; model: SectorModel; geo: number; name: string }) {
  const rows = useMemo(() => eiRows(d, geo), [d, geo]);
  const cite = credit(model, 'ei_ctt', EI_CTT_CREDIT);
  const pick = (id: string) => rows.find((r) => r.ind.id === id);
  const ren = pick('renewables_share');
  const low = pick('power_low_carbon');
  const ci = pick('carbon_intensity');
  const parts: string[] = [];
  if (ren) parts.push(`renewables were ${fmtEi(ren.value, 'share')} of ${name}’s energy consumption in ${ren.year} (median economy ${fmtEi(ren.median, 'share')})`);
  if (low) parts.push(`low-carbon sources made ${fmtEi(low.value, 'share')} of its electricity (median ${fmtEi(low.median, 'share')})`);
  if (ci) parts.push(`each unit of its energy carried ${fmtEi(ci.value, ci.ind.unit)}${ci.vsMedian ? `, ${VS[ci.vsMedian]}` : ''}`);
  const head = parts.length ? `${parts.join('; ')}.` : null;
  return (
    <div className="card p-5" data-testid="policy-ei">
      <div className="flex flex-wrap items-center gap-2">
        <h3 className="rule-neis-sm text-base font-medium text-paper lg:text-lg">Transition indicators against the median economy</h3>
        <span className="tag">Observed estimates</span>
      </div>
      {rows.length === 0 ? (
        <p className="mt-2 text-sm text-paper-mute">The Energy Institute’s Country Transition Tracker does not list {name}.</p>
      ) : (
        <>
          {head && <p className="mt-2 max-w-3xl text-sm text-paper-dim" data-testid="policy-ei-head">{head.charAt(0).toUpperCase() + head.slice(1)}</p>}
          <Disclosure summary={`Every indicator for ${name} (${rows.length})`}>
            <TableView
              rows={rows}
              caption={`${name}: Energy Institute transition indicators against the median economy`}
              rowKey={(r) => r.ind.id}
              cols={[
                { id: 'i', header: 'Indicator', value: (r) => r.ind.label, minWidth: 180 },
                { id: 'v', header: name, value: (r) => r.value, cell: (r) => <span className="whitespace-nowrap">{fmtEi(r.value, r.ind.unit)} <span className="text-xs text-paper-mute">({r.year})</span></span>, num: true },
                { id: 'm', header: 'Median economy', value: (r) => r.median, cell: (r) => <span className="whitespace-nowrap">{fmtEi(r.median, r.ind.unit)}{r.n ? <span className="text-xs text-paper-mute"> (of {r.n})</span> : null}</span>, num: true },
                { id: 'c', header: 'Against the median', value: (r) => r.vsMedian ?? '', cell: (r) => (r.vsMedian ? VS[r.vsMedian] : EI_TOTALS.has(r.ind.id) ? 'not ranked (a national total)' : 'no direction') },
                { id: 'p', header: `Change since ${rows.find((r) => r.prevYear !== null)?.prevYear ?? 'the previous edition year'}`, value: (r) => r.change, cell: (r) => (r.change === null ? (r.prev !== null && r.ind.unit === 'share' ? `${fmtEi(r.prev, 'share')} in ${r.prevYear}` : '–') : fmtPct(r.change)), num: true },
              ]}
            />
            <p className="mt-2 text-xs text-paper-mute">
              {model.caveats.get('src_ei_median')} The low-carbon share of power adds the wind, solar, hydro, other renewables and nuclear shares the tracker publishes. Total emissions and fossil fuel use are national totals that grow with the size of the economy, so they are not ranked against the median; energy per person has no better direction. Source: {cite}.
            </p>
          </Disclosure>
        </>
      )}
    </div>
  );
}

// ---- The WEO region's pathway (IEA) --------------------------------------------------------------------------

const fmtRow = (r: WeoRow, v: number | null) => (r.unit === 'share' ? fmtEi(v, 'share') : v === null ? '–' : fmtSig(v));

function RegionCard({ d, model, geo, name }: { d: ClimateSrc; model: SectorModel; geo: number; name: string }) {
  const { climate } = useClimate();
  const reg = useMemo(() => weoRegionOf(d, geo, name), [d, geo, name]);
  const rows = useMemo(() => (reg ? weoRegionRows(d, reg.region) : []), [d, reg]);
  const lines = useMemo(() => (reg ? weoRegionLines(d, reg.region, 'co2_energy') : []), [d, reg]);
  const world = climate?.nze ? nzeWorldLine(climate.nze) : null;
  const cite = credit(model, 'iea_weo_regions', WEO_REGIONS_CREDIT);
  const line = reg ? weoRegionLine(d, reg.region) : null;
  const ls: LineSeries[] = lines.map((l) => ({ id: l.scenario, label: SCENARIO_WORDS[l.scenario].replace(' (IEA estimate)', ''), group: 'neutral', points: l.points }));
  return (
    <div className="card p-5" data-testid="policy-weo-region">
      <div className="flex flex-wrap items-center gap-2">
        <h3 className="rule-neis-sm text-base font-medium text-paper lg:text-lg">{reg ? `Where the IEA’s scenarios lead for ${regionWords(d, reg.region)}` : 'Where the IEA’s scenarios lead'}</h3>
        <ScenarioTag region={reg?.region ?? 'World'} />
      </div>
      {reg ? (
        <>
          <p className="mt-2 max-w-3xl text-sm text-paper-dim" data-testid="weo-region-line">{line}</p>
          <p className="mt-1 max-w-3xl text-xs text-paper-mute" data-testid="weo-region-mapping">{reg.mapping} {model.caveats.get('src_weo_regions')}</p>
          <Disclosure summary={`The ${reg.region} pathway in detail`}>
            <LabelledLines lines={ls} highlight="stated" yLabel="Mt CO2" ariaLabel={`${reg.region}: energy-sector CO2, IEA historical estimates and scenario projections to 2050`} valueFormat={(v) => fmtMt(v)} yTickFormat={(v) => fmtSig(v)} height={220} />
            <p className="mb-3 text-xs text-paper-mute">Energy-sector CO2, Mt. The IEA’s estimates for 2010, 2023 and 2024, then its scenarios for 2035 and 2050 (lines join the published years).</p>
            <TableView
              rows={rows}
              caption={`${reg.region}: IEA World Energy Outlook 2025 regional series`}
              rowKey={(r) => r.id}
              cols={[
                { id: 's', header: 'Series', value: (r) => r.label, minWidth: 170 },
                { id: 'u', header: 'Unit', value: (r) => (r.unit === 'share' ? '%' : r.unit) },
                { id: 'b', header: 'Latest estimate', value: (r) => r.base, cell: (r) => <span className="whitespace-nowrap">{fmtRow(r, r.base)} <span className="text-xs text-paper-mute">({r.baseYear})</span></span>, num: true },
                { id: 'st', header: 'Stated 2035', value: (r) => r.stated35, cell: (r) => fmtRow(r, r.stated35), num: true },
                { id: 'cu', header: 'Current 2035', value: (r) => r.current35, cell: (r) => fmtRow(r, r.current35), num: true },
                { id: 's5', header: 'Stated 2050', value: (r) => r.stated50, cell: (r) => fmtRow(r, r.stated50), num: true },
                { id: 'c5', header: 'Current 2050', value: (r) => r.current50, cell: (r) => fmtRow(r, r.current50), num: true },
              ]}
            />
            <p className="mt-2 text-xs text-paper-mute">
              IEA scenario projections for the region; the latest estimate is the IEA’s. Renewables’ share of generation is computed here from the published generation. Source: {cite}.
            </p>
          </Disclosure>
        </>
      ) : (
        <p className="mt-2 max-w-3xl text-sm text-paper-mute">The IEA’s free regional dataset has no region for {name}; the World pathway below is context only.</p>
      )}
      {world && climate?.nze && (
        <div className="mt-2 border-t hairline pt-2" data-testid="nze-world">
          <ScenarioTag />
          <p className="mt-1.5 max-w-3xl text-sm text-paper-dim">{world}</p>
          <p className="mt-1 text-xs text-paper-mute">
            The IEA publishes its Net Zero pathway for the World only, so neither {name} nor its region has one here. Each sector’s World pathway is in its Climate section. Source: {ieaWeoCredit(climate.nze)}.
          </p>
        </div>
      )}
    </div>
  );
}
