// The Climate lens figures and the Climate section's emissions block of the Sector profile (climate data
// change 2026-10-17): Climate TRACE sector emissions by economy for the sources config/climate_trace_map.yml
// maps to the cell, with the match quality stated, the trend, the largest emitters, the emissions
// intensity of the tracked assets where Climate TRACE reports an activity, the materials shown as
// context, and the technology's IPCC lifecycle intensity (always; the only climate figure for a cell
// with no match). Observed facts: no preference setting changes them. Data: the lazy sector-climate chunk.

import { useMemo, useState, type ReactNode } from 'react';
import { ChartCard } from '../../../charts/ChartCard.tsx';
import { TableView } from '../../../components/TableView.tsx';
import { rowsToCsv } from '../../../lib/csv.ts';
import { TECH_LABELS, type ClimateMatch, type SectorPolicyChunk, type TechSlug } from '../../contract.ts';
import type { GeoInfo, ItemInfo, SectorModel } from '../../data/model.ts';
import { LeagueBars } from '../../charts/LeagueBars.tsx';
import type { LeagueRow } from '../../charts/types.ts';
import {
  cellMatch, contextLinks, emitters, fmtIntensity, fmtLifecycle, fmtTonnes, intensityKeys, intensityOf, MATCH_WORDS, ownLinks, worldTrend,
  type ClimateData, type ClimateLink, type ClimateSrc,
} from '../../derive/climate.ts';
import { fmtShare, fmtSignedPct, listText } from '../../format.ts';
import { ChunkGate, EconomyLink } from '../common.tsx';
import { useClimate } from '../data.ts';
import { catRatings } from './Lenses.tsx';

export const CLIMATE_TRACE_CREDIT = 'Climate TRACE (climatetrace.org), CC BY 4.0';

/** The match chip: plain words, the same on the lens and the section. */
export function MatchChip({ match }: { match: ClimateMatch }) {
  const tone = match === 'direct' ? 'border-signal text-paper' : match === 'none' ? 'text-paper-mute' : '';
  return (
    <span className={`tag ${tone}`} title={MATCH_WORDS[match].text} data-testid="climate-match">
      {MATCH_WORDS[match].label}
    </span>
  );
}

/** The technology's IPCC lifecycle line (null when the explorer has no IPCC value or proxy for it). */
export function lifecycleWords(d: ClimateData, techLabel: string): string | null {
  const l = d.lifecycle.get(techLabel);
  if (!l) return null;
  const g = fmtLifecycle(l);
  if (g) return `${techLabel} over a plant’s life: ${g} (IPCC median).`;
  if (l.proxyOf.length) return `The IPCC has no lifecycle row for ${techLabel}; the paper scores it with the values of ${listText(l.proxyOf)}.`;
  return null;
}

export interface LensFigures {
  headline: string;
  metrics: { label: string; value: ReactNode; sub?: ReactNode }[];
  note: string;
}

/** The Climate lens figures of a cell (pure). */
export function climateLensFigures(d: ClimateData, model: SectorModel, cell: ItemInfo, tech: TechSlug): LensFigures {
  const techLabel = TECH_LABELS[tech];
  const { match } = cellMatch(d, cell.idx);
  const own = ownLinks(d, cell.idx);
  const life = d.lifecycle.get(techLabel);
  const lifeMetric = { label: `Lifecycle intensity, ${techLabel}`, value: fmtLifecycle(life) ?? (life?.proxyOf.length ? 'No IPCC row' : 'Not available'), sub: life?.proxyOf.length ? `scored with ${listText(life.proxyOf)}` : life ? 'IPCC median, gCO2e per kWh' : undefined };
  const note = `${CLIMATE_TRACE_CREDIT}; direct emissions, tonnes CO2e (100-year). Climate Action Tracker ratings are in the Climate section.`;
  if (match === 'none' || !own.length) {
    const lw = lifecycleWords(d, techLabel);
    const ctx = contextLinks(d, cell.idx);
    const metrics: LensFigures['metrics'] = [{ label: 'Match with Climate TRACE', value: MATCH_WORDS.none.label }, lifeMetric];
    if (ctx.length) {
      const e = emitters(d, model, ctx[0].src.idx, d.latest);
      metrics.push({ label: `${ctx[0].src.label} (context), ${d.latest}`, value: fmtTonnes(e.world), sub: 'a material, not this sector’s emissions' });
    }
    return { headline: `Climate TRACE has no emissions subsector for ${model.cellLabel(cell)}.${lw ? ` ${lw}` : ''}`, metrics, note };
  }
  const primary = own[0];
  const e = emitters(d, model, primary.src.idx, d.latest);
  const top = e.rows[0] ?? null;
  const change = e.world !== null && e.worldFirst !== null && e.worldFirst > 0 ? e.world / e.worldFirst - 1 : null;
  const what = primary.src.label.charAt(0).toUpperCase() + primary.src.label.slice(1);
  const scale = primary.match === 'proxy' ? ' (a proxy, shown for scale)' : '';
  const headline = `${what}${scale}: ${fmtTonnes(e.world)} CO2e worldwide in ${d.latest}${change !== null ? `, ${fmtSignedPct(change)} since ${d.first}` : ''}.${top ? ` Largest emitter: ${top.g.name} (${fmtShare(top.share)}).` : ''}`;
  const metrics: LensFigures['metrics'] = [
    { label: 'Match with Climate TRACE', value: MATCH_WORDS[match].label, sub: own.length > 1 ? `${own.length} sources` : undefined },
    { label: `World, ${d.latest}`, value: fmtTonnes(e.world), sub: change !== null ? `${fmtSignedPct(change)} since ${d.first}${primary.src.repeated ? '; latest year carried forward' : ''}` : undefined },
  ];
  if (top) metrics.push({ label: 'Largest emitter', value: top.g.name, sub: fmtShare(top.share) });
  metrics.push(lifeMetric);
  return { headline, metrics, note };
}

const geoGroup = (g: GeoInfo): LeagueRow['group'] => (g.group === 'na' ? 'rest' : g.group) as LeagueRow['group'];

/** One own source: the largest emitters (bars and table), the world trend and the intensity of the tracked assets. */
function SourceBlock({ d, model, src, match, chunk, cellLabel }: { d: ClimateData; model: SectorModel; src: ClimateSrc; match: ClimateLink['match']; chunk: SectorPolicyChunk | null; cellLabel: string }) {
  const e = useMemo(() => emitters(d, model, src.idx, d.latest), [d, model, src.idx]);
  const trend = useMemo(() => worldTrend(d, model, src.idx), [d, model, src.idx]);
  const ik = useMemo(() => intensityKeys(d, src), [d, src]);
  const rate = useMemo(() => catRatings(model, chunk), [model, chunk]);
  const groupWord = (g: string) => (g === 'all' ? '' : ` (${g.toLowerCase()})`);
  const league: LeagueRow[] = e.rows.slice(0, 15).map((r) => ({ id: r.g.code, label: r.g.name, group: geoGroup(r.g), value: r.value, display: `${fmtTonnes(r.value)} · ${fmtShare(r.share)}` }));
  const worldInt = ik.map((k) => ({ ...k, w: model.world ? intensityOf(d, k.key, model.world.idx, d.latest) : null }));
  const csv = () => ({
    name: `climate-${src.id.replace(':', '-')}-${d.latest}`,
    text: rowsToCsv(e.rows.map((r) => {
      const row: Record<string, unknown> = { iso3: r.g.code, economy: r.g.name, year: d.latest, t_co2e_100yr: r.value, share_of_world: r.share, [`t_co2e_${d.first}`]: r.first, change_since_first: r.change };
      for (const k of ik) {
        const x = intensityOf(d, k.key, r.g.idx, d.latest);
        row[`intensity_${k.group}`] = x?.value ?? null;
        row[`intensity_unit_${k.group}`] = x?.unit ?? null;
      }
      row.source = `Climate TRACE subsector ${src.subsector}${src.group ? `, asset group ${src.group}` : ''}`;
      row.match_quality = match;
      return row;
    })),
  });
  return (
    <ChartCard
      headingLevel={3}
      title={`${src.label.charAt(0).toUpperCase()}${src.label.slice(1)}: emissions by economy, ${d.latest}`}
      sub={
        <>
          Tonnes of CO2-equivalent (100-year), Climate TRACE’s estimates. {match ? MATCH_WORDS[match].label : ''} for {cellLabel}.{' '}
          {src.basis === 'assets' ? 'Summed over the plants Climate TRACE tracks, grouped by the first fuel each lists.' : 'Country totals: tracked assets plus Climate TRACE’s estimate for the rest.'}
        </>
      }
      csv={csv}
      summary={`${src.label}: world ${fmtTonnes(e.world)} in ${d.latest}; largest ${e.rows.slice(0, 3).map((r) => `${r.g.name} ${fmtShare(r.share)}`).join(', ')}.`}
      notes={
        <div className="space-y-1">
          {src.repeated && <p>{model.caveats.get('climate_trace_repeat') ?? 'The latest year repeats the year before.'}</p>}
          {src.basis === 'assets' && <p>{model.caveats.get('climate_trace_assets')}</p>}
          <p>{model.caveats.get('climate_trace_direct')}</p>
          <p>Source: {CLIMATE_TRACE_CREDIT}.</p>
        </div>
      }
    >
      <dl className="mb-4 grid grid-cols-2 gap-x-6 gap-y-2 text-sm sm:grid-cols-3 lg:grid-cols-6" data-testid="climate-trend">
        {trend.map((t) => (
          <div key={t.year} className="border-t hairline pt-1">
            <dt className="text-xs text-paper-mute">World, {t.year}</dt>
            <dd className="tabular font-medium text-paper">{fmtTonnes(t.value)}</dd>
          </div>
        ))}
        {worldInt.filter((x) => x.w).map((x) => (
          <div key={x.key} className="border-t hairline pt-1">
            <dt className="text-xs text-paper-mute">Intensity{groupWord(x.group)}, world {d.latest}</dt>
            <dd className="tabular font-medium text-paper" title={`${x.w!.n} tracked assets with an activity`}>
              {fmtIntensity(x.w)}
            </dd>
          </div>
        ))}
      </dl>
      {league.length ? <LeagueBars rows={league} axisLabel={`Tonnes CO2e, ${d.latest}`} top={15} ariaLabel={`Largest emitters, ${src.label}, ${d.latest}`} tickFormat={(v) => fmtTonnes(v)} /> : <p className="text-sm text-paper-mute">No economy has a value.</p>}
      <div className="mt-4">
        <TableView
          rows={e.rows}
          caption={`${src.label}: emissions by economy, ${d.latest}`}
          rowKey={(r) => r.g.code}
          search
          pageSize={10}
          cols={[
            { id: 'c', header: 'Economy', value: (r) => r.g.name, cell: (r) => <EconomyLink g={r.g} />, minWidth: 150 },
            { id: 'v', header: `t CO2e, ${d.latest}`, value: (r) => r.value, cell: (r) => fmtTonnes(r.value), num: true },
            { id: 's', header: 'Share of world', value: (r) => r.share, cell: (r) => fmtShare(r.share), num: true },
            { id: 'g', header: `Change since ${d.first}`, value: (r) => r.change, cell: (r) => fmtSignedPct(r.change), num: true },
            ...ik.map((k) => ({
              id: `i-${k.group}`,
              header: `Intensity${groupWord(k.group)}`,
              value: (r: (typeof e.rows)[number]) => intensityOf(d, k.key, r.g.idx, d.latest)?.value ?? null,
              cell: (r: (typeof e.rows)[number]) => fmtIntensity(intensityOf(d, k.key, r.g.idx, d.latest)),
              num: true,
            })),
            { id: 'r', header: 'Climate Action Tracker', value: (r) => rate(r.g)?.rating ?? 'Not rated', cell: (r) => { const x = rate(r.g); return x ? `${x.rating}${x.viaEu ? ' (EU)' : ''}` : <span className="text-paper-mute">Not rated</span>; } },
          ]}
        />
        {ik.length > 0 && <p className="mt-2 text-xs text-paper-mute">Intensity: the emissions of the assets Climate TRACE tracks over their activity ({ik.map((k) => `${worldInt.find((x) => x.key === k.key)?.w?.unit ?? ''}`).filter(Boolean).join('; ')}), only assets that report an activity. The Climate Action Tracker rating grades a government’s overall climate policy, not this sector.</p>}
      </div>
    </ChartCard>
  );
}

/** The emissions block of the Climate section (loads sector-climate). */
export function ClimateEmissions({ model, cell, tech, chunk }: { model: SectorModel; cell: ItemInfo; tech: TechSlug; chunk: SectorPolicyChunk | null }) {
  const { climate, h, has } = useClimate();
  if (!has) return <p className="max-w-3xl text-sm text-paper-dim">This sector data build has no emissions figures (it was exported without the Climate TRACE data).</p>;
  return (
    <ChunkGate h={h} name="sector-climate">
      {() => (climate ? <Body d={climate} model={model} cell={cell} tech={tech} chunk={chunk} /> : null)}
    </ChunkGate>
  );
}

function Body({ d, model, cell, tech, chunk }: { d: ClimateData; model: SectorModel; cell: ItemInfo; tech: TechSlug; chunk: SectorPolicyChunk | null }) {
  const own = ownLinks(d, cell.idx);
  const [pick, setPick] = useState(0);
  const { match, note } = cellMatch(d, cell.idx);
  const ctx = contextLinks(d, cell.idx);
  const techLabel = TECH_LABELS[tech];
  const life = d.lifecycle.get(techLabel);
  const sel = own[Math.min(pick, Math.max(own.length - 1, 0))];
  return (
    <div className="space-y-6" data-testid="climate-emissions">
      <div className="card p-5">
        <div className="flex flex-wrap items-center gap-2">
          <h3 className="text-base font-medium text-paper">How well Climate TRACE matches {model.cellLabel(cell)}</h3>
          <MatchChip match={match} />
        </div>
        <p className="mt-2 max-w-3xl text-sm text-paper-dim">{note}</p>
        {own.length > 0 && (
          <ul className="mt-2 space-y-0.5 text-sm text-paper-dim">
            {own.map((l) => (
              <li key={l.src.id}>
                {l.src.label}: <span className="text-paper">{l.match ? MATCH_WORDS[l.match].label.toLowerCase() : ''}</span>
              </li>
            ))}
          </ul>
        )}
        <p className="mt-2 text-xs text-paper-mute">{model.caveats.get('climate_trace_match')}</p>
      </div>
      {own.length > 1 && (
        <div role="group" aria-label="Climate TRACE source" className="flex flex-wrap gap-2">
          {own.map((l, i) => (
            <button key={l.src.id} type="button" className={`btn btn-sm max-w-full text-left ${i === own.indexOf(sel) ? 'btn-primary' : 'btn-ghost'}`} style={{ whiteSpace: 'normal' }} aria-pressed={i === own.indexOf(sel)} onClick={() => setPick(i)}>
              {l.src.label}
            </button>
          ))}
        </div>
      )}
      {sel && <SourceBlock d={d} model={model} src={sel.src} match={sel.match} chunk={chunk} cellLabel={model.cellLabel(cell)} />}
      {ctx.length > 0 && (
        <div>
          <h3 className="mb-2 text-base font-medium text-paper">Materials, shown as context</h3>
          <TableView
            rows={ctx.map((l) => ({ l, e: emitters(d, model, l.src.idx, d.latest) }))}
            caption="Materials this sector's products are made of (context)"
            rowKey={(r) => r.l.src.id}
            cols={[
              { id: 'm', header: 'Climate TRACE subsector', value: (r) => r.l.src.label, minWidth: 180 },
              { id: 'w', header: `World, ${d.latest}`, value: (r) => r.e.world, cell: (r) => fmtTonnes(r.e.world), num: true },
              { id: 't', header: 'Largest emitters', value: (r) => r.e.rows[0]?.g.name ?? '', cell: (r) => r.e.rows.slice(0, 3).map((x) => `${x.g.name} ${fmtShare(x.share)}`).join(', ') },
            ]}
          />
          <p className="mt-2 text-xs text-paper-mute">Context: the emissions of making these materials for every use. They are not counted as this sector’s emissions. Source: {CLIMATE_TRACE_CREDIT}.</p>
        </div>
      )}
      <div className="card p-5" data-testid="climate-lifecycle">
        <h3 className="text-base font-medium text-paper">Lifecycle intensity of {techLabel}</h3>
        <p className="mt-2 max-w-3xl text-sm text-paper-dim">
          {fmtLifecycle(life) ? (
            <>
              <span className="tabular font-medium text-paper">{fmtLifecycle(life)}</span> over a plant’s life, the IPCC median{life?.rows ? ` (${life.rows.replace(/;/g, ',')} g)` : ''}.
            </>
          ) : life?.proxyOf.length ? (
            <>The IPCC lifecycle table has no row for {techLabel}; the paper scores its climate profile with the values of {listText(life.proxyOf)}.</>
          ) : (
            <>The IPCC lifecycle table has no row for {techLabel}, and the paper uses none for it.</>
          )}
          {match === 'none' ? ' Shown because Climate TRACE has no subsector for this sector.' : ''}
        </p>
        <p className="mt-2 text-xs text-paper-mute">{model.caveats.get('climate_trace_lifecycle')} Source: {model.sources.get('ipcc_lifecycle')?.citation ?? 'IPCC AR5 WGIII Annex III'}.</p>
      </div>
    </div>
  );
}
