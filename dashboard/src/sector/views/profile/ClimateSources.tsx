// The climate sources in the Sector profile's Climate section (user request 2026-10-17: seven files added
// to data/raw): energy investment in the sector's activity, or in the deployment it supplies, by region
// (IEA World Energy Investment 2026, with the match quality stated and 2026 as an IEA estimate), the World
// clean-versus-fossil investment line as context, and, for the coal, oil and gas sectors, World fossil CO2
// from the fuel they supply, handle or burn (Global Carbon Project; context, never the sector's own
// emissions). Manufacturing sectors (Refining, Components, Assembly) say why manufacturing emissions per value added are not shown (the IEA
// database is licence-restricted). Data: the lazy sector-climate-src chunk.

import { useMemo, useState } from 'react';
import { ChartCard } from '../../../charts/ChartCard.tsx';
import { TableView } from '../../../components/TableView.tsx';
import { rowsToCsv } from '../../../lib/csv.ts';
import { LabelledLines } from '../../charts/LabelledLines.tsx';
import type { LineSeries } from '../../charts/types.ts';
import { ACTIVITY_OF_STAGE, type ClimateMatch } from '../../contract.ts';
import type { ItemInfo, SectorModel } from '../../data/model.ts';
import {
  fmtBn, fmtMt, fmtPct, FUEL_WORDS, fuelRows, srcCell, srcLinks, weiContext, weiHeadline, weiPoints, weiRow, weiSummary, type ClimateSrc, type SrcLink,
} from '../../derive/sources.ts';
import { fmtSig } from '../../format.ts';
import { ChunkGate } from '../common.tsx';
import { useClimateSrc } from '../data.ts';

export const WEI_CREDIT = 'IEA (2026), World Energy Investment 2026, CC BY 4.0';
export const GCP_FUEL_CREDIT = 'Global Carbon Project, Global Carbon Budget 2025 (Friedlingstein et al., 2025)';

const MATCH_LABEL: Record<ClimateMatch, string> = { direct: 'Direct match', partial: 'Partial match', proxy: 'Proxy', none: 'No match' };
const ROLE_WORDS: Record<SrcLink['role'], string> = { own: 'this sector', demand: 'deployment it supplies', context: 'context' };

function MatchChip({ match, text }: { match: ClimateMatch; text: string }) {
  const tone = match === 'direct' ? 'border-signal text-paper' : match === 'none' ? 'text-paper-mute' : '';
  return (
    <span className={`tag ${tone}`} title={text} data-testid="wei-match">
      {MATCH_LABEL[match]}
    </span>
  );
}

/** The climate-sources blocks of the Climate section (loads sector-climate-src). */
export function ClimateSources({ model, cell }: { model: SectorModel; cell: ItemInfo }) {
  const { src, h, has } = useClimateSrc();
  if (!has) return null;
  return (
    <ChunkGate h={h} name="sector-climate-src">
      {() =>
        src ? (
          <div className="space-y-6" data-testid="climate-sources">
            <Investment d={src} model={model} cell={cell} />
            <FuelContext d={src} cell={cell} />
            {cell.stageSlug && ACTIVITY_OF_STAGE[cell.stageSlug] === 'manufacturing' && <p className="max-w-3xl text-xs text-paper-mute" data-testid="value-added-note">{model.caveats.get('src_value_added')}</p>}
          </div>
        ) : null
      }
    </ChunkGate>
  );
}

const MAX_REGIONS = 4;

function Investment({ d, model, cell }: { d: ClimateSrc; model: SectorModel; cell: ItemInfo }) {
  const links = srcLinks(d, cell.idx).filter((l) => l.src === 'wei');
  const { match, note } = srcCell(d, cell.idx);
  const [pick, setPick] = useState(0);
  const sel = links[Math.min(pick, Math.max(links.length - 1, 0))];
  const label = model.cellLabel(cell);
  const head = useMemo(() => weiHeadline(d, cell.idx), [d, cell.idx]);
  const ctx = useMemo(() => weiContext(d), [d]);
  const cite = model.sources.get('iea_wei')?.attribution?.replace(/^Source:\s*/, '').replace(/\.$/, '') ?? WEI_CREDIT;
  return (
    <div className="space-y-4" data-testid="wei-block">
      <div className="card p-5">
        <div className="flex flex-wrap items-center gap-2">
          <h3 className="text-base font-medium text-paper">Energy investment for {label}</h3>
          <span className="tag">IEA estimates</span>
          <MatchChip match={match} text={model.caveats.get('src_wei_match') ?? ''} />
        </div>
        {links.length ? (
          <>
            <p className="mt-2 max-w-3xl text-sm text-paper-dim" data-testid="wei-head">{head?.text}</p>
            <p className="mt-1 max-w-3xl text-xs text-paper-mute">{note}</p>
          </>
        ) : (
          <p className="mt-2 max-w-3xl text-sm text-paper-dim">{note}</p>
        )}
        {ctx && <p className="mt-2 max-w-3xl text-xs text-paper-mute" data-testid="wei-context">Context: {ctx}</p>}
      </div>
      {links.length > 1 && (
        <div role="group" aria-label="IEA investment rows" className="flex flex-wrap gap-2">
          {links.map((l, i) => (
            <button key={l.key} type="button" className={`btn btn-sm max-w-full text-left ${l === sel ? 'btn-primary' : 'btn-ghost'}`} style={{ whiteSpace: 'normal' }} aria-pressed={l === sel} onClick={() => setPick(i)}>
              {weiRow(d, l.key)?.name ?? l.key}
            </button>
          ))}
        </div>
      )}
      {sel && <InvestmentChart d={d} model={model} link={sel} cellLabel={label} cite={cite} />}
    </div>
  );
}

function InvestmentChart({ d, model, link, cellLabel, cite }: { d: ClimateSrc; model: SectorModel; link: SrcLink; cellLabel: string; cite: string }) {
  const s = useMemo(() => weiSummary(d, link.key), [d, link.key]);
  if (!s) return null;
  const shown = s.byRegion.slice(0, MAX_REGIONS);
  const lines: LineSeries[] = [
    { id: 'World', label: 'World', group: 'neutral', points: weiPoints(d, link.key, 'World').map((p) => ({ x: p.year, y: p.value, est: p.estimate })) },
    ...shown.map((r) => ({ id: r.region, label: r.label, group: 'neutral' as const, points: weiPoints(d, link.key, r.region).map((p) => ({ x: p.year, y: p.value, est: p.estimate })) })),
  ];
  const regions = d.weiRegions;
  const years = weiPoints(d, link.key, 'World').map((p) => p.year);
  const first = years[0];
  const rows = regions.map((r) => {
    const ps = weiPoints(d, link.key, r.region);
    const v = (y: number | null) => (y === null ? null : (ps.find((p) => p.year === y)?.value ?? null));
    return { r, first: v(first ?? null), last: v(s.year), est: s.est ? v(s.est.year) : null, change: fmtPct(s.year !== null && first !== undefined ? ((v(s.year) ?? NaN) / (v(first) ?? NaN)) - 1 : null) };
  });
  const csv = () => ({
    name: `iea-investment-${s.row.id}`,
    text: rowsToCsv(
      regions.flatMap((r) =>
        weiPoints(d, link.key, r.region).map((p) => ({
          row: s.row.name, wei_row: s.row.label, region: r.label, year: p.year, billion_usd_2025_mer: p.value, kind: p.estimate ? 'IEA estimate' : 'IEA data',
          role_for_sector: `${ROLE_WORDS[link.role]}${link.match ? ` (${link.match})` : ''}`, sector: cellLabel, source: cite,
        })),
      ),
    ),
  });
  return (
    <ChartCard
      headingLevel={3}
      title={`Investment in ${s.row.name.charAt(0).toLowerCase()}${s.row.name.slice(1)}, ${first}–${years[years.length - 1]}`}
      badge={<span className="tag">IEA estimates</span>}
      sub={
        <>
          Billion US dollars (2025, market exchange rates). World and the {shown.length} largest regions in {s.year}; {s.est ? `${s.est.year} is an IEA estimate (open marks).` : ''} {link.role === 'demand' ? 'The deployment this sector supplies.' : ''}
        </>
      }
      csv={csv}
      summary={`${s.row.name}, World: ${fmtBn(s.value)} in ${s.year}${s.est ? `; IEA estimate ${fmtBn(s.est.value)} for ${s.est.year}` : ''}. ${shown.map((r) => `${r.label} ${fmtBn(r.value)}`).join(', ')}.`}
      table={() => (
        <TableView
          rows={rows}
          caption={`${s.row.name}: investment by IEA region, billion USD (2025)`}
          rowKey={(x) => x.r.region}
          cols={[
            { id: 'r', header: 'Region', value: (x) => x.r.label, minWidth: 170 },
            { id: 'f', header: String(first), value: (x) => x.first, cell: (x) => fmtSig(x.first ?? NaN), num: true },
            { id: 'l', header: String(s.year), value: (x) => x.last, cell: (x) => fmtSig(x.last ?? NaN), num: true },
            ...(s.est ? [{ id: 'e', header: `${s.est.year} (estimate)`, value: (x: (typeof rows)[number]) => x.est, cell: (x: (typeof rows)[number]) => fmtSig(x.est ?? NaN), num: true }] : []),
            { id: 'c', header: `Change since ${first}`, value: (x) => x.change },
          ]}
        />
      )}
      notes={
        <div className="space-y-1">
          <p>{model.caveats.get('src_wei')}</p>
          {/* the row's name, not the IEA's own label (the oil and gas memo row is labelled "upstream" there; the CSV keeps the label) */}
          <p>IEA row: {s.row.name}. Source: {cite}.</p>
        </div>
      }
    >
      <LabelledLines lines={lines} highlight="World" yLabel="USD bn" ariaLabel={`${s.row.name}: investment by region, billion USD (2025), IEA`} valueFormat={(v) => fmtBn(v)} yTickFormat={(v) => fmtSig(v)} height={240} />
    </ChartCard>
  );
}

function FuelContext({ d, cell }: { d: ClimateSrc; cell: ItemInfo }) {
  const rows = useMemo(() => fuelRows(d, cell.idx), [d, cell.idx]);
  if (!rows.length) return null;
  // The fuel figures need only the budget citation (the dictionary attribution also cites the consumption-based method).
  const cite = GCP_FUEL_CREDIT;
  const main = rows[0];
  const lines: LineSeries[] = rows.map((r) => ({ id: r.fuel, label: FUEL_WORDS[r.fuel], group: 'neutral', points: r.points.filter(([x]) => x >= 1990).map(([x, y]) => ({ x, y: y * 1000 })) }));
  const head = main.last
    ? `World fossil CO2 from ${FUEL_WORDS[main.fuel].toLowerCase()} was ${fmtMt(main.last.value * 1000)} in ${main.last.year}${main.share !== null ? `, ${(main.share * 100).toFixed(0)}% of all fossil CO2` : ''}${main.since2015 !== null ? ` (${fmtPct(main.since2015)} since 2015)` : ''}.`
    : '';
  const years = [...new Set(rows.flatMap((r) => r.points.map((p) => p[0])))].sort((a, z) => a - z);
  return (
    <div data-testid="gcp-fuel">
      <ChartCard
        headingLevel={3}
        title={`World fossil CO2 from ${rows.map((r) => FUEL_WORDS[r.fuel].toLowerCase()).join(' and ')}, 1990–${main.last?.year ?? ''}`}
        badge={<span className="tag">Observed estimates · World</span>}
        sub={<>{head} Context: every use of the fuel worldwide, not this sector’s own emissions.</>}
        csv={() => ({ name: `gcp-fossil-co2-${rows.map((r) => r.fuel).join('-')}`, text: rowsToCsv(rows.flatMap((r) => r.points.map(([year, gt]) => ({ fuel: FUEL_WORDS[r.fuel], region: 'World', year, gt_co2: gt, source: cite })))) })}
        summary={head}
        table={() => (
          <TableView
            rows={years.filter((y) => y >= 1990).map((y) => ({ y, v: rows.map((r) => r.points.find((p) => p[0] === y)?.[1] ?? null) }))}
            caption="World fossil CO2 by fuel, Gt CO2 (Global Carbon Project)"
            rowKey={(x) => String(x.y)}
            cols={[{ id: 'y', header: 'Year', value: (x) => x.y }, ...rows.map((r, i) => ({ id: r.fuel, header: `${FUEL_WORDS[r.fuel]}, Gt`, value: (x: { y: number; v: (number | null)[] }) => x.v[i], cell: (x: { y: number; v: (number | null)[] }) => fmtSig(x.v[i] ?? NaN), num: true }))]}
            initialSort={{ id: 'y', desc: true }}
          />
        )}
        notes={
          <div className="space-y-1">
            <p>The CO2 from burning the fuel this sector supplies, handles or burns, in every use worldwide (flaring: gas burnt off at oil and gas fields). Converted from tonnes of carbon with the publisher’s factor 3.664.</p>
            <p>Source: {cite}.</p>
          </div>
        }
      >
        <LabelledLines lines={lines} highlight={main.fuel} yLabel="Mt CO2" ariaLabel={`World fossil CO2 from ${rows.map((r) => FUEL_WORDS[r.fuel].toLowerCase()).join(' and ')}, Mt CO2`} valueFormat={(v) => fmtMt(v)} yTickFormat={(v) => fmtSig(v)} height={220} />
      </ChartCard>
    </div>
  );
}
