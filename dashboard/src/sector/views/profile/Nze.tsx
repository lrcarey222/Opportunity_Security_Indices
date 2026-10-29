// The IEA pathways of the Sector profile's Climate lens and section (user request 2026-10-17: IEA Net Zero
// data for the Climate section): for the WEO series config/iea_pathways_map.yml maps to the cell (its own
// activity, the demand it supplies, or emissions context), the World pathway from the IEA's historical
// estimates through 2035, 2040 and 2050 under Stated Policies, Current Policies and Net Zero Emissions by
// 2050, with the match quality stated; for the mineral cells (Extraction and Refining), the IEA's mineral demand by scenario
// (Current Policies, Stated Policies, High Demand: the mineral dataset has no Net Zero scenario). Every
// figure is labelled as an IEA scenario projection, World only. Data: the optional climate.nze_* tables of
// the lazy sector-climate chunk. Beside each series, the 2030 milestone of the IEA's 2021 Net Zero roadmap
// where a 2021 row is paired with it (the lazy sector-climate-src chunk; an older edition, labelled).

import { useMemo, useState } from 'react';
import { Link } from 'react-router';
import * as Plot from '@observablehq/plot';
import { ChartCard } from '../../../charts/ChartCard.tsx';
import { Legend } from '../../../charts/legends.tsx';
import { TableView } from '../../../components/TableView.tsx';
import { rowsToCsv } from '../../../lib/csv.ts';
import { NEUTRAL_SERIES } from '../../../theme/palette.ts';
import { LabelledLines } from '../../charts/LabelledLines.tsx';
import { SmallMultiples } from '../../charts/SmallMultiples.tsx';
import type { LineSeries, SmallMultiplePanel } from '../../charts/types.ts';
import type { ClimateMatch } from '../../contract.ts';
import type { ItemInfo, SectorModel } from '../../data/model.ts';
import {
  fmtNze, fmtTimes, mineralAt, mineralPaths, MINERAL_SCENARIO_WORDS, MINERAL_SCENARIOS, NZE_MATCH_WORDS, NZE_YEAR, nzeCell, nzeCompare, nzeHeadline, nzeLinks,
  pathwayLines, SCENARIO_WORDS, techWords, type MineralPath, type NzeData, type NzeLink,
} from '../../derive/nze.ts';
import { fmtUnit, nze21For, NZE21_YEAR, type ClimateSrc, type Nze21Milestone } from '../../derive/sources.ts';
import { fmtSig } from '../../format.ts';
import { ChunkGate } from '../common.tsx';
import { useClimate, useClimateSrc } from '../data.ts';
import { useSectorUrl } from '../state.tsx';

export const ieaWeoCredit = (d: NzeData) => `IEA (2025), ${d.weo} free dataset (World), CC BY-NC-SA 4.0`;
export const IEA_CM_CREDIT = 'IEA (2026), Critical Minerals Dataset, CC BY 4.0';
export const IEA_NZE21_CREDIT = 'IEA (2021), Net Zero by 2050, Annex A, CC BY-NC-SA 3.0 IGO';

/** "IEA scenario projections · World" (or the WEO region): on every IEA pathway figure. */
export function ScenarioTag({ region = 'World' }: { region?: string }) {
  return (
    <span className="tag max-w-full" style={{ whiteSpace: 'normal' }} data-basis="scenario" data-testid="nze-tag">
      IEA scenario projections · {region}
    </span>
  );
}

function NzeMatchChip({ match }: { match: ClimateMatch }) {
  const tone = match === 'direct' ? 'border-signal text-paper' : match === 'none' ? 'text-paper-mute' : '';
  return (
    <span className={`tag ${tone}`} title={NZE_MATCH_WORDS[match].text} data-testid="nze-match">
      {NZE_MATCH_WORDS[match].label}
    </span>
  );
}

const ROLE_WORDS: Record<NzeLink['role'], string> = { own: 'this sector', demand: 'demand it serves', context: 'emissions context' };

/** The Climate lens line (null when the build has no IEA pathways or the cell no own or demand series). */
export function NzeLensLine({ d, cell }: { d: NzeData; cell: ItemInfo }) {
  const h = useMemo(() => nzeHeadline(d, cell.idx), [d, cell.idx]);
  if (!h) return null;
  return (
    <div className="border-t hairline pt-2" data-testid="nze-lens">
      <ScenarioTag />
      <p className="mt-1.5 text-sm leading-snug text-paper">{h.text}</p>
      <p className="mt-1 text-xs text-paper-mute">
        {NZE_MATCH_WORDS[h.link.match ?? 'none'].label}. {ieaWeoCredit(d)}.
      </p>
    </div>
  );
}

/** The IEA pathways block of the Climate section (loads sector-climate). */
export function NzePathways({ model, cell }: { model: SectorModel; cell: ItemInfo }) {
  const { climate, h, has } = useClimate();
  if (!has) return null;
  return (
    <ChunkGate h={h} name="sector-climate">
      {() => (climate?.nze ? <Body d={climate.nze} model={model} cell={cell} /> : <p className="max-w-3xl text-sm text-paper-dim">This sector data build has no IEA pathways (it was exported without them).</p>)}
    </ChunkGate>
  );
}

function Body({ d, model, cell }: { d: NzeData; model: SectorModel; cell: ItemInfo }) {
  const { src } = useClimateSrc();
  const links = nzeLinks(d, cell.idx);
  const { match, note, mineralTechs } = nzeCell(d, cell.idx);
  const [pick, setPick] = useState(0);
  const sel = links[Math.min(pick, Math.max(links.length - 1, 0))];
  const label = model.cellLabel(cell);
  return (
    <div className="space-y-6" data-testid="nze-pathways">
      <div className="card p-5">
        <div className="flex flex-wrap items-center gap-2">
          <h3 className="text-base font-medium text-paper">Where the IEA’s pathways lead for {label}</h3>
          <ScenarioTag />
          <NzeMatchChip match={match} />
        </div>
        <p className="mt-2 max-w-3xl text-sm text-paper-dim">{note}</p>
        {links.length > 0 && (
          <ul className="mt-2 space-y-0.5 text-sm text-paper-dim">
            {links.map((l) => (
              <li key={l.series.id}>
                {l.series.label}: <span className="text-paper">{ROLE_WORDS[l.role]}{l.match ? `, ${NZE_MATCH_WORDS[l.match].label.toLowerCase()}` : ''}</span>
              </li>
            ))}
          </ul>
        )}
        <p className="mt-2 text-xs text-paper-mute">
          {model.caveats.get('iea_nze_scenarios')} {model.caveats.get('iea_nze_world')}
        </p>
      </div>
      {links.length > 1 && (
        <div role="group" aria-label="IEA series" className="flex flex-wrap gap-2">
          {links.map((l, i) => (
            <button key={l.series.id} type="button" className={`btn btn-sm max-w-full text-left ${l === sel ? 'btn-primary' : 'btn-ghost'}`} style={{ whiteSpace: 'normal' }} aria-pressed={l === sel} onClick={() => setPick(i)}>
              {l.series.label}
            </button>
          ))}
        </div>
      )}
      {sel && <PathwayCard d={d} link={sel} cellLabel={label} m21={src ? nze21For(src, sel.series.id) : null} caveat={model.caveats.get('src_nze2021') ?? null} />}
      {links.length > 0 && <CompareTable d={d} links={links} cellLabel={label} src={src} />}
      {mineralTechs.length > 0 && <MineralScenarios d={d} techs={mineralTechs} cellLabel={label} />}
      <p className="text-xs text-paper-mute">
        {model.caveats.get('iea_nze_match')} Source: {ieaWeoCredit(d)}
        {mineralTechs.length ? `; ${IEA_CM_CREDIT}` : ''}.
      </p>
    </div>
  );
}

const SCENARIO_ORDER = ['historical', 'stated', 'current', 'nze'] as const;

/** One series: Historical, then the three scenarios to 2050 (LabelledLines; Net Zero highlighted). */
function PathwayCard({ d, link, cellLabel, m21, caveat }: { d: NzeData; link: NzeLink; cellLabel: string; m21: Nze21Milestone | null; caveat: string | null }) {
  const s = link.series;
  const lines = useMemo(() => pathwayLines(d, s), [d, s]);
  const cmp = useMemo(() => nzeCompare(d, s), [d, s]);
  const ls: LineSeries[] = lines.map((l) => ({ id: l.scenario, label: SCENARIO_WORDS[l.scenario].replace(' (IEA estimate)', '').replace('Net Zero Emissions by 2050', 'Net Zero'), group: 'neutral', points: l.points }));
  const csv = () => ({
    name: `iea-pathway-${s.id}`,
    text: rowsToCsv(
      SCENARIO_ORDER.flatMap((sc) =>
        [...(d.values.get(s.idx)?.get(sc) ?? new Map<number, number>())].sort((a, z) => a[0] - z[0]).map(([year, value]) => ({
          series: s.label, region: 'World', scenario: SCENARIO_WORDS[sc], year, value, unit: s.unit, kind: sc === 'historical' ? 'IEA historical estimate' : 'IEA scenario projection',
          weo_row: `${s.category} / ${s.product} / ${s.flow}`, role_for_sector: `${ROLE_WORDS[link.role]}${link.match ? ` (${link.match})` : ''}`, sector: cellLabel, source: ieaWeoCredit(d),
        })),
      ),
    ),
  });
  const up = cmp.nze !== null && cmp.stated !== null && cmp.nze >= cmp.stated;
  return (
    <ChartCard
      headingLevel={3}
      title={`${s.label}, World, to 2050`}
      badge={<ScenarioTag />}
      sub={
        <>
          {s.unit}. The IEA’s estimates to {d.baseYear}, then its three scenarios for {d.projYears.join(', ')} (the dataset has no years in between; lines join the published years). {link.role === 'context' ? 'Emissions context, not this sector’s own emissions.' : link.role === 'demand' ? 'The demand this sector serves.' : ''}
        </>
      }
      csv={csv}
      summary={`${s.label}, World: ${fmtNze(cmp.base, s.unit)} in ${d.baseYear}; in ${NZE_YEAR} ${fmtNze(cmp.stated, s.unit)} under Stated Policies, ${fmtNze(cmp.current, s.unit)} under Current Policies, ${fmtNze(cmp.nze, s.unit)} on the Net Zero pathway${up && cmp.ratio !== null ? ` (${fmtTimes(cmp.ratio)} Stated Policies)` : ''}.${m21?.value != null ? ` The IEA’s 2021 Net Zero roadmap set ${fmtUnit(m21.value, m21.series.unit)} for ${NZE21_YEAR}.` : ''}`}
      notes={
        <div className="space-y-1">
          <p>Scenario projections are what the IEA’s model finds under each set of assumptions, not forecasts and not observed data.</p>
          <p>WEO row: {s.category} / {s.product} / {s.flow}. Source: {ieaWeoCredit(d)}.</p>
          {m21 && (
            <p>
              {caveat}{m21.series.match === 'partial' && m21.series.note ? ` ${m21.series.note}` : ''} Source: {IEA_NZE21_CREDIT}.
            </p>
          )}
        </div>
      }
    >
      <dl className={`mb-4 grid grid-cols-2 gap-x-6 gap-y-2 text-sm sm:grid-cols-4${m21?.value != null ? " lg:grid-cols-5" : ""}`} data-testid="nze-compare">
        <div className="border-t hairline pt-1">
          <dt className="text-xs text-paper-mute">World, {d.baseYear} (IEA estimate)</dt>
          <dd className="tabular font-medium text-paper">{fmtNze(cmp.base, s.unit)}</dd>
        </div>
        <div className="border-t hairline pt-1">
          <dt className="text-xs text-paper-mute">Stated Policies, {NZE_YEAR}</dt>
          <dd className="tabular font-medium text-paper">{fmtNze(cmp.stated, s.unit)}</dd>
        </div>
        <div className="border-t hairline pt-1">
          <dt className="text-xs text-paper-mute">Current Policies, {NZE_YEAR}</dt>
          <dd className="tabular font-medium text-paper">{fmtNze(cmp.current, s.unit)}</dd>
        </div>
        <div className="border-t hairline pt-1">
          <dt className="text-xs text-paper-mute">Net Zero, {NZE_YEAR}</dt>
          <dd className="tabular font-medium text-paper">
            {fmtNze(cmp.nze, s.unit)}
            {cmp.ratio !== null && <span className="block text-xs font-normal text-paper-mute">{fmtTimes(cmp.ratio)} Stated Policies</span>}
          </dd>
        </div>
        {m21 && m21.value !== null && (
          <div className="border-t hairline pt-1" data-testid="nze21-milestone">
            <dt className="text-xs text-paper-mute">Net Zero, {NZE21_YEAR} (2021 roadmap, older edition)</dt>
            <dd className="tabular font-medium text-paper">
              {fmtUnit(m21.value, m21.series.unit)}
              <span className="block text-xs font-normal text-paper-mute">
                {m21.series.match === 'partial' ? `${m21.series.label}; ` : ''}
                {m21.base !== null ? `${fmtUnit(m21.base, m21.series.unit)} in ${m21.baseYear} in that edition` : ''}
              </span>
            </dd>
          </div>
        )}
      </dl>
      <LabelledLines lines={ls} highlight="nze" yLabel={s.unit} ariaLabel={`${s.label}, World: IEA historical estimates and scenario projections to 2050`} valueFormat={(v) => fmtNze(v, s.unit)} yTickFormat={(v) => fmtSig(v)} height={260} />
    </ChartCard>
  );
}

/** Every mapped series of the cell at a glance: the last estimate and the three scenarios in 2035, Net Zero in 2050. */
function CompareTable({ d, links, cellLabel, src }: { d: NzeData; links: NzeLink[]; cellLabel: string; src: ClimateSrc | null }) {
  const rows = links.map((l) => ({ l, c: nzeCompare(d, l.series), n50: d.values.get(l.series.idx)?.get('nze')?.get(2050) ?? null, m21: src ? nze21For(src, l.series.id) : null }));
  const any21 = rows.some((r) => r.m21?.value != null);
  return (
    <div>
      <h3 className="mb-2 text-base font-medium text-paper">Every IEA series mapped to {cellLabel}</h3>
      <TableView
        rows={rows}
        caption={`IEA World pathways mapped to ${cellLabel}: the ${d.baseYear} estimate and the scenarios in ${NZE_YEAR} and 2050`}
        rowKey={(r) => r.l.series.id}
        cols={[
          { id: 's', header: 'Series (World)', value: (r) => r.l.series.label, minWidth: 170 },
          { id: 'r', header: 'Shown as', value: (r) => ROLE_WORDS[r.l.role], cell: (r) => `${ROLE_WORDS[r.l.role]}${r.l.match ? ` (${NZE_MATCH_WORDS[r.l.match].label.toLowerCase()})` : ''}`, minWidth: 140 },
          { id: 'u', header: 'Unit', value: (r) => r.l.series.unit },
          { id: 'b', header: `${d.baseYear}`, value: (r) => r.c.base, cell: (r) => fmtSig(r.c.base ?? NaN), num: true },
          { id: 'st', header: `Stated ${NZE_YEAR}`, value: (r) => r.c.stated, cell: (r) => fmtSig(r.c.stated ?? NaN), num: true },
          { id: 'cu', header: `Current ${NZE_YEAR}`, value: (r) => r.c.current, cell: (r) => fmtSig(r.c.current ?? NaN), num: true },
          { id: 'nz', header: `Net Zero ${NZE_YEAR}`, value: (r) => r.c.nze, cell: (r) => fmtSig(r.c.nze ?? NaN), num: true },
          { id: 'x', header: 'Net Zero ÷ Stated', value: (r) => r.c.ratio, cell: (r) => (r.c.ratio === null ? '–' : `${fmtSig(r.c.ratio, 2)}×`), num: true },
          { id: 'n5', header: 'Net Zero 2050', value: (r) => r.n50, cell: (r) => fmtSig(r.n50 ?? NaN), num: true },
          ...(any21
            ? [{ id: 'n21', header: `Net Zero ${NZE21_YEAR} (2021 roadmap)`, value: (r: (typeof rows)[number]) => r.m21?.value ?? null, cell: (r: (typeof rows)[number]) => (r.m21 ? `${fmtUnit(r.m21.value, r.m21.series.unit)}${r.m21.series.match === 'partial' ? ' *' : ''}` : '–'), num: true }]
            : []),
        ]}
      />
      <p className="mt-2 text-xs text-paper-mute">
        IEA scenario projections (World); {d.baseYear} is the IEA’s estimate. Ratios are computed here from the published values.
        {any21 ? ` The ${NZE21_YEAR} column is the IEA’s Net Zero by 2050 roadmap of 2021, an older edition, in its own unit (* a narrower series, e.g. unabated coal only). Source: ${IEA_NZE21_CREDIT}.` : ''}
      </p>
    </div>
  );
}

const MINERAL_DASH: Record<string, string | null> = { current: '2 3', stated: null, high_demand: '7 3' };
const MINERAL_YEARS = (d: NzeData, m: MineralPath) => [...new Set([...m.by.values()].flatMap((s) => [...s.keys()]))].filter((y) => y >= d.mineralBase).sort((a, z) => a - z);

interface MRow {
  scenario: string;
  year: number;
  kt: number;
}

/** Mineral demand of the cell's technologies by scenario: small multiples, one panel per mineral. */
function MineralScenarios({ d, techs, cellLabel }: { d: NzeData; techs: string[]; cellLabel: string }) {
  const { link } = useSectorUrl();
  const paths = useMemo(() => mineralPaths(d, techs), [d, techs]);
  if (!paths.length) return null;
  const shown = paths.slice(0, 6);
  const panels: SmallMultiplePanel<MRow>[] = shown.map((m) => {
    const rows: MRow[] = [];
    for (const sc of MINERAL_SCENARIOS) for (const y of MINERAL_YEARS(d, m)) {
      const v = mineralAt(m, sc, y);
      if (v !== null) rows.push({ scenario: sc, year: y, kt: v });
    }
    const st = mineralAt(m, 'stated', NZE_YEAR);
    const hd = mineralAt(m, 'high_demand', NZE_YEAR);
    return { id: m.mineral, title: m.mineral, sub: `${fmtSig(m.base ?? NaN)} kt in ${d.mineralBase}; ${NZE_YEAR}: ${fmtSig(st ?? NaN)} (Stated), ${fmtSig(hd ?? NaN)} (High Demand)`, rows };
  });
  const csv = () => ({
    name: `iea-mineral-scenarios-${cellLabel.toLowerCase().replace(/[^a-z0-9]+/g, '-')}`,
    text: rowsToCsv(paths.flatMap((m) => MINERAL_SCENARIOS.flatMap((sc) => MINERAL_YEARS(d, m).map((y) => ({ mineral: m.mineral, technologies: m.techs.join('; '), scenario: MINERAL_SCENARIO_WORDS[sc], year: y, kt: mineralAt(m, sc, y), kind: y === d.mineralBase ? 'IEA base-year estimate' : 'IEA scenario projection', region: 'World', source: IEA_CM_CREDIT }))))),
  });
  return (
    <ChartCard
      headingLevel={3}
      title={`Mineral demand from ${techWords(techs)}, by IEA scenario`}
      badge={<ScenarioTag />}
      sub={<>Kilotonnes a year, World, summed over {techWords(techs)}; one panel per mineral, each on its own scale. {d.mineralBase} is the IEA’s base year, shared by every scenario.</>}
      csv={csv}
      summary={panels.map((p) => `${p.title}: ${p.sub}`).join('; ')}
      legend={<Legend label="Scenarios" items={MINERAL_SCENARIOS.map((sc) => ({ label: MINERAL_SCENARIO_WORDS[sc], color: NEUTRAL_SERIES, kind: 'line' as const, dash: MINERAL_DASH[sc] }))} />}
      table={() => (
        <TableView
          rows={paths}
          caption={`Mineral demand from ${techWords(techs)} by IEA scenario, kt`}
          rowKey={(m) => m.mineral}
          cols={[
            { id: 'm', header: 'Mineral', value: (m) => m.mineral, cell: (m) => (m.slug ? <Link className="link" to={link(`/sector/minerals/${m.slug}`)}>{m.mineral}</Link> : m.mineral), minWidth: 130 },
            { id: 'b', header: `${d.mineralBase}`, value: (m) => m.base, cell: (m) => fmtSig(m.base ?? NaN), num: true },
            ...MINERAL_SCENARIOS.map((sc) => ({ id: sc, header: `${MINERAL_SCENARIO_WORDS[sc]} ${NZE_YEAR}`, value: (m: MineralPath) => mineralAt(m, sc, NZE_YEAR), cell: (m: MineralPath) => fmtSig(mineralAt(m, sc, NZE_YEAR) ?? NaN), num: true })),
            ...MINERAL_SCENARIOS.map((sc) => ({ id: `${sc}50`, header: `${MINERAL_SCENARIO_WORDS[sc]} 2050`, value: (m: MineralPath) => mineralAt(m, sc, 2050), cell: (m: MineralPath) => fmtSig(mineralAt(m, sc, 2050) ?? NaN), num: true })),
          ]}
        />
      )}
      notes={
        <div className="space-y-1">
          <p>{d.minerals.length ? 'The IEA Critical Minerals Dataset 2026 publishes the Current Policies, Stated Policies and High Demand scenarios and no Net Zero scenario, so mineral demand on the Net Zero pathway is not shown.' : ''}</p>
          <p>Source: {IEA_CM_CREDIT} (sheet 3.1, demand by technology).</p>
        </div>
      }
    >
      <SmallMultiples
        panels={panels}
        shared={false}
        yValue={(r) => r.kt}
        height={150}
        minPanelWidth={240}
        ariaLabel={(p) => `${p.title} demand from ${techWords(techs)} by IEA scenario`}
        tooltip={(r) => `${MINERAL_SCENARIO_WORDS[r.scenario as keyof typeof MINERAL_SCENARIO_WORDS]}, ${r.year}: ${fmtSig(r.kt)} kt`}
        render={(panel, ctx) => ({
          x: { label: null, ticks: Math.max(3, Math.min(6, Math.floor(ctx.width / 70))), tickFormat: (v: number) => String(Math.round(v)) },
          y: { label: 'kt', ticks: 3, grid: true, domain: [0, Math.max(...panel.rows.map((r) => r.kt)) * 1.05 || 1] },
          marks: [
            ...MINERAL_SCENARIOS.map((sc) => Plot.lineY(panel.rows.filter((r) => r.scenario === sc), { x: 'year', y: 'kt', stroke: NEUTRAL_SERIES, strokeWidth: 1.5, ...(MINERAL_DASH[sc] ? { strokeDasharray: MINERAL_DASH[sc]! } : {}) })),
            Plot.dot(panel.rows, Plot.pointer({ x: 'year', y: 'kt', r: 4, fill: 'none', stroke: NEUTRAL_SERIES, strokeWidth: 2, maxRadius: 24 })),
          ],
        })}
      />
      {paths.length > shown.length && <p className="mt-2 text-xs text-paper-mute">The six minerals with the largest Stated Policies demand in {NZE_YEAR}; the table lists all {paths.length}.</p>}
    </ChartCard>
  );
}

