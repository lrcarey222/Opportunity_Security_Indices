// The CM-FDI cards (Cleantech Manufacturing FDI Dataset; sector-fdi), shared by the profile's
// Investment section, the Explore investment layout and Country in sector. Raw values only: project
// counts, disclosed capital investment in US$ m (an undisclosed value reads "not disclosed", never 0;
// sums say "disclosed projects only"), shares of counts or of disclosed capital. Investors are coloured
// by the parent's headquarters group (China includes Hong Kong and Macao here), destinations by their
// Sector Insights group. Every card names the measure, unit, years, source and basis (caption), and
// carries the CC BY 4.0 attribution; each has a table twin and a CSV.

import { useCallback, useMemo, useState, type ReactNode } from 'react';
import * as Plot from '@observablehq/plot';
import { useNavigate } from 'react-router';
import { PlotFigure, Tip } from '../../../charts/PlotFigure.tsx';
import { SankeyChart, type SankeyFlow } from '../../../charts/Sankey.tsx';
import { WorldMap, type MapCell } from '../../../charts/WorldMap.tsx';
import { Legend, TriLegend, type LegendItem } from '../../../charts/legends.tsx';
import { Term } from '../../../components/Term.tsx';
import { triScale } from '../../../charts/scale.ts';
import { TableView, type Col } from '../../../components/TableView.tsx';
import { Segmented, Select, StatTile } from '../../../components/ui.tsx';
import { useMediaQuery } from '../../../shell/useMediaQuery.ts';
import { GROUP_COLOR, INK, SIGNAL } from '../../../theme/palette.ts';
import { FDI_PROJECT_STATUSES, type FdiBasis } from '../../contract.ts';
import type { GeoInfo, ItemInfo, SectorModel } from '../../data/model.ts';
import {
  basisWords, byGeoCsv, cmfdiSource, disclosedWords, FDI_FROM_OPTIONS, FDI_GROUP_ORDER, FDI_GROUP_SHORT, FDI_GROUP_WORDS, fdiByGeo, fdiGroupSplit, fdiSankey, fdiTotals, fdiYearBars, fdiYears, flowsCsv, fmtUsdM, groupsCsv, hostOf,
  jvWords, NOT_DISCLOSED, OTHER_DESTS, OTHER_HOMES, pairFlows, partialYearNote, projectsCsv, projectsWord, safeHttpUrl, shown, shownText, splitShares, yearsCsv,
  type FdiData, type FdiFrom, type FdiGeoRow, type FdiGroup, type FdiGroupSplit, type FdiProject, type FdiShow,
} from '../../derive/fdi.ts';
import { fmtShare } from '../../format.ts';
import { ExternalLink, HowToRead, markGroup } from '../common.tsx';
import { BlocBar, LeagueBars, SectorChartCard, ShareSplitChart, type LeagueRow, type ShareSplitRow } from '../kit.tsx';
import { useCardCsv } from '../profile/ctx.tsx';

export const FDI_VARS = { n: 'investment.fdi_projects', value: 'investment.fdi_value', share: 'investment.fdi_parent_share' } as const;

/** What every CM-FDI card shares: the dictionary, the projects in scope and the choices. */
export interface FdiScope {
  model: SectorModel;
  data: FdiData;
  /** the Midstream cell of the technology */
  item: ItemInfo;
  /** the projects of the cell and basis (and, on a country page, the economy), in table order */
  projects: FdiProject[];
  basis: FdiBasis;
  show: FdiShow;
  highlight: GeoInfo | null;
  /** toggle the highlighted economy (null clears it) */
  onPick?: (g: GeoInfo | null) => void;
  /** a link to an economy's page in this sector (country in sector), or null */
  geoHref?: (g: GeoInfo) => string | null;
}

/** The announcement years of a set of projects, or the whole dataset's when it is empty. */
function windowOf(s: FdiScope): [number, number] {
  const t = fdiTotals(s.projects);
  if (t.yearMin !== null && t.yearMax !== null) return [t.yearMin, t.yearMax];
  const all = fdiTotals(s.data.projects);
  return [all.yearMin ?? 1996, all.yearMax ?? 2026];
}

/** The caption refs of a card. */
const refs = (s: FdiScope, which: 'n' | 'value' | 'share' | 'show', w: [number, number]) => {
  const id = which === 'show' ? FDI_VARS[s.show] : FDI_VARS[which];
  return [{ var: id, item: s.item.slug, window: w }];
};

/** " · year announced · all 314 projects (every status and activity)" after the caption sub line. */
const basisSub = (s: FdiScope) => ` · by year announced · ${basisWords(s.basis, s.projects.length)}`;

/** The CC BY 4.0 attribution of CM-FDI (dictionary text; its links as links). */
export function FdiAttribution({ model }: { model: SectorModel }) {
  const src = cmfdiSource(model);
  const text = src?.attribution ?? 'Source: Cleantech Manufacturing FDI Dataset (CM-FDI), Net Zero Industrial Policy Lab, Johns Hopkins University; CC BY 4.0.';
  const parts = text.split(/(https?:\/\/[^\s)]+)/g);
  const site = safeHttpUrl(src?.url);
  return (
    <p data-testid="cmfdi-attribution">
      {parts.map((p, i) => {
        const href = /^https?:\/\//.test(p) ? safeHttpUrl(p) : null;
        return href ? <ExternalLink key={i} href={href}>{p.replace(/^https?:\/\//, '').replace(/\/$/, '')}</ExternalLink> : <span key={i}>{p}</span>;
      })}
      {site && !text.includes(site.replace(/\/$/, '')) && (
        <>
          {' '}
          <ExternalLink href={site}>CM-FDI explorer</ExternalLink>
        </>
      )}
    </p>
  );
}

/** The legend of investor colours (the parent's headquarters group). */
export function FdiGroupLegend({ highlight, groups = FDI_GROUP_ORDER, label = 'Parent headquarters' }: { highlight?: string | null; groups?: readonly FdiGroup[]; label?: string }) {
  const items: LegendItem[] = groups.map((g) => ({ label: FDI_GROUP_WORDS[g], color: GROUP_COLOR[g], kind: 'rect' }));
  if (highlight) items.push({ label: `${highlight} (highlighted)`, kind: 'outline', color: SIGNAL });
  return (
    <div className="space-y-1">
      <p className="text-xs text-paper-mute">{label}:</p>
      <Legend items={items} label={label} />
    </div>
  );
}

/** The legend of destination colours (the economy's Sector Insights group). */
function DestLegend({ highlight }: { highlight?: string | null }) {
  const items: LegendItem[] = (['china', 'core', 'partner', 'rest'] as const).map((g) => ({ label: g === 'china' ? 'China' : FDI_GROUP_WORDS[g], color: GROUP_COLOR[g], kind: 'rect' }));
  if (highlight) items.push({ label: `${highlight} (highlighted)`, kind: 'outline', color: SIGNAL });
  return (
    <div className="space-y-1">
      <p className="text-xs text-paper-mute">Destination economy:</p>
      <Legend items={items} label="Destination economy" />
    </div>
  );
}

/** Basis, window and measure switches of a block of CM-FDI cards. */
export function FdiControls({
  basis, setBasis, show, setShow, from, setFrom, counts, firstYear,
}: {
  basis: FdiBasis;
  setBasis: (b: FdiBasis) => void;
  show?: FdiShow;
  setShow?: (s: FdiShow) => void;
  from?: FdiFrom;
  setFrom?: (f: FdiFrom) => void;
  counts: { all: number; index_use: number };
  /** the first announcement year of the technology's projects (the "every year" option) */
  firstYear?: number | null;
}) {
  return (
    <div className="flex flex-wrap items-end gap-x-5 gap-y-3" role="group" aria-label="Which cross-border projects">
      <Segmented<FdiBasis>
        label="Projects"
        value={basis}
        options={[
          { value: 'all', label: `All (${counts.all.toLocaleString('en-US')})`, title: 'Every project in CM-FDI for this technology: every status and activity' },
          { value: 'index_use', label: `Used by the indices (${counts.index_use.toLocaleString('en-US')})`, title: 'Manufacturing projects that are announced, under construction, completed or operational, and not marked by CM-FDI as having ceased operations or paused: the ones the Partnership indices use' },
        ]}
        onChange={setBasis}
      />
      {setFrom && (
        <Select
          label="Announced"
          value={from === null || from === undefined ? 'all' : String(from)}
          options={FDI_FROM_OPTIONS.map((y) => ({ value: y === null ? 'all' : String(y), label: y === null ? `Every year${firstYear ? ` (${firstYear} on)` : ''}` : `${y} onward` }))}
          onChange={(x) => setFrom(x === 'all' ? null : (Number(x) as FdiFrom))}
          className="min-w-[9rem]"
        />
      )}
      {show && setShow && (
        <Segmented<FdiShow>
          label="Count"
          value={show}
          options={[
            { value: 'n', label: 'Projects' },
            { value: 'value', label: 'Disclosed capital (US$ m)' },
          ]}
          onChange={setShow}
        />
      )}
    </div>
  );
}

/** Key figures of a set of projects. */
export function FdiTiles({ s }: { s: FdiScope }) {
  const t = fdiTotals(s.projects);
  const split = fdiGroupSplit(s.projects);
  const [y0, y1] = windowOf(s);
  const tiles: { k: string; label: string; value: string; sub: string }[] = [
    { k: 'n', label: 'Projects', value: t.n.toLocaleString('en-US'), sub: `announced ${y0}-${y1}` },
    { k: 'd', label: 'Destinations', value: String(t.dests), sub: 'economies receiving a project' },
    { k: 'h', label: 'Investor economies', value: String(t.homes), sub: `${t.parents.toLocaleString('en-US')} parent companies` },
    { k: 'v', label: 'Disclosed capital', value: fmtUsdM(t.value), sub: `${t.nValue} of ${projectsWord(t.n)} disclose a value` },
    { k: 'c', label: 'Chinese parents', value: fmtShare(split.parts.china.shareN), sub: `${split.parts.china.n} of ${projectsWord(t.n)} (headquartered in China, Hong Kong or Macao)` },
    { k: 'k', label: 'Coalition parents', value: fmtShare(split.parts.core.shareN + split.parts.partner.shareN), sub: `core ${fmtShare(split.parts.core.shareN)} · partners ${fmtShare(split.parts.partner.shareN)}` },
  ];
  return (
    <div className="grid grid-cols-2 gap-3 md:grid-cols-3 xl:grid-cols-6" role="list" aria-label="Key figures of the cross-border projects">
      {tiles.map((x) => (
        <div key={x.k} role="listitem" title={`CM-FDI ${cmfdiSource(s.model)?.edition ?? ''} · ${basisWords(s.basis, t.n)}`}>
          <StatTile label={x.label} value={<span className="font-medium">{x.value}</span>} sub={x.sub} />
        </div>
      ))}
    </div>
  );
}

const usdAxis = (x: number) => (Math.abs(x) >= 1000 ? `${(x / 1000).toLocaleString('en-US', { maximumFractionDigits: 1 })} bn` : `${x.toLocaleString('en-US', { maximumFractionDigits: 0 })} m`);
const countAxis = (x: number) => (Number.isInteger(x) ? x.toLocaleString('en-US') : '');

/** League rows of projects by economy (largest first; the highlight pinned after a gap). */
function geoLeague(model: SectorModel, rows: FdiGeoRow[], show: FdiShow, top: number, highlight: GeoInfo | null, colour: (r: FdiGeoRow) => LeagueRow['group']): LeagueRow[] {
  const out: LeagueRow[] = [];
  rows.forEach((r, i) => {
    const g = model.geos[r.geo];
    const isH = highlight?.idx === r.geo;
    if (!g || (i >= top && !isH)) return;
    const v = shown(r, show);
    out.push({
      id: g.code,
      label: g.name,
      group: colour(r),
      value: v === null ? NaN : v,
      display: show === 'n' ? projectsWord(r.n) : r.value === null ? `${NOT_DISCLOSED} (${projectsWord(r.n)})` : `${fmtUsdM(r.value)} · ${r.nValue} of ${r.n}`,
      pinned: i >= top && isH,
      state: v === null ? 'nq' : 'obs',
    });
  });
  if (highlight && !rows.some((r) => r.geo === highlight.idx)) out.push({ id: highlight.code, label: highlight.name, group: markGroup(highlight), value: NaN, display: 'no project recorded', pinned: true, state: 'not_covered' });
  return out;
}

/** The table twin of projects by economy. */
function GeoTable({ model, rows, side, caption }: { model: SectorModel; rows: FdiGeoRow[]; side: 'dest' | 'home'; caption: string }) {
  return (
    <TableView
      rows={rows}
      caption={caption}
      search
      pageSize={20}
      rowKey={(r) => model.geos[r.geo]?.code ?? String(r.geo)}
      cols={[
        { id: 'e', header: side === 'dest' ? 'Destination' : 'Parent headquarters', value: (r) => model.geos[r.geo]?.name ?? '', minWidth: 140 },
        { id: 'g', header: side === 'dest' ? 'Group' : 'Parent headquarters group', value: (r) => FDI_GROUP_SHORT[side === 'home' && r.homeGroup ? r.homeGroup : markGroup(model.geos[r.geo])] },
        { id: 'n', header: 'Projects', value: (r) => r.n, num: true },
        { id: 'nv', header: 'Disclosing a value', value: (r) => r.nValue, num: true },
        { id: 'v', header: 'Disclosed capital (US$ m)', value: (r) => r.value, cell: (r) => (r.value === null ? NOT_DISCLOSED : fmtUsdM(r.value, 'table')), num: true },
        ...FDI_GROUP_ORDER.map((g) => ({ id: g, header: `Parent headquarters: ${FDI_GROUP_SHORT[g]}`, value: (r: FdiGeoRow) => r.groups[g], num: true })),
      ]}
    />
  );
}

/** Where the projects go: a map of the destinations and their ranking. */
export function DestinationsCard({ s, title = 'Where the projects go' }: { s: FdiScope; title?: string }) {
  const { model, show, highlight } = s;
  const wide = useMediaQuery('(min-width: 1024px)');
  const tablet = useMediaQuery('(min-width: 768px)');
  const [view, setView] = useState<'bars' | 'map'>('bars');
  const rows = useMemo(() => fdiByGeo(s.projects, 'dest', show), [s.projects, show]);
  const w = windowOf(s);
  const map = useMemo(() => {
    // three-colour scale over the destinations with at least one project (white = their median; C1.5)
    const scale = triScale(rows.filter((r) => model.geos[r.geo]?.kind === 'country').map((r) => shown(r, show)).filter((x): x is number => x !== null && x > 0));
    const fmt = (x: number) => (show === 'n' ? String(Math.round(x * 10) / 10) : usdAxis(x));
    const cells = new Map<string, MapCell>();
    for (const r of rows) {
      const g = model.geos[r.geo];
      if (!g?.iso_n3) continue;
      const v = shown(r, show);
      cells.set(g.iso_n3, {
        iso3c: g.code,
        fill: v === null ? 'hatch' : (scale.color(v) ?? 'hatch'),
        outline: highlight?.idx === g.idx,
        tip: (
          <div className="space-y-0.5">
            <div className="text-sm font-semibold text-paper">{shownText(r, show)}</div>
            <div className="text-paper-dim">{g.name}</div>
            {v !== null && scale.n > 1 && <div className="text-paper-mute">{scale.side(v) === 'above' ? 'Above' : scale.side(v) === 'below' ? 'Below' : 'At'} the median of the {scale.n} destinations shown ({fmt(scale.mid)})</div>}
            <div className="text-paper-mute">{show === 'n' ? `disclosed capital: ${disclosedWords(r)}` : `${projectsWord(r.n)}; sums of the projects that disclose a value`}</div>
          </div>
        ),
      });
    }
    if (highlight?.iso_n3 && !cells.has(highlight.iso_n3)) {
      cells.set(highlight.iso_n3, { iso3c: highlight.code, fill: INK[700], outline: true, tip: <div className="text-paper-dim">{highlight.name}: no project recorded</div> });
    }
    return { cells, scale, fmt };
  }, [rows, show, model, highlight]);
  const top = wide ? 15 : 10;
  const league = geoLeague(model, rows, show, top, highlight, (r) => markGroup(model.geos[r.geo]));
  const csv = useCardCsv(() => ({ name: `${s.item.slug}-fdi-destinations-${s.basis}`, text: byGeoCsv(model, rows, 'dest', s.item.slug, s.basis) }), [FDI_VARS.n, FDI_VARS.value]);
  const pick = (code: string) => {
    const g = model.geoByCode.get(code) ?? null;
    s.onPick?.(g && g.idx !== highlight?.idx ? g : null);
  };
  const lead = rows[0] ? model.geos[rows[0].geo] : null;
  const undisclosed = rows.filter((r) => r.value === null).length;
  const showMap = tablet || view === 'map';
  const showBars = tablet || view === 'bars';
  return (
    <SectorChartCard
      title={title}
      vars={refs(s, 'show', w)}
      subExtra={
        <>
          {basisSub(s)}
          <HowToRead>Each bar, or each country on the map, is one destination: {show === 'n' ? 'how many cross-border factory projects it attracted' : 'the capital its cross-border factory projects disclose'}. Bars take the destination’s group colour; on the map orange means fewer than most destinations shown, white the middle one, blue more. Click one to highlight it.</HowToRead>
        </>
      }
      tools={!tablet ? <Segmented label="View" hideLabel value={view} options={[{ value: 'bars', label: 'Bars' }, { value: 'map', label: 'Map' }]} onChange={(x) => setView(x as 'bars' | 'map')} /> : undefined}
      table={() => <GeoTable model={model} rows={rows} side="dest" caption="Projects by destination" />}
      csv={csv}
      legend={
        <div className="space-y-2">
          {showMap && <TriLegend scale={map.scale} label={`Map colour: ${show === 'n' ? 'projects' : 'disclosed capital, US$ m'}`} format={map.fmt} noun="destinations" missing={show === 'value' ? 'Projects disclose no value' : undefined} />}
          <DestLegend highlight={highlight?.name ?? null} />
        </div>
      }
      notes={
        <>
          <p>Countries without colour on the map: no cross-border project recorded.{show === 'value' && undisclosed ? ` Hatched: ${undisclosed} destination${undisclosed === 1 ? '' : 's'} whose projects disclose no value.` : ''}</p>
          {show === 'value' && <p>Sums of disclosed capital only: a project that discloses no value is "not disclosed", never counted as 0.</p>}
          {!tablet && <p>Bars are easier to read on a small screen.</p>}
          <FdiAttribution model={model} />
        </>
      }
      summary={
        lead && rows[0]
          ? show === 'n'
            ? `${lead.name} receives the most: ${shownText(rows[0], show)} of the ${projectsWord(s.projects.length)} in ${model.cellLabel(s.item)}; ${rows.length} destinations in all.`
            : `${lead.name} receives the most disclosed capital: ${shownText(rows[0], show)} of ${disclosedWords(fdiTotals(s.projects))} in ${model.cellLabel(s.item)}; ${rows.length} destinations in all.`
          : 'No projects for this selection.'
      }
    >
      {!rows.length ? (
        <p className="text-sm text-paper-mute">No projects for this selection.</p>
      ) : (
        <div className={tablet ? 'grid gap-6 lg:grid-cols-5' : ''}>
          {showMap && (
            <div className={tablet ? 'lg:col-span-3' : ''}>
              <WorldMap cells={map.cells} ringStyle="double" ariaLabel={`Map of ${show === 'n' ? 'cross-border projects' : 'disclosed capital'} by destination, ${model.cellLabel(s.item)}`} selected={highlight?.code ?? null} onSelect={pick} />
            </div>
          )}
          {showBars && (
            <div className={tablet ? 'lg:col-span-2' : ''}>
              <LeagueBars rows={league} axisLabel={show === 'n' ? `Projects announced ${w[0]}-${w[1]}` : `Disclosed capital, US$ m, ${w[0]}-${w[1]}`} selected={highlight?.code ?? null} onSelect={(r) => pick(r.id)} top={top} ariaLabel="Destinations, ranked" tickFormat={show === 'n' ? countAxis : usdAxis} />
            </div>
          )}
        </div>
      )}
    </SectorChartCard>
  );
}

/** Who invests: investor economies (the parent's headquarters) ranked, coloured by their group. */
export function InvestorsCard({ s, title = 'Who invests: the parents’ headquarters' }: { s: FdiScope; title?: string }) {
  const { model, show, highlight } = s;
  const wide = useMediaQuery('(min-width: 1024px)');
  const rows = useMemo(() => fdiByGeo(s.projects, 'home', show), [s.projects, show]);
  const w = windowOf(s);
  const top = wide ? 15 : 10;
  const league = geoLeague(model, rows, show, top, highlight, (r) => r.homeGroup ?? 'rest');
  const csv = useCardCsv(() => ({ name: `${s.item.slug}-fdi-investors-${s.basis}`, text: byGeoCsv(model, rows, 'home', s.item.slug, s.basis) }), [FDI_VARS.n, FDI_VARS.value]);
  const lead = rows[0] ? model.geos[rows[0].geo] : null;
  return (
    <SectorChartCard
      title={title}
      vars={refs(s, 'show', w)}
      subExtra={
        <>
          {basisSub(s)}
          <HowToRead>Each bar is one economy where parent companies have their headquarters: {show === 'n' ? 'how many factory projects they announced in other economies' : 'the capital those projects disclose'}, largest first, in the investor’s group colour (China here includes Hong Kong and Macao).</HowToRead>
        </>
      }
      table={() => <GeoTable model={model} rows={rows} side="home" caption="Projects by the parent's headquarters" />}
      csv={csv}
      legend={<FdiGroupLegend highlight={highlight?.name ?? null} />}
      notes={<FdiAttribution model={model} />}
      summary={lead && rows[0] ? `Parents headquartered in ${lead.name} lead with ${shownText(rows[0], show)}; ${rows.length} investor economies in all.` : 'No projects for this selection.'}
    >
      {rows.length ? (
        <LeagueBars rows={league} axisLabel={show === 'n' ? `Projects announced ${w[0]}-${w[1]}` : `Disclosed capital, US$ m, ${w[0]}-${w[1]}`} selected={highlight?.code ?? null} onSelect={(r) => { const g = model.geoByCode.get(r.id) ?? null; s.onPick?.(g && g.idx !== highlight?.idx ? g : null); }} top={top} ariaLabel="Investor economies, ranked" tickFormat={show === 'n' ? countAxis : usdAxis} />
      ) : (
        <p className="text-sm text-paper-mute">No projects for this selection.</p>
      )}
    </SectorChartCard>
  );
}

const OTHER_H = '__OTHER_H';
const OTHER_D = '__OTHER_D';

/** Flows from the parents' headquarters to the destinations (Sankey; a ranked list on phones). */
export function FlowsCard({ s, title = 'From headquarters to destination' }: { s: FdiScope; title?: string }) {
  const { model, show } = s;
  const wide = useMediaQuery('(min-width: 640px)');
  const flows = useMemo(() => pairFlows(s.projects), [s.projects]);
  const sank = useMemo(() => fdiSankey(flows, show, { topH: 8, topD: 10 }), [flows, show]);
  const w = windowOf(s);
  const code = (g: number) => (g === OTHER_HOMES ? OTHER_H : g === OTHER_DESTS ? OTHER_D : (model.geos[g]?.code ?? String(g)));
  const homeGroup = useMemo(() => new Map(flows.map((f) => [model.geos[f.home]?.code ?? '', f.homeGroup])), [flows, model]);
  const nameOf = (c: string) => (c === OTHER_H ? 'Other investors' : c === OTHER_D ? 'Other destinations' : (model.geoByCode.get(c)?.name ?? c));
  const sankeyFlows: SankeyFlow[] = sank.links.map((l) => ({ producer: code(l.home), consumer: code(l.dest), value: l.v }));
  // investors (left nodes and the bands) by the parent's headquarters group; destinations (right) by their own group
  const homeColor = (c: string) => (c.startsWith('__') ? GROUP_COLOR.rest : GROUP_COLOR[homeGroup.get(c) ?? 'rest']);
  const destColor = (c: string) => (c.startsWith('__') ? GROUP_COLOR.rest : GROUP_COLOR[markGroup(model.geoByCode.get(c))]);
  const fmt = (v: number) => (show === 'n' ? projectsWord(v) : fmtUsdM(v));
  const csv = useCardCsv(() => ({ name: `${s.item.slug}-fdi-flows-${s.basis}`, text: flowsCsv(model, flows, s.item.slug, s.basis) }), [FDI_VARS.n, FDI_VARS.value]);
  const undisclosedFlows = show === 'value' ? flows.filter((f) => f.value === null).length : 0;
  const tableRows = useMemo(() => [...flows].sort((a, z) => z.n - a.n || (z.value ?? -1) - (a.value ?? -1)), [flows]);
  // phones: the twelve largest single flows (no folding), by the measure shown
  const topFlows = useMemo(() => flows.filter((f) => (shown(f, show) ?? 0) > 0).sort((a, z) => (shown(z, show) ?? 0) - (shown(a, show) ?? 0) || z.n - a.n || a.home - z.home || a.dest - z.dest).slice(0, 12), [flows, show]);
  const top = topFlows[0];
  return (
    <SectorChartCard
      title={title}
      vars={refs(s, 'show', w)}
      subExtra={
        <>
          {basisSub(s)}
          <HowToRead>Each band runs from where the parent company has its headquarters (left) to where the project goes (right); the wider the band, the more {show === 'n' ? 'projects' : 'disclosed capital'}. Bands and the left side take the investor’s group colour, the right side the destination’s; smaller economies are pooled as “Other”.</HowToRead>
        </>
      }
      table={() => (
        <TableView
          rows={tableRows}
          caption="Projects by headquarters and destination"
          search
          pageSize={20}
          cols={[
            { id: 'h', header: 'Parent headquarters', value: (f) => model.geos[f.home]?.name ?? '' },
            { id: 'g', header: 'Parent headquarters group', value: (f) => FDI_GROUP_SHORT[f.homeGroup] },
            { id: 'd', header: 'Destination', value: (f) => model.geos[f.dest]?.name ?? '' },
            { id: 'n', header: 'Projects', value: (f) => f.n, num: true },
            { id: 'nv', header: 'Disclosing a value', value: (f) => f.nValue, num: true },
            { id: 'v', header: 'Disclosed capital (US$ m)', value: (f) => f.value, cell: (f) => (f.value === null ? NOT_DISCLOSED : fmtUsdM(f.value, 'table')), num: true },
            { id: 'y', header: 'Years announced', value: (f) => (f.yearMin === f.yearMax ? String(f.yearMin) : `${f.yearMin}-${f.yearMax}`) },
          ]}
        />
      )}
      csv={csv}
      legend={<FdiGroupLegend label="Colours (bands and left: the parent's headquarters; right: the destination)" />}
      notes={
        <>
          <p>
            {wide ? 'The eight largest investor economies and the ten largest destinations are drawn; the rest fold into "Other investors" and "Other destinations".' : 'The twelve largest flows are listed.'} Every flow is in the table.
            {undisclosedFlows ? ` ${undisclosedFlows} flow${undisclosedFlows === 1 ? '' : 's'} without a disclosed value ${undisclosedFlows === 1 ? 'is' : 'are'} left out of the value view.` : ''}
          </p>
          <FdiAttribution model={model} />
        </>
      }
      summary={top ? `The largest flow is ${model.geos[top.home]?.name ?? ''} to ${model.geos[top.dest]?.name ?? ''}: ${fmt(shown(top, show) ?? 0)} (${fmtShare(sank.total > 0 ? (shown(top, show) ?? 0) / sank.total : NaN)} of the ${show === 'n' ? 'projects' : 'disclosed capital'}).` : 'No flows for this selection.'}
    >
      {!sank.links.length ? (
        <p className="text-sm text-paper-mute">No flows for this selection.</p>
      ) : wide ? (
        <SankeyChart
          flows={sankeyFlows}
          label={nameOf}
          ariaLabel={`Cross-border projects from the parents' headquarters to the destinations, ${model.cellLabel(s.item)}`}
          valueFormat={(v) => `${fmt(v)} · ${fmtShare(sank.total > 0 ? v / sank.total : NaN)}`}
          nodeColor={(c, side) => (side === 'consumer' ? destColor(c) : homeColor(c))}
          linkColor={(f) => `${homeColor(f.producer)}8c`}
          sideLabels={{ left: 'PARENT HEADQUARTERS', right: 'DESTINATION' }}
          tipNote={show === 'n' ? 'of the projects' : 'of the disclosed capital'}
        />
      ) : (
        <ol aria-label="The twelve largest flows from headquarters to destination" className="divide-y divide-white/5 text-sm">
          {topFlows.map((f, i) => (
            <li key={`${f.home}|${f.dest}`} className="flex min-h-11 items-start gap-3 py-2">
              <span className="w-6 shrink-0 text-right text-xs text-paper-mute tabular">{i + 1}</span>
              <span aria-hidden="true" className="mt-1.5 inline-block h-2.5 w-2.5 shrink-0 rounded-full" style={{ background: GROUP_COLOR[f.homeGroup] }} />
              <span className="relative min-w-0 flex-1 text-paper-dim">
                <span className="text-paper">
                  {model.geos[f.home]?.name ?? ''} {'→'} {model.geos[f.dest]?.name ?? ''}
                </span>
                <span className="sr-only"> (parent headquarters group: {FDI_GROUP_WORDS[f.homeGroup]})</span>
                <span className="block text-xs text-paper-mute tabular">
                  {fmt(shown(f, show) ?? 0)} · {fmtShare(sank.total > 0 ? (shown(f, show) ?? 0) / sank.total : NaN)} of the {show === 'n' ? 'projects' : 'disclosed capital'}
                </span>
              </span>
            </li>
          ))}
        </ol>
      )}
    </SectorChartCard>
  );
}

/** The split of projects by parent group: every destination together, then each destination. */
export function ParentGroupsCard({ s, title = 'Whose parents: projects by the parent’s headquarters group', initial = 15 }: { s: FdiScope; title?: string; initial?: number }) {
  const { model, show, highlight } = s;
  const navigate = useNavigate();
  const [all, setAll] = useState(false);
  const w = windowOf(s);
  const rows = useMemo(() => {
    const world = { dest: null as number | null, split: fdiGroupSplit(s.projects) };
    const per = fdiByGeo(s.projects, 'dest', 'n').map((r) => ({ dest: r.geo as number | null, split: fdiGroupSplit(s.projects.filter((p) => p.dest === r.geo)) }));
    return [world, ...per];
  }, [s.projects]);
  const drawable = rows.filter((r) => (show === 'n' ? r.split.n > 0 : r.split.value !== null));
  const hidden = rows.length - drawable.length;
  const hIdx = highlight ? drawable.findIndex((r) => r.dest === highlight.idx) : -1;
  const visible = all ? drawable : drawable.filter((_, i) => i <= initial || i === hIdx);
  const toRow = (r: { dest: number | null; split: FdiGroupSplit }): ShareSplitRow => {
    const g = r.dest === null ? null : model.geos[r.dest];
    const label = g ? g.name : 'All destinations';
    const parts = splitShares(r.split, show);
    const href = g && s.geoHref ? s.geoHref(g) : null;
    return {
      id: g ? g.code : 'WLD',
      label,
      sub: `${projectsWord(r.split.n)}${show === 'value' ? ` · ${r.split.nValue} disclose a value` : ''}`,
      parts,
      cols: { n: String(r.split.n), value: r.split.value === null ? NOT_DISCLOSED : fmtUsdM(r.split.value), china: fmtShare(show === 'n' ? r.split.parts.china.shareN : r.split.parts.china.shareValue) },
      outlined: !!g && highlight?.idx === g.idx,
      href: href ?? undefined,
      ariaLabel: `${label}: ${projectsWord(r.split.n)}; by ${show === 'n' ? 'project count' : 'disclosed capital'}, parents headquartered in ${FDI_GROUP_ORDER.map((k) => `${FDI_GROUP_WORDS[k]} ${(100 * (parts[k] ?? 0)).toFixed(1)} percent`).join(', ')}.`,
    };
  };
  const split = visible.map(toRow);
  const csv = useCardCsv(() => ({ name: `${s.item.slug}-fdi-parent-groups-${s.basis}`, text: groupsCsv(model, rows, s.item.slug, s.basis) }), [FDI_VARS.share]);
  const world = rows[0].split;
  return (
    <SectorChartCard
      title={title}
      // value view: the shares are of the disclosed capital (US$ m), not the dictionary's share of projects
      vars={refs(s, show === 'n' ? 'share' : 'value', w)}
      subExtra={
        <>
          {`${basisSub(s)} · ${rows.length - 1} destination${rows.length === 2 ? '' : 's'} · share of ${show === 'n' ? 'the projects' : 'the disclosed capital'}`}
          <HowToRead>Each bar is one destination (the first, all destinations together), split by where the parent companies have their headquarters: China (with Hong Kong and Macao), the coalition core, coalition partners and the rest of the world. The longer a segment, the larger that group’s share of {show === 'n' ? 'the projects' : 'the disclosed capital'} there.</HowToRead>
        </>
      }
      table={() => (
        <TableView
          rows={rows}
          caption="Projects by the parent's headquarters group"
          search
          pageSize={25}
          rowKey={(r) => (r.dest === null ? 'WLD' : (model.geos[r.dest]?.code ?? String(r.dest)))}
          cols={[
            { id: 'd', header: 'Destination', value: (r) => (r.dest === null ? 'All destinations' : (model.geos[r.dest]?.name ?? '')), minWidth: 140 },
            { id: 'n', header: 'Projects', value: (r) => r.split.n, num: true },
            { id: 'v', header: 'Disclosed capital (US$ m)', value: (r) => r.split.value, cell: (r) => (r.split.value === null ? NOT_DISCLOSED : fmtUsdM(r.split.value, 'table')), num: true },
            ...FDI_GROUP_ORDER.map((g) => ({ id: g, header: `${FDI_GROUP_SHORT[g]} (share of ${show === 'n' ? 'projects' : 'disclosed capital'})`, value: (r: { split: FdiGroupSplit }) => (show === 'n' ? r.split.parts[g].shareN : r.split.parts[g].shareValue), cell: (r: { split: FdiGroupSplit }) => `${fmtShare(show === 'n' ? r.split.parts[g].shareN : r.split.parts[g].shareValue)} (${r.split.parts[g].n})`, num: true })),
          ]}
        />
      )}
      csv={csv}
      legend={<FdiGroupLegend highlight={highlight?.name ?? null} />}
      notes={
        <>
          {show === 'value' && hidden > 0 && <p>{hidden} destination{hidden === 1 ? '' : 's'} whose projects disclose no value {hidden === 1 ? 'is' : 'are'} left out of the value split (in the table).</p>}
          <FdiAttribution model={model} />
        </>
      }
      summary={`Across all destinations, parents headquartered in China hold ${fmtShare(show === 'n' ? world.parts.china.shareN : world.parts.china.shareValue)} of the ${show === 'n' ? 'projects' : 'disclosed capital'}, coalition core ${fmtShare(show === 'n' ? world.parts.core.shareN : world.parts.core.shareValue)}, partners ${fmtShare(show === 'n' ? world.parts.partner.shareN : world.parts.partner.shareValue)}.`}
    >
      {split.length ? (
        <>
          <ShareSplitChart
            rows={split}
            columns={[{ key: 'value', label: 'Disclosed capital', num: true }, { key: 'china', label: 'Chinese parents', num: true }]}
            ariaLabel="Projects of each destination by the parent's headquarters group"
            onRowClick={(r) => r.href && navigate(r.href)}
          />
          {drawable.length > initial + 1 && (
            <button type="button" className="btn btn-ghost btn-sm mt-3" onClick={() => setAll((x) => !x)}>
              {all ? `Show the top ${initial}` : `Show all ${drawable.length - 1} destinations`}
            </button>
          )}
        </>
      ) : (
        <p className="text-sm text-paper-mute">No projects for this selection.</p>
      )}
    </SectorChartCard>
  );
}

/** Announcements by year, stacked by the parent's headquarters group. */
export function YearsCard({ s, title = 'Announcements by year' }: { s: FdiScope; title?: string }) {
  const { model, show } = s;
  const w = windowOf(s);
  const [w0, w1] = w;
  const bars = useMemo(() => fdiYearBars(fdiYears(s.projects), w0, w1), [s.projects, w0, w1]);
  const data = useMemo(() => bars.flatMap((b) => FDI_GROUP_ORDER.map((g) => ({ year: b.year, group: g, v: show === 'n' ? b.groups[g].n : (b.groups[g].value ?? 0) }))).filter((d) => d.v > 0), [bars, show]);
  const totals = useMemo(() => bars.map((b) => ({ ...b, v: show === 'n' ? b.total.n : (b.total.value ?? 0) })), [bars, show]);
  const build = useCallback(
    (width: number) => ({
      width,
      x: { type: 'band' as const, domain: bars.map((b) => b.year), label: null, tickFormat: (d: number) => (d % 5 === 0 || bars.length <= 12 ? String(d) : '') },
      y: { label: show === 'n' ? 'Projects announced' : 'Disclosed capital, US$ m', grid: true, tickFormat: show === 'n' ? countAxis : usdAxis },
      color: { domain: [...FDI_GROUP_ORDER], range: FDI_GROUP_ORDER.map((g) => GROUP_COLOR[g]) },
      marks: [
        Plot.barY(data, Plot.stackY({ x: 'year', y: 'v', fill: 'group', order: [...FDI_GROUP_ORDER], insetLeft: 1, insetRight: 1 })),
        Plot.ruleY([0]),
        Plot.barY(totals, Plot.pointerX({ x: 'year', y: 'v', fill: 'none', stroke: SIGNAL })),
      ],
    }),
    [bars, data, totals, show],
  );
  const partial = partialYearNote(cmfdiSource(model), w[1]);
  const csv = useCardCsv(() => ({ name: `${s.item.slug}-fdi-years-${s.basis}`, text: yearsCsv(model, bars, s.item.slug, s.basis) }), [FDI_VARS.n, FDI_VARS.value]);
  const peak = [...totals].sort((a, z) => z.v - a.v)[0];
  return (
    <SectorChartCard
      title={title}
      vars={refs(s, 'show', w)}
      subExtra={
        <>
          {basisSub(s)}
          <HowToRead>Each bar is one year: {show === 'n' ? 'the projects announced that year' : 'the capital disclosed by the projects announced that year'}, stacked by where the parent companies have their headquarters. A taller bar means more announcements, which are plans: some are never built.</HowToRead>
        </>
      }
      table={() => (
        <TableView
          rows={bars}
          caption="Announcements by year and parent group"
          pageSize={40}
          cols={[
            { id: 'y', header: 'Year announced', value: (b) => b.year },
            ...FDI_GROUP_ORDER.map((g) => ({ id: g, header: `${FDI_GROUP_SHORT[g]} (${show === 'n' ? 'projects' : 'US$ m'})`, value: (b: (typeof bars)[number]) => (show === 'n' ? b.groups[g].n : b.groups[g].value), cell: (b: (typeof bars)[number]) => (show === 'n' ? String(b.groups[g].n) : b.groups[g].n === 0 ? '0' : b.groups[g].value === null ? NOT_DISCLOSED : fmtUsdM(b.groups[g].value, 'table')), num: true })),
            { id: 't', header: show === 'n' ? 'All projects' : 'All (US$ m)', value: (b) => (show === 'n' ? b.total.n : b.total.value), cell: (b) => (show === 'n' ? String(b.total.n) : b.total.n === 0 ? '0' : b.total.value === null ? NOT_DISCLOSED : fmtUsdM(b.total.value, 'table')), num: true },
          ]}
        />
      )}
      csv={csv}
      legend={<FdiGroupLegend />}
      notes={
        <>
          {partial && <p>{partial}</p>}
          {show === 'value' && <p>Disclosed capital only: projects without a disclosed value add nothing to a bar.</p>}
          <FdiAttribution model={model} />
        </>
      }
      summary={peak && peak.v > 0 ? `${peak.year} has the most: ${show === 'n' ? projectsWord(peak.v) : fmtUsdM(peak.v)} announced.` : 'No announcements for this selection.'}
    >
      {data.length ? (
        <PlotFigure<(typeof totals)[number]>
          build={build}
          height={240}
          ariaLabel={`${show === 'n' ? 'Projects' : 'Disclosed capital'} announced by year and parent group, ${model.cellLabel(s.item)}`}
          tooltip={(d) => (
            <Tip
              value={show === 'n' ? projectsWord(d.total.n) : fmtUsdM(d.total.value)}
              title={`Announced in ${d.year}`}
              lines={FDI_GROUP_ORDER.filter((g) => d.groups[g].n > 0).map((g) => `${FDI_GROUP_WORDS[g]}: ${show === 'n' ? d.groups[g].n : `${fmtUsdM(d.groups[g].value)} (${d.groups[g].nValue} of ${d.groups[g].n})`}`)}
            />
          )}
        />
      ) : (
        <p className="text-sm text-paper-mute">No announcements for this selection.</p>
      )}
    </SectorChartCard>
  );
}

/** The links and notes of a project's source cells (inside a disclosure, so the table stays compact). */
export function ProjectSources({ data, p }: { data: FdiData; p: FdiProject }) {
  const cells = data.sourcesOf(p.row);
  if (!cells.length) return <span className="text-paper-mute">none listed</span>;
  const n = cells.reduce((a, c) => a + c.urls.length, 0);
  return (
    <details className="min-w-[10rem]">
      {/* relative: the sr-only text is positioned against the summary, so it never escapes a scrolling table */}
      <summary className="relative cursor-pointer text-paper-dim hover:text-paper">
        {n} link{n === 1 ? '' : 's'}
        <span className="sr-only"> for {p.parent} in {p.city ?? 'its destination'}</span>
      </summary>
      <ul className="mt-1 space-y-1 text-xs">
        {cells.map((c) => (
          <li key={c.field}>
            <span className="text-paper-mute">{c.label}:</span>{' '}
            {c.urls.map((u, i) => (
              <span key={u}>
                {i > 0 && ', '}
                <ExternalLink href={u}>{hostOf(u)}</ExternalLink>
              </span>
            ))}
            {c.note && <span className="text-paper-dim"> {c.urls.length ? `(${c.note.replace(/^\((.*)\)$/, '$1')})` : c.note}</span>}
            {c.unverified && <span className="tag ml-1 px-1 py-0 text-[10px]">unverified in the dataset</span>}
          </li>
        ))}
      </ul>
    </details>
  );
}

const ANY = '';

/** The project list: filters, the table with source links per field, CSV. */
export function ProjectsCard({ s, title = 'The projects', direction }: { s: FdiScope; title?: string; direction?: (p: FdiProject) => string }) {
  const { model, data } = s;
  const [status, setStatus] = useState(ANY);
  const [activity, setActivity] = useState(ANY);
  const [home, setHome] = useState(ANY);
  const [dest, setDest] = useState(ANY);
  const w = windowOf(s);
  const statuses = FDI_PROJECT_STATUSES.filter((x) => s.projects.some((p) => p.status === x));
  const otherStatuses = [...new Set(s.projects.map((p) => p.status))].filter((x) => !(FDI_PROJECT_STATUSES as readonly string[]).includes(x));
  const activities = [...new Set(s.projects.map((p) => p.activity))];
  const geoOpts = (side: 'home' | 'dest') =>
    [...new Set(s.projects.map((p) => p[side]))]
      .map((g) => model.geos[g])
      .filter((g): g is GeoInfo => !!g)
      .sort((a, z) => a.name.localeCompare(z.name, 'en'))
      .map((g) => ({ value: g.code, label: g.name }));
  const rows = useMemo(
    () =>
      s.projects.filter(
        (p) =>
          (status === ANY || p.status === status) &&
          (activity === ANY || p.activity === activity) &&
          (home === ANY || model.geos[p.home]?.code === home) &&
          (dest === ANY || model.geos[p.dest]?.code === dest),
      ),
    [s.projects, status, activity, home, dest, model],
  );
  const csv = useCardCsv(() => ({ name: `${s.item.slug}-fdi-projects-${s.basis}`, text: projectsCsv(model, data, rows, direction) }), [FDI_VARS.n, FDI_VARS.value]);
  const cols: Col<FdiProject>[] = [
    ...(direction ? [{ id: 'dir', header: 'Direction', value: (p: FdiProject) => direction(p) }] : []),
    { id: 'parent', header: 'Parent company', value: (p) => p.parent, minWidth: 150 },
    {
      id: 'hq',
      header: 'Headquarters',
      value: (p) => model.geos[p.home]?.name ?? '',
      cell: (p) => (
        <span className="relative whitespace-nowrap">
          <span aria-hidden="true" className="mr-1.5 inline-block h-2 w-2 rounded-full align-middle" style={{ background: GROUP_COLOR[p.homeGroup] }} />
          {model.geos[p.home]?.name ?? ''}
          <span className="sr-only"> ({FDI_GROUP_WORDS[p.homeGroup]})</span>
        </span>
      ),
    },
    { id: 'dest', header: 'Destination', value: (p) => model.geos[p.dest]?.name ?? '' },
    { id: 'city', header: 'City', value: (p) => p.city, minWidth: 120 },
    { id: 'status', header: 'Status', value: (p) => p.status, cell: (p) => (<span>{p.status}{p.operational && p.operational.toLowerCase() !== p.status.toLowerCase() ? <span className="block text-xs text-paper-mute">{p.operational}</span> : null}</span>) },
    { id: 'activity', header: 'Activity', value: (p) => p.activity },
    { id: 'ya', header: 'Year announced', value: (p) => p.yearAnnounced, num: true },
    { id: 'yc', header: 'Year completed (or planned)', value: (p) => p.yearCompleted, num: true },
    { id: 'v', header: 'Capital investment (US$ m)', value: (p) => p.value, cell: (p) => (p.value === null ? <span className="text-paper-mute">{NOT_DISCLOSED}</span> : fmtUsdM(p.value, 'table')), num: true },
    { id: 'jv', header: 'Joint venture', value: (p) => jvWords(p), minWidth: 130 },
    { id: 'iu', header: 'Used by the indices', value: (p) => (p.indexUse ? 'yes' : 'no') },
    { id: 'src', header: 'Sources', value: () => null, sortable: false, cell: (p) => <ProjectSources data={data} p={p} /> },
  ];
  const nv = rows.filter((p) => p.value !== null).length;
  return (
    <SectorChartCard
      title={title}
      vars={[{ var: FDI_VARS.n, item: s.item.slug, window: w }, { var: FDI_VARS.value, item: s.item.slug, window: w }]}
      subExtra={
        <>
          {basisSub(s)}
          <HowToRead>Each row is one project as the dataset lists it: the parent company and its headquarters, the destination, status, years and capital (“not disclosed” where the source gives none). Filter by status or activity; the links open the pages the dataset cites.</HowToRead>
        </>
      }
      csv={csv}
      notes={
        <>
          <p>Statuses as CM-FDI publishes them (the second line is its "Operational" field where it differs). A joint venture is listed under the parent CM-FDI names. Source links open the pages CM-FDI cites for each field.</p>
          <FdiAttribution model={model} />
        </>
      }
      summary={`${projectsWord(rows.length)} listed; ${nv} disclose a capital investment.`}
    >
      <div className="mb-3 flex flex-wrap items-end gap-x-4 gap-y-3" role="group" aria-label="Filter the projects">
        <Select label="Status" value={status} options={[{ value: ANY, label: 'All statuses' }, ...[...statuses, ...otherStatuses].map((x) => ({ value: x, label: x }))]} onChange={setStatus} className="min-w-[10rem]" />
        <Select label="Activity" value={activity} options={[{ value: ANY, label: 'All activities' }, ...activities.map((x) => ({ value: x, label: x }))]} onChange={setActivity} className="min-w-[10rem]" />
        <Select label="Headquarters" value={home} options={[{ value: ANY, label: 'All' }, ...geoOpts('home')]} onChange={setHome} className="min-w-[10rem]" />
        <Select label="Destination" value={dest} options={[{ value: ANY, label: 'All' }, ...geoOpts('dest')]} onChange={setDest} className="min-w-[10rem]" />
        <span className="pb-1 text-sm text-paper-mute">
          {projectsWord(rows.length)}
          {rows.length !== s.projects.length ? ` of ${s.projects.length}` : ''}
        </span>
      </div>
      <TableView rows={rows} cols={cols} caption={`Cross-border projects, ${model.cellLabel(s.item)}`} search pageSize={15} initialSort={{ id: 'ya', desc: true }} rowKey={(p) => p.pid} empty="No projects for these filters" />
    </SectorChartCard>
  );
}

/** A one-row split of a set of projects by parent group (country pages). */
export function GroupBar({ split, label, show = 'n' }: { split: FdiGroupSplit; label: string; show?: FdiShow }) {
  const parts = splitShares(split, show);
  return <BlocBar parts={parts} ariaLabel={`${label}: ${FDI_GROUP_ORDER.map((g) => `${FDI_GROUP_WORDS[g]} ${(100 * (parts[g] ?? 0)).toFixed(1)} percent`).join(', ')}`} label={label} />;
}

/** A section intro: what CM-FDI is and what is counted. */
export function FdiIntro({ model, children }: { model: SectorModel; children?: ReactNode }) {
  const src = cmfdiSource(model);
  return (
    <div className="max-w-3xl space-y-2 text-sm text-paper-dim">
      <p>
        <Term id="cmfdi">Factory projects</Term>: cross-border greenfield and expansion projects of parent companies headquartered in another economy, from the Cleantech Manufacturing FDI Dataset (CM-FDI{src?.edition ? `, edition ${src.edition}` : ''}). Counts and disclosed capital as published: nothing is scored or weighted, and domestic investment is not covered. Announced projects are included and may never be built.
      </p>
      {children}
    </div>
  );
}
