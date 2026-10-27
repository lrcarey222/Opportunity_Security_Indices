// Mineral profile /sector/minerals/:mineral (spec_sector.md 4.5): snapshot tiles (USGS world mine
// production and reserves, IEA top-3 mining and refining, China's refining share, coalition shares);
// mining vs refining (IEA base case, one share-split row per year with projections outlined, plus the
// slope of the largest holders' shares); reserves and production (USGS leagues and the share-vs-share
// scatter); trade (Comtrade critical-mineral exports: reported and mirror, HKG tagged); prices (IMF
// board and monthly series); who uses it (IEA demand by technology, observed vs STEPS). uranium
// redirects to Nuclear — Upstream; an unknown mineral goes to the index with a notice.

import { useCallback, useMemo, useState } from 'react';
import { Link, useLocation, useNavigate, useParams } from 'react-router';
import { Redirect } from '../../../shell/Redirect.tsx';
import * as Plot from '@observablehq/plot';
import { PlotFigure, Tip } from '../../../charts/PlotFigure.tsx';
import { TableView } from '../../../components/TableView.tsx';
import { PageHead, Segmented, StatTile } from '../../../components/ui.tsx';
import { rowsToCsv } from '../../../lib/csv.ts';
import { useTitle } from '../../../shell/title.ts';
import { GROUP_COLOR, NEUTRAL_SERIES, PAPER_DIM, SIGNAL } from '../../../theme/palette.ts';
import { MINERAL_REDIRECTS } from '../../contract.ts';
import type { FactsIndex } from '../../data/facts.ts';
import type { GeoInfo, ItemInfo, SectorModel, VarInfo } from '../../data/model.ts';
import { useModel } from '../../data/SectorDataProvider.tsx';
import { sectorCsv } from '../../derive/csv.ts';
import {
  commodityOf, CRITMIN_DEP_VAR, CRITMIN_EXPORTS_VAR, demandByTech, isProjection, MINE_VAR, MINING_VAR, observedSummary, REFINING_VAR, RESERVES_VAR,
  shareLines, shareScatter, summaryYears, techsOfMineral,
} from '../../derive/minerals.ts';
import { slice } from '../../derive/summary.ts';
import { scenarioWords } from '../../derive/text.ts';
import { lc, fmtHhi, fmtShare, fmtSig, listText, ordinal } from '../../format.ts';
import { LeagueCard } from '../cards.tsx';
import { ChunkGate, fmtFact, HowToRead, measureConc, Section, SPLIT_COLUMNS, SPLIT_READING, shareSplitRow, SplitTable, useHighlight } from '../common.tsx';
import { useFacts, useScrollToHash } from '../data.ts';
import { Dumbbell, GroupLegend, LabelledLines, SectorChartCard, ShareSplitChart, type DumbbellRow, type ShareSplitRow } from '../kit.tsx';
import { MonthlyPrices, PriceBoard } from '../profile/Prices.tsx';
import { useSectorUrl } from '../state.tsx';

interface Ctx {
  model: SectorModel;
  item: ItemInfo;
  facts: FactsIndex | null;
  highlight: GeoInfo | null;
  eu: 'members' | 'bloc';
}

const vOf = (model: SectorModel, id: string): VarInfo | null => model.varById.get(id) ?? null;
const hasData = (model: SectorModel, v: VarInfo | null, item: ItemInfo): v is VarInfo => !!v && model.availOf(v, item.idx).length > 0;
const pct = (x: number) => `${Math.round(x * 100)}%`;

// ---- snapshot ----------------------------------------------------------------------------------

function Snapshot({ model, item, facts, highlight, eu }: Ctx) {
  const mineV = vOf(model, MINE_VAR);
  const resV = vOf(model, RESERVES_VAR);
  const miningV = vOf(model, MINING_VAR);
  const refV = vOf(model, REFINING_VAR);
  const mine = observedSummary(model, mineV, item, eu);
  const res = observedSummary(model, resV, item, eu);
  const mining = observedSummary(model, miningV, item, eu);
  const ref = observedSummary(model, refV, item, eu);
  const topMining = mining ?? mine;
  const topMiningV = mining ? miningV : mineV;
  const tiles: { key: string; label: string; value: string; sub: string; title: string }[] = [];
  const cap = (v: VarInfo, y: number) => `${v.label} · ${model.unitOf(v, item.idx).long} · ${y} · ${v.source.short}`;
  if (mine && mineV) tiles.push({ key: 'mine', label: 'World mine production', value: fmtFact(model, mineV, item, mine.W), sub: `${mineV.source.short}, ${mine.year}`, title: cap(mineV, mine.year) });
  if (res && resV) tiles.push({ key: 'res', label: 'World reserves', value: fmtFact(model, resV, item, res.W), sub: `${resV.source.short}, end-${res.year}`, title: cap(resV, res.year) });
  if (topMining && topMiningV) {
    tiles.push({ key: 'top3m', label: 'Top-3 mining share', value: fmtShare(topMining.top3_share), sub: `${topMining.top1_geo !== null ? `largest ${model.geoName(topMining.top1_geo)} · ` : ''}${topMiningV.source.short}, ${topMining.year}`, title: cap(topMiningV, topMining.year) });
  }
  if (ref && refV) {
    tiles.push({ key: 'top3r', label: 'Top-3 refining share', value: fmtShare(ref.top3_share), sub: `concentration index ${fmtHhi(ref.hhi, ref.hhi_lower_bound)} · ${refV.source.short}, ${ref.year}`, title: cap(refV, ref.year) });
    // China's output and the world total in the source's unit ("235 of 319 kt"; spec 12.3)
    const chinaOf = ref.china > 0 && Number.isFinite(ref.W) ? `${fmtSig(ref.china * ref.W * (refV.display_scale || 1))} of ${fmtFact(model, refV, item, ref.W)} · ` : '';
    tiles.push({ key: 'chnr', label: "China's refining share", value: fmtShare(ref.china), sub: `${chinaOf}${refV.source.short}, ${ref.year}`, title: cap(refV, ref.year) });
  }
  if (topMining && topMiningV) tiles.push({ key: 'coalm', label: 'Coalition share of mining', value: fmtShare(topMining.core + topMining.partner), sub: `core ${fmtShare(topMining.core)} · partners ${fmtShare(topMining.partner)}${eu === 'bloc' ? ' · EU-27 as one' : ''}`, title: cap(topMiningV, topMining.year) });
  if (ref && refV) tiles.push({ key: 'coalr', label: 'Coalition share of refining', value: fmtShare(ref.core + ref.partner), sub: `core ${fmtShare(ref.core)} · partners ${fmtShare(ref.partner)}`, title: cap(refV, ref.year) });
  if (highlight) {
    const parts: string[] = [];
    for (const [v, s, words] of [[topMiningV, topMining, 'mining'], [refV, ref, 'refining']] as const) {
      if (!v || !s) continue;
      const m = measureConc(model, facts, v, item, s.year, eu);
      const r = m?.conc.rows.find((x) => x.geo === highlight.idx);
      if (r) parts.push(`${words} ${fmtShare(r.share)} (${ordinal(r.rank)})`);
      else if (m?.fromFacts) parts.push(`${words}: not listed`);
    }
    tiles.push({ key: 'hl', label: 'Highlight', value: highlight.name, sub: parts.length ? parts.join(' · ') : facts ? 'no mining or refining value' : 'loading the facts', title: 'Shares of the world total, same sources and years as the tiles' });
  }
  if (!tiles.length) return <p className="text-sm text-paper-mute">No production or reserves data for {lc(item.label)}.</p>;
  return (
    <div className="grid grid-cols-2 gap-3 md:grid-cols-3 xl:grid-cols-4" role="list" aria-label="Snapshot">
      {tiles.map((t) => (
        <div key={t.key} role="listitem" title={t.title}>
          <StatTile label={t.label} value={<span className="font-medium">{t.value}</span>} sub={t.sub} />
        </div>
      ))}
    </div>
  );
}

// ---- mining vs refining ------------------------------------------------------------------------

function YearSplit({ model, item, facts, highlight, eu, v, words }: Ctx & { v: VarInfo; words: string }) {
  const years = summaryYears(model, v, item, eu);
  const ys = years.length ? years : facts ? facts.yearsOf(v.idx, item.idx) : [];
  const rows: ShareSplitRow[] = [];
  for (const y of ys) {
    const m = measureConc(model, facts, v, item, y, eu);
    if (!m || m.conc.W === null) continue;
    const proj = isProjection(v, y);
    rows.push(shareSplitRow(model, v, item, m.conc, { id: `${v.id}-${y}`, label: String(y), sub: `${v.source.short}${proj ? ', base case' : ''}`, year: y, highlight, projection: proj }));
  }
  if (!rows.length) return null;
  const first = ys[0];
  const last = ys[ys.length - 1];
  const r0 = rows[0];
  const rN = rows[rows.length - 1];
  return (
    <SectorChartCard
      title={`${words[0].toUpperCase()}${words.slice(1)}, ${first === last ? first : `${first}-${last}`}`}
      vars={[{ var: v.id, item: item.slug, window: first === last ? null : [first, last], year: first === last ? first : null }]}
      subExtra={<HowToRead>{SPLIT_READING} One row per year; later rows are the IEA&apos;s projections, so you can see whether the split is expected to change.</HowToRead>}
      table={() => <SplitTable rows={rows} caption={`${item.label} ${words} by year`} />}
      csv={() => ({ name: `${item.slug}-${words}-by-year`, text: rowsToCsv(rows.map((r) => ({ mineral: item.slug, var_id: v.id, year: r.cols.year, projection: !!r.projection, ...Object.fromEntries(Object.entries(r.parts).map(([k, x]) => [`share_${k}`, x])), top1: r.cols.top1, top3: r.cols.top3, hhi: r.cols.hhi, source: r.cols.source }))) })}
      legend={<GroupLegend projection={rows.some((r) => r.projection)} highlight={highlight?.name ?? null} />}
      summary={`China holds ${fmtShare(r0.parts.china)} of ${lc(item.label)} ${words} in ${r0.cols.year}${rows.length > 1 ? ` and ${fmtShare(rN.parts.china)} in ${rN.cols.year} (projection)` : ''}.`}
    >
      <ShareSplitChart rows={rows} columns={SPLIT_COLUMNS} ariaLabel={`${item.label} ${words}: shares by group and year`} highlightHeader={highlight?.name} />
    </SectorChartCard>
  );
}

function Slope(c: Ctx & { vars: VarInfo[] }) {
  const { model, item, facts, highlight, eu, vars } = c;
  const [which, setWhich] = useState(vars[vars.length - 1]?.id ?? '');
  const v = vars.find((x) => x.id === which) ?? vars[0];
  const lines = useMemo(() => (facts && v ? shareLines(model, facts, v, item, eu, { top: 3, highlight }) : []), [facts, v, model, item, eu, highlight]);
  if (!v || !facts || lines.length < 1) return null;
  const years = facts.yearsOf(v.idx, item.idx);
  if (years.length < 2) return null;
  const first = years[0];
  const last = years[years.length - 1];
  const china = lines.find((l) => l.id === 'CHN');
  const c0 = china?.points.find((p) => p.x === first)?.y;
  const cN = china?.points.find((p) => p.x === last)?.y;
  const words = v.id === REFINING_VAR ? 'refining' : 'mining';
  return (
    <SectorChartCard
      title={`The largest holders' shares of ${words}, ${first} → ${last}`}
      vars={[{ var: v.id, item: item.slug, window: [first, last] }]}
      subExtra={<HowToRead>Each line is one of the largest holders: its share of world {words}, year by year, observed and then projected. A rising line means the economy is expected to hold more of the world total.</HowToRead>}
      tools={
        vars.length > 1 ? (
          <Segmented label="Stage" hideLabel value={v.id} options={vars.map((x) => ({ value: x.id, label: x.id === REFINING_VAR ? 'Refining' : 'Mining' }))} onChange={setWhich} />
        ) : undefined
      }
      table={() => (
        <TableView
          rows={lines}
          caption={`Shares of world ${words}`}
          cols={[{ id: 'e', header: 'Economy', value: (l) => l.label }, ...years.map((y) => ({ id: String(y), header: isProjection(v, y) ? `${y} (projection)` : String(y), value: (l: (typeof lines)[number]) => l.points.find((p) => p.x === y)?.y ?? null, cell: (l: (typeof lines)[number]) => fmtShare(l.points.find((p) => p.x === y)?.y), num: true }))]}
        />
      )}
      csv={() => ({ name: `${item.slug}-${words}-shares`, text: sectorCsv(model, lines.flatMap((l) => l.points.map((p) => ({ geo: model.geos[l.geo], v, item, year: p.x, value: facts.value(v.idx, item.idx, p.x, l.geo)?.value ?? null, status: p.projection ? 'projection' : 'obs', share: p.y })))) })}
      notes={<p>China{highlight ? ' and the highlighted economy are' : ' is'} drawn in group colour; the other holders are gray. Dashed segments and hollow points are projections (IEA base case).</p>}
      summary={china && c0 !== undefined && cN !== undefined ? `China's share of world ${lc(item.label)} ${words} goes from ${fmtShare(c0)} in ${first} to ${fmtShare(cN)} in ${last} (projection).` : `Shares of world ${words} of the largest holders, ${first} to ${last}.`}
    >
      <LabelledLines lines={lines.map((l) => ({ id: l.id, label: l.label, group: l.group, points: l.points }))} highlight={highlight?.code ?? null} yLabel={`Share of world ${words}`} ariaLabel={`Shares of world ${lc(item.label)} ${words} by economy`} valueFormat={(x) => fmtShare(x)} yTickFormat={pct} height={260} />
    </SectorChartCard>
  );
}

function MiningRefining(c: Ctx) {
  const { model, item } = c;
  const miningV = vOf(model, MINING_VAR);
  const refV = vOf(model, REFINING_VAR);
  const vars = [miningV, refV].filter((v): v is VarInfo => hasData(model, v, item));
  if (!vars.length) return <p className="text-sm text-paper-mute">The IEA Critical Minerals Dataset does not cover {lc(item.label)}; see USGS mine production below.</p>;
  return (
    <>
      {/* two up only on very wide screens: a half-width card squeezed the share split (stacked below) */}
      <div className="grid gap-6 min-[1600px]:grid-cols-2">
        {vars.map((v) => (
          <YearSplit key={v.id} {...c} v={v} words={v.id === REFINING_VAR ? 'refining' : 'mining'} />
        ))}
      </div>
      <Slope {...c} vars={vars} />
    </>
  );
}

// ---- reserves and production -------------------------------------------------------------------

function Scatter({ model, item, facts, highlight, eu, prod, res }: Ctx & { prod: VarInfo; res: VarInfo }) {
  const { set, state } = useSectorUrl();
  const yp = facts?.latestYear(prod.idx, item.idx) ?? null;
  const yr = facts?.latestYear(res.idx, item.idx) ?? null;
  const data = useMemo(() => (facts && yp !== null && yr !== null ? shareScatter(model, facts, prod, res, item, yp, yr, eu) : null), [facts, yp, yr, model, prod, res, item, eu]);
  const pts = data?.points ?? [];
  const labelled = useMemo(() => new Set([...pts.slice(0, 5).map((p) => p.geo), ...(highlight ? [highlight.idx] : [])]), [pts, highlight]);
  const build = useCallback(
    (width: number) => {
      const hi = Math.max(0.05, ...pts.map((p) => Math.max(p.x, p.y)));
      const dom: [number, number] = [0, Math.min(1, hi * 1.08)];
      return {
        width,
        x: { label: `Share of world mine production, ${yp}`, domain: dom, tickFormat: pct, grid: true },
        y: { label: `Share of world reserves, end-${yr}`, domain: dom, tickFormat: pct, grid: true },
        marks: [
          Plot.line([{ x: 0, y: 0 }, { x: dom[1], y: dom[1] }], { x: 'x', y: 'y', stroke: NEUTRAL_SERIES, strokeDasharray: '4 3', strokeOpacity: 0.7 }),
          Plot.text([{ x: dom[1] * 0.92, y: dom[1] * 0.92 }], { x: 'x', y: 'y', text: () => 'same share', dy: -8, fill: PAPER_DIM, fontSize: 11, textAnchor: 'end' }),
          Plot.dot(pts, { x: 'x', y: 'y', r: 5, fill: (d: (typeof pts)[number]) => GROUP_COLOR[d.group], fillOpacity: 0.9 }),
          Plot.dot(pts.filter((p) => p.geo === highlight?.idx), { x: 'x', y: 'y', r: 8, stroke: SIGNAL, strokeWidth: 2, fill: 'none' }),
          Plot.text(pts.filter((p) => labelled.has(p.geo)), { x: 'x', y: 'y', text: 'name', dy: -10, fill: PAPER_DIM, fontSize: 11 }),
          Plot.dot(pts, Plot.pointer({ x: 'x', y: 'y', r: 10, stroke: SIGNAL, fill: 'none' })),
        ],
      };
    },
    [pts, yp, yr, highlight, labelled],
  );
  if (!data || pts.length < 2 || yp === null || yr === null) return null;
  const above = pts.filter((p) => p.y > p.x).length;
  return (
    <SectorChartCard
      title="Share of reserves against share of production"
      vars={[{ var: prod.id, item: item.slug, year: yp }, { var: res.id, item: item.slug, year: yr }]}
      subExtra={<HowToRead>Each dot is one economy: further right, a larger share of world mine production; higher, a larger share of world reserves. Dots above the diagonal hold more of the reserves than they mine today (room to grow); dots below mine more than their share of reserves.</HowToRead>}
      table={() => (
        <TableView rows={pts} caption="Shares of world production and reserves" search cols={[{ id: 'e', header: 'Economy', value: (p) => p.name }, { id: 'x', header: `Share of mine production, ${yp}`, value: (p) => p.x, cell: (p) => fmtShare(p.x), num: true }, { id: 'y', header: `Share of reserves, end-${yr}`, value: (p) => p.y, cell: (p) => fmtShare(p.y), num: true }]} />
      )}
      csv={() => ({ name: `${item.slug}-production-vs-reserves`, text: sectorCsv(model, pts.flatMap((p) => [{ geo: model.geos[p.geo], v: prod, item, year: yp, value: facts?.value(prod.idx, item.idx, yp, p.geo)?.value ?? null, status: 'obs', share: p.x }, { geo: model.geos[p.geo], v: res, item, year: yr, value: facts?.value(res.idx, item.idx, yr, p.geo)?.value ?? null, status: 'obs', share: p.y }])) })}
      legend={<GroupLegend groups={['china', 'core', 'partner', 'rest']} highlight={highlight?.name ?? null} />}
      notes={
        <p>
          Both shares are of the source's world totals ({prod.source.short}). Above the line, an economy holds a larger share of reserves than of production.
          {data.only.production + data.only.reserves > 0 ? ` Not plotted: ${data.only.production} economies with production but no reserves figure, ${data.only.reserves} with reserves but no production figure.` : ''}
        </p>
      }
      summary={`${pts.length} economies; ${above} hold a larger share of world reserves than of world mine production.`}
    >
      <PlotFigure<(typeof pts)[number]>
        build={build}
        height={340}
        ariaLabel={`Scatter of shares of world ${lc(item.label)} production and reserves`}
        tooltip={(p) => <Tip title={p.name} value={`production ${fmtShare(p.x)} · reserves ${fmtShare(p.y)}`} />}
        onSelect={(p) => set({ c: p.code === state.c ? null : p.code })}
      />
    </SectorChartCard>
  );
}

function ReservesProduction(c: Ctx) {
  const { model, item, facts, highlight, eu } = c;
  const resV = vOf(model, RESERVES_VAR);
  const mineV = vOf(model, MINE_VAR);
  const league = (v: VarInfo | null, title: (y: number) => string) => {
    if (!facts || !hasData(model, v, item)) return null;
    const y = facts.latestYear(v.idx, item.idx);
    if (y === null) return null;
    const sl = slice(model, facts, v, item.idx, y, eu);
    return <LeagueCard key={v.id} model={model} v={v} item={item} year={y} rows={sl.rows} world={sl.world} title={title(y)} highlight={highlight} map top={15} csvName={`${item.slug}-${v.id}-${y}`} />;
  };
  const cards = [league(resV, (y) => `Reserves, end-${y}`), league(mineV, (y) => `Mine production, ${y}`)].filter(Boolean);
  return (
    <>
      {cards.length > 0 ? <div className="grid gap-6 xl:grid-cols-2">{cards}</div> : <p className="text-sm text-paper-mute">No USGS reserves or mine production for {lc(item.label)}.</p>}
      {hasData(model, mineV, item) && hasData(model, resV, item) && <Scatter {...c} prod={mineV} res={resV} />}
    </>
  );
}

// ---- trade -------------------------------------------------------------------------------------

function Trade({ model, item, facts, highlight, eu }: Ctx) {
  const v = vOf(model, CRITMIN_EXPORTS_VAR);
  const dep = vOf(model, CRITMIN_DEP_VAR);
  if (!facts || !hasData(model, v, item)) return <p className="text-sm text-paper-mute">No mineral trade codes for {lc(item.label)} in the Comtrade critical-minerals basket.</p>;
  const y = facts.latestYear(v.idx, item.idx);
  if (y === null) return null;
  const sl = slice(model, facts, v, item.idx, y, eu);
  const m = measureConc(model, facts, v, item, y, eu);
  const hDep = highlight && dep ? facts.value(dep.idx, item.idx, facts.latestYear(dep.idx, item.idx) ?? y, highlight.idx) : null;
  const mirror = sl.rows.filter((r) => r.status === 'mirror').length;
  return (
    <LeagueCard
      model={model}
      v={v}
      item={item}
      year={y}
      rows={sl.rows}
      world={sl.world}
      title={`Exporters of ${lc(item.label)}, ${y}`}
      highlight={highlight}
      map
      top={15}
      extraVars={dep && highlight ? [{ var: dep.id, item: item.slug, year: y }] : undefined}
      notes={
        <div className="space-y-1">
          {m && (
            <p>
              concentration index {fmtHhi(m.conc.hhi, m.conc.hhi_lower_bound)} · coalition {fmtShare(m.conc.split.core + m.conc.split.partner)} · China {fmtShare(m.conc.split.china)}
              {mirror ? ` · ${mirror} exporters seen only through partners' declarations ("mirror")` : ''}.
            </p>
          )}
          {highlight && dep && (
            <p>
              {highlight.name}: {lc(item.label)} exports are {hDep && Number.isFinite(hDep.value) ? fmtFact(model, dep, item, hDep.value) : 'not recorded as a share'} of its total exports ({dep.source.short}, {y}).
            </p>
          )}
        </div>
      }
      csvName={`${item.slug}-exports-${y}`}
    />
  );
}

// ---- who uses it -------------------------------------------------------------------------------

function WhoUses({ model, item }: Ctx) {
  const { link } = useSectorUrl();
  const navigate = useNavigate();
  const { rows, total } = useMemo(() => demandByTech(model, item), [model, item]);
  if (!rows.length) return <p className="text-sm text-paper-mute">The IEA does not report clean-energy demand for {lc(item.label)}.</p>;
  const kt = (x: number) => `${x.toLocaleString('en-US', { maximumFractionDigits: 0 })} kt`;
  const projYear = rows.find((r) => r.projectedYear !== null)?.projectedYear ?? null;
  const scenario = rows.find((r) => r.scenario)?.scenario ?? 'STEPS';
  const obsYear = rows[0].observedYear;
  const dumb: DumbbellRow[] = rows.map((r) => ({
    id: r.tech ?? r.label,
    label: r.label,
    a: r.projected,
    b: r.observed,
    group: 'neutral',
    aLabel: Number.isFinite(r.projected) ? `${kt(r.projected)} (${r.projectedYear} ${scenarioWords(r.scenario)} projection)` : '–',
    bLabel: `${kt(r.observed)} (${r.observedYear})${r.share !== null ? ` · ${fmtShare(r.share)} of clean-energy demand` : ''}`,
    href: r.tech ? link(`/sector/${r.tech}`) : undefined,
  }));
  return (
    <SectorChartCard
      title={`Who uses ${lc(item.label)}`}
      vars={[{ var: 'consumption.mineral_demand', item: item.slug, window: projYear ? [obsYear, projYear] : null, year: projYear ? null : obsYear }]}
      subExtra={<HowToRead>Each row is one technology; the filled dot is how many kilotonnes of {lc(item.label)} it used in the latest observed year, the hollow dot the IEA projection under today’s stated policies (the Stated Policies Scenario), not a forecast. The further right, the more it uses. Click a row for that technology’s supply chain.</HowToRead>}
      table={() => (
        <TableView
          rows={rows}
          caption={`Demand for ${lc(item.label)} by technology`}
          cols={[
            { id: 't', header: 'Technology', value: (r) => r.label, cell: (r) => (r.tech ? <Link className="link" to={link(`/sector/${r.tech}`)}>{r.label}</Link> : r.label) },
            { id: 'o', header: `Demand ${obsYear}, kt`, value: (r) => r.observed, num: true },
            { id: 'p', header: projYear ? `Demand ${projYear}, ${scenarioWords(scenario)} projection, kt` : 'Projection, kt', value: (r) => (Number.isFinite(r.projected) ? r.projected : null), num: true },
            { id: 's', header: 'Share of clean-energy demand', value: (r) => r.share, cell: (r) => fmtShare(r.share), num: true },
          ]}
        />
      )}
      csv={() => ({ name: `${item.slug}-demand-by-technology`, text: rowsToCsv(rows.map((r) => ({ mineral: item.slug, technology: r.label, tech_slug: r.tech, observed_year: r.observedYear, observed_kt: r.observed, projected_year: r.projectedYear, scenario: r.scenario, projected_kt: Number.isFinite(r.projected) ? r.projected : null, share_of_clean_demand: r.share, source: 'IEA Critical Minerals Dataset' }))) })}
      notes={total ? <p>Total clean-energy demand {kt(total.observed)} in {total.observedYear}{Number.isFinite(total.projected) ? `; ${kt(total.projected)} in ${total.projectedYear} (${scenarioWords(total.scenario)} projection)` : ''}.</p> : undefined}
      summary={`${rows[0].label} uses the most ${lc(item.label)} among clean technologies (${kt(rows[0].observed)} in ${obsYear}).`}
    >
      <Dumbbell rows={dumb} xLabel="Demand, kt" aName={projYear ? `${projYear} ${scenarioWords(scenario)} projection` : 'Projection'} bName={String(obsYear)} ariaLabel={`Demand for ${lc(item.label)} by technology`} onSelect={(r) => r.href && navigate(r.href)} />
    </SectorChartCard>
  );
}

// ---- page --------------------------------------------------------------------------------------

function Body(c: Ctx) {
  const { model, item } = c;
  const loc = useLocation();
  useScrollToHash(loc.hash, !!c.facts);
  const commodity = commodityOf(model, item);
  const priceV = vOf(model, 'prices.latest');
  const hasPrice = !!commodity && hasData(model, priceV, commodity);
  return (
    <div className="space-y-10">
      <Section id="snapshot" title="Snapshot">
        <Snapshot {...c} />
      </Section>
      <Section id="mining-refining" title="Mining and refining" sub="IEA base case: the observed year and the projections. IEA lists the six largest producers, so the rest is not attributed and the concentration index (HHI) is a lower bound.">
        <MiningRefining {...c} />
      </Section>
      <Section id="reserves" title="Reserves and production" sub="USGS Mineral Commodity Summaries: reserves and mine production by country, each against the USGS world total.">
        <ReservesProduction {...c} />
      </Section>
      <Section id="trade" title="Trade" sub="Comtrade critical-mineral exports: reported by the exporter, or seen through partners' declarations (mirror).">
        <Trade {...c} />
      </Section>
      {hasPrice && commodity && (
        <Section id="prices" title="Prices" sub="World prices: the same for every country.">
          <MonthlyPrices items={[commodity]} />
          <PriceBoard items={[commodity]} title="Price" />
        </Section>
      )}
      <Section id="uses" title="Who uses it">
        <WhoUses {...c} />
      </Section>
    </div>
  );
}

export default function MineralView() {
  const { mineral: raw } = useParams();
  const model = useModel();
  const { state, link } = useSectorUrl();
  const slug = (raw ?? '').toLowerCase();
  const item = model.item('mineral', slug);
  useTitle(item ? item.label : 'Minerals', 'Sector Insights');
  const { facts, h } = useFacts();
  const highlight = useHighlight(model);
  if (MINERAL_REDIRECTS[slug]) return <Redirect to={link(MINERAL_REDIRECTS[slug])} notice={`${slug[0].toUpperCase()}${slug.slice(1)} is measured on the Nuclear — Upstream page.`} />;
  if (!item) return <Redirect to={link('/sector/minerals')} notice={`No mineral called '${raw}'; showing all minerals.`} />;
  const techs = techsOfMineral(model, item);
  const ctx: Ctx = { model, item, facts, highlight, eu: state.eu };
  return (
    <div className="space-y-8">
      <PageHead
        eyebrow="SECTORS · MINERALS"
        title={item.label}
        lede={`Who mines, refines, holds and exports ${lc(item.label)}${techs.length ? `, and how much ${listText(techs.slice(0, 4).map((t) => t.label))} use${techs.length === 1 ? 's' : ''}` : ''}. Measured values in each source's own units and years.`}
      >
        <div className="mt-3 flex flex-wrap items-center gap-2">
          {techs.map((t) => (
            <Link key={t.slug} className="tag" to={link(`/sector/${t.slug}`)} title={t.share !== null ? `${fmtShare(t.share)} of clean-energy demand for ${lc(item.label)} (IEA)` : undefined}>
              {t.label}
            </Link>
          ))}
          <Link className="link ml-2 text-sm" to={link('/sector/minerals')}>
            All minerals
          </Link>
        </div>
      </PageHead>
      <ChunkGate h={h} name="sector-facts">
        {() => <Body {...ctx} />}
      </ChunkGate>
    </div>
  );
}
