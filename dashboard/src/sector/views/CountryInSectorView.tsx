// Country in sector /sector/:tech/:stage/:iso3 (spec_sector.md 4.4): one economy's position in one
// sector. Position tiles per country-level measure (with the value states), its rank among peers (a
// beeswarm of the primary measure), its trade partners (where imports come from, where exports go,
// and its import mix 2020 vs 2024), in the CM-FDI cells (Components and Assembly of four technologies) its inbound and outbound cross-border
// manufacturing projects (sector-fdi, loaded when the section nears the viewport), its aid and
// investment partners (sector-dev: aid for the stage's aid sector, bilateral direct-investment
// positions; loaded near the viewport), its measures over time (separate small charts, never a dual
// axis), its policies and its stage context.

import { useMemo } from 'react';
import { Link, useLocation, useParams } from 'react-router';
import { Redirect } from '../../shell/Redirect.tsx';
import { TableView } from '../../components/TableView.tsx';
import { PageHead, StatTile } from '../../components/ui.tsx';
import { Term } from '../../components/Term.tsx';
import { useTitle } from '../../shell/title.ts';
import { isStageSlug, isTechSlug, STAGE_LABELS, TECH_LABELS, type SectorTradeChunk } from '../contract.ts';
import { countryInSectorQuestion } from '../copy.ts';
import { strategyHref } from '../posture/derive.ts';
import { isLegacyStageSlug, legacyStageNote, legacyStageOfSlug, legacyStageTarget } from '../../lib/stages.ts';
import { useModel } from '../data/SectorDataProvider.tsx';
import type { FactsIndex } from '../data/facts.ts';
import type { GeoInfo, ItemInfo, SectorModel, VarInfo } from '../data/model.ts';
import { countryVars, measureItem, primaryMeasure, varsOfCell } from '../derive/cells.ts';
import { basisWords } from '../derive/concentration.ts';
import { isDiverging } from '../derive/explore.ts';
import { GROUP_LABEL, partOf } from '../derive/groups.ts';
import { regionOf, stateOf, stateWords } from '../derive/states.ts';
import { ASSUMED_MARK, noteLine } from '../derive/notes.ts';
import { slice } from '../derive/summary.ts';
import { blocFlows, flowsOf, partners, supplierMixes, type TradeYear } from '../derive/trade.ts';
import { hasFdi } from '../derive/fdi.ts';
import { sectorCsv } from '../derive/csv.ts';
import { rowsToCsv } from '../../lib/csv.ts';
import { lc, fmtShare, fmtUsd, listText, logAxisFormatter, ordinal } from '../format.ts';
import { factYearText, periodText } from './caption.ts';
import { rankSlice } from './cards.tsx';
import { ChunkGate, fmtFact, HowToRead, markGroup } from './common.tsx';
import { useFacts, useNear, usePolicy, useSeries, useTrade } from './data.ts';
import { FdiCountry } from './fdi/FdiCountry.tsx';
import { DevCountry } from './dev/DevCountry.tsx';
import { Beeswarm, Dumbbell, LabelledLines, LeagueBars, SectorChartCard, type BeeswarmPoint, type DumbbellRow, type LeagueRow } from './kit.tsx';
import { useSectorUrl } from './state.tsx';
import { baseYear } from './TechChainView.tsx';
import { CsvRegistryProvider, ProfileProvider } from './profile/ctx.tsx';
import { PolicyBody } from './profile/Policy.tsx';
import { contextItem } from './profile/SectorProfileView.tsx';

interface Position {
  v: VarInfo;
  year: number;
  text: string;
  sub: string;
  rank: number | null;
  n: number;
  /** ranked within its sign (net importers / exporters): not a "top 10" position */
  signed?: boolean;
}

/**
 * A measure in the lead sentence: one over a span of years says so ("aid received over 2013–2024"),
 * a yearly development measure names its year ("aid received in 2023"), so the two read apart.
 */
function leadLabel(v: VarInfo, year: number): string {
  const base = lc(v.short.replace(/, all years$/, ''));
  const period = periodText(v, 'short');
  if (period) return `${base} over ${period.replace(/ (total|average)$/, '')}`;
  return v.family === 'development' ? `${base} in ${year}` : base;
}

/** The plural noun of one side of a signed measure ("net importers", "falling"). */
function signNoun(v: VarInfo, negative: boolean): string {
  if (v.id === 'trade.net' || v.id === 'consumption.balance') return negative ? 'net importers' : 'net exporters';
  return negative ? 'with a fall' : 'with a rise';
}

function positions(model: SectorModel, facts: FactsIndex, cell: ItemInfo, g: GeoInfo, eu: 'members' | 'bloc'): Position[] {
  const out: Position[] = [];
  for (const v of countryVars(model, cell)) {
    const it = measureItem(model, cell, v) ?? cell;
    const year = baseYear(model, facts, v, it, eu);
    if (year === null) continue;
    const sl = slice(model, facts, v, it.idx, year, eu);
    const c = rankSlice(model, v, sl.rows, sl.world);
    const row = sl.rows.find((r) => r.geo === g.idx) ?? null;
    const st = stateOf(row, g, v, model.rosterOf(v), year);
    const pos = c.rows.find((r) => r.geo === g.idx);
    if (row && Number.isFinite(row.value)) {
      const signed = isDiverging(v) || sl.rows.some((r) => Number.isFinite(r.value) && r.value < 0);
      let rankText: string | null = null;
      let rank: number | null = null;
      let n = c.n;
      if (signed) {
        // a signed measure (net trade, balances, changes) is ranked within its sign: "#1 of 178 net importers"
        const same = sl.rows.filter((r) => model.geos[r.geo]?.kind === 'country' && Number.isFinite(r.value) && r.value !== 0 && Math.sign(r.value) === Math.sign(row.value));
        if (row.value !== 0 && same.length) {
          rank = 1 + same.filter((r) => Math.abs(r.value) > Math.abs(row.value)).length;
          n = same.length;
          rankText = `#${rank} of ${n} ${signNoun(v, row.value < 0)}`;
        }
      } else if (pos && row.value > 0 && pos.rank <= c.n) {
        // ranked among the holders with a value > 0 (a zero has no rank: it would read "#208 of 198")
        rank = pos.rank;
        rankText = `#${rank} of ${c.n}`;
      }
      out.push({
        v,
        year,
        text: `${fmtFact(model, v, it, row.value)}${st.assumed ? ` ${ASSUMED_MARK}` : ''}`,
        sub: [!signed && pos?.share !== null && pos?.share !== undefined ? `${fmtShare(pos.share)} ${basisWords(c.basis, v.source.short)}` : null, rankText, factYearText(v, row, year), noteLine(model, v.id, row) || null].filter(Boolean).join(' · '),
        rank,
        n,
        signed,
      });
    } else {
      out.push({ v, year, text: st.state === 'zero' ? 'Reported zero' : '–', sub: [stateWords(st, v, model, g), noteLine(model, v.id, row) || null].filter(Boolean).join(' · '), rank: null, n: c.n });
    }
  }
  // region-native measures: the economy's region value
  for (const v of model.vars.filter((x) => x.native_level === 'region' && model.availOf(x, cell.idx).length)) {
    const region = regionOf(model, g);
    const year = facts.latestYear(v.idx, cell.idx);
    if (!region || year === null) continue;
    const r = facts.value(v.idx, cell.idx, year, region.idx);
    if (!r || !Number.isFinite(r.value)) continue;
    out.push({ v, year, text: `${region.name} ${fmtFact(model, v, cell, r.value)}`, sub: `Regional value (${v.source.short}, ${year}): not this economy's own`, rank: null, n: 0 });
  }
  return out;
}

function TradePartners({ trade, cell, g, eu }: { trade: SectorTradeChunk; cell: ItemInfo; g: GeoInfo; eu: 'members' | 'bloc' }) {
  const model = useModel();
  const flows = (y: TradeYear) => {
    const raw = flowsOf(trade, cell.idx, y);
    return eu === 'bloc' ? blocFlows(raw, model) : raw;
  };
  const f24 = flows(2024);
  const imp = partners(f24, g.idx, 'imports').slice(0, 10);
  const exp = partners(f24, g.idx, 'exports').slice(0, 10);
  const mix = (y: TradeYear) => supplierMixes(flows(y), model.geos, new Set([g.idx]))[0] ?? null;
  const m20 = mix(2020);
  const m24 = mix(2024);
  const rowsOf = (ps: ReturnType<typeof partners>): LeagueRow[] => ps.map((p) => ({ id: model.geos[p.geo].code, label: model.geos[p.geo].name, group: markGroup(model.geos[p.geo]), value: p.value, display: `${fmtUsd(p.value)} · ${fmtShare(p.share)}`, badge: p.mirror ? 'mirror' : undefined }));
  const dumb: DumbbellRow[] = (['china', 'core', 'partner', 'rest'] as const).map((k) => ({ id: k, label: GROUP_LABEL[k], a: m20?.parts[k] ?? NaN, b: m24?.parts[k] ?? NaN, group: k, aLabel: `${fmtShare(m20?.parts[k])} (2020)`, bLabel: `${fmtShare(m24?.parts[k])} (2024)` }));
  const vFlow = model.varById.get('trade.flow');
  const twin = (ps: ReturnType<typeof partners>, caption: string, who: string) => () => (
    <TableView
      rows={ps}
      caption={caption}
      rowKey={(p) => model.geos[p.geo].code}
      cols={[
        { id: 'p', header: who, value: (p) => model.geos[p.geo].name },
        { id: 'v', header: 'Value (USD)', value: (p) => p.value, cell: (p) => fmtUsd(p.value, 'table'), num: true },
        { id: 's', header: 'Share', value: (p) => p.share, cell: (p) => fmtShare(p.share), num: true },
        { id: 'm', header: 'Source', value: (p) => (p.mirror ? "partner mirror ×1.06" : 'importer reported') },
      ]}
    />
  );
  return (
    <div className="space-y-6">
      <div className="grid gap-6 lg:grid-cols-2">
        <SectorChartCard
          title="Where its imports come from"
          vars={[{ var: 'trade.flow', item: cell.slug, year: 2024 }]}
          subExtra={<HowToRead>Each bar is one supplier, largest first: what {g.name} bought from it in 2024 and that supplier&apos;s share of {g.name}&apos;s imports of these goods. Colours show the supplier&apos;s group. One large bar means a dependence on one source.</HowToRead>}
          table={twin(imp, `${g.name}: import sources, 2024`, 'Supplier')}
          csv={() => (vFlow ? { name: `${g.code}-${cell.slug}-import-sources`, text: sectorCsv(model, imp.map((p) => ({ geo: model.geos[p.geo], v: vFlow, item: cell, year: 2024, value: p.value, status: p.mirror ? 'mirror' : 'obs', share: p.share }))) } : null)}
          notes={m24 ? <p>China share {fmtShare(m24.china)}; <Term id="hhi">concentration</Term> of its suppliers (HHI) {m24.hhi.toFixed(3)}.</p> : undefined}
          summary={imp[0] ? `${model.geos[imp[0].geo].name} supplies ${fmtShare(imp[0].share)} of ${g.name}'s imports.` : `No imports recorded for ${g.name}.`}
        >
          {imp.length ? <LeagueBars rows={rowsOf(imp)} axisLabel="US dollars, 2024" ariaLabel={`${g.name}: import sources`} top={10} tickFormat={(v) => fmtUsd(v).replace('USD ', '')} /> : <p className="text-sm text-paper-mute">No imports recorded.</p>}
        </SectorChartCard>
        <SectorChartCard
          title="Where its exports go"
          vars={[{ var: 'trade.flow', item: cell.slug, year: 2024 }]}
          subExtra={<HowToRead>Each bar is one buyer, largest first: what it bought from {g.name} in 2024 and its share of {g.name}&apos;s exports of these goods. Colours show the buyer&apos;s group.</HowToRead>}
          table={twin(exp, `${g.name}: export destinations, 2024`, 'Destination')}
          csv={() => (vFlow ? { name: `${g.code}-${cell.slug}-export-destinations`, text: sectorCsv(model, exp.map((p) => ({ geo: model.geos[p.geo], v: vFlow, item: cell, year: 2024, value: p.value, status: p.mirror ? 'mirror' : 'obs', share: p.share }))) } : null)}
          summary={exp[0] ? `${model.geos[exp[0].geo].name} takes ${fmtShare(exp[0].share)} of ${g.name}'s exports.` : `No exports recorded for ${g.name}.`}
        >
          {exp.length ? <LeagueBars rows={rowsOf(exp)} axisLabel="US dollars, 2024" ariaLabel={`${g.name}: export destinations`} top={10} tickFormat={(v) => fmtUsd(v).replace('USD ', '')} /> : <p className="text-sm text-paper-mute">No exports recorded.</p>}
        </SectorChartCard>
      </div>
      {(m20 || m24) && (
        <SectorChartCard
          title="Its imports by supplier group, 2020 and 2024"
          vars={[{ var: 'trade.flow', item: cell.slug, year: 2020 }, { var: 'trade.flow', item: cell.slug, year: 2024 }]}
          subExtra={<HowToRead>Each row is one group of suppliers; its two dots are that group&apos;s share of {g.name}&apos;s imports in 2020 and in 2024. A line running to the right means the group supplies a larger share than before.</HowToRead>}
          table={() => <TableView rows={dumb} caption="Import shares by supplier group" cols={[{ id: 'g', header: 'Supplier group', value: (d) => d.label }, { id: 'a', header: '2020', value: (d) => d.a, cell: (d) => fmtShare(d.a), num: true }, { id: 'b', header: '2024', value: (d) => d.b, cell: (d) => fmtShare(d.b), num: true }]} />}
          csv={() => ({ name: `${g.code}-${cell.slug}-supplier-groups`, text: rowsToCsv(dumb.map((d) => ({ iso3c: g.code, economy: g.name, item: cell.slug, supplier_group: d.id, share_2020: Number.isFinite(d.a) ? d.a : null, share_2024: Number.isFinite(d.b) ? d.b : null, var_id: 'trade.flow', source: vFlow?.source.short ?? 'UN Comtrade' }))) })}
          summary={`China supplied ${fmtShare(m20?.china)} of ${g.name}'s imports in 2020 and ${fmtShare(m24?.china)} in 2024.`}>
          <Dumbbell rows={dumb} xLabel="Share of its imports" aName="2020" bName="2024" ariaLabel="Import shares by supplier group" tickFormat={(x) => `${Math.round(x * 100)}%`} />
        </SectorChartCard>
      )}
    </div>
  );
}

/** The CM-FDI section (mounts, and loads sector-fdi, near the viewport). */
function CrossBorder({ cell, g, tech, stage }: { cell: ItemInfo; g: GeoInfo; tech: string; stage: string }) {
  const model = useModel();
  const { link } = useSectorUrl();
  const [ref, near] = useNear<HTMLElement>(600);
  return (
    <section ref={ref} id="investment-projects" aria-labelledby="fdi-h">
      <h2 id="fdi-h" className="rule-neis-sm mb-4 text-xl font-medium text-paper">
        Cross-border projects
      </h2>
      {near ? <FdiCountry model={model} cell={cell} g={g} geoHref={(x) => (x.kind === 'country' || x.kind === 'eu' ? link(`/sector/${tech}/${stage}/${x.code}`) : null)} /> : <div style={{ minHeight: 240 }} aria-hidden="true" />}
    </section>
  );
}

/** Aid and investment partners (sector-dev; mounts, and loads it, near the viewport). */
function DevPartnersSection({ cell, g }: { cell: ItemInfo; g: GeoInfo }) {
  const model = useModel();
  const [ref, near] = useNear<HTMLElement>(600);
  if (!model.vars.some((v) => v.id === 'development.fdi_out' || v.id === 'development.oda')) return null;
  return (
    <section ref={ref} id="aid-investment" aria-labelledby="dev-h">
      <h2 id="dev-h" className="rule-neis-sm mb-4 text-xl font-medium text-paper">
        Aid and investment partners
      </h2>
      {near ? <DevCountry model={model} cell={cell} g={g} /> : <div style={{ minHeight: 240 }} aria-hidden="true" />}
    </section>
  );
}

function OverTime({ cell, g, facts }: { cell: ItemInfo; g: GeoInfo; facts: FactsIndex }) {
  const model = useModel();
  // series-chunk measures of the cell (EI production / generation / consumption 1965-2025, CIM power
  // additions, IEA EV regions): the economy's own line, read from sector-series
  const seriesVars = varsOfCell(model, cell, 'series').filter((v) => v.native_level === 'country');
  const { series } = useSeries(seriesVars.length > 0);
  const entries: { v: VarInfo; it: ItemInfo; pts: { year: number; value: number; status: string }[] }[] = [];
  const seen = new Set<string>();
  for (const v of seriesVars) {
    const it = measureItem(model, cell, v, 'series');
    if (!it || !series) continue;
    const pts = series.seriesOf(v.idx, it.idx, g.idx).filter((p) => Number.isFinite(p.value));
    if (pts.length > 2) {
      entries.push({ v, it, pts });
      seen.add(v.id);
    }
  }
  for (const v of countryVars(model, cell)) {
    if (seen.has(v.id)) continue;
    const it = measureItem(model, cell, v) ?? cell;
    if (facts.yearsOf(v.idx, it.idx).length <= 2) continue;
    const pts = facts.seriesOf(v.idx, it.idx, g.idx).filter((p) => Number.isFinite(p.value));
    if (pts.length >= 2) entries.push({ v, it, pts });
  }
  if (!entries.length) return <p className="text-sm text-paper-mute">No series for {g.name} in this sector.</p>;
  return (
    <div className="grid gap-6 lg:grid-cols-2">
      {entries.map(({ v, it, pts }) => {
        const u = model.unitOf(v, it.idx);
        const y0 = pts[0].year;
        const y1 = pts[pts.length - 1].year;
        return (
          <SectorChartCard
            key={v.id}
            title={`${v.short}${it !== cell ? ` (${lc(it.label)})` : ''}, ${y0}-${y1}`}
            vars={[{ var: v.id, item: it.slug, window: [y0, y1] }]}
            subExtra={<HowToRead>The line follows {g.name}&apos;s {lc(v.short)} year by year, in the source&apos;s own unit; higher means more.</HowToRead>}
            table={() => <TableView rows={pts} caption={`${g.name}: ${v.short}`} cols={[{ id: 'y', header: 'Year', value: (p) => p.year }, { id: 'v', header: `${v.short} (${u.short})`, value: (p) => p.value, cell: (p) => fmtFact(model, v, it, p.value, 'table'), num: true }, { id: 's', header: 'State', value: (p) => (p.status === 'est' ? 'estimate' : p.status === 'obs' ? 'observed' : p.status) }]} />}
            csv={() => ({ name: `${g.code}-${v.id}-${it.slug}-series`, text: sectorCsv(model, pts.map((p) => ({ geo: g, v, item: it, year: p.year, value: p.value, status: p.status }))) })}
            summary={`${g.name}: ${lc(v.short)} from ${fmtFact(model, v, it, pts[0].value)} in ${y0} to ${fmtFact(model, v, it, pts[pts.length - 1].value)} in ${y1}.`}
          >
            <LabelledLines lines={[{ id: g.code, label: g.name, group: partOf(g) === 'unattributed' ? 'neutral' : partOf(g), points: pts.map((p) => ({ x: p.year, y: p.value, est: p.status === 'est' })) }]} highlight={g.code} yLabel={u.long || u.short} ariaLabel={`${g.name}: ${v.short} by year`} valueFormat={(x) => fmtFact(model, v, it, x)} height={180} />
          </SectorChartCard>
        );
      })}
    </div>
  );
}

function StageContext({ cell, g, facts }: { cell: ItemInfo; g: GeoInfo; facts: FactsIndex }) {
  const model = useModel();
  const act = contextItem(model, cell);
  const rows: { label: string; v: VarInfo; item: ItemInfo | null }[] = [];
  const w = model.varById.get('prices.wages');
  if (w && act) rows.push({ label: `Monthly earnings, ${lc(act.label)}`, v: w, item: act });
  for (const id of ['prices.lending_rate', 'prices.ppi_yoy']) {
    const v = model.varById.get(id);
    if (v && model.none) rows.push({ label: v.label, v, item: model.none });
  }
  const vals = rows.map((r) => {
    const y = r.item ? facts.latestYear(r.v.idx, r.item.idx) : null;
    const f = r.item && y !== null ? facts.value(r.v.idx, r.item.idx, y, g.idx) : null;
    return { ...r, f };
  });
  if (!vals.some((x) => x.f)) return null;
  return (
    <TableView
      rows={vals}
      caption="Stage context"
      cols={[
        { id: 'm', header: 'Measure', value: (r) => r.label },
        { id: 'v', header: 'Value', value: (r) => (r.f && Number.isFinite(r.f.value) ? fmtFact(model, r.v, r.item, r.f.value, 'table') : '–') },
        { id: 'y', header: 'Year', value: (r) => r.f?.obs_year ?? null },
        { id: 's', header: 'Source', value: (r) => r.v.source.short },
      ]}
    />
  );
}

export default function CountryInSectorView() {
  const { tech: rawT, stage: rawS, iso3: rawC } = useParams();
  const model = useModel();
  const { state, link } = useSectorUrl();
  const tech = rawT && isTechSlug(rawT) ? rawT : null;
  const stage = rawS && isStageSlug(rawS) ? rawS : null;
  const cell = tech && stage ? model.cell(tech, stage) : null;
  const code = (rawC ?? '').toUpperCase();
  const g = model.geoByCode.get(code) ?? null;
  const geo = g && (g.kind === 'country' || g.kind === 'eu') ? g : null;
  useTitle(cell && geo ? `${geo.name} in ${model.cellLabel(cell)}` : 'Sectors', 'Sector Insights');
  const { facts, h } = useFacts();
  const { h: th } = useTrade(tech, !!cell?.has_trade);
  const policy = usePolicy(!!cell);
  const pos = useMemo(() => (facts && cell && geo ? positions(model, facts, cell, geo, state.eu) : []), [facts, cell, geo, model, state.eu]);
  const beeswarm = useMemo(() => {
    if (!facts || !cell || !geo) return null;
    const pm = primaryMeasure(model, cell);
    const v = pm?.v ?? null;
    const it = pm?.item ?? cell;
    if (!v || v.native_level !== 'country') return null;
    const y = baseYear(model, facts, v, it, state.eu);
    if (y === null) return null;
    const sl = slice(model, facts, v, it.idx, y, state.eu);
    const pts = sl.rows.filter((r) => model.geos[r.geo]?.kind === 'country' && Number.isFinite(r.value) && r.value > 0);
    const sorted = pts.map((r) => r.value).sort((a, z) => a - z);
    const med = sorted.length ? sorted[Math.floor(sorted.length / 2)] : NaN;
    const coal = pts.filter((r) => ['core', 'partner'].includes(model.geos[r.geo].group)).sort((a, z) => z.value - a.value)[0];
    const points: BeeswarmPoint[] = pts.map((r) => {
      const gg = model.geos[r.geo];
      return { id: gg.code, label: gg.name, group: markGroup(gg), value: r.value, display: fmtFact(model, v, it, r.value), labelled: gg.idx === geo.idx || gg.code === 'CHN' || gg.idx === coal?.geo };
    });
    const log = sorted.length > 2 && med > 0 && sorted[sorted.length - 1] / med > 100;
    return { v, it, y, points, med, log };
  }, [facts, cell, geo, model, state.eu]);
  const loc = useLocation();

  if (!tech) return <Redirect to="/sector" notice={`No sector called '${rawT}'; showing all sectors.`} />;
  // an old three-stage link (design U1): Downstream is Deployment exactly (the economy, query and hash
  // kept); Upstream and Midstream split, so the link goes to the technology's chain page
  if (rawS && isLegacyStageSlug(rawS)) {
    const to = legacyStageTarget(rawS);
    const legacy = legacyStageOfSlug(rawS)!;
    return <Redirect to={{ pathname: to ? `/sector/${tech}/${to}/${rawC ?? ''}` : `/sector/${tech}`, search: loc.search, hash: to ? loc.hash : '' }} notice={legacyStageNote(legacy, TECH_LABELS[tech])} />;
  }
  if (!stage || !cell) return <Redirect to={`/sector/${tech}`} notice={`No stage called '${rawS}'; showing the whole ${TECH_LABELS[tech]} chain.`} />;
  if (!geo) return <Redirect to={link(`/sector/${tech}/${stage}`)} notice={`No economy '${rawC}' in the sector data; showing the sector.`} />;

  const top10 = pos.filter((p) => !p.signed && p.rank !== null && p.rank <= 10).map((p) => `${ordinal(p.rank as number)} in ${leadLabel(p.v, p.year)}`);
  const rank = top10.length ? `${geo.name} ranks ${listText(top10)} in ${model.cellLabel(cell)}.` : `${geo.name} does not rank in the top 10 on any country-level measure of ${model.cellLabel(cell)}.`;
  // the page's question first (copy refresh 2026-10-30), then the ranking; measured values only, so no strategic call here
  const lede = (
    <>
      <span className="block font-medium text-paper">{countryInSectorQuestion(geo.name, model.cellLabel(cell))}</span>
      {rank}
    </>
  );
  return (
    <ProfileProvider value={{ model, cell, tech, stage, facts, highlight: geo, eu: state.eu }}>
      <CsvRegistryProvider>
        <div className="space-y-8">
          <PageHead eyebrow={`SECTORS · ${model.cellLabel(cell).toUpperCase()}`} title={`${geo.name} in ${model.cellLabel(cell)}`} lede={lede}>
            <div className="mt-3 flex flex-wrap items-center gap-2">
              <span className="tag">{geo.tier === 'core' ? 'Coalition core' : geo.tier === 'partner' ? 'Coalition partner' : 'Outside the coalition'}</span>
              {geo.eu_member && <span className="tag">EU member</span>}
              {geo.kind === 'eu' && <span className="tag">EU-27 as one</span>}
              {geo.bloc && <span className="tag">UN vote: {geo.bloc}</span>}
              {geo.re_export_hub && <span className="tag">re-export hub</span>}
              <Link className="link ml-2 text-sm" to={link(`/sector/${tech}/${stage}`, { c: geo.code })}>
                Back to {model.cellLabel(cell)}
              </Link>
              {geo.kind === 'country' && (
                <Link className="link ml-2 text-sm" to={`/countries/${geo.code}`}>
                  {geo.name}: Country profile →
                </Link>
              )}
              {/* the strategic call lives on the Country Strategy Map, opened on this step */}
              {geo.kind === 'country' && cell.headline && (
                <Link className="link ml-2 text-sm" to={strategyHref(geo.code, TECH_LABELS[tech], STAGE_LABELS[stage])}>
                  What should {geo.name} do here? →
                </Link>
              )}
            </div>
          </PageHead>
          <ChunkGate h={h} name="sector-facts">
            {() => (
              <div className="space-y-8">
                <section aria-labelledby="pos-h">
                  <h2 id="pos-h" className="rule-neis-sm mb-4 text-xl font-medium text-paper">
                    Position
                  </h2>
                  {pos.length ? (
                    <div className="grid grid-cols-2 gap-3 md:grid-cols-3 xl:grid-cols-4" role="list" aria-label="Position tiles">
                      {pos.map((p) => (
                        <div key={p.v.id} role="listitem" title={`${p.v.label} · ${p.v.source.short} · ${p.year}`}>
                          <StatTile label={p.v.short} value={p.text} sub={p.sub} />
                        </div>
                      ))}
                    </div>
                  ) : (
                    <p className="text-sm text-paper-mute">No country-level measures for this sector.</p>
                  )}
                </section>
                {beeswarm && (
                  <SectorChartCard
                    headingLevel={2}
                    title="Rank among peers"
                    vars={[{ var: beeswarm.v.id, item: beeswarm.it.slug, year: beeswarm.y }]}
                    subExtra={<HowToRead>Each dot is one economy, placed by its {lc(beeswarm.v.short)} in {beeswarm.y}: further right is more. {geo.name} is ringed; the tick marks the median, the economy in the middle.</HowToRead>}
                    table={() => <TableView rows={[...beeswarm.points].sort((x, z) => z.value - x.value)} caption="Rank among peers" search rowKey={(p) => p.id} selectedKey={geo.code} cols={[{ id: 'r', header: 'Rank', value: (p) => [...beeswarm.points].sort((x, z) => z.value - x.value).indexOf(p) + 1, num: true }, { id: 'e', header: 'Economy', value: (p) => p.label }, { id: 'v', header: `${beeswarm.v.short} (${model.unitOf(beeswarm.v, beeswarm.it.idx).short})`, value: (p) => p.value, cell: (p) => p.display, num: true }]} />}
                    csv={() => ({ name: `${geo.code}-${cell.slug}-peers`, text: sectorCsv(model, beeswarm.points.map((p) => ({ geo: model.geoByCode.get(p.id) ?? null, v: beeswarm.v, item: beeswarm.it, year: beeswarm.y, value: p.value, status: 'obs' }))) })}
                    summary={`${geo.name} among ${beeswarm.points.length} economies with ${lc(beeswarm.v.short)} in ${beeswarm.y}.`}
                  >
                    <Beeswarm points={beeswarm.points} medians={Number.isFinite(beeswarm.med) ? [{ label: 'Median', value: beeswarm.med }] : []} log={beeswarm.log} tickFormat={beeswarm.log ? logAxisFormatter(model.unitOf(beeswarm.v, beeswarm.it.idx), beeswarm.v.display_scale) : undefined} xLabel={`${model.unitOf(beeswarm.v, beeswarm.it.idx).long}, ${beeswarm.y}${beeswarm.log ? ' (log scale)' : ''}`} ariaLabel={`Distribution of ${lc(beeswarm.v.short)}`} selected={geo.code} />
                  </SectorChartCard>
                )}
                {cell.has_trade && (
                  <section aria-labelledby="tp-h">
                    <h2 id="tp-h" className="rule-neis-sm mb-4 text-xl font-medium text-paper">
                      Trade partners
                    </h2>
                    <ChunkGate h={th} name={`sector-trade-${tech}`}>
                      {(t) => <TradePartners trade={t} cell={cell} g={geo} eu={state.eu} />}
                    </ChunkGate>
                  </section>
                )}
                {hasFdi(model, cell) && <CrossBorder key={`${cell.slug}|${geo.code}`} cell={cell} g={geo} tech={tech} stage={stage} />}
                {(geo.kind === 'country' || geo.kind === 'eu') && <DevPartnersSection key={`dev|${cell.slug}|${geo.code}`} cell={cell} g={geo} />}
                {facts && (
                  <section aria-labelledby="ot-h">
                    <h2 id="ot-h" className="rule-neis-sm mb-4 text-xl font-medium text-paper">
                      Over time
                    </h2>
                    <OverTime cell={cell} g={geo} facts={facts} />
                  </section>
                )}
                {tech !== 'semiconductors' && (
                  <section aria-labelledby="pol-h">
                    <h2 id="pol-h" className="rule-neis-sm mb-4 text-xl font-medium text-paper">
                      Policies
                    </h2>
                    <ChunkGate h={policy} name="sector-policy">
                      {(chunk) => <PolicyBody chunk={chunk} scopeGeo={geo} />}
                    </ChunkGate>
                  </section>
                )}
                {facts && (
                  <section aria-labelledby="ctx-h">
                    <h2 id="ctx-h" className="rule-neis-sm mb-4 text-xl font-medium text-paper">
                      Stage context
                    </h2>
                    <StageContext cell={cell} g={geo} facts={facts} />
                  </section>
                )}
              </div>
            )}
          </ChunkGate>
        </div>
      </CsvRegistryProvider>
    </ProfileProvider>
  );
}
