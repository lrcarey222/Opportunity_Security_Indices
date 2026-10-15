// Concentration across sectors /sector/concentration (spec_sector.md 4.7): the lens (China share | top-1
// | top-3 | HHI | coalition share; the EU switch from the SectorBar); (a) the concentration board
// (technologies with mineral sub-rows x chain-ordered columns, tiles open the profile section or the
// mineral page); (b) the share split of one family, faceted by stage, plus a minerals panel; (c) the
// table of every item x measure; (d) trends: matrix HHI 2020 -> 2024 per trade cell and IEA top-3
// base year -> projection per mineral and step. Renders from sector-meta alone. Described, never graded.

import { useMemo } from 'react';
import { useNavigate } from 'react-router';
import { TriLegend } from '../../charts/legends.tsx';
import { triScale } from '../../charts/scale.ts';
import { TableView } from '../../components/TableView.tsx';
import { Notice, PageHead, Segmented, Select } from '../../components/ui.tsx';
import { rowsToCsv } from '../../lib/csv.ts';
import { useTitle } from '../../shell/title.ts';
import { useModel } from '../data/SectorDataProvider.tsx';
import type { SectorModel, SummaryInfo } from '../data/model.ts';
import {
  boardTile, concentrationBoard, familyChoices, familySplit, latestSummaries, mineralTop3Trend, tradeHhiTrend, type Board, type BoardRowDef, type FamChoice, type TrendRow,
} from '../derive/board.ts';
import { blocNote } from '../derive/groups.ts';
import { LENS_LABEL, lensText, lensValue } from '../derive/text.ts';
import { lc, fmtEffN, fmtHhi, fmtShare } from '../format.ts';
import { LENSES, type FamKey, type Lens } from '../url.ts';
import { concFromSummary, fmtFact, HowToRead, itemLabel, SPLIT_COLUMNS, SPLIT_READING, shareSplitRow, SplitTable } from './common.tsx';
import { Dumbbell, GroupLegend, HeatTable, SectorChartCard, ShareSplitChart, type DumbbellRow, type HeatRow, type ShareSplitRow, type VarRef } from './kit.tsx';
import { useSectorUrl } from './state.tsx';
import { HhiExplainer } from './HhiExplainer.tsx';
import { Term } from '../../components/Term.tsx';
import { yearText } from './caption.ts';

/** The year words of a tile: the year, or the dictionary's static / unconfirmed year (never the processed Year of a static dataset). */
const tileYear = (v: import('../data/model.ts').VarInfo, item: { slug: string }, year: number): string => yearText(v, { var: v.id, item: item.slug, year });

export const CONCENTRATION_NOTE = 'Compared within one measure family; values come from different sources and years and are never added.';

const FAM_WORDS: Record<FamChoice, string> = {
  primary: 'Primary measure',
  exports: 'Exports',
  investment: 'Investment',
  production: 'Production',
  capacity: 'Manufacturing capacity',
  reserves: 'Reserves',
  consumption: 'Consumption',
  prices: 'Prices',
  policy: 'Policy',
  technology: 'Technology',
  context: 'Context',
  development: 'Development',
};

const LENS_SHORT: Record<Lens, string> = { china: 'China share', top1: 'Largest holder', top3: 'Top three', hhi: 'Concentration (HHI)', coalition: 'Coalition share' };

/** The lens value as printed in a tile ("43.7%", "0.207", "≥ 0.21"). */
function lensDisplay(lens: Lens, s: SummaryInfo): string {
  if (lens === 'hhi') return fmtHhi(s.hhi, s.hhi_lower_bound);
  return fmtShare(lensValue(lens, s));
}

/** "CHN 74%"; an entity or region by its name ("China (IEA) 76%"), never its internal code ("E:iea_etp:china"). */
function topHolder(model: SectorModel, s: SummaryInfo): string {
  const g = s.top1_geo !== null ? model.geos[s.top1_geo] : null;
  const who = g ? (g.kind === 'country' || g.kind === 'eu' ? g.code : g.name) : '';
  return g && Number.isFinite(s.top1_share) ? `${who} ${Math.round(s.top1_share * 100)}%` : '';
}

function tileWords(model: SectorModel, board: Board, row: string, rowLabel: string, colKey: string, lens: Lens): string {
  const col = board.cols.find((c) => c.key === colKey);
  const t = boardTile(board, row, colKey);
  if (!t) return `${rowLabel}, ${col?.label ?? colKey}: no data`;
  const s = t.s;
  return `${rowLabel}, ${col?.label ?? colKey}: ${lc(t.v.short)}${t.item.axis === 'cell' ? '' : ` of ${lc(t.item.label)}`}, ${t.v.source.short} ${tileYear(t.v, t.item, s.year)}. ${lensText(lens, s, model)}; largest holder ${model.geoName(s.top1_geo) || '–'} ${fmtShare(s.top1_share)}; top three ${fmtShare(s.top3_share)}; concentration index ${fmtHhi(s.hhi, s.hhi_lower_bound)}; coalition ${fmtShare(s.core + s.partner)}.`;
}

function BoardCard({ board, lens }: { board: Board; lens: Lens }) {
  const model = useModel();
  const { link } = useSectorUrl();
  const navigate = useNavigate();
  const heatRows: HeatRow[] = board.rows.map((r) => ({ key: r.key, label: r.label, children: r.children.map((c) => ({ key: c.key, label: c.label })) }));
  const flat: BoardRowDef[] = board.rows.flatMap((r) => [r, ...r.children]);
  const refs = useMemo(() => {
    const seen = new Map<string, VarRef>();
    for (const t of board.tiles.values()) if (!seen.has(t.v.id)) seen.set(t.v.id, { var: t.v.id });
    return [...seen.values()];
  }, [board]);
  // the three-colour scale over the lens values of every tile on the board (sub-rows included, so
  // opening a row never repaints the others): white = their median (spec_country.md C1.5)
  const scale = useMemo(() => triScale([...board.tiles.values()].map((t) => lensValue(lens, t.s))), [board, lens]);
  const label = (key: string) => {
    const r = flat.find((x) => x.key === key);
    return r ? (r.mineral ? `${r.label} (${board.rows.find((p) => p.key === r.tech)?.label ?? ''})` : r.label) : key;
  };
  return (
    <SectorChartCard
      headingLevel={2}
      title="Concentration board"
      vars={refs}
      caption={{ badge: 'latest year of each source', sub: `${refs.length} measures from ${new Set([...board.tiles.values()].map((t) => t.v.source.id)).size} sources. Each tile is one measure of one sector in its latest year, coloured by its ${lc(LENS_LABEL[lens])}: orange lower than most tiles, white the middle one, blue higher. The second line names the largest holder. Rows open to the minerals each technology uses; a tile opens the profile section or the mineral page.`, caveats: [], spanNote: null, coverage: null }}
      table={() => (
        <TableView
          rows={flat}
          caption="Concentration board"
          rowKey={(r) => r.key}
          pageSize={40}
          cols={[
            { id: 'row', header: 'Technology or mineral', value: (r) => label(r.key), minWidth: 170 },
            ...board.cols.map((c) => ({
              id: c.key,
              header: c.label,
              value: (r: BoardRowDef) => {
                const t = boardTile(board, r.key, c.key);
                return t ? lensValue(lens, t.s) : null;
              },
              cell: (r: BoardRowDef) => {
                const t = boardTile(board, r.key, c.key);
                return t ? `${lensDisplay(lens, t.s)} · ${topHolder(model, t.s)} · ${tileYear(t.v, t.item, t.s.year)}` : '–';
              },
              num: true,
            })),
          ]}
        />
      )}
      csv={() => ({
        name: `concentration-board-${lens}-${board.tiles.values().next().value?.s.eu_mode ?? 'members'}`,
        text: rowsToCsv(
          [...board.tiles.entries()].map(([k, t]) => {
            const [row, col] = k.split('|');
            return {
              row: label(row),
              column: board.cols.find((c) => c.key === col)?.label ?? col,
              var_id: t.v.id,
              item: t.item.slug,
              year: t.s.year,
              eu_mode: t.s.eu_mode,
              lens,
              lens_value: lensValue(lens, t.s),
              top1: model.geos[t.s.top1_geo ?? -1]?.code ?? null,
              top1_share: t.s.top1_share,
              top3_share: t.s.top3_share,
              hhi: t.s.hhi,
              hhi_lower_bound: t.s.hhi_lower_bound,
              china: t.s.china,
              coalition: t.s.core + t.s.partner,
              source: t.v.source.short,
            };
          }),
        ),
      })}
      legend={
        <div className="space-y-2">
          <TriLegend scale={scale} label={`Tile colour: ${LENS_LABEL[lens]} of each measure`} format={(x) => (lens === 'hhi' ? x.toFixed(2) : `${Math.round(x * 100)}%`)} noun="tiles" lowWord={lens === 'hhi' ? 'more spread out' : 'lower'} highWord={lens === 'hhi' ? 'more concentrated' : 'higher'} missing="No data for this measure" />
          <p className="text-xs text-paper-mute">Hatched: no data for this measure. "≥": the concentration index is a lower bound, because the source does not say who holds part of the world total.</p>
        </div>
      }
      summary={`${board.tiles.size} measures across ${board.rows.length} technologies and their minerals, shaded by ${lc(LENS_LABEL[lens])}.`}
    >
      <HeatTable
        scale={scale}
        rows={heatRows}
        cols={board.cols.map((c) => ({ key: c.key, label: c.label, stage: c.stage }))}
        rowHeader="Technology"
        caption={`Concentration board: ${LENS_LABEL[lens]} by technology and chain step`}
        cell={(row, col) => {
          const t = boardTile(board, row, col);
          const title = tileWords(model, board, row, label(row), col, lens);
          if (!t) return { value: null, display: '', title, disabled: true };
          return { value: lensValue(lens, t.s), display: lensDisplay(lens, t.s), sub: topHolder(model, t.s), title };
        }}
        onCell={(row, col) => {
          const t = boardTile(board, row, col);
          if (t) navigate(link(t.path, {}, t.hash || undefined));
        }}
      />
    </SectorChartCard>
  );
}

const SECTIONS = [
  { key: 'upstream', label: 'Upstream', stage: 'upstream' as const },
  { key: 'midstream', label: 'Midstream', stage: 'midstream' as const },
  { key: 'downstream', label: 'Downstream', stage: 'downstream' as const },
  { key: 'minerals', label: 'Minerals: mining and refining (IEA)', stage: null },
];

function SplitCard({ lens }: { lens: Lens }) {
  const model = useModel();
  const { state, set, link } = useSectorUrl();
  const navigate = useNavigate();
  const choices = useMemo(() => familyChoices(model, state.eu), [model, state.eu]);
  const fam: FamChoice = (choices as string[]).includes(state.fam) ? state.fam : 'primary';
  const entries = useMemo(() => familySplit(model, fam, lens, state.eu), [model, fam, lens, state.eu]);
  if (!entries.length) return null;
  const rows: ShareSplitRow[] = entries.map((e) => {
    const isCell = e.of.axis === 'cell';
    const lbl = isCell ? model.cellLabel(e.of) : `${e.of.label} ${e.words ?? ''}`.trim();
    const href = isCell ? link(`/sector/${e.of.techSlug}/${e.of.stageSlug}`, {}, e.v.section ?? undefined) : link(`/sector/minerals/${e.of.slug}`, {}, 'mining-refining');
    return shareSplitRow(model, e.v, e.item, concFromSummary(e.s), {
      id: `${e.of.slug}|${e.v.id}`,
      label: lbl,
      sub: `${e.v.short}${isCell && e.item !== e.of ? ` (${lc(e.item.label)})` : ''} · ${e.v.source.short} ${tileYear(e.v, e.item, e.s.year)}`,
      year: e.s.year,
      href,
      section: e.section,
    });
  });
  const refs = [...new Map(entries.map((e) => [e.v.id, { var: e.v.id } as VarRef])).values()];
  const sections = SECTIONS.filter((s) => rows.some((r) => r.section === s.key));
  return (
    <SectorChartCard
      headingLevel={2}
      title="Share split by sector"
      vars={refs}
      subExtra={<HowToRead>{SPLIT_READING} Click a row for the sector’s profile.</HowToRead>}
      table={() => <SplitTable rows={rows} caption="Share split by sector" />}
      tools={<Select label="Measure family" value={fam} options={choices.map((f) => ({ value: f, label: FAM_WORDS[f] }))} onChange={(f) => set({ fam: f as FamKey })} className="min-w-[11rem]" />}
      caption={{ badge: 'latest year of each source', sub: `One row per sector: ${lc(FAM_WORDS[fam])}, the latest year of its source (named in each row), sorted by ${lc(LENS_LABEL[lens])} within each stage.`, caveats: [], spanNote: null, coverage: null }}
      csv={() => ({ name: `concentration-split-${fam}-${state.eu}`, text: rowsToCsv(entries.map((e) => ({ item: e.of.slug, measure_item: e.item.slug, section: e.section, var_id: e.v.id, year: e.s.year, eu_mode: e.s.eu_mode, china: e.s.china, core: e.s.core, partner: e.s.partner, rest: e.s.rest, unattributed: e.s.unattributed, top1: model.geos[e.s.top1_geo ?? -1]?.code ?? null, top1_share: e.s.top1_share, top3_share: e.s.top3_share, hhi: e.s.hhi, hhi_lower_bound: e.s.hhi_lower_bound, eff_n: e.s.eff_n, source: e.v.source.short }))) })}
      legend={<GroupLegend euBloc={state.eu === 'bloc'} />}
      notes={state.eu === 'bloc' ? <p>{blocNote(model)}</p> : undefined}
      summary={`${rows.length} rows of group shares in ${sections.length} panels, sorted by ${lc(LENS_LABEL[lens])}.`}
    >
      <ShareSplitChart rows={rows} columns={SPLIT_COLUMNS} sections={sections} ariaLabel="Share split by sector" onRowClick={(r) => r.href && navigate(r.href)} />
    </SectorChartCard>
  );
}

function SummaryTable() {
  const model = useModel();
  const { state } = useSectorUrl();
  const rows = useMemo(() => latestSummaries(model, state.eu), [model, state.eu]);
  if (!rows.length) return null;
  const v = (s: SummaryInfo) => model.vars[s.var];
  const it = (s: SummaryInfo) => model.items[s.item];
  return (
    <SectorChartCard
      headingLevel={2}
      title="Every sector and measure"
      vars={[...new Map(rows.map((s) => [v(s).id, { var: v(s).id } as VarRef])).values()]}
      caption={{ badge: null, sub: `${rows.length} sector and measure pairs, each in the latest year of its source (Year and Source columns).`, caveats: [], spanNote: null, coverage: null }}
      subExtra={<HowToRead>Each row is one measure of one sector: the world total, the largest holder and its share, the share of the three largest, the concentration index with the number of equal-sized holders it behaves like, and the group shares. Sort by any column to find the most concentrated supply chains.</HowToRead>}
      csv={() => ({
        name: `concentration-table-${state.eu}`,
        text: rowsToCsv(rows.map((s) => ({ item: it(s).slug, item_axis: it(s).axis, family: v(s).family, var_id: v(s).id, measure: v(s).label, year: s.year, eu_mode: s.eu_mode, world_total: s.W * v(s).display_scale, unit: model.unitOf(v(s), s.item).short, world_basis: s.basis, n: s.n, top1: model.geos[s.top1_geo ?? -1]?.code ?? null, top1_share: s.top1_share, top3_share: s.top3_share, hhi: s.hhi, hhi_lower_bound: s.hhi_lower_bound, eff_n: s.eff_n, china: s.china, core: s.core, partner: s.partner, rest: s.rest, unattributed: s.unattributed, source: v(s).source.short }))),
      })}
      summary={`${rows.length} measures with their concentration statistics.`}
    >
      <TableView
        rows={rows}
        caption="Concentration of every sector and measure"
        search
        pageSize={20}
        rowKey={(s) => `${s.item}|${s.var}`}
        initialSort={{ id: 'top1s', desc: true }}
        cols={[
          { id: 'item', header: 'Sector or item', value: (s) => itemLabel(model, it(s)), minWidth: 170 },
          { id: 'm', header: 'Measure', value: (s) => v(s).short },
          { id: 'n', header: 'N', value: (s) => s.n, num: true },
          { id: 'top1', header: 'Top-1', value: (s) => model.geoName(s.top1_geo) || null },
          { id: 'top1s', header: 'Top-1 share', value: (s) => s.top1_share, cell: (s) => fmtShare(s.top1_share), num: true },
          { id: 'top3', header: 'Top-3', value: (s) => s.top3_share, cell: (s) => fmtShare(s.top3_share), num: true },
          { id: 'hhi', header: 'Concentration (HHI)', value: (s) => s.hhi, cell: (s) => fmtHhi(s.hhi, s.hhi_lower_bound), num: true },
          { id: 'effn', header: 'Effective n', value: (s) => s.eff_n, cell: (s) => fmtEffN(s.eff_n), num: true },
          { id: 'china', header: 'China', value: (s) => s.china, cell: (s) => fmtShare(s.china), num: true },
          { id: 'core', header: 'Core', value: (s) => s.core, cell: (s) => fmtShare(s.core), num: true },
          { id: 'partner', header: 'Partners', value: (s) => s.partner, cell: (s) => fmtShare(s.partner), num: true },
          { id: 'rest', header: 'Rest', value: (s) => s.rest, cell: (s) => fmtShare(s.rest), num: true },
          { id: 'un', header: 'Not attributed', value: (s) => s.unattributed, cell: (s) => fmtShare(s.unattributed), num: true },
          { id: 'y', header: 'Year', value: (s) => s.year, num: true },
          { id: 'w', header: 'World total', value: (s) => s.W, cell: (s) => fmtFact(model, v(s), it(s), s.W, 'table'), num: true },
          { id: 'src', header: 'Source', value: (s) => v(s).source.short },
        ]}
      />
    </SectorChartCard>
  );
}

interface TrendProps {
  rows: TrendRow[];
  title: string;
  metric: 'hhi' | 'top3';
  refs: VarRef[];
  aName: string;
  bName: string;
  xLabel: string;
  notes?: string;
  hrefOf: (r: TrendRow) => string;
  words: string;
}

function TrendCard({ rows, title, metric, refs, aName, bName, xLabel, notes, hrefOf, words }: TrendProps) {
  const navigate = useNavigate();
  if (!rows.length) return null;
  const val = (s: SummaryInfo) => (metric === 'hhi' ? s.hhi : s.top3_share);
  const txt = (s: SummaryInfo) => (metric === 'hhi' ? `concentration index ${fmtHhi(s.hhi, s.hhi_lower_bound)}` : `top three ${fmtShare(s.top3_share)}`);
  const proj = (r: TrendRow, s: SummaryInfo) => r.v.projection_from !== null && s.year >= r.v.projection_from;
  const dumb: DumbbellRow[] = rows.map((r) => ({ id: `${r.item.slug}|${r.v.id}`, label: r.words, a: val(r.a), b: val(r.b), group: 'neutral', aLabel: `${txt(r.a)} (${r.a.year}${proj(r, r.a) ? ', projection' : ''})`, bLabel: `${txt(r.b)} (${r.b.year})`, href: hrefOf(r) }));
  const higher = rows.filter((r) => val(r.b) > val(r.a)).length;
  return (
    <SectorChartCard
      title={title}
      vars={refs}
      subExtra={<HowToRead>Each row is one sector; its two dots are the {metric === 'hhi' ? 'concentration index' : 'share of the three largest holders'} in {aName} and in {bName}. A line running to the right means supply became more concentrated; to the left, more spread out. Click a row for the sector.</HowToRead>}
      table={() => (
        <TableView
          rows={rows}
          caption={title}
          cols={[
            { id: 'r', header: 'Sector', value: (r) => r.words },
            { id: 'a', header: aName, value: (r) => val(r.a), cell: (r) => (metric === 'hhi' ? fmtHhi(r.a.hhi, r.a.hhi_lower_bound) : fmtShare(r.a.top3_share)), num: true },
            { id: 'b', header: bName, value: (r) => val(r.b), cell: (r) => (metric === 'hhi' ? fmtHhi(r.b.hhi, r.b.hhi_lower_bound) : fmtShare(r.b.top3_share)), num: true },
          ]}
        />
      )}
      csv={() => ({ name: `concentration-trend-${metric}`, text: rowsToCsv(rows.map((r) => ({ item: r.item.slug, var_id: r.v.id, year_a: r.a.year, value_a: val(r.a), year_b: r.b.year, value_b: val(r.b), metric, eu_mode: r.a.eu_mode, source: r.v.source.short }))) })}
      notes={notes ? <p>{notes}</p> : undefined}
      summary={`${rows.length} ${words}; in ${higher} of them the ${metric === 'hhi' ? 'concentration index' : 'top-three share'} is higher in ${rows[0].b.year} than in ${rows[0].a.year}.`}
    >
      <Dumbbell rows={dumb} xLabel={xLabel} aName={aName} bName={bName} ariaLabel={title} tickFormat={(x) => (metric === 'hhi' ? x.toFixed(2) : `${Math.round(x * 100)}%`)} onSelect={(r) => r.href && navigate(r.href)} />
    </SectorChartCard>
  );
}

function Trends() {
  const model = useModel();
  const { state, link } = useSectorUrl();
  const trade = useMemo(() => tradeHhiTrend(model, state.eu), [model, state.eu]);
  const minerals = useMemo(() => mineralTop3Trend(model, state.eu), [model, state.eu]);
  if (!trade.length && !minerals.length) return null;
  const projYear = minerals[0]?.a.year;
  const baseYear = minerals[0]?.b.year;
  return (
    <section aria-labelledby="trend-h" className="space-y-4">
      <h2 id="trend-h" className="rule-neis-sm text-xl font-medium text-paper">
        Trend
      </h2>
      <div className="grid gap-6 xl:grid-cols-2">
        <TrendCard
          rows={trade}
          title="Exporter concentration, 2020 and 2024"
          metric="hhi"
          refs={[{ var: 'trade.exports', year: 2020 }, { var: 'trade.exports', year: 2024 }]}
          aName="2020"
          bName="2024"
          xLabel="Concentration (HHI) of exporters, reconciled matrix"
          notes="Nominal US dollars; 2020 was a COVID trough."
          hrefOf={(r) => link(`/sector/${r.item.techSlug}/${r.item.stageSlug}`, {}, 'trade')}
          words="trade cells"
        />
        <TrendCard
          rows={minerals}
          title={`Top-3 share of mining and refining, ${baseYear ?? ''} and ${projYear ?? ''} (projection)`}
          metric="top3"
          refs={[{ var: 'production.mining' }, { var: 'production.refining' }]}
          aName={`${projYear ?? ''} (projection)`}
          bName={String(baseYear ?? '')}
          xLabel="Top-3 share of world output (IEA base case)"
          notes="IEA lists the six largest producers; the projection is the IEA base case."
          hrefOf={(r) => link(`/sector/minerals/${r.item.slug}`, {}, 'mining-refining')}
          words="mineral steps"
        />
      </div>
    </section>
  );
}

export default function ConcentrationView() {
  const model = useModel();
  const { state, set } = useSectorUrl();
  useTitle('Concentration', 'Sector Insights');
  const lens = state.lens;
  const board = useMemo(() => concentrationBoard(model, state.eu), [model, state.eu]);
  return (
    <div className="space-y-8">
      <PageHead
        eyebrow="SECTOR INSIGHTS · CONCENTRATION"
        title="Concentration across sectors"
        lede={
          <>
            How concentrated each supply chain is, from reserves to deployment, one measure at a time: the share of the largest holder, of the top three, and the <Term id="hhi">concentration index (HHI)</Term>, which is 1 when one economy holds everything and falls as supply spreads out. The shares held by China and by the allied coalition are shown beside them. Pick a lens: the board colours every sector by it.
          </>
        }
      >
        <div className="mt-4 flex flex-wrap items-end gap-4">
          <Segmented<Lens> label="Lens" value={lens} options={LENSES.map((l) => ({ value: l, label: LENS_SHORT[l] }))} onChange={(l) => set({ lens: l })} />
        </div>
      </PageHead>
      <HhiExplainer model={model} noun="holders" of="the world total" />
      <p className="text-sm text-paper-dim">{CONCENTRATION_NOTE}</p>
      {state.eu === 'bloc' && <Notice kind="info">EU-27 as one raises measured concentration where members are large.</Notice>}
      {board.tiles.size > 0 ? <BoardCard board={board} lens={lens} /> : <Notice kind="info">No concentration statistics in this build.</Notice>}
      <SplitCard lens={lens} />
      <SummaryTable />
      <Trends />
      <p className="text-xs text-paper-mute">No thresholds are drawn on the charts: concentration is described, not graded.</p>
    </div>
  );
}
