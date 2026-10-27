// Minerals index /sector/minerals (spec_sector.md 4.5): a tile grid as in the Sector home strip, the
// group split of mining and refining by mineral (IEA base year), and the table of every mineral: USGS
// mine production, IEA top-3 mining and refining, China and coalition shares, and the technologies that
// use it. Renders from sector-meta alone (meta.summary, dims.mineral_demand, item links).

import { useMemo } from 'react';
import { Link, useNavigate } from 'react-router';
import { TableView } from '../../../components/TableView.tsx';
import { PageHead } from '../../../components/ui.tsx';
import { Term } from '../../../components/Term.tsx';
import { rowsToCsv } from '../../../lib/csv.ts';
import { useTitle } from '../../../shell/title.ts';
import { useModel } from '../../data/SectorDataProvider.tsx';
import type { SectorModel, SummaryInfo, VarInfo } from '../../data/model.ts';
import { MINE_VAR, MINING_VAR, mineralIndexRows, REFINING_VAR, type MineralIndexRow } from '../../derive/minerals.ts';
import { lensText } from '../../derive/text.ts';
import { lc, fmtShare, listText } from '../../format.ts';
import { concFromSummary, fmtFact, HowToRead, SPLIT_COLUMNS, SPLIT_READING, shareSplitRow, SplitTable } from '../common.tsx';
import { GroupLegend, SectorChartCard, ShareSplitChart, type ShareSplitRow, type VarRef } from '../kit.tsx';
import { MicroBar } from '../SectorHomeView.tsx';
import { useSectorUrl } from '../state.tsx';

const coalition = (s: SummaryInfo | null): number => (s ? s.core + s.partner : NaN);
const top1Name = (model: SectorModel, s: SummaryInfo | null): string => (s && s.top1_geo !== null ? `${model.geoName(s.top1_geo)} ${fmtShare(s.top1_share)}` : '–');

function MineralTiles({ rows }: { rows: MineralIndexRow[] }) {
  const model = useModel();
  const { state, link } = useSectorUrl();
  const mineV = model.varById.get(MINE_VAR) ?? null;
  return (
    <ul className="grid gap-3 sm:grid-cols-2 lg:grid-cols-4" aria-label="Minerals">
      {rows.map(({ item, mine, refining }) => {
        const words = mine && mineV ? `mine production ${fmtFact(model, mineV, item, mine.W)} ${mine.year}, ${lensText(state.lens, mine, model)}` : 'no mine production data';
        return (
          <li key={item.slug}>
            <Link to={link(`/sector/minerals/${item.slug}`)} className="card block h-full p-4" aria-label={`${item.label}: ${words}${refining ? `; refining China ${fmtShare(refining.china)} (${refining.year})` : ''}`}>
              <div className="text-base font-medium text-paper">{item.label}</div>
              {mine && mineV ? (
                <>
                  <div className="mt-1 text-xs text-paper-mute">
                    Mine production · {fmtFact(model, mineV, item, mine.W)} · {mine.year}
                  </div>
                  <div className="mt-1 text-sm text-paper-dim">{lensText(state.lens, mine, model)}</div>
                  <MicroBar s={mine} />
                </>
              ) : (
                <div className="mt-1 text-xs text-paper-mute">No mine production data</div>
              )}
              {refining && <div className="mt-2 text-xs text-paper-dim">Refining: China {fmtShare(refining.china)} ({refining.year})</div>}
            </Link>
          </li>
        );
      })}
    </ul>
  );
}

function SplitByMineral({ rows }: { rows: MineralIndexRow[] }) {
  const model = useModel();
  const { link } = useSectorUrl();
  const navigate = useNavigate();
  const mining = model.varById.get(MINING_VAR) ?? null;
  const refining = model.varById.get(REFINING_VAR) ?? null;
  const split: ShareSplitRow[] = [];
  const refs: VarRef[] = [];
  const add = (v: VarInfo | null, key: 'mining' | 'refining', words: string) => {
    if (!v) return;
    for (const r of rows) {
      const s = r[key];
      if (!s) continue;
      split.push(shareSplitRow(model, v, r.item, concFromSummary(s), { id: `${r.item.slug}-${key}`, label: `${r.item.label}`, sub: `${words}, ${v.source.short} ${s.year}`, year: s.year, href: link(`/sector/minerals/${r.item.slug}`), section: key }));
      if (!refs.some((x) => x.var === v.id)) refs.push({ var: v.id, item: r.item.slug, year: s.year });
    }
  };
  add(mining, 'mining', 'mining');
  add(refining, 'refining', 'refining');
  if (!split.length) return null;
  const sections = [
    { key: 'mining', label: 'Mine production' },
    { key: 'refining', label: 'Refining' },
  ].filter((s) => split.some((r) => r.section === s.key));
  return (
    <SectorChartCard
      headingLevel={2}
      title="Who mines and who refines, by mineral"
      vars={refs}
      subExtra={<HowToRead>{SPLIT_READING} Each mineral has two rows, mining and refining, so you can see where one economy controls the processing step even when mining is spread out. Click a row for the mineral.</HowToRead>}
      table={() => <SplitTable rows={split} caption="Mining and refining by mineral" />}
      csv={() => ({
        name: 'minerals-mining-refining-split',
        text: rowsToCsv(split.map((r) => ({ mineral: r.label, measure: r.section, ...Object.fromEntries(Object.entries(r.parts).map(([k, x]) => [`share_${k}`, x])), top1: r.cols.top1, top3: r.cols.top3, hhi: r.cols.hhi, n: r.cols.n, source: r.cols.source, year: r.cols.year }))),
      })}
      legend={<GroupLegend />}
      notes={<p>IEA lists the six largest producers of each mineral, so the rest is the not-attributed band and the <Term id="hhi">concentration index (HHI)</Term> is a lower bound ("≥").</p>}
      summary={`Group shares of mining and refining for ${split.length / Math.max(1, sections.length)} minerals.`}
    >
      <ShareSplitChart rows={split} columns={SPLIT_COLUMNS} sections={sections} ariaLabel="Mining and refining shares by mineral" onRowClick={(r) => r.href && navigate(r.href)} />
    </SectorChartCard>
  );
}

function MineralTable({ rows }: { rows: MineralIndexRow[] }) {
  const model = useModel();
  const { link } = useSectorUrl();
  const mineV = model.varById.get(MINE_VAR) ?? null;
  const mining = model.varById.get(MINING_VAR) ?? null;
  const refining = model.varById.get(REFINING_VAR) ?? null;
  const refs: VarRef[] = [mineV, mining, refining].filter((v): v is VarInfo => !!v).map((v) => ({ var: v.id }));
  if (!refs.length) return null;
  return (
    <SectorChartCard
      headingLevel={2}
      title="Every mineral"
      vars={refs}
      subExtra={<HowToRead>One row per mineral: how much is mined, how much of mining and of refining the three largest economies hold, China&apos;s and the coalition&apos;s shares, and the technologies that use it. Click a mineral for its page.</HowToRead>}
      csv={() => ({
        name: 'minerals',
        text: rowsToCsv(
          rows.map((r) => ({
            mineral: r.item.slug,
            mine_production_usgs: r.mine?.W ?? null,
            mine_production_unit: mineV ? model.unitOf(mineV, r.item.idx).short : null,
            mine_production_year: r.mine?.year ?? null,
            top3_mining_iea: r.mining?.top3_share ?? null,
            top3_refining_iea: r.refining?.top3_share ?? null,
            china_mining_iea: r.mining?.china ?? null,
            china_refining_iea: r.refining?.china ?? null,
            coalition_mining_iea: Number.isFinite(coalition(r.mining)) ? coalition(r.mining) : null,
            coalition_refining_iea: Number.isFinite(coalition(r.refining)) ? coalition(r.refining) : null,
            iea_year: r.mining?.year ?? r.refining?.year ?? null,
            technologies: r.techs.map((t) => t.label).join('; '),
          })),
        ),
      })}
      summary={`${rows.length} minerals with their mine production, the shares of the three largest miners and refiners, and the technologies that use them.`}
    >
      <TableView
        rows={rows}
        caption="Minerals"
        rowKey={(r) => r.item.slug}
        cols={[
          { id: 'm', header: 'Mineral', value: (r) => r.item.label, cell: (r) => <Link className="link" to={link(`/sector/minerals/${r.item.slug}`)}>{r.item.label}</Link> },
          { id: 'p', header: 'Mine production (USGS)', value: (r) => r.mine?.W ?? null, cell: (r) => (r.mine && mineV ? `${fmtFact(model, mineV, r.item, r.mine.W, 'table')} (${r.mine.year})` : '–'), num: true },
          { id: 'tm', header: 'Top-3 mining (IEA)', value: (r) => r.mining?.top3_share ?? null, cell: (r) => fmtShare(r.mining?.top3_share), num: true },
          { id: 'tr', header: 'Top-3 refining (IEA)', value: (r) => r.refining?.top3_share ?? null, cell: (r) => fmtShare(r.refining?.top3_share), num: true },
          { id: 'top', header: 'Largest refiner', value: (r) => top1Name(model, r.refining) },
          { id: 'cm', header: 'China, mining', value: (r) => r.mining?.china ?? null, cell: (r) => fmtShare(r.mining?.china), num: true },
          { id: 'cr', header: 'China, refining', value: (r) => r.refining?.china ?? null, cell: (r) => fmtShare(r.refining?.china), num: true },
          { id: 'km', header: 'Coalition, mining', value: (r) => coalition(r.mining), cell: (r) => fmtShare(coalition(r.mining)), num: true },
          { id: 'kr', header: 'Coalition, refining', value: (r) => coalition(r.refining), cell: (r) => fmtShare(coalition(r.refining)), num: true },
          {
            id: 't',
            header: 'Used in',
            value: (r) => r.techs.map((t) => t.label).join(', '),
            cell: (r) =>
              r.techs.length ? (
                <span>
                  {r.techs.map((t, i) => (
                    <span key={t.slug}>
                      {i > 0 && ', '}
                      <Link className="link" to={link(`/sector/${t.slug}`)}>
                        {t.label}
                      </Link>
                    </span>
                  ))}
                </span>
              ) : (
                '–'
              ),
            sortable: false,
          },
        ]}
      />
    </SectorChartCard>
  );
}

export default function MineralIndexView() {
  const model = useModel();
  const { state, link } = useSectorUrl();
  useTitle('Minerals', 'Sector Insights');
  const rows = useMemo(() => mineralIndexRows(model, state.eu).sort((a, z) => (Number.isFinite(z.demand) ? z.demand : -1) - (Number.isFinite(a.demand) ? a.demand : -1) || a.item.sort - z.item.sort), [model, state.eu]);
  const withRefining = rows.filter((r) => r.refining);
  const lede = rows.length
    ? `The minerals behind clean-energy technologies: who mines them, who refines them, who holds the reserves, and who exports them (${rows.length} minerals). ${withRefining.length ? `The IEA reports refining for ${listText(withRefining.map((r) => lc(r.item.label)))}.` : ''}`
    : 'No mineral data in this build.';
  return (
    <div className="space-y-8">
      <PageHead eyebrow="SECTORS · MINERALS" title="Minerals" lede={lede}>
        <p className="mt-2 text-sm text-paper-mute">
          Uranium is measured on the{' '}
          <Link className="link" to={link('/sector/nuclear/upstream')}>
            Nuclear — Upstream
          </Link>{' '}
          page.
        </p>
      </PageHead>
      {rows.length > 0 && (
        <>
          <MineralTiles rows={rows} />
          <SplitByMineral rows={rows} />
          <MineralTable rows={rows} />
        </>
      )}
    </div>
  );
}
