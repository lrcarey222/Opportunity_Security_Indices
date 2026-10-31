// Technology chain /sector/:tech (spec_sector.md 4.2): the whole supply chain of one technology and
// where in it concentration sits, at the six stages. (a) the chain concentration ladder (one share-split
// row per country-level measure, grouped by stage in chain order, plus the mining of the minerals the
// technology uses under Extraction and their refining under Refining, design C3), (b) the stage cards
// (three by two on wide screens), (c) the codes kept out of the technology's trade baskets (listed for
// the technology: they have no stage, design C6), (d) the minerals this technology uses (IEA demand,
// observed vs 2040 STEPS).

import { useMemo } from 'react';
import { Link, useNavigate, useParams } from 'react-router';
import { Redirect } from '../../shell/Redirect.tsx';
import { TableView } from '../../components/TableView.tsx';
import { PageHead } from '../../components/ui.tsx';
import { CHAIN_QUESTION } from '../copy.ts';
import { rowsToCsv } from '../../lib/csv.ts';
import { useTitle } from '../../shell/title.ts';
import { GROUP_COLOR } from '../../theme/palette.ts';
import { FAMILY_SECTION, isTechSlug, isTradeTechSlug, STAGE_LABELS, STAGE_SLUGS, TECH_LABELS, type StageSlug, type TechSlug } from '../contract.ts';
import { useModel } from '../data/SectorDataProvider.tsx';
import type { ItemInfo, SectorModel, VarInfo } from '../data/model.ts';
import type { FactsIndex } from '../data/facts.ts';
import { countryFamilies, coverageChips, familyMeasure, primaryMeasure } from '../derive/cells.ts';
import { growthKindWords, growthWords } from '../derive/concentration.ts';
import { lc, fmtShare, listText } from '../format.ts';
import { fmtFact, HowToRead, markGroup, measureConc, SPLIT_COLUMNS, SPLIT_READING, shareSplitRow, useHighlight } from './common.tsx';
import { EXCLUDED_WORDS, excludedCodes } from '../derive/trade.ts';
import { Disclosure } from '../../components/ui.tsx';
import { ChunkGate } from './common.tsx';
import { useFacts, useTrade } from './data.ts';
import { MINERAL_STEP } from '../derive/minerals.ts';
import { CoverageChips, Dumbbell, GroupLegend, SectorChartCard, ShareSplitChart, type CoverageChip, type DumbbellRow, type ShareSplitRow, type VarRef } from './kit.tsx';
import { useSectorUrl } from './state.tsx';
import { ChainNav } from './ChainNav.tsx';

interface LadderRow {
  row: ShareSplitRow;
  ref: VarRef;
  china: number;
  stage: StageSlug;
  words: string;
}

/** The latest observed (non-projected) year of a var for an item. */
export function baseYear(model: SectorModel, facts: FactsIndex | null, v: VarInfo, item: ItemInfo, eu: 'members' | 'bloc'): number | null {
  const lim = v.projection_from !== null ? v.projection_from - 1 : undefined;
  if (facts) {
    const y = facts.latestYear(v.idx, item.idx, lim);
    if (y !== null) return y;
  }
  const ys = model.summary.filter((s) => s.item === item.idx && s.var === v.idx && s.eu_mode === eu && (lim === undefined || s.year <= lim)).map((s) => s.year);
  return ys.length ? Math.max(...ys) : null;
}

/** Coverage chips of a cell in the kit's shape, linking to the profile sections. */
export function chipsOf(model: SectorModel, cell: ItemInfo, href: (section: string) => string): CoverageChip[] {
  return coverageChips(model, cell).map((c) => ({
    family: c.family,
    label: c.label,
    status: model.coverage.get(`${cell.idx}|${c.family}`)?.status ?? 'none',
    text: c.text,
    href: c.hasData && c.section ? href(c.section) : undefined,
    reason: c.reason ?? undefined,
  }));
}

function useLadder(tech: TechSlug, facts: FactsIndex | null): LadderRow[] {
  const model = useModel();
  const { state, link } = useSectorUrl();
  const highlight = useHighlight(model);
  return useMemo(() => {
    const out: LadderRow[] = [];
    for (const stage of STAGE_SLUGS) {
      const cell = model.cell(tech, stage);
      if (!cell) continue;
      for (const f of countryFamilies(model, cell)) {
        const cm = familyMeasure(model, cell, f);
        if (!cm || cm.v.world_basis === 'none') continue;
        const { v, item } = cm;
        const year = baseYear(model, facts, v, item, state.eu);
        if (year === null) continue;
        const m = measureConc(model, facts, v, item, year, state.eu);
        if (!m || m.conc.W === null) continue;
        const section = FAMILY_SECTION[f] ?? 'snapshot';
        out.push({
          row: shareSplitRow(model, v, item, m.conc, { id: `${cell.slug}|${v.id}`, label: `${STAGE_LABELS[stage]} ${lc(v.short)}`, year, highlight, href: link(`/sector/${tech}/${stage}`, {}, section), section: stage }),
          ref: { var: v.id, item: item.slug, year },
          china: m.conc.split.china,
          stage,
          words: `${STAGE_LABELS[stage]} ${lc(v.short)}`,
        });
      }
      // the minerals this technology uses: their mining under Extraction, their refining under Refining (base year)
      const step = MINERAL_STEP[stage];
      const v = step ? model.varById.get(step.id) : undefined;
      if (step && v) {
        for (const { item: mineral } of model.itemsOfCell(cell.idx, 'mineral')) {
          const year = baseYear(model, facts, v, mineral, state.eu);
          if (year === null) continue;
          const m = measureConc(model, facts, v, mineral, year, state.eu);
          if (!m || m.conc.W === null) continue;
          out.push({
            row: shareSplitRow(model, v, mineral, m.conc, { id: `${mineral.slug}|${step.id}`, label: `${mineral.label} ${step.words}`, year, highlight, href: link(`/sector/minerals/${mineral.slug}`), section: stage }),
            ref: { var: v.id, item: mineral.slug, year },
            china: m.conc.split.china,
            stage,
            words: `${lc(mineral.label)} ${step.words}`,
          });
        }
      }
    }
    return out;
  }, [model, tech, facts, state.eu, highlight, link]);
}

function StageCard({ tech, stage, facts }: { tech: TechSlug; stage: StageSlug; facts: FactsIndex | null }) {
  const model = useModel();
  const { state, link } = useSectorUrl();
  const cell = model.cell(tech, stage);
  if (!cell) return null;
  const pm = primaryMeasure(model, cell);
  const v = pm?.v ?? null;
  const item = pm?.item ?? cell;
  const year = v ? baseYear(model, facts, v, item, state.eu) : null;
  const m = v && year !== null ? measureConc(model, facts, v, item, year, state.eu) : null;
  const s = v && year !== null ? model.summaryOf(item.idx, v.idx, year, state.eu) : null;
  const top3 = m?.fromFacts ? m.conc.rows.slice(0, 3) : [];
  return (
    <article className="panel flex h-full flex-col border-t-[3px] border-t-ink-500 p-4" aria-labelledby={`stage-${stage}`}>
      <h3 id={`stage-${stage}`} className="text-lg font-medium text-paper">
        {STAGE_LABELS[stage]}
      </h3>
      <div className="mt-2">
        <CoverageChips chips={chipsOf(model, cell, (section) => link(`/sector/${tech}/${stage}`, {}, section))} ariaLabel={`Coverage of ${model.cellLabel(cell)}`} />
      </div>
      {v && m && m.conc.W !== null ? (
        <div className="mt-3">
          <div className="eyebrow">{v.short}</div>
          <div className="text-xl font-medium text-paper">{fmtFact(model, v, item, m.conc.W)}</div>
          <div className="text-xs text-paper-mute">
            {year} · {v.source.short}
            {s && Number.isFinite(s.growth) ? ` · ${growthWords({ value: s.growth, isNew: false })} (${growthKindWords(s.growth_kind)})` : ''}
          </div>
          {top3.length > 0 && (
            <ol className="mt-2 space-y-0.5 text-sm">
              {top3.map((r) => (
                <li key={r.geo} className="flex items-center gap-2">
                  <span className="inline-block h-2.5 w-2.5 rounded-full" style={{ background: GROUP_COLOR[markGroup(model.geos[r.geo])] }} aria-hidden="true" />
                  <span className="text-paper-dim">{model.geos[r.geo].name}</span>
                  <span className="tabular text-paper-mute">{fmtShare(r.share)}</span>
                </li>
              ))}
            </ol>
          )}
        </div>
      ) : (
        <p className="mt-3 text-sm text-paper-mute">No country-level primary measure for this stage.</p>
      )}
      <div className="mt-auto pt-4">
        <Link className="link" to={link(`/sector/${tech}/${stage}`)}>
          Open the {STAGE_LABELS[stage]} profile
        </Link>
      </div>
    </article>
  );
}

/** IEA demand rows of one technology label: observed vs 2040 STEPS per mineral. */
export function demandRows(model: SectorModel, techLabel: string, href: (slug: string) => string): (DumbbellRow & { share: number | null })[] {
  const out: (DumbbellRow & { share: number | null })[] = [];
  const minerals = [...new Set(model.mineralDemand.filter((d) => d.tech === techLabel).map((d) => d.mineral))];
  for (const mi of minerals) {
    const m = model.items[mi];
    if (!m) continue;
    const mine = model.mineralDemand.filter((d) => d.mineral === mi && d.tech === techLabel);
    const obs = mine.filter((d) => !/steps|aps|nze/i.test(d.scenario)).sort((a, z) => z.year - a.year)[0];
    const proj = mine.filter((d) => /steps/i.test(d.scenario)).sort((a, z) => z.year - a.year)[0];
    if (!obs) continue;
    const total = model.mineralDemand.find((d) => d.mineral === mi && d.tech === null && /^total/i.test(d.label) && d.year === obs.year && d.scenario === obs.scenario);
    const share = total && total.kt > 0 ? obs.kt / total.kt : null;
    out.push({
      id: m.slug,
      label: m.label,
      a: proj?.kt ?? NaN,
      b: obs.kt,
      group: 'neutral',
      aLabel: proj ? `${proj.kt.toLocaleString('en-US', { maximumFractionDigits: 0 })} kt (${proj.year} ${proj.scenario}, projection)` : '–',
      bLabel: `${obs.kt.toLocaleString('en-US', { maximumFractionDigits: 0 })} kt (${obs.year})${share !== null ? ` · ${techLabel} accounts for ${fmtShare(share)} of clean-energy demand for ${lc(m.label)}` : ''}`,
      href: href(m.slug),
      share,
    });
  }
  return out.sort((a, z) => z.b - a.b);
}

/** The minerals a technology uses (IEA demand, observed vs 2040 STEPS); also the Sector profile's Climate block. */
export function MineralsUsed({ tech, headingLevel = 2 }: { tech: TechSlug; headingLevel?: 2 | 3 }) {
  const model = useModel();
  const { link } = useSectorUrl();
  const navigate = useNavigate();
  const label = TECH_LABELS[tech];
  const rows = useMemo(() => demandRows(model, label, (slug) => link(`/sector/minerals/${slug}`)), [model, label, link]);
  if (!rows.length) return null;
  return (
    <SectorChartCard
      headingLevel={headingLevel}
      title={`Minerals ${label} uses`}
      vars={[{ var: 'consumption.mineral_demand' }]}
      subExtra={<HowToRead>Each row is one mineral; the filled dot is how many kilotonnes {label} used in the latest observed year, the hollow dot the IEA projection for 2040 under today’s stated policies (the Stated Policies Scenario), not a forecast. The further right, the more it uses. Click a row for that mineral’s page.</HowToRead>}
      table={() => (
        <TableView
          rows={rows}
          caption="Minerals this technology uses"
          cols={[
            { id: 'm', header: 'Mineral', value: (r) => r.label },
            { id: 'b', header: 'Demand (observed), kt', value: (r) => r.b, num: true },
            { id: 'a', header: 'Demand 2040, stated-policies projection, kt', value: (r) => (Number.isFinite(r.a) ? r.a : null), num: true },
            { id: 's', header: 'Share of clean-energy demand', value: (r) => r.share, cell: (r) => fmtShare(r.share), num: true },
          ]}
        />
      )}
      csv={() => ({ name: `${tech}-minerals-demand`, text: rowsToCsv(rows.map((r) => ({ mineral: r.id, demand_observed_kt: r.b, demand_2040_steps_kt: Number.isFinite(r.a) ? r.a : null, share_of_clean_demand: r.share, source: 'IEA Critical Minerals Dataset' }))) })}
      summary={`${label} uses ${listText(rows.slice(0, 4).map((r) => lc(r.label)))}; ${lc(rows[0].label)} has the largest demand.`}
    >
      <Dumbbell rows={rows} xLabel="Demand, kt" aName="2040 stated-policies projection" bName="Observed" ariaLabel={`Mineral demand of ${label}`} onSelect={(r) => r.href && navigate(r.href)} />
    </SectorChartCard>
  );
}

/**
 * The codes the reference list keeps out of the technology's trade baskets, with the reason (design C6):
 * listed once for the technology, since they have no supply-chain stage. Loads the technology's trade
 * chunk (shared with the chain navigator); nothing when the technology has no basket or excludes nothing.
 */
export function ExcludedCodes({ tech }: { tech: TechSlug }) {
  const model = useModel();
  const has = isTradeTechSlug(tech) && STAGE_SLUGS.some((s) => model.cell(tech, s)?.has_trade);
  const { h } = useTrade(tech, has);
  if (!has) return null;
  return (
    <ChunkGate h={h} name={`sector-trade-${tech}`}>
      {(t) => {
        const rows = excludedCodes(t, TECH_LABELS[tech]);
        if (!rows.length) return null;
        return (
          <section aria-labelledby="excluded-h" className="panel p-4" data-testid="excluded-codes">
            <h2 id="excluded-h" className="text-base font-medium text-paper">
              Codes kept out of the {TECH_LABELS[tech]} trade baskets
            </h2>
            <p className="mt-1 max-w-3xl text-sm text-paper-mute">{EXCLUDED_WORDS} Each stage’s profile lists the codes its basket holds.</p>
            <div className="mt-2">
              <Disclosure summary={`${rows.length} code${rows.length === 1 ? '' : 's'} and the reasons`}>
                <ul className="space-y-1 text-sm text-paper-dim">
                  {rows.map((e) => (
                    <li key={`${e.sub ?? ''}|${e.hs6}`}>
                      <span className="tabular text-paper">{e.hs6}</span>
                      {e.sub ? ` (${e.sub})` : ''}: {e.reason}
                    </li>
                  ))}
                </ul>
              </Disclosure>
            </div>
          </section>
        );
      }}
    </ChunkGate>
  );
}

export default function TechChainView() {
  const { tech: raw } = useParams();
  const model = useModel();
  const tech = raw && isTechSlug(raw) ? raw : null;
  useTitle(tech ? `${TECH_LABELS[tech]}: the supply chain` : 'Sectors', 'Sector Insights');
  const { facts } = useFacts();
  const ladder = useLadder((tech ?? 'batteries') as TechSlug, facts);
  const highlight = useHighlight(model);
  const navigate = useNavigate();
  if (!tech) return <Redirect to="/sector" notice={`No sector called '${raw}'; showing all sectors.`} />;
  const blurb = model.techText.get(tech)?.tech_blurb ?? `${TECH_LABELS[tech]} from raw materials to deployment.`;
  const stages = STAGE_SLUGS.filter((s) => model.cell(tech, s));
  const reading = ladder
    .filter((r) => r.china > 0.005)
    .sort((a, z) => z.china - a.china)
    .slice(0, 3)
    .map((r) => `${fmtShare(r.china)} of ${r.words}`);
  return (
    <div className="space-y-8">
      <PageHead
        eyebrow="SECTORS · TECHNOLOGY"
        title={`${TECH_LABELS[tech]}: the supply chain`}
        lede={
          <>
            <span className="block font-medium text-paper">{CHAIN_QUESTION}</span>
            {blurb} Follow it stage by stage: where supply is concentrated along the chain, then each stage’s profile, with who is best placed and what each country should do.
          </>
        }
      />
      <ChainNav tech={tech} />
      <SectorChartCard
        headingLevel={2}
        title="Where concentration sits along the chain"
        vars={ladder.map((r) => r.ref)}
        subExtra={<HowToRead>{SPLIT_READING} Rows run along the chain, from extraction of the raw materials to deployment and recovery, so the chart shows at which step one economy holds the most. Click a row for that sector.</HowToRead>}
        table={() => (
          <TableView
            rows={ladder}
            caption="Chain concentration"
            cols={[
              { id: 'stage', header: 'Stage', value: (r) => STAGE_LABELS[r.stage] },
              { id: 'm', header: 'Measure', value: (r) => r.row.label },
              { id: 'top1', header: 'Top-1', value: (r) => r.row.cols.top1 },
              { id: 'top3', header: 'Top-3', value: (r) => r.row.cols.top3, num: true },
              { id: 'hhi', header: 'Concentration (HHI)', value: (r) => r.row.cols.hhi, num: true },
              { id: 'china', header: 'China', value: (r) => r.china, cell: (r) => fmtShare(r.china), num: true },
              { id: 'y', header: 'Year', value: (r) => r.row.cols.year },
              { id: 's', header: 'Source', value: (r) => r.row.cols.source },
            ]}
          />
        )}
        csv={() => ({
          name: `${tech}-chain-concentration`,
          text: rowsToCsv(ladder.map((r) => ({ stage: r.stage, var_id: r.ref.var, item: r.ref.item, year: r.ref.year, top1: r.row.cols.top1, top3_share: r.row.cols.top3, hhi: r.row.cols.hhi, china: r.china, ...Object.fromEntries(Object.entries(r.row.parts).map(([k, x]) => [`share_${k}`, x])), source: r.row.cols.source }))),
        })}
        legend={<GroupLegend highlight={highlight?.name ?? null} />}
        notes={<p>Shares are within each measure; rows differ in source and year and do not add up.</p>}
        summary={reading.length ? `China holds ${listText(reading)}.` : `No country-level concentration measures for ${TECH_LABELS[tech]}.`}
      >
        {reading.length > 0 && <p className="mb-3 text-sm text-paper-dim">China: {listText(reading)}.</p>}
        {ladder.length ? (
          <ShareSplitChart
            rows={ladder.map((r) => r.row)}
            columns={[...SPLIT_COLUMNS.filter((c) => c.key !== 'effn' && c.key !== 'n'), { key: 'year', label: 'Year' }]}
            sections={stages.map((s) => ({ key: s, label: STAGE_LABELS[s], stage: s }))}
            onRowClick={(r) => r.href && navigate(r.href)}
            ariaLabel={`Concentration along the ${TECH_LABELS[tech]} supply chain`}
            highlightHeader={highlight?.name}
          />
        ) : (
          <p className="text-sm text-paper-mute">No country-level measures with a world total for this technology.</p>
        )}
      </SectorChartCard>
      <section aria-labelledby="stages-h">
        <h2 id="stages-h" className="sr-only">
          The stages of the chain
        </h2>
        <div className="grid gap-4 sm:grid-cols-2 lg:grid-cols-3">
          {stages.map((s) => (
            <StageCard key={s} tech={tech} stage={s} facts={facts} />
          ))}
        </div>
      </section>
      <ExcludedCodes tech={tech} />
      <MineralsUsed tech={tech} />
    </div>
  );
}
