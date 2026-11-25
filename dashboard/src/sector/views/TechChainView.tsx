// Technology chain /sector/:tech (spec_sector.md 4.2): the whole supply chain of one technology and
// where in it concentration sits, at the six stages. Conclusions first (external review readiness,
// 2026-11-23): "Where are the real bottlenecks?" (./Bottlenecks.tsx, from the AIB nodes ChainNav already
// loads), then (a) the chain concentration ladder (hidden when the technology has no row) (one share-split
// row per country-level measure, grouped by stage in chain order, plus the mining of the minerals the
// technology uses under Extraction and their refining under Refining, design C3), the chain navigator,
// (b) the stage cards (three by two on wide screens; the stages without comparable country-level evidence
// said once, in one line under the grid, instead of one empty card each), (c) the codes kept out of the technology's trade baskets (listed for
// the technology: they have no stage, design C6), (d) the minerals this technology uses (IEA demand,
// observed vs 2040 STEPS).

import { useMemo } from 'react';
import { Link, useNavigate, useParams } from 'react-router';
import { Redirect } from '../../shell/Redirect.tsx';
import { TableView } from '../../components/TableView.tsx';
import { PageHead } from '../../components/ui.tsx';
import { CHAIN_QUESTION, NO_EVIDENCE } from '../copy.ts';
import { rowsToCsv } from '../../lib/csv.ts';
import { useTitle } from '../../shell/title.ts';
import { GROUP_COLOR } from '../../theme/palette.ts';
import { FAMILY_SECTION, isTechSlug, isTradeTechSlug, STAGE_LABELS, STAGE_SLUGS, TECH_LABELS, type StageSlug, type TechSlug } from '../contract.ts';
import { useModel, useSectorManifest } from '../data/SectorDataProvider.tsx';
import type { ItemInfo, SectorModel, VarInfo } from '../data/model.ts';
import type { FactsIndex } from '../data/facts.ts';
import { countryFamilies, coverageChips, familyMeasure, primaryMeasure } from '../derive/cells.ts';
import { growthKindWords, growthWords } from '../derive/concentration.ts';
import { lc, fmtShare, listText } from '../format.ts';
import { fmtFact, HowToRead, markGroup, measureConc, SPLIT_COLUMNS, shareSplitRow, useHighlight } from './common.tsx';
import { EXCLUDED_WORDS, excludedCodes } from '../derive/trade.ts';
import { Disclosure } from '../../components/ui.tsx';
import { ChunkGate } from './common.tsx';
import { useFacts, useTrade } from './data.ts';
import { MINERAL_STEP } from '../derive/minerals.ts';
import { CoverageChips, Dumbbell, GroupLegend, SectorChartCard, ShareSplitChart, type CoverageChip, type DumbbellRow, type ShareSplitRow, type VarRef } from './kit.tsx';
import { useSectorUrl } from './state.tsx';
import { ChainNav } from './ChainNav.tsx';
import { Bottlenecks, type ConcentrationLine } from './Bottlenecks.tsx';
import { hhiBand } from './HhiExplainer.tsx';

/**
 * How to read the chain's share split: the groups named for what they are in the strategy (China is the
 * major supplier of the stress test; the coalition core and partners are the allied tiers of the
 * coalition). The generic wording is common.tsx SPLIT_READING.
 */
const CHAIN_SPLIT_READING =
  'Each bar is one measure, split by who holds it: China (the major supplier in the stress test), the coalition core, coalition partners, the rest of the world and the part the source does not assign to any economy. The longer a segment, the larger that group’s share; the columns beside it name the largest holder and give the top-three share and the concentration index.';

interface LadderRow {
  row: ShareSplitRow;
  ref: VarRef;
  china: number;
  stage: StageSlug;
  words: string;
  /** the largest holder and its share (the bottleneck block's concentration-only fallback) */
  top1Name: string | null;
  top1Share: number | null;
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
          top1Name: m.conc.top1 ? model.geos[m.conc.top1.geo].name : null,
          top1Share: m.conc.top1?.share ?? null,
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
            top1Name: m.conc.top1 ? model.geos[m.conc.top1.geo].name : null,
            top1Share: m.conc.top1?.share ?? null,
          });
        }
      }
    }
    return out;
  }, [model, tech, facts, state.eu, highlight, link]);
}

/** The primary measure of a stage as its card shows it, or null when the stage has no country-level measure with a world total. */
function stageMeasure(model: SectorModel, facts: FactsIndex | null, cell: ItemInfo, eu: 'members' | 'bloc') {
  const pm = primaryMeasure(model, cell);
  if (!pm) return null;
  const { v, item } = pm;
  const year = baseYear(model, facts, v, item, eu);
  if (year === null) return null;
  const m = measureConc(model, facts, v, item, year, eu);
  if (!m || m.conc.W === null) return null;
  return { v, item, year, m, s: model.summaryOf(item.idx, v.idx, year, eu) };
}

/**
 * The stages of a technology with and without comparable country-level evidence: a stage has none when the
 * manifest's coverage glyph says it has no country data, or when its card would have no primary measure
 * with a world total (judged once the facts chunk has loaded). Those stages are said once under the stage
 * grid instead of one empty card each.
 */
export function stageEvidence(model: SectorModel, facts: FactsIndex | null, tech: TechSlug, eu: 'members' | 'bloc', glyph: readonly { tech: string; stage: string; country_data: boolean }[] | null | undefined): { with: StageSlug[]; without: StageSlug[] } {
  const out = { with: [] as StageSlug[], without: [] as StageSlug[] };
  for (const s of STAGE_SLUGS) {
    const cell = model.cell(tech, s);
    if (!cell) continue;
    const noCountryData = glyph?.some((g) => g.tech === tech && g.stage === s && !g.country_data) ?? false;
    // until the facts chunk loads, a stage the meta summary cannot show keeps its card (no premature "not available")
    (noCountryData || (facts !== null && !stageMeasure(model, facts, cell, eu)) ? out.without : out.with).push(s);
  }
  return out;
}

function StageCard({ tech, stage, facts }: { tech: TechSlug; stage: StageSlug; facts: FactsIndex | null }) {
  const model = useModel();
  const { state, link } = useSectorUrl();
  const cell = model.cell(tech, stage);
  if (!cell) return null;
  const sm = stageMeasure(model, facts, cell, state.eu);
  const top3 = sm?.m.fromFacts ? sm.m.conc.rows.slice(0, 3) : [];
  const band = sm ? hhiBand(sm.m.conc.hhi, sm.m.conc.hhi_lower_bound, model.hhiRefs) : null;
  return (
    <article className="panel flex h-full flex-col border-t-[3px] border-t-ink-500 p-4" aria-labelledby={`stage-${stage}`}>
      <h3 id={`stage-${stage}`} className="text-lg font-medium text-paper">
        {STAGE_LABELS[stage]}
      </h3>
      <div className="mt-2">
        <CoverageChips chips={chipsOf(model, cell, (section) => link(`/sector/${tech}/${stage}`, {}, section))} ariaLabel={`Coverage of ${model.cellLabel(cell)}`} />
      </div>
      {sm && (
        <div className="mt-3">
          <div className="eyebrow">{sm.v.short}</div>
          <div className="text-xl font-medium text-paper">{fmtFact(model, sm.v, sm.item, sm.m.conc.W as number)}</div>
          <div className="text-xs text-paper-mute">
            {sm.year} · {sm.v.source.short}
            {sm.s && Number.isFinite(sm.s.growth) ? ` · ${growthWords({ value: sm.s.growth, isNew: false })} (${growthKindWords(sm.s.growth_kind)})` : ''}
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
          {band && (
            <p className="mt-2 text-xs text-paper-mute" title={band.title} data-testid="stage-hhi-band">
              {band.text}.
            </p>
          )}
        </div>
      )}
      <div className="mt-auto pt-4">
        <Link className="link" to={link(`/sector/${tech}/${stage}`)}>
          Open the {STAGE_LABELS[stage]} profile
        </Link>
      </div>
    </article>
  );
}

/** The stages without comparable country-level evidence, in one line, each linked to its profile. */
function NoEvidenceLine({ tech, without, nWith }: { tech: TechSlug; without: StageSlug[]; nWith: number }) {
  const { link } = useSectorUrl();
  if (!without.length) return null;
  return (
    <p className="mt-3 text-sm text-paper-mute" data-testid="no-evidence-stages">
      {NO_EVIDENCE.lead}{' '}
      {without.map((s, i) => (
        <span key={s}>
          {i > 0 ? (i === without.length - 1 ? ' and ' : ', ') : ''}
          <Link className="link" to={link(`/sector/${tech}/${s}`)}>
            {STAGE_LABELS[s]}
          </Link>
        </span>
      ))}
      .{' '}
      {nWith > 0 ? NO_EVIDENCE.focus(nWith) : NO_EVIDENCE.none}
    </p>
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
  const manifest = useSectorManifest();
  const { state } = useSectorUrl();
  const tech = raw && isTechSlug(raw) ? raw : null;
  useTitle(tech ? `${TECH_LABELS[tech]}: the supply chain` : 'Sectors', 'Sector Insights');
  const { facts } = useFacts();
  const ladder = useLadder((tech ?? 'batteries') as TechSlug, facts);
  const highlight = useHighlight(model);
  const navigate = useNavigate();
  const evidence = useMemo(() => stageEvidence(model, facts, (tech ?? 'batteries') as TechSlug, state.eu, manifest.data?.coverage_glyph), [model, facts, tech, state.eu, manifest.data]);
  if (!tech) return <Redirect to="/sector" notice={`No sector called '${raw}'; showing all sectors.`} />;
  const blurb = model.techText.get(tech)?.tech_blurb ?? `${TECH_LABELS[tech]} from raw materials to deployment.`;
  const stages = STAGE_SLUGS.filter((s) => model.cell(tech, s));
  const reading = ladder
    .filter((r) => r.china > 0.005)
    .sort((a, z) => z.china - a.china)
    .slice(0, 3)
    .map((r) => `${fmtShare(r.china)} of ${r.words}`);
  const fallback: ConcentrationLine[] = ladder.map((r) => ({ id: r.row.id, label: r.row.label, stage: r.stage, top1Name: r.top1Name, top1Share: r.top1Share, china: r.china, href: r.row.href }));
  return (
    <div className="space-y-8">
      <PageHead
        eyebrow="SECTORS · TECHNOLOGY"
        title={`${TECH_LABELS[tech]}: the supply chain`}
        lede={
          <>
            <span className="block font-medium text-paper">{CHAIN_QUESTION}</span>
            {blurb}
          </>
        }
      />
      <Bottlenecks tech={tech} fallback={fallback} />
      {/* no row once the facts have loaded: the card is left out (the stage line below says what is missing) */}
      {(ladder.length > 0 || facts === null) && (
        <SectorChartCard
          headingLevel={2}
          title="Where concentration sits along the chain"
          vars={ladder.map((r) => r.ref)}
          subExtra={<HowToRead>{CHAIN_SPLIT_READING} Rows run along the chain, from extraction of the raw materials to deployment and recovery, so the chart shows at which step one economy holds the most. Click a row for that sector.</HowToRead>}
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
          summary={reading.length ? `China holds ${listText(reading)}.` : ladder.length ? `China holds under 0.5% of each of the ${ladder.length} ${TECH_LABELS[tech]} measures shown.` : `Concentration along the ${TECH_LABELS[tech]} supply chain.`}
        >
          {reading.length > 0 && <p className="mb-3 text-sm font-medium text-paper">China holds {listText(reading)}.</p>}
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
            <p className="text-sm text-paper-mute">Loading the measures…</p>
          )}
        </SectorChartCard>
      )}
      <ChainNav tech={tech} />
      <section aria-labelledby="stages-h">
        <h2 id="stages-h" className="sr-only">
          The stages of the chain
        </h2>
        {evidence.with.length > 0 && (
          <div className="grid gap-4 sm:grid-cols-2 lg:grid-cols-3">
            {evidence.with.map((s) => (
              <StageCard key={s} tech={tech} stage={s} facts={facts} />
            ))}
          </div>
        )}
        <NoEvidenceLine tech={tech} without={evidence.without} nWith={evidence.with.length} />
      </section>
      <ExcludedCodes tech={tech} />
      <MineralsUsed tech={tech} />
    </div>
  );
}
