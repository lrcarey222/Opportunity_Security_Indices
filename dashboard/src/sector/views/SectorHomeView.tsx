// Sector home /sector (spec_sector.md 4.1): questions, the Atlas of every supply-chain cell (the 11
// technologies x the six stages, plus Semiconductors; lens and measure family from the URL), the
// "Strategic posture" section (design L3, 2026-10-28: the posture scatter of the strategic position
// framework, its node in ?pnode, with its own basis line; the one recommendation on these pages), the
// minerals strip and the ten theme cards. Renders from sector-meta (meta.summary); facts load only when a
// country is highlighted (its share and rank in every tile).

import { useMemo } from 'react';
import { Link } from 'react-router';
import { ChartCard } from '../../charts/ChartCard.tsx';
import { TriLegend } from '../../charts/legends.tsx';
import { TableView, type Col } from '../../components/TableView.tsx';
import { PageHead, Segmented, Select } from '../../components/ui.tsx';
import { InfoTip } from '../../components/InfoTip.tsx';
import { QUESTIONS } from '../../content/questions.ts';
import { rowsToCsv } from '../../lib/csv.ts';
import { useTitle } from '../../shell/title.ts';
import { GROUP_COLOR, HATCH_CSS } from '../../theme/palette.ts';
import { STAGE_LABELS, STAGE_SLUGS, TECH_LABELS, TECH_SLUGS, type FamilyId, type StageSlug } from '../contract.ts';
import { useModel, useSectorManifest } from '../data/SectorDataProvider.tsx';
import type { ItemInfo, SectorModel, SummaryInfo, VarInfo } from '../data/model.ts';
import { cellMeasure, familyVar, isThin, primaryVar, type CellMeasure } from '../derive/cells.ts';
import { sideWords, tileScale, triTile } from '../derive/contrast.ts';
import { GROUP_ORDER } from '../derive/groups.ts';
import { slice } from '../derive/summary.ts';
import { concentration } from '../derive/concentration.ts';
import { LENS_LABEL, lensText, lensValue } from '../derive/text.ts';
import { lc, fmtShare, fmtHhi, fmtRank } from '../format.ts';
import { FAM_KEYS, LENSES, type FamKey, type Lens } from '../url.ts';
import { fmtFact, QuestionCard, useHighlight } from './common.tsx';
import { useFacts } from './data.ts';
import { yearText } from './caption.ts';
import { CellGrid, GroupLegend, type CellGridTech } from './kit.tsx';
import { useSectorUrl } from './state.tsx';
import { Section } from './common.tsx';
import { PostureBasisTag, SectorPostureCard, SectorPostureIntro } from '../../posture/index.ts';

const FAM_LABEL: Record<FamKey, string> = {
  primary: 'Primary',
  exports: 'Exports',
  investment: 'Investment',
  production: 'Production',
  capacity: 'Capacity',
  reserves: 'Reserves',
  consumption: 'Consumption',
};

/** The var a cell tile shows for a family key. */
export function tileVar(model: SectorModel, cell: ItemInfo, fam: FamKey): VarInfo | null {
  return fam === 'primary' ? primaryVar(model, cell) : familyVar(model, cell, fam as FamilyId);
}

/** The measure (var and the item it is measured on: the cell or its linked fuel) of a cell tile. */
export function tileMeasure(model: SectorModel, cell: ItemInfo, fam: FamKey): CellMeasure | null {
  return cellMeasure(model, cell, tileVar(model, cell, fam));
}

/** A tiny share-split bar (aria-hidden: the numbers are in the tile's aria-label). */
export function MicroBar({ s }: { s: Pick<SummaryInfo, 'china' | 'core' | 'partner' | 'rest' | 'unattributed'> }) {
  return (
    <div className="mt-2 flex h-1.5 w-full gap-px overflow-hidden" aria-hidden="true">
      {GROUP_ORDER.map((k) => {
        const v = s[k];
        if (!(v > 0)) return null;
        return <span key={k} style={{ width: `${v * 100}%`, background: k === 'unattributed' ? HATCH_CSS : GROUP_COLOR[k] }} />;
      })}
    </div>
  );
}

function Atlas() {
  const model = useModel();
  const { state, set, link } = useSectorUrl();
  const highlight = useHighlight(model);
  const { facts } = useFacts(!!highlight);
  const lens = state.lens;
  const fam = state.fam;
  const eu = state.eu;

  const tiles = useMemo(() => {
    const m = new Map<string, { cell: ItemInfo; v: VarInfo | null; item: ItemInfo; s: SummaryInfo | null }>();
    for (const cell of model.cells) {
      const cm = tileMeasure(model, cell, fam);
      const v = cm?.v ?? null;
      const item = cm?.item ?? cell;
      const s = v ? model.summaryLatest(item.idx, v.idx, eu) : null;
      m.set(cell.slug, { cell, v, item, s });
    }
    return m;
  }, [model, fam, eu]);
  const withData = [...tiles.values()].filter((t) => t.s).length;
  // the three-colour scale over the lens values of the tiles shown: white = their median
  const scale = useMemo(() => tileScale([...tiles.values()].map((t) => (t.s ? lensValue(lens, t.s) : NaN))), [tiles, lens]);
  const lensFmt = (x: number) => (lens === 'hhi' ? x.toFixed(2) : `${Math.round(x * 100)}%`);

  const position = (item: ItemInfo, v: VarInfo | null, s: SummaryInfo | null): string | null => {
    if (!highlight || !facts || !v || !s) return null;
    const sl = slice(model, facts, v, item.idx, s.year, eu, null);
    const c = concentration(sl.rows, model.geos, sl.world, sl.basis);
    const r = c.rows.find((x) => x.geo === highlight.idx);
    return r ? `${highlight.code} ${fmtShare(r.share)} · ${fmtRank(r.rank)}` : `${highlight.code}: no value`;
  };

  const techs: CellGridTech[] = TECH_SLUGS.map((t) => ({ slug: t, label: TECH_LABELS[t], headline: t !== 'semiconductors' }));
  const tableRows = [...tiles.values()].filter((t) => t.v);
  const cols: Col<(typeof tableRows)[number]>[] = [
    { id: 'cell', header: 'Cell', value: (t) => model.cellLabel(t.cell), minWidth: 180 },
    { id: 'measure', header: 'Measure', value: (t) => t.v?.short ?? null },
    { id: 'year', header: 'Year', value: (t) => t.s?.year ?? null, num: true },
    { id: 'W', header: 'World total', value: (t) => t.s?.W ?? null, cell: (t) => (t.s && t.v ? fmtFact(model, t.v, t.item, t.s.W) : '–'), num: true },
    { id: 'top1', header: 'Top-1', value: (t) => (t.s?.top1_geo !== null && t.s ? `${model.geoName(t.s.top1_geo)} ${fmtShare(t.s.top1_share)}` : null) },
    { id: 'top3', header: 'Top-3', value: (t) => t.s?.top3_share ?? null, cell: (t) => fmtShare(t.s?.top3_share), num: true },
    { id: 'hhi', header: 'Concentration (HHI)', value: (t) => t.s?.hhi ?? null, cell: (t) => fmtHhi(t.s?.hhi, t.s?.hhi_lower_bound), num: true },
    { id: 'china', header: 'China', value: (t) => t.s?.china ?? null, cell: (t) => fmtShare(t.s?.china), num: true },
    { id: 'coal', header: 'Coalition', value: (t) => (t.s ? t.s.core + t.s.partner : null), cell: (t) => fmtShare(t.s ? t.s.core + t.s.partner : NaN), num: true },
    { id: 'source', header: 'Source', value: (t) => t.v?.source.short ?? null },
  ];
  const csv = () => ({
    name: `sector-atlas-${fam}-${eu}`,
    text: rowsToCsv(
      tableRows.map((t) => ({
        item: t.cell.slug,
        var_id: t.v?.id ?? null,
        measure: t.v?.label ?? null,
        year: t.s?.year ?? null,
        eu_mode: eu,
        world_total: t.s ? t.s.W * (t.v?.display_scale ?? 1) : null,
        unit: t.v ? model.unitOf(t.v, t.item.idx).short : null,
        world_basis: t.s?.basis ?? null,
        n: t.s?.n ?? null,
        top1: t.s ? model.geos[t.s.top1_geo ?? -1]?.code ?? null : null,
        top1_share: t.s?.top1_share ?? null,
        top3_share: t.s?.top3_share ?? null,
        hhi: t.s?.hhi ?? null,
        hhi_lower_bound: t.s ? t.s.hhi_lower_bound : null,
        eff_n: t.s?.eff_n ?? null,
        china: t.s?.china ?? null,
        core: t.s?.core ?? null,
        partner: t.s?.partner ?? null,
        rest: t.s?.rest ?? null,
        unattributed: t.s?.unattributed ?? null,
        source: t.v?.source.short ?? null,
        licence: t.v?.licence ?? null,
      })),
    ),
  });

  return (
    <ChartCard
      headingLevel={3}
      title={`All ${model.cells.length} sectors`}
      sub={`Each tile is one sector (a technology at one stage of its supply chain), coloured by the ${lc(LENS_LABEL[lens])} of its ${fam === 'primary' ? 'main measure' : `${lc(FAM_LABEL[fam])} measure`} in the source's latest year: orange lower than most sectors, white the middle one, blue higher. The tile names the largest holder. Shares are within one measure and never added across sectors.${model.cells.some((c) => !c.headline) ? ` The ${model.cells.filter((c) => c.headline).length} energy sectors come first; semiconductors, a technology the energy sectors depend on, are shown for context.` : ''}`}
      tools={
        <>
          <Segmented<Lens> label="Colour by" value={lens} options={LENSES.filter((l) => l !== 'top1').map((l) => ({ value: l, label: l === 'china' ? 'China share' : l === 'top3' ? 'Top-3 share' : l === 'hhi' ? 'Concentration (HHI)' : 'Coalition share' }))} onChange={(l) => set({ lens: l })} />
          {lens === 'hhi' && <span className="pb-1.5"><InfoTip id="hhi" term="concentration index (HHI)" /></span>}
          <Select label="Measure" value={fam} options={FAM_KEYS.map((f) => ({ value: f, label: FAM_LABEL[f] }))} onChange={(f) => set({ fam: f as FamKey })} className="min-w-[9rem]" />
        </>
      }
      table={() => <TableView rows={tableRows} cols={cols} caption="Atlas summary" search pageSize={40} />}
      csv={csv}
      legend={
        <div className="space-y-2">
          <TriLegend scale={scale} label={`Tile colour: ${LENS_LABEL[lens]} of the ${fam === 'primary' ? 'primary measure' : `${lc(FAM_LABEL[fam])} measure`}`} format={lensFmt} noun="sectors" missing="No data for this measure" />
          <GroupLegend euBloc={eu === 'bloc'} />
        </div>
      }
      summary={`Grid of ${model.cells.length} technology and stage cells; ${withData} have data for this measure.`}
    >
      <p className="mb-3 text-sm text-paper-mute" role="status">
        Showing {withData} of {model.cells.length} sectors with data for this measure.
      </p>
      <CellGrid
        techs={techs}
        stages={STAGE_SLUGS.map((s) => ({ slug: s, label: STAGE_LABELS[s] }))}
        ariaLabel="Supply-chain cells by technology and stage"
        selected={null}
        renderTile={(t, stage: StageSlug) => {
          const tile = tiles.get(`${t.slug}-${stage}`);
          if (!tile) return <div className="min-h-[112px]" aria-hidden="true" />;
          const { cell, v, item, s } = tile;
          const thin = isThin(model, cell);
          const val = s ? lensValue(lens, s) : NaN;
          const shade = s && Number.isFinite(val) ? triTile(scale, val) : null;
          const famLabel = v ? lc(model.families.get(v.family)?.label ?? v.family) : lc(FAM_LABEL[fam]);
          const pos = position(item, v, s);
          const cov = model.coverageOf(cell.idx);
          // a primary measure without a summary (a ratio, or no world basis: consumption per person) has
          // country data but no shares: its count of economies, never "no data"
          const av = v && !s ? model.availOf(v, item.idx).filter((a) => a.year_max !== null) : [];
          const avLast = av.length ? av.reduce((a, z) => ((z.year_max ?? 0) > (a.year_max ?? 0) ? z : a)) : null;
          const noShare = !s && v && avLast ? { n: avLast.n_geo, year: yearText(v, { var: v.id, item: item.slug, year: avLast.year_max }) } : null;
          // a measure that is itself a share has no size ("Capacity share · 100.0%")
          const shareUnit = v ? v.unit.unit_kind === 'share' || v.unit.unit_kind === 'pct' : false;
          const yr = s && v ? yearText(v, { var: v.id, item: item.slug, year: s.year }) : '';
          const aria = s && v
            ? `${model.cellLabel(cell)}: ${lc(v.short)}${shareUnit ? '' : ` ${fmtFact(model, v, item, s.W)}`} ${yr}, ${lensText(lens, s, model)}${shade ? ` (${sideWords(shade.side, scale.n, 'cells')})` : ''}, top three ${fmtShare(s.top3_share)}, concentration index ${fmtHhi(s.hhi, s.hhi_lower_bound)}${pos ? `, ${pos}` : ''}`
            : noShare && v
              ? `${model.cellLabel(cell)}: ${lc(v.short)}, ${noShare.n} economies, ${noShare.year}; no shares (no world total)`
              : `${model.cellLabel(cell)}: no ${famLabel} data${thin ? ', thin' : ''}`;
          return (
            <Link
              to={link(`/sector/${t.slug}/${stage}`)}
              aria-label={aria}
              className={`block min-h-[112px] p-3 text-xs ${thin ? 'border border-dashed border-white/30' : 'border hairline'} focus:outline-2 focus:outline-signal`}
              style={{ background: shade ? shade.bg : s || noShare ? undefined : HATCH_CSS, color: shade ? shade.fg : undefined }}
              title={s && v ? `Largest ${model.geoName(s.top1_geo)} ${fmtShare(s.top1_share)} · top three ${fmtShare(s.top3_share)} · concentration (HHI) ${fmtHhi(s.hhi, s.hhi_lower_bound)}, like ${Number.isFinite(s.eff_n) ? s.eff_n.toFixed(1) : '–'} equal holders · China ${fmtShare(s.china)} · coalition ${fmtShare(s.core + s.partner)} · ${v.source.short} ${s.year}${shade ? ` · ${sideWords(shade.side, scale.n, 'cells')}` : ''}` : undefined}
            >
              {s && v ? (
                <>
                  <div className="font-medium">
                    {v.short}
                    {shareUnit ? '' : ` · ${fmtFact(model, v, item, s.W)}`} · {yr}
                  </div>
                  <div className="mt-1 text-sm font-medium">{lensText(lens, s, model)}</div>
                  <MicroBar s={s} />
                </>
              ) : noShare && v ? (
                <>
                  <div className="font-medium">
                    {v.short} · {noShare.n} economies · {noShare.year}
                  </div>
                  <div className="mt-1 text-paper-mute">no shares: a ratio with no world total</div>
                </>
              ) : (
                // words on a solid chip: text over the hatch stripes had a contrast that varied pixel by pixel
                <div className="inline-block bg-ink-800 px-1 text-paper-dim">no {famLabel} data</div>
              )}
              <div className="mt-2 flex flex-wrap items-center gap-1" aria-hidden="true">
                {cov.map((c) => (
                  <span
                    key={c.family}
                    title={`${model.families.get(c.family)?.label ?? c.family}: ${c.status === 'country' ? 'country data' : c.status === 'none' || c.status === 'licensed' ? 'no data' : 'region or global only'}`}
                    className="inline-block h-2 w-2 rounded-full border border-current"
                    style={{ background: c.status === 'country' ? 'currentColor' : 'transparent', opacity: c.status === 'none' || c.status === 'licensed' ? 0.3 : 1 }}
                  />
                ))}
                {thin && <span className={`ml-1 ${!s && !noShare ? 'bg-ink-800 px-1 text-paper-dim' : ''}`}>thin: {cov.filter((c) => c.status !== 'none').length || 1} source{(cov.filter((c) => c.status !== 'none').length || 1) === 1 ? '' : 's'}</span>}
              </div>
              {pos && <div className="mt-1 font-medium">{pos}</div>}
            </Link>
          );
        }}
      />
    </ChartCard>
  );
}

function MineralsStrip() {
  const model = useModel();
  const { state, link } = useSectorUrl();
  const minerals = useMemo(() => {
    const total = (m: ItemInfo) => {
      const rows = model.mineralDemand.filter((d) => d.mineral === m.idx && d.tech === null && /^total/i.test(d.label));
      const obs = rows.filter((d) => !/steps|aps|nze/i.test(d.scenario)).sort((a, z) => z.year - a.year)[0];
      if (obs) return obs.kt;
      return model.mineralDemand.filter((d) => d.mineral === m.idx && d.tech !== null).reduce((a, d) => a + (Number.isFinite(d.kt) ? d.kt : 0), 0);
    };
    return model.items.filter((x) => x.axis === 'mineral').map((m) => ({ m, demand: total(m) })).sort((a, z) => z.demand - a.demand || a.m.sort - z.m.sort);
  }, [model]);
  const mine = model.varById.get('production.mine_usgs') ?? model.varById.get('production.mining') ?? null;
  const refining = model.varById.get('production.refining') ?? null;
  if (!minerals.length) return null;
  return (
    <section aria-labelledby="minerals-h" className="mt-10">
      <h2 id="minerals-h" className="rule-neis-sm mb-4 text-xl font-medium text-paper">
        Minerals
      </h2>
      <ul className="grid gap-3 sm:grid-cols-2 lg:grid-cols-4">
        {minerals.map(({ m }) => {
          const s = mine ? model.summaryLatest(m.idx, mine.idx, state.eu) : null;
          const r = refining ? model.summaryLatest(m.idx, refining.idx, state.eu) : null;
          const r25 = refining ? (model.summaryOf(m.idx, refining.idx, 2025, state.eu) ?? r) : null;
          return (
            <li key={m.slug}>
              <Link to={link(`/sector/minerals/${m.slug}`)} className="card block h-full p-4" aria-label={`${m.label}: ${s && mine ? `mine production ${fmtFact(model, mine, m, s.W)} ${s.year}, ${lensText(state.lens, s, model)}` : 'no mine production data'}${r25 ? `; refining China ${fmtShare(r25.china)}` : ''}`}>
                <div className="text-base font-medium text-paper">{m.label}</div>
                {s && mine ? (
                  <>
                    <div className="mt-1 text-xs text-paper-mute">
                      Mine production · {fmtFact(model, mine, m, s.W)} · {s.year}
                    </div>
                    <div className="mt-1 text-sm text-paper-dim">{lensText(state.lens, s, model)}</div>
                    <MicroBar s={s} />
                  </>
                ) : (
                  <div className="mt-1 text-xs text-paper-mute">No mine production data</div>
                )}
                {r25 && <div className="mt-2 text-xs text-paper-dim">Refining: China {fmtShare(r25.china)} ({r25.year})</div>}
              </Link>
            </li>
          );
        })}
      </ul>
    </section>
  );
}

function ThemeGlyph({ model, theme }: { model: SectorModel; theme: string }) {
  const vars = model.varsOfTheme(theme);
  const has = (cell: ItemInfo) => vars.some((v) => model.availOf(v, cell.idx).length > 0 || model.itemsOfCell(cell.idx).some((l) => model.availOf(v, l.item.idx).length > 0));
  return (
    <div className="mt-3 grid w-fit gap-0.5" style={{ gridTemplateColumns: `repeat(${STAGE_SLUGS.length}, minmax(0, 1fr))` }} aria-hidden="true">
      {model.cells
        .filter((c) => c.headline)
        .map((c) => (
          <span key={c.slug} className="inline-block h-2 w-2 border border-ink-500" style={{ background: has(c) ? '#6b7280' : 'transparent' }} title={model.cellLabel(c)} />
        ))}
    </div>
  );
}

function ThemeCards() {
  const model = useModel();
  const { link } = useSectorUrl();
  return (
    <section aria-labelledby="themes-h" className="mt-10">
      <h2 id="themes-h" className="rule-neis-sm mb-4 text-xl font-medium text-paper">
        Browse by theme
      </h2>
      <ul className="grid gap-4 sm:grid-cols-2 lg:grid-cols-5">
        {model.themes.map((t) => {
          const vars = model.varsOfTheme(t.slug);
          const sources = [...new Set(vars.map((v) => v.source.short))];
          const years = vars.flatMap((v) => model.availOf(v).flatMap((a) => [a.year_min, a.year_max])).filter((y): y is number => y !== null);
          const span = years.length ? (Math.min(...years) === Math.max(...years) ? String(Math.min(...years)) : `${Math.min(...years)}-${Math.max(...years)}`) : '';
          const cellsWith = model.cells.filter((c) => c.headline && vars.some((v) => model.availOf(v, c.idx).length > 0)).length;
          const nHeadline = model.cells.filter((c) => c.headline).length;
          return (
            <li key={t.slug}>
              <Link to={link(`/sector/explore/${t.slug}`)} className="card block h-full p-4">
                <h3 className="rule-neis-sm text-base font-medium text-paper">{t.label}</h3>
                {t.description && <p className="mt-2 text-sm text-paper-dim">{t.description}</p>}
                <p className="mt-2 text-xs text-paper-mute">
                  {vars.length} measure{vars.length === 1 ? '' : 's'} · {sources.slice(0, 2).join(', ')}
                  {sources.length > 2 ? ` and ${sources.length - 2} more` : ''}
                  {span ? ` · ${span}` : ''}
                </p>
                <ThemeGlyph model={model} theme={t.slug} />
                <span className="sr-only">Cell coverage: {cellsWith} of {nHeadline} cells.</span>
              </Link>
            </li>
          );
        })}
      </ul>
    </section>
  );
}

/** The first step of the sector path (IA refactor, brief s.5): a technology opens its chain (stage -> component -> profile). */
function TechPicker() {
  const { link } = useSectorUrl();
  return (
    <section aria-labelledby="techs-h" className="mb-10">
      <h2 id="techs-h" className="eyebrow rule-neis-sm mb-2">
        Pick a technology
      </h2>
      <p className="mb-3 max-w-3xl text-sm text-paper-mute">Each opens its supply chain at six stages ({STAGE_SLUGS.map((s) => STAGE_LABELS[s]).join(', ')}): pick a stage or one of its components for the sector profile.</p>
      <ul className="flex flex-wrap gap-2">
        {TECH_SLUGS.map((t) => (
          <li key={t}>
            <Link className="tag min-h-9" to={link(`/sector/${t}`)}>
              {TECH_LABELS[t]}
            </Link>
          </li>
        ))}
      </ul>
    </section>
  );
}

export default function SectorHomeView() {
  useTitle('Sectors', 'Sector Insights');
  const manifest = useSectorManifest();
  return (
    <div>
      <PageHead
        eyebrow="SECTORS · MEASURED DATA"
        title="Energy supply chains, measured"
        lede={
          <>
            Eleven energy technologies, each at six supply-chain stages, from extraction and refining through components and assembly to deployment and recovery: 66 sectors. Each tile of the sector grid shows who holds the most of that sector’s main measure; pick one for the full picture: trade, production, reserves, investment, prices and policy, in each source’s own units and year. The tiles carry no indices and no weights: they are the measured numbers the country rankings start from. One section is a recommendation instead:{' '}
            <a className="link" href="#posture">
              Strategic posture
            </a>
            , which says, at fixed thresholds, which economies should secure, compete in or deploy each sector.
          </>
        }
      />
      <TechPicker />
      <section aria-labelledby="questions-h" className="mb-10">
        <h2 id="questions-h" className="eyebrow rule-neis-sm mb-4">
          Start with a question
        </h2>
        <ul className="grid gap-4 sm:grid-cols-2 lg:grid-cols-3 xl:grid-cols-4">
          {QUESTIONS.map((q) => (
            <li key={q.id}>
              <QuestionCard q={q} headlines={manifest.data?.headlines} />
            </li>
          ))}
        </ul>
      </section>
      <section aria-labelledby="atlas-h">
        <h2 id="atlas-h" className="sr-only">
          Browse by sector
        </h2>
        <Atlas />
      </section>
      <div className="mt-10">
        <Section
          id="posture"
          title="Strategic posture"
          sub={
            <>
              <PostureBasisTag /> <SectorPostureIntro />
            </>
          }
        >
          <SectorPostureCard />
        </Section>
      </div>
      <MineralsStrip />
      <ThemeCards />
    </div>
  );
}
