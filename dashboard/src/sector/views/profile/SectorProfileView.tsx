// Sector profile /sector/:tech/:stage (spec_sector.md 4.3): the core page. Sections appear in
// rendering order only when the cell has data for them (S7 Investment before S5 Production); the rest
// are listed under "Not available for this sector" with their reason. Sections mount when they come
// within 800 px of the viewport, which also triggers their chunk. Footer: not available, sources on
// this page, "Download this sector (.zip)".

import { useMemo, useState, type ReactNode } from 'react';
import { Link, Navigate, useLocation, useNavigate, useParams } from 'react-router';
import { TableView, type Col } from '../../../components/TableView.tsx';
import { Notice, PageHead, StatTile } from '../../../components/ui.tsx';
import { downloadBlob } from '../../../lib/download.ts';
import { withBom } from '../../../lib/csv.ts';
import { useTitle } from '../../../shell/title.ts';
import { STAGE_COLOR } from '../../../theme/palette.ts';
import { FAMILY_SECTION, isStageSlug, isTechSlug, STAGE_LABELS, STAGE_SLUGS, TECH_LABELS, type SectionId, type SectorManifest, type StageSlug, type TechSlug } from '../../contract.ts';
import { useModel, useSectorManifest } from '../../data/SectorDataProvider.tsx';
import type { FactsIndex } from '../../data/facts.ts';
import type { GeoInfo, ItemInfo, SectorModel, VarInfo } from '../../data/model.ts';
import { countryVars, entityVars, isThin, measureItem, primaryMeasure, primaryVar, reasonText, shareVars, thinNotice } from '../../derive/cells.ts';
import { GROUP_SHORT, isCoalition, partOf } from '../../derive/groups.ts';
import { slice } from '../../derive/summary.ts';
import { stateOf, stateShort } from '../../derive/states.ts';
import { sectorCsv, type CsvInput } from '../../derive/csv.ts';
import { lc, fmtShare, fmtValue } from '../../format.ts';
import { ChunkGate, Section, useHighlight } from '../common.tsx';
import { useFacts, useNear, useScrollToHash } from '../data.ts';
import { CoverageChips, SectionRail } from '../kit.tsx';
import { useSectorUrl } from '../state.tsx';
import { baseYear, chipsOf } from '../TechChainView.tsx';
import { CsvRegistryProvider, ProfileProvider, useCardCsv, useCsvRegistry, useProfile, vOf, itemForCell, linkedWith } from './ctx.tsx';
import { Leaders, Snapshot, Who } from './Top.tsx';
import { TradeSection } from './Trade.tsx';
import { InvestmentSection } from './Investment.tsx';
import { ProductionSection, hasProduction } from './Production.tsx';
import { ResourcesSection, hasResources } from './Resources.tsx';
import { DemandSection, hasDemand } from './Demand.tsx';
import { PricesSection, hasPrices } from './Prices.tsx';
import { PolicySection } from './Policy.tsx';

export const SECTION_LABEL: Record<SectionId, string> = {
  snapshot: 'Snapshot',
  who: 'Who holds the market',
  leaders: 'Rank the countries',
  trade: 'Trade',
  investment: 'Investment',
  production: 'Production & capacity',
  resources: 'Resources & potential',
  demand: 'Demand & deployment',
  prices: 'Prices & costs',
  policy: 'Policy activity',
  technology: 'Technology readiness',
  context: 'Stage context',
  countries: 'Countries in this sector',
};

/** Which sections a cell renders (in rendering order). */
export function profileSections(model: SectorModel, cell: ItemInfo, hasPolicy: boolean): SectionId[] {
  const out: SectionId[] = [];
  if (primaryVar(model, cell)) out.push('snapshot');
  if (shareVars(model, cell).length || entityVars(model, cell).length) out.push('who');
  if (countryVars(model, cell).length) out.push('leaders');
  if (cell.has_trade) out.push('trade');
  const inv = ['investment.annual', 'investment.power_additions'].map((id) => vOf(model, id)).some((v) => v && model.availOf(v, cell.idx).length);
  if (inv) out.push('investment');
  if (hasProduction(model, cell)) out.push('production');
  if (hasResources(model, cell)) out.push('resources');
  if (hasDemand(model, cell)) out.push('demand');
  if (hasPrices(model, cell)) out.push('prices');
  if (hasPolicy && cell.techSlug && cell.techSlug !== 'semiconductors') out.push('policy');
  const trl = vOf(model, 'technology.trl');
  if (trl && model.availOf(trl, cell.idx).length) out.push('technology');
  if (contextItem(model, cell) || vOf(model, 'prices.lending_rate')) out.push('context');
  if (countryVars(model, cell).length) out.push('countries');
  return out;
}

/** The activity item (ILO wages) of a cell's stage. */
export function contextItem(model: SectorModel, cell: ItemInfo): ItemInfo | null {
  const w = vOf(model, 'prices.wages');
  if (!w) return null;
  const linked = linkedWith(model, cell, 'activity', w);
  if (linked.length) return linked[0];
  const stage = (cell.stage ?? '').toLowerCase();
  return model.items.find((x) => x.axis === 'activity' && (x.slug === cell.stageSlug || lc(x.label).includes(stage)) && model.availOf(w, x.idx).length) ?? null;
}

/** A section that mounts its content when it nears the viewport. */
function Lazy({ id, title, sub, children }: { id: SectionId; title: string; sub?: ReactNode; children: () => ReactNode }) {
  const [ref, near] = useNear<HTMLDivElement>(800);
  return (
    <div ref={ref}>
      <Section id={id} title={title} sub={sub}>
        {near ? children() : <div style={{ minHeight: 240 }} aria-hidden="true" />}
      </Section>
    </div>
  );
}

function Technology() {
  const { model, cell, facts } = useProfile();
  const trl = vOf(model, 'technology.trl');
  const chg = vOf(model, 'technology.trl_change');
  const wld = model.world;
  if (!trl || !facts || !wld) return null;
  const y = facts.latestYear(trl.idx, cell.idx);
  const t = y !== null ? facts.value(trl.idx, cell.idx, y, wld.idx) : null;
  const yc = chg ? facts.latestYear(chg.idx, cell.idx) : null;
  const d = chg && yc !== null ? facts.value(chg.idx, cell.idx, yc, wld.idx) : null;
  const fallback = model.caveatTexts(trl).some((c) => /technology-wide|fallback/i.test(c)) && false;
  return (
    <div className="grid max-w-xl grid-cols-2 gap-3">
      <div title={`${trl.label} · ${trl.unit.long} · ${y} · ${trl.source.short}`}>
        <StatTile label={`Readiness ${y ?? ''}`} value={t ? fmtValue(t.value, trl.unit, { scale: trl.display_scale }) : '–'} sub={`${trl.source.short}${fallback ? ' · technology-wide' : ''}`} />
      </div>
      {chg && (
        <div title={`${chg.label} · ${chg.source.short}`}>
          <StatTile label="Change 2020-25" value={d ? `${d.value > 0 ? '▲ +' : d.value < 0 ? '▼ ' : ''}${d.value.toFixed(1)} TRL` : '–'} sub={chg.source.short} />
        </div>
      )}
      <p className="col-span-2 text-sm text-paper-mute">A property of the technology, the same for every country: no map and no ranking.</p>
    </div>
  );
}

function Context() {
  const { model, cell, facts, highlight, eu } = useProfile();
  const w = vOf(model, 'prices.wages');
  const lr = vOf(model, 'prices.lending_rate');
  const ppi = vOf(model, 'prices.ppi_yoy');
  const act = contextItem(model, cell);
  const pm = primaryMeasure(model, cell);
  const prim = pm?.v ?? null;
  const pItem = pm?.item ?? cell;
  const rows = useMemo(() => {
    if (!facts) return [];
    let geos: GeoInfo[] = [];
    if (prim) {
      const y = baseYear(model, facts, prim, pItem, eu);
      if (y !== null) {
        const sl = slice(model, facts, prim, pItem.idx, y, 'members');
        geos = sl.rows.filter((r) => model.geos[r.geo]?.kind === 'country' && Number.isFinite(r.value)).sort((a, z) => z.value - a.value).slice(0, 15).map((r) => model.geos[r.geo]);
      }
    }
    if (highlight && highlight.kind === 'country' && !geos.includes(highlight)) geos.push(highlight);
    const get = (v: VarInfo | null, item: ItemInfo | null, g: GeoInfo) => {
      if (!v || !item) return null;
      const y = facts.latestYear(v.idx, item.idx);
      return y !== null ? facts.value(v.idx, item.idx, y, g.idx) : null;
    };
    return geos.map((g) => ({ g, wage: get(w, act, g), rate: get(lr, model.none, g), ppi: get(ppi, model.none, g) }));
  }, [facts, prim, pItem, model, eu, highlight, w, lr, ppi, act]);
  const cell3 = (v: VarInfo | null, f: { value: number; obs_year: number | null } | null) => {
    if (!v || !f) return '–';
    const old = v.stale_years !== null && f.obs_year !== null && new Date().getUTCFullYear() - 1 - f.obs_year > v.stale_years;
    return `${fmtValue(f.value, v.unit, { scale: v.display_scale, ctx: 'table' })}${f.obs_year !== null ? ` (${f.obs_year}${old ? ', old' : ''})` : ''}`;
  };
  const csv = useCardCsv(() => ({
    name: `${cell.slug}-stage-context`,
    text: sectorCsv(model, rows.flatMap((r) => {
      const out: CsvInput[] = [];
      if (w && r.wage) out.push({ geo: r.g, v: w, item: act, year: r.wage.obs_year, value: r.wage.value, status: r.wage.status });
      if (lr && r.rate) out.push({ geo: r.g, v: lr, item: model.none, year: r.rate.obs_year, value: r.rate.value, status: r.rate.status });
      if (ppi && r.ppi) out.push({ geo: r.g, v: ppi, item: model.none, year: r.ppi.obs_year, value: r.ppi.value, status: r.ppi.status });
      return out;
    })),
  }), [w?.id ?? '', lr?.id ?? '', ppi?.id ?? '']);
  return (
    <div>
      <TableView
        rows={rows}
        caption="Cost of labour and capital"
        rowKey={(r) => r.g.code}
        selectedKey={highlight?.code ?? null}
        cols={[
          { id: 'c', header: 'Economy', value: (r) => r.g.name, minWidth: 150 },
          { id: 'w', header: `Monthly earnings, ${act?.label.toLowerCase() ?? 'stage activity'} (USD; year)`, value: (r) => r.wage?.value ?? null, cell: (r) => cell3(w, r.wage), num: true },
          { id: 'r', header: 'Lending rate (% a year; year)', value: (r) => r.rate?.value ?? null, cell: (r) => cell3(lr, r.rate), num: true },
          { id: 'p', header: 'PPI (% y/y; year)', value: (r) => r.ppi?.value ?? null, cell: (r) => cell3(ppi, r.ppi), num: true },
        ]}
      />
      <div className="mt-2 flex flex-wrap items-start justify-between gap-x-4 gap-y-1 text-xs text-paper-mute">
        <span className="min-w-0 flex-1 basis-56">A table, not a chart: the years differ by country. Earnings: ILO, older than {w?.stale_years ?? 5} years marked "old"; lending rate: IMF (China: PBoC LPR); PPI: levels are never compared.</span>
        <button type="button" className="btn-quiet shrink-0" onClick={() => { const c = csv(); if (c) downloadBlob(new Blob([withBom(c.text)], { type: 'text/csv;charset=utf-8' }), `${c.name}.csv`); }}>
          Download CSV
        </button>
      </div>
    </div>
  );
}

type CountryRow = { g: GeoInfo; vals: Map<string, { value: number; status: string; share: number | null }>; n: number };

function Countries() {
  const { model, cell, facts, highlight, eu } = useProfile();
  const { link } = useSectorUrl();
  const navigate = useNavigate();
  const [filter, setFilter] = useState<'all' | 'coalition' | 'core' | 'partner' | 'china' | 'highlight'>('all');
  const vars = countryVars(model, cell);
  const prim = primaryVar(model, cell);
  const table = useMemo(() => {
    if (!facts) return { rows: [] as CountryRow[], heads: [] as { v: VarInfo; year: number; item: ItemInfo }[] };
    const heads: { v: VarInfo; year: number; item: ItemInfo }[] = [];
    const by = new Map<number, CountryRow>();
    for (const v of vars) {
      const item = measureItem(model, cell, v) ?? cell;
      const year = baseYear(model, facts, v, item, eu);
      if (year === null) continue;
      heads.push({ v, year, item });
      const sl = slice(model, facts, v, item.idx, year, eu);
      const W = sl.world?.value ?? null;
      for (const r of sl.rows) {
        const g = model.geos[r.geo];
        if (!g || (g.kind !== 'country' && g.kind !== 'eu')) continue;
        let row = by.get(r.geo);
        if (!row) by.set(r.geo, (row = { g, vals: new Map(), n: 0 }));
        row.vals.set(v.id, { value: r.value, status: r.status, share: W && Number.isFinite(r.value) ? r.value / W : null });
        row.n++;
      }
    }
    return { rows: [...by.values()], heads };
  }, [facts, vars, model, cell, eu]);
  const shown = table.rows.filter((r) => {
    switch (filter) {
      case 'coalition':
        return isCoalition(r.g);
      case 'core':
        return r.g.group === 'core';
      case 'partner':
        return r.g.group === 'partner';
      case 'china':
        return r.g.code === 'CHN';
      case 'highlight':
        return highlight?.idx === r.g.idx;
      default:
        return true;
    }
  });
  const cols: Col<CountryRow>[] = [
    { id: 'c', header: 'Economy', value: (r) => r.g.name, minWidth: 150 },
    { id: 'g', header: 'Group', value: (r) => GROUP_SHORT[partOf(r.g)] },
    ...table.heads.flatMap(({ v, year, item }) => {
      const unit = model.unitOf(v, item.idx);
      const base: Col<CountryRow> = {
        id: v.id,
        header: `${v.short}${item !== cell ? ` (${lc(item.label)})` : ''} ${year} (${unit.short || unit.long})`,
        value: (r) => r.vals.get(v.id)?.value ?? null,
        cell: (r) => {
          const x = r.vals.get(v.id);
          if (!x) return '–';
          return Number.isFinite(x.value) ? fmtValue(x.value, unit, { scale: v.display_scale, ctx: 'table' }) : stateShort(stateOf({ geo: r.g.idx, value: x.value, status: x.status as never, obs_year: null }, r.g, v, null), v);
        },
        num: true,
      };
      return v === prim && v.world_basis !== 'none' ? [base, { id: `${v.id}-share`, header: 'Share', value: (r: CountryRow) => r.vals.get(v.id)?.share ?? null, cell: (r: CountryRow) => fmtShare(r.vals.get(v.id)?.share), num: true }] : [base];
    }),
    { id: 'cov', header: 'Coverage', value: (r) => `${r.n} of ${table.heads.length}` },
  ];
  const csv = useCardCsv(() => ({
    name: `${cell.slug}-countries`,
    text: sectorCsv(model, table.rows.flatMap((r) => table.heads.filter((h) => r.vals.has(h.v.id)).map((h) => {
      const x = r.vals.get(h.v.id)!;
      return { geo: r.g, v: h.v, item: h.item, year: h.year, value: Number.isFinite(x.value) ? x.value : null, status: x.status, share: x.share };
    }))),
  }), vars.map((v) => v.id));
  const chips: { k: typeof filter; label: string }[] = [
    { k: 'all', label: 'All' },
    { k: 'coalition', label: 'Coalition' },
    { k: 'core', label: 'Core' },
    { k: 'partner', label: 'Partners' },
    { k: 'china', label: 'China' },
    ...(highlight ? [{ k: 'highlight' as const, label: 'Highlight' }] : []),
  ];
  return (
    <div>
      <div className="mb-3 flex flex-wrap items-center gap-2" role="group" aria-label="Filter the economies">
        {chips.map((c) => (
          <button key={c.k} type="button" className="tag" aria-pressed={filter === c.k} onClick={() => setFilter(c.k)}>
            {c.label}
          </button>
        ))}
        <button type="button" className="btn-quiet ml-auto" onClick={() => { const c = csv(); if (c) downloadBlob(new Blob([withBom(c.text)], { type: 'text/csv;charset=utf-8' }), `${c.name}.csv`); }}>
          Download CSV
        </button>
      </div>
      <TableView
        rows={shown}
        cols={cols}
        caption={`Countries in ${model.cellLabel(cell)}`}
        search
        pageSize={25}
        initialSort={table.heads[0] ? { id: table.heads[0].v.id, desc: true } : undefined}
        rowKey={(r) => r.g.code}
        selectedKey={highlight?.code ?? null}
        onRowClick={(r) => r.g.kind === 'country' && navigate(link(`/sector/${cell.techSlug}/${cell.stageSlug}/${r.g.code}`))}
      />
    </div>
  );
}

/** The files of "Download this sector (.zip)": every card's CSV, dictionary.csv (rows used), sources.csv, README.txt. */
export function sectorZipFiles(model: SectorModel, manifest: SectorManifest | null, cards: { name: string; text: string }[], usedVars: string[], url: string): Record<string, string> {
  const files: Record<string, string> = {};
  const seen = new Set<string>();
  for (const c of cards) {
    let name = `${c.name.replace(/[^a-z0-9._-]+/gi, '-')}.csv`;
    let k = 2;
    while (seen.has(name)) name = `${c.name}-${k++}.csv`;
    seen.add(name);
    files[name] = withBom(c.text);
  }
  const vars = model.vars.filter((v) => usedVars.includes(v.id));
  const dictRows = vars.map((v) => ({ var_id: v.id, label: v.label, unit: v.unit.long || v.unit.short, data_year: v.data_year, level: v.native_level, additive: v.agg, world_basis: v.world_basis, source: v.source.short, licence: v.licence, caveats: model.caveatTexts(v).join(' | ') }));
  const srcIds = [...new Set(vars.map((v) => v.source.id))];
  const srcRows = srcIds.map((id) => model.sources.get(id)).filter(Boolean).map((s) => ({ id: s!.id, citation: s!.citation, publisher: s!.publisher, edition: s!.edition, url: s!.url, licence: s!.licence, attribution: s!.attribution, accessed: s!.accessed }));
  const csvOf = (rows: Record<string, unknown>[]) => {
    const names = rows.length ? Object.keys(rows[0]) : [];
    const q = (x: unknown) => (x === null || x === undefined ? '' : /[",\r\n]/.test(String(x)) ? `"${String(x).replace(/"/g, '""')}"` : String(x));
    return [names.join(','), ...rows.map((r) => names.map((n) => q(r[n])).join(','))].join('\r\n') + '\r\n';
  };
  files['dictionary.csv'] = withBom(csvOf(dictRows));
  files['sources.csv'] = withBom(csvOf(srcRows));
  files['README.txt'] = [
    'Allied Industrial Base Explorer: Sector Insights',
    `Page: ${url}`,
    `Sector data bundle: ${manifest?.bundle_id ?? 'unknown'} (export ${manifest?.created ?? 'unknown'})`,
    '',
    'Measured values in each source’s own units and years; no index, weight or score values.',
    'Columns: iso3c, economy, group, tier, eu_member, var_id, measure, item, year, value, unit, status, share_of_world, world_basis, rank, source, licence.',
    'Status codes: obs observed; zero reported zero; nsr not reported separately (in the source residual); nq listed without a figure; mirror from partners’ declarations (x1.06); est estimate; na_growth base year zero.',
    'Licences: see sources.csv. Attribution is required for sources marked "attribution". Licensed data (BNEF, BCG) are not part of the public explorer.',
    '',
  ].join('\r\n');
  return files;
}

function ZipButton({ cell }: { cell: ItemInfo }) {
  const model = useModel();
  const reg = useCsvRegistry();
  const manifest = useSectorManifest();
  const loc = useLocation();
  const [busy, setBusy] = useState(false);
  return (
    <button
      type="button"
      className="btn btn-ghost btn-sm"
      disabled={busy}
      onClick={async () => {
        setBusy(true);
        try {
          const entries = reg?.all() ?? [];
          const cards = entries.map((e) => e.build()).filter((c): c is { name: string; text: string } => !!c);
          const used = [...new Set(entries.flatMap((e) => e.vars))];
          const files = sectorZipFiles(model, manifest.data, cards, used, `${window.location.origin}${loc.pathname}${loc.search}`);
          const { zipTexts } = await import('../../../lib/zip.ts');
          downloadBlob(new Blob([zipTexts(files) as BlobPart], { type: 'application/zip' }), `${cell.slug}.zip`);
        } finally {
          setBusy(false);
        }
      }}
    >
      {busy ? 'Preparing…' : 'Download this sector (.zip)'}
    </button>
  );
}

function NotAvailable({ sections }: { sections: SectionId[] }) {
  const { model, cell } = useProfile();
  const missing = model.coverageOf(cell.idx).filter((c) => (c.status === 'none' || c.status === 'licensed') && !sections.includes(FAMILY_SECTION[c.family] ?? 'snapshot'));
  if (!missing.length) return null;
  return (
    <section aria-labelledby="na-h" className="border-t hairline pt-6">
      <h2 id="na-h" className="text-base font-medium text-paper">
        Not available for this sector
      </h2>
      <ul className="mt-2 space-y-1 text-sm text-paper-dim">
        {missing.map((c) => (
          <li key={c.family}>
            <span className="text-paper">{model.families.get(c.family)?.label ?? c.family}</span>: {reasonText(c.reason)}
          </li>
        ))}
      </ul>
    </section>
  );
}

function SourcesOnPage() {
  const { model, cell } = useProfile();
  const vars = model.vars.filter((v) => model.availOf(v, cell.idx).length || model.itemsOfCell(cell.idx).some((l) => model.availOf(v, l.item.idx).length));
  const srcs = [...new Map(vars.map((v) => [v.source.id, v.source])).values()];
  if (!srcs.length) return null;
  return (
    <section aria-labelledby="src-h" className="border-t hairline pt-6">
      <h2 id="src-h" className="text-base font-medium text-paper">
        Sources on this page
      </h2>
      <ul className="mt-2 space-y-1 text-sm text-paper-dim">
        {srcs.map((s) => (
          <li key={s.id}>
            <span className="text-paper">{s.short}</span>
            {s.edition ? ` (${s.edition})` : ''}: {s.citation}
            {s.licence === 'attribution' && s.attribution ? ` ${s.attribution}` : ''}
          </li>
        ))}
      </ul>
      <p className="mt-2 text-sm">
        <Link className="link" to="/sector/sources">
          All sources and the variable dictionary
        </Link>
      </p>
    </section>
  );
}

function Header({ cell, tech, stage }: { cell: ItemInfo; tech: TechSlug; stage: StageSlug }) {
  const model = useModel();
  const { link } = useSectorUrl();
  const blurb = model.cellText.get(cell.slug)?.cell_blurb ?? `${model.cellLabel(cell)}: measured values from the sources listed below.`;
  return (
    <PageHead eyebrow={`SECTOR INSIGHTS · ${STAGE_LABELS[stage].toUpperCase()}`} title={model.cellLabel(cell)} lede={blurb}>
      <div className="mt-4 space-y-3">
        <CoverageChips chips={chipsOf(model, cell, (section) => `#${section}`)} ariaLabel={`Coverage of ${model.cellLabel(cell)}`} />
        <div className="flex flex-wrap items-center gap-4">
          <nav aria-label="Stages" className="flex items-center gap-1 text-sm">
            {STAGE_SLUGS.map((s, i) => {
              const exists = !!model.cell(tech, s);
              const cur = s === stage;
              return (
                <span key={s} className="flex items-center gap-1">
                  {i > 0 && <span className="text-paper-mute" aria-hidden="true">·</span>}
                  {exists ? (
                    <Link to={link(`/sector/${tech}/${s}`)} className="tag" aria-current={cur ? 'page' : undefined} style={{ borderLeft: `3px solid ${STAGE_COLOR[STAGE_LABELS[s]]}` }}>
                      {STAGE_LABELS[s]}
                    </Link>
                  ) : (
                    <span className="tag opacity-50" aria-disabled="true" title={`No ${STAGE_LABELS[s]} cell for ${TECH_LABELS[tech]}`}>
                      {STAGE_LABELS[s]}
                    </span>
                  )}
                </span>
              );
            })}
          </nav>
          <Link className="link text-sm" to={link(`/sector/${tech}`)}>
            The whole {TECH_LABELS[tech]} chain
          </Link>
          <ZipButton cell={cell} />
        </div>
      </div>
    </PageHead>
  );
}

function Body({ cell, tech, stage, facts }: { cell: ItemInfo; tech: TechSlug; stage: StageSlug; facts: FactsIndex | null }) {
  const model = useModel();
  const manifest = useSectorManifest();
  const hasPolicy = !!manifest.data?.chunks.some((c) => c.name === 'sector-policy');
  const sections = useMemo(() => profileSections(model, cell, hasPolicy), [model, cell, hasPolicy]);
  const loc = useLocation();
  useScrollToHash(loc.hash, !!facts);
  const thin = isThin(model, cell);
  return (
    <div className="xl:grid xl:grid-cols-[200px_minmax(0,1fr)] xl:gap-8">
      {/* < 1280 px: a sticky band under the SectorBar (its height is --sector-bar-h); >= 1280 px: the
          rail's own grid column (the rail sticks by itself), never a full-width band over the content */}
      <div
        className="sticky z-20 -mx-4 mb-6 border-b hairline bg-ink-900/95 px-4 py-2 sm:mx-0 sm:px-0 xl:static xl:z-auto xl:mx-0 xl:mb-0 xl:border-0 xl:bg-transparent xl:p-0"
        style={{ top: 'var(--sector-bar-h, 3.5rem)' }}
        data-testid="section-nav-band"
      >
        <SectionRail sections={sections.map((id) => ({ id, label: SECTION_LABEL[id] }))} />
      </div>
      <div className="min-w-0">
      {thin && (
        <div className="mb-6">
          <Notice kind="info">{thinNotice(model, cell)}</Notice>
          {cell.stageSlug === 'upstream' && model.itemsOfCell(cell.idx, 'mineral').length > 0 && (
            <p className="mt-2 text-sm text-paper-dim">
              Minerals:{' '}
              {model.itemsOfCell(cell.idx, 'mineral').map((l, i) => (
                <span key={l.item.slug}>
                  {i > 0 && ', '}
                  <Link className="link" to={`/sector/minerals/${l.item.slug}`}>
                    {l.item.label}
                  </Link>
                </span>
              ))}
            </p>
          )}
        </div>
      )}
      <div className="space-y-10">
        {sections.includes('snapshot') && (
          <Section id="snapshot" title="Snapshot">
            <Snapshot />
          </Section>
        )}
        {sections.includes('who') && (
          <Section id="who" title={SECTION_LABEL.who} sub="Does the answer change with the measure?">
            <Who />
          </Section>
        )}
        {sections.includes('leaders') && (
          <Section id="leaders" title={SECTION_LABEL.leaders}>
            <Leaders />
          </Section>
        )}
        {sections.includes('trade') && <Lazy id="trade" title={SECTION_LABEL.trade}>{() => <TradeSection />}</Lazy>}
        {sections.includes('investment') && <Lazy id="investment" title={SECTION_LABEL.investment}>{() => <InvestmentSection />}</Lazy>}
        {sections.includes('production') && <Lazy id="production" title={SECTION_LABEL.production}>{() => <ProductionSection />}</Lazy>}
        {sections.includes('resources') && <Lazy id="resources" title={SECTION_LABEL.resources}>{() => <ResourcesSection />}</Lazy>}
        {sections.includes('demand') && <Lazy id="demand" title={SECTION_LABEL.demand}>{() => <DemandSection />}</Lazy>}
        {sections.includes('prices') && <Lazy id="prices" title={SECTION_LABEL.prices}>{() => <PricesSection />}</Lazy>}
        {sections.includes('policy') && <Lazy id="policy" title={SECTION_LABEL.policy}>{() => <PolicySection />}</Lazy>}
        {sections.includes('technology') && (
          <Section id="technology" title={SECTION_LABEL.technology}>
            <Technology />
          </Section>
        )}
        {sections.includes('context') && (
          <Lazy id="context" title={SECTION_LABEL.context} sub="Not specific to this technology: the stage's activity in each economy.">
            {() => <Context />}
          </Lazy>
        )}
        {sections.includes('countries') && (
          <Lazy id="countries" title={SECTION_LABEL.countries}>
            {() => <Countries />}
          </Lazy>
        )}
        <NotAvailable sections={sections} />
        <SourcesOnPage />
      </div>
      </div>
      <span className="hidden">{tech}{stage}</span>
    </div>
  );
}

export default function SectorProfileView() {
  const { tech: rawT, stage: rawS } = useParams();
  const model = useModel();
  const { state } = useSectorUrl();
  const tech = rawT && isTechSlug(rawT) ? rawT : null;
  const stage = rawS && isStageSlug(rawS) ? rawS : null;
  const cell = tech && stage ? model.cell(tech, stage) : null;
  useTitle(cell ? model.cellLabel(cell) : 'Sectors', 'Sector Insights');
  const { facts, h } = useFacts();
  const highlight = useHighlight(model);
  if (!tech) return <Navigate to="/sector" replace state={{ notice: `No sector called '${rawT}'; showing all sectors.` }} />;
  if (!stage || !cell) {
    return <Navigate to={`/sector/${tech}`} replace state={{ notice: stage ? `${TECH_LABELS[tech]} has no ${STAGE_LABELS[stage]} cell; showing the whole chain.` : `No stage called '${rawS}'; showing the whole ${TECH_LABELS[tech]} chain.` }} />;
  }
  return (
    <ProfileProvider value={{ model, cell, tech, stage, facts, highlight, eu: state.eu }}>
      <CsvRegistryProvider>
        <Header cell={cell} tech={tech} stage={stage} />
        <ChunkGate h={h} name="sector-facts">
          {() => <Body cell={cell} tech={tech} stage={stage} facts={facts} />}
        </ChunkGate>
      </CsvRegistryProvider>
    </ProfileProvider>
  );
}

export { itemForCell };
