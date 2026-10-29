// The four lenses at the top of the Sector profile (IA refactor 2026-10-16, brief s.6): Economic
// Opportunity, Energy Security, Policy, Climate. Each card: one sentence, one to three key figures, one
// small existing chart or list where it helps, and a link down to the section that holds the detail.
// Everything is read from data the profile already loads (sector facts, the technology's trade chunk,
// the sector-policy chunk) plus the Allied Industrial Base document (read-only); no new measure. When
// the URL names a trade sub-component (`sub`), the market and concentration figures follow it, as the
// Trade section and "Rank the countries" do.
//
// ClimateSection is the expanded Climate block further down the profile: Climate TRACE sector emissions
// for the sources mapped to the cell, with the match quality, the materials shown as context and the
// technology's IPCC lifecycle intensity (Climate.tsx; climate data change 2026-10-17), then the IEA's World
// pathways to 2050 for the sector (Stated Policies, Current Policies, Net Zero; IEA scenario projections,
// Nze.tsx; user request 2026-10-17), the Climate Action Tracker ratings of the economies that lead the
// sector and the IEA mineral demand outlook. The Climate lens adds one IEA pathway line under its figures.

import { useMemo, type ReactNode } from 'react';
import { Link } from 'react-router';
import { ClassChip } from '../../../aib/charts/ClassChip.tsx';
import type { AibDoc, AibNode } from '../../../aib/contract.ts';
import { compactTitle, fmtPp, fmtUsd } from '../../../aib/derive/format.ts';
import { taxonomyOf } from '../../../aib/derive/taxonomy.ts';
import { parseCellSlug, stageLabel } from '../../../lib/stages.ts';
import { compactHref } from '../../../aib/nav.ts';
import { TableView } from '../../../components/TableView.tsx';
import { isTechSlug, STAGE_LABELS, TECH_LABELS, type SectorPolicyChunk, type SectorTradeChunk } from '../../contract.ts';
import { useSectorManifest } from '../../data/SectorDataProvider.tsx';
import type { GeoInfo, ItemInfo, SectorModel, VarInfo } from '../../data/model.ts';
import { primaryMeasure } from '../../derive/cells.ts';
import { matchNodes, nodesOfCell, subsOfNodes } from '../../derive/chain.ts';
import { concentration, growth, growthKindWords, growthWords, isWorldBasis, type Concentration } from '../../derive/concentration.ts';
import { countsByCountry, instrumentOf, policyRecords, scopePolicies, statusClass, tagBit } from '../../derive/policy.ts';
import { hasSubTrade, subComponents, subLatestYear, subSlice, validSub } from '../../derive/subs.ts';
import { concentrationSummary, theName } from '../../derive/text.ts';
import { fmtHhi, fmtShare, lc, listText } from '../../format.ts';
import { useAibDoc } from '../aibData.ts';
import { climateLensFigures, ClimateEmissions } from './Climate.tsx';
import { NzeLensLine, NzePathways } from './Nze.tsx';
import { ClimateSources } from './ClimateSources.tsx';
import { EconomyLink, fmtFact, measureConc, Section, splitParts } from '../common.tsx';
import { useClimate, usePolicy, useTrade } from '../data.ts';
import { BlocBar } from '../kit.tsx';
import { useSectorUrl } from '../state.tsx';
import { baseYear, demandRows, MineralsUsed } from '../TechChainView.tsx';
import { useProfile } from './ctx.tsx';
import { actorNoun } from './Top.tsx';

/** The measure the market and concentration lenses read: the cell's primary measure, or one sub-component's exports. */
export interface LensBasis {
  v: VarInfo;
  item: ItemInfo;
  year: number;
  conc: Concentration;
  /** "world exports of polysilicon & wafers", "world manufacturing capacity" */
  what: string;
  growth: { value: number | null; isNew: boolean } | null;
  growthKind: string | null;
  /** the sub-component, when the URL names a valid one */
  sub: string | null;
}

/** The lens basis of a cell (pure; the trade chunk may be null). */
export function lensBasis(model: SectorModel, facts: Parameters<typeof baseYear>[1], cell: ItemInfo, eu: 'members' | 'bloc', trade: SectorTradeChunk | null, subWanted: string | null): LensBasis | null {
  const subs = trade && cell.has_trade && hasSubTrade(trade) ? subComponents(trade, model, cell.idx) : [];
  const sub = subs.length > 1 ? validSub(subs, subWanted) : null;
  if (sub && trade) {
    const year = subLatestYear(trade, cell.idx);
    const v = model.varById.get('trade.sub_exports') ?? model.varById.get('trade.exports');
    if (year !== null && v) {
      const sl = subSlice(trade, model, cell.idx, sub, year, 'exports', eu);
      const conc = concentration(sl.rows, model.geos, sl.world, 'sum_all');
      const w0 = year > 2020 ? (subSlice(trade, model, cell.idx, sub, 2020, 'exports', eu).world?.value ?? null) : null;
      const g = w0 !== null && conc.W !== null ? growth('pct_2020_2024', w0, conc.W) : null;
      return { v, item: cell, year, conc, what: `world exports of ${lc(sub)}`, growth: g, growthKind: g ? 'pct_2020_2024' : null, sub };
    }
  }
  const pm = primaryMeasure(model, cell);
  if (!pm) return null;
  const year = baseYear(model, facts, pm.v, pm.item, eu);
  if (year === null) return null;
  const m = measureConc(model, facts, pm.v, pm.item, year, eu, trade);
  if (!m) return null;
  const s = model.summaryOf(pm.item.idx, pm.v.idx, year, eu);
  const g = s && Number.isFinite(s.growth) ? { value: s.growth, isNew: false } : null;
  const what = isWorldBasis(m.conc.basis) ? `world ${lc(pm.v.short)}` : lc(pm.v.short);
  return { v: pm.v, item: pm.item, year, conc: m.conc, what, growth: g, growthKind: g ? (s?.growth_kind ?? null) : null, sub: null };
}

const cap = (s: string) => (s ? s.charAt(0).toUpperCase() + s.slice(1) : s);

/** The largest holders that are economies (or the EU), largest first. */
export function leaders(model: SectorModel, c: Concentration, n = 3): { g: GeoInfo; share: number | null; value: number }[] {
  const out: { g: GeoInfo; share: number | null; value: number }[] = [];
  for (const r of c.rows) {
    const g = model.geos[r.geo];
    if (!g || (g.kind !== 'country' && g.kind !== 'eu') || !(r.value > 0)) continue;
    out.push({ g, share: r.share, value: r.value });
    if (out.length >= n) break;
  }
  return out;
}

/** The AIB nodes of the cell (of one sub-component when one is shown), most exposed first. */
export function cellNodes(doc: AibDoc | null, model: SectorModel, cell: ItemInfo, trade: SectorTradeChunk | null, sub: string | null): AibNode[] {
  if (!doc) return [];
  let nodes: AibNode[];
  if (sub && trade) {
    const names = subComponents(trade, model, cell.idx).map((s) => s.name);
    nodes = matchNodes(names, doc.nodes, cell.slug).bySub.get(sub) ?? [];
  } else nodes = nodesOfCell(doc.nodes, cell.slug);
  return [...nodes].sort((a, z) => (z.exposure ?? -1) - (a.exposure ?? -1) || a.label.localeCompare(z.label, 'en'));
}

/** The Climate Action Tracker overall rating of an economy (an EU member without its own carries the EU's). */
export function catRatings(model: SectorModel, chunk: SectorPolicyChunk | null): (g: GeoInfo) => { rating: string; updated: string | null; viaEu: boolean } | null {
  const t = chunk?.['policy.cat'];
  const by = new Map<number, { rating: string; updated: string | null }>();
  if (t) for (let i = 0; i < t.nrow; i++) if (t.cols.overall[i]) by.set(t.cols.geo[i], { rating: t.cols.overall[i] as string, updated: t.cols.updated[i] });
  return (g) => {
    const own = by.get(g.idx);
    if (own) return { ...own, viaEu: false };
    const eu = g.eu_member && model.eu ? by.get(model.eu.idx) : undefined;
    return eu ? { ...eu, viaEu: true } : null;
  };
}

function Metric({ label, value, sub }: { label: ReactNode; value: ReactNode; sub?: ReactNode }) {
  return (
    <div className="flex items-baseline justify-between gap-3 border-t hairline pt-1.5">
      <dt className="text-xs text-paper-mute">{label}</dt>
      <dd className="min-w-0 text-right text-sm font-medium text-paper">
        <span className="tabular">{value}</span>
        {sub && <span className="block text-xs font-normal text-paper-mute">{sub}</span>}
      </dd>
    </div>
  );
}

function LensCard({ id, title, headline, metrics, children, more, note }: { id: string; title: string; headline: ReactNode; metrics: ReactNode; children?: ReactNode; more: { href: string; label: string }; note?: ReactNode }) {
  return (
    <article aria-labelledby={`${id}-h`} className="panel flex h-full min-w-0 flex-col p-4" data-testid={id}>
      <h3 id={`${id}-h`} className="eyebrow">
        {title}
      </h3>
      <p className="mt-2 text-[0.9375rem] leading-snug text-paper">{headline}</p>
      <dl className="mt-3 space-y-1.5">{metrics}</dl>
      {children && <div className="mt-3 text-sm">{children}</div>}
      {note && <p className="mt-3 text-xs text-paper-mute">{note}</p>}
      <div className="mt-auto pt-3">
        <a className="link text-sm" href={more.href}>
          {more.label} ↓
        </a>
      </div>
    </article>
  );
}

function Ranked({ title, rows }: { title: string; rows: { g: GeoInfo; text: string }[] }) {
  if (!rows.length) return null;
  return (
    <div>
      <div className="text-xs text-paper-mute">{title}</div>
      <ol className="mt-1 space-y-0.5">
        {rows.map((r, i) => (
          <li key={r.g.code} className="flex items-baseline justify-between gap-3">
            <span className="min-w-0 truncate">
              <span className="text-paper-mute">{i + 1}.</span> <EconomyLink g={r.g} />
            </span>
            <span className="tabular shrink-0 text-paper-mute">{r.text}</span>
          </li>
        ))}
      </ol>
    </div>
  );
}

function EconomicLens({ basis }: { basis: LensBasis | null }) {
  const { model, cell, facts, eu } = useProfile();
  const inv = model.varById.get('investment.annual');
  const invYear = inv && model.availOf(inv, cell.idx).length ? baseYear(model, facts, inv, cell, eu) : null;
  const invM = inv && invYear !== null ? measureConc(model, facts, inv, cell, invYear, eu) : null;
  const more = { href: '#markets', label: 'Competitiveness & markets' };
  if (!basis) return <LensCard id="lens-eo" title="Economic Opportunity" headline="No country-level measure of market size for this sector." metrics={null} more={more} />;
  const { v, item, year, conc } = basis;
  const share = v.unit.unit_kind === 'share' || v.unit.unit_kind === 'pct';
  const gw = basis.growth ? growthWords(basis.growth) : null;
  const kind = basis.growthKind ? growthKindWords(basis.growthKind) : '';
  const top = leaders(model, conc);
  const headline =
    conc.W === null || share
      ? `${cap(basis.what)}: ${conc.n} economies have a value in ${year}; ${top[0] ? `${top[0].g.name} has the most` : 'none leads'}.`
      : `${cap(basis.what)}: ${fmtFact(model, v, item, conc.W)} in ${year}${gw ? `, ${gw} (${kind})` : ''}.`;
  return (
    <LensCard
      id="lens-eo"
      title="Economic Opportunity"
      headline={headline}
      metrics={
        <>
          {conc.W !== null && !share && <Metric label={basis.sub ? 'World exports' : cap(basis.what)} value={fmtFact(model, v, item, conc.W)} sub={`${year} · ${v.source.short}`} />}
          {gw && <Metric label="Growth" value={gw} sub={kind} />}
          {inv && invM && invM.conc.W !== null && <Metric label="Investment" value={fmtFact(model, inv, cell, invM.conc.W)} sub={`${invYear} · ${inv.source.short}`} />}
        </>
      }
      more={more}
    >
      <Ranked title={`Largest ${actorNoun(v)}`} rows={top.map((r) => ({ g: r.g, text: fmtShare(r.share) }))} />
    </LensCard>
  );
}

function SecurityLens({ basis, nodes, doc }: { basis: LensBasis | null; nodes: AibNode[]; doc: AibDoc | null }) {
  const { model, highlight } = useProfile();
  const more = { href: '#security', label: 'Security & concentration' };
  const shock = doc ? (doc.meta.shocks.find((s) => s.id === doc.meta.headlineShock)?.label ?? null) : null;
  const c = basis?.conc ?? null;
  const top = c?.top1 ? model.geos[c.top1.geo] : null;
  // export shares read easily as production shares: say which one this is
  const byExports = basis?.v.family === 'exports';
  const holds = basis && c && top ? `${theName(top)} holds ${fmtShare(c.top1?.share)} of ${basis.what}; the three largest ${actorNoun(basis.v)} hold ${fmtShare(c.top3_share)}.` : '';
  const headline =
    basis && c && top && c.W !== null
      ? byExports ? `By export value, ${holds}` : cap(holds)
      : 'No measure with a world total to show how concentrated this sector is.';
  return (
    <LensCard
      id="lens-es"
      title="Energy Security"
      headline={headline}
      note={byExports && c && c.W !== null ? 'These shares are of export value, not production. Production and capacity, where the data hold them, are under Security & concentration.' : undefined}
      metrics={
        basis && c && c.W !== null ? (
          <>
            <Metric label="China’s share" value={fmtShare(c.split.china)} />
            <Metric label="Concentration (HHI)" value={fmtHhi(c.hhi, c.hhi_lower_bound)} sub={Number.isFinite(c.eff_n) ? `like ${c.eff_n.toFixed(1)} equal-sized ${actorNoun(basis.v)}` : undefined} />
          </>
        ) : null
      }
      more={more}
    >
      {basis && c && c.W !== null && <BlocBar parts={splitParts(c)} ariaLabel={concentrationSummary(c, model, basis.v, highlight)} label="Shares: China, coalition core, partners, rest of the world" />}
      {doc && (
        <div className="mt-3 border-t hairline pt-2" data-testid="lens-es-aib">
          <div className="eyebrow">Observed / modelled facts</div>
          {nodes.length ? (
            <ul className="mt-1.5 space-y-2">
              {nodes.slice(0, 3).map((n) => {
                const compact = n.compact ? doc.compacts.find((x) => x.id === n.compact) : null;
                return (
                  <li key={n.id}>
                    <div className="flex flex-wrap items-center gap-x-2 gap-y-1">
                      <span className="text-paper">{n.label}</span>
                      <ClassChip cls={n.cls} />
                    </div>
                    <div className="text-xs text-paper-mute">
                      Shock exposure {fmtShare(n.exposure)} · residual gap {fmtShare(n.residual)}
                      {compact ? (
                        <>
                          {' · '}
                          <Link className="link" to={compactHref(compact.id)}>
                            {compactTitle(compact, doc.nodes)}
                          </Link>
                        </>
                      ) : (
                        ' · no compact'
                      )}
                    </div>
                  </li>
                );
              })}
            </ul>
          ) : (
            <p className="mt-1 text-xs text-paper-mute">
              The Allied Industrial Base model has no {taxonomyOf(doc.meta).dictionary ? 'node with codes in' : 'supply-chain node for'} this {basis?.sub ? 'component' : 'sector'}.
            </p>
          )}
          {nodes.length > 0 && (
            <p className="mt-2 text-xs text-paper-mute">
              Allied Industrial Base model{shock ? `, ${lc(shock)}` : ''}: shock exposure is the share of demand lost; the residual gap is what even coordinated trade cannot restore.
              {taxonomyOf(doc.meta).dictionary ? ' The nodes are electro-industrial dictionary nodes whose codes sit here; each result is for the whole node.' : ''}
            </p>
          )}
        </div>
      )}
    </LensCard>
  );
}

function usePolicyFacts() {
  const { tech, stage } = useProfile();
  const manifest = useSectorManifest();
  const has = !!manifest.data?.chunks.some((c) => c.name === 'sector-policy');
  const h = usePolicy(has);
  const chunk = h.data;
  return useMemo(() => {
    if (!chunk) return { chunk: null, loading: has && h.status === 'loading', bit: -1, n: 0, inForce: 0, byC: [] as ReturnType<typeof countsByCountry>, main: null as null | { label: string; share: number } };
    const recs = policyRecords(chunk);
    const bit = tagBit(chunk['policy.tags'], TECH_LABELS[tech]);
    const scoped = tech === 'semiconductors' ? [] : scopePolicies(recs, { bit, stage, cross: false });
    const byC = countsByCountry(scoped);
    const inst = new Map<string, number>();
    for (const r of scoped) inst.set(instrumentOf(r.type), (inst.get(instrumentOf(r.type)) ?? 0) + 1);
    const best = [...inst.entries()].filter(([k]) => k !== 'Other').sort((a, z) => z[1] - a[1])[0];
    return { chunk, loading: false, bit, n: scoped.length, inForce: scoped.filter((r) => statusClass(r.status) === 'in_force').length, byC, main: best && scoped.length ? { label: best[0], share: best[1] / scoped.length } : null };
  }, [chunk, has, h.status, tech, stage]);
}

function PolicyLens({ p }: { p: ReturnType<typeof usePolicyFacts> }) {
  const { model, cell, tech } = useProfile();
  const { link } = useSectorUrl();
  const label = model.cellLabel(cell);
  const more = { href: '#policy-technology', label: 'Policy' };
  const top = p.byC[0] ? model.geos[p.byC[0].geo] : null;
  const headline = p.loading
    ? 'Loading the policy records…'
    : !p.chunk || tech === 'semiconductors'
      ? `No policy records for ${label} in the explorer.`
      : p.bit < 0
        ? `IEA Policies and Measures has no technology tag for ${TECH_LABELS[tech]}.`
        : p.n
          ? `${p.n} policies recorded for ${label} in ${p.byC.length} jurisdictions; ${top ? `${top.name} has the most (${p.byC[0].n})` : ''}.`
          : `No policies recorded for ${label}.`;
  return (
    <LensCard
      id="lens-policy"
      title="Policy"
      headline={headline}
      metrics={
        p.n > 0 ? (
          <>
            <Metric label="Recorded policies" value={p.n} sub="IEA Policies and Measures" />
            <Metric label="Still in force" value={p.inForce} />
            {p.main && <Metric label="Main instrument" value={p.main.label} sub={`${fmtShare(p.main.share, 0)} of records`} />}
          </>
        ) : null
      }
      note={
        <>
          A count measures how active policy is, not how strong or well funded it is.{' '}
          <Link className="link" to={link('/sector/explore/policy')}>
            Policy in every sector
          </Link>
        </>
      }
      more={more}
    >
      <Ranked title="Most active jurisdictions" rows={p.byC.slice(0, 3).map((c) => ({ g: model.geos[c.geo], text: String(c.n) })).filter((r) => !!r.g)} />
    </LensCard>
  );
}

function ClimateLens({ basis, chunk }: { basis: LensBasis | null; chunk: SectorPolicyChunk | null }) {
  const { model, tech, cell } = useProfile();
  const { link } = useSectorUrl();
  const { climate, has } = useClimate();
  const label = TECH_LABELS[tech];
  const demand = useMemo(() => demandRows(model, label, (slug) => link(`/sector/minerals/${slug}`)), [model, label, link]);
  const rate = useMemo(() => catRatings(model, chunk), [model, chunk]);
  const fig = useMemo(() => (climate ? climateLensFigures(climate, model, cell, tech) : null), [climate, model, cell, tech]);
  const lead = basis ? leaders(model, basis.conc) : [];
  if (fig || has) {
    return (
      <LensCard
        id="lens-climate"
        title="Climate"
        headline={fig ? fig.headline : 'Loading sector emissions (Climate TRACE)…'}
        metrics={fig ? fig.metrics.map((m) => <Metric key={m.label} label={m.label} value={m.value} sub={m.sub} />) : null}
        note={fig?.note}
        more={{ href: '#climate', label: 'Climate' }}
      >
        {climate?.nze ? <NzeLensLine d={climate.nze} cell={cell} /> : null}
      </LensCard>
    );
  }
  const d = demand[0];
  const headline = d
    ? `${label} uses ${listText(demand.slice(0, 3).map((r) => lc(r.label)))}. ${d.label} demand from ${label}: ${d.b.toLocaleString('en-US', { maximumFractionDigits: 0 })} kt now${Number.isFinite(d.a) ? `, ${d.a.toLocaleString('en-US', { maximumFractionDigits: 0 })} kt in 2040 on today’s stated policies (IEA)` : ''}.`
    : `Climate data for ${label} are limited: the climate-policy ratings of the economies that lead it are shown.`;
  return (
    <LensCard
      id="lens-climate"
      title="Climate"
      headline={headline}
      metrics={
        lead.length && chunk ? (
          <>
            {lead.map((r) => {
              const x = rate(r.g);
              return <Metric key={r.g.code} label={r.g.name} value={x ? x.rating : 'Not rated'} sub={x?.viaEu ? 'rating of the EU' : undefined} />;
            })}
          </>
        ) : null
      }
      note={
        <>
          {lead.length > 0 && chunk && <>Climate Action Tracker ratings of the largest {basis ? actorNoun(basis.v) : 'holders'}. </>}
          Sector-level climate data are limited to these; the explorer holds no emissions figures for this sector.
        </>
      }
      more={{ href: '#climate', label: 'Climate' }}
    />
  );
}

/** The four lenses, in one row on wide screens. */
export function Lenses() {
  const { model, cell, tech, stage, facts, eu } = useProfile();
  const { state, link } = useSectorUrl();
  const { trade } = useTrade(tech, cell.has_trade);
  const doc = useAibDoc();
  const basis = useMemo(() => lensBasis(model, facts, cell, eu, trade, state.sub), [model, facts, cell, eu, trade, state.sub]);
  const nodes = useMemo(() => cellNodes(doc, model, cell, trade, basis?.sub ?? null), [doc, model, cell, trade, basis?.sub]);
  const p = usePolicyFacts();
  return (
    <section id="overview" aria-labelledby="overview-h" className="mb-10 scroll-mt-[calc(var(--sector-bar-h,4.75rem)+1.5rem)]">
      <div className="mb-3 flex flex-wrap items-baseline justify-between gap-x-4 gap-y-1">
        <h2 id="overview-h" className="eyebrow rule-neis-sm">
          What to know about {basis?.sub ? basis.sub : model.cellLabel(cell)}
        </h2>
        {basis?.sub ? (
          <p className="text-sm text-paper-mute">
            One component of {TECH_LABELS[tech]} {STAGE_LABELS[stage]}: market and concentration follow it; policy and climate are for the whole sector.{' '}
            <Link className="link" to={link(`/sector/${tech}/${stage}`)}>
              Whole sector
            </Link>
          </p>
        ) : null}
      </div>
      <div className="grid gap-4 sm:grid-cols-2 xl:grid-cols-4">
        <EconomicLens basis={basis} />
        <SecurityLens basis={basis} nodes={nodes} doc={doc} />
        <PolicyLens p={p} />
        <ClimateLens basis={basis} chunk={p.chunk} />
      </div>
    </section>
  );
}

/** The expanded Climate block (section #climate). */
export function ClimateSection() {
  const { model, cell, tech, facts, eu } = useProfile();
  const { state, link } = useSectorUrl();
  const { trade } = useTrade(tech, cell.has_trade);
  const basis = useMemo(() => lensBasis(model, facts, cell, eu, trade, state.sub), [model, facts, cell, eu, trade, state.sub]);
  const p = usePolicyFacts();
  const rate = useMemo(() => catRatings(model, p.chunk), [model, p.chunk]);
  const rows = useMemo(() => (basis ? leaders(model, basis.conc, 10) : []).map((r) => ({ ...r, cat: rate(r.g) })), [basis, model, rate]);
  return (
    <div className="space-y-6">
      <p className="max-w-3xl text-sm text-paper-dim">
        Sector emissions by economy from Climate TRACE, for the Climate TRACE subsectors that match this sector, with how well each matches; the technology’s lifecycle intensity from the IPCC; where the IEA’s World pathways lead to 2050 under Stated Policies, Current Policies and Net Zero, with the 2030 milestones of its 2021 Net Zero roadmap; energy investment in the sector by region (IEA); for coal, oil and gas, World fossil CO2 from the fuel (Global Carbon Project); the Climate Action Tracker rating of the governments that lead the sector; and, for clean technologies, the IEA outlook for the minerals they use. Emissions are Climate TRACE and Global Carbon Project estimates, lifecycle intensity is the IPCC's literature median, investment is the IEA's estimate and ratings are Climate Action Tracker assessments: measured or modelled facts, not projections. The IEA pathways are scenario projections, labelled as such. No perspective setting changes any of them.
      </p>
      <ClimateEmissions model={model} cell={cell} tech={tech} chunk={p.chunk} />
      <NzePathways model={model} cell={cell} />
      <ClimateSources model={model} cell={cell} />
      {rows.length > 0 && p.chunk && (
        <div>
          <h3 className="mb-2 text-base font-medium text-paper">Climate Action Tracker ratings of the largest {basis ? actorNoun(basis.v) : 'holders'}</h3>
          <TableView
            rows={rows}
            caption={`Climate Action Tracker ratings of the largest ${basis ? actorNoun(basis.v) : 'holders'}`}
            rowKey={(r) => r.g.code}
            cols={[
              { id: 'c', header: 'Economy', value: (r) => r.g.name, cell: (r) => <EconomyLink g={r.g} />, minWidth: 150 },
              { id: 's', header: `Share of ${basis?.what ?? 'the total'}`, value: (r) => r.share, cell: (r) => fmtShare(r.share), num: true },
              { id: 'r', header: 'Climate Action Tracker rating', value: (r) => r.cat?.rating ?? 'Not rated', cell: (r) => `${r.cat?.rating ?? 'Not rated'}${r.cat?.viaEu ? ' (rating of the EU)' : ''}` },
              { id: 'u', header: 'Rated', value: (r) => r.cat?.updated ?? '' },
            ]}
          />
          <p className="mt-2 text-xs text-paper-mute">
            The rating grades a government’s overall climate policy, not this sector.{' '}
            <Link className="link" to={link('/sector/explore/policy', { m: 'policy.cat' })}>
              Every economy’s rating on a map
            </Link>
          </p>
        </div>
      )}
      <MineralsUsed tech={tech} headingLevel={3} />
    </div>
  );
}

/** A node's coordination dividend: the exported one, else optimized minus existing coverage. */
const dividendOf = (n: AibNode): number | null => n.dividend ?? (n.optimized !== null && n.existing !== null ? n.optimized - n.existing : null);

/**
 * The OSI technologies of a node: those of its six-stage cells (node.yfCells), as equals, in the Sector
 * order (design A1); [] for a node with no OSI cell.
 */
export function nodeTechLabels(n: Pick<AibNode, 'yfCells'>): string[] {
  const slugs = new Set(n.yfCells.map((c) => parseCellSlug(c)?.techSlug).filter((t): t is string => !!t));
  return Object.keys(TECH_LABELS).filter((t) => slugs.has(t) && isTechSlug(t)).map((t) => TECH_LABELS[t as keyof typeof TECH_LABELS]);
}

/**
 * Every Allied Industrial Base node of the cell (Security & concentration): model results, never
 * preference-adjusted. The nodes reach this six-stage cell through their own six-stage cells
 * (node.yfCells, ../../derive/chain.ts; design A2). A node's stage (node.stage) is where the
 * electro-industrial dictionary files it; its traded codes can sit in a sector of another stage (Li-ion
 * cells & packs, an Assembly node, also has codes in Electric Vehicles — Components), which the table says.
 */
export function AibNodesBlock() {
  const { model, cell, tech, stage } = useProfile();
  const { trade } = useTrade(tech, cell.has_trade);
  const doc = useAibDoc();
  const rows = useMemo(() => cellNodes(doc, model, cell, null, null), [doc, model, cell]);
  // the sub-components each node sits under (many-to-many: a dictionary node can span several)
  const subsOf = useMemo(() => {
    if (!doc || !trade || !cell.has_trade || !hasSubTrade(trade)) return new Map<string, string[]>();
    return subsOfNodes(matchNodes(subComponents(trade, model, cell.idx).map((x) => x.name), doc.nodes, cell.slug));
  }, [doc, trade, model, cell]);
  if (!doc || !rows.length) return null;
  const shock = doc.meta.shocks.find((x) => x.id === doc.meta.headlineShock)?.label ?? null;
  const tx = taxonomyOf(doc.meta);
  const apart = (n: AibNode) => (subsOf.get(n.id) ?? []).filter((s) => s.toLowerCase() !== n.label.toLowerCase());
  const here = STAGE_LABELS[stage];
  const otherStage = rows.filter((n) => n.stage && n.stage !== here);
  return (
    <Section
      id="aib-nodes"
      title="Allied Industrial Base nodes"
      sub={
        tx.dictionary
          ? `What the Allied Industrial Base model finds for the nodes of the electro-industrial dictionary whose traded codes sit in this sector${shock ? ` under its headline shock (${lc(shock)})` : ''}: how much demand the shock cuts off, how much coordinated trade among allies restores, what is left, and the policy class that follows. The nodes are not this sector's sub-sectors: each has its own supply-chain stage and reaches this sector through the codes it shares with it (a node can sit in several sectors, and its results are for the whole node).`
          : `What the Allied Industrial Base model finds for the supply-chain nodes of this sector${shock ? ` under its headline shock (${lc(shock)})` : ''}: how much demand the shock cuts off, how much coordinated trade among allies restores, what is left, and the policy class that follows.`
      }
      aside={<span className="eyebrow">Observed / modelled facts</span>}
    >
      <TableView
        rows={rows}
        caption={tx.dictionary ? 'Allied Industrial Base nodes with codes in this sector' : 'Allied Industrial Base nodes of this sector'}
        rowKey={(n) => n.id}
        cols={[
          { id: 'n', header: 'Node', value: (n) => n.label, cell: (n) => (apart(n).length ? <span>{n.label} <span className="text-xs text-paper-mute">(in {apart(n).join(', ')})</span></span> : n.label), minWidth: 150 },
          { id: 's', header: 'Supply-chain stage', value: (n) => (n.stage ? stageLabel(n.stage) : '–') },
          { id: 't', header: 'Technologies', value: (n) => nodeTechLabels(n).join(', ') || '–', minWidth: 160 },
          { id: 'c', header: 'Class', value: (n) => n.cls, cell: (n) => <ClassChip cls={n.cls} /> },
          { id: 'e', header: 'Shock exposure', value: (n) => n.exposure, cell: (n) => fmtShare(n.exposure), num: true },
          { id: 'd', header: 'Coordination dividend', value: (n) => dividendOf(n), cell: (n) => fmtPp(dividendOf(n)), num: true },
          { id: 'r', header: 'Residual gap', value: (n) => n.residual, cell: (n) => fmtShare(n.residual), num: true },
          { id: 'l', header: 'Supply lost', value: (n) => n.lostUsd, cell: (n) => fmtUsd(n.lostUsd), num: true },
          { id: 'k', header: 'Compact', value: (n) => n.compact ?? '', cell: (n) => { const k = n.compact ? doc.compacts.find((x) => x.id === n.compact) : null; return k ? <Link className="link" to={compactHref(k.id)}>{compactTitle(k, doc.nodes)}</Link> : <span className="text-paper-mute">none</span>; } },
        ]}
      />
      {otherStage.length > 0 && (
        <p className="text-xs text-paper-mute" data-testid="aib-nodes-stages">
          A node sits here because some of its traded codes are filed under {TECH_LABELS[tech]} — {here}; its own stage is where the dictionary files the node as a whole, so {listText(otherStage.map((n) => `${n.label} (${stageLabel(n.stage)})`))} {otherStage.length === 1 ? 'reads' : 'read'} another stage here.
        </p>
      )}
      <p className="text-xs text-paper-mute">
        Shock exposure: the share of demand the shock cuts off. Coordination dividend: the extra share restored when allies coordinate trade rather than keep today’s relationships. Residual gap: what even coordinated trade cannot restore, which needs new capacity. These do not change with any preference setting.{' '}
        <Link className="link" to="/aib">
          The Allied Industrial Base
        </Link>
      </p>
    </Section>
  );
}
