// Profile S1 Snapshot, S2 Who holds the market, S3 Rank the countries (spec_sector.md 4.3).

import { useMemo } from 'react';
import { StatTile, Select } from '../../../components/ui.tsx';
import { TableView } from '../../../components/TableView.tsx';
import { rowsToCsv } from '../../../lib/csv.ts';
import type { FamilyId } from '../../contract.ts';
import type { VarInfo } from '../../data/model.ts';
import { countryVars, entityVars, measureItem, primaryMeasure, primaryVar, shareVars } from '../../derive/cells.ts';
import { basisWords, growth, growthWords, isWorldBasis, type Concentration } from '../../derive/concentration.ts';
import { blocNote } from '../../derive/groups.ts';
import { slice } from '../../derive/summary.ts';
import { lc, fmtShare, fmtHhi, fmtSignedPct, ordinal } from '../../format.ts';
import { LeagueCard } from '../cards.tsx';
import { fmtFact, measureConc, SPLIT_COLUMNS, shareSplitRow } from '../common.tsx';
import { useTrade } from '../data.ts';
import { GroupLegend, SectorChartCard, ShareSplitChart, type ShareSplitRow, type VarRef } from '../kit.tsx';
import { useSectorUrl } from '../state.tsx';
import { baseYear } from '../TechChainView.tsx';
import { useCardCsv, useProfile } from './ctx.tsx';
import { yearText } from '../caption.ts';

/** "exporters", "investors", "producers": the holders of a measure, for "≈ 4.8 effective exporters". */
export function actorNoun(v: VarInfo): string {
  if (v.id === 'trade.imports') return 'importers';
  if (v.family === 'exports') return 'exporters';
  if (v.family === 'investment') return 'investors';
  if (v.family === 'production' || v.family === 'capacity') return 'producers';
  if (v.family === 'reserves') return 'holders';
  if (v.family === 'consumption') return 'consumers';
  return 'economies';
}

const SIZE_NOUN: Partial<Record<FamilyId, string>> = {
  exports: 'exports',
  production: 'production',
  capacity: 'capacity',
  reserves: 'reserves',
  investment: 'investment',
  consumption: 'consumption',
};

/**
 * The label of the size tile, by family and world basis (3.6: a listed sum is never a world total):
 * "World exports", "World production", "EGS potential (165 economies listed)".
 */
export function sizeLabel(v: VarInfo, c: Concentration): string {
  if (isWorldBasis(c.basis)) {
    const noun = SIZE_NOUN[v.family as FamilyId];
    return noun ? `World ${noun}` : 'World total';
  }
  return `${v.short} (${c.rows.length} economies listed)`;
}

/** Is the measure itself a share (no size to show: "Capacity share · 100.0%")? */
const isShareUnit = (v: VarInfo) => v.unit.unit_kind === 'share' || v.unit.unit_kind === 'pct';

/** S1 for a primary measure without a world basis (a ratio such as consumption per person): median, highest, coalition. */
function SnapshotNoBasis({ v, it, year, conc }: { v: VarInfo; it: import('../../data/model.ts').ItemInfo; year: number; conc: Concentration }) {
  const { model, highlight } = useProfile();
  const rows = conc.rows.filter((r) => model.geos[r.geo]?.kind === 'country' && Number.isFinite(r.value));
  const sorted = rows.map((r) => r.value).sort((a, z) => a - z);
  const med = (xs: number[]) => (xs.length ? (xs.length % 2 ? xs[xs.length >> 1] : (xs[(xs.length >> 1) - 1] + xs[xs.length >> 1]) / 2) : NaN);
  const coal = rows.filter((r) => ['core', 'partner'].includes(model.geos[r.geo].group)).map((r) => r.value).sort((a, z) => a - z);
  const nCoal = model.countries.filter((g) => g.group === 'core' || g.group === 'partner').length;
  const top = rows[0] ? model.geos[rows[0].geo] : null;
  const yr = yearText(v, { var: v.id, item: it.slug, year });
  const caption = `${v.label} · ${model.unitOf(v, it.idx).long} · ${yr} · ${v.source.short}`;
  const hRow = highlight ? rows.find((r) => r.geo === highlight.idx) : null;
  const tiles = [
    { key: 'med', label: `${v.short}: median`, value: Number.isFinite(med(sorted)) ? fmtFact(model, v, it, med(sorted)) : '–', sub: `${rows.length} economies · ${yr}` },
    { key: 'top', label: 'Highest', value: top ? `${top.name} ${fmtFact(model, v, it, rows[0].value)}` : '–', sub: `${lc(v.short)} · ${yr}` },
    { key: 'coal', label: 'Coalition median', value: Number.isFinite(med(coal)) ? fmtFact(model, v, it, med(coal)) : '–', sub: `${coal.length} of ${nCoal} members with data` },
  ];
  if (highlight) {
    const rank = hRow ? 1 + rows.filter((r) => r.value > hRow.value).length : null;
    tiles.push({ key: 'hl', label: 'Highlight', value: hRow ? `${highlight.name} ${fmtFact(model, v, it, hRow.value)}` : highlight.name, sub: hRow && rank !== null ? `${ordinal(rank)} of ${rows.length}` : `not covered by ${v.source.short}` });
  }
  return (
    <div className="space-y-2">
      <div className="grid grid-cols-2 gap-3 md:grid-cols-3 xl:grid-cols-6" role="list" aria-label="Snapshot">
        {tiles.map((t) => (
          <div key={t.key} role="listitem" title={caption}>
            <StatTile label={t.label} value={<span className="font-medium">{t.value}</span>} sub={t.sub} />
          </div>
        ))}
      </div>
      <p className="text-xs text-paper-mute">{v.short} is a ratio with no world total, so the snapshot gives the spread across economies, not shares.</p>
    </div>
  );
}

/** S1: a KPI row, not a chart. */
export function Snapshot() {
  const { model, cell, tech, facts, highlight, eu } = useProfile();
  const { trade } = useTrade(tech, eu === 'bloc' && cell.has_trade);
  const pm = primaryMeasure(model, cell);
  const v = pm?.v ?? null;
  const it = pm?.item ?? cell;
  const year = v ? baseYear(model, facts, v, it, eu) : null;
  const m = v && year !== null ? measureConc(model, facts, v, it, year, eu, trade) : null;
  const s = v && year !== null ? model.summaryOf(it.idx, v.idx, year, eu) : null;
  const inv = model.varById.get('investment.annual');
  const invYear = inv && model.availOf(inv, cell.idx).length ? baseYear(model, facts, inv, cell, eu) : null;
  const invM = inv && invYear !== null ? measureConc(model, facts, inv, cell, invYear, eu) : null;
  const invW = (y: number) => {
    if (!inv || !facts) return null;
    const sl = slice(model, facts, inv, cell.idx, y, eu);
    return sl.world?.value ?? sl.rows.reduce((a, r) => a + (Number.isFinite(r.value) ? r.value : 0), 0);
  };
  if (v && m && year !== null && m.conc.W === null && m.conc.rows.length) return <SnapshotNoBasis v={v} it={it} year={year} conc={m.conc} />;
  if (!v || !m || m.conc.W === null || year === null) {
    return <p className="text-sm text-paper-mute">No country-level primary measure for this sector.</p>;
  }
  const c = m.conc;
  const yr = yearText(v, { var: v.id, item: it.slug, year });
  const bw = basisWords(c.basis, v.source.short);
  const top = c.top1 ? model.geos[c.top1.geo] : null;
  const g = s && Number.isFinite(s.growth) ? { value: s.growth, isNew: false } : null;
  const y0 = s?.growth_kind === 'pct_2020_2024' ? 2020 : s?.growth_kind === 'cagr_2018_2025' ? 2018 : null;
  const hRow = highlight ? c.rows.find((r) => r.geo === highlight.idx) : null;
  const caption = `${v.label} · ${model.unitOf(v, it.idx).long} · ${yr} · ${v.source.short}`;
  const tiles: { key: string; label: string; value: string; sub: string; title: string }[] = [];
  // the size tile, labelled by family and basis; none when the measure is itself a share
  if (!isShareUnit(v)) {
    tiles.push({
      key: 'size',
      label: sizeLabel(v, c),
      value: fmtFact(model, v, it, c.W as number),
      sub: `${isWorldBasis(c.basis) ? `World ${lc(v.short)}` : `${v.short}, listed by ${v.source.short}`} ${yr}${g && y0 ? ` · ${growthWords(g)} since ${y0}${v.unit.unit_kind === 'usd' ? ', nominal' : ''}` : ''}`,
      title: caption,
    });
  }
  if (inv && invM && invM.conc.W !== null && invYear !== null) {
    const w18 = invW(2018);
    const w22 = invW(2022);
    const wN = invM.conc.W;
    const c18 = w18 !== null ? growth('cagr_2018_2025', w18, wN) : null;
    const c22 = w22 !== null ? growth('cagr_2022_2025', w22, wN) : null;
    tiles.push({
      key: 'inv',
      label: 'Investment',
      value: fmtFact(model, inv, cell, wN),
      sub: `${inv.short} ${invYear}${c18?.value !== null && c18 ? ` · CAGR 2018-25 ${fmtSignedPct(c18.value)}` : ''}${c22?.value !== null && c22 ? `; 2022-25 ${fmtSignedPct(c22.value)}` : ''}`,
      title: `${inv.label} · ${inv.unit.long} · ${invYear} · ${inv.source.short}`,
    });
  }
  tiles.push(
    { key: 'top', label: 'Largest player', value: top ? `${top.name} ${fmtShare(c.top1?.share)}` : '–', sub: isWorldBasis(c.basis) ? `of world ${lc(v.short)} ${yr}` : `${bw} · ${lc(v.short)} ${yr}`, title: caption },
    { key: 'hhi', label: 'Concentration', value: `HHI ${fmtHhi(c.hhi, c.hhi_lower_bound)}`, sub: `≈ ${Number.isFinite(c.eff_n) ? c.eff_n.toFixed(1) : '–'} effective ${actorNoun(v)} · top-3 ${fmtShare(c.top3_share)}`, title: caption },
    { key: 'coal', label: 'Coalition', value: fmtShare(c.split.core + c.split.partner), sub: `core ${fmtShare(c.split.core)} · partners ${fmtShare(c.split.partner)}${eu === 'bloc' ? ' · EU-27 counted as one' : ''}`, title: caption },
  );
  if (highlight) {
    tiles.push({
      key: 'hl',
      label: 'Highlight',
      value: hRow ? `${highlight.name} ${fmtShare(hRow.share)}` : highlight.name,
      sub: hRow ? (hRow.value > 0 && hRow.rank <= c.n ? `${ordinal(hRow.rank)} of ${c.n}` : `none recorded · ${c.n} economies with a value`) : m.fromFacts ? `not covered by ${v.source.short}` : 'loading the sector facts',
      title: caption,
    });
  }
  return (
    <div className="grid grid-cols-2 gap-3 md:grid-cols-3 xl:grid-cols-6" role="list" aria-label="Snapshot">
      {tiles.map((t) => (
        <div key={t.key} role="listitem" title={t.title}>
          <StatTile label={t.label} value={<span className="font-medium">{t.value}</span>} sub={t.sub} />
        </div>
      ))}
    </div>
  );
}

/** S2: does the answer change with the measure? */
export function Who() {
  const { model, cell, tech, facts, highlight, eu } = useProfile();
  const { trade } = useTrade(tech, eu === 'bloc' && cell.has_trade);
  const { rows, refs, entity, entityRefs } = useMemo(() => {
    const rows: ShareSplitRow[] = [];
    const refs: VarRef[] = [];
    for (const v of shareVars(model, cell)) {
      const it = measureItem(model, cell, v) ?? cell;
      const year = baseYear(model, facts, v, it, eu);
      if (year === null) continue;
      const m = measureConc(model, facts, v, it, year, eu, trade);
      if (!m || m.conc.W === null) continue;
      const label = v.id === 'trade.imports' ? `Imports ${year} (who buys)` : v.id === 'trade.atlas_share' ? `Atlas world-export share ${year}` : `${v.short}${it !== cell ? ` (${lc(it.label)})` : ''} ${year}`;
      rows.push(shareSplitRow(model, v, it, m.conc, { label, year, highlight, sub: `${model.unitOf(v, it.idx).short} · ${v.source.short}` }));
      refs.push({ var: v.id, item: it.slug, year });
    }
    const entity: ShareSplitRow[] = [];
    const entityRefs: VarRef[] = [];
    for (const v of entityVars(model, cell)) {
      const year = facts?.latestYear(v.idx, cell.idx) ?? null;
      if (year === null || !facts) continue;
      const m = measureConc(model, facts, v, cell, year, 'members');
      if (!m || m.conc.W === null) continue;
      entity.push(shareSplitRow(model, v, cell, m.conc, { label: `${v.short} ${year}${v.projection_from !== null && year >= v.projection_from ? ' (projection)' : ''}`, year, projection: v.projection_from !== null && year >= v.projection_from, sub: `${v.native_level === 'entity' ? 'entity level' : 'regions'} · ${v.source.short}` }));
      entityRefs.push({ var: v.id, item: cell.slug, year });
    }
    return { rows, refs, entity, entityRefs };
  }, [model, cell, facts, eu, trade, highlight]);
  const csv = useCardCsv(() => ({
    name: `${cell.slug}-who-holds`,
    text: rowsToCsv([...rows, ...entity].map((r) => ({ measure: r.label, ...Object.fromEntries(Object.entries(r.parts).map(([k, x]) => [`share_${k}`, x])), top1: r.cols.top1, top3: r.cols.top3, hhi: r.cols.hhi, effective_n: r.cols.effn, n: r.cols.n, source: r.cols.source }))),
  }), refs.map((r) => r.var));
  if (!rows.length && !entity.length) return null;
  return (
    <SectorChartCard
      title="Who holds the market"
      vars={[...refs, ...entityRefs]}
      table={() => (
        <TableView
          rows={[...rows, ...entity]}
          caption="Who holds the market"
          cols={[
            { id: 'm', header: 'Measure', value: (r) => r.label },
            { id: 'china', header: 'China', value: (r) => r.parts.china ?? null, cell: (r) => fmtShare(r.parts.china), num: true },
            { id: 'core', header: 'Core', value: (r) => r.parts.core ?? null, cell: (r) => fmtShare(r.parts.core), num: true },
            { id: 'partner', header: 'Partners', value: (r) => r.parts.partner ?? null, cell: (r) => fmtShare(r.parts.partner), num: true },
            { id: 'rest', header: 'Rest', value: (r) => r.parts.rest ?? null, cell: (r) => fmtShare(r.parts.rest), num: true },
            { id: 'un', header: 'Not attributed', value: (r) => r.parts.unattributed ?? null, cell: (r) => fmtShare(r.parts.unattributed), num: true },
            { id: 'top1', header: 'Top-1', value: (r) => r.cols.top1 },
            { id: 'hhi', header: 'HHI', value: (r) => r.cols.hhi, num: true },
            { id: 'src', header: 'Source', value: (r) => r.cols.source },
          ]}
        />
      )}
      csv={csv}
      legend={<GroupLegend highlight={highlight?.name ?? null} projection={entity.some((r) => r.projection)} euBloc={eu === 'bloc'} mixed={entityRefs.some((r) => model.varById.get(r.var)?.native_level === 'region')} />}
      notes={
        <>
          <p>Shares are within each measure; rows differ in source and year and do not add up.</p>
          {cell.has_trade && <p>Exports are technology-attributed: some HS6 codes also count in other sectors.</p>}
          {eu === 'bloc' && <p>{blocNote(model)}</p>}
        </>
      }
      summary={rows.map((r) => r.ariaLabel).filter(Boolean).slice(0, 3).join(' ')}
    >
      {rows.length > 0 && (
        <ShareSplitChart rows={rows} columns={SPLIT_COLUMNS} ariaLabel={`Who holds ${model.cellLabel(cell)}, by measure`} highlightHeader={highlight?.name} onRowClick={undefined} />
      )}
      {entity.length > 0 && (
        <div className="mt-5">
          <h4 className="mb-2 text-sm font-medium text-paper-dim">Measures reported for regions or entities</h4>
          <ShareSplitChart rows={entity} columns={SPLIT_COLUMNS.filter((c) => c.key !== 'effn')} ariaLabel="Measures reported for regions or entities" minLabelShare={0.06} />
          <p className="mt-2 text-xs text-paper-mute">Entities are labelled because hues repeat: the EU and the United States are both coalition core.</p>
        </div>
      )}
    </SectorChartCard>
  );
}

/** S3: rank the countries on any country-level measure of the cell. */
export function Leaders() {
  const { model, cell, tech, facts, highlight, eu } = useProfile();
  const { state, set } = useSectorUrl();
  const vars = countryVars(model, cell);
  const primary = primaryVar(model, cell);
  const v: VarInfo | null = (state.m && vars.find((x) => x.id === state.m)) || (primary && vars.includes(primary) ? primary : vars[0]) || null;
  const it = (v && measureItem(model, cell, v)) || cell;
  const { trade } = useTrade(tech, !!v && eu === 'bloc' && v.id.startsWith('trade.') && cell.has_trade);
  const years = v && facts ? facts.yearsOf(v.idx, it.idx) : [];
  const yearOk = state.y !== null && years.includes(state.y);
  const year = yearOk ? (state.y as number) : v ? baseYear(model, facts, v, it, eu) : null;
  const sl = useMemo(() => (v && facts && year !== null ? slice(model, facts, v, it.idx, year, eu, trade) : null), [v, facts, year, model, it, eu, trade]);
  const change = useMemo(() => {
    if (!v || !facts || year === null || years.length < 2) return undefined;
    const y0 = years[0];
    if (y0 === year) return undefined;
    const m = new Map<number, string>();
    for (const r of facts.rows(v.idx, it.idx, year)) {
      const b = facts.value(v.idx, it.idx, y0, r.geo);
      if (!b) continue;
      const kind = year - y0 === 4 ? 'pct_2020_2024' : 'cagr_2018_2025';
      const g = growth(kind as 'pct_2020_2024', b.value, r.value);
      m.set(r.geo, g.isNew ? 'new' : g.value !== null ? `${fmtSignedPct(g.value)} since ${y0}` : '–');
    }
    return m;
  }, [v, facts, year, years, it]);
  if (!v) return null;
  const groups = (f: FamilyId) => model.families.get(f)?.label ?? f;
  return (
    <LeagueCard
      model={model}
      v={v}
      item={it}
      year={year}
      rows={sl?.rows ?? []}
      world={sl?.world}
      title={`Rank the countries: ${lc(v.short)}`}
      highlight={highlight}
      map
      change={change}
      diverging={v.id === 'trade.net' ? { negLabel: 'net importer ←', posLabel: '→ net exporter' } : undefined}
      tools={
        <>
          <Select label="Measure" value={v.id} options={vars.map((x) => { const xi = measureItem(model, cell, x); return { value: x.id, label: xi && xi !== cell ? `${x.short} (${lc(xi.label)})` : x.short, group: groups(x.family) }; })} onChange={(id) => set({ m: id, y: null })} className="min-w-[10rem]" />
          {years.length > 1 && <Select label="Year" value={String(year)} options={years.map((y) => ({ value: String(y), label: String(y) }))} onChange={(y) => set({ y: Number(y) })} className="min-w-[6rem]" />}
        </>
      }
      notes={eu === 'bloc' && sl?.mode === 'undefined' ? <p>Not defined for the EU as a bloc: shown per member.</p> : eu === 'bloc' && sl?.mode === 'needs_trade' ? <p>EU as one bloc needs the trade data; member rows shown until it loads.</p> : null}
    />
  );
}
