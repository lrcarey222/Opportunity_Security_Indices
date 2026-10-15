// Profile S10 Policy activity (spec_sector.md 4.3; chunk sector-policy, + sector-policy-text when the
// list opens). Scope: the technology's IEA tag (or a keyword rule, marked "text-derived"), the
// cross-cutting toggle (`xc`, off by default), and the stage. (a) policies by country, (b) new
// policies by year 2000-2030 (years after the data year are "announced or planned"), (c) what kind of
// policy (small multiples by instrument, one shared y), (d) the policy list. Counts measure recorded
// activity, not stringency or spending.

import { useCallback, useMemo, useState } from 'react';
import * as Plot from '@observablehq/plot';
import { PlotFigure, Tip } from '../../../charts/PlotFigure.tsx';
import { TableView } from '../../../components/TableView.tsx';
import { Checkbox, Segmented } from '../../../components/ui.tsx';
import { rowsToCsv } from '../../../lib/csv.ts';
import { GROUP_COLOR, UNATTRIBUTED_HATCH_ID } from '../../../theme/palette.ts';
import type { SectorPolicyChunk, StageSlug } from '../../contract.ts';
import { STAGE_LABELS, TECH_LABELS } from '../../contract.ts';
import type { GeoInfo, SectorModel } from '../../data/model.ts';
import { countsByCountry, countsByInstrument, countsByYear, INSTRUMENTS, policyRecords, policyTexts, safeUrl, scopePolicies, tagBit, type PolicyRecord } from '../../derive/policy.ts';
import { useSectorManifest } from '../../data/SectorDataProvider.tsx';
import { ChunkGate, ExternalLink, HowToRead, markGroup } from '../common.tsx';
import { usePolicy, usePolicyText } from '../data.ts';
import { LeagueBars, SectorChartCard, SmallMultiples, type LeagueRow } from '../kit.tsx';
import { useSectorUrl } from '../state.tsx';
import { useProfile } from './ctx.tsx';

const FROM = 2000;
const TO = 2030;

/** The first "announced or planned" year: the dictionary's projection_from, else the year after the export. */
export function announcedFrom(model: SectorModel, created: string | undefined): number {
  const v = model.varById.get('policy.records');
  if (v?.projection_from) return v.projection_from;
  const y = created ? new Date(created).getUTCFullYear() : NaN;
  return Number.isFinite(y) ? y + 1 : 2026;
}

export function PolicyList({ records, model, title }: { records: PolicyRecord[]; model: SectorModel; title: string }) {
  const [open, setOpen] = useState(false);
  const text = usePolicyText(open);
  const texts = useMemo(() => policyTexts(text.data), [text.data]);
  const rows = useMemo(() => [...records].sort((a, z) => (z.year ?? 0) - (a.year ?? 0)), [records]);
  const csv = () => ({
    name: 'policy-list',
    text: rowsToCsv(rows.map((r) => ({ id: r.id, iso3c: model.geos[r.geo]?.code, jurisdiction: model.geos[r.geo]?.name, level: r.jurisdiction, title: texts.get(r.id)?.title ?? null, url: safeUrl(texts.get(r.id)?.url), year: r.year, status: r.status, type: r.type, tech_tags: r.tags.join('|'), stage_tags: r.stages.join('|'), tag_source: r.tech_src, cross_cutting: r.cross_cutting, source: 'IEA Policies and Measures', licence: 'attribution' }))),
  });
  return (
    <details className="disclosure border-t hairline" onToggle={(e) => setOpen((e.currentTarget as HTMLDetailsElement).open || open)}>
      <summary className="flex items-center gap-2 py-2.5 text-sm font-medium text-paper-dim hover:text-paper">
        <span className="disclosure-chevron text-paper-mute" aria-hidden="true">
          ›
        </span>
        {title} ({rows.length})
      </summary>
      {open && (
        <ChunkGate h={text} name="sector-policy-text" minHeight={60}>
          {() => (
            <SectorChartCard title="Policy list" vars={[{ var: 'policy.records' }]} subExtra={<HowToRead>Each row is one policy in the IEA Policies and Measures database, newest first; the title links to the IEA page. “Tag source” says how it was matched to this technology: the IEA’s own tag, a keyword rule on the text (“text-derived”) or an economy-wide measure (“cross-cutting”).</HowToRead>} csv={csv} summary={`${rows.length} recorded policies.`}>
              <TableView
                rows={rows}
                caption="Policy list"
                search
                pageSize={15}
                cols={[
                  {
                    id: 't',
                    header: 'Title',
                    value: (r) => texts.get(r.id)?.title ?? `Record ${r.id}`,
                    cell: (r) => {
                      const t = texts.get(r.id);
                      const u = safeUrl(t?.url);
                      return u ? (
                        <ExternalLink href={u} srText="opens IEA page">
                          {t?.title ?? `Record ${r.id}`}
                        </ExternalLink>
                      ) : (
                        (t?.title ?? `Record ${r.id}`)
                      );
                    },
                    minWidth: 240,
                  },
                  { id: 'j', header: 'Jurisdiction', value: (r) => model.geos[r.geo]?.name ?? '' },
                  { id: 'y', header: 'Year', value: (r) => r.year, num: true },
                  { id: 's', header: 'Status', value: (r) => r.status },
                  { id: 'k', header: 'Type', value: (r) => r.type },
                  { id: 'g', header: 'Stage tags', value: (r) => r.stages.map((s) => STAGE_LABELS[s]).join(', ') || null },
                  { id: 'src', header: 'Tag source', value: (r) => (r.cross_cutting ? 'cross-cutting' : r.tech_src === 'text' ? 'text-derived' : 'IEA tag') },
                ]}
              />
            </SectorChartCard>
          )}
        </ChunkGate>
      )}
    </details>
  );
}

export function ByYear({ records, from }: { records: PolicyRecord[]; from: number }) {
  const data = useMemo(() => countsByYear(records, FROM, TO, from), [records, from]);
  const build = useCallback(
    (width: number) => ({
      width,
      x: { label: null, tickFormat: (d: number) => String(d), ticks: 8, type: 'band' as const },
      y: { label: 'New policies', grid: true },
      marks: [
        Plot.barY(data.filter((d) => !d.announced), { x: 'year', y: 'n', fill: GROUP_COLOR.core, insetLeft: 1, insetRight: 1 }),
        Plot.barY(data.filter((d) => d.announced), { x: 'year', y: 'n', fill: `url(#${UNATTRIBUTED_HATCH_ID})`, stroke: GROUP_COLOR.core, insetLeft: 1, insetRight: 1 }),
        Plot.ruleY([0]),
        Plot.barY(data, Plot.pointerX({ x: 'year', y: 'n', fill: 'none', stroke: '#f58229' })),
      ],
      xAxis: undefined,
    }),
    [data],
  );
  return (
    <SectorChartCard
      title={`New policies by year, ${FROM}-${TO}`}
      vars={[{ var: 'policy.records', window: [FROM, TO] }]}
      subExtra={<HowToRead>Each bar is one year: how many policies in scope were introduced that year. Taller bars mean more new policies, not stronger ones; hatched bars are years still to come, policies announced or planned.</HowToRead>}
      table={() => <TableView rows={data} caption="New policies by year" cols={[{ id: 'y', header: 'Year', value: (d) => d.year }, { id: 'n', header: 'Records', value: (d) => d.n, num: true }, { id: 'a', header: 'Announced or planned', value: (d) => (d.announced ? 'yes' : 'no') }]} />}
      csv={() => ({ name: 'policies-by-year', text: rowsToCsv(data.map((d) => ({ year: d.year, records: d.n, announced_or_planned: d.announced, source: 'IEA Policies and Measures' }))) })}
      notes={<p>Years from {from} are hatched and labelled "announced or planned".</p>}
      summary={`${data.reduce((a, d) => a + d.n, 0)} records from ${FROM} to ${TO}.`}
    >
      <PlotFigure<(typeof data)[number]> build={build} height={220} ariaLabel="New policies by year" tooltip={(d) => <Tip value={`${d.n} records`} title={String(d.year)} lines={[d.announced ? 'announced or planned' : null]} />} />
    </SectorChartCard>
  );
}

export function ByInstrument({ records }: { records: PolicyRecord[] }) {
  const data = useMemo(() => countsByInstrument(records, FROM, TO), [records]);
  return (
    <SectorChartCard
      title="What kind of policy"
      vars={[{ var: 'policy.records', window: [FROM, TO] }]}
      subExtra={<HowToRead>One panel per kind of instrument (financial incentives, regulations and standards, research and development, other): each bar is the number of new policies of that type in one year. The panels share one vertical scale, so taller bars mean that kind of instrument is used more often.</HowToRead>}
      table={() => <TableView rows={data[INSTRUMENTS[0]].map((d) => d.year)} caption="Records by instrument and year" cols={[{ id: 'y', header: 'Year', value: (y) => y }, ...INSTRUMENTS.map((k) => ({ id: k, header: k, value: (y: number) => data[k].find((d) => d.year === y)?.n ?? 0, num: true }))]} />}
      csv={() => ({ name: 'policies-by-instrument', text: rowsToCsv(INSTRUMENTS.flatMap((k) => data[k].map((d) => ({ instrument: k, year: d.year, records: d.n })))) })}
      notes={<p>Types are not groups, so the panels share one hue and one y scale.</p>}
      summary={INSTRUMENTS.map((k) => `${k}: ${data[k].reduce((a, d) => a + d.n, 0)}`).join('; ')}
    >
      <SmallMultiples
        panels={INSTRUMENTS.map((k) => ({ id: k, title: k, rows: data[k], empty: data[k].some((d) => d.n > 0) ? undefined : 'no records' }))}
        shared
        yValue={(d) => d.n}
        height={120}
        ariaLabel={(p) => `${p.title}: records by year`}
        tooltip={(d) => `${d.year}: ${d.n}`}
        render={(panel, ctx) => ({
          x: { label: null, ticks: 3, tickFormat: (d: number) => String(d) },
          y: { label: null, ticks: 3, grid: true, domain: ctx.yDomain ?? undefined },
          marks: [Plot.rectY(panel.rows, { x1: (d: { year: number }) => d.year - 0.4, x2: (d: { year: number }) => d.year + 0.4, y: 'n', fill: GROUP_COLOR.core })],
        })}
      />
    </SectorChartCard>
  );
}

export function PolicyBody({ chunk, scopeGeo }: { chunk: SectorPolicyChunk; scopeGeo?: GeoInfo | null }) {
  const { model, cell, tech, stage, highlight } = useProfile();
  const { state, set } = useSectorUrl();
  const [stageScope, setStageScope] = useState<'stage' | 'any'>('stage');
  const recs = useMemo(() => policyRecords(chunk), [chunk]);
  const tags = chunk['policy.tags'];
  const bit = tagBit(tags, TECH_LABELS[tech]);
  const textDerived = recs.some((r) => r.tech_src === 'text' && bit >= 0 && ((r.tech_mask >>> bit) & 1) === 1);
  const scoped = useMemo(() => scopePolicies(recs, { bit, stage: stageScope === 'stage' ? (stage as StageSlug) : null, cross: state.xc, geo: scopeGeo?.idx }), [recs, bit, stageScope, stage, state.xc, scopeGeo]);
  const nStage = useMemo(() => scopePolicies(recs, { bit, stage, cross: state.xc, geo: scopeGeo?.idx }).length, [recs, bit, stage, state.xc, scopeGeo]);
  const nAny = useMemo(() => scopePolicies(recs, { bit, stage: null, cross: state.xc, geo: scopeGeo?.idx }).length, [recs, bit, state.xc, scopeGeo]);
  const manifest = useSectorManifest();
  const from = announcedFrom(model, manifest.data?.created);
  const byC = useMemo(() => countsByCountry(scoped), [scoped]);
  const league: LeagueRow[] = byC.slice(0, 25).map((c) => {
    const g = model.geos[c.geo];
    return { id: g.code, label: g.name, group: markGroup(g), value: c.n, display: `${c.n} · in force ${c.inForce} · ended ${c.ended}`, badge: g.kind === 'eu' ? "EU's own measures" : undefined };
  });
  if (highlight && !league.some((r) => r.id === highlight.code)) {
    const c = byC.find((x) => x.geo === highlight.idx);
    league.push({ id: highlight.code, label: highlight.name, group: markGroup(highlight), value: c?.n ?? 0, display: c ? `${c.n} · in force ${c.inForce} · ended ${c.ended}` : 'no records', pinned: true });
  }
  if (bit < 0 && !state.xc) {
    return <p className="text-sm text-paper-mute">IEA Policies and Measures has no technology tag for {TECH_LABELS[tech]}. Include economy-wide measures to see the cross-cutting records.</p>;
  }
  return (
    <div className="space-y-6">
      <div className="flex flex-wrap items-end gap-x-6 gap-y-3 border-b hairline pb-4">
        <div className="text-sm text-paper-dim">
          Technology: <span className="text-paper">{TECH_LABELS[tech]}</span> ({bit >= 0 ? (textDerived ? 'IEA tag, some text-derived' : "IEA's tag") : 'no tag'})
        </div>
        <Checkbox label="Include economy-wide (cross-cutting) measures" checked={state.xc} onChange={(b) => set({ xc: b })} />
        <Segmented
          label="Stage"
          value={stageScope}
          options={[
            { value: 'stage', label: `Tagged to ${STAGE_LABELS[stage]} (${nStage})` },
            { value: 'any', label: `Any stage (${nAny})` },
          ]}
          onChange={(x) => setStageScope(x)}
        />
      </div>
      {!scopeGeo && (
        <SectorChartCard
          title="Policies by country"
          vars={[{ var: 'policy.records' }]}
          subExtra={<HowToRead>Each bar is one economy: how many policies the IEA has recorded for it in scope, longest first; the label splits them into still in force and ended. A count measures how active policy is, not how strong or well funded it is. Click a bar to highlight the economy.</HowToRead>}
          table={() => <TableView rows={byC} caption="Policies by country" search cols={[{ id: 'c', header: 'Jurisdiction', value: (c) => model.geos[c.geo]?.name ?? '' }, { id: 'n', header: 'Records', value: (c) => c.n, num: true }, { id: 'f', header: 'In force', value: (c) => c.inForce, num: true }, { id: 'e', header: 'Ended', value: (c) => c.ended, num: true }]} />}
          csv={() => ({ name: `${cell.slug}-policies-by-country`, text: rowsToCsv(byC.map((c) => ({ iso3c: model.geos[c.geo]?.code, jurisdiction: model.geos[c.geo]?.name, var_id: 'policy.records', item: cell.slug, records: c.n, in_force: c.inForce, ended: c.ended, source: 'IEA Policies and Measures', licence: 'attribution' }))) })}
          summary={byC[0] ? `${model.geos[byC[0].geo]?.name} has the most recorded policies in scope: ${byC[0].n}.` : 'No records in scope.'}
        >
          {league.length ? <LeagueBars rows={league} axisLabel="Recorded policies (count)" selected={highlight?.code ?? null} onSelect={(r) => set({ c: r.id === state.c ? null : r.id })} top={25} ariaLabel="Recorded policies by jurisdiction" /> : <p className="text-sm text-paper-mute">No records in scope.</p>}
        </SectorChartCard>
      )}
      <ByYear records={scoped} from={from} />
      <ByInstrument records={scoped} />
      <PolicyList records={scoped} model={model} title="Policy list" />
      <div className="space-y-1 text-xs text-paper-mute">
        <p>Counts measure policy activity recorded in the IEA Policies and Measures database, not stringency or spending. Coverage is uneven.</p>
        <p>The EU's own measures appear as "European Union".</p>
      </div>
    </div>
  );
}

export function PolicySection() {
  const h = usePolicy();
  return <ChunkGate h={h} name="sector-policy">{(chunk) => <PolicyBody chunk={chunk} />}</ChunkGate>;
}
