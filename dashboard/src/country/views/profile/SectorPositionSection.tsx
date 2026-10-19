// "Sector position" of the Country profile (IA refactor 2026-10-16, brief s.12B; plan.md 5.3 item 3): one
// compact row per sector from the fact sheet: Sector (links to the Sector profile) | Opportunity and
// Security (the share of economies the country beats, coloured on the dashboard's three-colour scale
// around the typical economy, 50%) | Policy (the policy-activity third) | Climate (the Strategic Index's
// low-emission technology term: one value per technology, the same for every economy) | Strategic
// priority (the Strategic Index rank, under the reader's Perspective). Only the last column is
// preference-adjusted. "Explore details" opens the embedded Country scores view (the model). Under the table
// (2026-10-19), core members get the monthly trade of the selected sector by sub-component (TradeMonthlyCard).

import { useMemo } from 'react';
import { Link } from 'react-router';
import { triScale } from '../../../charts/scale.ts';
import { ChartCard } from '../../../charts/ChartCard.tsx';
import { TableView, type Col } from '../../../components/TableView.tsx';
import { RankChip } from '../../../components/ui.tsx';
import CountryView from '../../../views/Country/CountryView.tsx';
import type { CellFacts } from '../../contract.ts';
import { siOrder, siRankOf } from '../../derive/framing.ts';
import { sectorProfileLink } from '../../derive/links.ts';
import { POLICY_THIRD_LABEL } from '../../derive/model.ts';
import { cap, proseName, quadrantWord } from '../../derive/text.ts';
import { BasisTag, csvWithSource, Fold, perspectiveNote, Section, usePerspectiveSi, useProfile } from './common.tsx';
import { TradeMonthlyCard } from './TradeMonthlyCard.tsx';

/** Sectors in the order of the Strategic priority (the Perspective's), then the rest by Economic Opportunity. */
export function sectorPositionRows(cells: readonly CellFacts[], ordered: readonly CellFacts[]): CellFacts[] {
  const seen = new Set(ordered.map((c) => c.key));
  const rest = cells.filter((c) => !seen.has(c.key)).sort((a, b) => (b.eo?.pct.v ?? -1) - (a.eo?.pct.v ?? -1));
  return [...ordered, ...rest];
}

function Pct({ v, d, color }: { v: number | null | undefined; d: string | undefined; color: string | null }) {
  if (v === null || v === undefined) return <span className="text-paper-mute">no score</span>;
  return (
    <span className="inline-flex items-center gap-1.5">
      <span className="inline-block h-2.5 w-2.5 rounded-sm" style={{ background: color ?? 'transparent' }} aria-hidden="true" />
      {d}
    </span>
  );
}

export function SectorPositionSection() {
  const { sheet, cell, setCell } = useProfile();
  const si = usePerspectiveSi();
  const name = proseName(sheet.meta.country);
  const iso = sheet.iso3;
  const rows = useMemo(() => sectorPositionRows(sheet.cells, siOrder(sheet, si.map)), [sheet, si.map]);
  const pctScale = useMemo(() => triScale([0, 50, 100], { domain: [0, 50, 100] }), []);
  const cols: Col<CellFacts>[] = [
    { id: 'sector', header: 'Sector', value: (c) => c.label, cell: (c) => <Link className="link text-paper" to={sectorProfileLink(c, iso)}>{c.label}</Link>, minWidth: 180 },
    { id: 'eo', header: 'Opportunity (beats)', value: (c) => c.eo?.pct.v ?? null, cell: (c) => <Pct v={c.eo?.pct.v} d={c.eo?.pct.d} color={pctScale.color(c.eo?.pct.v)} />, num: true },
    { id: 'es', header: 'Security (beats)', value: (c) => c.es?.pct.v ?? null, cell: (c) => <Pct v={c.es?.pct.v} d={c.es?.pct.d} color={pctScale.color(c.es?.pct.v)} />, num: true },
    { id: 'corner', header: 'Corner', value: (c) => (c.quadrant ? quadrantWord(sheet, c.quadrant) : 'not placed') },
    { id: 'policy', header: 'Policy activity', value: (c) => (c.policy?.third ? POLICY_THIRD_LABEL[c.policy.third] : null) },
    {
      id: 'climate',
      header: 'Climate: low-emission term',
      title: "The Strategic Index's low-emission technology term: one value per technology, the same for every economy (higher = cleaner technology).",
      value: (c) => c.si.parts?.tech.v ?? null,
      cell: (c) => c.si.parts?.tech.d ?? '–',
      num: true,
    },
    {
      id: 'priority',
      header: 'Strategic priority',
      value: (c) => siRankOf(c, si.map),
      cell: (c) => {
        const r = siRankOf(c, si.map);
        const o = si.map?.get(c.key);
        return r === null ? (
          <span className="text-paper-mute">not ranked</span>
        ) : (
          <span className="inline-flex items-center gap-1.5">
            {r}
            {o && <RankChip change={o.delta} isNew={o.isNew} />}
          </span>
        );
      },
      num: true,
    },
  ];
  const csv = () => ({
    name: `${sheet.meta.country} sector position`,
    text: csvWithSource(
      rows.map((c) => ({
        sector: c.tech, stage: c.stage, economic_opportunity_beats_pct: c.eo?.pct.v ?? null, energy_security_beats_pct: c.es?.pct.v ?? null,
        corner: c.quadrant ? quadrantWord(sheet, c.quadrant) : null, policy_activity_third: c.policy?.third ?? null, low_emission_term: c.si.parts?.tech.v ?? null,
        strategic_priority_rank: siRankOf(c, si.map), perspective: perspectiveNote(si),
      })),
      'Opportunity Security Indices',
      sheet.meta.as_of,
    ),
  });
  return (
    <Section
      id="sectors"
      title="Sector position"
      basis={
        <>
          <BasisTag kind="facts" note="scores, corners, policy, climate" />
          <BasisTag kind="prefs" note={`Strategic priority, ${perspectiveNote(si)}`} />
        </>
      }
      intro={
        <>
          Every sector of {name} in one table. Opportunity and Security give the share of other economies {name} beats in that sector (orange below the typical economy, blue above). Strategic
          priority is the rank on the Strategic Index and is the only column the perspective dials change. Click a sector name for its Sector profile; click a row to select it on the chart above.
        </>
      }
    >
      <ChartCard
        headingLevel={3}
        title={`${cap(name)}: sector by sector`}
        sub={`Sorted by Strategic priority (${perspectiveNote(si)}), then by Economic Opportunity for sectors outside the index. Climate shows the Strategic Index's low-emission technology term (the same for every economy). The economy's own emissions by sector, from Climate TRACE, are in Policy & climate below.`}
        summary={`${rows.length} sectors of ${name}.`}
        csv={csv}
      >
        <TableView rows={rows} cols={cols} caption={`${cap(name)}: sector position`} rowKey={(c) => c.key} onRowClick={(c) => setCell(c.key)} selectedKey={cell} pageSize={12} />
      </ChartCard>
      <TradeMonthlyCard />
      <Fold id="scores" engine summary="Explore details: the scores' categories, contributions and every measure (Country scores, the model)">
        <CountryView embedded />
      </Fold>
    </Section>
  );
}
