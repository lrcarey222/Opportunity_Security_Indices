// The Partners view with one sector chosen: the selected economy's partners in that technology and
// stage, as friendshoring opportunities (Friendshore) and as export markets (Partnership Opportunity), side by side, from
// the engine's per-sector scores under the scenario (engine.cellPartners()), with Development Potential
// in that sector and the Partnership Strength rank (all sectors) as context.

import { useCallback, useMemo } from 'react';
import * as Plot from '@observablehq/plot';
import { ChartCard } from '../../charts/ChartCard.tsx';
import { Legend } from '../../charts/legends.tsx';
import { PlotFigure, Tip } from '../../charts/PlotFigure.tsx';
import { labelMargin, symDomain } from '../../charts/theme.ts';
import { TableView, type Col } from '../../components/TableView.tsx';
import { EmptyState, Loading } from '../../components/ui.tsx';
import { DASH, fmtNum, fmtSigned, tierLabel } from '../../lib/format.ts';
import type { ShowMode } from '../../state/url.ts';
import { COMPARE_COLOR, SIGNAL, TIER_COLOR } from '../../theme/palette.ts';
import { showWord } from '../common.tsx';

export interface SectorRow {
  partner_iso: string;
  Country: string;
  tier: 'core' | 'partner' | 'other';
  bloc: string | null;
  fs: number | null;
  fs_base: number | null;
  fs_rank: number | null;
  fs_rank_base: number | null;
  po: number | null;
  po_base: number | null;
  po_rank: number | null;
  po_rank_base: number | null;
  /** Development Potential in this sector (NEIS baseline; null: high-income or not scored) */
  dp: number | null;
  /** Partnership Strength (all sectors) under the scenario, and its rank */
  psi: number | null;
  psi_rank: number | null;
}

export type Side = 'fs' | 'po';
export const SIDE_NAME: Record<Side, string> = { fs: 'Friendshore', po: 'Partnership Opportunity' };

/** The value a chart shows for a side under the show mode (scenario, baseline, or the change). */
export function sideValue(r: SectorRow, side: Side, show: ShowMode): number | null {
  const v = r[side];
  const b = r[`${side}_base`];
  if (show === 'baseline') return b;
  if (show === 'change') return v === null || b === null ? null : v - b;
  return v;
}

/** The top n partners of a side: by the rank under the scenario (baseline rank in Baseline mode). */
export function topOf(rows: SectorRow[], side: Side, show: ShowMode, n = 15): (SectorRow & { shown: number | null })[] {
  const rk = (r: SectorRow) => (show === 'baseline' ? r[`${side}_rank_base`] : r[`${side}_rank`]);
  return rows
    .filter((r) => rk(r) !== null)
    .sort((a, b) => (rk(a) as number) - (rk(b) as number) || (a.Country < b.Country ? -1 : 1))
    .slice(0, n)
    .map((r) => ({ ...r, shown: sideValue(r, side, show) }));
}

export function SectorBars({ rows, side, sector, rName, show, isBaseline, updating, onSelect }: {
  rows: SectorRow[] | null; side: Side; sector: string; rName: string; show: ShowMode; isBaseline: boolean; updating: boolean; onSelect: (iso: string) => void;
}) {
  const top = useMemo(() => (rows ? topOf(rows, side, show) : []), [rows, side, show]);
  const change = show === 'change';
  const build = useCallback(
    (width: number): Plot.PlotOptions | null => {
      if (!top.length) return null;
      const baseKey = `${side}_base`;
      return {
        width,
        height: 40 + top.length * 22,
        marginLeft: labelMargin(top.map((r) => r.Country), { min: 90, max: 170 }),
        marginRight: 56,
        x: { label: change ? `Change in ${SIDE_NAME[side]} against the NEIS baseline →` : `${SIDE_NAME[side]} in this sector (0 to 1) →`, grid: true, nice: true, domain: change ? symDomain(top.map((r) => r.shown), 0.005).map((v) => v * 1.5) : [0, 1] },
        y: { domain: top.map((r) => r.Country), label: null, tickSize: 0 },
        marks: [
          Plot.barX(top, { y: 'Country', x: 'shown', fill: (r: SectorRow) => TIER_COLOR[r.tier], insetTop: 4, insetBottom: 4 }),
          !change && !isBaseline ? Plot.tickX(top, { y: 'Country', x: baseKey, stroke: COMPARE_COLOR.baseline, strokeWidth: 2 }) : null,
          Plot.text(top.filter((r) => !change || (r.shown ?? 0) >= 0), { y: 'Country', x: 'shown', text: (r: { shown: number | null }) => (change ? fmtSigned(r.shown, 3) : fmtNum(r.shown, 3)), dx: 6, textAnchor: 'start', fill: '#d1d5db' }),
          change ? Plot.text(top.filter((r) => (r.shown ?? 0) < 0), { y: 'Country', x: 'shown', text: (r: { shown: number | null }) => fmtSigned(r.shown, 3), dx: -6, textAnchor: 'end', fill: '#d1d5db' }) : null,
          Plot.ruleX([0], { stroke: 'rgb(255 255 255 / 0.22)' }),
          Plot.barX(top, Plot.pointerY({ y: 'Country', x: 'shown', fill: 'none', stroke: SIGNAL, strokeWidth: 2, insetTop: 3, insetBottom: 3 })),
        ],
      };
    },
    [top, side, change, isBaseline],
  );
  const tick = !isBaseline && !change ? '; the gray tick is the NEIS baseline value' : '';
  const what =
    side === 'fs'
      ? `The 15 strongest friendshoring opportunities for ${rName} in ${sector} alone, by Friendshore in this sector (0 to 1)${tick}: partners where ${rName}'s security need meets the partner's economic opportunity, on existing trade and investment ties. ${rName}'s own need is the same for every partner here; the ranking turns on the partner's opportunity, the trade and investment ties, UN-vote alignment and ownership.`
      : `The 15 strongest export markets for ${rName} in ${sector} alone, by Partnership Opportunity in this sector (0 to 1)${tick}.`;
  return (
    <ChartCard
      title={side === 'fs' ? `Friendshoring opportunities in ${sector}` : `Best export markets in ${sector}`}
      sub={`${showWord(show)} ${what} Colour shows the partner's place in the coalition. Click a bar to see the pair sector by sector.`}
      updating={updating}
      summary={`${rName}, ${sector}: the ${top.length} strongest partners by ${SIDE_NAME[side]}.`}
      legend={
        <Legend
          items={[
            { label: 'Coalition core', color: TIER_COLOR.core },
            { label: 'Coalition partner', color: TIER_COLOR.partner },
            { label: 'Other economy', color: TIER_COLOR.other },
            ...(!isBaseline && !change ? [{ label: 'NEIS baseline (tick)', kind: 'tick' as const, color: COMPARE_COLOR.baseline }] : []),
          ]}
        />
      }
      table={() => <TableView rows={top} cols={sectorCols(sector).filter((c) => ['Country', 'tier', side, `${side}_rank`, 'dp', 'psi_rank'].includes(c.id))} caption={`${sector}: ${what}`} pageSize={15} />}
    >
      {!rows ? (
        <Loading label={`Scoring ${sector}`} />
      ) : top.length ? (
        <PlotFigure<SectorRow>
          build={build}
          height={40 + top.length * 22}
          ariaLabel={`${SIDE_NAME[side]} in ${sector}`}
          tooltip={(r) => sectorTip(r, sector, isBaseline)}
          onSelect={(r) => onSelect(r.partner_iso)}
        />
      ) : (
        <EmptyState>No {SIDE_NAME[side]} scores for {rName} in {sector}.</EmptyState>
      )}
    </ChartCard>
  );
}

export function sectorTip(r: SectorRow, sector: string, isBaseline: boolean) {
  return (
    <Tip
      value={`${sector}`}
      title={`${r.Country} (${tierLabel(r.tier)})`}
      lines={[
        `Friendshore ${fmtNum(r.fs, 3)} (rank ${r.fs_rank ?? DASH}) · Partnership Opportunity ${fmtNum(r.po, 3)} (rank ${r.po_rank ?? DASH})`,
        !isBaseline ? `NEIS baseline: Friendshore ${fmtNum(r.fs_base, 3)} (rank ${r.fs_rank_base ?? DASH}), Partnership Opportunity ${fmtNum(r.po_base, 3)} (rank ${r.po_rank_base ?? DASH})` : null,
        `Development Potential in this sector ${r.dp === null ? 'not scored' : fmtNum(r.dp, 3)} · Partnership Strength rank (all sectors) ${r.psi_rank ?? DASH}`,
      ]}
    />
  );
}

export function sectorCols(sector: string): Col<SectorRow>[] {
  return [
    { id: 'Country', header: 'Partner', value: (r) => r.Country, minWidth: 150 },
    { id: 'tier', header: 'Tier', value: (r) => tierLabel(r.tier) },
    { id: 'fs', header: `Friendshore, ${sector}`, title: 'How strong a friendshoring opportunity the partner is in this sector (0 to 1)', value: (r) => r.fs, cell: (r) => fmtNum(r.fs, 3), num: true },
    { id: 'fs_rank', header: 'Friendshore rank', value: (r) => r.fs_rank, num: true },
    { id: 'po', header: `Partnership Opportunity, ${sector}`, title: 'How good a market the partner is for exports in this sector (0 to 1)', value: (r) => r.po, cell: (r) => fmtNum(r.po, 3), num: true },
    { id: 'po_rank', header: 'Market rank', value: (r) => r.po_rank, num: true },
    { id: 'dp', header: `Development Potential, ${sector}`, title: 'NEIS baseline; scored for middle- and lower-income partners only', value: (r) => r.dp, cell: (r) => (r.dp === null ? 'not scored' : fmtNum(r.dp, 3)), num: true, minWidth: 120 },
    { id: 'psi_rank', header: 'Partnership Strength rank (all sectors)', value: (r) => r.psi_rank, num: true },
    { id: 'fs_base', header: 'Friendshore, baseline', value: (r) => r.fs_base, cell: (r) => fmtNum(r.fs_base, 3), num: true },
    { id: 'po_base', header: 'Partnership Opportunity, baseline', value: (r) => r.po_base, cell: (r) => fmtNum(r.po_base, 3), num: true },
  ];
}
