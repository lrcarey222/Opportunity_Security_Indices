// What changed among a set of posture cells (a Sector node across economies, a country's cells, a compact's or a
// partnership's cells), for the "What changed" notes on the conclusion cards and the Home panel. Pure: reads the
// change bundle only. The window is the bundle's recent pairs (those it carries records for).

import { ATTRIBUTION_LABEL, detailWindow, type Attribution, type ChangeRecord, type ChangesBundle } from './contract.ts';

export interface CellRef {
  iso3: string;
  tech: string;
  stage: string;
}

export interface ChangeSummary {
  /** the window the summary covers (null: the bundle carries no records) */
  window: { from: string; to: string } | null;
  /** the latest recommendation change of each matching cell, newest vintage first */
  recommendations: ChangeRecord[];
  /** matching cells whose latest change moved something other than the recommendation */
  otherMoves: number;
  /** attribution of the recommendation changes, most frequent first */
  attribution: { attribution: Attribution; label: string; n: number }[];
  /** true when at least one recommendation change is attributable (new data, or the method alone) */
  attributable: boolean;
}

export const cellKeyOf = (c: CellRef): string => `${c.iso3}|${c.tech}|${c.stage}`;

/** The cells of some economies at some technology x stage nodes (a compact's or a partnership's scope). */
export function cellsOf(isos: Iterable<string>, nodes: Iterable<{ tech: string; stage: string }>): Set<string> {
  const out = new Set<string>();
  const ns = [...nodes];
  for (const iso3 of isos) for (const n of ns) out.add(cellKeyOf({ iso3, tech: n.tech, stage: n.stage }));
  return out;
}

export function summarizeChanges(b: ChangesBundle, match: (c: ChangeRecord) => boolean): ChangeSummary {
  const order = new Map(b.vintages.map((v, i) => [v.id, i]));
  const latest = new Map<string, ChangeRecord>();
  for (const c of b.changes) {
    if (!match(c)) continue;
    const k = cellKeyOf(c);
    const prev = latest.get(k);
    if (!prev || (order.get(c.cur) ?? 0) > (order.get(prev.cur) ?? 0)) latest.set(k, c);
  }
  const all = [...latest.values()];
  const recommendations = all
    .filter((c) => c.types.includes('recommendation'))
    .sort((a, z) => (order.get(z.cur) ?? 0) - (order.get(a.cur) ?? 0) || a.id.localeCompare(z.id));
  const counts = new Map<Attribution, number>();
  for (const c of recommendations) counts.set(c.attribution, (counts.get(c.attribution) ?? 0) + 1);
  const attribution = [...counts.entries()]
    .map(([attribution, n]) => ({ attribution, label: ATTRIBUTION_LABEL[attribution], n }))
    .sort((a, z) => z.n - a.n);
  return {
    window: detailWindow(b),
    recommendations,
    otherMoves: all.length - recommendations.length,
    attribution,
    attributable: recommendations.some((c) => c.attribution === 'real_world' || c.attribution === 'methodology' || c.attribution === 'both'),
  };
}
