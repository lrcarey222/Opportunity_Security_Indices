// "What changed" for a compact and for a partnership (change layer): the posture cells in their scope, resolved
// from AIB nodes (each node: its OSI technologies at one of the six stages), checked against the change bundle.
//   compact     its core members x its nodes
//   partnership the two economies x the nodes of the compacts both are core members of

import { useCallback, useMemo } from 'react';
import { ChangedCellsNote, nodeLabel } from '../../../changes/ChangedCellsNote.tsx';
import type { ChangeRecord } from '../../../changes/contract.ts';
import { cellKeyOf, cellsOf } from '../../../changes/summary.ts';
import { useAibCtx } from '../../AibLayout.tsx';
import type { AibStrategic, AibStrategicCompact } from '../../contract.ts';
import { compactNodeIds } from '../../derive/strategic.ts';

function useNodeCells(compacts: AibStrategicCompact[]): { tech: string; stage: string }[] {
  const { nodeOf } = useAibCtx();
  return useMemo(() => {
    const out = new Map<string, { tech: string; stage: string }>();
    for (const c of compacts)
      for (const id of compactNodeIds(c)) {
        const n = nodeOf(id);
        if (!n) continue;
        for (const tech of n.techs) out.set(`${tech}|${n.stage}`, { tech, stage: n.stage });
      }
    return [...out.values()];
  }, [compacts, nodeOf]);
}

export function CompactChangesNote({ c }: { c: AibStrategicCompact }) {
  const { name } = useAibCtx();
  const compacts = useMemo(() => [c], [c]);
  const nodes = useNodeCells(compacts);
  const keys = useMemo(() => cellsOf(c.core.map((e) => e.iso3), nodes), [c.core, nodes]);
  const match = useCallback((r: ChangeRecord) => keys.has(cellKeyOf(r)), [keys]);
  const label = useCallback((r: ChangeRecord) => `${name(r.iso3)}, ${nodeLabel(r)}`, [name]);
  if (!keys.size) return null;
  return <ChangedCellsNote match={match} label={label} testid="compact-changes" />;
}

export function PairChangesNote({ s, a, b }: { s: AibStrategic; a: string; b: string }) {
  const { name } = useAibCtx();
  const shared = useMemo(
    () => s.compacts.filter((c) => c.core.some((e) => e.iso3 === a) && c.core.some((e) => e.iso3 === b)),
    [s.compacts, a, b],
  );
  const nodes = useNodeCells(shared);
  const keys = useMemo(() => cellsOf([a, b], nodes), [a, b, nodes]);
  const match = useCallback((r: ChangeRecord) => keys.has(cellKeyOf(r)), [keys]);
  const label = useCallback((r: ChangeRecord) => `${name(r.iso3)}, ${nodeLabel(r)}`, [name]);
  if (!keys.size) return null;
  return <ChangedCellsNote match={match} label={label} testid="pair-changes" />;
}
