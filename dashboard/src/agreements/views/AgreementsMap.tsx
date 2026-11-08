// The Agreements page's two maps over the shared WorldMap (keyed by ISO 3166 numeric, joined through the
// Country manifest's iso_n3): the global map (each economy coloured by the number of partners at depth 3 or
// more, in the scope and under the filters) and an economy's partner map (each partner coloured by its depth
// class). Observed zeros are the zero ink with the absence caveat in the tooltip; economies outside the
// roster are plain land. A click chooses the economy.

import { useMemo } from 'react';
import { Legend } from '../../charts/legends.tsx';
import { WorldMap, type MapCell } from '../../charts/WorldMap.tsx';
import type { AgrVocab, Depth } from '../contract.ts';
import { ABSENCE_CAVEAT, COUNT_BINS, countBin, DEPTH_FILL, partnerTipWords, plural, type Scope } from '../derive.ts';
import { DepthLegend } from './parts.tsx';

export interface RosterEntry {
  iso3: string;
  name: string;
  iso_n3?: string | null;
}

/**
 * The global map: partners at depth 3 or more per economy. `partners` (the economy's partners in the shown pairs)
 * keeps the absence caveat to the economies where nothing was found.
 */
export function CountMap({
  roster, counts, partners, scopeWords, onSelect, selected,
}: { roster: readonly RosterEntry[]; counts: Map<string, number>; partners?: Map<string, number>; scopeWords: string; onSelect: (iso3: string) => void; selected?: string | null }) {
  const cells = useMemo(() => {
    const m = new Map<string, MapCell>();
    for (const c of roster) {
      if (!c.iso_n3) continue;
      const n = counts.get(c.iso3) ?? 0;
      const np = partners?.get(c.iso3) ?? 0;
      const bin = countBin(n);
      m.set(c.iso_n3, {
        iso3c: c.iso3,
        fill: bin ? bin.fill : 'zero',
        tip: (
          <div>
            <div className="text-sm font-semibold text-paper">{c.name}</div>
            <div className="text-paper-dim">
              {n ? `${plural(n, 'partner')} at depth 3 or more (${scopeWords})` : `No partner at depth 3 or more (${scopeWords})${np ? `; ${plural(np, 'partner')} at a lower class` : ''}`}
            </div>
            {!n && !np && <div className="mt-1 text-paper-mute">{ABSENCE_CAVEAT}</div>}
          </div>
        ),
      });
    }
    return m;
  }, [roster, counts, partners, scopeWords]);
  return (
    <>
      <WorldMap cells={cells} ariaLabel={`World map: the number of partners at depth class 3 or more of each economy, ${scopeWords}`} onSelect={onSelect} selected={selected ?? null} />
      <div className="mt-3">
        <Legend label="Partners at depth 3 or more" items={[{ label: 'None found', color: DEPTH_FILL[0] }, ...COUNT_BINS.map((b) => ({ label: b.label, color: b.fill }))]} />
      </div>
    </>
  );
}

/**
 * An economy's partners by depth class (the focal economy outlined). `depth` has the partners with a matching
 * agreement; `inScope` their agreements in the scope, so a class-0 partner with agreements says that none counts
 * toward depth (the absence caveat only where nothing was found).
 */
export function PartnerMap({
  roster, focal, depth, inScope, scope, v, scopeWords, filtered, onSelect,
}: { roster: readonly RosterEntry[]; focal: string; depth: Map<string, number>; inScope: Map<string, number>; scope: Scope; v: AgrVocab; scopeWords: string; filtered?: boolean; onSelect: (iso3: string) => void }) {
  const cells = useMemo(() => {
    const m = new Map<string, MapCell>();
    for (const c of roster) {
      if (!c.iso_n3) continue;
      if (c.iso3 === focal) {
        m.set(c.iso_n3, { iso3c: c.iso3, fill: 'zero', outline: true, tip: <div className="text-sm font-semibold text-paper">{c.name}</div> });
        continue;
      }
      const d = (depth.get(c.iso3) ?? 0) as Depth;
      // class 0 of a listed partner: it has agreements, none of which counts toward depth (no absence caveat)
      const tip = partnerTipWords({ depth: depth.get(c.iso3) ?? null, nInScope: inScope.get(c.iso3) ?? 0, scope, scopeWords, filtered: !!filtered, v });
      m.set(c.iso_n3, {
        iso3c: c.iso3,
        fill: d > 0 ? DEPTH_FILL[d] : 'zero',
        tip: (
          <div>
            <div className="text-sm font-semibold text-paper">{c.name}</div>
            <div className="text-paper-dim">{tip.words}</div>
            {tip.caveat && <div className="mt-1 text-paper-mute">{ABSENCE_CAVEAT}</div>}
          </div>
        ),
      });
    }
    return m;
  }, [roster, focal, depth, inScope, scope, v, scopeWords, filtered]);
  return (
    <>
      <WorldMap cells={cells} ariaLabel={`World map: the depth class of each partner, ${scopeWords}`} onSelect={onSelect} />
      <div className="mt-3">
        <DepthLegend v={v} />
      </div>
    </>
  );
}
