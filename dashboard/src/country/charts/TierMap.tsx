// The landing's world map (spec_country.md A4.1): every economy of the roster coloured by its coalition
// tier (categorical: core, partner, other; no data hatched), with the words in the legend and the
// tooltip; a click opens the profile. Lazy: the atlas loads after the landing's first paint.

import { useMemo } from 'react';
import { Legend } from '../../charts/legends.tsx';
import { WorldMap, type MapCell } from '../../charts/WorldMap.tsx';
import { TIER_COLOR } from '../../theme/palette.ts';
import type { CountryManifest } from '../contract.ts';

const WORD = { core: 'Core coalition member', partner: 'Coalition partner', none: 'Outside the coalition' } as const;

export default function TierMap({ manifest, onSelect }: { manifest: CountryManifest; onSelect: (iso3: string) => void }) {
  const cells = useMemo(() => {
    const m = new Map<string, MapCell>();
    for (const c of manifest.countries) {
      if (!c.iso_n3) continue;
      const fill = c.status === 'no_data' ? 'hatch' : c.tier === 'core' ? TIER_COLOR.core : c.tier === 'partner' ? TIER_COLOR.partner : TIER_COLOR.other;
      m.set(c.iso_n3, {
        iso3c: c.iso3,
        fill,
        tip: (
          <div>
            <div className="text-sm font-semibold text-paper">{c.name}</div>
            <div className="text-paper-dim">{WORD[c.tier]}</div>
            {c.status === 'no_data' && <div className="text-paper-mute">No index data</div>}
          </div>
        ),
      });
    }
    return m;
  }, [manifest]);
  return (
    <>
      <WorldMap cells={cells} ariaLabel="World map of the economies in Country Insights, coloured by coalition tier" onSelect={onSelect} />
      <div className="mt-3">
        <Legend
          label="Coalition tier"
          items={[
            { label: 'Core coalition member', color: TIER_COLOR.core },
            { label: 'Coalition partner', color: TIER_COLOR.partner },
            { label: 'Outside the coalition', color: TIER_COLOR.other },
            { label: 'No index data', kind: 'hatch' },
          ]}
        />
      </div>
    </>
  );
}
