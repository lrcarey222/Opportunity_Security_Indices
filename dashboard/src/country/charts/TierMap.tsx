// The landing's world map (spec_country.md A4.1): every economy of the roster coloured by its coalition
// tier (categorical: core, partner, other; no data hatched), with the words in the legend and the
// tooltip; a click opens the profile. Lazy: the atlas loads after the landing's first paint.

import { useMemo } from 'react';
import { Legend } from '../../charts/legends.tsx';
import { WorldMap, type MapCell } from '../../charts/WorldMap.tsx';
import { TIER_COLOR } from '../../theme/palette.ts';
import { allianceTier } from '../../posture/trustedNetwork.ts';
import type { CountryManifest } from '../contract.ts';

const WORD = { core: 'Trusted network: core member', partner: 'Trusted network: partner', allied: 'Trusted network: treated as allied', none: 'Outside the trusted network' } as const;

export default function TierMap({ manifest, onSelect }: { manifest: CountryManifest; onSelect: (iso3: string) => void }) {
  const cells = useMemo(() => {
    const m = new Map<string, MapCell>();
    for (const c of manifest.countries) {
      if (!c.iso_n3) continue;
      const t = allianceTier(c.iso3, c.tier);
      const fill = c.status === 'no_data' ? 'hatch' : t === 'core' ? TIER_COLOR.core : t === 'none' ? TIER_COLOR.other : TIER_COLOR.partner;
      m.set(c.iso_n3, {
        iso3c: c.iso3,
        fill,
        tip: (
          <div>
            <div className="text-sm font-semibold text-paper">{c.name}</div>
            <div className="text-paper-dim">{WORD[t]}</div>
            {c.status === 'no_data' && <div className="text-paper-mute">No index data</div>}
          </div>
        ),
      });
    }
    return m;
  }, [manifest]);
  return (
    <>
      <WorldMap cells={cells} ariaLabel="World map of the economies with a Country profile, coloured by trusted-network membership" onSelect={onSelect} />
      <div className="mt-3">
        <Legend
          label="Trusted network"
          items={[
            { label: 'Core member', color: TIER_COLOR.core },
            { label: 'Partner or treated as allied', color: TIER_COLOR.partner },
            { label: 'Outside the trusted network', color: TIER_COLOR.other },
            { label: 'No index data', kind: 'hatch' },
          ]}
        />
      </div>
    </>
  );
}
