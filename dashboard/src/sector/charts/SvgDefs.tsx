// The Sector hatch, once per page (spec_sector.md 3.4, 5.3): 45 deg #9ca3af lines on ink-700, used by
// SVG and Plot marks as url(#sector-hatch) for "not attributed" and no-data. SectorLayout mounts it.

import { HATCH_GROUND, HATCH_LINE, UNATTRIBUTED_HATCH_ID } from '../../theme/palette.ts';

export function SvgDefs() {
  return (
    <svg width="0" height="0" aria-hidden="true" focusable="false" style={{ position: 'absolute', width: 0, height: 0, overflow: 'hidden' }} data-testid="sector-svg-defs">
      <defs>
        <pattern id={UNATTRIBUTED_HATCH_ID} width="5" height="5" patternUnits="userSpaceOnUse" patternTransform="rotate(45)">
          <rect width="5" height="5" fill={HATCH_GROUND} />
          <line x1="0" y1="0" x2="0" y2="5" stroke={HATCH_LINE} strokeWidth="1" />
        </pattern>
      </defs>
    </svg>
  );
}
