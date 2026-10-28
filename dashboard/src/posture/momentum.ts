// Trade momentum direction from the log export growth, as the exporter computes it (scripts/91:
// posture_momentum_arrow): up above +band, down below -band, flat between; none without a value.

import type { Momentum } from './contract.ts';

export function postureMomentum(raw: number | null | undefined, band = 0.1): Momentum | null {
  if (raw === null || raw === undefined || !Number.isFinite(raw)) return null;
  return raw > band ? 'up' : raw < -band ? 'down' : 'flat';
}
