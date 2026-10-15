// Opportunity-security space derivations (pure; shiny mod_space.R region_of(); since 2026-10-15 the
// user's orientation: x = Economic Opportunity, y = Energy Security, split at the medians of the
// economies shown). The four corners carry the names Country Insights uses (the user's decision, 2026-10-11:
// Upgrade, Grow, Secure, Watch; RMI 2025's leapfrog, export promotion, friendshore and alternative
// value creation) beside their plain description.

export const REGIONS = [
  'Upgrade: high opportunity, low energy security',
  'Grow: high opportunity, high energy security',
  'Secure: low opportunity, low energy security',
  'Watch: low opportunity, high energy security',
] as const;

/** The corner names alone, in REGIONS order. */
export const CORNERS = ['Upgrade', 'Grow', 'Secure', 'Watch'] as const;
/** The glossary id of each corner, in REGIONS order. */
export const CORNER_TERM = ['upgrade', 'grow', 'secure', 'watch'] as const;

/** shiny mod_space.R region_of(). */
export function regionOf(es: number | null, eo: number | null, esMed: number, eoMed: number): string | null {
  if (es === null || eo === null) return null;
  if (eo >= eoMed && es < esMed) return REGIONS[0];
  if (eo >= eoMed && es >= esMed) return REGIONS[1];
  if (eo < eoMed && es < esMed) return REGIONS[2];
  return REGIONS[3];
}

export function median(xs: number[]): number {
  if (!xs.length) return NaN;
  const s = [...xs].sort((a, b) => a - b);
  const m = s.length >> 1;
  return s.length % 2 ? s[m] : (s[m - 1] + s[m]) / 2;
}

/** A padded domain fitted to the values, on a 0.05 grid and inside [0, 1]. */
export function fitDomain(xs: (number | null)[], pad = 0.04): [number, number] {
  const v = xs.filter((x): x is number => x !== null && Number.isFinite(x));
  if (!v.length) return [0, 1];
  let lo = Math.min(...v) - pad;
  let hi = Math.max(...v) + pad;
  lo = Math.max(0, Math.floor(lo / 0.05) * 0.05);
  hi = Math.min(1, Math.ceil(hi / 0.05) * 0.05);
  if (hi - lo < 0.2) {
    const mid = (lo + hi) / 2;
    lo = Math.max(0, mid - 0.1);
    hi = Math.min(1, lo + 0.2);
  }
  return [lo, hi];
}
