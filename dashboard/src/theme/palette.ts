// Fixed entity colours and scales (spec_react.md 8.7), from shiny/R/theme.R and validated on the dark
// surface #1f2937 with the dataviz validator (scratch_dash/pal): stage and tier pairs pass CVD and the
// normal-vision floor as adjacent pairs; Upstream and Downstream are never two colour series in one
// panel (charts facet by stage instead). Orange is chrome only (and the selected / hovered outline).

export const INK = {
  900: '#111827',
  850: '#171f2e',
  800: '#1f2937',
  700: '#374151',
  600: '#4b5563',
  500: '#6b7280',
} as const;

export const PAPER = '#ffffff';
export const PAPER_DIM = '#d1d5db';
export const PAPER_MUTE = '#9ca3af';
export const SIGNAL = '#f58229';
export const ALERT = '#e2574c';
export const GRID = 'rgb(255 255 255 / 0.08)';
export const HAIRLINE = 'rgb(255 255 255 / 0.22)';
export const ZERO_RULE = 'rgb(255 255 255 / 0.22)';

export const STAGES = ['Upstream', 'Midstream', 'Downstream'] as const;
export type StageName = (typeof STAGES)[number];

export const STAGE_COLOR: Record<string, string> = {
  Upstream: '#1889b0',
  Midstream: '#8b9d17',
  Downstream: '#2fa5a6',
};

export const PILLAR_COLOR = { ES: '#1889b0', EO: '#8b9d17', Policy: '#2fa5a6' } as const;

export const TIER_COLOR: Record<string, string> = { core: '#1889b0', partner: '#8b9d17', other: '#6b7280' };
export const tierKey = (t: unknown): 'core' | 'partner' | 'other' => (t === 'core' ? 'core' : t === 'partner' ? 'partner' : 'other');

export const COMPARE_COLOR = { scenario: '#1889b0', baseline: '#9ca3af', observed: '#8b9d17' } as const;

/** Sequential, low -> high (dim -> bright on the dark ground). */
export const SEQ = ['#016787', '#007da3', '#2792ba', '#43a8d0', '#5cbfe8', '#75d5fe', '#b4e7fe'];

/** Diverging: decrease arm (bright = large decrease) | neutral | increase arm (dim -> bright). */
export const DIV_NEG = ['#fe968a', '#f16559', '#ce443b', '#ab1f1c'];
export const DIV_MID = '#4b5563';
export const DIV_POS = ['#016787', '#1286ad', '#3fa5cd', '#62c5ee'];
/** Low (large decrease) -> high (large increase), for a d3 / Plot piecewise scale. */
export const DIV_RANGE = [...DIV_NEG, DIV_MID, ...DIV_POS];

export const MISSING = '#374151';

/** Muted network flow colour (the hovered link is orange). */
export const FLOW = 'rgb(24 137 176 / 0.55)';

export const FONT = '"Roboto Variable", Roboto, ui-sans-serif, system-ui, "Segoe UI", Arial, sans-serif';

// ---- Sector Insights (spec_sector.md 3.4; chart use only) ------------------------------------------
// The "who" lens: China | coalition core | coalition partners | rest of world | not attributed.
// Validated 2026-10-10 with the dataviz validator on the dark surface #1f2937, all pairs:
//   validate_palette.js "#1889b0,#8b9d17,#c96f7a" --mode dark --surface "#1f2937" --pairs all
//   -> ALL CHECKS PASS (worst CVD dE 8.2 protan rose/blue; normal-vision floor 20.8).
// Colour follows the entity class and never moves; China is always named in text. Strategic is
// unchanged (China stays in TIER_COLOR.other there).

export type GroupColorKey = 'china' | 'core' | 'partner' | 'rest';

export const GROUP_COLOR: Record<GroupColorKey, string> = {
  china: '#c96f7a',
  core: '#1889b0',
  partner: '#8b9d17',
  rest: '#6b7280',
};

/** Legend words of the groups (and of the hatched residual). */
export const GROUP_LABEL: Record<GroupColorKey | 'unattributed', string> = {
  china: 'China',
  core: 'Coalition core',
  partner: 'Coalition partners',
  rest: 'Rest of world',
  unattributed: 'Not attributed',
};

/** Phase-2 UN-vote bloc lens: the same validated triple with different legend words. */
export const BLOC_COLOR = { us_leaning: '#1889b0', nonaligned: '#6b7280', china_leaning: '#c96f7a' } as const;

/** World totals, regions and a single neutral series. */
export const NEUTRAL_SERIES = '#d1d5db';
/** An observed zero on maps (ink-600). */
export const ZERO_FILL = '#4b5563';
/** The "not attributed" / no-data hatch: 45 deg #9ca3af lines on ink-700 (SvgDefs draws it once a page). */
export const UNATTRIBUTED_HATCH_ID = 'sector-hatch';
export const HATCH_LINE = '#9ca3af';
export const HATCH_GROUND = '#374151';
/** The same hatch as CSS (HTML marks: share bars, heat tables, legends): an image over the ground. */
export const HATCH_IMAGE = `repeating-linear-gradient(45deg, ${HATCH_LINE} 0 1px, transparent 1px 5px)`;
export const HATCH_CSS = `${HATCH_IMAGE}, ${HATCH_GROUND}`;
export const HATCH_STYLE = { backgroundColor: HATCH_GROUND, backgroundImage: HATCH_IMAGE } as const;

/**
 * Ordinal ramps (one hue's OKLCH lightness steps, strong -> weak = light -> dark on the dark ground),
 * each validated with `validate_palette.js "<4 hex>" --ordinal --mode dark --surface "#1f2937"`
 * (monotone L, adjacent dL >= 0.06, darkest step >= 2:1 against the surface, single hue): ALL PASS.
 * GROUP_ORDINAL: CIM capacity status per country (operational, under construction, announced, paused)
 * in the country's group hue. VOLATILITY_ORDINAL: price volatility over 1, 5, 10 and 20 years.
 */
export const GROUP_ORDINAL: Record<GroupColorKey, readonly [string, string, string, string]> = {
  core: ['#6dcff8', '#47abd4', '#1889b0', '#00688e'],
  partner: ['#b9cd55', '#97aa2c', '#778800', '#586700'],
  china: ['#ffa2ac', '#db7f8a', '#b65e6a', '#923e4b'],
  rest: ['#b9c1d1', '#979fad', '#767d8c', '#575e6b'],
};
export const VOLATILITY_ORDINAL = ['#b4e7fe', '#5cbfe8', '#2792ba', '#016787'] as const;
