// The Sector strategy card's chart (design section 5, 2026-10-29): every economy of one technology x
// six-stage node, placed by Competitive Position (across, percentile among the node's contenders) and the
// relevant need (up: supply-chain need at the producing stages, deployment need at Deployment & Use). The
// node's prize barely varies across economies, so it is stated in words beside the chart, not plotted.
// Since 2026-10-30 it takes the look of the shared quadrant scatter (charts/quadScatter.tsx; the Country
// page's Prize x Position chart): one dot per economy in its choice colour (grammar VERB_COLOR, the legend
// grouped by fence), radius 6 with a 1.5 px outline, a hover / focus ring in white (orange is Secure), the grid at 0 /
// 25 / 50 / 75 / 100 inside a faint frame, the axis labels at the ends ("... →", "↑ ..."). A Secure economy
// keeps its true coordinates (Secure overrides the triage) and is drawn last, on top. Dashed lines (paper-
// mute, 4,3): the need threshold (the deployment threshold at Deployment & Use) and, at the producing
// stages, the Preserve bar. It stays a React SVG, not Plot, for its strip and spread modes below.
// An economy without a measured Competitive Position sits in a narrow strip on the left; where no economy
// of the node has one (Recovery & Recycling today), the economies are spread across the width in verb order
// instead, the axis says so and there is no Preserve line (nor, at a producing stage, any region label). Light region labels give
// the tendencies (regionLabels), never rules. Hover or focus for the detail; click (or Enter)
// opens the economy's Country Strategy Map on this node.

import { useState, type KeyboardEvent } from 'react';
import { QUAD_DASH, QUAD_TICKS } from '../../charts/quadScatter.tsx';
import type { PostureMeta, Verb } from '../../posture/contract.ts';
import { pct } from '../../posture/derive.ts';
import {
  ALIGNMENT_LABEL, borderlineWords, EVIDENCE_LABEL, MOMENTUM_LABEL, NEED_TYPE_LABEL, PATHWAY_LABEL, RATIONALE_LABEL, VERB_COLOR, VERB_LONG, VERB_ORDER, VERB_OUTLINE,
} from '../../posture/grammar.ts';
import { ChoiceLegend } from '../../posture/PostureLegend.tsx';
import { mixedYards, rollupWords } from '../../posture/yards.ts';
import { PAPER_MUTE } from '../../theme/palette.ts';
import { DEPLOYMENT_STAGE } from '../../lib/stages.ts';
import { MEASURE_PLAIN } from '../../content/vocabulary.ts';
import type { RegionLabel, StrategyPoint } from './derive.ts';

const W = 640;
const H = 420;
const M = { l: 48, r: 20, t: 30, b: 40 };
/** the strip of economies without a measured Competitive Position */
const STRIP = 34;
const GAP = 14;

/** The dot radius of the shared quadrant scatter. */
const R = 6;
/** The hover / focus ring: white, since signal orange is the Secure fill. */
const RING = '#ffffff';

/** The mark of a verb at (cx, cy): a dot in its choice colour (the shared scatter's size and outline), ringed in white (orange is the Secure fill) when hovered or focused. */
export function VerbMark({ verb, cx, cy, on = false, scale = 1 }: { verb: Verb; cx: number; cy: number; on?: boolean; scale?: number }) {
  return (
    <>
      <circle cx={cx} cy={cy} r={R * scale} fill={VERB_COLOR[verb]} stroke={VERB_OUTLINE[verb]} strokeWidth={1.5} data-mark="dot" />
      {on && <circle cx={cx} cy={cy} r={R * scale + 3} fill="none" stroke={RING} strokeWidth={2.5} data-mark="ring" />}
    </>
  );
}

/** The scatter's legend: the choices the stage can take, by fence, in their colours and words. */
export function VerbMarkLegend({ verbs, notPlotted }: { verbs: readonly Verb[]; notPlotted?: number }) {
  return (
    <div className="space-y-2 text-xs text-paper-dim" data-testid="sector-strategy-legend">
      <ChoiceLegend verbs={verbs} notAssessed={false} />
      <p className="text-paper-mute">
        Colour is the choice, grouped by fence.
        {notPlotted ? ` ${notPlotted} not assessed or without a measured need: listed in the table, not plotted.` : ''}
      </p>
    </div>
  );
}

export interface SectorStrategyScatterProps {
  points: StrategyPoint[];
  stage: string;
  thresholds: PostureMeta['thresholds'];
  regions: RegionLabel[];
  onOpen: (iso3: string) => void;
  /** the coalition tier words for the tooltip ("Core member") */
  tierOf?: (iso3: string) => string | null;
  /** an economy's name (the first trusted partner in the tooltip) */
  nameOf?: (iso3: string) => string;
}

export function SectorStrategyScatter({ points, stage, thresholds, regions, onOpen, tierOf, nameOf }: SectorStrategyScatterProps) {
  const [hover, setHover] = useState<string | null>(null);
  const dep = stage === DEPLOYMENT_STAGE;
  // no measured position anywhere in the node: spread the economies out (x carries no value)
  const noCp = points.length > 0 && points.every((p) => p.cp === null);
  const strip = !noCp && points.some((p) => p.cp === null);
  const x0 = M.l + (strip ? STRIP + GAP : 0);
  const x = (v: number) => x0 + v * (W - x0 - M.r);
  const y = (v: number) => H - M.b - v * (H - M.t - M.b);
  const tNeed = dep ? thresholds.deployment : thresholds.need;
  // the measure's plain name (vocabulary MEASURE_PLAIN: "Supply-chain vulnerability"; the formal name is in the glossary)
  const needName = dep ? MEASURE_PLAIN.deploymentNeed : MEASURE_PLAIN.supplyChainNeed;
  // economies without a measured position: spread across the strip, deterministically
  const stripX = (i: number) => M.l + 8 + ((i * 7) % (STRIP - 14));
  let k = 0;
  const spread = (i: number) => x(points.length === 1 ? 0.5 : 0.03 + (0.94 * i) / (points.length - 1));
  const order = noCp ? [...points].sort((a, b) => VERB_ORDER.indexOf(a.verb) - VERB_ORDER.indexOf(b.verb) || a.name.localeCompare(b.name)) : points;
  // Secure last, so it is drawn on top (its coordinates are its own)
  const placed = order
    .map((p, i) => ({ p, cx: noCp ? spread(i) : p.cp === null ? stripX(k++) : x(p.cp), cy: y(p.need) }))
    .sort((a, b) => Number(a.p.verb === 'secure') - Number(b.p.verb === 'secure'));
  const hov = placed.find((q) => q.p.iso3 === hover) ?? null;
  const c = hov?.p.cell ?? null;
  return (
    <div className="relative w-full" data-testid="sector-strategy-scatter" data-stage={stage} data-cp={noCp ? 'none' : strip ? 'partial' : 'all'}>
      <svg viewBox={`0 0 ${W} ${H}`} className="h-auto w-full" role="group" aria-label={`${needName} against Competitive Position for ${points.length} economies`}>
        {/* the grid and the frame of the shared quadrant scatter */}
        <g aria-hidden="true" data-testid="scatter-grid">
          {QUAD_TICKS.map((v) => (
            <g key={v}>
              {!noCp && <line x1={x(v / 100)} x2={x(v / 100)} y1={M.t} y2={H - M.b} stroke={PAPER_MUTE} strokeOpacity={0.1} />}
              <line x1={x0} x2={W - M.r} y1={y(v / 100)} y2={y(v / 100)} stroke={PAPER_MUTE} strokeOpacity={0.1} />
            </g>
          ))}
          <rect x={x0} y={M.t} width={W - x0 - M.r} height={H - M.t - M.b} fill="none" stroke="rgb(255 255 255 / 0.12)" />
        </g>
        {strip && (
          <g data-testid="cp-missing-strip">
            <rect x={M.l} y={M.t} width={STRIP} height={H - M.t - M.b} fill="rgb(255 255 255 / 0.04)" />
            <text x={M.l + STRIP / 2} y={H - M.b + 16} textAnchor="middle" className="fill-paper-mute" style={{ fontSize: 10 }}>
              no data
            </text>
          </g>
        )}
        {!dep && !noCp && <line x1={x(thresholds.cp)} x2={x(thresholds.cp)} y1={M.t} y2={H - M.b} stroke={PAPER_MUTE} strokeDasharray={QUAD_DASH} data-testid="line-cp" />}
        <line x1={strip ? M.l : x0} x2={W - M.r} y1={y(tNeed)} y2={y(tNeed)} stroke={PAPER_MUTE} strokeDasharray={QUAD_DASH} data-testid="line-need" />
        {(noCp && !dep ? [] : regions).map((r) => (
          <text key={r.label} x={x(r.x)} y={y(r.y)} textAnchor="middle" className="fill-paper-mute" style={{ fontSize: 12, letterSpacing: '0.08em', opacity: 0.55 }} data-testid="region-label">
            {r.label}
          </text>
        ))}
        {QUAD_TICKS.map((t) => (
          <g key={t}>
            {!noCp && (
              <text x={x(t / 100)} y={H - M.b + 16} textAnchor="middle" className="fill-paper-mute" style={{ fontSize: 12 }}>
                {t}
              </text>
            )}
            <text x={M.l - 8} y={y(t / 100) + 4} textAnchor="end" className="fill-paper-mute" style={{ fontSize: 12 }}>
              {t}
            </text>
          </g>
        ))}
        <text x={noCp ? (x0 + W - M.r) / 2 : W - M.r} y={H - 4} textAnchor={noCp ? 'middle' : 'end'} className="fill-paper-mute" style={{ fontSize: 12 }}>
          {noCp
            ? 'Competitive Position not measured at this stage: economies spread out by choice'
            : dep
              ? 'Competitive Position (context only: the choice reads deployment need) →'
              : 'Competitive Position (higher = stronger, ranked among the main producers) →'}
        </text>
        <text x={4} y={M.t - 12} textAnchor="start" className="fill-paper-mute" style={{ fontSize: 12 }}>
          ↑ {needName} ({dep ? 'higher = more urgent' : 'higher = more vulnerable'})
        </text>
        {placed.map(({ p, cx, cy }) => {
          const on = hover === p.iso3;
          const off = () => setHover((h) => (h === p.iso3 ? null : h));
          return (
            <g
              key={p.iso3}
              tabIndex={0}
              role="button"
              aria-label={`${p.name}: ${VERB_LONG[p.verb]}, ${needName.toLowerCase()} ${pct(p.need)}, Competitive Position ${p.cp === null ? 'not measured' : pct(p.cp)}`}
              onMouseEnter={() => setHover(p.iso3)}
              onMouseLeave={off}
              onFocus={() => setHover(p.iso3)}
              onBlur={off}
              onClick={() => onOpen(p.iso3)}
              onKeyDown={(e: KeyboardEvent) => {
                if (e.key === 'Enter' || e.key === ' ') {
                  e.preventDefault();
                  onOpen(p.iso3);
                }
              }}
              style={{ cursor: 'pointer', outline: 'none' }}
              data-verb={p.verb}
              data-iso3={p.iso3}
            >
              <VerbMark verb={p.verb} cx={cx} cy={cy} on={on} />
            </g>
          );
        })}
      </svg>
      {hov && c && (
        <div className="pointer-events-none absolute right-2 top-2 w-64 rounded-md border border-white/15 bg-ink-900 p-2.5 text-xs text-paper-dim shadow-lg" role="status">
          <p className="font-medium text-paper">
            {hov.p.name}
            {tierOf?.(hov.p.iso3) ? <span className="font-normal text-paper-mute"> · {tierOf(hov.p.iso3)}</span> : null}
          </p>
          <p>
            {VERB_LONG[hov.p.verb]}
            {hov.p.verb === 'secure' && c.rationale ? ` · ${RATIONALE_LABEL[c.rationale]} (overrides the other choices)` : ''}
          </p>
          <p className="tabular">
            {NEED_TYPE_LABEL[c.needType]} {pct(c.need)} · Prize {c.prize === null ? 'not read here' : pct(c.prize)} · Position {c.cp === null ? 'not measured' : pct(c.cp)}
          </p>
          <p className="tabular">World supply share {pct(c.share, 1)}</p>
          {(hov.p.verb === 'catch_up' || hov.p.verb === 'partner') && c.pathway && <p>Credible pathway: {PATHWAY_LABEL[c.pathway].toLowerCase()}</p>}
          {hov.p.verb === 'partner' && <p>{c.partnerGap ? 'No credible allied solution yet' : `Best-placed trusted partner: ${c.partner1 ? (nameOf?.(c.partner1) ?? c.partner1) : '—'}`}</p>}
          {mixedYards(c) && <p data-testid="scatter-tip-mixed">{rollupWords(c)}</p>}
          {c.momentum && <p>Trade momentum: {MOMENTUM_LABEL[c.momentum]}</p>}
          {c.alignment && <p>Policy: {ALIGNMENT_LABEL[c.alignment].toLowerCase()}</p>}
          {c.evidence && <p>Evidence: {EVIDENCE_LABEL[c.evidence].toLowerCase()}</p>}
          {c.borderline && <p className="text-signal">Borderline on {borderlineWords(c.borderline)}</p>}
          <p className="mt-1 text-paper-mute">Click to open this step on its Country Strategy Map.</p>
        </div>
      )}
    </div>
  );
}
