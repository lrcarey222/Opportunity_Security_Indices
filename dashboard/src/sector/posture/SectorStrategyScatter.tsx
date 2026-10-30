// The Sector strategy card's chart (design section 5, 2026-10-29): every economy of one technology x
// six-stage node, placed by Competitive Position (across, percentile among the node's contenders) and the
// relevant need (up: supply-chain need at the producing stages, deployment need at Deployment & Use). The
// node's prize barely varies across economies, so it is stated in words beside the chart, not plotted.
// Colour is the family (Secure, Compete, Deploy: the site's three backgrounds); the shape is the verb:
// Secure an outlined diamond at its true coordinates (Secure overrides the triage), Preserve a circle, Catch
// Up an upward triangle, Partner a square, Accelerate an up arrow, Watch a ring. Dashed lines: the need
// threshold (the deployment threshold at Deployment & Use) and, at the producing stages, the Preserve bar.
// An economy without a measured Competitive Position sits in a narrow strip on the left; where no economy
// of the node has one (Recovery & Recycling today), the economies are spread across the width in verb order
// instead, the axis says so and there is no Preserve line (nor, at a producing stage, any region label). Light region labels give
// the tendencies (regionLabels), never rules. Hover or focus for the detail; click (or Enter)
// opens the economy's Country Strategy Map on this node.

import { useState, type KeyboardEvent } from 'react';
import type { PostureMeta, Verb } from '../../posture/contract.ts';
import { pct } from '../../posture/derive.ts';
import {
  ALIGNMENT_LABEL, borderlineWords, EVIDENCE_LABEL, MOMENTUM_LABEL, NEED_TYPE_LABEL, PATHWAY_LABEL, POSTURE_COLOR, RATIONALE_LABEL, VERB_FAMILY, VERB_LONG, VERB_ORDER,
} from '../../posture/grammar.ts';
import { DEPLOYMENT_STAGE } from '../../lib/stages.ts';
import type { RegionLabel, StrategyPoint } from './derive.ts';

const W = 640;
const H = 420;
const M = { l: 48, r: 16, t: 16, b: 44 };
/** the strip of economies without a measured Competitive Position */
const STRIP = 34;
const GAP = 14;

const DARK = 'rgb(31 41 55 / 0.9)';

/** The marker of a verb at (cx, cy), drawn in its family colour. */
export function VerbMark({ verb, cx, cy, on = false, scale = 1 }: { verb: Verb; cx: number; cy: number; on?: boolean; scale?: number }) {
  const col = POSTURE_COLOR[VERB_FAMILY[verb]];
  const stroke = on ? '#ffffff' : '#1f2937';
  const sw = on ? 2.5 : 1.5;
  const s = 6.5 * scale;
  switch (verb) {
    case 'secure':
      return <path d={`M${cx} ${cy - s - 1.5}L${cx + s + 1.5} ${cy}L${cx} ${cy + s + 1.5}L${cx - s - 1.5} ${cy}Z`} fill={DARK} stroke={col} strokeWidth={on ? 3 : 2} />;
    case 'preserve':
      return <circle cx={cx} cy={cy} r={s - 0.5} fill={col} stroke={stroke} strokeWidth={sw} />;
    case 'catch_up':
      return <path d={`M${cx} ${cy - s - 1}L${cx + s} ${cy + s * 0.75}L${cx - s} ${cy + s * 0.75}Z`} fill={col} stroke={stroke} strokeWidth={sw} />;
    case 'partner':
      return <rect x={cx - s + 1} y={cy - s + 1} width={2 * s - 2} height={2 * s - 2} fill={col} stroke={stroke} strokeWidth={sw} />;
    case 'accelerate':
      return (
        <path
          d={`M${cx} ${cy - s - 1.5}L${cx + s} ${cy - 0.5}L${cx + s * 0.38} ${cy - 0.5}L${cx + s * 0.38} ${cy + s + 1}L${cx - s * 0.38} ${cy + s + 1}L${cx - s * 0.38} ${cy - 0.5}L${cx - s} ${cy - 0.5}Z`}
          fill={col}
          stroke={stroke}
          strokeWidth={sw}
        />
      );
    case 'watch':
      return <circle cx={cx} cy={cy} r={s - 1} fill={DARK} stroke={on ? '#ffffff' : col} strokeWidth={on ? 3 : 2.25} />;
  }
}

/** The scatter's legend: the verbs it can show, each with its mark and family, in words. */
export function VerbMarkLegend({ verbs, notPlotted }: { verbs: readonly Verb[]; notPlotted?: number }) {
  return (
    <div className="space-y-2 text-xs text-paper-dim" aria-label="Legend" data-testid="sector-strategy-legend">
      <div className="flex flex-wrap items-center gap-x-4 gap-y-2">
        {verbs.map((v) => (
          <span key={v} className="inline-flex items-center gap-1.5" data-verb={v}>
            <svg width="18" height="18" viewBox="0 0 18 18" aria-hidden="true">
              <VerbMark verb={v} cx={9} cy={9} scale={0.95} />
            </svg>
            {VERB_LONG[v]}
          </span>
        ))}
      </div>
      <p className="text-paper-mute">
        Colour is the family (Secure, Compete, Deploy), the shape the choice.
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
  const needName = dep ? 'Deployment need' : 'Supply-chain need';
  // economies without a measured position: spread across the strip, deterministically
  const stripX = (i: number) => M.l + 8 + ((i * 7) % (STRIP - 14));
  let k = 0;
  const spread = (i: number) => x(points.length === 1 ? 0.5 : 0.03 + (0.94 * i) / (points.length - 1));
  const order = noCp ? [...points].sort((a, b) => VERB_ORDER.indexOf(a.verb) - VERB_ORDER.indexOf(b.verb) || a.name.localeCompare(b.name)) : points;
  const placed = order.map((p, i) => ({ p, cx: noCp ? spread(i) : p.cp === null ? stripX(k++) : x(p.cp), cy: y(p.need) }));
  const hov = placed.find((q) => q.p.iso3 === hover) ?? null;
  const c = hov?.p.cell ?? null;
  return (
    <div className="relative w-full" data-testid="sector-strategy-scatter" data-stage={stage} data-cp={noCp ? 'none' : strip ? 'partial' : 'all'}>
      <svg viewBox={`0 0 ${W} ${H}`} className="h-auto w-full" role="group" aria-label={`${needName} against Competitive Position for ${points.length} economies`}>
        <rect x={x0} y={M.t} width={W - x0 - M.r} height={H - M.t - M.b} fill="rgb(255 255 255 / 0.02)" />
        {strip && (
          <g data-testid="cp-missing-strip">
            <rect x={M.l} y={M.t} width={STRIP} height={H - M.t - M.b} fill="rgb(255 255 255 / 0.04)" />
            <text x={M.l + STRIP / 2} y={H - M.b + 16} textAnchor="middle" className="fill-paper-mute" style={{ fontSize: 10 }}>
              no CP
            </text>
          </g>
        )}
        {!dep && !noCp && <line x1={x(thresholds.cp)} x2={x(thresholds.cp)} y1={M.t} y2={H - M.b} stroke="rgb(255 255 255 / 0.35)" strokeDasharray="4 3" data-testid="line-cp" />}
        <line x1={strip ? M.l : x0} x2={W - M.r} y1={y(tNeed)} y2={y(tNeed)} stroke="rgb(255 255 255 / 0.35)" strokeDasharray="4 3" data-testid="line-need" />
        {(noCp && !dep ? [] : regions).map((r) => (
          <text key={r.label} x={x(r.x)} y={y(r.y)} textAnchor="middle" className="fill-paper-mute" style={{ fontSize: 12, letterSpacing: '0.08em', opacity: 0.55 }} data-testid="region-label">
            {r.label}
          </text>
        ))}
        {[0, 0.25, 0.5, 0.75, 1].map((v) => (
          <g key={v}>
            {!noCp && (
              <text x={x(v)} y={H - M.b + 16} textAnchor="middle" className="fill-paper-mute" style={{ fontSize: 11 }}>
                {Math.round(v * 100)}
              </text>
            )}
            <text x={M.l - 8} y={y(v) + 4} textAnchor="end" className="fill-paper-mute" style={{ fontSize: 11 }}>
              {Math.round(v * 100)}
            </text>
          </g>
        ))}
        <text x={(x0 + W - M.r) / 2} y={H - 8} textAnchor="middle" className="fill-paper-dim" style={{ fontSize: 12 }}>
          {noCp
            ? 'Competitive Position not measured at this stage: economies spread out by choice'
            : dep
              ? 'Competitive Position (context: the choice reads deployment need)'
              : 'Competitive Position (percentile among contenders)'}
        </text>
        <text transform={`translate(14 ${(M.t + H - M.b) / 2}) rotate(-90)`} textAnchor="middle" className="fill-paper-dim" style={{ fontSize: 12 }}>
          {needName} (percentile)
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
            {hov.p.verb === 'secure' && c.rationale ? ` · ${RATIONALE_LABEL[c.rationale]} (overrides the triage)` : ''}
          </p>
          <p className="tabular">
            {NEED_TYPE_LABEL[c.needType]} {pct(c.need)} · Prize {c.prize === null ? 'not read here' : pct(c.prize)} · Position {c.cp === null ? 'not measured' : pct(c.cp)}
          </p>
          <p className="tabular">World supply share {pct(c.share, 1)}</p>
          {(hov.p.verb === 'catch_up' || hov.p.verb === 'partner') && c.pathway && <p>Credible pathway: {PATHWAY_LABEL[c.pathway].toLowerCase()}</p>}
          {hov.p.verb === 'partner' && <p>{c.partnerGap ? 'No credible allied solution yet' : `First trusted partner: ${c.partner1 ? (nameOf?.(c.partner1) ?? c.partner1) : '—'}`}</p>}
          {c.momentum && <p>Trade momentum: {MOMENTUM_LABEL[c.momentum]}</p>}
          {c.alignment && <p>Policy: {ALIGNMENT_LABEL[c.alignment].toLowerCase()}</p>}
          {c.evidence && <p>Evidence: {EVIDENCE_LABEL[c.evidence].toLowerCase()}</p>}
          {c.borderline && <p className="text-signal">Borderline on {borderlineWords(c.borderline)}</p>}
          <p className="mt-1 text-paper-mute">Click to open this node on its Country Strategy Map.</p>
        </div>
      )}
    </div>
  );
}
