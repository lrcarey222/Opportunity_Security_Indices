// The Sector view's headline: every country in one technology x six-stage node, placed by Industrial
// Opportunity (x, among the node's contenders) and Strategic Need (y). The thresholds split the plane
// into CATCH UP / PRESERVE / PARTNER / WATCH-DEPLOY; a Secure country keeps its true coordinates and is
// drawn as an outlined diamond (Secure overrides the quadrant). Point size = Competitive Position. Hover
// or focus for the detail, click for the country's profile.

import { useMemo, useState } from 'react';
import type { PostureCell, PostureCore } from './contract.ts';
import { pct, postureWord } from './derive.ts';
import { POSTURE_COLOR, QUADRANT_LABEL, RATIONALE_LABEL } from './grammar.ts';

const W = 640;
const H = 420;
const M = { l: 48, r: 16, t: 16, b: 40 };

export function PostureScatter({ core, cells, onOpen, highlight }: { core: PostureCore; cells: PostureCell[]; onOpen: (iso3: string) => void; highlight?: string | null }) {
  const [hover, setHover] = useState<string | null>(null);
  const names = useMemo(() => Object.fromEntries(core.countries.map((c) => [c.iso3, c.name])), [core.countries]);
  const pts = cells.filter((c) => c.need !== null && c.opp !== null);
  const x = (v: number) => M.l + v * (W - M.l - M.r);
  const y = (v: number) => H - M.b - v * (H - M.t - M.b);
  const tN = core.meta.need_threshold;
  const tO = core.meta.opportunity_threshold;
  const r = (cp: number | null) => 3.5 + 5 * (cp ?? 0);
  const hov = pts.find((c) => c.iso3 === hover) ?? null;
  const q = [
    { label: 'CATCH UP', x: (tO + 1) / 2, y: (tN + 1) / 2 },
    { label: 'PARTNER', x: tO / 2, y: (tN + 1) / 2 },
    { label: 'PRESERVE', x: (tO + 1) / 2, y: tN / 2 },
    { label: 'WATCH · DEPLOY', x: tO / 2, y: tN / 2 },
  ];
  const order = [...pts].sort((a, b) => (a.posture === 'S' ? 1 : 0) - (b.posture === 'S' ? 1 : 0) || (b.cp ?? 0) - (a.cp ?? 0));
  return (
    <div className="relative w-full" data-testid="posture-scatter">
      <svg viewBox={`0 0 ${W} ${H}`} className="h-auto w-full" role="img" aria-label={`Strategic Need against Industrial Opportunity for ${pts.length} countries`}>
        <rect x={M.l} y={M.t} width={W - M.l - M.r} height={H - M.t - M.b} fill="rgb(255 255 255 / 0.02)" />
        <line x1={x(tO)} x2={x(tO)} y1={M.t} y2={H - M.b} stroke="rgb(255 255 255 / 0.35)" strokeDasharray="4 3" />
        <line x1={M.l} x2={W - M.r} y1={y(tN)} y2={y(tN)} stroke="rgb(255 255 255 / 0.35)" strokeDasharray="4 3" />
        {q.map((k) => (
          <text key={k.label} x={x(k.x)} y={y(k.y)} textAnchor="middle" className="fill-paper-mute" style={{ fontSize: 12, letterSpacing: '0.08em', opacity: 0.55 }}>
            {k.label}
          </text>
        ))}
        {[0, 0.25, 0.5, 0.75, 1].map((v) => (
          <g key={v}>
            <text x={x(v)} y={H - M.b + 16} textAnchor="middle" className="fill-paper-mute" style={{ fontSize: 11 }}>
              {Math.round(v * 100)}
            </text>
            <text x={M.l - 8} y={y(v) + 4} textAnchor="end" className="fill-paper-mute" style={{ fontSize: 11 }}>
              {Math.round(v * 100)}
            </text>
          </g>
        ))}
        <text x={(M.l + W - M.r) / 2} y={H - 6} textAnchor="middle" className="fill-paper-dim" style={{ fontSize: 12 }}>
          Industrial Opportunity (percentile among the node&apos;s contenders)
        </text>
        <text transform={`translate(14 ${(M.t + H - M.b) / 2}) rotate(-90)`} textAnchor="middle" className="fill-paper-dim" style={{ fontSize: 12 }}>
          Strategic Need (percentile)
        </text>
        {order.map((c) => {
          const cx = x(c.opp as number);
          const cy = y(c.need as number);
          const on = hover === c.iso3 || highlight === c.iso3;
          const common = {

            tabIndex: 0,
            role: 'button',
            'aria-label': `${names[c.iso3] ?? c.iso3}: ${postureWord(c.posture, c.mode)}, need ${pct(c.need)}, opportunity ${pct(c.opp)}`,
            onMouseEnter: () => setHover(c.iso3),
            onMouseLeave: () => setHover((h) => (h === c.iso3 ? null : h)),
            onFocus: () => setHover(c.iso3),
            onBlur: () => setHover((h) => (h === c.iso3 ? null : h)),
            onClick: () => onOpen(c.iso3),
            onKeyDown: (e: React.KeyboardEvent) => (e.key === 'Enter' || e.key === ' ') && onOpen(c.iso3),
            style: { cursor: 'pointer', outline: 'none' },
          };
          if (c.posture === 'S') {
            const s = r(c.cp) + 2;
            return (
              <path key={c.iso3}
                {...common}
                d={`M${cx} ${cy - s}L${cx + s} ${cy}L${cx} ${cy + s}L${cx - s} ${cy}Z`}
                fill="rgb(31 41 55 / 0.9)"
                stroke={POSTURE_COLOR.S}
                strokeWidth={on ? 3 : 2}
                data-posture="S"
              />
            );
          }
          return (
            <circle key={c.iso3}
              {...common}
              cx={cx}
              cy={cy}
              r={r(c.cp)}
              fill={POSTURE_COLOR[c.posture]}
              fillOpacity={c.posture === 'D' ? 0.55 : 0.85}
              stroke={on ? '#ffffff' : '#1f2937'}
              strokeWidth={on ? 2.5 : 1.5}
              data-posture={c.posture}
            />
          );
        })}
      </svg>
      {hov && (
        <div className="pointer-events-none absolute right-2 top-2 w-64 rounded-md border border-white/15 bg-ink-900 p-2.5 text-xs text-paper-dim shadow-lg" role="status">
          <p className="font-medium text-paper">{names[hov.iso3] ?? hov.iso3}</p>
          <p>
            {postureWord(hov.posture, hov.mode)}
            {hov.posture === 'S' && hov.rationale ? ` · ${RATIONALE_LABEL[hov.rationale]} (overrides ${QUADRANT_LABEL[hov.quadrant ?? 'watch'] ?? '—'})` : ''}
          </p>
          <p className="tabular">
            Need {pct(hov.need)} · Opportunity {pct(hov.opp)} · Position {pct(hov.cp)}
          </p>
          <p className="tabular">World supply share {pct(hov.share, 1)}</p>
          {hov.mode === 'partner' && <p>{hov.partnerGap ? 'Partner gap' : `First partner: ${names[hov.partner1 ?? ''] ?? hov.partner1 ?? '—'}`}</p>}
          {hov.borderline && <p className="text-signal">Borderline on {hov.borderline}</p>}
          <p className="mt-1 text-paper-mute">Click to open the country profile.</p>
        </div>
      )}
    </div>
  );
}
