// The Country headline chart: technology x six-stage posture matrix. Background = posture; a Compete
// cell carries its mode mark (P Preserve, C Catch Up, the link Partner); a Secure cell its
// rationale letter; the border = policy alignment; opacity = evidence quality. Watch (a Deploy node
// with neither need nor opportunity) is named in the tooltip only. Every cell is a button that opens the
// drawer; its accessible name says everything the colour, mark and border say.

import { useMemo, useState } from 'react';
import type { PostureCell, PostureCore } from './contract.ts';
import { cellKey, pct, postureWord } from './derive.ts';
import {
  ALIGNMENT_BORDER, ALIGNMENT_ICON, ALIGNMENT_LABEL, evidenceOpacity, MODE_MARK, MOMENTUM_ARROW, MOMENTUM_LABEL, POSTURE_COLOR, QUADRANT_LABEL,
  RATIONALE_LABEL, RATIONALE_MARK, stageShort,
} from './grammar.ts';

export function cellLabel(c: PostureCell): string {
  const bits = [`${c.tech} ${c.stage}: ${postureWord(c.posture, c.mode)}`];
  if (c.posture === 'S' && c.rationale) bits.push(RATIONALE_LABEL[c.rationale]);
  if (c.posture === 'D' && c.quadrant === 'watch') bits.push('Watch');
  if (c.need !== null) bits.push(`need ${pct(c.need)}`);
  if (c.opp !== null) bits.push(`opportunity ${pct(c.opp)}`);
  if (c.alignment) bits.push(`policy ${ALIGNMENT_LABEL[c.alignment].toLowerCase()}`);
  if (c.evidence) bits.push(`evidence ${c.evidence}`);
  if (c.borderline) bits.push('borderline');
  if (c.momentum) bits.push(`trade momentum ${MOMENTUM_LABEL[c.momentum]}`);
  return bits.join('; ');
}

function Tip({ c }: { c: PostureCell }) {
  return (
    <div className="pointer-events-none absolute left-1/2 top-full z-20 mt-1 w-60 -translate-x-1/2 rounded-md border border-white/15 bg-ink-900 p-2.5 text-left text-xs text-paper-dim shadow-lg" role="tooltip">
      <p className="font-medium text-paper">
        {c.tech} · {c.stage}
      </p>
      <p className="mt-0.5">
        {postureWord(c.posture, c.mode)}
        {c.posture === 'S' && c.rationale ? ` · ${RATIONALE_LABEL[c.rationale]}` : ''}
        {c.posture === 'D' && c.quadrant ? ` · ${QUADRANT_LABEL[c.quadrant]}` : ''}
      </p>
      <p className="mt-1 tabular">
        Need {pct(c.need)} · Opportunity {pct(c.opp)} · Position {pct(c.cp)}
      </p>
      {c.alignment && <p>Policy: {ALIGNMENT_LABEL[c.alignment]}</p>}
      {c.mode === 'partner' && <p>{c.partnerGap ? 'Partner gap: no credible trusted supplier' : `First partner: ${c.partner1 ?? '—'}`}</p>}
      {c.momentum && <p>Trade momentum {MOMENTUM_ARROW[c.momentum]}</p>}
      {c.borderline && <p className="text-signal">Borderline on {c.borderline}</p>}
      {c.flag && <p className="text-signal">Check: {c.flag.replace(/_/g, ' ')}</p>}
    </div>
  );
}

export function PostureMatrix({ core, cells, selected, onSelect }: { core: PostureCore; cells: PostureCell[]; selected: string | null; onSelect: (key: string) => void }) {
  const byKey = useMemo(() => new Map(cells.map((c) => [cellKey(c.tech, c.stage), c])), [cells]);
  const [hover, setHover] = useState<string | null>(null);
  const stages = core.meta.stages;
  return (
    <div role="grid" aria-label="Posture by technology and supply-chain stage" className="w-full" data-testid="posture-matrix">
      <div role="row" className="grid gap-[2px]" style={{ gridTemplateColumns: `minmax(4.25rem, 9rem) repeat(${stages.length}, minmax(2.25rem, 1fr))` }}>
        <span role="columnheader">
          <span className="sr-only">Technology</span>
        </span>
        {stages.map((s) => (
          <span key={s} role="columnheader" className="truncate px-0.5 pb-1 text-center text-[11px] font-medium text-paper-mute sm:text-xs" title={s}>
            <span className="sm:hidden">{stageShort(s)}</span>
            <span className="hidden sm:inline">{s}</span>
          </span>
        ))}
      </div>
      {core.meta.techs.map((t) => (
        <div key={t} role="row" className="grid gap-[2px] pb-[2px]" style={{ gridTemplateColumns: `minmax(4.25rem, 9rem) repeat(${stages.length}, minmax(2.25rem, 1fr))` }}>
          <span role="rowheader" className="flex items-center pr-2 text-xs text-paper-dim sm:text-sm">
            {t}
          </span>
          {stages.map((s) => {
            const k = cellKey(t, s);
            const c = byKey.get(k);
            if (!c) {
              return (
                <span key={k} role="gridcell" aria-label={`${t} ${s}: not classified`} className="h-9 rounded-[3px] bg-ink-700/40 sm:h-10" />
              );
            }
            const mark = c.posture === 'C' && c.mode ? MODE_MARK[c.mode] : c.posture === 'S' && c.rationale ? RATIONALE_MARK[c.rationale] : '';
            const on = selected === k;
            return (
              <span key={k} role="gridcell" className="relative">
                <button
                  type="button"
                  aria-label={cellLabel(c)}
                  aria-pressed={on}
                  onClick={() => onSelect(k)}
                  onMouseEnter={() => setHover(k)}
                  onMouseLeave={() => setHover((h) => (h === k ? null : h))}
                  onFocus={() => setHover(k)}
                  onBlur={() => setHover((h) => (h === k ? null : h))}
                  className="relative flex h-9 w-full items-center justify-center rounded-[3px] text-sm font-semibold text-white outline-none focus-visible:ring-2 focus-visible:ring-signal sm:h-10"
                  style={{
                    background: POSTURE_COLOR[c.posture],
                    opacity: evidenceOpacity(c.evidence),
                    border: c.alignment ? ALIGNMENT_BORDER[c.alignment] : undefined,
                    boxShadow: on ? '0 0 0 2px #111827, 0 0 0 4px #ffffff' : undefined,
                  }}
                  data-posture={c.posture}
                  data-mode={c.mode ?? undefined}
                >
                  <span aria-hidden="true">{mark}</span>
                  {c.alignment && ALIGNMENT_ICON[c.alignment] && (
                    <span aria-hidden="true" className="absolute right-0.5 top-0 text-[9px] leading-none">
                      {ALIGNMENT_ICON[c.alignment]}
                    </span>
                  )}
                  {c.momentum && (
                    <span aria-hidden="true" className="absolute bottom-0 left-0.5 text-[9px] leading-none" data-momentum={c.momentum}>
                      {MOMENTUM_ARROW[c.momentum]}
                    </span>
                  )}
                </button>
                {hover === k && <Tip c={c} />}
              </span>
            );
          })}
        </div>
      ))}
    </div>
  );
}
