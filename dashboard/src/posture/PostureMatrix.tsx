// The Country Strategy Map (spec XI.3-5; design section 4; colour alone since 2026-10-30): technology rows x
// the six stages, with the stages' display names (STAGE_DISPLAY; the shorts on a phone). Each cell is one
// solid fill in its choice colour (grammar VERB_COLOR, grouped by fence in the legend); a node not assessed
// is empty and hatched; a node with no posture at all is a dim empty cell. No letters, arrows, dots, corner
// marks or border codes: the choice, its fence, the Secure rationale, policy alignment, evidence and
// borderline flags are in the cell's accessible name, its hover / focus tooltip, the table twin and the
// drawer. Deployment & Use reads another Need (expected demand, policy and climate gaps, system
// requirements), so a subtle divider runs before and after its column, under a compact header annotation
// "Supply-chain strategy -> | Energy-system strategy | -> Circularity", and the helper line says what Need
// means on each side. Every node is a button that opens the drawer (keyboard: Tab, Enter / Space); its focus
// and selection rings are white (signal orange is the Secure fill). The column minimums (MAP_MIN_WIDTH in all,
// 268 px) fit a phone's card (about 300 px at 375), so the Recovery & Recycling column is never cut off.
// Mixed yards (2026-10-30, yards.ts): a node whose layers sit in different yards (the core's roll-up) is split
// on the diagonal: its own choice colour on the upper left, the fence colour of each other yard on the lower
// right (Secure orange for the Small yard, the brand blue for the Medium yard, Watch ink-600 for the Large yard,
// with the paper-mute hairline wherever ink-600 shows). Still colour alone, no letters or arrows; the accessible
// name and the tooltip name the yards.

import { Fragment, useId, useMemo, useState } from 'react';
import { DEPLOYMENT_STAGE } from '../lib/stages.ts';
import type { PostureCell, PostureCore } from './contract.ts';
import { cellKey, pct } from './derive.ts';
import {
  ALIGNMENT_LABEL, borderlineWords, EVIDENCE_LABEL, FENCE_LABEL, fenceChoiceWord, flagWords, NEED_TYPE_LABEL, nodeFence, nodeFill, NOT_ASSESSED_BORDER, RATIONALE_LABEL, stageDisplay,
  stageShort, VERB_LABEL, VERB_OUTLINE, verbOf,
} from './grammar.ts';
import { Swatch } from './PostureLegend.tsx';
import { mixedFill, mixedYards, rollupWords, rollupYards, showsLargeYard } from './yards.ts';

/** The helper text of the map (design section 4): what Need means on each side of the Deployment column. */
export const MAP_HELPER =
  'From Extraction to Final Assembly & Equipment, and at Recovery & Recycling, Need means supply-chain vulnerability. At Deployment & Use, it means how urgently the energy system needs more of the technology: expected demand, policy and climate gaps, and system requirements.';

/** The map's narrowest width in px: the column minimums (64 + 5 x 28 + 2 x 8 + 32) and the eight 2 px gaps. */
export const MAP_MIN_WIDTH = 64 + 5 * 28 + 2 * 8 + 32 + 8 * 2;

export function cellLabel(c: PostureCell): string {
  const bits = [`${c.tech} ${stageDisplay(c.stage)}: ${fenceChoiceWord(c)}`];
  if (c.posture === 'N') {
    bits.push(c.needType === 'deployment' ? 'deployment need not measured' : 'need or prize not measured');
    return bits.join('; ');
  }
  if (c.posture === 'S' && c.rationale) bits.push(RATIONALE_LABEL[c.rationale]);
  if (mixedYards(c)) bits.push((rollupWords(c) ?? '').replace(/^Mixed: /, 'mixed yards: '));
  if (c.need !== null) bits.push(`${NEED_TYPE_LABEL[c.needType].toLowerCase()} ${pct(c.need)}`);
  if (c.prize !== null) bits.push(`prize ${pct(c.prize)}`);
  if (c.cp !== null) bits.push(`position ${pct(c.cp)}`);
  if (c.alignment) bits.push(`policy ${ALIGNMENT_LABEL[c.alignment].toLowerCase()}`);
  if (c.evidence) bits.push(`evidence ${EVIDENCE_LABEL[c.evidence].toLowerCase()}`);
  if (c.borderline) bits.push('borderline');
  if (c.flag) bits.push('flagged for review');
  return bits.join('; ');
}

function Tip({ c, partnerName }: { c: PostureCell; partnerName: string | null }) {
  const v = verbOf(c);
  const fence = nodeFence(v, c.stage);
  return (
    <div
      className="pointer-events-none absolute left-1/2 top-full z-20 mt-1 w-64 -translate-x-1/2 rounded-md border border-white/15 bg-ink-900 p-2.5 text-left text-xs font-normal text-paper-dim shadow-lg"
      role="tooltip"
    >
      <p className="font-medium text-paper">
        {c.tech} · {stageDisplay(c.stage)}
      </p>
      <p className="mt-1 flex items-center gap-1.5" data-testid="strategy-map-tip-choice">
        {v ? <Swatch verb={v} size={12} /> : <Swatch hatched size={12} />}
        <span className="font-medium text-paper">{v ? VERB_LABEL[v] : 'Not assessed'}</span>
        {fence && <span>· {FENCE_LABEL[fence]}</span>}
      </p>
      {c.posture === 'S' && c.rationale && <p>{RATIONALE_LABEL[c.rationale]}</p>}
      {mixedYards(c) && (
        <p className="mt-1 text-paper" data-testid="strategy-map-tip-yards">
          {rollupWords(c)}
        </p>
      )}
      {c.posture === 'N' ? (
        <p className="mt-1">Not assessed: {c.needType === 'deployment' ? 'deployment need is not measured' : 'need or prize is not measured'}.</p>
      ) : (
        <p className="mt-1 tabular">
          {NEED_TYPE_LABEL[c.needType]} {pct(c.need)}
          {c.needType === 'supply_chain' ? ` · Prize ${pct(c.prize)}` : ''} · Position {pct(c.cp)}
        </p>
      )}
      {c.alignment && <p>Policy: {ALIGNMENT_LABEL[c.alignment]}</p>}
      {c.evidence && <p>Evidence: {EVIDENCE_LABEL[c.evidence]}</p>}
      {c.mode === 'partner' && <p>{c.partnerGap ? 'No credible allied solution yet' : `Best-placed partner: ${partnerName ?? '—'}`}</p>}
      {c.borderline && <p className="text-signal">Borderline on {borderlineWords(c.borderline)}</p>}
      {c.flag && <p className="text-signal">{flagWords(c.flag)}</p>}
    </div>
  );
}

/** The divider beside the Deployment column: a thin dashed rule, presentational. */
function Divider({ header = false }: { header?: boolean }) {
  return (
    <span aria-hidden="true" className="flex justify-center" data-testid={header ? undefined : 'deployment-divider'}>
      <span className="h-full w-0 border-l border-dashed border-white/25" />
    </span>
  );
}

export interface PostureMatrixProps {
  core: PostureCore;
  cells: PostureCell[];
  selected: string | null;
  onSelect: (key: string) => void;
  /** iso3 -> name, for the first partner in the tooltip */
  names?: Record<string, string>;
}

export function PostureMatrix({ core, cells, selected, onSelect, names = {} }: PostureMatrixProps) {
  const byKey = useMemo(() => new Map(cells.map((c) => [cellKey(c.tech, c.stage), c])), [cells]);
  const [hover, setHover] = useState<string | null>(null);
  const helperId = useId();
  const stages = core.meta.stages;
  const dep = stages.indexOf(DEPLOYMENT_STAGE);
  // the columns: technology, then each stage, with a narrow divider column before and after Deployment & Use
  // (minimums 4rem, 1.75rem per stage, 2rem for Deployment & Use, 0.5rem per divider: MAP_MIN_WIDTH with the gaps)
  const tracks = stages.map((s) => (s === DEPLOYMENT_STAGE ? '0.5rem minmax(2rem, 1fr) 0.5rem' : 'minmax(1.75rem, 1fr)')).join(' ');
  const template = `minmax(4rem, 9rem) ${tracks}`;
  const before = dep < 0 ? stages.length : dep;
  const after = dep < 0 ? 0 : stages.length - dep - 1;
  return (
    <div className="w-full">
      {/* the compact header annotation; the helper line below says the same for screen readers */}
      {dep >= 0 && (
        <div aria-hidden="true" className="grid gap-[2px] pb-1 text-[10px] uppercase leading-tight tracking-wide text-paper-mute" style={{ gridTemplateColumns: template }} data-testid="strategy-map-band">
          <span />
          <span className="border-b border-white/10 pb-0.5 text-center" style={{ gridColumn: `span ${before}` }}>
            <span className="sm:hidden">Supply chain →</span>
            <span className="hidden sm:inline">Supply-chain strategy →</span>
          </span>
          <span />
          <span className="border-b border-signal/40 pb-0.5 text-center text-paper-dim">
            <span className="sm:hidden">Energy</span>
            <span className="hidden sm:inline">Energy-system strategy</span>
          </span>
          <span />
          {after > 0 && (
            <span className="border-b border-white/10 pb-0.5 text-center" style={{ gridColumn: `span ${after}` }}>
              <span className="sm:hidden">Circ.</span>
              <span className="hidden sm:inline">→ Circularity</span>
            </span>
          )}
        </div>
      )}
      <div role="grid" aria-label="Country Strategy Map: choice by technology and supply-chain stage" aria-describedby={helperId} data-testid="posture-matrix">
        <div role="row" className="grid gap-[2px]" style={{ gridTemplateColumns: template }}>
          <span role="columnheader">
            <span className="sr-only">Technology</span>
          </span>
          {stages.map((s) => (
            <Fragment key={s}>
              {s === DEPLOYMENT_STAGE && <Divider header />}
              <span
                role="columnheader"
                className={`px-0.5 pb-1 text-center text-[11px] font-medium leading-tight sm:text-xs ${s === DEPLOYMENT_STAGE ? 'text-paper' : 'text-paper-mute'}`}
                title={stageDisplay(s)}
                data-deployment={s === DEPLOYMENT_STAGE ? 'true' : undefined}
              >
                <span className="sm:hidden" aria-hidden="true">
                  {stageShort(s)}
                </span>
                <span className="sr-only sm:not-sr-only">{stageDisplay(s)}</span>
              </span>
              {s === DEPLOYMENT_STAGE && <Divider header />}
            </Fragment>
          ))}
        </div>
        {core.meta.techs.map((t) => (
          <div key={t} role="row" className="grid gap-[2px] pb-[2px]" style={{ gridTemplateColumns: template }}>
            <span role="rowheader" className="flex items-center pr-2 text-xs text-paper-dim sm:text-sm">
              {t}
            </span>
            {stages.map((s) => {
              const k = cellKey(t, s);
              const c = byKey.get(k);
              const isDep = s === DEPLOYMENT_STAGE;
              let node: React.ReactNode;
              if (!c) {
                node = <span role="gridcell" aria-label={`${t} ${stageDisplay(s)}: not classified`} className="h-9 rounded-[3px] bg-ink-700/40 sm:h-10" data-deployment={isDep ? 'true' : undefined} />;
              } else {
                const on = selected === k;
                const na = c.posture === 'N';
                const v = verbOf(c);
                const split = mixedFill(c);
                node = (
                  <span role="gridcell" className="relative" data-deployment={isDep ? 'true' : undefined}>
                    <button
                      type="button"
                      aria-label={cellLabel(c)}
                      aria-pressed={on}
                      onClick={() => onSelect(k)}
                      onMouseEnter={() => setHover(k)}
                      onMouseLeave={() => setHover((h) => (h === k ? null : h))}
                      onFocus={() => setHover(k)}
                      onBlur={() => setHover((h) => (h === k ? null : h))}
                      className="block h-9 w-full rounded-[3px] outline-none transition-[filter] hover:brightness-110 focus-visible:ring-2 focus-visible:ring-white focus-visible:ring-offset-2 focus-visible:ring-offset-ink-800 sm:h-10"
                      style={{
                        background: split ?? nodeFill(c),
                        border: na ? NOT_ASSESSED_BORDER : showsLargeYard(c) ? `1px solid ${VERB_OUTLINE.watch}` : undefined,
                        boxShadow: on ? '0 0 0 2px #111827, 0 0 0 4px #ffffff' : undefined,
                      }}
                      data-posture={c.posture}
                      data-verb={v ?? 'not_assessed'}
                      data-fence={nodeFence(v, c.stage) ?? undefined}
                      data-mode={c.mode ?? c.deployMode ?? undefined}
                      data-yards={c.yards ? rollupYards(c.yards).join(' ') : undefined}
                      data-mixed={split ? 'true' : undefined}
                    />
                    {hover === k && <Tip c={c} partnerName={c.partner1 ? (names[c.partner1] ?? c.partner1) : null} />}
                  </span>
                );
              }
              return (
                <Fragment key={k}>
                  {isDep && <Divider />}
                  {node}
                  {isDep && <Divider />}
                </Fragment>
              );
            })}
          </div>
        ))}
      </div>
      <p id={helperId} className="mt-2 text-xs text-paper-mute" data-testid="strategy-map-helper">
        <span className="sr-only">The Deployment &amp; Use column is set apart: supply-chain strategy before it, energy-system strategy in it, circularity after it. </span>
        {MAP_HELPER}
      </p>
    </div>
  );
}
