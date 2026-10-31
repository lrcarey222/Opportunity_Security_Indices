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
// The parts of a step (2026-10-31, yards.ts stepParts): once the economy's detail document is in (`detail`), a
// step with capability layers lists its parts in its tooltip, the rest of the step (its own choice) and each
// layer by name, each with a swatch, its choice and its fence; a mixed step leads with them. The list is capped
// (MAX_TIP_PARTS, then "+N more: open the step"); the accessible name names the parts' fences briefly. Until the
// detail is in, the tooltip and the name keep the roll-up words. The tooltip is fixed to the viewport and
// clamped inside it (a 375 px phone included), so it is never cut off by the map's scroller.

import { Fragment, useId, useLayoutEffect, useMemo, useRef, useState } from 'react';
import { DEPLOYMENT_STAGE } from '../lib/stages.ts';
import type { PostureCell, PostureCore, PostureDetail } from './contract.ts';
import { MEASURE_PLAIN, TERMS } from '../content/vocabulary.ts';
import { cellKey, score100 } from './derive.ts';
import {
  ALIGNMENT_LABEL, borderlineWords, EVIDENCE_LABEL, FENCE_LABEL, fenceChoiceWord, flagWords, nodeFence, nodeFill, NOT_ASSESSED_BORDER, RATIONALE_LABEL, stageDisplay,
  stageShort, VERB_LABEL, VERB_OUTLINE, verbOf,
} from './grammar.ts';
import { Swatch } from './PostureLegend.tsx';
import { detailByKey } from './sections.ts';
import { mixedFill, mixedYards, partChoiceWords, partsFenceWords, partsMixed, rollupWords, rollupYards, showsLargeYard, stepParts, type StepPart } from './yards.ts';

/** The helper text of the map (design section 4): what Need means on each side of the Deployment column. */
export const MAP_HELPER =
  'From Extraction to Final Assembly & Equipment, and at Recovery & Recycling, Need means supply-chain vulnerability. At Deployment & Use, it means how urgently the energy system needs more of the technology: expected demand, policy and climate gaps, and system requirements.';

/**
 * The map's measure words and scale: the same as the fence lists and the drawer's questions (supply-chain
 * vulnerability, deployment need; 0-100), so a cell's tooltip and the list beside it agree (render review, 2026-10-31).
 */
const NEED_WORD: Record<PostureCell['needType'], string> = { supply_chain: MEASURE_PLAIN.supplyChainNeed, deployment: MEASURE_PLAIN.deploymentNeed };

/** The map's narrowest width in px: the column minimums (64 + 5 x 28 + 2 x 8 + 32) and the eight 2 px gaps. */
export const MAP_MIN_WIDTH = 64 + 5 * 28 + 2 * 8 + 32 + 8 * 2;

/** The most parts a tooltip lists; a longer list shows one fewer and "+N more: open the step". */
export const MAX_TIP_PARTS = 6;

/**
 * A cell's accessible name: the step, its choice and fence, the Secure rationale, the parts' fences (`parts`,
 * once the detail is in; else the mixed roll-up words), the scores, policy, evidence and the flags.
 */
export function cellLabel(c: PostureCell, parts: readonly StepPart[] = []): string {
  const bits = [`${c.tech} ${stageDisplay(c.stage)}: ${fenceChoiceWord(c)}`];
  if (c.posture === 'N') {
    bits.push(c.needType === 'deployment' ? 'deployment need not measured' : 'need or prize not measured');
    if (parts.length) bits.push(partsFenceWords(parts, MAX_TIP_PARTS));
    return bits.join('; ');
  }
  if (c.posture === 'S' && c.rationale) bits.push(RATIONALE_LABEL[c.rationale]);
  if (parts.length) bits.push(partsFenceWords(parts, MAX_TIP_PARTS));
  else if (mixedYards(c)) bits.push((rollupWords(c) ?? '').replace(/^Mixed: /, 'mixed yards: '));
  if (c.need !== null) bits.push(`${NEED_WORD[c.needType].toLowerCase()} ${score100(c.need)}`);
  if (c.prize !== null) bits.push(`prize ${score100(c.prize)}`);
  if (c.cp !== null) bits.push(`position ${score100(c.cp)}`);
  if (c.alignment) bits.push(`policy ${ALIGNMENT_LABEL[c.alignment].toLowerCase()}`);
  if (c.evidence) bits.push(`evidence ${EVIDENCE_LABEL[c.evidence].toLowerCase()}`);
  if (c.borderline) bits.push('borderline');
  if (c.flag) bits.push('flagged for review');
  return bits.join('; ');
}

/**
 * The parts of a step in the tooltip: a lead line (mixed: the fences differ), then each part with its swatch,
 * name, choice and fence, at most MAX_TIP_PARTS lines.
 */
function TipParts({ parts, lead }: { parts: readonly StepPart[]; lead: boolean }) {
  const cut = parts.length > MAX_TIP_PARTS;
  const shown = cut ? parts.slice(0, MAX_TIP_PARTS - 1) : parts;
  const mixed = partsMixed(parts);
  return (
    <div className={lead ? 'mt-1.5' : 'mt-1.5 border-t border-white/10 pt-1.5'} data-testid="strategy-map-tip-parts" data-mixed={mixed ? 'true' : undefined}>
      <p className={mixed ? 'font-medium text-paper' : 'text-paper-mute'}>{mixed ? 'Mixed: parts of this step sit behind different fences' : 'Parts of this step'}</p>
      <ul className="mt-0.5 space-y-0.5">
        {shown.map((p) => (
          <li key={p.layerId ?? 'step'} className="flex items-start gap-1.5" data-part={p.layerId ?? 'step'} data-verb={p.verb ?? 'not_assessed'} data-fence={p.fence ?? undefined}>
            <span className="mt-[2px] inline-flex shrink-0">{p.verb ? <Swatch verb={p.verb} size={10} /> : <Swatch hatched size={10} />}</span>
            <span className="min-w-0">
              <span className="text-paper">{p.label}</span>: {partChoiceWords(p)}
            </span>
          </li>
        ))}
      </ul>
      {cut && (
        <p className="mt-0.5 text-paper-mute" data-testid="strategy-map-tip-more">
          +{parts.length - shown.length} more: open the step
        </p>
      )}
    </div>
  );
}

/** The tooltip's place: under the cell (above it when there is no room below), clamped inside the viewport. */
function useTipPlace(anchor: HTMLElement | null, tip: React.RefObject<HTMLDivElement | null>): { left: number; top: number } | null {
  const [pos, setPos] = useState<{ left: number; top: number } | null>(null);
  useLayoutEffect(() => {
    const el = tip.current;
    if (!anchor || !el) return;
    const place = () => {
      const a = anchor.getBoundingClientRect();
      const vw = document.documentElement.clientWidth || window.innerWidth;
      const vh = window.innerHeight || document.documentElement.clientHeight;
      const w = el.offsetWidth;
      const h = el.offsetHeight;
      const left = Math.round(Math.max(8, Math.min(a.left + a.width / 2 - w / 2, vw - w - 8)));
      const below = a.bottom + 4;
      const want = below + h > vh - 8 && a.top - h - 4 >= 8 ? a.top - h - 4 : below;
      // never past the screen's bottom edge (a cell partly scrolled out of view), never above its top
      const top = Math.round(Math.max(8, Math.min(want, vh - h - 8)));
      setPos((p) => (p && p.left === left && p.top === top ? p : { left, top }));
    };
    place();
    window.addEventListener('scroll', place, true);
    window.addEventListener('resize', place);
    return () => {
      window.removeEventListener('scroll', place, true);
      window.removeEventListener('resize', place);
    };
  });
  return pos;
}

function Tip({ c, partnerName, parts, anchor, id }: { c: PostureCell; partnerName: string | null; parts: readonly StepPart[]; anchor: HTMLElement | null; id: string }) {
  const v = verbOf(c);
  const fence = nodeFence(v, c.stage);
  const ref = useRef<HTMLDivElement>(null);
  const pos = useTipPlace(anchor, ref);
  // a mixed step leads with its parts (or, until the detail is in, its roll-up words)
  const lead = mixedYards(c) !== null || partsMixed(parts);
  return (
    <div
      ref={ref}
      id={id}
      className="pointer-events-none fixed z-30 w-72 max-w-[calc(100vw-16px)] rounded-md border border-white/15 bg-ink-900 p-2.5 text-left text-xs font-normal text-paper-dim shadow-lg"
      style={{ left: pos?.left ?? 0, top: pos?.top ?? 0, visibility: pos ? undefined : 'hidden' }}
      role="tooltip"
      data-testid="strategy-map-tip"
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
      {lead && parts.length > 0 && <TipParts parts={parts} lead />}
      {!parts.length && mixedYards(c) && (
        <p className="mt-1 text-paper" data-testid="strategy-map-tip-yards">
          {rollupWords(c)}
        </p>
      )}
      {c.posture === 'N' ? (
        <p className="mt-1">Not assessed: {c.needType === 'deployment' ? 'deployment need is not measured' : 'need or prize is not measured'}.</p>
      ) : (
        <p className="mt-1 tabular">
          {NEED_WORD[c.needType]} {score100(c.need)}
          {c.needType === 'supply_chain' ? ` · Prize ${score100(c.prize)}` : ''} · Position {score100(c.cp)} <span className="text-paper-mute">(0-100)</span>
        </p>
      )}
      {c.alignment && <p>Policy: {ALIGNMENT_LABEL[c.alignment]}</p>}
      {c.evidence && <p>Evidence: {EVIDENCE_LABEL[c.evidence]}</p>}
      {c.mode === 'partner' && <p>{c.partnerGap ? 'No credible allied solution yet' : `${TERMS.bestPlacedPartner}: ${partnerName ?? '—'}`}</p>}
      {c.borderline && <p className="text-signal">Borderline on {borderlineWords(c.borderline)}</p>}
      {c.flag && <p className="text-signal">{flagWords(c.flag)}</p>}
      {!lead && parts.length > 0 && <TipParts parts={parts} lead={false} />}
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
  /** the economy's detail document (null or omitted until it is in): the parts of each step with layers */
  detail?: PostureDetail | null;
}

export function PostureMatrix({ core, cells, selected, onSelect, names = {}, detail = null }: PostureMatrixProps) {
  const byKey = useMemo(() => new Map(cells.map((c) => [cellKey(c.tech, c.stage), c])), [cells]);
  // the parts of each step with layers, from the economy's detail (none until it is in)
  const partsByKey = useMemo(() => {
    const d = detailByKey(detail);
    return new Map(cells.map((c) => [cellKey(c.tech, c.stage), stepParts(core.meta, c, d.get(cellKey(c.tech, c.stage)) ?? null)]));
  }, [cells, detail, core.meta]);
  const [hover, setHover] = useState<{ key: string; el: HTMLElement } | null>(null);
  const helperId = useId();
  const tipId = useId();
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
                node = <span role="gridcell" aria-label={`${t} ${stageDisplay(s)}: no data`} className="h-9 rounded-[3px] bg-ink-700/40 sm:h-10" data-deployment={isDep ? 'true' : undefined} />;
              } else {
                const on = selected === k;
                const na = c.posture === 'N';
                const v = verbOf(c);
                const split = mixedFill(c);
                const parts = partsByKey.get(k) ?? [];
                const shown = hover?.key === k;
                node = (
                  <span role="gridcell" className="relative" data-deployment={isDep ? 'true' : undefined}>
                    <button
                      type="button"
                      aria-label={cellLabel(c, parts)}
                      aria-describedby={shown ? tipId : undefined}
                      aria-pressed={on}
                      onClick={() => {
                        // the drawer takes over from the tooltip (on a phone a tap would otherwise leave it over the sheet)
                        setHover(null);
                        onSelect(k);
                      }}
                      onMouseEnter={(e) => setHover({ key: k, el: e.currentTarget })}
                      onMouseLeave={() => setHover((h) => (h?.key === k ? null : h))}
                      onFocus={(e) => setHover({ key: k, el: e.currentTarget })}
                      onBlur={() => setHover((h) => (h?.key === k ? null : h))}
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
                      data-parts={parts.length || undefined}
                    />
                    {shown && hover && <Tip c={c} partnerName={c.partner1 ? (names[c.partner1] ?? c.partner1) : null} parts={parts} anchor={hover.el} id={tipId} />}
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
