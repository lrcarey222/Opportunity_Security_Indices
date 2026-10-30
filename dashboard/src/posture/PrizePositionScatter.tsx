// The Prize & Position scatter (spec XIII; design 5b; since 2026-10-30 drawn on the shared quadrant-scatter
// core, charts/quadScatter.tsx, so it reads as the Legacy "Security and opportunity, sector by sector"
// chart the user preferred): one economy's producing-stage nodes placed by Industrial Opportunity (across,
// 0-100) and Competitive Position among the node's contenders (up, 0-100), with a stage filter. Each dot is
// coloured by its choice (grammar VERB_COLOR; the legend groups the choices by fence) and shaped by its stage
// (STAGE_SYMBOL, as on the Legacy chart); a Secure node keeps its true coordinates (Secure overrides the
// triage; it is never moved to an artificial position) and is drawn last, on top. The dashed lines are the
// prize threshold and the Preserve bar (the top quarter of the node's contenders), both from
// core.meta.thresholds; the four corner headings in the margins are tendencies, not rules (need and the
// Secure tests still matter). Labels: the selected node always (bold white, ringed white: orange is Secure
// here), the nodes with an industrial choice next, the Watch nodes where they fit clear of the other labels
// and dots. A node not assessed, or without a prize or position, is listed under the chart, grouped by why
// and by technology, not plotted. Hover and focus ring a point in white too.
//
// Plot draws the figure; over it, every point is a button (Tab, Enter / Space, click) with the node's words
// as its name: hover or focus rings it and shows the tooltip, a press opens the shared drawer (onOpen, spec
// XIX). A table twin and a CSV carry the same rows.

import { useCallback, useMemo, useState } from 'react';
import type * as Plot from '@observablehq/plot';
import { ChartCard } from '../charts/ChartCard.tsx';
import { PlotFigure, Tip } from '../charts/PlotFigure.tsx';
import {
  DashedRuleSwatch, QuadCorner, QuadCornerRow, quadLeft, quadRight, quadScatterOptions, quadTop, scatterHeight, STAGE_SYMBOL, StageSymbol,
} from '../charts/quadScatter.tsx';
import { useWidth } from '../charts/useWidth.ts';
import { TableView, type Col } from '../components/TableView.tsx';
import { Term } from '../components/Term.tsx';
import { Select } from '../components/ui.tsx';
import { isProducingStage, stageRank, STAGE_DISPLAY, type ProducingStage } from '../lib/stages.ts';
import { POSTURE_TERMS, PostureBasisTag } from './basis.tsx';
import type { PostureCell, PostureCore, Verb } from './contract.ts';
import { namePossessive, pct, postureCsvText, postureProvenance, unplottedWords } from './derive.ts';
import {
  ALIGNMENT_LABEL, choiceWord, EVIDENCE_LABEL, FENCE_LABEL, MOMENTUM_LABEL, nodeFence, RATIONALE_LABEL, stageDisplay, stageShort, VERB_COLOR, VERB_LABEL, VERB_OUTLINE, verbOf,
} from './grammar.ts';
import { ChoiceLegend, Swatch } from './PostureLegend.tsx';
import { PRIZE_QUADRANT_WORDS, prizePositionPoints, prizeTendency, type PrizePoint, type PrizeQuadrant } from './sections.ts';

export const PRIZE_POSITION_TITLE = 'Where is the prize, and where is the country already strong?';

const ALL = 'all';
/** The choices a producing-stage node can take, in the legend's order. */
const SCATTER_VERBS: Verb[] = ['secure', 'preserve', 'catch_up', 'partner', 'watch'];
/** The selection and focus ring: white, since signal orange is the Secure fill on this chart. */
const SELECT_RING = '#ffffff';
/** Drawing order: Watch first, Secure last (on top). */
const DRAW_RANK: Record<Verb, number> = { watch: 0, partner: 1, catch_up: 2, preserve: 3, accelerate: 3, secure: 4 };

export interface PrizePositionScatterProps {
  core: PostureCore;
  /** the economy's core cells */
  cells: PostureCell[];
  /** the economy's name, for the captions and the CSV */
  name: string;
  /** the selected node ("Tech|Stage"), ringed */
  selected?: string | null;
  /** open a node in the shared drawer */
  onOpen: (key: string) => void;
  /** the initial stage filter: "all" (default) or a producing stage id */
  stage?: string;
  title?: string;
  headingLevel?: 2 | 3;
  /** the basis tag in the title row (off when the page's Section carries it) */
  showBasis?: boolean;
  className?: string;
}

/** One plotted node on the 0-100 axes of the core. */
interface PrizeDatum {
  key: string;
  x: number;
  y: number;
  short: string;
  stage: string;
  verb: Verb;
  p: PrizePoint;
}

const momentumWords = (c: PostureCell) => (c.momentum ? MOMENTUM_LABEL[c.momentum] : 'unavailable');

/** A point's (or an unplotted node's) words, for its accessible name and the tooltip. */
export function prizePointLabel(c: PostureCell): string {
  const v = verbOf(c);
  const fence = nodeFence(v, c.stage);
  const bits = [
    `${c.tech} ${stageDisplay(c.stage)}: ${v ? VERB_LABEL[v] : choiceWord(c)}${c.posture === 'S' && c.rationale ? ` (${RATIONALE_LABEL[c.rationale].toLowerCase()})` : ''}${fence ? `, ${FENCE_LABEL[fence].toLowerCase()}` : ''}`,
  ];
  bits.push(`Industrial Opportunity ${pct(c.prize)}`, `Competitive Position ${pct(c.cp)}`, `supply-chain need ${pct(c.need)}`, `trade momentum ${momentumWords(c)}`);
  if (c.alignment) bits.push(`policy ${ALIGNMENT_LABEL[c.alignment].toLowerCase()}`);
  if (c.evidence) bits.push(`evidence ${EVIDENCE_LABEL[c.evidence].toLowerCase()}`);
  return bits.join('; ');
}

interface ScatterRow {
  key: string;
  tech: string;
  stage: string;
  choice: string;
  prize: number | null;
  cp: number | null;
  need: number | null;
  momentum: string;
  alignment: string | null;
  evidence: string | null;
  tendency: string | null;
  plotted: boolean;
}

const COLS: Col<ScatterRow>[] = [
  { id: 'tech', header: 'Technology', value: (r) => r.tech },
  { id: 'stage', header: 'Stage', value: (r) => stageDisplay(r.stage), sortValue: (r) => stageRank(r.stage) },
  { id: 'choice', header: 'Choice', value: (r) => r.choice },
  { id: 'prize', header: 'Industrial Opportunity', value: (r) => r.prize, cell: (r) => pct(r.prize), num: true },
  { id: 'cp', header: 'Competitive Position', value: (r) => r.cp, cell: (r) => pct(r.cp), num: true },
  { id: 'need', header: 'Supply-chain need', value: (r) => r.need, cell: (r) => pct(r.need), num: true },
  { id: 'momentum', header: 'Trade momentum', value: (r) => r.momentum },
  { id: 'alignment', header: 'Policy alignment', value: (r) => r.alignment },
  { id: 'evidence', header: 'Evidence', value: (r) => r.evidence },
  { id: 'tendency', header: 'Quadrant tendency', value: (r) => r.tendency },
  { id: 'plotted', header: 'On the chart', value: (r) => (r.plotted ? 'yes' : 'no: not assessed, or prize or position not measured') },
];

/** The legend: the choices by fence (with the counts on the chart), the stage shapes, the selection ring and the dashed lines. */
function PrizeLegend({ counts, stages, prize, cp }: { counts: Partial<Record<Verb, number>>; stages: readonly ProducingStage[]; prize: number; cp: number }) {
  return (
    <div className="space-y-2" data-testid="prize-position-legend">
      <ChoiceLegend counts={counts} verbs={SCATTER_VERBS} notAssessed={false} />
      <ul className="flex flex-wrap gap-x-4 gap-y-1.5 text-xs text-paper-dim" aria-label="How to read the Prize and Position dots">
        {stages.length > 1 &&
          stages.map((s) => (
            <li key={s} className="inline-flex items-center gap-1.5" data-stage={s}>
              <StageSymbol shape={STAGE_SYMBOL[s]} />
              {STAGE_DISPLAY[s]}
            </li>
          ))}
        <li className="inline-flex items-center gap-1.5">
          <StageSymbol shape="circle" stroke={SELECT_RING} />
          Selected
        </li>
        <li className="inline-flex items-center gap-1.5">
          <DashedRuleSwatch />
          Dashed lines: a high prize at {pct(prize)} or more; a strong position at {pct(cp)} or more among contenders
        </li>
      </ul>
    </div>
  );
}

export function PrizePositionScatter({
  core, cells, name, selected = null, onOpen, stage: initialStage = ALL, title = PRIZE_POSITION_TITLE, headingLevel = 3, showBasis = false, className,
}: PrizePositionScatterProps) {
  const t = core.meta.thresholds;
  const producing = useMemo(() => core.meta.stages.filter(isProducingStage), [core.meta.stages]);
  const [stage, setStage] = useState<string>(initialStage === ALL || (producing as string[]).includes(initialStage) ? initialStage : ALL);
  const [hover, setHover] = useState<string | null>(null);
  const { plotted, notPlotted } = useMemo(() => prizePositionPoints(cells, stage), [cells, stage]);
  const data = useMemo<PrizeDatum[]>(
    () =>
      plotted
        .map((p) => ({ key: p.key, x: p.prize * 100, y: p.cp * 100, short: `${p.tech} · ${stageShort(p.stage)}`, stage: p.stage, verb: p.verb, p }))
        .sort((a, b) => DRAW_RANK[a.verb] - DRAW_RANK[b.verb] || a.x - b.x),
    [plotted],
  );
  const build = useCallback(
    (width: number): Plot.PlotOptions | null =>
      quadScatterOptions<PrizeDatum>({
        width,
        height: scatterHeight(width),
        points: data,
        selected,
        // the nodes with an industrial choice are labelled first; Watch where it fits
        priority: (d) => d.verb !== 'watch',
        xLabel: 'Industrial Opportunity: the prize (percentile) →',
        yLabel: '↑ Competitive Position among contenders (percentile)',
        ruleX: t.prize * 100,
        ruleY: t.cp * 100,
        fill: (d) => VERB_COLOR[d.verb],
        stroke: (d) => VERB_OUTLINE[d.verb],
        stages: producing,
        pointer: false,
        // white, not signal orange: orange is Secure on this chart
        ring: SELECT_RING,
      }),
    [data, selected, t.prize, t.cp, producing],
  );
  const [ref, w] = useWidth<HTMLDivElement>();
  const H = scatterHeight(w || 440);
  const hov = data.find((d) => d.key === hover) ?? null;
  // the quadrant's tendency, or (a high-prize, top-quarter node that is not Preserve) why it did not pass the Preserve test
  const tend = (p: PrizePoint) => prizeTendency(p, t);
  const rows: ScatterRow[] = [
    ...plotted.map((p) => ({ cell: p.cell, plotted: true, tendency: tend(p).words })),
    ...notPlotted.map((c) => ({ cell: c, plotted: false, tendency: null })),
  ]
    .map(({ cell: c, plotted: on, tendency }) => ({
      key: `${c.tech}|${c.stage}`, tech: c.tech, stage: c.stage, choice: choiceWord(c), prize: c.prize, cp: c.cp, need: c.need, momentum: momentumWords(c),
      alignment: c.alignment ? ALIGNMENT_LABEL[c.alignment] : null, evidence: c.evidence ? EVIDENCE_LABEL[c.evidence] : null, tendency, plotted: on,
    }))
    .sort((a, b) => stageRank(a.stage) - stageRank(b.stage) || a.tech.localeCompare(b.tech));
  const verbCounts = Object.fromEntries(SCATTER_VERBS.map((v) => [v, plotted.filter((p) => p.verb === v).length])) as Record<Verb, number>;
  const counts = SCATTER_VERBS.filter((v) => verbCounts[v] > 0).map((v) => `${verbCounts[v]} ${VERB_LABEL[v]}`);
  const scope = stage === ALL ? 'producing-stage nodes' : `${stageDisplay(stage)} nodes`;
  const summary = `${name}: ${plotted.length} ${scope} placed by Industrial Opportunity and Competitive Position${counts.length ? ` (${counts.join(', ')})` : ''}${notPlotted.length ? `; ${notPlotted.length} not plotted` : ''}.`;
  const quad = (q: PrizeQuadrant, right: boolean) => (
    <QuadCorner title={PRIZE_QUADRANT_WORDS[q][0]} right={right}>
      {q === 'strong_weak_prize' ? 'mature or specialised: review' : PRIZE_QUADRANT_WORDS[q][1]}
    </QuadCorner>
  );
  const stageOptions = [{ value: ALL, label: 'All producing stages' }, ...producing.map((s) => ({ value: s, label: stageDisplay(s) }))];
  const prov = postureProvenance(core);
  const onKey = (key: string) => (e: React.KeyboardEvent) => {
    if (e.key === 'Enter' || e.key === ' ') {
      e.preventDefault();
      onOpen(key);
    }
  };
  return (
    <ChartCard
      className={className}
      headingLevel={headingLevel}
      title={title}
      badge={showBasis ? <PostureBasisTag note={null} /> : undefined}
      sub={
        <>
          Each dot is one of {namePossessive(name)} producing-stage nodes: a technology at one stage of its supply chain. Across:{' '}
          <Term id={POSTURE_TERMS.industrialOpportunity}>Industrial Opportunity</Term> (the prize). Up: <Term id={POSTURE_TERMS.competitivePosition}>Competitive Position</Term>{' '}
          among the node&apos;s contenders (how strong the country is today). The colour is the choice, the shape the stage; the dashed lines are the prize threshold and the
          Preserve bar.
        </>
      }
      tools={<Select label="Stage" value={stage} options={stageOptions} onChange={setStage} className="min-w-[12rem]" />}
      summary={summary}
      legend={<PrizeLegend counts={verbCounts} stages={stage === ALL ? producing : []} prize={t.prize} cp={t.cp} />}
      table={() => (
        <TableView rows={rows} cols={COLS} caption={`${name}: Industrial Opportunity and Competitive Position by node`} rowKey={(r) => r.key} onRowClick={(r) => onOpen(r.key)} selectedKey={selected} pageSize={60} />
      )}
      csv={() => ({
        name: `${name} prize and position${stage === ALL ? '' : ` ${stage}`}`,
        text: postureCsvText(
          rows.map((r) => ({
            technology: r.tech, stage: r.stage, stage_name: stageDisplay(r.stage), choice: r.choice, industrial_opportunity: r.prize, competitive_position: r.cp,
            supply_chain_need: r.need, trade_momentum: r.momentum, policy_alignment: r.alignment, evidence: r.evidence, quadrant_tendency: r.tendency, plotted: r.plotted,
          })),
          ['technology', 'stage', 'stage_name', 'choice', 'industrial_opportunity', 'competitive_position', 'supply_chain_need', 'trade_momentum', 'policy_alignment', 'evidence', 'quadrant_tendency', 'plotted'],
          prov,
        ),
      })}
      notes={
        <div className="space-y-1">
          <p>
            Dashed lines: a high prize at {pct(t.prize)} or more; a strong position at {pct(t.cp)} or more among the node&apos;s contenders (the top quarter). The corner headings
            are tendencies, not rules: need, the credible pathway and the Secure tests also decide.
          </p>
          {notPlotted.length > 0 && (
            <p data-testid="prize-position-unplotted">
              Not plotted ({notPlotted.length}): {unplottedWords(notPlotted, (c) => (verbOf(c) === null ? 'not assessed' : 'with prize or position not measured'))}.
            </p>
          )}
        </div>
      }
    >
      <div ref={ref} className="w-full" data-testid="prize-position-scatter">
        <QuadCornerRow testId="prize-position-quadrants" left={quad('strong_weak_prize', false)} right={quad('strong_strong', true)} />
        <div className="relative">
          <PlotFigure<PrizeDatum> build={build} height={H} ariaLabel={summary} />
          {/* the points as buttons over the figure: the same coordinates as Plot's dots */}
          <div className="absolute inset-x-0 top-0" style={{ height: H }} role="group" aria-label={`${name}: the plotted nodes; select one for its five questions`}>
            {data.map((d) => {
              const on = selected === d.key;
              return (
                <button
                  key={d.key}
                  type="button"
                  className={`absolute h-5 w-5 -translate-x-1/2 -translate-y-1/2 rounded-full border-2 outline-none hover:border-white focus-visible:border-white ${hover === d.key ? 'border-white' : 'border-transparent'}`}
                  style={{ left: quadLeft(d.x), top: quadTop(d.y, H) }}
                  aria-label={prizePointLabel(d.p.cell)}
                  aria-pressed={on}
                  data-key={d.key}
                  data-verb={d.verb}
                  data-x={+d.x.toFixed(2)}
                  data-y={+d.y.toFixed(2)}
                  onMouseEnter={() => setHover(d.key)}
                  onMouseLeave={() => setHover((h) => (h === d.key ? null : h))}
                  onFocus={() => setHover(d.key)}
                  onBlur={() => setHover((h) => (h === d.key ? null : h))}
                  onClick={() => onOpen(d.key)}
                  onKeyDown={onKey(d.key)}
                />
              );
            })}
            {hov && (
              <div
                aria-hidden="true"
                className="pointer-events-none absolute z-20 w-64 max-w-[18rem] border border-signal bg-ink-900 px-3 py-2 text-xs text-paper-dim"
                style={{ top: Math.max(0, quadTop(hov.y, H) - 10), ...(hov.x > 60 ? { right: `calc(${quadRight(hov.x)} + 14px)` } : { left: `calc(${quadLeft(hov.x)} + 14px)` }) }}
                data-testid="prize-position-tip"
              >
                <Tip
                  value={
                    <span className="inline-flex items-center gap-1.5">
                      <Swatch verb={hov.verb} size={12} />
                      {VERB_LABEL[hov.verb]}
                      <span className="font-normal text-paper-dim">· {FENCE_LABEL[nodeFence(hov.verb, hov.stage) ?? 'medium']}</span>
                    </span>
                  }
                  title={`${hov.p.tech} · ${stageDisplay(hov.stage)}`}
                  lines={[
                    hov.p.cell.posture === 'S' && hov.p.cell.rationale ? `Secure: ${RATIONALE_LABEL[hov.p.cell.rationale]}` : null,
                    `Supply-chain need ${pct(hov.p.need)}`,
                    `Industrial Opportunity ${pct(hov.p.prize)}`,
                    `Competitive Position ${pct(hov.p.cp)}`,
                    `Trade momentum: ${momentumWords(hov.p.cell)}`,
                    `Policy alignment: ${hov.p.cell.alignment ? ALIGNMENT_LABEL[hov.p.cell.alignment] : 'not assessed'}`,
                    `Evidence: ${hov.p.cell.evidence ? EVIDENCE_LABEL[hov.p.cell.evidence] : 'not stated'}`,
                    <span key="t" data-testid="prize-position-tendency">
                      {tend(hov.p).words}
                      {tend(hov.p).tendency ? ' (a tendency)' : ''}. Select for the five questions.
                    </span>,
                  ]}
                />
              </div>
            )}
          </div>
        </div>
        <QuadCornerRow below testId="prize-position-quadrants-low" left={quad('weak_weak', false)} right={quad('weak_strong_prize', true)} />
      </div>
    </ChartCard>
  );
}
