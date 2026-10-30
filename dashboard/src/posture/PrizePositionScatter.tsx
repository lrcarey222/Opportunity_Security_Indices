// The Prize & Position scatter (spec XIII; design 5b), presentational: one economy's producing-stage nodes
// placed by Industrial Opportunity (x, low -> high) and Competitive Position (y, weak -> strong), with a
// stage filter. Each point is drawn by its choice: Preserve a circle, Catch Up a triangle, Partner a square
// (Compete colour), Watch a ring (Deploy colour); a Secure node keeps its true coordinates and is drawn as an
// outlined diamond (Secure overrides the triage; it is never moved to an artificial position). Evidence
// quality fades a point (the grammar's opacity). The dashed lines are the prize threshold and the Preserve
// bar (the top quarter of the node's contenders); the quadrant labels are light and worded as tendencies,
// not rules: need and the Secure tests still matter. A node not assessed, or without a prize or position,
// is listed under the chart, not plotted. Every point is a button (Tab, Enter / Space) that opens the
// shared drawer (onOpen, spec XIX). A table twin and a CSV carry the same rows.

import { useMemo, useState } from 'react';
import { ChartCard } from '../charts/ChartCard.tsx';
import { TableView, type Col } from '../components/TableView.tsx';
import { Term } from '../components/Term.tsx';
import { Select } from '../components/ui.tsx';
import { isProducingStage, stageRank } from '../lib/stages.ts';
import { POSTURE_TERMS, PostureBasisTag } from './basis.tsx';
import type { PostureCell, PostureCore, Verb } from './contract.ts';
import { pct, postureCsvText, postureProvenance } from './derive.ts';
import {
  ALIGNMENT_LABEL, choiceWord, EVIDENCE_LABEL, evidenceOpacity, MOMENTUM_LABEL, POSTURE_COLOR, RATIONALE_LABEL, stageDisplay, VERB_FAMILY, VERB_LABEL, VERB_SHAPE, verbOf,
} from './grammar.ts';
import { PRIZE_QUADRANT_WORDS, prizePositionPoints, prizeTendency, type PrizePoint, type PrizeQuadrant } from './sections.ts';

export const PRIZE_POSITION_TITLE = 'Where is the prize, and where is the country already strong?';

const W = 640;
const H = 440;
const M = { l: 52, r: 16, t: 16, b: 44 };
const ALL = 'all';

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

type ScatterVerb = Exclude<Verb, 'accelerate'>;

/** One point's shape, centred at (x, y), radius r. */
function Shape({ verb, x, y, r, on, ...rest }: { verb: ScatterVerb; x: number; y: number; r: number; on: boolean } & React.SVGProps<SVGPathElement & SVGCircleElement>) {
  const shape = VERB_SHAPE[verb];
  const fam = POSTURE_COLOR[VERB_FAMILY[verb]];
  const ring = on ? '#ffffff' : '#111827';
  if (shape === 'diamond') {
    const s = r + 2.5;
    return <path {...rest} d={`M${x} ${y - s}L${x + s} ${y}L${x} ${y + s}L${x - s} ${y}Z`} fill="rgb(17 24 39 / 0.85)" stroke={on ? '#ffffff' : fam} strokeWidth={on ? 3 : 2.25} />;
  }
  if (shape === 'triangle') {
    const s = r + 1.5;
    return <path {...rest} d={`M${x} ${y - s}L${x + s} ${y + s * 0.8}L${x - s} ${y + s * 0.8}Z`} fill={fam} stroke={ring} strokeWidth={on ? 2.5 : 1.25} />;
  }
  if (shape === 'square') {
    const s = r * 0.9;
    return <path {...rest} d={`M${x - s} ${y - s}H${x + s}V${y + s}H${x - s}Z`} fill={fam} stroke={ring} strokeWidth={on ? 2.5 : 1.25} />;
  }
  if (shape === 'ring') return <circle {...rest} cx={x} cy={y} r={r} fill="rgb(17 24 39 / 0.6)" stroke={on ? '#ffffff' : fam} strokeWidth={on ? 3 : 2.25} />;
  return <circle {...rest} cx={x} cy={y} r={r} fill={fam} stroke={ring} strokeWidth={on ? 2.5 : 1.25} />;
}

/** The legend: the five shapes in words. */
function ScatterLegend() {
  const items: ScatterVerb[] = ['preserve', 'catch_up', 'partner', 'watch', 'secure'];
  return (
    <div className="flex flex-wrap items-center gap-x-4 gap-y-2 text-xs text-paper-dim" data-testid="prize-position-legend">
      {items.map((v) => (
        <span key={v} className="inline-flex items-center gap-1.5">
          <svg width="16" height="16" viewBox="0 0 16 16" aria-hidden="true">
            <Shape verb={v} x={8} y={8} r={5} on={false} />
          </svg>
          {v === 'secure' ? 'Secure (outlined, at its true position)' : VERB_LABEL[v]}
        </span>
      ))}
      <span>Faded: weaker evidence</span>
    </div>
  );
}

const momentumWords = (c: PostureCell) => (c.momentum ? MOMENTUM_LABEL[c.momentum] : 'unavailable');

/** A point's (or an unplotted node's) words, for its accessible name and the tooltip. */
export function prizePointLabel(c: PostureCell): string {
  const bits = [`${c.tech} ${stageDisplay(c.stage)}: ${choiceWord(c)}${c.posture === 'S' && c.rationale ? ` (${RATIONALE_LABEL[c.rationale].toLowerCase()})` : ''}`];
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

export function PrizePositionScatter({
  core, cells, name, selected = null, onOpen, stage: initialStage = ALL, title = PRIZE_POSITION_TITLE, headingLevel = 3, showBasis = false, className,
}: PrizePositionScatterProps) {
  const t = core.meta.thresholds;
  const producing = core.meta.stages.filter(isProducingStage);
  const [stage, setStage] = useState<string>(initialStage === ALL || (producing as string[]).includes(initialStage) ? initialStage : ALL);
  const [hover, setHover] = useState<string | null>(null);
  const { plotted, notPlotted } = useMemo(() => prizePositionPoints(cells, stage), [cells, stage]);
  const x = (v: number) => M.l + v * (W - M.l - M.r);
  const y = (v: number) => H - M.b - v * (H - M.t - M.b);
  // Secure last, so its outline sits on top
  const order = [...plotted].sort((a, b) => Number(a.verb === 'secure') - Number(b.verb === 'secure') || a.prize - b.prize);
  const hov = plotted.find((p) => p.key === hover) ?? null;
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
  const counts = (['preserve', 'catch_up', 'partner', 'watch', 'secure'] as ScatterVerb[])
    .map((v) => [v, plotted.filter((p) => p.verb === v).length] as const)
    .filter(([, n]) => n > 0)
    .map(([v, n]) => `${n} ${VERB_LABEL[v]}`);
  const scope = stage === ALL ? 'producing-stage nodes' : `${stageDisplay(stage)} nodes`;
  const summary = `${name}: ${plotted.length} ${scope} placed by Industrial Opportunity and Competitive Position${counts.length ? ` (${counts.join(', ')})` : ''}${notPlotted.length ? `; ${notPlotted.length} not plotted` : ''}.`;
  const QUAD_POS: Record<PrizeQuadrant, { x: number; y: number; anchor: 'start' | 'end' }> = {
    strong_strong: { x: x(1) - 6, y: y(1) + 14, anchor: 'end' },
    strong_weak_prize: { x: x(0) + 6, y: y(1) + 14, anchor: 'start' },
    weak_weak: { x: x(0) + 6, y: y(0) - 20, anchor: 'start' },
    weak_strong_prize: { x: x(1) - 6, y: y(0) - 20, anchor: 'end' },
  };
  const stageOptions = [{ value: ALL, label: 'All producing stages' }, ...producing.map((s) => ({ value: s, label: stageDisplay(s) }))];
  const prov = postureProvenance(core);
  return (
    <ChartCard
      className={className}
      headingLevel={headingLevel}
      title={title}
      badge={showBasis ? <PostureBasisTag note={null} /> : undefined}
      sub={
        <>
          Each producing-stage node by <Term id={POSTURE_TERMS.industrialOpportunity}>Industrial Opportunity</Term> (the prize) and{' '}
          <Term id={POSTURE_TERMS.competitivePosition}>Competitive Position</Term> (how strong the country is today). The marker is the choice.
        </>
      }
      tools={<Select label="Stage" value={stage} options={stageOptions} onChange={setStage} className="min-w-[12rem]" />}
      summary={summary}
      legend={<ScatterLegend />}
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
            Dashed lines: a high prize at {pct(t.prize)} or more; a strong position at {pct(t.cp)} or more among the node&apos;s contenders (the top quarter). The quadrant labels
            are tendencies, not rules: need, the credible pathway and the Secure tests also decide.
          </p>
          {notPlotted.length > 0 && (
            <p data-testid="prize-position-unplotted">
              Not plotted ({notPlotted.length}):{' '}
              {notPlotted
                .slice()
                .sort((a, b) => stageRank(a.stage) - stageRank(b.stage) || a.tech.localeCompare(b.tech))
                .map((c) => `${c.tech} ${stageDisplay(c.stage)} (${verbOf(c) === null ? 'not assessed' : 'prize or position not measured'})`)
                .join('; ')}
              .
            </p>
          )}
        </div>
      }
    >
      <div className="relative w-full" data-testid="prize-position-scatter">
        <svg viewBox={`0 0 ${W} ${H}`} className="h-auto w-full" role="group" aria-label={summary}>
          <rect x={M.l} y={M.t} width={W - M.l - M.r} height={H - M.t - M.b} fill="rgb(255 255 255 / 0.02)" />
          <line x1={x(t.prize)} x2={x(t.prize)} y1={M.t} y2={H - M.b} stroke="rgb(255 255 255 / 0.3)" strokeDasharray="4 3" />
          <line x1={M.l} x2={W - M.r} y1={y(t.cp)} y2={y(t.cp)} stroke="rgb(255 255 255 / 0.3)" strokeDasharray="4 3" />
          <g aria-hidden="true" data-testid="prize-position-quadrants">
            {(Object.keys(QUAD_POS) as PrizeQuadrant[]).map((q) => (
              <text key={q} x={QUAD_POS[q].x} y={QUAD_POS[q].y} textAnchor={QUAD_POS[q].anchor} className="fill-paper-mute" style={{ fontSize: 11, opacity: 0.6 }}>
                <tspan x={QUAD_POS[q].x}>{PRIZE_QUADRANT_WORDS[q][0]}</tspan>
                <tspan x={QUAD_POS[q].x} dy={13} style={{ fontStyle: 'italic' }}>
                  {q === 'strong_weak_prize' ? 'mature or specialised: review' : PRIZE_QUADRANT_WORDS[q][1]}
                </tspan>
              </text>
            ))}
          </g>
          <g aria-hidden="true">
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
            <text x={(M.l + W - M.r) / 2} y={H - 8} textAnchor="middle" className="fill-paper-dim" style={{ fontSize: 12 }}>
              Industrial Opportunity: low → high
            </text>
            <text transform={`translate(15 ${(M.t + H - M.b) / 2}) rotate(-90)`} textAnchor="middle" className="fill-paper-dim" style={{ fontSize: 12 }}>
              Competitive Position: weak → strong
            </text>
          </g>
          {order.map((p: PrizePoint) => {
            const on = selected === p.key || hover === p.key;
            const v = p.verb as ScatterVerb;
            return (
              <Shape
                key={p.key}
                verb={v}
                x={x(p.prize)}
                y={y(p.cp)}
                r={6}
                on={on}
                tabIndex={0}
                role="button"
                aria-label={prizePointLabel(p.cell)}
                aria-pressed={selected === p.key}
                data-key={p.key}
                data-verb={p.verb}
                opacity={evidenceOpacity(p.cell.evidence)}
                style={{ cursor: 'pointer', outline: 'none' }}
                onMouseEnter={() => setHover(p.key)}
                onMouseLeave={() => setHover((h) => (h === p.key ? null : h))}
                onFocus={() => setHover(p.key)}
                onBlur={() => setHover((h) => (h === p.key ? null : h))}
                onClick={() => onOpen(p.key)}
                onKeyDown={(e: React.KeyboardEvent) => {
                  if (e.key === 'Enter' || e.key === ' ') {
                    e.preventDefault();
                    onOpen(p.key);
                  }
                }}
              />
            );
          })}
        </svg>
        {hov && (
          <div aria-hidden="true" className="pointer-events-none absolute right-2 top-2 w-64 rounded-md border border-white/15 bg-ink-900 p-2.5 text-xs text-paper-dim shadow-lg" data-testid="prize-position-tip">
            <p className="font-medium text-paper">
              {hov.tech} · {stageDisplay(hov.stage)}
            </p>
            <p>
              {choiceWord(hov.cell)}
              {hov.cell.posture === 'S' && hov.cell.rationale ? ` · ${RATIONALE_LABEL[hov.cell.rationale]}` : ''}
            </p>
            <p className="tabular">Supply-chain need {pct(hov.need)}</p>
            <p className="tabular">Industrial Opportunity {pct(hov.prize)}</p>
            <p className="tabular">Competitive Position {pct(hov.cp)}</p>
            <p>Trade momentum: {momentumWords(hov.cell)}</p>
            <p>Policy alignment: {hov.cell.alignment ? ALIGNMENT_LABEL[hov.cell.alignment] : 'not assessed'}</p>
            <p>Evidence: {hov.cell.evidence ? EVIDENCE_LABEL[hov.cell.evidence] : 'not stated'}</p>
            <p className="mt-1 text-paper-mute" data-testid="prize-position-tendency">
              {tend(hov).words}
              {tend(hov).tendency ? ' (a tendency)' : ''}. Select for the five questions.
            </p>
          </div>
        )}
      </div>
    </ChartCard>
  );
}
