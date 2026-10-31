// The node drawer (spec XI.8; design D14): one country x technology x six-stage node, answered in exactly
// five questions, in order: NEED, PRIZE, POSITION, CHOICE, POLICY ALIGNMENT. Each gives its numbers, the
// components behind them, their basis and "not assessed" where a value is missing (never a zero).
// - NEED: supply-chain need at the producing stages (import dependence, supplier concentration, risky
//   supplier exposure, trusted alternatives, reconstitution and substitutability); at Deployment & Use the
//   deployment need families instead (demand adequacy, target / climate gap, system need, each with its
//   basis and scenario; a climate-aligned value only as a labelled alternative), never supply-chain need.
// - PRIZE: the Strategic Industrial Value (2026-10-30: is domestic capability here strategically valuable?)
//   and its three families: the market prize (Industrial Opportunity, with its components), industrial commons
//   and future option value (meta.yf_nodes), capped where only the market speaks for it; a core whose prize is
//   the market measure shows Industrial Opportunity alone (at Deployment: why the prize is read upstream, and
//   the technology's domestic manufacturing priority beside its deployment priority).
// - POSITION: Competitive Position, its families, world supply share and the trade evidence with momentum.
// - CHOICE: the choice with its fence (the page's: grammar nodeFence), rationale, delivery mode, mitigation
//   and the policy design text (its trade protection labelled as such, never as the fence), pathway, partners,
//   the kind of choice (the engine variant, yards.ts variantWords), the compact trace, borderline and pivotal
//   inputs, confidence and the consistency flag; then the Yards part
//   (yards.ts): the node's own yard and its roll-up, each child layer with its yard, choice and one-line
//   reason, the fence scope of a Secure layer, the node's large-yard case and the market-only cap. A layer
//   unit of the fence sections opens the drawer focused on its layer (?layer=, `focusLayer`).
// - POLICY ALIGNMENT: observed policy intensity, support and protection, momentum (each a percentile rank
//   within the country, never a share or a change; protection "none recorded" where the records hold none),
//   sources, the alignment and its confidence; observed policy is a comparator, never an input.
// The core cell gives the headline values at once; the detail document (fetched on first open) the rest.
// A side panel on wide screens, a bottom sheet on phones; Escape or the close button dismisses it.
// PostureNodeDrawer is the connected one the pages mount once, for every entry point (spec XIX).
// The NEIS copy refresh (2026-10-30): the questions are the strategy steps' own (vocabulary STRATEGY_STEP); each
// choice is explained as an action (CHOICE_DEFINITION; Watch as "Low domestic priority", a deployment Watch
// below the bar for acceleration); a Partner step lists its best-placed partners; "step", never "node".

import { useEffect, useId, useRef, type ReactNode } from 'react';
import { Link } from 'react-router';
import { isProducingStage, stageDisplay } from '../lib/stages.ts';
import { Term } from '../components/Term.tsx';
import { CHOICE_DEFINITION, STRATEGY_STEP, WATCH_DEPLOYMENT_DEFINITION, WATCH_PROSE_LABEL } from '../content/vocabulary.ts';
import { POSTURE_TERMS } from './basis.tsx';
import type { PostureCell, PostureCore, Verb, Yard } from './contract.ts';
import { cellKey, cellKeyParts, countryCells, countryNames, pct } from './derive.ts';
import { ratioVsCurrentWords, type Family, type NodeDetail } from './detail.ts';
import {
  ALIGNMENT_CONFIDENCE_LABEL, ALIGNMENT_LABEL, ALIGNMENT_WORDS, alignmentFlagWords, basisWords, borderlineWords, choiceWord, deliveryWords, EVIDENCE_LABEL, flagWords, MOMENTUM_LABEL, MOMENTUM_UNAVAILABLE,
  pathwayEvidenceWords, traceCodeWords, traceNumberWords, traceTestWords, FENCE_LABEL, MARKET_PRIZE_LABEL, nodeFence,
  nodeFill, NOT_ASSESSED_BORDER, PATHWAY_LABEL, POLICY_SOURCE_LABEL, prizeLabel, RATIONALE_LABEL, RATIONALE_WORDS, VERB_COLOR, VERB_LABEL, VERB_OUTLINE, verbOf,
} from './grammar.ts';
import { postureMomentum } from './momentum.ts';
import { catchUpPositionWords, countryRankWords, manufacturingPriority, manufacturingPriorityWord, policyMomentumWords, protectionWords, watchReasons } from './sections.ts';
import { usePostureCore, usePostureDetail } from './store.ts';
import {
  capWords, FENCE_SCOPE_WORDS, largeYardOf, largeYardWords, layerUnits, mixedFill, nodeYard, rollupWords, signalWords, sivFamilies, variantWords, YARD_COLOR, YARD_LABEL, yardWords,
} from './yards.ts';

/** The five questions, in order (design D14). */
export const DRAWER_QUESTIONS = [
  { id: 'need', title: STRATEGY_STEP.need.name, ask: STRATEGY_STEP.need.question },
  { id: 'prize', title: STRATEGY_STEP.prize.name, ask: STRATEGY_STEP.prize.question },
  { id: 'position', title: STRATEGY_STEP.position.name, ask: STRATEGY_STEP.position.question },
  { id: 'choice', title: STRATEGY_STEP.choice.name, ask: STRATEGY_STEP.choice.question },
  { id: 'policy', title: STRATEGY_STEP.policy.name, ask: 'Does current policy match the choice?' },
] as const;

const NA = 'not assessed';
const pctOr = (v: number | null | undefined, digits = 0) => (v === null || v === undefined ? NA : pct(v, digits));
const usd = (v: number | undefined) => (v === undefined ? '—' : v >= 1e9 ? `$${(v / 1e9).toFixed(1)}bn` : v >= 1e6 ? `$${(v / 1e6).toFixed(0)}m` : `$${Math.round(v / 1e3)}k`);

function Row({ k, v, indent = false }: { k: ReactNode; v: ReactNode; indent?: boolean }) {
  return (
    <div className={`flex justify-between gap-3 py-1 text-sm ${indent ? 'pl-3' : ''}`}>
      <dt className="text-paper-mute">{k}</dt>
      <dd className="text-right tabular text-paper-dim">{v}</dd>
    </div>
  );
}

function Question({ q, children }: { q: (typeof DRAWER_QUESTIONS)[number]; children: ReactNode }) {
  const id = useId();
  return (
    <section className="mt-4 border-t border-white/10 pt-3" aria-labelledby={id} data-testid="drawer-question" data-question={q.id}>
      <h4 id={id} className="eyebrow">
        {q.title}
      </h4>
      <p className="mb-1 text-xs text-paper-mute">{q.ask}</p>
      {children}
    </section>
  );
}

function Pending({ status, error }: { status: DrawerDetailStatus; error?: string | null }) {
  if (status === 'loading') return <p className="text-xs text-paper-mute" role="status">Loading the details…</p>;
  if (status === 'error') return <p className="text-xs text-paper-mute" role="status">The details could not be loaded{error ? `: ${error}` : ''}.</p>;
  return null;
}

/** A family with its basis and scenario: "80% · national data · National commitments", or "not assessed". */
function familyText(f: Family | null | undefined, scenarios: Record<string, string>): string {
  if (!f || f.value === null) {
    const b = f?.basis && f.basis !== 'not_assessed' ? ` (${basisWords(f.basis)})` : '';
    return `${NA}${b}`;
  }
  // the climate-aligned alternative is a ratio to current policies, not a score: "+104% vs current policies" with its note
  const ratio = ratioVsCurrentWords(f);
  return [ratio ?? pct(f.value), ratio ? f.note : null, basisWords(f.basis), f.scenario ? (scenarios[f.scenario] ?? f.scenario) : null].filter(Boolean).join(' · ');
}

function NeedAnswer({ cell, d, core, names, status }: { cell: PostureCell; d: NodeDetail | null; core: PostureCore; names: Record<string, string>; status: DrawerDetailStatus }) {
  const sc = core.meta.scenario_labels;
  if (cell.needType === 'deployment') {
    const dn = d?.deployment;
    return (
      <>
        <dl>
          <Row k={<Term id={POSTURE_TERMS.deploymentNeed}>Deployment need</Term>} v={pctOr(cell.need)} />
          {dn && (
            <>
              <Row k="Demand adequacy" v={familyText(dn.demand, sc)} />
              <Row k="Target / climate gap" v={familyText(dn.policyGap, sc)} />
              <Row k="System need" v={familyText(dn.system, sc)} />
              {dn.alternative && <Row k="Climate-aligned (alternative scenario, not used in the choice)" v={familyText(dn.alternative, sc)} />}
              {dn.trend !== null && <Row k="Current deployment trend" v={typeof dn.trend === 'number' ? `${dn.trend >= 0 ? '+' : ''}${(dn.trend * 100).toFixed(0)}% a year` : dn.trend} />}
            </>
          )}
        </dl>
        <Pending status={status} />
        <p className="mt-1 text-xs text-paper-mute">
          At Deployment &amp; Use, Need means how urgently the domestic energy system needs more of the technology (expected demand, policy and climate gaps, system
          requirements), not supply-chain vulnerability. The scenarios are not interchangeable.
        </p>
      </>
    );
  }
  const s = d?.supplyChain;
  return (
    <>
      <dl>
        <Row k={<Term id={POSTURE_TERMS.supplyChainNeed}>Supply-chain vulnerability</Term>} v={pctOr(cell.need)} />
        {s && (
          <>
            <Row k="Import dependence" v={pctOr(s.importDependence)} />
            <Row k="Supplier concentration" v={pctOr(s.supplierConcentration)} />
            <Row k="Risky supplier exposure (China, opposite bloc)" v={pctOr(s.riskyExposure)} />
            <Row k="Lack of trusted alternatives" v={pctOr(s.trustedAlternatives)} />
            {s.topSupplier && <Row k="Largest supplier" v={`${names[s.topSupplier] ?? s.topSupplier}${s.topSupplierShare !== null ? ` (${pct(s.topSupplierShare)} of imports)` : ''}`} />}
            {s.coerciveTop && (
              <Row k="Largest potentially coercive supplier" v={`${names[s.coerciveTop] ?? s.coerciveTop}${s.coerciveTopShare !== null ? ` (${pct(s.coerciveTopShare)} of world supply)` : ''}`} />
            )}
            {s.coerciveImportShare !== null && <Row k="Coercive share of own imports" v={pct(s.coerciveImportShare)} />}
            {s.substitutability && <Row k="Substitutability" v={s.substitutability} />}
            {s.reconstitution && <Row k="Reconstitution time" v={s.reconstitution.replace(/^y1to3$/, '1 to 3 years').replace(/^gt3y$/, 'more than 3 years')} />}
            {s.inventories && <Row k="Inventories" v={s.inventories} />}
            {s.essentiality && <Row k="Systemic essentiality" v={s.essentiality} />}
            {s.operationalControl && <Row k="Operational control" v={s.operationalControl} />}
          </>
        )}
      </dl>
      <Pending status={status} />
      {cell.posture === 'N' && cell.need === null && <p className="mt-1 text-xs text-paper-mute">{d?.notAssessedReason ?? 'Supply-chain vulnerability is not measured for this step.'}</p>}
    </>
  );
}

function PrizeAnswer({ cell, d, core, cells, status }: { cell: PostureCell; d: NodeDetail | null; core: PostureCore; cells: PostureCell[]; status: DrawerDetailStatus }) {
  const label = prizeLabel(core.meta);
  const siv = core.meta.prize_measure === 'strategic_value';
  const term = siv ? POSTURE_TERMS.strategicValue : POSTURE_TERMS.industrialOpportunity;
  if (!isProducingStage(cell.stage)) {
    const m = manufacturingPriority(cell.tech, cells);
    return (
      <>
        <p className="text-sm text-paper-dim">
          <Term id={term}>{label}</Term> is an industrial prize, read at the producing stages: deployment priority is separate from domestic manufacturing priority.
        </p>
        <dl>
          <Row k="Domestic manufacturing priority" v={`${manufacturingPriorityWord(m)} (${m.words})`} />
        </dl>
      </>
    );
  }
  const p = d?.prize;
  const f = sivFamilies(core.meta, cell, d);
  // the market prize's components: under the market family where the prize is the strategic value
  const sub = siv;
  return (
    <>
      <dl data-testid="drawer-prize">
        <Row k={<Term id={term}>{label}</Term>} v={`${pctOr(cell.prize)} (high at ${pct(core.meta.thresholds.prize)} or more)${siv && f.cap ? '; capped: market only' : ''}`} />
        {siv && <Row k={<Term id={POSTURE_TERMS.industrialOpportunity}>Market prize ({MARKET_PRIZE_LABEL})</Term>} v={pctOr(f.market)} />}
        {p && (
          <>
            <Row indent={sub} k="Expected world demand growth" v={pctOr(p.nodePrize)} />
            <Row indent={sub} k="Investment momentum" v={pctOr(p.investmentMomentum)} />
            <Row indent={sub} k="Home-market growth (national data only)" v={pctOr(p.homeMarket)} />
            {p.maturity !== null && <Row indent={sub} k="Technology maturity (context, not scored)" v={typeof p.maturity === 'number' ? `TRL ${p.maturity}` : p.maturity} />}
            {p.outlook && <Row indent={sub} k="Market outlook" v={p.outlook} />}
            {p.basis && <Row indent={sub} k="Basis" v={basisWords(p.basis)} />}
          </>
        )}
        {siv && (
          <>
            <Row k="Industrial commons (process knowledge, workforce and suppliers, cross-sector spillover)" v={commonsWords(f.commons, f.commonsN)} />
            <Row k="Future option value (the leap tests: is a credible leap ahead in reach?)" v={optionWords(f.option, f.optionN)} />
          </>
        )}
      </dl>
      {p?.note && <p className="mt-1 text-xs text-paper-mute">{p.note}</p>}
      <Pending status={status} />
    </>
  );
}

/** "67 (3 of 3 attributes observed)"; not assessed when the core has no value. */
function commonsWords(v: number | null, n: number | null): string {
  if (v === null) return NA;
  return `${pct(v)}${n !== null ? ` (${n} of 3 attributes observed)` : ''}`;
}
/** "38 (3 of 4 leap tests observed)"; "none: not a leap candidate" for a 0 with no test observed. */
function optionWords(v: number | null, n: number | null): string {
  if (v === null) return NA;
  if (v === 0 && n === 0) return 'none: not a leap candidate';
  return `${pct(v)}${n !== null ? ` (${n} of 4 leap tests observed)` : ''}`;
}

function PositionAnswer({ cell, d, core, status }: { cell: PostureCell; d: NodeDetail | null; core: PostureCore; status: DrawerDetailStatus }) {
  const meta = core.meta;
  const tr = d?.trade;
  const mom = postureMomentum(tr?.momentum_raw, meta.momentum_band);
  return (
    <>
      <dl>
        <Row
          k={<Term id={POSTURE_TERMS.competitivePosition}>Competitive Position</Term>}
          v={
            cell.cp !== null && cell.cp >= meta.thresholds.cp && isProducingStage(cell.stage) && verbOf(cell) !== 'preserve' && verbOf(cell) !== 'secure'
              ? `${pctOr(cell.cp)}: a top-quarter score, but not a main producer or below the capability floor, so not a strong position`
              : `${pctOr(cell.cp)} (strong at ${pct(meta.thresholds.cp)} or more among the step's main producers)`
          }
        />
        <Row k="World supply share" v={pctOr(cell.share ?? d?.homeShare ?? null, 1)} />
        {(d?.cpFamilies ?? []).map((f) => (
          <Row key={f.id} k={f.label} v={pct(f.value)} />
        ))}
        {tr && (
          <>
            <Row k={`Trade position ${meta.benchmark_period ?? ''}`} v={`${pct(tr.benchmark_share, 1)} of world exports · RCA ${tr.benchmark_rca !== undefined ? tr.benchmark_rca.toFixed(2) : '—'}`} />
            <Row k="Benchmark exports (avg.)" v={usd(tr.benchmark_exports)} />
            {mom ? (
              <Row
                k={`Trade momentum ${meta.momentum_period ?? ''}`}
                v={`${MOMENTUM_LABEL[mom]} ${tr.momentum_raw !== undefined ? `(${((Math.exp(tr.momentum_raw) - 1) * 100).toFixed(0)}%)` : ''} vs ${meta.momentum_previous_period ?? 'the year before'}`}
              />
            ) : (
              <Row k="Trade momentum" v={<span className="text-paper-mute">{MOMENTUM_UNAVAILABLE}</span>} />
            )}
          </>
        )}
        {!tr && cell.momentum && <Row k="Trade momentum" v={MOMENTUM_LABEL[cell.momentum]} />}
        {cell.evidence && <Row k="Evidence" v={EVIDENCE_LABEL[cell.evidence]} />}
      </dl>
      {tr && meta.momentum_through && <p className="mt-1 text-xs text-paper-mute">Data through {meta.momentum_through}.</p>}
      <Pending status={status} />
    </>
  );
}

function ChoiceAnswer({
  cell, d, core, cells, names, status, focusLayer,
}: { cell: PostureCell; d: NodeDetail | null; core: PostureCore; cells: PostureCell[]; names: Record<string, string>; status: DrawerDetailStatus; focusLayer?: string | null }) {
  const v = verbOf(cell);
  const t = core.meta.thresholds;
  const design = d?.variant || cell.variant ? core.design[`${d?.variant ?? cell.variant}|${d?.delivery ?? ''}`] : undefined;
  // the page's fence (the user's mapping, grammar nodeFence); the policy design's own trade-protection level
  // (R config/posture_policy_design.csv fence_level) is a different thing and is labelled as such below
  const fence = nodeFence(v, cell.stage);
  return (
    <>
      <p className="text-sm text-paper">
        <strong>{v ? VERB_LABEL[v] : choiceWord(cell)}</strong>
        {cell.posture === 'S' && cell.rationale ? ` · ${RATIONALE_LABEL[cell.rationale]}` : ''}
      </p>
      {cell.posture === 'N' && (
        <p className="mt-1 text-sm text-paper-dim">
          Not assessed: {d?.notAssessedReason ?? (cell.needType === 'deployment' ? 'deployment need is not measured.' : 'need or prize is not measured.')} Missing data are shown as
          uncertainty, never as Watch.
        </p>
      )}
      {cell.posture === 'S' && cell.rationale && (
        <p className="mt-1 text-sm text-paper-dim">
          {RATIONALE_WORDS[cell.rationale]}. {CHOICE_DEFINITION.secure}
          {d?.delivery ? ` Delivery here: ${deliveryWords(d.delivery)}.` : ''}
          {d && d.routes.length > 1 ? ` Routes met: ${d.routes.map((r) => RATIONALE_LABEL[r as keyof typeof RATIONALE_LABEL] ?? r).join(', ')}.` : ''}
        </p>
      )}
      {v === 'preserve' && (
        <p className="mt-1 text-sm text-paper-dim">{CHOICE_DEFINITION.preserve} Its position is in the top quarter of the step&apos;s main producers.</p>
      )}
      {v === 'catch_up' && (
        <p className="mt-1 text-sm text-paper-dim">
          A large prize, {catchUpPositionWords(cell, t, 'sentence')}, and a credible pathway: a real capability to build from (an anchor, corroborated by a signal of another
          kind). Catch Up is never a greenfield ambition.
        </p>
      )}
      {v === 'partner' && (
        <div className="mt-1 text-sm text-paper-dim">
          <p>{CHOICE_DEFINITION.partner}</p>
          {cell.partnerGap || d?.partnerGap ? (
            <p className="mt-1 text-signal">No credible allied solution yet: no trusted supplier can cover it.</p>
          ) : d?.partners.length ? (
            <ol className="mt-1 space-y-1" aria-label="Best-placed partners">
              {d.partners.map((p, i) => (
                <li key={p.iso3} className="flex justify-between gap-2">
                  <span className="text-paper">
                    {i + 1}. {names[p.iso3] ?? p.iso3}
                  </span>
                  <span className="tabular text-xs text-paper-mute">
                    holds {pct(p.capability)} · position {pct(p.cp)} · relationship {p.relationship !== undefined ? p.relationship.toFixed(2) : '—'}
                  </span>
                </li>
              ))}
            </ol>
          ) : cell.partner1 ? (
            <p className="mt-1">Best-placed partner: {names[cell.partner1] ?? cell.partner1}</p>
          ) : null}
          {d?.partnerReason && <p className="mt-1 text-xs text-paper-mute">{d.partnerReason}</p>}
        </div>
      )}
      {v === 'accelerate' && (
        <p className="mt-1 text-sm text-paper-dim">
          {CHOICE_DEFINITION.accelerate} Domestic manufacturing priority, read at the producing stages, is{' '}
          {manufacturingPriorityWord(manufacturingPriority(cell.tech, cells))} ({manufacturingPriority(cell.tech, cells).words}).
        </p>
      )}
      {v === 'watch' && (
        <p className="mt-1 text-sm text-paper-dim">
          {WATCH_PROSE_LABEL}
          {watchReasons(cell, t, d).length ? ` (${watchReasons(cell, t, d).join('; ').toLowerCase()})` : ''}: a deliberate choice, not a failure.{' '}
          {cell.needType === 'deployment' ? WATCH_DEPLOYMENT_DEFINITION : CHOICE_DEFINITION.watch}
        </p>
      )}
      <dl className="mt-1">
        {fence && <Row k="Fence" v={<span data-testid="drawer-fence">{FENCE_LABEL[fence]}</span>} />}
        {cell.needType === 'supply_chain' && cell.pathway && (
          <Row k={<Term id={POSTURE_TERMS.pathway}>Credible pathway</Term>} v={`${PATHWAY_LABEL[cell.pathway]}${d?.pathway.basis.length ? ` (${pathwayEvidenceWords(d.pathway.basis)})` : ''}`} />
        )}
        {(d?.variant ?? cell.variant) && <Row k="Kind of choice" v={<span data-testid="drawer-variant">{cap1(variantWords(d?.variant ?? cell.variant) ?? '')}</span>} />}
        {d?.delivery && <Row k={<Term id={POSTURE_TERMS.deliveryMode}>Delivery mode</Term>} v={deliveryWords(d.delivery)} />}
        {cell.confidence && <Row k="Confidence" v={cell.confidence} />}
        {cell.borderline && <Row k="Borderline on" v={borderlineWords(cell.borderline)} />}
        {d?.pivotal && <Row k="Pivotal inputs (would change the choice)" v={borderlineWords(d.pivotal.replace(/\|/g, ';'))} />}
        {d?.choicePossible && <Row k="Choices the evidence allows" v={possibleWords(d.choicePossible)} />}
        {cell.flag && <Row k={<Term id={POSTURE_TERMS.consistencyFlag}>Consistency review</Term>} v={flagWords(cell.flag)} />}
      </dl>
      {d?.mitigation && (
        <p className="mt-1 text-xs text-paper-mute">
          <strong className="text-paper-dim">Mitigation:</strong> {d.mitigation}
        </p>
      )}
      {design?.fence && (
        <p className="mt-1 text-xs text-paper-mute" data-testid="drawer-trade-protection">
          <strong className="text-paper-dim" title="The trade protection the policy design pairs with this kind of choice: not the step's fence above">
            Fence (policy design):
          </strong>{' '}
          {design.fence}
        </p>
      )}
      {design?.gate && (
        <p className="mt-1 text-xs text-paper-mute">
          <strong className="text-paper-dim">Gate:</strong> {design.gate}
        </p>
      )}
      {design?.build && (
        <p className="mt-1 text-xs text-paper-mute">
          <strong className="text-paper-dim">Build:</strong> {design.build}
        </p>
      )}
      {d && d.trace.length > 0 && (
        <details className="mt-2 text-xs text-paper-mute" data-testid="drawer-trace">
          <summary className="cursor-pointer text-paper-dim">How the choice was made</summary>
          <ol className="mt-1 space-y-0.5">
            {d.trace.map((s, i) => (
              <li key={i}>
                <span className="text-paper-dim">{traceTestWords(s.test)}</span>
                {s.value !== null ? `: ${typeof s.value === 'number' ? traceNumberWords(s.test, s.value, (x) => pct(x)) : traceCodeWords(s.value)}` : ''}
                {s.threshold !== null ? ` (bar ${typeof s.threshold === 'number' ? traceNumberWords(s.test, s.threshold, (x) => pct(x)) : s.threshold})` : ''} → {traceCodeWords(s.result)}
              </li>
            ))}
          </ol>
        </details>
      )}
      {d?.reason && <p className="mt-1 text-xs text-paper-mute">{d.reason}</p>}
      <Pending status={status} />
      <YardsPart cell={cell} d={d} core={core} focusLayer={focusLayer} />
    </>
  );
}

const cap1 = (s: string): string => s.charAt(0).toUpperCase() + s.slice(1);

/** "Catch Up or Watch" from R's "catch_up|watch" (an unknown word loses its underscores). */
function possibleWords(v: string): string {
  const w = v.split('|').map((x) => VERB_LABEL[x.trim() as Verb] ?? x.trim().replace(/_/g, ' '));
  return w.length > 1 ? `${w.slice(0, -1).join(', ')} or ${w[w.length - 1]}` : w[0];
}

/** A yard as a chip: its fence colour and its name ("Small yard"). */
export function YardChip({ yard }: { yard: Yard | null }) {
  if (!yard) return <span className="text-paper-mute">no yard (not assessed)</span>;
  return (
    <span className="inline-flex items-center gap-1 whitespace-nowrap rounded-sm border border-white/15 px-1 text-[11px] text-paper-dim" data-yard={yard}>
      <span
        aria-hidden="true"
        className="inline-block h-2 w-2 shrink-0 rounded-[2px]"
        style={{ background: YARD_COLOR[yard], border: yard === 'large' ? `1px solid ${VERB_OUTLINE.watch}` : undefined }}
      />
      {YARD_LABEL[yard]}
    </span>
  );
}

/**
 * The Yards part of the CHOICE question: the node's own yard (from its choice) and its roll-up with its
 * layers; each child layer with its yard, choice and one-line reason (yards.ts layerUnits); the fence scope
 * of a Secure layer; the node's large-yard case (the signals that hold, those not observed, the count against
 * the bar) and the market-only cap. `focusLayer` (?layer=) is scrolled to and focused once its row is in.
 */
function YardsPart({ cell, d, core, focusLayer }: { cell: PostureCell; d: NodeDetail | null; core: PostureCore; focusLayer?: string | null }) {
  const id = useId();
  const focusRef = useRef<HTMLLIElement | null>(null);
  const own = nodeYard(cell);
  const units = layerUnits(core.meta, d);
  const ly = isProducingStage(cell.stage) ? largeYardOf(core.meta, cell.tech, cell.stage, d) : null;
  const f = sivFamilies(core.meta, cell, d);
  const hit = focusLayer ? units.some((u) => u.layerId === focusLayer) : false;
  useEffect(() => {
    if (!hit || !focusRef.current) return;
    focusRef.current.scrollIntoView?.({ block: 'center' });
    focusRef.current.focus();
  }, [hit, focusLayer]);
  if (!own && !cell.yards && !units.length && !ly) return null;
  const roll = rollupWords(cell);
  const split = mixedFill(cell);
  return (
    <section className="mt-3 rounded-md border border-white/10 p-2.5" aria-labelledby={id} data-testid="drawer-yards">
      <h5 id={id} className="text-xs font-medium uppercase tracking-wide text-paper-dim">
        Yards
      </h5>
      <dl>
        <Row k="This step's own yard" v={own ? <span data-testid="drawer-own-yard">{yardWords(own)}</span> : NA} />
        {roll && (
          <Row
            k="With its layers"
            v={
              <span className="inline-flex items-center gap-1.5" data-testid="drawer-yard-rollup" data-yards={cell.yards ?? undefined}>
                {split && <span aria-hidden="true" className="inline-block h-3 w-3 shrink-0 rounded-sm" style={{ background: split }} />}
                {roll}
              </span>
            }
          />
        )}
      </dl>
      {units.length > 0 && (
        <>
          <p className="mt-1 text-xs text-paper-mute">The layers of a step can each need a different fence, so each has its own yard and choice:</p>
          <ul className="mt-1 space-y-1.5" aria-label="Layers of this step">
            {units.map((u) => {
              const on = focusLayer === u.layerId;
              return (
                <li
                  key={u.layerId}
                  ref={on ? focusRef : undefined}
                  tabIndex={on ? -1 : undefined}
                  className={`rounded-md px-1.5 py-1 text-xs outline-none ${on ? 'bg-white/10 ring-1 ring-white/50' : ''}`}
                  data-layer={u.layerId}
                  data-focused={on ? 'true' : undefined}
                  data-testid="drawer-layer"
                >
                  <span className="flex flex-wrap items-center gap-x-2 gap-y-0.5">
                    <YardChip yard={u.yard} />
                    <span className="inline-flex items-center gap-1 text-paper-dim" data-verb={u.verb ?? 'not_assessed'}>
                      {u.verb && (
                        <span
                          aria-hidden="true"
                          className="inline-block h-2.5 w-2.5 rounded-[2px]"
                          style={{ background: VERB_COLOR[u.verb], border: u.verb === 'watch' ? `1px solid ${VERB_OUTLINE.watch}` : undefined }}
                        />
                      )}
                      {u.verb ? VERB_LABEL[u.verb] : 'Not assessed'}
                    </span>
                    <span className="text-paper">{u.label}</span>
                  </span>
                  <span className="block text-paper-mute" data-testid="drawer-layer-reason">
                    {u.reason}
                  </span>
                </li>
              );
            })}
          </ul>
        </>
      )}
      {d?.yardFenceScope && (
        <p className="mt-1.5 text-xs text-paper-dim" data-testid="drawer-fence-scope">
          {FENCE_SCOPE_WORDS[d.yardFenceScope]}
        </p>
      )}
      {ly && (
        <div className="mt-1.5 text-xs text-paper-mute" data-testid="drawer-large-yard">
          <p>
            <strong className="font-medium text-paper-dim">Large-yard case{ly.own ? ' (this economy)' : ''}:</strong> {largeYardWords(ly)}
          </p>
          {ly.signals.length > 0 && <p>Signals that hold: {signalWords(ly.signals)}.</p>}
          {ly.unknown.length > 0 && <p>Not observed: {signalWords(ly.unknown)}.</p>}
        </div>
      )}
      {f.cap && core.meta.prize_measure === 'strategic_value' && (
        <p className="mt-1.5 text-xs text-paper-mute" data-testid="drawer-yard-cap">
          {capWords(f.cap)}.
        </p>
      )}
    </section>
  );
}

function PolicyAnswer({ cell, d, status }: { cell: PostureCell; d: NodeDetail | null; status: DrawerDetailStatus }) {
  if (cell.posture === 'N') return <p className="text-sm text-paper-dim">Not assessed, so there is no recommendation to compare policy with.</p>;
  const p = d?.policy;
  const al = cell.alignment ?? p?.alignment ?? null;
  const alc = cell.alignmentConfidence ?? p?.alignmentConfidence ?? null;
  return (
    <>
      <dl>
        <Row k={<Term id={POSTURE_TERMS.policyAlignment}>Alignment</Term>} v={al ? ALIGNMENT_LABEL[al] : NA} />
        {alc && <Row k="Alignment confidence" v={ALIGNMENT_CONFIDENCE_LABEL[alc]} />}
        {p && (
          <>
            {/* within-country percentile ranks, never shares or changes */}
            <Row k="Observed policy intensity" v={countryRankWords(p.priority)} />
            <Row k="Support" v={countryRankWords(p.support)} />
            <Row k="Protection or localization" v={protectionWords(p.protection, p.protectionRecorded)} />
            <Row k="Policy momentum (recent activity)" v={policyMomentumWords(p.momentum)} />
            {p.sources.length > 0 && <Row k="Sources" v={p.sources.map((s) => POLICY_SOURCE_LABEL[s] ?? s).join(', ')} />}
            {(p.flag ?? cell.alignmentFlag) && <Row k="Note" v={alignmentFlagWords(p.flag ?? cell.alignmentFlag)} />}
          </>
        )}
      </dl>
      {al && <p className="mt-1 text-xs text-paper-mute">{ALIGNMENT_WORDS[al][0].toUpperCase() + ALIGNMENT_WORDS[al].slice(1)}.</p>}
      <p className="mt-1 text-xs text-paper-mute">Policy is compared with the choice afterwards; it never feeds the recommendation.</p>
      <Pending status={status} />
    </>
  );
}

export type DrawerDetailStatus = 'idle' | 'loading' | 'ready' | 'error';

export interface CellDrawerProps {
  /** the node's core cell (headline values at once) */
  cell: PostureCell;
  /** the node's detail (components), null until the document loads */
  detail: NodeDetail | null;
  detailStatus: DrawerDetailStatus;
  detailError?: string | null;
  /** the economy's name */
  country: string;
  core: PostureCore;
  /** every node of the economy (Accelerate and Deployment read the technology's producing stages) */
  cells: PostureCell[];
  names: Record<string, string>;
  onClose: () => void;
  /** the node's Sector page, or null / undefined for no link */
  sectorHref?: string | null;
  /** a layer of the node to scroll to and focus in the Yards part (?layer=); null for the node itself */
  focusLayer?: string | null;
}

export function CellDrawer({ cell, detail, detailStatus, detailError, country, core, cells, names, onClose, sectorHref, focusLayer }: CellDrawerProps) {
  const ref = useRef<HTMLDivElement>(null);
  const key = cellKey(cell.tech, cell.stage);
  useEffect(() => {
    const k = (e: KeyboardEvent) => e.key === 'Escape' && onClose();
    window.addEventListener('keydown', k);
    // the dialog takes focus unless a layer row inside it already has it (the Yards part focuses ?layer=)
    if (!ref.current?.contains(document.activeElement)) ref.current?.focus();
    return () => window.removeEventListener('keydown', k);
  }, [onClose, key]);
  const d = detail;
  const status = detailStatus === 'ready' && !d ? 'ready' : detailStatus;
  const na = cell.posture === 'N';
  return (
    <div
      ref={ref}
      tabIndex={-1}
      role="dialog"
      aria-label={`${cell.tech} ${stageDisplay(cell.stage)}: five questions`}
      className="fixed inset-x-0 bottom-0 z-40 max-h-[75vh] overflow-y-auto rounded-t-xl border border-white/10 bg-ink-850 p-5 shadow-2xl outline-none sm:inset-x-auto sm:bottom-auto sm:right-4 sm:top-20 sm:max-h-[calc(100vh-6rem)] sm:w-[27rem] sm:rounded-xl"
      data-testid="cell-drawer"
    >
      <div className="flex items-start justify-between gap-3">
        <div>
          <p className="eyebrow">{country}</p>
          <h3 className="text-lg font-medium text-paper">
            {cell.tech} · {stageDisplay(cell.stage)}
          </h3>
          <p className="mt-1 inline-flex items-center gap-2 text-sm text-paper-dim">
            <span
              className="inline-block h-3 w-3 rounded-sm"
              style={{ background: nodeFill(cell), border: na ? NOT_ASSESSED_BORDER : verbOf(cell) === 'watch' ? `1px solid ${VERB_OUTLINE.watch}` : undefined }}
              data-testid="cell-drawer-chip"
              data-verb={verbOf(cell) ?? 'not_assessed'}
              aria-hidden="true"
            />
            {choiceWord(cell)}
            {cell.confidence ? <span className="text-paper-mute"> · {cell.confidence} confidence</span> : null}
          </p>
          {sectorHref && (
            <p className="mt-1 text-sm">
              <Link className="link" to={sectorHref} data-testid="cell-drawer-sector-link">
                {cell.tech} — {stageDisplay(cell.stage)} in Sectors &rarr;
              </Link>
            </p>
          )}
        </div>
        <button type="button" className="btn btn-ghost btn-sm" onClick={onClose} aria-label="Close the detail">
          ✕
        </button>
      </div>
      {detailStatus === 'error' && <p className="mt-2 text-xs text-paper-mute">The detail could not be loaded{detailError ? `: ${detailError}` : ''}.</p>}
      <Question q={DRAWER_QUESTIONS[0]}>
        <NeedAnswer cell={cell} d={d} core={core} names={names} status={status} />
      </Question>
      <Question q={DRAWER_QUESTIONS[1]}>
        <PrizeAnswer cell={cell} d={d} core={core} cells={cells} status={status} />
      </Question>
      <Question q={DRAWER_QUESTIONS[2]}>
        <PositionAnswer cell={cell} d={d} core={core} status={status} />
      </Question>
      <Question q={DRAWER_QUESTIONS[3]}>
        <ChoiceAnswer cell={cell} d={d} core={core} cells={cells} names={names} status={status} focusLayer={focusLayer} />
      </Question>
      <Question q={DRAWER_QUESTIONS[4]}>
        <PolicyAnswer cell={cell} d={d} status={status} />
      </Question>
    </div>
  );
}

export interface PostureNodeDrawerProps {
  iso3: string;
  /** the economy's name for the eyebrow (default: the posture data's) */
  name?: string;
  /** the selected node "Tech|Stage" (null: closed) */
  selected: string | null;
  onClose: () => void;
  /** the node's Sector page (posture/ knows no Sector routes) */
  sectorHref?: (tech: string, stage: string) => string | null;
  /** a layer of the selected node to focus (?layer=), null for none */
  layer?: string | null;
}

/**
 * The drawer a page mounts once (spec XIX): it loads the core and, on first open, the economy's detail
 * document, and shows the selected node. Nothing while closed, while the core loads, or for a key the data
 * does not know; a node the economy has no posture for says so.
 */
export function PostureNodeDrawer({ iso3, name, selected, onClose, sectorHref, layer }: PostureNodeDrawerProps) {
  const core = usePostureCore();
  const detail = usePostureDetail(selected ? iso3 : null);
  if (!selected || !core.data) return null;
  const c = core.data;
  const parts = cellKeyParts(selected, c.meta);
  if (!parts) return null;
  const cells = countryCells(c, iso3);
  const cell = cells.find((x) => cellKey(x.tech, x.stage) === selected);
  const names = countryNames(c);
  if (!cell) {
    return (
      <div role="dialog" aria-label={`${parts.tech} ${stageDisplay(parts.stage)}: not classified`} className="fixed inset-x-0 bottom-0 z-40 rounded-t-xl border border-white/10 bg-ink-850 p-5 sm:inset-x-auto sm:bottom-auto sm:right-4 sm:top-20 sm:w-[27rem] sm:rounded-xl" data-testid="cell-drawer">
        <div className="flex items-start justify-between gap-3">
          <p className="text-sm text-paper-dim">
            {parts.tech} · {stageDisplay(parts.stage)} is not on the strategy map of {name ?? names[iso3] ?? iso3}: there is not enough comparable evidence to make a strategic call.
          </p>
          <button type="button" className="btn btn-ghost btn-sm" onClick={onClose} aria-label="Close the detail">
            ✕
          </button>
        </div>
      </div>
    );
  }
  const d = detail.data?.cells.find((x) => x.key === selected) ?? null;
  return (
    <CellDrawer
      cell={cell}
      detail={d}
      detailStatus={detail.status}
      detailError={detail.error}
      country={name ?? detail.data?.country ?? names[iso3] ?? iso3}
      core={c}
      cells={cells}
      names={names}
      onClose={onClose}
      sectorHref={sectorHref ? sectorHref(cell.tech, cell.stage) : null}
      focusLayer={layer ?? null}
    />
  );
}
