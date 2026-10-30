// The node drawer (spec XI.8; design D14): one country x technology x six-stage node, answered in exactly
// five questions, in order: NEED, PRIZE, POSITION, CHOICE, POLICY ALIGNMENT. Each gives its numbers, the
// components behind them, their basis and "not assessed" where a value is missing (never a zero).
// - NEED: supply-chain need at the producing stages (import dependence, supplier concentration, risky
//   supplier exposure, trusted alternatives, reconstitution and substitutability); at Deployment & Use the
//   deployment need families instead (demand adequacy, target / climate gap, system need, each with its
//   basis and scenario; a climate-aligned value only as a labelled alternative), never supply-chain need.
// - PRIZE: Industrial Opportunity and its components (at Deployment: why the prize is read upstream, and the
//   technology's domestic manufacturing priority beside its deployment priority).
// - POSITION: Competitive Position, its families, world supply share and the trade evidence with momentum.
// - CHOICE: the choice, rationale, delivery mode, mitigation and design text, pathway, partners, the compact
//   trace, borderline, confidence and the consistency flag.
// - POLICY ALIGNMENT: observed policy intensity, support and protection, momentum (each a percentile rank
//   within the country, never a share or a change; protection "none recorded" where the records hold none),
//   sources, the alignment and its confidence; observed policy is a comparator, never an input.
// The core cell gives the headline values at once; the detail document (fetched on first open) the rest.
// A side panel on wide screens, a bottom sheet on phones; Escape or the close button dismisses it.
// PostureNodeDrawer is the connected one the pages mount once, for every entry point (spec XIX).

import { useEffect, useId, useRef, type ReactNode } from 'react';
import { Link } from 'react-router';
import { isProducingStage, stageDisplay } from '../lib/stages.ts';
import { Term } from '../components/Term.tsx';
import { POSTURE_TERMS } from './basis.tsx';
import type { PostureCell, PostureCore } from './contract.ts';
import { cellKey, cellKeyParts, countryCells, countryNames, pct } from './derive.ts';
import { ratioVsCurrentWords, type Family, type NodeDetail } from './detail.ts';
import {
  ALIGNMENT_CONFIDENCE_LABEL, ALIGNMENT_LABEL, ALIGNMENT_WORDS, alignmentFlagWords, basisWords, borderlineWords, choiceWord, deliveryWords, EVIDENCE_LABEL, flagWords, MOMENTUM_LABEL, MOMENTUM_UNAVAILABLE,
  pathwayEvidenceWords, traceCodeWords, traceTestWords,
  NOT_ASSESSED_BORDER, NOT_ASSESSED_HATCH, PATHWAY_LABEL, POLICY_SOURCE_LABEL, POSTURE_COLOR, RATIONALE_LABEL, RATIONALE_WORDS, verbOf,
} from './grammar.ts';
import { postureMomentum } from './momentum.ts';
import { catchUpPositionWords, countryRankWords, manufacturingPriority, manufacturingPriorityWord, policyMomentumWords, protectionWords, watchReasons } from './sections.ts';
import { usePostureCore, usePostureDetail } from './store.ts';

/** The five questions, in order (design D14). */
export const DRAWER_QUESTIONS = [
  { id: 'need', title: 'Need', ask: 'What problem does the country need to solve here?' },
  { id: 'prize', title: 'Prize', ask: 'Is there an industrial prize worth pursuing?' },
  { id: 'position', title: 'Position', ask: 'How strong is the country here today?' },
  { id: 'choice', title: 'Choice', ask: 'What should it do?' },
  { id: 'policy', title: 'Policy alignment', ask: 'Does current policy match the choice?' },
] as const;

const NA = 'not assessed';
const pctOr = (v: number | null | undefined, digits = 0) => (v === null || v === undefined ? NA : pct(v, digits));
const usd = (v: number | undefined) => (v === undefined ? '—' : v >= 1e9 ? `$${(v / 1e9).toFixed(1)}bn` : v >= 1e6 ? `$${(v / 1e6).toFixed(0)}m` : `$${Math.round(v / 1e3)}k`);

function Row({ k, v }: { k: ReactNode; v: ReactNode }) {
  return (
    <div className="flex justify-between gap-3 py-1 text-sm">
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
  if (status === 'loading') return <p className="text-xs text-paper-mute" role="status">Loading the components…</p>;
  if (status === 'error') return <p className="text-xs text-paper-mute" role="status">The components could not be loaded{error ? `: ${error}` : ''}.</p>;
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
          At Deployment &amp; Use, Need is how much more of the technology the energy system needs (expected demand, policy and climate gaps, system requirements), not supply-chain
          vulnerability. The scenarios are not interchangeable.
        </p>
      </>
    );
  }
  const s = d?.supplyChain;
  return (
    <>
      <dl>
        <Row k={<Term id={POSTURE_TERMS.supplyChainNeed}>Supply-chain need</Term>} v={pctOr(cell.need)} />
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
      {cell.posture === 'N' && cell.need === null && <p className="mt-1 text-xs text-paper-mute">{d?.notAssessedReason ?? 'Supply-chain need is not measured for this node.'}</p>}
    </>
  );
}

function PrizeAnswer({ cell, d, core, cells, status }: { cell: PostureCell; d: NodeDetail | null; core: PostureCore; cells: PostureCell[]; status: DrawerDetailStatus }) {
  if (!isProducingStage(cell.stage)) {
    const m = manufacturingPriority(cell.tech, cells);
    return (
      <>
        <p className="text-sm text-paper-dim">
          <Term id={POSTURE_TERMS.industrialOpportunity}>Industrial Opportunity</Term> is a manufacturing prize, read at the producing stages: deployment priority is separate from
          domestic manufacturing priority.
        </p>
        <dl>
          <Row k="Domestic manufacturing priority" v={`${manufacturingPriorityWord(m)} (${m.words})`} />
        </dl>
      </>
    );
  }
  const p = d?.prize;
  return (
    <>
      <dl>
        <Row k={<Term id={POSTURE_TERMS.industrialOpportunity}>Industrial Opportunity</Term>} v={`${pctOr(cell.prize)} (high at ${pct(core.meta.thresholds.prize)} or more)`} />
        {p && (
          <>
            <Row k="Node prize (expected world demand growth)" v={pctOr(p.nodePrize)} />
            <Row k="Investment momentum" v={pctOr(p.investmentMomentum)} />
            <Row k="Home-market growth (national data only)" v={pctOr(p.homeMarket)} />
            {p.maturity !== null && <Row k="Technology maturity (context, not scored)" v={typeof p.maturity === 'number' ? `TRL ${p.maturity}` : p.maturity} />}
            {p.outlook && <Row k="Market outlook" v={p.outlook} />}
            {p.basis && <Row k="Basis" v={basisWords(p.basis)} />}
          </>
        )}
      </dl>
      {p?.note && <p className="mt-1 text-xs text-paper-mute">{p.note}</p>}
      <Pending status={status} />
    </>
  );
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
              ? `${pctOr(cell.cp)}: a top-quarter score, but not a contender or below the capability floor, so not a strong position`
              : `${pctOr(cell.cp)} (strong at ${pct(meta.thresholds.cp)} or more among the node's contenders)`
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

function ChoiceAnswer({ cell, d, core, cells, names, status }: { cell: PostureCell; d: NodeDetail | null; core: PostureCore; cells: PostureCell[]; names: Record<string, string>; status: DrawerDetailStatus }) {
  const v = verbOf(cell);
  const t = core.meta.thresholds;
  const design = d?.variant || cell.variant ? core.design[`${d?.variant ?? cell.variant}|${d?.delivery ?? ''}`] : undefined;
  return (
    <>
      <p className="text-sm text-paper">
        <strong>{choiceWord(cell)}</strong>
        {cell.posture === 'S' && cell.rationale ? ` · ${RATIONALE_LABEL[cell.rationale]}` : ''}
      </p>
      {cell.posture === 'N' && (
        <p className="mt-1 text-sm text-paper-dim">
          Not assessed: {d?.notAssessedReason ?? (cell.needType === 'deployment' ? 'deployment need is not measured.' : 'need or prize is not measured.')} It is counted apart, never as
          Watch.
        </p>
      )}
      {cell.posture === 'S' && cell.rationale && (
        <p className="mt-1 text-sm text-paper-dim">
          {RATIONALE_WORDS[cell.rationale]}. Secure means an acute vulnerability needs deliberate mitigation, not reshoring everything
          {d?.delivery ? `: delivery is ${deliveryWords(d.delivery)}` : ''}.
          {d && d.routes.length > 1 ? ` Routes met: ${d.routes.map((r) => RATIONALE_LABEL[r as keyof typeof RATIONALE_LABEL] ?? r).join(', ')}.` : ''}
        </p>
      )}
      {v === 'preserve' && <p className="mt-1 text-sm text-paper-dim">A strong position (top quarter of the node&apos;s contenders) worth maintaining and extending.</p>}
      {v === 'catch_up' && <p className="mt-1 text-sm text-paper-dim">A large prize, {catchUpPositionWords(cell, t, 'sentence')} and a credible route to build capability.</p>}
      {v === 'partner' && (
        <div className="mt-1 text-sm text-paper-dim">
          <p>The capability matters, but allies and diversification are the better answer than a domestic build.</p>
          {cell.partnerGap || d?.partnerGap ? (
            <p className="mt-1 text-signal">No credible allied solution yet: no trusted supplier can cover it.</p>
          ) : d?.partners.length ? (
            <ol className="mt-1 space-y-1">
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
            <p className="mt-1">First trusted partner: {names[cell.partner1] ?? cell.partner1}</p>
          ) : null}
          {d?.partnerReason && <p className="mt-1 text-xs text-paper-mute">{d.partnerReason}</p>}
        </div>
      )}
      {v === 'accelerate' && (
        <p className="mt-1 text-sm text-paper-dim">
          Deployment priority: high. This says nothing about domestic manufacturing: domestic manufacturing priority is {manufacturingPriorityWord(manufacturingPriority(cell.tech, cells))} (
          {manufacturingPriority(cell.tech, cells).words}).
        </p>
      )}
      {v === 'watch' && (
        <p className="mt-1 text-sm text-paper-dim">
          Domestic industrial priority: low{watchReasons(cell, t, d).length ? ` (${watchReasons(cell, t, d).join('; ').toLowerCase()})` : ''}. This is a deliberate choice, not a
          failure.
        </p>
      )}
      <dl className="mt-1">
        {cell.needType === 'supply_chain' && cell.pathway && (
          <Row k={<Term id={POSTURE_TERMS.pathway}>Credible pathway</Term>} v={`${PATHWAY_LABEL[cell.pathway]}${d?.pathway.basis.length ? ` (${pathwayEvidenceWords(d.pathway.basis)})` : ''}`} />
        )}
        {d?.delivery && <Row k={<Term id={POSTURE_TERMS.deliveryMode}>Delivery mode</Term>} v={deliveryWords(d.delivery)} />}
        {cell.confidence && <Row k="Confidence" v={cell.confidence} />}
        {cell.borderline && <Row k="Borderline on" v={borderlineWords(cell.borderline)} />}
        {cell.flag && <Row k={<Term id={POSTURE_TERMS.consistencyFlag}>Consistency review</Term>} v={flagWords(cell.flag)} />}
      </dl>
      {d?.mitigation && (
        <p className="mt-1 text-xs text-paper-mute">
          <strong className="text-paper-dim">Mitigation:</strong> {d.mitigation}
        </p>
      )}
      {design?.fence && (
        <p className="mt-1 text-xs text-paper-mute">
          <strong className="text-paper-dim">Fence:</strong> {design.fence}
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
                {s.value !== null ? `: ${typeof s.value === 'number' ? pct(s.value) : traceCodeWords(s.value)}` : ''}
                {s.threshold !== null ? ` (bar ${typeof s.threshold === 'number' ? pct(s.threshold) : s.threshold})` : ''} → {traceCodeWords(s.result)}
              </li>
            ))}
          </ol>
        </details>
      )}
      {d?.reason && <p className="mt-1 text-xs text-paper-mute">{d.reason}</p>}
      <Pending status={status} />
    </>
  );
}

function PolicyAnswer({ cell, d, status }: { cell: PostureCell; d: NodeDetail | null; status: DrawerDetailStatus }) {
  if (cell.posture === 'N') return <p className="text-sm text-paper-dim">No recommendation, so no alignment to judge.</p>;
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
            <Row k="Protection or localisation" v={protectionWords(p.protection, p.protectionRecorded)} />
            <Row k="Policy momentum (recent activity)" v={policyMomentumWords(p.momentum)} />
            {p.sources.length > 0 && <Row k="Sources" v={p.sources.map((s) => POLICY_SOURCE_LABEL[s] ?? s).join(', ')} />}
            {(p.flag ?? cell.alignmentFlag) && <Row k="Note" v={alignmentFlagWords(p.flag ?? cell.alignmentFlag)} />}
          </>
        )}
      </dl>
      {al && <p className="mt-1 text-xs text-paper-mute">{ALIGNMENT_WORDS[al][0].toUpperCase() + ALIGNMENT_WORDS[al].slice(1)}.</p>}
      <p className="mt-1 text-xs text-paper-mute">Observed policy is compared with the choice afterwards; it never enters the recommendation.</p>
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
}

export function CellDrawer({ cell, detail, detailStatus, detailError, country, core, cells, names, onClose, sectorHref }: CellDrawerProps) {
  const ref = useRef<HTMLDivElement>(null);
  const key = cellKey(cell.tech, cell.stage);
  useEffect(() => {
    const k = (e: KeyboardEvent) => e.key === 'Escape' && onClose();
    window.addEventListener('keydown', k);
    ref.current?.focus();
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
              style={{ background: na ? NOT_ASSESSED_HATCH : POSTURE_COLOR[cell.posture as 'S' | 'C' | 'D'], border: na ? NOT_ASSESSED_BORDER : undefined }}
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
        <ChoiceAnswer cell={cell} d={d} core={core} cells={cells} names={names} status={status} />
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
}

/**
 * The drawer a page mounts once (spec XIX): it loads the core and, on first open, the economy's detail
 * document, and shows the selected node. Nothing while closed, while the core loads, or for a key the data
 * does not know; a node the economy has no posture for says so.
 */
export function PostureNodeDrawer({ iso3, name, selected, onClose, sectorHref }: PostureNodeDrawerProps) {
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
            {parts.tech} · {stageDisplay(parts.stage)} is not classified for {name ?? names[iso3] ?? iso3}.
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
    />
  );
}
