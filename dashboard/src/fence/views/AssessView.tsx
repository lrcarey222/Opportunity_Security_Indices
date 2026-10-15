// /assess: the guided assessment.
//
//   0 What are you assessing?   1 Gate 1: acute harm   2 Gate 2: where the vulnerability sits
//   (2A digital, 2B physical)   3 Gate 3: a capability worth building?   4 Gate 4: the cost of
//   protection   -> Result
//
// The framework's questions lead; a quiet context note beside each gate carries the few measured
// facts that inform it (figures and sources one click away). Analyst mode asks every question; Quick mode asks
// the few that matter most and leaves the rest at their curated (or Unknown) values. Both share the
// same state and engine.

import { useMemo, type ReactNode } from 'react';
import { Link } from 'react-router';
import { Segmented, Select } from '../../components/ui.tsx';
import { useTitle } from '../title.ts';
import { INPUT_DEFS, UNKNOWN } from '../engine/index.ts';
import type { CategoricalInputId, InputId, Strategy, Tri } from '../engine/index.ts';
import { digitalApplies, physicalApplies } from '../engine/gates.ts';
import { baselineValue, componentById, recordsFor, stageScopeId } from '../data/adapter.ts';
import type { Stage, YFEvidenceRecord } from '../data/contract.ts';
import { stageKey, STAGES, withLayer, type AssessState, type PremiumWhy } from '../state/assessment.ts';
import { useYards, type Evaluation } from '../state/YardsContext.tsx';
import { PathCards, PremiumConsequences, PremiumSlider, Question } from '../components/Controls.tsx';
import { ContextNote } from '../components/Context.tsx';
import { Completeness, ConfidencePill, ProvBadge, SectionHead, YardBadge } from '../components/kit.tsx';
import { PAGE } from '../FenceLayout.tsx';

const STEPS = ['What are you assessing?', 'Acute harm?', 'Where does the vulnerability sit?', 'A capability worth building?', 'The cost of protection'];
const QUICK_STEPS = [0, 1, 2, 3, 4];

function ProgressRail({ step, quick, onGo, canLeave0 }: { step: number; quick: boolean; onGo: (i: number) => void; canLeave0: boolean }) {
  const steps = STEPS.map((t, i) => ({ t, i })).filter((s) => !quick || QUICK_STEPS.includes(s.i));
  return (
    <nav aria-label="Assessment progress" className="yf-no-print">
      <ol className="yf-rail flex flex-wrap gap-x-1 gap-y-2">
        {steps.map(({ t, i }) => {
          const current = i === step;
          const done = i < step;
          return (
            <li key={i} className="flex items-center">
              <button
                type="button"
                className={`flex items-center gap-2 border-b-2 px-2 py-1.5 text-left text-xs font-medium uppercase tracking-wider ${current ? 'border-signal text-paper' : done ? 'border-ink-500 text-paper-dim hover:text-paper' : 'border-transparent text-paper-mute hover:text-paper'}`}
                aria-current={current ? 'step' : undefined}
                disabled={i > 0 && !canLeave0}
                onClick={() => onGo(i)}
              >
                <span className={`yf-step-num flex h-5 w-5 items-center justify-center border text-[0.6875rem] ${current ? 'border-signal' : 'border-ink-500'}`} aria-hidden="true" />
                {t}
              </button>
            </li>
          );
        })}
        <li className="flex items-center">
          <span className="px-2 text-xs uppercase tracking-wider text-paper-mute">→ Result</span>
        </li>
      </ol>
    </nav>
  );
}

const TRI_TEXT: Record<Tri, string> = { yes: 'Yes', no: 'No', unknown: 'Unknown' };

function TriRow({ label, state, marginal, na }: { label: string; state: Tri; marginal?: boolean; na?: boolean }) {
  return (
    <li className="flex items-center justify-between gap-3 py-1 text-sm">
      <span className="text-paper-dim">{label}</span>
      <span className={na ? 'text-paper-mute' : `yf-ans-${state}`}>
        {na ? 'n/a' : TRI_TEXT[state]}
        {marginal && !na && <span className="ml-1 text-xs text-paper-mute">(near threshold)</span>}
      </span>
    </li>
  );
}

function LiveStatus({ e }: { e: Evaluation }) {
  const { href } = useYards();
  const r = e.baselineResult;
  const g = r.gates;
  return (
    <aside className="panel p-4" aria-label="Where this is heading">
      <div className="eyebrow mb-2">Where this is heading</div>
      <YardBadge yard={r.yard} size="md" />
      {r.yard === 'indeterminate' && <p className="mt-1 text-xs text-paper-mute">Still possible: {r.possible.join(', ')}</p>}
      <div className="mt-3 flex flex-wrap gap-x-5 gap-y-2">
        <ConfidencePill c={r.confidence} />
        <Completeness value={r.evidenceCompleteness} />
      </div>
      <ul className="mt-3 border-t hairline pt-2">
        <TriRow label="Acute consequence" state={g.small.acute.state} />
        <TriRow label="Control-layer vulnerability" state={g.small.digital.state} na={!g.small.digital.applicable} marginal={g.small.digital.marginal} />
        <TriRow label="Physical chokepoint" state={g.small.physical.state} na={!g.small.physical.applicable} marginal={g.small.physical.marginal} />
        <TriRow label="Capability worth building" state={g.medium.state} />
      </ul>
      <Link className="link mt-3 inline-block text-sm" to={href('result')}>
        See the full result
      </Link>
    </aside>
  );
}

function LayerChips() {
  const { subjects, focus, update } = useYards();
  if (subjects.length < 2) return null;
  return (
    <div className="mb-5">
      <div className="label">Layer being assessed</div>
      <div className="flex flex-wrap gap-2" role="group" aria-label="Layer being assessed">
        {subjects.map((s) => (
          <button key={s.key} type="button" className="tag yf-chip" aria-pressed={s.key === focus?.key} onClick={() => update((st) => ({ ...st, focus: s.key }))}>
            {s.label}
          </button>
        ))}
      </div>
    </div>
  );
}

// ── Step 0 ───────────────────────────────────────────────────────────────────────────────────────

function ChooseStep() {
  const { data, state, update } = useYards();
  const ref = data.reference;
  const cells = data.bundle.cells;
  const techs = [...new Set(cells.map((c) => c.technology))].sort();
  const tech = state.sel?.tech ?? '';
  const stage: Stage = state.sel?.stage ?? 'Midstream';
  const stagesFor = (t: string) => STAGES.filter((s) => cells.some((c) => c.technology === t && c.stage === s));
  const cell = cells.find((c) => c.technology === tech && c.stage === stage);
  const comps = ref.components.filter((c) => c.technology === tech && c.stage === stage);
  const stacks = ref.stacks.filter((s) => (s.technology === tech && s.stage === stage) || s.layers.some((id) => comps.some((c) => c.id === id)));
  const choice = state.sel?.stack ? `stack:${state.sel.stack}` : state.sel?.component ? `comp:${state.sel.component}` : state.sel ? 'stage' : '';

  const select = (patch: Partial<NonNullable<AssessState['sel']>> & { tech: string; stage: Stage }, focus?: string) =>
    update((s) => ({ ...s, sel: { tech: patch.tech, stage: patch.stage, component: patch.component, stack: patch.stack }, focus, example: undefined }));

  return (
    <div className="space-y-8">
      <SectionHead eyebrow="Stage 1" title="What are you trying to assess?" />
      <div className="flex flex-wrap items-end gap-x-6 gap-y-4">
        <Select
          label="Country"
          value={state.country}
          options={data.manifest.countries.map((c) => ({ value: c.iso3, label: c.name }))}
          onChange={(v) => update((s) => ({ ...s, country: v }))}
        />
        <Select
          label="Technology"
          value={tech}
          options={[{ value: '', label: 'Choose a technology', disabled: true }, ...techs.map((t) => ({ value: t, label: t }))]}
          onChange={(t) => {
            const st = stagesFor(t).includes(stage) ? stage : stagesFor(t)[0];
            select({ tech: t, stage: st }, stageKey(t, st));
          }}
        />
        <Segmented<Stage>
          label="Supply-chain stage"
          value={stage}
          options={STAGES.map((s) => ({ value: s, label: s, disabled: !tech || !stagesFor(tech).includes(s) }))}
          onChange={(s) => tech && select({ tech, stage: s }, stageKey(tech, s))}
        />
      </div>
      {data.manifest.countries.length === 1 && (
        <p className="text-sm text-paper-mute">The data bundle is built for the United States; the architecture takes other countries as the export adds them.</p>
      )}

      {tech && (
        <section aria-labelledby="yf-where">
          <h3 id="yf-where" className="text-lg font-medium text-paper">
            Where exactly does the relevant risk or capability sit?
          </h3>
          <p className="mt-1 max-w-3xl text-sm text-paper-dim">
            Broad technologies have layers that face different problems. Component choices come from a curated NEIS table, not from the indices: the data describe the stage, an HS6 sub-component or a mineral, and every number says which.
          </p>
          <div className="mt-4 grid gap-3 md:grid-cols-2" role="radiogroup" aria-label="What to assess">
            {stacks.map((s) => (
              <label key={s.id} className="yf-choice">
                <input type="radio" name="yf-what" checked={choice === `stack:${s.id}`} onChange={() => select({ tech: s.technology, stage: s.stage, stack: s.id }, s.layers[0])} />
                <span>
                  <span className="block text-sm font-medium text-paper">{s.label}</span>
                  <span className="block text-xs text-paper-mute">Whole system · {s.layers.length} layers, each judged on its own</span>
                  <span className="mt-1 block text-sm text-paper-dim">{s.description}</span>
                </span>
              </label>
            ))}
            {comps.map((c) => (
              <label key={c.id} className="yf-choice">
                <input type="radio" name="yf-what" checked={choice === `comp:${c.id}`} onChange={() => select({ tech, stage, component: c.id }, c.id)} />
                <span>
                  <span className="block text-sm font-medium text-paper">{c.label}</span>
                  <span className="block text-xs text-paper-mute">
                    {c.layer} · {c.evidence ? `evidence: ${c.evidence.scope === 'subcomponent' ? 'HS6 sub-component' : c.evidence.scope}` : 'no measured scope (curated only)'}
                  </span>
                  <span className="mt-1 block text-sm text-paper-dim">{c.description}</span>
                </span>
              </label>
            ))}
            <label className="yf-choice">
              <input type="radio" name="yf-what" checked={choice === 'stage'} onChange={() => select({ tech, stage }, stageKey(tech, stage))} />
              <span>
                <span className="block text-sm font-medium text-paper">
                  {tech} — {stage}, as a whole
                </span>
                <span className="block text-xs text-paper-mute">technology-stage evidence; every judgment starts as Unknown</span>
              </span>
            </label>
          </div>
          {cell && cell.subcomponents.length > 0 && (
            <details className="disclosure mt-4">
              <summary className="cursor-pointer text-sm text-paper-dim">What the trade data can see at this stage ({cell.subcomponents.length} HS6 sub-components)</summary>
              <ul className="mt-2 flex flex-wrap gap-2">
                {cell.subcomponents.map((s) => (
                  <li key={s.id} className="tag">
                    {s.label}
                  </li>
                ))}
              </ul>
            </details>
          )}
        </section>
      )}
    </div>
  );
}

// ── Shared step pieces ───────────────────────────────────────────────────────────────────────────

function useLayerActions(e: Evaluation | null) {
  const { update } = useYards();
  const key = e?.subject.key ?? '';
  const setAnswer = (id: InputId, v: string | number | undefined) =>
    update((s) =>
      withLayer(s, key, (l) => {
        const answers = { ...l.answers };
        if (v === undefined) delete answers[id];
        else answers[id] = v;
        return { ...l, answers };
      }),
    );
  const setStrategies = (v: Strategy[]) => update((s) => withLayer(s, key, (l) => ({ ...l, strategies: v })));
  const setWhy = (v: PremiumWhy | undefined) => update((s) => withLayer(s, key, (l) => ({ ...l, why: v })));
  return { setAnswer, setStrategies, setWhy };
}

function Q({ e, id, hint, quick }: { e: Evaluation; id: CategoricalInputId; hint?: ReactNode; quick: boolean }) {
  const { setAnswer } = useLayerActions(e);
  if (quick && !INPUT_DEFS[id].quick) return null;
  return <Question id={id} iv={e.baseline.inputs[id]} baseline={baselineValue(e.subject, e.evidence, id)} onChange={(v) => setAnswer(id, v)} hint={hint} />;
}

/** A record on the component's own scope and engine measure. */
function prim(e: Evaluation, metric: string): YFEvidenceRecord | undefined {
  return e.evidence.primaryRecords.find((r) => r.metricId === metric && r.measure === e.evidence.engineMeasure);
}

// ── Gates 1-4 ────────────────────────────────────────────────────────────────────────────────────

function Gate1({ e, quick }: { e: Evaluation; quick: boolean }) {
  const comp = e.subject.component;
  return (
    <div>
      <SectionHead eyebrow="Decision gate 1" title="Could disruption or foreign control cause acute harm?">
        Start with the consequence, not the supplier share. A high foreign market share alone does not put anything in the Small Yard.
      </SectionHead>
      {comp?.criticalFunction && (
        <p className="mb-2 text-sm text-paper-dim">
          Critical function at stake <ProvBadge p="assessment" className="ml-1" />: <span className="text-paper">{comp.criticalFunction}</span>
        </p>
      )}
      <div className="panel px-5">
        <Q e={e} id="consequence" quick={quick} />
        <Q e={e} id="timeHorizon" quick={quick} />
      </div>
    </div>
  );
}

function ScopePreview({ e }: { e: Evaluation }) {
  const sep = e.baseline.inputs.separability.value;
  const rows = [
    { label: 'Firmware, communications, remote access', control: true },
    { label: 'Integrated hardware', control: false, integrated: true },
    { label: 'Passive hardware and structure', control: false },
  ];
  const fenced = (r: (typeof rows)[number]) => r.control || (sep === 'partial' && r.integrated) || sep === 'inseparable' || (sep === UNKNOWN && r.integrated);
  return (
    <figure className="yf-yard-small mt-3">
      <ol className="space-y-1">
        {rows.map((r) => (
          <li key={r.label} className={`yf-layer flex items-center justify-between border px-3 py-1.5 text-sm ${fenced(r) ? 'yf-yard-fill border-[var(--yard)] text-paper' : 'border-ink-600 text-paper-mute'}`}>
            {r.label}
            <span className="text-xs uppercase tracking-wider">{fenced(r) ? (sep === UNKNOWN && !r.control ? 'fence?' : 'behind the fence') : 'open'}</span>
          </li>
        ))}
      </ol>
      <figcaption className="mt-2 text-xs text-paper-mute">The fence should surround the vulnerability, not automatically the entire product.</figcaption>
    </figure>
  );
}

function Gate2({ e, quick }: { e: Evaluation; quick: boolean }) {
  const a = e.baseline;
  const dig = digitalApplies(a);
  const phys = physicalApplies(a);
  const share = a.inputs.nonAlliedTopShare;
  return (
    <div className="space-y-8">
      <SectionHead eyebrow="Decision gate 2" title="Where does the vulnerability sit?">
        A control layer an outsider could command is a different problem from a physical input that could be withheld.
      </SectionHead>
      {dig && (
        <section aria-labelledby="yf-2a">
          <h3 id="yf-2a" className="text-lg font-medium text-paper">
            2A · Digitally active systems
          </h3>
          <div className="mt-3 grid gap-6 lg:grid-cols-[minmax(0,3fr)_minmax(0,2fr)]">
            <div className="panel px-5">
              {e.subject.component?.layer !== 'control' && <Q e={e} id="digitallyActive" quick={quick} />}
              <Q e={e} id="remoteControl" quick={quick} />
              <Q e={e} id="separability" quick={quick} />
            </div>
            <ScopePreview e={e} />
          </div>
        </section>
      )}
      {phys && (
        <section aria-labelledby="yf-2b">
          <h3 id="yf-2b" className="text-lg font-medium text-paper">
            2B · Physical chokepoints
          </h3>
          <p className="mt-1 max-w-3xl text-sm text-paper-dim">
            Concentration becomes a small-yard problem only when a potentially coercive supplier holds most of supply, substitutes are poor, and trusted supply could not be restored before harm becomes intolerable.
          </p>
          <div className="mt-3 grid gap-6 lg:grid-cols-[minmax(0,3fr)_minmax(0,2fr)]">
            <div className="panel px-5">
              <Q e={e} id="substitutability" quick={quick} />
              <Q e={e} id="qualificationTime" quick={quick} />
              <Q e={e} id="reconstitutionTime" quick={quick} />
              <Q e={e} id="stockpile" quick={quick} />
              <Q e={e} id="surgeCapacity" quick={quick} />
            </div>
            <ContextNote
              records={share.provenance === 'assessment' ? [] : [prim(e, 'non_coalition_top'), prim(e, 'coalition_share')]}
              empty="No measured supplier shares isolate this component, so the concentration test stays Unknown unless you set it in the scenario lab."
            >
              {share.provenance === 'assessment' && (
                <p className="text-sm leading-relaxed text-paper-dim">
                  <ProvBadge p="assessment" className="mr-1" /> {share.subject} is assessed to hold about {Math.round(Number(share.value) * 100)}% of supply. {share.note}
                </p>
              )}
            </ContextNote>
          </div>
        </section>
      )}
    </div>
  );
}

function Gate3({ e, quick }: { e: Evaluation; quick: boolean }) {
  const { setStrategies } = useLayerActions(e);
  const { data } = useYards();
  const stage = recordsFor(data.bundle, stageScopeId(e.subject.technology, e.subject.stage));
  const rec = (id: string): YFEvidenceRecord | undefined => stage.find((r) => r.metricId === id);
  const home = e.evidence.primaryRecords.find((r) => r.metricId === 'home_share' && r.measure === e.evidence.homeMeasure);
  const leap = e.baseline.strategies.includes('leapAhead');
  return (
    <div className="space-y-8">
      <SectionHead eyebrow="Decision gate 3" title="Is this a capability worth building or preserving?">
        A capability belongs in the medium yard if there is a present position worth defending, production carries learning that matters for what comes next, or a successor technology offers a credible leap. Market size alone is not a reason.
      </SectionHead>
      <div className="grid gap-6 lg:grid-cols-[minmax(0,3fr)_minmax(0,2fr)]">
        <div className="panel px-5">
          <Q e={e} id="processKnowledge" quick={quick} />
          <Q e={e} id="workforceSuppliers" quick={quick} />
          <Q e={e} id="adjacentInnovation" quick={quick} />
          <Q e={e} id="complementaryCapabilities" quick={quick} />
        </div>
        <ContextNote records={[home, rec('home_investment_growth')]} />
      </div>
      <section aria-labelledby="yf-paths">
        <h3 id="yf-paths" className="mb-3 text-lg font-medium text-paper">
          Preserve, catch up or leap ahead?
        </h3>
        <PathCards
          value={e.baseline.strategies}
          onChange={setStrategies}
          notes={{
            preserve: 'Needs a position worth defending, and a reason it matters for what comes next.',
            catchUp: 'Usually depends on learning from the frontier: keep a gate open for licensing and joint ventures.',
            leapAhead: 'Needs an advantage customers value, evidence beyond novelty and a path to scale. Readiness data inform it; they never decide it.',
          }}
        />
      </section>
      {leap && (
        <section aria-labelledby="yf-leap">
          <h3 id="yf-leap" className="text-lg font-medium text-paper">
            Is the leap-ahead case credible?
          </h3>
          <div className="panel mt-3 px-5">
            <Q e={e} id="costPerfAdvantage" quick={false} />
            <Q e={e} id="incumbentVulnerable" quick={false} />
            <Q e={e} id="scalePathway" quick={false} />
            <Q e={e} id="beyondNovelty" quick={false} />
          </div>
        </section>
      )}
    </div>
  );
}

const WHY_OPTS: { value: PremiumWhy | 'unknown'; label: string }[] = [
  { value: 'security', label: 'Acute security (revisit gates 1-2)' },
  { value: 'learning', label: 'Industrial learning (gate 3)' },
  { value: 'diversification', label: 'Supply diversification only' },
  { value: 'jobs', label: 'Jobs and regional development' },
  { value: 'none', label: 'None' },
  { value: 'unknown', label: 'Unknown' },
];

function Gate4({ e }: { e: Evaluation }) {
  const { setAnswer, setWhy } = useLayerActions(e);
  const { data } = useYards();
  const stage = recordsFor(data.bundle, stageScopeId(e.subject.technology, e.subject.stage));
  const rec = (id: string, measure?: string) => stage.find((r) => r.metricId === id && (!measure || r.measure === measure));
  const p = e.baseline.inputs.premium;
  const why = e.baseline.premiumJustification;
  return (
    <div className="space-y-8">
      <SectionHead eyebrow="Decision gate 4" title="Would insisting on domestic production cost more than the strategic benefit it creates?">
        Where neither the acute-security case nor the industrial-capability case is strong, the default is the Large Yard. Make the cost of protection explicit.
      </SectionHead>
      <div className="grid gap-6 lg:grid-cols-[minmax(0,3fr)_minmax(0,2fr)]">
      <fieldset className="panel p-5">
        <legend className="text-[0.9375rem] font-medium text-paper">What strategic benefit would justify paying more for domestic production?</legend>
        <div className="mt-3 flex flex-wrap gap-2">
          {WHY_OPTS.map((o) => (
            <label key={o.value} className="yf-choice text-sm text-paper">
              <input type="radio" name="yf-why" checked={(why ?? 'unknown') === o.value} onChange={() => setWhy(o.value === 'unknown' ? undefined : (o.value as PremiumWhy))} />
              {o.label}
            </label>
          ))}
        </div>
      </fieldset>
      <ContextNote records={[rec('trl'), rec('osi_addressable_market')]} />
      </div>
      <div className="grid gap-6 lg:grid-cols-2">
        <div className="panel p-5">
          <PremiumSlider iv={p} onChange={(v) => setAnswer('premium', v)} />
          <p className="mt-3 text-xs leading-relaxed text-paper-mute">Bands used by the engine: under 10% low, 10-30% moderate, over 30% high. Enter your own estimate; the indices do not measure a domestic premium.</p>
        </div>
        <div className="panel p-5">
          <div className="label">Directional consequences</div>
          <PremiumConsequences premium={p.value === UNKNOWN ? null : Number(p.value)} />
          <p className="mt-2 text-xs text-paper-mute">Directions only: no deployment elasticity is estimated here.</p>
        </div>
      </div>
    </div>
  );
}

// ── Page ─────────────────────────────────────────────────────────────────────────────────────────

export default function AssessView() {
  useTitle('Assess');
  const { state, update, focus, evaluate, go, data } = useYards();
  const quick = state.mode === 'quick';
  const step = Math.min(state.step, STEPS.length - 1);
  const e = useMemo(() => (focus ? evaluate(focus) : null), [focus, evaluate]);
  const order = quick ? QUICK_STEPS : STEPS.map((_, i) => i);
  const idx = Math.max(0, order.indexOf(step));
  // steps replace the history entry, like answers do: browser Back leaves the assessment rather than
  // returning to a URL that lacks the answers given since (the in-page Back button moves between steps)
  const goStep = (i: number) => update((s) => ({ ...s, step: i }), { resetScroll: true });
  const next = () => (idx < order.length - 1 ? goStep(order[idx + 1]) : go('result'));
  const prev = () => idx > 0 && goStep(order[idx - 1]);
  const comp = focus?.component ?? componentById(data.reference, state.sel?.component);

  return (
    <div className={PAGE}>
      <div className="mb-6 flex flex-wrap items-center justify-between gap-4">
        <ProgressRail step={step} quick={quick} onGo={goStep} canLeave0={!!state.sel} />
        <Segmented<'analyst' | 'quick'>
          label="Depth"
          hideLabel
          value={state.mode}
          options={[
            { value: 'analyst', label: 'Analyst', title: 'Every question, with the evidence' },
            { value: 'quick', label: 'Quick', title: 'About five questions; the rest keep their curated or Unknown values' },
          ]}
          onChange={(m) => update((s) => ({ ...s, mode: m }))}
        />
      </div>

      <div className={step === 0 ? '' : 'grid gap-8 lg:grid-cols-[minmax(0,1fr)_17rem]'}>
        <div className="min-w-0">
          {step > 0 && <LayerChips />}
          {step > 0 && focus && (
            <p className="mb-4 text-sm text-paper-mute">
              {focus.label}
              {comp && <> · {comp.layer}</>} · {focus.technology} — {focus.stage}
            </p>
          )}
          {step === 0 && <ChooseStep />}
          {step === 1 && e && <Gate1 e={e} quick={quick} />}
          {step === 2 && e && <Gate2 e={e} quick={quick} />}
          {step === 3 && e && <Gate3 e={e} quick={quick} />}
          {step === 4 && e && <Gate4 e={e} />}
          <div className="yf-no-print mt-10 flex flex-wrap items-center gap-3 border-t hairline pt-5">
            {idx > 0 && (
              <button type="button" className="btn btn-ghost" onClick={prev}>
                Back
              </button>
            )}
            <button type="button" className="btn btn-primary" onClick={next} disabled={!state.sel}>
              {idx < order.length - 1 ? 'Next' : 'See the result'}
            </button>
            {state.sel && (
              <button
                type="button"
                className="btn btn-quiet ml-auto"
                onClick={() => {
                  if (window.confirm('Clear every answer and start again?')) go('assess', { country: state.country, sel: null, mode: state.mode, layers: {}, step: 0 });
                }}
              >
                Reset
              </button>
            )}
          </div>
        </div>
        {step > 0 && e && (
          <div className="yf-no-print lg:sticky lg:top-4 lg:self-start">
            <LiveStatus e={e} />
          </div>
        )}
      </div>
    </div>
  );
}
