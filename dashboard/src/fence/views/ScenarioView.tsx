// /scenario: "Test another world". Change observed data or curated assumptions and see
// the result move. Scenario values are kept apart from the observed baseline (SCENARIO badges), and
// a change of yard is explained by the gate that changed and the inputs that fed it.

import { useId, useMemo } from 'react';
import { Link } from 'react-router';
import { useTitle } from '../title.ts';
import { DOMAINS, explainTransition, formatNumber, INPUT_DEFS, optionLabel, UNKNOWN, YARD_NAME } from '../engine/index.ts';
import { digitalApplies, physicalApplies } from '../engine/gates.ts';
import type { CategoricalInputId, InputId, NumericInputId } from '../engine/index.ts';
import { layerOf, withLayer } from '../state/assessment.ts';
import { useYards, type Evaluation } from '../state/YardsContext.tsx';
import { ComponentStack } from '../components/ComponentStack.tsx';
import { InputTip } from '../components/InfoTip.tsx';
import { Completeness, ConfidencePill, ProvBadge, SectionHead, YardBadge, yardClass } from '../components/kit.tsx';
import { PAGE } from '../FenceLayout.tsx';
import { NoAssessment } from './ResultView.tsx';

/** Observed market conditions the tests read (shares and concentration), after the framework's own assumptions. */
const MARKET: NumericInputId[] = ['nonAlliedTopShare', 'homeShare', 'hhi'];
function groupsFor(e: Evaluation): { title: string; ids: CategoricalInputId[] }[] {
  const dig = digitalApplies(e.scenario);
  const phys = physicalApplies(e.scenario);
  const security: CategoricalInputId[] = ['consequence', 'timeHorizon'];
  if (dig) security.push(...(e.subject.component?.layer === 'control' ? [] : (['digitallyActive'] as CategoricalInputId[])), 'remoteControl', 'separability');
  if (phys) security.push('substitutability', 'qualificationTime', 'reconstitutionTime', 'stockpile', 'surgeCapacity');
  const out = [
    { title: 'Security', ids: security },
    { title: 'Capability', ids: ['processKnowledge', 'workforceSuppliers', 'adjacentInnovation', 'complementaryCapabilities', 'maturity'] as CategoricalInputId[] },
  ];
  if (e.scenario.strategies.includes('leapAhead'))
    out.push({ title: 'Leap-ahead', ids: ['costPerfAdvantage', 'incumbentVulnerable', 'scalePathway', 'beyondNovelty'] });
  return out;
}

function useScenarioSetter(e: Evaluation) {
  const { update } = useYards();
  return (id: InputId, v: string | number | undefined) =>
    update((s) =>
      withLayer(s, e.subject.key, (l) => {
        const scenario = { ...l.scenario };
        if (v === undefined) delete scenario[id];
        else scenario[id] = v;
        return { ...l, scenario };
      }),
    );
}

function NumericControl({ e, id }: { e: Evaluation; id: NumericInputId }) {
  const set = useScenarioSetter(e);
  const uid = useId();
  const def = INPUT_DEFS[id];
  const nm = def.numeric!;
  const base = e.baseline.inputs[id];
  const cur = e.scenario.inputs[id];
  const changed = cur.provenance === 'scenario' || (cur.provenance === 'unknown' && base.provenance !== 'unknown');
  const v = cur.value === UNKNOWN ? (base.value === UNKNOWN ? (nm.min + nm.max) / 2 : Number(base.value)) : Number(cur.value);
  return (
    <div className={`border-b hairline py-3 last:border-b-0 ${changed ? 'yf-flip' : ''}`}>
      <div className="flex flex-wrap items-baseline justify-between gap-2">
        <span className="flex items-center">
          <label htmlFor={uid} className="text-sm text-paper">
            {def.label}
          </label>
          <InputTip id={id} />
        </span>
        <span className="flex items-center gap-2 text-sm">
          <span className="tabular text-paper">{cur.value === UNKNOWN ? 'Unknown' : formatNumber(nm.unit, v)}</span>
          <ProvBadge p={cur.provenance} />
        </span>
      </div>
      <input
        id={uid}
        type="range"
        className="slider mt-2 w-full"
        min={nm.min}
        max={nm.max}
        step={nm.step}
        value={v}
        style={{ ['--fill' as string]: `${(100 * (v - nm.min)) / (nm.max - nm.min || 1)}%` }}
        aria-valuetext={formatNumber(nm.unit, v)}
        onChange={(ev) => set(id, Number(ev.target.value))}
      />
      <div className="mt-1 flex flex-wrap gap-x-3 text-xs text-paper-mute">
        <span>
          Observed: {optionLabel(id, base.value as string | number)}
          {base.subject && base.value !== UNKNOWN ? ` (${base.subject})` : ''} · {base.provenance === 'data' ? 'data' : base.provenance}
        </span>
        {changed && (
          <button type="button" className="link" onClick={() => set(id, undefined)}>
            Reset
          </button>
        )}
        {cur.value !== UNKNOWN && (
          <button type="button" className="link" onClick={() => set(id, UNKNOWN)}>
            Unknown
          </button>
        )}
      </div>
    </div>
  );
}

function CategoricalControl({ e, id }: { e: Evaluation; id: CategoricalInputId }) {
  const set = useScenarioSetter(e);
  const uid = useId();
  const def = INPUT_DEFS[id];
  const base = e.baseline.inputs[id];
  const cur = e.scenario.inputs[id];
  const changed = cur.provenance === 'scenario' || (cur.value === UNKNOWN && base.provenance !== 'unknown');
  return (
    <div className="flex flex-wrap items-center justify-between gap-2 border-b hairline py-2 last:border-b-0">
      <div className="min-w-[10rem] flex-1">
        <span className="flex items-center">
          <label htmlFor={uid} className="text-sm text-paper-dim">
            {def.label}
          </label>
          <InputTip id={id} />
        </span>
        <span id={`${uid}-base`} className="block text-xs text-paper-mute">
          Baseline: {optionLabel(id, base.value as string)} · <ProvBadge p={base.provenance} />
        </span>
      </div>
      <span className="flex items-center gap-2">
        <select
          id={uid}
          aria-describedby={`${uid}-base`}
          className="field field-sm w-44"
          value={String(cur.value)}
          onChange={(ev) => set(id, ev.target.value === String(base.value) && base.provenance !== 'unknown' ? undefined : ev.target.value)}
        >
          {[...(DOMAINS[id] as readonly string[]), UNKNOWN].map((o) => (
            <option key={o} value={o}>
              {o === UNKNOWN ? 'Unknown' : optionLabel(id, o)}
            </option>
          ))}
        </select>
        {changed ? <ProvBadge p="scenario" /> : <span className="w-[4.5rem]" aria-hidden="true" />}
      </span>
    </div>
  );
}

function ResultCard({ title, r, badge }: { title: string; r: Evaluation['baselineResult']; badge: 'data' | 'scenario' }) {
  return (
    <article className={`${yardClass(r.yard)} panel yf-yard-edge p-5`}>
      <div className="flex items-center justify-between gap-2">
        <h3 className="eyebrow">{title}</h3>
        <ProvBadge p={badge} />
      </div>
      <div key={`${r.yard}-${r.variant}`} className="yf-flip mt-3">
        <YardBadge yard={r.yard} />
        <p className="mt-3 text-sm font-medium text-paper">{r.recommendedPosture}</p>
      </div>
      <div className="mt-3 flex flex-wrap gap-x-5 gap-y-2">
        <ConfidencePill c={r.confidence} />
        <Completeness value={r.evidenceCompleteness} />
      </div>
    </article>
  );
}

export default function ScenarioView() {
  useTitle('Test another world');
  const { state, subjects, focus, evaluate, update, href, data } = useYards();
  const evals = useMemo(() => subjects.map((s) => evaluate(s)), [subjects, evaluate]);
  if (!state.sel || !focus) return <NoAssessment />;
  const e = evals.find((x) => x.subject.key === focus.key) ?? evals[0];
  const t = explainTransition(e.baseline, e.baselineResult, e.scenario, e.scenarioResult);
  const stack = data.reference.stacks.find((s) => s.id === state.sel?.stack);
  const nChanged = Object.keys(layerOf(state, e.subject.key).scenario).length;
  const resetAll = () =>
    update((s) => ({ ...s, layers: Object.fromEntries(Object.entries(s.layers).map(([k, l]) => [k, { ...l, scenario: {} }])) }));

  return (
    <div className={PAGE}>
      <SectionHead eyebrow="Scenario lab" title="Test another world">
        Change what you assume about the world and watch the diagnosis respond: harsher consequences, faster or slower replacement, weaker learning, a costlier fence, a more or less concentrated market. Observed values stay as they are; every change here is a scenario assumption.
      </SectionHead>

      {stack && (
        <section className="mb-8">
          <ComponentStack
            title={stack.label}
            layers={evals.map((x) => ({ key: x.subject.key, label: x.subject.label, layer: x.subject.component?.layer ?? 'stage', result: x.scenarioResult, changed: x.hasScenario && x.scenarioResult.yard !== x.baselineResult.yard }))}
            focusKey={focus.key}
            onFocus={(k) => update((s) => ({ ...s, focus: k }))}
          />
          <p className="mt-2 text-sm text-paper-mute">Scenario controls below apply to the selected layer: {focus.label}.</p>
        </section>
      )}

      <div className="grid gap-8 lg:grid-cols-[minmax(0,1fr)_22rem]">
        <div className="space-y-6">
          <div className="flex flex-wrap items-center gap-3">
            <h2 className="text-lg font-medium text-paper">{focus.label}</h2>
            {nChanged > 0 && <span className="yf-prov yf-prov-scenario">{nChanged} changed</span>}
            <button type="button" className="btn btn-ghost btn-sm ml-auto" onClick={resetAll} disabled={!evals.some((x) => x.hasScenario)}>
              Reset to observed data
            </button>
          </div>
          {groupsFor(e).map((g) => (
            <section key={g.title} className="panel px-5 py-2" aria-label={`${g.title} assumptions`}>
              <h3 className="eyebrow pt-3">{g.title} assumptions</h3>
              {g.ids.map((id) => (
                <CategoricalControl key={id} e={e} id={id} />
              ))}
            </section>
          ))}
          <section className="panel px-5 py-2" aria-labelledby="yf-sc-cost">
            <h3 id="yf-sc-cost" className="eyebrow pt-3">
              Cost of protection
            </h3>
            <NumericControl e={e} id="premium" />
          </section>
          <section className="panel px-5 py-2" aria-labelledby="yf-sc-data">
            <h3 id="yf-sc-data" className="eyebrow pt-3">
              Market conditions (observed data you can change)
            </h3>
            {MARKET.map((id) => (
              <NumericControl key={id} e={e} id={id} />
            ))}
          </section>
        </div>
        <div className="space-y-4 lg:sticky lg:top-4 lg:self-start">
          <ResultCard title="Observed baseline" r={e.baselineResult} badge="data" />
          <ResultCard title="Scenario" r={e.scenarioResult} badge="scenario" />
          <section className={`panel p-5 ${t.changed ? 'border-signal' : ''}`} aria-live="polite" aria-atomic="true">
            <div key={t.headline + t.explanation} className="yf-flip">
              <h3 className="text-base font-semibold text-paper">{t.changed ? t.headline : `Still ${YARD_NAME[t.to]}`}</h3>
              <p className="mt-2 text-sm leading-relaxed text-paper-dim">{t.changed ? t.explanation : nChanged ? 'Your changes move the evidence but not the diagnosis.' : 'Change an input to test another world.'}</p>
            </div>
          </section>
          <Link className="link text-sm" to={href('result')}>
            Full reasoning for the scenario result
          </Link>
        </div>
      </div>
    </div>
  );
}
