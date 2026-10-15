// /result: the result and the reasoning. For a system, the component stack first, then
// the layer in focus. Everything a printed or shared brief needs is on this page.

import { useEffect, useMemo } from 'react';
import { Link } from 'react-router';
import { useTitle } from '../title.ts';
import { ALL_INPUT_IDS, INPUT_DEFS, optionLabel, stackOutcome, UNKNOWN, YARD_NAME } from '../engine/index.ts';
import type { InputId } from '../engine/index.ts';
import { isRelevant } from '../engine/gates.ts';
import { exampleById } from '../content/examples.ts';
import { useYards, type Evaluation } from '../state/YardsContext.tsx';
import { ComponentStack } from '../components/ComponentStack.tsx';
import { ChangeConditions, DecisionTree, EvidenceGaps, PolicyPosture, Reasons, ResultHeadline, Warnings } from '../components/ResultBlocks.tsx';
import { ProvBadge, SectionHead, YardBadge } from '../components/kit.tsx';
import { EvidencePanel } from '../components/EvidencePanel.tsx';
import { InputTip } from '../components/InfoTip.tsx';
import { Disclosure } from '../../components/ui.tsx';
import { PAGE } from '../FenceLayout.tsx';

const STRAT_LABEL = { preserve: 'Preserve', catchUp: 'Catch up', leapAhead: 'Leap ahead' } as const;

export function NoAssessment() {
  const { href } = useYards();
  return (
    <div className={PAGE}>
      <SectionHead eyebrow="Result" title="Nothing assessed yet">
        Choose a technology and a component to assess, or open one of the examples.
      </SectionHead>
      <div className="flex gap-3">
        <Link className="btn btn-primary" to={href('assess')}>
          Start an assessment
        </Link>
        <Link className="btn btn-ghost" to={href('examples')}>
          Explore an example
        </Link>
      </div>
    </div>
  );
}

function Assumptions({ e }: { e: Evaluation }) {
  const a = e.scenario;
  const r = e.scenarioResult;
  const rows = ALL_INPUT_IDS.filter((id) => isRelevant(a, id, r.possible) || ['hhi', 'alliedShare', 'trl'].includes(id));
  return (
    <table className="w-full text-sm">
      <caption className="sr-only">Every input to this result, with where it came from</caption>
      <thead>
        <tr className="border-b hairline text-left">
          <th scope="col" className="label py-2 pr-3">
            Input
          </th>
          <th scope="col" className="label py-2 pr-3">
            Value
          </th>
          <th scope="col" className="label py-2">
            Basis
          </th>
        </tr>
      </thead>
      <tbody>
        {rows.map((id) => {
          const iv = a.inputs[id];
          return (
            <tr key={id} className="border-b hairline align-top last:border-b-0">
              <th scope="row" className="py-2 pr-3 text-left font-normal text-paper-dim">
                {INPUT_DEFS[id].label}
                <InputTip id={id} />
              </th>
              <td className="py-2 pr-3 text-paper">
                {optionLabel(id, iv.value as string | number)}
                {iv.subject && iv.value !== UNKNOWN && <span className="text-paper-mute"> · {iv.subject}</span>}
              </td>
              <td className="py-2">
                <ProvBadge p={iv.provenance} />
                {iv.note && <span className="ml-2 text-xs text-paper-mute">{iv.note}</span>}
              </td>
            </tr>
          );
        })}
      </tbody>
    </table>
  );
}

/** The assessment step that asks about an input. */
export function stepOfInput(id: InputId): number {
  const g = INPUT_DEFS[id].gate;
  return g === 'gate1' ? 1 : g === 'digital' || g === 'physical' ? 2 : g === 'capability' || g === 'leap' ? 3 : 4;
}

export function FullResult({ e }: { e: Evaluation }) {
  const { href, go, state, data } = useYards();
  const r = e.scenarioResult;
  const comp = e.subject.component;
  return (
    <div className="space-y-10">
      <ResultHeadline title={e.subject.label} result={r} context={`${e.subject.technology} — ${e.subject.stage}${e.hasScenario ? ' · scenario' : ''}`} />
      {e.hasScenario && (
        <p className="text-sm text-paper-dim">
          <ProvBadge p="scenario" /> This result includes scenario assumptions. Observed baseline: <strong className="font-medium text-paper">{YARD_NAME[e.baselineResult.yard]}</strong>.{' '}
          <Link className="link yf-no-print" to={href('scenario')}>
            Open the scenario lab
          </Link>
        </p>
      )}
      <Warnings result={r} />
      <section aria-labelledby="yf-why">
        <SectionHead id="yf-why" title="Why" />
        <Reasons result={r} />
        {r.strategies.length > 0 && (
          <p className="mt-4 text-sm text-paper-dim">
            Strategy: <span className="font-medium uppercase tracking-wider text-paper">{r.strategies.map((s) => STRAT_LABEL[s]).join(' + ')}</span>
          </p>
        )}
      </section>
      <section aria-labelledby="yf-path" className="panel p-5">
        <SectionHead id="yf-path" title="Why did I get this answer?">
          The decision path, gate by gate. Each answer shows the inputs behind it and where they came from. Nothing is hidden in a score.
        </SectionHead>
        <DecisionTree path={r.path} />
      </section>
      <div className="grid gap-8 lg:grid-cols-2">
        <section aria-labelledby="yf-gaps">
          <SectionHead id="yf-gaps" title="Evidence gaps">
            Unknown is a valid answer. Pivotal gaps could change the yard; the rest only the confidence.
          </SectionHead>
          <EvidenceGaps result={r} onFill={(input) => go('assess', { ...state, focus: e.subject.key, step: stepOfInput(input as InputId) })} />
        </section>
        <section aria-labelledby="yf-change">
          <SectionHead id="yf-change" title="What would change the answer?" />
          <ChangeConditions result={r} />
        </section>
      </div>
      <section aria-labelledby="yf-posture">
        <SectionHead id="yf-posture" title="Recommended policy posture" />
        <PolicyPosture result={r} />
        <p className="yf-no-print mt-4 text-sm">
          <Link className="link" to={href('fence')}>
            Build your fence: test a policy package against this diagnosis
          </Link>
        </p>
      </section>
      {comp && (
        <section aria-labelledby="yf-curated" className="panel p-5">
          <SectionHead id="yf-curated" title="About this component" />
          <p className="text-sm leading-relaxed text-paper-dim">{comp.notes}</p>
          <p className="mt-2 text-xs text-paper-mute">
            Curated assessment, {comp.confidence} confidence. Source: {comp.source}
          </p>
          {comp.paperPlacement && (
            <p className="mt-3 flex flex-wrap items-center gap-2 text-sm text-paper-dim">
              The paper’s provisional placement: <YardBadge yard={comp.paperPlacement} size="sm" />
              {comp.paperPlacement !== r.yard && <span className="text-paper-mute">— this assessment differs; the inputs above say why.</span>}
            </p>
          )}
        </section>
      )}
      <section aria-label="Inputs and data" className="space-y-3">
        <Disclosure summary="Every input and where it came from (data, assessment, your judgment)">
          <Assumptions e={e} />
        </Disclosure>
        <Disclosure summary="The data behind this assessment (context from the Opportunity & Security Indices)">
          <EvidencePanel subject={e.subject} ev={e.evidence} reference={data.reference} compact />
        </Disclosure>
      </section>
    </div>
  );
}

export default function ResultView() {
  useTitle('Result');
  const { state, subjects, focus, evaluate, update, href, data } = useYards();
  const evals = useMemo(() => subjects.map((s) => evaluate(s)), [subjects, evaluate]);
  // closed <details> do not print: open every one of them for the printout
  useEffect(() => {
    const open = () => document.querySelectorAll<HTMLDetailsElement>('.yf details').forEach((d) => (d.open = true));
    window.addEventListener('beforeprint', open);
    return () => window.removeEventListener('beforeprint', open);
  }, []);
  if (!state.sel || !focus) return <NoAssessment />;
  const e = evals.find((x) => x.subject.key === focus.key) ?? evals[0];
  const stack = data.reference.stacks.find((s) => s.id === state.sel?.stack);
  const ex = exampleById(state.example);
  const date = new Date().toISOString().slice(0, 10);
  return (
    <div className={PAGE}>
      <div className="yf-print-only mb-6 text-sm">
        <strong>Draw the Fence — Yards &amp; Fences assessment</strong> · {data.bundle.country.name} · printed {date} · data: Sector bundle {data.manifest.inputs.sector_created.slice(0, 10)}, network build {data.manifest.inputs.network_build}
        <div className="break-all text-xs">{typeof window !== 'undefined' ? window.location.href : ''}</div>
      </div>
      {ex && (
        <p className="yf-no-print mb-6 border-l-2 border-signal pl-3 text-sm text-paper-dim">
          Example: <strong className="font-medium text-paper">{ex.title}</strong>. {ex.summary} Its own assumptions are marked YOUR JUDGMENT; change any of them in{' '}
          <Link className="link" to={href('assess')}>
            Assess
          </Link>{' '}
          or{' '}
          <Link className="link" to={href('scenario')}>
            Test another world
          </Link>
          .
        </p>
      )}
      {stack && (
        <section aria-labelledby="yf-stack" className="mb-12">
          <div className="mb-4 flex flex-wrap items-center gap-4">
            <SectionHead id="yf-stack" eyebrow={`${stack.technology} — ${stack.stage} · system`} title={stack.label} />
            <span className="ml-auto">
              <YardBadge yard={stackOutcome(evals.map((x) => x.scenarioResult))} sub="The system as a whole" />
            </span>
          </div>
          <ComponentStack
            title={stack.label}
            layers={evals.map((x) => ({ key: x.subject.key, label: x.subject.label, layer: x.subject.component?.layer ?? 'stage', result: x.scenarioResult }))}
            focusKey={focus.key}
            onFocus={(k) => update((s) => ({ ...s, focus: k }))}
          />
          <p className="mt-3 text-sm text-paper-mute">Select a layer to see its full reasoning below.</p>
        </section>
      )}
      <FullResult e={e} />
      <div className="yf-no-print mt-10 flex flex-wrap gap-3 border-t hairline pt-5">
        <Link className="btn btn-ghost" to={href('assess')}>
          Change answers
        </Link>
        <Link className="btn btn-ghost" to={href('scenario')}>
          Test another world
        </Link>
        <Link className="btn btn-ghost" to={href('fence')}>
          Build your fence
        </Link>
        <Link className="btn btn-ghost" to={href('supply')}>
          Trusted supply
        </Link>
        <button type="button" className="btn btn-quiet ml-auto" onClick={() => window.print()}>
          Print / Save as PDF
        </button>
      </div>
    </div>
  );
}
