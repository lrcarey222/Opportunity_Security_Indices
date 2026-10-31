// Draw the Fence: the parts of a result. The headline, the reasons (and what did NOT drive it),
// the decision path as an expandable tree, the evidence gaps, what would change the answer, and the
// policy posture that follows from the diagnosis.

import type { ReactNode } from 'react';
import { optionLabel, YARD_NAME } from '../engine/index.ts';
import type { DecisionNode, InputRef, YardResult } from '../engine/index.ts';
import { TOOLKITS } from '../content/policy.ts';
import { InputTip } from './InfoTip.tsx';
import { Completeness, ConfidencePill, ProvBadge, YARD_COPY, YardBadge, yardClass } from './kit.tsx';

export function ResultHeadline({ title, result, context }: { title: string; result: YardResult; context?: ReactNode }) {
  const y = result.yard;
  return (
    <section className={`${yardClass(y)} panel yf-yard-edge yf-flip p-6`} aria-live="polite">
      <div className="eyebrow mb-2">{context ?? 'Result'}</div>
      <h2 className="text-2xl font-medium uppercase tracking-[0.06em] text-paper lg:text-3xl">{title}</h2>
      <div className="mt-4 flex flex-wrap items-center gap-x-8 gap-y-4">
        <YardBadge yard={y} size="lg" sub={result.scopeLabels?.[0] ? `${YARD_COPY[y].fence}: ${result.scopeLabels[0]}` : undefined} />
        <dl className="flex flex-wrap gap-x-8 gap-y-3">
          <div>
            <dt className="label mb-1">Confidence in this call</dt>
            <dd>
              <ConfidencePill c={result.confidence} />
            </dd>
          </div>
          <div>
            <dt className="label mb-1">Evidence completeness</dt>
            <dd>
              <Completeness value={result.evidenceCompleteness} />
            </dd>
          </div>
        </dl>
      </div>
      {y === 'indeterminate' && (
        <p className="mt-4 text-sm text-paper-dim">
          Still possible: <strong className="font-medium text-paper">{result.possible.map((p) => YARD_NAME[p]).join(', ')}</strong>
          {result.leaning && (
            <>
              . With unproven tests treated as not met, it leans <strong className="font-medium text-paper">{YARD_NAME[result.leaning]}</strong>.
            </>
          )}
        </p>
      )}
      <div className="mt-5 border-t hairline pt-4">
        <div className="label">What to do</div>
        <p className="text-lg font-medium text-paper">{result.recommendedPosture}</p>
        <p className="mt-1 max-w-3xl text-sm leading-relaxed text-paper-dim">{result.postureDetail}</p>
      </div>
    </section>
  );
}

export function Reasons({ result }: { result: YardResult }) {
  const drivers = result.reasons.filter((r) => r.kind === 'driver');
  const context = result.reasons.filter((r) => r.kind === 'context');
  const notBy = result.reasons.filter((r) => r.kind === 'not-by');
  return (
    <div className="grid gap-6 md:grid-cols-2">
      <div>
        <h3 className="label">This result is driven by</h3>
        <ul className="space-y-2">
          {drivers.map((r, i) => (
            <li key={i} className="flex gap-2 text-sm leading-relaxed text-paper-dim">
              <span aria-hidden="true" className="text-paper">✓</span>
              <span>{r.text}</span>
            </li>
          ))}
          {context.map((r, i) => (
            <li key={`c${i}`} className="flex gap-2 text-sm leading-relaxed text-paper-dim">
              <span aria-hidden="true" className="text-paper-mute">•</span>
              <span>{r.text}</span>
            </li>
          ))}
          {drivers.length + context.length === 0 && <li className="text-sm text-paper-mute">No decisive test was settled.</li>}
        </ul>
      </div>
      <div>
        <h3 className="label">Not by</h3>
        <ul className="space-y-2">
          {notBy.map((r, i) => (
            <li key={i} className="flex gap-2 text-sm leading-relaxed text-paper-dim">
              <span aria-hidden="true" className="text-paper-mute">×</span>
              <span>{r.text.replace(/^Not (by )?/, (m) => m)}</span>
            </li>
          ))}
        </ul>
      </div>
    </div>
  );
}

const ANSWER: Record<string, string> = { yes: 'YES', no: 'NO', unknown: 'UNKNOWN', 'n/a': 'N/A' };

function InputChips({ inputs }: { inputs: InputRef[] }) {
  if (!inputs.length) return null;
  return (
    <ul className="mt-2 flex flex-wrap gap-2">
      {inputs.map((i) => (
        <li key={i.id} className="tag gap-2">
          <span className="inline-flex items-center">
            {labelOf(i)}
            <InputTip id={i.id} />
          </span>
          <ProvBadge p={i.provenance} />
        </li>
      ))}
    </ul>
  );
}

function labelOf(i: InputRef): string {
  return `${shortLabel(i.id)}: ${optionLabel(i.id, i.value)}`;
}

const SHORT: Record<string, string> = {
  consequence: 'Consequence',
  timeHorizon: 'Time to harm',
  digitallyActive: 'Digital',
  remoteControl: 'External control',
  separability: 'Separable',
  substitutability: 'Substitution',
  qualificationTime: 'Qualification',
  reconstitutionTime: 'Reconstitution',
  stockpile: 'Stocks',
  surgeCapacity: 'Surge',
  nonAlliedTopShare: 'Largest non-coalition share',
  homeShare: 'Home share',
  processKnowledge: 'Process knowledge',
  workforceSuppliers: 'Skills & suppliers',
  adjacentInnovation: 'Adjacent innovation',
  complementaryCapabilities: 'Complementary',
  costPerfAdvantage: 'Advantage',
  incumbentVulnerable: 'Incumbent displaceable',
  scalePathway: 'Path to scale',
  beyondNovelty: 'Beyond novelty',
  premium: 'Premium',
};
const shortLabel = (id: string) => SHORT[id] ?? id;

function TreeNode({ n }: { n: DecisionNode }) {
  const ans = n.answer;
  if (n.outcome) {
    return (
      <li className="yf-tree-node pb-1 pt-2">
        <span className="yf-tree-dot" aria-hidden="true" />
        <div className={`${yardClass(n.outcome)} inline-flex items-center gap-2`}>
          <YardBadge yard={n.outcome} size="sm" />
          <span className="text-sm text-paper-dim">{n.detail}</span>
        </div>
      </li>
    );
  }
  return (
    <li className="yf-tree-node pb-3">
      <span className="yf-tree-dot" aria-hidden="true" />
      <details className="disclosure" open={ans === 'yes' || ans === 'unknown'}>
        <summary className="flex cursor-pointer flex-wrap items-baseline gap-x-3 py-1.5 text-sm text-paper">
          <span>{n.question}</span>
          <strong className={`yf-ans-${ans === 'n/a' ? 'no' : ans} tracking-[0.1em]`}>{ANSWER[ans]}</strong>
          {n.marginal && <span className="tag">near a threshold</span>}
        </summary>
        <div className="pb-1 text-sm leading-relaxed text-paper-dim">
          {n.detail}
          <InputChips inputs={n.inputs} />
        </div>
        {n.children && n.children.length > 0 && (
          <ul className="yf-tree mt-2">
            {n.children.map((c) => (
              <TreeNode key={c.id} n={c} />
            ))}
          </ul>
        )}
      </details>
    </li>
  );
}

export function DecisionTree({ path }: { path: DecisionNode[] }) {
  return (
    <ul className="yf-tree" aria-label="Decision path">
      {path.map((n) => (
        <TreeNode key={n.id} n={n} />
      ))}
    </ul>
  );
}

export function EvidenceGaps({ result, onFill }: { result: YardResult; onFill?: (input: string) => void }) {
  if (!result.missingEvidence.length) return <p className="text-sm text-paper-mute">No relevant input is unknown.</p>;
  return (
    <ul className="space-y-3">
      {result.missingEvidence.map((m) => (
        <li key={m.input} className={`border-l-2 pl-3 ${m.pivotal ? 'border-signal' : 'border-ink-600'}`}>
          <div className="flex flex-wrap items-center gap-2">
            <span className="inline-flex items-center text-sm font-medium text-paper">
              {m.label}
              <InputTip id={m.input} />
            </span>
            {m.pivotal ? <span className="tag tag-framework">pivotal</span> : <span className="tag">confidence only</span>}
            {onFill && (
              <button type="button" className="link text-xs" onClick={() => onFill(m.input)}>
                Answer it
              </button>
            )}
          </div>
          <p className="mt-1 text-sm leading-relaxed text-paper-dim">
            {m.pivotal
              ? `We cannot settle the yard while ${m.label.toLowerCase()} is unknown. ${m.effect.replace(/^Pivotal: /, '')}`
              : m.effect}
          </p>
        </li>
      ))}
    </ul>
  );
}

export function ChangeConditions({ result }: { result: YardResult }) {
  const specific = result.changeConditions.filter((c) => !c.generic);
  const generic = result.changeConditions.filter((c) => c.generic);
  return (
    <div className="space-y-4">
      {specific.length > 0 && (
        <ul className="space-y-2">
          {specific.map((c, i) => (
            <li key={i} className={`${yardClass(c.toward)} flex items-start gap-3 text-sm leading-relaxed text-paper-dim`}>
              <span className="mt-0.5 shrink-0">
                <YardBadge yard={c.toward} size="sm" />
              </span>
              <span>{c.text}</span>
            </li>
          ))}
        </ul>
      )}
      {generic.length > 0 && (
        <ul className="space-y-1 border-t hairline pt-3">
          {generic.map((c, i) => (
            <li key={i} className="text-sm leading-relaxed text-paper-mute">
              {c.text}
            </li>
          ))}
        </ul>
      )}
      <p className="text-xs text-paper-mute">Yard placement is conditional: vulnerabilities can be repaired, industries can become competitive, technologies can commoditise.</p>
    </div>
  );
}

export function PolicyPosture({ result }: { result: YardResult }) {
  const key = result.yard === 'indeterminate' || result.yard === 'mixed' ? 'indeterminate' : result.variant;
  const t = TOOLKITS[key];
  return (
    <div className="grid gap-6 md:grid-cols-2">
      <div>
        <h3 className="label">Tools consistent with this diagnosis</h3>
        <ul className="space-y-1.5">
          {t.recommended.map((x) => (
            <li key={x} className="flex gap-2 text-sm leading-relaxed text-paper-dim">
              <span aria-hidden="true" className="text-paper">+</span>
              {x}
            </li>
          ))}
        </ul>
      </div>
      <div>
        <h3 className="label">Don’t automatically use</h3>
        <ul className="space-y-1.5">
          {t.avoid.map((x) => (
            <li key={x} className="flex gap-2 text-sm leading-relaxed text-paper-dim">
              <span aria-hidden="true" className="text-paper-mute">−</span>
              {x}
            </li>
          ))}
        </ul>
      </div>
    </div>
  );
}

export function Warnings({ result }: { result: YardResult }) {
  if (!result.warnings.length) return null;
  return (
    <ul className="space-y-2">
      {result.warnings.map((w) => (
        <li key={w} className="block-interpretation text-sm leading-relaxed">
          {w}
        </li>
      ))}
    </ul>
  );
}
