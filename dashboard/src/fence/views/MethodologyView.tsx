// /methodology: the framework, the rules (rendered from engine/rules.ts, so the page
// cannot drift from the code), the data and its vintages, the validation the build ran, the metric
// map and the curated component table with sources and confidence.

import { useTitle } from '../title.ts';
import { Disclosure } from '../../components/ui.tsx';
import { ALL_INPUT_IDS, explainOf, INPUT_DEFS, RULES } from '../engine/index.ts';
import type { InputDef } from '../engine/index.ts';
import { useYards } from '../state/YardsContext.tsx';
import { SectionHead, YardBadge, ProvBadge } from '../components/kit.tsx';
import { METHOD_STATEMENT, PAGE } from '../FenceLayout.tsx';
import { MIXED_YARD_NOTE, YARD_DEFINITION } from '../../content/vocabulary.ts';

const GLOSSARY: { gate: InputDef['gate']; title: string }[] = [
  { gate: 'gate1', title: 'Gate 1 · Acute harm' },
  { gate: 'digital', title: 'Gate 2A · Digitally active systems' },
  { gate: 'physical', title: 'Gate 2B · Physical chokepoints' },
  { gate: 'capability', title: 'Gate 3 · Capability worth building' },
  { gate: 'leap', title: 'Gate 3 · Is a leap ahead credible?' },
  { gate: 'cost', title: 'Gate 4 · The cost of protection' },
];

const SAFEGUARDS = [
  'Headline Energy Security and Economic Opportunity scores are never mapped to yards.',
  'A percentile is never treated as an absolute resilience threshold.',
  'Raw measures are shown wherever they are interpretable, with source and year.',
  'Where data are unavailable the tool says so; nothing is imputed from unrelated indicators.',
  'Restricted (BNEF, BCG) data cannot enter: the build checks the Sector bundle for them.',
  'Normalised OSI components are shown as relative ranks and never feed the engine.',
  'The optional coupled-pillar methodology is not used.',
  'Technology-stage evidence is labelled as such; it is never presented as component-level.',
  'Forced-labour, safety, environmental rules and legitimate trade remedies apply in every yard; they do not decide yard placement.',
];

export default function MethodologyView() {
  useTitle('Methodology');
  const { data } = useYards();
  const m = data.manifest;
  const ref = data.reference;
  return (
    <div className={PAGE}>
      <SectionHead eyebrow="Methodology" title="How Draw the Fence decides">
        {METHOD_STATEMENT}
      </SectionHead>

      <section className="mt-8 grid gap-5 md:grid-cols-3" aria-label="The three tests">
        <article className="panel p-5">
          <YardBadge yard="small" />
          <p className="mt-3 text-sm leading-relaxed text-paper">{YARD_DEFINITION.small}</p>
          <p className="mt-2 text-sm leading-relaxed text-paper-dim">
            Requires an <strong className="text-paper">acute consequence</strong> (high or severe) <strong className="text-paper">and</strong> either (A) a control layer an outsider could command, push updates to or disable, with harm within months; or (B) a physical input where a potentially coercive supplier controls at least {RULES.coercion.yes * 100}% of supply, substitution is hard, and trusted supply could not be restored before harm becomes intolerable, net of stockpiles. Separability decides how much of the product the fence covers.
          </p>
        </article>
        <article className="panel p-5">
          <YardBadge yard="medium" />
          <p className="mt-3 text-sm leading-relaxed text-paper">{YARD_DEFINITION.medium}</p>
          <p className="mt-2 text-sm leading-relaxed text-paper-dim">
            If the small-yard test is not met: at least one of a present position (home share of at least {RULES.position.strong * 100}% of world supply, or {RULES.position.moderate * 100}% backed by complementary capabilities), an industrial-learning case, or a credible leap-ahead case. A strong case (learning strong, or two rationales) is medium; a single moderate rationale is medium unless the domestic premium exceeds {RULES.premium.high}%.
          </p>
        </article>
        <article className="panel p-5">
          <YardBadge yard="large" />
          <p className="mt-3 text-sm leading-relaxed text-paper">{YARD_DEFINITION.large}</p>
          <p className="mt-2 text-sm leading-relaxed text-paper-dim">
            Where neither case holds. A moderate capability case with a high premium becomes a large yard with a specified capability floor; concentrated supply adds diversification to the posture, not a high fence.
          </p>
        </article>
      </section>

      <p className="mt-4 max-w-3xl text-sm leading-relaxed text-paper-mute">{MIXED_YARD_NOTE}</p>

      <section className="mt-10" aria-labelledby="yf-unknown">
        <SectionHead id="yf-unknown" title="Unknowns, confidence and completeness" />
        <div className="max-w-3xl space-y-3 text-sm leading-relaxed text-paper-dim">
          <p>
            Every question accepts Unknown. The engine never fills an unknown: it evaluates the gates with every unknown at the open end and at the security end of its range (and likewise for the capability questions). If all four corners agree, the yard is settled; if not, the result is <em>indeterminate</em> and lists the yards still possible. A gap is <em>pivotal</em> when resolving it alone could change the set of possible yards.
          </p>
          <p>
            <strong className="text-paper">Classification confidence</strong> and <strong className="text-paper">evidence completeness</strong> are separate. Completeness is the share of relevant inputs that are known. {RULES.confidence.reason}
          </p>
        </div>
      </section>

      <section className="mt-10" aria-labelledby="yf-prov">
        <SectionHead id="yf-prov" title="Data, assessment, judgment" />
        <ul className="grid max-w-4xl gap-3 sm:grid-cols-2">
          <li className="text-sm text-paper-dim"><ProvBadge p="data" /> measured, from the Opportunity &amp; Security Indices bundles, with source and year.</li>
          <li className="text-sm text-paper-dim"><ProvBadge p="assessment" /> a curated NEIS Yards &amp; Fences assumption, with its source and confidence.</li>
          <li className="text-sm text-paper-dim"><ProvBadge p="judgment" /> an answer you supplied (examples supply some, all editable).</li>
          <li className="text-sm text-paper-dim"><ProvBadge p="scenario" /> an assumption set in the scenario lab; never part of the observed baseline.</li>
          <li className="text-sm text-paper-dim"><ProvBadge p="unknown" /> evidence is insufficient; bracketed, not guessed.</li>
        </ul>
      </section>

      <section className="mt-10" aria-labelledby="yf-glossary">
        <SectionHead id="yf-glossary" title="Glossary of assumptions">
          What each question means, in plain language. The same explanations sit behind the <span className="yf-info pointer-events-none">i</span> beside every assumption across the tool.
        </SectionHead>
        <div className="grid gap-x-10 gap-y-8 lg:grid-cols-2">
          {GLOSSARY.map((g) => (
            <div key={g.gate}>
              <h3 className="eyebrow mb-3">{g.title}</h3>
              <dl className="space-y-3">
                {ALL_INPUT_IDS.filter((id) => INPUT_DEFS[id].gate === g.gate).map((id) => (
                  <div key={id} id={`term-${id}`}>
                    <dt className="text-sm font-medium text-paper">{INPUT_DEFS[id].label}</dt>
                    <dd className="mt-0.5 text-sm leading-relaxed text-paper-dim">{explainOf(id)}</dd>
                  </div>
                ))}
              </dl>
            </div>
          ))}
        </div>
      </section>

      <section className="mt-10" aria-labelledby="yf-thresholds">
        <SectionHead id="yf-thresholds" title="Thresholds and why they exist">
          Screening conventions, not standards. They are kept in one file (engine/rules.ts); this table is rendered from it.
        </SectionHead>
        <table className="w-full max-w-4xl text-sm">
          <tbody>
            {[
              ['Coercive concentration', `largest non-coalition supplier ≥ ${RULES.coercion.yes * 100}% (flagged near threshold from ${RULES.coercion.marginal * 100}%)`, RULES.coercion.reason],
              ['Concentration (descriptive only)', `HHI ≥ ${RULES.concentration.high} high; ≥ ${RULES.concentration.moderate} moderate`, RULES.concentration.reason],
              ['Timing', `restore time > harm horizon + stockpile cover (near threshold within ${RULES.timingMarginalMonths} months)`, RULES.timingReason],
              ['Present position', `home share ≥ ${RULES.position.strong * 100}% strong; ≥ ${RULES.position.moderate * 100}% with complementary capabilities`, RULES.position.reason],
              ['Cost of protection', `premium > ${RULES.premium.high}% high; ≥ ${RULES.premium.moderate}% moderate`, RULES.premium.reason],
            ].map(([k, v, why]) => (
              <tr key={k} className="border-b hairline align-top">
                <th scope="row" className="py-2.5 pr-3 text-left font-medium text-paper">
                  {k}
                </th>
                <td className="py-2.5 pr-3 text-paper-dim">{v}</td>
                <td className="py-2.5 text-xs leading-relaxed text-paper-mute">{why}</td>
              </tr>
            ))}
          </tbody>
        </table>
        <p className="mt-2 text-xs text-paper-mute">
          Months assumed per answer band — time to harm: {Object.entries(RULES.months.timeHorizon).map(([k, v]) => `${k} ${v}`).join(', ')}; qualification: {Object.entries(RULES.months.qualificationTime).map(([k, v]) => `${k} ${v}`).join(', ')}; reconstitution: {Object.entries(RULES.months.reconstitutionTime).map(([k, v]) => `${k} ${v}`).join(', ')}; stockpile: {Object.entries(RULES.months.stockpile).map(([k, v]) => `${k} ${v}`).join(', ')}.
        </p>
      </section>

      <section className="mt-10" aria-labelledby="yf-safe">
        <SectionHead id="yf-safe" title="Safeguards" />
        <ul className="max-w-3xl list-disc space-y-1.5 pl-5 text-sm leading-relaxed text-paper-dim">
          {SAFEGUARDS.map((s) => (
            <li key={s}>{s}</li>
          ))}
        </ul>
      </section>

      <section className="mt-10" aria-labelledby="yf-data">
        <SectionHead id="yf-data" title="The data">
          Built by {m.generator} from the published Sector and Strategic bundles (so every number matches what those modules show) and the allied network outputs.
        </SectionHead>
        <dl className="grid max-w-4xl gap-x-6 gap-y-2 text-sm sm:grid-cols-2">
          <dt className="text-paper-mute">Built</dt>
          <dd className="text-paper">{m.created}</dd>
          <dt className="text-paper-mute">Sector bundle</dt>
          <dd className="text-paper">{m.inputs.sector_bundle_id.slice(0, 8)} ({m.inputs.sector_created})</dd>
          <dt className="text-paper-mute">Strategic bundle</dt>
          <dd className="text-paper">{m.inputs.strategic_bundle_id.slice(0, 8)} ({m.inputs.strategic_created})</dd>
          <dt className="text-paper-mute">Allied network build</dt>
          <dd className="text-paper">{m.inputs.network_build}</dd>
          <dt className="text-paper-mute">Pipeline</dt>
          <dd className="text-paper">{m.inputs.pipeline_marker}</dd>
        </dl>
        <ul className="mt-4 max-w-3xl space-y-1 text-sm text-paper-dim">
          {m.notes.map((n) => (
            <li key={n}>{n}</li>
          ))}
        </ul>
        <Disclosure summary={`Validation checks run by the build (${m.validation.filter((v) => v.ok).length} of ${m.validation.length} passed)`}>
          <ul className="space-y-1 text-sm">
            {m.validation.map((v) => (
              <li key={v.check} className={v.ok ? 'text-paper-dim' : 'text-alert'}>
                {v.ok ? '✓' : '✗'} {v.check}
              </li>
            ))}
          </ul>
        </Disclosure>
        <Disclosure summary={`Metric map (${ref.metrics.length} metrics)`}>
          <table className="w-full text-sm">
            <tbody>
              {ref.metrics.map((x) => (
                <tr key={x.id} className="border-b hairline align-top">
                  <td className="py-2 pr-3 text-paper">{x.label.replace(/\{[a-z]+\}/g, '…')}</td>
                  <td className="py-2 pr-3 text-paper-dim">{ref.concepts[x.concept]?.label ?? x.concept}</td>
                  <td className="py-2 text-xs text-paper-mute">{x.normalized ? 'normalised, display only' : x.engineInput ? `feeds the engine: ${x.engineInput}` : 'display only'}</td>
                </tr>
              ))}
            </tbody>
          </table>
        </Disclosure>
        <Disclosure summary={`Sources (${ref.sources.length})`}>
          <ul className="space-y-1 text-sm text-paper-dim">
            {ref.sources.map((s) => (
              <li key={s.id}>
                {s.short}
                {s.edition ? ` (${s.edition})` : ''}
                {s.licence ? <span className="text-paper-mute"> · licence: {s.licence}</span> : null}
              </li>
            ))}
          </ul>
        </Disclosure>
      </section>

      <section className="mt-10" aria-labelledby="yf-comp">
        <SectionHead id="yf-comp" title="Curated components">
          The table OSI cannot provide: data/reference/yards_fences_assessments.csv. Values the public record does not support are left Unknown.
        </SectionHead>
        <div className="overflow-x-auto">
          <table className="w-full min-w-[44rem] text-sm">
            <thead>
              <tr className="border-b hairline text-left">
                <th className="label py-2">Component</th>
                <th className="label py-2">Layer</th>
                <th className="label py-2">Evidence scope</th>
                <th className="label py-2">Paper</th>
                <th className="label py-2">Confidence</th>
                <th className="label py-2">Source</th>
              </tr>
            </thead>
            <tbody>
              {ref.components.map((c) => (
                <tr key={c.id} className="border-b hairline align-top">
                  <td className="py-2 pr-3 text-paper">
                    {c.label}
                    <span className="block text-xs text-paper-mute">
                      {c.technology} — {c.stage}
                    </span>
                  </td>
                  <td className="py-2 pr-3 text-paper-dim">{c.layer}</td>
                  <td className="py-2 pr-3 text-xs text-paper-dim">{c.evidence ? `${c.evidence.scope}: ${c.evidence.scopeId}` : 'none'}</td>
                  <td className="py-2 pr-3">{c.paperPlacement ? <YardBadge yard={c.paperPlacement} size="sm" /> : <span className="text-paper-mute">—</span>}</td>
                  <td className="py-2 pr-3 text-paper-dim">{c.confidence}</td>
                  <td className="py-2 text-xs text-paper-mute">{c.source}</td>
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      </section>

      <section className="mt-10 max-w-3xl" aria-labelledby="yf-paper">
        <SectionHead id="yf-paper" title="The framework" />
        <p className="text-sm leading-relaxed text-paper-dim">
          NEIS Center, <em>Clean Energy Industrial Strategy: Three Yards for Three Different Problems</em> (discussion draft, October 2026), extending the &ldquo;small yard, high fence&rdquo; idea with work by Brookings (criticality), Benich, Stewart and Krejsa (control layers), Matisek (remediation metrics), RMI&rsquo;s Secure, Competitive, Global, Pisano and Shih and Fuchs (the industrial commons), Carnegie and Zoffer (leapfrogging) and Deese and UC San Diego (selective openness).
        </p>
      </section>
    </div>
  );
}
