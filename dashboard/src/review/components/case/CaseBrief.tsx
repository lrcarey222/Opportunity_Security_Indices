// The neutral case brief (docs/QC_UX_REVIEW.md section 3; NEIS brief "reviewers are blind to the Explorer's
// conclusion, not to the information needed to form one"). Rendered from snapshot.brief only (qc/brief.ts built
// and linted it): identity, the decision question, the decision context, the objective, the time horizon, the
// scenario, the six-stage strip, the case-level data warnings, then the tailored section of the product, the
// evidence (5-8 facts with reading, source and vintage), the analytical assumptions (visibly apart), what the
// data cannot show and the definitions. Nothing here reads the conclusion, the reasoning or any reveal field.
// Tailored layouts:
//   country      evidence grouped as supply / domestic capability / alternatives / market (or deployment)
//   sector       per-stage facts for a sector chain; the stage strip for a single step
//   partnership  "A brings / B brings" with each side's exposures, then the shared problem
//   compact      the first links of the causal chain (problem today -> existing trade -> remaining gap); the rest
//                of the chain is shown only after the blind answer

import type { BriefFact, CaseBrief as Brief, ChainStep, PartnerSide } from '../../model.ts';
import { AssumptionsPanel, Definitions, EvidencePanel, FactRow, MissingEvidence, type FactGroup } from './EvidencePanel.tsx';
import { StageStrip } from './StageStrip.tsx';
import { WarningList } from './WarningList.tsx';

/** Country evidence groups (neutral headings: no model term such as need, prize or position). */
const COUNTRY_GROUPS: readonly [string, readonly string[]][] = [
  ['Where supply comes from', ['largest_supplier', 'china_import_share', 'rival_group_import_share', 'import_scale']],
  ['Domestic capability', ['own_world_share', 'export_position', 'export_trend', 'technology_readiness']],
  ['Alternative suppliers', ['major_supplier_world_share', 'same_group_world_share']],
  ['Market outlook', ['demand_outlook']],
  ['Deployment today', ['installed_trend']],
  ['Demand and targets', ['deployment_demand', 'target_gap']],
  ['The energy system', ['system_need', 'climate_context']],
];

/** The evidence in display groups: country cases by topic, every other product as one list. */
export function evidenceGroups(brief: Pick<Brief, 'product' | 'evidence'>): FactGroup[] {
  const ev = brief.evidence ?? [];
  if (brief.product !== 'country') return [{ title: null, facts: ev }];
  const used = new Set<string>();
  const out: FactGroup[] = [];
  for (const [title, ids] of COUNTRY_GROUPS) {
    const facts = ev.filter((f) => ids.includes(f.id));
    for (const f of facts) used.add(f.id);
    if (facts.length) out.push({ title, facts });
  }
  const rest = ev.filter((f) => !used.has(f.id));
  if (rest.length) out.push({ title: out.length ? 'Other evidence' : null, facts: rest });
  return out;
}

/** One causal chain, link by link, each estimate labelled as one. */
export function ChainList({ steps, label, testid, startAt = 1 }: { steps: readonly ChainStep[]; label: string; testid: string; startAt?: number }) {
  if (!steps.length) return null;
  return (
    <ol className="space-y-1.5" aria-label={label} data-testid={testid} start={startAt}>
      {steps.map((s, i) => (
        <li key={s.id} data-chain={s.id} className="panel px-3 py-2">
          <div className="flex flex-wrap items-baseline gap-x-2">
            <span className="text-xs text-paper-mute tabular" aria-hidden="true">
              {startAt + i}.
            </span>
            <span className="text-sm font-medium text-paper">{s.label}</span>
            {s.estimate && (
              <span className="tag text-[0.6875rem]" title="A model estimate, not an observation">
                Estimate
              </span>
            )}
          </div>
          <div className="mt-0.5 text-sm text-paper tabular">{s.display}</div>
          {s.text && <p className="mt-0.5 text-xs text-paper-dim">{s.text}</p>}
          {i < steps.length - 1 && (
            <span className="block text-center text-paper-mute" aria-hidden="true">
              ↓
            </span>
          )}
        </li>
      ))}
    </ol>
  );
}

function Side({ side, testid }: { side: PartnerSide; testid: string }) {
  return (
    <section className="panel px-4 py-3" aria-labelledby={`${testid}-h`} data-testid={testid}>
      <h4 id={`${testid}-h`} className="text-sm font-medium text-paper">
        {side.name} brings
      </h4>
      {side.brings.length ? (
        <ul className="mt-1 list-disc space-y-0.5 pl-5 text-sm text-paper-dim">
          {side.brings.map((b) => (
            <li key={b}>{b}</li>
          ))}
        </ul>
      ) : (
        <p className="mt-1 text-sm italic text-paper-mute">No large-supplier step in this release.</p>
      )}
      <div className="mt-2 text-xs font-medium uppercase tracking-wider text-paper-mute">{side.name} depends on one supplier for</div>
      {side.exposed_on.length ? (
        <ul className="mt-1 list-disc space-y-0.5 pl-5 text-sm text-paper-dim">
          {side.exposed_on.map((b) => (
            <li key={b}>{b}</li>
          ))}
        </ul>
      ) : (
        <p className="mt-1 text-sm italic text-paper-mute">No single-supplier dependence in this release.</p>
      )}
    </section>
  );
}

function SectorStages({ stages }: { stages: NonNullable<Brief['sector_chain']> }) {
  const withFacts = stages.filter((s) => s.facts.length);
  const without = stages.filter((s) => !s.facts.length).map((s) => s.label);
  return (
    <section aria-labelledby="brief-stages-h" data-testid="brief-sector-stages">
      <h4 id="brief-stages-h" className="mb-1 text-sm font-medium text-paper">
        The chain stage by stage
      </h4>
      <div className="grid gap-3 lg:grid-cols-2">
        {withFacts.map((s) => (
          <section key={s.stage} className="panel px-4 py-2" data-stage-facts={s.stage} aria-label={s.label}>
            <div className="eyebrow">{s.label}</div>
            <dl className="divide-y divide-white/10">
              {s.facts.map((f: BriefFact) => (
                <FactRow key={f.id} f={f} />
              ))}
            </dl>
          </section>
        ))}
      </div>
      {without.length > 0 && <p className="mt-2 text-xs text-paper-mute">This chain has no products at: {without.join(', ')}.</p>}
    </section>
  );
}

export interface CaseBriefProps {
  brief: Brief;
  campaign?: string | null;
  /** Show the identity line (the page head may show it already). */
  showIdentity?: boolean;
}

export function CaseBrief({ brief, campaign = null, showIdentity = true }: CaseBriefProps) {
  const b = brief;
  const chainStageWord = b.sector_chain ? 'covered by this chain' : b.product === 'compact' ? 'in this chain' : 'this case';
  return (
    <div className="space-y-5" data-testid="case-brief" data-product={b.product}>
      <section aria-labelledby="brief-decision-h" className="space-y-3">
        {showIdentity && (
          <p className="text-xs uppercase tracking-wider text-paper-mute" data-testid="brief-identity">
            {b.identity.title}
          </p>
        )}
        <h3 id="brief-decision-h" className="max-w-3xl text-lg font-medium text-paper" data-testid="brief-question">
          {b.decision_question}
        </h3>
        <p className="max-w-3xl text-sm text-paper-dim" data-testid="brief-context">
          {b.decision_context}
        </p>
        <dl className="grid gap-3 sm:grid-cols-3" data-testid="brief-frame">
          <div className="panel px-3 py-2">
            <dt className="eyebrow">Objective</dt>
            <dd className="mt-1 text-sm text-paper">{b.objective}</dd>
          </div>
          <div className="panel px-3 py-2" data-testid="brief-horizon">
            <dt className="eyebrow">Time horizon</dt>
            <dd className="mt-1 text-sm text-paper">{b.time_horizon.label}</dd>
            {b.time_horizon.note && <dd className="mt-0.5 text-xs text-paper-mute">{b.time_horizon.note}</dd>}
          </div>
          <div className="panel px-3 py-2" data-testid="brief-scenario">
            <dt className="eyebrow">Scenario</dt>
            <dd className="mt-1 text-sm text-paper">{b.scenario ? b.scenario.label : 'Today’s network'}</dd>
            {b.scenario?.description && <dd className="mt-0.5 text-xs text-paper-mute">{b.scenario.description}</dd>}
          </div>
        </dl>
      </section>

      {b.stage_location && <StageStrip location={b.stage_location} label="Where this case sits in the supply chain" highlightWord={chainStageWord} />}

      {b.warnings.length > 0 && (
        <section aria-labelledby="brief-warn-h">
          <h4 id="brief-warn-h" className="mb-1 text-sm font-medium text-paper">
            Data warnings
          </h4>
          <WarningList warnings={b.warnings} />
        </section>
      )}

      {b.partnership && (
        <section aria-labelledby="brief-pair-h" data-testid="brief-partnership" className="space-y-3">
          <h4 id="brief-pair-h" className="text-sm font-medium text-paper">
            What each side brings
          </h4>
          <div className="grid gap-3 md:grid-cols-2">
            <Side side={b.partnership.a} testid="brief-side-a" />
            <Side side={b.partnership.b} testid="brief-side-b" />
          </div>
          {b.partnership.shared_problem.length > 0 && (
            <section className="panel px-4 py-3" aria-labelledby="brief-shared-h" data-testid="brief-shared-problem">
              <h4 id="brief-shared-h" className="text-sm font-medium text-paper">
                Shared strategic problem
              </h4>
              <ul className="mt-1 list-disc space-y-0.5 pl-5 text-sm text-paper-dim">
                {b.partnership.shared_problem.map((p) => (
                  <li key={p}>{p}</li>
                ))}
              </ul>
            </section>
          )}
        </section>
      )}

      {b.compact_chain && b.compact_chain.length > 0 && (
        <section aria-labelledby="brief-chain-h" className="space-y-2">
          <h4 id="brief-chain-h" className="text-sm font-medium text-paper">
            The problem, step by step
          </h4>
          <ChainList steps={b.compact_chain} label="The problem, step by step" testid="brief-compact-chain" />
          <p className="text-xs text-paper-mute">The rest of the chain (new capacity, who could add it, the proposed coalition and the expected resilience after) is shown after your answer.</p>
        </section>
      )}

      {b.sector_chain && b.sector_chain.length > 0 && <SectorStages stages={b.sector_chain} />}

      <div className="grid gap-4 lg:grid-cols-[minmax(0,3fr)_minmax(0,2fr)]">
        <EvidencePanel groups={evidenceGroups(b)} />
        <div className="space-y-4">
          <AssumptionsPanel assumptions={b.assumptions} campaign={campaign} />
          <MissingEvidence items={b.missing_evidence} />
        </div>
      </div>
      <Definitions items={b.definitions} />
    </div>
  );
}

export default CaseBrief;
