// The reasoning reveal (docs/QC_UX_REVIEW.md section 7), shown only once the blind answer is stored: the
// reviewer's own answer, "Explorer conclusion: X" with the one-sentence summary, the analytical chain step by step
// (each step's level and one sentence: Need / Prize / Position for a country case, exposure / gap / concentration
// for an allied step, classes / materiality for a compact), "Therefore: X" with the decision rule in plain
// English, the tailored remainder of the product (the rest of a compact's causal chain, a partnership's
// coordination changes and memo choices, a sector chain's stages with the Explorer's first stage and the
// chokepoint view), "Why this, not the alternative?", then levels 2 and 3 of the analysis. All of it reads
// snapshot.reasoning (qc/reasoning.ts); a case built without it falls back to the conclusion and the trace.

import type { ReactNode } from 'react';
import { stageOfDisplay } from '../../../lib/stages.ts';
import { NOT_ASSESSED_HATCH, VERB_COLOR } from '../../../posture/grammar.ts';
import type { Verb } from '../../../posture/contract.ts';
import type { ReviewCase, ReviewMode } from '../../model.ts';
import { AlternativesPanel } from './AlternativesPanel.tsx';
import { ChainList } from './CaseBrief.tsx';
import { InspectAnalysisLevel, WhyThisResultLevel } from './DisclosureLevels.tsx';
import { StageStrip } from './StageStrip.tsx';

const isVerb = (v: string): v is Verb => Object.prototype.hasOwnProperty.call(VERB_COLOR, v);
export const swatchOf = (v: string): string | undefined => (isVerb(v) ? VERB_COLOR[v] : v === 'not_assessed' ? NOT_ASSESSED_HATCH : undefined);

/** The stage keys of a list of stage display names (or keys). */
const stageKeys = (xs: readonly string[]): string[] => xs.map((x) => stageOfDisplay(x) ?? x);

export interface ReasoningRevealProps {
  c: ReviewCase;
  /** "Your answer: ..." (built by the parent from the stored blind answer). */
  yourAnswer: ReactNode;
  mode: ReviewMode;
  campaign?: string | null;
}

export function ReasoningReveal({ c, yourAnswer, mode, campaign = null }: ReasoningRevealProps) {
  const k = c.snapshot.conclusion;
  const r = c.snapshot.reasoning ?? null;
  const sw = swatchOf(k.value);
  const label = k.label;
  const briefChainLen = c.snapshot.brief?.compact_chain?.length ?? 0;
  const technical = mode === 'technical';
  return (
    <div className="space-y-6" data-testid="reasoning-reveal">
      <p className="text-sm text-paper-dim" data-testid="your-answer">
        {yourAnswer}
      </p>

      <section className="panel border-l-2 border-l-signal px-4 py-4" aria-labelledby="conclusion-h" data-testid="conclusion">
        <h3 id="conclusion-h" className="flex flex-wrap items-center gap-2 text-2xl font-medium text-paper">
          <span className="text-base font-normal text-paper-dim">Explorer conclusion:</span>
          {sw && <span aria-hidden="true" className="inline-block h-4 w-4 flex-none" style={{ background: sw }} />}
          <span>{label}</span>
        </h3>
        {r?.conclusion.summary && (
          <p className="mt-2 max-w-3xl text-base text-paper" data-testid="conclusion-summary">
            {r.conclusion.summary}
          </p>
        )}
        {k.detail && <p className="mt-1 text-sm text-paper-dim">{k.detail}</p>}
        {/* the Explorer's own words (its trace text or the export's reason), verbatim and marked as a quote */}
        {k.reason && (
          <p className="mt-2 text-sm text-paper-dim" data-testid="conclusion-reason">
            In the Explorer&rsquo;s words: <q>{k.reason}</q>
          </p>
        )}
        {k.confidence && <p className="mt-1 text-xs text-paper-mute">Confidence in the call: {k.confidence}</p>}
      </section>

      {r && r.steps.length > 0 && (
        <section aria-labelledby="chain-h" data-testid="reasoning-chain">
          <h4 id="chain-h" className="mb-2 text-base font-medium text-paper">
            How the Explorer got there
          </h4>
          <ol className="space-y-1">
            {r.steps.map((st, i) => (
              <li key={st.id} data-step={st.id}>
                <div className="panel px-4 py-2.5">
                  <div className="flex flex-wrap items-baseline gap-x-2">
                    <span className="text-sm font-medium uppercase tracking-wider text-paper-dim">{st.label}</span>
                    <span className="text-sm font-medium text-paper">{st.level}</span>
                  </div>
                  <p className="mt-0.5 text-sm text-paper-dim">{st.sentence}</p>
                </div>
                {i < r.steps.length - 1 && (
                  <span className="block text-center text-paper-mute" aria-hidden="true">
                    ↓
                  </span>
                )}
              </li>
            ))}
          </ol>
          <div className="panel mt-2 border-l-2 border-l-signal px-4 py-3" data-testid="therefore">
            <div className="text-base font-medium text-paper">Therefore: {label}</div>
            <p className="mt-0.5 text-sm text-paper-dim">{r.rule}</p>
          </div>
        </section>
      )}

      {r?.compact_chain && r.compact_chain.length > 0 && (
        <section aria-labelledby="chain-rest-h" className="space-y-2" data-testid="compact-chain-rest">
          <h4 id="chain-rest-h" className="text-base font-medium text-paper">
            The rest of the chain: what the compact would do
          </h4>
          <ChainList steps={r.compact_chain} label="The rest of the chain" testid="reveal-compact-chain" startAt={briefChainLen + 1} />
        </section>
      )}

      {r?.partnership && (
        <section aria-labelledby="coord-h" className="space-y-3" data-testid="partnership-reveal">
          {r.partnership.coordination_changes.length > 0 && (
            <div className="panel px-4 py-3">
              <h4 id="coord-h" className="text-base font-medium text-paper">
                What coordination changes
              </h4>
              <ul className="mt-1 list-disc space-y-0.5 pl-5 text-sm text-paper-dim">
                {r.partnership.coordination_changes.map((x) => (
                  <li key={x}>{x}</li>
                ))}
              </ul>
            </div>
          )}
          <div>
            <h4 className="mb-2 text-base font-medium text-paper">The memo&rsquo;s choices</h4>
            <div className="grid gap-3 md:grid-cols-2">
              {r.partnership.memo_choices.map((m) => (
                <section key={m.kind} className="panel px-4 py-3" data-memo-box={m.kind} aria-label={m.label} style={m.kind === k.value ? { borderColor: 'var(--color-signal)' } : undefined}>
                  <div className="eyebrow">
                    {m.label}
                    {m.kind === k.value ? ' (leads)' : ''}
                  </div>
                  {m.items.length ? (
                    <ul className="mt-1 list-disc space-y-0.5 pl-5 text-sm text-paper-dim">
                      {m.items.map((x) => (
                        <li key={x}>{x}</li>
                      ))}
                    </ul>
                  ) : (
                    <p className="mt-1 text-sm italic text-paper-mute">Nothing in this box.</p>
                  )}
                </section>
              ))}
            </div>
          </div>
        </section>
      )}

      {r?.sector && (
        <section aria-labelledby="sector-h" className="space-y-2" data-testid="sector-reveal">
          <h4 id="sector-h" className="text-base font-medium text-paper">
            Where the Explorer would intervene
          </h4>
          <StageStrip
            location={c.snapshot.brief?.stage_location ?? null}
            label="The Explorer's stages"
            highlightWord="in this chain"
            marked={stageKeys(r.sector.explorer_stages)}
            markedWord="needs allied action"
            lead={r.sector.lead_stage ? (stageOfDisplay(r.sector.lead_stage) ?? r.sector.lead_stage) : null}
            leadWord="acts here first"
            testid="sector-reveal-strip"
          />
          {r.sector.chokepoint_note && (
            <p className="text-sm text-paper-dim" data-testid="chokepoint-note">
              Chokepoint view: {r.sector.chokepoint_note}
            </p>
          )}
        </section>
      )}

      {r && <AlternativesPanel conclusion={label} alternatives={r.alternatives} />}

      <div>
        <WhyThisResultLevel c={c} defaultOpen={technical} campaign={campaign} />
        <InspectAnalysisLevel c={c} defaultOpen={technical} />
      </div>
    </div>
  );
}

export default ReasoningReveal;
