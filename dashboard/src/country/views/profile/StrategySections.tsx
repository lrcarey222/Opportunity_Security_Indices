// Sections 1, 5 and 6 of the Country profile (spec XI-XVI, XIX; design 4, 5b, D1, D8, D14; 2026-10-29; the
// fences of 2026-10-30), and the pieces the fence sections share:
//   1 Strategy (#strategy)            the Country Strategy Map (posture/CountryStrategyMap) with its legend by
//                                     fence, table twin and CSV
//   2-4 the High, Medium and Low fence sections (FenceSections.tsx), which replace Need, Prize & Position and
//                                     Choices
//   5 Policy Alignment (#policy-alignment) "Does current policy match the strategy?" (PolicyAlignmentTable)
//                                     with the observed-policy evidence beneath it (climate rating, IEA records,
//                                     Global Trade Alert measures)
//   6 Allied Strategy (#allied)       the compacts in which the economy holds a role (AlliedCompacts.tsx)
// The NEIS copy refresh (2026-10-30): the h2s are the strategic questions of the shared vocabulary
// (derive/strategyPage.ts); under the map, the three yards in the user's words (vocabulary YARD_DEFINITION),
// so a first-time reader meets the three policy problems before the method.
// Every entry point (map cell, fence entry or row, scatter point, policy row) selects through the page's one
// ?node= selection (ProfileCtx.posture.node), which opens the one drawer the page mounts. None of these
// sections reads the Perspective dials or the in-browser engine: the framework's thresholds are fixed, and the
// Legacy OSI blocks that follow the dials sit in Evidence & Methodology.

import type { ReactNode } from 'react';
import { Notice } from '../../../components/ui.tsx';
import {
  ALIGNMENT_NAME, FRAMING, MIXED_YARD_NOTE, POLICY_ALIGNMENT_INTRO, YARD_AND_FENCE, YARD_CHOICES, YARD_DEFINITION, YARDS_NOT_A_SCALE,
} from '../../../content/vocabulary.ts';
import { CountryPostureIntro, CountryStrategyMap, PolicyAlignmentTable, PostureBasisTag, VERB_OUTLINE, YARD_COLOR, YARD_ORDER } from '../../../posture/index.ts';
import { postureSectorLink } from '../../derive/links.ts';
import { STRATEGY_SECTIONS } from '../../derive/strategyPage.ts';
import { policyBehaviour } from '../../derive/framing.ts';
import { cap, poss, proseName } from '../../derive/text.ts';
import { AlliedCompactsSection } from './AlliedCompacts.tsx';
import { BasisTag, Fold, Section, useProfile } from './common.tsx';
import { NipoCard, PolicyCoverage } from './PrioritiesSection.tsx';

const SEC = Object.fromEntries(STRATEGY_SECTIONS.map((s) => [s.id, s])) as Record<string, (typeof STRATEGY_SECTIONS)[number]>;

/**
 * The basis tag of sections 1-6: the framework's label and "fixed thresholds". The posture tag's default note
 * names the dials and the scenario toggle, which on this page live in Legacy OSI (Explore scenarios) and in the
 * Allied explore fold; the canonical strategy's own sections do not name them (the tag's tooltip still says so).
 */
const BASIS_NOTE = 'fixed thresholds';
export const Basis = () => <PostureBasisTag note={BASIS_NOTE} />;

/**
 * The framework's state for a section, in words, or null when the section can draw: loading, an error with
 * Retry, or an economy the framework does not classify (D1: never shown as Watch or zero).
 */
export function usePostureState(what: string): ReactNode {
  const { sheet, posture: p } = useProfile();
  const name = proseName(sheet.meta.country);
  if (!p || p.status === 'loading') return <p className="text-sm text-paper-mute">Loading the strategy…</p>;
  if (p.status === 'error' || !p.core) {
    return (
      <Notice kind="warning" title="The strategy data could not be loaded" action={<button type="button" className="btn btn-ghost btn-sm" onClick={p.retry}>Retry</button>}>
        {p.error ?? 'Unknown error.'}
      </Notice>
    );
  }
  if (!p.cells.length) {
    return (
      <Notice kind="info" title="No strategy map">
        There is not enough comparable evidence to produce a strategy map for {name}, so there is no {what} to show.
      </Notice>
    );
  }
  return null;
}

// ---- 1 Strategy ----------------------------------------------------------------------------------------

/** The three yards in the user's words (vocabulary): what each fence of the map's legend means, as a policy problem. */
export function YardsExplainer() {
  return (
    <div className="max-w-4xl" data-testid="strategy-yards">
      <p className="text-sm text-paper-dim">
        <strong className="font-medium text-paper">Three yards, three different policy problems.</strong> {FRAMING.noCountryMakesEverything} {FRAMING.whatNotToBuild}
      </p>
      <ul className="mt-3 grid gap-3 md:grid-cols-3" aria-label="The three yards">
        {YARD_ORDER.map((y) => (
          <li key={y} className="card p-3 text-sm" data-yard={y}>
            <span className="flex items-center gap-2">
              <span
                aria-hidden="true"
                className="inline-block h-3 w-3 shrink-0 rounded-[3px]"
                style={{ background: YARD_COLOR[y], border: y === 'large' ? `1px solid ${VERB_OUTLINE.watch}` : undefined }}
              />
              <span className="font-medium text-paper">{YARD_AND_FENCE[y]}</span>
            </span>
            <span className="mt-0.5 block text-xs text-paper-mute">{YARD_CHOICES[y]}</span>
            <span className="mt-1.5 block text-paper-dim">{YARD_DEFINITION[y]}</span>
          </li>
        ))}
      </ul>
      <p className="mt-2 text-xs text-paper-mute">
        {YARDS_NOT_A_SCALE} {MIXED_YARD_NOTE}
      </p>
    </div>
  );
}

export function StrategySection() {
  const { sheet, posture } = useProfile();
  const name = proseName(sheet.meta.country);
  const iso = sheet.iso3;
  const node = posture?.node;
  // spec XI.3: the map is the dominant above-the-fold visual, so the section opens on it (its own footnote says
  // to select a node) and the full reading of how the map is built follows it
  return (
    <Section id="strategy" aliases={SEC.strategy.aliases} title={SEC.strategy.label} basis={<Basis />}>
      <CountryStrategyMap
        iso3={iso}
        name={cap(name)}
        {...(node ? { selected: node.selected, onSelect: node.select } : {})}
        drawer={!node}
        showSentence={false}
        sectorHref={(tech, stage) => postureSectorLink(tech, stage, iso)}
      />
      <YardsExplainer />
      <p className="max-w-3xl text-sm leading-relaxed text-paper-dim" data-testid="strategy-how">
        <strong className="font-medium text-paper">How the map is built.</strong> <CountryPostureIntro name={name} />
      </p>
    </Section>
  );
}

// ---- 5 Policy Alignment --------------------------------------------------------------------------------

/** The observed-policy evidence beneath the alignment table: the climate rating and record counts, the IEA records, the measures. */
function PolicyEvidence() {
  const { sheet } = useProfile();
  const name = proseName(sheet.meta.country);
  const lines = policyBehaviour(sheet, name, { withSi: false });
  return (
    <div className="space-y-4">
      <div className="card p-5" data-testid="policy-behaviour">
        <h4 className="text-base font-medium text-paper">What government is doing, in brief</h4>
        <ul className="mt-2 list-disc space-y-1 pl-5 text-sm text-paper-dim">
          {lines.map((t) => (
            <li key={t}>{t}</li>
          ))}
        </ul>
        <p className="mt-3 text-xs text-paper-mute">Policy is compared with the strategy afterwards; it never feeds it.</p>
      </div>
      <Fold
        id="policy-records"
        summary={
          sheet.policy.nipo?.staged === false
            ? `Policy records by technology and stage, and ${poss(name)} active industrial-policy measures`
            : `Policy records by technology and stage, and the strongest industrial-policy measures in ${poss(name)} sectors`
        }
      >
        <PolicyCoverage />
        <NipoCard />
      </Fold>
    </div>
  );
}

export function PolicyAlignmentSection() {
  const { sheet, posture: p } = useProfile();
  const name = proseName(sheet.meta.country);
  const state = usePostureState('policy alignment');
  return (
    <Section
      id="policy-alignment"
      aliases={SEC['policy-alignment'].aliases}
      title={SEC['policy-alignment'].label}
      basis={
        <>
          <Basis />
          <BasisTag kind="facts" note="policy records, measures, climate rating" />
        </>
      }
      intro={
        <>
          {POLICY_ALIGNMENT_INTRO} Each assessed step&apos;s choice sits beside the policy observed for it in {name}.{' '}
          <strong className="font-medium text-paper">{ALIGNMENT_NAME.under_prioritized}</strong>: a Secure, Preserve or Catch Up step with weak support, or a technology the energy
          system needs deployed faster with weak deployment policy. <strong className="font-medium text-paper">{ALIGNMENT_NAME.over_prioritized}</strong>: heavy protection or
          localization where the choice is Watch. <strong className="font-medium text-paper">{ALIGNMENT_NAME.misaligned}</strong>: localization where the choice is Partner. There is
          no overall score; where policy records are thin, the section says so.
        </>
      }
    >
      {state ? (
        <>
          {state}
          <PolicyEvidence />
        </>
      ) : (
        p &&
        p.core && (
          <PolicyAlignmentTable core={p.core} cells={p.cells} detail={p.detail} name={cap(name)} selected={p.node.selected} onOpen={p.node.select}>
            <PolicyEvidence />
          </PolicyAlignmentTable>
        )
      )}
    </Section>
  );
}

// ---- 6 Allied Strategy ---------------------------------------------------------------------------------

/** The compacts in which the economy holds a role (the AIB results; the framework is not needed). */
export function AlliedSection() {
  return <AlliedCompactsSection />;
}
