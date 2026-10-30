// Sections 1-6 of the Country profile (spec XI-XVI, XIX; design 4, 5b, D1, D8, D14; 2026-10-29): the canonical
// framework, Need -> Prize -> Position -> Choice -> Policy Alignment, then the allies that meet the needs.
//   1 Strategy (#strategy)            the Country Strategy Map (posture/CountryStrategyMap) with its counts
//                                     line, legend, table twin and CSV
//   2 Need (#need)                    "What problems does this country need to solve?": supply-chain need by
//                                     node and deployment need by technology (posture/NeedPanels); the energy-
//                                     system context cards (emissions, transition, IEA pathway) in a disclosure
//   3 Prize & Position (#prize-position) Industrial Opportunity x Competitive Position (PrizePositionScatter)
//                                     and the monthly trade behind the position's momentum (TradeMonthlyCard)
//   4 Choices (#choices)              Strategic Choices, one group per verb (StrategicChoices)
//   5 Policy Alignment (#policy-alignment) "Does current policy match the strategy?" (PolicyAlignmentTable)
//                                     with the observed-policy evidence beneath it (climate rating, IEA records,
//                                     Global Trade Alert measures)
//   6 Allied Strategy (#allied)       the canonical partner outputs and AIB status (AlliedStrategy), and the
//                                     generic partnership material (Partnerships, Role in the Allied
//                                     Industrial Base) under a collapsed "Explore all partnership analysis"
// Every entry point (map cell, Need row, scatter point, choice card, policy row, allied node) selects through
// the page's one ?node= selection (ProfileCtx.posture.node), which opens the one drawer the page mounts. None
// of these sections reads the Perspective dials or the in-browser engine: the framework's thresholds are
// fixed, and the Legacy OSI blocks that follow the dials sit in Evidence & Methodology.

import type { ReactNode } from 'react';
import { Notice } from '../../../components/ui.tsx';
import { isProducingStage } from '../../../lib/stages.ts';
import {
  ALLIED_STRATEGY_TITLE, AlliedStrategy, CountryPostureIntro, CountryStrategyMap, EXPLORE_PARTNERSHIPS, NEED_HEADING, NeedPanels, PolicyAlignmentTable, PostureBasisTag,
  PrizePositionScatter, StrategicChoices,
} from '../../../posture/index.ts';
import { CountryClimateCard } from '../../../sector/views/climate/CountryClimate.tsx';
import { CountryTransitionCards } from '../../../sector/views/climate/CountryTransition.tsx';
import { postureSectorLink } from '../../derive/links.ts';
import { cellKeyOfNode, EXPLORE_FOLD, nodeKeyOfCell, STRATEGY_SECTIONS } from '../../derive/strategyPage.ts';
import { policyBehaviour } from '../../derive/framing.ts';
import { cap, poss, proseName } from '../../derive/text.ts';
import { countryHref } from '../../url.ts';
import { AibRoleSection } from './AibRoleSection.tsx';
import { BasisTag, Fold, Section, useProfile } from './common.tsx';
import { PartnershipsSection } from './PartnersSection.tsx';
import { NipoCard, PolicyCoverage } from './PrioritiesSection.tsx';
import { TradeMonthlyCard } from './TradeMonthlyCard.tsx';

const SEC = Object.fromEntries(STRATEGY_SECTIONS.map((s) => [s.id, s])) as Record<string, (typeof STRATEGY_SECTIONS)[number]>;

/**
 * The basis tag of sections 1-6: the framework's label and "fixed thresholds". The posture tag's default note
 * names the dials and the scenario toggle, which on this page live in Legacy OSI (Explore scenarios) and in the
 * Allied explore fold; the canonical strategy's own sections do not name them (the tag's tooltip still says so).
 */
const BASIS_NOTE = 'fixed thresholds';
const Basis = () => <PostureBasisTag note={BASIS_NOTE} />;

/** The AIB view of a producing-stage node (the allied division by technology and stage); null at Deployment & Use. */
export const aibNodeHref = (_tech: string, stage: string): string | null => (isProducingStage(stage) ? '/aib#division' : null);

/**
 * The framework's state for a section, in words, or null when the section can draw: loading, an error with
 * Retry, or an economy the framework does not classify (D1: never shown as Watch or zero).
 */
function usePostureState(what: string): ReactNode {
  const { sheet, posture: p } = useProfile();
  const name = proseName(sheet.meta.country);
  if (!p || p.status === 'loading') return <p className="text-sm text-paper-mute">Loading the strategic position framework…</p>;
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
        The strategic position framework classifies none of {poss(name)} technology and stage nodes, so there is no {what} to show.
      </Notice>
    );
  }
  return null;
}

// ---- 1 Strategy ----------------------------------------------------------------------------------------

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
      <p className="max-w-3xl text-sm leading-relaxed text-paper-dim" data-testid="strategy-how">
        <strong className="font-medium text-paper">How the map is built.</strong> <CountryPostureIntro name={name} />
      </p>
    </Section>
  );
}

// ---- 2 Need --------------------------------------------------------------------------------------------

export function NeedSection() {
  const { sheet, posture: p } = useProfile();
  const name = proseName(sheet.meta.country);
  const state = usePostureState('need ranking');
  return (
    <Section
      id="need"
      title={SEC.need.label}
      basis={<Basis />}
      intro={
        <>
          <strong className="font-medium text-paper">{NEED_HEADING}</strong> Need comes in two kinds that are never added together. At the five producing stages it is supply-chain
          need: how costly or dangerous dependence on outside supply is for the node (import dependence, supplier concentration, risky supplier exposure, trusted alternatives). At
          Deployment &amp; Use it is deployment need: how much more of the technology the energy system needs (demand adequacy, the gap to national targets, system requirements), each
          with its scenario named. Select a row for the node&apos;s five questions.
        </>
      }
    >
      {state ?? (p && p.core && <NeedPanels core={p.core} cells={p.cells} detail={p.detail} name={cap(name)} selected={p.node.selected} onOpen={p.node.select} />)}
      <Fold id="energy-context" summary={`Energy-system context: ${poss(name)} emissions, transition indicators and the IEA pathway of its region`}>
        <CountryTransitionCards iso3={sheet.iso3} name={name} />
        <CountryClimateCard iso3={sheet.iso3} name={name} />
      </Fold>
    </Section>
  );
}

// ---- 3 Prize & Position --------------------------------------------------------------------------------

export function PrizePositionSection() {
  const { sheet, posture: p, setCell } = useProfile();
  const name = proseName(sheet.meta.country);
  const state = usePostureState('prize and position chart');
  // the monthly trade card follows the selected node (its fact-sheet sector), else the page's sector
  const nodeCell = cellKeyOfNode(p?.node.selected);
  const pick = (k: string) => {
    const c = sheet.cells.find((x) => x.key === k);
    if (p && c && p.core?.meta.techs.includes(c.tech)) p.node.select(nodeKeyOfCell(c));
    else setCell(k);
  };
  return (
    <Section
      id="prize-position"
      title={SEC['prize-position'].label}
      basis={<Basis />}
      intro={
        <>
          Where is there a prize worth pursuing, and where is {name} already strong? Industrial Opportunity (the node&apos;s market prize) runs across, Competitive Position (demonstrated
          capability among the node&apos;s contenders) up. Need and Opportunity triage a node; Competitive Position decides the industrial response, and a Secure node stays at its true
          place, outlined. The quadrant words are tendencies, not rules.
        </>
      }
    >
      {state ?? (p && p.core && <PrizePositionScatter core={p.core} cells={p.cells} name={cap(name)} selected={p.node.selected} onOpen={p.node.select} />)}
      <TradeMonthlyCard cell={nodeCell} onPick={pick} />
    </Section>
  );
}

// ---- 4 Choices -----------------------------------------------------------------------------------------

export function ChoicesSection() {
  const { sheet, posture: p } = useProfile();
  const name = proseName(sheet.meta.country);
  const state = usePostureState('list of choices');
  return (
    <Section
      id="choices"
      title={SEC.choices.label}
      basis={<Basis />}
      intro={
        <>
          What {name} should secure, preserve, catch up in, rely on partners for and deploy faster, and what it should deliberately not prioritise. Watch is a conclusion, not a
          failure: domestic industrial priority is low there. Deployment priority is stated apart from domestic manufacturing priority.
        </>
      }
    >
      {state ?? (p && p.core && <StrategicChoices core={p.core} cells={p.cells} detail={p.detail} name={cap(name)} selected={p.node.selected} onOpen={p.node.select} aibHref={aibNodeHref} />)}
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
        <h4 className="text-base font-medium text-paper">Observed policy in brief</h4>
        <ul className="mt-2 list-disc space-y-1 pl-5 text-sm text-paper-dim">
          {lines.map((t) => (
            <li key={t}>{t}</li>
          ))}
        </ul>
        <p className="mt-3 text-xs text-paper-mute">Observed policy is a comparator of the strategy, never an input to it.</p>
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
          Does {poss(name)} current policy match the strategy? Each assessed node&apos;s choice beside its observed policy: under-prioritized (a Secure, Preserve or Catch Up node with
          weak support, or an Accelerate node with weak deployment policy), over-prioritized (heavy protection where the choice is Watch), wrong treatment (localisation where the
          choice is Partner). No overall score; where policy data are thin, the alignment says so.
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

export function AlliedSection() {
  const { sheet, posture: p, manifest, state: url } = useProfile();
  const name = proseName(sheet.meta.country);
  const state = usePostureState('allied strategy');
  const partnerHref = (iso: string): string | null => {
    const e = manifest.countries.find((c) => c.iso3 === iso);
    return e && e.status !== 'no_data' ? countryHref(iso, url.scn ? { scn: url.scn } : {}, 'allied') : null;
  };
  return (
    <Section
      id="allied"
      aliases={SEC.allied.aliases}
      title={SEC.allied.label}
      basis={
        <>
          <Basis />
          <BasisTag kind="facts" note="the Allied Industrial Base results" />
        </>
      }
      intro={
        <>
          Which of {poss(name)} needs allies should meet: every Partner node, and every Secure node whose delivery leans on trusted foreign capacity, with the recommended partners, why
          they are recommended and whether the Allied Industrial Base covers the node. Where no trusted supplier can cover a need yet, it says so.
        </>
      }
    >
      {state ?? (p && p.core && <AlliedStrategy core={p.core} cells={p.cells} detail={p.detail} name={name} title={`${ALLIED_STRATEGY_TITLE}: need, partners, why and AIB status`} selected={p.node.selected} onOpen={p.node.select} partnerHref={partnerHref} aibHref={aibNodeHref} />)}
      <div data-testid="allied-explore">
        <Fold id={EXPLORE_FOLD} summary={EXPLORE_PARTNERSHIPS}>
          <PartnershipsSection id="allied-partnerships" level={3} />
          <AibRoleSection id="allied-aib-role" level={3} />
        </Fold>
      </div>
    </Section>
  );
}
