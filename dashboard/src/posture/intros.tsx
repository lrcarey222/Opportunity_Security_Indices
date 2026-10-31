// The words a page's posture Section can open with (optional; the pages own their Sections). Phrasing
// content only (no <p>, no <dl>): the Country Section renders its intro inside a <p>. Each says what the
// block recommends, what it reads and that the dials and the scenario do not change it, with the posture
// glossary terms (Term shows the words alone until content/glossary.ts has the entries).

import { Term } from '../components/Term.tsx';
import { POSTURE_TERMS } from './basis.tsx';

/**
 * The Country Strategy section intro (spec I, VIII; the NEIS copy refresh of 2026-10-30: the three questions in
 * plain words first, the measures' names as glossary terms); `name` as it reads mid-sentence ("the United States").
 */
export function CountryPostureIntro({ name }: { name: string }) {
  return (
    <>
      For every technology at each of the six supply-chain stages, the analysis asks three questions: what problem {name} has to solve (
      <Term id={POSTURE_TERMS.supplyChainNeed}>supply-chain vulnerability</Term>, or <Term id={POSTURE_TERMS.deploymentNeed}>deployment need</Term> at Deployment &amp; Use),
      whether productive capability there is worth having (<Term id={POSTURE_TERMS.strategicValue}>Strategic Industrial Value</Term>) and how strong {name} already is (
      <Term id={POSTURE_TERMS.competitivePosition}>Competitive Position</Term>), then recommends one choice: Secure, Preserve, Catch Up, Partner, Accelerate or Watch.
      Its thresholds are fixed, not a weighting of preferences; current policy is compared with the result afterwards and never feeds it.
    </>
  );
}

/** The Sector posture Section intro: the one prescriptive block of the Sector pages. */
export function SectorPostureIntro() {
  return (
    <>
      Unlike the measured values on the rest of these pages, this is a recommendation: for one technology at one stage, the strategic position framework places every economy
      by <Term id={POSTURE_TERMS.strategicValue}>Strategic Industrial Value</Term> and <Term id={POSTURE_TERMS.supplyChainNeed}>supply-chain need</Term> and recommends one
      choice: Secure, Preserve, Catch Up, Partner or Watch (at Deployment &amp; Use, Accelerate or Watch on <Term id={POSTURE_TERMS.deploymentNeed}>deployment need</Term>). Its
      thresholds are fixed.
    </>
  );
}

/** The AIB Overview's division Section intro: it does not follow the scenario toggle. */
export function AibDivisionIntro() {
  return (
    <>
      For every technology at each stage that makes or supplies something, who among trusted allies holds the supply, who backs them up and where the allied base has a{' '}
      <Term id={POSTURE_TERMS.aibGap}>gap</Term>, against how many countries&apos; posture asks partners for it (<Term id={POSTURE_TERMS.partnerDemand}>Partner demand</Term>).
      It follows the strategic position framework, not the scenario toggle: switching scenarios does not change it.
    </>
  );
}
