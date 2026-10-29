// The words a page's "Strategic posture" Section can open with (optional; the pages own their Sections).
// Phrasing content only (no <p>, no <dl>): the Country Section renders its intro inside a <p>. Each says
// what the block recommends, what it reads and that the dials and the scenario do not change it, with the
// posture glossary terms (Term shows the words alone until content/glossary.ts has the entries).

import { Term } from '../components/Term.tsx';
import { POSTURE_TERMS } from './basis.tsx';

/** The Country profile's posture Section intro; `name` as it reads mid-sentence ("the United States"). */
export function CountryPostureIntro({ name }: { name: string }) {
  return (
    <>
      For every technology at each of the six supply-chain stages, the strategic position framework recommends whether {name} should secure the node, compete in it (preserve
      an advantage, catch up, or partner with trusted suppliers) or deploy openly. It reads <Term id={POSTURE_TERMS.strategicNeed}>Strategic Need</Term> and{' '}
      <Term id={POSTURE_TERMS.industrialOpportunity}>Industrial Opportunity</Term> at fixed thresholds, so the Perspective dials do not change it.
    </>
  );
}

/** The Sector posture Section intro: the one prescriptive block of the Sector pages. */
export function SectorPostureIntro() {
  return (
    <>
      Unlike the measured values on the rest of these pages, this is a recommendation: for one technology at one stage, the strategic position framework places every economy
      by <Term id={POSTURE_TERMS.industrialOpportunity}>Industrial Opportunity</Term> and <Term id={POSTURE_TERMS.strategicNeed}>Strategic Need</Term> and recommends whether it
      should secure the node, compete in it or deploy openly. Its thresholds are fixed.
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
