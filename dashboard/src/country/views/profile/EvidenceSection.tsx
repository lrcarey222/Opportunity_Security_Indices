// Section 7 of the Country profile, Evidence & Methodology (spec XVII, XVIII; design 4, 5b, D10; 2026-10-29):
// how the strategy is decided (the chain Need -> Prize -> Position -> Choice -> Policy Alignment, with the
// fixed thresholds), the six stages and which need each reads, the economy's data coverage, evidence
// specificity and sensitivity (nodes near a threshold), the trade benchmark and the current trade momentum
// cut-off, the data freshness, the score components, the methodology documents and raw data, the data notes
// and downloads, and the Legacy OSI framework sub-section (#legacy-osi): the Energy Security and Economic
// Opportunity indices, the four corners and the Strategic Index, clearly marked as not a current
// recommendation, with the conceptual mapping (Grow ~ Preserve, Upgrade ~ Catch Up, Exposed ~ Partner, Stable
// ~ Watch), why the results differ, and the Perspective dials as "Explore scenarios". The Legacy OSI results
// sit in a closed disclosure; the in-browser engine mounts only around them, so nothing above them follows
// the dials.
// The NEIS copy refresh (2026-10-30): the section and the framework card open with the shared vocabulary's
// words (EVIDENCE_INTRO, STRATEGY_EXPLAINER, LEGACY_OSI_NOTE); the framework card describes the current rules
// (the 0.10 need floor, the capability-anchor Catch Up test, the large-yard case); visible copy says
// "supply-chain step" for a node.

import type { ReactNode } from 'react';
import { Link } from 'react-router';
import { Term } from '../../../components/Term.tsx';
import { Notice } from '../../../components/ui.tsx';
import { EVIDENCE_INTRO, LEGACY_OSI_NOTE, MEASURE_PLAIN, STATE, STRATEGY_EXPLAINER } from '../../../content/vocabulary.ts';
import { STAGE_DISPLAY, SUPPLY_STAGES } from '../../../lib/stages.ts';
import { POSTURE_TERMS, PostureBasisTag } from '../../../posture/index.ts';
import { EngineScope } from '../../../shell/EngineScope.tsx';
import { sectorCountryLink } from '../../derive/links.ts';
import { legacyCornerLine } from '../../derive/model.ts';
import { coverageCounts, LEGACY_FOLD, LEGACY_MAPPING_ROWS, LEGACY_MIXES, LEGACY_SECTION, STRATEGY_SECTIONS } from '../../derive/strategyPage.ts';
import { dateLabel } from '../../derive/strategyPage.ts';
import { cap, ledeOf, poss, proseName } from '../../derive/text.ts';
import { FRESHNESS_NOTE } from './Header.tsx';
import { DataNotes } from './MemoNotes.tsx';
import { OpportunitiesSection } from './OpportunitiesSection.tsx';
import { PartnerScoresFold } from './PartnersSection.tsx';
import { EngineSi, PerspectiveControl } from './Perspective.tsx';
import { LegacyPolicyAgreement } from './PrioritiesSection.tsx';
import { SectorPositionSection } from './SectorPositionSection.tsx';
import { SpaceSection } from './SpaceSection.tsx';
import { BasisTag, Fold, Section, useProfile } from './common.tsx';

const EVIDENCE = STRATEGY_SECTIONS.find((s) => s.id === 'evidence')!;

/** The column group of a stage on the Country Strategy Map (spec XI.5). */
const STAGE_GROUP: Record<string, string> = {
  Extraction: 'Supply-chain strategy', Refining: 'Supply-chain strategy', Components: 'Supply-chain strategy', Assembly: 'Supply-chain strategy',
  Deployment: 'Energy-system strategy', Recovery: 'Circularity',
};

const pct = (v: number | null | undefined) => (v === null || v === undefined ? '–' : `${Math.round(v * 100)}`);

function Row({ term, children }: { term: ReactNode; children: ReactNode }) {
  return (
    <div className="grid gap-1 border-t hairline py-2 sm:grid-cols-[14rem_minmax(0,1fr)] sm:gap-4">
      <dt className="text-sm text-paper-mute">{term}</dt>
      <dd className="text-sm text-paper-dim">{children}</dd>
    </div>
  );
}

/** How the strategy is decided, with the fixed thresholds of the posture data. */
function FrameworkCard() {
  const { posture } = useProfile();
  const t = posture?.core?.meta.thresholds ?? null;
  return (
    <section className="card p-5" aria-labelledby="framework-h" data-testid="framework-chain">
      <h3 id="framework-h" className="rule-neis-sm text-base font-medium text-paper lg:text-lg">
        How the strategy is decided: Need &rarr; Prize &rarr; Position &rarr; Choice &rarr; Policy Alignment
      </h3>
      <p className="mt-2 max-w-3xl text-sm text-paper-dim">{STRATEGY_EXPLAINER}</p>
      <ol className="mt-3 list-decimal space-y-2 pl-5 text-sm text-paper-dim">
        <li>
          <strong className="text-paper">Need: what problem has to be solved?</strong> At the five producing stages,{' '}
          <Term id={POSTURE_TERMS.supplyChainNeed}>supply-chain vulnerability</Term>: import dependence, supplier concentration, risky supplier exposure and the lack of trusted
          alternatives (high at {pct(t?.need)} and above, as a percentile, and only where the need score itself reaches 0.10, so a tiny exposure in a thinly traded step does not read
          as high). At Deployment &amp; Use, <Term id={POSTURE_TERMS.deploymentNeed}>deployment need</Term>: whether expected demand is enough, the gap to national targets and system
          requirements, each with its scenario named. The two are never added together.
        </li>
        <li>
          <strong className="text-paper">Prize: is productive capability here worth having?</strong> <Term id={POSTURE_TERMS.strategicValue}>Strategic Industrial Value</Term>{' '}
          combines the market prize (<Term id={POSTURE_TERMS.industrialOpportunity}>Industrial Opportunity</Term>: expected world demand growth and investment momentum, lifted by
          national home-market growth only where national data exist), industrial commons (process knowledge, workforce and suppliers, spillovers to other sectors) and future option
          value (whether a credible leap ahead is in reach); high at {pct(t?.prize)} and above, and capped just below where only the market speaks for it. Current capability and
          need are kept out of it. A <Term id={POSTURE_TERMS.largeYard}>large-yard case</Term> (a mature, passive technology with most commodity signals holding) makes a step Watch
          whatever the size of its market.
        </li>
        <li>
          <strong className="text-paper">Position: how strong is the country today?</strong> <Term id={POSTURE_TERMS.competitivePosition}>Competitive Position</Term>: demonstrated
          capability (exports and specialization, trade momentum, production, capacity, investment, cost, resources, deployment scale where they apply), as a percentile among the
          step&apos;s main producers; the Preserve bar is {pct(t?.cp)} and above (the top quarter), for a producer above the capability floor.
        </li>
        <li>
          <strong className="text-paper">Choice: what should the country do?</strong> The Secure tests come first (control-system risk, chokepoint risk, system resilience). At
          Deployment &amp; Use, deployment need decides Accelerate ({pct(t?.deployment)} and above) or Watch, never Partner. At the producing stages need and the prize decide whether
          a step matters and Competitive Position decides the response: a strong position with a prize is Preserve; a weaker one is Catch Up only with a{' '}
          <Term id={POSTURE_TERMS.pathway}>credible pathway</Term> (a capability anchor to build from, corroborated by a signal of another kind); a high need without a domestic case
          is Partner; low need is Watch, with a low prize and also with a high prize where there is neither a strong position nor a credible pathway. A step missing need or prize is
          not assessed, never Watch.
        </li>
        <li>
          <strong className="text-paper">Policy alignment: is policy focused on the right places?</strong> Observed policy (support, protection, momentum) is compared with the
          choice afterwards and never feeds it.
        </li>
      </ol>
      <p className="mt-3 text-xs text-paper-mute">
        A value within {pct(t?.borderline_band)} points of its threshold is borderline. The thresholds are fixed (config/strategic_position.yml); the Perspective dials below re-weight
        only the Legacy OSI indices.
      </p>
    </section>
  );
}

/** The six stages, their display names and which need each reads (stage mapping). */
function StageMappingCard() {
  return (
    <section className="card p-5" aria-labelledby="stages-h" data-testid="stage-mapping">
      <h3 id="stages-h" className="rule-neis-sm text-base font-medium text-paper lg:text-lg">
        The six stages and the need each reads
      </h3>
      <div className="mt-3 overflow-x-auto">
        <table className="w-full min-w-[28rem] text-left text-sm">
          <caption className="sr-only">The six supply-chain stages and the need each reads</caption>
          <thead>
            <tr className="text-xs text-paper-mute">
              <th scope="col" className="py-1 pr-3 font-medium">
                Stage
              </th>
              <th scope="col" className="py-1 pr-3 font-medium">
                Strategy
              </th>
              <th scope="col" className="py-1 font-medium">
                Need read
              </th>
            </tr>
          </thead>
          <tbody className="text-paper-dim">
            {SUPPLY_STAGES.map((s, i) => (
              <tr key={s} className="border-t hairline">
                <th scope="row" className="py-1.5 pr-3 font-normal text-paper">
                  {i + 1}. {STAGE_DISPLAY[s]}
                </th>
                <td className="py-1.5 pr-3">{STAGE_GROUP[s]}</td>
                <td className="py-1.5">{s === 'Deployment' ? MEASURE_PLAIN.deploymentNeed : MEASURE_PLAIN.supplyChainNeed}</td>
              </tr>
            ))}
          </tbody>
        </table>
      </div>
    </section>
  );
}

/** The economy's coverage, evidence specificity, sensitivity, trade benchmark and momentum cut-off, freshness and score components. */
function CoverageCard() {
  const { sheet, manifest, posture } = useProfile();
  const name = proseName(sheet.meta.country);
  const core = posture?.core ?? null;
  const meta = core?.meta ?? null;
  const n = coverageCounts(posture?.cells ?? []);
  const built = dateLabel(core?.created ?? null);
  return (
    <section className="card p-5" aria-labelledby="coverage-h" data-testid="evidence-coverage">
      <h3 id="coverage-h" className="rule-neis-sm text-base font-medium text-paper lg:text-lg">
        {cap(poss(name))} evidence, coverage and freshness
      </h3>
      <dl className="mt-3">
        <Row term="Data coverage">
          {n.nodes
            ? `${n.assessed} of ${n.nodes} supply-chain steps assessed; ${n.notAssessed} not assessed (need or prize not measured: shown hatched on the map as uncertainty, never as Watch).`
            : posture?.status === 'loading'
              ? 'Loading…'
              : STATE.noStrategyMap}
        </Row>
        <Row term="Evidence specificity">
          {n.assessed
            ? `${n.strong} strong · ${n.moderate} moderate · ${n.weak} weak${n.ungraded ? ` · ${n.ungraded} without a grade` : ''} (how specific the data behind each step are; a step's evidence is in its tooltip, the map's table and its drawer, not drawn in the cell).`
            : '–'}
        </Row>
        <Row term="Sensitivity">
          {n.nodes
            ? `${n.borderline} ${n.borderline === 1 ? 'step sits' : 'steps sit'} within ${pct(meta?.thresholds.borderline_band)} points of a threshold (borderline: a small change in the data could change the choice); ${n.flagged} ${n.flagged === 1 ? 'is' : 'are'} flagged for consistency review.`
            : '–'}
        </Row>
        <Row term="Trade benchmark">{meta?.benchmark_period ? `Structural trade position over ${meta.benchmark_period}.` : 'Not stated in the data.'}</Row>
        <Row term="Current trade momentum">
          {meta?.momentum_period
            ? `${meta.momentum_period} against ${meta.momentum_previous_period ?? 'the previous 12 months'} (through ${meta.momentum_through ?? '–'}); a change within ±${Math.round((meta.momentum_band ?? 0) * 100)}% counts as flat. It enters Competitive Position conservatively, inside the trade family.`
            : 'Not available in this data build.'}
        </Row>
        <Row term="Data freshness">
          {[built ? `Strategy built ${built}${core?.gitCommit ? ` (export ${core.gitCommit.slice(0, 7)})` : ''}` : null, `index baseline as of ${sheet.meta.as_of_label}`, manifest.nipo?.vintage_label ? `Global Trade Alert measures: ${manifest.nipo.vintage_label}` : null]
            .filter(Boolean)
            .join('; ')}
          . {FRESHNESS_NOTE}
        </Row>
        <Row term="Score components">
          Need: import dependence, supplier concentration, risky supplier exposure, lack of trusted alternatives; at Deployment &amp; Use, demand adequacy, policy or climate gap,
          system need. Prize: the market prize (expected world demand growth, investment momentum, home market where national data exist), industrial commons and future option
          value; technology maturity as context. Position: export position, structural trade share and momentum, production, manufacturing capacity, investment, cost, resources,
          deployment scale. Each step&apos;s components are in its drawer.
        </Row>
        <Row term="Methodology and raw data">
          <span className="flex flex-wrap gap-x-4 gap-y-1">
            <Link className="link" to="/methods">
              Methods &rarr;
            </Link>
            <Link className="link" to="/glossary">
              Glossary &rarr;
            </Link>
            <Link className="link" to="/countries/about">
              About the country data &rarr;
            </Link>
            <Link className="link" to={sectorCountryLink(sheet.iso3)}>
              {cap(poss(name))} raw data in Sectors &rarr;
            </Link>
          </span>
        </Row>
      </dl>
    </section>
  );
}

export function EvidenceSection({ engineOn, hostKeys }: { engineOn: boolean; hostKeys: readonly string[] }) {
  const { sheet } = useProfile();
  const name = proseName(sheet.meta.country);
  return (
    <Section
      id={EVIDENCE.id}
      aliases={EVIDENCE.aliases}
      title={EVIDENCE.label}
      basis={<PostureBasisTag />}
      intro={
        <>
          {EVIDENCE_INTRO} For {name}: how the strategy is decided, the data behind each step and how far to trust it, the notes, sources and downloads.
        </>
      }
    >
      <FrameworkCard />
      <StageMappingCard />
      <CoverageCard />
      <DataNotes />
      <LegacyOsiSection engineOn={engineOn} hostKeys={hostKeys} />
    </Section>
  );
}

// ---- Legacy OSI ------------------------------------------------------------------------------------------

/** The Perspective dials, as "Explore scenarios": visibly apart from the canonical strategy (spec XVIII). */
function ExploreScenarios() {
  return (
    <section className="space-y-2" aria-labelledby="explore-scenarios-h" data-testid="explore-scenarios">
      <h3 id="explore-scenarios-h" className="text-base font-medium text-paper lg:text-lg">
        Explore scenarios
      </h3>
      <p className="max-w-3xl text-sm text-paper-dim">
        Preference-adjusted scenarios of the Legacy OSI indices: the four dials re-weight the Strategic Index and the partner scores below. They never change the strategy in the sections
        above, which rests on fixed thresholds.
      </p>
      <PerspectiveControl />
    </section>
  );
}

/** The Legacy OSI results, in the closed disclosure (the only blocks of the page that follow the dials). */
function LegacyResults() {
  return (
    <>
      <ExploreScenarios />
      <SpaceSection id="legacy-space" level={3} />
      <OpportunitiesSection id="legacy-opportunities" level={3} title="Bets, dependencies and capabilities by corner" />
      <SectorPositionSection id="legacy-sectors" level={3} />
      <LegacyPolicyAgreement />
      <PartnerScoresFold />
    </>
  );
}

function MappingTable() {
  return (
    <div className="overflow-x-auto" data-testid="legacy-mapping">
      <table className="w-full min-w-[36rem] text-left text-sm">
        <caption className="mb-1 text-left text-xs text-paper-mute">The four corners and the choice each maps to conceptually</caption>
        <thead>
          <tr className="text-xs text-paper-mute">
            <th scope="col" className="py-1 pr-3 font-medium">
              Legacy corner
            </th>
            <th scope="col" className="py-1 pr-3 font-medium">
              Maps to
            </th>
            <th scope="col" className="py-1 font-medium">
              Why a step can differ
            </th>
          </tr>
        </thead>
        <tbody className="text-paper-dim">
          {LEGACY_MAPPING_ROWS.map((r) => (
            <tr key={r.corner} className="border-t hairline align-top" data-corner={r.corner}>
              <th scope="row" className="py-1.5 pr-3 font-normal">
                <span className="text-paper">{r.label}</span>
                <span className="block text-xs text-paper-mute">{r.meaning}</span>
              </th>
              <td className="py-1.5 pr-3 text-paper">≈ {r.mapsTo}</td>
              <td className="py-1.5 text-xs">{r.differs}</td>
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
}

export function LegacyOsiSection({ engineOn, hostKeys }: { engineOn: boolean; hostKeys: readonly string[] }) {
  const { sheet, customInUrl } = useProfile();
  const m = sheet.meta;
  const name = proseName(m.country);
  const q = sheet.summary.legacy_corners;
  const corners = q ? legacyCornerLine({ legacy_grow: q.legacy_grow?.v ?? 0, legacy_upgrade: q.legacy_upgrade?.v ?? 0, legacy_exposed: q.legacy_exposed?.v ?? 0, legacy_stable: q.legacy_stable?.v ?? 0 }) : null;
  const results = <LegacyResults />;
  return (
    <Section
      id={LEGACY_SECTION.id}
      level={3}
      aliases={LEGACY_SECTION.aliases}
      title="Legacy OSI framework (paper baseline)"
      className="rounded-lg border border-dashed border-white/15 p-4"
      basis={<BasisTag kind="prefs" note="Legacy OSI · not a current recommendation" />}
      intro={
        <>
          {LEGACY_OSI_NOTE} Its <Term id="es" />, <Term id="eo" />, their four corners and the <Term id="si" /> are the paper baseline. The strategy above is the current
          framework.
        </>
      }
    >
      <Notice kind="info" title="Not a current recommendation">
        The legacy indices mix together {LEGACY_MIXES.join(', ')}, so a sector&apos;s corner can disagree with its choice in the strategy. The current framework keeps those
        apart: need, prize and position are measured separately, and the choice follows fixed rules.
      </Notice>
      {m.status === 'non_rankable' && (
        <Notice kind="info" title="Scores shown but not ranked">
          {cap(poss(name))} index scores are shown but not compared with other economies: {m.status_reason ?? 'most of its data are filled in'}. The legacy charts that compare sectors are
          empty; the tables list every value with its reason. The strategy above is computed separately.
        </Notice>
      )}
      <div className="space-y-2 text-sm text-paper-dim" data-testid="legacy-reading">
        <p>{ledeOf(sheet)}</p>
        {corners && (
          <p>
            Where its sectors sit in the four corners: <span className="text-paper">{corners}</span>.
          </p>
        )}
        {!m.has_si && <p>{cap(name)} has no Policy index (no International Energy Agency policy records), so no Strategic Index is computed for it.</p>}
      </div>
      <MappingTable />
      {customInUrl && (
        <p className="text-sm text-paper-dim" data-testid="legacy-custom-link">
          The link carries a custom perspective: the dials apply to the Legacy OSI results below.
        </p>
      )}
      <Fold id={LEGACY_FOLD} summary={`Show the Legacy OSI results for ${name}: the corners scatter, the Strategic Index priorities, the sector position table, the policy agreement matrix, the partner scores and the Perspective dials (Explore scenarios)`}>
        {engineOn ? (
          <EngineScope hostKeys={hostKeys} fallback={results}>
            <EngineSi iso3={sheet.iso3}>{results}</EngineSi>
          </EngineScope>
        ) : (
          results
        )}
      </Fold>
    </Section>
  );
}
