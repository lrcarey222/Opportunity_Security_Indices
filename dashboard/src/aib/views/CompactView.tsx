// One compact /compacts/:id (IA refactor, 2026-10-16; brief s.17): the head (name, archetype, type, what
// it covers), then four questions over the existing sections: Why this compact? (why it exists), Who and
// what? (nodes, design, critical partners, the country x role matrix), What does coordination change?
// (demand served: shock -> existing relationships -> coordinated trade -> trade + investment; the
// partnership network) and What should governments do? (node treatments and policy needs; the
// node-by-node detail in a disclosure). Method details (the specialization frontier, members joining,
// reading notes) sit in a closed disclosure. Old anchors (#overview #why #nodes #network #coverage
// #coalition) still land, opening their disclosure. An unknown id goes to the landing with a notice; a
// lower-case id is accepted. Every number is an observed / modelled fact: none follows the Perspective.
//
// The scenario toggle (2026-10-19, page work A): the lede, the case for coordination, the coalition, its
// tiers and roles, the coverage steps, who invests where, the node treatments and the coalition path follow
// the toggled scenario (Managed Interdependence: the compact as its scenario file solves it, loaded on
// demand; its reason when the scenario ran on none of the compact's nodes); the coverage comparison shows
// the three scenarios (Baseline, Managed Interdependence, Decoupling) for the compact and for each node; the
// compact's suppliers today come from compact.baseline. The context keeps `c` the main document's compact
// (Decoupling) and adds the scenario's `view` (compact/shared.tsx).
//
// The strategic layer (2026-10-20): with doc.strategic a compact candidate is a Workstream of a Strategic
// Sectoral Compact, so the page reads "WORKSTREAM · AIB-Cxx" and "Why this workstream?", says which strategic
// compact it is filed under (WorkstreamParent: a link up, its share of the allied demand, the others it
// spans, whether it is active under the scenario, why it was filed by review) and links back to that
// compact's workstreams and to the Workstreams index. A bundle without the layer keeps the former words.

import { useEffect, useMemo } from 'react';
import { Link, useLocation, useParams } from 'react-router';
import { Redirect } from '../../shell/Redirect.tsx';
import { Disclosure, PageHead } from '../../components/ui.tsx';
import { COMPACT } from '../../content/vocabulary.ts';
import { useTitle } from '../../shell/title.ts';
import { PAGE, useAibCtx } from '../AibLayout.tsx';
import { coalitionMembersOf, type AibCompact, type AibNode } from '../contract.ts';
import { useAibScenarioDoc } from '../data/hooks.ts';
import { fmtShare, fmtUsd, listWords, plural } from '../derive/format.ts';
import { compactNodes } from '../derive/nodes.ts';
import { compactUnchangedReasonOf, managedCompact, managedOf } from '../derive/scenarioPages.ts';
import { compactDetail, compactScenario, scenarioInfo, type CompactScenario } from '../derive/scenarios.ts';
import { cellStagesOfNodes, osiTechsOfNode } from '../derive/taxonomy.ts';
import { workstreamHomeOf, workstreamOf } from '../derive/strategic.ts';
import { rememberCompact, strategicCompactHref, WORKSTREAMS_LABEL, workstreamsHref } from '../nav.ts';
import { QuestionSection, ReadingNotes } from './common.tsx';
import { BasisTag } from './basis.tsx';
import { NetworkSection } from './compact/NetworkSection.tsx';
import { FrontierCard, NodeSection } from './compact/NodeSection.tsx';
import { OverviewSection, WhySection } from './compact/OverviewSections.tsx';
import { PolicySection } from './compact/PolicySection.tsx';
import { CoalitionSection, CoverageSection } from './compact/ResilienceSections.tsx';
import { CompactProvider, WorkstreamParent, type CompactCtx, type CompactViewState } from './compact/shared.tsx';
import { TypeTag } from '../charts/TypeTag.tsx';
import { compactVsFragmented } from '../derive/coverage.ts';
import { compactTypeInfo } from '../derive/types.ts';

const SECTIONS = [
  ['why', COMPACT.sectionTitles.why],
  ['overview', COMPACT.sectionTitles.what],
  ['change', COMPACT.sectionTitles.together],
  ['govern', COMPACT.sectionTitles.next],
  ['method', 'Method details'],
] as const;

/** Anchors inside the closed disclosures: a deep link to one opens it. */
const NODE_ANCHORS = ['nodes'];
const METHOD_ANCHORS = ['method', 'frontier', 'coalition', 'reading-h'];

export default function CompactView() {
  const { id = '' } = useParams();
  const { compactOf, title, href } = useAibCtx();
  const c = compactOf(id);
  useTitle(c ? `${c.id} ${title(c)}` : 'Compacts', 'aib');
  useEffect(() => {
    if (c) rememberCompact(c.id);
  }, [c]);
  // an unknown id: the Workstreams index (the compact candidates' list since 2026-10-20), the scenario kept
  if (!c) return <Redirect to={href('/compacts/workstreams')} notice={`No workstream with the id '${id}'; showing all workstreams.`} />;
  return <Compact key={c.id} c={c} />;
}

function Compact({ c }: { c: AibCompact }) {
  const { doc, names, name, nodeOf, shockLabel, tx, title: titleOf, scenario, hasScenarios, href, strategic, strategicScenario } = useAibCtx();
  // with the strategic layer (2026-10-20) a compact candidate is a Workstream of a Strategic Sectoral Compact
  const ws = strategic !== null && workstreamOf(strategic, c.id) !== null;
  const home = strategic ? workstreamHomeOf(strategic, c.id) : null;
  const whyTitle = ws ? 'Why this workstream?' : 'Why this compact?';
  const loc = useLocation();
  const mi = scenario === 'managed';
  const load = useAibScenarioDoc(mi ? 'managed' : null);
  const managedDoc = managedOf(load.doc);
  const view = useMemo(() => (!mi ? c : managedCompact(c, managedDoc)), [mi, c, managedDoc]);
  const viewState: CompactViewState = useMemo(() => {
    if (view) return { status: 'ready', reason: null, retry: load.retry };
    if (managedDoc) return { status: 'none', reason: compactDetail(c, 'managed', managedDoc).reason, retry: load.retry };
    if (load.error) return { status: 'error', reason: null, retry: load.retry };
    if (load.missing) return { status: 'missing', reason: null, retry: load.retry };
    return { status: 'loading', reason: null, retry: load.retry };
  }, [view, managedDoc, c, load.error, load.missing, load.retry]);
  const ctx: CompactCtx = useMemo(
    () => ({
      c,
      displayName: titleOf(c),
      nodes: compactNodes(c, nodeOf),
      meta: doc.meta,
      names,
      name,
      nodeLabel: (nid: string) => nodeOf(nid)?.label ?? nid,
      shockLabel,
      tx,
      scenario,
      hasScenarios,
      view,
      viewState,
      managedDoc,
      href,
    }),
    [c, titleOf, doc, names, name, nodeOf, shockLabel, tx, scenario, hasScenarios, view, viewState, managedDoc, href],
  );
  // follow a deep link's #anchor once the page has rendered
  useEffect(() => {
    if (loc.hash) document.getElementById(loc.hash.slice(1))?.scrollIntoView?.({ block: 'start' });
    // only on the first render of this compact
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [c.id]);
  const m = doc.meta;
  // the compact's stages in chain order, in prose ("across the refining and components stages"): those of its
  // nodes' cells, never node.stage (none when no node has a basket cell)
  const stages = cellStagesOfNodes(compactNodes(c, nodeOf)).map((x) => x.toLowerCase());
  // The type and archetype of the compact the page shows: Managed Interdependence's own (its classes, compact
  // gain and coalition size), none without a coalition there or while its file loads.
  const typed = mi ? view : c;
  const type = typed ? compactTypeInfo(typed.type, compactVsFragmented(typed.metrics)?.gain) : null;
  const hash = loc.hash.slice(1);
  const title = titleOf(c);
  const full = titleOf(c, true);
  const members = view ? coalitionMembersOf(view).length : null;
  const kind = typed && type
    ? `${type.short}, ${typed.archetype.replace(/ Compact$/, '').toLowerCase()} archetype`
    : viewState.status === 'none' ? 'No coalition under Managed Interdependence' : ws ? 'Workstream' : 'Sectoral compact';
  const head = `${title !== full ? `${full}. ` : ''}${kind}: ${coversWords(compactNodes(c, nodeOf))}${stages.length ? ` across the ${listWords(stages)} ${stages.length === 1 ? 'stage' : 'stages'}` : ''}, ${plural(c.nodes.length, 'supply-chain step')}${members !== null ? ` and ${plural(members, 'partner')}` : ''}.`;
  // the guide's compact lede: what is exposed, what existing partners cover, what coordination adds, what is left
  const lede = mi
    ? `${head} ${managedWords(compactScenario(c, 'managed', nodeOf), scenarioInfo(m, 'managed').label, viewState.reason ?? compactUnchangedReasonOf(compactNodes(c, nodeOf)))}`
    : `${head} ${exposedWords(c, hasScenarios ? scenarioInfo(m, 'decoupling').label : 'the headline disruption', shockLabel)}`;
  return (
    <CompactProvider value={ctx}>
      <div className={PAGE}>
        {ws ? (
          <p className="mb-4 flex flex-wrap gap-x-4 gap-y-1 text-sm" data-testid="workstream-back">
            {home && (
              <Link className="link" to={href(strategicCompactHref(home.id, undefined, 'workstreams'))}>
                &larr; {home.name}
              </Link>
            )}
            <Link className="link" to={href(workstreamsHref(undefined, home?.id))}>
              All {WORKSTREAMS_LABEL.toLowerCase()}
            </Link>
          </p>
        ) : (
          <p className="mb-4 text-sm">
            <Link className="link" to={href('/compacts')}>
              &larr; All compacts
            </Link>
          </p>
        )}
        <PageHead eyebrow={`${ws ? 'WORKSTREAM' : 'SECTORAL COMPACT'} · ${c.id}`} title={title} lede={lede}>
          <WorkstreamParent s={strategic} id={c.id} scenario={strategicScenario} href={href} />
          {type?.tone === 'unnecessary' && (
            <p className="mt-3 flex max-w-3xl flex-wrap items-center gap-x-3 gap-y-1 text-sm text-paper-dim">
              <TypeTag info={type} />
              {type.explanation}
            </p>
          )}
          <div className="mt-3">
            <BasisTag basis="facts" />
          </div>
          <nav aria-label={ws ? 'Sections of this workstream' : 'Sections of this compact'} className="mt-4">
            <ul className="flex flex-wrap gap-x-4 gap-y-1 text-sm">
              {SECTIONS.map(([sid, label]) => (
                <li key={sid}>
                  <a className="link" href={`#${sid}`}>
                    {sid === 'why' ? whyTitle : label}
                  </a>
                </li>
              ))}
            </ul>
          </nav>
        </PageHead>
        <div className="space-y-14">
          <WhySection title={whyTitle} />
          <OverviewSection title={COMPACT.sectionTitles.what} policy={false} />
          <QuestionSection
            id="change"
            title={COMPACT.sectionTitles.together}
            intro={
              <p>
                Start with the supply that survives. Then add what existing partners can replace, what a coordinated trade response adds, and what only new capacity can solve.{' '}
                {mi ? (view ? changeWords(view, true) : managedChange(compactScenario(c, 'managed', nodeOf))) : changeWords(c)}
              </p>
            }
          >
            <CoverageSection />
            <NetworkSection />
          </QuestionSection>
          <QuestionSection
            id="govern"
            title={COMPACT.sectionTitles.next}
            intro={
              <p>
                Different bottlenecks need different tools: trade rules where capacity already exists, investment and finance where capacity is missing, and safeguards where
                specialization has become too concentrated. Each supply-chain step gets the treatment its own results call for, so one compact can combine coordinating trade,
                building capacity and managing specialization.
              </p>
            }
          >
            <PolicySection />
            <Disclosure summary="Supply-chain step by step: suppliers, gaps, investment options and evidence" defaultOpen={NODE_ANCHORS.includes(hash)}>
              <NodeSection frontier={false} />
            </Disclosure>
          </QuestionSection>
          <section id="method" aria-label="Method details" className="scroll-mt-16">
            <Disclosure summary="Method details: concentration limits, resilience targets and how partners are selected" defaultOpen={METHOD_ANCHORS.includes(hash)}>
              <div className="space-y-10 pt-2">
                <div id="frontier" className="scroll-mt-16">
                  <FrontierCard />
                </div>
                <CoalitionSection />
                <ReadingNotes />
              </div>
            </Disclosure>
          </section>
        </div>
      </div>
    </CompactProvider>
  );
}

/**
 * What the compact covers, by OSI technology (2026-10-29, design A1): the technologies of its nodes' six-stage
 * cells, as equals ("Batteries and Electric Vehicles"), with the nodes none of whose codes is in the OSI
 * basket counted apart; "goods outside the OSI technology basket" when no node has a basket cell.
 */
function coversWords(nodes: readonly AibNode[]): string {
  const techs = [...new Set(nodes.flatMap((n) => osiTechsOfNode(n)))].sort((a, b) => a.localeCompare(b, 'en'));
  const outside = nodes.filter((n) => osiTechsOfNode(n).length === 0).length;
  if (!techs.length) return 'goods outside the OSI technology basket';
  return `${listWords(techs)}${outside ? ` (and ${plural(outside, 'supply-chain step')} outside the OSI basket)` : ''}`;
}

/**
 * The lede's sentence under the disruption (the guide's compact lede): the share of the compact's demand
 * exposed, what existing relationships keep supplied, what the members' coordinated trade raises it to, and
 * the rest (1 - the trade compact's coverage), which needs new capacity or other sourcing.
 */
function exposedWords(c: AibCompact, label: string, shockLabel: string): string {
  const m = c.metrics;
  const rest = typeof m.trade === 'number' ? Math.max(0, 1 - m.trade) : null;
  return `Under ${label} (${shockLabel}), ${fmtShare(m.exposure, 0)} of the compact's allied demand of ${fmtUsd(m.demandUsd)} (${fmtUsd(m.lostUsd)}) loses its supplier. Existing partners keep ${fmtShare(m.existing, 0)} of that demand supplied; coordinated trade among the members raises it to ${fmtShare(m.trade, 0)}${rest !== null && rest > 0.005 ? `; the remaining ${fmtShare(rest, 0)} needs new capacity or a different sourcing strategy` : ''}.`;
}

/** One plain sentence on what coordination changes (the coverage steps of the compact; `managed`: under Managed Interdependence). */
function changeWords(c: AibCompact, managed = false): string {
  const m = c.metrics;
  const inv = m.investment !== null && m.investment !== undefined ? `; adding new capacity in member countries, ${fmtShare(m.investment, 0)}` : '';
  return `Of the compact's demand, ${managed ? 'the concentration cap alone' : 'the disruption alone'} leaves ${fmtShare(m.static, 0)} supplied; existing relationships raise that to ${fmtShare(m.existing, 0)}; coordinated trade among the members, ${fmtShare(m.trade, 0)}${inv}.`;
}

/** The lede's sentence under Managed Interdependence (compact.managed, or the compact's nodes aggregated; `why`: why it is unchanged). */
function managedWords(s: CompactScenario, label: string, why: string): string {
  if (!s.ran) return `${label} leaves this compact unchanged (${why}): no supply needs to shift, so the compact keeps today's supplier network.`;
  return `Under ${label} this compact manages concentration without cutting a supplier out of the market: at ${s.nodesRun} of its ${plural(s.nodes, 'supply-chain step')} a supplier holds more of the allied market than the concentration cap allows, so ${fmtUsd(s.lostUsd)} of the compact's demand of ${fmtUsd(s.demandUsd)} (${fmtShare(s.exposure, 0)}) has to shift to other suppliers, keeping the specialization while adding redundancy.`;
}

/** What changes under Managed Interdependence before its file is loaded (or without a coalition). */
function managedChange(s: CompactScenario): string {
  if (!s.ran) return "Nothing changes under this scenario: no supply needs to shift, so the compact keeps today's supplier network.";
  return `Of the compact's demand, the concentration cap alone leaves ${fmtShare(s.static, 0)} supplied; existing relationships raise that to ${fmtShare(s.existing, 0)}; coordinated trade among every potential supplier, ${fmtShare(s.optimized, 0)}.`;
}
