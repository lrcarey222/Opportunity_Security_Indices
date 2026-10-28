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
import { useTitle } from '../../shell/title.ts';
import { PAGE, useAibCtx } from '../AibLayout.tsx';
import { coalitionMembersOf, type AibCompact } from '../contract.ts';
import { useAibScenarioDoc } from '../data/hooks.ts';
import { fmtShare, fmtUsd, listWords, plural } from '../derive/format.ts';
import { compactNodes } from '../derive/nodes.ts';
import { compactUnchangedReasonOf, managedCompact, managedOf } from '../derive/scenarioPages.ts';
import { compactDetail, compactScenario, scenarioInfo, type CompactScenario } from '../derive/scenarios.ts';
import { indexLabel, indicesOfCompact, type Taxonomy } from '../derive/taxonomy.ts';
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
import { stagesInChainOrder } from '../../lib/stages.ts';

const SECTIONS = [
  ['why', 'Why this compact?'],
  ['overview', 'Who and what?'],
  ['change', 'What does coordination change?'],
  ['govern', 'What should governments do?'],
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
    [c, doc, names, name, nodeOf, shockLabel, tx, scenario, hasScenarios, view, viewState, managedDoc, href],
  );
  // follow a deep link's #anchor once the page has rendered
  useEffect(() => {
    if (loc.hash) document.getElementById(loc.hash.slice(1))?.scrollIntoView?.({ block: 'start' });
    // only on the first render of this compact
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [c.id]);
  const m = doc.meta;
  // the compact's stages in chain order, in prose ("across the refining and components stages")
  const stages = stagesInChainOrder(c.stages).map((x) => x.toLowerCase());
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
  const head = `${title !== full ? `${full}. ` : ''}${kind}: ${coversWords(c, tx)} across the ${listWords(stages)} ${stages.length === 1 ? 'stage' : 'stages'}, ${plural(c.nodes.length, 'node')}${members !== null ? ` and ${plural(members, 'member')}` : ''}.`;
  const lede = mi
    ? `${head} ${managedWords(compactScenario(c, 'managed', nodeOf), scenarioInfo(m, 'managed').label, viewState.reason ?? compactUnchangedReasonOf(compactNodes(c, nodeOf)))}`
    : `${head} Under the headline shock (${shockLabel}; supply assumption: ${m.headlineScenario}) the compact's demand of ${fmtUsd(c.metrics.demandUsd)} loses ${fmtUsd(c.metrics.lostUsd)} (${fmtShare(c.metrics.exposure, 0)}).`;
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
          <OverviewSection title="Who and what?" policy={false} />
          <QuestionSection
            id="change"
            title="What does coordination change?"
            intro={<p>{mi ? (view ? changeWords(view, true) : managedChange(compactScenario(c, 'managed', nodeOf))) : changeWords(c)}</p>}
          >
            <CoverageSection />
            <NetworkSection />
          </QuestionSection>
          <QuestionSection
            id="govern"
            title="What should governments do?"
            intro={<p>Each node gets the treatment its own results call for, so one compact can combine coordinating trade, building capacity and managing specialization.</p>}
          >
            <PolicySection />
            <Disclosure summary="Node by node: coverage, suppliers, hosts and data quality" defaultOpen={NODE_ANCHORS.includes(hash)}>
              <NodeSection frontier={false} />
            </Disclosure>
          </QuestionSection>
          <section id="method" aria-label="Method details" className="scroll-mt-16">
            <Disclosure summary="Method details: specialization and resilience by supplier limit, members joining, reading these results" defaultOpen={METHOD_ANCHORS.includes(hash)}>
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
 * What the compact covers: its technologies (master: "Batteries and Electric Vehicles"), or its sectors
 * with their EIS indices (dictionary: "the Rare earths and Magnets sectors (indices A Materials & Inputs
 * and B Electrotechnologies)").
 */
function coversWords(c: AibCompact, tx: Taxonomy): string {
  if (!tx.dictionary) return listWords(c.technologies);
  const ix = indicesOfCompact(tx, c);
  const sectors = `the ${listWords(c.technologies)} ${c.technologies.length === 1 ? tx.one : tx.many}`;
  return ix.length ? `${sectors} (${ix.length === 1 ? 'index' : 'indices'} ${listWords(ix.map(indexLabel))})` : sectors;
}

/** One plain sentence on what coordination changes (the coverage steps of the compact; `managed`: under Managed Interdependence). */
function changeWords(c: AibCompact, managed = false): string {
  const m = c.metrics;
  const inv = m.investment !== null && m.investment !== undefined ? `, and adding new capacity at member hosts ${fmtShare(m.investment, 0)}` : '';
  return `Of the compact's demand, ${managed ? 'the cap alone' : 'the shock alone'} leaves ${fmtShare(m.static, 0)} served; existing relationships ${fmtShare(m.existing, 0)}; coordinated trade among the members ${fmtShare(m.trade, 0)}${inv}.`;
}

/** The lede's sentence under Managed Interdependence (compact.managed, or the compact's nodes aggregated; `why`: why it is unchanged). */
function managedWords(s: CompactScenario, label: string, why: string): string {
  if (!s.ran) return `${label} leaves this compact unchanged (${why}), so no supply has to shift.`;
  return `Under ${label} (no supplier is removed; suppliers above a cap in the allied market lose the excess), ${s.nodesRun} of its ${plural(s.nodes, 'node')} ${s.nodesRun === 1 ? 'has' : 'have'} a supplier above the cap and ${fmtUsd(s.lostUsd)} of the compact's demand of ${fmtUsd(s.demandUsd)} (${fmtShare(s.exposure, 0)}) has to shift.`;
}

/** What changes under Managed Interdependence before its file is loaded (or without a coalition). */
function managedChange(s: CompactScenario): string {
  if (!s.ran) return 'Nothing changes under this scenario: the compact keeps the supply it has today.';
  return `Of the compact's demand, the cap alone leaves ${fmtShare(s.static, 0)} served; existing relationships ${fmtShare(s.existing, 0)}; optimized trade among every partner ${fmtShare(s.optimized, 0)}.`;
}
