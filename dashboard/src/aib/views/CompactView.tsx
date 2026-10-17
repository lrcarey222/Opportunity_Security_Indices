// One compact /compacts/:id (IA refactor, 2026-10-16; brief s.17): the head (name, archetype, type, what
// it covers), then four questions over the existing sections: Why this compact? (why it exists), Who and
// what? (nodes, design, critical partners, the country x role matrix), What does coordination change?
// (demand served: shock -> existing relationships -> coordinated trade -> trade + investment; the
// partnership network) and What should governments do? (node treatments and policy needs; the
// node-by-node detail in a disclosure). Method details (the specialization frontier, members joining,
// reading notes) sit in a closed disclosure. Old anchors (#overview #why #nodes #network #coverage
// #coalition) still land, opening their disclosure. An unknown id goes to the landing with a notice; a
// lower-case id is accepted. Every number is an observed / modelled fact: none follows the Perspective.

import { useEffect, useMemo } from 'react';
import { Link, useLocation, useParams } from 'react-router';
import { Redirect } from '../../shell/Redirect.tsx';
import { Disclosure, PageHead } from '../../components/ui.tsx';
import { useTitle } from '../../shell/title.ts';
import { PAGE, useAibCtx } from '../AibLayout.tsx';
import type { AibCompact } from '../contract.ts';
import { compactTitle, fmtShare, fmtUsd, listWords, plural } from '../derive/format.ts';
import { compactNodes } from '../derive/nodes.ts';
import { rememberCompact } from '../nav.ts';
import { QuestionSection, ReadingNotes } from './common.tsx';
import { BasisTag } from './basis.tsx';
import { NetworkSection } from './compact/NetworkSection.tsx';
import { FrontierCard, NodeSection } from './compact/NodeSection.tsx';
import { OverviewSection, WhySection } from './compact/OverviewSections.tsx';
import { PolicySection } from './compact/PolicySection.tsx';
import { CoalitionSection, CoverageSection } from './compact/ResilienceSections.tsx';
import { CompactProvider, type CompactCtx } from './compact/shared.tsx';
import { TypeTag } from '../charts/TypeTag.tsx';
import { compactVsFragmented } from '../derive/coverage.ts';
import { compactTypeInfo } from '../derive/types.ts';

const STAGE_ORDER = ['Upstream', 'Midstream', 'Downstream'];
const stageRank = (s: string) => (STAGE_ORDER.includes(s) ? STAGE_ORDER.indexOf(s) : STAGE_ORDER.length);

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
  const { compactOf } = useAibCtx();
  const c = compactOf(id);
  useTitle(c ? `${c.id} ${compactTitle(c)}` : 'Compacts', 'aib');
  useEffect(() => {
    if (c) rememberCompact(c.id);
  }, [c]);
  if (!c) return <Redirect to="/compacts" notice={`No compact with the id '${id}'; showing all compacts.`} />;
  return <Compact key={c.id} c={c} />;
}

function Compact({ c }: { c: AibCompact }) {
  const { doc, names, name, nodeOf, shockLabel } = useAibCtx();
  const loc = useLocation();
  const ctx: CompactCtx = useMemo(
    () => ({
      c,
      nodes: compactNodes(c, nodeOf),
      meta: doc.meta,
      names,
      name,
      nodeLabel: (nid: string) => nodeOf(nid)?.label ?? nid,
      shockLabel,
    }),
    [c, doc, names, name, nodeOf, shockLabel],
  );
  // follow a deep link's #anchor once the page has rendered
  useEffect(() => {
    if (loc.hash) document.getElementById(loc.hash.slice(1))?.scrollIntoView?.({ block: 'start' });
    // only on the first render of this compact
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [c.id]);
  const m = doc.meta;
  const stages = [...c.stages].sort((x, y) => stageRank(x) - stageRank(y)).map((x) => x.toLowerCase());
  const type = compactTypeInfo(c.type, compactVsFragmented(c.metrics)?.gain);
  const hash = loc.hash.slice(1);
  const title = compactTitle(c);
  const lede = `${title !== c.name ? `${c.name}. ` : ''}${type.short}, ${c.archetype.replace(/ Compact$/, '').toLowerCase()} archetype: ${listWords(c.technologies)} across the ${listWords(stages)} ${stages.length === 1 ? 'stage' : 'stages'}, ${plural(c.nodes.length, 'node')} and ${plural(c.coalition.members.length, 'member')}. Under the headline shock (${shockLabel}, ${m.headlineScenario} scenario) the compact's demand of ${fmtUsd(c.metrics.demandUsd)} loses ${fmtUsd(c.metrics.lostUsd)} (${fmtShare(c.metrics.exposure, 0)}).`;
  return (
    <CompactProvider value={ctx}>
      <div className={PAGE}>
        <p className="mb-4 text-sm">
          <Link className="link" to="/compacts">
            &larr; All compacts
          </Link>
        </p>
        <PageHead eyebrow={`SECTORAL COMPACT · ${c.id}`} title={title} lede={lede}>
          {type.tone === 'unnecessary' && (
            <p className="mt-3 flex max-w-3xl flex-wrap items-center gap-x-3 gap-y-1 text-sm text-paper-dim">
              <TypeTag info={type} />
              {type.explanation}
            </p>
          )}
          <div className="mt-3">
            <BasisTag basis="facts" />
          </div>
          <nav aria-label="Sections of this compact" className="mt-4">
            <ul className="flex flex-wrap gap-x-4 gap-y-1 text-sm">
              {SECTIONS.map(([sid, label]) => (
                <li key={sid}>
                  <a className="link" href={`#${sid}`}>
                    {label}
                  </a>
                </li>
              ))}
            </ul>
          </nav>
        </PageHead>
        <div className="space-y-14">
          <WhySection title="Why this compact?" />
          <OverviewSection title="Who and what?" policy={false} />
          <QuestionSection id="change" title="What does coordination change?" intro={<p>{changeWords(c)}</p>}>
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

/** One plain sentence on what coordination changes (the coverage steps of the compact). */
function changeWords(c: AibCompact): string {
  const m = c.metrics;
  const inv = m.investment !== null && m.investment !== undefined ? `, and adding new capacity at member hosts ${fmtShare(m.investment, 0)}` : '';
  return `Of the compact's demand, the shock alone leaves ${fmtShare(m.static, 0)} served; existing relationships ${fmtShare(m.existing, 0)}; coordinated trade among the members ${fmtShare(m.trade, 0)}${inv}.`;
}
