// One compact /compacts/:id: the head (name, archetype, type, what it covers), then 1. the overview
// (nodes and classes, design, policy needs, the country x role matrix), 2. why it exists, 3. the
// node-by-node structure, 4. the partnership network, 5. the coverage comparison, 6. how resilience
// changes as countries join, and the notes on reading the results. An unknown id goes to the landing
// with a notice; a lower-case id is accepted.

import { useEffect, useMemo } from 'react';
import { Link, Navigate, useLocation, useParams } from 'react-router';
import { PageHead } from '../../components/ui.tsx';
import { useTitle } from '../../shell/title.ts';
import { PAGE, useAibCtx } from '../AibLayout.tsx';
import type { AibCompact } from '../contract.ts';
import { fmtShare, fmtUsd, listWords, plural } from '../derive/format.ts';
import { compactNodes } from '../derive/nodes.ts';
import { rememberCompact } from '../nav.ts';
import { ReadingNotes } from './common.tsx';
import { NetworkSection } from './compact/NetworkSection.tsx';
import { NodeSection } from './compact/NodeSection.tsx';
import { OverviewSection, WhySection } from './compact/OverviewSections.tsx';
import { CoalitionSection, CoverageSection } from './compact/ResilienceSections.tsx';
import { CompactProvider, type CompactCtx } from './compact/shared.tsx';
import { TypeTag } from '../charts/TypeTag.tsx';
import { compactVsFragmented } from '../derive/coverage.ts';
import { compactTypeInfo } from '../derive/types.ts';

const STAGE_ORDER = ['Upstream', 'Midstream', 'Downstream'];
const stageRank = (s: string) => (STAGE_ORDER.includes(s) ? STAGE_ORDER.indexOf(s) : STAGE_ORDER.length);

const SECTIONS = [
  ['overview', 'Overview'],
  ['why', 'Why it exists'],
  ['nodes', 'Node by node'],
  ['network', 'Partnership network'],
  ['coverage', 'Coverage comparison'],
  ['coalition', 'Members joining'],
] as const;

export default function CompactView() {
  const { id = '' } = useParams();
  const { compactOf } = useAibCtx();
  const c = compactOf(id);
  useTitle(c ? `${c.id} ${c.name}` : 'Compacts', 'aib');
  useEffect(() => {
    if (c) rememberCompact(c.id);
  }, [c]);
  if (!c) return <Navigate to="/compacts" replace state={{ notice: `No compact with the id '${id}'; showing all compacts.` }} />;
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
  const lede = `${type.short}, ${c.archetype.replace(/ Compact$/, '').toLowerCase()} archetype: ${listWords(c.technologies)} across the ${listWords(stages)} ${stages.length === 1 ? 'stage' : 'stages'}, ${plural(c.nodes.length, 'node')} and ${plural(c.coalition.members.length, 'member')}. Under the headline shock (${shockLabel}, ${m.headlineScenario} scenario) the compact's demand of ${fmtUsd(c.metrics.demandUsd)} loses ${fmtUsd(c.metrics.lostUsd)} (${fmtShare(c.metrics.exposure, 0)}).`;
  return (
    <CompactProvider value={ctx}>
      <div className={PAGE}>
        <p className="mb-4 text-sm">
          <Link className="link" to="/compacts">
            &larr; All compacts
          </Link>
        </p>
        <PageHead eyebrow={`SECTORAL COMPACT · ${c.id}`} title={c.name} lede={lede}>
          {type.tone === 'unnecessary' && (
            <p className="mt-3 flex max-w-3xl flex-wrap items-center gap-x-3 gap-y-1 text-sm text-paper-dim">
              <TypeTag info={type} />
              {type.explanation}
            </p>
          )}
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
          <OverviewSection />
          <WhySection />
          <NodeSection />
          <NetworkSection />
          <CoverageSection />
          <CoalitionSection />
          <ReadingNotes />
        </div>
      </div>
    </CompactProvider>
  );
}
