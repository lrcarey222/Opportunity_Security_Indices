// One Strategic Sectoral Compact /compacts/s/:sid (2026-10-20; task s.15, s.16, s.20, s.21, s.26). A reader
// should understand, in order: why the compact exists (its purpose, then its mission and headline figures
// under the toggled scenario); which nodes it covers (the supply-chain spine, the page's organizing device:
// each node's status under the scenario, a node selected for its existing evidence); where it is
// vulnerable; what the nodes need (BUILD / COORDINATE / MANAGE SPECIALIZATION: missions, portfolio and the
// instrument menu); who does what (the core countries' roles and the country x node division of labour);
// the workstreams underneath (the model's clusters, AIB-Cxx pages); and what changes between Managed
// Interdependence and Decoupling, Baseline as the reference. Progressive disclosure: compact -> workstreams
// -> nodes -> model evidence. The parts are in views/strategic/. A deep link's #anchor (#chain, #division,
// #workstreams, ...) scrolls once the page renders. An unknown id goes to the landing with a notice; a
// lower-case id is accepted.

import { useEffect, useState } from 'react';
import { Link, useLocation, useParams } from 'react-router';
import { PageHead } from '../../components/ui.tsx';
import { Redirect } from '../../shell/Redirect.tsx';
import { useTitle } from '../../shell/title.ts';
import { PAGE, useAibCtx } from '../AibLayout.tsx';
import type { AibStrategic, AibStrategicCompact, AibStrategicScenarioId } from '../contract.ts';
import { plural } from '../derive/format.ts';
import { compactNodeIds, strategicCompactOf } from '../derive/strategic.ts';
import { ChainSection } from './strategic/ChainSection.tsx';
import { PolicySection, ScenarioChangeSection, VulnerabilitySection, WhoSection, WhySection, WorkstreamsSection } from './strategic/sections.tsx';

/** The page's sections, in order: [anchor, title]. */
export const STRATEGIC_SECTIONS = [
  ['why', 'Why this compact exists'],
  ['chain', 'Which nodes it covers'],
  ['vulnerabilities', 'Where it is vulnerable'],
  ['policy', 'What the nodes need'],
  ['who', 'Who does what'],
  ['workstreams', 'Workstreams under this compact'],
  ['scenarios', 'What changes between scenarios'],
] as const;

export default function StrategicCompactView() {
  const { sid = '' } = useParams();
  const { strategic: s, strategicScenario, href } = useAibCtx();
  const c = s ? strategicCompactOf(s, sid) : null;
  useTitle(c ? c.name : 'Sectoral Compacts', 'aib');
  if (!s || !c || !strategicScenario) return <Redirect to={href('/compacts')} notice={`No strategic compact with the id '${sid}'; showing all compacts.`} />;
  return <Compact key={c.id} s={s} c={c} sid={strategicScenario} />;
}

function Compact({ s, c, sid }: { s: AibStrategic; c: AibStrategicCompact; sid: AibStrategicScenarioId }) {
  const { href } = useAibCtx();
  const loc = useLocation();
  const [node, setNode] = useState<string | null>(null);
  // follow a deep link's #anchor once the page has rendered (only on the first render of this compact)
  useEffect(() => {
    if (loc.hash) document.getElementById(loc.hash.slice(1))?.scrollIntoView?.({ block: 'start' });
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [c.id]);
  const facts = [
    c.family,
    plural(c.steps.length, 'step'),
    plural(compactNodeIds(c).length, 'node'),
    c.workstreams.length ? plural(c.workstreams.length, 'workstream') : 'no workstream',
  ].filter(Boolean);
  return (
    <div className={PAGE}>
      <p className="mb-4 text-sm">
        <Link className="link" to={href('/compacts')}>
          &larr; All Sectoral Compacts
        </Link>
      </p>
      <PageHead eyebrow="STRATEGIC SECTORAL COMPACT" title={c.name} lede={c.purpose ?? c.steps.map((st) => st.label).join(' → ')}>
        <p className="mt-2 text-sm text-paper-mute" data-testid="compact-facts">
          {facts.join(' · ')}
        </p>
        <nav aria-label="Sections of this compact" className="mt-4">
          <ul className="flex flex-wrap gap-x-4 gap-y-1 text-sm">
            {STRATEGIC_SECTIONS.map(([id, label]) => (
              <li key={id}>
                <a className="link" href={`#${id}`}>
                  {label}
                </a>
              </li>
            ))}
          </ul>
        </nav>
      </PageHead>
      <div className="space-y-14">
        <WhySection s={s} c={c} sid={sid} />
        <ChainSection s={s} c={c} sid={sid} node={node} onNode={setNode} />
        <VulnerabilitySection s={s} c={c} sid={sid} />
        <PolicySection s={s} c={c} sid={sid} />
        <WhoSection s={s} c={c} node={node} onNode={setNode} />
        <WorkstreamsSection s={s} c={c} sid={sid} />
        <ScenarioChangeSection s={s} c={c} sid={sid} />
      </div>
    </div>
  );
}
