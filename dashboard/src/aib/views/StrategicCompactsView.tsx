// The Strategic Sectoral Compacts landing /compacts (2026-10-20; task s.14, s.20, s.21). What a strategic
// compact is in plain words (a lasting framework around one coherent supply chain, its taxonomy drawn from
// industrial-process knowledge rather than discovered by the model), the toggled scenario as the stress
// applied to it (Managed Interdependence the default institutional lens, Decoupling the stress test), then
// one card per compact grouped by family (views/strategic/CompactCard.tsx: only its name, supply-chain
// span, core countries with their roles, and under the scenario its mission, policy mix, active
// workstreams and headline result; "View compact", "View workstreams"), and the same compacts under every
// scenario (views/strategic/ScenarioMatrix.tsx) with a table twin and a CSV. The model's clusters stay one
// click away as the Workstreams (/compacts/workstreams). A bundle without the strategic layer goes to the
// Workstreams index, the former landing (the query and the hash kept).

import { Link, Navigate, useLocation } from 'react-router';
import { PageHead } from '../../components/ui.tsx';
import { useTitle } from '../../shell/title.ts';
import { PAGE, useAibCtx } from '../AibLayout.tsx';
import type { AibStrategic, AibStrategicCompact, AibStrategicScenarioId } from '../contract.ts';
import { plural } from '../derive/format.ts';
import { strategicCompacts } from '../derive/strategic.ts';
import { workstreamsHref } from '../nav.ts';
import { StrategicCompactCard } from './strategic/CompactCard.tsx';
import { ScenarioMatrix } from './strategic/ScenarioMatrix.tsx';
import { scenarioLabelOf, STRATEGIC_EYEBROW } from './strategic/shared.tsx';

/** The compacts grouped by family, in the export's order of first appearance (a compact without one under "Other compacts"). */
export function compactFamilies(compacts: readonly AibStrategicCompact[]): { family: string; compacts: AibStrategicCompact[] }[] {
  const out: { family: string; compacts: AibStrategicCompact[] }[] = [];
  for (const c of compacts) {
    const f = c.family || 'Other compacts';
    const g = out.find((x) => x.family === f);
    if (g) g.compacts.push(c);
    else out.push({ family: f, compacts: [c] });
  }
  return out;
}

const slug = (s: string) => s.toLowerCase().replace(/[^a-z0-9]+/g, '-').replace(/^-|-$/g, '');

export default function StrategicCompactsView() {
  useTitle('Sectoral Compacts', 'aib');
  const { strategic: s, strategicScenario: sid } = useAibCtx();
  const loc = useLocation();
  if (!s || !sid) return <Navigate to={{ pathname: '/compacts/workstreams', search: loc.search, hash: loc.hash }} replace />;
  return <Landing s={s} sid={sid} />;
}

function Landing({ s, sid }: { s: AibStrategic; sid: AibStrategicScenarioId }) {
  const { doc, href, hasScenarios, scenario } = useAibCtx();
  const compacts = strategicCompacts(s);
  const families = compactFamilies(compacts);
  const label = scenarioLabelOf(s, sid);
  const nWs = doc.compacts.length;
  const lens = !hasScenarios
    ? 'the only scenario in this data build'
    : scenario === 'managed'
      ? 'the default institutional lens; choose Decoupling above for the stress test'
      : 'the stress test; Managed Interdependence above is the default institutional lens';
  return (
    <div className={PAGE}>
      <PageHead
        eyebrow={STRATEGIC_EYEBROW}
        title="Sectoral Compacts"
        lede={`A Strategic Sectoral Compact is a lasting framework for allied cooperation around one coherent supply chain, from raw materials to finished equipment. Different parts of the chain can need different partners and different policies, so one compact can build new capacity at one step, coordinate trade at another and protect a specialized supplier at a third. The ${plural(compacts.length, 'compact')} below are grouped from how the goods are made (which steps feed which), not discovered by the model; the model’s results then fill each one in.`}
      >
        <p className="mt-3 max-w-3xl text-sm text-paper-mute" data-testid="strategic-reading">
          Shown under <span className="text-paper-dim">{label}</span> ({lens}). A compact’s supply chain and core countries stay the same in every scenario; its mission, policy
          mix, active workstreams and result show the stress the scenario applies.
        </p>
        <p className="mt-2 max-w-3xl text-sm text-paper-mute">
          Under each compact sit its{' '}
          <Link className="link" to={href(workstreamsHref())}>
            workstreams
          </Link>{' '}
          (the model’s {nWs} clusters of related nodes, partners and policy needs), then the nodes and the model evidence behind them.{' '}
          <Link className="link" to={href('/compacts/about')}>
            How compacts are derived &rarr;
          </Link>
        </p>
      </PageHead>
      <div className="space-y-12">
        {families.map((f) => (
          <section key={f.family} aria-labelledby={`family-${slug(f.family)}`} data-family={f.family}>
            <h2 id={`family-${slug(f.family)}`} className="rule-neis-sm mb-4 text-xl font-medium text-paper lg:text-2xl">
              {f.family}
            </h2>
            <ul className="grid gap-4 md:grid-cols-2 xl:grid-cols-3" aria-label={`${f.family}: strategic compacts`}>
              {f.compacts.map((c) => (
                <StrategicCompactCard key={c.id} s={s} c={c} sid={sid} />
              ))}
            </ul>
          </section>
        ))}
        <ScenarioMatrix s={s} sid={sid} />
      </div>
    </div>
  );
}
