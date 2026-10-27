// One Strategic Sectoral Compact as a card on the landing (/compacts; 2026-10-20, task s.14): only its
// name, its supply-chain span and core countries with their roles (the persistent architecture, the same
// in every scenario), then, under the toggled scenario, its mission, policy mix, active workstreams and
// headline resilience result; "View compact" and "View workstreams". The policy mix carries a small
// square per node in value-chain order (aria-hidden; the words say the same).

import { Link } from 'react-router';
import { useAibCtx } from '../../AibLayout.tsx';
import type { AibStrategic, AibStrategicCompact, AibStrategicScenarioId } from '../../contract.ts';
import { activeWorkstreamsOf, compactScenarioOf, policyMixWords } from '../../derive/strategic.ts';
import { strategicCompactHref, workstreamsHref } from '../../nav.ts';
import { activeWords, CoreCountries, countsWords, Fact, headlineBody, interventionWords, MiniSpine, scenarioLabelOf, SpanList } from './shared.tsx';

export function StrategicCompactCard({ s, c, sid }: { s: AibStrategic; c: AibStrategicCompact; sid: AibStrategicScenarioId }) {
  const { name, href } = useAibCtx();
  const sc = compactScenarioOf(c, sid);
  const label = scenarioLabelOf(s, sid);
  const active = activeWorkstreamsOf(c, sid);
  const counts = countsWords(sc);
  const hid = `sc-card-${c.id}`;
  return (
    <li data-testid="strategic-compact-card" data-compact={c.id}>
      <article className="card flex h-full flex-col p-5" aria-labelledby={hid}>
        <h3 id={hid} className="text-lg font-medium leading-snug text-paper">
          <Link className="hover:text-signal" to={href(strategicCompactHref(c.id))}>
            {c.name}
          </Link>
        </h3>
        <dl className="mt-3 grid grid-cols-[6.5rem_minmax(0,1fr)] gap-x-3 gap-y-2 text-sm" data-testid="compact-persistent">
          <Fact term="Supply chain">
            <SpanList c={c} />
          </Fact>
          <Fact term="Core countries">
            <CoreCountries core={c.core} name={name} href={null} label={`Core countries of ${c.name} and their roles`} />
          </Fact>
        </dl>
        <div className="mt-4 border-t hairline pt-3" data-testid="compact-under-scenario">
          <p className="eyebrow text-paper-mute">Under {label}</p>
          <dl className="mt-2 grid grid-cols-[6.5rem_minmax(0,1fr)] gap-x-3 gap-y-2 text-sm">
            <Fact term="Mission">{sc?.statement ?? '–'}</Fact>
            <Fact term="Policy mix">
              <span className="block text-paper">{policyMixWords(sc)}</span>
              <span className="mt-1 flex flex-wrap items-center gap-x-2 gap-y-1 text-xs text-paper-mute" title={counts}>
                <MiniSpine c={c} sc={sc} />
                <span>{interventionWords(c, sc)}</span>
                <span className="sr-only">: {counts}</span>
              </span>
            </Fact>
            <Fact term="Workstreams">{activeWords(c, active)}</Fact>
            <Fact term="Result">
              {headlineBody(sc?.headline, label)}
              {sc?.stressNote && <span className="mt-1 block text-xs text-paper-mute">{sc.stressNote}</span>}
            </Fact>
          </dl>
        </div>
        <p className="mt-auto flex flex-wrap gap-x-5 gap-y-1 pt-4 text-sm">
          <Link className="link" to={href(strategicCompactHref(c.id))} aria-label={`View compact: ${c.name}`}>
            View compact &rarr;
          </Link>
          <Link className="link" to={href(workstreamsHref(undefined, c.id))} aria-label={`View workstreams: ${c.name}`}>
            View workstreams &rarr;
          </Link>
        </p>
      </article>
    </li>
  );
}
