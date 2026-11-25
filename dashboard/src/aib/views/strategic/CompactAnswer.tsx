// The answer at the top of a Strategic Sectoral Compact (Explorer usability pass, 2026-11-25): the bottleneck
// it solves, who contributes what and the gap that remains under the selected scenario, three generated lines
// (derive/compactAnswer.ts) above the page's sections; then the compact memo where one is shipped and the list
// of the trusted network the compact's "allied" economies are drawn from.

import { useMemo } from 'react';
import { Term } from '../../../components/Term.tsx';
import { proseName } from '../../../country/derive/text.ts';
import { MemoCta } from '../../../partnerships/MemoCta.tsx';
import { compactMemoOf } from '../../../partnerships/memos.ts';
import { useMemoManifest } from '../../../partnerships/memoStore.ts';
import { TrustedNetworkControl } from '../../../posture/TrustedNetworkControl.tsx';
import { useAibCtx } from '../../AibLayout.tsx';
import type { AibStrategic, AibStrategicCompact, AibStrategicScenarioId } from '../../contract.ts';
import { compactAnswer } from '../../derive/compactAnswer.ts';
import { nodeScenario } from '../../derive/scenarios.ts';
import { compactNodeIds, compactScenarioOf } from '../../derive/strategic.ts';
import { scenarioLabelOf } from './shared.tsx';

export function CompactAnswer({ s, c, sid }: { s: AibStrategic; c: AibStrategicCompact; sid: AibStrategicScenarioId }) {
  const { nodeOf, name, scenario, doc } = useAibCtx();
  const label = scenarioLabelOf(s, sid);
  const memos = useMemoManifest();
  const a = useMemo(() => {
    const steps = compactNodeIds(c).map((id) => {
      const n = nodeOf(id);
      const r = n ? nodeScenario(n, scenario) : null;
      return { id, label: n?.label ?? id, status: null, lostUsd: r?.ran ? (r.lostUsd ?? null) : null };
    });
    return compactAnswer(c, compactScenarioOf(c, sid), steps, label, (iso) => proseName(name(iso)), sid === 'managed');
  }, [c, sid, label, nodeOf, name, scenario]);
  const row = (k: string, dt: string, dd: string) => (
    <div key={k} className="contents" data-answer={k}>
      <dt className="text-paper-mute">{dt}</dt>
      <dd className="text-paper-dim">{dd}</dd>
    </div>
  );
  return (
    <div className="mt-4 max-w-3xl" data-testid="compact-answer">
      <dl className="grid gap-x-4 gap-y-1.5 border-l-2 border-signal pl-3 text-sm sm:grid-cols-[10rem_minmax(0,1fr)]">
        {row('bottleneck', 'Bottleneck', a.bottleneck)}
        {row('who', 'Who contributes', a.who)}
        {row('gap', 'Remaining gap', a.gap)}
      </dl>
      <p className="mt-1.5 text-xs text-paper-mute">
        Under {label}. Allied demand is the demand of the <Term id="coalition">allied coalition</Term>; the countries with a role are drawn from the{' '}
        <Term id="trusted_network">trusted network</Term>.
      </p>
      <TrustedNetworkControl className="mt-2" />
      <MemoCta className="mt-3" entry={compactMemoOf(memos, c.id)} kind="compact" dataCreated={doc.meta.created ?? null} />
    </div>
  );
}
