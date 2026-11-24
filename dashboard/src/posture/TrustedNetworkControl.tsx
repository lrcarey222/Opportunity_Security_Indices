// "Trusted network: N economies · View list" (external review readiness, 2026-11-23): a small disclosure that
// names the economies the Explorer treats as eligible Allied Industrial Base partners, how membership is decided
// and the data date. Reads the posture core (shared, cached), so it costs no extra data where the posture core is
// already loaded. Definition: src/posture/trustedNetwork.ts.

import { useMemo } from 'react';
import { usePostureCore } from './store.ts';
import { NETWORK_TIER_LABEL, TRUSTED_NETWORK_METHOD, trustedNetwork, type NetworkMember } from './trustedNetwork.ts';

const TIERS: NetworkMember['tier'][] = ['core', 'partner', 'allied'];

export function TrustedNetworkControl({ className = '' }: { className?: string }) {
  const load = usePostureCore();
  const core = load.status === 'ready' ? load.data : null;
  const net = useMemo(() => (core ? trustedNetwork(core.countries) : null), [core]);
  if (!net || !core) return null;
  const date = (core.created ?? '').slice(0, 10);
  return (
    <details className={`text-sm ${className}`} data-testid="trusted-network">
      <summary className="cursor-pointer select-none">
        <span className="font-medium">Trusted network: {net.size} economies</span>
        <span className="text-paper-mute"> · View list</span>
      </summary>
      <div className="mt-2 space-y-2 max-w-prose">
        <p>{TRUSTED_NETWORK_METHOD}</p>
        {TIERS.map((t) => {
          const list = net.members.filter((m) => m.tier === t);
          if (!list.length) return null;
          return (
            <p key={t}>
              <span className="font-medium">
                {NETWORK_TIER_LABEL[t]} ({list.length}):
              </span>{' '}
              {list.map((m) => m.name).join(', ')}.
            </p>
          );
        })}
        <p className="text-paper-mute">
          Only these economies are named as partners for the Allied Industrial Base strategy. Other economies, including
          China and Russia, still appear where they are current suppliers, sources of concentration or exposure.
          {date ? ` Data release ${date}.` : ''}
        </p>
      </div>
    </details>
  );
}
