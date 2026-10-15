// Draw the Fence layout: loads the Yards & Fences bundle for the assessed country, provides the
// assessment context, and shows a slim bar with what is being assessed, the methodology drawer,
// copy-link and print. A missing bundle shows the module's "No data" screen.

import { useEffect, useRef, useState } from 'react';
import { Link, Outlet, useSearchParams } from 'react-router';
import { Loading, Notice } from '../components/ui.tsx';
import { useTitle } from '../shell/title.ts';
import { decodeState } from './state/assessment.ts';
import { useYFData } from './data/store.ts';
import { useYards, YardsProvider } from './state/YardsContext.tsx';
import './yards.css';

export const PAGE = 'mx-auto w-full max-w-7xl px-4 py-8 sm:px-6 lg:px-8';

export const METHOD_STATEMENT =
  'OSI metrics describe observed economic, technological and supply-chain conditions. They do not themselves determine yard placement. Yards & Fences combines those data with explicit tests of security consequences, substitutability, industrial learning, competitiveness and policy cost.';

function MethodDrawer({ open, onClose }: { open: boolean; onClose: () => void }) {
  const ref = useRef<HTMLDialogElement>(null);
  useEffect(() => {
    const d = ref.current;
    if (!d) return;
    if (open && !d.open) d.showModal?.();
    if (!open && d.open) d.close?.();
  }, [open]);
  const { href, data } = useYards();
  return (
    <dialog ref={ref} onClose={onClose} className="yf m-auto max-w-2xl border hairline bg-ink-800 p-0 text-paper backdrop:bg-black/60" aria-labelledby="yf-method-title">
      <div className="p-6">
        <div className="flex items-start justify-between gap-4">
          <h2 id="yf-method-title" className="rule-neis-sm text-xl font-medium">
            How Draw the Fence decides
          </h2>
          <button type="button" className="btn btn-quiet btn-sm" onClick={onClose}>
            Close
          </button>
        </div>
        <p className="mt-3 text-sm leading-relaxed text-paper-dim">{METHOD_STATEMENT}</p>
        <ul className="mt-4 space-y-2 text-sm leading-relaxed text-paper-dim">
          <li>The three yards are different policy problems, not low, medium and high scores on one scale. There is no aggregate score.</li>
          <li>Small Yard requires an acute consequence and either a control layer an outsider could command, or a physical input where coercive concentration, poor substitutes and slow replacement reinforce one another.</li>
          <li>Medium Yard requires a present position, an industrial-learning case or a credible leap-ahead opportunity, weighed against the cost of protection.</li>
          <li>Large Yard is where neither case holds: openness, with the lightest tools for real concentration risk.</li>
          <li>Unknown is always allowed. Unknown inputs are bracketed, never filled: a result that depends on them is reported as indeterminate.</li>
        </ul>
        <p className="mt-4 text-xs text-paper-mute">
          Data: Sector bundle {data.manifest.inputs.sector_created.slice(0, 10)}, Strategic bundle {data.manifest.inputs.strategic_created.slice(0, 10)}, allied network build {data.manifest.inputs.network_build ?? 'n/a'}.
        </p>
        <div className="mt-5">
          <Link className="btn btn-ghost btn-sm" to={href('methodology')} onClick={onClose}>
            Full methodology
          </Link>
        </div>
      </div>
    </dialog>
  );
}

function YardsBar() {
  const { state, subjects, focus, data } = useYards();
  const [method, setMethod] = useState(false);
  const [copied, setCopied] = useState(false);
  const scenarioCount = subjects.reduce((n, s) => n + Object.keys(state.layers[s.key]?.scenario ?? {}).length, 0);
  const what = state.sel
    ? subjects.length > 1
      ? `${data.reference.stacks.find((s) => s.id === state.sel?.stack)?.label ?? 'System'} (${subjects.length} layers)`
      : focus?.label
    : null;
  const copy = async () => {
    try {
      await navigator.clipboard.writeText(window.location.href);
      setCopied(true);
      window.setTimeout(() => setCopied(false), 2000);
    } catch {
      setCopied(false);
    }
  };
  return (
    <div className="yf-no-print border-b hairline bg-ink-850">
      <div className="mx-auto flex max-w-7xl flex-wrap items-center gap-x-4 gap-y-2 px-4 py-2 text-sm sm:px-6 lg:px-8">
        <span className="eyebrow">Yards &amp; Fences · NEIS Center</span>
        {what && (
          <span className="min-w-0 truncate text-paper-dim">
            Assessing: <span className="text-paper">{what}</span> · {data.bundle.country.name}
          </span>
        )}
        {scenarioCount > 0 && <span className="yf-prov yf-prov-scenario">Scenario active: {scenarioCount} assumption{scenarioCount > 1 ? 's' : ''} changed</span>}
        <span className="ml-auto flex items-center gap-2">
          <button type="button" className="btn btn-quiet btn-sm" onClick={() => setMethod(true)}>
            Methodology
          </button>
          {state.sel && (
            <button type="button" className="btn btn-quiet btn-sm" onClick={copy} aria-live="polite">
              {copied ? 'Link copied' : 'Copy assessment link'}
            </button>
          )}
        </span>
      </div>
      <MethodDrawer open={method} onClose={() => setMethod(false)} />
    </div>
  );
}

export default function YardsLayout() {
  useTitle('', 'Draw the Fence');
  const [params] = useSearchParams();
  const country = decodeState(params.get('a')).country;
  const load = useYFData(country);
  if (load.status === 'loading')
    return (
      <div className={PAGE}>
        <Loading label="Loading the Yards & Fences evidence" />
      </div>
    );
  if (load.status === 'error')
    return (
      <div className={PAGE}>
        <Notice kind="info" title="No Yards & Fences data">
          <p>{load.message}</p>
          <p className="mt-2 text-paper-mute">
            Build it from the repository root after scripts/85 and 87: <code>Rscript scripts/89_export_yards_fences_data.R</code>
          </p>
        </Notice>
      </div>
    );
  return (
    <YardsProvider data={load.data}>
      <div className="yf">
        <YardsBar />
        <Outlet />
      </div>
    </YardsProvider>
  );
}
