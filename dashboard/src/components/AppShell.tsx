// Strategic Insights chrome (spec_react.md 8.4; spec_sector.md 2.5): the sticky status strip
// (baseline / scenario badge, chips for every moved setting, the Show control, the compute status in a
// live region, data notes), the banners, the dial panel (sticky sidebar >= 1024 px, a drawer below)
// and the Dials button. The header, the route notice and the footer moved to the shared shell
// (src/shell/AppRoot.tsx), which also owns <main>.

import { useCallback, useEffect, useRef, useState, type ReactNode } from 'react';
import { DialPanel, DIAL_SPECS } from './DialPanel.tsx';
import { Notice, Segmented } from './ui.tsx';
import { dialMultiplier } from '../engine/params.ts';
import { useAdvancedChanged, useApp, useEngine, useRun, useScenario } from '../state/EngineProvider.tsx';
import { useMediaQuery } from '../shell/useMediaQuery.ts';
import { cloneAdvanced, type ShowMode } from '../state/url.ts';
import { fmtBytes, fmtMs, fmtMult } from '../lib/format.ts';

const CHUNK_LABEL: Record<string, string> = { vars: 'variable data', partner: 'partnership data', network: 'network data', core: 'core data' };

function useComputeStatus(): { text: string; busy: boolean; error: string | null } {
  const { state } = useEngine();
  const { status, core, partner, network } = useRun();
  const { key, isBaseline } = useScenario();
  const app = useApp();
  const loading = (['vars', 'partner', 'network'] as const).find((c) => state.chunks[c].state === 'loading');
  if (loading) {
    const info = app.manifest.chunks.find((c) => c.name === loading);
    return { text: `Loading ${CHUNK_LABEL[loading]}${info ? `, ${fmtBytes(info.bytes_gz)}` : ''}`, busy: true, error: null };
  }
  if (status.phase === 'error' && status.error) return { text: `Error: ${status.error.message}`, busy: false, error: status.error.message };
  if (status.phase === 'running' || status.phase === 'waiting') {
    const p = status.progress;
    if (p?.layer === 'network') {
      if (p.phase === 'portfolio') return { text: `Network: portfolio caps, iteration ${p.iter ?? 1}, stage ${p.current} of ${p.total}`, busy: true, error: null };
      if (p.phase === 'inputs') return { text: 'Network: building inputs', busy: true, error: null };
      return { text: `Network: stage ${Math.min(p.total, p.current + 1)} of ${p.total}`, busy: true, error: null };
    }
    if (p?.layer === 'partner') return { text: `Recomputing partnerships (${p.phase})`, busy: true, error: null };
    return { text: 'Recomputing', busy: true, error: null };
  }
  const fresh = [core, partner, network].filter((r) => r && r.key === key);
  if (!fresh.length) return { text: 'Starting', busy: true, error: null };
  if (isBaseline) return { text: 'Paper baseline: exact values from the export', busy: false, error: null };
  const ms = status.ms;
  return { text: `Updated${ms !== null ? ` in ${fmtMs(ms)}` : ''}`, busy: false, error: null };
}

function StatusStrip({ onOpenDials, showDialButton }: { onOpenDials: () => void; showDialButton: boolean }) {
  const app = useApp();
  const { scenario, isBaseline, view, setView, setDial, setAdvanced } = useScenario();
  const { retry } = useRun();
  const ch = useAdvancedChanged();
  const st = useComputeStatus();
  const d = app.urlCtx.defaults;
  const grid = app.manifest.dial_grid;
  const chips: { label: string; title: string; reset: () => void }[] = [];
  for (const s of DIAL_SPECS) {
    const v = scenario.dials[s.key];
    if (v !== 50) chips.push({ label: `${s.label.toUpperCase()} ${v} ${fmtMult(dialMultiplier(v, grid))}`, title: `Reset ${s.label.toLowerCase()} to 50`, reset: () => setDial(s.key, 50, { replace: false }) });
  }
  const adv = (label: string, f: (a: ReturnType<typeof cloneAdvanced>) => void) =>
    chips.push({ label, title: `Reset ${label.toLowerCase()} to the paper's setting`, reset: () => setAdvanced((a) => { f(a); return a; }) });
  if (ch.es) adv('ES weights', (a) => (a.es_weights = { ...d.es_weights }));
  if (ch.eo) adv('EO weights', (a) => (a.eo_weights = { ...d.eo_weights }));
  if (ch.sample) adv(scenario.advanced.tau === null ? 'Rankability off' : `Rankability tau ${scenario.advanced.tau}`, (a) => (a.tau = d.tau));
  if (ch.fossil) adv(`Fossil ${scenario.advanced.fossil}`, (a) => (a.fossil = d.fossil));
  // the endowment screen of the allied picks (URL key `screen`); the chip names the moved state
  if (ch.screen) adv(scenario.advanced.require_resource_base ? 'Endowment screen on' : 'Endowment screen off', (a) => (a.require_resource_base = d.require_resource_base));
  if (ch.psi) adv(`PSI view ${scenario.advanced.psi_view}`, (a) => (a.psi_view = d.psi_view));
  if (ch.net) adv('Network settings', (a) => (a.network = { ...d.network }));
  const notes = app.manifest.notes ?? [];
  return (
    <div className="sticky top-0 z-30 border-b hairline bg-ink-850">
      <div className="mx-auto flex max-w-7xl flex-wrap items-center gap-x-4 gap-y-2 px-4 py-2 sm:px-6 lg:px-8">
        {showDialButton && (
          <button type="button" className="btn btn-primary btn-sm" onClick={onOpenDials} aria-haspopup="dialog">
            <svg aria-hidden="true" viewBox="0 0 16 16" className="mr-1.5 h-3.5 w-3.5" fill="currentColor">
              <rect x="1" y="3" width="14" height="1.5" />
              <rect x="1" y="11.5" width="14" height="1.5" />
              <rect x="9" y="1" width="3" height="5.5" />
              <rect x="4" y="9.5" width="3" height="5.5" />
            </svg>
            Dials
          </button>
        )}
        <span className={`tag ${isBaseline ? 'border-signal text-paper' : 'tag-framework'}`}>{isBaseline ? 'Paper baseline' : 'Scenario'}</span>
        {chips.length > 0 && (
          <ul className="flex flex-wrap gap-1.5" aria-label="Changed settings">
            {chips.map((c) => (
              <li key={c.label} className="tag tabular">
                {c.label}
                <button type="button" className="ml-1 text-paper-mute hover:text-signal" onClick={c.reset} title={c.title} aria-label={c.title}>
                  {'×'}
                </button>
              </li>
            ))}
          </ul>
        )}
        <div className="ml-auto flex flex-wrap items-center gap-x-4 gap-y-2">
          <Segmented<ShowMode>
            label="Show"
            hideLabel
            value={isBaseline ? 'scenario' : view.show}
            disabled={isBaseline}
            title={isBaseline ? 'Move a dial to compare' : 'Show the scenario, the paper baseline or the change'}
            options={[
              { value: 'scenario', label: 'Scenario' },
              { value: 'baseline', label: 'Baseline' },
              { value: 'change', label: 'Change' },
            ]}
            onChange={(v) => setView({ show: v })}
          />
          <span className="flex items-center gap-2 text-xs text-paper-mute" role="status" aria-live="polite">
            {st.busy && <span className="inline-block h-1.5 w-1.5 animate-pulse bg-signal" aria-hidden="true" />}
            {st.text}
            {st.error && (
              <button type="button" className="btn-quiet" onClick={retry}>
                Retry
              </button>
            )}
          </span>
          {notes.length > 0 && (
            <details className="disclosure relative text-xs">
              <summary className="text-paper-mute hover:text-paper">Data notes ({notes.length})</summary>
              <ul className="panel absolute right-0 z-40 mt-2 w-[min(28rem,85vw)] list-disc space-y-1.5 p-4 pl-7 text-paper-dim">
                {notes.map((n) => (
                  <li key={n}>{n}</li>
                ))}
              </ul>
            </details>
          )}
        </div>
      </div>
    </div>
  );
}

function Banners() {
  const { state, client } = useEngine();
  const { problems, dismissProblems } = useScenario();
  const { retry } = useRun();
  const failed = Object.values(state.selfcheck).filter((s) => s && !s.ok);
  const chunkErr = (['vars', 'partner', 'network'] as const).filter((c) => state.chunks[c].state === 'error');
  if (!problems.length && !failed.length && !chunkErr.length) return null;
  return (
    <div className="mx-auto mt-4 flex max-w-7xl flex-col gap-2 px-4 sm:px-6 lg:px-8">
      {problems.length > 0 && (
        <Notice kind="warning" title="Ignored in the link" onDismiss={dismissProblems}>
          {problems.join('; ')}
        </Notice>
      )}
      {failed.length > 0 && (
        <Notice kind="error" title="Recomputed values differ from the export: scenario results may be wrong">
          {failed.map((f) => `${f.layer}: ${f.detail ?? `max |diff| ${f.max_abs_diff}`}`).join('; ')}. The baseline view still shows the exported values. See the Data page.
        </Notice>
      )}
      {chunkErr.map((c) => (
        <Notice
          key={c}
          kind="error"
          title={`Could not load the ${CHUNK_LABEL[c]}`}
          action={
            <button
              type="button"
              className="btn btn-ghost btn-sm"
              onClick={() => {
                void client.load(c).then(retry, () => undefined);
              }}
            >
              Retry
            </button>
          }
        >
          {state.chunks[c].error ?? 'The request failed.'}
        </Notice>
      ))}
    </div>
  );
}

function DialDrawer({ open, onClose }: { open: boolean; onClose: () => void }) {
  const ref = useRef<HTMLDivElement>(null);
  useEffect(() => {
    if (!open) return;
    const prev = document.activeElement as HTMLElement | null;
    ref.current?.focus();
    const onKey = (e: KeyboardEvent) => {
      if (e.key === 'Escape') onClose();
    };
    document.addEventListener('keydown', onKey);
    return () => {
      document.removeEventListener('keydown', onKey);
      prev?.focus?.();
    };
  }, [open, onClose]);
  if (!open) return null;
  return (
    <div className="fixed inset-0 z-50 flex" role="dialog" aria-modal="true" aria-label="Scenario dials">
      <div className="absolute inset-0 bg-black/60" onClick={onClose} aria-hidden="true" />
      <div ref={ref} tabIndex={-1} className="relative h-full w-[min(22rem,92vw)] overflow-y-auto border-r hairline bg-ink-800 p-5 outline-none">
        <div className="mb-2 flex justify-end">
          <button type="button" className="btn btn-ghost btn-sm" onClick={onClose}>
            Done
          </button>
        </div>
        <DialPanel />
      </div>
    </div>
  );
}

/** The Strategic chrome around a Strategic view (inside AppRoot's <main>). */
export function StrategicChrome({ children }: { children: ReactNode }) {
  const wide = useMediaQuery('(min-width: 1024px)');
  const [drawer, setDrawer] = useState(false);
  const closeDrawer = useCallback(() => setDrawer(false), []);
  useEffect(() => {
    if (wide) setDrawer(false);
  }, [wide]);
  return (
    <>
      <StatusStrip showDialButton={!wide} onOpenDials={() => setDrawer(true)} />
      <Banners />
      <div className="mx-auto w-full max-w-7xl flex-1 px-4 sm:px-6 lg:grid lg:grid-cols-[320px_minmax(0,1fr)] lg:gap-8 lg:px-8">
        {wide && (
          <aside aria-label="Scenario dials" className="panel sticky top-16 mt-8 max-h-[calc(100vh-5rem)] self-start overflow-y-auto p-5">
            <DialPanel />
          </aside>
        )}
        <div className="min-w-0 py-8 lg:py-10" data-strategic-content="">
          {children}
        </div>
      </div>
      {!wide && <DialDrawer open={drawer} onClose={closeDrawer} />}
    </>
  );
}

/** The old name (spec_react.md 8.4). */
export const AppShell = StrategicChrome;
