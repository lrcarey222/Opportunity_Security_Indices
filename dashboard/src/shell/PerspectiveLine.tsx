// The one-line perspective of a page that reads the engine without the dial panel (IA refactor
// 2026-10-16, plan 4 and 5.5): "Perspective: NEIS baseline", or "Perspective: custom (Hawkishness 80, …)"
// with "Reset to NEIS baseline". The dials themselves live in the Country profile; a link from there
// carries them here (the h / b / p / g keys). Needs a ScenarioProvider (EngineScope).

import { DIAL_SPECS } from '../components/DialPanel.tsx';
import { useAdvancedChanged, useScenario } from '../state/EngineProvider.tsx';

/** "Hawkishness 80, Climate 70" (+ "advanced settings"), or '' at the baseline. */
export function perspectiveSummary(dials: Readonly<Record<string, number>>, advancedMoved: boolean): string {
  const parts = DIAL_SPECS.filter((s) => dials[s.key] !== 50).map((s) => `${s.label} ${dials[s.key]}`);
  if (advancedMoved) parts.push('advanced settings');
  return parts.join(', ');
}

export function PerspectiveLine() {
  const { scenario, isBaseline, reset } = useScenario();
  const ch = useAdvancedChanged();
  const summary = perspectiveSummary(scenario.dials, Object.values(ch).some(Boolean));
  return (
    <div className="border-b hairline bg-ink-850">
      <div className="mx-auto flex max-w-7xl flex-wrap items-center gap-x-3 gap-y-1 px-4 py-2 text-xs text-paper-mute sm:px-6 lg:px-8">
        <span className="flex flex-wrap items-center gap-x-3 gap-y-1" role="status" aria-live="polite">
          <span>Perspective:</span>
          <span className={`tag ${isBaseline ? 'border-signal text-paper' : 'tag-framework'}`}>{isBaseline ? 'NEIS baseline' : 'Custom perspective'}</span>
          {!isBaseline && summary && <span className="tabular">{summary}</span>}
        </span>
        {!isBaseline && (
          <>
            <button type="button" className="btn btn-ghost btn-sm" onClick={reset}>
              Reset to NEIS baseline
            </button>
          </>
        )}
        <span className="basis-full sm:basis-auto">
          The evidence (trade, production, concentration, compacts) and the strategic choices are the same under every perspective; only the Legacy OSI priorities
          and partner scores follow it.
        </span>
      </div>
    </div>
  );
}
