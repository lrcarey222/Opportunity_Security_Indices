// The scenario toggle (2026-10-19; reordered 2026-10-20): a radio group "Managed Interdependence |
// Decoupling" (Segmented: native radios, so arrow keys move the choice and each option has its label;
// Managed Interdependence, the default institutional lens, first; Decoupling, the stress test, second), the
// chosen scenario's short words, a link to "How the scenarios work" (About) and, on a line of its own, the
// framing: a strategic compact is the persistent architecture, the scenario the stress applied to it
// (strategic.framing when the bundle has it, else STRATEGIC_FRAMING), and which choice is the lens and which
// the stress test. AibLayout shows it on every /aib and /compacts page; it reads and writes the URL key
// `scn` (scenarioUrl.ts) and prefetches the Managed Interdependence file on hover, focus or touch. Baseline
// is not a choice: every comparison shows it beside the two.

import { Link } from 'react-router';
import { Segmented } from '../components/ui.tsx';
import type { AibMeta } from './contract.ts';
import { scenarioCaveat, scenarioInfo, SCENARIO_LENS, SCENARIOS_ABOUT_HREF, STRATEGIC_FRAMING } from './derive/scenarios.ts';
import { SCENARIO_TOGGLES, withScenario, type AibToggle } from './scenarioUrl.ts';

export interface ScenarioToggleProps {
  value: AibToggle;
  onChange: (s: AibToggle) => void;
  meta?: AibMeta | null;
  /** a hover, focus or touch on the control: start loading what the other choice needs */
  onIntent?: () => void;
  /** the About link (default /compacts/about#scenarios, the choice carried) */
  aboutHref?: string;
  /** the framing sentence (default STRATEGIC_FRAMING; pass strategic.framing when the bundle has it) */
  framing?: string | null;
}

/** The framing line: the sentence, then which choice is the default view and which the stress test. */
export function scenarioFramingText(meta: AibMeta | null | undefined, framing?: string | null): string {
  const [lens, stress] = SCENARIO_TOGGLES.map((id) => scenarioInfo(meta, id).label);
  return `${framing || STRATEGIC_FRAMING} ${lens} is ${SCENARIO_LENS.managed}; ${stress} is ${SCENARIO_LENS.decoupling}.`;
}

export function ScenarioToggle({ value, onChange, meta, onIntent, aboutHref, framing }: ScenarioToggleProps) {
  const options = SCENARIO_TOGGLES.map((id) => {
    const s = scenarioInfo(meta, id);
    return { value: id, label: s.label, title: s.words };
  });
  const chosen = scenarioInfo(meta, value);
  return (
    <section
      aria-label="Scenario choice"
      className="card flex flex-col gap-2 p-3"
      data-testid="scenario-toggle"
      onPointerEnter={onIntent}
      onFocusCapture={onIntent}
      onTouchStart={onIntent}
    >
      <div className="flex flex-col gap-2 sm:flex-row sm:items-center sm:gap-4">
        <div className="shrink-0">
          <Segmented label="Scenario" value={value} options={options} onChange={onChange} hideLabel />
        </div>
        <p className="min-w-0 text-sm leading-snug text-paper-dim" aria-live="polite">
          <span className="font-medium text-paper">{chosen.label}:</span> {chosen.words}{' '}
          <span className="text-paper-mute">{scenarioCaveat(meta)}</span>{' '}
          <Link className="link whitespace-nowrap" to={aboutHref ?? withScenario(SCENARIOS_ABOUT_HREF, value)}>
            How the scenarios work &rarr;
          </Link>
        </p>
      </div>
      <p className="text-xs leading-snug text-paper-mute" data-testid="scenario-framing">
        {scenarioFramingText(meta, framing)}
      </p>
    </section>
  );
}
