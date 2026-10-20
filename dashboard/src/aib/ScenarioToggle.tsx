// The scenario toggle (2026-10-19): a radio group "Decoupling | Managed Interdependence" (Segmented: native
// radios, so arrow keys move the choice and each option has its label), the chosen scenario's short words
// and a link to "How the scenarios work" (About). AibLayout shows it on every /aib and /compacts page; it
// reads and writes the URL key `scn` (scenarioUrl.ts) and prefetches the Managed Interdependence file on
// hover, focus or touch (never on load). Baseline is not a choice: every comparison shows it beside the two.

import { Link } from 'react-router';
import { Segmented } from '../components/ui.tsx';
import type { AibMeta } from './contract.ts';
import { scenarioCaveat, scenarioInfo, SCENARIOS_ABOUT_HREF } from './derive/scenarios.ts';
import { SCENARIO_TOGGLES, withScenario, type AibToggle } from './scenarioUrl.ts';

export interface ScenarioToggleProps {
  value: AibToggle;
  onChange: (s: AibToggle) => void;
  meta?: AibMeta | null;
  /** a hover, focus or touch on the control: start loading what the other choice needs */
  onIntent?: () => void;
  /** the About link (default /compacts/about#scenarios, the choice carried) */
  aboutHref?: string;
}

export function ScenarioToggle({ value, onChange, meta, onIntent, aboutHref }: ScenarioToggleProps) {
  const options = SCENARIO_TOGGLES.map((id) => {
    const s = scenarioInfo(meta, id);
    return { value: id, label: s.label, title: s.words };
  });
  const chosen = scenarioInfo(meta, value);
  return (
    <section
      aria-label="Scenario choice"
      className="card flex flex-col gap-2 p-3 sm:flex-row sm:items-center sm:gap-4"
      data-testid="scenario-toggle"
      onPointerEnter={onIntent}
      onFocusCapture={onIntent}
      onTouchStart={onIntent}
    >
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
    </section>
  );
}
