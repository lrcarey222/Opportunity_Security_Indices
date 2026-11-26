// Home: two EconVitals-style questions answered from measured trade (posture/signals.ts), for the trusted network:
//   "Where are exports growing?"           latest 12 months vs the 12 before, momentum quality ok only
//   "Where is dependence on China rising?" China's import share, latest 12 months vs the base year
// Trends in trade, said plainly not to be changes in the assessment. Renders nothing when the bundle has no signals.

import { useEffect, useState } from 'react';
import { Link } from 'react-router';
import { stageDisplay } from '../lib/stages.ts';
import { monthLabel } from './detail.ts';
import { pct, pctChange, type PostureSignals } from './signals.ts';
import { postureStore } from './store.ts';

export const HOME_SIGNALS_SHOWN = 5;

const cellHref = (c: { iso3: string; tech: string; stage: string }) => `/countries/${c.iso3}?node=${encodeURIComponent(`${c.tech}|${c.stage}`)}`;

export function HomeSignalsView({ s }: { s: PostureSignals }) {
  const e = s.exports;
  const d = s.dependence;
  const growing = e.growing_trusted.slice(0, HOME_SIGNALS_SHOWN);
  const rising = d.rising_trusted.slice(0, HOME_SIGNALS_SHOWN);
  const through = monthLabel(d.window_end);
  if (!growing.length && !rising.length) return null;
  return (
    <section aria-labelledby="home-signals" className="mt-12" data-testid="home-signals">
      <h2 id="home-signals" className="rule-neis-sm mb-2 text-xl font-medium text-paper">
        What is moving in trade?
      </h2>
      <p className="mb-4 max-w-3xl text-sm text-paper-mute">
        Measured trade for the trusted network, not changes to the assessment. Each line opens that economy&rsquo;s step.
      </p>
      <div className="grid gap-6 lg:grid-cols-2">
        <div className="panel p-4" data-testid="home-signals-exports">
          <h3 className="text-base font-medium text-paper">Where are exports growing?</h3>
          <p className="mt-1 text-xs text-paper-mute">
            {e.period ?? 'The latest 12 months'} against {e.previous_period ?? 'the 12 before'}: exports rose by more than {Math.round((Math.exp(e.band) - 1) * 100)}% in{' '}
            {e.n_up_trusted} trusted-network cells and fell as much in {e.n_down_trusted}. The largest exporters among them, by world share; small bases and
            incomplete reporting are left out.
          </p>
          <ul className="mt-3 space-y-1.5 text-sm">
            {growing.map((r) => (
              <li key={`${r.iso3}|${r.tech}|${r.stage}`} className="flex flex-wrap items-baseline justify-between gap-x-3">
                <Link className="link" to={cellHref(r)}>
                  {r.name} · {r.tech} · {stageDisplay(r.stage)}
                </Link>
                <span className="text-paper-dim">
                  {pctChange(r.growth)}
                  {r.capped ? ' or more' : ''}
                  {r.world_share !== null && <span className="text-paper-mute"> · {pct(r.world_share)} of world exports</span>}
                </span>
              </li>
            ))}
          </ul>
        </div>
        <div className="panel p-4" data-testid="home-signals-dependence">
          <h3 className="text-base font-medium text-paper">Where is dependence on China rising?</h3>
          <p className="mt-1 text-xs text-paper-mute">
            China&rsquo;s share of imports{through ? ` in the 12 months to ${through}` : ' in the latest 12 months'} against the base year: up 10 points or more (on
            $100m+ of imports) in {d.n_china_up_trusted} trusted-network cells, down as much in {d.n_china_down_trusted}.
          </p>
          <ul className="mt-3 space-y-1.5 text-sm">
            {rising.map((r) => (
              <li key={`${r.iso3}|${r.tech}|${r.stage}`} className="flex flex-wrap items-baseline justify-between gap-x-3">
                <Link className="link" to={cellHref(r)}>
                  {r.name} · {r.tech} · {stageDisplay(r.stage)}
                </Link>
                <span className="text-paper-dim">
                  China {pct(r.china_base)} → {pct(r.china_latest)}
                  {r.source === 'ember_fill' && <span className="text-paper-mute"> (estimated)</span>}
                </span>
              </li>
            ))}
          </ul>
        </div>
      </div>
    </section>
  );
}

export function HomeSignals() {
  const [s, setS] = useState<PostureSignals | null>(null);
  useEffect(() => {
    let alive = true;
    postureStore()
      .signals()
      .then(
        (v) => alive && setS(v),
        () => undefined,
      );
    return () => {
      alive = false;
    };
  }, []);
  return s ? <HomeSignalsView s={s} /> : null;
}
