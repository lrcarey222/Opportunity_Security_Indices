// Home: "Which recommendations changed recently?" (change layer; the EconVitals-style question of
// docs/change-layer-implementation-plan.md Task 7). The latest recommendation changes across every assessment, each
// with its attribution. Says plainly when none can yet be credited to new data or to a method change alone, rather
// than headlining unattributable history. Renders nothing without change history.

import { useCallback, useEffect, useState } from 'react';
import { onIdle } from '../shell/prefetch.ts';
import { Link } from 'react-router';
import { ATTRIBUTION_LABEL, choiceLabel, vintageDate, type ChangesBundle } from './contract.ts';
import { nodeLabel } from './ChangedCellsNote.tsx';
import { useChanges } from './store.ts';
import { summarizeChanges } from './summary.ts';

export const HOME_CHANGES_SHOWN = 6;

export function HomeChangesView({ b }: { b: ChangesBundle }) {
  const all = useCallback(() => true, []);
  const s = summarizeChanges(b, all);
  if (!s.window) return null;
  const recs = s.recommendations.slice(0, HOME_CHANGES_SHOWN);
  return (
    <section aria-labelledby="home-changes" className="mt-12" data-testid="home-changes">
      <h2 id="home-changes" className="rule-neis-sm mb-3 text-xl font-medium text-paper">
        Which recommendations changed recently?
      </h2>
      {recs.length ? (
        <>
          <p className="mb-3 max-w-3xl text-sm text-paper-dim">
            {s.recommendations.length} recommendation{s.recommendations.length === 1 ? '' : 's'} changed between the {s.window.from} and {s.window.to} vintages.{' '}
            {s.attributable
              ? 'Each says whether it reflects new data or a change to the method.'
              : 'None of them can yet be credited to new data or to a method change alone: the exports that made them changed the method and did not record their input data.'}
          </p>
          <ul className="grid gap-2 sm:grid-cols-2 lg:grid-cols-3">
            {recs.map((c) => (
              <li key={c.id + c.cur} className="panel p-3 text-sm">
                <Link className="link" to={`/countries/${c.iso3}?node=${encodeURIComponent(`${c.tech}|${c.stage}`)}`}>
                  {c.iso3} · {nodeLabel(c)}
                </Link>
                <p className="mt-1 text-paper">
                  {choiceLabel(c.before.choice)} → {choiceLabel(c.after.choice)}
                </p>
                <p className="mt-1 text-xs text-paper-mute">
                  {vintageDate(b, c.cur)} · {ATTRIBUTION_LABEL[c.attribution]}
                </p>
              </li>
            ))}
          </ul>
        </>
      ) : (
        <p className="text-sm text-paper-dim">
          No recommendation changed between the {s.window.from} and {s.window.to} vintages.
        </p>
      )}
      <p className="mt-3 text-sm">
        <Link className="link" to="/changes?type=recommendation">
          Every change, with why it changed &rarr;
        </Link>
      </p>
    </section>
  );
}

function HomeChangesLoaded() {
  const { status, data } = useChanges();
  if (status !== 'ready' || !data) return null;
  return <HomeChangesView b={data} />;
}

/** Home makes no request before the browser is idle (shell/prefetch.ts): the change history loads after it. */
export function HomeChanges() {
  const [idle, setIdle] = useState(false);
  useEffect(() => onIdle(() => setIdle(true)), []);
  return idle ? <HomeChangesLoaded /> : null;
}
