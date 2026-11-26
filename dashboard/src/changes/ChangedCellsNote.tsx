// "What changed" on a conclusion card (Sector strategy card, Country bottom line, a compact's answer, a partnership's
// case): the recommendation changes among the card's cells over the recent vintages, how many other assessments
// moved, and how the changes are attributed. A quiet line when nothing changed; nothing when the build has no change
// history. Details live on the cell drawer and /changes.

import { useMemo } from 'react';
import { Link } from 'react-router';
import { stageDisplay } from '../lib/stages.ts';
import { choiceLabel, type ChangeRecord, type ChangesBundle } from './contract.ts';
import { useChanges } from './store.ts';
import { summarizeChanges } from './summary.ts';

export interface ChangedCellsNoteProps {
  /** which change records belong to the card */
  match: (c: ChangeRecord) => boolean;
  /** how a changed cell is named on this card ("Japan", "Batteries · Cell Manufacturing", ...) */
  label: (c: ChangeRecord) => string;
  /** the /changes link's query (e.g. "?country=JPN") */
  query?: string;
  /** at most this many changes are named */
  limit?: number;
  className?: string;
  testid?: string;
}

/** The note for a loaded bundle (exported for tests). */
export function ChangedCellsNoteView({ b, match, label, query = '', limit = 4, className = 'mt-3', testid = 'changed-cells' }: ChangedCellsNoteProps & { b: ChangesBundle }) {
  const s = useMemo(() => summarizeChanges(b, match), [b, match]);
  if (!s.window) return null;
  const recs = s.recommendations;
  if (!recs.length) {
    return (
      <p className={`${className} text-xs text-paper-mute`} data-testid={testid} data-state="none">
        What changed: no recommendation here changed between the {s.window.from} and {s.window.to} vintages
        {s.otherMoves ? `; ${s.otherMoves} assessment${s.otherMoves === 1 ? '' : 's'} moved in other ways` : ''}.{' '}
        <Link className="link" to={`/changes${query}`}>
          What changed &rarr;
        </Link>
      </p>
    );
  }
  const named = recs.slice(0, limit);
  return (
    <div className={`${className} text-xs text-paper-mute`} data-testid={testid} data-state="changed">
      <p>
        <span className="text-paper-dim">What changed since {s.window.from}:</span> the recommendation changed for {recs.length} assessment
        {recs.length === 1 ? '' : 's'} here
        {s.otherMoves ? `, and ${s.otherMoves} other${s.otherMoves === 1 ? '' : 's'} moved` : ''}.
      </p>
      <ul className="mt-1 space-y-0.5">
        {named.map((c) => (
          <li key={c.id + c.cur}>
            {label(c)}: {choiceLabel(c.before.choice)} → <span className="text-paper-dim">{choiceLabel(c.after.choice)}</span>
          </li>
        ))}
        {recs.length > named.length && <li>and {recs.length - named.length} more</li>}
      </ul>
      <p className="mt-1">
        {s.attribution.map((a) => `${a.label}: ${a.n}`).join(' · ')}
        {!s.attributable && ' (none of these can yet be credited to new data or to a method change alone)'}.{' '}
        <Link className="link" to={`/changes${query}${query ? '&' : '?'}type=recommendation`}>
          All changes &rarr;
        </Link>
      </p>
    </div>
  );
}

export function ChangedCellsNote(p: ChangedCellsNoteProps) {
  const { status, data } = useChanges();
  if (status !== 'ready' || !data) return null;
  return <ChangedCellsNoteView b={data} {...p} />;
}

/** "Batteries · Cell Manufacturing" */
export const nodeLabel = (c: { tech: string; stage: string }): string => `${c.tech} · ${stageDisplay(c.stage)}`;
