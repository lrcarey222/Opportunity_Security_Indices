// The reviewer's scratchpad (docs/QC_UX_REVIEW.md section 16): autosaved notes on the case, available from the
// brief to the wrap-up, never required. Each note is timestamped, so a note written before the blind answer stays
// distinguishable from one written after the reveal. At the end (the wrap step, shown only when there are notes)
// each note can be converted into a data issue, missing context, a methodological concern or a general comment;
// a data issue can also be sent as a full data-issue report through the existing IssueReportDialog. The notes
// live in the flow state (review/flow.ts add_note / edit_note / delete_note / convert_note); the parent saves them.

import { lazy, Suspense, useState } from 'react';
import { notePhase, MAX_NOTES, MAX_NOTE_LENGTH, type CaseFlowStateV2, type FlowActionV2 } from '../../flow.ts';
import { NOTE_CONVERSION_LABEL, type NoteConversion } from '../../model.ts';
import { ChipChoice } from './BlindQuestion.tsx';

const IssueReportDialog = lazy(() => import('../IssueReportDialog.tsx').then((m) => ({ default: m.IssueReportDialog })));

export const KEEP_AS_NOTE = 'keep';

export interface NotesPanelProps {
  s: Pick<CaseFlowStateV2, 'notes' | 'blind' | 'step'>;
  dispatch: (a: FlowActionV2) => void;
  newId: () => string;
  now: () => string;
  defaultOpen?: boolean;
}

export function NotesPanel({ s, dispatch, newId, now, defaultOpen = false }: NotesPanelProps) {
  const [active, setActive] = useState<string | null>(null);
  const notes = s.notes;
  const current = active ? (notes.find((n) => n.id === active) ?? null) : null;
  const others = notes.filter((n) => n.id !== active);
  const full = notes.length >= MAX_NOTES;
  const write = (text: string) => {
    if (current) dispatch({ type: 'edit_note', id: current.id, text, at: now() });
    else if (text.trim() && !full) {
      const id = newId();
      dispatch({ type: 'add_note', id, text, at: now() });
      setActive(id);
    }
  };
  const filled = notes.filter((n) => n.text.trim()).length;
  return (
    <details className="disclosure panel mb-5 px-4" open={defaultOpen || undefined} data-testid="notes-panel">
      <summary className="flex items-center gap-2 py-2.5 text-sm font-medium text-paper-dim hover:text-paper">
        <span className="disclosure-chevron text-paper-mute" aria-hidden="true">
          ›
        </span>
        <span className="flex-1">Your notes{filled ? ` (${filled})` : ''}</span>
        <span className="text-xs font-normal text-paper-mute">saved as you type · optional</span>
      </summary>
      <div className="pb-3">
        <p className="mb-2 text-xs text-paper-mute">Jot down anything as you read: a doubtful number, missing context, a thought on the method. At the end you can turn notes into reports.</p>
        <label htmlFor="note-current" className="sr-only">
          {current ? 'Edit this note' : 'New note'}
        </label>
        <textarea
          id="note-current"
          className="field"
          rows={3}
          maxLength={MAX_NOTE_LENGTH}
          placeholder={full ? 'The note limit is reached.' : 'Write a note'}
          disabled={full && !current}
          value={current?.text ?? ''}
          onChange={(e) => write(e.target.value)}
          data-testid="note-input"
        />
        {current && (
          <button type="button" className="btn btn-ghost btn-sm mt-1.5" onClick={() => setActive(null)}>
            Start another note
          </button>
        )}
        {others.length > 0 && (
          <ul className="mt-3 space-y-2" aria-label="Your earlier notes">
            {others.map((n) => (
              <li key={n.id} className="border-t hairline pt-2" data-note={n.id}>
                <div className="mb-1 flex flex-wrap items-center justify-between gap-2 text-xs text-paper-mute">
                  <span>{notePhase(n, s) === 'before' ? 'Written before your answer' : 'Written after the reveal'}</span>
                  <span className="flex gap-2">
                    <button type="button" className="btn-quiet text-xs" onClick={() => setActive(n.id)}>
                      Edit
                    </button>
                    <button type="button" className="btn-quiet text-xs" onClick={() => dispatch({ type: 'delete_note', id: n.id })} aria-label={`Delete note: ${n.text.slice(0, 40)}`}>
                      Delete
                    </button>
                  </span>
                </div>
                <p className="whitespace-pre-wrap text-sm text-paper-dim">{n.text}</p>
              </li>
            ))}
          </ul>
        )}
      </div>
    </details>
  );
}

export interface NotesWrapProps {
  s: Pick<CaseFlowStateV2, 'notes' | 'blind'>;
  dispatch: (a: FlowActionV2) => void;
  /** For a data-issue report: the case's Explorer address, its conclusion label and the campaign. */
  target: string;
  displayedResult: string;
  campaignId: string | null;
}

export function NotesWrap({ s, dispatch, target, displayedResult, campaignId }: NotesWrapProps) {
  const [reporting, setReporting] = useState<string | null>(null);
  const notes = s.notes.filter((n) => n.text.trim());
  const options = [...(Object.keys(NOTE_CONVERSION_LABEL) as NoteConversion[]).map((k) => ({ value: k as string, label: NOTE_CONVERSION_LABEL[k] })), { value: KEEP_AS_NOTE, label: 'Keep as a note' }];
  const reportNote = notes.find((n) => n.id === reporting) ?? null;
  return (
    <div data-testid="notes-wrap">
      <p className="mb-4 max-w-3xl text-sm text-paper-mute">Your notes travel with this review. Say what each one is, so NEIS can route it; this is optional.</p>
      <ul className="space-y-4">
        {notes.map((n) => (
          <li key={n.id} className="panel px-4 py-3" data-wrap-note={n.id}>
            <p className="mb-1 text-xs text-paper-mute">{notePhase(n, s) === 'before' ? 'Written before your answer' : 'Written after the reveal'}</p>
            <p className="mb-2 whitespace-pre-wrap text-sm text-paper">{n.text}</p>
            <ChipChoice<string>
              legend="Turn this note into"
              options={options}
              value={n.converted_to ?? KEEP_AS_NOTE}
              onChange={(v) => dispatch({ type: 'convert_note', id: n.id, to: v === KEEP_AS_NOTE ? null : (v as NoteConversion) })}
            />
            {n.converted_to === 'data_issue' && (
              <button type="button" className="btn btn-ghost btn-sm" onClick={() => setReporting(n.id)} data-testid="report-note">
                Also send it as a full data-issue report
              </button>
            )}
          </li>
        ))}
      </ul>
      {reportNote && (
        <Suspense fallback={null}>
          <IssueReportDialog open onClose={() => setReporting(null)} target={target} displayedResult={displayedResult} campaignId={campaignId} initialText={reportNote.text} />
        </Suspense>
      )}
    </div>
  );
}

export default NotesPanel;
