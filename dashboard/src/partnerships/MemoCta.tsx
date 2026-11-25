// "Download partnership memo" / "Download compact memo": the shipped memo of a pair or a Strategic Sectoral
// Compact (memos.ts), as PDF and Word links with one line on what it holds and when it was written. Renders
// nothing until the memo manifest loads, or when the pair or compact has no memo.

import { MEMO_CONTENTS, memoProvenance, type MemoEntry, type MemoManifest } from './memos.ts';
import { memoFileUrl } from './memoStore.ts';

export function MemoCta({ entry, kind, dataCreated, className = '' }: { entry: MemoEntry | null; kind: 'pair' | 'compact'; dataCreated?: string | null; className?: string }) {
  if (!entry) return null;
  const label = kind === 'pair' ? 'Download partnership memo' : 'Download compact memo';
  const brief = entry.format === 'brief';
  return (
    <div className={`text-sm ${className}`} data-testid="memo-cta">
      <p className="flex flex-wrap items-center gap-x-3 gap-y-2">
        {entry.pdf && (
          <a className="btn btn-primary" href={memoFileUrl(entry.pdf)} download data-memo-file="pdf">
            {label} (PDF)
          </a>
        )}
        {entry.docx && (
          <a className="link" href={memoFileUrl(entry.docx)} download data-memo-file="docx">
            Word version
          </a>
        )}
      </p>
      <p className="mt-1 max-w-prose text-xs text-paper-mute">
        {brief ? `${MEMO_CONTENTS[kind]} ` : ''}
        {memoProvenance(entry, dataCreated)}
      </p>
    </div>
  );
}

/** Every shipped compact memo as a PDF link (the Sectoral Compacts landing: a memo can span several compacts). */
export function CompactMemoList({ manifest, className = '' }: { manifest: MemoManifest | null; className?: string }) {
  const list = Object.entries(manifest?.compacts ?? {}).filter(([, e]) => e.pdf);
  if (!list.length) return null;
  return (
    <p className={`text-sm text-paper-mute ${className}`} data-testid="compact-memos">
      <span className="text-paper-dim">Compact memos (PDF):</span>{' '}
      {list.map(([id, e], i) => (
        <span key={id}>
          {i > 0 && ' · '}
          <a className="link" href={memoFileUrl(e.pdf as string)} download>
            {e.title ?? id}
          </a>
        </span>
      ))}
      . Decision briefs written from the Explorer data{list.some(([, e]) => e.verified === false) ? '; not independently verified' : ''}.
    </p>
  );
}
