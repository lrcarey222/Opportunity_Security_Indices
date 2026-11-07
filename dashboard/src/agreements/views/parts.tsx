// Shared pieces of the Agreements page: the depth chip and legend, the grade mark (established vs mentioned
// in text: a glyph and a word, never colour alone), code lists with their basis, the existence and delivery
// tiles (kept apart), the provenance line of an agreement (every source dataset, the record links, the
// number of sources, the confidence and the primary source), the absence caveat, and the agreements table.

import type { ReactNode } from 'react';
import { TableView, type Col } from '../../components/TableView.tsx';
import type { AgrAgreement, AgrCode, AgrVocab, CodeGrade } from '../contract.ts';
import {
  ABSENCE_CAVEAT, BASIS_WORD, bestGrades, dateKey, dateWords, deliveryWords, DEPTH_FILL, DEPTH_INK, depthName, existenceWords, GRADE_GLYPH, GRADE_WORD, isAcronym, labelOf,
  nameOf, PRECISION_WORD, primarySource, sourceShort, type Scope,
} from '../derive.ts';
import type { Depth } from '../contract.ts';

export const PAGE = 'mx-auto w-full max-w-7xl px-4 py-6 sm:px-6 lg:px-8 lg:py-8';
export const EYEBROW = 'ALLIED INDUSTRIAL BASE';

/** The absence caveat, wherever an empty result or depth class 0 is shown. */
export function AbsenceCaveat({ className }: { className?: string }) {
  return (
    <p className={`text-xs text-paper-mute ${className ?? ''}`} data-testid="absence-caveat">
      {ABSENCE_CAVEAT}
    </p>
  );
}

/** An empty result with the caveat. */
export function NoneFound({ children }: { children?: ReactNode }) {
  return (
    <div className="px-1 py-4 text-sm text-paper-mute">
      {children && <p className="mb-1 text-paper-dim">{children}</p>}
      <AbsenceCaveat />
    </div>
  );
}

/** A depth class: a swatch with its number and name (colour never alone). */
export function DepthChip({ d, v, compact }: { d: number; v: AgrVocab; compact?: boolean }) {
  const k = (d >= 0 && d <= 5 ? d : 0) as Depth;
  return (
    <span className="inline-flex items-center gap-1.5 whitespace-nowrap" data-depth={k}>
      <span className="inline-flex h-5 min-w-5 items-center justify-center px-1 text-xs font-medium tabular" style={{ background: DEPTH_FILL[k], color: DEPTH_INK[k] }} aria-hidden="true">
        {k}
      </span>
      <span className={compact ? 'sr-only' : 'text-paper-dim'}>
        Class {k} · {depthName(v, k)}
      </span>
    </span>
  );
}

export function DepthLegend({ v }: { v: AgrVocab }) {
  return (
    <ul className="flex flex-wrap gap-x-4 gap-y-1.5 text-xs text-paper-dim" aria-label="Partnership depth classes">
      {([0, 1, 2, 3, 4, 5] as const).map((d) => (
        <li key={d} className="inline-flex items-center gap-1.5">
          <span className="inline-flex h-4 min-w-4 items-center justify-center px-0.5 text-[0.625rem] font-medium" style={{ background: DEPTH_FILL[d], color: DEPTH_INK[d] }} aria-hidden="true">
            {d}
          </span>
          {d} {depthName(v, d)}
        </li>
      ))}
    </ul>
  );
}

/** The grade mark: "● established" or "○ mentioned in text". */
export function GradeMark({ grade, short }: { grade: CodeGrade; short?: boolean }) {
  return (
    <span className={grade === 'specific' ? 'text-paper' : 'text-paper-mute'} data-grade={grade}>
      <span aria-hidden="true">{GRADE_GLYPH[grade]}</span> {short ? <span className="sr-only">{GRADE_WORD[grade]}</span> : GRADE_WORD[grade]}
    </span>
  );
}

/** The two grades in words, for a legend. */
export function GradeLegend() {
  return (
    <p className="text-xs text-paper-mute" data-testid="grade-legend">
      <GradeMark grade="specific" />: coded by the source, recorded by hand from the official text, implied by the agreement type or, for a sector, named in the title (a title alone never establishes an instrument).{' '}
      <GradeMark grade="text" />: only in the body text of a document; shown, but never counted as established.
    </p>
  );
}

/** The codes of an agreement (sectors or instruments) with their grade and basis. */
export function CodeList({ codes, kind, v }: { codes: readonly AgrCode[]; kind: 'sectors' | 'instruments'; v: AgrVocab }) {
  const g = bestGrades(codes, v, kind);
  if (!g.size) return <span className="text-paper-mute">none recorded</span>;
  const rows = [...g].sort(([, a], [, b]) => (a.grade === b.grade ? 0 : a.grade === 'specific' ? -1 : 1));
  return (
    <ul className="flex flex-col gap-0.5">
      {rows.map(([code, x]) => (
        <li key={code} title={x.bases.map((b) => BASIS_WORD[b] ?? b).join('; ')} data-grade={x.grade}>
          <span aria-hidden="true" className={x.grade === 'specific' ? 'text-paper' : 'text-paper-mute'}>
            {GRADE_GLYPH[x.grade]}
          </span>{' '}
          <span className={x.grade === 'specific' ? 'text-paper-dim' : 'text-paper-mute'}>{labelOf(v, kind, code)}</span>{' '}
          <span className="text-[0.6875rem] text-paper-mute">({x.grade === 'specific' ? x.bases.filter((b) => b !== 'text_body').map((b) => BASIS_WORD[b] ?? b).join('; ') : 'mentioned in text'})</span>
        </li>
      ))}
    </ul>
  );
}

/** Existence (announced, signed, in force; binding or not). */
export function ExistsLine({ a }: { a: AgrAgreement }) {
  return (
    <span className="text-paper-dim" data-testid="exists">
      {existenceWords(a)}
    </span>
  );
}

/** Delivery (only a source that records delivery sets it): muted when there is none. */
export function DeliveryLine({ a, v, links = true }: { a: AgrAgreement; v: AgrVocab; links?: boolean }) {
  const d = deliveryWords(a, v);
  return (
    <span className={d.has ? 'text-paper' : 'text-paper-mute'} data-testid="delivery" data-delivery={d.has ? 'yes' : 'no'}>
      {d.words}
      {links && a.delivery.length > 0 && (
        <ul className="mt-0.5 list-disc pl-4 text-xs text-paper-dim">
          {a.delivery.map((x, i) => (
            <li key={i}>
              {x.url ? (
                <a className="link" href={x.url} target="_blank" rel="noreferrer noopener">
                  {x.text}
                </a>
              ) : (
                x.text
              )}
            </li>
          ))}
        </ul>
      )}
    </span>
  );
}

/** A source's short name, an <abbr> with its full name when it is an acronym. */
export function SourceName({ id, v }: { id: string; v: AgrVocab }) {
  const s = sourceShort(v, id);
  const full = labelOf(v, 'sources', id);
  return isAcronym(s) ? <abbr title={full}>{s}</abbr> : <span title={full}>{s}</span>;
}

/** Provenance: each source dataset (linked to its record where there is a link), the number of sources, the confidence and the primary source. */
export function Provenance({ a, v }: { a: AgrAgreement; v: AgrVocab }) {
  const primary = primarySource(a, v);
  return (
    <span className="text-xs text-paper-mute" data-testid="provenance">
      {a.sources.map((s, i) => (
        <span key={`${s.dataset}-${s.record_id}`}>
          {i > 0 && ', '}
          {s.url ? (
            <a className="link" href={s.url} target="_blank" rel="noreferrer noopener" title={`Record ${s.record_id}`}>
              <SourceName id={s.dataset} v={v} />
            </a>
          ) : (
            <span title={`Record ${s.record_id}`}>
              <SourceName id={s.dataset} v={v} />
            </span>
          )}
        </span>
      ))}
      {' · '}
      {a.source_count === 1 ? '1 source' : `${a.source_count} sources`} · <span title={labelOf(v, 'confidence_levels', a.confidence)}>confidence {a.confidence}</span>
      <span className="block" data-testid="primary-source" data-primary={primary.kind}>
        Primary source:{' '}
        {primary.url ? (
          <a className="link" href={primary.url} target="_blank" rel="noreferrer noopener">
            {primary.words}
          </a>
        ) : (
          primary.words
        )}
      </span>
    </span>
  );
}

/** The parties other than the focal economy (and the EU, for an agreement reached through it). */
export function partiesWords(a: AgrAgreement, focal: string | null, names: Readonly<Record<string, string>>): string {
  const others = a.parties.filter((p) => p !== focal);
  if (others.length > 6) return `${others.length} parties: ${others.slice(0, 5).map((p) => nameOf(names, p)).join(', ')} and ${others.length - 5} more`;
  return others.map((p) => nameOf(names, p)).join(', ');
}

/** The agreements table: one row per agreement, existence and delivery in separate columns, provenance on every row. */
export function AgreementsTable({ rows, v, names, focal, caption, pageSize = 10 }: { rows: AgrAgreement[]; v: AgrVocab; names: Readonly<Record<string, string>>; focal: string | null; caption: string; pageSize?: number }) {
  if (!rows.length) return <NoneFound>No agreement matches these filters.</NoneFound>;
  const cols: Col<AgrAgreement>[] = [
    {
      id: 'title',
      header: 'Agreement',
      value: (a) => a.title,
      minWidth: 220,
      cell: (a) => (
        <div>
          <div className="text-paper">{a.title}</div>
          <div className="text-xs text-paper-mute">
            {partiesWords(a, focal, names)}
            {a.via_eu && <span className="ml-1 tag">through the EU</span>}
          </div>
        </div>
      ),
    },
    {
      id: 'date',
      header: 'Date',
      value: (a) => a.date ?? '',
      sortValue: (a) => dateKey(a.date),
      cell: (a) => (
        <span className="whitespace-nowrap">
          {dateWords(a.date) ?? 'undated'}
          <span className="block text-xs text-paper-mute">{PRECISION_WORD[a.date_precision] ?? a.date_precision}</span>
        </span>
      ),
    },
    { id: 'type', header: 'Type', value: (a) => labelOf(v, 'agreement_types', a.type), cell: (a) => <span>{labelOf(v, 'agreement_types', a.type)}{a.label ? <span className="block text-xs text-paper-mute">{labelOf(v, 'partnership_labels', a.label)}</span> : null}</span> },
    { id: 'sectors', header: 'Sectors', value: (a) => a.sectors.map(([c]) => c).join(' '), sortable: false, minWidth: 180, cell: (a) => <CodeList codes={a.sectors} kind="sectors" v={v} /> },
    { id: 'instruments', header: 'Instruments', value: (a) => a.instruments.map(([c]) => c).join(' '), sortable: false, minWidth: 180, cell: (a) => <CodeList codes={a.instruments} kind="instruments" v={v} /> },
    { id: 'exists', header: 'Exists', value: (a) => existenceWords(a), sortable: false, minWidth: 170, cell: (a) => <ExistsLine a={a} /> },
    { id: 'delivery', header: 'Delivery', value: (a) => deliveryWords(a, v).words, sortable: false, minWidth: 170, cell: (a) => <DeliveryLine a={a} v={v} /> },
    { id: 'sources', header: 'Sources', value: (a) => a.sources.map((s) => s.dataset).join(' '), sortable: false, minWidth: 150, cell: (a) => <Provenance a={a} v={v} /> },
  ];
  return <TableView rows={rows} cols={cols} caption={caption} pageSize={pageSize} rowKey={(a) => a.id} search={rows.length > 10} wrapHeaders />;
}

/** The scope in a few words, for headings ("electro-industrial agreements"). */
export const scopeNoun = (s: Scope): string => (s === 'ei' ? 'electro-industrial agreements' : 'agreements');
