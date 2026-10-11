// SectorChartCard and MeasureCaption (spec_sector.md 3.9, 4.0, 5.3): ChartCard with provenance as a
// REQUIRED prop. `vars` names the measures the card shows; the caption resolver (CaptionProvider,
// from the dictionary) gives the sub line "{label} · {unit} · {year} · {source} · {n} economies", the
// vintage badge, the caveats (two inline, the rest to the SectorBar "Data notes"), the coverage line
// and, when the vars differ in year or source, the note that says so. Card titles are h3 by default
// (profile sections are h2). Everything else is ChartCard: table twin, CSV, summary, "Updating".

import { useId, type ReactNode } from 'react';
import { ChartCard, type ChartCardProps } from '../../charts/ChartCard.tsx';
import { useAddPageNotes } from '../chrome/pageNotes.tsx';
import { useCaption } from './caption.tsx';
import type { CaptionInfo, VarRef } from './types.ts';

export const INLINE_CAVEATS = 2;

/** The sub line (and the coverage line under it) of a card. Inline content, safe inside <p>. */
export function MeasureCaption({ caption }: { caption: CaptionInfo }) {
  return (
    <>
      <span className="block">{caption.sub}</span>
      {caption.coverage && <span className="mt-0.5 block text-xs">{caption.coverage}</span>}
    </>
  );
}

/** The vintage badge ("2024", "2020 → 2024", "2035 · projection"). */
export function VintageBadge({ text }: { text: string }) {
  const projection = /projection/i.test(text);
  return <span className={`tag tabular ${projection ? 'border-dashed' : ''}`}>{text}</span>;
}

export type SectorChartCardProps = Omit<ChartCardProps, 'sub' | 'badge'> & {
  /** the measures shown (required: no chart without provenance) */
  vars: readonly VarRef[];
  /** field-by-field overrides of the resolved caption (e.g. a coverage line computed by the view) */
  caption?: Partial<CaptionInfo>;
  /** extra text after the sub line */
  subExtra?: ReactNode;
};

export function SectorChartCard({ vars, caption: override, subExtra, notes, headingLevel = 3, ...rest }: SectorChartCardProps) {
  const owner = useId();
  const caption = useCaption(vars, override);
  const caveats = caption.caveats ?? [];
  const inline = caveats.slice(0, INLINE_CAVEATS);
  useAddPageNotes(owner, caveats.slice(INLINE_CAVEATS));
  const hasNotes = inline.length > 0 || !!caption.spanNote || !!notes;
  return (
    <ChartCard
      {...rest}
      headingLevel={headingLevel}
      badge={caption.badge ? <VintageBadge text={caption.badge} /> : undefined}
      sub={
        <>
          <MeasureCaption caption={caption} />
          {subExtra}
        </>
      }
      notes={
        hasNotes ? (
          <div className="space-y-1">
            {caption.spanNote && <p>{caption.spanNote}</p>}
            {inline.map((c) => (
              <p key={c}>{c}</p>
            ))}
            {notes}
          </div>
        ) : undefined
      }
    />
  );
}
