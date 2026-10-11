// A chart in a card (spec_react.md 8.7): `card` panel, `rule-neis-sm`, title, sub, tools, the figure
// or its table twin (TABLE / CHART toggle), legend, notes and "Download CSV". While a new result
// computes, the frame keeps the previous render at reduced opacity with an "Updating" label.
// Additive for Sector Insights (spec_sector.md 5.2): headingLevel (2 | 3) and a title-row badge.

import { useId, useState, type ReactNode } from 'react';
import { downloadText, fileSlug } from '../lib/download.ts';
import { withBom } from '../lib/csv.ts';

export interface ChartCardProps {
  title: ReactNode;
  sub?: ReactNode;
  tools?: ReactNode;
  /** the figure */
  children: ReactNode;
  /** the table twin (rendered only when shown) */
  table?: () => ReactNode;
  legend?: ReactNode;
  notes?: ReactNode;
  /** CSV of the chart's data */
  csv?: () => { name: string; text: string } | null;
  updating?: boolean;
  /** an accessible summary of what the figure shows */
  summary?: string;
  className?: string;
  /** the title's heading level: 2 (default; Strategic) or 3 (a card inside an h2 section, Sector profiles) */
  headingLevel?: 2 | 3;
  /** a badge in the title row, after the title (Sector: the vintage tag, "2024", "2035 · projection") */
  badge?: ReactNode;
}

export function ChartCard({ title, sub, tools, children, table, legend, notes, csv, updating, summary, className, headingLevel = 2, badge }: ChartCardProps) {
  const [showTable, setShowTable] = useState(false);
  const id = useId();
  const Heading = headingLevel === 3 ? 'h3' : 'h2';
  return (
    <section className={`card p-5 ${className ?? ''}`} aria-labelledby={`${id}-t`}>
      <div className="mb-3 flex flex-wrap items-start gap-x-4 gap-y-2">
        {/* basis 15rem: on a phone the tools (selects, Table) wrap below the title instead of squeezing it */}
        <div className="min-w-0 grow basis-60">
          {badge ? (
            // the badge sits beside the heading, outside its accessible name
            <div className="flex flex-wrap items-end gap-x-2 gap-y-1">
              <Heading id={`${id}-t`} className="rule-neis-sm text-base font-medium text-paper lg:text-lg">
                {title}
              </Heading>
              <span className="mb-0.5 inline-flex">{badge}</span>
            </div>
          ) : (
            <Heading id={`${id}-t`} className="rule-neis-sm text-base font-medium text-paper lg:text-lg">
              {title}
            </Heading>
          )}
          {sub && <p className="mt-1 text-sm text-paper-mute">{sub}</p>}
        </div>
        <div className="flex flex-wrap items-center gap-2">
          {updating && (
            <span className="eyebrow" role="status">
              Updating
            </span>
          )}
          {tools}
          {table && (
            <button type="button" className="btn-quiet" aria-pressed={showTable} onClick={() => setShowTable((v) => !v)}>
              {showTable ? 'Chart' : 'Table'}
            </button>
          )}
        </div>
      </div>
      <figure className={updating ? 'chart-updating' : undefined} aria-label={summary}>
        {showTable && table ? table() : children}
        {summary && <figcaption className="sr-only">{summary}</figcaption>}
      </figure>
      {legend && !showTable && <div className="mt-3">{legend}</div>}
      {(notes || csv) && (
        <div className="mt-3 flex flex-wrap items-start justify-between gap-3 text-xs text-paper-mute">
          <div className="min-w-0 flex-1">{notes}</div>
          {csv && (
            <button
              type="button"
              className="btn-quiet"
              onClick={() => {
                const c = csv();
                if (c) downloadText(withBom(c.text), `${fileSlug(c.name)}.csv`);
              }}
            >
              Download CSV
            </button>
          )}
        </div>
      )}
    </section>
  );
}
