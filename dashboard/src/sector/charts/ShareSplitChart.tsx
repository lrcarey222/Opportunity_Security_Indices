// ShareSplitChart (spec_sector.md 4.2a, 4.3 S2, 5.3): a 100% stacked bar-table. One row per
// measure (or item, or year), split into China | coalition core | coalition partners | rest of world
// | not attributed (hatched), always in that order; 2 px surface gaps between segments; direct "%"
// labels inside segments at least 9% wide (text colour by contrast); text columns aligned per row;
// optional section headers with a neutral stage stripe (never a stage colour); the highlighted economy as an orange caret above
// the bar (it never crosses a segment label). Projected rows are outlined and say "projection".
// The layout follows the chart's own width (ResizeObserver, not the viewport): from 1024 px the
// text columns sit beside a bar of at least 240 px; below, they drop under each bar as one line
// (a two-up card on a wide screen is as narrow as a tablet). The source is in each row's sub-line, so
// a "source" column is not repeated in the wide table. HTML, not SVG: rows are table rows (keyboard,
// text reflow, screen readers).
//
// BlocBar is the one-row form (the explorer's KPI strip).

import { Fragment, useLayoutEffect, useMemo, useState, type CSSProperties, type ReactNode } from 'react';
import { Link } from 'react-router';
import { SIGNAL } from '../../theme/palette.ts';
import { useMediaQuery } from '../../shell/useMediaQuery.ts';
import { groupBg, groupColor, groupLabel, SPLIT_ORDER, textOn } from './color.ts';
import { TipBox, useHtmlTip } from './HtmlTip.tsx';
import type { BlocBarProps, GroupKey, ShareSplitChartProps, ShareSplitColumn, ShareSplitRow } from './types.ts';

/** The chart width from which the text columns sit beside the bars. */
export const SPLIT_WIDE_PX = 1024;

/** The width of an element (ResizeObserver; the first layout's clientWidth where it is absent). */
function useWidth(ref: { current: HTMLElement | null }): number {
  const [w, setW] = useState(0);
  useLayoutEffect(() => {
    const el = ref.current;
    if (!el) return;
    setW(el.clientWidth);
    if (typeof ResizeObserver === 'undefined') return;
    const ro = new ResizeObserver((es) => setW(Math.round(es[0]?.contentRect.width ?? el.clientWidth)));
    ro.observe(el);
    return () => ro.disconnect();
  }, [ref]);
  return w;
}

/** The parts in order, the remainder (1 - sum) added to "not attributed"; negative or NaN parts are 0. */
export function splitSegments(parts: Partial<Record<GroupKey, number>>): { key: GroupKey; share: number }[] {
  const val = (k: GroupKey) => {
    const v = parts[k];
    return typeof v === 'number' && Number.isFinite(v) && v > 0 ? v : 0;
  };
  const listed = SPLIT_ORDER.filter((k) => k !== 'unattributed').reduce((s, k) => s + val(k), 0);
  const rest = Math.max(0, 1 - listed - val('unattributed'));
  return SPLIT_ORDER.map((k) => ({ key: k, share: k === 'unattributed' ? val(k) + (rest > 1e-9 ? rest : 0) : val(k) })).filter((s) => s.share > 0);
}

const pct1 = (x: number) => `${(x * 100).toFixed(1)}%`;
const pct0 = (x: number) => (x < 0.995 ? `${Math.round(x * 100)}%` : `${(x * 100).toFixed(0)}%`);

/** The row in words: "Exports 2024: China 43.7%, Coalition core 37.9%, …". */
export function splitText(label: string, parts: Partial<Record<GroupKey, number>>): string {
  return `${label}: ${splitSegments(parts)
    .map((s) => `${groupLabel(s.key)} ${pct1(s.share)}`)
    .join(', ')}`;
}

function SplitBar({
  parts, highlight, outlined, projection, minLabelShare, onTip, onHide, height = 24,
}: {
  parts: Partial<Record<GroupKey, number>>;
  highlight?: { offset: number; label: string };
  outlined?: boolean;
  projection?: boolean;
  minLabelShare: number;
  onTip: (e: { clientX: number; clientY: number }, content: ReactNode) => void;
  onHide: () => void;
  height?: number;
}) {
  const segs = splitSegments(parts);
  const hl = highlight && Number.isFinite(highlight.offset);
  return (
    <div
      className="relative flex w-full"
      style={{ height, outline: outlined ? `2px solid ${SIGNAL}` : undefined, outlineOffset: outlined ? 2 : undefined, marginTop: hl ? 7 : undefined }}
      aria-hidden="true"
      data-testid="split-bar"
    >
      {segs.map((s, i) => {
        const solid = !projection && s.key !== 'unattributed';
        const color = groupColor(s.key);
        const style: CSSProperties = {
          width: `${s.share * 100}%`,
          ...(projection && s.key !== 'unattributed' ? { background: 'transparent' } : groupBg(s.key)),
          border: projection && s.key !== 'unattributed' ? `2px solid ${color}` : undefined,
          // the 2 px gap in the surface colour (widths stay exact)
          boxShadow: i < segs.length - 1 ? 'inset -2px 0 0 var(--color-ink-800)' : undefined,
          transition: 'background-color 300ms ease',
        };
        const label = s.share >= minLabelShare ? pct0(s.share) : null;
        return (
          <div
            key={s.key}
            className="flex h-full min-w-0 items-center justify-center overflow-hidden text-[11px] font-medium tabular"
            style={style}
            data-part={s.key}
            data-hatch={s.key === 'unattributed' ? '' : undefined}
            onPointerMove={(e) => onTip(e, <SegTip k={s.key} share={s.share} projection={projection} />)}
            onPointerLeave={onHide}
          >
            {label && (
              <span style={{ color: solid ? textOn(color) : '#ffffff', textShadow: solid ? undefined : '0 0 2px #111827' }} className="truncate px-0.5">
                {label}
              </span>
            )}
          </div>
        );
      })}
      {hl && (
        // a caret above the bar, pointing at the highlighted economy's place: it never crosses a label
        <span
          className="pointer-events-none absolute"
          style={{ left: `calc(${Math.min(1, Math.max(0, highlight.offset)) * 100}% - 5px)`, top: -7, width: 0, height: 0, borderLeft: '5px solid transparent', borderRight: '5px solid transparent', borderTop: `7px solid ${SIGNAL}` }}
          data-testid="split-highlight"
        />
      )}
    </div>
  );
}

function SegTip({ k, share, projection }: { k: GroupKey; share: number; projection?: boolean }) {
  return (
    <div className="space-y-0.5 tabular">
      <div className="text-sm font-semibold text-paper">{pct1(share)}</div>
      <div className="text-paper-dim">{groupLabel(k)}</div>
      {projection && <div className="text-paper-mute">projection</div>}
      {k === 'unattributed' && <div className="text-paper-mute">the source&rsquo;s residual or the world total minus the listed economies</div>}
    </div>
  );
}

function RowLabel({ row, onRowClick }: { row: ShareSplitRow; onRowClick?: (r: ShareSplitRow) => void }) {
  const text = (
    <>
      <span className={row.outlined ? 'font-medium text-paper' : undefined}>{row.label}</span>
      {row.projection && <span className="tag ml-2 border-dashed px-1.5 py-0 text-[10px] uppercase tracking-wider">projection</span>}
    </>
  );
  return (
    <>
      {row.href ? (
        <Link to={row.href} className="hover:text-signal" onClick={onRowClick ? () => onRowClick(row) : undefined}>
          {text}
        </Link>
      ) : onRowClick ? (
        <button type="button" className="text-left hover:text-signal" onClick={() => onRowClick(row)}>
          {text}
        </button>
      ) : (
        text
      )}
      {row.sub && <span className="block text-xs text-paper-mute">{row.sub}</span>}
    </>
  );
}

export function ShareSplitChart({ rows, columns: allColumns, sections, onRowClick, ariaLabel, labelWidth = 220, minLabelShare = 0.09, highlightHeader }: ShareSplitChartProps) {
  const mediaWide = useMediaQuery('(min-width: 768px)');
  const { hostRef, tip, atPointer, hide } = useHtmlTip<HTMLDivElement>();
  const cw = useWidth(hostRef);
  const wide = cw > 0 ? cw >= SPLIT_WIDE_PX : mediaWide;
  // the source is in every row's sub-line; as a column it was clipped ("UN Comtrade, reconcile")
  const subHasSource = rows.every((r) => !!r.sub);
  const columns = subHasSource ? allColumns.filter((c) => c.key !== 'source') : allColumns;
  const hasHighlight = rows.some((r) => r.highlight);
  const groups = useMemo(() => {
    if (!sections?.length) return [{ section: null, rows }];
    const out = sections.map((s) => ({ section: s, rows: rows.filter((r) => r.section === s.key) })).filter((g) => g.rows.length);
    const loose = rows.filter((r) => !r.section || !sections.some((s) => s.key === r.section));
    return loose.length ? [{ section: null, rows: loose }, ...out] : out;
  }, [rows, sections]);
  const nCols = 2 + columns.length + (hasHighlight ? 1 : 0);
  const cellText = (r: ShareSplitRow, c: ShareSplitColumn) => r.cols[c.key] ?? '';

  return (
    <div ref={hostRef} className="relative" onPointerLeave={hide}>
      {wide ? (
        // focusable, so a keyboard can scroll the table when it is wider than the card (axe: scrollable-region-focusable)
        <div className="overflow-x-auto" tabIndex={0} role="region" aria-label={ariaLabel}>
          <table className="w-full border-collapse text-sm">
            <caption className="sr-only">{ariaLabel}</caption>
            <thead>
              <tr className="text-left">
                <th scope="col" className="eyebrow pb-2 pr-3 font-semibold" style={{ width: labelWidth }}>
                  Measure
                </th>
                <th scope="col" className="eyebrow pb-2 pr-3 font-semibold" style={{ width: '40%', minWidth: 240 }}>
                  <span className="sr-only">Share split</span>
                </th>
                {columns.map((c) => (
                  <th key={c.key} scope="col" className={`eyebrow whitespace-nowrap pb-2 pr-3 font-semibold ${c.num ? 'text-right' : ''}`}>
                    {c.label}
                  </th>
                ))}
                {hasHighlight && (
                  <th scope="col" className="eyebrow whitespace-nowrap pb-2 font-semibold">
                    {highlightHeader ?? 'Highlight'}
                  </th>
                )}
              </tr>
            </thead>
            {groups.map((g, gi) => (
              <tbody key={g.section?.key ?? `loose-${gi}`}>
                {g.section && (
                  <tr>
                    <th scope="colgroup" colSpan={nCols} className="pb-1 pt-3 text-left">
                      <span className="inline-flex items-center gap-2 text-xs font-semibold uppercase tracking-wider text-paper-dim">
                        {g.section.stage && <span className="inline-block h-4 w-[3px] bg-ink-500" aria-hidden="true" />}
                        {g.section.label}
                      </span>
                    </th>
                  </tr>
                )}
                {g.rows.map((r) => (
                  <tr
                    key={r.id}
                    className={`border-t border-white/5 ${onRowClick ? 'cursor-pointer hover:bg-ink-850' : ''}`}
                    aria-label={r.ariaLabel ?? splitText(r.label, r.parts)}
                    onClick={
                      onRowClick && !r.href
                        ? (e) => {
                            // the label's own button already handled it
                            if (!(e.target as Element).closest('a,button')) onRowClick(r);
                          }
                        : undefined
                    }
                  >
                    <th scope="row" className="py-2 pr-3 text-left align-middle font-normal text-paper-dim">
                      <RowLabel row={r} onRowClick={onRowClick} />
                    </th>
                    <td className="min-w-[240px] py-2 pr-3 align-middle">
                      <SplitBar parts={r.parts} highlight={r.highlight} outlined={r.outlined} projection={r.projection} minLabelShare={minLabelShare} onTip={atPointer} onHide={hide} />
                    </td>
                    {columns.map((c) => (
                      <td key={c.key} className={`whitespace-nowrap py-2 pr-3 align-middle text-paper-dim ${c.num ? 'text-right tabular' : ''}`}>
                        {cellText(r, c)}
                      </td>
                    ))}
                    {hasHighlight && <td className="whitespace-nowrap py-2 align-middle text-paper">{r.highlight?.label ?? ''}</td>}
                  </tr>
                ))}
              </tbody>
            ))}
          </table>
        </div>
      ) : (
        <div role="table" aria-label={ariaLabel}>
          {groups.map((g, gi) => (
            <Fragment key={g.section?.key ?? `loose-${gi}`}>
              {g.section && (
                <div role="row" className="pb-1 pt-3">
                  <span role="rowheader" className="inline-flex items-center gap-2 text-xs font-semibold uppercase tracking-wider text-paper-dim">
                    {g.section.stage && <span className="inline-block h-4 w-[3px] bg-ink-500" aria-hidden="true" />}
                    {g.section.label}
                  </span>
                </div>
              )}
              {g.rows.map((r) => (
                <div key={r.id} role="row" className="border-t border-white/5 py-2" aria-label={r.ariaLabel ?? splitText(r.label, r.parts)}>
                  <div role="rowheader" className="mb-1 text-sm text-paper-dim">
                    <RowLabel row={r} onRowClick={onRowClick} />
                  </div>
                  <div role="cell">
                    <SplitBar parts={r.parts} highlight={r.highlight} outlined={r.outlined} projection={r.projection} minLabelShare={minLabelShare} onTip={atPointer} onHide={hide} />
                  </div>
                  <div role="cell" className="mt-1 text-xs text-paper-mute tabular">
                    {[...columns.map((c) => (cellText(r, c) ? `${c.label} ${cellText(r, c)}` : '')), r.highlight?.label ?? '']
                      .filter(Boolean)
                      .join(' · ')}
                  </div>
                </div>
              ))}
            </Fragment>
          ))}
        </div>
      )}
      <TipBox tip={tip} hostWidth={hostRef.current?.clientWidth ?? 0} />
    </div>
  );
}

/** A one-row share split (the KPI strip's bloc bar), with the parts in words below it. */
export function BlocBar({ parts, ariaLabel, label, highlight, minLabelShare = 0.09 }: BlocBarProps) {
  const { hostRef, tip, atPointer, hide } = useHtmlTip<HTMLDivElement>();
  const segs = splitSegments(parts);
  return (
    <figure ref={hostRef} className="relative" aria-label={ariaLabel} onPointerLeave={hide}>
      {label && <div className="mb-1 text-xs text-paper-mute">{label}</div>}
      <SplitBar parts={parts} highlight={highlight} minLabelShare={minLabelShare} onTip={atPointer} onHide={hide} height={20} />
      <figcaption className="mt-1.5 flex flex-wrap gap-x-4 gap-y-1 text-xs text-paper-dim tabular">
        {segs.map((s) => (
          <span key={s.key}>
            {groupLabel(s.key)} {pct1(s.share)}
          </span>
        ))}
        {highlight && <span className="text-paper">{highlight.label}</span>}
      </figcaption>
      <TipBox tip={tip} hostWidth={hostRef.current?.clientWidth ?? 0} />
    </figure>
  );
}
