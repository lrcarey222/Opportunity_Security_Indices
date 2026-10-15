// HeatTable (spec_sector.md 4.7a, 4.8 coverage, 4.6 dyad OD matrix, 5.3): an HTML table with the
// three-colour cell fill (spec_country.md C1.5: orange low, white at the median of the values shown,
// blue high; the caller's `scale`, else one over every cell of the table) and the numbers printed in
// the cells (text colour by contrast: ink or black, never white on orange), no-data cells hatched, a
// sticky first column, rows with sub-rows behind a disclosure chevron, and cells as buttons when
// interactive (the current one ringed ink under white, C1.4). Wide tables scroll inside their card.

import { Fragment, useMemo, useState, type CSSProperties } from 'react';
import { HATCH_STYLE, RING_INNER, RING_OUTER, STAGE_COLOR } from '../../theme/palette.ts';
import { triScale, triText } from '../../charts/scale.ts';
import type { HeatCol, HeatRow, HeatTableProps } from './types.ts';

const STAGE_NAME: Record<string, string> = { upstream: 'Upstream', midstream: 'Midstream', downstream: 'Downstream' };

/** Every cell value of the table, sub-rows included (the default scale's values). */
function allValues(rows: readonly HeatRow[], cols: readonly HeatCol[], cell: HeatTableProps['cell']): number[] {
  const out: number[] = [];
  const walk = (rs: readonly HeatRow[]) => {
    for (const r of rs) {
      for (const c of cols) {
        const v = cell(r.key, c.key).value;
        if (v !== null && Number.isFinite(v)) out.push(v as number);
      }
      if (r.children?.length) walk(r.children);
    }
  };
  walk(rows);
  return out;
}

export function HeatTable({ rows, cols, cell, onCell, scale, reverse, caption, rowHeader = '' }: HeatTableProps) {
  const [open, setOpen] = useState<Set<string>>(() => new Set());
  const own = useMemo(() => (scale ? null : triScale(allValues(rows, cols, cell), { reverse })), [scale, rows, cols, cell, reverse]);
  const s = scale ?? own;
  const toggle = (k: string) =>
    setOpen((s) => {
      const n = new Set(s);
      if (n.has(k)) n.delete(k);
      else n.add(k);
      return n;
    });

  const renderRow = (r: HeatRow, depth: number) => {
    const kids = r.children ?? [];
    const isOpen = open.has(r.key);
    return (
      <Fragment key={`${depth}-${r.key}`}>
        <tr>
          <th scope="row" className="sticky left-0 z-10 bg-ink-800 py-1.5 pr-3 text-left align-middle text-sm font-normal text-paper-dim" style={{ paddingLeft: 4 + depth * 16 }}>
            <span className="inline-flex items-center gap-1.5">
              {kids.length > 0 && (
                <button type="button" className="btn-quiet -ml-1 px-1" aria-expanded={isOpen} aria-label={`${isOpen ? 'Hide' : 'Show'} ${r.label} sub-rows`} onClick={() => toggle(r.key)}>
                  <span aria-hidden="true" className="inline-block transition-transform" style={{ transform: isOpen ? 'rotate(90deg)' : undefined }}>
                    ›
                  </span>
                </button>
              )}
              <span className={depth === 0 ? 'text-paper' : undefined}>{r.label}</span>
            </span>
            {r.sub && <span className="block text-xs text-paper-mute">{r.sub}</span>}
          </th>
          {cols.map((c) => {
            const v = cell(r.key, c.key);
            const has = v.value !== null && Number.isFinite(v.value);
            const bg = has && s ? s.color(v.value as number) : null;
            const style: CSSProperties = {
              ...(bg ? { backgroundColor: bg } : HATCH_STYLE),
              color: bg ? triText(bg) : '#ffffff',
              // the selected cell: white at the edge over ink inside (orange would vanish on the low end)
              outline: v.selected ? `2px solid ${RING_INNER}` : undefined,
              outlineOffset: -2,
              boxShadow: v.selected ? `inset 0 0 0 4px ${RING_OUTER}` : undefined,
              transition: 'background-color 300ms ease',
            };
            // on a hatched (no-data) cell the words sit on a solid chip: text over the stripes had a
            // contrast that changed pixel by pixel (WCAG 1.4.3)
            const chip = !bg ? 'inline-block bg-ink-800 px-1 text-paper-dim' : 'block';
            const body = (
              <>
                {v.display && <span className={`${chip} text-xs font-medium leading-tight tabular`}>{v.display}</span>}
                {v.sub && <span className={`${chip} text-[10px] leading-tight tabular`}>{v.sub}</span>}
              </>
            );
            return (
              <td key={c.key} className="border border-ink-800 p-0 align-middle" style={style} title={v.title} data-hatch={bg ? undefined : ''}>
                {onCell && !v.disabled ? (
                  <button
                    type="button"
                    className="block h-full min-h-11 w-full px-2 py-1 text-left"
                    aria-label={v.title ?? `${r.label}, ${c.label}: ${v.display || 'no data'}`}
                    aria-pressed={v.selected ? true : undefined}
                    onClick={() => onCell(r.key, c.key)}
                  >
                    {body}
                  </button>
                ) : (
                  <div className="min-h-11 px-2 py-1">
                    {body}
                  </div>
                )}
              </td>
            );
          })}
        </tr>
        {isOpen && kids.map((k) => renderRow(k, depth + 1))}
      </Fragment>
    );
  };

  // a table without buttons is made focusable so a keyboard can scroll it when it is wider than its
  // card (axe: scrollable-region-focusable)
  return (
    <div className="overflow-x-auto" {...(onCell ? {} : { tabIndex: 0, role: 'region', 'aria-label': caption })}>
      <table className="w-full border-collapse text-sm">
        <caption className="sr-only">{caption}</caption>
        <thead>
          <tr>
            <th scope="col" className="sticky left-0 z-10 bg-ink-800 pb-2 pr-3 text-left">
              <span className="eyebrow">{rowHeader}</span>
            </th>
            {cols.map((c: HeatCol) => (
              <th key={c.key} scope="col" className="min-w-[5.5rem] px-2 pb-2 text-left align-bottom">
                {c.stage && <span className="mb-1 block h-[3px] w-full" style={{ background: STAGE_COLOR[STAGE_NAME[c.stage]] }} aria-hidden="true" />}
                <span className="eyebrow block leading-tight">{c.label}</span>
              </th>
            ))}
          </tr>
        </thead>
        <tbody>{rows.map((r) => renderRow(r, 0))}</tbody>
      </table>
    </div>
  );
}
