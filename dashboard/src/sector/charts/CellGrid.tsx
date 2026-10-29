// CellGrid (spec_sector.md 4.1 Atlas, 4.6 CellPicker / cell_global layout, 4.9, 5.3): the 11 x 6
// technology x stage grid, plus the non-headline row (Semiconductors) after a divider. HTML, not SVG:
// stage columns carry a neutral 3 px stripe and their name (a stage is never a colour, design S4);
// rows carry the technology; each slot holds
// the view's tile (a link or a button). Arrow keys move focus between tiles (Home / End to the row's
// ends). Below 768 px it becomes an accordion: one <details> per technology (the first open), each
// with its stage tiles stacked, so colour is never needed to read a tile.

import { Fragment, type KeyboardEvent } from 'react';
import { useMediaQuery } from '../../shell/useMediaQuery.ts';
import type { CellGridProps } from './types.ts';

const FOCUSABLE = 'a[href], button:not([disabled]), [tabindex]:not([tabindex="-1"])';

/** Move focus to the tile at (r, c) from a keyboard event inside the grid (skips empty slots). */
function moveFocus(root: HTMLElement, r: number, c: number, dr: number, dc: number): boolean {
  const rows = Number(root.dataset.rows);
  const colsN = Number(root.dataset.cols);
  let rr = r + dr;
  let cc = c + dc;
  while (rr >= 0 && rr < rows && cc >= 0 && cc < colsN) {
    const slot = root.querySelector<HTMLElement>(`[data-slot="${rr}-${cc}"]`);
    const target = slot?.querySelector<HTMLElement>(FOCUSABLE);
    if (target) {
      target.focus();
      return true;
    }
    rr += dr;
    cc += dc;
  }
  return false;
}

export function CellGrid({ techs, stages, renderTile, selected, ariaLabel, accordion = true }: CellGridProps) {
  const wide = useMediaQuery('(min-width: 768px)');
  const headline = techs.filter((t) => t.headline);
  const other = techs.filter((t) => !t.headline);
  const ordered = [...headline, ...other];

  const onKeyDown = (e: KeyboardEvent<HTMLDivElement>) => {
    const slot = (e.target as HTMLElement).closest<HTMLElement>('[data-slot]');
    if (!slot) return;
    const [r, c] = (slot.dataset.slot ?? '0-0').split('-').map(Number);
    const root = e.currentTarget;
    let moved = false;
    if (e.key === 'ArrowRight') moved = moveFocus(root, r, c, 0, 1);
    else if (e.key === 'ArrowLeft') moved = moveFocus(root, r, c, 0, -1);
    else if (e.key === 'ArrowDown') moved = moveFocus(root, r, c, 1, 0);
    else if (e.key === 'ArrowUp') moved = moveFocus(root, r, c, -1, 0);
    else if (e.key === 'Home') moved = moveFocus(root, r, -1, 0, 1);
    else if (e.key === 'End') moved = moveFocus(root, r, stages.length, 0, -1);
    if (moved) e.preventDefault();
  };

  if (!wide && accordion) {
    return (
      <div aria-label={ariaLabel} role="group" className="space-y-2">
        {ordered.map((t, i) => (
          <details key={t.slug} className="disclosure border hairline bg-ink-850" open={i === 0}>
            <summary className="flex min-h-11 items-center gap-2 px-3 py-2 text-sm font-medium text-paper">
              <span className="disclosure-chevron text-paper-mute" aria-hidden="true">
                ›
              </span>
              {t.label}
              {!t.headline && <span className="text-xs font-normal text-paper-mute">non-headline</span>}
            </summary>
            <div className="space-y-2 px-3 pb-3">
              {stages.map((s) => {
                const tile = renderTile(t, s.slug, { selected: selected === `${t.slug}-${s.slug}` });
                if (tile === null || tile === undefined) return null;
                return (
                  <div key={s.slug} className="border-l-[3px] border-ink-500 pl-2">
                    <div className="eyebrow mb-1">{s.label}</div>
                    {tile}
                  </div>
                );
              })}
            </div>
          </details>
        ))}
      </div>
    );
  }

  return (
    <div role="group" aria-label={ariaLabel} data-rows={ordered.length} data-cols={stages.length} onKeyDown={onKeyDown} className="grid gap-2" style={{ gridTemplateColumns: `minmax(7rem, 10rem) repeat(${stages.length}, minmax(0, 1fr))` }}>
      <div aria-hidden="true" />
      {stages.map((s) => (
        <div key={s.slug} className="pb-1">
          <span className="mb-1 block h-[3px] bg-ink-500" aria-hidden="true" />
          <span className="eyebrow">{s.label}</span>
        </div>
      ))}
      {ordered.map((t, r) => (
        <Fragment key={t.slug}>
          {!t.headline && r === headline.length && (
            <div className="col-span-full mt-2 border-t hairline pt-2">
              <span className="eyebrow">Non-headline</span>
            </div>
          )}
          <div className="flex items-center pr-2 text-sm font-medium text-paper">{t.label}</div>
          {stages.map((s, c) => {
            const tile = renderTile(t, s.slug, { selected: selected === `${t.slug}-${s.slug}` });
            return (
              <div key={s.slug} data-slot={`${r}-${c}`} className="min-w-0">
                {tile}
              </div>
            );
          })}
        </Fragment>
      ))}
    </div>
  );
}
