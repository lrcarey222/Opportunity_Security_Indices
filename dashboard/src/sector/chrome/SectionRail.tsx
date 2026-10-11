// SectionRail (spec_sector.md 4.3, 6, 7): the profile's section navigation, <nav aria-label="Sections">
// (id "sections", the target of the profile's "Skip to sections" link).
//   * >= 1280 px: a 200 px sticky vertical rail with scrollspy (aria-current="location"); it sticks
//     under the SectorBar (`--sector-bar-h`) and is meant for a grid column of its own (the profile
//     lays out `xl:grid-cols-[200px_minmax(0,1fr)]`), never a full-width band;
//   * 768-1279 px: a horizontally scrolling chip row;
//   * < 768 px: a "Sections" select under the SectorBar.
// Links are in-page anchors (#trade); scrolling to a section is what triggers its lazy load in the
// view. Scrollspy uses IntersectionObserver when present; `current` overrides it.

import { useEffect, useState } from 'react';
import { Select } from '../../components/ui.tsx';
import { useMediaQuery } from '../../shell/useMediaQuery.ts';

export interface RailSection {
  id: string;
  label: string;
}

export interface SectionRailProps {
  sections: RailSection[];
  /** the current section (controlled); else the scrollspy decides */
  current?: string | null;
  /** called on a pick (after the browser follows the anchor) */
  onNavigate?: (id: string) => void;
  label?: string;
}

/** The id of the topmost section intersecting the reading band, tracked with IntersectionObserver. */
export function useScrollSpy(ids: readonly string[]): string | null {
  const [active, setActive] = useState<string | null>(ids[0] ?? null);
  const key = ids.join('|');
  useEffect(() => {
    const list = key ? key.split('|') : [];
    if (typeof IntersectionObserver === 'undefined' || !list.length) return;
    const visible = new Map<string, boolean>();
    const io = new IntersectionObserver(
      (entries) => {
        for (const e of entries) visible.set(e.target.id, e.isIntersecting);
        const first = list.find((id) => visible.get(id));
        if (first) setActive(first);
      },
      { rootMargin: '-96px 0px -55% 0px', threshold: 0 },
    );
    for (const id of list) {
      const el = document.getElementById(id);
      if (el) io.observe(el);
    }
    return () => io.disconnect();
  }, [key]);
  return active;
}

function go(id: string, onNavigate?: (id: string) => void) {
  const el = document.getElementById(id);
  el?.scrollIntoView?.({ block: 'start' });
  if (typeof window !== 'undefined' && window.location.hash !== `#${id}`) window.location.hash = id;
  onNavigate?.(id);
}

export function SectionRail({ sections, current, onNavigate, label = 'Sections' }: SectionRailProps) {
  const xl = useMediaQuery('(min-width: 1280px)');
  const md = useMediaQuery('(min-width: 768px)');
  const spy = useScrollSpy(sections.map((s) => s.id));
  const active = current ?? spy;
  if (!sections.length) return null;

  if (!md) {
    return (
      <nav id="sections" tabIndex={-1} aria-label={label} className="outline-none py-2">
        <Select label={label} value={active ?? sections[0].id} options={sections.map((s) => ({ value: s.id, label: s.label }))} onChange={(id) => go(id, onNavigate)} className="w-full" />
      </nav>
    );
  }

  if (!xl) {
    return (
      <nav id="sections" tabIndex={-1} aria-label={label} className="outline-none overflow-x-auto py-2">
        <ul className="flex w-max gap-2">
          {sections.map((s) => (
            <li key={s.id}>
              <a
                href={`#${s.id}`}
                aria-current={active === s.id ? 'location' : undefined}
                className="tag min-h-8"
                onClick={() => onNavigate?.(s.id)}
              >
                {s.label}
              </a>
            </li>
          ))}
        </ul>
      </nav>
    );
  }

  return (
    <nav
      id="sections"
      tabIndex={-1}
      aria-label={label}
      className="outline-none sticky w-[200px] self-start overflow-y-auto py-2"
      style={{ top: 'calc(var(--sector-bar-h, 4.75rem) + 1rem)', maxHeight: 'calc(100vh - var(--sector-bar-h, 4.75rem) - 2rem)' }}
    >
      <div className="eyebrow mb-2">{label}</div>
      <ul className="space-y-0.5 border-l hairline">
        {sections.map((s) => (
          <li key={s.id}>
            <a
              href={`#${s.id}`}
              aria-current={active === s.id ? 'location' : undefined}
              className={`-ml-px block border-l-2 py-1 pl-3 text-sm ${active === s.id ? 'border-signal text-paper' : 'border-transparent text-paper-mute hover:text-paper'}`}
              onClick={() => onNavigate?.(s.id)}
            >
              {s.label}
            </a>
          </li>
        ))}
      </ul>
    </nav>
  );
}
