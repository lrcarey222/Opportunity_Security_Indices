// The section navigation of a Country profile (spec_country.md A4.2): <nav aria-label="Sections of this
// page">. At >= 1280 px a 200 px sticky left rail with scrollspy (the profile lays out
// `xl:grid-cols-[200px_minmax(0,1fr)]`); below, a sticky row of section chips under the header that
// scrolls horizontally. Links are in-page anchors (#glance ... #notes). Country-owned (not imported from
// sector/chrome).

import { useEffect, useState, type ReactNode } from 'react';
import { useMediaQuery } from '../../shell/useMediaQuery.ts';

export interface NavSection {
  id: string;
  label: string;
}

// IA refactor 2026-10-16 (brief s.11-12): the profile's sections; old ids (space, priorities, why,
// partners) stay as anchors inside them. Since 2026-10-28 the six-stage posture chart above the sections
// is the first entry (#posture), and #position, the paper's three-stage scatter, is called "Security and
// opportunity" so "Strategic position" no longer names the legacy chart.
export const PROFILE_SECTIONS: readonly NavSection[] = [
  { id: 'posture', label: 'Strategic posture' },
  { id: 'position', label: 'Security and opportunity' },
  { id: 'opportunities', label: 'Opportunities & risks' },
  { id: 'sectors', label: 'Sector position' },
  { id: 'policy', label: 'Policy & climate' },
  { id: 'partnerships', label: 'Partnerships' },
  { id: 'aib-role', label: 'Allied Industrial Base role' },
  { id: 'memo', label: 'Memo' },
  { id: 'notes', label: 'Data notes' },
];

/** The id of the topmost section in the reading band (IntersectionObserver; the first id without it). */
export function useSectionSpy(ids: readonly string[]): string | null {
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
      { rootMargin: '-80px 0px -55% 0px', threshold: 0 },
    );
    for (const id of list) {
      const el = document.getElementById(id);
      if (el) io.observe(el);
    }
    return () => io.disconnect();
  }, [key]);
  return active;
}

/** `footer`: shown under the rail on wide screens (the profile's Perspective tag). */
export function SectionNav({ sections = PROFILE_SECTIONS, rail, footer }: { sections?: readonly NavSection[]; rail?: boolean; footer?: ReactNode }) {
  const xl = useMediaQuery('(min-width: 1280px)');
  const active = useSectionSpy(sections.map((s) => s.id));
  const asRail = rail ?? xl;
  if (!sections.length) return null;
  if (!asRail) {
    return (
      <nav aria-label="Sections of this page" className="sticky top-0 z-20 -mx-4 mb-6 overflow-x-auto border-b hairline bg-ink-900/95 px-4 py-2 sm:-mx-6 sm:px-6">
        <ul className="flex w-max gap-2">
          {sections.map((s) => (
            <li key={s.id}>
              <a href={`#${s.id}`} aria-current={active === s.id ? 'location' : undefined} className="tag min-h-8">
                {s.label}
              </a>
            </li>
          ))}
        </ul>
      </nav>
    );
  }
  return (
    <nav aria-label="Sections of this page" className="sticky top-4 w-[200px] self-start py-2">
      <div className="eyebrow mb-2">On this page</div>
      <ul className="space-y-0.5 border-l hairline">
        {sections.map((s) => (
          <li key={s.id}>
            <a
              href={`#${s.id}`}
              aria-current={active === s.id ? 'location' : undefined}
              className={`-ml-px block border-l-2 py-1 pl-3 text-sm ${active === s.id ? 'border-signal text-paper' : 'border-transparent text-paper-mute hover:text-paper'}`}
            >
              {s.label}
            </a>
          </li>
        ))}
      </ul>
      {footer}
    </nav>
  );
}
