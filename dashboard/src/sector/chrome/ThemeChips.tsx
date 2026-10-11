// ThemeChips (spec_sector.md 4.6, 6, 7): the explorer's ten themes as <nav aria-label="Themes"> of
// .tag links; the active one has an orange border and aria-current="page". On phones the row scrolls
// inside its own box instead of wrapping (no horizontal page scroll).

import { Link } from 'react-router';

export interface ThemeChip {
  slug: string;
  label: string;
  /** the link (the view keeps c and eu) */
  href: string;
}

export interface ThemeChipsProps {
  themes: ThemeChip[];
  active: string | null;
  label?: string;
}

export function ThemeChips({ themes, active, label = 'Themes' }: ThemeChipsProps) {
  return (
    <nav aria-label={label} className="-mx-4 overflow-x-auto px-4 pb-1 sm:mx-0 sm:px-0">
      <ul className="flex w-max gap-2 md:w-auto md:flex-wrap">
        {themes.map((t) => {
          const on = t.slug === active;
          return (
            <li key={t.slug}>
              <Link to={t.href} aria-current={on ? 'page' : undefined} className="tag min-h-8">
                {t.label}
              </Link>
            </li>
          );
        })}
      </ul>
    </nav>
  );
}
