// The shared header (spec_sector.md 2.2).
//
//   Tier 1 (every page): the NEIS logo (neiscenter.org), a hairline, the eyebrow "Allied Industrial
//          Base Explorer" linking to Home, and the module tabs <nav aria-label="Modules"> (active:
//          white, 2 px orange underline, aria-current="true"; none on Home).
//   Tier 2 (inside a module, >= 1024 px): <nav aria-label="{module} views"> with the links the module
//          registered (ModuleNav.tsx), or the static fallback until it does.
//   Below 1024 px: the logo and a Menu button; the panel pushes the content down and lists the
//          modules (the current one with the orange tab and the word "current"), the active
//          module's views and a link to Home.

import { useEffect, useState } from 'react';
import { Link } from 'react-router';
import { Logo } from '../components/Logo.tsx';
import { useMediaQuery } from './useMediaQuery.ts';
import { MODULES, placeOf, SECTOR_NAV, sectorCellOfPath, sectorNavKeyOf, STRATEGIC_NAV, strategicNavOf, YARDS_NAV, yardsNavKeyOf, type ModuleId } from './modules.ts';
import { useModuleNav, type ModuleNavItem } from './ModuleNav.tsx';
import { sectorCarry, sectorProfileHref, sectorTabHref, strategicCarry, strategicTabHref, yardsTabHref } from './lastLocation.ts';
import { prefetchStrategic } from './prefetch.ts';

/** The tier-2 links of a module: the registered ones, else the static list; `active` filled in. */
export function moduleItems(module: ModuleId, registered: { module: ModuleId | null; items: readonly ModuleNavItem[] }, pathname: string, search: string): ModuleNavItem[] {
  let items: ModuleNavItem[];
  if (registered.module === module && registered.items.length) items = registered.items.map((i) => ({ ...i }));
  else if (module === 'strategic') {
    const q = strategicCarry(search);
    items = STRATEGIC_NAV.map((n) => ({ key: n.to, label: n.label, to: n.to + q }));
  } else if (module === 'yards') {
    const a = new URLSearchParams(search).get('a');
    items = YARDS_NAV.map((n) => ({ key: n.key, label: n.label, to: n.to + (a ? `?a=${encodeURIComponent(a)}` : '') }));
  } else {
    const q = sectorCarry(search);
    // PROFILE: the cell on screen, else the last visited one (the store updates after this render)
    const cell = sectorCellOfPath(pathname);
    const profile = cell ? `/sector/${cell.tech}/${cell.stage}` : sectorProfileHref();
    items = SECTOR_NAV.map((n) => ({ key: n.key, label: n.label, to: (n.key === 'profile' ? profile : n.to) + q }));
  }
  const current = module === 'strategic' ? strategicNavOf(pathname) : module === 'yards' ? yardsNavKeyOf(pathname) : sectorNavKeyOf(pathname);
  return items.map((i) => ({ ...i, active: i.active ?? i.key === current }));
}

const VIEWS_LABEL: Record<ModuleId, string> = { sector: 'Sector Insights views', strategic: 'Strategic Insights views', yards: 'Draw the Fence views' };

function prefetchProps(id: ModuleId) {
  return id === 'strategic' ? { onPointerEnter: prefetchStrategic, onFocus: prefetchStrategic, onTouchStart: prefetchStrategic } : {};
}

export function Header({ pathname, search }: { pathname: string; search: string }) {
  const wide = useMediaQuery('(min-width: 1024px)');
  const [open, setOpen] = useState(false);
  const registered = useModuleNav();
  const place = placeOf(pathname);
  const module: ModuleId | null = place === 'sector' || place === 'strategic' || place === 'yards' ? place : null;
  const items = module ? moduleItems(module, registered, pathname, search) : [];
  const tabs = MODULES.map((m) => ({
    ...m,
    href: m.id === 'sector' ? sectorTabHref(pathname, search) : m.id === 'yards' ? yardsTabHref(pathname, search) : strategicTabHref(pathname, search),
    current: m.id === module,
  }));

  // the menu closes on navigation and when the viewport widens
  useEffect(() => setOpen(false), [pathname, search]);
  useEffect(() => {
    if (wide) setOpen(false);
  }, [wide]);

  return (
    <header className="bg-ink-900">
      <div className="border-b hairline">
        <div className="mx-auto flex max-w-7xl items-center gap-4 px-4 py-3 sm:px-6 lg:px-8">
          <a href="https://neiscenter.org" className="shrink-0 text-paper" aria-label="NEIS Center home (neiscenter.org)">
            <Logo className="h-8 w-auto" />
          </a>
          <span className="h-7 border-l hairline" aria-hidden="true" />
          <Link to="/" className="eyebrow min-w-0 flex-1 text-paper-dim hover:text-paper lg:flex-none">
            Allied Industrial Base Explorer
          </Link>
          {wide ? (
            <nav aria-label="Modules" className="ml-auto flex items-center gap-x-6">
              {tabs.map((t) => (
                <Link
                  key={t.id}
                  to={t.href}
                  aria-current={t.current ? 'true' : undefined}
                  className={`border-b-2 py-1 text-sm font-medium uppercase tracking-wider transition-colors ${
                    t.current ? 'border-signal text-paper' : 'border-transparent text-white/80 hover:text-paper'
                  }`}
                  {...prefetchProps(t.id)}
                >
                  {t.label}
                </Link>
              ))}
            </nav>
          ) : (
            <button type="button" className="btn btn-ghost btn-sm ml-auto" aria-expanded={open} aria-controls="mobile-nav" onClick={() => setOpen(!open)}>
              {open ? 'Close' : 'Menu'}
            </button>
          )}
        </div>
      </div>
      {wide && module && (
        <div className="border-b hairline">
          <nav aria-label={VIEWS_LABEL[module]} className="mx-auto flex h-10 max-w-7xl flex-wrap items-center gap-x-5 px-4 sm:px-6 lg:px-8">
            {items.map((n) => (
              <Link
                key={n.key}
                to={n.to}
                aria-current={n.active ? 'page' : undefined}
                className={`block border-b-2 py-1 text-sm font-medium uppercase tracking-wider transition-colors ${
                  n.active ? 'border-signal text-paper' : 'border-transparent text-white/80 hover:text-paper'
                }`}
              >
                {n.label}
              </Link>
            ))}
          </nav>
        </div>
      )}
      {!wide && open && (
        <div id="mobile-nav" className="border-b hairline">
          <div className="mx-auto max-w-7xl space-y-4 px-4 pb-4 pt-3 sm:px-6">
            <nav aria-label="Modules">
              <div className="eyebrow mb-2">Modules</div>
              <ul className="space-y-1">
                {tabs.map((t) => (
                  <li key={t.id}>
                    <Link
                      to={t.href}
                      aria-current={t.current ? 'true' : undefined}
                      className="relative flex min-h-11 items-center gap-3 py-2 pl-4 text-lg font-medium text-paper hover:text-signal"
                      {...prefetchProps(t.id)}
                    >
                      {t.current && <span className="absolute left-0 top-1/2 h-6 w-1 -translate-y-1/2 bg-signal" aria-hidden="true" />}
                      {t.label}
                      {t.current && <span className="text-xs font-normal uppercase tracking-wider text-paper-mute">current</span>}
                    </Link>
                  </li>
                ))}
              </ul>
            </nav>
            {module && (
              <nav aria-label={VIEWS_LABEL[module]} className="border-t hairline pt-3">
                {items.map((n) => (
                  <Link
                    key={n.key}
                    to={n.to}
                    aria-current={n.active ? 'page' : undefined}
                    className={`flex min-h-11 items-center border-l-2 pl-3 text-sm font-medium uppercase tracking-wider ${
                      n.active ? 'border-signal text-paper' : 'border-transparent text-white/80 hover:text-paper'
                    }`}
                  >
                    {n.label}
                  </Link>
                ))}
              </nav>
            )}
            <div className="border-t hairline pt-3">
              <Link to="/" className="link text-sm" aria-current={place === 'home' ? 'page' : undefined}>
                Home
              </Link>
            </div>
          </div>
        </div>
      )}
    </header>
  );
}
