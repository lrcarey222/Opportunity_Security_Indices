// The Draw the Fence header, laid out like the other NEIS tools: a brand row (the NEIS logo and the
// site's own name), then a row of views, which the layout renders with the assessment's actions
// (it needs the data). Below 1024 px the views move into a Menu panel.

import { useEffect, useState } from 'react';
import { Link, useLocation } from 'react-router';
import { Logo } from '../../components/Logo.tsx';
import { useMediaQuery } from '../../shell/useMediaQuery.ts';
import { FENCE_NAV } from '../nav.ts';

/** The views, carrying the assessment (`a`), with the current one marked. */
export function useNavItems() {
  const { pathname, search } = useLocation();
  const a = new URLSearchParams(search).get('a');
  const carry = a ? `?a=${encodeURIComponent(a)}` : '';
  const current = pathname.split('/').filter(Boolean)[0] ?? '';
  return { carry, current, items: FENCE_NAV.map((n) => ({ ...n, to: n.to + carry, active: n.key === current })) };
}

/** The views as a horizontal row (wide screens). */
export function NavLinks() {
  const { items } = useNavItems();
  return (
    <nav aria-label="Draw the Fence" className="flex flex-wrap items-center gap-x-5">
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
  );
}

export function Header() {
  const { pathname, search } = useLocation();
  const { carry, current, items } = useNavItems();
  const wide = useMediaQuery('(min-width: 1024px)');
  const [open, setOpen] = useState(false);

  useEffect(() => setOpen(false), [pathname, search]);
  useEffect(() => {
    if (wide) setOpen(false);
  }, [wide]);

  return (
    <header className="yf-no-print bg-ink-900">
      <div className="border-b hairline">
        <div className="mx-auto flex max-w-7xl items-center gap-4 px-4 py-3 sm:px-6 lg:px-8">
          <a href="https://neiscenter.org" className="shrink-0 text-paper" aria-label="NEIS Center home (neiscenter.org)">
            <Logo className="h-8 w-auto" />
          </a>
          <span className="h-7 border-l hairline" aria-hidden="true" />
          <Link to={`/${carry}`} className="flex min-w-0 items-baseline gap-3" aria-current={current === '' ? 'page' : undefined}>
            <span className="text-base font-semibold uppercase tracking-[0.14em] text-paper">Draw the Fence</span>
            <span className="eyebrow hidden sm:inline">Yards &amp; Fences · NEIS Center</span>
          </Link>
          {!wide && (
            <button type="button" className="btn btn-ghost btn-sm ml-auto" aria-expanded={open} aria-controls="fence-nav" onClick={() => setOpen(!open)}>
              {open ? 'Close' : 'Menu'}
            </button>
          )}
        </div>
      </div>
      {!wide && open && (
        <nav id="fence-nav" aria-label="Draw the Fence" className="border-b hairline">
          <ul className="mx-auto max-w-7xl px-4 pb-4 pt-3 sm:px-6">
            {items.map((n) => (
              <li key={n.key}>
                <Link
                  to={n.to}
                  aria-current={n.active ? 'page' : undefined}
                  className={`flex min-h-11 items-center border-l-2 pl-3 text-sm font-medium uppercase tracking-wider ${
                    n.active ? 'border-signal text-paper' : 'border-transparent text-white/80 hover:text-paper'
                  }`}
                >
                  {n.label}
                </Link>
              </li>
            ))}
          </ul>
        </nav>
      )}
    </header>
  );
}

export function Footer({ vintage }: { vintage?: string | null }) {
  return (
    <footer className="yf-no-print mt-16 border-t hairline">
      <div className="mx-auto max-w-7xl space-y-1 px-4 py-6 text-xs leading-relaxed text-paper-mute sm:px-6 lg:px-8">
        <p>
          Draw the Fence applies the NEIS Center&rsquo;s Yards &amp; Fences framework. The decision engine is rule-based and runs in your browser; nothing you enter leaves it except in links you share.
        </p>
        <p>
          Context data from the Opportunity &amp; Security Indices{vintage ? ` (data build ${vintage})` : ''}. Data inform the questions; they never decide the yard.
        </p>
      </div>
    </footer>
  );
}
