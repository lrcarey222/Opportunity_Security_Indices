// The shared header after the IA refactor (2026-10-16): the desktop tabs Sectors, Countries, Allied
// Industrial Base and About (secondary), the tier-2 nav of Countries (the static fallback before the
// module registers its links), the legacy Strategic views filed under the tab that owns them, and the
// phone menu with the current tab marked. No bare acronym in the header (C3.4).

import { afterAll, afterEach, beforeAll, describe, expect, it } from 'vitest';
import { cleanup, fireEvent, render, screen, within } from '@testing-library/react';
import { createMemoryRouter, RouterProvider, useLocation } from 'react-router';
import { Header } from '../Header.tsx';
import { ModuleNavProvider } from '../ModuleNav.tsx';
import { rememberLocation, resetLastLocations } from '../lastLocation.ts';
import { bareAcronyms } from '../../components/testing/acronyms.ts';

let wide = true;
const restore: (() => void)[] = [];
beforeAll(() => {
  const mm = window.matchMedia;
  window.matchMedia = ((q: string) => ({
    matches: /min-width: 1024px/.test(q) ? wide : false, media: q, onchange: null,
    addEventListener: () => undefined, removeEventListener: () => undefined, addListener: () => undefined, removeListener: () => undefined, dispatchEvent: () => false,
  })) as unknown as typeof window.matchMedia;
  restore.push(() => (window.matchMedia = mm));
  const NodeRequest = globalThis.Request;
  globalThis.Request = class extends NodeRequest {
    constructor(input: RequestInfo | URL, init?: RequestInit) {
      const rest = { ...(init ?? {}) };
      delete rest.signal;
      super(input, rest);
    }
  } as typeof Request;
  restore.push(() => (globalThis.Request = NodeRequest));
});
afterEach(() => {
  cleanup();
  resetLastLocations();
  wide = true;
});
afterAll(() => restore.forEach((f) => f()));

function Shell() {
  const loc = useLocation();
  return <Header pathname={loc.pathname} search={loc.search} />;
}

function renderAt(path: string) {
  const router = createMemoryRouter([{ path: '*', element: <Shell /> }], { initialEntries: [path] });
  render(
    <ModuleNavProvider>
      <RouterProvider router={router} />
    </ModuleNavProvider>,
  );
  return router;
}

const TABS = ['Sectors', 'Countries', 'Allied Industrial Base', 'About'];

describe('Header (IA refactor)', () => {
  it('desktop: four tabs, Countries current, and the Country views nav with PROFILE on the country shown', () => {
    renderAt('/countries/JPN/memo');
    const tabs = screen.getByRole('navigation', { name: 'Modules' });
    const links = within(tabs).getAllByRole('link');
    expect(links.map((a) => a.textContent)).toEqual(TABS);
    expect(links.map((a) => a.getAttribute('aria-current'))).toEqual([null, 'true', null, null]);
    expect(links[0].getAttribute('href')).toBe('/sector?c=JPN');
    expect(links[2].getAttribute('href')).toBe('/aib');
    expect(links[3].getAttribute('href')).toBe('/about');
    const views = screen.getByRole('navigation', { name: 'Country views' });
    const items = within(views).getAllByRole('link');
    expect(items.map((a) => [a.textContent, a.getAttribute('href'), a.getAttribute('aria-current')])).toEqual([
      ['All countries', '/countries', null],
      ['Country profile', '/countries/JPN', 'page'],
    ]);
    expect(bareAcronyms(document.body)).toEqual([]);
  });

  it('desktop: a legacy Strategic page shows the tab that owns it; the Countries tab opens the same country', () => {
    rememberLocation('/countries/about', '');
    renderAt('/partners/CAN?h=80');
    const tabs = screen.getByRole('navigation', { name: 'Modules' });
    expect(within(tabs).getByRole('link', { name: 'Countries' }).getAttribute('href')).toBe('/countries/CAN');
    expect(within(tabs).getByRole('link', { name: 'Allied Industrial Base' }).getAttribute('aria-current')).toBe('true');
    const views = screen.getByRole('navigation', { name: 'Allied Industrial Base views' });
    expect(within(views).getByRole('link', { name: 'Partnerships' }).getAttribute('aria-current')).toBe('page');
    cleanup();
    renderAt('/methods?h=80');
    expect(within(screen.getByRole('navigation', { name: 'Modules' })).getByRole('link', { name: 'About' }).getAttribute('aria-current')).toBe('true');
    const about = screen.getByRole('navigation', { name: 'About pages' });
    expect(within(about).getByRole('link', { name: 'Data & sources' }).getAttribute('href')).toBe('/data?h=80');
    expect(within(about).getByRole('link', { name: 'Glossary' }).getAttribute('href')).toBe('/glossary');
  });

  it('phone: the menu lists the four tabs (current marked) and the module views', () => {
    wide = false;
    renderAt('/countries');
    expect(screen.queryByRole('navigation', { name: 'Modules' })).toBeNull();
    const menu = screen.getByRole('button', { name: 'Menu' });
    expect(menu.getAttribute('aria-expanded')).toBe('false');
    fireEvent.click(menu);
    const mods = screen.getByRole('navigation', { name: 'Modules' });
    const links = within(mods).getAllByRole('link');
    expect(links.map((a) => a.textContent?.replace('current', '').trim())).toEqual(TABS);
    expect(links[1].getAttribute('aria-current')).toBe('true');
    const views = screen.getByRole('navigation', { name: 'Country views' });
    expect(within(views).getByRole('link', { name: 'All countries' }).getAttribute('aria-current')).toBe('page');
    expect(within(views).getByRole('link', { name: 'Country profile' }).getAttribute('href')).toBe('/countries/USA');
  });
});
