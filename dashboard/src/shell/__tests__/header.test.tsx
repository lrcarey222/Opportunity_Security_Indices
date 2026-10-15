// The shared header with three modules (spec_country.md A2.1): the desktop tabs and the tier-2 nav of
// Country Insights (the static fallback before the module registers its links), and the phone menu
// that lists the three modules with the current one marked. No bare acronym in the header (C3.4).

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

describe('Header with Country Insights', () => {
  it('desktop: three tabs, Country current, and the Country views nav with PROFILE on the country shown', () => {
    renderAt('/countries/JPN/memo');
    const tabs = screen.getByRole('navigation', { name: 'Modules' });
    const links = within(tabs).getAllByRole('link');
    expect(links.map((a) => a.textContent)).toEqual(['Sector Insights', 'Strategic Insights', 'Country Insights']);
    expect(links.map((a) => a.getAttribute('aria-current'))).toEqual([null, null, 'true']);
    expect(links[0].getAttribute('href')).toBe('/sector?c=JPN');
    const views = screen.getByRole('navigation', { name: 'Country Insights views' });
    const items = within(views).getAllByRole('link');
    expect(items.map((a) => [a.textContent, a.getAttribute('href'), a.getAttribute('aria-current')])).toEqual([
      ['Countries', '/countries', null],
      ['Country profile', '/countries/JPN', 'page'],
      ['About & data', '/countries/about', null],
    ]);
    expect(bareAcronyms(document.body)).toEqual([]);
  });

  it('desktop: on a Strategic country page the Country tab opens the same country', () => {
    rememberLocation('/countries/about', '');
    renderAt('/partners/CAN?h=80');
    const tabs = screen.getByRole('navigation', { name: 'Modules' });
    expect(within(tabs).getByRole('link', { name: 'Country Insights' }).getAttribute('href')).toBe('/countries/CAN');
    expect(within(tabs).getByRole('link', { name: 'Strategic Insights' }).getAttribute('aria-current')).toBe('true');
    const views = screen.getByRole('navigation', { name: 'Strategic Insights views' });
    expect(within(views).getByRole('link', { name: 'Partners' }).getAttribute('href')).toBe('/partners?h=80');
    expect(within(views).getByRole('link', { name: 'Security vs opportunity' })).toBeTruthy();
  });

  it('phone: the menu lists the three modules (current marked) and the module views', () => {
    wide = false;
    renderAt('/countries');
    expect(screen.queryByRole('navigation', { name: 'Modules' })).toBeNull();
    const menu = screen.getByRole('button', { name: 'Menu' });
    expect(menu.getAttribute('aria-expanded')).toBe('false');
    fireEvent.click(menu);
    const mods = screen.getByRole('navigation', { name: 'Modules' });
    const links = within(mods).getAllByRole('link');
    expect(links.map((a) => a.textContent?.replace('current', '').trim())).toEqual(['Sector Insights', 'Strategic Insights', 'Country Insights']);
    expect(links[2].getAttribute('aria-current')).toBe('true');
    const views = screen.getByRole('navigation', { name: 'Country Insights views' });
    expect(within(views).getByRole('link', { name: 'Countries' }).getAttribute('aria-current')).toBe('page');
    expect(within(views).getByRole('link', { name: 'Country profile' }).getAttribute('href')).toBe('/countries/USA');
  });
});
