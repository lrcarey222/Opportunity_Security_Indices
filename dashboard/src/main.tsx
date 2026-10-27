import './artifactFetch.ts';
import { StrictMode } from 'react';
import { createRoot } from 'react-dom/client';
import { createBrowserRouter, createMemoryRouter, RouterProvider } from 'react-router';
import '@fontsource-variable/roboto';
import './styles/globals.css';
import { artifactEntry, basename, routes } from './routes.tsx';

// The engine provider is no longer global: StrategicLayout mounts it, so Home and Sector Insights
// never start the worker (spec_sector.md 2.5).
// VITE_ARTIFACT=1 (the beta preview hosted as a claude.ai artifact, built with VITE_BASE=./): the host
// serves only the page itself, so routes live in memory; '#countries.JPN' in the artifact link opens
// /countries/JPN.
const ARTIFACT = import.meta.env.VITE_ARTIFACT === '1';
const router = ARTIFACT
  ? createMemoryRouter(routes, { initialEntries: [artifactEntry(window.location.hash)] })
  : createBrowserRouter(routes, { basename: basename() });

// A few views render plain <a href="/..."> links; in the preview they would leave the page, so they go
// through the router (router <Link>s have already called preventDefault by the time this runs).
if (ARTIFACT) {
  document.addEventListener('click', (e) => {
    if (e.defaultPrevented || e.button !== 0 || e.metaKey || e.ctrlKey || e.shiftKey || e.altKey) return;
    const a = (e.target as Element | null)?.closest?.('a[href]');
    if (!(a instanceof HTMLAnchorElement) || a.target || a.hasAttribute('download')) return;
    const href = a.getAttribute('href') ?? '';
    if (!href.startsWith('/') || href.startsWith('//')) return;
    e.preventDefault();
    void router.navigate(href);
  });
}

createRoot(document.getElementById('root')!).render(
  <StrictMode>
    <RouterProvider router={router} />
  </StrictMode>,
);
