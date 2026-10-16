// Draw the Fence: the standalone site's entry (fence/index.html). It shares the Explorer's design
// system (styles/globals.css, components/ui.tsx, the logo, the self-hosted Roboto face) but not its
// shell, routes or engine.

import { StrictMode } from 'react';
import { createRoot } from 'react-dom/client';
import { createBrowserRouter, RouterProvider } from 'react-router';
import '@fontsource-variable/roboto';
import './fence.css';
import { basename, fenceRoutes } from './routes.tsx';

const router = createBrowserRouter(fenceRoutes, { basename: basename() });

createRoot(document.getElementById('root')!).render(
  <StrictMode>
    <RouterProvider router={router} />
  </StrictMode>,
);
