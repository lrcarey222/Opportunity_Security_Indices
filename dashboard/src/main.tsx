import { StrictMode } from 'react';
import { createRoot } from 'react-dom/client';
import { createBrowserRouter, RouterProvider } from 'react-router';
import '@fontsource-variable/roboto';
import './styles/globals.css';
import { basename, routes } from './routes.tsx';

// The engine provider is no longer global: StrategicLayout mounts it, so Home and Sector Insights
// never start the worker (spec_sector.md 2.5).
const router = createBrowserRouter(routes, { basename: basename() });

createRoot(document.getElementById('root')!).render(
  <StrictMode>
    <RouterProvider router={router} />
  </StrictMode>,
);
