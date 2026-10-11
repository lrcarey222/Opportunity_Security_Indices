// Route errors (a lazy route chunk that fails to load, e.g. after a deploy replaced the hashed files,
// or a loader error): instead of React Router's default page, a notice with Reload. RouteError renders
// inside AppRoot's <main> (header and footer kept); RootError is the last resort when AppRoot itself
// failed.

import { isRouteErrorResponse, useRouteError } from 'react-router';
import { Logo } from '../components/Logo.tsx';
import { Notice } from '../components/ui.tsx';

function message(err: unknown): string {
  if (isRouteErrorResponse(err)) return `${err.status} ${err.statusText}`;
  if (err instanceof Error) return err.message;
  return String(err ?? 'Unknown error');
}

const isChunkLoad = (m: string) => /dynamically imported module|Importing a module script failed|error loading dynamically/i.test(m);

export function RouteError() {
  const err = useRouteError();
  const m = message(err);
  return (
    <div className="mx-auto w-full max-w-7xl px-4 py-10 sm:px-6 lg:px-8">
      <Notice
        kind="error"
        title={isChunkLoad(m) ? 'This page could not be loaded' : 'This page failed'}
        action={
          <button type="button" className="btn btn-ghost btn-sm" onClick={() => window.location.reload()}>
            Reload
          </button>
        }
      >
        {isChunkLoad(m) ? 'The explorer may have been updated since this page was opened; reloading fetches the current version.' : m}
      </Notice>
    </div>
  );
}

export function RootError() {
  const m = message(useRouteError());
  return (
    <div className="flex min-h-screen flex-col items-center justify-center gap-6 bg-ink-900 px-4 text-center text-paper" role="alert">
      <Logo className="h-12 w-auto" />
      <div className="eyebrow">Allied Industrial Base Explorer</div>
      <p className="max-w-xl text-sm text-paper-dim">The explorer could not start: {m}</p>
      <button type="button" className="btn btn-primary" onClick={() => window.location.reload()}>
        Reload
      </button>
    </div>
  );
}
