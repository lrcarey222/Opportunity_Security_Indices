// Full-page and in-content states of the shell. BootScreen is the router's hydrateFallbackElement
// (lazy route chunks loading on a first visit); EngineFailure renders inside the Strategic content
// area with the header kept (spec_sector.md 2.5).

import type { ReactNode } from 'react';
import { Logo } from '../components/Logo.tsx';

function Splash({ children }: { children: ReactNode }) {
  return (
    <div className="flex min-h-screen flex-col items-center justify-center gap-6 bg-ink-900 px-4 text-center text-paper">
      <Logo className="h-12 w-auto" />
      <div className="eyebrow">Allied Industrial Base Explorer</div>
      <div className="max-w-xl text-sm text-paper-dim">{children}</div>
    </div>
  );
}

export function BootScreen() {
  return (
    <Splash>
      <span role="status" aria-live="polite">
        <span className="mr-2 inline-block h-2 w-2 animate-pulse bg-signal" aria-hidden="true" />
        Loading
      </span>
    </Splash>
  );
}

/** The Strategic engine could not start (no data, schema mismatch, worker failure): in content. */
export function EngineFailure({ code, message }: { code: string; message: string }) {
  const noData = code === 'DATA_LOAD';
  return (
    <div className="mx-auto flex max-w-3xl flex-col items-start gap-3 px-4 py-16 text-sm text-paper-dim sm:px-6" role="alert">
      <div className="eyebrow">Legacy OSI scores</div>
      <p className="text-base text-paper">{code === 'SCHEMA' ? 'This data build needs a newer app.' : noData ? 'No data.' : 'The Legacy OSI scores could not be loaded.'}</p>
      <p>{message}</p>
      {noData && (
        <p className="text-paper-mute">
          Developers: run <code>Rscript scripts/85_export_dashboard_data.R</code> from the repository root, or <code>npm run dev:fixture</code> for the synthetic bundle.
        </p>
      )}
      <button type="button" className="btn btn-primary" onClick={() => window.location.reload()}>
        Reload
      </button>
    </div>
  );
}
