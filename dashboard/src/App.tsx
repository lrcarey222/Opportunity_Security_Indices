// The app's root moved to the shared shell (spec_sector.md 2.5): AppRoot (src/shell/AppRoot.tsx)
// holds the header, the route notice, <main> and the footer; StrategicLayout
// (src/shell/StrategicLayout.tsx) is today's Root minus that chrome, mounted only under the
// Strategic routes. This file keeps the old imports working.

export { APP_TITLE, useTitle } from './shell/title.ts';
export { BootScreen } from './shell/screens.tsx';
