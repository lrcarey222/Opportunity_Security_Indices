// Parity of the TypeScript engine with the R reference engine on the committed synthetic fixtures
// (scripts/86_export_dashboard_fixtures.R --set synthetic): s1 = dash_fixture_bundle(seed = 1),
// s2 = seed 2 with inputs rounded to one decimal (heavy ties).
import { fileURLToPath } from 'node:url';
import { join } from 'node:path';
import { loadWebBundle } from './harness.ts';
import { paritySuite } from './suite.ts';

const root = fileURLToPath(new URL('../../__fixtures__/synthetic/', import.meta.url));
for (const set of ['s1', 's2']) {
  const dir = join(root, set);
  paritySuite({ name: `synthetic/${set}`, fixtureDir: dir, bundle: () => loadWebBundle(join(dir, 'data')) });
}
