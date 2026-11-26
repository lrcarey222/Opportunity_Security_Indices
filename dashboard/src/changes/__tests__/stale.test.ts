// CI guard (change layer, docs/change-layer-implementation-plan.md "Stamp and automate"): the shipped change bundle
// must describe the shipped posture bundle. scripts/91 runs scripts/45 after every export; this fails when a posture
// re-export was committed without it, so the "what changed" pages never lag the results they explain.

import { existsSync, readFileSync } from 'node:fs';
import { join, resolve } from 'node:path';
import { describe, expect, it } from 'vitest';

const dataDir = resolve(process.cwd(), 'public/data');
const read = (p: string) => JSON.parse(readFileSync(join(dataDir, p), 'utf8')) as Record<string, unknown>;
const present = existsSync(join(dataDir, 'posture', 'manifest.json')) && existsSync(join(dataDir, 'changes', 'manifest.json'));

describe.skipIf(!present)('change bundle is current', () => {
  it('its latest vintage is the shipped posture core', () => {
    const posture = read('posture/manifest.json');
    const changes = read('changes/manifest.json');
    const hash = /^posture-core\.([0-9a-f]+)\.json$/.exec(String(posture.core))?.[1];
    expect(hash, 'posture manifest names no core').toBeTruthy();
    expect(
      String(changes.latest).endsWith(`-${hash}`),
      `changes/manifest.json latest is ${String(changes.latest)} but the posture core is ${String(posture.core)}: run Rscript scripts/45_build_posture_vintages.R --include-worktree (scripts/91 does this after an export)`,
    ).toBe(true);
  });
  it('the change document it names is shipped', () => {
    const changes = read('changes/manifest.json');
    expect(existsSync(join(dataDir, 'changes', String(changes.changes)))).toBe(true);
  });
});

const aibPresent = existsSync(join(dataDir, 'aib', 'manifest.json')) && existsSync(join(dataDir, 'changes', 'aib-manifest.json'));

describe.skipIf(!aibPresent)('partnership change bundle is current', () => {
  it('its latest AIB vintage is the shipped AIB compacts document', () => {
    const aib = read('aib/manifest.json') as { files?: { compacts?: string } };
    const ch = read('changes/aib-manifest.json');
    expect(
      ch.latest_doc,
      `changes/aib-manifest.json describes ${String(ch.latest_doc)} but the AIB bundle ships ${String(aib.files?.compacts)}: run Rscript scripts/46_build_aib_vintages.R --include-worktree (scripts/84 does this after an export)`,
    ).toBe(aib.files?.compacts);
  });
});
