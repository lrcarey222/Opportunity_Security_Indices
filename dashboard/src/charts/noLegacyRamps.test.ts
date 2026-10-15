// The three-colour scale everywhere (user edit 1a; spec_country.md C1.5): no view, Sector derivation or
// Country file imports the superseded single-hue sequential or red/blue diverging ramps. Categorical
// colours (groups, tiers, stages) and the ordinal ramps (VOLATILITY_ORDINAL, GROUP_ORDINAL) stay allowed.
// The allowlist is empty: a new use is a regression.

import { describe, expect, it } from 'vitest';
import { readdirSync, readFileSync, statSync } from 'node:fs';
import { join, relative, resolve } from 'node:path';

const ROOT = resolve(process.cwd(), 'src');
const DIRS = ['views', 'sector/views', 'sector/derive', 'country'];
const FORBIDDEN = ['SEQ', 'DIV_RANGE', 'DIV_NEG', 'DIV_POS', 'DIV_MID', 'seqColor', 'divColor', 'seqScale', 'divScale', 'seqStep', 'RampLegend'];
const ALLOW: readonly string[] = [];

function files(dir: string): string[] {
  let out: string[] = [];
  let names: string[];
  try {
    names = readdirSync(dir);
  } catch {
    return out;
  }
  for (const n of names) {
    const p = join(dir, n);
    if (statSync(p).isDirectory()) {
      if (n === '__fixtures__' || n === 'node_modules') continue;
      out = out.concat(files(p));
    } else if (/\.(ts|tsx)$/.test(n) && !/\.test\.tsx?$/.test(n)) out.push(p);
  }
  return out;
}

/** The names a source file imports (named imports and re-exports), from any module. */
export function importedNames(src: string): string[] {
  const names: string[] = [];
  const re = /(?:import|export)\s+(?:type\s+)?\{([^}]*)\}\s*from\s*['"][^'"]+['"]/g;
  for (const m of src.matchAll(re)) {
    for (const part of m[1].split(',')) {
      const name = part.trim().replace(/^type\s+/, '').split(/\s+as\s+/)[0].trim();
      if (name) names.push(name);
    }
  }
  return names;
}

describe('no superseded colour ramps in views (C1.5)', () => {
  it('parses named imports, aliases and type imports', () => {
    expect(importedNames("import { SEQ, type TriScale, seqColor as sc } from '../theme/palette.ts';\nexport { divScale } from './x';")).toEqual(['SEQ', 'TriScale', 'seqColor', 'divScale']);
  });

  it('views, Sector derivations and Country Insights import none of them', () => {
    const offenders: string[] = [];
    for (const d of DIRS) {
      for (const f of files(join(ROOT, d))) {
        const rel = relative(ROOT, f).replace(/\\/g, '/');
        if (ALLOW.includes(rel)) continue;
        const hit = importedNames(readFileSync(f, 'utf8')).filter((n) => FORBIDDEN.includes(n));
        if (hit.length) offenders.push(`${rel}: ${hit.join(', ')}`);
      }
    }
    expect(offenders).toEqual([]);
  });
});
