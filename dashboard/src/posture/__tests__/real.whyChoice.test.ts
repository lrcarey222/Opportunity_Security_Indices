// whyChoice (posture/whyChoice.ts) over the real posture bundle (public/data/posture): every detail cell of
// every economy. The trace-read explanation must name the shipped choice; where the trace's branch does not
// reproduce it, the helper falls back to the engine's own sentence (counted here, and kept rare). The words never
// use the copy guide's ruled-out words, and a "why not" never names the shipped choice itself. Skipped without
// an export, like real.posture.test.ts.

import { existsSync, readFileSync } from 'node:fs';
import { join, resolve } from 'node:path';
import { describe, expect, it } from 'vitest';
import { AVOID_RE, CHOICE_NAME } from '../../content/vocabulary.ts';
import { checkPostureDetail, checkPostureManifest, decodeCore } from '../contract.ts';
import { shippedChoice, whyChoice } from '../whyChoice.ts';

const DIR = resolve(process.cwd(), 'public/data/posture');
const read = (f: string): unknown => JSON.parse(readFileSync(join(DIR, f), 'utf8'));

describe.skipIf(!existsSync(join(DIR, 'manifest.json')))('whyChoice on the real posture bundle', () => {
  it('explains every shipped choice from its trace, falling back to the engine sentence only rarely', () => {
    const manifest = checkPostureManifest(read('manifest.json'));
    const core = decodeCore(read(manifest.core) as Record<string, unknown>);
    const files = Object.values((manifest as unknown as { details: Record<string, string> }).details);
    let cells = 0;
    let traced = 0;
    let fallback = 0;
    const odd: string[] = [];
    for (const f of files) {
      const doc = checkPostureDetail(read(f));
      for (const d of doc.cells) {
        if (!d.trace.length) continue;
        cells++;
        const w = whyChoice(d, core.meta);
        const choice = shippedChoice(d);
        expect(w.choice).toBe(choice);
        expect(w.because.length).toBeGreaterThan(0);
        expect(`${w.because} ${w.whyNot?.text ?? ''}`).not.toMatch(AVOID_RE);
        if (w.whyNot && choice && choice !== 'not_assessed') expect(w.whyNot.alt).not.toBe(CHOICE_NAME[choice]);
        // the trace-read words carry the trace's own numbers; the engine sentence is the fallback
        const engine = d.reason ? d.reason.trim()[0].toUpperCase() + d.reason.trim().slice(1) : null;
        if (w.because === engine || w.because === 'The decision trace for this step is not available.') {
          fallback++;
          if (odd.length < 8) odd.push(`${doc.iso3} ${d.tech}|${d.stage} ${choice}`);
        } else traced++;
      }
    }
    // eslint-disable-next-line no-console
    console.log(`whyChoice real: ${cells} traced cells, ${traced} explained from the trace, ${fallback} engine-sentence fallbacks`, odd);
    expect(cells).toBeGreaterThan(1000);
    expect(fallback / cells).toBeLessThan(0.02);
  });
});
