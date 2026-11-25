// Tests of the UX v2 answer options (qc/answerOptions.ts): canonical conclusions with one-line definitions (and the
// action wording of the posture choices, "Watch (keep open)"), followed by the three reviewer states; the model
// states (not assessed, the AIB class INSUFFICIENT DATA) are possible conclusions but never reviewer options; the
// sector-chain options are the six stages and "no stage"; the snapshot hash stays the canonical JSON's.

import { describe, expect, it } from 'vitest';
import { CHOICE_ACTION, CHOICE_DEFINITION, CHOICE_NAME } from '../../content/vocabulary.ts';
import { STAGE_DISPLAY, SUPPLY_STAGES } from '../../lib/stages.ts';
import { CASE_KINDS, DONT_KNOW, REVIEWER_STATE_LABEL, REVIEWER_STATES } from '../model.ts';
import { canonicalJson, sha256HexSync } from '../privacy.ts';
import { answerOptions, conclusionLabel, conclusionOptions, NO_STAGE, NO_STAGE_LABEL, PARTNERSHIP_BOX_LABEL } from './answerOptions.ts';
import { buildCaseBrief } from './brief.ts';
import { buildCaseReasoning } from './reasoning.ts';
import { buildCaseSnapshot } from './snapshot.ts';
import { mkDecoded, mkPosture, triageTrace } from './__tests__/fixtures.ts';

describe('answerOptions (UX v2)', () => {
  it('every conclusion option carries a one-line definition; posture options carry their action', () => {
    for (const k of CASE_KINDS) for (const o of answerOptions(k, k === 'compact' ? { sid: 'SC-X' } : {}).filter((x) => x.group === 'conclusion')) expect(o.definition, `${k} ${o.value}`).toBeTruthy();
    const post = answerOptions('posture_cell');
    expect(post.find((o) => o.value === 'partner')).toMatchObject({ label: CHOICE_NAME.partner, action: CHOICE_ACTION.partner, definition: CHOICE_DEFINITION.partner });
    // the name does not change; it is shown with the first words of its action
    expect(post.find((o) => o.value === 'watch')!.label).toBe(`${CHOICE_NAME.watch} (keep open)`);
  });
  it('ends with the three reviewer states, never offers a model state or DONT_KNOW', () => {
    for (const k of CASE_KINDS) {
      const opts = answerOptions(k);
      expect(opts.slice(-3).map((o) => o.value)).toEqual([...REVIEWER_STATES]);
      expect(opts.slice(-3).map((o) => o.label)).toEqual(REVIEWER_STATES.map((s) => REVIEWER_STATE_LABEL[s]));
      expect(opts.map((o) => o.value)).not.toContain('not_assessed');
      expect(opts.map((o) => o.value)).not.toContain('INSUFFICIENT DATA');
      expect(opts.map((o) => o.value)).not.toContain(DONT_KNOW);
    }
    expect(conclusionOptions('posture_cell').map((o) => o.value)).toContain('not_assessed');
    expect(conclusionOptions('aib_node').map((o) => o.value)).toContain('INSUFFICIENT DATA');
  });
  it('sector chains: the six stages and "no stage"; partnerships: the memo boxes', () => {
    const sc = answerOptions('sector_chain');
    expect(sc.slice(0, 7).map((o) => o.value)).toEqual([...SUPPLY_STAGES, NO_STAGE]);
    expect(sc.find((o) => o.value === 'Assembly')!.label).toBe(STAGE_DISPLAY.Assembly);
    expect(conclusionLabel('sector_chain', NO_STAGE)).toBe(NO_STAGE_LABEL);
    expect(answerOptions('partnership').slice(0, 4).map((o) => o.label)).toEqual(Object.values(PARTNERSHIP_BOX_LABEL));
  });
});

describe('the snapshot with its brief and reasoning', () => {
  it('hashes stably: the same inputs give the same canonical JSON and sha256', () => {
    const mk = () => {
      const p = mkPosture([{ iso3: 'KOR', tech: 'Batteries', stage: 'Components', choice: 'catch_up', need: 0.73, prize: 0.79, cp: 0.71, share: 0.06, pathway: 'yes', trace: triageTrace({ need: 0.73, prize: 0.79, cp: 0.71, pos: 'not_top_quarter', pathway: 'yes', choice: 'catch_up' }) }]);
      const b = mkDecoded({ posture: p });
      const def = { kind: 'posture_cell' as const, key: { iso3: 'KOR', tech: 'Batteries', stage: 'Components' }, title: 't', explorer_path: '/x' };
      const s = buildCaseSnapshot(def, b);
      s.brief = buildCaseBrief(def, b, s);
      s.reasoning = buildCaseReasoning(def, b, s);
      return s;
    };
    const a = mk();
    const b = mk();
    expect(sha256HexSync(canonicalJson(a))).toBe(sha256HexSync(canonicalJson(b)));
    expect(a.brief).toBeTruthy();
    expect(a.reasoning!.alternatives[0].value).toBe('preserve');
  });
});
