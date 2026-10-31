// The shared node selection of the Country page (design D8, spec XIX): ?node=<Tech>|<Stage>, checked
// against the data; a value not used as written is reported as ignored (or as mapped, for Downstream).

import { describe, expect, it } from 'vitest';
import { decodeCore } from '../contract.ts';
import { parseNodeParam } from '../selection.ts';
import { fixtureCore } from './fixture.ts';

const meta = decodeCore(fixtureCore()).meta;

describe('?node= (parseNodeParam)', () => {
  it('no value selects nothing and reports nothing', () => {
    expect(parseNodeParam(null, meta)).toEqual({ key: null, problem: null });
    expect(parseNodeParam('  ', meta)).toEqual({ key: null, problem: null });
  });

  it('a node of the data is used as written, by stage id or by display name, at any of the six stages', () => {
    expect(parseNodeParam('Solar|Components', meta)).toEqual({ key: 'Solar|Components', problem: null });
    expect(parseNodeParam('Batteries|Refining & Processing', meta)).toEqual({ key: 'Batteries|Refining', problem: null });
    expect(parseNodeParam('Solar|Deployment & Use', meta)).toEqual({ key: 'Solar|Deployment', problem: null });
    expect(parseNodeParam(' Solar | Recovery ', meta)).toEqual({ key: 'Solar|Recovery', problem: null });
  });

  it('an invalid value is reported as ignored, with why', () => {
    expect(parseNodeParam('Wind|Refining', meta)).toEqual({ key: null, problem: 'node=Wind|Refining (Wind is not a technology of the posture data; ignored)' });
    expect(parseNodeParam('Solar|refining', meta)).toEqual({ key: null, problem: 'node=Solar|refining (refining is not a supply-chain stage; ignored)' });
    expect(parseNodeParam('Solar', meta)).toEqual({ key: null, problem: 'node=Solar (not a technology|stage pair; ignored)' });
    expect(parseNodeParam('Solar|Refining|x', meta).problem).toBe('node=Solar|Refining|x (not a technology|stage pair; ignored)');
    expect(parseNodeParam('|Refining', meta).problem).toBe('node=|Refining (an empty name is not a technology of the posture data; ignored)');
  });

  it('an old three-stage stage: Downstream is mapped to Deployment & Use (and said); Upstream and Midstream are ignored', () => {
    expect(parseNodeParam('Solar|Downstream', meta)).toEqual({ key: 'Solar|Deployment', problem: 'node=Solar|Downstream (Downstream corresponds to Deployment & Use; showing Solar — Deployment & Use)' });
    expect(parseNodeParam('Solar|Midstream', meta)).toEqual({ key: null, problem: 'node=Solar|Midstream (Midstream corresponds mostly to Components and Assembly; ignored)' });
    expect(parseNodeParam('Batteries|Upstream', meta).key).toBeNull();
  });

  it('another URL key names itself in the report', () => {
    expect(parseNodeParam('Wind|Refining', meta, 'cell').problem).toBe('cell=Wind|Refining (Wind is not a technology of the posture data; ignored)');
  });
});
