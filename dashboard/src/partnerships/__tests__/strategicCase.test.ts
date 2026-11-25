// The strategic case of a pair (Explorer usability pass, 2026-11-25): the thesis's logic labels regrouped
// under the brief's five purposes, each with the priority compacts whose compact roles support it, the
// principal ones first (the most supporting compacts, ties in the brief's order). On the synthetic fixture:
// exact readings; a pair without a thesis has no strategic case. On the real export (when present): every
// label maps to a purpose; the first compact found for a label is the one the thesis names (the lookup
// mirrors thesis.ts logicRows()); a pair without a thesis never gets a case; no sentence leans on alignment.

import { describe, expect, it } from 'vitest';
import { strategicOf, type AibStrategic } from '../../aib/contract.ts';
import { haveFixture, readDoc, REAL_DIR, SYNTHETIC_DIR } from '../../aib/__tests__/fixture.ts';
import { proseName } from '../../country/derive/text.ts';
import { AVOID_RE } from '../../content/vocabulary.ts';
import { LEAD_MAX, LOGIC_PURPOSE, NO_CASE, PURPOSE_ORDER, PURPOSE_WORDS, strategicCase } from '../strategicCase.ts';
import { LOGIC_INFO, LOGIC_ORDER, pairThesis } from '../thesis.ts';

const doc = readDoc(SYNTHETIC_DIR);
const s = strategicOf(doc) as AibStrategic;
const name = (x: string) => proseName(doc.countries[x] ?? x);
const caseOf = (st: AibStrategic, a: string, b: string, nm = name) => strategicCase(st, pairThesis(st, a, b, nm)!, nm);

describe('strategicCase on the synthetic strategic section', () => {
  it('maps every logic label of the export to one of the five purposes', () => {
    for (const l of LOGIC_ORDER) expect(PURPOSE_ORDER).toContain(LOGIC_PURPOSE[l]);
    expect(Object.values(PURPOSE_WORDS)).toEqual(['securing supply', 'combining complementary capabilities', 'expanding a market', 'building new capacity', 'reducing concentration']);
  });

  it('leads with the purposes supported in the most priority compacts', () => {
    const c = caseOf(s, 'AAA', 'BBB');
    expect(c.state).toBe('case');
    expect(c.lead).toEqual(['market']);
    expect(c.sentence).toBe('For Avalon and Borduria, the strategic case is mainly expanding a market.');
    expect(c.purposes.map((p) => p.id)).toEqual(['market', 'secure']);
    expect(c.purposes[0].support.map((x) => x.compact.id)).toEqual(['SC-WIND', 'SC-GRID']);
    expect(c.purposes[0].support[0].words).toBe('Avalon is the main buyer (demand anchor); Borduria supplies it as scale manufacturer (Towers, nacelles & hubs)');
    // trade rests on the analysis's relationships, not a compact: it keeps its plain meaning
    expect(c.purposes[1].support).toEqual([]);
    expect(c.purposes[1].meaning).toBe(LOGIC_INFO.trade.meaning);
  });

  it('a label whose lookup finds no compact keeps its meaning and does not lead', () => {
    const c = caseOf(s, 'AAA', 'CCC');
    expect(c.lead).toEqual(['concentration']);
    expect(c.purposes.find((p) => p.id === 'concentration')?.support[0].words).toBe('Carpania can be a second source for Avalon');
    const tech = c.purposes.find((p) => p.id === 'complement')!;
    expect(tech.labels).toEqual(['technology']);
    expect(tech.support).toEqual([]);
    expect(tech.meaning).toBe(LOGIC_INFO.technology.meaning);
  });

  it('two labels of one purpose read as one; without compact support the brief order decides', () => {
    const d = caseOf(s, 'AAA', 'DDD');
    expect(d.purposes.map((p) => [p.id, p.labels])).toEqual([['capacity', ['investment', 'development / capacity building']]]);
    expect(d.purposes[0].support[0].words).toBe('Dorvalia can host new capacity; Avalon can invest');
    const e = caseOf(s, 'AAA', 'EEE');
    expect(e.lead).toEqual(['secure', 'capacity']);
    expect(e.sentence).toBe('For Avalon and Elbonia, the strategic case is mainly securing supply and building new capacity.');
  });

  it('a pair without a thesis has no strategic case', () => {
    const c = caseOf(s, 'BBB', 'CCC');
    expect(c).toEqual({ state: 'none', purposes: [], lead: [], sentence: NO_CASE });
    expect(caseOf(s, 'AAA', 'ZZZ').state).toBe('none');
  });
});

describe.skipIf(!haveFixture(REAL_DIR))('strategicCase on the real export (public/data/aib)', () => {
  const real = readDoc(REAL_DIR);
  const rs = real.strategic as AibStrategic | null | undefined;
  const nm = (x: string) => proseName(real.countries[x] ?? x);

  it.skipIf(!rs)('every thesis: a case led by at most two purposes; the first compact per label is the thesis’s own', () => {
    const st = rs!;
    for (const [a, b] of st.theses) {
      const t = pairThesis(st, a, b, nm)!;
      const c = strategicCase(st, t, nm);
      if (!t.logic.length) {
        expect(c.state).toBe('none');
        continue;
      }
      expect(c.state, `${a}_${b}`).toBe('case');
      expect(c.lead.length).toBeGreaterThan(0);
      expect(c.lead.length).toBeLessThanOrEqual(LEAD_MAX);
      expect(c.sentence).not.toMatch(/align|friend|vote/i);
      expect(c.sentence).not.toMatch(AVOID_RE);
      // parity with thesis.ts: where the thesis names the compact of a label, it is the first one found here
      for (const l of t.logic) {
        if (!l.compact) continue;
        const p = c.purposes.find((x) => x.labels.includes(l.label))!;
        const first = p.support.find((x) => x.label === l.label);
        expect(first?.compact.id, `${a}_${b} ${l.label}`).toBe(l.compact.id);
      }
      // a purpose that leads has at least as many supporting compacts as any that follows
      const n = (id: string) => c.purposes.find((p) => p.id === id)!.support.length;
      const tail = c.purposes.filter((p) => !c.lead.includes(p.id));
      for (const id of c.lead) for (const p of tail) expect(n(id)).toBeGreaterThanOrEqual(p.support.length);
    }
  });

  it.skipIf(!rs)('allied pairs without a thesis get no case (the United States and Taiwan in this export)', () => {
    const st = rs!;
    const t = pairThesis(st, 'USA', 'TWN', nm);
    if (!t || t.state === 'thesis') return;
    expect(strategicCase(st, t, nm).sentence).toBe(NO_CASE);
  });
});
