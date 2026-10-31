// The bilateral relationship thesis (2026-10-20; strategic task spec s.19), pure: on the synthetic fixture's
// strategic section, a pair with a thesis (either order), its priority compacts with each side's compact
// role, its logic labels in the export's fixed order with their compact-role evidence (and the plain
// meaning where the lookup finds none), a pair without a thesis (allied, no modelled relationship; one side
// outside the allied economies), the words of a contribution and of a side's Strategic AIB Roles, the
// suggested partners, and where a partner sits in the cross-lens summary. On the real export (when
// present): every exported thesis reads back with the same labels, and the evidence lookup, which mirrors
// aib_relationship_theses(), finds evidence for every compact-role label.

import { describe, expect, it } from 'vitest';
import { AIB_THESIS_LOGIC, strategicOf, type AibStrategic } from '../../aib/contract.ts';
import { haveFixture, readDoc, REAL_DIR, SYNTHETIC_DIR } from '../../aib/__tests__/fixture.ts';
import { proseName } from '../../country/derive/text.ts';
import { crossLens, lensNames, lensNone, LENS_LOADING, pairReading } from '../lenses.ts';
import { contributionWords, LOGIC_INFO, LOGIC_ORDER, pairThesis, strategicRoleWords, thesisPartners } from '../thesis.ts';

const doc = readDoc(SYNTHETIC_DIR);
const s = strategicOf(doc) as AibStrategic;
const name = (x: string) => proseName(doc.countries[x] ?? x);

describe('pairThesis on the synthetic strategic section', () => {
  it('a pair with a thesis: priority compacts in the export order, each side’s compact role, logic in the fixed order', () => {
    const t = pairThesis(s, 'AAA', 'CCC', name)!;
    expect(t.state).toBe('thesis');
    expect(t.a.iso3).toBe('AAA');
    expect(t.b.iso3).toBe('CCC');
    expect(t.compacts.map((r) => r.compact.id)).toEqual(['SC-MAGNETS', 'SC-BATTERIES']);
    expect(t.compacts[0].a?.role).toBe('Demand anchor');
    expect(t.compacts[0].b?.role).toBe('Processing anchor');
    expect(t.logic.map((l) => l.label)).toEqual(['trade', 'technology', 'resilience / second sourcing']);
    expect(t.headline).toBe(
      'Avalon and Carpania: a trade, technology and resilience / second sourcing relationship, with Permanent Magnets & Drive Systems and Battery Materials & Cells as the priority compacts.',
    );
    // trade is the model's relationship; no compact names a specialized technology supplier here, so the
    // label keeps its plain meaning; resilience is read from Battery Materials & Cells
    expect(t.logic[0].evidence).toBe(LOGIC_INFO.trade.meaning);
    expect(t.logic[1].evidence).toBe(LOGIC_INFO.technology.meaning);
    expect(t.logic[1].compact).toBeNull();
    expect(t.logic[2].evidence).toBe('In Battery Materials & Cells, Carpania can second-source for Avalon, the demand anchor, as a diversification partner.');
    expect(t.logic[2].compact?.id).toBe('SC-BATTERIES');
    // the Strategic AIB Roles of each side
    expect(t.a.primary.map((r) => r.code)).toEqual(['DMA', 'CIA']);
    expect(t.b.secondary.map((r) => r.code)).toEqual(['SMI', 'CBH']);
  });

  it('either order: the same thesis seen from the other side', () => {
    const t = pairThesis(s, 'ccc', 'aaa', name)!;
    expect(t.a.iso3).toBe('CCC');
    expect(t.compacts[0].a?.role).toBe('Processing anchor');
    expect(t.compacts[0].b?.role).toBe('Demand anchor');
    expect(t.headline.startsWith('Carpania and Avalon: a trade, technology and resilience')).toBe(true);
  });

  it('development / capacity building and market access read their compact', () => {
    const d = pairThesis(s, 'AAA', 'DDD', name)!;
    expect(d.headline).toBe('Avalon and Dorvalia: an investment and development / capacity building relationship, with Permanent Magnets & Drive Systems as the priority compact.');
    expect(d.logic.find((l) => l.label === 'development / capacity building')?.evidence).toBe(
      'In Permanent Magnets & Drive Systems, Dorvalia can host new capacity and Avalon is an investment anchor there.',
    );
    const m = pairThesis(s, 'AAA', 'BBB', name)!;
    expect(m.logic.find((l) => l.label === 'market access')?.evidence).toBe('In Wind Power Equipment, Avalon is the demand anchor and Borduria supplies it as a scale manufacturer (and in 1 more priority compact).');
    // Borduria and Avalon also both take part in Magnets and Batteries, beyond the two priority compacts
    expect(m.moreShared.map((c) => c.id).sort()).toEqual(['SC-BATTERIES', 'SC-MAGNETS']);
  });

  it('four or more logics: a broad relationship, with how many of the six hold and the labels in brackets', () => {
    const withLogic = (labels: string[]): AibStrategic => ({
      ...s,
      theses: s.theses.map((t) => (t[0] === 'AAA' && t[1] === 'CCC' ? ([t[0], t[1], t[2], labels] as typeof t) : t)),
    });
    const five = pairThesis(withLogic(['trade', 'investment', 'technology', 'market access', 'resilience / second sourcing']), 'AAA', 'CCC', name)!;
    expect(five.headline).toBe(
      'Avalon and Carpania: a broad relationship on five of the six AIB logics (trade, investment, technology, market access and resilience / second sourcing), with Permanent Magnets & Drive Systems and Battery Materials & Cells as the priority compacts.',
    );
    const all = pairThesis(withLogic([...AIB_THESIS_LOGIC]), 'AAA', 'CCC', name)!;
    expect(all.headline.startsWith('Avalon and Carpania: a broad relationship on all six AIB logics (trade, investment, technology, development / capacity building, market access and resilience')).toBe(true);
    expect(all.logic.map((l) => l.label)).toEqual([...AIB_THESIS_LOGIC]);
  });

  it('a thesis without priority compacts says so', () => {
    const t = pairThesis(s, 'CCC', 'EEE', name)!;
    expect(t.state).toBe('thesis');
    expect(t.compacts).toEqual([]);
    expect(t.headline).toBe('Carpania and Elbonia: a resilience / second sourcing relationship, in no strategic compact both take part in.');
  });

  it('allied without a modelled relationship: no thesis, one line why, and the compacts both take part in (most relevant to both first)', () => {
    const t = pairThesis(s, 'AAA', 'FFF', name)!;
    expect(t.state).toBe('no-relationship');
    expect(t.logic).toEqual([]);
    expect(t.compacts.map((r) => r.compact.id)).toEqual(['SC-SOLAR', 'SC-WIND']);
    expect(t.compacts[0].b?.role).toBe('Scale manufacturer');
    expect(t.headline).toBe(
      'The analysis finds no trade or investment relationship between Avalon and Freedonia under Managed Interdependence or the major-supplier disruption, so there is no relationship thesis for them. Both take part in 2 strategic compacts: Solar Manufacturing and Wind Power Equipment.',
    );
  });

  it('outside the allied economies: a major supplier, or an economy the layer does not assess', () => {
    const z = pairThesis(s, 'AAA', 'ZZZ', name)!;
    expect(z.state).toBe('outside');
    expect(z.b.assessed).toBe(true);
    expect(z.b.allied).toBe(false);
    // four shared compacts: the three that matter most to both are shown, the fourth named after them
    expect(z.headline).toBe(
      'Relationship theses are written for pairs of allied economies: Zembla is a major supplier outside the allied economies. Both take part in 4 strategic compacts, among them Permanent Magnets & Drive Systems, Battery Materials & Cells and Grid & Electrolysis Equipment.',
    );
    expect(z.compacts.map((r) => r.compact.id)).toEqual(['SC-MAGNETS', 'SC-BATTERIES', 'SC-GRID']);
    expect(z.moreShared.map((c) => c.id)).toEqual(['SC-SOLAR']);
    expect(z.compacts[0].b?.role).toBe('Processing anchor (non-allied supplier)');
    const h = pairThesis(s, 'AAA', 'HHH', name)!;
    expect(h.state).toBe('outside');
    expect(h.b.assessed).toBe(false);
    expect(h.headline).toBe('Relationship theses are written for pairs of allied economies: Hyperborea is not one of the allied economies. They share no strategic compact.');
    expect(strategicRoleWords(h.b)).toMatch(/^not assessed by the strategic layer/);
  });

  it('no pair: a missing side or the same economy twice', () => {
    expect(pairThesis(s, 'AAA', null, name)).toBeNull();
    expect(pairThesis(s, null, 'AAA', name)).toBeNull();
    expect(pairThesis(s, 'AAA', 'aaa', name)).toBeNull();
  });

  it('a contribution and a side’s roles in words', () => {
    const t = pairThesis(s, 'AAA', 'CCC', name)!;
    expect(contributionWords(t.compacts[0].b)).toBe('Processing anchor / specialized technology supplier; largest position in Separated oxides (11% of world exports or production)');
    expect(contributionWords(t.compacts[0].a)).toBe('Demand anchor / specialized technology supplier; largest position in Magnets (36% of world imports)');
    expect(contributionWords(null)).toBe('no role in this compact');
    expect(strategicRoleWords(t.a)).toBe('Demand & Market Anchor and Capital & Investment Anchor; secondary: Coordination / Bridge Hub');
    const f = pairThesis(s, 'AAA', 'FFF', name)!;
    expect(strategicRoleWords(f.b)).toMatch(/^no Strategic AIB Role above the role thresholds/);
    // secondary roles only
    expect(strategicRoleWords({ iso3: 'X', assessed: true, allied: true, primary: [], secondary: t.b.secondary })).toBe(
      'no primary Strategic AIB Role; secondary: Scale Manufacturing & Integration Platform and Coordination / Bridge Hub',
    );
    // a role without a lead step is just its words
    expect(contributionWords(s.compactRoles.find((r) => r.iso3 === 'EEE' && r.compact === 'SC-MAGNETS'))).toBe('Bridge / coordination partner');
  });

  it('the suggested partners: the most priority compacts first, then the most logic labels, then by code', () => {
    expect(thesisPartners(s, 'AAA')).toEqual(['CCC', 'BBB', 'EEE', 'DDD']);
    expect(thesisPartners(s, 'AAA', 2)).toEqual(['CCC', 'BBB']);
    expect(thesisPartners(s, 'FFF')).toEqual([]);
    expect(thesisPartners(null, 'AAA')).toEqual([]);
  });

  it('every logic label has words, in the export order', () => {
    expect(LOGIC_ORDER).toEqual(AIB_THESIS_LOGIC);
    for (const l of AIB_THESIS_LOGIC) expect(LOGIC_INFO[l]?.title).toBeTruthy();
  });
});

describe('where the partner sits in the cross-lens summary', () => {
  it('the reading of the group naming it, null when no compared lens names it, undefined before any lens is compared', () => {
    const cl = crossLens({ today: lensNames(['BBB', 'CCC']), osi: lensNames(['CCC']), aib: lensNone('no data here') });
    expect(pairReading(cl, 'CCC')).toBe('A tie today that OSI ranks high');
    expect(pairReading(cl, 'BBB')).toBe('A tie today, not ranked high by OSI');
    expect(pairReading(cl, 'ZZZ')).toBeNull();
    expect(pairReading(crossLens({ today: LENS_LOADING, osi: LENS_LOADING, aib: lensNone('x') }), 'BBB')).toBeUndefined();
  });
});

describe.skipIf(!haveFixture(REAL_DIR))('the relationship theses of the real export (public/data/aib)', () => {
  const real = strategicOf(readDoc(REAL_DIR));
  const rname = (x: string) => x;

  it.skipIf(!real)('every exported thesis reads back with its labels and its compacts, and every compact-role label finds its evidence', () => {
    const r = real as AibStrategic;
    const misses: string[] = [];
    for (const [a, b, compacts, logic] of r.theses) {
      const t = pairThesis(r, a, b, rname)!;
      expect(t.state).toBe('thesis');
      expect(t.compacts.map((c) => c.compact.id)).toEqual(compacts);
      expect(t.logic.map((l) => l.label)).toEqual([...logic].sort((p, q) => LOGIC_ORDER.indexOf(p) - LOGIC_ORDER.indexOf(q)));
      for (const row of t.compacts) {
        expect(row.a).not.toBeNull();
        expect(row.b).not.toBeNull();
      }
      for (const l of t.logic) if (l.label !== 'trade' && l.label !== 'investment' && l.compact === null) misses.push(`${a}-${b} ${l.label}`);
    }
    expect(misses).toEqual([]);
  });

  it.skipIf(!real)('Japan and the United States: a thesis with three priority compacts, and both sides bring a role to each', () => {
    const t = pairThesis(real as AibStrategic, 'JPN', 'USA', rname);
    expect(t?.state).toBe('thesis');
    expect(t!.compacts.length).toBeGreaterThan(0);
    expect(t!.headline.startsWith('JPN and USA: a ')).toBe(true);
  });
});
