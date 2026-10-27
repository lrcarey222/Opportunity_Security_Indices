// A country's strategic summary (2026-10-20; views/strategicRoles/derive.ts), pure: a narrative slot's list
// split back into its items (labels with commas and parentheses kept whole, names with commas, an unknown
// item kept), each item joined to its node and strategic compact, the needs to the allied economies with a
// supply position, the partners to their codes and the pair's thesis, the capacity's total, the host
// compacts; the role evidence in words (metrics by kind, percentiles as ordinals, the rule with the
// section's thresholds); the default economy and the selector's choices; the Country profile's wrapper
// (aibStrategicOf); and on the real export, when present, every slot item of every economy resolves and
// the contributions are the division of labour's own positions.

import { describe, expect, it } from 'vitest';
import type { AibDoc } from '../contract.ts';
import { countryStrategicOf, divisionCells } from '../derive/strategic.ts';
import {
  alliedSuppliersOf, compactRoleWords, defaultEconomy, economyOptions, firstOnMeasures, focusStages, focusWords, gateWords, heldRoles, logicWords, metricText, ordinal,
  otherFunctions, percentileWords, proseNodeLabel, roleRuleWords, roleStatusWords, splitSlotList, strategicSummaryOf, summaryHeading,
} from '../views/strategicRoles/derive.ts';
import type { AibCompactRole } from '../contract.ts';
import { aibStrategicOf } from '../../country/derive/framing.ts';
import { pairHref, rolesHref } from '../nav.ts';
import { haveFixture, readDoc, REAL_DIR, stripStrategic, SYNTHETIC_DIR } from './fixture.ts';

const d = (): AibDoc => structuredClone(readDoc(SYNTHETIC_DIR));
const doc = d();
const s = doc.strategic!;
const names = doc.countries as Record<string, string>;
const name = (iso: string) => names[iso] ?? iso;
const REE = 'ev-wind-mid-ree-compounds';
const MAG = 'ev-wind-mid-magnets';
const ANODE = 'bat-mid-anode';
const ELY = 'h2-down-electrolysers';

describe('splitSlotList', () => {
  const nodes = new Map([
    ['Magnets, magnet parts & assemblies', 'mag'],
    ['Synthetic graphite (anode-grade, spherical)', 'anode'],
    ['PGMs (iridium, ruthenium)', 'pgm'],
    ['Magnets', 'short'],
  ]);
  it('labels with commas and parentheses stay whole; the parenthesis after an item is its detail; the longest known label wins', () => {
    expect(splitSlotList('Magnets, magnet parts & assemblies (18%), Synthetic graphite (anode-grade, spherical) (38% of allied demand), PGMs (iridium, ruthenium) (47% of allied demand)', nodes)).toEqual([
      { label: 'Magnets, magnet parts & assemblies', key: 'mag', detail: '18%' },
      { label: 'Synthetic graphite (anode-grade, spherical)', key: 'anode', detail: '38% of allied demand' },
      { label: 'PGMs (iridium, ruthenium)', key: 'pgm', detail: '47% of allied demand' },
    ]);
    // a label that is a prefix of a longer one only where the text ends the item there
    expect(splitSlotList('Magnets (4%)', nodes)).toEqual([{ label: 'Magnets', key: 'short', detail: '4%' }]);
  });
  it('names with commas (economies); an unknown item runs to the next comma and keeps its own parentheses', () => {
    const econ = new Map([
      ['Japan', 'JPN'],
      ['Korea, Rep.', 'KOR'],
      ['United States', 'USA'],
    ]);
    expect(splitSlotList('Japan, Korea, Rep., United States', econ).map((p) => p.key)).toEqual(['JPN', 'KOR', 'USA']);
    expect(splitSlotList('Atlantis, Japan', econ)).toEqual([
      { label: 'Atlantis', key: null, detail: null },
      { label: 'Japan', key: 'JPN', detail: null },
    ]);
    expect(splitSlotList('Unknown (a, b) (12% of allied demand), Japan', econ)).toEqual([
      { label: 'Unknown (a, b)', key: null, detail: '12% of allied demand' },
      { label: 'Japan', key: 'JPN', detail: null },
    ]);
  });
  it('nothing to split: empty, null', () => {
    expect(splitSlotList('', nodes)).toEqual([]);
    expect(splitSlotList(null, nodes)).toEqual([]);
    expect(splitSlotList(undefined, nodes)).toEqual([]);
  });
});

describe('the role evidence in words', () => {
  it('ordinals, percentiles, metric values by kind', () => {
    expect([1, 2, 3, 4, 11, 12, 13, 21, 22, 90, 97, 100].map(ordinal)).toEqual(['1st', '2nd', '3rd', '4th', '11th', '12th', '13th', '21st', '22nd', '90th', '97th', '100th']);
    expect(percentileWords(0.974)).toBe('97th percentile');
    expect(percentileWords(null)).toBe('–');
    expect(metricText('fdi_out_usd_bn', 1547)).toBe('US$1,547 bn');
    expect(metricText('cmfdi_out_usd_bn', 45.12)).toBe('US$45.1 bn');
    expect(metricText('rp_mean_share', 0.02619)).toBe('2.6%');
    expect(metricText('demand_mean_share', 0.1679)).toBe('17%');
    expect(metricText('mfg_value_share', 0.0002951)).toBe('<0.1%');
    expect(metricText('net_import_mf', 0)).toBe('0.0%');
    expect(metricText('scaleup_rate', 0.375)).toBe('38%');
    expect(metricText('spec_compacts', 6)).toBe('6');
    expect(metricText('sc_span', null)).toBe('–');
  });
  it('a gate: label, value and percentile; no data says so', () => {
    expect(gateWords({ metric: 'host_gap_usd_bn', label: 'unmet allied demand it can host', value: 1.6, pct: 0.9 })).toBe('unmet allied demand it can host: US$1.6 bn (90th percentile)');
    expect(gateWords({ metric: 'fdi_out_usd_bn', label: 'outward direct investment', value: null, pct: null })).toBe('outward direct investment: no data');
  });
  it('the statuses and the rule with the section\'s thresholds', () => {
    expect(['primary', 'secondary', 'qualifies beyond the cap', null].map(roleStatusWords)).toEqual(['Primary role', 'Secondary role', 'Qualifies, beyond the cap', 'Not held']);
    const rule = roleRuleWords(s);
    expect(rule).toContain(`at or above the ${ordinal(s.thresholds.pPrimary * 100)} percentile`);
    expect(rule).toContain(`at most ${s.thresholds.maxPrimary} primary and ${s.thresholds.maxSecondary} secondary roles`);
    expect(rule).toMatch(/never averaged and there is no overall score/);
  });
});

describe('the summary in plain words', () => {
  it('the heading: a possessive, "the" where the name takes it; a name written with a comma takes none', () => {
    expect(summaryHeading('Japan')).toBe("Japan's role in the Allied Industrial Base");
    expect(summaryHeading('United States')).toBe("The United States' role in the Allied Industrial Base");
    expect(summaryHeading('Korea, Rep.')).toBe('Role in the Allied Industrial Base: Korea, Rep.');
  });
  it('the focus slot as stages of the chain, the stronger first; an unknown slot as written', () => {
    expect(focusStages('assembled system and end-product and component, material and equipment')).toEqual(['assembled systems and end products', 'components, materials and equipment']);
    expect(focusStages('upstream resource and processing')).toEqual(['upstream resources', 'processing']);
    expect(focusStages('processing')).toEqual(['processing']);
    expect(focusStages('something else')).toEqual(['something else']);
    expect(focusStages(null)).toEqual([]);
    expect(focusWords(['upstream resources'])).toBe('Strongest stage of the chain: upstream resources.');
    expect(focusWords(['assembled systems and end products', 'components, materials and equipment'])).toBe(
      'Strongest stages of the chain: assembled systems and end products; then components, materials and equipment.',
    );
    expect(focusWords([])).toBeNull();
  });
  it('the measures where it is first among allied economies (the gate labels); anything else as written', () => {
    expect(firstOnMeasures(s, 'share of allied demand, outward direct investment')).toEqual(['share of allied demand', 'outward direct investment']);
    expect(firstOnMeasures(s, 'share of allied demand, a new measure')).toEqual(['share of allied demand, a new measure']);
    expect(firstOnMeasures(s, '')).toEqual([]);
  });
  it("a relationship logic in words (broad from four logics); the pair's Partnerships link with the toggle", () => {
    expect(logicWords([])).toBe('No relationship logic recorded');
    expect(logicWords(['investment'])).toBe('An investment relationship');
    expect(logicWords(['trade', 'technology', 'market access'])).toBe('A trade, technology and market access relationship');
    expect(logicWords(['trade', 'investment', 'technology', 'market access'])).toBe('A broad relationship: trade, investment, technology and market access');
    expect(pairHref('jpn', 'USA', 'managed')).toBe('/aib/partnerships?country=JPN&partner=USA');
    expect(pairHref('JPN', 'USA', 'decoupling')).toBe('/aib/partnerships?country=JPN&partner=USA&scn=decoupling');
  });
  it('an aggregated node reads as its members; the Country roles link chooses the economy and keeps its row', () => {
    expect(proseNodeLabel('Uranium/fuel Upstream: aggregated low-coverage sub-sectors (Spent fuel & nuclear scrap, Uranium ores)')).toBe(
      'Spent fuel & nuclear scrap and uranium ores (aggregated)',
    );
    expect(proseNodeLabel('PGMs (iridium, ruthenium)')).toBe('PGMs (iridium, ruthenium)');
    expect(rolesHref('jpn')).toBe('/aib/roles?economy=JPN#role-JPN');
    expect(rolesHref('JPN', 'decoupling')).toBe('/aib/roles?economy=JPN&scn=decoupling#role-JPN');
    expect(rolesHref()).toBe('/aib/roles');
  });
  it("a non-allied supplier's compact role says so at the end", () => {
    const r = { role: 'Processing anchor (non-allied supplier)', leadStep: 'Metals & alloys', leadShare: 0.84, leadBasis: 'share of world exports or production', functions: ['Processing anchor'] } as unknown as AibCompactRole;
    expect(compactRoleWords(r)).toBe('Processing anchor in metals & alloys (84% of world exports or production); a non-allied supplier');
    expect(otherFunctions(r)).toEqual([]);
  });
});

describe.skipIf(!haveFixture(SYNTHETIC_DIR))('strategicSummaryOf (the synthetic fixture)', () => {
  it('Avalon: the headline and secondary line of its narrative, the roles in display order, what it contributes joined to its node and compact', () => {
    const sum = strategicSummaryOf(s, doc.nodes, names, 'aaa')!;
    expect(sum.iso3).toBe('AAA');
    expect(sum.headline).toBe('Combines demand & market and capital & investment roles');
    expect(sum.secondaryLine).toBe('secondary coordination / bridge role');
    expect(sum.firstOn).toBe('share of allied demand');
    expect(sum.firstOnList).toEqual(['share of allied demand']);
    expect(sum.cs.primary.map((r) => r.code)).toEqual(['DMA', 'CIA']);
    expect(heldRoles(sum.cs).map((r) => r.code)).toEqual(['DMA', 'CIA', 'CBH']);
    expect(sum.contributes).toEqual([{ label: 'Magnets, magnet parts & assemblies', nodeId: MAG, compact: expect.objectContaining({ id: 'SC-MAGNETS' }), detail: '18% of world supply' }]);
    expect(sum.focus).toBe('component');
  });

  it('what it needs: each node with its share of allied demand and the allied economies with the largest supply position in it (2% or more, itself and non-allied ones left out)', () => {
    const sum = strategicSummaryOf(s, doc.nodes, names, 'AAA')!;
    expect(sum.needs.map((n) => [n.label, n.nodeId, n.compact?.id, n.detail, n.suppliers])).toEqual([
      ['Rare-earth oxides & compounds', REE, 'SC-MAGNETS', '41% of allied demand', [{ iso3: 'CCC', share: 0.11 }, { iso3: 'DDD', share: 0.06 }]],
      ['Synthetic graphite (anode-grade, spherical)', ANODE, 'SC-BATTERIES', '38% of allied demand', [{ iso3: 'CCC', share: 0.14 }, { iso3: 'BBB', share: 0.08 }]],
    ]);
    expect(sum.dependency).toMatch(/^a significant net buyer of upstream materials/);
    // ZZZ (non-allied, 70%) is never a suggested supplier; below 2% neither
    expect(alliedSuppliersOf(s, REE, 'CCC', 5).map((x) => x.iso3)).toEqual(['DDD', 'AAA']);
    expect(alliedSuppliersOf(s, 'no-such-node', 'AAA')).toEqual([]);
  });

  it('its compacts by relevance; its partners joined to codes and the pair\'s thesis; no capacity slot', () => {
    const sum = strategicSummaryOf(s, doc.nodes, names, 'AAA')!;
    expect(sum.compacts.map((r) => r.compact)).toEqual(['SC-WIND', 'SC-MAGNETS', 'SC-BATTERIES', 'SC-GRID', 'SC-SOLAR']);
    expect(compactRoleWords(sum.compacts[1])).toBe('Demand anchor in magnets (36% of world imports)');
    expect(sum.partners.map((p) => [p.iso3, p.name, p.thesis?.logic, p.thesis?.compacts.map((c) => c.id)])).toEqual([
      ['CCC', 'Carpania', ['trade', 'technology', 'resilience / second sourcing'], ['SC-MAGNETS', 'SC-BATTERIES']],
      ['BBB', 'Borduria', ['trade', 'market access'], ['SC-WIND', 'SC-GRID']],
      ['EEE', 'Elbonia', ['trade', 'investment'], ['SC-WIND', 'SC-SOLAR']],
    ]);
    // the thesis is oriented from the economy read: what each side contributes
    expect(sum.partners[0].thesis!.complementarity.map((x) => [x.compact.id, x.a?.role, x.b?.role])).toEqual([
      ['SC-MAGNETS', 'Demand anchor', 'Processing anchor'],
      ['SC-BATTERIES', 'Demand anchor', 'Processing anchor'],
    ]);
    expect(sum.partnersScenario).toBe('Managed Interdependence');
    expect(sum.capacity).toEqual([]);
    expect(sum.capacityTotal).toBeNull();
    expect(sum.hostIn).toEqual([]);
  });

  it('the items, when the export carries them, give the same summary as the slots without reading the prose', () => {
    const d2 = d();
    const s2 = d2.strategic!;
    const a = s2.countries.find((x) => x.iso3 === 'AAA')!;
    a.items = { firstOn: ['demand_mean_share'], contributes: [[MAG, 0.18]], focus: ['components'], needs: [[REE, 0.41], [ANODE, 0.38]], compacts: [], partners: ['CCC', 'BBB', 'EEE'], capacity: [], capacityUsdBn: null };
    a.slots = { ...a.slots!, firstOn: 'garbled', contributes: 'garbled', needs: 'garbled', partners: 'garbled' };
    const c = s2.countries.find((x) => x.iso3 === 'CCC')!;
    c.items = { firstOn: [], contributes: [], focus: [], needs: [], compacts: [], partners: [], capacity: [[ELY, 1.0], [MAG, 0.6]], capacityUsdBn: 1.6 };
    c.slots = { ...c.slots!, capacity: 'garbled' };
    const viaItems = strategicSummaryOf(s2, d2.nodes, names, 'AAA')!;
    const viaSlots = strategicSummaryOf(s, doc.nodes, names, 'AAA')!;
    for (const k of ['contributes', 'needs', 'partners', 'firstOnList'] as const) expect(viaItems[k]).toEqual(viaSlots[k]);
    expect(viaItems.focusList).toEqual(['components, materials and equipment']);
    const cc = strategicSummaryOf(s2, d2.nodes, names, 'CCC')!;
    expect(cc.capacity.map((e) => e.nodeId)).toEqual([ELY, MAG]);
    expect(cc.capacityTotal).toBe('US$1.6bn of unmet allied demand');
  });

  it('Carpania: where new capacity matters, its total, the compacts where it is a capacity-building host; a role beyond the cap; other functions', () => {
    const sum = strategicSummaryOf(s, doc.nodes, names, 'CCC')!;
    expect(sum.capacity.map((e) => [e.label, e.nodeId, e.compact?.id])).toEqual([
      ['Electrolysers', ELY, 'SC-GRID'],
      ['Magnets, magnet parts & assemblies', MAG, 'SC-MAGNETS'],
    ]);
    expect(sum.capacityTotal).toBe('US$1.6bn of unmet allied demand');
    expect(sum.hostIn.map((c) => c.id)).toEqual(['SC-MAGNETS', 'SC-GRID']);
    expect(sum.cs.beyondCap.map((r) => r.code)).toEqual(['DSP']);
    expect(gateWords(sum.cs.beyondCap[0].gates[0])).toBe('unmet allied demand it can host: US$1.6 bn (90th percentile)');
    expect(otherFunctions(sum.compacts[0])).toEqual(['Specialized technology supplier', 'Capacity-building host']);
    expect(sum.needs).toEqual([]);
  });

  it('an economy without a narrative: the headline from its roles; a non-allied supplier; an economy the layer does not assess', () => {
    const fff = strategicSummaryOf(s, doc.nodes, names, 'FFF')!;
    expect(fff.headline).toBe('No strategic AIB role at the current thresholds; it contributes through its compact-specific roles');
    expect([fff.contributes, fff.needs, fff.partners, fff.capacity]).toEqual([[], [], [], []]);
    const ggg = strategicSummaryOf(s, doc.nodes, names, 'GGG')!;
    expect(ggg.allied).toBe(false);
    expect(ggg.cs.metrics).toEqual({});
    const zzz = strategicSummaryOf(s, doc.nodes, names, 'ZZZ')!;
    expect(zzz.headline).toBe('Resource & Processing Anchor, Scale Manufacturing & Integration Platform');
    expect(zzz.secondaryLine).toBe('secondary: Specialized Technology & Manufacturing Anchor');
    expect(strategicSummaryOf(s, doc.nodes, names, 'HHH')).toBeNull();
  });

  it('the default economy (alphabetical, a primary role) and the selector (allied, then the major non-allied suppliers)', () => {
    expect(defaultEconomy(s, name)).toBe('AAA');
    // by name, not by code: rename Avalon and the first allied economy with a primary role changes
    expect(defaultEconomy(s, (iso) => (iso === 'AAA' ? 'Zorro' : name(iso)))).toBe('BBB');
    const opts = economyOptions(s, name);
    expect(opts.map((o) => o.value)).toEqual(['AAA', 'BBB', 'CCC', 'DDD', 'EEE', 'FFF', 'GGG', 'ZZZ']);
    expect(opts.map((o) => o.group)).toEqual([...Array(6).fill('Allied economies'), ...Array(2).fill('Major non-allied suppliers (supply roles only)')]);
  });

  it('the Country profile\'s wrapper: the layer and the summary; null without the layer; the summary null for an economy outside it', () => {
    const p = aibStrategicOf(doc, names, 'CCC')!;
    expect(p.s).toBe(s);
    expect(p.sum!.iso3).toBe('CCC');
    expect(aibStrategicOf(doc, names, 'HHH')!.sum).toBeNull();
    expect(aibStrategicOf(stripStrategic(d()), names, 'CCC')).toBeNull();
    expect(aibStrategicOf(null, names, 'CCC')).toBeNull();
  });

  it('deterministic: the same inputs give the same summary', () => {
    expect(strategicSummaryOf(d().strategic!, doc.nodes, names, 'CCC')).toEqual(strategicSummaryOf(s, doc.nodes, names, 'CCC'));
  });
});

describe.skipIf(!haveFixture(REAL_DIR) || !readDoc(REAL_DIR).strategic)('strategicSummaryOf on the real export (public/data/aib)', () => {
  const real = readDoc(REAL_DIR);
  const rs = real.strategic!;
  const rnames = real.countries as Record<string, string>;

  it('every slot item of every economy resolves to a node of a strategic compact, every partner to an economy with a thesis', () => {
    const unresolved: string[] = [];
    for (const c of rs.countries) {
      const sum = strategicSummaryOf(rs, real.nodes, rnames, c.iso3)!;
      for (const e of [...sum.contributes, ...sum.needs, ...sum.capacity]) if (!e.nodeId || !e.compact) unresolved.push(`${c.iso3}: ${e.label}`);
      for (const p of sum.partners) if (!p.iso3 || !p.thesis) unresolved.push(`${c.iso3}: partner ${p.name}`);
      if (c.slots?.capacity) expect(sum.capacityTotal).toMatch(/^US\$[\d.]+bn of unmet allied demand$/);
    }
    expect(unresolved).toEqual([]);
  });

  it('the items name the nodes and partners the prose slots name (and an aggregated node by the same prose label)', () => {
    const bare = structuredClone(rs);
    for (const c of bare.countries) delete c.items;
    let withItems = 0;
    for (const c of rs.countries) {
      if (!c.items) continue;
      withItems += 1;
      const a = strategicSummaryOf(rs, real.nodes, rnames, c.iso3)!;
      const b = strategicSummaryOf(bare, real.nodes, rnames, c.iso3)!;
      for (const k of ['contributes', 'needs', 'capacity'] as const) {
        expect(a[k].length, `${c.iso3} ${k}`).toBe(b[k].length);
        a[k].forEach((e, i) => (b[k][i].nodeId ? expect(e.nodeId).toBe(b[k][i].nodeId) : expect(e.label).toBe(b[k][i].label)));
      }
      expect(a.partners.map((p) => p.iso3)).toEqual(b.partners.map((p) => p.iso3));
      expect(a.firstOnList).toEqual(b.firstOnList);
      expect(a.focusList).toEqual(b.focusList);
    }
    expect(withItems).toBe(rs.countries.filter((c) => c.slots).length);
  });

  it('what an economy contributes is among the division of labour\'s own anchor and specialist positions (R orders them by US$ value, 2026-10-21)', () => {
    for (const c of rs.countries) {
      const sum = strategicSummaryOf(rs, real.nodes, rnames, c.iso3)!;
      if (!sum.contributes.length) continue;
      const positions = new Set(
        rs.compacts
          .flatMap((x) => divisionCells(x))
          .filter((x) => x.iso3 === c.iso3 && (x.flags.anchor || x.flags.specialist))
          .map((x) => x.node),
      );
      for (const e of sum.contributes) expect(e.nodeId !== null && positions.has(e.nodeId)).toBe(true);
    }
  });

  it('every assessed economy has a summary; its roles are the export\'s', () => {
    for (const c of rs.countries) {
      const sum = strategicSummaryOf(rs, real.nodes, rnames, c.iso3)!;
      expect(sum.cs).toEqual(countryStrategicOf(rs, c.iso3));
      expect(sum.cs.primary.length).toBeLessThanOrEqual(rs.thresholds.maxPrimary);
    }
  });
});
