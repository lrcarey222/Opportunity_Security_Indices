// The shared three-lens partnerships derivations (2026-10-19): the kinds, the lens words, the cross-lens
// summary (which lenses name a partner, in plain words; a lens without data at the focus is left out, never
// said to differ), the Today / OSI helpers on the synthetic ties chunk (agreements, UN votes, direct-
// investment partners, the coalition) and the aid flows of the Development lens.

import { describe, expect, it } from 'vitest';
import type { SectorDevChunk } from '../../sector/contract.ts';
import { syntheticTies } from '../../views/Partners/tiesFixture.ts';
import { agreementList, agreementMembers, agreementTies, alignmentCounts, alignmentWords, coalitionOf, dipPartners, tierWords } from '../derive.ts';
import {
  comparedWords, crossLens, kindOf, KIND_LABEL, KIND_LENS_WORDS, LENS_IDS, LENS_LOADING, lensNames, lensNone, PARTNERSHIP_KINDS, readingWords, summaryLine, type LensNames,
} from '../lenses.ts';
import { aidItemsOfStage, aidSharedStages, aidStages, odaPartners, odaRecipients, odaView, type AidCellLink } from '../oda.ts';
import { bareAcronymsInText } from '../../components/testing/acronyms.ts';

const ready = (keys: string[]): LensNames => ({ status: 'ready', keys });

describe('kinds and lenses', () => {
  it('four kinds, Trade by default; three lenses in the panel order', () => {
    expect(PARTNERSHIP_KINDS).toEqual(['trade', 'investment', 'development', 'geopolitical']);
    expect(PARTNERSHIP_KINDS.map((k) => KIND_LABEL[k])).toEqual(['Trade', 'Investment', 'Development', 'Geopolitical']);
    expect(kindOf('geopolitical')).toBe('geopolitical');
    expect(kindOf('nope')).toBe('trade');
    expect(kindOf(null)).toBe('trade');
    expect(LENS_IDS).toEqual(['today', 'osi', 'aib']);
  });

  it('every kind has words for every lens, without a bare acronym; development notes the export-credit gap', () => {
    for (const k of PARTNERSHIP_KINDS) for (const l of LENS_IDS) {
      expect(KIND_LENS_WORDS[k][l].length).toBeGreaterThan(20);
      expect(bareAcronymsInText(KIND_LENS_WORDS[k][l])).toEqual([]);
    }
    expect(KIND_LENS_WORDS.development.today).toMatch(/Export credits are not in this build/);
  });

  it('lensNames keeps the order, drops duplicates and the focus, and caps the list', () => {
    expect(lensNames(['JPN', 'USA', 'JPN', 'DEU', '', 'KOR'], 3, 'USA')).toEqual({ status: 'ready', keys: ['JPN', 'DEU', 'KOR'] });
    expect(lensNone('no data here')).toEqual({ status: 'none', keys: [], reason: 'no data here' });
  });
});

describe('the cross-lens summary', () => {
  it('names each reading in plain words (the spec phrases)', () => {
    expect(readingWords({ today: true, osi: true, aib: true })).toBe('A tie today that ranks high on the partner scores and that the AIB keeps');
    expect(readingWords({ today: false, osi: true, aib: false })).toBe('Ranked high on the partner scores, not a tie today and not chosen by the AIB');
    expect(readingWords({ today: false, osi: false, aib: true })).toBe('Chosen by the AIB, not a tie today and not ranked high on the partner scores');
    expect(readingWords({ today: true, osi: true, aib: false })).toBe('A tie today that ranks high on the partner scores, not chosen by the AIB');
    expect(readingWords({ today: true, osi: false, aib: true })).toBe('A tie today that the AIB keeps, not ranked high on the partner scores');
    expect(readingWords({ today: false, osi: true, aib: true })).toBe('Ranked high on the partner scores and chosen by the AIB, not a tie today');
    expect(readingWords({ today: true, osi: false, aib: false })).toBe('A tie today, not ranked high on the partner scores and not chosen by the AIB');
  });

  it('a lens that is not compared never speaks: no "not a tie today" without the Today lens', () => {
    expect(readingWords({ today: null, osi: true, aib: false })).toBe('Ranked high on the partner scores, not chosen by the AIB');
    expect(readingWords({ today: true, osi: null, aib: true })).toBe('A tie today that the AIB keeps');
    expect(readingWords({ today: null, osi: true, aib: true })).toBe('Ranked high on the partner scores and chosen by the AIB');
  });

  it('groups every named partner by which lenses name it, in a fixed order, the lenses\' best position first', () => {
    const cl = crossLens({ today: ready(['JPN', 'DEU', 'KOR', 'CHN']), osi: ready(['DEU', 'JPN', 'IND']), aib: ready(['VNM', 'JPN', 'IND']) });
    expect(cl.compared).toEqual(['today', 'osi', 'aib']);
    expect(cl.total).toBe(6);
    expect(cl.groups.map((g) => [g.id, g.keys])).toEqual([
      ['today+osi+aib', ['JPN']],
      ['today+osi', ['DEU']],
      ['osi+aib', ['IND']],
      ['today', ['KOR', 'CHN']],
      ['aib', ['VNM']],
    ]);
    expect(cl.groups[0].words).toBe('A tie today that ranks high on the partner scores and that the AIB keeps');
    expect(summaryLine(cl, { one: 'partner', many: 'partners' })).toBe('The three lenses name 6 partners: 1 by all three, 2 by two of them and 3 by one.');
  });

  it('a loading lens and a lens with no data at the focus are listed apart and left out of the readings', () => {
    const cl = crossLens({ today: lensNone('no aid data for Zembla'), osi: ready(['AAA', 'BBB']), aib: LENS_LOADING });
    expect(cl.compared).toEqual(['osi']);
    expect(cl.pending).toEqual(['aib']);
    expect(cl.absent).toEqual([{ id: 'today', reason: 'no aid data for Zembla' }]);
    expect(cl.groups.map((g) => g.words)).toEqual(['Ranked high on the partner scores']);
    expect(summaryLine(cl, { one: 'partner', many: 'partners' })).toBe('');
    const two = crossLens({ today: lensNone('none'), osi: ready(['AAA', 'BBB']), aib: ready(['BBB', 'CCC']) });
    expect(two.groups.map((g) => [g.words, g.keys])).toEqual([
      ['Ranked high on the partner scores and chosen by the AIB', ['BBB']],
      ['Ranked high on the partner scores, not chosen by the AIB', ['AAA']],
      ['Chosen by the AIB, not ranked high on the partner scores', ['CCC']],
    ]);
    expect(summaryLine(two, { one: 'economy', many: 'economies' })).toBe('Partner score ranking and AIB analysis name 3 economies: 1 by both and 2 by one.');
    expect(comparedWords(['today', 'aib'])).toBe('Today and AIB analysis');
  });
});

describe('Today and OSI helpers (synthetic ties chunk)', () => {
  const ties = syntheticTies();

  it('agreement ties: the economies sharing an agreement, most agreements first, with UN-vote alignment', () => {
    expect(agreementTies(ties, 'USA')).toEqual([
      { iso3: 'JPN', agreements: ['IPEF', 'MSP'], alignment: 0.8 },
      { iso3: 'IND', agreements: ['IPEF'], alignment: null },
    ]);
    expect(agreementMembers(ties).map((r) => [r.iso3, r.agreements])).toEqual([
      ['JPN', ['IPEF', 'MSP']],
      ['USA', ['IPEF', 'MSP']],
      ['IND', ['IPEF']],
    ]);
    expect(agreementList(ties)).toEqual([
      { agreement: 'IPEF', members: ['IND', 'JPN', 'USA'] },
      { agreement: 'MSP', members: ['JPN', 'USA'] },
    ]);
    expect(alignmentCounts(ties, 'USA')).toEqual({ same: 0, mixed: 1, opposite: 0 });
    expect(alignmentWords(1)).toBe('votes in the same bloc');
    expect(alignmentWords(0)).toBe('votes in opposite blocs');
    expect(alignmentWords(null)).toBe('UN votes not in the data');
  });

  it('direct-investment partners by size, conduit financial centres left out (as Friendshore does)', () => {
    expect(dipPartners(ties, 'USA')).toEqual(['JPN', 'DEU', 'IND']);
  });

  it('the coalition: core members first, then partners; tiers in words', () => {
    const c = coalitionOf([
      { iso3: 'ZZZ', name: 'Zembla', tier: 'none' },
      { iso3: 'BBB', name: 'Borduria', tier: 'partner' },
      { iso3: 'CCC', name: 'Carpania', tier: 'core' },
      { iso3: 'AAA', name: 'Avalon', tier: 'core' },
    ]);
    expect(c.map((x) => x.iso3)).toEqual(['AAA', 'CCC', 'BBB']);
    expect(tierWords('core')).toBe('a core member of the allied coalition');
    expect(tierWords('none')).toBe('outside the allied coalition');
  });
});

describe('aid flows (the Development lens, Today)', () => {
  // items 0 energy, 1 mining; geos 0..3
  const rows = [
    { item: 0, year: 2020, donor: 0, recipient: 1, value: 10 },
    { item: 0, year: 2021, donor: 0, recipient: 1, value: 5 },
    { item: 0, year: 2021, donor: 2, recipient: 1, value: 7 },
    { item: 1, year: 2022, donor: 0, recipient: 3, value: 4 },
    { item: 1, year: 2022, donor: 1, recipient: 3, value: -2 },
  ];
  const col = <K extends keyof (typeof rows)[number]>(k: K) => rows.map((r) => r[k]);
  const chunk = { 'dev.oda': { nrow: rows.length, cols: { item: col('item'), year: col('year'), donor: col('donor'), recipient: col('recipient'), value: col('value') } } } as unknown as SectorDevChunk;

  it('sums the aid sectors and years in scope; an economy\'s donors and recipients, largest first', () => {
    const v = odaView(chunk, [0, 1], 1);
    expect(v.years).toEqual([2020, 2022]);
    expect(v.received).toEqual([
      { geo: 0, v: 15 },
      { geo: 2, v: 7 },
    ]);
    // a negative net flow (mining net of its coal and oil-and-gas sub-rows) is not a recipient
    expect(v.given).toEqual([]);
    expect(odaPartners(v)).toEqual([0, 2]);
    const all = odaView(chunk, [0], null);
    expect(all.flows.map((f) => [f.from, f.to, f.v])).toEqual([
      [0, 1, 15],
      [2, 1, 7],
    ]);
    expect(odaRecipients(all)).toEqual([1]);
    expect(odaView(chunk, [], null).years).toBeNull();
  });

  it('six stages: a stage reads the aid sectors linked to its cells (energy at Deployment, industry at Refining, Components and Assembly, mining at Extraction, none at Recovery)', () => {
    // items 0 energy, 1 mining, 2 industry, as scripts/87 links them (the pipeline's CRS purpose crosswalk);
    // item 3 is the "All sectors" total, linked to no cell
    const links: AidCellLink[] = [
      { stage: 'Extraction', items: [1] },
      { stage: 'Refining', items: [2] },
      { stage: 'Components', items: [2] },
      { stage: 'Assembly', items: [2] },
      { stage: 'Deployment', items: [0] },
      { stage: 'Recovery', items: [] },
      { stage: 'Deployment', items: [0] },
    ];
    expect(aidItemsOfStage(links, [0, 1, 2], 'Deployment')).toEqual([0]);
    expect(aidItemsOfStage(links, [0, 1, 2], 'Components')).toEqual([2]);
    expect(aidItemsOfStage(links, [0, 1, 2], 'Extraction')).toEqual([1]);
    expect(aidItemsOfStage(links, [0, 1, 2], 'Recovery')).toEqual([]);
    // a legacy stage name matches no cell
    expect(aidItemsOfStage(links, [0, 1, 2], 'Midstream')).toEqual([]);
    // no stage: the three linked aid sectors, never the All sectors total (it would count them again)
    expect(aidItemsOfStage(links, [0, 1, 2, 3], null)).toEqual([0, 1, 2]);
    expect(aidItemsOfStage(links, [3, 2, 1, 0], null)).toEqual([2, 1, 0]);
    expect(aidStages(links)).toEqual(['Extraction', 'Refining', 'Components', 'Assembly', 'Deployment']);
    // the same industry aid sits on three stages: never added across them
    expect(aidSharedStages(links, [2], 'Components')).toEqual(['Refining', 'Assembly']);
    expect(aidSharedStages(links, [0], 'Deployment')).toEqual([]);
  });
});
