// The Agreements page's pure derivations on the synthetic bundle: URL filters (checked against the
// vocabularies), the agreement filter, partner ranking by each transparent sort key (no composite score),
// coverage tallies (established vs mentioned), timeline bins, the global pairs and network matrix, existence
// vs delivery words, and the strategic-gap and execution-gap sentences with their exact numbers.

import { describe, expect, it } from 'vitest';
import type { AgrAgreement, AgreementsGlobal, AgrGap, AgrVocab } from '../contract.ts';
import {
  ABSENCE_CAVEAT, gapMatrix, stageStates, activeKeys, alignmentWords, bestGrades, compactName, complementarityNotAssessed, complementarityWord, depthRestsOnEu, gapComponents, gapsInScope, viaEuForPair, agreementPasses, architectureOf, codeTallies, countBin, countryChoices, dateWords, deliveryWords, depthMatrix, depthName, depthRuleWords, executionGapText,
  existenceWords, filterAgreements, filterOfSearch, filterPairs, gapGroups, gapText, globalDates, globalPairs, gradedSectors, gradeOf, indexCountry, instrumentBases, instrumentCoverage, labelOf, NO_FILTER, orderPairs, pairInScope,
  partnersAtDepth, partnerTipWords, percentileWords, primarySource, priorityBasisWords, rankPartners, recentAgreements, sectorCoverage, setFilterSearch, sortKeyOf, statusCounts, timelineBins, topEconomies, zeroDepthShort, zeroDepthWords,
  type AgrFilter, type GlobalPair,
} from '../derive.ts';
import { readCountry, readGlobal, readManifest } from './fixture.ts';

const m = readManifest();
const v = m.vocab;
const jpn = readCountry('JPN');
const aus = readCountry('AUS');
const deu = readCountry('DEU');
const f = (over: Partial<AgrFilter> = {}): AgrFilter => ({ ...NO_FILTER, ...over });
const byId = (file: typeof jpn, id: string) => file.agreements.find((a) => a.id === id) as AgrAgreement;

describe('words', () => {
  it('dates at their precision, percentiles, labels that spell acronyms out', () => {
    expect(dateWords('2014-07-08')).toBe('8 July 2014');
    expect(dateWords('2022-10')).toBe('October 2022');
    expect(dateWords('2018')).toBe('2018');
    expect(dateWords(null)).toBeNull();
    expect(percentileWords(0.93)).toBe('93rd percentile');
    expect(percentileWords(0.81)).toBe('81st percentile');
    expect(percentileWords(0.12)).toBe('12th percentile');
    expect(percentileWords(null)).toBeNull();
    expect(labelOf(v, 'agreement_types', 'mou')).toBe('Memorandum of understanding (MOU)');
    expect(labelOf(v, 'agreement_types', 'fta')).not.toMatch(/FTA/);
    expect(labelOf(v, 'sources', 'gsp')).toBe('Global Strategic Partnership Dataset (GSP)');
    expect(labelOf(v, 'sectors', 'critical_minerals')).toBe('Critical minerals');
    expect(depthName(v, 5)).toBe('Integrated partnership');
    expect(ABSENCE_CAVEAT).toMatch(/does not mean there is no cooperation/);
  });

  it('existence and delivery are separate: the status verb and legal status vs the delivery evidence', () => {
    const epa = byId(jpn, 'agr-1a2b3c4d5e');
    expect(existenceWords(epa)).toBe('Exists: in force since 15 January 2015 · binding');
    expect(deliveryWords(epa, v)).toEqual({ has: false, words: 'Delivery: no evidence recorded' });
    const cmp = byId(jpn, 'agr-4d5e6f7a8b');
    expect(existenceWords(cmp)).toBe('Exists: dated October 2022 · non-binding');
    expect(deliveryWords(cmp, v)).toEqual({ has: true, words: 'Delivery: implementation underway (2 items)' });
    const hy = byId(jpn, 'agr-5e6f7a8b9c');
    expect(deliveryWords(hy, v).words).toBe('Delivery: implemented, operational (1 item)');
    expect(existenceWords(byId(jpn, 'agr-6f7a8b9c0d'))).toBe('Exists: announced September 2025 · binding status unknown');
    expect(existenceWords(byId(jpn, 'agr-9c0d1e2f3a'))).toBe('Exists: signed 10 April 2024 · non-binding');
  });

  it('grades: a body-text mention is never established', () => {
    expect(gradeOf('text_body', v)).toBe('text');
    for (const b of ['source_coded', 'curated', 'type_implied', 'text_title']) expect(gradeOf(b, v), b).toBe('specific');
  });

  it('an instrument named only in the title is mentioned, not established (vocab.established_instrument_bases, else the specific bases without text_title)', () => {
    expect(v.established_instrument_bases).toEqual(['source_coded', 'curated', 'type_implied']);
    expect(gradeOf('text_title', v, 'instruments')).toBe('text');
    expect(gradeOf('text_title', v, 'sectors')).toBe('specific');
    for (const b of ['source_coded', 'curated', 'type_implied']) expect(gradeOf(b, v, 'instruments'), b).toBe('specific');
    // an older manifest without the field: the specific bases minus the title keyword
    const old = { ...v, established_instrument_bases: undefined, specific_bases: undefined } as AgrVocab;
    expect(instrumentBases(old)).toEqual(['source_coded', 'curated', 'type_implied']);
    expect(gradeOf('text_title', old, 'instruments')).toBe('text');
    // the hydrogen supply-chain memorandum names long-term supply in its title only
    const hy = byId(jpn, 'agr-5e6f7a8b9c');
    expect(bestGrades(hy.instruments, v, 'instruments').get('long_term_supply')?.grade).toBe('text');
    const t = codeTallies([hy], v).groups.find((g) => g.id === 'supply');
    expect(t).toEqual({ id: 'supply', specific: 0, text: 1 });
  });

  it('the primary source: the official text, an official page from a source record, or not identified', () => {
    expect(primarySource(byId(jpn, 'agr-1a2b3c4d5e'), v)).toEqual({ kind: 'text', url: 'https://www.dfat.gov.au/trade/agreements/in-force/jaepa', words: 'official text' });
    // no primary_url, but an official source: the curated record's link
    const hy = byId(jpn, 'agr-5e6f7a8b9c');
    expect(hy.primary_url).toBeNull();
    expect(hy.primary_available).toBe(true);
    expect(primarySource(hy, v)).toEqual({ kind: 'page', url: hy.sources[0].url, words: 'official page' });
    // a DESTA record links the dataset's site, never an official page
    const desta = { ...hy, sources: [{ dataset: 'desta', record_id: 'x', url: 'https://www.designoftradeagreements.org/downloads/' }] };
    expect(primarySource(desta, v)).toEqual({ kind: 'page', url: null, words: 'official page' });
    // a membership record linking the USTR page (also that dataset's own link) is an official page
    const ustr = 'https://ustr.gov/trade-agreements/free-trade-agreements';
    expect(v.sources.find((x) => x.id === 'memberships')?.url).toBe(ustr);
    expect(primarySource({ ...desta, sources: [...desta.sources, { dataset: 'memberships', record_id: 'm', url: ustr }] }, v).url).toBe(ustr);
    expect(primarySource(byId(jpn, 'agr-0d1e2f3a4b'), v)).toEqual({ kind: 'none', url: null, words: 'not identified' });
  });
});

describe('filters', () => {
  it('reads the URL against the vocabularies; drops unknown values; the default scope is electro-industrial', () => {
    const q = new URLSearchParams('country=jpn&partner=AUS&from=2015&to=x&type=mou&sector=nope&tech=Batteries&igroup=finance&status=delivery&depth=3&scope=all');
    const x = filterOfSearch(q, m);
    expect(x).toMatchObject({ country: 'JPN', partner: 'AUS', from: 2015, to: null, type: 'mou', sector: null, tech: 'Batteries', igroup: 'finance', status: 'delivery', depth: 3, scope: 'all' });
    expect(filterOfSearch(new URLSearchParams('partner=AUS'), m).partner).toBeNull();
    expect(filterOfSearch(new URLSearchParams(''), m).scope).toBe('ei');
    expect(activeKeys(x)).toEqual(['from', 'type', 'tech', 'igroup', 'status', 'depth']);
  });

  it('writes the URL: the default scope left out, a partner dropped with the economy, an instrument with its group', () => {
    const q = new URLSearchParams('country=JPN&partner=AUS&igroup=finance&inst=export_credit&scn=decoupling');
    expect(setFilterSearch(q, 'scope', 'ei').has('scope')).toBe(false);
    expect(setFilterSearch(q, 'scope', 'all').get('scope')).toBe('all');
    const c = setFilterSearch(q, 'country', 'AUS');
    expect(c.get('partner')).toBeNull();
    expect(c.get('scn')).toBe('decoupling');
    expect(setFilterSearch(q, 'igroup', 'trade').has('inst')).toBe(false);
  });

  it('an agreement passes on its own fields: scope, years, type, sector, technology, segment, instrument group, legal status, delivery, source', () => {
    const epa = byId(jpn, 'agr-1a2b3c4d5e');
    expect(agreementPasses(epa, f(), v)).toBe(true);
    expect(agreementPasses(byId(jpn, 'agr-0d1e2f3a4b'), f(), v)).toBe(false); // the U.S.-Japan trade agreement is not electro-industrial
    expect(agreementPasses(byId(jpn, 'agr-0d1e2f3a4b'), f({ scope: 'all' }), v)).toBe(true);
    expect(agreementPasses(epa, f({ from: 2015 }), v)).toBe(false);
    expect(agreementPasses(epa, f({ tech: 'Batteries' }), v)).toBe(true); // critical minerals map to Batteries
    expect(agreementPasses(epa, f({ segment: 'Refining' }), v)).toBe(true);
    expect(agreementPasses(epa, f({ igroup: 'finance' }), v)).toBe(false);
    expect(agreementPasses(epa, f({ igroup: 'investment' }), v)).toBe(true);
    expect(agreementPasses(epa, f({ legal: 'non_binding' }), v)).toBe(false);
    expect(agreementPasses(byId(jpn, 'agr-4d5e6f7a8b'), f({ status: 'delivery' }), v)).toBe(true);
    expect(agreementPasses(epa, f({ status: 'delivery' }), v)).toBe(false);
    expect(agreementPasses(epa, f({ source: 'desta' }), v)).toBe(true);
    expect(agreementPasses(epa, f({ source: 'gsp' }), v)).toBe(false);
  });

  it("an economy's agreements with a partner and a depth floor", () => {
    expect(filterAgreements(jpn, f({ scope: 'all', partner: 'NZL' }), v).map((a) => a.id)).toEqual(['agr-1c2d3e4f5a']);
    expect(filterAgreements(jpn, f({ scope: 'all', partner: 'NZL', depth: 1 }), v)).toEqual([]);
    const deep = filterAgreements(jpn, f({ depth: 5 }), v);
    expect(deep.length).toBeGreaterThan(0);
    for (const a of deep) expect(a.parties).toContain('AUS');
  });
});

describe('partner ranking: one transparent key at a time, no composite score', () => {
  it('depth: the pair class, then the number of matching agreements', () => {
    const r = rankPartners(jpn, f(), 'depth', v, m.names);
    expect(r[0].partner).toBe('AUS');
    expect(r[0].depth).toBe(5);
    for (let i = 1; i < r.length; i++) expect(r[i - 1].depth).toBeGreaterThanOrEqual(r[i].depth);
    // no partner without a matching agreement (New Zealand's only agreement is not electro-industrial)
    expect(r.some((x) => x.partner === 'NZL')).toBe(false);
    const nzl = rankPartners(jpn, f({ scope: 'all' }), 'depth', v, m.names).find((x) => x.partner === 'NZL');
    // class 0 with one agreement in the scope: it does not count, it is not "none found"
    expect(nzl).toMatchObject({ depth: 0, nScope: 1 });
    expect(r.find((x) => x.partner === 'AUS')?.nScope).toBe(jpn.agreements.filter((a) => a.ei && a.parties.includes('AUS')).length);
  });

  it('breadth, sector relevance, implementation and recency each order by their own number', () => {
    const all = f({ scope: 'all' });
    const b = rankPartners(jpn, all, 'breadth', v, m.names);
    for (let i = 1; i < b.length; i++) expect(b[i - 1].n).toBeGreaterThanOrEqual(b[i].n);
    expect(b[0].n).toBe(b[0].nTotal); // no narrowing filter: matching = all
    const s = rankPartners(jpn, all, 'sector', v, m.names);
    for (let i = 1; i < s.length; i++) expect(s[i - 1].eiSpecific).toBeGreaterThanOrEqual(s[i].eiSpecific);
    const im = rankPartners(jpn, all, 'implementation', v, m.names);
    expect(im[0].partner).toBe('AUS');
    expect(im[0].nDelivery).toBe(2);
    const re = rankPartners(jpn, all, 'recency', v, m.names);
    for (let i = 1; i < re.length; i++) expect((re[i - 1].newest ?? '') >= (re[i].newest ?? '')).toBe(true);
    expect(sortKeyOf('bogus')).toBe('depth');
  });

  it('a narrowing filter counts only the matching agreements', () => {
    const r = rankPartners(jpn, f({ scope: 'all', source: 'desta' }), 'breadth', v, m.names);
    const ausRow = r.find((x) => x.partner === 'AUS');
    expect(ausRow?.n).toBe(2);
    expect(ausRow?.nTotal).toBe(8); // seven bilateral agreements and the Minerals Security Partnership
  });
});

describe('coverage, tallies, timeline', () => {
  it('sectors x partners keep the grade of the pair (established vs mentioned); scope limits the sectors', () => {
    const cov = sectorCoverage(jpn.partners, 'ei', v, m.names);
    const ausRow = cov.rows.find((r) => r.partner === 'AUS');
    expect(ausRow?.cells.critical_minerals).toBe('specific');
    expect(cov.sectors).not.toContain('trade');
    expect(sectorCoverage(jpn.partners, 'all', v, m.names).sectors).toContain('trade');
    const deuRow = cov.rows.find((r) => r.partner === 'DEU');
    expect(deuRow?.cells.hydrogen).toBe('text');
  });

  it('instrument groups x partners: 0 none, 1 mentioned, 2 established; the electro-industrial scope reads the electro-industrial agreements only', () => {
    const ic = instrumentCoverage(jpn.partners, v, m.names);
    const ausRow = ic.rows.find((r) => r.partner === 'AUS');
    expect(ausRow?.cells.trade).toBe(2);
    expect(ausRow?.cells.offtake).toBe(1);
    expect(ic.groups).toContain('finance');
    // the United States: the trade agreement (not electro-industrial) holds the only trade instruments
    const ix = indexCountry(jpn);
    expect(ic.rows.find((r) => r.partner === 'USA')?.cells.trade).toBe(2);
    const ei = instrumentCoverage(jpn.partners, v, m.names, { scope: 'ei', ix });
    expect(ei.rows.find((r) => r.partner === 'USA')?.cells.trade).toBe(0);
    expect(ei.rows.find((r) => r.partner === 'USA')?.cells.standards).toBe(2);
    // without the index the pair profile is used whatever the scope
    expect(instrumentCoverage(jpn.partners, v, m.names, { scope: 'ei' }).rows.find((r) => r.partner === 'USA')?.cells.trade).toBe(2);
  });

  it('tallies by sector and instrument group, and statuses with delivery apart', () => {
    const t = codeTallies(jpn.agreements, v);
    const cm = t.sectors.find((s) => s.id === 'critical_minerals');
    expect(cm && cm.specific + cm.text).toBe(jpn.agreements.filter((a) => a.sectors.some(([c]) => c === 'critical_minerals')).length);
    const st = statusCounts(jpn.agreements, v);
    expect(st.filter((s) => s.delivery).map((s) => s.status).sort()).toEqual(['implementation_underway', 'operational']);
    expect(st.reduce((n, s) => n + s.n, 0)).toBe(jpn.agreements.length);
  });

  it('timeline bins: every year from the first to the last, counts that add up', () => {
    const t = timelineBins(jpn.agreements, v);
    expect(t.bins[0].year).toBe(1957);
    expect(t.bins.at(-1)?.year).toBe(2025);
    expect(t.bins.length).toBe(2025 - 1957 + 1);
    expect(t.bins.reduce((n, b) => n + b.n, 0) + t.undated).toBe(jpn.agreements.length);
    expect(recentAgreements(jpn.agreements, m.as_of, 5).every((a) => Number(a.date?.slice(0, 4)) >= 2021)).toBe(true);
  });
});

describe('the global pairs and the network', () => {
  const pairs = globalPairs(readGlobal());
  it('scope-aware partner counts at depth 3 or more; filters it cannot apply are named', () => {
    const all = partnersAtDepth(pairs, 'all');
    const ei = partnersAtDepth(pairs, 'ei');
    expect(all.get('JPN')).toBe(6); // AUS 5, DEU 4, EUU 4, FRA 4, ITA 4, USA 4
    expect(ei.get('JPN')).toBe(3); // AUS 5, DEU 3, USA 4
    // a single instrument and a non-electro-industrial sector need an economy
    expect(filterPairs(pairs, f({ scope: 'all', inst: 'export_credit' }), v).unsupported).toEqual(['inst']);
    expect(filterPairs(pairs, f({ sector: 'trade' }), v).unsupported).toEqual(['sector']);
    expect(filterPairs(pairs, f({ tech: 'Green Hydrogen' }), v).rows.map((p) => `${p.a}|${p.b}`)).toContain('AUS|JPN');
    expect(countBin(0)).toBeNull();
    expect(countBin(6)?.label).toBe('5 to 9');
  });

  it('with the pair columns, the stage, instrument group, legal and implementation status filters apply: a pair passes when any of its agreements has the value', () => {
    const r = filterPairs(pairs, f({ scope: 'all', legal: 'binding', country: 'JPN' }), v);
    expect(r.unsupported).toEqual([]);
    expect(r.perPair).toEqual(['legal']);
    expect(r.rows.length).toBeGreaterThan(0);
    expect(r.rows.every((p) => (p.a === 'JPN' || p.b === 'JPN') && p.legal?.includes('binding'))).toBe(true);
    // Japan and India have only a non-binding vision statement
    expect(r.rows.some((p) => p.a === 'IND' && p.b === 'JPN')).toBe(false);
    const key = (p: GlobalPair) => `${p.a}|${p.b}`;
    // delivery: implementation underway or operational
    expect(filterPairs(pairs, f({ scope: 'all', status: 'delivery' }), v).rows.map(key).sort()).toEqual(['AUS|DEU', 'AUS|JPN']);
    expect(filterPairs(pairs, f({ scope: 'all', status: 'cancelled' }), v).rows.every((p) => p.status?.includes('cancelled'))).toBe(true);
    // the instrument group: established in an agreement of the pair
    const fin = filterPairs(pairs, f({ scope: 'all', igroup: 'finance' }), v).rows;
    expect(fin.map(key)).toContain('AUS|JPN');
    expect(fin.every((p) => p.igroups?.includes('finance'))).toBe(true);
    // the stage: covered at an established grade (critical minerals: Extraction, Refining, Recovery)
    const ref = filterPairs(pairs, f({ scope: 'all', segment: 'Refining' }), v).rows;
    expect(ref.map(key)).toContain('AUS|JPN');
    expect(ref.every((p) => p.stages?.includes('Refining'))).toBe(true);
    expect(filterPairs(pairs, f({ scope: 'all', segment: 'Assembly' }), v).rows).toEqual([]);
    // each filter on its own: several filters list all of them
    expect(filterPairs(pairs, f({ scope: 'all', legal: 'binding', status: 'signed', from: 2020 }), v).perPair).toEqual(['from', 'legal', 'status']);
  });

  it('the years: any agreement of the pair in the range (its electro-industrial ones in that scope), not only the newest', () => {
    const key = (p: GlobalPair) => `${p.a}|${p.b}`;
    // Australia and Japan's newest agreement is from 2025; their 2014 partnership agreement is in 2010-2015
    const r = filterPairs(pairs, f({ scope: 'all', from: 2010, to: 2015 }), v);
    expect(r.yearsByNewest).toBe(false);
    expect(r.rows.map(key)).toContain('AUS|JPN');
    expect(r.rows.every((p) => p.years?.some((y) => y >= 2010 && y <= 2015))).toBe(true);
    // Japan and New Zealand: one agreement, 1958; none electro-industrial
    expect(filterPairs(pairs, f({ scope: 'all', to: 1960 }), v).rows.map(key)).toContain('JPN|NZL');
    expect(filterPairs(pairs, f({ scope: 'ei', to: 1960 }), v).rows).toEqual([]);
    // in the electro-industrial scope the electro-industrial years: Australia and Japan's 1957 commerce agreement is not electro-industrial
    expect(filterPairs(pairs, f({ scope: 'ei', from: 1950, to: 1960 }), v).rows.map(key)).not.toContain('AUS|JPN');
  });

  it('an export without the pair columns: the years read the newest agreement (and the page says so), the other filters need an economy', () => {
    const g = readGlobal();
    const bare = { ...g, pairs: { ...g.pairs } } as AgreementsGlobal;
    for (const k of ['years', 'years_ei', 'igroups', 'legal', 'status', 'stages'] as const) delete bare.pairs[k];
    const old = globalPairs(bare);
    expect(old[0].years).toBeNull();
    const r = filterPairs(old, f({ scope: 'all', from: 2010, to: 2015, legal: 'binding', status: 'delivery', segment: 'Refining', igroup: 'trade' }), v);
    expect(r.yearsByNewest).toBe(true);
    expect(r.unsupported.sort()).toEqual(['igroup', 'legal', 'segment', 'status']);
    expect(r.perPair).toEqual(['from', 'to']);
    // the newest agreement of Australia and Japan is from 2025: out
    expect(r.rows.some((p) => p.a === 'AUS' && p.b === 'JPN')).toBe(false);
    // the Years filter's choices start from the oldest agreement year when the file lists them
    expect(Math.min(...globalDates(g).map((d) => Number(d)))).toBe(1957);
    expect(globalDates(bare)).toEqual(g.pairs.newest);
  });

  it('the electro-industrial scope keeps a pair with an electro-industrial agreement even at class 0', () => {
    const base = pairs.find((p) => p.a === 'JPN' && p.b === 'NZL')!;
    expect(pairInScope(base, 'all')).toBe(true);
    expect(pairInScope(base, 'ei')).toBe(false);
    expect(pairInScope({ ...base, years_ei: [2009] }, 'ei')).toBe(true);
    expect(pairInScope({ ...base, depth_ei: 2, years_ei: null }, 'ei')).toBe(true);
  });

  it('the matrix is symmetric, -1 on the diagonal, the depth class in the scope elsewhere; 0 for a pair whose agreements do not count, null for none', () => {
    const group = topEconomies(readGlobal(), 5);
    expect(group).not.toContain('EUU');
    const mx = depthMatrix(pairs, group, 'all');
    for (let i = 0; i < group.length; i++) {
      expect(mx[i][i]).toBe(-1);
      for (let j = 0; j < group.length; j++) expect(mx[i][j]).toBe(mx[j][i]);
    }
    const ix = group.indexOf('JPN');
    const ia = group.indexOf('AUS');
    if (ix >= 0 && ia >= 0) expect(mx[ix][ia]).toBe(5);
    // Japan and New Zealand: an expired agreement, class 0 (not blank); New Zealand and Norway: no agreement
    const z = depthMatrix(pairs, ['JPN', 'NOR', 'NZL'], 'all');
    expect(z[0][2]).toBe(0);
    expect(z[1][2]).toBeNull();
    expect(zeroDepthShort(1, 'all')).toBe('1 agreement, which does not count toward depth');
    expect(zeroDepthShort(3, 'ei')).toBe('3 electro-industrial agreements, none counts toward depth');
    expect(zeroDepthShort(0, 'all')).toBe('No agreement found');
    expect(orderPairs(pairs, 'all')[0].depth_all).toBe(5);
    expect(countryChoices(m).map((c) => c.iso3)).toEqual(['AUS', 'DEU', 'JPN', 'USA']);
  });
});

describe('strategic gaps and execution gaps in words, with the exact numbers', () => {
  const name = (iso: string) => m.names[iso] ?? iso;
  const depthEi = (p: string) => jpn.partners.find((x) => x.partner === p)?.depth_ei ?? null;
  const ctx = { focal: 'JPN', name, depthEi, compacts: v.compacts };
  const gap = (partner: string, tech: string) => jpn.gaps.find((g) => g.partner === partner && g.tech === tech) as AgrGap;

  it('opportunity: who could supply whom, the components (compacts by name), no agreement in these sources covers it', () => {
    expect(gapText(gap('KOR', 'Batteries'), ctx)).toBe(
      'High complementarity, weak architecture: Korea could supply Batteries to Japan (3 recommended cells, capability 97th percentile, need 81st percentile, Friendshore 0.62, shared strategic compacts: Battery Materials & Cells, SC-UNNAMED) but no agreement in these sources covers Batteries.',
    );
    // a compact the vocabulary does not name keeps its id; without the vocabulary every id is kept
    expect(compactName('SC-BATTERIES', v.compacts)).toBe('Battery Materials & Cells');
    expect(compactName('SC-UNNAMED', v.compacts)).toBe('SC-UNNAMED');
    expect(gapComponents(gap('KOR', 'Batteries'))).toMatch(/shared strategic compacts: SC-BATTERIES, SC-UNNAMED$/);
    expect(gapText(gap('IND', 'Electric Grid'), ctx)).toMatch(/^High complementarity, weak architecture: Japan could supply Electric Grid to India \(1 recommended cell, capability 90th percentile, need 85th percentile\)/);
  });

  it('established (an agreement covers it), a mention only (weak architecture) and legacy', () => {
    expect(gapText(gap('AUS', 'Batteries'), ctx)).toMatch(/strong architecture: Australia could supply Batteries to Japan .* and an agreement covers Batteries\.$/);
    expect(gapText(gap('USA', 'Nuclear'), ctx)).toMatch(/^High complementarity, weak architecture: .*shared strategic compacts: Nuclear Fuel & Reactor Systems\) but Nuclear is only mentioned in the text of an agreement, not established\.$/);
    expect(gapText(gap('USA', 'Oil'), ctx)).toBe(
      'Low complementarity, strong architecture: an agreement of Japan and United States covers Oil; but United States is not a recommended supplier of Oil to Japan, and need and capability are not both high at the same step (0 recommended cells, capability 95th percentile, need 30th percentile, Friendshore 0.55).',
    );
    // strong architecture without coverage (a pair-depth clause, strong_depth): the absence is in these sources
    const viaDepth: AgrGap = { ...gap('USA', 'Oil'), covered_grade: 'none' };
    expect(gapText(viaDepth, ctx)).toMatch(/^Low complementarity, strong architecture: the electro-industrial partnership of Japan and United States is class 4, though no agreement in these sources covers Oil;/);
  });

  it('complementarity not assessed (need or capability data missing) is never read as low', () => {
    const na = gap('NZL', 'Wind');
    expect(na).toMatchObject({ complementarity: 'not_assessed', quadrant: 'none', capability: null });
    expect(complementarityNotAssessed(na)).toBe(true);
    const text = gapText(na, ctx);
    expect(text).toBe(
      'Complementarity not assessed, weak architecture: New Zealand as a supplier of Wind to Japan could not be assessed, need or capability data missing (0 recommended cells, capability not assessed, need 70th percentile); no agreement in these sources covers Wind.',
    );
    expect(text).not.toMatch(/not a recommended supplier|not both high/);
    expect(complementarityWord(na)).toBe('could not be assessed (need or capability data missing)');
    // an older export: a legacy row with the need missing, flagged low
    const legacy: AgrGap = { ...gap('USA', 'Oil'), need: null };
    expect(complementarityNotAssessed(legacy)).toBe(true);
    expect(gapText(legacy, ctx)).toMatch(/^Complementarity not assessed, strong architecture: United States as a supplier of Oil to Japan could not be assessed, need or capability data missing \(.*\); an agreement covers Oil\.$/);
    expect(gapText(legacy, ctx)).not.toMatch(/not a recommended supplier/);
    expect(complementarityWord(gap('USA', 'Oil'))).toBe('low');
    // the not-assessed rows are in quadrant none: not a gap group
    expect(gapGroups(jpn.gaps).flatMap((g) => g.rows).some((r) => r.complementarity === 'not_assessed')).toBe(false);
  });

  it('the rule that set a depth class, in words: instruments with their basis, sectors with their grade, types and groups by label, ids as titles', () => {
    const ix = new Map(jpn.agreements.map((a) => [a.id, a]));
    const p = (iso: string) => jpn.partners.find((x) => x.partner === iso)!;
    const t = (id: string) => `“${ix.get(id)!.title}”`;
    const deuAll = depthRuleWords(p('DEU').depth_all_rule, v, ix)!;
    expect(deuAll).not.toMatch(/agr-[0-9a-f]{10}|\[|Operational partnership \(all agreements\)/);
    expect(deuAll).toMatch(/^Tariff reduction \(coded by the source\), /);
    const deuEi = depthRuleWords(p('DEU').depth_ei_rule, v, ix)!;
    expect(deuEi).toMatch(/^Critical minerals \(mentioned in text\), Hydrogen \(mentioned in text\) in “/);
    const aus = depthRuleWords(p('AUS').depth_all_rule, v, ix)!;
    expect(aus).toMatch(/^trade instrument in “[^”]+” \(2014\); finance, investment or manufacturing \(Finance, Supply security\) in “[^”]+” \(\d{4}\); sectoral cooperation: Steel \(established\), Hydrogen \(established\) in “[^”]+” \(\d{4}\); in effect: “[^”]+” \(\d{4}\)$/);
    expect(depthRuleWords(p('IND').depth_all_rule, v, ix)).toBe(`${labelOf(v, 'agreement_types', 'strategic_partnership')} in ${t('agr-0b1c2d3e4f')} (${ix.get('agr-0b1c2d3e4f')!.date!.slice(0, 4)})`);
    expect(depthRuleWords(p('NZL').depth_all_rule, v, ix)).toBeNull();
    // an id the file does not hold, and a rule in another wording
    expect(depthRuleWords('2 Framework (all agreements): mou in agr-ffffffffff', v, ix)).toBe('Memorandum of understanding (MOU) in an agreement');
    expect(depthRuleWords('1 Dialogue (electro-industrial agreements): only plurilaterals above max_parties_for_depth in agr-ffffffffff', v)).toBe('only plurilateral agreements with too many parties to set a deeper class in an agreement');
    expect(depthRuleWords('hand-written rule', v)).toBe('hand-written rule');
    // class 0 in words: none found in the scope, or agreements that no longer count
    expect(zeroDepthWords(0, 'ei')).toBe('No electro-industrial agreement of the pair is found in these sources.');
    expect(zeroDepthWords(1, 'all')).toMatch(/^The pair's one agreement does not count toward depth: it is expired/);
    expect(zeroDepthWords(3, 'all')).toMatch(/^None of the pair's 3 agreements counts toward depth: each is expired/);
  });

  it("the global file's electro-industrial sectors: plain ids (section 9) or R's id:grade", () => {
    expect(gradedSectors(['batteries:specific', 'hydrogen:text', 'hydrogen:specific', 'solar'])).toEqual({ ids: ['batteries', 'hydrogen', 'solar'], grades: { batteries: 'specific', hydrogen: 'specific' } });
    const g = readGlobal();
    const graded = { ...g, pairs: { ...g.pairs, ei_sectors: g.pairs.ei_sectors.map((x) => (x ? x.split('|').map((s) => `${s}:text`).join('|') : x)) } };
    const plain = globalPairs(g);
    const rows = globalPairs(graded);
    expect(rows.map((r) => r.ei_sectors)).toEqual(plain.map((r) => r.ei_sectors));
    expect(plain.every((r) => Object.keys(r.ei_grades).length === 0)).toBe(true);
    expect(rows.find((r) => r.ei_sectors.length)!.ei_grades[rows.find((r) => r.ei_sectors.length)!.ei_sectors[0]]).toBe('text');
  });

  it('groups by quadrant in the order opportunity, established, legacy; the low-low rows are left out', () => {
    const g = gapGroups(jpn.gaps);
    expect(g.map((x) => x.quadrant)).toEqual(['opportunity', 'established', 'legacy']);
    expect(g[0].rows[0].partner).toBe('KOR'); // most recommended cells first
    expect(gapGroups(jpn.gaps, 'AUS').map((x) => x.quadrant)).toEqual(['established']);
  });

  it('execution gaps name the basis, the priority and the suppliers without coverage', () => {
    expect(executionGapText(jpn.execution_gaps[0], ctx)).toBe(
      'Solar at Components and Assembly is a priority for Japan (policy priority, priority 86th percentile within Japan), but none of its agreements in these sources with 2 capable, aligned suppliers (India and Korea) covers it at an established level.',
    );
    expect(executionGapText(jpn.execution_gaps[1], ctx)).toBe('Electric Grid at Components is a priority for Japan (strategic choice Secure), but no agreement in these sources with a capable supplier covers it at an established level.');
    expect(priorityBasisWords('policy_priority|partner')).toBe('policy priority; strategic choice Partner');
    expect(priorityBasisWords('policy_priority|choice_secure')).toBe('policy priority; strategic choice Secure');
    expect(executionGapText({ ...jpn.execution_gaps[1], stages: ['Components', 'Assembly'] }, ctx)).toMatch(/^Electric Grid at Components and Assembly is a priority for Japan/);
  });

  it('coverage is read stage by stage, and a match across opposite UN-vote blocs is flagged, not counted', () => {
    const base = gap('KOR', 'Batteries');
    // complementary at Assembly; a minerals agreement covers only the upstream stages
    const staged: AgrGap = { ...base, alignment: 1, stages_high: ['Assembly'], stages_covered: ['Extraction', 'Refining', 'Recovery'] };
    expect(gapText(staged, ctx)).toMatch(/same UN-vote bloc, Friendshore 0\.62, shared strategic compacts: Battery Materials & Cells, SC-UNNAMED, complementary at Assembly\) but no agreement in these sources covers Batteries at Assembly \(agreements cover Batteries only at Extraction, Refining and Recovery\)\.$/);
    const covered: AgrGap = { ...staged, covered_grade: 'specific', architecture: 'strong', quadrant: 'established', stages_covered: ['Assembly'] };
    expect(gapText(covered, ctx)).toMatch(/and an agreement covers Batteries \(at Assembly\)\.$/);
    const blocked: AgrGap = { ...base, alignment: 0, alignment_blocked: true, complementarity: 'low', covered_grade: 'specific', architecture: 'strong', quadrant: 'legacy' };
    expect(gapText(blocked, ctx)).toMatch(/Korea could supply Batteries to Japan, but the two are in opposite UN-vote blocs, so the match does not count as complementarity \(.*opposite UN-vote blocs/);
    expect(alignmentWords(0.5)).toBe('one side non-aligned in UN votes');
    // the electro-industrial scope keeps the electro-industrial technologies only
    const rows = [{ tech: 'Batteries' }, { tech: 'Oil' }, { tech: 'Nuclear' }, { tech: 'Coal' }];
    expect(gapsInScope(rows, 'ei', v).map((r) => r.tech)).toEqual(['Batteries', 'Nuclear']);
    expect(gapsInScope(rows, 'all', v)).toHaveLength(4);
    // a long supplier list is cut after EXEC_NAMES_SHOWN names
    const many = { ...jpn.execution_gaps[0], suppliers_without_coverage: ['USA', 'KOR', 'IND', 'AUS', 'DEU', 'FRA', 'GBR', 'CAN'] };
    expect(executionGapText(many, ctx)).toMatch(/with 8 capable, aligned suppliers \([^)]*, [^)]* and 2 others\) covers it/);
    expect(alignmentWords(null)).toBeNull();
  });
});

describe('the pair', () => {
  it('architecture: strategic partnership, trade agreement with its trade depth, sectoral cooperation, sectors established first', () => {
    const p = jpn.partners.find((x) => x.partner === 'AUS');
    const a = architectureOf(p!);
    expect(a).toMatchObject({ strategic: true, trade: true, tradeDepth: 6, sectoral: true });
    expect(a.sectors[0].grade).toBe('specific');
    const d = deu.partners.find((x) => x.partner === 'JPN');
    expect(d?.n_via_eu).toBe(2);
    expect(aus.partners.find((x) => x.partner === 'USA')?.depth_all).toBe(5);
  });
});

describe('pairs whose depth rests on EU agreements', () => {
  it('an EU-level agreement reaches a pair through the EU from either side', () => {
    const a = { id: 'agr-0000000001', parties: ['EUU', 'JPN'], via_eu: false } as unknown as AgrAgreement;
    expect(viaEuForPair(a, 'POL')).toBe(true); // Japan's own agreement with the EU, Poland a member
    expect(viaEuForPair(a, 'EUU')).toBe(false); // with the EU itself it is direct
    expect(viaEuForPair({ ...a, parties: ['CHL', 'EUU'], via_eu: true } as AgrAgreement, 'CHL')).toBe(true); // counted for the focal member
    const ix = { byId: new Map([[a.id, a]]), partnersOf: new Map(), partner: new Map() };
    expect(depthRestsOnEu('4 Operational partnership (electro-industrial agreements): rd_cooperation [curated] in agr-0000000001', ix, 'POL')).toBe(true);
    expect(depthRestsOnEu('4 Operational partnership (electro-industrial agreements): rd_cooperation [curated] in agr-0000000001', ix, 'EUU')).toBe(false);
    expect(depthRestsOnEu(null, ix, 'POL')).toBe(false);
  });
});

describe('class 0 with agreements is not "none found"', () => {
  it('the partner map tooltip: agreements that do not count, or nothing found (with the absence caveat)', () => {
    const o = { scope: 'all' as const, scopeWords: 'all partnerships', filtered: false, v };
    expect(partnerTipWords({ ...o, depth: 4, nInScope: 3 })).toEqual({ words: 'Class 4 · Operational partnership (all partnerships)', caveat: false });
    expect(partnerTipWords({ ...o, depth: 0, nInScope: 1 })).toEqual({ words: 'Class 0 · 1 agreement, which does not count toward depth (all partnerships)', caveat: false });
    expect(partnerTipWords({ ...o, depth: 0, nInScope: 4, scope: 'ei', scopeWords: 'electro-industrial partnerships' })).toEqual({
      words: 'Class 0 · 4 electro-industrial agreements, none counts toward depth (electro-industrial partnerships)',
      caveat: false,
    });
    expect(partnerTipWords({ ...o, depth: null, nInScope: 0 })).toEqual({ words: 'No agreement found (all partnerships)', caveat: true });
    expect(partnerTipWords({ ...o, depth: null, nInScope: 0, filtered: true })).toEqual({ words: 'No agreement that matches the filters (all partnerships)', caveat: true });
  });
});

describe('the gap visuals: matrix model and stage states', () => {
  const g = (partner: string, tech: string, quadrant: AgrGap['quadrant'], side: AgrGap['side'] = 'needs', extra: Partial<AgrGap> = {}): AgrGap => ({
    partner, tech, side, need: 0.8, capability: 0.9, recommended: 1, friendshore: null, aib_shared: [], complementarity: quadrant === 'legacy' ? 'low' : 'high',
    covered_grade: quadrant === 'opportunity' ? 'none' : 'specific', architecture: quadrant === 'opportunity' ? 'weak' : 'strong', quadrant, ...extra,
  });
  it('rows: most opportunities, then established cells, then the strongest counterpart; columns in the dashboard order; low-low rows left out', () => {
    const rows = [
      g('AAA', 'Wind', 'established'), g('BBB', 'Batteries', 'opportunity'), g('BBB', 'Solar', 'opportunity'), g('CCC', 'Solar', 'opportunity', 'needs', { capability: 0.99 }),
      g('DDD', 'Solar', 'opportunity', 'needs', { capability: 0.5 }), g('EEE', 'Oil', 'none'), g('FFF', 'Solar', 'opportunity', 'supplies'),
    ];
    const m = gapMatrix(rows, 'needs', {});
    expect(m.rows.map((r) => r.partner)).toEqual(['BBB', 'CCC', 'DDD', 'AAA']);
    expect(m.techs).toEqual(['Solar', 'Wind', 'Batteries']);
    expect(m.counts).toEqual({ opportunity: 4, established: 1, legacy: 0 });
    expect(gapMatrix(rows, 'supplies', {}).rows.map((r) => r.partner)).toEqual(['FFF']);
  });
  it('stage states: complementary and uncovered is a gap; covered where complementary, or covered only', () => {
    const st = stageStates(g('AAA', 'Batteries', 'established', 'needs', { stages_high: ['Extraction', 'Assembly'], stages_covered: ['Extraction', 'Refining', 'Recovery'] }));
    expect(st).toEqual({ Extraction: 'covered', Refining: 'covered_only', Components: 'none', Assembly: 'gap', Recovery: 'covered_only' });
    expect(Object.values(stageStates(g('AAA', 'Batteries', 'opportunity'))).every((s) => s === 'none')).toBe(true);
  });
});
