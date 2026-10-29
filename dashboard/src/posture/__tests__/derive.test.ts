// The posture bundle contract and the derivations of the posture cards (src/posture/contract.ts, derive.ts):
// the takeaways, the counts of a page's tile, the checked ?pnode node, and the rows of the table twins and
// CSVs with their provenance (source, as_of, git_commit last).

import { describe, expect, it } from 'vitest';
import { STAGE_SHORT as LIB_STAGE_SHORT, SUPPLY_STAGES } from '../../lib/stages.ts';
import { checkPostureDetail, decodeCore, POSTURE_STAGES, PostureContractError } from '../contract.ts';
import {
  aibHeader, aibHeaderTiles, aibTakeaway, cellKeyParts, countPostures, countryCells, countryNames, countryPostureRows, countryTakeaway, DIVISION_CSV_COLS, divisionCsvRows,
  divisionRows, freshnessLine, parsePostureNode, POSTURE_CSV_COLS, postureCountsText, postureCsvRows, postureCsvText, postureProvenance, producingStagesOf, sectorPostureRows,
  sectorTakeaway,
} from '../derive.ts';
import { MODE_MARK, POSTURE_COLOR, POSTURE_LABEL, STAGE_SHORT } from '../grammar.ts';
import { postureMomentum } from '../momentum.ts';
import { fixtureCore, fixtureDetail, fixtureStore, renameEconomies, withoutCell } from './fixture.ts';

describe('the posture contract', () => {
  it('decodes the columnar core into six-stage cells', () => {
    const core = decodeCore(fixtureCore());
    expect(core.cells).toHaveLength(36);
    expect(new Set(core.cells.map((c) => c.stage)).size).toBe(6);
    const s = core.cells.find((c) => c.iso3 === 'AAA' && c.tech === 'Batteries' && c.stage === 'Refining');
    expect(s?.posture).toBe('S');
    expect(s?.rationale).toBe('coercive_chokepoint');
    expect(core.division[1].gap).toBe('single_point_of_failure');
    expect(core.division[0].backups.map((b) => b.iso3)).toEqual(['AAA']);
  });
  it('refuses three-stage data, an unknown posture and a newer schema', () => {
    const three = fixtureCore();
    (three.meta as { stages: string[] }).stages = ['Upstream', 'Midstream', 'Downstream'];
    expect(() => decodeCore(three)).toThrow(PostureContractError);
    const bad = fixtureCore();
    (bad.cells as { p: string[] }).p[0] = 'X';
    expect(() => decodeCore(bad)).toThrow(/unknown posture/);
    const newer = { ...fixtureCore(), schema_version: 9 };
    expect(() => decodeCore(newer)).toThrow(/newer app/);
    const mode = fixtureCore();
    (mode.cells as { m: (string | null)[] }).m[0] = 'watch';
    expect(() => decodeCore(mode)).toThrow(/mode/);
  });
  it('checks the detail document', () => {
    expect(checkPostureDetail(fixtureDetail()).cells).toHaveLength(3);
    const bad = fixtureDetail();
    (bad.cells as { stage: string }[])[0].stage = 'Midstream';
    expect(() => checkPostureDetail(bad)).toThrow(PostureContractError);
  });
  it('the store fetches, checks and caches', async () => {
    const st = fixtureStore();
    const a = await st.core();
    expect(await st.core()).toBe(a);
    expect(st.peek('core')).toBe(a);
    expect((await st.detail('AAA')).country).toBe('Aland');
    await expect(st.detail('ZZZ')).rejects.toThrow(/No posture detail/);
  });
});

describe('the headline derivations', () => {
  const core = decodeCore(fixtureCore());
  it('counts postures and modes, Watch only among Deploy', () => {
    const n = countPostures(countryCells(core, 'AAA'));
    expect([n.S, n.C, n.D]).toEqual([1, 2, 9]);
    expect([n.partner, n.preserve, n.catch_up]).toEqual([1, 1, 0]);
    expect(n.watch).toBe(9);
  });
  it('the country takeaway names the Secure rationale and the Compete mix', () => {
    const t = countryTakeaway('Aland', countryCells(core, 'AAA'));
    expect(t).toMatch(/^Aland should secure 1 node \(Batteries refining; coercive chokepoint\)/);
    expect(t).toMatch(/compete in 2 nodes \(1 partner, 1 preserve/);
    expect(t).toMatch(/deploy openly in the other 9\.$/);
    const none = countryTakeaway('Bland', countryCells(core, 'BBB').filter((c) => c.posture === 'D'));
    expect(none).toMatch(/deploy openly in all/);
  });
  it('the sector takeaway counts each quadrant in the node', () => {
    const cells = core.cells.filter((c) => c.tech === 'Batteries' && c.stage === 'Components');
    const t = sectorTakeaway('Batteries', 'Components', cells, { AAA: 'Aland', BBB: 'Bland', CCC: 'Cland' });
    expect(t).toContain('1 have the need and opportunity to catch up');
    expect(t).toContain('2 need it without the opportunity, and should partner');
  });
  it('the sector takeaway names the preserve countries as the group when there are two at most, else as its leaders', () => {
    const base = core.cells[0];
    const preserve = (iso3: string, opp: number) => ({ ...base, iso3, tech: 'Batteries', stage: 'Refining' as const, posture: 'C' as const, mode: 'preserve' as const, opp });
    const names = { AAA: 'Aland', BBB: 'Bland', CCC: 'Cland' };
    const many = sectorTakeaway('Batteries', 'Refining', [preserve('AAA', 0.2), preserve('BBB', 0.9), preserve('CCC', 0.5)], names);
    expect(many).toBe('In Batteries refining, 3 hold an advantage to preserve (led by Bland and Cland).');
    const two = sectorTakeaway('Batteries', 'Refining', [preserve('AAA', 0.2), preserve('BBB', 0.9)], names);
    expect(two).toBe('In Batteries refining, 2 hold an advantage to preserve (Bland and Aland).');
  });
  it('the AIB takeaway leads with the worst gap among nodes members rely on', () => {
    expect(aibTakeaway(core.division, null)).toMatch(/3 of the 3 nodes .* have a gap; the most pressing is Solar assembly \(no trusted supplier\)/);
    expect(aibTakeaway(core.division, 'Geothermal')).toMatch(/^No country's posture asks/);
    expect(aibHeader(core)).toEqual({ coreMembers: 2, coveredShare: 0.333, unresolvedGaps: 3, priorityNodes: 3 });
  });
  it('the freshness line states the benchmark and the signal, or why there is none', () => {
    expect(freshnessLine(core.meta)).toEqual({ benchmark: 'Structural trade benchmark: 2022–24', signal: 'Current signal through: Aug 2026' });
    const none = decodeCore(fixtureCore(false));
    expect(freshnessLine(none.meta).signal).toBe('Current momentum unavailable — incomplete matched-period reporting');
  });
  it('momentum direction follows the exporter rule', () => {
    expect([postureMomentum(0.2), postureMomentum(0.05), postureMomentum(-0.3), postureMomentum(null)]).toEqual(['up', 'flat', 'down', null]);
  });
  it('one grammar: three postures, three modes', () => {
    expect(Object.keys(POSTURE_COLOR)).toEqual(['S', 'C', 'D']);
    expect(Object.values(POSTURE_LABEL)).toEqual(['Secure', 'Compete', 'Deploy']);
    expect(Object.keys(MODE_MARK).sort()).toEqual(['catch_up', 'partner', 'preserve']);
  });
});

describe('the shared stages and the provenance of the core', () => {
  it('the posture stages are the shared six; the stage shorts are the shared map', () => {
    expect(POSTURE_STAGES).toBe(SUPPLY_STAGES);
    expect(STAGE_SHORT).toBe(LIB_STAGE_SHORT);
    expect(producingStagesOf(decodeCore(fixtureCore()).meta.stages)).toEqual(['Extraction', 'Refining', 'Components', 'Assembly', 'Recovery']);
  });
  it('carries the core created and git_commit, null when absent', () => {
    const core = decodeCore(fixtureCore());
    expect([core.created, core.gitCommit]).toEqual(['2026-10-28T00:00:00Z', 'abc1234']);
    expect(postureProvenance(core)).toEqual({ source: 'Opportunity Security Indices, strategic position framework', as_of: '2026-10-28', git_commit: 'abc1234' });
    const bare = fixtureCore();
    delete bare.created;
    delete bare.git_commit;
    const none = decodeCore(bare);
    expect([none.created, none.gitCommit]).toEqual([null, null]);
    expect(postureProvenance(none, 'x')).toEqual({ source: 'x', as_of: null, git_commit: null });
    expect(postureProvenance({ created: 'last week', gitCommit: null }).as_of).toBe('last week');
  });
  it('renames the fixture economies for a page test, codes and names', () => {
    const core = decodeCore(renameEconomies(fixtureCore(), { AAA: { iso3: 'USA', name: 'United States' } }));
    expect(core.countries.map((c) => c.iso3)).toEqual(['USA', 'BBB', 'CCC']);
    expect(countryNames(core)).toEqual({ USA: 'United States', BBB: 'Bland', CCC: 'Cland' });
    expect(core.division[0].backups.map((b) => b.iso3)).toEqual(['USA']);
  });
});

describe('the counts of a page tile and the AIB header tiles', () => {
  const core = decodeCore(fixtureCore());
  it('words the Secure / Compete / Deploy counts and the Compete mix', () => {
    expect(postureCountsText(countPostures(countryCells(core, 'AAA')))).toEqual({ value: 'Secure 1 · Compete 2 · Deploy 9', sub: 'Compete: 1 preserve · 0 catch up · 1 partner' });
  });
  it('the three AIB metrics as tiles', () => {
    expect(aibHeaderTiles(core)).toEqual([
      { id: 'core', label: 'Core coalition members', value: '2', sub: '3 members in all' },
      { id: 'covered', label: 'Priority nodes with credible trusted coverage', value: '33%', sub: 'of 3 nodes members ask partners to supply or secure' },
      { id: 'gaps', label: 'Unresolved gaps', value: '3', sub: 'priority nodes with a gap' },
    ]);
  });
});

describe('the node of the Sector card (?pnode)', () => {
  const meta = decodeCore(fixtureCore()).meta;
  it('no value: the default node, Batteries — Refining', () => {
    expect(parsePostureNode(null, meta)).toEqual({ tech: 'Batteries', stage: 'Refining', problem: null });
    expect(parsePostureNode('', meta).problem).toBeNull();
  });
  it('a valid node is used as written', () => {
    expect(parsePostureNode('Solar|Components', meta)).toEqual({ tech: 'Solar', stage: 'Components', problem: null });
  });
  it('a three-stage stage is reported with the stages that replaced it, never used', () => {
    expect(parsePostureNode('Solar|Midstream', meta)).toEqual({
      tech: 'Solar', stage: 'Refining', problem: 'pnode=Solar|Midstream (Midstream is no longer a stage: it is now mostly Components and Assembly; showing Solar — Refining)',
    });
    expect(parsePostureNode('Batteries|Upstream', meta).problem).toBe('pnode=Batteries|Upstream (Upstream is no longer a stage: it is now Extraction, Refining and Recovery; showing Batteries — Refining)');
    // Downstream is exactly Deployment: used where Deployment is offered, reported where it is left out
    expect(parsePostureNode('Solar|Downstream', meta).problem).toBe('pnode=Solar|Downstream (Downstream is now Deployment, which this chart leaves out; showing Solar — Refining)');
    expect(parsePostureNode('Solar|Downstream', meta, meta.stages)).toEqual({ tech: 'Solar', stage: 'Deployment', problem: 'pnode=Solar|Downstream (Downstream is now Deployment; showing Solar — Deployment)' });
  });
  it('Deployment, an unknown technology or stage and a malformed value fall back and say why', () => {
    expect(parsePostureNode('Solar|Deployment', meta)).toEqual({ tech: 'Solar', stage: 'Refining', problem: 'pnode=Solar|Deployment (Deployment is left out here: every country deploys openly there; showing Solar — Refining)' });
    expect(parsePostureNode('Solar|Deployment', meta, meta.stages).problem).toBeNull();
    expect(parsePostureNode('Wind|Assembly', meta).problem).toBe('pnode=Wind|Assembly (Wind is not a technology of the posture data; showing Batteries — Refining)');
    expect(parsePostureNode('Solar|refining', meta).problem).toBe('pnode=Solar|refining (refining is not a supply-chain stage; showing Solar — Refining)');
    expect(parsePostureNode('Solar', meta).problem).toBe('pnode=Solar (not a technology|stage pair; showing Batteries — Refining)');
    expect(parsePostureNode('Batteries|Refining', { techs: ['Solar'], stages: ['Extraction', 'Components'] }, ['Components'])).toEqual({
      tech: 'Solar', stage: 'Components', problem: 'pnode=Batteries|Refining (Batteries is not a technology of the posture data; showing Solar — Components)',
    });
  });
  it('checks a cell key against the data', () => {
    expect(cellKeyParts('Batteries|Deployment', meta)).toEqual({ tech: 'Batteries', stage: 'Deployment' });
    for (const bad of ['Batteries|Midstream', 'Wind|Refining', 'Batteries', '', null, undefined]) expect(cellKeyParts(bad, meta), String(bad)).toBeNull();
  });
});

describe('the table twins and CSVs', () => {
  // Aland's Solar Recovery node is not classified: kept as a row, never dropped
  const core = decodeCore(withoutCell(fixtureCore(), 0, 1, 5));
  const prov = postureProvenance(core);
  const source = '"Opportunity Security Indices, strategic position framework"';

  it("one economy's rows: the full technology x six-stage grid, in chain order, unclassified nodes kept", () => {
    const rows = countryPostureRows(core, 'AAA');
    expect(rows).toHaveLength(12);
    expect(rows.slice(0, 6).map((r) => r.key)).toEqual(['Batteries|Extraction', 'Batteries|Refining', 'Batteries|Components', 'Batteries|Assembly', 'Batteries|Deployment', 'Batteries|Recovery']);
    const sec = rows[1];
    expect([sec.postureWord, sec.rationale, sec.mode, sec.quadrant, sec.alignment, sec.need, sec.tier]).toEqual(['Secure', 'Coercive chokepoint', null, 'Partner', 'Under-prioritized', 0.95, 'core']);
    const part = rows[2];
    expect([part.postureWord, part.mode, part.partner1Name, part.momentum, part.flag, part.plotted]).toEqual(['Compete', 'Partner', 'Bland', 'rising', 'partner despite capability', true]);
    const none = rows[11];
    expect([none.key, none.classified, none.posture, none.postureWord, none.need, none.plotted]).toEqual(['Solar|Recovery', false, null, null, null, false]);
  });

  it("one node's rows: Secure first, then by Industrial Opportunity, unmeasured last, then by name", () => {
    expect(sectorPostureRows(core, 'Batteries', 'Components').map((r) => r.iso3)).toEqual(['BBB', 'AAA', 'CCC']);
    expect(sectorPostureRows(core, 'Batteries', 'Refining').map((r) => [r.iso3, r.postureWord])).toEqual([['AAA', 'Secure'], ['BBB', 'Deploy'], ['CCC', 'Deploy']]);
    // Cland's Solar Refining has no measured opportunity: last, and not plotted
    const solar = sectorPostureRows(core, 'Solar', 'Refining');
    expect(solar.map((r) => r.iso3)).toEqual(['AAA', 'BBB', 'CCC']);
    expect(solar[2].plotted).toBe(false);
    expect(sectorPostureRows(core, 'Solar', 'Recovery').map((r) => r.iso3)).toEqual(['BBB', 'CCC']);
  });

  it('the posture CSV: plain-word columns, fractions, logicals as R writes them, provenance last', () => {
    const text = postureCsvText(postureCsvRows(countryPostureRows(core, 'AAA')), POSTURE_CSV_COLS, prov);
    const lines = text.split('\r\n');
    expect(lines[0]).toBe([...POSTURE_CSV_COLS, 'source', 'as_of', 'git_commit'].join(','));
    expect(lines).toHaveLength(14); // header, 12 nodes, the final line break
    expect(lines[2]).toBe(`Aland,AAA,core,Batteries,Refining,TRUE,Secure,,Coercive chokepoint,Partner,0.95,0.4,0.3,,Under-prioritized,high,,,,,,,${source},2026-10-28,abc1234`);
    expect(lines[3]).toBe(`Aland,AAA,core,Batteries,Components,TRUE,Compete,Partner,,Partner,0.8,0.3,0.9,,Aligned,high,rising,Bland,BBB,FALSE,,partner despite capability,${source},2026-10-28,abc1234`);
    expect(lines[12]).toBe(['Aland', 'AAA', 'core', 'Solar', 'Recovery', 'FALSE', ...Array(16).fill(''), source, '2026-10-28', 'abc1234'].join(','));
    // an empty table still names its columns
    expect(postureCsvText([], POSTURE_CSV_COLS, prov).split('\r\n')[0]).toBe(lines[0]);
  });

  it('the division rows and CSV: one row per node, names and gaps in words', () => {
    const rows = divisionRows(core, null);
    expect(rows.map((r) => r.key)).toEqual(['Batteries|Refining', 'Batteries|Components', 'Solar|Assembly']);
    expect([rows[0].primaryName, rows[0].backups, rows[0].gapLabel, rows[0].otherGaps, rows[0].capabilitySource, rows[0].aibNodes]).toEqual([
      'Bland', 'Aland (10%)', 'Insufficient redundancy', 'Weak evidence', 'Allied Industrial Base positions', 2,
    ]);
    expect([rows[2].primaryName, rows[2].backups, rows[2].gapLabel, rows[2].leaderName, rows[2].capabilitySource]).toEqual([null, null, 'No trusted supplier', 'XXX', 'World supply shares']);
    expect(divisionRows(core, 'Solar').map((r) => r.key)).toEqual(['Solar|Assembly']);
    const lines = postureCsvText(divisionCsvRows(rows), DIVISION_CSV_COLS, postureProvenance(core, 'division source')).split('\r\n');
    expect(lines[0]).toBe([...DIVISION_CSV_COLS, 'source', 'as_of', 'git_commit'].join(','));
    expect(lines[1]).toBe('Batteries,Refining,Bland,BBB,0.3,Aland (10%),AAA,2,0.4,0.5,XXX,XXX,0.5,Insufficient redundancy,Weak evidence,0,1,0,0,Allied Industrial Base positions,2,division source,2026-10-28,abc1234');
    expect(lines).toHaveLength(5);
  });

  it('the sector takeaway at Deployment says every country deploys', () => {
    const d = core.cells.filter((c) => c.tech === 'Batteries' && c.stage === 'Deployment');
    expect(sectorTakeaway('Batteries', 'Deployment', d, countryNames(core))).toBe(
      'In Batteries deployment, every country should deploy openly: deployment is where the technology is used, and its supply risks are read at the stages that make or supply it.',
    );
  });
});
