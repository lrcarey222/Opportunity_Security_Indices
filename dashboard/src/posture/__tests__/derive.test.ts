// The derivations of the posture cards (src/posture/derive.ts) and the site-wide grammar (grammar.ts), on
// the schema-2 fixture: the counts (six verbs primary, three families, not assessed), the Sector and AIB
// takeaways, the counts of a page's tile, the checked ?pnode node, and the rows of the table twins and CSVs
// with their provenance (source, as_of, git_commit last). The contract itself is in contract.test.ts.

import { describe, expect, it } from 'vitest';
import { STAGE_DISPLAY as LIB_STAGE_DISPLAY, STAGE_SHORT as LIB_STAGE_SHORT, SUPPLY_STAGES } from '../../lib/stages.ts';
import { ALIGNMENTS, decodeCore, POSTURE_STAGES } from '../contract.ts';
import {
  aibHeader, aibHeaderTiles, aibTakeaway, cellKeyParts, countPostures, countryCells, countryNames, countryPostureRows, DIVISION_CSV_COLS, divisionCsvRows, divisionRows,
  freshnessLine, parsePostureNode, POSTURE_CSV_COLS, postureCountsText, postureCsvRows, postureCsvText, postureProvenance, producingStagesOf, score100, sectorPostureRows,
  sectorTakeaway, strategyCountsText,
} from '../derive.ts';
import {
  ALIGNMENT_CLASS_LABEL, ALIGNMENT_LABEL, alignmentClass, alignmentFlagWords, borderlineWords, cellMark, choiceWord, DEPLOY_MARK, EVIDENCE_LABEL, EVIDENCE_LEVELS, evidenceOpacity,
  flagWords, MODE_MARK, pathwayEvidenceWords, POSTURE_COLOR, POSTURE_LABEL, RATIONALE_MARK, STAGE_DISPLAY, STAGE_SHORT, traceCodeWords, traceTestWords, VERB_FAMILY, VERB_LABEL,
  VERB_ORDER, verbOf,
} from '../grammar.ts';
import { postureMomentum } from '../momentum.ts';
import { fixtureCore, renameEconomies, withoutCell } from './fixture.ts';

describe('one site-wide grammar (spec XX)', () => {
  it('three background families only; a node not assessed has none', () => {
    expect(Object.keys(POSTURE_COLOR)).toEqual(['S', 'C', 'D']);
    expect(Object.values(POSTURE_LABEL)).toEqual(['Secure', 'Compete', 'Deploy', 'Not assessed']);
    expect(new Set(VERB_ORDER.map((v) => VERB_FAMILY[v]))).toEqual(new Set(['S', 'C', 'D']));
  });

  it('the markers: Secure rationale, Compete mode, Deploy mode; nothing when not assessed', () => {
    expect(RATIONALE_MARK).toEqual({ operational_control: 'O', coercive_chokepoint: 'K', systemic_resilience: 'R' });
    expect(MODE_MARK).toEqual({ preserve: 'P', catch_up: 'C', partner: '⇄' });
    expect(DEPLOY_MARK).toEqual({ accelerate: '↑', watch: '•' });
    const c = { posture: 'N' as const, mode: null, deployMode: null, rationale: null };
    expect(cellMark(c)).toBe('');
    expect(cellMark({ ...c, posture: 'S', rationale: 'systemic_resilience' })).toBe('R');
    expect(cellMark({ ...c, posture: 'D', deployMode: 'accelerate' })).toBe('↑');
    expect([verbOf(c), choiceWord(c)]).toEqual([null, 'Not assessed']);
    expect(choiceWord({ ...c, posture: 'C', mode: 'catch_up' })).toBe('Compete · Catch Up');
    expect(VERB_ORDER.map((v) => VERB_LABEL[v])).toEqual(['Secure', 'Preserve', 'Catch Up', 'Partner', 'Accelerate', 'Watch']);
  });

  it('policy alignment: five classes; a deployment gap is drawn as under-prioritized and worded as an under-response', () => {
    expect(ALIGNMENTS.map(alignmentClass)).toEqual(['aligned', 'under_prioritized', 'over_prioritized', 'misaligned', 'under_prioritized', 'mixed', 'insufficient_data']);
    expect(ALIGNMENT_CLASS_LABEL.misaligned).toBe('Wrong treatment / misaligned');
    expect(ALIGNMENT_LABEL.deployment_gap).toBe('Under-prioritized: under-response (deployment)');
  });

  it('the R codes a reader meets in a drawer or tooltip are worded: consistency flags, alignment notes, borderline inputs', () => {
    // every flag the real export writes (R SP_CHOICE_FLAGS; sp_policy_alignment flags) has words, not the code
    for (const f of ['review_strong_position_low_prize', 'review_top_quarter_below_floor']) expect(flagWords(f)).not.toMatch(/_|^review /);
    for (const f of ['partner_autarkic', 'fence_without_build', 'watch_protection', 'watch_localisation_deployment', 'watch_attention', 'weak_deployment_policy', 'sources_disagree']) {
      expect(alignmentFlagWords(f)).not.toMatch(/_/);
      expect(alignmentFlagWords(f)).not.toBe(f.replace(/_/g, ' '));
    }
    expect(borderlineWords('supply_chain_need;industrial_opportunity;cp_contender_pct')).toBe('supply-chain need, Industrial Opportunity, Competitive Position');
    expect(borderlineWords('deployment_need')).toBe('deployment need');
    expect(borderlineWords(null)).toBeNull();
    // the credible-pathway evidence and the compact decision trace (R pathway.R, decision_engine.R sp_choice_trace)
    expect(pathwayEvidenceWords(['feasibility', 'adjacent_stage', 'input_cost'])).toBe('feasibility, adjacent stage, input-cost advantage');
    expect(pathwayEvidenceWords('none_observed_fires')).toBe('evidence observed, none sufficient');
    expect(pathwayEvidenceWords('feasibility;resource_base_unknown')).toBe('feasibility, resource base unknown');
    // the two-leg pathway (config pathway.min_legs): one leg fired, or the only observed leg fired
    expect(pathwayEvidenceWords('adjacent_stage;too_few_legs')).toBe('adjacent stage, too few pieces of evidence');
    expect(pathwayEvidenceWords('feasibility;too_few_observed')).toBe('feasibility, too little evidence observed to reach the bar');
    expect(['secure_override', 'deployment_need', 'supply_chain_need', 'industrial_opportunity', 'cp_contender_pct', 'pathway_credible', 'choice'].map(traceTestWords)).toEqual([
      'Secure tests', 'Deployment need', 'Supply-chain need', 'Industrial Opportunity', 'Competitive Position among contenders', 'Credible pathway', 'Choice',
    ]);
    expect(traceCodeWords('operational_control;coercive_chokepoint')).toBe('operational / control, chokepoint');
    expect(['capability_floor_not_met', 'not_a_contender', 'not_top_quarter', 'not_assessed', 'catch_up', 'high'].map(traceCodeWords)).toEqual([
      'top quarter, capability floor not met', 'top quarter, not a contender', 'not in the top quarter', 'not assessed', 'Catch Up', 'high',
    ]);
  });

  it('evidence: strong, moderate, weak as opacity, strongest solid', () => {
    expect(EVIDENCE_LEVELS.map((e) => EVIDENCE_LABEL[e])).toEqual(['Strong', 'Moderate', 'Weak']);
    expect(EVIDENCE_LEVELS.map(evidenceOpacity)).toEqual([1, 0.85, 0.6]);
  });
});

describe('the headline derivations', () => {
  const core = decodeCore(fixtureCore());
  it('counts the three families, the nodes not assessed and the six verbs', () => {
    const n = countPostures(countryCells(core, 'AAA'));
    expect([n.S, n.C, n.D, n.N]).toEqual([3, 4, 4, 1]);
    expect(VERB_ORDER.map((v) => n[v])).toEqual([3, 2, 1, 1, 1, 3]);
  });
  it('the counts line: the verbs primary, the families and not assessed secondary (spec XI.6)', () => {
    expect(strategyCountsText(countPostures(countryCells(core, 'AAA')))).toEqual({
      verbs: '3 Secure · 2 Preserve · 1 Catch Up · 1 Partner · 1 Accelerate · 3 Watch',
      families: '3 Secure · 4 Compete · 4 Deploy',
      notAssessed: '1 not assessed',
    });
    expect(strategyCountsText(countPostures(countryCells(core, 'BBB'))).notAssessed).toBeNull();
  });
  it('the sector takeaway counts each choice in the node', () => {
    const cells = core.cells.filter((c) => c.tech === 'Batteries' && c.stage === 'Components');
    const t = sectorTakeaway('Batteries', 'Components', cells, countryNames(core));
    expect(t).toBe('In Batteries components, 2 have the prize and a credible pathway to catch up and 1 need it without a domestic case, and should partner.');
  });
  it('the sector takeaway names the preserve countries as the group when there are two at most, else as its leaders', () => {
    const base = core.cells.find((c) => c.mode === 'preserve')!;
    const preserve = (iso3: string, prize: number) => ({ ...base, iso3, tech: 'Batteries', stage: 'Refining' as const, prize });
    const names = { AAA: 'Aland', BBB: 'Bland', CCC: 'Cland' };
    expect(sectorTakeaway('Batteries', 'Refining', [preserve('AAA', 0.2), preserve('BBB', 0.9), preserve('CCC', 0.5)], names)).toBe(
      'In Batteries refining, 3 hold an advantage to preserve (led by Bland and Cland).',
    );
    expect(sectorTakeaway('Batteries', 'Refining', [preserve('AAA', 0.2), preserve('BBB', 0.9)], names)).toBe('In Batteries refining, 2 hold an advantage to preserve (Bland and Aland).');
  });
  it('the sector takeaway at Deployment counts Accelerate and Watch, never Partner', () => {
    const d = core.cells.filter((c) => c.tech === 'Batteries' && c.stage === 'Deployment');
    expect(sectorTakeaway('Batteries', 'Deployment', d, countryNames(core))).toBe('In Batteries deployment, 2 should accelerate deployment and 1 need no acceleration (watch).');
  });
  it('the AIB takeaway leads with the worst gap among nodes members rely on', () => {
    expect(aibTakeaway(core.division, null)).toBe(
      '3 of the 4 nodes that countries rely on partners for across the allied base have a gap; the most pressing is Solar assembly (no trusted supplier), relied on by 1 country.',
    );
    expect(aibTakeaway(core.division, 'Geothermal')).toMatch(/^No country's posture asks/);
    expect(aibHeader(core)).toEqual({ coreMembers: 2, coveredShare: 0.25, unresolvedGaps: 3, priorityNodes: 4 });
  });
  it('the freshness line states the benchmark and the signal, or why there is none', () => {
    expect(freshnessLine(core.meta)).toEqual({ benchmark: 'Structural trade benchmark: 2022–24', signal: 'Current signal through: Aug 2026' });
    expect(freshnessLine(decodeCore(fixtureCore(false)).meta).signal).toBe('Current momentum unavailable — incomplete matched-period reporting');
  });
  it('momentum direction follows the exporter rule', () => {
    expect([postureMomentum(0.2), postureMomentum(0.05), postureMomentum(-0.3), postureMomentum(null)]).toEqual(['up', 'flat', 'down', null]);
  });
  it('a 0-1 score on the Need ranking scale', () => {
    expect([score100(0.924), score100(0), score100(null)]).toEqual(['92', '0', '—']);
  });
});

describe('the shared stages and the provenance of the core', () => {
  it('the posture stages are the shared six; the shorts and display names are the shared maps', () => {
    expect(POSTURE_STAGES).toBe(SUPPLY_STAGES);
    expect(STAGE_SHORT).toBe(LIB_STAGE_SHORT);
    expect(STAGE_DISPLAY).toBe(LIB_STAGE_DISPLAY);
    expect(producingStagesOf(decodeCore(fixtureCore()).meta.stages)).toEqual(['Extraction', 'Refining', 'Components', 'Assembly', 'Recovery']);
  });
  it('carries the core created and git_commit, null when absent', () => {
    const core = decodeCore(fixtureCore());
    expect(postureProvenance(core)).toEqual({ source: 'Opportunity Security Indices, strategic position framework', as_of: '2026-10-29', git_commit: 'abc1234' });
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
    expect(core.division[1].backups.map((b) => b.iso3)).toEqual(['USA']);
  });
});

describe('the counts of a page tile and the AIB header tiles', () => {
  const core = decodeCore(fixtureCore());
  it('words the family counts (and not assessed) with the six verbs below', () => {
    expect(postureCountsText(countPostures(countryCells(core, 'AAA')))).toEqual({
      value: 'Secure 3 · Compete 4 · Deploy 4 · 1 not assessed',
      sub: '3 Secure · 2 Preserve · 1 Catch Up · 1 Partner · 1 Accelerate · 3 Watch',
    });
  });
  it('the three AIB metrics as tiles', () => {
    expect(aibHeaderTiles(core)).toEqual([
      { id: 'core', label: 'Core coalition members', value: '2', sub: '3 members in all' },
      { id: 'covered', label: 'Priority nodes with credible trusted coverage', value: '25%', sub: 'of 4 nodes members ask partners to supply or secure' },
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
    expect(parsePostureNode('Solar|Downstream', meta).problem).toBe('pnode=Solar|Downstream (Downstream is now Deployment, which this chart leaves out; showing Solar — Refining)');
    expect(parsePostureNode('Solar|Downstream', meta, meta.stages)).toEqual({ tech: 'Solar', stage: 'Deployment', problem: 'pnode=Solar|Downstream (Downstream is now Deployment; showing Solar — Deployment)' });
  });
  it('Deployment, an unknown technology or stage and a malformed value fall back and say why', () => {
    expect(parsePostureNode('Solar|Deployment', meta)).toEqual({
      tech: 'Solar', stage: 'Refining',
      problem: 'pnode=Solar|Deployment (Deployment is left out here: its choice reads deployment need (Accelerate or Watch) and it has no Industrial Opportunity; showing Solar — Refining)',
    });
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

  it("one economy's rows: the full technology x six-stage grid, in chain order, display names, unclassified nodes kept", () => {
    const rows = countryPostureRows(core, 'AAA');
    expect(rows).toHaveLength(12);
    expect(rows.slice(0, 6).map((r) => r.key)).toEqual(['Batteries|Extraction', 'Batteries|Refining', 'Batteries|Components', 'Batteries|Assembly', 'Batteries|Deployment', 'Batteries|Recovery']);
    expect(rows.slice(0, 6).map((r) => r.stageName)).toEqual(SUPPLY_STAGES.map((s) => LIB_STAGE_DISPLAY[s]));
    const sec = rows[1];
    expect([sec.postureWord, sec.choice, sec.rationale, sec.mode, sec.alignment, sec.needType, sec.supplyChainNeed, sec.tier]).toEqual([
      'Secure', 'Secure', 'Coercive chokepoint', null, 'Under-prioritized', 'Supply-chain need', 0.95, 'core',
    ]);
    const part = rows[0];
    expect([part.choice, part.mode, part.partner1Name, part.momentum, part.pathway, part.alignment, part.plotted]).toEqual([
      'Compete · Partner', 'Partner', 'Bland', 'rising', 'Not credible', 'Wrong treatment / misaligned', true,
    ]);
    const acc = rows[4];
    expect([acc.choice, acc.deployMode, acc.needType, acc.deploymentNeed, acc.supplyChainNeed, acc.alignment]).toEqual([
      'Deploy · Accelerate', 'Accelerate', 'Deployment need', 0.85, null, 'Under-prioritized: under-response (deployment)',
    ]);
    const na = rows[5];
    expect([na.posture, na.postureWord, na.choice, na.classified, na.plotted]).toEqual(['N', 'Not assessed', 'Not assessed', true, false]);
    const none = rows[11];
    expect([none.key, none.classified, none.posture, none.postureWord, none.need, none.plotted]).toEqual(['Solar|Recovery', false, null, null, null, false]);
    expect(rows[9].flag).toBe('Review: strong position with high need but a low prize (kept as Preserve, not Partner)');
  });

  it("one node's rows: Secure first, then Compete, Deploy, not assessed; within each by Industrial Opportunity, then by name", () => {
    expect(sectorPostureRows(core, 'Batteries', 'Components').map((r) => r.iso3)).toEqual(['BBB', 'AAA', 'CCC']);
    expect(sectorPostureRows(core, 'Batteries', 'Refining').map((r) => [r.iso3, r.postureWord])).toEqual([['AAA', 'Secure'], ['BBB', 'Deploy'], ['CCC', 'Deploy']]);
    const solar = sectorPostureRows(core, 'Solar', 'Refining');
    expect(solar.map((r) => [r.iso3, r.postureWord])).toEqual([['AAA', 'Secure'], ['BBB', 'Deploy'], ['CCC', 'Not assessed']]);
    expect(solar[2].plotted).toBe(false);
    expect(sectorPostureRows(core, 'Solar', 'Recovery').map((r) => r.iso3)).toEqual(['BBB', 'CCC']);
  });

  it('the posture CSV: plain-word columns, fractions, logicals as R writes them, provenance last', () => {
    const text = postureCsvText(postureCsvRows(countryPostureRows(core, 'AAA')), POSTURE_CSV_COLS, prov);
    const lines = text.split('\r\n');
    expect(lines[0]).toBe([...POSTURE_CSV_COLS, 'source', 'as_of', 'git_commit'].join(','));
    expect(lines).toHaveLength(14); // header, 12 nodes, the final line break
    expect(lines[1]).toBe(
      `Aland,AAA,core,Batteries,Extraction,Extraction,TRUE,Compete,Partner,,Compete · Partner,,Supply-chain need,0.8,,0.3,0.2,0.004,Not credible,Wrong treatment / misaligned,high,Strong,high,rising,Bland,BBB,FALSE,,,${source},2026-10-29,abc1234`,
    );
    expect(lines[2]).toBe(
      `Aland,AAA,core,Batteries,Refining,Refining & Processing,TRUE,Secure,,,Secure,Coercive chokepoint,Supply-chain need,0.95,,0.4,0.1,0.002,,Under-prioritized,medium,Strong,high,,,,,,,${source},2026-10-29,abc1234`,
    );
    expect(lines[12]).toBe(['Aland', 'AAA', 'core', 'Solar', 'Recovery', 'Recovery & Recycling', 'FALSE', ...Array(22).fill(''), source, '2026-10-29', 'abc1234'].join(','));
    // an empty table still names its columns
    expect(postureCsvText([], POSTURE_CSV_COLS, prov).split('\r\n')[0]).toBe(lines[0]);
  });

  it('the division rows and CSV: one row per node, names and gaps in words', () => {
    const rows = divisionRows(core, null);
    expect(rows.map((r) => r.key)).toEqual(['Batteries|Extraction', 'Batteries|Refining', 'Batteries|Components', 'Solar|Assembly']);
    expect([rows[1].primaryName, rows[1].backups, rows[1].gapLabel, rows[1].otherGaps, rows[1].capabilitySource, rows[1].aibNodes]).toEqual([
      'Bland', 'Aland (10%)', 'Insufficient redundancy', 'Weak evidence', 'Allied Industrial Base positions', 2,
    ]);
    expect([rows[3].primaryName, rows[3].backups, rows[3].gapLabel, rows[3].leaderName, rows[3].capabilitySource]).toEqual([null, null, 'No trusted supplier', 'XXX', 'World supply shares']);
    expect(divisionRows(core, 'Solar').map((r) => r.key)).toEqual(['Solar|Assembly']);
    const lines = postureCsvText(divisionCsvRows(rows), DIVISION_CSV_COLS, postureProvenance(core, 'division source')).split('\r\n');
    expect(lines[0]).toBe([...DIVISION_CSV_COLS, 'source', 'as_of', 'git_commit'].join(','));
    expect(lines[2]).toBe('Batteries,Refining,Bland,BBB,0.3,Aland (10%),AAA,2,0.4,0.5,XXX,XXX,0.5,Insufficient redundancy,Weak evidence,0,1,0,0,Allied Industrial Base positions,2,division source,2026-10-29,abc1234');
    expect(lines).toHaveLength(6);
  });
});
