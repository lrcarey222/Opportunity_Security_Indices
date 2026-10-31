// The derivations of the posture cards (src/posture/derive.ts) and the site-wide grammar (grammar.ts), on
// the schema-2 fixture: the counts (six verbs primary, three families, not assessed), the Sector and AIB
// takeaways, the counts of a page's tile, the checked ?pnode node, and the rows of the table twins and CSVs
// with their provenance (source, as_of, git_commit last). The contract itself is in contract.test.ts.

import { describe, expect, it } from 'vitest';
import { STAGE_DISPLAY as LIB_STAGE_DISPLAY, STAGE_SHORT as LIB_STAGE_SHORT, SUPPLY_STAGES } from '../../lib/stages.ts';
import { ALIGNMENTS, decodeCore, POSTURE_STAGES, type PostureCell } from '../contract.ts';
import {
  aibHeader, aibHeaderTiles, aibTakeaway, cellKeyParts, countPostures, countryCells, countryNames, countryPostureRows, DIVISION_CSV_COLS, divisionCsvRows, divisionRows,
  freshnessLine, parsePostureNode, POSTURE_CSV_COLS, postureCountsText, postureCsvRows, postureCsvText, postureProvenance, producingStagesOf, score100, sectorPostureRows,
  sectorTakeaway, strategyCountsText,
} from '../derive.ts';
import {
  ALIGNMENT_CLASS_LABEL, ALIGNMENT_LABEL, alignmentClass, alignmentFlagWords, borderlineWords, choiceWord, EVIDENCE_LABEL, EVIDENCE_LEVELS, FENCE_LABEL, FENCE_OF_VERB,
  FENCE_ORDER, FENCE_VERBS, fenceChoiceWord, flagWords, MARKET_PRIZE_LABEL, nodeFence, nodeFill, NOT_ASSESSED_HATCH, pathwayEvidenceWords, POSTURE_COLOR, POSTURE_LABEL, PRIZE_LABEL,
  prizeLabel, STAGE_DISPLAY, STAGE_SHORT, traceCodeWords, traceNumberWords, traceTestWords, VERB_COLOR, VERB_FAMILY, VERB_LABEL, VERB_LONG, VERB_ORDER, VERB_OUTLINE, verbOf,
  watchReasonWords,
} from '../grammar.ts';
import { variantWords } from '../yards.ts';
import { postureMomentum } from '../momentum.ts';
import { contrast as contrastRatio } from '../../charts/scale.ts';
import { fixtureCore, renameEconomies, withoutCell } from './fixture.ts';

// the NEIS tokens of dashboard/src/styles/globals.css (@theme): signal, brand blue / green / teal, the sequential ramp, the ink greys
const NEIS = new Set(['#f58229', '#1889b0', '#a3b63b', '#55c4c5', '#016787', '#007da3', '#2792ba', '#43a8d0', '#5cbfe8', '#75d5fe', '#b4e7fe', '#111827', '#1f2937', '#374151', '#4b5563', '#6b7280', '#9ca3af', '#d1d5db', '#ffffff']);

describe('one site-wide grammar (spec XX)', () => {
  it('three families in NEIS colours (orange, blue, green) where they are still drawn; a node not assessed has none', () => {
    expect(POSTURE_COLOR).toEqual({ S: '#f58229', C: '#1889b0', D: '#a3b63b' });
    expect(Object.values(POSTURE_LABEL)).toEqual(['Secure', 'Compete', 'Deploy', 'Not assessed']);
    expect(new Set(VERB_ORDER.map((v) => VERB_FAMILY[v]))).toEqual(new Set(['S', 'C', 'D']));
  });

  it('one colour per choice, NEIS tokens only, all six distinct; not assessed is the hatch, never a colour (2026-10-30)', () => {
    expect(VERB_COLOR).toEqual({ secure: '#f58229', preserve: '#1889b0', catch_up: '#b4e7fe', partner: '#55c4c5', accelerate: '#a3b63b', watch: '#4b5563' });
    for (const v of VERB_ORDER) expect(NEIS.has(VERB_COLOR[v]), v).toBe(true);
    expect(new Set(Object.values(VERB_COLOR)).size).toBe(6);
    expect(Object.values(VERB_COLOR)).not.toContain(NOT_ASSESSED_HATCH);
    const c = { posture: 'N' as const, mode: null, deployMode: null, rationale: null };
    expect(nodeFill(c)).toBe(NOT_ASSESSED_HATCH);
    expect(nodeFill({ ...c, posture: 'C', mode: 'catch_up' })).toBe('#b4e7fe');
    expect(nodeFill({ ...c, posture: 'D', deployMode: 'watch' })).toBe('#4b5563');
    // Watch (ink-600) is below 3:1 on the card, so it carries a light outline; the others the card ring
    expect(contrastRatio(VERB_COLOR.watch, '#1f2937')).toBeLessThan(3);
    expect(contrastRatio(VERB_OUTLINE.watch, '#1f2937')).toBeGreaterThanOrEqual(3);
    for (const v of VERB_ORDER.filter((x) => x !== 'watch')) expect(contrastRatio(VERB_COLOR[v], '#1f2937'), v).toBeGreaterThanOrEqual(3);
    expect(contrastRatio('#ffffff', '#000000')).toBeCloseTo(21, 5);
  });

  it('the fences: Secure high; Preserve, Catch Up, Partner and Accelerate medium; Watch low; at Deployment & Use, Accelerate medium and Watch low', () => {
    expect(FENCE_ORDER.map((f) => FENCE_LABEL[f])).toEqual(['High fence', 'Medium fence', 'Low fence']);
    expect(FENCE_VERBS).toEqual({ high: ['secure'], medium: ['preserve', 'catch_up', 'partner', 'accelerate'], low: ['watch'] });
    expect(VERB_ORDER.map((v) => FENCE_OF_VERB[v])).toEqual(['high', 'medium', 'medium', 'medium', 'medium', 'low']);
    for (const s of ['Extraction', 'Refining', 'Components', 'Assembly', 'Recovery']) {
      expect([nodeFence('secure', s), nodeFence('preserve', s), nodeFence('catch_up', s), nodeFence('partner', s), nodeFence('watch', s)], s).toEqual(['high', 'medium', 'medium', 'medium', 'low']);
    }
    expect([nodeFence('accelerate', 'Deployment'), nodeFence('watch', 'Deployment'), nodeFence(null, 'Deployment')]).toEqual(['medium', 'low', null]);
    const c = { posture: 'C' as const, mode: 'partner' as const, deployMode: null, stage: 'Extraction' as PostureCell['stage'] };
    expect(fenceChoiceWord(c)).toBe('Partner · Medium fence');
    expect(fenceChoiceWord({ ...c, posture: 'D', mode: null, deployMode: 'accelerate', stage: 'Deployment' })).toBe('Accelerate · Medium fence');
    expect(fenceChoiceWord({ ...c, posture: 'S', mode: null })).toBe('Secure · High fence');
    expect(fenceChoiceWord({ ...c, posture: 'N', mode: null })).toBe('Not assessed');
  });

  it('the verbs and their words; nothing when not assessed', () => {
    const c = { posture: 'N' as const, mode: null, deployMode: null, rationale: null };
    expect([verbOf(c), choiceWord(c)]).toEqual([null, 'Not assessed']);
    // a choice is named with its fence, never with the R family: a producing-stage Watch is not "Deploy · Watch"
    expect(choiceWord({ ...c, posture: 'C', mode: 'catch_up' })).toBe('Catch Up · Medium fence');
    expect(choiceWord({ ...c, posture: 'D', deployMode: 'watch' })).toBe('Watch · Low fence');
    expect(choiceWord({ ...c, posture: 'S' })).toBe('Secure · High fence');
    for (const v of VERB_ORDER) {
      expect(VERB_LONG[v], v).toBe(`${VERB_LABEL[v]} · ${FENCE_LABEL[FENCE_OF_VERB[v]]}`);
      expect(VERB_LONG[v], v).not.toMatch(/Compete|Deploy/);
    }
    expect(VERB_ORDER.map((v) => VERB_LABEL[v])).toEqual(['Secure', 'Preserve', 'Catch Up', 'Partner', 'Accelerate', 'Watch']);
  });

  it('policy alignment: five classes; a deployment gap is drawn as under-prioritized and worded as an under-response', () => {
    expect(ALIGNMENTS.map(alignmentClass)).toEqual(['aligned', 'under_prioritized', 'over_prioritized', 'misaligned', 'under_prioritized', 'mixed', 'insufficient_data']);
    expect(ALIGNMENT_CLASS_LABEL.misaligned).toBe('Wrong tool for the problem');
    expect(ALIGNMENT_LABEL.deployment_gap).toBe('Needs more attention: deployment policy lags');
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
    // the Yards & Fences inputs (2026-10-30): the borderline and pivotal inputs name the prize the choice read and the large-yard case
    expect(borderlineWords('strategic_industrial_value;large_yard_case')).toBe('Strategic Industrial Value, the large-yard case');
    for (const b of ['strategic_industrial_value', 'large_yard_case', 'coercive_import_share', 'home_share', 'stockpile', 'exposed']) expect(borderlineWords(b), b).not.toMatch(/_/);
    expect(watchReasonWords('large_yard_case')).toBe('Large yard: affordable, diversified access should dominate');
    expect(traceTestWords('large_yard_case')).toBe('Large-yard case (commodity signals)');
    expect(traceTestWords('strategic_industrial_value')).toBe('Strategic Industrial Value');
    // the large-yard step counts signals, every other number is a score
    expect([traceNumberWords('large_yard_case', 3, (x) => `${x * 100}%`), traceNumberWords('large_yard_case', 1, (x) => `${x}`), traceNumberWords('supply_chain_need', 0.5, (x) => `${x * 100}%`)]).toEqual([
      '3 signals', '1 signal', '50%',
    ]);
    // the variants a drawer or a layer reason names, the new ones included (deploy_accelerate, deploy_floor)
    for (const v of ['deploy_accelerate', 'deploy_floor', 'deploy_diversify', 'deploy', 'secure_control', 'secure_integrated', 'secure_whole', 'secure_unclear', 'secure_input', 'secure_dual_use', 'secure_resilience', 'compete_preserve', 'compete_catch_up', 'compete_partner']) {
      expect(variantWords(v), v).not.toMatch(/_/);
    }
    expect(variantWords('deploy_floor')).toBe('a large yard kept with a capability floor');
    expect(variantWords('deploy_accelerate')).toBe('deployment priority high');
    // the prize as the core names it: the Strategic Industrial Value, or the market prize for an older core
    expect([prizeLabel({ prize_measure: 'strategic_value' }), prizeLabel({ prize_measure: 'market' }), PRIZE_LABEL, MARKET_PRIZE_LABEL]).toEqual([
      'Strategic Industrial Value', 'Industrial Opportunity', 'Strategic Industrial Value', 'Industrial Opportunity',
    ]);
    // the credible-pathway evidence and the compact decision trace (R pathway.R, decision_engine.R sp_choice_trace)
    expect(pathwayEvidenceWords(['feasibility', 'adjacent_stage', 'input_cost'])).toBe('feasibility, adjacent stage, input-cost advantage');
    expect(pathwayEvidenceWords('none_observed_fires')).toBe('evidence observed, none sufficient');
    expect(pathwayEvidenceWords('feasibility;resource_base_unknown')).toBe('feasibility, resource base unknown');
    // the two-leg pathway (config pathway.min_legs): one leg fired, or the only observed leg fired
    expect(pathwayEvidenceWords('adjacent_stage;too_few_legs')).toBe('adjacent stage, too few pieces of evidence');
    // the capability-anchor rule's codes (2026-10-30): anchors, supports and states in plain words, never raw codes
    expect(pathwayEvidenceWords('direct_export_share;anchor_uncorroborated')).toBe('world export share (anchor), anchor not corroborated');
    expect(pathwayEvidenceWords('adjacent_stage_relative;no_anchor')).toBe('relatively strong adjacent stage, no capability anchor');
    expect(pathwayEvidenceWords(['operating_capacity', 'committed_pipeline', 'announced_pipeline', 'anchor_unobserved'])).toBe('operating capacity (anchor), committed capacity (anchor), announced capacity, too little evidence to find an anchor');
    expect(pathwayEvidenceWords('feasibility;too_few_observed')).toBe('feasibility, too little evidence observed to reach the bar');
    expect(['secure_override', 'deployment_need', 'supply_chain_need', 'industrial_opportunity', 'cp_contender_pct', 'pathway_credible', 'choice'].map(traceTestWords)).toEqual([
      'Secure tests', 'Deployment need', 'Supply-chain need', 'Industrial Opportunity (market prize)', 'Competitive Position among contenders', 'Credible pathway', 'Choice',
    ]);
    expect(traceCodeWords('operational_control;coercive_chokepoint')).toBe('operational / control, chokepoint');
    expect(['capability_floor_not_met', 'not_a_contender', 'not_top_quarter', 'not_assessed', 'catch_up', 'high'].map(traceCodeWords)).toEqual([
      'top quarter, capability floor not met', 'top quarter, not a contender', 'not in the top quarter', 'not assessed', 'Catch Up', 'high',
    ]);
  });

  it('evidence: strong, moderate, weak, in words (the map draws no fade since 2026-10-30)', () => {
    expect(EVIDENCE_LEVELS.map((e) => EVIDENCE_LABEL[e])).toEqual(['Strong', 'Moderate', 'Weak']);
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
      problem: 'pnode=Solar|Deployment (Deployment is left out here: its choice reads deployment need (Accelerate or Watch) and it has no prize of its own; showing Solar — Refining)',
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
      'Secure', 'Secure · High fence', 'Chokepoint risk', null, 'Needs more attention', 'Supply-chain need', 0.95, 'core',
    ]);
    const part = rows[0];
    expect([part.choice, part.mode, part.partner1Name, part.momentum, part.pathway, part.alignment, part.plotted]).toEqual([
      'Partner · Medium fence', 'Partner', 'Bland', 'rising', 'Not credible', 'Wrong tool for the problem', true,
    ]);
    const acc = rows[4];
    expect([acc.choice, acc.deployMode, acc.needType, acc.deploymentNeed, acc.supplyChainNeed, acc.alignment]).toEqual([
      'Accelerate · Medium fence', 'Accelerate', 'Deployment need', 0.85, null, 'Needs more attention: deployment policy lags',
    ]);
    const na = rows[5];
    expect([na.posture, na.postureWord, na.choice, na.classified, na.plotted]).toEqual(['N', 'Not assessed', 'Not assessed', true, false]);
    const none = rows[11];
    expect([none.key, none.classified, none.posture, none.postureWord, none.need, none.plotted]).toEqual(['Solar|Recovery', false, null, null, null, false]);
    expect(rows[9].flag).toBe('Review: strong position with high need but a low prize (kept as Preserve, not Partner)');
  });

  it("one node's rows: Secure first, then Compete, Deploy, not assessed; within each by the prize, then by name", () => {
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
    // the prize: the Strategic Industrial Value and the market prize (Industrial Opportunity) beside it; the node's yards
    expect(lines[1]).toBe(
      `Aland,AAA,core,Batteries,Extraction,Extraction,TRUE,Compete,Partner,,Partner · Medium fence,,Supply-chain need,0.8,,0.3,0.3,Medium yard,0.2,0.004,Not credible,Wrong tool for the problem,high,Strong,high,rising,Bland,BBB,FALSE,,,${source},2026-10-29,abc1234`,
    );
    expect(lines[2]).toBe(
      `Aland,AAA,core,Batteries,Refining,Refining & Processing,TRUE,Secure,,,Secure · High fence,Chokepoint risk,Supply-chain need,0.95,,0.4,0.4,Small yard,0.1,0.002,,Needs more attention,medium,Strong,high,,,,,,,${source},2026-10-29,abc1234`,
    );
    // Batteries Components: a value of 0.8 on a market prize of 0.7
    expect(lines[3]).toContain(',Supply-chain need,0.5,,0.8,0.7,Medium yard,0.4,');
    expect(lines[12]).toBe(['Aland', 'AAA', 'core', 'Solar', 'Recovery', 'Recovery & Recycling', 'FALSE', ...Array(24).fill(''), source, '2026-10-29', 'abc1234'].join(','));
    // an empty table still names its columns
    expect(postureCsvText([], POSTURE_CSV_COLS, prov).split('\r\n')[0]).toBe(lines[0]);
    // an older core (its prize the market measure, no yards): no strategic value, the market prize where it was, no yards
    const legacy = postureCsvText(postureCsvRows(countryPostureRows(decodeCore(withoutCell(fixtureCore(true, { legacy: true }), 0, 1, 5)), 'AAA')), POSTURE_CSV_COLS, prov).split('\r\n');
    expect(legacy[1]).toContain(',Supply-chain need,0.8,,,0.3,,0.2,0.004,');
    // mixed yards in words (the layered fixture): Aland's Batteries Final Assembly spans all three
    const mixed = postureCsvText(postureCsvRows(countryPostureRows(decodeCore(withoutCell(fixtureCore(true, { yards: true }), 0, 1, 5)), 'AAA')), POSTURE_CSV_COLS, prov).split('\r\n');
    expect(mixed[4]).toContain(',0.9,0.85,"Mixed: this step in the Medium yard, its layers also in the Small and Large yards",0.95,');
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
