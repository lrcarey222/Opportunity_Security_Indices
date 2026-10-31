// The fences of the Country page (posture/fences.ts; 2026-10-30, the Yards & Fences framing) on the posture fixture's
// Aland: each node behind exactly one fence by its choice and stage (Secure high; Preserve, Catch Up, Partner and
// Accelerate medium; Watch low; not assessed behind none), each list in its stated ranking order (Secure by
// supply-chain need; Preserve / Catch Up / Partner by the prize, the Strategic Industrial Value, with ties by Competitive
// Position; Accelerate and Deployment & Use Watch by deployment need; producing-stage Watch by the prize), the variables
// read through the drawer's accessors (core cell, detail.ts, meta.yf_nodes; null where the data carry none, nothing
// invented), the one field that explains each choice, and the layers as yard units (2026-10-30): a layer whose own choice
// differs from its node's sits behind its own fence, ranked with its node's value, its reason its own.

import { describe, expect, it } from 'vitest';
import { checkPostureDetail, decodeCore } from '../contract.ts';
import { countryCells } from '../derive.ts';
import { byPositionOrder, deploymentVars, familyWords, fenceGroups, fenceReason, pathwayWords, positionVars, prizeVars, supplyNeedVars, topDeploymentFamily, tradeMomentumWords } from '../fences.ts';
import { FENCE_VERBS, nodeFence, verbOf } from '../grammar.ts';
import { fixtureCore, fixtureDetail, withCell } from './fixture.ts';

const core = decodeCore(fixtureCore());
const detail = checkPostureDetail(fixtureDetail());
const cells = countryCells(core, 'AAA');
const names = { BBB: 'Bland', CCC: 'Cland' };
const keys = (xs: { key: string }[]) => xs.map((x) => x.key);

describe('fenceGroups: the nodes behind each fence, in ranking order', () => {
  const g = fenceGroups(cells, detail);

  it('every assessed node sits behind exactly one fence, by its choice and stage; a node not assessed behind none', () => {
    const all = [...g.secure, ...g.compete, ...g.accelerate, ...g.watchSupply, ...g.watchDeploy];
    expect(all).toHaveLength(cells.filter((c) => verbOf(c) !== null).length);
    expect(new Set(keys(all)).size).toBe(all.length);
    expect(keys(all)).not.toContain('Batteries|Recovery');
    for (const n of g.secure) expect(nodeFence(n.verb, n.stage)).toBe('high');
    for (const n of [...g.compete, ...g.accelerate]) expect(nodeFence(n.verb, n.stage)).toBe('medium');
    for (const n of [...g.watchSupply, ...g.watchDeploy]) expect(nodeFence(n.verb, n.stage)).toBe('low');
    expect(new Set(g.secure.map((n) => n.verb))).toEqual(new Set(FENCE_VERBS.high));
    expect(new Set(g.compete.map((n) => n.verb))).toEqual(new Set(['preserve', 'catch_up', 'partner']));
    expect(new Set(g.accelerate.map((n) => n.verb))).toEqual(new Set(['accelerate']));
    expect(g.watchSupply.every((n) => n.verb === 'watch' && n.stage !== 'Deployment')).toBe(true);
    expect(g.watchDeploy.every((n) => n.verb === 'watch' && n.stage === 'Deployment')).toBe(true);
  });

  it('High fence: Secure by supply-chain need, highest first', () => {
    expect(keys(g.secure)).toEqual(['Batteries|Refining', 'Solar|Refining', 'Solar|Components']);
    expect(g.secure.map((n) => n.cell.need)).toEqual([0.95, 0.9, 0.7]);
  });

  it('Medium fence: Preserve, Catch Up and Partner by the prize (Strategic Industrial Value), ties by Competitive Position; Accelerate by deployment need', () => {
    // 0.9, 0.8, then 0.3 twice: Solar Assembly (position 0.9) before Batteries Extraction (position 0.2)
    expect(keys(g.compete)).toEqual(['Batteries|Assembly', 'Batteries|Components', 'Solar|Assembly', 'Batteries|Extraction']);
    expect(keys(g.accelerate)).toEqual(['Batteries|Deployment']);
    // the tie breaks the other way when the positions swap
    const swapped = countryCells(decodeCore(withCell(withCell(fixtureCore(), 0, 1, 3, { cp: 0.1 }), 0, 0, 0, { cp: 0.6 })), 'AAA');
    expect(keys(fenceGroups(swapped, detail).compete).slice(2)).toEqual(['Batteries|Extraction', 'Solar|Assembly']);
  });

  it('the position tables: the same nodes in Competitive Position order (ties by the prize), a missing position last', () => {
    expect(keys(byPositionOrder(g.compete))).toEqual(['Batteries|Assembly', 'Solar|Assembly', 'Batteries|Components', 'Batteries|Extraction']);
    expect(byPositionOrder(g.compete).map((n) => n.cell.cp)).toEqual([0.95, 0.9, 0.4, 0.2]);
    // the input is not reordered
    expect(keys(g.compete)).toEqual(['Batteries|Assembly', 'Batteries|Components', 'Solar|Assembly', 'Batteries|Extraction']);
    // a tie on position: the higher prize first; a node without a position last
    const tie = countryCells(decodeCore(withCell(withCell(fixtureCore(), 0, 0, 2, { cp: 0.9 }), 0, 0, 0, { cp: null })), 'AAA');
    expect(keys(byPositionOrder(fenceGroups(tie, detail).compete))).toEqual(['Batteries|Assembly', 'Batteries|Components', 'Solar|Assembly', 'Batteries|Extraction']);
  });

  it('Low fence: producing-stage Watch by the prize (the biggest prizes not chased); Deployment & Use Watch by deployment need', () => {
    expect(keys(g.watchSupply)).toEqual(['Solar|Extraction', 'Solar|Recovery']);
    expect(keys(g.watchDeploy)).toEqual(['Solar|Deployment']);
    // two deployment Watch nodes: the higher need (closest to Accelerate) first
    const two = countryCells(decodeCore(withCell(fixtureCore(), 0, 0, 4, { v: 'deploy_watch', dm: 'watch', need: 0.6 })), 'AAA');
    expect(keys(fenceGroups(two, detail).watchDeploy)).toEqual(['Batteries|Deployment', 'Solar|Deployment']);
  });

  it('without the detail document, the groups and orders are the same (they read the core); the nodes carry no detail', () => {
    const bare = fenceGroups(cells, null);
    expect(keys(bare.secure)).toEqual(keys(g.secure));
    expect(keys(bare.compete)).toEqual(keys(g.compete));
    expect([...bare.secure, ...bare.compete].every((n) => n.detail === null)).toBe(true);
  });
});

describe('the variables: the drawer accessors, never invented', () => {
  const g = fenceGroups(cells, detail);
  const by = (k: string) => [...g.secure, ...g.compete, ...g.accelerate, ...g.watchSupply, ...g.watchDeploy].find((n) => n.key === k)!;

  it('need: supply-chain need and its four families from the detail (either spelling); a family not written is null', () => {
    expect(supplyNeedVars(by('Batteries|Refining'))).toEqual({ need: 0.95, importDependence: 0.95, supplierConcentration: 0.9, riskyExposure: 0.92, trustedAlternatives: null });
    expect(supplyNeedVars(by('Solar|Extraction'))).toEqual({ need: 0.2, importDependence: 0.1, supplierConcentration: 0.2, riskyExposure: 0.1, trustedAlternatives: 0.2 });
    expect(supplyNeedVars(fenceGroups(cells, null).secure[0]).importDependence).toBeNull();
  });

  it('prize: the Strategic Industrial Value and the market prize from the core, commons, option and the cap from the node evidence, the market components from the detail, the pathway for Catch Up only', () => {
    expect(prizeVars(by('Batteries|Components'), core.meta)).toEqual({
      opportunity: 0.8, market: 0.7, commons: 0.5, option: 0, cap: null, valueCap: null,
      nodePrize: 0.85, homeMarket: 0.75, investmentMomentum: 0.7, outlook: null, basis: 'national data', need: 0.5, pathway: 'Credible (feasibility, pipeline)',
    });
    expect(prizeVars(by('Solar|Assembly'), core.meta)).toMatchObject({ opportunity: 0.3, market: 0.3, commons: 0.5, option: 0, nodePrize: 0.3, outlook: 'oversupplied', basis: null, pathway: null });
    // a market-only node carries its cap; the market prize differs from the value where the core says so
    expect(prizeVars(by('Batteries|Extraction'), core.meta)).toMatchObject({ opportunity: 0.3, market: 0.3, commons: 0.333, cap: 'market_only', pathway: null });
    expect(prizeVars(by('Batteries|Assembly'), core.meta)).toMatchObject({ opportunity: 0.9, market: 0.85, commons: 1, option: 0.5, cap: null });
    // without the node evidence (no meta) the families the core does not carry per cell are null, never invented
    expect(prizeVars(by('Batteries|Components'))).toMatchObject({ opportunity: 0.8, market: 0.7, commons: null, option: null, cap: null });
    expect(pathwayWords({ pathway: 'yes' }, null)).toBe('Credible');
    expect(pathwayWords({ pathway: null }, null)).toBeNull();
  });

  it('position: Competitive Position and world supply share from the core, the families from the detail, momentum as the drawer reads it', () => {
    const band = core.meta.momentum_band;
    // R writes the structural share only where momentum moved the export position: where it dropped an identical
    // one, the structural share is the export position (flagged), never "not measured"; where it wrote one, that one
    expect(positionVars(by('Batteries|Assembly'), band)).toEqual({
      cp: 0.95, share: 0.12, exportPosition: 0.9, structural: 0.9, structuralSame: true, investment: null, momentum: 'rising', evidence: 'Weak',
    });
    expect(positionVars(by('Solar|Assembly'), band)).toMatchObject({ exportPosition: 0.85, structural: 0.7, structuralSame: false });
    // no export position: no structural share either (not invented)
    expect(positionVars(fenceGroups(cells, null).compete[0], band)).toMatchObject({ exportPosition: null, structural: null, structuralSame: false });
    expect(positionVars(by('Batteries|Components'), band)).toMatchObject({ cp: 0.4, share: 0.03, exportPosition: 0.35, investment: 0.6, momentum: 'flat' });
    // the detail's trade evidence without a matched momentum: not measured (the drawer's "unavailable"), never the core arrow
    expect(tradeMomentumWords({ momentum: 'up' }, by('Batteries|Refining').detail, band)).toBeNull();
    expect(tradeMomentumWords({ momentum: 'up' }, null, band)).toBe('rising');
  });

  it('deployment: deployment need and its families with basis and scenario; the climate-aligned ratio is its own words', () => {
    const v = deploymentVars(by('Batteries|Deployment'));
    expect(v.need).toBe(0.85);
    expect(familyWords(v.demand, core.meta.scenario_labels)).toBe('90 · Current policy · national data');
    expect(familyWords(v.policyGap, core.meta.scenario_labels)).toBe('80 · National commitments · national data');
    expect(familyWords(v.system, core.meta.scenario_labels)).toBeNull();
    expect(familyWords(v.alternative, core.meta.scenario_labels)).toBe('+104% vs current policies · Climate-aligned · World 2035, Net Zero vs Current Policies · global context');
    expect(v.trend).toBe('rising');
    expect(deploymentVars(by('Solar|Deployment')).trend).toBe('+8% a year');
    // the top family is the highest 0-1 score, never the ratio
    expect(topDeploymentFamily(by('Batteries|Deployment'))?.label).toBe('Demand adequacy');
    expect(topDeploymentFamily(by('Solar|Deployment'))?.label).toBe('System need');
    expect(topDeploymentFamily(fenceGroups(cells, null).accelerate[0])).toBeNull();
  });

  it('the field that explains each choice', () => {
    expect(fenceReason(by('Batteries|Refining'), core, names)).toBe('Coercive chokepoint');
    expect(fenceReason(by('Batteries|Assembly'), core, names)).toBe('Competitive Position 95');
    expect(fenceReason(by('Batteries|Components'), core, names)).toBe('Pathway: Credible (feasibility, pipeline)');
    expect(fenceReason(by('Batteries|Extraction'), core, names)).toBe('Leading partner: Bland');
    expect(fenceReason(by('Batteries|Deployment'), core, names)).toBe('Top need: Demand adequacy 90 · Current policy · national data');
    expect(fenceReason(by('Solar|Extraction'), core, names)).toBe('Low need; Low prize; Ample diversified supply');
    expect(fenceReason(by('Solar|Deployment'), core, names)).toBe('Deployment need below the acceleration bar');
    // a Partner node with the economy's partner gap: no credible allied solution yet (Cland's Batteries Components)
    const cland = fenceGroups(countryCells(core, 'CCC'), null).compete.find((n) => n.key === 'Batteries|Components')!;
    expect(fenceReason(cland, core, names)).toBe('No credible allied solution yet');
    // the routes met, where a Secure node meets more than one
    const routes = checkPostureDetail({ ...fixtureDetail(), cells: (fixtureDetail().cells as Record<string, unknown>[]).map((c) => (c.stage === 'Refining' && c.tech === 'Batteries' ? { ...c, security_routes: 'coercive_chokepoint;systemic_resilience' } : c)) });
    expect(fenceReason(fenceGroups(cells, routes).secure[0], core, names)).toBe('Coercive chokepoint (routes met: Coercive chokepoint, Systemic resilience)');
    // Accelerate before its detail loads: not available (the page shows "…" while it loads)
    expect(fenceReason(fenceGroups(cells, null).accelerate[0], core, names)).toBeNull();
  });
});

describe('layers as yard units (fenceGroups with the economy layers)', () => {
  const ycore = decodeCore(fixtureCore(true, { yards: true }));
  const ydetail = checkPostureDetail(fixtureDetail({ yards: true }));
  const ycells = countryCells(ycore, 'AAA');
  const g = fenceGroups(ycells, ydetail, ycore.meta);
  const layerOf = (xs: { layer: { id: string } | null }[]) => xs.map((x) => x.layer?.id ?? null);

  it('a layer whose own choice differs from its node sits behind the fence of its own choice, ranked with its node value', () => {
    // the Secure control layer of a Preserve node (Medium) joins the High fence, after the three Secure nodes (its node need 0.4)
    expect(keys(g.secure)).toEqual(['Batteries|Refining', 'Solar|Refining', 'Solar|Components', 'Batteries|Assembly#bess-control']);
    expect(layerOf(g.secure)).toEqual([null, null, null, 'bess-control']);
    const ctl = g.secure[3];
    expect([ctl.verb, ctl.nodeKey, ctl.cell.need, ctl.layer?.yard]).toEqual(['secure', 'Batteries|Assembly', 0.4, 'small']);
    expect(ctl.layer?.label).toBe('Battery-management firmware, energy-management software and remote updates');
    // the Watch layers join the Low fence by their node prize (0.9 and 0.3) before the Watch nodes (0.2, 0.15)
    expect(keys(g.watchSupply)).toEqual(['Batteries|Assembly#bess-enclosure', 'Solar|Assembly#pv-racking', 'Solar|Extraction', 'Solar|Recovery']);
    expect(g.watchSupply.map((n) => n.cell.prize)).toEqual([0.9, 0.3, 0.2, 0.15]);
    // a layer that shares its node choice (bess-cells, Preserve) is the node own row, not a unit of its own
    expect(keys(g.compete)).toEqual(['Batteries|Assembly', 'Batteries|Components', 'Solar|Assembly', 'Batteries|Extraction']);
    expect([...g.secure, ...g.compete, ...g.watchSupply].some((n) => n.layer?.id === 'bess-cells')).toBe(false);
    // the nodes themselves are unchanged
    expect(new Set([...g.compete].map((n) => n.verb))).toEqual(new Set(['preserve', 'catch_up', 'partner']));
  });

  it('on a tie a layer follows its node; without the detail document there are no layers', () => {
    const tie = countryCells(decodeCore(withCell(fixtureCore(true, { yards: true }), 0, 0, 3, { p: 'D', v: 'deploy', m: null, dm: 'watch', yd: 'SL' })), 'AAA');
    // Batteries Assembly turned Watch: its Secure layer still a unit, its Watch layer now shares the choice
    const t = fenceGroups(tie, ydetail, ycore.meta);
    expect(keys(t.watchSupply).slice(0, 1)).toEqual(['Batteries|Assembly']);
    expect(keys(t.watchSupply)).not.toContain('Batteries|Assembly#bess-enclosure');
    expect(keys(t.compete)).toContain('Batteries|Assembly#bess-cells');
    const bare = fenceGroups(ycells, null, ycore.meta);
    expect([...bare.secure, ...bare.watchSupply].every((n) => n.layer === null)).toBe(true);
  });

  it('a layer unit explains itself: its variant, its fence scope, its large-yard signals or its commons and option', () => {
    expect(fenceReason(g.secure[3], ycore, names)).toBe('Control built partly into the hardware: the high fence extends to the hardware.');
    expect(fenceReason(g.watchSupply[0], ycore, names)).toBe(
      'Large-yard case: mature, passive, standardized, substitutable, quick to qualify, little process learning, little cross-sector spillover, little product differentiation; kept with a capability floor.',
    );
    expect(fenceReason(g.watchSupply[1], ycore, names)).toMatch(/^Large-yard case: mature, passive, .*commoditized\.$/);
    // the layer own commons and option in the prize table (bess-control: commons 0.333 of one attribute, market only)
    expect(prizeVars(g.secure[3], ycore.meta)).toMatchObject({ opportunity: 0.9, commons: 0.333, option: 0, cap: 'market_only', valueCap: null });
  });
});
