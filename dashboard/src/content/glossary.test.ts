import { describe, expect, it } from 'vitest';
import { fillGlossaryText, GLOSSARY, GLOSSARY_DEFAULTS, GLOSSARY_TEMPLATES, glossaryById, glossaryFor, glossaryWith, wordCount } from './glossary.ts';
import { ACRONYM_NAMES, bareAcronymsInText } from '../components/testing/acronyms.ts';
import { POSTURE_TERMS } from '../posture/basis.tsx';

/** The ids of spec_country.md C3.2, in its order (the interface every module codes against). */
const SPEC_IDS = [
  'es', 'eo', 'si', 'need', 'psi', 'fs', 'po', 'dp', 'policy_index', 'policy_basis', 'pams', 'cat', 'rankable', 'imputed', 's_curve', 'percentile',
  'median', 'colour_scale', 'sector', 'stage', 'coalition', 'alignment', 'allied_pick', 'endowment_screen', 'best_five',
  'hhi', 'effective_n', 'rca', 'feasibility', 'matrix', 'mirror', 'basket', 'sub_component', 'net_balance', 'self_sufficiency', 'cmfdi', 'chinese_parent',
  'investment_ties', 'trl', 'unattributed', 'paper_baseline', 'scenario', 'dials', 'hawkishness', 'bullishness', 'populism', 'greenness', 'grow',
  // the corners with the keys secure and watch, shown as Exposed and Stable since 2026-10-28
  'upgrade', 'exposed', 'stable', 'quadrant',
  // the paper baseline's index model, demoted to Legacy OSI (2026-10-29)
  'legacy_osi', 'priority_no_policy', 'ties_today', 'memo',
  // Development & finance raw data (Sector Insights, 2026-10-15)
  'oda', 'dip_position', 'conduit_centre', 'wdi',
  // the strategic position framework (posture/basis.tsx POSTURE_TERMS; canonical since 2026-10-29)
  'posture', 'strategic_need', 'supply_chain_need', 'deployment_need', 'need_type', 'strategic_industrial_value', 'industrial_opportunity', 'competitive_position', 'pathway',
  'deploy_mode', 'security_rationale', 'delivery_mode', 'consistency_flag', 'policy_alignment', 'partner_gap', 'trade_momentum', 'aib_gap', 'partner_demand',
  // Yards & Fences (2026-10-30)
  'yard', 'large_yard_case',
];

describe('glossary content (C3.2)', () => {
  it('has exactly the spec ids, in order, each once', () => {
    expect(GLOSSARY.map((e) => e.id)).toEqual(SPEC_IDS);
    expect(glossaryById.size).toBe(SPEC_IDS.length);
  });

  it('every short definition is at most 35 words; every entry names a module', () => {
    for (const e of GLOSSARY) {
      expect(wordCount(e.short), e.id).toBeLessThanOrEqual(35);
      expect(e.short.trim().length, e.id).toBeGreaterThan(20);
      expect(e.modules.length, e.id).toBeGreaterThan(0);
      if (e.more) expect(e.more.split(/(?<=[.!?])\s+(?=[A-Z])/).length, e.id).toBeLessThanOrEqual(4);
    }
  });

  it('related ids exist and never point at the entry itself', () => {
    for (const e of GLOSSARY) for (const r of e.related ?? []) {
      expect(glossaryById.has(r), `${e.id} -> ${r}`).toBe(true);
      expect(r).not.toBe(e.id);
    }
  });

  it('fills every placeholder from the defaults, and from newer values when given', () => {
    for (const e of GLOSSARY) {
      expect(e.short, e.id).not.toMatch(/\{\{/);
      if (e.more) expect(e.more, e.id).not.toMatch(/\{\{/);
    }
    // the templates do carry placeholders (so a manifest can update them)
    expect(GLOSSARY_TEMPLATES.some((e) => e.short.includes('{{n_core}}'))).toBe(true);
    expect(glossaryById.get('coalition')?.short).toContain(`${GLOSSARY_DEFAULTS.n_core} core members`);
    const next = glossaryWith({ n_core: '32', n_partner: '27' });
    expect(next.find((e) => e.id === 'coalition')?.short).toContain('32 core members');
    // the retired allied network design has no entries (2026-10-17)
    for (const id of ['network', 'lambda', 'eu_node']) expect(next.some((e) => e.id === id), id).toBe(false);
    expect(fillGlossaryText('{{unknown}} and {{n_partner}}')).toBe(`{{unknown}} and ${GLOSSARY_DEFAULTS.n_partner}`);
  });

  it('module lists are alphabetical and complete', () => {
    const all = glossaryFor('all');
    expect(all.length).toBe(GLOSSARY.length);
    const terms = all.map((e) => e.term);
    expect(terms).toEqual([...terms].sort((a, b) => a.localeCompare(b, 'en')));
    const country = glossaryFor('country');
    for (const id of ['es', 'eo', 'si', 'psi', 'grow', 'upgrade', 'exposed', 'stable', 'memo', 'priority_no_policy', 'ties_today', 'paper_baseline']) {
      expect(country.some((e) => e.id === id), id).toBe(true);
    }
    expect(glossaryFor('sector').some((e) => e.id === 'memo')).toBe(false);
    expect(glossaryFor('strategic').some((e) => e.id === 'endowment_screen')).toBe(true);
  });

  it('every abbreviation is spelled out by its term, as the acronym test helper expects', () => {
    for (const e of GLOSSARY.filter((x) => x.abbr)) {
      const names = ACRONYM_NAMES[e.abbr as string];
      expect(names, e.abbr).toBeTruthy();
      const named = e.term.includes(`(${e.abbr})`) ? e.term : `${e.term} (${e.abbr})`;
      expect(bareAcronymsInText(named), e.id).toEqual([]);
    }
  });

  it('the corner definitions use "high / low energy security", never "secure / insecure" for a sector', () => {
    for (const id of ['grow', 'upgrade', 'exposed', 'stable']) {
      const s = glossaryById.get(id)?.short ?? '';
      expect(s).toMatch(/(high|low) opportunity, (high|low) energy security/i);
      expect(s).not.toMatch(/\binsecure\b/i);
      // the corners of the Security and opportunity chart, told apart from the strategic posture; six
      // stages only, so no three-stage qualifier (2026-10-28)
      expect(s, id).toContain('A corner of the Security and opportunity chart');
      expect(s, id).toMatch(/not (a strategic|the Secure) posture/);
      expect(s, id).not.toMatch(/paper|three-stage|six-stage/i);
    }
    // the corners are Legacy OSI (the paper baseline), each with its closest current choice (design 5)
    expect(glossaryById.get('quadrant')?.more).toContain('they are Legacy OSI (the paper baseline), not the strategic posture');
    const closest = { grow: 'Preserve', upgrade: 'Catch Up', exposed: 'Partner', stable: 'Watch' } as const;
    for (const [id, verb] of Object.entries(closest)) {
      expect(glossaryById.get(id)?.short, id).toContain('(Legacy OSI)');
      expect(glossaryById.get(id)?.more, id).toContain(`Its closest current choice is ${verb}, but the results differ`);
    }
  });

  it('six stages only: no Upstream, Midstream or Downstream and no three-stage qualifier anywhere (design S1)', () => {
    for (const e of GLOSSARY) {
      const text = [e.term, e.short, e.more ?? ''].join(' ');
      expect(text, e.id).not.toMatch(/\b(upstream|midstream|downstream)\b/i);
      expect(text, e.id).not.toMatch(/three[- ]stage|paper's (numbers|three)/i);
    }
    const stage = glossaryById.get('stage');
    for (const s of ['Extraction', 'Refining', 'Components', 'Assembly', 'Deployment', 'Recovery']) expect(stage?.short, s).toContain(s);
    // 11 technologies x 6 stages = 66 sectors; the Sector pages add Semiconductors' three (69)
    expect(GLOSSARY_DEFAULTS).toMatchObject({ n_tech: '11', n_sectors: '66', n_sector_cells: '69' });
    expect(glossaryById.get('sector')?.short).toContain('Batteries — Refining');
    expect(glossaryById.get('sector')?.short).toContain('11 technologies at six stages: 66 sectors');
    expect(glossaryById.get('sector')?.short).toContain('(69 in all)');
    // the allied pick reads every stage but deployment; the endowment screen applies to extraction
    expect(glossaryById.get('allied_pick')?.short).toContain('at any stage but deployment');
    expect(glossaryById.get('allied_pick')?.short).toContain('extraction sectors without recorded resources');
    expect(glossaryById.get('endowment_screen')?.short).toMatch(/^An extraction sector counts/);
    expect(glossaryById.get('populism')?.short).toContain('components and assembly stages');
    // the NEIS baseline is the pipeline's six-stage defaults, not "the paper's numbers" (E3)
    expect(glossaryById.get('paper_baseline')?.term).toBe('NEIS baseline');
    expect(glossaryById.get('paper_baseline')?.short).toContain("the pipeline's own six-stage weights");
  });

  it('the posture terms: every POSTURE_TERMS id has an entry, apart from the UN-vote alignment and the security need', () => {
    for (const id of Object.values(POSTURE_TERMS)) expect(glossaryById.has(id), id).toBe(true);
    expect(glossaryById.get('strategic_need')?.term).toBe('Strategic Need');
    expect(glossaryById.get('industrial_opportunity')?.term).toBe('Industrial Opportunity');
    // the prize the choice reads (2026-10-30) and the market prize it holds as one of three families
    expect(glossaryById.get('strategic_industrial_value')?.term).toBe('Strategic Industrial Value');
    expect(glossaryById.get('strategic_industrial_value')?.short).toContain('the market prize (Industrial Opportunity), industrial-commons value and future-option value');
    expect(glossaryById.get('strategic_industrial_value')?.more).toContain('capped just below the prize bar');
    expect(glossaryById.get('industrial_opportunity')?.short).toMatch(/^The market prize/);
    expect(glossaryById.get('industrial_opportunity')?.more).toContain('One of the three families of the Strategic Industrial Value');
    // the yards in the user's mapping, and the large-yard case
    expect(glossaryById.get('yard')?.short).toContain('Small yard, High fence (Secure); Medium yard, permeable fence (Preserve, Catch Up, Partner, Accelerate); Large yard, Low fence (Watch)');
    expect(glossaryById.get('large_yard_case')?.more).toContain('Market size is never a signal');
    expect(glossaryById.get('policy_alignment')?.more).toContain('Not the same as UN-vote alignment');
    expect(glossaryById.get('strategic_need')?.more).toContain("Not the same as the Strategic Index's security need");
    for (const m of ['sector', 'strategic', 'country'] as const) expect(glossaryFor(m).some((e) => e.id === 'posture'), m).toBe(true);
  });

  it('the canonical framework: Need (two kinds, never combined) -> Prize -> Position -> Choice (six verbs) -> Policy alignment; the legacy indices say so (design 5)', () => {
    const g = (id: string) => glossaryById.get(id) as { term: string; short: string; more?: string };
    expect(g('posture').short).toContain('Secure, Compete (Preserve, Catch Up or Partner) or Deploy (Accelerate or Watch)');
    // the retired need x opportunity rule is gone
    for (const id of ['posture', 'strategic_need', 'industrial_opportunity', 'competitive_position']) {
      const text = `${g(id).short} ${g(id).more ?? ''}`;
      expect(text, id).not.toMatch(/catch up \(both high\)|preserve \(opportunity only\)|consistency check on the posture|Assembly need|Economic Opportunity placed among/);
    }
    expect(g('strategic_need').short).toContain('never combined into one score');
    expect(g('supply_chain_need').short).toMatch(/Producing stages only\.$/);
    expect(g('deployment_need').short).toContain('Deployment & Use only');
    expect(g('deployment_need').more).toContain('never Partner');
    expect(g('deployment_need').more).toContain('not assessed, never zero');
    expect(g('industrial_opportunity').more).toContain('no current capability');
    expect(g('industrial_opportunity').more).toContain('Not the legacy Economic Opportunity index');
    expect(g('competitive_position').more).toMatch(/^Demonstrated capability only/);
    expect(g('pathway').short).toContain('Unknown counts as not credible');
    for (const v of ['aligned', 'under-prioritized', 'over-prioritized', 'wrong treatment (misaligned)', 'deployment under-response', 'mixed']) expect(g('policy_alignment').short).toContain(v);
    expect(g('policy_alignment').more).toContain('insufficient data or low confidence, never a guess');
    expect(g('partner_demand').short).toContain('Deploy · Accelerate is a deployment priority and is not counted');
    // the paper baseline's model is Legacy OSI, with the mapping and why the results differ
    expect(g('legacy_osi').term).toBe('Legacy OSI framework');
    expect(g('legacy_osi').more).toContain('Grow to Preserve, Upgrade to Catch Up, Exposed to Partner, Stable to Watch');
    expect(g('legacy_osi').more).toContain('capability, vulnerability, production, trade, investment, demand, reserves, concentration and deployment');
    for (const id of ['es', 'eo', 'si']) expect(g(id).more, id).toContain('Part of the Legacy OSI framework (the paper baseline)');
    expect(g('paper_baseline').more).toContain('The dials re-weight only the Legacy OSI indices');
  });

  it('the renamed corners (2026-10-28) keep no old id and carry their new names', () => {
    for (const id of ['secure', 'watch']) expect(glossaryById.has(id), id).toBe(false);
    expect(glossaryById.get('exposed')?.term).toBe('Exposed');
    expect(glossaryById.get('stable')?.term).toBe('Stable');
  });
});
