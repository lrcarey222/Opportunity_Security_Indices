import { describe, expect, it } from 'vitest';
import { fillGlossaryText, GLOSSARY, GLOSSARY_DEFAULTS, GLOSSARY_TEMPLATES, glossaryById, glossaryFor, glossaryWith, wordCount } from './glossary.ts';
import { ACRONYM_NAMES, bareAcronymsInText } from '../components/testing/acronyms.ts';

/** The ids of spec_country.md C3.2, in its order (the interface every module codes against). */
const SPEC_IDS = [
  'es', 'eo', 'si', 'need', 'psi', 'fs', 'po', 'dp', 'policy_index', 'policy_basis', 'pams', 'cat', 'rankable', 'imputed', 's_curve', 'percentile',
  'median', 'colour_scale', 'sector', 'stage', 'coalition', 'alignment', 'allied_pick', 'endowment_screen', 'best_five',
  'hhi', 'effective_n', 'rca', 'feasibility', 'matrix', 'mirror', 'basket', 'sub_component', 'net_balance', 'self_sufficiency', 'cmfdi', 'chinese_parent',
  'investment_ties', 'trl', 'unattributed', 'paper_baseline', 'scenario', 'dials', 'hawkishness', 'bullishness', 'populism', 'greenness', 'grow',
  // the corners with the keys secure and watch, shown as Exposed and Stable since 2026-10-28
  'upgrade', 'exposed', 'stable', 'quadrant', 'priority_no_policy', 'ties_today', 'memo',
  // Development & finance raw data (Sector Insights, 2026-10-15)
  'oda', 'dip_position', 'conduit_centre', 'wdi',
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
      // the paper's three-stage corners, told apart from the six-stage posture (2026-10-28)
      expect(s, id).toContain("paper's three-stage chart");
      expect(s, id).toMatch(/not (a|the) six-stage/);
    }
  });

  it('the renamed corners (2026-10-28) keep no old id and carry their new names', () => {
    for (const id of ['secure', 'watch']) expect(glossaryById.has(id), id).toBe(false);
    expect(glossaryById.get('exposed')?.term).toBe('Exposed');
    expect(glossaryById.get('stable')?.term).toBe('Stable');
  });
});
