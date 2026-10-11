import { describe, expect, it } from 'vitest';
import { HEADLINE_RULES, type SectorHeadline } from '../sector/contract.ts';
import { fillTemplate, HOME_QUESTIONS, QUESTIONS, questionFact, templateKeys } from './questions.ts';

const NUMBER_FORMATS = new Set(['pct', 'pct1', 'int', 'usd', 'usd_bn', 'ej', null]);
const LABEL_FORMATS = new Set(['cap', null]);

describe('questions (spec_sector 2.8)', () => {
  it('has unique ids, six Home entries and in-app links', () => {
    expect(new Set(QUESTIONS.map((q) => q.id)).size).toBe(QUESTIONS.length);
    expect(HOME_QUESTIONS).toHaveLength(6);
    for (const q of QUESTIONS) expect(q.href.startsWith('/sector')).toBe(true);
  });

  it('templates use only the keys and formats of their headline rule', () => {
    for (const q of QUESTIONS) {
      expect(q.headline_id === null).toBe(q.fact === null);
      if (!q.headline_id || !q.fact) continue;
      const rule = HEADLINE_RULES[q.headline_id];
      for (const { key, format } of templateKeys(q.fact)) {
        if (key in rule.values) expect(NUMBER_FORMATS.has(format), `${q.id}: {${key}|${format}}`).toBe(true);
        else if (key in rule.labels) expect(LABEL_FORMATS.has(format), `${q.id}: {${key}|${format}}`).toBe(true);
        else throw new Error(`${q.id}: {${key}} is not a key of ${rule.rule}`);
      }
    }
  });

  it('fills a template from manifest values', () => {
    expect(fillTemplate("{geo_name} refines {share|pct} of the world's {item_text}", { share: 0.7367 }, { geo_name: 'China', item_text: 'lithium' })).toBe(
      "China refines 74% of the world's lithium",
    );
    expect(fillTemplate('{a|usd} {b|usd} {c|usd_bn} {d|ej} {e|pct1} {f|int} {g|cap}', { a: 1.006e12, b: 2.027e11, c: 1875.2, d: 6.8123, e: 0.4371, f: 15598 }, { g: 'cobalt' })).toBe(
      'USD 1.01 tn USD 203 bn USD 1,875 bn 6.81 EJ 43.7% 15,598 Cobalt',
    );
  });

  it('yields no fact for a missing headline, another rule version or an unresolved key', () => {
    const q = QUESTIONS.find((x) => x.id === 'supply')!;
    const h: SectorHeadline = {
      id: 'supply',
      template: HEADLINE_RULES.supply.rule,
      values: { share: 0.944, value_kt: 2139, total_kt: 2266 },
      labels: { geo: 'CHN', geo_name: 'China', item: 'graphite', item_text: 'battery-grade graphite' },
      source: 'IEA Critical Minerals Dataset 2026',
      year: '2025',
      href: '/sector/minerals/graphite',
    };
    expect(questionFact(q, [h])).toEqual({
      text: "China refines 94% of the world's battery-grade graphite",
      source: 'IEA Critical Minerals Dataset 2026',
      year: '2025',
      href: '/sector/minerals/graphite',
    });
    expect(questionFact(q, [])).toBeNull();
    expect(questionFact(q, null)).toBeNull();
    expect(questionFact(q, [{ ...h, template: 'supply@0' }])).toBeNull();
    expect(questionFact(q, [{ ...h, labels: { geo: 'CHN', item: 'graphite' } }])).toBeNull();
    expect(questionFact(q, [{ ...h, values: { share: Number.NaN } }])).toBeNull();
  });
});
