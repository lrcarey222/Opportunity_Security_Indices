// The relationship thesis card (2026-10-20; strategic task spec s.19) on the synthetic strategic section:
// a pair with a thesis (its heading names the pair; the headline; Complementarity, Priority compacts and AIB
// logic as sub-headings one level down; compacts and economies as links; the logic in the export's order
// with its evidence; the basis note), a pair without one (one line why, the compacts both take part in, no
// logic), no partner (the suggestions, linked), the tools slot, and the cross-lens summary's line on where
// the partner sits. No bare acronym.

import { afterEach, describe, expect, it } from 'vitest';
import { cleanup, render, screen, within } from '@testing-library/react';
import { MemoryRouter } from 'react-router';
import { bareAcronyms } from '../../components/testing/acronyms.ts';
import { strategicOf, type AibStrategic } from '../../aib/contract.ts';
import { readDoc, SYNTHETIC_DIR } from '../../aib/__tests__/fixture.ts';
import { lensNames, lensNone } from '../lenses.ts';
import { PartnershipLenses } from '../PartnershipLenses.tsx';
import { RelationshipThesis, type RelationshipThesisProps } from '../RelationshipThesis.tsx';
import { PARTNERSHIP } from '../../content/vocabulary.ts';

afterEach(cleanup);

const doc = readDoc(SYNTHETIC_DIR);
const s = strategicOf(doc) as AibStrategic;
const name = (x: string) => doc.countries[x] ?? x;

function renderCard(p: Partial<RelationshipThesisProps> & { a: string; b: string | null }) {
  render(
    <MemoryRouter>
      <RelationshipThesis
        id="t"
        strategic={s}
        name={name}
        compactHref={(id) => `/compacts/s/${id}`}
        countryHref={(x) => `/countries/${x}#aib-role`}
        headingLevel={2}
        pairHref={(x) => `?country=AAA&partner=${x}`}
        {...p}
      />
    </MemoryRouter>,
  );
  return screen.getByTestId('relationship-thesis');
}

describe('RelationshipThesis', () => {
  it('a pair with a thesis: the headline, then complementarity, priority compacts and the AIB logic', () => {
    const card = renderCard({ a: 'AAA', b: 'CCC' });
    expect(card.getAttribute('data-state')).toBe('thesis');
    expect(within(card).getByRole('heading', { level: 2 }).textContent).toBe('Relationship thesis: Avalon and Carpania');
    // the guide's question for the pair, then what decides the tools
    expect(within(card).getByTestId('thesis-question').textContent).toBe(`${PARTNERSHIP.thesisHeading} ${PARTNERSHIP.thesisPrompt}`);
    expect(within(card).getByTestId('thesis-headline').textContent).toMatch(/^Avalon and Carpania: a trade, technology and resilience \/ second sourcing relationship/);
    // the three parts, one heading level down
    expect(within(card).getAllByRole('heading', { level: 3 }).map((h) => h.textContent)).toEqual([
      'Complementarity: what each side brings',
      'Priority compacts: where cooperation matters most',
      'AIB logic: what the relationship is about',
    ]);
    // complementarity: each side's Strategic AIB Roles, its name linked to its page
    const roles = within(card).getByTestId('thesis-roles');
    expect(within(roles).getByRole('link', { name: 'Avalon' }).getAttribute('href')).toBe('/countries/AAA#aib-role');
    expect(roles.querySelector('[data-thesis-side="AAA"] dd')?.textContent).toBe('Demand & Market Anchor and Capital & Investment Anchor; secondary: Coordination / Bridge Hub.');
    // priority compacts: linked, each with what both bring
    const list = within(card).getByRole('list', { name: 'What Avalon and Carpania bring to their priority compacts' });
    const items = within(list).getAllByRole('listitem');
    expect(items.map((li) => li.getAttribute('data-compact'))).toEqual(['SC-MAGNETS', 'SC-BATTERIES']);
    expect(within(items[0]).getByRole('link', { name: 'Permanent Magnets & Drive Systems' }).getAttribute('href')).toBe('/compacts/s/SC-MAGNETS');
    expect(items[0].textContent).toContain('Avalon brings');
    expect(items[0].textContent).toContain('Carpania bringsProcessing anchor / specialized technology supplier; largest position in Separated oxides (11% of world exports or production)');
    // the logic, in the export's order, each with its words
    const logic = within(card).getByRole('list', { name: 'What the relationship of Avalon and Carpania is about' });
    expect(within(logic).getAllByRole('listitem').map((li) => li.getAttribute('data-logic'))).toEqual(['trade', 'technology', 'resilience / second sourcing']);
    expect(within(logic).getAllByRole('listitem')[2].textContent).toBe(
      'Resilience / second sourcing. In Battery Materials & Cells, Carpania can second-source for Avalon, the demand anchor, as a diversification partner.',
    );
    expect(within(card).getByTestId('thesis-basis').textContent).toMatch(/no bilateral score\.$/);
    expect(bareAcronyms(document.body)).toEqual([]);
  });

  it('a pair without a thesis: one line why and the compacts both take part in; no logic', () => {
    const card = renderCard({ a: 'AAA', b: 'FFF', headingLevel: 3 });
    expect(card.getAttribute('data-state')).toBe('no-relationship');
    expect(within(card).getByRole('heading', { level: 3 }).textContent).toBe('Relationship thesis: Avalon and Freedonia');
    expect(within(card).getByTestId('thesis-headline').textContent).toMatch(/^The analysis finds no trade or investment relationship between Avalon and Freedonia/);
    expect(within(card).getAllByRole('heading', { level: 4 }).map((h) => h.textContent)).toEqual(['Complementarity: what each side brings', 'Compacts both take part in']);
    expect(within(card).queryByTestId('thesis-logic')).toBeNull();
    expect(bareAcronyms(document.body)).toEqual([]);
  });

  it('no partner: what to do, and the partners with the most priority compacts as links', () => {
    const card = renderCard({ a: 'AAA', b: null });
    expect(card.getAttribute('data-state')).toBe('choose');
    expect(within(card).getByRole('heading', { level: 2 }).textContent).toBe('Relationship thesis');
    const prompt = within(card).getByTestId('thesis-prompt');
    expect(prompt.textContent).toBe('Choose a partner to read the strategic case for the pair. The analysis has 4 for Avalon; those with the most priority compacts: Carpania, Borduria, Elbonia and Dorvalia.');
    expect(within(prompt).getAllByRole('link').map((a) => a.getAttribute('href'))).toEqual(['/?country=AAA&partner=CCC', '/?country=AAA&partner=BBB', '/?country=AAA&partner=EEE', '/?country=AAA&partner=DDD']);
    expect(within(card).queryByTestId('thesis-basis')).toBeNull();
    cleanup();
    // an economy without a thesis says so
    const none = renderCard({ a: 'FFF', b: null });
    expect(within(none).getByTestId('thesis-prompt').textContent).toBe('The analysis writes relationship theses for pairs of allied economies it links by trade or investment; it has none for Freedonia.');
  });

  it('the tools slot sits under the heading', () => {
    const card = renderCard({ a: 'AAA', b: 'BBB', tools: <label>Partner picker</label> });
    expect(within(card).getByText('Partner picker')).toBeTruthy();
  });
});

describe('the cross-lens summary names where the pair’s partner sits', () => {
  const lenses = {
    today: { names: lensNames(['BBB', 'CCC']), content: <p>today</p> },
    osi: { names: lensNone('not in this build') },
    aib: { names: lensNames(['CCC']), content: <p>aib</p> },
  };
  const renderLenses = (pair: string | null) =>
    render(
      <MemoryRouter>
        <PartnershipLenses idPrefix="p" kind="trade" onKind={() => undefined} headingLevel={2} focus="Avalon" noun={{ one: 'partner', many: 'partners' }} name={name} scenarioLabel="Managed Interdependence" lenses={lenses} pair={pair} />
      </MemoryRouter>,
    );

  it('a named partner reads its group; one not named says the lenses name only their leading partners; none without a pair', () => {
    renderLenses('CCC');
    expect(screen.getByTestId('cross-lens-pair').textContent).toBe('Where Carpania sits: A tie today that the AIB keeps.');
    cleanup();
    renderLenses('DDD');
    expect(screen.getByTestId('cross-lens-pair').textContent).toBe('Where Dorvalia sits: not among the partners Today and AIB analysis name here (each names only its leading partners).');
    cleanup();
    renderLenses(null);
    expect(screen.queryByTestId('cross-lens-pair')).toBeNull();
  });
});
