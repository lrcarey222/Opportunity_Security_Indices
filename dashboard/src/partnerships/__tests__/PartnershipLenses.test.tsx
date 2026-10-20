// The shared three-lens partnerships component (2026-10-19): the kind control (a radio group of the four
// kinds), the cross-lens summary (readings in plain words, names linked where the page gives a link, "and N
// more"), three tagged panels in the order Today, OSI recommends, AIB model (its scenario in the tag and the
// heading), the one-line empty state of a lens with no data (never a blank panel), and no bare acronym.

import { afterEach, describe, expect, it, vi } from 'vitest';
import { cleanup, fireEvent, render, screen, within } from '@testing-library/react';
import { MemoryRouter } from 'react-router';
import { bareAcronyms } from '../../components/testing/acronyms.ts';
import { LENS_LOADING, lensNames, lensNone } from '../lenses.ts';
import { CrossLensSummary, PartnershipLenses, SUMMARY_NAMES, type LensPanelSpec } from '../PartnershipLenses.tsx';

afterEach(cleanup);

const NAMES: Record<string, string> = { AAA: 'Avalon', BBB: 'Borduria', CCC: 'Carpania', DDD: 'Dorvalia', EEE: 'Elbonia' };
const name = (x: string) => NAMES[x] ?? x;

function renderLenses(lenses: Record<'today' | 'osi' | 'aib', LensPanelSpec>, onKind = vi.fn()) {
  render(
    <MemoryRouter>
      <PartnershipLenses
        idPrefix="t"
        kind="trade"
        onKind={onKind}
        headingLevel={2}
        focus="Avalon"
        noun={{ one: 'partner', many: 'partners' }}
        name={name}
        partnerHref={(x) => (x === 'EEE' ? null : `/countries/${x}`)}
        scenarioLabel="Managed Interdependence"
        lenses={lenses}
      />
    </MemoryRouter>,
  );
  return onKind;
}

describe('PartnershipLenses', () => {
  it('the kind control, the summary and the three tagged panels in order; the AIB lens names its scenario', () => {
    const onKind = renderLenses({
      today: { names: lensNames(['BBB', 'CCC']), content: <p>today body</p> },
      osi: { names: lensNames(['CCC', 'DDD']), content: <p>osi body</p> },
      aib: { names: lensNames(['CCC', 'EEE']), content: <p>aib body</p> },
    });
    const radios = within(screen.getByRole('radiogroup', { name: 'Partnership type' })).getAllByRole('radio');
    expect(radios.map((r) => r.parentElement?.textContent)).toEqual(['Trade', 'Investment', 'Development', 'Geopolitical']);
    expect((radios[0] as HTMLInputElement).checked).toBe(true);
    fireEvent.click(radios[3]);
    expect(onKind).toHaveBeenCalledWith('geopolitical');
    // the panels: Today, OSI recommends, AIB model (h2 here), each tagged
    const panels = screen.getAllByTestId(/^lens-(today|osi|aib)$/);
    expect(panels.map((p) => p.getAttribute('data-lens'))).toEqual(['today', 'osi', 'aib']);
    expect(screen.getByRole('heading', { level: 2, name: 'Today' })).toBeTruthy();
    expect(screen.getByRole('heading', { level: 2, name: 'OSI recommends' })).toBeTruthy();
    expect(screen.getByRole('heading', { level: 2, name: 'AIB model: Managed Interdependence' })).toBeTruthy();
    expect(panels.map((p) => p.querySelector('[data-lens-tag]')?.textContent)).toEqual(['Today · observed', 'OSI recommends · NEIS defaults', 'AIB model · Managed Interdependence']);
    expect(within(panels[1]).getByText('osi body')).toBeTruthy();
    // the summary: Carpania is named by all three
    const sum = screen.getByTestId('cross-lens-summary');
    expect(within(sum).getByRole('heading', { level: 2 }).textContent).toBe('Where the lenses agree: trade, Avalon');
    expect(within(sum).getByTestId('cross-lens-line').textContent).toBe('The three lenses name 4 partners: 1 by all three and 3 by one.');
    const items = within(sum).getAllByRole('listitem');
    expect(items[0].textContent).toBe('A tie today that both OSI and the AIB keep (1): Carpania');
    expect(within(items[0]).getByRole('link', { name: 'Carpania' }).getAttribute('href')).toBe('/countries/CCC');
    // a name without a page is text
    const aibOnly = items.find((li) => li.getAttribute('data-reading') === 'aib')!;
    expect(aibOnly.textContent).toBe('Chosen by the AIB, not a tie today and not recommended by OSI (1): Elbonia');
    expect(within(aibOnly).queryByRole('link')).toBeNull();
    expect(bareAcronyms(document.body)).toEqual([]);
  });

  it('a lens with no data at the focus shows one line, and the summary leaves it out and says why', () => {
    renderLenses({
      today: { names: lensNone('no aid data for Avalon'), empty: 'No aid data for Avalon in this build.' },
      osi: { names: lensNames(['BBB']), content: <p>osi</p> },
      aib: { names: LENS_LOADING, content: <p>loading</p> },
    });
    const today = screen.getByTestId('lens-today');
    expect(within(today).getByTestId('lens-empty').textContent).toBe('No aid data for Avalon in this build.');
    const sum = screen.getByTestId('cross-lens-summary');
    expect(within(sum).getByTestId('cross-lens-line').textContent).toBe('Only OSI recommends has partners here, so there is nothing to compare it with yet: Borduria.');
    expect(within(sum).getByText('Still loading: AIB model.')).toBeTruthy();
    expect(within(sum).getByTestId('cross-lens-absent-today').textContent).toBe('Today: no aid data for Avalon; left out of the comparison.');
  });

  it('a panel without content or an empty line still says so', () => {
    renderLenses({ today: { names: lensNames([]) }, osi: { names: lensNames([]) }, aib: { names: lensNames([]) } });
    expect(within(screen.getByTestId('lens-aib')).getByTestId('lens-empty').textContent).toBe('No AIB model data for this selection.');
    expect(screen.getByTestId('cross-lens-line').textContent).toBe('The three lenses name 0 partners.');
  });

  it('a long reading lists the first names and "and N more"', () => {
    const keys = ['AAA', 'BBB', 'CCC', 'DDD', 'EEE', 'FFF', 'GGG', 'HHH'];
    render(
      <MemoryRouter>
        <CrossLensSummary id="s" level={3} kind="geopolitical" focus="every economy" noun={{ one: 'economy', many: 'economies' }} name={name} lenses={{ today: lensNames(keys, 40), osi: lensNames(keys, 40), aib: lensNames([], 40) }} />
      </MemoryRouter>,
    );
    const li = screen.getAllByRole('listitem')[0];
    expect(li.textContent).toMatch(new RegExp(`\\(8\\): .* and ${keys.length - SUMMARY_NAMES} more$`));
    expect(screen.getByRole('heading', { level: 3 }).textContent).toBe('Where the lenses agree: geopolitical, every economy');
  });
});
