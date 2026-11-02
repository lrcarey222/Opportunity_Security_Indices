// The shared three-lens partnerships component (2026-10-19): the kind control (a radio group of the four
// kinds), three tagged panels in the order Today, OSI ranking, AIB model (its scenario in the tag and the
// heading), the one-line empty state of a lens with no data (never a blank panel), and no bare acronym.
// No cross-lens summary between the control and the panels (gone from every page, 2026-11-02).

import { afterEach, describe, expect, it, vi } from 'vitest';
import { cleanup, fireEvent, render, screen, within } from '@testing-library/react';
import { MemoryRouter } from 'react-router';
import { bareAcronyms } from '../../components/testing/acronyms.ts';
import { PartnershipLenses, type LensPanelSpec } from '../PartnershipLenses.tsx';

afterEach(cleanup);

function renderLenses(lenses: Record<'today' | 'osi' | 'aib', LensPanelSpec>, onKind = vi.fn()) {
  render(
    <MemoryRouter>
      <PartnershipLenses idPrefix="t" kind="trade" onKind={onKind} headingLevel={2} scenarioLabel="Managed Interdependence" lenses={lenses} />
    </MemoryRouter>,
  );
  return onKind;
}

describe('PartnershipLenses', () => {
  it('the kind control and the three tagged panels in order; the AIB lens names its scenario; no cross-lens summary', () => {
    const onKind = renderLenses({
      today: { content: <p>today body</p> },
      osi: { content: <p>osi body</p> },
      aib: { content: <p>aib body</p> },
    });
    const radios = within(screen.getByRole('radiogroup', { name: 'Partnership type' })).getAllByRole('radio');
    expect(radios.map((r) => r.parentElement?.textContent)).toEqual(['Trade', 'Investment', 'Development', 'Geopolitical']);
    expect((radios[0] as HTMLInputElement).checked).toBe(true);
    fireEvent.click(radios[3]);
    expect(onKind).toHaveBeenCalledWith('geopolitical');
    // the panels: Today, OSI ranking, AIB model (h2 here), each tagged
    const panels = screen.getAllByTestId(/^lens-(today|osi|aib)$/);
    expect(panels.map((p) => p.getAttribute('data-lens'))).toEqual(['today', 'osi', 'aib']);
    expect(screen.getByRole('heading', { level: 2, name: 'Today' })).toBeTruthy();
    expect(screen.getByRole('heading', { level: 2, name: 'Partner score ranking' })).toBeTruthy();
    expect(screen.getByRole('heading', { level: 2, name: 'AIB analysis: Managed Interdependence' })).toBeTruthy();
    expect(panels.map((p) => p.querySelector('[data-lens-tag]')?.textContent)).toEqual(['Today · observed', 'Partner score ranking · default weights', 'AIB analysis · Managed Interdependence']);
    expect(within(panels[1]).getByText('osi body')).toBeTruthy();
    // the summary is gone: the control is followed by the three panels only
    expect(screen.getAllByRole('heading').map((h) => h.textContent)).toEqual(['Today', 'Partner score ranking', 'AIB analysis: Managed Interdependence']);
    expect(screen.queryByTestId('cross-lens-summary')).toBeNull();
    expect(screen.queryByText('Across the three lenses')).toBeNull();
    expect(bareAcronyms(document.body)).toEqual([]);
  });

  it('a lens with no data at the focus shows its one line', () => {
    renderLenses({
      today: { empty: 'No aid data for Avalon in this build.' },
      osi: { content: <p>osi</p> },
      aib: { content: <p>loading</p> },
    });
    const today = screen.getByTestId('lens-today');
    expect(within(today).getByTestId('lens-empty').textContent).toBe('No aid data for Avalon in this build.');
    expect(within(screen.getByTestId('lens-osi')).queryByTestId('lens-empty')).toBeNull();
  });

  it('a panel without content or an empty line still says so', () => {
    renderLenses({ today: {}, osi: {}, aib: {} });
    expect(within(screen.getByTestId('lens-aib')).getByTestId('lens-empty').textContent).toBe('No AIB analysis data for this selection.');
    expect(within(screen.getByTestId('lens-today')).getByTestId('lens-empty').textContent).toBe('No Today data for this selection.');
  });
});
