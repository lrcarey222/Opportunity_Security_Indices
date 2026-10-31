// The glossary components (spec_country.md C3.3): InfoTip opens a non-modal popover with the short
// definition and a link to the glossary, closes on Escape (focus back on the button), outside click and
// blur; Term spells the name out with its tip; GlossaryList filters and anchors; the /glossary page
// filters by module and keeps a linked entry visible; a manifest's newer numbers fill the placeholders.

import { afterAll, afterEach, beforeAll, describe, expect, it } from 'vitest';
import { act, cleanup, fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import { createMemoryRouter, RouterProvider } from 'react-router';
import type { ReactNode } from 'react';
import { InfoTip, glossaryAnchor } from './InfoTip.tsx';
import { Term } from './Term.tsx';
import { GlossaryList } from './GlossaryList.tsx';
import { GlossaryVarsProvider } from './glossaryVars.tsx';
import { bareAcronyms } from './testing/acronyms.ts';
import { GLOSSARY, glossaryById, glossaryFor } from '../content/glossary.ts';
import GlossaryView from '../shell/GlossaryView.tsx';

afterEach(cleanup);

// the data router builds a fetch Request with jsdom's AbortSignal, which Node's Request rejects; these
// routes have no loaders, so the tests drop the signal (as app.test.tsx does)
const NodeRequest = globalThis.Request;
beforeAll(() => {
  globalThis.Request = class extends NodeRequest {
    constructor(input: RequestInfo | URL, init?: RequestInit) {
      const rest = { ...(init ?? {}) };
      delete rest.signal;
      super(input, rest);
    }
  } as typeof Request;
});
afterAll(() => {
  globalThis.Request = NodeRequest;
});

function withRouter(node: ReactNode, path = '/') {
  const router = createMemoryRouter([{ path: '*', element: node }], { initialEntries: [path] });
  render(<RouterProvider router={router} />);
  return router;
}

describe('InfoTip', () => {
  it('toggles a popover with the term, the short text and the glossary link', () => {
    withRouter(
      <p>
        Scores <InfoTip id="es" /> run from 0 to 1. <button type="button">elsewhere</button>
      </p>,
    );
    const btn = screen.getByRole('button', { name: 'What is “Energy Security”?' });
    expect(btn.getAttribute('aria-expanded')).toBe('false');
    expect(screen.queryByRole('dialog')).toBeNull();
    fireEvent.click(btn);
    expect(btn.getAttribute('aria-expanded')).toBe('true');
    const pop = screen.getByRole('dialog', { name: 'Energy Security' });
    expect(pop.textContent).toContain(glossaryById.get('es')?.short as string);
    const more = within(pop).getByRole('link', { name: /More in the glossary/ });
    expect(more.getAttribute('href')).toBe(`/glossary#${glossaryAnchor('es')}`);
    expect(btn.getAttribute('aria-controls')).toBe(pop.id);
    // click again closes
    fireEvent.click(btn);
    expect(screen.queryByRole('dialog')).toBeNull();
  });

  it('Escape closes and returns focus to the button; an outside press closes', () => {
    withRouter(
      <div>
        <InfoTip id="psi" />
        <button type="button">outside</button>
      </div>,
    );
    const btn = screen.getByRole('button', { name: 'What is “Partnership Strength Index”?' });
    fireEvent.click(btn);
    expect(screen.getByRole('dialog')).toBeTruthy();
    fireEvent.keyDown(document, { key: 'Escape' });
    expect(screen.queryByRole('dialog')).toBeNull();
    expect(document.activeElement).toBe(btn);
    fireEvent.click(btn);
    fireEvent.pointerDown(screen.getByRole('button', { name: 'outside' }));
    expect(screen.queryByRole('dialog')).toBeNull();
  });

  it('names the term as the page does, and renders nothing for an unknown id', () => {
    withRouter(
      <p>
        <InfoTip id="hhi" term="the concentration index" />
        <InfoTip id="no_such_term" />
      </p>,
    );
    expect(screen.getByRole('button', { name: 'What is “the concentration index”?' })).toBeTruthy();
    expect(screen.getAllByRole('button')).toHaveLength(1);
  });
});

describe('Term', () => {
  it('spells the term out with its tip; plain drops the tip; children replace the words', () => {
    withRouter(
      <p>
        <Term id="si" /> · <Term id="es" plain /> · <Term id="eo">economic opportunity</Term>
      </p>,
    );
    const p = screen.getByText(/Strategic Index/).closest('p') as HTMLElement;
    expect(p.textContent).toContain('Strategic Index');
    expect(p.textContent).toContain('Energy Security');
    expect(p.textContent).toContain('economic opportunity');
    const names = screen.getAllByRole('button').map((b) => b.getAttribute('aria-label'));
    expect(names).toEqual(['What is “Strategic Index”?', 'What is “economic opportunity”?']);
  });
});

describe('GlossaryList', () => {
  it('lists a module alphabetically with anchors and abbreviations in parentheses', () => {
    withRouter(<GlossaryList module="country" />);
    const list = screen.getByTestId('glossary-list');
    const terms = [...list.querySelectorAll('dt')].map((d) => d.textContent ?? '');
    expect(terms.length).toBe(glossaryFor('country').length);
    expect(terms.some((t) => t === 'Energy Security(ES)')).toBe(true);
    expect(list.querySelector(`#${glossaryAnchor('grow')}`)).toBeTruthy();
    expect(list.querySelector(`#${glossaryAnchor('endowment_screen')}`)).toBeNull();
    // related terms link within the page
    const es = list.querySelector(`#${glossaryAnchor('es')}`) as HTMLElement;
    expect(within(es).getByRole('link', { name: 'Economic Opportunity' }).getAttribute('href')).toBe(`#${glossaryAnchor('eo')}`);
  });

  it('shows exactly the ids asked for, in order; a provider fills newer numbers', () => {
    withRouter(
      <GlossaryVarsProvider vars={{ n_core: '32' }}>
        <GlossaryList ids={['coalition', 'es']} anchors={false} />
      </GlossaryVarsProvider>,
    );
    const list = screen.getByTestId('glossary-list');
    expect([...list.querySelectorAll('dt')].map((d) => d.textContent)).toEqual(['Allied coalition', 'Energy Security(ES)']);
    expect(list.textContent).toContain('32 core members');
    expect(list.querySelector('[id]')).toBeNull();
  });
});

describe('/glossary', () => {
  it('lists every term, filters by module in the URL, and has no bare acronym in its chrome', async () => {
    const router = withRouter(<GlossaryView />, '/glossary');
    expect(await screen.findByRole('heading', { level: 1, name: 'Glossary' })).toBeTruthy();
    expect(screen.getByText(`${GLOSSARY.length} terms`)).toBeTruthy();
    fireEvent.click(screen.getByRole('radio', { name: 'Country' }));
    await waitFor(() => expect(router.state.location.search).toBe('?module=country'));
    expect(screen.getByText(`${glossaryFor('country').length} terms`)).toBeTruthy();
    fireEvent.click(screen.getByRole('radio', { name: 'All' }));
    await waitFor(() => expect(router.state.location.search).toBe(''));
    expect(bareAcronyms(document.body)).toEqual([]);
  });

  it('a link to an entry outside the filter shows every module, so the entry is there', async () => {
    let scrolled: string | null = null;
    const proto = HTMLElement.prototype as unknown as { scrollIntoView?: () => void };
    const had = proto.scrollIntoView;
    proto.scrollIntoView = function (this: HTMLElement) {
      scrolled = this.id;
    };
    try {
      withRouter(<GlossaryView />, `/glossary?module=country#${glossaryAnchor('endowment_screen')}`);
      expect(await screen.findByRole('heading', { level: 1, name: 'Glossary' })).toBeTruthy();
      expect(document.getElementById(glossaryAnchor('endowment_screen'))).toBeTruthy();
      expect((screen.getByRole('radio', { name: 'All' }) as HTMLInputElement).checked).toBe(true);
      await act(async () => undefined);
      expect(scrolled).toBe(glossaryAnchor('endowment_screen'));
    } finally {
      proto.scrollIntoView = had;
    }
  });
});
