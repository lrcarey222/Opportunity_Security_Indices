// The three posture cards on the fixture bundle (design L1, 2026-10-28): the Country posture matrix (marks,
// borders, Watch in the tooltip only, the drawer with partners, the Secure rationale and restraint, trade
// freshness), the Sector scatter (quadrant labels, Secure at true coordinates, click opens the profile,
// the checked ?pnode node, a node fixed by the page) and the AIB division matrix (holders, gaps, the
// header metrics, the AIB nodes of a cell). Each card is a ChartCard with an h3 (the page's Section has
// the h2), the takeaway as its sub and summary, a table twin and a CSV with the posture provenance; the
// counts and the AIB metrics are separate pieces for the pages' tiles. A card can still stand alone (an h2
// with the counts or metrics inside: headingLevel 2 with showCounts / stats); the hero wrappers that did
// that were removed on 2026-10-29, once the Country, Sector and AIB pages mounted the cards in Sections.

import { afterAll, afterEach, beforeAll, beforeEach, describe, expect, it, vi } from 'vitest';
import { cleanup, fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import { createMemoryRouter, RouterProvider } from 'react-router';
import { bareAcronyms } from '../../components/testing/acronyms.ts';
import { AibDivisionCard, AIB_DIVISION_TITLE, indexNodesByCell } from '../AibDivisionCard.tsx';
import { PostureBasisTag } from '../basis.tsx';
import { COUNTRY_POSTURE_TITLE, CountryPostureCard } from '../CountryPostureCard.tsx';
import { AibDivisionStats, CountryPostureTile } from '../PostureCounts.tsx';
import { SECTOR_POSTURE_TITLE, SectorPostureCard } from '../SectorPostureCard.tsx';
import { setPostureStore } from '../store.ts';
import { fixtureCore, fixtureStore, withoutCell } from './fixture.ts';

function at(path: string, element: React.ReactNode) {
  const router = createMemoryRouter(
    [
      { path: '/sector', element },
      { path: '/countries/:iso3', element: <p>profile page</p> },
      { path: '*', element },
    ],
    { initialEntries: [path] },
  );
  render(<RouterProvider router={router} />);
  return router;
}

// jsdom's AbortSignal is not undici's: the router's navigation Request drops the signal (as the AIB view tests do).
const NodeRequest = globalThis.Request;
beforeAll(() => {
  globalThis.Request = class extends NodeRequest {
    constructor(input: RequestInfo | URL, init?: RequestInit) {
      const rest = { ...(init ?? {}) };
      delete (rest as { signal?: unknown }).signal;
      super(input, rest);
    }
  } as typeof Request;
});
afterAll(() => {
  globalThis.Request = NodeRequest;
});
beforeEach(() => setPostureStore(fixtureStore()));
afterEach(() => {
  cleanup();
  setPostureStore(null);
});

/** Click "Download CSV" and read the file the card saves (jsdom's Blob has no text()). */
async function downloadCsv(scope: HTMLElement): Promise<string[]> {
  const blobs: Blob[] = [];
  const cu = URL.createObjectURL;
  const ru = URL.revokeObjectURL;
  URL.createObjectURL = ((b: Blob) => (blobs.push(b), 'blob:test')) as typeof URL.createObjectURL;
  URL.revokeObjectURL = (() => undefined) as typeof URL.revokeObjectURL;
  const click = vi.spyOn(HTMLAnchorElement.prototype, 'click').mockImplementation(() => undefined);
  try {
    fireEvent.click(within(scope).getByRole('button', { name: 'Download CSV' }));
    await waitFor(() => expect(blobs).toHaveLength(1));
    const bytes = await new Promise<ArrayBuffer>((res, rej) => {
      const fr = new FileReader();
      fr.onload = () => res(fr.result as ArrayBuffer);
      fr.onerror = () => rej(fr.error);
      fr.readAsArrayBuffer(blobs[0]);
    });
    // UTF-8 with a BOM, as every CSV of the dashboard (kept: ignoreBOM)
    const text = new TextDecoder('utf-8', { ignoreBOM: true }).decode(bytes);
    expect(text.startsWith('﻿')).toBe(true);
    return text.replace(/^﻿/, '').split('\r\n');
  } finally {
    URL.createObjectURL = cu;
    URL.revokeObjectURL = ru ?? ((() => undefined) as typeof URL.revokeObjectURL);
    click.mockRestore();
  }
}

const SOURCE = '"Opportunity Security Indices, strategic position framework"';
const noAcronyms = () => expect(bareAcronyms(document.body)).toEqual([]);

describe('the Country posture card', () => {
  it('draws every technology x six-stage node with its posture, mark and alignment', async () => {
    at('/', <CountryPostureCard iso3="AAA" name="Aland" />);
    const m = await screen.findByTestId('posture-matrix');
    const cells = within(m).getAllByRole('button');
    expect(cells).toHaveLength(12);
    expect(within(m).getAllByRole('columnheader').map((h) => h.textContent)).toEqual(['Technology', 'ExtExtraction', 'RefRefining', 'ComComponents', 'AsmAssembly', 'DepDeployment', 'RecRecovery']);
    const sec = within(m).getByRole('button', { name: /^Batteries Refining: Secure; Coercive chokepoint/ });
    expect(sec.textContent).toContain('K');
    expect(sec.getAttribute('aria-label')).toContain('policy under-prioritized');
    const part = within(m).getByRole('button', { name: /^Batteries Components: Compete · Partner/ });
    expect(part.getAttribute('data-mode')).toBe('partner');
    const pres = within(m).getByRole('button', { name: /^Solar Assembly: Compete · Preserve/ });
    expect(pres.textContent).toContain('P');
    expect(Number((pres as HTMLElement).style.opacity)).toBeLessThan(1); // weak evidence fades
    // Watch: named in the accessible name and tooltip, never as a mark
    const w = within(m).getByRole('button', { name: /^Solar Extraction: Deploy; Watch/ });
    expect(w.textContent?.trim()).toBe('');
    fireEvent.mouseEnter(w);
    expect(screen.getByRole('tooltip').textContent).toContain('Watch');
  });

  it('is a ChartCard for a page Section: an h3, the takeaway as its sub and summary, the freshness and Deployment notes, no counts', async () => {
    at('/', <CountryPostureCard iso3="AAA" name="Aland" />);
    const takeaway = await screen.findByTestId('posture-takeaway');
    expect(takeaway.textContent).toMatch(/^Aland should secure 1 node/);
    expect(screen.getByRole('heading', { level: 3 }).textContent).toBe(COUNTRY_POSTURE_TITLE);
    expect(screen.queryAllByRole('heading', { level: 2 })).toHaveLength(0);
    expect(screen.getByRole('region', { name: COUNTRY_POSTURE_TITLE })).toBeTruthy();
    expect(screen.getByRole('figure').getAttribute('aria-label')).toBe(takeaway.textContent);
    expect(screen.getByTestId('trade-freshness').textContent).toBe('Structural trade benchmark: 2022–24 · Current signal through: Aug 2026');
    expect(screen.getByTestId('country-posture-hero').textContent).toContain('Deployment is where the technology is used');
    // the counts are the page's tile, not the card's
    expect(screen.queryByLabelText('Posture counts')).toBeNull();
    noAcronyms();
  });

  it('opens the drawer: partners for a Partner node, the rationale for Secure, restraint for Deploy', async () => {
    at('/', <CountryPostureCard iso3="AAA" name="Aland" />);
    const m = await screen.findByTestId('posture-matrix');
    fireEvent.click(within(m).getByRole('button', { name: /^Batteries Components/ }));
    const d = await screen.findByTestId('cell-drawer');
    await waitFor(() => expect(d.textContent).toContain('Trusted partners'));
    expect(d.textContent).toContain('Bland');
    expect(d.textContent).toContain('partner despite capability');
    expect(d.textContent).toMatch(/Data through Aug 2026/);
    // no Sector link unless the page builds one
    expect(screen.queryByTestId('cell-drawer-sector-link')).toBeNull();
    fireEvent.click(within(m).getByRole('button', { name: /^Batteries Refining/ }));
    await waitFor(() => expect(screen.getByTestId('cell-drawer').textContent).toContain('Coercive chokepoint'));
    expect(screen.getByTestId('cell-drawer').textContent).toContain('Secure does not mean reshoring');
    expect(screen.getByTestId('cell-drawer').textContent).toContain('Current momentum unavailable');
    fireEvent.click(within(m).getByRole('button', { name: /^Solar Extraction/ }));
    await waitFor(() => expect(screen.getByTestId('cell-drawer').textContent).toContain('Why restraint'));
    fireEvent.keyDown(window, { key: 'Escape' });
    await waitFor(() => expect(screen.queryByTestId('cell-drawer')).toBeNull());
  });

  it('the table twin lists all 12 nodes, an unclassified one kept, and a row opens the drawer', async () => {
    // Aland's Solar Recovery has no posture
    setPostureStore(fixtureStore({ core: withoutCell(fixtureCore(), 0, 1, 5) }));
    at('/', <CountryPostureCard iso3="AAA" name="Aland" />);
    await screen.findByTestId('posture-matrix');
    expect(screen.getByRole('gridcell', { name: 'Solar Recovery: not classified' })).toBeTruthy();
    fireEvent.click(screen.getByRole('button', { name: 'Table' }));
    const table = screen.getByRole('table', { name: 'Aland: strategic posture by technology and stage' });
    expect(within(table).getAllByRole('columnheader').map((h) => h.textContent?.replace(/[▲▼]/g, ''))).toEqual([
      'Technology', 'Stage', 'Posture', 'Compete mode or Secure rationale', 'Quadrant', 'Strategic Need', 'Industrial Opportunity', 'Competitive Position', 'World supply share',
      'Policy alignment', 'Evidence', 'Trade momentum', 'First trusted partner', 'Borderline on',
    ]);
    const rows = within(table).getAllByRole('row').slice(1) as HTMLTableRowElement[];
    expect(rows).toHaveLength(12);
    expect(rows[1].textContent).toContain('Coercive chokepoint');
    expect(rows[11].textContent).toMatch(/^SolarRecoveryNot classified/);
    noAcronyms();
    fireEvent.click(rows[2]);
    await waitFor(() => expect(screen.getByTestId('cell-drawer').textContent).toContain('Trusted partners'));
    expect(rows[2].getAttribute('aria-selected')).toBe('true');
    // back to the chart: the drawer stays open on the same node
    fireEvent.click(screen.getByRole('button', { name: 'Chart' }));
    expect(screen.getByRole('button', { name: /^Batteries Components/ }).getAttribute('aria-pressed')).toBe('true');
  });

  it('the CSV: one row per node with source, as-of date and commit last', async () => {
    setPostureStore(fixtureStore({ core: withoutCell(fixtureCore(), 0, 1, 5) }));
    at('/', <CountryPostureCard iso3="AAA" name="Aland" />);
    const card = await screen.findByTestId('country-posture-hero');
    await screen.findByTestId('posture-matrix');
    const lines = await downloadCsv(card);
    expect(lines[0].split(',').slice(-3)).toEqual(['source', 'as_of', 'git_commit']);
    expect(lines[0].split(',').slice(0, 7)).toEqual(['country', 'iso3', 'coalition_tier', 'technology', 'stage', 'classified', 'posture']);
    expect(lines).toHaveLength(14);
    expect(lines[2]).toBe(`Aland,AAA,core,Batteries,Refining,TRUE,Secure,,Coercive chokepoint,Partner,0.95,0.4,0.3,,Under-prioritized,high,,,,,,,${SOURCE},2026-10-28,abc1234`);
    expect(lines[12].startsWith('Aland,AAA,core,Solar,Recovery,FALSE,,')).toBe(true);
  });

  it('a page can hold the selection (the URL) and link the drawer to the Sector page', async () => {
    const onSelect = vi.fn();
    at('/', <CountryPostureCard iso3="AAA" name="Aland" selected="Batteries|Components" onSelect={onSelect} sectorHref={(t, s) => `/sector/${t.toLowerCase()}/${s.toLowerCase()}`} />);
    const d = await screen.findByTestId('cell-drawer');
    await waitFor(() => expect(d.textContent).toContain('Trusted partners'));
    expect(screen.getByTestId('cell-drawer-sector-link').getAttribute('href')).toBe('/sector/batteries/components');
    fireEvent.click(screen.getByRole('button', { name: /^Batteries Refining/ }));
    expect(onSelect).toHaveBeenLastCalledWith('Batteries|Refining');
    fireEvent.click(screen.getByRole('button', { name: 'Close the detail' }));
    expect(onSelect).toHaveBeenLastCalledWith(null);
  });

  it('serves a renamed fixture economy, for the page route tests', async () => {
    setPostureStore(fixtureStore({ economies: { AAA: { iso3: 'USA', name: 'United States' } } }));
    at('/', <CountryPostureCard iso3="USA" name="The United States" />);
    expect((await screen.findByTestId('posture-takeaway')).textContent).toMatch(/^The United States should secure 1 node/);
    fireEvent.click(screen.getByRole('button', { name: /^Batteries Components/ }));
    await waitFor(() => expect(screen.getByTestId('cell-drawer').textContent).toContain('United States'));
  });

  it('an economy without a posture: a notice, no card', async () => {
    at('/', <CountryPostureCard iso3="ZZZ" name="Zland" />);
    expect((await screen.findByText('No posture')).closest('[role="status"]')?.textContent).toContain('Zland has no classified supply-chain nodes');
    expect(screen.queryByRole('heading')).toBeNull();
  });

  it('without a momentum window it says why there is no current signal', async () => {
    setPostureStore(fixtureStore({ momentum: false }));
    at('/', <CountryPostureCard iso3="AAA" name="Aland" />);
    expect((await screen.findByTestId('trade-freshness')).textContent).toContain('Current momentum unavailable — incomplete matched-period reporting');
  });
});

describe("the page's posture pieces", () => {
  it('the Country tile carries the counts and the Compete mix, and links to the section', async () => {
    at('/', <CountryPostureTile iso3="AAA" link={{ href: '#posture', label: 'Strategic posture' }} />);
    const counts = await screen.findByTestId('posture-tile-counts');
    expect(counts.textContent).toContain('Secure 1 · Compete 2 · Deploy 9');
    expect(document.body.textContent).toContain('STRATEGIC POSTURE');
    expect(document.body.textContent).toContain('Compete: 1 preserve · 0 catch up · 1 partner');
    expect(screen.getByRole('link', { name: 'Strategic posture' }).getAttribute('href')).toBe('#posture');
  });
  it('the tile without a posture or without the data', async () => {
    at('/', <CountryPostureTile iso3="ZZZ" />);
    expect(await screen.findByText('No posture')).toBeTruthy();
    cleanup();
    setPostureStore(fixtureStore({ core: { schema: 'nope' } }));
    at('/', <CountryPostureTile iso3="AAA" />);
    expect(await screen.findByText('Not available')).toBeTruthy();
  });
  it('the basis tag names the strategic position framework', () => {
    render(<PostureBasisTag />);
    const tag = document.querySelector('[data-basis="posture"]') as HTMLElement;
    expect(tag.textContent).toBe('Strategic position framework · fixed thresholds; the dials and the scenario do not change it');
    cleanup();
    render(<PostureBasisTag note={null} />);
    expect(document.querySelector('[data-basis="posture"]')?.textContent).toBe('Strategic position framework');
  });
});

describe('the Sector posture card', () => {
  it('places countries by opportunity and need, keeps Secure at its true coordinates, and opens the posture section of the profile on click', async () => {
    const router = at('/sector?pnode=Batteries|Refining', <SectorPostureCard />);
    const sc = await screen.findByTestId('posture-scatter');
    for (const q of ['CATCH UP', 'PRESERVE', 'PARTNER', 'WATCH · DEPLOY']) expect(sc.textContent).toContain(q);
    const secure = sc.querySelector('path[data-posture="S"]') as SVGPathElement;
    expect(secure).not.toBeNull();
    // need 0.95, opportunity 0.40 -> in the partner quadrant, drawn as an outlined diamond
    expect(secure.getAttribute('fill')).toMatch(/rgb/);
    expect(secure.getAttribute('aria-label')).toMatch(/Aland: Secure, need 95%, opportunity 40%/);
    expect(screen.getByRole('heading', { level: 3 }).textContent).toBe(SECTOR_POSTURE_TITLE);
    // the reading line sits under the takeaway, as on every Sector chart card (C3.6)
    const reading = screen.getByTestId('posture-reading');
    expect(reading.className).toBe('mt-1 block text-paper-dim');
    // (a glossary Term ends with a no-break space and its tip button, so a space precedes the punctuation)
    expect(reading.textContent).toMatch(/^Each point is one economy\. Across: Industrial Opportunity\s?, .*up: Strategic Need\s?\. Outlined diamonds are Secure.*Point size is Competitive Position\s?\.$/);
    fireEvent.click(secure);
    await waitFor(() => expect(router.state.location.pathname).toBe('/countries/AAA'));
    expect(router.state.location.hash).toBe('#posture');
  });

  it('the node selector writes the URL and the takeaway follows it; Deployment is a disabled choice', async () => {
    const router = at('/sector?pnode=Batteries|Components', <SectorPostureCard />);
    expect((await screen.findByTestId('posture-takeaway')).textContent).toMatch(/^In Batteries components/);
    const stage = screen.getByLabelText('Stage') as HTMLSelectElement;
    expect([...stage.options].map((o) => [o.value, o.disabled])).toEqual([
      ['Extraction', false], ['Refining', false], ['Components', false], ['Assembly', false], ['Deployment', true], ['Recovery', false],
    ]);
    fireEvent.change(screen.getByLabelText('Technology'), { target: { value: 'Solar' } });
    await waitFor(() => expect(router.state.location.search).toContain('pnode=Solar%7CComponents'));
    expect(screen.queryByTestId('posture-node-ignored')).toBeNull();
  });

  it('an old three-stage node is reported as ignored, and the chart, selects and takeaway show the same checked node', async () => {
    at('/sector?pnode=Batteries|Midstream', <SectorPostureCard />);
    const ignored = await screen.findByTestId('posture-node-ignored');
    expect(ignored.textContent).toContain('Ignored in the link');
    expect(ignored.textContent).toContain('pnode=Batteries|Midstream (Midstream is no longer a stage: it is now mostly Components and Assembly; showing Batteries — Refining)');
    expect(screen.getByTestId('posture-takeaway').textContent).toMatch(/^In Batteries refining, 1 country must secure it/);
    expect((screen.getByLabelText('Stage') as HTMLSelectElement).value).toBe('Refining');
    // the scatter shows the checked node: Aland is Secure in Batteries Refining
    expect(screen.getByTestId('posture-scatter').querySelector('path[data-posture="S"]')).not.toBeNull();
    fireEvent.click(within(ignored).getByRole('button', { name: 'Dismiss' }));
    expect(screen.queryByTestId('posture-node-ignored')).toBeNull();
  });

  it('a Deployment node in the link is reported, and the counts are not the Deployment cells', async () => {
    at('/sector?pnode=Solar|Deployment', <SectorPostureCard showCounts />);
    expect((await screen.findByTestId('posture-node-ignored')).textContent).toContain('Deployment is left out here');
    expect(screen.getByTestId('posture-takeaway').textContent).toMatch(/^In Solar refining/);
    expect((screen.getByLabelText('Technology') as HTMLSelectElement).value).toBe('Solar');
    expect(screen.getByLabelText('Posture counts').textContent).toMatch(/Deploy3/);
  });

  it('a page that lists link problems itself turns the notice off', async () => {
    at('/sector?pnode=Batteries|Midstream', <SectorPostureCard reportIgnored={false} />);
    await screen.findByTestId('posture-scatter');
    expect(screen.queryByTestId('posture-node-ignored')).toBeNull();
  });

  it('fixed to one node (a Sector profile): no selects, the URL ignored, the title names the node; Deployment allowed', async () => {
    at('/sector?pnode=Solar|Refining', <SectorPostureCard node={{ tech: 'Batteries', stage: 'Components' }} />);
    expect((await screen.findByTestId('posture-takeaway')).textContent).toMatch(/^In Batteries components/);
    expect(screen.queryByLabelText('Technology')).toBeNull();
    expect(screen.getByRole('heading', { level: 3 }).textContent).toBe('Who should secure, compete or deploy in Batteries — Components');
    cleanup();
    at('/', <SectorPostureCard node={{ tech: 'Solar', stage: 'Deployment' }} compact />);
    expect((await screen.findByTestId('posture-takeaway')).textContent).toMatch(/^In Solar deployment, every country should deploy openly/);
    expect(screen.getByTestId('sector-posture-hero').textContent).toContain('every economy deploys openly there');
    cleanup();
    at('/', <SectorPostureCard node={{ tech: 'Solar', stage: 'Midstream' }} />);
    expect(await screen.findByText('No posture for this node')).toBeTruthy();
  });

  it("the table twin lists the node's economies (a row opens the profile) and the CSV carries the node", async () => {
    const router = at('/sector?pnode=Batteries|Components', <SectorPostureCard />);
    const card = await screen.findByTestId('sector-posture-hero');
    await screen.findByTestId('posture-scatter');
    const lines = await downloadCsv(card);
    expect(lines).toHaveLength(5);
    expect(lines.slice(1, 4).map((l) => l.split(',').slice(0, 5).join(','))).toEqual(['Bland,BBB,core,Batteries,Components', 'Aland,AAA,core,Batteries,Components', 'Cland,CCC,partner,Batteries,Components']);
    expect(lines[1].endsWith(`${SOURCE},2026-10-28,abc1234`)).toBe(true);
    fireEvent.click(screen.getByRole('button', { name: 'Table' }));
    const table = screen.getByRole('table', { name: 'Batteries — Components: strategic posture by economy' });
    const rows = within(table).getAllByRole('row').slice(1) as HTMLTableRowElement[];
    expect(rows.map((r) => r.cells[0].textContent)).toEqual(['Bland', 'Aland', 'Cland']);
    expect(rows[2].textContent).toContain('Partner gap');
    noAcronyms();
    fireEvent.click(rows[0]);
    await waitFor(() => expect(router.state.location.pathname).toBe('/countries/BBB'));
  });

  it('a page can send a point elsewhere', async () => {
    const router = at('/sector', <SectorPostureCard countryHref={(iso) => `/elsewhere/${iso}`} />);
    const sc = await screen.findByTestId('posture-scatter');
    fireEvent.click(sc.querySelector('path[data-posture="S"]') as SVGPathElement);
    await waitFor(() => expect(router.state.location.pathname).toBe('/elsewhere/AAA'));
  });
});

describe('the AIB division card', () => {
  it('names holders and gaps over the five supply stages; the header metrics are a separate list', async () => {
    at(
      '/',
      <>
        <AibDivisionStats />
        <AibDivisionCard />
      </>,
    );
    const m = await screen.findByTestId('aib-matrix');
    expect(within(m).getAllByRole('columnheader').map((h) => h.textContent)).toEqual(['Technology', 'ExtExtraction', 'RefRefining', 'ComComponents', 'AsmAssembly', 'RecRecovery']);
    const node = within(m).getByRole('button', { name: /^Batteries Components: primary Bland 20%/ });
    expect(node.getAttribute('data-gap')).toBe('single_point_of_failure');
    expect(within(m).getByRole('button', { name: /^Solar Assembly: no allied holder/ }).getAttribute('data-gap')).toBe('no_trusted_supplier');
    const stats = screen.getByRole('list', { name: 'The allied base at a glance' });
    expect(within(stats).getAllByRole('listitem').map((li) => li.textContent)).toEqual([
      'Core coalition members23 members in all',
      'Priority nodes with credible trusted coverage33%of 3 nodes members ask partners to supply or secure',
      'Unresolved gaps3priority nodes with a gap',
    ]);
    // the card itself has no metrics, an h3, and says why Deployment is not a column
    const card = screen.getByTestId('aib-division-hero');
    expect(card.textContent).not.toContain('Core coalition members');
    expect(within(card).getByRole('heading', { level: 3 }).textContent).toBe(AIB_DIVISION_TITLE);
    expect(card.textContent).toContain('Deployment is left out');
    fireEvent.click(node);
    expect(screen.getByTestId('aib-node-detail').textContent).toMatch(/Single point of failure .*also insufficient redundancy/);
    // no node list unless the page passes the nodes
    expect(screen.queryByTestId('aib-node-detail-nodes')).toBeNull();
    noAcronyms();
  });

  it('the node detail lists and links the Allied Industrial Base nodes of the cell', async () => {
    const nodesByCell = indexNodesByCell([
      { id: 'n1', label: 'Cell makers', href: '/aib/compacts/batteries', cells: ['batteries-components'] },
      { id: 'n2', label: 'Separators', cells: ['batteries-components', 'solar-assembly', 'batteries-components'] },
    ]);
    expect(nodesByCell['batteries-components'].map((n) => n.id)).toEqual(['n1', 'n2']);
    at('/', <AibDivisionCard nodesByCell={nodesByCell} />);
    const m = await screen.findByTestId('aib-matrix');
    fireEvent.click(within(m).getByRole('button', { name: /^Batteries Components/ }));
    const list = screen.getByTestId('aib-node-detail-nodes');
    expect(within(list).getByRole('link', { name: 'Cell makers' }).getAttribute('href')).toBe('/aib/compacts/batteries');
    expect(list.textContent).toBe('Allied Industrial Base nodes in this cell: Cell makers, Separators');
    fireEvent.click(within(m).getByRole('button', { name: /^Batteries Refining/ }));
    expect(screen.getByTestId('aib-node-detail-nodes').textContent).toBe('No Allied Industrial Base node is mapped to this cell.');
  });

  it('the table twin and the CSV, with the posture provenance (not the scenario run)', async () => {
    at('/', <AibDivisionCard />);
    const card = await screen.findByTestId('aib-division-hero');
    await screen.findByTestId('aib-matrix');
    const lines = await downloadCsv(card);
    expect(lines).toHaveLength(5);
    expect(lines[0].split(',').slice(0, 3)).toEqual(['technology', 'stage', 'primary_holder']);
    expect(lines[1].endsWith('"Opportunity Security Indices, strategic position framework (allied division of labour)",2026-10-28,abc1234')).toBe(true);
    fireEvent.click(screen.getByRole('button', { name: 'Table' }));
    const table = screen.getByRole('table', { name: 'Allied holders and gaps by technology and stage' });
    const rows = within(table).getAllByRole('row').slice(1) as HTMLTableRowElement[];
    expect(rows.map((r) => `${r.cells[0].textContent} ${r.cells[1].textContent}`)).toEqual(['Batteries Refining', 'Batteries Components', 'Solar Assembly']);
    noAcronyms();
    fireEvent.click(rows[2]);
    expect(screen.getByTestId('aib-node-detail').textContent).toMatch(/^Solar · Assembly: No trusted supplier/);
  });

  it('the Technology filter can live on the page', async () => {
    const onTechChange = vi.fn();
    at('/', <AibDivisionCard tech="Solar" onTechChange={onTechChange} />);
    const m = await screen.findByTestId('aib-matrix');
    expect(within(m).getAllByRole('rowheader').map((h) => h.textContent)).toEqual(['Solar']);
    expect(screen.getByTestId('posture-takeaway').textContent).toMatch(/in Solar/);
    fireEvent.change(screen.getByLabelText('Technology'), { target: { value: 'all' } });
    expect(onTechChange).toHaveBeenCalledWith('all');
    // controlled: the page has not changed it yet
    expect(within(m).getAllByRole('rowheader')).toHaveLength(1);
    cleanup();
    // the page's own filter bar: no second Technology select in the card
    at('/', <AibDivisionCard tech="Solar" techFilter={false} />);
    expect(within(await screen.findByTestId('aib-matrix')).getAllByRole('rowheader').map((h) => h.textContent)).toEqual(['Solar']);
    expect(screen.queryByLabelText('Technology')).toBeNull();
  });
});

describe('a card standing alone (headingLevel 2 with the counts or metrics inside)', () => {
  it('keeps an h2 and the counts or metrics inside the card', async () => {
    at('/', <CountryPostureCard iso3="AAA" name="Aland" headingLevel={2} showCounts />);
    expect((await screen.findByRole('heading', { level: 2 })).textContent).toBe(COUNTRY_POSTURE_TITLE);
    const counts = screen.getByLabelText('Posture counts');
    expect(counts.textContent).toMatch(/Secure1/);
    expect(counts.textContent).toMatch(/Compete2/);
    cleanup();
    at('/sector', <SectorPostureCard headingLevel={2} showCounts />);
    expect((await screen.findByRole('heading', { level: 2 })).textContent).toBe(SECTOR_POSTURE_TITLE);
    expect(screen.getByLabelText('Posture counts')).toBeTruthy();
    cleanup();
    at('/', <AibDivisionCard headingLevel={2} stats />);
    const hero = await screen.findByTestId('aib-division-hero');
    expect(within(hero).getByRole('heading', { level: 2 }).textContent).toBe(AIB_DIVISION_TITLE);
    expect(hero.textContent).toContain('Core coalition members');
    expect(hero.textContent).toContain('33%');
    expect(hero.textContent).toContain('Unresolved gaps');
  });
});
