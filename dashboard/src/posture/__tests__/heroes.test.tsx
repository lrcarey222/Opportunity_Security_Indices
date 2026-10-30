// The three posture cards on the schema-2 fixture (design section 4, 5b): the Country Strategy Map (the six
// stages under their display names, colour alone since 2026-10-30: each cell its choice colour with no letter,
// arrow or dot, the not-assessed hatch, the legend above the map grouped by fence with the node counts, the
// Deployment column set apart with its header annotation and helper, the tooltip naming the node, the choice
// and its fence, the generated sentence, the five-question drawer, the table twin and CSV), the Sector scatter and
// the AIB division matrix (kept working on schema 2; their wording is finished on the pages' side). Each
// card is a ChartCard with an h3 (the page's Section has the h2), a table twin and a CSV with the posture
// provenance; the tile and the AIB metrics are separate pieces.

import { afterAll, afterEach, beforeAll, beforeEach, describe, expect, it, vi } from 'vitest';
import { cleanup, fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import { createMemoryRouter, RouterProvider } from 'react-router';
import { bareAcronyms } from '../../components/testing/acronyms.ts';
import { cssHex } from '../../components/testing/color.ts';
import { AibDivisionCard, AIB_DIVISION_TITLE, indexNodesByCell } from '../AibDivisionCard.tsx';
import { PostureBasisTag } from '../basis.tsx';
import { DRAWER_QUESTIONS } from '../CellDrawer.tsx';
import { COUNTRY_STRATEGY_MAP_TITLE, CountryPostureCard } from '../CountryPostureCard.tsx';
import { AibDivisionStats, CountryPostureTile } from '../PostureCounts.tsx';
import { MAP_HELPER } from '../PostureMatrix.tsx';
import { SECTOR_POSTURE_TITLE, SectorPostureCard } from '../SectorPostureCard.tsx';
import type { Verb } from '../contract.ts';
import { VERB_COLOR, VERB_OUTLINE } from '../grammar.ts';
import { setPostureStore } from '../store.ts';
import { fixtureCore, fixtureStore, withCell, withoutCell } from './fixture.ts';

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
const SENTENCE =
  'Aland should preserve its strongest positions in battery pack assembly and solar module assembly; secure battery materials refining (chokepoint), polysilicon refining (systemic resilience) and solar inverters (operational / control); catch up in battery cell manufacturing; rely on partners for lithium and nickel mining; and accelerate deployment of battery storage.';

describe('the Country Strategy Map', () => {
  it('renders all six stages under their display names, every node a button filled with its choice colour alone: no letter, arrow or dot', async () => {
    at('/', <CountryPostureCard iso3="AAA" name="Aland" />);
    const m = await screen.findByTestId('posture-matrix');
    const buttons = within(m).getAllByRole('button');
    expect(buttons).toHaveLength(12);
    expect(within(m).getAllByRole('columnheader').map((h) => h.textContent)).toEqual([
      'Technology', 'ExtExtraction', 'RefRefining & Processing', 'ComComponent Manufacturing', 'AsmFinal Assembly & Equipment', 'DepDeployment & Use', 'RecRecovery & Recycling',
    ]);
    // no glyph of any kind: a cell is an empty button, its fill the only mark
    for (const b of buttons) {
      expect(b.textContent, b.getAttribute('aria-label') ?? '').toBe('');
      expect(b.children).toHaveLength(0);
    }
    const node = (name: RegExp) => within(m).getByRole('button', { name });
    const look = (b: HTMLElement) => [b.getAttribute('data-verb'), b.getAttribute('data-fence'), cssHex(b.style.background)];
    // the name says the choice and its fence; the fill is the choice's colour
    expect(look(node(/^Batteries Refining & Processing: Secure · High fence; Coercive chokepoint/))).toEqual(['secure', 'high', '#f58229']);
    expect(look(node(/^Solar Refining & Processing: Secure · High fence; Systemic resilience/))).toEqual(['secure', 'high', '#f58229']);
    expect(look(node(/^Batteries Final Assembly & Equipment: Preserve · Medium fence/))).toEqual(['preserve', 'medium', '#1889b0']);
    expect(look(node(/^Batteries Component Manufacturing: Catch Up · Medium fence/))).toEqual(['catch_up', 'medium', '#b4e7fe']);
    expect(look(node(/^Batteries Extraction: Partner · Medium fence/))).toEqual(['partner', 'medium', '#55c4c5']);
    const acc = node(/^Batteries Deployment & Use: Accelerate · Medium fence/);
    expect([...look(acc), acc.getAttribute('data-mode')]).toEqual(['accelerate', 'medium', '#a3b63b', 'accelerate']);
    const watch = node(/^Solar Extraction: Watch · Low fence/);
    expect(look(watch)).toEqual(['watch', 'low', '#4b5563']);
    // Watch's grey is below 3:1 on the card: a light hairline makes it a cell
    expect(cssHex(watch.style.border)).toBe(VERB_OUTLINE.watch);
    // not assessed: no colour, hatched and dashed
    const na = node(/^Batteries Recovery & Recycling: Not assessed; need or prize not measured$/);
    expect([na.getAttribute('data-verb'), na.getAttribute('data-fence')]).toEqual(['not_assessed', null]);
    expect(na.style.background).toContain('repeating-linear-gradient');
    expect(na.style.border).toContain('dashed');
    // every assessed cell is exactly its verb's colour: six choices, six colours, no family scheme
    const assessed = buttons.filter((b) => b.getAttribute('data-verb') !== 'not_assessed');
    for (const b of assessed) expect(cssHex(b.style.background), b.getAttribute('aria-label') ?? '').toBe(VERB_COLOR[b.getAttribute('data-verb') as Verb]);
    expect(new Set(assessed.map((b) => b.getAttribute('data-verb')))).toEqual(new Set(['secure', 'preserve', 'catch_up', 'partner', 'accelerate', 'watch']));
    expect(new Set(assessed.map((b) => cssHex(b.style.background))).size).toBe(6);
    // Deployment & Use holds only Accelerate and Watch (and the medium or low fence), never Secure
    const dep = assessed.filter((b) => / Deployment & Use: /.test(b.getAttribute('aria-label') ?? ''));
    expect(new Set(dep.map((b) => b.getAttribute('data-verb')))).toEqual(new Set(['accelerate', 'watch']));
    expect(dep.every((b) => b.getAttribute('data-fence') !== 'high')).toBe(true);
  });

  it('policy alignment, evidence, the Secure rationale and momentum are words (the name, the tooltip), never drawn on the cell', async () => {
    at('/', <CountryPostureCard iso3="AAA" name="Aland" />);
    const m = await screen.findByTestId('posture-matrix');
    const sec = within(m).getByRole('button', { name: /^Batteries Refining & Processing/ });
    expect(sec.getAttribute('aria-label')).toContain('policy under-prioritized');
    const gap = within(m).getByRole('button', { name: /^Batteries Deployment & Use/ });
    expect(gap.getAttribute('aria-label')).toContain('policy under-prioritized: under-response (deployment)');
    const weak = within(m).getByRole('button', { name: /^Batteries Final Assembly & Equipment/ });
    expect(weak.getAttribute('aria-label')).toContain('evidence weak');
    // no border code, no fade, no alignment attribute: the colour is the choice and nothing else
    for (const b of [sec, gap, weak, within(m).getByRole('button', { name: /^Batteries Extraction/ })]) {
      expect([b.style.border, b.style.opacity, b.getAttribute('data-alignment')], b.getAttribute('aria-label') ?? '').toEqual(['', '', null]);
    }
    // the Partner node's momentum is rising: said in the table and drawer, never in the cell or its name
    const ext = within(m).getByRole('button', { name: /^Batteries Extraction/ });
    expect(ext.textContent).toBe('');
    expect(ext.getAttribute('aria-label')).not.toContain('rising');
    // hover or focus: the tooltip names the node, the choice and its fence
    fireEvent.mouseEnter(within(m).getByRole('button', { name: /^Solar Extraction/ }));
    let tip = screen.getByRole('tooltip');
    expect(tip.textContent).toMatch(/^Solar · Extraction/);
    expect(within(tip).getByTestId('strategy-map-tip-choice').textContent).toBe('Watch· Low fence');
    fireEvent.mouseLeave(within(m).getByRole('button', { name: /^Solar Extraction/ }));
    fireEvent.focus(ext);
    tip = screen.getByRole('tooltip');
    expect(tip.textContent).toMatch(/^Batteries · Extraction/);
    expect(within(tip).getByTestId('strategy-map-tip-choice').textContent).toBe('Partner· Medium fence');
    expect(tip.textContent).toContain('First partner: Bland');
    expect(tip.textContent).toContain('Policy: Wrong treatment / misaligned');
    fireEvent.blur(ext);
    fireEvent.focus(sec);
    expect(screen.getByRole('tooltip').textContent).toContain('Coercive chokepoint');
    expect(within(screen.getByRole('tooltip')).getByTestId('strategy-map-tip-choice').textContent).toBe('Secure· High fence');
  });

  it('every node, assessed or not, is a keyboard button that opens the drawer with its own key', async () => {
    const onSelect = vi.fn();
    at('/', <CountryPostureCard iso3="AAA" name="Aland" selected={null} onSelect={onSelect} drawer={false} />);
    const m = await screen.findByTestId('posture-matrix');
    const buttons = within(m).getAllByRole('button');
    const keys: string[] = [];
    for (const b of buttons) {
      expect(b.tagName).toBe('BUTTON');
      expect(b.getAttribute('type')).toBe('button');
      const [tech, rest] = (b.getAttribute('aria-label') ?? '').split(/ (?=Extraction|Refining|Component|Final|Deployment|Recovery)/);
      fireEvent.click(b);
      keys.push(`${tech}|${rest.split(':')[0]}`);
    }
    const stageOf: Record<string, string> = {
      Extraction: 'Extraction', 'Refining & Processing': 'Refining', 'Component Manufacturing': 'Components', 'Final Assembly & Equipment': 'Assembly', 'Deployment & Use': 'Deployment', 'Recovery & Recycling': 'Recovery',
    };
    expect(onSelect.mock.calls.map((c) => c[0])).toEqual(keys.map((k) => { const [t, s] = k.split('|'); return `${t}|${stageOf[s]}`; }));
    expect(new Set(onSelect.mock.calls.map((c) => c[0])).size).toBe(12);
    cleanup();
    // uncontrolled: a click opens the card's own drawer on that node
    at('/', <CountryPostureCard iso3="AAA" name="Aland" />);
    const m2 = await screen.findByTestId('posture-matrix');
    fireEvent.click(within(m2).getByRole('button', { name: /^Solar Deployment & Use/ }));
    expect((await screen.findByTestId('cell-drawer')).getAttribute('aria-label')).toBe('Solar Deployment & Use: five questions');
  });

  it('sets the Deployment column apart: dividers before and after, the header annotation and the helper', async () => {
    at('/', <CountryPostureCard iso3="AAA" name="Aland" />);
    const m = await screen.findByTestId('posture-matrix');
    expect(within(m).getAllByTestId('deployment-divider')).toHaveLength(4);
    expect(m.querySelectorAll('[role="gridcell"][data-deployment="true"]')).toHaveLength(2);
    expect(m.querySelector('[role="columnheader"][data-deployment="true"]')?.textContent).toBe('DepDeployment & Use');
    const band = screen.getByTestId('strategy-map-band');
    expect(band.textContent).toBe('Supply chain →Supply-chain strategy →EnergyEnergy-system strategyCirc.→ Circularity');
    const helper = screen.getByTestId('strategy-map-helper');
    expect(helper.textContent).toContain(MAP_HELPER);
    expect(m.getAttribute('aria-describedby')).toBe(helper.id);
  });

  it('is a ChartCard for the Strategy Section: an h3, the strategy sentence, and above the map the legend grouped by fence with the node counts', async () => {
    at('/', <CountryPostureCard iso3="AAA" name="Aland" />);
    expect((await screen.findByTestId('posture-takeaway')).textContent).toBe(SENTENCE);
    expect(screen.getByRole('heading', { level: 3 }).textContent).toBe(COUNTRY_STRATEGY_MAP_TITLE);
    expect(screen.queryAllByRole('heading', { level: 2 })).toHaveLength(0);
    expect(screen.getByRole('region', { name: COUNTRY_STRATEGY_MAP_TITLE })).toBeTruthy();
    expect(screen.getByRole('figure').getAttribute('aria-label')).toBe(`${SENTENCE} 3 Secure · 2 Preserve · 1 Catch Up · 1 Partner · 1 Accelerate · 3 Watch.`);
    const legend = screen.getByTestId('posture-legend');
    expect(legend.getAttribute('aria-label')).toBe('Legend: choices by fence');
    expect([...legend.querySelectorAll('[data-fence]')].map((g) => g.getAttribute('data-fence'))).toEqual(['high', 'medium', 'low']);
    expect([...legend.querySelectorAll('[data-fence]')].map((g) => [...g.querySelectorAll('[data-verb]')].map((v) => v.getAttribute('data-verb')))).toEqual([
      ['secure'], ['preserve', 'catch_up', 'partner', 'accelerate'], ['watch'],
    ]);
    expect(legend.textContent).toBe('High fence:Secure 3Medium fence:Preserve 2,Catch Up 1,Partner 1,Accelerate 1Low fence:Watch 3Not assessed 1');
    // each swatch is its choice's colour (the not-assessed one hatched), and each count is the map's own
    const m = screen.getByTestId('posture-matrix');
    for (const it of [...legend.querySelectorAll('[data-verb]')] as HTMLElement[]) {
      const v = it.getAttribute('data-verb')!;
      const sw = it.querySelector('[data-swatch]') as HTMLElement;
      if (v === 'not_assessed') expect(sw.style.background).toContain('repeating-linear-gradient');
      else expect(cssHex(sw.style.background), v).toBe(VERB_COLOR[v as Verb]);
      expect(it.textContent?.match(/(\d+),?$/)?.[1], v).toBe(String(m.querySelectorAll(`button[data-verb="${v}"]`).length));
    }
    // the legend leads: above the map, inside the card
    expect(legend.compareDocumentPosition(m) & Node.DOCUMENT_POSITION_FOLLOWING).toBeTruthy();
    expect(screen.queryByTestId('strategy-counts')).toBeNull();
    expect(screen.getByTestId('trade-freshness').textContent).toBe('Structural trade benchmark: 2022–24 · Current signal through: Aug 2026');
    noAcronyms();
  });

  it('the page can carry the sentence and drop the counts: the legend keeps its colours and fences without numbers', async () => {
    at('/', <CountryPostureCard iso3="AAA" name="Aland" showSentence={false} showCounts={false} />);
    await screen.findByTestId('posture-matrix');
    expect(screen.queryByTestId('posture-takeaway')).toBeNull();
    const legend = screen.getByTestId('posture-legend');
    expect(legend.textContent).toBe('High fence:SecureMedium fence:Preserve,Catch Up,Partner,AccelerateLow fence:WatchNot assessed');
  });

  it('opens the drawer with exactly five questions, in order', async () => {
    at('/', <CountryPostureCard iso3="AAA" name="Aland" />);
    const m = await screen.findByTestId('posture-matrix');
    fireEvent.click(within(m).getByRole('button', { name: /^Batteries Extraction/ }));
    const d = await screen.findByTestId('cell-drawer');
    const qs = within(d).getAllByTestId('drawer-question');
    expect(qs.map((q) => q.getAttribute('data-question'))).toEqual(['need', 'prize', 'position', 'choice', 'policy']);
    expect(qs.map((q) => within(q).getByRole('heading', { level: 4 }).textContent)).toEqual(DRAWER_QUESTIONS.map((q) => q.title));
    expect(DRAWER_QUESTIONS.map((q) => q.title)).toEqual(['Need', 'Prize', 'Position', 'Choice', 'Policy alignment']);
    await waitFor(() => expect(qs[0].textContent).toContain('Import dependence'));
    expect(qs[0].textContent).toContain('Supply-chain need');
    expect(qs[0].textContent).toContain('Lack of trusted alternatives');
    expect(qs[1].textContent).toContain('Node prize');
    expect(qs[2].textContent).toContain('Competitive Position');
    expect(qs[2].textContent).toMatch(/Trade momentum.*rising/);
    // the choice with the page's fence (never the R family words); the policy design's trade protection is labelled as such, never "Fence:"
    expect(within(d).getByRole('heading', { level: 3 }).nextElementSibling?.textContent).toMatch(/^Partner · Medium fence/);
    expect(d.textContent).not.toMatch(/Compete ·|Deploy ·/);
    expect(within(qs[3]).getByTestId('drawer-fence').textContent).toBe('Medium fence');
    expect(qs[3].textContent).not.toMatch(/Fence:/);
    expect(qs[3].textContent).toContain('Bland');
    expect(qs[3].textContent).toContain('How the choice was made');
    expect(qs[4].textContent).toContain('Wrong treatment / misaligned');
    expect(qs[4].textContent).toContain('Protection or localisation');
    expect(qs[4].textContent).toContain('it never enters the recommendation');
    expect(screen.queryByTestId('cell-drawer-sector-link')).toBeNull();
    fireEvent.keyDown(window, { key: 'Escape' });
    await waitFor(() => expect(screen.queryByTestId('cell-drawer')).toBeNull());
  });

  it("the drawer names each node's choice with the page's fence: a producing-stage Watch is the Low fence, never Deploy; a Secure node's policy-design protection is not its fence", async () => {
    at('/', <CountryPostureCard iso3="AAA" name="Aland" />);
    const m = await screen.findByTestId('posture-matrix');
    const head = (d: HTMLElement) => within(d).getByRole('heading', { level: 3 }).nextElementSibling?.textContent ?? '';
    // Solar Extraction: Watch at a producing stage (the R family is Deploy): "Watch · Low fence"
    fireEvent.click(within(m).getByRole('button', { name: /^Solar Extraction/ }));
    let d = await screen.findByTestId('cell-drawer');
    expect(head(d)).toMatch(/^Watch · Low fence/);
    expect(d.textContent).not.toMatch(/Deploy · Watch|Compete ·/);
    expect(within(d).getByTestId('drawer-fence').textContent).toBe('Low fence');
    // Batteries Refining: Secure; the policy design's fence text is labelled as trade protection, apart from the High fence
    fireEvent.click(within(m).getByRole('button', { name: /^Batteries Refining/ }));
    await waitFor(() => expect(within(screen.getByTestId('cell-drawer')).getByRole('heading', { level: 3 }).textContent).toBe('Batteries · Refining & Processing'));
    d = screen.getByTestId('cell-drawer');
    expect(head(d)).toMatch(/^Secure · High fence/);
    expect(within(d).getByTestId('drawer-fence').textContent).toBe('High fence');
    const tp = await within(d).findByTestId('drawer-trade-protection');
    expect(tp.textContent).toBe('Trade protection (policy design): Screen coercive suppliers of refined battery materials.');
    // every choice and fence word on the cell's name, the drawer and the legend agree
    expect(within(m).getByRole('button', { name: /^Batteries Refining/ }).getAttribute('aria-label')).toMatch(/: Secure · High fence;/);
  });

  it('a Deployment node answers Need with the deployment families, never supply-chain need', async () => {
    at('/', <CountryPostureCard iso3="AAA" name="Aland" selected="Batteries|Deployment" />);
    const d = await screen.findByTestId('cell-drawer');
    const need = d.querySelector('[data-question="need"]') as HTMLElement;
    await waitFor(() => expect(need.textContent).toContain('Demand adequacy'));
    expect(need.textContent).toContain('90% · national data · Current policy');
    expect(need.textContent).toContain('Target / climate gap80% · national data · National commitments');
    expect(need.textContent).toContain('System neednot assessed');
    expect(need.textContent).toContain('Climate-aligned (alternative scenario, not used in the choice)+104% vs current policies · World 2035, Net Zero vs Current Policies · global context · Climate-aligned');
    expect(need.textContent).not.toContain('Supply-chain need');
    expect(need.textContent).not.toContain('Import dependence');
    expect((d.querySelector('[data-question="prize"]') as HTMLElement).textContent).toContain('Domestic manufacturing priority');
    expect((d.querySelector('[data-question="choice"]') as HTMLElement).textContent).toContain('Deployment priority: high');
    expect((d.querySelector('[data-question="policy"]') as HTMLElement).textContent).toContain('Under-prioritized: under-response (deployment)');
  });

  it('Secure says its rationale and mitigation; Watch is a deliberate low priority; not assessed is counted apart', async () => {
    at('/', <CountryPostureCard iso3="AAA" name="Aland" selected="Batteries|Refining" />);
    const d = await screen.findByTestId('cell-drawer');
    await waitFor(() => expect(d.textContent).toContain('Mitigation:'));
    expect(d.textContent).toContain('Coercive chokepoint');
    expect(d.textContent).toContain('not reshoring everything');
    cleanup();
    at('/', <CountryPostureCard iso3="AAA" name="Aland" selected="Solar|Extraction" />);
    const w = await screen.findByTestId('cell-drawer');
    await waitFor(() => expect(w.textContent).toContain('Domestic industrial priority: low (low need; low prize; ample diversified supply)'));
    expect(w.textContent).toContain('not a failure');
    cleanup();
    at('/', <CountryPostureCard iso3="AAA" name="Aland" selected="Batteries|Recovery" />);
    const n = await screen.findByTestId('cell-drawer');
    await waitFor(() => expect(n.textContent).toContain('Supply-chain need is not measured for this node.'));
    expect(n.textContent).toContain('never as Watch');
    expect((n.querySelector('[data-question="policy"]') as HTMLElement).textContent).toContain('No recommendation, so no alignment to judge.');
  });

  it('Catch Up says its position from the cell: a top-quarter score that did not pass the Preserve test, or not measured', async () => {
    const choiceOf = async () => (await screen.findByTestId('cell-drawer')).querySelector('[data-question="choice"]') as HTMLElement;
    // Aland's battery cells: Catch Up with a position of 88, in the top quarter but not a Preserve (below the capability floor)
    setPostureStore(fixtureStore({ core: withCell(fixtureCore(), 0, 0, 2, { cp: 0.875 }) }));
    at('/', <CountryPostureCard iso3="AAA" name="Aland" selected="Batteries|Components" />);
    let choice = await choiceOf();
    expect(choice.textContent).toContain(
      'A large prize, a top-quarter score that does not pass the Preserve test (not a contender, or below the capability floor) and a credible route to build capability.',
    );
    expect(choice.textContent).not.toContain('a position below the top quarter');
    cleanup();
    setPostureStore(fixtureStore({ core: withCell(fixtureCore(), 0, 0, 2, { cp: null }) }));
    at('/', <CountryPostureCard iso3="AAA" name="Aland" selected="Batteries|Components" />);
    choice = await choiceOf();
    expect(choice.textContent).toContain('A large prize, position not measured and a credible route to build capability.');
    cleanup();
    setPostureStore(fixtureStore());
    at('/', <CountryPostureCard iso3="AAA" name="Aland" selected="Batteries|Components" />);
    choice = await choiceOf();
    expect(choice.textContent).toContain('A large prize, a position below the top quarter and a credible route to build capability.');
  });

  it('Policy alignment gives support, protection and momentum as ranks within the country: never shares or a signed change', async () => {
    at('/', <CountryPostureCard iso3="AAA" name="Aland" selected="Batteries|Components" />);
    const d = await screen.findByTestId('cell-drawer');
    const policy = d.querySelector('[data-question="policy"]') as HTMLElement;
    await waitFor(() => expect(policy.textContent).toContain('Supportmoderate, 60th percentile in the country'));
    expect(policy.textContent).toContain('Observed policy intensitymoderate, 60th percentile in the country');
    // no protection measure recorded: not its tied rank
    expect(policy.textContent).toContain('Protection or localisationnone recorded');
    expect(policy.textContent).toContain('Policy momentum (recent activity)low, 12th percentile in the country');
    expect(policy.textContent).not.toMatch(/\+12%|30%|within the country\)/);
    cleanup();
    at('/', <CountryPostureCard iso3="AAA" name="Aland" selected="Batteries|Extraction" />);
    const e = (await screen.findByTestId('cell-drawer')).querySelector('[data-question="policy"]') as HTMLElement;
    await waitFor(() => expect(e.textContent).toContain('Protection or localisationhigh, 90th percentile in the country'));
    expect(e.textContent).toContain('Supportlow, 20th percentile in the country');
    expect(e.textContent).toContain('Policy momentum (recent activity)rising');
  });

  it('the table twin lists all 12 nodes, an unclassified one kept, and a row opens the drawer', async () => {
    // Aland's Solar Recovery has no posture
    setPostureStore(fixtureStore({ core: withoutCell(fixtureCore(), 0, 1, 5) }));
    at('/', <CountryPostureCard iso3="AAA" name="Aland" />);
    await screen.findByTestId('posture-matrix');
    expect(screen.getByRole('gridcell', { name: 'Solar Recovery & Recycling: not classified' })).toBeTruthy();
    fireEvent.click(screen.getByRole('button', { name: 'Table' }));
    const table = screen.getByRole('table', { name: 'Aland: choice by technology and stage' });
    expect(within(table).getAllByRole('columnheader').map((h) => h.textContent?.replace(/[▲▼]/g, ''))).toEqual([
      'Technology', 'Stage', 'Choice', 'Secure rationale', 'Need type', 'Need', 'Industrial Opportunity', 'Competitive Position', 'World supply share', 'Credible pathway',
      'Policy alignment', 'Evidence', 'Trade momentum', 'First trusted partner', 'Borderline on', 'Consistency review',
    ]);
    const rows = within(table).getAllByRole('row').slice(1) as HTMLTableRowElement[];
    expect(rows).toHaveLength(12);
    expect(rows[1].textContent).toContain('Coercive chokepoint');
    expect(rows[0].textContent).toContain('rising');
    expect(rows[11].textContent).toMatch(/^SolarRecovery & RecyclingNot classified/);
    noAcronyms();
    fireEvent.click(rows[0]);
    await waitFor(() => expect(screen.getByTestId('cell-drawer').textContent).toContain('Bland'));
    expect(rows[0].getAttribute('aria-selected')).toBe('true');
    fireEvent.click(screen.getByRole('button', { name: 'Chart' }));
    expect(screen.getByRole('button', { name: /^Batteries Extraction/ }).getAttribute('aria-pressed')).toBe('true');
  });

  it('the CSV: one row per node with source, as-of date and commit last', async () => {
    setPostureStore(fixtureStore({ core: withoutCell(fixtureCore(), 0, 1, 5) }));
    at('/', <CountryPostureCard iso3="AAA" name="Aland" />);
    const card = await screen.findByTestId('country-posture-hero');
    await screen.findByTestId('posture-matrix');
    const lines = await downloadCsv(card);
    expect(lines[0].split(',').slice(-3)).toEqual(['source', 'as_of', 'git_commit']);
    expect(lines[0].split(',').slice(0, 8)).toEqual(['country', 'iso3', 'coalition_tier', 'technology', 'stage', 'stage_name', 'classified', 'posture']);
    expect(lines).toHaveLength(14);
    expect(lines[2]).toBe(
      `Aland,AAA,core,Batteries,Refining,Refining & Processing,TRUE,Secure,,,Secure · High fence,Coercive chokepoint,Supply-chain need,0.95,,0.4,0.1,0.002,,Under-prioritized,medium,Strong,high,,,,,,,${SOURCE},2026-10-29,abc1234`,
    );
    expect(lines[12].startsWith('Aland,AAA,core,Solar,Recovery,Recovery & Recycling,FALSE,,')).toBe(true);
  });

  it('a page can hold the selection (the URL), mount the drawer itself and link the drawer to the Sector page', async () => {
    const onSelect = vi.fn();
    at('/', <CountryPostureCard iso3="AAA" name="Aland" selected="Batteries|Components" onSelect={onSelect} sectorHref={(t, s) => `/sector/${t.toLowerCase()}/${s.toLowerCase()}`} />);
    const d = await screen.findByTestId('cell-drawer');
    await waitFor(() => expect(d.textContent).toContain('feasibility, pipeline'));
    expect(screen.getByTestId('cell-drawer-sector-link').getAttribute('href')).toBe('/sector/batteries/components');
    fireEvent.click(screen.getByRole('button', { name: /^Batteries Refining & Processing/ }));
    expect(onSelect).toHaveBeenLastCalledWith('Batteries|Refining');
    fireEvent.click(screen.getByRole('button', { name: 'Close the detail' }));
    expect(onSelect).toHaveBeenLastCalledWith(null);
    cleanup();
    at('/', <CountryPostureCard iso3="AAA" name="Aland" selected="Batteries|Components" drawer={false} />);
    await screen.findByTestId('posture-matrix');
    expect(screen.queryByTestId('cell-drawer')).toBeNull();
  });

  it('serves a renamed fixture economy, for the page route tests', async () => {
    setPostureStore(fixtureStore({ economies: { AAA: { iso3: 'USA', name: 'United States' } } }));
    at('/', <CountryPostureCard iso3="USA" name="The United States" />);
    expect((await screen.findByTestId('posture-takeaway')).textContent).toMatch(/^The United States should preserve its strongest positions/);
    fireEvent.click(screen.getByRole('button', { name: /^Batteries Extraction/ }));
    await waitFor(() => expect(screen.getByTestId('cell-drawer').textContent).toContain('United States'));
  });

  it('an economy without a posture: a notice, no card', async () => {
    at('/', <CountryPostureCard iso3="ZZZ" name="Zland" />);
    expect((await screen.findByText('No posture')).closest('[role="status"]')?.textContent).toContain('Zland has no classified supply-chain nodes');
    expect(screen.queryByRole('heading')).toBeNull();
  });

  it('without a momentum window it says why there is no current signal; a v1 bundle is refused with a notice', async () => {
    setPostureStore(fixtureStore({ momentum: false }));
    at('/', <CountryPostureCard iso3="AAA" name="Aland" />);
    expect((await screen.findByTestId('trade-freshness')).textContent).toContain('Current momentum unavailable — incomplete matched-period reporting');
    cleanup();
    setPostureStore(fixtureStore({ core: { ...fixtureCore(), schema_version: 1 } }));
    at('/', <CountryPostureCard iso3="AAA" name="Aland" />);
    expect((await screen.findByText('The posture data could not be loaded')).closest('[role="status"], [role="alert"]')?.textContent).toMatch(/retired Need x Opportunity/);
  });

  it('keeps an h2 when standing alone', async () => {
    at('/', <CountryPostureCard iso3="AAA" name="Aland" headingLevel={2} />);
    expect((await screen.findByRole('heading', { level: 2 })).textContent).toBe(COUNTRY_STRATEGY_MAP_TITLE);
    expect(screen.getByTestId('posture-legend').textContent).toContain('Secure 3');
  });
});

describe("the page's posture pieces", () => {
  it('the Country tile carries the family counts, not assessed and the six verbs, and links to the section', async () => {
    at('/', <CountryPostureTile iso3="AAA" link={{ href: '#strategy', label: 'Country Strategy Map' }} />);
    const counts = await screen.findByTestId('posture-tile-counts');
    expect(counts.textContent).toContain('Secure 3 · Compete 4 · Deploy 4 · 1 not assessed');
    expect(document.body.textContent).toContain('STRATEGIC POSTURE');
    expect(document.body.textContent).toContain('3 Secure · 2 Preserve · 1 Catch Up · 1 Partner · 1 Accelerate · 3 Watch');
    expect(screen.getByRole('link', { name: 'Country Strategy Map' }).getAttribute('href')).toBe('#strategy');
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

describe('the Sector posture card (schema 2)', () => {
  it('places countries by prize and need, keeps Secure at its true coordinates, and opens the Country Strategy Map on click', async () => {
    const router = at('/sector?pnode=Batteries|Refining', <SectorPostureCard />);
    const sc = await screen.findByTestId('posture-scatter');
    for (const q of ['HIGH PRIZE: POSITION DECIDES', 'PARTNER', 'WATCH']) expect(sc.textContent).toContain(q);
    const secure = sc.querySelector('path[data-posture="S"]') as SVGPathElement;
    expect(secure.getAttribute('aria-label')).toBe('Aland: Secure · High fence, need 95%, prize 40%');
    expect(screen.getByRole('heading', { level: 3 }).textContent).toBe(SECTOR_POSTURE_TITLE);
    const reading = screen.getByTestId('posture-reading');
    expect(reading.textContent).toMatch(/^Each point is one economy\. Across: Industrial Opportunity\s?, .*up: supply-chain need\s?\. Outlined diamonds are Secure.*Point size is Competitive Position\s?\.$/);
    fireEvent.click(secure);
    await waitFor(() => expect(router.state.location.pathname).toBe('/countries/AAA'));
    expect(router.state.location.hash).toBe('#strategy');
  });

  it('the node selector writes the URL and the takeaway follows it; Deployment is a disabled choice', async () => {
    const router = at('/sector?pnode=Batteries|Components', <SectorPostureCard />);
    expect((await screen.findByTestId('posture-takeaway')).textContent).toBe(
      'In Batteries components, 2 have the prize and a credible pathway to catch up and 1 need it without a domestic case, and should partner.',
    );
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
    expect(ignored.textContent).toContain('pnode=Batteries|Midstream (Midstream is no longer a stage: it is now mostly Components and Assembly; showing Batteries — Refining)');
    expect(screen.getByTestId('posture-takeaway').textContent).toBe('In Batteries refining, 1 country must secure it.');
    expect((screen.getByLabelText('Stage') as HTMLSelectElement).value).toBe('Refining');
    expect(screen.getByTestId('posture-scatter').querySelector('path[data-posture="S"]')).not.toBeNull();
    fireEvent.click(within(ignored).getByRole('button', { name: 'Dismiss' }));
    expect(screen.queryByTestId('posture-node-ignored')).toBeNull();
  });

  it('a Deployment node in the link is reported; the counts include the node not assessed', async () => {
    at('/sector?pnode=Solar|Deployment', <SectorPostureCard showCounts />);
    expect((await screen.findByTestId('posture-node-ignored')).textContent).toContain('Deployment is left out here');
    expect((screen.getByLabelText('Technology') as HTMLSelectElement).value).toBe('Solar');
    const counts = screen.getByLabelText('Posture counts').textContent;
    expect(counts).toMatch(/Secure1/);
    expect(counts).toMatch(/Deploy1/);
    expect(counts).toMatch(/Not assessed1/);
  });

  it('a page that lists link problems itself turns the notice off', async () => {
    at('/sector?pnode=Batteries|Midstream', <SectorPostureCard reportIgnored={false} />);
    await screen.findByTestId('posture-scatter');
    expect(screen.queryByTestId('posture-node-ignored')).toBeNull();
  });

  it('fixed to one node (a Sector profile): no selects, the URL ignored; Deployment allowed with deployment-need words', async () => {
    at('/sector?pnode=Solar|Refining', <SectorPostureCard node={{ tech: 'Batteries', stage: 'Components' }} />);
    expect((await screen.findByTestId('posture-takeaway')).textContent).toMatch(/^In Batteries components/);
    expect(screen.queryByLabelText('Technology')).toBeNull();
    expect(screen.getByRole('heading', { level: 3 }).textContent).toBe('Who should secure, compete or deploy in Batteries — Components');
    cleanup();
    at('/', <SectorPostureCard node={{ tech: 'Solar', stage: 'Deployment' }} compact />);
    expect((await screen.findByTestId('posture-takeaway')).textContent).toBe('In Solar deployment, 3 need no acceleration (watch).');
    expect(screen.getByTestId('sector-posture-hero').textContent).toContain('At Deployment & Use the choice reads deployment need');
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
    expect(lines[1].endsWith(`${SOURCE},2026-10-29,abc1234`)).toBe(true);
    fireEvent.click(screen.getByRole('button', { name: 'Table' }));
    const table = screen.getByRole('table', { name: 'Batteries — Components: strategic posture by economy' });
    const rows = within(table).getAllByRole('row').slice(1) as HTMLTableRowElement[];
    expect(rows.map((r) => r.cells[0].textContent)).toEqual(['Bland', 'Aland', 'Cland']);
    expect(rows[2].textContent).toContain('No credible allied solution yet');
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

  it('keeps an h2 and the counts when standing alone', async () => {
    at('/sector', <SectorPostureCard headingLevel={2} showCounts />);
    expect((await screen.findByRole('heading', { level: 2 })).textContent).toBe(SECTOR_POSTURE_TITLE);
    expect(screen.getByLabelText('Posture counts')).toBeTruthy();
  });
});

describe('the AIB division card (schema 2)', () => {
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
      'Priority nodes with credible trusted coverage25%of 4 nodes members ask partners to supply or secure',
      'Unresolved gaps3priority nodes with a gap',
    ]);
    const card = screen.getByTestId('aib-division-hero');
    expect(card.textContent).not.toContain('Core coalition members');
    expect(within(card).getByRole('heading', { level: 3 }).textContent).toBe(AIB_DIVISION_TITLE);
    fireEvent.click(node);
    expect(screen.getByTestId('aib-node-detail').textContent).toMatch(/Single point of failure .*also insufficient redundancy/);
    expect(screen.queryByTestId('aib-node-detail-nodes')).toBeNull();
    // the extended gaps appear in the legend only when the division carries them
    expect(card.textContent).not.toContain('Excessive concentration');
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
    expect(lines).toHaveLength(6);
    expect(lines[0].split(',').slice(0, 3)).toEqual(['technology', 'stage', 'primary_holder']);
    expect(lines[1].endsWith('"Opportunity Security Indices, strategic position framework (allied division of labour)",2026-10-29,abc1234')).toBe(true);
    fireEvent.click(screen.getByRole('button', { name: 'Table' }));
    const table = screen.getByRole('table', { name: 'Allied holders and gaps by technology and stage' });
    const rows = within(table).getAllByRole('row').slice(1) as HTMLTableRowElement[];
    expect(rows.map((r) => `${r.cells[0].textContent} ${r.cells[1].textContent}`)).toEqual(['Batteries Extraction', 'Batteries Refining', 'Batteries Components', 'Solar Assembly']);
    noAcronyms();
    fireEvent.click(rows[3]);
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
    expect(within(m).getAllByRole('rowheader')).toHaveLength(1);
    cleanup();
    at('/', <AibDivisionCard tech="Solar" techFilter={false} />);
    expect(within(await screen.findByTestId('aib-matrix')).getAllByRole('rowheader').map((h) => h.textContent)).toEqual(['Solar']);
    expect(screen.queryByLabelText('Technology')).toBeNull();
  });

  it('keeps an h2 and the metrics inside when standing alone', async () => {
    at('/', <AibDivisionCard headingLevel={2} stats />);
    const hero = await screen.findByTestId('aib-division-hero');
    expect(within(hero).getByRole('heading', { level: 2 }).textContent).toBe(AIB_DIVISION_TITLE);
    expect(hero.textContent).toContain('Core coalition members');
    expect(hero.textContent).toContain('25%');
  });
});
