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
import { DRAWER_QUESTIONS, PostureNodeDrawer } from '../CellDrawer.tsx';
import { COUNTRY_STRATEGY_MAP_INTRO, COUNTRY_STRATEGY_MAP_TITLE, CountryPostureCard } from '../CountryPostureCard.tsx';
import { AibDivisionStats, CountryPostureTile } from '../PostureCounts.tsx';
import { MAP_CHOICE_DEFINITION, MIXED_DEFINITION, NOT_ASSESSED_DEFINITION } from '../PostureLegend.tsx';
import { MAP_HELPER, MAX_TIP_PARTS, PostureMatrix } from '../PostureMatrix.tsx';
import { SECTOR_POSTURE_TITLE, SectorPostureCard } from '../SectorPostureCard.tsx';
import type { PostureDetail, Verb } from '../contract.ts';
import { countryCells } from '../derive.ts';
import { VERB_COLOR, VERB_LABEL, VERB_OUTLINE } from '../grammar.ts';
import { setPostureStore } from '../store.ts';
import { YARD_AND_FENCE, YARD_DEFINITION } from '../../content/vocabulary.ts';
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

const SOURCE = '"Strategic position framework, computed by the Explorer\'s authors"';
const noAcronyms = () => expect(bareAcronyms(document.body)).toEqual([]);
const SENTENCE =
  'Aland should preserve its strongest positions in battery pack assembly and solar module assembly; secure battery materials refining (chokepoint risk), polysilicon refining (system resilience) and solar inverters (control-system risk); catch up in battery cell manufacturing; rely on partners for lithium and nickel mining; and accelerate deployment of battery storage.';

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
    expect(look(node(/^Batteries Refining & Processing: Secure · High fence; Chokepoint risk/))).toEqual(['secure', 'high', '#f58229']);
    expect(look(node(/^Solar Refining & Processing: Secure · High fence; System resilience/))).toEqual(['secure', 'high', '#f58229']);
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
    expect(sec.getAttribute('aria-label')).toContain('policy needs more attention');
    const gap = within(m).getByRole('button', { name: /^Batteries Deployment & Use/ });
    expect(gap.getAttribute('aria-label')).toContain('policy needs more attention: deployment policy lags');
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
    expect(tip.textContent).toContain('Best-placed partner: Bland');
    expect(tip.textContent).toContain('Policy: Wrong tool for the problem');
    fireEvent.blur(ext);
    fireEvent.focus(sec);
    expect(screen.getByRole('tooltip').textContent).toContain('Chokepoint risk');
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
    // one legend, grouped by fence: each fence headed by its yard in the user's words, each choice defined in one line
    expect(screen.getAllByTestId('posture-legend')).toHaveLength(1);
    const legend = screen.getByTestId('posture-legend');
    expect(legend.getAttribute('aria-label')).toBe('Legend: what each color means, by fence');
    const groups = [...legend.querySelectorAll('[data-fence]')] as HTMLElement[];
    expect(groups.map((g) => [g.getAttribute('data-fence'), g.getAttribute('data-yard')])).toEqual([['high', 'small'], ['medium', 'medium'], ['low', 'large']]);
    expect(groups.map((g) => [...g.querySelectorAll('[data-verb]')].map((v) => v.getAttribute('data-verb')))).toEqual([
      ['secure'], ['preserve', 'catch_up', 'partner', 'accelerate'], ['watch'],
    ]);
    expect(groups.map((g) => [...g.querySelectorAll('p')].map((p) => p.textContent))).toEqual(
      (['small', 'medium', 'large'] as const).map((y) => [YARD_AND_FENCE[y], YARD_DEFINITION[y]]),
    );
    expect(groups.map((g) => g.querySelector('p')?.textContent)).toEqual(['Small yard / High fence', 'Medium yard / medium, permeable fence', 'Large yard / Low fence']);
    // each entry: the name, the map's own step count and the one-line definition; each swatch its choice's colour (not assessed hatched)
    const m = screen.getByTestId('posture-matrix');
    const entries = [...legend.querySelectorAll('li[data-verb]')] as HTMLElement[];
    expect(entries.map((e) => e.getAttribute('data-verb'))).toEqual(['secure', 'preserve', 'catch_up', 'partner', 'accelerate', 'watch', 'not_assessed']);
    const steps = (n: number) => `${n} ${n === 1 ? 'step' : 'steps'}`;
    for (const it of entries) {
      const v = it.getAttribute('data-verb')!;
      const sw = it.querySelector('[data-swatch]') as HTMLElement;
      const n = m.querySelectorAll(`button[data-verb="${v}"]`).length;
      if (v === 'not_assessed') {
        expect(sw.style.background).toContain('repeating-linear-gradient');
        expect(it.textContent).toBe(`Not assessed · ${steps(n)} · no fence — ${NOT_ASSESSED_DEFINITION}`);
      } else {
        expect(cssHex(sw.style.background), v).toBe(VERB_COLOR[v as Verb]);
        expect(it.textContent, v).toBe(`${VERB_LABEL[v as Verb]} · ${steps(n)}${v === 'accelerate' ? ' · Deployment & Use only' : ''} — ${MAP_CHOICE_DEFINITION[v as Verb]}`);
      }
    }
    expect(NOT_ASSESSED_DEFINITION).toBe('The available data are not strong enough to make a strategic call.');
    expect(entries[0].textContent).toBe('Secure · 3 steps — Protect what cannot be allowed to fail.');
    expect(entries[5].textContent).toBe(
      'Watch · 3 steps — Keep open: buy competitively, do not force localization. At Deployment & Use, deployment need is below the bar for acceleration.',
    );
    // the legend leads on wide screens (above the map, inside the card); on a phone it follows the map, so the map stays near the top
    expect(legend.compareDocumentPosition(m) & Node.DOCUMENT_POSITION_FOLLOWING).toBeTruthy();
    expect(legend.className).toMatch(/\border-last\b.*\bsm:order-none\b/);
    // three columns only from the lg breakpoint: one column (no sideways scroll) on a phone
    expect(legend.className).toMatch(/\bgrid\b/);
    expect(legend.className).toMatch(/\blg:grid-cols-/);
    expect(legend.className).not.toMatch(/(^|\s)grid-cols-/);
    expect(screen.queryByTestId('strategy-counts')).toBeNull();
    expect(screen.getByTestId('trade-freshness').textContent).toBe('Structural trade benchmark: 2022–24 · Current signal through: Aug 2026');
    noAcronyms();
  });

  it('the page can carry the sentence: the sub then says how to read the map; without counts the legend keeps every definition', async () => {
    at('/', <CountryPostureCard iso3="AAA" name="Aland" showSentence={false} showCounts={false} />);
    await screen.findByTestId('posture-matrix');
    expect(screen.queryByTestId('posture-takeaway')).toBeNull();
    // how to read the map, under the title: rows, the six step columns with Deployment & Use set apart, color, the split, the hatch, the evidence
    const intro = screen.getByTestId('strategy-map-intro');
    expect(intro.textContent).toBe(COUNTRY_STRATEGY_MAP_INTRO);
    expect(intro.closest('p')?.previousElementSibling?.textContent).toBe(COUNTRY_STRATEGY_MAP_TITLE);
    for (const bit of [
      'Each row is a technology',
      'the six columns are the steps of its supply chain, from Extraction to Recovery & Recycling',
      'Deployment & Use set apart as the energy-system column',
      "A cell's color is the country's choice for that step",
      'groups the colors by fence',
      'A cell split on the diagonal has parts behind different fences: hover over it, or tap it, to see which.',
      'Hatched cells are not assessed.',
      'Select a cell for the evidence behind its choice.',
    ]) {
      expect(intro.textContent).toContain(bit);
    }
    expect(intro.textContent).not.toMatch(/\bnodes?\b|optimi[sz]e|leverage|unlock/i);
    const legend = screen.getByTestId('posture-legend');
    expect(legend.textContent).not.toMatch(/\d/);
    expect([...legend.querySelectorAll('li[data-verb]')].map((e) => e.textContent)).toEqual([
      ...(['secure', 'preserve', 'catch_up', 'partner'] as const).map((v) => `${VERB_LABEL[v]} — ${MAP_CHOICE_DEFINITION[v]}`),
      `Accelerate · Deployment & Use only — ${MAP_CHOICE_DEFINITION.accelerate}`,
      `Watch — ${MAP_CHOICE_DEFINITION.watch}`,
      `Not assessed · no fence — ${NOT_ASSESSED_DEFINITION}`,
    ]);
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
    expect(qs[0].textContent).toContain('Supply-chain vulnerability');
    expect(qs[0].textContent).toContain('Lack of trusted alternatives');
    expect(qs[1].textContent).toContain('Expected world demand growth');
    expect(qs[2].textContent).toContain('Competitive Position');
    expect(qs[2].textContent).toMatch(/Trade momentum.*rising/);
    // the choice with the page's fence (never the R family words); the policy design's trade protection is labelled as such, never "Fence:"
    expect(within(d).getByRole('heading', { level: 3 }).nextElementSibling?.textContent).toMatch(/^Partner · Medium fence/);
    expect(d.textContent).not.toMatch(/Compete ·|Deploy ·/);
    expect(within(qs[3]).getByTestId('drawer-fence').textContent).toBe('Medium fence');
    expect(qs[3].textContent).not.toMatch(/Fence:/);
    expect(qs[3].textContent).toContain('Bland');
    expect(qs[3].textContent).toContain('How the choice was made');
    expect(qs[4].textContent).toContain('Wrong tool for the problem');
    expect(qs[4].textContent).toContain('Protection or localization');
    expect(qs[4].textContent).toContain('it never feeds the recommendation');
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
    expect(tp.textContent).toBe('Fence (policy design): Screen coercive suppliers of refined battery materials.');
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
    expect(need.textContent).not.toContain('Supply-chain vulnerability');
    expect(need.textContent).not.toContain('Import dependence');
    expect((d.querySelector('[data-question="prize"]') as HTMLElement).textContent).toContain('Domestic manufacturing priority');
    expect((d.querySelector('[data-question="choice"]') as HTMLElement).textContent).toContain('Deploy a technology faster because the domestic energy system needs more of it. It says nothing about making the equipment at home. Domestic manufacturing priority, read at the producing stages, is');
    expect((d.querySelector('[data-question="policy"]') as HTMLElement).textContent).toContain('Needs more attention: deployment policy lags');
  });

  it('Secure says its rationale and mitigation; Watch is a deliberate low priority; not assessed is counted apart', async () => {
    at('/', <CountryPostureCard iso3="AAA" name="Aland" selected="Batteries|Refining" />);
    const d = await screen.findByTestId('cell-drawer');
    await waitFor(() => expect(d.textContent).toContain('Mitigation:'));
    expect(d.textContent).toContain('Chokepoint risk');
    expect(d.textContent).toContain('not necessarily self-sufficiency');
    cleanup();
    at('/', <CountryPostureCard iso3="AAA" name="Aland" selected="Solar|Extraction" />);
    const w = await screen.findByTestId('cell-drawer');
    await waitFor(() => expect(w.textContent).toContain('Low domestic priority (low need; low prize; ample diversified supply): a deliberate choice, not a failure.'));
    expect(w.textContent).toContain('not a failure');
    cleanup();
    at('/', <CountryPostureCard iso3="AAA" name="Aland" selected="Batteries|Recovery" />);
    const n = await screen.findByTestId('cell-drawer');
    // the export's reason in the page's words (Supply-chain need -> vulnerability, node -> step)
    await waitFor(() => expect(n.textContent).toContain('Supply-chain vulnerability is not measured for this step.'));
    expect(n.textContent).not.toContain('this node')
    expect(n.textContent).toContain('never as Watch');
    expect((n.querySelector('[data-question="policy"]') as HTMLElement).textContent).toContain('Not assessed, so there is no recommendation to compare policy with.');
  });

  it('Catch Up says its position from the cell: a top-quarter score that did not pass the Preserve test, or not measured', async () => {
    const choiceOf = async () => (await screen.findByTestId('cell-drawer')).querySelector('[data-question="choice"]') as HTMLElement;
    // Aland's battery cells: Catch Up with a position of 88, in the top quarter but not a Preserve (below the capability floor)
    setPostureStore(fixtureStore({ core: withCell(fixtureCore(), 0, 0, 2, { cp: 0.875 }) }));
    at('/', <CountryPostureCard iso3="AAA" name="Aland" selected="Batteries|Components" />);
    let choice = await choiceOf();
    expect(choice.textContent).toContain(
      'A large prize, a top-quarter score that does not pass the Preserve test (not a main producer, or below the capability floor), and a credible pathway: a real capability to build from (an anchor, corroborated by a signal of another kind).',
    );
    expect(choice.textContent).not.toContain('a position below the top quarter');
    cleanup();
    setPostureStore(fixtureStore({ core: withCell(fixtureCore(), 0, 0, 2, { cp: null }) }));
    at('/', <CountryPostureCard iso3="AAA" name="Aland" selected="Batteries|Components" />);
    choice = await choiceOf();
    expect(choice.textContent).toContain('A large prize, position not measured, and a credible pathway: a real capability to build from');
    cleanup();
    setPostureStore(fixtureStore());
    at('/', <CountryPostureCard iso3="AAA" name="Aland" selected="Batteries|Components" />);
    choice = await choiceOf();
    expect(choice.textContent).toContain('A large prize, a position below the top quarter, and a credible pathway: a real capability to build from');
  });

  it('Policy alignment gives support, protection and momentum as ranks within the country: never shares or a signed change', async () => {
    at('/', <CountryPostureCard iso3="AAA" name="Aland" selected="Batteries|Components" />);
    const d = await screen.findByTestId('cell-drawer');
    const policy = d.querySelector('[data-question="policy"]') as HTMLElement;
    await waitFor(() => expect(policy.textContent).toContain('Supportmoderate, 60th percentile in the country'));
    expect(policy.textContent).toContain('Observed policy intensitymoderate, 60th percentile in the country');
    // no protection measure recorded: not its tied rank
    expect(policy.textContent).toContain('Protection or localizationnone recorded');
    expect(policy.textContent).toContain('Policy momentum (recent activity)low, 12th percentile in the country');
    expect(policy.textContent).not.toMatch(/\+12%|30%|within the country\)/);
    cleanup();
    at('/', <CountryPostureCard iso3="AAA" name="Aland" selected="Batteries|Extraction" />);
    const e = (await screen.findByTestId('cell-drawer')).querySelector('[data-question="policy"]') as HTMLElement;
    await waitFor(() => expect(e.textContent).toContain('Protection or localizationhigh, 90th percentile in the country'));
    expect(e.textContent).toContain('Supportlow, 20th percentile in the country');
    expect(e.textContent).toContain('Policy momentum (recent activity)rising');
  });

  it('the table twin lists all 12 nodes, an unclassified one kept, and a row opens the drawer', async () => {
    // Aland's Solar Recovery has no posture
    setPostureStore(fixtureStore({ core: withoutCell(fixtureCore(), 0, 1, 5) }));
    at('/', <CountryPostureCard iso3="AAA" name="Aland" />);
    await screen.findByTestId('posture-matrix');
    expect(screen.getByRole('gridcell', { name: 'Solar Recovery & Recycling: no data' })).toBeTruthy();
    fireEvent.click(screen.getByRole('button', { name: 'Table' }));
    const table = screen.getByRole('table', { name: 'Aland: choice by technology and stage' });
    expect(within(table).getAllByRole('columnheader').map((h) => h.textContent?.replace(/[▲▼]/g, ''))).toEqual([
      'Technology', 'Stage', 'Choice', 'Secure rationale', 'Yards (with its layers)', 'Need type', 'Need', 'Strategic Industrial Value', 'Market prize', 'Competitive Position', 'World supply share', 'Credible pathway',
      'Policy alignment', 'Evidence', 'Trade momentum', 'First trusted partner', 'Borderline on', 'Consistency review',
    ]);
    const rows = within(table).getAllByRole('row').slice(1) as HTMLTableRowElement[];
    expect(rows).toHaveLength(12);
    expect(rows[1].textContent).toContain('Chokepoint risk');
    expect(rows[0].textContent).toContain('rising');
    expect(rows[11].textContent).toMatch(/^SolarRecovery & RecyclingNo data/);
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
      `Aland,AAA,core,Batteries,Refining,Refining & Processing,TRUE,Secure,,,Secure · High fence,Chokepoint risk,Supply-chain need,0.95,,0.4,0.4,Small yard,0.1,0.002,,Needs more attention,medium,Strong,high,,,,,,,${SOURCE},2026-10-29,abc1234`,
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
    expect((await screen.findByText('No strategy map')).closest('[role="status"]')?.textContent).toContain('There is not enough comparable evidence to produce a strategy map for Zland.');
    expect(screen.queryByRole('heading')).toBeNull();
  });

  it('without a momentum window it says why there is no current signal; a v1 bundle is refused with a notice', async () => {
    setPostureStore(fixtureStore({ momentum: false }));
    at('/', <CountryPostureCard iso3="AAA" name="Aland" />);
    expect((await screen.findByTestId('trade-freshness')).textContent).toContain('Current momentum unavailable — incomplete matched-period reporting');
    cleanup();
    setPostureStore(fixtureStore({ core: { ...fixtureCore(), schema_version: 1 } }));
    at('/', <CountryPostureCard iso3="AAA" name="Aland" />);
    expect((await screen.findByText('The strategy data could not be loaded')).closest('[role="status"], [role="alert"]')?.textContent).toMatch(/retired Need x Opportunity/);
  });

  it('keeps an h2 when standing alone', async () => {
    at('/', <CountryPostureCard iso3="AAA" name="Aland" headingLevel={2} />);
    expect((await screen.findByRole('heading', { level: 2 })).textContent).toBe(COUNTRY_STRATEGY_MAP_TITLE);
    expect(screen.getByTestId('posture-legend').textContent).toContain('Secure · 3 steps');
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
    expect(tag.textContent).toBe('Country strategy · fixed thresholds; the dials and the scenario do not change it');
    cleanup();
    render(<PostureBasisTag note={null} />);
    expect(document.querySelector('[data-basis="posture"]')?.textContent).toBe('Country strategy');
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
    expect(reading.textContent).toMatch(/^Each point is one economy\. Across: Strategic Industrial Value\s?, .*up: supply-chain need\s?\. Outlined diamonds are Secure.*Point size is Competitive Position\s?\.$/);
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
      'Priority steps with a credible trusted supplier and backup25%of 4 supply-chain steps coalition members rely on partners to supply or secure',
      'Remaining allied gaps3priority steps that still have a gap',
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
    expect(list.textContent).toBe('Allied Industrial Base steps in this cell: Cell makers, Separators');
    fireEvent.click(within(m).getByRole('button', { name: /^Batteries Refining/ }));
    expect(screen.getByTestId('aib-node-detail-nodes').textContent).toBe('No Allied Industrial Base step is mapped to this cell.');
  });

  it('the table twin and the CSV, with the posture provenance (not the scenario run)', async () => {
    at('/', <AibDivisionCard />);
    const card = await screen.findByTestId('aib-division-hero');
    await screen.findByTestId('aib-matrix');
    const lines = await downloadCsv(card);
    expect(lines).toHaveLength(6);
    expect(lines[0].split(',').slice(0, 3)).toEqual(['technology', 'stage', 'primary_holder']);
    expect(lines[1].endsWith('"Strategic position framework, computed by the Explorer\'s authors (allied division of labour)",2026-10-29,abc1234')).toBe(true);
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

describe('Yards & Fences on the Strategy Map and in the drawer (2026-10-30)', () => {
  beforeEach(() => setPostureStore(fixtureStore({ yards: true })));

  it("a mixed step is colour alone, split on the diagonal; the legend says what it means; until the detail is in, the tooltip and the name say which yards", async () => {
    // the page's detail document not in yet (null): the roll-up words
    at('/', <CountryPostureCard iso3="AAA" name="Aland" detail={null} />);
    const m = await screen.findByTestId('posture-matrix');
    const asm = within(m).getByRole('button', { name: /^Batteries Final Assembly & Equipment/ });
    const sol = within(m).getByRole('button', { name: /^Solar Final Assembly & Equipment/ });
    // the node keeps its own choice; the split is marked, and the yards it spans named
    expect([asm.getAttribute('data-verb'), asm.getAttribute('data-mixed'), asm.getAttribute('data-yards')]).toEqual(['preserve', 'true', 'small medium large']);
    expect([sol.getAttribute('data-verb'), sol.getAttribute('data-mixed'), sol.getAttribute('data-yards')]).toEqual(['preserve', 'true', 'medium large']);
    // still no letter, arrow or glyph in a cell
    for (const b of [asm, sol]) expect([b.textContent, b.children.length]).toEqual(['', 0]);
    // a Large-yard part shows the Watch grey: the light hairline makes it a mark on the card
    expect(cssHex(sol.style.border)).toBe(VERB_OUTLINE.watch);
    // a step whose layers agree is not split
    const ref = within(m).getByRole('button', { name: /^Batteries Refining & Processing/ });
    expect([ref.getAttribute('data-mixed'), ref.getAttribute('data-yards'), cssHex(ref.style.background)]).toEqual([null, 'small', VERB_COLOR.secure]);
    expect(asm.getAttribute('aria-label')).toContain('Preserve · Medium fence; mixed yards: this step in the Medium yard, its layers also in the Small and Large yards;');
    // the legend: one entry for the mixed steps, defined, with its count and a split swatch
    const mixed = within(screen.getByTestId('posture-legend')).getByTestId('legend-mixed');
    expect(mixed.textContent).toBe(`Mixed · 2 steps — ${MIXED_DEFINITION}`);
    expect(MIXED_DEFINITION).toBe('A cell split on the diagonal: parts of the step sit behind different fences. Hover over it, or tap it, to see which.');
    expect(mixed.querySelector('[data-swatch="mixed"]')).toBeTruthy();
    expect(screen.getByTestId('strategy-map-mixed-note').textContent).toMatch(/A cell split on the diagonal is a mixed step: its capability layers .* sit in different\s*yards\s*, behind different fences/);
    expect(screen.getByTestId('strategy-map-mixed-note').textContent).toContain('(orange the High fence, blue the Medium fence, gray the Low fence)');
    // the tooltip names the yards, leading (right after the choice), and no parts list yet
    fireEvent.mouseEnter(asm);
    const tip = screen.getByRole('tooltip');
    expect(within(tip).getByTestId('strategy-map-tip-yards').textContent).toBe('Mixed: this step in the Medium yard, its layers also in the Small and Large yards');
    expect(within(tip).getByTestId('strategy-map-tip-yards').previousElementSibling).toBe(within(tip).getByTestId('strategy-map-tip-choice'));
    expect(within(tip).queryByTestId('strategy-map-tip-parts')).toBeNull();
    fireEvent.mouseLeave(asm);
    fireEvent.focus(ref);
    expect(within(screen.getByRole('tooltip')).queryByTestId('strategy-map-tip-yards')).toBeNull();
    noAcronyms();
  });

  it("once the economy's detail is in, a layered step's tooltip lists its parts (the rest of the step and each layer, its choice and fence), a mixed step leading with them; the name names the parts' fences", async () => {
    at('/', <CountryPostureCard iso3="AAA" name="Aland" />);
    const m = await screen.findByTestId('posture-matrix');
    const asm = within(m).getByRole('button', { name: /^Batteries Final Assembly & Equipment/ });
    // the accessible name: the parts' fences, briefly, in place of the roll-up words
    await waitFor(() => expect(asm.getAttribute('aria-label')).toContain('parts behind different fences'));
    expect(asm.getAttribute('aria-label')).toBe(
      'Batteries Final Assembly & Equipment: Preserve · Medium fence; parts behind different fences: High fence: Battery-management firmware, energy-management software and remote updates; Medium fence: the rest of the step, Battery cells (LFP and NMC); Low fence: Enclosures, racking, thermal management and balance of system; supply-chain vulnerability 40; prize 90; position 95; policy broadly aligned; evidence weak; borderline',
    );
    expect(asm.getAttribute('aria-label')).not.toContain('mixed yards:');
    expect(within(m).getByRole('button', { name: /^Solar Final Assembly & Equipment/ }).getAttribute('aria-label')).toContain(
      'Preserve · Medium fence; parts behind different fences: Medium fence: the rest of the step; Low fence: Passive racking and mounting;',
    );
    // a step without layers keeps its name as it was
    expect(within(m).getByRole('button', { name: /^Batteries Refining & Processing/ }).getAttribute('aria-label')).not.toMatch(/parts|mixed/);
    // hover: the mixed step leads with its parts, right after its choice; one swatch per part in its choice colour
    fireEvent.mouseEnter(asm);
    const tip = screen.getByRole('tooltip');
    expect(asm.getAttribute('aria-describedby')).toBe(tip.id);
    const parts = within(tip).getByTestId('strategy-map-tip-parts');
    expect(parts.previousElementSibling).toBe(within(tip).getByTestId('strategy-map-tip-choice'));
    expect(parts.getAttribute('data-mixed')).toBe('true');
    expect(within(tip).queryByTestId('strategy-map-tip-yards')).toBeNull();
    // the rest of the step, then the layers behind another fence (High to Low), then those behind the step's own fence
    const lines = [...parts.querySelectorAll('li')] as HTMLElement[];
    expect(lines.map((l) => [l.getAttribute('data-part'), l.getAttribute('data-verb'), l.getAttribute('data-fence'), l.textContent])).toEqual([
      ['step', 'preserve', 'medium', 'Rest of the step: Preserve · Medium fence'],
      ['bess-control', 'secure', 'high', 'Battery-management firmware, energy-management software and remote updates: Secure · High fence'],
      ['bess-enclosure', 'watch', 'low', 'Enclosures, racking, thermal management and balance of system: Watch · Low fence'],
      ['bess-cells', 'preserve', 'medium', 'Battery cells (LFP and NMC): Preserve · Medium fence'],
    ]);
    for (const l of lines) expect(cssHex((l.querySelector('[data-swatch]') as HTMLElement).style.background)).toBe(VERB_COLOR[l.getAttribute('data-verb') as Verb]);
    expect(parts.querySelector('p')?.textContent).toBe('Mixed: parts of this step sit behind different fences');
    expect(within(tip).queryByTestId('strategy-map-tip-more')).toBeNull();
    // the tooltip is fixed to the viewport (never cut off by the map's scroller) and at most the screen less its margins
    expect(tip.className).toMatch(/\bfixed\b/);
    expect(tip.className).toContain('max-w-[calc(100vw-16px)]');
    fireEvent.mouseLeave(asm);
    expect(screen.queryByRole('tooltip')).toBeNull();
    expect(asm.getAttribute('aria-describedby')).toBeNull();
    noAcronyms();
  });

  it('the tooltip caps its parts (then "+N more: open the step"), puts the parts of a step that is not mixed after its numbers, and stays inside a 375 px screen', async () => {
    const store = fixtureStore({ yards: true });
    const core = await store.core();
    const detail = await store.detail('AAA');
    const cells = countryCells(core, 'AAA');
    // seven layers on Batteries Final Assembly, and one Secure layer on Batteries Refining (all High fence: not mixed)
    const asm = detail.cells.find((c) => c.key === 'Batteries|Assembly')!;
    asm.yardChildren = [
      ...asm.yardChildren,
      ...['a', 'b', 'c', 'd'].map((x) => ({ layerId: `extra-${x}`, yard: 'medium' as const, choice: 'catch_up' as const, variant: 'compete_catch_up' })),
    ];
    detail.cells.find((c) => c.key === 'Batteries|Refining')!.yardChildren = [{ layerId: 'refining-control', yard: 'small', choice: 'secure', variant: 'secure_control' }];
    const rect = (left: number, top: number) => ({ left, top, right: left + 20, bottom: top + 36, width: 20, height: 36, x: left, y: top, toJSON: () => ({}) }) as DOMRect;
    const ow = Object.getOwnPropertyDescriptor(HTMLElement.prototype, 'offsetWidth');
    const oh = Object.getOwnPropertyDescriptor(HTMLElement.prototype, 'offsetHeight');
    const iw = window.innerWidth;
    const ih = window.innerHeight;
    Object.defineProperty(HTMLElement.prototype, 'offsetWidth', { configurable: true, get: () => 288 });
    Object.defineProperty(HTMLElement.prototype, 'offsetHeight', { configurable: true, get: () => 150 });
    Object.defineProperty(window, 'innerWidth', { configurable: true, value: 375 });
    Object.defineProperty(window, 'innerHeight', { configurable: true, value: 812 });
    try {
      at('/', <PostureMatrix core={core} cells={cells} selected={null} onSelect={() => undefined} detail={detail} />);
      const m = await screen.findByTestId('posture-matrix');
      const big = within(m).getByRole('button', { name: /^Batteries Final Assembly & Equipment/ });
      // the name caps its names too, never at the cost of a part behind another fence
      expect(big.getAttribute('aria-label')).toContain(
        'parts behind different fences: High fence: Battery-management firmware, energy-management software and remote updates; Medium fence: the rest of the step, Battery cells (LFP and NMC), extra a, extra b, 2 more; Low fence: Enclosures, racking, thermal management and balance of system;',
      );
      // at the right edge of a phone: clamped inside the screen, under the cell
      big.getBoundingClientRect = () => rect(340, 100);
      fireEvent.mouseEnter(big);
      let tip = screen.getByRole('tooltip');
      const lines = [...within(tip).getByTestId('strategy-map-tip-parts').querySelectorAll('li')];
      expect(lines).toHaveLength(MAX_TIP_PARTS - 1);
      // the parts behind another fence than the step's own are never the ones cut
      expect(lines.map((l) => l.getAttribute('data-part'))).toEqual(['step', 'bess-control', 'bess-enclosure', 'bess-cells', 'extra-a']);
      expect(within(tip).getByTestId('strategy-map-tip-more').textContent).toBe('+3 more: open the step');
      expect([tip.style.left, tip.style.top, tip.style.visibility]).toEqual([`${375 - 288 - 8}px`, '140px', '']);
      fireEvent.mouseLeave(big);
      // at the left edge, low on the screen: clamped to the 8 px margin and flipped above the cell
      const ext = within(m).getByRole('button', { name: /^Batteries Extraction/ });
      ext.getBoundingClientRect = () => rect(0, 770);
      fireEvent.mouseEnter(ext);
      tip = screen.getByRole('tooltip');
      expect([tip.style.left, tip.style.top]).toEqual(['8px', `${770 - 150 - 4}px`]);
      fireEvent.mouseLeave(ext);
      // a layered step that is not mixed: its parts follow its numbers, under a plain heading
      const ref = within(m).getByRole('button', { name: /^Batteries Refining & Processing/ });
      expect(ref.getAttribute('aria-label')).toContain('Secure · High fence; Chokepoint risk; both parts behind the High fence;');
      fireEvent.mouseEnter(ref);
      tip = screen.getByRole('tooltip');
      const parts = within(tip).getByTestId('strategy-map-tip-parts');
      expect(parts.getAttribute('data-mixed')).toBeNull();
      expect(parts.querySelector('p')?.textContent).toBe('Parts of this step');
      expect(parts.nextElementSibling).toBeNull();
      expect(tip.textContent).toMatch(/Supply-chain vulnerability 95.*Parts of this stepRest of the step: Secure · High fencerefining control: Secure · High fence$/);
    } finally {
      if (ow) Object.defineProperty(HTMLElement.prototype, 'offsetWidth', ow);
      if (oh) Object.defineProperty(HTMLElement.prototype, 'offsetHeight', oh);
      Object.defineProperty(window, 'innerWidth', { configurable: true, value: iw });
      Object.defineProperty(window, 'innerHeight', { configurable: true, value: ih });
    }
  });

  it('without mixed steps the legend has no Mixed entry', async () => {
    setPostureStore(fixtureStore());
    at('/', <CountryPostureCard iso3="AAA" name="Aland" />);
    await screen.findByTestId('posture-matrix');
    expect(screen.queryByTestId('legend-mixed')).toBeNull();
    expect(screen.queryByTestId('strategy-map-mixed-note')).toBeNull();
  });

  it("the drawer's Prize is the Strategic Industrial Value with its three families; its Choice has the Yards part: own yard, roll-up, the fence scope, the large-yard case (the layers are listed once, at the top)", async () => {
    at('/', <CountryPostureCard iso3="AAA" name="Aland" />);
    const m = await screen.findByTestId('posture-matrix');
    fireEvent.click(within(m).getByRole('button', { name: /^Batteries Final Assembly & Equipment/ }));
    const d = await screen.findByTestId('cell-drawer');
    const qs = within(d).getAllByTestId('drawer-question');
    expect(qs).toHaveLength(5);
    expect(qs[1].querySelector('p')?.textContent).toBe('Is productive capability here strategically valuable?');
    await waitFor(() => expect(within(qs[1]).getByTestId('drawer-prize').textContent).toContain('Expected world demand growth'));
    const prize = [...within(qs[1]).getByTestId('drawer-prize').querySelectorAll('dt')].map((dt) => [dt.textContent?.trim(), dt.nextElementSibling?.textContent]);
    expect(prize).toEqual([
      ['Strategic Industrial Value', '90% (high at 60% or more)'],
      ['Market prize (Industrial Opportunity)', '85%'],
      ['Expected world demand growth', '90%'],
      ['Investment momentum', 'not assessed'],
      ['Home-market growth (national data only)', 'not assessed'],
      ['Basis', 'global context'],
      ['Industrial commons (process knowledge, workforce and suppliers, cross-sector spillover)', '100% (3 of 3 attributes observed)'],
      ['Future option value (the leap tests: is a credible leap ahead in reach?)', '50% (4 of 4 leap tests observed)'],
    ]);
    // Choice: the trace counts the large-yard signals, the pivotal input is named
    expect(qs[3].textContent).toContain('Large-yard case (commodity signals): 2 signals (bar 5 signals) → no');
    expect(qs[3].textContent).toContain('Strategic Industrial Value: 90% (bar 60%) → high');
    expect(qs[3].textContent).toMatch(/Pivotal inputs \(would change the choice\)\s*Strategic Industrial Value/);
    // the kind of choice (the engine variant) in words
    expect(within(qs[3]).getByTestId('drawer-variant').textContent).toBe('A strong position worth keeping');
    const y = within(qs[3]).getByTestId('drawer-yards');
    expect(within(y).getByTestId('drawer-own-yard').textContent).toBe('Medium yard · Medium fence');
    const roll = within(y).getByTestId('drawer-yard-rollup');
    expect([roll.getAttribute('data-yards'), roll.textContent]).toEqual(['mixed_high_medium_low', 'Mixed: this step in the Medium yard, its layers also in the Small and Large yards']);
    // the layers are not listed twice: the Yards part points to the parts block at the top
    expect(within(y).queryAllByTestId('drawer-layer')).toHaveLength(0);
    expect(within(y).getByTestId('drawer-yards-parts').textContent).toBe('Each layer of this step has its own choice and fence: see Parts of this step, at the top.');
    expect(d.querySelectorAll('[data-testid="drawer-layer"]')).toHaveLength(3);
    expect(within(y).getByTestId('drawer-fence-scope').textContent).toBe('The high fence of a Secure layer here extends to the hardware it is built into, not the layer alone.');
    expect(within(y).getByTestId('drawer-large-yard').textContent).toBe(
      'Large-yard case for the step: Does not hold: 2 commodity signals hold (5 needed).Required signals (mature, passive) that hold: passive.Commodity signals that hold: substitutable, quick to qualify.',
    );
    // no market-only cap on this node
    expect(within(y).queryByTestId('drawer-yard-cap')).toBeNull();
    noAcronyms();
  });

  it('the drawer lists the parts of a step with layers right under its header, before the five questions: swatch, name, choice, fence and reason; a layer row marks that layer', async () => {
    at('/', <CountryPostureCard iso3="AAA" name="Aland" />);
    const m = await screen.findByTestId('posture-matrix');
    fireEvent.click(within(m).getByRole('button', { name: /^Batteries Final Assembly & Equipment/ }));
    const d = await screen.findByTestId('cell-drawer');
    const block = await within(d).findByTestId('drawer-parts');
    await waitFor(() => expect(within(block).getAllByTestId('drawer-layer')).toHaveLength(3));
    // placed under the header (the title, the choice chip), before the first question
    const qs = within(d).getAllByTestId('drawer-question');
    expect(block.compareDocumentPosition(qs[0]) & Node.DOCUMENT_POSITION_FOLLOWING).toBeTruthy();
    expect(within(d).getByRole('heading', { level: 3 }).compareDocumentPosition(block) & Node.DOCUMENT_POSITION_FOLLOWING).toBeTruthy();
    expect(qs.some((q) => q.contains(block))).toBe(false);
    expect(within(block).getByRole('heading', { level: 4 }).textContent).toBe('Parts of this step');
    expect([block.getAttribute('data-mixed'), within(block).getByTestId('drawer-parts-lead').textContent]).toEqual([
      'true',
      "Parts of this step sit behind different fences: High fence, Medium fence, Low fence. The rest of the step takes the step's own choice; each layer takes its own. A control layer is in the Small yard only where the data show coercive foreign control for this country.",
    ]);
    // the same parts as the map's tooltip, each with its swatch, choice, fence and one-line reason
    const rows = [...within(block).getByRole('list').children] as HTMLElement[];
    expect(rows.map((r) => [r.getAttribute('data-part'), r.getAttribute('data-verb'), r.getAttribute('data-fence')])).toEqual([
      ['step', 'preserve', 'medium'], ['bess-control', 'secure', 'high'], ['bess-enclosure', 'watch', 'low'], ['bess-cells', 'preserve', 'medium'],
    ]);
    for (const r of rows) expect(cssHex((r.querySelector('[data-swatch]') as HTMLElement).style.background)).toBe(VERB_COLOR[r.getAttribute('data-verb') as Verb]);
    expect(rows[0].textContent).toBe('Rest of the step · Preserve · Medium fenceA strong position worth keeping.');
    expect(rows[1].textContent).toBe(
      'Battery-management firmware, energy-management software and remote updates · Secure · High fenceControl built partly into the hardware: the high fence extends to the hardware.',
    );
    expect(within(rows[2]).getByTestId('drawer-layer-reason').textContent).toMatch(/^Large-yard case: mature, passive, .*; kept with a capability floor\.$/);
    expect(within(rows[3]).getByTestId('drawer-layer-reason').textContent).toBe('A strong position worth keeping: industrial commons 100, future option 50.');
    expect(rows[0].querySelector('button')).toBeNull();
    // a layer row is a button: it marks that layer (pressed, ringed, focused) and names it at the top of the block; again, it clears
    const ctl = within(rows[1]).getByRole('button');
    expect(ctl.getAttribute('aria-pressed')).toBe('false');
    expect(within(block).queryByTestId('drawer-layer-note')).toBeNull();
    fireEvent.click(ctl);
    await waitFor(() => expect(rows[1].getAttribute('data-focused')).toBe('true'));
    expect(ctl.getAttribute('aria-pressed')).toBe('true');
    expect(document.activeElement).toBe(rows[1]);
    expect(within(block).getByTestId('drawer-layer-note').textContent).toContain('Showing the layer Battery-management firmware, energy-management software and remote updates (Secure · Small yard · High fence)');
    fireEvent.click(within(rows[3]).getByRole('button'));
    await waitFor(() => expect(rows[3].getAttribute('data-focused')).toBe('true'));
    expect([rows[1].getAttribute('data-focused'), d.querySelectorAll('[data-focused="true"]').length]).toEqual([null, 1]);
    fireEvent.click(within(rows[3]).getByRole('button'));
    await waitFor(() => expect(d.querySelectorAll('[data-focused="true"]')).toHaveLength(0));
    expect(within(block).queryByTestId('drawer-layer-note')).toBeNull();
    // a step without layers has no parts block
    fireEvent.click(within(m).getByRole('button', { name: /^Batteries Component Manufacturing/ }));
    await waitFor(() => expect(within(screen.getByTestId('cell-drawer')).getByRole('heading', { level: 3 }).textContent).toBe('Batteries · Component Manufacturing'));
    expect(screen.queryByTestId('drawer-parts')).toBeNull();
    noAcronyms();
  });

  it('before the detail is in, the parts block of a mixed step says which yards it spans; a step whose URL holds the selection writes the marked layer to ?layer=', async () => {
    // the drawer with the detail still loading: the roll-up words
    let release: () => void = () => undefined;
    const store = fixtureStore({ yards: true });
    const detail = store.detail.bind(store);
    store.detail = (iso3: string) => new Promise<PostureDetail>((res) => (release = () => res(detail(iso3))));
    setPostureStore(store);
    at('/', <PostureNodeDrawer iso3="AAA" selected="Batteries|Assembly" onClose={() => undefined} />);
    const d = await screen.findByTestId('cell-drawer');
    const block = within(d).getByTestId('drawer-parts');
    expect(within(block).getByTestId('drawer-parts-rollup').textContent).toBe('Mixed: this step in the Medium yard, its layers also in the Small and Large yards');
    expect(block.textContent).toContain('Loading the details');
    release();
    await waitFor(() => expect(within(screen.getByTestId('drawer-parts')).getAllByTestId('drawer-layer')).toHaveLength(3));
    cleanup();
    // on a page whose URL holds the selection (?node=), a layer row writes ?layer= beside it
    setPostureStore(fixtureStore({ yards: true }));
    const r = at('/?node=Batteries%7CAssembly', <PostureNodeDrawer iso3="AAA" selected="Batteries|Assembly" onClose={() => undefined} />);
    const d2 = await screen.findByTestId('cell-drawer');
    const enc = await within(d2).findByText('Enclosures, racking, thermal management and balance of system');
    fireEvent.click(enc.closest('button') as HTMLElement);
    await waitFor(() => expect(new URLSearchParams(r.state.location.search).get('layer')).toBe('bess-enclosure'));
    expect(new URLSearchParams(r.state.location.search).get('node')).toBe('Batteries|Assembly');
  });

  it("the drawer of a Watch step: the economy's own large-yard case, the signals and the market-only cap; Deployment & Use has no large-yard case", async () => {
    at('/', <CountryPostureCard iso3="AAA" name="Aland" />);
    const m = await screen.findByTestId('posture-matrix');
    fireEvent.click(within(m).getByRole('button', { name: /^Solar Extraction/ }));
    const d = await screen.findByTestId('cell-drawer');
    const y = await within(d).findByTestId('drawer-yards');
    await waitFor(() => expect(within(y).getByTestId('drawer-large-yard').textContent).toContain('(this economy)'));
    expect(within(y).getByTestId('drawer-own-yard').textContent).toBe('Large yard · Low fence');
    expect(within(d).getByTestId('drawer-variant').textContent).toBe('Low domestic priority');
    expect(within(y).getByTestId('drawer-large-yard').textContent).toBe(
      'Large-yard case for the step (this economy): Holds: affordable, diversified access should dominate, whatever the size of the market (6 commodity signals hold); kept with a capability floor (moderate industrial learning).Required signals (mature, passive) that hold: mature, passive.Commodity signals that hold: substitutable, quick to qualify, diversified world supply, little process learning, little cross-sector spillover, little product differentiation.',
    );
    expect(within(y).getByTestId('drawer-yard-cap').textContent).toMatch(/^Market only: neither industrial commons nor future option reaches the gate/);
    expect(within(d).getAllByTestId('drawer-question')[1].textContent).toContain('20% (high at 60% or more); capped: market only');
    fireEvent.click(within(m).getByRole('button', { name: /^Batteries Deployment & Use/ }));
    await waitFor(() => expect(within(screen.getByTestId('cell-drawer')).getByRole('heading', { level: 3 }).textContent).toBe('Batteries · Deployment & Use'));
    const dy = within(screen.getByTestId('cell-drawer')).getByTestId('drawer-yards');
    expect(within(dy).getByTestId('drawer-own-yard').textContent).toBe('Medium yard · Medium fence');
    expect(within(dy).queryByTestId('drawer-large-yard')).toBeNull();
  });

  it('a layer link opens its step focused on the layer: the row is marked, scrolled to and focused', async () => {
    at('/', <PostureNodeDrawer iso3="AAA" selected="Batteries|Assembly" layer="bess-control" onClose={() => undefined} />);
    const d = await screen.findByTestId('cell-drawer');
    // the layer's name is its row in the parts block (and the note's button at the top of the block)
    const named = await within(d).findAllByText('Battery-management firmware, energy-management software and remote updates');
    const li = named.map((x) => x.closest('li')).find(Boolean) as HTMLElement;
    await waitFor(() => expect(document.activeElement).toBe(li));
    expect([li.getAttribute('data-layer'), li.getAttribute('data-focused')]).toEqual(['bess-control', 'true']);
    expect(within(li).getByRole('button').getAttribute('aria-pressed')).toBe('true');
    expect(within(d).getByTestId('drawer-parts').contains(li)).toBe(true);
    expect(d.querySelectorAll('[data-focused="true"]')).toHaveLength(1);
    // the top of the block says the heading is the step's and names the layer's own choice, with a way back to it
    const note = within(d).getByTestId('drawer-layer-note');
    expect(note.textContent).toBe(
      'Showing the layer Battery-management firmware, energy-management software and remote updates (Secure · Small yard · High fence). The choice at the top and the five questions are for the whole step.',
    );
    expect(note.compareDocumentPosition(li) & Node.DOCUMENT_POSITION_FOLLOWING).toBeTruthy();
    li.blur();
    fireEvent.click(within(note).getByRole('button'));
    expect(document.activeElement).toBe(li);
    cleanup();
    // a layer the step does not have: nothing focused, the drawer itself takes focus
    at('/', <PostureNodeDrawer iso3="AAA" selected="Batteries|Assembly" layer="no-such-layer" onClose={() => undefined} />);
    const d2 = await screen.findByTestId('cell-drawer');
    await within(d2).findAllByTestId('drawer-layer');
    expect(d2.querySelectorAll('[data-focused="true"]')).toHaveLength(0);
    expect(within(d2).queryByTestId('drawer-layer-note')).toBeNull();
    expect(document.activeElement).toBe(d2);
  });

  it('an older core (its prize the market measure): the drawer shows Industrial Opportunity alone, without the families', async () => {
    setPostureStore(fixtureStore({ core: fixtureCore(true, { legacy: true }) }));
    at('/', <CountryPostureCard iso3="AAA" name="Aland" />);
    const m = await screen.findByTestId('posture-matrix');
    fireEvent.click(within(m).getByRole('button', { name: /^Batteries Component Manufacturing/ }));
    const d = await screen.findByTestId('cell-drawer');
    const p = within(d).getAllByTestId('drawer-question')[1];
    await waitFor(() => expect(p.textContent).toContain('Expected world demand growth'));
    expect([...p.querySelectorAll('dt')][0].textContent?.trim()).toBe('Industrial Opportunity');
    expect(p.textContent).not.toMatch(/Strategic Industrial Value|Industrial commons|Future option|Market prize/);
    expect(screen.queryByTestId('legend-mixed')).toBeNull();
  });
});
