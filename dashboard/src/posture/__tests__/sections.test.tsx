// The Country page's reusable sections on the schema-2 fixture (spec XII-XVI, XIX; design 4, 5b, D8): the
// Need panels, the Prize & Position scatter, the Strategic Choices, the Policy Alignment table and the Allied
// Strategy, each a presentational ChartCard (data in as props, a table twin and a CSV), and the shared node
// selection: every entry point writes ?node=<Tech>|<Stage> and opens the same drawer; an invalid value is
// reported as ignored. The test page below is how the Country page is meant to mount them.

import { afterAll, afterEach, beforeAll, beforeEach, describe, expect, it, vi } from 'vitest';
import { cleanup, fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import { createMemoryRouter, RouterProvider } from 'react-router';
import { bareAcronyms } from '../../components/testing/acronyms.ts';
import { cssHex } from '../../components/testing/color.ts';
import { placeQuadLabels, QUAD_LABEL_DY, quadLeft } from '../../charts/quadScatter.tsx';
import { textWidth } from '../../charts/theme.ts';
import { AlliedStrategy, EXPLORE_PARTNERSHIPS } from '../AlliedStrategy.tsx';
import { PostureNodeDrawer } from '../CellDrawer.tsx';
import { decodeCore, checkPostureDetail } from '../contract.ts';
import { CountryPostureCard } from '../CountryPostureCard.tsx';
import { countryCells } from '../derive.ts';
import { DEPLOYMENT_NEED_TITLE, NeedPanels, SUPPLY_NEED_TITLE } from '../NeedPanels.tsx';
import { POLICY_ALIGNMENT_TITLE, PolicyAlignmentTable } from '../PolicyAlignmentTable.tsx';
import { PRIZE_POSITION_TITLE, PrizePositionScatter } from '../PrizePositionScatter.tsx';
import type { Verb } from '../contract.ts';
import { VERB_COLOR, VERB_OUTLINE } from '../grammar.ts';
import { useNodeSelection } from '../selection.ts';
import { setPostureStore, usePostureCore, usePostureDetail } from '../store.ts';
import { STRATEGIC_CHOICES_TITLE, StrategicChoices } from '../StrategicChoices.tsx';
import { fixtureCore, fixtureDetail, fixtureStore } from './fixture.ts';

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

const core = decodeCore(fixtureCore());
const detail = checkPostureDetail(fixtureDetail());
const cells = countryCells(core, 'AAA');
const noAcronyms = () => expect(bareAcronyms(document.body)).toEqual([]);

function inRouter(element: React.ReactNode, path = '/') {
  const router = createMemoryRouter([{ path: '*', element }], { initialEntries: [path] });
  render(<RouterProvider router={router} />);
  return router;
}

/** The Country page's composition: one selection in ?node=, every section an entry point, one drawer. */
function CountryPage() {
  const c = usePostureCore();
  const d = usePostureDetail('AAA');
  const sel = useNodeSelection(c.data?.meta ?? null);
  if (!c.data) return <p>Loading</p>;
  const cs = countryCells(c.data, 'AAA');
  const common = { core: c.data, cells: cs, detail: d.data, name: 'Aland', selected: sel.selected, onOpen: sel.select };
  return (
    <>
      {sel.problem && <p data-testid="node-ignored">Ignored in the link: {sel.problem}</p>}
      <div data-testid="s-strategy">
        <CountryPostureCard iso3="AAA" name="Aland" selected={sel.selected} onSelect={sel.select} drawer={false} />
      </div>
      <div data-testid="s-need">
        <NeedPanels {...common} />
      </div>
      <div data-testid="s-prize">
        <PrizePositionScatter {...common} />
      </div>
      <div data-testid="s-choices">
        <StrategicChoices {...common} aibHref={(t, s) => `/aib?node=${t}|${s}`} />
      </div>
      <div data-testid="s-policy">
        <PolicyAlignmentTable {...common} />
      </div>
      <div data-testid="s-allied">
        <AlliedStrategy {...common} partnerHref={(i) => `/countries/${i}`} aibHref={(t, s) => `/aib?node=${t}|${s}`} explore={<p>generic partnership material</p>} />
      </div>
      <PostureNodeDrawer iso3="AAA" name="Aland" selected={sel.selected} onClose={() => sel.select(null)} />
    </>
  );
}

describe('Need (spec XII)', () => {
  it('panel A ranks producing-stage nodes, Secure first, with reason tags; panel B ranks deployment need with its families and named scenarios', () => {
    const onOpen = vi.fn();
    inRouter(<NeedPanels core={core} cells={cells} detail={detail} name="Aland" onOpen={onOpen} limit={4} />);
    expect(screen.getByRole('heading', { name: SUPPLY_NEED_TITLE })).toBeTruthy();
    expect(screen.getByRole('heading', { name: DEPLOYMENT_NEED_TITLE })).toBeTruthy();
    const a = screen.getByTestId('supply-need-ranking');
    const rows = within(a).getAllByRole('button');
    expect(rows.map((b) => b.getAttribute('data-key'))).toEqual(['Batteries|Refining', 'Solar|Refining', 'Solar|Components', 'Batteries|Extraction']);
    expect(rows[0].getAttribute('aria-label')).toBe('Batteries Refining & Processing: supply-chain need 95; Secure, chokepoint risk; CHOKEPOINT, HIGH DEPENDENCE');
    expect(within(rows[2]).getByTestId('need-tags').textContent).toBe('CONTROL LAYERHIGH DEPENDENCE');
    fireEvent.click(screen.getByRole('button', { name: 'Show all 9' }));
    expect(within(a).getAllByRole('button')).toHaveLength(9);
    fireEvent.click(rows[1]);
    expect(onOpen).toHaveBeenLastCalledWith('Solar|Refining');
    const b = screen.getByTestId('deployment-need-ranking');
    const deps = within(b).getAllByRole('button');
    expect(deps.map((x) => x.getAttribute('data-key'))).toEqual(['Batteries|Deployment', 'Solar|Deployment']);
    const fam = within(deps[0]).getByTestId('deployment-families').textContent;
    expect(fam).toContain('Demand: 90 · Current policy · national data');
    expect(fam).toContain('Target gap: 80 · National commitments · national data');
    expect(fam).toContain('System: not assessed');
    expect(fam).toContain('Alternative, climate-aligned: +104% vs current policies · Climate-aligned · World 2035, Net Zero vs Current Policies · global context');
    expect(screen.getByTestId('deployment-scenarios').textContent).toContain('demand adequacy reads Current policy; the target or climate gap reads National commitments');
    expect(screen.getByTestId('deployment-scenarios').textContent).toContain('does not enter the choice');
    noAcronyms();
  });

  it('without the detail: the totals alone, and a word that the families are loading', () => {
    inRouter(<NeedPanels core={core} cells={cells} detail={null} name="Aland" onOpen={() => undefined} />);
    expect(screen.queryByTestId('deployment-families')).toBeNull();
    expect(screen.getByText(/The family breakdown loads/)).toBeTruthy();
  });
});

describe('Prize & Position (spec XIII)', () => {
  it('plots the producing-stage nodes by prize and position on the quadrant-scatter look: a dot per node in its choice colour, Secure at its true coordinates', () => {
    const onOpen = vi.fn();
    inRouter(<PrizePositionScatter core={core} cells={cells} name="Aland" onOpen={onOpen} />);
    expect(screen.getByRole('heading', { name: PRIZE_POSITION_TITLE })).toBeTruthy();
    const sc = screen.getByTestId('prize-position-scatter');
    const pts = [...sc.querySelectorAll('[data-key]')] as HTMLElement[];
    expect(pts).toHaveLength(9);
    expect(new Set(pts.map((p) => p.getAttribute('data-verb')))).toEqual(new Set(['secure', 'preserve', 'catch_up', 'partner', 'watch']));
    // every point is a button over the figure, named in words; Secure is drawn last (on top)
    expect(pts.every((p) => p.tagName === 'BUTTON')).toBe(true);
    expect(pts[pts.length - 1].getAttribute('data-verb')).toBe('secure');
    // Batteries Refining: prize 0.40, position 0.10, placed at exactly those coordinates (not moved for Secure)
    const sec = sc.querySelector('[data-key="Batteries|Refining"]') as HTMLElement;
    expect([sec.getAttribute('data-x'), sec.getAttribute('data-y')]).toEqual(['40', '10']);
    // the plot area between the margins, as the shared core places it (jsdom normalises the calc)
    const probe = document.createElement('div');
    probe.style.left = quadLeft(40);
    expect(sec.style.left).toBe(probe.style.left);
    expect(sec.style.left).toMatch(/52px.*0\.4.*72px|52px.*72px.*0\.4/);
    expect(sec.getAttribute('aria-label')).toBe(
      'Batteries Refining & Processing: Secure (chokepoint risk), high fence; Strategic Industrial Value 40%; Competitive Position 10%; supply-chain vulnerability 95%; trade momentum unavailable; policy needs more attention; evidence strong',
    );
    // the corner headings sit in the margins above and below the plot: tendencies, not rules
    const q = `${screen.getByTestId('prize-position-quadrants').textContent} ${screen.getByTestId('prize-position-quadrants-low').textContent}`;
    for (const w of ['typically Preserve', 'Catch Up, given a real base to build from', 'Watch, unless high vulnerability makes it Partner', 'mature or specialized: review']) expect(q).toContain(w);
    expect(screen.getByTestId('prize-position-quadrants').textContent).toMatch(/^Strong position \+ weak prize.*Strong position \+ strong prize/);
    // the nodes not plotted, grouped by why and by technology (not one sentence per node)
    expect(screen.getByTestId('prize-position-unplotted').textContent).toBe('Not plotted (1): 1 not assessed: Batteries (Recovery & Recycling).');
    // the legend: the choices by fence with the counts on the chart, the stage shapes, the selection ring and the dashed lines
    const legend = screen.getByTestId('prize-position-legend');
    expect(within(legend).getByTestId('posture-legend').textContent).toBe('High fence:Secure 3Medium fence:Preserve 2,Catch Up 1,Partner 1Low fence:Watch 2');
    expect([...legend.querySelectorAll('[data-stage]')].map((e) => e.textContent)).toEqual(['Extraction', 'Refining & Processing', 'Component Manufacturing', 'Final Assembly & Equipment', 'Recovery & Recycling']);
    expect(legend.textContent).toContain("Dashed lines: a high prize at 60% or more; a strong position at 75% or more among the step's main producers");
    // the tooltip: the node, its choice and fence, need, prize, position, momentum, alignment, evidence
    fireEvent.mouseEnter(sc.querySelector('[data-key="Batteries|Extraction"]') as Element);
    const tip = screen.getByTestId('prize-position-tip').textContent;
    for (const w of ['Partner· Medium fence', 'Batteries · Extraction', 'Supply-chain vulnerability 80%', 'Strategic Industrial Value 30%', 'Competitive Position 20%', 'Trade momentum: rising', 'Policy alignment: Wrong tool for the problem', 'Evidence: Strong']) {
      expect(tip).toContain(w);
    }
    fireEvent.keyDown(sc.querySelector('[data-key="Batteries|Components"]') as Element, { key: 'Enter' });
    expect(onOpen).toHaveBeenLastCalledWith('Batteries|Components');
    fireEvent.click(sec);
    expect(onOpen).toHaveBeenLastCalledWith('Batteries|Refining');
    // the stage filter
    fireEvent.change(screen.getByLabelText('Stage'), { target: { value: 'Assembly' } });
    expect([...sc.querySelectorAll('[data-key]')].map((p) => p.getAttribute('data-key')).sort()).toEqual(['Batteries|Assembly', 'Solar|Assembly']);
    expect([...(screen.getByLabelText('Stage') as HTMLSelectElement).options].map((o) => o.textContent)).toEqual([
      'All producing stages', 'Extraction', 'Refining & Processing', 'Component Manufacturing', 'Final Assembly & Equipment', 'Recovery & Recycling',
    ]);
    // one stage: no stage-shape key
    expect(screen.getByTestId('prize-position-legend').querySelectorAll('[data-stage]')).toHaveLength(0);
    noAcronyms();
  });

  describe('the figure (Plot, at a measured width)', () => {
    const cw = Object.getOwnPropertyDescriptor(HTMLElement.prototype, 'clientWidth');
    beforeAll(() => Object.defineProperty(HTMLElement.prototype, 'clientWidth', { configurable: true, get: () => 900 }));
    afterAll(() => cw && Object.defineProperty(HTMLElement.prototype, 'clientWidth', cw));
    // 900 px wide: the plot area runs 52..880 across and 24..514 up (height 560; margins 24 / 20 / 46 / 52: the
    // top margin keeps the y axis's label clear of the "100" tick)
    const px = (x: number) => 52 + (x / 100) * 828;
    const py = (y: number) => 24 + (1 - y / 100) * 490;
    const near = (a: number, b: number) => Math.abs(a - b) < 0.6;

    it('labels never cover another point: each goes above its dot, else below, else unlabelled; the selected one always', () => {
      // three dots in a tight cluster and one alone, on a 900 x 560 figure
      const pts = [
        { key: 'a', x: 50, y: 50, short: 'Alpha · Ref', stage: 'Refining' },
        { key: 'b', x: 51, y: 52.5, short: 'Bravo · Com', stage: 'Components' },
        { key: 'c', x: 50.5, y: 47.5, short: 'Charlie · Asm', stage: 'Assembly' },
        { key: 'd', x: 90, y: 10, short: 'Delta · Ext', stage: 'Extraction' },
      ];
      const at = { px, py, r: 6, width: 900, height: 560, left: 52 };
      const box = (p: (typeof pts)[number], dy: number, sel = false) => {
        const w = textWidth(p.short, sel ? 11 : 10) + 4;
        return { x0: px(p.x) - w / 2, x1: px(p.x) + w / 2, y0: py(p.y) + dy - 6, y1: py(p.y) + dy + 6 };
      };
      const dot = (p: (typeof pts)[number]) => ({ x0: px(p.x) - 8, x1: px(p.x) + 8, y0: py(p.y) - 8, y1: py(p.y) + 8 });
      const hit = (a: ReturnType<typeof dot>, b: ReturnType<typeof dot>) => a.x0 < b.x1 && a.x1 > b.x0 && a.y0 < b.y1 && a.y1 > b.y0;
      const out = placeQuadLabels(pts, pts, at, null);
      const placed = [...out.above.map((p) => box(p, QUAD_LABEL_DY.above)), ...out.below.map((p) => box(p, QUAD_LABEL_DY.below))];
      const owners = [...out.above, ...out.below];
      placed.forEach((b, i) => {
        for (const p of pts) if (p.key !== owners[i].key) expect(hit(b, dot(p)), `${owners[i].key} over ${p.key}`).toBe(false);
        placed.forEach((o, j) => j !== i && expect(hit(b, o)).toBe(false));
      });
      // the lone dot is labelled above; the cluster cannot label all three
      expect(out.above.map((p) => p.key)).toContain('d');
      expect(owners.length).toBeLessThan(4);
      // the selected point is labelled even inside the cluster
      const sel = placeQuadLabels([pts[1], ...pts.filter((p) => p.key !== 'b')], pts, at, 'b');
      expect([...sel.above, ...sel.below].map((p) => p.key)).toContain('b');
    });

    it('the dashed lines sit at the prize threshold and the Preserve bar of core.meta.thresholds', () => {
      inRouter(<PrizePositionScatter core={core} cells={cells} name="Aland" onOpen={() => undefined} />);
      const sc = screen.getByTestId('prize-position-scatter');
      const svg = sc.querySelector('svg[role="img"]') as SVGSVGElement;
      expect(svg.getAttribute('aria-label')).toMatch(/^Aland: 9 producing-stage supply-chain steps placed by Strategic Industrial Value and Competitive Position/);
      const rx = sc.querySelector('.quad-rule-x line') as SVGLineElement;
      const ry = sc.querySelector('.quad-rule-y line') as SVGLineElement;
      expect(near(Number(rx.getAttribute('x1')), px(core.meta.thresholds.prize * 100))).toBe(true);
      expect(near(Number(ry.getAttribute('y1')), py(core.meta.thresholds.cp * 100))).toBe(true);
      for (const g of [rx.parentElement!, ry.parentElement!]) expect(g.getAttribute('stroke-dasharray')).toBe('4,3');
      // the axes: Strategic Industrial Value across, Competitive Position among contenders up, ticks 0-100
      expect(svg.textContent).toContain('Strategic Industrial Value: the prize (percentile) →');
      expect(svg.textContent).toContain('↑ Competitive Position among the main producers (percentile)');
    });

    it('each dot is drawn at its node, in its choice colour with its outline; the overlay buttons sit on the same spots', () => {
      inRouter(<PrizePositionScatter core={core} cells={cells} name="Aland" onOpen={() => undefined} selected="Solar|Assembly" />);
      const sc = screen.getByTestId('prize-position-scatter');
      const dots = [...sc.querySelectorAll('.quad-dots path')] as SVGPathElement[];
      const btns = [...sc.querySelectorAll('button[data-key]')] as HTMLElement[];
      expect(dots).toHaveLength(btns.length);
      // the same order: dot i is button i (both in drawing order, Secure last)
      btns.forEach((b, i) => {
        const v = b.getAttribute('data-verb') as Verb;
        const [tx, ty] = (dots[i].getAttribute('transform') ?? '').match(/[-\d.]+/g)!.map(Number);
        expect(near(tx, px(Number(b.getAttribute('data-x')))) && near(ty, py(Number(b.getAttribute('data-y')))), b.getAttribute('data-key') ?? '').toBe(true);
        expect([dots[i].getAttribute('fill'), dots[i].getAttribute('stroke')]).toEqual([VERB_COLOR[v], VERB_OUTLINE[v]]);
        expect(b.style.top).toBe(`${py(Number(b.getAttribute('data-y')))}px`);
      });
      // one colour per choice on the chart, no other fills
      expect(new Set(dots.map((d) => d.getAttribute('fill')))).toEqual(new Set(['secure', 'preserve', 'catch_up', 'partner', 'watch'].map((v) => VERB_COLOR[v as Verb])));
      expect(new Set(dots.map((d) => cssHex(d.getAttribute('fill'))))).not.toContain('#d1497e');
      // the selected node: a white ring (signal orange is the Secure fill here) and its bold label
      const ring = sc.querySelector('.quad-selected path') as SVGPathElement;
      const solar = btns.find((b) => b.getAttribute('data-key') === 'Solar|Assembly')!;
      const [rx, ry] = (ring.getAttribute('transform') ?? '').match(/[-\d.]+/g)!.map(Number);
      expect(near(rx, px(Number(solar.getAttribute('data-x')))) && near(ry, py(Number(solar.getAttribute('data-y'))))).toBe(true);
      expect(ring.parentElement?.getAttribute('stroke')).toBe('#ffffff');
      expect(Object.values(VERB_COLOR)).not.toContain(ring.parentElement?.getAttribute('stroke'));
      expect(solar.getAttribute('aria-pressed')).toBe('true');
      expect(sc.querySelector('svg[role="img"]')?.textContent).toContain('Solar · Asm');
    });
  });

  it('the table twin lists plotted and unplotted nodes with their tendency', () => {
    inRouter(<PrizePositionScatter core={core} cells={cells} name="Aland" onOpen={() => undefined} />);
    fireEvent.click(screen.getByRole('button', { name: 'Table' }));
    const t = screen.getByRole('table', { name: 'Aland: Strategic Industrial Value and Competitive Position by supply-chain step' });
    const rows = within(t).getAllByRole('row').slice(1);
    expect(rows).toHaveLength(10);
    expect(rows.find((r) => r.textContent?.includes('Final Assembly & Equipment') && r.textContent.includes('Batteries'))?.textContent).toContain('Strong position + strong prize: typically Preserve');
  });

  it('a Catch Up node with a top-quarter score did not pass the Preserve test: the tooltip and the table say so, not "typically Preserve"', () => {
    const top = cells.map((c) => (c.tech === 'Batteries' && c.stage === 'Components' ? { ...c, cp: 0.875 } : c));
    inRouter(<PrizePositionScatter core={core} cells={top} name="Aland" onOpen={() => undefined} />);
    fireEvent.mouseEnter(screen.getByTestId('prize-position-scatter').querySelector('[data-key="Batteries|Components"]') as Element);
    const tip = screen.getByTestId('prize-position-tendency').textContent;
    expect(tip).toBe('A top-quarter score that does not pass the Preserve test (not a main producer, or below the capability floor). Select for the five questions.');
    expect(tip).not.toContain('typically Preserve');
    // a Preserve node in the same quadrant keeps the tendency
    fireEvent.mouseEnter(screen.getByTestId('prize-position-scatter').querySelector('[data-key="Batteries|Assembly"]') as Element);
    expect(screen.getByTestId('prize-position-tendency').textContent).toBe('Strong position + strong prize: typically Preserve (a tendency). Select for the five questions.');
    fireEvent.click(screen.getByRole('button', { name: 'Table' }));
    const rows = within(screen.getByRole('table', { name: 'Aland: Strategic Industrial Value and Competitive Position by supply-chain step' })).getAllByRole('row');
    expect(rows.find((r) => r.textContent?.includes('Component Manufacturing') && r.textContent.includes('Batteries'))?.textContent).toContain(
      'A top-quarter score that does not pass the Preserve test (not a main producer, or below the capability floor)',
    );
  });
});

describe('Strategic Choices (spec XIV)', () => {
  it('six groups with their subtitles and fields, the review list and the nodes not assessed', () => {
    const onOpen = vi.fn();
    inRouter(<StrategicChoices core={core} cells={cells} detail={detail} name="Aland" onOpen={onOpen} aibHref={(t, s) => `/aib?node=${t}|${s}`} />);
    expect(screen.getByRole('heading', { name: STRATEGIC_CHOICES_TITLE })).toBeTruthy();
    const groups = screen.getAllByTestId('choice-group');
    expect(groups.map((g) => g.getAttribute('data-verb'))).toEqual(['secure', 'preserve', 'catch_up', 'partner', 'accelerate', 'watch']);
    // the choice colour (a swatch, no mark), the verb, the count
    expect(groups.map((g) => within(g).getByRole('heading', { level: 4 }).textContent)).toEqual([
      'Secure · High fence3', 'Preserve · Medium fence2', 'Catch Up · Medium fence1', 'Partner · Medium fence1', 'Accelerate · Medium fence1', 'Watch · Low fence3',
    ]);
    for (const g of groups) {
      const sw = within(g).getByRole('heading', { level: 4 }).querySelector('[data-swatch]') as HTMLElement;
      expect(cssHex(sw.style.background), g.getAttribute('data-verb') ?? '').toBe(VERB_COLOR[g.getAttribute('data-verb') as Verb]);
    }
    const [secure, preserve, catchUp, partner, accelerate, watch] = groups;
    expect(secure.textContent).toContain('Protect what cannot be allowed to fail');
    expect(secure.textContent).toContain('Main riskXXX holds 70% of world supply; 80% of imports come from potentially coercive suppliers');
    expect(secure.textContent).toContain('Recommended treatmentDiversify refining through allied offtake and hold a strategic stock.');
    expect(secure.textContent).toMatch(/Delivery mode\s?mixed/);
    expect(preserve.textContent).toContain('Keep and strengthen an existing strength');
    expect(preserve.textContent).toMatch(/Competitive Position\s?95%/);
    expect(catchUp.textContent).toContain('Position gap35 points below the Preserve bar (position 40)');
    expect(catchUp.textContent).toContain('Pathway evidencefeasibility, pipeline');
    expect(partner.textContent).toContain('Recommended partnersBland, Cland');
    expect(partner.textContent).toContain('Diversification valuemoving 10% of imports to BBB would cut import concentration (HHI) by 0.050');
    expect(partner.textContent).toMatch(/AIB status\s?covered/);
    expect(within(partner).getByTestId('choice-aib-link').getAttribute('href')).toBe('/aib?node=Batteries|Extraction');
    expect(accelerate.textContent).toContain('Deploy faster at home');
    expect(accelerate.textContent).toContain('Deployment priorityhigh');
    expect(accelerate.textContent).toContain('Domestic manufacturing priorityset at the producing stages (Partner at Extraction;');
    expect(watch.textContent).toContain('Keep open: buy competitively, do not force localization');
    // the engine's reasons (detail watch_reasons), not a UI guess: no weak domestic case on a low-prize node
    expect(watch.textContent).toContain('Domestic industrial prioritylow: low need; low prize; ample diversified supply');
    expect(watch.textContent).not.toContain('weak domestic case');
    expect(watch.textContent).toContain('low need; low prize; emerging technology, research and option value');
    expect(screen.getByTestId('choice-review').textContent).toContain('Solar · Final Assembly & Equipment');
    expect(screen.getByTestId('choice-not-assessed').textContent).toContain('Not assessed 1');
    fireEvent.click(within(catchUp).getByRole('button', { name: /Batteries · Component Manufacturing/ }));
    expect(onOpen).toHaveBeenLastCalledWith('Batteries|Components');
    fireEvent.click(within(screen.getByTestId('choice-not-assessed')).getByRole('button', { name: /Batteries · Recovery/ }));
    expect(onOpen).toHaveBeenLastCalledWith('Batteries|Recovery');
    noAcronyms();
  });

  it('the Catch Up position gap is read from the cell: a top-quarter score that did not pass the Preserve test, or not measured', () => {
    const at = (cp: number | null) => cells.map((c) => (c.tech === 'Batteries' && c.stage === 'Components' ? { ...c, cp } : c));
    inRouter(<StrategicChoices core={core} cells={at(0.875)} detail={detail} name="Aland" onOpen={() => undefined} />);
    let catchUp = screen.getAllByTestId('choice-group')[2];
    expect(catchUp.textContent).toContain('Position gapa top-quarter score that does not pass the Preserve test (not a main producer, or below the capability floor)');
    expect(catchUp.textContent).not.toContain('at the Preserve bar');
    cleanup();
    inRouter(<StrategicChoices core={core} cells={at(null)} detail={detail} name="Aland" onOpen={() => undefined} />);
    catchUp = screen.getAllByTestId('choice-group')[2];
    expect(catchUp.textContent).toContain('Position gapposition not measured');
  });

  it('a long group shows its first nodes and a "Show all" toggle', () => {
    inRouter(<StrategicChoices core={core} cells={cells} detail={detail} name="Aland" onOpen={() => undefined} limit={2} />);
    const secure = screen.getAllByTestId('choice-group')[0];
    expect(within(secure).getAllByRole('listitem')).toHaveLength(2);
    fireEvent.click(within(secure).getByRole('button', { name: 'Show all 3' }));
    expect(within(secure).getAllByRole('listitem')).toHaveLength(3);
  });
});

describe('Policy Alignment (spec XV)', () => {
  it('a compact table per node, attention first, deployment gap as an under-response; no overall score', () => {
    const onOpen = vi.fn();
    inRouter(
      <PolicyAlignmentTable core={core} cells={cells} detail={detail} name="Aland" onOpen={onOpen}>
        <p>PAMS and NIPO detail</p>
      </PolicyAlignmentTable>,
    );
    expect(screen.getByRole('heading', { name: POLICY_ALIGNMENT_TITLE })).toBeTruthy();
    const t = within(screen.getByTestId('policy-alignment')).getByRole('table');
    expect(within(t).getAllByRole('columnheader').map((h) => h.textContent)).toEqual([
      'Supply-chain step', 'Recommended choice', 'Observed policy intensity', 'Support', 'Protection', 'Policy momentum', 'Alignment',
    ]);
    const rows = within(t).getAllByRole('row').slice(1);
    expect(rows.map((r) => r.getAttribute('data-alignment'))).toEqual([
      'misaligned', 'under_prioritized', 'deployment_gap', 'over_prioritized', 'mixed', 'aligned', 'aligned', 'aligned', 'aligned', 'aligned', 'insufficient_data',
    ]);
    // intensity, support, protection and momentum are ranks within the country, never shares or signed changes
    expect(rows[0].textContent).toBe('Batteries · ExtractionPartner · Medium fencehigh, 85th percentilelow, 20th percentilehigh, 90th percentilerising≠Wrong tool for the problem(high confidence)');
    const com = rows.find((r) => r.getAttribute('data-key') === 'Batteries|Components')!;
    expect(com.textContent).toContain('moderate, 60th percentilemoderate, 60th percentilenone recordedlow, 12th percentile');
    expect(com.textContent).not.toContain('+12%');
    expect(screen.getByTestId('policy-ranks-note').textContent).toContain("percentile ranks among Aland's own supply-chain steps");
    expect(screen.getByTestId('policy-ranks-note').textContent).not.toMatch(/shares of that attention/);
    expect(rows[2].textContent).toContain('Needs more attention: deployment policy lags');
    expect(rows[5].textContent).toContain('not available');
    // judged node by node: no score column, no total
    expect(document.body.textContent).toContain('there is no overall score');
    expect(within(t).queryByRole('columnheader', { name: /score/i })).toBeNull();
    expect(screen.getByTestId('policy-coverage').textContent).toContain('1 wrong tool for the problem · 2 needs more attention (1 where deployment policy lags) · 1 too much intervention · 1 mixed signals · 5 broadly aligned · 1 not enough policy evidence.');
    expect(screen.getByText('PAMS and NIPO detail')).toBeTruthy();
    fireEvent.click(within(rows[3]).getByRole('button'));
    expect(onOpen).toHaveBeenLastCalledWith('Solar|Deployment');
  });
});

describe('Allied Strategy (spec XVI)', () => {
  it('opens with the computed sentence, then need -> partners -> why -> AIB status, with the gap logic; generic material collapsed', () => {
    const onOpen = vi.fn();
    inRouter(
      <AlliedStrategy core={core} cells={cells} detail={detail} name="Aland" onOpen={onOpen} partnerHref={(i) => `/countries/${i}`} explore={<p>generic partnership material</p>} />,
    );
    expect(screen.getByTestId('allied-sentence').textContent).toBe("1 of Aland's 7 priority nodes is better solved through allies; 2 Secure nodes also depend on trusted foreign capacity.");
    const list = screen.getByTestId('allied-strategy');
    const items = within(list).getAllByRole('listitem').filter((li) => li.hasAttribute('data-key'));
    expect(items.map((li) => [li.getAttribute('data-key'), li.getAttribute('data-verb'), li.getAttribute('data-status')])).toEqual([
      ['Batteries|Extraction', 'partner', 'covered'], ['Batteries|Refining', 'secure', 'gap'], ['Solar|Refining', 'secure', 'not_mapped'],
    ]);
    expect(within(items[0]).getByRole('link', { name: 'Bland' }).getAttribute('href')).toBe('/countries/BBB');
    expect(items[0].textContent).toContain('strong supply position (20% of world supply); strong competitive position (top quarter); trusted relationship; diversification contribution');
    expect(within(items[1]).getByTestId('allied-gaps').textContent).toContain('capability evidence is weak');
    const explore = screen.getByTestId('allied-explore');
    expect(within(explore).getByText(EXPLORE_PARTNERSHIPS)).toBeTruthy();
    expect((explore.querySelector('details') as HTMLDetailsElement).open).toBe(false);
    fireEvent.click(within(items[2]).getByRole('button'));
    expect(onOpen).toHaveBeenLastCalledWith('Solar|Refining');
    noAcronyms();
  });

  it('an honest "no credible allied solution yet"', () => {
    const cland = countryCells(core, 'CCC');
    inRouter(<AlliedStrategy core={core} cells={cland} detail={null} name="Cland" onOpen={() => undefined} />);
    const item = within(screen.getByTestId('allied-strategy')).getAllByRole('listitem')[0];
    expect(within(item).getByTestId('allied-status').textContent).toBe('Gap · no credible allied solution yet');
    expect(item.textContent).toContain('only one credible supplier exists');
  });
});

describe('one drawer for every entry point (spec XIX, design D8)', () => {
  it('?node= opens the drawer on load; each section selects through the URL and the same single drawer follows', async () => {
    const router = inRouter(<CountryPage />, '/country?node=Batteries|Refining');
    let d = await screen.findByTestId('cell-drawer');
    expect(d.getAttribute('aria-label')).toBe('Batteries Refining & Processing: five questions');
    const go = async (click: () => void, key: string, label: string) => {
      click();
      await waitFor(() => expect(router.state.location.search).toBe(`?node=${encodeURIComponent(key).replace(/%20/g, '+')}`));
      d = screen.getByTestId('cell-drawer');
      expect(screen.getAllByTestId('cell-drawer')).toHaveLength(1);
      expect(d.getAttribute('aria-label')).toBe(`${label}: five questions`);
    };
    await go(() => fireEvent.click(within(screen.getByTestId('s-strategy')).getByRole('button', { name: /^Solar Extraction/ })), 'Solar|Extraction', 'Solar Extraction');
    await go(() => fireEvent.click(within(screen.getByTestId('supply-need-ranking')).getAllByRole('button')[1]), 'Solar|Refining', 'Solar Refining & Processing');
    await go(() => fireEvent.click(screen.getByTestId('prize-position-scatter').querySelector('[data-key="Batteries|Assembly"]') as Element), 'Batteries|Assembly', 'Batteries Final Assembly & Equipment');
    await go(
      () => fireEvent.click(within(screen.getAllByTestId('choice-group')[2]).getByRole('button', { name: /Batteries · Component Manufacturing/ })),
      'Batteries|Components', 'Batteries Component Manufacturing',
    );
    await go(() => fireEvent.click(within(screen.getByTestId('policy-alignment')).getByRole('button', { name: 'Solar · Deployment & Use' })), 'Solar|Deployment', 'Solar Deployment & Use');
    await go(() => fireEvent.click(within(screen.getByTestId('allied-strategy')).getByRole('button', { name: /Batteries · Extraction/ })), 'Batteries|Extraction', 'Batteries Extraction');
    // the Strategy Map shows the same node pressed
    expect(within(screen.getByTestId('s-strategy')).getByRole('button', { name: /^Batteries Extraction/ }).getAttribute('aria-pressed')).toBe('true');
    fireEvent.click(screen.getByRole('button', { name: 'Close the detail' }));
    await waitFor(() => expect(router.state.location.search).toBe(''));
    expect(screen.queryByTestId('cell-drawer')).toBeNull();
  });

  it('an invalid ?node= is reported as ignored and opens nothing; a display name or Downstream is understood', async () => {
    inRouter(<CountryPage />, '/country?node=Wind|Refining');
    expect((await screen.findByTestId('node-ignored')).textContent).toBe('Ignored in the link: node=Wind|Refining (Wind is not a technology of the posture data; ignored)');
    expect(screen.queryByTestId('cell-drawer')).toBeNull();
    cleanup();
    inRouter(<CountryPage />, '/country?node=Solar|Deployment%20%26%20Use');
    expect((await screen.findByTestId('cell-drawer')).getAttribute('aria-label')).toBe('Solar Deployment & Use: five questions');
    expect(screen.queryByTestId('node-ignored')).toBeNull();
    cleanup();
    inRouter(<CountryPage />, '/country?node=Solar|Downstream');
    expect((await screen.findByTestId('cell-drawer')).getAttribute('aria-label')).toBe('Solar Deployment & Use: five questions');
    expect(screen.getByTestId('node-ignored').textContent).toContain('Downstream corresponds to Deployment & Use');
  });
});
