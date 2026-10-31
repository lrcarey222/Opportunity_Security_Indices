// The three fence sections of the Country profile (2026-10-30, the Yards & Fences framing; the user's design
// "Only that fence's nodes"). They replace Need, Prize & Position and Choices and follow the Strategy map:
//   High fence · Secure (#high-fence, alias #need)
//     the top five Secure nodes by supply-chain need, each with its security rationale; a closed drop-down of
//     every Secure node x the need variables (import dependence, supplier concentration, risky supplier
//     exposure, lack of trusted alternatives, with the supply-chain need score).
//   Medium fence · Preserve, Catch Up, Partner, Accelerate (#medium-fence, aliases #prize-position, #choices)
//     the top five producing-stage Preserve / Catch Up / Partner nodes by Strategic Industrial Value (ties by
//     Competitive Position) and the top five Accelerate nodes by deployment need; the Prize x Position chart;
//     then four sub-sections (h3, each with its anchor): Prize (#medium-prize: the nodes x the Strategic
//     Industrial Value and its three families: the market prize, from the core or the detail, industrial
//     commons, future option and the market-only cap, from the node's or layer's evidence in meta.yf_nodes;
//     then the market prize's components), Competitive Position (#medium-position: the nodes x the position evidence, the Preserve bar
//     named, the monthly trade card), Partner (#medium-partner, aliases #partnerships, #partners,
//     #allied-partnerships: the economy's Partner nodes in one line above the partnership analysis as it read
//     before the redesign, open: the one place in the strategy sections where the Legacy OSI partner scores
//     appear, as one of its three lenses and named so) and Accelerate (#medium-accelerate: the Accelerate nodes x the deployment-need
//     families, each with its scenario, and the energy-system context).
//   Low fence · Watch (#low-fence)
//     the top five producing-stage Watch nodes by Strategic Industrial Value (the biggest prizes not chased) and
//     the top five Deployment & Use Watch nodes by deployment need (closest to Accelerate), each with the
//     engine's Watch reasons; closed drop-downs of every Watch node x the same variables.
// Layers as yard units (2026-10-30, posture/fences.ts): a layer whose own choice differs from its step's sits
// behind the fence of its own choice, beside the steps, named on a "Layer" line (top lists) or in a Layer
// column (tables); it ranks with its step's value and opens its step's drawer focused on the layer
// (?node=Tech|Stage&layer=id). Its explaining field is its own reason (yards.ts layerReason).
// Each top-five entry shows the node, its choice colour (VERB_COLOR) and word, the ranking value and the one
// field that explains the choice (posture/fences.ts fenceReason). Every entry and every table row opens the
// one drawer through ?node= (ProfileCtx.posture.node). Values come from the drawer's accessors (fences.ts);
// a value the data do not carry reads "not measured"; a value of the economy's detail document reads "…" while
// it loads and "could not be loaded" when it failed (with one Retry per fence), never "not measured". A Watch
// node's reasons are the engine's (the detail document's): "…" until it is in. The tables follow the top-five
// pattern: up to five rows shown, the full table in a closed drop-down; the position tables run in Competitive
// Position order; the headers wrap so a table fits its card. A fence with no node says so in one line. The
// sections carry no basis badge of their own (the Strategy section's says the framework and its fixed
// thresholds once). None of these sections reads the Perspective dials or the engine.

import type { ReactNode } from 'react';
import { TableView, type Col } from '../../../components/TableView.tsx';
import { Term } from '../../../components/Term.tsx';
import { Notice } from '../../../components/ui.tsx';
import {
  byPositionOrder, ChoiceLegend, countryNames, deploymentVars, familyWords, FENCE_TOP_N, FENCE_VERBS, fenceGroups, fenceReason, MARKET_PRIZE_LABEL, POSTURE_TERMS, positionVars, PrizePositionScatter,
  prizeLabel, prizeVars, shareWords, stageDisplay, supplyNeedVars, Swatch, VERB_LABEL, verbOf, YARD_LABEL, type Family, type Fence, type FenceNode, type PostureCore,
} from '../../../posture/index.ts';
import { score100 } from '../../../posture/derive.ts';
import { ratioVsCurrentWords } from '../../../posture/detail.ts';
import { CountryClimateCard } from '../../../sector/views/climate/CountryClimate.tsx';
import { CountryTransitionCards } from '../../../sector/views/climate/CountryTransition.tsx';
import { cellKeyOfNode, MEDIUM_SUBSECTIONS, nodeKeyOfCell, STRATEGY_SECTIONS } from '../../derive/strategyPage.ts';
import { cap, poss, proseName } from '../../derive/text.ts';
import { Fold, Section, useProfile, type CountryPosture } from './common.tsx';
import { PartnershipsSection } from './PartnersSection.tsx';
import { usePostureState } from './StrategySections.tsx';
import { TradeMonthlyCard } from './TradeMonthlyCard.tsx';

const SEC = Object.fromEntries(STRATEGY_SECTIONS.map((s) => [s.id, s])) as Record<string, (typeof STRATEGY_SECTIONS)[number]>;
const SUB = Object.fromEntries(MEDIUM_SUBSECTIONS.map((s) => [s.id, s])) as Record<string, (typeof MEDIUM_SUBSECTIONS)[number]>;

/** The one-line plain description of each fence (the section intros). */
export const FENCE_MEANING: Record<Fence, string> = {
  high: 'An acute vulnerability, secured whatever the prize.',
  medium: 'Compete for the capability, at home or with partners, or accelerate deployment.',
  low: 'Watch: domestic priority is low.',
};

/** Tables longer than this show their first rows (the top five), the full table in a closed drop-down. */
const VISIBLE_ROWS = FENCE_TOP_N;
const NOT_MEASURED = 'not measured';
const PENDING = '…';
const UNAVAILABLE = 'could not be loaded';

/** The load of the economy's detail document (the components behind the core values). */
type DetailState = 'loading' | 'ready' | 'error';

/** The posture data of a section, once the framework can draw (else null: the section shows its state). */
function useReady(): (CountryPosture & { core: PostureCore }) | null {
  const { posture: p } = useProfile();
  return p && p.status === 'ready' && p.core && p.cells.length ? (p as CountryPosture & { core: PostureCore }) : null;
}

/** The detail document's state for the page (loading, ready or error). */
function detailStateOf(p: CountryPosture | null): DetailState {
  if (!p) return 'loading';
  if (p.detailStatus) return p.detailStatus;
  return p.detailPending ? 'loading' : 'ready';
}

/**
 * What a missing value of a node says: a value of the detail document reads by the document's state while
 * the node has none ("…" while it loads, "could not be loaded" after an error); else "not measured".
 */
function missingWords(n: FenceNode, ds: DetailState, fromDetail = true): string {
  if (!fromDetail || n.detail) return NOT_MEASURED;
  return ds === 'loading' ? PENDING : ds === 'error' ? UNAVAILABLE : NOT_MEASURED;
}

function Missing({ words }: { words: string }) {
  return (
    <span className="text-paper-mute" data-missing={words === UNAVAILABLE ? 'error' : words === PENDING ? 'loading' : 'not-measured'}>
      {words}
    </span>
  );
}

/**
 * The field that explains a node's choice (fences.ts fenceReason). A Watch node's reasons are the engine's,
 * written in the detail document: until it is in (or when it failed), null, so the caller shows the state.
 */
function whyOf(n: FenceNode, core: PostureCore, names: Record<string, string>, ds: DetailState): string | null {
  if (n.verb === 'watch' && !n.detail && ds !== 'ready') return null;
  return fenceReason(n, core, names);
}

/** One notice under a fence when the detail document failed: its values say so, and Retry loads it again. */
function DetailNotice() {
  const { posture: p } = useProfile();
  if (!p || detailStateOf(p) !== 'error') return null;
  return (
    <div data-testid="fence-detail-error">
      <Notice
        kind="warning"
        title="The node details could not be loaded"
        action={
          p.retryDetail ? (
            <button type="button" className="btn btn-ghost btn-sm" onClick={p.retryDetail}>
              Retry
            </button>
          ) : undefined
        }
      >
        {p.detailError ? `${p.detailError} ` : ''}The values that come from them read &ldquo;{UNAVAILABLE}&rdquo;, not &ldquo;{NOT_MEASURED}&rdquo;.
      </Notice>
    </div>
  );
}

/** The node's choice: its colour and its word (a Catch Up node's credible pathway beneath, where asked). */
function Choice({ n, pathway }: { n: FenceNode; pathway?: string | null }) {
  return (
    <>
      <span className="inline-flex items-center gap-1.5 whitespace-nowrap" data-verb={n.verb}>
        <Swatch verb={n.verb} size={12} />
        {VERB_LABEL[n.verb]}
      </span>
      {pathway !== undefined && (
        <span className="block text-[11px] text-paper-mute" data-testid="fence-pathway">
          {pathway}
        </span>
      )}
    </>
  );
}

const nodeWords = (n: FenceNode) => `${n.tech} · ${stageDisplay(n.stage)}`;
/** A unit's words: the node, and a layer unit's layer ("Electric Grid · Final Assembly & Equipment, layer Grid-control software"). */
const unitWords = (n: FenceNode) => (n.layer ? `${nodeWords(n)}, layer ${n.layer.label}` : nodeWords(n));

/** Open a unit in the drawer: its node, focused on a layer unit's layer. */
const openUnit = (p: CountryPosture, n: FenceNode) => p.node.select(n.nodeKey, n.layer?.id ?? null);
/** The row key of the open unit (a node, or its layer when the drawer is focused on one). */
const selectedUnitKey = (p: CountryPosture | null): string | null => (p?.node.selected ? (p.node.layer ? `${p.node.selected}#${p.node.layer}` : p.node.selected) : null);

// ---- the columns -----------------------------------------------------------------------------------------

type Get = (n: FenceNode) => number | null;
type GetText = (n: FenceNode) => string | null;

const nodeCol: Col<FenceNode> = { id: 'node', header: 'Node', value: nodeWords, minWidth: 150 };
/**
 * The Layer column, only where the rows hold a layer unit: the layer's name with its yard, "Whole step" for a
 * node's own row.
 */
function layerCols(rows: readonly FenceNode[]): Col<FenceNode>[] {
  if (!rows.some((n) => n.layer)) return [];
  return [
    {
      id: 'layer', header: 'Layer', minWidth: 140, title: 'A capability layer with its own choice, ranked with its step',
      value: (n) => n.layer?.label ?? 'Whole step',
      cell: (n) =>
        n.layer ? (
          <span data-testid="fence-layer-cell">
            {n.layer.label}
            {n.layer.yard && <span className="block text-[11px] text-paper-mute">{YARD_LABEL[n.layer.yard]}</span>}
          </span>
        ) : (
          <span className="text-paper-mute">Whole step</span>
        ),
    },
  ];
}

/**
 * The Choice column, only where the rows hold more than one choice (a table of one choice says it in its
 * caption and its fence); with `pathway`, a Catch Up row names its credible pathway beneath the word.
 */
function choiceCols(rows: readonly FenceNode[], ds: DetailState, pathway = false): Col<FenceNode>[] {
  if (new Set(rows.map((n) => n.verb)).size < 2) return [];
  return [
    {
      id: 'choice', header: 'Choice', value: (n) => VERB_LABEL[n.verb], minWidth: 110,
      title: pathway ? 'A Catch Up node names its credible pathway and the evidence behind it' : undefined,
      cell: (n) => <Choice n={n} pathway={pathway && n.verb === 'catch_up' ? (prizeVars(n).pathway ?? missingWords(n, ds)) : undefined} />,
    },
  ];
}

/** A 0-1 score as 0-100 (or a share in %), right-aligned; a missing value reads "not measured" (a detail value: "…" while it loads, "could not be loaded" after an error). */
function scoreCol(
  id: string, header: string, get: Get, opts: { detail?: boolean; ds?: DetailState; fmt?: (v: number) => string; title?: string; note?: (n: FenceNode) => string | null } = {},
): Col<FenceNode> {
  const fmt = opts.fmt ?? ((v: number) => score100(v));
  return {
    id, header, num: true, title: opts.title,
    value: get,
    cell: (n) => {
      const v = get(n);
      if (v === null) return <Missing words={missingWords(n, opts.ds ?? 'ready', !!opts.detail)} />;
      const note = opts.note?.(n);
      return note ? (
        <>
          {fmt(v)}
          <span className="block text-[11px] text-paper-mute">{note}</span>
        </>
      ) : (
        fmt(v)
      );
    },
  };
}

function textCol(id: string, header: string, get: GetText, opts: { detail?: boolean; ds?: DetailState; minWidth?: number } = {}): Col<FenceNode> {
  return {
    id, header, minWidth: opts.minWidth ?? 90,
    value: get,
    cell: (n) => {
      const v = get(n);
      return v === null ? <Missing words={missingWords(n, opts.ds ?? 'ready', !!opts.detail)} /> : v;
    },
  };
}

/** A deployment-need family: its score (or the climate-aligned ratio) with its scenario and basis beneath. */
function familyCol(id: string, header: string, get: (n: FenceNode) => Family | null, scenarios: Record<string, string>, ds: DetailState, title?: string): Col<FenceNode> {
  return {
    id, header, num: true, minWidth: 112, title,
    value: (n) => get(n)?.value ?? null,
    cell: (n) => {
      const f = get(n);
      if (!f || f.value === null) return <Missing words={missingWords(n, ds)} />;
      const ratio = ratioVsCurrentWords(f);
      const rest = (familyWords(f, scenarios) ?? '').split(' · ').slice(1).join(' · ');
      return (
        <>
          <span className="tabular">{ratio ?? score100(f.value)}</span>
          {rest && <span className="block text-[11px] text-paper-mute">{rest}</span>}
        </>
      );
    },
  };
}

function needCols(rows: readonly FenceNode[], core: PostureCore, names: Record<string, string>, ds: DetailState): Col<FenceNode>[] {
  const v = supplyNeedVars;
  return [
    nodeCol,
    ...layerCols(rows),
    scoreCol('need', 'Supply-chain need', (n) => v(n).need),
    scoreCol('import', 'Import dependence', (n) => v(n).importDependence, { detail: true, ds }),
    scoreCol('concentration', 'Supplier concentration', (n) => v(n).supplierConcentration, { detail: true, ds }),
    scoreCol('risky', 'Risky supplier exposure', (n) => v(n).riskyExposure, { detail: true, ds, title: 'China and the opposite bloc' }),
    scoreCol('trusted', 'Lack of trusted alternatives', (n) => v(n).trustedAlternatives, { detail: true, ds }),
    textCol('rationale', 'Security rationale', (n) => whyOf(n, core, names, ds), { minWidth: 160 }),
  ];
}

function prizeCols(rows: readonly FenceNode[], core: PostureCore, names: Record<string, string>, ds: DetailState, opts: { pathway: boolean; reasons: boolean }): Col<FenceNode>[] {
  const v = (n: FenceNode) => prizeVars(n, core.meta);
  // the three families where the prize is the Strategic Industrial Value; a core whose prize is the market measure shows it alone
  const siv = core.meta.prize_measure === 'strategic_value';
  return [
    nodeCol,
    ...layerCols(rows),
    ...choiceCols(rows, ds, opts.pathway),
    scoreCol('opportunity', prizeLabel(core.meta), (n) => v(n).opportunity, {
      title: siv ? 'Market prize, industrial commons and future option value; a layer ranks with its step' : undefined,
      // the cap of the value shown (a layer ranks with its step's value, so its step's cap)
      note: (n) => (siv && v(n).valueCap ? 'capped: market only' : null),
    }),
    ...(siv
      ? [
          scoreCol('market', 'Market prize', (n) => v(n).market, { detail: true, ds, title: `${MARKET_PRIZE_LABEL}: world demand growth and investment momentum, lifted by home-market growth` }),
          scoreCol('commons', 'Industrial commons', (n) => v(n).commons, {
            title: 'Process knowledge, workforce and suppliers, cross-sector spillover (the layer’s own for a layer)',
            // a layer's own market-only cap (its value is its step's)
            note: (n) => (n.layer && v(n).cap ? 'market only' : null),
          }),
          scoreCol('option', 'Future option', (n) => v(n).option, { title: 'The leap tests of a leap candidate; 0 for any other node' }),
        ]
      : []),
    scoreCol('node-prize', 'Node prize', (n) => v(n).nodePrize, { detail: true, ds, title: 'Expected world demand growth' }),
    scoreCol('home', 'Home-market growth', (n) => v(n).homeMarket, { detail: true, ds, title: 'National data only' }),
    scoreCol('investment', 'Investment momentum', (n) => v(n).investmentMomentum, { detail: true, ds }),
    textCol('outlook', 'Market outlook', (n) => v(n).outlook, { detail: true, ds }),
    textCol('basis', 'Basis', (n) => v(n).basis, { detail: true, ds, minWidth: 100 }),
    scoreCol('need', 'Supply-chain need', (n) => v(n).need, { title: 'Context: need triages the node; the prize and position decide the response' }),
    ...(opts.reasons ? [textCol('reasons', 'Why Watch', (n) => whyOf(n, core, names, ds), { detail: true, ds, minWidth: 160 })] : []),
  ];
}

function positionCols(rows: readonly FenceNode[], core: PostureCore, ds: DetailState): Col<FenceNode>[] {
  const band = core.meta.momentum_band;
  const v = (n: FenceNode) => positionVars(n, band);
  const bench = core.meta.benchmark_period ? `The ${core.meta.benchmark_period} benchmark trade share` : 'The benchmark trade share';
  return [
    nodeCol,
    ...layerCols(rows),
    ...choiceCols(rows, ds),
    scoreCol('cp', 'Competitive Position', (n) => v(n).cp),
    scoreCol('share', 'World supply share', (n) => v(n).share, { fmt: (x) => shareWords(x) ?? NOT_MEASURED }),
    scoreCol('export', 'Export position', (n) => v(n).exportPosition, { detail: true, ds }),
    scoreCol('structural', 'Structural trade share', (n) => v(n).structural, {
      detail: true, ds,
      title: `${bench}; where trade momentum did not move the export position, the two are the same`,
      note: (n) => (v(n).structuralSame ? 'same as export' : null),
    }),
    scoreCol('invest', 'Current investment', (n) => v(n).investment, { detail: true, ds }),
    textCol('momentum', 'Trade momentum', (n) => v(n).momentum, { detail: true, ds }),
    textCol('evidence', 'Evidence', (n) => v(n).evidence),
  ];
}

function deployCols(rows: readonly FenceNode[], core: PostureCore, names: Record<string, string>, ds: DetailState, reasons: boolean): Col<FenceNode>[] {
  const sc = core.meta.scenario_labels;
  const v = deploymentVars;
  return [
    { id: 'node', header: 'Technology', value: (n) => n.tech, minWidth: 110 },
    ...layerCols(rows),
    ...choiceCols(rows, ds),
    scoreCol('need', 'Deployment need', (n) => v(n).need),
    familyCol('demand', 'Demand adequacy', (n) => v(n).demand, sc, ds),
    familyCol('gap', 'Target / climate gap', (n) => v(n).policyGap, sc, ds),
    familyCol('system', 'System need', (n) => v(n).system, sc, ds),
    familyCol('alt', 'Climate-aligned (alternative)', (n) => v(n).alternative, sc, ds, 'An alternative scenario, never used in the choice'),
    textCol('trend', 'Deployment trend', (n) => v(n).trend, { detail: true, ds }),
    ...(reasons ? [textCol('reasons', 'Why Watch', (n) => whyOf(n, core, names, ds), { detail: true, ds, minWidth: 160 })] : []),
  ];
}

// ---- the pieces ------------------------------------------------------------------------------------------

/** A fence's table: TableView rows that open the drawer. */
function FenceTable({ rows, cols, caption, testId }: { rows: FenceNode[]; cols: Col<FenceNode>[]; caption: string; testId: string }) {
  const p = useReady();
  return (
    <div data-testid={testId}>
      <TableView rows={rows} cols={cols} caption={caption} rowKey={(n) => n.key} onRowClick={(n) => p && openUnit(p, n)} selectedKey={selectedUnitKey(p)} pageSize={25} wrapHeaders />
    </div>
  );
}

/** A table that may be long, in the top-five pattern: up to five rows shown, else the top five and the full table in a closed drop-down. */
function LongTable({ id, rows, cols, caption, testId, what }: { id: string; rows: FenceNode[]; cols: Col<FenceNode>[]; caption: string; testId: string; what: string }) {
  if (rows.length <= VISIBLE_ROWS) return <FenceTable rows={rows} cols={cols} caption={caption} testId={testId} />;
  return (
    <div className="space-y-2">
      <FenceTable rows={rows.slice(0, VISIBLE_ROWS)} cols={cols} caption={`${caption}: the top ${VISIBLE_ROWS}`} testId={`${testId}-first`} />
      <Fold id={`${id}-all`} summary={`All ${rows.length} ${what}`}>
        <FenceTable rows={rows} cols={cols} caption={caption} testId={testId} />
      </Fold>
    </div>
  );
}

/** The fence's choices with the economy's count of each (the legend of the map, one fence). */
function FenceLegend({ fence, cells }: { fence: Fence; cells: CountryPosture['cells'] }) {
  const counts: Partial<Record<(typeof FENCE_VERBS)[Fence][number], number>> = {};
  for (const c of cells) {
    const v = verbOf(c);
    if (v) counts[v] = (counts[v] ?? 0) + 1;
  }
  return <ChoiceLegend verbs={FENCE_VERBS[fence]} counts={counts} notAssessed={false} />;
}

interface TopListProps {
  title: string;
  testId: string;
  nodes: FenceNode[];
  /** what the list ranks by, in words ("supply-chain need") */
  by: string;
  value: (n: FenceNode) => number | null;
  empty: string;
}

/** The top five of a fence: rank, choice colour and word, node, the ranking value and the field that explains the choice. */
function TopList({ title, testId, nodes, by, value, empty }: TopListProps) {
  const p = useReady();
  const names = useNames(p?.core ?? null);
  if (!p) return null;
  const ds = detailStateOf(p);
  const top = nodes.slice(0, FENCE_TOP_N);
  const headId = `${testId}-h`;
  return (
    <section className="card p-4" aria-labelledby={headId} data-testid={testId}>
      <h3 id={headId} className="text-base font-medium text-paper">
        {title}
      </h3>
      <p className="mb-2 text-xs text-paper-mute">
        Ranked by {by} (0-100), highest first{nodes.length > FENCE_TOP_N ? `; ${FENCE_TOP_N} of ${nodes.length}` : ''}
        {nodes.some((n) => n.layer) ? '; a layer ranks with its step' : ''}.
      </p>
      {top.length ? (
        <ol className="space-y-1">
          {top.map((n, i) => {
            const why = whyOf(n, p.core, names, ds);
            const v = value(n);
            const label = `${n.tech} ${stageDisplay(n.stage)}${n.layer ? `, layer ${n.layer.label}` : ''}: ${VERB_LABEL[n.verb]}; ${by} ${score100(v)}${n.layer ? ' (its step’s)' : ''}${why ? `; ${why}` : ''}`;
            return (
              <li key={n.key} data-key={n.key} data-verb={n.verb} data-layer={n.layer?.id}>
                <button
                  type="button"
                  className="grid w-full grid-cols-[1.25rem_1rem_minmax(0,1fr)_2.5rem] items-start gap-x-2 rounded-md px-1.5 py-1.5 text-left outline-none hover:bg-white/5 focus-visible:ring-2 focus-visible:ring-signal"
                  aria-label={label}
                  aria-pressed={selectedUnitKey(p) === n.key}
                  onClick={() => openUnit(p, n)}
                >
                  <span className="text-sm tabular text-paper-mute">{i + 1}.</span>
                  <span className="pt-0.5">
                    <Swatch verb={n.verb} size={14} />
                  </span>
                  <span className="min-w-0">
                    <span className="block text-sm text-paper">{nodeWords(n)}</span>
                    {n.layer && (
                      <span className="block text-xs text-paper" data-testid="fence-layer">
                        <span className="text-paper-mute">Layer:</span> {n.layer.label}
                      </span>
                    )}
                    <span className="block text-xs text-paper-dim" data-testid="fence-why">
                      {VERB_LABEL[n.verb]} · {why ?? missingWords(n, ds)}
                    </span>
                  </span>
                  <span className="text-right text-sm tabular text-paper" data-testid="fence-value">
                    {v === null ? '—' : score100(v)}
                  </span>
                </button>
              </li>
            );
          })}
        </ol>
      ) : (
        <p className="text-sm text-paper-mute" data-testid="fence-empty">
          {empty}
        </p>
      )}
    </section>
  );
}

/** The economy's names for the reasons (the posture core's, the AIB's where the core has none). */
function useNames(core: PostureCore | null): Record<string, string> {
  const { aib } = useProfile();
  if (!core) return {};
  return { ...(aib?.names ?? {}), ...countryNames(core) };
}

// ---- High fence ----------------------------------------------------------------------------------------

export function HighFenceSection() {
  const { sheet } = useProfile();
  const name = proseName(sheet.meta.country);
  const state = usePostureState('Secure node');
  const p = useReady();
  const names = useNames(p?.core ?? null);
  const g = p ? fenceGroups(p.cells, p.detail, p.core.meta) : null;
  const ds = detailStateOf(p);
  return (
    <Section
      id="high-fence"
      aliases={SEC['high-fence'].aliases}
      title={SEC['high-fence'].label}
      intro={
        <>
          <strong className="font-medium text-paper">{FENCE_MEANING.high}</strong> The nodes of {name} that passed the Secure tests (an operational control layer, a coercive
          chokepoint or systemic resilience), ranked by <Term id={POSTURE_TERMS.supplyChainNeed}>supply-chain need</Term>: how costly or dangerous dependence on outside supply is.
          A capability layer is a <Term id={POSTURE_TERMS.yard}>yard</Term> unit of its own: a Secure layer is listed here, with its step&apos;s need, even when its step sits behind
          another fence.
        </>
      }
    >
      {state ??
        (p && g && (
          <>
            <FenceLegend fence="high" cells={p.cells} />
            <DetailNotice />
            <TopList
              title="Top 5 supply-chain nodes by need"
              testId="high-top"
              nodes={g.secure}
              by="supply-chain need"
              value={(n) => n.cell.need}
              empty="No Secure nodes: no acute vulnerability passed the Secure tests."
            />
            {g.secure.length > 0 && (
              <Fold
                id="high-all"
                summary={`All ${g.secure.length} Secure ${g.secure.length === 1 ? 'node' : 'nodes'} × need variables: import dependence, supplier concentration, risky supplier exposure, lack of trusted alternatives`}
              >
                <p className="text-xs text-paper-mute">Scores are 0-100. Select a row for the node&apos;s five questions.</p>
                <FenceTable rows={g.secure} cols={needCols(g.secure, p.core, names, ds)} caption={`${cap(name)}: Secure nodes by supply-chain need`} testId="high-table" />
              </Fold>
            )}
          </>
        ))}
    </Section>
  );
}

// ---- Medium fence --------------------------------------------------------------------------------------

/** The Partner sub-section's lead: the economy's Partner nodes, each opening the drawer. */
function PartnerLead({ nodes, name }: { nodes: FenceNode[]; name: string }) {
  const p = useReady();
  const names = useNames(p?.core ?? null);
  if (!p) return null;
  const partners = nodes.filter((n) => n.verb === 'partner');
  return (
    <p className="text-sm text-paper-dim" data-testid="partner-lead">
      {partners.length ? (
        <>
          <span className="inline-flex items-center gap-1.5 align-middle">
            <Swatch verb="partner" size={12} />
            <span className="text-paper">
              {cap(poss(name))} Partner {partners.length === 1 ? 'node' : 'nodes'}:
            </span>
          </span>{' '}
          {partners.map((n, i) => {
            const why = whyOf(n, p.core, names, detailStateOf(p));
            return (
              <span key={n.key} data-key={n.key}>
                {i > 0 && '; '}
                <button type="button" className="link" aria-pressed={selectedUnitKey(p) === n.key} onClick={() => openUnit(p, n)}>
                  {unitWords(n)}
                </button>
                {why ? ` (${why.charAt(0).toLowerCase()}${why.slice(1)})` : ''}
              </span>
            );
          })}
          . The capability matters, but allies and diversification are the better answer than a domestic build.
        </>
      ) : (
        `No node of ${name} is a Partner choice.`
      )}
    </p>
  );
}

export function MediumFenceSection() {
  const { sheet, posture, setCell } = useProfile();
  const name = proseName(sheet.meta.country);
  const state = usePostureState('Preserve, Catch Up, Partner or Accelerate node');
  const p = useReady();
  const names = useNames(p?.core ?? null);
  const g = p ? fenceGroups(p.cells, p.detail, p.core.meta) : null;
  const ds = detailStateOf(p);
  const t = p?.core.meta.thresholds ?? null;
  // the position tables run in Competitive Position order (the prize tables in the fence's own, by prize)
  const competeByPosition = g ? byPositionOrder(g.compete) : [];
  // the monthly trade card follows the selected node (its fact-sheet sector), else the page's sector
  const nodeCell = cellKeyOfNode(posture?.node.selected);
  const pick = (k: string) => {
    const c = sheet.cells.find((x) => x.key === k);
    if (posture && c && posture.core?.meta.techs.includes(c.tech)) posture.node.select(nodeKeyOfCell(c));
    else setCell(k);
  };
  const scen = p?.core.meta.scenario_labels ?? {};
  const fam = p?.core.meta.family_scenarios;
  const scenarioNote = fam
    ? [
        fam.demand ? `demand adequacy reads ${scen[fam.demand] ?? fam.demand}` : null,
        fam.policy_gap ? `the target or climate gap reads ${scen[fam.policy_gap] ?? fam.policy_gap}` : null,
        fam.system ? `system need reads ${scen[fam.system] ?? fam.system}` : null,
      ].filter(Boolean)
    : [];
  return (
    <Section
      id="medium-fence"
      aliases={SEC['medium-fence'].aliases}
      title={SEC['medium-fence'].label}
      intro={
        <>
          <strong className="font-medium text-paper">{FENCE_MEANING.medium}</strong> At the producing stages, prize and position decide the response: Preserve a strong position,
          Catch Up where a credible pathway leads to a large prize, Partner where allies are the better answer. At Deployment &amp; Use, a high deployment need is Accelerate.
        </>
      }
    >
      {state ??
        (p && g && (
          <>
            <FenceLegend fence="medium" cells={p.cells} />
            <DetailNotice />
            <div className="grid gap-4 lg:grid-cols-2">
              <TopList
                title="Top 5 supply-chain nodes"
                testId="medium-top-supply"
                nodes={g.compete}
                by={prizeLabel(p.core.meta)}
                value={(n) => n.cell.prize}
                empty="No Preserve, Catch Up or Partner nodes."
              />
              <TopList
                title="Top 5 deployment (Accelerate)"
                testId="medium-top-deploy"
                nodes={g.accelerate}
                by="deployment need"
                value={(n) => n.cell.need}
                empty={`No Accelerate nodes: no technology's deployment need reached the acceleration bar (${score100(t?.deployment)}).`}
              />
            </div>
            <PrizePositionScatter core={p.core} cells={p.cells} name={cap(name)} selected={p.node.selected} onOpen={p.node.select} />
          </>
        ))}

      {p && g && t && (
        <Section
          id="medium-prize"
          level={3}
          title={SUB['medium-prize'].label}
          intro={
            p.core.meta.prize_measure === 'strategic_value' ? (
              <>
                <Term id={POSTURE_TERMS.strategicValue}>Strategic Industrial Value</Term> asks whether domestic capability in the node is strategically valuable; high at{' '}
                {score100(t.prize)} or more. It combines three families: the market prize (<Term id={POSTURE_TERMS.industrialOpportunity}>{MARKET_PRIZE_LABEL}</Term>: expected world
                demand growth, the node prize, and investment momentum, lifted by home-market growth only where national data exist), industrial commons (process knowledge, workforce
                and suppliers, cross-sector spillover) and future option value (the leap tests of a leap candidate). Where only the market speaks for it, the value is capped below
                the bar. Supply-chain need is context, and a Catch Up node shows the evidence of its credible pathway. Scores are 0-100.
              </>
            ) : (
              <>
                <Term id={POSTURE_TERMS.industrialOpportunity}>Industrial Opportunity</Term> is the node&apos;s market prize: expected world demand growth (the node prize) and
                investment momentum, lifted by home-market growth only where national data exist; high at {score100(t.prize)} or more. Supply-chain need is context, and a Catch Up
                node shows the evidence of its credible pathway. Scores are 0-100.
              </>
            )
          }
        >
          {g.compete.length ? (
            <LongTable
              id="medium-prize"
              rows={g.compete}
              cols={prizeCols(g.compete, p.core, names, ds, { pathway: true, reasons: false })}
              caption={`${cap(name)}: the prize of the Preserve, Catch Up and Partner nodes`}
              testId="medium-prize-table"
              what="Preserve, Catch Up and Partner nodes × the prize variables"
            />
          ) : (
            <p className="text-sm text-paper-mute">No Preserve, Catch Up or Partner nodes.</p>
          )}
        </Section>
      )}

      <Section
        id="medium-position"
        level={3}
        title={SUB['medium-position'].label}
        intro={
          t ? (
            <>
              <Term id={POSTURE_TERMS.competitivePosition}>Competitive Position</Term> is demonstrated capability (exports, specialisation, production, capacity, investment, trade
              momentum where they apply), placed among the node&apos;s contenders. The Preserve bar is {score100(t.cp)}: the top quarter of the node&apos;s contenders, for a contender above
              the capability floor. The nodes are listed by Competitive Position, strongest first. Scores are 0-100; world supply share in %.
            </>
          ) : undefined
        }
      >
        {p &&
          g &&
          (g.compete.length ? (
            <LongTable
              id="medium-position"
              rows={competeByPosition}
              cols={positionCols(competeByPosition, p.core, ds)}
              caption={`${cap(name)}: the position of the Preserve, Catch Up and Partner nodes, by Competitive Position`}
              testId="medium-position-table"
              what="Preserve, Catch Up and Partner nodes × the position variables"
            />
          ) : (
            <p className="text-sm text-paper-mute">No Preserve, Catch Up or Partner nodes.</p>
          ))}
        <TradeMonthlyCard cell={nodeCell} onPick={pick} />
      </Section>

      <PartnershipsSection
        id="medium-partner"
        level={3}
        title={SUB['medium-partner'].label}
        aliases={SUB['medium-partner'].aliases}
        lead={g ? <PartnerLead nodes={g.compete} name={name} /> : undefined}
      />

      <Section
        id="medium-accelerate"
        level={3}
        title={SUB['medium-accelerate'].label}
        intro={
          t ? (
            <>
              <Term id={POSTURE_TERMS.deploymentNeed}>Deployment need</Term> is how much more of the technology the energy system needs: demand adequacy, the gap to national targets
              and system requirements, each with its scenario named (the scenarios are not interchangeable). Accelerate at {score100(t.deployment)} or more; a climate-aligned value is an
              alternative scenario and never enters the choice. This says nothing about manufacturing at home.
            </>
          ) : undefined
        }
      >
        {p &&
          g &&
          (g.accelerate.length ? (
            <>
              <LongTable
                id="medium-accelerate"
                rows={g.accelerate}
                cols={deployCols(g.accelerate, p.core, names, ds, false)}
                caption={`${cap(name)}: the deployment need of the Accelerate nodes`}
                testId="medium-accelerate-table"
                what="Accelerate nodes × the deployment variables"
              />
              {scenarioNote.length > 0 && (
                <p className="text-xs text-paper-mute" data-testid="deployment-scenarios">
                  Scenarios: {scenarioNote.join('; ')}.
                </p>
              )}
            </>
          ) : (
            <p className="text-sm text-paper-mute">No Accelerate nodes: no technology&apos;s deployment need reached the acceleration bar ({score100(t?.deployment)}).</p>
          ))}
        <Fold id="energy-context" summary={`Energy-system context: ${poss(name)} emissions, transition indicators and the IEA pathway of its region`}>
          <CountryTransitionCards iso3={sheet.iso3} name={name} />
          <CountryClimateCard iso3={sheet.iso3} name={name} />
        </Fold>
      </Section>
    </Section>
  );
}

// ---- Low fence -----------------------------------------------------------------------------------------

export function LowFenceSection() {
  const { sheet } = useProfile();
  const name = proseName(sheet.meta.country);
  const state = usePostureState('Watch node');
  const p = useReady();
  const names = useNames(p?.core ?? null);
  const g = p ? fenceGroups(p.cells, p.detail, p.core.meta) : null;
  const ds = detailStateOf(p);
  const watchByPosition = g ? byPositionOrder(g.watchSupply) : [];
  const sub = (title: string, children: ReactNode) => (
    <div className="space-y-2">
      <h4 className="text-sm font-medium text-paper">{title}</h4>
      {children}
    </div>
  );
  return (
    <Section
      id="low-fence"
      aliases={SEC['low-fence'].aliases}
      title={SEC['low-fence'].label}
      intro={
        <>
          <strong className="font-medium text-paper">{FENCE_MEANING.low}</strong> A deliberate conclusion, not a failure: the node does not call for domestic industrial intervention.
          First the biggest prizes {name} is not chasing, then the deployments closest to Accelerate, each with the reasons the engine gives.
        </>
      }
    >
      {state ??
        (p && g && (
          <>
            <FenceLegend fence="low" cells={p.cells} />
            <DetailNotice />
            {!g.watchSupply.length && !g.watchDeploy.length ? (
              <p className="text-sm text-paper-mute" data-testid="fence-empty">
                No Watch nodes: every assessed node of {name} calls for securing, competing or accelerating.
              </p>
            ) : (
              <div className="grid gap-4 lg:grid-cols-2">
                <TopList
                  title="Top 5 supply-chain nodes"
                  testId="low-top-supply"
                  nodes={g.watchSupply}
                  by={prizeLabel(p.core.meta)}
                  value={(n) => n.cell.prize}
                  empty="No producing-stage Watch nodes."
                />
                <TopList
                  title="Top 5 deployment"
                  testId="low-top-deploy"
                  nodes={g.watchDeploy}
                  by="deployment need"
                  value={(n) => n.cell.need}
                  empty="No Deployment & Use Watch nodes."
                />
              </div>
            )}
            {g.watchSupply.length > 0 && (
              <Fold id="low-supply-all" summary={`All ${g.watchSupply.length} producing-stage Watch ${g.watchSupply.length === 1 ? 'node' : 'nodes'} × prize, position and need`}>
                <p className="text-xs text-paper-mute">Scores are 0-100; world supply share in %. Select a row for the node&apos;s five questions.</p>
                {sub(
                  'Prize and need',
                  <FenceTable rows={g.watchSupply} cols={prizeCols(g.watchSupply, p.core, names, ds, { pathway: false, reasons: true })} caption={`${cap(name)}: the prize of the producing-stage Watch nodes`} testId="low-prize-table" />,
                )}
                {sub(
                  'Competitive Position',
                  <FenceTable
                    rows={watchByPosition}
                    cols={positionCols(watchByPosition, p.core, ds)}
                    caption={`${cap(name)}: the position of the producing-stage Watch nodes, by Competitive Position`}
                    testId="low-position-table"
                  />,
                )}
              </Fold>
            )}
            {g.watchDeploy.length > 0 && (
              <Fold id="low-deploy-all" summary={`All ${g.watchDeploy.length} Deployment & Use Watch ${g.watchDeploy.length === 1 ? 'node' : 'nodes'} × deployment variables`}>
                <p className="text-xs text-paper-mute">Scores are 0-100. Select a row for the node&apos;s five questions.</p>
                <FenceTable rows={g.watchDeploy} cols={deployCols(g.watchDeploy, p.core, names, ds, true)} caption={`${cap(name)}: the deployment need of the Deployment & Use Watch nodes`} testId="low-deploy-table" />
              </Fold>
            )}
          </>
        ))}
    </Section>
  );
}
