// Allied Industrial Base partnerships /aib/partnerships (IA refactor, 2026-10-16; three lenses 2026-10-19,
// aib_scenarios_spec.md s.5 C and s.6): "Who should work with whom?" The kind of partnership, Trade |
// Investment | Development | Geopolitical (URL `mode`), and four filters, economy / OSI technology / stage /
// node (URL `country`, `tech`, `stage`, `node`), read through the shared three-lens component
// (src/partnerships): a cross-lens summary, then three tagged panels.
//   Today           Trade: who supplies whom today and today's largest suppliers (the Baseline scenario
//                   file: observed 2024 supplier -> buyer pairs, each node's top suppliers). Investment:
//                   the IMF positions and factory projects of one economy. Development: official aid
//                   (OECD; the Sector bundle, loaded only on this tab). Geopolitical: shared economic-
//                   security agreements, UN-vote alignment and the coalition tier.
//   OSI recommends  Partnership Opportunity, Friendshore and Development Potential at the NEIS defaults (the
//                   ties and pcell chunks; Development Potential through the page's one EngineScope); the
//                   allied coalition (core members and partners: the Country manifest) and Friendshore's
//                   alignment part.
//   AIB model       the toggled scenario (scn; Decoupling the main document, Managed Interdependence its
//                   file): the coordinated trade relationships across the compacts (Sankey + table) and the
//                   specialized suppliers; where new capacity buys most resilience, its hosts and the
//                   investor -> host flows; the capacity gaps (BUILD) and their hosts; the compact
//                   coalitions (members, critical supply partners, critical dependencies outside).
// Facts never respond to the Perspective; the rankings that use preference-weighted scores say so and are
// at the NEIS defaults.
//
// A pair (2026-10-20, strategic task spec s.19): the Partner filter (URL `partner`, read only with an
// economy; options grouped by whether the model writes a relationship thesis for the pair) opens the
// relationship thesis above the lenses (src/partnerships/RelationshipThesis.tsx: complementarity, priority
// strategic compacts and the AIB logic, from doc.strategic; no score), and the cross-lens summary says
// where the partner sits in the three lenses. With an economy and no partner, the card suggests the
// economy's relationships with the most priority compacts. The lenses stay the economy's.
//
// The partner indices (Partnership Opportunity, Friendshore, Development Potential) and the factory
// projects are kept by OSI technology x six-stage stage (the cells of the Strategic data). The AIB filter (an
// OSI technology, a stage, a node; 2026-10-29, design A1/A2) maps to OSI cells through the matching nodes'
// six-stage cells (osiCellPick, node.yfCells): a node matches by its cells (nodeInCell(), never node.stage),
// and the filter's technology and stage also limit the cells, so a node in two cells never lends the other
// one. The Stage choices are the stages that hold the nodes' cells (of the chosen technology). Only
// the cells this build keeps partner indices for count (its pcell chunks; the engine's cells for
// Development Potential). The OSI cards say which OSI cell they reflect (the dominant one: the first such
// cell of the matching node with the largest lost supply) and, where no matching node has one, say so
// instead of falling back silently. The Investment tab says the IMF positions are economy-wide and whether
// the dominant cell has factory projects at all (CM-FDI: the Components and Assembly cells of four
// technologies, the same projects on both, counted once). The aid (the Sector data's aid sectors, linked to
// six-stage cells) is read at the filter's stage, else the stage of the node's dominant cell (aidStageOf).
// Old ?stage=Upstream / Midstream / Downstream links name no stage of the nodes' cells and are dropped; old
// ?tech=<dictionary sector> links name no option and are dropped.

import { lazy, Suspense, useCallback, useEffect, useMemo, useState, type ReactNode } from 'react';
import { Link, useSearchParams } from 'react-router';
import { ChartCard } from '../../charts/ChartCard.tsx';
import { Legend } from '../../charts/legends.tsx';
import { SankeyChart, type SankeyFlow } from '../../charts/Sankey.tsx';
import { TableView, type Col } from '../../components/TableView.tsx';
import { EmptyState, FilterRow, Loading, Notice, PageHead, Select } from '../../components/ui.tsx';
import { useCountryManifest } from '../../country/data/hooks.ts';
import { cellLabel } from '../../lib/format.ts';
import { isSupplyStage } from '../../lib/stages.ts';
import { EngineScope } from '../../shell/EngineScope.tsx';
import { useTitle } from '../../shell/title.ts';
import { useApp } from '../../state/EngineProvider.tsx';
import { InvestmentCards } from '../../views/Partners/InvestmentCard.tsx';
import { alignmentOf, CMFDI_SECTORS, cmfdiSameProjectsNote, cmfdiStages, hasCmfdi, loadRankedCells, loadSectorTop, loadTies, type Ties, type TopRow } from '../../views/Partners/ties.ts';
import { AGGREGATE_NAMES, agreementList, agreementNames, agreementMembers, agreementTies, alignmentCounts, alignmentWords, coalitionOf, dipPartners, tierWords } from '../../partnerships/derive.ts';
import { kindOf, lensNames, lensNone, LENS_LOADING, type LensId, type LensNames, type PartnershipKind } from '../../partnerships/lenses.ts';
import { LensCard, PartnershipLenses, RankedNames, type LensPanelSpec } from '../../partnerships/PartnershipLenses.tsx';
import { RelationshipThesis } from '../../partnerships/RelationshipThesis.tsx';
import { thesisPartners } from '../../partnerships/thesis.ts';
import { PAGE, useAibCtx } from '../AibLayout.tsx';
import type { AibInvestmentFlows, AibManagedScenarioDoc, AibMeta, AibScenarioId, AibStrategic } from '../contract.ts';
import { ClassChip } from '../charts/ClassChip.tsx';
import { useAibScenarioDoc, type ScenarioLoad } from '../data/hooks.ts';
import { fmtPp, fmtScore, fmtShare, fmtUsd, listWords, plural } from '../derive/format.ts';
import { investmentPairs } from '../derive/investment.ts';
import {
  aidStageOf,
  dpRows,
  filterChoices,
  hostsNamed,
  lensBuildGaps,
  lensCoalitions,
  lensHostRows,
  lensInvestmentNodes,
  lensNodes,
  lensSpecializedSuppliers,
  osiCellPick,
  pairInvestmentFlowsOf,
  pairTradeFlowsOf,
  partnersInPairs,
  scenarioCompacts,
  scoredCellsOf,
  sideTotals,
  todayPairs,
  todaySuppliers,
  topPairs,
  type DpRow,
  type HostRow,
  type LensNode,
  type LensScenario,
  type OsiCellPick,
  type PairInvestment,
  type PairTrade,
  type PartnershipFilter,
  type ScenarioCompactRel,
  type SupplierRow,
  type TodayPair,
  type TodaySupplier,
} from '../derive/partnerships.ts';
import { runMetaOf, scenarioCsvFields, scenarioLabel, scenarioNameOf, scenarioShockLabel } from '../derive/scenarios.ts';
import { nodeWhere, osiFields, techOptions, type OsiCell } from '../derive/taxonomy.ts';
import { compactHref, strategicCompactHref } from '../nav.ts';
import { csvWithRun } from './common.tsx';
import { REL_COLOR, REL_LINK, REL_WORDS } from './compact/NetworkSection.tsx';
import { AIB_EYEBROW } from './AibOverviewView.tsx';
import { InvestmentFlowsCard } from './InvestmentFlowsCard.tsx';

const OdaToday = lazy(() => import('../../partnerships/OdaToday.tsx'));

/** The query keys this page owns (kept when an embedded Strategic view writes the URL), the scenario toggle (scn) among them. */
export const PARTNERSHIP_KEYS = ['mode', 'country', 'partner', 'tech', 'stage', 'node', 'scn'] as const;

export type PartnershipMode = PartnershipKind;
const TOP = 15;

/** The page's mode from the URL (Trade by default). */
export const modeOf = (v: string | null): PartnershipMode => kindOf(v);

function useTies(): { ties: Ties | null; error: string | null } | null {
  const [s, set] = useState<{ ties: Ties | null; error: string | null } | null>(null);
  useEffect(() => {
    let live = true;
    loadTies().then(
      (t) => live && set({ ties: t, error: t ? null : 'This data build has no partner rankings.' }),
      (e: Error) => live && set({ ties: null, error: `Could not load the partner rankings: ${e.message}` }),
    );
    return () => {
      live = false;
    };
  }, []);
  return s;
}

/**
 * The top partners of one OSI cell at the NEIS defaults (null while loading or when the build has none).
 * The cell comes from the crosswalk (osiCellPick), never from the AIB filter's names, and is six-stage, as
 * the pcell chunks are (pcell-batteries-refining).
 */
function useSectorTop(cell: OsiCell | null): { key: string; top: TopRow[] | null } | null {
  const key = cell?.slug ?? '';
  const [s, set] = useState<{ key: string; top: TopRow[] | null } | null>(null);
  useEffect(() => {
    if (!cell) return;
    let live = true;
    loadSectorTop(cell.tech, cell.stage).then(
      (t) => live && set({ key: cell.slug, top: t?.top ?? null }),
      () => live && set({ key: cell.slug, top: null }),
    );
    return () => {
      live = false;
    };
  }, [cell]);
  return key && s?.key === key ? s : null;
}

/**
 * The OSI cells this data build keeps partner rankings for (its pcell chunks; null while loading, when the
 * build has none or on error: not known, every cell counts).
 */
function useRankedCells(): ReadonlySet<string> | null {
  const [s, set] = useState<ReadonlySet<string> | null>(null);
  useEffect(() => {
    let live = true;
    loadRankedCells().then(
      (x) => live && set(x),
      () => undefined,
    );
    return () => {
      live = false;
    };
  }, []);
  return s;
}

/**
 * The OSI cells of the page's filter (memoized; null without a technology, stage or node). Its technology
 * limits the cells; its stage limits the nodes, whose six-stage cells (node.yfCells) are read. `scored`:
 * the cells with partner indices (default: the build's ranked cells).
 */
function useCellPick(f: PartnershipFilter, scored?: ReadonlySet<string> | null): OsiCellPick | null {
  const { doc } = useAibCtx();
  const ranked = useRankedCells();
  const set = scored === undefined ? ranked : scored;
  const { tech, stage, node } = f;
  return useMemo(() => osiCellPick(doc.nodes, { country: null, tech, stage, node }, { scored: set }), [doc, set, tech, stage, node]);
}

/** A node's place in a CSV: its OSI technologies and cells, its dictionary sector (provenance), the stages of its cells and the AIB model's own stage. */
const osiCsv = (tx: Parameters<typeof osiFields>[0], n: Parameters<typeof osiFields>[1]) => {
  const p = osiFields(tx, n);
  return { technologies: p.technologies, osi_cells: p.osiCells, dictionary_sector: p.dictionarySector, stage: p.stages, aib_model_stage: p.modelStage };
};

/** "Rare-earth oxides & compounds", "these 3 nodes" or "2 of these 3 nodes". */
const whoOf = (list: readonly { label: string }[], all: readonly { label: string }[]): string =>
  list.length === 1 ? list[0].label : list.length === all.length ? `these ${plural(all.length, 'node')}` : `${list.length} of these ${plural(all.length, 'node')}`;
const whoWords = (pick: OsiCellPick): string => whoOf(pick.nodes, pick.nodes);
const itsOf = (k: number) => (k === 1 ? 'its' : 'their');
/** The cells a card reads, labelled as their data are ("Solar – Components"). */
const cellWordsOf = (cells: readonly OsiCell[]) => cells.map((c) => cellLabel(c.tech, c.stage)).join(', ');

/** "the basket cell the codes of Wafers sit in; its codes also sit in Solar – Components" (the node is where its cells are: no node.stage) */
function cellSourceWords(pick: OsiCellPick, cell: OsiCell): string {
  const others = pick.cells.filter((c) => c.slug !== cell.slug);
  const who =
    pick.from && pick.nodes.length > 1
      ? `${pick.from.label} (the largest lost supply of ${whoWords(pick)})`
      : pick.nodes.length === 1
        ? pick.nodes[0].label
        : whoWords(pick);
  return `the basket cell the codes of ${who} sit in${others.length ? `; ${itsOf(pick.nodes.length)} codes also sit in ${cellWordsOf(others)}` : ''}`;
}

/** After a note on the cells with partner indices: the cells without them and the matching nodes left with none. */
function pickCaveats(pick: OsiCellPick): string {
  const out: string[] = [];
  if (pick.unscored.length) out.push(`This data build keeps no partner indices for ${cellWordsOf(pick.unscored)}, where ${itsOf(pick.nodes.length)} codes also sit.`);
  if (pick.nodes.length > 1 && pick.unmapped.length) out.push(`${plural(pick.unmapped.length, 'matching node has', 'matching nodes have')} no basket code (neutral partner scores).`);
  if (pick.nodes.length > 1 && pick.unscoredNodes.length) out.push(`${plural(pick.unscoredNodes.length, 'matching node sits', 'matching nodes sit')} only in cells without partner indices.`);
  return out.length ? ` ${out.join(' ')}` : '';
}

/**
 * The notice of a selection that reaches no OSI cell with partner indices: no matching node, nodes with no
 * basket code (partner scores neutral) or nodes whose codes sit only in cells the build keeps no partner
 * indices for (Semiconductors). `fallback`: what the card shows instead.
 */
function NoCellNotice({ pick, fallback }: { pick: OsiCellPick; fallback: string }) {
  const parts = [
    pick.unmapped.length ? `the codes of ${whoOf(pick.unmapped, pick.nodes)} sit in no OSI basket cell: in the model ${itsOf(pick.unmapped.length)} partner scores are neutral` : '',
    pick.unscoredNodes.length
      ? `the codes of ${whoOf(pick.unscoredNodes, pick.nodes)} sit only in ${cellWordsOf(pick.unscored)}, which this data build keeps no partner indices for`
      : '',
  ].filter(Boolean);
  return (
    <div data-testid="osi-cell-notice">
      <Notice kind="info" title={pick.unscoredNodes.length ? 'No OSI cell with partner indices for this selection' : 'No OSI cell for this selection'}>
        {pick.nodes.length
          ? `Partner indices are kept by OSI technology × stage, and ${parts.join('; ')}. ${fallback}`
          : `No node matches this selection, so it reaches no OSI cell (partner indices are kept by OSI technology × stage). ${fallback}`}
      </Notice>
    </div>
  );
}

/**
 * Which OSI cell an OSI card reflects, in words (null without a filter): the dominant cell of the matching
 * nodes (`cells`: all of them, for a card that reads every cell), or the notice that no matching node has a
 * cell with partner indices. `fallback`: what the card shows instead.
 */
function OsiCellNote({ pick, cells = false, fallback }: { pick: OsiCellPick | null; cells?: boolean; fallback: string }) {
  if (!pick) return null;
  if (!pick.cells.length) return <NoCellNotice pick={pick} fallback={fallback} />;
  const text = cells
    ? `Partner indices for the OSI ${pick.cells.length === 1 ? 'cell' : 'cells'} ${cellWordsOf(pick.cells)}: the basket ${pick.cells.length === 1 ? 'cell' : 'cells'} the codes of ${whoWords(pick)} sit in.`
    : pick.cell
      ? `Partner indices for the OSI cell ${cellLabel(pick.cell.tech, pick.cell.stage)}, ${cellSourceWords(pick, pick.cell)}.`
      : `This selection spans ${plural(pick.cells.length, 'OSI cell')} (${cellWordsOf(pick.cells)}): ${fallback}`;
  return (
    <p className="text-xs text-paper-mute" data-testid="osi-cell-note">
      {text}
      {pickCaveats(pick)}
    </p>
  );
}

const ECONOMY_WIDE = 'The direct-investment positions below are economy-wide';
/**
 * The cells with factory projects (hasCmfdi: the CM-FDI technologies at the stages the ties chunk records
 * them at, Components and Assembly, which carry the same projects), in words.
 */
const cmfdiWords = (stages: readonly string[]) => `the ${listWords([...stages])} cells of ${listWords(CMFDI_SECTORS)}`;

/**
 * What the Investment tab's cards for one economy read (null without a filter): the IMF positions are
 * economy-wide; the factory projects are kept by OSI cell and recorded only for the CM-FDI cells, so they
 * are shown for the dominant cell when it is one of those (saying once that the CM-FDI stages carry the
 * same projects), and the note says so when it is not.
 */
function InvestmentCellNote({ pick, ties }: { pick: OsiCellPick | null; ties: Ties | null }) {
  if (!pick) return null;
  const stages = cmfdiStages(ties);
  const same = cmfdiSameProjectsNote(stages);
  const notShown = `${ECONOMY_WIDE}; factory projects are kept by OSI cell and are not shown.`;
  if (!pick.cells.length) return <NoCellNotice pick={pick} fallback={notShown} />;
  const cell = pick.cell;
  const text = !cell
    ? `This selection spans ${plural(pick.cells.length, 'OSI cell')} (${cellWordsOf(pick.cells)}): ${notShown}`
    : hasCmfdi(cell.tech, cell.stage, stages)
      ? `Factory projects are kept by OSI cell: shown for ${cellLabel(cell.tech, cell.stage)}, ${cellSourceWords(pick, cell)}.${same ? ` ${same}` : ''} ${ECONOMY_WIDE}.`
      : `${ECONOMY_WIDE}. Factory projects are kept by OSI cell and recorded only for ${cmfdiWords(stages)}; ${cellLabel(cell.tech, cell.stage)}, ${cellSourceWords(pick, cell)}, has none, so none are shown.`;
  return (
    <p className="text-xs text-paper-mute" data-testid="osi-cell-investment-note">
      {text}
      {pickCaveats(pick)}
    </p>
  );
}

/**
 * Names for any economy: the compacts document's, then a scenario file's, the ties chunk's (it covers every
 * scored economy) and the Country roster's.
 */
function useNamer(ties: Ties | null | undefined, extra?: { scenario?: Parameters<typeof scenarioNameOf>[1]; roster?: readonly { iso3: string; name: string }[] }): (iso: string) => string {
  const { names } = useAibCtx();
  const scenarioDoc = extra?.scenario;
  const roster = extra?.roster;
  return useMemo(() => {
    const m = new Map<string, string>(Object.entries(AGGREGATE_NAMES));
    for (const r of roster ?? []) m.set(r.iso3, r.name);
    if (ties) ties.U.forEach((u, i) => m.set(u, ties.U_names[i] ?? u));
    const sc = scenarioNameOf((iso) => (iso ? (m.get(iso) ?? iso) : ''), scenarioDoc);
    return (iso: string) => names[iso] ?? sc(iso);
  }, [ties, names, scenarioDoc, roster]);
}

/** A short ranked list (the chart of a card whose twin is a table). */
function RankList<T>({ rows, label, render }: { rows: T[]; label: string; render: (r: T) => ReactNode }) {
  return (
    <ol className="divide-y divide-white/5 text-sm" aria-label={label}>
      {rows.map((r, i) => (
        <li key={i} className="flex flex-wrap items-baseline gap-x-3 gap-y-1 py-2">
          {render(r)}
        </li>
      ))}
    </ol>
  );
}

/**
 * The link to an economy's Country profile (its Partnerships section by default), the scenario toggle
 * carried: the AIB lens there follows the same `scn`.
 */
function useCountryHref(): (iso: string, hash?: string) => string {
  const { href } = useAibCtx();
  return useCallback((iso: string, hash = 'partnerships') => href(`/countries/${iso}#${hash}`), [href]);
}

/** The scenario the AIB lens reads, its label, and the Managed Interdependence file when that is the choice. */
function useLensScenario(): { id: LensScenario; label: string; load: ScenarioLoad<AibManagedScenarioDoc>; ready: boolean } {
  const { doc, scenario } = useAibCtx();
  // Decoupling reads the main document: the hook stays idle
  const load = useAibScenarioDoc(scenario === 'managed' ? 'managed' : null) as ScenarioLoad<AibManagedScenarioDoc>;
  return { id: scenario, label: scenarioLabel(scenario, doc.meta), load, ready: scenario === 'decoupling' || !!load.doc };
}

/** The AIB lens while the Managed Interdependence file is not there: loading, failed (Retry) or not in this build. */
function pendingSpec(load: ScenarioLoad<unknown>): LensPanelSpec {
  if (load.error) {
    return {
      names: lensNone('the Managed Interdependence data could not be loaded'),
      content: (
        <Notice kind="error" title="The Managed Interdependence data could not be loaded" action={<button type="button" className="btn btn-ghost btn-sm" onClick={load.retry}>Retry</button>}>
          {load.error.message}
        </Notice>
      ),
    };
  }
  if (load.missing) return { names: lensNone('this build has no Managed Interdependence detail'), empty: 'This data build has no Managed Interdependence detail for the partnerships: choose Decoupling.' };
  return { names: LENS_LOADING, content: <Loading label="Loading the Managed Interdependence results" /> };
}

/** A line under the AIB lens under Managed Interdependence: what it changes and what it leaves as today. */
function ManagedNote() {
  return (
    <p className="text-xs leading-relaxed text-paper-mute" data-testid="managed-note">
      Managed Interdependence changes only the sub-sectors more concentrated than comparable goods; elsewhere trade stays as it is today, so those sub-sectors add no relationship here.
    </p>
  );
}

/** CSV text with the scenario named (aib_scenario, aib_scenario_shock; csvWithRun() owns scenario and shock) and the run columns naming its shock. */
const csvScenario = (rows: Record<string, unknown>[], meta: AibMeta, id: AibScenarioId) => csvWithRun(rows.map((r) => ({ ...r, ...scenarioCsvFields(meta, id) })), runMetaOf(meta, id));

export default function AibPartnershipsView() {
  useTitle('Partnerships', 'aib');
  const { doc, tx, strategic, name: nameOf, href } = useAibCtx();
  const [params, setParams] = useSearchParams();
  const mode = modeOf(params.get('mode'));
  const raw: PartnershipFilter = { country: params.get('country'), tech: params.get('tech'), stage: params.get('stage'), node: params.get('node') };
  const all = useMemo(() => filterChoices(doc, { country: null, tech: null, stage: null, node: null }), [doc]);
  const tech = raw.tech && all.techs.includes(raw.tech) ? raw.tech : null;
  // a stage that holds the nodes' cells, of the chosen technology (six-stage; an old three-stage link names none and is dropped)
  const stages = useMemo(() => filterChoices(doc, { country: null, tech, stage: null, node: null }).stages, [doc, tech]);
  const stage = raw.stage && isSupplyStage(raw.stage) && stages.includes(raw.stage) ? raw.stage : null;
  const country = raw.country && all.countries.some((c) => c.iso3 === raw.country) ? raw.country : null;
  const scopedNodes = useMemo(() => filterChoices(doc, { country: null, tech, stage, node: null }).nodes, [doc, tech, stage]);
  const node = raw.node && scopedNodes.some((n) => n.id === raw.node) ? raw.node : null;
  const f = useMemo<PartnershipFilter>(() => ({ country, tech, stage, node }), [country, tech, stage, node]);
  // the pair: a partner is read only with an economy; the economies the model writes a thesis with come first
  const paired = useMemo(() => partnerChoices(all.countries, strategic, country, nameOf), [all.countries, strategic, country, nameOf]);
  const rawPartner = params.get('partner');
  const partner = country && rawPartner && paired.some((c) => c.iso3 === rawPartner) ? rawPartner : null;

  const set = (k: string, v: string) => {
    const q = new URLSearchParams(params);
    if (v && !(k === 'mode' && v === 'trade')) q.set(k, v);
    else q.delete(k);
    if (k === 'tech' || k === 'stage') q.delete('node');
    // a partner needs an economy, and never the economy itself
    if (k === 'country' && (!v || v === q.get('partner'))) q.delete('partner');
    setParams(q, { replace: true, preventScrollReset: true });
  };
  const onKind = (k: PartnershipKind) => set('mode', k);
  const pairHref = (iso: string) => {
    const q = new URLSearchParams(params);
    q.set('partner', iso);
    return `?${q.toString()}`;
  };
  const partnerOptions = [
    { value: '', label: country ? 'No partner' : 'Choose an economy first' },
    ...paired.map((c) => ({ value: c.iso3, label: c.name, ...(strategic && country ? { group: c.thesis ? 'With a relationship thesis' : 'Other economies' } : {}) })),
  ];

  return (
    <div className={PAGE}>
      <PageHead
        eyebrow={AIB_EYEBROW}
        title="Partnerships"
        lede="Who should work with whom? Each kind of partnership (trade, investment, development, geopolitical) is read three ways: as it is today, as the Opportunity Security Indices recommend, and as the Allied Industrial Base model arranges it under the chosen scenario. The summary says where the three agree and where they differ."
      />
      <FilterRow>
        <Select
          label="Economy"
          value={country ?? ''}
          options={[{ value: '', label: 'All economies' }, ...all.countries.map((c) => ({ value: c.iso3, label: c.name }))]}
          onChange={(v) => set('country', v)}
        />
        <Select
          label="Partner"
          value={partner ?? ''}
          options={partnerOptions}
          onChange={(v) => set('partner', v)}
          disabled={!country}
          title={country ? 'A second economy: the relationship thesis of the pair, above the lenses' : 'Choose an economy first'}
        />
        <Select label={tx.One} value={tech ?? ''} options={[{ value: '', label: tx.all }, ...techOptions(all.techs)]} onChange={(v) => set('tech', v)} />
        <Select label="Stage" value={stage ?? ''} options={[{ value: '', label: 'All stages' }, ...stages.map((s) => ({ value: s, label: s }))]} onChange={(v) => set('stage', v)} className="min-w-[9rem]" />
        <Select
          label="Node"
          value={node ?? ''}
          options={[{ value: '', label: 'All nodes' }, ...scopedNodes.map((n) => ({ value: n.id, label: n.label }))]}
          onChange={(v) => set('node', v)}
          className="min-w-[14rem]"
        />
      </FilterRow>
      {strategic && country && (
        <div className="mb-8">
          <RelationshipThesis
            id="aibp-thesis"
            strategic={strategic}
            a={country}
            b={partner}
            name={nameOf}
            compactHref={(id) => href(strategicCompactHref(id))}
            countryHref={(iso) => href(`/countries/${iso}#aib-role`)}
            headingLevel={2}
            pairHref={pairHref}
          />
        </div>
      )}
      <div className="mt-6">
        {mode === 'trade' && <TradeTab f={f} partner={partner} onKind={onKind} />}
        {mode === 'investment' && <InvestmentTab f={f} partner={partner} onKind={onKind} />}
        {mode === 'development' && <DevelopmentTab f={f} partner={partner} onKind={onKind} />}
        {mode === 'geopolitical' && <GeopoliticalTab f={f} partner={partner} onKind={onKind} />}
      </div>
    </div>
  );
}

/** "Japan, Solar, Components" or "all economies and sub-sectors" (the filter's own, six-stage stage). */
function useScope(f: PartnershipFilter): string {
  const { name, nodeOf } = useAibCtx();
  const parts = [f.country ? name(f.country) : null, f.node ? (nodeOf(f.node)?.label ?? f.node) : null, !f.node && f.tech ? f.tech : null, !f.node && f.stage ? f.stage : null].filter(Boolean);
  return parts.length ? parts.join(', ') : 'all economies and sub-sectors';
}

interface TabProps {
  f: PartnershipFilter;
  /** the pair's other economy (with an economy only) */
  partner: string | null;
  onKind: (k: PartnershipKind) => void;
}

/** The shared component with the page's own settings. */
function Lenses({ kind, f, partner, onKind, name, lenses, scenario }: TabProps & { kind: PartnershipKind; name: (iso: string) => string; lenses: Record<LensId, LensPanelSpec>; scenario: string }) {
  const scope = useScope(f);
  const toCountry = useCountryHref();
  return (
    <PartnershipLenses
      idPrefix={`aibp-${kind}`}
      kind={kind}
      onKind={onKind}
      kindLabel="Partnership type"
      headingLevel={2}
      focus={scope}
      noun={f.country ? { one: 'partner', many: 'partners' } : { one: 'economy', many: 'economies' }}
      name={name}
      partnerHref={toCountry}
      scenarioLabel={scenario}
      lenses={lenses}
      pair={partner}
    />
  );
}

/**
 * The Partner filter's economies: the document's (as the Economy filter), the economies the strategic layer
 * assesses (a major supplier outside the allied economies reads as such) and the theses' partners of the
 * economy, the focus left out; `thesis` marks a pair the model writes a thesis for.
 */
function partnerChoices(
  countries: readonly { iso3: string; name: string }[],
  strategic: AibStrategic | null,
  country: string | null,
  name: (iso: string) => string,
): { iso3: string; name: string; thesis: boolean }[] {
  if (!country) return countries.map((c) => ({ ...c, thesis: false }));
  const withThesis = new Set(thesisPartners(strategic, country));
  const seen = new Map(countries.map((c) => [c.iso3, c.name]));
  for (const x of [...withThesis, ...(strategic?.countries ?? []).map((c) => c.iso3)]) if (!seen.has(x)) seen.set(x, name(x));
  return [...seen.entries()]
    .filter(([iso]) => iso !== country)
    .map(([iso3, n]) => ({ iso3, name: n, thesis: withThesis.has(iso3) }))
    .sort((a, b) => Number(b.thesis) - Number(a.thesis) || a.name.localeCompare(b.name, 'en'));
}

// ---- The OSI rankings (Partnership Opportunity, Friendshore) ---------------------------------------------

interface TopRows {
  ties: { ties: Ties | null; error: string | null } | null;
  pick: OsiCellPick | null;
  cell: OsiCell | null;
  /** the pcell chunk of the dominant cell is read (else every sector, the ties chunk's best five) */
  bySector: boolean;
  /** the dominant cell's ranking is still loading */
  sectorLoading: boolean;
  /** the dominant cell has no ranking in this build */
  sectorMissing: boolean;
  rows: TopRow[];
}

/** The NEIS-defaults top partners of one side, scoped (the economy's own ranking when one is chosen). */
function useTopRows(side: 'po' | 'fs', f: PartnershipFilter): TopRows {
  const ties = useTies();
  const pick = useCellPick(f);
  const cell = pick?.cell ?? null;
  const sector = useSectorTop(cell);
  const bySector = !!(cell && sector?.top);
  const rows = useMemo(() => {
    const src = bySector ? (sector?.top ?? []) : (ties?.ties?.top_all ?? []);
    return topPairs(src, side, f.country, TOP);
  }, [bySector, sector, ties, side, f.country]);
  return { ties, pick, cell, bySector, sectorLoading: !!cell && sector === null, sectorMissing: !!cell && sector !== null && !sector.top, rows };
}

/** What the OSI lens names: the economy's partners, else (no economy) the exporters (Partnership Opportunity) or the partners to build with (Friendshore). */
function topNames(t: TopRows, side: 'po' | 'fs', country: string | null): LensNames {
  if (!t.ties || t.sectorLoading) return LENS_LOADING;
  if (t.pick && !t.pick.cells.length) return lensNone('no OSI cell with partner indices for this selection');
  if (t.ties.error && !t.bySector) return lensNone('the partner rankings are not in this build');
  const keys = country ? t.rows.map((r) => r.partner_iso) : t.rows.map((r) => (side === 'po' ? r.reporter_iso : r.partner_iso));
  return lensNames(keys, 10, country);
}

const SIDE_WORDS = {
  po: { title: 'Partnership Opportunity', score: 'Partnership Opportunity score', who: 'Exporter', partner: 'Market', what: 'how attractive each partner is as an export market' },
  fs: { title: 'Friendshore partners', score: 'Friendshore score', who: 'Economy', partner: 'Partner to build with', what: 'how attractive each partner is for friendshoring production and investment' },
} as const;

/** The NEIS-defaults top partners (Partnership Opportunity or Friendshore) of the ties chunk, scoped. */
function TopPartnersCard({ side, f, t }: { side: 'po' | 'fs'; f: PartnershipFilter; t: TopRows }) {
  const name = useNamer(t.ties?.ties);
  const toCountry = useCountryHref();
  const w = SIDE_WORDS[side];
  const { rows, cell, bySector, ties, pick } = t;
  const where = bySector && cell ? `the OSI cell ${cellLabel(cell.tech, cell.stage)}` : 'all sectors';
  const cols: Col<TopRow>[] = [
    { id: 'who', header: w.who, value: (r) => name(r.reporter_iso), cell: (r) => <Link className="link" to={toCountry(r.reporter_iso)}>{name(r.reporter_iso)}</Link> },
    { id: 'partner', header: w.partner, value: (r) => name(r.partner_iso), cell: (r) => <Link className="link" to={toCountry(r.partner_iso)}>{name(r.partner_iso)}</Link> },
    { id: 'value', header: w.score, value: (r) => r.value, cell: (r) => fmtScore(r.value), num: true },
    { id: 'rank', header: 'Rank among its partners', value: (r) => r.rank, num: true },
  ];
  const body = !ties ? (
    <Loading label="Loading the partner rankings" />
  ) : ties.error && !bySector ? (
    <Notice kind="info">{ties.error}</Notice>
  ) : rows.length ? (
    <RankList
      rows={rows.slice(0, 8)}
      label={`${w.title}: the strongest pairs`}
      render={(r) => (
        <>
          <span className="w-12 shrink-0 text-right font-medium tabular text-paper">{fmtScore(r.value)}</span>
          <span className="text-paper-dim">
            {name(r.reporter_iso)} &rarr; {name(r.partner_iso)}
          </span>
        </>
      )}
    />
  ) : (
    <EmptyState>No ranked partners for this selection.</EmptyState>
  );
  return (
    <ChartCard
      headingLevel={3}
      title={w.title}
      sub={`The strongest pairs by ${w.score} (${w.what}), ${where}${f.country ? `, ${name(f.country)}'s own ranking` : ''}. A preference-weighted score: a Country profile can change the Perspective; here it is at the NEIS defaults.${t.sectorMissing ? ' No ranking for that OSI cell in this data build: all sectors shown.' : ''}`}
      table={() => <TableView rows={rows} cols={cols} caption={`${w.title}, ${where}`} pageSize={20} />}
    >
      <div className="space-y-3">
        <OsiCellNote pick={pick} fallback="The ranking below covers all sectors." />
        {body}
      </div>
    </ChartCard>
  );
}

// ---- Trade -----------------------------------------------------------------------------------------------

function TradeTab({ f, partner, onKind }: TabProps) {
  const { doc, nodeOf } = useAibCtx();
  const sc = useLensScenario();
  const baseline = useAibScenarioDoc('baseline');
  const t = useTopRows('po', f);
  const name = useNamer(t.ties?.ties, { scenario: sc.load.doc ?? baseline.doc });
  const scope = useScope(f);
  const toCountry = useCountryHref();
  const label = (id: string) => nodeOf(id)?.label ?? id;
  const compacts = useMemo(() => scenarioCompacts(doc, sc.id, sc.load.doc), [doc, sc.id, sc.load.doc]);
  const pairs = useMemo(() => (compacts ? pairTradeFlowsOf(doc.nodes, compacts, f) : []), [doc, compacts, f]);
  const rows = useMemo(() => lensNodes(doc.nodes, sc.id), [doc, sc.id]);
  const suppliers = useMemo(() => lensSpecializedSuppliers(rows, f), [rows, f]);
  const tPairs = useMemo(() => todayPairs(baseline.doc?.network, doc.nodes, f, baseline.doc), [baseline.doc, doc, f]);
  const tSup = useMemo(() => todaySuppliers(doc.nodes, f, baseline.doc), [doc, f, baseline.doc]);

  // Today
  const todayNames: LensNames = f.country
    ? baseline.loading
      ? LENS_LOADING
      : baseline.doc?.network
        ? lensNames(partnersInPairs(tPairs, f.country), 10, f.country)
        : lensNone("today's supplier-to-buyer pairs are not in this build")
    : baseline.loading && !tSup.length
      ? LENS_LOADING
      : tSup.length
        ? lensNames(tSup.map((s) => s.iso3))
        : lensNone("today's suppliers are not in this build");
  const today: LensPanelSpec = {
    names: todayNames,
    content: <TodayTrade f={f} pairs={tPairs} suppliers={tSup} baseline={baseline} name={name} label={label} />,
  };
  // OSI
  const osi: LensPanelSpec = { names: topNames(t, 'po', f.country), content: <TopPartnersCard side="po" f={f} t={t} /> };
  // AIB
  let aib: LensPanelSpec;
  if (!sc.ready) aib = pendingSpec(sc.load);
  else {
    const total = pairs.reduce((s, p) => s + p.value, 0);
    const nNew = pairs.filter((p) => p.relationship === 'new').length;
    const cols: Col<PairTrade>[] = [
      { id: 'from', header: 'Exporter', value: (p) => name(p.producer), cell: (p) => <Link className="link" to={toCountry(p.producer)}>{name(p.producer)}</Link> },
      { id: 'to', header: 'Importer', value: (p) => name(p.consumer), cell: (p) => <Link className="link" to={toCountry(p.consumer)}>{name(p.consumer)}</Link> },
      { id: 'usd', header: 'Added trade', value: (p) => p.value, cell: (p) => fmtUsd(p.value), num: true },
      { id: 'contribution', header: 'Share of node demand served', value: (p) => p.share, cell: (p) => fmtShare(p.share), num: true, title: 'The largest share of one node’s demand the relationship serves in the coordinated solution' },
      { id: 'rel', header: 'Relationship', value: (p) => REL_WORDS[p.relationship] },
      { id: 'opp', header: 'Partnership Opportunity score', value: (p) => p.opportunity, cell: (p) => fmtScore(p.opportunity), num: true },
      { id: 'nodes', header: 'Sub-sectors', value: (p) => p.nodes.map(label).join(', '), minWidth: 200 },
      { id: 'compacts', header: 'Compacts', value: (p) => p.compacts.join(', '), cell: (p) => <CompactLinks ids={p.compacts} /> },
    ];
    const csv = () => ({
      name: `Allied Industrial Base trade relationships ${sc.label} ${scope}`,
      text: csvScenario(
        pairs.map((p) => ({
          exporter_iso3: p.producer, exporter: name(p.producer), importer_iso3: p.consumer, importer: name(p.consumer), added_trade_usd: p.value,
          largest_share_of_node_demand: p.share, relationship: p.relationship, mean_partnership_opportunity: p.opportunity, sub_sectors: p.nodes.map(label).join('; '), compacts: p.compacts.join('; '),
        })),
        doc.meta,
        sc.id,
      ),
    });
    const supCols: Col<SupplierRow>[] = [
      { id: 'name', header: 'Economy', value: (r) => name(r.iso3), cell: (r) => <Link className="link" to={toCountry(r.iso3, 'aib-role')}>{name(r.iso3)}</Link> },
      { id: 'n', header: 'Sub-sectors where it is the largest supplier', value: (r) => r.nodes.length, num: true },
      { id: 'top', header: 'Its largest share', value: (r) => r.top, cell: (r) => fmtShare(r.top), num: true },
      { id: 'nodes', header: 'Sub-sectors', value: (r) => r.nodes.map(label).join(', '), minWidth: 240 },
    ];
    aib = {
      names: lensNames(f.country ? partnersInPairs(pairs, f.country) : sideTotals(pairs, 'producer'), 10, f.country),
      content: (
        <div className="space-y-6">
          {sc.id === 'managed' && <ManagedNote />}
          <ChartCard
            headingLevel={3}
            title="Coordinated trade relationships"
            sub={`Exporter to importer, ${scope}, under ${sc.label}: the ${Math.min(TOP, pairs.length)} largest of ${plural(pairs.length, 'relationship')} the model adds or expands to restore supply (${fmtUsd(total)} a year${nNew ? `; ${nNew} new` : ''}). Band width is the added trade.`}
            legend={<Legend label="Relationship" items={[{ label: REL_WORDS.new, color: REL_COLOR.new }, { label: REL_WORDS.expanded, color: REL_COLOR.expanded }]} />}
            summary={`Trade relationships, ${scope}, ${sc.label}: ${pairs.slice(0, 5).map((p) => `${name(p.producer)} to ${name(p.consumer)} ${fmtUsd(p.value)}`).join('; ') || 'none'}.`}
            table={() => <TableView rows={pairs} cols={cols} caption={`Coordinated trade relationships, ${scope}, ${sc.label}`} search pageSize={20} initialSort={{ id: 'usd', desc: true }} />}
            csv={csv}
            notes={<span>Each compact&apos;s export keeps its largest relationships; the Partnership Opportunity score is the partner score the model uses to break ties among equally good solutions.</span>}
          >
            {pairs.length ? (
              <SankeyChart
                flows={pairs.slice(0, TOP)}
                label={(code) => name(code)}
                ariaLabel={`Coordinated trade relationships, ${scope}, exporter to importer`}
                valueFormat={fmtUsd}
                linkColor={(fl: SankeyFlow) => REL_LINK[(fl as PairTrade).relationship] ?? REL_LINK.expanded}
                nodeColor={(_, side) => (side === 'producer' ? '#1889b0' : '#6b7280')}
                sideLabels={{ left: 'EXPORTERS', right: 'IMPORTERS' }}
                tipNote="added trade a year, coordinated solution"
              />
            ) : (
              <EmptyState>No added trade relationship for {scope} under {sc.label}: existing shipments carry the response, or no compact covers this selection.</EmptyState>
            )}
          </ChartCard>
          <ChartCard
            headingLevel={3}
            title="Specialized suppliers"
            sub={`Who the coordinated solution leans on, ${scope}, under ${sc.label}: the economies that are the largest supplier of a sub-sector after coordination, with their largest share.`}
            table={() => <TableView rows={suppliers} cols={supCols} caption={`Specialized suppliers, ${scope}`} search pageSize={20} />}
          >
            {suppliers.length ? (
              <RankList
                rows={suppliers.slice(0, 8)}
                label="Largest suppliers of the coordinated solution"
                render={(r) => (
                  <>
                    <Link className="link min-w-[9rem]" to={toCountry(r.iso3, 'aib-role')}>
                      {name(r.iso3)}
                    </Link>
                    <span className="text-paper-dim">
                      {plural(r.nodes.length, 'sub-sector')} (up to {fmtShare(r.top, 0)}): {listWords(r.nodes.slice(0, 3).map(label))}
                      {r.nodes.length > 3 ? ' and more' : ''}
                    </span>
                  </>
                )}
              />
            ) : (
              <EmptyState>No optimized supplier shares for {scope} under {sc.label}.</EmptyState>
            )}
          </ChartCard>
        </div>
      ),
    };
  }
  return <Lenses kind="trade" f={f} partner={partner} onKind={onKind} name={name} lenses={{ today, osi, aib }} scenario={sc.label} />;
}

/** Today's trade: who supplies whom (the Baseline network) and the largest suppliers to the allied market. */
function TodayTrade({
  f, pairs, suppliers, baseline, name, label,
}: {
  f: PartnershipFilter;
  pairs: TodayPair[];
  suppliers: TodaySupplier[];
  baseline: ScenarioLoad<unknown>;
  name: (iso: string) => string;
  label: (id: string) => string;
}) {
  const { doc } = useAibCtx();
  const scope = useScope(f);
  const toCountry = useCountryHref();
  const filtered = !!(f.tech || f.stage || f.node);
  const year = doc.meta.tradeYear;
  const pairCols: Col<TodayPair>[] = [
    { id: 'from', header: 'Supplier', value: (p) => name(p.producer), cell: (p) => <Link className="link" to={toCountry(p.producer)}>{name(p.producer)}</Link> },
    { id: 'to', header: 'Buyer', value: (p) => name(p.consumer), cell: (p) => <Link className="link" to={toCountry(p.consumer)}>{name(p.consumer)}</Link> },
    ...(filtered
      ? [{ id: 'scope', header: `Supplier's supply in scope, ${year}`, value: (p: TodayPair) => p.scopeUsd, cell: (p: TodayPair) => fmtUsd(p.scopeUsd), num: true, title: "The supplier's observed supply to the allied market over the sub-sectors in scope (each sub-sector's top ten suppliers): what the pairs are ranked by" }]
      : []),
    { id: 'usd', header: filtered ? `Pair's trade in ${year}, every sub-sector` : `Trade in ${year}`, value: (p) => p.value, cell: (p) => fmtUsd(p.value), num: true },
    { id: 'nodes', header: 'Sub-sectors in scope', value: (p) => p.nodes.length, num: true },
    { id: 'all', header: 'Sub-sectors it trades in', value: (p) => p.nAll, num: true },
  ];
  const supCols: Col<TodaySupplier>[] = [
    { id: 'name', header: 'Supplier', value: (r) => name(r.iso3), cell: (r) => <Link className="link" to={toCountry(r.iso3)}>{name(r.iso3)}</Link> },
    { id: 'usd', header: `Supply to the allied market, ${year}`, value: (r) => r.usd, cell: (r) => fmtUsd(r.usd), num: true },
    { id: 'n', header: 'Sub-sectors where it is a top-ten supplier', value: (r) => r.nodes.length, num: true },
    { id: 'nodes', header: 'Sub-sectors', value: (r) => r.nodes.map(label).join(', '), minWidth: 220 },
  ];
  const retry = baseline.error ? (
    <p className="text-sm text-paper-mute">
      Today&apos;s supplier-to-buyer pairs could not be loaded.{' '}
      <button type="button" className="link" onClick={baseline.retry}>
        Retry
      </button>
    </p>
  ) : null;
  return (
    <div className="space-y-6">
      <ChartCard
        headingLevel={3}
        title="Who supplies whom today"
        sub={`Observed ${year} trade, supplier to buyer (buyers in the allied market), ${scope}: ${
          filtered
            ? 'the pairs whose supplier supplies most over the sub-sectors in scope (each sub-sector’s top ten suppliers). A pair’s value covers every strategic sub-sector it trades in; the table counts those in scope.'
            : 'the largest pairs.'
        }`}
        table={() => <TableView rows={pairs} cols={pairCols} caption={`Who supplies whom today, ${scope}`} search pageSize={20} initialSort={{ id: filtered ? 'scope' : 'usd', desc: true }} />}
        csv={() => ({
          name: `Allied Industrial Base trade today ${scope}`,
          text: csvScenario(
            pairs.map((p) => ({
              supplier_iso3: p.producer, supplier: name(p.producer), buyer_iso3: p.consumer, buyer: name(p.consumer),
              ...(filtered ? { supplier_supply_in_scope_usd: p.scopeUsd } : {}),
              trade_usd: p.value, sub_sectors_in_scope: p.nodes.length, sub_sectors_traded: p.nAll,
            })),
            doc.meta,
            'baseline',
          ),
        })}
      >
        {baseline.loading ? (
          <Loading label="Loading today's trade" />
        ) : retry ? (
          retry
        ) : baseline.missing || !baseline.doc ? (
          <p className="text-sm text-paper-mute" data-testid="lens-empty">
            This data build has no supplier-to-buyer pairs for today; the largest suppliers below come from each sub-sector&apos;s top five.
          </p>
        ) : pairs.length ? (
          <RankedNames
            rows={pairs}
            max={8}
            label="Who supplies whom today, the largest pairs"
            keyOf={(p) => `${p.producer}|${p.consumer}`}
            nameOf={(p) => `${name(p.producer)} → ${name(p.consumer)}`}
            value={(p) => fmtUsd(p.value)}
            note={(p) =>
              filtered
                ? `${name(p.producer)} supplies ${fmtUsd(p.scopeUsd)} in scope; the pair trades in ${plural(p.nodes.length, 'sub-sector')} in scope of ${p.nAll}`
                : `${plural(p.nAll, 'sub-sector')}`
            }
          />
        ) : (
          <EmptyState>No observed supplier-to-buyer pair for {scope}.</EmptyState>
        )}
      </ChartCard>
      <ChartCard
        headingLevel={3}
        title="Largest suppliers today"
        sub={`Observed ${year} supply to the allied market, summed over the sub-sectors in scope (each sub-sector's top ten suppliers)${f.country ? '; the economy filter does not apply here' : ''}.`}
        table={() => <TableView rows={suppliers} cols={supCols} caption={`Largest suppliers today, ${scope}`} search pageSize={20} initialSort={{ id: 'usd', desc: true }} />}
      >
        {suppliers.length ? (
          <RankedNames
            rows={suppliers}
            max={8}
            label="Largest suppliers to the allied market today"
            keyOf={(r) => r.iso3}
            nameOf={(r) => name(r.iso3)}
            href={(r) => toCountry(r.iso3)}
            value={(r) => fmtUsd(r.usd)}
            note={(r) => `a top-ten supplier in ${plural(r.nodes.length, 'sub-sector')}`}
          />
        ) : baseline.loading ? (
          <Loading label="Loading today's suppliers" />
        ) : (
          <EmptyState>No observed supplier figures for {scope}.</EmptyState>
        )}
      </ChartCard>
    </div>
  );
}

function CompactLinks({ ids }: { ids: string[] }) {
  const { href } = useAibCtx();
  return (
    <>
      {ids.map((id, i) => (
        <span key={id}>
          {i > 0 && ', '}
          <Link className="link" to={href(compactHref(id))}>
            {id}
          </Link>
        </span>
      ))}
    </>
  );
}

// ---- Investment ------------------------------------------------------------------------------------------

function InvestmentTab({ f, partner, onKind }: TabProps) {
  const { doc, nodeOf, tx } = useAibCtx();
  const sc = useLensScenario();
  const t = useTopRows('fs', f);
  const ties = t.ties;
  const name = useNamer(ties?.ties, { scenario: sc.load.doc });
  const scope = useScope(f);
  const toCountry = useCountryHref();
  const label = (id: string) => nodeOf(id)?.label ?? id;
  const pick = t.pick;
  const flows: AibInvestmentFlows | null = sc.id === 'decoupling' ? (doc.investmentFlows ?? null) : (sc.load.doc?.investmentFlows ?? null);
  const rows = useMemo(() => lensNodes(doc.nodes, sc.id, flows), [doc, sc.id, flows]);
  const nodes = useMemo(() => lensInvestmentNodes(rows, f), [rows, f]);
  const hosts = useMemo(() => lensHostRows(rows, f), [rows, f]);
  const compacts = useMemo(() => scenarioCompacts(doc, sc.id, sc.load.doc), [doc, sc.id, sc.load.doc]);
  const compactPairs = useMemo(() => (compacts ? pairInvestmentFlowsOf(doc.nodes, compacts, f) : []), [doc, compacts, f]);
  const best = useMemo(() => (flows ? investmentPairs({ nodes: doc.nodes, investmentFlows: flows }, f) : []), [doc, flows, f]);

  // Today
  const today: LensPanelSpec = f.country
    ? {
        names: !ties ? LENS_LOADING : ties.ties ? lensNames(dipPartners(ties.ties, f.country), 10, f.country) : lensNone('the direct-investment positions are not in this build'),
        content: (
          <div className="space-y-4">
            <InvestmentCellNote pick={pick} ties={ties?.ties ?? null} />
            <InvestmentCards
              iso={f.country}
              name={name(f.country)}
              ties={ties?.ties ?? null}
              error={ties?.error ?? null}
              sector={pick?.cell ? { tech: pick.cell.tech, stage: pick.cell.stage } : null}
              nameOf={name}
              headingLevel={3}
              projectsHint="shown below when the selection's OSI cell is one of those"
            />
          </div>
        ),
      }
    : {
        names: lensNone('direct-investment positions are kept economy by economy: choose an economy'),
        empty: 'Choose an economy to see its direct-investment positions (IMF) and factory projects.',
      };
  // OSI
  const osi: LensPanelSpec = { names: topNames(t, 'fs', f.country), content: <TopPartnersCard side="fs" f={f} t={t} /> };
  // AIB
  let aib: LensPanelSpec;
  if (!sc.ready) aib = pendingSpec(sc.load);
  else {
    const nodeCols: Col<LensNode>[] = [
      { id: 'node', header: 'Node', value: (n) => n.node.label },
      { id: 'stage', header: `${tx.Many} and stage`, value: (n) => nodeWhere(n.node) },
      { id: 'cls', header: 'Treatment', value: (n) => n.cls, cell: (n) => (n.cls ? <ClassChip cls={n.cls} /> : '–') },
      { id: 'residual', header: 'Residual gap after trade', value: (n) => n.residual, cell: (n) => fmtShare(n.residual), num: true },
      { id: 'gain', header: 'Resilience gain of the best host', value: (n) => n.bestGain, cell: (n) => fmtPp(n.bestGain), num: true },
      { id: 'hosts', header: 'Eligible hosts, best first', value: (n) => n.hosts.map((h) => name(h)).join(', '), minWidth: 220 },
      { id: 'compact', header: 'Compact', value: (n) => n.node.compact, cell: (n) => (n.node.compact ? <CompactLinks ids={[n.node.compact]} /> : 'none') },
    ];
    const hostCols: Col<HostRow>[] = [
      { id: 'name', header: 'Host', value: (r) => name(r.iso3), cell: (r) => <Link className="link" to={toCountry(r.iso3)}>{name(r.iso3)}</Link> },
      { id: 'first', header: 'Sub-sectors where it is the best host', value: (r) => r.first.length, num: true },
      { id: 'eligible', header: 'Sub-sectors where it is eligible', value: (r) => r.eligible.length, num: true },
      { id: 'nodes', header: 'Sub-sectors', value: (r) => r.eligible.map(label).join(', '), minWidth: 240 },
    ];
    const flowCols: Col<PairInvestment>[] = [
      { id: 'investor', header: 'Investor', value: (p) => name(p.producer) },
      { id: 'host', header: 'Host', value: (p) => name(p.consumer) },
      { id: 'usd', header: 'Supply from new capacity', value: (p) => p.value, cell: (p) => fmtUsd(p.value), num: true },
      { id: 'fs', header: 'Friendshore score', value: (p) => p.friendshore, cell: (p) => fmtScore(p.friendshore), num: true },
      { id: 'nodes', header: 'Sub-sectors', value: (p) => p.nodes.map(label).join(', ') },
    ];
    const csv = () => ({
      name: `Allied Industrial Base investment hosts ${sc.label} ${scope}`,
      text: csvScenario(
        nodes.map((n) => ({ node_id: n.node.id, sub_sector: n.node.label, ...osiCsv(tx, n.node), classification: n.cls, residual_gap: n.residual, best_host_resilience_gain: n.bestGain, eligible_hosts_best_first: n.hosts.join('; '), compact: n.node.compact })),
        doc.meta,
        sc.id,
      ),
    });
    const flowPartners = f.country ? partnersInPairs(best.map((p) => ({ producer: p.investor, consumer: p.host })), f.country) : [];
    aib = {
      names: f.country ? lensNames(flowPartners.length ? flowPartners : hostsNamed(nodes), 10, f.country) : lensNames(best.length ? sideTotals(best.map((p) => ({ producer: p.investor, consumer: p.host, value: p.usd })), 'consumer') : hostsNamed(nodes)),
      content: (
        <div className="space-y-6">
          {sc.id === 'managed' && <ManagedNote />}
          <ChartCard
            headingLevel={3}
            title="Where new capacity buys most resilience"
            sub={`Where a gap remains after coordinated trade, the model adds an increment of capacity at each eligible host in turn (a fixed share of node demand, capped at the host’s current output in the node) and ranks the hosts by the resilience it buys. ${scope.charAt(0).toUpperCase()}${scope.slice(1)}, ${sc.label}: ${plural(nodes.length, 'sub-sector')}, largest gain first. A host must already show the capability (revealed by its exports).`}
            table={() => <TableView rows={nodes} cols={nodeCols} caption={`Investment hosts by sub-sector, ${scope}`} search pageSize={20} initialSort={{ id: 'gain', desc: true }} />}
            csv={csv}
            notes={<span>The increment is a synthetic simulation, not a project or a cost estimate.</span>}
          >
            {nodes.length ? (
              <RankList
                rows={nodes.slice(0, 8)}
                label="Sub-sectors where new capacity buys most resilience"
                render={(n) => (
                  <>
                    <span className="w-14 shrink-0 text-right font-medium tabular text-paper">{fmtPp(n.bestGain, 0)}</span>
                    <span className="min-w-0 flex-1 text-paper-dim">
                      <span className="text-paper">{n.node.label}</span> ({nodeWhere(n.node)}): best hosts {listWords(n.hosts.slice(0, 3).map((h) => name(h)))}
                    </span>
                    {n.cls && <ClassChip cls={n.cls} />}
                  </>
                )}
              />
            ) : (
              <EmptyState>No simulated investment for {scope} under {sc.label}: only sub-sectors whose gap after coordinated trade passes the investment threshold are simulated.</EmptyState>
            )}
          </ChartCard>
          <ChartCard
            headingLevel={3}
            title="Investment hosts"
            sub={`Host capability, ${scope}, ${sc.label}: how often each economy is the best-ranked or an eligible host for new capacity.`}
            table={() => <TableView rows={hosts} cols={hostCols} caption={`Investment hosts, ${scope}`} search pageSize={20} />}
          >
            {hosts.length ? (
              <RankList
                rows={hosts.slice(0, 8)}
                label="Most frequent investment hosts"
                render={(r) => (
                  <>
                    <Link className="link min-w-[9rem]" to={toCountry(r.iso3)}>
                      {name(r.iso3)}
                    </Link>
                    <span className="text-paper-dim">
                      best host in {plural(r.first.length, 'sub-sector')}, eligible in {r.eligible.length}
                    </span>
                  </>
                )}
              />
            ) : (
              <EmptyState>No eligible host for {scope} under {sc.label}.</EmptyState>
            )}
          </ChartCard>
          {flows ? (
            <InvestmentFlowsCard
              doc={{ nodes: doc.nodes, investmentFlows: flows, meta: doc.meta }}
              meta={doc.meta}
              shockLabel={scenarioShockLabel(doc.meta, sc.id)}
              filter={f}
              scope={scope}
              name={name}
              nodeLabel={label}
              headingLevel={3}
              csvName={`Allied Industrial Base ${sc.label}`}
            />
          ) : (
            compactPairs.length > 0 && (
              <ChartCard
                headingLevel={3}
                title="Investment relationships"
                sub={`Investor to host, ${scope}, ${sc.label}: supply from the simulated new capacity (USD a year).`}
                table={() => <TableView rows={compactPairs} cols={flowCols} caption={`Investment relationships, ${scope}`} search pageSize={20} initialSort={{ id: 'usd', desc: true }} />}
              >
                <SankeyChart
                  flows={compactPairs.slice(0, TOP)}
                  label={(code) => name(code)}
                  ariaLabel={`Investment relationships, ${scope}, investor to host`}
                  valueFormat={fmtUsd}
                  sideLabels={{ left: 'INVESTORS', right: 'HOSTS' }}
                  tipNote="supply from the simulated new capacity a year"
                />
              </ChartCard>
            )
          )}
        </div>
      ),
    };
  }
  return <Lenses kind="investment" f={f} partner={partner} onKind={onKind} name={name} lenses={{ today, osi, aib }} scenario={sc.label} />;
}

// ---- Development -----------------------------------------------------------------------------------------

function DevelopmentTab({ f, partner, onKind }: TabProps) {
  const { doc, tx } = useAibCtx();
  const sc = useLensScenario();
  const ties = useTies();
  const name = useNamer(ties?.ties, { scenario: sc.load.doc });
  const scope = useScope(f);
  const toCountry = useCountryHref();
  const [odaNames, setOdaNames] = useState<LensNames>(LENS_LOADING);
  const [dpNames, setDpNames] = useState<LensNames>(LENS_LOADING);
  const onOda = useCallback((n: LensNames) => setOdaNames(n), []);
  const onDp = useCallback((n: LensNames) => setDpNames(n), []);
  const flows: AibInvestmentFlows | null = sc.id === 'decoupling' ? (doc.investmentFlows ?? null) : (sc.load.doc?.investmentFlows ?? null);
  const rows = useMemo(() => lensNodes(doc.nodes, sc.id, flows), [doc, sc.id, flows]);
  const gaps = useMemo(() => lensBuildGaps(rows, f), [rows, f]);
  const buildIds = useMemo(() => new Set(lensBuildGaps(rows, { ...f, country: null }).map((g) => g.node.id)), [rows, f]);
  const buildFlows = useMemo(() => (flows ? investmentPairs({ nodes: doc.nodes, investmentFlows: flows }, f).filter((p) => buildIds.has(p.node)) : []), [doc, flows, f, buildIds]);
  // the aid of the Sector data is linked to six-stage cells: read at the filter's stage, else the node's own
  const odaStage = useMemo(() => aidStageOf(doc.nodes, f), [doc, f]);

  const today: LensPanelSpec = {
    names: odaNames,
    content: (
      <Suspense fallback={<Loading label="Loading the aid flows" />}>
        <OdaToday iso={f.country} focusName={f.country ? name(f.country) : undefined} stage={odaStage} href={toCountry} onNames={onOda} level={3} />
      </Suspense>
    ),
  };
  const osi: LensPanelSpec = {
    names: dpNames,
    content: (
      <EngineScope hostKeys={PARTNERSHIP_KEYS} fallback={<Loading label="Loading the Development Potential scores" />}>
        <DevelopmentPartners f={f} scope={scope} onNames={onDp} />
      </EngineScope>
    ),
  };
  let aib: LensPanelSpec;
  if (!sc.ready) aib = pendingSpec(sc.load);
  else {
    const cols: Col<LensNode>[] = [
      { id: 'node', header: 'Node', value: (n) => n.node.label },
      { id: 'stage', header: `${tx.Many} and stage`, value: (n) => nodeWhere(n.node) },
      { id: 'residual', header: 'Residual gap after trade', value: (n) => n.residual, cell: (n) => fmtShare(n.residual), num: true },
      { id: 'lost', header: sc.id === 'managed' ? 'Supply to shift' : 'Lost supply', value: (n) => n.lostUsd, cell: (n) => fmtUsd(n.lostUsd), num: true },
      { id: 'hosts', header: 'Eligible hosts, best first', value: (n) => n.hosts.map((h) => name(h)).join(', '), minWidth: 220 },
      { id: 'compact', header: 'Compact', value: (n) => n.node.compact, cell: (n) => (n.node.compact ? <CompactLinks ids={[n.node.compact]} /> : 'none') },
    ];
    const csv = () => ({
      name: `Allied Industrial Base capacity gaps ${sc.label} ${scope}`,
      text: csvScenario(
        gaps.map((n) => ({ node_id: n.node.id, sub_sector: n.node.label, ...osiCsv(tx, n.node), residual_gap: n.residual, lost_usd: n.lostUsd, eligible_hosts_best_first: n.hosts.join('; '), compact: n.node.compact })),
        doc.meta,
        sc.id,
      ),
    });
    const partners = f.country
      ? [...partnersInPairs(buildFlows.map((p) => ({ producer: p.investor, consumer: p.host })), f.country), ...gaps.flatMap((g) => g.hosts)]
      : buildFlows.length
        ? sideTotals(buildFlows.map((p) => ({ producer: p.investor, consumer: p.host, value: p.usd })), 'consumer')
        : hostsNamed(gaps);
    aib = {
      names: lensNames(partners, 10, f.country),
      content: (
        <div className="space-y-6">
          {sc.id === 'managed' && <ManagedNote />}
          <ChartCard
            headingLevel={3}
            title="Capacity gaps"
            sub={`The BUILD sub-sectors, ${scope}, under ${sc.label}: even coordinated trade leaves a large share of demand unserved, so new capacity is needed. Largest residual gap first, with the hosts able to provide it.`}
            table={() => <TableView rows={gaps} cols={cols} caption={`Capacity gaps, ${scope}`} search pageSize={20} initialSort={{ id: 'residual', desc: true }} />}
            csv={csv}
          >
            {gaps.length ? (
              <RankList
                rows={gaps.slice(0, 8)}
                label="Largest capacity gaps"
                render={(n) => (
                  <>
                    <span className="w-14 shrink-0 text-right font-medium tabular text-paper">{fmtShare(n.residual, 0)}</span>
                    <span className="min-w-0 flex-1 text-paper-dim">
                      <span className="text-paper">{n.node.label}</span> ({nodeWhere(n.node)}, {fmtUsd(n.lostUsd)} {sc.id === 'managed' ? 'to shift' : 'lost'})
                      {n.hosts.length ? `: hosts ${listWords(n.hosts.slice(0, 3).map((h) => name(h)))}` : ''}
                    </span>
                  </>
                )}
              />
            ) : (
              <EmptyState>No BUILD sub-sector for {scope} under {sc.label}.</EmptyState>
            )}
          </ChartCard>
        </div>
      ),
    };
  }
  return <Lenses kind="development" f={f} partner={partner} onKind={onKind} name={name} lenses={{ today, osi, aib }} scenario={sc.label} />;
}

function DevelopmentPartners({ f, scope, onNames }: { f: PartnershipFilter; scope: string; onNames: (n: LensNames) => void }) {
  const app = useApp();
  const { doc, nodeOf } = useAibCtx();
  const ties = useTies();
  const name = useNamer(ties?.ties);
  const toCountry = useCountryHref();
  const label = (id: string) => nodeOf(id)?.label ?? id;
  // the cells with Development Potential are the engine's own; the note and the rows read the same pick
  const scored = useMemo(() => scoredCellsOf(app.cells), [app.cells]);
  const pick = useCellPick(f, scored);
  // the OSI cells come from the matching nodes' crosswalk; none: no rows and a notice
  const rows = useMemo(() => (ties?.ties ? dpRows(ties.ties.dp, app.cells, doc.nodes, f) : []), [ties, app.cells, doc.nodes, f]);
  const noCell = !!pick && !pick.cells.length;
  const names: LensNames = !ties ? LENS_LOADING : ties.error ? lensNone('the Development Potential scores are not in this build') : noCell ? lensNone('no OSI cell with partner indices for this selection') : lensNames(rows.map((r) => r.iso3));
  const sig = `${names.status}|${names.keys.join(',')}`;
  useEffect(() => {
    onNames(names);
    // the signature carries the value
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [sig, onNames]);
  const cols: Col<DpRow>[] = [
    { id: 'name', header: 'Economy', value: (r) => name(r.iso3), cell: (r) => <Link className="link" to={toCountry(r.iso3)}>{name(r.iso3)}</Link> },
    { id: 'value', header: 'Development Potential', value: (r) => r.value, cell: (r) => fmtScore(r.value), num: true },
    { id: 'sector', header: 'Highest in OSI cell', value: (r) => cellLabel(r.tech, r.stage) },
    { id: 'host', header: 'Capacity gaps where it is an eligible host', value: (r) => r.hostIn.map(label).join(', '), minWidth: 220 },
  ];
  return (
    <ChartCard
      headingLevel={3}
      title="Development partners"
      sub={`The economies with the most Development Potential (where partnering builds capability in a middle- or lower-income economy), ${scope}; each with the capacity gaps where it is already an eligible host under Decoupling. A preference-weighted ranking at the NEIS defaults.`}
      table={() => <TableView rows={rows} cols={cols} caption={`Development partners, ${scope}`} search pageSize={20} />}
    >
      <div className="mb-3">
        <OsiCellNote pick={pick} cells fallback="No Development Potential ranking is shown for it." />
      </div>
      {!ties ? (
        <Loading label="Loading the Development Potential scores" />
      ) : ties.error ? (
        <Notice kind="info">{ties.error}</Notice>
      ) : noCell ? null : rows.length ? (
        <RankList
          rows={rows.slice(0, 10)}
          label="Economies with the most Development Potential"
          render={(r) => (
            <>
              <span className="w-12 shrink-0 text-right font-medium tabular text-paper">{fmtScore(r.value)}</span>
              <Link className="link" to={toCountry(r.iso3)}>
                {name(r.iso3)}
              </Link>
              <span className="text-paper-mute">
                {cellLabel(r.tech, r.stage)}
                {r.hostIn.length ? `; eligible host for ${listWords(r.hostIn.slice(0, 2).map(label))}` : ''}
              </span>
            </>
          )}
        />
      ) : (
        <EmptyState>No Development Potential scores for {scope}.</EmptyState>
      )}
    </ChartCard>
  );
}

// ---- Geopolitical ----------------------------------------------------------------------------------------

function GeopoliticalTab({ f, partner, onKind }: TabProps) {
  const { doc, nodeOf } = useAibCtx();
  const sc = useLensScenario();
  const ties = useTies();
  const man = useCountryManifest();
  const roster = man.data?.countries;
  const name = useNamer(ties?.ties, { scenario: sc.load.doc, roster });
  const scope = useScope(f);
  const toCountry = useCountryHref();
  const label = (id: string) => nodeOf(id)?.label ?? id;
  const iso = f.country;
  const coalition = useMemo(() => (roster ? coalitionOf(roster) : []), [roster]);
  const tierOf = (x: string) => roster?.find((c) => c.iso3 === x)?.tier ?? null;
  const fsTop = useMemo(() => (ties?.ties && iso ? topPairs(ties.ties.top_all, 'fs', iso, 8) : []), [ties, iso]);

  // Today: agreements and UN votes
  let today: LensPanelSpec;
  if (!ties) today = { names: LENS_LOADING, content: <Loading label="Loading the agreements and UN votes" /> };
  else if (!ties.ties) today = { names: lensNone('the agreements and UN votes are not in this build'), empty: ties.error ?? 'This data build has no agreements or UN votes.' };
  else if (iso) {
    const tiesNow = agreementTies(ties.ties, iso);
    const counts = alignmentCounts(ties.ties, iso);
    const tier = tierOf(iso);
    today = {
      names: lensNames(tiesNow.map((r) => r.iso3), 40, iso),
      content: (
        <LensCard id="geo-today" title={`${name(iso)}: agreements and UN votes`} sub={`${name(iso)} is ${roster ? tierWords(tier) : 'in the roster'} today. In the UN General Assembly it votes in the same bloc as ${plural(counts.same, 'economy', 'economies')}, and in the opposite bloc from ${counts.opposite}.`}>
          {tiesNow.length ? (
            <RankedNames
              rows={tiesNow}
              max={12}
              label={`Economies that share an economic-security agreement with ${name(iso)}`}
              keyOf={(r) => r.iso3}
              nameOf={(r) => name(r.iso3)}
              href={(r) => toCountry(r.iso3)}
              value={(r) => plural(r.agreements.length, 'agreement')}
              note={(r) => `${agreementNames(r.agreements)}; ${alignmentWords(r.alignment)}`}
            />
          ) : (
            <p className="text-paper-mute">{name(iso)} shares no economic-security agreement in the data.</p>
          )}
          {tiesNow.length > 12 && <p className="text-xs text-paper-mute">And {tiesNow.length - 12} more.</p>}
        </LensCard>
      ),
    };
  } else {
    const list = agreementList(ties.ties);
    today = {
      names: lensNames(agreementMembers(ties.ties).map((r) => r.iso3), 40),
      content: (
        <LensCard id="geo-today" title="Economic-security agreements today" sub="The economic-security agreements in the data and their members. UN-vote alignment is between two economies: choose an economy to see it.">
          {list.length ? (
            <RankedNames
              rows={list}
              label="Economic-security agreements"
              keyOf={(r) => r.agreement}
              nameOf={(r) => agreementNames([r.agreement])}
              value={(r) => plural(r.members.length, 'member')}
              note={(r) => `${r.members.slice(0, 8).map(name).join(', ')}${r.members.length > 8 ? ` and ${r.members.length - 8} more` : ''}`}
            />
          ) : (
            <p className="text-paper-mute">No agreement in the data.</p>
          )}
        </LensCard>
      ),
    };
  }

  // OSI: the allied coalition and Friendshore's alignment part
  let osi: LensPanelSpec;
  if (man.status === 'loading') osi = { names: LENS_LOADING, content: <Loading label="Loading the coalition tiers" /> };
  else if (!roster) osi = { names: lensNone('the coalition tiers could not be loaded'), empty: 'The coalition tiers could not be loaded (the Countries data is not available).' };
  else {
    const core = coalition.filter((c) => c.tier === 'core');
    const partner = coalition.filter((c) => c.tier === 'partner');
    const inside = !iso || coalition.some((c) => c.iso3 === iso);
    osi = {
      names: inside ? lensNames(coalition.map((c) => c.iso3), 200, iso) : lensNone(`${name(iso as string)} is outside the OSI's allied coalition`),
      content: (
        <div className="space-y-4">
          <LensCard
            id="geo-osi"
            title="The allied coalition"
            sub={`${plural(core.length, 'core member')} and ${plural(partner.length, 'partner')}: the club the indices treat as allies, reached through UN-vote alignment, an economic-security agreement or a supplier route.${iso ? ` ${name(iso)} is ${tierWords(tierOf(iso))}.` : ''}`}
          >
            <p>
              <span className="text-paper">Core members:</span> {core.map((c) => c.name).join(', ') || 'none'}.
            </p>
            <p>
              <span className="text-paper">Partners:</span> {partner.map((c) => c.name).join(', ') || 'none'}.
            </p>
          </LensCard>
          {iso && (
            <LensCard id="geo-osi-fs" title={`Friendshore partners of ${name(iso)} and how they vote`} sub="Friendshore weighs UN-vote alignment as one of its parts; its strongest partners, with their tier and alignment.">
              {!ties ? (
                <Loading label="Loading the partner rankings" />
              ) : fsTop.length && ties.ties ? (
                <RankedNames
                  rows={fsTop}
                  label={`Friendshore partners of ${name(iso)}`}
                  keyOf={(r) => r.partner_iso}
                  nameOf={(r) => name(r.partner_iso)}
                  href={(r) => toCountry(r.partner_iso)}
                  value={(r) => fmtScore(r.value)}
                  note={(r) => `${tierWords(tierOf(r.partner_iso))}; ${alignmentWords(alignmentOf(ties.ties as Ties, iso, r.partner_iso))}`}
                />
              ) : (
                <p className="text-paper-mute">No Friendshore ranking for {name(iso)} in this build.</p>
              )}
            </LensCard>
          )}
        </div>
      ),
    };
  }

  // AIB: the compact coalitions under the scenario
  let aib: LensPanelSpec;
  const compacts = scenarioCompacts(doc, sc.id, sc.load.doc);
  if (!sc.ready || !compacts) aib = pendingSpec(sc.load);
  else {
    const co = lensCoalitions(compacts, doc.nodes, f);
    aib = {
      names: lensNames(co.members.map((m) => m.iso3), 60, iso),
      content: <AibCoalitions co={co} f={f} name={name} label={label} scenario={sc.label} scope={scope} managed={sc.id === 'managed'} />,
    };
  }
  return <Lenses kind="geopolitical" f={f} partner={partner} onKind={onKind} name={name} lenses={{ today, osi, aib }} scenario={sc.label} />;
}

function AibCoalitions({
  co, f, name, label, scenario, scope, managed,
}: {
  co: ReturnType<typeof lensCoalitions>;
  f: PartnershipFilter;
  name: (iso: string) => string;
  label: (id: string) => string;
  scenario: string;
  scope: string;
  managed: boolean;
}) {
  const { title, compactOf } = useAibCtx();
  const toCountry = useCountryHref();
  const cols: Col<ScenarioCompactRel>[] = [
    { id: 'id', header: 'Compact', value: (c) => c.id, cell: (c) => <CompactLinks ids={[c.id]} /> },
    { id: 'title', header: 'Sub-sectors', value: (c) => c.nodes.map(label).join(', '), minWidth: 200 },
    { id: 'members', header: 'Coalition', value: (c) => (c.members ? c.members.map(name).join(', ') : (c.reason ?? 'none')), minWidth: 220 },
    { id: 'critical', header: 'Critical supply partners', value: (c) => c.criticalPartners.map(name).join(', ') },
    { id: 'external', header: 'Critical dependencies outside', value: (c) => c.criticalExternal.map(name).join(', ') },
  ];
  const iso = f.country;
  const lead = iso
    ? co.seats.length
      ? `${name(iso)} sits in the coalition of ${plural(co.seats.length, 'compact')} under ${scenario}${co.dependency.length ? `, and is a critical dependency outside ${co.dependency.length} more` : ''}. Its co-members, most compacts first:`
      : co.dependency.length
        ? `${name(iso)} sits in no coalition under ${scenario}, but ${plural(co.dependency.length, 'compact')} ${co.dependency.length === 1 ? 'depends' : 'depend'} on it from outside: ${co.dependency.join(', ')}.`
        : `${name(iso)} sits in no compact coalition under ${scenario}${co.compacts.length ? '; it holds other roles in the compacts below' : ''}.`
    : `${plural(co.compacts.length, 'compact')} in scope under ${scenario}; ${plural(co.members.length, 'economy', 'economies')} sit in at least one coalition, most compacts first:`;
  return (
    <div className="space-y-6">
      {managed && <ManagedNote />}
      <ChartCard
        headingLevel={3}
        title="Compact coalitions"
        sub={`The economies the model says must coordinate in each compact (the minimum coalition), the members no other candidate can replace (critical supply partners) and the economies outside on which a compact still depends, ${scope}.`}
        table={() => <TableView rows={co.compacts} cols={cols} caption={`Compact coalitions, ${scope}, ${scenario}`} search pageSize={20} />}
      >
        <div className="space-y-3 text-sm text-paper-dim">
          <p>{lead}</p>
          {co.members.length > 0 && (
            <RankedNames
              rows={co.members}
              max={12}
              label={iso ? `Co-members of ${name(iso)}` : 'Coalition members across the compacts'}
              keyOf={(m) => m.iso3}
              nameOf={(m) => name(m.iso3)}
              href={(m) => toCountry(m.iso3)}
              value={(m) => plural(m.n, 'compact')}
            />
          )}
          {co.criticalPartners.length > 0 && (
            <p>
              <span className="text-paper">Critical supply partners:</span> {co.criticalPartners.map(name).join(', ')}.
            </p>
          )}
          {co.criticalExternal.length > 0 && (
            <p>
              <span className="text-paper">Critical dependencies outside the coalitions:</span> {co.criticalExternal.map(name).join(', ')}.
            </p>
          )}
          {!co.compacts.length && <EmptyState>No compact covers {scope}.</EmptyState>}
          {co.compacts
            .filter((c) => !c.members)
            .slice(0, 3)
            .map((c) => {
              const cp = compactOf(c.id);
              return (
                <p key={c.id} className="text-xs text-paper-mute">
                  {c.id}
                  {cp ? ` (${title(cp)})` : ''}: {c.reason}
                </p>
              );
            })}
        </div>
      </ChartCard>
    </div>
  );
}
