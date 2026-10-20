// The Sectoral Compacts landing /compacts (IA refactor, 2026-10-16; brief s.16: progressive disclosure;
// credibility pass 2026-10-16): what a compact is and the run's settings (demand scope and candidate
// partners, shock semantics, supply constraint, strategic scope), then the strategic map of every node
// (shock exposure x coordination dividend, area = lost supply, colour = class; filterable by technology,
// URL `tech`, and by default only the nodes of the strategic scope, URL `scope=all` includes the others),
// then one slim card per compact (name, why it exists, main nodes, key countries, the coalition with its
// associates and critical external dependencies, the marginal ones apart, dominant policy need, headline
// resilience effect with the mini coverage strip, physical-capability warnings, "View compact"), and the
// full classification of every node behind "Explore all nodes" (open on #nodes or a technology filter),
// grouped by class with each node's policy needs where they say more than the class, its warning and its
// growth-bound flag; a node outside every compact opens its class under other assumptions and growth
// bounds in place, and its place in the diversification scenario (2026-10-19). Table twins and CSVs. A dictionary bundle (2026-10-19) calls the node techs sectors,
// groups the sector filter by EIS index and names each node's and compact's index; titles come from the
// compacts' structured fields (compactTitle), never from parsing their names.
//
// The scenario toggle (2026-10-19, page work A): the run line names the scenario shown; the strategic map
// places the nodes under it (Managed Interdependence: the share shifted, its dividend, its class; the nodes
// it leaves unchanged are not placed); the compact cards are ordered by the supply the scenario removes or
// shifts, say why each compact matters and what it restores under it (its coalition from the scenario's
// file once loaded) and carry a three-way mini bar (Baseline, Managed Interdependence, Decoupling: kept /
// restored by trade / new capacity, the toggled one current) in place of the coverage strip, with a table
// twin of every compact under the three scenarios and its CSV; the node classification groups the nodes by
// their class under the scenario (Managed Interdependence: the unchanged nodes apart), and its table and
// CSV carry the scenario's figures. A bundle without the scenarios shows Decoupling alone, as before.

import { useEffect, useMemo } from 'react';
import { Link, useLocation, useNavigate, useSearchParams } from 'react-router';
import { ChartCard } from '../../charts/ChartCard.tsx';
import { TableView, type Col } from '../../components/TableView.tsx';
import { Checkbox, Disclosure, EmptyState, FilterRow, PageHead, Select } from '../../components/ui.tsx';
import { useTitle } from '../../shell/title.ts';
import { EYEBROW, PAGE, useAibCtx } from '../AibLayout.tsx';
import { AIB_PHYSICAL_WARNING, coalitionMembersOf, diversifyShockIdOf, inStrategicScope, type AibCompact, type AibManagedScenarioDoc, type AibNode } from '../contract.ts';
import { ClassLegend, ClassSwatch, FlaggedClass, PhysicalWarning, PolicyNeeds } from '../charts/ClassChip.tsx';
import { CoverageStrip } from '../charts/CoverageStrip.tsx';
import { LostByClassBar } from '../charts/LostByClassBar.tsx';
import { CoverageLegend, ScenarioTag } from '../charts/ScenarioCompare.tsx';
import { ScenarioMiniBars } from '../charts/ScenarioMini.tsx';
import { mapPoints, mapPointsFor, StrategicMap } from '../charts/StrategicMap.tsx';
import { TypeTag } from '../charts/TypeTag.tsx';
import { useAibScenarioDoc } from '../data/hooks.ts';
import { classInfo, policyNeedsText } from '../derive/classes.ts';
import { compactVsFragmented, coverageSteps } from '../derive/coverage.ts';
import { fmtPp, fmtShare, fmtUsd, listWords, plural } from '../derive/format.ts';
import { compactNodes, nodeCsvRow, nodeRow, outOfScopeCount, scopedNodes, technologiesOf } from '../derive/nodes.ts';
import {
  compactScenarioCsvRow, compactScenarioRows, compactsByScenario, compactUnchangedReasonOf, managedCompact, managedOf, nodeScenarioCsv, nodeScenarioNote, scenarioClassGroups, type CompactScenarioRow,
} from '../derive/scenarioPages.ts';
import { compactDetail, compactScenario, compactScenarios, compactSuppliersToday, nodeScenario, scenarioCsvFields, scenarioInfo, scenarioShockLabel, type CompactScenario } from '../derive/scenarios.ts';
import { growthFlag, robustnessSummary, variantWarning } from '../derive/sensitivity.ts';
import { candidateUniverse, coverageBase, demandScope, nodeTaxonomy, shockSemantics, strategicScope, supplyConstraint } from '../derive/settings.ts';
import { groupedSectorWords, indexLabel, indexOfNode, nodeWhere, techOptions } from '../derive/taxonomy.ts';
import { tierEconomies } from '../derive/tiers.ts';
import { compactTypeInfo } from '../derive/types.ts';
import { compactHref } from '../nav.ts';
import type { AibToggle } from '../scenarioUrl.ts';
import { BasisTag } from './basis.tsx';
import { csvWithRun, ReadingNotes, Section } from './common.tsx';
import { NodeRobustness } from './NodeRobustness.tsx';

/** The coalition line of a card: members, associates and critical external dependencies of a compact as solved under a scenario. */
function coalitionLine(c: AibCompact, names: Readonly<Record<string, string>>): string {
  const tiers = tierEconomies(c, names);
  const associates = tiers.filter((e) => e.tier === 'associate partner').length;
  const critExternal = tiers.filter((e) => e.tier === 'critical external dependency').length;
  const marginal = tiers.filter((e) => e.tier === 'critical external dependency (marginal)').length;
  return `${plural(c.nodes.length, 'node')} · ${plural(coalitionMembersOf(c).length, 'member')}${associates ? ` + ${plural(associates, 'associate')}` : ''}${
    critExternal || marginal ? ` · ${plural(critExternal, 'critical external dependency', 'critical external dependencies')}` : ''
  }${marginal ? ` (+${marginal} marginal)` : ''}`;
}

function CompactCard({ c, managed, managedState }: { c: AibCompact; managed: AibManagedScenarioDoc | null; managedState: 'ready' | 'loading' | 'error' | 'missing' }) {
  const { name, names, nodeOf, tx, title: titleOf, scenario, hasScenarios, href, doc } = useAibCtx();
  const title = titleOf(c);
  const steps = coverageSteps(c.metrics);
  const warned = c.physicalWarnings ?? [];
  const mi = scenario === 'managed';
  const figs = useMemo(() => compactScenarios(c, nodeOf), [c, nodeOf]);
  const shown = figs.find((f) => f.id === scenario) as CompactScenario;
  const solved = mi ? managedCompact(c, managed) : c;
  const detail = mi && managed ? compactDetail(c, 'managed', managed) : null;
  // the type, policy need and key countries of the compact as the toggled scenario solves it (Managed
  // Interdependence: its own; without a coalition there, or while its file loads, no type and today's suppliers)
  const type = solved ? compactTypeInfo(solved.type, compactVsFragmented(solved.metrics)?.gain) : null;
  const pending = managedState === 'loading' ? 'loading…' : managedState === 'ready' ? 'none: no coalition under this scenario' : '–';
  const nodes = compactNodes(c, nodeOf);
  // why Managed Interdependence leaves the compact unchanged (the file's reason once loaded; the same words before)
  const unchangedWhy = detail?.reason ?? compactUnchangedReasonOf(nodes);
  const main = nodes
    .map((n) => nodeScenario(n, scenario))
    .sort((a, b) => (b.lostUsd ?? -1) - (a.lostUsd ?? -1) || a.node.label.localeCompare(b.node.label, 'en'))
    .map((r) => r.node);
  const anchors = solved ? solved.anchors.slice(0, 4).map((x) => name(x)) : [];
  const suppliers = solved ? solved.suppliers.filter((x) => !solved.anchors.includes(x)).slice(0, 3).map((x) => name(x)) : [];
  const today = solved ? [] : compactSuppliersToday(c).slice(0, 3).map((t) => name(t.iso3));
  const before = mi ? shown.existing : c.metrics.existing;
  const after = mi ? (solved ? (solved.metrics.investment ?? solved.metrics.trade) : shown.optimized) : (c.metrics.investment ?? c.metrics.trade);
  const why = mi
    ? shown.nodesRun > 0
      ? `Managed Interdependence shifts ${fmtShare(shown.exposure, 0)} of ${fmtUsd(shown.demandUsd)} demand away from capped suppliers (${shown.nodesRun} of ${plural(c.nodes.length, 'node')} ${shown.nodesRun === 1 ? 'has' : 'have'} a supplier above the cap).`
      : `Managed Interdependence leaves it unchanged: ${unchangedWhy}.`
    : `The shock removes ${fmtShare(c.metrics.exposure, 0)} of ${fmtUsd(c.metrics.demandUsd)} demand.`;
  const coalition = !mi
    ? coalitionLine(c, names)
    : solved
      ? coalitionLine(solved, names)
      : detail?.reason
        ? `No coalition: ${detail.reason}`
        : managedState === 'loading'
          ? 'Loading the coalition under this scenario…'
          : managedState === 'missing'
            ? 'Not in this export'
            : managedState === 'error'
              ? 'Could not be loaded'
              : '–';
  return (
    <li>
      <article className="card flex h-full flex-col p-5" aria-labelledby={`card-${c.id}`} data-compact={c.id}>
        <div className="flex flex-wrap items-center gap-2">
          <span className="eyebrow">{c.id}</span>
          {type ? <TypeTag info={type} /> : detail ? <span className="tag text-paper-mute">No coalition</span> : null}
        </div>
        <h3 id={`card-${c.id}`} className="mt-2 text-lg font-medium leading-snug text-paper">
          <Link className="hover:text-signal" to={href(compactHref(c.id))} title={titleOf(c, true)}>
            {title}
          </Link>
        </h3>
        {tx.dictionary && c.technologies.length > 0 && (
          <p className="mt-1 text-xs text-paper-mute" data-testid="compact-taxonomy">
            {tx.Many}: {groupedSectorWords(tx, c.technologies)}
          </p>
        )}
        <dl className="mt-3 grid grid-cols-[7.5rem_minmax(0,1fr)] gap-x-3 gap-y-1.5 text-sm">
          <dt className="text-paper-mute">Why it exists</dt>
          <dd className="text-paper-dim">
            {why}
            {type ? ` ${type.explanation}` : ''}
          </dd>
          <dt className="text-paper-mute">Main nodes</dt>
          <dd className="text-paper-dim">
            {listWords(main.slice(0, 3).map((n) => n.label))}
            {main.length > 3 ? ` (+${main.length - 3} more)` : ''}
          </dd>
          <dt className="text-paper-mute">Key countries</dt>
          <dd className="text-paper-dim">
            {anchors.length ? `Demand: ${anchors.join(', ')}` : ''}
            {anchors.length && suppliers.length ? '; ' : ''}
            {suppliers.length ? `supply: ${suppliers.join(', ')}` : ''}
            {today.length ? `Suppliers today: ${today.join(', ')}` : ''}
            {!anchors.length && !suppliers.length && !today.length ? '–' : ''}
          </dd>
          <dt className="text-paper-mute">Coalition</dt>
          <dd className="text-paper-dim">{coalition}</dd>
          <dt className="text-paper-mute">Policy need</dt>
          <dd className="text-paper-dim">{solved ? (solved.policyNeeds[0] ?? 'none listed') : pending}</dd>
          <dt className="text-paper-mute">Resilience effect</dt>
          <dd className="text-paper-dim">
            {mi && shown.nodesRun === 0
              ? 'Demand served in full, as today'
              : `Demand served ${fmtShare(before, 0)} with existing relationships, ${fmtShare(after, 0)} ${mi && !solved ? 'with optimized trade' : 'with the compact'}`}
          </dd>
        </dl>
        {warned.length > 0 && (
          <p className="mt-2 flex flex-wrap items-center gap-2 text-xs text-paper-dim">
            <PhysicalWarning warning={AIB_PHYSICAL_WARNING} />
            <span>{listWords(warned)}</span>
          </p>
        )}
        <div className="mt-4">
          {hasScenarios ? (
            <ScenarioMiniBars
              rows={figs.map((f) => ({ id: f.id, split: f.split, note: f.id === 'managed' && !f.ran ? `unchanged: ${unchangedWhy}` : null }))}
              highlight={scenario}
              label={`${title}: demand served under each scenario`}
              meta={doc.meta}
            />
          ) : (
            <CoverageStrip steps={steps} label={`${title}: demand served at each step`} />
          )}
        </div>
        <div className="mt-auto pt-4 text-sm">
          <Link className="link" to={href(compactHref(c.id))} aria-label={`View compact: ${title}`}>
            View compact &rarr;
          </Link>
        </div>
      </article>
    </li>
  );
}

function NodeChip({ n, scenario }: { n: AibNode; scenario: AibToggle }) {
  const { doc, names, tx, href, hasScenarios } = useAibCtx();
  const flag = growthFlag(n);
  const mi = scenario === 'managed';
  const r = mi ? nodeScenario(n, 'managed') : null;
  const miWarning = r?.ran && r.cls ? variantWarning(n, diversifyShockIdOf(doc.meta)) : null;
  return (
    <li className="max-w-full border hairline px-2.5 py-1.5 text-xs">
      <span className="block text-paper">{n.label}</span>
      <span className="block text-paper-mute">
        {nodeWhere(tx, n)}
        {n.compact && (
          <>
            {' · '}
            <Link className="link" to={href(compactHref(n.compact))}>
              {n.compact}
            </Link>
          </>
        )}
        {!inStrategicScope(n) && ' · outside the strategic scope'}
      </span>
      {mi ? (
        <span className="block text-paper-dim" data-testid="scenario-node-line">
          {r?.ran ? (
            <>
              {fmtShare(r.exposure, 0)} shifted ({fmtUsd(r.lostUsd)}); {fmtShare(r.residual, 0)} left for new capacity
              {miWarning ? (
                <span className="mt-1 block">
                  <FlaggedClass cls={r.cls} warning={miWarning} />
                </span>
              ) : null}
            </>
          ) : (
            (nodeScenarioNote(r!) ?? 'unchanged').replace(/^unchanged: /, 'Unchanged: ')
          )}
        </span>
      ) : (
        <>
          <span className="block">
            <PolicyNeeds cls={n.cls} needs={n.policyNeeds} />
          </span>
          {n.warning && (
            <span className="mt-1 block">
              <PhysicalWarning warning={n.warning} validation={n.physicalValidation} />
            </span>
          )}
          {flag && (
            <span className="mt-1 block max-w-xs border-l-2 border-l-signal pl-2 text-paper-dim" data-testid="growth-flag">
              {flag}
            </span>
          )}
        </>
      )}
      {!n.compact && (n.variants !== undefined || n.growth !== undefined || n.diversify !== undefined) && (
        <div className="mt-1">
          <Disclosure summary={robustnessSummary(n, doc.meta)}>
            <NodeRobustness n={n} meta={doc.meta} names={names} scenario={hasScenarios ? scenario : null} />
          </Disclosure>
        </div>
      )}
    </li>
  );
}

/** The run's settings in one paragraph (an older bundle: its single universe); the scenario shown first when the bundle has the toggle. */
function RunLine() {
  const { doc, shockLabel, scenario, hasScenarios, href } = useAibCtx();
  const m = doc.meta;
  const ds = demandScope(m);
  const cu = candidateUniverse(m);
  const sem = shockSemantics(m);
  const con = supplyConstraint(m);
  const st = strategicScope(m);
  const nt = nodeTaxonomy(m);
  const mi = scenario === 'managed';
  return (
    <p className="mt-3 max-w-3xl text-sm text-paper-mute" data-testid="run-settings">
      {hasScenarios ? (
        <>
          Shown: <span className="text-paper-dim">{scenarioInfo(m, scenario).label}</span> ({scenarioShockLabel(m, scenario)}); supply assumption: {m.headlineScenario}
        </>
      ) : (
        <>
          Headline: <span className="text-paper-dim">{shockLabel}</span>; supply assumption: {m.headlineScenario}
        </>
      )}
      {sem && !mi ? `, ${sem.label.toLowerCase()} semantics (${sem.mode === 'hold' ? 'the shocked economy keeps buying' : sem.words})` : ''}
      {con ? `, ${con.label.toLowerCase()} supply` : ''}; trade in {m.tradeYear}.{' '}
      {ds || cu ? (
        <>
          Demand scope: <span className="text-paper-dim">{ds ? ds.words : m.universe}</span>. Candidate partners:{' '}
          <span className="text-paper-dim">{cu ? cu.words : m.universe}</span>.
        </>
      ) : (
        <>Candidate universe: {m.universe}.</>
      )}{' '}
      {nt.dictionary ? `Nodes: ${nt.label.charAt(0).toLowerCase()}${nt.label.slice(1)}, index > sector > stage. ` : ''}
      {st && st.mode !== 'all' ? `Strategic scope: ${st.label.toLowerCase()} (${listWords(st.techs)}). ` : ''}
      Coverage is the share of {coverageBase(m)} served after {mi ? 'the cap' : 'the shock'}.{' '}
      <Link className="link" to={href('/compacts/about')}>
        How compacts are derived &rarr;
      </Link>
    </p>
  );
}

/** The table twin of the compact cards: every compact under the three scenarios. */
function compactCols(titleOf: (c: AibCompact) => string, href: (to: string) => string): Col<CompactScenarioRow>[] {
  const share = (s: CompactScenario) => s.split?.kept ?? null;
  return [
    {
      id: 'compact',
      header: 'Compact',
      value: (r) => `${r.compact.id} ${titleOf(r.compact)}`,
      cell: (r) => (
        <Link className="link" to={href(compactHref(r.compact.id))}>
          {r.compact.id}
        </Link>
      ),
    },
    { id: 'title', header: 'Supply chain', value: (r) => titleOf(r.compact), minWidth: 220 },
    { id: 'nodes', header: 'Nodes', value: (r) => r.compact.nodes.length, num: true },
    { id: 'demand', header: 'Allied demand', value: (r) => r.decoupling.demandUsd, cell: (r) => fmtUsd(r.decoupling.demandUsd), num: true },
    { id: 'dLost', header: 'Lost: Decoupling', value: (r) => r.decoupling.lostUsd, cell: (r) => fmtUsd(r.decoupling.lostUsd), num: true },
    { id: 'mLost', header: 'Shifted: Managed Interdependence', value: (r) => r.managed.lostUsd, cell: (r) => fmtUsd(r.managed.lostUsd), num: true },
    { id: 'mRun', header: 'Nodes with supply to shift: Managed Interdependence', value: (r) => r.managed.nodesRun, num: true },
    { id: 'dKept', header: 'Kept: Decoupling', value: (r) => share(r.decoupling), cell: (r) => fmtShare(share(r.decoupling), 0), num: true },
    { id: 'mKept', header: 'Kept: Managed Interdependence', value: (r) => share(r.managed), cell: (r) => fmtShare(share(r.managed), 0), num: true },
    { id: 'dServed', header: 'Served after trade: Decoupling', value: (r) => r.decoupling.optimized, cell: (r) => fmtShare(r.decoupling.optimized, 0), num: true },
    { id: 'mServed', header: 'Served after trade: Managed Interdependence', value: (r) => r.managed.optimized, cell: (r) => fmtShare(r.managed.optimized, 0), num: true },
    { id: 'dGap', header: 'Left for new capacity: Decoupling', value: (r) => r.decoupling.residualUsd, cell: (r) => fmtUsd(r.decoupling.residualUsd), num: true },
    { id: 'mGap', header: 'Left for new capacity: Managed Interdependence', value: (r) => r.managed.residualUsd, cell: (r) => fmtUsd(r.managed.residualUsd), num: true },
    {
      id: 'hhi',
      header: "Today's concentration (HHI) of supply to the allied market",
      value: (r) => r.baseline.hhi,
      cell: (r) => (r.baseline.hhi === null ? '–' : r.baseline.hhi.toFixed(2)),
      num: true,
      title:
        "Herfindahl-Hirschman Index of the compact's supply to the allied market today: 1 = one supplier. Managed Interdependence tests a different measure, the concentration of world exporters, against the benchmark.",
    },
  ];
}

export default function CompactsIndexView() {
  useTitle('Compacts', 'aib');
  const { doc, names, tx, nodeOf, scenario, hasScenarios, href, title: titleOf } = useAibCtx();
  const loc = useLocation();
  const navigate = useNavigate();
  const [params, setParams] = useSearchParams();
  const mi = scenario === 'managed';
  const managedLoad = useAibScenarioDoc(mi ? 'managed' : null);
  const managed = managedOf(managedLoad.doc);
  const managedState = managed ? 'ready' : managedLoad.loading ? 'loading' : managedLoad.missing ? 'missing' : managedLoad.error ? 'error' : 'loading';
  const nOut = useMemo(() => outOfScopeCount(doc.nodes), [doc]);
  const all = params.get('scope') === 'all';
  const pool = useMemo(() => scopedNodes(doc.nodes, all), [doc, all]);
  const techs = useMemo(() => technologiesOf(pool), [pool]);
  const raw = params.get('tech');
  const tech = raw && techs.includes(raw) ? raw : null;
  const groups = useMemo(() => scenarioClassGroups(pool, scenario, tech), [pool, scenario, tech]);
  const classed = groups.filter((g) => !g.unchanged);
  const shown = groups.flatMap((g) => g.nodes);
  const points = useMemo(() => (mi ? mapPointsFor(shown.map((n) => nodeScenario(n, 'managed'))) : mapPoints(shown)), [shown, mi]);
  const unplaced = mi ? shown.length - points.length : 0;
  const lostTotal = classed.reduce((s, g) => s + g.lostUsd, 0);
  const m = doc.meta;
  const sc = scenarioInfo(m, scenario);
  const lostWord = mi ? 'shifted supply' : 'lost supply';
  const ordered = useMemo(() => (hasScenarios ? compactsByScenario(doc.compacts, scenario, nodeOf) : doc.compacts), [doc, scenario, nodeOf, hasScenarios]);
  const compactRows = useMemo(() => compactScenarioRows(ordered, nodeOf), [ordered, nodeOf]);
  const openNodes = loc.hash === '#nodes' || tech !== null || all;
  const setParam = (k: string, v: string | null) => {
    const q = new URLSearchParams(params);
    if (v) q.set(k, v);
    else q.delete(k);
    setParams(q, { replace: true, preventScrollReset: true });
  };

  // follow a deep link's #anchor (#nodes, #map)
  useEffect(() => {
    if (loc.hash) document.getElementById(loc.hash.slice(1))?.scrollIntoView?.({ block: 'start' });
  }, [loc.hash]);

  const fig = (n: AibNode) => nodeScenario(n, scenario);
  // the table shows the toggled scenario's figures: their headers say so under Managed Interdependence
  const underMi = mi ? ' (Managed Interdependence)' : '';
  const miCls = (n: AibNode) => {
    const r = fig(n);
    return r.ran && r.cls ? classInfo(r.cls).label : 'Unchanged';
  };
  const cols: Col<AibNode>[] = [
    { id: 'node', header: 'Sub-sector', value: (n) => n.label },
    ...(tx.dictionary ? [{ id: 'index', header: 'Index', value: (n: AibNode) => { const ix = indexOfNode(tx, n); return ix ? indexLabel(ix) : null; } }] : []),
    { id: 'techs', header: tx.Many, value: (n) => n.techs.join(', ') },
    { id: 'stage', header: 'Stage', value: (n) => n.stage },
    mi ? { id: 'cls', header: 'Class under Managed Interdependence', value: miCls, minWidth: 160 } : { id: 'cls', header: 'Class', value: (n) => classInfo(n.cls).label },
    ...(mi ? [{ id: 'headlineCls', header: 'Class under Decoupling', value: (n: AibNode) => classInfo(n.cls).label }] : []),
    { id: 'needs', header: 'Policy needs', value: (n) => policyNeedsText(n.policyNeeds) },
    { id: 'warning', header: 'Warning', value: (n) => n.warning ?? 'none', minWidth: 200 },
    {
      id: 'growth',
      header: 'Class depends on the growth bound',
      value: (n) => growthFlag(n) ?? (typeof n.coordinateSensitiveToGrowth === 'boolean' ? 'no' : null),
      minWidth: 220,
    },
    { id: 'exposure', header: mi ? 'Share shifted' : 'Shock exposure', value: (n) => fig(n).exposure, cell: (n) => fmtShare(fig(n).exposure), num: true },
    { id: 'existing', header: `Existing coverage${underMi}`, value: (n) => fig(n).existing, cell: (n) => fmtShare(fig(n).existing), num: true },
    { id: 'optimized', header: `Optimized coverage${underMi}`, value: (n) => fig(n).optimized, cell: (n) => fmtShare(fig(n).optimized), num: true },
    { id: 'dividend', header: `Coordination dividend${underMi}`, value: (n) => fig(n).dividend, cell: (n) => fmtPp(fig(n).dividend), num: true },
    { id: 'residual', header: `Residual gap${underMi}`, value: (n) => fig(n).residual, cell: (n) => fmtShare(fig(n).residual), num: true },
    { id: 'lost', header: mi ? 'Supply shifted' : 'Lost supply', value: (n) => fig(n).lostUsd, cell: (n) => fmtUsd(fig(n).lostUsd), num: true },
    {
      id: 'compact',
      header: 'Compact',
      value: (n) => n.compact,
      cell: (n) =>
        n.compact ? (
          <Link className="link" to={href(compactHref(n.compact))}>
            {n.compact}
          </Link>
        ) : (
          'none'
        ),
    },
    ...(nOut ? [{ id: 'scope', header: 'Strategic scope', value: (n: AibNode) => (inStrategicScope(n) ? 'in scope' : 'outside') }] : []),
  ];
  const csv = () => ({
    name: `Sectoral compacts node classification${tech ? ` ${tech}` : ''}${all ? ' all nodes' : ''}${mi ? ' managed interdependence' : ''}`,
    text: csvWithRun(
      // the unprefixed columns are Decoupling's (aib_scenario names it); baseline_* and managed_* name their scenario
      shown.map((n) => ({ ...nodeCsvRow(nodeRow(n, names, tx)), ...(hasScenarios ? { ...scenarioCsvFields(m, 'decoupling'), ...nodeScenarioCsv(n) } : {}) })),
      m,
    ),
  });
  const compactsCsv = () => ({
    name: 'Sectoral compacts under the three scenarios',
    text: csvWithRun(
      compactRows.map((r) => compactScenarioCsvRow(r, titleOf(r.compact))),
      m,
    ),
  });
  const shareLine = classed
    .filter((g) => g.nodes.length)
    .map((g) => `${g.info.label} ${plural(g.nodes.length, 'node')}${lostTotal > 0 ? `, ${fmtShare(g.lostUsd / lostTotal, 0)} of ${lostWord}` : ''}`)
    .join('; ');
  const unchangedGroup = groups.find((g) => g.unchanged);
  const high = typeof m.thresholds.high_severity === 'number' ? m.thresholds.high_severity : null;
  const rule = typeof m.thresholds.coordinate_dividend === 'number' ? m.thresholds.coordinate_dividend : null;
  const table = () => <TableView rows={shown} cols={cols} caption={`Node classification${tech ? `: ${tech}` : ''}${hasScenarios ? `, ${sc.label}` : ''}`} rowKey={(n) => n.id} search pageSize={25} initialSort={{ id: 'lost', desc: true }} />;
  const xWords = mi ? 'Share shifted' : 'Shock exposure';

  return (
    <div className={PAGE}>
      <PageHead
        eyebrow={EYEBROW}
        title="Sectoral Compacts"
        lede={`A Sectoral Compact is a variable-geometry coalition that coordinates trade, investment, demand and risk-management policy across a coherent supply chain, where joint action improves resilience more than fragmented national or bilateral action would. The compacts here are derived from the model, from a supply shock on the observed trade network, not fixed in advance: nothing forces one compact per ${tx.one} or a compact on every ${tx.one}.${hasScenarios ? ' The compacts are formed under Decoupling; under Managed Interdependence the same compacts are solved again, with their own coalitions.' : ''}`}
      >
        <RunLine />
      </PageHead>

      <div className="space-y-14">
        <Section
          id="map"
          title="Where compacts are justified"
          intro={
            <>
              <span className="flex flex-wrap items-center gap-2">
                <BasisTag basis="facts" />
                {hasScenarios && <ScenarioTag id={scenario} meta={m} />}
              </span>
              <p className="mt-2">
                {mi
                  ? 'Each bubble is a supply-chain node Managed Interdependence runs on. Further right, a larger share of its supply has to shift away from capped suppliers; higher up, coordinated trade restores more than existing relationships do (the coordination dividend). Large bubbles shift the most supply. The colour is the class under the scenario; nodes it leaves unchanged are not placed.'
                  : 'Each bubble is a supply-chain node. Further right, the shock removes more of its supply; higher up, coordinated trade restores more than existing relationships do (the coordination dividend). Large bubbles lose the most supply. High, right bubbles are where coordination pays (COORDINATE); right but low, even coordinated trade leaves a gap and new capacity is needed (BUILD); MANAGE SPECIALIZATION marks supply that leans on one country.'}
              </p>
            </>
          }
        >
          <FilterRow result={`${plural(shown.length, 'node')} of ${pool.length}${!all && nOut ? ` in the strategic scope (${nOut} outside it not shown)` : ''}`}>
            <Select label={tx.One} value={tech ?? ''} options={[{ value: '', label: tx.all }, ...techOptions(tx, techs)]} onChange={(v) => setParam('tech', v || null)} />
            {nOut > 0 && <Checkbox label="Include nodes outside the strategic scope" checked={all} onChange={(v) => setParam('scope', v ? 'all' : null)} />}
          </FilterRow>
          <ChartCard
            headingLevel={3}
            title="Strategic map of every node"
            sub={`Across: ${mi ? 'share of allied demand shifted' : 'shock exposure'}. Up: coordination dividend. Area: ${lostWord}. Dotted rules: the model's thresholds for high severity${high !== null ? ` (${fmtShare(high, 0)})` : ''} and for a material coordination dividend${rule !== null ? ` (${fmtPp(rule, 0)})` : ''}. Click a bubble to open its compact.${unplaced > 0 ? ` ${plural(unplaced, 'node')} unchanged under Managed Interdependence ${unplaced === 1 ? 'is' : 'are'} not placed.` : ''}`}
            summary={shareLine}
            legend={<ClassLegend />}
            table={table}
            csv={csv}
          >
            {points.length ? (
              <StrategicMap
                points={points}
                highSeverity={high}
                dividendRule={rule}
                ariaLabel={`Strategic map of ${plural(points.length, 'node')}${tech ? ` in ${tech}` : ''}${hasScenarios ? `, ${sc.label}` : ''}: ${xWords.toLowerCase()} across, coordination dividend up, bubble area ${lostWord}. ${shareLine}`}
                onSelect={(p) => p.node.compact && navigate(href(compactHref(p.node.compact)))}
                where={(n) => nodeWhere(tx, n)}
                xWords={xWords}
                lostWord={mi ? 'shifted' : 'lost'}
              />
            ) : (
              <EmptyState>
                {mi
                  ? `Managed Interdependence runs on no node${tech ? ` in ${tech}` : ''}: every one is already as diversified as its benchmark, within the cap, or without one.`
                  : `No node with both a shock exposure and a coordination dividend${tech ? ` in ${tech}` : ''}.`}
              </EmptyState>
            )}
          </ChartCard>
        </Section>

        <Section
          id="compacts"
          title={`The compacts (${doc.compacts.length})`}
          intro={
            <p>
              {hasScenarios
                ? `Ordered by the supply ${mi ? 'Managed Interdependence shifts' : 'the shock removes'}, largest first. Each card says why the compact exists under the scenario shown, its main nodes and countries, the policy it most needs and how much of its demand is served; the mini bar compares the three scenarios (kept, restored by trade, new capacity; the figure is the share served after optimized trade). A group marked “compact unnecessary” is shown too: its supply chains hang together, but a coalition would barely improve on existing relationships.`
                : 'Ordered by the supply the shock removes, largest first. Each card says why the compact exists, its main nodes and countries, the policy it most needs and how much of its demand is served at each step: the shock alone, existing relationships, the trade compact and trade plus investment. A group marked “compact unnecessary” is shown too: its supply chains hang together, but a coalition would barely improve on existing relationships.'}
            </p>
          }
        >
          {doc.compacts.length ? (
            hasScenarios ? (
              <ChartCard
                headingLevel={3}
                title="Every compact under the three scenarios"
                sub={`Cards ordered by the supply ${mi ? 'Managed Interdependence shifts' : 'Decoupling removes'}; the table compares the scenarios side by side and sorts by any column.`}
                legend={<CoverageLegend />}
                table={() => (
                  <TableView
                    rows={compactRows}
                    cols={compactCols((c) => titleOf(c), href)}
                    caption="Every compact under the three scenarios"
                    rowKey={(r) => r.compact.id}
                    pageSize={50}
                    initialSort={{ id: mi ? 'mLost' : 'dLost', desc: true }}
                  />
                )}
                csv={compactsCsv}
                summary={`The compacts in order of the supply ${mi ? 'Managed Interdependence shifts' : 'Decoupling removes'}: ${ordered
                  .slice(0, 5)
                  .map((c) => `${c.id} ${fmtUsd(compactScenario(c, scenario, nodeOf).lostUsd)}`)
                  .join('; ')}.`}
              >
                <ul className="grid gap-4 md:grid-cols-2 xl:grid-cols-3">
                  {ordered.map((c) => (
                    <CompactCard key={c.id} c={c} managed={managed} managedState={managedState} />
                  ))}
                </ul>
              </ChartCard>
            ) : (
              <ul className="grid gap-4 md:grid-cols-2 xl:grid-cols-3">
                {doc.compacts.map((c) => (
                  <CompactCard key={c.id} c={c} managed={null} managedState="ready" />
                ))}
              </ul>
            )
          ) : (
            <EmptyState>Under the headline shock no group of supply chains passed the tests for a compact: coordinated action would not do materially better than bilateral action.</EmptyState>
          )}
        </Section>

        <Section
          id="nodes"
          title="How every sub-sector is classified"
          intro={
            <p>
              {mi
                ? 'Under Managed Interdependence, every sub-sector node more concentrated than its benchmark gets a treatment from its own results under the cap; the others are unchanged. Nodes in one compact can get different treatments.'
                : 'Every sub-sector node (a group of traded goods at one stage of a supply chain) gets a headline treatment from its own results, and a set of policy needs that can name several instruments at once. Nodes in one compact can get different treatments: a compact coordinates trade where new relationships help, builds capacity where a gap remains and manages specialization where supply leans on one country.'}
              {nOut
                ? ` ${plural(nOut, 'node')} outside the strategic scope ${nOut === 1 ? 'is' : 'are'} modelled too but ${nOut === 1 ? 'is' : 'are'} not headline results; ${all ? 'they are included below, marked' : 'include them below to see them'}.`
                : ''}
            </p>
          }
        >
          <Disclosure summary={`Explore all nodes (${shown.length}${tech ? ` in ${tech}` : ''})`} defaultOpen={openNodes}>
            <ChartCard
              headingLevel={3}
              title="Nodes by class"
              sub={`Where the ${lostWord} sits${tech ? ` in ${tech}` : ''}${!all && nOut ? ', nodes of the strategic scope' : ''}${hasScenarios ? `, ${sc.label}` : ''}: the bar splits it by class; below, every node by class, largest ${mi ? 'shift' : 'loss'} first, with its compact${mi ? '' : ', its policy needs where they say more than its class, any physical-capability warning, and a flag where the class depends on the existing-relationship growth bound'}. A node outside every compact opens its class under other assumptions and growth bounds in place; the others show it on their compact's page.`}
              summary={shareLine}
              legend={<ClassLegend />}
              table={table}
              csv={csv}
              notes={<span>Classes are policy interpretations of continuous results under the thresholds on the About page.</span>}
            >
              {lostTotal > 0 ? <LostByClassBar groups={classed} label={`${mi ? 'Shifted' : 'Lost'} supply by class. ${shareLine}`} /> : null}
              <p className="mt-2 text-xs text-paper-mute">{shareLine || `No node for this ${tx.one}.`}</p>
              <div className="mt-5 space-y-6">
                {groups
                  .filter((g) => g.nodes.length)
                  .map((g) => (
                    <div key={g.cls} data-group={g.cls}>
                      <h4 className="flex items-center gap-2 text-sm font-medium text-paper">
                        {g.unchanged ? null : <ClassSwatch cls={g.cls} />}
                        {g.unchanged ? 'Unchanged under Managed Interdependence' : g.info.label}{' '}
                        <span className="font-normal text-paper-mute">({g.nodes.length})</span>
                      </h4>
                      <p className="mb-2 mt-0.5 text-xs text-paper-mute">{g.info.meaning}</p>
                      <ul className="flex flex-wrap gap-2">
                        {g.nodes.map((n) => (
                          <NodeChip key={n.id} n={n} scenario={scenario} />
                        ))}
                      </ul>
                    </div>
                  ))}
              </div>
              {mi && unchangedGroup && unchangedGroup.nodes.length > 0 && (
                <p className="mt-3 text-xs text-paper-mute">The unchanged nodes keep the supply they have today; their class under Decoupling is on their compact&apos;s page.</p>
              )}
            </ChartCard>
          </Disclosure>
        </Section>

        <ReadingNotes />
      </div>
    </div>
  );
}
