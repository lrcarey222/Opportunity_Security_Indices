// The Sectoral Compacts landing /compacts: what a compact is and the run's settings (demand scope and
// candidate partners, shock semantics, supply constraint, strategic scope), a card per compact
// (archetype, type, nodes, members, associates, critical external dependencies with the marginal ones
// apart, physical-capability warnings and the mini coverage strip: shock -> existing -> trade -> trade +
// investment), then how every classified sub-sector node is treated, grouped by class (with each node's
// policy needs where they say more than the class, its warning and its growth-bound flag; a node outside
// every compact opens its class under other assumptions and growth bounds in place), filterable by
// technology (URL `tech`) and by default only the nodes of the strategic scope (URL `scope=all` includes
// the others), each node linking to its compact when it has one. Table twin and CSV.

import { useEffect, useMemo } from 'react';
import { Link, useLocation, useSearchParams } from 'react-router';
import { ChartCard } from '../../charts/ChartCard.tsx';
import { TableView, type Col } from '../../components/TableView.tsx';
import { Checkbox, Disclosure, EmptyState, FilterRow, PageHead, Select, Tag } from '../../components/ui.tsx';
import { useTitle } from '../../shell/title.ts';
import { EYEBROW, PAGE, useAibCtx } from '../AibLayout.tsx';
import { AIB_PHYSICAL_WARNING, coalitionMembersOf, inStrategicScope, type AibCompact, type AibNode } from '../contract.ts';
import { ClassLegend, ClassSwatch, PhysicalWarning, PolicyNeeds } from '../charts/ClassChip.tsx';
import { CoverageStrip } from '../charts/CoverageStrip.tsx';
import { LostByClassBar } from '../charts/LostByClassBar.tsx';
import { TypeTag } from '../charts/TypeTag.tsx';
import { classInfo, policyNeedsText } from '../derive/classes.ts';
import { compactVsFragmented, coverageSteps } from '../derive/coverage.ts';
import { fmtShare, fmtUsd, listWords, plural } from '../derive/format.ts';
import { nodeCsvRow, nodeRow, nodesByClass, outOfScopeCount, scopedNodes, technologiesOf } from '../derive/nodes.ts';
import { growthFlag, robustnessSummary } from '../derive/sensitivity.ts';
import { candidateUniverse, coverageBase, demandScope, shockSemantics, strategicScope, supplyConstraint } from '../derive/settings.ts';
import { tierEconomies } from '../derive/tiers.ts';
import { compactTypeInfo } from '../derive/types.ts';
import { compactHref } from '../nav.ts';
import { csvWithRun, ReadingNotes, Section } from './common.tsx';
import { NodeRobustness } from './NodeRobustness.tsx';

function CompactCard({ c }: { c: AibCompact }) {
  const { names } = useAibCtx();
  const steps = coverageSteps(c.metrics);
  const tiers = tierEconomies(c, names);
  const associates = tiers.filter((e) => e.tier === 'associate partner').length;
  const critExternal = tiers.filter((e) => e.tier === 'critical external dependency').length;
  const marginal = tiers.filter((e) => e.tier === 'critical external dependency (marginal)').length;
  const warned = c.physicalWarnings ?? [];
  const type = compactTypeInfo(c.type, compactVsFragmented(c.metrics)?.gain);
  return (
    <li>
      <article className="card flex h-full flex-col p-5" aria-labelledby={`card-${c.id}`}>
        <div className="flex flex-wrap items-center gap-2">
          <span className="eyebrow">{c.id}</span>
          <Tag>{c.archetype}</Tag>
          <TypeTag info={type} />
        </div>
        <h3 id={`card-${c.id}`} className="mt-2 text-lg font-medium leading-snug text-paper">
          <Link className="hover:text-signal" to={compactHref(c.id)}>
            {c.name}
          </Link>
        </h3>
        {type.tone === 'unnecessary' && <p className="mt-1 text-xs text-paper-mute">{type.explanation}</p>}
        <p className="mt-1 text-sm text-paper-mute">
          {plural(c.nodes.length, 'node')} · {plural(coalitionMembersOf(c).length, 'member')}
          {associates ? ` + ${plural(associates, 'associate')}` : ''}
          {critExternal || marginal ? ` · ${plural(critExternal, 'critical external dependency', 'critical external dependencies')}` : ''}
          {marginal ? ` (+${marginal} marginal)` : ''}
        </p>
        <p className="mt-1 text-sm text-paper-dim">
          {fmtUsd(c.metrics.lostUsd)} of {fmtUsd(c.metrics.demandUsd)} demand lost to the shock ({fmtShare(c.metrics.exposure, 0)})
        </p>
        {warned.length > 0 && (
          <p className="mt-2 flex flex-wrap items-center gap-2 text-xs text-paper-dim">
            <PhysicalWarning warning={AIB_PHYSICAL_WARNING} />
            <span>{listWords(warned)}</span>
          </p>
        )}
        <div className="mt-4">
          <CoverageStrip steps={steps} label={`${c.name}: demand served at each step`} />
        </div>
        <div className="mt-auto pt-4 text-sm">
          <Link className="link" to={compactHref(c.id)} aria-label={`Open the compact ${c.name}`}>
            Open the compact &rarr;
          </Link>
        </div>
      </article>
    </li>
  );
}

function NodeChip({ n }: { n: AibNode }) {
  const { doc } = useAibCtx();
  const flag = growthFlag(n);
  return (
    <li className="max-w-full border hairline px-2.5 py-1.5 text-xs">
      <span className="block text-paper">{n.label}</span>
      <span className="block text-paper-mute">
        {n.techs.join(', ')} · {n.stage}
        {n.compact && (
          <>
            {' · '}
            <Link className="link" to={compactHref(n.compact)}>
              {n.compact}
            </Link>
          </>
        )}
        {!inStrategicScope(n) && ' · outside the strategic scope'}
      </span>
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
      {!n.compact && (n.variants !== undefined || n.growth !== undefined) && (
        <div className="mt-1">
          <Disclosure summary={robustnessSummary(n, doc.meta)}>
            <NodeRobustness n={n} meta={doc.meta} />
          </Disclosure>
        </div>
      )}
    </li>
  );
}

/** The run's settings in one paragraph (an older bundle: its single universe). */
function RunLine() {
  const { doc, shockLabel } = useAibCtx();
  const m = doc.meta;
  const ds = demandScope(m);
  const cu = candidateUniverse(m);
  const sem = shockSemantics(m);
  const con = supplyConstraint(m);
  const st = strategicScope(m);
  return (
    <p className="mt-3 max-w-3xl text-sm text-paper-mute" data-testid="run-settings">
      Headline: <span className="text-paper-dim">{shockLabel}</span>, {m.headlineScenario} scenario
      {sem ? `, ${sem.label.toLowerCase()} semantics (${sem.mode === 'hold' ? 'the shocked economy keeps buying' : sem.words})` : ''}
      {con ? `, ${con.label.toLowerCase()} supply` : ''}; trade in {m.tradeYear}.{' '}
      {ds || cu ? (
        <>
          Demand scope: <span className="text-paper-dim">{ds ? ds.words : m.universe}</span>. Candidate partners:{' '}
          <span className="text-paper-dim">{cu ? cu.words : m.universe}</span>.
        </>
      ) : (
        <>Candidate universe: {m.universe}.</>
      )}{' '}
      {st && st.mode !== 'all' ? `Strategic scope: ${st.label.toLowerCase()} (${listWords(st.techs)}). ` : ''}
      Coverage is the share of {coverageBase(m)} served after the shock.{' '}
      <Link className="link" to="/compacts/about">
        How compacts are derived &rarr;
      </Link>
    </p>
  );
}

export default function CompactsIndexView() {
  useTitle('Compacts', 'aib');
  const { doc, names } = useAibCtx();
  const loc = useLocation();
  const [params, setParams] = useSearchParams();
  const nOut = useMemo(() => outOfScopeCount(doc.nodes), [doc]);
  const all = params.get('scope') === 'all';
  const pool = useMemo(() => scopedNodes(doc.nodes, all), [doc, all]);
  const techs = useMemo(() => technologiesOf(pool), [pool]);
  const raw = params.get('tech');
  const tech = raw && techs.includes(raw) ? raw : null;
  const groups = useMemo(() => nodesByClass(pool, tech), [pool, tech]);
  const shown = groups.flatMap((g) => g.nodes);
  const lostTotal = groups.reduce((s, g) => s + g.lostUsd, 0);
  const m = doc.meta;
  const setParam = (k: string, v: string | null) => {
    const q = new URLSearchParams(params);
    if (v) q.set(k, v);
    else q.delete(k);
    setParams(q, { replace: true, preventScrollReset: true });
  };

  // follow a deep link's #anchor (#nodes from Home)
  useEffect(() => {
    if (loc.hash) document.getElementById(loc.hash.slice(1))?.scrollIntoView?.({ block: 'start' });
  }, [loc.hash]);

  const cols: Col<AibNode>[] = [
    { id: 'node', header: 'Sub-sector', value: (n) => n.label },
    { id: 'techs', header: 'Technologies', value: (n) => n.techs.join(', ') },
    { id: 'stage', header: 'Stage', value: (n) => n.stage },
    { id: 'cls', header: 'Class', value: (n) => classInfo(n.cls).label },
    { id: 'needs', header: 'Policy needs', value: (n) => policyNeedsText(n.policyNeeds) },
    { id: 'warning', header: 'Warning', value: (n) => n.warning ?? 'none', minWidth: 200 },
    {
      id: 'growth',
      header: 'Class depends on the growth bound',
      value: (n) => growthFlag(n) ?? (typeof n.coordinateSensitiveToGrowth === 'boolean' ? 'no' : null),
      minWidth: 220,
    },
    { id: 'exposure', header: 'Shock exposure', value: (n) => n.exposure, cell: (n) => fmtShare(n.exposure), num: true },
    { id: 'existing', header: 'Existing coverage', value: (n) => n.existing, cell: (n) => fmtShare(n.existing), num: true },
    { id: 'optimized', header: 'Optimized coverage', value: (n) => n.optimized, cell: (n) => fmtShare(n.optimized), num: true },
    { id: 'residual', header: 'Residual gap', value: (n) => n.residual, cell: (n) => fmtShare(n.residual), num: true },
    { id: 'lost', header: 'Lost supply', value: (n) => n.lostUsd, cell: (n) => fmtUsd(n.lostUsd), num: true },
    {
      id: 'compact',
      header: 'Compact',
      value: (n) => n.compact,
      cell: (n) =>
        n.compact ? (
          <Link className="link" to={compactHref(n.compact)}>
            {n.compact}
          </Link>
        ) : (
          'none'
        ),
    },
    ...(nOut ? [{ id: 'scope', header: 'Strategic scope', value: (n: AibNode) => (inStrategicScope(n) ? 'in scope' : 'outside') }] : []),
  ];
  const csv = () => ({
    name: `Sectoral compacts node classification${tech ? ` ${tech}` : ''}${all ? ' all nodes' : ''}`,
    text: csvWithRun(
      shown.map((n) => nodeCsvRow(nodeRow(n, names))),
      m,
    ),
  });
  const shareLine = groups
    .filter((g) => g.nodes.length)
    .map((g) => `${g.info.label} ${plural(g.nodes.length, 'node')}${lostTotal > 0 ? `, ${fmtShare(g.lostUsd / lostTotal, 0)} of lost supply` : ''}`)
    .join('; ');

  return (
    <div className={PAGE}>
      <PageHead
        eyebrow={EYEBROW}
        title="Sectoral Compacts"
        lede="A Sectoral Compact is a variable-geometry coalition that coordinates trade, investment, demand and risk-management policy across a coherent supply chain, where joint action improves resilience more than fragmented national or bilateral action would. The compacts here are derived from the model, from a supply shock on the observed trade network, not fixed in advance: nothing forces one compact per technology or a compact on every technology."
      >
        <RunLine />
      </PageHead>

      <div className="space-y-14">
        <Section
          id="compacts"
          title={`The compacts (${doc.compacts.length})`}
          intro={
            <p>
              Ordered by the supply the shock removes, largest first. Each strip shows how much of the compact&apos;s demand is served after the shock: by the shock alone (nothing reroutes), by
              existing relationships, by the trade compact (existing relationships plus the members&apos; new and expanded ones) and by trade plus a standardized investment response. A group marked &ldquo;compact
              unnecessary&rdquo; is shown too: its supply chains hang together, but a coalition would barely improve on what the demand anchors&apos; existing relationships give them.
            </p>
          }
        >
          {doc.compacts.length ? (
            <ul className="grid gap-4 md:grid-cols-2 xl:grid-cols-3">
              {doc.compacts.map((c) => (
                <CompactCard key={c.id} c={c} />
              ))}
            </ul>
          ) : (
            <EmptyState>Under the headline shock no group of supply chains passed the tests for a compact: coordinated action would not do materially better than bilateral action.</EmptyState>
          )}
        </Section>

        <Section
          id="nodes"
          title="How every sub-sector is classified"
          intro={
            <p>
              Every sub-sector node (a group of traded goods at one stage of a supply chain) gets a headline treatment from its own results, and a set of policy needs that can name several
              instruments at once. Nodes in one compact can get different treatments: a compact coordinates trade where new relationships help, builds capacity where a gap remains and
              manages specialization where supply leans on one country.
              {nOut
                ? ` ${plural(nOut, 'node')} outside the strategic scope ${nOut === 1 ? 'is' : 'are'} modelled too but ${nOut === 1 ? 'is' : 'are'} not headline results; ${all ? 'they are included below, marked' : 'include them below to see them'}.`
                : ''}
            </p>
          }
        >
          <FilterRow result={`${plural(shown.length, 'node')} of ${pool.length}${!all && nOut ? ` in the strategic scope (${nOut} outside it not shown)` : ''}`}>
            <Select
              label="Technology"
              value={tech ?? ''}
              options={[{ value: '', label: 'All technologies' }, ...techs.map((t) => ({ value: t, label: t }))]}
              onChange={(v) => setParam('tech', v || null)}
            />
            {nOut > 0 && <Checkbox label="Include nodes outside the strategic scope" checked={all} onChange={(v) => setParam('scope', v ? 'all' : null)} />}
          </FilterRow>
          <ChartCard
            headingLevel={3}
            title="Nodes by class"
            sub={`Where the lost supply sits${tech ? ` in ${tech}` : ''}${!all && nOut ? ', nodes of the strategic scope' : ''}: the bar splits it by class; below, every node by class, largest loss first, with its compact, its policy needs where they say more than its class, any physical-capability warning, and a flag where the class depends on the existing-relationship growth bound. A node outside every compact opens its class under other assumptions and growth bounds in place; the others show it on their compact's page.`}
            summary={shareLine}
            legend={<ClassLegend />}
            table={() => <TableView rows={shown} cols={cols} caption={`Node classification${tech ? `: ${tech}` : ''}`} rowKey={(n) => n.id} search pageSize={25} initialSort={{ id: 'lost', desc: true }} />}
            csv={csv}
            notes={<span>Classes are policy interpretations of continuous results under the thresholds on the About page.</span>}
          >
            <LostByClassBar groups={groups} label={`Lost supply by class. ${shareLine}`} />
            <p className="mt-2 text-xs text-paper-mute">{shareLine || 'No node for this technology.'}</p>
            <div className="mt-5 space-y-6">
              {groups
                .filter((g) => g.nodes.length)
                .map((g) => (
                  <div key={g.cls}>
                    <h4 className="flex items-center gap-2 text-sm font-medium text-paper">
                      <ClassSwatch cls={g.cls} />
                      {g.info.label}
                      <span className="font-normal text-paper-mute">({g.nodes.length})</span>
                    </h4>
                    <p className="mb-2 mt-0.5 text-xs text-paper-mute">{g.info.meaning}</p>
                    <ul className="flex flex-wrap gap-2">
                      {g.nodes.map((n) => (
                        <NodeChip key={n.id} n={n} />
                      ))}
                    </ul>
                  </div>
                ))}
            </div>
          </ChartCard>
        </Section>

        <ReadingNotes />
      </div>
    </div>
  );
}
