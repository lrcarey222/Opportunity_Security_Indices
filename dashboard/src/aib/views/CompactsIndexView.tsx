// The Sectoral Compacts landing /compacts (IA refactor, 2026-10-16; brief s.16: progressive disclosure):
// what a compact is, then the strategic map of every node (shock exposure x coordination dividend, area
// = lost supply, colour = class; filterable by technology, URL `tech`), then one slim card per compact
// (name, why it exists, main nodes, key countries, dominant policy need, headline resilience effect with
// the mini coverage strip, "View compact"), and the full classification of every node behind "Explore
// all nodes" (open on #nodes or a technology filter). Table twins and CSVs.

import { useEffect, useMemo } from 'react';
import { Link, useLocation, useNavigate, useSearchParams } from 'react-router';
import { ChartCard } from '../../charts/ChartCard.tsx';
import { TableView, type Col } from '../../components/TableView.tsx';
import { Disclosure, EmptyState, FilterRow, PageHead, Select } from '../../components/ui.tsx';
import { useTitle } from '../../shell/title.ts';
import { EYEBROW, PAGE, useAibCtx } from '../AibLayout.tsx';
import type { AibCompact, AibNode } from '../contract.ts';
import { ClassLegend, ClassSwatch } from '../charts/ClassChip.tsx';
import { CoverageStrip } from '../charts/CoverageStrip.tsx';
import { LostByClassBar } from '../charts/LostByClassBar.tsx';
import { mapPoints, StrategicMap } from '../charts/StrategicMap.tsx';
import { TypeTag } from '../charts/TypeTag.tsx';
import { classInfo } from '../derive/classes.ts';
import { compactVsFragmented, coverageSteps } from '../derive/coverage.ts';
import { compactTitle, fmtPp, fmtShare, fmtUsd, listWords, plural } from '../derive/format.ts';
import { byLostSupply, compactNodes, nodeCsvRow, nodeRow, nodesByClass, technologiesOf } from '../derive/nodes.ts';
import { dividendOf } from '../derive/partnerships.ts';
import { compactTypeInfo } from '../derive/types.ts';
import { compactHref } from '../nav.ts';
import { BasisTag } from './basis.tsx';
import { csvWithRun, ReadingNotes, Section } from './common.tsx';

function CompactCard({ c }: { c: AibCompact }) {
  const { name, nodeOf } = useAibCtx();
  const steps = coverageSteps(c.metrics);
  const type = compactTypeInfo(c.type, compactVsFragmented(c.metrics)?.gain);
  const main = compactNodes(c, nodeOf).sort(byLostSupply);
  const anchors = c.anchors.slice(0, 4).map((x) => name(x));
  const suppliers = c.suppliers.filter((x) => !c.anchors.includes(x)).slice(0, 3).map((x) => name(x));
  const before = c.metrics.existing;
  const after = c.metrics.investment ?? c.metrics.trade;
  return (
    <li>
      <article className="card flex h-full flex-col p-5" aria-labelledby={`card-${c.id}`}>
        <div className="flex flex-wrap items-center gap-2">
          <span className="eyebrow">{c.id}</span>
          <TypeTag info={type} />
        </div>
        <h3 id={`card-${c.id}`} className="mt-2 text-lg font-medium leading-snug text-paper">
          <Link className="hover:text-signal" to={compactHref(c.id)} title={c.name}>
            {compactTitle(c)}
          </Link>
        </h3>
        <dl className="mt-3 grid grid-cols-[7.5rem_minmax(0,1fr)] gap-x-3 gap-y-1.5 text-sm">
          <dt className="text-paper-mute">Why it exists</dt>
          <dd className="text-paper-dim">
            The shock removes {fmtShare(c.metrics.exposure, 0)} of {fmtUsd(c.metrics.demandUsd)} demand. {type.explanation}
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
            {!anchors.length && !suppliers.length ? '–' : ''}
          </dd>
          <dt className="text-paper-mute">Policy need</dt>
          <dd className="text-paper-dim">{c.policyNeeds[0] ?? 'none listed'}</dd>
          <dt className="text-paper-mute">Resilience effect</dt>
          <dd className="text-paper-dim">
            Demand served {fmtShare(before, 0)} with existing relationships, {fmtShare(after, 0)} with the compact
          </dd>
        </dl>
        <div className="mt-4">
          <CoverageStrip steps={steps} label={`${c.name}: demand served at each step`} />
        </div>
        <div className="mt-auto pt-4 text-sm">
          <Link className="link" to={compactHref(c.id)} aria-label={`View compact: ${compactTitle(c)}`}>
            View compact &rarr;
          </Link>
        </div>
      </article>
    </li>
  );
}

function NodeChip({ n }: { n: AibNode }) {
  return (
    <li className="border hairline px-2.5 py-1.5 text-xs">
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
      </span>
    </li>
  );
}

export default function CompactsIndexView() {
  useTitle('Compacts', 'aib');
  const { doc, shockLabel, names } = useAibCtx();
  const loc = useLocation();
  const navigate = useNavigate();
  const [params, setParams] = useSearchParams();
  const techs = useMemo(() => technologiesOf(doc.nodes), [doc]);
  const raw = params.get('tech');
  const tech = raw && techs.includes(raw) ? raw : null;
  const groups = useMemo(() => nodesByClass(doc.nodes, tech), [doc, tech]);
  const shown = groups.flatMap((g) => g.nodes);
  const points = useMemo(() => mapPoints(shown), [shown]);
  const lostTotal = groups.reduce((s, g) => s + g.lostUsd, 0);
  const m = doc.meta;
  const openNodes = loc.hash === '#nodes' || tech !== null;

  // follow a deep link's #anchor (#nodes, #map)
  useEffect(() => {
    if (loc.hash) document.getElementById(loc.hash.slice(1))?.scrollIntoView?.({ block: 'start' });
  }, [loc.hash]);

  const cols: Col<AibNode>[] = [
    { id: 'node', header: 'Sub-sector', value: (n) => n.label },
    { id: 'techs', header: 'Technologies', value: (n) => n.techs.join(', ') },
    { id: 'stage', header: 'Stage', value: (n) => n.stage },
    { id: 'cls', header: 'Class', value: (n) => classInfo(n.cls).label },
    { id: 'exposure', header: 'Shock exposure', value: (n) => n.exposure, cell: (n) => fmtShare(n.exposure), num: true },
    { id: 'existing', header: 'Existing coverage', value: (n) => n.existing, cell: (n) => fmtShare(n.existing), num: true },
    { id: 'optimized', header: 'Optimized coverage', value: (n) => n.optimized, cell: (n) => fmtShare(n.optimized), num: true },
    { id: 'dividend', header: 'Coordination dividend', value: (n) => dividendOf(n), cell: (n) => fmtPp(dividendOf(n)), num: true },
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
  ];
  const csv = () => ({
    name: `Sectoral compacts node classification${tech ? ` ${tech}` : ''}`,
    text: csvWithRun(
      shown.map((n) => nodeCsvRow(nodeRow(n, names))),
      m,
    ),
  });
  const shareLine = groups
    .filter((g) => g.nodes.length)
    .map((g) => `${g.info.label} ${plural(g.nodes.length, 'node')}${lostTotal > 0 ? `, ${fmtShare(g.lostUsd / lostTotal, 0)} of lost supply` : ''}`)
    .join('; ');
  const high = typeof m.thresholds.high_severity === 'number' ? m.thresholds.high_severity : null;
  const rule = typeof m.thresholds.coordinate_dividend === 'number' ? m.thresholds.coordinate_dividend : null;
  const table = () => <TableView rows={shown} cols={cols} caption={`Node classification${tech ? `: ${tech}` : ''}`} rowKey={(n) => n.id} search pageSize={25} initialSort={{ id: 'lost', desc: true }} />;

  return (
    <div className={PAGE}>
      <PageHead
        eyebrow={EYEBROW}
        title="Sectoral Compacts"
        lede="A Sectoral Compact is a variable-geometry coalition that coordinates trade, investment, demand and risk-management policy across a coherent supply chain, where joint action improves resilience more than fragmented national or bilateral action would. The compacts here are derived from the model, from a supply shock on the observed trade network, not fixed in advance: nothing forces one compact per technology or a compact on every technology."
      >
        <p className="mt-3 max-w-3xl text-sm text-paper-mute">
          Headline: <span className="text-paper-dim">{shockLabel}</span>, {m.headlineScenario} scenario; trade in {m.tradeYear}; candidate universe: {m.universe}. Coverage is the share of
          the universe&apos;s baseline traded demand served after the shock.{' '}
          <Link className="link" to="/compacts/about">
            How compacts are derived &rarr;
          </Link>
        </p>
      </PageHead>

      <div className="space-y-14">
        <Section
          id="map"
          title="Where compacts are justified"
          intro={
            <>
              <BasisTag basis="facts" />
              <p className="mt-2">
                Each bubble is a supply-chain node. Further right, the shock removes more of its supply; higher up, coordinated trade restores more than existing relationships do (the
                coordination dividend). Large bubbles lose the most supply. High, right bubbles are where coordination pays (COORDINATE); right but low, even coordinated trade leaves a gap and new
                capacity is needed (BUILD); MANAGE SPECIALIZATION marks supply that leans on one country.
              </p>
            </>
          }
        >
          <FilterRow result={`${plural(shown.length, 'node')} of ${doc.nodes.length}`}>
            <Select
              label="Technology"
              value={tech ?? ''}
              options={[{ value: '', label: 'All technologies' }, ...techs.map((t) => ({ value: t, label: t }))]}
              onChange={(v) => {
                const q = new URLSearchParams(params);
                if (v) q.set('tech', v);
                else q.delete('tech');
                setParams(q, { replace: true, preventScrollReset: true });
              }}
            />
          </FilterRow>
          <ChartCard
            headingLevel={3}
            title="Strategic map of every node"
            sub={`Across: shock exposure. Up: coordination dividend. Area: lost supply. Dotted rules: the model's thresholds for high severity${high !== null ? ` (${fmtShare(high, 0)})` : ''} and for a material coordination dividend${rule !== null ? ` (${fmtPp(rule, 0)})` : ''}. Click a bubble to open its compact.`}
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
                ariaLabel={`Strategic map of ${plural(points.length, 'node')}${tech ? ` in ${tech}` : ''}: shock exposure across, coordination dividend up, bubble area lost supply. ${shareLine}`}
                onSelect={(p) => p.node.compact && navigate(compactHref(p.node.compact))}
              />
            ) : (
              <EmptyState>No node with both a shock exposure and a coordination dividend{tech ? ` in ${tech}` : ''}.</EmptyState>
            )}
          </ChartCard>
        </Section>

        <Section
          id="compacts"
          title={`The compacts (${doc.compacts.length})`}
          intro={
            <p>
              Ordered by the supply the shock removes, largest first. Each card says why the compact exists, its main nodes and countries, the policy it most needs and how much of its demand is
              served at each step: the shock alone, existing relationships, the trade compact and trade plus investment. A group marked &ldquo;compact unnecessary&rdquo; is shown too: its supply chains
              hang together, but a coalition would barely improve on existing relationships.
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
              Every sub-sector node (a group of traded goods at one stage of a supply chain) gets a treatment from its own results. Nodes in one compact can get different treatments: a compact
              coordinates trade where new relationships help, builds capacity where a gap remains and manages specialization where supply leans on one country.
            </p>
          }
        >
          <Disclosure summary={`Explore all nodes (${shown.length}${tech ? ` in ${tech}` : ''})`} defaultOpen={openNodes}>
            <ChartCard
              headingLevel={3}
              title="Nodes by class"
              sub={`Where the lost supply sits${tech ? ` in ${tech}` : ''}: the bar splits it by class; below, every node by class, largest loss first, with its compact.`}
              summary={shareLine}
              legend={<ClassLegend />}
              table={table}
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
          </Disclosure>
        </Section>

        <ReadingNotes />
      </div>
    </div>
  );
}
