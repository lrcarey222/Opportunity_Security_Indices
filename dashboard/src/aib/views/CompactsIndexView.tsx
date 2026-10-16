// The Sectoral Compacts landing /compacts: what a compact is, a card per compact (archetype, type, nodes,
// members and the mini coverage strip: shock -> existing -> trade -> trade + investment), then how every
// classified sub-sector node is treated, grouped by class, filterable by technology (URL `tech`), each
// node linking to its compact when it has one. Table twin and CSV.

import { useEffect, useMemo } from 'react';
import { Link, useLocation, useSearchParams } from 'react-router';
import { ChartCard } from '../../charts/ChartCard.tsx';
import { TableView, type Col } from '../../components/TableView.tsx';
import { EmptyState, FilterRow, PageHead, Select, Tag } from '../../components/ui.tsx';
import { useTitle } from '../../shell/title.ts';
import { EYEBROW, PAGE, useAibCtx } from '../AibLayout.tsx';
import type { AibCompact, AibNode } from '../contract.ts';
import { ClassLegend, ClassSwatch } from '../charts/ClassChip.tsx';
import { CoverageStrip } from '../charts/CoverageStrip.tsx';
import { LostByClassBar } from '../charts/LostByClassBar.tsx';
import { TypeTag } from '../charts/TypeTag.tsx';
import { classInfo } from '../derive/classes.ts';
import { compactVsFragmented, coverageSteps } from '../derive/coverage.ts';
import { fmtShare, fmtUsd, plural } from '../derive/format.ts';
import { nodeCsvRow, nodeRow, nodesByClass, technologiesOf } from '../derive/nodes.ts';
import { compactTypeInfo } from '../derive/types.ts';
import { compactHref } from '../nav.ts';
import { csvWithRun, ReadingNotes, Section } from './common.tsx';

function CompactCard({ c }: { c: AibCompact }) {
  const steps = coverageSteps(c.metrics);
  const nAssoc = c.roles.filter((r) => !r.member).map((r) => r.iso3);
  const associates = new Set(nAssoc).size;
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
          {plural(c.nodes.length, 'node')} · {plural(c.coalition.members.length, 'member')}
          {associates ? ` + ${plural(associates, 'associate')}` : ''}
        </p>
        <p className="mt-1 text-sm text-paper-dim">
          {fmtUsd(c.metrics.lostUsd)} of {fmtUsd(c.metrics.demandUsd)} demand lost to the shock ({fmtShare(c.metrics.exposure, 0)})
        </p>
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
  const [params, setParams] = useSearchParams();
  const techs = useMemo(() => technologiesOf(doc.nodes), [doc]);
  const raw = params.get('tech');
  const tech = raw && techs.includes(raw) ? raw : null;
  const groups = useMemo(() => nodesByClass(doc.nodes, tech), [doc, tech]);
  const shown = groups.flatMap((g) => g.nodes);
  const lostTotal = groups.reduce((s, g) => s + g.lostUsd, 0);
  const m = doc.meta;

  // follow a deep link's #anchor (#nodes from Home)
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
              Every sub-sector node (a group of traded goods at one stage of a supply chain) gets a treatment from its own results. Nodes in one compact can get different treatments: a compact
              coordinates trade where new relationships help, builds capacity where a gap remains and manages specialization where supply leans on one country.
            </p>
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
            title="Nodes by class"
            sub={`Where the lost supply sits${tech ? ` in ${tech}` : ''}: the bar splits it by class; below, every node by class, largest loss first, with its compact.`}
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
