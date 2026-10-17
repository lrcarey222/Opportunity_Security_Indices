// 3. Node-by-node structure: one row per node of the compact with its class, its coverage track (static
// shock, existing relationships, optimized trade), the numbers, the optimized largest supplier, the
// investment hosts, the recommended treatment and its data quality (flags in a Disclosure). Nodes in one
// compact can get different treatments; the card says how many. Table twin and CSV. Then the
// specialization-resilience frontier of one node at a time (a node selector): coverage, coverage after a
// second shock and the specialization score under each supplier-share limit, with the two readings
// marked. Table twin and CSV (every node of the compact).

import { useState } from 'react';
import { ChartCard } from '../../../charts/ChartCard.tsx';
import { Legend } from '../../../charts/legends.tsx';
import { TableView, type Col } from '../../../components/TableView.tsx';
import { Disclosure, EmptyState, Select } from '../../../components/ui.tsx';
import type { AibNode } from '../../contract.ts';
import { ClassChip } from '../../charts/ClassChip.tsx';
import { NODE_MARK, NodeCoverageBar } from '../../charts/NodeCoverageBar.tsx';
import { ACCEPTABLE_COVERAGE, COSTLESS_COLOR, FrontierChart, REQUIRED_COLOR } from '../../charts/FrontierChart.tsx';
import { capLabel, FRONTIER_SERIES, frontierReadings, frontierRows, nodesWithFrontier, type FrontierRow } from '../../derive/frontier.ts';
import { classInfo, classMix } from '../../derive/classes.ts';
import { fmtPp, fmtScore, fmtShare, fmtUsd, listWords, plural } from '../../derive/format.ts';
import { nodeCsvRow, nodeRow } from '../../derive/nodes.ts';
import { csvWithRun, Section } from '../common.tsx';
import { Economy, useCompact } from './shared.tsx';

function NodeItem({ n }: { n: AibNode }) {
  const { name } = useCompact();
  const readings = frontierReadings(n);
  const coverageWords = `${n.label}: shock alone ${fmtShare(n.static)}, existing relationships ${fmtShare(n.existing)}, optimized trade ${fmtShare(n.optimized)} of demand served`;
  return (
    <li className="border-t hairline py-4 first:border-t-0 first:pt-0">
      <div className="flex flex-wrap items-center gap-x-3 gap-y-1.5">
        <ClassChip cls={n.cls} />
        <h4 className="text-base font-medium text-paper">{n.label}</h4>
        <span className="text-xs text-paper-mute">
          {n.techs.join(', ')} · {n.stage}
        </span>
      </div>
      <div className="mt-3 grid gap-x-8 gap-y-3 lg:grid-cols-2">
        <div>
          <NodeCoverageBar stat={n.static} existing={n.existing} optimized={n.optimized} label={coverageWords} />
          <p className="mt-1.5 text-xs leading-relaxed text-paper-dim tabular">
            Shock exposure {fmtShare(n.exposure)} ({fmtUsd(n.lostUsd)} of {fmtUsd(n.demandUsd)}) · existing {fmtShare(n.existing)} &rarr; optimized {fmtShare(n.optimized)} (dividend{' '}
            {fmtPp(n.dividend)}) · residual gap {fmtShare(n.residual)}
          </p>
        </div>
        <dl className="grid grid-cols-[minmax(0,11rem)_minmax(0,1fr)] gap-x-3 gap-y-1 text-xs">
          <dt className="text-paper-mute">Largest optimized supplier</dt>
          <dd className="text-paper-dim">
            {n.largest ? (
              <>
                <Economy iso3={n.largest} /> ({fmtShare(n.largestShare)}; before the shock <Economy iso3={n.baseLargest} /> {fmtShare(n.baseLargestShare)})
              </>
            ) : (
              '–'
            )}
          </dd>
          <dt className="text-paper-mute">Investment hosts</dt>
          <dd className="text-paper-dim">{n.hosts.length ? listWords(n.hosts.map((h) => name(h))) : 'none ranked'}</dd>
          <dt className="text-paper-mute">Recommended treatment</dt>
          <dd className="text-paper-dim">{n.treatment ?? '–'}</dd>
          <dt className="text-paper-mute">Why this class</dt>
          <dd className="text-paper-dim">{n.reason ?? '–'}</dd>
          {readings.costless !== null && (
            <>
              <dt className="text-paper-mute">Tightest costless supplier limit</dt>
              <dd className="text-paper-dim">{readings.costless}</dd>
            </>
          )}
          {readings.required !== null && (
            <>
              <dt className="text-paper-mute">Least diversification for resilience</dt>
              <dd className="text-paper-dim">{readings.required}</dd>
            </>
          )}
        </dl>
      </div>
      <div className="mt-2">
        <Disclosure summary={`Data quality: ${n.quality ?? 'not assessed'}${n.flags.length ? ` · ${plural(n.flags.length, 'flag')}` : ''}`}>
          {n.flags.length ? (
            <ul className="list-disc space-y-0.5 pl-5 text-xs text-paper-dim">
              {n.flags.map((f) => (
                <li key={f}>{f}</li>
              ))}
            </ul>
          ) : (
            <p className="text-xs text-paper-mute">No data-quality flags for this node.</p>
          )}
        </Disclosure>
      </div>
    </li>
  );
}

/** `frontier`: also show the specialization frontier card (false when the page puts it under method details). */
export function NodeSection({ frontier = true }: { frontier?: boolean } = {}) {
  const { c, nodes, meta, names } = useCompact();
  const mix = classMix(nodes.map((n) => n.cls));
  const rows = nodes.map((n) => nodeRow(n, names));
  const mixed = mix.length > 1;
  const cols: Col<(typeof rows)[number]>[] = [
    { id: 'node', header: 'Sub-sector', value: (r) => r.node },
    { id: 'stage', header: 'Stage', value: (r) => r.stage },
    { id: 'cls', header: 'Class', value: (r) => classInfo(r.cls).label },
    { id: 'exposure', header: 'Shock exposure', value: (r) => r.exposure, cell: (r) => fmtShare(r.exposure), num: true },
    { id: 'existing', header: 'Existing coverage', value: (r) => r.existing, cell: (r) => fmtShare(r.existing), num: true },
    { id: 'optimized', header: 'Optimized coverage', value: (r) => r.optimized, cell: (r) => fmtShare(r.optimized), num: true },
    { id: 'dividend', header: 'Coordination dividend', value: (r) => r.dividend, cell: (r) => fmtPp(r.dividend), num: true },
    { id: 'residual', header: 'Residual gap', value: (r) => r.residual, cell: (r) => fmtShare(r.residual), num: true },
    { id: 'largest', header: 'Largest optimized supplier', value: (r) => r.optimizedLargest },
    { id: 'largestShare', header: 'Its share', value: (r) => r.optimizedLargestShare, cell: (r) => fmtShare(r.optimizedLargestShare), num: true },
    { id: 'hhi', header: 'Concentration (HHI)', value: (r) => r.hhi, cell: (r) => fmtScore(r.hhi), num: true, title: 'Herfindahl-Hirschman Index of the optimized supply: 1 = one supplier' },
    { id: 'hosts', header: 'Investment hosts', value: (r) => r.hosts || 'none ranked' },
    { id: 'treatment', header: 'Recommended treatment', value: (r) => r.treatment, minWidth: 220 },
    { id: 'quality', header: 'Data quality', value: (r) => r.quality },
    { id: 'flags', header: 'Flags', value: (r) => r.flags || 'none', minWidth: 200 },
  ];
  const csv = () => ({ name: `${c.id} node by node`, text: csvWithRun(rows.map((r) => ({ compact: c.id, ...nodeCsvRow(r) })), meta) });
  return (
    <Section
      id="nodes"
      title="Node-by-node structure"
      intro={
        <p>
          {mixed
            ? `The nodes of this compact get ${mix.length} different treatments (${listWords(mix.map((m) => `${m.n} ${classInfo(m.cls).label}`))}): one compact combines different instruments node by node.`
            : `All ${plural(nodes.length, 'node')} of this compact ${nodes.length === 1 ? 'is' : 'are'} classified ${classInfo(mix[0]?.cls ?? '').label}; in other compacts nodes can get different treatments.`}
        </p>
      }
    >
      <ChartCard
        headingLevel={3}
        title="Each node's coverage and treatment"
        sub="On each track: the gray tick is the shock alone, the hollow dot existing relationships, the blue dot the optimized trade solution; the line between them is the coordination dividend and the empty track to 100% the residual gap."
        legend={
          <Legend
            label="Coverage marks"
            items={[
              { label: 'Shock alone (static)', color: NODE_MARK.static, kind: 'tick' },
              { label: 'Existing relationships', color: NODE_MARK.existing, kind: 'hollow' },
              { label: 'Optimized trade', color: NODE_MARK.optimized, kind: 'dot' },
            ]}
          />
        }
        table={() => <TableView rows={rows} cols={cols} caption={`${c.name}: node by node`} rowKey={(r) => r.id} pageSize={50} />}
        csv={csv}
      >
        <ul>
          {nodes.map((n) => (
            <NodeItem key={n.id} n={n} />
          ))}
        </ul>
      </ChartCard>
      {frontier && <FrontierCard />}
    </Section>
  );
}

export function FrontierCard() {
  const { c, nodes, meta, names, name } = useCompact();
  const withFrontier = nodesWithFrontier(nodes);
  const [pick, setPick] = useState('');
  const node = withFrontier.find((n) => n.id === pick) ?? withFrontier[0] ?? null;
  const rows = node ? frontierRows(node, names) : [];
  const required = node && typeof node.minRequiredDiversification === 'number' ? node.minRequiredDiversification : null;
  const costless = node && typeof node.tightestCostless === 'number' ? node.tightestCostless : null;
  const readings = node ? frontierReadings(node) : { costless: null, required: null };
  const acceptable = `${Math.round(ACCEPTABLE_COVERAGE * 100)}%`;
  const cols: Col<FrontierRow>[] = [
    { id: 'cap', header: 'Supplier-share limit', value: (r) => r.cap, cell: (r) => capLabel(r.cap), num: true },
    { id: 'coverage', header: 'Coverage', value: (r) => r.coverage, cell: (r) => fmtShare(r.coverage), num: true },
    { id: 'secondShock', header: 'Coverage if the largest supplier is also lost', value: (r) => r.secondShock, cell: (r) => fmtShare(r.secondShock), num: true },
    { id: 'specialization', header: 'Specialization score', value: (r) => r.specialization, cell: (r) => fmtScore(r.specialization), num: true },
    { id: 'largest', header: 'Largest optimized supplier', value: (r) => r.largestName },
    { id: 'largestShare', header: 'Its share', value: (r) => r.largestShare, cell: (r) => fmtShare(r.largestShare), num: true },
  ];
  const csv = () => ({
    name: `${c.id} specialization and resilience frontier`,
    text: csvWithRun(
      nodes.flatMap((n) =>
        frontierRows(n, names).map((r) => ({
          compact: c.id,
          node_id: n.id,
          sub_sector: n.label,
          max_supplier_share: r.cap,
          coverage: r.coverage,
          second_shock_coverage: r.secondShock,
          specialization_score: r.specialization,
          largest_supplier: r.largest ? name(r.largest) : null,
          largest_share: r.largestShare,
          tightest_costless_max_share: n.tightestCostless ?? null,
          min_required_diversification: n.minRequiredDiversification ?? null,
        })),
      ),
      meta,
    ),
  });
  const summary = node
    ? `${node.label}: ${rows.map((r) => `limit ${capLabel(r.cap)}: coverage ${fmtShare(r.coverage)}, after a second shock ${fmtShare(r.secondShock)}`).join('; ')}.`
    : 'No node of this compact has a supplier-limit frontier.';
  return (
    <ChartCard
      headingLevel={3}
      title="Specialization and resilience by supplier limit"
      sub="Each limit re-solves the node with no exporter allowed more than that share of the supply. Coverage is the share of demand served; the second line is the coverage if the solution then also lost its largest supplier; the specialization score (0 to 1) is how much of the supply comes from exporters with a revealed comparative advantage in the node. Tighter limits buy resilience at some cost in coverage and specialization."
      tools={
        withFrontier.length > 1 ? (
          <Select label="Sub-sector" hideLabel value={node?.id ?? ''} options={withFrontier.map((n) => ({ value: n.id, label: n.label }))} onChange={setPick} className="min-w-[14rem]" />
        ) : undefined
      }
      summary={summary}
      legend={
        <Legend
          label="Frontier lines"
          items={[
            ...FRONTIER_SERIES.filter((s) => rows.some((r) => r[s.key] !== null)).map((s) => ({ label: s.label, color: s.color, kind: 'line' as const, dash: s.dash })),
            { label: `${acceptable} coverage after a second shock`, color: 'rgb(255 255 255 / 0.4)', kind: 'line' as const, dash: '2,3' },
            ...(required !== null ? [{ label: 'Least diversification for resilience', color: REQUIRED_COLOR, kind: 'tick' as const }] : []),
            ...(costless !== null ? [{ label: 'Tightest costless limit', color: COSTLESS_COLOR, kind: 'tick' as const }] : []),
          ]}
        />
      }
      notes={
        node ? (
          <span>
            {node.label}: the tightest limit that costs at most 2 points of coverage is {readings.costless ?? 'not in this export'}; the loosest limit that keeps coverage at {acceptable} or
            more after a second shock is {readings.required ?? 'not in this export'}.
          </span>
        ) : undefined
      }
      table={() => <TableView rows={rows} cols={cols} caption={`${node?.label ?? c.name}: specialization and resilience by supplier limit`} />}
      csv={csv}
    >
      {node && rows.length ? (
        <FrontierChart rows={rows} required={required} costless={costless} ariaLabel={summary} />
      ) : (
        <EmptyState>None of this compact&apos;s nodes has a supplier-limit frontier in this export.</EmptyState>
      )}
    </ChartCard>
  );
}
