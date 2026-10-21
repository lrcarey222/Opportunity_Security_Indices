// 3. Node-by-node structure: one row per node of the compact with its class, its multi-label policy needs
// and any physical-capability warning (visible, with the physical validation in words), its coverage
// track (static shock, existing relationships, optimized trade), the numbers, the optimized largest
// supplier, the N-1 readings, the investment hosts, the recommended treatment, the class under the other
// variants (trade-only vs production-constrained supply, release semantics, the conservative scenario,
// the other shocks) and the existing-relationship growth-bound sensitivity (flagged, from the headline
// class, when the node is COORDINATE under some bounds only), each class with its own physical-capability
// tag, its place in the diversification scenario ("Diversification scenario", worded per status; nothing
// when the export has none), and its data quality (flags in a Disclosure). Nodes in one compact can get
// different treatments; the card says how many. Table twin and CSV. Then the specialization-resilience frontier of one node at a
// time (a node selector): coverage, N-1 re-optimized, instantaneous N-1 and the specialization score
// under each supplier-share limit, with the two readings marked. Table twin and CSV (every node of the
// compact).
//
// The scenario toggle (2026-10-19): each node opens with its allied demand under the three scenarios
// (Baseline, Managed Interdependence, Decoupling: kept / restored by trade / new capacity, the toggled one
// highlighted; NodeScenarioRows) in place of the coverage track, then its figures under the toggled
// scenario (Managed Interdependence: the share shifted, its class, largest supplier and hosts; the readings
// computed under Decoupling only, N-1, redirection, the class by supply constraint and the frontier, stay
// with Decoupling) and its suppliers today (node.baseline). The Managed Interdependence block keeps what
// the rows do not show (its benchmark, cap, capped suppliers, class, hosts and relationships). The table
// and the CSV carry the scenario's columns; the frontier says it is computed under Decoupling.

import { useState } from 'react';
import { ChartCard } from '../../../charts/ChartCard.tsx';
import { Legend } from '../../../charts/legends.tsx';
import { TableView, type Col } from '../../../components/TableView.tsx';
import { Disclosure, EmptyState, Select } from '../../../components/ui.tsx';
import { AIB_PHYSICAL_WARNING, diversifyShockIdOf, type AibNode } from '../../contract.ts';
import { ClassChip, FlaggedClass, PhysicalWarning } from '../../charts/ClassChip.tsx';
import { NODE_MARK, NodeCoverageBar } from '../../charts/NodeCoverageBar.tsx';
import { CoverageLegend, ScenarioTag } from '../../charts/ScenarioCompare.tsx';
import { ACCEPTABLE_COVERAGE, COSTLESS_COLOR, FrontierChart, REQUIRED_COLOR } from '../../charts/FrontierChart.tsx';
import { capLabel, FRONTIER_SERIES, frontierReadings, frontierRows, hasReoptimizedN1, INSTANTANEOUS_N1_LABEL, N1_REOPTIMIZED_LABEL, nodesWithFrontier, type FrontierRow } from '../../derive/frontier.ts';
import { classInfo, classMix, policyNeedsText } from '../../derive/classes.ts';
import { fmtPp, fmtScore, fmtShare, fmtUsd, listWords, plural } from '../../derive/format.ts';
import { screenedHostNote } from '../../derive/investment.ts';
import { nodeCsvRow, nodeRow } from '../../derive/nodes.ts';
import { nodeScenarioCsv, nodeScenarioNote } from '../../derive/scenarioPages.ts';
import { nodeScenario, nodeSuppliersToday, scenarioCsvFields } from '../../derive/scenarios.ts';
import { classVariants, constraintClasses, growthFlag, growthRows, robustnessSummary, variantWarning } from '../../derive/sensitivity.ts';
import { nodeWhere } from '../../derive/taxonomy.ts';
import { csvWithRun, Section } from '../common.tsx';
import { DiversifyBlock, NodeScenarioRows } from '../DiversifyBlock.tsx';
import { NodeRobustness } from '../NodeRobustness.tsx';
import { Economy, useCompact } from './shared.tsx';

/** A class with the warning named after it in a plain-text cell ("COORDINATE (PHYSICAL CAPABILITY NOT VALIDATED)"). */
const clsText = (cls: string | null, warning: boolean | null) => (cls ? `${classInfo(cls).label}${warning ? ` (${AIB_PHYSICAL_WARNING})` : ''}` : null);

function NodeItem({ n }: { n: AibNode }) {
  const { name, names, meta, tx, scenario, hasScenarios } = useCompact();
  const mi = scenario === 'managed';
  const r = nodeScenario(n, 'managed');
  const miWarning = mi && r.ran && r.cls ? variantWarning(n, diversifyShockIdOf(meta)) : null;
  const readings = frontierReadings(n);
  const modes = constraintClasses(n);
  const flag = growthFlag(n);
  const today = hasScenarios ? nodeSuppliersToday(n) : [];
  const coverageWords = `${n.label}: shock alone ${fmtShare(n.static)}, existing relationships ${fmtShare(n.existing)}, optimized trade ${fmtShare(n.optimized)} of demand served`;
  // the host types (2026-10-20): an older bundle has the one ranked list only
  const typed = n.specialtyHosts !== undefined || n.developmentHosts !== undefined;
  const typedHosts = (xs: readonly string[] | undefined) => (xs?.length ? listWords(xs.map((h) => name(h))) : 'none');
  return (
    <li className="border-t hairline py-4 first:border-t-0 first:pt-0" data-node={n.id}>
      <div className="flex flex-wrap items-center gap-x-3 gap-y-1.5">
        {mi ? (
          r.ran && r.cls ? (
            <span className="inline-flex flex-wrap items-center gap-x-2 gap-y-1">
              <ClassChip cls={r.cls} />
              {miWarning && <PhysicalWarning warning={AIB_PHYSICAL_WARNING} testId="variant-physical-warning" />}
            </span>
          ) : (
            <span className="tag text-paper-mute">Unchanged</span>
          )
        ) : (
          <ClassChip cls={n.cls} needs={n.policyNeeds} warning={n.warning} />
        )}
        <h4 className="text-base font-medium text-paper">{n.label}</h4>
        <span className="text-xs text-paper-mute">{nodeWhere(tx, n)}</span>
      </div>
      {!mi && (n.policyNeeds !== undefined || n.warning) && (
        <div className="mt-1.5 flex flex-wrap items-center gap-x-3 gap-y-1">
          {n.policyNeeds !== undefined && (
            <span className="text-xs text-paper-dim" data-testid="policy-needs">
              <span className="text-paper-mute">Policy needs:</span> {policyNeedsText(n.policyNeeds)}
            </span>
          )}
          {n.warning && (
            <>
              <PhysicalWarning warning={n.warning} validation={n.physicalValidation} />
              {n.physicalValidation && <span className="text-xs text-paper-mute">{n.physicalValidation}</span>}
            </>
          )}
        </div>
      )}
      {!mi && flag && (
        <p className="mt-1.5 border-l-2 border-l-signal pl-2 text-xs text-paper-dim" data-testid="growth-flag">
          {flag}
        </p>
      )}
      {hasScenarios && (
        <div className="mt-3">
          <NodeScenarioRows n={n} highlight={scenario} meta={meta} />
        </div>
      )}
      <div className="mt-3 grid gap-x-8 gap-y-3 lg:grid-cols-2">
        <div>
          {!hasScenarios && <NodeCoverageBar stat={n.static} existing={n.existing} optimized={n.optimized} label={coverageWords} />}
          <p className={`${hasScenarios ? '' : 'mt-1.5 '}text-xs leading-relaxed text-paper-dim tabular`} data-testid="node-figures">
            {mi ? (
              r.ran ? (
                <>
                  Share shifted {fmtShare(r.exposure)} ({fmtUsd(r.lostUsd)} of {fmtUsd(r.demandUsd)}) · existing {fmtShare(r.existing)} &rarr; optimized {fmtShare(r.optimized)} (dividend{' '}
                  {fmtPp(r.dividend)}) · residual gap {fmtShare(r.residual)}
                </>
              ) : (
                <>Under Managed Interdependence: {nodeScenarioNote(r) ?? 'unchanged'}.</>
              )
            ) : (
              <>
                Shock exposure {fmtShare(n.exposure)} ({fmtUsd(n.lostUsd)} of {fmtUsd(n.demandUsd)}) · existing {fmtShare(n.existing)} &rarr; optimized {fmtShare(n.optimized)} (dividend{' '}
                {fmtPp(n.dividend)}) · residual gap {fmtShare(n.residual)}
              </>
            )}
          </p>
          {today.length > 0 && (
            <p className="mt-1.5 text-xs leading-relaxed text-paper-dim" data-testid="node-suppliers-today">
              <span className="text-paper-mute">Suppliers today:</span>{' '}
              {today.map((t, i) => (
                <span key={t.iso3}>
                  {i > 0 && ', '}
                  <Economy iso3={t.iso3} /> {fmtShare(t.share, 0)}
                </span>
              ))}{' '}
              <span className="text-paper-mute">(shares of its allied demand, before any shock)</span>
            </p>
          )}
        </div>
        <dl className="grid grid-cols-[minmax(0,11rem)_minmax(0,1fr)] gap-x-3 gap-y-1 text-xs">
          <dt className="text-paper-mute">Largest optimized supplier</dt>
          <dd className="text-paper-dim">
            {mi && r.ran ? (
              r.largest ? (
                <>
                  {/* the cap is a share of the allied demand; largestShare is of the supply served: print both bases */}
                  <Economy iso3={r.largest} /> ({fmtShare(typeof r.largestShare === 'number' && typeof r.optimized === 'number' ? r.largestShare * r.optimized : null)} of the allied
                  demand under the cap, {fmtShare(r.largestShare)} of the supply served; today <Economy iso3={n.baseline?.largest ?? n.baseLargest} />{' '}
                  {fmtShare(n.baseline ? n.baseline.largestShare : n.baseLargestShare)})
                </>
              ) : (
                '–'
              )
            ) : mi ? (
              <>
                Today <Economy iso3={n.baseline?.largest ?? n.baseLargest} /> {fmtShare(n.baseline ? n.baseline.largestShare : n.baseLargestShare)} (unchanged)
              </>
            ) : n.largest ? (
              <>
                <Economy iso3={n.largest} /> ({fmtShare(n.largestShare)}; before the shock <Economy iso3={n.baseLargest} /> {fmtShare(n.baseLargestShare)})
              </>
            ) : (
              '–'
            )}
          </dd>
          {!mi && (n.n1Reoptimized !== undefined || n.instantaneousN1 !== undefined) && (
            <>
              <dt className="text-paper-mute">If its largest supplier is lost</dt>
              <dd className="text-paper-dim tabular" data-testid="node-n1">
                N-1 re-optimized {fmtShare(n.n1Reoptimized)} (that supplier removed, trade re-optimized); instantaneous N-1 {fmtShare(n.instantaneousN1)} (coverage × (1 − largest share))
              </dd>
            </>
          )}
          {!mi && n.diversion !== undefined && (
            <>
              <dt className="text-paper-mute">Redirected from other markets</dt>
              <dd className="text-paper-dim">{fmtShare(n.diversion)} of demand, served by exports pulled from markets outside the demand scope</dd>
            </>
          )}
          <dt className="text-paper-mute">{typed ? 'Scaling hosts' : 'Investment hosts'}</dt>
          <dd className="text-paper-dim">
            {mi ? (r.ran ? ((n.diversify?.hosts.length ?? 0) ? listWords(n.diversify!.hosts.map((h) => name(h))) : 'none ranked') : 'none: nothing to replace') : n.hosts.length ? listWords(n.hosts.map((h) => name(h))) : 'none ranked'}
          </dd>
          {typed && (!mi || r.ran) && (
            <>
              <dt className="text-paper-mute">Specialty hosts</dt>
              <dd className="text-paper-dim">{typedHosts(mi ? n.diversify?.specialtyHosts : n.specialtyHosts)}</dd>
              <dt className="text-paper-mute">Development hosts</dt>
              <dd className="text-paper-dim">{typedHosts(mi ? n.diversify?.developmentHosts : n.developmentHosts)}{(mi ? n.diversify?.developmentHosts : n.developmentHosts)?.length ? ' (future capacity, not counted toward the coalition target)' : ''}</dd>
            </>
          )}
          {!mi && n.screenedHost && (
            <>
              <dt className="text-paper-mute">Screened out</dt>
              <dd className="text-paper-dim tabular" data-testid="node-screened-host">
                {screenedHostNote(n, name)} (alignment screen: economies outside the demand scope that vote with the China-leaning UNGA bloc do not host)
              </dd>
            </>
          )}
          {!mi && (
            <>
              <dt className="text-paper-mute">Recommended treatment</dt>
              <dd className="text-paper-dim">{n.treatment ?? '–'}</dd>
              <dt className="text-paper-mute">Why this class</dt>
              <dd className="text-paper-dim">{n.reason ?? '–'}</dd>
            </>
          )}
          {(n.physicalValidation !== undefined || n.physicalData !== undefined) && (
            <>
              <dt className="text-paper-mute">Physical validation</dt>
              <dd className="text-paper-dim" data-testid="physical-validation">
                {n.physicalValidation ?? '–'}
                {n.physicalData && !/physical data/.test(n.physicalValidation ?? '') ? `; physical data ${n.physicalData}` : ''}
                {typeof n.physicalDivergence === 'number' ? `; physical and trade concentration differ by ${fmtPp(Math.abs(n.physicalDivergence)).replace(/^\+/, '')}` : ''}
              </dd>
            </>
          )}
          {!mi && modes && (
            <>
              <dt className="text-paper-mute">Class by supply constraint</dt>
              <dd className="text-paper-dim" data-testid="constraint-classes">
                production-constrained <FlaggedClass cls={modes.productionConstrained} warning={modes.productionConstrainedWarning} />; trade-only{' '}
                <FlaggedClass cls={modes.tradeOnly} warning={modes.tradeOnlyWarning} />
                {modes.differs ? ' (they differ)' : ''}
              </dd>
            </>
          )}
          {!mi && readings.costless !== null && (
            <>
              <dt className="text-paper-mute">Tightest costless supplier limit</dt>
              <dd className="text-paper-dim">{readings.costless}</dd>
            </>
          )}
          {!mi && readings.required !== null && (
            <>
              <dt className="text-paper-mute">Least diversification for resilience</dt>
              <dd className="text-paper-dim">{readings.required}</dd>
            </>
          )}
        </dl>
      </div>
      <DiversifyBlock n={n} meta={meta} names={names} scenarios={hasScenarios} />
      <div className="mt-2">
        {(n.variants !== undefined || n.growth !== undefined) && (
          <Disclosure summary={robustnessSummary(n, meta)}>
            <NodeRobustness n={n} meta={meta} names={names} diversify={false} />
          </Disclosure>
        )}
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
  const { c, nodes, meta, names, tx, scenario, hasScenarios } = useCompact();
  const mi = scenario === 'managed';
  const mix = classMix(nodes.map((n) => n.cls));
  const rows = nodes.map((n) => nodeRow(n, names, tx));
  const mixed = mix.length > 1;
  // the non-headline settings under which each node's class carries the warning (null: the export does not say)
  const warnedElsewhere = new Map(
    nodes.map((n) => {
      const vs = classVariants(n, meta).filter((v) => v.warning === true && !v.headline).map((v) => v.setting);
      const gs = growthRows(n).filter((g) => g.warning === true && !g.headline).map((g) => `growth bound ${g.label}`);
      const known = n.variantWarnings !== undefined || growthRows(n).some((g) => g.warning !== null);
      return [n.id, known ? [...vs, ...gs].join('; ') || 'none' : null] as const;
    }),
  );
  const nodeById = new Map(nodes.map((n) => [n.id, n]));
  // each node under Managed Interdependence (the table's scenario columns)
  const managedById = new Map(nodes.map((n) => [n.id, nodeScenario(n, 'managed')]));
  const mf = (id: string) => managedById.get(id) ?? null;
  const scenarioCols: Col<(typeof rows)[number]>[] = [
    { id: 'miCls', header: 'Class under Managed Interdependence', value: (r) => { const f = mf(r.id); return f?.ran && f.cls ? classInfo(f.cls).label : 'unchanged'; }, minWidth: 160 },
    { id: 'miShift', header: 'Share shifted (Managed Interdependence)', value: (r) => (mf(r.id)?.ran ? mf(r.id)!.exposure : null), cell: (r) => (mf(r.id)?.ran ? fmtShare(mf(r.id)!.exposure) : '–'), num: true },
    { id: 'miOpt', header: 'Optimized coverage (Managed Interdependence)', value: (r) => mf(r.id)?.optimized ?? null, cell: (r) => fmtShare(mf(r.id)?.optimized), num: true },
    { id: 'miGap', header: 'Gap left (Managed Interdependence, USD)', value: (r) => mf(r.id)?.residualUsd ?? null, cell: (r) => fmtUsd(mf(r.id)?.residualUsd), num: true },
    {
      id: 'todayHhi',
      header: "Today's concentration (HHI) of supply to the allied market",
      value: (r) => nodeById.get(r.id)?.baseline?.hhi ?? null,
      cell: (r) => fmtScore(nodeById.get(r.id)?.baseline?.hhi ?? null),
      num: true,
      title:
        "Herfindahl-Hirschman Index of the node's supply to the allied market today: 1 = one supplier. Managed Interdependence tests a different measure, the concentration of world exporters, against the benchmark.",
    },
  ];
  const cols: Col<(typeof rows)[number]>[] = [
    { id: 'node', header: 'Sub-sector', value: (r) => r.node },
    ...(tx.dictionary ? [{ id: 'sector', header: tx.One, value: (r: (typeof rows)[number]) => r.technologies }] : []),
    { id: 'stage', header: 'Stage', value: (r) => r.stage },
    { id: 'cls', header: hasScenarios ? 'Class under Decoupling' : 'Class', value: (r) => classInfo(r.cls).label },
    { id: 'needs', header: 'Policy needs', value: (r) => r.policyNeeds },
    { id: 'warning', header: 'Warning', value: (r) => r.warning ?? 'none', minWidth: 200 },
    { id: 'exposure', header: 'Shock exposure', value: (r) => r.exposure, cell: (r) => fmtShare(r.exposure), num: true },
    { id: 'existing', header: 'Existing coverage', value: (r) => r.existing, cell: (r) => fmtShare(r.existing), num: true },
    { id: 'optimized', header: 'Optimized coverage', value: (r) => r.optimized, cell: (r) => fmtShare(r.optimized), num: true },
    { id: 'dividend', header: 'Coordination dividend', value: (r) => r.dividend, cell: (r) => fmtPp(r.dividend), num: true },
    { id: 'residual', header: 'Residual gap', value: (r) => r.residual, cell: (r) => fmtShare(r.residual), num: true },
    { id: 'largest', header: 'Largest optimized supplier', value: (r) => r.optimizedLargest },
    { id: 'largestShare', header: 'Its share', value: (r) => r.optimizedLargestShare, cell: (r) => fmtShare(r.optimizedLargestShare), num: true },
    { id: 'hhi', header: 'Concentration (HHI)', value: (r) => r.hhi, cell: (r) => fmtScore(r.hhi), num: true, title: 'Herfindahl-Hirschman Index of the optimized supply: 1 = one supplier' },
    { id: 'n1', header: 'N-1 re-optimized', value: (r) => r.n1Reoptimized, cell: (r) => fmtShare(r.n1Reoptimized), num: true, title: N1_REOPTIMIZED_LABEL },
    { id: 'n1i', header: 'Instantaneous N-1', value: (r) => r.instantaneousN1, cell: (r) => fmtShare(r.instantaneousN1), num: true, title: INSTANTANEOUS_N1_LABEL },
    { id: 'tradeOnly', header: 'Class, trade-only', value: (r) => clsText(r.tradeOnlyCls, r.tradeOnlyWarning), minWidth: 160 },
    { id: 'warnedVariants', header: 'Warning under other settings', value: (r) => warnedElsewhere.get(r.id) ?? null, minWidth: 200 },
    { id: 'growth', header: 'Class depends on the growth bound', value: (r) => r.growthFlag ?? (r.coordinateSensitiveToGrowth === null ? null : 'no'), minWidth: 220 },
    { id: 'physical', header: 'Physical validation', value: (r) => r.physicalValidation, minWidth: 220 },
    { id: 'hosts', header: 'Scaling hosts', value: (r) => r.hosts || 'none ranked' },
    { id: 'specialtyHosts', header: 'Specialty hosts', value: (r) => r.specialtyHosts || 'none' },
    { id: 'developmentHosts', header: 'Development hosts', value: (r) => r.developmentHosts || 'none' },
    { id: 'treatment', header: 'Recommended treatment', value: (r) => r.treatment, minWidth: 220 },
    { id: 'quality', header: 'Data quality', value: (r) => r.quality },
    { id: 'flags', header: 'Flags', value: (r) => r.flags || 'none', minWidth: 200 },
    ...(hasScenarios ? scenarioCols : []),
  ];
  const csv = () => ({
    name: `${c.id} node by node`,
    text: csvWithRun(
      rows.map((r) => {
        const n = nodeById.get(r.id);
        // the unprefixed columns are Decoupling's (aib_scenario names it); baseline_* and managed_* name their scenario
        return { compact: c.id, ...nodeCsvRow(r), ...(hasScenarios && n ? { ...scenarioCsvFields(meta, 'decoupling'), ...nodeScenarioCsv(n) } : {}) };
      }),
      meta,
    ),
  });
  const warned = nodes.filter((n) => n.warning);
  return (
    <Section
      id="nodes"
      title="Node-by-node structure"
      intro={
        <p>
          {mixed
            ? `The nodes of this compact get ${mix.length} different treatments (${listWords(mix.map((m) => `${m.n} ${classInfo(m.cls).label}`))}): one compact combines different instruments node by node.`
            : `All ${plural(nodes.length, 'node')} of this compact ${nodes.length === 1 ? 'is' : 'are'} classified ${classInfo(mix[0]?.cls ?? '').label}; in other compacts nodes can get different treatments.`}{' '}
          Each node has one headline class; its policy needs can name several instruments at once.
          {warned.length ? ` The class of ${listWords(warned.map((n) => n.label))} carries a physical-capability warning.` : ''}
        </p>
      }
    >
      <ChartCard
        headingLevel={3}
        title="Each node's coverage and treatment"
        badge={hasScenarios ? <ScenarioTag id={scenario} meta={meta} /> : undefined}
        sub={
          hasScenarios
            ? `Each node's allied demand under the three scenarios (Baseline is trade as it is today): what the market keeps, what existing relationships and optimized trade restore, and what only new capacity fills; the scenario chosen above is highlighted and the figures below each node are ${mi ? 'Managed Interdependence' : 'Decoupling'}'s.`
            : 'On each track: the gray tick is the shock alone, the hollow dot existing relationships, the blue dot the optimized trade solution; the line between them is the coordination dividend and the empty track to 100% the residual gap.'
        }
        legend={
          hasScenarios ? (
            <CoverageLegend />
          ) : (
            <Legend
              label="Coverage marks"
              items={[
                { label: 'Shock alone (static)', color: NODE_MARK.static, kind: 'tick' },
                { label: 'Existing relationships', color: NODE_MARK.existing, kind: 'hollow' },
                { label: 'Optimized trade', color: NODE_MARK.optimized, kind: 'dot' },
              ]}
            />
          )
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
  const { c, nodes, meta, names, name, scenario, hasScenarios } = useCompact();
  const withFrontier = nodesWithFrontier(nodes);
  const [pick, setPick] = useState('');
  const node = withFrontier.find((n) => n.id === pick) ?? withFrontier[0] ?? null;
  const rows = node ? frontierRows(node, names) : [];
  const required = node && typeof node.minRequiredDiversification === 'number' ? node.minRequiredDiversification : null;
  const costless = node && typeof node.tightestCostless === 'number' ? node.tightestCostless : null;
  const readings = node ? frontierReadings(node) : { costless: null, required: null };
  const acceptable = `${Math.round(ACCEPTABLE_COVERAGE * 100)}%`;
  // the reading uses the true N-1 test where the export has it (an older bundle: the instantaneous one)
  const n1Words = node && !hasReoptimizedN1(node) ? 'the instantaneous N-1 reading' : 'the N-1 test (largest remaining supplier removed, trade re-optimized)';
  const cols: Col<FrontierRow>[] = [
    { id: 'cap', header: 'Supplier-share limit', value: (r) => r.cap, cell: (r) => capLabel(r.cap), num: true },
    { id: 'coverage', header: 'Coverage', value: (r) => r.coverage, cell: (r) => fmtShare(r.coverage), num: true },
    { id: 'n1Reoptimized', header: N1_REOPTIMIZED_LABEL, value: (r) => r.n1Reoptimized, cell: (r) => fmtShare(r.n1Reoptimized), num: true },
    { id: 'n1Removed', header: 'Supplier removed', value: (r) => r.n1RemovedName },
    { id: 'instantaneousN1', header: INSTANTANEOUS_N1_LABEL, value: (r) => r.instantaneousN1, cell: (r) => fmtShare(r.instantaneousN1), num: true },
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
          n1_reoptimized_coverage: r.n1Reoptimized,
          n1_removed_supplier: r.n1Removed ? name(r.n1Removed) : null,
          instantaneous_n1_retained_coverage: r.instantaneousN1,
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
    ? `${node.label}: ${rows.map((r) => `limit ${capLabel(r.cap)}: coverage ${fmtShare(r.coverage)}, N-1 re-optimized ${fmtShare(r.n1Reoptimized)}, instantaneous N-1 ${fmtShare(r.instantaneousN1)}`).join('; ')}.`
    : 'No node of this compact has a supplier-limit frontier.';
  return (
    <ChartCard
      headingLevel={3}
      title="Specialization and resilience by supplier limit"
      badge={hasScenarios && scenario === 'managed' ? <ScenarioTag id="decoupling" meta={meta} /> : undefined}
      sub="Each limit re-solves the node with no exporter allowed more than that share of the supply. Coverage is the share of demand served. Two readings of resilience follow: N-1 re-optimized removes the largest remaining supplier and re-optimizes the trade (the true N-1 test); instantaneous N-1 retained coverage is mechanical, coverage × (1 − the largest supplier's share), with nothing re-routed. The specialization score (0 to 1) is how much of the supply comes from exporters with a revealed comparative advantage in the node. Tighter limits buy resilience at some cost in coverage and specialization."
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
            { label: `${acceptable} coverage after the N-1 test`, color: 'rgb(255 255 255 / 0.4)', kind: 'line' as const, dash: '2,3' },
            ...(required !== null ? [{ label: 'Least diversification for resilience', color: REQUIRED_COLOR, kind: 'tick' as const }] : []),
            ...(costless !== null ? [{ label: 'Tightest costless limit', color: COSTLESS_COLOR, kind: 'tick' as const }] : []),
          ]}
        />
      }
      notes={
        node ? (
          <span>
            {hasScenarios && scenario === 'managed' ? 'Computed under Decoupling only. ' : ''}
            {node.label}: the tightest limit that costs at most 2 points of coverage is {readings.costless ?? 'not in this export'}; the loosest limit that keeps coverage at {acceptable} or
            more after {n1Words} is {readings.required ?? 'not in this export'}.
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
