// 5. Coverage comparison (shock -> existing relationships -> trade compact -> trade + investment, in %
// of demand, with the global optimum, "if every economy coordinated", as a reference rule) and 6. How resilience changes as countries join (the greedy coalition path against the
// 90 / 95 / 99% targets). Table twins and CSVs.

import { ChartCard } from '../../../charts/ChartCard.tsx';
import { Legend } from '../../../charts/legends.tsx';
import { TableView, type Col } from '../../../components/TableView.tsx';
import { EmptyState } from '../../../components/ui.tsx';
import { CoalitionChart, CURVE_COLOR } from '../../charts/CoalitionChart.tsx';
import { CoverageBars, REFERENCE_COLOR } from '../../charts/CoverageBars.tsx';
import { STEP_COLOR } from '../../charts/CoverageStrip.tsx';
import { coalitionCurve, targetLabel, targetMarks, type CurvePoint } from '../../derive/coalition.ts';
import { coverageSteps, GLOBAL_OPTIMUM_LABEL, globalOptimum, largestGain, type CoverageStep } from '../../derive/coverage.ts';
import { fmtPp, fmtShare, listWords, plural } from '../../derive/format.ts';
import { csvWithRun, Section } from '../common.tsx';
import { useCompact } from './shared.tsx';

export function CoverageSection() {
  const { c, meta } = useCompact();
  const steps = coverageSteps(c.metrics);
  const gain = largestGain(steps);
  const optimum = globalOptimum(c.metrics);
  const rows = [
    ...steps.map((s, i) => ({ ...s, change: i > 0 && s.value !== null && steps[i - 1].value !== null ? (s.value as number) - (steps[i - 1].value as number) : null })),
    ...(optimum !== null
      ? [{ key: 'global', label: `${GLOBAL_OPTIMUM_LABEL} (reference)`, short: 'Every economy', meaning: 'every economy in the universe coordinates its trade: the global optimum, not a compact', value: optimum, change: null }]
      : []),
  ];
  const cols: Col<(typeof rows)[number]>[] = [
    { id: 'label', header: 'Step', value: (r) => r.label },
    { id: 'value', header: 'Demand served', value: (r) => r.value, cell: (r) => fmtShare(r.value), num: true },
    { id: 'change', header: 'Change on the step before', value: (r) => r.change, cell: (r) => (r.change === null ? '–' : fmtPp(r.change)), num: true },
    { id: 'meaning', header: 'What it assumes', value: (r) => r.meaning, minWidth: 280 },
  ];
  const csv = () => ({
    name: `${c.id} coverage comparison`,
    text: csvWithRun(
      rows.map((r) => ({ compact: c.id, step: r.label, demand_served: r.value, change_on_step_before: r.change, assumption: r.meaning })),
      meta,
    ),
  });
  const summary = `${c.name}: ${steps.map((s: CoverageStep) => `${s.label} ${fmtShare(s.value)}`).join(', ')} of demand served${optimum !== null ? `; ${GLOBAL_OPTIMUM_LABEL.toLowerCase()}: ${fmtShare(optimum)}` : ''}.`;
  return (
    <Section
      id="coverage"
      title="Coverage comparison"
      intro={
        <p>
          How much of the compact&apos;s baseline traded demand is served after the shock, from the shock alone to the full response. Values are demand-weighted over the compact&apos;s nodes.
          {gain && gain.gain > 0 ? ` The largest step is from ${gain.from.label.toLowerCase()} to ${gain.to.label.toLowerCase()} (${fmtPp(gain.gain)}).` : ''} The trade compact is
          every economy&apos;s existing-relationship response plus the new and expanded relationships of the compact&apos;s minimum-coalition members, so it never falls below
          existing relationships
          {optimum !== null ? `; the dashed line is what coordination among every economy would reach (${fmtShare(optimum)}), a reference rather than a step` : ''}.
        </p>
      }
    >
      <ChartCard
        headingLevel={3}
        title="Demand served at each step"
        sub={`In % of the compact's demand. Gray: the shock alone; blue: the responses. The figure above each column is the change on the step before.${optimum !== null ? ' Dashed line: if every economy coordinated.' : ''}`}
        summary={summary}
        legend={
          <Legend
            label="Coverage steps"
            items={[
              { label: 'Shock alone', color: STEP_COLOR.static },
              { label: 'Responses', color: STEP_COLOR.response },
              ...(optimum !== null ? [{ label: GLOBAL_OPTIMUM_LABEL, color: REFERENCE_COLOR, kind: 'line' as const, dash: '6,4' }] : []),
            ]}
          />
        }
        table={() => <TableView rows={rows} cols={cols} caption={`${c.name}: coverage at each step`} />}
        csv={csv}
      >
        <CoverageBars steps={steps} ariaLabel={summary} reference={optimum} referenceLabel={GLOBAL_OPTIMUM_LABEL} />
      </ChartCard>
    </Section>
  );
}

export function CoalitionSection() {
  const { c, meta, names, name, nodeLabel } = useCompact();
  const points = coalitionCurve(c, names);
  const marks = targetMarks(points, meta.targets);
  const cols: Col<CurvePoint & { reaches: string }>[] = [
    { id: 'step', header: 'Step', value: (p) => p.step, num: true },
    { id: 'n', header: 'Members', value: (p) => p.n, num: true },
    { id: 'added', header: 'Economy added', value: (p) => p.addedName },
    { id: 'min', header: 'Minimum coverage', value: (p) => p.min, cell: (p) => fmtShare(p.min), num: true },
    { id: 'mean', header: 'Mean coverage', value: (p) => p.mean, cell: (p) => fmtShare(p.mean), num: true },
    { id: 'reaches', header: 'First to reach', value: (p) => p.reaches || null },
  ];
  const rows = points.map((p) => ({
    ...p,
    reaches: marks
      .filter((m) => m.point?.step === p.step)
      .map((m) => `${targetLabel(m.target)} target`)
      .join(', '),
  }));
  const nodeIds = [...new Set(points.flatMap((p) => Object.keys(p.nodes)))];
  const csv = () => ({
    name: `${c.id} coalition path`,
    text: csvWithRun(
      rows.map((p) => ({
        compact: c.id,
        step: p.step,
        members_count: p.n,
        added_iso3: p.added,
        added: p.added ? p.addedName : null,
        members: p.members.join(' '),
        min_coverage: p.min,
        mean_coverage: p.mean,
        first_to_reach: p.reaches || null,
        ...Object.fromEntries(nodeIds.map((id) => [`coverage_${id}`, p.nodes[id] ?? null])),
      })),
      meta,
    ),
  });
  const coalitions: { t: number; list: string[] }[] = [];
  const lists = [c.coalition.t90, c.coalition.t95, c.coalition.t99];
  [...meta.targets].sort((a, b) => a - b).forEach((t, i) => coalitions.push({ t, list: lists[i] ?? [] }));
  const summary = `${c.name}: minimum coverage of the anchors' demand from ${fmtShare(points[0]?.min ?? null)} with the demand anchors to ${fmtShare(points[points.length - 1]?.min ?? null)} with ${plural(points[points.length - 1]?.n ?? 0, 'member')}.`;
  return (
    <Section
      id="coalition"
      title="How resilience changes as countries join"
      intro={
        <p>
          Starting from the demand anchors (the economies that lose most of the compact&apos;s supply), the model adds one economy at a time, the one that most reduces the shortfall, and
          measures the coverage of the anchors&apos; demand in every node. The weakest node sets the minimum. Fewer members means less coordination to organise.
        </p>
      }
    >
      <ChartCard
        headingLevel={3}
        title="Coverage as members join"
        sub="Across: members in the coalition (each tick names the economy added at that step; anchors for the start). Up: coverage of the anchors' demand, the minimum over the nodes (solid) and the mean (dashed). Dotted rules: the coverage targets; a ring marks the first coalition to reach each."
        summary={summary}
        legend={
          <Legend
            label="Coalition path"
            items={[
              { label: 'Minimum coverage over the nodes', color: CURVE_COLOR.min, kind: 'line' },
              { label: 'Mean coverage', color: CURVE_COLOR.mean, kind: 'line', dash: '5,4' },
              { label: 'Coverage target', color: CURVE_COLOR.target, kind: 'line', dash: '2,3' },
              { label: 'First coalition to reach a target', color: '#ffffff', kind: 'hollow' },
            ]}
          />
        }
        table={() => <TableView rows={rows} cols={cols} caption={`${c.name}: the coalition path`} pageSize={50} />}
        csv={csv}
        notes={
          nodeIds.length ? (
            <span>Per-node coverage at each step is in the CSV ({listWords(nodeIds.map((id) => nodeLabel(id)))}).</span>
          ) : undefined
        }
      >
        {points.length ? (
          <CoalitionChart points={points} marks={marks} ariaLabel={summary} />
        ) : (
          <EmptyState>The export carries no coalition path for this compact.</EmptyState>
        )}
        <ul className="mt-4 grid gap-2 text-sm sm:grid-cols-3">
          {coalitions.map(({ t, list }) => (
            <li key={t} className="panel px-3 py-2">
              <span className="eyebrow block">{targetLabel(t)} coalition</span>
              <span className="mt-1 block text-paper-dim">
                {list.length ? `${plural(list.length, 'member')}: ${listWords(list.map((x) => name(x)))}` : 'not reached'}
              </span>
            </li>
          ))}
        </ul>
        {marks.some((m) => m.point?.step === 0) && (
          <p className="mt-2 text-xs text-paper-mute">
            The demand anchors alone already reach the{' '}
            {listWords(marks.filter((m) => m.point?.step === 0).map((m) => targetLabel(m.target)))} {marks.filter((m) => m.point?.step === 0).length === 1 ? 'target' : 'targets'}: the
            economies that lose the supply can restore it among themselves.
          </p>
        )}
        {!c.coalition.reached99 && (
          <p className="mt-2 text-xs text-paper-mute">
            The highest target is not reached: the best coalition the model found serves {fmtShare(c.coalition.maxCoverage)} of the anchors&apos; demand in its weakest node.
          </p>
        )}
      </ChartCard>
    </Section>
  );
}
