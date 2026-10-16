// 1. Compact overview (the included nodes with their classes; the archetype with the class mix of the
//    lost supply and the secondary class; the type, "compact unnecessary" muted with its explanation;
//    the policy needs; the country x role matrix) and 2. Why it exists (shock exposure, compact vs
//    fragmented action for the demand anchors, coordination dividend, residual gaps, concentration).
//    Each card has a table twin and a CSV.

import { ChartCard } from '../../../charts/ChartCard.tsx';
import { TableView, type Col } from '../../../components/TableView.tsx';
import { EmptyState, Notice, StatTile, Tag } from '../../../components/ui.tsx';
import { ClassChip, ClassLegend, ClassSwatch } from '../../charts/ClassChip.tsx';
import { TypeTag } from '../../charts/TypeTag.tsx';
import { classInfo, classMix, parseClassMix } from '../../derive/classes.ts';
import { compactVsFragmented } from '../../derive/coverage.ts';
import { compactTypeInfo } from '../../derive/types.ts';
import { fmtPp, fmtShare, fmtUsd, listWords, plural } from '../../derive/format.ts';
import { concentration } from '../../derive/nodes.ts';
import { ROLE_ORDER, roleMatrix, type RoleRow } from '../../derive/roles.ts';
import type { AibNode } from '../../contract.ts';
import { csvWithRun, Section } from '../common.tsx';
import { Economy, useCompact } from './shared.tsx';

export function OverviewSection() {
  const { c, nodes, meta } = useCompact();
  const mix = classMix(nodes.map((n) => n.cls));
  const type = compactTypeInfo(c.type, compactVsFragmented(c.metrics)?.gain);
  const lostMix = parseClassMix(c.classMix);
  const nodeCols: Col<AibNode>[] = [
    { id: 'node', header: 'Sub-sector', value: (n) => n.label },
    { id: 'techs', header: 'Technologies', value: (n) => n.techs.join(', ') },
    { id: 'stage', header: 'Stage', value: (n) => n.stage },
    { id: 'cls', header: 'Class', value: (n) => classInfo(n.cls).label },
    { id: 'treatment', header: 'Recommended treatment', value: (n) => n.treatment, minWidth: 220 },
  ];
  const nodesCsv = () => ({
    name: `${c.id} included nodes`,
    text: csvWithRun(
      nodes.map((n) => ({ compact: c.id, node_id: n.id, sub_sector: n.label, technologies: n.techs.join('; '), stage: n.stage, classification: n.cls, recommended_treatment: n.treatment })),
      meta,
    ),
  });
  return (
    <Section
      id="overview"
      title="Compact overview"
      intro={
        <p>
          What the compact covers, how its nodes are treated and which economies take part. {listWords(mix.map((m) => `${m.n} ${classInfo(m.cls).label}`))}{' '}
          {nodes.length === 1 ? 'node' : 'nodes'}.
        </p>
      }
    >
      <ChartCard
        headingLevel={3}
        title="Included nodes and the compact's design"
        sub={`What the compact covers, how its nodes are treated and what it needs. ${type.short}.`}
        legend={<ClassLegend classes={mix.map((m) => m.cls)} />}
        table={() => <TableView rows={nodes} cols={nodeCols} caption={`${c.name}: included nodes`} rowKey={(n) => n.id} pageSize={50} />}
        csv={nodesCsv}
      >
        <dl className="grid gap-x-6 gap-y-3 text-sm sm:grid-cols-[10rem_minmax(0,1fr)]">
          <dt className="text-paper-mute">Archetype</dt>
          <dd className="flex flex-wrap items-center gap-x-4 gap-y-2">
            <Tag>{c.archetype}</Tag>
            {lostMix.length > 0 && (
              <span className="inline-flex flex-wrap items-center gap-x-3 gap-y-1 text-paper-dim" data-testid="class-mix">
                <span className="text-paper-mute">Class mix of the lost supply:</span>
                {lostMix.map((m) => (
                  <span key={m.cls} className="inline-flex items-center gap-1.5">
                    <ClassSwatch cls={m.cls} />
                    {classInfo(m.cls).label} {Math.round(m.share * 100)}%
                  </span>
                ))}
              </span>
            )}
            {c.secondaryClass && (
              <span className="text-paper-dim" data-testid="secondary-class">
                <span className="text-paper-mute">Secondary class:</span> {classInfo(c.secondaryClass).label}
              </span>
            )}
          </dd>
          <dt className="text-paper-mute">Type</dt>
          <dd className="flex flex-wrap items-center gap-x-3 gap-y-1">
            <TypeTag info={type} />
            <span className="text-paper-dim">{type.explanation}</span>
          </dd>
          <dt className="text-paper-mute">Technologies</dt>
          <dd className="text-paper-dim">{c.technologies.join(', ') || '–'}</dd>
          <dt className="text-paper-mute">Stages</dt>
          <dd className="text-paper-dim">{c.stages.join(', ') || '–'}</dd>
          <dt className="text-paper-mute">Included nodes</dt>
          <dd>
            <ul className="flex flex-wrap gap-2">
              {nodes.map((n) => (
                <li key={n.id} className="inline-flex flex-wrap items-center gap-2 border hairline px-2 py-1">
                  <ClassChip cls={n.cls} />
                  <span className="text-paper">{n.label}</span>
                  <span className="text-xs text-paper-mute">{n.stage}</span>
                </li>
              ))}
            </ul>
          </dd>
          <dt className="text-paper-mute">{lostMix.length > 1 ? 'Policy needs (the instruments of each major class)' : 'Policy needs'}</dt>
          <dd>
            {c.policyNeeds.length ? (
              <ul className={`list-disc space-y-0.5 pl-5 text-paper-dim ${c.policyNeeds.length > 6 ? "sm:columns-2 lg:columns-3" : ""}`}>
                {c.policyNeeds.map((p) => (
                  <li key={p}>{p}</li>
                ))}
              </ul>
            ) : (
              <span className="text-paper-mute">None listed</span>
            )}
          </dd>
        </dl>
      </ChartCard>
      <RolesCard />
    </Section>
  );
}

function RolesCard() {
  const { c, meta, names, name } = useCompact();
  const rows = roleMatrix(c, names);
  const flat = c.roles.map((r) => ({ ...r, name: name(r.iso3) }));
  const cols: Col<(typeof flat)[number]>[] = [
    { id: 'name', header: 'Economy', value: (r) => r.name },
    { id: 'role', header: 'Role', value: (r) => r.role },
    { id: 'basis', header: 'Basis in the model', value: (r) => r.basis, minWidth: 240 },
    { id: 'member', header: 'In the minimum coalition', value: (r) => (r.member ? 'yes' : 'no (associate)') },
  ];
  const csv = () => ({
    name: `${c.id} members and roles`,
    text: csvWithRun(
      c.roles.map((r) => ({ compact: c.id, iso3: r.iso3, economy: name(r.iso3), role: r.role, basis: r.basis, in_minimum_coalition: r.member })),
      meta,
    ),
  });
  const nMembers = c.coalition.members.length;
  const nAssoc = rows.filter((r) => !r.member).length;
  return (
    <ChartCard
      headingLevel={3}
      title="Members and their roles"
      sub={`${plural(nMembers, 'economy', 'economies')} in the minimum coalition${nAssoc ? `, ${plural(nAssoc, 'associate')} that ${nAssoc === 1 ? 'joins' : 'join'} only to reach the highest target` : ''}. The model assigns the roles from its results; an economy can hold several. Hover a mark for the reason.`}
      table={() => <TableView rows={flat} cols={cols} caption={`${c.name}: members and roles`} pageSize={50} />}
      csv={csv}
    >
      {rows.length ? <RoleMatrix rows={rows} /> : <EmptyState>The model assigned no roles in this compact.</EmptyState>}
    </ChartCard>
  );
}

/** Row headers read as names, not column labels, and stay in view when the matrix scrolls sideways. */
const ROW_HEAD = {
  position: 'sticky',
  left: 0,
  zIndex: 2,
  background: 'var(--color-ink-800)',
  color: 'var(--color-paper)',
  textTransform: 'none',
  letterSpacing: 'normal',
  fontSize: '0.8125rem',
  fontWeight: 400,
  borderBottom: '1px solid rgb(255 255 255 / 0.08)',
} as const;

function RoleMatrix({ rows }: { rows: RoleRow[] }) {
  const { c } = useCompact();
  return (
    <div className="relative overflow-x-auto">
      <table className="data-table">
        <caption className="sr-only">{c.name}: the role of each economy</caption>
        <thead>
          <tr>
            <th scope="col">Economy</th>
            {ROLE_ORDER.map((r) => (
              <th key={r} scope="col" className="align-bottom" style={{ minWidth: 76, maxWidth: 120, whiteSpace: 'normal', letterSpacing: '0.04em', textAlign: 'center' }}>
                {r}
              </th>
            ))}
          </tr>
        </thead>
        <tbody>
          {rows.map((row) => (
            <tr key={row.iso3}>
              <th scope="row" className="whitespace-nowrap text-left" style={ROW_HEAD}>
                <Economy iso3={row.iso3} />
                {!row.member && <span className="ml-1 text-xs text-paper-mute">associate</span>}
              </th>
              {ROLE_ORDER.map((r) => {
                const basis = row.roles[r];
                return (
                  <td key={r} className="text-center">
                    {basis !== undefined ? (
                      <span title={basis} className={row.member ? 'text-paper' : 'text-paper-mute'}>
                        <span aria-hidden="true">●</span>
                        <span className="sr-only">yes: {basis}</span>
                      </span>
                    ) : (
                      <span className="sr-only">no</span>
                    )}
                  </td>
                );
              })}
            </tr>
          ))}
        </tbody>
      </table>
      {rows.some((r) => r.other.length) && (
        <p className="mt-2 text-xs text-paper-mute">
          Other roles: {rows.flatMap((r) => r.other.map((o) => `${o.role} (${r.name})`)).join('; ')}.
        </p>
      )}
    </div>
  );
}

export function WhySection() {
  const { c, nodes, meta, name } = useCompact();
  const m = c.metrics;
  const conc = concentration(nodes);
  const vs = compactVsFragmented(m);
  const type = compactTypeInfo(c.type, vs?.gain);
  const tiles = [
    { key: 'exposure', label: 'Shock exposure', value: fmtShare(m.exposure), sub: `${fmtUsd(m.lostUsd)} of ${fmtUsd(m.demandUsd)} demand lost`, raw: m.exposure },
    ...(vs
      ? [
          {
            key: 'compactVsFragmented',
            label: 'Compact vs fragmented action',
            value: fmtPp(vs.gain),
            sub: `the demand anchors' coverage: ${fmtShare(vs.fragmented)} through existing relationships, ${fmtShare(vs.compact)} under the compact's minimum coalition`,
            raw: vs.gain,
          },
        ]
      : []),
    { key: 'dividend', label: 'Coordination dividend', value: fmtPp(m.dividend), sub: "coverage the members' new and expanded relationships add over existing relationships alone", raw: m.dividend },
    { key: 'residual', label: 'Residual gap after trade', value: fmtShare(m.residual), sub: 'demand the trade compact still cannot serve', raw: m.residual },
    { key: 'residualInv', label: 'Residual gap after investment', value: fmtShare(m.residualAfterInvestment), sub: 'after the standardized increment at member hosts in its BUILD nodes', raw: m.residualAfterInvestment },
    {
      key: 'concentration',
      label: 'Concentration',
      value: conc ? fmtShare(conc.share) : '–',
      sub: conc ? `${name(conc.supplier)} in ${conc.node.label}: the largest optimized supplier share across the nodes` : 'no optimized supplier shares',
      raw: conc?.share ?? null,
    },
  ];
  const csv = () => ({
    name: `${c.id} why it exists`,
    text: csvWithRun(
      tiles.map((t) => ({ compact: c.id, measure: t.label, value: t.raw, meaning: t.sub })),
      meta,
    ),
  });
  return (
    <Section
      id="why"
      title="Why it exists"
      intro={
        <p>
          A compact forms where the shock removes a material share of supply and joint action restores more of it than existing relationships would. Shares are of the compact&apos;s baseline
          traded demand, weighted by each node&apos;s demand; the comparison with fragmented action is for the demand anchors&apos; own demand.
        </p>
      }
    >
      {type.tone === 'unnecessary' && (
        <Notice kind="info" title="Compact unnecessary">
          {type.explanation} The group is listed because its supply chains hang together and the shock still removes supply; existing relationships, not a new coalition, carry most of the
          response.
        </Notice>
      )}
      <ChartCard
        headingLevel={3}
        title="The case for coordination"
        table={() => (
          <TableView
            rows={tiles}
            cols={[
              { id: 'label', header: 'Measure', value: (t) => t.label },
              { id: 'value', header: 'Value', value: (t) => t.value, num: true },
              { id: 'sub', header: 'What it is', value: (t) => t.sub, minWidth: 260 },
            ]}
            caption={`${c.name}: why the compact exists`}
          />
        )}
        csv={csv}
      >
        <div className="grid gap-3 sm:grid-cols-2 lg:grid-cols-3">
          {tiles.map((t) => (
            <StatTile key={t.key} label={t.label} value={t.value} sub={t.sub} />
          ))}
        </div>
      </ChartCard>
    </Section>
  );
}
