// 1. Compact overview (the included nodes with their classes, policy needs and any physical-capability
//    warning; the archetype with the class mix of the lost supply and the secondary class; the type,
//    "compact unnecessary" muted with its explanation; the policy needs, by compact and by node; the
//    coalition: the exact minimum coalition (its tie-break among minimum-size coalitions said), the
//    greedy heuristic beside it when it differs, or the widest greedy set when the middle target is out
//    of reach; and the six tiers (core coordinating members with their single substitutes as
//    alternatives, critical supply partners with how many economies together would replace them,
//    associate partners, critical, marginal critical and other external dependencies); the country x
//    role matrix with each economy's tier) and 2. Why it exists (shock exposure, compact vs fragmented
//    action for the demand anchors, coordination dividend, residual gaps, concentration). Each card has a
//    table twin and a CSV.

import { Fragment } from 'react';
import { Link } from 'react-router';
import { ChartCard } from '../../../charts/ChartCard.tsx';
import { TableView, type Col } from '../../../components/TableView.tsx';
import { EmptyState, Notice, StatTile, Tag } from '../../../components/ui.tsx';
import { useAibCtx } from '../../AibLayout.tsx';
import { ClassLegend, ClassSwatch, NodeClass } from '../../charts/ClassChip.tsx';
import { TypeTag } from '../../charts/TypeTag.tsx';
import { classInfo, classMix, parseClassMix, parseNodePolicyNeeds, policyNeedsText } from '../../derive/classes.ts';
import { targetLabel } from '../../derive/coalition.ts';
import { compactVsFragmented } from '../../derive/coverage.ts';
import { compactTypeInfo } from '../../derive/types.ts';
import { fmtPp, fmtShare, fmtUsd, listWords, plural } from '../../derive/format.ts';
import { concentration } from '../../derive/nodes.ts';
import { ROLE_ORDER, roleMatrix, type RoleRow } from '../../derive/roles.ts';
import { additionsNeeded, coalitionMethod, coalitionTiers, replacementWords, tierEconomies, tierName, tierShares, type TierEconomy } from '../../derive/tiers.ts';
import { AIB_PHYSICAL_WARNING, coalitionMembersOf, type AibNode } from '../../contract.ts';
import { csvWithRun, Section } from '../common.tsx';
import { Economy, useCompact } from './shared.tsx';

/** The compact's nodes whose class carries the physical-capability warning (the exported list, else the nodes' own). */
function warnedNodes(physicalWarnings: readonly string[] | undefined, nodes: readonly AibNode[]): string[] {
  return physicalWarnings ? [...physicalWarnings] : nodes.filter((n) => n.warning).map((n) => n.label);
}

/**
 * `title`: the section heading (the compact page asks "Who and what?"); `policy`: list the policy needs
 * here, by compact and by node (false when the page shows them under "What should governments do?").
 * Critical supply partners are the members no single other candidate can replace (the coalition's
 * criticalPartners; the compact's suppliers in a bundle without it).
 */
export function OverviewSection({ title = 'Compact overview', policy = true }: { title?: string; policy?: boolean } = {}) {
  const { c, nodes, meta, name, shockLabel } = useCompact();
  const critical = (c.coalition.criticalPartners ?? c.suppliers).map((x) => name(x));
  const mix = classMix(nodes.map((n) => n.cls));
  const type = compactTypeInfo(c.type, compactVsFragmented(c.metrics)?.gain);
  const lostMix = parseClassMix(c.classMix);
  const byNode = parseNodePolicyNeeds(c.nodePolicyNeeds);
  const warned = warnedNodes(c.physicalWarnings, nodes);
  const warning = nodes.find((n) => n.warning)?.warning ?? AIB_PHYSICAL_WARNING;
  const nodeCols: Col<AibNode>[] = [
    { id: 'node', header: 'Sub-sector', value: (n) => n.label },
    { id: 'techs', header: 'Technologies', value: (n) => n.techs.join(', ') },
    { id: 'stage', header: 'Stage', value: (n) => n.stage },
    { id: 'cls', header: 'Class', value: (n) => classInfo(n.cls).label },
    { id: 'needs', header: 'Policy needs', value: (n) => policyNeedsText(n.policyNeeds) },
    { id: 'warning', header: 'Warning', value: (n) => n.warning ?? 'none', minWidth: 200 },
    { id: 'treatment', header: 'Recommended treatment', value: (n) => n.treatment, minWidth: 220 },
  ];
  const nodesCsv = () => ({
    name: `${c.id} included nodes`,
    text: csvWithRun(
      nodes.map((n) => ({
        compact: c.id, node_id: n.id, sub_sector: n.label, technologies: n.techs.join('; '), stage: n.stage, classification: n.cls,
        policy_needs: policyNeedsText(n.policyNeeds), classification_warning: n.warning ?? null, physical_validation: n.physicalValidation ?? null, recommended_treatment: n.treatment,
      })),
      meta,
    ),
  });
  return (
    <Section
      id="overview"
      title={title}
      intro={
        <p>
          The supply-chain nodes the compact covers, how each is treated, and which economies take part in what role. {listWords(mix.map((m) => `${m.n} ${classInfo(m.cls).label}`))}{' '}
          {nodes.length === 1 ? 'node' : 'nodes'}.
        </p>
      }
    >
      {warned.length > 0 && (
        <Notice kind="warning" title={`${warning}: ${listWords(warned)}`}>
          <span data-testid="compact-physical-warnings">
            The class of {warned.length === 1 ? 'this node' : 'these nodes'} (LOW INTERVENTION or COORDINATE) rests on trade data that physical production data do not validate: the
            physical data are incomplete, or physical and trade concentration diverge while the physical rule was not applied or the class rests on low trade exposure. Each node&apos;s
            physical validation is under Node by node.
          </span>
        </Notice>
      )}
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
                  <NodeClass n={n} />
                  <span className="text-paper">{n.label}</span>
                  <span className="text-xs text-paper-mute">{n.stage}</span>
                </li>
              ))}
            </ul>
          </dd>
          <dt className="text-paper-mute">Critical supply partners</dt>
          <dd className="text-paper-dim">{critical.length ? listWords(critical) : 'none named'}</dd>
          <dt className="text-paper-mute">External dependency</dt>
          <dd className="text-paper-dim">The supplier the headline shock removes: {shockLabel}</dd>
          {policy && (
            <>
              {byNode.length > 0 && (
                <>
                  <dt className="text-paper-mute">Policy needs by node</dt>
                  <dd>
                    <NodePolicyNeeds byNode={byNode} />
                  </dd>
                </>
              )}
              <dt className="text-paper-mute">{lostMix.length > 1 ? 'Policy needs (the instruments of each major class)' : 'Policy needs'}</dt>
              <dd>
                <PolicyNeeds needs={c.policyNeeds} />
              </dd>
            </>
          )}
        </dl>
      </ChartCard>
      <CoalitionCard />
      <RolesCard />
    </Section>
  );
}

/** The compact's policy needs (the instruments of its major classes). */
export function PolicyNeeds({ needs }: { needs: readonly string[] }) {
  return needs.length ? (
    <ul className={`list-disc space-y-0.5 pl-5 text-paper-dim ${needs.length > 6 ? 'sm:columns-2 lg:columns-3' : ''}`}>
      {needs.map((p) => (
        <li key={p}>{p}</li>
      ))}
    </ul>
  ) : (
    <span className="text-paper-mute">None listed</span>
  );
}

/** The policy needs of each node (multi-label: a node can need several instruments). */
export function NodePolicyNeeds({ byNode }: { byNode: readonly { node: string; needs: string }[] }) {
  return (
    <ul className="space-y-0.5 text-paper-dim" data-testid="node-policy-needs">
      {byNode.map((x) => (
        <li key={x.node}>
          <span className="text-paper">{x.node}</span>: {x.needs || '–'}
        </li>
      ))}
    </ul>
  );
}

/** Tiers listed inline (names separated by commas) rather than one per line: they can run to dozens. */
const INLINE_TIERS = ['critical external dependency (marginal)'];

function CoalitionCard() {
  const { c, meta, names, name } = useCompact();
  const { doc } = useAibCtx();
  const mt = coalitionMethod(c, meta.targets);
  const mid = targetLabel(mt.mid);
  const groups = coalitionTiers(c, names, meta.targets, tierShares(doc.compacts));
  const econ = tierEconomies(c, names);
  const members = mt.members;
  const greedy = mt.greedy;
  const extra = greedy && mt.greedyDiffers ? greedy.length - members.length : 0;
  const tieBreak = 'among coalitions of that size the exact program keeps the one with the widest margin over the target in its weakest node, then the one that adds the most supply';
  const cols: Col<TierEconomy>[] = [
    { id: 'name', header: 'Economy', value: (e) => e.name },
    { id: 'tier', header: 'Coalition tier', value: (e) => tierName(e.tier) },
    { id: 'critical', header: 'Critical', value: (e) => (e.critical ? 'yes' : 'no') },
    { id: 'loo', header: 'Needed with the coalition fixed', value: (e) => e.looCritical, title: 'Leave one out: losing its supply pushes a node below the target with the rest of the coalition unchanged' },
    {
      id: 'subs',
      header: 'Single substitutes (any one could replace it)',
      value: (e) => (e.substituteNames.length ? e.substituteNames.join(', ') : '–'),
      minWidth: 200,
    },
    { id: 'needed', header: 'Economies needed to replace it', value: (e) => additionsNeeded(e, mt.reached), minWidth: 120 },
    { id: 'roles', header: 'Roles', value: (e) => e.roles.join('; ') || '–', minWidth: 220 },
  ];
  const csv = () => ({
    name: `${c.id} coalition tiers`,
    text: csvWithRun(
      econ.map((e) => ({
        compact: c.id, iso3: e.iso3, economy: e.name, tier: e.tier, critical: e.critical, loo_critical: e.looCritical, single_substitutes: e.substitutes.join(' '),
        substitutes_needed: e.substitutesNeeded ?? null, roles: e.roles.join('; '),
        in_coalition: members.includes(e.iso3), in_greedy_coalition: greedy ? greedy.includes(e.iso3) : null,
      })),
      meta,
    ),
  });
  const lead = mt.exact
    ? `The members are the exact minimum coalition: the fewest economies whose coordination brings every node to the ${mid} target for the demand anchors' demand, the anchors always among them; ${tieBreak}.${
        mt.tieBreakSolved === false ? ' Here that tie-break hit its time limit: the members are a coalition of the minimum size, not necessarily the one with the widest margin.' : ''
      }`
    : mt.reached
      ? !mt.proven
        ? `The exact program hit its time limit at the ${mid} target: the members are the best coalition it found, and minimality is not proven.`
        : `The members are the minimum coalition at the ${mid} target.`
      : `No coalition brings every node to the ${mid} target; the members are the widest coalition the greedy path found (its weakest node reaches ${fmtShare(c.coalition.maxCoverage)}). With the target out of reach, no member is tested for substitutes and none is called a critical supply partner.`;
  const greedyLine =
    greedy === null
      ? null
      : mt.greedyDiffers
        ? `A greedy heuristic (adding, one at a time, the economy that most reduces the shortfall) reaches the ${mid} target with ${plural(greedy.length, 'economy', 'economies')}: ${listWords(greedy.map((x) => name(x)))}. It is a different coalition${
            extra > 0
              ? `, ${plural(extra, 'economy', 'economies')} larger than the exact minimum.`
              : extra === 0
                ? ` of the same size${
                    !mt.exact
                      ? '.'
                      : mt.tieBreakSolved === false
                        ? `; the exact program's tie-break among coalitions of that size hit its time limit, so neither is shown to have the wider margin.`
                        : `; ${tieBreak}.`
                  }`
                : '.'
          }`
        : greedy.length
          ? 'The greedy heuristic (adding, one at a time, the economy that most reduces the shortfall) finds the same coalition.'
          : mt.reached
            ? `The greedy heuristic does not reach the ${mid} target.`
            : null;
  const shown = groups.filter(
    (g) =>
      g.economies.length ||
      g.tier === 'core coordinating member' ||
      (mt.reached && g.tier === 'critical supply partner') ||
      (mt.reached && g.tier === 'associate partner' && c.coalition.associatesReach99 === false),
  );
  return (
    <ChartCard
      headingLevel={3}
      title="The coalition and its tiers"
      sub={`${mt.label}: ${plural(members.length, 'economy', 'economies')}.${mt.method ? ` Method: ${mt.method}.` : ''}`}
      table={() => <TableView rows={econ} cols={cols} caption={`${c.name}: coalition tiers`} pageSize={50} />}
      csv={csv}
    >
      <p className="max-w-3xl text-sm leading-relaxed text-paper-dim" data-testid="coalition-method">
        {lead}
      </p>
      {greedyLine && (
        <p className="mt-2 max-w-3xl text-sm leading-relaxed text-paper-dim" data-testid="greedy-coalition">
          {greedyLine}
        </p>
      )}
      <dl className="mt-4 grid gap-x-6 gap-y-3 text-sm sm:grid-cols-[14rem_minmax(0,1fr)]" data-testid="coalition-tiers">
        {shown.map((g) => (
          <div key={g.tier} className="contents">
            <dt className="text-paper">
              {g.label} <span className="text-paper-mute">({g.economies.length})</span>
              <span className="mt-0.5 block text-xs font-normal text-paper-mute">{g.meaning}</span>
            </dt>
            <dd className="text-paper-dim">
              {!g.economies.length ? (
                <span className="text-paper-mute">none</span>
              ) : INLINE_TIERS.includes(g.tier) ? (
                <p data-testid="inline-tier">
                  {g.economies.map((e, i) => (
                    <Fragment key={e.iso3}>
                      {i > 0 && ', '}
                      <Economy iso3={e.iso3} />
                    </Fragment>
                  ))}
                </p>
              ) : (
                <ul className="space-y-0.5">
                  {g.economies.map((e) => {
                    const why = replacementWords(e, mt.mid, mt.reached);
                    return (
                      <li key={e.iso3}>
                        <Economy iso3={e.iso3} />
                        {e.anchor && <span className="ml-1 text-xs text-paper-mute">demand anchor</span>}
                        {why && <span className="text-paper-mute">: {why}</span>}
                      </li>
                    );
                  })}
                </ul>
              )}
            </dd>
          </div>
        ))}
      </dl>
    </ChartCard>
  );
}

function RolesCard() {
  const { c, meta, names, name } = useCompact();
  const rows = roleMatrix(c, names);
  const tiers = new Map(tierEconomies(c, names).map((e) => [e.iso3, e]));
  const flat = c.roles.map((r) => ({ ...r, name: name(r.iso3), tierName: tierName(tiers.get(r.iso3)?.tier ?? r.tier) }));
  const cols: Col<(typeof flat)[number]>[] = [
    { id: 'name', header: 'Economy', value: (r) => r.name },
    { id: 'role', header: 'Role', value: (r) => r.role },
    { id: 'basis', header: 'Basis in the model', value: (r) => r.basis, minWidth: 240 },
    { id: 'tier', header: 'Coalition tier', value: (r) => r.tierName },
    { id: 'member', header: 'In the coalition', value: (r) => (r.member ? 'yes' : 'no') },
  ];
  const csv = () => ({
    name: `${c.id} members and roles`,
    text: csvWithRun(
      c.roles.map((r) => ({
        compact: c.id, iso3: r.iso3, economy: name(r.iso3), role: r.role, basis: r.basis, in_coalition: r.member,
        tier: tiers.get(r.iso3)?.tier ?? r.tier ?? null, critical: r.critical ?? null, loo_critical: r.looCritical ?? null,
        single_substitutes: (r.substitutes ?? []).join(' '), substitutes_needed: r.substitutesNeeded ?? null,
      })),
      meta,
    ),
  });
  const nMembers = coalitionMembersOf(c).length;
  const nOutside = rows.filter((r) => !r.member).length;
  return (
    <ChartCard
      headingLevel={3}
      title="Economies and their roles"
      sub={`${plural(nMembers, 'economy', 'economies')} in the coalition${nOutside ? `; ${plural(nOutside, 'economy', 'economies')} outside it (associates or external dependencies)` : ''}. The model assigns the roles from its results; an economy can hold several. Hover a mark for the reason.`}
      table={() => <TableView rows={flat} cols={cols} caption={`${c.name}: economies and roles`} pageSize={50} />}
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
            <th scope="col" className="align-bottom" style={{ minWidth: 120, whiteSpace: 'normal', letterSpacing: '0.04em' }}>
              Coalition tier
            </th>
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
                <Link className="link" to={`/countries/${row.iso3}#aib-role`} title={`${row.name}: Country profile, role in the Allied Industrial Base`}>
                  <Economy iso3={row.iso3} />
                </Link>
              </th>
              <td className={`whitespace-nowrap text-xs ${row.member ? 'text-paper-dim' : 'text-paper-mute'}`}>
                {tierName(row.tier)}
              </td>
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

/** `title`: the section heading (the compact page asks "Why this compact?"). */
export function WhySection({ title = 'Why it exists' }: { title?: string } = {}) {
  const { c, nodes, meta, name } = useCompact();
  const m = c.metrics;
  const conc = concentration(nodes);
  const vs = compactVsFragmented(m);
  const type = compactTypeInfo(c.type, vs?.gain);
  const gap = typeof m.residual === 'number' && m.residual < 0.005 ? 'closing the gap' : `still leaving ${fmtShare(m.residual, 0)} unserved`;
  const answer = `The shock removes ${fmtShare(m.exposure, 0)} of the compact's demand. After it, existing relationships serve ${fmtShare(m.existing, 0)} of that demand and coordinated trade among the members ${fmtShare(m.trade, 0)}, ${gap}${
    vs ? `. Acting together, the demand anchors cover ${fmtPp(vs.gain).replace(/^\+/, '')} more of their own demand than through existing relationships alone` : ''
  }${conc ? `; ${name(conc.supplier)} alone supplies up to ${fmtShare(conc.share, 0)} of one node (${conc.node.label})` : ''}.`;
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
      title={title}
      intro={
        <p>
          <span className="text-paper" data-testid="why-answer">
            {answer}
          </span>{' '}
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
