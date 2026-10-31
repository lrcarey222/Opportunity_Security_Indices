// The sections of one Strategic Sectoral Compact's page (2026-10-20, task s.15, s.16, s.20, s.26), each
// answering one question in plain words over the export's readings (derive/strategic.ts) and the main
// document's node evidence:
//   WhySection           why the compact exists: the mission and the headline figures under the scenario
//   VulnerabilitySection where it is vulnerable: the nodes with the largest gap (NodeCoverageBar), the
//                        leading suppliers outside the alliance, the critical partners and dependencies
//   PolicySection        what the nodes need: the missions, the policy portfolio and the instrument menu
//                        generated from the nodes' classes (interpretive, not a prescription)
//   WhoSection           who does what: the core countries' compact roles, the division-of-labour matrix
//                        (DivisionOfLabour) and every economy's compact role behind a disclosure
//   WorkstreamsSection   the workstreams underneath (the model's clusters, AIB-Cxx pages) and those that
//                        also touch the compact
//   ScenarioChangeSection what changes between Managed Interdependence and Decoupling, Baseline as the
//                        reference: the compact side by side, its demand served (ScenarioCoverageCompare)
//                        and every node's status in every scenario
// Country names link to their role in the Allied Industrial Base (the Country profile); workstream ids to
// their pages. Every list says what it is; colour never carries a reading alone.

import { useState, type ReactNode } from 'react';
import { Link } from 'react-router';
import { ChartCard } from '../../../charts/ChartCard.tsx';
import { Legend } from '../../../charts/legends.tsx';
import { TableView, type Col } from '../../../components/TableView.tsx';
import { ChangedDot, Disclosure, EmptyState, StatTile } from '../../../components/ui.tsx';
import { useAibCtx } from '../../AibLayout.tsx';
import { ClassChip } from '../../charts/ClassChip.tsx';
import { DivisionOfLabour } from '../../charts/DivisionOfLabour.tsx';
import { NODE_MARK, NodeCoverageBar } from '../../charts/NodeCoverageBar.tsx';
import { ScenarioCoverageCompare } from '../../charts/ScenarioCompare.tsx';
import { SpineLegend, SpineStatusSwatch } from '../../charts/ValueChainSpine.tsx';
import type { AibCompactRole, AibStrategic, AibStrategicCompact, AibStrategicCompactScenario, AibStrategicScenarioDef, AibStrategicScenarioId } from '../../contract.ts';
import { demandSplit } from '../../derive/diversify.ts';
import { fmtShare, fmtUsd, listWords, plural } from '../../derive/format.ts';
import { nodeScenario, type AibScenarioId } from '../../derive/scenarios.ts';
import {
  activeWorkstreamsOf, alsoTouchingOf, compactNodeIds, compactRoleOf, compactRolesOfCompact, compactScenarioOf, countryStrategicOf, divisionMatrix, missionsOf, nodeHomeOf,
  policyMixWords, spineStatusInfo, strategicScenarioDef, strategicScenarios, workstreamsOf, type DivisionMeasure, type WorkstreamRow,
} from '../../derive/strategic.ts';
import { compactHref, strategicCompactHref, workstreamsHref } from '../../nav.ts';
import { BasisTag, LazyDisclosure } from '../basis.tsx';
import { csvWithRun, Section } from '../common.tsx';
import { strategicCsvRows } from './ScenarioMatrix.tsx';
import { activeWords, countryRoleHref, Fact, interventionCount, interventionWords, lowerFirst, namesWords, ROW_TH_STYLE, scenarioLabelOf, STRATEGIC_SECTION_TITLE, upperFirst } from './shared.tsx';

const SLOT_WORDS: Readonly<Record<string, string>> = { primary: 'Primary mission', secondary: 'Secondary mission', supporting: 'Supporting mission' };

/** The node's label from the main document (its id when the document has none). */
function useNodeLabel(): (id: string) => string {
  const { nodeOf } = useAibCtx();
  return (id: string) => nodeOf(id)?.label ?? id;
}

// ---- Why this compact exists -------------------------------------------------------------------------------

export function WhySection({ s, c, sid }: { s: AibStrategic; c: AibStrategicCompact; sid: AibStrategicScenarioId }) {
  const sc = compactScenarioOf(c, sid);
  const def = strategicScenarioDef(s, sid);
  const label = scenarioLabelOf(s, sid);
  const mi = sid === 'managed';
  const first = c.steps[0]?.label ?? '';
  const last = c.steps[c.steps.length - 1]?.label ?? '';
  const cov = sc?.coverage;
  return (
    <Section
      id="why"
      title={STRATEGIC_SECTION_TITLE.why}
      intro={
        <p>
          {c.steps.length > 1
            ? `It ties ${plural(c.steps.length, 'stage')} of one supply chain together, from ${lowerFirst(first)} to ${lowerFirst(last)} (${plural(compactNodeIds(c).length, 'supply-chain step')}), so that the countries along it can plan supply as one system rather than step by step.`
            : `It brings the ${plural(compactNodeIds(c).length, 'supply-chain step')} of ${lowerFirst(first)} together, so that the countries that make and buy them can plan supply as one system.`}{' '}
          The compact is the same in every scenario; the scenario sets how hard it is stressed.
        </p>
      }
    >
      <div className="card p-5" data-testid="compact-mission">
        <BasisTag basis="facts" scenario={label} />
        <p className="mt-2 text-lg leading-snug text-paper">{sc?.statement ?? '–'}</p>
        {def?.words && (
          <p className="mt-1 text-sm text-paper-mute">
            {label}: {def.words}
          </p>
        )}
        <div className="mt-4 grid gap-3 sm:grid-cols-2 xl:grid-cols-4">
          <StatTile label="Allied demand" value={fmtUsd(sc?.demandUsd)} sub="imports of the compact’s goods into the allied market" />
          <StatTile label={mi ? 'Supply to shift' : 'Supply lost'} value={fmtUsd(sc?.lostUsd)} sub={`${fmtShare(sc?.exposure, 0)} of allied demand`} />
          <StatTile label="Met with coordinated trade" value={fmtShare(cov?.coordinated, 0)} sub={`share of allied demand; existing relationships alone ${fmtShare(cov?.existing, 0)}`} />
          <StatTile
            label="After simulated new capacity"
            value={fmtShare(cov?.postInvestment, 0)}
            sub={
              cov?.postInvestment === null || cov?.postInvestment === undefined
                ? 'not simulated under this scenario'
                : 'coordinated trade plus a standard capacity addition at each step (by construction: + the addition or the gap, the smaller)'
            }
          />
        </div>
        {sc?.stressNote && (
          <p className="mt-3 border-l-2 border-signal pl-3 text-sm text-paper-dim" data-testid="stress-note">
            {sc.stressNote}
          </p>
        )}
      </div>
    </Section>
  );
}

// ---- Where it is vulnerable ----------------------------------------------------------------------------------

export function VulnerabilitySection({ s, c, sid }: { s: AibStrategic; c: AibStrategicCompact; sid: AibStrategicScenarioId }) {
  const { nodeOf, name, scenario, href, doc } = useAibCtx();
  const nodeLabel = useNodeLabel();
  const sc = compactScenarioOf(c, sid);
  const label = scenarioLabelOf(s, sid);
  const mi = scenario === 'managed';
  const status = new Map((sc?.nodes ?? []).map((t) => [t[0], t[1]]));
  const rows = compactNodeIds(c).map((id) => {
    const n = nodeOf(id);
    return { id, label: nodeLabel(id), status: status.get(id) ?? null, r: n ? nodeScenario(n, scenario) : null };
  });
  const hit = rows.filter((x) => x.r?.ran && (x.r.lostUsd ?? 0) > 0).sort((a, b) => (b.r?.lostUsd ?? 0) - (a.r?.lostUsd ?? 0) || a.label.localeCompare(b.label));
  const top = hit.slice(0, 5);
  const thin = rows.filter((x) => x.status === 'INSUFFICIENT DATA');
  const untested = rows.filter((x) => x.status === 'NOT TESTED');
  const lost = mi ? 'has to shift' : 'is lost';
  const summary = hit.length
    ? `Under ${label}, ${plural(hit.length, 'supply-chain step')} of ${rows.length} ${hit.length === 1 ? 'loses' : 'lose'} supply; ${fmtUsd(sc?.lostUsd)} of the compact’s allied demand ${lost}, most of it in ${listWords(top.slice(0, 2).map((x) => x.label))}.`
    : `Under ${label} no supply-chain step of the compact loses supply.`;
  const cols: Col<(typeof rows)[number]>[] = [
    { id: 'node', header: 'Supply-chain step', value: (x) => x.label, minWidth: 180 },
    { id: 'status', header: `Status: ${label}`, value: (x) => (x.status ? spineStatusInfo(x.status).label : null) },
    { id: 'lost', header: mi ? 'Supply to shift' : 'Supply lost', value: (x) => x.r?.lostUsd ?? null, cell: (x) => fmtUsd(x.r?.lostUsd), num: true },
    { id: 'share', header: 'Share of its allied demand', value: (x) => x.r?.exposure ?? null, cell: (x) => fmtShare(x.r?.exposure, 0), num: true },
    { id: 'static', header: mi ? 'Met under the cap alone' : 'Met after the disruption alone', value: (x) => x.r?.static ?? null, cell: (x) => fmtShare(x.r?.static, 0), num: true },
    { id: 'existing', header: 'Met by existing relationships', value: (x) => x.r?.existing ?? null, cell: (x) => fmtShare(x.r?.existing, 0), num: true },
    { id: 'optimized', header: 'Met with coordinated trade', value: (x) => x.r?.optimized ?? null, cell: (x) => fmtShare(x.r?.optimized, 0), num: true },
  ];
  const nonAllied = c.nonAllied.map((x) => ({ x, role: compactRoleOf(s, c.id, x.iso3) }));
  return (
    <Section id="vulnerabilities" title={STRATEGIC_SECTION_TITLE.vulnerabilities} intro={<p>{summary}</p>}>
      <div className="grid gap-4 lg:grid-cols-[minmax(0,3fr)_minmax(0,2fr)]">
        <ChartCard
          headingLevel={3}
          title={mi ? 'Where supply has to shift' : 'Where supply is lost'}
          sub={`The supply-chain steps with the largest gap under ${label}, largest first. The empty track to the right of the filled dot is what new capacity would have to fill.`}
          summary={summary}
          legend={
            <Legend
              label="Coverage marks"
              items={[
                { label: mi ? 'Cap alone' : 'Shock alone', color: NODE_MARK.static, kind: 'tick' },
                { label: 'Existing relationships', color: NODE_MARK.existing, kind: 'hollow' },
                { label: 'Coordinated trade', color: NODE_MARK.optimized, kind: 'dot' },
              ]}
            />
          }
          table={() => <TableView rows={rows} cols={cols} caption={`${c.name}: every supply-chain step under ${label}`} rowKey={(x) => x.id} initialSort={{ id: 'lost', desc: true }} />}
          csv={() => ({
            name: `${c.id} nodes ${label.toLowerCase()}`,
            text: csvWithRun(
              rows.map((x) => ({
                strategic_compact_id: c.id,
                node_id: x.id,
                node: x.label,
                scenario: label,
                status: x.status,
                lost_supply_usd: x.r?.lostUsd ?? null,
                share_lost: x.r?.exposure ?? null,
                coverage_shock_alone: x.r?.static ?? null,
                coverage_existing: x.r?.existing ?? null,
                coverage_coordinated: x.r?.optimized ?? null,
              })),
              doc.meta,
            ),
          })}
        >
          {top.length ? (
            <ol className="space-y-3" aria-label={`Supply-chain steps with the largest gap under ${label}`}>
              {top.map((x) => (
                <li key={x.id} className="text-sm" data-node={x.id}>
                  <p className="flex flex-wrap items-center gap-x-2 gap-y-0.5">
                    <span className="text-paper">{x.label}</span>
                    <span className="inline-flex items-center gap-1 text-xs text-paper-mute">
                      <SpineStatusSwatch status={x.status} size={8} />
                      {x.status ? spineStatusInfo(x.status).label : '–'}
                    </span>
                  </p>
                  <p className="text-xs text-paper-dim">
                    {fmtShare(x.r?.exposure, 0)} of its allied demand ({fmtUsd(x.r?.lostUsd)}) {lost}; coordinated trade serves {fmtShare(x.r?.optimized, 0)}
                  </p>
                  <NodeCoverageBar
                    stat={x.r?.static ?? null}
                    existing={x.r?.existing ?? null}
                    optimized={x.r?.optimized ?? null}
                    label={`${x.label}: ${mi ? 'cap' : 'shock'} alone ${fmtShare(x.r?.static, 0)}, existing relationships ${fmtShare(x.r?.existing, 0)}, coordinated trade ${fmtShare(x.r?.optimized, 0)} of demand served`}
                  />
                </li>
              ))}
            </ol>
          ) : (
            <EmptyState>No supply-chain step of this compact loses supply under {label}.</EmptyState>
          )}
        </ChartCard>
        <div className="card p-5" data-testid="concentration">
          <h3 className="rule-neis-sm text-base font-medium text-paper lg:text-lg">Concentrated supply</h3>
          <dl className="mt-3 grid grid-cols-1 gap-y-1 text-sm">
            <Fact term="Leading suppliers outside the alliance">
              {nonAllied.length ? (
                <ul className="space-y-1">
                  {nonAllied.map(({ x, role }) => (
                    <li key={x.iso3}>
                      <span className="text-paper">{name(x.iso3)}</span> — {lowerFirst(x.role.replace(/ \(non-allied supplier\)$/, ''))}
                      {role?.leadShare !== null && role?.leadShare !== undefined && role.leadNode ? (
                        <span className="text-paper-mute">
                          : {fmtShare(role.leadShare, 0)} {role.leadBasis ?? 'share'} of {lowerFirst(nodeLabel(role.leadNode))}
                        </span>
                      ) : x.step ? (
                        <span className="text-paper-mute"> ({lowerFirst(x.step)})</span>
                      ) : null}
                    </li>
                  ))}
                </ul>
              ) : (
                'none identified'
              )}
            </Fact>
            <Fact term={`Critical dependencies outside the alliance (${label})`}>{namesWords(sc?.criticalExternal, name)}</Fact>
            <Fact term={`Allied partners the compact cannot do without (${label})`}>{namesWords(sc?.criticalPartners, name)}</Fact>
            {sc?.alliedOutsideCore && sc.alliedOutsideCore.length > 0 && (
              <Fact term={`Allied suppliers outside the core coalition (${label})`}>{namesWords(sc.alliedOutsideCore, name)}</Fact>
            )}
            {sc?.nonAlliedReliedOn && sc.nonAlliedReliedOn.length > 0 && (
              <Fact term={`Non-allied suppliers the coalition relies on (${label})`}>{namesWords(sc.nonAlliedReliedOn, name)}</Fact>
            )}
            <Fact term="Steps with too little data to test">{thin.length ? listWords(thin.map((x) => x.label)) : 'none'}</Fact>
            {untested.length > 0 && (
              <Fact term={`Concentrated steps not tested under ${label}`}>
                {listWords(untested.map((x) => x.label))} (comparable goods are as concentrated; see {scenarioLabelOf(s, 'decoupling')})
              </Fact>
            )}
          </dl>
          <p className="mt-3 text-xs text-paper-mute">
            Who supplies each step is in the{' '}
            <a className="link" href="#division">
              division of labor
            </a>{' '}
            below; the economies’ full positions are on their{' '}
            <Link className="link" to={href('/aib/roles')}>
              country roles
            </Link>
            .
          </p>
        </div>
      </div>
    </Section>
  );
}

// ---- What the nodes need ---------------------------------------------------------------------------------------

export function PolicySection({ s, c, sid }: { s: AibStrategic; c: AibStrategicCompact; sid: AibStrategicScenarioId }) {
  const nodeLabel = useNodeLabel();
  const sc = compactScenarioOf(c, sid);
  const label = scenarioLabelOf(s, sid);
  const missions = missionsOf(sc);
  const k = sc?.counts;
  const portfolio: [string, number][] = k
    ? [
        ['BUILD', k.build],
        ['COORDINATE', k.coordinate],
        ['MANAGE SPECIALIZATION', k.manage],
        ['LOW INTERVENTION', k.low],
      ]
    : [];
  return (
    <Section
      id="policy"
      title={STRATEGIC_SECTION_TITLE.policy}
      intro={
        <p>
          Different bottlenecks need different tools: trade rules where capacity already exists, investment and finance where capacity is missing, and safeguards where
          specialization has become too concentrated. Each supply-chain step gets the treatment its own results call for, so one compact can combine building capacity,
          coordinating trade and managing specialization. Under {label},{' '}
          {interventionWords(c, sc)}.
        </p>
      }
    >
      <div className="grid gap-4 lg:grid-cols-2">
        <div className="card p-5" data-testid="missions">
          <h3 className="rule-neis-sm text-base font-medium text-paper lg:text-lg">Missions</h3>
          {missions.length ? (
            <ol className="mt-3 space-y-3">
              {missions.map((m) => (
                <li key={m.slot} data-targeted={m.targeted ? 'true' : undefined}>
                  <span className="eyebrow text-paper-mute">{m.targeted ? 'Targeted action' : (SLOT_WORDS[m.slot] ?? m.slot)}</span>
                  <p className="mt-0.5 text-paper">{upperFirst(m.text)}</p>
                  {m.basis && <p className="text-xs text-paper-mute">{m.basis}</p>}
                </li>
              ))}
            </ol>
          ) : (
            <p className="mt-3 text-sm text-paper-dim">{sc?.statement ?? '–'}</p>
          )}
          <h4 className="mt-5 text-sm font-medium text-paper">Policy portfolio</h4>
          <ul className="mt-2 grid grid-cols-2 gap-2 text-sm" aria-label={`Supply-chain steps by treatment under ${label}`}>
            {portfolio.map(([cls, n]) => (
              <li key={cls} className="panel min-w-0 px-3 py-2" title={spineStatusInfo(cls).meaning}>
                <span className="block text-xl font-medium tabular text-paper">{n}</span>
                <span className="inline-flex min-w-0 items-center gap-1.5 break-words text-xs text-paper-dim">
                  <SpineStatusSwatch status={cls} size={8} />
                  {spineStatusInfo(cls).label}
                </span>
              </li>
            ))}
          </ul>
          {k && k.insufficient + k.notStressed + (k.notTested ?? 0) + k.noShock > 0 && (
            <p className="mt-2 text-xs text-paper-mute" data-testid="portfolio-rest">
              The other {plural(k.insufficient + k.notStressed + (k.notTested ?? 0) + k.noShock, 'supply-chain step')}:{' '}
              {[
                k.insufficient ? `${k.insufficient} with too little data to test` : null,
                k.notStressed ? `${k.notStressed} unaffected under ${label}` : null,
                k.notTested ? `${k.notTested} concentrated, not tested under ${label}` : null,
                k.noShock ? `${k.noShock} with no disruption applied` : null,
              ]
                .filter(Boolean)
                .join(', ')}
              .
            </p>
          )}
        </div>
        <div className="card p-5" data-testid="policy-menu">
          <h3 className="rule-neis-sm text-base font-medium text-paper lg:text-lg">Policy menu</h3>
          <p className="mt-1 text-sm text-paper-mute">
            Instruments that fit each kind of need, drawn from what each of the compact’s supply-chain steps needs: a menu to choose from, not a prescription.
          </p>
          {sc?.menu.length ? (
            <ul className="mt-3 space-y-4">
              {sc.menu.map((m) => (
                <li key={m.cls}>
                  <ClassChip cls={m.cls} />
                  <p className="mt-1 text-xs text-paper-mute">For {listWords(m.nodes.map((id) => lowerFirst(nodeLabel(id))))}</p>
                  <ul className="mt-1.5 flex flex-wrap gap-1.5" aria-label={`Instruments for ${spineStatusInfo(m.cls).label}`}>
                    {m.instruments.map((i) => (
                      <li key={i} className="border hairline px-2 py-0.5 text-xs text-paper-dim">
                        {i}
                      </li>
                    ))}
                  </ul>
                </li>
              ))}
            </ul>
          ) : (
            <p className="mt-3 text-sm text-paper-dim">No supply-chain step needs action under {label}, so the menu is empty.</p>
          )}
        </div>
      </div>
    </Section>
  );
}

// ---- Who does what ------------------------------------------------------------------------------------------------

/** Scroll the node-evidence panel (ChainSection) into view and move focus to its heading. */
function focusNodeEvidence(id: string): void {
  const el = document.getElementById(`node-evidence-${id}`);
  if (!el) return;
  el.scrollIntoView?.({ behavior: 'smooth', block: 'start' });
  el.focus?.({ preventScroll: true });
}

function roleCols(name: (iso3: string) => string, href: (to: string) => string, nodeLabel: (id: string) => string): Col<AibCompactRole>[] {
  const share = (id: string, header: string, pick: (r: AibCompactRole) => number | null, title: string): Col<AibCompactRole> => ({
    id,
    header,
    value: pick,
    cell: (r) => fmtShare(pick(r), 1),
    num: true,
    title,
  });
  return [
    {
      id: 'economy',
      header: 'Economy',
      value: (r) => name(r.iso3),
      cell: (r) => (
        <Link className="link" to={href(countryRoleHref(r.iso3))}>
          {name(r.iso3)}
        </Link>
      ),
      minWidth: 140,
    },
    { id: 'allied', header: 'Allied', value: (r) => r.allied },
    { id: 'role', header: 'Role in the compact', value: (r) => r.role, minWidth: 180 },
    { id: 'functions', header: 'Every function', value: (r) => r.functions.join('; '), minWidth: 220 },
    { id: 'lead', header: 'Strongest step', value: (r) => (r.leadNode ? nodeLabel(r.leadNode) : null), minWidth: 160 },
    share('leadShare', 'Its share there', (r) => r.leadShare, 'Its share of world supply or imports of that supply-chain step (see the CSV of the division of labor)'),
    share('supplier', 'Supplier contribution', (r) => r.supplier, "Its share of the world supply of the compact's goods"),
    share('demand', 'Demand contribution', (r) => r.demand, "Its share of the allied market's imports of the compact's goods"),
    share('investment', 'Investment contribution', (r) => r.investment, "Its share of outward clean-tech manufacturing capital in the compact's sectors, where the data cover them"),
    share('resM', 'Resilience added: Managed Interdependence', (r) => r.resilienceManaged, "Share of the compact's allied demand its new and expanded relationships serve under Managed Interdependence (US$ recovered over the compact's allied demand)"),
    share('resD', 'Resilience added: major-supplier disruption', (r) => r.resilienceDecoupling, "Share of the compact's allied demand its new and expanded relationships serve under the major-supplier disruption (US$ recovered over the compact's allied demand)"),
    { id: 'crit', header: 'Criticality', value: (r) => r.criticality, minWidth: 160 },
  ];
}

export function WhoSection({
  s, c, node, onNode,
}: {
  s: AibStrategic;
  c: AibStrategicCompact;
  node: string | null;
  onNode: (id: string | null) => void;
}) {
  const { name, href, nodeOf } = useAibCtx();
  const nodeLabel = useNodeLabel();
  const [measure, setMeasure] = useState<DivisionMeasure>('supply');
  const matrix = divisionMatrix(s, c, { measure, nodeOf });
  const roles = compactRolesOfCompact(s, c.id);
  return (
    <Section
      id="who"
      title={STRATEGIC_SECTION_TITLE.who}
      intro={
        <p>
          The compact’s core partners and what each contributes, then every country’s position at each supply-chain step. Who has the strongest capability, and which countries
          need a partner? The same country can do different things in different compacts, and no country is expected to make every part.
        </p>
      }
    >
      <ul className="grid gap-4 md:grid-cols-2 xl:grid-cols-3" aria-label={`Core countries of ${c.name}`}>
        {c.core.map((x) => {
          const r = compactRoleOf(s, c.id, x.iso3);
          const strategic = countryStrategicOf(s, x.iso3);
          const detail = r?.detail && r.detail !== r.role ? r.detail : null;
          return (
            <li key={x.iso3} className="panel p-4 text-sm" data-iso3={x.iso3}>
              <p className="text-base">
                <Link className="link" to={href(countryRoleHref(x.iso3))}>
                  {name(x.iso3)}
                </Link>
              </p>
              <p className="mt-1 text-paper">{x.role}</p>
              {detail && <p className="text-xs text-paper-mute">Also: {lowerFirst(detail.split(' / ').filter((d) => d !== x.role).join(', '))}</p>}
              <dl className="mt-2 grid grid-cols-[6.5rem_minmax(0,1fr)] gap-x-3 gap-y-1 text-xs">
                {r?.leadNode && (
                  <Fact term="Strongest at">
                    {nodeLabel(r.leadNode)}
                    {r.leadShare !== null ? ` (${fmtShare(r.leadShare, 0)} ${r.leadBasis ?? ''})`.replace(/ \)$/, ')') : ''}
                  </Fact>
                )}
                {r && r.keyNodes.length > 0 && <Fact term="Key supply-chain steps">{r.keyNodes.join('; ')}</Fact>}
                {strategic && strategic.primary.length > 0 && <Fact term="Its overall role">{strategic.primary.map((p) => p.name).join('; ')}</Fact>}
              </dl>
            </li>
          );
        })}
      </ul>
      {/* [&_.overflow-x-auto]:relative keeps the cells' screen-reader text inside the matrix's scroll box (absolutely placed, it would otherwise widen the page) */}
      <div id="division" className="scroll-mt-16 [&_.overflow-x-auto]:relative">
        <DivisionOfLabour
          matrix={matrix}
          name={name}
          title="Division of labor"
          headingLevel={3}
          sub="Who holds which position at each supply-chain step today, from the evidence. Choose what the dots show; select a step’s name to see its evidence in the supply chain above."
          onMeasure={setMeasure}
          selectedNode={node}
          onSelectNode={(id) => {
            onNode(node === id ? null : id);
            // the evidence opens in the supply chain above: take the reader there (sighted readers, phones)
            if (node !== id) window.setTimeout(() => focusNodeEvidence(id), 0);
          }}
          rowHref={(iso) => href(countryRoleHref(iso))}
          renderLink={(h, children) => (
            <Link className="link" to={h}>
              {children}
            </Link>
          )}
          nodeLabel={nodeLabel}
        />
      </div>
      {roles.length > 0 && (
        <LazyDisclosure summary={`Every economy’s role in this compact (${roles.length})`}>
          {() => (
            <TableView rows={roles} cols={roleCols(name, href, nodeLabel)} caption={`Every economy’s role in ${c.name}`} rowKey={(r) => r.iso3} search pageSize={15} />
          )}
        </LazyDisclosure>
      )}
    </Section>
  );
}

// ---- Workstreams ----------------------------------------------------------------------------------------------------

function WorkstreamCard({ s, c, row, label, also = false }: { s: AibStrategic; c: AibStrategicCompact; row: WorkstreamRow; label: string; also?: boolean }) {
  const { title, href } = useAibCtx();
  const nodeLabel = useNodeLabel();
  const cand = row.candidate;
  const nodes = cand?.nodes ?? [];
  const here = nodes.filter((id) => nodeHomeOf(s, id)?.compact.id === c.id);
  const elsewhere = nodes.filter((id) => nodeHomeOf(s, id)?.compact.id !== c.id);
  const home = row.home ? (row.home === c.id ? c : (s.compacts.find((x) => x.id === row.home) ?? null)) : null;
  const others = row.ws.alsoSpans.flatMap((id) => {
    const x = s.compacts.find((k) => k.id === id);
    return x ? [x] : [];
  });
  const hid = `ws-${row.id}`;
  return (
    <li>
      <article className="card flex h-full flex-col p-5" aria-labelledby={hid} data-workstream={row.id}>
        <div className="flex flex-wrap items-center gap-2">
          {also ? (
            <span className="eyebrow">{row.id}</span>
          ) : (
            <Link className="eyebrow link" to={href(compactHref(row.id))}>
              {row.id}
            </Link>
          )}
          <span className={row.active ? 'tag border-signal text-paper' : 'tag text-paper-mute'}>{row.active ? `Active under ${label}` : `Not active under ${label}`}</span>
        </div>
        <h3 id={hid} className="mt-2 text-base font-medium leading-snug text-paper">
          <Link className="hover:text-signal" to={href(compactHref(row.id))}>
            {cand ? title(cand) : row.id}
          </Link>
        </h3>
        <dl className="mt-3 grid grid-cols-[7rem_minmax(0,1fr)] gap-x-3 gap-y-1.5 text-sm">
          <Fact term="Steps here">{here.length ? listWords(here.map(nodeLabel)) : 'none'}</Fact>
          {elsewhere.length > 0 && (
            <Fact term="Steps in other workstreams">
              {elsewhere
                .map((id) => {
                  const h = nodeHomeOf(s, id);
                  return `${nodeLabel(id)}${h ? ` (${h.compact.name})` : ''}`;
                })
                .join('; ')}
            </Fact>
          )}
          <Fact term="Filed under">
            {home ? (home.id === c.id ? 'this compact' : <Link className="link" to={href(strategicCompactHref(home.id))}>{home.name}</Link>) : 'no strategic compact'}
            {row.ws.share !== null && home ? `, which holds ${fmtShare(row.ws.share, 0)} of its allied demand` : ''}
            {others.length > 0 && (
              <>
                ; it also spans{' '}
                {others.map((o, i) => (
                  <span key={o.id}>
                    {i > 0 ? ', ' : ''}
                    {o.id === c.id ? 'this compact' : (
                      <Link className="link" to={href(strategicCompactHref(o.id))}>
                        {o.name}
                      </Link>
                    )}
                  </span>
                ))}
              </>
            )}
          </Fact>
        </dl>
        {row.ws.overrideReason && (
          <div className="mt-2">
            <Disclosure summary="Why it is filed here">
              <p className="text-sm text-paper-dim">{row.ws.overrideReason}</p>
            </Disclosure>
          </div>
        )}
        <p className="mt-auto pt-3 text-sm">
          <Link className="link" to={href(compactHref(row.id))} aria-label={`View compact workstream: ${cand ? title(cand) : row.id}`}>
            View compact workstream &rarr;
          </Link>
        </p>
      </article>
    </li>
  );
}

export function WorkstreamsSection({ s, c, sid }: { s: AibStrategic; c: AibStrategicCompact; sid: AibStrategicScenarioId }) {
  const { doc, href } = useAibCtx();
  const label = scenarioLabelOf(s, sid);
  const rows = workstreamsOf(s, c.id, doc, sid);
  const also = alsoTouchingOf(s, c.id, doc, sid);
  const active = activeWorkstreamsOf(c, sid);
  return (
    <Section
      id="workstreams"
      title={STRATEGIC_SECTION_TITLE.workstreams}
      intro={
        <p>
          Workstreams are groups of related supply-chain steps that need similar partners and similar policy tools: where the compact’s work happens in practice.{' '}
          {rows.length ? `${activeWords(c, active)} under ${label}.` : ''}
        </p>
      }
    >
      {rows.length ? (
        <ul className="grid gap-4 md:grid-cols-2 xl:grid-cols-3" aria-label={`Workstreams of ${c.name}`}>
          {rows.map((r) => (
            <WorkstreamCard key={r.id} s={s} c={c} row={r} label={label} />
          ))}
        </ul>
      ) : (
        <EmptyState>
          No workstream: none of the analysis’s clusters falls mainly in this compact. Its supply-chain steps are on the supply chain above, and their evidence is under each step.
        </EmptyState>
      )}
      {also.length > 0 && (
        <div>
          <h3 className="mb-2 text-lg font-medium text-paper">Also touching this compact</h3>
          <p className="mb-3 max-w-3xl text-sm text-paper-dim">Workstreams filed under another compact that hold some of this compact’s supply-chain steps.</p>
          <ul className="grid gap-4 md:grid-cols-2 xl:grid-cols-3" aria-label={`Workstreams filed elsewhere that touch ${c.name}`}>
            {also.map((r) => (
              <WorkstreamCard key={r.id} s={s} c={c} row={r} label={label} also />
            ))}
          </ul>
        </div>
      )}
      <p className="text-sm">
        <Link className="link" to={href(workstreamsHref(undefined, c.id))}>
          All workstreams, by compact &rarr;
        </Link>
      </p>
    </Section>
  );
}

// ---- What changes between scenarios -----------------------------------------------------------------------------

/** The BUILD hosts in words: the named ones, then the nodes whose hosts tie ("Carpania; 3 BUILD nodes where hosts tie"). */
function hostWords(sc: AibStrategicCompactScenario | null, name: (iso3: string) => string): string {
  if (!sc) return '–';
  const named = sc.buildHosts.map(name);
  const ties = sc.buildHostTies ?? 0;
  const parts = [named.length ? listWords(named) : null, ties > 0 ? `${plural(ties, 'step needing new capacity', 'steps needing new capacity')} where hosts tie` : null].filter(Boolean);
  return parts.length ? parts.join('; ') : 'none';
}

/** The rows of the side-by-side comparison: a measure and its words under one scenario. */
function compareRows(c: AibStrategicCompact, name: (iso3: string) => string): { key: string; label: string; value: (d: AibStrategicScenarioDef, sc: AibStrategicCompactScenario | null) => ReactNode }[] {
  return [
    { key: 'does', label: 'What the scenario does', value: (d) => d.words ?? '–' },
    { key: 'lost', label: 'Supply lost or shifted', value: (_d, sc) => (sc ? `${fmtUsd(sc.lostUsd)} (${fmtShare(sc.exposure, 0)} of allied demand)` : '–') },
    { key: 'nodes', label: 'Supply-chain steps needing action', value: (_d, sc) => (sc ? `${interventionCount(sc)} of ${compactNodeIds(c).length}` : '–') },
    { key: 'mix', label: 'Actions needed', value: (_d, sc) => policyMixWords(sc) },
    { key: 'mission', label: 'Mission', value: (_d, sc) => sc?.statement ?? '–' },
    { key: 'existing', label: 'Met by existing relationships', value: (_d, sc) => fmtShare(sc?.coverage.existing, 0) },
    { key: 'coordinated', label: 'Met with coordinated trade', value: (_d, sc) => fmtShare(sc?.coverage.coordinated, 0) },
    {
      key: 'post',
      label: 'Met after simulated new capacity',
      value: (d, sc) => (sc?.coverage.postInvestment === null || sc?.coverage.postInvestment === undefined ? (d.investment ? '–' : 'not simulated') : fmtShare(sc.coverage.postInvestment, 0)),
    },
    { key: 'ws', label: 'Active workstreams', value: (_d, sc) => (sc ? `${sc.activeWorkstreams.length} of ${c.workstreams.length}` : '–') },
    { key: 'partners', label: 'Allied partners it cannot do without', value: (_d, sc) => namesWords(sc?.criticalPartners, name, 'none', 'not assessed') },
    { key: 'external', label: 'Critical dependencies outside the alliance', value: (_d, sc) => namesWords(sc?.criticalExternal, name, 'none', 'not assessed') },
    { key: 'hosts', label: 'Hosts for new capacity (named where no other host ties)', value: (d, sc) => (d.investment ? hostWords(sc, name) : 'not simulated') },
  ];
}

export function ScenarioChangeSection({ s, c, sid }: { s: AibStrategic; c: AibStrategicCompact; sid: AibStrategicScenarioId }) {
  const { doc, name, scenario } = useAibCtx();
  const nodeLabel = useNodeLabel();
  const all = strategicScenarios(s);
  const compared = all.filter((d) => d.id !== 'partial');
  const of = (id: string) => compactScenarioOf(c, id);
  const mi = of('managed');
  const dec = of('decoupling');
  const miLabel = scenarioLabelOf(s, 'managed');
  const decLabel = scenarioLabelOf(s, 'decoupling');
  const ids = compactNodeIds(c);
  const statusOf = (id: string, sc: AibStrategicCompactScenario | null) => sc?.nodes.find((t) => t[0] === id)?.[1] ?? null;
  const changed = ids.filter((id) => statusOf(id, mi) !== statusOf(id, dec));
  const intro =
    mi && dec
      ? `Under ${miLabel}, ${interventionWords(c, mi)} and ${fmtUsd(mi.lostUsd)} (${fmtShare(mi.exposure, 0)}) of the compact’s allied demand has to shift; under ${decLabel}, ${interventionWords(c, dec)} and ${fmtUsd(dec.lostUsd)} (${fmtShare(dec.exposure, 0)}) is lost. ${plural(changed.length, 'supply-chain step')} of ${ids.length} ${changed.length === 1 ? 'changes' : 'change'} what it needs between the two. ${scenarioLabelOf(s, 'baseline')}, trade as it is today, is the reference.`
      : `The compact under each scenario the data carry; ${scenarioLabelOf(s, 'baseline')}, trade as it is today, is the reference.`;
  const rows = compareRows(c, name);
  const coverageRows = compared
    .filter((d): d is AibStrategicScenarioDef & { id: AibScenarioId } => d.id === 'baseline' || d.id === 'managed' || d.id === 'decoupling')
    .map((d) => {
      const cov = of(d.id)?.coverage;
      return { id: d.id, ran: true, split: cov ? demandSplit({ static: cov.static, optimized: cov.coordinated }) : null };
    });
  const nodeCsv = () =>
    ids.flatMap((id) => {
      const h = nodeHomeOf(s, id);
      return all.map((d) => {
        const t = of(d.id)?.nodes.find((x) => x[0] === id);
        return {
          strategic_compact_id: c.id,
          value_chain_order: h?.order ?? null,
          value_chain_step: h?.step ?? null,
          node_function: h?.fn ?? null,
          node_id: id,
          node: nodeLabel(id),
          scenario: d.label,
          status: t?.[1] ?? null,
          coverage_coordinated: t?.[2] ?? null,
          new_capacity_host: t?.[3] ? name(t[3]) : null,
          hosts_tied: t?.[4] ?? null,
        };
      });
    });
  return (
    <Section id="scenarios" title={STRATEGIC_SECTION_TITLE.scenarios} intro={<p>{intro}</p>}>
      <div className="card p-5" data-testid="scenario-compare">
        <h3 className="rule-neis-sm text-base font-medium text-paper lg:text-lg">The compact under each scenario</h3>
        <p className="mt-1 text-sm text-paper-mute">The same supply chain and core countries in every column; only the stress and the response differ.</p>
        <div className="mt-3 overflow-x-auto">
          <table className="data-table w-full text-sm">
            <caption className="sr-only">{c.name} under {compared.map((d) => scenarioLabelOf(s, d.id)).join(', ')}</caption>
            <thead>
              <tr>
                <th scope="col" className="text-left">
                  <span className="sr-only">Measure</span>
                </th>
                {compared.map((d) => (
                  <th key={d.id} scope="col" className="min-w-36 text-left align-bottom" aria-current={d.id === sid ? 'true' : undefined} data-scenario={d.id}>
                    {scenarioLabelOf(s, d.id)}
                    {d.id === 'baseline' && <span className="block text-xs font-normal text-paper-mute">reference</span>}
                    {d.id === sid && <span className="block text-xs font-normal text-paper-mute">shown above</span>}
                  </th>
                ))}
              </tr>
            </thead>
            <tbody>
              {rows.map((r) => (
                <tr key={r.key} data-row={r.key}>
                  <th scope="row" className="text-left" style={ROW_TH_STYLE}>
                    {r.label}
                  </th>
                  {compared.map((d) => (
                    <td key={d.id} className={d.id === sid ? 'bg-white/[0.04] text-paper' : d.id === 'baseline' ? 'text-paper-mute' : 'text-paper-dim'}>
                      {r.value(d, of(d.id))}
                    </td>
                  ))}
                </tr>
              ))}
            </tbody>
          </table>
        </div>
        {all.some((d) => d.id === 'partial') && (
          <p className="mt-2 text-xs text-paper-mute" data-testid="partial-note">
            {scenarioLabelOf(s, 'partial')} sits between them
            {strategicScenarioDef(s, 'partial')?.words ? `: ${strategicScenarioDef(s, 'partial')?.words?.replace(/\.?$/, '.')}` : '.'} What each step needs under it is below, and every figure is in
            the CSV.
          </p>
        )}
      </div>
      {coverageRows.length > 0 && (
        <ChartCard
          headingLevel={3}
          title="Allied demand met under each scenario"
          sub="What stays supplied, what coordinated trade recovers and the gap that still needs new capacity: the compact’s allied demand, to scale."
          summary={coverageRows.map((r) => `${scenarioLabelOf(s, r.id)}: ${r.split ? `still supplied ${fmtShare(r.split.kept, 0)}, recovered through trade ${fmtShare(r.split.trade, 0)}, gap that still needs new capacity ${fmtShare(r.split.build, 0)}` : 'no figures'}`).join('; ')}
          csv={() => ({ name: `${c.id} under every scenario`, text: csvWithRun(strategicCsvRows(s, name).filter((r) => r.strategic_compact_id === c.id), doc.meta) })}
        >
          <ScenarioCoverageCompare rows={coverageRows} highlight={scenario} label={`${c.name}: allied demand met under each scenario`} meta={doc.meta} />
        </ChartCard>
      )}
      <ChartCard
        headingLevel={3}
        title="Each supply-chain step under each scenario"
        sub={`The same steps in every column. A step marked “changed” needs something different under ${miLabel} and ${decLabel}.`}
        summary={`${plural(changed.length, 'supply-chain step')} of ${ids.length} ${changed.length === 1 ? 'changes' : 'change'} what it needs between ${miLabel} and ${decLabel}${changed.length ? `: ${listWords(changed.map(nodeLabel))}` : ''}.`}
        legend={<SpineLegend />}
        csv={() => ({ name: `${c.id} node status by scenario`, text: csvWithRun(nodeCsv(), doc.meta) })}
      >
        <div className="overflow-x-auto" data-testid="node-status-grid">
          <table className="data-table w-full text-sm">
            <caption className="sr-only">Each supply-chain step of {c.name} under each scenario</caption>
            <thead>
              <tr>
                <th scope="col" className="text-left">
                  Supply-chain step
                </th>
                {all.map((d) => (
                  <th key={d.id} scope="col" className="min-w-32 text-left align-bottom" aria-current={d.id === sid ? 'true' : undefined}>
                    {scenarioLabelOf(s, d.id)}
                  </th>
                ))}
              </tr>
            </thead>
            <tbody>
              {c.steps.flatMap((st) =>
                st.nodes.map((id) => (
                  <tr key={id} data-node={id}>
                    <th scope="row" className="text-left" style={ROW_TH_STYLE}>
                      <span className="block text-paper">{nodeLabel(id)}</span>
                      <span className="block text-xs text-paper-mute">{st.label}</span>
                      {changed.includes(id) && <ChangedDot />}
                    </th>
                    {all.map((d) => {
                      const st2 = statusOf(id, of(d.id));
                      return (
                        <td key={d.id} className={d.id === sid ? 'bg-white/[0.04]' : undefined}>
                          <span className="inline-flex items-center gap-1.5 text-xs">
                            <SpineStatusSwatch status={st2} size={8} />
                            {st2 ? spineStatusInfo(st2).label : '–'}
                          </span>
                        </td>
                      );
                    })}
                  </tr>
                )),
              )}
            </tbody>
          </table>
        </div>
      </ChartCard>
    </Section>
  );
}
