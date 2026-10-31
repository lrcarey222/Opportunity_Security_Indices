// Allied Industrial Base country roles /aib/roles (IA refactor, 2026-10-16; brief s.18; rebuilt 2026-10-17 on
// the emergent network): "What division of labour emerges across countries?" From the economy-level summary
// of the headline run (doc.network: emergent_aib_country_summary + emergent_aib_edges) beside the model's
// per-compact roles (derive/network.ts): how roles are read, the economies that hold each role, then the
// economy x role matrix (Resource / Supplier, Specialized supplier, Investment host, Investor, Demand
// anchor / Buyer, Bridge, Compact participation). A cell is the role's own metric (the share of node
// demand served under the headline shock, with the partner count; nodes or compacts for the compact-based
// roles); a role is named only where the metrics support it. Row ids `role-ISO3` (rolesHref); each economy
// links to its Country profile, #aib-role. No new score, no ranking. Modelled facts: nothing follows the
// Perspective.
//
// The scenarios (2026-10-19): the page follows the toggle. Decoupling reads the main document's network
// through headlineNetwork() (only relationships with a headline contribution; a role is minor only where
// the headline shock contributes below the floor) and keeps its every-shock totals in the table, labelled
// as Decoupling's supply shocks; Managed Interdependence reads its lazily loaded scenario file (network,
// compacts, the largest supplier of node.diversify), with loading, failed and missing states. Today's
// network (the Baseline file, loaded on this page) is the reference: "What changes from today" sets who
// supplies whom today beside the scenario, and the roles each economy gains or loses.
//
// The strategic layer (2026-10-20; task spec s.17): with doc.strategic the page opens on "Strategic roles"
// (strategicRoles/StrategicRolesSection.tsx): the chosen economy's role in the Allied Industrial Base (its
// primary strategic roles, what it contributes and needs, its most important strategic compacts, its
// highest-value partners, where new capacity matters) with "How the model derives this" (both gates of
// every role, then this page's operational evidence for the economy under the scenario), and who holds
// each strategic role. The operational part above follows unchanged under "Operational network roles". A
// bundle without the layer shows the operational part only.

import { useEffect, useMemo, useState } from 'react';
import { Link, useLocation } from 'react-router';
import { ChartCard } from '../../charts/ChartCard.tsx';
import { TableView, type Col } from '../../components/TableView.tsx';
import { EmptyState, FilterRow, PageHead, Select } from '../../components/ui.tsx';
import { MODULE_QUESTION, ROLES_FRAMING } from '../../content/vocabulary.ts';
import { useTitle } from '../../shell/title.ts';
import { PAGE, useAibCtx } from '../AibLayout.tsx';
import { ScenarioTag } from '../charts/ScenarioCompare.tsx';
import { scenarioNamesOf } from '../contract.ts';
import { plural } from '../derive/format.ts';
import { strategicScopeShort } from '../derive/settings.ts';
import {
  countryRoleMatrix, decoupledEconomy, everyShock, everyShockLabel, fmtPts, MATRIX_ROLES, roleChanges, roleHolders, roleNamedWords, ROLE_FLOOR, ROLE_READING,
  scenarioRoleSource, type CountryRoles, type MatrixRole, type RoleCell,
} from '../derive/network.ts';
import { runMetaOf, scenarioCaveat, scenarioCsvFields, scenarioFileTag, scenarioInfo, scenarioShockLabel } from '../derive/scenarios.ts';
import { compactHref } from '../nav.ts';
import { AIB_EYEBROW } from './AibOverviewView.tsx';
import { BasisTag } from './basis.tsx';
import { csvWithRun, QuestionLede } from './common.tsx';
import { cellWords, ModelledNote, OperationalRoleList, RoleCellView, ScenarioFileState, useBaselineDoc, useManagedDoc } from './networkParts.tsx';
import { StrategicRolesSection } from './strategicRoles/StrategicRolesSection.tsx';
import { RoleChangesCard, WhoSuppliesWhomCard } from './scenarioNetwork.tsx';

/** The page's title (the guide, Country roles). */
export const ROLES_PAGE_TITLE = 'Country roles in the Allied Industrial Base';

/** The matrix columns in the brief's order. */
export const ROLE_COLUMNS = MATRIX_ROLES.map((role) => ({ role, ...ROLE_READING[role] }));

const SIDE_ROLES: readonly MatrixRole[] = ['supplier', 'host', 'investor', 'buyer'];
const participation = (r: CountryRoles) => new Set([...r.member, ...r.associate]).size;
/** A side's partners under the scenario: the headline ones where the network's own count sums every shock. */
const partnersOf = (c: RoleCell): number | null => (c.side ? (c.headlinePartners ?? c.side.partners) : null);

export default function AibRolesView() {
  useTitle('Country roles', 'aib');
  const { doc, names, name: docName, compactOf, shockLabel, title, scenario, hasScenarios, href, strategic } = useAibCtx();
  const loc = useLocation();
  const meta = doc.meta;
  const mi = useManagedDoc(scenario === 'managed');
  const today = useBaselineDoc(hasScenarios);
  const src = useMemo(() => scenarioRoleSource(doc, scenario, mi.doc), [doc, scenario, mi.doc]);
  // the scenario files name the economies only they use (today's network reaches economies the model never selects)
  const nm = useMemo(
    () => ({ ...(today.doc ? scenarioNamesOf(today.doc) : {}), ...(scenario === 'managed' && mi.doc ? scenarioNamesOf(mi.doc) : {}), ...names }),
    [today.doc, scenario, mi.doc, names],
  );
  const rows = useMemo(() => (src ? countryRoleMatrix(doc, nm, src) : []), [doc, nm, src]);
  const [show, setShow] = useState<MatrixRole | ''>('');
  const shown = useMemo(() => (show ? roleHolders(rows, show) : rows), [rows, show]);
  const target = loc.hash.startsWith('#role-') ? loc.hash.slice(6).toUpperCase() : null;
  const net = src?.network ?? null;
  const v = net?.variant;
  const info = scenarioInfo(meta, scenario);
  // the words of the scenario: "under Major-supplier disruption"; a bundle without the scenarios keeps "the headline disruption"
  const under = hasScenarios ? `under ${info.label}` : 'under the headline disruption';
  const shockWords = scenario === 'managed' ? scenarioShockLabel(meta, 'managed') : shockLabel;
  // what the Decoupling network's "every shock" figures sum (Managed Interdependence has none: cum = headline)
  const label = useMemo(() => everyShockLabel(doc), [doc]);
  const plain = useMemo(() => everyShock(doc), [doc]);
  const every = scenario === 'decoupling' ? (hasScenarios ? label.phrase : plain.phrase) : null;
  // an older bundle keeps its run note; with the scenarios the every-shock totals get a sentence of their own
  const everyNote = scenario === 'decoupling' && !hasScenarios ? plain.note : null;
  const everyLine =
    scenario === 'decoupling' && hasScenarios ? `${label.note} They appear only in the table and in each role's details; every role is read on ${info.label} alone.` : null;
  const everyHeader = hasScenarios ? label.header : 'every shock';
  const removed = useMemo(() => (scenario === 'decoupling' ? decoupledEconomy(meta, names) : null), [scenario, meta, names]);
  const changes = useMemo(
    () => (src && today.doc?.network ? roleChanges({ scenario, today: today.doc.network, network: src.network, nodes: doc.nodes, names: nm, removed }) : null),
    [src, today.doc, scenario, doc.nodes, nm, removed],
  );

  // follow #role-ISO3 (from a Country profile or a compact page)
  useEffect(() => {
    if (target) document.getElementById(`role-${target}`)?.scrollIntoView?.({ block: 'center' });
  }, [target, rows]);

  const name = (iso: string) => nm[iso] ?? iso;
  const compactWords = (ids: readonly string[]) =>
    ids
      .map((id) => {
        const c = compactOf(id);
        return `${id} ${c ? title(c) : ''}`.trim();
      })
      .join('; ');
  const leaders = ROLE_COLUMNS.map((col) => ({ ...col, top: roleHolders(rows, col.role, 'named') })).filter((c) => c.top.length > 0);
  const hHead = hasScenarios ? info.label : 'headline disruption';
  const sideCols = (k: MatrixRole): Col<CountryRoles>[] =>
    SIDE_ROLES.includes(k)
      ? [
          { id: `${k}-h`, header: `${ROLE_READING[k].short}: ${hHead} (pts)`, value: (r) => r.cells[k].side?.headline ?? null, cell: (r) => fmtPts(r.cells[k].side?.headline), num: true, title: `Share of a supply-chain step's demand served ${under}, summed over steps, in points` },
          ...(every !== null
            ? [{ id: `${k}-c`, header: `${ROLE_READING[k].short}: ${everyHeader} (pts)`, value: (r: CountryRoles) => r.cells[k].side?.cumulative ?? null, cell: (r: CountryRoles) => fmtPts(r.cells[k].side?.cumulative), num: true, title: `Share of a supply-chain step's demand served across ${every}, summed over steps, in points` } as Col<CountryRoles>]
            : []),
          { id: `${k}-p`, header: `${ROLE_READING[k].short}: partners`, value: (r) => partnersOf(r.cells[k]), num: true, title: `Distinct partners of the relationships ${under}` },
          { id: `${k}-l`, header: `${ROLE_READING[k].short}: read as`, value: (r) => r.cells[k].level },
        ]
      : [{ id: `${k}-l`, header: ROLE_READING[k].short, value: (r) => (r.cells[k].level === 'none' ? 'none' : cellWords(k, r.cells[k], every, under)), minWidth: 160 }];
  const cols: Col<CountryRoles>[] = [
    { id: 'name', header: 'Economy', value: (r) => r.name, cell: (r) => <Link className="link" to={href(`/countries/${r.iso3}#aib-role`)}>{r.name}</Link>, minWidth: 160 },
    ...MATRIX_ROLES.flatMap(sideCols),
    { id: 'member', header: 'Member of', value: (r) => r.member.join(', ') },
    { id: 'associate', header: 'Associate in', value: (r) => r.associate.join(', ') },
  ];
  const runMeta = runMetaOf(meta, scenario);
  const scnCsv = hasScenarios ? scenarioCsvFields(meta, scenario) : null;
  const csv = () => ({
    name: `Allied Industrial Base country roles${scenarioFileTag(scenario, meta, hasScenarios)}`,
    text: csvWithRun(
      rows.map((r) => {
        const out: Record<string, unknown> = { iso3: r.iso3, economy: r.name };
        for (const k of MATRIX_ROLES) {
          const c = r.cells[k];
          const key = ROLE_READING[k].short.toLowerCase().replace(/[^a-z]+/g, '_');
          out[`${key}_read_as`] = c.level;
          if (SIDE_ROLES.includes(k)) {
            out[`${key}_headline_contribution`] = c.side?.headline ?? null;
            if (every !== null) out[`${key}_cumulative_contribution`] = c.side?.cumulative ?? null;
            out[`${key}_n_partners`] = partnersOf(c);
            // read on the headline, the network's own counts sum every shock: kept, named so
            if (c.headlinePartners !== undefined && every !== null) {
              out[`${key}_n_partners_cumulative`] = c.side?.partners ?? null;
              out[`${key}_n_selected_cumulative`] = c.side?.selected ?? null;
            } else {
              out[`${key}_n_selected`] = c.side?.selected ?? null;
            }
          }
          if (k === 'specialized') out[`${key}_largest_supplier_nodes`] = c.nodes.join('; ');
          out[`${key}_compacts`] = c.compacts.join('; ');
        }
        out.member_of = r.member.join('; ');
        out.associate_in = r.associate.join('; ');
        out.headline_lost_usd = r.net?.lostUsd ?? null;
        if (scnCsv) {
          out.aib_scenario = scnCsv.aib_scenario;
          out.aib_scenario_shock = scnCsv.aib_scenario_shock;
        }
        return out;
      }),
      runMeta,
    ),
  });

  const pending = !src;

  // the operational evidence under a strategic summary: the economy's matrix row under the scenario
  const renderName = (iso: string | null, label: string) =>
    iso ? (
      <Link className="link" to={href(`/countries/${iso}#aib-role`)}>
        {label}
      </Link>
    ) : (
      <span className="text-paper">{label}</span>
    );
  const evidenceFor = (iso: string) => {
    const r = rows.find((x) => x.iso3 === iso);
    return (
      <div className="text-sm" data-testid="operational-evidence">
        <p className="mb-1 font-medium text-paper">Operational network roles {under}</p>
        <p className="mb-2 text-xs leading-relaxed text-paper-mute">
          The evidence beneath the strategic roles: what the country does in the network of trade and investment relationships the analysis selects. Unlike the strategic roles,
          these follow the scenario. The table below sets every country side by side.
        </p>
        {pending ? (
          <p className="text-paper-mute">{mi.error || mi.missing ? `Not available for ${info.label} in this view: see the note below.` : `Loading the roles ${under}…`}</p>
        ) : r ? (
          <>
            <OperationalRoleList roles={r} every={every} under={under} />
            <p className="mt-2 text-xs text-paper-mute">
              In the coalition of {plural(r.member.length, 'workstream')}
              {r.associate.length ? `; an associate in ${r.associate.length} more` : ''}
              {r.member.length ? ': ' : '.'}
              {r.member.map((id, i) => (
                <span key={id}>
                  {i > 0 && ', '}
                  <Link className="link" to={compactHref(id, scenario)} title={compactWords([id])}>
                    {id}
                  </Link>
                </span>
              ))}
            </p>
          </>
        ) : (
          <p className="text-paper-mute">No role in the network {under}, and no seat in a workstream&apos;s coalition.</p>
        )}
      </div>
    );
  };

  return (
    <div className={PAGE}>
      <PageHead
        eyebrow={AIB_EYEBROW}
        title={ROLES_PAGE_TITLE}
        lede={
          <QuestionLede question={MODULE_QUESTION.roles}>
            {`${ROLES_FRAMING.lede} ${ROLES_FRAMING.key}`}
          </QuestionLede>
        }
      >
        <p className="mt-3 max-w-3xl text-sm text-paper-mute" data-testid="roles-reading">
          {strategic
            ? `Strategic roles come first: the two or three main functions each country performs, what it contributes and needs, its most important compacts and partners, the same in every scenario. The operational roles beneath them are what it does in the network of trade and investment relationships the analysis selects ${hasScenarios ? under : 'after the disruption'}${hasScenarios ? ', set against trade as it is today' : ''}. `
            : `For each country, the roles it plays in the network of trade and investment relationships the analysis selects ${hasScenarios ? under : 'after the disruption'}, and the Sectoral Compacts it joins${hasScenarios ? ', set against trade as it is today' : ''}. `}
          {ROLES_FRAMING.derived} They are not a ranking or a score.
        </p>
      </PageHead>
      <div className="space-y-10">
        {strategic && (
          <StrategicRolesSection s={strategic} nodes={doc.nodes} names={names} name={docName} scenario={scenario} renderName={renderName} evidenceFor={evidenceFor} />
        )}

        {strategic && (
          <div className="max-w-4xl border-t hairline pt-8" data-testid="operational-intro">
            <h2 id="operational-roles-h" className="rule-neis-sm mb-2 mt-1 text-xl font-medium text-paper lg:text-2xl">
              Operational network roles
            </h2>
            <p className="text-[0.9375rem] leading-relaxed text-paper-dim">
              The evidence beneath the strategic roles: the supplier, buyer, investor, host and bridge roles each country plays in the network of trade and investment relationships the
              analysis selects {under}, and the workstreams whose coalitions it joins. Unlike the strategic roles, they follow the scenario.
            </p>
          </div>
        )}

        <section aria-labelledby="reading-roles-h" className="max-w-4xl">
          <BasisTag basis={hasScenarios ? 'model' : 'facts'} scenario={hasScenarios ? info.label : null} />
          <h2 id="reading-roles-h" className="rule-neis-sm mb-2 mt-1 text-xl font-medium text-paper lg:text-2xl">
            How roles are read
          </h2>
          <p className="mb-3 text-[0.9375rem] leading-relaxed text-paper-dim">
            {v || pending
              ? `A country can hold several roles. A trade or investment role is measured by the share of a supply-chain step's demand its relationships serve ${hasScenarios ? under : 'after the disruption'}, summed over the steps: 1 point is 1% of one step's demand, so 100 points is the whole demand of one step. A role is named at ${fmtPts(ROLE_FLOOR)} or more ${under} (${shockWords}); below that it is shown as minor. Specialized supplier and bridge come from the steps' and compacts' own results.${scenario === 'managed' ? ` ${info.label} removes no supplier: its network is the trade and new capacity that replace the supply held above each step's cap.` : ''}`
              : 'This build has no country-level network summary, so the roles are the compacts’ own labels only.'}
          </p>
          <dl className="grid gap-x-6 gap-y-2 text-sm sm:grid-cols-2">
            {ROLE_COLUMNS.map((c) => (
              <div key={c.role}>
                <dt className="font-medium text-paper">{c.short}</dt>
                <dd className="text-paper-dim">
                  A country that {c.meaning}; {roleNamedWords(c.role, under)}.
                </dd>
              </div>
            ))}
          </dl>
          <ModelledNote className="mt-3">
            {v
              ? `${shockWords}; supply assumption: ${v.scenario}, ${(v.supplyConstraint ?? '').replace(/_/g, '-')} supply, ${strategicScopeShort(meta, v.strategicScope)}${everyNote ? `; ${everyNote}` : ''}`
              : shockWords}
          </ModelledNote>
          {everyLine && (
            <p className="mt-1 text-xs leading-relaxed text-paper-mute" data-testid="every-shock-note">
              {everyLine}
            </p>
          )}
        </section>

        {pending ? (
          <ScenarioFileState load={mi} what="the country roles" label={info.label} />
        ) : (
          <section aria-labelledby="leaders-h">
            <h2 id="leaders-h" className="rule-neis-sm mb-2 mt-1 text-xl font-medium text-paper lg:text-2xl">
              Who does what
            </h2>
            <p className="mb-4 max-w-3xl text-[0.9375rem] leading-relaxed text-paper-dim">
              {plural(rows.length, 'country', 'countries')} hold a role or a compact seat{hasScenarios ? ` ${under}` : ''}. For each role, the named countries with the largest value:
            </p>
            {leaders.length ? (
              <ul className="grid gap-3 sm:grid-cols-2 lg:grid-cols-3">
                {leaders.map((l) => (
                  <li key={l.role} className="card p-4">
                    <h3 className="text-base font-medium text-paper">{l.short}</h3>
                    <p className="mt-0.5 text-xs leading-relaxed text-paper-mute">
                      {plural(l.top.length, 'country', 'countries')} named. A country that {l.meaning}.
                    </p>
                    <p className="mt-2 text-sm text-paper-dim">
                      {l.top.slice(0, 4).map((r, i) => {
                        const c = r.cells[l.role];
                        const val = c.side && c.side.headline >= ROLE_FLOOR ? fmtPts(c.side.headline) : l.role === 'specialized' && c.nodes.length ? plural(c.nodes.length, 'step') : plural(c.compacts.length, 'compact');
                        return (
                          <span key={r.iso3}>
                            {i > 0 && ', '}
                            <Link className="link" to={href(`/countries/${r.iso3}#aib-role`)}>
                              {r.name}
                            </Link>{' '}
                            <span className="tabular text-paper-mute">({val})</span>
                          </span>
                        );
                      })}
                      {l.top.length > 4 ? ` and ${l.top.length - 4} more` : ''}
                    </p>
                  </li>
                ))}
              </ul>
            ) : (
              <EmptyState>No country holds a named role under this scenario.</EmptyState>
            )}
          </section>
        )}

        {hasScenarios && !pending && (
          <section aria-labelledby="changes-h" className="space-y-6" data-testid="roles-today">
            <div className="max-w-4xl">
              <BasisTag basis="today" />
              <h2 id="changes-h" className="rule-neis-sm mb-2 mt-1 text-xl font-medium text-paper lg:text-2xl">
                What changes from today
              </h2>
              <p className="text-[0.9375rem] leading-relaxed text-paper-dim">
                Today&apos;s network is the trade as it is ({meta.tradeYear}): who supplies whom, before any disruption. {info.label}&apos;s network is the trade and new capacity it adds or
                redirects to replace the supply {scenario === 'managed' ? 'held above each step’s cap' : 'it removes'}. The two measure different things, so they stand side by side, each on its own
                scale. {scenarioCaveat(meta)}
              </p>
            </div>
            {today.doc ? (
              today.doc.network && net ? (
                <>
                  <WhoSuppliesWhomCard today={today.doc.network} network={net} scenario={scenario} meta={meta} name={name} />
                  {changes && <RoleChangesCard cmp={changes} scenario={scenario} meta={meta} href={href} />}
                </>
              ) : (
                <EmptyState>{today.doc.network ? `This build has no network ${under}.` : 'This build has no network of today’s trade.'}</EmptyState>
              )
            ) : (
              <ScenarioFileState load={today} what="today's network" label={scenarioInfo(meta, 'baseline').label} />
            )}
          </section>
        )}

        {!pending && (
          <ChartCard
            title="Roles by economy"
            badge={hasScenarios ? <ScenarioTag id={scenario} meta={meta} /> : undefined}
            sub={`Trade and investment roles: points of supply-chain-step demand served ${under}, with the number of partners; specialized supplier: the steps where the country is the largest supplier after coordination; bridge: compacts. Faint cells are minor. Compact participation counts the compacts the country belongs to, as a member of the coalition or as an associate. Hover a cell for every metric; country names open the Country profile.`}
            table={() => <TableView rows={shown} cols={cols} caption={`Economies by role in the Allied Industrial Base${hasScenarios ? `, ${under}` : ''}`} search rowKey={(r) => r.iso3} pageSize={25} />}
            csv={csv}
          >
            <FilterRow result={show ? `${plural(shown.length, 'country', 'countries')} with this role, named first, largest value first` : `${plural(rows.length, 'country', 'countries')}, by compact participation`}>
              <Select
                label="Show"
                value={show}
                options={[{ value: '', label: 'Every country' }, ...ROLE_COLUMNS.map((c) => ({ value: c.role, label: c.short }))]}
                onChange={(x) => setShow(x as MatrixRole | '')}
                className="min-w-[14rem]"
              />
            </FilterRow>
            {shown.length ? (
              <div className="relative overflow-x-auto" data-testid="role-matrix">
                <table className="data-table">
                  <caption className="sr-only">Economies by role in the Allied Industrial Base{hasScenarios ? `, ${under}` : ''}</caption>
                  <thead>
                    <tr>
                      <th scope="col">Economy</th>
                      {ROLE_COLUMNS.map((c) => (
                        <th key={c.role} scope="col" title={`A country that ${c.meaning}; ${roleNamedWords(c.role, under)}`} className="align-bottom" style={{ minWidth: 84, maxWidth: 120, whiteSpace: 'normal', textAlign: 'center' }}>
                          {c.short}
                        </th>
                      ))}
                      <th scope="col" className="align-bottom" style={{ minWidth: 140, whiteSpace: 'normal' }}>
                        Compact participation
                      </th>
                    </tr>
                  </thead>
                  <tbody>
                    {shown.map((r) => (
                      <tr key={r.iso3} id={`role-${r.iso3}`} className={`scroll-mt-24 ${target === r.iso3 ? 'bg-signal/10' : ''}`}>
                        <th scope="row" className="whitespace-nowrap text-left" style={{ position: 'sticky', left: 0, zIndex: 2, background: 'var(--color-ink-800)', textTransform: 'none', letterSpacing: 'normal', fontSize: '0.8125rem', fontWeight: 400 }}>
                          <Link className="link" to={href(`/countries/${r.iso3}#aib-role`)}>
                            {r.name}
                          </Link>
                        </th>
                        {ROLE_COLUMNS.map((c) => (
                          <td key={c.role} className="text-center text-sm">
                            <RoleCellView k={c.role} cell={r.cells[c.role]} every={every} under={under} />
                          </td>
                        ))}
                        <td className="text-sm">
                          <span className="tabular text-paper">{participation(r)}</span>{' '}
                          <span className="text-xs text-paper-mute">
                            {r.member.map((id, i) => (
                              <span key={id}>
                                {i > 0 && ', '}
                                <Link className="link" to={compactHref(id, scenario)} title={compactWords([id])}>
                                  {id}
                                </Link>
                              </span>
                            ))}
                            {r.associate.length > 0 && ` (associate: ${r.associate.join(', ')})`}
                          </span>
                        </td>
                      </tr>
                    ))}
                  </tbody>
                </table>
              </div>
            ) : (
              <EmptyState>No country holds this role under this scenario.</EmptyState>
            )}
          </ChartCard>
        )}
      </div>
    </div>
  );
}
