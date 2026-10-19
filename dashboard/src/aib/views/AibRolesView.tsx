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

import { useEffect, useMemo, useState } from 'react';
import { Link, useLocation } from 'react-router';
import { ChartCard } from '../../charts/ChartCard.tsx';
import { TableView, type Col } from '../../components/TableView.tsx';
import { EmptyState, FilterRow, PageHead, Select } from '../../components/ui.tsx';
import { useTitle } from '../../shell/title.ts';
import { PAGE, useAibCtx } from '../AibLayout.tsx';
import { plural } from '../derive/format.ts';
import { countryRoleMatrix, fmtPts, MATRIX_ROLES, roleHolders, ROLE_FLOOR, ROLE_READING, type CountryRoles, type MatrixRole } from '../derive/network.ts';
import { compactHref } from '../nav.ts';
import { AIB_EYEBROW } from './AibOverviewView.tsx';
import { BasisTag } from './basis.tsx';
import { csvWithRun } from './common.tsx';
import { cellWords, ModelledNote, RoleCellView } from './networkParts.tsx';

/** The matrix columns in the brief's order. */
export const ROLE_COLUMNS = MATRIX_ROLES.map((role) => ({ role, ...ROLE_READING[role] }));

const SIDE_ROLES: readonly MatrixRole[] = ['supplier', 'host', 'investor', 'buyer'];
const participation = (r: CountryRoles) => new Set([...r.member, ...r.associate]).size;

export default function AibRolesView() {
  useTitle('Country roles', 'aib');
  const { doc, names, compactOf, shockLabel } = useAibCtx();
  const loc = useLocation();
  const rows = useMemo(() => countryRoleMatrix(doc, names), [doc, names]);
  const [show, setShow] = useState<MatrixRole | ''>('');
  const shown = useMemo(() => (show ? roleHolders(rows, show) : rows), [rows, show]);
  const target = loc.hash.startsWith('#role-') ? loc.hash.slice(6).toUpperCase() : null;
  const v = doc.network?.variant;

  // follow #role-ISO3 (from a Country profile or a compact page)
  useEffect(() => {
    if (target) document.getElementById(`role-${target}`)?.scrollIntoView?.({ block: 'center' });
  }, [target, rows]);

  const compactWords = (ids: readonly string[]) => ids.map((id) => `${id} ${compactOf(id)?.name ?? ''}`.trim()).join('; ');
  const leaders = ROLE_COLUMNS.map((col) => ({ ...col, top: roleHolders(rows, col.role, 'named') })).filter((c) => c.top.length > 0);
  const sideCols = (k: MatrixRole): Col<CountryRoles>[] =>
    SIDE_ROLES.includes(k)
      ? [
          { id: `${k}-h`, header: `${ROLE_READING[k].short}: headline shock (pts)`, value: (r) => r.cells[k].side?.headline ?? null, cell: (r) => fmtPts(r.cells[k].side?.headline), num: true, title: 'Share of node demand served under the headline shock, summed over nodes, in points' },
          { id: `${k}-c`, header: `${ROLE_READING[k].short}: every shock (pts)`, value: (r) => r.cells[k].side?.cumulative ?? null, cell: (r) => fmtPts(r.cells[k].side?.cumulative), num: true },
          { id: `${k}-p`, header: `${ROLE_READING[k].short}: partners`, value: (r) => r.cells[k].side?.partners ?? null, num: true },
          { id: `${k}-l`, header: `${ROLE_READING[k].short}: read as`, value: (r) => r.cells[k].level },
        ]
      : [{ id: `${k}-l`, header: ROLE_READING[k].short, value: (r) => (r.cells[k].level === 'none' ? 'none' : cellWords(k, r.cells[k])), minWidth: 160 }];
  const cols: Col<CountryRoles>[] = [
    { id: 'name', header: 'Economy', value: (r) => r.name, cell: (r) => <Link className="link" to={`/countries/${r.iso3}#aib-role`}>{r.name}</Link>, minWidth: 160 },
    ...MATRIX_ROLES.flatMap(sideCols),
    { id: 'member', header: 'Member of', value: (r) => r.member.join(', ') },
    { id: 'associate', header: 'Associate in', value: (r) => r.associate.join(', ') },
  ];
  const csv = () => ({
    name: 'Allied Industrial Base country roles',
    text: csvWithRun(
      rows.map((r) => {
        const out: Record<string, unknown> = { iso3: r.iso3, economy: r.name };
        for (const k of MATRIX_ROLES) {
          const c = r.cells[k];
          const key = ROLE_READING[k].short.toLowerCase().replace(/[^a-z]+/g, '_');
          out[`${key}_read_as`] = c.level;
          if (SIDE_ROLES.includes(k)) {
            out[`${key}_headline_contribution`] = c.side?.headline ?? null;
            out[`${key}_cumulative_contribution`] = c.side?.cumulative ?? null;
            out[`${key}_n_partners`] = c.side?.partners ?? null;
            out[`${key}_n_selected`] = c.side?.selected ?? null;
          }
          if (k === 'specialized') out[`${key}_largest_supplier_nodes`] = c.nodes.join('; ');
          out[`${key}_compacts`] = c.compacts.join('; ');
        }
        out.member_of = r.member.join('; ');
        out.associate_in = r.associate.join('; ');
        out.headline_lost_usd = r.net?.lostUsd ?? null;
        return out;
      }),
      doc.meta,
    ),
  });

  return (
    <div className={PAGE}>
      <PageHead
        eyebrow={AIB_EYEBROW}
        title="Country roles"
        lede="What division of labour emerges across countries? For each economy, the roles it plays in the network of trade and investment relationships the model selects after the shock, and the Sectoral Compacts it joins. Roles are read from the model's own results: they are not a ranking or a score."
      />
      <div className="space-y-10">
        <section aria-labelledby="reading-roles-h" className="max-w-4xl">
          <BasisTag basis="facts" />
          <h2 id="reading-roles-h" className="rule-neis-sm mb-2 mt-1 text-xl font-medium text-paper lg:text-2xl">
            How roles are read
          </h2>
          <p className="mb-3 text-[0.9375rem] leading-relaxed text-paper-dim">
            {v
              ? `An economy can hold several roles. A trade or investment role is measured by the share of a supply-chain node's demand its relationships serve after the shock, summed over the nodes: 1 point is 1% of one node's demand, so 100 points is the whole demand of one node. A role is named at ${fmtPts(ROLE_FLOOR)} or more under the headline shock (${shockLabel}); below that it is shown as minor. Specialized supplier and bridge come from the nodes' and compacts' own results.`
              : 'This data build has no economy-level network summary, so the roles are the compacts’ own labels only.'}
          </p>
          <dl className="grid gap-x-6 gap-y-2 text-sm sm:grid-cols-2">
            {ROLE_COLUMNS.map((c) => (
              <div key={c.role}>
                <dt className="font-medium text-paper">{c.short}</dt>
                <dd className="text-paper-dim">
                  An economy that {c.meaning}; {c.named}.
                </dd>
              </div>
            ))}
          </dl>
          <ModelledNote className="mt-3">
            {v
              ? `${shockLabel}, ${v.scenario} scenario, ${(v.supplyConstraint ?? '').replace(/_/g, '-')} supply, ${(v.strategicScope ?? '').replace(/_/g, '-')} technologies; "every shock" sums the ${plural(v.shocks?.length ?? 0, 'configured shock')}`
              : shockLabel}
          </ModelledNote>
        </section>

        <section aria-labelledby="leaders-h">
          <h2 id="leaders-h" className="rule-neis-sm mb-2 mt-1 text-xl font-medium text-paper lg:text-2xl">
            Who does what
          </h2>
          <p className="mb-4 max-w-3xl text-[0.9375rem] leading-relaxed text-paper-dim">
            {plural(rows.length, 'economy', 'economies')} hold a role or a compact seat. For each role, the named economies with the largest value:
          </p>
          {leaders.length ? (
            <ul className="grid gap-3 sm:grid-cols-2 lg:grid-cols-3">
              {leaders.map((l) => (
                <li key={l.role} className="card p-4">
                  <h3 className="text-base font-medium text-paper">{l.short}</h3>
                  <p className="mt-0.5 text-xs leading-relaxed text-paper-mute">
                    {plural(l.top.length, 'economy', 'economies')} named. An economy that {l.meaning}.
                  </p>
                  <p className="mt-2 text-sm text-paper-dim">
                    {l.top.slice(0, 4).map((r, i) => {
                      const c = r.cells[l.role];
                      const val = c.side && c.side.headline >= ROLE_FLOOR ? fmtPts(c.side.headline) : l.role === 'specialized' && c.nodes.length ? plural(c.nodes.length, 'node') : plural(c.compacts.length, 'compact');
                      return (
                        <span key={r.iso3}>
                          {i > 0 && ', '}
                          <Link className="link" to={`/countries/${r.iso3}#aib-role`}>
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
            <EmptyState>The model assigned no roles in this run.</EmptyState>
          )}
        </section>

        <ChartCard
          title="Roles by economy"
          sub="Trade and investment roles: points of node demand served under the headline shock, with the number of partners; specialized supplier: the nodes where the economy is the largest supplier after coordination; bridge: compacts. Faint cells are minor. Compact participation counts the compacts the economy belongs to, as a member of the coalition or as an associate. Hover a cell for every metric; economy names open the Country profile."
          table={() => <TableView rows={shown} cols={cols} caption="Economies by role in the Allied Industrial Base" search rowKey={(r) => r.iso3} pageSize={25} />}
          csv={csv}
        >
          <FilterRow result={show ? `${plural(shown.length, 'economy', 'economies')} with this role, named first, largest value first` : `${plural(rows.length, 'economy', 'economies')}, by compact participation`}>
            <Select
              label="Show"
              value={show}
              options={[{ value: '', label: 'Every economy' }, ...ROLE_COLUMNS.map((c) => ({ value: c.role, label: c.short }))]}
              onChange={(x) => setShow(x as MatrixRole | '')}
              className="min-w-[14rem]"
            />
          </FilterRow>
          {shown.length ? (
            <div className="relative overflow-x-auto" data-testid="role-matrix">
              <table className="data-table">
                <caption className="sr-only">Economies by role in the Allied Industrial Base</caption>
                <thead>
                  <tr>
                    <th scope="col">Economy</th>
                    {ROLE_COLUMNS.map((c) => (
                      <th key={c.role} scope="col" title={`An economy that ${c.meaning}; ${c.named}`} className="align-bottom" style={{ minWidth: 84, maxWidth: 120, whiteSpace: 'normal', textAlign: 'center' }}>
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
                        <Link className="link" to={`/countries/${r.iso3}#aib-role`}>
                          {r.name}
                        </Link>
                      </th>
                      {ROLE_COLUMNS.map((c) => (
                        <td key={c.role} className="text-center text-sm">
                          <RoleCellView k={c.role} cell={r.cells[c.role]} />
                        </td>
                      ))}
                      <td className="text-sm">
                        <span className="tabular text-paper">{participation(r)}</span>{' '}
                        <span className="text-xs text-paper-mute">
                          {r.member.map((id, i) => (
                            <span key={id}>
                              {i > 0 && ', '}
                              <Link className="link" to={compactHref(id)} title={compactWords([id])}>
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
            <EmptyState>No economy holds this role in this run.</EmptyState>
          )}
        </ChartCard>
      </div>
    </div>
  );
}
