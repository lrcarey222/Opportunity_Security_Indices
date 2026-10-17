// Allied Industrial Base country roles /aib/roles (IA refactor, 2026-10-16; brief s.18): "What division of
// labour emerges across countries?" From the model's own per-compact roles (derive/countryRoles.ts): first
// the economies that hold each role most often, then the economy x role matrix (Resource supplier,
// Specialized supplier, Investment host, Demand anchor, Bridge, Technology partner, other supplier, and
// compact participation), each cell the number of compacts with the compacts named on hover. Row ids
// `role-ISO3` (rolesHref); each economy links to its Country profile, #aib-role. No new score: the roles
// are the model's labels per compact, not a ranking. Facts: nothing follows the Perspective.

import { useEffect, useMemo } from 'react';
import { Link, useLocation } from 'react-router';
import { ChartCard } from '../../charts/ChartCard.tsx';
import { TableView, type Col } from '../../components/TableView.tsx';
import { EmptyState, PageHead } from '../../components/ui.tsx';
import { useTitle } from '../../shell/title.ts';
import { PAGE, useAibCtx } from '../AibLayout.tsx';
import { countryRoleRows, type CountryRoleRow } from '../derive/countryRoles.ts';
import { plural } from '../derive/format.ts';
import type { RoleName } from '../derive/roles.ts';
import { compactHref } from '../nav.ts';
import { AIB_EYEBROW } from './AibOverviewView.tsx';
import { BasisTag } from './basis.tsx';
import { csvWithRun } from './common.tsx';

/** The matrix columns in the brief's order, with a short header and what the model means by the role. */
export const ROLE_COLUMNS: { role: RoleName; short: string; meaning: string }[] = [
  { role: 'Resource supplier', short: 'Resource supplier', meaning: 'carries a material share of the demand anchors’ coordinated upstream supply, with revealed specialization' },
  { role: 'Specialized supplier', short: 'Specialized supplier', meaning: 'carries a material share of the demand anchors’ coordinated supply, with revealed specialization' },
  { role: 'Capacity-building / investment host', short: 'Investment host', meaning: 'leads the investment ranking of a node that needs new capacity (BUILD)' },
  { role: 'Demand anchor', short: 'Demand anchor', meaning: 'imports most of the supply the shock removes' },
  { role: 'Bridge partner', short: 'Bridge', meaning: 'supplies one node of a compact and depends on another, or spans its stages' },
  { role: 'Technology partner', short: 'Technology partner', meaning: 'shows capability in nodes that need new capacity or managed specialization' },
  { role: 'Supplier', short: 'Other supplier', meaning: 'any other member of the minimum coalition' },
];

const participation = (r: CountryRoleRow) => new Set([...r.member, ...r.associate]).size;

export default function AibRolesView() {
  useTitle('Country roles', 'aib');
  const { doc, names, compactOf } = useAibCtx();
  const loc = useLocation();
  const rows = useMemo(() => countryRoleRows(doc, names), [doc, names]);
  const target = loc.hash.startsWith('#role-') ? loc.hash.slice(6).toUpperCase() : null;

  // follow #role-ISO3 (from a Country profile or a compact page)
  useEffect(() => {
    if (target) document.getElementById(`role-${target}`)?.scrollIntoView?.({ block: 'center' });
  }, [target, rows]);

  const compactWords = (ids: readonly string[]) => ids.map((id) => `${id} ${compactOf(id)?.name ?? ''}`.trim()).join('; ');
  const leaders = ROLE_COLUMNS.map((col) => ({
    ...col,
    top: rows
      .filter((r) => (r.roles[col.role]?.length ?? 0) > 0)
      .sort((a, b) => (b.roles[col.role]?.length ?? 0) - (a.roles[col.role]?.length ?? 0) || a.name.localeCompare(b.name, 'en')),
  })).filter((c) => c.top.length > 0);

  const cols: Col<CountryRoleRow>[] = [
    { id: 'name', header: 'Economy', value: (r) => r.name, cell: (r) => <Link className="link" to={`/countries/${r.iso3}#aib-role`}>{r.name}</Link>, minWidth: 160 },
    ...ROLE_COLUMNS.map((c) => ({ id: c.role, header: c.short, value: (r: CountryRoleRow) => r.roles[c.role]?.length ?? 0, num: true, title: c.meaning })),
    { id: 'member', header: 'Member of', value: (r) => r.member.join(', ') },
    { id: 'associate', header: 'Associate in', value: (r) => r.associate.join(', ') },
  ];
  const csv = () => ({
    name: 'Allied Industrial Base country roles',
    text: csvWithRun(
      rows.map((r) => ({
        iso3: r.iso3,
        economy: r.name,
        ...Object.fromEntries(ROLE_COLUMNS.map((c) => [`${c.short.toLowerCase().replace(/ /g, '_')}_compacts`, (r.roles[c.role] ?? []).join('; ')])),
        member_of: r.member.join('; '),
        associate_in: r.associate.join('; '),
      })),
      doc.meta,
    ),
  });

  return (
    <div className={PAGE}>
      <PageHead
        eyebrow={AIB_EYEBROW}
        title="Country roles"
        lede="What division of labour emerges across countries? For each economy, the roles the model gives it in the Sectoral Compacts and the compacts it belongs to. A role is the model's label for what an economy does in one compact, read from its results: not a ranking or a score."
      />
      <div className="space-y-10">
        <section aria-labelledby="leaders-h">
          <BasisTag basis="facts" />
          <h2 id="leaders-h" className="rule-neis-sm mb-2 mt-1 text-xl font-medium text-paper lg:text-2xl">
            Who does what
          </h2>
          <p className="mb-4 max-w-3xl text-[0.9375rem] leading-relaxed text-paper-dim">
            {plural(rows.length, 'economy', 'economies')} hold a role or a seat in at least one of the {plural(doc.compacts.length, 'compact')}. The economies that hold each role most often:
          </p>
          {leaders.length ? (
            <ul className="grid gap-3 sm:grid-cols-2 lg:grid-cols-3">
              {leaders.map((l) => (
                <li key={l.role} className="card p-4">
                  <h3 className="text-base font-medium text-paper">{l.short}</h3>
                  <p className="mt-0.5 text-xs leading-relaxed text-paper-mute">An economy that {l.meaning}.</p>
                  <p className="mt-2 text-sm text-paper-dim">
                    {l.top.slice(0, 4).map((r, i) => (
                      <span key={r.iso3}>
                        {i > 0 && ', '}
                        <Link className="link" to={`/countries/${r.iso3}#aib-role`}>
                          {r.name}
                        </Link>{' '}
                        <span className="tabular text-paper-mute">({r.roles[l.role]?.length})</span>
                      </span>
                    ))}
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
          sub="Each cell: the number of compacts in which the economy holds the role (hover for the compacts). Participation counts the compacts it belongs to, as a member of the minimum coalition or as an associate that joins only for the highest target. Economy names open the Country profile."
          table={() => <TableView rows={rows} cols={cols} caption="Economies by role across the Sectoral Compacts" search rowKey={(r) => r.iso3} pageSize={25} />}
          csv={csv}
        >
          {rows.length ? (
            <div className="relative overflow-x-auto">
              <table className="data-table">
                <caption className="sr-only">Economies by role across the Sectoral Compacts</caption>
                <thead>
                  <tr>
                    <th scope="col">Economy</th>
                    {ROLE_COLUMNS.map((c) => (
                      <th key={c.role} scope="col" title={c.meaning} className="align-bottom" style={{ minWidth: 72, maxWidth: 110, whiteSpace: 'normal', textAlign: 'center' }}>
                        {c.short}
                      </th>
                    ))}
                    <th scope="col" className="align-bottom" style={{ minWidth: 140, whiteSpace: 'normal' }}>
                      Compact participation
                    </th>
                  </tr>
                </thead>
                <tbody>
                  {rows.map((r) => (
                    <tr key={r.iso3} id={`role-${r.iso3}`} className={`scroll-mt-24 ${target === r.iso3 ? 'bg-signal/10' : ''}`}>
                      <th scope="row" className="whitespace-nowrap text-left" style={{ position: 'sticky', left: 0, zIndex: 2, background: 'var(--color-ink-800)', textTransform: 'none', letterSpacing: 'normal', fontSize: '0.8125rem', fontWeight: 400 }}>
                        <Link className="link" to={`/countries/${r.iso3}#aib-role`}>
                          {r.name}
                        </Link>
                      </th>
                      {ROLE_COLUMNS.map((c) => {
                        const ids = r.roles[c.role] ?? [];
                        return (
                          <td key={c.role} className="text-center tabular">
                            {ids.length ? (
                              <span title={`${c.short}: ${compactWords(ids)}`} className="text-paper">
                                {ids.length}
                                <span className="sr-only"> ({compactWords(ids)})</span>
                              </span>
                            ) : (
                              <span className="text-paper-mute" aria-label="none">
                                ·
                              </span>
                            )}
                          </td>
                        );
                      })}
                      <td className="text-sm">
                        <span className="tabular text-paper">{participation(r)}</span>{' '}
                        <span className="text-xs text-paper-mute">
                          {r.member.map((id, i) => (
                            <span key={id}>
                              {i > 0 && ', '}
                              <Link className="link" to={compactHref(id)} title={compactOf(id)?.name}>
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
            <EmptyState>No economy holds a role in this run.</EmptyState>
          )}
        </ChartCard>
      </div>
    </div>
  );
}
