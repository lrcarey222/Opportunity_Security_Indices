// The pieces of a country's strategic summary (2026-10-20; task spec s.17-18), shared by the AIB Country
// roles page (/aib/roles: the summary card above the operational table) and the Country profile's "Role
// in the Allied Industrial Base": the Strategic AIB Roles held, the items of a narrative slot (each node
// with the strategic compact it sits in, linked to the compact's page), what the economy needs with the
// allied economies that could supply it, its roles in the strategic compacts, its highest-value partners
// with the pair's relationship thesis, where new capacity matters, and "How the model derives this" (the
// two gates of every role and the rule). Plain words, no score; lists wrap at phone width; every compact
// link carries the scenario toggle. The data come from ./derive.ts (pure).

import type { ReactNode } from 'react';
import { Link } from 'react-router';
import type { AibCompactRole, AibStrategic } from '../../contract.ts';
import { cap } from '../../../country/derive/text.ts';
import { listWords } from '../../derive/format.ts';
import { strategicCompactOf } from '../../derive/strategic.ts';
import { pairHref, strategicCompactHref } from '../../nav.ts';
import type { AibToggle } from '../../scenarioUrl.ts';
import {
  compactRoleWords, gateWords, heldRoles, logicWords, otherFunctions, ROLE_MEANING, roleRuleWords, roleStatusWords, type NeedEntry, type PartnerEntry, type SlotEntry, type StrategicSummary,
} from './derive.ts';

const pct = (x: number): string => `${Math.round(x * 100)}%`;

/** Renders an economy's name (a link to its page where the context has one). */
export type NameRenderer = (iso3: string | null, name: string) => ReactNode;

/** A compact's name as a link to its strategic compact page. */
export function CompactLink({ id, name, scenario }: { id: string; name: string; scenario: AibToggle }) {
  return (
    <Link className="link" to={strategicCompactHref(id, scenario)}>
      {name}
    </Link>
  );
}

/** The headline sentence of the summary ("Combines ... roles; secondary ... role."). */
export function headlineSentence(sum: StrategicSummary): string {
  return `${sum.headline}${sum.secondaryLine ? `; ${sum.secondaryLine}` : ''}.`;
}

// `.tag` is unlayered CSS (white-space: nowrap, its own border): Tailwind utilities on it lose, so the
// accent and the wrapping of a long role name are inline (the accent as the current chip's colours)
const PRIMARY_CHIP = { whiteSpace: 'normal', borderColor: 'var(--color-signal)', color: 'var(--color-paper)' } as const;

/** The Strategic AIB Roles an economy holds: primary as accented chips, then secondary and beyond the cap in words. */
export function HeldRoles({ sum }: { sum: StrategicSummary }) {
  const { primary, secondary, beyondCap } = sum.cs;
  return (
    <div className="space-y-1.5" data-testid="strategic-held-roles">
      {primary.length ? (
        <ul className="flex flex-wrap gap-1.5" aria-label="Primary strategic roles">
          {primary.map((r) => (
            <li key={r.code}>
              <span className="tag max-w-full" style={PRIMARY_CHIP} title={ROLE_MEANING[r.code]}>
                {r.name}
              </span>
            </li>
          ))}
        </ul>
      ) : (
        <p className="text-paper-dim">No primary strategic role at the current thresholds.</p>
      )}
      {secondary.length > 0 && <p className="text-xs text-paper-mute">Secondary: {listWords(secondary.map((r) => r.name))}.</p>}
      {beyondCap.length > 0 && <p className="text-xs text-paper-mute">Also meets the bar, beyond the cap: {listWords(beyondCap.map((r) => r.name))}.</p>}
    </div>
  );
}

/** The items of a narrative slot: each node, its figure, and the strategic compact it sits in. */
export function EntryList({ entries, scenario, label }: { entries: readonly SlotEntry[]; scenario: AibToggle; label: string }) {
  return (
    <ul className="space-y-1.5 text-sm" aria-label={label}>
      {entries.map((e) => (
        <li key={e.label} className="min-w-0 break-words leading-snug">
          <span className="text-paper">{e.label}</span>
          {e.detail && <span className="tabular text-paper-mute"> · {e.detail}</span>}
          {e.compact && (
            <span className="block text-xs text-paper-mute">
              in <CompactLink id={e.compact.id} name={e.compact.name} scenario={scenario} />
            </span>
          )}
        </li>
      ))}
    </ul>
  );
}

/** What the economy needs: each node, its share of allied demand, and the allied economies with a supply position in it. */
export function NeedList({ needs, scenario, name, renderName }: { needs: readonly NeedEntry[]; scenario: AibToggle; name: (iso: string) => string; renderName: NameRenderer }) {
  return (
    <ul className="space-y-1.5 text-sm" aria-label="What it needs">
      {needs.map((e) => (
        <li key={e.label} className="min-w-0 break-words leading-snug">
          <span className="text-paper">{e.label}</span>
          {e.detail && <span className="tabular text-paper-mute"> · {e.detail}</span>}
          <span className="block text-xs text-paper-mute">
            {e.suppliers.length ? (
              <>
                Allied suppliers with a position:{' '}
                {e.suppliers.map((x, i) => (
                  <span key={x.iso3}>
                    {i > 0 && ', '}
                    {renderName(x.iso3, name(x.iso3))} ({pct(x.share)} of world supply)
                  </span>
                ))}
              </>
            ) : (
              'No allied economy holds 2% or more of world supply'
            )}
            {e.compact && (
              <>
                {' '}
                · in <CompactLink id={e.compact.id} name={e.compact.name} scenario={scenario} />
              </>
            )}
          </span>
        </li>
      ))}
    </ul>
  );
}

/** The economy's roles in the strategic compacts (the most relevant first; `limit`: the first n). */
export function CompactRoleList({ roles, s, scenario, limit, detail = false }: { roles: readonly AibCompactRole[]; s: AibStrategic; scenario: AibToggle; limit?: number; detail?: boolean }) {
  const shown = limit ? roles.slice(0, limit) : roles;
  return (
    <ul className="space-y-2 text-sm" aria-label="Roles in the strategic compacts">
      {shown.map((r) => {
        const c = strategicCompactOf(s, r.compact);
        const others = otherFunctions(r);
        const core = c?.core.some((x) => x.iso3 === r.iso3) ?? false;
        return (
          <li key={r.compact} className="min-w-0 break-words leading-snug" data-compact={r.compact}>
            {c ? <CompactLink id={c.id} name={c.name} scenario={scenario} /> : <span className="text-paper">{r.compact}</span>}
            {core && <span className="text-xs text-paper-mute"> · core country</span>}
            <span className="block text-paper-dim">{compactRoleWords(r)}</span>
            {detail && others.length > 0 && <span className="block text-xs text-paper-mute">Also: {others.map((f) => f.toLowerCase()).join(', ')}</span>}
            {detail && r.keyNodes.length > 0 && <span className="block text-xs text-paper-mute">Key nodes: {r.keyNodes.join('; ')}</span>}
            {r.criticality && <span className="block text-xs text-paper-mute">{cap(r.criticality)}</span>}
          </li>
        );
      })}
    </ul>
  );
}

/** The highest-value partners, each with the pair's relationship logic and priority compacts (`detail`: what each side contributes in them). */
export function PartnerList({
  partners, scenario, self, name, renderName, detail = false,
}: {
  partners: readonly PartnerEntry[];
  scenario: AibToggle;
  self: string;
  name: (iso: string) => string;
  renderName: NameRenderer;
  detail?: boolean;
}) {
  return (
    <ul className="space-y-2 text-sm" aria-label="Highest-value partners">
      {partners.map((p) => {
        const t = p.thesis;
        return (
          <li key={p.name} className="min-w-0 break-words leading-snug" data-partner={p.iso3 ?? ''}>
            {renderName(p.iso3, p.name)}
            {t ? (
              <>
                <span className="block text-xs text-paper-mute">
                  {logicWords(t.logic)}
                  {t.compacts.length > 0 && (
                    <>
                      {' '}
                      · priority compacts:{' '}
                      {t.compacts.map((c, i) => (
                        <span key={c.id}>
                          {i > 0 && ', '}
                          <CompactLink id={c.id} name={c.name} scenario={scenario} />
                        </span>
                      ))}
                    </>
                  )}
                </span>
                {detail && t.complementarity.length > 0 && (
                  <ul className="mt-1 space-y-0.5 text-xs text-paper-mute" aria-label={`What each side contributes: ${name(self)} and ${p.name}`}>
                    {t.complementarity.map((x) => (
                      <li key={x.compact.id}>
                        {x.compact.name}: {name(self)} {x.a ? `as ${x.a.role.toLowerCase()}` : 'with no compact role'}, {p.name} {x.b ? `as ${x.b.role.toLowerCase()}` : 'with no compact role'}
                      </li>
                    ))}
                  </ul>
                )}
              </>
            ) : null}
            {p.iso3 && (
              <Link className="link mt-0.5 block text-xs" to={pairHref(self, p.iso3, scenario)} aria-label={`The relationship between ${name(self)} and ${p.name} in Partnerships`}>
                The relationship in Partnerships &rarr;
              </Link>
            )}
          </li>
        );
      })}
    </ul>
  );
}

/** Where new capacity matters: the nodes where it is a top host with capability at scale, the unmet demand they hold, and its host compacts. */
export function CapacityBlock({ sum, scenario }: { sum: StrategicSummary; scenario: AibToggle }) {
  return (
    <div className="space-y-1.5">
      {sum.capacity.length > 0 && <EntryList entries={sum.capacity} scenario={scenario} label="Where new capacity matters" />}
      {sum.capacityTotal && <p className="text-xs text-paper-mute">Together {sum.capacityTotal} that it could host.</p>}
      {sum.hostIn.length > 0 && (
        <p className="text-xs text-paper-mute">
          A capacity-building host in{' '}
          {sum.hostIn.map((c, i) => (
            <span key={c.id}>
              {i > 0 && (i === sum.hostIn.length - 1 ? ' and ' : ', ')}
              <CompactLink id={c.id} name={c.name} scenario={scenario} />
            </span>
          ))}
          .
        </p>
      )}
    </div>
  );
}

/** "How the model derives this": the rule, then every assessed role with its status and both gates. */
export function RoleEvidence({ sum, s }: { sum: StrategicSummary; s: AibStrategic }) {
  const held = new Set(heldRoles(sum.cs).map((r) => r.code));
  const rows = [...heldRoles(sum.cs), ...sum.cs.roles.filter((r) => !held.has(r.code))];
  return (
    <div className="space-y-3 text-sm text-paper-dim" data-testid="strategic-role-evidence">
      <p className="leading-relaxed">{roleRuleWords(s)}</p>
      {!sum.allied && <p className="leading-relaxed">A non-allied supplier is assessed on the three supply roles only: resources and processing, specialized technology, and scale manufacturing.</p>}
      <ul className="space-y-2.5" aria-label="Every strategic role assessed, with its evidence">
        {rows.map((r) => (
          <li key={r.code} data-role={r.code} data-status={r.status ?? 'none'}>
            <span className={r.status ? 'font-medium text-paper' : 'text-paper-dim'}>{r.name}</span>
            <span className="text-xs text-paper-mute"> · {roleStatusWords(r.status)}</span>
            <span className="block text-xs text-paper-mute">An economy that {ROLE_MEANING[r.code] ?? 'holds this role'}.</span>
            <ul className="mt-0.5 list-disc space-y-0.5 pl-5 text-xs text-paper-dim">
              {r.gates.map((g) => (
                <li key={g.metric || g.label}>{gateWords(g)}</li>
              ))}
            </ul>
          </li>
        ))}
      </ul>
    </div>
  );
}

/** One labelled block of the summary (a term and its description). */
export function SummaryItem({ term, children, testId }: { term: string; children: ReactNode; testId?: string }) {
  return (
    <div className="min-w-0" data-testid={testId}>
      <dt className="mb-1 text-xs font-medium uppercase tracking-wide text-paper-mute">{term}</dt>
      <dd className="text-sm text-paper-dim">{children}</dd>
    </div>
  );
}

/** A quiet line in place of a slot the export left empty. */
export const Quiet = ({ children }: { children: ReactNode }) => <p className="text-paper-mute">{children}</p>;

/** Words for an empty slot: a non-allied supplier is not read on the demand-side slots. */
export function emptyWords(sum: StrategicSummary, what: 'needs' | 'partners' | 'capacity' | 'contributes'): string {
  if (!sum.allied && what !== 'contributes') return 'Not read for a non-allied supplier.';
  if (what === 'contributes') return 'No anchor or specialist position in the mapped supply chains.';
  if (what === 'needs') return 'No supply-chain node where its net imports reach 5% of allied demand.';
  if (what === 'partners') return `No relationship in the ${sum.partnersScenario} network.`;
  return 'It is not a top host with capability at scale for any node.';
}
