// The strategic summary of the AIB Country roles page (2026-10-20; task spec s.17): "Strategic roles" above
// the operational table. An economy selector (allied economies, then the major non-allied suppliers), the
// chosen economy's summary card (StrategicSummaryCard) and who holds each strategic role. The choice is in
// the URL (`economy`, written on a change with the history entry replaced; the toggle and every other key
// kept); without one the page opens on the economy of a #role-ISO3 link (a Country profile's "See all
// country roles"), else on the first allied economy by name that holds a primary role. The strategic roles
// are structural, the same in every scenario: the toggle changes only the operational evidence the page
// passes in (`evidenceFor`).

import { useEffect, useMemo, useRef, type ReactNode } from 'react';
import { useLocation, useNavigate } from 'react-router';
import { EmptyState, FilterRow, Select } from '../../../components/ui.tsx';
import type { AibNode, AibStrategic } from '../../contract.ts';
import { plural } from '../../derive/format.ts';
import type { AibToggle } from '../../scenarioUrl.ts';
import { defaultEconomy, economyOptions, strategicSummaryOf } from './derive.ts';
import type { NameRenderer } from './parts.tsx';
import { RoleHolders, StrategicSummaryCard } from './StrategicSummaryCard.tsx';

/** The URL key of the chosen economy. */
export const ECONOMY_KEY = 'economy';

export function StrategicRolesSection({
  s, nodes, names, name, scenario, renderName, evidenceFor,
}: {
  s: AibStrategic;
  nodes: readonly Pick<AibNode, 'id' | 'label'>[];
  /** the document's economy names (the narratives name partners; joined back to codes) */
  names: Readonly<Record<string, string>>;
  name: (iso: string) => string;
  scenario: AibToggle;
  renderName: NameRenderer;
  /** the page's operational evidence for an economy (its network roles under the scenario) */
  evidenceFor?: (iso3: string) => ReactNode;
}) {
  const loc = useLocation();
  const nav = useNavigate();
  const heading = useRef<HTMLHeadingElement>(null);
  const focusNext = useRef(false);
  const known = useMemo(() => new Set(s.countries.map((c) => c.iso3)), [s]);
  const fromParam = new URLSearchParams(loc.search).get(ECONOMY_KEY)?.toUpperCase() ?? null;
  const fromHash = loc.hash.startsWith('#role-') ? loc.hash.slice(6).toUpperCase() : null;
  const fallback = useMemo(() => defaultEconomy(s, name), [s, name]);
  const iso = fromParam && known.has(fromParam) ? fromParam : fromHash && known.has(fromHash) ? fromHash : fallback;
  const sum = useMemo(() => (iso ? strategicSummaryOf(s, nodes, names, iso) : null), [s, nodes, names, iso]);
  const options = useMemo(() => economyOptions(s, name), [s, name]);
  const nAllied = s.countries.filter((c) => c.allied).length;
  const nHolding = s.countries.filter((c) => c.allied && (c.primary.length > 0 || c.secondary.length > 0)).length;

  const choose = (next: string, focus: boolean) => {
    if (next === iso) {
      if (focus) heading.current?.focus();
      return;
    }
    const p = new URLSearchParams(loc.search);
    p.set(ECONOMY_KEY, next);
    focusNext.current = focus;
    nav({ pathname: loc.pathname, search: `?${p.toString()}`, hash: '' }, { replace: true, preventScrollReset: true });
  };

  // a role holder chosen below: bring its summary into view and move focus to its heading
  useEffect(() => {
    if (!focusNext.current) return;
    focusNext.current = false;
    heading.current?.scrollIntoView?.({ block: 'start' });
    heading.current?.focus({ preventScroll: true });
  }, [iso]);

  return (
    <section aria-labelledby="strategic-roles-h" data-testid="strategic-roles">
      <span className="eyebrow block">The long-run division of labor · the same in every scenario</span>
      <h2 id="strategic-roles-h" className="rule-neis-sm mb-2 mt-1 text-xl font-medium text-paper lg:text-2xl">
        Strategic roles
      </h2>
      <p className="mb-4 max-w-3xl text-[0.9375rem] leading-relaxed text-paper-dim">
        The two or three main things a country does in the allied industrial system: anchoring resources and processing, specialized technology, scale manufacturing, demand,
        capital, new capacity, or coordination. Each role is read from the evidence by a stated rule, never from a score, and it is structural: switching the scenario changes the
        stress on the system, not who plays which role. {plural(nHolding, 'allied country', 'allied countries')} of {nAllied} hold at least one role at these bars; the others
        contribute through their roles in particular compacts.
      </p>
      <FilterRow result={sum && !sum.allied ? 'A major non-allied supplier' : undefined}>
        <Select label="Strategic summary of" value={iso ?? ''} options={options} onChange={(x) => choose(x, false)} className="min-w-[14rem]" />
      </FilterRow>
      {sum ? (
        <StrategicSummaryCard sum={sum} s={s} scenario={scenario} name={name} renderName={renderName} headingRef={heading} evidence={iso ? evidenceFor?.(iso) : null} />
      ) : (
        <EmptyState>The strategic roles of this build assess no country.</EmptyState>
      )}
      <RoleHolders s={s} name={name} selected={iso} onSelect={(x) => choose(x, true)} />
    </section>
  );
}
