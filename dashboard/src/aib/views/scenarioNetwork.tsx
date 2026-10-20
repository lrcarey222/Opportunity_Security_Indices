// Today against the toggled scenario, for the network views (2026-10-19). Baseline, trade as it is today
// (the observed 2024 supply), is the reference; the scenario's network carries the supply it adds or
// redirects (trade from other suppliers and new capacity at a host). The two measure different things, so
// they are shown side by side, each on its own scale, never subtracted.
//
//   SuppliersCompare     a compact or a node: its largest suppliers today (share of the allied demand)
//                        beside the largest suppliers of the scenario's added trade (USD), marking the
//                        supplier the scenario removes (Decoupling) or holds to its cap (Managed
//                        Interdependence) and the added suppliers outside today's list.
//   WhoSuppliesWhomCard  the Country roles page: the largest supplier -> buyer pairs today and under the
//                        scenario, filterable by economy; a table twin and CSV of every pair of either.
//   RoleChangesCard      the Country roles page: the roles each economy gains or loses under the scenario
//                        against today (network.ts roleChanges()); a table twin and CSV.
//
// Lists, not charts: each value is printed beside its bar, every bar has an aria-label, and the lists stack
// on a phone.

import { useId, useMemo, useState, type ReactNode } from 'react';
import { Link } from 'react-router';
import { ChartCard } from '../../charts/ChartCard.tsx';
import { TableView, type Col } from '../../components/TableView.tsx';
import { EmptyState, FilterRow, Select } from '../../components/ui.tsx';
import { ScenarioName, ScenarioTag } from '../charts/ScenarioCompare.tsx';
import type { AibMeta, AibNetwork } from '../contract.ts';
import type { AddedSupplier } from '../derive/flows.ts';
import { fmtShare, fmtUsd, plural } from '../derive/format.ts';
import { changeHolders, edgeTotal, fmtPts, supplyPairs, type ChangeRole, type PairCompare, type RoleChange, type RoleComparison } from '../derive/network.ts';
import { runMetaOf, scenarioCsvFields, scenarioInfo, type SupplierToday } from '../derive/scenarios.ts';
import type { AibToggle } from '../scenarioUrl.ts';
import { csvWithRun } from './common.tsx';
import { INVEST_COLOR, TRADE_COLOR } from './networkParts.tsx';

type Heading = 'h3' | 'h4';

/** A bar of a list row: Baseline hollow (the reference), a scenario filled in its colour. */
function ListBar({ value, max, id, label, meta }: { value: number; max: number; id: 'baseline' | AibToggle; label: string; meta?: AibMeta | null }) {
  const s = scenarioInfo(meta, id);
  const w = `${max > 0 ? Math.max(0, Math.min(1, value / max)) * 100 : 0}%`;
  const style = s.mark === 'fill' ? { background: s.color } : { border: `2px solid ${s.color}`, boxSizing: 'border-box' as const };
  return (
    <span className="block h-2 w-full bg-white/5" role="img" aria-label={label}>
      <span className="block h-full" style={{ width: w, ...style }} />
    </span>
  );
}

/** The run columns of a scenario's CSV (its own shock in place of the headline; the scenario named). */
function scenarioCsv(rows: Record<string, unknown>[], meta: AibMeta, id: AibToggle): string {
  const scn = scenarioCsvFields(meta, id);
  const runMeta = runMetaOf(meta, id);
  return csvWithRun(
    rows.map((r) => ({ ...r, ...scn })),
    runMeta,
  );
}

// ---- A compact or a node: suppliers today and under the scenario -------------------------------------------

export interface SuppliersCompareProps {
  /** today's largest suppliers (nodeSuppliersToday / compactSuppliersToday) */
  today: readonly SupplierToday[];
  /** the scenario's added trade by exporter (addedSuppliers) */
  added: readonly AddedSupplier[];
  scenario: AibToggle;
  meta: AibMeta;
  name: (iso3: string) => string;
  /** "the compact's nodes", a node's label */
  scope: string;
  /** the economy whose supply the scenario removes (Decoupling) */
  removed?: string | null;
  /** economy -> the nodes where Managed Interdependence holds it to the cap */
  capped?: ReadonlyMap<string, readonly string[]>;
  /** today's list still comes from the main document (the Baseline file is loading) */
  todayPartial?: boolean;
  heading?: Heading;
  n?: number;
}

/** Today's largest suppliers beside the largest suppliers of the scenario's added trade. */
export function SuppliersCompare({ today, added, scenario, meta, name, scope, removed, capped, todayPartial = false, heading = 'h4', n = 6 }: SuppliersCompareProps) {
  const H = heading;
  const label = scenarioInfo(meta, scenario).label;
  const t = today.slice(0, n);
  const a = added.slice(0, n);
  const tMax = Math.max(0, ...t.map((x) => x.share ?? 0));
  const aMax = Math.max(0, ...a.map((x) => x.usd));
  const todaySet = new Set(today.map((x) => x.iso3));
  const flag = (iso: string): string | null => {
    if (scenario === 'decoupling' && removed === iso) return `its supply is removed under ${label}`;
    const c = scenario === 'managed' ? capped?.get(iso) : undefined;
    if (c && c.length) return `held to its cap under ${label} in ${plural(c.length, 'node')}`;
    return null;
  };
  return (
    <div className="grid gap-5 border-t hairline pt-4 sm:grid-cols-2" data-testid="suppliers-compare">
      <section aria-label={`Largest suppliers today, ${scope}`}>
        <H className="mb-1 text-sm font-medium text-paper">
          <ScenarioName id="baseline" meta={meta} /> <span className="font-normal text-paper-dim">· who supplies {scope} today</span>
        </H>
        {t.length ? (
          <ol className="space-y-1.5 text-sm">
            {t.map((x) => {
              const f = flag(x.iso3);
              const words = `${name(x.iso3)}: ${fmtShare(x.share, 0)} of the demand${x.usd !== null ? `, ${fmtUsd(x.usd)}` : ''}`;
              return (
                <li key={x.iso3} className="grid grid-cols-[minmax(0,1fr)_auto] items-center gap-x-3 gap-y-0.5">
                  <span className="min-w-0 truncate text-paper">{name(x.iso3)}</span>
                  <span className="tabular text-xs text-paper-dim">
                    {fmtShare(x.share, 0)}
                    {x.usd !== null ? ` · ${fmtUsd(x.usd)}` : ''}
                  </span>
                  <span className="col-span-2">
                    <ListBar value={x.share ?? 0} max={tMax} id="baseline" label={words} meta={meta} />
                  </span>
                  {f && <span className="col-span-2 text-xs text-paper-mute">{f}</span>}
                </li>
              );
            })}
          </ol>
        ) : (
          <p className="text-sm text-paper-mute">This data build has no list of today&apos;s suppliers here.</p>
        )}
        <p className="mt-2 text-xs text-paper-mute">
          Observed {meta.tradeYear} supply to the allied market, as a share of its demand
          {todayPartial ? '; the five largest while the full list loads' : ''}.
        </p>
      </section>
      <section aria-label={`Largest added suppliers under ${label}, ${scope}`}>
        <H className="mb-1 text-sm font-medium text-paper">
          <ScenarioName id={scenario} meta={meta} /> <span className="font-normal text-paper-dim">· who adds supply</span>
        </H>
        {a.length ? (
          <ol className="space-y-1.5 text-sm">
            {a.map((x) => {
              const words = `${name(x.iso3)}: ${fmtUsd(x.usd)} of added trade${x.newUsd > 0 ? `, ${fmtUsd(x.newUsd)} of it in new relationships` : ''}`;
              return (
                <li key={x.iso3} className="grid grid-cols-[minmax(0,1fr)_auto] items-center gap-x-3 gap-y-0.5">
                  <span className="min-w-0 truncate text-paper">{name(x.iso3)}</span>
                  <span className="tabular text-xs text-paper-dim">{fmtUsd(x.usd)}</span>
                  <span className="col-span-2">
                    <ListBar value={x.usd} max={aMax} id={scenario} label={words} meta={meta} />
                  </span>
                  {(x.newUsd > 0 || !todaySet.has(x.iso3)) && (
                    <span className="col-span-2 text-xs text-paper-mute">
                      {[x.newUsd > 0 ? `${fmtUsd(x.newUsd)} in new relationships` : null, !todaySet.has(x.iso3) && today.length ? `not among today's ${plural(today.length, 'largest supplier')}` : null]
                        .filter(Boolean)
                        .join(' · ')}
                    </span>
                  )}
                </li>
              );
            })}
          </ol>
        ) : (
          <p className="text-sm text-paper-mute">No added trade here under {label}.</p>
        )}
        <p className="mt-2 text-xs text-paper-mute">The trade the solution adds or redirects to replace the supply lost or shifted (USD a year, {meta.tradeYear} values); its own scale.</p>
      </section>
    </div>
  );
}

// ---- The Country roles page: who supplies whom ---------------------------------------------------------------

const TOP_PAIRS = 10;

function PairList({ pairs, side, meta, scenario, name }: { pairs: readonly PairCompare[]; side: 'today' | 'scenario'; meta: AibMeta; scenario: AibToggle; name: (iso: string) => string }) {
  const top = pairs.slice(0, TOP_PAIRS);
  const val = (p: PairCompare) => (side === 'today' ? (p.today?.tradeUsd ?? 0) : p.scenario ? edgeTotal(p.scenario) : 0);
  const max = Math.max(0, ...top.map(val));
  const label = scenarioInfo(meta, scenario).label;
  if (!top.length) return <p className="text-sm text-paper-mute">{side === 'today' ? 'No supply relationship today here.' : `No relationship ${label} adds here.`}</p>;
  return (
    <ol className="space-y-2 text-sm" aria-label={side === 'today' ? 'Largest supply relationships today' : `Largest relationships under ${label}`}>
      {top.map((p) => {
        const pair = `${name(p.from)} to ${name(p.to)}`;
        if (side === 'today') {
          const e = p.today!;
          return (
            <li key={`${p.from}|${p.to}`} className="grid grid-cols-[minmax(0,1fr)_auto] items-center gap-x-3 gap-y-0.5">
              <span className="min-w-0 text-paper">
                {name(p.from)} <span className="text-paper-mute">&rarr;</span> {name(p.to)}
              </span>
              <span className="tabular text-xs text-paper-dim">{fmtUsd(e.tradeUsd)}</span>
              <span className="col-span-2">
                <ListBar value={e.tradeUsd} max={max} id="baseline" label={`${pair}: ${fmtUsd(e.tradeUsd)} a year today, ${fmtPts(e.trade)} of node demand`} meta={meta} />
              </span>
              <span className="col-span-2 text-xs text-paper-mute">
                {fmtPts(e.trade)} of node demand · {plural(e.nTrade, 'node')}
                {p.scenario ? ` · ${label} adds ${fmtPts(edgeTotal(p.scenario))}` : ''}
              </span>
            </li>
          );
        }
        const e = p.scenario!;
        const tot = edgeTotal(e);
        const w = (x: number) => `${max > 0 ? Math.max(0, Math.min(1, x / max)) * 100 : 0}%`;
        return (
          <li key={`${p.from}|${p.to}`} className="grid grid-cols-[minmax(0,1fr)_auto] items-center gap-x-3 gap-y-0.5">
            <span className="min-w-0 text-paper">
              {name(p.from)} <span className="text-paper-mute">&rarr;</span> {name(p.to)}
            </span>
            <span className="tabular text-xs text-paper-dim">{fmtPts(tot)}</span>
            <span className="col-span-2">
              <span className="flex h-2 w-full bg-white/5" role="img" aria-label={`${pair}: trade ${fmtPts(e.trade)}, new capacity ${fmtPts(e.investment)} of node demand under ${label}`}>
                <span style={{ width: w(e.trade), background: TRADE_COLOR }} />
                <span style={{ width: w(e.investment), background: INVEST_COLOR }} />
              </span>
            </span>
            <span className="col-span-2 text-xs text-paper-mute">
              {e.trade > 0 ? `trade ${fmtPts(e.trade)}${e.tradeUsd > 0 ? ` (${fmtUsd(e.tradeUsd)})` : ''}` : ''}
              {e.trade > 0 && e.investment > 0 ? ' · ' : ''}
              {e.investment > 0 ? `new capacity ${fmtPts(e.investment)}` : ''}
              {p.today ? ' · a tie today' : ' · not a tie today'}
            </span>
          </li>
        );
      })}
    </ol>
  );
}

export interface WhoSuppliesWhomCardProps {
  /** today's network (the Baseline scenario file) */
  today: AibNetwork;
  /** the scenario's network (Decoupling: its headline network) */
  network: AibNetwork;
  scenario: AibToggle;
  meta: AibMeta;
  name: (iso3: string) => string;
  headingLevel?: 2 | 3;
}

/** The largest supplier -> buyer pairs today and under the scenario, side by side, for every economy or one. */
export function WhoSuppliesWhomCard({ today, network, scenario, meta, name, headingLevel = 3 }: WhoSuppliesWhomCardProps) {
  const [iso, setIso] = useState('');
  const uid = useId();
  const label = scenarioInfo(meta, scenario).label;
  const p = useMemo(() => supplyPairs(today, network, iso || null), [today, network, iso]);
  const economies = useMemo(() => {
    const s = new Set<string>();
    for (const e of [...today.edges, ...network.edges]) {
      s.add(e.from);
      s.add(e.to);
    }
    return [...s].map((x) => ({ value: x, label: name(x) })).sort((a, b) => a.label.localeCompare(b.label, 'en'));
  }, [today, network, name]);
  const scope = iso ? name(iso) : 'every economy';
  const newTies = p.scenario.filter((x) => !x.today).length;
  const cols: Col<PairCompare>[] = [
    { id: 'from', header: 'Supplier', value: (r) => name(r.from) },
    { id: 'to', header: 'Buyer', value: (r) => name(r.to) },
    { id: 't-usd', header: 'Today: supply (USD a year)', value: (r) => r.today?.tradeUsd ?? null, cell: (r) => (r.today ? fmtUsd(r.today.tradeUsd) : 'not a tie today'), num: true },
    { id: 't-pts', header: 'Today: share of node demand (pts)', value: (r) => r.today?.trade ?? null, cell: (r) => fmtPts(r.today?.trade), num: true },
    { id: 's-trade', header: `${label}: added trade (pts)`, value: (r) => r.scenario?.trade ?? null, cell: (r) => fmtPts(r.scenario?.trade), num: true },
    { id: 's-usd', header: `${label}: added trade (USD a year)`, value: (r) => r.scenario?.tradeUsd ?? null, cell: (r) => (r.scenario ? fmtUsd(r.scenario.tradeUsd) : '–'), num: true },
    { id: 's-inv', header: `${label}: new capacity (pts)`, value: (r) => r.scenario?.investment ?? null, cell: (r) => fmtPts(r.scenario?.investment), num: true },
  ];
  const csv = () => ({
    name: `Allied Industrial Base who supplies whom today and under ${label}${iso ? ` ${scope}` : ''}`,
    text: scenarioCsv(
      p.all.map((r) => ({
        supplier_iso3: r.from, supplier: name(r.from), buyer_iso3: r.to, buyer: name(r.to),
        today_supply_usd: r.today?.tradeUsd ?? null, today_share_of_node_demand: r.today?.trade ?? null, today_nodes: r.today?.nTrade ?? null,
        scenario_added_trade_share: r.scenario?.trade ?? null, scenario_added_trade_usd: r.scenario?.tradeUsd ?? null, scenario_new_capacity_share: r.scenario?.investment ?? null,
      })),
      meta,
      scenario,
    ),
  });
  return (
    <ChartCard
      headingLevel={headingLevel}
      title={`Who supplies whom: today and under ${label}`}
      badge={<ScenarioTag id={scenario} meta={meta} />}
      sub={`Supplier to buyer, ${scope}. Today: the observed supply (${meta.tradeYear}, USD a year), largest first. Under ${label}: the trade and new capacity the solution adds or redirects, largest first. Points are shares of a node's demand summed over the nodes (1 point is 1% of one node's demand); each list has its own scale.`}
      summary={`Largest supply relationships today, ${scope}: ${p.today.slice(0, 3).map((x) => `${name(x.from)} to ${name(x.to)}`).join('; ') || 'none'}. Under ${label}: ${p.scenario.slice(0, 3).map((x) => `${name(x.from)} to ${name(x.to)}`).join('; ') || 'none'}.`}
      table={() => <TableView rows={p.all} cols={cols} caption={`Who supplies whom today and under ${label}, ${scope}`} search pageSize={20} initialSort={{ id: 't-usd', desc: true }} rowKey={(r) => `${r.from}|${r.to}`} />}
      csv={csv}
      notes={
        <span>
          Today&apos;s network keeps the supplier to buyer pairs that carry at least half a point of node demand. &ldquo;Not a tie today&rdquo;: the pair is not among them. Trade
          <span aria-hidden="true" className="mx-1 inline-block h-2 w-3 align-middle" style={{ background: TRADE_COLOR }} />
          and new capacity
          <span aria-hidden="true" className="mx-1 inline-block h-2 w-3 align-middle" style={{ background: INVEST_COLOR }} />
          share the scenario&apos;s bars; new capacity runs from the host to the economy it serves.
        </span>
      }
    >
      <FilterRow result={`${plural(p.today.length, 'tie')} today; ${plural(p.scenario.length, 'relationship')} under ${label}, ${newTies} of them not a tie today`}>
        <Select label="Economy" value={iso} options={[{ value: '', label: 'Every economy' }, ...economies]} onChange={setIso} className="min-w-[14rem]" />
      </FilterRow>
      <div className="grid gap-6 sm:grid-cols-2">
        <section aria-labelledby={`${uid}-today`}>
          <h4 id={`${uid}-today`} className="mb-2 text-sm font-medium text-paper">
            <ScenarioName id="baseline" meta={meta} /> <span className="font-normal text-paper-dim">· the largest supply today</span>
          </h4>
          <PairList pairs={p.today} side="today" meta={meta} scenario={scenario} name={name} />
        </section>
        <section aria-labelledby={`${uid}-scn`}>
          <h4 id={`${uid}-scn`} className="mb-2 text-sm font-medium text-paper">
            <ScenarioName id={scenario} meta={meta} /> <span className="font-normal text-paper-dim">· the largest added supply</span>
          </h4>
          <PairList pairs={p.scenario} side="scenario" meta={meta} scenario={scenario} name={name} />
        </section>
      </div>
    </ChartCard>
  );
}

// ---- The Country roles page: roles gained or lost -------------------------------------------------------------

/** The words of each role gained or lost, and the line under an economy. */
const GAIN_GROUPS: { k: ChangeRole; title: string; line: (r: RoleChange) => string }[] = [
  { k: 'supplier', title: 'New suppliers', line: (r) => `adds ${fmtPts(r.scenario.supplier)}; ${r.today.supplier !== null ? `${fmtPts(r.today.supplier)} today` : 'not a supplier in today’s network'}` },
  { k: 'specialized', title: 'Become the largest supplier of a node', line: (r) => `${plural(r.scenario.largestOf.length, 'node')}; none today` },
  { k: 'host', title: 'Host new capacity', line: (r) => `serves ${fmtPts(r.scenario.host)} of node demand` },
  { k: 'investor', title: 'Invest in new capacity', line: (r) => `new capacity serves ${fmtPts(r.scenario.investor)} of its demand` },
];
const LOSS_GROUPS: { k: ChangeRole; title: string; line: (r: RoleChange) => string }[] = [
  { k: 'supplier', title: 'Stop supplying the allied market', line: (r) => `${fmtPts(r.today.supplier)} of node demand today` },
  { k: 'specialized', title: 'No longer the largest supplier of any node', line: (r) => `the largest of ${plural(r.today.largestOf.length, 'node')} today` },
];

const SHOW = 6;

function ChangeGroup({ title, rows, line, href }: { title: string; rows: readonly RoleChange[]; line: (r: RoleChange) => string; href: (to: string) => string }) {
  if (!rows.length) return null;
  return (
    <div>
      <h4 className="text-sm font-medium text-paper">
        {title} <span className="font-normal text-paper-mute">({rows.length})</span>
      </h4>
      <ul className="mt-1 space-y-0.5 text-sm">
        {rows.slice(0, SHOW).map((r) => (
          <li key={r.iso3}>
            <Link className="link" to={href(`/countries/${r.iso3}#aib-role`)}>
              {r.name}
            </Link>{' '}
            <span className="text-xs text-paper-mute">{line(r)}</span>
          </li>
        ))}
      </ul>
      {rows.length > SHOW && <p className="mt-0.5 text-xs text-paper-mute">and {rows.length - SHOW} more in the table</p>}
    </div>
  );
}

const roleWords = (r: RoleChange, side: 'gained' | 'lost'): string =>
  r[side]
    .map((k) => (side === 'gained' ? GAIN_GROUPS : LOSS_GROUPS).find((g) => g.k === k)?.title.toLowerCase() ?? k)
    .join('; ');

export interface RoleChangesCardProps {
  cmp: RoleComparison;
  scenario: AibToggle;
  meta: AibMeta;
  /** an href with the toggle carried */
  href: (to: string) => string;
  headingLevel?: 2 | 3;
}

/** The roles each economy gains or loses under the scenario against today, grouped by role. */
export function RoleChangesCard({ cmp, scenario, meta, href, headingLevel = 3 }: RoleChangesCardProps) {
  const label = scenarioInfo(meta, scenario).label;
  const gained = cmp.rows.filter((r) => r.gained.length > 0);
  const lost = cmp.rows.filter((r) => r.lost.length > 0);
  const capped = cmp.rows.filter((r) => r.capped.length > 0).sort((a, b) => b.capped.length - a.capped.length || a.name.localeCompare(b.name, 'en'));
  const cols: Col<RoleChange>[] = [
    { id: 'name', header: 'Economy', value: (r) => r.name, cell: (r) => <Link className="link" to={href(`/countries/${r.iso3}#aib-role`)}>{r.name}</Link>, minWidth: 140 },
    { id: 'gained', header: `Gained under ${label}`, value: (r) => roleWords(r, 'gained') || null, minWidth: 180 },
    { id: 'lost', header: `Lost under ${label}`, value: (r) => roleWords(r, 'lost') || null, minWidth: 160 },
    { id: 't-sup', header: 'Supplier today (pts)', value: (r) => r.today.supplier, cell: (r) => fmtPts(r.today.supplier), num: true },
    { id: 's-sup', header: `Added supply under ${label} (pts)`, value: (r) => r.scenario.supplier, cell: (r) => fmtPts(r.scenario.supplier), num: true },
    { id: 't-lg', header: 'Largest supplier today (nodes)', value: (r) => r.today.largestOf.length, num: true },
    { id: 's-lg', header: `Largest supplier under ${label} (nodes)`, value: (r) => r.scenario.largestOf.length, num: true },
    { id: 'host', header: 'Hosts new capacity (pts)', value: (r) => r.scenario.host, cell: (r) => fmtPts(r.scenario.host), num: true },
    { id: 'inv', header: 'New capacity serving it (pts)', value: (r) => r.scenario.investor, cell: (r) => fmtPts(r.scenario.investor), num: true },
    ...(scenario === 'managed' ? [{ id: 'cap', header: 'Held to its cap (nodes)', value: (r: RoleChange) => r.capped.length, num: true } as Col<RoleChange>] : []),
  ];
  const csv = () => ({
    name: `Allied Industrial Base roles gained or lost under ${label}`,
    text: scenarioCsv(
      cmp.rows.map((r) => ({
        iso3: r.iso3, economy: r.name, gained: r.gained.join('; '), lost: r.lost.join('; '), supply_removed: r.removed,
        today_supplier_contribution: r.today.supplier, today_buyer_contribution: r.today.buyer, today_largest_supplier_nodes: r.today.largestOf.join('; '),
        scenario_added_supply_contribution: r.scenario.supplier, scenario_buyer_contribution: r.scenario.buyer, scenario_host_contribution: r.scenario.host,
        scenario_investor_contribution: r.scenario.investor, scenario_largest_supplier_nodes: r.scenario.largestOf.join('; '), capped_nodes: r.capped.join('; '),
      })),
      meta,
      scenario,
    ),
  });
  const group = (side: 'gained' | 'lost', g: { k: ChangeRole; title: string; line: (r: RoleChange) => string }): ReactNode => (
    <ChangeGroup key={`${side}-${g.k}`} title={g.title} rows={changeHolders(cmp.rows, g.k, side)} line={g.line} href={href} />
  );
  return (
    <ChartCard
      headingLevel={headingLevel}
      title="Roles gained or lost"
      badge={<ScenarioTag id={scenario} meta={meta} />}
      sub={`Against today, ${plural(gained.length, 'economy', 'economies')} gain a role under ${label} and ${plural(lost.length, 'economy', 'economies')} lose one${scenario === 'managed' ? `; ${plural(capped.length, 'supplier')} keep their role but supply less, held to the cap` : ''}. A supplier or a host is named at 1 point of node demand or more; the largest supplier is compared over ${plural(cmp.nodes, 'node')}.`}
      summary={`Roles gained under ${label}: ${gained.slice(0, 5).map((r) => `${r.name} (${roleWords(r, 'gained')})`).join('; ') || 'none'}. Lost: ${lost.map((r) => `${r.name} (${roleWords(r, 'lost')})`).join('; ') || 'none'}.`}
      table={() => <TableView rows={cmp.rows} cols={cols} caption={`Roles gained or lost under ${label}, against today`} search pageSize={20} rowKey={(r) => r.iso3} />}
      csv={csv}
      notes={
        <span>
          Hosting and investing in new capacity are always gains: no new capacity exists today. {scenario === 'managed' ? `${label} removes no supplier: a capped supplier keeps supplying, up to the share that brings the node to its benchmark.` : `Under ${label} one economy's supply to the allied market is removed; every other supplier keeps what it supplies today.`}
        </span>
      }
    >
      {cmp.rows.length ? (
        <div className="grid gap-6 sm:grid-cols-2">
          <section aria-label={`Roles gained under ${label}`} className="space-y-3">
            <p className="eyebrow">Gained</p>
            {gained.length ? GAIN_GROUPS.map((g) => group('gained', g)) : <p className="text-sm text-paper-mute">No economy gains a role.</p>}
          </section>
          <section aria-label={`Roles lost or reduced under ${label}`} className="space-y-3">
            <p className="eyebrow">{scenario === 'managed' ? 'Lost or reduced' : 'Lost'}</p>
            {LOSS_GROUPS.map((g) => group('lost', g))}
            {scenario === 'managed' && (
              <ChangeGroup title="Supply less: held to the cap" rows={capped} line={(r) => `in ${plural(r.capped.length, 'node')}`} href={href} />
            )}
            {!lost.length && !(scenario === 'managed' && capped.length) && <p className="text-sm text-paper-mute">No economy loses a role.</p>}
          </section>
        </div>
      ) : (
        <EmptyState>No economy gains or loses a role under {label} against today.</EmptyState>
      )}
    </ChartCard>
  );
}
