// Shared pieces of the emergent-network and investment-flow views (2026-10-17): the modelled-result note
// (headline run; does not move with the Perspective dials), a role cell of the country x role matrix, the
// trade-against-investment bar of one relationship and a ranked list of investor -> host flows. Used by
// the AIB Country roles and Partnerships pages, a compact page and a Country profile.
// The scenarios (2026-10-19): a cell's words name the scenario ("under Managed Interdependence") and leave
// out the every-shock total where the network has none (`every` null); ScenarioFileState is the loading,
// failed and missing states of a lazily loaded scenario file.
// The strategic layer (2026-10-20): OperationalRoleList, one economy's operational roles as a list, is the
// Country profile's role card and the operational evidence under a strategic summary on /aib/roles.
// Stages (2026-10-28): a side's partner counts are six-stage (AIB_STAGE_KEYS); the words list the non-zero
// ones in chain order (deployment is always 0: the AIB has no Deployment node).

import type { ReactNode } from 'react';
import { Link } from 'react-router';
import { EmptyState, Loading, Notice } from '../../components/ui.tsx';
import { AIB_STAGE_KEYS, type AibBaselineScenarioDoc, type AibInvestmentPair, type AibManagedScenarioDoc, type AibNetworkSide } from '../contract.ts';
import { useAibScenarioDoc, type ScenarioLoad } from '../data/hooks.ts';
import type { AibLoadError } from '../data/store.ts';
import { fmtScore, fmtUsd, plural } from '../derive/format.ts';
import { rankWords } from '../derive/investment.ts';
import { fmtPts, MATRIX_ROLES, ROLE_FLOOR, ROLE_READING, type CountryRoles, type MatrixRole, type RoleCell } from '../derive/network.ts';

export const TRADE_COLOR = '#1889b0';
export const INVEST_COLOR = '#fba36b';

/** "Modelled result ... does not move with the Perspective dials." */
export function ModelledNote({ children, className = '' }: { children?: ReactNode; className?: string }) {
  return (
    <p className={`text-xs leading-relaxed text-paper-mute ${className}`} data-testid="modelled-note">
      A result of the Allied Industrial Base analysis{children ? <> ({children})</> : null}. It does not move with the Perspective dials.
    </p>
  );
}

/** The partners word of a side: suppliers of a buyer, hosts of an investor, ... */
const PARTNER_WORD: Partial<Record<MatrixRole, [string, string]>> = {
  supplier: ['buyer', 'buyers'],
  buyer: ['supplier', 'suppliers'],
  host: ['investor', 'investors'],
  investor: ['host', 'hosts'],
};

/**
 * A side's partners by stage, the non-zero counts in chain order: "(refining 2, components 1)"; '' when
 * every count is 0. A partner can sit in several stages, so the counts need not sum to the partners.
 */
export function stageCountWords(side: AibNetworkSide): string {
  const parts = AIB_STAGE_KEYS.filter((k) => side[k] > 0).map((k) => `${k} ${side[k]}`);
  return parts.length ? `(${parts.join(', ')})` : '';
}

/**
 * One cell's words, for a hover title and the table twin. `every`: what the cumulative figure sums
 * (everyShock(doc).phrase, everyShockLabel(doc).phrase; "every shock" by default; null leaves it out, for a
 * network without one); `under`: the scenario ("under the headline shock" by default, "under Decoupling").
 */
export function cellWords(k: MatrixRole, c: RoleCell, every: string | null = 'every shock', under = 'under the headline disruption'): string {
  const parts: string[] = [];
  if (c.side && c.side.selected === 0 && c.side.cumulative === 0) {
    parts.push('no relationship of this kind in the network');
  } else if (c.side) {
    const pw = PARTNER_WORD[k];
    const pwords = pw ?? ['partner', 'partners'];
    const who = (n: number) => `${n} ${n === 1 ? pwords[0] : pwords[1]}`;
    const by = stageCountWords(c.side);
    const stages = by ? ` ${by}` : '';
    const hp = c.headlinePartners;
    if (hp !== undefined && every !== null) {
      // read on the headline (Decoupling): its own partners; the side's counts sum every shock, and say so
      parts.push(`${fmtPts(c.side.headline)} ${under}${pw ? ` with ${who(hp)}` : ''}`);
      parts.push(`across ${every}: ${fmtPts(c.side.cumulative)}${pw ? `, ${who(c.side.partners)}${stages}` : ''}, ${plural(c.side.selected, 'selected relationship')}`);
    } else {
      parts.push(`${fmtPts(c.side.headline)} ${under}${every === null ? '' : `, ${fmtPts(c.side.cumulative)} across ${every}`}`);
      if (pw) parts.push(`${who(c.side.partners)}${stages}`);
      parts.push(`${plural(c.side.selected, 'selected relationship')}`);
    }
  }
  if (k === 'specialized' && c.nodes.length) parts.push(`largest supplier after coordinated trade at ${plural(c.nodes.length, 'step')}`);
  if (c.compacts.length) parts.push(`the compact role in ${c.compacts.join(', ')}`);
  return parts.join('; ') || 'none';
}

/** A matrix cell: the role's own metric when named, "minor" below the floor, a dot when none (`every`, `under`: cellWords()). */
export function RoleCellView({ k, cell, every, under }: { k: MatrixRole; cell: RoleCell; every?: string | null; under?: string }) {
  if (cell.level === 'none')
    return (
      <span className="text-paper-mute" aria-label="none">
        ·
      </span>
    );
  const words = cellWords(k, cell, every, under);
  const title = `${ROLE_READING[k].short}${cell.level === 'minor' ? ' (minor)' : ''}: ${words}`;
  // named by a compact while the network carries nothing under the headline shock: the compacts lead
  const byCompact = cell.level === 'named' && !!cell.side && cell.side.headline < ROLE_FLOOR && cell.compacts.length > 0;
  // the partners under the scenario: the headline ones where the network's own count sums every shock
  const np = cell.headlinePartners ?? cell.side?.partners ?? 0;
  const partners = cell.side ? `${np} ${(PARTNER_WORD[k] ?? ['partner', 'partners'])[np === 1 ? 0 : 1]}` : '';
  const main = cell.side && !byCompact
    ? fmtPts(cell.side.headline)
    : k === 'specialized' && cell.nodes.length
      ? plural(cell.nodes.length, 'step')
      : plural(cell.compacts.length, 'compact');
  const sub = byCompact
    ? `network: ${fmtPts(cell.side?.headline)}`
    : cell.side
      ? `${partners}${cell.compacts.length ? ` · ${plural(cell.compacts.length, 'compact')}` : ''}`
      : k === 'specialized' && cell.nodes.length && cell.compacts.length
        ? plural(cell.compacts.length, 'compact')
        : '';
  return (
    <span title={title} className={`inline-flex flex-col items-center leading-tight ${cell.level === 'minor' ? 'text-paper-mute' : 'text-paper'}`}>
      <span className="tabular">{cell.level === 'minor' ? `minor · ${main}` : main}</span>
      {sub && <span className="text-[0.6875rem] text-paper-mute">{sub}</span>}
      <span className="sr-only"> ({words})</span>
    </span>
  );
}

/** Trade against investment for one relationship: two bar segments on a shared scale (max = the scale's 100%). */
export function TradeInvestBar({ trade, investment, max, label }: { trade: number; investment: number; max: number; label: string }) {
  const w = (x: number) => `${max > 0 ? Math.max(0, Math.min(100, (x / max) * 100)) : 0}%`;
  return (
    <span className="flex h-2 w-full max-w-[12rem] overflow-hidden rounded-sm bg-white/5" role="img" aria-label={`${label}: trade ${fmtPts(trade)}, investment ${fmtPts(investment)}`}>
      <span style={{ width: w(trade), background: TRADE_COLOR }} />
      <span style={{ width: w(investment), background: INVEST_COLOR }} />
    </span>
  );
}

/** A ranked list of investor -> host flows (one row per node), largest first. */
export function FlowList({
  pairs, name, nodeLabel, maxRank, n = 8, countryLinks = true,
}: {
  pairs: readonly AibInvestmentPair[];
  name: (iso: string) => string;
  nodeLabel: (id: string) => string;
  maxRank?: number;
  n?: number;
  countryLinks?: boolean;
}) {
  const max = pairs.reduce((m, p) => Math.max(m, p.usd), 0);
  const who = (iso: string) =>
    countryLinks ? (
      <Link className="link" to={`/countries/${iso}#partnerships`}>
        {name(iso)}
      </Link>
    ) : (
      <span className="text-paper">{name(iso)}</span>
    );
  return (
    <ol className="divide-y divide-white/5 text-sm" aria-label="Investor to host flows, largest first" data-testid="flow-list">
      {pairs.slice(0, n).map((p) => (
        <li key={`${p.investor}|${p.host}|${p.node}|${p.rank}`} className="grid gap-1 py-2 sm:grid-cols-[minmax(0,1fr)_12rem] sm:items-center sm:gap-3">
          <span className="min-w-0">
            {who(p.investor)} <span className="text-paper-mute">&rarr;</span> {who(p.host)}
            <span className="block text-xs text-paper-mute">
              {nodeLabel(p.node)} · {rankWords(p.rank, maxRank)}
              {p.friendshore !== null ? ` · Friendshore ${fmtScore(p.friendshore)}` : ''}
              {p.opportunity !== null ? ` · Partnership Opportunity ${fmtScore(p.opportunity)}` : ''}
            </span>
          </span>
          <span className="flex items-center gap-2">
            <span className="h-2 flex-1 overflow-hidden rounded-sm bg-white/5">
              <span className="block h-full" style={{ width: `${max > 0 ? (p.usd / max) * 100 : 0}%`, background: INVEST_COLOR }} />
            </span>
            <span className="w-16 shrink-0 text-right tabular text-paper">{fmtUsd(p.usd)}</span>
          </span>
        </li>
      ))}
    </ol>
  );
}

/** The line under a role in a list: its metric and partner count, or the compacts. */
export function roleLine(r: CountryRoles, k: MatrixRole, every?: string | null, under?: string): string {
  return cellWords(k, r.cells[k], every, under);
}

/** How a cell's level reads in a list. */
export const LEVEL_WORD = { named: 'named', minor: 'minor', none: 'not held' } as const;

/**
 * One economy's operational network roles as a list (the Country profile's role card; the evidence under
 * a strategic summary on /aib/roles): each matrix role, its level, and the cell's words (`every`, `under`:
 * cellWords()).
 */
export function OperationalRoleList({ roles, every, under, className = '' }: { roles: CountryRoles; every?: string | null; under?: string; className?: string }) {
  return (
    <ul className={`space-y-2 text-sm text-paper-dim ${className}`} aria-label={`Operational network roles of ${roles.name}`}>
      {MATRIX_ROLES.map((k) => {
        const c = roles.cells[k];
        return (
          <li key={k} data-level={c.level}>
            <span className={c.level === 'named' ? 'font-medium text-paper' : 'text-paper-mute'}>{ROLE_READING[k].short}</span>
            <span className="text-xs text-paper-mute"> · {LEVEL_WORD[c.level]}</span>
            {c.level !== 'none' && <span className="block text-xs text-paper-mute">{cellWords(k, c, every, under)}</span>}
          </li>
        );
      })}
    </ul>
  );
}

/** What a scenario file's load says (useAibScenarioDoc()). */
export interface FileLoad {
  loading: boolean;
  error: AibLoadError | null;
  retry: () => void;
  missing: boolean;
}

/**
 * The state of a lazily loaded scenario file in place of the content that needs it: loading, failed
 * (with Retry) or missing (an older bundle). `what`: the content ("the country roles"); `label`: the
 * scenario ("Managed Interdependence").
 */
export function ScenarioFileState({ load, what, label }: { load: FileLoad; what: string; label: string }) {
  if (load.error) {
    return (
      <Notice
        kind="error"
        title={`The ${label} results could not be loaded`}
        action={
          <button type="button" className="btn btn-ghost btn-sm" onClick={load.retry}>
            Retry
          </button>
        }
      >
        {load.error.message}
      </Notice>
    );
  }
  if (load.missing) return <EmptyState>This build has no {label} results, so {what} cannot be shown.</EmptyState>;
  return <Loading label={`Loading ${what} under ${label}`} />;
}


/** The Managed Interdependence file when `on` (idle otherwise): useAibScenarioDoc() typed for a conditional id. */
export function useManagedDoc(on: boolean): ScenarioLoad<AibManagedScenarioDoc> {
  const l = useAibScenarioDoc(on ? 'managed' : null);
  return { ...l, doc: l.doc && l.doc.scenario === 'managed' ? l.doc : null };
}

/** The Baseline file (today's network and suppliers) when `on` (idle otherwise). */
export function useBaselineDoc(on: boolean): ScenarioLoad<AibBaselineScenarioDoc> {
  const l = useAibScenarioDoc(on ? 'baseline' : null);
  return { ...l, doc: l.doc && l.doc.scenario === 'baseline' ? l.doc : null };
}
