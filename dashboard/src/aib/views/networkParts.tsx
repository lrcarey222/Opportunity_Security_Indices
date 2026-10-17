// Shared pieces of the emergent-network and investment-flow views (2026-10-17): the modelled-result note
// (headline run; does not move with the Perspective dials), a role cell of the country x role matrix, the
// trade-against-investment bar of one relationship and a ranked list of investor -> host flows. Used by
// the AIB Country roles and Partnerships pages, a compact page and a Country profile.

import type { ReactNode } from 'react';
import { Link } from 'react-router';
import type { AibInvestmentPair } from '../contract.ts';
import { fmtScore, fmtUsd, plural } from '../derive/format.ts';
import { rankWords } from '../derive/investment.ts';
import { fmtPts, ROLE_FLOOR, ROLE_READING, type CountryRoles, type MatrixRole, type RoleCell } from '../derive/network.ts';

export const TRADE_COLOR = '#1889b0';
export const INVEST_COLOR = '#fba36b';

/** "Modelled result ... does not move with the Perspective dials." */
export function ModelledNote({ children, className = '' }: { children?: ReactNode; className?: string }) {
  return (
    <p className={`text-xs leading-relaxed text-paper-mute ${className}`} data-testid="modelled-note">
      Modelled result of the Allied Industrial Base run{children ? <> ({children})</> : null}. It does not move with the Perspective dials.
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

/** One cell's words, for a hover title and the table twin. */
export function cellWords(k: MatrixRole, c: RoleCell): string {
  const parts: string[] = [];
  if (c.side && c.side.selected === 0 && c.side.cumulative === 0) {
    parts.push('no relationship of this kind in the network');
  } else if (c.side) {
    const pw = PARTNER_WORD[k];
    parts.push(`${fmtPts(c.side.headline)} under the headline shock, ${fmtPts(c.side.cumulative)} across every shock`);
    if (pw) parts.push(`${c.side.partners} ${c.side.partners === 1 ? pw[0] : pw[1]} (upstream ${c.side.upstream}, midstream ${c.side.midstream}, downstream ${c.side.downstream})`);
    parts.push(`${plural(c.side.selected, 'selected relationship')}`);
  }
  if (k === 'specialized' && c.nodes.length) parts.push(`largest optimized supplier of ${plural(c.nodes.length, 'node')}`);
  if (c.compacts.length) parts.push(`the compact role in ${c.compacts.join(', ')}`);
  return parts.join('; ') || 'none';
}

/** A matrix cell: the role's own metric when named, "minor" below the floor, a dot when none. */
export function RoleCellView({ k, cell }: { k: MatrixRole; cell: RoleCell }) {
  if (cell.level === 'none')
    return (
      <span className="text-paper-mute" aria-label="none">
        ·
      </span>
    );
  const title = `${ROLE_READING[k].short}${cell.level === 'minor' ? ' (minor)' : ''}: ${cellWords(k, cell)}`;
  // named by a compact while the network carries nothing under the headline shock: the compacts lead
  const byCompact = cell.level === 'named' && !!cell.side && cell.side.headline < ROLE_FLOOR && cell.compacts.length > 0;
  const partners = cell.side ? `${cell.side.partners} ${(PARTNER_WORD[k] ?? ['partner', 'partners'])[cell.side.partners === 1 ? 0 : 1]}` : '';
  const main = cell.side && !byCompact
    ? fmtPts(cell.side.headline)
    : k === 'specialized' && cell.nodes.length
      ? plural(cell.nodes.length, 'node')
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
      <span className="sr-only"> ({cellWords(k, cell)})</span>
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
              {p.opportunity !== null ? ` · Export Opportunity ${fmtScore(p.opportunity)}` : ''}
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
export function roleLine(r: CountryRoles, k: MatrixRole): string {
  return cellWords(k, r.cells[k]);
}

