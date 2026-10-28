// The AIB view's headline: the allied division of labour as a technology x six-stage matrix. Each node
// names its primary allied holder and up to two backups, its redundancy (allied holders with at least 2%
// of supply) and its gap: no trusted supplier, single point of failure, insufficient redundancy or weak
// evidence. The number in the corner is the Partner demand: how many countries' posture asks trusted
// partners to supply the node. Header: one sentence and three metrics.

import { useMemo, useState } from 'react';
import { Loading, Notice, Select, StatTile } from '../components/ui.tsx';
import type { DivisionNode } from './contract.ts';
import { aibHeader, aibTakeaway, pct } from './derive.ts';
import { GAP_LABEL, GAP_ORDER, GAP_RING, STAGE_SHORT } from './grammar.ts';
import { TradeFreshness } from './PostureLegend.tsx';
import { usePostureCore } from './store.ts';

function NodeCell({ d, names, on, onSelect }: { d: DivisionNode | undefined; names: Record<string, string>; on: boolean; onSelect: () => void }) {
  if (!d) return <span role="gridcell" className="h-14 rounded-[3px] bg-ink-700/30" aria-label="Not traded: no division" />;
  const label = `${d.tech} ${d.stage}: ${d.primary ? `primary ${names[d.primary] ?? d.primary} ${pct(d.primaryShare)}` : 'no allied holder'}; backups ${d.backups.map((b) => b.iso3).join(', ') || 'none'}; redundancy ${d.redundancy}; ${GAP_LABEL[d.gap]}; partner demand ${d.partnerDemand}`;
  return (
    <span role="gridcell" className="relative">
      <button
        type="button"
        onClick={onSelect}
        aria-label={label}
        aria-pressed={on}
        className="flex h-14 w-full flex-col items-start justify-center overflow-hidden rounded-[3px] bg-ink-900/70 px-1.5 text-left outline-none focus-visible:ring-2 focus-visible:ring-signal"
        style={{ boxShadow: `inset 0 0 0 2px ${GAP_RING[d.gap]}${on ? ', 0 0 0 2px #ffffff' : ''}` }}
        data-gap={d.gap}
      >
        <span className="text-xs font-semibold text-paper">{d.primary ?? '—'}</span>
        <span className="truncate text-[10px] text-paper-mute">{d.backups.map((b) => b.iso3).join(' · ') || 'no backup'}</span>
        {d.partnerDemand + d.secureDemand > 0 && (
          <span className="absolute right-1 top-0.5 text-[10px] tabular text-signal" aria-hidden="true">
            {d.partnerDemand + d.secureDemand}
          </span>
        )}
      </button>
    </span>
  );
}

export function AibDivisionHero() {
  const core = usePostureCore();
  const [tech, setTech] = useState<string>('all');
  const [sel, setSel] = useState<string | null>(null);
  const names = useMemo(() => Object.fromEntries((core.data?.countries ?? []).map((c) => [c.iso3, c.name])), [core.data]);
  if (core.status === 'error') {
    return (
      <Notice kind="warning" title="The posture data could not be loaded">
        {core.error}
      </Notice>
    );
  }
  if (!core.data) return <Loading label="Loading the allied division of labour" />;
  const c = core.data;
  const stages = c.meta.stages.filter((s) => s !== 'Deployment');
  const techs = tech === 'all' ? c.meta.techs : [tech];
  const by = new Map(c.division.map((d) => [`${d.tech}|${d.stage}`, d]));
  const h = aibHeader(c);
  const selNode = sel ? by.get(sel) : undefined;
  const cols = `minmax(4.25rem, 9rem) repeat(${stages.length}, minmax(2.8rem, 1fr))`;
  return (
    <section className="card p-5" aria-labelledby="aib-division-title" data-testid="aib-division-hero">
      <div className="flex flex-wrap items-end justify-between gap-x-6 gap-y-3">
        <div className="min-w-0 max-w-3xl">
          <h2 id="aib-division-title" className="rule-neis-sm text-base font-medium text-paper lg:text-lg">
            The allied division of labour
          </h2>
          <p className="mt-2 text-sm text-paper-dim" data-testid="posture-takeaway">
            {aibTakeaway(c.division, tech === 'all' ? null : tech)}
          </p>
        </div>
        <Select label="Technology" value={tech} options={[{ value: 'all', label: 'All technologies' }, ...c.meta.techs.map((t) => ({ value: t, label: t }))]} onChange={(t) => setTech(t)} />
      </div>
      <div className="mt-3 grid grid-cols-1 gap-3 sm:grid-cols-3">
        <StatTile label="Core coalition members" value={h.coreMembers} sub={`${c.aibHeader.members} members in all`} />
        <StatTile label="Priority nodes with credible trusted coverage" value={pct(h.coveredShare)} sub={`of ${h.priorityNodes} nodes members ask partners to supply or secure`} />
        <StatTile label="Unresolved gaps" value={h.unresolvedGaps} sub="priority nodes with a gap" />
      </div>
      <div className="mt-2">
        <TradeFreshness meta={c.meta} />
      </div>
      <div className="mt-4 overflow-x-auto" role="grid" aria-label="Allied holders by technology and stage" data-testid="aib-matrix">
        <div role="row" className="grid gap-[2px]" style={{ gridTemplateColumns: cols }}>
          <span role="columnheader">
          <span className="sr-only">Technology</span>
        </span>
          {stages.map((s) => (
            <span key={s} role="columnheader" className="truncate pb-1 text-center text-[11px] font-medium text-paper-mute sm:text-xs" title={s}>
              <span className="sm:hidden">{STAGE_SHORT[s] ?? s}</span>
              <span className="hidden sm:inline">{s}</span>
            </span>
          ))}
        </div>
        {techs.map((t) => (
          <div key={t} role="row" className="grid gap-[2px] pb-[2px]" style={{ gridTemplateColumns: cols }}>
            <span role="rowheader" className="flex items-center pr-2 text-xs text-paper-dim sm:text-sm">
              {t}
            </span>
            {stages.map((s) => (
              <NodeCell key={s} d={by.get(`${t}|${s}`)} names={names} on={sel === `${t}|${s}`} onSelect={() => setSel((x) => (x === `${t}|${s}` ? null : `${t}|${s}`))} />
            ))}
          </div>
        ))}
      </div>
      <div className="mt-3 flex flex-wrap gap-x-4 gap-y-2 text-xs text-paper-dim" aria-label="Legend">
        {GAP_ORDER.map((g) => (
          <span key={g} className="inline-flex items-center gap-1.5">
            <span aria-hidden="true" className="inline-block h-4 w-4 rounded-[3px] bg-ink-900" style={{ boxShadow: `inset 0 0 0 2px ${g === 'none' ? 'rgb(255 255 255 / 0.15)' : GAP_RING[g]}` }} />
            {GAP_LABEL[g]}
          </span>
        ))}
        <span className="text-signal">n</span> countries asking partners to supply or secure the node
      </div>
      {selNode && (
        <div className="mt-4 rounded-md border border-white/10 bg-ink-900/60 p-3 text-sm text-paper-dim" data-testid="aib-node-detail">
          <p className="font-medium text-paper">
            {selNode.tech} · {selNode.stage}: {GAP_LABEL[selNode.gap]}
            {selNode.gaps.length > 1 ? ` (also ${selNode.gaps.slice(1).map((g) => GAP_LABEL[g].toLowerCase()).join(', ')})` : ''}
          </p>
          <p className="mt-1">
            Primary allied holder {selNode.primary ? `${names[selNode.primary] ?? selNode.primary} (${pct(selNode.primaryShare)})` : 'none'}; backups{' '}
            {selNode.backups.length ? selNode.backups.map((b) => `${names[b.iso3] ?? b.iso3} (${pct(b.share)})`).join(', ') : 'none'}. Allied supply {pct(selNode.alliedShare)}; coercive powers{' '}
            {pct(selNode.coerciveShare)}; world leader {selNode.leader ? `${names[selNode.leader] ?? selNode.leader} (${pct(selNode.leaderShare)})` : '—'}.
          </p>
          <p className="mt-1 text-xs text-paper-mute">
            Partner demand {selNode.partnerDemand} countries ({selNode.memberPartnerDemand} coalition members), {selNode.partnerGaps} with no credible trusted supplier; Secure in{' '}
            {selNode.secureDemand}. Capability from {selNode.source === 'aib_position_share' ? `the Allied Industrial Base (${selNode.aibNodes ?? 0} nodes)` : 'world supply shares (no AIB node)'}.
          </p>
        </div>
      )}
    </section>
  );
}
