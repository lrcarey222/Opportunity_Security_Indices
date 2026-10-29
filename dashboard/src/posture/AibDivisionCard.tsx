// The AIB division card (design L1, L4, 2026-10-28): the allied division of labour as a technology x
// stage matrix, as a ChartCard for the AIB Overview's Section (heading level 3 by default). Each node
// names its primary allied holder and up to two backups, its redundancy (allied holders with at least 2%
// of supply) and its gap: no trusted supplier, single point of failure, insufficient redundancy or weak
// evidence. The number in the corner is the Partner demand: how many countries' posture asks trusted
// partners to supply or secure the node. Columns are the stages that make or supply something
// (SUPPLY_PRODUCING_STAGES): no allied supply node sits at Deployment, and the notes say so. Takeaway as
// the sub and the figure's summary; the gap legend in the legend slot; a table twin of the division nodes
// and a CSV with the posture provenance (not the AIB scenario run: the division does not follow the
// scenario toggle). The node detail renders below the card, so a matrix cell or a table row opens it; it
// lists and links the Allied Industrial Base nodes in the cell when the page passes them (`nodesByCell`,
// keyed by cell slug; posture/ imports nothing from aib/). The three header metrics are AibDivisionStats
// (PostureCounts.tsx), shown inside the card only with `stats`.

import { useMemo, useState } from 'react';
import { Link } from 'react-router';
import { ChartCard } from '../charts/ChartCard.tsx';
import { TableView } from '../components/TableView.tsx';
import { Loading, Notice, Select } from '../components/ui.tsx';
import { cellSlug, isSupplyStage } from '../lib/stages.ts';
import type { DivisionNode } from './contract.ts';
import { aibTakeaway, cellKey, countryNames, DIVISION_CSV_COLS, divisionCsvRows, divisionRows, pct, postureCsvText, postureProvenance, POSTURE_SOURCE, producingStagesOf } from './derive.ts';
import { GAP_LABEL, GAP_ORDER, GAP_RING, stageShort } from './grammar.ts';
import { AibDivisionStats } from './PostureCounts.tsx';
import { TradeFreshness } from './PostureLegend.tsx';
import { usePostureCore } from './store.ts';
import { DIVISION_COLS } from './tables.tsx';

export const AIB_DIVISION_TITLE = 'Trusted holders and gaps, node by node';

/** An Allied Industrial Base node in a cell, as the page names and links it. */
export interface PostureLinkedNode {
  id: string;
  label: string;
  /** its page (a compact, a node profile); no link without one */
  href?: string | null;
}

/** Group nodes by the six-stage cells they belong to (a node in several cells is listed under each), keyed by cell slug. */
export function indexNodesByCell(nodes: readonly (PostureLinkedNode & { cells: readonly string[] })[]): Record<string, PostureLinkedNode[]> {
  const out: Record<string, PostureLinkedNode[]> = {};
  for (const n of nodes) {
    for (const cell of new Set(n.cells)) (out[cell] ??= []).push({ id: n.id, label: n.label, href: n.href ?? null });
  }
  return out;
}

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

function NodeDetail({ d, names, nodes }: { d: DivisionNode; names: Record<string, string>; nodes: readonly PostureLinkedNode[] | null }) {
  return (
    <div className="mt-3 rounded-md border border-white/10 bg-ink-900/60 p-3 text-sm text-paper-dim" data-testid="aib-node-detail" aria-live="polite">
      <p className="font-medium text-paper">
        {d.tech} · {d.stage}: {GAP_LABEL[d.gap]}
        {d.gaps.length > 1 ? ` (also ${d.gaps.slice(1).map((g) => GAP_LABEL[g].toLowerCase()).join(', ')})` : ''}
      </p>
      <p className="mt-1">
        Primary allied holder {d.primary ? `${names[d.primary] ?? d.primary} (${pct(d.primaryShare)})` : 'none'}; backups{' '}
        {d.backups.length ? d.backups.map((b) => `${names[b.iso3] ?? b.iso3} (${pct(b.share)})`).join(', ') : 'none'}. Allied supply {pct(d.alliedShare)}; coercive powers{' '}
        {pct(d.coerciveShare)}; world leader {d.leader ? `${names[d.leader] ?? d.leader} (${pct(d.leaderShare)})` : '—'}.
      </p>
      <p className="mt-1 text-xs text-paper-mute">
        Partner demand {d.partnerDemand} countries ({d.memberPartnerDemand} coalition members), {d.partnerGaps} with no credible trusted supplier; Secure in {d.secureDemand}.
        Capability from {d.source === 'aib_position_share' ? `the Allied Industrial Base (${d.aibNodes ?? 0} nodes)` : 'world supply shares (no Allied Industrial Base node)'}.
      </p>
      {nodes && (
        <div className="mt-2 text-xs" data-testid="aib-node-detail-nodes">
          {nodes.length ? (
            <>
              <span className="text-paper-mute">Allied Industrial Base nodes in this cell: </span>
              {nodes.map((n, i) => (
                <span key={n.id}>
                  {i > 0 ? ', ' : ''}
                  {n.href ? (
                    <Link className="link" to={n.href}>
                      {n.label}
                    </Link>
                  ) : (
                    <span className="text-paper-dim">{n.label}</span>
                  )}
                </span>
              ))}
            </>
          ) : (
            <span className="text-paper-mute">No Allied Industrial Base node is mapped to this cell.</span>
          )}
        </div>
      )}
    </div>
  );
}

export interface AibDivisionCardProps {
  title?: string;
  /** 3 (default) inside the page's Section; 2 only for a standalone mount */
  headingLevel?: 2 | 3;
  /** the Technology filter ('all' or a technology), when the page holds it (e.g. in the URL); omit to keep it in the card */
  tech?: string;
  onTechChange?: (tech: string) => void;
  /** the card's own Technology select (default true; false when the page's filter bar sets `tech`) */
  techFilter?: boolean;
  /** the open node ("Tech|Stage"), when the page holds it; omit to keep it in the card */
  selected?: string | null;
  onSelect?: (key: string | null) => void;
  /** the Allied Industrial Base nodes of each cell, keyed by cell slug ("batteries-refining"; indexNodesByCell builds it) */
  nodesByCell?: Readonly<Record<string, readonly PostureLinkedNode[]>>;
  /** the three header metrics above the card (default off: the page shows AibDivisionStats in its own place) */
  stats?: boolean;
  className?: string;
}

export function AibDivisionCard({
  title = AIB_DIVISION_TITLE, headingLevel = 3, tech: techIn, onTechChange, techFilter = true, selected, onSelect, nodesByCell, stats = false, className,
}: AibDivisionCardProps) {
  const core = usePostureCore();
  const [ownTech, setOwnTech] = useState<string>('all');
  const [ownSel, setOwnSel] = useState<string | null>(null);
  const names = useMemo(() => (core.data ? countryNames(core.data) : {}), [core.data]);
  if (core.status === 'error') {
    return (
      <Notice kind="warning" title="The posture data could not be loaded">
        {core.error}
      </Notice>
    );
  }
  if (!core.data) return <Loading label="Loading the allied division of labour" />;
  const c = core.data;
  // a controlled value the data does not know shows all technologies (the page checks its own URL value)
  const tech = techIn === undefined ? ownTech : techIn === 'all' || c.meta.techs.includes(techIn) ? techIn : 'all';
  const setTech = (t: string) => {
    if (techIn === undefined) setOwnTech(t);
    onTechChange?.(t);
  };
  const sel = selected !== undefined ? selected : ownSel;
  const setSel = (k: string | null) => {
    if (selected === undefined) setOwnSel(k);
    onSelect?.(k);
  };
  const toggle = (k: string) => setSel(sel === k ? null : k);
  const stages = producingStagesOf(c.meta.stages);
  const techs = tech === 'all' ? c.meta.techs : [tech];
  const by = new Map(c.division.map((d) => [cellKey(d.tech, d.stage), d]));
  const selNode = sel ? by.get(sel) : undefined;
  const rows = divisionRows(c, tech === 'all' ? null : tech);
  const takeaway = aibTakeaway(c.division, tech === 'all' ? null : tech);
  const cols = `minmax(4.25rem, 9rem) repeat(${stages.length}, minmax(2.8rem, 1fr))`;
  const nodesOf = (d: DivisionNode): readonly PostureLinkedNode[] | null =>
    nodesByCell && isSupplyStage(d.stage) ? (nodesByCell[cellSlug(d.tech, d.stage)] ?? []) : null;
  const csv = () => ({
    name: 'allied holders by technology and stage',
    text: postureCsvText(divisionCsvRows(rows), DIVISION_CSV_COLS, postureProvenance(c, `${POSTURE_SOURCE} (allied division of labour)`)),
  });
  return (
    <div className={className} data-testid="aib-division-hero">
      {stats && <AibDivisionStats className="mb-4" />}
      <ChartCard
        headingLevel={headingLevel}
        title={title}
        sub={<span data-testid="posture-takeaway">{takeaway}</span>}
        summary={takeaway}
        tools={
          techFilter ? (
            <Select
              label="Technology"
              value={tech}
              options={[{ value: 'all', label: 'All technologies' }, ...c.meta.techs.map((t) => ({ value: t, label: t }))]}
              onChange={setTech}
            />
          ) : undefined
        }
        table={() => (
          <TableView rows={rows} cols={DIVISION_COLS} caption="Allied holders and gaps by technology and stage" rowKey={(r) => r.key} onRowClick={(r) => toggle(r.key)} selectedKey={sel} pageSize={60} />
        )}
        csv={csv}
        legend={
          <div className="flex flex-wrap gap-x-4 gap-y-2 text-xs text-paper-dim" aria-label="Legend">
            {GAP_ORDER.map((g) => (
              <span key={g} className="inline-flex items-center gap-1.5">
                <span aria-hidden="true" className="inline-block h-4 w-4 rounded-[3px] bg-ink-900" style={{ boxShadow: `inset 0 0 0 2px ${g === 'none' ? 'rgb(255 255 255 / 0.15)' : GAP_RING[g]}` }} />
                {GAP_LABEL[g]}
              </span>
            ))}
            <span>
              <span className="text-signal">n</span> countries asking partners to supply or secure the node
            </span>
          </div>
        }
        notes={
          <div className="space-y-1">
            <TradeFreshness meta={c.meta} />
            <p>Deployment is left out: it is where the technology is used, and no allied supply node sits there. Select a node, or a row of the table, for its holders and gaps.</p>
          </div>
        }
      >
        <div className="overflow-x-auto" role="grid" aria-label="Allied holders by technology and stage" data-testid="aib-matrix">
          <div role="row" className="grid gap-[2px]" style={{ gridTemplateColumns: cols }}>
            <span role="columnheader">
              <span className="sr-only">Technology</span>
            </span>
            {stages.map((s) => (
              <span key={s} role="columnheader" className="truncate pb-1 text-center text-[11px] font-medium text-paper-mute sm:text-xs" title={s}>
                <span className="sm:hidden">{stageShort(s)}</span>
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
                <NodeCell key={s} d={by.get(cellKey(t, s))} names={names} on={sel === cellKey(t, s)} onSelect={() => toggle(cellKey(t, s))} />
              ))}
            </div>
          ))}
        </div>
      </ChartCard>
      {selNode && <NodeDetail d={selNode} names={names} nodes={nodesOf(selNode)} />}
    </div>
  );
}
