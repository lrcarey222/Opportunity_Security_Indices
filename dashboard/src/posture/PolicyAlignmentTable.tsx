// Policy Alignment (spec XV; design 5b, D5), presentational: "Does current policy match the strategy?" A
// compact table of one economy's assessed nodes, the ones needing attention first: node, recommendation,
// observed policy intensity, support, protection, policy momentum (where the time series supports it, else
// "not available") and alignment with its confidence. Intensity, support, protection and momentum are
// percentile ranks among the country's own nodes (high: the top third), never shares or changes; protection
// reads "none recorded" where the records hold no protection measure. No overall score: each node is judged
// on its own.
// Alignment uses the site grammar (aligned, under-prioritized, over-prioritized, wrong treatment /
// misaligned, mixed; a deployment gap reads "under-prioritized: under-response (deployment)"). Observed
// policy is a comparator, never an input to the choice. The PAMS / NIPO / CAT detail goes below (children).
// Each node's name is a button that opens the shared drawer (onOpen, spec XIX); a table twin and a CSV carry
// the rows. Values come from the economy's detail document (null while it loads: the alignment shows from
// the core, the rest says "…").

import { useState, type ReactNode } from 'react';
import { ChartCard } from '../charts/ChartCard.tsx';
import { TableView, type Col } from '../components/TableView.tsx';
import { Term } from '../components/Term.tsx';
import { stageRank } from '../lib/stages.ts';
import { POSTURE_TERMS, PostureBasisTag } from './basis.tsx';
import type { Alignment, PostureCell, PostureCore, PostureDetail } from './contract.ts';
import { namePossessive, postureCsvText, postureProvenance } from './derive.ts';
import {
  ALIGNMENT_BORDER, ALIGNMENT_CLASS_LABEL, ALIGNMENT_CONFIDENCE_LABEL, ALIGNMENT_ICON, ALIGNMENT_LABEL, alignmentClass, stageDisplay, VERB_LONG,
} from './grammar.ts';
import { countryRankWords, intensityWord, policyMomentumWords, policyRows, protectionWords, type PolicyRow } from './sections.ts';

export { policyMomentumWords };

export const POLICY_ALIGNMENT_TITLE = 'Does current policy match the strategy?';

export interface PolicyAlignmentTableProps {
  core: PostureCore;
  /** the economy's core cells */
  cells: PostureCell[];
  /** the economy's detail document (policy evidence); null while it loads */
  detail: PostureDetail | null;
  name: string;
  selected?: string | null;
  onOpen: (key: string) => void;
  /** rows shown before "Show all" (default 12) */
  limit?: number;
  title?: string;
  headingLevel?: 2 | 3;
  showBasis?: boolean;
  /** the PAMS / NIPO / CAT detail, below the table */
  children?: ReactNode;
  className?: string;
}

/** The rank words of a table cell ("high, 92nd percentile"): the headers and the note say the ranks are within the country. */
const rank = (v: number | null) => countryRankWords(v, true);
const protectionCell = (r: PolicyRow) => protectionWords(r.protection, r.protectionRecorded, true);

function AlignmentChip({ a }: { a: Alignment | null }) {
  if (!a) return <span className="text-paper-mute">not assessed</span>;
  const cls = alignmentClass(a);
  return (
    <span className="inline-flex items-center gap-1.5" data-alignment={a}>
      <span aria-hidden="true" className="inline-flex h-3.5 w-3.5 shrink-0 items-center justify-center rounded-[3px] text-[8px] leading-none" style={{ border: ALIGNMENT_BORDER[cls] }}>
        {ALIGNMENT_ICON[cls] ?? ''}
      </span>
      <span>{ALIGNMENT_LABEL[a]}</span>
    </span>
  );
}

const COLS: Col<PolicyRow & { node: string }>[] = [
  { id: 'node', header: 'Node', value: (r) => r.node, sortValue: (r) => stageRank(r.stage) },
  { id: 'recommendation', header: 'Recommendation', value: (r) => VERB_LONG[r.verb] },
  { id: 'intensity', header: 'Observed policy intensity (rank in the country)', value: (r) => r.intensity, cell: (r) => rank(r.intensity) },
  { id: 'support', header: 'Support (rank in the country)', value: (r) => r.support, cell: (r) => rank(r.support) },
  { id: 'protection', header: 'Protection or localisation (rank in the country)', value: (r) => (r.protectionRecorded === false ? 0 : r.protection), cell: protectionCell },
  { id: 'momentum', header: 'Policy momentum (rank of recent activity in the country)', value: (r) => r.momentum, cell: (r) => policyMomentumWords(r.momentum, true) },
  { id: 'alignment', header: 'Alignment', value: (r) => (r.alignment ? ALIGNMENT_LABEL[r.alignment] : null) },
  { id: 'confidence', header: 'Alignment confidence', value: (r) => (r.alignmentConfidence ? ALIGNMENT_CONFIDENCE_LABEL[r.alignmentConfidence] : null) },
  { id: 'sources', header: 'Sources', value: (r) => r.sources.join(', ') || null },
];

export function PolicyAlignmentTable({
  core, cells, detail, name, selected = null, onOpen, limit = 12, title = POLICY_ALIGNMENT_TITLE, headingLevel = 3, showBasis = false, children, className,
}: PolicyAlignmentTableProps) {
  const [all, setAll] = useState(false);
  const rows = policyRows(cells, detail).map((r) => ({ ...r, node: `${r.tech} · ${stageDisplay(r.stage)}` }));
  const shown = all ? rows : rows.slice(0, limit);
  const loading = detail === null;
  const by = new Map<string, number>();
  for (const r of rows) {
    const k = r.alignment ? alignmentClass(r.alignment) : 'none';
    by.set(k, (by.get(k) ?? 0) + 1);
  }
  const depGap = rows.filter((r) => r.alignment === 'deployment_gap').length;
  const coverage = (['misaligned', 'under_prioritized', 'over_prioritized', 'mixed', 'aligned', 'insufficient_data'] as const)
    .filter((k) => by.get(k))
    .map((k) => `${by.get(k)} ${ALIGNMENT_CLASS_LABEL[k].toLowerCase()}${k === 'under_prioritized' && depGap ? ` (${depGap} deployment under-response)` : ''}`);
  const conf = (['high', 'medium', 'low'] as const).map((k) => [k, rows.filter((r) => r.alignmentConfidence === k).length] as const).filter(([, n]) => n > 0);
  const notJudged = by.get('none') ?? 0;
  const summary = `${name}: policy alignment of ${rows.length} assessed nodes: ${coverage.join(', ') || 'none judged'}${notJudged ? `; ${notJudged} not judged` : ''}.`;
  const prov = postureProvenance(core);
  return (
    <ChartCard
      className={className}
      headingLevel={headingLevel}
      title={title}
      badge={showBasis ? <PostureBasisTag note={null} /> : undefined}
      sub={
        <>
          Observed policy compared with each node&apos;s recommended choice, node by node: <Term id={POSTURE_TERMS.policyAlignment}>policy alignment</Term>. Policy is a comparator, never
          an input to the choice, and there is no overall score.
        </>
      }
      summary={summary}
      table={() => <TableView rows={rows} cols={COLS} caption={`${name}: policy alignment by node`} rowKey={(r) => r.key} onRowClick={(r) => onOpen(r.key)} selectedKey={selected} pageSize={70} />}
      csv={() => ({
        name: `${name} policy alignment`,
        text: postureCsvText(
          // intensity, support, protection and momentum are within-country percentile ranks (0-1), named so: never shares or changes
          rows.map((r) => ({
            technology: r.tech, stage: r.stage, stage_name: stageDisplay(r.stage), recommendation: VERB_LONG[r.verb],
            policy_intensity_rank_in_country: r.intensity, policy_intensity_words: intensityWord(r.intensity),
            support_rank_in_country: r.support, support_words: countryRankWords(r.support),
            protection_rank_in_country: r.protection, protection_recorded: r.protectionRecorded, protection_words: protectionWords(r.protection, r.protectionRecorded),
            policy_momentum_rank_in_country: typeof r.momentum === 'number' ? r.momentum : null, policy_momentum_words: policyMomentumWords(r.momentum),
            alignment: r.alignment, alignment_words: r.alignment ? ALIGNMENT_LABEL[r.alignment] : null, alignment_confidence: r.alignmentConfidence, sources: r.sources.join(';') || null,
          })),
          [
            'technology', 'stage', 'stage_name', 'recommendation', 'policy_intensity_rank_in_country', 'policy_intensity_words', 'support_rank_in_country', 'support_words',
            'protection_rank_in_country', 'protection_recorded', 'protection_words', 'policy_momentum_rank_in_country', 'policy_momentum_words', 'alignment', 'alignment_words',
            'alignment_confidence', 'sources',
          ],
          prov,
        ),
      })}
      notes={
        <div className="space-y-1">
          <p data-testid="policy-coverage">
            {coverage.length ? `${coverage.join(' · ')}.` : 'No node has a judged alignment.'}
            {conf.length ? ` Alignment confidence: ${conf.map(([k, n]) => `${n} ${k}`).join(', ')} (from how complete the policy records are).` : ''}
          </p>
          <p data-testid="policy-ranks-note">
            Intensity, support, protection and momentum are percentile ranks among {namePossessive(name)} own nodes (high: the top third; low: the bottom third), not shares of policy
            attention or changes. Intensity ranks the node&apos;s revealed policy priority; support and protection rank the support and the protection or localisation measures
            recorded for it, and protection reads &ldquo;none recorded&rdquo; where there are none; momentum ranks recent policy activity and is shown only where the time series
            supports it.
          </p>
        </div>
      }
    >
      <div className="overflow-x-auto" data-testid="policy-alignment">
        <table className="w-full min-w-[40rem] text-left text-xs">
          <thead className="text-paper-mute">
            <tr>
              <th scope="col" className="py-1 pr-2 font-medium">Node</th>
              <th scope="col" className="py-1 pr-2 font-medium">Recommendation</th>
              <th scope="col" className="py-1 pr-2 font-medium">Observed policy intensity</th>
              <th scope="col" className="py-1 pr-2 font-medium">Support</th>
              <th scope="col" className="py-1 pr-2 font-medium">Protection</th>
              <th scope="col" className="py-1 pr-2 font-medium">Policy momentum</th>
              <th scope="col" className="py-1 font-medium">Alignment</th>
            </tr>
          </thead>
          <tbody>
            {shown.map((r) => (
              <tr key={r.key} className={`border-t border-white/5 ${selected === r.key ? 'bg-white/5' : ''}`} data-key={r.key} data-alignment={r.alignment ?? undefined}>
                <th scope="row" className="py-1.5 pr-2 font-normal">
                  <button type="button" className="text-left text-paper hover:underline focus-visible:ring-2 focus-visible:ring-signal" aria-pressed={selected === r.key} onClick={() => onOpen(r.key)}>
                    {r.node}
                  </button>
                </th>
                <td className="py-1.5 pr-2 text-paper-dim">{VERB_LONG[r.verb]}</td>
                <td className="py-1.5 pr-2 text-paper-dim">{loading ? '…' : rank(r.intensity)}</td>
                <td className="py-1.5 pr-2 text-paper-dim">{loading ? '…' : rank(r.support)}</td>
                <td className="py-1.5 pr-2 text-paper-dim">{loading ? '…' : protectionCell(r)}</td>
                <td className="py-1.5 pr-2 text-paper-dim">{loading ? '…' : policyMomentumWords(r.momentum, true)}</td>
                <td className="py-1.5 text-paper-dim">
                  <AlignmentChip a={r.alignment} />
                  {r.alignmentConfidence && <span className="ml-1 text-paper-mute">({ALIGNMENT_CONFIDENCE_LABEL[r.alignmentConfidence].toLowerCase()} confidence)</span>}
                </td>
              </tr>
            ))}
          </tbody>
        </table>
      </div>
      {rows.length > limit && (
        <button type="button" className="btn-quiet mt-2 text-xs" aria-expanded={all} onClick={() => setAll((v) => !v)}>
          {all ? `Show the first ${limit}` : `Show all ${rows.length}`}
        </button>
      )}
      {children && <div className="mt-4">{children}</div>}
    </ChartCard>
  );
}
