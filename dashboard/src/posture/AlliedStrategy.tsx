// Allied Strategy (spec XVI; design 5b), presentational: the economy's needs that allies should meet. It
// opens with a sentence computed from the outputs ("2 of Aland's priority nodes are better solved through
// allies; 1 Secure node also depends on trusted foreign capacity"), then one row per node: country need ->
// recommended partner(s) -> why -> AIB status (covered or gap), with the gap logic in words (no trusted
// supplier, only one credible supplier, weak capability evidence, insufficient allied capacity, excessive
// concentration) and an honest "no credible allied solution yet" where there is none: not every Partner
// choice has a solution. The rows are the canonical partner outputs (sections.ts alliedRows over the core,
// the economy's detail document and the allied division), or the page's own (rows). The generic partnership
// material goes under a collapsed "Explore all partnership analysis" (explore). Each node's name is a
// button that opens the shared drawer (onOpen, spec XIX); partner and AIB links come from the page
// (partnerHref, aibHref: posture/ knows no Country or AIB routes). A table twin and a CSV carry the rows.

import type { ReactNode } from 'react';
import { Link } from 'react-router';
import { ChartCard } from '../charts/ChartCard.tsx';
import { TableView, type Col } from '../components/TableView.tsx';
import { Term } from '../components/Term.tsx';
import { Disclosure } from '../components/ui.tsx';
import { stageRank } from '../lib/stages.ts';
import { POSTURE_TERMS, PostureBasisTag } from './basis.tsx';
import type { PostureCell, PostureCore, PostureDetail } from './contract.ts';
import { countryNames, pct, postureCsvText, postureProvenance } from './derive.ts';
import { cellMark, deliveryWords, POSTURE_COLOR, RATIONALE_LABEL, stageDisplay } from './grammar.ts';
import { Swatch } from './PostureLegend.tsx';
import { alliedRows, alliedSentence, type AlliedRow } from './sections.ts';

export const ALLIED_STRATEGY_TITLE = 'Allied Strategy';
export const EXPLORE_PARTNERSHIPS = 'Explore all partnership analysis';

export interface AlliedStrategyProps {
  core: PostureCore;
  /** the economy's core cells */
  cells: PostureCell[];
  /** the economy's detail document (recommended partners, delivery mode, gap reasons); null while it loads */
  detail: PostureDetail | null;
  name: string;
  /** the page's own rows (else alliedRows over the core, the detail and the allied division) */
  rows?: AlliedRow[];
  selected?: string | null;
  onOpen: (key: string) => void;
  /** a partner's page (e.g. its Country profile), or null for no link */
  partnerHref?: (iso3: string) => string | null;
  /** the node in the AIB view, or null for no link */
  aibHref?: (tech: string, stage: string) => string | null;
  /** the generic partnership material (partner opportunity, Friendshore, alignment, trade partners, development potential, network charts), collapsed */
  explore?: ReactNode;
  title?: string;
  headingLevel?: 2 | 3;
  showBasis?: boolean;
  className?: string;
}

const STATUS_WORD: Record<AlliedRow['status'], string> = { covered: 'Covered', gap: 'Gap', not_mapped: 'Not mapped' };

/** The AIB status of a row in words, with its gap logic. */
export function alliedStatusWords(r: AlliedRow): string {
  if (r.noSolution) return `Gap: no credible allied solution yet${r.gaps.length ? ` (${r.gaps.join('; ')})` : ''}`;
  if (r.status === 'gap') return `Gap${r.gaps.length ? `: ${r.gaps.join('; ')}` : ''}`;
  if (r.status === 'covered') return `Covered${r.gaps.length ? ` (note: ${r.gaps.join('; ')})` : ''}`;
  return 'Not mapped in the allied division';
}

type Row = AlliedRow & { node: string; choice: string; partnersWords: string; whyWords: string; statusWords: string };

const COLS: Col<Row>[] = [
  { id: 'node', header: 'Node', value: (r) => r.node, sortValue: (r) => stageRank(r.stage) },
  { id: 'choice', header: 'Choice', value: (r) => r.choice },
  { id: 'need', header: 'Supply-chain need', value: (r) => r.need, cell: (r) => pct(r.need), num: true },
  { id: 'partners', header: 'Recommended partners', value: (r) => r.partnersWords },
  { id: 'why', header: 'Why', value: (r) => r.whyWords || null },
  { id: 'status', header: 'AIB status', value: (r) => r.statusWords },
];

export function AlliedStrategy({
  core, cells, detail, name, rows: rowsIn, selected = null, onOpen, partnerHref, aibHref, explore, title = ALLIED_STRATEGY_TITLE, headingLevel = 3, showBasis = false, className,
}: AlliedStrategyProps) {
  const names = countryNames(core);
  const base = rowsIn ?? alliedRows(cells, detail, core.division, names);
  const sentence = alliedSentence(name, cells, base);
  const rows: Row[] = base.map((r) => ({
    ...r,
    node: `${r.tech} · ${stageDisplay(r.stage)}`,
    choice: r.verb === 'secure' ? `Secure${r.rationale ? ` (${RATIONALE_LABEL[r.rationale].toLowerCase()})` : ''}` : 'Compete · Partner',
    partnersWords: r.partners.length ? r.partners.map((p) => p.name).join(', ') : r.noSolution ? 'none yet' : 'not stated',
    whyWords: r.partners.map((p) => (p.why.length ? `${p.name}: ${p.why.join(', ')}` : '')).filter(Boolean).join('; '),
    statusWords: alliedStatusWords(r),
  }));
  const gaps = rows.filter((r) => r.status === 'gap').length;
  const prov = postureProvenance(core);
  return (
    <div className={className}>
      <ChartCard
        headingLevel={headingLevel}
        title={title}
        badge={showBasis ? <PostureBasisTag note={null} /> : undefined}
        sub={<span data-testid="allied-sentence">{sentence}</span>}
        summary={`${sentence} ${rows.length} nodes listed; ${gaps} with an AIB gap.`}
        table={() => <TableView rows={rows} cols={COLS} caption={`${name}: needs met with allies`} rowKey={(r) => r.key} onRowClick={(r) => onOpen(r.key)} selectedKey={selected} pageSize={50} />}
        csv={() => ({
          name: `${name} allied strategy`,
          text: postureCsvText(
            rows.map((r) => ({
              technology: r.tech, stage: r.stage, stage_name: stageDisplay(r.stage), choice: r.choice, supply_chain_need: r.need, delivery_mode: r.delivery,
              recommended_partners: r.partnersWords, recommended_partners_iso3: r.partners.map((p) => p.iso3).join(';') || null, why: r.whyWords || null,
              aib_status: r.status, gap_reasons: r.gaps.join(';') || null, no_credible_allied_solution: r.noSolution,
            })),
            ['technology', 'stage', 'stage_name', 'choice', 'supply_chain_need', 'delivery_mode', 'recommended_partners', 'recommended_partners_iso3', 'why', 'aib_status', 'gap_reasons', 'no_credible_allied_solution'],
            prov,
          ),
        })}
        notes={
          <p>
            Rows: every Partner node, and every Secure node whose delivery leans on trusted foreign capacity (allied or mixed). AIB status is the{' '}
            <Term id={POSTURE_TERMS.aibGap}>allied division&apos;s</Term> coverage of the node: covered when a credible trusted holder and backups exist.
          </p>
        }
      >
        {rows.length ? (
          <ul className="space-y-2" data-testid="allied-strategy">
            {rows.map((r) => {
              const href = aibHref ? aibHref(r.tech, r.stage) : null;
              const mark = r.verb === 'partner' ? cellMark({ posture: 'C', mode: 'partner', deployMode: null, rationale: null }) : cellMark({ posture: 'S', mode: null, deployMode: null, rationale: r.rationale });
              return (
                <li key={r.key} className="rounded-lg border border-white/10 bg-ink-900/40 p-3" data-key={r.key} data-status={r.status} data-verb={r.verb}>
                  <div className="grid gap-2 md:grid-cols-[minmax(0,1.1fr)_minmax(0,1fr)_minmax(0,1.3fr)_minmax(0,1fr)] md:items-start">
                    <div>
                      <p className="text-[10px] uppercase tracking-wide text-paper-mute">{name} needs</p>
                      <button type="button" className="inline-flex items-center gap-1.5 text-left text-sm text-paper hover:underline focus-visible:ring-2 focus-visible:ring-signal" aria-pressed={selected === r.key} onClick={() => onOpen(r.key)}>
                        <Swatch color={POSTURE_COLOR[r.verb === 'partner' ? 'C' : 'S']} mark={mark} size={16} />
                        {r.node}
                      </button>
                      <p className="text-xs text-paper-dim">
                        {r.choice}
                        {r.need !== null ? ` · need ${pct(r.need)}` : ''}
                        {r.delivery ? ` · delivery ${deliveryWords(r.delivery)}` : ''}
                      </p>
                    </div>
                    <div>
                      <p className="text-[10px] uppercase tracking-wide text-paper-mute">Recommended</p>
                      {r.partners.length ? (
                        <ul className="text-sm text-paper">
                          {r.partners.map((p) => {
                            const ph = partnerHref ? partnerHref(p.iso3) : null;
                            return <li key={p.iso3}>{ph ? <Link className="link" to={ph}>{p.name}</Link> : p.name}</li>;
                          })}
                        </ul>
                      ) : (
                        <p className="text-sm text-paper-mute">{r.noSolution ? 'No credible allied solution yet' : detail ? 'Not stated' : '…'}</p>
                      )}
                    </div>
                    <div>
                      <p className="text-[10px] uppercase tracking-wide text-paper-mute">Why</p>
                      {r.whyWords ? (
                        <ul className="text-xs text-paper-dim">
                          {r.partners
                            .filter((p) => p.why.length)
                            .map((p) => (
                              <li key={p.iso3}>
                                {r.partners.length > 1 ? `${p.name}: ` : ''}
                                {p.why.join('; ')}
                              </li>
                            ))}
                        </ul>
                      ) : (
                        <p className="text-xs text-paper-mute">{r.noSolution ? 'No trusted supplier can cover it yet.' : 'not stated'}</p>
                      )}
                    </div>
                    <div>
                      <p className="text-[10px] uppercase tracking-wide text-paper-mute">AIB status</p>
                      <p className={`text-sm ${r.status === 'gap' ? 'text-signal' : 'text-paper'}`} data-testid="allied-status">
                        {r.noSolution ? 'Gap · no credible allied solution yet' : STATUS_WORD[r.status]}
                      </p>
                      {r.gaps.length > 0 && (
                        <ul className="text-xs text-paper-dim" data-testid="allied-gaps">
                          {r.gaps.map((g) => (
                            <li key={g}>{g}</li>
                          ))}
                        </ul>
                      )}
                      {href && (
                        <Link className="link text-xs" to={href} data-testid="allied-aib-link">
                          In the allied base &rarr;
                        </Link>
                      )}
                    </div>
                  </div>
                </li>
              );
            })}
          </ul>
        ) : (
          <p className="text-sm text-paper-mute" data-testid="allied-strategy">
            No node of {name} is a Partner choice, and no Secure node leans on trusted foreign capacity.
          </p>
        )}
      </ChartCard>
      {explore && (
        <div className="mt-4" data-testid="allied-explore">
          <Disclosure summary={EXPLORE_PARTNERSHIPS}>{explore}</Disclosure>
        </div>
      )}
    </div>
  );
}
