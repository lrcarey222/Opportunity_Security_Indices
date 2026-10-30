// Strategic Choices (spec XIV; design 5b), presentational: one economy's nodes grouped under the six verbs,
// each with the subtitle and fields of spec XIV, plus the consistency-review list (design D2) and the count
// of nodes not assessed (design D1). Watch is a positive conclusion, shown in full ("Domestic industrial
// priority: low" and why), not a failure state. Accelerate states the deployment priority apart from the
// technology's domestic manufacturing priority. Partner names the recommended partners, the diversification
// value and the AIB status (covered, gap with its reasons, or no credible allied solution yet) and links to
// the AIB partner recommendations (aibHref; posture/ knows no AIB routes). Each node's name is a button that
// opens the shared drawer (onOpen, spec XIX). A table twin and a CSV list every node with its group.

import { useState } from 'react';
import { Link } from 'react-router';
import { ChartCard } from '../charts/ChartCard.tsx';
import { TableView, type Col } from '../components/TableView.tsx';
import { Term } from '../components/Term.tsx';
import { stageRank } from '../lib/stages.ts';
import { POSTURE_TERMS, PostureBasisTag } from './basis.tsx';
import type { PostureCell, PostureCore, PostureDetail, Verb } from './contract.ts';
import { countryNames, pct, postureCsvText, postureProvenance, score100 } from './derive.ts';
import type { Family, NodeDetail } from './detail.ts';
import {
  ALIGNMENT_LABEL, basisWords, cellMark, choiceWord, deliveryWords, flagWords, PATHWAY_LABEL, pathwayEvidenceWords, POSTURE_COLOR, RATIONALE_LABEL, stageDisplay, VERB_FAMILY, VERB_LONG, VERB_ORDER,
  VERB_SUBTITLE,
} from './grammar.ts';
import { Swatch } from './PostureLegend.tsx';
import {
  alliedRows, catchUpPositionWords, choiceGroups, detailByKey, diversificationWords, domesticCase, manufacturingPriority, manufacturingPriorityWord, secureRisk, secureTreatment, watchReasons,
  type AlliedRow,
} from './sections.ts';

export const STRATEGIC_CHOICES_TITLE = 'Strategic Choices';

export interface StrategicChoicesProps {
  core: PostureCore;
  /** the economy's core cells */
  cells: PostureCell[];
  /** the economy's detail document (fields beyond the core); null while it loads */
  detail: PostureDetail | null;
  /** the economy's name */
  name: string;
  /** the selected node ("Tech|Stage"), shown pressed */
  selected?: string | null;
  /** open a node in the shared drawer */
  onOpen: (key: string) => void;
  /** the node's AIB partner recommendations (the AIB page), or null for no link */
  aibHref?: (tech: string, stage: string) => string | null;
  /** nodes shown per group before "Show all" (default 6) */
  limit?: number;
  title?: string;
  headingLevel?: 2 | 3;
  /** the basis tag in the title row (off when the page's Section carries it) */
  showBasis?: boolean;
  className?: string;
}

const NA = 'not assessed';

function familyWords(f: Family | null | undefined, scenarios: Record<string, string>): string {
  if (!f || f.value === null) return NA;
  return [score100(f.value), f.scenario ? (scenarios[f.scenario] ?? f.scenario) : null, basisWords(f.basis)].filter(Boolean).join(' · ');
}

/** One field of a node under its verb: its label (with the glossary term, when it has one) and its words. */
interface ChoiceField {
  label: string;
  term?: string;
  words: string;
}

function Field({ f }: { f: ChoiceField }) {
  return (
    <div className="flex justify-between gap-3 text-xs">
      <dt className="text-paper-mute">{f.term ? <Term id={f.term}>{f.label}</Term> : f.label}</dt>
      <dd className="text-right text-paper-dim">{f.words}</dd>
    </div>
  );
}

/** The current deployment trend in words ("+8% a year", "rising"), or "not available". */
function trendWords(t: string | number | null): string {
  if (t === null) return 'not available';
  return typeof t === 'number' ? `${t >= 0 ? '+' : ''}${(t * 100).toFixed(0)}% a year` : t;
}

const MARK_OF: Record<Verb, Pick<PostureCell, 'posture' | 'mode' | 'deployMode' | 'rationale'>> = {
  secure: { posture: 'S', mode: null, deployMode: null, rationale: null },
  preserve: { posture: 'C', mode: 'preserve', deployMode: null, rationale: null },
  catch_up: { posture: 'C', mode: 'catch_up', deployMode: null, rationale: null },
  partner: { posture: 'C', mode: 'partner', deployMode: null, rationale: null },
  accelerate: { posture: 'D', mode: null, deployMode: 'accelerate', rationale: null },
  watch: { posture: 'D', mode: null, deployMode: 'watch', rationale: null },
};

/** The AIB status of a Partner node in words. */
function aibStatusWords(a: AlliedRow | undefined): string {
  if (!a) return 'not mapped';
  if (a.noSolution) return `gap: no credible allied solution yet${a.gaps.length ? ` (${a.gaps.join('; ')})` : ''}`;
  if (a.status === 'gap') return `gap${a.gaps.length ? `: ${a.gaps.join('; ')}` : ''}`;
  return a.status === 'covered' ? 'covered' : 'not mapped in the allied division';
}

interface ChoiceRow {
  key: string;
  group: string;
  tech: string;
  stage: string;
  choice: string;
  detail: string;
}

const COLS: Col<ChoiceRow>[] = [
  { id: 'group', header: 'Group', value: (r) => r.group },
  { id: 'tech', header: 'Technology', value: (r) => r.tech },
  { id: 'stage', header: 'Stage', value: (r) => stageDisplay(r.stage), sortValue: (r) => stageRank(r.stage) },
  { id: 'choice', header: 'Choice', value: (r) => r.choice },
  { id: 'detail', header: 'Why', value: (r) => r.detail },
];

export function StrategicChoices({
  core, cells, detail, name, selected = null, onOpen, aibHref, limit = 6, title = STRATEGIC_CHOICES_TITLE, headingLevel = 3, showBasis = false, className,
}: StrategicChoicesProps) {
  const [open, setOpen] = useState<Partial<Record<Verb, boolean>>>({});
  const t = core.meta.thresholds;
  const scen = core.meta.scenario_labels;
  const names = countryNames(core);
  const g = choiceGroups(cells);
  const dk = detailByKey(detail);
  const allied = new Map(alliedRows(cells, detail, core.division, names).map((a) => [a.key, a]));
  const keyOf = (c: PostureCell) => `${c.tech}|${c.stage}`;
  const det = (c: PostureCell): NodeDetail | null => dk.get(keyOf(c)) ?? null;

  /** The fields of one node under its verb (spec XIV); shared by the cards, the table and the CSV. */
  const fields = (v: Verb, c: PostureCell): ChoiceField[] => {
    const d = det(c);
    switch (v) {
      case 'secure':
        return [
          { label: 'Rationale', term: POSTURE_TERMS.securityRationale, words: c.rationale ? RATIONALE_LABEL[c.rationale] : 'not stated' },
          { label: 'Main risk', words: secureRisk(c, d, names) },
          { label: 'Delivery mode', term: POSTURE_TERMS.deliveryMode, words: deliveryWords(d?.delivery) ?? (detail ? 'not stated' : '…') },
          { label: 'Recommended treatment', words: secureTreatment(d, core.design, c.variant) ?? (detail ? 'not stated' : '…') },
        ];
      case 'preserve':
        return [
          { label: 'Competitive Position', term: POSTURE_TERMS.competitivePosition, words: pct(c.cp) },
          { label: 'Industrial prize', words: pct(c.prize) },
          { label: 'Policy alignment', words: c.alignment ? ALIGNMENT_LABEL[c.alignment] : NA },
        ];
      case 'catch_up': {
        return [
          { label: 'Opportunity', term: POSTURE_TERMS.industrialOpportunity, words: pct(c.prize) },
          // from the cell: not measured, a top-quarter score that did not pass the Preserve test, or the gap below the bar
          { label: 'Position gap', words: catchUpPositionWords(c, t, 'gap') },
          { label: 'Credible pathway', term: POSTURE_TERMS.pathway, words: c.pathway ? PATHWAY_LABEL[c.pathway] : NA },
          { label: 'Pathway evidence', words: d?.pathway.basis.length ? (pathwayEvidenceWords(d.pathway.basis) ?? 'not stated') : detail ? 'not stated' : '…' },
        ];
      }
      case 'partner': {
        const a = allied.get(keyOf(c));
        const partners = a?.partners.length ? a.partners.map((p) => p.name).join(', ') : a?.noSolution ? 'none: no credible allied solution yet' : 'not stated';
        return [
          { label: 'Need', term: POSTURE_TERMS.supplyChainNeed, words: pct(c.need) },
          { label: 'Domestic case', words: domesticCase(c, t) },
          { label: 'Recommended partners', words: partners },
          { label: 'Diversification value', words: d ? (diversificationWords(d.partners) ?? 'not measured') : '…' },
          { label: 'AIB status', term: POSTURE_TERMS.partnerGap, words: aibStatusWords(a) },
        ];
      }
      case 'accelerate': {
        const dn = d?.deployment;
        const m = manufacturingPriority(c.tech, cells);
        return [
          { label: 'Deployment need', term: POSTURE_TERMS.deploymentNeed, words: pct(c.need) },
          { label: 'Demand need', words: dn ? familyWords(dn.demand, scen) : '…' },
          { label: 'Climate / target gap', words: dn ? familyWords(dn.policyGap, scen) : '…' },
          { label: 'System requirement', words: dn ? familyWords(dn.system, scen) : '…' },
          { label: 'Current deployment trend', words: dn ? trendWords(dn.trend) : '…' },
          { label: 'Deployment priority', words: 'high' },
          { label: 'Domestic manufacturing priority', words: `${manufacturingPriorityWord(m)} (${m.words})` },
        ];
      }
      case 'watch': {
        const why = watchReasons(c, t, d);
        return [{ label: 'Domestic industrial priority', words: `low${why.length ? `: ${why.join('; ').toLowerCase()}` : ''}` }];
      }
    }
  };

  const rows: ChoiceRow[] = [
    ...VERB_ORDER.flatMap((v) =>
      g[v].map((c) => ({
        key: keyOf(c), group: VERB_LONG[v], tech: c.tech, stage: c.stage, choice: choiceWord(c),
        detail: fields(v, c).map((f) => `${f.label}: ${f.words}`).join('; '),
      })),
    ),
    ...g.notAssessed.map((c) => ({ key: keyOf(c), group: 'Not assessed', tech: c.tech, stage: c.stage, choice: 'Not assessed', detail: det(c)?.notAssessedReason ?? 'need or prize not measured' })),
  ];
  const counts = VERB_ORDER.map((v) => `${g[v].length} ${VERB_LONG[v]}`).join(', ');
  const summary = `${name}: ${counts}; ${g.review.length} for consistency review; ${g.notAssessed.length} not assessed.`;
  const prov = postureProvenance(core);

  return (
    <ChartCard
      className={className}
      headingLevel={headingLevel}
      title={title}
      badge={showBasis ? <PostureBasisTag note={null} /> : undefined}
      sub="Every assessed node under its choice: what to secure, preserve, catch up in, rely on partners for, deploy faster, and deliberately not prioritise."
      summary={summary}
      table={() => <TableView rows={rows} cols={COLS} caption={`${name}: strategic choices by node`} rowKey={(r) => r.key} onRowClick={(r) => onOpen(r.key)} selectedKey={selected} pageSize={70} />}
      csv={() => ({
        name: `${name} strategic choices`,
        text: postureCsvText(
          rows.map((r) => ({ group: r.group, technology: r.tech, stage: r.stage, stage_name: stageDisplay(r.stage), choice: r.choice, why: r.detail })),
          ['group', 'technology', 'stage', 'stage_name', 'choice', 'why'],
          prov,
        ),
      })}
      notes={<p>Select a node for its five questions. Deploy · Watch is a deliberate conclusion: the node does not call for domestic industrial intervention.</p>}
    >
      <div className="grid gap-3 md:grid-cols-2" data-testid="strategic-choices">
        {VERB_ORDER.map((v) => {
          const list = g[v];
          const all = open[v] ?? false;
          const shown = all ? list : list.slice(0, limit);
          return (
            <section key={v} className="rounded-lg border border-white/10 bg-ink-900/40 p-3" data-testid="choice-group" data-verb={v} aria-label={`${VERB_LONG[v]}: ${list.length}`}>
              <h4 className="flex items-center gap-2 text-sm font-medium text-paper">
                <Swatch color={POSTURE_COLOR[VERB_FAMILY[v]]} mark={v === 'secure' ? undefined : cellMark(MARK_OF[v])} size={16} />
                {VERB_LONG[v]}
                <span className="tabular text-paper-mute">{list.length}</span>
              </h4>
              <p className="mb-2 text-xs text-paper-mute">{VERB_SUBTITLE[v]}</p>
              {list.length === 0 ? (
                <p className="text-xs text-paper-mute">No node.</p>
              ) : (
                <ul className="space-y-2">
                  {shown.map((c) => {
                    const k = keyOf(c);
                    const href = v === 'partner' && aibHref ? aibHref(c.tech, c.stage) : null;
                    return (
                      <li key={k} className="rounded-md border border-white/5 p-2" data-key={k}>
                        <button
                          type="button"
                          className="w-full text-left text-sm text-paper outline-none hover:underline focus-visible:ring-2 focus-visible:ring-signal"
                          aria-pressed={selected === k}
                          onClick={() => onOpen(k)}
                        >
                          {c.tech} · {stageDisplay(c.stage)}
                          {c.flag ? <span className="ml-1 text-xs text-signal">(review)</span> : null}
                        </button>
                        <dl className="mt-1 space-y-0.5">
                          {fields(v, c).map((f) => (
                            <Field key={f.label} f={f} />
                          ))}
                        </dl>
                        {href && (
                          <p className="mt-1 text-xs">
                            <Link className="link" to={href} data-testid="choice-aib-link">
                              AIB partner recommendations &rarr;
                            </Link>
                          </p>
                        )}
                      </li>
                    );
                  })}
                </ul>
              )}
              {list.length > limit && (
                <button type="button" className="btn-quiet mt-2 text-xs" aria-expanded={all} onClick={() => setOpen((o) => ({ ...o, [v]: !all }))}>
                  {all ? `Show the first ${limit}` : `Show all ${list.length}`}
                </button>
              )}
            </section>
          );
        })}
      </div>
      <div className="mt-3 grid gap-3 md:grid-cols-2">
        <section className="rounded-lg border border-white/10 p-3" data-testid="choice-review" aria-label={`Consistency review: ${g.review.length}`}>
          <h4 className="text-sm font-medium text-paper">
            <Term id={POSTURE_TERMS.consistencyFlag}>Consistency review</Term> <span className="tabular text-paper-mute">{g.review.length}</span>
          </h4>
          {g.review.length ? (
            <ul className="mt-1 space-y-1 text-xs text-paper-dim">
              {g.review.map((c) => (
                <li key={keyOf(c)}>
                  <button type="button" className="text-left text-paper hover:underline" onClick={() => onOpen(keyOf(c))} aria-pressed={selected === keyOf(c)}>
                    {c.tech} · {stageDisplay(c.stage)}
                  </button>{' '}
                  ({choiceWord(c)}): {flagWords(c.flag)}
                </li>
              ))}
            </ul>
          ) : (
            <p className="mt-1 text-xs text-paper-mute">No node is flagged for review.</p>
          )}
        </section>
        <section className="rounded-lg border border-dashed border-white/15 p-3" data-testid="choice-not-assessed" aria-label={`Not assessed: ${g.notAssessed.length}`}>
          <h4 className="text-sm font-medium text-paper">
            Not assessed <span className="tabular text-paper-mute">{g.notAssessed.length}</span>
          </h4>
          <p className="mt-1 text-xs text-paper-mute">
            {g.notAssessed.length ? 'Need or prize is not measured, so no choice is made (never Watch):' : 'Every node has the need and prize the choice reads.'}
          </p>
          {g.notAssessed.length > 0 && (
            <ul className="mt-1 flex flex-wrap gap-x-3 gap-y-1 text-xs">
              {g.notAssessed.map((c) => (
                <li key={keyOf(c)}>
                  <button type="button" className="text-left text-paper-dim hover:underline" onClick={() => onOpen(keyOf(c))} aria-pressed={selected === keyOf(c)}>
                    {c.tech} · {stageDisplay(c.stage)}
                  </button>
                </li>
              ))}
            </ul>
          )}
        </section>
      </div>
    </ChartCard>
  );
}
