// "What should [COUNTRY] do?" (external review readiness, 2026-11-23): the answer first, at the top of the
// Strategy section, above the Country Strategy Map. Built only from what the page already loaded (the posture
// core's cells and meta, the economy's detail document): the top three steps of each choice from
// posture/sections.ts choiceGroups (Secure, Preserve, Catch Up, Partner, Accelerate, in VERB_ORDER), each named
// by its phrase (strategy.ts nodePhrase) with the one field that explains it (fences.ts fenceReason; Partner's
// lead from the trusted network only, partnerEligibility.ts). Flagged or low-confidence steps are never listed
// as an answer; they are counted. Watch is one line, and Not assessed is its own count, never merged into Watch.
// Each step opens the page's one drawer (?node=) and has a collapsed "Why this result?" read from the shipped
// decision trace (posture/whyChoice.ts). One muted note says what the strategic case does not cover.
// Explorer usability pass (2026-11-25): each "Why this result?" opens with the step's confidence in words
// (posture/confidence.ts); one deployment row (Deployment & Use cells: Accelerate, its cases, the deployment
// policy gap; costs are not in the strategy, so it links to world prices instead of asserting anything); and a
// compact footer (posture/EvidenceSummary.tsx): evidence quality over the listed steps, "Why this conclusion?"
// with the inputs and data periods, and links to the evidence.

import { useMemo } from 'react';
import { Link } from 'react-router';
import { CHOICE_ACTION, CHOICE_NAME } from '../../../content/vocabulary.ts';
import { stageDisplay } from '../../../lib/stages.ts';
import { choiceGroups, countryNames, detailByKey, fenceReason, nodePhrase, Swatch, verbOf, type FenceNode, type PostureCell, type Verb } from '../../../posture/index.ts';
import { networkOf, partnerLead, partnerLeadWords } from '../../../posture/partnerEligibility.ts';
import { TrustedNetworkControl } from '../../../posture/TrustedNetworkControl.tsx';
import { whyChoice } from '../../../posture/whyChoice.ts';
import { confidenceSentence, confidenceSummary } from '../../../posture/confidence.ts';
import { EvidenceSummary } from '../../../posture/EvidenceSummary.tsx';
import { conclusionInputs, deploymentLine, deploymentSummary, evidenceQualityLine } from '../../../posture/evidenceSummary.ts';
import { freshnessItems } from '../../derive/strategyPage.ts';
import { listAnd, plural } from '../../../posture/derive.ts';
import { cap, proseName } from '../../derive/text.ts';
import { useProfile } from './common.tsx';

/** The choices the block lists, in the page's order (Watch is a line of its own). */
export const BOTTOM_LINE_VERBS: readonly Exclude<Verb, 'watch'>[] = ['secure', 'preserve', 'catch_up', 'partner', 'accelerate'];
const MEDIUM: readonly Verb[] = ['preserve', 'catch_up', 'partner', 'accelerate'];
export const BOTTOM_LINE_TOP = 3;

/** The fence section that lists every step of a choice. */
const SECTION_OF: Record<Exclude<Verb, 'watch'>, string> = {
  secure: '#high-fence', preserve: '#medium-fence', catch_up: '#medium-fence', partner: '#medium-partner', accelerate: '#medium-fence',
};

export const FEASIBILITY_CAVEAT =
  'Strategic case, not an investment recommendation. The Explorer identifies where capability may be strategically valuable; it does not estimate fiscal cost, consumer-price premium, policy durability or the odds that intervention succeeds.';
export const FEASIBILITY_NOT_ASSESSED = 'Feasibility evidence: Not assessed.';
/** The deployment row's close: cost evidence is not in the strategy, so the row points to the prices the Explorer has. */
export const PRICES_NOTE = 'Costs and prices do not enter these choices.';
export const pricesHref = (iso3: string): string => `/sector/explore/prices?c=${iso3}`;

/** The steps the block lists (top of each choice, held-back steps left out) and how many it holds back. */
export function listedSteps(groups: Record<Exclude<Verb, 'watch'>, PostureCell[]>): { listed: PostureCell[]; held: number } {
  const listed: PostureCell[] = [];
  let held = 0;
  for (const v of BOTTOM_LINE_VERBS) {
    const ok = groups[v].filter((c) => !heldBack(c));
    held += groups[v].length - ok.length;
    listed.push(...ok.slice(0, BOTTOM_LINE_TOP));
  }
  return { listed, held };
}

/** A step that should not be offered as an answer: a consistency flag or low confidence. */
export const heldBack = (c: Pick<PostureCell, 'flag' | 'confidence'>): boolean => c.flag !== null || c.confidence === 'low';

const [KEEP_OPEN, KEEP_OPEN_HOW] = CHOICE_ACTION.watch.split(': ');

/** "Keep open: 22 steps (buy competitively, do not force localization)" */
export const keepOpenLine = (n: number): string => `${KEEP_OPEN}: ${plural(n, 'step')} (${KEEP_OPEN_HOW})`;

/** The 1-2 sentence reading of the counts: the fences' sizes, then where the medium-fence choices concentrate. */
export function bottomLineSynthesis(cells: readonly PostureCell[]): string | null {
  const verbs = cells.map(verbOf);
  const assessed = verbs.filter((v) => v !== null).length;
  if (!assessed) return null;
  const high = verbs.filter((v) => v === 'secure').length;
  const mediumCells = cells.filter((c) => { const v = verbOf(c); return v !== null && MEDIUM.includes(v); });
  const low = verbs.filter((v) => v === 'watch').length;
  const first = `Of ${plural(assessed, 'assessed step')}, ${high} ${high === 1 ? 'needs' : 'need'} a high fence (Secure), ${mediumCells.length} a medium fence (Preserve, Catch Up, Partner or Accelerate) and ${low} can stay open (Watch).`;
  if (mediumCells.length < 3) return first;
  const top = (key: (c: PostureCell) => string, words: (k: string) => string): { names: string; n: number } | null => {
    const by = new Map<string, number>();
    for (const c of mediumCells) by.set(key(c), (by.get(key(c)) ?? 0) + 1);
    const max = Math.max(...by.values());
    const lead = [...by.entries()].filter(([, n]) => n === max).map(([k]) => words(k));
    return max >= 2 && lead.length <= 2 ? { names: listAnd(lead), n: max } : null;
  };
  const tech = top((c) => c.tech, (k) => k);
  const stage = top((c) => c.stage, stageDisplay);
  const parts = [tech ? `in ${tech.names} (${tech.n} of ${mediumCells.length}${tech.names.includes(' and ') ? ' each' : ''})` : null, stage ? `at ${stage.names} (${stage.n}${stage.names.includes(' and ') ? ' each' : ''})` : null].filter(Boolean);
  return parts.length ? `${first} Medium-fence choices are most common ${parts.join(' and ')}.` : first;
}

export function BottomLine() {
  const { sheet, posture: p, aib } = useProfile();
  const name = proseName(sheet.meta.country);
  const cells = p?.cells ?? [];
  const core = p?.core ?? null;
  const groups = useMemo(() => choiceGroups(cells), [cells]);
  const details = useMemo(() => detailByKey(p?.detail ?? null), [p?.detail]);
  const names = useMemo(() => ({ ...(aib?.names ?? {}), ...(core ? countryNames(core) : {}) }), [aib?.names, core]);
  const net = useMemo(() => (core ? networkOf(core.countries) : null), [core]);
  if (!p || p.status !== 'ready' || !core || !cells.length || !net) return null;
  const meta = core.meta;
  const synthesis = bottomLineSynthesis(cells);
  const { listed, held: heldAll } = listedSteps(groups);
  const dep = deploymentSummary(cells);
  const data = freshnessItems({ created: core.created, benchmark: meta.benchmark_period, momentumThrough: meta.momentum_through, asOfLabel: sheet.meta.as_of_label });
  const shown = BOTTOM_LINE_VERBS.filter((v) => groups[v].length);
  const hasMedium = MEDIUM.some((v) => groups[v].length > 0);
  const reasonOf = (c: PostureCell, verb: Verb): string | null => {
    const key = `${c.tech}|${c.stage}`;
    const d = details.get(key) ?? null;
    if (verb === 'partner') return partnerLeadWords(partnerLead(d?.partners ?? null, c.partner1, !!(c.partnerGap || d?.partnerGap), net), names);
    const n: FenceNode = { key, nodeKey: key, tech: c.tech, stage: c.stage, verb, cell: c, detail: d, layer: null };
    return fenceReason(n, core, names);
  };
  return (
    <section className="card mb-6 p-4 sm:p-5" aria-labelledby="bottom-line-h" data-testid="bottom-line">
      <h3 id="bottom-line-h" className="text-lg font-medium text-paper">
        What should {name} do?
      </h3>
      {synthesis && (
        <p className="mt-1 max-w-3xl text-sm text-paper-dim" data-testid="bottom-line-synthesis">
          {synthesis}
        </p>
      )}
      <div className="mt-3 grid gap-x-6 gap-y-4 md:grid-cols-2">
        {shown.map((verb) => {
          const all = groups[verb];
          const ok = all.filter((c) => !heldBack(c));
          const list = ok.slice(0, BOTTOM_LINE_TOP);
          const held = all.length - ok.length;
          const more = ok.length - list.length;
          return (
            <div key={verb} data-verb={verb} data-testid={`bottom-line-${verb}`}>
              <p className="flex items-center gap-2 text-sm">
                <Swatch verb={verb} size={12} />
                <span className="font-medium text-paper">{CHOICE_NAME[verb]}</span>
                <span className="text-paper-mute">
                  · {plural(all.length, 'step')} · {CHOICE_ACTION[verb]}
                </span>
              </p>
              {list.length > 0 && (
                <ol className="mt-1 space-y-1">
                  {list.map((c) => {
                    const key = `${c.tech}|${c.stage}`;
                    const why = reasonOf(c, verb);
                    const d = details.get(key);
                    const trace = d ? whyChoice(d, meta) : null;
                    const phrase = cap(nodePhrase(meta, c.tech, c.stage));
                    return (
                      <li key={key} data-key={key}>
                        <button
                          type="button"
                          className="w-full rounded-md px-1.5 py-1 text-left outline-none hover:bg-white/5 focus-visible:ring-2 focus-visible:ring-signal"
                          aria-pressed={p.node.selected === key}
                          onClick={() => p.node.select(key)}
                        >
                          <span className="block text-sm text-paper">{phrase}</span>
                          {why && (
                            <span className="block text-xs text-paper-dim" data-testid="bottom-line-why">
                              {why}
                            </span>
                          )}
                        </button>
                        <details className="ml-1.5 text-xs text-paper-dim" data-testid="why-this-result">
                          <summary className="cursor-pointer select-none text-paper-mute">Why this result?</summary>
                          {trace && <p className="mt-1 max-w-prose">{trace.because}</p>}
                          {trace?.whyNot && (
                            <p className="mt-1 max-w-prose">
                              <span className="text-paper">Why not {trace.whyNot.alt}?</span> {trace.whyNot.text}
                            </p>
                          )}
                          <p className="mt-1 max-w-prose text-paper-mute" data-testid="step-confidence">
                            {confidenceSentence(confidenceSummary(c))}
                          </p>
                        </details>
                      </li>
                    );
                  })}
                </ol>
              )}
              {(more > 0 || held > 0) && (
                <p className="mt-1 pl-1.5 text-xs text-paper-mute">
                  {more > 0 && (
                    <a className="link" href={SECTION_OF[verb]}>
                      {plural(more, 'more step')}
                    </a>
                  )}
                  {more > 0 && held > 0 && ' · '}
                  {held > 0 && `${plural(held, 'step')} flagged for review or low confidence, not listed`}
                </p>
              )}
              {verb === 'partner' && <TrustedNetworkControl className="mt-2 pl-1.5" />}
            </div>
          );
        })}
      </div>
      {!groups.partner.length && <TrustedNetworkControl className="mt-3" />}
      <p className="mt-4 text-sm text-paper-dim" data-testid="bottom-line-watch">
        <span className="text-paper">{keepOpenLine(groups.watch.length)}</span>
        <span className="text-paper-mute"> · Not assessed: {groups.notAssessed.length}</span>
      </p>
      <p className="mt-2 max-w-3xl text-sm text-paper-dim" data-testid="bottom-line-deployment">
        <span className="text-paper">Affordability &amp; deployment:</span> {deploymentLine(dep)}{' '}
        <span className="text-paper-mute">
          {PRICES_NOTE}{' '}
          <Link className="link" to={pricesHref(sheet.iso3)}>
            World commodity prices &rarr;
          </Link>
        </span>
      </p>
      {hasMedium && (
        <p className="mt-3 max-w-3xl text-xs text-paper-mute" data-testid="bottom-line-feasibility">
          {FEASIBILITY_CAVEAT} {FEASIBILITY_NOT_ASSESSED}
        </p>
      )}
      <EvidenceSummary
        className="mt-3"
        quality={evidenceQualityLine(listed)}
        inputs={conclusionInputs(cells, meta, { held: heldAll, top: BOTTOM_LINE_TOP })}
        data={data}
        links={[
          { label: 'Explore the evidence', href: '#evidence' },
          { label: 'Policy alignment', href: '#policy-alignment' },
        ]}
      />
    </section>
  );
}
