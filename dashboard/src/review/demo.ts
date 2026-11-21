// Synthetic demo submissions for the admin dashboard and its tests (public/data/review/fixtures/
// demo_submissions.json; docs/QC_SYSTEM_DESIGN.md 7). Deterministic from a seed; every reviewer id starts
// 'DEMO-', every name is null, every free-text field starts '[synthetic]'. They exercise each summary:
// agreement, persuasion, rejection, outside-expertise answers, a too-few case, a critical data issue with
// cosmetic noise, method feedback, framework mismatches, decision use and threat reviews.
// UX v2 rows (blind.ux_version 2) come from separate DEMO reviewers and sit beside the v1 rows, so mixed data is
// exercised: every verdict statement, a persuaded expert, insufficient-evidence answers with evidence gaps (twice
// on one case: a repeated inability to judge), none-of-these and outside-expertise answers, red-team answers,
// product answers, notes with conversions and both review modes.
// THESE ARE NOT EXPERT JUDGEMENTS and must never be quoted as such. Pure: no IO, no clock, no Math.random.

import {
  DONT_KNOW,
  INSUFFICIENT_EVIDENCE,
  NONE_OF_THESE,
  OUTSIDE_EXPERTISE,
  PRODUCT_QUESTIONS,
  REVIEWER_STATES,
  REVIEW_VERDICT_CODES,
  REVIEW_VERDICT_ISSUE_TYPE,
  REVIEW_VERDICT_LEGACY,
  SUBMISSION_SCHEMA,
  SUBMISSION_SCHEMA_VERSION,
  type FrameworkScenario,
  type MethodCard,
  type PageContext,
  type PayloadByKind,
  type ReviewCase,
  type ReviewerNote,
  type ReviewerStamp,
  type ReviewVerdictCode,
  type CaseReviewPayload,
  type Persuasion,
  type Validity,
  type Submission,
  type SubmissionKind,
  type Verdict,
} from './model.ts';
import { hashString, seededRandom } from './assign.ts';
import { canonicalJson, sha256HexSync } from './privacy.ts';
import { caseTags } from './tags.ts';
import { capturePageContext } from './context.ts';
import { explorerComparisonValue, explorerStageSet, productOf, type BlindAnswerV2 } from './flow.ts';

export const SYNTHETIC = '[synthetic]';
const BASE_TIME = Date.UTC(2027, 0, 20, 9, 0, 0);

interface DemoReviewer {
  stamp: ReviewerStamp;
}

function demoReviewer(i: number, sectors: string[], countries: string[], methods: string[]): DemoReviewer {
  const id = `DEMO-${String(i + 1).padStart(2, '0')}`;
  const confidence: Record<string, number> = {};
  for (const s of sectors) confidence[`sector:${s}`] = 4;
  for (const c of countries) confidence[`country:${c}`] = 4;
  for (const m of methods) confidence[`method:${m}`] = 4;
  const expertise = { sectors, countries, stages: [], methods, confidence };
  return { stamp: { reviewer_id: id, name: null, anonymous: true, profile_digest: sha256HexSync(canonicalJson({ reviewer_id: id, ...expertise })), expertise } };
}

/**
 * Deterministic synthetic submissions over the given cases, cards and scenarios. Needs at least one case for
 * the case sections; empty inputs simply give fewer submissions.
 */
export function makeDemoSubmissions(cases: readonly ReviewCase[], cards: readonly MethodCard[], scenarios: readonly FrameworkScenario[], seed: number | string, campaignId = 'demo'): Submission[] {
  const rand = seededRandom(typeof seed === 'number' ? seed >>> 0 : hashString(seed));
  let n = 0;
  const tick = () => new Date(BASE_TIME + (n += 1) * 7 * 60_000).toISOString();
  const uuid = () => {
    const h = sha256HexSync(`${String(seed)}|${n}|${rand()}`);
    return `${h.slice(0, 8)}-${h.slice(8, 12)}-4${h.slice(13, 16)}-a${h.slice(17, 20)}-${h.slice(20, 32)}`;
  };
  const out: Submission[] = [];
  const release = cases[0]?.data_release ?? null;
  const commit = cases[0]?.app_commit ?? 'unknown';
  const push = <K extends SubmissionKind>(r: DemoReviewer, kind: K, payload: PayloadByKind[K], page?: PageContext | null) => {
    const at = tick();
    out.push({
      submission_id: `SUB-DEMO-${uuid()}`,
      schema: SUBMISSION_SCHEMA,
      schema_version: SUBMISSION_SCHEMA_VERSION,
      campaign_id: campaignId,
      kind,
      reviewer: r.stamp,
      created_at: at,
      submitted_at: at,
      app_commit: commit,
      data_release: release,
      page_context: page ?? null,
      payload,
    } as Submission);
  };

  const first = cases[0] ? caseTags(cases[0]) : { sectors: [], countries: [], stage: null };
  const reviewers = Array.from({ length: 6 }, (_, i) =>
    demoReviewer(i, i < 4 ? first.sectors : ['solar'], i < 4 ? first.countries : ['IND'], i % 2 === 0 ? ['trade_data', 'indices_statistics'] : ['industrial_policy']),
  );

  const other = (c: ReviewCase, avoid: string): string => {
    const opts = c.answer_options.map((o) => o.value).filter((v) => v !== avoid && v !== DONT_KNOW && !REVIEWER_STATES.includes(v));
    return opts.length ? opts[Math.floor(rand() * opts.length)] : avoid;
  };
  const review = (r: DemoReviewer, c: ReviewCase, blind: string, verdict: Verdict, issue?: { type: 'data' | 'method' | 'context' | 'insufficient_evidence'; text: string }) => {
    const answered = tick();
    const revealed = tick();
    push(r, 'case_review', {
      case_id: c.case_id,
      case_snapshot_sha256: c.snapshot_sha256,
      assignment_reason: c.anchor ? 'anchor' : 'assigned',
      expertise_match: blind === DONT_KNOW || verdict === 7 ? 'outside' : 'in',
      blind: {
        choice: blind,
        confidence: blind === DONT_KNOW ? null : ((2 + Math.floor(rand() * 4)) as 2 | 3 | 4 | 5),
        factors: blind === DONT_KNOW ? [] : ['import_dependence', 'market'].slice(0, 1 + Math.floor(rand() * 2)),
        rationale: `${SYNTHETIC} demo rationale`,
        answered_at: answered,
      },
      reveal: {
        revealed_at: revealed,
        verdict,
        main_issue: issue ? { text: `${SYNTHETIC} ${issue.text}`, type: issue.type } : null,
        suggested_choice: verdict >= 4 && verdict <= 6 ? blind : null,
        evidence_wanted: verdict === 3 ? `${SYNTHETIC} production capacity by plant` : null,
      },
      outside_expertise: blind === DONT_KNOW || verdict === 7,
      time_spent_s: 300 + Math.floor(rand() * 600),
    });
  };

  // case 1 (an anchor where possible): agreement, persuasion, rejection, an outside answer -> summarisable
  const ordered = [...cases].sort((a, b) => Number(b.anchor) - Number(a.anchor));
  const c1 = ordered[0];
  if (c1) {
    const m = c1.snapshot.conclusion.value;
    review(reviewers[0], c1, m, 1);
    review(reviewers[1], c1, m, 1);
    review(reviewers[2], c1, other(c1, m), 2);
    review(reviewers[3], c1, other(c1, m), 5, { type: 'method', text: 'the threshold puts this cell on a knife-edge' });
    review(reviewers[4], c1, DONT_KNOW, 7);
  }
  // case 2: two reviews only -> "too few reviews to summarise"; one data-driven rejection
  const c2 = ordered[1];
  if (c2) {
    const m = c2.snapshot.conclusion.value;
    review(reviewers[0], c2, m, 1);
    review(reviewers[5], c2, other(c2, m), 4, { type: 'data', text: 'the trade share looks like re-exports, not production' });
  }
  // case 3: unconvinced
  const c3 = ordered[2];
  if (c3) review(reviewers[1], c3, other(c3, c3.snapshot.conclusion.value), 3, { type: 'insufficient_evidence', text: 'no capacity data behind the position score' });

  // data issues: one critical (corroborated) and several cosmetic
  if (c1) {
    // the page context as the app would capture it at the case's Explorer path
    const [pathname, search = ''] = c1.explorer_path.split('?');
    const captured = capturePageContext({ pathname, search }, {}, null, { captured_at: tick(), app_commit: commit, data_release: release, displayed_result: c1.snapshot.conclusion.label, metric_ids: ['home_share'], view: 'demo' });
    const page: PageContext = {
      ...captured,
      country: captured.country ?? c1.key.iso3 ?? null,
      tech: captured.tech ?? caseTags(c1).sectors[0] ?? null,
      stage: captured.stage ?? c1.key.stage ?? null,
      node: captured.node ?? c1.key.node ?? null,
      pair: captured.pair ?? c1.key.pair ?? null,
      sid: captured.sid ?? c1.key.sid ?? null,
      scenario: captured.scenario ?? c1.key.scenario ?? null,
    };
    const metric = { id: 'home_share', label: 'World supply share', value: 0.3, display: '30%', country: page.country, tech: page.tech, stage: page.stage, source: 'demo', source_year: 2024, transform: null };
    push(reviewers[0], 'data_issue', { metric, what_is_wrong: `${SYNTHETIC} the share counts re-exports as production`, corrected_value: `${SYNTHETIC} about 5%`, citation: `${SYNTHETIC} national statistics`, severity: 'critical' }, page);
    push(reviewers[1], 'break_model', { metric, what_is_wrong: `${SYNTHETIC} entrepot trade drives the result`, severity: 'critical', break_type: 'misleading_data' }, page);
    for (let i = 0; i < 4; i++)
      push(reviewers[(i + 2) % reviewers.length], 'data_issue', { metric: { ...metric, id: `label_${i}`, label: `Axis label ${i}` }, what_is_wrong: `${SYNTHETIC} typo in a label`, severity: 'cosmetic' }, { ...page, captured_at: tick() });
  }

  // method feedback on up to three cards: retain, modify, reject
  const stances = ['retain', 'modify', 'reject'] as const;
  cards.slice(0, 3).forEach((card, i) => {
    for (let j = 0; j < 2; j++)
      push(reviewers[(i + j) % reviewers.length], 'method_feedback', {
        card_id: card.card_id,
        stance: stances[(i + j) % 3],
        materiality: ((1 + ((i + j) % 4)) as 1 | 2 | 3 | 4),
        alternative: stances[(i + j) % 3] === 'retain' ? null : `${SYNTHETIC} use a continuous score instead of a cut-off`,
        affected_outputs: null,
        bias_direction: j === 0 ? 'favours_incumbents' : 'none',
        comment: `${SYNTHETIC} demo comment`,
        outside_expertise: false,
      });
  });

  // framework answers: the intended answer, a mismatch and a don't-know per scenario (first three)
  scenarios.slice(0, 3).forEach((sc, i) => {
    const wrong = sc.options.find((o) => o.value !== sc.intended_answer)?.value ?? sc.intended_answer;
    const answers = [sc.intended_answer, sc.intended_answer, wrong, DONT_KNOW];
    answers.forEach((answer, j) =>
      push(reviewers[(i + j) % reviewers.length], 'framework_answer', {
        scenario_id: sc.scenario_id,
        answer,
        intended_answer: sc.intended_answer,
        correct: answer === sc.intended_answer,
        confidence: answer === DONT_KNOW ? null : 3,
        confusing_terms: answer === wrong ? [sc.terms[0] ?? 'fence'] : [],
        comment: answer === wrong ? `${SYNTHETIC} the term reads two ways` : null,
      }),
    );
  });

  // decision use and threat reviews on flagged cases (else the first case)
  const du = cases.filter((c) => c.decision_use).slice(0, 2);
  for (const [i, c] of (du.length ? du : cases.slice(0, 1)).entries())
    push(reviewers[i % reviewers.length], 'decision_use', {
      case_id: c.case_id,
      would_do_differently: `${SYNTHETIC} prioritise offtake over subsidies`,
      actionable: i === 0 ? 2 : 3,
      identifies_problem: 3,
      separates_diagnosis_from_prescription: 2,
      right_geography: 4,
      evidence_required: `${SYNTHETIC} plant-level capacity`,
      improvement: null,
      decision_use: `${SYNTHETIC} ministry briefing`,
    });
  const th = cases.filter((c) => c.threat).slice(0, 2);
  for (const [i, c] of (th.length ? th : cases.slice(0, 1)).entries())
    push(reviewers[(i + 1) % reviewers.length], 'threat_review', {
      case_id: c.case_id,
      threat_snapshot_sha256: sha256HexSync(canonicalJson(c.snapshot.threat ?? null)),
      reduces_threat: 3,
      scale_sufficient: 2,
      timing_plausible: 2,
      commercially_plausible: 3,
      missing_constraints: ['qualification', 'financing'],
      success_looks_like: `${SYNTHETIC} two qualified non-dominant suppliers by 2030`,
      stop_point: `${SYNTHETIC} no offtake signed within two years`,
    });

  pushV2Reviews(cases, ordered, first, push, tick, rand);
  return out;
}

// ---- UX v2 synthetic reviews ----------------------------------------------------------------------------------

type Push = <K extends SubmissionKind>(r: DemoReviewer, kind: K, payload: PayloadByKind[K], page?: PageContext | null) => void;

/** How each statement is demonstrated: the blind answer relative to the Explorer, validity, persuasion, confidence. */
const V2_PLAN: Readonly<Record<ReviewVerdictCode, { blind: 'model' | 'other' | 'outside'; validity: Validity | null; persuasion: Persuasion | null; low?: boolean }>> = {
  same_same: { blind: 'model', validity: 'supported', persuasion: 'unchanged' },
  same_different_reasons: { blind: 'model', validity: 'partly', persuasion: 'unchanged' },
  persuaded: { blind: 'other', validity: 'supported', persuasion: 'changed' },
  plausible_insufficient: { blind: 'other', validity: 'partly', persuasion: 'unchanged', low: true },
  wrong_fact: { blind: 'other', validity: 'not_supported', persuasion: 'unchanged' },
  missing_factor: { blind: 'other', validity: 'not_supported', persuasion: 'somewhat' },
  wrong_weighting: { blind: 'other', validity: 'partly', persuasion: 'unchanged' },
  wrong_rule: { blind: 'other', validity: 'not_supported', persuasion: 'unchanged' },
  too_broad: { blind: 'other', validity: 'partly', persuasion: 'unchanged', low: true },
  outside_expertise: { blind: 'outside', validity: null, persuasion: null },
};

function pushV2Reviews(
  cases: readonly ReviewCase[],
  ordered: readonly ReviewCase[],
  first: { sectors: string[]; countries: string[] },
  push: Push,
  tick: () => string,
  rand: () => number,
): void {
  if (!cases.length) return;
  // spread over one case per product where the campaign has one, away from the v1 anchor rows where possible
  const pool = ordered.length > 2 ? ordered.slice(2) : [...ordered];
  const byProduct = (['country', 'sector', 'partnership', 'compact'] as const).map((p) => pool.find((c) => productOf(c.kind) === p)).filter((c): c is ReviewCase => !!c);
  const targets = byProduct.length ? byProduct : [pool[0]];
  const reviewers = Array.from({ length: 14 }, (_, i) => demoReviewer(6 + i, first.sectors, first.countries, i % 2 === 0 ? ['trade_data'] : ['industrial_policy']));

  const canonical = (c: ReviewCase) => c.answer_options.map((o) => o.value).filter((v) => v !== DONT_KNOW && !REVIEWER_STATES.includes(v));
  const modelOf = (c: ReviewCase) => explorerComparisonValue(c.kind, c.snapshot) ?? canonical(c)[0] ?? 'watch';
  const otherThan = (c: ReviewCase, avoid: string) => {
    const opts = canonical(c).filter((v) => v !== avoid);
    return opts.length ? opts[Math.floor(rand() * opts.length)] : avoid;
  };
  const productAnswers = (c: ReviewCase, k: number): Record<string, string> | undefined => {
    const qs = PRODUCT_QUESTIONS[productOf(c.kind)];
    if (!qs.length) return undefined;
    return Object.fromEntries(qs.map((q, j) => [q.id, q.options[(k + j) % q.options.length].value]));
  };
  const notes = (k: number, answered: string, revealed: string): ReviewerNote[] => {
    const conv = (['data_issue', 'missing_context', 'method_concern', 'comment', null] as const)[k % 5];
    return [
      { id: `note-${k}-a`, text: `${SYNTHETIC} checked the trade year before answering`, created_at: answered, updated_at: answered, converted_to: null },
      { id: `note-${k}-b`, text: `${SYNTHETIC} the reveal leans on one supplier share`, created_at: revealed, updated_at: revealed, converted_to: conv },
    ];
  };

  const caseReview = (r: DemoReviewer, c: ReviewCase, k: number, code: ReviewVerdictCode, blindChoice: string, extra: { blind?: Partial<BlindAnswerV2>; low?: boolean; validity: Validity | null; persuasion: Persuasion | null }) => {
    const answered = tick();
    const revealed = tick();
    const model = modelOf(c);
    const state = REVIEWER_STATES.includes(blindChoice);
    const level = state && blindChoice !== NONE_OF_THESE ? null : extra.low ? 'low' : k % 2 ? 'high' : 'medium';
    const blind: BlindAnswerV2 = {
      choice: blindChoice,
      confidence: level === 'low' ? 1 : level === 'medium' ? 3 : level === 'high' ? 5 : null,
      factors: level ? ['allied_alternatives'] : [],
      rationale: `${SYNTHETIC} demo rationale`,
      answered_at: answered,
      confidence_level: level,
      driver: level ? (['concentration_security', 'allied_alternatives', 'domestic_capability', 'demand_growth', 'other'] as const)[k % 5] : null,
      driver_text: level && k % 5 === 4 ? `${SYNTHETIC} stockpile policy` : null,
      ux_version: 2,
      ...extra.blind,
    };
    if (level) blind.factors = [blind.driver as string];
    if (c.kind === 'sector_chain' && !state) blind.choices = [...new Set([blindChoice, ...(explorerStageSet(c.kind, c.snapshot) ?? []).slice(0, 1)])];
    const needsIssue = code === 'same_different_reasons' || (REVIEW_VERDICT_ISSUE_TYPE[code] !== undefined);
    const disagree = REVIEW_VERDICT_ISSUE_TYPE[code] !== undefined;
    const payload: CaseReviewPayload = {
      case_id: c.case_id,
      case_snapshot_sha256: c.snapshot_sha256,
      assignment_reason: c.anchor ? 'anchor' : 'assigned',
      expertise_match: code === 'outside_expertise' ? 'outside' : 'in',
      blind,
      reveal: {
        revealed_at: revealed,
        verdict: REVIEW_VERDICT_LEGACY[code],
        main_issue: needsIssue ? { text: `${SYNTHETIC} demo issue for ${code}`, type: REVIEW_VERDICT_ISSUE_TYPE[code] ?? 'method' } : null,
        suggested_choice: disagree && !state ? blindChoice : null,
        evidence_wanted: null,
        verdict_code: code,
        prior_differs: state ? null : blindChoice !== model,
        validity: extra.validity,
        persuasion: extra.persuasion,
        red_team:
          disagree || extra.low
            ? { must_be_true: `${SYNTHETIC} trusted suppliers can qualify within three years`, resolving_evidence: (['production_capacity', 'qualification_constraints', 'allied_spare_capacity', 'cost_competitiveness'] as const)[k % 4], resolving_evidence_text: k % 2 ? `${SYNTHETIC} plant-level capacity` : null }
            : null,
        ...(productAnswers(c, k) ? { product_answers: productAnswers(c, k) } : {}),
      },
      outside_expertise: blindChoice === OUTSIDE_EXPERTISE || code === 'outside_expertise',
      time_spent_s: 300 + Math.floor(rand() * 600),
      notes: k % 3 === 0 ? [] : notes(k, answered, revealed),
      review_mode: k % 4 === 3 ? 'technical' : 'decision',
    };
    push(r, 'case_review', payload);
  };

  // every statement once
  REVIEW_VERDICT_CODES.forEach((code, k) => {
    const c = targets[k % targets.length];
    const plan = V2_PLAN[code];
    const model = modelOf(c);
    const choice = plan.blind === 'model' ? model : plan.blind === 'outside' ? OUTSIDE_EXPERTISE : otherThan(c, model);
    caseReview(reviewers[k], c, k, code, choice, { low: plan.low, validity: plan.validity, persuasion: plan.persuasion });
  });
  // insufficient evidence twice on the first target (a repeated inability to judge), with evidence gaps
  const c0 = targets[0];
  caseReview(reviewers[10], c0, 10, 'plausible_insufficient', INSUFFICIENT_EVIDENCE, {
    blind: { missing_evidence: ['production_capacity', 'firm_capability'], missing_evidence_text: `${SYNTHETIC} no plant list for this stage` },
    validity: 'unsure',
    persuasion: 'unchanged',
  });
  caseReview(reviewers[11], c0, 11, 'plausible_insufficient', INSUFFICIENT_EVIDENCE, { blind: { missing_evidence: ['production_capacity', 'project_pipeline'] }, validity: 'partly', persuasion: 'unchanged' });
  // none of these, with the reviewer's own recommendation
  caseReview(reviewers[12], targets[targets.length - 1], 12, 'missing_factor', NONE_OF_THESE, {
    blind: { own_recommendation: `${SYNTHETIC} a shared stockpile rather than new capacity` },
    validity: 'not_supported',
    persuasion: 'unchanged',
  });
  // a second persuaded expert on another target, so the highlighted group spans cases where it can
  const c1 = targets[1 % targets.length];
  caseReview(reviewers[13], c1, 13, 'persuaded', otherThan(c1, modelOf(c1)), { validity: 'supported', persuasion: 'changed' });
}
