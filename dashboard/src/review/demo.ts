// Synthetic demo submissions for the admin dashboard and its tests (public/data/review/fixtures/
// demo_submissions.json; docs/QC_SYSTEM_DESIGN.md 7). Deterministic from a seed; every reviewer id starts
// 'DEMO-', every name is null, every free-text field starts '[synthetic]'. They exercise each summary:
// agreement, persuasion, rejection, outside-expertise answers, a too-few case, a critical data issue with
// cosmetic noise, method feedback, framework mismatches, decision use and threat reviews.
// THESE ARE NOT EXPERT JUDGEMENTS and must never be quoted as such. Pure: no IO, no clock, no Math.random.

import {
  DONT_KNOW,
  SUBMISSION_SCHEMA,
  SUBMISSION_SCHEMA_VERSION,
  type FrameworkScenario,
  type MethodCard,
  type PageContext,
  type PayloadByKind,
  type ReviewCase,
  type ReviewerStamp,
  type Submission,
  type SubmissionKind,
  type Verdict,
} from './model.ts';
import { hashString, seededRandom } from './assign.ts';
import { canonicalJson, sha256HexSync } from './privacy.ts';
import { caseTags } from './tags.ts';
import { capturePageContext } from './context.ts';

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
    const opts = c.answer_options.map((o) => o.value).filter((v) => v !== avoid && v !== DONT_KNOW);
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

  return out;
}
