// "Development partnerships" (2026-11-03; src/country/devpart.ts, scripts/93): the development arm of the Allied
// Industrial Base on the Country page, a sub-section of Allied strategy (#development-partnerships). It reads
// the economy's development-partnership file in its perspective:
//   donor      "Where could development cooperation build trusted supply this country needs?": the developing
//              partners whose same step is a credible Catch Up, ranked (no score: the donor reason, the
//              recipient's anchor, the relationship ties and the tool are columns), the commercial-route and
//              review cases apart, the steps no partner qualifies for, and current aid against the pairs;
//   recipient  "Where could development partnerships help this country Catch Up?": its Catch Up steps with
//              the test result, and the developed partners whose same step needs the capability.
// Development Potential stays in the Partner section's Development tab as a supporting indicator and never sets
// a pair here. A step opens the posture drawer; a partner links to its Country profile.

import { useMemo, useState, type ReactNode } from 'react';
import { Link } from 'react-router';
import { useAibScenario } from '../../../aib/data/hooks.ts';
import { TableView, type Col } from '../../../components/TableView.tsx';
import { Notice, Select } from '../../../components/ui.tsx';
import { CHOICE_NAME } from '../../../content/vocabulary.ts';
import { score100 } from '../../../posture/derive.ts';
import { useDevpartDoc } from '../../data/hooks.ts';
import {
  aidGap, aidWords, anchorWords, byRoute, devpartPairs, devpartRows, donorReasonWords, fmtUsdM, recipientDonorGroups, ROUTE_ORDER, tieWords, wordsOf, type DevpartDoc, type DevpartDonorStep, type DevpartPair, type DevpartRecipientStep, type DevpartRoute, aidStageWords, compactRoleWords, constraintWords, type DevpartPreserveStep,
} from '../../devpart.ts';
import { cap, poss, proseName } from '../../derive/text.ts';
import { Fold, Section, useProfile } from './common.tsx';

export const DEVPART_SECTION_ID = 'development-partnerships';

export const DEVPART_QUESTION = {
  donor: (name: string) => `Where could development cooperation build trusted supply ${name} needs?`,
  recipient: (name: string) => `Where could development partnerships help ${name} Catch Up?`,
} as const;

export const DEVPART_INTRO = {
  donor:
    'These partnerships pair a real supply-chain or allied-capacity need with a developing economy that has a credible pathway to build the capability. The aim is to strengthen the allied industrial base while creating productive capacity in the partner economy.',
  neutral:
    'Development partnerships pair a high-income economy\'s supply-chain need with a developing economy\'s credible pathway to build the same capability.',
  recipient:
    "These are the country's Catch Up steps with a credible pathway, and the developed economies whose own supply chains need the same capability. Development cooperation here is a route into productive capability, export markets, technology transfer and allied specialization, not aid for its own sake.",
} as const;

export const NO_DEVELOPMENT_SOLUTION = 'No credible development-led supply solution identified.';

const choiceName = (c: string | null): string => (c && c in CHOICE_NAME ? CHOICE_NAME[c as keyof typeof CHOICE_NAME] : c ? cap(c.replace(/_/g, ' ')) : '—');
const aibWords = (s: string | null): string => (s === 'gap' ? 'allied supply falls short' : s === 'covered' ? 'allied supply covers the need' : 'not assessed for this step');

/** An economy's name: a link to its Country profile when it has one (the AIB scenario toggle carried; as PartnersSection's PartnerName). */
function PartnerName({ iso3, name }: { iso3: string; name: string }) {
  const { manifest } = useProfile();
  const { href } = useAibScenario();
  const has = manifest.countries.some((e) => e.iso3 === iso3 && e.status !== 'no_data' && !!e.file);
  return has ? (
    <Link className="link text-paper" to={href(`/countries/${iso3}`)}>
      {name}
    </Link>
  ) : (
    <span className="text-paper">{name}</span>
  );
}

/** The step as a button that opens the economy's posture drawer at it (the donor's own step, or the recipient's). */
function StepButton({ tech, stage, label }: { tech: string; stage: string; label: string }) {
  const { posture } = useProfile();
  if (!posture) return <span className="text-paper">{label}</span>;
  return (
    <button type="button" className="link inline text-left" onClick={() => posture.node.select(`${tech}|${stage}`, null)}>
      {label}
    </button>
  );
}

/** "thin_anchor:2;trust:1" -> "; 2 a thin anchor ..., 1 outside the AIB trust screen". */
function failWords(fails: readonly string[], words: Record<string, string>): string {
  if (!fails.length) return '';
  return `; ${fails.map((f) => { const [k, n] = f.split(':'); return `${n ?? ''} ${words[k] ?? k.replace(/_/g, ' ')}`.trim(); }).join(', ')}`;
}

function SubCard({ id, title, children }: { id: string; title: string; children: ReactNode }) {
  return (
    <section className="card p-5" aria-labelledby={`${id}-h`} data-testid={id}>
      <h4 id={`${id}-h`} className="rule-neis-sm text-base font-medium text-paper">
        {title}
      </h4>
      <div className="mt-2 space-y-2 text-sm text-paper-dim">{children}</div>
    </section>
  );
}

// ---- donor ----------------------------------------------------------------------------------------------------

function donorCols(doc: DevpartDoc, route: DevpartRoute, nameOf: (iso: string) => string): Col<DevpartPair>[] {
  const w = doc.words;
  const short = (p: DevpartPair) => wordsOf(p.donor_signals.filter((s) => s !== 'partner'), w.signal_short ?? w.signal).join('; ');
  const own = cap(poss(proseName(doc.country)));
  return [
    { id: 'step', header: 'Capability / supply-chain step', value: (r) => r.capability, cell: (r) => <StepButton tech={r.technology} stage={r.stage} label={r.capability} />, minWidth: 120 },
    {
      id: 'reason', header: `${own} choice · need · security rationale`, minWidth: 170,
      value: (r) => `${choiceName(r.donor_choice)} · need ${score100(r.donor_need)}${short(r) ? ` · ${short(r)}` : ''}`,
      cell: (r) => <span title={donorReasonWords(r, w)}><span className="text-paper">{choiceName(r.donor_choice)}</span> · need {score100(r.donor_need)}{short(r) ? ` · ${short(r)}` : ''}</span>,
    },
    { id: 'partner', header: 'Developing partner', value: (r) => nameOf(r.recipient_iso3), cell: (r) => <PartnerName iso3={r.recipient_iso3} name={nameOf(r.recipient_iso3)} />, minWidth: 90 },
    {
      id: 'pchoice', header: 'Partner choice · Competitive Position', minWidth: 120,
      value: (r) => `${choiceName(r.recipient_choice)} · CP ${score100(r.recipient_competitive_position)}${r.recipient_income_group ? ` · ${r.recipient_income_group.toLowerCase()}` : ''}`,
    },
    { id: 'anchor', header: 'Catch Up anchor / pathway', value: (r) => anchorWords(r, w), minWidth: 180 },
    {
      id: 'aib', header: 'AIB supply status', minWidth: 120,
      value: (r) => `${aibWords(r.aib_gap_status)}${r.aib_build_nodes.length ? '; new capacity needed (BUILD)' : ''}`,
      cell: (r) => <span title={r.aib_build_nodes.length ? `BUILD: ${r.aib_build_nodes.join(', ')}` : undefined}>{aibWords(r.aib_gap_status)}{r.aib_build_nodes.length ? '; new capacity needed (BUILD)' : ''}</span>,
    },
    {
      id: 'aid', header: 'Existing aid relationship (relevant aid stage)', minWidth: 170,
      value: (r) => `${aidWords(r, w)}${aidStageWords(r)}`,
    },
    { id: 'tie', header: 'Investment / finance tie', value: (r) => tieWords(r, w), minWidth: 150 },
    {
      id: 'tool', header: route === 'review' ? 'Why it is a review case' : 'Recommended development intervention', minWidth: 220,
      value: (r) => (route === 'review' ? (r.donor_review ? `${own} own call needs review (low confidence or flagged)` : (w.fail[r.recipient_fail ?? ''] ?? r.recipient_fail ?? 'review')) : r.recommended_tool_family),
    },
    { id: 'ev', header: 'Evidence quality', value: (r) => r.evidence_quality, cell: (r) => <span title={r.caveats.join('; ')}>{r.evidence_quality}{r.caveats.length ? ` · ${r.caveats.length} caveat${r.caveats.length === 1 ? '' : 's'}` : ''}</span>, minWidth: 80 },
  ];
}

function useNames(): (iso: string) => string {
  const { manifest } = useProfile();
  return useMemo(() => {
    const m = new Map(manifest.countries.map((e) => [e.iso3, e.name] as const));
    return (iso: string) => m.get(iso) ?? iso;
  }, [manifest]);
}

function DonorView({ doc }: { doc: DevpartDoc }) {
  const nameOf = useNames();
  const name = proseName(doc.country);
  const pairs = useMemo(() => devpartPairs(doc), [doc]);
  const routes = useMemo(() => byRoute(pairs), [pairs]);
  const steps = useMemo(() => devpartRows<DevpartDonorStep>(doc.steps), [doc]);
  const [route, setRoute] = useState<DevpartRoute>(routes.development.length ? 'development' : routes.commercial.length ? 'commercial' : 'review');
  const [step, setStep] = useState('all');
  const shown = routes[route].filter((p) => step === 'all' || p.key === step);
  const stepOptions = useMemo(() => {
    const keys = [...new Set(routes[route].map((p) => p.key))];
    return [{ value: 'all', label: 'All steps' }, ...keys.map((k) => ({ value: k, label: routes[route].find((p) => p.key === k)?.capability ?? k }))];
  }, [routes, route]);
  const cols = useMemo(() => donorCols(doc, route, nameOf), [doc, route, nameOf]);
  const devSteps = new Set(routes.development.map((p) => p.key));
  // the donor's Partner and Secure steps (choice-based reasons, tiers 1-3; not a low-confidence call) with no development pair
  const none = steps.filter((s) => s.stage !== 'Deployment' && !devSteps.has(s.key) && (s.donor_tier ?? 9) <= 3 && s.conf !== 'low');
  const nPartners = new Set(routes.development.map((p) => p.recipient_iso3)).size;
  const gap = useMemo(() => aidGap(doc), [doc]);
  const w = doc.words;
  return (
    <div className="space-y-5">
      <p className="max-w-3xl text-sm text-paper-dim" data-testid="devpart-summary">
        {routes.development.length
          ? `${routes.development.length} development partnership${routes.development.length === 1 ? '' : 's'} with ${nPartners} developing partner${nPartners === 1 ? '' : 's'} across ${devSteps.size} supply-chain step${devSteps.size === 1 ? '' : 's'}; ${routes.commercial.length} more where the partner is already at commercial scale (trade, offtake and investment rather than aid); ${routes.review.length} review case${routes.review.length === 1 ? '' : 's'} with insufficient evidence.`
          : `${NO_DEVELOPMENT_SOLUTION} ${routes.commercial.length ? `${routes.commercial.length} partner pairs are already at commercial scale (trade, offtake and investment rather than aid).` : ''}`}
      </p>
      <div className="flex flex-wrap items-end gap-3">
        <div role="group" aria-label="Kind of pair" className="flex flex-wrap gap-1.5">
          {ROUTE_ORDER.map((r) => (
            <button key={r} type="button" className={`btn btn-sm ${route === r ? 'btn-primary' : 'btn-ghost'}`} aria-pressed={route === r} onClick={() => { setRoute(r); setStep('all'); }} disabled={!routes[r].length}>
              {w.route[r]} ({routes[r].length})
            </button>
          ))}
        </div>
        <Select label="Supply-chain step" value={step} options={stepOptions} onChange={setStep} />
      </div>
      <div data-testid={`devpart-table-${route}`}>
        <TableView rows={shown} cols={cols} caption={`${cap(poss(name))} development partnerships: ${w.route[route].toLowerCase()}`} rowKey={(r) => `${r.recipient_iso3}|${r.key}`} pageSize={10} wrapHeaders search={shown.length > 10} />
      </div>
      {(route === 'development' || !routes.development.length) && none.length > 0 && (
        <SubCard id="devpart-none" title="Needs with no development partner">
          <p>Partner and Secure steps of {name} for which no developing partner passes the tests (no threshold is relaxed to fill them):</p>
          <ul className="space-y-1">
            {none.map((s) => (
              <li key={s.key}>
                <StepButton tech={s.tech} stage={s.stage} label={s.step_name} /> <span className="text-xs text-paper-mute">({choiceName(s.choice)}; {wordsOf(s.signals, w.signal).join('; ')})</span>
                <span className="block text-xs text-paper-mute">
                  {s.n_considered ? `${NO_DEVELOPMENT_SOLUTION} Candidates considered: ${s.n_considered}${failWords(s.fails, w.fail)}${(s.commercial ?? []).length ? `; already on the commercial route: ${(s.commercial ?? []).join(', ')}` : ''}.` : `${NO_DEVELOPMENT_SOLUTION} No developing economy is Catch Up or Preserve in this step.`}
                </span>
              </li>
            ))}
          </ul>
        </SubCard>
      )}
      <SubCard id="devpart-aid-gap" title="What development cooperation does today, against the AIB strategy">
        {!gap.reporting ? (
          <p>{cap(name)} does not report purpose-coded aid to the OECD Creditor Reporting System, so current aid is missing, not zero.</p>
        ) : (
          <>
            <p className="text-xs text-paper-mute">
              OECD CRS official development assistance, {doc.meta.aid_years.join('-')}, {doc.meta.aid_flow}s, energy, industry and mining purpose codes. This compares aid only against
              the energy-industrial objective here; aid outside it is not judged.
            </p>
            {gap.groups.map((g) => (
              <div key={g.gap} data-testid={`devpart-gap-${g.gap}`}>
                <p className="text-paper">{w.aid_gap[g.gap] ?? g.gap}</p>
                <p className="text-xs">{[...new Set(g.rows.map((r) => `${nameOf(r.recipient_iso3)} (${r.capability})`))].slice(0, 8).join('; ')}{g.rows.length > 8 ? '…' : ''}</p>
              </div>
            ))}
            {gap.noAib.length > 0 && (
              <div data-testid="devpart-gap-no_aib">
                <p className="text-paper">{w.aid_gap.no_aib}</p>
                <p className="text-xs">{gap.noAib.map((r) => `${nameOf(r.recipient_iso3)} ${fmtUsdM(r.oda_all) ?? ''}`.trim()).join('; ')}</p>
              </div>
            )}
          </>
        )}
      </SubCard>
    </div>
  );
}

// ---- recipient -------------------------------------------------------------------------------------------------

function RecipientView({ doc }: { doc: DevpartDoc }) {
  const nameOf = useNames();
  const name = proseName(doc.country);
  const w = doc.words;
  const steps = useMemo(() => devpartRows<DevpartRecipientStep>(doc.steps), [doc]);
  const pairs = useMemo(() => devpartPairs(doc), [doc]);
  const ok = steps.filter((s) => s.recipient_ok);
  const [step, setStep] = useState(ok[0]?.key ?? '');
  const sel = steps.find((s) => s.key === step) ?? null;
  const groups = useMemo(() => recipientDonorGroups(pairs.filter((p) => p.key === step)), [pairs, step]);
  // its Preserve steps: already competitive, so trade, offtake and investment with the partners that need them
  const preserve = useMemo(() => devpartRows<DevpartPreserveStep>(doc.preserve_steps).filter((s) => s.n_commercial > 0 || s.n_review > 0), [doc]);
  const commercialOf = (key: string) => [...new Set(pairs.filter((p) => p.key === key && p.route === 'commercial' && p.recipient_choice === 'preserve').map((p) => nameOf(p.donor_iso3)))];
  const stepCols: Col<DevpartRecipientStep>[] = [
    { id: 'step', header: 'Catch Up capability', value: (r) => r.step_name, cell: (r) => <StepButton tech={r.tech} stage={r.stage} label={r.step_name} /> },
    { id: 'anchor', header: 'Capability anchor / pathway', value: (r) => anchorWords({ catch_up_anchor_type: r.anchors, pathway_signals: r.supports }, w), minWidth: 200 },
    { id: 'cons', header: 'What constrains scale', value: (r) => constraintWords(r.constraints, r.unobserved, w), minWidth: 200 },
    { id: 'result', header: 'Development case', value: (r) => (r.recipient_ok ? 'credible' : (w.fail[r.recipient_fail ?? ''] ?? r.recipient_fail ?? '—')), minWidth: 150 },
    { id: 'n', header: 'Developed partners', value: (r) => r.n_donors, num: true },
  ];
  const donorCols: Col<DevpartPair>[] = [
    { id: 'donor', header: 'Developed partner', value: (r) => nameOf(r.donor_iso3), cell: (r) => <PartnerName iso3={r.donor_iso3} name={nameOf(r.donor_iso3)} /> },
    { id: 'reason', header: 'Its own need', value: (r) => `${choiceName(r.donor_choice)}; ${donorReasonWords(r, w)}`, minWidth: 180 },
    {
      id: 'role', header: 'Demand anchor / compact role', minWidth: 160,
      value: (r) => [...(r.demand_anchor === 'allied' || r.demand_anchor === 'both' ? ['deploys the technology fast (Accelerate)'] : []), ...compactRoleWords(r.donor_compact_role, w, r.compact)].join('; ') || 'no role in the compact',
    },
    { id: 'aid', header: 'Existing aid / finance / investment', value: (r) => `${aidWords(r, w)}${aidStageWords(r)}; ${tieWords(r)}`, minWidth: 200 },
    { id: 'tool', header: 'Useful support', value: (r) => r.recommended_tool_family, minWidth: 220 },
    { id: 'gets', header: 'What the partner gets', value: (r) => wordsOf(r.donor_signals.slice(0, 1), w.benefit_donor).join(''), minWidth: 150 },
    { id: 'builds', header: `What ${name} builds`, value: (r) => w.benefit_recipient[r.stage] ?? r.capability, minWidth: 170 },
  ];
  return (
    <div className="space-y-5">
      <div data-testid="devpart-recipient-steps">
        <TableView rows={steps} cols={stepCols} caption={`${cap(poss(name))} Catch Up steps and the development case`} rowKey={(r) => r.key} pageSize={10} wrapHeaders />
      </div>
      {!ok.length ? (
        <Notice kind="info" title={NO_DEVELOPMENT_SOLUTION}>
          None of {poss(name)} Catch Up steps passes the development test (a capability anchor corroborated beyond context signals, sufficient evidence and confidence, inside the AIB trust screen).
        </Notice>
      ) : (
        <>
          <Select label="Catch Up capability" value={step} options={ok.map((s) => ({ value: s.key, label: s.step_name }))} onChange={setStep} />
          <SubCard id="devpart-recipient-partners" title={`Developed partners that need ${sel?.step_name ?? 'this capability'}`}>
            {groups.own.length ? (
              <TableView rows={groups.own} cols={donorCols} caption={`Developed partners with their own need for ${sel?.step_name ?? ''}`} rowKey={(r) => r.donor_iso3} pageSize={8} wrapHeaders />
            ) : (
              <p>No developed partner has its own need for this step; the allied gap below is the case for support.</p>
            )}
            {groups.allied.length > 0 && (
              <Fold id="devpart-allied-only" summary={`${groups.allied.length} more allied economies where the AIB needs new capacity in this step (BUILD) though they have no need of their own`}>
                <p className="text-xs text-paper-mute">{groups.allied.map((p) => nameOf(p.donor_iso3)).join(', ')}</p>
              </Fold>
            )}
          </SubCard>
        </>
      )}
      {preserve.length > 0 && (
        <SubCard id="devpart-recipient-commercial" title={`Where ${name} is already competitive: commercial partners (not aid)`}>
          <p>{cap(poss(name))} Preserve steps: the developed partners that need them are a commercial case (trade, offtake, investment and export finance), not a development one.</p>
          <ul className="space-y-1">
            {preserve.map((s) => {
              const who = commercialOf(s.key);
              return (
                <li key={s.key}>
                  <StepButton tech={s.tech} stage={s.stage} label={s.step_name} />{' '}
                  <span className="text-xs text-paper-mute">
                    {who.length ? `${who.slice(0, 6).join(', ')}${who.length > 6 ? ` and ${who.length - 6} more` : ''}` : 'no commercial partner'}
                    {s.n_review ? ` · ${s.n_review} review case${s.n_review === 1 ? '' : 's'}${s.preserve_fail ? ` (${w.fail[s.preserve_fail] ?? s.preserve_fail})` : ''}` : ''}
                  </span>
                </li>
              );
            })}
          </ul>
        </SubCard>
      )}
    </div>
  );
}

// ---- the section ----------------------------------------------------------------------------------------------

/** The method in a disclosure: how a pair qualifies, without a score. */
function Method({ doc }: { doc: DevpartDoc }) {
  return (
    <Fold id="devpart-method" summary="How a development partnership qualifies (no score: every reason is shown)">
      <ul className="list-disc space-y-1 pl-5 text-sm text-paper-dim">
        <li>Donor reason: a high-income economy in the Allied Industrial Base demand scope whose step is Partner, Secure with allied delivery, a High fence with few trusted alternatives, of high supply-chain need, or where allied supply falls short or the AIB needs new capacity. Watch alone is no reason.</li>
        <li>Recipient reason: a non-high-income economy whose same step is Catch Up, with a capability anchor corroborated beyond context signals, sufficient evidence and confidence, and inside the AIB trust screen.</li>
        <li>Route: a partner that already hosts the step&apos;s new capacity in the AIB, or Preserves it, is a commercial case (trade, offtake, investment, export finance), not an aid case.</li>
        <li>Relationship: aid by step (OECD CRS; Components and Assembly share purpose codes; recycling has none), export and lender finance (OCI, not aid), direct investment (IMF) and factory projects (CM-FDI) describe and order the pairs; they never make one. Missing data stay missing.</li>
        <li>Development Potential, a supporting partner indicator, is context in the Partner section and does not set these pairs.</li>
      </ul>
      <p className="mt-2 text-xs text-paper-mute">Sources: {doc.meta.sources.join('; ')}.</p>
    </Fold>
  );
}

export function DevelopmentPartnershipsSection() {
  const { sheet, entry } = useProfile();
  const name = proseName(sheet.meta.country);
  const dp = useDevpartDoc(entry);
  const doc = dp.data;
  const kind = doc?.perspective ?? null;
  let body: ReactNode;
  if (!entry.devpart) body = <p className="text-sm text-paper-mute">No development-partnership analysis for {name} in this build.</p>;
  else if (dp.status === 'loading') body = <p className="text-sm text-paper-mute">Loading the development partnerships…</p>;
  else if (dp.status === 'error' || !doc) {
    body = (
      <Notice kind="warning" title="The development partnerships could not be loaded" action={<button type="button" className="btn btn-ghost btn-sm" onClick={dp.retry}>Retry</button>}>
        {dp.error?.message ?? 'Unknown error.'}
      </Notice>
    );
  } else if (kind === 'none') body = <p className="text-sm text-paper-mute" data-testid="devpart-none-perspective">No development-partnership view for {name}: {doc.perspective_reason ?? 'not applicable'}.</p>;
  else body = kind === 'donor' ? <DonorView doc={doc} /> : <RecipientView doc={doc} />;
  // the donor or recipient question only once the file says which; neutral words otherwise
  const q = kind === 'recipient' ? DEVPART_QUESTION.recipient(name) : kind === 'donor' ? DEVPART_QUESTION.donor(name) : null;
  return (
    <Section id={DEVPART_SECTION_ID} level={3} title={kind === 'recipient' ? 'Development partnerships: what support could help it Catch Up' : 'Development partnerships'}
      lead={q ? <p className="text-[0.9375rem] font-medium text-paper" data-testid="devpart-question">{q}</p> : undefined}
      intro={kind === 'recipient' ? DEVPART_INTRO.recipient : kind === 'donor' ? DEVPART_INTRO.donor : DEVPART_INTRO.neutral}>
      <div data-testid={`devpart-${kind ?? 'pending'}`}>{body}</div>
      {doc && kind !== 'none' && <Method doc={doc} />}
    </Section>
  );
}

/** The short card at the top of the Partner section's Development tab: the canonical analysis first, the link to it. */
export function DevelopmentPartnershipsSummary() {
  const { sheet, entry } = useProfile();
  const name = proseName(sheet.meta.country);
  const dp = useDevpartDoc(entry);
  const doc = dp.data;
  let text: string;
  if (!entry.devpart) text = `No development-partnership analysis for ${name} in this build.`;
  else if (!doc) text = dp.status === 'error' ? 'The development partnerships could not be loaded.' : 'Loading the development partnerships…';
  else if (doc.perspective === 'none') text = `No development-partnership view: ${doc.perspective_reason ?? 'not applicable'}.`;
  else {
    const r = byRoute(devpartPairs(doc));
    const partners = [...new Set(r.development.map((p) => p.recipient_iso3))];
    text = doc.perspective === 'donor'
      ? (r.development.length ? `${r.development.length} pairs with ${partners.length} developing partners where ${poss(name)} need meets a credible Catch Up pathway.` : NO_DEVELOPMENT_SOLUTION)
      : (devpartRows<DevpartRecipientStep>(doc.steps).some((s) => s.recipient_ok) ? `Catch Up steps of ${name} with developed partners that need the same capability.` : NO_DEVELOPMENT_SOLUTION);
  }
  return (
    <section className="card p-5" aria-labelledby="devpart-summary-card-h" data-testid="devpart-summary-card">
      <h4 id="devpart-summary-card-h" className="rule-neis-sm text-base font-medium text-paper">Development partnerships</h4>
      <p className="mt-2 text-sm text-paper-dim">{text}</p>
      <p className="mt-2 text-sm">
        <a className="link" href={`#${DEVPART_SECTION_ID}`}>
          The partnerships, partner by partner (Allied strategy) &rarr;
        </a>
      </p>
    </section>
  );
}
