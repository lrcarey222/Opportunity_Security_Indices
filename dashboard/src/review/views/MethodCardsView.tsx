// /review/methods: the methodology red team (docs/QC_SYSTEM_DESIGN.md 5 step 5). One card of
// public/data/review/method_cards.json at a time: the current choice, why it was made, a concrete example with
// a link into the Explorer, and the alternatives; then the reviewer's stance (Retain / Modify / Reject /
// Unsure), materiality 1-4 (read through the severity words), the alternative they would use, the outputs they
// expect it to affect, the direction of any systematic bias and a comment. Cards outside the reviewer's stated
// method expertise are marked (assign.ts methodCardRelevance) but stay answerable. Drafts autosave per card
// (storage.ts); each card is submitted on its own through the build's sink. A visitor without a profile answers
// as an anonymous reviewer.

import { useEffect, useMemo, useState, type ReactNode } from 'react';
import { Link, useSearchParams } from 'react-router';
import { Loading, Notice, ProgressBar, Tag } from '../../components/ui.tsx';
import { useTitle } from '../../shell/title.ts';
import { useOptionalReviewCtx, type ReviewCtx } from '../ReviewLayout.tsx';
import { methodCardRelevance } from '../assign.ts';
import { appCommit } from '../appInfo.ts';
import { newSubmission } from '../context.ts';
import { METHOD_MATERIALITY_SEVERITY } from '../aggregate.ts';
import { SEVERITY_LABEL, type BiasDirection, type MethodCard, type MethodFeedbackPayload, type Stance } from '../model.ts';
import { getSink } from '../sink.ts';
import { reviewStorage, type ReviewStorage } from '../storage.ts';
import { pickCampaign, reviewStore, type ReviewManifestDoc } from '../data/store.ts';
import { ChoiceGrid, DontKnowToggle, Field, TextArea } from '../components/form.tsx';
import { reviewerProfileOrAnonymous } from '../components/IssueReportDialog.tsx';

export const STANCE_LABEL: Readonly<Record<Stance, string>> = {
  retain: 'Retain: the current choice is right',
  modify: 'Modify: right idea, wrong setting or detail',
  reject: 'Reject: a different approach is needed',
  unsure: 'Unsure',
};
const STANCES = Object.keys(STANCE_LABEL) as Stance[];

export const BIAS_LABEL: Readonly<Record<BiasDirection, string>> = {
  overstates_need: 'Overstates need',
  understates_need: 'Understates need',
  favours_incumbents: 'Favours incumbent producers',
  favours_new_entrants: 'Favours new entrants',
  other: 'Another direction (say which in the comment)',
  none: 'No systematic bias',
  unsure: 'Unsure',
};
const BIASES = Object.keys(BIAS_LABEL) as BiasDirection[];
const MATERIALITY = [1, 2, 3, 4] as const;

interface CardDraft {
  stance: Stance | null;
  materiality: 1 | 2 | 3 | 4 | null;
  alternative: string;
  affected_outputs: string;
  bias_direction: BiasDirection | null;
  comment: string;
  outside_expertise: boolean;
}
const EMPTY: CardDraft = { stance: null, materiality: null, alternative: '', affected_outputs: '', bias_direction: null, comment: '', outside_expertise: false };

/** What a card draft still needs before it can be submitted. */
export function cardDraftErrors(d: CardDraft): string[] {
  const e: string[] = [];
  if (!d.stance) e.push('Choose retain, modify, reject or unsure.');
  if ((d.stance === 'modify' || d.stance === 'reject') && !d.alternative.trim() && !d.comment.trim()) e.push('Say what you would do instead (the alternative or a comment).');
  return e;
}

function toPayload(card: MethodCard, d: CardDraft): MethodFeedbackPayload {
  return {
    card_id: card.card_id,
    stance: d.stance as Stance,
    materiality: d.materiality,
    alternative: d.alternative.trim() || null,
    affected_outputs: d.affected_outputs.trim() || null,
    bias_direction: d.bias_direction ?? 'unsure',
    comment: d.comment.trim() || null,
    outside_expertise: d.outside_expertise,
  };
}

interface Loaded {
  manifest: ReviewManifestDoc;
  cards: MethodCard[];
}

/**
 * Why the cards' examples are held back, or null when they may show. The examples quote the Explorer's conclusions
 * for named cells and nodes, several of which are review cases (blind-first, docs/QC_SYSTEM_DESIGN.md 5 step 4):
 * inside the review layout they show only once the reviewer has a profile and has submitted every required
 * assigned case. 'no_profile': no profile yet (the visitor may still become a case reviewer); 'open_cases': a
 * required case is not submitted. Outside the layout (a view on its own) nothing is held.
 */
export function exampleHoldReason(ctx: Pick<ReviewCtx, 'profile' | 'assignments' | 'statusOf'> | null): 'no_profile' | 'open_cases' | null {
  if (!ctx) return null;
  if (!ctx.profile) return 'no_profile';
  return ctx.assignments.some((a) => !a.optional && ctx.statusOf(a.case_id) !== 'submitted') ? 'open_cases' : null;
}

export const EXAMPLE_HELD_TEXT: Readonly<Record<'no_profile' | 'open_cases', string>> = {
  open_cases: 'The examples name the Explorer’s conclusions for cells that are also review cases, so they show once you have submitted your assigned cases. You can answer the card without them.',
  no_profile: 'The examples name the Explorer’s conclusions for cells that are also review cases. If you will review cases, set up your profile and submit them first; you can answer the card without the example.',
};

/** The page's own heading under the review layout's title (the layout holds the h1). */
function ViewHead({ eyebrow, title, lede, children }: { eyebrow: string; title: string; lede: string; children?: ReactNode }) {
  return (
    <header className="mb-5">
      <p className="eyebrow">{eyebrow}</p>
      <h2 className="mt-1 text-xl font-medium text-paper">{title}</h2>
      <p className="mt-2 max-w-3xl text-sm leading-relaxed text-paper-dim">{lede}</p>
      {children}
    </header>
  );
}

export default function MethodCardsView({ storage }: { storage?: ReviewStorage } = {}) {
  useTitle('Method cards', 'Expert review');
  const layout = useOptionalReviewCtx();
  const store = storage ?? layout?.storage ?? reviewStorage();
  const [sp, setSp] = useSearchParams();
  const [data, setData] = useState<Loaded | null>(null);
  const [error, setError] = useState<string | null>(null);
  const [version, setVersion] = useState(0);
  const [showAnyway, setShowAnyway] = useState(false);
  const holdReason = exampleHoldReason(layout);
  const held = holdReason === 'open_cases' || (holdReason === 'no_profile' && !showAnyway) ? holdReason : null;

  useEffect(() => {
    let live = true;
    const rs = reviewStore();
    Promise.all([rs.manifest(), rs.methodCards()]).then(
      ([manifest, doc]) => {
        if (!live) return;
        const entry = pickCampaign(manifest, sp.get('campaign'));
        const ids = entry?.method_card_ids ?? [];
        setData({ manifest, cards: ids.length ? doc.cards.filter((c) => ids.includes(c.card_id)) : doc.cards });
      },
      (e) => live && setError(String((e as Error)?.message ?? e)),
    );
    return () => {
      live = false;
    };
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, []);

  const campaign = data ? pickCampaign(data.manifest, sp.get('campaign')) : null;
  const campaignId = campaign?.campaign_id ?? null;
  const profile = store.loadProfile();
  const submitted = useMemo(() => {
    const m = new Map<string, MethodFeedbackPayload>();
    for (const s of store.listSubmissions(campaignId ?? undefined)) if (s.kind === 'method_feedback') m.set((s.payload as MethodFeedbackPayload).card_id, s.payload as MethodFeedbackPayload);
    return m;
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [store, campaignId, version]);

  if (error) return <Notice kind="error">The method cards could not be loaded: {error}</Notice>;
  if (!data) return <Loading label="Loading the method cards…" />;
  const cards = data.cards;
  if (!cards.length) return <Notice kind="info">This campaign has no method cards.</Notice>;
  const wanted = sp.get('card');
  const idx = Math.max(0, cards.findIndex((c) => c.card_id === wanted));
  const card = cards[idx];
  const go = (i: number) => {
    const q = new URLSearchParams(sp);
    q.set('card', cards[(i + cards.length) % cards.length].card_id);
    setSp(q, { replace: true });
  };
  return (
    <div className="max-w-3xl">
      <ViewHead eyebrow="Methodology" title="Method cards" lede="One methodological choice at a time: what the Explorer does, why, an example, and the alternatives. Tell us whether to keep it, change it or drop it, and how much it matters.">
        <div className="mt-4">
          <ProgressBar value={submitted.size} max={cards.length} label={`${submitted.size} of ${cards.length} cards answered`} />
        </div>
      </ViewHead>
      {!profile && (
        <Notice kind="info">
          You have no reviewer profile in this browser: your answers are recorded anonymously and no card is matched to your expertise. <Link className="link" to="/review">Set up a profile</Link>.
        </Notice>
      )}
      <nav className="mt-4 flex flex-wrap items-center justify-between gap-2" aria-label="Method cards">
        <button type="button" className="btn btn-ghost btn-sm" onClick={() => go(idx - 1)}>
          &larr; Previous card
        </button>
        <span className="text-sm text-paper-mute tabular">
          Card {idx + 1} of {cards.length}
        </span>
        <button type="button" className="btn btn-ghost btn-sm" onClick={() => go(idx + 1)}>
          Next card &rarr;
        </button>
      </nav>
      <CardForm
        key={card.card_id}
        card={card}
        campaignId={campaignId}
        dataRelease={data.manifest.data_release.id}
        relevance={profile ? methodCardRelevance(profile, card) : null}
        exampleHeld={held}
        onShowExample={held === 'no_profile' ? () => setShowAnyway(true) : null}
        done={submitted.get(card.card_id) ?? null}
        store={store}
        onSubmitted={() => {
          setVersion((v) => v + 1);
          layout?.refresh();
        }}
        onNext={() => go(idx + 1)}
      />
    </div>
  );
}

function CardForm({
  card, campaignId, dataRelease, relevance, exampleHeld = null, onShowExample = null, done, store, onSubmitted, onNext,
}: {
  card: MethodCard;
  campaignId: string | null;
  dataRelease: string;
  relevance: 'in' | 'outside' | null;
  exampleHeld?: 'no_profile' | 'open_cases' | null;
  onShowExample?: (() => void) | null;
  done: MethodFeedbackPayload | null;
  store: ReviewStorage;
  onSubmitted: () => void;
  onNext: () => void;
}) {
  const slot = [campaignId ?? '_none', 'method_feedback', card.card_id] as const;
  const [d, setD] = useState<CardDraft>(() => ({ ...EMPTY, ...(store.loadDraft<Partial<CardDraft>>(...slot) ?? {}) }));
  const [tried, setTried] = useState(false);
  const [msg, setMsg] = useState<string | null>(null);
  const [busy, setBusy] = useState(false);
  const set = (patch: Partial<CardDraft>) =>
    setD((prev) => {
      const next = { ...prev, ...patch };
      store.saveDraft(...slot, next);
      return next;
    });
  const errors = cardDraftErrors(d);

  const submit = async () => {
    setTried(true);
    if (errors.length || busy) return;
    setBusy(true);
    const sub = newSubmission('method_feedback', toPayload(card, d), {
      profile: reviewerProfileOrAnonymous(store),
      campaign_id: campaignId,
      page_context: null,
      release: { data_release: dataRelease, app_commit: appCommit() },
    });
    const r = await getSink(undefined, store).submit(sub);
    setBusy(false);
    setMsg(r.message);
    if (r.ok) {
      store.deleteDraft(...slot);
      onSubmitted();
    }
  };

  return (
    <article className="panel mt-4 p-5" data-testid="method-card" data-card={card.card_id}>
      <div className="flex flex-wrap items-center gap-2">
        <span className="eyebrow">{card.card_id}</span>
        {relevance === 'outside' && <Tag title="Your profile does not list this kind of method: answer if you can, or say you do not know.">Outside your stated expertise</Tag>}
        {relevance === 'in' && <Tag accent>In your expertise</Tag>}
        {done && <Tag>Answered</Tag>}
      </div>
      <h3 className="mt-2 text-lg font-medium text-paper">{card.title}</h3>
      <dl className="mt-4 space-y-3 text-sm">
        <div>
          <dt className="label">Current choice</dt>
          <dd className="mt-1 text-paper-dim">{card.current_choice}</dd>
        </div>
        <div>
          <dt className="label">Why</dt>
          <dd className="mt-1 text-paper-dim">{card.why}</dd>
        </div>
        <div>
          <dt className="label">Example</dt>
          {exampleHeld ? (
            <dd className="mt-1 text-paper-mute" data-testid="example-held">
              {EXAMPLE_HELD_TEXT[exampleHeld]}
              {onShowExample && (
                <>
                  {' '}
                  <button type="button" className="btn btn-quiet btn-sm" onClick={onShowExample}>
                    Show the examples: I will not review cases
                  </button>
                </>
              )}
            </dd>
          ) : (
            <dd className="mt-1 text-paper-dim">
              {card.example.text}
              {card.example.explorer_path && (
                <>
                  {' '}
                  <Link className="link" to={card.example.explorer_path}>
                    See it in the Explorer &rarr;
                  </Link>
                </>
              )}
            </dd>
          )}
        </div>
        {card.alternatives.length > 0 && (
          <div>
            <dt className="label">Alternatives</dt>
            <dd className="mt-1">
              <ul className="list-disc space-y-1 pl-5 text-paper-dim">
                {card.alternatives.map((a, i) => (
                  <li key={i}>{a}</li>
                ))}
              </ul>
            </dd>
          </div>
        )}
      </dl>

      <form
        className="mt-6 border-t hairline pt-4"
        noValidate
        onSubmit={(e) => {
          e.preventDefault();
          void submit();
        }}
      >
        {done && <p className="mb-3 text-sm text-paper-mute">You answered this card ({STANCE_LABEL[done.stance]}). Submitting again records a new answer; the latest counts.</p>}
        <ChoiceGrid legend="Should the Explorer keep this choice?" options={STANCES.map((s) => ({ value: s, label: STANCE_LABEL[s] }))} value={d.stance} onChange={(v) => set({ stance: v as Stance })} error={tried && !d.stance ? errors[0] : null} />
        <fieldset className="mb-4">
          <legend className="mb-1 text-sm font-medium text-paper">How much does it matter?</legend>
          <p className="mb-1.5 text-xs text-paper-mute">If the choice were changed, how much would the Explorer&apos;s conclusions move?</p>
          <div className="flex flex-wrap gap-1.5">
            {MATERIALITY.map((m) => (
              <label key={m} className="tag cursor-pointer" style={d.materiality === m ? { borderColor: 'var(--color-signal)', color: 'var(--color-paper)' } : undefined}>
                <input type="radio" className="sr-only" name={`mat-${card.card_id}`} value={m} checked={d.materiality === m} onChange={() => set({ materiality: m })} />
                {m} · {SEVERITY_LABEL[METHOD_MATERIALITY_SEVERITY[m]]}
              </label>
            ))}
          </div>
        </fieldset>
        <TextArea
          label="The alternative you would use"
          rows={3}
          value={d.alternative}
          onChange={(v) => set({ alternative: v })}
          required={d.stance === 'modify' || d.stance === 'reject'}
          error={tried && errors.length && (d.stance === 'modify' || d.stance === 'reject') && !d.alternative.trim() && !d.comment.trim() ? errors[errors.length - 1] : null}
        />
        <TextArea label="Outputs you expect it to affect (optional)" rows={2} value={d.affected_outputs} onChange={(v) => set({ affected_outputs: v })} hint={card.affected_outputs.length ? `The Explorer lists: ${card.affected_outputs.join('; ')}.` : undefined} />
        <Field label="Does it bias the results systematically?">
          {({ id }) => (
            <select id={id} className="field" value={d.bias_direction ?? ''} onChange={(e) => set({ bias_direction: (e.target.value || null) as BiasDirection | null })}>
              <option value="">Choose a direction</option>
              {BIASES.map((b) => (
                <option key={b} value={b}>
                  {BIAS_LABEL[b]}
                </option>
              ))}
            </select>
          )}
        </Field>
        <TextArea label="Comment (optional)" rows={3} value={d.comment} onChange={(v) => set({ comment: v })} />
        <DontKnowToggle checked={d.outside_expertise} onChange={(v) => set({ outside_expertise: v })} />
        <div className="mt-3 flex flex-wrap items-center gap-2">
          <button type="submit" className="btn btn-primary btn-sm" disabled={busy}>
            {busy ? 'Sending…' : 'Submit this card'}
          </button>
          <button type="button" className="btn btn-ghost btn-sm" onClick={onNext}>
            Skip to the next card
          </button>
          <span className="text-xs text-paper-mute">Your draft is saved as you type.</span>
        </div>
        {msg && (
          <p role="status" className="mt-2 text-sm text-paper-dim" data-testid="method-result">
            {msg}
          </p>
        )}
      </form>
    </article>
  );
}
