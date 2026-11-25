// /review/profile: the short reviewer profile (docs/QC_SYSTEM_DESIGN.md 4.3, 5 step 2). Name (optional) and an
// anonymous switch, organization, role, the countries, sectors, supply-chain stages and methods the reviewer
// knows (the shared tags of review/tags.ts), each selected one rated 1-5, familiarity with NEIS and with the
// Explorer, and whom they have advised. No biography. The form autosaves as a draft on every change and resumes
// from it; "Save profile" stores the profile (review/storage.ts) with a random reviewer id made here, once,
// and goes to the one-minute calibration (first time) or the cases. UX v2: the review mode, decision review (the
// default: factual realism, strategic reasoning, an actionable conclusion) or technical review (each measure from
// raw value to classification), saved on the profile and as the stored mode (storage.ts saveReviewMode).
// Everything stays in this browser; submissions carry only a digest and the tags (the mode is not in the digest).

import { useEffect, useMemo, useState } from 'react';
import { useNavigate } from 'react-router';
import { Notice, Segmented, Switch } from '../../components/ui.tsx';
import { useTitle } from '../../shell/title.ts';
import { ChoiceGrid, Field, MultiCheck } from '../components/form.tsx';
import type { AdvisedOn, ReviewerProfile, ReviewMode } from '../model.ts';
import { MODULE, PROFILE_DRAFT_REF, REVIEW_PATH, useReviewCtx, withQuery } from '../ReviewLayout.tsx';
import { confidenceKey, METHOD_TAG_LABEL, METHOD_TAGS, normaliseCountryTag, normaliseProfile, SECTOR_TAG_LABEL, SECTOR_TAGS, STAGE_TAGS, tagLabel, type TagKind } from '../tags.ts';

export interface ProfileForm {
  name: string;
  anonymous: boolean;
  organization: string;
  role: string;
  countries: string[];
  /** Other countries typed as three-letter codes. */
  countryText: string;
  sectors: string[];
  stages: string[];
  methods: string[];
  confidence: Record<string, number>;
  familiarity_neis: 0 | 1 | 2 | 3 | null;
  familiarity_explorer: 0 | 1 | 2 | 3 | null;
  advised_on: AdvisedOn[];
  /** UX v2: decision review (default) or technical review. */
  review_mode: ReviewMode;
}

export const EMPTY_FORM: ProfileForm = {
  name: '',
  anonymous: false,
  organization: '',
  role: '',
  countries: [],
  countryText: '',
  sectors: [],
  stages: [],
  methods: [],
  confidence: {},
  familiarity_neis: null,
  familiarity_explorer: null,
  advised_on: [],
  review_mode: 'decision',
};

/** The two review modes, as the profile offers them. */
export const REVIEW_MODE_OPTION: Readonly<Record<ReviewMode, { label: string; definition: string }>> = {
  decision: {
    label: 'Decision review',
    definition: 'For sector and country experts. Judge factual realism, strategic reasoning and whether the conclusion is one a government could act on.',
  },
  technical: {
    label: 'Technical review',
    definition: 'For data and methods specialists. Follow each measure: raw value → transformation → normalized indicator → contribution → score → threshold → classification.',
  },
};

export const RATING_LABEL: Readonly<Record<1 | 2 | 3 | 4 | 5, string>> = {
  1: 'Some familiarity',
  2: 'Working knowledge',
  3: 'Professional expertise',
  4: 'Deep expertise',
  5: 'Recognised authority',
};
export const FAMILIARITY_LABEL = ['Not at all', 'A little', 'Well', 'Very well'] as const;
export const ADVISED_LABEL: Readonly<Record<AdvisedOn, string>> = {
  governments: 'Governments',
  firms: 'Firms',
  investors: 'Investors',
  dfis: 'Development finance institutions',
  none: 'None of these',
};

/** The form of a stored profile. */
export function formOf(p: ReviewerProfile): ProfileForm {
  return {
    name: p.name ?? '',
    anonymous: p.anonymous,
    organization: p.organization ?? '',
    role: p.role ?? '',
    countries: [...p.countries],
    countryText: '',
    sectors: [...p.sectors],
    stages: [...p.stages],
    methods: [...p.methods],
    confidence: { ...p.confidence },
    familiarity_neis: p.familiarity_neis,
    familiarity_explorer: p.familiarity_explorer,
    advised_on: [...p.advised_on],
    review_mode: p.review_mode === 'technical' ? 'technical' : 'decision',
  };
}

/** The valid three-letter codes typed in the free country field. */
export const typedCountries = (text: string): string[] => [...new Set(text.split(/[\s,;]+/).map(normaliseCountryTag).filter((x): x is string => !!x))];

/** The tags of a form by kind (typed countries included). */
function tagsOf(f: ProfileForm): { kind: TagKind; tag: string }[] {
  const countries = [...new Set([...f.countries, ...typedCountries(f.countryText)])];
  return [
    ...f.sectors.map((tag) => ({ kind: 'sector' as const, tag })),
    ...countries.map((tag) => ({ kind: 'country' as const, tag })),
    ...f.stages.map((tag) => ({ kind: 'stage' as const, tag })),
    ...f.methods.map((tag) => ({ kind: 'method' as const, tag })),
  ];
}

/** Problems that keep the form from being saved. */
export function formErrors(f: ProfileForm): string[] {
  const e: string[] = [];
  const tags = tagsOf(f);
  if (!tags.some((t) => t.kind !== 'method') && !f.methods.length) e.push('Choose at least one country, sector, stage or method you know.');
  const unrated = tags.filter((t) => !f.confidence[confidenceKey(t.kind, t.tag)]);
  if (unrated.length) e.push(`Rate each area you chose (1 to 5): ${unrated.map((t) => tagLabel(t.kind, t.tag)).join(', ')}.`);
  const bad = f.countryText.split(/[\s,;]+/).filter((x) => x && !normaliseCountryTag(x));
  if (bad.length) e.push(`Use three-letter country codes: ${bad.join(', ')} is not one.`);
  return e;
}

function newReviewerId(): string {
  const c = (globalThis as { crypto?: { randomUUID?: () => string } }).crypto;
  if (c?.randomUUID) return `rv-${c.randomUUID()}`;
  return `rv-${Date.now().toString(36)}-${Math.random().toString(36).slice(2, 10)}`;
}

/** The profile a form saves as (the id and creation time kept from the stored profile). */
export function profileOf(f: ProfileForm, prev: ReviewerProfile | null, now: string, id: () => string = newReviewerId): ReviewerProfile {
  const tags = tagsOf(f);
  const confidence: Record<string, number> = {};
  for (const t of tags) {
    const k = confidenceKey(t.kind, t.tag);
    if (f.confidence[k]) confidence[k] = f.confidence[k];
  }
  const p: ReviewerProfile = {
    reviewer_id: prev?.reviewer_id ?? id(),
    name: f.anonymous || !f.name.trim() ? null : f.name.trim(),
    anonymous: f.anonymous,
    organization: f.organization.trim() || null,
    role: f.role.trim() || null,
    countries: tags.filter((t) => t.kind === 'country').map((t) => t.tag),
    sectors: [...f.sectors],
    stages: [...f.stages],
    methods: [...f.methods],
    confidence,
    familiarity_neis: f.familiarity_neis ?? 0,
    familiarity_explorer: f.familiarity_explorer ?? 0,
    advised_on: [...f.advised_on],
    created_at: prev?.created_at ?? now,
    review_mode: f.review_mode === 'technical' ? 'technical' : 'decision',
  };
  return normaliseProfile(p);
}

function TextInput({ label, value, onChange, hint, disabled }: { label: string; value: string; onChange: (v: string) => void; hint?: string; disabled?: boolean }) {
  return (
    <Field label={label} hint={hint}>
      {({ id, describedBy }) => <input id={id} className="field" value={value} disabled={disabled} aria-describedby={describedBy} onChange={(e) => onChange(e.target.value)} />}
    </Field>
  );
}

function RatingRow({ kind, tag, value, onChange }: { kind: TagKind; tag: string; value: number | undefined; onChange: (v: number) => void }) {
  const id = `rate-${kind}-${tag}`;
  return (
    <div className="flex flex-wrap items-center justify-between gap-2 border-t hairline py-1.5">
      <label htmlFor={id} className="text-sm text-paper-dim">
        {tagLabel(kind, tag)}
      </label>
      <select id={id} className="field field-sm w-auto" value={value ?? ''} onChange={(e) => onChange(Number(e.target.value))}>
        <option value="" disabled>
          Rate 1 to 5
        </option>
        {([1, 2, 3, 4, 5] as const).map((v) => (
          <option key={v} value={v}>
            {v} · {RATING_LABEL[v]}
          </option>
        ))}
      </select>
    </div>
  );
}

const FAMILIARITY_OPTIONS = FAMILIARITY_LABEL.map((label, i) => ({ value: String(i), label }));

export default function ProfileView() {
  useTitle('Your profile', MODULE);
  const ctx = useReviewCtx();
  const navigate = useNavigate();
  const [ref0, ref1, ref2] = PROFILE_DRAFT_REF;
  const [initial] = useState<{ form: ProfileForm; fromDraft: boolean }>(() => {
    const d = ctx.storage.loadDraft<Partial<ProfileForm>>(ref0, ref1, ref2);
    if (d && typeof d === 'object') return { form: { ...EMPTY_FORM, review_mode: ctx.reviewMode, ...d }, fromDraft: true };
    return { form: { ...(ctx.profile ? formOf(ctx.profile) : EMPTY_FORM), review_mode: ctx.reviewMode }, fromDraft: false };
  });
  const [form, setForm] = useState<ProfileForm>(initial.form);
  const [touched, setTouched] = useState(initial.fromDraft);
  const [saved, setSaved] = useState<string | null>(null);

  // autosave the form as a draft on every change (not before the first edit, so a stored profile stays the source)
  useEffect(() => {
    if (touched) ctx.storage.saveDraft(ref0, ref1, ref2, form);
  }, [form, touched, ctx.storage, ref0, ref1, ref2]);

  const set = (patch: Partial<ProfileForm>) => {
    setTouched(true);
    setSaved(null);
    setForm((f) => ({ ...f, ...patch }));
  };
  const rate = (kind: TagKind, tag: string, v: number) => set({ confidence: { ...form.confidence, [confidenceKey(kind, tag)]: v } });

  // the countries the campaign's cases are about (quick picks); other codes can be typed
  const campaignCountries = useMemo(() => [...new Set(ctx.cases.flatMap((c) => c.country_groups ?? []))].sort(), [ctx.cases]);
  const tags = tagsOf(form);
  const errors = formErrors(form);
  const min = ctx.campaign.assignment.min_domain_confidence;

  const save = () => {
    if (errors.length) return;
    const p = profileOf(form, ctx.profile, new Date().toISOString());
    const durable = ctx.saveProfile(p);
    ctx.setReviewMode(p.review_mode ?? 'decision');
    ctx.storage.deleteDraft(ref0, ref1, ref2);
    setTouched(false);
    setSaved(durable ? 'Profile saved in this browser.' : 'Profile kept for this visit only (browser storage is unavailable).');
    navigate(withQuery(ctx.calibrationSeen ? REVIEW_PATH.cases : REVIEW_PATH.calibration, ctx.carry));
  };

  return (
    <form
      className="space-y-8"
      onSubmit={(e) => {
        e.preventDefault();
        save();
      }}
      aria-labelledby="profile-h"
    >
      <div>
        <h2 id="profile-h" className="text-xl font-medium text-paper">
          Your profile
        </h2>
        <p className="mt-2 max-w-3xl text-sm text-paper-dim">
          Two minutes. It decides which cases you are given and lets NEIS weigh answers by expertise. It stays in this browser: your answers carry a random reviewer code and the areas you
          rate, and your name only if you give one.
        </p>
      </div>

      <section aria-labelledby="who-h" className="max-w-xl">
        <h3 id="who-h" className="mb-3 text-lg font-medium text-paper">
          About you
        </h3>
        <div className="mb-4">
          <Switch label="Stay anonymous" hint="Your name is never attached to your answers." checked={form.anonymous} onChange={(v) => set({ anonymous: v })} />
        </div>
        <TextInput label="Name (optional)" value={form.anonymous ? '' : form.name} disabled={form.anonymous} onChange={(v) => set({ name: v })} />
        <TextInput label="Organization (optional)" value={form.organization} onChange={(v) => set({ organization: v })} />
        <TextInput label="Role (optional)" value={form.role} onChange={(v) => set({ role: v })} hint="For example: trade economist, ministry adviser, battery supply-chain analyst." />
      </section>

      <section aria-labelledby="exp-h">
        <h3 id="exp-h" className="mb-1 text-lg font-medium text-paper">
          What you know
        </h3>
        <p className="mb-4 max-w-3xl text-sm text-paper-mute">Choose what applies, then rate each choice. Cases are assigned where you rate {min} or more.</p>
        <div className="grid gap-x-8 sm:grid-cols-2">
          <MultiCheck legend="Sectors" options={SECTOR_TAGS.map((t) => ({ value: t, label: SECTOR_TAG_LABEL[t] }))} value={form.sectors} onChange={(v) => set({ sectors: v })} />
          <MultiCheck legend="Supply-chain stages" options={STAGE_TAGS.map((t) => ({ value: t, label: t }))} value={form.stages} onChange={(v) => set({ stages: v })} />
          <div>
            <MultiCheck legend="Countries in this campaign" options={campaignCountries.map((c) => ({ value: c, label: c }))} value={form.countries} onChange={(v) => set({ countries: v })} />
            <TextInput label="Other countries (optional)" hint="Three-letter codes separated by commas, for example IDN, MEX." value={form.countryText} onChange={(v) => set({ countryText: v })} />
          </div>
          <MultiCheck legend="Methods" options={METHOD_TAGS.map((t) => ({ value: t, label: METHOD_TAG_LABEL[t] }))} value={form.methods} onChange={(v) => set({ methods: v })} />
        </div>
        {tags.length > 0 && (
          <fieldset className="mt-2 max-w-xl" data-testid="ratings">
            <legend className="mb-1 text-sm font-medium text-paper">How well do you know each one?</legend>
            {tags.map((t) => (
              <RatingRow key={`${t.kind}:${t.tag}`} kind={t.kind} tag={t.tag} value={form.confidence[confidenceKey(t.kind, t.tag)]} onChange={(v) => rate(t.kind, t.tag, v)} />
            ))}
          </fieldset>
        )}
      </section>

      <section aria-labelledby="fam-h" className="space-y-4">
        <h3 id="fam-h" className="text-lg font-medium text-paper">
          Familiarity and advice
        </h3>
        <Segmented label="How well do you know NEIS's work?" value={form.familiarity_neis === null ? '' : String(form.familiarity_neis)} options={FAMILIARITY_OPTIONS} onChange={(v) => set({ familiarity_neis: Number(v) as 0 | 1 | 2 | 3 })} />
        <Segmented
          label="How well do you know this Explorer?"
          value={form.familiarity_explorer === null ? '' : String(form.familiarity_explorer)}
          options={FAMILIARITY_OPTIONS}
          onChange={(v) => set({ familiarity_explorer: Number(v) as 0 | 1 | 2 | 3 })}
        />
        <MultiCheck
          legend="Have you advised any of these on these topics? (optional)"
          options={(Object.keys(ADVISED_LABEL) as AdvisedOn[]).map((k) => ({ value: k, label: ADVISED_LABEL[k] }))}
          value={form.advised_on}
          onChange={(v) => set({ advised_on: v as AdvisedOn[] })}
        />
      </section>

      <section aria-labelledby="mode-h" className="max-w-3xl">
        <h3 id="mode-h" className="mb-1 text-lg font-medium text-paper">
          How you would like to review
        </h3>
        <p className="mb-3 text-sm text-paper-mute">You can change this at any time. Either way you answer each case before you see the Explorer&rsquo;s conclusion.</p>
        <ChoiceGrid
          legend="Review mode"
          name="review-mode"
          options={(Object.keys(REVIEW_MODE_OPTION) as ReviewMode[]).map((m) => ({ value: m, label: REVIEW_MODE_OPTION[m].label, definition: REVIEW_MODE_OPTION[m].definition }))}
          value={form.review_mode}
          onChange={(v) => set({ review_mode: v === 'technical' ? 'technical' : 'decision' })}
        />
      </section>

      {touched && errors.length > 0 && (
        <Notice kind="warning" title="Before you save">
          <ul className="list-disc pl-5">
            {errors.map((e) => (
              <li key={e}>{e}</li>
            ))}
          </ul>
        </Notice>
      )}
      {saved && <Notice kind="info">{saved}</Notice>}
      <div className="flex flex-wrap items-center gap-3">
        <button type="submit" className="btn btn-primary" disabled={errors.length > 0}>
          Save profile
        </button>
        <span className="text-xs text-paper-mute" data-testid="draft-state">
          {initial.fromDraft && !saved ? 'Resumed your unsaved draft. ' : ''}
          {touched ? 'Draft saved in this browser as you type.' : ctx.profile ? 'Your saved profile.' : ''}
        </span>
      </div>
    </form>
  );
}
