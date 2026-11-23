// /review/break: break the model (docs/QC_SYSTEM_DESIGN.md 5 step 7). Explains the mode, lists the six kinds
// of break (model.ts BREAK_TYPE_LABEL), and opens the IssueReportDialog in break-the-model mode for an Explorer
// page: by default the last Explorer page this visitor saw (review/components/ReviewToolbar.tsx records it;
// the per-module last locations of shell/lastLocation.ts stand in when it has none), or an address the
// reviewer pastes. The dialog captures that page's context (country, technology, stage, compact, scenario)
// from the address. The reviewer's past break reports in this browser are listed below.

import { useCallback, useEffect, useMemo, useState, type ReactNode } from 'react';
import { Link } from 'react-router';
import { Notice } from '../../components/ui.tsx';
import { useTitle } from '../../shell/title.ts';
import { useOptionalReviewCtx } from '../ReviewLayout.tsx';
import { lastLocations } from '../../shell/lastLocation.ts';
import { BREAK_TYPE_LABEL, SEVERITY_LABEL, type BreakType, type DataIssuePayload, type Submission } from '../model.ts';
import { reviewStorage, type ReviewStorage } from '../storage.ts';
import { pickCampaign, reviewStore } from '../data/store.ts';
import { Field } from '../components/form.tsx';
import { IssueReportDialog, locationOfAddress } from '../components/IssueReportDialog.tsx';
import { lastExplorerLocation } from '../components/ReviewToolbar.tsx';

const BREAK_HINT: Readonly<Record<BreakType, string>> = {
  surprising: 'A result you did not expect and cannot explain from what the page shows.',
  obviously_wrong: 'A result anyone who knows the sector would reject.',
  misleading_data: 'A result that follows from data that are wrong, stale or not comparable.',
  right_for_wrong_reason: 'The conclusion is right, but the reasons the page gives are not why.',
  threshold_artifact: 'A result that flips on a tiny change in a number near a bar.',
  omitted_variable: 'Something that decides the question in reality, which the model does not consider.',
};

/** The default Explorer page to report on: the last one seen, else the last Country, Sector, AIB or legacy page. */
export function defaultBreakTarget(): string {
  const l = lastLocations();
  return lastExplorerLocation() ?? l.country ?? l.sector ?? l.aib ?? l.strategic ?? '';
}

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

export default function BreakModelView({ storage }: { storage?: ReviewStorage } = {}) {
  useTitle('Break the model', 'Expert review');
  const layout = useOptionalReviewCtx();
  const store = storage ?? layout?.storage ?? reviewStorage();
  const [address, setAddress] = useState(defaultBreakTarget);
  const [open, setOpen] = useState(false);
  const [version, setVersion] = useState(0);
  const [campaignId, setCampaignId] = useState<string | null>(null);
  const close = useCallback(() => setOpen(false), []);
  useEffect(() => {
    let live = true;
    reviewStore()
      .manifest()
      .then((m) => live && setCampaignId(pickCampaign(m)?.campaign_id ?? null))
      .catch(() => undefined);
    return () => {
      live = false;
    };
  }, []);
  const past = useMemo(
    () => store.listSubmissions().filter((s): s is Submission<'break_model'> => s.kind === 'break_model').reverse(),
    // eslint-disable-next-line react-hooks/exhaustive-deps
    [store, version],
  );
  const loc = locationOfAddress(address);
  const valid = !!loc && loc.pathname !== '/' && !/^\/review(\/|$)/.test(loc.pathname);

  return (
    <div className="max-w-3xl">
      <ViewHead
        eyebrow="Break the model"
        title="Find where the Explorer is wrong"
        lede="Find a result the Explorer gets wrong, and tell us why. The most useful reports are specific: one page, one result, what is wrong with it and how sure you are."
      />
      <section aria-labelledby="break-kinds">
        <h3 id="break-kinds" className="eyebrow mb-2">
          Six ways a model breaks
        </h3>
        <ul className="grid gap-2 sm:grid-cols-2" data-testid="break-types">
          {(Object.keys(BREAK_TYPE_LABEL) as BreakType[]).map((b) => (
            <li key={b} className="panel px-3 py-2 text-sm">
              <span className="block text-paper">{BREAK_TYPE_LABEL[b]}</span>
              <span className="block text-xs text-paper-mute">{BREAK_HINT[b]}</span>
            </li>
          ))}
        </ul>
      </section>
      <section className="panel mt-6 p-5" aria-labelledby="break-report">
        <h3 id="break-report" className="text-lg font-medium text-paper">
          Report a break
        </h3>
        <p className="mt-1 text-sm text-paper-mute">
          Use the page you were just on, or paste the address of any Explorer page. While you browse the Explorer, the review bar at the bottom of every page does the same.
        </p>
        <div className="mt-3">
          <Field label="Explorer page" hint="An address such as /countries/KOR?node=Batteries|Components, or a full link copied from the browser." error={address && !valid ? 'This is not an Explorer page address.' : null}>
            {({ id, describedBy, invalid }) => (
              <input id={id} className="field" value={address} aria-describedby={describedBy} aria-invalid={invalid || undefined} onChange={(e) => setAddress(e.target.value)} placeholder="/countries/KOR?node=Batteries|Components" />
            )}
          </Field>
        </div>
        <div className="flex flex-wrap gap-2">
          <button type="button" className="btn btn-primary btn-sm" disabled={!valid} onClick={() => setOpen(true)} aria-haspopup="dialog">
            Report a break on this page
          </button>
          {valid && loc && (
            <Link className="btn btn-ghost btn-sm" to={`${loc.pathname}${loc.search ?? ''}${loc.hash ?? ''}`}>
              Open the page
            </Link>
          )}
        </div>
      </section>
      <section className="mt-8" aria-labelledby="break-past">
        <h3 id="break-past" className="eyebrow mb-2">
          Your reports in this browser
        </h3>
        {past.length === 0 ? (
          <p className="text-sm text-paper-mute">None yet.</p>
        ) : (
          <ul className="space-y-2" data-testid="break-past">
            {past.map((s) => (
              <PastReport key={s.submission_id} s={s} />
            ))}
          </ul>
        )}
        {!store.persistent() && (
          <div className="mt-3">
            <Notice kind="warning">This browser does not keep your reports after you leave: download a packet from the review home page before you go.</Notice>
          </div>
        )}
      </section>
      <IssueReportDialog
        open={open}
        onClose={close}
        mode="break_model"
        target={address}
        campaignId={campaignId}
        store={store}
        onSubmitted={() => {
          setVersion((v) => v + 1);
          layout?.refresh();
        }}
      />
    </div>
  );
}

function PastReport({ s }: { s: Submission<'break_model'> }) {
  const p: DataIssuePayload = s.payload;
  const text = p.what_is_wrong.length > 160 ? `${p.what_is_wrong.slice(0, 157)}…` : p.what_is_wrong;
  return (
    <li className="panel px-3 py-2 text-sm">
      <div className="flex flex-wrap items-center justify-between gap-2 text-xs text-paper-mute">
        <span>{p.break_type ? BREAK_TYPE_LABEL[p.break_type] : 'Break'}</span>
        <span>
          {SEVERITY_LABEL[p.severity]} · {(s.submitted_at ?? s.created_at).slice(0, 10)}
        </span>
      </div>
      <p className="mt-1 text-paper-dim">{text}</p>
      {s.page_context?.url && <p className="mt-0.5 break-all text-xs text-paper-mute">{s.page_context.url}</p>}
    </li>
  );
}
