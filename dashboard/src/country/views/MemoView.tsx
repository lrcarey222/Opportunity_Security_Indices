// The memo page /countries/:iso3/memo (spec_country.md A4.3): the pre-written, AI-generated policy memo of
// a core coalition member, on screen with its PDF and Word downloads and the verbatim label. States: not a
// core member -> the profile with a notice; a core member without a memo yet -> the profile's memo
// section with a notice; a stale memo (a later fact sheet changed a claim, or the memo is under revision:
// config memo_revision, 2026-10-17) -> not shown, with a notice that it is being revised; a
// memo kept under an earlier rule (memo.rule_note, e.g. the quadrant rule of 2026-10-15) -> shown with its
// notice (memo.rule_note_d). An economy with neither a core memo nor a brief -> the profile with "No decision
// brief yet". A decision brief written before the current data release (posture manifest created) -> shown with
// a notice that some counts may differ from the Country page (external review readiness, 2026-11-23).

import { useEffect, useState } from 'react';
import { Link, useParams } from 'react-router';
import { Loading, Notice, PageHead, Switch } from '../../components/ui.tsx';
import { useTitle } from '../../shell/title.ts';
import { Redirect } from '../../shell/Redirect.tsx';
import { PAGE, useCountryCtx } from '../CountryLayout.tsx';
import { useMemoDoc } from '../data/hooks.ts';
import { countryStore } from '../data/store.ts';
import { memoDownloadName } from '../derive/model.ts';
import { MemoDocument } from '../memo/MemoDocument.tsx';
import { isBrief } from '../contract.ts';
import { rememberCountry } from '../nav.ts';
import { postureStore } from '../../posture/store.ts';
import { briefPredatesData, briefStaleLine, MEMO_REVISED_TEXT, noBriefLine } from './profile/Header.tsx';

/** The current data release's date (the posture manifest's created; small, no-cache), or null until it is in or when it fails. */
function usePostureRelease(): string | null {
  const [created, setCreated] = useState<string | null>(null);
  useEffect(() => {
    let alive = true;
    postureStore()
      .manifest()
      .then(
        (m) => alive && setCreated(typeof m.created === 'string' ? m.created : null),
        () => undefined,
      );
    return () => {
      alive = false;
    };
  }, []);
  return created;
}

const when = (iso: string | null | undefined): string => {
  if (!iso) return '–';
  const d = new Date(iso);
  if (Number.isNaN(d.getTime())) return iso;
  const months = ['January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December'];
  return `${d.getUTCDate()} ${months[d.getUTCMonth()]} ${d.getUTCFullYear()}`;
};

export default function MemoView() {
  const params = useParams();
  const raw = (params.iso3 ?? '').toUpperCase();
  const { entryOf } = useCountryCtx();
  const entry = entryOf(raw);
  useTitle(entry ? `${entry.name} memo` : 'Countries', 'country');
  const memo = useMemoDoc(entry);
  const [trace, setTrace] = useState(false);
  const [copied, setCopied] = useState(false);
  const release = usePostureRelease();
  if (!entry) return <Redirect to="/countries" notice={`No country with the code '${raw}'; showing all countries.`} />;
  rememberCountry(entry.iso3);
  // legacy memos are for core members; a brief can be for any economy (config/country_brief.yml beyond_core)
  if (entry.tier !== 'core' && entry.memo?.format !== 'brief') {
    return <Redirect to={`/countries/${entry.iso3}`} notice={noBriefLine(entry.name)} />;
  }
  if (!entry.memo) {
    return <Redirect to={`/countries/${entry.iso3}#memo`} notice={`The memo for ${entry.name} is being prepared.`} />;
  }
  const back = (
    <Link className="link text-sm" to={`/countries/${entry.iso3}`}>
      &larr; Back to {entry.name}
    </Link>
  );
  if (entry.memo.stale) {
    return (
      <div className={PAGE}>
        <PageHead eyebrow="COUNTRY INSIGHTS · POLICY MEMO" title={`${entry.name}: policy memo`} lede="" />
        <Notice kind="info">{MEMO_REVISED_TEXT}</Notice>
        <p className="mt-6">{back}</p>
      </div>
    );
  }
  if (memo.status === 'error' && memo.error) {
    return (
      <div className={PAGE}>
        <PageHead eyebrow="COUNTRY INSIGHTS · POLICY MEMO" title={`${entry.name}: policy memo`} lede="" />
        <Notice kind="error" title="The memo could not be loaded" action={<button type="button" className="btn btn-ghost btn-sm" onClick={memo.retry}>Retry</button>}>
          {memo.error.message}
        </Notice>
        <p className="mt-6">{back}</p>
      </div>
    );
  }
  const doc = memo.data;
  if (!doc) {
    return (
      <div className={PAGE}>
        <PageHead eyebrow="COUNTRY INSIGHTS · POLICY MEMO" title={`${entry.name}: policy memo`} lede="" />
        <div className="min-h-[60vh]">
          <Loading label={`Loading the memo for ${entry.name}`} />
        </div>
      </div>
    );
  }
  const url = (f: string | null) => (f ? countryStore().url(f) : null);
  const brief = isBrief(doc);
  const old = briefPredatesData(brief ? { ...entry.memo, format: 'brief' } : entry.memo, release);
  const pdf = url(entry.memo.pdf);
  const docx = url(entry.memo.docx);
  const copy = async () => {
    try {
      await navigator.clipboard.writeText(window.location.href);
      setCopied(true);
      setTimeout(() => setCopied(false), 2000);
    } catch {
      setCopied(false);
    }
  };
  return (
    <div className={PAGE}>
      <PageHead eyebrow={brief ? (doc.eyebrow ?? 'Country strategy brief').toUpperCase() : 'COUNTRY INSIGHTS · POLICY MEMO'} title={doc.title} lede={doc.subtitle} />
      <div className="mb-6 max-w-3xl">
        <Notice kind="info">
          <span className="text-paper">{doc.label}</span>
          <span className="mt-1 block text-xs text-paper-mute">
            Written {when(doc.generated_at)} · checked {when(doc.verified_at)} · {doc.words.toLocaleString('en-US')} words · {brief ? 'data' : 'fact sheet'} {doc.facts_sha256.slice(0, 8)}
          </span>
        </Notice>
      </div>
      {old && (
        <div className="mb-6 max-w-3xl" data-testid="memo-stale-note">
          <Notice kind="warning" title="Written before the current data release">
            {briefStaleLine(old, 'the Country page')} <Link className="link" to={`/countries/${entry.iso3}`}>See the current choices &rarr;</Link>
          </Notice>
        </div>
      )}
      {entry.memo.rule_note && entry.memo.rule_note_d && (
        <div className="mb-6 max-w-3xl" data-testid="memo-rule-note">
          <Notice kind="warning" title="Written under an earlier corner rule">
            {entry.memo.rule_note_d}
          </Notice>
        </div>
      )}
      {doc.placeholder && (
        <div className="mb-6 max-w-3xl">
          <Notice kind="warning" title="Placeholder">
            This is a placeholder used while the memos are written; it is not a memo.
          </Notice>
        </div>
      )}
      <div className="mb-8 flex flex-wrap items-center gap-3 print:hidden">
        {pdf ? (
          <a className="btn btn-primary" href={pdf} download={memoDownloadName(entry.name, doc.as_of, 'pdf')}>
            DOWNLOAD PDF
          </a>
        ) : (
          <span className="btn btn-primary" aria-disabled="true" title="The PDF is not available yet">
            DOWNLOAD PDF
          </span>
        )}
        {docx ? (
          <a className="btn btn-ghost" href={docx} download={memoDownloadName(entry.name, doc.as_of, 'docx')}>
            DOWNLOAD WORD (.DOCX)
          </a>
        ) : (
          <span className="btn btn-ghost" aria-disabled="true" title="The Word file is not available yet">
            DOWNLOAD WORD (.DOCX)
          </span>
        )}
        <button type="button" className="btn-quiet" onClick={() => void copy()}>
          {copied ? 'Link copied' : 'Copy link'}
        </button>
        {back}
      </div>
      <div className="mb-6 print:hidden">
        <Switch label="Show where each number comes from" checked={trace} onChange={setTrace} hint={`Outlines every value taken from the ${brief ? 'data' : 'fact sheet'}; hover one for its field and source.`} />
      </div>
      <MemoDocument doc={doc} trace={trace} fileUrl={(f) => countryStore().url(f)} />
    </div>
  );
}
