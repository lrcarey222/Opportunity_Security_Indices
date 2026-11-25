// The head of the Country profile (spec XI.2, design 4, 2026-10-29): the country's name, ONE analytical
// strategy sentence generated from the strategic position framework's choices (posture/strategy.ts
// strategySentence: every clause backed by at least one node, nothing invented), the data freshness (when
// the strategy was built, the trade benchmark and momentum cut-off, the country facts' date), the country picker
// with the tier / EU / policy-basis tags, and the actions: the memo (memos are being revised: the action says
// so), the strategy CSV and the data downloads. The "at a glance" tiles (the old corners, the top Strategic
// Index sector, the strongest partner, the climate rating) are gone: the counts line under the Country
// Strategy Map replaces them, the corners and the Strategic Index live in Legacy OSI, the partner and the
// climate rating in Allied Strategy and Policy Alignment. The head never reads the Perspective dials.
// The NEIS copy refresh (2026-10-30): the lede opens with the page's strategic question for this economy
// (vocabulary MODULE_QUESTION.country), then answers it with the strategy sentence; the data freshness line
// carries the guide's note on the trade benchmark and momentum signal as its tooltip.

import { Link, useNavigate } from 'react-router';
import { CountrySelect } from '../../../components/CountrySelect.tsx';
import { InfoTip } from '../../../components/InfoTip.tsx';
import { PageHead, Tag } from '../../../components/ui.tsx';
import { DATA_FRESHNESS_NOTE, MODULE_QUESTION } from '../../../content/vocabulary.ts';
import { downloadText, fileSlug } from '../../../lib/download.ts';
import { countryPostureRows, postureCsvText, postureProvenance, strategySentence } from '../../../posture/index.ts';
import { POSTURE_CSV_COLS, postureCsvRows } from '../../../posture/derive.ts';
import { networkOf } from '../../../posture/partnerEligibility.ts';
import type { CountryManifest } from '../../contract.ts';
import { sectorCountryLink } from '../../derive/links.ts';
import { countryOptions } from '../../derive/model.ts';
import { dateLabel, freshnessItems } from '../../derive/strategyPage.ts';
import { cap, poss, proseName, tierTag } from '../../derive/text.ts';
import { countryHref } from '../../url.ts';
import { useProfile } from './common.tsx';

/** A memo the economy may show: a core member's, or a brief for any economy (config/country_brief.yml beyond_core). */
const memoTier = (e: { tier: string; memo: { stale: boolean; format?: string } | null }) => e.tier === 'core' || e.memo?.format === 'brief';

export function memoReady(e: { tier: string; memo: { stale: boolean; format?: string } | null }): boolean {
  return memoTier(e) && !!e.memo && !e.memo.stale;
}

/** A core member's memo that exists but is not shown: a later fact sheet changed a claim, or the memo is
 *  under revision (config memo_revision, 2026-10-17: the original allied network design is retired and
 *  the memos are being rewritten). The memo page and the profile's memo action say so. */
export function memoRevised(e: { tier: string; memo: { stale: boolean; format?: string } | null }): boolean {
  return memoTier(e) && !!e.memo && e.memo.stale;
}

export const MEMO_REVISED_TEXT = 'This memo is being revised. It will be available again once it has been rewritten.';
export const memoRevisedLine = (name: string) => `The memo for ${name} is being revised. It will be available again once it has been rewritten.`;

/** The memo action's label: a decision brief is downloaded from its page; a legacy memo is read there. Nothing is generated on click. */
export const BRIEF_ACTION = 'Download decision brief';
export const MEMO_ACTION = 'Read the policy memo';
export const memoActionLabel = (e: { memo: { format?: string } | null }): string => (e.memo?.format === 'brief' ? BRIEF_ACTION : MEMO_ACTION);

/** For an economy with no decision brief (the briefs are the manifest's memo entries of format 'brief' that are not stale). */
export const noBriefLine = (name: string): string => `No decision brief yet for ${name}.`;

/** The economies with a decision brief on show, by name. */
export function briefCountries(m: Pick<CountryManifest, 'countries'>): string[] {
  return m.countries.filter((c) => c.memo?.format === 'brief' && memoReady(c)).map((c) => c.name);
}

/**
 * A decision brief written before the current data release: the dates (as "4 November 2026") when the brief's
 * generated_at (else verified_at) falls on an earlier day than the posture release's created date; null when
 * the entry is not a brief, a date is missing or unreadable, or the brief is as recent as the data.
 */
export function briefPredatesData(
  memo: { format?: string; generated_at?: string | null; verified_at?: string | null } | null | undefined,
  release: string | null | undefined,
): { written: string; release: string } | null {
  if (!memo || memo.format !== 'brief') return null;
  const at = memo.generated_at || memo.verified_at || null;
  const written = dateLabel(at);
  const rel = dateLabel(release);
  if (!at || !release || !written || !rel) return null;
  return at.slice(0, 10) < release.slice(0, 10) ? { written, release: rel } : null;
}

/** The notice for a brief written before the current data release. */
export const briefStaleLine = (d: { written: string; release: string }, where = 'this page'): string =>
  `This brief was written on ${d.written}, before the current data release (${d.release}); some counts may differ from ${where}.`;

/** The page's strategic question for one economy: "What should Japan secure, build, partner for or deploy?" */
export const profileQuestion = (name: string): string => MODULE_QUESTION.country.replace('this country', name);

/** What the freshness line dates, in a sentence (the guide's data-freshness note, content/vocabulary.ts). */
export const FRESHNESS_NOTE = DATA_FRESHNESS_NOTE;

/** The head's strategy sentence: the framework's (loading and error states in words). */
function useStrategyLede(): string {
  const { sheet, posture } = useProfile();
  const name = cap(proseName(sheet.meta.country));
  if (!posture || posture.status === 'loading') return `Loading the strategy of ${proseName(sheet.meta.country)}…`;
  if (posture.status === 'error' || !posture.core) return `The strategy of ${proseName(sheet.meta.country)} could not be loaded; the sections below say why.`;
  if (!posture.cells.length) return `There is not enough comparable evidence to produce a strategy map for ${proseName(sheet.meta.country)}.`;
  return strategySentence(name, posture.cells, posture.core.meta);
}

export function ProfileHead({ manifest }: { manifest: CountryManifest }) {
  const { sheet, entry, posture } = useProfile();
  const nav = useNavigate();
  const m = sheet.meta;
  const name = proseName(m.country);
  const lede = useStrategyLede();
  const core = posture?.core ?? null;
  const fresh = freshnessItems({ created: core?.created ?? null, benchmark: core?.meta.benchmark_period ?? null, momentumThrough: core?.meta.momentum_through ?? null, asOfLabel: m.as_of_label });
  const csv = () => {
    if (!core) return;
    const rows = postureCsvRows(countryPostureRows(core, sheet.iso3), networkOf(core.countries));
    downloadText(postureCsvText(rows, POSTURE_CSV_COLS, postureProvenance(core)), `${fileSlug(m.country)}-strategy-map.csv`);
  };
  return (
    <>
      {/* the strategy sentence runs wide so the head stays short and the Country Strategy Map sits near the fold (spec XI.3) */}
      <PageHead
        eyebrow="COUNTRIES · COUNTRY PROFILE"
        title={m.country}
        wideLede
        lede={
          <>
            <span className="mb-1 block font-medium text-paper" data-testid="profile-question">
              {profileQuestion(name)}
            </span>
            <span data-testid="strategy-sentence">{lede}</span>
          </>
        }
      >
        <div className="mt-2 flex flex-wrap items-center gap-x-4 gap-y-2">
          <p className="max-w-3xl text-xs text-paper-mute" data-testid="data-freshness" title={FRESHNESS_NOTE}>
            Data: {fresh.join(' · ')}.
          </p>
          <div className="flex flex-wrap items-center gap-2">
            <Tag accent={m.tier === 'core'}>{tierTag(m.tier)}</Tag>
            {m.eu_member && <Tag>EU MEMBER</Tag>}
            {m.policy_basis === 'IEA PAMS only' && (
              <span className="inline-flex items-center gap-1">
                <Tag>POLICY: RECORDS ONLY, NO CLIMATE RATING</Tag>
                <InfoTip id="policy_basis" />
              </span>
            )}
            {m.alignment_basis === 'inherited' && <Tag>TERRITORY</Tag>}
          </div>
        </div>
      </PageHead>
      {/* one row on a wide screen (picker, then the actions), so the Country Strategy Map sits near the fold (spec XI.3) */}
      <div className="mb-6 flex flex-wrap items-start gap-x-8 gap-y-4">
        <CountrySelect label="Choose a country" value={entry.iso3} options={countryOptions(manifest)} onChange={(iso) => nav(countryHref(iso))} className="w-72 max-w-full" />
        <div id="memo" className="flex scroll-mt-16 flex-wrap items-start gap-x-6 gap-y-3" data-testid="profile-actions">
          <MemoAction manifest={manifest} />
          <div>
            <button type="button" className="btn btn-ghost" onClick={csv} disabled={!core || !posture?.cells.length}>
              DOWNLOAD STRATEGY (CSV)
            </button>
            <p className="mt-1 max-w-xs text-xs text-paper-mute">Every technology and stage with its choice and the need, prize and position behind it</p>
          </div>
          <div className="flex flex-col gap-1 self-center text-sm">
            <a className="link" href="#downloads">
              All data and downloads &darr;
            </a>
            <Link to={sectorCountryLink(entry.iso3)} className="link">
              {cap(poss(name))} raw data in Sectors &rarr;
            </Link>
          </div>
        </div>
      </div>
    </>
  );
}

/**
 * The memo action: "Download decision brief" (it opens the brief's page, with the PDF and Word files) for an
 * economy with a brief, "Read the policy memo" for a core member's memo, else the reason in words (being
 * revised, being prepared, no brief yet). A brief written before the current data release says so.
 */
function MemoAction({ manifest }: { manifest: CountryManifest }) {
  const { sheet, entry, posture } = useProfile();
  const name = proseName(sheet.meta.country);
  const label = memoActionLabel(entry);
  if (memoReady(entry)) {
    const brief = entry.memo?.format === 'brief';
    const old = briefPredatesData(entry.memo, posture?.core?.created ?? null);
    const written = dateLabel(entry.memo?.generated_at ?? null);
    return (
      <div>
        <Link to={`/countries/${entry.iso3}/memo`} className="btn btn-primary">
          {label}
        </Link>
        <p className="mt-1 max-w-xs text-xs text-paper-mute">
          {brief
            ? `A short decision brief${written ? ` written ${written}` : ''} from the Explorer data, as PDF or Word; evidence, sources and caveats in its appendix.`
            : `AI-generated from the data and checked against it; baseline as of ${manifest.as_of_label}. It reads the security and opportunity scores, not the strategy above.`}
        </p>
        {old && (
          <p className="mt-1 max-w-xs text-xs text-signal-hi" data-testid="memo-stale-note">
            {briefStaleLine(old)}
          </p>
        )}
        {entry.memo?.rule_note && entry.memo.rule_note_d && (
          <p className="mt-1 max-w-md text-xs text-paper-mute" data-testid="memo-rule-note">
            {entry.memo.rule_note_d}
          </p>
        )}
      </div>
    );
  }
  if (memoRevised(entry)) {
    return (
      <div>
        <button type="button" className="btn btn-primary" disabled aria-describedby="memo-revised-head">
          {label}
        </button>
        <p id="memo-revised-head" className="mt-1 max-w-xs text-xs text-paper-mute">
          {memoRevisedLine(name)}
        </p>
      </div>
    );
  }
  if (entry.tier === 'core') return <p className="max-w-xs self-center text-sm text-paper-dim">{`The memo for ${name} is being prepared.`}</p>;
  const briefs = briefCountries(manifest);
  return (
    <p className="max-w-xs self-center text-sm text-paper-dim" data-testid="memo-none">
      {noBriefLine(name)}
      {briefs.length > 0 && <span className="mt-1 block text-xs text-paper-mute">{`Briefs so far: ${briefs.join(', ')}.`}</span>}
    </p>
  );
}
