// The Country Insights landing /countries (spec_country.md A4.1): the country picker, the core members
// (with their strategy counts and the MEMO tag), the coalition partners, the other economies (closed
// <details>), a world map coloured by coalition tier (lazy atlas; omitted below 768 px) and "How to read a
// country profile". Since 2026-10-29 (design 5, spec XI) the roster line under each name is the six-verb
// count of the strategic position framework ("2 Secure · 5 Preserve · ..."), read from the posture core
// (shared with the profiles, so the first profile opened needs no second fetch), and the reading guide
// leads with Need -> Prize -> Position -> Choice -> Policy Alignment; the four corners are explained as the
// Legacy OSI framework with their conceptual mapping (Grow ~ Preserve, Upgrade ~ Catch Up, Exposed ~
// Partner, Stable ~ Watch).
// The NEIS copy refresh (2026-10-30): the page opens with its strategic question (vocabulary MODULE_QUESTION)
// and the guide's lede (no country should own the whole stack; deciding what not to build), the reading guide
// starts with the Strategy Map, the six choices as actions and the three yards in the user's words, and the
// Legacy OSI corners sit in a closed disclosure under it.

import { lazy, Suspense, useMemo, useState } from 'react';
import { Link, useNavigate } from 'react-router';
import { CountrySelect } from '../../components/CountrySelect.tsx';
import { InfoTip } from '../../components/InfoTip.tsx';
import { Loading, PageHead } from '../../components/ui.tsx';
import { FRAMING, LEGACY_OSI_NOTE, MODULE_QUESTION, PARTNER_LONG_NAME, STATE, YARD_AND_FENCE, YARD_CHOICES, YARD_DEFINITION } from '../../content/vocabulary.ts';
import { countPostures, countryCells, strategyCountsText, usePostureCore, VERB_LABEL, VERB_ORDER, VERB_SUBTITLE, YARD_ORDER, type PostureCore } from '../../posture/index.ts';
import { useMediaQuery } from '../../shell/useMediaQuery.ts';
import { useTitle } from '../../shell/title.ts';
import { PAGE, useCountryCtx } from '../CountryLayout.tsx';
import { LEGACY_CORNERS, type CountryEntry, type LegacyCorner } from '../contract.ts';
import { countryOptions, rosterGroups } from '../derive/model.ts';
import { countryHref } from '../url.ts';

const TierMap = lazy(() => import('../charts/TierMap.tsx'));

/** The landing's lede (the editorial guide's Country landing lede). */
export const COUNTRY_LANDING_LEDE =
  'No country should try to own the whole stack. Country profiles show where an economy has a strategic reason to build capability, where it should rely on trusted partners, and where open access and faster deployment matter more than domestic production.';
/** How to read a profile (the editorial guide). */
export const HOW_TO_READ =
  'Start with the Strategy Map. Each technology-stage combination gets one strategic choice based on the country’s need, the industrial prize and its current position. The profile then shows why that choice was made, whether policy matches it, and where allies fit.';

/** The reading guide's line for each Legacy OSI corner (the names and glossary ids come from LEGACY_CORNERS). */
const READING: Record<LegacyCorner, string> = {
  legacy_grow: 'high economic opportunity and high energy security',
  legacy_upgrade: 'high economic opportunity but low energy security',
  legacy_exposed: 'low economic opportunity and low energy security',
  legacy_stable: 'low economic opportunity but high energy security',
};

/** The roster line of an economy: its six-verb counts ("2 Secure · 5 Preserve · ..."), or null without a strategy map. */
export function rosterStrategyLine(core: PostureCore | null, iso3: string): string | null {
  if (!core) return null;
  const cells = countryCells(core, iso3);
  if (!cells.length) return null;
  // the not-assessed cells too, so the line accounts for every combination the strategy looked at
  const t = strategyCountsText(countPostures(cells));
  return t.notAssessed ? `${t.verbs} · ${t.notAssessed}` : t.verbs;
}

function CountryCard({ c, memo, core }: { c: CountryEntry; memo: boolean; core: PostureCore | null }) {
  const q = rosterStrategyLine(core, c.iso3);
  return (
    <li>
      <Link to={countryHref(c.iso3)} className="card block h-full p-4 hover:border-signal">
        <span className="flex items-start justify-between gap-2">
          <span className="font-medium text-paper">{c.name}</span>
          {memo && c.memo && !c.memo.stale && <span className="tag py-0 text-[10px]">MEMO</span>}
        </span>
        <span className="mt-1 block text-xs text-paper-mute" data-testid="roster-line">
          {q ?? (c.status === 'no_data' ? 'No index data' : core ? 'Not enough evidence for a strategy map' : c.status === 'non_rankable' ? 'Scores shown, not ranked' : '')}
        </span>
      </Link>
    </li>
  );
}

export default function CountryHomeView() {
  useTitle('Countries', 'country');
  const { manifest } = useCountryCtx();
  const nav = useNavigate();
  const g = useMemo(() => rosterGroups(manifest), [manifest]);
  const opts = useMemo(() => countryOptions(manifest), [manifest]);
  const [pick, setPick] = useState('');
  const wide = useMediaQuery('(min-width: 768px)');
  const nMemos = manifest.countries.filter((c) => c.memo && !c.memo.stale).length;
  const nRevised = manifest.countries.filter((c) => c.tier === 'core' && c.memo?.stale).length;
  const core = usePostureCore().data;
  return (
    <div className={PAGE}>
      <PageHead
        eyebrow="COUNTRIES"
        title={MODULE_QUESTION.country}
        lede={
          <>
            {COUNTRY_LANDING_LEDE} {FRAMING.whatNotToBuild}{' '}
            <span className="text-paper-mute">
              {`Every technology is read at six supply-chain stages, from extraction to recovery. For the ${manifest.counts.core} core coalition members a short policy memo ${nMemos ? 'is ready to read or download' : nRevised ? 'is being revised' : 'is being prepared'}.`}
            </span>
          </>
        }
      />
      <p className="-mt-4 mb-6 text-sm">
        <Link className="link" to="/overview">
          World map of any measure &rarr;
        </Link>
      </p>
      <form
        className="mb-10 flex flex-wrap items-end gap-3"
        onSubmit={(e) => {
          e.preventDefault();
          if (pick) nav(countryHref(pick));
        }}
      >
        <CountrySelect label="Choose a country to see its strategy" value={pick} options={opts} onChange={(iso) => setPick(iso)} className="w-80 max-w-full" />
        <button type="submit" className="btn btn-ghost" disabled={!pick}>
          GO
        </button>
      </form>

      <section id="core" aria-labelledby="core-h" className="mb-10 scroll-mt-16">
        <h2 id="core-h" className="rule-neis-sm mb-1 text-xl font-medium text-paper">
          Core coalition members ({g.core.length})
        </h2>
        <p className="mb-4 max-w-3xl text-sm text-paper-mute">
          The economies the <span className="whitespace-nowrap">allied coalition <InfoTip id="coalition" term="allied coalition" /></span> is built around. The line under each name
          counts its strategic choices across the 66 technology-stage combinations, and the combinations without enough evidence to assess.
        </p>
        <ul className="grid gap-3 sm:grid-cols-2 lg:grid-cols-3 xl:grid-cols-4">
          {g.core.map((c) => (
            <CountryCard key={c.iso3} c={c} memo core={core} />
          ))}
        </ul>
      </section>

      <section id="partners" aria-labelledby="partners-h" className="mb-10 scroll-mt-16">
        <h2 id="partners-h" className="rule-neis-sm mb-4 text-xl font-medium text-paper">
          Coalition partners ({g.partner.length})
        </h2>
        <ul className="grid gap-3 sm:grid-cols-2 lg:grid-cols-3 xl:grid-cols-4">
          {g.partner.map((c) => (
            <CountryCard key={c.iso3} c={c} memo={false} core={core} />
          ))}
        </ul>
      </section>

      <section id="others" aria-labelledby="others-h" className="mb-10 scroll-mt-16">
        <h2 id="others-h" className="rule-neis-sm mb-4 text-xl font-medium text-paper">
          Other economies ({g.none.length})
        </h2>
        <details className="disclosure">
          <summary className="cursor-pointer py-2 text-sm text-paper-dim hover:text-paper">Show the {g.none.length} other economies</summary>
          <ul className="mt-2 columns-1 gap-6 text-sm sm:columns-2 lg:columns-4">
            {g.none.map((c) => (
              <li key={c.iso3} className="mb-1 break-inside-avoid">
                <Link className="link" to={countryHref(c.iso3)}>
                  {c.name}
                </Link>
                {c.status === 'no_data' && <span className="text-paper-mute"> (no index data)</span>}
              </li>
            ))}
          </ul>
        </details>
      </section>

      {wide && (
        <section aria-labelledby="map-h" className="card mb-10 p-5">
          <h2 id="map-h" className="rule-neis-sm text-base font-medium text-paper lg:text-lg">
            The coalition on the map
          </h2>
          <p className="mt-1 text-sm text-paper-mute">Each economy colored by its place in the allied coalition. Click one to open its profile.</p>
          <div className="mt-3">
            <Suspense fallback={<Loading label="Loading the map" />}>
              <TierMap manifest={manifest} onSelect={(iso) => nav(countryHref(iso))} />
            </Suspense>
          </div>
        </section>
      )}

      <section aria-labelledby="read-h" className="panel p-5">
        <h2 id="read-h" className="text-base font-medium text-paper">
          How to read a country profile
        </h2>
        <p className="mt-2 text-sm text-paper-dim">{HOW_TO_READ} Each step takes one of six choices:</p>
        <ul className="mt-3 space-y-2 text-sm text-paper-dim" data-testid="reading-verbs">
          {VERB_ORDER.map((v) => (
            <li key={v}>
              <strong className="text-paper">{v === 'partner' ? PARTNER_LONG_NAME : VERB_LABEL[v]}</strong>: {VERB_SUBTITLE[v].toLowerCase()}.
            </li>
          ))}
        </ul>
        <p className="mt-4 text-sm text-paper-dim">The choices fall behind three fences, each a different policy problem:</p>
        <ul className="mt-2 space-y-2 text-sm text-paper-dim" data-testid="reading-yards">
          {YARD_ORDER.map((y) => (
            <li key={y}>
              <strong className="text-paper">{YARD_AND_FENCE[y]}</strong> ({YARD_CHOICES[y]}): {YARD_DEFINITION[y]}
            </li>
          ))}
        </ul>
        <p className="mt-4 text-sm text-paper-dim">
          {STATE.dataLimitation} A step without enough evidence is {STATE.notAssessedShort.toLowerCase()}: hatched on the map.
        </p>
        <details className="disclosure mt-4">
          <summary className="cursor-pointer py-2 text-sm text-paper-dim hover:text-paper">The Legacy OSI framework, kept for comparison</summary>
          <p className="mt-2 text-sm text-paper-dim">
            {LEGACY_OSI_NOTE} It sits at the end of each profile under Evidence &amp; methodology: the same sectors in four corners of Energy Security and Economic Opportunity, split at
            the country&apos;s own median sector (the NEIS baseline as of {manifest.as_of_label}, with a Perspective control to weigh it differently).
          </p>
          <ul className="mt-3 space-y-2 text-sm text-paper-dim" data-testid="reading-corners">
            {LEGACY_CORNERS.map((q) => (
              <li key={q.id}>
                <strong className="text-paper">{q.label}</strong> <InfoTip id={q.glossary} term={q.label} />: {READING[q.id]}; conceptually close to {q.maps_to}.
              </li>
            ))}
            <li>
              NEIS baseline <InfoTip id="paper_baseline" term="NEIS baseline" />: every legacy index value is the pipeline&apos;s six-stage baseline, as of {manifest.as_of_label}. To try other
              weightings, open a country&apos;s Legacy OSI framework and Explore scenarios.
            </li>
            <li>
              Memos <InfoTip id="memo" term="memo" />: short AI-written briefs for the core members, built from each country&apos;s fact sheet and checked against it.
            </li>
          </ul>
        </details>
        <p className="mt-3 text-sm">
          <Link className="link" to="/countries/about">
            About the country data &rarr;
          </Link>
        </p>
      </section>
    </div>
  );
}
