// The Country Insights landing /countries (spec_country.md A4.1): the country picker, the core members
// (with their strategy counts and the MEMO tag), the coalition partners, the other economies (closed
// <details>), a world map coloured by coalition tier (lazy atlas; omitted below 768 px) and "How to read a
// country profile". Since 2026-10-29 (design 5, spec XI) the roster line under each name is the six-verb
// count of the strategic position framework ("2 Secure · 5 Preserve · ..."), read from the posture core
// (shared with the profiles, so the first profile opened needs no second fetch), and the reading guide
// leads with Need -> Prize -> Position -> Choice -> Policy Alignment; the four corners are explained as the
// Legacy OSI framework with their conceptual mapping (Grow ~ Preserve, Upgrade ~ Catch Up, Exposed ~
// Partner, Stable ~ Watch).

import { lazy, Suspense, useMemo, useState } from 'react';
import { Link, useNavigate } from 'react-router';
import { CountrySelect } from '../../components/CountrySelect.tsx';
import { InfoTip } from '../../components/InfoTip.tsx';
import { Loading, PageHead } from '../../components/ui.tsx';
import { countPostures, countryCells, strategyCountsText, usePostureCore, VERB_LABEL, VERB_ORDER, VERB_SUBTITLE, type PostureCore } from '../../posture/index.ts';
import { useMediaQuery } from '../../shell/useMediaQuery.ts';
import { useTitle } from '../../shell/title.ts';
import { PAGE, useCountryCtx } from '../CountryLayout.tsx';
import { LEGACY_CORNERS, type CountryEntry, type LegacyCorner } from '../contract.ts';
import { countryOptions, rosterGroups } from '../derive/model.ts';
import { countryHref } from '../url.ts';

const TierMap = lazy(() => import('../charts/TierMap.tsx'));

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
  return strategyCountsText(countPostures(cells)).verbs;
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
          {q ?? (c.status === 'no_data' ? 'No index data' : core ? 'No strategy map' : c.status === 'non_rankable' ? 'Scores shown, not ranked' : '')}
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
        title="Countries"
        lede={`Pick a country to see its strategy in one place: what it needs (supply-chain need, and deployment need at Deployment & Use), where there is a prize worth pursuing, where it is already strong, what it should secure, preserve, catch up in, rely on partners for, deploy faster or deliberately not prioritise, whether its current policy matches, and which allies can meet its needs. Every technology is read at six supply-chain stages, from extraction to recovery. The legacy index values (the NEIS baseline as of ${manifest.as_of_label}, with a Perspective control to weigh them differently) stay at the end of each profile as the Legacy OSI framework. For the ${manifest.counts.core} core coalition members a short policy memo ${nMemos ? 'is ready to read or download' : nRevised ? 'is being revised' : 'is being prepared'}.`}
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
        <CountrySelect label="Choose a country" value={pick} options={opts} onChange={(iso) => setPick(iso)} className="w-80 max-w-full" />
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
          counts its strategic choices across the 66 technology and stage nodes.
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
          <p className="mt-1 text-sm text-paper-mute">Each economy coloured by its place in the allied coalition. Click one to open its profile.</p>
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
        <p className="mt-2 text-sm text-paper-dim">
          A profile reads Need &rarr; Prize &rarr; Position &rarr; Choice &rarr; Policy Alignment for each technology at each of the six supply-chain stages, on its Country Strategy
          Map. Need and the prize triage a node; Competitive Position decides the industrial response; the Secure tests override both; at Deployment &amp; Use deployment need decides.
          Each node takes one of six choices:
        </p>
        <ul className="mt-3 space-y-2 text-sm text-paper-dim" data-testid="reading-verbs">
          {VERB_ORDER.map((v) => (
            <li key={v}>
              <strong className="text-paper">{VERB_LABEL[v]}</strong>: {VERB_SUBTITLE[v].toLowerCase()}.
            </li>
          ))}
        </ul>
        <p className="mt-4 text-sm text-paper-dim">
          The Legacy OSI framework (the paper baseline) sits at the end of each profile under Evidence &amp; Methodology: the same sectors in four corners of Energy Security and Economic
          Opportunity, split at the country&apos;s own median sector. The corners describe; they are not current recommendations.
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
        <p className="mt-3 text-sm">
          <Link className="link" to="/countries/about">
            About the country data &rarr;
          </Link>
        </p>
      </section>
    </div>
  );
}
