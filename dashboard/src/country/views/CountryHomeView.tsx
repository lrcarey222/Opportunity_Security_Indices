// The Country Insights landing /countries (spec_country.md A4.1): the country picker, the core members
// (with their corner counts and the MEMO tag), the coalition partners, the other economies (closed
// <details>), a world map coloured by coalition tier (lazy atlas; omitted below 768 px) and "How to
// read Country Insights".

import { lazy, Suspense, useMemo, useState } from 'react';
import { Link, useNavigate } from 'react-router';
import { CountrySelect } from '../../components/CountrySelect.tsx';
import { InfoTip } from '../../components/InfoTip.tsx';
import { Loading, PageHead } from '../../components/ui.tsx';
import { useMediaQuery } from '../../shell/useMediaQuery.ts';
import { useTitle } from '../../shell/title.ts';
import { PAGE, useCountryCtx } from '../CountryLayout.tsx';
import type { CountryEntry } from '../contract.ts';
import { countryOptions, quadrantLine, rosterGroups } from '../derive/model.ts';
import { countryHref } from '../url.ts';

const TierMap = lazy(() => import('../charts/TierMap.tsx'));

function CountryCard({ c, memo }: { c: CountryEntry; memo: boolean }) {
  const q = quadrantLine(c.quadrants);
  return (
    <li>
      <Link to={countryHref(c.iso3)} className="card block h-full p-4 hover:border-signal">
        <span className="flex items-start justify-between gap-2">
          <span className="font-medium text-paper">{c.name}</span>
          {memo && c.memo && !c.memo.stale && <span className="tag py-0 text-[10px]">MEMO</span>}
        </span>
        <span className="mt-1 block text-xs text-paper-mute">
          {c.status === 'ok' && q ? q : c.status === 'non_rankable' ? 'Scores shown, not ranked' : 'No index data'}
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
  return (
    <div className={PAGE}>
      <PageHead
        eyebrow="COUNTRIES"
        title="Countries"
        lede={`Pick a country to see its strategic position in one place: where its energy supply chains are secure or exposed, where it has economic openings, which sectors to prioritise and how that compares with its current policies, its trade, investment and development partners, and its role in the Allied Industrial Base. Values are the NEIS baseline (as of ${manifest.as_of_label}); on each profile a Perspective control lets you weigh security, opportunity, policy and climate differently. For the ${manifest.counts.core} core coalition members a short policy memo ${nMemos ? 'is ready to read or download' : nRevised ? 'is being revised' : 'is being prepared'}.`}
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
          The economies the paper&apos;s <span className="whitespace-nowrap">allied coalition <InfoTip id="coalition" term="allied coalition" /></span> is built around. The line under each name counts its placed sectors in each corner.
        </p>
        <ul className="grid gap-3 sm:grid-cols-2 lg:grid-cols-3 xl:grid-cols-4">
          {g.core.map((c) => (
            <CountryCard key={c.iso3} c={c} memo />
          ))}
        </ul>
      </section>

      <section id="partners" aria-labelledby="partners-h" className="mb-10 scroll-mt-16">
        <h2 id="partners-h" className="rule-neis-sm mb-4 text-xl font-medium text-paper">
          Coalition partners ({g.partner.length})
        </h2>
        <ul className="grid gap-3 sm:grid-cols-2 lg:grid-cols-3 xl:grid-cols-4">
          {g.partner.map((c) => (
            <CountryCard key={c.iso3} c={c} memo={false} />
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
        <ul className="mt-3 space-y-2 text-sm text-paper-dim">
          <li>
            <strong className="text-paper">Grow</strong> <InfoTip id="grow" term="Grow" />: high economic opportunity and high energy security, compared with the middle economy in the sector.
          </li>
          <li>
            <strong className="text-paper">Upgrade</strong> <InfoTip id="upgrade" term="Upgrade" />: high economic opportunity but low energy security; building capability there would meet a need and create value.
          </li>
          <li>
            <strong className="text-paper">Secure</strong> <InfoTip id="secure" term="Secure" />: low economic opportunity and low energy security; the sectors where supply leans most on partners.
          </li>
          <li>
            <strong className="text-paper">Watch</strong> <InfoTip id="watch" term="Watch" />: low economic opportunity but high energy security; value may lie elsewhere.
          </li>
          <li>
            Paper baseline <InfoTip id="paper_baseline" term="paper baseline" />: every number is the paper&apos;s own, as of {manifest.as_of_label}. To try other weightings, open a country and change its Perspective.
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
