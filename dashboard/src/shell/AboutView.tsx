// About /about (IA refactor, 2026-10-16): the secondary tab's landing. Where the methods, the data and
// sources and the glossary are, and the one distinction the whole explorer keeps: observed or modelled
// facts versus preference-adjusted priorities. The pages it links keep their paths (/methods and
// /data are the Strategic pages; each module keeps its own sources page). A shell page: no engine, no data.

import { Link } from 'react-router';
import { PageHead } from '../components/ui.tsx';
import { useTitle } from './title.ts';

interface AboutLink {
  to: string;
  title: string;
  body: string;
}

export const ABOUT_MAIN: readonly AboutLink[] = [
  {
    to: '/methods',
    title: 'Methods',
    body: 'How Energy Security, Economic Opportunity, the Strategic Index and the partnership measures are built, what the four perspective dials change, and how the allied network is designed, with the formulas.',
  },
  {
    to: '/data',
    title: 'Data & sources',
    body: 'Download the numbers behind the country and partnership views, for the NEIS baseline or your own perspective, and see how this build was checked.',
  },
  {
    to: '/glossary',
    title: 'Glossary',
    body: 'Every index, measure and term in plain words, with how it is built where that helps.',
  },
];

export const ABOUT_SOURCES: readonly AboutLink[] = [
  { to: '/sector/sources', title: 'Sector sources and downloads', body: 'The variable dictionary, every source, coverage and the sector data downloads.' },
  { to: '/countries/about', title: 'Country profiles: data notes', body: 'How each country comparison is made, the policy memos and the data vintage.' },
  { to: '/compacts/about', title: 'Allied Industrial Base: method and data', body: 'The shock, trade and investment model behind partnerships, Sectoral Compacts and country roles.' },
];

function Card({ l, level }: { l: AboutLink; level: 2 | 3 }) {
  const H = level === 2 ? 'h2' : 'h3';
  return (
    <li className="panel flex flex-col p-5">
      <H className="text-lg font-medium text-paper">
        <Link className="link" to={l.to}>
          {l.title}
        </Link>
      </H>
      <p className="mt-2 text-sm leading-relaxed text-paper-dim">{l.body}</p>
    </li>
  );
}

export default function AboutView() {
  useTitle('About');
  return (
    <div className="mx-auto w-full max-w-7xl px-4 py-8 sm:px-6 lg:px-8">
      <PageHead
        eyebrow="About the explorer"
        title="About"
        lede="How the explorer's measures are built, where every number comes from, and what its terms mean."
      />
      <ul className="grid gap-4 md:grid-cols-3">
        {ABOUT_MAIN.map((l) => (
          <Card key={l.to} l={l} level={2} />
        ))}
      </ul>
      <section aria-labelledby="about-sources" className="mt-10">
        <h2 id="about-sources" className="rule-neis-sm text-base font-medium text-paper lg:text-lg">
          Sources by part of the explorer
        </h2>
        <ul className="mt-4 grid gap-4 md:grid-cols-3">
          {ABOUT_SOURCES.map((l) => (
            <Card key={l.to} l={l} level={3} />
          ))}
        </ul>
      </section>
      <section aria-labelledby="about-facts" className="mt-10 max-w-3xl">
        <h2 id="about-facts" className="rule-neis-sm text-base font-medium text-paper lg:text-lg">
          Facts and preferences
        </h2>
        <p className="mt-3 text-sm leading-relaxed text-paper-dim">
          <strong className="text-paper">Observed or modelled facts</strong> (trade flows, production, concentration, how much supply a shock removes, what
          existing and coordinated partners can replace, the gaps that remain) are the same for every reader.{' '}
          <strong className="text-paper">Preference-adjusted priorities</strong> (which sectors a country should put first, how attractive a partner is) depend on how
          much weight you give security, opportunity, home production and climate. The perspective dials on a country profile change only the second kind; every
          page starts from the NEIS baseline.
        </p>
      </section>
    </div>
  );
}
