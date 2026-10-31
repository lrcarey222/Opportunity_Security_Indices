// About /about (IA refactor, 2026-10-16; the NEIS copy refresh, 2026-10-30): the secondary tab's landing. It says
// what the Explorer is for first (three questions that are often blurred together), then how it reads a
// capability (the strategy's five steps, the three yards with the user's definitions, the Allied Industrial Base
// in the canonical words), then where the methods, data and glossary are, and finally which results are the
// same for every reader. Every sentence another page also says comes from content/vocabulary.ts. The pages it
// links keep their paths (/methods and /data are the Strategic pages; each module keeps its own sources page).
// A shell page: no engine, no data.

import { Link } from 'react-router';
import { PageHead } from '../components/ui.tsx';
import {
  AIB_EXPLAINER,
  AIB_OUTCOME,
  CONTROL_LAYER_NOTE,
  FRAMING,
  MIXED_YARD_NOTE,
  STRATEGY_EXPLAINER,
  STRATEGY_STEP,
  STRATEGY_STEPS,
  YARD_AND_FENCE,
  YARD_CHOICES,
  YARD_DEFINITION,
  YARDS_NOT_A_SCALE,
} from '../content/vocabulary.ts';
import type { Yard } from '../posture/contract.ts';
import { useTitle } from './title.ts';

interface AboutLink {
  to: string;
  title: string;
  body: string;
}

export const ABOUT_LEDE =
  'The Explorer turns a large body of trade, production, investment, policy and technology data into strategic choices. It separates three questions that are often blurred together: what must be secure, where productive capability is worth building or preserving, and where open access and faster deployment are the better strategy.';

export const ABOUT_MAIN: readonly AboutLink[] = [
  {
    to: '/methods',
    title: 'Methods',
    body: 'How the Explorer moves from evidence to a strategic choice: what the country needs, how valuable the capability is, how strong its current position is, what action follows, and whether policy matches. Legacy OSI methods are kept separately for comparison.',
  },
  {
    to: '/data',
    title: 'Data & sources',
    body: 'The data behind each view, including trade, production, investment, policy, technology and Allied Industrial Base results, with coverage notes and downloadable files.',
  },
  {
    to: '/glossary',
    title: 'Glossary',
    body: 'Plain-language definitions of the strategic choices, Allied Industrial Base concepts, indices and data measures used across the Explorer.',
  },
];

export const ABOUT_SOURCES: readonly AboutLink[] = [
  {
    to: '/sector/sources',
    title: 'Sector sources and downloads',
    body: 'What each sector measure means, where it comes from, where coverage is strong or weak, and the sector data downloads.',
  },
  {
    to: '/countries/about',
    title: 'Country profiles: data notes',
    body: 'How each country comparison is made, the policy memos and the data vintage.',
  },
  {
    to: '/compacts/about',
    title: 'Allied Industrial Base: method and data',
    body: 'How the Allied Industrial Base tests supply disruptions and concentration, identifies trusted alternatives, decides when trade is enough, and finds where new capacity or a Sectoral Compact is needed.',
  },
];

const YARDS: readonly Yard[] = ['small', 'medium', 'large'];

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
      <PageHead eyebrow="About" title="About the Explorer" lede={ABOUT_LEDE} />

      <section aria-labelledby="about-strategy" className="max-w-4xl">
        <h2 id="about-strategy" className="rule-neis-sm text-base font-medium text-paper lg:text-lg">
          From evidence to a choice
        </h2>
        <p className="mt-3 text-sm leading-relaxed text-paper-dim">{STRATEGY_EXPLAINER}</p>
        <ol className="mt-4 grid gap-3 sm:grid-cols-2 lg:grid-cols-5" aria-label="The five steps of the strategy">
          {STRATEGY_STEPS.map((k, i) => (
            <li key={k} className="panel p-4">
              <div className="eyebrow">
                {i + 1}. {STRATEGY_STEP[k].name}
              </div>
              <p className="mt-2 text-sm font-medium leading-snug text-paper">{STRATEGY_STEP[k].question}</p>
              <p className="mt-1 text-xs leading-relaxed text-paper-mute">{STRATEGY_STEP[k].plain}</p>
            </li>
          ))}
        </ol>
      </section>

      <section aria-labelledby="about-yards" className="mt-10">
        <h2 id="about-yards" className="rule-neis-sm text-base font-medium text-paper lg:text-lg">
          Three kinds of capability: yards and fences
        </h2>
        <p className="mt-3 max-w-3xl text-sm leading-relaxed text-paper-dim">
          {FRAMING.whatNotToBuild} {YARDS_NOT_A_SCALE}
        </p>
        <ul className="mt-4 grid gap-4 md:grid-cols-3" aria-label="The three yards">
          {YARDS.map((y) => (
            <li key={y} className="panel p-5" data-yard={y}>
              <h3 className="text-base font-medium text-paper">{YARD_AND_FENCE[y]}</h3>
              <p className="mt-2 text-sm leading-relaxed text-paper-dim">{YARD_DEFINITION[y]}</p>
              <p className="mt-2 text-xs text-paper-mute">Choices: {YARD_CHOICES[y]}</p>
            </li>
          ))}
        </ul>
        <p className="mt-3 max-w-3xl text-sm leading-relaxed text-paper-mute">
          {MIXED_YARD_NOTE} {CONTROL_LAYER_NOTE}
        </p>
      </section>

      <section aria-labelledby="about-aib" className="mt-10 max-w-3xl">
        <h2 id="about-aib" className="rule-neis-sm text-base font-medium text-paper lg:text-lg">
          The Allied Industrial Base
        </h2>
        <p className="mt-3 text-sm leading-relaxed text-paper-dim">{AIB_EXPLAINER}</p>
        <p className="mt-2 text-sm leading-relaxed text-paper-dim">{AIB_OUTCOME}</p>
      </section>

      <section aria-labelledby="about-pages" className="mt-10">
        <h2 id="about-pages" className="rule-neis-sm text-base font-medium text-paper lg:text-lg">
          Methods, data and glossary
        </h2>
        <ul className="mt-4 grid gap-4 md:grid-cols-3">
          {ABOUT_MAIN.map((l) => (
            <Card key={l.to} l={l} level={3} />
          ))}
        </ul>
      </section>
      <section aria-labelledby="about-sources" className="mt-10">
        <h2 id="about-sources" className="rule-neis-sm text-base font-medium text-paper lg:text-lg">
          Sources by part of the Explorer
        </h2>
        <ul className="mt-4 grid gap-4 md:grid-cols-3">
          {ABOUT_SOURCES.map((l) => (
            <Card key={l.to} l={l} level={3} />
          ))}
        </ul>
      </section>
      <section aria-labelledby="about-facts" className="mt-10 max-w-3xl">
        <h2 id="about-facts" className="rule-neis-sm text-base font-medium text-paper lg:text-lg">
          What changes with your perspective, and what does not
        </h2>
        <p className="mt-3 text-sm leading-relaxed text-paper-dim">
          <strong className="text-paper">The evidence</strong> (trade flows, production, concentration, and what the stress tests show allies can and cannot
          replace) is the same for every reader. <strong className="text-paper">The strategic choices</strong> on the Country, Sector and Allied Industrial Base
          pages (Secure, Preserve, Catch Up, Partner, Accelerate or Watch) are set at fixed thresholds and do not move with any reader&rsquo;s preferences.
        </p>
        <p className="mt-2 text-sm leading-relaxed text-paper-mute">
          Only the Legacy OSI framework (the paper baseline&rsquo;s indices, kept for comparison) has perspective dials: how much weight to give security,
          opportunity, home production and climate. They re-rank its priorities and partner scores, starting from the NEIS baseline.
        </p>
      </section>
    </div>
  );
}
