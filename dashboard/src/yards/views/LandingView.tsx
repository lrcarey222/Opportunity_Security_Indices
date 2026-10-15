// /yards-fences: what the framework is, in one screen, and the fastest ways in.

import { Link } from 'react-router';
import { useTitle } from '../../shell/title.ts';
import { EXAMPLES } from '../content/examples.ts';
import { stateSearch } from '../state/assessment.ts';
import { useYards } from '../state/YardsContext.tsx';
import { YARD_COPY, YardGlyph, yardClass } from '../components/kit.tsx';
import { PAGE } from '../YardsLayout.tsx';

const SEQUENCE = ['Problem', 'Evidence', 'Yard', 'Fence', 'Policy', 'Metrics', 'Reassessment'];

export default function LandingView() {
  useTitle('Draw the Fence', 'Yards & Fences');
  const { state, href } = useYards();
  return (
    <div className={PAGE}>
      <header className="max-w-4xl pt-4">
        <div className="eyebrow mb-3">Yards &amp; Fences — NEIS Center</div>
        <h1 className="rule-neis text-4xl font-medium text-paper sm:text-5xl">Draw the Fence</h1>
        <p className="mt-4 text-xl leading-snug text-paper">What does America need to secure, where should it compete, and where is openness an advantage?</p>
        <p className="mt-3 max-w-3xl text-[0.9375rem] leading-relaxed text-paper-dim">
          An interactive tool for deciding what the United States needs to secure, where it should compete, and where openness is an advantage. The Opportunity &amp; Security Indices tell you what the world looks like; Yards &amp; Fences helps you decide what to do about it.
        </p>
        <div className="mt-6 flex flex-wrap gap-3">
          <Link className="btn btn-primary" to={href('assess', { ...state, step: 0 })}>
            Start an assessment
          </Link>
          <Link className="btn btn-ghost" to={href('examples')}>
            Explore an example
          </Link>
          {state.sel && (
            <Link className="btn btn-quiet" to={href('result')}>
              Continue your assessment
            </Link>
          )}
        </div>
      </header>

      <section aria-label="The three yards" className="mt-12 grid gap-5 md:grid-cols-3">
        {(['small', 'medium', 'large'] as const).map((y) => (
          <article key={y} className={`${yardClass(y)} panel yf-yard-edge p-6`}>
            <YardGlyph yard={y} className="h-12 w-[4.5rem]" />
            <h2 className="mt-4 text-2xl font-medium text-paper">{YARD_COPY[y].verb}</h2>
            <p className="yf-yard-text mt-1 text-sm font-semibold uppercase tracking-[0.12em]">
              {YARD_COPY[y].name} / {YARD_COPY[y].fence}
            </p>
            <p className="mt-4 text-sm leading-relaxed text-paper">{YARD_COPY[y].question}</p>
            <p className="mt-2 text-sm leading-relaxed text-paper-dim">{YARD_COPY[y].objective}</p>
          </article>
        ))}
      </section>

      <section className="mt-10 max-w-3xl">
        <p className="block-interpretation text-[0.9375rem] leading-relaxed">
          Different policy problems require different tools. This decision engine combines real-world evidence with explicit judgments to identify the problem first, and only then draw the fence. The three yards are not low, medium and high scores on one scale: each answers a different question.
        </p>
      </section>

      <section aria-labelledby="yf-seq" className="mt-10">
        <h2 id="yf-seq" className="eyebrow mb-3">
          The sequence
        </h2>
        <ol className="flex flex-wrap items-center gap-2">
          {SEQUENCE.map((s, i) => (
            <li key={s} className="flex items-center gap-2">
              <span className="tag text-sm text-paper">{s}</span>
              {i < SEQUENCE.length - 1 && (
                <span aria-hidden="true" className="text-paper-mute">
                  →
                </span>
              )}
            </li>
          ))}
        </ol>
        <p className="mt-2 text-sm text-paper-mute">Not technology → score → label.</p>
      </section>

      <section aria-labelledby="yf-ex" className="mt-12">
        <h2 id="yf-ex" className="rule-neis-sm mb-5 text-xl font-medium text-paper">
          Explore an example
        </h2>
        <ul className="grid gap-4 sm:grid-cols-2 lg:grid-cols-3">
          {EXAMPLES.map((e) => (
            <li key={e.id}>
              <Link to={`/yards-fences/${e.view}${stateSearch(e.state())}`} className="card block h-full p-5">
                <h3 className="text-lg font-medium text-paper">{e.title}</h3>
                <p className="mt-1 text-sm text-paper-mute">{e.question}</p>
                <p className="mt-3 text-sm leading-relaxed text-paper-dim">{e.shows}</p>
              </Link>
            </li>
          ))}
        </ul>
      </section>
    </div>
  );
}
