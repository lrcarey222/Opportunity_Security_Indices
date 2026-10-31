// /: what the framework is, in one screen, and the fastest ways in. The strategic question comes first, then the
// three yards in the Explorer's words (content/vocabulary.ts YARD_DEFINITION), then how a capability is placed.

import { Link } from 'react-router';
import { useTitle } from '../title.ts';
import { EXAMPLES } from '../content/examples.ts';
import { stateSearch } from '../state/assessment.ts';
import { useYards } from '../state/YardsContext.tsx';
import { YARD_COPY, YardGlyph, yardClass } from '../components/kit.tsx';
import { PAGE } from '../FenceLayout.tsx';
import { MIXED_YARD_NOTE, YARDS_NOT_A_SCALE } from '../../content/vocabulary.ts';

const SEQUENCE = ['Problem', 'Evidence', 'Yard', 'Fence', 'Policy', 'Metrics', 'Reassessment'];

const GATES: { q: string; then: string }[] = [
  { q: 'Could disruption or foreign control cause acute harm?', then: 'Start with the consequence, not the supplier share. If harm would not be acute, there is no high fence.' },
  { q: 'Where does the vulnerability sit?', then: 'A control layer an outsider could command, or a physical input that could be withheld and not replaced in time. Either makes a Small Yard; the fence surrounds the vulnerability, not the whole product.' },
  { q: 'Is this a capability worth building or preserving?', then: 'A present position, industrial learning, or a credible leap ahead: one of these makes a Medium Yard, with gates in the fence.' },
  { q: 'Would protection cost more than it is worth?', then: 'Otherwise the default is the Large Yard: affordable, diversified access, with the lightest tools for real concentration risk.' },
];

export default function LandingView() {
  useTitle('');
  const { state, href } = useYards();
  return (
    <div className={PAGE}>
      <header className="max-w-4xl pt-4">
        <div className="eyebrow mb-3">Yards &amp; Fences — NEIS Center</div>
        <h1 className="rule-neis text-4xl font-medium text-paper sm:text-5xl">Draw the Fence</h1>
        <p className="mt-4 text-xl leading-snug text-paper">What does America need to secure, where should it compete, and where is openness an advantage?</p>
        <p className="mt-3 max-w-3xl text-[0.9375rem] leading-relaxed text-paper-dim">
          Draw the Fence walks through the NEIS Yards &amp; Fences tests one question at a time and shows why each component or layer of a technology lands
          where it does. {MIXED_YARD_NOTE} Measured data from the Opportunity &amp; Security Indices sit beside the questions as context; they never decide the
          answer.
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
            <p className="mt-4 text-sm leading-relaxed text-paper">{YARD_COPY[y].definition}</p>
            <p className="mt-2 text-sm leading-relaxed text-paper-dim">{YARD_COPY[y].question}</p>
          </article>
        ))}
      </section>

      <section className="mt-10 max-w-3xl">
        <p className="block-interpretation text-[0.9375rem] leading-relaxed">
          Different policy problems need different tools. Draw the Fence combines measured evidence with explicit judgments to identify the problem first, and
          only then draw the fence. {YARDS_NOT_A_SCALE} Each answers a different question.
        </p>
      </section>

      <section aria-labelledby="yf-how" className="mt-12">
        <h2 id="yf-how" className="rule-neis-sm mb-5 text-xl font-medium text-paper">
          How a capability is placed
        </h2>
        <ol className="grid gap-3 lg:grid-cols-4">
          {GATES.map((g, i) => (
            <li key={g.q} className="panel relative p-5">
              <div className="eyebrow">Gate {i + 1}</div>
              <p className="mt-2 text-[0.9375rem] font-medium leading-snug text-paper">{g.q}</p>
              <p className="mt-2 text-sm leading-relaxed text-paper-dim">{g.then}</p>
            </li>
          ))}
        </ol>
        <p className="mt-3 max-w-3xl text-sm leading-relaxed text-paper-mute">
          Every rule is explicit and on the Methodology page. There is no score. Unknown is always a valid answer: the tool brackets it rather than guessing, and
          says which missing answer would settle the yard.
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
              <Link to={`/${e.view}${stateSearch(e.state())}`} className="card block h-full p-5">
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
