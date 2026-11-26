// How to use the Explorer /how-to-use (usability pass, 2026-11-25; brief section 10): five practical workflows, a few
// lines each, with links to the pages where each one starts. Operational only: what to open and in what order, no
// method (that is /methods and the glossary). Filed under About; linked from Home and the About page. A shell page:
// no engine, no data.

import { Link } from 'react-router';
import { PageHead } from '../components/ui.tsx';
import { useTitle } from './title.ts';

export interface Workflow {
  id: string;
  title: string;
  /** two or three short sentences: where to start, what to read, where to go next */
  steps: string;
  links: { to: string; label: string }[];
}

export const HOW_TO_LEDE =
  'Five common tasks and where to start each one. Every page leads with its conclusion; the evidence and the detailed data sit below it.';

export const HOW_TO_WORKFLOWS: readonly Workflow[] = [
  {
    id: 'sector',
    title: 'Understand a sector',
    steps:
      'Start with a technology and look across its whole supply chain: where supply is concentrated and who holds the strongest positions. Then open one supply-chain step to see who leads it and why it matters.',
    links: [
      { to: '/sector', label: 'All sectors' },
      { to: '/sector/concentration', label: 'Where are the chokepoints?' },
      { to: '/sector/minerals', label: 'Minerals' },
    ],
  },
  {
    id: 'country',
    title: 'Develop a country strategy',
    steps:
      'Open a country to see what it should secure, build, preserve, rely on partners for or leave open at each supply-chain step, with the reasons and how confident the conclusion is. Then check the evidence sections below it.',
    links: [{ to: '/countries', label: 'All countries' }],
  },
  {
    id: 'partnership',
    title: 'Assess a partnership',
    steps:
      'Pick two countries to see where their roles complement each other and what specific problem working together could solve: securing supply, opening a market or building new capacity. A friendly relationship alone is not the case.',
    links: [
      { to: '/aib/partnerships', label: 'Partnerships' },
      { to: '/aib/agreements', label: 'Agreements between two countries' },
    ],
  },
  {
    id: 'compact',
    title: 'Design a compact',
    steps:
      'Start from a supply-chain bottleneck. See the smallest useful group of partners, what each contributes, and whether trade, investment, finance or demand action closes the remaining gap.',
    links: [
      { to: '/compacts', label: 'Sectoral Compacts' },
      { to: '/aib', label: 'Where does coordination change the outcome?' },
      { to: '/aib/roles', label: 'Country roles' },
    ],
  },
  {
    id: 'research',
    title: 'Answer a specific research question',
    steps:
      'Use the conclusion pages and the decision briefs first: where a country has one, Download decision brief on its page gives a short brief as PDF or Word, with evidence, sources and caveats in its appendix. Then use the Advanced explorer to examine one dataset across countries, technologies and years.',
    links: [
      { to: '/countries', label: 'Decision briefs (pick a country)' },
      { to: '/sector/explore', label: 'Advanced explorer' },
      { to: '/data', label: 'Data & sources' },
    ],
  },
];

export default function HowToUseView() {
  useTitle('How to use the Explorer');
  return (
    <div className="mx-auto w-full max-w-7xl px-4 py-8 sm:px-6 lg:px-8">
      <PageHead eyebrow="About" title="How to use the Explorer" lede={HOW_TO_LEDE} />
      <ol className="grid max-w-4xl gap-4" aria-label="Workflows">
        {HOW_TO_WORKFLOWS.map((w, i) => (
          <li key={w.id} id={`how-${w.id}`} className="panel p-5">
            <h2 className="text-lg font-medium text-paper">
              {i + 1}. {w.title}
            </h2>
            <p className="mt-2 max-w-prose text-sm leading-relaxed text-paper-dim">{w.steps}</p>
            <ul className="mt-3 flex flex-wrap gap-x-5 gap-y-1 text-sm">
              {w.links.map((l) => (
                <li key={l.to + l.label}>
                  <Link className="link" to={l.to}>
                    {l.label} &rarr;
                  </Link>
                </li>
              ))}
            </ul>
          </li>
        ))}
      </ol>
      <p className="mt-8 max-w-3xl text-sm text-paper-mute">
        New to the terms? The{' '}
        <Link className="link" to="/glossary">
          glossary
        </Link>{' '}
        says what each one means and why it matters. How the conclusions are reached is in{' '}
        <Link className="link" to="/methods">
          Methods
        </Link>
        .
      </p>
    </div>
  );
}
