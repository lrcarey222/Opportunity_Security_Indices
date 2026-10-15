// Home (spec_sector.md 2.1): what each module is for, the fastest ways in, then out of the way.
// It renders with no data request before idle and never touches the engine:
//   * on idle (1.5 s fallback; skipped under Save-Data) it fetches data/sector/manifest.json (the
//     question facts and the coverage glyph) and prefetches sector-meta;
//   * pointer-enter, focus or touch on the Strategic card prefetches the Strategic chunk and starts
//     the engine worker (prefetch.ts); nothing else does.
// A failed prefetch is silent: the questions show without facts and the glyph is omitted.

import { useEffect, useId, useState } from 'react';
import { Link, useNavigate } from 'react-router';
import { Segmented, Select } from '../components/ui.tsx';
import { HOME_QUESTIONS, questionFact, type Question } from '../content/questions.ts';
import {
  HEADLINE_TECH_SLUGS, MINERAL_SLUGS, STAGE_LABELS, STAGE_SLUGS, TECH_LABELS, TECH_SLUGS,
  type MineralSlug, type SectorManifest, type StageSlug, type TechSlug,
} from '../sector/contract.ts';
import { moduleResumeHref } from './lastLocation.ts';
import { onIdle, prefetchStrategic, saveData } from './prefetch.ts';
import { loadSectorManifest, peekSectorManifest, prefetchSectorChunk } from './sectorManifest.ts';
import { useTitle } from './title.ts';

/** Display names of the mineral pages (the data's dims.items labels are the authority on those pages). */
export const MINERAL_LABELS: Readonly<Record<MineralSlug, string>> = {
  lithium: 'Lithium',
  cobalt: 'Cobalt',
  nickel: 'Nickel',
  graphite: 'Graphite',
  copper: 'Copper',
  manganese: 'Manganese',
  'rare-earths': 'Rare earths',
  zinc: 'Zinc',
  pgms: 'Platinum-group metals',
  silver: 'Silver',
  silicon: 'Silicon',
  chromium: 'Chromium',
  molybdenum: 'Molybdenum',
};

/** Technologies with one stage only: Semiconductors is a Midstream trade basket. */
const ONLY_STAGE: Partial<Record<TechSlug, StageSlug>> = { semiconductors: 'midstream' };

/** The idle prefetch of the Sector manifest (and meta). Null until it arrives; null on failure. */
function useIdleSectorManifest(): SectorManifest | null {
  const [m, setM] = useState<SectorManifest | null>(() => peekSectorManifest());
  useEffect(() => {
    if (m || saveData()) return;
    let live = true;
    const cancel = onIdle(() => {
      loadSectorManifest().then(
        (man) => {
          if (live) setM(man);
          prefetchSectorChunk(man, 'sector-meta').catch(() => undefined);
        },
        () => undefined,
      );
    });
    return () => {
      live = false;
      cancel();
    };
  }, [m]);
  return m;
}

// ---- module cards ------------------------------------------------------------------------------

/** 11 x 3 squares: filled where the cell has a country-level measure (manifest.coverage_glyph). */
export function CoverageGlyph({ glyph }: { glyph: SectorManifest['coverage_glyph'] }) {
  const has = new Set(glyph.filter((g) => g.country_data).map((g) => `${g.tech}|${g.stage}`));
  return (
    <div className="inline-grid grid-cols-3 gap-[3px]" aria-hidden="true" data-testid="coverage-glyph">
      {HEADLINE_TECH_SLUGS.map((t) =>
        STAGE_SLUGS.map((s) => (
          <span key={`${t}-${s}`} className={`block h-2 w-2 border border-ink-500 ${has.has(`${t}|${s}`) ? 'bg-ink-500' : ''}`} />
        )),
      )}
    </div>
  );
}

/** A static glyph of the four priority dials (decorative). */
function DialGlyph() {
  const knobs = [0.5, 0.8, 0.3, 0.6];
  return (
    <svg viewBox="0 0 96 44" className="h-11 w-24" aria-hidden="true">
      {knobs.map((k, i) => (
        <g key={i} transform={`translate(0 ${4 + i * 11})`}>
          <rect x="0" y="2" width="96" height="2" fill="#374151" />
          <rect x="0" y="2" width={96 * k} height="2" fill="#f58229" />
          <rect x={96 * k - 3} y="0" width="6" height="6" fill="#f58229" />
        </g>
      ))}
    </svg>
  );
}

interface CardSpec {
  id: 'sector' | 'strategic' | 'yards';
  tag: string;
  title: string;
  question: string;
  lede: string;
  links: { to: string; label: string }[];
  button: string;
}

const SECTOR_CARD: CardSpec = {
  id: 'sector',
  tag: 'DESCRIPTIVE DATA',
  title: 'Sector Insights',
  question: 'Who produces, holds, trades, invests in and regulates each energy supply chain?',
  lede: "Investment, trade, reserves, production, consumption, prices, policy and development finance for 11 energy technologies from upstream to downstream, each in its source's own units and year.",
  links: [
    { to: '/sector/batteries/midstream', label: 'Batteries — Midstream' },
    { to: '/sector/minerals/lithium', label: 'Who mines and refines lithium?' },
    { to: '/sector/concentration', label: 'Which supply chains are most concentrated?' },
  ],
  button: 'Open Sector Insights',
};

const STRATEGIC_CARD: CardSpec = {
  id: 'strategic',
  tag: 'NORMATIVE PRIORITISATION',
  title: 'Strategic Insights',
  question: 'Which sectors and partners should allies prioritise?',
  lede: 'Energy Security, Economic Opportunity, the Strategic Index, Partnership Strength and the allied network, recomputed in your browser as you move four priority dials: hawkishness, bullishness, populism and greenness. Model outputs for exploration, not forecasts.',
  links: [
    { to: '/overview', label: 'World map of Energy Security' },
    { to: '/strategic/USA', label: 'United States: Strategic Index' },
    { to: '/network/batteries-midstream', label: 'Allied network: Batteries Midstream' },
  ],
  button: 'Open Strategic Insights',
};

const YARDS_CARD: CardSpec = {
  id: 'yards',
  tag: 'DECISION TOOL',
  title: 'Draw the Fence',
  question: 'What does America need to secure, where should it compete, and where is openness an advantage?',
  lede: 'The NEIS Yards & Fences framework as a guided policy lab: diagnose the problem, see the evidence, place the technology or component in a small, medium or large yard, then test policy packages and other assumptions against the diagnosis.',
  links: [
    { to: '/yards-fences/examples', label: 'Grid control, magnets, battery cells, solar modules' },
    { to: '/yards-fences/assess', label: 'Start an assessment' },
    { to: '/yards-fences/methodology', label: 'How the tool decides' },
  ],
  button: 'Open Draw the Fence',
};

/** A static glyph of three yards: small with a high fence, medium, large with a low fence. */
function YardsGlyph() {
  return (
    <svg viewBox="0 0 96 44" className="h-11 w-24" aria-hidden="true">
      <rect x="2" y="30" width="16" height="10" fill="none" stroke="#c96a5a" strokeWidth="2" />
      <path d="M2 30V12M10 30V12M18 30V12M2 16H18" stroke="#c96a5a" strokeWidth="2" />
      <rect x="26" y="26" width="26" height="14" fill="none" stroke="#d4a944" strokeWidth="2" />
      <path d="M26 26V18M34 26V18M44 26V18M52 26V18M26 21H31M37 21H52" stroke="#d4a944" strokeWidth="2" />
      <rect x="60" y="22" width="34" height="18" fill="none" stroke="#4fb3a9" strokeWidth="2" />
      <path d="M60 22V18M72 22V18M84 22V18M94 22V18" stroke="#4fb3a9" strokeWidth="2" />
    </svg>
  );
}

function ModuleCard({ spec, manifest }: { spec: CardSpec; manifest: SectorManifest | null }) {
  const id = useId();
  const strategic = spec.id === 'strategic';
  const prefetch = strategic ? { onPointerEnter: prefetchStrategic, onFocus: prefetchStrategic, onTouchStart: prefetchStrategic } : {};
  const nCells = manifest ? manifest.coverage_glyph.filter((g) => g.country_data).length : 0;
  return (
    <article className="card flex flex-col p-6" aria-labelledby={`${id}-t`} {...prefetch}>
      <div className="flex items-start justify-between gap-4">
        <h2 id={`${id}-t`} className="text-2xl font-medium text-paper">
          {spec.title}
        </h2>
        <span className="tag shrink-0 uppercase tracking-wider">{spec.tag}</span>
      </div>
      <p className="mt-3 text-base text-paper">{spec.question}</p>
      <p className="mt-2 text-sm leading-relaxed text-paper-dim">{spec.lede}</p>
      <div className="mt-5 flex min-h-11 items-center gap-4">
        {spec.id === 'yards' ? (
          <>
            <YardsGlyph />
            <span className="text-xs text-paper-mute">Evidence from the indices, judgments from you, rules in the open</span>
          </>
        ) : strategic ? (
          <>
            <DialGlyph />
            <span className="text-xs text-paper-mute">Paper baseline + your scenario, computed in your browser</span>
          </>
        ) : (
          manifest && (
            <>
              <CoverageGlyph glyph={manifest.coverage_glyph} />
              <span className="text-xs text-paper-mute">
                Country data for {nCells} of {manifest.coverage_glyph.length} technology-stage cells
              </span>
            </>
          )
        )}
      </div>
      <ul className="mt-5 space-y-2 text-sm">
        {spec.links.map((l) => (
          <li key={l.to}>
            <Link className="link" to={l.to}>
              {l.label}
            </Link>
          </li>
        ))}
      </ul>
      <div className="mt-auto pt-6">
        <Link className="btn btn-primary" to={moduleResumeHref(spec.id)}>
          {spec.button}
        </Link>
      </div>
    </article>
  );
}

// ---- questions ---------------------------------------------------------------------------------

export function QuestionCard({ q, manifest }: { q: Question; manifest: SectorManifest | null }) {
  const id = useId();
  const fact = questionFact(q, manifest?.headlines);
  return (
    <Link to={fact?.href ?? q.href} className="card block h-full p-5" aria-labelledby={`${id}-q`} aria-describedby={fact ? `${id}-f` : undefined}>
      <h3 id={`${id}-q`} className="text-lg font-medium text-paper">
        {q.question}
      </h3>
      {/* the fact line keeps its height until the manifest arrives, so nothing shifts */}
      <p id={`${id}-f`} className="mt-2 min-h-[2.75rem] text-sm text-paper-dim">
        {fact && (
          <>
            {fact.text}
            <span className="mt-0.5 block text-xs text-paper-mute">
              {fact.source}, {fact.year}
            </span>
          </>
        )}
      </p>
    </Link>
  );
}

// ---- jump to a sector --------------------------------------------------------------------------

function JumpPicker() {
  const navigate = useNavigate();
  const [tech, setTech] = useState<TechSlug>('batteries');
  const [stage, setStage] = useState<StageSlug>('midstream');
  const [mineral, setMineral] = useState<MineralSlug>('lithium');
  const only = ONLY_STAGE[tech];
  const techOptions = TECH_SLUGS.map((t) => ({ value: t, label: TECH_LABELS[t], group: (HEADLINE_TECH_SLUGS as readonly string[]).includes(t) ? 'Headline technologies' : 'Non-headline' }));
  return (
    <div className="flex flex-wrap items-end gap-x-5 gap-y-4 border-b hairline pb-5">
      <form
        className="flex flex-wrap items-end gap-x-4 gap-y-3"
        aria-label="Jump to a sector"
        onSubmit={(e) => {
          e.preventDefault();
          navigate(`/sector/${tech}/${only ?? stage}`);
        }}
      >
        <Select
          label="Technology"
          value={tech}
          options={techOptions}
          onChange={(v) => {
            const t = v as TechSlug;
            setTech(t);
            const o = ONLY_STAGE[t];
            if (o) setStage(o);
          }}
        />
        <Segmented<StageSlug>
          label="Stage"
          value={only ?? stage}
          options={STAGE_SLUGS.map((s) => ({ value: s, label: STAGE_LABELS[s], disabled: !!only && s !== only, title: only && s !== only ? `${TECH_LABELS[tech]}: ${STAGE_LABELS[only]} only` : undefined }))}
          onChange={setStage}
        />
        <button type="submit" className="btn btn-ghost btn-sm" aria-label={`Go to ${TECH_LABELS[tech]} ${STAGE_LABELS[only ?? stage]}`}>
          Go
        </button>
      </form>
      <form
        className="flex flex-wrap items-end gap-x-4 gap-y-3"
        aria-label="Jump to a mineral"
        onSubmit={(e) => {
          e.preventDefault();
          navigate(`/sector/minerals/${mineral}`);
        }}
      >
        <span className="pb-2 text-sm text-paper-mute">or a mineral</span>
        <Select label="Mineral" value={mineral} options={MINERAL_SLUGS.map((m) => ({ value: m, label: MINERAL_LABELS[m] }))} onChange={(v) => setMineral(v as MineralSlug)} />
        <button type="submit" className="btn btn-ghost btn-sm" aria-label={`Go to ${MINERAL_LABELS[mineral]}`}>
          Go
        </button>
      </form>
    </div>
  );
}

// ---- page --------------------------------------------------------------------------------------

export default function Home() {
  useTitle('');
  const manifest = useIdleSectorManifest();
  return (
    <div className="mx-auto w-full max-w-7xl px-4 py-10 sm:px-6 lg:px-8">
      <header className="mb-10">
        <div className="eyebrow mb-3">NEIS Center · Allied Industrial Base Explorer</div>
        <h1 className="rule-neis text-3xl font-medium text-paper sm:text-[2.25rem] sm:leading-tight">Allied Industrial Base Explorer</h1>
        <p className="mt-3 max-w-3xl text-[0.9375rem] leading-relaxed text-paper-dim">
          Three ways into the data behind <em>Opportunity, Security, and the Allied Industrial Base</em>. Sector Insights shows what the data say about each energy technology&rsquo;s supply chain. Strategic Insights turns the same data into priorities for a coalition, under weights you choose. Draw the Fence helps decide what to do about it: what to secure, where to compete, and where openness is an advantage.
        </p>
      </header>

      <section aria-label="Modules" className="grid gap-6 md:grid-cols-2 xl:grid-cols-3">
        <ModuleCard spec={SECTOR_CARD} manifest={manifest} />
        <ModuleCard spec={STRATEGIC_CARD} manifest={manifest} />
        <ModuleCard spec={YARDS_CARD} manifest={manifest} />
      </section>

      <section aria-labelledby="home-questions" className="mt-12">
        <div className="eyebrow mb-2">Questions</div>
        <h2 id="home-questions" className="rule-neis-sm mb-5 text-xl font-medium text-paper">
          Start with a question
        </h2>
        <ul className="grid gap-4 sm:grid-cols-2 lg:grid-cols-3">
          {HOME_QUESTIONS.map((q) => (
            <li key={q.id}>
              <QuestionCard q={q} manifest={manifest} />
            </li>
          ))}
        </ul>
      </section>

      <section aria-labelledby="home-jump" className="mt-12">
        <h2 id="home-jump" className="rule-neis-sm mb-5 text-xl font-medium text-paper">
          Jump to a sector
        </h2>
        <JumpPicker />
      </section>

      <section aria-labelledby="home-fit" className="panel mt-12 p-5">
        <h2 id="home-fit" className="mb-3 text-base font-medium text-paper">
          How the two fit together
        </h2>
        <div className="grid gap-4 text-sm leading-relaxed text-paper-dim md:grid-cols-2">
          <p>Sector Insights shows measured values in their own units: who holds what, and how concentrated it is. It has no weights and no scores.</p>
          <p>Strategic Insights converts the same inputs into within-sector ranks and weighs them under the priorities you set. It ranks and weighs; Sector Insights shows the measured data those rankings start from.</p>
        </div>
      </section>
    </div>
  );
}
