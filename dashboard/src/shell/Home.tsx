// Home (IA refactor 2026-10-16; before it spec_sector.md 2.1, spec_country.md A2.3): "Explore the
// electro-industrial system" and three entry points (a Sector, a Country, the Allied Industrial Base),
// a quick way straight to a sector, a mineral or a country, and the sector questions below the fold.
// It renders with no data request before idle and never touches the engine:
//   * on idle (1.5 s fallback; skipped under Save-Data) it fetches data/sector/manifest.json (the
//     question facts and the coverage glyph), prefetches sector-meta, and loads the Country manifest
//     and layout chunk (so the Country landing is instant and the country picker fills; never a fact
//     sheet);
//   * pointer-enter, focus or touch on the Country card prefetches the Country chunk and manifest; on
//     the Allied Industrial Base card, its chunk and manifest (never the compacts document).
// A failed prefetch is silent: the questions show without facts, the glyph is omitted and the country
// picker is replaced by a link to all countries.

import { useEffect, useId, useState } from 'react';
import { Link, useNavigate } from 'react-router';
import { CountrySelect, type CountryOption } from '../components/CountrySelect.tsx';
import { Segmented, Select } from '../components/ui.tsx';
import { HOME_QUESTIONS, questionFact, type Question } from '../content/questions.ts';
import {
  HEADLINE_TECH_SLUGS, MINERAL_SLUGS, STAGE_LABELS, STAGE_SLUGS, TECH_LABELS, TECH_SLUGS,
  type MineralSlug, type SectorManifest, type StageSlug, type TechSlug,
} from '../sector/contract.ts';
import { moduleInfo } from './modules.ts';
import { onIdle, prefetchAib, prefetchCountry, saveData } from './prefetch.ts';
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

/** The economies of the Country manifest for the picker (lazy: the Country store and its options
 *  helper are not part of Home's chunk). Rejects when the manifest cannot be read. */
async function loadCountryOptions(): Promise<CountryOption[]> {
  const [{ countryStore }, { countryOptions }] = await Promise.all([import('../country/data/store.ts'), import('../country/derive/model.ts')]);
  return countryOptions(await countryStore().manifest());
}

/** The country picker's options, loaded on idle: undefined until then, null on failure. */
function useIdleCountryOptions(): CountryOption[] | null | undefined {
  const [opts, setOpts] = useState<CountryOption[] | null | undefined>(undefined);
  useEffect(() => {
    if (saveData()) {
      setOpts(null);
      return;
    }
    let live = true;
    const cancel = onIdle(() => {
      loadCountryOptions().then(
        (o) => live && setOpts(o),
        () => live && setOpts(null),
      );
    });
    return () => {
      live = false;
      cancel();
    };
  }, []);
  return opts;
}

/** The idle prefetch of the Sector manifest (and meta). Null until it arrives; null on failure. */
function useIdleSectorManifest(): SectorManifest | null {
  const [m, setM] = useState<SectorManifest | null>(() => peekSectorManifest());
  useEffect(() => {
    if (m || saveData()) return;
    let live = true;
    const cancel = onIdle(() => {
      prefetchCountry();
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

/** A static glyph of the four corners of the Country profile's scatter (decorative): Watch and Grow on
 *  top (higher energy security), Secure and Upgrade below; lower economic opportunity on the left. */
function QuadrantGlyph() {
  const cells: [string, string][] = [
    ['W', 'Watch'],
    ['G', 'Grow'],
    ['S', 'Secure'],
    ['U', 'Upgrade'],
  ];
  return (
    <span className="inline-grid grid-cols-2 gap-[3px]" aria-hidden="true" data-testid="quadrant-glyph">
      {cells.map(([l, t]) => (
        <span key={l} title={t} className="flex h-4 w-4 items-center justify-center border border-ink-500 text-[0.625rem] font-medium leading-none text-paper-mute">
          {l}
        </span>
      ))}
    </span>
  );
}

/** A static glyph of a compact's coverage steps (decorative): the shock, existing relationships, the
 *  trade compact and the trade + investment response, each restoring more of the lost supply. */
function CoverageStepsGlyph() {
  const steps = [0.35, 0.55, 0.8, 0.92];
  return (
    <svg viewBox="0 0 64 28" className="h-7 w-16" aria-hidden="true" data-testid="compacts-glyph">
      <rect x="0" y="1" width="64" height="1" fill="#4b5563" />
      {steps.map((v, i) => (
        <rect key={i} x={i * 16 + 2} y={28 - 26 * v} width="12" height={26 * v} fill={i === 0 ? '#6b7280' : '#1889b0'} />
      ))}
    </svg>
  );
}

type EntryId = 'sector' | 'country' | 'aib';

interface CardSpec {
  id: EntryId;
  title: string;
  /** the question the module answers (brief s.1) */
  question: string;
  lede: string;
  /** a line beside the glyph (the Sector card counts its cells instead) */
  glyphNote: string;
  links: { to: string; label: string }[];
  /** the call to action, to the module's landing page */
  button: string;
}

const SECTOR_CARD: CardSpec = {
  id: 'sector',
  title: 'Explore a Sector',
  question: 'What matters about this sector?',
  lede: 'Understand economic opportunity, energy security, policy intervention, and climate implications across the supply chain.',
  glyphNote: '',
  links: [
    { to: '/sector/solar/midstream', label: 'Solar — Midstream' },
    { to: '/sector/minerals/lithium', label: 'Who mines and refines lithium?' },
    { to: '/sector/concentration', label: 'Which supply chains are most concentrated?' },
  ],
  button: 'Explore sectors',
};

const COUNTRY_CARD: CardSpec = {
  id: 'country',
  title: 'Explore a Country',
  question: "What is this country's strategic position?",
  lede: 'Understand where a country has strategic opportunities, vulnerabilities, policy strengths, partnership potential, and a role in the Allied Industrial Base.',
  glyphNote: 'Strategic position, risks, partners and role, from a perspective you can adjust',
  links: [
    { to: '/countries/JPN', label: 'Japan' },
    { to: '/countries/DEU', label: 'Germany' },
    { to: '/countries/USA/memo', label: 'United States: policy memo' },
  ],
  button: 'Explore countries',
};

const AIB_CARD: CardSpec = {
  id: 'aib',
  title: 'Explore the Allied Industrial Base',
  question: 'Where should international coordination occur, and what should it look like?',
  lede: 'Understand which countries should trade, invest, and develop together—and where sectoral compacts could make supply chains more resilient.',
  glyphNote: 'Partnerships, sectoral compacts and country roles',
  links: [
    { to: '/aib/partnerships', label: 'Who should work with whom?' },
    { to: '/compacts', label: 'Which supply chains justify a sectoral compact?' },
    { to: '/aib/roles', label: 'What role does each country play?' },
  ],
  button: 'Explore AIB',
};

/** The three entry points (brief s.4): a Sector, a Country, the Allied Industrial Base. */
export const MODULE_CARDS: readonly CardSpec[] = [SECTOR_CARD, COUNTRY_CARD, AIB_CARD];

function cardPrefetch(id: EntryId) {
  if (id === 'country') return { onPointerEnter: prefetchCountry, onFocus: prefetchCountry, onTouchStart: prefetchCountry };
  if (id === 'aib') return { onPointerEnter: prefetchAib, onFocus: prefetchAib, onTouchStart: prefetchAib };
  return {};
}

function ModuleCard({ spec, manifest }: { spec: CardSpec; manifest: SectorManifest | null }) {
  const id = useId();
  const prefetch = cardPrefetch(spec.id);
  const info = moduleInfo(spec.id);
  const nCells = manifest ? manifest.coverage_glyph.filter((g) => g.country_data).length : 0;
  return (
    <article className="card flex flex-col p-6" aria-labelledby={`${id}-t`} {...prefetch}>
      {/* the tag sits above the title so the three cards' questions line up */}
      <div className="flex flex-col items-start gap-3">
        <span className="tag uppercase tracking-wider">{info.tag}</span>
        <h2 id={`${id}-t`} className="text-2xl font-medium text-paper">
          {spec.title}
        </h2>
      </div>
      <p className="mt-3 text-base text-paper">{spec.question}</p>
      <p className="mt-2 text-sm leading-relaxed text-paper-dim">{spec.lede}</p>
      <div className="mt-5 flex min-h-11 items-center gap-4">
        {spec.id === 'country' ? (
          <>
            <QuadrantGlyph />
            <span className="text-xs text-paper-mute">{spec.glyphNote}</span>
          </>
        ) : spec.id === 'aib' ? (
          <>
            <CoverageStepsGlyph />
            <span className="text-xs text-paper-mute">{spec.glyphNote}</span>
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
        <Link className="btn btn-primary" to={info.home}>
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
    <div className="flex flex-wrap items-end gap-x-5 gap-y-4">
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

// ---- jump to a country -------------------------------------------------------------------------

function CountryJump({ options }: { options: CountryOption[] | null | undefined }) {
  const navigate = useNavigate();
  const [pick, setPick] = useState('');
  if (options === null) {
    return (
      <p className="pb-2 text-sm">
        <Link className="link" to="/countries">
          All countries
        </Link>
      </p>
    );
  }
  const name = options?.find((o) => o.iso3c === pick)?.country;
  return (
    <form
      className="flex flex-wrap items-end gap-x-4 gap-y-3"
      aria-label="Jump to a country"
      onSubmit={(e) => {
        e.preventDefault();
        if (pick) navigate(`/countries/${pick}`);
      }}
    >
      <CountrySelect label="Country" value={pick} options={options ?? []} onChange={setPick} className="w-64 max-w-full" />
      <button type="submit" className="btn btn-ghost btn-sm" disabled={!pick} aria-label={name ? `Go to ${name}` : 'Go to the country'}>
        Go
      </button>
    </form>
  );
}

// ---- page --------------------------------------------------------------------------------------

export default function Home() {
  useTitle('');
  const manifest = useIdleSectorManifest();
  const countries = useIdleCountryOptions();
  return (
    <div className="mx-auto w-full max-w-7xl px-4 py-10 sm:px-6 lg:px-8">
      <header className="mb-10">
        <div className="eyebrow mb-3">NEIS Center · Allied Industrial Base Explorer</div>
        <h1 className="rule-neis text-3xl font-medium text-paper sm:text-[2.25rem] sm:leading-tight">Explore the electro-industrial system</h1>
        <p className="mt-3 max-w-3xl text-[0.9375rem] leading-relaxed text-paper-dim">
          The data and analysis behind <em>Opportunity, Security, and the Allied Industrial Base</em>, organised around three questions: what matters about a sector, where a country stands, and where allies should work together.
        </p>
      </header>

      <section aria-label="Start here" className="grid gap-6 md:grid-cols-3">
        {MODULE_CARDS.map((c) => (
          <ModuleCard key={c.id} spec={c} manifest={manifest} />
        ))}
      </section>

      <section aria-labelledby="home-jump" className="mt-12">
        <h2 id="home-jump" className="rule-neis-sm mb-5 text-xl font-medium text-paper">
          Go straight to a sector or a country
        </h2>
        <div className="flex flex-wrap items-end gap-x-8 gap-y-5 border-b hairline pb-5">
          <JumpPicker />
          <CountryJump options={countries} />
        </div>
      </section>

      <section aria-labelledby="home-questions" className="mt-12">
        <h2 id="home-questions" className="rule-neis-sm mb-5 text-xl font-medium text-paper">
          Or start with a question about supply chains
        </h2>
        <ul className="grid gap-4 sm:grid-cols-2 lg:grid-cols-3">
          {HOME_QUESTIONS.map((q) => (
            <li key={q.id}>
              <QuestionCard q={q} manifest={manifest} />
            </li>
          ))}
        </ul>
      </section>

      <p className="mt-12 text-sm text-paper-mute">
        New to the terms? The{' '}
        <Link className="link" to="/glossary">
          glossary
        </Link>{' '}
        explains every index, category and measure in plain words; the{' '}
        <Link className="link" to="/about">
          About
        </Link>{' '}
        pages have the methods and the data sources.
      </p>
    </div>
  );
}
