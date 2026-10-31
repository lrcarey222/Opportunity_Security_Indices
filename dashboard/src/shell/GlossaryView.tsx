// The shared glossary page /glossary (spec_country.md C3.3; the NEIS copy refresh 2026-10-30): the strategic
// choices, the yards and fences, the Allied Industrial Base concepts, the indices and the data measures in plain
// words, filtered by where they are used (All · Sector · Country · Allied Industrial Base · Methods, the last the
// Methods page and the Legacy OSI views; ?module= keeps the filter in the link). Linked from the footer, every InfoTip ("More in the
// glossary"), Methods, Sector Sources and Country About. A shell page: no engine, no data.

import { useEffect, useMemo } from 'react';
import { useLocation, useSearchParams } from 'react-router';
import { PageHead, Segmented } from '../components/ui.tsx';
import { GlossaryList } from '../components/GlossaryList.tsx';
import { useGlossary } from '../components/glossaryVars.tsx';
import { glossaryFor, type GlossaryModule } from '../content/glossary.ts';
import { useTitle } from './title.ts';

type Filter = GlossaryModule | 'all';

const FILTERS: { value: Filter; label: string }[] = [
  { value: 'all', label: 'All' },
  { value: 'sector', label: 'Sector' },
  { value: 'country', label: 'Country' },
  { value: 'aib', label: 'Allied Industrial Base' },
  { value: 'strategic', label: 'Methods' },
];

const isFilter = (v: string | null): v is Filter => !!v && FILTERS.some((f) => f.value === v);

export default function GlossaryView() {
  useTitle('Glossary');
  const loc = useLocation();
  const [params, setParams] = useSearchParams();
  const { list } = useGlossary();
  const raw = params.get('module');
  const hashId = loc.hash.startsWith('#term-') ? loc.hash.slice(6) : null;
  // a link to one entry shows every module, so the entry is never filtered out
  const wanted: Filter = isFilter(raw) ? raw : 'all';
  const entries = useMemo(() => glossaryFor(wanted, list), [wanted, list]);
  const filter: Filter = hashId && !entries.some((e) => e.id === hashId) ? 'all' : wanted;

  useEffect(() => {
    if (!loc.hash) return;
    const el = document.getElementById(decodeURIComponent(loc.hash.slice(1)));
    if (el && typeof el.scrollIntoView === 'function') el.scrollIntoView({ block: 'start' });
  }, [loc.hash, filter]);

  const n = filter === wanted ? entries.length : list.length;
  return (
    <div className="mx-auto w-full max-w-7xl px-4 py-8 sm:px-6 lg:px-8">
      <PageHead
        eyebrow="About"
        title="Glossary"
        lede="Plain-language definitions of the strategic choices, Allied Industrial Base concepts, indices and data measures used across the Explorer. Each entry says first what a term means, then, where it helps, how it is measured. The info buttons beside a term anywhere in the Explorer open the same short definition."
      />
      <div className="mb-4 flex flex-wrap items-end justify-between gap-4 border-b hairline pb-4">
        <Segmented<Filter>
          label="Show terms used in"
          value={filter}
          options={FILTERS}
          onChange={(v) => {
            const next = new URLSearchParams(params);
            if (v === 'all') next.delete('module');
            else next.set('module', v);
            setParams(next, { replace: true });
          }}
        />
        <p className="text-sm text-paper-mute" aria-live="polite">
          {n} terms
        </p>
      </div>
      <GlossaryList module={filter} current={hashId} />
    </div>
  );
}
