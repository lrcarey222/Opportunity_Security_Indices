// The Agreements page (/aib/agreements, 2026-11-06; a lazy route chunk of the Allied Industrial Base hub): the
// partnership agreements of docs/partnership_agreements.md, from public/data/agreements/ (scripts/37), which
// this page loads itself (the manifest with no-cache, then the global file or one economy's file, lazily).
// Three views on one page, by the URL:
//   * no economy                  GlobalView: the world map, the pair table and the network matrix;
//   * ?country=JPN                CountryView: the economy's partners, maps, coverage, recent agreements, gaps;
//   * ?country=JPN&partner=AUS    PairView: the relationship, its architecture, timeline and every agreement.
// URL keys: country, partner, scope (all | ei; absent = ei), the filters (from, to, type, sector, tech,
// segment, igroup, inst, legal, status, source, depth), sort (the partner ranking's key) and group (the
// network's economies). Every other key (the scenario toggle `scn`) is kept. The Country manifest (when it is
// in) gives the map's ISO numeric join, the names and the coalition tiers; without it the maps say so.
// Without the agreements bundle (CI) the page says the data is not in this build; the rest of the explorer
// is unaffected.

import { useMemo } from 'react';
import { Link, useSearchParams } from 'react-router';
import { Term } from '../../components/Term.tsx';
import { FilterRow, Loading, Notice, PageHead, Segmented, Select, type Option } from '../../components/ui.tsx';
import { useMediaQuery } from '../../shell/useMediaQuery.ts';
import { useCountryManifest } from '../../country/data/hooks.ts';
import { useTitle } from '../../shell/title.ts';
import { SEGMENTS, type AgreementsManifest, type AgrVocab } from '../contract.ts';
import { useAgreementsCountry, useAgreementsGlobal, useAgreementsManifest, type AgreementsLoadError } from '../data.ts';
import {
  ABSENCE_CAVEAT, activeKeys, agreementsHref, clearFiltersSearch, countryChoices, dateWords, DEFAULT_SORT, depthName, filterOfSearch, firstYear, globalDates, labelOf, nameOf, networkGroupOf,
  partnerChoices, plural, SCOPE_LABEL, SCOPES, segmentLabel, setFilterSearch, sortKeyOf, techsOf, yearChoices, type AgrFilter, type NetworkGroup, type Scope, type SortKey,
} from '../derive.ts';
import type { RosterEntry } from './AgreementsMap.tsx';
import { CountryView } from './CountryView.tsx';
import { GlobalView } from './GlobalView.tsx';
import { PairView } from './PairView.tsx';
import { EYEBROW, NoneFound, PAGE, SourceName } from './parts.tsx';

export const AGREEMENTS_TITLE = 'Agreements';
export const AGREEMENTS_QUESTION = 'Which agreements already tie economies together, how far do they go, and are they delivered?';

/** The Country roster with its tier (the network's default group) and its map join. */
type Roster = (RosterEntry & { tier?: string })[];

export default function AgreementsView() {
  useTitle(AGREEMENTS_TITLE, 'aib');
  // the narrowing filters are folded on narrow screens (open on wide ones, and whenever one is on)
  const wide = useMediaQuery('(min-width: 1024px)');
  const [params, setParams] = useSearchParams();
  const man = useAgreementsManifest();
  const m = man.data;
  const f = useMemo(() => filterOfSearch(params, m), [params, m]);
  const sort = sortKeyOf(params.get('sort'));
  const group = networkGroupOf(params.get('group'));
  const hasFile = !!(m && f.country && m.files.countries?.[f.country]);
  const glob = useAgreementsGlobal(m && !f.country ? m : null);
  const file = useAgreementsCountry(m, hasFile ? f.country : null);
  const cman = useCountryManifest();
  const roster = useMemo<Roster | null>(() => (cman.data ? cman.data.countries.map((c) => ({ iso3: c.iso3, name: c.name, iso_n3: c.iso_n3 ?? null, tier: c.tier })) : null), [cman.data]);
  const names = useMemo<Record<string, string>>(() => ({ ...Object.fromEntries((roster ?? []).map((c) => [c.iso3, c.name])), ...(m?.names ?? {}) }), [roster, m]);
  // the Years filter's first choice: every agreement year of the global file when it lists them, else each pair's newest
  const globDates = useMemo(() => (glob.data ? globalDates(glob.data) : []), [glob.data]);

  const set = (k: string, val: string) => setParams(setFilterSearch(params, k, val), { replace: true, preventScrollReset: true });
  const setPage = (k: string, val: string, dflt: string) => {
    const q = new URLSearchParams(params);
    if (val && val !== dflt) q.set(k, val);
    else q.delete(k);
    setParams(q, { replace: true, preventScrollReset: true });
  };
  const hrefOf = (country: string | null, partner?: string | null) => agreementsHref(country, partner, params);
  const select = (k: 'country' | 'partner') => (iso: string) => set(k, iso);

  return (
    <div className={PAGE}>
      <PageHead
        eyebrow={EYEBROW}
        title={AGREEMENTS_TITLE}
        lede={
          <>
            {AGREEMENTS_QUESTION} The agreements between economies (treaties, trade agreements, strategic partnerships, memoranda of understanding, joint statements), how deep each
            relationship goes and whether anything is delivered, from several public datasets and official texts.
          </>
        }
      >
        <p className="mt-3 max-w-3xl text-sm text-paper-mute" data-testid="agreements-reading">
          Each pair has a <Term id="partnership_depth" />, a labelled rule over its agreements (never a score). <Term id="electro_industrial_agreement">Electro-industrial</Term> partnerships are
          the default view. An agreement that exists is kept apart from <Term id="delivery_evidence" />, and a code named only in the text of a document is shown as mentioned, not established (
          <Term id="coding_basis" />
          ).
        </p>
      </PageHead>
      {body()}
    </div>
  );

  // plain functions returning elements (not components defined in render: those would remount every render)
  function body() {
    if (man.status === 'loading' || man.status === 'idle') return <Loading label="Loading the agreements" />;
    if (man.error || !man.data) return <LoadProblem error={man.error} retry={man.retry} what="the agreements data" />;
    const manifest = man.data;
    const v = manifest.vocab;
    const partnerOpts: Option[] = file.data ? partnerChoices(file.data, names).map((c) => ({ value: c.iso3, label: c.name })) : [];
    if (f.partner && !partnerOpts.some((o) => o.value === f.partner)) partnerOpts.push({ value: f.partner, label: nameOf(names, f.partner) });
    const dates = file.data ? file.data.agreements.map((a) => a.date) : globDates;
    const years = yearChoices(manifest.as_of, firstYear(dates));
    const active = activeKeys(f);
    const narrowing = active.length;
    return (
      <>
        <FilterRow
          result={
            active.length ? (
              <span>
                {plural(active.length, 'filter')} on ·{' '}
                <button type="button" className="btn-quiet" onClick={() => setParams(clearFiltersSearch(params), { replace: true, preventScrollReset: true })}>
                  Clear filters
                </button>
              </span>
            ) : undefined
          }
        >
          <Select
            label="Economy"
            value={f.country ?? ''}
            options={[{ value: '', label: 'All economies' }, ...countryChoices(manifest).map((c) => ({ value: c.iso3, label: c.name })), ...(f.country && !hasFile ? [{ value: f.country, label: nameOf(names, f.country) }] : [])]}
            onChange={(x) => set('country', x)}
          />
          <Select
            label="Partner"
            value={f.partner ?? ''}
            options={[{ value: '', label: f.country ? 'Every partner' : 'Choose an economy first' }, ...partnerOpts]}
            onChange={(x) => set('partner', x)}
            disabled={!f.country}
            title={f.country ? 'A second economy: the relationship of the pair' : 'Choose an economy first'}
          />
          <Segmented<Scope> label="Scope" value={f.scope} options={SCOPES.map((s) => ({ value: s, label: SCOPE_LABEL[s] }))} onChange={(s) => set('scope', s)} />
          <details className="w-full" open={wide || narrowing > 0 ? true : undefined} data-testid="more-filters">
            <summary className="cursor-pointer text-xs font-medium tracking-wide text-paper-dim uppercase">
              More filters{narrowing ? ` (${narrowing} on)` : ''}
            </summary>
            <div className="mt-3 flex flex-wrap items-end gap-x-4 gap-y-3">
              <NarrowingFilters f={f} v={v} years={years} set={set} />
            </div>
          </details>
        </FilterRow>
        {view(manifest)}
        <SourcesCard manifest={manifest} />
      </>
    );
  }

  function view(manifest: AgreementsManifest) {
    if (!f.country) {
      if (glob.status === 'error') return <LoadProblem error={glob.error} retry={glob.retry} what="the agreements of all economies" />;
      if (!glob.data) return <Loading label="Loading the agreements of all economies" />;
      return (
        <GlobalView
          manifest={manifest}
          global={glob.data}
          f={f}
          roster={roster}
          group={group}
          onGroup={(g: NetworkGroup) => setPage('group', g, 'core')}
          hrefOf={hrefOf}
          onSelectCountry={(iso) => {
            if (manifest.files.countries?.[iso]) select('country')(iso);
          }}
        />
      );
    }
    if (!hasFile) {
      return (
        <section className="card p-5" data-testid="agreements-no-file">
          <h2 className="rule-neis-sm text-base font-medium text-paper lg:text-lg">{nameOf(names, f.country)}</h2>
          <NoneFound>No agreement of {nameOf(names, f.country)} was found in these sources.</NoneFound>
          <p className="text-sm">
            <Link className="link" to={hrefOf(null)}>
              Every economy &rarr;
            </Link>
          </p>
        </section>
      );
    }
    if (file.status === 'error') return <LoadProblem error={file.error} retry={file.retry} what={`the agreements of ${nameOf(names, f.country)}`} />;
    if (!file.data) return <Loading label={`Loading the agreements of ${nameOf(names, f.country)}`} />;
    if (f.partner) return <PairView manifest={manifest} file={file.data} partner={f.partner} f={f} names={names} hrefOf={hrefOf} />;
    return (
      <CountryView
        manifest={manifest}
        file={file.data}
        f={f}
        sort={sort}
        onSort={(s: SortKey) => setPage('sort', s, DEFAULT_SORT)}
        roster={roster}
        names={names}
        hrefOf={hrefOf}
        onSelectPartner={select('partner')}
      />
    );
  }
}

/** The filters that narrow the agreements (each a URL key; unknown values are dropped by filterOfSearch). */
function NarrowingFilters({ f, v, years, set }: { f: AgrFilter; v: AgrVocab; years: number[]; set: (k: string, val: string) => void }) {
  const any = (label: string): Option => ({ value: '', label });
  const ofMap = (map: Record<string, string> | undefined, kind: Parameters<typeof labelOf>[1]): Option[] => Object.keys(map && !Array.isArray(map) ? map : {}).map((id) => ({ value: id, label: labelOf(v, kind, id) }));
  const groupLabel = (g: string) => labelOf(v, 'instrument_groups', g);
  const instruments = v.instruments.filter((i) => !f.igroup || i.group === f.igroup);
  const yearOpts = years.map((y) => ({ value: String(y), label: String(y) }));
  return (
    <>
      <Select label="From" value={f.from === null ? '' : String(f.from)} options={[any('Any year'), ...yearOpts]} onChange={(x) => set('from', x)} className="min-w-[7rem]" />
      <Select label="To" value={f.to === null ? '' : String(f.to)} options={[any('Any year'), ...yearOpts]} onChange={(x) => set('to', x)} className="min-w-[7rem]" />
      <Select label="Agreement type" value={f.type ?? ''} options={[any('Every type'), ...ofMap(v.agreement_types, 'agreement_types')]} onChange={(x) => set('type', x)} />
      <Select
        label="Sector"
        value={f.sector ?? ''}
        options={[any('Every sector'), ...v.sectors.map((s) => ({ value: s.id, label: labelOf(v, 'sectors', s.id), group: s.ei ? 'Electro-industrial sectors' : 'Other sectors' }))]}
        onChange={(x) => set('sector', x)}
      />
      <Select label="Technology" value={f.tech ?? ''} options={[any('Every technology'), ...techsOf(v).map((t) => ({ value: t, label: t }))]} onChange={(x) => set('tech', x)} />
      <Select label="Stage" value={f.segment ?? ''} options={[any('Every stage'), ...SEGMENTS.map((s) => ({ value: s, label: segmentLabel(s) }))]} onChange={(x) => set('segment', x)} className="min-w-[9rem]" />
      <Select label="Instrument group" value={f.igroup ?? ''} options={[any('Every instrument group'), ...ofMap(v.instrument_groups, 'instrument_groups')]} onChange={(x) => set('igroup', x)} />
      <Select
        label="Instrument"
        value={f.inst ?? ''}
        options={[any('Every instrument'), ...instruments.map((i) => ({ value: i.id, label: labelOf(v, 'instruments', i.id), group: groupLabel(i.group) }))]}
        onChange={(x) => set('inst', x)}
      />
      <Select label="Binding or not" value={f.legal ?? ''} options={[any('Binding or not'), ...ofMap(v.legal_statuses, 'legal_statuses')]} onChange={(x) => set('legal', x)} />
      <Select
        label="Implementation status"
        value={f.status ?? ''}
        options={[any('Any status'), { value: 'delivery', label: 'Delivery evidence (implementation underway or operational)' }, ...ofMap(v.implementation_statuses, 'implementation_statuses')]}
        onChange={(x) => set('status', x)}
      />
      <Select label="Source" value={f.source ?? ''} options={[any('Every source'), ...v.sources.map((s) => ({ value: s.id, label: labelOf(v, 'sources', s.id) }))]} onChange={(x) => set('source', x)} />
      <Select
        label="Minimum depth"
        value={f.depth === null ? '' : String(f.depth)}
        options={[any('Any depth'), ...[1, 2, 3, 4, 5].map((d) => ({ value: String(d), label: `Class ${d} (${depthName(v, d)}) or deeper` }))]}
        onChange={(x) => set('depth', x)}
      />
    </>
  );
}

/** A load failure: no bundle in this build, a newer export, or a file that failed (with Retry). */
function LoadProblem({ error, retry, what }: { error: AgreementsLoadError | null; retry: () => void; what: string }) {
  if (error?.code === 'NO_DATA') {
    return (
      <Notice kind="info" title="No agreements data in this build">
        The partnership agreements are not part of this build of the explorer (public/data/agreements/ is written by scripts/37). The rest of the explorer is unaffected.
      </Notice>
    );
  }
  if (error?.code === 'NEWER') {
    return (
      <Notice kind="warning" title="A newer version of the data" action={<button type="button" className="btn-quiet shrink-0" onClick={() => window.location.reload()}>Reload</button>}>
        {error.message}
      </Notice>
    );
  }
  return (
    <Notice kind="error" title={`Could not load ${what}`} action={<button type="button" className="btn-quiet shrink-0" onClick={retry}>Retry</button>}>
      {error?.message ?? 'The file could not be loaded.'}
    </Notice>
  );
}

/** The sources and caveats of the bundle: every dataset with its citation, licence and size, the as-of date. */
function SourcesCard({ manifest }: { manifest: AgreementsManifest }) {
  const v = manifest.vocab;
  return (
    <section className="card mt-8 p-5" aria-labelledby="agr-sources-t" data-testid="agreements-sources">
      <h2 id="agr-sources-t" className="rule-neis-sm text-base font-medium text-paper lg:text-lg">
        Sources and caveats
      </h2>
      <p className="mt-1 text-sm text-paper-mute">
        {plural(manifest.counts.canonical, 'agreement')} from {plural(manifest.counts.records, 'source record')} (records of the same agreement in several sources are linked),{' '}
        {plural(manifest.counts.pairs, 'pair')} of economies; as of {dateWords(manifest.as_of)}.
      </p>
      <ul className="mt-3 space-y-2 text-sm text-paper-dim">
        {v.sources.map((s) => (
          <li key={s.id}>
            <span className="text-paper">
              <SourceName id={s.id} v={v} />
            </span>
            : {labelOf(v, 'sources', s.id)}. {plural(manifest.counts.by_source?.[s.id] ?? s.n_records, 'record')}
            {s.last_updated ? `, updated ${dateWords(s.last_updated)}` : ''}. <span className="text-paper-mute [overflow-wrap:anywhere]">{s.citation}</span> <span className="text-xs text-paper-mute">Licence: {s.licence}.</span>{' '}
            {s.url && (
              <a className="link text-xs" href={s.url} target="_blank" rel="noreferrer noopener">
                Source
              </a>
            )}
          </li>
        ))}
      </ul>
      <ul className="mt-3 list-disc space-y-1 pl-5 text-xs text-paper-mute">
        <li>{ABSENCE_CAVEAT}</li>
        {manifest.caveats.map((c) => (
          <li key={c}>{c}</li>
        ))}
      </ul>
    </section>
  );
}
