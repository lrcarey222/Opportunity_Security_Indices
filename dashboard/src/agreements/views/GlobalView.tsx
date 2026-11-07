// The Agreements page without an economy: the world map (partners at depth 3 or more, in the scope and under
// the filters; its table twin lists each economy's numbers), the pair table (the deepest pairs first; sortable
// and searchable) and the network: a depth matrix among a group the reader picks (the core coalition members of
// the Country roster by default; the coalition with its partners; or the 30 economies with the most agreements).
// Filters are checked against each pair's agreements one by one (a pair passes when some agreement matches each);
// the page says so, and names the filters that need an economy.

import { useMemo } from 'react';
import { Link } from 'react-router';
import { ChartCard } from '../../charts/ChartCard.tsx';
import { TableView, type Col } from '../../components/TableView.tsx';
import { Notice, Select } from '../../components/ui.tsx';
import type { AgreementsGlobal, AgreementsManifest, Depth } from '../contract.ts';
import {
  activeKeys, dateKey, dateWords, depthMatrix, depthName, depthOf, DEPTH_FILL, DEPTH_INK, filterPairs, globalPairs, GRADE_WORD, labelOf, listWords, nameOf, orderPairs, PAIR_COLUMN_KEYS, partnersAtDepth, plural, SCOPE_LABEL,
  topEconomies,
  NETWORK_GROUPS, type AgrFilter, type FilterKey, type GlobalPair, type NetworkGroup,
} from '../derive.ts';
import { CountMap, type RosterEntry } from './AgreementsMap.tsx';
import { AbsenceCaveat, DepthChip, DepthLegend, GradeMark, NoneFound, SourceName } from './parts.tsx';

export const NETWORK_GROUP_LABEL: Record<NetworkGroup, string> = {
  core: 'Core coalition members',
  coalition: 'The coalition (core members and partners)',
  top: 'The 30 economies with the most agreements',
};

const FILTER_WORD: Record<FilterKey, string> = {
  country: 'economy', partner: 'partner', scope: 'scope', from: 'years', to: 'years', type: 'type', sector: 'sector', tech: 'technology', segment: 'stage', igroup: 'instrument group',
  inst: 'instrument', legal: 'binding or not', status: 'implementation status', source: 'source', depth: 'depth',
};

/** A row of the map's table twin. */
interface EconomyRow {
  iso3: string;
  name: string;
  /** partners at depth class 3 or more in the scope, under the filters (the map's colour) */
  d3: number;
  partners: number;
  agreements: number;
  ei: number;
}

export interface GlobalProps {
  manifest: AgreementsManifest;
  global: AgreementsGlobal;
  f: AgrFilter;
  roster: readonly (RosterEntry & { tier?: string })[] | null;
  group: NetworkGroup;
  onGroup: (g: NetworkGroup) => void;
  /** a link to the page with an economy (and partner) chosen */
  hrefOf: (country: string, partner?: string | null) => string;
  onSelectCountry: (iso3: string) => void;
}

export function GlobalView({ manifest, global, f, roster, group, onGroup, hrefOf, onSelectCountry }: GlobalProps) {
  const v = manifest.vocab;
  const names = useMemo(() => ({ ...Object.fromEntries((roster ?? []).map((c) => [c.iso3, c.name])), ...manifest.names }), [roster, manifest.names]);
  const name = (iso: string) => nameOf(names, iso);
  const all = useMemo(() => globalPairs(global), [global]);
  const { rows, unsupported, yearsByNewest, perPair } = useMemo(() => filterPairs(all, f, v), [all, f, v]);
  const ordered = useMemo(() => orderPairs(rows, f.scope), [rows, f.scope]);
  const counts = useMemo(() => partnersAtDepth(rows, f.scope), [rows, f.scope]);
  // every partner in the shown pairs (any class): the map keeps the absence caveat to the economies with none
  const partnerCounts = useMemo(() => partnersAtDepth(rows, f.scope, 0), [rows, f.scope]);
  const scopeWords = SCOPE_LABEL[f.scope].toLowerCase();
  const filtered = activeKeys(f).length > 0;
  const withFile = (iso: string) => !!manifest.files.countries?.[iso];

  // the network group: the Country roster's tiers when it is in, else the economies with the most agreements
  const cores = useMemo(() => (roster ?? []).filter((c) => c.tier === 'core').map((c) => c.iso3), [roster]);
  const coalition = useMemo(() => (roster ?? []).filter((c) => c.tier === 'core' || c.tier === 'partner').map((c) => c.iso3), [roster]);
  const effective: NetworkGroup = group !== 'top' && !cores.length ? 'top' : group;
  const members = useMemo(() => {
    const g = effective === 'core' ? cores : effective === 'coalition' ? coalition : topEconomies(global, 30);
    return [...g].sort((a, b) => name(a).localeCompare(name(b), 'en'));
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [effective, cores, coalition, global, names]);
  const matrix = useMemo(() => depthMatrix(rows, members, f.scope), [rows, members, f.scope]);

  // the map's numbers as a table: each economy's partners at class 3 or more (the map's colour), its partners and agreements
  const economies = useMemo<EconomyRow[]>(() => {
    const c = global.countries;
    return c.iso3
      .map((iso, i) => ({ iso3: iso, name: nameOf(names, iso), d3: counts.get(iso) ?? 0, partners: c.n_partners[i], agreements: c.n_agreements[i], ei: c.n_ei_agreements[i] }))
      .sort((x, y) => y.d3 - x.d3 || y.agreements - x.agreements || x.name.localeCompare(y.name, 'en'));
  }, [global, counts, names]);
  const economyCols: Col<EconomyRow>[] = [
    { id: 'name', header: 'Economy', value: (r) => r.name, minWidth: 160 },
    { id: 'd3', header: `Partners at class 3 or more (${scopeWords}${filtered ? ', under the filters' : ''})`, value: (r) => r.d3, num: true },
    { id: 'partners', header: 'Partners with an agreement', value: (r) => r.partners, num: true },
    { id: 'agreements', header: 'Agreements', value: (r) => r.agreements, num: true },
    { id: 'ei', header: 'Electro-industrial agreements', value: (r) => r.ei, num: true },
  ];
  const noneWords = filtered ? 'no agreement that matches the filters' : 'no agreement found';
  const zeroWords = `class 0: ${f.scope === 'ei' ? 'electro-industrial agreements' : 'agreements'}, none counts toward depth`;
  const yearsSet = f.from !== null || f.to !== null;

  const pairHref = (p: GlobalPair): string | null => (withFile(p.a) ? hrefOf(p.a, p.b) : withFile(p.b) ? hrefOf(p.b, p.a) : null);
  const cols: Col<GlobalPair>[] = [
    {
      id: 'pair',
      header: 'Pair',
      value: (p) => `${name(p.a)} and ${name(p.b)}`,
      minWidth: 200,
      cell: (p) => {
        const h = pairHref(p);
        const label = `${name(p.a)} and ${name(p.b)}`;
        return h ? (
          <Link className="link" to={h}>
            {label}
          </Link>
        ) : (
          <span>{label}</span>
        );
      },
    },
    { id: 'depth', header: 'Depth', value: (p) => depthOf(p, f.scope), num: true, cell: (p) => <DepthChip d={depthOf(p, f.scope)} v={v} /> },
    { id: 'n', header: 'Agreements', value: (p) => p.n, num: true },
    { id: 'recent', header: 'In the last five years', value: (p) => p.n_recent, num: true },
    { id: 'newest', header: 'Newest', value: (p) => p.newest ?? '', sortValue: (p) => dateKey(p.newest), cell: (p) => <span className="whitespace-nowrap">{dateWords(p.newest) ?? 'undated'}</span> },
    {
      id: 'ei',
      header: 'Electro-industrial sectors',
      value: (p) => p.ei_sectors.map((s) => `${labelOf(v, 'sectors', s)}${p.ei_grades[s] ? ` (${GRADE_WORD[p.ei_grades[s]]})` : ''}`).join(', '),
      sortable: false,
      minWidth: 160,
      // the grade when the file carries it (established vs mentioned in text: a glyph and a word, never colour alone)
      cell: (p) =>
        p.ei_sectors.length ? (
          <ul className="flex flex-col gap-0.5 text-xs">
            {p.ei_sectors.map((s) => (
              <li key={s}>
                {labelOf(v, 'sectors', s)}
                {p.ei_grades[s] && (
                  <>
                    {' '}
                    <GradeMark grade={p.ei_grades[s]} />
                  </>
                )}
              </li>
            ))}
          </ul>
        ) : (
          <span className="text-paper-mute">none recorded</span>
        ),
    },
    {
      id: 'arch',
      header: 'Architecture',
      value: (p) => [p.has_sp ? 'strategic partnership' : '', p.has_trade ? 'trade agreement' : '', p.has_sectoral ? 'sectoral cooperation' : ''].filter(Boolean).join(', '),
      sortable: false,
      minWidth: 150,
    },
    { id: 'delivery', header: 'With delivery evidence', value: (p) => p.n_delivery, num: true },
    {
      id: 'sources',
      header: 'Sources',
      value: (p) => p.sources.join(' '),
      sortable: false,
      cell: (p) => (
        <span className="text-xs">
          {p.sources.map((s, i) => (
            <span key={s}>
              {i > 0 && ', '}
              <SourceName id={s} v={v} />
            </span>
          ))}
        </span>
      ),
    },
  ];

  return (
    <div className="space-y-8" data-testid="agreements-global">
      {unsupported.length > 0 && (
        <Notice kind="info" title="Some filters apply only with an economy">
          The table of all pairs cannot apply the {listWords([...new Set(unsupported.map((k) => FILTER_WORD[k]))])} filter{unsupported.length === 1 ? '' : 's'}: a single instrument, and a sector outside the
          electro-industrial ones, are read from an economy&rsquo;s agreements
          {unsupported.some((k) => k in PAIR_COLUMN_KEYS) ? '; this export does not list the stages, instrument groups or statuses of each pair' : ''}. Choose an economy to apply{' '}
          {unsupported.length === 1 ? 'it' : 'them'}.
        </Notice>
      )}
      {(perPair.length > 0 || yearsByNewest) && (
        <div className="space-y-1 text-sm text-paper-mute" data-testid="global-filter-note">
          {perPair.length > 0 && (
            <p>
              In the all-economies view each filter is checked on its own against the pair&rsquo;s agreements{f.scope === 'ei' && yearsSet && !yearsByNewest ? ' (the years against its electro-industrial ones)' : ''}: a pair
              passes when some agreement matches each filter, not necessarily the same agreement, and the table counts all its agreements. An exact filter on each agreement needs an economy chosen.
            </p>
          )}
          {yearsByNewest && <p data-testid="years-by-newest">In this view the years filter reads each pair&rsquo;s most recent agreement: this export does not list the years of every agreement of a pair.</p>}
        </div>
      )}
      <ChartCard
        title="Where partnerships run deep"
        sub={`Each economy by its number of partners at depth class 3 (sectoral cooperation) or more: ${scopeWords}${filtered ? ', under the filters' : ''}. Choose an economy for its partners.`}
        notes={<AbsenceCaveat />}
        table={() => <TableView rows={economies} cols={economyCols} caption="Partners at depth class 3 or more, partners and agreements of each economy" pageSize={20} rowKey={(r) => r.iso3} search wrapHeaders />}
      >
        {roster ? (
          <CountMap roster={roster} counts={counts} partners={partnerCounts} scopeWords={scopeWords} onSelect={onSelectCountry} />
        ) : (
          <p className="text-sm text-paper-mute">The map needs the Countries data, which is not available; the table below has every pair.</p>
        )}
      </ChartCard>

      <ChartCard title="The deepest pairs" sub={`${plural(rows.length, 'pair')} with ${scopeWords === 'all partnerships' ? 'an agreement' : 'an electro-industrial agreement'}${filtered ? ' under the filters' : ''}, deepest first. The depth class is a labelled rule, not a score.`} legend={<DepthLegend v={v} />}>
        {ordered.length ? <TableView rows={ordered} cols={cols} caption="Pairs of economies with an agreement" pageSize={15} rowKey={(p) => `${p.a}|${p.b}`} search wrapHeaders /> : <NoneFound>No pair matches these filters.</NoneFound>}
      </ChartCard>

      <ChartCard
        title="The network"
        sub={`The depth class of each pair among ${NETWORK_GROUP_LABEL[effective].toLowerCase()} (${members.length}), ${scopeWords}. A 0 on grey: the pair has agreements, but none counts toward depth (expired, cancelled, or an older signed or announced text). A blank cell: ${noneWords}.`}
        tools={
          <Select
            label="Group"
            value={effective}
            options={NETWORK_GROUPS.map((g) => ({ value: g, label: NETWORK_GROUP_LABEL[g], disabled: g !== 'top' && !cores.length }))}
            onChange={(x) => onGroup(x as NetworkGroup)}
            className="min-w-[16rem]"
          />
        }
        legend={<DepthLegend v={v} />}
        notes={<AbsenceCaveat />}
      >
        {members.length ? (
          <div className="overflow-x-auto" data-testid="network-matrix">
            <table className="border-collapse text-xs">
              <caption className="sr-only">Depth class of each pair of the group</caption>
              <thead>
                <tr>
                  <th scope="col" className="sticky left-0 bg-ink-800 px-1 text-left font-normal text-paper-mute">
                    <span className="sr-only">Economy</span>
                  </th>
                  {members.map((m) => (
                    <th key={m} scope="col" className="h-16 w-6 px-0 align-bottom font-normal text-paper-mute">
                      <span className="inline-block origin-bottom-left translate-x-3 -rotate-60 whitespace-nowrap">
                        <abbr title={name(m)}>{m}</abbr>
                      </span>
                    </th>
                  ))}
                </tr>
              </thead>
              <tbody>
                {members.map((a, i) => (
                  <tr key={a}>
                    <th scope="row" className="sticky left-0 whitespace-nowrap bg-ink-800 pr-2 text-left font-normal text-paper-dim">
                      {name(a)}
                    </th>
                    {members.map((b, j) => {
                      const d = matrix[i][j];
                      if (d === -1) return <td key={b} className="h-6 w-6 bg-ink-900" aria-label={`${name(a)} itself`} />;
                      // no pair row: nothing found (or nothing that matches the filters)
                      if (d === null)
                        return (
                          <td key={b} className="h-6 w-6 border border-ink-800" title={`${name(a)} and ${name(b)}: ${noneWords}`} data-cell="none">
                            <span className="sr-only">{noneWords}</span>
                          </td>
                        );
                      const k = d as Depth;
                      // class 0 of a pair with agreements: a 0 on the zero fill, never a blank
                      const words = d > 0 ? `class ${d}, ${depthName(v, d)}` : zeroWords;
                      return (
                        <td
                          key={b}
                          className="h-6 w-6 border border-ink-800 text-center tabular"
                          style={{ background: DEPTH_FILL[k], color: DEPTH_INK[k] }}
                          title={`${name(a)} and ${name(b)}: ${words}`}
                          data-cell={d > 0 ? 'depth' : 'zero'}
                        >
                          <span aria-hidden="true">{d}</span>
                          <span className="sr-only">{words}</span>
                        </td>
                      );
                    })}
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
        ) : (
          <NoneFound>No economy in this group.</NoneFound>
        )}
      </ChartCard>
    </div>
  );
}
