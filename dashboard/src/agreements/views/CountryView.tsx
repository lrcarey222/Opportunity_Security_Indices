// The Agreements page with an economy: its top partners ranked by one transparent sort key the reader picks
// (depth, breadth, sector relevance, implementation, recency; no composite score), the partner map (depth
// class of each partner), sector coverage (sectors x partners, established vs mentioned in text), instrument
// coverage (instrument groups x partners), the recent agreements (the last five years), and the strategic gaps
// (gap rows by quadrant with every component, then the execution gaps).

import { useMemo, useState } from 'react';
import { Link } from 'react-router';
import { ChartCard } from '../../charts/ChartCard.tsx';
import { Segmented } from '../../components/ui.tsx';
import type { AgreementsCountry, AgreementsManifest, CodeGrade, InstLevel } from '../contract.ts';
import {
  activeKeys, complementarityWord, dateWords, depthRuleWords, executionGapText, filterAgreements, gapComponents, gapGroups, gapsInScope, gapText, indexCountry, instrumentCoverage, labelOf, nameOf, plural, QUADRANT_NAME, QUADRANT_TITLE,
  rankPartners, recentAgreements, SCOPE_LABEL, sectorCoverage, SORT_INFO, SORT_KEYS, zeroDepthWords, type AgrFilter, type PartnerRank, type SortKey,
} from '../derive.ts';
import { PartnerMap, type RosterEntry } from './AgreementsMap.tsx';
import { AbsenceCaveat, AgreementsTable, DepthChip, DepthLegend, GradeLegend, GradeMark, NoneFound } from './parts.tsx';

const TOP = 25;
const MATRIX_PARTNERS = 12;

export interface CountryProps {
  manifest: AgreementsManifest;
  file: AgreementsCountry;
  f: AgrFilter;
  sort: SortKey;
  onSort: (s: SortKey) => void;
  roster: readonly RosterEntry[] | null;
  names: Readonly<Record<string, string>>;
  hrefOf: (country: string, partner?: string | null) => string;
  onSelectPartner: (iso3: string) => void;
}

export function CountryView({ manifest, file, f, sort, onSort, roster, names, hrefOf, onSelectPartner }: CountryProps) {
  const v = manifest.vocab;
  const iso = file.iso3;
  const name = (x: string) => nameOf(names, x);
  const ranked = useMemo(() => rankPartners(file, f, sort, v, names), [file, f, sort, v, names]);
  const [all, setAll] = useState(false);
  const shown = all ? ranked : ranked.slice(0, TOP);
  const scopeWords = SCOPE_LABEL[f.scope].toLowerCase();
  const ix = useMemo(() => indexCountry(file), [file]);
  const agreements = useMemo(() => filterAgreements(file, f, v, ix), [file, f, v, ix]);
  const recent = useMemo(() => recentAgreements(agreements, manifest.as_of, 5), [agreements, manifest.as_of]);
  const depthMap = useMemo(() => new Map(ranked.map((r) => [r.partner, r.depth])), [ranked]);
  // the partners' agreements in the scope (a class-0 partner with agreements is not "none found")
  const inScope = useMemo(() => new Map(ranked.map((r) => [r.partner, r.nScope])), [ranked]);
  const top = useMemo(() => ranked.slice(0, MATRIX_PARTNERS).map((r) => r.row), [ranked]);
  const filtered = activeKeys(f).length > 0;
  const sectors = useMemo(() => sectorCoverage(top, f.scope, v, names), [top, f.scope, v, names]);
  const insts = useMemo(() => instrumentCoverage(top, v, names, { scope: f.scope, ix }), [top, v, names, f.scope, ix]);
  const groups = useMemo(() => gapGroups(gapsInScope(file.gaps, f.scope, manifest.vocab), null, f.tech), [file.gaps, f.scope, manifest.vocab, f.tech]);
  const execution = useMemo(
    () => gapsInScope(file.execution_gaps, f.scope, manifest.vocab).filter((g) => !f.tech || g.tech === f.tech),
    [file.execution_gaps, f.scope, manifest.vocab, f.tech],
  );
  const depthEi = (p: string) => ix.partner.get(p)?.depth_ei ?? null;
  const gctx = { focal: iso, name, depthEi, compacts: v.compacts };
  // the coverage tables read whole pair profiles: only the scope applies to them
  const coverageNote = "all the pair's agreements; filters other than the scope are not applied";

  return (
    <div className="space-y-8" data-testid="agreements-country">
      <ChartCard
        title={`Who ${name(iso)} works with`}
        sub={`${plural(ranked.length, 'partner')} with ${f.scope === 'ei' ? 'an electro-industrial agreement' : 'an agreement'}${ranked.length ? '' : ' in these sources'}, ranked by ${SORT_INFO[sort].words}. No composite score: pick the order.`}
        tools={<Segmented label="Rank partners by" value={sort} options={SORT_KEYS.map((k) => ({ value: k, label: SORT_INFO[k].label }))} onChange={onSort} />}
        legend={<DepthLegend v={v} />}
        notes={
          <>
            <p>The depth class and the electro-industrial sectors are the pair&rsquo;s, from all its agreements ({scopeWords}); the counts and the newest date are over the agreements that match the filters.</p>
            <AbsenceCaveat className="mt-1" />
          </>
        }
      >
        {ranked.length ? (
          <>
            <div className="overflow-x-auto border hairline">
              <table className="data-table" data-testid="partner-ranking">
                <caption className="sr-only">Partners of {name(iso)} ranked by {SORT_INFO[sort].label.toLowerCase()}</caption>
                <thead>
                  <tr>
                    <th className="num">Rank</th>
                    <th>Partner</th>
                    <th aria-sort={sort === 'depth' ? 'descending' : 'none'}>Depth</th>
                    <th className="num" aria-sort={sort === 'breadth' ? 'descending' : 'none'}>Matching agreements</th>
                    <th className="num" aria-sort={sort === 'sector' ? 'descending' : 'none'}>Electro-industrial sectors established</th>
                    <th className="num" aria-sort={sort === 'implementation' ? 'descending' : 'none'}>Delivery evidence / in force</th>
                    <th aria-sort={sort === 'recency' ? 'descending' : 'none'}>Newest</th>
                    <th>Rule that set the depth</th>
                  </tr>
                </thead>
                <tbody>
                  {shown.map((r, i) => (
                    <PartnerRow
                      key={r.partner}
                      r={r}
                      i={i}
                      manifest={manifest}
                      href={hrefOf(iso, r.partner)}
                      // class 0 without rule words: the pair has agreements in the scope (it is ranked), none of which counts toward depth
                      rule={depthRuleWords(r.depthRule, v, ix.byId) ?? (r.depth === 0 ? zeroDepthWords(r.nScope, f.scope) : null)}
                    />
                  ))}
                </tbody>
              </table>
            </div>
            {ranked.length > TOP && (
              <button type="button" className="btn-quiet mt-2" onClick={() => setAll((x) => !x)}>
                {all ? `Show the first ${TOP}` : `Show all ${ranked.length} partners`}
              </button>
            )}
          </>
        ) : (
          <NoneFound>{name(iso)} has no partner with a matching agreement.</NoneFound>
        )}
      </ChartCard>

      <ChartCard title={`${name(iso)}'s partners by depth`} sub={`The depth class of each partner, ${scopeWords}. Choose a partner for the relationship.`} notes={<AbsenceCaveat />}>
        {roster ? (
          <PartnerMap roster={roster} focal={iso} depth={depthMap} inScope={inScope} scope={f.scope} v={v} scopeWords={scopeWords} filtered={filtered} onSelect={(p) => {
              if (p !== iso && depthMap.has(p)) onSelectPartner(p);
            }}
          />
        ) : (
          <p className="text-sm text-paper-mute">The map needs the Countries data, which is not available; the ranking above lists every partner.</p>
        )}
      </ChartCard>

      <div className="grid gap-6 lg:grid-cols-2">
        <ChartCard
          title="Sector coverage"
          sub={`Sectors the agreements of ${name(iso)} with its first ${Math.min(MATRIX_PARTNERS, ranked.length)} partners cover (in this order), ${scopeWords}: ${coverageNote}.`}
          notes={<GradeLegend />}
        >
          <CoverageMatrix
            rows={sectors.rows}
            cols={sectors.sectors}
            colLabel={(s) => labelOf(v, 'sectors', s)}
            cell={(g) => (g ? <GradeMark grade={g as CodeGrade} short /> : null)}
            caption={`Sectors covered with each partner of ${name(iso)}`}
            what="sector"
          />
        </ChartCard>
        <ChartCard
          title="Instrument coverage"
          sub={`Instrument groups in the agreements of ${name(iso)} with the same partners, ${scopeWords}: ${f.scope === 'ei' ? "the pair's electro-industrial agreements" : "all the pair's agreements"}; filters other than the scope are not applied.`}
          notes={<GradeLegend />}
        >
          <CoverageMatrix
            rows={insts.rows}
            cols={insts.groups}
            colLabel={(g) => labelOf(v, 'instrument_groups', g)}
            cell={(l) => ((l as InstLevel) === 2 ? <GradeMark grade="specific" short /> : (l as InstLevel) === 1 ? <GradeMark grade="text" short /> : null)}
            caption={`Instrument groups with each partner of ${name(iso)}`}
            what="instrument"
          />
        </ChartCard>
      </div>

      <ChartCard title="Recent agreements" sub={`The agreements of ${name(iso)} dated in the five years to ${dateWords(manifest.as_of)}, ${scopeWords}, newest first.`}>
        <AgreementsTable rows={recent} v={v} names={names} focal={iso} caption={`Recent agreements of ${name(iso)}`} />
      </ChartCard>

      <section className="card p-5" aria-labelledby="agr-gaps-t" data-testid="strategic-gaps">
        <h2 id="agr-gaps-t" className="rule-neis-sm text-base font-medium text-paper lg:text-lg">
          Strategic gaps
        </h2>
        <p className="mt-1 text-sm text-paper-mute">
          Where {name(iso)} and a partner complement each other in a technology (one needs supply the other could provide) and whether an agreement covers it. Each row shows the parts
          the label is read from; the label is a rule over them, not a score.{f.tech ? ` ${f.tech} only.` : ''}
        </p>
        {groups.length ? (
          groups.map((g) => (
            <div key={g.quadrant} className="mt-5" data-testid={`gaps-${g.quadrant}`}>
              <h3 className="text-sm font-medium text-paper">
                {QUADRANT_NAME[g.quadrant]}: {QUADRANT_TITLE[g.quadrant].toLowerCase()} ({g.rows.length})
              </h3>
              <ul className="mt-2 space-y-2 text-sm text-paper-dim">
                {g.rows.map((r) => (
                  <li key={`${r.partner}-${r.tech}-${r.side}`} className="border-l-2 border-ink-600 pl-3">
                    <p>{gapText(r, gctx)}</p>
                    <p className="mt-0.5 text-xs text-paper-mute">
                      <Link className="link" to={hrefOf(iso, r.partner)}>
                        {name(r.partner)}: the relationship
                      </Link>
                      {' · '}
                      complementarity {complementarityWord(r)}, architecture {r.architecture}, coverage {r.covered_grade === 'specific' ? 'established' : r.covered_grade === 'text' ? 'mentioned in text' : 'none found'}
                      <span className="sr-only"> ({gapComponents(r, v.compacts)})</span>
                    </p>
                  </li>
                ))}
              </ul>
              {/* an opportunity says no agreement in these sources covers the technology */}
              {g.quadrant === 'opportunity' && <AbsenceCaveat className="mt-2" />}
            </div>
          ))
        ) : (
          <NoneFound>No strategic gap rows for {name(iso)}{f.tech ? ` in ${f.tech}` : ''}.</NoneFound>
        )}
        <div className="mt-6" data-testid="execution-gaps">
          <h3 className="text-sm font-medium text-paper">Execution gaps</h3>
          <p className="mt-1 text-xs text-paper-mute">
            Technologies {name(iso)} prioritises where no agreement in these sources with a capable supplier covers the technology at an established level.
          </p>
          {execution.length ? (
            <>
              <ul className="mt-2 space-y-1.5 text-sm text-paper-dim">
                {execution.map((g) => (
                  <li key={g.tech} className="border-l-2 border-ink-600 pl-3">
                    {executionGapText(g, gctx)}
                  </li>
                ))}
              </ul>
              <AbsenceCaveat className="mt-2" />
            </>
          ) : (
            <p className="mt-2 text-sm text-paper-mute">No execution gap recorded{f.tech ? ` in ${f.tech}` : ''}.</p>
          )}
        </div>
      </section>
    </div>
  );
}

/** Characters of the depth rule shown in the ranking table before it folds (the full rule opens on a click). */
const RULE_PREVIEW = 90;

function PartnerRow({ r, i, manifest, href, rule }: { r: PartnerRank; i: number; manifest: AgreementsManifest; href: string; rule: string | null }) {
  return (
    <tr>
      <td className="num tabular">{i + 1}</td>
      <td>
        <Link className="link" to={href}>
          {r.name}
        </Link>
        {r.viaEuOnly && <span className="block text-xs text-paper-mute">class set by EU agreements</span>}
      </td>
      <td>
        <DepthChip d={r.depth} v={manifest.vocab} />
      </td>
      <td className="num tabular">
        {r.n}
        {r.n !== r.nTotal && <span className="text-paper-mute"> of {r.nTotal}</span>}
      </td>
      <td className="num tabular">{r.eiSpecific}</td>
      <td className="num tabular">
        {r.nDelivery} / {r.nInForce}
      </td>
      <td className="whitespace-nowrap">{dateWords(r.newest) ?? 'undated'}</td>
      <td className="min-w-[14rem] max-w-[24rem] text-xs text-paper-mute">
        {rule && rule.length > RULE_PREVIEW ? (
          <details>
            <summary className="cursor-pointer">{`${rule.slice(0, RULE_PREVIEW).replace(/\s+\S*$/, '')}…`}</summary>
            <p className="mt-1">{rule}</p>
          </details>
        ) : (
          (rule ?? '')
        )}
      </td>
    </tr>
  );
}

/** A partners x codes matrix with glyph cells (never colour alone). */
function CoverageMatrix<C>({ rows, cols, colLabel, cell, caption, what }: { rows: { partner: string; name: string; cells: Record<string, C | null> }[]; cols: string[]; colLabel: (c: string) => string; cell: (x: C | null) => React.ReactNode; caption: string; what: string }) {
  if (!rows.length || !cols.length) return <NoneFound>No {what} coverage recorded for these partners.</NoneFound>;
  return (
    <div className="overflow-x-auto" data-testid={`${what}-coverage`}>
      <table className="data-table">
        <caption className="sr-only">{caption}</caption>
        <thead>
          <tr>
            <th>Partner</th>
            {cols.map((c) => (
              <th key={c} className="text-center" style={{ whiteSpace: 'normal', verticalAlign: 'bottom', minWidth: 72 }}>
                {colLabel(c)}
              </th>
            ))}
          </tr>
        </thead>
        <tbody>
          {rows.map((r) => (
            <tr key={r.partner}>
              <td className="whitespace-nowrap">{r.name}</td>
              {cols.map((c) => (
                <td key={c} className="text-center">
                  {cell(r.cells[c] ?? null)}
                </td>
              ))}
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
}
