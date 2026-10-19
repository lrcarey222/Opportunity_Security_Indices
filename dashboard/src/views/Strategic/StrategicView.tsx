// View 4: Strategic Index (spec_react.md 8.6; paper section 8). A bar-table of the economy's top ten
// in-sample sectors with the five weighted terms on one shared scale and the paper's values as
// ticks; the weights in use; the allied strategic selection; the excluded sectors. The economy's
// policy basis (IEA PAMS + CAT, or IEA PAMS only) and the endowment screen of the allied picks
// (upstream sectors without recorded reserves or production are flagged and never picked while the
// screen is on) are shown with the index, because the need reading 1 - ES rates such sectors high.

import { useMemo } from 'react';
import { useParams } from 'react-router';
import { ChartCard } from '../../charts/ChartCard.tsx';
import { Legend } from '../../charts/legends.tsx';
import { CountrySelect } from '../../components/CountrySelect.tsx';
import { TableView, type Col } from '../../components/TableView.tsx';
import { Term } from '../../components/Term.tsx';
import { EmptyState, FilterRow, Loading, Notice, PageHead, RankChip, ChangedDot, Tag } from '../../components/ui.tsx';
import { rowsToCsv } from '../../lib/csv.ts';
import { DASH, fmtNum, fmtSigned, tierLabel } from '../../lib/format.ts';
import { useApp, useCore, useScenario } from '../../state/EngineProvider.tsx';
import { useGoto } from '../../state/nav.ts';
import { COMPARE_COLOR, STAGE_COLOR } from '../../theme/palette.ts';
import { CountryInsightsLink, useViewTitle, noteText, numOr, policyBasisShort, policyBasisText, policyComponentName, resourceBaseText, rowsOf, strOr, useCountryOptions, type Row } from '../common.tsx';

// The five parts of the Strategic Index (spec_country.md C3.5 facet labels), each already weighted
const TERMS: { key: string; label: string; title: string }[] = [
  { key: 'contrib_es_need', label: 'Security need', title: 'Security need (1 minus Energy Security: how exposed the economy is in the sector), times its weight' },
  { key: 'contrib_eo', label: 'Economic Opportunity', title: 'Economic Opportunity (how well placed the economy is to create value in the sector), times its weight' },
  { key: 'contrib_policy', label: 'Policy activity', title: "Policy activity, ranked among the economy's own sectors (more policy ranks higher), times its weight" },
  { key: 'contrib_sc', label: 'Stage', title: 'The supply-chain stage weight (midstream highest in the paper), times its weight' },
  { key: 'contrib_ghg', label: 'Technology (low-emission)', title: "The technology's low-emission value, times its weight" },
];

const WEIGHT_LABEL: Record<string, string> = {
  policy: 'Policy activity (IEA records, Climate Action Tracker)',
  economic_opportunity: 'Economic Opportunity',
  energy_security_need: 'Security need (1 minus Energy Security)',
  supply_chain: 'Supply-chain stage',
  technology: 'Technology (low-emission value)',
};

interface SiBarRow {
  sector: string;
  stage: string;
  rank: number | null;
  rank_delta: number | null;
  isNew: boolean;
  si: number | null;
  si_base: number | null;
  terms: { key: string; v: number | null; base: number | null }[];
  /** endowment screen: false = an upstream sector without recorded reserves or production (never an allied pick while the screen is on) */
  resource_base: boolean | null;
  resource_evidence: string | null;
}

/** `embedded` (IA refactor 2026-10-16): inside the Country profile, which owns the title, the head and the country. */
export default function StrategicView({ embedded = false }: { embedded?: boolean } = {}) {
  useViewTitle('Strategic Index', embedded);
  const app = useApp();
  const params = useParams();
  const goto = useGoto();
  const { show, isBaseline } = useScenario();
  const core = useCore();
  const iso = params.iso3 && app.byIso.has(params.iso3) ? params.iso3 : app.defaultCountry;
  const c = app.byIso.get(iso) ?? app.countries[0];
  const options = useCountryOptions((x) => app.scored.some((s) => s.iso3c === x.iso3c));

  const baseBy = useMemo(() => new Map(rowsOf(app.baseline.si.cells).map((r) => [`${r.iso3c}|${r.tech}|${r.supply_chain}`, r])), [app.baseline]);

  const rows = useMemo((): SiBarRow[] => {
    if (!core.data) return [];
    const mine = rowsOf(core.data.compare.si).filter((r) => r.iso3c === iso && r.in_sample === true && numOr(r.rank_in_country) !== null);
    let sel = mine;
    if (show === 'baseline') {
      sel = rowsOf(app.baseline.si.cells).filter((r) => r.iso3c === iso && r.in_sample === true && numOr(r.rank_in_country) !== null);
    }
    return sel
      .sort((a, b) => (a.rank_in_country as number) - (b.rank_in_country as number))
      .slice(0, 10)
      .map((r) => {
        const b = baseBy.get(`${r.iso3c}|${r.tech}|${r.supply_chain}`);
        return {
          sector: String(r.sector),
          stage: String(r.supply_chain),
          rank: numOr(r.rank_in_country),
          rank_delta: show === 'baseline' ? null : numOr(r.rank_delta),
          isNew: show !== 'baseline' && r.in_sample_base !== true,
          si: numOr(r.strategic_index),
          si_base: b ? numOr(b.strategic_index) : null,
          terms: TERMS.map((t) => ({ key: t.key, v: numOr(r[t.key]), base: b ? numOr(b[t.key]) : null })),
          resource_base: typeof r.resource_base === 'boolean' ? r.resource_base : null,
          resource_evidence: strOr(r.resource_evidence),
        };
      });
  }, [core.data, iso, show, app.baseline, baseBy]);

  // The economy's policy basis (dash_policy(): IEA PAMS + CAT, IEA PAMS only, or no Policy index)
  const policyInfo = useMemo(() => {
    if (!core.data) return null;
    const cell = rowsOf(core.data.compare.si).find((r) => r.iso3c === iso);
    const name = strOr(cell?.Country) ?? c.country;
    const el = rowsOf(core.data.policy.eligibility).find((r) => r.Country === name);
    return {
      basis: strOr(el?.policy_basis) ?? strOr(cell?.policy_basis),
      eligible: el ? el.eligible === true : null,
      missingRequired: strOr(el?.missing_required),
    };
  }, [core.data, iso, c.country]);
  const screenOn = core.data ? core.data.params.si.require_resource_base !== false : true;
  const screenPaper = app.urlCtx.defaults.require_resource_base;
  const nScreenedOut = rows.filter((r) => r.resource_base === false).length;

  const maxTerm = Math.max(0.05, ...rows.flatMap((r) => r.terms.flatMap((t) => [t.v ?? 0, t.base ?? 0])));
  const maxSi = Math.max(0.05, ...rows.flatMap((r) => [r.si ?? 0, r.si_base ?? 0]));

  const weights = useMemo(() => {
    if (!core.data) return { si: [], stage: [], policy: [] };
    const mk = (cur: Record<string, number>, base: Record<string, number>, label?: Record<string, string>) =>
      Object.keys(base).map((k) => ({ name: label?.[k] ?? k, scenario: cur[k] ?? null, paper: base[k] ?? null }));
    return {
      si: mk(core.data.si.weights, app.baseline.si.weights, WEIGHT_LABEL),
      stage: mk(core.data.si.stage_weights, app.baseline.si.stage_weights),
      policy: mk(core.data.policy.weights, app.baseline.policy.weights, Object.fromEntries(Object.keys(app.baseline.policy.weights).map((k) => [k, policyComponentName(k)]))),
    };
  }, [core.data, app.baseline]);

  const allied = rowsOf(core.data?.compare.allied);
  const excluded = useMemo(() => {
    if (!core.data) return [];
    const deg = new Map(rowsOf(core.data.si.degenerate).map((r) => [String(r.sector), r]));
    return rowsOf(core.data.si.excluded).map((r) => {
      const d = deg.get(String(r.sector));
      return {
        sector: String(r.sector),
        reason: String(r.reason ?? ''),
        detail: d ? `constant across ${d.n_countries} economies (${d.es_constant ? 'Energy Security' : ''}${d.es_constant && d.eo_constant ? ' and ' : ''}${d.eo_constant ? 'Economic Opportunity' : ''})` : '',
      };
    });
  }, [core.data]);

  const weightCols: Col<{ name: string; scenario: number | null; paper: number | null }>[] = [
    { id: 'name', header: 'Weight', value: (r) => r.name, minWidth: 170 },
    { id: 'scenario', header: 'Scenario', value: (r) => r.scenario, cell: (r) => fmtNum(r.scenario, 3), num: true },
    { id: 'paper', header: 'Paper', value: (r) => r.paper, cell: (r) => fmtNum(r.paper, 3), num: true },
    {
      id: 'chg', header: 'Change', value: (r) => (r.scenario !== null && r.paper !== null ? r.scenario - r.paper : null),
      cell: (r) => (r.scenario !== null && r.paper !== null && Math.abs(r.scenario - r.paper) > 1e-12 ? fmtSigned(r.scenario - r.paper, 3) : 'paper'), num: true,
    },
  ];
  const alliedCols: Col<Row>[] = [
    { id: 'Country', header: 'Member', value: (r) => strOr(r.Country), minWidth: 140 },
    { id: 'tier', header: 'Tier', value: (r) => tierLabel(r.tier) },
    { id: 'sector', header: 'Scenario top sector', value: (r) => strOr(r.sector), minWidth: 170 },
    { id: 'si', header: 'Strategic Index', value: (r) => numOr(r.strategic_index), cell: (r) => fmtNum(r.strategic_index, 3), num: true },
    { id: 'sector_base', header: 'Paper top sector', value: (r) => strOr(r.sector_base), minWidth: 170 },
    { id: 'changed', header: 'Changed', value: (r) => (r.changed === true ? 'changed' : ''), cell: (r) => (r.changed === true ? <ChangedDot /> : '') },
    { id: 'policy_basis', header: 'Policy basis', value: (r) => policyBasisShort(strOr(r.policy_basis)), cell: (r) => policyBasisShort(strOr(r.policy_basis)) ?? DASH, minWidth: 170 },
    {
      id: 'resource_base', header: 'Resource base',
      value: (r) => (r.resource_base === true ? 'yes' : r.resource_base === false ? 'none recorded' : 'not screened'),
      cell: (r) => (r.resource_base === false ? <Tag accent title="No recorded reserves or production of the sector's resources: barred while the endowment screen is on">none recorded</Tag> : resourceBaseText(r.resource_base)),
    },
  ];

  const updating = !core.fresh;
  return (
    <div>
      {!embedded && (<>
      <PageHead
        eyebrow="Where to focus"
        title="Strategic Index"
        lede={
          <>
            The <Term id="si" /> ranks one economy&rsquo;s own sectors by how strongly the data point to prioritising them. It adds five things: economic
            opportunity (30%), <Term id="need">security need</Term>, meaning how exposed the economy is (30%), how active its policy already is (20%), the
            supply-chain stage (10%) and the technology&rsquo;s low-emission value (10%). Those are the paper&rsquo;s weights; the dials move them. Ranks compare
            sectors within an economy, not economies with each other. Because exposure counts in a sector&rsquo;s favour, an upstream sector where the economy has no
            recorded reserves or production can rank high; it is flagged and is never the economy&rsquo;s <Term id="allied_pick">allied pick</Term>.
          </>
        }
      >
        <CountryInsightsLink iso3={iso} country={c.country} />
      </PageHead>
      <FilterRow
        result={
          policyInfo ? (
            <span title="What this economy's Policy activity rests on: IEA Policies and Measures records are required; the Climate Action Tracker rating is added where the Tracker rates the country.">
              Policy activity rests on: {policyInfo.eligible === false ? `nothing (lacks ${policyInfo.missingRequired ?? 'a required component'})` : policyBasisText(policyInfo.basis)}
            </span>
          ) : undefined
        }
      >
        <CountrySelect label="Economy" value={iso} options={options} onChange={(v) => goto(`/strategic/${v}`)} />
      </FilterRow>
      </>)}
      {!core.data ? (
        <Loading label="Computing" />
      ) : (
        <div className="flex flex-col gap-6">
          {core.data.notes.length > 0 && <Notice kind="info">{core.data.notes.map(noteText).join(' ')}</Notice>}
          <ChartCard
            title={`${c.country}: top ten sectors`}
            sub={`${show === 'baseline' ? 'Paper baseline.' : 'Scenario.'} Each row is one of the economy's ten highest-ranked sectors. The five bars show how much each part adds to the sector's Strategic Index, all on one scale: a longer bar adds more${isBaseline ? '' : " (gray tick = the paper's value)"}. The Strategic Index column is their total; the rank chip compares the sector's rank with the paper's. Hover a column name for what it measures.${nScreenedOut ? ` "No resource base" marks an upstream sector without recorded reserves or production: it ranks here, but the endowment screen keeps it out of the allied selection${screenOn ? '' : ' (screen off in this scenario)'}.` : ''}`}
            updating={updating}
            summary={`Bar-table of ${rows.length} sectors with the five parts of the Strategic Index each${nScreenedOut ? `; ${nScreenedOut} without a recorded resource base` : ''}.`}
            legend={
              <Legend
                items={[
                  { label: 'Scenario term', color: COMPARE_COLOR.scenario },
                  { label: "Paper's value (tick)", kind: 'tick', color: COMPARE_COLOR.baseline },
                  ...['Upstream', 'Midstream', 'Downstream'].map((s) => ({ label: `${s} sector`, kind: 'dot' as const, color: STAGE_COLOR[s] })),
                ]}
              />
            }
            table={() => (
              <TableView
                rows={rows}
                caption="Top sectors"
                pageSize={10}
                cols={[
                  { id: 'rank', header: 'Rank', value: (r) => r.rank, num: true },
                  { id: 'sector', header: 'Sector', value: (r) => r.sector, minWidth: 170 },
                  ...TERMS.map((t, i) => ({ id: t.key, header: t.label, title: t.title, value: (r: SiBarRow) => r.terms[i].v, cell: (r: SiBarRow) => fmtNum(r.terms[i].v, 3), num: true })),
                  { id: 'si', header: 'Strategic Index', value: (r) => r.si, cell: (r) => fmtNum(r.si, 3), num: true },
                  { id: 'si_base', header: 'Strategic Index, paper', value: (r) => r.si_base, cell: (r) => fmtNum(r.si_base, 3), num: true },
                  { id: 'rd', header: 'Rank change', value: (r) => r.rank_delta, cell: (r) => <RankChip change={r.rank_delta} isNew={r.isNew} /> },
                  { id: 'rb', header: 'Resource base', value: (r) => resourceBaseText(r.resource_base, r.resource_evidence) },
                ]}
              />
            )}
            csv={() => ({
              name: `strategic-index-${iso}`,
              text: rowsToCsv(rows.map((r) => ({
                rank: r.rank, sector: r.sector, ...Object.fromEntries(r.terms.map((t) => [t.key, t.v])), strategic_index: r.si, strategic_index_base: r.si_base, rank_delta: r.rank_delta,
                resource_base: r.resource_base, resource_evidence: r.resource_evidence, policy_basis: policyInfo?.basis ?? null,
              }))),
            })}
          >
            {rows.length ? (
              <div className="overflow-x-auto" tabIndex={0} role="region" aria-label={`Top sectors of ${c.country} (scrolls sideways on narrow screens)`}>
                <div role="table" aria-label={`Top sectors of ${c.country}`} className="min-w-[820px] text-sm">
                  <div role="row" className="grid grid-cols-[minmax(9rem,1.5fr)_repeat(5,minmax(5rem,1fr))_minmax(6.5rem,1fr)_6.5rem] gap-x-2 border-b hairline pb-2">
                    <span role="columnheader" className="eyebrow">Sector</span>
                    {TERMS.map((t) => (
                      <span role="columnheader" key={t.key} className="eyebrow" title={t.title}>
                        {t.label}
                      </span>
                    ))}
                    <span role="columnheader" className="eyebrow">Strategic Index</span>
                    <span role="columnheader" className="eyebrow">Rank</span>
                  </div>
                  {rows.map((r) => (
                    <div role="row" key={r.sector} className="grid grid-cols-[minmax(9rem,1.5fr)_repeat(5,minmax(5rem,1fr))_minmax(6.5rem,1fr)_6.5rem] items-center gap-x-2 border-b border-white/5 py-2">
                      <span role="cell" className="flex items-center gap-2 text-paper-dim">
                        <span className="inline-block h-2.5 w-2.5 shrink-0 rounded-full" style={{ background: STAGE_COLOR[r.stage] }} aria-hidden="true" />
                        <span>
                          {r.sector.split(' - ')[0]} <span className="text-paper-mute">{r.stage.toLowerCase()}</span>
                          {r.resource_base === false && (
                            <span className="ml-1.5 inline-block align-middle">
                              <Tag accent title="No recorded reserves or production of this sector's resources: not eligible as an allied pick while the endowment screen is on">
                                no resource base
                              </Tag>
                            </span>
                          )}
                        </span>
                      </span>
                      {r.terms.map((t, i) => (
                        <span role="cell" key={t.key} className="relative h-4" title={`${TERMS[i].label}: ${fmtNum(t.v, 3)} (paper ${fmtNum(t.base, 3)})`}>
                          <span className="absolute inset-y-0.5 left-0 bg-[#1889b0]" style={{ width: `${(100 * Math.max(0, t.v ?? 0)) / maxTerm}%` }} />
                          {t.base !== null && !isBaseline && <span className="absolute -inset-y-0.5 w-0.5 bg-[#9ca3af]" style={{ left: `${(100 * Math.max(0, t.base)) / maxTerm}%` }} />}
                          <span className="sr-only">{fmtNum(t.v, 3)}</span>
                        </span>
                      ))}
                      <span role="cell" className="flex items-center gap-2 tabular" title={`Strategic Index ${fmtNum(r.si, 3)} (paper ${fmtNum(r.si_base, 3)})`}>
                        <span className="relative h-3 flex-1">
                          <span className="absolute inset-y-0 left-0 bg-[#43a8d0]" style={{ width: `${(100 * Math.max(0, r.si ?? 0)) / maxSi}%` }} />
                          {r.si_base !== null && !isBaseline && <span className="absolute -inset-y-0.5 w-0.5 bg-[#9ca3af]" style={{ left: `${(100 * r.si_base) / maxSi}%` }} />}
                        </span>
                        <span className="w-10 text-right text-paper">{fmtNum(r.si, 3)}</span>
                      </span>
                      <span role="cell" className="flex items-center gap-1.5 tabular text-paper-dim">
                        {r.rank ?? DASH}
                        {!isBaseline && <RankChip change={r.rank_delta} isNew={r.isNew} />}
                      </span>
                    </div>
                  ))}
                </div>
              </div>
            ) : (
              <EmptyState>No sector of this economy is in the Strategic Index (it needs Energy Security, Economic Opportunity and a Policy activity score).</EmptyState>
            )}
          </ChartCard>
          <div className="grid gap-6 2xl:grid-cols-2">
            <ChartCard title="Weights in use" sub="The weights of the Strategic Index's five parts, the supply-chain stage weights and the weights inside Policy activity, in the scenario against the paper. The dials move them; at 50 they are the paper's." updating={updating}>
              <div className="space-y-4">
                <TableView rows={weights.si} cols={weightCols} caption="Strategic Index weights" />
                <TableView rows={weights.stage} cols={weightCols.map((c0) => (c0.id === 'name' ? { ...c0, header: 'Stage weight' } : c0))} caption="Stage weights" />
                <TableView rows={weights.policy} cols={weightCols.map((c0) => (c0.id === 'name' ? { ...c0, header: 'Policy activity component' } : c0))} caption="Policy activity weights" />
              </div>
            </ChartCard>
            <ChartCard
              title="Allied strategic selection"
              sub={`Each coalition member's top upstream or midstream sector on the Strategic Index (its allied pick), in the scenario against the paper; changed rows are marked. Endowment screen ${screenOn ? 'on' : 'off'}${screenOn === screenPaper ? ' (as in the paper)' : ' (the paper has it ' + (screenPaper ? 'on' : 'off') + ')'}: ${screenOn ? 'an upstream sector qualifies only where the member has recorded reserves or production of its resources (USGS, EI, Red Book).' : 'upstream sectors without a resource base can be picked; they are marked.'} Policy basis: IEA Policies and Measures records plus the Climate Action Tracker where the Tracker rates the member, else IEA records only.`}
              updating={updating}
              csv={() => ({ name: 'allied-selection', text: rowsToCsv(allied as Record<string, unknown>[]) })}
            >
              <TableView rows={allied} cols={alliedCols} caption="Allied strategic selection" search pageSize={25} initialSort={{ id: 'changed', desc: true }} />
            </ChartCard>
          </div>
          <ChartCard title="Excluded sectors" sub="Sectors left out of the Strategic Index for every economy, and why (for example, a sector whose scores do not vary across economies cannot rank one against another)." updating={updating}>
            {excluded.length ? (
              <TableView
                rows={excluded}
                caption="Excluded sectors"
                cols={[
                  { id: 'sector', header: 'Sector', value: (r) => r.sector, minWidth: 180 },
                  { id: 'reason', header: 'Reason', value: (r) => r.reason, minWidth: 220 },
                  { id: 'detail', header: 'Detail', value: (r) => r.detail },
                ]}
              />
            ) : (
              <EmptyState>No sector is excluded.</EmptyState>
            )}
          </ChartCard>
        </div>
      )}
    </div>
  );
}
