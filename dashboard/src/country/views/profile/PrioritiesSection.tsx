// S3 of the Country profile (spec_country.md A4.2): "Which sectors the Strategic Index puts first"
// (the paper's ranking with its five parts as bars), "Do priorities and policy agree?" (the 3 x 3
// matrix of the Strategic Index without its policy term against policy activity, thirds within the
// country, with the Climate Action Tracker and IEA record tiles), "Policy records by technology and
// stage" (IEA Policies and Measures coverage on the three-colour scale) and the user's addition of
// 2026-10-12: the strongest active industrial-policy measures by sector from the latest completed GTA
// New Industrial Policy Observatory run (context; not part of the Policy index).

import { useMemo } from 'react';
import { Link } from 'react-router';
import { ChartCard } from '../../../charts/ChartCard.tsx';
import { TriLegend } from '../../../charts/legends.tsx';
import { triScale } from '../../../charts/scale.ts';
import { InfoTip } from '../../../components/InfoTip.tsx';
import { Term } from '../../../components/Term.tsx';
import { TableView, type Col } from '../../../components/TableView.tsx';
import { Notice, Select, StatTile, Tag } from '../../../components/ui.tsx';
import { HeatTable } from '../../../sector/charts/HeatTable.tsx';
import { cellOf, THIRDS, type CellFacts, type NipoPolicy } from '../../contract.ts';
import { strategicCountryLink } from '../../derive/links.ts';
import {
  coverageRows, matrixCaption, matrixCells, maxPart, nipoCells, nipoOfCell, POLICY_THIRD_LABEL, PRIORITY_THIRD_LABEL, priorityRows,
  SI_PART_GLOSSARY, SI_PART_KEYS, SI_PART_LABEL, SI_PART_SHORT,
} from '../../derive/model.ts';
import { quadrantWord, proseName, poss, cap } from '../../derive/text.ts';
import { csvWithSource, Section, useProfile } from './common.tsx';

export function PrioritiesSection() {
  const { sheet } = useProfile();
  const name = proseName(sheet.meta.country);
  return (
    <Section
      id="priorities"
      title="Priorities and policy"
      intro={
        <>
          The <Term id="si" /> ranks {poss(name)} own sectors by how strongly the data point to prioritising them. This section sets that ranking beside the
          policies {name} already has: its <Term id="pams">International Energy Agency policy records</Term>, its <Term id="cat" /> and, as context, the strongest industrial-policy measures
          recorded by Global Trade Alert.
        </>
      }
    >
      {sheet.meta.has_si ? (
        <>
          <PriorityTable />
          <AgreementMatrix />
        </>
      ) : (
        <Notice kind="info" title="No Strategic Index">
          {cap(name)} has no Policy index (no International Energy Agency Policies and Measures records), so the paper computes no Strategic Index for it. Its policy records and the
          industrial-policy measures below are shown as they are.
        </Notice>
      )}
      <PolicyCoverage />
      <NipoCard />
    </Section>
  );
}

// ---- PriorityTable ---------------------------------------------------------------------------------

function Bar({ v, max, label }: { v: number | null; max: number; label: string }) {
  const w = v === null ? 0 : Math.max(0, Math.min(100, (100 * v) / max));
  return (
    <div className="flex items-center gap-1.5" title={label}>
      <div className="relative h-2.5 w-full min-w-10 bg-ink-700/60" aria-hidden="true">
        <span className="absolute inset-y-0 left-0" style={{ width: `${w}%`, background: '#d1d5db' }} />
      </div>
    </div>
  );
}

function PriorityTable() {
  const { sheet, state, setAll, setCell, cell } = useProfile();
  const name = proseName(sheet.meta.country);
  const all = useMemo(() => priorityRows(sheet), [sheet]);
  const rows = state.all ? all : all.slice(0, 15);
  const max = maxPart(all);
  const near = new Set<string>();
  all.forEach((c, i) => {
    if (c.si.margin_next.v !== null && c.si.margin_next.v < 0.01 && all[i + 1]) {
      near.add(c.key);
      near.add(all[i + 1].key);
    }
  });
  const noBase = (c: CellFacts) => c.stage === 'Upstream' && c.si.pick_exclusion === 'no resource base';
  const twinCols: Col<CellFacts>[] = [
    { id: 'rank', header: 'Rank', value: (c) => c.si.rank.v, num: true },
    { id: 'sector', header: 'Sector', value: (c) => c.label },
    ...SI_PART_KEYS.map((k) => ({ id: k, header: SI_PART_LABEL[k], value: (c: CellFacts) => c.si.parts?.[k]?.v ?? null, cell: (c: CellFacts) => c.si.parts?.[k]?.d ?? '–', num: true })),
    { id: 'si', header: 'Score', value: (c) => c.si.v, cell: (c) => c.si.d, num: true },
    { id: 'margin', header: 'Lead over the next', value: (c) => c.si.margin_next.v, cell: (c) => c.si.margin_next.d, num: true },
    { id: 'records', header: 'Policy records', value: (c) => c.policy?.records_specific.v ?? null, num: true },
    { id: 'third', header: 'Policy third', value: (c) => (c.policy?.third ? POLICY_THIRD_LABEL[c.policy.third] : null) },
  ];
  const csv = () => ({
    name: `${sheet.meta.country} Strategic Index ranking`,
    text: csvWithSource(
      all.map((c) => ({
        rank: c.si.rank.v, sector: c.tech, stage: c.stage, opportunity_part: c.si.parts?.eo.v ?? null, security_need_part: c.si.parts?.need.v ?? null,
        policy_activity_part: c.si.parts?.pol.v ?? null, stage_part: c.si.parts?.stage.v ?? null, technology_part: c.si.parts?.tech.v ?? null,
        strategic_index: c.si.v, lead_over_next: c.si.margin_next.v, corner: c.quadrant ? quadrantWord(sheet, c.quadrant) : null, allied_pick: c.si.pick,
        policy_records_specific: c.policy?.records_specific.v ?? null, policy_third: c.policy?.third ?? null,
      })),
      'Opportunity Security Indices, paper baseline (Strategic Index terms)',
      sheet.meta.as_of,
    ),
  });
  return (
    <ChartCard
      headingLevel={3}
      title="Which sectors the Strategic Index puts first"
      sub={`The Strategic Index ranks ${poss(name)} own sectors. It adds up five things: economic opportunity (30%), security need, meaning how exposed the country is (30%), how much policy it already has in the sector compared with its other sectors (20%), the supply-chain stage (10%; manufacturing counts most, then raw materials, then use) and the technology's low-emission value (10%). The last two are the same for every country. The bars show what each adds. Ranks compare sectors within ${name} only; they say nothing about other countries.`}
      summary={`${poss(name)} ${all.length} sectors in the Strategic Index, ranked; the top one is ${all[0]?.label ?? 'none'}.`}
      table={() => <TableView rows={all} cols={twinCols} caption={`${cap(name)}: Strategic Index parts by sector`} rowKey={(c) => c.key} pageSize={33} />}
      csv={csv}
      notes={near.size ? 'NEAR-TIE: two neighbouring sectors whose scores differ by less than 0.01; small changes in the data or the weights can swap them.' : undefined}
    >
      <div className="relative overflow-x-auto">
        <table className="data-table">
          <caption className="sr-only">{cap(name)}: sectors by Strategic Index rank with the five parts of the score</caption>
          <thead>
            <tr>
              <th scope="col" className="num">
                Rank
              </th>
              <th scope="col" className="min-w-[12rem]">
                Sector
              </th>
              {SI_PART_KEYS.map((k) => (
                <th key={k} scope="col" className="hidden min-w-20 sm:table-cell">
                  {SI_PART_LABEL[k]} <InfoTip id={SI_PART_GLOSSARY[k]} term={SI_PART_LABEL[k]} />
                </th>
              ))}
              <th scope="col" className="num">
                Score
              </th>
              <th scope="col">Policy</th>
            </tr>
          </thead>
          <tbody>
            {rows.map((c) => (
              <tr key={c.key} aria-selected={cell === c.key}>
                <td className="num tabular">{c.si.rank.d}</td>
                <td>
                  <button type="button" className="text-left text-paper hover:underline" onClick={() => setCell(c.key)} aria-label={`${c.tech}, ${c.stage}: Strategic Index rank ${c.si.rank.d}, score ${c.si.d}`}>
                    {c.label}
                  </button>
                  {/* tag words on one quiet line (text, not colour): corner, allied pick, near-tie, no resource base */}
                  <span className="mt-0.5 block text-[10px] font-medium uppercase tracking-wider text-paper-mute">
                    {[
                      c.quadrant ? quadrantWord(sheet, c.quadrant).toUpperCase() : null,
                      c.si.pick ? 'ALLIED PICK' : null,
                      near.has(c.key) ? 'NEAR-TIE' : null,
                      noBase(c) ? 'NO RESOURCE BASE' : null,
                    ]
                      .filter(Boolean)
                      .join(' · ')}
                  </span>
                  <span className="mt-0.5 block text-xs text-paper-mute sm:hidden">
                    {SI_PART_KEYS.map((k) => `${SI_PART_SHORT[k]} ${c.si.parts?.[k]?.d ?? '–'}`).join(' · ')}
                  </span>
                </td>
                {SI_PART_KEYS.map((k) => (
                  <td key={k} className="hidden sm:table-cell">
                    <Bar v={c.si.parts?.[k]?.v ?? null} max={max} label={`${SI_PART_LABEL[k]}: ${c.si.parts?.[k]?.d ?? '–'}`} />
                  </td>
                ))}
                <td className="num tabular text-paper">{c.si.d}</td>
                <td className="min-w-[7rem] text-xs text-paper-dim">
                  {c.policy ? (
                    <>
                      <span className="block whitespace-nowrap">{c.policy.records_specific.d} records</span>
                      {c.policy.third && <span className="block whitespace-nowrap text-paper-mute">{sheet.words?.policy_third?.[c.policy.third] ?? POLICY_THIRD_LABEL[c.policy.third]}</span>}
                    </>
                  ) : (
                    '–'
                  )}
                </td>
              </tr>
            ))}
          </tbody>
        </table>
      </div>
      <div className="mt-3 flex flex-wrap items-center justify-between gap-3 text-sm">
        {all.length > 15 && (
          <button type="button" className="btn-quiet" aria-pressed={state.all} onClick={() => setAll(!state.all)}>
            {state.all ? 'Show the top 15' : `Show all ${all.length}`}
          </button>
        )}
        <Link className="link ml-auto" to={strategicCountryLink(sheet.iso3)}>
          Move the dials for {name}: Strategic Index &rarr;
        </Link>
      </div>
    </ChartCard>
  );
}

// ---- AgreementMatrix ------------------------------------------------------------------------------

function AgreementMatrix() {
  const { sheet, cell, setCell } = useProfile();
  const name = proseName(sheet.meta.country);
  const ag = sheet.policy.agreement;
  const cat = sheet.policy.cat;
  const grid = useMemo(() => matrixCells(sheet), [sheet]);
  const insample = useMemo(() => priorityRows(sheet), [sheet]);
  const twin: Col<CellFacts>[] = [
    { id: 'sector', header: 'Sector', value: (c) => c.label },
    { id: 'prank', header: 'Data-priority rank', value: (c) => c.priority?.rank.v ?? null, cell: (c) => c.priority?.rank.d ?? '–', num: true },
    { id: 'pthird', header: 'Data-priority third', value: (c) => (c.priority?.third ? PRIORITY_THIRD_LABEL[c.priority.third] : null) },
    { id: 'polrank', header: 'Policy-activity rank', value: (c) => c.policy?.rank.v ?? null, cell: (c) => c.policy?.rank.d ?? '–', num: true },
    { id: 'polthird', header: 'Policy-activity third', value: (c) => (c.policy?.third ? POLICY_THIRD_LABEL[c.policy.third] : null) },
    { id: 'class', header: 'Class', value: (c) => (c.policy?.class ? (sheet.words?.class?.[c.policy.class] ?? c.policy.class) : null) },
    { id: 'rs', header: 'Records (this sector)', value: (c) => c.policy?.records_specific.v ?? null, num: true },
    { id: 'rt', header: 'Records (technology)', value: (c) => c.policy?.records_tech.v ?? null, num: true },
    { id: 'ra', header: 'Records (all)', value: (c) => c.policy?.records_all.v ?? null, num: true },
    { id: 'ly', header: 'Latest record year', value: (c) => c.policy?.latest_year.v ?? null, cell: (c) => c.policy?.latest_year.d ?? '–' },
    { id: 'pi', header: 'Policy activity (index)', value: (c) => c.policy?.index.v ?? null, cell: (c) => c.policy?.index.d ?? '–', num: true },
  ];
  const csv = () => ({
    name: `${sheet.meta.country} priorities and policy`,
    text: csvWithSource(
      insample.map((c) => ({
        sector: c.tech, stage: c.stage, priority_without_policy: c.priority?.v.v ?? null, data_priority_rank: c.priority?.rank.v ?? null,
        data_priority_third: c.priority?.third ?? null, policy_activity_rank: c.policy?.rank.v ?? null, policy_activity_third: c.policy?.third ?? null,
        class: c.policy?.class ?? null, records_specific: c.policy?.records_specific.v ?? null, records_tech: c.policy?.records_tech.v ?? null,
        records_all: c.policy?.records_all.v ?? null, latest_record_year: c.policy?.latest_year.v ?? null, policy_index: c.policy?.index.v ?? null,
        policy_basis: sheet.meta.policy_basis,
      })),
      'Opportunity Security Indices, paper baseline; International Energy Agency, Policies and Measures; Climate Action Tracker',
      sheet.meta.as_of,
    ),
  });
  return (
    <ChartCard
      headingLevel={3}
      title="Do priorities and policy agree?"
      sub={`To compare like with like, sectors are ranked here on the Strategic Index without its policy term (so policy is not counted twice), and separately on policy activity, ranked within ${name}. Policy activity combines International Energy Agency Policies and Measures records with, where it rates ${name}, the Climate Action Tracker rating read against the technology: a strong climate rating counts for low-emission technologies and against high-emission ones such as coal, oil and gas. Each ranking is cut into thirds. Sectors on the diagonal are where the data-based priority and current policy line up.`}
      summary={`${cap(name)}: sectors placed by data-based priority third and policy-activity third.`}
      table={() => <TableView rows={insample} cols={twin} caption={`${cap(name)}: priority and policy thirds by sector`} rowKey={(c) => c.key} pageSize={33} />}
      csv={csv}
      notes="Policy activity counts and rates policies; it does not measure whether they work."
      badge={<InfoTip id="priority_no_policy" term="the priority without policy" />}
    >
      {!ag || !ag.variation ? (
        <Notice kind="info">Policy activity is the same in every sector of {name}, so the comparison is not shown.</Notice>
      ) : (
        <div className="relative overflow-x-auto">
          <table className="w-full min-w-[36rem] border-collapse text-sm">
            <caption className="sr-only">Data-based priority (rows) against policy activity (columns), thirds within {name}</caption>
            <thead>
              <tr>
                <th scope="col" className="w-36 pb-2 text-left align-bottom">
                  <span className="eyebrow">Priority ↓ · Policy →</span>
                </th>
                {THIRDS.map((q) => (
                  <th key={q} scope="col" className="px-2 pb-2 text-left align-bottom">
                    <span className="eyebrow">{POLICY_THIRD_LABEL[q]}</span>
                  </th>
                ))}
              </tr>
            </thead>
            <tbody>
              {THIRDS.map((p) => (
                <tr key={p}>
                  <th scope="row" className="py-2 pr-2 text-left align-top">
                    <span className="eyebrow">{PRIORITY_THIRD_LABEL[p]}</span>
                  </th>
                  {grid
                    .filter((g) => g.p === p)
                    .map((g) => (
                      <td key={g.key} className={`border p-2 align-top ${g.diagonal ? 'border-paper-mute' : 'border-ink-700'}`}>
                        <span className="label mb-1.5">{matrixCaption(g.p, g.q)}</span>
                        <div className="flex flex-wrap gap-1.5">
                          {g.cells.map((c) => (
                            <button
                              key={c.key}
                              type="button"
                              className="tag"
                              aria-pressed={cell === c.key}
                              aria-label={`${c.tech}, ${c.stage}: ${PRIORITY_THIRD_LABEL[g.p].toLowerCase()} of priority, ${POLICY_THIRD_LABEL[g.q].toLowerCase()} policy`}
                              onClick={() => setCell(c.key)}
                            >
                              {c.label}
                            </button>
                          ))}
                          {!g.cells.length && <span className="text-xs text-paper-mute">none</span>}
                        </div>
                      </td>
                    ))}
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      )}
      <div className="mt-4 grid gap-3 sm:grid-cols-2">
        <StatTile
          label="CLIMATE ACTION TRACKER"
          value={cat ? `${cat.overall}${cat.date_label ? ` (${cat.date_label})` : ''}` : 'Not rated'}
          sub={
            cat
              ? cat.scope === 'eu_bloc'
                ? "The EU's rating, applied to its member states"
                : `${poss(name)} own rating`
              : `Not rated by the Climate Action Tracker; policy activity rests on International Energy Agency records only`
          }
        />
        <StatTile
          label="INTERNATIONAL ENERGY AGENCY POLICY RECORDS"
          value={`${sheet.policy.pams_total.d} records`}
          sub={`${sheet.policy.pams_tech_specific.d} tied to a technology; ${sheet.policy.pams_no_tech.d} name no technology${sheet.policy.pams_eu_records ? `; the EU's own ${sheet.policy.pams_eu_records.d} are not added` : ''}`}
        />
      </div>
    </ChartCard>
  );
}

// ---- PolicyCoverage ------------------------------------------------------------------------------

function PolicyCoverage() {
  const { sheet, cell, setCell } = useProfile();
  const name = proseName(sheet.meta.country);
  const rows = useMemo(() => coverageRows(sheet), [sheet]);
  const values = rows.flatMap((r) => r.cells.map((c) => c.v));
  const scale = useMemo(() => triScale(values), [values.join(',')]); // eslint-disable-line react-hooks/exhaustive-deps
  const byKey = new Map(rows.flatMap((r) => r.cells.map((c) => [`${r.slug}|${c.stage.toLowerCase()}`, { r, c }] as const)));
  const cm = sheet.policy.coverage.critical_minerals ?? [];
  const cmLine = cm.length ? sheet.policy.coverage.cols.map((st, j) => `${st} ${cm[j]?.d ?? '–'}`).join(' · ') : null;
  const csv = () => ({
    name: `${sheet.meta.country} policy records by technology and stage`,
    text: csvWithSource(
      rows.flatMap((r) =>
        r.cells.map((c) => {
          const cf = cellOf(sheet, c.key);
          return { technology: r.tech, stage: c.stage, records_specific: c.v, records_tech: r.any.v, records_all: cf?.policy?.records_all.v ?? null, latest_year: cf?.policy?.latest_year.v ?? null };
        }),
      ),
      'International Energy Agency, Policies and Measures',
      sheet.meta.as_of,
    ),
  });
  type TwinRow = { tech: string; stage: string; specific: string; any: string; all: string; latest: string };
  const twinRows: TwinRow[] = rows.flatMap((r) =>
    r.cells.map((c) => {
      const cf = cellOf(sheet, c.key);
      return { tech: r.tech, stage: c.stage, specific: c.d, any: r.any.d, all: cf?.policy?.records_all.d ?? '–', latest: cf?.policy?.latest_year.d ?? '–' };
    }),
  );
  return (
    <ChartCard
      headingLevel={3}
      title="Policy records by technology and stage"
      sub={`How many of ${poss(name)} International Energy Agency Policies and Measures records concern each technology at each stage. A record that names no technology counts in every technology and is shown separately. Colour: orange fewer records, white the middle sector of ${poss(name)} 33, blue more.`}
      summary={`${cap(name)}: International Energy Agency Policies and Measures records by technology and stage.`}
      table={() => (
        <TableView<TwinRow>
          rows={twinRows}
          cols={[
            { id: 'tech', header: 'Technology', value: (r) => r.tech },
            { id: 'stage', header: 'Stage', value: (r) => r.stage },
            { id: 'specific', header: 'Records for this stage', value: (r) => r.specific, num: true },
            { id: 'any', header: 'Records for the technology', value: (r) => r.any, num: true },
            { id: 'all', header: 'Records incl. those naming no technology', value: (r) => r.all, num: true },
            { id: 'latest', header: 'Latest record year', value: (r) => r.latest },
          ]}
          caption={`${cap(name)}: policy records by technology and stage`}
          pageSize={33}
        />
      )}
      csv={csv}
      legend={<TriLegend scale={scale} label="Records for the stage" format={(v) => String(Math.round(v))} noun="sectors" />}
      notes={
        <>
          Records naming no technology: {sheet.policy.pams_no_tech.d} (they count in every technology). {cmLine ? `Critical-minerals records (a tag outside the 33 sectors): ${cmLine}.` : ''}
        </>
      }
    >
      <HeatTable
        rows={rows.map((r) => ({ key: r.slug, label: r.tech, sub: `any stage: ${r.any.d}` }))}
        cols={sheet.policy.coverage.cols.map((st) => ({ key: st.toLowerCase(), label: st, stage: st.toLowerCase() as 'upstream' | 'midstream' | 'downstream' }))}
        cell={(rk, ck) => {
          const hit = byKey.get(`${rk}|${ck}`);
          const k = `${rk}-${ck}`;
          return {
            value: hit?.c.v ?? null,
            display: hit?.c.d ?? '',
            title: hit ? `${hit.r.tech}, ${hit.c.stage}: ${hit.c.d} records specific to this stage (${hit.r.any.d} for the technology at any stage)` : undefined,
            selected: cell === k,
          };
        }}
        onCell={(rk, ck) => setCell(`${rk}-${ck}`)}
        scale={scale}
        caption={`${cap(name)}: International Energy Agency policy records by technology and stage`}
        rowHeader="Technology"
      />
    </ChartCard>
  );
}

// ---- NIPO measures --------------------------------------------------------------------------------

function NipoItem({ p }: { p: NipoPolicy }) {
  return (
    <li className="border-t hairline py-2.5 first:border-t-0">
      <div className="text-sm text-paper">
        {p.url ? (
          <a className="link" href={p.url} target="_blank" rel="noopener noreferrer">
            {p.title}
          </a>
        ) : (
          p.title
        )}
      </div>
      <div className="mt-0.5 flex flex-wrap gap-x-4 gap-y-0.5 text-xs text-paper-mute">
        <span>
          Strength <span className="tabular text-paper-dim">{p.strength.d}</span>
          {p.strength_base.v !== null && p.strength_base.v !== p.strength.v ? <span> ({p.strength_base.d} before counting its package)</span> : null}
        </span>
        <span>
          Mapping confidence <span className="tabular text-paper-dim">{p.confidence.d}</span>
        </span>
        {(p.scope === 'eu' || p.scope === 'multi') && (
          <span>
            <span className="tag py-0 text-[10px]">{p.scope === 'eu' ? 'EU-WIDE' : 'SHARED'}</span>{' '}
            {p.scope === 'eu' ? 'an EU measure' : 'a joint measure'}, recorded under {p.n_jurisdictions?.d ?? 'several'} jurisdictions
          </span>
        )}
        {p.economy_wide && (
          <span>
            <span className="tag py-0 text-[10px]">ECONOMY-WIDE</span> economy-wide measure (maps to {p.techs.length} technologies)
          </span>
        )}
        {p.in_force === false || !p.date ? (
          <span>
            <span className="tag py-0 text-[10px]">NOT YET IN FORCE</span> {p.date ? `implementation ${p.date_label}` : 'announced; no implementation date recorded'}
          </span>
        ) : (
          <span>In force since {p.date_label}</span>
        )}
        {p.removal && <span>Scheduled to end {p.removal_label ?? p.removal}</span>}
      </div>
    </li>
  );
}

function NipoCard() {
  const { sheet, cell, setCell } = useProfile();
  const name = proseName(sheet.meta.country);
  const n = sheet.policy.nipo;
  const withMeasures = useMemo(() => nipoCells(sheet), [sheet]);
  const current = withMeasures.find((c) => c.key === cell) ?? withMeasures[0] ?? null;
  const list = nipoOfCell(sheet, current?.key);
  const vintage = n?.vintage_label ?? 'Global Trade Alert, New Industrial Policy Observatory';
  const strictRule = n?.active_rule === 'in_force';
  const sub = `The five strongest active industrial-policy measures in each sector, from the ${vintage}. Sector-specific measures come first; economy-wide ones (sanctions, export controls and other measures that map to almost every technology) follow and are tagged. Shown as context: these measures are not part of the Policy index, which counts International Energy Agency Policies and Measures records and the Climate Action Tracker rating.`;
  // EU-wide and shared measures (2026-10-14): Global Trade Alert records an EU measure under every member it covers
  const scopeWord = (sc: NipoPolicy['scope']) => (sc ? (sheet.words?.nipo_scope?.[sc] ?? sc) : '');
  const nShared = n?.n_listed_shared?.v ?? 0;
  const nEu = n?.n_listed_eu?.v ?? 0;
  const sharedLine =
    n && nShared > 0
      ? `${n.n_listed_shared!.d} of the ${n.n_listed?.d ?? ''} measures listed for ${name} ${nShared === 1 ? 'is' : 'are'} ${
          nEu === nShared ? 'EU-wide' : nEu > 0 ? `EU-wide or shared with other jurisdictions (${n.n_listed_eu!.d} EU-wide)` : 'shared with other jurisdictions'
        }: Global Trade Alert records such a measure under every jurisdiction it covers, so it is not ${poss(name)} national policy alone.${
          n.n_active_eu && (n.n_active_eu.v ?? 0) > 0 ? ` Of all ${n.n_active.d} active measures, ${n.n_active_eu.d} are EU-wide.` : ''
        }`
      : null;
  type Row = { sector: string; rank: number; title: string; strength: string; base: string; confidence: string; date: string; status: string; scope: string; url: string };
  const twinRows: Row[] = n
    ? withMeasures.flatMap((c) => nipoOfCell(sheet, c.key).map((p, i) => ({ sector: c.label, rank: i + 1, title: p.title, strength: p.strength.d, base: p.strength_base.d, confidence: p.confidence.d, date: p.date_label, status: p.in_force === false || !p.date ? 'not yet in force' : 'in force', scope: scopeWord(p.scope), url: p.url ?? '' })))
    : [];
  const csv = () => ({
    name: `${sheet.meta.country} strongest industrial-policy measures`,
    text: csvWithSource(
      withMeasures.flatMap((c) =>
        nipoOfCell(sheet, c.key).map((p, i) => ({
          sector: c.tech, stage: c.stage, rank_in_sector: i + 1, title: p.title, implementation_date: p.date, in_force: p.in_force !== false && !!p.date, removal_date: p.removal, economy_wide: !!p.economy_wide, strength_package_adjusted: p.strength.v,
          strength_without_package: p.strength_base.v, mapping_confidence: p.confidence.v, scope: p.scope ?? null, jurisdictions: p.n_jurisdictions?.v ?? null, gta_link: p.url,
        })),
      ),
      vintage,
      sheet.meta.as_of,
    ),
  });
  return (
    <ChartCard
      headingLevel={3}
      title="Strongest active industrial-policy measures"
      sub={sub}
      summary={n?.covered ? `${cap(name)}: the strongest active industrial-policy measures for ${current?.label ?? 'each sector'}.` : `${name} is not covered by the industrial-policy measures run.`}
      table={n?.covered && twinRows.length ? () => (
        <TableView<Row>
          rows={twinRows}
          cols={[
            { id: 'sector', header: 'Sector', value: (r) => r.sector },
            { id: 'rank', header: 'Rank', value: (r) => r.rank, num: true },
            { id: 'title', header: 'Measure', value: (r) => r.title },
            { id: 'strength', header: 'Strength', value: (r) => r.strength, num: true },
            { id: 'confidence', header: 'Mapping confidence', value: (r) => r.confidence, num: true },
            { id: 'date', header: 'Implementation', value: (r) => r.date },
            { id: 'status', header: 'Status', value: (r) => r.status },
            { id: 'scope', header: 'Scope', value: (r) => r.scope },
          ]}
          caption={`${cap(name)}: strongest industrial-policy measures by sector`}
          search
        />
      ) : undefined}
      csv={n?.covered && twinRows.length ? csv : undefined}
      badge={<Tag>CONTEXT</Tag>}
      notes={
        n ? (
          <div className="space-y-1.5">
            {n.strength_words && <p>{n.strength_words}</p>}
            {n.confidence_words && <p>{n.confidence_words}</p>}
            <p>
              {strictRule
                ? `Active here means implemented on or before ${sheet.meta.as_of_label} and not removed (no removal date, or one after it).${n.n_not_in_force && (n.n_not_in_force.v ?? 0) > 0 ? ` A further ${n.n_not_in_force.d} measures with no removal date are only announced or scheduled and are not listed.` : ''}`
                : `Active means not removed: no removal date, or one after ${sheet.meta.as_of_label}.${n.n_not_in_force && (n.n_not_in_force.v ?? 0) > 0 ? ` ${n.n_not_in_force.d} of ${poss(name)} active measures had not been implemented by then (announced or scheduled); any listed here is marked "not yet in force".` : ''}`}
              {(n.n_crosscut.v ?? 0) > 0 ? ` ${n.n_crosscut.d} active measures list no products and are not tied to any sector.` : ''} Source: {vintage}, the latest completed run; strength scored by the authors. Links open Global Trade Alert.
            </p>
          </div>
        ) : undefined
      }
    >
      {!n ? (
        <p className="text-sm text-paper-mute">This export has no industrial-policy measures run.</p>
      ) : !n.covered ? (
        <p className="text-sm text-paper-mute">
          The {vintage} run scores measures for {n.jurisdictions?.d ?? 'a set of'} implementing jurisdictions; {name} is not among them.
        </p>
      ) : !withMeasures.length ? (
        <p className="text-sm text-paper-mute">No active measure is tied to one of {poss(name)} 33 sectors in this run.</p>
      ) : (
        <>
          {sharedLine && <p className="mb-3 text-sm text-paper-dim">{sharedLine}</p>}
          <div className="mb-3 flex flex-wrap items-end gap-4">
            <Select
              label="Sector"
              value={current?.key ?? ''}
              options={withMeasures.map((c) => ({ value: c.key, label: `${c.label} (${n.n_by_cell?.[c.key]?.d ?? '0'} active)`, group: c.stage }))}
              onChange={(k) => setCell(k)}
              className="min-w-[18rem]"
            />
            {cell && current && cell !== current.key && (
              <span className="text-xs text-paper-mute">No active measure is tied to {cellOf(sheet, cell)?.label ?? 'the selected sector'}; showing {current.label}.</span>
            )}
          </div>
          <ol className="m-0 list-none p-0" aria-label={`Strongest active measures for ${current?.label ?? ''}`}>
            {list.map((p) => (
              <NipoItem key={p.id} p={p} />
            ))}
          </ol>
        </>
      )}
    </ChartCard>
  );
}

