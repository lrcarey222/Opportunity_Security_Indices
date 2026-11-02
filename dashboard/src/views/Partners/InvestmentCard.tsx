// Bilateral direct investment of one economy, partner by partner (the Partners view, the Country
// partners section and AIB Partnerships): IMF direct investment positions, inward to the left and outward
// to the right, with the conduit financial centres listed apart (greyed, never ranked: Friendshore treats
// them as unobserved); then the cleantech factory projects both ways (the CM-FDI database: cross-border
// manufacturing projects in solar, wind, batteries and electric vehicles), with a technology choice (all
// four, or one), each project counted once. The choice starts on the page's sector when it is one of the
// four (any stage) and follows the page's sector until the reader picks a technology here. A page can ask
// instead for its own sector's projects only (projects 'sector': Solar, Wind, Batteries and Electric
// Vehicles at the CM-FDI stages, the ties chunk's cmfdi_stages: Components and Assembly, which carry the
// same projects). HTML bars, so the chart reads as a list to assistive technology; the side of the axis
// and the words carry the direction, never the colour alone.

import { useMemo, useState } from 'react';
import { ChartCard } from '../../charts/ChartCard.tsx';
import { Legend } from '../../charts/legends.tsx';
import { TableView, type Col } from '../../components/TableView.tsx';
import { EmptyState, Loading, Notice, Segmented, Select } from '../../components/ui.tsx';
import { rowsToCsv } from '../../lib/csv.ts';
import { cellLabel } from '../../lib/format.ts';
import { COMPARE_COLOR, HATCH_STYLE, PAPER_MUTE } from '../../theme/palette.ts';
import { stageLabel } from '../../lib/stages.ts';
import { poss } from '../../country/derive/text.ts';
import {
  bySize, CMFDI_ATTRIBUTION, CMFDI_SECTORS, cmfdiSameProjectsNote, cmfdiStages, fmtUsdM, hasCmfdi, positionsOf, projectFlows, projectFlowsAllTechs, type PositionRow, type ProjectFlowRow, type Ties,
} from './ties.ts';

/** 'A', 'A and B', 'A, B and C'. */
const andWords = (xs: readonly string[]): string => (xs.length <= 1 ? (xs[0] ?? '') : `${xs.slice(0, -1).join(', ')} and ${xs[xs.length - 1]}`);

// two neutral marks, not the three-colour scale: direction is not a low or high value (the side and the words carry it)
export const OUT_COLOR = COMPARE_COLOR.scenario;
export const IN_COLOR = PAPER_MUTE;
const N_TOP = 15;
const N_CONDUIT = 6;

interface Bar {
  key: string;
  label: string;
  left: number | null;
  right: number | null;
  leftText: string;
  rightText: string;
  note?: string;
  muted?: boolean;
}

/** Diverging HTML bars: `left` grows leftwards from the axis, `right` rightwards (one shared scale). */
export function DivergingBars({ bars, label, leftWord, rightWord, selected, onSelect }: {
  bars: Bar[]; label: string; leftWord: string; rightWord: string; selected?: string | null; onSelect?: (key: string) => void;
}) {
  const max = Math.max(1e-12, ...bars.flatMap((b) => [Math.abs(b.left ?? 0), Math.abs(b.right ?? 0)]));
  const w = (v: number | null) => (v === null || v <= 0 ? 0 : (48 * v) / max);
  return (
    <div>
      <div className="mb-1 grid grid-cols-[minmax(6rem,9rem)_1fr] gap-x-3 text-xs text-paper-mute" aria-hidden="true">
        <span />
        {/* a gap, so a long left word that wraps on a phone never runs into the right one */}
        <span className="flex justify-between gap-4">
          <span>&larr; {leftWord}</span>
          <span className="shrink-0 text-right">{rightWord} &rarr;</span>
        </span>
      </div>
      <ul className="space-y-1" aria-label={label}>
        {bars.map((b) => {
          const sel = selected === b.key;
          const body = (
            <>
              <span className={`truncate text-left text-sm ${b.muted ? 'text-paper-mute' : 'text-paper-dim'}`}>
                {b.label}
                {b.note && <span className="text-xs text-paper-mute"> · {b.note}</span>}
              </span>
              <span className="relative block h-4 w-full bg-ink-700/60" aria-hidden="true">
                <span className="absolute inset-y-0 left-1/2 w-px bg-paper-mute/60" />
                {w(b.left) > 0 && <span className="absolute inset-y-0" style={{ right: '50%', width: `${w(b.left)}%`, ...(b.muted ? HATCH_STYLE : { background: IN_COLOR }) }} />}
                {w(b.right) > 0 && <span className="absolute inset-y-0" style={{ left: '50%', width: `${w(b.right)}%`, ...(b.muted ? HATCH_STYLE : { background: OUT_COLOR }) }} />}
              </span>
              <span className="sr-only">
                {leftWord}: {b.leftText}; {rightWord}: {b.rightText}
              </span>
            </>
          );
          return (
            <li key={b.key}>
              {onSelect ? (
                <button
                  type="button"
                  className={`grid w-full grid-cols-[minmax(6rem,9rem)_1fr] items-center gap-x-3 rounded px-1 ${sel ? 'outline outline-2 outline-signal' : 'hover:bg-ink-700/40'}`}
                  title={`${b.label}: ${leftWord} ${b.leftText}; ${rightWord} ${b.rightText}`}
                  aria-pressed={sel}
                  onClick={() => onSelect(b.key)}
                >
                  {body}
                </button>
              ) : (
                <div className="grid grid-cols-[minmax(6rem,9rem)_1fr] items-center gap-x-3 px-1" title={`${b.label}: ${leftWord} ${b.leftText}; ${rightWord} ${b.rightText}`}>
                  {body}
                </div>
              )}
            </li>
          );
        })}
      </ul>
    </div>
  );
}

const yearText = (r: PositionRow, side: 'out' | 'in') => {
  const y = side === 'out' ? r.out_year : r.in_year;
  const m = side === 'out' ? r.out_mirror : r.in_mirror;
  return `${y ?? 'year unknown'}${m ? ', as reported by the partner' : ''}`;
};

export interface InvestmentCardProps {
  iso: string;
  /** the economy in prose ("the United States") */
  name: string;
  ties: Ties | null;
  error?: string | null;
  sector: { tech: string; stage: string } | null;
  nameOf: (iso: string) => string;
  headingLevel?: 2 | 3;
  selected?: string | null;
  onSelect?: (iso: string) => void;
  /**
   * the factory projects: 'always' (default) shows the cleantech factory-projects card after the positions,
   * with its own technology choice; 'sector' shows the projects only when `sector` is one of the four
   * technologies at a CM-FDI stage, for that sector (a page whose own note explains the projects by sector)
   */
  projects?: 'always' | 'sector';
  /**
   * projects 'sector' only: what the positions card's notes tell the reader about the factory-project
   * sectors (default "choose one of those sectors"; a page whose sector filter does not offer those
   * sectors says something else)
   */
  projectsHint?: string;
}

/** IMF positions, then the cleantech factory projects (always, or only in the four sectors: `projects`). */
export function InvestmentCards(p: InvestmentCardProps) {
  const sectorOnly = p.projects === 'sector';
  return (
    <>
      <PositionsCard {...p} />
      {!sectorOnly ? (
        // the positions card already says when the data are missing; the projects come from the same chunk
        !p.error && <CleantechProjectsCard {...p} />
      ) : (
        p.sector && hasCmfdi(p.sector.tech, p.sector.stage, p.ties) && <ProjectsCard {...p} sector={p.sector} />
      )}
    </>
  );
}

function PositionsCard({ iso, name, ties, error, nameOf, headingLevel, selected, onSelect, projects = 'always', projectsHint = 'choose one of those sectors' }: InvestmentCardProps) {
  const [kind, setKind] = useState<'all' | 'debt'>('all');
  const all = useMemo(() => (ties ? positionsOf(ties, iso, kind) : []), [ties, iso, kind]);
  const ranked = useMemo(() => bySize(all.filter((r) => !r.conduit)), [all]);
  const conduits = useMemo(() => bySize(all.filter((r) => r.conduit)), [all]);
  const m = ties?.meta;
  const yrs = m && m.dip_years.length ? `${Math.min(...m.dip_years)}–${Math.max(...m.dip_years)}` : 'the index years';
  const bar = (r: PositionRow, muted = false): Bar => ({
    key: r.partner_iso,
    label: nameOf(r.partner_iso),
    left: r.in,
    right: r.out,
    leftText: r.in === null ? 'no position reported' : `${fmtUsdM(r.in)} (${yearText(r, 'in')})`,
    rightText: r.out === null ? 'no position reported' : `${fmtUsdM(r.out)} (${yearText(r, 'out')})`,
    note: muted ? 'conduit centre' : r.out_mirror || r.in_mirror ? 'partly from the partner' : undefined,
    muted,
  });
  const bars = [...ranked.slice(0, N_TOP).map((r) => bar(r)), ...conduits.slice(0, N_CONDUIT).map((r) => bar(r, true))];
  const yearsOut = [...new Set(all.filter((r) => r.out !== null && !r.out_mirror).map((r) => r.out_year))].filter((y) => y !== null);
  type T = PositionRow & { name: string };
  const tableRows: T[] = all.map((r) => ({ ...r, name: nameOf(r.partner_iso) }));
  const cols: Col<T>[] = [
    { id: 'name', header: 'Partner', value: (r) => r.name, minWidth: 140 },
    { id: 'out', header: `Outward: ${name} in the partner`, value: (r) => r.out, cell: (r) => `${fmtUsdM(r.out)}${r.out !== null ? ` (${yearText(r, 'out')})` : ''}`, num: true, minWidth: 160 },
    { id: 'in', header: `Inward: the partner in ${name}`, value: (r) => r.in, cell: (r) => `${fmtUsdM(r.in)}${r.in !== null ? ` (${yearText(r, 'in')})` : ''}`, num: true, minWidth: 160 },
    { id: 'idx', header: `What Friendshore ranks (outward debt instruments, ${yrs} average)`, value: (r) => (r.conduit ? null : r.idx_mean), cell: (r) => (r.conduit ? 'not used (conduit centre)' : fmtUsdM(r.idx_mean)), num: true, minWidth: 190 },
    { id: 'conduit', header: 'Conduit financial centre', value: (r) => (r.conduit ? 'yes' : '') },
  ];
  const kindWord = kind === 'all' ? 'all financial instruments, net' : 'debt instruments, gross';
  const cmStages = andWords(cmfdiStages(ties).map(stageLabel));
  return (
    <ChartCard
      headingLevel={headingLevel}
      title={`Direct investment between ${name} and its partners`}
      sub={`IMF direct investment positions (the stock of investment, in US dollars; ${kindWord}), latest year reported. Right: what ${poss(name)} firms hold in the partner (outward). Left: what the partner's firms hold in ${name} (inward). Largest ${N_TOP} pairs first; conduit financial centres, where investment mostly passes through on its way elsewhere, are listed apart and hatched.`}
      summary={`${name}: inward and outward direct investment positions with ${ranked.length} partners and ${conduits.length} conduit centres.`}
      tools={<Segmented<'all' | 'debt'> label="Positions" hideLabel value={kind} onChange={setKind} options={[{ value: 'all', label: 'All instruments' }, { value: 'debt', label: 'Debt instruments' }]} />}
      legend={
        <Legend
          items={[
            { label: `Inward (partner's firms in ${name})`, color: IN_COLOR },
            { label: `Outward (${poss(name)} firms in the partner)`, color: OUT_COLOR },
            { label: 'Conduit financial centre (not ranked)', kind: 'hatch' },
          ]}
        />
      }
      table={() => <TableView<T> rows={tableRows} cols={cols} caption={`${name}: direct investment positions by partner`} search pageSize={15} initialSort={{ id: 'out', desc: true }} rowKey={(r) => r.partner_iso} />}
      csv={() => ({
        name: `direct-investment-${iso}-${kind}`,
        text: rowsToCsv(
          tableRows.map((r) => ({
            economy: iso, partner: r.name, partner_iso3: r.partner_iso, outward_usd_m: r.out, outward_year: r.out_year, outward_reported_by_partner: r.out_mirror,
            inward_usd_m: r.in, inward_year: r.in_year, inward_reported_by_partner: r.in_mirror, friendshore_ranked_outward_debt_mean_usd_m: r.conduit ? null : r.idx_mean,
            conduit_centre: r.conduit, instruments: kindWord, source: m?.dip_source ?? 'IMF, Direct Investment Positions',
          })),
        ),
      })}
      notes={
        <>
          How this feeds the index: Friendshore&apos;s investment tie ranks {poss(name)} outward debt-instrument position in each partner, averaged over {yrs}, together
          with that position relative to the partner&apos;s economy and to both economies ({m ? `GDP of ${m.dip_gdp_year}` : 'GDP'}); a larger position makes the
          partner a stronger friendshoring opportunity. Conduit centres are left out on both sides, so they neither gain nor lose. In {andWords(CMFDI_SECTORS)} at
          the {cmStages} stages the tie is the factory projects instead{projects === 'sector' ? ` (${projectsHint})` : error ? '' : ' (see the factory projects card below)'}. Positions are the stock at the end of the year, as reported to
          the IMF by {name}{yearsOut.length ? ` (${yearsOut.sort().join(', ')})` : ''}; where {name} reports nothing for a pair, the partner&apos;s report is used and
          marked. Source: {m?.dip_source ?? 'IMF, Direct Investment Positions'}.
        </>
      }
    >
      {error ? (
        <Notice kind="error">{error}</Notice>
      ) : !ties ? (
        <Loading label="Loading direct investment" />
      ) : bars.length ? (
        <DivergingBars bars={bars} label={`${name}: direct investment by partner`} leftWord="inward" rightWord="outward" selected={selected} onSelect={onSelect} />
      ) : (
        <EmptyState>No direct investment positions are reported for {name} or by its partners.</EmptyState>
      )}
    </ChartCard>
  );
}

// ---- factory projects (both cards) ---------------------------------------------------------------------

// counts are attributed: a joint venture is split between its parents' home economies, so they can be fractions
const projectCount = (n: number) => `${Number.isInteger(n) ? n : n.toFixed(1)} project${n === 1 ? '' : 's'}`;
const projectTotal = (n: number) => +n.toFixed(1);
const disclosedMoney = (v: number | null, n: number | null) => (v === null || !n ? 'capital not disclosed' : `${fmtUsdM(v)} of disclosed capital`);
type ProjectTableRow = ProjectFlowRow & { name: string };

/** The bars, table rows and columns, and the totals both ways, of one list of project flows. */
function projectParts(rows: ProjectFlowRow[], name: string, nameOf: (iso: string) => string) {
  const bars: Bar[] = rows.slice(0, N_TOP).map((r) => ({
    key: r.partner_iso,
    label: nameOf(r.partner_iso),
    left: r.in_projects,
    right: r.out_projects,
    leftText: `${projectCount(r.in_projects)}${r.in_projects ? `, ${disclosedMoney(r.in_value, r.in_disclosed)}` : ''}`,
    rightText: `${projectCount(r.out_projects)}${r.out_projects ? `, ${disclosedMoney(r.out_value, r.out_disclosed)}` : ''}`,
  }));
  const tableRows: ProjectTableRow[] = rows.map((r) => ({ ...r, name: nameOf(r.partner_iso) }));
  const cols: Col<ProjectTableRow>[] = [
    { id: 'name', header: 'Partner', value: (r) => r.name, minWidth: 140 },
    { id: 'out', header: `Projects by ${poss(name)} firms there`, value: (r) => r.out_projects, cell: (r) => projectCount(r.out_projects), num: true },
    { id: 'out_v', header: 'Their disclosed capital', value: (r) => r.out_value, cell: (r) => (r.out_projects ? disclosedMoney(r.out_value, r.out_disclosed) : '–'), num: true, minWidth: 170 },
    { id: 'in', header: `Projects by the partner's firms in ${name}`, value: (r) => r.in_projects, cell: (r) => projectCount(r.in_projects), num: true },
    { id: 'in_v', header: 'Their disclosed capital', value: (r) => r.in_value, cell: (r) => (r.in_projects ? disclosedMoney(r.in_value, r.in_disclosed) : '–'), num: true, minWidth: 170 },
  ];
  const nOut = projectTotal(rows.reduce((s, r) => s + r.out_projects, 0));
  const nIn = projectTotal(rows.reduce((s, r) => s + r.in_projects, 0));
  return { bars, tableRows, cols, nOut, nIn };
}

/** The CSV of one list of project flows; `what` names the sector or technology column. */
const projectsCsv = (iso: string, what: Record<string, string>, tableRows: ProjectTableRow[], source: string) =>
  rowsToCsv(
    tableRows.map((r) => ({
      economy: iso, ...what, partner: r.name, partner_iso3: r.partner_iso, projects_by_economy_firms_in_partner: r.out_projects, disclosed_capital_usd_m_out: r.out_value,
      projects_disclosing_out: r.out_disclosed, projects_by_partner_firms_in_economy: r.in_projects, disclosed_capital_usd_m_in: r.in_value, projects_disclosing_in: r.in_disclosed,
      source,
    })),
  );

const projectsLegend = (name: string) => (
  <Legend
    items={[
      { label: `Partner's firms in ${name}`, color: IN_COLOR },
      { label: `${poss(name)} firms in the partner`, color: OUT_COLOR },
    ]}
  />
);

/** projects 'sector': the factory projects of the page's sector (one of the four technologies at a CM-FDI stage). */
function ProjectsCard({ iso, name, ties, nameOf, headingLevel, sector, selected, onSelect }: InvestmentCardProps & { sector: { tech: string; stage: string } }) {
  const rows = useMemo(() => (ties ? projectFlows(ties, iso, sector.tech, sector.stage) : []), [ties, iso, sector.tech, sector.stage]);
  const label = cellLabel(sector.tech, sector.stage);
  const { bars, tableRows, cols, nOut, nIn } = projectParts(rows, name, nameOf);
  const attribution = ties?.meta.cmfdi_attribution ?? CMFDI_ATTRIBUTION;
  // said once: the stages that carry the same projects (Components and Assembly)
  const same = cmfdiSameProjectsNote(cmfdiStages(ties));
  return (
    <ChartCard
      headingLevel={headingLevel}
      title={`Factory projects in ${label}: ${name} and its partners`}
      sub={`Cross-border manufacturing projects in ${label} (announced, under construction or operating). Right: projects by ${poss(name)} firms in the partner (${nOut}). Left: projects by the partner's firms in ${name} (${nIn}). A joint venture counts as a share for each parent's home economy, so counts can be fractions. In this sector these projects, not the IMF positions, are Friendshore's investment tie: a partner where ${poss(name)} firms have built or announced factories scores higher as a friendshoring opportunity.`}
      summary={`${name}, ${label}: ${nOut} factory projects abroad and ${nIn} at home by foreign firms, across ${rows.length} partners.`}
      legend={projectsLegend(name)}
      table={() => <TableView<ProjectTableRow> rows={tableRows} cols={cols} caption={`${name}: factory projects in ${label} by partner`} pageSize={15} rowKey={(r) => r.partner_iso} />}
      csv={() => ({ name: `factory-projects-${iso}-${sector.tech}-${sector.stage}`.replace(/\s+/g, '-'), text: projectsCsv(iso, { sector: label }, tableRows, attribution) })}
      notes={
        <>
          {same && <span data-testid="cmfdi-same-projects">{same} </span>}
          <span data-testid="cmfdi-attribution">{attribution}</span>
        </>
      }
    >
      {!ties ? (
        <Loading label="Loading factory projects" />
      ) : bars.length ? (
        <DivergingBars bars={bars} label={`${name}: factory projects in ${label} by partner`} leftWord={`into ${name}`} rightWord="abroad" selected={selected} onSelect={onSelect} />
      ) : (
        <EmptyState>No cross-border factory projects in {label} involve {name}.</EmptyState>
      )}
    </ChartCard>
  );
}

/** The factory-projects card's technology choice: one of the four, or '' for all four together. */
const ALL_FOUR_TECHS = '';
const TECH_OPTIONS = [{ value: ALL_FOUR_TECHS, label: 'All four technologies' }, ...CMFDI_SECTORS.map((t) => ({ value: t, label: t }))];
const techWords = (tech: string) => tech.toLowerCase();
const orWords = (xs: readonly string[]): string => (xs.length <= 1 ? (xs[0] ?? '') : `${xs.slice(0, -1).join(', ')} or ${xs[xs.length - 1]}`);

/** The technology the factory-projects card starts on: the page's sector's when it is one of the four (any stage), else all four
 *  (the card also falls back to all four when that technology has no projects for the economy). */
const projectsTechOf = (sector: { tech: string } | null): string => (sector && CMFDI_SECTORS.includes(sector.tech) ? sector.tech : ALL_FOUR_TECHS);

/**
 * projects 'always': the cleantech factory projects between the economy and its partners, all four
 * technologies together or one (each project once), whatever the page's sector.
 */
function CleantechProjectsCard({ iso, name, ties, nameOf, headingLevel, sector, selected, onSelect }: InvestmentCardProps) {
  // the reader's own technology; until they pick one the card follows the page's sector
  const [picked, setPicked] = useState<string | null>(null);
  const start = projectsTechOf(sector);
  // a sector's technology with no projects for this economy opens on all four, not on an empty card
  const startTech = useMemo(
    () => (start !== ALL_FOUR_TECHS && ties && !projectFlows(ties, iso, start, null).length ? ALL_FOUR_TECHS : start),
    [ties, iso, start],
  );
  const tech = picked ?? startTech;
  const rows = useMemo(() => (!ties ? [] : tech === ALL_FOUR_TECHS ? projectFlowsAllTechs(ties, iso) : projectFlows(ties, iso, tech, null)), [ties, iso, tech]);
  const { bars, tableRows, cols, nOut, nIn } = projectParts(rows, name, nameOf);
  const four = CMFDI_SECTORS.map(techWords);
  const words = tech === ALL_FOUR_TECHS ? andWords(four) : techWords(tech);
  const attribution = ties?.meta.cmfdi_attribution ?? CMFDI_ATTRIBUTION;
  const stages = cmfdiStages(ties);
  // said once: the stages that carry the same projects (Components and Assembly)
  const same = cmfdiSameProjectsNote(stages);
  return (
    <ChartCard
      headingLevel={headingLevel}
      title={`Cleantech factory projects between ${name} and its partners`}
      sub={`Cross-border manufacturing projects in ${words} (announced, under construction or operating), by the home country of the investing firm and the country where the factory is. Right: projects by ${poss(name)} firms in the partner (${nOut}). Left: projects by the partner's firms in ${name} (${nIn}). Capital is shown where the project discloses it. A joint venture counts as a share for each parent's home economy, so counts can be fractions. The data cover four technologies only: ${andWords(four)}.`}
      summary={`${name}, ${words}: ${nOut} factory projects abroad and ${nIn} at home by foreign firms, across ${rows.length} partners.`}
      tools={<Select label="Technology" value={tech} options={TECH_OPTIONS} onChange={setPicked} className="min-w-[12rem]" />}
      legend={projectsLegend(name)}
      table={() => <TableView<ProjectTableRow> rows={tableRows} cols={cols} caption={`${name}: factory projects in ${words} by partner`} pageSize={15} rowKey={(r) => r.partner_iso} />}
      csv={() => ({
        name: `factory-projects-${iso}-${tech || 'all-four-technologies'}`.replace(/\s+/g, '-'),
        text: projectsCsv(iso, { technology: tech || andWords(CMFDI_SECTORS) }, tableRows, attribution),
      })}
      notes={
        <>
          How this feeds the index: in {andWords(CMFDI_SECTORS)} at the {andWords(stages.map(stageLabel))} stages, these projects, not the IMF positions, are
          Friendshore&apos;s investment tie, so a partner where {poss(name)} firms have built or announced factories scores higher as a friendshoring
          opportunity. {same && <span data-testid="cmfdi-same-projects">{same} </span>}
          <span data-testid="cmfdi-attribution">{attribution}</span>
        </>
      }
    >
      {!ties ? (
        <Loading label="Loading factory projects" />
      ) : bars.length ? (
        <DivergingBars bars={bars} label={`${name}: factory projects in ${words} by partner`} leftWord={`into ${name}`} rightWord="abroad" selected={selected} onSelect={onSelect} />
      ) : (
        <EmptyState>
          No cross-border factory projects in {tech === ALL_FOUR_TECHS ? orWords(four) : words} involve {name}, in either direction.
        </EmptyState>
      )}
    </ChartCard>
  );
}
