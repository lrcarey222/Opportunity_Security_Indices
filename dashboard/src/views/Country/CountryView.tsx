// View 2: Country scores (spec_react.md 8.6; nav label "Country scores", spec_country.md C3.5). Stat
// tiles, Energy Security and Economic Opportunity per sector (faceted by stage; click a dot to pick
// the sector), the category contributions of the chosen sector, the variable drill-down (vars chunk)
// and the economy's top Strategic Index sectors; a bridge to the same economy in Country Insights.

import { useCallback, useEffect, useMemo, useState } from 'react';
import { useParams } from 'react-router';
import * as Plot from '@observablehq/plot';
import { ChartCard } from '../../charts/ChartCard.tsx';
import { Legend } from '../../charts/legends.tsx';
import { PlotFigure, Tip } from '../../charts/PlotFigure.tsx';
import { labelMargin, symDomain } from '../../charts/theme.ts';
import { CountrySelect } from '../../components/CountrySelect.tsx';
import { TableView, type Col } from '../../components/TableView.tsx';
import { Term } from '../../components/Term.tsx';
import { EmptyState, FilterRow, Loading, Notice, PageHead, RankChip, Segmented, StatTile } from '../../components/ui.tsx';
import type { Table } from '../../engine/types.ts';
import { rowsToCsv, tableToCsv } from '../../lib/csv.ts';
import { cellLabel, DASH, fmtNum, fmtPct, fmtSigned, isNum, tierLabel } from '../../lib/format.ts';
import { useApp, useCore, useEngine, usePartner, useScenario } from '../../state/EngineProvider.tsx';
import { useGoto } from '../../state/nav.ts';
import { toSpec, type ShowMode } from '../../state/url.ts';
import { COMPARE_COLOR, INK, PILLAR_COLOR, SIGNAL } from '../../theme/palette.ts';
import { stageLabel, stageRank } from '../../lib/stages.ts';
import { alliedStagesWords, CountryInsightsLink, noteText, numOr, policyBasisShort, policyBasisText, resourceBaseText, rowsOf, showWord, strOr, TechStageFilters, useCountryOptions, useTechStage, useViewTitle, type Row } from '../common.tsx';
import { panelAxisX, stageFacetLayout, stageLabels } from '../stageFacets.ts';

/** The dot plot's margins around its grid of stage panels. */
const DOTS_MARGIN = { top: 30, right: 16, bottom: 44 } as const;

interface PillarPoint {
  pillar: 'Energy Security' | 'Economic Opportunity';
  tech: string;
  stage: string;
  value: number | null;
  value_base: number | null;
  rankable: boolean;
  reason: string | null;
  shown: number | null;
}

export interface ContribBar {
  category: string;
  score: number | null;
  weight: number | null;
  contribution: number | null;
  weight_base: number | null;
  contribution_base: number | null;
  imputed: boolean;
  rule: string | null;
}

export const SI_TERMS: [string, string][] = [
  ['contrib_es_need', 'Security need'],
  ['contrib_eo', 'Economic Opportunity'],
  ['contrib_policy', 'Policy activity'],
  ['contrib_sc', 'Stage'],
  ['contrib_ghg', 'Technology (low-emission)'],
];

/** Contributions of one cell, with the baseline weight shares recomputed over the same categories
 * (category scores do not depend on the dials; only the weights move). */
export function contribBars(t: Table | undefined, country: string, tech: string, stage: string, w0: Record<string, number>): ContribBar[] {
  const rows = rowsOf(t).filter((r) => r.Country === country && r.tech === tech && r.supply_chain === stage);
  const sum0 = rows.reduce((a, r) => a + (w0[String(r.category)] ?? 0), 0);
  return rows.map((r) => {
    const score = numOr(r.category_score);
    const wb = sum0 > 0 ? (w0[String(r.category)] ?? 0) / sum0 : null;
    return {
      category: String(r.category),
      score,
      weight: numOr(r.category_weight),
      contribution: numOr(r.contribution),
      weight_base: wb,
      contribution_base: score !== null && wb !== null ? score * wb : null,
      imputed: r.imputed === true,
      rule: strOr(r.missing_rule_applied),
    };
  });
}

function ContribChart({ bars, color, title, showMode }: { bars: ContribBar[]; color: string; title: string; showMode: ShowMode }) {
  const height = 40 + bars.length * 26;
  const build = useCallback(
    (width: number): Plot.PlotOptions | null => {
      if (!bars.length) return null;
      const val = (b: ContribBar) => (showMode === 'baseline' ? b.contribution_base : b.contribution) ?? 0;
      const order = [...bars].sort((a, b) => val(b) - val(a)).map((b) => b.category);
      const hi = Math.max(0.05, ...bars.map((b) => Math.max(b.contribution ?? 0, b.contribution_base ?? 0)));
      return {
        width,
        height,
        marginLeft: labelMargin(bars.map((b) => b.category), { min: 80, max: 170 }),
        marginRight: 56,
        x: { label: 'Adds to the score (category score × its weight share) →', grid: true, domain: [0, hi * 1.1] },
        y: { domain: order, label: null, tickSize: 0 },
        marks: [
          Plot.barX(bars, { y: 'category', x: val, fill: color, insetTop: 5, insetBottom: 5 }),
          showMode === 'change'
            ? Plot.barX(bars, { y: 'category', x: (b: ContribBar) => b.contribution_base ?? 0, fill: 'none', stroke: COMPARE_COLOR.baseline, strokeWidth: 1.5, insetTop: 4, insetBottom: 4 })
            : null,
          Plot.text(bars, { y: 'category', x: val, text: (b: ContribBar) => fmtNum(val(b), 3), dx: 6, textAnchor: 'start', fill: '#d1d5db' }),
          Plot.ruleX([0], { stroke: 'rgb(255 255 255 / 0.22)' }),
          Plot.barX(bars, Plot.pointerY({ y: 'category', x: val, fill: 'none', stroke: SIGNAL, strokeWidth: 2, insetTop: 4, insetBottom: 4 })),
        ],
      };
    },
    [bars, color, showMode, height],
  );
  if (!bars.length) return <EmptyState>No category scores for this cell.</EmptyState>;
  return (
    <PlotFigure<ContribBar>
      build={build}
      height={height}
      ariaLabel={title}
      tooltip={(b) => (
        <Tip
          value={fmtNum(b.contribution, 3)}
          title={b.category}
          lines={[
            `score ${fmtNum(b.score, 3)} × weight share ${fmtPct(b.weight, 1)}`,
            `baseline: ${fmtNum(b.contribution_base, 3)} (weight share ${fmtPct(b.weight_base, 1)})`,
            b.imputed ? `rests on filled-in data${b.rule ? `: ${b.rule}` : ''}` : null,
          ]}
        />
      )}
    />
  );
}

/** `embedded` (IA refactor 2026-10-16): inside the Country profile, which owns the title, the head and the country. */
export default function CountryView({ embedded = false }: { embedded?: boolean } = {}) {
  useViewTitle('Country scores', embedded);
  const app = useApp();
  const { client } = useEngine();
  const params = useParams();
  const goto = useGoto();
  const { scenario, key, show, isBaseline } = useScenario();
  const iso = params.iso3 && app.byIso.has(params.iso3) ? params.iso3 : app.defaultCountry;
  const c = app.byIso.get(iso) ?? app.countries[0];
  const { tech, stage } = useTechStage();
  const { setView } = useScenario();
  const pickSector = useCallback(
    (t: string, s: string) => setView({ tech: t === app.defaultTech ? null : t, stage: s === app.defaultStage ? null : s }, { replace: true }),
    [setView, app.defaultTech, app.defaultStage],
  );
  const core = useCore({ contrib: true });
  const partner = usePartner(false);
  const scoredSet = useMemo(() => new Set(app.scored.map((s) => s.iso3c)), [app.scored]);
  const options = useCountryOptions((x) => scoredSet.has(x.iso3c));
  const [varPillar, setVarPillar] = useState<'es' | 'eo'>('es');
  const [vars, setVars] = useState<{ key: string; table: Table | null; error: string | null } | null>(null);

  // the pipeline's name for the country (pillar tables are keyed by Country)
  const countryName = useMemo(() => {
    for (const r of rowsOf(app.baseline.pillars.es)) if (r.iso3c === iso) return String(r.Country);
    for (const r of rowsOf(app.baseline.pillars.eo)) if (r.iso3c === iso) return String(r.Country);
    return c.country;
  }, [app.baseline, iso, c.country]);

  // sectors without any Energy Security score at the baseline (the lede says how many)
  const nNoEs = useMemo(() => {
    const has = new Set(rowsOf(app.baseline.pillars.es).filter((r) => numOr(r.value) !== null).map((r) => `${r.tech}|${r.supply_chain}`));
    return app.cells.filter((cl) => !has.has(`${cl.tech}|${cl.supply_chain}`)).length;
  }, [app]);

  const pillars = useMemo(() => {
    if (!core.data) return null;
    const pick = (t: Table, pillar: PillarPoint['pillar']): PillarPoint[] =>
      rowsOf(t)
        .filter((r) => r.iso3c === iso)
        .map((r) => {
          const v = numOr(r.value);
          const vb = numOr(r.value_base);
          return {
            pillar, tech: String(r.tech), stage: String(r.supply_chain), value: v, value_base: vb,
            rankable: r.rankable === true, reason: strOr(r.rank_exclusion),
            shown: show === 'baseline' ? vb : show === 'change' ? (v !== null && vb !== null ? v - vb : null) : v,
          };
        });
    return [...pick(core.data.compare.es, 'Energy Security'), ...pick(core.data.compare.eo, 'Economic Opportunity')];
  }, [core.data, iso, show]);

  const siRows = useMemo(() => rowsOf(core.data?.compare.si).filter((r) => r.iso3c === iso), [core.data, iso]);
  const topSi = useMemo(
    () =>
      siRows
        .filter((r) => r.in_sample === true && numOr(r.rank_in_country) !== null)
        .sort((a, b) => (a.rank_in_country as number) - (b.rank_in_country as number))
        .slice(0, 10),
    [siRows],
  );

  // tiles
  const es = pillars?.filter((p) => p.pillar === 'Energy Security') ?? [];
  const eo = pillars?.filter((p) => p.pillar === 'Economic Opportunity') ?? [];
  const mean = (xs: (number | null)[]) => {
    const v = xs.filter(isNum);
    return v.length ? v.reduce((a, b) => a + b, 0) / v.length : null;
  };
  const cellKeys = new Set([...es, ...eo].map((p) => `${p.tech}|${p.stage}`));
  const esOk = new Set(es.filter((p) => p.rankable).map((p) => `${p.tech}|${p.stage}`));
  const eoOk = new Set(eo.filter((p) => p.rankable).map((p) => `${p.tech}|${p.stage}`));
  const rankableBoth = [...cellKeys].filter((k) => esOk.has(k) && eoOk.has(k)).length;
  const top = topSi[0];
  // what the economy's Policy index rests on (IEA PAMS required; CAT added where it rates) and its allied pick
  const policyRow = useMemo(() => rowsOf(core.data?.policy.eligibility).find((r) => r.Country === countryName) ?? null, [core.data, countryName]);
  const alliedPick = useMemo(() => rowsOf(core.data?.compare.allied).filter((r) => r.iso3c === iso), [core.data, iso]);
  const psiRow = useMemo(() => {
    const scen = partner.fresh && partner.data ? rowsOf(partner.data.compare.psi_partner).find((r) => r.partner_iso === iso) : undefined;
    if (scen) return { rank: numOr(scen.rank), change: numOr(scen.rank_delta), src: 'scenario' };
    const base = rowsOf(app.baseline.psi_partner).find((r) => r.partner_iso === iso);
    return base ? { rank: numOr(base.rank), change: null, src: isBaseline ? 'Baseline' : 'Baseline (open Partners to recompute)' } : null;
  }, [partner, iso, app.baseline.psi_partner, isBaseline]);
  // contributions
  const esBars = useMemo(() => contribBars(core.data?.pillars.es_contrib, countryName, tech, stage, app.urlCtx.defaults.es_weights), [core.data, countryName, tech, stage, app.urlCtx.defaults.es_weights]);
  const eoBars = useMemo(() => contribBars(core.data?.pillars.eo_contrib, countryName, tech, stage, app.urlCtx.defaults.eo_weights), [core.data, countryName, tech, stage, app.urlCtx.defaults.eo_weights]);

  // variables (vars chunk, on request)
  const varsKey = `${key}|${varPillar}|${countryName}|${tech}|${stage}`;
  useEffect(() => {
    let live = true;
    client.variables(toSpec(scenario), varPillar, countryName, tech, stage).then(
      (t) => live && setVars({ key: varsKey, table: t, error: null }),
      (e: Error) => live && setVars({ key: varsKey, table: null, error: e.message }),
    );
    return () => {
      live = false;
    };
    // the key folds scenario, pillar, country and cell in
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [varsKey, client]);

  // one stage panel's height (a row per technology); the figure's own height follows its rows of panels
  const dotsPanel = app.techs.length * 25;
  const dotsHeight = DOTS_MARGIN.top + dotsPanel + DOTS_MARGIN.bottom;
  const buildDots = useCallback(
    (width: number): Plot.PlotOptions | null => {
      if (!pillars?.length) return null;
      const pts = pillars.filter((p) => p.shown !== null);
      const ghosts = show === 'scenario' && !isBaseline ? pillars.filter((p) => p.value_base !== null && p.value !== null && Math.abs(p.value - p.value_base) > 1e-12) : [];
      const change = show === 'change';
      const dom = change ? symDomain(pts.map((p) => p.shown), 0.01) : [0, 1];
      const marginLeft = labelMargin(app.techs, { min: 80, max: 160 });
      // the six stage panels side by side where each gets 104 px, else three or two a row (a phone shows
      // 2 x 3 panels); the technologies label the left panel of each row, the stage names each panel
      const L = stageFacetLayout(app.stages, { width, marginLeft, marginRight: DOTS_MARGIN.right, marginTop: DOTS_MARGIN.top, marginBottom: DOTS_MARGIN.bottom, panelHeight: dotsPanel, minPanel: 104, gapX: 16, gapY: 48 });
      // a narrow panel: three gridlines, one or two labels, so the end ticks of adjacent panels do not run together
      const narrow = L.panelWidth < 140;
      return {
        width,
        height: L.height,
        marginLeft,
        marginRight: DOTS_MARGIN.right,
        marginTop: DOTS_MARGIN.top,
        marginBottom: DOTS_MARGIN.bottom,
        fx: L.fx,
        fy: L.fy,
        x: { domain: dom, nice: !narrow },
        y: { domain: app.techs, label: null, tickSize: 0 },
        color: { domain: ['Energy Security', 'Economic Opportunity'], range: [PILLAR_COLOR.ES, PILLAR_COLOR.EO] },
        symbol: { domain: ['Energy Security', 'Economic Opportunity'], range: ['circle', 'diamond'] },
        marks: [
          ...panelAxisX({
            label: change ? 'Change against the baseline →' : 'Score (0 to 1; right = higher) →',
            ticks: narrow ? (change ? [dom[0], 0, dom[1]] : [0, 0.5, 1]) : 4,
            tickFormat: (change
              ? (v: number) => (v === 0 ? '0' : narrow && v < 0 ? '' : fmtSigned(v, 2))
              : narrow
                ? (v: number) => (v === 0.5 ? '0.5' : '')
                : (v: number) => (v === 0 || v === 1 ? String(v) : v.toFixed(1))) as (v: never) => string,
          }),
          change ? Plot.ruleX([0], { stroke: 'rgb(255 255 255 / 0.22)' }) : null,
          stageLabels(L.grid),
          Plot.dot(ghosts, { ...L.facet, y: 'tech', x: 'value_base', r: 4.5, stroke: COMPARE_COLOR.baseline, strokeWidth: 1.5, fill: 'none' }),
          Plot.dot(pts, { ...L.facet, y: 'tech', x: 'shown', r: 4.5, fill: 'pillar', symbol: 'pillar', stroke: INK[800], strokeWidth: 1.5 }),
          Plot.dot(pts, Plot.pointer({ ...L.facet, y: 'tech', x: 'shown', r: 8, fill: 'none', stroke: SIGNAL, strokeWidth: 2, maxRadius: 24 })),
        ],
      };
    },
    [pillars, show, isBaseline, app, dotsPanel],
  );

  const siCols: Col<Row>[] = [
    { id: 'rank', header: 'Rank', value: (r) => numOr(r.rank_in_country), num: true },
    { id: 'sector', header: 'Sector', value: (r) => strOr(r.sector), minWidth: 180 },
    { id: 'si', header: 'Strategic Index', value: (r) => numOr(r.strategic_index), cell: (r) => fmtNum(r.strategic_index, 3), num: true },
    ...SI_TERMS.map(([k, label]) => ({ id: k, header: label, value: (r: Row) => numOr(r[k]), cell: (r: Row) => fmtNum(r[k], 3), num: true })),
    { id: 'chg', header: 'Rank change', value: (r) => numOr(r.rank_delta), cell: (r) => <RankChip change={numOr(r.rank_delta)} isNew={r.in_sample_base !== true} /> },
    { id: 'rb', header: 'Resource base', value: (r) => resourceBaseText(r.resource_base, r.resource_evidence) },
  ];
  const varCols: Col<Row>[] = [
    { id: 'category', header: 'Category', value: (r) => strOr(r.category), minWidth: 140 },
    { id: 'variable', header: 'Variable', value: (r) => strOr(r.variable), minWidth: 160 },
    { id: 'component_value', header: 'Ranked value (0 to 1)', value: (r) => numOr(r.component_value), cell: (r) => fmtNum(r.component_value, 3), num: true },
    { id: 'w', header: 'Weight in category', value: (r) => numOr(r.component_weight_within_overall), cell: (r) => fmtPct(r.component_weight_within_overall, 1), num: true },
    { id: 'contrib', header: 'Adds to the score', value: (r) => numOr(r.weighted_variable_contribution), cell: (r) => fmtNum(r.weighted_variable_contribution, 3), num: true },
    { id: 'imputed', header: 'Filled in', value: (r) => (r.imputed === true ? `filled in${r.missing_rule_applied ? `: ${r.missing_rule_applied}` : ''}` : '') },
  ];

  const updating = !core.fresh;
  const pillarsMoved = (pillars ?? []).some((p) => p.value !== null && p.value_base !== null && Math.abs(p.value - p.value_base) > 1e-12);
  return (
    <div>
      {!embedded && (
      <PageHead
        eyebrow="Security and opportunity scores · Country scores"
        title={`Country scores: ${c.country}`}
        lede={
          <>
            One economy&rsquo;s scores across the {app.cells.length} sectors ({app.techs.length} technologies at {app.stages.length} supply-chain stages;{' '}
            {nNoEs === 1 ? 'one of them has' : `${nNoEs} of them have`} no Energy Security score): <Term id="es" />, <Term id="eo" />, what drives them, and its top
            sectors on the <Term id="si" />. Pick an economy and click a sector to see which data move its scores. For its strategic choices and a
            plain-language briefing, open its Country profile.
          </>
        }
      >
        <CountryInsightsLink iso3={iso} country={c.country} />
      </PageHead>
      )}
      <FilterRow>
        {!embedded && <CountrySelect label="Economy" value={iso} options={options} onChange={(v) => goto(`/country/${v}`)} />}
        <TechStageFilters techLabel="Sector for the drivers: technology" />
      </FilterRow>
      {!core.data ? (
        <Loading label="Computing" />
      ) : (
        <div className="flex flex-col gap-6">
          {core.data.notes.length > 0 && <Notice kind="info">{core.data.notes.map(noteText).join(' ')}</Notice>}
          <div className={`grid grid-cols-1 gap-3 sm:grid-cols-2 xl:grid-cols-4 ${updating ? 'chart-updating' : ''}`}>
            <StatTile label="Coalition membership" value={c.tier === 'none' ? 'Not a member' : tierLabel(c.tier)} sub={c.eu_member ? 'EU member (the coalition counts the EU as one member)' : (c.region ?? undefined)} />
            <StatTile label="Ranked sectors" value={`${rankableBoth} of ${cellKeys.size}`} sub="both scores ranked against other economies" />
            <StatTile label="Average Energy Security" value={fmtNum(mean(es.map((p) => p.value)), 2)} sub={`over ${es.length} scored sectors (0 to 1)${!isBaseline ? `; baseline ${fmtNum(mean(es.map((p) => p.value_base)), 2)}` : ''}`} />
            <StatTile label="Average Economic Opportunity" value={fmtNum(mean(eo.map((p) => p.value)), 2)} sub={`over ${eo.length} scored sectors (0 to 1)${!isBaseline ? `; baseline ${fmtNum(mean(eo.map((p) => p.value_base)), 2)}` : ''}`} />
            <StatTile
              label="Top Strategic Index sector"
              value={<span className="text-base">{top ? String(top.sector) : DASH}</span>}
              chip={top && !isBaseline ? <RankChip change={numOr(top.rank_delta)} isNew={top.in_sample_base !== true} /> : undefined}
              sub={top ? `Strategic Index ${fmtNum(top.strategic_index, 3)}` : 'no sector in the Strategic Index'}
            />
            <StatTile
              label="Partnership Strength rank as a partner"
              value={psiRow && psiRow.rank !== null ? String(psiRow.rank) : DASH}
              chip={psiRow && psiRow.change !== null && !isBaseline ? <RankChip change={psiRow.change} /> : undefined}
              sub={psiRow ? psiRow.src : 'not rated'}
            />
            <StatTile
              label="Allied pick"
              value={<span className="text-base">{c.tier === 'none' ? DASH : alliedPick.length ? alliedPick.map((r) => String(r.sector)).join(', ') : 'None'}</span>}
              sub={
                c.tier === 'none'
                  ? 'not a coalition member'
                  : `its top eligible Strategic Index sector at ${alliedStagesWords(app.bundleLike.baselineInputs.allied_stages, app.stages) ?? 'its eligible stages'}${core.data.params.si.require_resource_base === false ? ' (endowment screen off)' : ''}`
              }
            />
            <StatTile
              label="Policy activity rests on"
              value={<span className="text-base">{policyRow?.eligible === true ? policyBasisShort(strOr(policyRow.policy_basis)) ?? DASH : 'None'}</span>}
              sub={
                policyRow?.eligible === true
                  ? policyBasisText(strOr(policyRow.policy_basis))
                  : policyRow
                    ? `no Policy activity score: lacks ${strOr(policyRow.missing_required) ?? 'a required component'}, so no Strategic Index`
                    : 'no policy data, so no Strategic Index'
              }
            />
          </div>

          <ChartCard
            title="Energy Security and Economic Opportunity by sector"
            sub={`${showWord(show)} Each row is a technology and each panel one of the ${app.stages.length} supply-chain stages, named above it (on a narrow screen the panels wrap into rows). The circle is Energy Security (further right = more secure) and the diamond Economic Opportunity (further right = a bigger opening); both run from 0 to 1 and compare the economy with every other economy in the same sector. Click a dot to see what drives that sector's scores below. Hollow rings mark the baseline value where the scenario differs (only the advanced category weights move these scores).${
              !isBaseline && !pillarsMoved ? ' In this scenario no sector moves: the four dials leave both scores at the baseline values.' : ''
            }`}
            updating={updating}
            summary={`Dot plot of ${es.length} Energy Security and ${eo.length} Economic Opportunity scores for ${c.country}, faceted by stage.`}
            legend={
              <Legend
                items={[
                  { label: 'Energy Security (circle)', kind: 'dot', color: PILLAR_COLOR.ES },
                  { label: 'Economic Opportunity (diamond)', kind: 'diamond', color: PILLAR_COLOR.EO },
                  { label: 'Baseline (where different)', kind: 'hollow', color: COMPARE_COLOR.baseline },
                ]}
              />
            }
            table={() => (
              <TableView
                rows={pillars ?? []}
                caption={`Energy Security and Economic Opportunity of ${c.country}`}
                search
                cols={[
                  { id: 'pillar', header: 'Score', value: (p) => p.pillar },
                  { id: 'tech', header: 'Technology', value: (p) => p.tech },
                  { id: 'stage', header: 'Stage', value: (p) => stageLabel(p.stage), sortValue: (p) => stageRank(p.stage) },
                  { id: 'value', header: 'Scenario', value: (p) => p.value, cell: (p) => fmtNum(p.value, 3), num: true },
                  { id: 'value_base', header: 'Baseline', value: (p) => p.value_base, cell: (p) => fmtNum(p.value_base, 3), num: true },
                  { id: 'rankable', header: 'Ranked', value: (p) => (p.rankable ? 'yes' : `no${p.reason ? `: ${p.reason}` : ''}`) },
                ]}
              />
            )}
            csv={() => ({
              name: `pillars-${iso}`,
              text: rowsToCsv((pillars ?? []).map((p) => ({ iso3c: iso, pillar: p.pillar, tech: p.tech, supply_chain: p.stage, value: p.value, value_base: p.value_base, rankable: p.rankable, rank_exclusion: p.reason }))),
            })}
          >
            {pillars?.length ? (
              <PlotFigure<PillarPoint>
                build={buildDots}
                height={dotsHeight}
                ariaLabel={`Energy Security and Economic Opportunity of ${c.country}`}
                onSelect={(p) => pickSector(p.tech, p.stage)}
                tooltip={(p) => (
                  <Tip
                    value={fmtNum(p.shown, 3)}
                    title={`${p.pillar}: ${cellLabel(p.tech, p.stage)}`}
                    lines={[!isBaseline ? `scenario ${fmtNum(p.value, 3)} · baseline ${fmtNum(p.value_base, 3)}` : null, !p.rankable ? `not ranked${p.reason ? `: ${p.reason}` : ''}` : null, 'click for what drives it']}
                  />
                )}
              />
            ) : (
              <EmptyState>No Energy Security or Economic Opportunity scores for this economy.</EmptyState>
            )}
          </ChartCard>

          <div className="grid gap-6 xl:grid-cols-2">
            {(
              [
                ['Energy Security', esBars, PILLAR_COLOR.ES],
                ['Economic Opportunity', eoBars, PILLAR_COLOR.EO],
              ] as const
            ).map(([name, bars, color]) => (
              <ChartCard
                key={name}
                title={`${name}: what drives it in ${tech} ${DASH} ${stage}`}
                sub={`Each bar is one category of data (for example trade, reserves or prices): its score times its share of the weight, so the bars add up to the sector's ${name}. The longest bars are the categories that lift the score most.${show === 'change' ? ' The outline is the baseline value.' : ''}`}
                updating={updating}
                summary={`${name} category contributions for ${c.country}, ${cellLabel(tech, stage)}.`}
                legend={show === 'change' ? <Legend items={[{ label: 'Scenario', color }, { label: 'Baseline', kind: 'outline', color: COMPARE_COLOR.baseline }]} /> : undefined}
                table={() => (
                  <TableView
                    rows={bars}
                    caption={`${name} contributions`}
                    cols={[
                      { id: 'category', header: 'Category', value: (b) => b.category },
                      { id: 'score', header: 'Score', value: (b) => b.score, cell: (b) => fmtNum(b.score, 3), num: true },
                      { id: 'weight', header: 'Weight share', value: (b) => b.weight, cell: (b) => fmtPct(b.weight, 1), num: true },
                      { id: 'contribution', header: 'Contribution', value: (b) => b.contribution, cell: (b) => fmtNum(b.contribution, 3), num: true },
                      { id: 'contribution_base', header: 'Baseline', value: (b) => b.contribution_base, cell: (b) => fmtNum(b.contribution_base, 3), num: true },
                      { id: 'imputed', header: 'Filled in', value: (b) => (b.imputed ? 'rests on filled-in data' : '') },
                    ]}
                  />
                )}
                csv={() => ({ name: `${name}-contributions-${iso}-${tech}-${stage}`, text: rowsToCsv(bars as unknown as Record<string, unknown>[]) })}
              >
                <ContribChart bars={bars} color={color} title={`${name} contributions`} showMode={show} />
              </ChartCard>
            ))}
          </div>

          <ChartCard
            title={`The data behind ${cellLabel(tech, stage)}`}
            sub="Each row is one measure inside a category: its value after ranking against the other economies in the sector (0 to 1, higher is better for the score), its weight inside the category and how much it adds to the score. Values the pipeline filled in where data were missing are flagged."
            tools={
              <>
              {!embedded && app.Uindex.has(iso) && (
                <button type="button" className="btn btn-ghost btn-sm" onClick={() => goto(`/partners/${iso}`, { tech, stage })} title={`${c.country}'s friendshoring opportunities and export markets in ${cellLabel(tech, stage)}`}>
                  Partners in this sector
                </button>
              )}
              <Segmented
                label="Pillar"
                hideLabel
                value={varPillar}
                options={[
                  { value: 'es', label: 'Energy Security' },
                  { value: 'eo', label: 'Economic Opportunity' },
                ]}
                onChange={setVarPillar}
              />
              </>
            }
            updating={!!vars && vars.key !== varsKey}
            csv={vars?.table ? () => ({ name: `variables-${varPillar}-${iso}-${tech}-${stage}`, text: tableToCsv(vars.table as Table) }) : undefined}
          >
            {!vars ? (
              <Loading label="Loading variable data" />
            ) : vars.error ? (
              <Notice kind="error">{vars.error}</Notice>
            ) : (
              <TableView rows={rowsOf(vars.table)} cols={varCols} caption="Variables" search empty="No variable data for this cell." />
            )}
          </ChartCard>

          <ChartCard
            title="Strategic Index: top sectors"
            sub="The economy's ten highest-ranked sectors on the Strategic Index under the scenario, with the five weighted parts that add up to it: security need, Economic Opportunity, Policy activity, the stage and the technology's low-emission value."
            updating={updating}
            tools={
              embedded ? undefined : (
              <button type="button" className="btn btn-ghost btn-sm" onClick={() => goto(`/strategic/${iso}`)}>
                Open Strategic Index
              </button>
              )
            }
            csv={() => ({ name: `strategic-index-top-${iso}`, text: rowsToCsv(topSi as Record<string, unknown>[]) })}
          >
            {topSi.length ? (
              <TableView rows={topSi} cols={siCols} caption="Top Strategic Index sectors" pageSize={10} />
            ) : (
              <EmptyState>This economy has no sectors in the Strategic Index (it needs Energy Security, Economic Opportunity and a Policy activity score).</EmptyState>
            )}
          </ChartCard>
        </div>
      )}
    </div>
  );
}
