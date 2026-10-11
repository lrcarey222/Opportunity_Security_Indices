// View 2: Country profile (spec_react.md 8.6). Stat tiles, ES and EO per cell (faceted by stage),
// category contributions of a chosen cell, the variable drill-down (vars chunk) and the economy's
// top Strategic Index sectors.

import { useCallback, useEffect, useMemo, useState } from 'react';
import { useParams } from 'react-router';
import * as Plot from '@observablehq/plot';
import { useTitle } from '../../App.tsx';
import { ChartCard } from '../../charts/ChartCard.tsx';
import { Legend } from '../../charts/legends.tsx';
import { PlotFigure, Tip } from '../../charts/PlotFigure.tsx';
import { labelMargin, symDomain } from '../../charts/theme.ts';
import { CountrySelect } from '../../components/CountrySelect.tsx';
import { TableView, type Col } from '../../components/TableView.tsx';
import { EmptyState, FilterRow, Loading, Notice, PageHead, RankChip, Segmented, StatTile } from '../../components/ui.tsx';
import type { Table } from '../../engine/types.ts';
import { rowsToCsv, tableToCsv } from '../../lib/csv.ts';
import { cellLabel, DASH, fmtNum, fmtPct, fmtSigned, isNum, tierLabel } from '../../lib/format.ts';
import { nodeLabel } from '../../state/appData.ts';
import { useApp, useCore, useEngine, useNetwork, usePartner, useScenario } from '../../state/EngineProvider.tsx';
import { useGoto } from '../../state/nav.ts';
import { toSpec, type ShowMode } from '../../state/url.ts';
import { COMPARE_COLOR, INK, PILLAR_COLOR, SIGNAL } from '../../theme/palette.ts';
import { noteText, numOr, policyBasisText, resourceBaseText, rowsOf, showWord, strOr, TechStageFilters, useCountryOptions, useTechStage, type Row } from '../common.tsx';

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
  ['contrib_es_need', 'Need (1 − ES)'],
  ['contrib_eo', 'EO'],
  ['contrib_policy', 'Policy*'],
  ['contrib_sc', 'Stage'],
  ['contrib_ghg', 'Technology'],
];

/** Contributions of one cell, with the paper's weight shares recomputed over the same categories
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
        x: { label: 'Contribution (score × weight share)', grid: true, domain: [0, hi * 1.1] },
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
            `paper: ${fmtNum(b.contribution_base, 3)} (weight share ${fmtPct(b.weight_base, 1)})`,
            b.imputed ? `imputed${b.rule ? `: ${b.rule}` : ''}` : null,
          ]}
        />
      )}
    />
  );
}

export default function CountryView() {
  useTitle('Country profile');
  const app = useApp();
  const { client } = useEngine();
  const params = useParams();
  const goto = useGoto();
  const { scenario, key, show, isBaseline } = useScenario();
  const iso = params.iso3 && app.byIso.has(params.iso3) ? params.iso3 : app.defaultCountry;
  const c = app.byIso.get(iso) ?? app.countries[0];
  const { tech, stage } = useTechStage();
  const core = useCore({ contrib: true });
  const partner = usePartner(false);
  const network = useNetwork(false);
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
    return base ? { rank: numOr(base.rank), change: null, src: isBaseline ? 'paper' : 'paper (open Partners to recompute)' } : null;
  }, [partner, iso, app.baseline.psi_partner, isBaseline]);
  const netTile = useMemo(() => {
    const node = c.network_node;
    if (!node) return { value: 'Not a member', sub: 'not in the allied network' };
    const fromScenario = network.fresh && network.data;
    const spec = fromScenario ? network.data!.solution.specialization : app.baseline.network_summary?.specialization;
    if (!spec) return { value: DASH, sub: 'network not available' };
    const nodeCol = fromScenario ? 'iso3c' : 'network_node';
    const prod = rowsOf(spec).filter((r) => r[nodeCol] === node && (numOr(r.production_share) ?? 0) > 0);
    const src = fromScenario ? 'scenario network' : isBaseline ? "the paper's network" : "the paper's network (open Network to redesign)";
    const list = prod.slice(0, 4).map((r) => `${r.tech} ${String(r.supply_chain).toLowerCase()}`);
    return {
      value: `${prod.length} stage${prod.length === 1 ? '' : 's'}`,
      sub: `${src}${node !== iso ? `; node ${nodeLabel(app, node)}` : ''}${list.length ? `: ${list.join(', ')}${prod.length > 4 ? ', ...' : ''}` : ''}`,
    };
  }, [c.network_node, network, app, iso, isBaseline]);

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

  const dotsHeight = 60 + app.techs.length * 26;
  const buildDots = useCallback(
    (width: number): Plot.PlotOptions | null => {
      if (!pillars?.length) return null;
      const pts = pillars.filter((p) => p.shown !== null);
      const ghosts = show === 'scenario' && !isBaseline ? pillars.filter((p) => p.value_base !== null && p.value !== null && Math.abs(p.value - p.value_base) > 1e-12) : [];
      const change = show === 'change';
      const dom = change ? symDomain(pts.map((p) => p.shown), 0.01) : [0, 1];
      // a phone leaves ~65 px per stage panel: three gridlines, one or two labels per panel
      const narrow = width < 640;
      return {
        width,
        height: dotsHeight,
        marginLeft: labelMargin(app.techs, { min: 80, max: 160 }),
        marginTop: 28,
        // wider facet gaps and short end labels ('1', '0'), so the end ticks of adjacent facets do not run together
        fx: { domain: app.stages, label: null, axis: 'top', padding: 0.12 },
        x: {
          domain: dom,
          label: change ? 'Change against the paper' : 'Pillar value (0-1)',
          grid: true,
          nice: !narrow,
          ticks: narrow ? (change ? [dom[0], 0, dom[1]] : [0, 0.5, 1]) : 4,
          tickFormat: change
            ? (v: number) => (v === 0 ? '0' : narrow && v < 0 ? '' : fmtSigned(v, 2))
            : narrow
              ? (v: number) => (v === 0.5 ? '0.5' : '')
              : (v: number) => (v === 0 || v === 1 ? String(v) : v.toFixed(1)),
        },
        y: { domain: app.techs, label: null, tickSize: 0 },
        color: { domain: ['Energy Security', 'Economic Opportunity'], range: [PILLAR_COLOR.ES, PILLAR_COLOR.EO] },
        symbol: { domain: ['Energy Security', 'Economic Opportunity'], range: ['circle', 'diamond'] },
        marks: [
          change ? Plot.ruleX([0], { stroke: 'rgb(255 255 255 / 0.22)' }) : null,
          Plot.dot(ghosts, { fx: 'stage', y: 'tech', x: 'value_base', r: 4.5, stroke: COMPARE_COLOR.baseline, strokeWidth: 1.5, fill: 'none' }),
          Plot.dot(pts, { fx: 'stage', y: 'tech', x: 'shown', r: 4.5, fill: 'pillar', symbol: 'pillar', stroke: INK[800], strokeWidth: 1.5 }),
          Plot.dot(pts, Plot.pointer({ fx: 'stage', y: 'tech', x: 'shown', r: 8, fill: 'none', stroke: SIGNAL, strokeWidth: 2, maxRadius: 24 })),
        ],
      };
    },
    [pillars, show, isBaseline, app, dotsHeight],
  );

  const siCols: Col<Row>[] = [
    { id: 'rank', header: 'Rank', value: (r) => numOr(r.rank_in_country), num: true },
    { id: 'sector', header: 'Sector', value: (r) => strOr(r.sector), minWidth: 180 },
    { id: 'si', header: 'SI', value: (r) => numOr(r.strategic_index), cell: (r) => fmtNum(r.strategic_index, 3), num: true },
    ...SI_TERMS.map(([k, label]) => ({ id: k, header: label, value: (r: Row) => numOr(r[k]), cell: (r: Row) => fmtNum(r[k], 3), num: true })),
    { id: 'chg', header: 'Rank change', value: (r) => numOr(r.rank_delta), cell: (r) => <RankChip change={numOr(r.rank_delta)} isNew={r.in_sample_base !== true} /> },
    { id: 'rb', header: 'Resource base', value: (r) => resourceBaseText(r.resource_base, r.resource_evidence) },
  ];
  const varCols: Col<Row>[] = [
    { id: 'category', header: 'Category', value: (r) => strOr(r.category), minWidth: 140 },
    { id: 'variable', header: 'Variable', value: (r) => strOr(r.variable), minWidth: 160 },
    { id: 'component_value', header: 'Value', value: (r) => numOr(r.component_value), cell: (r) => fmtNum(r.component_value, 3), num: true },
    { id: 'w', header: 'Weight in category', value: (r) => numOr(r.component_weight_within_overall), cell: (r) => fmtPct(r.component_weight_within_overall, 1), num: true },
    { id: 'contrib', header: 'Contribution', value: (r) => numOr(r.weighted_variable_contribution), cell: (r) => fmtNum(r.weighted_variable_contribution, 3), num: true },
    { id: 'imputed', header: 'Imputed', value: (r) => (r.imputed === true ? `imputed${r.missing_rule_applied ? `: ${r.missing_rule_applied}` : ''}` : '') },
  ];

  const updating = !core.fresh;
  const pillarsMoved = (pillars ?? []).some((p) => p.value !== null && p.value_base !== null && Math.abs(p.value - p.value_base) > 1e-12);
  return (
    <div>
      <PageHead
        eyebrow="Profile"
        title={`Country profile: ${c.country}`}
        lede="One economy's Energy Security and Economic Opportunity across technologies and stages, what drives them, and where its Strategic Index points."
      />
      <FilterRow>
        <CountrySelect label="Economy" value={iso} options={options} onChange={(v) => goto(`/country/${v}`)} />
        <TechStageFilters techLabel="Technology (for contributions)" />
      </FilterRow>
      {!core.data ? (
        <Loading label="Computing" />
      ) : (
        <div className="flex flex-col gap-6">
          {core.data.notes.length > 0 && <Notice kind="info">{core.data.notes.map(noteText).join(' ')}</Notice>}
          <div className={`grid grid-cols-1 gap-3 sm:grid-cols-2 xl:grid-cols-4 ${updating ? 'chart-updating' : ''}`}>
            <StatTile label="Coalition tier" value={c.tier === 'none' ? 'Not a member' : tierLabel(c.tier)} sub={c.eu_member ? 'EU member (the EU is one network node)' : (c.region ?? undefined)} />
            <StatTile label="Rankable cells" value={`${rankableBoth} of ${cellKeys.size}`} sub="both pillars rankable" />
            <StatTile label="Mean ES" value={fmtNum(mean(es.map((p) => p.value)), 2)} sub={`over ${es.length} scored cells${!isBaseline ? `; paper ${fmtNum(mean(es.map((p) => p.value_base)), 2)}` : ''}`} />
            <StatTile label="Mean EO" value={fmtNum(mean(eo.map((p) => p.value)), 2)} sub={`over ${eo.length} scored cells${!isBaseline ? `; paper ${fmtNum(mean(eo.map((p) => p.value_base)), 2)}` : ''}`} />
            <StatTile
              label="Top SI sector"
              value={<span className="text-base">{top ? String(top.sector) : DASH}</span>}
              chip={top && !isBaseline ? <RankChip change={numOr(top.rank_delta)} isNew={top.in_sample_base !== true} /> : undefined}
              sub={top ? `SI ${fmtNum(top.strategic_index, 3)}` : 'no in-sample sector'}
            />
            <StatTile
              label="PSI rank as a partner"
              value={psiRow && psiRow.rank !== null ? String(psiRow.rank) : DASH}
              chip={psiRow && psiRow.change !== null && !isBaseline ? <RankChip change={psiRow.change} /> : undefined}
              sub={psiRow ? psiRow.src : 'not rated'}
            />
            <StatTile label="Network producer stages" value={<span className="text-base">{netTile.value}</span>} sub={netTile.sub} />
            <StatTile
              label="Policy index basis"
              value={<span className="text-base">{policyRow?.eligible === true ? strOr(policyRow.policy_basis) ?? DASH : 'None'}</span>}
              sub={
                policyRow?.eligible === true
                  ? `${policyBasisText(strOr(policyRow.policy_basis))}${
                      c.tier !== 'none'
                        ? `; allied pick ${alliedPick.length ? alliedPick.map((r) => String(r.sector)).join(', ') : 'none'}${core.data.params.si.require_resource_base === false ? ' (endowment screen off)' : ''}`
                        : ''
                    }`
                  : policyRow
                    ? `no Policy index: lacks ${strOr(policyRow.missing_required) ?? 'a required component'}, so no Strategic Index`
                    : 'no policy data: no Strategic Index'
              }
            />
          </div>

          <ChartCard
            title="Pillars by technology and stage"
            sub={`${showWord(show)} Energy Security (higher = more secure) and Economic Opportunity per cell. Hollow rings mark the paper's value where the scenario differs (only the advanced category weights move the pillars).${
              !isBaseline && !pillarsMoved ? " In this scenario no cell moves: the four dials leave the pillars at the paper's values." : ''
            }`}
            updating={updating}
            summary={`Dot plot of ${es.length} Energy Security and ${eo.length} Economic Opportunity cells for ${c.country}, faceted by stage.`}
            legend={
              <Legend
                items={[
                  { label: 'Energy Security (circle)', kind: 'dot', color: PILLAR_COLOR.ES },
                  { label: 'Economic Opportunity (diamond)', kind: 'diamond', color: PILLAR_COLOR.EO },
                  { label: 'Paper baseline (where different)', kind: 'hollow', color: COMPARE_COLOR.baseline },
                ]}
              />
            }
            table={() => (
              <TableView
                rows={pillars ?? []}
                caption={`Pillars of ${c.country}`}
                search
                cols={[
                  { id: 'pillar', header: 'Pillar', value: (p) => p.pillar },
                  { id: 'tech', header: 'Technology', value: (p) => p.tech },
                  { id: 'stage', header: 'Stage', value: (p) => p.stage },
                  { id: 'value', header: 'Scenario', value: (p) => p.value, cell: (p) => fmtNum(p.value, 3), num: true },
                  { id: 'value_base', header: 'Paper', value: (p) => p.value_base, cell: (p) => fmtNum(p.value_base, 3), num: true },
                  { id: 'rankable', header: 'Rankable', value: (p) => (p.rankable ? 'yes' : `no${p.reason ? `: ${p.reason}` : ''}`) },
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
                ariaLabel={`Pillars of ${c.country}`}
                tooltip={(p) => (
                  <Tip
                    value={fmtNum(p.shown, 3)}
                    title={`${p.pillar}: ${cellLabel(p.tech, p.stage)}`}
                    lines={[!isBaseline ? `scenario ${fmtNum(p.value, 3)} · paper ${fmtNum(p.value_base, 3)}` : null, !p.rankable ? `not rankable${p.reason ? `: ${p.reason}` : ''}` : null]}
                  />
                )}
              />
            ) : (
              <EmptyState>No pillar data for this economy.</EmptyState>
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
                title={`${name} – ${tech}, ${stage}`}
                sub="Contribution of each category = score × weight share. Change adds the paper's contribution as an outline."
                updating={updating}
                summary={`${name} category contributions for ${c.country}, ${cellLabel(tech, stage)}.`}
                legend={show === 'change' ? <Legend items={[{ label: 'Scenario', color }, { label: 'Paper baseline', kind: 'outline', color: COMPARE_COLOR.baseline }]} /> : undefined}
                table={() => (
                  <TableView
                    rows={bars}
                    caption={`${name} contributions`}
                    cols={[
                      { id: 'category', header: 'Category', value: (b) => b.category },
                      { id: 'score', header: 'Score', value: (b) => b.score, cell: (b) => fmtNum(b.score, 3), num: true },
                      { id: 'weight', header: 'Weight share', value: (b) => b.weight, cell: (b) => fmtPct(b.weight, 1), num: true },
                      { id: 'contribution', header: 'Contribution', value: (b) => b.contribution, cell: (b) => fmtNum(b.contribution, 3), num: true },
                      { id: 'contribution_base', header: 'Paper', value: (b) => b.contribution_base, cell: (b) => fmtNum(b.contribution_base, 3), num: true },
                      { id: 'imputed', header: 'Imputed', value: (b) => (b.imputed ? 'imputed' : '') },
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
            title={`Variables behind ${cellLabel(tech, stage)}`}
            sub="Component values and their weighted contribution to the pillar under the scenario's category weights; imputed values are flagged in words."
            tools={
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
            sub="The economy's ten highest-ranked in-sample sectors under the scenario, with the five weighted terms."
            updating={updating}
            tools={
              <button type="button" className="btn btn-ghost btn-sm" onClick={() => goto(`/strategic/${iso}`)}>
                Open Strategic Index
              </button>
            }
            csv={() => ({ name: `strategic-index-top-${iso}`, text: rowsToCsv(topSi as Record<string, unknown>[]) })}
          >
            {topSi.length ? (
              <TableView rows={topSi} cols={siCols} caption="Top Strategic Index sectors" pageSize={10} />
            ) : (
              <EmptyState>This economy has no sectors in the Strategic Index sample (it needs ES, EO and a Policy value).</EmptyState>
            )}
          </ChartCard>
        </div>
      )}
    </div>
  );
}
