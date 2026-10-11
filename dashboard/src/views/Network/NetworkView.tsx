// View 6: Allied network (spec_react.md 8.6; paper section 10). For each upstream and midstream
// stage the design chooses which coalition nodes produce and who supplies whom. Scenarios are
// re-designed in the browser with the greedy solver; the paper's exact (MILP) design is shipped for
// the baseline when the export found it consistent with the current inputs.

import { useCallback, useMemo, useState, type ReactNode } from 'react';
import { useParams } from 'react-router';
import * as Plot from '@observablehq/plot';
import { useTitle } from '../../App.tsx';
import { ChartCard } from '../../charts/ChartCard.tsx';
import { Legend, RampLegend } from '../../charts/legends.tsx';
import { PlotFigure, Tip } from '../../charts/PlotFigure.tsx';
import { SankeyChart } from '../../charts/Sankey.tsx';
import { divScale, labelMargin, seqScale, textWidth } from '../../charts/theme.ts';
import { WorldMap, type MapCell } from '../../charts/WorldMap.tsx';
import { TableView, type Col } from '../../components/TableView.tsx';
import { EmptyState, FilterRow, Loading, Notice, PageHead, ProgressBar, Segmented, Select, Tag } from '../../components/ui.tsx';
import { rowsToCsv, tableToCsv } from '../../lib/csv.ts';
import { cellLabel, DASH, fmtDelta, fmtMs, fmtNum, fmtPct, fmtSigned, stageSlug } from '../../lib/format.ts';
import { nodeLabel } from '../../state/appData.ts';
import { useApp, useNetwork, useRun, useScenario } from '../../state/EngineProvider.tsx';
import { useGoto } from '../../state/nav.ts';
import type { CmpMode } from '../../state/url.ts';
import { COMPARE_COLOR, FLOW, INK, SIGNAL } from '../../theme/palette.ts';
import { numOr, rowsOf, strOr } from '../common.tsx';
import {
  compareSpecs, designGap, flowRows, specRows, stageId, stagesOf, workerCompare, type FlowRow, type NodeCompareRow, type StageCompareRow, type StageRef,
} from './derive.ts';

const stageShort = (s: StageRef) => `${s.tech} · ${s.supply_chain.slice(0, 2)}`;

export default function NetworkView() {
  useTitle('Allied network');
  const app = useApp();
  const params = useParams();
  const goto = useGoto();
  const { show, isBaseline, view, setView } = useScenario();
  const network = useNetwork(true);
  const { status } = useRun();
  const consistent = app.manifest.flags?.network_consistent === true;
  const summary = app.baseline.network_summary;
  const paperIsMilp = consistent && summary?.source === 'milp';
  const cmp: CmpMode = view.cmp === 'paper' && paperIsMilp ? 'paper' : 'greedy';
  const [tip, setTip] = useState<{ x: number; y: number; content: ReactNode } | null>(null);
  const [hoverLink, setHoverLink] = useState<number | null>(null);

  // stages: the paper's solved stages; those missing from the scenario are excluded (greenness)
  const order = (s: StageRef) => app.techs.indexOf(s.tech) * 10 + app.stages.indexOf(s.supply_chain);
  const allStages = useMemo(() => {
    const base = stagesOf(summary?.diagnostics);
    const cur = stagesOf(network.data?.solution.diagnostics);
    const m = new Map<string, StageRef>();
    for (const s of [...base, ...cur]) m.set(stageId(s), s);
    return [...m.values()].sort((a, b) => order(a) - order(b));
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [summary, network.data, app]);
  const curIds = useMemo(() => new Set(stagesOf(network.data?.solution.diagnostics).map(stageId)), [network.data]);
  const bySlug = new Map(allStages.map((s) => [stageSlug(s.tech, s.supply_chain), s]));
  const fallback = allStages.find((s) => s.tech === app.defaultTech && s.supply_chain === app.defaultStage) ?? allStages[0];
  const stage = (params.stage && bySlug.get(params.stage)) || fallback;
  const sid = stage ? stageId(stage) : '';
  const stageExcluded = !!network.data && stage && !curIds.has(sid);

  const sol = network.data?.solution;
  const spec = useMemo(() => specRows(sol?.specialization), [sol]);
  const flows = useMemo(() => flowRows(sol?.flows), [sol]);
  const ref = useMemo((): { stage: StageCompareRow[]; node: NodeCompareRow[] } | null => {
    if (!network.data) return null;
    if (cmp === 'paper' && summary) return compareSpecs(spec, specRows(summary.specialization));
    return workerCompare(network.data.compare);
  }, [network.data, cmp, summary, spec]);

  const stageSpec = spec.filter((r) => stageId(r) === sid);
  const stageFlows = flows.filter((f) => stageId(f) === sid);
  const nodeTotals = useMemo(() => {
    const t = new Map<string, number>();
    for (const r of spec) t.set(r.node, (t.get(r.node) ?? 0) + r.share);
    return t;
  }, [spec]);

  // ---- solver line ------------------------------------------------------------------------------
  const nStages = network.data ? stagesOf(network.data.solution.diagnostics).length : 0;
  const method = network.data?.method;
  const solverLine = !network.data
    ? null
    : isBaseline
      ? method === 'milp'
        ? "The paper's network: the exact (MILP) design."
        : `Greedy design at the paper's settings${consistent ? '' : ' (the stored exact design predates the current inputs)'}, ${nStages} stages.`
      : `Scenario: fast greedy design, ${nStages} stages${network.data.elapsed_ms ? `, ${fmtMs(network.data.elapsed_ms)}` : ''}.`;
  const gap = useMemo(() => {
    if (!network.data || !isBaseline || method !== 'milp' || !network.data.compare) return null;
    const w = workerCompare(network.data.compare);
    return w ? designGap(w.stage, w.node) : null;
  }, [network.data, isBaseline, method]);

  const progress = status.progress?.layer === 'network' ? status.progress : null;
  const solving = !network.fresh && (status.phase === 'running' || status.phase === 'waiting');

  // ---- flow map ---------------------------------------------------------------------------------
  const mapCells = useMemo(() => {
    const m = new Map<string, MapCell>();
    for (const c of app.countries) {
      if (!c.network_node || !c.iso_n3) continue;
      m.set(c.iso_n3, {
        iso3c: c.iso3c,
        fill: INK[600],
        tip: <Tip title={c.country} lines={[`coalition ${c.tier === 'core' ? 'core' : 'partner'}; node ${nodeLabel(app, c.network_node)}`, 'click for the country profile']} />,
      });
    }
    return m;
  }, [app]);
  const maxShare = Math.max(0.01, ...stageSpec.map((r) => r.share));
  const overlay = useCallback(
    (projection: (p: [number, number]) => [number, number] | null) => {
      const pos = new Map<string, [number, number]>();
      for (const n of app.nodes) {
        if (n.lon === null || n.lat === null) continue;
        const p = projection([n.lon, n.lat]);
        if (p) pos.set(n.network_node, p);
      }
      const links = stageFlows.filter((f) => f.producer !== f.consumer && pos.has(f.producer) && pos.has(f.consumer));
      const shareOf = new Map(stageSpec.map((r) => [r.node, r]));
      const self = new Map(stageFlows.filter((f) => f.producer === f.consumer).map((f) => [f.producer, f.share]));
      const onTip = (e: React.PointerEvent, content: ReactNode) => {
        const r = (e.currentTarget as SVGElement).ownerSVGElement?.getBoundingClientRect();
        if (r) setTip({ x: e.clientX - r.left, y: e.clientY - r.top, content });
      };
      return (
        <g>
          <g fill="none">
            {links.map((f, i) => {
              const [x1, y1] = pos.get(f.producer)!;
              const [x2, y2] = pos.get(f.consumer)!;
              const mx = (x1 + x2) / 2 - (y2 - y1) * 0.15;
              const my = (y1 + y2) / 2 + (x2 - x1) * 0.15;
              return (
                <path
                  key={i}
                  d={`M${x1},${y1}Q${mx},${my} ${x2},${y2}`}
                  stroke={hoverLink === i ? SIGNAL : FLOW}
                  strokeWidth={0.6 + 40 * f.share}
                  strokeLinecap="round"
                  onPointerMove={(e) => {
                    setHoverLink(i);
                    onTip(e, <Tip value={fmtPct(f.share, 1)} title={`${nodeLabel(app, f.producer)} → ${nodeLabel(app, f.consumer)}`} lines={['share of the stage’s coalition demand', `edge weight ${fmtNum(f.edge_weight, 3)}`]} />);
                  }}
                  onPointerLeave={() => {
                    setHoverLink(null);
                    setTip(null);
                  }}
                />
              );
            })}
          </g>
          <g>
            {app.nodes.map((n) => {
              const p = pos.get(n.network_node);
              if (!p) return null;
              const s = shareOf.get(n.network_node);
              const share = s?.share ?? 0;
              const producer = share > 0;
              const r = producer ? 3 + 16 * Math.sqrt(share / maxShare) : 3.5;
              return (
                <circle
                  key={n.network_node}
                  cx={p[0]}
                  cy={p[1]}
                  r={r}
                  fill={producer ? COMPARE_COLOR.scenario : 'none'}
                  stroke={producer ? INK[900] : COMPARE_COLOR.baseline}
                  strokeWidth={producer ? 1.5 : 1.5}
                  onPointerMove={(e) =>
                    onTip(
                      e,
                      <Tip
                        value={producer ? fmtPct(share, 1) : 'consumer only'}
                        title={`${n.label}${n.n_members > 1 ? ` (${n.n_members} members)` : ''}`}
                        lines={[
                          producer ? 'production share' : null,
                          s?.score !== null && s?.score !== undefined ? `producer score ${fmtNum(s.score, 3)}` : null,
                          self.get(n.network_node) ? `self-supply ${fmtPct(self.get(n.network_node), 1)}` : null,
                          n.n_members > 1 ? "members' shares are derived from the node share" : null,
                        ]}
                      />,
                    )
                  }
                  onPointerLeave={() => setTip(null)}
                />
              );
            })}
          </g>
        </g>
      );
    },
    [app, stageFlows, stageSpec, maxShare, hoverLink],
  );

  // ---- specialization matrix --------------------------------------------------------------------
  const matrix = useMemo(() => {
    const stagesCur = allStages.filter((s) => curIds.has(stageId(s)));
    const refBy = new Map((ref?.node ?? []).map((r) => [`${stageId(r)}|${r.node}`, r]));
    const nodes = app.nodes.map((n) => n.network_node).sort((a, b) => (nodeTotals.get(b) ?? 0) - (nodeTotals.get(a) ?? 0));
    const specBy = new Map(spec.map((r) => [`${stageId(r)}|${r.node}`, r]));
    const data: { stage: string; stageRef: StageRef; node: string; label: string; share: number; ref: number; delta: number; shown: number }[] = [];
    for (const s of stagesCur) {
      for (const n of nodes) {
        const k = `${stageId(s)}|${n}`;
        const cur = specBy.get(k)?.share ?? 0;
        const rr = refBy.get(k);
        const refShare = rr ? rr.share_ref : cur;
        const shown = show === 'change' ? cur - refShare : show === 'baseline' ? refShare : cur;
        data.push({ stage: stageShort(s), stageRef: s, node: n, label: nodeLabel(app, n), share: cur, ref: refShare, delta: cur - refShare, shown });
      }
    }
    return { data, stages: stagesCur.map(stageShort), labels: nodes.map((n) => nodeLabel(app, n)) };
  }, [allStages, curIds, ref, spec, app, nodeTotals, show]);
  const matrixHeight = 120 + 15 * matrix.labels.length;
  const buildMatrix = useCallback(
    (width: number): Plot.PlotOptions | null => {
      if (!matrix.data.length) return null;
      const change = show === 'change';
      const L = Math.max(1e-6, ...matrix.data.map((d) => Math.abs(d.delta)));
      const hi = Math.max(0.01, ...matrix.data.map((d) => d.shown));
      const curLabel = stage ? stageShort(stage) : '';
      return {
        width,
        height: matrixHeight,
        marginLeft: labelMargin(matrix.labels, { min: 110, max: 220 }),
        marginBottom: Math.ceil(0.82 * Math.max(...matrix.stages.map((s) => textWidth(s)))) + 16,
        marginTop: 8,
        padding: 0,
        x: { domain: matrix.stages, label: null, tickRotate: -55, tickSize: 0 },
        y: { domain: matrix.labels, label: null, tickSize: 0 },
        color: change ? divScale(L) : seqScale([0, hi]),
        marks: [
          Plot.cell(
            matrix.data.filter((d) => change || d.shown > 0),
            { x: 'stage', y: 'label', fill: 'shown', inset: 0.5 },
          ),
          Plot.cell(matrix.data.filter((d) => !change && d.shown <= 0), { x: 'stage', y: 'label', fill: INK[700], inset: 0.5 }),
          Plot.cell(
            matrix.data.filter((d) => d.stage === curLabel),
            { x: 'stage', y: 'label', fill: 'none', stroke: SIGNAL, strokeWidth: 1, inset: 0 },
          ),
          Plot.cell(matrix.data, Plot.pointer({ x: 'stage', y: 'label', fill: 'none', stroke: SIGNAL, strokeWidth: 2, maxRadius: 10 })),
        ],
      };
    },
    [matrix, matrixHeight, show, stage],
  );

  // ---- concentration ----------------------------------------------------------------------------
  const conc = useMemo(() => {
    if (!network.data) return [];
    const refHhi = new Map((ref?.stage ?? []).map((r) => [stageId(r), r.hhi_ref]));
    return rowsOf(network.data.concentration)
      .map((r) => {
        const s = { tech: String(r.tech), supply_chain: String(r.supply_chain) };
        const h = numOr(r.hhi_designed);
        const hr = refHhi.get(stageId(s)) ?? numOr(r.hhi_baseline);
        return {
          ...s,
          label: stageShort(s),
          hhi: show === 'baseline' ? hr : h,
          hhi_scenario: h,
          hhi_ref: hr,
          hhi_obs: numOr(r.hhi_observed),
          top_node: strOr(r.top_node),
          top_share: numOr(r.top_share),
          n: numOr(r.n_producers),
        };
      })
      .sort((a, b) => order(a) - order(b));
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [network.data, ref, show, app]);
  type ConcRow = (typeof conc)[number];
  const buildConc = useCallback(
    (width: number): Plot.PlotOptions | null => {
      if (!conc.length) return null;
      const curLabel = stage ? stageShort(stage) : '';
      return {
        width,
        height: 40 + conc.length * 20,
        marginLeft: labelMargin(conc.map((c) => `▸ ${c.label}`), { min: 110, max: 200 }),
        x: { label: 'Herfindahl index of production shares (lower = more diversified)', grid: true, nice: true, domain: [0, Math.max(0.1, ...conc.flatMap((c) => [c.hhi ?? 0, c.hhi_ref ?? 0, c.hhi_obs ?? 0])) * 1.05] },
        y: { domain: conc.map((c) => c.label), label: null, tickSize: 0, tickFormat: (t: string) => (t === curLabel ? `▸ ${t}` : t) },
        marks: [
          Plot.ruleY(conc, { y: 'label', x1: 0, x2: (c: ConcRow) => Math.max(c.hhi ?? 0, c.hhi_ref ?? 0, c.hhi_obs ?? 0), stroke: 'rgb(255 255 255 / 0.06)' }),
          Plot.dot(conc.filter((c) => c.hhi_obs !== null), { y: 'label', x: 'hhi_obs', r: 4, fill: COMPARE_COLOR.observed, symbol: 'square' }),
          show !== 'baseline' && !isBaseline ? Plot.dot(conc, { y: 'label', x: 'hhi_ref', r: 4.5, stroke: COMPARE_COLOR.baseline, strokeWidth: 1.5, fill: 'none' }) : null,
          Plot.dot(conc, { y: 'label', x: 'hhi', r: 4.5, fill: COMPARE_COLOR.scenario, stroke: INK[800], strokeWidth: 1.5 }),
          Plot.dot(conc, Plot.pointerY({ y: 'label', x: 'hhi', r: 8, fill: 'none', stroke: SIGNAL, strokeWidth: 2 })),
        ],
      };
    },
    [conc, stage, show, isBaseline],
  );

  // ---- portfolio caps ---------------------------------------------------------------------------
  const counts = useMemo(
    () =>
      rowsOf(sol?.portfolio?.counts)
        .map((r) => ({ node: String(r.iso3c), label: nodeLabel(app, String(r.iso3c)), count: numOr(r.topk_count) ?? 0, cap: numOr(r.portfolio_cap), over: numOr(r.over_by) ?? 0 }))
        .sort((a, b) => b.count - a.count || a.label.localeCompare(b.label)),
    [sol, app],
  );
  type CountRow = (typeof counts)[number];
  const buildCaps = useCallback(
    (width: number): Plot.PlotOptions | null => {
      if (!counts.length) return null;
      // bar insets that leave a visible bar at phone width (37 nodes in ~260 px)
      const inset = width / counts.length > 18 ? 3 : 0.5;
      return {
        width,
        height: 300,
        marginTop: 28,
        marginBottom: Math.ceil(0.82 * Math.max(...counts.map((c) => textWidth(c.label)))) + 16,
        marginLeft: 40,
        x: { domain: counts.map((c) => c.label), label: null, tickRotate: -55, tickSize: 0 },
        y: { label: '↑ Stages among the top five producers', grid: true, nice: true },
        marks: [
          Plot.barY(counts, { x: 'label', y: 'count', fill: COMPARE_COLOR.scenario, insetLeft: inset, insetRight: inset }),
          Plot.tickY(counts.filter((c) => c.cap !== null), { x: 'label', y: 'cap', stroke: '#d1d5db', strokeWidth: 2 }),
          Plot.text(counts.filter((c) => c.over > 0), { x: 'label', y: 'count', text: (c: CountRow) => `over by ${c.over}`, dy: -8, fill: '#ffffff', fontSize: 10 }),
          Plot.ruleY([0], { stroke: 'rgb(255 255 255 / 0.22)' }),
          Plot.barY(counts, Plot.pointerX({ x: 'label', y: 'count', fill: 'none', stroke: SIGNAL, strokeWidth: 2, insetLeft: Math.min(2, inset), insetRight: Math.min(2, inset) })),
        ],
      };
    },
    [counts],
  );

  const members = useMemo(
    () =>
      rowsOf(sol?.member_shares)
        .filter((r) => r.tech === stage?.tech && r.supply_chain === stage?.supply_chain && (numOr(r.node_production_share) ?? 0) > 0)
        .map((r) => ({
          country: String(r.country ?? r.iso3c),
          node: nodeLabel(app, strOr(r.network_node)),
          node_share: numOr(r.node_production_share),
          weight: numOr(r.member_weight),
          share: numOr(r.production_share_derived),
          derived: r.share_is_derived === true,
          basis: strOr(r.weight_basis),
        }))
        .sort((a, b) => (b.share ?? 0) - (a.share ?? 0)),
    [sol, stage, app],
  );

  const changesStage = ref?.stage.filter((r) => r.changed).sort((a, b) => order(a) - order(b)) ?? [];
  const movers = useMemo(() => {
    const moved = (ref?.node ?? []).filter((r) => Math.abs(r.delta) > 1e-6);
    const gains = moved.filter((r) => r.delta > 0).sort((a, b) => b.delta - a.delta).slice(0, 8);
    const losses = moved.filter((r) => r.delta < 0).sort((a, b) => a.delta - b.delta).slice(0, 8);
    return [...gains, ...losses];
  }, [ref]);

  const stageCols: Col<StageCompareRow>[] = [
    { id: 'stage', header: 'Stage', value: (r) => cellLabel(r.tech, r.supply_chain), minWidth: 170 },
    { id: 'in', header: 'In', value: (r) => r.producers_added || null, minWidth: 110 },
    { id: 'out', header: 'Out', value: (r) => r.producers_removed || null, minWidth: 110 },
    { id: 'jaccard', header: 'Jaccard', value: (r) => r.jaccard, cell: (r) => fmtNum(r.jaccard, 2), num: true, title: 'Overlap of the producer sets (1 = identical)' },
    { id: 'hhi_delta', header: 'HHI change', value: (r) => r.hhi_delta, cell: (r) => fmtSigned(r.hhi_delta, 3), num: true },
    { id: 'max', header: 'Max |share change|', value: (r) => r.max_abs_share_delta, cell: (r) => fmtNum(r.max_abs_share_delta, 3), num: true },
    { id: 'turn', header: 'Flow turnover', value: (r) => r.flow_turnover, cell: (r) => fmtNum(r.flow_turnover, 3), num: true },
  ];

  const stageOptions = allStages.map((s) => ({
    value: stageSlug(s.tech, s.supply_chain),
    label: `${cellLabel(s.tech, s.supply_chain)}${network.data && !curIds.has(stageId(s)) ? ' (excluded by greenness)' : ''}`,
    disabled: !!network.data && !curIds.has(stageId(s)),
  }));
  const refName = cmp === 'paper' ? "the paper's exact design" : "the greedy design at the paper's settings";

  return (
    <div>
      <PageHead
        eyebrow="Network design"
        title="Allied network"
        lede="For each upstream and midstream stage, the network design chooses which coalition members produce and who supplies whom, maximising producer quality and partnership strength while penalising concentration. Move the dials and the network is re-designed."
      />
      <FilterRow>
        <Select label="Stage" value={stage ? stageSlug(stage.tech, stage.supply_chain) : ''} options={stageOptions} onChange={(v) => goto(`/network/${v}`, { cmp: view.cmp })} className="min-w-[16rem]" />
        {!isBaseline && (
          <Segmented<CmpMode>
            label="Compare with"
            value={cmp}
            options={[
              { value: 'greedy', label: "Greedy at the paper's settings" },
              { value: 'paper', label: "The paper's exact design", disabled: !paperIsMilp, title: paperIsMilp ? undefined : 'The exact design shipped with this build predates the current inputs' },
            ]}
            onChange={(v) => setView({ cmp: v })}
          />
        )}
      </FilterRow>
      <div className="mb-6 space-y-3">
        {solverLine && (
          <p className="flex flex-wrap items-center gap-2 text-sm text-paper-dim">
            <Tag accent>{isBaseline ? (method === 'milp' ? 'Exact design' : 'Greedy design') : 'Designed live'}</Tag>
            {solverLine} {!isBaseline && `Comparisons are against ${refName}.`}
          </p>
        )}
        {solving && (
          <div className="max-w-xl">
            <ProgressBar
              value={progress ? progress.current + 1 : 0}
              max={progress?.total ?? nStages ?? 1}
              label={progress ? `${progress.phase === 'portfolio' ? `Portfolio caps, iteration ${progress.iter ?? 1}` : 'Designing'}: ${progress.tech ? cellLabel(progress.tech, progress.supply_chain ?? '') : 'preparing'}` : 'Designing the network'}
            />
          </div>
        )}
        <Notice kind="info">
          Exact re-solves of scenarios are not available in the browser; scenarios use the fast greedy solver, which is close to the exact design at the paper&rsquo;s settings.
          {gap && ` At the paper's settings the greedy design differs from the exact one by a mean absolute share difference of ${fmtNum(gap.meanAbs, 3)}, with a median producer-set overlap (Jaccard) of ${fmtNum(gap.medianJaccard, 2)}.`}
          {!consistent && ' The exact design stored with this build predates the current inputs, so the paper baseline here is the greedy design at the paper’s settings.'}
        </Notice>
        {stageExcluded && <Notice kind="warning">This stage is not in the current design: fossil technologies are dropped at greenness 90 or above (or by the advanced setting).</Notice>}
      </div>
      {!network.data ? (
        <Loading label={solving && progress ? `Designing the network: stage ${progress.current + 1} of ${progress.total}` : 'Loading network data'} />
      ) : (
        <div className="flex flex-col gap-6">
          <div className="grid gap-6">
            <ChartCard
              title={stage ? `${cellLabel(stage.tech, stage.supply_chain)}: designed flows` : 'Designed flows'}
              sub="Coalition nodes at their location (the EU is one node). Filled circles produce, sized by production share; hollow circles only consume. Lines carry the designed flows, thicker = larger. The map always shows the scenario design."
              updating={!network.fresh}
              summary={`Flow map with ${stageFlows.length} flows and ${stageSpec.filter((r) => r.share > 0).length} producers.`}
              legend={
                <Legend
                  items={[
                    { label: 'Producer (size = share)', kind: 'dot', color: COMPARE_COLOR.scenario },
                    { label: 'Consumer only', kind: 'hollow', color: COMPARE_COLOR.baseline },
                    { label: 'Designed flow', kind: 'line', color: FLOW },
                    { label: 'Coalition member', color: INK[600] },
                  ]}
                />
              }
              table={() => (
                <TableView
                  rows={stageFlows}
                  caption="Designed flows"
                  search
                  initialSort={{ id: 'share', desc: true }}
                  cols={[
                    { id: 'producer', header: 'Producer', value: (f: FlowRow) => nodeLabel(app, f.producer), minWidth: 130 },
                    { id: 'consumer', header: 'Consumer', value: (f) => nodeLabel(app, f.consumer), minWidth: 130 },
                    { id: 'share', header: 'Flow share', value: (f) => f.share, cell: (f) => fmtPct(f.share, 1), num: true },
                    { id: 'ew', header: 'Edge weight', value: (f) => f.edge_weight, cell: (f) => fmtNum(f.edge_weight, 3), num: true },
                    { id: 'fs', header: 'Friendshore', value: (f) => f.fs, cell: (f) => fmtNum(f.fs, 3), num: true },
                    { id: 'po', header: 'Opportunity', value: (f) => f.po, cell: (f) => fmtNum(f.po, 3), num: true },
                  ]}
                />
              )}
              csv={() => ({ name: `network-flows-${stage?.tech}-${stage?.supply_chain}`, text: rowsToCsv(stageFlows as unknown as Record<string, unknown>[]) })}
            >
              <WorldMap cells={mapCells} ariaLabel="Map of the designed flows" overlay={overlay} overlayTip={tip} onSelect={(iso) => goto(`/country/${iso}`)} />
            </ChartCard>
            <ChartCard
              title="Who supplies whom"
              sub="Producers (left) to consumers (right); band width = share of the stage's coalition demand."
              updating={!network.fresh}
              summary={`Sankey of ${stageFlows.length} flows.`}
              legend={<Legend items={[{ label: 'Producer', color: COMPARE_COLOR.scenario }, { label: 'Consumer', color: '#6b7280' }, { label: 'Flow (hovered: orange)', kind: 'line', color: FLOW }]} />}
            >
              {stageFlows.length ? (
                <SankeyChart flows={stageFlows.map((f) => ({ producer: f.producer, consumer: f.consumer, value: f.share }))} label={(c) => nodeLabel(app, c)} ariaLabel="Producer to consumer flows" />
              ) : (
                <EmptyState>No flows in this stage.</EmptyState>
              )}
            </ChartCard>
          </div>

          <ChartCard
            title="Specialization matrix"
            sub={`Production share of each node in every stage${show === 'change' ? `; Change maps the difference from ${refName}` : show === 'baseline' ? `; Baseline shows ${refName}` : ''}. The selected stage is outlined; click a column to select its stage.`}
            updating={!network.fresh}
            summary={`Matrix of ${matrix.labels.length} nodes by ${matrix.stages.length} stages.`}
            legend={
              <div className="flex flex-wrap items-end gap-x-8 gap-y-3">
                <RampLegend
                  kind={show === 'change' ? 'div' : 'seq'}
                  min={show === 'change' ? -Math.max(1e-6, ...matrix.data.map((d) => Math.abs(d.delta))) : 0}
                  max={show === 'change' ? Math.max(1e-6, ...matrix.data.map((d) => Math.abs(d.delta))) : Math.max(0.01, ...matrix.data.map((d) => d.shown))}
                  label={show === 'change' ? 'Share change (increase = blue)' : 'Production share'}
                  format={(v) => fmtPct(v, 0)}
                />
                {show !== 'change' && <Legend items={[{ label: 'Not producing', color: INK[700] }]} />}
              </div>
            }
            table={() => (
              <TableView
                rows={matrix.data.filter((d) => d.share > 0 || d.ref > 0)}
                caption="Specialization"
                search
                cols={[
                  { id: 'label', header: 'Node', value: (d) => d.label, minWidth: 130 },
                  { id: 'stage', header: 'Stage', value: (d) => cellLabel(d.stageRef.tech, d.stageRef.supply_chain), minWidth: 160 },
                  { id: 'share', header: 'Share', value: (d) => d.share, cell: (d) => fmtPct(d.share, 1), num: true },
                  { id: 'ref', header: 'Comparison', value: (d) => d.ref, cell: (d) => fmtPct(d.ref, 1), num: true },
                  { id: 'delta', header: 'Change', value: (d) => d.delta, cell: (d) => fmtDelta(d.delta, 1, true), num: true },
                ]}
              />
            )}
            csv={() => ({ name: 'network-specialization', text: sol ? tableToCsv(sol.specialization) : '' })}
          >
            <div className="overflow-x-auto">
              <PlotFigure<(typeof matrix.data)[number]>
                build={buildMatrix}
                height={matrixHeight}
                minWidth={720}
                ariaLabel="Specialization matrix"
                tooltip={(d) => (
                  <Tip
                    value={show === 'change' ? fmtDelta(d.delta, 1, true) : fmtPct(d.shown, 1)}
                    title={`${d.label}: ${cellLabel(d.stageRef.tech, d.stageRef.supply_chain)}`}
                    lines={[`scenario ${fmtPct(d.share, 1)} · comparison ${fmtPct(d.ref, 1)}`]}
                  />
                )}
                onSelect={(d) => goto(`/network/${stageSlug(d.stageRef.tech, d.stageRef.supply_chain)}`, { cmp: view.cmp })}
              />
            </div>
          </ChartCard>

          <div className="grid gap-6 2xl:grid-cols-2">
            <ChartCard
              title="Concentration: designed against observed"
              sub="Herfindahl index of production shares per stage (lower = more diversified); hover for the equivalent number of producers. The selected stage is marked with a triangle."
              updating={!network.fresh}
              summary={`Concentration of ${conc.length} stages.`}
              legend={
                <Legend
                  items={[
                    { label: show === 'baseline' ? 'Comparison design' : 'Scenario design', kind: 'dot', color: COMPARE_COLOR.scenario },
                    ...(show !== 'baseline' && !isBaseline ? [{ label: 'Comparison design', kind: 'hollow' as const, color: COMPARE_COLOR.baseline }] : []),
                    { label: 'Observed exports (stages with a basket)', kind: 'rect' as const, color: COMPARE_COLOR.observed },
                  ]}
                />
              }
              table={() => (
                <TableView
                  rows={conc}
                  caption="Concentration"
                  cols={[
                    { id: 'stage', header: 'Stage', value: (c) => cellLabel(c.tech, c.supply_chain), minWidth: 160 },
                    { id: 'hhi', header: 'HHI scenario', value: (c) => c.hhi_scenario, cell: (c) => fmtNum(c.hhi_scenario, 3), num: true },
                    { id: 'hhi_ref', header: 'HHI comparison', value: (c) => c.hhi_ref, cell: (c) => fmtNum(c.hhi_ref, 3), num: true },
                    { id: 'hhi_obs', header: 'HHI observed', value: (c) => c.hhi_obs, cell: (c) => fmtNum(c.hhi_obs, 3), num: true },
                    { id: 'n', header: 'Producers', value: (c) => c.n, num: true },
                    { id: 'top', header: 'Top node', value: (c) => (c.top_node ? `${nodeLabel(app, c.top_node)} ${fmtPct(c.top_share, 0)}` : null) },
                  ]}
                />
              )}
              csv={() => ({ name: 'network-concentration', text: rowsToCsv(conc as unknown as Record<string, unknown>[]) })}
            >
              <PlotFigure<ConcRow>
                build={buildConc}
                height={40 + conc.length * 20}
                ariaLabel="Concentration by stage"
                tooltip={(c) => (
                  <Tip
                    value={`HHI ${fmtNum(c.hhi, 3)}`}
                    title={cellLabel(c.tech, c.supply_chain)}
                    lines={[
                      c.hhi ? `equivalent to ${(1 / c.hhi).toFixed(1)} equal producers` : null,
                      c.hhi_ref !== null ? `comparison ${fmtNum(c.hhi_ref, 3)}` : null,
                      c.hhi_obs !== null ? `observed exports ${fmtNum(c.hhi_obs, 3)} (${(1 / (c.hhi_obs as number)).toFixed(1)} equivalent exporters)` : 'no observed basket',
                    ]}
                  />
                )}
                onSelect={(c) => goto(`/network/${stageSlug(c.tech, c.supply_chain)}`, { cmp: view.cmp })}
              />
            </ChartCard>
            <ChartCard
              title={stage ? `Producers in ${cellLabel(stage.tech, stage.supply_chain)}` : 'Producers'}
              sub="Production share by member; members of a multi-country node (the EU) carry a share derived from the node's share and their weight, labelled 'derived'."
              updating={!network.fresh}
              csv={() => ({ name: `network-members-${stage?.tech}-${stage?.supply_chain}`, text: rowsToCsv(members as unknown as Record<string, unknown>[]) })}
            >
              {members.length ? (
                <TableView
                  rows={members}
                  caption="Producers and member shares"
                  pageSize={12}
                  cols={[
                    { id: 'country', header: 'Member', value: (m) => m.country, minWidth: 130 },
                    { id: 'node', header: 'Node', value: (m) => m.node },
                    { id: 'share', header: 'Share', value: (m) => m.share, cell: (m) => `${fmtPct(m.share, 1)}${m.derived ? ' (derived)' : ''}`, num: true },
                    { id: 'node_share', header: 'Node share', value: (m) => m.node_share, cell: (m) => fmtPct(m.node_share, 1), num: true },
                  ]}
                />
              ) : (
                <EmptyState>No member shares for this stage.</EmptyState>
              )}
            </ChartCard>
          </div>

          <ChartCard
            title="What changed"
            sub={`Per stage, against ${refName}: producers that enter or leave, the overlap of the producer sets (Jaccard), the change in concentration and the flow turnover. Then the nodes with the largest share gains and losses (in percentage points).`}
            updating={!network.fresh}
            csv={() => ({ name: 'network-changes', text: rowsToCsv(changesStage as unknown as Record<string, unknown>[]) })}
          >
            {isBaseline && method !== 'milp' ? (
              <EmptyState>At the paper&rsquo;s settings the design is the comparison design: nothing changed. Move a dial to compare.</EmptyState>
            ) : (
              <div className="space-y-5">
                {changesStage.length ? <TableView rows={changesStage} cols={stageCols} caption="Stage changes" pageSize={22} /> : <EmptyState>No stage changed.</EmptyState>}
                {movers.length > 0 && (
                  <TableView
                    rows={movers}
                    caption="Largest share gains and losses"
                    pageSize={16}
                    cols={[
                      { id: 'node', header: 'Node', value: (r) => nodeLabel(app, r.node), minWidth: 120 },
                      { id: 'stage', header: 'Stage', value: (r) => cellLabel(r.tech, r.supply_chain), minWidth: 160 },
                      { id: 'ref', header: 'Comparison', value: (r) => r.share_ref, cell: (r) => fmtPct(r.share_ref, 1), num: true },
                      { id: 'share', header: 'Scenario', value: (r) => r.share, cell: (r) => fmtPct(r.share, 1), num: true },
                      { id: 'delta', header: 'Change', value: (r) => r.delta, cell: (r) => fmtDelta(r.delta, 1, true), num: true },
                    ]}
                  />
                )}
              </div>
            )}
          </ChartCard>

          <ChartCard
            title="Portfolio caps"
            sub="Number of stages in which each node is among the five largest producers, against its cap (2 to 10, rising with log GDP). Nodes over their cap are labelled."
            updating={!network.fresh}
            legend={<Legend items={[{ label: 'Top-five stages', color: COMPARE_COLOR.scenario }, { label: 'Cap', kind: 'tick', color: '#d1d5db' }]} />}
            table={() => (
              <TableView
                rows={counts}
                caption="Portfolio caps"
                cols={[
                  { id: 'label', header: 'Node', value: (c) => c.label },
                  { id: 'count', header: 'Top-five stages', value: (c) => c.count, num: true },
                  { id: 'cap', header: 'Cap', value: (c) => c.cap, num: true },
                  { id: 'over', header: 'Over by', value: (c) => (c.over > 0 ? c.over : null), num: true },
                ]}
              />
            )}
          >
            {counts.length ? (
              <>
                <div className="overflow-x-auto">
                  <PlotFigure<CountRow>
                    build={buildCaps}
                    height={300}
                    minWidth={680}
                    ariaLabel="Portfolio caps by node"
                    tooltip={(c) => <Tip value={`${c.count} stages`} title={c.label} lines={[`cap ${c.cap ?? DASH}`, c.over > 0 ? `over the cap by ${c.over}` : 'within the cap']} />}
                  />
                </div>
                {sol?.portfolio && (
                  <p className="mt-2 text-xs text-paper-mute">
                    Cap loop {sol.portfolio.converged ? 'converged' : 'did not converge'}
                    {sol.portfolio.iters !== null ? ` after ${sol.portfolio.iters} iterations` : ''}
                    {sol.portfolio.violations_remaining ? `; ${sol.portfolio.violations_remaining} violations remain` : ''}.
                  </p>
                )}
              </>
            ) : (
              <EmptyState>Portfolio counts are not available for this design{network.data.method !== 'greedy' ? ' (the exact design ships without them)' : ''}.</EmptyState>
            )}
          </ChartCard>
        </div>
      )}
    </div>
  );
}

