// The Sector strategy card (design section 5, 2026-10-29; it replaces the posture module's need x prize card
// on the Sector pages): every economy of one technology x six-stage node as the canonical framework decides
// it (Need -> Prize -> Position -> Choice), as a ChartCard for the page's "Strategic posture" Section
// (heading level 3 by default). Within one node the prize barely varies across economies, so the chart plots
// Competitive Position against the relevant need, marks each economy by its verb, and says the node's prize
// and the decision rule in words. Two uses:
// - the Sector home: Technology and Stage selects in the card's tools, every stage offered (Deployment &
//   Use reads deployment need); the node lives in the URL (?pnode=Tech|Stage, checked by the posture
//   module's parsePostureNode, Downstream read as Deployment); a value not used as written is reported in an
//   "Ignored in the link" notice above the card.
// - a Sector profile (/sector/:tech/:stage): `node` fixes the card to that node (no selects, no URL key).
// The takeaway (six verbs, "led by" for named economies) is the sub and the figure's summary, the six-verb
// counts follow it; the legend and notes (the node's prize, the decision rule, trade freshness) sit in the
// card's slots; a table twin of the node's economies (a row opens the economy's Country Strategy Map on this
// node, like a point) and a CSV with the posture provenance.

import { useMemo, useState } from 'react';
import { useNavigate, useSearchParams } from 'react-router';
import { ChartCard } from '../../charts/ChartCard.tsx';
import { TableView, type Col } from '../../components/TableView.tsx';
import { Term } from '../../components/Term.tsx';
import { Loading, Notice, Select } from '../../components/ui.tsx';
import { DEPLOYMENT_STAGE, stageDisplay } from '../../lib/stages.ts';
import { POSTURE_TERMS } from '../../posture/basis.tsx';
import type { PostureCell } from '../../posture/contract.ts';
import {
  countPostures, countryNames, parsePostureNode, POSTURE_CSV_COLS, postureCsvRows, postureCsvText, postureProvenance, sectorPostureRows, strategyCountsText,
  type PostureRow,
} from '../../posture/derive.ts';
import { TradeFreshness } from '../../posture/PostureLegend.tsx';
import { usePostureCore } from '../../posture/store.ts';
import { SECTOR_POSTURE_COLS } from '../../posture/tables.tsx';
import {
  decisionWords, nodePrize, notPlottedReason, prizeWords, regionLabels, sectorStrategyTakeaway, sectorStrategyTitle, strategyHref, strategyPoints, verbRank, verbsOfStage,
} from './derive.ts';
import { SectorStrategyScatter, VerbMarkLegend } from './SectorStrategyScatter.tsx';

/** The URL key of the Sector home's node: ?pnode=Tech|Stage (stage ids and technology names, not slugs). */
export const SECTOR_NODE_KEY = 'pnode';

/** The card's title on the Sector home (a profile names its node: sectorStrategyTitle). */
export const SECTOR_STRATEGY_TITLE = "Every economy's choice in one node";

const TIER_WORD: Record<string, string> = { core: 'Core member', partner: 'Coalition partner' };

export interface SectorStrategyCardProps {
  /** fix the card to one node (a Sector profile); omit on the Sector home (selects + ?pnode) */
  node?: { tech: string; stage: string };
  /** default: SECTOR_STRATEGY_TITLE on the home, sectorStrategyTitle(tech, stage) with a node */
  title?: string;
  /** 3 (default) inside the page's Section; 2 only for a standalone mount */
  headingLevel?: 2 | 3;
  /** a narrower chart (a profile's section) */
  compact?: boolean;
  /** where a point or a table row goes (default: the economy's Country Strategy Map on this node) */
  countryHref?: (iso3: string, tech: string, stage: string) => string;
  /** show the "Ignored in the link" notice for a bad ?pnode (default true; the home only) */
  reportIgnored?: boolean;
  className?: string;
}

export function SectorStrategyCard({
  node, title, headingLevel = 3, compact = false, countryHref = strategyHref, reportIgnored = true, className,
}: SectorStrategyCardProps) {
  const core = usePostureCore();
  const [sp, setSp] = useSearchParams();
  const nav = useNavigate();
  const [dismissed, setDismissed] = useState<string | null>(null);
  const raw = sp.get(SECTOR_NODE_KEY);
  // every stage is offered: Deployment & Use is plotted on deployment need
  const parsed = useMemo(() => (core.data && !node ? parsePostureNode(raw, core.data.meta, core.data.meta.stages) : null), [core.data, node, raw]);
  const tech = node ? node.tech : (parsed?.tech ?? '');
  const stage = node ? node.stage : (parsed?.stage ?? '');
  const cells = useMemo<PostureCell[]>(() => (core.data ? core.data.cells.filter((c) => c.tech === tech && c.stage === stage) : []), [core.data, tech, stage]);
  const names = useMemo(() => (core.data ? countryNames(core.data) : {}), [core.data]);
  const tiers = useMemo(() => (core.data ? Object.fromEntries(core.data.countries.map((c) => [c.iso3, c.tier])) : {}), [core.data]);
  const rows = useMemo(() => {
    if (!core.data) return [];
    const byIso = new Map(cells.map((c) => [c.iso3, c]));
    const rank = (r: PostureRow) => verbRank(byIso.get(r.iso3) ?? null);
    return sectorPostureRows(core.data, tech, stage).sort((a, b) => rank(a) - rank(b) || (b.cp ?? -1) - (a.cp ?? -1) || a.country.localeCompare(b.country));
  }, [core.data, cells, tech, stage]);
  const points = useMemo(() => strategyPoints(cells, names), [cells, names]);
  if (core.status === 'error') {
    return (
      <Notice kind="warning" title="The posture data could not be loaded">
        {core.error}
      </Notice>
    );
  }
  if (!core.data) return <Loading label="Loading the strategic posture" />;
  const c = core.data;
  if (node && (!c.meta.techs.includes(node.tech) || !c.meta.stages.includes(node.stage))) {
    return (
      <Notice kind="info" title="No strategic choice for this node">
        The strategic position framework classifies {c.meta.techs.length} technologies at the six stages; {node.tech} — {stageDisplay(node.stage)} is not one of its nodes.
      </Notice>
    );
  }
  const t = c.meta.thresholds;
  const set = (tt: string, s: string) => {
    const q = new URLSearchParams(sp);
    q.set(SECTOR_NODE_KEY, `${tt}|${s}`);
    setSp(q, { replace: true, preventScrollReset: true });
  };
  const dep = stage === DEPLOYMENT_STAGE;
  const prize = nodePrize(cells, t.prize);
  const takeaway = sectorStrategyTakeaway(tech, stage, cells, names, c.meta);
  const counts = strategyCountsText(countPostures(cells));
  const notPlotted = cells.length - points.length;
  const problem = parsed?.problem ?? null;
  const open = (iso: string) => nav(countryHref(iso, tech, stage));
  const byIso = new Map(cells.map((x) => [x.iso3, x]));
  const plottedCol: Col<PostureRow> = { id: 'plotted', header: 'On the chart', value: (r) => (notPlottedReason(byIso.get(r.iso3) ?? null) ? `no: ${notPlottedReason(byIso.get(r.iso3) ?? null)}` : 'yes') };
  const cols = [...SECTOR_POSTURE_COLS.filter((x) => x.id !== 'plotted'), plottedCol];
  const tools = node ? undefined : (
    <>
      <Select label="Technology" value={tech} options={c.meta.techs.map((x) => ({ value: x, label: x }))} onChange={(x) => set(x, stage)} />
      <Select label="Stage" value={stage} options={c.meta.stages.map((s) => ({ value: s, label: stageDisplay(s) }))} onChange={(s) => set(tech, s)} className="min-w-[12rem]" />
    </>
  );
  const csv = () => ({ name: `${tech} ${stage} strategic choice by economy`, text: postureCsvText(postureCsvRows(rows), POSTURE_CSV_COLS, postureProvenance(c)) });
  return (
    <div className={className} data-testid="sector-posture-hero">
      {problem && reportIgnored && dismissed !== raw && (
        <div className="mb-3" data-testid="posture-node-ignored">
          <Notice kind="warning" title="Ignored in the link" onDismiss={() => setDismissed(raw)}>
            {problem}
          </Notice>
        </div>
      )}
      <ChartCard
        headingLevel={headingLevel}
        title={title ?? (node ? sectorStrategyTitle(tech, stage) : SECTOR_STRATEGY_TITLE)}
        sub={
          <>
            <span data-testid="posture-takeaway">{takeaway}</span>
            <span className="mt-1 block tabular text-paper" data-testid="sector-strategy-counts">
              {counts.verbs}
              <span className="text-paper-mute">
                {' '}
                ({counts.families}
                {counts.notAssessed ? ` · ${counts.notAssessed}` : ''})
              </span>
            </span>
            {/* the reading line, as the Sector pages' chart cards give it */}
            <span className="mt-1 block text-paper-dim" data-testid="posture-reading">
              Each mark is one economy. Across: <Term id={POSTURE_TERMS.competitivePosition}>Competitive Position</Term>, among the node&apos;s contenders; up:{' '}
              {dep ? <Term id={POSTURE_TERMS.deploymentNeed}>deployment need</Term> : <Term id={POSTURE_TERMS.supplyChainNeed}>supply-chain need</Term>}. The colour is
              the choice, grouped by fence. The node&apos;s prize (<Term id={POSTURE_TERMS.industrialOpportunity}>Industrial Opportunity</Term>) barely varies across economies, so the notes give it in
              words.
            </span>
          </>
        }
        summary={takeaway}
        tools={tools}
        table={() => (
          <TableView
            rows={rows}
            cols={cols}
            caption={`${tech} — ${stageDisplay(stage)}: strategic choice by economy`}
            rowKey={(r) => r.key}
            onRowClick={(r) => open(r.iso3)}
            search
            pageSize={25}
          />
        )}
        csv={csv}
        legend={<VerbMarkLegend verbs={verbsOfStage(stage)} notPlotted={notPlotted} />}
        notes={
          <div className="space-y-1" data-testid="sector-strategy-notes">
            <p data-testid="sector-strategy-prize">{prizeWords(stage, prize, t.prize)}</p>
            <p data-testid="sector-strategy-rule">{decisionWords(stage, prize, t)}</p>
            <p>Region labels are tendencies, not rules: the marks give each economy&apos;s actual choice. A mark opens that economy&apos;s Country Strategy Map on this node.</p>
            <TradeFreshness meta={c.meta} />
          </div>
        }
      >
        <div className={compact ? 'mx-auto max-w-xl' : undefined}>
          <SectorStrategyScatter
            points={points}
            stage={stage}
            thresholds={t}
            regions={regionLabels(stage, prize?.kind ?? null, t)}
            onOpen={open}
            tierOf={(iso) => TIER_WORD[tiers[iso] ?? ''] ?? null}
            nameOf={(iso) => names[iso] ?? iso}
          />
        </div>
      </ChartCard>
    </div>
  );
}

/** The Sector posture Section intro: the one prescriptive block of the Sector pages (phrasing content only). */
export function SectorStrategyIntro() {
  return (
    <>
      Unlike the measured values on the rest of these pages, this is a recommendation: for one technology at one stage, the strategic position framework reads each
      economy&apos;s need (<Term id={POSTURE_TERMS.supplyChainNeed}>supply-chain need</Term>, or <Term id={POSTURE_TERMS.deploymentNeed}>deployment need</Term> at Deployment
      &amp; Use), the node&apos;s prize (<Term id={POSTURE_TERMS.industrialOpportunity}>Industrial Opportunity</Term>) and its{' '}
      <Term id={POSTURE_TERMS.competitivePosition}>Competitive Position</Term>, and recommends one choice: Secure, Preserve, Catch Up, Partner, Accelerate or Watch. Its
      thresholds are fixed; observed policy is compared with it afterwards, never fed into it.
    </>
  );
}
