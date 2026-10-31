// The Sector posture card (design L1, L3, 2026-10-28): every economy in one technology x six-stage node on
// the need x opportunity plane, as a ChartCard for the page's "Strategic posture" Section (heading level 3
// by default). Two uses:
// - the Sector home: Technology and Stage selects in the card's tools; the node lives in the URL
//   (?pnode=Tech|Stage), checked against the data (parsePostureNode): a value that is not used as written
//   (a three-stage stage such as Batteries|Midstream, an unknown technology) is reported in an "Ignored in
//   the link" notice above the card, unless the page lists it in its own notice (reportIgnored={false}).
//   Deployment is not offered (its choice reads deployment need, Accelerate or Watch, and it has no prize to plot); it shows as a disabled choice.
// - a Sector profile (/sector/:tech/:stage): `node` fixes the card to that node (no selects, no URL key);
//   Deployment is allowed there and says that every economy deploys.
// Takeaway as the sub and the figure's summary; legend and notes in the card's slots; a table twin of the
// node's economies (a row opens the profile, like a point) and a CSV with the posture provenance. A point
// opens /countries/:iso3#strategy (the Country Strategy Map; #posture is its old alias) unless `countryHref` says otherwise. Imports nothing from sector/.

import { useMemo, useState } from 'react';
import { useNavigate, useSearchParams } from 'react-router';
import { ChartCard } from '../charts/ChartCard.tsx';
import { TableView } from '../components/TableView.tsx';
import { Term } from '../components/Term.tsx';
import { Loading, Notice, Select } from '../components/ui.tsx';
import { DEPLOYMENT_STAGE } from '../lib/stages.ts';
import { POSTURE_TERMS } from './basis.tsx';
import type { PostureCell } from './contract.ts';
import { countPostures, countryNames, parsePostureNode, POSTURE_CSV_COLS, postureCsvRows, postureCsvText, postureProvenance, producingStagesOf, sectorPostureRows, sectorTakeaway } from './derive.ts';
import { prizeLabel } from './grammar.ts';
import { PostureCountsList } from './PostureCounts.tsx';
import { ChoiceLegend, TradeFreshness } from './PostureLegend.tsx';
import { PostureScatter } from './PostureScatter.tsx';
import { usePostureCore } from './store.ts';
import { sectorPostureCols } from './tables.tsx';

/** The URL key of the Sector home's node: ?pnode=Tech|Stage (stage and technology labels, not slugs). */
export const SECTOR_NODE_KEY = 'pnode';

export const SECTOR_POSTURE_TITLE = 'Who should secure, compete or deploy in this node';

/** The default profile link of a point or a table row: the economy's Strategic posture section. */
export const postureProfileHref = (iso3: string): string => `/countries/${iso3}#strategy`;

export interface SectorPostureCardProps {
  /** fix the card to one node (a Sector profile); omit on the Sector home (selects + ?pnode) */
  node?: { tech: string; stage: string };
  /** default: SECTOR_POSTURE_TITLE on the home, "Who should secure, compete or deploy in <Tech> — <Stage>" with a node */
  title?: string;
  /** 3 (default) inside the page's Section; 2 only for a standalone mount */
  headingLevel?: 2 | 3;
  /** the counts list at the top of the card (default off) */
  showCounts?: boolean;
  /** a narrower scatter (a profile's side column or a short section) */
  compact?: boolean;
  /** where a point or a table row goes (default postureProfileHref) */
  countryHref?: (iso3: string) => string;
  /** show the "Ignored in the link" notice for a bad ?pnode (default true; the home only) */
  reportIgnored?: boolean;
  className?: string;
}

export function SectorPostureCard({
  node, title, headingLevel = 3, showCounts = false, compact = false, countryHref = postureProfileHref, reportIgnored = true, className,
}: SectorPostureCardProps) {
  const core = usePostureCore();
  const [sp, setSp] = useSearchParams();
  const nav = useNavigate();
  const [dismissed, setDismissed] = useState<string | null>(null);
  const raw = sp.get(SECTOR_NODE_KEY);
  const parsed = useMemo(() => (core.data && !node ? parsePostureNode(raw, core.data.meta) : null), [core.data, node, raw]);
  const tech = node ? node.tech : (parsed?.tech ?? '');
  const stage = node ? node.stage : (parsed?.stage ?? '');
  // filter on the checked node (the selects and the takeaway show the same one)
  const cells = useMemo<PostureCell[]>(() => (core.data ? core.data.cells.filter((c) => c.tech === tech && c.stage === stage) : []), [core.data, tech, stage]);
  const rows = useMemo(() => (core.data ? sectorPostureRows(core.data, tech, stage) : []), [core.data, tech, stage]);
  const names = useMemo(() => (core.data ? countryNames(core.data) : {}), [core.data]);
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
      <Notice kind="info" title="No posture for this node">
        The strategic position framework classifies {c.meta.techs.length} technologies at the six stages; {node.tech} — {node.stage} is not one of its nodes.
      </Notice>
    );
  }
  const set = (t: string, s: string) => {
    const q = new URLSearchParams(sp);
    q.set(SECTOR_NODE_KEY, `${t}|${s}`);
    setSp(q, { replace: true, preventScrollReset: true });
  };
  const offered = producingStagesOf(c.meta.stages);
  const takeaway = sectorTakeaway(tech, stage, cells, names);
  const notPlotted = cells.length - cells.filter((x) => x.posture !== 'N' && x.need !== null && x.prize !== null).length;
  const problem = parsed?.problem ?? null;
  const open = (iso: string) => nav(countryHref(iso));
  const tools = node ? undefined : (
    <>
      <Select label="Technology" value={tech} options={c.meta.techs.map((t) => ({ value: t, label: t }))} onChange={(t) => set(t, stage)} />
      <Select
        label="Stage"
        value={stage}
        options={c.meta.stages.map((s) => (offered.includes(s) ? { value: s, label: s } : { value: s, label: `${s} (deployment need: not on this chart)`, disabled: true }))}
        onChange={(s) => set(tech, s)}
        className="min-w-[9rem]"
      />
    </>
  );
  const csv = () => ({ name: `${tech} ${stage} strategic posture by economy`, text: postureCsvText(postureCsvRows(rows), POSTURE_CSV_COLS, postureProvenance(c)) });
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
        title={title ?? (node ? `Who should secure, compete or deploy in ${tech} — ${stage}` : SECTOR_POSTURE_TITLE)}
        sub={
          <>
            <span data-testid="posture-takeaway">{takeaway}</span>
            {/* the reading line, as the Sector pages' chart cards give it (spec_country.md C3.6) */}
            <span className="mt-1 block text-paper-dim" data-testid="posture-reading">
              Each point is one economy. Across: <Term id={c.meta.prize_measure === 'market' ? POSTURE_TERMS.industrialOpportunity : POSTURE_TERMS.strategicValue}>{prizeLabel(c.meta)}</Term>, among the node&apos;s contenders; up:{' '}
              <Term id={POSTURE_TERMS.supplyChainNeed}>supply-chain need</Term>. Outlined diamonds are Secure: they keep their true coordinates because Secure overrides the triage.
              Point size is <Term id={POSTURE_TERMS.competitivePosition}>Competitive Position</Term>.
            </span>
          </>
        }
        summary={takeaway}
        tools={tools}
        table={() => (
          <TableView
            rows={rows}
            cols={sectorPostureCols(c.meta)}
            caption={`${tech} — ${stage}: strategic posture by economy`}
            rowKey={(r) => r.key}
            onRowClick={(r) => open(r.iso3)}
            search
            pageSize={25}
          />
        )}
        csv={csv}
        legend={<ChoiceLegend counts={countPostures(cells)} verbs={stage === DEPLOYMENT_STAGE ? ['accelerate', 'watch'] : ['secure', 'preserve', 'catch_up', 'partner', 'watch']} />}
        notes={
          <div className="space-y-1">
            <TradeFreshness meta={c.meta} />
            <p>
              Economies without a measured need or prize, or not assessed, are not plotted ({notPlotted} here; the table lists them).
              {stage === DEPLOYMENT_STAGE
                ? ` At Deployment & Use the choice reads deployment need (Accelerate or Watch); the prize (${prizeLabel(c.meta)}) is read at the stages that make or supply the technology.`
                : ''}
            </p>
          </div>
        }
      >
        {showCounts && <PostureCountsList counts={countPostures(cells)} className="mb-3" />}
        <div className={compact ? 'mx-auto max-w-xl' : undefined}>
          <PostureScatter core={c} cells={cells} onOpen={open} />
        </div>
      </ChartCard>
    </div>
  );
}
