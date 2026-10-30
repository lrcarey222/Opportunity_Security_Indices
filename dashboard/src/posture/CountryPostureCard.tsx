// The Country Strategy Map card (spec XI.3-6; design section 4): one economy's choice in every technology x
// six-stage node, as a ChartCard for the page's Strategy Section (heading level 3 by default; the Section
// gives the h2 and the basis tag). The generated strategy sentence as the sub (off when the page header
// carries it); the counts line, six verbs first, above the map; the legend; the trade freshness line and how
// the choice is made in the notes; a table twin of all 66 nodes (unclassified ones kept) and a CSV with the
// posture provenance. The selection can be lifted to the page (`selected` / `onSelect`, e.g. the shared
// ?node= of selection.ts); the page then mounts one PostureNodeDrawer for every entry point (`drawer={false}`),
// or the card renders its own. Imports nothing from country/.

import { useCallback, useMemo, useState } from 'react';
import { ChartCard } from '../charts/ChartCard.tsx';
import { TableView } from '../components/TableView.tsx';
import { Term } from '../components/Term.tsx';
import { Loading, Notice } from '../components/ui.tsx';
import { POSTURE_TERMS } from './basis.tsx';
import { PostureNodeDrawer } from './CellDrawer.tsx';
import { countPostures, countryCells, countryNames, countryPostureRows, POSTURE_CSV_COLS, postureCsvRows, postureCsvText, postureProvenance, strategyCountsText } from './derive.ts';
import { StrategyCounts } from './PostureCounts.tsx';
import { PostureLegend, TradeFreshness } from './PostureLegend.tsx';
import { PostureMatrix } from './PostureMatrix.tsx';
import { usePostureCore } from './store.ts';
import { strategySentence } from './strategy.ts';
import { COUNTRY_POSTURE_COLS } from './tables.tsx';

export const COUNTRY_STRATEGY_MAP_TITLE = 'Country Strategy Map';
/** The old name of the title constant (the map was "Secure, compete or deploy, by technology and stage" until 2026-10-29). */
export const COUNTRY_POSTURE_TITLE = COUNTRY_STRATEGY_MAP_TITLE;

export interface CountryPostureCardProps {
  iso3: string;
  /** the economy's name as the sentence's first words ("The United States"); also names the CSV and the table */
  name: string;
  title?: string;
  /** 3 (default) inside the page's Section; 2 only for a standalone mount */
  headingLevel?: 2 | 3;
  /** the open node ("Tech|Stage"), when the page holds the selection (e.g. ?node=); omit to keep it in the card */
  selected?: string | null;
  /** called with the new selection (null: closed), controlled or not */
  onSelect?: (key: string | null) => void;
  /** the counts line above the map (default on) */
  showCounts?: boolean;
  /** the strategy sentence as the card's sub (default on; off when the page header carries it) */
  showSentence?: boolean;
  /** render the drawer beside the card (default on; off when the page mounts one PostureNodeDrawer for every entry point) */
  drawer?: boolean;
  /** the node's Sector page, for a link in the drawer (posture/ knows no Sector routes) */
  sectorHref?: (tech: string, stage: string) => string | null;
  className?: string;
}

export function CountryPostureCard({
  iso3, name, title = COUNTRY_STRATEGY_MAP_TITLE, headingLevel = 3, selected, onSelect, showCounts = true, showSentence = true, drawer = true, sectorHref, className,
}: CountryPostureCardProps) {
  const core = usePostureCore();
  const [own, setOwn] = useState<string | null>(null);
  const controlled = selected !== undefined;
  const sel = controlled ? selected : own;
  const setSel = useCallback(
    (k: string | null) => {
      if (!controlled) setOwn(k);
      onSelect?.(k);
    },
    [controlled, onSelect],
  );
  const cells = useMemo(() => (core.data ? countryCells(core.data, iso3) : []), [core.data, iso3]);
  const rows = useMemo(() => (core.data ? countryPostureRows(core.data, iso3) : []), [core.data, iso3]);
  const names = useMemo(() => (core.data ? countryNames(core.data) : {}), [core.data]);
  const close = useCallback(() => setSel(null), [setSel]);
  if (core.status === 'error') {
    return (
      <Notice kind="warning" title="The posture data could not be loaded">
        {core.error}
      </Notice>
    );
  }
  if (!core.data) return <Loading label="Loading the Country Strategy Map" />;
  if (!cells.length) {
    return (
      <Notice kind="info" title="No posture">
        {name} has no classified supply-chain nodes in the strategic position framework.
      </Notice>
    );
  }
  const c = core.data;
  const sentence = strategySentence(name, cells, c.meta);
  const counts = countPostures(cells);
  const cw = strategyCountsText(counts);
  const summary = `${sentence} ${cw.verbs}.`;
  const toggle = (k: string) => setSel(sel === k ? null : k);
  const csv = () => ({ name: `${name} country strategy map`, text: postureCsvText(postureCsvRows(rows), POSTURE_CSV_COLS, postureProvenance(c)) });
  return (
    <div className={className} data-testid="country-posture-hero">
      <ChartCard
        headingLevel={headingLevel}
        title={title}
        sub={showSentence ? <span data-testid="posture-takeaway">{sentence}</span> : undefined}
        summary={summary}
        table={() => (
          <TableView
            rows={rows}
            cols={COUNTRY_POSTURE_COLS}
            caption={`${name}: choice by technology and stage`}
            rowKey={(r) => r.key}
            onRowClick={(r) => r.classified && toggle(r.key)}
            selectedKey={sel}
            pageSize={66}
          />
        )}
        csv={csv}
        legend={<PostureLegend />}
        notes={
          <div className="space-y-1">
            <TradeFreshness meta={c.meta} />
            <p>
              Need and <Term id={POSTURE_TERMS.industrialOpportunity}>Industrial Opportunity</Term> triage each node; <Term id={POSTURE_TERMS.competitivePosition}>Competitive Position</Term>{' '}
              decides the industrial response; the Secure tests override both. Deployment &amp; Use reads <Term id={POSTURE_TERMS.deploymentNeed}>deployment need</Term> and is
              Accelerate or Watch, never Partner. Select a node, or a row of the table, for its five questions: need, prize, position, choice and policy alignment.
            </p>
          </div>
        }
      >
        {showCounts && <StrategyCounts counts={counts} className="mb-3" />}
        <div className="overflow-x-auto">
          <PostureMatrix core={c} cells={cells} selected={sel} onSelect={toggle} names={names} />
        </div>
      </ChartCard>
      {drawer && sel && <PostureNodeDrawer iso3={iso3} name={name} selected={sel} onClose={close} sectorHref={sectorHref} />}
    </div>
  );
}

/** The new name of the card. */
export const CountryStrategyMap = CountryPostureCard;
export type CountryStrategyMapProps = CountryPostureCardProps;
