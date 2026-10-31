// The Country Strategy Map card (spec XI.3-6; design section 4): one economy's choice in every technology x
// six-stage node, as a ChartCard for the page's Strategy Section (heading level 3 by default; the Section
// gives the h2 and the basis tag). The generated strategy sentence as the sub, or, where the page header carries
// it, how to read the map (COUNTRY_STRATEGY_MAP_INTRO, 2026-10-31). Above the map (below it on a phone, so the
// map stays near the top), the one legend (StrategyMapLegend): the colours grouped by fence, each fence headed by
// its yard in the user's words, each choice, Not assessed and Mixed defined in one plain line with the economy's
// step count (showCounts=false keeps the definitions without the counts); the trade freshness line and how the
// choice is made in the notes; a table twin of all 66 nodes (unclassified ones kept) and a CSV with the
// posture provenance. The economy's detail document (the page's, or the card's own load of the same store
// entry) names the parts of each step in the map's tooltips and accessible names. The selection can be lifted to the page (`selected` / `onSelect`, e.g. the shared
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
import { prizeLabel } from './grammar.ts';
import { StrategyMapLegend, TradeFreshness } from './PostureLegend.tsx';
import { PostureMatrix } from './PostureMatrix.tsx';
import { mixedYards } from './yards.ts';
import { usePostureCore, usePostureDetail } from './store.ts';
import type { PostureDetail } from './contract.ts';
import { strategySentence } from './strategy.ts';
import { countryPostureCols } from './tables.tsx';

export const COUNTRY_STRATEGY_MAP_TITLE = 'Country Strategy Map';
/**
 * The map's sub where the page header carries the strategy sentence: how to read the map (the user's request,
 * 2026-10-31); the legend defines each term. A first-time reader meets the rows, the six columns, what a color,
 * a split and a hatch mean, and where the evidence is.
 */
export const COUNTRY_STRATEGY_MAP_INTRO =
  "One map of the country's strategic choices. Each row is a technology; the six columns are the steps of its supply chain, from Extraction to Recovery & Recycling, with Deployment & Use set apart as the energy-system column. A cell's color is the country's choice for that step, and the legend defines each choice and groups the colors by fence. A cell split on the diagonal has parts behind different fences: hover over it, or tap it, to see which. Hatched cells are not assessed. Select a cell for the evidence behind its choice.";
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
  /** the node counts in the legend above the map (default on) */
  showCounts?: boolean;
  /** the strategy sentence as the card's sub (default on; off when the page header carries it) */
  showSentence?: boolean;
  /** render the drawer beside the card (default on; off when the page mounts one PostureNodeDrawer for every entry point) */
  drawer?: boolean;
  /** the node's Sector page, for a link in the drawer (posture/ knows no Sector routes) */
  sectorHref?: (tech: string, stage: string) => string | null;
  /**
   * the economy's detail document where the page has loaded it (null while it loads or after an error): the
   * parts of each step in the tooltips and names; omit and the card loads the same store entry itself
   */
  detail?: PostureDetail | null;
  className?: string;
}

export function CountryPostureCard({
  iso3, name, title = COUNTRY_STRATEGY_MAP_TITLE, headingLevel = 3, selected, onSelect, showCounts = true, showSentence = true, drawer = true, sectorHref, detail, className,
}: CountryPostureCardProps) {
  const core = usePostureCore();
  // the page's detail document, else the card's own load of the same store entry (fetched once, shared)
  const ownDetail = usePostureDetail(detail === undefined ? iso3 : null);
  const doc = detail === undefined ? ownDetail.data : detail;
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
      <Notice kind="warning" title="The strategy data could not be loaded">
        {core.error}
      </Notice>
    );
  }
  if (!core.data) return <Loading label="Loading the Country Strategy Map" />;
  if (!cells.length) {
    return (
      <Notice kind="info" title="No strategy map">
        There is not enough comparable evidence to produce a strategy map for {name}.
      </Notice>
    );
  }
  const c = core.data;
  const sentence = strategySentence(name, cells, c.meta);
  const counts = countPostures(cells);
  // the nodes the map splits on the diagonal (layers in different yards)
  const mixed = cells.filter((x) => mixedYards(x) !== null).length;
  const prizeTermId = c.meta.prize_measure === 'market' ? POSTURE_TERMS.industrialOpportunity : POSTURE_TERMS.strategicValue;
  const cw = strategyCountsText(counts);
  const summary = `${sentence} ${cw.verbs}.`;
  const toggle = (k: string) => setSel(sel === k ? null : k);
  const csv = () => ({ name: `${name} country strategy map`, text: postureCsvText(postureCsvRows(rows), POSTURE_CSV_COLS, postureProvenance(c)) });
  return (
    <div className={className} data-testid="country-posture-hero">
      <ChartCard
        headingLevel={headingLevel}
        title={title}
        sub={showSentence ? <span data-testid="posture-takeaway">{sentence}</span> : <span data-testid="strategy-map-intro">{COUNTRY_STRATEGY_MAP_INTRO}</span>}
        summary={summary}
        table={() => (
          <TableView
            rows={rows}
            cols={countryPostureCols(c.meta)}
            caption={`${name}: choice by technology and stage`}
            rowKey={(r) => r.key}
            onRowClick={(r) => r.classified && toggle(r.key)}
            selectedKey={sel}
            pageSize={66}
          />
        )}
        csv={csv}
        notes={
          <div className="space-y-1">
            <TradeFreshness meta={c.meta} />
            <p>
              Need and the prize (<Term id={prizeTermId}>{prizeLabel(c.meta)}</Term>) decide whether a supply-chain step matters;{' '}
              <Term id={POSTURE_TERMS.competitivePosition}>Competitive Position</Term> decides the response; the Secure tests come first. At Deployment &amp; Use the question is{' '}
              <Term id={POSTURE_TERMS.deploymentNeed}>deployment need</Term>: Accelerate (medium fence) or Watch (low fence), never Partner. Color alone shows the choice; hover or
              focus a cell for its name, choice and fence and, where the step has layers, the choice and fence of each part; select it, or a row of the table, for the five
              questions behind the choice: need, prize, position, choice and policy alignment.
            </p>
            {mixed > 0 && (
              <p data-testid="strategy-map-mixed-note">
                A cell split on the diagonal is a mixed step: its capability layers (control software, firmware, a critical input) sit in different{' '}
                <Term id={POSTURE_TERMS.yard}>yards</Term>, behind different fences. The step&apos;s own choice color fills the upper left; the fence color of each other part
                the lower right (orange the High fence, blue the Medium fence, gray the Low fence). Hover over the cell, or open it, to see which part sits behind which fence.
              </p>
            )}
          </div>
        }
      >
        {/* the legend leads on wide screens; on a phone it follows the map, so the map stays near the top */}
        <div className="flex flex-col">
          <StrategyMapLegend counts={showCounts ? counts : null} mixed={mixed > 0 ? (showCounts ? mixed : true) : null} className="order-last mt-4 sm:order-none sm:mt-0 sm:mb-4" />
          <div className="overflow-x-auto">
            <PostureMatrix core={c} cells={cells} selected={sel} onSelect={toggle} names={names} detail={doc} />
          </div>
        </div>
      </ChartCard>
      {drawer && sel && <PostureNodeDrawer iso3={iso3} name={name} selected={sel} onClose={close} sectorHref={sectorHref} />}
    </div>
  );
}

/** The new name of the card. */
export const CountryStrategyMap = CountryPostureCard;
export type CountryStrategyMapProps = CountryPostureCardProps;
