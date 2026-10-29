// The Country posture card (design L1, 2026-10-28): one economy's recommended posture in every technology x
// six-stage node, as a ChartCard for the page's "Strategic posture" Section (heading level 3 by default;
// the Section gives the h2 and the basis tag). Takeaway as the sub and the figure's summary; the posture
// legend and the Secure marks in the legend slot; the trade freshness line and the Deployment note in the
// notes; a table twin of all 66 nodes (unclassified ones kept) and a CSV with the posture provenance. The
// drawer renders beside the card, so a matrix cell or a table row opens it. The selection can be lifted to
// the page (`selected` / `onSelect`, e.g. to the URL); the counts go in the page's tile (CountryPostureTile)
// unless `showCounts`. Imports nothing from country/.

import { useCallback, useMemo, useState } from 'react';
import { ChartCard } from '../charts/ChartCard.tsx';
import { TableView } from '../components/TableView.tsx';
import { Term } from '../components/Term.tsx';
import { Loading, Notice } from '../components/ui.tsx';
import { POSTURE_TERMS } from './basis.tsx';
import { CellDrawer } from './CellDrawer.tsx';
import { cellKey, countPostures, countryCells, countryNames, countryPostureRows, countryTakeaway, POSTURE_CSV_COLS, postureCsvRows, postureCsvText, postureProvenance } from './derive.ts';
import { RATIONALE_LABEL, RATIONALE_MARK } from './grammar.ts';
import { PostureCountsList } from './PostureCounts.tsx';
import { PostureLegend, TradeFreshness } from './PostureLegend.tsx';
import { PostureMatrix } from './PostureMatrix.tsx';
import { usePostureCore, usePostureDetail } from './store.ts';
import { COUNTRY_POSTURE_COLS } from './tables.tsx';

export const COUNTRY_POSTURE_TITLE = 'Secure, compete or deploy, by technology and stage';

export interface CountryPostureCardProps {
  iso3: string;
  /** the economy's name as the takeaway's first words ("The United States"); also names the CSV and the table */
  name: string;
  title?: string;
  /** 3 (default) inside the page's Section; 2 only for a standalone mount */
  headingLevel?: 2 | 3;
  /** the open cell ("Tech|Stage"), when the page holds the selection (e.g. in the URL); omit to keep it in the card */
  selected?: string | null;
  /** called with the new selection (null: closed), controlled or not */
  onSelect?: (key: string | null) => void;
  /** the counts list at the top of the card (default off: the page's tile shows them) */
  showCounts?: boolean;
  /** the node's Sector page, for a link in the drawer (posture/ knows no Sector routes) */
  sectorHref?: (tech: string, stage: string) => string | null;
  className?: string;
}

export function CountryPostureCard({
  iso3, name, title = COUNTRY_POSTURE_TITLE, headingLevel = 3, selected, onSelect, showCounts = false, sectorHref, className,
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
  const detail = usePostureDetail(sel ? iso3 : null);
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
  if (!core.data) return <Loading label="Loading the strategic posture" />;
  if (!cells.length) {
    return (
      <Notice kind="info" title="No posture">
        {name} has no classified supply-chain nodes in the strategic position framework.
      </Notice>
    );
  }
  const c = core.data;
  const takeaway = countryTakeaway(name, cells);
  const toggle = (k: string) => setSel(sel === k ? null : k);
  const drawerCell = sel && detail.data ? (detail.data.cells.find((d) => cellKey(d.tech, d.stage) === sel) ?? null) : null;
  const csv = () => ({ name: `${name} strategic posture by technology and stage`, text: postureCsvText(postureCsvRows(rows), POSTURE_CSV_COLS, postureProvenance(c)) });
  return (
    <div className={className} data-testid="country-posture-hero">
      <ChartCard
        headingLevel={headingLevel}
        title={title}
        sub={<span data-testid="posture-takeaway">{takeaway}</span>}
        summary={takeaway}
        table={() => (
          <TableView
            rows={rows}
            cols={COUNTRY_POSTURE_COLS}
            caption={`${name}: strategic posture by technology and stage`}
            rowKey={(r) => r.key}
            onRowClick={(r) => r.classified && toggle(r.key)}
            selectedKey={sel}
            pageSize={66}
          />
        )}
        csv={csv}
        legend={
          <div className="space-y-2">
            <PostureLegend />
            <p className="text-xs text-paper-mute">
              Secure marks: {Object.entries(RATIONALE_MARK).map(([r, m]) => `${m} ${RATIONALE_LABEL[r as keyof typeof RATIONALE_LABEL].toLowerCase()}`).join(' · ')}.
            </p>
          </div>
        }
        notes={
          <div className="space-y-1">
            <TradeFreshness meta={c.meta} />
            <p>
              Each node&apos;s posture follows from its <Term id={POSTURE_TERMS.strategicNeed}>Strategic Need</Term> and{' '}
              <Term id={POSTURE_TERMS.industrialOpportunity}>Industrial Opportunity</Term>, and from the Secure tests. Deployment is where the technology is used: the posture
              there is Deploy, and its supply risks are read at the stages that make or supply it. Select a cell, or a row of the table, for its partners, rationale or restraint,
              policy design and trade evidence.
            </p>
          </div>
        }
      >
        {showCounts && <PostureCountsList counts={countPostures(cells)} className="mb-3" />}
        <div className="overflow-x-auto">
          <PostureMatrix core={c} cells={cells} selected={sel} onSelect={toggle} />
        </div>
      </ChartCard>
      {sel && detail.status === 'loading' && (
        <p className="mt-2 text-xs text-paper-mute" role="status">
          Loading the detail…
        </p>
      )}
      {sel && detail.status === 'error' && (
        <p className="mt-2 text-xs text-paper-mute" role="status">
          The detail could not be loaded: {detail.error}
        </p>
      )}
      {sel && (
        <CellDrawer
          detail={detail.data}
          cell={drawerCell}
          names={names}
          core={c}
          onClose={close}
          sectorHref={drawerCell && sectorHref ? sectorHref(drawerCell.tech, drawerCell.stage) : null}
        />
      )}
    </div>
  );
}
