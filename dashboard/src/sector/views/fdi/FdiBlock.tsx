// The CM-FDI block of one Midstream cell (Solar, Wind, Batteries, Electric Vehicles): the profile's
// Investment subsection and the Explore investment layout. It loads sector-fdi lazily (only when it
// mounts), then shows the switches (all projects or the ones the indices use; every announcement year
// or a recent window; projects or disclosed capital), the key figures, where the projects go (map and
// ranking), the home -> destination flows (Sankey), who invests, the parent-headquarters group split
// per destination, announcements by year and the project table. Raw values only.

import { useMemo, useState } from 'react';
import type { GeoInfo, ItemInfo, SectorModel } from '../../data/model.ts';
import type { FdiBasis } from '../../contract.ts';
import { filterProjects, fdiTotals, type FdiData, type FdiFrom, type FdiShow } from '../../derive/fdi.ts';
import { ChunkGate } from '../common.tsx';
import { useFdi } from '../data.ts';
import { DestinationsCard, FdiControls, FdiIntro, FdiTiles, FlowsCard, InvestorsCard, ParentGroupsCard, ProjectsCard, YearsCard, type FdiScope } from './cards.tsx';

/** Which card leads (the Explore measure): projects, disclosed capital or the parent-group split. */
export type FdiFocus = 'n' | 'value' | 'share';

export interface FdiBlockProps {
  model: SectorModel;
  /** the Midstream cell */
  item: ItemInfo;
  highlight: GeoInfo | null;
  onPick?: (g: GeoInfo | null) => void;
  geoHref?: (g: GeoInfo) => string | null;
  focus?: FdiFocus;
  /** the intro paragraph (the profile shows it; Explore has its own notes card) */
  intro?: boolean;
}

function Body({ data, p }: { data: FdiData; p: FdiBlockProps }) {
  const { model, item, focus = 'n' } = p;
  const [basis, setBasis] = useState<FdiBasis>('all');
  const [from, setFrom] = useState<FdiFrom>(null);
  const [show, setShow] = useState<FdiShow>(focus === 'value' ? 'value' : 'n');
  const cellAll = useMemo(() => filterProjects(data.projects, { items: [item.idx] }), [data, item]);
  const windowed = useMemo(() => filterProjects(cellAll, { from }), [cellAll, from]);
  const projects = useMemo(() => filterProjects(windowed, { basis }), [windowed, basis]);
  const counts = useMemo(() => ({ all: windowed.length, index_use: windowed.filter((x) => x.indexUse).length }), [windowed]);
  const first = useMemo(() => fdiTotals(cellAll).yearMin, [cellAll]);
  const s: FdiScope = { model, data, item, projects, basis, show, highlight: p.highlight, onPick: p.onPick, geoHref: p.geoHref };
  if (!cellAll.length) return <p className="text-sm text-paper-mute">CM-FDI records no cross-border project in {model.cellLabel(item)}.</p>;
  const groups = <ParentGroupsCard s={s} />;
  return (
    <div className="space-y-6" data-testid="fdi-block">
      {p.intro !== false && <FdiIntro model={model} />}
      <FdiControls basis={basis} setBasis={setBasis} from={from} setFrom={setFrom} show={show} setShow={setShow} counts={counts} firstYear={first} />
      {!projects.length ? (
        <p className="text-sm text-paper-mute">No projects for this selection.</p>
      ) : (
        <>
          <FdiTiles s={s} />
          {focus === 'share' && groups}
          <DestinationsCard s={s} />
          <FlowsCard s={s} />
          <InvestorsCard s={s} />
          {focus !== 'share' && groups}
          <YearsCard s={s} />
          <ProjectsCard s={s} />
        </>
      )}
    </div>
  );
}

/** The CM-FDI cards of one cell (loads sector-fdi when mounted). */
export function FdiBlock(p: FdiBlockProps) {
  const { fdi, h, has } = useFdi(true);
  if (!has) return <p className="text-sm text-paper-mute">The cross-border project data (CM-FDI) is not part of this data build.</p>;
  return <ChunkGate h={h} name="sector-fdi">{() => (fdi ? <Body data={fdi} p={p} /> : null)}</ChunkGate>;
}
