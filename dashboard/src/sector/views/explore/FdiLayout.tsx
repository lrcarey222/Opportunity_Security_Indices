// Explore, the investment layout of the CM-FDI measures (investment.fdi_projects, fdi_value,
// fdi_parent_share; chunk sector-fdi): the cell's cross-border projects as in the profile (key figures,
// destinations on a map and ranked, home -> destination flows, investors, the parent-headquarters group
// split per destination, announcements by year, the project table). The measure picks the lead: the
// project count, the disclosed capital, or the group split first. A click on an economy sets `c`; a
// destination row opens that economy's page in the sector.

import { useSectorUrl } from '../state.tsx';
import { FdiBlock, type FdiFocus } from '../fdi/FdiBlock.tsx';
import type { ExploreCtx } from './ctx.ts';

const FOCUS: Record<string, FdiFocus> = {
  'investment.fdi_projects': 'n',
  'investment.fdi_value': 'value',
  'investment.fdi_parent_share': 'share',
};

export function FdiLayout({ c }: { c: ExploreCtx }) {
  const { link } = useSectorUrl();
  const { model, item, v, highlight } = c;
  if (!item) return <p className="text-sm text-paper-mute">No sector has cross-border project data for this measure.</p>;
  const tech = item.techSlug;
  const stage = item.stageSlug;
  return (
    <FdiBlock
      key={`${v.id}|${item.slug}`}
      model={model}
      item={item}
      highlight={highlight}
      focus={FOCUS[v.id] ?? 'n'}
      intro={false}
      onPick={(g) => c.setHighlight(g && g.idx !== highlight?.idx ? g : null)}
      geoHref={(g) => (tech && stage && (g.kind === 'country' || g.kind === 'eu') ? link(`/sector/${tech}/${stage}/${g.code}`) : null)}
    />
  );
}
