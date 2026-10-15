// Country in sector, the CM-FDI part (sector-fdi, loaded when the section mounts): one economy's
// inbound cross-border projects in the cell (destination inside, parent headquartered elsewhere) and
// its outbound ones (its parents' projects abroad). Key figures, who invests in it (ranked, coloured by
// the parent's headquarters group), its inbound split by parent group against every destination,
// where its parents invest (map and ranking), and the projects in both directions with their source
// links. For the EU-27 node the members count together and projects between two members are left out.
// Raw counts and disclosed capital only.

import { useMemo, useState } from 'react';
import { TableView } from '../../../components/TableView.tsx';
import { StatTile } from '../../../components/ui.tsx';
import { rowsToCsv } from '../../../lib/csv.ts';
import type { FdiBasis } from '../../contract.ts';
import type { GeoInfo, ItemInfo, SectorModel } from '../../data/model.ts';
import {
  cmfdiSource, coalitionShare, countryProjects, FDI_GROUP_ORDER, FDI_GROUP_WORDS, fdiByGeo, fdiGroupSplit, fdiTotals, filterProjects, fmtUsdM, geoMembers, geoRank, NOT_DISCLOSED, projectsWord, rankWords, splitShares,
  type FdiData, type FdiGroupSplit, type FdiProject,
} from '../../derive/fdi.ts';
import { fmtShare } from '../../format.ts';
import { ChunkGate, HowToRead } from '../common.tsx';
import { useFdi } from '../data.ts';
import { SectorChartCard, ShareSplitChart, type ShareSplitRow } from '../kit.tsx';
import { useCardCsv } from '../profile/ctx.tsx';
import { DestinationsCard, FDI_VARS, FdiAttribution, FdiControls, FdiGroupLegend, InvestorsCard, ProjectsCard, type FdiScope } from './cards.tsx';

interface Props {
  model: SectorModel;
  cell: ItemInfo;
  g: GeoInfo;
  /** a link to another economy's page in this sector */
  geoHref?: (g: GeoInfo) => string | null;
}

function splitRow(id: string, label: string, split: FdiGroupSplit, show: 'n' | 'value'): ShareSplitRow {
  const parts = splitShares(split, show);
  const basis = show === 'n' ? 'projects' : 'disclosed capital';
  return {
    id,
    label: `${label}: by ${basis}`,
    sub: show === 'n' ? projectsWord(split.n) : `${fmtUsdM(split.value)} · ${split.nValue} of ${projectsWord(split.n)} disclose a value`,
    parts,
    cols: { china: fmtShare(show === 'n' ? split.parts.china.shareN : split.parts.china.shareValue), coalition: fmtShare(coalitionShare(split, show)) },
    ariaLabel: `${label}, by ${basis}: ${FDI_GROUP_ORDER.map((k) => `parents headquartered in ${FDI_GROUP_WORDS[k]} ${(100 * (parts[k] ?? 0)).toFixed(1)} percent`).join(', ')}.`,
  };
}

/** Its inbound projects by parent group, against every destination of the cell. */
function InboundGroups({ model, cell, g, inbound, all, basis }: { model: SectorModel; cell: ItemInfo; g: GeoInfo; inbound: FdiProject[]; all: FdiProject[]; basis: FdiBasis }) {
  const own = fdiGroupSplit(inbound);
  const world = fdiGroupSplit(all);
  const t = fdiTotals(all);
  const w: [number, number] = [t.yearMin ?? 0, t.yearMax ?? 0];
  const rows = [splitRow(g.code, g.name, own, 'n'), splitRow('WLD', 'All destinations', world, 'n')];
  if (own.value !== null) rows.splice(1, 0, splitRow(`${g.code}-v`, g.name, own, 'value'));
  if (world.value !== null) rows.push(splitRow('WLD-v', 'All destinations', world, 'value'));
  const src = cmfdiSource(model);
  const csv = useCardCsv(
    () => ({
      name: `${g.code}-${cell.slug}-fdi-inbound-groups-${basis}`,
      text: rowsToCsv(
        [
          { who: g.code, split: own },
          { who: 'WLD', split: world },
        ].flatMap((r) =>
          FDI_GROUP_ORDER.map((k) => ({
            destination_iso3c: r.who,
            destination: r.who === 'WLD' ? 'All destinations' : g.name,
            item: cell.slug,
            basis,
            parent_hq_group: k,
            projects: r.split.parts[k].n,
            share_projects: Number.isFinite(r.split.parts[k].shareN) ? r.split.parts[k].shareN : null,
            projects_disclosing_value: r.split.parts[k].nValue,
            capital_investment_usd_m: r.split.parts[k].value,
            share_disclosed_value: Number.isFinite(r.split.parts[k].shareValue) ? r.split.parts[k].shareValue : null,
            var_id: FDI_VARS.share,
            source: src ? `${src.short} ${src.edition ?? ''}`.trim() : 'CM-FDI',
            licence: 'CC BY 4.0 (attribution required)',
            attribution: src?.attribution ?? null,
          })),
        ),
      ),
    }),
    [FDI_VARS.share],
  );
  return (
    <SectorChartCard
      title={`Whose parents invest in ${g.name}`}
      vars={[{ var: FDI_VARS.share, item: cell.slug, window: w }]}
      subExtra={
        <>
          {` · by year announced · ${basis === 'all' ? 'every status and activity' : 'the projects the Partnership indices use'}`}
          <HowToRead>Two bars: the projects in {g.name} and in all destinations together, each split by where the parent companies have their headquarters. Compare the two to see whether {g.name} draws more of its projects from one group than the world does.</HowToRead>
        </>
      }
      table={() => (
        <TableView
          rows={rows}
          caption={`Inbound projects by the parent's headquarters group: ${g.name} and all destinations`}
          cols={[
            { id: 'r', header: 'Projects', value: (r) => r.label },
            ...FDI_GROUP_ORDER.map((k) => ({ id: k, header: FDI_GROUP_WORDS[k], value: (r: ShareSplitRow) => r.parts[k] ?? 0, cell: (r: ShareSplitRow) => fmtShare(r.parts[k] ?? 0), num: true })),
            { id: 's', header: 'Base', value: (r) => r.sub ?? '' },
          ]}
        />
      )}
      csv={csv}
      legend={<FdiGroupLegend />}
      notes={
        <>
          <p>Shares of the projects (or of their disclosed capital) by where the parent company is headquartered; a joint venture counts wholly under its listed parent.{own.value === null && own.n > 0 ? ` ${g.name}'s inbound projects disclose no capital investment (${NOT_DISCLOSED}).` : ''}</p>
          <FdiAttribution model={model} />
        </>
      }
      summary={`Parents headquartered in China hold ${fmtShare(own.parts.china.shareN)} of the ${projectsWord(own.n)} in ${g.name}, against ${fmtShare(world.parts.china.shareN)} across all destinations.`}
    >
      <ShareSplitChart rows={rows} columns={[{ key: 'china', label: 'Chinese parents', num: true }, { key: 'coalition', label: 'Coalition parents', num: true }]} ariaLabel={`Inbound projects of ${g.name} and of all destinations by the parent's headquarters group`} />
    </SectorChartCard>
  );
}

function Body({ data, p }: { data: FdiData; p: Props }) {
  const { model, cell, g } = p;
  const [basis, setBasis] = useState<FdiBasis>('all');
  const members = useMemo(() => geoMembers(model, g), [model, g]);
  const cellAll = useMemo(() => filterProjects(data.projects, { items: [cell.idx] }), [data, cell]);
  const scoped = useMemo(() => filterProjects(cellAll, { basis }), [cellAll, basis]);
  const { inbound, outbound } = useMemo(() => countryProjects(scoped, members), [scoped, members]);
  const both = useMemo(() => {
    const ins = new Set(inbound.map((x) => x.row));
    return [...inbound, ...outbound].sort((a, z) => a.row - z.row).map((x) => ({ x, dir: ins.has(x.row) ? 'Inbound' : 'Outbound' }));
  }, [inbound, outbound]);
  const dirOf = useMemo(() => new Map(both.map((b) => [b.x.row, b.dir])), [both]);
  const counts = useMemo(() => {
    const a = countryProjects(cellAll, members);
    const u = countryProjects(cellAll.filter((x) => x.indexUse), members);
    return { all: a.inbound.length + a.outbound.length, index_use: u.inbound.length + u.outbound.length };
  }, [cellAll, members]);
  const tin = fdiTotals(inbound);
  const tout = fdiTotals(outbound);
  const split = fdiGroupSplit(inbound);
  // ranks by project count; economies with the same count share a rank
  const dRank = g.kind === 'country' ? rankWords(geoRank(fdiByGeo(scoped, 'dest', 'n'), g.idx), 'destinations') : null;
  const hRank = g.kind === 'country' ? rankWords(geoRank(fdiByGeo(scoped, 'home', 'n'), g.idx), 'investor economies') : null;
  const base = { model, data, item: cell, basis, show: 'n' as const, highlight: null, geoHref: p.geoHref };
  const inScope: FdiScope = { ...base, projects: inbound };
  const outScope: FdiScope = { ...base, projects: outbound };
  const bothScope: FdiScope = { ...base, projects: both.map((b) => b.x) };
  const eu = g.kind === 'eu' ? ' (projects between two EU members are left out)' : '';
  const tiles = [
    { k: 'in', label: 'Inbound projects', value: tin.n.toLocaleString('en-US'), sub: dRank ?? (tin.n ? `announced ${tin.yearMin}-${tin.yearMax}` : 'none recorded') },
    { k: 'inv', label: 'Inbound capital', value: tin.n ? fmtUsdM(tin.value) : '–', sub: tin.n ? `${tin.nValue} of ${projectsWord(tin.n)} disclose a value` : 'no inbound project' },
    { k: 'cn', label: 'Chinese parents (inbound)', value: tin.n ? fmtShare(split.parts.china.shareN) : '–', sub: tin.n ? `${split.parts.china.n} of ${projectsWord(tin.n)} (HQ in China, Hong Kong or Macao)` : 'no inbound project' },
    { k: 'out', label: 'Outbound projects', value: tout.n.toLocaleString('en-US'), sub: hRank ?? (tout.n ? `announced ${tout.yearMin}-${tout.yearMax}` : 'none recorded') },
    { k: 'outv', label: 'Outbound capital', value: tout.n ? fmtUsdM(tout.value) : '–', sub: tout.n ? `${tout.nValue} of ${projectsWord(tout.n)} disclose a value` : 'no outbound project' },
    { k: 'dd', label: 'Destinations of its parents', value: String(tout.dests), sub: tout.n ? `${tout.parents} parent compan${tout.parents === 1 ? 'y' : 'ies'}` : 'no outbound project' },
  ];
  return (
    <div className="space-y-6" data-testid="fdi-country">
      <p className="max-w-3xl text-sm text-paper-dim">
        Cross-border greenfield and expansion projects in {model.cellLabel(cell)} from the Cleantech Manufacturing FDI Dataset: inbound = a parent headquartered elsewhere invests in {g.name}; outbound = a parent headquartered in {g.name} invests abroad{eu}. Counts and disclosed capital as published; domestic investment is not covered.
      </p>
      <FdiControls basis={basis} setBasis={setBasis} counts={counts} />
      <div className="grid grid-cols-2 gap-3 md:grid-cols-3 xl:grid-cols-6" role="list" aria-label={`${g.name}: cross-border projects in ${model.cellLabel(cell)}`}>
        {tiles.map((x) => (
          <div key={x.k} role="listitem">
            <StatTile label={x.label} value={<span className="font-medium">{x.value}</span>} sub={x.sub} />
          </div>
        ))}
      </div>
      {!both.length ? (
        <>
          <p className="text-sm text-paper-mute">CM-FDI records no cross-border project into or out of {g.name} in {model.cellLabel(cell)} for this selection.</p>
          <div className="text-xs text-paper-mute">
            <FdiAttribution model={model} />
          </div>
        </>
      ) : (
        <>
          {inbound.length > 0 && (
            <div className="grid items-start gap-6 xl:grid-cols-2">
              <InvestorsCard s={inScope} title={`Who invests in ${g.name}`} />
              <InboundGroups model={model} cell={cell} g={g} inbound={inbound} all={scoped} basis={basis} />
            </div>
          )}
          {outbound.length > 0 && <DestinationsCard s={outScope} title={`Where parents headquartered in ${g.name} invest`} />}
          <ProjectsCard s={bothScope} title={`${g.name}: inbound and outbound projects`} direction={(x) => dirOf.get(x.row) ?? ''} />
        </>
      )}
    </div>
  );
}

/** The CM-FDI section of an economy's page in a covered Midstream cell. */
export function FdiCountry(p: Props) {
  const { fdi, h, has } = useFdi(true);
  if (!has) return <p className="text-sm text-paper-mute">The cross-border project data (CM-FDI) is not part of this data build.</p>;
  return <ChunkGate h={h} name="sector-fdi">{() => (fdi ? <Body data={fdi} p={p} /> : null)}</ChunkGate>;
}
