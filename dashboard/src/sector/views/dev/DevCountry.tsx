// The economy page's "Aid and investment partners" (development data change 2026-10-15; sector-dev,
// loaded when the section nears the viewport): for the sector's stage, who funds this economy's aid
// in the stage's aid sector (OECD CRS, the pipeline's purpose crosswalk: energy for Deployment, industry
// for Refining, Components and Assembly, mining for Extraction; none for Recovery) and, for a donor, whom it funds; then its bilateral direct-investment positions (IMF DIP):
// where its firms invest and who invests in it, as the investors report it, conduit centres flagged.
// Raw values in USD millions; nothing here is a score.

import { useMemo } from 'react';
import type { SectorDevChunk } from '../../contract.ts';
import type { GeoInfo, ItemInfo, SectorModel } from '../../data/model.ts';
import { devPartners, dipFlows, dipWindow, odaFlows, odaYears, type DevPartner } from '../../derive/dev.ts';
import { rowsToCsv } from '../../../lib/csv.ts';
import { fmtShare } from '../../format.ts';
import { ChunkGate, HowToRead, markGroup } from '../common.tsx';
import { useDev } from '../data.ts';
import { usdM } from '../explore/DevFlowsLayout.tsx';
import { LeagueBars, SectorChartCard, type LeagueRow } from '../kit.tsx';

/** The aid-sector item of a cell (dims.item_links), or null. */
export function aidItemOf(model: SectorModel, cell: ItemInfo): ItemInfo | null {
  return model.itemsOfCell(cell.idx, 'aid_sector')[0]?.item ?? null;
}

function rows(model: SectorModel, ps: DevPartner[]): LeagueRow[] {
  return ps.map((p) => {
    const g = model.geos[p.geo];
    return { id: g.code, label: g.name, group: markGroup(g), value: p.v, display: `${usdM(p.v)}${Number.isFinite(p.share) ? ` · ${fmtShare(p.share)}` : ''}`, badge: p.conduit !== 'none' ? 'conduit centre' : undefined };
  });
}

function Card({ model, title, varId, item, ps, unit, empty, csvName, how }: { model: SectorModel; title: string; varId: string; item: string | null; ps: DevPartner[]; unit: string; empty: string; csvName: string; how: string }) {
  return (
    <SectorChartCard
      title={title}
      vars={[{ var: varId, item }]}
      subExtra={<HowToRead>{how}</HowToRead>}
      csv={() => ({ name: csvName, text: rowsToCsv(ps.map((p) => ({ partner: model.geos[p.geo].code, partner_name: model.geos[p.geo].name, value_usd_m: p.v, share: p.share, conduit: p.conduit, unit }))) })}
      summary={ps[0] ? `${model.geos[ps[0].geo].name} first: ${usdM(ps[0].v)}.` : empty}
    >
      {ps.length ? <LeagueBars rows={rows(model, ps)} axisLabel={unit} top={10} ariaLabel={title} /> : <p className="text-sm text-paper-mute">{empty}</p>}
    </SectorChartCard>
  );
}

function Body({ model, cell, g, d }: { model: SectorModel; cell: ItemInfo; g: GeoInfo; d: SectorDevChunk }) {
  const aid = aidItemOf(model, cell);
  const yrs = aid ? odaYears(d, aid.idx) : [];
  const span = yrs.length ? `${yrs[0]}-${yrs[yrs.length - 1]}` : '';
  const oda = useMemo(() => (aid ? odaFlows(d, aid.idx, null) : []), [d, aid]);
  const dip = useMemo(() => dipFlows(d, false), [d]);
  const what = aid ? aid.label.toLowerCase() : '';
  const fromDonors = aid ? devPartners(oda, g.idx, 'in') : [];
  const toRecipients = aid ? devPartners(oda, g.idx, 'out') : [];
  const out = devPartners(dip, g.idx, 'out');
  const inn = devPartners(dip, g.idx, 'in');
  const win = dipWindow(d) ?? '';
  const aidUnit = 'USD millions, constant 2023 prices';
  const dipUnit = 'USD millions, current';
  return (
    <div className="space-y-6">
      {aid && (
        <div className="grid gap-6 lg:grid-cols-2">
          <Card model={model} title={`Aid for ${what} to ${g.name}, ${span}`} varId="development.oda" item={aid.slug} ps={fromDonors} unit={aidUnit} csvName={`${g.code}-aid-${aid.slug}-donors`}
            how={`Official development assistance for ${what} (the aid sector of this stage) that each donor government disbursed to ${g.name}, summed over ${span}.`}
            empty={`No donor in the data recorded aid for ${what} to ${g.name}.`} />
          {toRecipients.length > 0 && (
            <Card model={model} title={`Aid for ${what} from ${g.name}, ${span}`} varId="development.oda" item={aid.slug} ps={toRecipients} unit={aidUnit} csvName={`${g.code}-aid-${aid.slug}-recipients`}
              how={`What ${g.name} disbursed as official development assistance for ${what} to each recipient, summed over ${span}.`} empty="" />
          )}
        </div>
      )}
      <div className="grid gap-6 lg:grid-cols-2">
        <Card model={model} title={`Where ${g.name}'s firms invest`} varId="development.fdi_out" item="none" ps={out} unit={dipUnit} csvName={`${g.code}-positions-out`}
          how={`Outward direct-investment positions in debt instruments as ${g.name} reports them (mean of ${win}); not specific to this sector.`}
          empty={`${g.name} reports no outward positions.`} />
        <Card model={model} title={`Who invests in ${g.name}`} varId="development.fdi_out" item="none" ps={inn} unit={dipUnit} csvName={`${g.code}-positions-in`}
          how={`Positions in ${g.name} as each investing economy reports them (mean of ${win}); not specific to this sector.`}
          empty={`No investing economy reports a position in ${g.name}.`} />
      </div>
    </div>
  );
}

export function DevCountry({ model, cell, g }: { model: SectorModel; cell: ItemInfo; g: GeoInfo }) {
  const { h, has } = useDev(true);
  if (!has) return null;
  return <ChunkGate h={h} name="sector-dev">{(d) => <Body model={model} cell={cell} g={g} d={d} />}</ChunkGate>;
}
