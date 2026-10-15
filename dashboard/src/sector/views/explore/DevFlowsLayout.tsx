// Explore, the flow layout of Development & finance (development data change 2026-10-15; chunk
// sector-dev, loaded here only): development.oda, official development assistance from each donor
// government to each recipient for the chosen aid sector (one year, or every year summed), and
// development.fdi_out, the IMF bilateral direct-investment positions (who invests where). Each shows
// the Sankey of the largest sources to the largest destinations (the top flows list on phones), the
// highlighted economy's partners on both sides, and the table of every flow. Positions with a conduit
// financial centre are shown and flagged (grey bands); "Leave out conduit centres" drops them as the
// Friendshore investment component does. Raw values only: nothing here is a score.

import { useMemo, useState } from 'react';
import { SankeyChart, type SankeyFlow } from '../../../charts/Sankey.tsx';
import { TableView } from '../../../components/TableView.tsx';
import { Segmented } from '../../../components/ui.tsx';
import { rowsToCsv } from '../../../lib/csv.ts';
import { useMediaQuery } from '../../../shell/useMediaQuery.ts';
import { GROUP_COLOR } from '../../../theme/palette.ts';
import type { SectorDevChunk } from '../../contract.ts';
import type { SectorModel } from '../../data/model.ts';
import { conduitWords, devPartners, devSankey, devTotals, dipFlows, dipWindow, odaFlows, OTHER_FROM, OTHER_TO, positiveTotal, type DevFlow, type DevPartner } from '../../derive/dev.ts';
import { fmtShare, fmtUsd } from '../../format.ts';
import { ChunkGate, HowToRead, markGroup } from '../common.tsx';
import { useDev } from '../data.ts';
import { GroupLegend, LeagueBars, SectorChartCard, TopFlowsList, type LeagueRow } from '../kit.tsx';
import type { ExploreCtx } from './ctx.ts';

/** USD millions as text ("USD 1.2 bn"). */
export const usdM = (x: number) => fmtUsd(x * 1e6);
const GREY = '#6b7280';
const CONDUIT_LINK = '#9ca3af59';

interface Words {
  title: string;
  left: string;
  right: string;
  from: string;
  to: string;
  out: string;
  in: string;
  unit: string;
}

function words(isOda: boolean, what: string, when: string): Words {
  return isOda
    ? { title: `Who funds whom: aid for ${what}, ${when}`, left: 'DONORS', right: 'RECIPIENTS', from: 'Donor', to: 'Recipient', out: 'gives to', in: 'receives from', unit: `USD millions, constant 2023 prices` }
    : { title: `Who invests where: direct-investment positions, ${when}`, left: 'INVESTORS', right: 'DESTINATIONS', from: 'Investor', to: 'Destination', out: 'invests in', in: 'is invested in by', unit: 'USD millions, current' };
}

function partnerRows(model: SectorModel, rows: DevPartner[]): LeagueRow[] {
  return rows.map((p) => {
    const g = model.geos[p.geo];
    return {
      id: g.code,
      label: g.name,
      group: markGroup(g),
      value: p.v,
      display: `${usdM(p.v)}${Number.isFinite(p.share) ? ` · ${fmtShare(p.share)}` : ''}`,
      badge: p.conduit !== 'none' ? 'conduit centre' : undefined,
    };
  });
}

function Body({ c, d }: { c: ExploreCtx; d: SectorDevChunk }) {
  const { model, v, item, highlight } = c;
  const wide = useMediaQuery('(min-width: 640px)');
  const isOda = v.id === 'development.oda';
  const [span, setSpan] = useState<'year' | 'all'>('year');
  const [conduits, setConduits] = useState<'show' | 'hide'>('show');
  const geos = model.geos;
  const year = c.year;
  const flows: DevFlow[] = useMemo(() => {
    if (isOda) return item ? odaFlows(d, item.idx, span === 'all' ? null : year) : [];
    return dipFlows(d, conduits === 'hide');
  }, [d, isOda, item, span, year, conduits]);
  const sank = useMemo(() => devSankey(flows, { topFrom: 8, topTo: 10, fold: 0.005 }), [flows]);
  const totals = useMemo(() => [...devTotals(flows).values()], [flows]);
  const W = positiveTotal(flows);
  const yrs = v.years;
  const when = isOda ? (span === 'all' && yrs.length ? `${yrs[0]}-${yrs[yrs.length - 1]}` : String(year ?? '')) : `mean of ${dipWindow(d) ?? ''}`;
  const what = isOda && item ? item.label.toLowerCase() : '';
  const w = words(isOda, what, when);
  const code = (g: number) => (g === OTHER_FROM ? '__OTHER_F' : g === OTHER_TO ? '__OTHER_T' : geos[g].code);
  const nameOf = (cd: string) => (cd === '__OTHER_F' ? (isOda ? 'Other donors' : 'Other investors') : cd === '__OTHER_T' ? (isOda ? 'Other recipients' : 'Other destinations') : (model.geoByCode.get(cd)?.name ?? cd));
  const conduitLinks = new Set(sank.links.filter((l) => l.conduit).map((l) => `${code(l.from)}|${code(l.to)}`));
  const sankeyFlows: SankeyFlow[] = sank.links.map((l) => ({ producer: code(l.from), consumer: code(l.to), value: l.v }));
  const negatives = flows.filter((f) => f.v < 0).length;
  const nConduit = flows.filter((f) => f.conduit !== 'none').length;
  const sorted = useMemo(() => [...flows].sort((a, z) => z.v - a.v), [flows]);
  const ref = { var: v.id, item: item?.slug ?? null, year: isOda && span === 'year' ? year : null };
  const csv = () => ({
    name: isOda ? `aid-flows-${item?.slug ?? 'all'}-${span === 'all' ? 'all-years' : year}` : `investment-positions${conduits === 'hide' ? '-no-conduits' : ''}`,
    text: rowsToCsv(
      sorted.map((f) => ({
        [isOda ? 'donor' : 'investor']: geos[f.from].code,
        [isOda ? 'donor_name' : 'investor_name']: geos[f.from].name,
        [isOda ? 'recipient' : 'destination']: geos[f.to].code,
        [isOda ? 'recipient_name' : 'destination_name']: geos[f.to].name,
        var_id: v.id,
        item: item?.slug ?? null,
        year: isOda ? (span === 'all' ? `${yrs[0]}-${yrs[yrs.length - 1]}` : year) : `${f.y0}-${f.y1}`,
        value_usd_m: f.v,
        unit: w.unit,
        ...(isOda ? {} : { years_reported: f.n, conduit: f.conduit }),
        source: v.source.short,
        licence: v.licence,
      })),
    ),
  });
  const tools = (
    <div className="flex flex-wrap gap-2">
      {isOda ? (
        <Segmented<'year' | 'all'> label="Period" hideLabel value={span} options={[{ value: 'year', label: String(year ?? '') }, { value: 'all', label: yrs.length ? `${yrs[0]}-${yrs[yrs.length - 1]} total` : 'All years' }]} onChange={setSpan} />
      ) : (
        <Segmented<'show' | 'hide'> label="Conduit centres" hideLabel value={conduits} options={[{ value: 'show', label: 'All positions' }, { value: 'hide', label: 'Leave out conduit centres' }]} onChange={setConduits} />
      )}
    </div>
  );
  const top = sank.links[0];
  const tableCols = [
    { id: 'f', header: w.from, value: (f: DevFlow) => geos[f.from].name },
    { id: 't', header: w.to, value: (f: DevFlow) => geos[f.to].name },
    { id: 'v', header: `Value (${isOda ? 'USD m, 2023 prices' : 'USD m'})`, value: (f: DevFlow) => f.v, cell: (f: DevFlow) => usdM(f.v), num: true },
    { id: 's', header: 'Share of all flows shown', value: (f: DevFlow) => (W > 0 && f.v > 0 ? f.v / W : null), cell: (f: DevFlow) => (W > 0 && f.v > 0 ? fmtShare(f.v / W) : '–'), num: true },
    ...(isOda
      ? []
      : [
          { id: 'y', header: 'Years reported', value: (f: DevFlow) => `${f.y0}-${f.y1} (${f.n})` },
          { id: 'c', header: 'Conduit centre', value: (f: DevFlow) => conduitWords(f.conduit) },
        ]),
  ];
  const outRows = highlight ? devPartners(flows, highlight.idx, 'out') : [];
  const inRows = highlight ? devPartners(flows, highlight.idx, 'in') : [];
  const bigOut = [...totals].filter((t) => t.out > 0).sort((a, z) => z.out - a.out);
  const bigIn = [...totals].filter((t) => t.in > 0).sort((a, z) => z.in - a.in);
  const totalRows = (rows: typeof totals, side: 'out' | 'in'): LeagueRow[] =>
    rows.map((t) => {
      const g = geos[t.geo];
      const x = side === 'out' ? t.out : t.in;
      return { id: g.code, label: g.name, group: markGroup(g), value: x, display: `${usdM(x)} · ${fmtShare(W > 0 ? x / W : NaN)}`, badge: d['dev.conduits'].cols.geo.includes(t.geo) ? 'conduit centre' : undefined };
    });
  const pick = (r: LeagueRow) => {
    const g = model.geoByCode.get(r.id) ?? null;
    c.setHighlight(g && g.idx !== highlight?.idx ? g : null);
  };

  return (
    <div className="space-y-6">
      <SectorChartCard
        title={w.title}
        vars={[ref]}
        subExtra={
          <HowToRead>
            Bands run from the {isOda ? 'donor governments' : 'investing economies'} (left) to the {isOda ? 'recipients' : 'economies they invest in'} (right); the wider the band, the larger the {isOda ? 'aid' : 'position'}. Colours show the {isOda ? 'donor' : 'investor'}&apos;s group{isOda ? '' : '; grey bands involve a conduit financial centre'}. Small flows fold into &quot;Other&quot;; the table lists every flow.
          </HowToRead>
        }
        tools={tools}
        table={() => <TableView rows={sorted} caption={isOda ? 'Aid flows' : 'Investment positions'} search pageSize={25} cols={tableCols} />}
        csv={csv}
        legend={<GroupLegend groups={['china', 'core', 'partner', 'rest']} />}
        notes={
          <>
            <p>
              Total of the flows shown: {usdM(W)} ({w.unit}).{' '}
              {isOda
                ? 'Gross disbursements of official development assistance by bilateral donor governments; multilateral lenders and private flows are not in the data.'
                : 'Outward positions in debt instruments (loans and debt securities between related firms; equity is not in this measure), the mean of the years each pair reports.'}
            </p>
            {negatives > 0 && (
              <p>
                {negatives.toLocaleString('en-US')} {isOda ? (negatives === 1 ? 'flow is' : 'flows are') : negatives === 1 ? 'position is' : 'positions are'} below zero ({isOda ? 'mining aid net of its coal and oil-and-gas sub-rows' : 'reported as negative by the investing economy'}): in the table, not in the picture.
              </p>
            )}
            {!isOda && nConduit > 0 && <p>{nConduit.toLocaleString('en-US')} positions involve a conduit financial centre (pass-through investment with little activity behind it; grey bands). The Friendshore investment component treats them as not observed; they are shown here, flagged.</p>}
          </>
        }
        summary={top ? `The largest flow is ${nameOf(code(top.from))} to ${nameOf(code(top.to))}, ${usdM(top.v)} (${fmtShare(top.share)} of the flows shown).` : 'No flows.'}
      >
        {!sank.links.length ? (
          <p className="text-sm text-paper-mute">No flows recorded{isOda && item ? ` for ${what}` : ''}.</p>
        ) : wide ? (
          <SankeyChart
            flows={sankeyFlows}
            label={nameOf}
            ariaLabel={w.title}
            valueFormat={(x) => `${usdM(x)} · ${fmtShare(W > 0 ? x / W : NaN)}`}
            nodeColor={(cd, side) => (cd.startsWith('__') ? GREY : side === 'producer' ? GROUP_COLOR[markGroup(model.geoByCode.get(cd))] : GREY)}
            linkColor={(f) => (conduitLinks.has(`${f.producer}|${f.consumer}`) ? CONDUIT_LINK : `${f.producer.startsWith('__') ? GREY : GROUP_COLOR[markGroup(model.geoByCode.get(f.producer))]}8c`)}
            sideLabels={{ left: w.left, right: w.right }}
            tipNote="of the flows shown"
          />
        ) : (
          <TopFlowsList
            flows={sank.links.slice(0, 12).map((l, i) => ({
              id: String(i),
              from: nameOf(code(l.from)),
              to: nameOf(code(l.to)),
              fromGroup: l.from >= 0 ? markGroup(geos[l.from]) : 'rest',
              value: usdM(l.v),
              share: `${fmtShare(l.share)} of the flows shown`,
              flag: l.conduit ? 'conduit centre' : undefined,
            }))}
            ariaLabel={w.title}
          />
        )}
      </SectorChartCard>

      <div className="grid gap-6 lg:grid-cols-2">
        <SectorChartCard title={isOda ? `Largest donors, ${when}` : `Largest investors, ${when}`} vars={[ref]} summary={bigOut[0] ? `${geos[bigOut[0].geo].name} ${isOda ? 'gives' : 'holds'} the most: ${usdM(bigOut[0].out)}.` : 'None.'}>
          <LeagueBars rows={totalRows(bigOut, 'out')} axisLabel={w.unit} selected={highlight?.code ?? null} onSelect={pick} top={15} ariaLabel={isOda ? 'Largest donors' : 'Largest investors'} />
        </SectorChartCard>
        <SectorChartCard title={isOda ? `Largest recipients, ${when}` : `Largest destinations, ${when}`} vars={[ref]} summary={bigIn[0] ? `${geos[bigIn[0].geo].name} ${isOda ? 'receives' : 'is invested in'} the most: ${usdM(bigIn[0].in)}.` : 'None.'}>
          <LeagueBars rows={totalRows(bigIn, 'in')} axisLabel={w.unit} selected={highlight?.code ?? null} onSelect={pick} top={15} ariaLabel={isOda ? 'Largest recipients' : 'Largest destinations'} />
        </SectorChartCard>
      </div>

      {highlight && (
        <div className="grid gap-6 lg:grid-cols-2">
          {(['out', 'in'] as const).map((side) => {
            const rows = side === 'out' ? outRows : inRows;
            const title = `${highlight.name} ${side === 'out' ? w.out : w.in}${isOda ? '' : side === 'in' ? ' (as the investors report it)' : ''}`;
            return (
              <SectorChartCard
                key={side}
                title={title}
                vars={[ref]}
                csv={() => ({ name: `${highlight.code}-${isOda ? 'aid' : 'positions'}-${side}`, text: rowsToCsv(rows.map((p) => ({ economy: highlight.code, partner: geos[p.geo].code, partner_name: geos[p.geo].name, side: side === 'out' ? w.out : w.in, value_usd_m: p.v, share: p.share, conduit: p.conduit, unit: w.unit, source: v.source.short }))) })}
                summary={rows[0] ? `${geos[rows[0].geo].name} first: ${usdM(rows[0].v)}.` : 'No flows.'}
              >
                {rows.length ? <LeagueBars rows={partnerRows(model, rows)} axisLabel={w.unit} top={15} ariaLabel={title} /> : <p className="text-sm text-paper-mute">{isOda ? (side === 'out' ? `${highlight.name} is not a donor in the data.` : `${highlight.name} received no recorded aid here.`) : side === 'out' ? `${highlight.name} reports no outward positions.` : 'No investor reports a position in it.'}</p>}
              </SectorChartCard>
            );
          })}
        </div>
      )}
    </div>
  );
}

export function DevFlowsLayout({ c }: { c: ExploreCtx }) {
  const { h, has } = useDev(true);
  if (!has) return <p className="text-sm text-paper-mute">Aid flows and investment positions are not in this build.</p>;
  return <ChunkGate h={h} name="sector-dev">{(d) => <Body c={c} d={d} />}</ChunkGate>;
}
