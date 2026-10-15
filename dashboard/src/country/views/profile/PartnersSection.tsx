// S5 of the Country profile (spec_country.md A4.2, "Partners"): the partners the Partnership Strength
// Index rates highest from the country's viewpoint beside whom it trades with today (PartnerSlope), the
// partner-by-partner table (reasons, components, trade, factory projects, agreements, UN votes, match),
// who supplies the country in each sector, and its role in the allied network design.

import { useMemo } from 'react';
import { Link } from 'react-router';
import { ChartCard } from '../../../charts/ChartCard.tsx';
import { Term } from '../../../components/Term.tsx';
import { TableView, type Col } from '../../../components/TableView.tsx';
import { cellOf, type PartnerRow } from '../../contract.ts';
import { PartnerSlope } from '../../charts/PartnerSlope.tsx';
import { SupplierMatrix } from '../../charts/SupplierMatrix.tsx';
import { networkLink, partnersLink } from '../../derive/links.ts';
import { GROUP_WORD, partnerTableRows, slopeModel, supplierModel } from '../../derive/model.ts';
import { agreementsText, alignmentText, matchText, networkSentence, partnerReason, proseName, poss, cap } from '../../derive/text.ts';
import { csvWithSource, LinkRow, Section, useProfile } from './common.tsx';

type Row = PartnerRow & { extra: boolean };

export function PartnersSection() {
  const { sheet } = useProfile();
  const name = proseName(sheet.meta.country);
  const hasRows = sheet.partners.rows.length > 0;
  return (
    <Section
      id="partners"
      title="Partners"
      intro={
        <>
          The <Term id="psi" /> rates every other economy as a partner for {name} in energy supply chains: as a supplier it can rely on (<Term id="fs" />), as a market for
          its exports (<Term id="po" />) and, for middle- and lower-income partners, as a place to build supply chains together (<Term id="dp" />). This section sets the
          partners the index points to beside the ties {name} has today.
        </>
      }
    >
      {hasRows ? (
        <>
          <SlopeCard />
          <PartnerTable />
          <SupplierCard />
        </>
      ) : (
        <p className="text-sm text-paper-mute">{cap(name)} has no partner rows in the Partnership Strength Index.</p>
      )}
      <NetworkCard />
      <LinkRow>
        <Link className="link" to={partnersLink(sheet.iso3)}>
          Change the weighting (hawkishness, bullishness) for {poss(name)} partners &rarr;
        </Link>
      </LinkRow>
    </Section>
  );
}

function SlopeCard() {
  const { sheet, partner, setPartner } = useProfile();
  const name = proseName(sheet.meta.country);
  const m = useMemo(() => slopeModel(sheet), [sheet]);
  const union = [...new Set([...m.left.map((s) => s.iso3), ...m.right.map((s) => s.iso3)])];
  const rows = new Map(sheet.partners.rows.map((r) => [r.iso3, r]));
  type U = { iso3: string; name: string; group: string; psi_rank: string; psi: string; trade_rank: string; imports: string; exports: string };
  const twin: U[] = union.map((iso) => {
    const r = rows.get(iso)!;
    return { iso3: iso, name: r.name, group: GROUP_WORD[r.group], psi_rank: r.rankable ? r.rank.d : 'not ranked', psi: r.psi.d, trade_rank: r.ties.tie_rank.d, imports: r.ties.imp_mean.d, exports: r.ties.exp_mean.d };
  });
  return (
    <ChartCard
      headingLevel={3}
      title="Priority partners and today's partners"
      sub={`Left: the partners the index rates highest for ${name}. Right: the partners it trades most with today across its energy-goods baskets (average share of its imports and of its exports in ${sheet.meta.trade_year}, never summed across baskets). Lines join the same partner.`}
      summary={`${cap(name)}: the top ${m.left.length} partners by Partnership Strength and the top ${m.right.length} by average trade share.`}
      table={() => (
        <TableView<U>
          rows={twin}
          cols={[
            { id: 'name', header: 'Partner', value: (r) => r.name },
            { id: 'group', header: 'Group', value: (r) => r.group },
            { id: 'psi_rank', header: 'Partnership Strength rank', value: (r) => r.psi_rank },
            { id: 'psi', header: 'Partnership Strength', value: (r) => r.psi, num: true },
            { id: 'trade_rank', header: 'Trade rank', value: (r) => r.trade_rank },
            { id: 'imports', header: 'Share of imports (average)', value: (r) => r.imports, num: true },
            { id: 'exports', header: 'Share of exports (average)', value: (r) => r.exports, num: true },
          ]}
          caption={`${cap(name)}: priority partners and trade partners`}
          rowKey={(r) => r.iso3}
          onRowClick={(r) => setPartner(r.iso3)}
          selectedKey={partner}
        />
      )}
    >
      <PartnerSlope left={m.left} right={m.right} selected={partner} onSelect={setPartner} />
    </ChartCard>
  );
}

function PartnerTable() {
  const { sheet, partner, setPartner } = useProfile();
  const name = proseName(sheet.meta.country);
  const rows = useMemo(() => partnerTableRows(sheet), [sheet]);
  const cols: Col<Row>[] = [
    { id: 'partner', header: 'Partner', value: (r) => r.name, cell: (r) => <span>{r.name} <span className="text-xs text-paper-mute">({GROUP_WORD[r.group]}){r.extra ? ' · trade partner' : ''}</span></span>, minWidth: 150 },
    { id: 'rank', header: 'Rank', value: (r) => (r.rankable ? r.rank.v : null), cell: (r) => (r.rankable ? r.rank.d : `not ranked${r.rank_reason ? `: ${r.rank_reason}` : ''}`), num: true },
    { id: 'psi', header: 'Partnership Strength', value: (r) => r.psi.v, cell: (r) => r.psi.d, num: true },
    { id: 'reason', header: 'Main reason', value: (r) => partnerReason(sheet, r), minWidth: 200 },
    { id: 'fs', header: 'Friendshore', value: (r) => r.fs.v, cell: (r) => r.fs.d, num: true },
    { id: 'po', header: 'Partnership Opportunity', value: (r) => r.po.v, cell: (r) => r.po.d, num: true },
    { id: 'dp', header: 'Development Potential', value: (r) => r.dp.v, cell: (r) => r.dp.d, num: true, minWidth: 120 },
    { id: 'imp', header: 'Imports from', value: (r) => r.ties.imp_mean.v, cell: (r) => `${r.ties.imp_mean.d}${(r.ties.n_import_cells.v ?? 0) > 0 ? `; top-3 supplier in ${r.ties.top3_import_cells.d} of ${r.ties.n_import_cells.d}` : ''}`, num: true, minWidth: 150 },
    { id: 'exp', header: 'Exports to', value: (r) => r.ties.exp_mean.v, cell: (r) => r.ties.exp_mean.d, num: true },
    { id: 'fdi', header: 'Factory projects', value: (r) => (r.ties.fdi_in.v ?? 0) + (r.ties.fdi_out.v ?? 0), cell: (r) => `${r.ties.fdi_in.d} in ${name} · ${r.ties.fdi_out.d} by ${poss(name)} firms there`, minWidth: 170 },
    { id: 'agreements', header: 'Agreements', value: (r) => agreementsText(sheet, r.ties.agreements), minWidth: 140 },
    { id: 'un', header: 'UN votes', value: (r) => alignmentText(r), cell: (r) => <span title={r.ties.bloc ? `${r.name}: ${r.ties.bloc}` : undefined}>{alignmentText(r)}</span> },
    { id: 'match', header: 'Match', value: (r) => matchText(sheet, r), minWidth: 160 },
  ];
  const csv = () => ({
    name: `${sheet.meta.country} partners`,
    text: csvWithSource(
      rows.map((r) => ({
        partner: r.name, iso3: r.iso3, group: GROUP_WORD[r.group], rank: r.rankable ? r.rank.v : null, partnership_strength: r.psi.v, main_reason: partnerReason(sheet, r),
        friendshore: r.fs.v, partnership_opportunity: r.po.v, development_potential: r.dp.v, imports_share_average: r.ties.imp_mean.v, exports_share_average: r.ties.exp_mean.v,
        top3_supplier_in: r.ties.top3_import_cells.v, import_baskets: r.ties.n_import_cells.v, trade_rank: r.ties.tie_rank.v, factory_projects_in: r.ties.fdi_in.v,
        factory_projects_out: r.ties.fdi_out.v, agreements: agreementsText(sheet, r.ties.agreements), un_votes: alignmentText(r), match: r.match,
      })),
      'Opportunity Security Indices, paper baseline; UN Comtrade (reconciled); CM-FDI; agreements; UN General Assembly votes',
      sheet.meta.as_of,
    ),
  });
  return (
    <ChartCard
      headingLevel={3}
      title="Partner by partner"
      sub={`The top 15 partners by Partnership Strength from ${poss(name)} viewpoint, plus any of its ten largest trade partners not among them (marked "trade partner"). Development Potential is scored only for middle- and lower-income partners. Factory projects are cross-border clean-tech manufacturing projects (solar, wind, batteries, electric vehicles). Click a row to select the partner.`}
      summary={`${cap(name)}: ${rows.length} partners with their Partnership Strength, components and today's ties.`}
      csv={csv}
    >
      <TableView<Row> rows={rows} cols={cols} caption={`${cap(name)}: partners by Partnership Strength`} rowKey={(r) => r.iso3} onRowClick={(r) => setPartner(r.iso3)} selectedKey={partner} search pageSize={15} initialSort={{ id: 'rank' }} />
    </ChartCard>
  );
}

function SupplierCard() {
  const { sheet, partner, setPartner } = useProfile();
  const name = proseName(sheet.meta.country);
  const m = useMemo(() => supplierModel(sheet), [sheet]);
  type L = { partner: string; sector: string; share: number | null; value: number | null; status: string | null };
  const long: L[] = [];
  m.partners.forEach((p, i) =>
    m.cells.forEach((c, j) => {
      const s = m.share[i]?.[j] ?? null;
      if (s !== null) long.push({ partner: p.name, sector: c.label, share: s, value: m.value[i]?.[j] ?? null, status: m.status[i]?.[j] ?? null });
    }),
  );
  const pct = (x: number | null) => (x === null ? '–' : `${(100 * x).toFixed(1)}%`);
  return (
    <ChartCard
      headingLevel={3}
      title={`Who supplies ${name} in each sector (${sheet.meta.trade_year})`}
      sub={`Each dot is one supplier's share of ${poss(name)} imports in a sector (bigger dot, bigger share). Rows: the largest suppliers of its high-priority sectors and its largest suppliers on average. The selected partner's row is outlined; a star marks the sectors among its Friendshore best five for ${name}.`}
      summary={`${cap(name)}: import shares by supplier and sector, ${sheet.meta.trade_year}.`}
      table={() => (
        <TableView<L>
          rows={long}
          cols={[
            { id: 'partner', header: 'Supplier', value: (r) => r.partner },
            { id: 'sector', header: 'Sector', value: (r) => r.sector },
            { id: 'share', header: 'Share of imports', value: (r) => r.share, cell: (r) => pct(r.share), num: true },
            { id: 'value', header: 'Value (USD)', value: (r) => r.value, cell: (r) => (r.value === null ? '–' : Math.round(r.value).toLocaleString('en-US')), num: true },
            { id: 'status', header: 'Recorded by', value: (r) => (r.status === 'mirror' ? 'the supplier (mirror)' : 'the importer') },
          ]}
          caption={`${cap(name)}: import shares by supplier and sector`}
          search
        />
      )}
      csv={() => ({
        name: `${sheet.meta.country} suppliers by sector`,
        text: csvWithSource(long.map((r) => ({ supplier: r.partner, sector: r.sector, import_share: r.share, value_usd: r.value, recorded_by: r.status })), 'UN Comtrade, reconciled bilateral matrix', sheet.meta.as_of),
      })}
      notes="Where a flow was not reported by the importer it is read from the exporter's report, raised by 6% to value it like imports."
    >
      <SupplierMatrix sheet={sheet} selected={partner} onSelect={setPartner} />
    </ChartCard>
  );
}

function NetworkCard() {
  const { sheet } = useProfile();
  const n = sheet.network;
  const largest = n.member && n.producer.length ? [...n.producer].sort((a, b) => (b.share.v ?? 0) - (a.share.v ?? 0))[0] : null;
  const c = largest ? cellOf(sheet, largest.cell) : null;
  return (
    <section className="card p-5" aria-labelledby="network-h">
      <h3 id="network-h" className="rule-neis-sm text-base font-medium text-paper lg:text-lg">
        Role in the allied network design
      </h3>
      <div className="mt-2 space-y-2 text-sm text-paper-dim">
        {networkSentence(sheet).map((t) => (
          <p key={t}>{t}</p>
        ))}
        {n.member && (
          <p className="text-xs text-paper-mute">
            The <Term id="network" /> spreads each upstream and midstream sector&apos;s production across coalition members so that none dominates.
          </p>
        )}
      </div>
      {n.member && (
        <LinkRow>
          <Link className="link" to={networkLink(c)}>
            See the network &rarr;
          </Link>
        </LinkRow>
      )}
    </section>
  );
}
