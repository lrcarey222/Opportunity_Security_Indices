// S5 of the Country profile (spec_country.md A4.2, "Partners"): the partners the Partnership Strength
// Index rates highest from the country's viewpoint beside whom it trades with today (PartnerSlope), the
// partner-by-partner table (reasons, components, trade, factory projects, agreements, UN votes, match),
// who supplies the country in each sector, the top friendshoring opportunities and export markets of one sector (or all
// sectors) at the paper baseline with the pair's ties, its bilateral direct investment (IMF positions;
// factory projects in the four CM-FDI sectors). The sector-level data come from the Strategic bundle's auxiliary chunks (views/Partners/ties.ts), not
// the fact sheet.

import { useMemo, type ReactNode } from 'react';
import { Link } from 'react-router';
import { ChartCard } from '../../../charts/ChartCard.tsx';
import { Term } from '../../../components/Term.tsx';
import { TableView, type Col } from '../../../components/TableView.tsx';
import { cellOf, type PartnerRow } from '../../contract.ts';
import { PartnerSlope } from '../../charts/PartnerSlope.tsx';
import { SupplierMatrix } from '../../charts/SupplierMatrix.tsx';
import type { AibDoc } from '../../../aib/contract.ts';
import { fmtShare, fmtUsd, listWords, nameOf, plural } from '../../../aib/derive/format.ts';
import { compactHref } from '../../../aib/nav.ts';
import { Segmented } from '../../../components/ui.tsx';
import PartnersView from '../../../views/Partners/PartnersView.tsx';
import { aibCountry, buildNodesOf } from '../../derive/framing.ts';
import { devDataLink } from '../../derive/links.ts';
import type { PartnerTab } from '../../url.ts';
import { GROUP_WORD, partnerTableRows, slopeModel, supplierModel } from '../../derive/model.ts';
import { agreementsText, alignmentText, matchText, partnerReason, proseName, poss, cap } from '../../derive/text.ts';
import { BasisTag, csvWithSource, Fold, LinkRow, Section, useProfile, type Basis } from './common.tsx';
import { PartnersBySector } from './PartnersBySector.tsx';
import { AibInvestmentCard } from './AibInvestmentCard.tsx';

type Row = PartnerRow & { extra: boolean };

/** The partners worth showing as a short list: ranked rows by one score, best first. */
function topBy(rows: readonly PartnerRow[], k: 'po' | 'fs' | 'dp', n = 5): PartnerRow[] {
  return rows.filter((r) => r[k].v !== null).sort((a, b) => (b[k].v ?? 0) - (a[k].v ?? 0)).slice(0, n);
}

function Card({ id, title, basis, note, children }: { id: string; title: string; basis: Basis; note?: string; children: ReactNode }) {
  return (
    <section className="card p-5" aria-labelledby={`${id}-h`} data-testid={id}>
      <div className="mb-2">
        <BasisTag kind={basis} note={note} />
      </div>
      <h3 id={`${id}-h`} className="rule-neis-sm text-base font-medium text-paper lg:text-lg">
        {title}
      </h3>
      <div className="mt-2 space-y-2 text-sm text-paper-dim">{children}</div>
    </section>
  );
}

/** A partner's name: a link to its Country profile when it has one. */
export function PartnerName({ iso3, name }: { iso3: string; name: string }) {
  const { manifest } = useProfile();
  const has = manifest.countries.some((e) => e.iso3 === iso3 && e.status !== 'no_data' && !!e.file);
  return has ? (
    <Link className="link text-paper" to={`/countries/${iso3}`}>
      {name}
    </Link>
  ) : (
    <span className="text-paper">{name}</span>
  );
}

function ScoreList({ rows, k, label }: { rows: PartnerRow[]; k: 'po' | 'fs' | 'dp'; label: string }) {
  const { sheet } = useProfile();
  if (!rows.length) return <p className="text-paper-mute">No partner has a {label} score for {proseName(sheet.meta.country)}.</p>;
  return (
    <ol className="space-y-1.5">
      {rows.map((r, i) => (
        <li key={r.iso3} className="flex items-baseline justify-between gap-3">
          <span>
            <span className="tabular text-paper-mute">{i + 1}. </span>
            <PartnerName iso3={r.iso3} name={r.name} />
            {r.income_group && k === 'dp' && <span className="text-xs text-paper-mute"> · {r.income_group}</span>}
          </span>
          <span className="tabular text-xs text-paper-mute">{r[k].d}</span>
        </li>
      ))}
    </ol>
  );
}

const TAB_OPTIONS: { value: PartnerTab; label: string }[] = [
  { value: 'trade', label: 'Trade' },
  { value: 'investment', label: 'Investment' },
  { value: 'development', label: 'Development' },
];

/**
 * "Partnerships" (IA refactor 2026-10-16, brief s.12D): Trade | Investment | Development (URL `ptab`),
 * conclusions first (the Allied Industrial Base results are facts; the partner scores are preference-
 * adjusted and shown at the NEIS baseline from the fact sheet, the model's version under the reader's
 * Perspective in a disclosure), the existing cards behind disclosures.
 */
export function PartnershipsSection() {
  const { sheet, state, setPtab, aib } = useProfile();
  const name = proseName(sheet.meta.country);
  const iso = sheet.iso3;
  const doc = aib?.doc ?? null;
  const names = aib?.names;
  const a = useMemo(() => (doc && names ? aibCountry(doc, names, iso) : null), [doc, names, iso]);
  const tab = state.ptab;
  const rows = sheet.partners.rows;
  const nodeName = (id: string) => doc?.nodes.find((nd) => nd.id === id)?.label ?? id;
  const aibMissing =
    aib?.status === 'loading' ? <p className="text-paper-mute">Loading the Allied Industrial Base results…</p> : <p className="text-paper-mute">The Allied Industrial Base results are not available in this build.</p>;
  return (
    <Section
      id="partnerships"
      aliases={['partners']}
      title="Partnerships"
      intro={
        <>
          Who {name} could trade, invest and build supply chains with. The Allied Industrial Base results (coordinated trade, hosts for new capacity, nodes that need new capacity) are
          modelled facts; the partner scores of the <Term id="psi" /> (export markets, <Term id="fs" />, <Term id="dp" />) weigh preferences and are shown at the NEIS baseline.
        </>
      }
    >
      <Segmented label="Kind of partnership" value={tab} options={TAB_OPTIONS} onChange={(v) => setPtab?.(v)} />
      {tab === 'trade' && (
        <div className="space-y-6" data-testid="ptab-trade">
          <div className="grid gap-4 lg:grid-cols-2">
            <Card id="aib-trade" title="Coordinated trade in the Allied Industrial Base" basis="facts">
              {!a ? (
                aibMissing
              ) : a.pairs.length ? (
                <>
                  <p>
                    In the compacts&apos; coordinated trade {name} has {plural(a.trade.n, 'relationship')} worth {fmtUsd(a.trade.usd)}
                    {a.trade.nNew ? `, ${a.trade.nNew} of them new` : ''}. The largest:
                  </p>
                  <ol className="space-y-1.5">
                    {a.pairs.slice(0, 5).map((p) => (
                      <li key={p.partner}>
                        <PartnerName iso3={p.partner} name={nameOf(names ?? {}, p.partner)} /> <span className="tabular text-xs text-paper-mute">{fmtUsd(p.usd)}</span>
                        <span className="block text-xs text-paper-mute">
                          {p.outUsd >= p.inUsd ? `${cap(name)} supplies` : `${cap(name)} buys`} {listWords(p.nodes.slice(0, 2).map(nodeName))}
                          {p.nNew ? ` · ${p.nNew} new` : ''}
                        </span>
                      </li>
                    ))}
                  </ol>
                </>
              ) : (
                <p>{cap(name)} has no trade relationship in the compacts&apos; coordinated trade.</p>
              )}
            </Card>
            <Card id="po-top" title="Export markets (Partnership Opportunity)" basis="prefs" note="NEIS baseline">
              <ScoreList rows={topBy(rows, 'po')} k="po" label="Partnership Opportunity" />
            </Card>
          </div>
          <div>
            <Fold id="po-sector" summary="Top export markets sector by sector">
              <PartnersBySector part="po" />
            </Fold>
            <Fold id="suppliers" summary={`Who supplies ${name} in each sector today`}>
              {rows.length ? <SupplierCard /> : <p className="text-sm text-paper-mute">{cap(name)} has no partner rows.</p>}
            </Fold>
          </div>
        </div>
      )}
      {tab === 'investment' && (
        <div className="space-y-6" data-testid="ptab-investment">
          <div className="grid gap-4 lg:grid-cols-2">
            <AibInvestmentCard />
            <Card id="fs-top" title="Friendshoring opportunities (Friendshore)" basis="prefs" note="NEIS baseline">
              <ScoreList rows={topBy(rows, 'fs')} k="fs" label="Friendshore" />
            </Card>
          </div>
          <div>
            <Fold id="fs-sector" summary="Friendshoring opportunities sector by sector">
              <PartnersBySector part="fs" />
            </Fold>
            <Fold id="fdi" summary={`Direct investment today: IMF positions and factory projects of ${name}`}>
              <PartnersBySector part="investment" />
            </Fold>
          </div>
        </div>
      )}
      {tab === 'development' && (
        <div className="space-y-6" data-testid="ptab-development">
          <div className="grid gap-4 lg:grid-cols-2">
            <Card id="dp-top" title="Development partners (Development Potential)" basis="prefs" note="NEIS baseline">
              <p className="text-xs text-paper-mute">Middle- and lower-income partners where {name} could build supply chains together.</p>
              <ScoreList rows={topBy(rows, 'dp')} k="dp" label="Development Potential" />
            </Card>
            <Card id="aib-build" title="Capacity to build in its compacts" basis="facts">
              {!a ? aibMissing : <BuildNodes doc={doc} compactIds={a.compacts.map((x) => x.compact.id)} names={names ?? {}} name={name} />}
            </Card>
          </div>
          <LinkRow>
            <Link className="link" to={devDataLink(null)}>
              The development data behind this (World Bank indicators, aid flows, direct investment) &rarr;
            </Link>
          </LinkRow>
        </div>
      )}
      <div>
        <Fold id="partner-table" summary={`Every partner of ${name}: priority partners beside today's partners, partner by partner (NEIS baseline)`}>
          {rows.length ? (
            <>
              <SlopeCard />
              <PartnerTable />
            </>
          ) : (
            <p className="text-sm text-paper-mute">{cap(name)} has no partner rows in the Partnership Strength Index.</p>
          )}
        </Fold>
        <Fold id="partners-model" engine summary="Partner scores under your perspective: Partnership Strength, Friendshore, Partnership Opportunity and Development Potential (the model)">
          <PartnersView embedded />
        </Fold>
      </div>
    </Section>
  );
}

function BuildNodes({ doc, compactIds, names, name }: { doc: AibDoc | null; compactIds: string[]; names: Readonly<Record<string, string>>; name: string }) {
  const key = compactIds.join('|');
  // eslint-disable-next-line react-hooks/exhaustive-deps
  const nodes = useMemo(() => (doc ? buildNodesOf(doc, compactIds) : []), [doc, key]);
  if (!nodes.length) return <p>No node of {poss(name)} compacts is classed BUILD (new capacity needed).</p>;
  return (
    <>
      <p>Supply-chain nodes of {poss(name)} compacts that need new capacity (BUILD), largest remaining gap first, with the hosts the model lists:</p>
      <ul className="space-y-1.5">
        {nodes.slice(0, 6).map((nd) => (
          <li key={nd.id}>
            <span className="text-paper">{nd.label}</span> <span className="tabular text-xs text-paper-mute">residual gap {fmtShare(nd.residual, 0)}</span>
            {nd.compact && (
              <>
                {' '}
                <Link className="link text-xs" to={compactHref(nd.compact)}>
                  {nd.compact}
                </Link>
              </>
            )}
            <span className="block text-xs text-paper-mute">{nd.hosts.length ? `Hosts: ${listWords(nd.hosts.slice(0, 4).map((h) => nameOf(names, h)))}` : 'No host listed'}</span>
          </li>
        ))}
      </ul>
    </>
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
  const { sheet, cell, partner, setPartner } = useProfile();
  const pc = partner ? cellOf(sheet, cell) : null;
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
      notes={
        <span className="text-sm" data-testid="dev-data-link">
          <Link className="link" to={devDataLink(partner ?? null, pc ? { tech: pc.tech, stage: pc.stage } : null)}>
            The development data behind this &rarr;
          </Link>
          <span className="text-paper-mute">
            {pc && partner
              ? ` the selected partner's aid received and given, and its direct-investment positions, in Sector Insights (${pc.label})`
              : ' World Bank development indicators, OECD aid flows and IMF direct-investment positions behind Development Potential, in Sector Insights'}
          </span>
        </span>
      }
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
