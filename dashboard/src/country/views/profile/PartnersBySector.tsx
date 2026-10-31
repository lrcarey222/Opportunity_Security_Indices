// S5 of the Country profile, partners by sector (spec_country.md A4.2 "Partners", 2026-10-15): the top
// friendshoring opportunities (Friendshore) and export markets (Partnership Opportunity) of one sector (or all sectors)
// at the baseline with the ties each pair has today, then the country's bilateral direct
// investment (IMF positions; factory projects in the four CM-FDI sectors). The data come from the
// Strategic bundle's auxiliary chunks (views/Partners/ties.ts; one pcell-<tech>-<stage> chunk per six-stage
// sector), not the fact sheet. The factory projects sit on the stages the sheet names (meta.cmfdi_stages:
// Components and Assembly carry the same projects, design E5), and the card says so once.

import { useEffect, useMemo, useState } from 'react';
import { Link } from 'react-router';
import { ChartCard } from '../../../charts/ChartCard.tsx';
import { TableView } from '../../../components/TableView.tsx';
import { Select } from '../../../components/ui.tsx';
import { cellOptions } from '../../../lib/format.ts';
import { PILLAR_COLOR } from '../../../theme/palette.ts';
import { InvestmentCards } from '../../../views/Partners/InvestmentCard.tsx';
import {
  alignmentOf, CMFDI_ATTRIBUTION, cmfdiSameProjectsNote, hasCmfdi, loadSectorTop, loadTies, projectFlows, sharedAgreements, tiesName, type Ties, type TopRow,
} from '../../../views/Partners/ties.ts';
import { cellOf } from '../../contract.ts';
import { devDataLink, partnersLink } from '../../derive/links.ts';
import { supplierModel } from '../../derive/model.ts';
import { agreementsText, cap, poss, proseName } from '../../derive/text.ts';
import { csvWithSource, LinkRow, useProfile } from './common.tsx';

/** The ties chunk of the Strategic bundle (loaded once per page). */
function useTies(): { ties: Ties | null; error: string | null } {
  const [s, set] = useState<{ ties: Ties | null; error: string | null }>({ ties: null, error: null });
  useEffect(() => {
    let live = true;
    loadTies().then(
      (t) => live && set({ ties: t, error: t ? null : 'This data build has no partner-by-sector data.' }),
      (e: Error) => live && set({ ties: null, error: `Could not load the partner data: ${e.message}` }),
    );
    return () => {
      live = false;
    };
  }, []);
  return s;
}

type TopItem = TopRow & { name: string; projects: string | null; agreements: string[]; un: number | null; share: number | null };

/**
 * `part` (IA refactor 2026-10-16, the Partnerships tabs): 'po' = export markets only (Trade), 'fs' =
 * friendshoring opportunities only (Investment), 'investment' = the direct-investment cards only; none =
 * the original card pair.
 */
export function PartnersBySector({ part }: { part?: 'po' | 'fs' | 'investment' } = {}) {
  const { sheet, cell, partner, setPartner } = useProfile();
  const name = proseName(sheet.meta.country);
  const iso = sheet.iso3;
  const { ties, error } = useTies();
  // the sector shown: the reader's choice here, else the sector selected on the page, else all sectors
  const [pick, setPick] = useState<string | null | undefined>(undefined);
  const c = cellOf(sheet, pick === undefined ? cell : pick);
  const cKey = c?.key ?? null;
  const [top, setTop] = useState<{ key: string; rows: TopRow[] | null; error: string | null } | null>(null);
  useEffect(() => {
    if (!c) return;
    let live = true;
    loadSectorTop(c.tech, c.stage).then(
      (t) => live && setTop({ key: c.key, rows: t ? t.top.filter((r) => r.reporter_iso === iso) : null, error: t ? null : 'No partner data for this sector in this build.' }),
      (e: Error) => live && setTop({ key: c.key, rows: null, error: e.message }),
    );
    return () => {
      live = false;
    };
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [cKey, iso]);
  const supplier = useMemo(() => supplierModel(sheet), [sheet]);
  const rows: TopRow[] | null = c ? (top && top.key === c.key ? top.rows : null) : ties ? ties.top_all.filter((r) => r.reporter_iso === iso) : null;
  // the stages whose sectors carry the CM-FDI factory projects: the sheet's own list (Components, Assembly)
  const cmStages = sheet.meta.cmfdi_stages ?? [];
  const cmfdiHere = !!c && hasCmfdi(c.tech, c.stage, cmStages);
  const items = useMemo((): TopItem[] => {
    if (!rows || !ties) return [];
    const j = c ? supplier.cells.findIndex((x) => x.key === c.key) : -1;
    const flows = c && cmfdiHere ? projectFlows(ties, iso, c.tech, c.stage) : null;
    return rows.map((r) => {
      const i = supplier.partners.findIndex((p) => p.iso3 === r.partner_iso);
      const f = flows?.find((x) => x.partner_iso === r.partner_iso);
      return {
        ...r,
        name: tiesName(ties, r.partner_iso),
        projects: flows ? (f ? `${f.out_projects} by ${poss(name)} firms there, ${f.in_projects} by theirs in ${name}` : 'none either way') : null,
        agreements: sharedAgreements(ties, iso, r.partner_iso),
        un: alignmentOf(ties, iso, r.partner_iso),
        share: i >= 0 && j >= 0 ? (supplier.share[i]?.[j] ?? null) : null,
      };
    });
  }, [rows, ties, c, cmfdiHere, supplier, iso, name]);
  const label = c ? c.label : "all sectors (average of each partner's five strongest)";
  const options = [{ value: '', label: 'All sectors' }, ...cellOptions(sheet.cells, (x) => x.stage, (x) => x.key)];
  const side = (sd: 'fs' | 'po') => items.filter((r) => r.side === sd).sort((a, b) => a.k - b.k);
  const asWord = (r: TopItem) => (r.side === 'fs' ? 'friendshoring opportunity (Friendshore)' : 'market (Partnership Opportunity)');
  const list = (sd: 'fs' | 'po') => (
    <div>
      <h4 className="mb-2 text-sm font-medium text-paper">{sd === 'fs' ? 'Friendshoring opportunities (Friendshore)' : 'Top export markets (Partnership Opportunity)'}</h4>
      <ol className="space-y-1.5" aria-label={sd === 'fs' ? 'Friendshoring opportunities' : 'Top export markets'}>
        {side(sd).map((r) => {
          const notes = [
            sd === 'fs' && r.share !== null ? `${(100 * r.share).toFixed(1)}% of ${poss(name)} imports in this sector` : null,
            r.projects ? `factory projects: ${r.projects}` : null,
            r.agreements.length ? `agreements: ${agreementsText(sheet, r.agreements)}` : null,
            r.un !== null ? `UN-vote alignment ${r.un.toFixed(2)}` : null,
          ].filter(Boolean);
          const on = partner === r.partner_iso;
          return (
            <li key={r.partner_iso}>
              <button type="button" className={`w-full rounded px-1 text-left ${on ? 'outline outline-2 outline-signal' : 'hover:bg-ink-700/40'}`} onClick={() => setPartner(r.partner_iso)} aria-pressed={on}>
                <span className="flex items-baseline justify-between gap-3 text-sm">
                  <span className="text-paper-dim">
                    {r.rank ?? r.k}. {r.name}
                    {r.psi_rank !== null && <span className="text-xs text-paper-mute"> · Partnership Strength rank {r.psi_rank}</span>}
                  </span>
                  <span className="tabular text-xs text-paper-mute">{r.value === null ? '–' : r.value.toFixed(3)}</span>
                </span>
                <span className="relative mt-0.5 block h-1.5 w-full bg-ink-700/60" aria-hidden="true">
                  <span className="absolute inset-y-0 left-0" style={{ width: `${Math.max(0, Math.min(1, r.value ?? 0)) * 100}%`, background: sd === 'fs' ? PILLAR_COLOR.ES : PILLAR_COLOR.EO }} />
                </span>
                {notes.length > 0 && <span className="mt-0.5 block text-xs text-paper-mute">{notes.join(' · ')}</span>}
              </button>
            </li>
          );
        })}
      </ol>
    </div>
  );
  const err = error ?? (c && top?.key === c.key ? top.error : null);
  const invCards = (
    <InvestmentCards
      iso={iso}
      name={name}
      ties={ties}
      error={error}
      sector={c ? { tech: c.tech, stage: c.stage } : null}
      nameOf={(x) => tiesName(ties, x)}
      headingLevel={3}
      selected={partner}
      onSelect={setPartner}
    />
  );
  if (part === 'investment') return invCards;
  const sides: ('fs' | 'po')[] = part ? [part] : ['fs', 'po'];
  const cardTitle = part === 'po' ? 'Top export markets' : part === 'fs' ? 'Friendshoring opportunities' : 'Friendshoring opportunities and export markets';
  return (
    <>
      <ChartCard
        headingLevel={3}
        title={`${cardTitle}: ${c ? c.label : 'all sectors'}`}
        sub={`The partners the index rates highest for ${name} in ${label}, at the baseline: as friendshoring opportunities (Friendshore, 0 to 1: partners where ${poss(name)} security need meets the partner's economic opportunity, on existing trade and investment ties) and as markets for its exports (Partnership Opportunity, 0 to 1), with the ties each pair has today where they exist. Within one sector ${poss(name)} own need is the same for every partner, so the Friendshore ranking there turns on the partner's opportunity, the ties, UN-vote alignment and ownership; the import shares show who supplies it today. The sector starts on the one selected above; choose another here. Click a partner to select it.`}
        summary={`${cap(name)}, ${label}: top ${side('fs').length} friendshoring opportunities and top ${side('po').length} export markets.`}
        tools={<Select label="Sector" value={c ? c.key : ''} options={options} onChange={(v) => setPick(v || null)} className="min-w-[14rem]" />}
        table={() => (
          <TableView<TopItem>
            rows={items}
            cols={[
              { id: 'side', header: 'As', value: asWord },
              { id: 'k', header: 'Rank', value: (r) => r.rank ?? r.k, num: true },
              { id: 'name', header: 'Partner', value: (r) => r.name },
              { id: 'value', header: 'Score (0 to 1)', value: (r) => r.value, cell: (r) => (r.value === null ? '–' : r.value.toFixed(3)), num: true },
              { id: 'psi', header: 'Partnership Strength rank', value: (r) => r.psi_rank, num: true },
              { id: 'share', header: 'Share of imports in the sector', value: (r) => (r.side === 'fs' ? r.share : null), cell: (r) => (r.side === 'fs' && r.share !== null ? `${(100 * r.share).toFixed(1)}%` : '–'), num: true },
              { id: 'projects', header: 'Factory projects', value: (r) => r.projects },
              { id: 'agreements', header: 'Shared agreements', value: (r) => agreementsText(sheet, r.agreements) },
              { id: 'un', header: 'UN-vote alignment', value: (r) => r.un, cell: (r) => (r.un === null ? '–' : r.un.toFixed(2)), num: true },
            ]}
            caption={`${cap(name)}: top partners, ${label}`}
            rowKey={(r) => `${r.side}-${r.partner_iso}`}
          />
        )}
        csv={() => ({
          name: `${sheet.meta.country} top partners ${c ? c.label : 'all sectors'}`,
          text: csvWithSource(
            items.map((r) => ({
              sector: c ? c.label : 'all sectors (best five)', as: asWord(r), rank: r.rank ?? r.k, partner: r.name, iso3: r.partner_iso, score: r.value, partnership_strength_rank: r.psi_rank,
              import_share: r.side === 'fs' ? r.share : null, factory_projects: r.projects, shared_agreements: agreementsText(sheet, r.agreements), un_vote_alignment: r.un,
            })),
            "Partner scores computed by the Explorer's authors (baseline); UN Comtrade (reconciled); CM-FDI (CC BY 4.0); agreements; UN General Assembly votes",
            sheet.meta.as_of,
          ),
        })}
        notes={
          cmfdiHere ? (
            <>
              {cmfdiSameProjectsNote(cmStages) && <span data-testid="cmfdi-same-projects">{cmfdiSameProjectsNote(cmStages)} </span>}
              <span data-testid="cmfdi-attribution">{ties?.meta.cmfdi_attribution ?? CMFDI_ATTRIBUTION}</span>
            </>
          ) : undefined
        }
      >
        {err ? (
          <p className="text-sm text-paper-mute">{err}</p>
        ) : !rows || !ties ? (
          <p className="text-sm text-paper-mute">Loading partners…</p>
        ) : items.length ? (
          <div className={sides.length > 1 ? 'grid gap-6 md:grid-cols-2' : undefined}>{sides.map((sd) => <div key={sd}>{list(sd)}</div>)}</div>
        ) : (
          <p className="text-sm text-paper-mute">No partner scores for {name} in {label}.</p>
        )}
        <LinkRow>
          {!part && (
            <Link className="link" to={partnersLink(iso, c ? { tech: c.tech, stage: c.stage } : null)}>
              Change the weighting for {poss(name)} partners in {c ? c.label : 'all sectors'} &rarr;
            </Link>
          )}
          <Link className="link" to={devDataLink(partner ?? null, c && partner ? { tech: c.tech, stage: c.stage } : null)}>
            The development data behind this &rarr;
          </Link>
        </LinkRow>
      </ChartCard>
      {!part && invCards}
    </>
  );
}
