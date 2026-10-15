// S6 and S7 of the Country profile (spec_country.md A4.2): the memo card (GENERATE MEMO for core members
// with a memo, "being prepared" for core members without one, the core-only explanation for everyone
// else) and the data notes (caveats, sources, the export line, the fact-sheet and all-tables downloads).

import { Link } from 'react-router';
import { Term } from '../../../components/Term.tsx';
import { rowsToCsv, withBom } from '../../../lib/csv.ts';
import { downloadBlob } from '../../../lib/download.ts';
import { factSheetDownloadName, supplierModel } from '../../derive/model.ts';
import { editionSuffix, quadrantWord, proseName } from '../../derive/text.ts';
import { memoReady } from './Header.tsx';
import { Section, useProfile } from './common.tsx';

export function MemoCard() {
  const { sheet, entry, manifest } = useProfile();
  const name = proseName(sheet.meta.country);
  const ready = memoReady(entry);
  return (
    <Section id="memo" title="Policy memo">
      <div className="card p-5">
        {entry.tier !== 'core' ? (
          <p className="text-sm text-paper-dim">
            Memos are available for the {manifest.counts.core} core coalition members for now.{' '}
            <Link className="link" to="/countries#core">
              See the list &rarr;
            </Link>
          </p>
        ) : !ready ? (
          <p className="text-sm text-paper-dim">
            {entry.memo?.stale ? `The memo for ${name} is being updated to the latest data.` : `The memo for ${name} is being prepared.`}
          </p>
        ) : (
          <>
            <p className="max-w-3xl text-sm text-paper-dim">
              A short <Term id="memo">policy memo</Term> summarises this page for {name}: its energy security risks and why, its economic opportunities and why, its
              sectoral priorities beside its current policies, and its partnership priorities.
            </p>
            <div className="mt-4 flex flex-wrap items-center gap-4">
              <Link to={`/countries/${entry.iso3}/memo`} className="btn btn-primary">
                GENERATE MEMO
              </Link>
              <span className="text-sm text-paper-mute">AI-generated from the data and checked against it; paper baseline as of {manifest.as_of_label}.</span>
            </div>
          </>
        )}
      </div>
    </Section>
  );
}

function csvFile(rows: Record<string, unknown>[]): string {
  return withBom(rowsToCsv(rows));
}

export function DataNotes() {
  const { sheet, entry, fileUrl } = useProfile();
  const m = sheet.meta;
  const name = proseName(m.country);
  const zipAll = async () => {
    const { zipTexts } = await import('../../../lib/zip.ts');
    const cells = sheet.cells.map((c) => ({
      sector: c.tech, stage: c.stage, energy_security: c.es?.v ?? null, energy_security_beats_pct: c.es?.pct.v ?? null, energy_security_median: c.es?.med.v ?? null,
      economic_opportunity: c.eo?.v ?? null, economic_opportunity_beats_pct: c.eo?.pct.v ?? null, economic_opportunity_median: c.eo?.med.v ?? null,
      corner: c.quadrant ? quadrantWord(sheet, c.quadrant) : null, strategic_index: c.si.v, strategic_index_rank: c.si.rank.v, priority_third: c.priority?.third ?? null,
      policy_third: c.policy?.third ?? null, policy_records_specific: c.policy?.records_specific.v ?? null, imports_usd: c.trade?.imports.v ?? null, exports_usd: c.trade?.exports.v ?? null,
    }));
    const drivers = sheet.cells.flatMap((c) =>
      (['es', 'eo'] as const).flatMap((pl) =>
        c.drivers[pl].flatMap((d) => [
          { sector: c.tech, stage: c.stage, pillar: pl === 'es' ? 'Energy Security' : 'Economic Opportunity', category: d.category, variable: null, score: d.score.v, median: d.median.v, weight: d.weight.v, impact: d.impact.v, raw_value: null, raw_unit: null, raw_year: null },
          ...d.vars.map((v) => ({ sector: c.tech, stage: c.stage, pillar: pl === 'es' ? 'Energy Security' : 'Economic Opportunity', category: d.category, variable: v.label, score: v.score.v, median: v.median.v, weight: v.weight.v, impact: null, raw_value: v.raw?.v ?? null, raw_unit: v.raw?.unit ?? null, raw_year: v.raw?.year ?? null })),
        ]),
      ),
    );
    const partners = sheet.partners.rows.map((r) => ({
      partner: r.name, iso3: r.iso3, rank: r.rankable ? r.rank.v : null, partnership_strength: r.psi.v, friendshore: r.fs.v, partnership_opportunity: r.po.v, development_potential: r.dp.v,
      main_reason: r.reason_d, imports_share_average: r.ties.imp_mean.v, exports_share_average: r.ties.exp_mean.v, trade_rank: r.ties.tie_rank.v, factory_projects_in: r.ties.fdi_in.v,
      factory_projects_out: r.ties.fdi_out.v, un_votes: r.ties.alignment_d,
    }));
    const sm = supplierModel(sheet);
    const supplier = sm.partners.flatMap((p, i) => sm.cells.map((c, j) => ({ supplier: p.name, sector: c.label, import_share: sm.share[i]?.[j] ?? null, value_usd: sm.value[i]?.[j] ?? null })).filter((r) => r.import_share !== null));
    const nipo = sheet.policy.nipo;
    const measures = nipo
      ? sheet.cells.flatMap((c) => (nipo.cells?.[c.key] ?? []).map((id, k) => {
          const p = nipo.policies.find((x) => x.id === id);
          return { sector: c.tech, stage: c.stage, rank: k + 1, title: p?.title ?? '', implementation_date: p?.date ?? null, in_force: !!p?.date && p?.in_force !== false, strength: p?.strength.v ?? null, mapping_confidence: p?.confidence.v ?? null, link: p?.url ?? null };
        }))
      : [];
    const readme = [
      `Country Insights tables for ${name} (paper baseline as of ${m.as_of_label}).`,
      `Built ${m.created} from Strategic export ${(m.strategic_bundle_id ?? '').slice(0, 8)} and Sector export ${(m.sector_bundle_id ?? '').slice(0, 8)}.`,
      '',
      'sectors.csv     Energy Security, Economic Opportunity, corners, Strategic Index and policy thirds by sector',
      'drivers.csv     categories and variables behind the scores (the sectors the fact sheet carries)',
      'partners.csv    partners listed for the country: Partnership Strength, components, trade, factory projects, UN votes',
      'suppliers.csv   import shares by supplier and sector',
      'measures.csv    strongest active industrial-policy measures by sector (Global Trade Alert, New Industrial Policy Observatory; context only)',
      '',
      'Notes:',
      ...sheet.caveats.map((c) => `- ${c.text}`),
      '',
      'Sources:',
      ...sheet.sources.map((s) => `- ${s.label}${editionSuffix(s.label, s.edition)}${s.licence ? `; ${s.licence}` : ''}${s.url ? `; ${s.url}` : ''}`),
      '',
    ].join('\r\n');
    const bytes = zipTexts({
      'README.txt': readme,
      'sectors.csv': csvFile(cells),
      'drivers.csv': csvFile(drivers),
      'partners.csv': csvFile(partners),
      'suppliers.csv': csvFile(supplier),
      ...(measures.length ? { 'measures.csv': csvFile(measures) } : {}),
    });
    downloadBlob(new Blob([bytes as BlobPart], { type: 'application/zip' }), `${name.toLowerCase().replace(/[^a-z0-9]+/g, '-')}-tables-${m.as_of}.zip`);
  };
  return (
    <Section id="notes" title="Data notes">
      <div className="card p-5">
        <h3 className="mb-2 text-base font-medium text-paper">Notes</h3>
        <ul className="list-disc space-y-1.5 pl-5 text-sm text-paper-dim">
          {sheet.caveats.map((c) => (
            <li key={c.id}>{c.text}</li>
          ))}
        </ul>
        <h3 className="mb-2 mt-5 text-base font-medium text-paper">Sources</h3>
        <ul className="space-y-1.5 text-sm text-paper-dim">
          {sheet.sources.map((s) => (
            <li key={s.id}>
              {s.label}
              {s.edition ? <span className="text-paper-mute">{editionSuffix(s.label, s.edition)}</span> : null}
              {s.licence ? <span className="text-paper-mute">; {s.licence}</span> : null}
              {s.url ? (
                <>
                  {'; '}
                  <a className="link underline decoration-1 underline-offset-2" href={s.url} target="_blank" rel="noopener noreferrer">
                    {s.url.replace(/^https?:\/\//, '').replace(/\/$/, '')}
                  </a>
                </>
              ) : null}
            </li>
          ))}
        </ul>
        <p className="mt-5 text-xs text-paper-mute">
          Paper baseline as of {m.as_of_label}. Built {m.created} from Strategic export {(m.strategic_bundle_id ?? '–').slice(0, 8)} and Sector export {(m.sector_bundle_id ?? '–').slice(0, 8)}.
        </p>
        <div className="mt-4 flex flex-wrap gap-3">
          {entry.file && (
            <a className="btn btn-ghost btn-sm" href={fileUrl(entry.file)} download={factSheetDownloadName(sheet)}>
              Fact sheet (.json)
            </a>
          )}
          <button type="button" className="btn btn-ghost btn-sm" onClick={() => void zipAll()}>
            All tables (.zip)
          </button>
        </div>
      </div>
    </Section>
  );
}

