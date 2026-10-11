// Sources & data /sector/sources (spec_sector.md 4.8): (1) About Sector Insights (content/sector.md,
// rendered with marked, counts filled from the data build); (2) the variable dictionary (search, CSV);
// (3) one row per source; (4) coverage: items x families as a heat table shaded by the number of
// economies, and the coalition's members x sources; (5) the trade-basis reconciliation (matrix vs
// exporter-reported, WARN rows marked); (6) downloads (#downloads; /sector/data scrolls here): all
// sector data as a ZIP, the flows of one technology, the policy list; (7) the build: export date, git
// commit, bundle id, whether it comes from the same pipeline run as the Strategic data, the inputs and
// the validation summary.

import { useEffect, useMemo, useState } from 'react';
import { useLocation } from 'react-router';
import { marked } from 'marked';
import sectorMd from '../../content/sector.md?raw';
import { TableView } from '../../components/TableView.tsx';
import { KeyValue, Notice, PageHead, Select } from '../../components/ui.tsx';
import { withBom } from '../../lib/csv.ts';
import { downloadBlob } from '../../lib/download.ts';
import { rowsToCsv } from '../../lib/csv.ts';
import { useBuildInfo } from '../../shell/buildInfo.ts';
import { useTitle } from '../../shell/title.ts';
import { FAMILY_IDS, TECH_LABELS, tradeChunkName, type TradeTechSlug } from '../contract.ts';
import { factsIndexOf, seriesIndexOf } from '../data/facts.ts';
import { useModel, useSectorManifest, useSectorStore } from '../data/SectorDataProvider.tsx';
import type { SectorModel } from '../data/model.ts';
import { reasonText } from '../derive/cells.ts';
import { allDataFiles, coverageRows, dictionaryRows, flowsCsv, policyCsv, RESTRICTED_LINE, sourcesRows } from '../derive/downloads.ts';
import { safeUrl } from '../derive/policy.ts';
import { fmtExportDate, fmtHhi, fmtShare, fmtSize, fmtUsd } from '../format.ts';
import { ExternalLink, Section } from './common.tsx';
import { useScrollToHash } from './data.ts';
import { HeatTable, SectorChartCard, type HeatRow } from './kit.tsx';

/** content/sector.md with the counts of this data build. */
export function aboutHtml(model: SectorModel): string {
  const core = model.countries.filter((g) => g.group === 'core').length;
  const partner = model.countries.filter((g) => g.group === 'partner').length;
  const out = model.euOutsideCoalition;
  const eu = out.length
    ? `${out.join(', ')} ${out.length === 1 ? 'is an EU member' : 'are EU members'} outside the coalition tiers; in bloc mode ${out.length === 1 ? 'its values count' : 'their values count'} in the core.`
    : '';
  const md = sectorMd
    .replace(/\{members_n\}/g, String(core + partner))
    .replace(/\{core_n\}/g, String(core))
    .replace(/\{partner_n\}/g, String(partner))
    .replace(/\{eu_outside_sentence\}/g, eu);
  return marked.parse(md, { async: false, gfm: true }) as string;
}

function Dictionary() {
  const model = useModel();
  const rows = useMemo(() => dictionaryRows(model), [model]);
  return (
    <SectorChartCard
      title="Variable dictionary"
      vars={model.vars.slice(0, 1).map((v) => ({ var: v.id }))}
      caption={{ sub: `${model.vars.length} measures in this data build: label, unit, data year, level, how it adds up, source, licence and caveats.`, badge: null, caveats: [], coverage: null, spanNote: null }}
      csv={() => ({ name: 'sector-dictionary', text: rowsToCsv(rows) })}
      summary={`${rows.length} measures.`}
    >
      <TableView
        rows={model.vars}
        caption="Variable dictionary"
        search
        pageSize={20}
        rowKey={(v) => v.id}
        cols={[
          { id: 'l', header: 'Measure', value: (v) => v.label, minWidth: 200 },
          { id: 'id', header: 'Id', value: (v) => v.id },
          { id: 'u', header: 'Unit', value: (v) => v.unit.long || v.unit.short },
          { id: 'y', header: 'Data year', value: (v) => v.data_year },
          { id: 'lv', header: 'Level', value: (v) => v.native_level },
          { id: 'a', header: 'Adds up', value: (v) => (v.agg === 'sum' ? 'sum' : v.agg === 'ratio' ? 'ratio of sums' : 'no') },
          { id: 's', header: 'Source', value: (v) => v.source.short },
          { id: 'lc', header: 'Licence', value: (v) => v.licence },
          { id: 'p', header: 'Phase', value: (v) => v.phase },
          { id: 'c', header: 'Caveats', value: (v) => model.caveatTexts(v).join(' · ') || null, minWidth: 240 },
        ]}
      />
    </SectorChartCard>
  );
}

function Sources() {
  const model = useModel();
  const rows = [...model.sources.values()];
  return (
    <SectorChartCard
      title="Sources"
      vars={model.vars.slice(0, 1).map((v) => ({ var: v.id }))}
      caption={{ sub: `${rows.length} sources, with how this export transforms each one and its known issues.`, badge: null, caveats: [], coverage: null, spanNote: null }}
      csv={() => ({ name: 'sector-sources', text: rowsToCsv(sourcesRows(model)) })}
      summary={`${rows.length} sources.`}
    >
      <TableView
        rows={rows}
        caption="Sources"
        search
        pageSize={20}
        rowKey={(s) => s.id}
        cols={[
          { id: 's', header: 'Source', value: (s) => s.short, minWidth: 150 },
          { id: 'c', header: 'Citation', value: (s) => s.citation, minWidth: 260 },
          { id: 'p', header: 'Publisher', value: (s) => s.publisher },
          { id: 'e', header: 'Edition', value: (s) => s.edition },
          { id: 'u', header: 'Link', value: (s) => safeUrl(s.url), cell: (s) => { const u = safeUrl(s.url); return u ? <ExternalLink href={u}>{new URL(u).hostname}</ExternalLink> : '–'; } },
          { id: 'l', header: 'Licence', value: (s) => (s.licence === 'attribution' ? 'open, attribution' : s.licence) },
          { id: 'a', header: 'Accessed', value: (s) => s.accessed },
          { id: 'at', header: 'Attribution', value: (s) => s.attribution, minWidth: 200 },
          { id: 't', header: 'How the export handles it', value: (s) => s.transform, minWidth: 220 },
          { id: 'i', header: 'Known issues', value: (s) => s.issues, minWidth: 200 },
        ]}
      />
    </SectorChartCard>
  );
}

const STATUS_ICON: Record<string, string> = { country: '✓', region: '◐', global: '○', licensed: '', none: '—' };

function Coverage() {
  const model = useModel();
  const fams = FAMILY_IDS.filter((f) => f !== 'development' && [...model.coverage.values()].some((c) => c.family === f));
  const cells = model.cells;
  const techRows: HeatRow[] = useMemo(() => {
    const byTech = new Map<string, HeatRow>();
    for (const c of cells) {
      const t = c.techSlug ?? c.slug;
      let r = byTech.get(t);
      if (!r) byTech.set(t, (r = { key: `tech:${t}`, label: c.tech ?? c.label, children: [] }));
      r.children!.push({ key: String(c.idx), label: c.stage ?? c.label });
    }
    return [...byTech.values()].flatMap((r) => r.children!.map((k) => ({ key: k.key, label: `${r.label} — ${k.label}` })));
  }, [cells]);
  const max = Math.max(1, ...[...model.coverage.values()].map((c) => c.n_geo));
  // coalition members x rosters
  const members = model.countries.filter((g) => g.group === 'core' || g.group === 'partner').sort((a, z) => (a.group === z.group ? a.name.localeCompare(z.name, 'en') : a.group === 'core' ? -1 : 1));
  const rosterCols = useMemo(() => {
    const seen = new Map<string, string>();
    for (const v of model.vars) if (v.roster && model.rosters.has(v.roster) && !seen.has(v.roster)) seen.set(v.roster, v.source.short);
    const labels = [...seen.entries()].map(([r, s]) => ({ roster: r, label: s }));
    const dup = new Set(labels.map((x) => x.label).filter((l, i, a) => a.indexOf(l) !== i));
    return labels.map((x) => ({ ...x, label: dup.has(x.label) ? `${x.label} (${x.roster.replace(/_/g, ' ')})` : x.label }));
  }, [model]);
  const missing = rosterCols
    .map((c) => ({ c, miss: members.filter((g) => !model.rosters.get(c.roster)?.has(g.idx)) }))
    .filter((x) => x.miss.length > 0 && x.miss.length < members.length);
  return (
    <div className="space-y-6">
      <SectorChartCard
        title="Coverage by sector and measure family"
        vars={model.vars.slice(0, 1).map((v) => ({ var: v.id }))}
        caption={{ sub: '✓ country data · ◐ region or entity data · ○ a global value only · — not available. Shaded by the number of economies with data.', badge: null, caveats: [], coverage: null, spanNote: null }}
        csv={() => ({ name: 'sector-coverage', text: rowsToCsv(coverageRows(model)) })}
        summary={`Coverage of ${cells.length} sectors across ${fams.length} measure families.`}
      >
        <HeatTable
          rows={techRows}
          cols={fams.map((f) => ({ key: f, label: model.families.get(f)?.label ?? f }))}
          rowHeader="Sector"
          caption="Coverage: sectors by measure family"
          domain={[0, max]}
          cell={(row, col) => {
            const c = model.coverage.get(`${row}|${col}`);
            if (!c || c.status === 'none' || c.status === 'licensed') return { value: null, display: c?.status === 'licensed' ? 'licensed' : STATUS_ICON.none, title: c ? (c.status === 'licensed' ? 'Licensed: not in this build' : reasonText(c.reason)) : 'No data' };
            return { value: c.n_geo, display: `${STATUS_ICON[c.status]} ${c.status === 'country' ? c.n_geo : ''}`.trim(), title: `${c.status === 'country' ? `${c.n_geo} economies` : c.status === 'region' ? `${c.n_geo} regions or entities` : 'a global value'}${c.sources.length ? `; ${c.sources.map((s) => model.sources.get(s)?.short ?? s).join(', ')}` : ''}` };
          }}
        />
      </SectorChartCard>
      <SectorChartCard
        title="Coalition coverage"
        vars={model.vars.slice(0, 1).map((v) => ({ var: v.id }))}
        caption={{ sub: `The coalition's ${members.length} members against each source's coverage (✓ covered, — not covered).`, badge: null, caveats: [], coverage: null, spanNote: null }}
        csv={() => ({ name: 'coalition-coverage', text: rowsToCsv(members.map((g) => ({ iso3c: g.code, economy: g.name, tier: g.tier, ...Object.fromEntries(rosterCols.map((c) => [c.roster, model.rosters.get(c.roster)?.has(g.idx) ? 1 : 0])) }))) })}
        notes={
          missing.length ? (
            <ul className="list-disc space-y-0.5 pl-5">
              {missing.map((x) => (
                <li key={x.c.roster}>
                  {x.c.label} does not cover {x.miss.map((g) => g.code).join(', ')}.
                </li>
              ))}
            </ul>
          ) : undefined
        }
        summary={`${members.length} coalition members against ${rosterCols.length} source rosters.`}
      >
        <TableView
          rows={members}
          caption="Coalition members by source"
          search
          pageSize={20}
          rowKey={(g) => g.code}
          cols={[
            { id: 'e', header: 'Member', value: (g) => g.name, minWidth: 140 },
            { id: 't', header: 'Tier', value: (g) => (g.group === 'core' ? 'core' : 'partner') },
            ...rosterCols.map((c) => ({ id: c.roster, header: c.label, value: (g: (typeof members)[number]) => (model.rosters.get(c.roster)?.has(g.idx) ? '✓' : '—'), title: c.roster })),
          ]}
        />
      </SectorChartCard>
    </div>
  );
}

function Reconciliation() {
  const model = useModel();
  const rows = [...model.reconcile].sort((a, z) => (model.items[a.item]?.sort ?? 0) - (model.items[z.item]?.sort ?? 0) || z.year - a.year);
  if (!rows.length) return <p className="text-sm text-paper-mute">No reconciliation rows in this build.</p>;
  const warn = rows.filter((r) => r.warn).length;
  return (
    <SectorChartCard
      title="Trade basis: the matrix and the exporters' reports"
      vars={[{ var: 'trade.exports' }, { var: 'trade.rca' }]}
      caption={{ coverage: `${rows.length} sector-years; ${warn} marked WARN (China's shares differ by more than 3 points).` }}
      csv={() => ({ name: 'trade-basis-reconciliation', text: rowsToCsv(rows.map((r) => ({ item: model.items[r.item]?.slug, year: r.year, matrix_total_usd: r.matrix_total, reported_total_usd: r.reported_total, matrix_china_share: r.matrix_china, reported_china_share: r.reported_china, matrix_hhi: r.matrix_hhi, reported_hhi: r.reported_hhi, n_reporters: r.n_reporters, warn: r.warn }))) })}
      notes={
        <p>
          The gap has three reasons: importers declare cost, insurance and freight while exporters declare free on board; economies that do not report are seen in the matrix through their partners; Hong Kong's
          re-exports are out of the exporter-reported basis. Strategic Insights ranks the exporter-reported HHI.
        </p>
      }
      summary={`${rows.length} sector-years compared; ${warn} differ in China's share by more than 3 points.`}
    >
      <TableView
        rows={rows}
        caption="Trade-basis reconciliation"
        pageSize={30}
        rowKey={(r) => `${r.item}|${r.year}`}
        cols={[
          { id: 'c', header: 'Sector', value: (r) => (model.items[r.item] ? model.cellLabel(model.items[r.item]) : ''), minWidth: 170 },
          { id: 'y', header: 'Year', value: (r) => r.year, num: true },
          { id: 'mt', header: 'Matrix total', value: (r) => r.matrix_total, cell: (r) => fmtUsd(r.matrix_total, 'table'), num: true },
          { id: 'rt', header: 'Exporter-reported total', value: (r) => r.reported_total, cell: (r) => fmtUsd(r.reported_total, 'table'), num: true },
          { id: 'mc', header: 'China (matrix)', value: (r) => r.matrix_china, cell: (r) => fmtShare(r.matrix_china), num: true },
          { id: 'rc', header: 'China (reported)', value: (r) => r.reported_china, cell: (r) => fmtShare(r.reported_china), num: true },
          { id: 'mh', header: 'HHI (matrix)', value: (r) => r.matrix_hhi, cell: (r) => fmtHhi(r.matrix_hhi), num: true },
          { id: 'rh', header: 'HHI (reported)', value: (r) => r.reported_hhi, cell: (r) => fmtHhi(r.reported_hhi), num: true },
          { id: 'n', header: 'Reporters', value: (r) => r.n_reporters, num: true },
          { id: 'w', header: 'Check', value: (r) => (r.warn ? 'WARN' : 'ok'), cell: (r) => (r.warn ? <span className="tag" style={{ borderColor: 'var(--color-signal)', color: 'var(--color-paper)' }}>WARN</span> : 'ok') },
        ]}
      />
    </SectorChartCard>
  );
}

function Downloads() {
  const model = useModel();
  const store = useSectorStore();
  const manifest = useSectorManifest();
  const loc = useLocation();
  const tradeTechs = (manifest.data?.chunks ?? []).filter((c) => c.name.startsWith('sector-trade-')).map((c) => c.name.slice('sector-trade-'.length) as TradeTechSlug);
  const [tech, setTech] = useState<TradeTechSlug | null>(null);
  const t = tech && tradeTechs.includes(tech) ? tech : (tradeTechs[0] ?? null);
  const [busy, setBusy] = useState<string | null>(null);
  const [err, setErr] = useState<string | null>(null);
  const run = (key: string, fn: () => Promise<void>) => async () => {
    setBusy(key);
    setErr(null);
    try {
      await fn();
    } catch (e) {
      setErr(e instanceof Error ? e.message : String(e));
    } finally {
      setBusy(null);
    }
  };
  const size = (name: string) => {
    const b = store.chunkBytes(name as never);
    return b ? ` (${fmtSize(b)} to load)` : '';
  };
  const all = run('all', async () => {
    const f = await store.load('sector-facts');
    const s = manifest.data?.chunks.some((c) => c.name === 'sector-series') ? await store.load('sector-series') : null;
    const files = allDataFiles(model, manifest.data, factsIndexOf(f), s ? seriesIndexOf(s) : null, `${window.location.origin}${loc.pathname}`);
    const withBoms = Object.fromEntries(Object.entries(files).map(([k, v]) => [k, k.endsWith('.csv') ? withBom(v) : v]));
    const { zipTexts } = await import('../../lib/zip.ts');
    downloadBlob(new Blob([zipTexts(withBoms) as BlobPart], { type: 'application/zip' }), 'sector-data.zip');
  });
  const flows = run('flows', async () => {
    if (!t) return;
    const chunk = await store.load(tradeChunkName(t));
    downloadBlob(new Blob([withBom(flowsCsv(model, chunk as never))], { type: 'text/csv;charset=utf-8' }), `trade-flows-${t}.csv`);
  });
  const policy = run('policy', async () => {
    const p = await store.load('sector-policy');
    const pt = manifest.data?.chunks.some((c) => c.name === 'sector-policy-text') ? await store.load('sector-policy-text') : null;
    downloadBlob(new Blob([withBom(policyCsv(model, p, pt))], { type: 'text/csv;charset=utf-8' }), 'policy-list.csv');
  });
  return (
    <div className="space-y-4">
      <ul className="grid gap-4 md:grid-cols-3">
        <li className="card flex flex-col p-5">
          <h3 className="text-base font-medium text-paper">All sector data (.zip)</h3>
          <p className="mt-1 flex-1 text-sm text-paper-dim">One CSV per measure family, dictionary.csv, sources.csv, coverage.csv and a README with citations, licences, the vintage and the status codes.{size('sector-facts')}</p>
          <button type="button" className="btn btn-ghost btn-sm mt-3 self-start" disabled={busy !== null} onClick={all}>
            {busy === 'all' ? 'Preparing…' : 'Download all sector data'}
          </button>
        </li>
        <li className="card flex flex-col p-5">
          <h3 className="text-base font-medium text-paper">Trade flows for one technology (.csv)</h3>
          <p className="mt-1 flex-1 text-sm text-paper-dim">Every bilateral flow of the technology's sectors, 2020 and 2024, from the reconciled matrix.{t ? size(tradeChunkName(t)) : ''}</p>
          {tradeTechs.length > 0 ? (
            <div className="mt-3 flex flex-wrap items-end gap-2">
              <Select label="Technology" value={t ?? ''} options={tradeTechs.map((x) => ({ value: x, label: TECH_LABELS[x] }))} onChange={(x) => setTech(x as TradeTechSlug)} className="min-w-[10rem]" />
              <button type="button" className="btn btn-ghost btn-sm" disabled={busy !== null || !t} onClick={flows}>
                {busy === 'flows' ? 'Preparing…' : 'Download flows'}
              </button>
            </div>
          ) : (
            <p className="mt-3 text-sm text-paper-mute">No trade data in this build.</p>
          )}
        </li>
        <li className="card flex flex-col p-5">
          <h3 className="text-base font-medium text-paper">Policy list (.csv)</h3>
          <p className="mt-1 flex-1 text-sm text-paper-dim">Every IEA Policies and Measures record with its year, status, type, technology and stage tags, title and IEA link (attribution: IEA).{size('sector-policy')}</p>
          <button type="button" className="btn btn-ghost btn-sm mt-3 self-start" disabled={busy !== null || !manifest.data?.chunks.some((c) => c.name === 'sector-policy')} onClick={policy}>
            {busy === 'policy' ? 'Preparing…' : 'Download the policy list'}
          </button>
        </li>
      </ul>
      {err && <Notice kind="error" title="The download could not be prepared">{err}</Notice>}
      <p className="text-sm text-paper-mute">{RESTRICTED_LINE}</p>
    </div>
  );
}

/** The Strategic data's bundle id: the stamp the Strategic module published, else its manifest (same origin). */
function useStrategicBundleId(): string | null | undefined {
  const info = useBuildInfo();
  const [fetched, setFetched] = useState<string | null | undefined>(undefined);
  useEffect(() => {
    if (info.strategic) return;
    let live = true;
    const base = (import.meta.env?.BASE_URL as string | undefined) ?? '/';
    const origin = typeof window !== 'undefined' ? window.location.origin : '';
    fetch(`${origin}${base}data/manifest.json`, { cache: 'no-cache' })
      .then((r) => (r.ok ? r.json() : null))
      .then((j: unknown) => {
        if (live) setFetched(j && typeof j === 'object' && typeof (j as { bundle_id?: unknown }).bundle_id === 'string' ? (j as { bundle_id: string }).bundle_id : null);
      })
      .catch(() => live && setFetched(null));
    return () => {
      live = false;
    };
  }, [info.strategic]);
  return info.strategic ? info.strategic.bundle_id : fetched;
}

function Build() {
  const manifest = useSectorManifest();
  const strategic = useStrategicBundleId();
  const m = manifest.data;
  if (!m) return null;
  const same = m.strategic_bundle_id && strategic ? m.strategic_bundle_id === strategic : null;
  const pass = m.validation.filter((x) => x.status === 'PASS').length;
  const warns = m.validation.filter((x) => x.status === 'WARN');
  return (
    <div className="grid gap-6 lg:grid-cols-2">
      <section className="card p-5" aria-labelledby="build-h">
        <h3 id="build-h" className="rule-neis-sm mb-3 text-base font-medium text-paper">
          This data build
        </h3>
        <KeyValue
          rows={[
            ['Exported', fmtExportDate(m.created)],
            ['Git commit', m.git_commit ? m.git_commit.slice(0, 10) : '–'],
            ['Sector bundle', m.bundle_id.slice(0, 12)],
            ['Strategic bundle it was built against', m.strategic_bundle_id ? m.strategic_bundle_id.slice(0, 12) : '–'],
            ['Generator', m.generator],
            ['Pipeline marker', m.pipeline_marker ?? '–'],
          ]}
        />
        <div className="mt-3">
          {same === true && <p className="text-sm text-paper-dim">✓ Built from the same pipeline run as the Strategic data.</p>}
          {same === false && <Notice kind="warning">Built from a different run than the Strategic data loaded here ({String(strategic).slice(0, 12)}).</Notice>}
          {same === null && <p className="text-sm text-paper-mute">{strategic === undefined ? 'Checking the Strategic data…' : 'The Strategic data could not be checked.'}</p>}
        </div>
      </section>
      <section className="card p-5" aria-labelledby="valid-h">
        <h3 id="valid-h" className="rule-neis-sm mb-3 text-base font-medium text-paper">
          Validation
        </h3>
        <p className="text-sm text-paper-dim">
          {pass} checks passed{warns.length ? `; ${warns.length} warnings` : ''}.
        </p>
        {warns.length > 0 && (
          <ul className="mt-2 list-disc space-y-1 pl-5 text-sm text-paper-dim">
            {warns.map((w) => (
              <li key={w.check}>
                <span className="text-paper">{w.check}</span> ({w.n}){w.detail ? `: ${w.detail}` : ''}
              </li>
            ))}
          </ul>
        )}
      </section>
      <section className="card p-5 lg:col-span-2" aria-labelledby="inputs-h">
        <h3 id="inputs-h" className="rule-neis-sm mb-3 text-base font-medium text-paper">
          Input files
        </h3>
        <TableView rows={m.sources} caption="Input files" search pageSize={15} rowKey={(s) => s.path} cols={[{ id: 'p', header: 'Path', value: (s) => s.path, minWidth: 260 }, { id: 'm', header: 'Modified', value: (s) => s.mtime }, { id: 'b', header: 'Bytes', value: (s) => s.bytes, num: true }, { id: 'h', header: 'MD5', value: (s) => s.md5.slice(0, 12), title: 'first 12 characters' }]} />
      </section>
    </div>
  );
}

export default function SourcesView() {
  const model = useModel();
  const loc = useLocation();
  useTitle('Sources & data', 'Sector Insights');
  const html = useMemo(() => aboutHtml(model), [model]);
  useScrollToHash(loc.hash, true);
  return (
    <div className="space-y-10">
      <PageHead eyebrow="SECTOR INSIGHTS · SOURCES" title="Sources & data" lede="Where every number comes from, how the export handles it, what each source covers, and the downloads." />
      <Section id="about" title="About Sector Insights">
        {/* the content is this app's own Markdown, filled with counts from the data build */}
        <article className="prose-neis max-w-4xl" dangerouslySetInnerHTML={{ __html: html }} />
      </Section>
      <Section id="dictionary" title="Variable dictionary">
        <Dictionary />
      </Section>
      <Section id="sources" title="Sources">
        <Sources />
      </Section>
      <Section id="coverage" title="Coverage">
        <Coverage />
      </Section>
      <Section id="reconciliation" title="Trade basis">
        <Reconciliation />
      </Section>
      <Section id="downloads" title="Downloads">
        <Downloads />
      </Section>
      <Section id="build" title="Build">
        <Build />
      </Section>
    </div>
  );
}
