// View 8: Data (spec_react.md 8.6). Download the current scenario (a zip of CSVs built in the
// browser, or one table), share or reload a scenario (link, parameter file), the export's validation
// and the worker's self-check, sources and build information.

import { useMemo, useRef, useState } from 'react';
import { useTitle } from '../../App.tsx';
import { TableView } from '../../components/TableView.tsx';
import { Loading, Notice, PageHead, Select, KeyValue } from '../../components/ui.tsx';
import { checkAdvanced, checkDials } from '../../engine/params.ts';
import { ENGINE_VERSION } from '../../engine/version.ts';
import type { Table } from '../../engine/types.ts';
import { tableToCsv, withBom } from '../../lib/csv.ts';
import { downloadBlob, downloadText } from '../../lib/download.ts';
import { DASH, fmtBytes, fmtDate, fmtMs, isNum } from '../../lib/format.ts';
import { useApp, useEngine, useScenario } from '../../state/EngineProvider.tsx';
import { toSpec, writeSearch, VIEW_DEFAULTS } from '../../state/url.ts';
import { DIAL_SPECS } from '../../components/DialPanel.tsx';

const TABLE_LABEL: Record<string, string> = {
  parameters: 'Parameters',
  data_vintage: 'Data vintage',
  strategic_index: 'Strategic Index (with the baseline for comparison)',
  strategic_excluded: 'Strategic Index: excluded sectors',
  allied_selection: 'Allied strategic selection',
  energy_security: 'Energy Security',
  economic_opportunity: 'Economic Opportunity',
  es_contributions: 'Energy Security: what drives it (category contributions)',
  eo_contributions: 'Economic Opportunity: what drives it (category contributions)',
  policy_index: 'Policy activity',
  psi_partner: 'Partnership Strength as a partner',
  psi_dyad: 'Partnership Strength of every pair',
};

type Status = { kind: 'idle' } | { kind: 'busy'; text: string } | { kind: 'error'; text: string } | { kind: 'done'; text: string };

export default function DataView() {
  useTitle('Data');
  const app = useApp();
  const { client, state } = useEngine();
  const { scenario, isBaseline, setScenario, key } = useScenario();
  const [one, setOne] = useState('strategic_index');
  const [status, setStatus] = useState<Status>({ kind: 'idle' });
  const [loadMsg, setLoadMsg] = useState<{ kind: 'info' | 'warning' | 'error'; text: string } | null>(null);
  const [copied, setCopied] = useState(false);
  const fileRef = useRef<HTMLInputElement>(null);
  const m = app.manifest;

  const include: ('core' | 'partner')[] = ['core', 'partner'];
  const link = typeof window !== 'undefined' ? `${window.location.origin}${import.meta.env.BASE_URL.replace(/\/$/, '')}/overview${writeSearch(scenario, VIEW_DEFAULTS, app.urlCtx)}` : '';

  const readme = () =>
    [
      'Allied Industrial Base Explorer: scenario tables',
      '',
      `Methods paper: ${m.paper?.title ?? DASH}`,
      `Scenario: ${isBaseline ? "the baseline (all dials at 50, advanced settings at their defaults)" : DIAL_SPECS.map((d) => `${d.label} ${scenario.dials[d.key]}`).join(', ')}`,
      `Link: ${link}`,
      `Data build: ${m.bundle_id} (exported ${m.created}, git ${m.git_commit ?? DASH})`,
      `Engine: ${ENGINE_VERSION}`,
      '',
      'Every table is a CSV (UTF-8 with a byte-order mark) with the R pipeline\'s column names. Empty cells are NA.',
      'Columns ending in _base hold the baseline value (every dial at 50, the pipeline\'s six-stage defaults); delta = scenario minus baseline; rank_delta = baseline rank minus scenario rank (positive = moved up).',
      'parameters.csv lists every parameter of the scenario; data_vintage.csv the export the numbers come from.',
      'Scenario results are model outputs for exploration, not forecasts.',
      '',
      ...(m.notes ?? []).map((n) => `Note: ${n}`),
    ].join('\r\n');

  const downloadZip = async () => {
    setStatus({ kind: 'busy', text: 'Preparing the tables' });
    try {
      const tables = await client.tables(toSpec(scenario), include);
      const { zipTexts } = await import('../../lib/zip.ts');
      const files: Record<string, string> = { 'README.txt': readme() };
      for (const [name, t] of Object.entries(tables)) files[`${name}.csv`] = withBom(tableToCsv(t));
      const bytes = zipTexts(files);
      downloadBlob(new Blob([bytes as BlobPart], { type: 'application/zip' }), `allied-industrial-base-${isBaseline ? 'neis-baseline' : 'scenario'}.zip`);
      setStatus({ kind: 'done', text: `Downloaded ${Object.keys(tables).length} tables.` });
    } catch (e) {
      setStatus({ kind: 'error', text: (e as Error).message });
    }
  };

  const downloadOne = async () => {
    setStatus({ kind: 'busy', text: 'Preparing the table' });
    try {
      const tables = await client.tables(toSpec(scenario), include);
      const t: Table | undefined = tables[one];
      if (!t) {
        setStatus({ kind: 'error', text: 'This table is not available.' });
        return;
      }
      downloadText(withBom(tableToCsv(t)), `${one}.csv`);
      setStatus({ kind: 'done', text: `Downloaded ${one}.csv (${t.nrow} rows).` });
    } catch (e) {
      setStatus({ kind: 'error', text: (e as Error).message });
    }
  };

  const saveParams = () => {
    const doc = { app: 'allied-industrial-base-explorer', engine_version: ENGINE_VERSION, bundle_id: m.bundle_id, spec: toSpec(scenario) };
    downloadText(JSON.stringify(doc, null, 2), 'allied-industrial-base-scenario.json', 'application/json');
  };

  const loadParams = async (file: File) => {
    try {
      const doc = JSON.parse(await file.text()) as { app?: string; bundle_id?: string; spec?: { dials?: unknown; advanced?: unknown } };
      if (!doc || typeof doc !== 'object' || !doc.spec) throw new Error('That file is not a parameter file from this explorer.');
      const dials = checkDials(doc.spec.dials ?? {});
      for (const d of DIAL_SPECS) if (!Number.isInteger(dials[d.key])) dials[d.key] = Math.round(dials[d.key]);
      // a file saved before 2026-10-17 can carry the settings of the retired allied network design: dropped
      const raw = doc.spec.advanced && typeof doc.spec.advanced === 'object' ? { ...(doc.spec.advanced as Record<string, unknown>) } : null;
      const hadNetwork = !!raw && 'network' in raw;
      if (raw) delete raw.network;
      const advanced = checkAdvanced(raw, app.bundleLike);
      setScenario({ dials, advanced });
      const retired = hadNetwork ? ' Settings this Explorer does not use were ignored.' : '';
      setLoadMsg(
        doc.bundle_id && doc.bundle_id !== m.bundle_id
          ? { kind: 'warning', text: `Scenario loaded. It was saved with another data build (${doc.bundle_id.slice(0, 8)}); the numbers may differ from when it was saved.${retired}` }
          : { kind: hadNetwork ? 'warning' : 'info', text: `Scenario loaded.${retired}` },
      );
    } catch (e) {
      setLoadMsg({ kind: 'error', text: `Could not load the parameters: ${(e as Error).message}` });
    }
  };

  const validation = useMemo(
    () =>
      (m.validation ?? []).map((v) => ({
        check: String(v.check ?? ''),
        layer: String(v.layer ?? ''),
        status: String(v.status ?? ''),
        n: isNum(v.n) ? (v.n as number) : null,
        max_abs_diff: isNum(v.max_abs_diff) ? (v.max_abs_diff as number) : null,
        note: typeof v.note === 'string' ? v.note : null,
      })),
    [m.validation],
  );
  const statusCount = validation.reduce<Record<string, number>>((a, v) => ((a[v.status] = (a[v.status] ?? 0) + 1), a), {});
  const sources = useMemo(
    () => (m.sources ?? []).map((s) => ({ path: String(s.path ?? ''), mtime: typeof s.mtime === 'string' ? s.mtime : null, size: isNum(s.size) ? (s.size as number) : null, md5: typeof s.md5 === 'string' ? s.md5 : null })),
    [m.sources],
  );
  const budget = (m.budget ?? {}) as Record<string, unknown>;
  // the per-sector partner lists (pcell-<technology>-<stage>: one per technology and stage, 66 at six stages)
  // are read on the main thread by the partner lists that need them, never by the engine: one line for all
  const pcells = m.chunks.filter((c) => c.name.startsWith('pcell-'));
  const sumOf = (k: 'bytes' | 'bytes_gz') => pcells.reduce((s, c) => s + (isNum(c[k]) ? (c[k] as number) : 0), 0);
  const tableOptions = Object.entries(TABLE_LABEL).map(([value, label]) => ({ value, label }));

  return (
    <div>
      <PageHead
        eyebrow="About · Data & sources"
        title="Data"
        lede="Download the numbers behind the index and partnership views, for the baseline or a custom perspective, and see how this export was checked. The sector, country and Allied Industrial Base data have their own source pages, listed under About."
      />
      <div className="flex flex-col gap-6">
        <div className="grid gap-6 xl:grid-cols-2">
          <section className="card p-5">
            <h2 className="rule-neis-sm text-base font-medium text-paper lg:text-lg">Download the current scenario</h2>
            <p className="mt-1 text-sm text-paper-mute">
              Every table of the scenario as CSV files in one zip: parameters and data vintage, the Strategic Index, pillars and their contributions, Policy and partnership tables, with the baseline values for comparison.
            </p>
            <div className="mt-4 flex flex-wrap items-center gap-3">
              <button type="button" className="btn btn-primary" onClick={downloadZip} disabled={status.kind === 'busy'}>
                All tables (.zip)
              </button>
            </div>
            <div className="mt-5 flex flex-wrap items-end gap-3">
              <Select label="One table" value={one} options={tableOptions} onChange={setOne} className="min-w-[16rem]" />
              <button type="button" className="btn btn-ghost" onClick={downloadOne} disabled={status.kind === 'busy'}>
                Download CSV
              </button>
            </div>
            <div className="mt-3 text-sm" role="status" aria-live="polite">
              {status.kind === 'busy' && <Loading label={status.text} />}
              {status.kind === 'done' && <span className="text-paper-dim">{status.text}</span>}
              {status.kind === 'error' && <span className="text-alert">{status.text}</span>}
            </div>
          </section>

          <section className="card p-5">
            <h2 className="rule-neis-sm text-base font-medium text-paper lg:text-lg">Share or reload a scenario</h2>
            <p className="mt-1 text-sm text-paper-mute">The page address carries the scenario; anyone opening the link sees the same numbers (with the same data build).</p>
            <label htmlFor="share-link" className="label mt-4">
              Link to this scenario
            </label>
            <div className="flex gap-2">
              <input id="share-link" className="field field-sm" readOnly value={link} onFocus={(e) => e.currentTarget.select()} />
              <button
                type="button"
                className="btn btn-ghost btn-sm"
                onClick={() => {
                  void navigator.clipboard?.writeText(link).then(() => {
                    setCopied(true);
                    setTimeout(() => setCopied(false), 2000);
                  });
                }}
              >
                {copied ? 'Copied' : 'Copy'}
              </button>
            </div>
            <div className="mt-5 flex flex-wrap items-center gap-3">
              <button type="button" className="btn btn-ghost" onClick={saveParams}>
                Save parameters (.json)
              </button>
              <input
                ref={fileRef}
                type="file"
                accept=".json,application/json"
                className="sr-only"
                id="load-params"
                tabIndex={-1}
                aria-hidden="true"
                onChange={(e) => {
                  const f = e.target.files?.[0];
                  if (f) void loadParams(f);
                  e.target.value = '';
                }}
              />
              <button type="button" className="btn btn-ghost" onClick={() => fileRef.current?.click()}>
                Load parameters
              </button>
            </div>
            {loadMsg && (
              <div className="mt-3">
                <Notice kind={loadMsg.kind} onDismiss={() => setLoadMsg(null)}>
                  {loadMsg.text}
                </Notice>
              </div>
            )}
            <p className="mt-3 text-xs text-paper-mute tabular">Scenario key: {key || '(the baseline)'}</p>
          </section>
        </div>

        <section className="card p-5">
          <h2 className="rule-neis-sm text-base font-medium text-paper lg:text-lg">Validation of this export</h2>
          <p className="mt-1 text-sm text-paper-mute">
            At the baseline (every dial at 50, the default settings) the engine must reproduce the pipeline. The export script checks every layer and records the result here
            ({Object.entries(statusCount).map(([k, v]) => `${v} ${k}`).join(', ') || 'no checks'}).
          </p>
          <div className="mt-4">
            <TableView
              rows={validation}
              caption="Export validation"
              search
              pageSize={20}
              cols={[
                { id: 'status', header: 'Status', value: (v) => v.status, cell: (v) => <span className={v.status === 'FAIL' ? 'tag border-alert text-alert' : 'tag'}>{v.status}</span> },
                { id: 'layer', header: 'Layer', value: (v) => v.layer },
                { id: 'check', header: 'Check', value: (v) => v.check, minWidth: 260 },
                { id: 'n', header: 'N', value: (v) => v.n, num: true },
                { id: 'max', header: 'Max |diff|', value: (v) => v.max_abs_diff, cell: (v) => (v.max_abs_diff === null ? DASH : v.max_abs_diff.toExponential(1)), num: true },
                { id: 'note', header: 'Note', value: (v) => v.note, minWidth: 200 },
              ]}
            />
          </div>
          <h3 className="mt-6 text-sm font-medium text-paper">Self-check in this browser</h3>
          <p className="mt-1 text-sm text-paper-mute">After loading, the engine recomputes the baseline and compares it with the exported values (core layers at once, partnerships when their data load).</p>
          <ul className="mt-2 space-y-1 text-sm text-paper-dim">
            {(['core', 'partner'] as const).map((l) => {
              const s = state.selfcheck[l];
              return (
                <li key={l} className="flex flex-wrap items-center gap-2">
                  <span className={s ? (s.ok ? 'tag' : 'tag border-alert text-alert') : 'tag text-paper-mute'}>{s ? (s.ok ? 'PASS' : 'FAIL') : 'pending'}</span>
                  <span className="capitalize">{l}</span>
                  {s && (
                    <span className="text-paper-mute tabular">
                      {s.n} values, max |diff| {isNum(s.max_abs_diff) ? s.max_abs_diff.toExponential(1) : DASH}
                      {s.detail ? `; ${s.detail}` : ''}
                    </span>
                  )}
                  {!s && l !== 'core' && <span className="text-paper-mute">runs when the partnership data are first used</span>}
                </li>
              );
            })}
          </ul>
        </section>

        <div className="grid gap-6 2xl:grid-cols-2">
          <section className="card p-5">
            <h2 className="rule-neis-sm text-base font-medium text-paper lg:text-lg">Build information</h2>
            <div className="mt-3">
              <KeyValue
                rows={[
                  ['Data build', m.bundle_id],
                  ['Exported', fmtDate(m.created)],
                  ['Git commit', m.git_commit ?? DASH],
                  ['Branch', m.osi_branch ?? DASH],
                  ['Engine', `${state.engine ?? ENGINE_VERSION}; ready in ${fmtMs(state.readyMs)}`],
                  ['Initial load (gzip)', fmtBytes(budget.initial_gz)],
                  ['All data (gzip)', fmtBytes(budget.all_gz)],
                  ['Lossy encoding', (m.encoding as { lossy?: boolean } | undefined)?.lossy ? 'yes' : 'no (every value is exact)'],
                ]}
              />
            </div>
            <h3 className="mt-5 text-sm font-medium text-paper">Data chunks</h3>
            <ul className="mt-2 space-y-1 text-sm text-paper-dim tabular">
              {m.chunks.filter((c) => !c.name.startsWith('pcell-')).map((c) => (
                <li key={c.name}>
                  {c.name}: {fmtBytes(c.bytes_gz)} compressed ({fmtBytes(c.bytes)}), {c.initial ? 'loaded at start' : 'loaded on first use'}; {state.chunks[c.name as keyof typeof state.chunks]?.state ?? 'idle'}
                </li>
              ))}
              {pcells.length > 0 && (
                <li key="pcell" data-testid="pcell-chunks">
                  pcell-&lt;technology&gt;-&lt;stage&gt;: {pcells.length} sector partner lists (one per technology and supply-chain stage), {fmtBytes(sumOf('bytes_gz'))} compressed in all ({fmtBytes(sumOf('bytes'))}),
                  each read when a partner list needs it
                </li>
              )}
            </ul>
          </section>
          <section className="card p-5">
            <h2 className="rule-neis-sm text-base font-medium text-paper lg:text-lg">Sources</h2>
            <p className="mt-1 text-sm text-paper-mute">The pipeline files this export was built from.</p>
            <div className="mt-3">
              <TableView
                rows={sources}
                caption="Sources"
                search
                pageSize={10}
                cols={[
                  { id: 'path', header: 'File', value: (s) => s.path, minWidth: 220 },
                  { id: 'mtime', header: 'Modified', value: (s) => s.mtime, cell: (s) => fmtDate(s.mtime) },
                  { id: 'size', header: 'Size', value: (s) => s.size, cell: (s) => fmtBytes(s.size), num: true },
                ]}
              />
            </div>
          </section>
        </div>
      </div>
    </div>
  );
}
