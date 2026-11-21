// /review/admin: the internal QC dashboard (docs/QC_SYSTEM_DESIGN.md 6.6). It runs in the admin's browser
// only: submission files and packets are read with FileReader, the synthetic demo fixtures can be loaded
// instead (and are labelled so), and adjudications live in this browser's review store (storage.ts). Nothing
// is uploaded. Tabs: Overview, Agreement (anchor cases first: the blind distribution, the modal answer, the
// shares matching the Explorer, persuaded and rejecting, the reasons; "too few reviews to summarise" below
// three), Disagreement taxonomy, Data QC (qc_summary.json and the open data-issue reports), Robustness
// (robustness.json class counts by technology x stage, and the drivers) and Priority fixes (materiality x
// confidence the issue is real x breadth, the three factors shown, an adjudication editor per issue). Every
// number comes from review/aggregate.ts buildDashboard; this file lays it out. Exports: the CSVs of
// review/exports.ts (a zip) and the adjudications JSON. Reviewer names are never shown unless the admin ticks
// "show names", and never for an anonymous reviewer.

import { useEffect, useMemo, useState, type ReactNode } from 'react';
import { Checkbox, EmptyState, Loading, Notice, StatTile, Tag } from '../../components/ui.tsx';
import { useTitle } from '../../shell/title.ts';
import { useOptionalReviewCtx } from '../ReviewLayout.tsx';
import { downloadBlob, downloadText } from '../../lib/download.ts';
import { buildDashboard, breadthOf, MIN_REVIEWS_TO_SUMMARISE, TOO_FEW_LABEL, type CaseAgreement, type Dashboard, type PriorityItem } from '../aggregate.ts';
import { csvFiles } from '../exports.ts';
import { validateSubmission } from '../manifest.ts';
import { exportableName, pseudonymReviewerCode } from '../privacy.ts';
import { ISSUE_TYPE_LABEL, SEVERITY_LABEL, type Adjudication, type AdjudicationStatus, type CasesFile, type FrameworkScenario, type IssueType, type MethodCard, type ReviewCase, type RobustnessClass, type Submission } from '../model.ts';
import { reviewStorage, type ReviewStorage } from '../storage.ts';
import { campaignOf, pickCampaign, reviewStore, type ReviewCampaignEntry, type ReviewManifestDoc } from '../data/store.ts';
import type { RobustnessMap } from '../qc/reports.ts';
import { Field, TextArea } from '../components/form.tsx';

// ---- pure helpers (exported for tests) ----------------------------------------------------------------------

/** The submissions in a loaded JSON document: a packet, a list, one submission or a demo document. Invalid ones are reported, not kept. */
export function submissionsFromJson(x: unknown, file = 'file'): { submissions: Submission[]; errors: string[]; synthetic: boolean } {
  const isObj = (v: unknown): v is Record<string, unknown> => typeof v === 'object' && v !== null && !Array.isArray(v);
  let list: unknown[];
  let synthetic = false;
  if (Array.isArray(x)) list = x;
  else if (isObj(x) && Array.isArray(x.submissions)) {
    list = x.submissions;
    synthetic = x.synthetic === true;
  } else if (isObj(x) && typeof x.submission_id === 'string') list = [x];
  else return { submissions: [], errors: [`${file}: not a submission, a packet or a list of submissions`], synthetic: false };
  const submissions: Submission[] = [];
  const errors: string[] = [];
  list.forEach((s, i) => {
    const v = validateSubmission(s);
    if (v.ok) submissions.push(s as Submission);
    else errors.push(`${file} #${i + 1}: ${v.errors.slice(0, 2).join('; ')}`);
  });
  return { submissions, errors, synthetic };
}

export const ROBUSTNESS_CLASSES: readonly RobustnessClass[] = ['stable', 'sensitive', 'fragile', 'not_testable'];
export const ROBUSTNESS_LABEL: Readonly<Record<RobustnessClass, string>> = { stable: 'Stable', sensitive: 'Sensitive', fragile: 'Fragile', not_testable: 'Not testable' };

export interface RobustnessGrid {
  techs: string[];
  stages: string[];
  /** counts[tech][stage][class] over every economy's cell */
  counts: Record<string, Record<string, Record<RobustnessClass, number>>>;
}

/** Robustness classes counted by technology x stage from robustness.json's per-economy code strings. */
export function robustnessGrid(map: Pick<RobustnessMap, 'codes' | 'posture'>): RobustnessGrid | null {
  const p = map.posture;
  if (!p) return null;
  const counts: RobustnessGrid['counts'] = {};
  for (const t of p.techs) {
    counts[t] = {};
    for (const s of p.stages) counts[t][s] = { stable: 0, sensitive: 0, fragile: 0, not_testable: 0 };
  }
  for (const code of Object.values(p.cells)) {
    for (let i = 0; i < code.length; i++) {
      const cls = map.codes[code[i]];
      const t = p.techs[Math.floor(i / p.stages.length)];
      const s = p.stages[i % p.stages.length];
      if (cls && t && s) counts[t][s][cls] += 1;
    }
  }
  return { techs: [...p.techs], stages: [...p.stages], counts };
}

const pct = (x: number | null): string => (x === null ? '–' : `${Math.round(x * 100)}%`);
const n2 = (x: number): string => (Math.round(x * 100) / 100).toFixed(2);

// ---- data ------------------------------------------------------------------------------------------------------

interface QcCheck {
  check_id: string;
  area: string;
  status: 'pass' | 'warn' | 'fail' | 'info';
  n: number;
  detail: string;
  examples: string[];
}
interface QcSummaryLike {
  data_release?: string;
  built_at?: string;
  status?: Partial<Record<'pass' | 'warn' | 'fail' | 'info', number>>;
  checks?: QcCheck[];
  reproduction?: { cells: number; reproduced: number; rate: number; exceptions_total?: number } | null;
  robustness?: { posture?: { by_class?: Record<string, number>; by_driver?: Record<string, number>; fence_change?: number } | null; aib?: { by_class?: Record<string, number>; by_driver?: Record<string, number> } | null } | null;
  golden?: { summary?: Record<string, number>; results?: { id: string; status: string; failures: string[] }[] } | null;
}

interface Loaded {
  manifest: ReviewManifestDoc | null;
  entry: ReviewCampaignEntry | null;
  cases: CasesFile | null;
  cards: MethodCard[];
  scenarios: FrameworkScenario[];
  qc: QcSummaryLike | null;
  robustness: RobustnessMap | null;
  errors: string[];
}

async function loadAll(): Promise<Loaded> {
  const rs = reviewStore();
  const errors: string[] = [];
  const settle = async <T,>(p: Promise<T>, what: string): Promise<T | null> => {
    try {
      return await p;
    } catch (e) {
      errors.push(`${what}: ${String((e as Error)?.message ?? e)}`);
      return null;
    }
  };
  const manifest = await settle(rs.manifest(), 'manifest');
  const entry = manifest ? pickCampaign(manifest) : null;
  const [cases, cards, scen, qc, rob] = await Promise.all([
    entry ? settle(rs.cases(entry), 'cases') : Promise.resolve(null),
    settle(rs.methodCards(), 'method cards'),
    settle(rs.frameworkScenarios(), 'framework scenarios'),
    settle(rs.qcSummary(), 'QC summary'),
    settle(rs.robustness(), 'robustness'),
  ]);
  return { manifest, entry, cases, cards: cards?.cards ?? [], scenarios: scen?.scenarios ?? [], qc: (qc as QcSummaryLike) ?? null, robustness: (rob as RobustnessMap) ?? null, errors };
}

const TABS = [
  ['overview', 'Overview'],
  ['agreement', 'Agreement'],
  ['taxonomy', 'Disagreement taxonomy'],
  ['dataqc', 'Data QC'],
  ['robustness', 'Robustness'],
  ['priority', 'Priority fixes'],
] as const;
type TabId = (typeof TABS)[number][0];

interface LoadedSet {
  name: string;
  n: number;
  synthetic: boolean;
}

/** The page's own heading under the review layout's title (the layout holds the h1). */
function ViewHead({ eyebrow, title, lede, children }: { eyebrow: string; title: string; lede: string; children?: ReactNode }) {
  return (
    <header className="mb-5">
      <p className="eyebrow">{eyebrow}</p>
      <h2 className="mt-1 text-xl font-medium text-paper">{title}</h2>
      <p className="mt-2 max-w-3xl text-sm leading-relaxed text-paper-dim">{lede}</p>
      {children}
    </header>
  );
}

export default function AdminDashboardView({ storage }: { storage?: ReviewStorage } = {}) {
  useTitle('QC dashboard', 'Expert review');
  const layout = useOptionalReviewCtx();
  const store = storage ?? layout?.storage ?? reviewStorage();
  const [data, setData] = useState<Loaded | null>(null);
  const [subs, setSubs] = useState<Submission[]>([]);
  const [sets, setSets] = useState<LoadedSet[]>([]);
  const [fileErrors, setFileErrors] = useState<string[]>([]);
  const [adjs, setAdjs] = useState<Adjudication[]>(() => store.listAdjudications());
  const [showNames, setShowNames] = useState(false);
  const [tab, setTab] = useState<TabId>('overview');
  const [decidedBy, setDecidedBy] = useState('NEIS');

  useEffect(() => {
    let live = true;
    loadAll().then((d) => live && setData(d));
    return () => {
      live = false;
    };
  }, []);

  const add = (name: string, got: Submission[], synthetic: boolean, errors: string[] = []) => {
    setSubs((prev) => {
      const ids = new Set(prev.map((s) => s.submission_id));
      return [...prev, ...got.filter((s) => !ids.has(s.submission_id))];
    });
    setSets((prev) => [...prev, { name, n: got.length, synthetic }]);
    if (errors.length) setFileErrors((prev) => [...prev, ...errors]);
  };

  const onFiles = (files: FileList | null) => {
    for (const f of [...(files ?? [])]) {
      const r = new FileReader();
      r.onload = () => {
        try {
          const got = submissionsFromJson(JSON.parse(String(r.result)), f.name);
          add(f.name, got.submissions, got.synthetic, got.errors);
        } catch {
          setFileErrors((prev) => [...prev, `${f.name}: not a JSON file`]);
        }
      };
      r.onerror = () => setFileErrors((prev) => [...prev, `${f.name}: could not be read`]);
      r.readAsText(f);
    }
  };

  const loadDemo = async () => {
    try {
      const d = await reviewStore().demoSubmissions();
      const got = submissionsFromJson(d, 'demo fixtures');
      add('Demo fixtures (synthetic)', got.submissions, true, got.errors);
    } catch (e) {
      setFileErrors((prev) => [...prev, `demo fixtures: ${String((e as Error)?.message ?? e)}`]);
    }
  };

  const campaign = useMemo(() => (data?.entry ? campaignOf(data.entry, data.cases).campaign : null), [data]);
  const cases: ReviewCase[] = useMemo(() => data?.cases?.cases ?? [], [data]);
  const dash = useMemo(
    () => (data ? buildDashboard({ submissions: subs, cases, campaign, adjudications: adjs, cards: data.cards, scenarios: data.scenarios }) : null),
    [data, subs, cases, campaign, adjs],
  );
  const synthetic = sets.some((s) => s.synthetic);

  const saveAdj = (a: Adjudication) => {
    store.saveAdjudication(a);
    setAdjs(store.listAdjudications());
  };

  const exportCsv = async () => {
    const files = csvFiles(subs, cases, adjs, { includeNames: showNames, cards: data?.cards, scenarios: data?.scenarios });
    const { zipTexts } = await import('../../lib/zip.ts');
    const bytes = zipTexts(Object.fromEntries(files.map((f) => [f.name, f.text])));
    downloadBlob(new Blob([bytes as BlobPart], { type: 'application/zip' }), `aib-review-exports${synthetic ? '-SYNTHETIC' : ''}.zip`);
  };

  return (
    <div>
      <ViewHead eyebrow="Internal" title="QC dashboard" lede="Expert review submissions, the automated checks of the shipped data, and the fixes they point to, ranked." />
      <Notice kind="warning" title="Runs in this browser only: files you load are not uploaded.">
        Submissions, packets and adjudications stay on this computer. Close the tab and the loaded files are gone; adjudications are kept in this browser until you export them.
      </Notice>
      <section className="panel mt-4 p-4" aria-label="Load submissions">
        <div className="flex flex-wrap items-end gap-4">
          <Field label="Submission files or packets (JSON)">
            {({ id }) => <input id={id} type="file" accept=".json,application/json" multiple className="text-sm text-paper-dim" onChange={(e) => onFiles(e.target.files)} data-testid="admin-files" />}
          </Field>
          <button type="button" className="btn btn-ghost btn-sm mb-4" onClick={() => void loadDemo()}>
            Load the demo data (synthetic)
          </button>
          <button type="button" className="btn btn-ghost btn-sm mb-4" onClick={() => add('This browser', store.listSubmissions(), false)}>
            Add this browser&apos;s own submissions
          </button>
          {subs.length > 0 && (
            <button
              type="button"
              className="btn btn-quiet btn-sm mb-4"
              onClick={() => {
                setSubs([]);
                setSets([]);
                setFileErrors([]);
              }}
            >
              Clear the loaded submissions
            </button>
          )}
        </div>
        {sets.length > 0 && (
          <p className="text-xs text-paper-mute" data-testid="admin-sets">
            Loaded: {sets.map((s) => `${s.name} (${s.n})`).join(' · ')} · {subs.length} distinct submissions
          </p>
        )}
        {synthetic && (
          <p className="mt-2 text-sm text-signal" data-testid="admin-synthetic">
            Synthetic demo data are loaded: nothing here is a real reviewer&apos;s answer.
          </p>
        )}
        {fileErrors.length > 0 && (
          <details className="mt-2 text-xs text-paper-mute">
            <summary className="cursor-pointer text-alert">{fileErrors.length} problems reading the files</summary>
            <ul className="mt-1 list-disc pl-5">
              {fileErrors.slice(0, 50).map((e, i) => (
                <li key={i}>{e}</li>
              ))}
            </ul>
          </details>
        )}
        <div className="mt-3 flex flex-wrap items-center gap-4 border-t hairline pt-3">
          <Checkbox label="Show names (never for anonymous reviewers)" checked={showNames} onChange={setShowNames} />
          <button type="button" className="btn btn-ghost btn-sm" disabled={!subs.length} onClick={() => void exportCsv()}>
            Download the CSV exports (zip)
          </button>
          <button type="button" className="btn btn-ghost btn-sm" disabled={!adjs.length} onClick={() => downloadText(JSON.stringify({ schema: 'aib-review-adjudications', schema_version: 1, adjudications: adjs }, null, 2), 'aib-review-adjudications.json', 'application/json')}>
            Download the adjudications (JSON)
          </button>
        </div>
      </section>

      {!data || !dash ? (
        <Loading label="Loading the review data…" />
      ) : (
        <>
          {data.errors.length > 0 && (
            <div className="mt-4">
              <Notice kind="warning">Some review files could not be loaded: {data.errors.join('; ')}</Notice>
            </div>
          )}
          <div className="mt-6 flex flex-wrap gap-1 border-b hairline" role="tablist" aria-label="Dashboard views">
            {TABS.map(([id, label]) => (
              <button key={id} type="button" role="tab" id={`tab-${id}`} aria-selected={tab === id} aria-controls={`panel-${id}`} className={`btn btn-sm ${tab === id ? 'btn-primary' : 'btn-quiet'}`} onClick={() => setTab(id)}>
                {label}
              </button>
            ))}
          </div>
          <div role="tabpanel" id={`panel-${tab}`} aria-labelledby={`tab-${tab}`} className="mt-5">
            {tab === 'overview' && <OverviewTab dash={dash} subs={subs} showNames={showNames} campaignName={campaign?.name ?? null} />}
            {tab === 'agreement' && <AgreementTab dash={dash} cases={cases} />}
            {tab === 'taxonomy' && <TaxonomyTab dash={dash} />}
            {tab === 'dataqc' && <DataQcTab qc={data.qc} dash={dash} />}
            {tab === 'robustness' && <RobustnessTab map={data.robustness} qc={data.qc} dash={dash} />}
            {tab === 'priority' && <PriorityTab dash={dash} subs={subs} showNames={showNames} decidedBy={decidedBy} onDecidedBy={setDecidedBy} onSave={saveAdj} />}
          </div>
        </>
      )}
    </div>
  );
}

// ---- tabs --------------------------------------------------------------------------------------------------------

function NoData({ children }: { children: ReactNode }) {
  return <EmptyState>{children}</EmptyState>;
}

function reviewerLabel(s: Submission, showNames: boolean): string {
  const name = exportableName(s, { includeNames: showNames });
  const code = pseudonymReviewerCode(s.reviewer.reviewer_id);
  return name ? `${name} (${code})` : code;
}

function OverviewTab({ dash, subs, showNames, campaignName }: { dash: Dashboard; subs: Submission[]; showNames: boolean; campaignName: string | null }) {
  const o = dash.overview;
  const reviewers = [...new Map(subs.map((s) => [s.reviewer.reviewer_id, s])).values()];
  return (
    <div data-testid="admin-overview">
      {campaignName && <p className="mb-3 text-sm text-paper-mute">Campaign: {campaignName}</p>}
      <div className="grid gap-3 sm:grid-cols-2 lg:grid-cols-4">
        <StatTile label="Submissions" value={o.n_submissions} sub={`${o.n_reviewers} reviewers`} />
        <StatTile label="Cases reviewed" value={`${o.n_cases_reviewed} of ${o.n_cases_in_scope}`} sub={`${o.n_case_reviews} case reviews · ${o.n_outside_answers} outside expertise`} />
        <StatTile label="Anchor cases at target" value={`${o.n_anchor_met} of ${o.n_anchor_cases}`} />
        <StatTile label="Unresolved critical issues" value={o.n_unresolved_critical} sub={`${o.n_open_data_reports} open data reports of ${o.n_data_reports}`} />
      </div>
      <dl className="mt-4 space-y-1 text-sm">
        <div>
          <dt className="inline text-paper-mute">Sectors covered: </dt>
          <dd className="inline text-paper-dim">{o.sectors_covered.join(', ') || 'none yet'}</dd>
        </div>
        <div>
          <dt className="inline text-paper-mute">Countries covered: </dt>
          <dd className="inline text-paper-dim">{o.countries_covered.join(', ') || 'none yet'}</dd>
        </div>
        <div>
          <dt className="inline text-paper-mute">Data releases reviewed: </dt>
          <dd className="inline text-paper-dim">
            {o.data_releases.join(', ') || '–'}
            {o.data_releases.length > 1 && <span className="text-signal"> (more than one: agreement mixes releases)</span>}
            {o.n_snapshot_mismatch > 0 && <span className="text-signal"> · {o.n_snapshot_mismatch} reviews of an older case snapshot</span>}
          </dd>
        </div>
      </dl>
      {dash.anchors.length > 0 && (
        <div className="mt-4 overflow-x-auto">
        <table className="data-table" data-testid="admin-anchors">
          <caption className="label text-left">Anchor coverage</caption>
          <thead>
            <tr>
              <th>Case</th>
              <th>Reviews</th>
              <th>Usable</th>
              <th>Target</th>
            </tr>
          </thead>
          <tbody>
            {dash.anchors.map((a) => (
              <tr key={a.case_id}>
                <td>
                  {a.case_id} {a.title}
                </td>
                <td className="tabular">{a.n_reviews}</td>
                <td className="tabular">{a.n_interpretable}</td>
                <td className="tabular">
                  {a.target}
                  {a.met ? ' · met' : ` · ${a.remaining} to go`}
                </td>
              </tr>
            ))}
          </tbody>
        </table>
        </div>
      )}
      {reviewers.length > 0 && (
        <details className="disclosure mt-4 border-t hairline">
          <summary className="py-2 text-sm text-paper-dim">Reviewers ({reviewers.length})</summary>
          <ul className="pb-2 pl-4 text-sm text-paper-dim" data-testid="admin-reviewers">
            {reviewers.map((s) => (
              <li key={s.reviewer.reviewer_id}>{reviewerLabel(s, showNames)}</li>
            ))}
          </ul>
        </details>
      )}
    </div>
  );
}

function optionLabel(c: ReviewCase | undefined, v: string): string {
  return c?.answer_options?.find((o) => o.value === v)?.label ?? v;
}

function AgreementCard({ a, c }: { a: CaseAgreement; c: ReviewCase | undefined }) {
  const total = Object.values(a.blind_distribution).reduce((x, y) => x + y, 0);
  return (
    <article className="panel p-4" data-testid="agreement-case" data-case={a.case_id} data-too-few={a.too_few || undefined}>
      <div className="flex flex-wrap items-center gap-2">
        <span className="eyebrow">{a.case_id}</span>
        {a.anchor && <Tag accent>Anchor</Tag>}
      </div>
      <h3 className="mt-1 text-base font-medium text-paper">{a.title ?? a.case_id}</h3>
      <p className="mt-1 text-sm text-paper-dim">
        Explorer: <strong className="text-paper">{a.model_label ?? a.model_choice ?? '–'}</strong> · {a.n_reviews} reviews ({a.n_outside} outside expertise or don&apos;t know)
      </p>
      {a.too_few ? (
        <p className="mt-2 text-sm text-paper-mute" data-testid="too-few">
          {TOO_FEW_LABEL} ({a.n_interpretable} usable; {MIN_REVIEWS_TO_SUMMARISE} needed).
        </p>
      ) : (
        <>
          <ul className="mt-2 space-y-1 text-sm" aria-label="Blind answers">
            {Object.entries(a.blind_distribution)
              .sort((x, y) => y[1] - x[1])
              .map(([k, n]) => (
                <li key={k} className="flex items-center gap-2">
                  <span className="w-32 shrink-0 truncate text-paper-dim sm:w-44" title={optionLabel(c, k)}>
                    {optionLabel(c, k)}
                    {k === a.model_choice ? ' (Explorer)' : ''}
                  </span>
                  <span className="min-w-0 flex-1" aria-hidden="true">
                    <span className="block h-2 bg-signal" style={{ width: `${(100 * n) / Math.max(1, total)}%` }} />
                  </span>
                  <span className="tabular text-paper-mute">{n}</span>
                </li>
              ))}
          </ul>
          <dl className="mt-2 grid grid-cols-2 gap-x-4 gap-y-1 text-sm sm:grid-cols-4">
            <div>
              <dt className="text-xs text-paper-mute">Modal blind answer</dt>
              <dd className="text-paper-dim">{a.modal.map((m) => optionLabel(c, m)).join(' / ') || '–'}</dd>
            </div>
            <div>
              <dt className="text-xs text-paper-mute">Matching the Explorer (blind)</dt>
              <dd className="tabular text-paper-dim" data-testid="share-matching">
                {pct(a.share_matching_model)}
              </dd>
            </div>
            <div>
              <dt className="text-xs text-paper-mute">Persuaded after the reveal</dt>
              <dd className="tabular text-paper-dim">{pct(a.share_persuaded)}</dd>
            </div>
            <div>
              <dt className="text-xs text-paper-mute">Rejecting (data, method or context)</dt>
              <dd className="tabular text-paper-dim">{pct(a.share_rejecting)}</dd>
            </div>
          </dl>
        </>
      )}
      {Object.keys(a.issue_types).length > 0 && (
        <p className="mt-2 text-xs text-paper-mute">
          Reasons:{' '}
          {Object.entries(a.issue_types)
            .map(([t, n]) => `${ISSUE_TYPE_LABEL[t as IssueType] ?? t} ${n}`)
            .join(' · ')}
        </p>
      )}
    </article>
  );
}

function AgreementTab({ dash, cases }: { dash: Dashboard; cases: ReviewCase[] }) {
  const byId = new Map(cases.map((c) => [c.case_id, c]));
  const rows = dash.agreement.filter((a) => a.anchor || a.n_reviews > 0).sort((x, y) => Number(y.anchor) - Number(x.anchor));
  const t = dash.transitions.counts;
  return (
    <div data-testid="admin-agreement">
      <p className="mb-4 max-w-3xl text-sm text-paper-dim" data-testid="elicitation-note">
        These are the answers of the experts who took part: structured expert elicitation, not representative polling. Shares are descriptive; no significance test or agreement
        statistic is implied, and a case with fewer than {MIN_REVIEWS_TO_SUMMARISE} usable reviews is not summarised.
      </p>
      {rows.length === 0 ? (
        <NoData>No case reviews loaded.</NoData>
      ) : (
        <div className="grid gap-3 lg:grid-cols-2">
          {rows.map((a) => (
            <AgreementCard key={a.case_id} a={a} c={byId.get(a.case_id)} />
          ))}
        </div>
      )}
      {dash.overview.n_case_reviews > 0 && (
        <table className="data-table mt-6">
          <caption className="label text-left">Blind answer and verdict after the reveal</caption>
          <tbody>
            <tr>
              <td>Agreed blind, confirmed</td>
              <td className="tabular">{t.agreed_confirmed}</td>
            </tr>
            <tr>
              <td>Agreed blind, now doubtful or rejecting</td>
              <td className="tabular">{t.agreed_doubtful + t.agreed_rejecting}</td>
            </tr>
            <tr>
              <td>Differed blind, persuaded</td>
              <td className="tabular">{t.differed_persuaded}</td>
            </tr>
            <tr>
              <td>Differed blind, not persuaded or rejecting</td>
              <td className="tabular">{t.differed_unconvinced + t.differed_rejecting}</td>
            </tr>
            <tr>
              <td>Outside expertise</td>
              <td className="tabular">{t.outside}</td>
            </tr>
            <tr>
              <td>Verdict contradicts the reviewer&apos;s own blind answer</td>
              <td className="tabular">{t.inconsistent}</td>
            </tr>
          </tbody>
        </table>
      )}
    </div>
  );
}

function TaxonomyTab({ dash }: { dash: Dashboard }) {
  const rows = dash.taxonomy.filter((r) => r.total > 0);
  if (!rows.length) return <NoData>No disagreement recorded yet.</NoData>;
  return (
    <div className="overflow-x-auto">
    <table className="data-table" data-testid="admin-taxonomy">
      <thead>
        <tr>
          <th>Kind of disagreement</th>
          <th>Case reviews</th>
          <th>Framework scenarios</th>
          <th>Decision use</th>
          <th>Total</th>
        </tr>
      </thead>
      <tbody>
        {rows.map((r) => (
          <tr key={r.issue_type}>
            <td>{r.label}</td>
            <td className="tabular">{r.from_case_reviews}</td>
            <td className="tabular">{r.from_framework}</td>
            <td className="tabular">{r.from_decision_use}</td>
            <td className="tabular">{r.total}</td>
          </tr>
        ))}
      </tbody>
    </table>
    </div>
  );
}

function CheckList({ checks, empty }: { checks: QcCheck[]; empty: string }) {
  if (!checks.length) return <p className="text-sm text-paper-mute">{empty}</p>;
  return (
    <ul className="space-y-2">
      {checks.map((c) => (
        <li key={c.check_id} className="panel px-3 py-2 text-sm" data-check={c.check_id} data-status={c.status}>
          <span className="eyebrow mr-2">{c.check_id}</span>
          <span className="text-paper-dim">{c.detail}</span>
          {c.examples.length > 0 && <span className="block text-xs text-paper-mute">e.g. {c.examples.slice(0, 3).join(', ')}</span>}
        </li>
      ))}
    </ul>
  );
}

function DataQcTab({ qc, dash }: { qc: QcSummaryLike | null; dash: Dashboard }) {
  const open = dash.priorities.filter((p) => (p.source === 'data_issue' || p.source === 'break_model') && p.unresolved);
  const checks = qc?.checks ?? [];
  const rep = qc?.reproduction ?? null;
  const gold = qc?.golden?.summary ?? null;
  return (
    <div className="space-y-6" data-testid="admin-dataqc">
      {!qc ? (
        <Notice kind="warning">The automated QC summary (qc_summary.json) is not available.</Notice>
      ) : (
        <>
          <p className="text-sm text-paper-mute">
            Automated checks of the shipped bundles, data release {qc.data_release ?? '–'}, run {qc.built_at?.slice(0, 16).replace('T', ' ') ?? '–'}. A passing check is not evidence that the analysis is right.
          </p>
          <div className="grid gap-3 sm:grid-cols-2 lg:grid-cols-4">
            <StatTile label="Checks" value={`${qc.status?.fail ?? 0} failing`} sub={`${qc.status?.warn ?? 0} warnings · ${qc.status?.pass ?? 0} passing · ${qc.status?.info ?? 0} for information`} />
            <StatTile label="Choices reproduced" value={rep ? pct(rep.rate) : '–'} sub={rep ? `${rep.reproduced} of ${rep.cells} cells from the decision trace; ${rep.exceptions_total ?? 0} exceptions` : undefined} />
            <StatTile label="Golden cases" value={gold ? `${gold.fail ?? 0} failing` : '–'} sub={gold ? Object.entries(gold).map(([k, v]) => `${k.replace('_', ' ')} ${v}`).join(' · ') : undefined} />
            <StatTile label="Open data reports" value={open.length} />
          </div>
          <section>
            <h3 className="eyebrow mb-2">Failures</h3>
            <CheckList checks={checks.filter((c) => c.status === 'fail')} empty="No automated check failed." />
          </section>
          <section>
            <h3 className="eyebrow mb-2">Stale data and missing provenance</h3>
            <CheckList checks={checks.filter((c) => c.status === 'warn' && /provenance|memos/.test(c.area))} empty="No provenance warning." />
          </section>
          <section>
            <h3 className="eyebrow mb-2">Other warnings</h3>
            <CheckList checks={checks.filter((c) => c.status === 'warn' && !/provenance|memos/.test(c.area))} empty="No other warning." />
          </section>
          {(qc.golden?.results ?? []).some((r) => r.status !== 'pass') && (
            <section>
              <h3 className="eyebrow mb-2">Golden cases not passing</h3>
              <ul className="list-disc pl-5 text-sm text-paper-dim">
                {(qc.golden?.results ?? [])
                  .filter((r) => r.status !== 'pass')
                  .map((r) => (
                    <li key={r.id}>
                      {r.id}: {r.status} {r.failures.join('; ')}
                    </li>
                  ))}
              </ul>
            </section>
          )}
        </>
      )}
      <section>
        <h3 className="eyebrow mb-2">Open data-issue reports</h3>
        {open.length === 0 ? (
          <p className="text-sm text-paper-mute">No open report.</p>
        ) : (
          <ul className="space-y-2">
            {open.map((p) => (
              <li key={p.group_id} className="panel px-3 py-2 text-sm">
                <span className="text-paper-dim">{p.title}</span>
                <span className="block text-xs text-paper-mute">
                  {SEVERITY_LABEL[p.severity]} · {p.n_reports} reports · {[p.country, p.tech, p.stage].filter(Boolean).join(' · ')}
                </span>
              </li>
            ))}
          </ul>
        )}
      </section>
    </div>
  );
}

function RobustnessTab({ map, qc, dash }: { map: RobustnessMap | null; qc: QcSummaryLike | null; dash: Dashboard }) {
  const grid = useMemo(() => (map ? robustnessGrid(map) : null), [map]);
  const drivers = qc?.robustness?.posture?.by_driver ?? {};
  const aib = map?.aib?.summary.by_class ?? qc?.robustness?.aib?.by_class ?? null;
  return (
    <div className="space-y-6" data-testid="admin-robustness">
      <p className="max-w-3xl text-sm text-paper-mute">
        Fragile: a small move of one threshold changes the choice. Sensitive: only a structural alternative does. Stable: none does. Not testable: the inputs are not in the release. Data
        quality, robustness and expert validation are reported separately, never merged.
      </p>
      {map?.posture ? (
        <>
          <div className="grid gap-3 sm:grid-cols-2 lg:grid-cols-5">
            {ROBUSTNESS_CLASSES.map((c) => (
              <StatTile key={c} label={ROBUSTNESS_LABEL[c]} value={(map.posture?.summary.by_class[c] ?? 0).toLocaleString('en-US')} sub="posture cells" />
            ))}
            <StatTile label="Fence would change" value={(map.posture.summary.fence_change ?? 0).toLocaleString('en-US')} sub="posture cells" />
          </div>
          {grid && (
            <div className="overflow-x-auto">
              <table className="data-table" data-testid="robustness-grid">
                <caption className="label text-left">Fragile / sensitive cells by technology and stage (all economies)</caption>
                <thead>
                  <tr>
                    <th>Technology</th>
                    {grid.stages.map((s) => (
                      <th key={s}>{s}</th>
                    ))}
                  </tr>
                </thead>
                <tbody>
                  {grid.techs.map((t) => (
                    <tr key={t}>
                      <td>{t}</td>
                      {grid.stages.map((s) => {
                        const k = grid.counts[t][s];
                        return (
                          <td key={s} className="tabular" title={ROBUSTNESS_CLASSES.map((c) => `${ROBUSTNESS_LABEL[c]} ${k[c]}`).join(', ')}>
                            {k.fragile} / {k.sensitive}
                          </td>
                        );
                      })}
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          )}
        </>
      ) : (
        <Notice kind="info">The robustness map (robustness.json) is not available.</Notice>
      )}
      {Object.keys(drivers).length > 0 && (
        <section>
          <h3 className="eyebrow mb-2">What drives the flips (posture cells)</h3>
          <ul className="text-sm text-paper-dim">
            {Object.entries(drivers)
              .sort((a, b) => b[1] - a[1])
              .map(([d, n]) => (
                <li key={d}>
                  <span className="tabular">{n.toLocaleString('en-US')}</span> · {d.replace(/_/g, ' ')}
                </li>
              ))}
          </ul>
        </section>
      )}
      {aib && (
        <p className="text-sm text-paper-dim">
          Allied Industrial Base steps (Decoupling): {ROBUSTNESS_CLASSES.map((c) => `${ROBUSTNESS_LABEL[c].toLowerCase()} ${aib[c] ?? 0}`).join(', ')}.
        </p>
      )}
      <p className="text-sm text-paper-dim">
        Review cases: {ROBUSTNESS_CLASSES.map((c) => `${ROBUSTNESS_LABEL[c].toLowerCase()} ${dash.robustness.counts[c]}`).join(', ')}
        {dash.robustness.counts.missing ? `, no record ${dash.robustness.counts.missing}` : ''}.
      </p>
      {map && map.not_runnable_on_bundle.length > 0 && (
        <details className="disclosure border-t hairline">
          <summary className="py-2 text-sm text-paper-dim">Alternatives not runnable on the shipped data ({map.not_runnable_on_bundle.length})</summary>
          <ul className="list-disc pb-2 pl-8 text-xs text-paper-mute">
            {map.not_runnable_on_bundle.map((s) => (
              <li key={s}>{s}</li>
            ))}
          </ul>
        </details>
      )}
    </div>
  );
}

const ADJ_STATUSES: AdjudicationStatus[] = ['open', 'confirmed', 'rejected', 'duplicate', 'resolved', 'wont_fix'];
const ADJ_STATUS_LABEL: Readonly<Record<AdjudicationStatus, string>> = {
  open: 'Open',
  confirmed: 'Confirmed',
  rejected: 'Rejected (not an issue)',
  duplicate: 'Duplicate',
  resolved: 'Resolved',
  wont_fix: 'Will not fix',
};

function PriorityTab({
  dash, subs, showNames, decidedBy, onDecidedBy, onSave,
}: {
  dash: Dashboard;
  subs: Submission[];
  showNames: boolean;
  decidedBy: string;
  onDecidedBy: (v: string) => void;
  onSave: (a: Adjudication) => void;
}) {
  const [editing, setEditing] = useState<string | null>(null);
  const byId = new Map(subs.map((s) => [s.submission_id, s]));
  if (!dash.priorities.length) return <NoData>No issue reported yet.</NoData>;
  return (
    <div data-testid="admin-priority">
      <p className="mb-3 max-w-3xl text-sm text-paper-mute">
        Priority = materiality × confidence the issue is real × breadth. Materiality: cosmetic 0.1, minor 0.3, material 0.6, critical 1.0. Confidence: the adjudication where there is one, else the
        reporters&apos; confidence weighted by their expertise. Breadth = 1 + log2(1 + outputs affected). The number of reports is shown but never ranks alone.
      </p>
      <ol className="space-y-3">
        {dash.priorities.map((p, i) => (
          <li key={p.group_id} className="panel p-4" data-testid="priority-item" data-severity={p.severity} data-status={p.adjudication_status ?? 'none'}>
            <div className="flex flex-wrap items-start justify-between gap-2">
              <div className="min-w-0">
                <span className="eyebrow">#{i + 1} · {p.source.replace('_', ' ')}</span>
                <p className="mt-1 text-sm text-paper">{p.title}</p>
                <p className="mt-0.5 text-xs text-paper-mute">
                  {ISSUE_TYPE_LABEL[p.issue_type]} · {[p.case_id, p.metric_id, p.country, p.tech, p.stage].filter(Boolean).join(' · ')}
                </p>
              </div>
              <div className="text-right">
                <p className="text-lg font-medium tabular text-paper" title="Priority score">
                  {n2(p.score)}
                </p>
                <p className="text-xs tabular text-paper-mute" data-testid="priority-factors">
                  {n2(p.materiality)} × {n2(p.confidence_real)} × {n2(p.breadth)}
                </p>
              </div>
            </div>
            <dl className="mt-2 grid grid-cols-2 gap-x-4 gap-y-1 text-xs sm:grid-cols-4">
              <div>
                <dt className="text-paper-mute">Materiality</dt>
                <dd className="text-paper-dim">{SEVERITY_LABEL[p.severity]}</dd>
              </div>
              <div>
                <dt className="text-paper-mute">Confidence it is real</dt>
                <dd className="text-paper-dim">
                  {pct(p.confidence_real)} ({p.confidence_basis === 'adjudication' ? 'adjudicated' : 'from the reports'})
                </dd>
              </div>
              <div>
                <dt className="text-paper-mute">Breadth</dt>
                <dd className="text-paper-dim">
                  {p.affected_outputs} outputs → {n2(breadthOf(p.affected_outputs))}
                </dd>
              </div>
              <div>
                <dt className="text-paper-mute">Reports</dt>
                <dd className="text-paper-dim">
                  {p.n_reports} from {p.n_reviewers} reviewers · {p.adjudication_status ? ADJ_STATUS_LABEL[p.adjudication_status] : 'not adjudicated'}
                </dd>
              </div>
            </dl>
            <p className="mt-1 text-xs text-paper-mute">
              Reported by:{' '}
              {p.submission_ids
                .map((id) => byId.get(id))
                .filter((s): s is Submission => !!s)
                .map((s) => reviewerLabel(s, showNames))
                .join(', ')}
            </p>
            <button type="button" className="btn btn-quiet btn-sm mt-2" aria-expanded={editing === p.group_id} onClick={() => setEditing(editing === p.group_id ? null : p.group_id)}>
              {editing === p.group_id ? 'Close the adjudication' : 'Adjudicate'}
            </button>
            {editing === p.group_id && (
              <AdjudicationEditor
                item={p}
                decidedBy={decidedBy}
                onDecidedBy={onDecidedBy}
                onSave={(a) => {
                  onSave(a);
                  setEditing(null);
                }}
              />
            )}
          </li>
        ))}
      </ol>
    </div>
  );
}

function AdjudicationEditor({ item, decidedBy, onDecidedBy, onSave }: { item: PriorityItem; decidedBy: string; onDecidedBy: (v: string) => void; onSave: (a: Adjudication) => void }) {
  const [status, setStatus] = useState<AdjudicationStatus>(item.adjudication_status ?? 'confirmed');
  const [issueType, setIssueType] = useState<IssueType>(item.issue_type);
  const [conf, setConf] = useState(item.confidence_basis === 'adjudication' ? String(item.confidence_real) : '');
  const [affected, setAffected] = useState(String(item.affected_outputs));
  const [note, setNote] = useState('');
  const [commit, setCommit] = useState('');
  const c = conf.trim() === '' ? null : Number(conf);
  const a = affected.trim() === '' ? null : Number(affected);
  const bad = (c !== null && (!Number.isFinite(c) || c < 0 || c > 1)) || (a !== null && (!Number.isFinite(a) || a < 0));
  return (
    <form
      className="mt-3 grid gap-x-4 border-t hairline pt-3 sm:grid-cols-2"
      noValidate
      data-testid="adjudication-editor"
      onSubmit={(e) => {
        e.preventDefault();
        if (bad) return;
        const uuid = (globalThis.crypto?.randomUUID?.() ?? `${Date.now().toString(16)}${Math.random().toString(16).slice(2)}`) as string;
        onSave({
          adjudication_id: `ADJ-${uuid}`,
          submission_id: item.submission_ids[0],
          status,
          issue_type: issueType,
          confidence_real: c,
          affected_outputs: a,
          note: note.trim() || null,
          decided_by: decidedBy.trim() || 'NEIS',
          decided_at: new Date().toISOString(),
          resolution_commit: commit.trim() || null,
        });
      }}
    >
      <Field label="Status">
        {({ id }) => (
          <select id={id} className="field" value={status} onChange={(e) => setStatus(e.target.value as AdjudicationStatus)}>
            {ADJ_STATUSES.map((s) => (
              <option key={s} value={s}>
                {ADJ_STATUS_LABEL[s]}
              </option>
            ))}
          </select>
        )}
      </Field>
      <Field label="Kind of issue">
        {({ id }) => (
          <select id={id} className="field" value={issueType} onChange={(e) => setIssueType(e.target.value as IssueType)}>
            {(Object.keys(ISSUE_TYPE_LABEL) as IssueType[]).map((t) => (
              <option key={t} value={t}>
                {ISSUE_TYPE_LABEL[t]}
              </option>
            ))}
          </select>
        )}
      </Field>
      <Field label="Confidence it is real (0 to 1)" error={c !== null && (!Number.isFinite(c) || c < 0 || c > 1) ? 'A number from 0 to 1.' : null}>
        {({ id, describedBy, invalid }) => <input id={id} className="field" inputMode="decimal" value={conf} aria-describedby={describedBy} aria-invalid={invalid || undefined} onChange={(e) => setConf(e.target.value)} placeholder="1 when confirmed" />}
      </Field>
      <Field label="Outputs affected (count)" error={a !== null && (!Number.isFinite(a) || a < 0) ? 'A count of 0 or more.' : null}>
        {({ id, describedBy, invalid }) => <input id={id} className="field" inputMode="numeric" value={affected} aria-describedby={describedBy} aria-invalid={invalid || undefined} onChange={(e) => setAffected(e.target.value)} />}
      </Field>
      <div className="sm:col-span-2">
        <TextArea label="Note" rows={2} value={note} onChange={setNote} />
      </div>
      <Field label="Resolution commit">
        {({ id }) => <input id={id} className="field" value={commit} onChange={(e) => setCommit(e.target.value)} placeholder="git commit that fixes it" />}
      </Field>
      <Field label="Decided by">
        {({ id }) => <input id={id} className="field" value={decidedBy} onChange={(e) => onDecidedBy(e.target.value)} />}
      </Field>
      <div className="sm:col-span-2">
        <button type="submit" className="btn btn-primary btn-sm" disabled={bad}>
          Save the adjudication
        </button>
      </div>
    </form>
  );
}
