// A small synthetic review campaign for the reviewer-app UI tests (test-only; no real reviewer data): a manifest,
// a cases file and helpers that inject them (review/data/store.ts) with an in-memory browser store
// (review/storage.ts). The conclusion, reason, fence, why-selected text and reveal-only values carry distinctive
// strings so a test can prove they are absent from the page before the blind answer is stored.

import { setReviewStoreForTests, memoryReviewSource, ReviewStore, type ReviewManifestDoc } from '../../data/store.ts';
import { answerOptions } from '../../qc/answerOptions.ts';
import type { CaseFact, CasesFile, ReviewCase, ReviewerProfile } from '../../model.ts';
import { createReviewStorage, setReviewStorage, type KeyValueBackend, type ReviewStorage } from '../../storage.ts';

export const RELEASE = 'rel-0123456789ab';
export const SHA = (n: number) => n.toString(16).padStart(64, '0');

const fact = (id: string, label: string, value: CaseFact['value'], display: string, extra: Partial<CaseFact> = {}): CaseFact => ({
  id, label, value, display, unit: null, source: null, source_year: null, note: null, ...extra,
});

/** Distinctive strings that may only appear after the blind answer. */
export const SECRET = {
  reason: 'a top-quarter position zqx-reason among contenders',
  detail: 'Medium fence zqx-detail · Compete',
  why: 'Chosen because zqx-why sits on the threshold',
  needDisplay: '0.7321',
  cpDisplay: '0.7634',
  choiceDisplay: 'Preserve zqx-choice',
  fenceDisplay: 'Medium zqx-fence',
};

function postureCase(id: string, iso3: string, tech: string, stage: string, over: Partial<ReviewCase> = {}): ReviewCase {
  return {
    case_id: id,
    kind: 'posture_cell',
    key: { iso3, tech, stage },
    title: `${iso3}: ${tech} ${stage.toLowerCase()}`,
    question: `Which strategic choice would you recommend for ${iso3} ${tech} ${stage}?`,
    sector_groups: [tech.toLowerCase().replace(/ /g, '-')],
    country_groups: [iso3],
    stage,
    assignment_group: tech.toLowerCase(),
    anchor: false,
    why_selected: `Why ${id} was chosen`,
    explorer_path: `/countries/${iso3}?node=${tech}|${stage}`,
    context_fields: ['home_share', 'china_share', 'imports_to_gdp'],
    reveal_fields: ['choice', 'fence', 'reason', 'need', 'cp'],
    snapshot: {
      conclusion: { value: 'watch', label: 'Watch', detail: 'Low fence', reason: `reason of ${id}`, confidence: 'high' },
      facts: {
        home_share: fact('home_share', "The economy's share of world supply", 0.062, '6.2%'),
        china_share: fact('china_share', 'China (with HKG and MAC) share of imports', 0.766, '76.6%', { note: 'blended with 12 months of trade' }),
        imports_to_gdp: fact('imports_to_gdp', 'Imports of the stage basket / GDP', null, '—'),
        choice: fact('choice', 'Strategic choice', 'watch', 'Watch'),
        fence: fact('fence', 'Fence', 'low', 'Low'),
        reason: fact('reason', 'Why (the deciding test, in words)', `reason of ${id}`, `reason of ${id}`),
        need: fact('need', 'Supply-chain vulnerability (Strategic Need, 0-1 percentile)', 0.2, '0.200'),
        cp: fact('cp', 'Competitive Position among contenders (0-1)', 0.1, '0.100'),
      },
      trace: [
        { step: 'need', test: 'supply_chain_need', value: 0.2, threshold: 0.67, result: 'low' },
        { step: 'choice', test: 'choice', value: null, threshold: null, result: 'watch' },
      ],
      threat: null,
      robustness: null,
      data_quality: { grade: null, notes: [] },
    },
    snapshot_sha256: SHA(Number(id.slice(-3))),
    bundle: [{ file: 'posture/manifest.json', sha256: SHA(99), created: '2026-11-17T20:33:31Z', git_commit: '0496e7ba' }],
    data_release: RELEASE,
    app_commit: 'test',
    built_at: '2026-11-20T00:00:00Z',
    answer_options: answerOptions('posture_cell'),
    ...over,
  };
}

/** CASE_001: Korea batteries components, an anchor with decision-use questions and distinctive reveal strings. */
const case1 = postureCase('CASE_001', 'KOR', 'Batteries', 'Components', {
  title: 'Korea: battery component manufacturing',
  anchor: true,
  decision_use: true,
  why_selected: SECRET.why,
  snapshot: {
    conclusion: { value: 'preserve', label: 'Preserve', detail: SECRET.detail, reason: SECRET.reason, confidence: 'moderate' },
    facts: {
      home_share: fact('home_share', "The economy's share of world supply", 0.0617, '6.2%'),
      china_share: fact('china_share', 'China (with HKG and MAC) share of imports', 0.766, '76.6%'),
      imports_to_gdp: fact('imports_to_gdp', 'Imports of the stage basket / GDP', null, '—'),
      choice: fact('choice', 'Strategic choice', 'preserve', SECRET.choiceDisplay),
      fence: fact('fence', 'Fence', 'medium', SECRET.fenceDisplay),
      reason: fact('reason', 'Why (the deciding test, in words)', SECRET.reason, SECRET.reason),
      need: fact('need', 'Supply-chain vulnerability (Strategic Need, 0-1 percentile)', 0.7321, SECRET.needDisplay),
      cp: fact('cp', 'Competitive Position among contenders (0-1)', 0.7634, SECRET.cpDisplay),
    },
    trace: [
      { step: 'need', test: 'supply_chain_need', value: 0.7321, threshold: 0.67, result: 'high' },
      { step: 'position', test: 'cp_contender_pct', value: 0.7634, threshold: 0.75, result: 'high' },
      { step: 'choice', test: 'choice', value: null, threshold: null, result: 'preserve' },
    ],
    threat: null,
    robustness: {
      layer: 'bundle',
      case_key: { iso3: 'KOR', tech: 'Batteries', stage: 'Components' },
      baseline_choice: 'preserve',
      specs_run: 13,
      local_flips: [{ spec_id: 'cp_plus', kind: 'local', label: 'Competitive Position bar 0.8 (+0.05)', choice: 'catch_up', changed: true, fence_changed: false }],
      structural_flips: [],
      fence_change: false,
      class: 'fragile',
      drivers: ['cp_contender_pct'],
      note: null,
    },
    data_quality: { grade: 'medium', notes: ['import shares blended with current trade'] },
  },
});

/** CASE_005: an allied supply-chain step with threat questions and a threat block with nulls. */
const case5: ReviewCase = {
  case_id: 'CASE_005',
  kind: 'aib_node',
  key: { node: 'graphite-electrodes', scenario: 'decoupling' },
  title: 'Graphite electrodes under a major-supplier disruption',
  question: 'What does this step need from allies?',
  sector_groups: ['critical-minerals'],
  country_groups: [],
  stage: null,
  assignment_group: 'critical-minerals',
  anchor: false,
  why_selected: 'A step with a large gap',
  explorer_path: '/aib/diversification',
  context_fields: ['exposure', 'existing'],
  reveal_fields: ['class', 'residual'],
  threat: true,
  snapshot: {
    conclusion: { value: 'BUILD', label: 'Build capacity', detail: null, reason: 'the gap stays material', confidence: null },
    facts: {
      exposure: fact('exposure', 'Share of allied traded demand lost to the shock', 0.6, '60.0%'),
      existing: fact('existing', 'Coverage with existing relationships', 0.3, '30.0%'),
      class: fact('class', 'Treatment class', 'BUILD', 'BUILD'),
      residual: fact('residual', 'Remaining gap after coordinated trade', 0.4, '40.0%'),
    },
    trace: [],
    threat: {
      subject: { kind: 'aib_node', id: 'graphite-electrodes', scenario: 'decoupling', label: 'Graphite electrodes' },
      threat: 'Major-supplier disruption: 60% of allied demand lost',
      intervention: 'coordinated trade and new capacity',
      before: { hhi: 0.41, largest_supplier: 'CHN', largest_share: 0.6, top3_share: 0.8, risky_share: 0.6, coverage: 0.4, n_credible_suppliers: 3, n1_coverage: null },
      after: { hhi: null, largest_supplier: null, largest_share: null, top3_share: null, risky_share: null, coverage: 0.6, n_credible_suppliers: null, n1_coverage: null },
      lost_supply_usd: 2.5e9,
      lost_supply_replaced_share: 0.33,
      allied_coverage: 0.6,
      remaining_gap: 0.4,
      remaining_gap_usd: 1e9,
      new_capacity_share: null,
      time_to_capacity_years: null,
      dependence_outcome: 'unknown',
      notes: ['time to capacity is not modelled'],
    },
    robustness: null,
    data_quality: { grade: null, notes: [] },
  },
  snapshot_sha256: SHA(5),
  bundle: [{ file: 'aib/manifest.json', sha256: SHA(98), created: '2026-11-17T19:07:29Z', git_commit: null }],
  data_release: RELEASE,
  app_commit: 'test',
  built_at: '2026-11-20T00:00:00Z',
  answer_options: answerOptions('aib_node'),
};

/** CASE_006: a methodology-wide case (no sector or country group): never handed to a Korea-batteries reviewer. */
const case6 = postureCase('CASE_006', 'USA', 'Nuclear', 'Deployment', { sector_groups: [], country_groups: [], title: 'Method-wide: deployment thresholds' });

export const CASES: ReviewCase[] = [
  case1,
  postureCase('CASE_002', 'KOR', 'Batteries', 'Refining'),
  postureCase('CASE_003', 'KOR', 'Batteries', 'Extraction'),
  postureCase('CASE_004', 'CHN', 'Solar', 'Components', { anchor: true }),
  case5,
  case6,
];

export const CAMPAIGN_ID = 'test_round';

export function casesFile(cases: ReviewCase[] = CASES): CasesFile {
  return { schema: 'aib-review-cases', schema_version: 1, campaign_id: CAMPAIGN_ID, data_release: RELEASE, built_at: '2026-11-20T00:00:00Z', app_commit: 'test', cases };
}

export function manifest(): ReviewManifestDoc {
  return {
    schema: 'aib-review-manifest',
    schema_version: 1,
    built_at: '2026-11-20T00:00:00Z',
    app_commit: 'test',
    data_release: {
      id: RELEASE,
      stamps: {
        posture: { created: '2026-11-17T20:33:31Z', git_commit: '0496e7ba' },
        country: { created: '2026-10-31T15:58:15Z', git_commit: '4bb2adc' },
        sector: { bundle_id: 'sectorbundle' },
        aib: { created: '2026-11-17T19:07:29Z' },
        strategic: { bundle_id: 'strategicbundle' },
        memos: { manifest_sha256: SHA(7) },
      },
      complete: true,
      missing: [],
    },
    campaigns: [
      {
        campaign_id: CAMPAIGN_ID,
        name: 'Test campaign',
        status: 'draft',
        opens: null,
        closes: null,
        review_types: ['case', 'decision_use', 'threat'],
        n_cases: CASES.length,
        anchor_cases: ['CASE_001', 'CASE_004'],
        cases_file: `cases.${CAMPAIGN_ID}.json`,
        method_card_ids: [],
        framework_scenario_ids: [],
      },
    ],
    files: [],
  };
}

/** An in-memory Web Storage backend. */
export function memoryBackend(): KeyValueBackend & { map: Map<string, string> } {
  const map = new Map<string, string>();
  return {
    map,
    get length() {
      return map.size;
    },
    key: (i) => [...map.keys()][i] ?? null,
    getItem: (k) => map.get(k) ?? null,
    setItem: (k, v) => void map.set(k, v),
    removeItem: (k) => void map.delete(k),
  };
}

/** Inject the fixture data and a fresh browser store; returns the store and its backend. */
export function useReviewFixture(files?: Record<string, unknown>): { storage: ReviewStorage; backend: ReturnType<typeof memoryBackend> } {
  setReviewStoreForTests(new ReviewStore(memoryReviewSource(files ?? { 'manifest.json': manifest(), [`cases.${CAMPAIGN_ID}.json`]: casesFile() })));
  const backend = memoryBackend();
  const storage = createReviewStorage(backend);
  setReviewStorage(storage);
  return { storage, backend };
}

export function resetReviewFixture(): void {
  setReviewStoreForTests(null);
  setReviewStorage(null);
}

/** A Korea-batteries reviewer (rated 5 on both). */
export function koreaBatteriesProfile(over: Partial<ReviewerProfile> = {}): ReviewerProfile {
  return {
    reviewer_id: 'rv-test-0001',
    name: 'Test Reviewer',
    anonymous: false,
    organization: null,
    role: null,
    countries: ['KOR'],
    sectors: ['batteries'],
    stages: [],
    methods: [],
    confidence: { 'country:KOR': 5, 'sector:batteries': 5 },
    familiarity_neis: 1,
    familiarity_explorer: 1,
    advised_on: ['governments'],
    created_at: '2026-11-20T00:00:00Z',
    ...over,
  };
}

/** The jsdom stubs the shell needs (matchMedia, ResizeObserver, scrolling, Node's Request); returns the restore. */
export function installDomStubs(): () => void {
  const restore: (() => void)[] = [];
  const mm = window.matchMedia;
  window.matchMedia = ((q: string) => ({
    matches: /min-width/.test(q),
    media: q,
    onchange: null,
    addEventListener: () => undefined,
    removeEventListener: () => undefined,
    addListener: () => undefined,
    removeListener: () => undefined,
    dispatchEvent: () => false,
  })) as unknown as typeof window.matchMedia;
  restore.push(() => (window.matchMedia = mm));
  window.scrollTo = (() => undefined) as typeof window.scrollTo;
  Element.prototype.scrollIntoView = function () {} as typeof Element.prototype.scrollIntoView;
  if (!('ResizeObserver' in globalThis)) {
    (globalThis as { ResizeObserver?: unknown }).ResizeObserver = class {
      observe() {}
      unobserve() {}
      disconnect() {}
    };
    restore.push(() => delete (globalThis as { ResizeObserver?: unknown }).ResizeObserver);
  }
  const NodeRequest = globalThis.Request;
  globalThis.Request = class extends NodeRequest {
    constructor(input: RequestInfo | URL, init?: RequestInit) {
      const rest = { ...(init ?? {}) };
      delete (rest as { signal?: unknown }).signal;
      super(input, rest);
    }
  } as typeof Request;
  restore.push(() => (globalThis.Request = NodeRequest));
  return () => {
    for (const r of restore.reverse()) r();
  };
}
