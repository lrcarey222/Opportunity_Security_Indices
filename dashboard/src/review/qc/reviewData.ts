// The review data build (docs/QC_SYSTEM_DESIGN.md 3, 4.2): authored YAML content + decoded shipped bundles + one
// shipped-bundle QC run -> the documents dashboard/public/data/review/ serves: cases.<campaign>.json (CasesFile:
// each case's snapshot, its sha256, the stamps of the bundle files it was read from, the data release, app
// commit, build time and canonical answer options), method_cards.json, framework_scenarios.json (canonical
// option sets filled from the vocabulary constants), robustness.json, qc_summary.json and manifest.json.
// Every authored file is validated with review/manifest.ts first, and the built cases are validated again; any
// error stops the build. Deterministic: the same content, bundles, app commit and built_at give byte-identical
// documents (keys in a fixed order, campaigns and files sorted). Pure: no IO (dashboard/scripts/review/
// build-review-data.mjs reads and writes the files).

import { REVIEW_MANIFEST_SCHEMA, CASES_SCHEMA, DONT_KNOW, type BundleFileStamp, type Campaign, type CaseDefinition, type CasesFile, type FrameworkScenario, type MethodCard, type ReviewCase, type RobustnessRecord } from '../model.ts';
import { validateCampaign, validateCaseCatalogue, validateCasesFile, validateFrameworkScenarios, validateMethodCards } from '../manifest.ts';
import { canonicalJson, sha256HexSync } from '../privacy.ts';
import type { ReleaseStamps } from '../release.ts';
import { answerOptions } from './answerOptions.ts';
import { cellKey, isObj, type DecodedBundles } from './bundles.ts';
import { qcSummaryDoc, robustnessMap } from './reports.ts';
import type { ShippedQcRun } from './run.ts';
import { buildCaseSnapshot, snapshotFound, splitFacts } from './snapshot.ts';

export const METHOD_CARDS_SCHEMA = 'aib-review-method-cards';
export const FRAMEWORK_SCENARIOS_SCHEMA = 'aib-review-framework-scenarios';

// ---- authored content --------------------------------------------------------------------------------------------

export interface ReviewContentRaw {
  /** each config/review/campaigns/<file>.yml, parsed */
  campaigns: { file: string; doc: unknown }[];
  /** config/review/review_cases.yml ({cases: [...]} or a list) */
  catalogue: unknown;
  /** config/review/method_cards.yml ({cards: [...]} or a list) */
  cards: unknown;
  /** config/review/framework_scenarios.yml ({scenarios: [...]} or a list) */
  scenarios: unknown;
}

export interface ReviewContent {
  campaigns: Campaign[];
  cases: CaseDefinition[];
  cards: MethodCard[];
  scenarios: FrameworkScenario[];
}

const listOf = (doc: unknown, key: string): unknown => (isObj(doc) && Array.isArray(doc[key]) ? doc[key] : doc);
const byId = <T,>(xs: readonly T[], id: (x: T) => string) => [...xs].sort((a, b) => (id(a) < id(b) ? -1 : id(a) > id(b) ? 1 : 0));

/** Validate every authored file (review/manifest.ts) and the cross-references between them. */
export function validateReviewContent(raw: ReviewContentRaw): { content: ReviewContent | null; errors: string[]; warnings: string[] } {
  const errors: string[] = [];
  const warnings: string[] = [];
  const pre = (where: string, r: { errors: string[]; warnings: string[] }) => {
    errors.push(...r.errors.map((e) => `${where}: ${e}`));
    warnings.push(...r.warnings.map((w) => `${where}: ${w}`));
  };
  const cases = listOf(raw.catalogue, 'cases');
  pre('config/review/review_cases.yml', validateCaseCatalogue(cases));
  const caseIds = Array.isArray(cases) ? cases.filter(isObj).map((c) => String(c.case_id)) : [];
  const cards = listOf(raw.cards, 'cards');
  pre('config/review/method_cards.yml', validateMethodCards(cards));
  const scenarios = listOf(raw.scenarios, 'scenarios');
  pre('config/review/framework_scenarios.yml', validateFrameworkScenarios(scenarios));
  const cardIds = new Set(Array.isArray(cards) ? cards.filter(isObj).map((c) => String(c.card_id)) : []);
  const scenIds = new Set(Array.isArray(scenarios) ? scenarios.filter(isObj).map((s) => String(s.scenario_id)) : []);
  const seen = new Set<string>();
  if (!raw.campaigns.length) errors.push('config/review/campaigns: no campaign file');
  for (const { file, doc } of raw.campaigns) {
    const where = `config/review/campaigns/${file}`;
    pre(where, validateCampaign(doc, caseIds));
    if (!isObj(doc)) continue;
    const id = String(doc.campaign_id);
    if (seen.has(id)) errors.push(`${where}: campaign_id ${id} is used by another campaign file`);
    seen.add(id);
    if (file.replace(/\.ya?ml$/, '') !== id) warnings.push(`${where}: the file name differs from campaign_id ${id}`);
    for (const c of Array.isArray(doc.method_card_ids) ? doc.method_card_ids : []) if (!cardIds.has(String(c))) errors.push(`${where}: method_card_ids names ${String(c)}, which is not in method_cards.yml`);
    for (const s of Array.isArray(doc.framework_scenario_ids) ? doc.framework_scenario_ids : []) if (!scenIds.has(String(s))) errors.push(`${where}: framework_scenario_ids names ${String(s)}, which is not in framework_scenarios.yml`);
  }
  if (errors.length) return { content: null, errors, warnings };
  return {
    content: {
      campaigns: byId(raw.campaigns.map((c) => c.doc as Campaign), (c) => c.campaign_id),
      cases: cases as CaseDefinition[],
      cards: cards as MethodCard[],
      scenarios: scenarios as FrameworkScenario[],
    },
    errors,
    warnings,
  };
}

// ---- bundle files a case reads -----------------------------------------------------------------------------------

/** The bundle files (paths relative to public/data) a case's snapshot is read from. */
export function caseBundleFiles(def: Pick<CaseDefinition, 'kind' | 'key'>, b: DecodedBundles): string[] {
  const out: string[] = [];
  const pm = b.posture?.manifest ?? null;
  const am = b.aib?.manifest ?? null;
  const afiles = am && isObj(am.files) ? am.files : {};
  const scen = isObj(afiles.scenarios) ? afiles.scenarios : {};
  switch (def.kind) {
    case 'posture_cell':
    case 'sector': {
      out.push('posture/manifest.json');
      if (pm && typeof pm.core === 'string') out.push(`posture/${pm.core}`);
      const det = pm && isObj(pm.details) && def.key.iso3 ? pm.details[def.key.iso3] : null;
      if (typeof det === 'string') out.push(`posture/${det}`);
      break;
    }
    case 'aib_node':
    case 'compact': {
      out.push('aib/manifest.json');
      if (typeof afiles.compacts === 'string') out.push(`aib/${afiles.compacts}`);
      if (def.key.scenario === 'managed' && typeof scen.managed === 'string') out.push(`aib/${scen.managed}`);
      break;
    }
    case 'partnership': {
      out.push('partnerships/manifest.json');
      const e = b.partnerships && def.key.pair ? (b.partnerships.manifest.pairs[def.key.pair] ?? b.partnerships.manifest.compacts[def.key.pair]) : null;
      if (e?.json) out.push(`partnerships/${e.json}`);
      break;
    }
  }
  return out;
}

// ---- framework scenarios: canonical option sets ------------------------------------------------------------------

/**
 * The options of a scenario's canonical option_set (DONT_KNOW left to the form, which offers it everywhere). A
 * compact_decision scenario answers one of two questions, so it gets one family only: the strategic compact keys
 * (sectoral / narrower / none) when its intended answer is one of them, the operational compact types when the
 * intended answer is one of those; only a scenario whose intended answer is in neither family gets both.
 */
export function optionSetOptions(set: FrameworkScenario['option_set'], intendedAnswer?: string | null): FrameworkScenario['options'] | null {
  const strip = (o: FrameworkScenario['options']) => o.filter((x) => x.value !== DONT_KNOW);
  switch (set) {
    case 'posture_choice':
      return strip(answerOptions('posture_cell'));
    case 'aib_class':
      return strip(answerOptions('aib_node'));
    case 'compact_decision': {
      const strategic = strip(answerOptions('compact', { sid: 'SC-' }));
      const operational = strip(answerOptions('compact', { sid: 'AIB-C' }));
      if (intendedAnswer && strategic.some((o) => o.value === intendedAnswer)) return strategic;
      if (intendedAnswer && operational.some((o) => o.value === intendedAnswer)) return operational;
      return [...strategic, ...operational];
    }
    default:
      return null;
  }
}

// ---- the build ---------------------------------------------------------------------------------------------------

export interface BuildInput {
  content: ReviewContent;
  decoded: DecodedBundles;
  run: ShippedQcRun;
  releaseId: string;
  appCommit: string;
  builtAt: string;
}

export interface BuiltReviewData {
  casesFiles: CasesFile[];
  methodCards: { schema: typeof METHOD_CARDS_SCHEMA; schema_version: 1; data_release: string; built_at: string; cards: MethodCard[] };
  frameworkScenarios: { schema: typeof FRAMEWORK_SCENARIOS_SCHEMA; schema_version: 1; data_release: string; built_at: string; scenarios: FrameworkScenario[] };
  robustness: ReturnType<typeof robustnessMap>;
  qcSummary: ReturnType<typeof qcSummaryDoc>;
  /** bundle files the cases read (union, sorted) */
  bundleFiles: BundleFileStamp[];
  errors: string[];
  warnings: string[];
}

/** Build every review document from validated content, the decoded bundles and one QC run. */
export function buildReviewData(input: BuildInput): BuiltReviewData {
  const { content, decoded, run, releaseId, appCommit, builtAt } = input;
  const errors: string[] = [];
  const warnings: string[] = [];
  const stampOf = new Map(decoded.files.map((f) => [f.file, f]));
  const ctx = {
    postureRobustness: new Map<string, RobustnessRecord>((run.posture?.records ?? []).map((r) => [cellKey(r.case_key.iso3 ?? '', r.case_key.tech ?? '', r.case_key.stage ?? ''), r])),
    aibRobustness: new Map<string, RobustnessRecord>(run.aib.map((r) => [r.case_key.node ?? '', r])),
  };
  const defs = new Map(content.cases.map((c) => [c.case_id, c]));
  const used = new Map<string, BundleFileStamp>();
  const casesFiles: CasesFile[] = content.campaigns.map((camp) => {
    const cases: ReviewCase[] = [];
    for (const id of camp.case_ids) {
      const def = defs.get(id);
      if (!def) {
        errors.push(`${camp.campaign_id}: case ${id} is not in the catalogue`);
        continue;
      }
      const snapshot = buildCaseSnapshot(def, decoded, ctx);
      if (!snapshotFound(snapshot)) errors.push(`${id}: ${def.kind} ${canonicalJson(def.key)} is not in this release (${snapshot.conclusion.reason ?? 'not found'})`);
      const split = splitFacts(def, snapshot);
      errors.push(...split.errors);
      for (const f of split.context) if (f.value === null) warnings.push(`${id}: context fact ${f.id} is not in this release (${f.note ?? 'no note'})`);
      const bundle: BundleFileStamp[] = [];
      for (const rel of caseBundleFiles(def, decoded)) {
        const s = stampOf.get(rel);
        if (!s) errors.push(`${id}: bundle file ${rel} was not read (no stamp)`);
        else {
          bundle.push(s);
          used.set(rel, s);
        }
      }
      cases.push({
        ...def,
        snapshot,
        snapshot_sha256: sha256HexSync(canonicalJson(snapshot)),
        bundle,
        data_release: releaseId,
        app_commit: appCommit,
        built_at: builtAt,
        answer_options: answerOptions(def.kind, def.key),
      });
    }
    const file: CasesFile = { schema: CASES_SCHEMA, schema_version: 1, campaign_id: camp.campaign_id, data_release: releaseId, built_at: builtAt, app_commit: appCommit, cases };
    const v = validateCasesFile(file);
    errors.push(...v.errors.map((e) => `cases.${camp.campaign_id}.json: ${e}`));
    // the per-case "no git_commit" warnings repeat one provenance gap per file: say it once per file
    const noCommit = new Map<string, number>();
    for (const w of v.warnings) {
      const m = /\.bundle\[\d+\] (\S+): no git_commit/.exec(w);
      if (m) noCommit.set(m[1], (noCommit.get(m[1]) ?? 0) + 1);
      else warnings.push(`cases.${camp.campaign_id}.json: ${w}`);
    }
    if (noCommit.size)
      warnings.push(`cases.${camp.campaign_id}.json: ${noCommit.size} bundle files the cases read carry no git_commit (provenance gap): ${[...noCommit.keys()].sort().join(', ')}`);
    return file;
  });
  const scenarios = content.scenarios.map((s) => {
    const fill = s.options && s.options.length ? null : optionSetOptions(s.option_set, s.intended_answer);
    return fill ? { ...s, options: fill } : s;
  });
  const fv = validateFrameworkScenarios(scenarios);
  errors.push(...fv.errors.map((e) => `framework_scenarios.json: ${e}`));
  const techs = decoded.posture?.core.meta.techs ?? [];
  return {
    casesFiles,
    methodCards: { schema: METHOD_CARDS_SCHEMA, schema_version: 1, data_release: releaseId, built_at: builtAt, cards: content.cards },
    frameworkScenarios: { schema: FRAMEWORK_SCENARIOS_SCHEMA, schema_version: 1, data_release: releaseId, built_at: builtAt, scenarios },
    robustness: robustnessMap(run, techs),
    qcSummary: qcSummaryDoc(run, { built_at: builtAt, app_commit: appCommit }),
    bundleFiles: [...used.values()].sort((a, b) => (a.file < b.file ? -1 : a.file > b.file ? 1 : 0)),
    errors,
    warnings,
  };
}

// ---- manifest.json -----------------------------------------------------------------------------------------------

export interface OutputFile {
  /** path relative to public/data/review */
  file: string;
  sha256: string;
  bytes: number;
}

/** The review manifest (schema aib-review-manifest): campaigns, the data release, the files written and read. */
export function reviewManifest(o: {
  releaseId: string;
  stamps: ReleaseStamps;
  completeness: { complete: boolean; missing: string[] };
  appCommit: string;
  builtAt: string;
  content: ReviewContent;
  casesFiles: readonly CasesFile[];
  outputs: readonly OutputFile[];
  bundleFiles: readonly BundleFileStamp[];
  run: ShippedQcRun;
}) {
  const fileOf = (id: string) => `cases.${id}.json`;
  return {
    schema: REVIEW_MANIFEST_SCHEMA,
    schema_version: 1 as const,
    built_at: o.builtAt,
    app_commit: o.appCommit,
    data_release: { id: o.releaseId, stamps: o.stamps, complete: o.completeness.complete, missing: o.completeness.missing },
    campaigns: o.content.campaigns.map((c) => {
      const cf = o.casesFiles.find((f) => f.campaign_id === c.campaign_id);
      const a = c.assignment;
      return {
        campaign_id: c.campaign_id, name: c.name, status: c.status, opens: c.opens ?? null, closes: c.closes ?? null,
        description: typeof c.description === 'string' ? c.description.trim() : null, review_types: c.review_types,
        n_cases: cf?.cases.length ?? 0, case_ids: [...c.case_ids], anchor_cases: c.anchor_cases, cases_file: fileOf(c.campaign_id),
        // the campaign's own rules, so the browser applies the YAML (review/data/store.ts campaignOf); keys in a fixed order
        target_reviews_per_anchor: c.target_reviews_per_anchor,
        assignment: {
          assigned_per_reviewer: { min: a.assigned_per_reviewer.min, max: a.assigned_per_reviewer.max },
          anchors_per_reviewer: a.anchors_per_reviewer,
          out_of_specialty: a.out_of_specialty,
          allow_self_selected: a.allow_self_selected,
          min_domain_confidence: a.min_domain_confidence,
        },
        // pre-registered reviewer codes only (never names): the YAML's optional list
        reviewers: (c.reviewers ?? []).map((r) => ({ code: r.code, groups: [...(r.groups ?? [])] })),
        method_card_ids: c.method_card_ids?.length ? c.method_card_ids : o.content.cards.map((x) => x.card_id),
        framework_scenario_ids: c.framework_scenario_ids?.length ? c.framework_scenario_ids : o.content.scenarios.map((x) => x.scenario_id),
      };
    }),
    files: [...o.outputs].sort((a, b) => (a.file < b.file ? -1 : a.file > b.file ? 1 : 0)),
    bundle_files: o.bundleFiles,
    qc: {
      status: o.run.status,
      reproduction: o.run.reproduction ? { cells: o.run.reproduction.summary.cells, reproduced: o.run.reproduction.summary.reproduced, rate: o.run.reproduction.summary.rate } : null,
      golden: o.run.golden?.summary ?? null,
    },
  };
}
