// Draw the Fence: the evidence adapter. Pure.
//
// It turns the published bundle (contract.ts), the curated component table and the reader's answers
// into engine inputs, keeping the provenance of every value. Rules:
//   * Only raw measures with an `engineInput` feed the engine, from the component's primary evidence
//     scope, using the measure priority of the metric map. Nothing is read across scopes except the
//     global TRL of the technology-stage (shown as stage-level evidence).
//   * A curated assessment is ASSESSMENT; a reader's answer is YOUR JUDGMENT; a scenario value is
//     SCENARIO; nothing is ever filled from an unrelated indicator.

import { emptyInputs, UNKNOWN } from '../engine/index.ts';
import type { Assessment, AssessmentContext, CategoricalInputId, Confidence, InputId, Inputs, InputValue, NumericInputId, Provenance } from '../engine/index.ts';
import type { LayerState } from '../state/assessment.ts';
import { stageKey } from '../state/assessment.ts';
import type { ScopeType, Stage, YFComponent, YFCountryBundle, YFEvidenceRecord, YFReference, YFStack } from './contract.ts';

export interface Subject {
  key: string;
  label: string;
  technology: string;
  stage: Stage;
  component: YFComponent | null;
}

export function componentById(ref: YFReference, id: string | undefined): YFComponent | null {
  return id ? ref.components.find((c) => c.id === id) ?? null : null;
}

export function stackById(ref: YFReference, id: string | undefined): YFStack | null {
  return id ? ref.stacks.find((s) => s.id === id) ?? null : null;
}

export const stageScopeId = (technology: string, stage: string) => `${technology.toLowerCase().replace(/\s+/g, '-')}-${stage.toLowerCase()}`;

export function stageSubject(technology: string, stage: Stage): Subject {
  return { key: stageKey(technology, stage), label: `${technology} — ${stage} (whole stage)`, technology, stage, component: null };
}

export function componentSubject(c: YFComponent): Subject {
  return { key: c.id, label: c.label, technology: c.technology, stage: c.stage, component: c };
}

// ── Evidence selection ───────────────────────────────────────────────────────────────────────────

export interface ScopeRef {
  scope: ScopeType;
  scopeId: string;
}

export interface EvidenceSet {
  /** the scope that best describes the subject (null: no measured scope matches the component) */
  primary: ScopeRef | null;
  primaryRecords: YFEvidenceRecord[];
  secondary: { ref: ScopeRef; records: YFEvidenceRecord[] }[];
  /** the technology-stage records (always shown, labelled as stage-level) */
  stage: YFEvidenceRecord[];
  /** the measure whose concentration feeds the engine */
  engineMeasure: string | null;
  /** its label, e.g. "world exports of this sub-component" */
  engineMeasureLabel: string | null;
  /** the measure the home share is read from (the engine measure when it lists the country) */
  homeMeasure: string | null;
}

export function recordsFor(bundle: YFCountryBundle, scopeId: string): YFEvidenceRecord[] {
  return bundle.records.filter((r) => r.scopeId === scopeId);
}

function priority(ref: YFReference, scope: ScopeType): string[] {
  const raw = ref.enginePriority[scope] as string[] | string | undefined;
  return Array.isArray(raw) ? raw : raw ? [raw] : [];
}

function pickMeasure(ref: YFReference, scope: ScopeType, recs: YFEvidenceRecord[]): string | null {
  for (const m of priority(ref, scope)) if (recs.some((r) => r.measure === m && r.engineInput)) return m;
  return null;
}

/** A measure on which the home country has a numeric share (a source that does not list it is skipped). */
function pickHomeMeasure(ref: YFReference, scope: ScopeType, recs: YFEvidenceRecord[]): string | null {
  for (const m of priority(ref, scope))
    if (recs.some((r) => r.measure === m && r.engineInput === 'homeShare' && typeof r.rawValue === 'number')) return m;
  return null;
}

export function evidenceFor(subject: Subject, bundle: YFCountryBundle, ref: YFReference): EvidenceSet {
  const stageId = stageScopeId(subject.technology, subject.stage);
  const stage = recordsFor(bundle, stageId);
  const c = subject.component;
  const primary: ScopeRef | null = c ? c.evidence : { scope: 'stage', scopeId: stageId };
  const primaryRecords = primary ? (primary.scope === 'stage' ? stage : recordsFor(bundle, primary.scopeId)) : [];
  const secondary = (c?.secondary ?? []).map((s) => ({ ref: s, records: s.scope === 'stage' ? recordsFor(bundle, s.scopeId) : recordsFor(bundle, s.scopeId) }));
  const engineMeasure = primary ? pickMeasure(ref, primary.scope, primaryRecords) : null;
  const homeMeasure = primary ? pickHomeMeasure(ref, primary.scope, primaryRecords) : null;
  return { primary, primaryRecords, secondary, stage, engineMeasure, engineMeasureLabel: engineMeasure ? (ref.measures[engineMeasure]?.label ?? null) : null, homeMeasure };
}

// ── Engine inputs ────────────────────────────────────────────────────────────────────────────────

const DATA_INPUTS: NumericInputId[] = ['topSupplierShare', 'hhi', 'nonAlliedTopShare', 'alliedShare', 'homeShare'];

/** Which record fed which numeric input (for display beside the input). */
export type DataSources = Partial<Record<NumericInputId, YFEvidenceRecord>>;

export function dataInputs(ev: EvidenceSet): { values: Partial<Record<NumericInputId, InputValue<number>>>; sources: DataSources } {
  const values: Partial<Record<NumericInputId, InputValue<number>>> = {};
  const sources: DataSources = {};
  if (ev.primary && ev.engineMeasure) {
    const recs = ev.primaryRecords.filter((r) => r.measure === ev.engineMeasure);
    for (const id of DATA_INPUTS) {
      const r =
        id === 'homeShare'
          ? ev.primaryRecords.find((x) => x.measure === ev.homeMeasure && x.engineInput === 'homeShare')
          : recs.find((x) => x.engineInput === id);
      if (r && typeof r.rawValue === 'number') {
        values[id] = {
          value: r.rawValue,
          provenance: 'data',
          subject: r.subject ?? undefined,
          measure: r.measure && r.measure === ev.engineMeasure && ev.engineMeasureLabel ? `${ev.engineMeasureLabel} (${r.year})` : `${r.label.replace(/^.*? share of /, '')} (${r.year})`,
          note: `${r.label} · ${r.year} · ${r.source}`,
        };
        sources[id] = r;
      }
    }
    // concentration measured but nobody outside the coalition supplies: the largest such share is 0
    if (!values.nonAlliedTopShare && recs.some((r) => r.metricId === 'coalition_share'))
      values.nonAlliedTopShare = { value: 0, provenance: 'data', note: 'No supplier outside the coalition is recorded on this measure.' };
  }
  const trl = ev.stage.find((r) => r.metricId === 'trl');
  if (trl && typeof trl.rawValue === 'number') {
    values.trl = { value: trl.rawValue, provenance: 'data', note: `${trl.label} · stage-level · ${trl.year}` };
    sources.trl = trl;
  }
  return { values, sources };
}

export interface BuildOptions {
  country: string;
  withScenario: boolean;
}

/** The engine's Assessment for one subject. */
export function buildAssessment(subject: Subject, layer: LayerState, ev: EvidenceSet, opts: BuildOptions): Assessment {
  const inputs = emptyInputs() as unknown as Record<InputId, InputValue<string | number>>;
  const c = subject.component;
  // data
  const { values } = dataInputs(ev);
  for (const [id, v] of Object.entries(values)) inputs[id as InputId] = v as InputValue<string | number>;
  // curated assessments
  if (c) {
    const conf = c.confidence as Confidence;
    for (const [id, v] of Object.entries(c.assessments)) {
      if (v && v !== UNKNOWN) inputs[id as InputId] = { value: v, provenance: 'assessment', assessmentConfidence: conf, note: c.source };
    }
    if (c.shareOverride)
      inputs.nonAlliedTopShare = {
        value: c.shareOverride.value,
        provenance: 'assessment',
        assessmentConfidence: conf,
        subject: c.shareOverride.subject,
        note: 'Curated: trade codes cannot isolate this component. See the component notes.',
      };
  }
  const apply = (a: Record<string, string | number | undefined>, prov: Provenance) => {
    for (const [id, v] of Object.entries(a)) {
      if (v === undefined) continue;
      if (v === UNKNOWN) inputs[id as InputId] = { value: UNKNOWN, provenance: 'unknown' };
      // a reader's or scenario value is not about the data's supplier, measure or year
      else inputs[id as InputId] = { value: v, provenance: prov, note: prov === 'scenario' ? 'Scenario assumption' : undefined };
    }
  };
  apply(layer.answers, 'judgment');
  if (opts.withScenario) apply(layer.scenario, 'scenario');
  const context: AssessmentContext = {
    country: opts.country,
    technology: subject.technology,
    stage: subject.stage,
    componentId: c?.id,
    componentLabel: subject.label,
    layer: c ? c.layer : 'stage',
  };
  return {
    context,
    inputs: inputs as unknown as Inputs,
    strategies: layer.strategies,
    premiumJustification: layer.why,
  };
}

/** The value an input would have with no reader answer and no scenario: data or assessment. */
export function baselineValue(subject: Subject, ev: EvidenceSet, id: InputId): InputValue<string | number> {
  const a = buildAssessment(subject, { answers: {}, scenario: {}, strategies: [] }, ev, { country: '', withScenario: false });
  return a.inputs[id] as InputValue<string | number>;
}

export function isCategorical(id: InputId): id is CategoricalInputId {
  return !DATA_INPUTS.includes(id as NumericInputId) && id !== 'trl' && id !== 'premium';
}
