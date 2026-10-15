// Draw the Fence: the module context. The assessment lives in the URL (`a`); this provider decodes
// it, exposes setters that write it back, and memoises engine runs per layer.

import { createContext, useCallback, useContext, useMemo, type ReactNode } from 'react';
import { useNavigate, useSearchParams } from 'react-router';
import { classify } from '../engine/index.ts';
import type { Assessment, YardResult } from '../engine/index.ts';
import { buildAssessment, componentById, componentSubject, evidenceFor, stackById, stageSubject, type EvidenceSet, type Subject } from '../data/adapter.ts';
import type { YFData } from '../data/store.ts';
import { decodeState, encodeState, layerOf, stateSearch, type AssessState } from './assessment.ts';

export interface Evaluation {
  subject: Subject;
  evidence: EvidenceSet;
  baseline: Assessment;
  baselineResult: YardResult;
  scenario: Assessment;
  scenarioResult: YardResult;
  hasScenario: boolean;
}

interface Ctx {
  data: YFData;
  state: AssessState;
  update: (f: (s: AssessState) => AssessState, opts?: { push?: boolean }) => void;
  go: (view: string, next?: AssessState) => void;
  href: (view: string, next?: AssessState) => string;
  subjects: Subject[];
  focus: Subject | null;
  evaluate: (subject: Subject) => Evaluation;
}

const YardsContext = createContext<Ctx | null>(null);

export function useYards(): Ctx {
  const c = useContext(YardsContext);
  if (!c) throw new Error('useYards() outside YardsProvider');
  return c;
}

/** The subjects (layers) a selection covers: a system's layers, one component, or the whole stage. */
export function subjectsOf(state: AssessState, data: YFData): Subject[] {
  const sel = state.sel;
  if (!sel) return [];
  const ref = data.reference;
  const stack = stackById(ref, sel.stack);
  if (stack) return stack.layers.map((id) => componentById(ref, id)).filter((c): c is NonNullable<typeof c> => !!c).map(componentSubject);
  const comp = componentById(ref, sel.component);
  if (comp) return [componentSubject(comp)];
  return [stageSubject(sel.tech, sel.stage)];
}

export function YardsProvider({ data, children }: { data: YFData; children: ReactNode }) {
  const [params, setParams] = useSearchParams();
  const navigate = useNavigate();
  const raw = params.get('a');
  const state = useMemo(() => decodeState(raw), [raw]);

  const update = useCallback(
    (f: (s: AssessState) => AssessState, opts?: { push?: boolean }) => {
      // read the router's current params (not window.location), so a memory router behaves the same
      setParams((p) => {
        const q = new URLSearchParams(p);
        q.set('a', encodeState(f(decodeState(p.get('a')))));
        return q;
      }, { replace: !opts?.push, preventScrollReset: true });
    },
    [setParams],
  );

  const href = useCallback((view: string, next?: AssessState) => `/yards-fences${view ? `/${view}` : ''}${stateSearch(next ?? state)}`, [state]);
  const go = useCallback(
    (view: string, next?: AssessState) => {
      navigate(href(view, next));
      window.scrollTo?.({ top: 0 });
    },
    [navigate, href],
  );

  const subjects = useMemo(() => subjectsOf(state, data), [state, data]);
  const focus = useMemo(() => subjects.find((s) => s.key === state.focus) ?? subjects[0] ?? null, [subjects, state.focus]);

  const evaluate = useCallback(
    (subject: Subject): Evaluation => {
      const layer = layerOf(state, subject.key);
      const evidence = evidenceFor(subject, data.bundle, data.reference);
      const opts = { country: data.bundle.country.name };
      const baseline = buildAssessment(subject, layer, evidence, { ...opts, withScenario: false });
      const scenario = buildAssessment(subject, layer, evidence, { ...opts, withScenario: true });
      const hasScenario = Object.keys(layer.scenario).length > 0;
      const baselineResult = classify(baseline);
      return { subject, evidence, baseline, baselineResult, scenario, scenarioResult: hasScenario ? classify(scenario) : baselineResult, hasScenario };
    },
    [state, data],
  );

  const value = useMemo<Ctx>(() => ({ data, state, update, go, href, subjects, focus, evaluate }), [data, state, update, go, href, subjects, focus, evaluate]);
  return <YardsContext.Provider value={value}>{children}</YardsContext.Provider>;
}

