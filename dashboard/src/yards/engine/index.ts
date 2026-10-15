// Yards & Fences decision engine: public API. Pure functions only; see types.ts for the model.

export * from './types.ts';
export { RULES } from './rules.ts';
export { INPUT_DEFS, ALL_INPUT_IDS, CATEGORICAL_IDS, emptyInputs, formatNumber, isNumericId, optionLabel, PROVENANCE_LABEL } from './inputs.ts';
export type { GateKey, InputDef } from './inputs.ts';
export { and3, or3, evaluateGates, evaluateLargeYard, evaluateMediumYard, evaluateSmallYard } from './gates.ts';
export { calculateEvidenceCompleteness, classify, generateDecisionPath, possibleYards, withValue, YARD_NAME } from './classify.ts';
export { diagnosedFence, evaluatePolicyFit, needsFor, POLICIES, POLICY_BY_ID, successMetrics } from './policy.ts';
export type { FenceShape, FindingKind, Intensity, Need, PolicyDef, PolicyFinding, PolicyFit, PolicyId, PolicyPackage, Purpose } from './policy.ts';
export { evaluateStack, explainTransition, stackOutcome } from './stack.ts';
export type { StackLayerResult, StackResult, Transition } from './stack.ts';
