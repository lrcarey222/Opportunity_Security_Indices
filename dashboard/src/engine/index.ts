// Public API of the recompute engine (spec_react.md 6). Pure TypeScript with no dependencies and no
// IO: the worker (src/worker) fetches and inflates the data and calls these functions. The UI talks
// to the worker through EngineClient (src/worker/client.ts) and imports from here only types and the
// cheap pure helpers it needs on the main thread (dial checks, defaults, presets, table helpers).

export { ENGINE_VERSION } from './version.ts';

// Types of the bundle, the parameters and the results.
export type * from './types.ts';
export { DIAL_NAMES } from './types.ts';

// Numerics with R semantics.
export {
  NA_INT, NA_BOOL, EngineError, sumR, meanR, medianR, sdR, rankAvg, rankMin, medianScurve, roundR, fmtF, fmtG, formatR7, cmpC, orderBy,
  sortUniqueC, deepEqual, yieldNow,
} from './numeric.ts';

// Columnar tables.
export { table, num, int, str, lgl, has, take, takeRows, select, filterRows, bindRows, toRows, rowKeys, matchIdx } from './table.ts';

// The OSB container and the bundle.
export { decodeOsb, readOsb, isGzip, gunzipStream, isTable, isNDArray, unflatten } from './codec.ts';
export type { Column, OsbDecoded, OsbHeader } from './codec.ts';
export { SCHEMA, SCHEMA_VERSION, CHUNK_NAMES, assembleBundle, addChunk, parseBaselineInputs } from './bundle.ts';
export type { ChunkName } from './bundle.ts';

// Dials and parameters.
export {
  DIAL_CODES, dialMultiplier, stageWeights, defaultDials, checkDials, clampDials, defaultAdvanced, checkAdvanced, makeParams, isBaseline,
  paramsLayer, paramsKey, paramsToRJson, dialReadout, presets, paramsSpec, advancedDiff,
} from './params.ts';
export type { Layer } from './params.ts';

// L1-L4.
export { pillars, variableContrib } from './pillars.ts';
export { policy } from './policy.ts';
export { si, alliedSelection } from './si.ts';
export { fsCells, poCells, keepCells, partnerTop5, psi, partnerDecompose } from './partner.ts';

// Orchestration, comparisons, download tables, cache.
export {
  run, runPartner, compare, compareDyads, scenarioTables, paramsLong, layerKeys, resultNotes, joinBase, LayerCache,
} from './engine.ts';
export type { DashResult, PartnerResult, CompareResult, CacheLayer } from './engine.ts';

// L5: the allied network (greedy solver).
export {
  networkStages, networkInputs, networkScore, networkSolve, networkCompare, networkConcentration, buildTopkTbl, countTopk, SHARE_RANK_DIGITS,
  stageKey,
} from './network/index.ts';
export type { NetInputs, NetScored, NetworkSolution, SolveProgress, SolveOptions, Stage } from './network/index.ts';
export type { Portfolio as NetworkPortfolio } from './network/solve.ts';

// Self-check against the shipped baseline tables.
export { selfCheckCore, selfCheckPartner, selfCheckNetwork } from './selfcheck.ts';
export type { SelfCheck } from './selfcheck.ts';
