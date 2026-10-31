// The shared node selection of the Country page (design D8, spec XIX): one technology x six-stage node,
// country x technology x stage, held in the URL as ?node=<Tech>|<Stage> so a node is shareable. Every entry
// point (Strategy Map cell, Need ranking row, Prize & Position dot, Strategic Choice card, Policy Alignment
// row, Allied Strategy node) selects through it and opens the same drawer. A value is checked against the
// data: a technology of meta.techs and one of the six stages (by id, or by display name "Refining &
// Processing"); an old three-stage stage is mapped when it became one stage (Downstream -> Deployment) and
// reported otherwise. A value that is not used as written is reported as ignored ("node=Wind|Refining (Wind
// is not a technology of the posture data; ignored)"), like the Sector page's ?pnode. parseNodeParam is pure;
// useNodeSelection reads and writes the URL (react-router).

import { useCallback, useMemo } from 'react';
import { useSearchParams } from 'react-router';
import { isLegacyStage, legacyStageCorresponds, legacyStageTargetName, stageDisplay, stageOfDisplay } from '../lib/stages.ts';
import type { PostureMeta } from './contract.ts';

/** The URL key of the selected node. */
export const NODE_KEY = 'node';
/**
 * The URL key of a layer of the selected node (?layer=<layer_id>, 2026-10-30): a fence section's layer unit
 * opens its node's drawer focused on that layer. Read only beside a node; a layer the node does not have is
 * left unfocused.
 */
export const LAYER_KEY = 'layer';

export interface NodeParse {
  /** the selected cell key "Tech|Stage" (stage id), or null for none */
  key: string | null;
  /** "node=Wind|Refining (why; ignored)" when the value was not used as written; null when it was (or there was none) */
  problem: string | null;
}

/** The node of a ?node= value, checked against the data. */
export function parseNodeParam(raw: string | null, meta: Pick<PostureMeta, 'techs' | 'stages'>, key = NODE_KEY): NodeParse {
  if (raw === null || raw.trim() === '') return { key: null, problem: null };
  const ignored = (why: string): NodeParse => ({ key: null, problem: `${key}=${raw} (${why}; ignored)` });
  const parts = raw.split('|').map((x) => x.trim());
  if (parts.length !== 2) return ignored('not a technology|stage pair');
  const [tech, stageIn] = parts;
  if (!meta.techs.includes(tech)) return ignored(`${tech || 'an empty name'} is not a technology of the posture data`);
  if (isLegacyStage(stageIn)) {
    const exact = legacyStageTargetName(stageIn);
    if (exact && meta.stages.includes(exact)) {
      return { key: `${tech}|${exact}`, problem: `${key}=${raw} (${stageIn} corresponds to ${stageDisplay(exact)}; showing ${tech} — ${stageDisplay(exact)})` };
    }
    return ignored(`${stageIn} ${legacyStageCorresponds(stageIn)}`);
  }
  const stage = stageOfDisplay(stageIn);
  if (!stage || !meta.stages.includes(stage)) return ignored(`${stageIn || 'an empty name'} is not a supply-chain stage`);
  return { key: `${tech}|${stage}`, problem: null };
}

export interface NodeSelection {
  /** the selected cell key, checked; null while the data load, with no value, or with an ignored one */
  selected: string | null;
  /** the raw URL value */
  raw: string | null;
  /** why the URL value was not used as written (show it as "Ignored in the link"); null otherwise */
  problem: string | null;
  /** the layer of the selected node to focus in the drawer (?layer=), null for the node itself or with no node */
  layer: string | null;
  /** select a node (null: none), optionally focused on one of its layers, replacing the URL entry and keeping the scroll */
  select: (key: string | null, layer?: string | null) => void;
  /** select a node, or clear it when it is already selected */
  toggle: (key: string) => void;
}

/**
 * The page's node selection in ?node=. `meta` is the posture core's (null while it loads: nothing is
 * selected, nothing reported). Writing a key the data does not know is the caller's bug; the next read
 * reports it as ignored.
 */
export function useNodeSelection(meta: Pick<PostureMeta, 'techs' | 'stages'> | null, key = NODE_KEY): NodeSelection {
  const [sp, setSp] = useSearchParams();
  const raw = sp.get(key);
  const parsed = useMemo(() => (meta ? parseNodeParam(raw, meta, key) : { key: null, problem: null }), [meta, raw, key]);
  const select = useCallback(
    (k: string | null, layer?: string | null) => {
      setSp(
        (prev) => {
          const q = new URLSearchParams(prev);
          if (k === null) q.delete(key);
          else q.set(key, k);
          if (k !== null && layer) q.set(LAYER_KEY, layer);
          else q.delete(LAYER_KEY);
          return q;
        },
        { replace: true, preventScrollReset: true },
      );
    },
    [setSp, key],
  );
  const selected = parsed.key;
  const rawLayer = sp.get(LAYER_KEY);
  const layer = selected && rawLayer && rawLayer.trim() ? rawLayer.trim() : null;
  const toggle = useCallback((k: string) => select(selected === k ? null : k), [select, selected]);
  return { selected, raw, problem: parsed.problem, layer, select, toggle };
}
