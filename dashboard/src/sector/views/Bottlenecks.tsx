// "Where are the real bottlenecks?" (external review readiness, 2026-11-23): the answer that opens a
// technology's chain page (/sector/:tech). Built from data the page already has, no new measure:
//   * the Allied Industrial Base nodes (the AIB document ChainNav already loads) whose codes sit in one of the
//     technology's six-stage cells (node.yfCells), kept when the stress test says they need action (Build
//     capacity, Manage specialization, Coordinate trade), largest remaining gap first (node.residual, then
//     node.exposure), at most five. Each names today's largest supplier to allied markets
//     (node.baseLargest / baseLargestShare: observed trade before any shock; never node.hhi or
//     node.largestShare, which describe the optimized post-shock network) and says why it matters from its
//     class and its stress-test figures;
//   * without such a node, the chain ladder's most concentrated measures, labelled as concentration only.
// Each line links to the stage profile. The derivation (techBottlenecks, bottleneckWhy) is pure.

import { Link } from 'react-router';
import { ClassSwatch } from '../../aib/charts/ClassChip.tsx';
import type { AibDoc, AibNode } from '../../aib/contract.ts';
import { useAibManifest, useCompactsDoc } from '../../aib/data/hooks.ts';
import { AIB_CLASS_NAME, SCENARIO_NAME } from '../../content/vocabulary.ts';
import { STAGE_LABELS, STAGE_SLUGS, TECH_LABELS, type StageSlug, type TechSlug } from '../contract.ts';
import { BOTTLENECKS } from '../copy.ts';
import { fmtShare, lc, listText } from '../format.ts';
import { useSectorUrl } from './state.tsx';

/** The stress-test classes that call for action, the ones a bottleneck list shows. */
export const ACTION_CLASSES: ReadonlySet<string> = new Set(['BUILD', 'MANAGE SPECIALIZATION', 'COORDINATE']);
/** At most this many steps are listed. */
export const BOTTLENECK_LIMIT = 5;

export interface Bottleneck {
  node: AibNode;
  /** the stage of the technology's cell the node's codes sit in (its first such cell: the largest share of its trade) */
  stage: StageSlug;
}

export interface TechBottlenecks {
  /** the technology's AIB nodes (each once) */
  assessed: AibNode[];
  /** the nodes that need action, largest remaining gap first (all of them; the view shows BOTTLENECK_LIMIT) */
  rows: Bottleneck[];
}

/** The stage of the technology's first cell in a node's cells, or null when none of its cells is the technology's. */
export function nodeStageIn(n: AibNode, tech: TechSlug): StageSlug | null {
  for (const c of n.yfCells) for (const s of STAGE_SLUGS) if (c === `${tech}-${s}`) return s;
  return null;
}

const num = (x: number | null | undefined): number => (typeof x === 'number' && Number.isFinite(x) ? x : -1);

/** The technology's AIB nodes and those that need action, sorted by remaining gap, then supply exposed, then label. */
export function techBottlenecks(nodes: readonly AibNode[], tech: TechSlug): TechBottlenecks {
  const assessed: AibNode[] = [];
  const rows: Bottleneck[] = [];
  for (const n of nodes) {
    const stage = nodeStageIn(n, tech);
    if (!stage) continue;
    assessed.push(n);
    if (ACTION_CLASSES.has(n.cls)) rows.push({ node: n, stage });
  }
  rows.sort((a, z) => num(z.node.residual) - num(a.node.residual) || num(z.node.exposure) - num(a.node.exposure) || a.node.label.localeCompare(z.node.label, 'en'));
  return { assessed, rows };
}

const pct = (x: number | null | undefined): string | null => (typeof x === 'number' && Number.isFinite(x) ? fmtShare(x, 0) : null);

/**
 * Why a step matters, in plain words, from its class and its stress-test figures (shares of allied demand):
 * "Build capacity: a major-supplier disruption would cut off 70% of allied supply, and even coordinated
 * trade would leave a 69% gap." A figure the node does not have is left out, never guessed.
 */
export function bottleneckWhy(n: AibNode, supplierName: string | null): string {
  const cls = AIB_CLASS_NAME[n.cls as keyof typeof AIB_CLASS_NAME] ?? n.cls;
  const x = pct(n.exposure);
  const r = pct(n.residual);
  const d = pct(n.dividend);
  const cut = x ? `a ${lc(SCENARIO_NAME.decoupling)} would cut off ${x} of allied supply` : null;
  const parts: string[] = [];
  if (n.cls === 'BUILD') {
    if (cut) parts.push(cut);
    if (r) parts.push(`even coordinated trade would leave a ${r} gap`);
    return `${cls}: ${parts.join(', and ') || 'trade alone cannot close the gap'}.`;
  }
  if (n.cls === 'COORDINATE') {
    if (cut) parts.push(cut);
    if (d) parts.push(`acting together would recover an extra ${d}${r && (n.residual ?? 0) > 0 ? `, leaving a ${r} gap` : ''}`);
    return `${cls}: ${parts.join('; ') || 'existing suppliers can cover the need if allies coordinate'}.`;
  }
  // MANAGE SPECIALIZATION: the specialization is kept, the concentration is the risk
  const share = pct(n.baseLargestShare);
  if (share) parts.push(`${supplierName ?? 'one supplier'} supplies ${share} of allied demand today`);
  if (cut) parts.push(`${cut}${r && (n.residual ?? 0) > 0 ? `, leaving a ${r} gap even with coordinated trade` : ''}`);
  return `${cls}: ${parts.join('; ') || 'too much supply sits with one supplier'}.`;
}

/** A row of the concentration-only fallback (from the chain ladder). */
export interface ConcentrationLine {
  id: string;
  /** "Refining exports" */
  label: string;
  stage: StageSlug;
  top1Name: string | null;
  top1Share: number | null;
  china: number;
  href?: string;
}

/** The three most concentrated ladder rows: the largest single holder's share first, then China's. */
export function topConcentration(lines: readonly ConcentrationLine[], n = 3): ConcentrationLine[] {
  return [...lines].sort((a, z) => num(z.top1Share) - num(a.top1Share) || z.china - a.china).slice(0, n);
}

/** The class counts of nodes as words: "5 Low intervention and 1 Not enough evidence". */
function classCountWords(nodes: readonly AibNode[]): string {
  const by = new Map<string, number>();
  for (const n of nodes) by.set(n.cls, (by.get(n.cls) ?? 0) + 1);
  return listText([...by.entries()].sort((a, z) => z[1] - a[1]).map(([c, k]) => `${k} ${AIB_CLASS_NAME[c as keyof typeof AIB_CLASS_NAME] ?? c}`));
}

function ConcentrationList({ lines, tech }: { lines: ConcentrationLine[]; tech: TechSlug }) {
  const { link } = useSectorUrl();
  if (!lines.length) return null;
  return (
    <>
      <p className="mt-2 text-sm text-paper-mute">{BOTTLENECKS.concentrationOnly}</p>
      <ol className="mt-1.5 space-y-1 text-sm" data-testid="bottlenecks-concentration">
        {lines.map((l) => (
          <li key={l.id}>
            <Link className="link" to={l.href ?? link(`/sector/${tech}/${l.stage}`)}>
              {l.label}
            </Link>
            <span className="text-paper-dim">
              {l.top1Name ? `: largest holder ${l.top1Name} ${fmtShare(l.top1Share)}` : ''}
              {l.top1Name !== 'China' && l.china > 0.005 ? `${l.top1Name ? '; ' : ': '}China ${fmtShare(l.china)}` : ''}
            </span>
          </li>
        ))}
      </ol>
    </>
  );
}

function AibList({ doc, tech, fallback }: { doc: AibDoc; tech: TechSlug; fallback: ConcentrationLine[] }) {
  const { link } = useSectorUrl();
  const label = TECH_LABELS[tech];
  const { assessed, rows } = techBottlenecks(doc.nodes, tech);
  const shock = doc.meta.shocks.find((s) => s.id === doc.meta.headlineShock)?.label ?? null;
  const test = `the Allied Industrial Base stress test${shock ? ` (${lc(shock)})` : ''}`;
  if (!assessed.length) {
    return (
      <>
        <p className="mt-1 text-sm text-paper-dim">{`${test.charAt(0).toUpperCase()}${test.slice(1)} covers no ${label} supply-chain step.`}</p>
        <ConcentrationList lines={topConcentration(fallback)} tech={tech} />
      </>
    );
  }
  if (!rows.length) {
    return (
      <>
        <p className="mt-1 text-sm text-paper-dim">
          {`None of the ${assessed.length} ${label} supply-chain steps in ${test} needs new capacity or coordinated trade (${classCountWords(assessed)}).`}
        </p>
        <ConcentrationList lines={topConcentration(fallback)} tech={tech} />
      </>
    );
  }
  const shown = rows.slice(0, BOTTLENECK_LIMIT);
  return (
    <>
      <p className="mt-1 text-sm text-paper-dim">
        {`${rows.length} of the ${assessed.length} ${label} supply-chain steps in ${test} need action. ${shown.length < rows.length ? `The ${shown.length} with the largest remaining supply gap:` : 'Largest remaining supply gap first:'}`}
      </p>
      <ol className="mt-2 space-y-2.5" data-testid="bottlenecks-list">
        {shown.map(({ node: n, stage }) => {
          const supplier = n.baseLargest ? (doc.countries[n.baseLargest] ?? n.baseLargest) : null;
          return (
            <li key={n.id} className="text-sm">
              <div className="flex flex-wrap items-baseline gap-x-2">
                <span className="self-center" aria-hidden="true">
                  <ClassSwatch cls={n.cls} size={9} />
                </span>
                <Link className="link font-medium" to={link(`/sector/${tech}/${stage}`)}>
                  {n.label}
                </Link>
                <span className="text-xs text-paper-mute">{STAGE_LABELS[stage]}</span>
                {supplier && pct(n.baseLargestShare) && <span className="text-xs text-paper-mute">{`· largest supplier today: ${supplier} ${pct(n.baseLargestShare)}`}</span>}
              </div>
              <p className="mt-0.5 text-paper-dim">{bottleneckWhy(n, supplier)}</p>
            </li>
          );
        })}
      </ol>
    </>
  );
}

/** The bottleneck block: the stress test's steps when the AIB document loads, the concentration-only list otherwise. */
export function Bottlenecks({ tech, fallback }: { tech: TechSlug; fallback: ConcentrationLine[] }) {
  const manifest = useAibManifest();
  const doc = useCompactsDoc(manifest.data);
  const failed = manifest.status === 'error' || doc.status === 'error';
  return (
    <section aria-labelledby="bottlenecks-h" className="panel p-4" data-testid="bottlenecks">
      <h2 id="bottlenecks-h" className="text-base font-medium text-paper">
        {BOTTLENECKS.title}
      </h2>
      {doc.data ? (
        <AibList doc={doc.data} tech={tech} fallback={fallback} />
      ) : failed ? (
        <ConcentrationList lines={topConcentration(fallback)} tech={tech} />
      ) : (
        <p className="mt-1 text-sm text-paper-mute">{BOTTLENECKS.loading}</p>
      )}
    </section>
  );
}
