// The supply-chain navigator (IA refactor 2026-10-16, brief s.7): Upstream -> Midstream -> Downstream
// for one technology, each stage with its trade sub-components (the groups behind the Trade section's
// "View by: Sub-component", ../derive/subs.ts) and a mark for the Allied Industrial Base node(s) of the
// same component (../derive/chain.ts). A stage opens its Sector profile; a component opens the profile
// scoped to it (the existing `sub` URL key). Folded (the three stages in one row, the components behind
// "Show components"): on phones, and on the Sector profile at every width (`collapsible`), so the
// profile's four lenses stay on the first screen.

import { useMemo, useState } from 'react';
import { Link } from 'react-router';
import { ClassSwatch } from '../../aib/charts/ClassChip.tsx';
import { classInfo } from '../../aib/derive/classes.ts';
import { useMediaQuery } from '../../shell/useMediaQuery.ts';
import { STAGE_COLOR } from '../../theme/palette.ts';
import { STAGE_LABELS, STAGE_SLUGS, TECH_LABELS, type StageSlug, type TechSlug } from '../contract.ts';
import { useModel } from '../data/SectorDataProvider.tsx';
import { matchNodes, nodeNamedApart } from '../derive/chain.ts';
import { subComponents, type SubComponent } from '../derive/subs.ts';
import { isDictionary, type AibNode } from '../../aib/contract.ts';
import { joinLabels } from '../../aib/derive/format.ts';
import { fmtShare } from '../format.ts';
import { useAibDoc } from './aibData.ts';
import { useTrade } from './data.ts';
import { useSectorUrl } from './state.tsx';

interface StageCol {
  stage: StageSlug;
  exists: boolean;
  hasTrade: boolean;
  subs: SubComponent[];
  nodes: Map<string, AibNode[]>;
  unmatched: AibNode[];
}

function NodeMarks({ nodes, sub }: { nodes: AibNode[]; sub: string }) {
  if (!nodes.length) return null;
  const apart = nodes.filter((n) => nodeNamedApart(n, sub));
  return (
    <>
      <span className="ml-1.5 inline-flex gap-0.5 align-middle">
        {nodes.map((n) => (
          <span key={n.id} title={`${n.label}: ${classInfo(n.cls).label}`}>
            <ClassSwatch cls={n.cls} size={8} />
            <span className="sr-only">{`${n.label}: ${classInfo(n.cls).label}`}</span>
          </span>
        ))}
      </span>
      {apart.length > 0 && <span className="block text-xs text-paper-mute">Allied Industrial Base nodes: {joinLabels(apart.map((n) => n.label))}</span>}
    </>
  );
}

export function ChainNav({ tech, stage = null, sub = null, collapsible = false }: { tech: TechSlug; stage?: StageSlug | null; sub?: string | null; collapsible?: boolean }) {
  const model = useModel();
  const { link } = useSectorUrl();
  const md = useMediaQuery('(min-width: 768px)');
  const [open, setOpen] = useState(false);
  const anyTrade = STAGE_SLUGS.some((s) => model.cell(tech, s)?.has_trade);
  const { trade } = useTrade(tech, anyTrade);
  const doc = useAibDoc();
  const cols = useMemo<StageCol[]>(
    () =>
      STAGE_SLUGS.map((s) => {
        const cell = model.cell(tech, s);
        const subs = cell && cell.has_trade && trade ? subComponents(trade, model, cell.idx) : [];
        const m = cell && doc ? matchNodes(subs.map((x) => x.name), doc.nodes, cell.slug) : { bySub: new Map<string, AibNode[]>(), unmatched: [] };
        return { stage: s, exists: !!cell, hasTrade: !!cell?.has_trade, subs, nodes: m.bySub, unmatched: m.unmatched };
      }),
    [model, tech, trade, doc],
  );
  const anyNodes = cols.some((c) => c.unmatched.length || [...c.nodes.values()].some((n) => n.length));
  const dictionary = !!doc && isDictionary(doc.meta);
  // a sub-component the stage does not have (or before the trade chunk loads) scopes nothing
  if (sub && !cols.find((c) => c.stage === stage)?.subs.some((x) => x.name === sub)) sub = null;
  const foldable = collapsible || !md;
  const folded = foldable && !open;
  return (
    <nav aria-label={`${TECH_LABELS[tech]} supply chain`} className="panel p-4" data-testid="chain-nav">
      <div className="flex flex-wrap items-baseline justify-between gap-2">
        <div className="eyebrow">{TECH_LABELS[tech]} supply chain{folded ? '' : ': pick a stage or a component'}</div>
        {foldable && (
          <button type="button" className="btn-quiet text-sm" aria-expanded={open} onClick={() => setOpen((o) => !o)}>
            {open ? 'Hide components' : 'Show components'}
          </button>
        )}
      </div>
      {folded ? (
        <div className="mt-2 flex flex-wrap items-center gap-2 text-sm">
          {cols.map((c, i) => {
            const cur = c.stage === stage;
            return (
              <span key={c.stage} className="flex items-center gap-2">
                {i > 0 && (
                  <span className="text-paper-mute" aria-hidden="true">
                    →
                  </span>
                )}
                {c.exists ? (
                  <Link to={link(`/sector/${tech}/${c.stage}`)} className="tag" aria-current={cur && !sub ? 'page' : undefined} style={{ borderLeft: `3px solid ${STAGE_COLOR[STAGE_LABELS[c.stage]]}` }}>
                    {STAGE_LABELS[c.stage]}
                  </Link>
                ) : (
                  <span className="tag opacity-50" aria-disabled="true" title={`${TECH_LABELS[tech]} has no ${STAGE_LABELS[c.stage]} sector`}>
                    {STAGE_LABELS[c.stage]}
                  </span>
                )}
              </span>
            );
          })}
          {sub && stage && (
            <span className="flex items-center gap-2">
              <span className="text-paper-mute" aria-hidden="true">
                ›
              </span>
              <span className="font-medium text-paper" aria-current="page">
                {sub}
              </span>
            </span>
          )}
        </div>
      ) : (
        <ol className="mt-3 grid gap-4 md:grid-cols-3 md:gap-6">
          {cols.map((c, i) => {
            const cur = c.stage === stage;
            return (
              <li key={c.stage} className="relative min-w-0 border-t-[3px] pt-2" style={{ borderTopColor: STAGE_COLOR[STAGE_LABELS[c.stage]] }}>
                {i > 0 && (
                  <span aria-hidden="true" className="absolute -left-5 top-1 hidden text-paper-mute md:block">
                    →
                  </span>
                )}
                {c.exists ? (
                  <Link to={link(`/sector/${tech}/${c.stage}`)} aria-current={cur && !sub ? 'page' : undefined} className={`text-sm font-medium ${cur && !sub ? 'text-signal' : 'text-paper hover:text-signal'}`}>
                    {STAGE_LABELS[c.stage]}
                    {cur && !sub ? <span className="sr-only"> (shown)</span> : null}
                  </Link>
                ) : (
                  <span className="text-sm text-paper-mute" title={`${TECH_LABELS[tech]} has no ${STAGE_LABELS[c.stage]} sector`}>
                    {STAGE_LABELS[c.stage]} (no data)
                  </span>
                )}
                <ul className="mt-1.5 space-y-1 text-sm">
                  {c.subs.map((s) => {
                    const here = cur && sub === s.name;
                    return (
                      <li key={s.name} className="min-w-0">
                        <Link to={link(`/sector/${tech}/${c.stage}`, { sub: s.name })} aria-current={here ? 'page' : undefined} className={here ? 'font-medium text-signal' : 'text-paper-dim hover:text-paper'}>
                          {s.name}
                        </Link>
                        {Number.isFinite(s.share) && <span className="tabular ml-1.5 text-xs text-paper-mute">{fmtShare(s.share)}</span>}
                        <NodeMarks nodes={c.nodes.get(s.name) ?? []} sub={s.name} />
                      </li>
                    );
                  })}
                  {c.exists && !c.subs.length && (
                    <li className="text-xs text-paper-mute">{c.hasTrade ? (trade ? 'One trade basket, no components' : 'Loading the components…') : 'No traded components (deployment and use)'}</li>
                  )}
                  {c.unmatched.length > 0 && <li className="text-xs text-paper-mute">Also modelled: {joinLabels(c.unmatched.map((n) => n.label))}</li>}
                </ul>
              </li>
            );
          })}
        </ol>
      )}
      {!folded && (
        <p className="mt-3 text-xs text-paper-mute">
          Percentages: each component’s share of the stage’s world exports.
          {anyNodes && (
            <>
              {' '}Marks: the class the <Link className="link" to="/aib">Allied Industrial Base</Link> model gives the matching supply-chain node (shown under Energy Security)
              {dictionary ? ': a node of the electro-industrial dictionary whose codes sit in the component; one node can sit under several components and stages.' : '.'}
            </>
          )}
        </p>
      )}
    </nav>
  );
}
