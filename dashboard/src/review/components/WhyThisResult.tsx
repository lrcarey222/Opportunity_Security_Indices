// "Why this result?" (docs/QC_SYSTEM_DESIGN.md 5): the provenance chain of a posture cell, from the strategic
// conclusion to the decision trace (value, bar, margin, result), the component scores, their families, the raw
// values the release ships, the transformation (config recipe) and the source with its vintage and licence.
// Built by review/qc/provenance.ts provenanceChain; this component only lays it out. Every link states its
// status in words ("shipped", "from configuration", "not in this release"): a gap is shown, never hidden.
// Collapsed by default (no complexity in the default view); the chain builder (review/qc/provenance.ts) and the
// source dictionary (the Sector bundle's dict.sources, the same ids as config/sector_dictionary.yml) are loaded
// only when it is opened, so the drawer's own code stays small.

import { useEffect, useMemo, useRef, useState, type ReactNode } from 'react';
import type { PostureCore } from '../../posture/contract.ts';
import type { NodeDetail } from '../../posture/detail.ts';
import { CHOICE_NAME } from '../../content/vocabulary.ts';
import { POSTURE_LABEL, traceCodeWords, traceTestWords, VERB_ORDER } from '../../posture/grammar.ts';
import type { DictionarySource, LinkLevel, LinkStatus, ProvenanceChain, ProvenanceDictionary, ProvenanceLink } from '../qc/provenance.ts';

type ChainBuilder = (typeof import('../qc/provenance.ts'))['provenanceChain'];

export const LINK_STATUS_LABEL: Readonly<Record<LinkStatus, string>> = {
  shipped: 'shipped',
  config_only: 'from configuration',
  not_in_release: 'not in this release',
};

export const LINK_LEVEL_LABEL: Readonly<Record<LinkLevel, string>> = {
  choice: 'Strategic conclusion',
  trace: 'Decision step',
  component: 'Component score',
  family: 'Indicator',
  raw: 'Raw value',
  transform: 'Transformation',
  source: 'Source',
};

/** The source dictionary from the Sector bundle's meta chunk (dict.sources). */
export async function sectorSourceDictionary(): Promise<ProvenanceDictionary> {
  const { getSectorStore } = await import('../../sector/data/store.ts');
  const meta = await getSectorStore().load('sector-meta');
  const c = meta['dict.sources'].cols;
  const out: Record<string, DictionarySource> = {};
  const s = (v: string | null | undefined): string | null => (typeof v === 'string' && v !== '' ? v : null);
  for (let i = 0; i < meta['dict.sources'].nrow; i++) {
    const id = c.id[i];
    if (id) out[id] = { id, short: s(c.short[i]), edition: s(c.edition[i]), licence: s(c.licence[i]), accessed: s(c.accessed[i]), transform: s(c.transform[i]) };
  }
  return { sources: out };
}

const fmt = (v: number): string => (Math.abs(v) >= 1000 ? Math.round(v).toLocaleString('en-US') : Math.abs(v) > 0 && Math.abs(v) < 0.001 ? v.toExponential(2) : String(Number(v.toPrecision(3))));

function valueWords(l: ProvenanceLink): string | null {
  if (l.value === null || l.value === undefined) return null;
  if (l.level === 'choice' && typeof l.value === 'string') return (VERB_ORDER as string[]).includes(l.value) ? CHOICE_NAME[l.value as keyof typeof CHOICE_NAME] : l.value === 'not_assessed' ? POSTURE_LABEL.N : l.value;
  if (typeof l.value === 'number') return fmt(l.value);
  return l.level === 'trace' ? traceCodeWords(l.value) : l.value;
}

/** value - threshold, when both are numbers (the distance from the bar). */
export function linkMargin(l: Pick<ProvenanceLink, 'value' | 'threshold'>): number | null {
  return typeof l.value === 'number' && typeof l.threshold === 'number' ? l.value - l.threshold : null;
}

function Status({ s }: { s: LinkStatus }) {
  return (
    <span className="tag ml-1.5 text-[0.6875rem]" data-status={s} style={s === 'shipped' ? undefined : { borderColor: 'var(--color-signal)', color: 'var(--color-paper)' }}>
      {LINK_STATUS_LABEL[s]}
    </span>
  );
}

function Link_({ l, kids, depth, sourcesState }: { l: ProvenanceLink; kids: Map<string, ProvenanceLink[]>; depth: number; sourcesState: 'idle' | 'loading' | 'ready' | 'error' }) {
  const v = valueWords(l);
  const m = linkMargin(l);
  const label = l.level === 'trace' ? traceTestWords(l.label) : l.label;
  const children = kids.get(l.id) ?? [];
  const sourceText = l.source ?? (l.level === 'raw' || l.level === 'family' || l.level === 'component' ? (sourcesState === 'ready' ? null : sourcesState === 'error' ? 'source names could not be loaded' : null) : null);
  const facts: ReactNode[] = [];
  if (l.threshold !== undefined && l.threshold !== null) facts.push(`bar ${typeof l.threshold === 'number' ? fmt(l.threshold) : l.threshold}`);
  if (m !== null) facts.push(`margin ${m >= 0 ? '+' : ''}${fmt(m)}`);
  if (l.result) facts.push(`→ ${traceCodeWords(l.result)}`);
  return (
    <li className={depth ? 'mt-1 border-l hairline pl-3' : 'mt-1'} data-link={l.id} data-level={l.level}>
      <div className="text-xs">
        <span className="text-paper-mute">{LINK_LEVEL_LABEL[l.level]}: </span>
        <span className="text-paper-dim">{label}</span>
        {v !== null && <span className="tabular text-paper">{` ${v}`}</span>}
        {facts.length > 0 && <span className="tabular text-paper-mute"> ({facts.join(', ')})</span>}
        <Status s={l.status} />
      </div>
      {(l.text || l.config_ref || sourceText || l.vintage || l.licence) && (
        <div className="mt-0.5 text-[0.6875rem] leading-snug text-paper-mute">
          {l.text && <p>{l.text}</p>}
          {l.config_ref && <p>Recipe: {l.config_ref}</p>}
          {(l.source || sourceText) && <p>Source: {l.source ?? sourceText}</p>}
          {l.vintage && <p>Source year / vintage: {l.vintage}</p>}
          {l.licence && <p>Licence: {l.licence}</p>}
        </div>
      )}
      {children.length > 0 && (
        <ul>
          {children.map((c) => (
            <Link_ key={c.id} l={c} kids={kids} depth={depth + 1} sourcesState={sourcesState} />
          ))}
        </ul>
      )}
    </li>
  );
}

export interface WhyThisResultProps {
  cell: NodeDetail | null;
  core: Pick<PostureCore, 'meta'>;
  iso3?: string | null;
  /** The source dictionary (default: loaded from the Sector bundle when the disclosure opens). */
  dictionary?: ProvenanceDictionary | null;
  loadDictionary?: () => Promise<ProvenanceDictionary>;
  defaultOpen?: boolean;
}

export function WhyThisResult({ cell, core, iso3 = null, dictionary = null, loadDictionary = sectorSourceDictionary, defaultOpen = false }: WhyThisResultProps) {
  const [open, setOpen] = useState(defaultOpen);
  const [dict, setDict] = useState<ProvenanceDictionary | null>(dictionary);
  const [dictState, setDictState] = useState<'idle' | 'loading' | 'ready' | 'error'>(dictionary ? 'ready' : 'idle');
  // the loader as of the latest render (a caller's inline function must not restart the load)
  const loader = useRef(loadDictionary);
  loader.current = loadDictionary;
  // one attempt per opening: the load starts when the disclosure opens and is dropped if it closes first
  useEffect(() => {
    if (!open || dict) return;
    let live = true;
    setDictState('loading');
    loader.current().then(
      (d) => {
        if (!live) return;
        setDict(d);
        setDictState('ready');
      },
      () => live && setDictState('error'),
    );
    return () => {
      live = false;
    };
  }, [open, dict]);
  const [build, setBuild] = useState<ChainBuilder | null>(null);
  const [buildError, setBuildError] = useState(false);
  useEffect(() => {
    if (!open || build) return;
    let live = true;
    import('../qc/provenance.ts').then(
      (m) => live && setBuild(() => m.provenanceChain),
      () => live && setBuildError(true),
    );
    return () => {
      live = false;
    };
  }, [open, build]);
  const chain: ProvenanceChain | null = useMemo(() => (open && cell && build ? build(cell, core, dict ?? { sources: {} }, iso3) : null), [open, cell, core, dict, iso3, build]);
  const kids = useMemo(() => {
    const m = new Map<string, ProvenanceLink[]>();
    for (const l of chain?.links ?? []) if (l.parent) m.set(l.parent, [...(m.get(l.parent) ?? []), l]);
    return m;
  }, [chain]);
  const root = chain?.links.filter((l) => !l.parent) ?? [];
  const counts = chain ? chain.links.reduce<Record<LinkStatus, number>>((a, l) => ((a[l.status] += 1), a), { shipped: 0, config_only: 0, not_in_release: 0 }) : null;
  return (
    <details className="disclosure mt-2 border-t hairline" data-testid="why-this-result" open={open} onToggle={(e) => setOpen((e.currentTarget as HTMLDetailsElement).open)}>
      <summary className="flex items-center gap-2 py-2 text-xs font-medium text-paper-dim hover:text-paper">
        <span className="disclosure-chevron text-paper-mute" aria-hidden="true">
          ›
        </span>
        <span className="flex-1">Why this result?</span>
      </summary>
      {open && (
        <div className="pb-2 pl-2" data-testid="why-chain">
          {!cell ? (
            <p className="text-xs text-paper-mute">The details of this step are not loaded yet.</p>
          ) : buildError ? (
            <p className="text-xs text-paper-mute">The explanation could not be loaded.</p>
          ) : !chain ? (
            <p className="text-xs text-paper-mute" role="status">
              Loading the explanation…
            </p>
          ) : (
            <>
              <p className="text-[0.6875rem] text-paper-mute">
                From the conclusion back to the sources. Each link says whether this release ships it, whether it is known only from the configuration, or whether it is not in this release.
                {counts && ` ${counts.shipped} shipped · ${counts.config_only} from configuration · ${counts.not_in_release} not in this release.`}
                {dictState === 'loading' && ' Loading source names…'}
                {dictState === 'error' && ' Source names could not be loaded: source ids are shown where known.'}
              </p>
              <ul className="mt-1">
                {root.map((l) => (
                  <Link_ key={l.id} l={l} kids={kids} depth={0} sourcesState={dictState} />
                ))}
              </ul>
              {chain && chain.gaps.length > 0 && (
                <div className="mt-2 text-[0.6875rem] text-paper-mute" data-testid="why-gaps">
                  <p className="text-paper-dim">Not in this release, or known only from the configuration:</p>
                  <ul className="list-disc pl-4">
                    {chain.gaps.map((g, i) => (
                      <li key={i}>{g}</li>
                    ))}
                  </ul>
                </div>
              )}
            </>
          )}
        </div>
      )}
    </details>
  );
}
