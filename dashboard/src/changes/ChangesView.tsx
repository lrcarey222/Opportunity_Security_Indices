// What changed? /changes (docs/change-layer-architecture.md): the posture vintages, what changed between each
// comparable pair (counts, and whether the method changed), and the change records of the recent pairs, filterable
// by economy, change type, attribution and pair. Every value is the deterministic output of
// scripts/45_build_posture_vintages.R; a row opens the cell's drawer on its Country page.

import { useMemo, useState } from 'react';
import { Link, useSearchParams } from 'react-router';
import { EmptyState, FilterRow, Loading, Notice, PageHead, Select } from '../components/ui.tsx';
import { stageDisplay } from '../lib/stages.ts';
import { useTitle } from '../shell/title.ts';
import { ATTRIBUTION_LABEL, TYPE_LABEL, choiceLabel, fmtScore, vintageDate, type Attribution, type ChangeType, type ChangesBundle } from './contract.ts';
import { useChanges } from './store.ts';

const PAGE = 200;
const SEV_ORDER = { major: 3, moderate: 2, minor: 1 } as const;

export const CHANGES_LEDE =
  'Each published run of the strategic position framework is kept as a dated vintage. This page compares each vintage with the previous comparable one, cell by cell (economy × technology × supply-chain step), and says whether a change came from new data or from a change to the method. Detection is rule-based; nothing here is model-judged.';

function PairTable({ b }: { b: ChangesBundle }) {
  return (
    <div className="overflow-x-auto">
      <table className="w-full min-w-[40rem] text-sm" data-testid="changes-pairs">
        <thead>
          <tr className="border-b hairline text-left text-xs text-paper-mute">
            <th className="py-2 pr-3 font-normal">From → to</th>
            <th className="py-2 pr-3 font-normal">Method</th>
            <th className="py-2 pr-3 font-normal">Changed cells</th>
            <th className="py-2 pr-3 font-normal">Recommendation changes</th>
            <th className="py-2 font-normal">Attribution</th>
          </tr>
        </thead>
        <tbody>
          {[...b.pairs].reverse().map((p) => (
            <tr key={`${p.prev}>${p.cur}`} className="border-b hairline align-top">
              <td className="py-2 pr-3 text-paper">
                {vintageDate(b, p.prev)} → {vintageDate(b, p.cur)}
              </td>
              <td className="py-2 pr-3 text-paper-dim">{!p.comparable ? 'not comparable' : p.methodChanged ? 'changed' : 'same'}</td>
              <td className="py-2 pr-3 text-paper-dim">
                {!p.comparable ? (p.reason ?? '') : `${p.counts.meaningful ?? 0} of ${p.nCells ?? '?'}`}
                {p.comparable && !p.detail && (p.counts.meaningful ?? 0) > 0 ? <span className="text-paper-mute"> (summary only)</span> : null}
              </td>
              <td className="py-2 pr-3 text-paper-dim">{p.comparable ? (p.counts.recommendation ?? 0) : ''}</td>
              <td className="py-2 text-xs text-paper-mute">
                {p.comparable
                  ? (Object.keys(ATTRIBUTION_LABEL) as Attribution[])
                      .filter((a) => (p.counts[a] ?? 0) > 0)
                      .map((a) => `${ATTRIBUTION_LABEL[a]}: ${p.counts[a]}`)
                      .join(' · ') || '—'
                  : ''}
              </td>
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
}

export function ChangesTable({ b }: { b: ChangesBundle }) {
  const [params, setParams] = useSearchParams();
  const country = params.get('country') ?? '';
  const type = (params.get('type') ?? '') as ChangeType | '';
  const attribution = (params.get('attribution') ?? '') as Attribution | '';
  const pairKey = params.get('pair') ?? '';
  const [limit, setLimit] = useState(PAGE);
  const set = (k: string, v: string) => {
    const n = new URLSearchParams(params);
    if (v) n.set(k, v);
    else n.delete(k);
    setParams(n, { replace: true });
    setLimit(PAGE);
  };
  const order = useMemo(() => new Map(b.vintages.map((v, i) => [v.id, i])), [b]);
  const rows = useMemo(
    () =>
      b.changes
        .filter(
          (c) =>
            (!country || c.iso3 === country) &&
            (!type || c.types.includes(type)) &&
            (!attribution || c.attribution === attribution) &&
            (!pairKey || `${c.prev}>${c.cur}` === pairKey),
        )
        .sort(
          (a, z) =>
            (order.get(z.cur) ?? 0) - (order.get(a.cur) ?? 0) ||
            SEV_ORDER[z.severity] - SEV_ORDER[a.severity] ||
            (z.magnitude ?? 0) - (a.magnitude ?? 0) ||
            a.id.localeCompare(z.id),
        ),
    [b, country, type, attribution, pairKey, order],
  );
  const isos = useMemo(() => [...new Set(b.changes.map((c) => c.iso3))].sort(), [b]);
  const detailPairs = b.pairs.filter((p) => p.comparable && p.detail && (p.counts.meaningful ?? 0) > 0);
  return (
    <>
      <FilterRow result={`${rows.length.toLocaleString()} change${rows.length === 1 ? '' : 's'}`}>
        <Select label="Economy" value={country} onChange={(v) => set('country', v)} options={[{ value: '', label: 'All economies' }, ...isos.map((i) => ({ value: i, label: i }))]} />
        <Select
          label="Change type"
          value={type}
          onChange={(v) => set('type', v)}
          options={[{ value: '', label: 'All types' }, ...(Object.keys(TYPE_LABEL) as ChangeType[]).map((t) => ({ value: t, label: TYPE_LABEL[t] }))]}
        />
        <Select
          label="Attribution"
          value={attribution}
          onChange={(v) => set('attribution', v)}
          options={[{ value: '', label: 'Any' }, ...(Object.keys(ATTRIBUTION_LABEL) as Attribution[]).map((a) => ({ value: a, label: ATTRIBUTION_LABEL[a] }))]}
        />
        <Select
          label="Vintages"
          value={pairKey}
          onChange={(v) => set('pair', v)}
          options={[
            { value: '', label: 'All recent pairs' },
            ...detailPairs.map((p) => ({ value: `${p.prev}>${p.cur}`, label: `${vintageDate(b, p.prev)} → ${vintageDate(b, p.cur)}` })),
          ]}
        />
      </FilterRow>
      {rows.length === 0 ? (
        <EmptyState>No change records match these filters.</EmptyState>
      ) : (
        <div className="overflow-x-auto">
          <table className="w-full min-w-[56rem] text-sm" data-testid="changes-table">
            <thead>
              <tr className="border-b hairline text-left text-xs text-paper-mute">
                <th className="py-2 pr-3 font-normal">Date</th>
                <th className="py-2 pr-3 font-normal">Economy</th>
                <th className="py-2 pr-3 font-normal">Technology</th>
                <th className="py-2 pr-3 font-normal">Step</th>
                <th className="py-2 pr-3 font-normal">Change</th>
                <th className="py-2 pr-3 font-normal">Magnitude</th>
                <th className="py-2 pr-3 font-normal">Previous</th>
                <th className="py-2 pr-3 font-normal">Current</th>
                <th className="py-2 font-normal">Attribution</th>
              </tr>
            </thead>
            <tbody>
              {rows.slice(0, limit).map((c) => (
                <tr key={`${c.id}@${c.cur}`} className="border-b hairline align-top" data-testid="changes-row" title={c.explanation}>
                  <td className="py-1.5 pr-3 whitespace-nowrap text-paper-dim">{vintageDate(b, c.cur)}</td>
                  <td className="py-1.5 pr-3">
                    <Link className="link" to={`/countries/${c.iso3}?node=${encodeURIComponent(`${c.tech}|${c.stage}`)}`}>
                      {c.iso3}
                    </Link>
                  </td>
                  <td className="py-1.5 pr-3 text-paper">{c.tech}</td>
                  <td className="py-1.5 pr-3 text-paper">{stageDisplay(c.stage)}</td>
                  <td className="py-1.5 pr-3 text-paper-dim">
                    {c.types.map((t) => TYPE_LABEL[t]).join(', ')}
                    <span className="text-paper-mute"> · {c.severity}</span>
                  </td>
                  <td className="py-1.5 pr-3 text-paper-dim">{c.magnitude === null ? '—' : fmtScore(c.magnitude)}</td>
                  <td className="py-1.5 pr-3 text-paper-dim">{choiceLabel(c.before.choice)}</td>
                  <td className={`py-1.5 pr-3 ${c.types.includes('recommendation') ? 'text-paper' : 'text-paper-dim'}`}>{choiceLabel(c.after.choice)}</td>
                  <td className="py-1.5 text-xs text-paper-mute">{ATTRIBUTION_LABEL[c.attribution]}</td>
                </tr>
              ))}
            </tbody>
          </table>
          {rows.length > limit && (
            <button type="button" className="btn btn-ghost btn-sm mt-3" onClick={() => setLimit((l) => l + PAGE)}>
              Show {Math.min(PAGE, rows.length - limit)} more
            </button>
          )}
        </div>
      )}
    </>
  );
}

export default function ChangesView() {
  useTitle('What changed?');
  const { status, data, error } = useChanges();
  return (
    <div className="mx-auto w-full max-w-7xl px-4 py-8 sm:px-6 lg:px-8">
      <PageHead eyebrow="About" title="What changed?" lede={CHANGES_LEDE} />
      {status === 'loading' && <Loading label="Loading the change history" />}
      {status === 'error' && <Notice kind="error">The change history could not be loaded: {error}</Notice>}
      {status === 'absent' && <Notice>This build carries no change history yet.</Notice>}
      {status === 'ready' && data && (
        <>
          <section className="mb-8" aria-label="Vintages compared">
            <h2 className="mb-2 text-lg font-medium text-paper">Vintages compared</h2>
            <p className="mb-3 max-w-3xl text-sm text-paper-mute">
              A result is credited to new data only when the method was the same in both vintages. When the method changed, it is credited to the
              method alone only when the input data is verifiably unchanged; otherwise it is left unattributed.
            </p>
            <PairTable b={data} />
          </section>
          <section aria-label="Change records">
            <h2 className="mb-2 text-lg font-medium text-paper">Changes in the recent vintages</h2>
            <ChangesTable b={data} />
          </section>
        </>
      )}
    </div>
  );
}
