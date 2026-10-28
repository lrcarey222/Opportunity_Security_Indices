// The Country view's headline: one economy's recommended posture in every technology x six-stage node,
// with the takeaway sentence, the posture counts, the trade freshness line and the cell drawer. Sits
// above the fold of the Country profile; the profile's existing sections follow below.

import { useCallback, useMemo, useState } from 'react';
import { Loading, Notice } from '../components/ui.tsx';
import { CellDrawer } from './CellDrawer.tsx';
import { cellKey, countPostures, countryCells, countryTakeaway } from './derive.ts';
import { MODE_LABEL, POSTURE_COLOR, POSTURE_LABEL, RATIONALE_LABEL, RATIONALE_MARK } from './grammar.ts';
import { PostureLegend, Swatch, TradeFreshness } from './PostureLegend.tsx';
import { PostureMatrix } from './PostureMatrix.tsx';
import { usePostureCore, usePostureDetail } from './store.ts';

export function CountryPostureHero({ iso3, name }: { iso3: string; name: string }) {
  const core = usePostureCore();
  const [sel, setSel] = useState<string | null>(null);
  const detail = usePostureDetail(sel ? iso3 : null);
  const cells = useMemo(() => (core.data ? countryCells(core.data, iso3) : []), [core.data, iso3]);
  const names = useMemo(() => Object.fromEntries((core.data?.countries ?? []).map((c) => [c.iso3, c.name])), [core.data]);
  const close = useCallback(() => setSel(null), []);
  if (core.status === 'error') {
    return (
      <Notice kind="warning" title="The posture data could not be loaded">
        {core.error}
      </Notice>
    );
  }
  if (!core.data) return <Loading label="Loading the strategic posture" />;
  if (!cells.length) {
    return (
      <Notice kind="info" title="No posture">
        {name} has no classified supply-chain nodes in the strategic position framework.
      </Notice>
    );
  }
  const n = countPostures(cells);
  const drawerCell = sel && detail.data ? detail.data.cells.find((c) => cellKey(c.tech, c.stage) === sel) ?? null : null;
  return (
    <section className="card p-5" aria-labelledby="posture-hero-title" data-testid="country-posture-hero">
      <div className="flex flex-wrap items-start justify-between gap-x-6 gap-y-3">
        <div className="min-w-0 max-w-3xl">
          <h2 id="posture-hero-title" className="rule-neis-sm text-base font-medium text-paper lg:text-lg">
            Strategic posture by technology and stage
          </h2>
          <p className="mt-2 text-sm text-paper-dim" data-testid="posture-takeaway">
            {countryTakeaway(name, cells)}
          </p>
        </div>
        <dl className="flex flex-wrap gap-2" aria-label="Posture counts">
          {(['S', 'C', 'D'] as const).map((p) => (
            <div key={p} className="flex items-center gap-2 rounded-md bg-ink-900/60 px-3 py-1.5">
              <Swatch color={POSTURE_COLOR[p]} size={12} />
              <dt className="text-xs text-paper-mute">{POSTURE_LABEL[p]}</dt>
              <dd className="tabular text-sm font-medium text-paper">{n[p]}</dd>
            </div>
          ))}
          <div className="flex items-center gap-1.5 rounded-md bg-ink-900/60 px-3 py-1.5 text-xs text-paper-mute">
            {(['preserve', 'catch_up', 'partner'] as const).map((m) => (
              <span key={m}>
                {MODE_LABEL[m]} <span className="tabular text-paper">{n[m]}</span>
              </span>
            ))}
          </div>
        </dl>
      </div>
      <div className="mt-3">
        <TradeFreshness meta={core.data.meta} />
      </div>
      <div className="mt-4 overflow-x-auto">
        <PostureMatrix core={core.data} cells={cells} selected={sel} onSelect={(k) => setSel((s) => (s === k ? null : k))} />
      </div>
      <div className="mt-4 space-y-2">
        <PostureLegend />
        <p className="text-xs text-paper-mute">
          Secure marks: {Object.entries(RATIONALE_MARK).map(([r, m]) => `${m} ${RATIONALE_LABEL[r as keyof typeof RATIONALE_LABEL].toLowerCase()}`).join(' · ')}. Select a cell for
          its partners, rationale or restraint, policy design and trade evidence.
        </p>
      </div>
      {sel && detail.status === 'loading' && <p className="mt-2 text-xs text-paper-mute">Loading the detail…</p>}
      {sel && <CellDrawer detail={detail.data} cell={drawerCell} names={names} core={core.data} onClose={close} />}
    </section>
  );
}
